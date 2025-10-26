#!/usr/bin/env python3
import os, csv, json, time, sys
from datetime import datetime, timezone
from typing import List, Dict, Any
import requests
from flask import Flask, jsonify, send_file, Response

# ----------------------------
# Config
# ----------------------------
CSV_PATH = os.getenv("CSV_PATH", "traffic_observations.csv")
CORRIDORS_JSON = os.getenv("CORRIDORS_JSON", "corridors.json")
PLOT_WINDOW_LIMIT = int(os.getenv("PLOT_WINDOW_LIMIT", "150"))
HOST = os.getenv("HOST", "0.0.0.0")
PORT = int(os.getenv("PORT", "8080"))

ROUTES_URL = "https://routes.googleapis.com/directions/v2:computeRoutes"
FIELD_MASK = "routes.duration,routes.distanceMeters,routes.staticDuration,routes.travelAdvisory"

app = Flask(__name__)

# ----------------------------
# Load corridors
# ----------------------------
def load_corridors() -> List[Dict[str, Any]]:
    with open(CORRIDORS_JSON, "r", encoding="utf-8") as f:
        return json.load(f)

corridors = load_corridors()

# ----------------------------
# CSV headers & ensure files
# ----------------------------
CSV_HEADER = [
    "timestamp_utc","label",
    "origin_lat","origin_lng","dest_lat","dest_lng",
    "duration_sec","static_sec","distance_m","congestion_index",
    "advisory_json"
]

def ensure_long_csv():
    if not os.path.exists(CSV_PATH):
        with open(CSV_PATH, "w", newline="", encoding="utf-8") as f:
            csv.writer(f).writerow(CSV_HEADER)

ensure_long_csv()

# ----------------------------
# In-memory caches
# ----------------------------
latest_cache: Dict[str, Dict[str, Any]] = {}
history_cache: Dict[str, list] = {}

# Health tracking
last_poll_at = None
last_poll_error = None
rows_written_total = 0

# ----------------------------
# Helpers
# ----------------------------
def seconds_to_int(s: str):
    if not s:
        return None
    s = s.strip()
    if s.endswith("s"):
        try:
            return int(float(s[:-1]))
        except:
            return None
    return None

# ----------------------------
# Poller
# ----------------------------
def poll_once():
    global last_poll_at, last_poll_error, rows_written_total
    
    print(f"Starting traffic poll at {datetime.now(timezone.utc).isoformat()}")
    
    api_key = os.getenv("GOOGLE_MAPS_API_KEY")
    if not api_key:
        last_poll_error = "GOOGLE_MAPS_API_KEY is not set"
        print("ERROR: GOOGLE_MAPS_API_KEY is not set", file=sys.stderr)
        return {"status": "error", "message": "API key not set"}

    headers = {
        "X-Goog-Api-Key": api_key,
        "X-Goog-FieldMask": FIELD_MASK,
        "Content-Type": "application/json"
    }

    ts = datetime.now(timezone.utc).isoformat()
    rows = []
    successful_corridors = 0

    for c in corridors:
        label = c["label"]
        o = c["origin"]; d = c["dest"]
        body = {
            "origin": {"location": {"latLng": {"latitude": o["lat"], "longitude": o["lng"]}}},
            "destination": {"location": {"latLng": {"latitude": d["lat"], "longitude": d["lng"]}}},
            "travelMode": "DRIVE",
            "routingPreference": "TRAFFIC_AWARE"
        }
        try:
            r = requests.post(ROUTES_URL, headers=headers, json=body, timeout=20)
            r.raise_for_status()
            data = r.json()
            route = (data.get("routes") or [{}])[0]
            dur = seconds_to_int(route.get("duration"))
            static_dur = seconds_to_int(route.get("staticDuration"))
            dist = route.get("distanceMeters", None)
            cong = None
            if dur and static_dur and static_dur > 0:
                cong = round(dur / static_dur, 3)
            advisory = route.get("travelAdvisory", {})

            row = {
                "timestamp_utc": ts,
                "label": label,
                "origin_lat": o["lat"],
                "origin_lng": o["lng"],
                "dest_lat": d["lat"],
                "dest_lng": d["lng"],
                "duration_sec": dur,
                "static_sec": static_dur,
                "distance_m": dist,
                "congestion_index": cong,
                "advisory_json": json.dumps(advisory, ensure_ascii=False)
            }
            rows.append(row)
            successful_corridors += 1
            print(f"{label} - Congestion: {cong}, Duration: {dur}s")
            
        except Exception as e:
            error_msg = f"{label}: {str(e)}"
            last_poll_error = error_msg
            print(f"ERROR {error_msg}", file=sys.stderr)

    last_poll_at = ts

    if rows:
        # Append long CSV
        with open(CSV_PATH, "a", newline="", encoding="utf-8") as f:
            w = csv.DictWriter(f, fieldnames=CSV_HEADER)
            for row in rows:
                w.writerow(row)
                rows_written_total += 1
                latest_cache[row["label"]] = row
                history_cache.setdefault(row["label"], []).append(
                    (row["timestamp_utc"], row["congestion_index"], row["duration_sec"])
                )

        last_poll_error = None
    
    return {
        "status": "success",
        "corridors_polled": successful_corridors,
        "total_corridors": len(corridors),
        "timestamp": last_poll_at
    }

# ----------------------------
# Cloud Scheduler Endpoint
# ----------------------------
@app.route("/api/poll", methods=['POST', 'GET'])
def trigger_poll():
    """Endpoint for Cloud Scheduler to trigger polling"""
    print("Cloud Scheduler triggered traffic poll")
    result = poll_once()
    return jsonify(result)

# ----------------------------
# Web portal
# ----------------------------
@app.route("/")
def index():
    html = '''
<!doctype html>
<html>
<head>
<meta charset="utf-8"/>
<title>Yerevan Traffic Monitor</title>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<style>
body { font-family: system-ui, -apple-system, Segoe UI, Roboto, Arial, sans-serif; margin: 20px; }
h1 { margin-bottom: 0.2rem; }
small { color:#555; }
table { border-collapse: collapse; width: 100%; margin-top: 1rem; }
th, td { border: 1px solid #ddd; padding: 8px; text-align:left; }
th { background: #f5f5f5; }
.status { margin: 0.2rem 0 1rem 0; }
.badge { display:inline-block; padding:2px 6px; border-radius:6px; background:#eee; margin-right:6px; }
.chartwrap { margin: 1.2rem 0; }
#combined { width: 100%; height: 380px; }
.cloud-scheduler-info { background: #e8f5e8; padding: 15px; border-radius: 5px; margin: 1rem 0; }
</style>
</head>
<body>
  <h1>Yerevan Traffic Monitor</h1>
  <div class="status">
    <span class="badge">Cloud Scheduler Powered</span>
    <span class="badge"><a href="/export.csv">Download long CSV</a></span>
    <span class="badge"><a href="/api/latest">Latest JSON</a></span>
    <span class="badge"><a href="/api/poll" target="_blank">Manual Poll</a></span>
  </div>

  <div class="cloud-scheduler-info">
    <strong>Cloud Scheduler Integration</strong><br>
    This app uses Google Cloud Scheduler to trigger polling automatically every minute.<br>
    Manual poll: <a href="/api/poll" target="_blank">Trigger Now</a>
  </div>

  <div class="chartwrap">
    <canvas id="combined"></canvas>
  </div>

  <table id="live">
    <thead>
      <tr>
        <th>Label</th>
        <th>Last update (UTC)</th>
        <th>Congestion</th>
        <th>Duration (s)</th>
        <th>Free-flow (s)</th>
        <th>Distance (m)</th>
      </tr>
    </thead>
    <tbody></tbody>
  </table>

<!-- Chart.js + Luxon time adapter -->
<script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
<script src="https://cdn.jsdelivr.net/npm/luxon@^3"></script>
<script src="https://cdn.jsdelivr.net/npm/chartjs-adapter-luxon@^1"></script>

<script>
const WINDOW_LIMIT = ''' + str(PLOT_WINDOW_LIMIT) + ''';
const RIGHT_PAD = 10;

// ---------- Live table ----------
async function refreshTable(){
  try {
    const res = await fetch('/api/latest', { cache: 'no-store' });
    const data = await res.json();
    const tbody = document.querySelector('#live tbody');
    tbody.innerHTML = '';
    const rows = Object.values(data).sort((a,b) => (a.label || '').localeCompare(b.label || ''));
    rows.forEach(row => {
      const tr = document.createElement('tr');
      tr.innerHTML = `<td>${row.label || ''}</td>
        <td>${row.timestamp_utc || ''}</td>
        <td>${row.congestion_index ?? ''}</td>
        <td>${row.duration_sec ?? ''}</td>
        <td>${row.static_sec ?? ''}</td>
        <td>${row.distance_m ?? ''}</td>`;
      tbody.appendChild(tr);
    });
  } catch(e) { console.error(e); }
}
refreshTable();
setInterval(refreshTable, 15000);

// ---------- Combined congestion chart ----------
let combinedChart;

async function buildChart(){
  const res = await fetch('/api/all_history?limit=' + encodeURIComponent(WINDOW_LIMIT), { cache: 'no-store' });
  const all = await res.json();

  const labels = Object.keys(all).sort();
  const datasets = labels.map((label, i) => {
    const raw = (all[label] || []).filter(p => p && p[1] != null);
    const points = raw.map((p, idx) => ({ x: idx + 1, y: p[1] }));

    const hue = Math.floor(i * 360 / Math.max(1, labels.length));
    return {
      label,
      data: points,
      showLine: true,
      spanGaps: true,
      borderColor: `hsl(${hue}, 70%, 45%)`,
      backgroundColor: `hsl(${hue}, 70%, 45%)`,
      borderWidth: 2,
      pointRadius: 3,
      pointHoverRadius: 5,
      tension: 0
    };
  });

  const lastX = datasets.reduce((m, d) => Math.max(m, d.data.length), 0);
  const xMax  = lastX + RIGHT_PAD;
  const xMin  = Math.max(1, xMax - WINDOW_LIMIT + 1);

  if (combinedChart) combinedChart.destroy();
  const ctx = document.getElementById('combined').getContext('2d');
  combinedChart = new Chart(ctx, {
    type: 'line',
    data: { datasets },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      parsing: false,
      animation: false,
      interaction: { mode: 'nearest', intersect: false },
      elements: { point: { radius: 3 } },
      plugins: {
        legend: { position: 'bottom' },
        tooltip: {
          callbacks: {
            title: items => `Point #${items[0]?.parsed?.x ?? ''}`,
            label: item => `${item.dataset.label}: ${item.formattedValue}`
          }
        }
      },
      scales: {
        x: {
          type: 'linear',
          min: xMin,
          max: xMax,
          title: { display: true, text: `Latest ${WINDOW_LIMIT} points` },
          ticks: { autoSkip: true }
        },
        y: {
          min: 0,
          max: 3,
          title: { display: true, text: 'Congestion Index (duration / static)' }
        }
      }
    }
  });
}

buildChart();
setInterval(buildChart, 30000);
</script>
</body>
</html>
'''
    return Response(html, mimetype="text/html")

# ----------------------------
# Data endpoints
# ----------------------------
@app.route("/api/labels")
def api_labels():
    return jsonify([c["label"] for c in corridors])

@app.route("/api/latest")
def api_latest():
    return jsonify(latest_cache)

@app.route("/api/history")
def api_history():
    from flask import request
    label = request.args.get("label")
    limit = int(request.args.get("limit", "200"))
    series = history_cache.get(label, [])
    return jsonify(series[-limit:])

@app.route("/api/all_history")
def api_all_history():
    from flask import request
    limit = int(request.args.get("limit", "150"))
    out = {}
    for label, series in history_cache.items():
        out[label] = series[-limit:]
    return jsonify(out)

@app.route("/export.csv")
def export_csv():
    return send_file(CSV_PATH, as_attachment=True, download_name="traffic_observations.csv")

@app.route("/api/health")
def api_health():
    return jsonify({
        "last_poll_at": last_poll_at,
        "last_poll_error": last_poll_error,
        "rows_written_total": rows_written_total,
        "corridors_monitored": len(corridors),
        "service_url": "https://yerevantrafficmonitor-578058838716.europe-west1.run.app"
    })

# ----------------------------
# Main
# ----------------------------
if __name__ == "__main__":
    print(f"Starting Yerevan Traffic Monitor")
    print(f"Monitoring {len(corridors)} corridors")
    
    # Initial poll at startup
    poll_once()
    
    print(f"Starting web server on {HOST}:{PORT}")
    app.run(host=HOST, port=PORT, debug=False)
    
