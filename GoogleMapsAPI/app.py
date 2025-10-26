#!/usr/bin/env python3
import os, csv, json, time, sys
from datetime import datetime, timezone
from typing import List, Dict, Any
import requests
from flask import Flask, jsonify, send_file, Response
from google.cloud import storage

# ----------------------------
# Config
# ----------------------------
CSV_PATH = os.getenv("CSV_PATH", "traffic_observations.csv")
CORRIDORS_JSON = os.getenv("CORRIDORS_JSON", "corridors.json")
PLOT_WINDOW_LIMIT = int(os.getenv("PLOT_WINDOW_LIMIT", "150"))
HOST = os.getenv("HOST", "0.0.0.0")
PORT = int(os.getenv("PORT", "8080"))

# GCS Configuration
GCS_BUCKET_NAME = os.getenv("GCS_BUCKET_NAME", "yerevan-traffic-data")
GCS_CSV_PATH = os.getenv("GCS_CSV_PATH", "traffic_observations.csv")
GCS_CORRIDORS_PATH = os.getenv("GCS_CORRIDORS_PATH", "corridors.json")
USE_GCS = os.getenv("USE_GCS", "false").lower() == "true"

ROUTES_URL = "https://routes.googleapis.com/directions/v2:computeRoutes"
FIELD_MASK = "routes.duration,routes.distanceMeters,routes.staticDuration,routes.travelAdvisory"

app = Flask(__name__)

# Initialize GCS client
gcs_client = None
if USE_GCS:
    try:
        gcs_client = storage.Client()
        print(f"GCS client initialized, bucket: {GCS_BUCKET_NAME}")
    except Exception as e:
        print(f"Warning: Failed to initialize GCS client: {e}")
        gcs_client = None

# ----------------------------
# File operations with GCS support
# ----------------------------
def load_corridors() -> List[Dict[str, Any]]:
    if USE_GCS and gcs_client:
        try:
            bucket = gcs_client.bucket(GCS_BUCKET_NAME)
            blob = bucket.blob(GCS_CORRIDORS_PATH)
            content = blob.download_as_string().decode('utf-8')
            print(f"Loaded corridors from GCS: {GCS_BUCKET_NAME}/{GCS_CORRIDORS_PATH}")
            return json.loads(content)
        except Exception as e:
            print(f"Failed to load corridors from GCS, falling back to local: {e}")
    
    # Fallback to local file
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
    if USE_GCS and gcs_client:
        # For GCS, we don't need to create the file in advance
        return
        
    if not os.path.exists(CSV_PATH):
        with open(CSV_PATH, "w", newline="", encoding="utf-8") as f:
            csv.writer(f).writerow(CSV_HEADER)

ensure_long_csv()

def append_to_csv(rows: List[Dict[str, Any]]):
    """Append rows to CSV, both locally and to GCS if enabled"""
    
    # Local file append
    with open(CSV_PATH, "a", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=CSV_HEADER)
        for row in rows:
            w.writerow(row)
    
    # GCS append (download, append, upload)
    if USE_GCS and gcs_client:
        try:
            bucket = gcs_client.bucket(GCS_BUCKET_NAME)
            blob = bucket.blob(GCS_CSV_PATH)
            
            # Download existing content
            try:
                existing_content = blob.download_as_string().decode('utf-8')
            except Exception:
                # File doesn't exist yet, create with header
                existing_content = ",".join(CSV_HEADER) + "\n"
            
            # Append new rows
            output = existing_content
            for row in rows:
                output += ",".join(str(row.get(col, "")) for col in CSV_HEADER) + "\n"
            
            # Upload back to GCS
            blob.upload_from_string(output, content_type='text/csv')
            print(f"Appended {len(rows)} rows to GCS: {GCS_BUCKET_NAME}/{GCS_CSV_PATH}")
            
        except Exception as e:
            print(f"Error updating GCS CSV: {e}")

def download_csv_from_gcs() -> str:
    """Download CSV from GCS to local file for serving"""
    if USE_GCS and gcs_client:
        try:
            bucket = gcs_client.bucket(GCS_BUCKET_NAME)
            blob = bucket.blob(GCS_CSV_PATH)
            content = blob.download_as_string().decode('utf-8')
            
            # Write to local file for serving
            with open(CSV_PATH, "w", encoding="utf-8") as f:
                f.write(content)
                
            return CSV_PATH
        except Exception as e:
            print(f"Error downloading from GCS: {e}")
    
    return CSV_PATH

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
        # Use new append function that handles both local and GCS
        append_to_csv(rows)
        
        for row in rows:
            rows_written_total += 1
            latest_cache[row["label"]] = row
            
            # Store with proper datetime for plotting
            dt = datetime.fromisoformat(row["timestamp_utc"].replace('Z', '+00:00'))
            history_cache.setdefault(row["label"], []).append(
                (dt.isoformat(), row["congestion_index"], row["duration_sec"])
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
.gcs-info { background: #e8f0f5; padding: 15px; border-radius: 5px; margin: 1rem 0; }
</style>
</head>
<body>
  <h1>Yerevan Traffic Monitor</h1>
  <div class="status">
    <span class="badge">Cloud Scheduler Powered</span>
    <span class="badge">GCS Storage: ''' + ("Enabled" if USE_GCS else "Disabled") + '''</span>
    <span class="badge"><a href="/export.csv">Download long CSV</a></span>
    <span class="badge"><a href="/api/latest">Latest JSON</a></span>
    <span class="badge"><a href="/api/poll" target="_blank">Manual Poll</a></span>
  </div>

  <div class="cloud-scheduler-info">
    <strong>Cloud Scheduler Integration</strong><br>
    This app uses Google Cloud Scheduler to trigger polling automatically every minute.<br>
    Manual poll: <a href="/api/poll" target="_blank">Trigger Now</a>
  </div>

  <div class="gcs-info">
    <strong>Google Cloud Storage: ''' + ("Enabled" if USE_GCS else "Disabled") + '''</strong><br>
    ''' + (f"Bucket: {GCS_BUCKET_NAME}, CSV: {GCS_CSV_PATH}" if USE_GCS else "GCS is not enabled. Set USE_GCS=true to enable cloud storage.") + '''
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
const RIGHT_PAD_MINUTES = 10;

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

// ---------- Combined congestion chart with proper timestamps ----------
let combinedChart;

async function buildChart(){
  const res = await fetch('/api/all_history?limit=' + encodeURIComponent(WINDOW_LIMIT), { cache: 'no-store' });
  const all = await res.json();

  const labels = Object.keys(all).sort();
  const datasets = labels.map((label, i) => {
    const raw = (all[label] || []).filter(p => p && p[1] != null);
    
    // Convert to proper time-based data points
    const points = raw.map(p => ({ 
      x: luxon.DateTime.fromISO(p[0]).toMillis(), // Convert ISO timestamp to milliseconds
      y: p[1] 
    }));

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
      tension: 0.1
    };
  });

  // Calculate time window for x-axis
  const now = luxon.DateTime.now().toMillis();
  const xMax = now;
  const xMin = now - (WINDOW_LIMIT * 60 * 1000); // Show last N minutes

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
            title: function(items) {
              if (items.length > 0) {
                const timestamp = items[0].parsed.x;
                return luxon.DateTime.fromMillis(timestamp).toLocaleString(luxon.DateTime.DATETIME_MED);
              }
              return '';
            },
            label: item => `${item.dataset.label}: ${item.formattedValue}`
          }
        }
      },
      scales: {
        x: {
          type: 'time',
          time: {
            unit: 'minute',
            displayFormats: {
              minute: 'HH:mm'
            }
          },
          min: xMin,
          max: xMax,
          title: { display: true, text: 'Time' },
          ticks: { autoSkip: true, maxTicksLimit: 10 }
        },
        y: {
          min: 0.5,
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
    if USE_GCS and gcs_client:
        # Ensure we have the latest from GCS
        download_csv_from_gcs()
    return send_file(CSV_PATH, as_attachment=True, download_name="traffic_observations.csv")

@app.route("/api/health")
def api_health():
    return jsonify({
        "last_poll_at": last_poll_at,
        "last_poll_error": last_poll_error,
        "rows_written_total": rows_written_total,
        "corridors_monitored": len(corridors),
        "gcs_enabled": USE_GCS,
        "gcs_bucket": GCS_BUCKET_NAME if USE_GCS else None,
        "service_url": "https://yerevantrafficmonitor-578058838716.europe-west1.run.app"
    })

# ----------------------------
# Main
# ----------------------------
if __name__ == "__main__":
    print(f"Starting Yerevan Traffic Monitor")
    print(f"Monitoring {len(corridors)} corridors")
    print(f"GCS Storage: {'Enabled' if USE_GCS else 'Disabled'}")
    if USE_GCS:
        print(f"GCS Bucket: {GCS_BUCKET_NAME}")
    
    # Initial poll at startup
    poll_once()
    
    print(f"Starting web server on {HOST}:{PORT}")
    app.run(host=HOST, port=PORT, debug=False)
