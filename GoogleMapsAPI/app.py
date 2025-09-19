#!/usr/bin/env python3
import os, csv, json, time, threading, signal, sys
from datetime import datetime, timezone
from typing import List, Dict, Any
import requests
from flask import Flask, jsonify, send_file, Response

# ----------------------------
# Config
# ----------------------------
POLL_INTERVAL_SEC = int(os.getenv("POLL_INTERVAL_SEC", "60"))
CSV_PATH = os.getenv("CSV_PATH", "traffic_observations.csv")             # long format (1 row per corridor)
COMPACT_WIDE_PATH = os.getenv("COMPACT_WIDE_PATH", "traffic_compact_wide.csv")  # compact wide: one column per corridor with JSON list [cong,dur,statdur,dist]
CORRIDORS_JSON = os.getenv("CORRIDORS_JSON", "corridors.json")
PLOT_WINDOW_LIMIT = int(os.getenv("PLOT_WINDOW_LIMIT", "150"))           # how many recent points to show on the chart
HOST = os.getenv("HOST", "0.0.0.0")
PORT = int(os.getenv("PORT", "8000"))

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

def _compact_wide_headers():
    """
    Compact wide header:
    timestamp_utc, <label1>, <label2>, ...
    Each corridor column contains a JSON list: [cong, dur, statdur, dist]
    """
    labels = [c["label"] for c in corridors]
    return ["timestamp_utc"] + labels

def ensure_compact_wide_csv():
    if not os.path.exists(COMPACT_WIDE_PATH):
        with open(COMPACT_WIDE_PATH, "w", newline="", encoding="utf-8") as f:
            csv.writer(f).writerow(_compact_wide_headers())

ensure_long_csv()
ensure_compact_wide_csv()

# ----------------------------
# In-memory caches (for APIs/portal)
# ----------------------------
latest_cache: Dict[str, Dict[str, Any]] = {}   # label -> last row dict
history_cache: Dict[str, list] = {}            # label -> list of (ts, cong, dur)

# Health tracking
last_poll_at = None
last_poll_error = None
rows_written_total = 0

stop_event = threading.Event()

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

def write_compact_wide_row(ts_iso: str, rows: list):
    """
    Append ONE row to COMPACT_WIDE_PATH:
      [ timestamp_utc,
        json.dumps([cong, dur, statdur, dist]) for corridor1,
        json.dumps([...]) for corridor2, ...
      ]
    If a corridor failed this poll, its cell is an empty string.
    """
    labels = [c["label"] for c in corridors]
    by_label = { r["label"]: r for r in rows }

    def pack(r: Dict[str, Any]) -> str:
        return json.dumps([
            r.get("congestion_index", None),
            r.get("duration_sec", None),
            r.get("static_sec", None),
            r.get("distance_m", None),
        ], ensure_ascii=False)

    cells = []
    for lab in labels:
        r = by_label.get(lab)
        cells.append(pack(r) if r else "")

    out = [ts_iso] + cells
    with open(COMPACT_WIDE_PATH, "a", newline="", encoding="utf-8") as f:
        csv.writer(f).writerow(out)

# ----------------------------
# Poller
# ----------------------------
def poll_once():
    global last_poll_at, last_poll_error, rows_written_total
    api_key = os.getenv("GOOGLE_MAPS_API_KEY")
    if not api_key:
        last_poll_at = datetime.now(timezone.utc).isoformat()
        last_poll_error = "GOOGLE_MAPS_API_KEY is not set"
        print("ERROR: GOOGLE_MAPS_API_KEY is not set", file=sys.stderr)
        return

    headers = {
        "X-Goog-Api-Key": api_key,
        "X-Goog-FieldMask": FIELD_MASK,
        "Content-Type": "application/json"
    }

    ts = datetime.now(timezone.utc).isoformat()
    rows = []

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
        except Exception as e:
            last_poll_error = f"{label}: {e}"
            print(f"[{ts}] ERROR {label}: {e}", file=sys.stderr)

    last_poll_at = ts

    if rows:
        # Append long CSV (one row per corridor)
        with open(CSV_PATH, "a", newline="", encoding="utf-8") as f:
            w = csv.DictWriter(f, fieldnames=CSV_HEADER)
            for row in rows:
                w.writerow(row)
                rows_written_total += 1
                latest_cache[row["label"]] = row
                history_cache.setdefault(row["label"], []).append(
                    (row["timestamp_utc"], row["congestion_index"], row["duration_sec"])
                )

        # Append one compact wide row (JSON lists per corridor column)
        write_compact_wide_row(ts, rows)

def poll_loop():
    print(f"Poller started: interval={POLL_INTERVAL_SEC}s, CSV={CSV_PATH}, COMPACT_WIDE={COMPACT_WIDE_PATH}")
    while not stop_event.is_set():
        start = time.time()
        poll_once()
        elapsed = time.time() - start
        to_sleep = max(0, POLL_INTERVAL_SEC - elapsed)
        stop_event.wait(to_sleep)

# ----------------------------
# Web portal (table + ONE chart)
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
</style>
</head>
<body>
  <h1>Yerevan Traffic Monitor</h1>
  <div class="status">
    <span class="badge">Polling every __INTERVAL__s</span>
    <span class="badge"><a href="/export.csv">Download long CSV</a></span>
    <span class="badge"><a href="/export_compact_wide.csv">Download compact wide CSV</a></span>
    <span class="badge"><a href="/api/latest">Latest JSON</a></span>
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
const WINDOW_LIMIT = __WINDOW_LIMIT__;

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

// ---------- Combined congestion chart (dots + lines, last N points) ----------
const RIGHT_PAD = 10; // how many empty "slots" to leave on the right

let combinedChart;

async function buildChart(){
  const res = await fetch('/api/all_history?limit=' + encodeURIComponent(WINDOW_LIMIT), { cache: 'no-store' });
  const all = await res.json(); // { label: [[tsISO, cong, dur], ...], ... }

  const labels = Object.keys(all).sort();

  // Build index-based points: x = 1..N, y = congestion
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

  // --- Sliding window calc ---
  const lastX = datasets.reduce((m, d) => Math.max(m, d.data.length), 0);
  const xMax  = lastX + RIGHT_PAD;                 // leave space on the right
  const xMin  = Math.max(1, xMax - WINDOW_LIMIT + 1); // show only the last WINDOW_LIMIT points

  if (combinedChart) combinedChart.destroy();
  const ctx = document.getElementById('combined').getContext('2d');
  combinedChart = new Chart(ctx, {
    type: 'line',
    data: { datasets },
    options: {
      responsive: true,
      maintainAspectRatio: false,   // make sure canvas has CSS height set
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
          title: { display: true, text: `Latest ${WINDOW_LIMIT} points (padded)` },
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
    # Avoid str.format(); replace simple tokens safely
    html = html.replace("__INTERVAL__", str(POLL_INTERVAL_SEC))
    html = html.replace("__WINDOW_LIMIT__", str(PLOT_WINDOW_LIMIT))
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
        out[label] = series[-limit:]   # last N points only (hide old data)
    return jsonify(out)

@app.route("/export.csv")
def export_csv():
    return send_file(CSV_PATH, as_attachment=True, download_name="traffic_observations.csv")

@app.route("/export_compact_wide.csv")
def export_compact_wide():
    return send_file(COMPACT_WIDE_PATH, as_attachment=True, download_name="traffic_compact_wide.csv")

@app.route("/api/health")
def api_health():
    return jsonify({
        "last_poll_at": last_poll_at,
        "last_poll_error": last_poll_error,
        "rows_written_total": rows_written_total,
        "poll_interval_sec": POLL_INTERVAL_SEC,
        "csv_path": CSV_PATH,
        "compact_wide_path": COMPACT_WIDE_PATH,
        "labels": [c["label"] for c in corridors]
    })

# ----------------------------
# Lifecycle
# ----------------------------
def handle_sigterm(signum, frame):
    stop_event.set()

signal.signal(signal.SIGTERM, handle_sigterm)
signal.signal(signal.SIGINT, handle_sigterm)

if __name__ == "__main__":
    t = threading.Thread(target=poll_loop, daemon=True)
    t.start()
    try:
        # Poll once at startup so the page & chart have data immediately
        poll_once()
        app.run(host=HOST, port=PORT)
    finally:
        stop_event.set()
        t.join(timeout=5)
