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
CSV_PATH = os.getenv("CSV_PATH", "/tmp/pollution_data.csv")
HOST = os.getenv("HOST", "0.0.0.0")
PORT = int(os.getenv("PORT", "8080"))

# Cloud Storage Configuration
BUCKET_NAME = "yerevan-air-quality-data"  # You'll create this bucket
CSV_FILENAME = "air_quality_data.csv"

# Google Air Quality API
AIR_QUALITY_URL = "https://airquality.googleapis.com/v1/currentConditions:lookup"
API_KEY = os.getenv("GOOGLE_AIR_QUALITY_API_KEY")

# Geolocations of all the 73 Clarity sensors 
def load_monitoring_locations():
    try:
        with open("monitoring_locations.json", "r", encoding="utf-8") as f:
            locations = json.load(f)
        print(f"Loaded {len(locations)} monitoring locations from JSON")
        return locations
    except FileNotFoundError:
        print("monitoring_locations.json not found. Using default locations.")
        # Fallback to original locations if JSON doesn't exist
        return [
            {
                "label": "Yerevan_Center",
                "latitude": 40.1814,
                "longitude": 44.5146,
                "description": "Yerevan City Center - Republic Square"
            }
        ]

MONITORING_LOCATIONS = load_monitoring_locations()

# old monitoring stations 
'''
MONITORING_LOCATIONS = [
    {
        "label": "Yerevan_Center",
        "latitude": 40.1814,
        "longitude": 44.5146,
        "description": "Yerevan City Center - Republic Square"
    },
    {
        "label": "Yerevan_North",
        "latitude": 40.2100,
        "longitude": 44.5830,
        "description": "Yerevan Northern District - Avan"
    },
    {
        "label": "Yerevan_South", 
        "latitude": 40.1550,
        "longitude": 44.4750,
        "description": "Yerevan Southern District - Shengavit"
    },
    {
        "label": "Yerevan_West",
        "latitude": 40.1990,
        "longitude": 44.4690,
        "description": "Yerevan Western District - Ajapnyak"
    },
    {
        "label": "Yerevan_East",
        "latitude": 40.1750,
        "longitude": 44.5330,
        "description": "Yerevan Eastern District - Nor Nork"
    },
    {
        "label": "Yerevan_Northwest",
        "latitude": 40.2050,
        "longitude": 44.5000,
        "description": "Yerevan Northwest - Eritasardakan"
    },
    {
        "label": "Yerevan_Southwest",
        "latitude": 40.1670,
        "longitude": 44.4330,
        "description": "Yerevan Southwest - Malatia"
    }
]
'''

app = Flask(__name__)

# Pollutant mapping
POLLUTANT_INFO = {
    'CO': {'name': 'Carbon Monoxide', 'units': 'ppb', 'column_suffix': 'ppb'},
    'NO2': {'name': 'Nitrogen Dioxide', 'units': 'ppb', 'column_suffix': 'ppb'},
    'O3': {'name': 'Ozone', 'units': 'ppb', 'column_suffix': 'ppb'},
    'SO2': {'name': 'Sulfur Dioxide', 'units': 'ppb', 'column_suffix': 'ppb'},
    'PM25': {'name': 'PM2.5', 'units': 'μg/m³', 'column_suffix': 'ugm3'},
    'PM10': {'name': 'PM10', 'units': 'μg/m³', 'column_suffix': 'ugm3'}
}

# CSV headers
CSV_HEADER = [
    "timestamp_utc", "location_label", "latitude", "longitude", "description",
    "overall_aqi", "CO_ppb", "NO2_ppb", "O3_ppb", "SO2_ppb", "PM25_ugm3", "PM10_ugm3"
]

# Cloud Storage Functions
def save_to_cloud_storage():
    """Save CSV to Cloud Storage"""
    try:
        client = storage.Client()
        bucket = client.bucket(BUCKET_NAME)
        blob = bucket.blob(CSV_FILENAME)
        
        blob.upload_from_filename(CSV_PATH)
        print(f"💾 CSV backed up to Cloud Storage: {BUCKET_NAME}/{CSV_FILENAME}")
        return True
    except Exception as e:
        print(f"❌ Cloud Storage backup failed: {e}")
        return False

def load_from_cloud_storage():
    """Load CSV from Cloud Storage"""
    try:
        client = storage.Client()
        bucket = client.bucket(BUCKET_NAME)
        blob = bucket.blob(CSV_FILENAME)
        
        if blob.exists():
            blob.download_to_filename(CSV_PATH)
            print(f"📥 Loaded existing CSV from Cloud Storage")
            return True
        else:
            print(f"📝 No existing CSV found in Cloud Storage, will create new one")
            return False
    except Exception as e:
        print(f"❌ Cloud Storage load failed: {e}")
        return False

def ensure_csv():
    """Create CSV file if it doesn't exist"""
    if not os.path.exists(CSV_PATH):
        with open(CSV_PATH, "w", newline="", encoding="utf-8") as f:
            csv.writer(f).writerow(CSV_HEADER)
        print(f"✅ Created new CSV file: {CSV_PATH}")
        
        # Save initial CSV to Cloud Storage
        save_to_cloud_storage()
    else:
        file_size = os.path.getsize(CSV_PATH)
        print(f"📊 Using existing CSV file: {CSV_PATH} (size: {file_size} bytes)")

# In-memory cache for latest readings only
latest_cache: Dict[str, Dict[str, Any]] = {}

# Health tracking
last_poll_at = None
last_poll_error = None
rows_written_total = 0

# Air Quality API Helper
def get_air_quality_data(latitude: float, longitude: float) -> Dict[str, Any]:
    if not API_KEY:
        raise ValueError("Google Air Quality API key not configured")
    
    headers = {"Content-Type": "application/json"}
    payload = {
        "location": {"latitude": latitude, "longitude": longitude},
        "extraComputations": ["POLLUTANT_CONCENTRATION"],
        "universalAqi": True
    }
    
    url = f"{AIR_QUALITY_URL}?key={API_KEY}"
    response = requests.post(url, headers=headers, json=payload, timeout=30)
    response.raise_for_status()
    return response.json()

def parse_air_quality_data(raw_data: Dict[str, Any], location: Dict[str, Any]) -> Dict[str, Any]:
    parsed = {
        "timestamp_utc": datetime.now(timezone.utc).isoformat(),
        "location_label": location["label"],
        "latitude": location["latitude"],
        "longitude": location["longitude"],
        "description": location["description"],
        "overall_aqi": None,        
        "pollutants": {}
    }
    
    try:
        if 'indexes' in raw_data:
            for index in raw_data['indexes']:
                if index['code'] == 'uaqi':
                    parsed['overall_aqi'] = index['aqi']                    
        
        if 'pollutants' in raw_data:
            for pollutant in raw_data['pollutants']:
                code = pollutant['code']
                concentration = pollutant.get('concentration', {})
                parsed['pollutants'][code] = {
                    'concentration': concentration.get('value'),
                    'units': concentration.get('units')
                }
        
        return parsed
    except Exception as e:
        print(f"Error parsing air quality data: {e}")
        return parsed

def create_csv_row(parsed_data: Dict[str, Any]) -> Dict[str, Any]:
    row = {
        "timestamp_utc": parsed_data["timestamp_utc"],
        "location_label": parsed_data["location_label"],
        "latitude": parsed_data["latitude"],
        "longitude": parsed_data["longitude"],
        "description": parsed_data["description"],
        "overall_aqi": parsed_data["overall_aqi"],        
    }
    
    # Initialize pollutant columns
    for pollutant in POLLUTANT_INFO.keys():
        column_name = f"{pollutant}_{POLLUTANT_INFO[pollutant]['column_suffix']}"
        row[column_name] = None
    
    # Fill pollutant data
    for code, info in parsed_data["pollutants"].items():
        code_upper = code.upper()
        if code_upper in POLLUTANT_INFO:
            column_name = f"{code_upper}_{POLLUTANT_INFO[code_upper]['column_suffix']}"
            row[column_name] = info['concentration']
    
    return row

def poll_once():
    global last_poll_at, last_poll_error, rows_written_total
    
    print(f"🚀 Starting poll at {datetime.now(timezone.utc).isoformat()}")
    
    if not API_KEY:
        last_poll_error = "GOOGLE_AIR_QUALITY_API_KEY is not set"
        print("❌ ERROR: GOOGLE_AIR_QUALITY_API_KEY is not set", file=sys.stderr)
        return {"status": "error", "message": "API key not set"}

    all_rows = []
    successful_locations = 0
    
    for location in MONITORING_LOCATIONS:
        try:
            print(f"🔍 Polling {location['label']}...")
            raw_data = get_air_quality_data(location["latitude"], location["longitude"])
            parsed_data = parse_air_quality_data(raw_data, location)
            csv_row = create_csv_row(parsed_data)
            all_rows.append(csv_row)
            
            latest_cache[location["label"]] = csv_row
            successful_locations += 1
            print(f"✅ {location['label']} - AQI: {csv_row['overall_aqi']}")
            
        except Exception as e:
            error_msg = f"{location['label']}: {str(e)}"
            last_poll_error = error_msg
            print(f"❌ ERROR {error_msg}", file=sys.stderr)
    
    last_poll_at = datetime.now(timezone.utc).isoformat()
    
    if all_rows:
        # Append to local CSV
        with open(CSV_PATH, "a", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=CSV_HEADER)
            for row in all_rows:
                writer.writerow(row)
                rows_written_total += 1
        
        # Save to Cloud Storage
        cloud_success = save_to_cloud_storage()
        
        print(f"📈 Added {len(all_rows)} rows. Cloud Storage: {'✅' if cloud_success else '❌'}")
        last_poll_error = None
    
    return {
        "status": "success",
        "locations_polled": successful_locations,
        "total_locations": len(MONITORING_LOCATIONS),
        "timestamp": last_poll_at
    }

# Cloud Scheduler Endpoint
@app.route("/api/poll", methods=['POST', 'GET'])
def trigger_poll():
    """Endpoint for Cloud Scheduler to trigger polling"""
    print("🎯 Cloud Scheduler triggered poll")
    result = poll_once()
    return jsonify(result)

# Health check endpoint
@app.route("/health")
def health_check():
    return jsonify({
        "status": "healthy", 
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "service": "Yerevan Air Quality Monitor"
    })

# Web Portal
@app.route("/")
def index():
    html = '''
<!doctype html>
<html>
<head>
<meta charset="utf-8"/>
<title>Yerevan Air Quality Monitor</title>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<style>
body { font-family: system-ui, -apple-system, Segoe UI, Roboto, Arial, sans-serif; margin: 20px; }
h1 { margin-bottom: 0.2rem; }
table { border-collapse: collapse; width: 100%; margin-top: 1rem; }
th, td { border: 1px solid #ddd; padding: 8px; text-align:left; }
th { background: #f5f5f5; }
.status { margin: 0.2rem 0 1rem 0; }
.badge { display:inline-block; padding:2px 6px; border-radius:6px; background:#eee; margin-right:6px; }
.aqi-good { background: #00e400; color: white; }
.aqi-moderate { background: #ffff00; }
.aqi-unhealthy-sensitive { background: #ff7e00; color: white; }
.aqi-unhealthy { background: #ff0000; color: white; }
.aqi-very-unhealthy { background: #8f3f97; color: white; }
.aqi-hazardous { background: #7e0023; color: white; }
.location-section { margin-bottom: 2rem; }
.location-header { background: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px; }
.cloud-scheduler-info { background: #e8f5e8; padding: 15px; border-radius: 5px; margin: 1rem 0; }
.last-updated { color: #666; font-style: italic; margin-top: 5px; }
.cloud-storage-info { background: #e3f2fd; padding: 10px; border-radius: 5px; margin: 1rem 0; }
</style>
</head>
<body>
  <h1>Yerevan Air Quality Monitor</h1>
  <div class="status">
    <span class="badge">Cloud Scheduler Powered</span>
    <span class="badge">Cloud Storage Backed</span>
    <span class="badge">Monitoring ''' + str(len(MONITORING_LOCATIONS)) + ''' locations</span>
    <span class="badge"><a href="/download.csv">Download Full CSV</a></span>
    <span class="badge"><a href="/api/health">Health Check</a></span>
  </div>

  <div class="cloud-storage-info">
    <strong>☁️ Cloud Storage Integration</strong><br>
    All data is automatically saved to Google Cloud Storage for permanent storage.
  </div>

  <div class="cloud-scheduler-info">
    <strong>🔔 Cloud Scheduler Integration</strong><br>
    This app uses Google Cloud Scheduler to trigger polling automatically.<br>
    Manual poll: <a href="/api/poll" target="_blank">Trigger Now</a>
  </div>

  <div id="current-data">
    <!-- Current data will be populated by JavaScript -->
  </div>

<script>
function getAQIClass(aqi) {
    if (aqi <= 50) return 'aqi-good';
    if (aqi <= 100) return 'aqi-moderate';
    if (aqi <= 150) return 'aqi-unhealthy-sensitive';
    if (aqi <= 200) return 'aqi-unhealthy';
    if (aqi <= 300) return 'aqi-very-unhealthy';
    return 'aqi-hazardous';
}

function formatLocationData(locationData) {
    const timestamp = locationData.timestamp_utc ? new Date(locationData.timestamp_utc) : null;
    const timeString = timestamp ? timestamp.toLocaleString() : 'N/A';
    
    return `
        <div class="location-section">
            <div class="location-header">
                <h3>${locationData.description}</h3>
                <small>${locationData.latitude.toFixed(4)}, ${locationData.longitude.toFixed(4)}</small>
                <div class="last-updated">Last updated: ${timeString}</div>
            </div>
            <table>
                <thead>
                    <tr>
                        <th>Overall AQI</th>           
                        <th>PM2.5 (μg/m³)</th>
                        <th>PM10 (μg/m³)</th>
                        <th>NO2 (ppb)</th>
                        <th>O3 (ppb)</th>
                        <th>CO (ppb)</th>
                        <th>SO2 (ppb)</th>
                    </tr>
                </thead>
                <tbody>
                    <tr>
                        <td class="${locationData.overall_aqi ? getAQIClass(locationData.overall_aqi) : ''}">${locationData.overall_aqi || 'N/A'}</td>             
                        <td>${locationData.PM25_ugm3 || 'N/A'}</td>
                        <td>${locationData.PM10_ugm3 || 'N/A'}</td>
                        <td>${locationData.NO2_ppb || 'N/A'}</td>
                        <td>${locationData.O3_ppb || 'N/A'}</td>
                        <td>${locationData.CO_ppb || 'N/A'}</td>
                        <td>${locationData.SO2_ppb || 'N/A'}</td>
                    </tr>
                </tbody>
            </table>
        </div>
    `;
}

async function refreshData() {
    try {
        const latestRes = await fetch('/api/latest', { cache: 'no-store' });
        const latestData = await latestRes.json();
        
        const currentDataDiv = document.getElementById('current-data');
        currentDataDiv.innerHTML = '<h2>Current Air Quality Across Yerevan</h2>';
        
        // Sort locations for consistent display
        const sortedLocations = Object.entries(latestData).sort(([a], [b]) => a.localeCompare(b));
        
        sortedLocations.forEach(([label, locationData]) => {
            currentDataDiv.innerHTML += formatLocationData(locationData);
        });
        
    } catch(e) { 
        console.error('Error refreshing data:', e); 
    }
}

// Auto-refresh data every 30 seconds
setInterval(refreshData, 30000);
refreshData();
</script>

</body>
</html>
'''
    return Response(html, mimetype="text/html")

# API Endpoints
@app.route("/api/latest")
def api_latest():
    return jsonify(latest_cache)

#@app.route("/download.csv")
#def download_csv():
#    return send_file(CSV_PATH, as_attachment=True, download_name="yerevan_air_quality_data.csv")

@app.route("/download.csv")
def download_csv():
    try:
        # Create a temporary file
        temp_path = "/tmp/download_temp.csv"
        
        # Download from Cloud Storage
        client = storage.Client()
        bucket = client.bucket(BUCKET_NAME)
        blob = bucket.blob(CSV_FILENAME)
        
        if not blob.exists():
            return "CSV file not found in Cloud Storage", 404
            
        blob.download_to_filename(temp_path)
        
        return send_file(
            temp_path, 
            as_attachment=True, 
            download_name="yerevan_air_quality_data.csv",
            mimetype="text/csv"
        )
    except Exception as e:
        print(f"❌ Error downloading CSV: {e}")
        return f"Error downloading CSV: {e}", 500


@app.route("/api/health")
def api_health():
    return jsonify({
        "last_poll_at": last_poll_at,
        "last_poll_error": last_poll_error,
        "rows_written_total": rows_written_total,
        "locations_monitored": len(MONITORING_LOCATIONS),
        "service_url": "https://armeniapollutionanalysis-578058838716.europe-west1.run.app"
    })

if __name__ == "__main__":
    # Load from Cloud Storage or create new
    print("🔧 Initializing Cloud Storage...")
    if not load_from_cloud_storage():
        ensure_csv()
    
    # Do one initial poll at startup
    print(f"🚀 Starting Yerevan Air Quality Monitor")
    print(f"📍 Monitoring {len(MONITORING_LOCATIONS)} locations")
    print(f"🔧 Environment check:")
    print(f"   - API Key: {'SET' if API_KEY else 'NOT SET'}")
    print(f"   - Cloud Storage Bucket: {BUCKET_NAME}")
    print(f"   - Port: {PORT}")
    
    # Initial poll
    poll_once()
    
    print(f"🌐 Starting web server on {HOST}:{PORT}")
    app.run(host=HOST, port=PORT, debug=False)
