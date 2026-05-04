# cPanel Combined PineGuard API

Use this path when Render or another cloud cannot connect to Semrush/cPanel
MySQL because MySQL rejects the cloud provider's outbound host.

This does not change the database layer or the OCI AI layer:

- The API connects to the Semrush MySQL database locally from cPanel.
- The API calls the existing OCI AI layer with `AI_LAYER_BASE_URL`.
- Web Admin, Mobile, and ESP32 hardware call one public API URL.

## cPanel Python App

Create a Python app in cPanel/CloudLinux:

```text
Application root: pineguard-api
Application URL: api.your-domain.com or your-domain.com/pineguard-api
Application startup file: passenger_wsgi.py
Application entry point: application
Python version: 3.11 if available
```

Upload the contents of `ai_backend/` into the application root so
`passenger_wsgi.py`, `ollama_backend.py`, `db.py`, and `requirements.txt` sit
directly inside the cPanel Python app folder:

```text
pineguard-api/
  __init__.py
  db.py
  ollama_backend.py
  passenger_wsgi.py
  requirements.txt
```

Install dependencies from cPanel's Python app page or terminal:

```bash
pip install -r requirements.txt
```

## Environment Variables

Set these in the cPanel Python app environment.

If the app runs on the same cPanel account as `mytrusth_pineguard`, use
`localhost` for the database host:

```text
DATABASE_URL=mysql+pymysql://mytrusth_pgapp:YOUR_PASSWORD@localhost:3306/mytrusth_pineguard?charset=utf8mb4
ALLOW_DATABASE_FALLBACK=false
DATABASE_FALLBACK_URL=
AI_LAYER_BASE_URL=http://161.118.235.40:8000
AI_LAYER_TIMEOUT_SECONDS=240
REQUEST_TIMEOUT_SECONDS=240
PUBLIC_BASE_URL=https://api.your-domain.com
UPLOADS_DIR=/home/YOUR_CPANEL_USER/pineguard-api/uploads
```

Restart the Python app after changing environment variables.

## Web Admin

Set the Web Admin config to the cPanel API URL:

```js
window.PINEGUARD_API_BASE_URL = "https://api.your-domain.com";
window.PINEGUARD_API_BASE_URLS = ["https://api.your-domain.com"];
window.PINEGUARD_ENABLE_LOCAL_API_DISCOVERY = false;
```

## Verify

```powershell
$api = "https://api.your-domain.com"
Invoke-RestMethod "$api/health"
Invoke-RestMethod "$api/api/auth/login" -Method Post -ContentType "application/json" -Body '{"email":"admin@pineguard.local","password":"pineapple123"}'
```

Expected `/health` shape:

```json
{
  "status": "ok",
  "backend": "running",
  "database": "mysql",
  "analysisLayer": "remote_ai_backend"
}
```

## Build Upload Zip Locally

From the project root:

```powershell
powershell -ExecutionPolicy Bypass -File .\deploy\build_cpanel_python_app.ps1
```

This creates:

```text
tmp/pineguard-cpanel-python-app.zip
```

Upload and extract that zip into the cPanel Python app root.
