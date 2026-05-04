# PineGuard Render Deployment

Render should host the combined PineGuard API. Web Admin, Flutter mobile, and ESP32 hardware call this API only.

Do not point any client directly at the OCI AI layer. The OCI service stays behind the API as:

```text
AI_LAYER_BASE_URL=http://161.118.235.40:8000
```

## Services

The root `render.yaml` creates:

- `pineguard-api`: FastAPI backend connected to the Semrush/cPanel MySQL database.
- `pineguard-web-admin`: static Web Admin UI configured to call `pineguard-api`.

Set these Render environment variables on `pineguard-api`:

```text
DATABASE_URL=<Semrush cPanel MySQL SQLAlchemy URL>
ALLOW_DATABASE_FALLBACK=false
DATABASE_FALLBACK_URL=
AI_LAYER_BASE_URL=http://161.118.235.40:8000
AI_LAYER_TIMEOUT_SECONDS=240
REQUEST_TIMEOUT_SECONDS=240
PUBLIC_BASE_URL=https://pineguard-api.onrender.com
```

If Render assigns a different service URL, update both:

- `pineguard-api` -> `PUBLIC_BASE_URL`
- `pineguard-web-admin` -> `PINEGUARD_API_BASE_URL`

## Verification

After deploy, verify the combined backend:

```powershell
$api = "https://pineguard-api.onrender.com"
Invoke-RestMethod "$api/health"
Invoke-RestMethod "$api/api/auth/login" -Method Post -ContentType "application/json" -Body '{"email":"admin@pineguard.local","password":"pineapple123"}'
```

## cPanel Remote MySQL Access

If Render logs show MySQL error 1045 for a Render hostname such as:

```text
Access denied for user 'mytrusth_pgapp'@'ip-74-220-52-251.singapore-egress.render.com'
```

authorize the Render outbound host in cPanel Remote Database Access. You can use
the cPanel UI, or run the helper with a cPanel API token:

```powershell
$env:CPANEL_API_TOKEN = "YOUR_CPANEL_API_TOKEN"
powershell -ExecutionPolicy Bypass -File .\deploy\authorize_cpanel_remote_mysql_host.ps1 `
  -CpanelHost servernew.syokdc.com `
  -CpanelUser mytrusth `
  -RemoteHost 74.220.52.251
```

Then redeploy `pineguard-api` on Render.

For mobile builds, pass the same cloud API:

```powershell
flutter build apk --dart-define=API_BASE_URL=https://pineguard-api.onrender.com
```

For ESP32 firmware, set `BACKEND_BASE_URL` to the same cloud API. Database credentials stay only in Render.
