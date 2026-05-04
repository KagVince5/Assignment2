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

For mobile builds, pass the same cloud API:

```powershell
flutter build apk --dart-define=API_BASE_URL=https://pineguard-api.onrender.com
```

For ESP32 firmware, set `BACKEND_BASE_URL` to the same cloud API. Database credentials stay only in Render.
