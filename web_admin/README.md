# PineGuard Admin Web Prototype

This folder contains a light-mode web administration prototype for the Pineapple Plant Monitoring System.

## Source of truth

- Page requirements: [docs/FYP_REPORT_BITS_COVER_TO_C4.md](C:/Users/Vincent/Desktop/FYP/docs/FYP_REPORT_BITS_COVER_TO_C4.md), [docs/FEATURES.md](C:/Users/Vincent/Desktop/FYP/docs/FEATURES.md), and [docs/SYSTEM_ARCHITECTURE.md](C:/Users/Vincent/Desktop/FYP/docs/SYSTEM_ARCHITECTURE.md)
- Desktop flow reference: [design-system/figma-export/handoff/prototype-interactions.json](C:/Users/Vincent/Desktop/FYP/design-system/figma-export/handoff/prototype-interactions.json)
- Visual base template: [DESIGN.md](C:/Users/Vincent/Desktop/FYP/DESIGN.md)

## Included pages

- `Admin Login`
- `Admin Dashboard`
- `User Management`
- `Submitted Records`
- `Record Detail`
- `Data Analysis`
- `System Monitor`
- `Settings`

## Open

Open [index.html](C:/Users/Vincent/Desktop/FYP/web_admin/index.html) in a browser.

For cPanel-MySQL-backed deployments, keep MySQL private to the FastAPI backend
and pass the backend API URL to the page:

```text
index.html?apiBaseUrl=https://api.pineguard.example.com
```

Use this public HTTPS backend URL when the browser is not on the same Wi-Fi/LAN
as the backend machine. Same-LAN hostname and IP discovery is only for local
development.

For a quick public demo from this laptop, start the backend with a Cloudflare
Tunnel and use the printed `PublicBaseUrl`:

```powershell
powershell -ExecutionPolicy Bypass -File ..\deploy\start_cpanel_mysql_backend.ps1 -UseCloudflareTunnel
```

For local testing on the same machine:

```text
index.html?apiBaseUrl=http://127.0.0.1:8000
```

The cPanel MySQL backend launcher writes the current reachable backend URL plus
stable hostname fallbacks to `pineguard-api-config.js`. If Wi-Fi changes, the
web console automatically tries those candidates before showing a connection
error:

```powershell
powershell -ExecutionPolicy Bypass -File ..\deploy\start_cpanel_mysql_backend.ps1
```

After the backend seeds demo users, sign in with:

```text
admin@pineguard.local / pineapple123
```
