# PineGuard Render Deploy

Cloud deploy package for PineGuard Web Admin and combined FastAPI backend.

- Clients call the Render FastAPI backend only.
- Backend uses `DATABASE_URL` for Semrush/cPanel MySQL.
- Backend uses `AI_LAYER_BASE_URL` for the OCI Gemma AI layer.
- Do not commit `.env.local`, database files, uploads, or credentials.
