"""
cPanel Passenger entrypoint for the PineGuard FastAPI backend.

Passenger serves WSGI applications. The backend remains FastAPI/ASGI for Render,
local uvicorn, and normal cloud services; this adapter only exposes the same app
through WSGI when hosted inside cPanel/Semrush.
"""

from a2wsgi import ASGIMiddleware

try:
    from ai_backend.ollama_backend import app as fastapi_app
except ImportError:
    from ollama_backend import app as fastapi_app

application = ASGIMiddleware(fastapi_app)
