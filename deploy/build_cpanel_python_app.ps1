[CmdletBinding()]
param(
    [string]$OutputPath = "tmp\pineguard-cpanel-python-app.zip"
)

$ErrorActionPreference = "Stop"

$repoRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
$stagingRoot = Join-Path $repoRoot "tmp\pineguard-cpanel-python-app"
$resolvedOutputPath = Join-Path $repoRoot $OutputPath

if (Test-Path -LiteralPath $stagingRoot) {
    Remove-Item -LiteralPath $stagingRoot -Recurse -Force
}
New-Item -ItemType Directory -Force -Path $stagingRoot | Out-Null
New-Item -ItemType Directory -Force -Path (Split-Path $resolvedOutputPath) | Out-Null

$backendDir = Join-Path $repoRoot "ai_backend"
$files = @(
    "__init__.py",
    "db.py",
    "ollama_backend.py",
    "passenger_wsgi.py",
    "requirements.txt"
)

foreach ($file in $files) {
    Copy-Item -LiteralPath (Join-Path $backendDir $file) -Destination (Join-Path $stagingRoot $file) -Force
}

@"
PineGuard cPanel Python App

Set these cPanel Python App environment variables before restart:

DATABASE_URL=mysql+pymysql://mytrusth_pgapp:YOUR_PASSWORD@localhost:3306/mytrusth_pineguard?charset=utf8mb4
ALLOW_DATABASE_FALLBACK=false
DATABASE_FALLBACK_URL=
AI_LAYER_BASE_URL=http://161.118.235.40:8000
AI_LAYER_TIMEOUT_SECONDS=240
REQUEST_TIMEOUT_SECONDS=240
PUBLIC_BASE_URL=https://api.your-domain.com
UPLOADS_DIR=/home/YOUR_CPANEL_USER/pineguard-api/uploads

Startup file: passenger_wsgi.py
Entry point: application
"@ | Set-Content -LiteralPath (Join-Path $stagingRoot "README.txt") -Encoding UTF8

if (Test-Path -LiteralPath $resolvedOutputPath) {
    Remove-Item -LiteralPath $resolvedOutputPath -Force
}

Compress-Archive -Path (Join-Path $stagingRoot "*") -DestinationPath $resolvedOutputPath -Force

[pscustomobject]@{
    Package = $resolvedOutputPath
    Staging = $stagingRoot
}
