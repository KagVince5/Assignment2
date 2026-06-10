"""
PineGuard API backend.

This service combines:
- OCI or Ollama image analysis
- a cPanel-MySQL-backed API
- upload storage for camera and import images
- database-backed relay jobs to replace Firebase
"""

from __future__ import annotations

import base64
import hashlib
import ipaddress
import json
import os
import re
import socket
import time
from collections import defaultdict, deque
from datetime import timedelta, timezone
from pathlib import Path
from typing import Any
from urllib.parse import urlparse
from uuid import uuid4

import requests
from fastapi import (
    BackgroundTasks,
    Depends,
    FastAPI,
    File,
    HTTPException,
    Request,
    UploadFile,
    status,
)
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel
from sqlalchemy import Select, desc, func, or_, select
from sqlalchemy.exc import OperationalError
from sqlalchemy.orm import Session

try:
    from .db import (
        ActivityRecord,
        AlertRecord,
        AlertRule,
        AnalysisJob,
        CameraFrame,
        ConsultationMessage,
        ConsultationSession,
        Expert,
        IoTWifiConfig,
        RecordReview,
        SensorNode,
        SensorReading,
        SessionToken,
        TreatmentRecord,
        TranslationCache,
        User,
        UserSettings,
        create_password_hash,
        create_session_token,
        database_fallback_active,
        database_fallback_reason,
        database_kind,
        db_session,
        datetime_to_unix_ms,
        default_display_name,
        ensure_default_alert_rule,
        ensure_directory,
        ensure_user_settings,
        get_db,
        hash_session_token,
        initialize_database,
        parse_client_timestamp,
        utcnow,
        verify_password,
    )
except ImportError:
    from db import (
        ActivityRecord,
        AlertRecord,
        AlertRule,
        AnalysisJob,
        CameraFrame,
        ConsultationMessage,
        ConsultationSession,
        Expert,
        IoTWifiConfig,
        RecordReview,
        SensorNode,
        SensorReading,
        SessionToken,
        TreatmentRecord,
        TranslationCache,
        User,
        UserSettings,
        create_password_hash,
        create_session_token,
        database_fallback_active,
        database_fallback_reason,
        database_kind,
        db_session,
        datetime_to_unix_ms,
        default_display_name,
        ensure_default_alert_rule,
        ensure_directory,
        ensure_user_settings,
        get_db,
        hash_session_token,
        initialize_database,
        parse_client_timestamp,
        utcnow,
        verify_password,
    )


OLLAMA_BASE_URL = os.getenv("OLLAMA_BASE_URL", "http://127.0.0.1:11434").rstrip("/")
OLLAMA_MODEL = os.getenv("OLLAMA_MODEL", "gemma4:e4b")
BACKEND_HOST = os.getenv("BACKEND_HOST", "0.0.0.0")
BACKEND_PORT = int(os.getenv("BACKEND_PORT", "8000"))
MAX_IMAGE_BYTES = int(os.getenv("MAX_IMAGE_BYTES", str(8 * 1024 * 1024)))
# CPU-only Gemma 4 inference can take longer than a quick health probe.
REQUEST_TIMEOUT_SECONDS = float(os.getenv("REQUEST_TIMEOUT_SECONDS", "180"))
AI_LAYER_BASE_URL = os.getenv("AI_LAYER_BASE_URL", "").rstrip("/")
AI_LAYER_FALLBACK_URLS = os.getenv(
    "AI_LAYER_FALLBACK_URLS",
    os.getenv("AI_LAYER_FALLBACK_BASE_URLS", ""),
)
AI_LAYER_TIMEOUT_SECONDS = float(
    os.getenv("AI_LAYER_TIMEOUT_SECONDS", str(REQUEST_TIMEOUT_SECONDS))
)
LOW_CONFIDENCE_THRESHOLD = float(os.getenv("LOW_CONFIDENCE_THRESHOLD", "0.72"))
REVIEW_DIR = Path(os.getenv("REVIEW_DIR", "training/review_queue"))
SESSION_WINDOW = int(os.getenv("SESSION_WINDOW", "5"))
PUBLIC_BASE_URL = os.getenv(
    "PUBLIC_BASE_URL", f"http://127.0.0.1:{BACKEND_PORT}"
).rstrip("/")
UPLOADS_DIR = ensure_directory(
    Path(os.getenv("UPLOADS_DIR", str(Path(__file__).resolve().parent / "uploads")))
)
PUBLIC_UPLOADS_ROUTE = "/uploads"

LIVE_SESSIONS: dict[str, deque[dict[str, Any]]] = defaultdict(
    lambda: deque(maxlen=SESSION_WINDOW)
)

DEFAULT_NODE_ID = "node_field_001"
DEFAULT_DEVICE_NAME = "PineGuard Field Node 001"
DEFAULT_FIELD_LOCATION = "Johor Operations A"
IOT_WIFI_DEVICE_LABELS = {
    "esp32": "ESP32 Sensor Node",
    "esp32_cam": "ESP32-CAM",
}
IOT_WIFI_RESPONSE_POLL_SECONDS = int(os.getenv("IOT_WIFI_CONFIG_POLL_SECONDS", "60"))
IOT_WIFI_CONFIG_TOKEN = os.getenv("IOT_WIFI_CONFIG_TOKEN", "").strip()

DEFAULT_TREATMENTS: dict[str, list[str]] = {
    "healthy": [
        "No urgent treatment needed.",
        "Keep monitoring the plant weekly for early symptom changes.",
        "Maintain good drainage, airflow, and balanced fertilization.",
    ],
    "heart_rot": [
        "Remove badly infected tissue or plants immediately.",
        "Improve drainage and reduce prolonged leaf wetness.",
        "Apply a suitable fungicide only if approved for your crop and region.",
    ],
    "mealybug_wilt": [
        "Inspect for mealybugs around leaf bases and roots.",
        "Control ants and mealybug populations early.",
        "Separate heavily affected plants from healthy ones if possible.",
    ],
    "fruit_rot": [
        "Remove rotting fruit to reduce spread.",
        "Improve airflow and lower excess moisture around the plant.",
        "Avoid damaging fruit during handling and inspection.",
    ],
    "nutrient_deficiency": [
        "Check soil and fertilizer balance before applying more nutrients.",
        "Inspect for common deficiency patterns such as chlorosis or edge browning.",
        "Adjust fertilization gradually and monitor plant response.",
    ],
    "water_stress": [
        "Check soil moisture before watering again.",
        "Improve drainage if the area stays waterlogged.",
        "Use a steadier watering schedule instead of large swings.",
    ],
    "not_pineapple": [
        "Capture or import an image that clearly contains a pineapple plant, leaf, crown, or fruit.",
        "Move closer to the crop and keep the pineapple object centered in the frame.",
        "Retake the image under steady light so PineGuard can run a plant diagnosis.",
    ],
    "analysis_unavailable": [
        "Retake or re-import the image when the AI analysis layer is reachable.",
        "Do not mark the plant healthy until a vision model has confirmed a pineapple object.",
        "Review the visible image manually if a field decision is urgent.",
    ],
}

SUPPORTED_LOCALES = {"en", "ms"}
DEFAULT_LOCALE = "en"

DISEASE_DISPLAY_NAMES: dict[str, dict[str, str]] = {
    "healthy": {"en": "Healthy", "ms": "Sihat"},
    "heart_rot": {"en": "Heart Rot", "ms": "Reput Jantung"},
    "mealybug_wilt": {"en": "Mealybug Wilt", "ms": "Layu Kutu Putih"},
    "fruit_rot": {"en": "Fruit Rot", "ms": "Reput Buah"},
    "nutrient_deficiency": {
        "en": "Nutrient Deficiency",
        "ms": "Kekurangan Nutrien",
    },
    "water_stress": {"en": "Water Stress", "ms": "Tekanan Air"},
    "not_pineapple": {
        "en": "No Pineapple Captured",
        "ms": "Tiada Nanas Dikesan",
    },
    "analysis_unavailable": {
        "en": "Analysis Unavailable",
        "ms": "Analisis Tidak Tersedia",
    },
}

MS_TEXT_TRANSLATIONS: dict[str, str] = {
    "No urgent treatment needed.": "Tiada rawatan segera diperlukan.",
    "Keep monitoring the plant weekly for early symptom changes.": "Pantau pokok setiap minggu untuk perubahan simptom awal.",
    "Maintain good drainage, airflow, and balanced fertilization.": "Kekalkan saliran, aliran udara, dan pembajaan seimbang.",
    "Remove badly infected tissue or plants immediately.": "Buang tisu atau pokok yang teruk dijangkiti dengan segera.",
    "Improve drainage and reduce prolonged leaf wetness.": "Baiki saliran dan kurangkan kelembapan daun yang berpanjangan.",
    "Apply a suitable fungicide only if approved for your crop and region.": "Gunakan racun kulat yang sesuai hanya jika diluluskan untuk tanaman dan kawasan anda.",
    "Inspect for mealybugs around leaf bases and roots.": "Periksa kutu putih di pangkal daun dan akar.",
    "Control ants and mealybug populations early.": "Kawal semut dan populasi kutu putih lebih awal.",
    "Separate heavily affected plants from healthy ones if possible.": "Asingkan pokok yang teruk terjejas daripada pokok sihat jika boleh.",
    "Remove rotting fruit to reduce spread.": "Buang buah yang reput untuk mengurangkan penyebaran.",
    "Improve airflow and lower excess moisture around the plant.": "Baiki aliran udara dan kurangkan kelembapan berlebihan di sekitar pokok.",
    "Avoid damaging fruit during handling and inspection.": "Elakkan merosakkan buah semasa pengendalian dan pemeriksaan.",
    "Check soil and fertilizer balance before applying more nutrients.": "Periksa keseimbangan tanah dan baja sebelum menambah nutrien.",
    "Inspect for common deficiency patterns such as chlorosis or edge browning.": "Periksa corak kekurangan biasa seperti klorosis atau tepi daun keperangan.",
    "Adjust fertilization gradually and monitor plant response.": "Laraskan pembajaan secara beransur-ansur dan pantau tindak balas pokok.",
    "Check soil moisture before watering again.": "Periksa kelembapan tanah sebelum menyiram semula.",
    "Improve drainage if the area stays waterlogged.": "Baiki saliran jika kawasan kekal bertakung air.",
    "Use a steadier watering schedule instead of large swings.": "Gunakan jadual siraman yang lebih stabil dan elakkan perubahan besar.",
    "Capture or import an image that clearly contains a pineapple plant, leaf, crown, or fruit.": "Tangkap atau import imej yang jelas mengandungi pokok, daun, mahkota, atau buah nanas.",
    "Move closer to the crop and keep the pineapple object centered in the frame.": "Dekati tanaman dan pastikan objek nanas berada di tengah bingkai.",
    "Retake the image under steady light so PineGuard can run a plant diagnosis.": "Ambil semula imej di bawah cahaya stabil supaya PineGuard boleh menjalankan diagnosis tanaman.",
    "Retake or re-import the image when the AI analysis layer is reachable.": "Ambil semula atau import semula imej apabila lapisan analisis AI boleh dicapai.",
    "Do not mark the plant healthy until a vision model has confirmed a pineapple object.": "Jangan tandakan pokok sebagai sihat sehingga model penglihatan mengesahkan objek nanas.",
    "Review the visible image manually if a field decision is urgent.": "Semak imej yang kelihatan secara manual jika keputusan ladang adalah segera.",
    "No live sensor reading was provided for this image.": "Tiada bacaan sensor langsung diberikan untuk imej ini.",
    "Sensors look stable for this image.": "Bacaan sensor kelihatan stabil untuk imej ini.",
    "No pineapple object was captured in the analyzed image.": "Tiada objek nanas ditangkap dalam imej yang dianalisis.",
    "The AI did not find a pineapple plant, leaf, crown, or fruit to diagnose.": "AI tidak menemui pokok, daun, mahkota, atau buah nanas untuk didiagnosis.",
    "AI image analysis is unavailable, so this scan is not marked healthy.": "Analisis imej AI tidak tersedia, jadi imbasan ini tidak ditandakan sebagai sihat.",
    "No vision model result was available for this image.": "Tiada keputusan model penglihatan tersedia untuk imej ini.",
    "Visually healthy, but sensors show stress risk.": "Secara visual sihat, tetapi sensor menunjukkan risiko tekanan.",
    "Plant appears healthy in the analyzed image.": "Pokok kelihatan sihat dalam imej yang dianalisis.",
    "Temperature exceeded the safe maximum.": "Suhu melebihi had maksimum selamat.",
    "Temperature dropped below the safe minimum.": "Suhu jatuh di bawah had minimum selamat.",
    "Temperature is elevated for pineapple growth.": "Suhu tinggi untuk pertumbuhan nanas.",
    "Humidity is lower than the recommended level.": "Kelembapan lebih rendah daripada tahap disyorkan.",
    "Soil moisture is critically low.": "Kelembapan tanah sangat rendah.",
    "Soil moisture dropped below the warning threshold.": "Kelembapan tanah jatuh di bawah ambang amaran.",
    "Soil moisture is unusually high.": "Kelembapan tanah luar biasa tinggi.",
    "Soil pH is too acidic for pineapple.": "pH tanah terlalu berasid untuk nanas.",
    "Soil pH is too alkaline for pineapple.": "pH tanah terlalu beralkali untuk nanas.",
}

MS_REPLACEMENTS: tuple[tuple[str, str], ...] = (
    ("AI Scan", "Imbasan AI"),
    ("Sensor snapshot", "Bacaan sensor"),
    ("Treatment plan", "Pelan rawatan"),
    ("Confidence", "Keyakinan"),
    (
        "Backend fallback guidance was used because the configured AI analysis layer was unavailable.",
        "Panduan sandaran backend digunakan kerana lapisan analisis AI yang dikonfigurasi tidak tersedia.",
    ),
    (
        "The image suggests signs most consistent with",
        "Imej menunjukkan tanda yang paling konsisten dengan",
    ),
    (
        "The plant appears visually healthy in this image.",
        "Pokok kelihatan sihat secara visual dalam imej ini.",
    ),
    (
        "No pineapple object was captured in this image.",
        "Tiada objek nanas ditangkap dalam imej ini.",
    ),
    ("Visible content", "Kandungan kelihatan"),
    ("Vision model suggests", "Model penglihatan mencadangkan"),
    ("at", "pada"),
    ("confidence", "keyakinan"),
    ("Possible", "Kemungkinan"),
    ("detected in image analysis.", "dikesan dalam analisis imej."),
    (
        "Soil moisture is low, which increases water stress risk.",
        "Kelembapan tanah rendah, yang meningkatkan risiko tekanan air.",
    ),
    (
        "Soil moisture is very high, which increases rot risk.",
        "Kelembapan tanah sangat tinggi, yang meningkatkan risiko reput.",
    ),
    (
        "Humidity is low, which can worsen dehydration stress.",
        "Kelembapan rendah, yang boleh memburukkan tekanan kekeringan.",
    ),
    (
        "Humidity is high, which favors fungal and rot conditions.",
        "Kelembapan tinggi, yang menggalakkan keadaan kulat dan reput.",
    ),
    (
        "Temperature is cooler than ideal for stable growth.",
        "Suhu lebih sejuk daripada ideal untuk pertumbuhan stabil.",
    ),
    (
        "Temperature is high, which can intensify plant stress.",
        "Suhu tinggi, yang boleh meningkatkan tekanan tanaman.",
    ),
    (
        "Soil pH is outside the optimal pineapple range of 4.5 to 6.5.",
        "pH tanah berada di luar julat optimum nanas 4.5 hingga 6.5.",
    ),
    ("Sensor status", "Status sensor"),
    ("temperature", "suhu"),
    ("humidity", "kelembapan"),
    ("soil moisture", "kelembapan tanah"),
    ("normal", "normal"),
    ("warning", "amaran"),
    ("critical", "kritikal"),
    ("healthy", "sihat"),
    ("heart rot", "reput jantung"),
    ("mealybug wilt", "layu kutu putih"),
    ("fruit rot", "reput buah"),
    ("nutrient deficiency", "kekurangan nutrien"),
    ("water stress", "tekanan air"),
    ("not pineapple", "tiada nanas"),
    ("analysis unavailable", "analisis tidak tersedia"),
)

NOT_PINEAPPLE_PHRASES: tuple[str, ...] = (
    "not pineapple",
    "not a pineapple",
    "not a pineapple plant",
    "no pineapple",
    "no visible pineapple",
    "no pineapple detected",
    "no pineapple object",
    "no pineapple object captured",
    "no pineapple plant",
    "no pineapple plant detected",
    "no pineapple plant visible",
    "non pineapple",
    "no pinapple",
    "no pinapple object",
    "pinapple not detected",
    "not pinapple",
    "pineapple not detected",
    "pineapple object not captured",
    "pineapple not visible",
    "cannot detect pineapple",
    "can't detect pineapple",
    "unable to detect pineapple",
    "does not contain pineapple",
    "does not contain a pineapple",
    "doesn't contain pineapple",
    "does not show pineapple",
    "does not show a pineapple",
    "does not show any pineapple",
    "doesn't show pineapple",
    "does not clearly show pineapple",
    "does not clearly show any pineapple",
    "does not clearly show pineapple foliage",
    "does not clearly show any pineapple foliage",
    "no pineapple foliage",
    "no pineapple leaf",
    "no pineapple leaves",
    "no pineapple plant detail",
    "not capture for the pineapple object",
    "not captured for the pineapple object",
    "object not captured",
    "no plant",
    "no crop",
    "unrelated image",
)

LABEL_KEYWORDS: list[tuple[str, tuple[str, ...]]] = [
    (
        "healthy",
        ("healthy", "normal", "no disease", "no visible disease", "no major issue"),
    ),
    ("heart_rot", ("heart rot", "phytophthora", "crown rot", "central rot")),
    (
        "mealybug_wilt",
        ("mealybug", "mealybugs", "wilt virus", "mealybug wilt", "pink disease"),
    ),
    (
        "fruit_rot",
        ("fruit rot", "fusarium", "fungal fruit rot", "rotting fruit", "soft rot"),
    ),
    (
        "nutrient_deficiency",
        (
            "nutrient deficiency",
            "chlorosis",
            "nitrogen deficiency",
            "potassium deficiency",
            "iron deficiency",
        ),
    ),
    (
        "water_stress",
        (
            "water stress",
            "drought",
            "underwater",
            "overwater",
            "overwatering",
            "dehydration",
        ),
    ),
    ("not_pineapple", NOT_PINEAPPLE_PHRASES),
]

SYSTEM_PROMPT = """You are an agricultural assistant for pineapple disease monitoring.
Analyze pineapple plant images and respond with JSON only.
Do not add markdown fences, explanations, or extra prose.
If the image does not show a pineapple plant, leaf, crown, or fruit, never return "healthy"; return disease "not_pineapple" and explain what is visible instead."""

USER_PROMPT = """Classify this pineapple leaf or plant image into exactly one label from this list:
- healthy
- heart_rot
- mealybug_wilt
- fruit_rot
- nutrient_deficiency
- water_stress
- not_pineapple

Required JSON schema:
{
  "disease": "healthy | heart_rot | mealybug_wilt | fruit_rot | nutrient_deficiency | water_stress | not_pineapple",
  "confidence": 0.0,
  "containsPineapple": true,
  "sceneDescription": "what the image mainly shows",
  "visibleObjects": ["object 1", "object 2"],
  "description": "one short paragraph",
  "treatment": ["short action 1", "short action 2", "short action 3"]
}

Focus on visible symptoms only. If the image is unclear but a pineapple object is visible, choose the closest label and lower the confidence.
If no pineapple object is visible, set containsPineapple to false, use not_pineapple, never use healthy, mention that no pineapple object was captured, and describe the main visible subject in sceneDescription and description."""

app = FastAPI(title="Pineapple Monitor Local Ollama Backend", version="1.0.0")
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)
initialize_database()
app.mount(PUBLIC_UPLOADS_ROUTE, StaticFiles(directory=UPLOADS_DIR), name="uploads")


class LoginRequest(BaseModel):
    email: str
    password: str


class SensorReadingRequest(BaseModel):
    nodeId: str = DEFAULT_NODE_ID
    deviceName: str = DEFAULT_DEVICE_NAME
    location: str = DEFAULT_FIELD_LOCATION
    firmwareVersion: str = "1.0.0"
    temperature: float
    humidity: float
    soilMoisture: float
    pH: float
    status: str | None = None
    timestamp: str | int | float | None = None


class RecordRequest(BaseModel):
    type: str
    title: str
    description: str
    timestamp: int | str | float | None = None
    treatment: dict[str, Any] | None = None
    scanResult: dict[str, Any] | None = None


TEMP_HIGH_THRESHOLD = 35.0
TEMP_LOW_THRESHOLD = 15.0
HUMIDITY_LOW_THRESHOLD = 40.0
SOIL_DRY_THRESHOLD = 30.0
SOIL_CRITICAL_DRY_THRESHOLD = 20.0
SOIL_WET_THRESHOLD = 90.0
PH_LOW_THRESHOLD = 4.5
PH_HIGH_THRESHOLD = 6.5
RECORD_TYPE_ALIASES = {
    "activity": "manual",
    "manual": "manual",
    "scan": "scan",
    "treatment": "treatment",
    "inspection": "inspection",
}


class UserSettingsRequest(BaseModel):
    pushNotificationsEnabled: bool = True
    dailyDigestEnabled: bool = True
    assistantRecommendationsEnabled: bool = True
    syncCadence: str = "realtime"
    language: str = "en"
    theme: str = "system"
    backendMode: str | None = None


class IoTWifiDeviceConfigRequest(BaseModel):
    ssid: str = ""
    password: str | None = None


class IoTWifiSettingsRequest(BaseModel):
    esp32: IoTWifiDeviceConfigRequest | None = None
    esp32Cam: IoTWifiDeviceConfigRequest | None = None


class AlertRuleRequest(BaseModel):
    tempHigh: float = TEMP_HIGH_THRESHOLD
    tempLow: float = TEMP_LOW_THRESHOLD
    humidityLow: float = HUMIDITY_LOW_THRESHOLD
    soilDry: float = SOIL_DRY_THRESHOLD
    soilCriticalDry: float = SOIL_CRITICAL_DRY_THRESHOLD
    soilWet: float = SOIL_WET_THRESHOLD
    phLow: float = PH_LOW_THRESHOLD
    phHigh: float = PH_HIGH_THRESHOLD
    cooldownMinutes: int = 30


class RecordReviewRequest(BaseModel):
    recordId: str
    reviewStatus: str = "reviewed"
    reviewNote: str = ""
    assignedToUserId: str | None = None


class AdminUserCreateRequest(BaseModel):
    displayName: str
    email: str
    role: str = "farmer"
    location: str = DEFAULT_FIELD_LOCATION
    password: str = ""


class AdminUserRemoveRequest(BaseModel):
    adminPassword: str


def safe_json_loads(raw_value: str | None) -> dict[str, Any] | None:
    if not raw_value:
        return None
    try:
        parsed = json.loads(raw_value)
    except json.JSONDecodeError:
        return None
    return parsed if isinstance(parsed, dict) else None


def normalize_language(value: Any) -> str:
    raw_value = str(value or "").strip().lower().replace("_", "-")
    if raw_value.startswith("ms"):
        return "ms"
    if raw_value.startswith("en"):
        return "en"
    return DEFAULT_LOCALE


def request_language(request: Request | None, explicit_lang: str | None = None) -> str:
    if explicit_lang:
        return normalize_language(explicit_lang)
    if request is None:
        return DEFAULT_LOCALE
    accept_language = request.headers.get("accept-language", "")
    for candidate in accept_language.split(","):
        raw_language = candidate.split(";", 1)[0].strip().lower().replace("_", "-")
        if raw_language.startswith("ms"):
            return "ms"
        if raw_language.startswith("en"):
            return "en"
    return DEFAULT_LOCALE


def source_payload_hash(payload: dict[str, Any]) -> str:
    encoded = json.dumps(payload, sort_keys=True, ensure_ascii=False).encode("utf-8")
    return hashlib.sha256(encoded).hexdigest()


def translated_disease_name(label: Any, locale: str) -> str:
    normalized = str(label or "").strip().lower().replace("-", "_").replace(" ", "_")
    if normalized not in DISEASE_DISPLAY_NAMES:
        normalized = normalize_disease_label(str(label))
    names = DISEASE_DISPLAY_NAMES.get(normalized)
    if names:
        return names.get(locale) or names["en"]
    return disease_display_name(label)


def translate_text(
    value: Any,
    locale: str,
    ai_translations: dict[str, str] | None = None,
) -> str:
    text_value = str(value or "").strip()
    if locale != "ms" or not text_value:
        return text_value

    if text_value.startswith("AI Scan - "):
        return f"Imbasan AI - {translated_disease_name(text_value.split(' - ', 1)[1], locale)}"

    exact = MS_TEXT_TRANSLATIONS.get(text_value)
    if exact:
        return exact

    ai_translation = (ai_translations or {}).get(text_value)
    if ai_translation:
        return ai_translation

    translated = text_value
    for source, target in MS_REPLACEMENTS:
        if re.fullmatch(r"[A-Za-z ]+", source):
            translated = re.sub(
                rf"\b{re.escape(source)}\b",
                target,
                translated,
                flags=re.IGNORECASE,
            )
        else:
            translated = translated.replace(source, target)
    return translated


def translate_text_list(
    values: Any,
    locale: str,
    ai_translations: dict[str, str] | None = None,
) -> list[str]:
    if not isinstance(values, list):
        return []
    return [
        translate_text(item, locale, ai_translations)
        for item in values
        if str(item).strip()
    ]


def collect_translation_text(value: Any, texts: list[str]) -> None:
    text_value = str(value or "").strip()
    if (
        text_value
        and text_value not in MS_TEXT_TRANSLATIONS
        and text_value not in texts
    ):
        texts.append(text_value)


def collect_translation_text_list(values: Any, texts: list[str]) -> None:
    if isinstance(values, list):
        for item in values:
            collect_translation_text(item, texts)


def collect_remote_translation_candidates(
    source_type: str, source_payload: dict[str, Any]
) -> list[str]:
    texts: list[str] = []
    if source_type == "alert":
        collect_translation_text(source_payload.get("message"), texts)
        return texts

    if source_type == "record":
        collect_translation_text(source_payload.get("title"), texts)
        collect_translation_text(source_payload.get("description"), texts)
        treatment = source_payload.get("treatment")
        if isinstance(treatment, dict):
            collect_translation_text_list(treatment.get("steps"), texts)

    scan_result = source_payload.get("scanResult")
    if isinstance(scan_result, dict):
        collect_translation_text(scan_result.get("description"), texts)
        collect_translation_text(scan_result.get("sceneDescription"), texts)
        collect_translation_text_list(scan_result.get("treatment"), texts)
        collect_translation_text_list(scan_result.get("visibleObjects"), texts)

    live_assessment = source_payload.get("liveAssessment")
    if isinstance(live_assessment, dict):
        for key in ("plantStatus", "sensorSummary", "recommendation"):
            collect_translation_text(live_assessment.get(key), texts)
        collect_translation_text_list(live_assessment.get("reasons"), texts)

    return texts


def translate_texts_with_remote_ai_layer(
    texts: list[str], locale: str
) -> dict[str, str]:
    if locale != "ms" or not texts:
        return {}

    for base_url in configured_remote_ai_layer_urls():
        try:
            response = requests.post(
                f"{base_url}/translate",
                json={"locale": locale, "texts": texts},
                timeout=min(AI_LAYER_TIMEOUT_SECONDS, 45),
            )
            response.raise_for_status()
            payload = response.json()
        except (requests.RequestException, ValueError):
            continue

        translated_texts = payload.get("texts")
        if not isinstance(translated_texts, list) or len(translated_texts) != len(
            texts
        ):
            continue
        return {
            original: str(translated or original).strip() or original
            for original, translated in zip(texts, translated_texts)
        }

    return {}


def translate_scan_result_payload(
    scan_result: dict[str, Any] | None,
    locale: str,
    ai_translations: dict[str, str] | None = None,
) -> dict[str, Any] | None:
    if not isinstance(scan_result, dict):
        return None
    disease = scan_result.get("disease")
    payload: dict[str, Any] = {
        "diseaseDisplayName": translated_disease_name(disease, locale),
    }
    if scan_result.get("description") is not None:
        payload["description"] = translate_text(
            scan_result.get("description"), locale, ai_translations
        )
    if scan_result.get("sceneDescription") is not None:
        payload["sceneDescription"] = translate_text(
            scan_result.get("sceneDescription"), locale, ai_translations
        )
    if isinstance(scan_result.get("treatment"), list):
        payload["treatment"] = translate_text_list(
            scan_result.get("treatment"), locale, ai_translations
        )
    if isinstance(scan_result.get("visibleObjects"), list):
        payload["visibleObjects"] = translate_text_list(
            scan_result.get("visibleObjects"), locale, ai_translations
        )
    return payload


def translate_live_assessment_payload(
    live_assessment: dict[str, Any] | None,
    locale: str,
    ai_translations: dict[str, str] | None = None,
) -> dict[str, Any] | None:
    if not isinstance(live_assessment, dict):
        return None
    payload: dict[str, Any] = {}
    for key in ("plantStatus", "sensorSummary", "recommendation"):
        if live_assessment.get(key) is not None:
            payload[key] = translate_text(
                live_assessment.get(key), locale, ai_translations
            )
    if isinstance(live_assessment.get("reasons"), list):
        payload["reasons"] = translate_text_list(
            live_assessment.get("reasons"), locale, ai_translations
        )
    stable_label = live_assessment.get("stableLabel")
    if stable_label is not None:
        payload["stableLabelDisplayName"] = translated_disease_name(
            stable_label, locale
        )
    return payload


def translate_record_payload(
    source_payload: dict[str, Any],
    locale: str,
    ai_translations: dict[str, str] | None = None,
) -> dict[str, Any]:
    localized: dict[str, Any] = {}
    if source_payload.get("title") is not None:
        localized["title"] = translate_text(
            source_payload.get("title"), locale, ai_translations
        )
    if source_payload.get("description") is not None:
        localized["description"] = translate_text(
            source_payload.get("description"), locale, ai_translations
        )

    scan_result = translate_scan_result_payload(
        source_payload.get("scanResult"), locale, ai_translations
    )
    if scan_result:
        localized["scanResult"] = scan_result

    treatment = source_payload.get("treatment")
    if isinstance(treatment, dict) and isinstance(treatment.get("steps"), list):
        localized["treatment"] = {
            "steps": translate_text_list(
                treatment.get("steps"), locale, ai_translations
            )
        }
    return localized


def translate_job_payload(
    source_payload: dict[str, Any],
    locale: str,
    ai_translations: dict[str, str] | None = None,
) -> dict[str, Any]:
    localized: dict[str, Any] = {}
    scan_result = translate_scan_result_payload(
        source_payload.get("scanResult"), locale, ai_translations
    )
    if scan_result:
        localized["scanResult"] = scan_result
    live_assessment = translate_live_assessment_payload(
        source_payload.get("liveAssessment"), locale, ai_translations
    )
    if live_assessment:
        localized["liveAssessment"] = live_assessment
    return localized


def translate_alert_payload(
    source_payload: dict[str, Any],
    locale: str,
    ai_translations: dict[str, str] | None = None,
) -> dict[str, Any]:
    return {
        "message": translate_text(
            source_payload.get("message"), locale, ai_translations
        )
    }


def translate_direct_analysis_payload(
    source_payload: dict[str, Any],
    locale: str,
    ai_translations: dict[str, str] | None = None,
) -> dict[str, Any]:
    return translate_job_payload(source_payload, locale, ai_translations)


def translate_source_payload(
    source_type: str, source_payload: dict[str, Any], locale: str
) -> dict[str, Any]:
    ai_translations = translate_texts_with_remote_ai_layer(
        collect_remote_translation_candidates(source_type, source_payload),
        locale,
    )
    if source_type == "record":
        return translate_record_payload(source_payload, locale, ai_translations)
    if source_type == "analysis_job":
        return translate_job_payload(source_payload, locale, ai_translations)
    if source_type == "alert":
        return translate_alert_payload(source_payload, locale, ai_translations)
    if source_type == "direct_analysis":
        return translate_direct_analysis_payload(
            source_payload, locale, ai_translations
        )
    return {}


def cached_localized_payload(
    db: Session,
    *,
    source_type: str,
    source_id: str,
    locale: str,
    source_payload: dict[str, Any],
) -> dict[str, Any] | None:
    if locale == DEFAULT_LOCALE:
        return None

    normalized_locale = normalize_language(locale)
    if normalized_locale == DEFAULT_LOCALE:
        return None

    digest = source_payload_hash(source_payload)
    cache = db.scalar(
        select(TranslationCache)
        .where(TranslationCache.source_type == source_type)
        .where(TranslationCache.source_id == source_id)
        .where(TranslationCache.locale == normalized_locale)
        .limit(1)
    )
    if cache is not None and cache.source_hash == digest:
        payload = safe_json_loads(cache.payload_json)
        if payload is not None:
            return payload

    translated = translate_source_payload(
        source_type, source_payload, normalized_locale
    )
    if cache is None:
        cache = TranslationCache(
            source_type=source_type,
            source_id=source_id,
            locale=normalized_locale,
            source_hash=digest,
            payload_json=json.dumps(translated, ensure_ascii=False),
            updated_at=utcnow(),
        )
        db.add(cache)
    else:
        cache.source_hash = digest
        cache.payload_json = json.dumps(translated, ensure_ascii=False)
        cache.updated_at = utcnow()
    db.flush()
    return translated


def is_local_hostname(hostname: str | None) -> bool:
    if not hostname:
        return True

    value = hostname.strip().lower()
    if value in {"localhost", "0.0.0.0", "pineguard-backend"}:
        return True
    if value.endswith(".local"):
        return True

    try:
        ip_address = ipaddress.ip_address(value)
    except ValueError:
        return "." not in value

    return ip_address.is_loopback or ip_address.is_private or ip_address.is_link_local


def is_local_url(url: str | None) -> bool:
    parsed = urlparse(str(url or ""))
    if parsed.scheme not in {"http", "https"} or not parsed.netloc:
        return True
    return is_local_hostname(parsed.hostname)


def request_public_base_url(request: Request | None) -> str | None:
    if request is None:
        return None

    forwarded_host = request.headers.get("x-forwarded-host", "").split(",")[0].strip()
    host = forwarded_host or request.headers.get("host", "").split(",")[0].strip()
    if not host:
        return None

    forwarded_proto = request.headers.get("x-forwarded-proto", "").split(",")[0].strip()
    scheme = forwarded_proto or request.url.scheme
    return f"{scheme}://{host}".rstrip("/")


def effective_public_base_url(request: Request | None = None) -> str:
    request_base_url = request_public_base_url(request)
    if (
        request_base_url
        and is_local_url(PUBLIC_BASE_URL)
        and not is_local_url(request_base_url)
    ):
        return request_base_url
    return PUBLIC_BASE_URL


def upload_relative_path_from_url(url: str | None) -> str | None:
    parsed = urlparse(str(url or ""))
    upload_prefix = f"{PUBLIC_UPLOADS_ROUTE}/"
    if parsed.path.startswith(upload_prefix):
        return parsed.path[len(upload_prefix) :]
    return None


def public_upload_url(
    relative_path: str | None, request: Request | None = None
) -> str | None:
    if not relative_path:
        return None
    cleaned = relative_path.replace("\\", "/").lstrip("/")
    return f"{effective_public_base_url(request)}{PUBLIC_UPLOADS_ROUTE}/{cleaned}"


def public_or_stored_upload_url(
    stored_url: str | None,
    relative_path: str | None,
    request: Request | None = None,
) -> str | None:
    if stored_url and not is_local_url(stored_url):
        return stored_url

    stored_relative_path = upload_relative_path_from_url(stored_url)
    return public_upload_url(relative_path or stored_relative_path, request)


def backend_discovery_urls(request: Request | None = None) -> list[str]:
    candidates: list[str] = []

    try:
        host_name = socket.gethostname().strip().lower()
    except OSError:
        host_name = ""

    for name in [host_name, "pineguard-backend"]:
        if not name:
            continue
        candidates.append(f"http://{name}:{BACKEND_PORT}")
        if not name.endswith(".local"):
            candidates.append(f"http://{name}.local:{BACKEND_PORT}")

    candidates.extend(
        [
            effective_public_base_url(request),
            f"http://127.0.0.1:{BACKEND_PORT}",
            f"http://localhost:{BACKEND_PORT}",
        ]
    )

    try:
        for ip_address in socket.gethostbyname_ex(socket.gethostname())[2]:
            if not ip_address.startswith(("127.", "169.254.")):
                candidates.append(f"http://{ip_address}:{BACKEND_PORT}")
    except OSError:
        pass

    urls: list[str] = []
    for candidate in candidates:
        value = str(candidate or "").rstrip("/")
        if value and value not in urls:
            urls.append(value)
    return urls


def backend_identity_payload(request: Request | None = None) -> dict[str, Any]:
    return {
        "publicBaseUrl": effective_public_base_url(request),
        "discoveryUrls": backend_discovery_urls(request),
    }


def save_upload_bytes(
    image_bytes: bytes,
    folder: str,
    file_name: str | None = None,
    request: Request | None = None,
) -> tuple[str, str]:
    target_dir = ensure_directory(UPLOADS_DIR / folder)
    final_name = file_name or f"{int(time.time())}_{uuid4().hex[:8]}.jpg"
    file_path = target_dir / final_name
    file_path.write_bytes(image_bytes)
    relative_path = str(file_path.relative_to(UPLOADS_DIR)).replace("\\", "/")
    return relative_path, public_upload_url(relative_path, request) or ""


def optional_bearer_token(request: Request) -> str | None:
    auth_header = request.headers.get("authorization", "").strip()
    if auth_header.lower().startswith("bearer "):
        token = auth_header[7:].strip()
        return token or None
    return None


def ensure_utc_datetime(value):
    if value is None:
        return None
    if value.tzinfo is None:
        return value.replace(tzinfo=timezone.utc)
    return value.astimezone(timezone.utc)


def is_transient_database_lock_error(error: OperationalError) -> bool:
    original = getattr(error, "orig", None)
    args = getattr(original, "args", ()) or ()
    code = str(args[0]) if args else ""
    message = str(error).lower()
    return code in {"1205", "1213"} or "deadlock" in message or "lock wait timeout" in message


def commit_activity_update(db: Session) -> None:
    try:
        db.commit()
    except OperationalError as error:
        db.rollback()
        if not is_transient_database_lock_error(error):
            raise


def get_current_user(request: Request, db: Session = Depends(get_db)) -> User:
    token = optional_bearer_token(request)
    if not token:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED, detail="Missing bearer token"
        )

    token_hash = hash_session_token(token)
    session_token = db.scalar(
        select(SessionToken).where(SessionToken.token_hash == token_hash)
    )
    expires_at = (
        ensure_utc_datetime(session_token.expires_at) if session_token else None
    )
    revoked_at = (
        ensure_utc_datetime(session_token.revoked_at) if session_token else None
    )
    if (
        not session_token
        or revoked_at is not None
        or (expires_at is not None and expires_at <= utcnow())
    ):
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED, detail="Invalid session token"
        )

    user = db.get(User, session_token.user_id)
    if not user or user.account_status != "active":
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Session user no longer exists",
        )
    user.last_active_at = utcnow()
    commit_activity_update(db)
    return user


def get_optional_user(request: Request, db: Session = Depends(get_db)) -> User | None:
    token = optional_bearer_token(request)
    if not token:
        return None

    token_hash = hash_session_token(token)
    session_token = db.scalar(
        select(SessionToken).where(SessionToken.token_hash == token_hash)
    )
    expires_at = (
        ensure_utc_datetime(session_token.expires_at) if session_token else None
    )
    revoked_at = (
        ensure_utc_datetime(session_token.revoked_at) if session_token else None
    )
    if (
        not session_token
        or revoked_at is not None
        or (expires_at is not None and expires_at <= utcnow())
    ):
        return None
    user = db.get(User, session_token.user_id)
    if user is None or user.account_status != "active":
        return None
    user.last_active_at = utcnow()
    commit_activity_update(db)
    return user


def get_admin_user(user: User = Depends(get_current_user)) -> User:
    if not is_admin_role(user):
        raise HTTPException(status_code=403, detail="Admin access required")
    return user


def serialize_user(user: User) -> dict[str, Any]:
    return {
        "uid": user.id,
        "email": user.email,
        "displayName": user.display_name,
        "photoURL": None,
        "role": user.role,
        "location": user.farm_location,
        "accountStatus": user.account_status,
        "lastLoginAt": datetime_to_unix_ms(user.last_login),
        "lastActiveAt": datetime_to_unix_ms(user.last_active_at),
    }


def serialize_sensor_reading(row: SensorReading) -> dict[str, Any]:
    return {
        "id": row.id,
        "nodeId": row.node_id,
        "temperature": row.temperature,
        "humidity": row.humidity,
        "soilMoisture": row.soil_moisture,
        "pH": row.ph_level,
        "status": row.status,
        "timestamp": datetime_to_unix_ms(row.recorded_at),
        "receivedAt": datetime_to_unix_ms(row.created_at),
    }


def serialize_alert(
    row: AlertRecord,
    *,
    db: Session | None = None,
    locale: str = DEFAULT_LOCALE,
) -> dict[str, Any]:
    payload = {
        "id": str(row.id),
        "severity": row.severity,
        "sensorType": row.sensor_type,
        "value": row.sensor_value,
        "threshold": row.threshold_value,
        "message": row.message,
        "timestamp": datetime_to_unix_ms(row.created_at),
        "read": row.is_read,
    }
    if db is not None:
        localized = cached_localized_payload(
            db,
            source_type="alert",
            source_id=str(row.id),
            locale=locale,
            source_payload={"message": row.message},
        )
        if localized:
            payload["localized"] = localized
    return payload


def normalize_scan_result_payload(
    payload: dict[str, Any] | None,
    request: Request | None = None,
) -> dict[str, Any] | None:
    if not isinstance(payload, dict):
        return None

    normalized = payload.copy()
    image_uri = normalized.get("imageUri")
    if isinstance(image_uri, str) and is_local_url(image_uri):
        normalized_uri = public_or_stored_upload_url(image_uri, None, request)
        if normalized_uri:
            normalized["imageUri"] = normalized_uri
    return normalized


def serialize_record(
    row: ActivityRecord,
    request: Request | None = None,
    *,
    db: Session | None = None,
    locale: str = DEFAULT_LOCALE,
) -> dict[str, Any]:
    scan_result = normalize_scan_result_payload(
        safe_json_loads(row.scan_result_json), request
    )
    treatment = safe_json_loads(row.treatment_json)
    payload = {
        "id": row.id,
        "type": serialize_record_type(row.record_type),
        "title": row.title,
        "description": row.description,
        "timestamp": datetime_to_unix_ms(row.record_timestamp),
        "userId": row.user_id,
        "scanResult": scan_result,
        "treatment": treatment,
    }
    if db is not None:
        localized = cached_localized_payload(
            db,
            source_type="record",
            source_id=row.id,
            locale=locale,
            source_payload={
                "title": row.title,
                "description": row.description,
                "scanResult": scan_result,
                "treatment": treatment,
            },
        )
        if localized:
            payload["localized"] = localized
    return payload


def scan_record_dedupe_key(scan_result: dict[str, Any] | None) -> str | None:
    if not isinstance(scan_result, dict):
        return None

    image_uri = scan_result.get("imageUri") or scan_result.get("imageUrl")
    if not isinstance(image_uri, str) or not image_uri.strip():
        return None

    value = image_uri.strip()
    relative_path = upload_relative_path_from_url(value)
    if relative_path:
        return f"uploads:{relative_path}"

    return f"url:{value.rstrip('/')}"


def dedupe_serialized_records(records: list[dict[str, Any]]) -> list[dict[str, Any]]:
    deduped: list[dict[str, Any]] = []
    seen: set[str] = set()

    for record in records:
        key = None
        if record.get("type") == "scan":
            scan_key = scan_record_dedupe_key(record.get("scanResult"))
            if scan_key:
                key = f"scan:{scan_key}"

        key = key or f"record:{record.get('id')}"
        if key in seen:
            continue

        seen.add(key)
        deduped.append(record)

    return deduped


def serialize_job(
    job: AnalysisJob,
    request: Request | None = None,
    *,
    db: Session | None = None,
    locale: str = DEFAULT_LOCALE,
) -> dict[str, Any]:
    meta = safe_json_loads(job.meta_json) or {}
    fallback_reason = meta.get("fallbackReason")
    inference_source = (
        "fallback_rules"
        if meta.get("source") == "backend_fallback"
        else "backend_enhanced"
    )
    image_source = (
        "esp32_frame"
        if job.transport == "esp32_camera_auto_analysis"
        else "user_import"
    )
    scan_result = normalize_scan_result_payload(
        safe_json_loads(job.scan_result_json), request
    )
    live_assessment = safe_json_loads(job.live_assessment_json)
    payload = {
        "id": job.id,
        "userId": job.user_id,
        "nodeId": job.node_id,
        "status": job.status,
        "imageUrl": public_or_stored_upload_url(job.image_uri, job.image_path, request),
        "imageSource": image_source,
        "transport": job.transport,
        "createdAt": datetime_to_unix_ms(job.created_at),
        "updatedAt": datetime_to_unix_ms(job.updated_at),
        "completedAt": datetime_to_unix_ms(job.completed_at),
        "error": job.error,
        "scanResult": scan_result,
        "liveAssessment": live_assessment,
        "meta": meta or None,
        "inferenceSource": inference_source,
        "fallbackReason": "inference_failed" if fallback_reason else None,
    }
    if db is not None:
        localized = cached_localized_payload(
            db,
            source_type="analysis_job",
            source_id=job.id,
            locale=locale,
            source_payload={
                "scanResult": scan_result,
                "liveAssessment": live_assessment,
            },
        )
        if localized:
            payload["localized"] = localized
    return payload


def is_admin_role(user: User | None) -> bool:
    if user is None:
        return False
    return user.role in {"admin", "officer"}


def serialize_settings(settings: UserSettings) -> dict[str, Any]:
    payload = safe_json_loads(settings.notification_preferences_json) or {}
    return {
        "pushNotificationsEnabled": bool(payload.get("pushNotificationsEnabled", True)),
        "dailyDigestEnabled": bool(payload.get("dailyDigestEnabled", True)),
        "assistantRecommendationsEnabled": bool(
            payload.get("assistantRecommendationsEnabled", True)
        ),
        "syncCadence": settings.sync_cadence,
        "language": settings.language,
        "theme": settings.theme,
        "backendMode": settings.backend_mode,
        "updatedAt": datetime_to_unix_ms(settings.updated_at),
    }


def apply_settings_update(settings: UserSettings, payload: UserSettingsRequest) -> None:
    settings.notification_preferences_json = json.dumps(
        {
            "pushNotificationsEnabled": payload.pushNotificationsEnabled,
            "dailyDigestEnabled": payload.dailyDigestEnabled,
            "assistantRecommendationsEnabled": payload.assistantRecommendationsEnabled,
        }
    )
    settings.sync_cadence = payload.syncCadence
    settings.language = normalize_language(payload.language)
    settings.theme = payload.theme
    settings.backend_mode = payload.backendMode or database_kind()
    settings.updated_at = utcnow()


def normalize_iot_wifi_device_type(value: str | None) -> str:
    normalized = re.sub(r"[^a-z0-9]+", "_", (value or "").strip().lower()).strip("_")
    aliases = {
        "esp32": "esp32",
        "sensor": "esp32",
        "sensor_node": "esp32",
        "esp32_sensor": "esp32",
        "esp32_node": "esp32",
        "esp32_cam": "esp32_cam",
        "esp32cam": "esp32_cam",
        "camera": "esp32_cam",
        "cam": "esp32_cam",
    }
    device_type = aliases.get(normalized)
    if device_type is None:
        raise HTTPException(status_code=400, detail="Unknown IoT device type")
    return device_type


def iot_wifi_revision(device_type: str, ssid: str, password: str) -> str:
    raw_value = f"{device_type}\n{ssid}\n{password}\n{time.time_ns()}"
    return hashlib.sha256(raw_value.encode("utf-8")).hexdigest()[:16]


def serialize_iot_wifi_admin(
    config: IoTWifiConfig | None, device_type: str
) -> dict[str, Any]:
    return {
        "deviceType": device_type,
        "displayName": IOT_WIFI_DEVICE_LABELS[device_type],
        "ssid": config.ssid if config else "",
        "passwordConfigured": bool(config and config.password),
        "configured": bool(config and config.ssid),
        "revision": config.revision if config else "",
        "updatedAt": datetime_to_unix_ms(config.updated_at) if config else None,
    }


def serialize_iot_wifi_device(
    config: IoTWifiConfig | None, device_type: str
) -> dict[str, Any]:
    configured = bool(config and config.ssid)
    return {
        "deviceType": device_type,
        "displayName": IOT_WIFI_DEVICE_LABELS[device_type],
        "configured": configured,
        "ssid": config.ssid if configured and config else "",
        "password": config.password if configured and config else "",
        "revision": config.revision if config else "",
        "updatedAt": datetime_to_unix_ms(config.updated_at) if config else None,
        "pollAfterSeconds": max(30, IOT_WIFI_RESPONSE_POLL_SECONDS),
    }


def iot_wifi_settings_payload(db: Session) -> dict[str, Any]:
    return {
        "configs": {
            "esp32": serialize_iot_wifi_admin(db.get(IoTWifiConfig, "esp32"), "esp32"),
            "esp32Cam": serialize_iot_wifi_admin(
                db.get(IoTWifiConfig, "esp32_cam"), "esp32_cam"
            ),
        }
    }


def apply_iot_wifi_settings_update(
    db: Session,
    *,
    payload: IoTWifiSettingsRequest,
    user: User,
) -> None:
    if payload.esp32 is not None:
        apply_iot_wifi_device_update(
            db, device_type="esp32", payload=payload.esp32, user=user
        )
    if payload.esp32Cam is not None:
        apply_iot_wifi_device_update(
            db, device_type="esp32_cam", payload=payload.esp32Cam, user=user
        )


def apply_iot_wifi_device_update(
    db: Session,
    *,
    device_type: str,
    payload: IoTWifiDeviceConfigRequest,
    user: User,
) -> None:
    ssid = payload.ssid.strip()
    if len(ssid) > 64:
        raise HTTPException(
            status_code=400, detail="Wi-Fi SSID must be 64 characters or less"
        )

    existing = db.get(IoTWifiConfig, device_type)
    previous_password = existing.password if existing is not None else ""
    password = previous_password if payload.password is None else payload.password
    password = (password or "").strip()
    if len(password) > 128:
        raise HTTPException(
            status_code=400, detail="Wi-Fi password must be 128 characters or less"
        )

    revision = iot_wifi_revision(device_type, ssid, password)
    if existing is None:
        db.add(
            IoTWifiConfig(
                device_type=device_type,
                display_name=IOT_WIFI_DEVICE_LABELS[device_type],
                ssid=ssid,
                password=password,
                revision=revision,
                updated_by_user_id=user.id,
                updated_at=utcnow(),
            )
        )
        return

    if (
        existing.ssid == ssid
        and existing.password == password
        and existing.updated_by_user_id == user.id
    ):
        return
    existing.display_name = IOT_WIFI_DEVICE_LABELS[device_type]
    existing.ssid = ssid
    existing.password = password
    existing.revision = revision
    existing.updated_by_user_id = user.id
    existing.updated_at = utcnow()


def require_iot_wifi_config_access(request: Request, token: str | None = None) -> None:
    if not IOT_WIFI_CONFIG_TOKEN:
        return
    header_token = request.headers.get("x-pineguard-device-token", "").strip()
    if token != IOT_WIFI_CONFIG_TOKEN and header_token != IOT_WIFI_CONFIG_TOKEN:
        raise HTTPException(status_code=403, detail="Invalid IoT Wi-Fi config token")


def serialize_alert_rule(rule: AlertRule) -> dict[str, Any]:
    return {
        "id": rule.id,
        "scope": rule.scope,
        "tempHigh": rule.temp_high,
        "tempLow": rule.temp_low,
        "humidityLow": rule.humidity_low,
        "soilDry": rule.soil_dry,
        "soilCriticalDry": rule.soil_critical_dry,
        "soilWet": rule.soil_wet,
        "phLow": rule.ph_low,
        "phHigh": rule.ph_high,
        "cooldownMinutes": rule.cooldown_minutes,
        "updatedAt": datetime_to_unix_ms(rule.updated_at),
    }


def apply_alert_rule_update(rule: AlertRule, payload: AlertRuleRequest) -> None:
    rule.temp_high = payload.tempHigh
    rule.temp_low = payload.tempLow
    rule.humidity_low = payload.humidityLow
    rule.soil_dry = payload.soilDry
    rule.soil_critical_dry = payload.soilCriticalDry
    rule.soil_wet = payload.soilWet
    rule.ph_low = payload.phLow
    rule.ph_high = payload.phHigh
    rule.cooldown_minutes = max(0, payload.cooldownMinutes)
    rule.updated_at = utcnow()


def serialize_camera_frame(
    frame: CameraFrame, request: Request | None = None
) -> dict[str, Any]:
    return {
        "id": frame.id,
        "nodeId": frame.node_id,
        "imageUrl": public_or_stored_upload_url(
            frame.image_url, frame.image_path, request
        ),
        "imagePath": frame.image_path,
        "capturedAt": datetime_to_unix_ms(frame.captured_at),
        "uploadedAt": datetime_to_unix_ms(frame.uploaded_at),
    }


def serialize_record_review(review: RecordReview) -> dict[str, Any]:
    return {
        "id": review.id,
        "recordId": review.record_id,
        "reviewedByUserId": review.reviewed_by_user_id,
        "assignedToUserId": review.assigned_to_user_id,
        "reviewStatus": review.review_status,
        "reviewNote": review.review_note,
        "reviewedAt": datetime_to_unix_ms(review.reviewed_at),
    }


DATABASE_ENTITY_DEFINITIONS: list[dict[str, Any]] = [
    {
        "model": User,
        "name": "Users",
        "domain": "Identity",
        "description": "Admin, officer, and farmer accounts.",
        "latestField": "last_active_at",
        "apiPath": "/api/admin/users",
        "mobileSurface": "Profile session and settings.",
        "webSurface": "User Management table.",
        "interfaceStatus": "covered",
    },
    {
        "model": SessionToken,
        "name": "Session tokens",
        "domain": "Identity",
        "description": "Login sessions that keep mobile and web users authenticated.",
        "latestField": "created_at",
        "apiPath": "/api/auth/login",
        "mobileSurface": "Sign-in session storage.",
        "webSurface": "Admin sign-in session.",
        "interfaceStatus": "internal",
    },
    {
        "model": UserSettings,
        "name": "User settings",
        "domain": "Settings",
        "description": "Language, theme, notification, and sync preferences.",
        "latestField": "updated_at",
        "apiPath": "/api/settings/profile",
        "mobileSurface": "Settings screen.",
        "webSurface": "Settings page.",
        "interfaceStatus": "covered",
    },
    {
        "model": IoTWifiConfig,
        "name": "IoT Wi-Fi configs",
        "domain": "Hardware",
        "description": "Admin-managed SSID and password used by ESP32 and ESP32-CAM firmware.",
        "latestField": "updated_at",
        "apiPath": "/api/admin/settings/iot-wifi",
        "mobileSurface": "Hardware follows the active field Wi-Fi after firmware sync.",
        "webSurface": "Settings hardware Wi-Fi controls.",
        "interfaceStatus": "covered",
    },
    {
        "model": TranslationCache,
        "name": "Translation cache",
        "domain": "Localization",
        "description": "Saved Malay localized payloads for records, alerts, and jobs.",
        "latestField": "updated_at",
        "apiPath": "Automatic cache behind localized responses.",
        "mobileSurface": "Localized record and analysis text.",
        "webSurface": "Localized record and analysis text.",
        "interfaceStatus": "internal",
    },
    {
        "model": SensorNode,
        "name": "Sensor nodes",
        "domain": "Hardware",
        "description": "Registered ESP32 field nodes and their heartbeat state.",
        "latestField": "last_heartbeat",
        "apiPath": "/api/sensor-readings/latest",
        "mobileSurface": "Dashboard, zones, and diagnostics.",
        "webSurface": "System Monitor node status.",
        "interfaceStatus": "covered",
    },
    {
        "model": SensorReading,
        "name": "Sensor readings",
        "domain": "Hardware",
        "description": "Temperature, humidity, soil moisture, and pH samples.",
        "latestField": "created_at",
        "apiPath": "/api/sensor-readings/history",
        "mobileSurface": "Dashboard, zones, diagnostics, scan context.",
        "webSurface": "Dashboard and System Monitor.",
        "interfaceStatus": "covered",
    },
    {
        "model": CameraFrame,
        "name": "Camera frames",
        "domain": "Hardware",
        "description": "ESP32-CAM uploads and stored public image URLs.",
        "latestField": "captured_at",
        "apiPath": "/api/camera-frames/history",
        "mobileSurface": "Scan and diagnostics frame preview.",
        "webSurface": "System Monitor camera preview.",
        "interfaceStatus": "covered",
    },
    {
        "model": AlertRule,
        "name": "Alert rules",
        "domain": "Alerts",
        "description": "Threshold settings used to create sensor alerts.",
        "latestField": "updated_at",
        "apiPath": "/api/admin/settings/alert-rules",
        "mobileSurface": "Alert behavior through user settings.",
        "webSurface": "Settings threshold controls.",
        "interfaceStatus": "covered",
    },
    {
        "model": AlertRecord,
        "name": "Alerts",
        "domain": "Alerts",
        "description": "Threshold events raised from live sensor readings.",
        "latestField": "created_at",
        "apiPath": "/api/alerts",
        "mobileSurface": "Alerts screen and dashboard badge.",
        "webSurface": "Dashboard, analysis, and monitor cards.",
        "interfaceStatus": "covered",
    },
    {
        "model": ActivityRecord,
        "name": "Activity records",
        "domain": "Records",
        "description": "Submitted scans, sensor snapshots, and field activity.",
        "latestField": "record_timestamp",
        "apiPath": "/api/records",
        "mobileSurface": "Records page and record detail sheet.",
        "webSurface": "Submitted Records page and detail view.",
        "interfaceStatus": "covered",
    },
    {
        "model": TreatmentRecord,
        "name": "Treatment records",
        "domain": "Records",
        "description": "Treatment plans linked to submitted activity records.",
        "latestField": "created_at",
        "apiPath": "/api/records",
        "mobileSurface": "Record detail recommendations.",
        "webSurface": "Record detail treatment panel.",
        "interfaceStatus": "covered",
    },
    {
        "model": AnalysisJob,
        "name": "Analysis jobs",
        "domain": "AI",
        "description": "Queued and completed AI image analysis jobs.",
        "latestField": "updated_at",
        "apiPath": "/api/analysis-jobs",
        "mobileSurface": "Scan result and records sync.",
        "webSurface": "Data Analysis queue and result card.",
        "interfaceStatus": "covered",
    },
    {
        "model": RecordReview,
        "name": "Record reviews",
        "domain": "Reviews",
        "description": "Admin review notes and follow-up assignment state.",
        "latestField": "reviewed_at",
        "apiPath": "/api/admin/record-reviews",
        "mobileSurface": "Record detail status badges.",
        "webSurface": "Submitted Records review status.",
        "interfaceStatus": "covered",
    },
    {
        "model": Expert,
        "name": "Experts",
        "domain": "Consultation",
        "description": "Available agriculture experts for consultation workflows.",
        "latestField": None,
        "apiPath": "No public endpoint yet.",
        "mobileSurface": "Listed in Database Coverage until consultation UI is enabled.",
        "webSurface": "Listed in Entity Coverage until consultation UI is enabled.",
        "interfaceStatus": "pending",
    },
    {
        "model": ConsultationSession,
        "name": "Consultation sessions",
        "domain": "Consultation",
        "description": "Open or closed expert consultation threads.",
        "latestField": "started_at",
        "apiPath": "No public endpoint yet.",
        "mobileSurface": "Listed in Database Coverage until consultation UI is enabled.",
        "webSurface": "Listed in Entity Coverage until consultation UI is enabled.",
        "interfaceStatus": "pending",
    },
    {
        "model": ConsultationMessage,
        "name": "Consultation messages",
        "domain": "Consultation",
        "description": "Messages and images inside consultation threads.",
        "latestField": "sent_at",
        "apiPath": "No public endpoint yet.",
        "mobileSurface": "Listed in Database Coverage until consultation UI is enabled.",
        "webSurface": "Listed in Entity Coverage until consultation UI is enabled.",
        "interfaceStatus": "pending",
    },
]


def serialize_database_entity_coverage(
    db: Session, definition: dict[str, Any]
) -> dict[str, Any]:
    model = definition["model"]
    row_count = db.scalar(select(func.count()).select_from(model)) or 0
    latest_value = None
    latest_field = definition.get("latestField")
    if latest_field:
        latest_column = getattr(model, latest_field, None)
        if latest_column is not None:
            latest_value = db.scalar(select(func.max(latest_column)).select_from(model))

    table = model.__table__
    interface_status = str(definition.get("interfaceStatus") or "pending")
    if row_count == 0 and interface_status == "covered":
        data_state = "empty"
    elif row_count == 0 and interface_status == "pending":
        data_state = "pending"
    elif interface_status == "internal":
        data_state = "internal"
    else:
        data_state = "synced"

    return {
        "table": table.name,
        "name": definition["name"],
        "domain": definition["domain"],
        "description": definition["description"],
        "rowCount": int(row_count),
        "hasRecords": bool(row_count),
        "latestAt": database_entity_latest_ms(latest_value),
        "apiPath": definition["apiPath"],
        "mobileSurface": definition["mobileSurface"],
        "webSurface": definition["webSurface"],
        "interfaceStatus": interface_status,
        "dataState": data_state,
        "columns": [
            column.name
            for column in table.columns
            if column.name not in {"password_hash", "token_hash"}
        ],
    }


def database_entity_latest_ms(value: Any) -> int | None:
    if value is None:
        return None
    if isinstance(value, str):
        return datetime_to_unix_ms(parse_client_timestamp(value))
    return datetime_to_unix_ms(value)


def database_entity_coverage_payload(db: Session) -> dict[str, Any]:
    entities = [
        serialize_database_entity_coverage(db, definition)
        for definition in DATABASE_ENTITY_DEFINITIONS
    ]
    groups: dict[str, dict[str, Any]] = {}
    for entity in entities:
        domain = entity["domain"]
        group = groups.setdefault(
            domain,
            {
                "domain": domain,
                "tableCount": 0,
                "rowCount": 0,
                "emptyTables": 0,
                "pendingTables": 0,
                "internalTables": 0,
                "latestAt": None,
            },
        )
        group["tableCount"] += 1
        group["rowCount"] += entity["rowCount"]
        if not entity["hasRecords"]:
            group["emptyTables"] += 1
        if entity["interfaceStatus"] == "pending":
            group["pendingTables"] += 1
        if entity["interfaceStatus"] == "internal":
            group["internalTables"] += 1
        latest_at = entity.get("latestAt")
        if latest_at and (group["latestAt"] is None or latest_at > group["latestAt"]):
            group["latestAt"] = latest_at

    return {
        "generatedAt": datetime_to_unix_ms(utcnow()),
        "database": {
            "kind": database_kind(),
            "fallbackActive": database_fallback_active(),
            "fallbackReason": database_fallback_reason(),
        },
        "summary": {
            "tableCount": len(entities),
            "rowCount": sum(entity["rowCount"] for entity in entities),
            "emptyTables": len(
                [entity for entity in entities if not entity["hasRecords"]]
            ),
            "pendingTables": len(
                [
                    entity
                    for entity in entities
                    if entity["interfaceStatus"] == "pending"
                ]
            ),
            "coveredTables": len(
                [
                    entity
                    for entity in entities
                    if entity["interfaceStatus"] == "covered"
                ]
            ),
            "internalTables": len(
                [
                    entity
                    for entity in entities
                    if entity["interfaceStatus"] == "internal"
                ]
            ),
        },
        "groups": list(groups.values()),
        "entities": entities,
    }


def serialize_admin_user(user: User) -> dict[str, Any]:
    return {
        "id": user.id,
        "email": user.email,
        "displayName": user.display_name,
        "role": user.role,
        "location": user.farm_location,
        "accountStatus": user.account_status,
        "lastLoginAt": datetime_to_unix_ms(user.last_login),
        "lastActiveAt": datetime_to_unix_ms(user.last_active_at),
        "accessScope": (
            "Access control and system oversight"
            if user.role == "admin"
            else "Record review and farmer support"
            if user.role == "officer"
            else "Monitoring, alerts, and scan submission"
        ),
    }


def normalize_admin_managed_role(value: str | None) -> str:
    role = (value or "").strip().lower()
    if role == "staff":
        return "officer"
    if role in {"admin", "officer", "farmer"}:
        return role
    return "farmer"


def normalize_record_type_for_storage(value: str | None) -> str:
    record_type = (value or "").strip().lower().replace("-", "_").replace(" ", "_")
    if record_type not in RECORD_TYPE_ALIASES:
        raise HTTPException(status_code=400, detail="Invalid record type")
    return RECORD_TYPE_ALIASES[record_type]


def serialize_record_type(value: str | None) -> str:
    return "activity" if value == "manual" else (value or "activity")


def remove_record_dependencies(db: Session, record_id: str) -> None:
    db.query(TreatmentRecord).filter(TreatmentRecord.record_id == record_id).delete(
        synchronize_session=False
    )
    db.query(RecordReview).filter(RecordReview.record_id == record_id).delete(
        synchronize_session=False
    )
    db.query(ConsultationSession).filter(
        ConsultationSession.record_id == record_id
    ).update(
        {ConsultationSession.record_id: None},
        synchronize_session=False,
    )


def get_effective_alert_rule(db: Session) -> AlertRule:
    return ensure_default_alert_rule(db)


def determine_sensor_status_snapshot(
    temperature: float,
    humidity: float,
    soil_moisture: float,
    ph_level: float,
    rule: AlertRule | None = None,
) -> str:
    effective_rule = rule
    temp_high = effective_rule.temp_high if effective_rule else TEMP_HIGH_THRESHOLD
    temp_low = effective_rule.temp_low if effective_rule else TEMP_LOW_THRESHOLD
    humidity_low = (
        effective_rule.humidity_low if effective_rule else HUMIDITY_LOW_THRESHOLD
    )
    soil_critical_dry = (
        effective_rule.soil_critical_dry
        if effective_rule
        else SOIL_CRITICAL_DRY_THRESHOLD
    )
    soil_dry = effective_rule.soil_dry if effective_rule else SOIL_DRY_THRESHOLD
    soil_wet = effective_rule.soil_wet if effective_rule else SOIL_WET_THRESHOLD
    ph_low = effective_rule.ph_low if effective_rule else PH_LOW_THRESHOLD
    ph_high = effective_rule.ph_high if effective_rule else PH_HIGH_THRESHOLD

    if (
        temperature > temp_high
        or temperature < temp_low
        or soil_moisture < soil_critical_dry
        or ph_level < ph_low
        or ph_level > ph_high
    ):
        return "critical"

    if (
        humidity < humidity_low
        or soil_moisture < soil_dry
        or soil_moisture > soil_wet
        or temperature > 32.0
    ):
        return "warning"

    return "normal"


SENSOR_READING_STATUS_ALIASES = {
    "normal": "normal",
    "ok": "normal",
    "healthy": "normal",
    "online": "normal",
    "warning": "warning",
    "warn": "warning",
    "critical": "critical",
    "danger": "critical",
    "error": "critical",
}


NODE_STATUS_BY_READING_STATUS = {
    "normal": "online",
    "warning": "warning",
    "critical": "critical",
}


def normalize_sensor_reading_status(value: str | None) -> str | None:
    if value is None:
        return None
    normalized = value.strip().lower().replace("-", "_").replace(" ", "_")
    return SENSOR_READING_STATUS_ALIASES.get(normalized)


def build_threshold_alerts(
    reading: SensorReadingRequest,
    status_value: str,
    event_time,
    alert_rule: AlertRule | None = None,
) -> list[AlertRecord]:
    alerts: list[AlertRecord] = []
    effective_rule = alert_rule
    temp_high = effective_rule.temp_high if effective_rule else TEMP_HIGH_THRESHOLD
    temp_low = effective_rule.temp_low if effective_rule else TEMP_LOW_THRESHOLD
    humidity_low = (
        effective_rule.humidity_low if effective_rule else HUMIDITY_LOW_THRESHOLD
    )
    soil_critical_dry = (
        effective_rule.soil_critical_dry
        if effective_rule
        else SOIL_CRITICAL_DRY_THRESHOLD
    )
    soil_dry = effective_rule.soil_dry if effective_rule else SOIL_DRY_THRESHOLD
    soil_wet = effective_rule.soil_wet if effective_rule else SOIL_WET_THRESHOLD
    ph_low = effective_rule.ph_low if effective_rule else PH_LOW_THRESHOLD
    ph_high = effective_rule.ph_high if effective_rule else PH_HIGH_THRESHOLD

    def add_alert(
        severity: str,
        sensor_type: str,
        sensor_value: float,
        threshold_value: float,
        message: str,
    ) -> None:
        alerts.append(
            AlertRecord(
                node_id=reading.nodeId,
                user_id=None,
                alert_rule_id=effective_rule.id if effective_rule else None,
                severity=severity,
                sensor_type=sensor_type,
                sensor_value=sensor_value,
                threshold_value=threshold_value,
                message=message,
                created_at=event_time,
            )
        )

    if reading.temperature > temp_high:
        add_alert(
            "critical",
            "temperature",
            reading.temperature,
            temp_high,
            "Temperature exceeded the safe maximum.",
        )
    elif reading.temperature < temp_low:
        add_alert(
            "critical",
            "temperature",
            reading.temperature,
            temp_low,
            "Temperature dropped below the safe minimum.",
        )
    elif reading.temperature > 32.0 and status_value == "warning":
        add_alert(
            "warning",
            "temperature",
            reading.temperature,
            32.0,
            "Temperature is elevated for pineapple growth.",
        )

    if reading.humidity < humidity_low:
        add_alert(
            "warning",
            "humidity",
            reading.humidity,
            humidity_low,
            "Humidity is lower than the recommended level.",
        )

    if reading.soilMoisture < soil_critical_dry:
        add_alert(
            "critical",
            "soilMoisture",
            reading.soilMoisture,
            soil_critical_dry,
            "Soil moisture is critically low.",
        )
    elif reading.soilMoisture < soil_dry:
        add_alert(
            "warning",
            "soilMoisture",
            reading.soilMoisture,
            soil_dry,
            "Soil moisture dropped below the warning threshold.",
        )
    elif reading.soilMoisture > soil_wet:
        add_alert(
            "warning",
            "soilMoisture",
            reading.soilMoisture,
            soil_wet,
            "Soil moisture is unusually high.",
        )

    if reading.pH < ph_low:
        add_alert(
            "critical", "pH", reading.pH, ph_low, "Soil pH is too acidic for pineapple."
        )
    elif reading.pH > ph_high:
        add_alert(
            "critical",
            "pH",
            reading.pH,
            ph_high,
            "Soil pH is too alkaline for pineapple.",
        )

    return alerts


def save_activity_record(
    db: Session,
    user_id: str | None,
    record_type: str,
    title: str,
    description: str,
    record_timestamp,
    *,
    scan_result: dict[str, Any] | None = None,
    treatment: dict[str, Any] | None = None,
) -> ActivityRecord:
    record = ActivityRecord(
        user_id=user_id,
        record_type=normalize_record_type_for_storage(record_type),
        title=title,
        description=description,
        record_timestamp=record_timestamp,
        scan_result_json=json.dumps(scan_result) if scan_result is not None else None,
        treatment_json=json.dumps(treatment) if treatment is not None else None,
    )
    db.add(record)
    return record


def disease_display_name(label: Any) -> str:
    raw_value = str(label or "").strip().lower().replace("-", "_").replace(" ", "_")
    value = (
        raw_value
        if raw_value in DEFAULT_TREATMENTS
        else normalize_disease_label(str(label))
    )
    if value == "not_pineapple":
        return "No Pineapple Captured"
    return value.replace("_", " ").title()


def find_existing_scan_activity_record(
    db: Session,
    user_id: str | None,
    scan_result: dict[str, Any] | None,
) -> ActivityRecord | None:
    scan_key = scan_record_dedupe_key(scan_result)
    if not scan_key:
        return None

    statement: Select[tuple[ActivityRecord]] = (
        select(ActivityRecord)
        .where(ActivityRecord.record_type == "scan")
        .order_by(
            desc(ActivityRecord.record_timestamp), desc(ActivityRecord.created_at)
        )
        .limit(100)
    )
    if user_id is not None:
        statement = statement.where(
            or_(ActivityRecord.user_id == user_id, ActivityRecord.user_id.is_(None))
        )

    for record in db.scalars(statement).all():
        existing_scan_result = safe_json_loads(record.scan_result_json)
        if scan_record_dedupe_key(existing_scan_result) == scan_key:
            return record

    return None


def save_scan_activity_record(
    db: Session,
    user_id: str | None,
    title: str,
    description: str,
    record_timestamp,
    *,
    scan_result: dict[str, Any] | None = None,
    treatment: dict[str, Any] | None = None,
) -> ActivityRecord:
    existing = find_existing_scan_activity_record(db, user_id, scan_result)
    if existing is None:
        return save_activity_record(
            db,
            user_id,
            "scan",
            title,
            description,
            record_timestamp,
            scan_result=scan_result,
            treatment=treatment,
        )

    existing.title = title
    existing.description = description
    existing.record_timestamp = record_timestamp
    existing.scan_result_json = (
        json.dumps(scan_result) if scan_result is not None else None
    )
    existing.treatment_json = json.dumps(treatment) if treatment is not None else None
    return existing


def sensor_activity_title(node_id: str) -> str:
    return f"Sensor snapshot - {node_id}"


def sensor_activity_description(
    payload: SensorReadingRequest, reading_status: str
) -> str:
    return (
        f"{payload.deviceName} at {payload.location}: "
        f"temperature {payload.temperature:.1f} deg C, "
        f"humidity {payload.humidity:.0f}%, "
        f"soil moisture {payload.soilMoisture:.0f}%, "
        f"pH {payload.pH:.1f}. "
        f"Sensor status: {reading_status}."
    )


def save_sensor_activity_record(
    db: Session,
    payload: SensorReadingRequest,
    reading_status: str,
    recorded_at,
) -> ActivityRecord | None:
    title = sensor_activity_title(payload.nodeId)
    existing = db.scalar(
        select(ActivityRecord)
        .where(ActivityRecord.record_type == "manual")
        .where(ActivityRecord.title == title)
        .where(ActivityRecord.record_timestamp == recorded_at)
        .limit(1)
    )
    if existing is not None:
        return None

    return save_activity_record(
        db,
        None,
        "manual",
        title,
        sensor_activity_description(payload, reading_status),
        recorded_at,
    )


def persist_treatment_record(
    db: Session,
    record: ActivityRecord,
    *,
    record_title: str,
    treatment: dict[str, Any] | None = None,
) -> TreatmentRecord | None:
    treatment_steps = treatment.get("steps") if isinstance(treatment, dict) else None
    if not isinstance(treatment_steps, list):
        return None

    cleaned_steps = [str(step).strip() for step in treatment_steps if str(step).strip()]
    if not cleaned_steps:
        return None

    db.flush()
    existing = db.scalar(
        select(TreatmentRecord)
        .where(TreatmentRecord.record_id == record.id)
        .order_by(desc(TreatmentRecord.created_at))
        .limit(1)
    )
    description = "\n".join(cleaned_steps)
    title = f"Treatment plan for {record_title}"

    if existing is not None:
        existing.title = title
        existing.description = description
        return existing

    treatment_record = TreatmentRecord(
        record_id=record.id,
        title=title,
        description=description,
    )
    db.add(treatment_record)
    return treatment_record


def normalize_disease_label(raw_label: str | None) -> str:
    if not raw_label:
        return "water_stress"

    value = raw_label.strip().lower().replace("-", " ").replace("_", " ")
    for normalized, keywords in LABEL_KEYWORDS:
        if any(keyword in value for keyword in keywords):
            return normalized
    return "water_stress"


def is_not_pineapple_payload(raw_result: dict[str, Any]) -> bool:
    for key in ("containsPineapple", "pineappleDetected", "pineappleVisible"):
        value = raw_result.get(key)
        if value is False:
            return True
        if isinstance(value, str) and value.strip().lower() in {
            "false",
            "no",
            "none",
            "0",
        }:
            return True

    text_parts = [
        raw_result.get("disease"),
        raw_result.get("label"),
        raw_result.get("description"),
        raw_result.get("sceneDescription"),
        raw_result.get("visibleObjects"),
        raw_result.get("objects"),
    ]
    combined = " ".join(str(part).lower() for part in text_parts if part is not None)
    combined = re.sub(r"[\s_-]+", " ", combined)
    return any(phrase in combined for phrase in NOT_PINEAPPLE_PHRASES)


def normalize_confidence(value: Any) -> float:
    try:
        confidence = float(value)
    except (TypeError, ValueError):
        return 0.5

    if confidence > 1:
        confidence = confidence / 100
    return max(0.0, min(1.0, confidence))


def extract_json_object(text: str) -> dict[str, Any]:
    stripped = text.strip()
    if not stripped:
        raise ValueError("Model returned an empty response")

    try:
        parsed = json.loads(stripped)
        if isinstance(parsed, dict):
            return parsed
    except json.JSONDecodeError:
        pass

    match = re.search(r"\{.*\}", stripped, re.DOTALL)
    if not match:
        raise ValueError("No JSON object found in model response")

    return json.loads(match.group(0))


def extract_ollama_message_content(payload: dict[str, Any]) -> str:
    message = payload.get("message")
    if isinstance(message, dict):
        content = message.get("content")
        if isinstance(content, str) and content.strip():
            return content

    response = payload.get("response")
    if isinstance(response, str) and response.strip():
        return response

    raise ValueError("Model returned an empty response")


def fetch_image_from_url(image_url: str) -> bytes:
    parsed = urlparse(image_url)
    if parsed.scheme not in {"http", "https"}:
        raise HTTPException(
            status_code=400, detail="Only http and https image URLs are supported"
        )

    response = requests.get(image_url, timeout=REQUEST_TIMEOUT_SECONDS)
    response.raise_for_status()
    content = response.content
    validate_image_bytes(content)
    return content


def validate_image_bytes(image_bytes: bytes) -> None:
    if not image_bytes:
        raise HTTPException(status_code=400, detail="Image is empty")
    if len(image_bytes) > MAX_IMAGE_BYTES:
        raise HTTPException(
            status_code=413, detail="Image is too large for local analysis"
        )


def normalize_visible_objects(value: Any) -> list[str]:
    if value is None:
        return []
    if isinstance(value, str):
        return [item.strip() for item in re.split(r"[,;\n]", value) if item.strip()]
    if isinstance(value, list):
        objects: list[str] = []
        for item in value:
            if isinstance(item, dict):
                label = item.get("label") or item.get("name") or item.get("object")
                if label:
                    objects.append(str(label).strip())
            elif item is not None:
                objects.append(str(item).strip())
        return [item for item in objects if item]
    return [str(value).strip()] if str(value).strip() else []


def normalize_scene_description(
    raw_result: dict[str, Any], visible_objects: list[str]
) -> str:
    raw_scene = raw_result.get("sceneDescription") or raw_result.get("scene") or ""
    if isinstance(raw_scene, list):
        raw_scene = ", ".join(str(item) for item in raw_scene if str(item).strip())
    scene_description = str(raw_scene).strip()
    if scene_description:
        return scene_description
    return ", ".join(visible_objects)


def normalize_contains_pineapple(raw_result: dict[str, Any], disease: str) -> bool:
    if disease == "not_pineapple":
        return False
    for key in ("containsPineapple", "pineappleDetected", "pineappleVisible"):
        value = raw_result.get(key)
        if isinstance(value, bool):
            return value
        if isinstance(value, str):
            lowered = value.strip().lower()
            if lowered in {"true", "yes", "1", "visible"}:
                return True
            if lowered in {"false", "no", "none", "0", "not visible"}:
                return False
    return disease != "not_pineapple"


def normalize_analysis_result(raw_result: dict[str, Any]) -> dict[str, Any]:
    disease = (
        "not_pineapple"
        if is_not_pineapple_payload(raw_result)
        else normalize_disease_label(raw_result.get("disease"))
    )
    confidence = normalize_confidence(raw_result.get("confidence"))
    description = str(raw_result.get("description") or "").strip()
    visible_objects = normalize_visible_objects(
        raw_result.get("visibleObjects") or raw_result.get("objects")
    )
    scene_description = normalize_scene_description(raw_result, visible_objects)
    contains_pineapple = normalize_contains_pineapple(raw_result, disease)
    treatment = raw_result.get("treatment")

    if disease == "not_pineapple":
        treatment = DEFAULT_TREATMENTS[disease]
    elif not isinstance(treatment, list) or not treatment:
        treatment = DEFAULT_TREATMENTS[disease]
    else:
        treatment = [str(item).strip() for item in treatment if str(item).strip()]
        if not treatment:
            treatment = DEFAULT_TREATMENTS[disease]

    if not description:
        if disease == "not_pineapple":
            visible_detail = (
                f" Visible content: {scene_description}."
                if scene_description
                else " The visible content does not provide enough pineapple plant detail for diagnosis."
            )
            description = (
                f"No pineapple object was captured in this image.{visible_detail}"
            )
        elif disease == "healthy":
            description = "The plant appears visually healthy in this image."
        else:
            description = f"The image suggests signs most consistent with {disease.replace('_', ' ')}."
    elif disease == "not_pineapple" and "pineapple" not in description.lower():
        description = f"No pineapple object was captured in this image. {description}"

    return {
        "disease": disease,
        "confidence": confidence,
        "containsPineapple": contains_pineapple,
        "sceneDescription": scene_description,
        "visibleObjects": visible_objects,
        "description": description,
        "treatment": treatment[:5],
    }


def normalize_image_analysis_text(value: Any) -> Any:
    if not isinstance(value, str):
        return value
    return (
        value.replace(
            "No live sensor reading was provided for this frame.",
            "No live sensor reading was provided for this image.",
        )
        .replace(
            "Sensors look stable for this frame.", "Sensors look stable for this image."
        )
        .replace(
            "Plant appears healthy in the recent live view.",
            "Plant appears healthy in the analyzed image.",
        )
        .replace("recent live view", "analyzed image")
        .replace("live monitoring", "image analysis")
        .replace("this frame", "this image")
    )


def normalize_image_analysis_language(payload: Any) -> Any:
    if isinstance(payload, dict):
        return {
            key: normalize_image_analysis_language(value)
            for key, value in payload.items()
        }
    if isinstance(payload, list):
        return [normalize_image_analysis_language(item) for item in payload]
    return normalize_image_analysis_text(payload)


def call_ollama(image_bytes: bytes) -> dict[str, Any]:
    encoded_image = base64.b64encode(image_bytes).decode("utf-8")
    payload = {
        "model": OLLAMA_MODEL,
        "messages": [
            {
                "role": "system",
                "content": SYSTEM_PROMPT,
            },
            {
                "role": "user",
                "content": USER_PROMPT,
                "images": [encoded_image],
            },
        ],
        "stream": False,
        "format": "json",
    }

    response = requests.post(
        f"{OLLAMA_BASE_URL}/api/chat",
        json=payload,
        timeout=REQUEST_TIMEOUT_SECONDS,
    )
    response.raise_for_status()

    outer = response.json()
    inner_text = extract_ollama_message_content(outer)
    parsed = extract_json_object(inner_text)
    return normalize_analysis_result(parsed)


def normalize_remote_ai_layer_url(value: str | None) -> str:
    normalized = str(value or "").strip().rstrip("/")
    if not normalized:
        return ""
    parsed = urlparse(normalized)
    if parsed.scheme not in {"http", "https"} or not parsed.netloc:
        return ""
    return normalized


def parse_remote_ai_layer_urls(value: str) -> list[str]:
    urls: list[str] = []
    for candidate in re.split(r"[,;\s]+", value or ""):
        normalized = normalize_remote_ai_layer_url(candidate)
        if normalized and normalized not in urls:
            urls.append(normalized)
    return urls


def blocked_remote_ai_layer_urls() -> set[str]:
    local_urls = {
        PUBLIC_BASE_URL,
        f"http://0.0.0.0:{BACKEND_PORT}",
        f"http://127.0.0.1:{BACKEND_PORT}",
        f"http://localhost:{BACKEND_PORT}",
    }
    return {
        normalized
        for normalized in (normalize_remote_ai_layer_url(value) for value in local_urls)
        if normalized
    }


def configured_remote_ai_layer_urls() -> list[str]:
    blocked_urls = blocked_remote_ai_layer_urls()
    urls: list[str] = []
    for candidate in [
        AI_LAYER_BASE_URL,
        *parse_remote_ai_layer_urls(AI_LAYER_FALLBACK_URLS),
    ]:
        normalized = normalize_remote_ai_layer_url(candidate)
        if not normalized or normalized in blocked_urls or normalized in urls:
            continue
        urls.append(normalized)
    return urls


def remote_ai_layer_enabled() -> bool:
    return bool(configured_remote_ai_layer_urls())


def remote_ai_layer_role(index: int) -> str:
    return "primary" if index == 0 else "fallback"


def call_remote_ai_layer(
    image_bytes: bytes,
    *,
    sensor_reading: dict[str, float] | None,
    session_id: str,
    live_mode: bool,
) -> tuple[dict[str, Any], dict[str, Any], dict[str, Any]]:
    ai_layer_urls = configured_remote_ai_layer_urls()
    if not ai_layer_urls:
        raise ValueError("Remote AI layer is not configured")

    relay_failures: list[str] = []
    for index, base_url in enumerate(ai_layer_urls):
        relay_role = remote_ai_layer_role(index)
        try:
            result, live_assessment, meta = call_remote_ai_layer_url(
                base_url,
                image_bytes,
                sensor_reading=sensor_reading,
                session_id=session_id,
                live_mode=live_mode,
            )
            meta["relayRole"] = relay_role
            meta["relayIndex"] = index
            meta["remoteAiLayerAttempts"] = index + 1
            if relay_failures:
                meta["relayFallbackReasons"] = relay_failures
            return result, live_assessment, meta
        except (requests.RequestException, ValueError) as error:
            relay_failures.append(f"{relay_role}: {error}")

    raise ValueError(
        "All configured remote AI layers failed: " + " | ".join(relay_failures)
    )


def call_remote_ai_layer_url(
    base_url: str,
    image_bytes: bytes,
    *,
    sensor_reading: dict[str, float] | None,
    session_id: str,
    live_mode: bool,
) -> tuple[dict[str, Any], dict[str, Any], dict[str, Any]]:
    data = {
        "session_id": session_id,
        "live_mode": "true" if live_mode else "false",
    }
    if sensor_reading is not None:
        data["sensor_reading"] = json.dumps(sensor_reading)

    response = requests.post(
        f"{base_url}/analyze",
        files={"image": ("analysis.jpg", image_bytes, "image/jpeg")},
        data=data,
        timeout=AI_LAYER_TIMEOUT_SECONDS,
    )
    response.raise_for_status()

    payload = response.json()
    scan_result_payload = payload.get("scanResult")
    if not isinstance(scan_result_payload, dict):
        raise ValueError("Remote AI layer returned an invalid scanResult payload")

    result = normalize_image_analysis_language(
        normalize_analysis_result(scan_result_payload)
    )
    live_assessment_payload = payload.get("liveAssessment")
    live_assessment = (
        normalize_image_analysis_language(live_assessment_payload)
        if isinstance(live_assessment_payload, dict)
        else None
    )
    if live_assessment is None:
        session_summary = update_live_session(
            session_id, result["disease"], result["confidence"]
        )
        live_assessment = build_live_assessment(result, sensor_reading, session_summary)

    meta_payload = payload.get("meta")
    meta = meta_payload.copy() if isinstance(meta_payload, dict) else {}
    meta = normalize_image_analysis_language(meta)
    upstream_source = meta.get("source")
    if upstream_source:
        meta["upstreamSource"] = upstream_source
    meta["source"] = "remote_ai_layer"
    meta["relayBaseUrl"] = base_url
    meta["model"] = meta.get("model") or OLLAMA_MODEL
    return result, live_assessment, meta


def analyze_image_bytes(
    image_bytes: bytes,
    *,
    sensor_reading: dict[str, float] | None,
    session_id: str,
    live_mode: bool,
) -> tuple[dict[str, Any], dict[str, Any], dict[str, Any]]:
    if remote_ai_layer_enabled():
        result, live_assessment, meta = call_remote_ai_layer(
            image_bytes,
            sensor_reading=sensor_reading,
            session_id=session_id,
            live_mode=live_mode,
        )
        meta["liveMode"] = live_mode
        meta["sessionId"] = session_id or None
        meta.setdefault("reviewPath", None)
        return result, live_assessment, meta

    result = normalize_image_analysis_language(
        normalize_analysis_result(call_ollama(image_bytes))
    )
    session_summary = update_live_session(
        session_id, result["disease"], result["confidence"]
    )
    live_assessment = build_live_assessment(result, sensor_reading, session_summary)
    review_path = save_review_frame(
        image_bytes, result["disease"], result["confidence"]
    )
    return (
        result,
        live_assessment,
        {
            "source": "ollama",
            "model": OLLAMA_MODEL,
            "liveMode": live_mode,
            "sessionId": session_id or None,
            "reviewPath": review_path,
        },
    )


def build_backend_fallback_result(
    sensor_reading: dict[str, float] | None,
    *,
    reason: str,
) -> tuple[dict[str, Any], dict[str, Any], dict[str, Any]]:
    sensor_analysis = analyze_sensor_risk(sensor_reading)
    supports = sensor_analysis.get("supports") or []
    disease = supports[0] if supports else "analysis_unavailable"
    confidence = 0.38 if supports else 0.2
    description = (
        "Backend fallback guidance was used because the configured AI analysis layer "
        f"was unavailable. {sensor_analysis['summary']}"
    )
    result = {
        "disease": disease,
        "confidence": confidence,
        "description": description,
        "treatment": DEFAULT_TREATMENTS.get(
            disease, DEFAULT_TREATMENTS["analysis_unavailable"]
        ),
    }
    session_summary = {
        "stableLabel": disease,
        "stableConfidence": confidence,
        "historySize": 1,
    }
    live_assessment = build_live_assessment(result, sensor_reading, session_summary)
    meta = {
        "source": "backend_fallback",
        "model": OLLAMA_MODEL,
        "fallbackReason": reason,
        "sensorSummary": sensor_analysis["summary"],
    }
    return result, live_assessment, meta


def analyze_image_bytes_optional(
    image_bytes: bytes,
    *,
    sensor_reading: dict[str, float] | None,
    session_id: str,
    live_mode: bool,
) -> tuple[dict[str, Any], dict[str, Any], dict[str, Any]]:
    try:
        return analyze_image_bytes(
            image_bytes,
            sensor_reading=sensor_reading,
            session_id=session_id,
            live_mode=live_mode,
        )
    except (requests.RequestException, ValueError) as error:
        return build_backend_fallback_result(sensor_reading, reason=str(error))


def build_scan_result_payload(
    image_uri: str | None, result: dict[str, Any]
) -> dict[str, Any]:
    return {
        "imageUri": image_uri,
        "disease": result["disease"],
        "confidence": result["confidence"],
        "containsPineapple": bool(
            result.get("containsPineapple", result["disease"] != "not_pineapple")
        ),
        "sceneDescription": str(result.get("sceneDescription") or ""),
        "visibleObjects": list(result.get("visibleObjects") or []),
        "description": result["description"],
        "treatment": result["treatment"],
        "timestamp": int(time.time() * 1000),
    }


def ollama_tags() -> dict[str, Any]:
    response = requests.get(f"{OLLAMA_BASE_URL}/api/tags", timeout=10)
    response.raise_for_status()
    return response.json()


def parse_json_value(raw_value: Any) -> dict[str, Any] | None:
    if raw_value is None:
        return None
    if isinstance(raw_value, dict):
        return raw_value
    if isinstance(raw_value, str) and raw_value.strip():
        try:
            parsed = json.loads(raw_value)
            if isinstance(parsed, dict):
                return parsed
        except json.JSONDecodeError:
            return None
    return None


def normalize_sensor_reading(raw: dict[str, Any] | None) -> dict[str, float] | None:
    if not raw:
        return None

    return {
        "temperature": float(raw.get("temperature", 0) or 0),
        "humidity": float(raw.get("humidity", 0) or 0),
        "soilMoisture": float(raw.get("soilMoisture", 0) or 0),
        "pH": float(raw.get("pH", 0) or 0),
    }


def analyze_sensor_risk(sensor_reading: dict[str, float] | None) -> dict[str, Any]:
    if not sensor_reading:
        return {
            "healthScore": None,
            "issues": [],
            "supports": [],
            "summary": "No live sensor reading was provided for this image.",
        }

    issues: list[str] = []
    supports: list[str] = []
    score = 100

    temperature = sensor_reading["temperature"]
    humidity = sensor_reading["humidity"]
    soil_moisture = sensor_reading["soilMoisture"]
    soil_ph = sensor_reading["pH"]

    if soil_moisture < 30:
        issues.append("Soil moisture is low, which increases water stress risk.")
        supports.append("water_stress")
        score -= 20
    elif soil_moisture > 80:
        issues.append("Soil moisture is very high, which increases rot risk.")
        supports.extend(["heart_rot", "fruit_rot", "water_stress"])
        score -= 20

    if humidity < 40:
        issues.append("Humidity is low, which can worsen dehydration stress.")
        supports.append("water_stress")
        score -= 10
    elif humidity > 85:
        issues.append("Humidity is high, which favors fungal and rot conditions.")
        supports.extend(["heart_rot", "fruit_rot"])
        score -= 12

    if temperature < 18:
        issues.append("Temperature is cooler than ideal for stable growth.")
        score -= 8
    elif temperature > 32:
        issues.append("Temperature is high, which can intensify plant stress.")
        supports.append("water_stress")
        score -= 12

    if soil_ph and (soil_ph < 4.5 or soil_ph > 6.5):
        issues.append("Soil pH is outside the optimal pineapple range of 4.5 to 6.5.")
        supports.append("nutrient_deficiency")
        score -= 18

    summary = "Sensors look stable for this image."
    if issues:
        summary = " ".join(issues)

    return {
        "healthScore": max(0, min(100, score)),
        "issues": issues,
        "supports": supports,
        "summary": summary,
    }


def update_live_session(
    session_id: str | None, disease: str, confidence: float
) -> dict[str, Any]:
    if not session_id:
        return {
            "stableLabel": disease,
            "stableConfidence": confidence,
            "historySize": 1,
        }

    session = LIVE_SESSIONS[session_id]
    session.append(
        {
            "disease": disease,
            "confidence": confidence,
            "timestamp": int(time.time() * 1000),
        }
    )

    weighted_scores: dict[str, float] = defaultdict(float)
    for item in session:
        weighted_scores[item["disease"]] += item["confidence"]

    stable_label = max(weighted_scores.items(), key=lambda item: item[1])[0]
    stable_confidence = round(weighted_scores[stable_label] / max(1, len(session)), 2)

    return {
        "stableLabel": stable_label,
        "stableConfidence": stable_confidence,
        "historySize": len(session),
    }


def build_live_assessment(
    vision_result: dict[str, Any],
    sensor_reading: dict[str, float] | None,
    session_summary: dict[str, Any],
) -> dict[str, Any]:
    sensor_analysis = analyze_sensor_risk(sensor_reading)
    stable_label = session_summary["stableLabel"]
    stable_confidence = session_summary["stableConfidence"]
    vision_label = vision_result["disease"]

    primary_label = stable_label if stable_confidence >= 0.7 else vision_label
    reasons = [
        f"Vision model suggests {vision_label.replace('_', ' ')} at {vision_result['confidence']:.2f} confidence."
    ]
    reasons.extend(sensor_analysis["issues"])

    urgency = "low"
    if primary_label == "not_pineapple":
        urgency = "low"
    elif primary_label == "analysis_unavailable":
        urgency = "low"
    elif primary_label in {"heart_rot", "mealybug_wilt"} or (
        sensor_analysis["healthScore"] is not None
        and sensor_analysis["healthScore"] < 60
    ):
        urgency = "high"
    elif primary_label in {"fruit_rot", "water_stress", "nutrient_deficiency"}:
        urgency = "medium"

    if primary_label == "not_pineapple":
        plant_status = "No pineapple object was captured in the analyzed image."
        reasons = [
            "The AI did not find a pineapple plant, leaf, crown, or fruit to diagnose.",
            vision_result["description"],
        ]
        reasons.extend(sensor_analysis["issues"])
    elif primary_label == "analysis_unavailable":
        plant_status = (
            "AI image analysis is unavailable, so this scan is not marked healthy."
        )
        reasons = [
            "No vision model result was available for this image.",
            vision_result["description"],
        ]
        reasons.extend(sensor_analysis["issues"])
    elif primary_label == "healthy" and sensor_analysis["issues"]:
        plant_status = "Visually healthy, but sensors show stress risk."
    elif primary_label == "healthy":
        plant_status = "Plant appears healthy in the analyzed image."
    else:
        plant_status = (
            f"Possible {primary_label.replace('_', ' ')} detected in image analysis."
        )

    recommendation = DEFAULT_TREATMENTS.get(
        primary_label, DEFAULT_TREATMENTS["water_stress"]
    )[0]

    return {
        "plantStatus": plant_status,
        "urgency": urgency,
        "stableLabel": stable_label,
        "stableConfidence": stable_confidence,
        "historySize": session_summary["historySize"],
        "sensorHealthScore": sensor_analysis["healthScore"],
        "sensorSummary": sensor_analysis["summary"],
        "reasons": reasons[:5],
        "recommendation": recommendation,
    }


def save_review_frame(
    image_bytes: bytes, disease: str, confidence: float
) -> str | None:
    if confidence >= LOW_CONFIDENCE_THRESHOLD:
        return None

    target_dir = REVIEW_DIR / disease
    target_dir.mkdir(parents=True, exist_ok=True)
    file_path = target_dir / f"{int(time.time())}_{uuid4().hex[:8]}.jpg"
    file_path.write_bytes(image_bytes)
    return str(file_path)


@app.get("/")
def root() -> dict[str, str]:
    return {"message": "PineGuard backend is running"}


@app.get("/livez")
def livez() -> dict[str, str]:
    return {"status": "ok", "backend": "running"}


@app.get("/health")
@app.post("/health")
def health(request: Request) -> dict[str, Any]:
    database_payload = {
        "kind": database_kind(),
        "fallbackActive": database_fallback_active(),
        "fallbackReason": database_fallback_reason(),
    }
    base_status = "degraded" if database_fallback_active() else "ok"
    remote_ai_layer_urls = configured_remote_ai_layer_urls()
    try:
        if remote_ai_layer_urls:
            relay_failures = 0
            for index, base_url in enumerate(remote_ai_layer_urls):
                try:
                    response = requests.get(
                        f"{base_url}/health",
                        timeout=min(AI_LAYER_TIMEOUT_SECONDS, 5),
                    )
                    response.raise_for_status()
                    payload = response.json()
                except (requests.RequestException, ValueError):
                    relay_failures += 1
                    continue

                payload_status = str(payload.get("status", "ok"))
                return {
                    "status": "degraded"
                    if base_status == "degraded"
                    or payload_status == "degraded"
                    or index > 0
                    or relay_failures > 0
                    else payload_status,
                    "backend": "running",
                    **backend_identity_payload(request),
                    "database": database_kind(),
                    "databaseDetails": database_payload,
                    "analysisLayer": "remote_ai_backend",
                    "ollama": payload.get("ollama", "reachable"),
                    "model": payload.get("model", OLLAMA_MODEL),
                    "modelAvailable": payload.get("modelAvailable", True),
                    "availableModels": payload.get("availableModels", []),
                    "remoteAiLayerConfigured": True,
                    "remoteAiLayerCount": len(remote_ai_layer_urls),
                    "activeRemoteAiLayer": remote_ai_layer_role(index),
                    "remoteAiLayerFailures": relay_failures,
                }

            return {
                "status": "degraded",
                "backend": "running",
                **backend_identity_payload(request),
                "database": database_kind(),
                "databaseDetails": database_payload,
                "analysisLayer": "remote_ai_backend",
                "ollama": "unreachable",
                "model": OLLAMA_MODEL,
                "modelAvailable": False,
                "availableModels": [],
                "remoteAiLayerConfigured": True,
                "remoteAiLayerCount": len(remote_ai_layer_urls),
                "activeRemoteAiLayer": None,
                "remoteAiLayerFailures": relay_failures,
                "error": "All configured remote AI layers are unreachable",
            }

        tags = ollama_tags()
        available_models = [item.get("name", "") for item in tags.get("models", [])]
        return {
            "status": base_status,
            "backend": "running",
            **backend_identity_payload(request),
            "database": database_kind(),
            "databaseDetails": database_payload,
            "analysisLayer": "local_ollama",
            "ollama": "reachable",
            "model": OLLAMA_MODEL,
            "modelAvailable": OLLAMA_MODEL in available_models,
            "availableModels": available_models,
        }
    except requests.RequestException as exc:
        return {
            "status": "degraded",
            "backend": "running",
            **backend_identity_payload(request),
            "database": database_kind(),
            "databaseDetails": database_payload,
            "analysisLayer": "remote_ai_backend"
            if remote_ai_layer_urls
            else "local_ollama",
            "ollama": "unreachable",
            "model": OLLAMA_MODEL,
            "error": str(exc),
        }


@app.post("/analyze")
async def analyze(
    request: Request,
    lang: str | None = None,
    db: Session = Depends(get_db),
) -> dict[str, Any]:
    image_bytes: bytes | None = None
    image_uri = ""
    sensor_reading: dict[str, float] | None = None
    session_id = ""
    live_mode = False
    content_type = request.headers.get("content-type", "")

    if content_type.startswith("application/json"):
        payload = await request.json()
        image_url = str(payload.get("image_url") or "").strip()
        sensor_reading = normalize_sensor_reading(
            parse_json_value(payload.get("sensor_reading"))
        )
        session_id = str(payload.get("session_id") or "").strip()
        live_mode = bool(payload.get("live_mode"))
        if not image_url:
            raise HTTPException(
                status_code=400, detail="Missing image_url in JSON request"
            )
        image_bytes = fetch_image_from_url(image_url)
        image_uri = image_url
    else:
        form = await request.form()
        upload = form.get("image")
        image_url = str(form.get("image_url") or "").strip()
        sensor_reading = normalize_sensor_reading(
            parse_json_value(form.get("sensor_reading"))
        )
        session_id = str(form.get("session_id") or "").strip()
        live_mode = str(form.get("live_mode") or "").strip().lower() in {
            "1",
            "true",
            "yes",
            "on",
        }

        if upload is not None and hasattr(upload, "read"):
            image_bytes = await upload.read()
            validate_image_bytes(image_bytes)
            image_uri = getattr(upload, "filename", "uploaded-image")
        elif image_url:
            image_bytes = fetch_image_from_url(image_url)
            image_uri = image_url
        else:
            raise HTTPException(
                status_code=400, detail="Provide either an image upload or image_url"
            )

    result, live_assessment, meta = analyze_image_bytes_optional(
        image_bytes,
        sensor_reading=sensor_reading,
        session_id=session_id,
        live_mode=live_mode,
    )

    meta["liveMode"] = live_mode
    meta["sessionId"] = session_id or None

    scan_result = build_scan_result_payload(image_uri, result)
    response_payload = {
        "status": "ok",
        "scanResult": scan_result,
        "meta": meta,
        "liveAssessment": live_assessment,
    }
    locale = request_language(request, lang)
    localized = cached_localized_payload(
        db,
        source_type="direct_analysis",
        source_id=source_payload_hash(
            {
                "imageUri": image_uri,
                "scanResult": scan_result,
                "liveAssessment": live_assessment,
            }
        ),
        locale=locale,
        source_payload={"scanResult": scan_result, "liveAssessment": live_assessment},
    )
    if localized:
        response_payload["localized"] = localized
        db.commit()
    return response_payload


def load_job_image_bytes(job: AnalysisJob) -> bytes:
    if job.image_path:
        file_path = UPLOADS_DIR / job.image_path
        if not file_path.exists():
            raise FileNotFoundError(f"Queued job image is missing: {file_path}")
        image_bytes = file_path.read_bytes()
        validate_image_bytes(image_bytes)
        return image_bytes

    if job.image_uri:
        return fetch_image_from_url(job.image_uri)

    raise ValueError("Analysis job does not contain an image reference")


def process_analysis_job(job_id: str) -> None:
    with db_session() as db:
        job = db.get(AnalysisJob, job_id)
        if not job:
            return

        job.status = "processing"
        job.updated_at = utcnow()
        db.commit()

        try:
            image_bytes = load_job_image_bytes(job)
            sensor_reading = normalize_sensor_reading(safe_json_loads(job.sensor_json))
            result, live_assessment, meta = analyze_image_bytes_optional(
                image_bytes,
                sensor_reading=sensor_reading,
                session_id=job.session_id or "",
                live_mode=job.live_mode,
            )

            image_uri = job.image_uri or public_upload_url(job.image_path)
            scan_result = build_scan_result_payload(image_uri, result)
            meta["liveMode"] = job.live_mode
            meta["sessionId"] = job.session_id
            meta.setdefault("reviewPath", None)
            meta["transport"] = job.transport

            job.status = "done"
            job.scan_result_json = json.dumps(scan_result)
            job.live_assessment_json = json.dumps(live_assessment)
            job.meta_json = json.dumps(meta)
            job.error = None
            job.updated_at = utcnow()
            job.completed_at = utcnow()

            treatment_payload = {
                "steps": result["treatment"],
            }
            record_title = f"AI Scan - {disease_display_name(result['disease'])}"
            record = save_scan_activity_record(
                db,
                job.user_id,
                record_title,
                f"Confidence: {(result['confidence'] * 100):.1f}%. {result['description']}",
                parse_client_timestamp(scan_result["timestamp"]),
                scan_result=scan_result | {"userId": job.user_id},
                treatment=treatment_payload,
            )
            persist_treatment_record(
                db,
                record,
                record_title=record_title,
                treatment=treatment_payload,
            )
            serialize_job(job, db=db, locale="ms")
            serialize_record(record, db=db, locale="ms")
            db.commit()
        except Exception as error:  # noqa: BLE001
            job = db.get(AnalysisJob, job_id)
            if not job:
                return
            job.status = "failed"
            job.error = str(error)
            job.updated_at = utcnow()
            job.completed_at = utcnow()
            db.commit()


@app.post("/api/auth/login")
def login(
    payload: LoginRequest, request: Request, db: Session = Depends(get_db)
) -> dict[str, Any]:
    email = payload.email.strip().lower()
    password = payload.password
    if not email or not password:
        raise HTTPException(status_code=400, detail="Email and password are required")

    user = db.scalar(select(User).where(User.email == email))
    if user is None:
        raise HTTPException(status_code=401, detail="Account not found")
    elif not verify_password(password, user.password_hash):
        raise HTTPException(status_code=401, detail="Incorrect password")
    elif user.account_status != "active":
        raise HTTPException(status_code=403, detail="This account is not active")

    for attempt in range(3):
        token = create_session_token()
        try:
            ensure_user_settings(db, user.id)
            now = utcnow()
            user.last_login = now
            user.last_active_at = now
            db.add(
                SessionToken(
                    user_id=user.id,
                    token_hash=hash_session_token(token),
                    device_info=request.headers.get("user-agent", "api-client")[:255],
                )
            )
            db.commit()
            db.refresh(user)
            break
        except OperationalError as error:
            db.rollback()
            if not is_transient_database_lock_error(error) or attempt == 2:
                raise HTTPException(
                    status_code=503,
                    detail="Authentication database is busy. Please try again.",
                ) from error
            time.sleep(0.08 * (attempt + 1))
            user = db.scalar(select(User).where(User.email == email))
            if user is None:
                raise HTTPException(status_code=401, detail="Account not found")
            if user.account_status != "active":
                raise HTTPException(status_code=403, detail="This account is not active")

    return {
        "token": token,
        "user": serialize_user(user),
    }


@app.get("/api/auth/me")
def get_me(user: User = Depends(get_current_user)) -> dict[str, Any]:
    return {"user": serialize_user(user)}


@app.post("/api/auth/logout")
def logout(
    request: Request,
    db: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> dict[str, str]:
    token = optional_bearer_token(request)
    if token:
        session_token = db.scalar(
            select(SessionToken).where(
                SessionToken.token_hash == hash_session_token(token)
            )
        )
        if session_token:
            session_token.revoked_at = utcnow()
            db.commit()
    return {"status": "ok"}


@app.post("/api/sensor-readings")
def create_sensor_reading(
    payload: SensorReadingRequest,
    request: Request,
    lang: str | None = None,
    db: Session = Depends(get_db),
) -> dict[str, Any]:
    recorded_at = parse_client_timestamp(payload.timestamp)
    alert_rule = get_effective_alert_rule(db)
    reading_status = normalize_sensor_reading_status(
        payload.status
    ) or determine_sensor_status_snapshot(
        payload.temperature,
        payload.humidity,
        payload.soilMoisture,
        payload.pH,
        alert_rule,
    )
    node_status = NODE_STATUS_BY_READING_STATUS[reading_status]

    node = db.get(SensorNode, payload.nodeId)
    if node is None:
        node = SensorNode(
            node_id=payload.nodeId,
            device_name=payload.deviceName,
            location=payload.location,
            firmware_version=payload.firmwareVersion,
            status=node_status,
            last_heartbeat=recorded_at,
        )
        db.add(node)
    else:
        node.device_name = payload.deviceName
        node.location = payload.location
        node.firmware_version = payload.firmwareVersion
        node.status = node_status
        node.last_heartbeat = recorded_at

    reading = SensorReading(
        node_id=payload.nodeId,
        temperature=payload.temperature,
        humidity=payload.humidity,
        soil_moisture=payload.soilMoisture,
        ph_level=payload.pH,
        status=reading_status,
        recorded_at=recorded_at,
    )
    db.add(reading)
    sensor_record = save_sensor_activity_record(
        db, payload, reading_status, recorded_at
    )

    alert_rows = build_threshold_alerts(
        payload, reading_status, recorded_at, alert_rule
    )
    cooldown_start = recorded_at - timedelta(
        minutes=max(0, alert_rule.cooldown_minutes)
    )
    persisted_alert_rows: list[AlertRecord] = []
    for alert_row in alert_rows:
        duplicate = db.scalar(
            select(AlertRecord)
            .where(AlertRecord.node_id == alert_row.node_id)
            .where(AlertRecord.sensor_type == alert_row.sensor_type)
            .where(AlertRecord.severity == alert_row.severity)
            .where(AlertRecord.created_at >= cooldown_start)
            .order_by(desc(AlertRecord.created_at))
            .limit(1)
        )
        if duplicate is not None:
            persisted_alert_rows.append(duplicate)
            continue
        db.add(alert_row)
        persisted_alert_rows.append(alert_row)

    db.flush()
    if sensor_record is not None:
        serialize_record(sensor_record, db=db, locale="ms")
    for alert_row in persisted_alert_rows:
        serialize_alert(alert_row, db=db, locale="ms")

    db.commit()
    db.refresh(reading)

    return {
        "status": "ok",
        "reading": serialize_sensor_reading(reading),
        "alerts": [
            serialize_alert(
                alert_row,
                db=db,
                locale=request_language(request, lang),
            )
            for alert_row in persisted_alert_rows
        ],
    }


@app.get("/api/sensor-readings/latest")
def latest_sensor_reading(
    node_id: str = DEFAULT_NODE_ID, db: Session = Depends(get_db)
) -> dict[str, Any]:
    reading = db.scalar(
        select(SensorReading)
        .where(SensorReading.node_id == node_id)
        .order_by(desc(SensorReading.created_at), desc(SensorReading.id))
        .limit(1)
    )
    if reading is None:
        return {"reading": None}
    return {"reading": serialize_sensor_reading(reading)}


@app.get("/api/sensor-readings/history")
def sensor_history(
    node_id: str = DEFAULT_NODE_ID, limit: int = 50, db: Session = Depends(get_db)
) -> dict[str, Any]:
    rows = db.scalars(
        select(SensorReading)
        .where(SensorReading.node_id == node_id)
        .order_by(desc(SensorReading.created_at), desc(SensorReading.id))
        .limit(max(1, min(limit, 500)))
    ).all()
    return {"readings": [serialize_sensor_reading(row) for row in rows]}


@app.post("/api/camera-frames")
async def upload_camera_frame(
    request: Request,
    background_tasks: BackgroundTasks,
    node_id: str = DEFAULT_NODE_ID,
    queue_analysis: bool = False,
    image: UploadFile | None = File(default=None),
    file: UploadFile | None = File(default=None),
    db: Session = Depends(get_db),
) -> dict[str, Any]:
    upload = image or file
    image_bytes = await upload.read() if upload is not None else await request.body()
    validate_image_bytes(image_bytes)
    relative_path, image_url = save_upload_bytes(
        image_bytes, f"camera/{node_id}", request=request
    )

    node = db.get(SensorNode, node_id)
    if node is None:
        node = SensorNode(node_id=node_id, latest_image_path=relative_path)
        db.add(node)
    else:
        node.latest_image_path = relative_path
        node.status = "online"
        node.last_heartbeat = utcnow()

    frame = CameraFrame(
        node_id=node_id,
        image_path=relative_path,
        image_url=image_url,
        captured_at=utcnow(),
        uploaded_at=utcnow(),
    )
    db.add(frame)

    analysis_job_id: str | None = None
    if queue_analysis:
        latest_reading = db.scalar(
            select(SensorReading)
            .where(SensorReading.node_id == node_id)
            .order_by(desc(SensorReading.created_at), desc(SensorReading.id))
            .limit(1)
        )
        analysis_job_id = str(uuid4())
        db.add(
            AnalysisJob(
                id=analysis_job_id,
                user_id=None,
                node_id=node_id,
                status="pending",
                image_uri=image_url,
                image_path=relative_path,
                sensor_json=(
                    json.dumps(serialize_sensor_reading(latest_reading))
                    if latest_reading is not None
                    else None
                ),
                session_id=f"esp32-{int(time.time() * 1000)}",
                live_mode=True,
                transport="esp32_camera_auto_analysis",
                created_at=utcnow(),
                updated_at=utcnow(),
            )
        )

    db.commit()
    if analysis_job_id is not None:
        background_tasks.add_task(process_analysis_job, analysis_job_id)

    response = {
        "status": "ok",
        "nodeId": node_id,
        "imageUrl": image_url,
    }
    if analysis_job_id is not None:
        response["analysisJobId"] = analysis_job_id
        response["analysisStatus"] = "pending"
    return response


@app.get("/api/camera-frames/latest")
def latest_camera_frame(
    request: Request,
    node_id: str = DEFAULT_NODE_ID,
    db: Session = Depends(get_db),
) -> dict[str, Any]:
    frame = db.scalar(
        select(CameraFrame)
        .where(CameraFrame.node_id == node_id)
        .order_by(desc(CameraFrame.captured_at), desc(CameraFrame.id))
        .limit(1)
    )
    if frame is None:
        return {"imageUrl": None}
    return {
        "imageUrl": public_or_stored_upload_url(
            frame.image_url, frame.image_path, request
        )
    }


@app.get("/api/camera-frames/history")
def camera_frame_history(
    request: Request,
    node_id: str = DEFAULT_NODE_ID,
    limit: int = 12,
    db: Session = Depends(get_db),
) -> dict[str, Any]:
    rows = db.scalars(
        select(CameraFrame)
        .where(CameraFrame.node_id == node_id)
        .order_by(desc(CameraFrame.captured_at), desc(CameraFrame.id))
        .limit(max(1, min(limit, 100)))
    ).all()
    return {"frames": [serialize_camera_frame(row, request) for row in rows]}


@app.get("/api/alerts")
def get_alerts(
    request: Request,
    lang: str | None = None,
    db: Session = Depends(get_db),
    user: User | None = Depends(get_optional_user),
) -> dict[str, Any]:
    statement: Select[tuple[AlertRecord]] = select(AlertRecord).order_by(
        desc(AlertRecord.created_at), desc(AlertRecord.id)
    )
    if user is not None and not is_admin_role(user):
        statement = statement.where(
            or_(AlertRecord.user_id == user.id, AlertRecord.user_id.is_(None))
        )
    rows = db.scalars(statement.limit(50)).all()
    locale = request_language(request, lang)
    alerts = [serialize_alert(row, db=db, locale=locale) for row in rows]
    if locale != DEFAULT_LOCALE:
        db.commit()
    return {"alerts": alerts}


@app.patch("/api/alerts/{alert_id}/read")
def mark_alert_read(
    alert_id: int,
    db: Session = Depends(get_db),
    user: User | None = Depends(get_optional_user),
) -> dict[str, str]:
    alert_row = db.get(AlertRecord, alert_id)
    if alert_row is None:
        raise HTTPException(status_code=404, detail="Alert not found")
    alert_row.is_read = True
    alert_row.read_at = utcnow()
    db.commit()
    return {"status": "ok"}


@app.get("/api/records")
def get_records(
    request: Request,
    record_type: str | None = None,
    lang: str | None = None,
    db: Session = Depends(get_db),
    user: User | None = Depends(get_optional_user),
) -> dict[str, Any]:
    statement: Select[tuple[ActivityRecord]] = select(ActivityRecord).order_by(
        desc(ActivityRecord.record_timestamp),
        desc(ActivityRecord.created_at),
    )
    if user is not None and not is_admin_role(user):
        statement = statement.where(
            or_(ActivityRecord.user_id == user.id, ActivityRecord.user_id.is_(None))
        )
    if record_type:
        statement = statement.where(
            ActivityRecord.record_type == normalize_record_type_for_storage(record_type)
        )
    rows = db.scalars(statement.limit(100)).all()
    locale = request_language(request, lang)
    records = [serialize_record(row, request, db=db, locale=locale) for row in rows]
    if locale != DEFAULT_LOCALE:
        db.commit()
    return {"records": dedupe_serialized_records(records)}


@app.post("/api/records")
def create_record(
    payload: RecordRequest,
    request: Request,
    lang: str | None = None,
    db: Session = Depends(get_db),
    user: User | None = Depends(get_optional_user),
) -> dict[str, Any]:
    user_id = user.id if user is not None else None
    record_timestamp = parse_client_timestamp(payload.timestamp)
    if normalize_record_type_for_storage(payload.type) == "scan":
        record = save_scan_activity_record(
            db,
            user_id,
            payload.title,
            payload.description,
            record_timestamp,
            scan_result=payload.scanResult,
            treatment=payload.treatment,
        )
    else:
        record = save_activity_record(
            db,
            user_id,
            payload.type,
            payload.title,
            payload.description,
            record_timestamp,
            scan_result=payload.scanResult,
            treatment=payload.treatment,
        )
    persist_treatment_record(
        db,
        record,
        record_title=payload.title,
        treatment=payload.treatment,
    )
    db.flush()
    serialize_record(record, request, db=db, locale="ms")
    db.commit()
    db.refresh(record)
    locale = request_language(request, lang)
    return {"record": serialize_record(record, request, db=db, locale=locale)}


@app.delete("/api/records/{record_id}")
def delete_record(
    record_id: str,
    db: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> dict[str, str]:
    record = db.get(ActivityRecord, record_id)
    if record is None:
        raise HTTPException(status_code=404, detail="Record not found")

    if not is_admin_role(user) and record.user_id not in {user.id, None}:
        raise HTTPException(
            status_code=403,
            detail="You can only remove records visible to your account",
        )

    remove_record_dependencies(db, record.id)
    db.delete(record)
    db.commit()
    return {"status": "ok", "recordId": record_id}


@app.post("/api/analysis-jobs")
async def create_analysis_job(
    request: Request,
    background_tasks: BackgroundTasks,
    db: Session = Depends(get_db),
    user: User | None = Depends(get_optional_user),
) -> dict[str, Any]:
    image_uri: str | None = None
    image_path: str | None = None
    sensor_payload: dict[str, Any] | None = None
    session_id = ""
    live_mode = False

    content_type = request.headers.get("content-type", "")
    if content_type.startswith("application/json"):
        payload = await request.json()
        image_uri = str(payload.get("image_url") or "").strip() or None
        sensor_payload = parse_json_value(payload.get("sensor_reading"))
        request_node_id = str(
            payload.get("node_id") or payload.get("nodeId") or ""
        ).strip()
        session_id = str(payload.get("session_id") or "").strip()
        live_mode = bool(payload.get("live_mode"))
    else:
        form = await request.form()
        upload = form.get("image")
        image_uri = str(form.get("image_url") or "").strip() or None
        sensor_payload = parse_json_value(form.get("sensor_reading"))
        request_node_id = str(form.get("node_id") or form.get("nodeId") or "").strip()
        session_id = str(form.get("session_id") or "").strip()
        live_mode = str(form.get("live_mode") or "").strip().lower() in {
            "1",
            "true",
            "yes",
            "on",
        }

        if upload is not None and hasattr(upload, "read"):
            image_bytes = await upload.read()
            validate_image_bytes(image_bytes)
            image_path, image_uri = save_upload_bytes(
                image_bytes, "analysis_jobs", request=request
            )

    if not image_uri and not image_path:
        raise HTTPException(
            status_code=400, detail="Provide either an uploaded image or image_url"
        )

    node_id: str | None = None
    if request_node_id:
        node_id = request_node_id
    if isinstance(sensor_payload, dict):
        raw_node_id = sensor_payload.get("nodeId") or sensor_payload.get("node_id")
        if raw_node_id:
            node_id = str(raw_node_id)

    job = AnalysisJob(
        user_id=user.id if user is not None else None,
        node_id=node_id,
        status="pending",
        image_uri=image_uri,
        image_path=image_path,
        sensor_json=json.dumps(sensor_payload) if sensor_payload is not None else None,
        session_id=session_id or None,
        live_mode=live_mode,
        transport="backend_queue",
        created_at=utcnow(),
        updated_at=utcnow(),
    )
    db.add(job)
    db.commit()
    db.refresh(job)

    background_tasks.add_task(process_analysis_job, job.id)
    return {"jobId": job.id, "status": job.status}


@app.get("/api/analysis-jobs/{job_id}")
def get_analysis_job(
    job_id: str,
    request: Request,
    lang: str | None = None,
    db: Session = Depends(get_db),
    user: User | None = Depends(get_optional_user),
) -> dict[str, Any]:
    job = db.get(AnalysisJob, job_id)
    if job is None:
        raise HTTPException(status_code=404, detail="Analysis job not found")
    locale = request_language(request, lang)
    payload = serialize_job(job, request, db=db, locale=locale)
    if locale != DEFAULT_LOCALE:
        db.commit()
    return payload


@app.get("/api/analysis-jobs")
def list_analysis_jobs(
    request: Request,
    status_filter: str | None = None,
    lang: str | None = None,
    limit: int = 25,
    db: Session = Depends(get_db),
    user: User | None = Depends(get_optional_user),
) -> dict[str, Any]:
    statement: Select[tuple[AnalysisJob]] = select(AnalysisJob).order_by(
        desc(AnalysisJob.updated_at),
        desc(AnalysisJob.created_at),
    )
    if user is not None and not is_admin_role(user):
        statement = statement.where(
            or_(AnalysisJob.user_id == user.id, AnalysisJob.user_id.is_(None))
        )
    if status_filter:
        statement = statement.where(AnalysisJob.status == status_filter)
    rows = db.scalars(statement.limit(max(1, min(limit, 100)))).all()
    locale = request_language(request, lang)
    jobs = [serialize_job(row, request, db=db, locale=locale) for row in rows]
    if locale != DEFAULT_LOCALE:
        db.commit()
    return {"jobs": jobs}


@app.get("/api/settings/profile")
def get_profile_settings(
    db: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> dict[str, Any]:
    settings = ensure_user_settings(db, user.id)
    db.commit()
    return {"settings": serialize_settings(settings)}


@app.put("/api/settings/profile")
def update_profile_settings(
    payload: UserSettingsRequest,
    db: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> dict[str, Any]:
    settings = ensure_user_settings(db, user.id)
    apply_settings_update(settings, payload)
    db.commit()
    return {"settings": serialize_settings(settings)}


@app.get("/api/settings/iot-wifi")
def get_user_iot_wifi_settings(
    db: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> dict[str, Any]:
    return iot_wifi_settings_payload(db)


@app.put("/api/settings/iot-wifi")
def update_user_iot_wifi_settings(
    payload: IoTWifiSettingsRequest,
    db: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> dict[str, Any]:
    apply_iot_wifi_settings_update(db, payload=payload, user=user)
    db.commit()
    return iot_wifi_settings_payload(db)


@app.get("/api/admin/settings/iot-wifi")
def get_iot_wifi_settings(
    db: Session = Depends(get_db),
    user: User = Depends(get_admin_user),
) -> dict[str, Any]:
    return iot_wifi_settings_payload(db)


@app.put("/api/admin/settings/iot-wifi")
def update_iot_wifi_settings(
    payload: IoTWifiSettingsRequest,
    db: Session = Depends(get_db),
    user: User = Depends(get_admin_user),
) -> dict[str, Any]:
    apply_iot_wifi_settings_update(db, payload=payload, user=user)
    db.commit()
    return iot_wifi_settings_payload(db)


@app.get("/api/iot/wifi-config")
def get_iot_wifi_config_for_device(
    request: Request,
    deviceType: str | None = None,
    device: str | None = None,
    token: str | None = None,
    db: Session = Depends(get_db),
) -> dict[str, Any]:
    require_iot_wifi_config_access(request, token)
    device_type = normalize_iot_wifi_device_type(deviceType or device)
    return {
        "config": serialize_iot_wifi_device(
            db.get(IoTWifiConfig, device_type), device_type
        )
    }


@app.get("/api/system/entity-coverage")
def get_database_entity_coverage(
    db: Session = Depends(get_db),
    user: User = Depends(get_current_user),
) -> dict[str, Any]:
    return database_entity_coverage_payload(db)


@app.get("/api/admin/settings/alert-rules")
def get_alert_rule_settings(
    db: Session = Depends(get_db),
    user: User = Depends(get_admin_user),
) -> dict[str, Any]:
    rule = get_effective_alert_rule(db)
    return {"rule": serialize_alert_rule(rule)}


@app.put("/api/admin/settings/alert-rules")
def update_alert_rule_settings(
    payload: AlertRuleRequest,
    db: Session = Depends(get_db),
    user: User = Depends(get_admin_user),
) -> dict[str, Any]:
    rule = get_effective_alert_rule(db)
    apply_alert_rule_update(rule, payload)
    db.commit()
    return {"rule": serialize_alert_rule(rule)}


@app.get("/api/admin/users")
def list_admin_users(
    include_removed: bool = False,
    db: Session = Depends(get_db),
    user: User = Depends(get_admin_user),
) -> dict[str, Any]:
    statement: Select[tuple[User]] = select(User).order_by(
        desc(User.last_active_at), desc(User.created_at)
    )
    if not include_removed:
        statement = statement.where(User.account_status == "active")
    rows = db.scalars(statement).all()
    return {"users": [serialize_admin_user(row) for row in rows]}


@app.post("/api/admin/users")
def create_admin_user(
    payload: AdminUserCreateRequest,
    db: Session = Depends(get_db),
    user: User = Depends(get_admin_user),
) -> dict[str, Any]:
    email = payload.email.strip().lower()
    if not email:
        raise HTTPException(status_code=400, detail="Email is required")
    role = normalize_admin_managed_role(payload.role)
    password = payload.password.strip()
    if len(password) < 6:
        raise HTTPException(
            status_code=400, detail="Password must be at least 6 characters"
        )
    existing = db.scalar(select(User).where(User.email == email))
    if existing is not None:
        if existing.account_status == "active":
            raise HTTPException(
                status_code=409, detail="A user with this email already exists"
            )
        existing.password_hash = create_password_hash(password)
        existing.display_name = payload.displayName.strip() or default_display_name(
            email
        )
        existing.role = role
        existing.farm_location = payload.location.strip() or DEFAULT_FIELD_LOCATION
        existing.account_status = "active"
        existing.last_login = utcnow()
        existing.last_active_at = utcnow()
        ensure_user_settings(db, existing.id)
        db.commit()
        db.refresh(existing)
        return {"user": serialize_admin_user(existing)}
    created_user = User(
        email=email,
        password_hash=create_password_hash(password),
        display_name=payload.displayName.strip() or default_display_name(email),
        role=role,
        farm_location=payload.location.strip() or DEFAULT_FIELD_LOCATION,
        account_status="active",
        created_at=utcnow(),
        last_login=utcnow(),
        last_active_at=utcnow(),
    )
    db.add(created_user)
    db.flush()
    ensure_user_settings(db, created_user.id)
    db.commit()
    db.refresh(created_user)
    return {"user": serialize_admin_user(created_user)}


@app.delete("/api/admin/users/{target_user_id}")
def remove_admin_user(
    target_user_id: str,
    payload: AdminUserRemoveRequest,
    db: Session = Depends(get_db),
    user: User = Depends(get_admin_user),
) -> dict[str, Any]:
    if user.role != "admin":
        raise HTTPException(
            status_code=403, detail="Only admin accounts can remove users"
        )
    if not verify_password(payload.adminPassword, user.password_hash):
        raise HTTPException(status_code=403, detail="Admin password did not match")

    target_user = db.get(User, target_user_id)
    if target_user is None or target_user.account_status != "active":
        raise HTTPException(status_code=404, detail="User not found")
    if target_user.id == user.id:
        raise HTTPException(
            status_code=400, detail="You cannot remove your own admin account"
        )

    if target_user.role == "admin":
        active_admins = (
            db.scalar(
                select(func.count())
                .select_from(User)
                .where(User.role == "admin")
                .where(User.account_status == "active")
            )
            or 0
        )
        if active_admins <= 1:
            raise HTTPException(
                status_code=400, detail="At least one active admin account is required"
            )

    removal_time = utcnow()
    target_user.account_status = "disabled"
    target_user.last_active_at = removal_time
    db.query(SessionToken).filter(SessionToken.user_id == target_user.id).filter(
        SessionToken.revoked_at.is_(None)
    ).update({SessionToken.revoked_at: removal_time}, synchronize_session=False)
    db.commit()
    db.refresh(target_user)
    return {"status": "ok", "user": serialize_admin_user(target_user)}


@app.get("/api/admin/record-reviews")
def list_record_reviews(
    limit: int = 50,
    db: Session = Depends(get_db),
    user: User = Depends(get_admin_user),
) -> dict[str, Any]:
    rows = db.scalars(
        select(RecordReview)
        .order_by(desc(RecordReview.reviewed_at))
        .limit(max(1, min(limit, 100)))
    ).all()
    return {"reviews": [serialize_record_review(row) for row in rows]}


@app.post("/api/admin/record-reviews")
def create_record_review(
    payload: RecordReviewRequest,
    db: Session = Depends(get_db),
    user: User = Depends(get_admin_user),
) -> dict[str, Any]:
    record = db.get(ActivityRecord, payload.recordId)
    if record is None:
        raise HTTPException(status_code=404, detail="Record not found")
    if payload.assignedToUserId:
        assigned_user = db.get(User, payload.assignedToUserId)
        if assigned_user is None:
            raise HTTPException(
                status_code=404, detail="Assigned follow-up user not found"
            )
    review = RecordReview(
        record_id=payload.recordId,
        reviewed_by_user_id=user.id,
        assigned_to_user_id=payload.assignedToUserId,
        review_status=payload.reviewStatus,
        review_note=payload.reviewNote.strip(),
        reviewed_at=utcnow(),
    )
    db.add(review)
    db.commit()
    db.refresh(review)
    return {"review": serialize_record_review(review)}


@app.get("/api/admin/summary")
def admin_summary(
    request: Request,
    db: Session = Depends(get_db),
    user: User = Depends(get_admin_user),
) -> dict[str, Any]:
    users = db.scalars(select(User).where(User.account_status == "active")).all()
    unread_alerts = (
        db.scalar(
            select(func.count())
            .select_from(AlertRecord)
            .where(AlertRecord.is_read.is_(False))
        )
        or 0
    )
    queued_jobs = (
        db.scalar(
            select(func.count())
            .select_from(AnalysisJob)
            .where(AnalysisJob.status.in_(["pending", "processing"]))
        )
        or 0
    )
    completed_jobs = (
        db.scalar(
            select(func.count())
            .select_from(AnalysisJob)
            .where(AnalysisJob.status == "done")
        )
        or 0
    )
    total_records = db.scalar(select(func.count()).select_from(ActivityRecord)) or 0
    total_nodes = db.scalar(select(func.count()).select_from(SensorNode)) or 0
    online_nodes = (
        db.scalar(
            select(func.count())
            .select_from(SensorNode)
            .where(SensorNode.status.in_(["online", "warning", "critical"]))
        )
        or 0
    )

    latest_reading = db.scalar(
        select(SensorReading)
        .order_by(desc(SensorReading.created_at), desc(SensorReading.id))
        .limit(1)
    )
    latest_frame = db.scalar(
        select(CameraFrame)
        .order_by(desc(CameraFrame.captured_at), desc(CameraFrame.id))
        .limit(1)
    )
    latest_review = db.scalar(
        select(RecordReview).order_by(desc(RecordReview.reviewed_at)).limit(1)
    )
    rule = get_effective_alert_rule(db)

    return {
        "generatedAt": datetime_to_unix_ms(utcnow()),
        "database": {
            "kind": database_kind(),
            "fallbackActive": database_fallback_active(),
            "fallbackReason": database_fallback_reason(),
        },
        "users": {
            "total": len(users),
            "admins": len([item for item in users if item.role == "admin"]),
            "officers": len([item for item in users if item.role == "officer"]),
            "farmers": len([item for item in users if item.role == "farmer"]),
        },
        "alerts": {
            "unread": unread_alerts,
        },
        "analysisJobs": {
            "queued": queued_jobs,
            "completed": completed_jobs,
        },
        "records": {
            "total": total_records,
        },
        "nodes": {
            "total": total_nodes,
            "online": online_nodes,
            "latestReading": serialize_sensor_reading(latest_reading)
            if latest_reading
            else None,
            "latestFrame": serialize_camera_frame(latest_frame, request)
            if latest_frame
            else None,
        },
        "latestReview": serialize_record_review(latest_review)
        if latest_review
        else None,
        "alertRule": serialize_alert_rule(rule),
    }


if __name__ == "__main__":
    import uvicorn

    uvicorn.run(
        "ollama_backend:app", host=BACKEND_HOST, port=BACKEND_PORT, reload=False
    )
