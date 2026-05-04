from __future__ import annotations

import hashlib
import json
import os
import secrets
from contextlib import contextmanager
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any, Iterator
from urllib.parse import unquote
from uuid import uuid4

from sqlalchemy import (
    Boolean,
    DateTime,
    Float,
    ForeignKey,
    Integer,
    String,
    Text,
    create_engine,
    inspect,
    select,
    text,
)
from sqlalchemy.engine import make_url
from sqlalchemy.exc import SQLAlchemyError
from sqlalchemy.orm import DeclarativeBase, Mapped, Session, mapped_column, sessionmaker


BASE_DIR = Path(__file__).resolve().parent
DEFAULT_SQLITE_URL = f"sqlite:///{(BASE_DIR / 'pineguard.db').as_posix()}"
REQUESTED_DATABASE_URL = os.getenv("DATABASE_URL", DEFAULT_SQLITE_URL).strip() or DEFAULT_SQLITE_URL
DATABASE_FALLBACK_URL = os.getenv("DATABASE_FALLBACK_URL", DEFAULT_SQLITE_URL).strip() or DEFAULT_SQLITE_URL
ALLOW_DATABASE_FALLBACK = (
    os.getenv("ALLOW_DATABASE_FALLBACK", "true").strip().lower()
    not in {"0", "false", "no", "off"}
)
DATABASE_URL = REQUESTED_DATABASE_URL
IS_SQLITE = DATABASE_URL.startswith("sqlite")
DATABASE_FALLBACK_ACTIVE = False
DATABASE_FALLBACK_REASON: str | None = None


def normalize_database_url(url: str) -> str:
    if url.startswith("sqlite"):
        return url

    try:
        parsed = make_url(url)
    except Exception:
        return url

    if parsed.database and "%" in parsed.database:
        parsed = parsed.set(database=unquote(parsed.database))

    return parsed.render_as_string(hide_password=False)


REQUESTED_DATABASE_URL = normalize_database_url(REQUESTED_DATABASE_URL)
DATABASE_FALLBACK_URL = normalize_database_url(DATABASE_FALLBACK_URL)
DATABASE_URL = normalize_database_url(DATABASE_URL)
IS_SQLITE = DATABASE_URL.startswith("sqlite")

DEFAULT_TEMP_HIGH = 35.0
DEFAULT_TEMP_LOW = 15.0
DEFAULT_HUMIDITY_LOW = 40.0
DEFAULT_SOIL_DRY = 30.0
DEFAULT_SOIL_CRITICAL_DRY = 20.0
DEFAULT_SOIL_WET = 90.0
DEFAULT_PH_LOW = 4.5
DEFAULT_PH_HIGH = 6.5
DEFAULT_ALERT_RULE_SCOPE = "system_default"
DEFAULT_SESSION_TTL_DAYS = 7

def _create_database_engine(url: str):
    is_sqlite = url.startswith("sqlite")
    kwargs: dict[str, Any] = {}
    if is_sqlite:
        kwargs["connect_args"] = {"check_same_thread": False}
    else:
        kwargs["pool_pre_ping"] = True
        kwargs["pool_recycle"] = int(os.getenv("DATABASE_POOL_RECYCLE_SECONDS", "180"))
        kwargs["pool_timeout"] = int(os.getenv("DATABASE_POOL_TIMEOUT_SECONDS", "30"))
        kwargs["connect_args"] = {
            "connect_timeout": int(os.getenv("DATABASE_CONNECT_TIMEOUT_SECONDS", "10")),
            "read_timeout": int(os.getenv("DATABASE_READ_TIMEOUT_SECONDS", "30")),
            "write_timeout": int(os.getenv("DATABASE_WRITE_TIMEOUT_SECONDS", "30")),
        }
    return create_engine(url, **kwargs)


def _switch_database_url(url: str, *, fallback_reason: str | None = None) -> None:
    global DATABASE_URL, IS_SQLITE, engine, SessionLocal
    global DATABASE_FALLBACK_ACTIVE, DATABASE_FALLBACK_REASON

    DATABASE_URL = normalize_database_url(url)
    IS_SQLITE = DATABASE_URL.startswith("sqlite")
    DATABASE_FALLBACK_ACTIVE = fallback_reason is not None
    DATABASE_FALLBACK_REASON = fallback_reason
    engine = _create_database_engine(DATABASE_URL)
    SessionLocal.configure(bind=engine)


engine = _create_database_engine(DATABASE_URL)
SessionLocal = sessionmaker(bind=engine, autoflush=False, autocommit=False, expire_on_commit=False)


class Base(DeclarativeBase):
    pass


class User(Base):
    __tablename__ = "users"

    id: Mapped[str] = mapped_column(String(36), primary_key=True, default=lambda: str(uuid4()))
    email: Mapped[str] = mapped_column(String(255), unique=True, index=True)
    password_hash: Mapped[str] = mapped_column(String(255))
    display_name: Mapped[str] = mapped_column(String(255))
    role: Mapped[str] = mapped_column(String(32), default="farmer", index=True)
    farm_location: Mapped[str] = mapped_column(String(255), default="Johor, Malaysia")
    account_status: Mapped[str] = mapped_column(String(32), default="active", index=True)
    created_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=lambda: utcnow())
    last_login: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=lambda: utcnow())
    last_active_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=lambda: utcnow())


class SessionToken(Base):
    __tablename__ = "session_tokens"

    id: Mapped[str] = mapped_column(String(36), primary_key=True, default=lambda: str(uuid4()))
    user_id: Mapped[str] = mapped_column(String(36), ForeignKey("users.id"), index=True)
    token_hash: Mapped[str] = mapped_column(String(128), unique=True, index=True)
    device_info: Mapped[str] = mapped_column(String(255), default="unknown")
    created_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=lambda: utcnow())
    expires_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=lambda: utcnow() + timedelta(days=DEFAULT_SESSION_TTL_DAYS))
    revoked_at: Mapped[datetime | None] = mapped_column(DateTime(timezone=True), nullable=True)


class UserSettings(Base):
    __tablename__ = "user_settings"

    user_id: Mapped[str] = mapped_column(String(36), ForeignKey("users.id"), primary_key=True)
    notification_preferences_json: Mapped[str] = mapped_column(
        Text,
        default='{"pushNotificationsEnabled": true, "dailyDigestEnabled": true, "assistantRecommendationsEnabled": true}',
    )
    sync_cadence: Mapped[str] = mapped_column(String(64), default="realtime")
    language: Mapped[str] = mapped_column(String(32), default="en")
    theme: Mapped[str] = mapped_column(String(32), default="system")
    backend_mode: Mapped[str] = mapped_column(String(64), default="mysql")
    updated_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=lambda: utcnow())


class SensorNode(Base):
    __tablename__ = "sensor_nodes"

    node_id: Mapped[str] = mapped_column(String(64), primary_key=True)
    device_name: Mapped[str] = mapped_column(String(255), default="Demo Zone - Pineapple Plant")
    location: Mapped[str] = mapped_column(String(255), default="Single demonstration area")
    firmware_version: Mapped[str] = mapped_column(String(64), default="1.0.0")
    status: Mapped[str] = mapped_column(String(32), default="online", index=True)
    last_heartbeat: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=lambda: utcnow())
    latest_image_path: Mapped[str | None] = mapped_column(String(512), nullable=True)
    created_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=lambda: utcnow())


class SensorReading(Base):
    __tablename__ = "sensor_readings"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    node_id: Mapped[str] = mapped_column(String(64), ForeignKey("sensor_nodes.node_id"), index=True)
    temperature: Mapped[float] = mapped_column(Float)
    humidity: Mapped[float] = mapped_column(Float)
    soil_moisture: Mapped[float] = mapped_column(Float)
    ph_level: Mapped[float] = mapped_column(Float)
    status: Mapped[str] = mapped_column(String(32))
    recorded_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), index=True)
    created_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=lambda: utcnow())


class CameraFrame(Base):
    __tablename__ = "camera_frames"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    node_id: Mapped[str] = mapped_column(String(64), ForeignKey("sensor_nodes.node_id"), index=True)
    image_path: Mapped[str] = mapped_column(String(1024))
    image_url: Mapped[str | None] = mapped_column(String(1024), nullable=True)
    captured_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=lambda: utcnow(), index=True)
    uploaded_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=lambda: utcnow())


class AlertRule(Base):
    __tablename__ = "alert_rules"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    scope: Mapped[str] = mapped_column(String(64), unique=True, index=True, default=DEFAULT_ALERT_RULE_SCOPE)
    temp_high: Mapped[float] = mapped_column(Float, default=DEFAULT_TEMP_HIGH)
    temp_low: Mapped[float] = mapped_column(Float, default=DEFAULT_TEMP_LOW)
    humidity_low: Mapped[float] = mapped_column(Float, default=DEFAULT_HUMIDITY_LOW)
    soil_dry: Mapped[float] = mapped_column(Float, default=DEFAULT_SOIL_DRY)
    soil_critical_dry: Mapped[float] = mapped_column(Float, default=DEFAULT_SOIL_CRITICAL_DRY)
    soil_wet: Mapped[float] = mapped_column(Float, default=DEFAULT_SOIL_WET)
    ph_low: Mapped[float] = mapped_column(Float, default=DEFAULT_PH_LOW)
    ph_high: Mapped[float] = mapped_column(Float, default=DEFAULT_PH_HIGH)
    cooldown_minutes: Mapped[int] = mapped_column(Integer, default=30)
    updated_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=lambda: utcnow())


class AlertRecord(Base):
    __tablename__ = "alerts"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    node_id: Mapped[str] = mapped_column(String(64), ForeignKey("sensor_nodes.node_id"), index=True)
    user_id: Mapped[str | None] = mapped_column(String(36), ForeignKey("users.id"), nullable=True, index=True)
    alert_rule_id: Mapped[int | None] = mapped_column(Integer, ForeignKey("alert_rules.id"), nullable=True, index=True)
    severity: Mapped[str] = mapped_column(String(32))
    sensor_type: Mapped[str] = mapped_column(String(64))
    sensor_value: Mapped[float] = mapped_column(Float)
    threshold_value: Mapped[float] = mapped_column(Float)
    message: Mapped[str] = mapped_column(Text)
    is_read: Mapped[bool] = mapped_column(Boolean, default=False)
    created_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=lambda: utcnow(), index=True)
    read_at: Mapped[datetime | None] = mapped_column(DateTime(timezone=True), nullable=True)


class ActivityRecord(Base):
    __tablename__ = "activity_records"

    id: Mapped[str] = mapped_column(String(36), primary_key=True, default=lambda: str(uuid4()))
    user_id: Mapped[str | None] = mapped_column(String(36), ForeignKey("users.id"), nullable=True, index=True)
    record_type: Mapped[str] = mapped_column(String(32), index=True)
    title: Mapped[str] = mapped_column(String(255))
    description: Mapped[str] = mapped_column(Text)
    record_timestamp: Mapped[datetime] = mapped_column(DateTime(timezone=True), index=True)
    scan_result_json: Mapped[str | None] = mapped_column(Text, nullable=True)
    treatment_json: Mapped[str | None] = mapped_column(Text, nullable=True)
    created_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=lambda: utcnow())


class TreatmentRecord(Base):
    __tablename__ = "treatment_records"

    id: Mapped[str] = mapped_column(String(36), primary_key=True, default=lambda: str(uuid4()))
    record_id: Mapped[str] = mapped_column(String(36), ForeignKey("activity_records.id"), index=True)
    title: Mapped[str] = mapped_column(String(255))
    description: Mapped[str] = mapped_column(Text)
    product_used: Mapped[str | None] = mapped_column(String(255), nullable=True)
    dosage: Mapped[str | None] = mapped_column(String(255), nullable=True)
    follow_up_date: Mapped[datetime | None] = mapped_column(DateTime(timezone=True), nullable=True)
    created_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=lambda: utcnow())


class AnalysisJob(Base):
    __tablename__ = "analysis_jobs"

    id: Mapped[str] = mapped_column(String(36), primary_key=True, default=lambda: str(uuid4()))
    user_id: Mapped[str | None] = mapped_column(String(36), ForeignKey("users.id"), nullable=True, index=True)
    node_id: Mapped[str | None] = mapped_column(String(64), ForeignKey("sensor_nodes.node_id"), nullable=True, index=True)
    status: Mapped[str] = mapped_column(String(32), index=True, default="pending")
    image_uri: Mapped[str | None] = mapped_column(String(1024), nullable=True)
    image_path: Mapped[str | None] = mapped_column(String(1024), nullable=True)
    sensor_json: Mapped[str | None] = mapped_column(Text, nullable=True)
    session_id: Mapped[str | None] = mapped_column(String(255), nullable=True)
    live_mode: Mapped[bool] = mapped_column(Boolean, default=False)
    transport: Mapped[str] = mapped_column(String(64), default="backend_queue")
    scan_result_json: Mapped[str | None] = mapped_column(Text, nullable=True)
    live_assessment_json: Mapped[str | None] = mapped_column(Text, nullable=True)
    meta_json: Mapped[str | None] = mapped_column(Text, nullable=True)
    error: Mapped[str | None] = mapped_column(Text, nullable=True)
    created_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=lambda: utcnow())
    updated_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=lambda: utcnow(), index=True)
    completed_at: Mapped[datetime | None] = mapped_column(DateTime(timezone=True), nullable=True)


class RecordReview(Base):
    __tablename__ = "record_reviews"

    id: Mapped[str] = mapped_column(String(36), primary_key=True, default=lambda: str(uuid4()))
    record_id: Mapped[str] = mapped_column(String(36), ForeignKey("activity_records.id"), index=True)
    reviewed_by_user_id: Mapped[str] = mapped_column(String(36), ForeignKey("users.id"), index=True)
    assigned_to_user_id: Mapped[str | None] = mapped_column(String(36), ForeignKey("users.id"), nullable=True, index=True)
    review_status: Mapped[str] = mapped_column(String(64), default="pending", index=True)
    review_note: Mapped[str] = mapped_column(Text, default="")
    reviewed_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=lambda: utcnow())


class Expert(Base):
    __tablename__ = "experts"

    id: Mapped[str] = mapped_column(String(36), primary_key=True, default=lambda: str(uuid4()))
    full_name: Mapped[str] = mapped_column(String(255))
    specialization: Mapped[str] = mapped_column(String(255))
    organization: Mapped[str] = mapped_column(String(255))
    contact_info: Mapped[str] = mapped_column(String(255))
    status: Mapped[str] = mapped_column(String(64), default="available")


class ConsultationSession(Base):
    __tablename__ = "consultation_sessions"

    id: Mapped[str] = mapped_column(String(36), primary_key=True, default=lambda: str(uuid4()))
    user_id: Mapped[str] = mapped_column(String(36), ForeignKey("users.id"), index=True)
    expert_id: Mapped[str] = mapped_column(String(36), ForeignKey("experts.id"), index=True)
    record_id: Mapped[str | None] = mapped_column(String(36), ForeignKey("activity_records.id"), nullable=True, index=True)
    status: Mapped[str] = mapped_column(String(64), default="open", index=True)
    started_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=lambda: utcnow())
    closed_at: Mapped[datetime | None] = mapped_column(DateTime(timezone=True), nullable=True)


class ConsultationMessage(Base):
    __tablename__ = "consultation_messages"

    id: Mapped[str] = mapped_column(String(36), primary_key=True, default=lambda: str(uuid4()))
    consultation_id: Mapped[str] = mapped_column(String(36), ForeignKey("consultation_sessions.id"), index=True)
    sender_role: Mapped[str] = mapped_column(String(64))
    sender_name: Mapped[str] = mapped_column(String(255))
    message_text: Mapped[str] = mapped_column(Text)
    image_url: Mapped[str | None] = mapped_column(String(1024), nullable=True)
    sent_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=lambda: utcnow())


def utcnow() -> datetime:
    return datetime.now(timezone.utc)


def _database_kind_for_url(url: str) -> str:
    if url.startswith("sqlite"):
        return "sqlite"
    try:
        backend = make_url(url).get_backend_name()
    except Exception:
        return "unknown"
    if backend in {"mysql", "mariadb"}:
        return "mysql"
    return backend


def database_kind() -> str:
    return _database_kind_for_url(DATABASE_URL)


def requested_database_kind() -> str:
    return _database_kind_for_url(REQUESTED_DATABASE_URL)


def database_fallback_active() -> bool:
    return DATABASE_FALLBACK_ACTIVE


def database_fallback_reason() -> str | None:
    return DATABASE_FALLBACK_REASON


_NO_DEFAULT = object()


def _quote_identifier(connection, value: str) -> str:
    return connection.dialect.identifier_preparer.quote(value)


def _column_default_value(column) -> Any:
    if column.default is None:
        return _NO_DEFAULT

    arg = column.default.arg
    if callable(arg):
        try:
            return arg()
        except TypeError:
            return _NO_DEFAULT
    return arg


def _add_missing_column(connection, table, column) -> None:
    column_type = column.type.compile(dialect=connection.dialect)
    add_column_sql = (
        f"ALTER TABLE {_quote_identifier(connection, table.name)} "
        f"ADD COLUMN {_quote_identifier(connection, column.name)} {column_type}"
    )
    connection.execute(text(add_column_sql))

    default_value = _column_default_value(column)
    if default_value is not _NO_DEFAULT:
        backfill_sql = (
            f"UPDATE {_quote_identifier(connection, table.name)} "
            f"SET {_quote_identifier(connection, column.name)} = :value "
            f"WHERE {_quote_identifier(connection, column.name)} IS NULL"
        )
        connection.execute(text(backfill_sql), {"value": default_value})


def _create_missing_indexes(connection) -> None:
    for table in Base.metadata.sorted_tables:
        for index in table.indexes:
            index.create(bind=connection, checkfirst=True)


def _enforce_not_null_columns(connection) -> None:
    return


def _mysql_business_rule_triggers() -> dict[str, str]:
    sensor_reading_rules = """
        IF NEW.temperature < -40 OR NEW.temperature > 85 THEN
            SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'temperature out of range';
        END IF;
        IF NEW.humidity < 0 OR NEW.humidity > 100 THEN
            SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'humidity out of range';
        END IF;
        IF NEW.soil_moisture < 0 OR NEW.soil_moisture > 100 THEN
            SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'soil moisture out of range';
        END IF;
        IF NEW.ph_level < 0 OR NEW.ph_level > 14 THEN
            SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'pH out of range';
        END IF;
        IF NEW.status NOT IN ('normal', 'warning', 'critical') THEN
            SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'invalid sensor status';
        END IF;
    """
    alert_record_rules = """
        IF NEW.severity NOT IN ('info', 'warning', 'critical') THEN
            SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'invalid alert severity';
        END IF;
        IF NEW.sensor_type NOT IN ('temperature', 'humidity', 'soilMoisture', 'pH', 'camera', 'system') THEN
            SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'invalid alert sensor type';
        END IF;
        IF NEW.is_read = 0 AND NEW.read_at IS NOT NULL THEN
            SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'unread alert cannot have read_at';
        END IF;
        IF NEW.is_read = 1 AND NEW.read_at IS NULL THEN
            SET NEW.read_at = CURRENT_TIMESTAMP;
        END IF;
    """
    analysis_job_rules = """
        IF NEW.status NOT IN ('pending', 'processing', 'done', 'failed', 'error') THEN
            SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'invalid analysis job status';
        END IF;
        IF NEW.completed_at IS NOT NULL AND NEW.status NOT IN ('done', 'failed', 'error') THEN
            SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'completed_at requires terminal job status';
        END IF;
    """
    consultation_session_rules = """
        IF NEW.status NOT IN ('open', 'closed', 'cancelled') THEN
            SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'invalid consultation status';
        END IF;
        IF NEW.closed_at IS NOT NULL AND NEW.status = 'open' THEN
            SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'open consultation cannot have closed_at';
        END IF;
    """

    return {
        "bi_users_business_rules": """
            CREATE TRIGGER bi_users_business_rules BEFORE INSERT ON users
            FOR EACH ROW
            BEGIN
                IF NEW.email NOT LIKE '%@%' THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'invalid user email';
                END IF;
                IF NEW.role NOT IN ('admin', 'officer', 'farmer') THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'invalid user role';
                END IF;
                IF NEW.account_status NOT IN ('active', 'disabled', 'suspended') THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'invalid account status';
                END IF;
            END
        """,
        "bu_users_business_rules": """
            CREATE TRIGGER bu_users_business_rules BEFORE UPDATE ON users
            FOR EACH ROW
            BEGIN
                IF NEW.email NOT LIKE '%@%' THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'invalid user email';
                END IF;
                IF NEW.role NOT IN ('admin', 'officer', 'farmer') THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'invalid user role';
                END IF;
                IF NEW.account_status NOT IN ('active', 'disabled', 'suspended') THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'invalid account status';
                END IF;
            END
        """,
        "bi_sensor_nodes_business_rules": """
            CREATE TRIGGER bi_sensor_nodes_business_rules BEFORE INSERT ON sensor_nodes
            FOR EACH ROW
            BEGIN
                IF NEW.status NOT IN ('online', 'offline', 'warning', 'critical') THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'invalid node status';
                END IF;
            END
        """,
        "bu_sensor_nodes_business_rules": """
            CREATE TRIGGER bu_sensor_nodes_business_rules BEFORE UPDATE ON sensor_nodes
            FOR EACH ROW
            BEGIN
                IF NEW.status NOT IN ('online', 'offline', 'warning', 'critical') THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'invalid node status';
                END IF;
            END
        """,
        "bi_sensor_readings_business_rules": f"""
            CREATE TRIGGER bi_sensor_readings_business_rules BEFORE INSERT ON sensor_readings
            FOR EACH ROW
            BEGIN
                {sensor_reading_rules}
            END
        """,
        "bu_sensor_readings_business_rules": f"""
            CREATE TRIGGER bu_sensor_readings_business_rules BEFORE UPDATE ON sensor_readings
            FOR EACH ROW
            BEGIN
                {sensor_reading_rules}
            END
        """,
        "bi_camera_frames_business_rules": """
            CREATE TRIGGER bi_camera_frames_business_rules BEFORE INSERT ON camera_frames
            FOR EACH ROW
            BEGIN
                IF NEW.image_path IS NULL OR LENGTH(TRIM(NEW.image_path)) = 0 THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'camera frame image_path is required';
                END IF;
            END
        """,
        "bu_camera_frames_business_rules": """
            CREATE TRIGGER bu_camera_frames_business_rules BEFORE UPDATE ON camera_frames
            FOR EACH ROW
            BEGIN
                IF NEW.image_path IS NULL OR LENGTH(TRIM(NEW.image_path)) = 0 THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'camera frame image_path is required';
                END IF;
            END
        """,
        "bi_alert_rules_business_rules": """
            CREATE TRIGGER bi_alert_rules_business_rules BEFORE INSERT ON alert_rules
            FOR EACH ROW
            BEGIN
                IF NEW.temp_low >= NEW.temp_high THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'temperature rule bounds invalid';
                END IF;
                IF NEW.soil_critical_dry > NEW.soil_dry OR NEW.soil_dry > NEW.soil_wet THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'soil moisture rule bounds invalid';
                END IF;
                IF NEW.ph_low >= NEW.ph_high THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'pH rule bounds invalid';
                END IF;
                IF NEW.cooldown_minutes < 0 THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'cooldown cannot be negative';
                END IF;
            END
        """,
        "bu_alert_rules_business_rules": """
            CREATE TRIGGER bu_alert_rules_business_rules BEFORE UPDATE ON alert_rules
            FOR EACH ROW
            BEGIN
                IF NEW.temp_low >= NEW.temp_high THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'temperature rule bounds invalid';
                END IF;
                IF NEW.soil_critical_dry > NEW.soil_dry OR NEW.soil_dry > NEW.soil_wet THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'soil moisture rule bounds invalid';
                END IF;
                IF NEW.ph_low >= NEW.ph_high THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'pH rule bounds invalid';
                END IF;
                IF NEW.cooldown_minutes < 0 THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'cooldown cannot be negative';
                END IF;
            END
        """,
        "bi_alerts_business_rules": f"""
            CREATE TRIGGER bi_alerts_business_rules BEFORE INSERT ON alerts
            FOR EACH ROW
            BEGIN
                {alert_record_rules}
            END
        """,
        "bu_alerts_business_rules": f"""
            CREATE TRIGGER bu_alerts_business_rules BEFORE UPDATE ON alerts
            FOR EACH ROW
            BEGIN
                {alert_record_rules}
            END
        """,
        "bi_activity_records_business_rules": """
            CREATE TRIGGER bi_activity_records_business_rules BEFORE INSERT ON activity_records
            FOR EACH ROW
            BEGIN
                IF NEW.record_type NOT IN ('scan', 'manual', 'treatment', 'inspection') THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'invalid activity record type';
                END IF;
            END
        """,
        "bu_activity_records_business_rules": """
            CREATE TRIGGER bu_activity_records_business_rules BEFORE UPDATE ON activity_records
            FOR EACH ROW
            BEGIN
                IF NEW.record_type NOT IN ('scan', 'manual', 'treatment', 'inspection') THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'invalid activity record type';
                END IF;
            END
        """,
        "bi_analysis_jobs_business_rules": f"""
            CREATE TRIGGER bi_analysis_jobs_business_rules BEFORE INSERT ON analysis_jobs
            FOR EACH ROW
            BEGIN
                {analysis_job_rules}
            END
        """,
        "bu_analysis_jobs_business_rules": f"""
            CREATE TRIGGER bu_analysis_jobs_business_rules BEFORE UPDATE ON analysis_jobs
            FOR EACH ROW
            BEGIN
                {analysis_job_rules}
            END
        """,
        "bi_record_reviews_business_rules": """
            CREATE TRIGGER bi_record_reviews_business_rules BEFORE INSERT ON record_reviews
            FOR EACH ROW
            BEGIN
                IF NEW.review_status NOT IN ('pending', 'reviewed', 'action_required', 'closed') THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'invalid review status';
                END IF;
            END
        """,
        "bu_record_reviews_business_rules": """
            CREATE TRIGGER bu_record_reviews_business_rules BEFORE UPDATE ON record_reviews
            FOR EACH ROW
            BEGIN
                IF NEW.review_status NOT IN ('pending', 'reviewed', 'action_required', 'closed') THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'invalid review status';
                END IF;
            END
        """,
        "bi_consultation_sessions_business_rules": f"""
            CREATE TRIGGER bi_consultation_sessions_business_rules BEFORE INSERT ON consultation_sessions
            FOR EACH ROW
            BEGIN
                {consultation_session_rules}
            END
        """,
        "bu_consultation_sessions_business_rules": f"""
            CREATE TRIGGER bu_consultation_sessions_business_rules BEFORE UPDATE ON consultation_sessions
            FOR EACH ROW
            BEGIN
                {consultation_session_rules}
            END
        """,
        "bi_consultation_messages_business_rules": """
            CREATE TRIGGER bi_consultation_messages_business_rules BEFORE INSERT ON consultation_messages
            FOR EACH ROW
            BEGIN
                IF NEW.sender_role NOT IN ('farmer', 'admin', 'officer', 'expert', 'system') THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'invalid message sender role';
                END IF;
            END
        """,
        "bu_consultation_messages_business_rules": """
            CREATE TRIGGER bu_consultation_messages_business_rules BEFORE UPDATE ON consultation_messages
            FOR EACH ROW
            BEGIN
                IF NEW.sender_role NOT IN ('farmer', 'admin', 'officer', 'expert', 'system') THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'invalid message sender role';
                END IF;
            END
        """,
    }


def _install_mysql_business_rules(connection) -> None:
    if connection.dialect.name not in {"mysql", "mariadb"}:
        return

    for trigger_name, trigger_sql in _mysql_business_rule_triggers().items():
        connection.execute(text(f"DROP TRIGGER IF EXISTS {trigger_name}"))
        connection.execute(text(trigger_sql))


def schema_gaps(bind=None) -> dict[str, dict[str, list[str] | bool]]:
    target = bind or engine
    inspector = inspect(target)
    existing_tables = set(inspector.get_table_names())
    gaps: dict[str, dict[str, list[str] | bool]] = {}

    for table in Base.metadata.sorted_tables:
        if table.name not in existing_tables:
            gaps[table.name] = {"missing_table": True, "missing_columns": []}
            continue

        existing_columns = {
            column["name"] for column in inspector.get_columns(table.name)
        }
        missing_columns = [
            column.name for column in table.columns if column.name not in existing_columns
        ]
        if missing_columns:
            gaps[table.name] = {
                "missing_table": False,
                "missing_columns": missing_columns,
            }

    return gaps


def _schema_gap_message(gaps: dict[str, dict[str, list[str] | bool]]) -> str:
    details: list[str] = []
    for table_name, info in gaps.items():
        if info.get("missing_table"):
            details.append(f"{table_name}: missing table")
            continue
        missing_columns = info.get("missing_columns") or []
        details.append(
            f"{table_name}: missing columns {', '.join(str(item) for item in missing_columns)}"
        )
    return "; ".join(details)


def reconcile_schema(bind=None) -> None:
    target = bind or engine
    Base.metadata.create_all(target)

    with target.begin() as connection:
        inspector = inspect(connection)
        existing_tables = set(inspector.get_table_names())
        for table in Base.metadata.sorted_tables:
            if table.name not in existing_tables:
                table.create(bind=connection, checkfirst=True)

        inspector = inspect(connection)
        for table in Base.metadata.sorted_tables:
            existing_columns = {
                column["name"] for column in inspector.get_columns(table.name)
            }
            for column in table.columns:
                if column.name not in existing_columns:
                    _add_missing_column(connection, table, column)

        _create_missing_indexes(connection)
        _enforce_not_null_columns(connection)
        _install_mysql_business_rules(connection)

    gaps = schema_gaps(target)
    if gaps:
        raise RuntimeError(
            "Database schema reconciliation did not complete successfully: "
            + _schema_gap_message(gaps)
        )


def _initialize_current_database() -> None:
    if IS_SQLITE:
        Base.metadata.create_all(engine)
    else:
        reconcile_schema(engine)


def initialize_database() -> None:
    try:
        _initialize_current_database()
    except SQLAlchemyError as exc:
        if (
            IS_SQLITE
            or not ALLOW_DATABASE_FALLBACK
            or normalize_database_url(DATABASE_FALLBACK_URL) == DATABASE_URL
        ):
            raise

        _switch_database_url(
            DATABASE_FALLBACK_URL,
            fallback_reason=(
                f"{type(exc).__name__}: {str(exc).splitlines()[0]}"
                if str(exc)
                else type(exc).__name__
            ),
        )
        _initialize_current_database()

    seed_default_data()


def create_session_token() -> str:
    return secrets.token_urlsafe(32)


def hash_session_token(token: str) -> str:
    return hashlib.sha256(token.encode("utf-8")).hexdigest()


def create_password_hash(password: str) -> str:
    salt = secrets.token_bytes(16)
    digest = hashlib.pbkdf2_hmac("sha256", password.encode("utf-8"), salt, 100_000)
    return f"{salt.hex()}${digest.hex()}"


def verify_password(password: str, password_hash: str) -> bool:
    try:
        salt_hex, digest_hex = password_hash.split("$", 1)
    except ValueError:
        return False

    digest = hashlib.pbkdf2_hmac(
        "sha256",
        password.encode("utf-8"),
        bytes.fromhex(salt_hex),
        100_000,
    )
    return secrets.compare_digest(digest.hex(), digest_hex)


def default_display_name(email: str) -> str:
    local = email.split("@", 1)[0].replace(".", " ").replace("_", " ").strip()
    return " ".join(part.capitalize() for part in local.split()) or "Farmer"


def datetime_to_unix_ms(value: datetime | None) -> int | None:
    if value is None:
        return None
    if value.tzinfo is None:
        value = value.replace(tzinfo=timezone.utc)
    return int(value.timestamp() * 1000)


def parse_client_timestamp(raw_value: Any) -> datetime:
    if raw_value is None:
        return utcnow()

    if isinstance(raw_value, (int, float)):
        number = float(raw_value)
        if number > 1_000_000_000_000:
            number = number / 1000
        return datetime.fromtimestamp(number, tz=timezone.utc)

    value = str(raw_value).strip()
    if not value:
        return utcnow()

    if value.isdigit():
        return parse_client_timestamp(int(value))

    normalized = value.replace("Z", "+00:00")
    try:
        parsed = datetime.fromisoformat(normalized)
    except ValueError:
        return utcnow()

    if parsed.tzinfo is None:
        parsed = parsed.replace(tzinfo=timezone.utc)
    return parsed.astimezone(timezone.utc)


def ensure_directory(path: Path) -> Path:
    path.mkdir(parents=True, exist_ok=True)
    return path


def ensure_user_settings(db: Session, user_id: str) -> UserSettings:
    settings = db.get(UserSettings, user_id)
    if settings is None:
        settings = UserSettings(
            user_id=user_id,
            backend_mode=database_kind(),
        )
        db.add(settings)
        db.flush()
    return settings


def ensure_default_alert_rule(db: Session) -> AlertRule:
    rule = db.scalar(select(AlertRule).where(AlertRule.scope == DEFAULT_ALERT_RULE_SCOPE))
    if rule is None:
        rule = AlertRule(
            scope=DEFAULT_ALERT_RULE_SCOPE,
            temp_high=DEFAULT_TEMP_HIGH,
            temp_low=DEFAULT_TEMP_LOW,
            humidity_low=DEFAULT_HUMIDITY_LOW,
            soil_dry=DEFAULT_SOIL_DRY,
            soil_critical_dry=DEFAULT_SOIL_CRITICAL_DRY,
            soil_wet=DEFAULT_SOIL_WET,
            ph_low=DEFAULT_PH_LOW,
            ph_high=DEFAULT_PH_HIGH,
            cooldown_minutes=30,
            updated_at=utcnow(),
        )
        db.add(rule)
        db.flush()
    return rule


def _load_json_object(raw_value: str | None) -> dict[str, Any] | None:
    if not raw_value:
        return None
    try:
        parsed = json.loads(raw_value)
    except json.JSONDecodeError:
        return None
    return parsed if isinstance(parsed, dict) else None


def repair_data_consistency(db: Session) -> None:
    sensor_node_ids = [node_id for (node_id,) in db.execute(select(SensorNode.node_id)).all()]
    fallback_node_id = sensor_node_ids[0] if len(sensor_node_ids) == 1 else None

    analysis_jobs = db.scalars(select(AnalysisJob)).all()
    for job in analysis_jobs:
        if job.node_id:
            continue
        sensor_payload = _load_json_object(job.sensor_json)
        inferred_node_id = None
        if sensor_payload is not None:
            inferred_node_id = sensor_payload.get("nodeId") or sensor_payload.get("node_id")
        if inferred_node_id:
            job.node_id = str(inferred_node_id)
        elif fallback_node_id:
            job.node_id = fallback_node_id

    records = db.scalars(select(ActivityRecord)).all()
    for record in records:
        if record.record_type != "scan":
            continue

        treatment_payload = _load_json_object(record.treatment_json)
        if treatment_payload is None:
            scan_result = _load_json_object(record.scan_result_json) or {}
            raw_steps = scan_result.get("treatment")
            if isinstance(raw_steps, list):
                cleaned_steps = [
                    str(step).strip() for step in raw_steps if str(step).strip()
                ]
                if cleaned_steps:
                    treatment_payload = {"steps": cleaned_steps}
                    record.treatment_json = json.dumps(treatment_payload)

        if treatment_payload is None:
            continue

        treatment_steps = treatment_payload.get("steps")
        if not isinstance(treatment_steps, list):
            continue
        cleaned_steps = [
            str(step).strip() for step in treatment_steps if str(step).strip()
        ]
        if not cleaned_steps:
            continue

        existing_treatment = db.scalar(
            select(TreatmentRecord)
            .where(TreatmentRecord.record_id == record.id)
            .limit(1)
        )
        if existing_treatment is None:
            db.add(
                TreatmentRecord(
                    record_id=record.id,
                    title=f"Treatment plan for {record.title}",
                    description="\n".join(cleaned_steps),
                )
            )


def seed_default_data() -> None:
    with SessionLocal() as db:
        for seed in (
            {
                "email": "admin@pineguard.local",
                "password": "pineapple123",
                "display_name": "PineGuard Admin",
                "role": "admin",
                "farm_location": "Johor Demo HQ",
            },
            {
                "email": "officer@pineguard.local",
                "password": "pineapple123",
                "display_name": "Field Officer",
                "role": "officer",
                "farm_location": "Johor Demo Support",
            },
            {
                "email": "farmer@pineguard.local",
                "password": "pineapple123",
                "display_name": "Demo Farmer",
                "role": "farmer",
                "farm_location": "Single demonstration area",
            },
        ):
            user = db.scalar(select(User).where(User.email == seed["email"]))
            if user is None:
                user = User(
                    email=seed["email"],
                    password_hash=create_password_hash(seed["password"]),
                    display_name=seed["display_name"],
                    role=seed["role"],
                    farm_location=seed["farm_location"],
                    account_status="active",
                    created_at=utcnow(),
                    last_login=utcnow(),
                    last_active_at=utcnow(),
                )
                db.add(user)
                db.flush()
            else:
                user.password_hash = create_password_hash(seed["password"])
                user.display_name = seed["display_name"]
                user.role = seed["role"]
                user.farm_location = seed["farm_location"]
                user.account_status = "active"
            ensure_user_settings(db, user.id)

        ensure_default_alert_rule(db)

        node = db.get(SensorNode, "node_demo")
        if node is None:
            db.add(
                SensorNode(
                    node_id="node_demo",
                    device_name="Demo Zone - Pineapple Plant",
                    location="Single demonstration area",
                    firmware_version="demo",
                    status="offline",
                    last_heartbeat=utcnow(),
                )
            )
        repair_data_consistency(db)
        db.commit()


def get_db() -> Iterator[Session]:
    session = SessionLocal()
    try:
        yield session
    finally:
        session.close()


@contextmanager
def db_session() -> Iterator[Session]:
    session = SessionLocal()
    try:
        yield session
    finally:
        session.close()
