from __future__ import annotations

import argparse
import json
import os
import re
import sys
from pathlib import Path
from typing import Any

if __package__ in {None, ""}:
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

try:
    from ai_backend.training.fuse_predictions import STATUS_RANK
except ModuleNotFoundError:  # pragma: no cover - direct script execution fallback
    from fuse_predictions import STATUS_RANK

import requests


DEFAULT_OUTPUT_DIR = Path(__file__).resolve().parent / "outputs" / "predictions"
DEFAULT_OLLAMA_BASE_URL = os.getenv("OLLAMA_BASE_URL", "http://127.0.0.1:11434").rstrip("/")
DEFAULT_GEMMA_MODEL = os.getenv("GEMMA_MODEL", os.getenv("OLLAMA_GEMMA_MODEL", "gemma3:latest"))
DEFAULT_TIMEOUT_SECONDS = float(os.getenv("GEMMA_TIMEOUT_SECONDS", "45"))


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

    parsed = json.loads(match.group(0))
    if not isinstance(parsed, dict):
        raise ValueError("Model response JSON must be an object")
    return parsed


def _status_rank(status: str) -> int:
    return STATUS_RANK.get(status, STATUS_RANK["Healthy"])


def _normalize_status(status: Any) -> str:
    value = str(status or "").strip().lower().replace("-", "_").replace(" ", "_")
    mapping = {
        "healthy": "Healthy",
        "needs_attention": "Needs_Attention",
        "needsattention": "Needs_Attention",
        "disease_risk": "Disease_Risk",
        "diseaserisk": "Disease_Risk",
        "urgent_action": "Urgent_Action",
        "urgentaction": "Urgent_Action",
    }
    return mapping.get(value, "Needs_Attention")


def _normalize_detector_findings(detector_findings: list[dict[str, Any]] | None) -> list[dict[str, Any]]:
    normalized: list[dict[str, Any]] = []
    for finding in detector_findings or []:
        if not isinstance(finding, dict):
            continue
        label = str(finding.get("label", "")).strip()
        if not label:
            continue
        normalized.append(
            {
                "label": label,
                "confidence": float(finding.get("confidence", 0.0) or 0.0),
            }
        )
    return normalized


def _compact_fused_context(fused_payload: dict[str, Any]) -> dict[str, Any]:
    classifier = fused_payload.get("classifier") if isinstance(fused_payload.get("classifier"), dict) else {}
    fused = fused_payload.get("fused") if isinstance(fused_payload.get("fused"), dict) else fused_payload
    detector_findings = fused_payload.get("detector_findings")
    if not isinstance(detector_findings, list):
        detector_findings = fused.get("detector_findings") if isinstance(fused.get("detector_findings"), list) else []

    compact = {
        "image_path": str(fused_payload.get("image_path") or ""),
        "classifier_status": str(classifier.get("status") or fused.get("classifier_status") or ""),
        "classifier_confidence": float(classifier.get("confidence") or fused.get("classifier_confidence") or 0.0),
        "detector_findings": _normalize_detector_findings(detector_findings),
        "final_status": str(fused.get("final_status") or classifier.get("status") or ""),
        "final_status_rank": int(fused.get("final_status_rank") or _status_rank(str(fused.get("final_status") or classifier.get("status") or ""))),
        "signals": fused.get("signals") if isinstance(fused.get("signals"), dict) else {},
        "reasons": fused.get("reasons") if isinstance(fused.get("reasons"), list) else [],
        "structural_only": bool(fused.get("structural_only", False)),
    }
    return compact


def build_task_prompt(fused_payload: dict[str, Any]) -> str:
    compact = _compact_fused_context(fused_payload)
    compact_json = json.dumps(compact, indent=2, sort_keys=True)
    schema_json = json.dumps(
        {
            "summary": "short user-facing explanation",
            "priority": "low | medium | high",
            "recommended_tasks": ["action 1", "action 2", "action 3"],
            "follow_up_hours": 0,
            "status": "Healthy | Needs_Attention | Disease_Risk | Urgent_Action",
        },
        indent=2,
    )

    return (
        "You are a pineapple farm assistant.\n"
        "Use only the structured prediction context below.\n"
        "Do not reference raw dataset access, training data, or hidden image details.\n"
        "Turn the prediction into concise, practical actions for the user.\n"
        "Respond with JSON only and no markdown fences.\n\n"
        f"Structured prediction context:\n{compact_json}\n\n"
        f"Required JSON schema:\n{schema_json}\n"
    )


def parse_gemma_task_response(text: str) -> dict[str, Any]:
    parsed = extract_json_object(text)
    summary = str(parsed.get("summary") or "").strip()
    priority = str(parsed.get("priority") or "medium").strip().lower()
    recommended_tasks_raw = parsed.get("recommended_tasks")
    follow_up_hours = parsed.get("follow_up_hours")
    status = str(parsed.get("status") or "").strip()

    if not summary:
        raise ValueError("Gemma response is missing summary")
    if priority not in {"low", "medium", "high"}:
        priority = "medium"
    if not isinstance(recommended_tasks_raw, list):
        recommended_tasks_raw = []

    recommended_tasks = [str(item).strip() for item in recommended_tasks_raw if str(item).strip()]
    if not recommended_tasks:
        recommended_tasks = ["Review the pineapple plant again and confirm the next inspection time."]

    try:
        follow_up_hours_int = int(follow_up_hours)
    except (TypeError, ValueError):
        follow_up_hours_int = 24
    follow_up_hours_int = max(0, follow_up_hours_int)

    status = _normalize_status(status)

    return {
        "summary": summary,
        "priority": priority,
        "recommended_tasks": recommended_tasks[:5],
        "follow_up_hours": follow_up_hours_int,
        "status": status,
        "raw": parsed,
    }


def call_local_gemma(prompt: str, *, base_url: str = DEFAULT_OLLAMA_BASE_URL, model: str = DEFAULT_GEMMA_MODEL, timeout_seconds: float = DEFAULT_TIMEOUT_SECONDS) -> dict[str, Any]:
    payload = {
        "model": model,
        "prompt": prompt,
        "stream": False,
        "format": "json",
    }
    response = requests.post(f"{base_url}/api/generate", json=payload, timeout=timeout_seconds)
    response.raise_for_status()

    outer = response.json()
    inner_text = str(outer.get("response", ""))
    return parse_gemma_task_response(inner_text)


def _default_output_path(input_path: Path, output_dir: Path) -> Path:
    stem = input_path.stem
    if stem.endswith(".fused"):
        stem = stem[: -len(".fused")]
    return output_dir / f"{stem}.tasks.json"


def generate_tasks_from_fused_json(
    fused_json_path: Path,
    *,
    output_dir: Path = DEFAULT_OUTPUT_DIR,
    base_url: str = DEFAULT_OLLAMA_BASE_URL,
    model: str = DEFAULT_GEMMA_MODEL,
    timeout_seconds: float = DEFAULT_TIMEOUT_SECONDS,
) -> dict[str, Any]:
    if not fused_json_path.exists():
        raise SystemExit(f"Fused prediction JSON not found: {fused_json_path}")

    payload = json.loads(fused_json_path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise SystemExit("Fused prediction JSON must contain an object")

    prompt = build_task_prompt(payload)
    gemma_result = call_local_gemma(prompt, base_url=base_url, model=model, timeout_seconds=timeout_seconds)

    compact_context = _compact_fused_context(payload)
    output_dir.mkdir(parents=True, exist_ok=True)
    output_path = _default_output_path(fused_json_path, output_dir)
    output_payload = {
        "source_fused_json": str(fused_json_path),
        "model": model,
        "base_url": base_url,
        "prediction_context": compact_context,
        "prompt": prompt,
        "gemma": gemma_result,
        "task": {
            "status": gemma_result["status"],
            "priority": gemma_result["priority"],
            "summary": gemma_result["summary"],
            "recommended_tasks": gemma_result["recommended_tasks"],
            "follow_up_hours": gemma_result["follow_up_hours"],
        },
    }
    output_path.write_text(json.dumps(output_payload, indent=2), encoding="utf-8")
    return {"output_path": str(output_path), **output_payload}


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Generate pineapple tasks from fused prediction JSON with local Gemma.")
    parser.add_argument("--fused-json-path", type=Path, required=True, help="Fused prediction JSON from inference")
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR, help="Directory for generated task JSON")
    parser.add_argument("--base-url", type=str, default=DEFAULT_OLLAMA_BASE_URL, help="Local Ollama base URL")
    parser.add_argument("--model", type=str, default=DEFAULT_GEMMA_MODEL, help="Local Gemma/Ollama model name")
    parser.add_argument("--timeout-seconds", type=float, default=DEFAULT_TIMEOUT_SECONDS, help="Request timeout in seconds")
    return parser


def main(argv: list[str] | None = None) -> None:
    args = build_arg_parser().parse_args(argv)
    generate_tasks_from_fused_json(
        args.fused_json_path,
        output_dir=args.output_dir,
        base_url=args.base_url,
        model=args.model,
        timeout_seconds=args.timeout_seconds,
    )


if __name__ == "__main__":
    main()
