from __future__ import annotations

from dataclasses import dataclass
from typing import Any


STATUS_RANK = {
    "Healthy": 0,
    "Needs_Attention": 1,
    "Disease_Risk": 2,
    "Urgent_Action": 3,
}

STRUCTURAL_LABELS = {"leaf", "fruit", "crown"}
RISK_LABELS = {"mealybug", "rot_region", "discoloration_region"}
ROT_CONFIDENCE_THRESHOLD = 0.7
MEALYBUG_CONFIDENCE_THRESHOLD = 0.7
STRONG_CLASSIFIER_CONFIDENCE = 0.8


@dataclass(frozen=True)
class DetectorFinding:
    label: str
    confidence: float


def _rank(status: str) -> int:
    return STATUS_RANK.get(status, STATUS_RANK["Healthy"])


def _upgrade(status: str, target: str) -> str:
    return target if _rank(target) > _rank(status) else status


def _normalize_finding(finding: Any) -> DetectorFinding:
    if isinstance(finding, DetectorFinding):
        return finding
    if not isinstance(finding, dict):
        raise TypeError("Detector findings must be mappings with label and confidence")
    label = str(finding.get("label", "")).strip()
    confidence = float(finding.get("confidence", 0.0))
    if not label:
        raise ValueError("Detector finding is missing a label")
    return DetectorFinding(label=label, confidence=confidence)


def fuse_predictions(
    classifier_status: str,
    classifier_confidence: float,
    detector_findings: list[Any] | None = None,
) -> dict[str, Any]:
    findings = [_normalize_finding(finding) for finding in detector_findings or []]
    labels = {finding.label for finding in findings}
    structural_only = bool(findings) and labels.issubset(STRUCTURAL_LABELS)

    has_rot = any(finding.label == "rot_region" and finding.confidence >= ROT_CONFIDENCE_THRESHOLD for finding in findings)
    has_mealybug = any(
        finding.label == "mealybug" and finding.confidence >= MEALYBUG_CONFIDENCE_THRESHOLD for finding in findings
    )

    final_status = classifier_status
    reasons: list[str] = []

    if has_rot and has_mealybug:
        final_status = "Urgent_Action"
        reasons.append("Detector found both rot_region and mealybug with high confidence.")
    elif has_rot:
        final_status = _upgrade(final_status, "Disease_Risk")
        reasons.append("Detector found rot_region with high confidence.")
    elif has_mealybug and final_status in {"Healthy", "Needs_Attention"}:
        final_status = "Disease_Risk"
        reasons.append("Detector found mealybug and the classifier was not already at a higher severity.")

    if structural_only and classifier_confidence >= STRONG_CLASSIFIER_CONFIDENCE and not reasons:
        reasons.append("Detector only found structural pineapple objects and classifier confidence was strong.")
    elif not reasons:
        reasons.append("Classifier result was retained because no detector evidence required escalation.")

    return {
        "classifier_status": classifier_status,
        "classifier_confidence": float(classifier_confidence),
        "detector_findings": [
            {"label": finding.label, "confidence": float(finding.confidence)} for finding in findings
        ],
        "final_status": final_status,
        "final_status_rank": _rank(final_status),
        "structural_only": structural_only,
        "signals": {
            "has_rot_region": has_rot,
            "has_mealybug": has_mealybug,
            "strong_classifier": classifier_confidence >= STRONG_CLASSIFIER_CONFIDENCE,
        },
        "reasons": reasons,
    }
