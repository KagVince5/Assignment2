from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Any

if __package__ in {None, ""}:
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from ai_backend.training.fuse_predictions import fuse_predictions
from ai_backend.training.export_object_detector_tf import DEFAULT_EXPORT_DIR
from ai_backend.training.train_object_detector_tf import DETECTOR_CLASS_ORDER
from ai_backend.training.train_status_classifier_tf import DEFAULT_MODEL_DIR as DEFAULT_CLASSIFIER_MODEL_DIR, LABEL_ORDER

DEFAULT_OUTPUT_DIR = Path(__file__).resolve().parent / "outputs" / "predictions"
DEFAULT_IMAGE_SIZE = 224


def _load_tensorflow() -> Any:
    try:
        import tensorflow as tf
    except Exception as exc:  # pragma: no cover - exercised in environments without TF
        raise SystemExit(f"TensorFlow is required for inference: {exc}") from exc
    return tf


def load_classifier_artifacts(model_dir: Path) -> tuple[Any, list[str]]:
    tf = _load_tensorflow()
    model_path = model_dir / "pineapple_status_classifier.keras"
    label_map_path = model_dir / "label_map.json"
    if not model_path.exists():
        raise SystemExit(f"Classifier model not found: {model_path}")
    if not label_map_path.exists():
        raise SystemExit(f"Classifier label map not found: {label_map_path}")

    model = tf.keras.models.load_model(model_path)
    payload = json.loads(label_map_path.read_text(encoding="utf-8"))
    labels = payload.get("labels")
    if labels != list(LABEL_ORDER):
        raise SystemExit(f"Classifier label map mismatch. Expected {list(LABEL_ORDER)} but found {labels}")
    return model, list(labels)


def load_detector_contract(export_dir: Path) -> dict[str, Any]:
    manifest_path = export_dir / "detector_export_manifest.json"
    label_map_path = export_dir / "label_map.json"
    if not manifest_path.exists():
        raise SystemExit(f"Detector export manifest not found: {manifest_path}")
    if not label_map_path.exists():
        raise SystemExit(f"Detector label map not found: {label_map_path}")

    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    label_map = json.loads(label_map_path.read_text(encoding="utf-8"))
    labels = label_map.get("labels")
    if labels != list(DETECTOR_CLASS_ORDER):
        raise SystemExit(f"Detector label map mismatch. Expected {list(DETECTOR_CLASS_ORDER)} but found {labels}")
    return {"manifest": manifest, "labels": list(labels)}


def _prepare_image(tf: Any, image_path: Path, image_size: int) -> Any:
    image = tf.keras.utils.load_img(image_path, target_size=(image_size, image_size))
    image_array = tf.keras.utils.img_to_array(image)
    return tf.expand_dims(image_array, axis=0)


def run_classifier_inference(
    image_path: Path,
    model_dir: Path = DEFAULT_CLASSIFIER_MODEL_DIR,
    image_size: int = DEFAULT_IMAGE_SIZE,
) -> dict[str, Any]:
    tf = _load_tensorflow()
    model, labels = load_classifier_artifacts(model_dir)
    image_batch = _prepare_image(tf, image_path, image_size)
    probabilities = model.predict(image_batch, verbose=0)[0]
    top_index = int(probabilities.argmax())
    return {
        "status": labels[top_index],
        "confidence": float(probabilities[top_index]),
        "probabilities": {label: float(probabilities[index]) for index, label in enumerate(labels)},
    }


def run_detector_inference(
    image_path: Path,
    export_dir: Path = DEFAULT_EXPORT_DIR,
    image_size: int = DEFAULT_IMAGE_SIZE,
) -> list[dict[str, Any]]:
    tf = _load_tensorflow()
    _ = load_detector_contract(export_dir)

    model_candidates = [
        export_dir / "saved_model",
        export_dir / "model.keras",
        export_dir / "pineapple_detector.keras",
    ]
    model_path = next((path for path in model_candidates if path.exists()), None)
    if model_path is None:
        raise SystemExit(
            "Detector model artifact not found. Expected one of: "
            + ", ".join(str(path) for path in model_candidates)
        )

    if model_path.is_dir():
        model = tf.keras.models.load_model(model_path)
    else:
        model = tf.keras.models.load_model(model_path)

    image_batch = _prepare_image(tf, image_path, image_size)
    raw_predictions = model.predict(image_batch, verbose=0)[0]
    if raw_predictions.ndim == 0:
        raw_predictions = [float(raw_predictions)]

    findings: list[dict[str, Any]] = []
    for index, confidence in enumerate(raw_predictions):
        if index >= len(DETECTOR_CLASS_ORDER):
            break
        if float(confidence) < 0.5:
            continue
        findings.append({"label": DETECTOR_CLASS_ORDER[index], "confidence": float(confidence)})
    return findings


def infer_pineapple_status(
    image_path: Path,
    classifier_model_dir: Path = DEFAULT_CLASSIFIER_MODEL_DIR,
    detector_export_dir: Path = DEFAULT_EXPORT_DIR,
    output_dir: Path = DEFAULT_OUTPUT_DIR,
    image_size: int = DEFAULT_IMAGE_SIZE,
) -> dict[str, Any]:
    if not image_path.exists():
        raise SystemExit(f"Input image not found: {image_path}")

    classifier_result = run_classifier_inference(image_path, model_dir=classifier_model_dir, image_size=image_size)
    detector_findings = run_detector_inference(image_path, export_dir=detector_export_dir, image_size=image_size)
    fused = fuse_predictions(
        classifier_status=classifier_result["status"],
        classifier_confidence=classifier_result["confidence"],
        detector_findings=detector_findings,
    )

    output_dir.mkdir(parents=True, exist_ok=True)
    output_path = output_dir / f"{image_path.stem}.json"
    payload = {
        "image_path": str(image_path),
        "classifier": classifier_result,
        "detector_findings": detector_findings,
        "fused": fused,
    }
    output_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    return {"output_path": str(output_path), **payload}


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Run local pineapple inference and rule-based fusion.")
    parser.add_argument("--image-path", type=Path, required=True, help="Input pineapple image")
    parser.add_argument(
        "--classifier-model-dir",
        type=Path,
        default=DEFAULT_CLASSIFIER_MODEL_DIR,
        help="Directory containing the saved classifier model and label map",
    )
    parser.add_argument(
        "--detector-export-dir",
        type=Path,
        default=DEFAULT_EXPORT_DIR,
        help="Directory containing detector export artifacts",
    )
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR, help="Directory for fused JSON output")
    parser.add_argument("--image-size", type=int, default=DEFAULT_IMAGE_SIZE, help="Input image size")
    return parser


def main(argv: list[str] | None = None) -> None:
    args = build_arg_parser().parse_args(argv)
    infer_pineapple_status(
        image_path=args.image_path,
        classifier_model_dir=args.classifier_model_dir,
        detector_export_dir=args.detector_export_dir,
        output_dir=args.output_dir,
        image_size=args.image_size,
    )


if __name__ == "__main__":
    main()
