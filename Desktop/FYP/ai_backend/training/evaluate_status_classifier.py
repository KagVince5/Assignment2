from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any

try:
    from ai_backend.training.train_status_classifier_tf import DEFAULT_DATASET_ROOT, DEFAULT_MODEL_DIR, LABEL_ORDER
except ModuleNotFoundError:  # pragma: no cover - direct script execution fallback
    from train_status_classifier_tf import DEFAULT_DATASET_ROOT, DEFAULT_MODEL_DIR, LABEL_ORDER

DEFAULT_OUTPUT_DIR = Path(__file__).resolve().parent / "outputs" / "evaluation"


def _load_tensorflow() -> Any:
    try:
        import tensorflow as tf
    except Exception as exc:  # pragma: no cover - exercised in environments without TF
        raise SystemExit(f"TensorFlow is required for evaluation: {exc}") from exc
    return tf


def _evaluate_dataset(model: Any, dataset: Any) -> dict[str, Any]:
    from sklearn.metrics import classification_report, confusion_matrix

    probabilities = model.predict(dataset, verbose=0)
    y_true = []
    for _, labels in dataset.unbatch():
        y_true.append(int(labels.numpy()))
    y_pred = [int(index) for index in probabilities.argmax(axis=1)]

    report = classification_report(
        y_true,
        y_pred,
        labels=list(range(len(LABEL_ORDER))),
        target_names=list(LABEL_ORDER),
        output_dict=True,
        zero_division=0,
    )
    matrix = confusion_matrix(y_true, y_pred, labels=list(range(len(LABEL_ORDER))))

    per_class = {
        label: {
            "precision": float(report[label]["precision"]),
            "recall": float(report[label]["recall"]),
            "f1_score": float(report[label]["f1-score"]),
            "support": int(report[label]["support"]),
        }
        for label in LABEL_ORDER
    }

    return {
        "confusion_matrix": matrix.tolist(),
        "per_class": per_class,
        "accuracy": float(report["accuracy"]),
        "macro_avg": {
            "precision": float(report["macro avg"]["precision"]),
            "recall": float(report["macro avg"]["recall"]),
            "f1_score": float(report["macro avg"]["f1-score"]),
            "support": int(report["macro avg"]["support"]),
        },
        "weighted_avg": {
            "precision": float(report["weighted avg"]["precision"]),
            "recall": float(report["weighted avg"]["recall"]),
            "f1_score": float(report["weighted avg"]["f1-score"]),
            "support": int(report["weighted avg"]["support"]),
        },
    }


def evaluate_status_classifier(
    model_path: Path = DEFAULT_MODEL_DIR / "pineapple_status_classifier.keras",
    dataset_root: Path = DEFAULT_DATASET_ROOT,
    output_dir: Path = DEFAULT_OUTPUT_DIR,
    image_size: int = 224,
) -> dict[str, Any]:
    tf = _load_tensorflow()

    test_dir = dataset_root / "test"
    if not test_dir.exists():
        raise SystemExit(f"Prepared test dataset not found at: {test_dir}")

    dataset = tf.keras.utils.image_dataset_from_directory(
        test_dir,
        label_mode="int",
        class_names=list(LABEL_ORDER),
        image_size=(image_size, image_size),
        batch_size=32,
        shuffle=False,
    ).prefetch(buffer_size=tf.data.AUTOTUNE)

    model = tf.keras.models.load_model(model_path)
    metrics = _evaluate_dataset(model, dataset)

    output_dir.mkdir(parents=True, exist_ok=True)
    report_path = output_dir / "status_classifier_metrics.json"
    report_path.write_text(
        json.dumps(
            {
                "model_path": str(model_path),
                "dataset_root": str(dataset_root),
                "labels": list(LABEL_ORDER),
                **metrics,
            },
            indent=2,
        ),
        encoding="utf-8",
    )

    return {"metrics_path": str(report_path), **metrics}


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Evaluate the local TensorFlow pineapple status classifier.")
    parser.add_argument("--model-path", type=Path, default=DEFAULT_MODEL_DIR / "pineapple_status_classifier.keras", help="Path to the saved classifier model")
    parser.add_argument("--dataset-root", type=Path, default=DEFAULT_DATASET_ROOT, help="Prepared classifier dataset root")
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR, help="Directory for metrics output")
    parser.add_argument("--image-size", type=int, default=224, help="Input image size")
    return parser


def main(argv: list[str] | None = None) -> None:
    args = build_arg_parser().parse_args(argv)
    evaluate_status_classifier(
        model_path=args.model_path,
        dataset_root=args.dataset_root,
        output_dir=args.output_dir,
        image_size=args.image_size,
    )


if __name__ == "__main__":
    main()
