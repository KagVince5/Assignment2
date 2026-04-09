from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any


DETECTOR_CLASS_ORDER = (
    "leaf",
    "fruit",
    "crown",
    "mealybug",
    "rot_region",
    "discoloration_region",
)

DEFAULT_DATASET_ROOT = Path(__file__).resolve().parent / "datasets" / "detector"
DEFAULT_MODEL_DIR = Path(__file__).resolve().parent / "models" / "detector"
DEFAULT_IMAGE_SIZE = 640
DEFAULT_BATCH_SIZE = 8
DEFAULT_EPOCHS = 20
DEFAULT_SEED = 13
SUPPORTED_ANNOTATION_FORMATS = ("tfrecord", "csv")


def _load_tensorflow() -> Any:
    try:
        import tensorflow as tf
    except Exception as exc:  # pragma: no cover - exercised in environments without TF
        raise SystemExit(f"TensorFlow is required for detector training: {exc}") from exc
    return tf


def _annotation_path(dataset_root: Path, split: str, annotation_format: str) -> Path:
    return dataset_root / "annotations" / f"{split}.{annotation_format}"


def validate_detector_dataset(
    dataset_root: Path,
    annotation_format: str = "tfrecord",
) -> dict[str, str]:
    annotation_format = annotation_format.lower().strip()
    if annotation_format not in SUPPORTED_ANNOTATION_FORMATS:
        raise SystemExit(
            f"Unsupported annotation format: {annotation_format}. "
            f"Use one of: {', '.join(SUPPORTED_ANNOTATION_FORMATS)}"
        )

    required_dirs = [
        dataset_root / "images" / "train",
        dataset_root / "images" / "val",
        dataset_root / "labels" / "train",
        dataset_root / "labels" / "val",
    ]
    missing_dirs = [str(path) for path in required_dirs if not path.exists()]
    if missing_dirs:
        raise SystemExit(
            "Prepared detector dataset is missing required folders:\n" + "\n".join(f" - {path}" for path in missing_dirs)
        )

    required_annotations = [
        _annotation_path(dataset_root, "train", annotation_format),
        _annotation_path(dataset_root, "val", annotation_format),
    ]
    missing_annotations = [str(path) for path in required_annotations if not path.exists()]
    if missing_annotations:
        if annotation_format == "csv":
            raise SystemExit(
                "CSV annotations are accepted only as a conversion step. "
                "Convert them to TensorFlow TFRecord files before training."
            )
        raise SystemExit(
            "Prepared detector dataset is missing required TFRecord files:\n"
            + "\n".join(f" - {path}" for path in missing_annotations)
        )

    return {
        "annotation_format": annotation_format,
        "train_annotations": str(required_annotations[0]),
        "val_annotations": str(required_annotations[1]),
    }


def write_detector_label_map(output_dir: Path) -> Path:
    output_dir.mkdir(parents=True, exist_ok=True)
    label_map_path = output_dir / "label_map.json"
    label_map_path.write_text(
        json.dumps({"labels": list(DETECTOR_CLASS_ORDER)}, indent=2),
        encoding="utf-8",
    )
    return label_map_path


def write_detector_training_contract(
    output_dir: Path,
    dataset_root: Path,
    annotation_format: str,
    image_size: int,
    batch_size: int,
    epochs: int,
    seed: int,
) -> Path:
    output_dir.mkdir(parents=True, exist_ok=True)
    contract_path = output_dir / "training_contract.json"
    contract_path.write_text(
        json.dumps(
            {
                "dataset_root": str(dataset_root),
                "annotation_format": annotation_format,
                "image_size": image_size,
                "batch_size": batch_size,
                "epochs": epochs,
                "seed": seed,
                "classes": list(DETECTOR_CLASS_ORDER),
            },
            indent=2,
        ),
        encoding="utf-8",
    )
    return contract_path


def train_object_detector(
    dataset_root: Path = DEFAULT_DATASET_ROOT,
    output_dir: Path = DEFAULT_MODEL_DIR,
    annotation_format: str = "tfrecord",
    epochs: int = DEFAULT_EPOCHS,
    image_size: int = DEFAULT_IMAGE_SIZE,
    batch_size: int = DEFAULT_BATCH_SIZE,
    seed: int = DEFAULT_SEED,
) -> dict[str, str]:
    tf = _load_tensorflow()
    dataset_contract = validate_detector_dataset(dataset_root, annotation_format=annotation_format)

    # The scaffold verifies the TensorFlow detector contract and persists the
    # label map / training metadata before a full training implementation lands.
    _ = tf
    label_map_path = write_detector_label_map(output_dir)
    contract_path = write_detector_training_contract(
        output_dir=output_dir,
        dataset_root=dataset_root,
        annotation_format=dataset_contract["annotation_format"],
        image_size=image_size,
        batch_size=batch_size,
        epochs=epochs,
        seed=seed,
    )

    return {
        "label_map_path": str(label_map_path),
        "training_contract_path": str(contract_path),
        "annotation_format": dataset_contract["annotation_format"],
    }


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Train the local TensorFlow pineapple detector scaffold.")
    parser.add_argument("--dataset-root", type=Path, default=DEFAULT_DATASET_ROOT, help="Prepared detector dataset root")
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_MODEL_DIR, help="Directory for detector artifacts")
    parser.add_argument(
        "--annotation-format",
        choices=SUPPORTED_ANNOTATION_FORMATS,
        default="tfrecord",
        help="Annotation format expected by the scaffold",
    )
    parser.add_argument("--epochs", type=int, default=DEFAULT_EPOCHS, help="Training epochs")
    parser.add_argument("--image-size", type=int, default=DEFAULT_IMAGE_SIZE, help="Input image size")
    parser.add_argument("--batch-size", type=int, default=DEFAULT_BATCH_SIZE, help="Training batch size")
    parser.add_argument("--seed", type=int, default=DEFAULT_SEED, help="Deterministic seed")
    return parser


def main(argv: list[str] | None = None) -> None:
    args = build_arg_parser().parse_args(argv)
    train_object_detector(
        dataset_root=args.dataset_root,
        output_dir=args.output_dir,
        annotation_format=args.annotation_format,
        epochs=args.epochs,
        image_size=args.image_size,
        batch_size=args.batch_size,
        seed=args.seed,
    )


if __name__ == "__main__":
    main()
