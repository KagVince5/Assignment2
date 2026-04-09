from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any


LABEL_ORDER = (
    "Healthy",
    "Needs_Attention",
    "Disease_Risk",
    "Urgent_Action",
)

DEFAULT_DATASET_ROOT = Path(__file__).resolve().parent / "datasets" / "classifier"
DEFAULT_MODEL_DIR = Path(__file__).resolve().parent / "models" / "classifier"
DEFAULT_IMAGE_SIZE = 224
DEFAULT_BATCH_SIZE = 16
DEFAULT_EPOCHS = 10
DEFAULT_SEED = 13


def _load_tensorflow() -> Any:
    try:
        import tensorflow as tf
    except Exception as exc:  # pragma: no cover - exercised in environments without TF
        raise SystemExit(f"TensorFlow is required for training: {exc}") from exc
    return tf


def build_datasets(
    tf: Any,
    dataset_root: Path,
    image_size: int = DEFAULT_IMAGE_SIZE,
    batch_size: int = DEFAULT_BATCH_SIZE,
    seed: int = DEFAULT_SEED,
):
    train_dir = dataset_root / "train"
    val_dir = dataset_root / "val"

    train_ds = tf.keras.utils.image_dataset_from_directory(
        train_dir,
        label_mode="int",
        class_names=list(LABEL_ORDER),
        image_size=(image_size, image_size),
        batch_size=batch_size,
        shuffle=True,
        seed=seed,
    )
    val_ds = tf.keras.utils.image_dataset_from_directory(
        val_dir,
        label_mode="int",
        class_names=list(LABEL_ORDER),
        image_size=(image_size, image_size),
        batch_size=batch_size,
        shuffle=False,
    )

    autotune = tf.data.AUTOTUNE
    return (
        train_ds.cache().prefetch(buffer_size=autotune),
        val_ds.cache().prefetch(buffer_size=autotune),
    )


def build_model(tf: Any, image_size: int = DEFAULT_IMAGE_SIZE) -> Any:
    inputs = tf.keras.Input(shape=(image_size, image_size, 3))
    x = tf.keras.applications.efficientnet.preprocess_input(inputs)
    base_model = tf.keras.applications.EfficientNetB0(
        include_top=False,
        weights="imagenet",
        input_tensor=x,
        pooling="avg",
    )
    base_model.trainable = False

    x = tf.keras.layers.Dropout(0.2)(base_model.output)
    outputs = tf.keras.layers.Dense(len(LABEL_ORDER), activation="softmax")(x)
    model = tf.keras.Model(inputs=inputs, outputs=outputs, name="pineapple_status_classifier")
    model.compile(
        optimizer=tf.keras.optimizers.Adam(),
        loss="sparse_categorical_crossentropy",
        metrics=["accuracy"],
    )
    return model


def save_label_map(output_dir: Path) -> Path:
    output_dir.mkdir(parents=True, exist_ok=True)
    label_map_path = output_dir / "label_map.json"
    label_map_path.write_text(
        json.dumps({"labels": list(LABEL_ORDER)}, indent=2),
        encoding="utf-8",
    )
    return label_map_path


def train_status_classifier(
    dataset_root: Path = DEFAULT_DATASET_ROOT,
    output_dir: Path = DEFAULT_MODEL_DIR,
    epochs: int = DEFAULT_EPOCHS,
    image_size: int = DEFAULT_IMAGE_SIZE,
    batch_size: int = DEFAULT_BATCH_SIZE,
    seed: int = DEFAULT_SEED,
) -> dict[str, str]:
    tf = _load_tensorflow()

    if not (dataset_root / "train").exists() or not (dataset_root / "val").exists():
        raise SystemExit(f"Prepared classifier dataset not found at: {dataset_root}")

    train_ds, val_ds = build_datasets(
        tf,
        dataset_root=dataset_root,
        image_size=image_size,
        batch_size=batch_size,
        seed=seed,
    )
    model = build_model(tf, image_size=image_size)
    history = model.fit(train_ds, validation_data=val_ds, epochs=epochs)

    output_dir.mkdir(parents=True, exist_ok=True)
    model_path = output_dir / "pineapple_status_classifier.keras"
    model.save(model_path)
    label_map_path = save_label_map(output_dir)

    history_path = output_dir / "training_history.json"
    history_path.write_text(
        json.dumps({key: [float(value) for value in values] for key, values in history.history.items()}, indent=2),
        encoding="utf-8",
    )

    return {
        "model_path": str(model_path),
        "label_map_path": str(label_map_path),
        "history_path": str(history_path),
    }


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Train the local TensorFlow pineapple status classifier.")
    parser.add_argument("--dataset-root", type=Path, default=DEFAULT_DATASET_ROOT, help="Prepared classifier dataset root")
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_MODEL_DIR, help="Directory for the trained model and label map")
    parser.add_argument("--epochs", type=int, default=DEFAULT_EPOCHS, help="Training epochs")
    parser.add_argument("--image-size", type=int, default=DEFAULT_IMAGE_SIZE, help="Input image size")
    parser.add_argument("--batch-size", type=int, default=DEFAULT_BATCH_SIZE, help="Training batch size")
    parser.add_argument("--seed", type=int, default=DEFAULT_SEED, help="Deterministic seed")
    return parser


def main(argv: list[str] | None = None) -> None:
    args = build_arg_parser().parse_args(argv)
    train_status_classifier(
        dataset_root=args.dataset_root,
        output_dir=args.output_dir,
        epochs=args.epochs,
        image_size=args.image_size,
        batch_size=args.batch_size,
        seed=args.seed,
    )


if __name__ == "__main__":
    main()
