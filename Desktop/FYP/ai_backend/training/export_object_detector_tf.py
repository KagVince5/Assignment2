from __future__ import annotations

import argparse
import json
import shutil
from pathlib import Path
from typing import Any

try:
    from ai_backend.training.train_object_detector_tf import DEFAULT_MODEL_DIR, DETECTOR_CLASS_ORDER, write_detector_label_map
except ModuleNotFoundError:  # pragma: no cover - direct script execution fallback
    from train_object_detector_tf import DEFAULT_MODEL_DIR, DETECTOR_CLASS_ORDER, write_detector_label_map

DEFAULT_EXPORT_DIR = DEFAULT_MODEL_DIR / "inference"


def _copy_source_artifact(source_path: Path | None, export_dir: Path) -> Path | None:
    if source_path is None:
        return None
    if not source_path.exists():
        raise SystemExit(f"Source detector artifact not found: {source_path}")

    target_path = export_dir / source_path.name
    if source_path.is_dir():
        if target_path.exists():
            shutil.rmtree(target_path)
        shutil.copytree(source_path, target_path)
    else:
        shutil.copy2(source_path, target_path)
    return target_path


def export_object_detector(
    source_path: Path | None = None,
    export_dir: Path = DEFAULT_EXPORT_DIR,
) -> dict[str, Any]:
    export_dir.mkdir(parents=True, exist_ok=True)
    label_map_path = write_detector_label_map(export_dir)
    copied_source = _copy_source_artifact(source_path, export_dir)

    manifest_path = export_dir / "detector_export_manifest.json"
    manifest_path.write_text(
        json.dumps(
            {
                "source_path": str(source_path) if source_path is not None else None,
                "copied_source_path": str(copied_source) if copied_source is not None else None,
                "export_dir": str(export_dir),
                "label_map_path": str(label_map_path),
                "classes": list(DETECTOR_CLASS_ORDER),
            },
            indent=2,
        ),
        encoding="utf-8",
    )

    return {
        "export_dir": str(export_dir),
        "label_map_path": str(label_map_path),
        "manifest_path": str(manifest_path),
        "copied_source_path": str(copied_source) if copied_source is not None else None,
    }


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Export the local TensorFlow pineapple detector scaffold.")
    parser.add_argument("--source-path", type=Path, default=None, help="Optional trained detector artifact to copy")
    parser.add_argument("--export-dir", type=Path, default=DEFAULT_EXPORT_DIR, help="Detector inference export directory")
    return parser


def main(argv: list[str] | None = None) -> None:
    args = build_arg_parser().parse_args(argv)
    export_object_detector(source_path=args.source_path, export_dir=args.export_dir)


if __name__ == "__main__":
    main()
