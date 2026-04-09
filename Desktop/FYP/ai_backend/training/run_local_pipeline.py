from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Any

if __package__ in {None, ""}:
    sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

try:
    from ai_backend.training.generate_tasks_with_gemma import (
        DEFAULT_GEMMA_MODEL,
        DEFAULT_OLLAMA_BASE_URL,
        DEFAULT_TIMEOUT_SECONDS,
        generate_tasks_from_fused_json,
    )
    from ai_backend.training.infer_pineapple_status import (
        DEFAULT_CLASSIFIER_MODEL_DIR,
        DEFAULT_EXPORT_DIR,
        DEFAULT_IMAGE_SIZE,
        DEFAULT_OUTPUT_DIR,
        infer_pineapple_status,
    )
except ModuleNotFoundError:  # pragma: no cover - direct script execution fallback
    from generate_tasks_with_gemma import (
        DEFAULT_GEMMA_MODEL,
        DEFAULT_OLLAMA_BASE_URL,
        DEFAULT_TIMEOUT_SECONDS,
        generate_tasks_from_fused_json,
    )
    from infer_pineapple_status import (
        DEFAULT_CLASSIFIER_MODEL_DIR,
        DEFAULT_EXPORT_DIR,
        DEFAULT_IMAGE_SIZE,
        DEFAULT_OUTPUT_DIR,
        infer_pineapple_status,
    )


def run_local_pipeline(
    *,
    image_path: Path,
    classifier_model_dir: Path = DEFAULT_CLASSIFIER_MODEL_DIR,
    detector_export_dir: Path = DEFAULT_EXPORT_DIR,
    output_dir: Path = DEFAULT_OUTPUT_DIR,
    image_size: int = DEFAULT_IMAGE_SIZE,
    base_url: str = DEFAULT_OLLAMA_BASE_URL,
    model: str = DEFAULT_GEMMA_MODEL,
    timeout_seconds: float = DEFAULT_TIMEOUT_SECONDS,
) -> dict[str, Any]:
    fused_result = infer_pineapple_status(
        image_path=image_path,
        classifier_model_dir=classifier_model_dir,
        detector_export_dir=detector_export_dir,
        output_dir=output_dir,
        image_size=image_size,
    )
    fused_prediction_path = Path(str(fused_result["output_path"]))

    task_result = generate_tasks_from_fused_json(
        fused_prediction_path,
        output_dir=output_dir,
        base_url=base_url,
        model=model,
        timeout_seconds=timeout_seconds,
    )

    result = {
        "image_path": str(image_path),
        "fused_prediction_path": str(fused_prediction_path),
        "task_output_path": str(task_result["output_path"]),
        "fused": fused_result.get("fused"),
        "task": task_result.get("task"),
    }
    return result


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Run the local pineapple detector, classifier, fusion, and Gemma task pipeline."
    )
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
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR, help="Directory for JSON outputs")
    parser.add_argument("--image-size", type=int, default=DEFAULT_IMAGE_SIZE, help="Input image size")
    parser.add_argument("--base-url", type=str, default=DEFAULT_OLLAMA_BASE_URL, help="Local Ollama base URL")
    parser.add_argument("--model", type=str, default=DEFAULT_GEMMA_MODEL, help="Local Gemma/Ollama model name")
    parser.add_argument("--timeout-seconds", type=float, default=DEFAULT_TIMEOUT_SECONDS, help="Request timeout")
    return parser


def main(argv: list[str] | None = None) -> None:
    args = build_arg_parser().parse_args(argv)
    result = run_local_pipeline(
        image_path=args.image_path,
        classifier_model_dir=args.classifier_model_dir,
        detector_export_dir=args.detector_export_dir,
        output_dir=args.output_dir,
        image_size=args.image_size,
        base_url=args.base_url,
        model=args.model,
        timeout_seconds=args.timeout_seconds,
    )
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
