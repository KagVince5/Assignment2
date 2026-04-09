from __future__ import annotations

import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from ai_backend.training.run_local_pipeline import build_arg_parser, run_local_pipeline


class RunLocalPipelineTests(unittest.TestCase):
    def test_pipeline_returns_fused_and_task_outputs(self) -> None:
        with tempfile.TemporaryDirectory() as tmp_dir:
            image_path = Path(tmp_dir) / "sample.jpg"
            image_path.write_bytes(b"fake-image")

            with patch(
                "ai_backend.training.run_local_pipeline.infer_pineapple_status",
                return_value={
                    "output_path": str(Path(tmp_dir) / "outputs" / "sample.json"),
                    "fused": {"final_status": "Disease_Risk"},
                },
            ) as infer_mock, patch(
                "ai_backend.training.run_local_pipeline.generate_tasks_from_fused_json",
                return_value={
                    "output_path": str(Path(tmp_dir) / "outputs" / "sample.tasks.json"),
                    "task": {"status": "Disease_Risk", "priority": "high"},
                },
            ) as task_mock:
                result = run_local_pipeline(image_path=image_path, output_dir=Path(tmp_dir) / "outputs")

        infer_mock.assert_called_once()
        task_mock.assert_called_once()
        self.assertEqual(result["fused_prediction_path"], str(Path(tmp_dir) / "outputs" / "sample.json"))
        self.assertEqual(result["task_output_path"], str(Path(tmp_dir) / "outputs" / "sample.tasks.json"))
        self.assertIn("fused", result)
        self.assertIn("task", result)

    def test_parser_accepts_required_image_argument(self) -> None:
        parser = build_arg_parser()
        args = parser.parse_args(["--image-path", "sample.png"])

        self.assertEqual(args.image_path, Path("sample.png"))


if __name__ == "__main__":
    unittest.main()
