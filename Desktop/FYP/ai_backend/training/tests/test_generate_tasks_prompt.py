from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from ai_backend.training.generate_tasks_with_gemma import (
    build_arg_parser,
    build_task_prompt,
    generate_tasks_from_fused_json,
    parse_gemma_task_response,
)


class GenerateTasksWithGemmaTests(unittest.TestCase):
    def setUp(self) -> None:
        self.fused_payload = {
            "image_path": "training/outputs/predictions/sample.jpg",
            "classifier": {
                "status": "Disease_Risk",
                "confidence": 0.84,
                "probabilities": {
                    "Healthy": 0.03,
                    "Needs_Attention": 0.12,
                    "Disease_Risk": 0.84,
                    "Urgent_Action": 0.01,
                },
            },
            "detector_findings": [
                {"label": "leaf", "confidence": 0.93},
                {"label": "mealybug", "confidence": 0.77},
            ],
            "fused": {
                "classifier_status": "Disease_Risk",
                "classifier_confidence": 0.84,
                "detector_findings": [
                    {"label": "leaf", "confidence": 0.93},
                    {"label": "mealybug", "confidence": 0.77},
                ],
                "final_status": "Disease_Risk",
                "final_status_rank": 2,
                "signals": {"has_rot_region": False, "has_mealybug": True, "strong_classifier": True},
                "reasons": ["Detector found mealybug and the classifier was not already at a higher severity."],
                "structural_only": False,
            },
        }

    def test_prompt_includes_structured_status_confidence_and_findings(self) -> None:
        prompt = build_task_prompt(self.fused_payload)

        self.assertIn('"final_status": "Disease_Risk"', prompt)
        self.assertIn('"classifier_confidence": 0.84', prompt)
        self.assertIn('"label": "mealybug"', prompt)
        self.assertIn("Respond with JSON only", prompt)
        self.assertIn('"recommended_tasks"', prompt)
        self.assertNotIn("training/datasets", prompt.lower())
        self.assertNotIn("source_manifest.csv", prompt.lower())

    def test_parse_gemma_response_normalizes_output(self) -> None:
        parsed = parse_gemma_task_response(
            json.dumps(
                {
                    "summary": "Inspect the lower leaf bases and isolate the plant.",
                    "priority": "high",
                    "recommended_tasks": ["Inspect for pests", "Take another photo"],
                    "follow_up_hours": "24",
                    "status": "Disease_Risk",
                }
            )
        )

        self.assertEqual(parsed["summary"], "Inspect the lower leaf bases and isolate the plant.")
        self.assertEqual(parsed["priority"], "high")
        self.assertEqual(parsed["follow_up_hours"], 24)
        self.assertEqual(parsed["status"], "Disease_Risk")

    def test_generate_tasks_writes_json_output(self) -> None:
        with tempfile.TemporaryDirectory() as tmp_dir:
            fused_path = Path(tmp_dir) / "sample.fused.json"
            fused_path.write_text(json.dumps(self.fused_payload), encoding="utf-8")

            with patch(
                "ai_backend.training.generate_tasks_with_gemma.call_local_gemma",
                return_value={
                    "summary": "The plant shows disease risk and should be checked today.",
                    "priority": "high",
                    "recommended_tasks": [
                        "Inspect leaf bases for mealybugs.",
                        "Separate the plant from healthy rows.",
                    ],
                    "follow_up_hours": 24,
                    "status": "Disease_Risk",
                    "raw": {},
                },
            ):
                result = generate_tasks_from_fused_json(fused_path, output_dir=Path(tmp_dir) / "outputs")

            output_path = Path(result["output_path"])
            self.assertTrue(output_path.exists())
            written = json.loads(output_path.read_text(encoding="utf-8"))
            self.assertEqual(written["task"]["status"], "Disease_Risk")
            self.assertEqual(written["task"]["priority"], "high")
            self.assertIn("Inspect leaf bases", written["task"]["recommended_tasks"][0])

    def test_parser_accepts_required_cli_arguments(self) -> None:
        parser = build_arg_parser()
        args = parser.parse_args(["--fused-json-path", "sample.json"])

        self.assertEqual(args.fused_json_path, Path("sample.json"))


if __name__ == "__main__":
    unittest.main()
