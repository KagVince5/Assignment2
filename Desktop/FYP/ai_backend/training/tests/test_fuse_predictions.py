from __future__ import annotations

import unittest

from ai_backend.training.fuse_predictions import fuse_predictions


class FusePredictionsTests(unittest.TestCase):
    def test_rot_region_raises_past_healthy(self) -> None:
        result = fuse_predictions(
            classifier_status="Healthy",
            classifier_confidence=0.96,
            detector_findings=[{"label": "rot_region", "confidence": 0.91}],
        )
        self.assertEqual(result["final_status"], "Disease_Risk")

    def test_mealybug_and_needs_attention_raise_to_disease_risk(self) -> None:
        result = fuse_predictions(
            classifier_status="Needs_Attention",
            classifier_confidence=0.82,
            detector_findings=[{"label": "mealybug", "confidence": 0.88}],
        )
        self.assertEqual(result["final_status"], "Disease_Risk")

    def test_mealybug_and_rot_region_raise_to_urgent_action(self) -> None:
        result = fuse_predictions(
            classifier_status="Disease_Risk",
            classifier_confidence=0.77,
            detector_findings=[
                {"label": "mealybug", "confidence": 0.86},
                {"label": "rot_region", "confidence": 0.9},
            ],
        )
        self.assertEqual(result["final_status"], "Urgent_Action")

    def test_strong_classifier_with_structural_objects_stays_stable(self) -> None:
        result = fuse_predictions(
            classifier_status="Healthy",
            classifier_confidence=0.97,
            detector_findings=[
                {"label": "leaf", "confidence": 0.92},
                {"label": "crown", "confidence": 0.89},
            ],
        )
        self.assertEqual(result["final_status"], "Healthy")
        self.assertTrue(result["structural_only"])


if __name__ == "__main__":
    unittest.main()
