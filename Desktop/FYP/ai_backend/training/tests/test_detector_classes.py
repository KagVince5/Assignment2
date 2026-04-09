from __future__ import annotations

import unittest

from ai_backend.training.train_object_detector_tf import DETECTOR_CLASS_ORDER


class DetectorClassOrderTests(unittest.TestCase):
    def test_label_order_matches_training_contract(self) -> None:
        self.assertEqual(
            DETECTOR_CLASS_ORDER,
            (
                "leaf",
                "fruit",
                "crown",
                "mealybug",
                "rot_region",
                "discoloration_region",
            ),
        )


if __name__ == "__main__":
    unittest.main()
