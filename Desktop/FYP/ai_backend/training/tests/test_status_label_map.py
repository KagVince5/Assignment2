from __future__ import annotations

import unittest

from ai_backend.training.train_status_classifier_tf import LABEL_ORDER


class StatusLabelMapTests(unittest.TestCase):
    def test_label_order_matches_training_contract(self) -> None:
        self.assertEqual(
            LABEL_ORDER,
            (
                "Healthy",
                "Needs_Attention",
                "Disease_Risk",
                "Urgent_Action",
            ),
        )


if __name__ == "__main__":
    unittest.main()
