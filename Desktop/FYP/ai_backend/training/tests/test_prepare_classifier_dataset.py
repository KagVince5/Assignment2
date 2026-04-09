from __future__ import annotations

import csv
import json
import tempfile
from pathlib import Path
import unittest

from ai_backend.training.prepare_source_manifest import MANIFEST_FIELDS, normalize_source_manifest
from ai_backend.training.prepare_classifier_dataset import (
    CLASS_LABELS,
    map_raw_label_to_status,
    prepare_classifier_dataset,
    split_rows,
)


class PrepareClassifierDatasetTests(unittest.TestCase):
    def test_map_raw_label_to_status(self) -> None:
        self.assertEqual(map_raw_label_to_status("healthy"), "Healthy")
        self.assertEqual(map_raw_label_to_status("water_stress"), "Needs_Attention")
        self.assertEqual(map_raw_label_to_status("nutrient_deficiency"), "Needs_Attention")
        self.assertEqual(map_raw_label_to_status("mealybug_wilt"), "Disease_Risk")
        self.assertEqual(map_raw_label_to_status("fruit_rot"), "Disease_Risk")
        self.assertEqual(map_raw_label_to_status("heart_rot"), "Urgent_Action")

    def test_split_rows_is_deterministic(self) -> None:
        rows = [{"local_path": f"image_{index}.jpg", "raw_label": "healthy"} for index in range(12)]
        first = split_rows(rows, seed=42)
        second = split_rows(rows, seed=42)
        self.assertEqual(first, second)

    def test_prepare_classifier_dataset_copies_expected_files(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            source_dir = root / "source"
            dataset_dir = root / "dataset"
            source_dir.mkdir()

            rows = [
                ("healthy_1.jpg", "healthy"),
                ("stress_1.jpg", "water_stress"),
                ("disease_1.jpg", "fruit_rot"),
                ("urgent_1.jpg", "heart_rot"),
            ]

            manifest_path = root / "classifier_manifest.csv"
            with manifest_path.open("w", newline="", encoding="utf-8") as handle:
                writer = csv.DictWriter(handle, fieldnames=["local_path", "raw_label"])
                writer.writeheader()
                for filename, raw_label in rows:
                    image_path = source_dir / filename
                    image_path.write_bytes(b"fake-image")
                    writer.writerow({"local_path": str(image_path), "raw_label": raw_label})

            result = prepare_classifier_dataset(manifest_path, dataset_dir, seed=7)

            self.assertEqual(sorted(result["Healthy"]), ["healthy_1.jpg"])
            self.assertEqual(sorted(result["Needs_Attention"]), ["stress_1.jpg"])
            self.assertEqual(sorted(result["Disease_Risk"]), ["disease_1.jpg"])
            self.assertEqual(sorted(result["Urgent_Action"]), ["urgent_1.jpg"])

            for label in CLASS_LABELS:
                self.assertTrue((dataset_dir / "train" / label).exists())
                self.assertTrue((dataset_dir / "val" / label).exists())
                self.assertTrue((dataset_dir / "test" / label).exists())

    def test_normalize_source_manifest_supports_aliases_and_fields(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            input_path = root / "source_manifest.json"
            output_path = root / "source_manifest.csv"
            image_path = root / "pineapple_01.jpg"
            image_path.write_bytes(b"fake-image")

            input_path.write_text(
                json.dumps(
                    [
                        {
                            "source": "Roboflow Pineapple Set",
                            "url": "https://example.com/pineapple",
                            "licence": "CC BY 4.0",
                            "file_path": str(image_path),
                            "description": "healthy field image",
                        }
                    ]
                ),
                encoding="utf-8",
            )

            normalized = normalize_source_manifest(input_path, output_path)

            self.assertEqual(
                normalized,
                [
                    {
                        "source_name": "Roboflow Pineapple Set",
                        "source_url": "https://example.com/pineapple",
                        "license": "CC BY 4.0",
                        "local_path": str(image_path),
                        "notes": "healthy field image",
                    }
                ],
            )

            with output_path.open("r", encoding="utf-8", newline="") as handle:
                reader = csv.DictReader(handle)
                self.assertEqual(reader.fieldnames, MANIFEST_FIELDS)
                rows = list(reader)

            self.assertEqual(len(rows), 1)
            self.assertEqual(rows[0]["source_name"], "Roboflow Pineapple Set")
            self.assertEqual(rows[0]["source_url"], "https://example.com/pineapple")
            self.assertEqual(rows[0]["license"], "CC BY 4.0")
            self.assertEqual(rows[0]["local_path"], str(image_path))
            self.assertEqual(rows[0]["notes"], "healthy field image")


if __name__ == "__main__":
    unittest.main()
