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
    _safe_output_name,
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
            source_dir_a = root / "source_a"
            source_dir_b = root / "source_b"
            dataset_dir = root / "dataset"
            source_dir_a.mkdir()
            source_dir_b.mkdir()

            rows = [
                (source_dir_a / "healthy_1.jpg", "healthy"),
                (source_dir_b / "healthy_1.jpg", "healthy"),
                (source_dir_a / "stress_1.jpg", "water_stress"),
                (source_dir_a / "disease_1.jpg", "fruit_rot"),
                (source_dir_a / "urgent_1.jpg", "heart_rot"),
            ]

            manifest_path = root / "classifier_manifest.csv"
            with manifest_path.open("w", newline="", encoding="utf-8") as handle:
                writer = csv.DictWriter(handle, fieldnames=["local_path", "raw_label"])
                writer.writeheader()
                for image_path, raw_label in rows:
                    image_path.write_bytes(b"fake-image")
                    writer.writerow({"local_path": str(image_path), "raw_label": raw_label})

            result = prepare_classifier_dataset(manifest_path, dataset_dir, seed=7)

            self.assertEqual(
                {Path(path).name for path in result["Healthy"]},
                {
                    _safe_output_name(source_dir_a / "healthy_1.jpg"),
                    _safe_output_name(source_dir_b / "healthy_1.jpg"),
                },
            )
            self.assertEqual(
                {Path(path).name for path in result["Needs_Attention"]},
                {_safe_output_name(source_dir_a / "stress_1.jpg")},
            )
            self.assertEqual(
                {Path(path).name for path in result["Disease_Risk"]},
                {_safe_output_name(source_dir_a / "disease_1.jpg")},
            )
            self.assertEqual(
                {Path(path).name for path in result["Urgent_Action"]},
                {_safe_output_name(source_dir_a / "urgent_1.jpg")},
            )

            for label in CLASS_LABELS:
                self.assertTrue((dataset_dir / "train" / label).exists())
                self.assertTrue((dataset_dir / "val" / label).exists())
                self.assertTrue((dataset_dir / "test" / label).exists())

    def test_normalize_source_manifest_supports_aliases_and_optional_blanks(self) -> None:
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
                            "url": "",
                            "licence": "",
                            "file_path": str(image_path),
                            "description": "",
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
                        "source_url": "",
                        "license": "",
                        "local_path": str(image_path),
                        "notes": "",
                    }
                ],
            )

            with output_path.open("r", encoding="utf-8", newline="") as handle:
                reader = csv.DictReader(handle)
                self.assertEqual(reader.fieldnames, MANIFEST_FIELDS)
                rows = list(reader)

            self.assertEqual(len(rows), 1)
            self.assertEqual(rows[0]["source_name"], "Roboflow Pineapple Set")
            self.assertEqual(rows[0]["source_url"], "")
            self.assertEqual(rows[0]["license"], "")
            self.assertEqual(rows[0]["local_path"], str(image_path))
            self.assertEqual(rows[0]["notes"], "")

    def test_normalize_source_manifest_rejects_malformed_csv_headers(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            input_path = root / "source_manifest.csv"
            output_path = root / "source_manifest_out.csv"
            input_path.write_text("source_url,license\nhttps://example.com,pineapple\n", encoding="utf-8")

            with self.assertRaises(ValueError):
                normalize_source_manifest(input_path, output_path)

    def test_normalize_source_manifest_rejects_conflicting_alias_values(self) -> None:
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
                            "source_name": "Roboflow Pineapple Set",
                            "source": "Different Source Name",
                            "url": "https://example.com/pineapple",
                            "license": "CC BY 4.0",
                            "local_path": str(image_path),
                            "notes": "healthy field image",
                        }
                    ]
                ),
                encoding="utf-8",
            )

            with self.assertRaises(ValueError):
                normalize_source_manifest(input_path, output_path)


if __name__ == "__main__":
    unittest.main()
