from __future__ import annotations

import csv
import tempfile
from pathlib import Path
import unittest

from ai_backend.training.prepare_source_manifest import normalize_record
from ai_backend.training.prepare_detector_dataset import (
    DETECTOR_SPLITS,
    _safe_output_stem,
    prepare_detector_dataset,
    split_rows,
    validate_row,
)


class PrepareDetectorDatasetTests(unittest.TestCase):
    def test_validate_row_rejects_missing_required_values(self) -> None:
        with self.assertRaises(ValueError):
            validate_row(
                {
                    "image_path": "",
                    "label_path": "labels/sample.txt",
                    "label": "leaf",
                    "xmin": "1",
                    "ymin": "2",
                    "xmax": "3",
                    "ymax": "4",
                }
            )

    def test_validate_row_rejects_missing_box_coordinates(self) -> None:
        with self.assertRaises(ValueError):
            validate_row(
                {
                    "image_path": "images/sample.jpg",
                    "label_path": "labels/sample.txt",
                    "label": "leaf",
                    "xmin": "",
                    "ymin": "2",
                    "xmax": "3",
                    "ymax": "4",
                }
            )

    def test_split_rows_is_deterministic(self) -> None:
        rows = [
            {
                "image_path": f"image_{index}.jpg",
                "label_path": f"label_{index}.txt",
                "label": "leaf",
                "xmin": "1",
                "ymin": "2",
                "xmax": "3",
                "ymax": "4",
            }
            for index in range(12)
        ]
        first = split_rows(rows, seed=99)
        second = split_rows(rows, seed=99)
        self.assertEqual(first, second)

    def test_prepare_detector_dataset_copies_images_and_labels(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            source_images_a = root / "source_images_a"
            source_images_b = root / "source_images_b"
            source_labels_a = root / "source_labels_a"
            source_labels_b = root / "source_labels_b"
            dataset_dir = root / "dataset"
            source_images_a.mkdir()
            source_images_b.mkdir()
            source_labels_a.mkdir()
            source_labels_b.mkdir()

            manifest_path = root / "detector_manifest.csv"
            with manifest_path.open("w", newline="", encoding="utf-8") as handle:
                writer = csv.DictWriter(
                    handle,
                    fieldnames=["image_path", "label_path", "label", "xmin", "ymin", "xmax", "ymax"],
                )
                writer.writeheader()
                image_path_a = source_images_a / "shared.jpg"
                label_path_a = source_labels_a / "shared.txt"
                image_path_b = source_images_b / "shared.jpg"
                label_path_b = source_labels_b / "shared.txt"
                image_path_a.write_bytes(b"fake-image-a")
                image_path_b.write_bytes(b"fake-image-b")
                label_path_a.write_text("0 0.1 0.2 0.3 0.4\n", encoding="utf-8")
                label_path_b.write_text("0 0.5 0.6 0.7 0.8\n", encoding="utf-8")

                writer.writerow(
                    {
                        "image_path": str(image_path_a),
                        "label_path": str(label_path_a),
                        "label": "leaf",
                        "xmin": "1",
                        "ymin": "2",
                        "xmax": "3",
                        "ymax": "4",
                    }
                )
                writer.writerow(
                    {
                        "image_path": str(image_path_a),
                        "label_path": str(label_path_a),
                        "label": "leaf",
                        "xmin": "5",
                        "ymin": "6",
                        "xmax": "7",
                        "ymax": "8",
                    }
                )
                writer.writerow(
                    {
                        "image_path": str(image_path_b),
                        "label_path": str(label_path_b),
                        "label": "leaf",
                        "xmin": "1",
                        "ymin": "2",
                        "xmax": "3",
                        "ymax": "4",
                    }
                )

            result = prepare_detector_dataset(manifest_path, dataset_dir, seed=5)

            self.assertEqual(sum(len(paths) for paths in result["images"].values()), 2)
            self.assertEqual(sum(len(paths) for paths in result["labels"].values()), 2)
            for split, paths in result["images"].items():
                for copied_path in paths:
                    self.assertTrue(copied_path.startswith(str(dataset_dir / "images" / split)))
                    self.assertIn("__", Path(copied_path).name)
                    self.assertTrue(Path(copied_path).name.endswith(".jpg"))
            for split, paths in result["labels"].items():
                for copied_path in paths:
                    self.assertTrue(copied_path.startswith(str(dataset_dir / "labels" / split)))
                    self.assertIn("__", Path(copied_path).name)
                    self.assertTrue(Path(copied_path).name.endswith(".txt"))

            for split in DETECTOR_SPLITS:
                self.assertTrue((dataset_dir / "images" / split).exists())
                self.assertTrue((dataset_dir / "labels" / split).exists())

    def test_split_rows_keeps_same_image_rows_in_one_split(self) -> None:
        rows = [
            {
                "image_path": "image_a.jpg",
                "label_path": "image_a.txt",
                "label": "leaf",
                "xmin": "1",
                "ymin": "2",
                "xmax": "3",
                "ymax": "4",
            },
            {
                "image_path": "image_a.jpg",
                "label_path": "image_a.txt",
                "label": "leaf",
                "xmin": "5",
                "ymin": "6",
                "xmax": "7",
                "ymax": "8",
            },
            {
                "image_path": "image_b.jpg",
                "label_path": "image_b.txt",
                "label": "leaf",
                "xmin": "1",
                "ymin": "2",
                "xmax": "3",
                "ymax": "4",
            },
        ]

        split_map = split_rows(rows, seed=17)
        image_splits: dict[str, set[str]] = {}
        for split_name, split_rows_list in split_map.items():
            for row in split_rows_list:
                image_splits.setdefault(row["image_path"], set()).add(split_name)

        self.assertEqual(len(image_splits["image_a.jpg"]), 1)
        self.assertEqual(len(image_splits["image_b.jpg"]), 1)

    def test_source_manifest_validation_requires_only_core_fields(self) -> None:
        record = {
            "source_name": "Roboflow Pineapple Set",
            "source_url": "",
            "license": "",
            "local_path": "pineapple_01.jpg",
            "notes": "",
        }

        normalized = normalize_record(record)
        self.assertEqual(normalized["source_name"], "Roboflow Pineapple Set")
        self.assertEqual(normalized["source_url"], "")
        self.assertEqual(normalized["license"], "")
        self.assertEqual(normalized["local_path"], "pineapple_01.jpg")
        self.assertEqual(normalized["notes"], "")

        for field in ("source_name", "local_path"):
            invalid = dict(record)
            invalid[field] = ""
            with self.subTest(field=field):
                with self.assertRaises(ValueError):
                    normalize_record(invalid)


if __name__ == "__main__":
    unittest.main()
