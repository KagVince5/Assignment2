from __future__ import annotations

import csv
import tempfile
from pathlib import Path
import unittest

from ai_backend.training.prepare_detector_dataset import (
    DETECTOR_SPLITS,
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
            source_images = root / "source_images"
            source_labels = root / "source_labels"
            dataset_dir = root / "dataset"
            source_images.mkdir()
            source_labels.mkdir()

            manifest_path = root / "detector_manifest.csv"
            with manifest_path.open("w", newline="", encoding="utf-8") as handle:
                writer = csv.DictWriter(
                    handle,
                    fieldnames=["image_path", "label_path", "label", "xmin", "ymin", "xmax", "ymax"],
                )
                writer.writeheader()
                for index in range(6):
                    image_path = source_images / f"image_{index}.jpg"
                    label_path = source_labels / f"image_{index}.txt"
                    image_path.write_bytes(b"fake-image")
                    label_path.write_text("0 0.1 0.2 0.3 0.4\n", encoding="utf-8")
                    writer.writerow(
                        {
                            "image_path": str(image_path),
                            "label_path": str(label_path),
                            "label": "leaf",
                            "xmin": "1",
                            "ymin": "2",
                            "xmax": "3",
                            "ymax": "4",
                        }
                    )

            result = prepare_detector_dataset(manifest_path, dataset_dir, seed=5)

            self.assertEqual(sum(len(paths) for paths in result["images"].values()), 6)
            self.assertEqual(sum(len(paths) for paths in result["labels"].values()), 6)

            for split in DETECTOR_SPLITS:
                self.assertTrue((dataset_dir / "images" / split).exists())
                self.assertTrue((dataset_dir / "labels" / split).exists())


if __name__ == "__main__":
    unittest.main()
