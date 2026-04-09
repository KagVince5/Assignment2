from __future__ import annotations

import argparse
import csv
import hashlib
import random
import shutil
from collections import defaultdict
from pathlib import Path
from typing import Any


DETECTOR_SPLITS = ("train", "val", "test")
DEFAULT_SPLIT_RATIOS = (0.7, 0.15, 0.15)
REQUIRED_COLUMNS = ("image_path", "label_path", "label", "xmin", "ymin", "xmax", "ymax")


def validate_row(row: dict[str, Any]) -> dict[str, Any]:
    validated: dict[str, Any] = {}
    for key in REQUIRED_COLUMNS:
        value = row.get(key)
        if value is None or str(value).strip() == "":
            raise ValueError(f"Missing required value: {key}")
        validated[key] = str(value).strip()

    xmin = float(validated["xmin"])
    ymin = float(validated["ymin"])
    xmax = float(validated["xmax"])
    ymax = float(validated["ymax"])
    if xmax <= xmin or ymax <= ymin:
        raise ValueError("Invalid bounding box coordinates")

    validated["xmin"] = xmin
    validated["ymin"] = ymin
    validated["xmax"] = xmax
    validated["ymax"] = ymax
    return validated


def load_manifest(manifest_path: Path) -> list[dict[str, str]]:
    with manifest_path.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle)
        return [dict(row) for row in reader]


def _group_rows_by_image(rows: list[dict[str, Any]]) -> dict[tuple[str, str], list[dict[str, Any]]]:
    grouped: dict[tuple[str, str], list[dict[str, Any]]] = defaultdict(list)
    for row in rows:
        validated = validate_row(row)
        key = (validated["image_path"], validated["label_path"])
        grouped[key].append(validated)
    return grouped


def _stable_split_counts(size: int, ratios: tuple[float, float, float]) -> list[int]:
    raw_counts = [int(size * ratio) for ratio in ratios]
    remainder = size - sum(raw_counts)
    index = 0
    while remainder > 0:
        raw_counts[index % len(raw_counts)] += 1
        remainder -= 1
        index += 1
    return raw_counts


def split_rows(rows: list[dict[str, Any]], seed: int = 13, ratios: tuple[float, float, float] = DEFAULT_SPLIT_RATIOS) -> dict[str, list[dict[str, Any]]]:
    if len(ratios) != 3:
        raise ValueError("split ratios must contain exactly three values")

    rng = random.Random(seed)
    split_rows_map: dict[str, list[dict[str, Any]]] = {split: [] for split in DETECTOR_SPLITS}

    grouped = list(_group_rows_by_image(rows).values())
    rng.shuffle(grouped)
    counts = _stable_split_counts(len(grouped), ratios)
    start = 0
    for split_name, count in zip(DETECTOR_SPLITS, counts):
        for group in grouped[start : start + count]:
            split_rows_map[split_name].extend(group)
        start += count

    for split_name in DETECTOR_SPLITS:
        rng.shuffle(split_rows_map[split_name])

    return split_rows_map


def _safe_output_stem(source_path: Path) -> str:
    digest = hashlib.sha1(source_path.as_posix().encode("utf-8")).hexdigest()[:10]
    return f"{source_path.stem}__{digest}"


def prepare_detector_dataset(manifest_path: Path, dataset_root: Path, seed: int = 13) -> dict[str, dict[str, list[str]]]:
    rows = load_manifest(manifest_path)
    for split_name in DETECTOR_SPLITS:
        (dataset_root / "images" / split_name).mkdir(parents=True, exist_ok=True)
        (dataset_root / "labels" / split_name).mkdir(parents=True, exist_ok=True)

    split_rows_map = split_rows(rows, seed=seed)
    copied_images: dict[str, list[str]] = {split: [] for split in DETECTOR_SPLITS}
    copied_labels: dict[str, list[str]] = {split: [] for split in DETECTOR_SPLITS}

    for split_name, split_rows_list in split_rows_map.items():
        image_target_dir = dataset_root / "images" / split_name
        label_target_dir = dataset_root / "labels" / split_name
        copied_keys: set[tuple[str, str]] = set()

        for row in split_rows_list:
            image_path = Path(row["image_path"])
            label_path = Path(row["label_path"])
            key = (str(image_path), str(label_path))
            if key in copied_keys:
                continue
            if not image_path.exists():
                raise ValueError(f"Image does not exist: {image_path}")
            if not label_path.exists():
                raise ValueError(f"Label file does not exist: {label_path}")

            target_stem = _safe_output_stem(image_path)
            target_image = image_target_dir / f"{target_stem}{image_path.suffix or '.jpg'}"
            target_label = label_target_dir / f"{target_stem}.txt"
            shutil.copy2(image_path, target_image)
            shutil.copy2(label_path, target_label)
            copied_images[split_name].append(str(target_image))
            copied_labels[split_name].append(str(target_label))
            copied_keys.add(key)

    return {"images": copied_images, "labels": copied_labels}


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Prepare pineapple detector dataset splits from a manifest.")
    parser.add_argument("--manifest", required=True, help="CSV manifest with image_path, label_path, label, xmin, ymin, xmax, ymax")
    parser.add_argument("--dataset-root", required=True, help="Target dataset root directory")
    parser.add_argument("--seed", type=int, default=13, help="Deterministic split seed")
    return parser


def main() -> None:
    args = build_arg_parser().parse_args()
    prepare_detector_dataset(Path(args.manifest), Path(args.dataset_root), seed=args.seed)


if __name__ == "__main__":
    main()
