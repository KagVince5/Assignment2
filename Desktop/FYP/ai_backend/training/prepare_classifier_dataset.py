from __future__ import annotations

import argparse
import csv
import random
import shutil
from collections import defaultdict
from pathlib import Path
from typing import Any


CLASS_LABELS = ["Healthy", "Needs_Attention", "Disease_Risk", "Urgent_Action"]
RAW_LABEL_TO_STATUS = {
    "healthy": "Healthy",
    "water_stress": "Needs_Attention",
    "nutrient_deficiency": "Needs_Attention",
    "mealybug_wilt": "Disease_Risk",
    "fruit_rot": "Disease_Risk",
    "heart_rot": "Urgent_Action",
}
DEFAULT_SPLITS = ("train", "val", "test")
DEFAULT_SPLIT_RATIOS = (0.7, 0.15, 0.15)


def map_raw_label_to_status(raw_label: str) -> str:
    normalized = raw_label.strip().lower()
    if not normalized:
        raise ValueError("raw_label cannot be empty")
    try:
        return RAW_LABEL_TO_STATUS[normalized]
    except KeyError as exc:
        raise ValueError(f"Unsupported raw label: {raw_label!r}") from exc


def load_manifest(manifest_path: Path) -> list[dict[str, str]]:
    with manifest_path.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle)
        rows = [dict(row) for row in reader]
    return rows


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

    grouped: dict[str, list[dict[str, Any]]] = defaultdict(list)
    for row in rows:
        raw_label = row.get("raw_label") or row.get("label")
        if raw_label is None:
            raise ValueError("Each row must include raw_label or label")
        grouped[map_raw_label_to_status(str(raw_label))].append(row)

    rng = random.Random(seed)
    split_rows_map: dict[str, list[dict[str, Any]]] = {split: [] for split in DEFAULT_SPLITS}

    for status in CLASS_LABELS:
        items = list(grouped.get(status, []))
        rng.shuffle(items)
        counts = _stable_split_counts(len(items), ratios)
        start = 0
        for split_name, count in zip(DEFAULT_SPLITS, counts):
            split_rows_map[split_name].extend(items[start : start + count])
            start += count

    for split_name in DEFAULT_SPLITS:
        rng.shuffle(split_rows_map[split_name])

    return split_rows_map


def _require_value(row: dict[str, str], key: str) -> str:
    value = (row.get(key) or "").strip()
    if not value:
        raise ValueError(f"Missing required value: {key}")
    return value


def prepare_classifier_dataset(manifest_path: Path, dataset_root: Path, seed: int = 13) -> dict[str, list[str]]:
    rows = load_manifest(manifest_path)
    valid_rows: list[dict[str, str]] = []

    for split_name in DEFAULT_SPLITS:
        for label in CLASS_LABELS:
            (dataset_root / split_name / label).mkdir(parents=True, exist_ok=True)

    for row in rows:
        local_path = _require_value(row, "local_path")
        raw_label = _require_value(row, "raw_label")
        if not Path(local_path).exists():
            raise ValueError(f"Input image does not exist: {local_path}")
        valid_rows.append({"local_path": local_path, "raw_label": raw_label})

    split_rows_map = split_rows(valid_rows, seed=seed)
    copied_files: dict[str, list[str]] = {label: [] for label in CLASS_LABELS}

    for split_name, split_rows_list in split_rows_map.items():
        for row in split_rows_list:
            status = map_raw_label_to_status(row["raw_label"])
            source_path = Path(row["local_path"])
            target_dir = dataset_root / split_name / status
            target_dir.mkdir(parents=True, exist_ok=True)
            target_path = target_dir / source_path.name
            shutil.copy2(source_path, target_path)
            copied_files[status].append(source_path.name)

    return copied_files


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Prepare pineapple classifier dataset splits from a manifest.")
    parser.add_argument("--manifest", required=True, help="CSV manifest with local_path and raw_label columns")
    parser.add_argument("--dataset-root", required=True, help="Target dataset root directory")
    parser.add_argument("--seed", type=int, default=13, help="Deterministic split seed")
    return parser


def main() -> None:
    args = build_arg_parser().parse_args()
    prepare_classifier_dataset(Path(args.manifest), Path(args.dataset_root), seed=args.seed)


if __name__ == "__main__":
    main()
