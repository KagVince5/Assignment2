from __future__ import annotations

import argparse
import csv
import json
from pathlib import Path
from typing import Any


MANIFEST_FIELDS = ["source_name", "source_url", "license", "local_path", "notes"]


def normalize_record(record: dict[str, Any]) -> dict[str, str]:
    normalized = {
        "source_name": str(record.get("source_name") or record.get("source") or record.get("name") or "").strip(),
        "source_url": str(record.get("source_url") or record.get("url") or "").strip(),
        "license": str(record.get("license") or record.get("licence") or "").strip(),
        "local_path": str(record.get("local_path") or record.get("path") or record.get("file_path") or "").strip(),
        "notes": str(record.get("notes") or record.get("note") or record.get("description") or "").strip(),
    }

    for field in MANIFEST_FIELDS:
        if not normalized[field]:
            raise ValueError(f"Missing required value: {field}")

    return normalized


def load_records(input_path: Path) -> list[dict[str, Any]]:
    suffix = input_path.suffix.lower()
    if suffix == ".json":
        data = json.loads(input_path.read_text(encoding="utf-8"))
        if isinstance(data, dict):
            if "items" in data and isinstance(data["items"], list):
                data = data["items"]
            else:
                data = [data]
        if not isinstance(data, list):
            raise ValueError("JSON source manifest input must be a list or object")
        return [dict(item) for item in data]

    with input_path.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle)
        return [dict(row) for row in reader]


def write_manifest(records: list[dict[str, Any]], output_path: Path) -> None:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with output_path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=MANIFEST_FIELDS)
        writer.writeheader()
        for record in records:
            writer.writerow(normalize_record(record))


def normalize_source_manifest(input_path: Path, output_path: Path) -> list[dict[str, str]]:
    records = load_records(input_path)
    normalized = [normalize_record(record) for record in records]
    write_manifest(normalized, output_path)
    return normalized


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Normalize pineapple image source metadata into source_manifest.csv.")
    parser.add_argument("--input", required=True, help="CSV or JSON source metadata file")
    parser.add_argument("--output", required=True, help="Path to source_manifest.csv")
    return parser


def main() -> None:
    args = build_arg_parser().parse_args()
    normalize_source_manifest(Path(args.input), Path(args.output))


if __name__ == "__main__":
    main()
