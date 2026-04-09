from __future__ import annotations

import argparse
import csv
import json
from pathlib import Path
from typing import Any


MANIFEST_FIELDS = ["source_name", "source_url", "license", "local_path", "notes"]
REQUIRED_FIELDS = ("source_name", "local_path")
FIELD_ALIASES = {
    "source_name": ("source_name", "source", "name"),
    "source_url": ("source_url", "url"),
    "license": ("license", "licence"),
    "local_path": ("local_path", "path", "file_path"),
    "notes": ("notes", "note", "description"),
}


def _coalesce_field(record: dict[str, Any], field_name: str) -> str:
    aliases = FIELD_ALIASES[field_name]
    values = [str(record[key]).strip() for key in aliases if key in record and str(record[key]).strip()]
    if not values:
        return ""
    if len(set(values)) > 1:
        raise ValueError(f"Conflicting values for {field_name}")
    return values[0]


def normalize_record(record: dict[str, Any]) -> dict[str, str]:
    normalized = {field: _coalesce_field(record, field) for field in MANIFEST_FIELDS}

    for field in REQUIRED_FIELDS:
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
        if reader.fieldnames is None:
            raise ValueError("CSV source manifest input is missing a header row")

        header_names = {name.strip() for name in reader.fieldnames if name and name.strip()}
        if not header_names:
            raise ValueError("CSV source manifest input is missing a header row")

        for field_name in REQUIRED_FIELDS:
            aliases = FIELD_ALIASES[field_name]
            if not any(alias in header_names for alias in aliases):
                raise ValueError(f"CSV source manifest header is missing required field: {field_name}")

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
