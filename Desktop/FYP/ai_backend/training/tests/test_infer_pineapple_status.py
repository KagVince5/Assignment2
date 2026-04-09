from __future__ import annotations

import unittest
from pathlib import Path

from ai_backend.training.infer_pineapple_status import DEFAULT_OUTPUT_DIR, build_arg_parser


class InferPineappleStatusCliTests(unittest.TestCase):
    def test_parser_accepts_required_image_argument(self) -> None:
        parser = build_arg_parser()
        args = parser.parse_args(["--image-path", "sample.png"])

        self.assertEqual(args.image_path, Path("sample.png"))
        self.assertEqual(args.output_dir, DEFAULT_OUTPUT_DIR)


if __name__ == "__main__":
    unittest.main()
