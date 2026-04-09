from __future__ import annotations

try:
    from ai_backend.training.train_status_classifier_tf import main
except ModuleNotFoundError:  # pragma: no cover - direct script execution fallback
    from train_status_classifier_tf import main


if __name__ == "__main__":
    main()
