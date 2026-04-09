# Pineapple Status Local AI Pipeline Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build a no-database-first local AI pipeline that trains a TensorFlow pineapple status classifier and TensorFlow-compatible detector, fuses their outputs, and generates actionable local Gemma task guidance from a single pineapple image.

**Architecture:** Add a dedicated file-based training workspace under `ai_backend/training`, keep detector and classifier datasets separate, train each model independently, then add a small local inference pipeline that fuses detector and classifier results before calling Gemma with structured JSON. Preserve the current backend until the new local AI path is verified.

**Tech Stack:** Python, TensorFlow, TensorFlow Object Detection API or TensorFlow-compatible detector export path, Pillow, CSV/JSON manifests, local Gemma/Ollama integration, `unittest`

---

### Task 1: Create the file-based training workspace

**Files:**
- Create: `ai_backend/training/datasets/.gitkeep`
- Create: `ai_backend/training/manifests/.gitkeep`
- Create: `ai_backend/training/models/.gitkeep`
- Create: `ai_backend/training/outputs/.gitkeep`
- Modify: `ai_backend/training/README.md`
- Create: `ai_backend/training/requirements-training.txt`

**Steps:**
1. Update `ai_backend/training/README.md` so it describes the new two-model TensorFlow pipeline, folder layout, and manifest-driven workflow instead of only the older YOLO classifier flow.
2. Add placeholder `.gitkeep` files so the new `datasets`, `manifests`, `models`, and `outputs` directories are tracked consistently.
3. Create `ai_backend/training/requirements-training.txt` for training-only dependencies such as TensorFlow, Pillow, pandas, scikit-learn, and any detector-specific TensorFlow tooling.
4. Run a quick tree check with `Get-ChildItem -Recurse ai_backend\training`.
5. Commit only the files created or modified in this task.

### Task 2: Add dataset manifest and split preparation utilities

**Files:**
- Create: `ai_backend/training/prepare_source_manifest.py`
- Create: `ai_backend/training/prepare_classifier_dataset.py`
- Create: `ai_backend/training/prepare_detector_dataset.py`
- Create: `ai_backend/training/tests/test_prepare_classifier_dataset.py`
- Create: `ai_backend/training/tests/test_prepare_detector_dataset.py`

**Steps:**
1. Write failing tests for manifest parsing, class mapping, deterministic split behavior, and invalid-row rejection in the two new test files.
2. Run `python -m unittest ai_backend.training.tests.test_prepare_classifier_dataset ai_backend.training.tests.test_prepare_detector_dataset` and confirm the tests fail because the scripts do not exist yet.
3. Implement `prepare_source_manifest.py` to normalize raw image metadata into `source_manifest.csv` with fields such as `source_name`, `source_url`, `license`, `local_path`, and `notes`.
4. Implement `prepare_classifier_dataset.py` to read a manifest, map raw labels into the four status classes, and copy images into `train/val/test/<class>` folders.
5. Implement `prepare_detector_dataset.py` to validate detector annotations and organize `images/` and `labels/` into `train/val/test` folders with matching basenames.
6. Re-run `python -m unittest ai_backend.training.tests.test_prepare_classifier_dataset ai_backend.training.tests.test_prepare_detector_dataset` and confirm the tests pass.
7. Commit only the manifest and dataset-prep files from this task.

### Task 3: Build the TensorFlow status classifier training path

**Files:**
- Create: `ai_backend/training/train_status_classifier_tf.py`
- Create: `ai_backend/training/evaluate_status_classifier.py`
- Create: `ai_backend/training/tests/test_status_label_map.py`
- Modify: `ai_backend/training/train_classifier.py`

**Steps:**
1. Write a failing test in `test_status_label_map.py` that verifies the classifier label order is exactly `Healthy`, `Needs_Attention`, `Disease_Risk`, `Urgent_Action`.
2. Run `python -m unittest ai_backend.training.tests.test_status_label_map` and confirm it fails before the new TensorFlow classifier path exists.
3. Implement `train_status_classifier_tf.py` using TensorFlow transfer learning with `EfficientNetB0` or `MobileNetV3`, reading the prepared classifier dataset from `ai_backend/training/datasets/classifier`.
4. Save the trained model and label map into `ai_backend/training/models/classifier/`.
5. Implement `evaluate_status_classifier.py` to write confusion matrix and per-class metrics into `ai_backend/training/outputs/evaluation/`.
6. Update `train_classifier.py` to become a thin compatibility wrapper that points developers to the new TensorFlow classifier entry point instead of silently preserving the older training behavior.
7. Re-run `python -m unittest ai_backend.training.tests.test_status_label_map` and then run a smoke command such as `python ai_backend/training/train_status_classifier_tf.py --help`.
8. Commit only the classifier training and evaluation files from this task.

### Task 4: Build the TensorFlow detector training scaffold

**Files:**
- Create: `ai_backend/training/train_object_detector_tf.py`
- Create: `ai_backend/training/export_object_detector_tf.py`
- Create: `ai_backend/training/tests/test_detector_classes.py`

**Steps:**
1. Write a failing test that checks the detector class list is exactly `leaf`, `fruit`, `crown`, `mealybug`, `rot_region`, `discoloration_region`.
2. Run `python -m unittest ai_backend.training.tests.test_detector_classes` and confirm it fails before the detector scaffold exists.
3. Implement `train_object_detector_tf.py` as the detector entry point, reading from `ai_backend/training/datasets/detector` and expecting TensorFlow-native annotation inputs or a documented conversion step.
4. Implement `export_object_detector_tf.py` so the trained detector can be exported into a predictable inference directory under `ai_backend/training/models/detector/`.
5. Re-run `python -m unittest ai_backend.training.tests.test_detector_classes` and then run a smoke command such as `python ai_backend/training/train_object_detector_tf.py --help`.
6. Commit only the detector training and export files from this task.

### Task 5: Add local inference and fusion

**Files:**
- Create: `ai_backend/training/infer_pineapple_status.py`
- Create: `ai_backend/training/fuse_predictions.py`
- Create: `ai_backend/training/tests/test_fuse_predictions.py`

**Steps:**
1. Write failing fusion tests that cover these cases:
   - classifier says `Healthy` but detector finds `rot_region`
   - classifier says `Needs_Attention` and detector finds `mealybug`
   - detector finds both `mealybug` and `rot_region`
   - classifier is strong and detector only finds structural objects
2. Run `python -m unittest ai_backend.training.tests.test_fuse_predictions` and confirm the tests fail before the fusion module exists.
3. Implement `fuse_predictions.py` with simple, explainable rules that raise severity when detector evidence justifies it.
4. Implement `infer_pineapple_status.py` to load the exported detector and classifier, run both on an input image, call the fusion logic, and write structured JSON into `ai_backend/training/outputs/predictions/`.
5. Re-run `python -m unittest ai_backend.training.tests.test_fuse_predictions`.
6. Run a smoke command such as `python ai_backend/training/infer_pineapple_status.py --help`.
7. Commit only the inference and fusion files from this task.

### Task 6: Add local Gemma task generation from structured prediction JSON

**Files:**
- Create: `ai_backend/training/generate_tasks_with_gemma.py`
- Create: `ai_backend/training/tests/test_generate_tasks_prompt.py`

**Steps:**
1. Write a failing test that verifies the prompt builder includes `final_status`, classifier confidence, and detector findings, and that it expects structured JSON back from Gemma.
2. Run `python -m unittest ai_backend.training.tests.test_generate_tasks_prompt` and confirm it fails before the Gemma helper exists.
3. Implement `generate_tasks_with_gemma.py` so it reads the fused JSON output, builds a grounded prompt, calls the configured local Gemma endpoint, and writes a final task JSON file into `ai_backend/training/outputs/predictions/`.
4. Keep the Gemma input limited to structured model outputs rather than raw dataset access.
5. Re-run `python -m unittest ai_backend.training.tests.test_generate_tasks_prompt`.
6. Run a smoke command such as `python ai_backend/training/generate_tasks_with_gemma.py --help`.
7. Commit only the Gemma task-generation files from this task.

### Task 7: Add an end-to-end local verification path

**Files:**
- Create: `ai_backend/training/run_local_pipeline.py`
- Create: `ai_backend/training/tests/test_pipeline_contract.py`
- Modify: `ai_backend/training/README.md`

**Steps:**
1. Write a failing contract test that verifies the pipeline writes:
   - one fused prediction JSON
   - one Gemma task JSON
   - the expected top-level keys
2. Run `python -m unittest ai_backend.training.tests.test_pipeline_contract` and confirm it fails before the runner exists.
3. Implement `run_local_pipeline.py` as the single command that accepts one image path and executes detection, classification, fusion, and Gemma task generation in sequence.
4. Update `ai_backend/training/README.md` with exact local commands for:
   - preparing datasets
   - training classifier
   - training detector
   - running one-image inference
   - generating Gemma tasks
   - running the end-to-end pipeline
5. Re-run `python -m unittest ai_backend.training.tests.test_pipeline_contract`.
6. Run the full local verification sequence:
   - `python -m unittest ai_backend.training.tests.test_prepare_classifier_dataset ai_backend.training.tests.test_prepare_detector_dataset ai_backend.training.tests.test_status_label_map ai_backend.training.tests.test_detector_classes ai_backend.training.tests.test_fuse_predictions ai_backend.training.tests.test_generate_tasks_prompt ai_backend.training.tests.test_pipeline_contract`
   - `python ai_backend/training/run_local_pipeline.py --help`
7. Commit only the verification runner and README updates from this task.
