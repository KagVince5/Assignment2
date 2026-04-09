# Pineapple Status TensorFlow And Gemma Design

**Project:** Pineapple Plant Monitoring System  
**Date:** 2026-04-09  
**Design Template:** `n/a` (AI pipeline design)

## Goal

Create a no-database-first pineapple plant status pipeline that learns visual pineapple objects and final plant condition locally, then converts the result into actionable user tasks with a local Gemma model.

## Chosen Direction

Build a two-model local pipeline with file-based datasets and outputs:

1. a TensorFlow object detector that learns pineapple plant parts and suspicious visual regions
2. a TensorFlow image classifier that predicts the final user-facing plant status
3. a lightweight fusion layer that combines both model outputs into one final status
4. a local Gemma step that turns the final status into clear next actions for the user

This version intentionally avoids PostgreSQL and Firebase so the team can focus first on model quality, dataset curation, and a working end-to-end local AI loop.

## Final User Status Labels

The classifier and fused runtime output should use these four final labels:

- `Healthy`
- `Needs_Attention`
- `Disease_Risk`
- `Urgent_Action`

These labels are intentionally user-facing rather than agronomy-specific so they can be shown directly in the app later.

## System Architecture

### Model A: Object Detector

The detector should learn concrete, visible objects or regions rather than abstract health states.

Recommended detector labels:

- `leaf`
- `fruit`
- `crown`
- `mealybug`
- `rot_region`
- `discoloration_region`

This model gives the system the "pineapple object understanding" layer the user requested.

### Model B: Status Classifier

The classifier should operate on the whole image and predict one of the four final user-facing status labels.

Recommended status mapping:

- `Healthy`: normal visual appearance, no strong stress signal
- `Needs_Attention`: mild stress, early discoloration, or a weak anomaly
- `Disease_Risk`: credible disease or pest evidence without catastrophic damage
- `Urgent_Action`: severe damage, rot, or a strong visible infestation

### Fusion Layer

The first version should use explainable rule-based fusion rather than another ML model.

Recommended fusion behavior:

- if the detector finds `rot_region` with high confidence, do not allow the final status to remain `Healthy`
- if the detector finds `mealybug` and the classifier predicts `Needs_Attention`, raise the final status to at least `Disease_Risk`
- if the detector finds both `mealybug` and `rot_region`, raise the final status to `Urgent_Action`
- if the detector finds only normal structural objects and classifier confidence is high, trust the classifier result

This keeps the pipeline transparent and easier to debug during early training.

## File-Based Data Strategy

This design intentionally avoids a database in phase one.

Recommended file structure:

```text
ai_backend/
  training/
    datasets/
      detector/
        images/
          train/
          val/
          test/
        labels/
          train/
          val/
          test/
        classes.json
      classifier/
        train/
          Healthy/
          Needs_Attention/
          Disease_Risk/
          Urgent_Action/
        val/
          Healthy/
          Needs_Attention/
          Disease_Risk/
          Urgent_Action/
        test/
          Healthy/
          Needs_Attention/
          Disease_Risk/
          Urgent_Action/
        label_map.json
    manifests/
      source_manifest.csv
      detector_manifest.csv
      classifier_manifest.csv
    models/
      detector/
      classifier/
    outputs/
      predictions/
      evaluation/
```

All collection, labeling, splitting, predictions, and evaluation artifacts should live inside this tree.

## Training Workflow

1. Collect approved pineapple images from public datasets and the project's own field images.
2. Record image provenance and licensing in `source_manifest.csv`.
3. Split the dataset into two tracks:
   - detector images with bounding boxes
   - classifier images with one final status label
4. Remove duplicates, blurry frames, and non-pineapple images.
5. Create `train / val / test` splits using a `70 / 15 / 15` ratio.
6. Train the detector first so the team can validate pineapple-object understanding.
7. Train the classifier second using the four final status labels.
8. Evaluate each model independently before introducing fusion.
9. Save fused prediction results into local JSON output files.
10. Run Gemma only after the fused final status is available.

## TensorFlow Recommendations

### Detector

Preferred path:

- TensorFlow Object Detection API with an SSD MobileNet or EfficientDet-style model

Fallback path if needed:

- use a detector trained in another toolchain only if it can be exported cleanly into the TensorFlow runtime expected by the project

### Classifier

Use TensorFlow transfer learning with a lightweight architecture such as:

- `EfficientNetB0`
- `MobileNetV3`

Recommended image sizes:

- `224x224`
- `256x256`

Recommended augmentations:

- horizontal flip where biologically reasonable
- mild rotation
- brightness shift
- zoom

Do not rely on aggressive synthetic augmentation before the core real-image dataset is stable.

## Local Gemma Role

Gemma should not make the primary visual classification decision in this phase.

Instead, it should read structured prediction output such as:

```json
{
  "classifier_status": "Disease_Risk",
  "classifier_confidence": 0.84,
  "detector_findings": [
    {"label": "leaf", "confidence": 0.93},
    {"label": "discoloration_region", "confidence": 0.81},
    {"label": "mealybug", "confidence": 0.77}
  ],
  "final_status": "Disease_Risk"
}
```

Gemma should then produce structured actionable output such as:

```json
{
  "summary": "The pineapple plant shows visible stress and likely pest-related disease risk.",
  "priority": "high",
  "recommended_tasks": [
    "Inspect the lower leaf bases for mealybugs.",
    "Separate this plant from healthy plants if possible.",
    "Take another close-up image within 24 hours."
  ],
  "follow_up_hours": 24
}
```

This keeps the LLM grounded and useful instead of letting it replace the trained vision models.

## Dataset Sources

Recommended starting sources for phase one:

- Roboflow pineapple disease datasets for detector and status seed data
- Mendeley pineapple image datasets where license terms permit training use
- Wikimedia Commons `Ananas comosus` images for healthy and general pineapple context
- GBIF `Ananas comosus` observations for more real-world diversity
- project-owned pineapple photos from ESP32-CAM or mobile capture

License and source tracking should be recorded in the manifest files before images are promoted into the cleaned training sets.

## Evaluation And Success Criteria

### Detector

Track:

- precision
- recall
- mAP

Goal:

- reliably detect pineapple structures and suspicious regions on held-out images

### Classifier

Track:

- overall accuracy
- per-class precision and recall
- confusion matrix

Critical confusion checks:

- `Needs_Attention` vs `Disease_Risk`
- `Disease_Risk` vs `Urgent_Action`

### End-To-End

The fused system is successful when:

- severe detector findings correctly prevent falsely calm final statuses
- the final status is stable across realistic pineapple images
- Gemma outputs short, actionable, and non-alarmist tasks

## Rollout Plan

### Phase 1

- local folder datasets
- local TensorFlow training
- local inference and JSON outputs
- local Gemma task generation

### Phase 2

- integrate the same fused runtime into FastAPI
- expose local prediction results to the app
- optionally persist outputs to PostgreSQL

### Phase 3

- add sensor data into the fusion layer
- save prediction history
- show timeline and tasks in the mobile app

## Design Rules

- Keep the first version fully local and file-based.
- Keep detector labels visual and literal.
- Keep classifier labels user-facing and simple.
- Keep fusion explainable with clear rules.
- Use Gemma only after structured model outputs exist.
- Prefer real pineapple imagery over synthetic imagery for early training.
