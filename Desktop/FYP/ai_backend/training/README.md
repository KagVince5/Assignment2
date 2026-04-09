# Pineapple Local AI Training Workspace

This folder is the local, file-based workspace for the pineapple status MVP.

## Goal

Train two local TensorFlow models without a database first:

1. a detector that understands pineapple plant objects and suspicious regions
2. a status classifier that predicts the user-facing plant condition

The final runtime pipeline will combine both model outputs and later pass the structured result to a local Gemma model for user-facing task generation.

## Current Training Approach

Use a manifest-driven workflow so every image has explicit source and label metadata.

- `source_manifest.csv` tracks image provenance and licensing
- `detector_manifest.csv` tracks object-detection annotations
- `classifier_manifest.csv` tracks final status labels

The workspace is intentionally file-based for now. PostgreSQL and the backend database layer will be added later after the local training loop is stable.

## Folder Layout

```text
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

## Label Sets

### Detector Labels

- `leaf`
- `fruit`
- `crown`
- `mealybug`
- `rot_region`
- `discoloration_region`

### Classifier Labels

- `Healthy`
- `Needs_Attention`
- `Disease_Risk`
- `Urgent_Action`

## Suggested Workflow

1. Collect pineapple images from approved sources or project photos.
2. Record source metadata in `manifests/source_manifest.csv`.
3. Prepare detector annotations and classifier labels into the folder layout above.
4. Split the dataset into `train`, `val`, and `test`.
5. Train the detector and classifier separately.
6. Run local inference on a pineapple image.
7. Fuse both outputs into one final status.
8. Pass the fused JSON to local Gemma for task generation.

## Training Dependencies

Use `requirements-training.txt` for the local training environment.

## Notes

- Keep the first version fully local and file-based.
- Do not replace the runtime backend yet.
- Keep labels simple and user-facing for the classifier.
