# Mobile Local-First Pineapple AI Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a Flutter mobile scan flow that returns immediate local AI pineapple status results and optionally upgrades them with backend Gemma guidance when the PC backend is reachable.

**Architecture:** The app will run a local TensorFlow Lite classifier first, then try a short backend `/health` check. If the backend is reachable, the result screen upgrades in place with richer backend analysis. If it is not reachable, the app remains fully usable in offline AI mode.

**Tech Stack:** Flutter, Riverpod, Dio, flutter_secure_storage, tflite_flutter, image, existing FastAPI backend

---

### Task 1: Add backend URL persistence and dynamic Dio base URL

**Files:**
- Modify: `flutter_mobile/lib/core/storage/session_storage.dart`
- Create: `flutter_mobile/lib/core/network/backend_target_provider.dart`
- Modify: `flutter_mobile/lib/core/network/app_dio.dart`
- Test: `flutter_mobile/test/features/scan/backend_target_provider_test.dart`

**Step 1: Write the failing test**

Write a provider test that verifies a stored backend URL overrides the compile-time default after loading.

**Step 2: Run test to verify it fails**

Run: `flutter test test/features/scan/backend_target_provider_test.dart`

**Step 3: Write minimal implementation**

- Add storage keys for backend URL and last-good backend URL.
- Create a provider/controller that loads and updates the backend URL.
- Rebuild Dio from the provider’s effective base URL.

**Step 4: Run test to verify it passes**

Run: `flutter test test/features/scan/backend_target_provider_test.dart`

**Step 5: Commit**

`git commit -m "feat: add configurable backend target"`

### Task 2: Add local AI models and fallback rules

**Files:**
- Create: `flutter_mobile/lib/features/scan/data/local_ai_models.dart`
- Create: `flutter_mobile/lib/features/scan/data/local_ai_service.dart`
- Test: `flutter_mobile/test/features/scan/local_ai_service_test.dart`

**Step 1: Write the failing test**

Write a pure Dart test that verifies local fallback advice and urgency mapping for the four status classes.

**Step 2: Run test to verify it fails**

Run: `flutter test test/features/scan/local_ai_service_test.dart`

**Step 3: Write minimal implementation**

- Add local-first scan result models.
- Add a local AI service that:
  - tries to load a TFLite model
  - falls back gracefully if the model is missing
  - returns immediate status, confidence, and basic actions

**Step 4: Run test to verify it passes**

Run: `flutter test test/features/scan/local_ai_service_test.dart`

**Step 5: Commit**

`git commit -m "feat: add local scan ai service"`

### Task 3: Refactor scan repository for local-first result plus backend upgrade

**Files:**
- Modify: `flutter_mobile/lib/features/scan/data/scan_repository.dart`

**Step 1: Write the failing test**

Add or expand tests for result mapping if needed, or validate through the new service tests plus analyzer constraints.

**Step 2: Run validation to confirm missing APIs**

Run: `flutter analyze`

**Step 3: Write minimal implementation**

- Add a bundle object that carries:
  - immediate local result
  - optional backend-upgrade request
- Add a short backend health check method.
- Keep the existing backend analysis methods for the upgrade phase.
- Change scan entry methods to return the local-first bundle immediately.

**Step 4: Re-run analyzer**

Run: `flutter analyze`

**Step 5: Commit**

`git commit -m "feat: refactor scan flow for local-first results"`

### Task 4: Update router and result screen for in-place backend enhancement

**Files:**
- Modify: `flutter_mobile/lib/app/router/app_router.dart`
- Modify: `flutter_mobile/lib/features/scan/presentation/scan_result_screen.dart`

**Step 1: Write the failing test or compile-time expectation**

Use analyzer failures from changed route extra typing and screen state as the initial failing signal.

**Step 2: Run analyzer to verify it fails**

Run: `flutter analyze`

**Step 3: Write minimal implementation**

- Route `ScanResultScreen` with the new local-result bundle.
- Show the local result immediately.
- Start backend enhancement in `initState`.
- Update the screen when backend analysis returns.
- Show offline/local mode when backend is not reachable.

**Step 4: Re-run analyzer**

Run: `flutter analyze`

**Step 5: Commit**

`git commit -m "feat: add result screen backend enhancement flow"`

### Task 5: Add backend URL controls to profile/settings

**Files:**
- Modify: `flutter_mobile/lib/features/profile/presentation/profile_screen.dart`

**Step 1: Add minimal UI**

- Show current effective backend URL
- Add edit action
- Save new URL into storage
- Add reset-to-default option

**Step 2: Run analyzer**

Run: `flutter analyze`

**Step 3: Commit**

`git commit -m "feat: add backend url settings"`

### Task 6: Add TFLite dependency and asset placeholders

**Files:**
- Modify: `flutter_mobile/pubspec.yaml`
- Create: `flutter_mobile/assets/models/README.txt`

**Step 1: Add dependencies**

- `tflite_flutter`
- `image`

**Step 2: Add asset path**

Add `assets/models/` to Flutter assets and include a placeholder file so builds do not fail.

**Step 3: Run dependency sync**

Run: `flutter pub get`

**Step 4: Commit**

`git commit -m "chore: add mobile ai dependencies"`

### Task 7: Full verification

**Files:**
- No code changes unless fixes are needed

**Step 1: Run tests**

Run: `flutter test`

**Step 2: Run analyzer**

Run: `flutter analyze`

**Step 3: Fix any issues**

Keep fixes minimal and scoped.

**Step 4: Commit final verification fixes if needed**

`git commit -m "fix: polish mobile local ai integration"`
