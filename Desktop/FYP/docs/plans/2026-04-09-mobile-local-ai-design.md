# Mobile Local-First Pineapple AI Design

## Goal

Add a hybrid mobile AI flow to the Flutter app so pineapple scans return a fast on-device result first, then optionally upgrade with richer backend Gemma guidance when the local PC backend is reachable.

## Direction

Use a local TensorFlow Lite classifier on the phone for the first result, then attempt a short backend reachability check before requesting backend AI advice. The app must remain usable when the PC is off, Ollama is closed, or the phone is on a different Wi-Fi network.

## Architecture

### 1. Local-first scan pipeline

- Add a `LocalAiService` in Flutter.
- Load the classifier interpreter once and keep it warm in memory.
- Run image preprocessing and local classification on-device.
- Produce an immediate scan result with:
  - local status
  - confidence
  - basic status-based actions
  - optional sensor-aware urgency summary

### 2. Backend upgrade path

- Add a backend reachability check with a short timeout against `/health`.
- If reachable, the app starts the existing backend analysis path in the background.
- The result screen upgrades itself when the backend finishes.
- If unreachable, the result screen stays in offline AI mode without blocking the user.

### 3. Different Wi-Fi support

- Add configurable backend URL support in app storage.
- Use:
  - compile-time default URL
  - cached user-configured URL
  - last-known reachable URL
- Do not depend on auto LAN scanning in the first version.
- Allow manual backend URL entry from the settings/profile area.

### 4. UX behavior

- Open the result screen immediately after local inference.
- Show an offline/local badge when the backend is not reachable.
- Replace the “wait for backend job before navigating” flow with:
  1. local result now
  2. optional backend enhancement after

## Fallback behavior

If the `.tflite` model file is not present or cannot be loaded:

- do not crash the scan flow
- fall back to a sensor-only estimate when sensor data exists
- otherwise show a user-facing message that local AI model assets are missing

This keeps development builds usable before the real mobile model asset is added.

## Files to Touch

- `flutter_mobile/pubspec.yaml`
- `flutter_mobile/lib/core/config/app_config.dart`
- `flutter_mobile/lib/core/network/app_dio.dart`
- `flutter_mobile/lib/core/storage/session_storage.dart`
- `flutter_mobile/lib/app/router/app_router.dart`
- `flutter_mobile/lib/features/scan/data/scan_repository.dart`
- `flutter_mobile/lib/features/scan/presentation/scan_screen.dart`
- `flutter_mobile/lib/features/scan/presentation/scan_result_screen.dart`
- `flutter_mobile/lib/features/profile/presentation/profile_screen.dart`

New files:

- `flutter_mobile/lib/core/network/backend_target_provider.dart`
- `flutter_mobile/lib/features/scan/data/local_ai_service.dart`
- `flutter_mobile/lib/features/scan/data/local_ai_models.dart`
- `flutter_mobile/test/features/scan/local_ai_service_test.dart`

## Validation

- `flutter pub get`
- `flutter analyze`
- `flutter test`

## Notes

- Real offline image inference depends on adding the trained `.tflite` classifier asset later.
- The first implementation should be honest about missing assets and degrade gracefully.
