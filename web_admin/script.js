const enterAdminButton = document.getElementById("enter-admin");
const loginView = document.querySelector('[data-screen="login"]');
const adminView = document.querySelector('[data-screen="admin"]');
const pageTitle = document.getElementById("page-title");
const logoutButton = document.getElementById("logout-button");
const navItems = Array.from(document.querySelectorAll(".nav-item"));
const pages = Array.from(document.querySelectorAll(".page"));
const jumpButtons = Array.from(document.querySelectorAll("[data-page-jump]"));
const presetButtons = Array.from(document.querySelectorAll("[data-user-preset]"));
const managedUserForm = document.getElementById("managed-user-form");
const managedUserName = document.getElementById("managed-user-name");
const managedUserEmail = document.getElementById("managed-user-email");
const managedUserRole = document.getElementById("managed-user-role");
const managedUserLocation = document.getElementById("managed-user-location");
const managedUserNote = document.getElementById("managed-user-note");
const managedUserPassword = document.getElementById("managed-user-password");
const managedUserPasswordConfirm = document.getElementById("managed-user-password-confirm");
const userDirectoryBody = document.getElementById("user-directory-body");
const managedUserCount = document.getElementById("managed-user-count");
const adminEmailInput = document.getElementById("admin-email");
const adminPasswordInput = document.getElementById("admin-password");
const loginFeedback = document.getElementById("login-feedback");
const removeUserDialog = document.getElementById("remove-user-dialog");
const removeUserForm = document.getElementById("remove-user-form");
const removeUserTitle = document.getElementById("remove-user-title");
const removeUserMessage = document.getElementById("remove-user-message");
const removeUserPasswordInput = document.getElementById("remove-user-password");
const cancelRemoveUserButton = document.getElementById("cancel-remove-user");

const heroSignalValues = Array.from(
  document.querySelectorAll(".hero-signal strong"),
);
const topbarSyncValue = document.querySelector(".topbar-actions .capsule strong");
const refreshCloudDataButton = document.getElementById("refresh-cloud-data-button");
const dashboardRibbonValues = Array.from(
  document.querySelectorAll(".dashboard-ribbon article strong"),
);
const dashboardMiniCardValues = Array.from(
  document.querySelectorAll(".dashboard-mini-card strong"),
);
const dashboardMiniCardNotes = Array.from(
  document.querySelectorAll(".dashboard-mini-card small"),
);
const lensCenterValue = document.querySelector(".lens-center strong");
const analysisCompletedJobsValue = document.getElementById("analysis-completed-jobs");
const analysisCompletedNote = document.getElementById("analysis-completed-note");
const analysisTopClassValue = document.getElementById("analysis-top-class");
const analysisTopClassNote = document.getElementById("analysis-top-class-note");
const analysisOverlapRateValue = document.getElementById("analysis-overlap-rate");
const analysisOverlapNote = document.getElementById("analysis-overlap-note");
const analysisQueueWaiting = document.getElementById("analysis-queue-waiting");
const analysisQueueRunning = document.getElementById("analysis-queue-running");
const analysisQueueCompleted = document.getElementById("analysis-queue-completed");
const analysisPredictionSpread = document.getElementById("analysis-prediction-spread");
const analysisCorrelationLog = document.getElementById("analysis-correlation-log");
const analysisSubmitForm = document.getElementById("analysis-submit-form");
const analysisImageInput = document.getElementById("analysis-image-input");
const analysisSubmitButton = document.getElementById("analysis-submit-button");
const analysisLatestFrameButton = document.getElementById("analysis-latest-frame-button");
const analysisSubmitState = document.getElementById("analysis-submit-state");
const analysisUploadNote = document.getElementById("analysis-upload-note");
const analysisResultCard = document.getElementById("analysis-result-card");
const analysisResultStatus = document.getElementById("analysis-result-status");
const analysisResultImage = document.getElementById("analysis-result-image");
const analysisResultDisease = document.getElementById("analysis-result-disease");
const analysisResultConfidence = document.getElementById("analysis-result-confidence");
const analysisResultAdvice = document.getElementById("analysis-result-advice");
const recordsStream = document.getElementById("records-stream");
const recordsMixList = document.getElementById("records-mix-list");
const recordFilterButtons = Array.from(document.querySelectorAll("[data-record-filter]"));
const openSelectedRecordButton = document.getElementById("open-selected-record-button");
const detailRemoveRecordButton = document.getElementById("detail-remove-record-button");
const detailRecordTitle = document.getElementById("detail-record-title");
const detailImageStage = document.getElementById("detail-image-stage");
const detailImage = document.getElementById("detail-image");
const detailImageCaption = document.getElementById("detail-image-caption");
const detailImageLabel = document.getElementById("detail-image-label");
const detailConfidencePill = document.getElementById("detail-confidence-pill");
const detailPredictedCondition = document.getElementById("detail-predicted-condition");
const detailSuggestedAction = document.getElementById("detail-suggested-action");
const detailReviewerNote = document.getElementById("detail-reviewer-note");
const detailTemperature = document.getElementById("detail-temperature");
const detailHumidity = document.getElementById("detail-humidity");
const detailSoilMoisture = document.getElementById("detail-soil-moisture");
const detailPh = document.getElementById("detail-ph");
const detailReviewThread = document.getElementById("detail-review-thread");
const monitorBackendStatus = document.getElementById("monitor-backend-status");
const monitorBackendNote = document.getElementById("monitor-backend-note");
const monitorNodeStatus = document.getElementById("monitor-node-status");
const monitorNodeNote = document.getElementById("monitor-node-note");
const monitorCameraStatus = document.getElementById("monitor-camera-status");
const monitorCameraNote = document.getElementById("monitor-camera-note");
const monitorAlertStatus = document.getElementById("monitor-alert-status");
const monitorAlertNote = document.getElementById("monitor-alert-note");
const monitorCameraPreview = document.getElementById("monitor-camera-preview");
const monitorCameraFreshness = document.getElementById("monitor-camera-freshness");
const monitorFreshnessLog = document.getElementById("monitor-freshness-log");
const coverageHealthPill = document.getElementById("coverage-health-pill");
const coverageTableCount = document.getElementById("coverage-table-count");
const coverageRowCount = document.getElementById("coverage-row-count");
const coverageEmptyCount = document.getElementById("coverage-empty-count");
const coverageGroupGrid = document.getElementById("coverage-group-grid");
const coverageEntityList = document.getElementById("coverage-entity-list");
const settingsSoilDry = document.getElementById("settings-soil-dry");
const settingsPhLow = document.getElementById("settings-ph-low");
const settingsTempHigh = document.getElementById("settings-temp-high");
const settingsPollingCadence = document.getElementById("settings-polling-cadence");
const settingsAnalysisMode = document.getElementById("settings-analysis-mode");
const settingsBackendUrl = document.getElementById("settings-backend-url");
const settingsCriticalAlerts = document.getElementById("settings-critical-alerts");
const settingsAnalysisNotices = document.getElementById("settings-analysis-notices");
const settingsDigestSummary = document.getElementById("settings-digest-summary");
const settingsSessionTimeout = document.getElementById("settings-session-timeout");
const settingsConfigStatus = document.getElementById("settings-config-status");
const settingsEsp32SsidInput = document.getElementById("settings-esp32-ssid-input");
const settingsEsp32PasswordInput = document.getElementById("settings-esp32-password-input");
const settingsEsp32Status = document.getElementById("settings-esp32-status");
const settingsEsp32CamSsidInput = document.getElementById("settings-esp32-cam-ssid-input");
const settingsEsp32CamPasswordInput = document.getElementById("settings-esp32-cam-password-input");
const settingsEsp32CamStatus = document.getElementById("settings-esp32-cam-status");
const settingsSoilDryInput = document.getElementById("settings-soil-dry-input");
const settingsPhLowInput = document.getElementById("settings-ph-low-input");
const settingsTempHighInput = document.getElementById("settings-temp-high-input");
const settingsSyncCadenceInput = document.getElementById("settings-sync-cadence-input");
const languageSelects = Array.from(document.querySelectorAll("[data-language-select]"));
const settingsCriticalAlertsInput = document.getElementById("settings-critical-alerts-input");
const settingsAnalysisNoticesInput = document.getElementById("settings-analysis-notices-input");
const settingsDigestSummaryInput = document.getElementById("settings-digest-summary-input");
const saveSettingsButton = document.getElementById("save-settings-button");
const resetSettingsButton = document.getElementById("reset-settings-button");

const I18N = {
  en: {
    pageTitles: {
      dashboard: "Admin Dashboard",
      users: "User Management",
      records: "Submitted Records",
      detail: "Record Detail",
      analysis: "Data Analysis",
      monitor: "System Monitor",
      settings: "Settings",
    },
    nav: {
      dashboard: ["Admin Dashboard", "Operational state and drift pressure"],
      users: ["User Management", "Roles, onboarding, and account control"],
      records: ["Submitted Records", "Saved scans and activity journal"],
      analysis: ["Data Analysis", "Sensor trends and AI job performance"],
      monitor: ["System Monitor", "Device, camera, and service flow"],
      settings: ["Settings", "Thresholds, sync, and preferences"],
    },
    ui: {
      enterDashboard: "Enter admin dashboard",
      logout: "Logout",
      logoutHint: "Return to admin sign-in",
      refreshCloudData: "Refresh data",
      signInTitle: "Sign in to continue",
      signInSubtitle: "Manage users, records, alerts, and reviews.",
      emailLabel: "Work email",
      passwordLabel: "Password",
      languageLabel: "Language",
      topbarEyebrow: "Web administration",
    },
  },
  ms: {
    pageTitles: {
      dashboard: "Papan Pemuka Admin",
      users: "Pengurusan Pengguna",
      records: "Rekod Dihantar",
      detail: "Butiran Rekod",
      analysis: "Analisis Data",
      monitor: "Pemantauan Sistem",
      settings: "Tetapan",
    },
    nav: {
      dashboard: ["Papan Pemuka Admin", "Status operasi dan tekanan perubahan"],
      users: ["Pengurusan Pengguna", "Peranan, pendaftaran, dan kawalan akaun"],
      records: ["Rekod Dihantar", "Imbasan tersimpan dan jurnal aktiviti"],
      analysis: ["Analisis Data", "Trend sensor dan prestasi kerja AI"],
      monitor: ["Pemantauan Sistem", "Aliran peranti, kamera, dan servis"],
      settings: ["Tetapan", "Ambang, penyegerakan, dan pilihan"],
    },
    ui: {
      enterDashboard: "Masuk ke papan pemuka admin",
      logout: "Log keluar",
      logoutHint: "Kembali ke log masuk admin",
      refreshCloudData: "Muat semula data",
      signInTitle: "Log masuk untuk teruskan",
      signInSubtitle: "Urus pengguna, rekod, amaran, dan semakan.",
      emailLabel: "E-mel kerja",
      passwordLabel: "Kata laluan",
      languageLabel: "Bahasa",
      topbarEyebrow: "Pentadbiran web",
    },
  },
};

const webTextNodeState = new WeakMap();

const WEB_MS_EXACT = {
  "PineGuard admin": "Admin PineGuard",
  "Operational oversight for orchard health, people, and follow-up.":
    "Pemantauan operasi untuk kesihatan kebun, pengguna, dan susulan.",
  "Light mode": "Mod cerah",
  "Liquid glass": "Kaca cair",
  "Johor operations zone": "Zon operasi Johor",
  "System trust": "Kepercayaan sistem",
  "Live node": "Nod langsung",
  "Active analysis": "Analisis aktif",
  "Secure admin gateway": "Gerbang admin selamat",
  "Role access: admin and farmer": "Akses peranan: admin dan petani",
  "Session managed securely": "Sesi diurus dengan selamat",
  "Light mode active": "Mod cerah aktif",
  "Last sync": "Segerak terakhir",
  "Operational overview": "Gambaran operasi",
  "Temperature": "Suhu",
  "Humidity": "Kelembapan",
  "Soil moisture": "Kelembapan tanah",
  "Node health": "Kesihatan nod",
  "Drift": "Perubahan",
  "Pressure": "Tekanan",
  "Signal": "Isyarat",
  "Moisture": "Kelembapan",
  "Queued scans": "Imbasan beratur",
  "Field node online": "Nod ladang dalam talian",
  "Users": "Pengguna",
  "Records": "Rekod",
  "Analysis": "Analisis",
  "Monitor": "Pantau",
  "Settings": "Tetapan",
  "Activity": "Aktiviti",
  "Scan": "Imbasan",
  "Logged": "Direkod",
  "Needs review": "Perlu semakan",
  "Open selected record": "Buka rekod dipilih",
  "Record stream": "Aliran rekod",
  "Newest items": "Item terbaharu",
  "Data Analysis": "Analisis Data",
  "System Monitor": "Pemantauan Sistem",
  "User Management": "Pengurusan Pengguna",
  "Full name": "Nama penuh",
  "Work email": "E-mel kerja",
  "Role": "Peranan",
  "Base location": "Lokasi asas",
  "New password": "Kata laluan baharu",
  "Confirm password": "Sahkan kata laluan",
  "Set user password": "Tetapkan kata laluan pengguna",
  "Re-enter password": "Masukkan semula kata laluan",
  "Access note": "Nota akses",
  "Add user": "Tambah pengguna",
  "Set a password here so the new farmer can sign in from mobile immediately.":
    "Tetapkan kata laluan di sini supaya petani baharu boleh log masuk dari mudah alih serta-merta.",
  "Set a password with at least 6 characters for the new user.":
    "Tetapkan kata laluan sekurang-kurangnya 6 aksara untuk pengguna baharu.",
  "The new user password confirmation does not match.":
    "Pengesahan kata laluan pengguna baharu tidak sepadan.",
  "Sign in as an admin before creating users.":
    "Log masuk sebagai admin sebelum mencipta pengguna.",
  "Unable to create this user.": "Tidak dapat mencipta pengguna ini.",
  "Submitted Records": "Rekod Dihantar",
  "Record Detail": "Butiran Rekod",
  "Admin Dashboard": "Papan Pemuka Admin",
  "Refresh data": "Muat semula data",
  "Syncing...": "Menyegerak...",
  "Retry data": "Cuba semula data",
  "Choose image": "Pilih imej",
  "Processing": "Memproses",
  "Complete": "Selesai",
  "Queued": "Dalam giliran",
  "Running": "Berjalan",
  "Waiting": "Menunggu",
  "Healthy": "Sihat",
  "Water Stress": "Tekanan Air",
  "No Pineapple Captured": "Tiada Nanas Dikesan",
  "Connected": "Disambung",
  "Service live": "Servis langsung",
  "Field camera": "Kamera ladang",
  "Field camera image": "Imej kamera ladang",
  "Latest field camera frame": "Bingkai kamera ladang terkini",
  "Operational checks": "Semakan operasi",
  "Sensor readings": "Bacaan sensor",
  "Plant image analysis": "Analisis imej tanaman",
  "Service controls": "Kawalan servis",
  "Hardware Wi-Fi": "Wi-Fi Perkakasan",
  "ESP32 connection": "Sambungan ESP32",
  "ESP32 sensor node": "Nod sensor ESP32",
  "ESP32-CAM": "ESP32-CAM",
  "Network name used by the sensor node": "Nama rangkaian yang digunakan oleh nod sensor",
  "Network name used by the camera node": "Nama rangkaian yang digunakan oleh nod kamera",
  "Wi-Fi name": "Nama Wi-Fi",
  "Wi-Fi password": "Kata laluan Wi-Fi",
  "Keep current password": "Kekalkan kata laluan semasa",
  "Not configured": "Belum dikonfigurasi",
  "Configured": "Dikonfigurasi",
  "Saved for hardware sync": "Disimpan untuk segerak perkakasan",
  "Saved for ESP32 and ESP32-CAM sync": "Disimpan untuk segerak ESP32 dan ESP32-CAM",
  "After each device reaches the backend once, it stores this Wi-Fi and reconnects with the latest admin setting.":
    "Selepas setiap peranti mencapai backend sekali, ia menyimpan Wi-Fi ini dan menyambung semula dengan tetapan admin terkini.",
  "Service route": "Laluan servis",
  "Active workspace connection": "Sambungan ruang kerja aktif",
  "Workspace status": "Status ruang kerja",
  "Workspace controls": "Kawalan ruang kerja",
  "Standard route": "Laluan standard",
  "Cloud analysis relay": "Relay analisis awan",
  "Service degraded": "Servis terganggu",
  "Connected to PineGuard workspace.": "Disambung ke ruang kerja PineGuard.",
  "Connected with limited workspace data.":
    "Disambung dengan data ruang kerja terhad.",
  "Unable to sign in. Check the service connection.":
    "Tidak dapat log masuk. Periksa sambungan servis.",
  "Unable to save settings.": "Tidak dapat menyimpan tetapan.",
  "Unable to reset settings.": "Tidak dapat menetapkan semula tetapan.",
  "Unable to sync language.": "Tidak dapat menyegerakkan bahasa.",
  "PineGuard could not complete this AI analysis job.":
    "PineGuard tidak dapat melengkapkan kerja analisis AI ini.",
  "Unavailable": "Tidak tersedia",
  "Saved": "Disimpan",
  "Defaults restored": "Tetapan asal dipulihkan",
  "Remove": "Buang",
  "Delete": "Padam",
  "Cancel": "Batal",
  "Save": "Simpan",
  "On": "Hidup",
  "Off": "Mati",
  "Admin": "Admin",
  "Farmer": "Petani",
  "No activity yet": "Belum ada aktiviti",
  "Just now": "Baru sahaja",
  "Administration": "Pentadbiran",
  "Online": "Dalam talian",
  "Read the orchard like a live instrument, not a static dashboard.":
    "Baca kebun seperti instrumen langsung, bukan papan pemuka statik.",
  "The admin side now prioritises drift, trend, and analysis pressure. Live readings, recent alerts, and AI scan volume sit in one visual field so the deployment stays supervised rather than merely listed.":
    "Bahagian admin kini mengutamakan perubahan, trend, dan tekanan analisis. Bacaan langsung, amaran terkini, dan jumlah imbasan AI berada dalam satu paparan supaya pelaksanaan kekal dipantau, bukan sekadar disenaraikan.",
  "Live orchard lens": "Lensa kebun langsung",
  "Unread alerts": "Amaran belum dibaca",
  "Queued analysis": "Analisis beratur",
  "Record growth": "Pertumbuhan rekod",
  "2 warning, 1 critical": "2 amaran, 1 kritikal",
  "2 active, 5 completed today": "2 aktif, 5 selesai hari ini",
  "Scan and field log activity rising": "Aktiviti imbasan dan log ladang meningkat",
  "Manage users, records, alerts, and reviews.": "Urus pengguna, rekod, amaran, dan semakan.",
  "A desktop workspace for user access, submitted records, data analysis, and live operational health.":
    "Ruang kerja desktop untuk akses pengguna, rekod dihantar, analisis data, dan kesihatan operasi langsung.",
  "Work email": "E-mel kerja",
  "Password": "Kata laluan",
  "English": "Bahasa Inggeris",
  "Bahasa Melayu": "Bahasa Melayu",
  "Role access: admin and farmer": "Akses peranan: admin dan petani",
  "Session managed securely": "Sesi diurus dengan selamat",
  "Open selected record": "Buka rekod dipilih",
  "Newest items": "Item terbaharu",
  "Needs review": "Perlu semakan",
  "Confidence": "Keyakinan",
  "Predicted condition": "Keadaan diramal",
  "Suggested action": "Tindakan dicadangkan",
  "Reviewer note": "Nota penyemak",
  "Image unavailable": "Imej tidak tersedia",
  "Stored preview": "Pratonton tersimpan",
  "Remove": "Buang",
  "Remove record": "Buang rekod",
  "Treatment": "Rawatan",
  "Recommendation": "Cadangan",
  "Description": "Penerangan",
  "System coverage": "Liputan sistem",
  "Workspace areas and screen use": "Kawasan ruang kerja dan penggunaan skrin",
  "Areas": "Kawasan",
  "Records": "Rekod",
  "Empty": "Kosong",
  "Checking": "Memeriksa",
  "Open every system area": "Buka setiap kawasan sistem",
  "Waiting for system coverage": "Menunggu liputan sistem",
  "Coverage counts will appear after the admin session syncs.":
    "Kiraan liputan akan muncul selepas sesi admin disegerakkan.",
  "Pending": "Menunggu",
  "Synced": "Disegerakkan",
  "Internal": "Dalaman",
  "Degraded": "Terganggu",
  "No recent sync": "Tiada segerak terkini",
  "Mobile": "Mudah alih",
  "Web": "Web",
};

const WEB_MS_REPLACEMENTS = [
  ["Operational", "Operasi"],
  ["overview", "gambaran"],
  ["dashboard", "papan pemuka"],
  ["records", "rekod"],
  ["record", "rekod"],
  ["alerts", "amaran"],
  ["alert", "amaran"],
  ["analysis", "analisis"],
  ["sensor", "sensor"],
  ["camera", "kamera"],
  ["image", "imej"],
  ["upload", "muat naik"],
  ["uploaded", "dimuat naik"],
  ["result", "keputusan"],
  ["recommendation", "cadangan"],
  ["treatment", "rawatan"],
  ["confidence", "keyakinan"],
  ["description", "penerangan"],
  ["status", "status"],
  ["settings", "tetapan"],
  ["threshold", "ambang"],
  ["critical", "kritikal"],
  ["warning", "amaran"],
  ["healthy", "sihat"],
  ["humidity", "kelembapan"],
  ["temperature", "suhu"],
  ["soil moisture", "kelembapan tanah"],
  ["pH", "pH"],
  ["service", "servis"],
  ["node", "nod"],
  ["sync", "segerak"],
  ["live", "langsung"],
  ["review", "semakan"],
  ["logged", "direkod"],
  ["saved", "disimpan"],
  ["new", "baharu"],
  ["latest", "terkini"],
  ["active", "aktif"],
  ["connected", "disambung"],
  ["available", "tersedia"],
  ["unavailable", "tidak tersedia"],
  ["running", "berjalan"],
  ["waiting", "menunggu"],
  ["processing", "memproses"],
  ["complete", "selesai"],
  ["user", "pengguna"],
  ["role", "peranan"],
  ["location", "lokasi"],
  ["password", "kata laluan"],
  ["email", "e-mel"],
  ["login", "log masuk"],
  ["logout", "log keluar"],
];

const DEFAULT_DATABASE_API_BASE_URL = "https://pineguard-api.onrender.com";
const LOCAL_MACHINE_API_BASE_URL = "http://127.0.0.1:8000";
const API_BASE_URL_STORAGE_KEY = "pineguardAdminApiBaseUrl";
const LANGUAGE_STORAGE_KEY = "pineguardAdminLanguage";
const LOCAL_API_DISCOVERY_QUERY_KEY = "localApiDiscovery";
const DEFAULT_ALERT_RULE_SETTINGS = {
  tempHigh: 35,
  tempLow: 15,
  humidityLow: 40,
  soilDry: 30,
  soilCriticalDry: 20,
  soilWet: 90,
  phLow: 4.5,
  phHigh: 6.5,
  cooldownMinutes: 30,
};
const DEFAULT_PROFILE_SETTINGS = {
  pushNotificationsEnabled: true,
  dailyDigestEnabled: true,
  assistantRecommendationsEnabled: true,
  syncCadence: "realtime",
  language: "en",
  theme: "system",
  backendMode: "mysql",
};
const LIVE_REFRESH_INTERVAL_MS = 5000;
const LIVE_VALUE_UPDATE_CLASS = "is-live-updated";
const LIVE_VALUE_SHELL_CLASS = "has-live-update";
const API_DISCOVERY_TIMEOUT_MS = 2500;
const API_UPLOAD_TIMEOUT_MS = 30000;
const API_HEALTH_TIMEOUT_MS = 8000;
const RECORD_IMAGE_PREVIEW_MAX_EDGE = 420;
const RECORD_IMAGE_PREVIEW_MAX_CHARS = 60000;
const RECORD_IMAGE_PREVIEW_QUALITY = 0.72;
const verifiedDatabaseApiBaseUrls = new Set();
const rejectedDatabaseApiBaseUrls = new Set();

function normalizeDatabaseApiBaseUrl(value) {
  const normalized = (value || "").trim().replace(/\/+$/, "");
  return normalized;
}

function readStoredApiBaseUrl() {
  try {
    return window.localStorage.getItem(API_BASE_URL_STORAGE_KEY) || "";
  } catch (_) {
    return "";
  }
}

function writeStoredApiBaseUrl(value) {
  try {
    window.localStorage.setItem(API_BASE_URL_STORAGE_KEY, value);
  } catch (_) {
    // Some browser contexts disable local storage. Runtime failover still works.
  }
}

function normalizeLanguage(value) {
  const normalized = String(value || "").trim().toLowerCase();
  return normalized.startsWith("ms") ? "ms" : "en";
}

function currentLanguageBundle() {
  return I18N[activeLanguage] || I18N.en;
}

function currentPageTitles() {
  return currentLanguageBundle().pageTitles;
}

function setTextContent(selector, text) {
  const element = document.querySelector(selector);
  if (element) {
    element.textContent = text;
  }
}

function setTranslatedPlaceholder(element, sourceText) {
  if (element) {
    element.placeholder = translateWebTextNodeValue(sourceText);
  }
}

function translateWebTextNodeValue(sourceText) {
  if (activeLanguage !== "ms") {
    return sourceText;
  }
  const leading = sourceText.match(/^\s*/)?.[0] || "";
  const trailing = sourceText.match(/\s*$/)?.[0] || "";
  const trimmed = sourceText.trim();
  if (!trimmed) {
    return sourceText;
  }

  let translated = WEB_MS_EXACT[trimmed] || trimmed;
  if (translated === trimmed) {
    WEB_MS_REPLACEMENTS.forEach(([source, target]) => {
      translated = translated.replace(
        new RegExp(`\\b${source.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}\\b`, "gi"),
        target,
      );
    });
  }
  return `${leading}${translated}${trailing}`;
}

function shouldTranslateTextNode(node) {
  const parent = node.parentElement;
  if (!parent || !node.nodeValue || !node.nodeValue.trim()) {
    return false;
  }
  return !["SCRIPT", "STYLE", "OPTION", "INPUT", "TEXTAREA"].includes(
    parent.tagName,
  );
}

function translateVisibleTextNodes(root = document.body) {
  const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
  while (walker.nextNode()) {
    const node = walker.currentNode;
    if (!shouldTranslateTextNode(node)) {
      continue;
    }

    const state = webTextNodeState.get(node);
    const current = node.nodeValue || "";
    const source = state && current === state.lastValue ? state.source : current;
    const nextValue = translateWebTextNodeValue(source);
    webTextNodeState.set(node, { source, lastValue: nextValue });
    node.nodeValue = nextValue;
  }
}

function applyStaticLanguage() {
  const bundle = currentLanguageBundle();
  document.documentElement.lang = activeLanguage;

  Object.entries(bundle.nav).forEach(([pageId, labels]) => {
    const item = document.querySelector(`[data-page-target="${pageId}"]`);
    const title = item?.querySelector(".nav-copy strong");
    const subtitle = item?.querySelector(".nav-copy small");
    if (title) {
      title.textContent = labels[0];
    }
    if (subtitle) {
      subtitle.textContent = labels[1];
    }
  });

  setTextContent("#enter-admin", bundle.ui.enterDashboard);
  setTextContent(".sidebar-logout-copy strong", bundle.ui.logout);
  setTextContent(".sidebar-logout-copy small", bundle.ui.logoutHint);
  setTextContent("#refresh-cloud-data-button", bundle.ui.refreshCloudData);
  setTextContent(".login-card h2", bundle.ui.signInTitle);
  setTextContent(".login-card .support-copy", bundle.ui.signInSubtitle);
  setTextContent('label[for="admin-email"]', bundle.ui.emailLabel);
  setTextContent('label[for="admin-password"]', bundle.ui.passwordLabel);
  setTextContent('label[for="login-language-select"]', bundle.ui.languageLabel);
  setTextContent(".topbar .eyebrow", bundle.ui.topbarEyebrow);
  setTranslatedPlaceholder(managedUserPassword, "Set user password");
  setTranslatedPlaceholder(managedUserPasswordConfirm, "Re-enter password");
  setTranslatedPlaceholder(settingsEsp32PasswordInput, "Keep current password");
  setTranslatedPlaceholder(settingsEsp32CamPasswordInput, "Keep current password");

  const activePageId =
    document.querySelector(".page.is-active")?.dataset.page || "dashboard";
  if (pageTitle && bundle.pageTitles[activePageId]) {
    pageTitle.textContent = bundle.pageTitles[activePageId];
  }
  translateVisibleTextNodes();
}

function readStoredLanguage() {
  try {
    return normalizeLanguage(window.localStorage.getItem(LANGUAGE_STORAGE_KEY));
  } catch (_) {
    return "en";
  }
}

function writeStoredLanguage(value) {
  activeLanguage = normalizeLanguage(value);
  try {
    window.localStorage.setItem(LANGUAGE_STORAGE_KEY, activeLanguage);
  } catch (_) {
    // Keep language in memory if local storage is unavailable.
  }
  languageSelects.forEach((select) => {
    select.value = activeLanguage;
  });
  applyStaticLanguage();
}

function withLanguageQuery(path) {
  if (!path.startsWith("/api/") && path !== "/analyze") {
    return path;
  }
  const separator = path.includes("?") ? "&" : "?";
  return `${path}${separator}lang=${encodeURIComponent(activeLanguage)}`;
}

function sameOriginApiBaseUrl() {
  if (window.location.protocol === "http:" || window.location.protocol === "https:") {
    return window.location.origin;
  }
  return "";
}

function localMachineApiBaseUrl() {
  const host = window.location.hostname;
  if (
    window.location.protocol === "file:" ||
    host === "localhost" ||
    host === "127.0.0.1" ||
    host === "::1"
  ) {
    return LOCAL_MACHINE_API_BASE_URL;
  }
  return "";
}

function apiBaseUrlFromQuery() {
  try {
    const params = new URLSearchParams(window.location.search);
    return params.get("apiBaseUrl") || params.get("api_base_url") || "";
  } catch (_) {
    return "";
  }
}

function localApiDiscoveryEnabled() {
  if (window.PINEGUARD_ENABLE_LOCAL_API_DISCOVERY === true) {
    return true;
  }
  try {
    const params = new URLSearchParams(window.location.search);
    return params.get(LOCAL_API_DISCOVERY_QUERY_KEY) === "1";
  } catch (_) {
    return false;
  }
}

function generatedApiBaseUrlCandidates() {
  const urls = [DEFAULT_DATABASE_API_BASE_URL];
  urls.push(window.PINEGUARD_API_BASE_URL);
  if (Array.isArray(window.PINEGUARD_API_BASE_URLS)) {
    urls.push(...window.PINEGUARD_API_BASE_URLS);
  }
  return urls;
}

function browserCanRequestApiBaseUrl(value, { explicit = false } = {}) {
  if (explicit || window.location.protocol !== "https:") {
    return true;
  }
  try {
    const url = new URL(value);
    return url.protocol !== "http:";
  } catch (_) {
    return false;
  }
}

function stableHostnameApiBaseUrls() {
  return [
    "http://pineguard-backend:8000",
    "http://pineguard-backend.local:8000",
  ];
}

function pageHostApiBaseUrls() {
  if (window.location.protocol !== "http:" && window.location.protocol !== "https:") {
    return [];
  }
  const host = window.location.hostname;
  if (!host) {
    return [];
  }
  const urls = [sameOriginApiBaseUrl()];
  if (!["localhost", "127.0.0.1", "::1"].includes(host)) {
    urls.push(`http://${host}:8000`);
  }
  return urls;
}

function databaseApiBaseUrlCandidates() {
  const generatedCandidates = generatedApiBaseUrlCandidates();
  const queryCandidate = apiBaseUrlFromQuery();
  const storedCandidate = readStoredApiBaseUrl();
  const candidates = [
    queryCandidate,
    storedCandidate,
    ...generatedCandidates,
    ...pageHostApiBaseUrls(),
    localMachineApiBaseUrl(),
    ...stableHostnameApiBaseUrls(),
    ...(localApiDiscoveryEnabled() ? [sameOriginApiBaseUrl()] : []),
    DEFAULT_DATABASE_API_BASE_URL,
  ];

  const normalized = [];
  candidates.forEach((candidate) => {
    const value = normalizeDatabaseApiBaseUrl(candidate);
    const explicit = value === normalizeDatabaseApiBaseUrl(queryCandidate);
    if (
      value &&
      browserCanRequestApiBaseUrl(value, { explicit }) &&
      !normalized.includes(value)
    ) {
      normalized.push(value);
    }
  });
  return normalized;
}

let API_BASE_URL = databaseApiBaseUrlCandidates()[0] || DEFAULT_DATABASE_API_BASE_URL;
let adminToken = window.localStorage.getItem("pineguardAdminToken") || "";
let activeLanguage = readStoredLanguage();
let latestAdminSummary = null;
let latestAdminRecords = [];
let latestAdminRecordReviews = [];
let latestEntityCoverage = null;
let latestProfileSettings = { ...DEFAULT_PROFILE_SETTINGS };
let latestAlertRuleSettings = { ...DEFAULT_ALERT_RULE_SETTINGS };
let latestIotWifiSettings = null;
let activeRecordFilter = "all";
let selectedRecordId = "";
let analysisStatusTimer = null;
let cloudRefreshTimer = null;
let cloudRefreshInFlight = false;
let profileSettingsAutosaveTimer = null;
let activeCloudRefreshIntervalMs = LIVE_REFRESH_INTERVAL_MS;

writeStoredLanguage(activeLanguage);

function setLoginFeedback(message, tone = "error") {
  if (!loginFeedback) {
    return;
  }
  loginFeedback.textContent = message || "";
  loginFeedback.classList.toggle("is-success", tone === "success");
  loginFeedback.classList.toggle("is-info", tone === "info");
}

function setBackendDisconnectedState(message) {
  if (monitorBackendStatus) {
    monitorBackendStatus.textContent = "Unavailable";
  }
  if (monitorBackendNote) {
    monitorBackendNote.textContent = message;
  }
  if (settingsConfigStatus) {
    settingsConfigStatus.textContent = "Service unavailable";
    settingsConfigStatus.classList.add("warning");
  }
}

function setCloudRefreshState(label, isBusy = false) {
  if (!refreshCloudDataButton) {
    return;
  }
  refreshCloudDataButton.disabled = isBusy;
  refreshCloudDataButton.textContent = label;
  refreshCloudDataButton.classList.toggle("is-syncing", isBusy);
}

function markLiveUpdate(element) {
  if (!element) {
    return;
  }

  const shell = element.closest(
    ".dashboard-ribbon article, .dashboard-mini-card, .lens-center, .hero-signal > div, .mini-kpis article, .capsule",
  );
  element.classList.remove(LIVE_VALUE_UPDATE_CLASS);
  shell?.classList.remove(LIVE_VALUE_SHELL_CLASS);
  void element.offsetWidth;
  element.classList.add(LIVE_VALUE_UPDATE_CLASS);
  shell?.classList.add(LIVE_VALUE_SHELL_CLASS);
  window.setTimeout(() => {
    element.classList.remove(LIVE_VALUE_UPDATE_CLASS);
    shell?.classList.remove(LIVE_VALUE_SHELL_CLASS);
  }, 900);
}

function setLiveText(element, value, { animate = true } = {}) {
  if (!element) {
    return;
  }

  const nextValue = String(value ?? "");
  if (element.textContent === nextValue) {
    return;
  }

  const hadExistingValue = element.textContent.trim().length > 0;
  element.textContent = nextValue;
  if (animate && hadExistingValue) {
    markLiveUpdate(element);
  }
}

function startCloudRefreshLoop() {
  clearInterval(cloudRefreshTimer);
  cloudRefreshTimer = window.setInterval(async () => {
    if (!adminToken || adminView?.classList.contains("is-hidden")) {
      return;
    }
    try {
      await hydrateAdminFromBackend({ silent: true });
    } catch (_) {
      // Keep the current screen visible; status cards show connectivity.
    }
  }, activeCloudRefreshIntervalMs);
}

function setButtonBusy(button, label, isBusy) {
  if (!button) {
    return;
  }
  if (!button.dataset.idleLabel) {
    button.dataset.idleLabel = button.textContent || "";
  }
  button.disabled = isBusy;
  button.textContent = isBusy ? label : button.dataset.idleLabel;
}

function requestRemoveUserPassword(user) {
  if (!removeUserDialog || !removeUserForm || !removeUserPasswordInput) {
    return Promise.resolve(window.prompt(`Enter admin password to remove ${user.displayName}.`) || "");
  }

  return new Promise((resolve) => {
    if (removeUserTitle) {
      removeUserTitle.textContent = `Remove ${user.displayName}`;
    }
    if (removeUserMessage) {
      removeUserMessage.textContent =
        `${user.email} will lose active Web Admin or mobile access after this password confirmation.`;
    }
    removeUserPasswordInput.value = "";

    const cleanup = () => {
      removeUserForm.removeEventListener("submit", handleSubmit);
      cancelRemoveUserButton?.removeEventListener("click", handleCancel);
      removeUserDialog.removeEventListener("cancel", handleCancel);
    };
    const handleCancel = (event) => {
      event?.preventDefault();
      cleanup();
      removeUserDialog.close();
      resolve("");
    };
    const handleSubmit = (event) => {
      event.preventDefault();
      const password = removeUserPasswordInput.value;
      cleanup();
      removeUserDialog.close();
      resolve(password);
    };

    removeUserForm.addEventListener("submit", handleSubmit);
    cancelRemoveUserButton?.addEventListener("click", handleCancel);
    removeUserDialog.addEventListener("cancel", handleCancel);
    removeUserDialog.showModal();
    removeUserPasswordInput.focus();
  });
}

function numberFromControl(input, fallback) {
  if (!input) {
    return fallback;
  }
  const value = Number(input.value);
  return Number.isFinite(value) ? value : fallback;
}

function checkboxFromControl(input, fallback) {
  return input ? input.checked : fallback;
}

function refreshIntervalForCadence(value) {
  switch ((value || "").toString().trim().toLowerCase()) {
    case "30s":
      return 30000;
    case "60s":
      return 60000;
    case "hourly":
      return 60 * 60 * 1000;
    case "realtime":
    default:
      return LIVE_REFRESH_INTERVAL_MS;
  }
}

function applyCloudRefreshCadence(value) {
  const nextInterval = refreshIntervalForCadence(value);
  if (activeCloudRefreshIntervalMs === nextInterval) {
    return;
  }
  activeCloudRefreshIntervalMs = nextInterval;
  if (adminToken && adminView && !adminView.classList.contains("is-hidden")) {
    startCloudRefreshLoop();
  }
}

function setActivePage(pageId) {
  const navPageId = pageId === "detail" ? "records" : pageId;
  if (pageId === "detail") {
    renderSelectedRecordDetail();
  }

  navItems.forEach((item) => {
    item.classList.toggle("is-active", item.dataset.pageTarget === navPageId);
  });

  pages.forEach((page) => {
    page.classList.toggle("is-active", page.dataset.page === pageId);
  });

  const pageTitles = currentPageTitles();
  if (pageTitle && pageTitles[pageId]) {
    pageTitle.textContent = pageTitles[pageId];
  }
}

async function apiRequest(path, options = {}) {
  const headers = {
    Accept: "application/json",
    ...(options.headers || {}),
  };
  if (adminToken) {
    headers.Authorization = `Bearer ${adminToken}`;
  }

  const response = await fetchFromApi(withLanguageQuery(path), {
    ...options,
    headers,
  });

  if (!response.ok) {
    const payload = await response.json().catch(() => ({}));
    const message =
      payload.detail || payload.message || `Request failed with ${response.status}`;
    throw new Error(message);
  }

  return response.json();
}

function persistActiveApiBaseUrl(value) {
  API_BASE_URL = value;
  writeStoredApiBaseUrl(value);
  if (settingsBackendUrl) {
    settingsBackendUrl.textContent = "Standard route";
  }
}

function readableFetchError(error) {
  const message = error?.message?.toString() || "";
  return message && message !== "Failed to fetch"
    ? message
    : "network request failed";
}

async function fetchFromApi(path, options = {}) {
  const candidates = [];
  [API_BASE_URL, ...databaseApiBaseUrlCandidates()].forEach((candidate) => {
    const value = normalizeDatabaseApiBaseUrl(candidate);
    if (value && !candidates.includes(value)) {
      candidates.push(value);
    }
  });

  const failures = [];
  for (const candidate of candidates) {
    try {
      await verifyDatabaseApiBaseUrl(candidate);
      const timeoutMs =
        options.body instanceof FormData ? API_UPLOAD_TIMEOUT_MS : API_DISCOVERY_TIMEOUT_MS;
      const response = await fetchWithBackendTimeout(`${candidate}${path}`, options, timeoutMs);
      persistActiveApiBaseUrl(candidate);
      return response;
    } catch (error) {
      failures.push(`${candidate} (${readableFetchError(error)})`);
    }
  }

  throw new Error(
    failures.length
      ? "Unable to reach PineGuard service. Check the network connection and try again."
      : "Unable to reach PineGuard service.",
  );
}

async function verifyDatabaseApiBaseUrl(candidate) {
  const baseUrl = normalizeDatabaseApiBaseUrl(candidate);
  if (!baseUrl) {
    throw new Error("empty service route");
  }
  if (verifiedDatabaseApiBaseUrls.has(baseUrl)) {
    return;
  }
  if (rejectedDatabaseApiBaseUrls.has(baseUrl)) {
    throw new Error("not a PineGuard service");
  }

  const response = await fetchWithBackendTimeout(`${baseUrl}/health`, {
    headers: { Accept: "application/json" },
  }, API_HEALTH_TIMEOUT_MS);
  if (!response.ok) {
    if (response.status === 404 || response.status === 405) {
      rejectedDatabaseApiBaseUrls.add(baseUrl);
    }
    throw new Error(`health check failed with ${response.status}`);
  }

  const payload = await response.json().catch(() => ({}));
  const hasDatabaseHealth =
    payload?.backend === "running" &&
    typeof payload?.database === "string" &&
    payload?.databaseDetails &&
    typeof payload.databaseDetails === "object" &&
    typeof payload?.analysisLayer === "string";
  if (!hasDatabaseHealth) {
    rejectedDatabaseApiBaseUrls.add(baseUrl);
    throw new Error("not a PineGuard service");
  }

  verifiedDatabaseApiBaseUrls.add(baseUrl);
}

async function fetchWithBackendTimeout(url, options = {}, timeoutMs = API_DISCOVERY_TIMEOUT_MS) {
  const controller = new AbortController();
  const timeoutId = window.setTimeout(() => {
    controller.abort();
  }, timeoutMs);

  try {
    return await fetch(url, {
      ...options,
      signal: controller.signal,
    });
  } finally {
    window.clearTimeout(timeoutId);
  }
}

async function readBackendHealth() {
  const response = await fetchFromApi("/health", {
    headers: { Accept: "application/json" },
  });

  let payload = {};
  try {
    payload = await response.json();
  } catch (_) {
    payload = {};
  }

  if (!response.ok) {
    throw new Error(
        payload.detail ||
        payload.message ||
        `Service check failed with ${response.status}`,
    );
  }

  if (payload.backend !== "running") {
    throw new Error("PineGuard service is not available.");
  }

  if (payload.status && !["ok", "degraded"].includes(payload.status)) {
    throw new Error(`PineGuard service status is ${payload.status}.`);
  }

  return payload;
}

function formatDateTime(ms) {
  if (!ms) {
    return "No recent sync";
  }
  return new Date(ms).toLocaleString();
}

function recordScanResult(record) {
  const scanResult = record?.scanResult && typeof record.scanResult === "object"
    ? { ...record.scanResult }
    : {};
  const localizedScanResult = record?.localized?.scanResult;
  if (localizedScanResult && typeof localizedScanResult === "object") {
    return { ...scanResult, ...localizedScanResult };
  }
  return scanResult;
}

function recordTitle(record) {
  return record?.localized?.title || record?.title || "";
}

function recordDescription(record) {
  return record?.localized?.description || record?.description || "";
}

function recordTreatmentSteps(record) {
  const localizedSteps = record?.localized?.treatment?.steps;
  if (Array.isArray(localizedSteps) && localizedSteps.length > 0) {
    return localizedSteps;
  }
  return record?.treatment?.steps || [];
}

function jobScanResult(job) {
  const scanResult = job?.scanResult && typeof job.scanResult === "object"
    ? { ...job.scanResult }
    : {};
  const localizedScanResult = job?.localized?.scanResult;
  if (localizedScanResult && typeof localizedScanResult === "object") {
    return { ...scanResult, ...localizedScanResult };
  }
  return scanResult;
}

function jobLiveAssessment(job) {
  const liveAssessment = job?.liveAssessment && typeof job.liveAssessment === "object"
    ? { ...job.liveAssessment }
    : {};
  const localizedLiveAssessment = job?.localized?.liveAssessment;
  if (localizedLiveAssessment && typeof localizedLiveAssessment === "object") {
    return { ...liveAssessment, ...localizedLiveAssessment };
  }
  return liveAssessment;
}

function alertMessage(alert) {
  return alert?.localized?.message || alert?.message || "";
}

function recordImageUrl(record) {
  return recordImageSources(record)[0] || "";
}

function recordImageSources(record) {
  const scanResult = recordScanResult(record);
  const candidates = [
    scanResult.imageUri,
    scanResult.imageUrl,
    scanResult.image_url,
    record?.imageUrl,
    scanResult.imagePreviewDataUri,
    scanResult.imageDataUri,
    record?.imagePreviewDataUri,
  ];
  const normalized = [];
  candidates.forEach((value) => {
    const source = typeof value === "string" ? value.trim() : "";
    if (source && !normalized.includes(source)) {
      normalized.push(source);
    }
  });
  return normalized;
}

function recordImageSourceLabel(record) {
  const imageUrl = recordImageUrl(record).toLowerCase();
  if (!imageUrl) {
    return record?.type === "scan" ? "Image pending" : "No scan image";
  }
  if (imageUrl.startsWith("data:image/")) {
    return translateWebTextNodeValue("Stored preview");
  }
  if (imageUrl.includes("/camera/") || imageUrl.includes("camera")) {
    return "Field camera image";
  }
  if (imageUrl.includes("analysis_jobs")) {
    return "Imported image";
  }
  return "Scan image";
}

function formatConfidencePercent(value) {
  const confidence = Number(value || 0);
  return confidence > 0 ? `${Math.round(confidence * 100)}% confidence` : "";
}

function markImageUnavailable(imageElement) {
  if (!imageElement) {
    return;
  }
  const frame = imageElement.closest(".record-media, .media-frame, .image-stage");
  const unavailableLabel = frame?.querySelector(".record-media-label");
  imageElement.hidden = true;
  imageElement.removeAttribute("src");
  frame?.classList.remove("has-image");
  frame?.classList.add("is-empty", "is-unavailable");
  if (unavailableLabel) {
    unavailableLabel.textContent = translateWebTextNodeValue("Image unavailable");
  }
  if (imageElement === detailImage && detailImageCaption) {
    detailImageCaption.textContent = translateWebTextNodeValue("Image unavailable");
  }
}

function normalizeImageSourceList(imageSource) {
  const values = Array.isArray(imageSource) ? imageSource : [imageSource];
  const normalized = [];
  values.forEach((value) => {
    const source = typeof value === "string" ? value.trim() : "";
    if (source && !normalized.includes(source)) {
      normalized.push(source);
    }
  });
  return normalized;
}

function setImageElementSource(imageElement, imageUrl, altText) {
  if (!imageElement) {
    return;
  }
  const sources = normalizeImageSourceList(imageUrl);
  let sourceIndex = 0;
  const applySource = (source) => {
    imageElement.src = source;
    imageElement.alt = altText;
    imageElement.hidden = false;
  };
  imageElement.onload = () => {
    if (imageElement.naturalWidth > 0) {
      const frame = imageElement.closest(".record-media, .media-frame, .image-stage");
      frame?.classList.add("has-image");
      frame?.classList.remove("is-empty", "is-unavailable");
    }
  };
  imageElement.onerror = () => {
    sourceIndex += 1;
    if (sourceIndex < sources.length) {
      applySource(sources[sourceIndex]);
      return;
    }
    markImageUnavailable(imageElement);
  };
  if (sources.length > 0) {
    applySource(sources[0]);
    return;
  }
  imageElement.removeAttribute("src");
  imageElement.alt = altText;
  imageElement.hidden = true;
}

function ensureMediaFrameImage(container, altText) {
  if (!container) {
    return null;
  }
  let imageElement = container.querySelector(".media-frame-img");
  if (!imageElement) {
    imageElement = document.createElement("img");
    imageElement.className = "media-frame-img";
    imageElement.alt = altText;
    container.prepend(imageElement);
  }
  return imageElement;
}

function updateMediaFrame(container, imageUrl, altText) {
  if (!container) {
    return;
  }
  const imageElement = ensureMediaFrameImage(container, altText);
  const sources = normalizeImageSourceList(imageUrl);
  setImageElementSource(imageElement, sources, altText);
  container.classList.toggle("has-image", sources.length > 0);
  container.classList.toggle("is-empty", sources.length === 0);
}

function applySummary(summary) {
  if (!summary) {
    return;
  }

  const latestReading = summary.nodes?.latestReading;
  const latestFrame = summary.nodes?.latestFrame;

  if (heroSignalValues.length >= 3) {
    setLiveText(heroSignalValues[0], "Service live");
    setLiveText(
      heroSignalValues[1],
      summary.nodes?.online > 0 ? "Online" : "Waiting",
    );
    setLiveText(heroSignalValues[2], `${summary.analysisJobs?.queued || 0} jobs`);
  }

  if (topbarSyncValue) {
    setLiveText(
      topbarSyncValue,
      summary.generatedAt ? new Date(summary.generatedAt).toLocaleTimeString() : "--:--",
    );
  }

  if (latestReading && dashboardRibbonValues.length >= 4) {
    setLiveText(dashboardRibbonValues[0], `${latestReading.temperature.toFixed(1)} deg C`);
    setLiveText(dashboardRibbonValues[1], `${latestReading.humidity.toFixed(0)}%`);
    setLiveText(dashboardRibbonValues[2], `${latestReading.soilMoisture.toFixed(0)}%`);
    setLiveText(dashboardRibbonValues[3], `${latestReading.pH.toFixed(1)}`);
  }

  if (dashboardMiniCardValues.length >= 3) {
    setLiveText(dashboardMiniCardValues[0], String(summary.alerts?.unread || 0));
    setLiveText(dashboardMiniCardValues[1], String(summary.analysisJobs?.queued || 0));
    setLiveText(dashboardMiniCardValues[2], `${summary.records?.total || 0} total`);
  }

  if (dashboardMiniCardNotes.length >= 3) {
    setLiveText(
      dashboardMiniCardNotes[0],
      `${summary.users?.farmers || 0} farmer accounts active`,
    );
    setLiveText(
      dashboardMiniCardNotes[1],
      `${summary.analysisJobs?.completed || 0} completed jobs`,
    );
    setLiveText(
      dashboardMiniCardNotes[2],
      latestFrame?.capturedAt
        ? `Latest frame ${formatDateTime(latestFrame.capturedAt)}`
        : "No frame history yet",
    );
  }

  if (lensCenterValue) {
    const healthScore = latestReading?.status === "critical"
      ? 48
      : latestReading?.status === "warning"
        ? 72
        : summary.nodes?.online > 0
          ? 94
          : 36;
    setLiveText(lensCenterValue, `${healthScore} / 100`);
  }
}

function renderUserDirectory(users) {
  if (!userDirectoryBody) {
    return;
  }

  if (!users || users.length === 0) {
    return;
  }

  userDirectoryBody.innerHTML = "";
  users.forEach((user) => {
    const role = normalizeManagedUserRole(user.role);
    const row = document.createElement("tr");
    row.innerHTML = `
      <td>${user.displayName}<br /><small>${user.email}</small></td>
      <td>${role.label}</td>
      <td>${user.location}</td>
      <td>${role.scope || user.accessScope}</td>
      <td>${user.lastActiveAt ? new Date(user.lastActiveAt).toLocaleString() : "No activity yet"}</td>
      <td><span class="pill ${user.accountStatus === "active" ? "accent" : ""}">${user.accountStatus}</span></td>
      <td>
        <button class="table-action-button danger" type="button" data-remove-user-id="${user.id}">
          Remove
        </button>
      </td>
    `;
    row
      .querySelector("[data-remove-user-id]")
      ?.addEventListener("click", (event) => removeManagedUser(user, event.currentTarget));
    userDirectoryBody.appendChild(row);
  });

  if (managedUserCount) {
    managedUserCount.textContent = String(users.length).padStart(2, "0");
  }
}

async function removeManagedUser(user, button) {
  if (!adminToken) {
    window.alert("Sign in as admin before removing users.");
    return;
  }

  const adminPassword = await requestRemoveUserPassword(user);
  if (!adminPassword) {
    return;
  }

  setButtonBusy(button, "Removing...", true);
  try {
    await apiRequest(`/api/admin/users/${encodeURIComponent(user.id)}`, {
      method: "DELETE",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ adminPassword }),
    });
    await hydrateAdminFromBackend();
  } catch (error) {
    window.alert(error.message || "Unable to remove this user.");
  } finally {
    setButtonBusy(button, "", false);
  }
}

function normalizeManagedUserRole(value) {
  const role = (value || "").toString().trim().toLowerCase();
  if (role === "admin" || role === "officer" || role === "staff") {
    return {
      value: "admin",
      label: "Admin",
      scope: "Access control, records, approvals, and system oversight",
    };
  }
  return {
    value: "farmer",
    label: "Farmer",
    scope: "Monitoring, alerts, scan submission, and field activity logs",
  };
}

function reviewStatusPillClass(value) {
  if (value === "needs review" || value === "pending") {
    return "warning";
  }
  if (value === "assigned" || value === "reviewed") {
    return "success";
  }
  return "neutral";
}

function reviewForRecord(record, reviews = latestAdminRecordReviews) {
  return (reviews || []).find((item) => item.recordId === record?.id);
}

function reviewStatusForRecord(record, reviews = latestAdminRecordReviews) {
  const review = reviewForRecord(record, reviews);
  return review?.reviewStatus
    ? review.reviewStatus
    : record?.type === "scan"
      ? "Needs review"
      : "Logged";
}

function normalizeRecordFilter(value) {
  const normalized = (value || "all").toString().trim().toLowerCase();
  return ["all", "scan", "activity", "needs-review"].includes(normalized)
    ? normalized
    : "all";
}

function recordMatchesActiveFilter(record, reviews = latestAdminRecordReviews) {
  const filter = normalizeRecordFilter(activeRecordFilter);
  if (filter === "all") {
    return true;
  }
  if (filter === "needs-review") {
    const reviewStatus = reviewStatusForRecord(record, reviews).toLowerCase();
    return reviewStatus === "needs review" || reviewStatus === "pending";
  }
  return (record?.type || "").toString().trim().toLowerCase() === filter;
}

function updateRecordFilterButtons() {
  recordFilterButtons.forEach((button) => {
    const selected =
      normalizeRecordFilter(button.dataset.recordFilter) === activeRecordFilter;
    button.classList.toggle("is-active", selected);
    button.classList.toggle("accent", selected);
    button.setAttribute("aria-pressed", selected ? "true" : "false");
  });
}

function updateSelectedRecordCard() {
  if (recordsStream) {
    recordsStream.querySelectorAll(".record-card").forEach((card) => {
      const selected = card.dataset.recordId === selectedRecordId;
      card.classList.toggle("is-selected", selected);
      card.setAttribute("aria-pressed", selected ? "true" : "false");
    });
  }
  if (openSelectedRecordButton) {
    openSelectedRecordButton.disabled = !selectedRecordId;
  }
  if (detailRemoveRecordButton) {
    detailRemoveRecordButton.disabled = !selectedRecordId;
  }
}

function selectRecord(recordId, { openDetail = false } = {}) {
  selectedRecordId = recordId || selectedRecordId;
  renderSelectedRecordDetail();
  updateSelectedRecordCard();
  if (openDetail && selectedRecordId) {
    setActivePage("detail");
    adminView?.scrollIntoView({ behavior: "smooth", block: "start" });
  }
}

function setRecordFilter(value) {
  activeRecordFilter = normalizeRecordFilter(value);
  applyRecordsView(latestAdminRecords, latestAdminRecordReviews, latestAdminSummary);
}

function applyRecordsView(records, reviews, summary) {
  latestAdminRecords = records || [];
  latestAdminRecordReviews = reviews || [];
  latestAdminSummary = summary || latestAdminSummary;
  updateRecordFilterButtons();

  const filteredRecords = latestAdminRecords.filter((record) =>
    recordMatchesActiveFilter(record, latestAdminRecordReviews),
  );
  const latestRecords = filteredRecords.slice(0, 6);
  if (recordsStream) {
    recordsStream.innerHTML = "";
    if (!latestRecords.length) {
      const empty = document.createElement("article");
      empty.className = "record-card";
      empty.innerHTML = `
        <div class="record-body">
          <p class="record-tag">Record stream</p>
          <strong>No records match this filter</strong>
          <p>Choose another filter to widen the submitted record stream.</p>
        </div>
      `;
      recordsStream.appendChild(empty);
    }
    latestRecords.forEach((record, index) => {
      const article = document.createElement("article");
      article.className = "record-card";
      article.dataset.recordId = record.id || "";
      article.tabIndex = 0;
      article.setAttribute("role", "button");
      article.setAttribute("aria-pressed", record.id === selectedRecordId ? "true" : "false");
      const scanResult = recordScanResult(record);
      const imageSources = recordImageSources(record);
      const pillLabel = reviewStatusForRecord(record, latestAdminRecordReviews);

      const media = document.createElement("div");
      media.className = `record-media ${imageSources.length ? "has-image" : "is-empty"}`;
      const icon = document.createElement("span");
      icon.className = "record-media-icon";
      icon.textContent = record.type === "scan" ? "SCAN" : "LOG";
      media.appendChild(icon);
      if (imageSources.length) {
        const image = document.createElement("img");
        image.loading = "lazy";
        media.appendChild(image);
        setImageElementSource(
          image,
          imageSources,
          `${recordTitle(record) || "Scan record"} capture`,
        );
      }
      const mediaLabel = document.createElement("span");
      mediaLabel.className = "record-media-label";
      mediaLabel.textContent = recordImageSourceLabel(record);
      media.appendChild(mediaLabel);

      const body = document.createElement("div");
      body.className = "record-body";
      const tag = document.createElement("p");
      tag.className = "record-tag";
      tag.textContent = record.type || "Record";
      const title = document.createElement("strong");
      title.textContent = `${record.id || `REC-${String(index + 1).padStart(3, "0")}`} - ${recordTitle(record) || "Untitled record"}`;
      const description = document.createElement("p");
      description.textContent = recordDescription(record) || "No description provided.";
      const chipRow = document.createElement("div");
      chipRow.className = "record-chip-row";
      if (scanResult.disease) {
        const diseaseChip = document.createElement("span");
        diseaseChip.className = "record-chip";
        diseaseChip.textContent = scanResult.diseaseDisplayName || diseaseDisplayName(scanResult.disease);
        chipRow.appendChild(diseaseChip);
      }
      const confidenceLabel = formatConfidencePercent(scanResult.confidence);
      if (confidenceLabel) {
        const confidenceChip = document.createElement("span");
        confidenceChip.className = "record-chip";
        confidenceChip.textContent = confidenceLabel;
        chipRow.appendChild(confidenceChip);
      }
      body.append(tag, title, description);
      if (chipRow.childElementCount) {
        body.appendChild(chipRow);
      }

      const meta = document.createElement("div");
      meta.className = "record-meta";
      const time = document.createElement("span");
      time.textContent = formatDateTime(record.timestamp);
      const pill = document.createElement("span");
      pill.className = `pill ${reviewStatusPillClass((pillLabel || "").toLowerCase())}`;
      pill.textContent = pillLabel;
      const actions = document.createElement("div");
      actions.className = "record-actions";
      const removeButton = document.createElement("button");
      removeButton.className = "table-action-button danger record-remove-button";
      removeButton.type = "button";
      removeButton.textContent = translateWebTextNodeValue("Remove");
      removeButton.addEventListener("click", (event) => {
        event.stopPropagation();
        removeRecord(record, removeButton);
      });
      actions.appendChild(removeButton);
      meta.append(time, pill, actions);

      article.append(media, body, meta);
      article.addEventListener("click", () => selectRecord(record.id, { openDetail: true }));
      article.addEventListener("keydown", (event) => {
        if (event.key === "Enter" || event.key === " ") {
          event.preventDefault();
          selectRecord(record.id, { openDetail: true });
        }
      });
      recordsStream.appendChild(article);
    });
  }

  if (recordsMixList) {
    const scanCount = filteredRecords.filter((item) => item.type === "scan").length;
    const activityCount = filteredRecords.filter((item) => item.type === "activity").length;
    const reviewCount = filteredRecords.filter((item) =>
      recordMatchesActiveFilter(item, latestAdminRecordReviews) &&
      reviewStatusForRecord(item, latestAdminRecordReviews).toLowerCase() !== "logged",
    ).length;
    const total = Math.max(1, filteredRecords.length);
    recordsMixList.innerHTML = `
      <li><strong>${Math.round((scanCount / total) * 100)}%</strong> scan records</li>
      <li><strong>${Math.round((activityCount / total) * 100)}%</strong> activity logs</li>
      <li><strong>${Math.round((reviewCount / total) * 100)}%</strong> review notes</li>
    `;
  }

  const selectedRecord =
    filteredRecords.find((record) => record.id === selectedRecordId) ||
    filteredRecords[0] ||
    latestAdminRecords[0];
  if (!selectedRecord) {
    selectedRecordId = "";
    updateSelectedRecordCard();
    return;
  }
  selectedRecordId = selectedRecord.id || selectedRecordId;
  renderSelectedRecordDetail();
  updateSelectedRecordCard();
}

function renderSelectedRecordDetail() {
  const selectedRecord =
    latestAdminRecords.find((record) => record.id === selectedRecordId) ||
    latestAdminRecords[0];
  if (!selectedRecord) {
    if (detailRecordTitle) {
      detailRecordTitle.textContent = "No selected record";
    }
    if (detailRemoveRecordButton) {
      detailRemoveRecordButton.disabled = true;
    }
    setImageElementSource(detailImage, "", "Selected scan capture");
    return;
  }

  selectedRecordId = selectedRecord.id || selectedRecordId;
  const selectedReview = reviewForRecord(selectedRecord, latestAdminRecordReviews);
  const scanResult = recordScanResult(selectedRecord);
  const selectedImageSources = recordImageSources(selectedRecord);
  const treatmentSteps = recordTreatmentSteps(selectedRecord);
  if (detailRecordTitle) {
    detailRecordTitle.textContent = `${selectedRecord.id} - review the diagnosis, environment, and follow-up context.`;
  }
  if (detailRemoveRecordButton) {
    detailRemoveRecordButton.disabled = !selectedRecord.id;
  }
  if (detailImageCaption) {
    detailImageCaption.textContent = selectedImageSources.length
      ? recordImageSourceLabel(selectedRecord)
      : "No image";
  }
  if (detailImageLabel) {
    detailImageLabel.textContent = scanResult.diseaseDisplayName || diseaseDisplayName(scanResult.disease || selectedRecord.type);
  }
  if (detailImageStage) {
    detailImageStage.classList.toggle("has-image", selectedImageSources.length > 0);
    detailImageStage.classList.toggle("is-empty", selectedImageSources.length === 0);
    detailImageStage.style.backgroundImage = "";
  }
  setImageElementSource(
    detailImage,
    selectedImageSources,
    `${recordTitle(selectedRecord) || "Selected record"} capture`,
  );
  if (detailImage) {
    detailImage.loading = "eager";
  }
  if (detailConfidencePill) {
    const confidence = scanResult.confidence
      ? `${Math.round(scanResult.confidence * 100)}% confidence`
      : "No AI confidence";
    detailConfidencePill.textContent = confidence;
  }
  if (detailPredictedCondition) {
    detailPredictedCondition.textContent = scanResult.diseaseDisplayName || diseaseDisplayName(scanResult.disease || selectedRecord.type);
  }
  if (detailSuggestedAction) {
    detailSuggestedAction.textContent = treatmentSteps[0] || "Review the record and choose the next operational action.";
  }
  if (detailReviewerNote) {
    detailReviewerNote.textContent =
      selectedReview?.reviewNote ||
      "No admin review note has been stored for this record yet.";
  }

  const latestReading = latestAdminSummary?.nodes?.latestReading;
  if (detailTemperature) {
    setLiveText(
      detailTemperature,
      latestReading ? `${latestReading.temperature.toFixed(1)} deg C` : "--",
    );
  }
  if (detailHumidity) {
    setLiveText(
      detailHumidity,
      latestReading ? `${latestReading.humidity.toFixed(0)}%` : "--",
    );
  }
  if (detailSoilMoisture) {
    setLiveText(
      detailSoilMoisture,
      latestReading ? `${latestReading.soilMoisture.toFixed(0)}%` : "--",
    );
  }
  if (detailPh) {
    setLiveText(detailPh, latestReading ? `${latestReading.pH.toFixed(1)}` : "--");
  }

  renderTimeline(
    detailReviewThread,
    [
      {
        time: formatDateTime(selectedRecord.timestamp),
        title: `${recordTitle(selectedRecord) || "Record submitted"}`,
        note: recordDescription(selectedRecord) || "Stored in the records history.",
      },
      selectedReview
        ? {
            time: formatDateTime(selectedReview.reviewedAt),
            title: `Review status: ${selectedReview.reviewStatus}`,
            note:
              selectedReview.reviewNote ||
              "Admin review was stored without a note.",
          }
        : {
            time: "Pending",
            title: "Review pending",
            note: "No review entry has been stored for this record yet.",
          },
      latestReading
        ? {
            time: formatDateTime(latestReading.timestamp),
            title: "Latest linked sensor snapshot",
            note: `Temperature ${latestReading.temperature.toFixed(1)} deg C, humidity ${latestReading.humidity.toFixed(0)}%, soil moisture ${latestReading.soilMoisture.toFixed(0)}%, pH ${latestReading.pH.toFixed(1)}.`,
          }
        : {
            time: "Pending",
            title: "Sensor context pending",
            note: "No live sensor reading is currently available.",
          },
    ],
  );
}

async function removeRecord(record, button) {
  const recordId = record?.id || selectedRecordId;
  if (!recordId) {
    return;
  }
  if (!adminToken) {
    window.alert("Sign in before removing records.");
    return;
  }

  const title = recordTitle(record) || recordId;
  const confirmed = window.confirm(
    `Remove "${title}" from Submitted Records? This also removes its review and treatment entries.`,
  );
  if (!confirmed) {
    return;
  }

  setButtonBusy(button || detailRemoveRecordButton, "Removing...", true);
  try {
    await apiRequest(`/api/records/${encodeURIComponent(recordId)}`, {
      method: "DELETE",
    });
    selectedRecordId = "";
    await hydrateAdminFromBackend({ silent: true });
    setActivePage("records");
  } catch (error) {
    window.alert(error.message || "Unable to remove this record.");
  } finally {
    setButtonBusy(button || detailRemoveRecordButton, "", false);
  }
}

async function hydrateAdminFromBackend({ silent = false } = {}) {
  if (cloudRefreshInFlight) {
    return latestAdminSummary;
  }
  cloudRefreshInFlight = true;
  if (!silent) {
    setCloudRefreshState(translateWebTextNodeValue("Syncing..."), true);
  }
  try {
    const [
      summaryPayload,
      usersPayload,
      jobsPayload,
      alertsPayload,
      recordsPayload,
      rulePayload,
      profileSettingsPayload,
      iotWifiPayload,
      entityCoveragePayload,
    ] = await Promise.all([
      apiRequest("/api/admin/summary"),
      apiRequest("/api/admin/users"),
      apiRequest("/api/analysis-jobs?limit=25"),
      apiRequest("/api/alerts"),
      apiRequest("/api/records"),
      apiRequest("/api/admin/settings/alert-rules"),
      apiRequest("/api/settings/profile"),
      apiRequest("/api/admin/settings/iot-wifi").catch(() => null),
      apiRequest("/api/system/entity-coverage").catch(() => null),
    ]);
    latestAdminSummary = summaryPayload;
    applySummary(summaryPayload);
    renderUserDirectory(usersPayload.users || []);
    applyAnalysisView(
      jobsPayload.jobs || [],
      alertsPayload.alerts || [],
      recordsPayload.records || [],
    );
    const reviewsPayload = await apiRequest("/api/admin/record-reviews");
    applyRecordsView(
      recordsPayload.records || [],
      reviewsPayload.reviews || [],
      summaryPayload,
    );
    applyMonitorView(summaryPayload);
    applyEntityCoverageView(entityCoveragePayload);
    applySettingsView(
      rulePayload.rule,
      profileSettingsPayload.settings,
      summaryPayload,
      iotWifiPayload,
    );
    applyStaticLanguage();
    if (!silent) {
      setCloudRefreshState(
        currentLanguageBundle().ui.refreshCloudData,
        false,
      );
    }
    return summaryPayload;
  } catch (error) {
    setBackendDisconnectedState(error.message || "Unable to refresh workspace data.");
    if (!silent) {
      setCloudRefreshState(translateWebTextNodeValue("Retry data"), false);
    }
    throw error;
  } finally {
    cloudRefreshInFlight = false;
  }
}

function diseaseDisplayName(value) {
  if (!value) {
    return "No result";
  }
  if (value.toString().trim().toLowerCase() === "not_pineapple") {
    return "No Pineapple Captured";
  }
  return value
    .toString()
    .replaceAll("_", " ")
    .split(" ")
    .map((part) => (part ? part[0].toUpperCase() + part.slice(1) : part))
    .join(" ");
}

function setAnalysisSubmitState(label, tone = "neutral") {
  if (!analysisSubmitState) {
    return;
  }
  analysisSubmitState.textContent = label;
  analysisSubmitState.classList.remove("neutral", "warning", "accent", "success");
  analysisSubmitState.classList.add(tone);
}

function latestSensorPayload() {
  const reading = latestAdminSummary?.nodes?.latestReading;
  if (!reading) {
    return null;
  }
  return {
    nodeId: reading.nodeId || "node_field_001",
    temperature: reading.temperature,
    humidity: reading.humidity,
    soilMoisture: reading.soilMoisture,
    pH: reading.pH,
  };
}

function setAnalysisControlsBusy(isBusy) {
  if (analysisSubmitButton) {
    analysisSubmitButton.disabled = isBusy;
  }
  if (analysisLatestFrameButton) {
    analysisLatestFrameButton.disabled = isBusy;
  }
}

function renderAnalysisJobResult(job, message) {
  if (analysisResultCard) {
    analysisResultCard.classList.add("is-visible");
  }
  if (analysisResultStatus) {
    analysisResultStatus.textContent = message || `Job status: ${(job.status || "pending").toUpperCase()}`;
  }

  const scanResult = jobScanResult(job);
  const jobImageUrl = job.imageUrl || scanResult.imageUri || "";
  const confidence = Number(scanResult.confidence || 0);
  if (analysisResultImage) {
    const media = analysisResultImage.closest(".media-frame");
    setImageElementSource(
      analysisResultImage,
      jobImageUrl,
      `${scanResult.disease ? scanResult.diseaseDisplayName || diseaseDisplayName(scanResult.disease) : "Analyzed scan"} image`,
    );
    media?.classList.toggle("has-image", Boolean(jobImageUrl));
    media?.classList.toggle("is-empty", !jobImageUrl);
  }
  if (analysisResultDisease) {
    analysisResultDisease.textContent = scanResult.disease
      ? scanResult.diseaseDisplayName || diseaseDisplayName(scanResult.disease)
      : "Diagnosis pending";
  }
  if (analysisResultConfidence) {
    analysisResultConfidence.textContent = scanResult.disease
      ? `${Math.round(confidence * 100)}% confidence from the analysis queue`
      : "The image is queued for AI analysis.";
  }
  if (analysisResultAdvice) {
    const treatment = Array.isArray(scanResult.treatment) ? scanResult.treatment[0] : "";
    analysisResultAdvice.textContent =
      treatment || scanResult.description || job.error || "Waiting for structured disease status.";
  }
}

async function pollAnalysisJob(jobId) {
  clearInterval(analysisStatusTimer);
  setAnalysisControlsBusy(true);
  setAnalysisSubmitState("Processing", "warning");

  const startedAt = Date.now();
  analysisStatusTimer = window.setInterval(async () => {
    try {
      const job = await apiRequest(`/api/analysis-jobs/${jobId}`);
      renderAnalysisJobResult(job);
      if (job.status === "done" || job.status === "failed" || job.status === "error") {
        clearInterval(analysisStatusTimer);
        setAnalysisControlsBusy(false);
        setAnalysisSubmitState(job.status === "done" ? "Complete" : "Needs review", job.status === "done" ? "success" : "warning");
        if (analysisUploadNote) {
          analysisUploadNote.textContent = job.status === "done"
            ? "AI analysis completed and the scan record is now available in the record history."
            : job.error || "PineGuard could not complete this AI analysis job.";
        }
        await hydrateAdminFromBackend();
      }
    } catch (error) {
      clearInterval(analysisStatusTimer);
      setAnalysisControlsBusy(false);
      setAnalysisSubmitState("Error", "warning");
      if (analysisUploadNote) {
        analysisUploadNote.textContent = error.message || "Unable to poll the AI job status.";
      }
    }

    if (Date.now() - startedAt > 180000) {
      clearInterval(analysisStatusTimer);
      setAnalysisControlsBusy(false);
      setAnalysisSubmitState("Still running", "warning");
      if (analysisUploadNote) {
        analysisUploadNote.textContent = "The AI job is still running. Refresh the analysis page to check the latest status.";
      }
    }
  }, 2000);
}

function createRecordImagePreviewDataUri(file) {
  if (!file || !file.type?.startsWith("image/")) {
    return Promise.resolve("");
  }

  return new Promise((resolve) => {
    const objectUrl = URL.createObjectURL(file);
    const image = new Image();
    image.onload = () => {
      try {
        const scale = Math.min(
          1,
          RECORD_IMAGE_PREVIEW_MAX_EDGE / Math.max(image.width, image.height),
        );
        const width = Math.max(1, Math.round(image.width * scale));
        const height = Math.max(1, Math.round(image.height * scale));
        const canvas = document.createElement("canvas");
        canvas.width = width;
        canvas.height = height;
        const context = canvas.getContext("2d");
        context?.drawImage(image, 0, 0, width, height);

        let preview = canvas.toDataURL("image/jpeg", RECORD_IMAGE_PREVIEW_QUALITY);
        if (preview.length > RECORD_IMAGE_PREVIEW_MAX_CHARS) {
          preview = canvas.toDataURL("image/jpeg", 0.52);
        }
        resolve(preview.length <= RECORD_IMAGE_PREVIEW_MAX_CHARS ? preview : "");
      } catch (_) {
        resolve("");
      } finally {
        URL.revokeObjectURL(objectUrl);
      }
    };
    image.onerror = () => {
      URL.revokeObjectURL(objectUrl);
      resolve("");
    };
    image.src = objectUrl;
  });
}

async function submitImportedAnalysisImage(event) {
  event.preventDefault();
  const file = analysisImageInput?.files?.[0];
  if (!file) {
    if (analysisUploadNote) {
      analysisUploadNote.textContent = "Choose a pineapple plant image before starting analysis.";
    }
    setAnalysisSubmitState("Choose image", "warning");
    return;
  }

  setAnalysisControlsBusy(true);
  setAnalysisSubmitState("Queued", "accent");
  if (analysisUploadNote) {
    analysisUploadNote.textContent = "Uploading image to the AI analysis queue.";
  }

  try {
    const formData = new FormData();
    formData.append("image", file, file.name || "admin-import.jpg");
    formData.append("session_id", `web-admin-${Date.now()}`);
    formData.append("live_mode", "true");
    const imagePreviewDataUri = await createRecordImagePreviewDataUri(file);
    if (imagePreviewDataUri) {
      formData.append("image_preview_data_uri", imagePreviewDataUri);
    }
    const sensorPayload = latestSensorPayload();
    if (sensorPayload) {
      formData.append("sensor_reading", JSON.stringify(sensorPayload));
    }

    const payload = await apiRequest("/api/analysis-jobs", {
      method: "POST",
      body: formData,
    });
    renderAnalysisJobResult({ status: payload.status || "pending" }, `Queued job ${payload.jobId}`);
    await pollAnalysisJob(payload.jobId);
  } catch (error) {
    setAnalysisControlsBusy(false);
    setAnalysisSubmitState("Error", "warning");
    if (analysisUploadNote) {
      analysisUploadNote.textContent = error.message || "Image analysis upload failed.";
    }
  }
}

async function submitLatestFrameAnalysis() {
  const latestFrame = latestAdminSummary?.nodes?.latestFrame;
  if (!latestFrame?.imageUrl) {
    if (analysisUploadNote) {
      analysisUploadNote.textContent = "No field camera frame is available yet.";
    }
    setAnalysisSubmitState("No frame", "warning");
    return;
  }

  setAnalysisControlsBusy(true);
  setAnalysisSubmitState("Queued", "accent");
  if (analysisUploadNote) {
    analysisUploadNote.textContent = "Sending latest field camera frame to the AI analysis queue.";
  }

  try {
    const payload = await apiRequest("/api/analysis-jobs", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        image_url: latestFrame.imageUrl,
        sensor_reading: latestSensorPayload(),
        session_id: `web-admin-frame-${Date.now()}`,
        live_mode: true,
      }),
    });
    renderAnalysisJobResult({ status: payload.status || "pending" }, `Queued job ${payload.jobId}`);
    await pollAnalysisJob(payload.jobId);
  } catch (error) {
    setAnalysisControlsBusy(false);
    setAnalysisSubmitState("Error", "warning");
    if (analysisUploadNote) {
      analysisUploadNote.textContent = error.message || "Latest frame analysis failed.";
    }
  }
}

function renderTimeline(container, items) {
  if (!container) {
    return;
  }
  container.innerHTML = "";
  items.forEach((item) => {
    const article = document.createElement("article");
    article.innerHTML = `
      <span>${item.time}</span>
      <div>
        <strong>${item.title}</strong>
        <p>${item.note}</p>
      </div>
    `;
    container.appendChild(article);
  });
}

function applyAnalysisView(jobs, alerts, records) {
  const completedJobs = jobs.filter((job) => job.status === "done");
  const queuedJobs = jobs.filter((job) => job.status === "pending");
  const runningJobs = jobs.filter((job) => job.status === "processing");
  const withSensorContext = completedJobs.filter(
    (job) =>
      job.liveAssessment?.sensorHealthScore !== undefined ||
      job.liveAssessment?.sensorSummary,
  );

  if (analysisCompletedJobsValue) {
    analysisCompletedJobsValue.textContent = String(completedJobs.length);
  }
  if (analysisCompletedNote) {
    analysisCompletedNote.textContent = completedJobs.length
      ? `Latest update ${formatDateTime(completedJobs[0]?.updatedAt)}`
      : "No completed jobs yet";
  }

  const diseaseCounts = completedJobs.reduce((acc, job) => {
    const disease = jobScanResult(job).disease || "unknown";
    acc[disease] = (acc[disease] || 0) + 1;
    return acc;
  }, {});
  const sortedDiseaseCounts = Object.entries(diseaseCounts).sort((a, b) => b[1] - a[1]);
  const topDisease = sortedDiseaseCounts[0];
  if (analysisTopClassValue) {
    analysisTopClassValue.textContent = topDisease
      ? diseaseDisplayName(topDisease[0])
      : "No completed jobs";
  }
  if (analysisTopClassNote) {
    analysisTopClassNote.textContent = topDisease
      ? `${topDisease[1]} of ${completedJobs.length} completed jobs`
      : "Waiting for scan output";
  }

  const overlapRate = completedJobs.length
    ? Math.round((withSensorContext.length / completedJobs.length) * 100)
    : 0;
  if (analysisOverlapRateValue) {
    analysisOverlapRateValue.textContent = `${overlapRate}%`;
  }
  if (analysisOverlapNote) {
    analysisOverlapNote.textContent = completedJobs.length
      ? `${withSensorContext.length} completed jobs include live sensor context`
      : "No completed jobs to compare against sensor context";
  }

  if (analysisQueueWaiting) {
    analysisQueueWaiting.textContent = `${queuedJobs.length} jobs waiting for AI processing after upload.`;
  }
  if (analysisQueueRunning) {
    analysisQueueRunning.textContent = `${runningJobs.length} active jobs are still moving through the analysis queue.`;
  }
  if (analysisQueueCompleted) {
    analysisQueueCompleted.textContent = `${completedJobs.length} jobs completed with structured diagnosis output.`;
  }

  if (analysisPredictionSpread) {
    analysisPredictionSpread.innerHTML = "";
    (sortedDiseaseCounts.slice(0, 3).length
      ? sortedDiseaseCounts.slice(0, 3)
      : [["no_result", 0]]
    ).forEach(([label, count]) => {
      const li = document.createElement("li");
      const percentage = completedJobs.length
        ? Math.round((count / completedJobs.length) * 100)
        : 0;
      li.innerHTML = `<strong>${percentage}%</strong> ${diseaseDisplayName(label)}`;
      analysisPredictionSpread.appendChild(li);
    });
  }

  renderTimeline(
    analysisCorrelationLog,
    [
      ...(alerts || []).slice(0, 1).map((alert) => ({
        time: formatDateTime(alert.timestamp),
        title: `${alert.sensorType || "Sensor"} alert raised`,
        note: alertMessage(alert) || "Threshold condition recorded.",
      })),
      ...(completedJobs || []).slice(0, 1).map((job) => ({
        time: formatDateTime(job.updatedAt),
        title: `AI job completed: ${jobScanResult(job).diseaseDisplayName || diseaseDisplayName(jobScanResult(job).disease)}`,
        note: jobLiveAssessment(job).sensorSummary || jobScanResult(job).description || "Structured scan result stored.",
      })),
      ...(records || []).slice(0, 1).map((record) => ({
        time: formatDateTime(record.timestamp),
        title: `${recordTitle(record) || "Record saved"}`,
        note: recordDescription(record) || "Activity record captured in the workspace.",
      })),
    ],
  );
}

function applyMonitorView(summary) {
  const latestReading = summary.nodes?.latestReading;
  const latestFrame = summary.nodes?.latestFrame;
  const unreadAlerts = summary.alerts?.unread || 0;
  const serviceNote = summary.database?.fallbackActive
    ? "Service is degraded; workspace data remains available."
    : "Workspace service is connected and ready.";

  if (monitorBackendStatus) {
    monitorBackendStatus.textContent = summary.database?.fallbackActive ? "Degraded" : "Healthy";
  }
  if (monitorBackendNote) {
    monitorBackendNote.textContent = serviceNote;
  }
  if (monitorNodeStatus) {
    monitorNodeStatus.textContent = summary.nodes?.online > 0 ? "Fresh" : "Waiting";
  }
  if (monitorNodeNote) {
    monitorNodeNote.textContent = latestReading
      ? `Latest reading received ${formatDateTime(latestReading.timestamp)}`
      : "No sensor reading stored yet";
  }
  if (monitorCameraStatus) {
    monitorCameraStatus.textContent = latestFrame ? "Online" : "Waiting";
  }
  if (monitorCameraNote) {
    monitorCameraNote.textContent = latestFrame
      ? `Latest frame stored at ${formatDateTime(latestFrame.capturedAt)}`
      : "No camera frame history yet";
  }
  if (monitorAlertStatus) {
    monitorAlertStatus.textContent = unreadAlerts > 0 ? "Active" : "Running";
  }
  if (monitorAlertNote) {
    monitorAlertNote.textContent = `${unreadAlerts} unread alerts in the threshold pipeline`;
  }
  if (monitorCameraFreshness) {
    monitorCameraFreshness.textContent = latestFrame
      ? formatDateTime(latestFrame.capturedAt)
      : "No frame yet";
  }
  if (monitorCameraPreview) {
    updateMediaFrame(
      monitorCameraPreview,
      latestFrame?.imageUrl || "",
      "Latest field camera frame",
    );
    const overlayLabel = monitorCameraPreview.querySelector(".camera-overlay span");
    const overlayTitle = monitorCameraPreview.querySelector(".camera-overlay strong");
    if (overlayLabel) {
      overlayLabel.textContent = latestFrame?.imageUrl ? "Latest frame" : "Camera frame";
    }
    if (overlayTitle) {
      overlayTitle.textContent = latestFrame?.imageUrl
        ? formatDateTime(latestFrame.capturedAt || latestFrame.uploadedAt)
        : "Waiting for uploaded image";
    }
  }
  renderTimeline(
    monitorFreshnessLog,
    [
      {
        time: summary.generatedAt ? formatDateTime(summary.generatedAt) : "Now",
        title: "Admin summary refreshed",
        note: "Dashboard, monitor, and settings now reflect current workspace data.",
      },
      latestFrame
        ? {
            time: formatDateTime(latestFrame.uploadedAt || latestFrame.capturedAt),
            title: "Camera frame stored",
            note: "Field camera image is available in the workspace history.",
          }
        : {
            time: "Pending",
            title: "Camera frame pending",
            note: "Waiting for field hardware to upload a frame.",
          },
      latestReading
        ? {
            time: formatDateTime(latestReading.timestamp),
            title: "Sensor reading persisted",
            note: `Current node status is ${latestReading.status}.`,
          }
        : {
            time: "Pending",
            title: "Sensor reading pending",
            note: "No reading has been posted by the field hardware yet.",
          },
    ],
  );
}

function entityCoveragePillClass(entity) {
  if (!entity?.hasRecords || entity?.interfaceStatus === "pending") {
    return "warning";
  }
  if (entity?.interfaceStatus === "internal") {
    return "neutral";
  }
  return "success";
}

function entityCoverageLabel(entity) {
  if (entity?.interfaceStatus === "internal") {
    return translateWebTextNodeValue("Internal");
  }
  if (entity?.interfaceStatus === "pending") {
    return translateWebTextNodeValue("Pending");
  }
  if (!entity?.hasRecords) {
    return translateWebTextNodeValue("Empty");
  }
  return translateWebTextNodeValue("Synced");
}

function applyEntityCoverageView(coverage) {
  latestEntityCoverage = coverage || null;
  const summary = coverage?.summary || {};

  if (coverageHealthPill) {
    if (!coverage) {
      coverageHealthPill.textContent = translateWebTextNodeValue("Pending");
      coverageHealthPill.className = "pill neutral";
    } else {
      const degraded = coverage?.database?.fallbackActive;
      coverageHealthPill.textContent = degraded
        ? translateWebTextNodeValue("Degraded")
        : translateWebTextNodeValue("Synced");
      coverageHealthPill.className = `pill ${degraded ? "warning" : "success"}`;
    }
  }
  if (coverageTableCount) {
    setLiveText(coverageTableCount, summary.tableCount ?? "--");
  }
  if (coverageRowCount) {
    setLiveText(coverageRowCount, summary.rowCount ?? "--");
  }
  if (coverageEmptyCount) {
    setLiveText(coverageEmptyCount, summary.emptyTables ?? "--");
  }

  if (coverageGroupGrid) {
    coverageGroupGrid.innerHTML = "";
    (coverage?.groups || []).forEach((group) => {
      const hasPending = (group.pendingTables || 0) > 0 || (group.emptyTables || 0) > 0;
      const card = document.createElement("article");
      card.className = "coverage-card";
      const copy = document.createElement("div");
      const title = document.createElement("strong");
      const groupDomain = group.domain === "Database" ? "Workspace" : group.domain;
      title.textContent = translateWebTextNodeValue(groupDomain || "Workspace");
      const note = document.createElement("p");
      const latest = group.latestAt
        ? formatDateTime(group.latestAt)
        : translateWebTextNodeValue("No recent sync");
      note.textContent =
        `${group.tableCount || 0} ${translateWebTextNodeValue("Areas").toLowerCase()} | ${group.rowCount || 0} ${translateWebTextNodeValue("Records").toLowerCase()} | ${latest}`;
      copy.append(title, note);
      const pill = document.createElement("span");
      pill.className = `pill ${hasPending ? "warning" : "success"}`;
      pill.textContent = hasPending
        ? translateWebTextNodeValue("Pending")
        : translateWebTextNodeValue("Synced");
      card.append(copy, pill);
      coverageGroupGrid.appendChild(card);
    });
    if (!coverageGroupGrid.childElementCount) {
      const card = document.createElement("article");
      card.className = "coverage-card";
      const copy = document.createElement("div");
      const title = document.createElement("strong");
      title.textContent = translateWebTextNodeValue("Waiting for system coverage");
      const note = document.createElement("p");
      note.textContent = translateWebTextNodeValue(
        "Coverage counts will appear after the admin session syncs.",
      );
      copy.append(title, note);
      const pill = document.createElement("span");
      pill.className = "pill neutral";
      pill.textContent = translateWebTextNodeValue("Pending");
      card.append(copy, pill);
      coverageGroupGrid.appendChild(card);
    }
  }

  if (coverageEntityList) {
    coverageEntityList.innerHTML = "";
    (coverage?.entities || []).forEach((entity) => {
      const row = document.createElement("article");
      row.className = "coverage-entity-row";

      const copy = document.createElement("div");
      const title = document.createElement("strong");
      title.textContent = translateWebTextNodeValue(entity.name || "Workspace area");
      const description = document.createElement("p");
      description.textContent = translateWebTextNodeValue(entity.description || "");
      const meta = document.createElement("small");
      const latest = entity.latestAt
        ? formatDateTime(entity.latestAt)
        : translateWebTextNodeValue("No recent sync");
      meta.textContent =
        `${entity.rowCount || 0} ${translateWebTextNodeValue("Records").toLowerCase()} | ${latest} | ${translateWebTextNodeValue("Mobile")}: ${translateWebTextNodeValue(entity.mobileSurface || "")} | ${translateWebTextNodeValue("Web")}: ${translateWebTextNodeValue(entity.webSurface || "")}`;
      copy.append(title, description, meta);

      const pill = document.createElement("span");
      pill.className = `pill ${entityCoveragePillClass(entity)}`;
      pill.textContent = entityCoverageLabel(entity);
      row.append(copy, pill);
      coverageEntityList.appendChild(row);
    });
  }
}

function setIotWifiStatus(element, config) {
  if (!element) {
    return;
  }
  const configured = !!config?.configured;
  element.textContent = configured
    ? translateWebTextNodeValue("Configured")
    : translateWebTextNodeValue("Not configured");
  element.classList.toggle("accent", configured);
  element.classList.toggle("neutral", !configured);
}

function applyIotWifiSettingsView(payload) {
  latestIotWifiSettings = payload || latestIotWifiSettings;
  const configs = latestIotWifiSettings?.configs || {};
  const esp32 = configs.esp32 || {};
  const esp32Cam = configs.esp32Cam || {};

  if (settingsEsp32SsidInput) {
    settingsEsp32SsidInput.value = esp32.ssid || "";
  }
  if (settingsEsp32PasswordInput) {
    settingsEsp32PasswordInput.value = "";
    settingsEsp32PasswordInput.placeholder = esp32.passwordConfigured
      ? translateWebTextNodeValue("Keep current password")
      : translateWebTextNodeValue("Wi-Fi password");
  }
  if (settingsEsp32CamSsidInput) {
    settingsEsp32CamSsidInput.value = esp32Cam.ssid || "";
  }
  if (settingsEsp32CamPasswordInput) {
    settingsEsp32CamPasswordInput.value = "";
    settingsEsp32CamPasswordInput.placeholder = esp32Cam.passwordConfigured
      ? translateWebTextNodeValue("Keep current password")
      : translateWebTextNodeValue("Wi-Fi password");
  }
  setIotWifiStatus(settingsEsp32Status, esp32);
  setIotWifiStatus(settingsEsp32CamStatus, esp32Cam);
}

function readIotWifiSettingsPayload() {
  return {
    esp32: {
      ssid: settingsEsp32SsidInput?.value.trim() || "",
      password: settingsEsp32PasswordInput?.value
        ? settingsEsp32PasswordInput.value
        : null,
    },
    esp32Cam: {
      ssid: settingsEsp32CamSsidInput?.value.trim() || "",
      password: settingsEsp32CamPasswordInput?.value
        ? settingsEsp32CamPasswordInput.value
        : null,
    },
  };
}

function iotWifiConfigForDevice(deviceKey) {
  const configs = latestIotWifiSettings?.configs || {};
  return deviceKey === "esp32Cam" ? configs.esp32Cam || {} : configs.esp32 || {};
}

function validateIotWifiDevicePayload(label, payload, existingConfig) {
  const ssid = payload?.ssid || "";
  const password = payload?.password;
  if (ssid.length > 32) {
    return `${label} Wi-Fi name must be 32 characters or less.`;
  }
  if (!ssid && password) {
    return `Enter the ${label} Wi-Fi name before saving a password.`;
  }
  if (password && password.length < 8) {
    return `${label} Wi-Fi password must be at least 8 characters.`;
  }
  if (password && password.length > 64) {
    return `${label} Wi-Fi password must be 64 characters or less.`;
  }
  if (ssid && !password && !existingConfig?.passwordConfigured) {
    return `${label} password is empty. Enter a password, or use the ESP32 setup portal for an open network.`;
  }
  return "";
}

function validateIotWifiSettingsPayload(payload) {
  return (
    validateIotWifiDevicePayload(
      "ESP32 sensor node",
      payload.esp32,
      iotWifiConfigForDevice("esp32"),
    ) ||
    validateIotWifiDevicePayload(
      "ESP32-CAM",
      payload.esp32Cam,
      iotWifiConfigForDevice("esp32Cam"),
    )
  );
}

function setSettingsSaveError(message) {
  if (settingsConfigStatus) {
    settingsConfigStatus.textContent = message || "Unable to save settings.";
    settingsConfigStatus.classList.add("warning");
  }
}

function applySettingsView(rule, settings, summary, iotWifiPayload = null) {
  const effectiveRule = { ...DEFAULT_ALERT_RULE_SETTINGS, ...(rule || {}) };
  const effectiveSettings = { ...DEFAULT_PROFILE_SETTINGS, ...(settings || {}) };
  latestAlertRuleSettings = effectiveRule;
  latestProfileSettings = effectiveSettings;
  writeStoredLanguage(effectiveSettings.language || activeLanguage);
  applyCloudRefreshCadence(effectiveSettings.syncCadence);
  applyIotWifiSettingsView(iotWifiPayload);

  if (settingsSoilDryInput) {
    settingsSoilDryInput.value = `${effectiveRule.soilDry}`;
  }
  if (settingsPhLowInput) {
    settingsPhLowInput.value = `${Number(effectiveRule.phLow).toFixed(1)}`;
  }
  if (settingsTempHighInput) {
    settingsTempHighInput.value = `${Number(effectiveRule.tempHigh).toFixed(0)}`;
  }
  if (settingsSyncCadenceInput) {
    settingsSyncCadenceInput.value = effectiveSettings.syncCadence || "realtime";
  }
  if (settingsCriticalAlertsInput) {
    settingsCriticalAlertsInput.checked = !!effectiveSettings.pushNotificationsEnabled;
  }
  if (settingsAnalysisNoticesInput) {
    settingsAnalysisNoticesInput.checked = !!effectiveSettings.assistantRecommendationsEnabled;
  }
  if (settingsDigestSummaryInput) {
    settingsDigestSummaryInput.checked = !!effectiveSettings.dailyDigestEnabled;
  }

  if (settingsSoilDry && rule?.soilDry !== undefined) {
    settingsSoilDry.textContent = `${rule.soilDry}%`;
  }
  if (settingsPhLow && rule?.phLow !== undefined) {
    settingsPhLow.textContent = `${rule.phLow.toFixed(1)}`;
  }
  if (settingsTempHigh && rule?.tempHigh !== undefined) {
    settingsTempHigh.textContent = `${rule.tempHigh.toFixed(0)} deg C`;
  }
  if (settingsPollingCadence) {
    settingsPollingCadence.textContent =
      effectiveSettings.syncCadence === "realtime"
        ? "Realtime"
        : effectiveSettings.syncCadence || "Realtime";
  }
  if (settingsAnalysisMode) {
    settingsAnalysisMode.textContent = "Cloud analysis relay";
  }
  if (settingsBackendUrl) {
    settingsBackendUrl.textContent = "Standard route";
  }
  if (settingsCriticalAlerts) {
    settingsCriticalAlerts.textContent = effectiveSettings.pushNotificationsEnabled ? "On" : "Off";
    settingsCriticalAlerts.classList.toggle("is-on", !!effectiveSettings.pushNotificationsEnabled);
  }
  if (settingsAnalysisNotices) {
    settingsAnalysisNotices.textContent = effectiveSettings.assistantRecommendationsEnabled ? "On" : "Off";
    settingsAnalysisNotices.classList.toggle("is-on", !!effectiveSettings.assistantRecommendationsEnabled);
  }
  if (settingsDigestSummary) {
    settingsDigestSummary.textContent = effectiveSettings.dailyDigestEnabled ? "On" : "Off";
    settingsDigestSummary.classList.toggle("is-on", !!effectiveSettings.dailyDigestEnabled);
  }
  if (settingsSessionTimeout) {
    settingsSessionTimeout.textContent = "7 days";
  }
  if (settingsConfigStatus) {
    settingsConfigStatus.textContent = summary?.database?.fallbackActive
      ? "Service degraded"
      : "Connected";
    settingsConfigStatus.classList.toggle("warning", !!summary?.database?.fallbackActive);
  }
}

function clampNumber(value, min, max) {
  return Math.min(max, Math.max(min, value));
}

function readProfileSettingsPayload(currentSettings = {}) {
  return {
    ...DEFAULT_PROFILE_SETTINGS,
    ...currentSettings,
    pushNotificationsEnabled: checkboxFromControl(
      settingsCriticalAlertsInput,
      DEFAULT_PROFILE_SETTINGS.pushNotificationsEnabled,
    ),
    dailyDigestEnabled: checkboxFromControl(
      settingsDigestSummaryInput,
      DEFAULT_PROFILE_SETTINGS.dailyDigestEnabled,
    ),
    assistantRecommendationsEnabled: checkboxFromControl(
      settingsAnalysisNoticesInput,
      DEFAULT_PROFILE_SETTINGS.assistantRecommendationsEnabled,
    ),
    syncCadence: settingsSyncCadenceInput?.value || DEFAULT_PROFILE_SETTINGS.syncCadence,
    language: activeLanguage,
    backendMode: currentSettings.backendMode || DEFAULT_PROFILE_SETTINGS.backendMode,
  };
}

function readAlertRulePayload(currentRule = {}) {
  const effectiveRule = { ...DEFAULT_ALERT_RULE_SETTINGS, ...currentRule };
  const soilDry = clampNumber(
    numberFromControl(settingsSoilDryInput, effectiveRule.soilDry),
    0,
    100,
  );
  const phHigh = Number(effectiveRule.phHigh) || DEFAULT_ALERT_RULE_SETTINGS.phHigh;
  const phLow = clampNumber(
    numberFromControl(settingsPhLowInput, effectiveRule.phLow),
    0,
    phHigh,
  );
  const tempLowValue = Number(effectiveRule.tempLow);
  const tempLow = Number.isFinite(tempLowValue)
    ? tempLowValue
    : DEFAULT_ALERT_RULE_SETTINGS.tempLow;
  const tempHigh = clampNumber(
    numberFromControl(settingsTempHighInput, effectiveRule.tempHigh),
    tempLow + 1,
    80,
  );

  return {
    ...effectiveRule,
    soilDry,
    soilCriticalDry: Math.min(Number(effectiveRule.soilCriticalDry) || 0, soilDry),
    phLow,
    tempHigh,
  };
}

function updateSettingsPreviewFromControls() {
  const profile = readProfileSettingsPayload(latestProfileSettings);
  if (settingsCriticalAlerts) {
    settingsCriticalAlerts.textContent = profile.pushNotificationsEnabled ? "On" : "Off";
    settingsCriticalAlerts.classList.toggle("is-on", !!profile.pushNotificationsEnabled);
  }
  if (settingsAnalysisNotices) {
    settingsAnalysisNotices.textContent = profile.assistantRecommendationsEnabled ? "On" : "Off";
    settingsAnalysisNotices.classList.toggle("is-on", !!profile.assistantRecommendationsEnabled);
  }
  if (settingsDigestSummary) {
    settingsDigestSummary.textContent = profile.dailyDigestEnabled ? "On" : "Off";
    settingsDigestSummary.classList.toggle("is-on", !!profile.dailyDigestEnabled);
  }
  if (settingsPollingCadence) {
    settingsPollingCadence.textContent =
      profile.syncCadence === "realtime" ? "Realtime" : profile.syncCadence || "Realtime";
  }
  applyCloudRefreshCadence(profile.syncCadence);
}

async function saveProfileSettingsFromControls({ feedback = "Saved" } = {}) {
  const profilePayload = readProfileSettingsPayload(latestProfileSettings);
  latestProfileSettings = profilePayload;
  updateSettingsPreviewFromControls();
  if (!adminToken) {
    return;
  }
  if (settingsConfigStatus) {
    settingsConfigStatus.textContent = "Saving...";
    settingsConfigStatus.classList.remove("warning");
  }
  await apiRequest("/api/settings/profile", {
    method: "PUT",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(profilePayload),
  });
  if (settingsConfigStatus) {
    settingsConfigStatus.textContent = feedback;
    settingsConfigStatus.classList.remove("warning");
  }
}

function queueProfileSettingsAutosave() {
  window.clearTimeout(profileSettingsAutosaveTimer);
  updateSettingsPreviewFromControls();
  profileSettingsAutosaveTimer = window.setTimeout(() => {
    saveProfileSettingsFromControls({ feedback: "Saved" }).catch((error) => {
      setBackendDisconnectedState(error.message || "Unable to save settings.");
    });
  }, 250);
}

async function loginToBackend() {
  const email = adminEmailInput?.value.trim() || "";
  const password = adminPasswordInput?.value || "";
  if (!email || !password) {
    throw new Error("Enter both email and password.");
  }

  const payload = await apiRequest("/api/auth/login", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify({ email, password }),
  });

  if (payload.user?.role !== "admin") {
    throw new Error("Use an admin account for PineGuard Admin.");
  }

  const healthPayload = await readBackendHealth();
  adminToken = payload.token || "";
  if (adminToken) {
    window.localStorage.setItem("pineguardAdminToken", adminToken);
  }
  return healthPayload;
}

async function restoreSavedAdminSession() {
  if (!adminToken) {
    return;
  }

  try {
    await readBackendHealth();
    await hydrateAdminFromBackend();
    loginView?.classList.add("is-hidden");
    adminView?.classList.remove("is-hidden");
    setActivePage("dashboard");
    startCloudRefreshLoop();
    setLoginFeedback("Resumed saved admin session.", "success");
  } catch (error) {
    adminToken = "";
    clearInterval(cloudRefreshTimer);
    window.localStorage.removeItem("pineguardAdminToken");
    adminView?.classList.add("is-hidden");
    loginView?.classList.remove("is-hidden");
    setLoginFeedback(
      error.message || "Saved admin session expired. Sign in again.",
      "info",
    );
  }
}

enterAdminButton?.addEventListener("click", async () => {
  setLoginFeedback("");
  if (enterAdminButton) {
    enterAdminButton.disabled = true;
  }
  try {
    const healthPayload = await loginToBackend();
    await hydrateAdminFromBackend();
    loginView?.classList.add("is-hidden");
    adminView?.classList.remove("is-hidden");
    setActivePage("dashboard");
    startCloudRefreshLoop();
    const fallbackActive = !!healthPayload.databaseDetails?.fallbackActive;
    setLoginFeedback(
      fallbackActive
        ? "Connected with limited workspace data."
        : "Connected to PineGuard workspace.",
      "success",
    );
  } catch (error) {
    adminToken = "";
    clearInterval(cloudRefreshTimer);
    window.localStorage.removeItem("pineguardAdminToken");
    adminView?.classList.add("is-hidden");
    loginView?.classList.remove("is-hidden");
    setLoginFeedback(
      error.message || "Unable to sign in. Check the service connection.",
    );
  } finally {
    if (enterAdminButton) {
      enterAdminButton.disabled = false;
    }
  }
});

logoutButton?.addEventListener("click", () => {
  adminToken = "";
  clearInterval(cloudRefreshTimer);
  window.localStorage.removeItem("pineguardAdminToken");
  adminView?.classList.add("is-hidden");
  loginView?.classList.remove("is-hidden");
});

saveSettingsButton?.addEventListener("click", async () => {
  setButtonBusy(saveSettingsButton, "Saving...", true);
  try {
    const currentSettings = await apiRequest("/api/settings/profile");
    const currentRule = await apiRequest("/api/admin/settings/alert-rules");
    const profilePayload = readProfileSettingsPayload(currentSettings.settings || {});
    const alertRulePayload = readAlertRulePayload(currentRule.rule || {});
    const iotWifiPayload = readIotWifiSettingsPayload();
    const iotWifiValidation = validateIotWifiSettingsPayload(iotWifiPayload);
    if (iotWifiValidation) {
      throw new Error(iotWifiValidation);
    }

    await apiRequest("/api/settings/profile", {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(profilePayload),
    });
    await apiRequest("/api/admin/settings/alert-rules", {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(alertRulePayload),
    });
    const savedIotWifiPayload = await apiRequest("/api/admin/settings/iot-wifi", {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(iotWifiPayload),
    });
    latestProfileSettings = profilePayload;
    latestAlertRuleSettings = alertRulePayload;
    latestIotWifiSettings = savedIotWifiPayload;
    applyCloudRefreshCadence(profilePayload.syncCadence);
    applySettingsView(
      alertRulePayload,
      profilePayload,
      latestAdminSummary,
      savedIotWifiPayload,
    );
    hydrateAdminFromBackend({ silent: true }).catch(() => {
      // The Wi-Fi settings are already saved; keep the typed values visible and
      // let the normal cloud refresh loop recover dashboard data.
    });
    if (settingsConfigStatus) {
      settingsConfigStatus.textContent = "Hardware Wi-Fi saved for ESP32 and ESP32-CAM sync";
      settingsConfigStatus.classList.remove("warning");
    }
  } catch (error) {
    setSettingsSaveError(error.message || "Unable to save settings.");
  } finally {
    setButtonBusy(saveSettingsButton, "", false);
  }
});

resetSettingsButton?.addEventListener("click", async () => {
  setButtonBusy(resetSettingsButton, "Resetting...", true);
  try {
    await apiRequest("/api/settings/profile", {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(DEFAULT_PROFILE_SETTINGS),
    });
    await apiRequest("/api/admin/settings/alert-rules", {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(DEFAULT_ALERT_RULE_SETTINGS),
    });
    latestProfileSettings = { ...DEFAULT_PROFILE_SETTINGS };
    latestAlertRuleSettings = { ...DEFAULT_ALERT_RULE_SETTINGS };
    applyCloudRefreshCadence(DEFAULT_PROFILE_SETTINGS.syncCadence);
    await hydrateAdminFromBackend();
    if (settingsConfigStatus) {
      settingsConfigStatus.textContent = "Defaults restored";
      settingsConfigStatus.classList.remove("warning");
    }
  } catch (error) {
    setBackendDisconnectedState(error.message || "Unable to reset settings.");
  } finally {
    setButtonBusy(resetSettingsButton, "", false);
  }
});

refreshCloudDataButton?.addEventListener("click", async () => {
  try {
    await hydrateAdminFromBackend();
  } catch (_) {
    // The service status cards already show the current failure state.
  }
});

recordFilterButtons.forEach((button) => {
  button.addEventListener("click", () => {
    setRecordFilter(button.dataset.recordFilter);
  });
});

openSelectedRecordButton?.addEventListener("click", () => {
  if (!selectedRecordId && latestAdminRecords.length) {
    selectedRecordId = latestAdminRecords[0].id || "";
  }
  selectRecord(selectedRecordId, { openDetail: true });
});

detailRemoveRecordButton?.addEventListener("click", () => {
  const selectedRecord =
    latestAdminRecords.find((record) => record.id === selectedRecordId) ||
    latestAdminRecords[0];
  removeRecord(selectedRecord, detailRemoveRecordButton);
});

[
  settingsCriticalAlertsInput,
  settingsAnalysisNoticesInput,
  settingsDigestSummaryInput,
  settingsSyncCadenceInput,
].forEach((control) => {
  control?.addEventListener("change", queueProfileSettingsAutosave);
});

languageSelects.forEach((select) => {
  select.value = activeLanguage;
  select.addEventListener("change", async () => {
    writeStoredLanguage(select.value);
    if (!adminToken) {
      return;
    }
    try {
      const currentSettings = await apiRequest("/api/settings/profile");
      await apiRequest("/api/settings/profile", {
        method: "PUT",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          ...DEFAULT_PROFILE_SETTINGS,
          ...(currentSettings.settings || {}),
          language: activeLanguage,
        }),
      });
      await hydrateAdminFromBackend();
    } catch (error) {
      setBackendDisconnectedState(error.message || "Unable to sync language.");
    }
  });
});

analysisSubmitForm?.addEventListener("submit", submitImportedAnalysisImage);
analysisLatestFrameButton?.addEventListener("click", submitLatestFrameAnalysis);

navItems.forEach((item) => {
  item.addEventListener("click", () => {
    setActivePage(item.dataset.pageTarget);
  });
});

jumpButtons.forEach((button) => {
  button.addEventListener("click", () => {
    const pageId = button.dataset.pageJump;
    if (pageId) {
      setActivePage(pageId);
      adminView?.scrollIntoView({ behavior: "smooth", block: "start" });
    }
  });
});

const presetDefaults = {
  admin: {
    name: "Nur Izzati",
    email: "nur.admin@pineguard.local",
    location: "HQ oversight",
    note: "Access control, record review, and operational approvals.",
  },
  farmer: {
    name: "Hafiz Salleh",
    email: "hafiz@pineguard.local",
    location: "Managed Block East",
    note: "Daily crop observation and scan submission for the new farmer account.",
  },
};

function applyUserPreset(role) {
  const preset = presetDefaults[role];
  if (!preset || !managedUserRole) {
    return;
  }

  managedUserRole.value = role;
  if (managedUserName) managedUserName.value = preset.name;
  if (managedUserEmail) managedUserEmail.value = preset.email;
  if (managedUserLocation) managedUserLocation.value = preset.location;
  if (managedUserNote) managedUserNote.value = preset.note;
  if (managedUserPassword) managedUserPassword.value = "";
  if (managedUserPasswordConfirm) managedUserPasswordConfirm.value = "";
}

presetButtons.forEach((button) => {
  button.addEventListener("click", () => {
    const role = button.dataset.userPreset;
    if (role) {
      applyUserPreset(role);
    }
  });
});

managedUserForm?.addEventListener("submit", async (event) => {
  event.preventDefault();

  const name = managedUserName?.value.trim() || "Field User";
  const email = managedUserEmail?.value.trim() || "field.user@pineguard.local";
  const roleValue = normalizeManagedUserRole(managedUserRole?.value).value;
  const location = managedUserLocation?.value.trim() || "Assigned location";
  const password = managedUserPassword?.value || "";
  const passwordConfirm = managedUserPasswordConfirm?.value || "";

  if (password.length < 6) {
    managedUserPassword?.focus();
    window.alert(translateWebTextNodeValue("Set a password with at least 6 characters for the new user."));
    return;
  }
  if (password !== passwordConfirm) {
    managedUserPasswordConfirm?.focus();
    window.alert(translateWebTextNodeValue("The new user password confirmation does not match."));
    return;
  }

  if (!adminToken) {
    window.alert(translateWebTextNodeValue("Sign in as an admin before creating users."));
    return;
  }

  try {
    await apiRequest("/api/admin/users", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        displayName: name,
        email,
        role: roleValue,
        location,
        password,
      }),
    });
    await hydrateAdminFromBackend();
    managedUserForm.reset();
    applyUserPreset("farmer");
  } catch (error) {
    const message = error.message || "Unable to create this user.";
    setBackendDisconnectedState(message);
    window.alert(message);
  }
});

restoreSavedAdminSession();
