const enterAdminButton = document.getElementById("enter-admin");
const loginView = document.querySelector('[data-screen="login"]');
const adminView = document.querySelector('[data-screen="admin"]');
const pageTitle = document.getElementById("page-title");
const logoutButton = document.getElementById("logout-button");
const navItems = Array.from(document.querySelectorAll(".nav-item"));
const pages = Array.from(document.querySelectorAll(".page"));
const jumpButtons = Array.from(document.querySelectorAll("[data-page-jump]"));
const presetButtons = Array.from(document.querySelectorAll("[data-user-preset]"));
const demoUserForm = document.getElementById("demo-user-form");
const demoUserName = document.getElementById("demo-user-name");
const demoUserEmail = document.getElementById("demo-user-email");
const demoUserRole = document.getElementById("demo-user-role");
const demoUserLocation = document.getElementById("demo-user-location");
const demoUserNote = document.getElementById("demo-user-note");
const userDirectoryBody = document.getElementById("user-directory-body");
const demoUserCount = document.getElementById("demo-user-count");
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
const analysisResultDisease = document.getElementById("analysis-result-disease");
const analysisResultConfidence = document.getElementById("analysis-result-confidence");
const analysisResultAdvice = document.getElementById("analysis-result-advice");
const recordsStream = document.getElementById("records-stream");
const recordsMixList = document.getElementById("records-mix-list");
const detailRecordTitle = document.getElementById("detail-record-title");
const detailImageStage = document.getElementById("detail-image-stage");
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
const settingsSoilDryInput = document.getElementById("settings-soil-dry-input");
const settingsPhLowInput = document.getElementById("settings-ph-low-input");
const settingsTempHighInput = document.getElementById("settings-temp-high-input");
const settingsSyncCadenceInput = document.getElementById("settings-sync-cadence-input");
const settingsCriticalAlertsInput = document.getElementById("settings-critical-alerts-input");
const settingsAnalysisNoticesInput = document.getElementById("settings-analysis-notices-input");
const settingsDigestSummaryInput = document.getElementById("settings-digest-summary-input");
const saveSettingsButton = document.getElementById("save-settings-button");
const resetSettingsButton = document.getElementById("reset-settings-button");

const pageTitles = {
  dashboard: "Admin Dashboard",
  users: "User Management",
  records: "Submitted Records",
  detail: "Record Detail",
  analysis: "Data Analysis",
  monitor: "System Monitor",
  settings: "Settings",
};

const DEFAULT_DATABASE_API_BASE_URL = "";
const LOCAL_MACHINE_API_BASE_URL = "http://127.0.0.1:8000";
const API_BASE_URL_STORAGE_KEY = "pineguardAdminApiBaseUrl";
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
  const urls = [];
  if (Array.isArray(window.PINEGUARD_API_BASE_URLS)) {
    urls.push(...window.PINEGUARD_API_BASE_URLS);
  }
  urls.push(window.PINEGUARD_API_BASE_URL);
  return urls;
}

function stableHostnameApiBaseUrls() {
  return [
    "http://pineguard-backend:8000",
    "http://pineguard-backend.local:8000",
  ];
}

function databaseApiBaseUrlCandidates() {
  const generatedCandidates = generatedApiBaseUrlCandidates();
  const candidates = localApiDiscoveryEnabled()
    ? [
        apiBaseUrlFromQuery(),
        readStoredApiBaseUrl(),
        ...generatedCandidates,
        sameOriginApiBaseUrl(),
        localMachineApiBaseUrl(),
        ...stableHostnameApiBaseUrls(),
        DEFAULT_DATABASE_API_BASE_URL,
      ]
    : generatedCandidates;

  const normalized = [];
  candidates.forEach((candidate) => {
    const value = normalizeDatabaseApiBaseUrl(candidate);
    if (value && !normalized.includes(value)) {
      normalized.push(value);
    }
  });
  return normalized;
}

let API_BASE_URL = databaseApiBaseUrlCandidates()[0] || DEFAULT_DATABASE_API_BASE_URL;
let adminToken = window.localStorage.getItem("pineguardAdminToken") || "";
let latestAdminSummary = null;
let analysisStatusTimer = null;
let cloudRefreshTimer = null;
let cloudRefreshInFlight = false;

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
    settingsConfigStatus.textContent = "Database unavailable";
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
  }, LIVE_REFRESH_INTERVAL_MS);
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

function setActivePage(pageId) {
  const navPageId = pageId === "detail" ? "records" : pageId;

  navItems.forEach((item) => {
    item.classList.toggle("is-active", item.dataset.pageTarget === navPageId);
  });

  pages.forEach((page) => {
    page.classList.toggle("is-active", page.dataset.page === pageId);
  });

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

  const response = await fetchFromApi(path, {
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
    settingsBackendUrl.textContent = value;
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
      ? `Unable to reach the backend API. Tried ${failures.join(", ")}.`
      : "Unable to reach the backend API.",
  );
}

async function verifyDatabaseApiBaseUrl(candidate) {
  const baseUrl = normalizeDatabaseApiBaseUrl(candidate);
  if (!baseUrl) {
    throw new Error("empty backend URL");
  }
  if (verifiedDatabaseApiBaseUrls.has(baseUrl)) {
    return;
  }
  if (rejectedDatabaseApiBaseUrls.has(baseUrl)) {
    throw new Error("not a PineGuard database API");
  }

  const response = await fetchWithBackendTimeout(`${baseUrl}/health`, {
    headers: { Accept: "application/json" },
  }, API_UPLOAD_TIMEOUT_MS);
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
    throw new Error("not a PineGuard database API");
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
        `Backend health check failed with ${response.status}`,
    );
  }

  if (payload.backend !== "running") {
    throw new Error("The backend is not running.");
  }

  if (payload.status && !["ok", "degraded"].includes(payload.status)) {
    throw new Error(`The backend health status is ${payload.status}.`);
  }

  return payload;
}

function formatDateTime(ms) {
  if (!ms) {
    return "No recent sync";
  }
  return new Date(ms).toLocaleString();
}

function applySummary(summary) {
  if (!summary) {
    return;
  }

  const latestReading = summary.nodes?.latestReading;
  const latestFrame = summary.nodes?.latestFrame;

  if (heroSignalValues.length >= 3) {
    setLiveText(heroSignalValues[0], `${summary.database?.kind || "db"} live`);
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

  if (demoUserCount) {
    demoUserCount.textContent = String(users.length).padStart(2, "0");
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

function applyRecordsView(records, reviews, summary) {
  const latestRecords = (records || []).slice(0, 6);
  if (!latestRecords.length) {
    return;
  }
  if (recordsStream) {
    recordsStream.innerHTML = "";
    latestRecords.forEach((record, index) => {
      const article = document.createElement("article");
      article.className = "record-card";
      const review = (reviews || []).find((item) => item.recordId === record.id);
      const pillLabel = review?.reviewStatus
        ? review.reviewStatus
        : record.type === "scan"
          ? "Needs review"
          : "Logged";
      article.innerHTML = `
        <div>
          <p class="record-tag">${record.type || "Record"}</p>
          <strong>${record.id || `REC-${String(index + 1).padStart(3, "0")}`} - ${record.title || "Untitled record"}</strong>
          <p>${record.description || "No description provided."}</p>
        </div>
        <div class="record-meta">
          <span>${formatDateTime(record.timestamp)}</span>
          <span class="pill ${reviewStatusPillClass((pillLabel || "").toLowerCase())}">${pillLabel}</span>
        </div>
      `;
      recordsStream.appendChild(article);
    });
  }

  if (recordsMixList) {
    const scanCount = (records || []).filter((item) => item.type === "scan").length;
    const activityCount = (records || []).filter((item) => item.type === "activity").length;
    const reviewCount = (reviews || []).length;
    const total = Math.max(1, (records || []).length);
    recordsMixList.innerHTML = `
      <li><strong>${Math.round((scanCount / total) * 100)}%</strong> scan records</li>
      <li><strong>${Math.round((activityCount / total) * 100)}%</strong> activity logs</li>
      <li><strong>${Math.round((reviewCount / total) * 100)}%</strong> review notes</li>
    `;
  }

  const selectedRecord = latestRecords[0];
  if (!selectedRecord) {
    return;
  }

  const selectedReview = (reviews || []).find((item) => item.recordId === selectedRecord.id);
  const scanResult = selectedRecord.scanResult || {};
  const treatmentSteps = selectedRecord.treatment?.steps || [];
  if (detailRecordTitle) {
    detailRecordTitle.textContent = `${selectedRecord.id} - review the diagnosis, environment, and follow-up context.`;
  }
  if (detailImageCaption) {
    detailImageCaption.textContent = scanResult.imageUri ? "Leaf capture" : "No image";
  }
  if (detailImageLabel) {
    detailImageLabel.textContent = diseaseDisplayName(scanResult.disease || selectedRecord.type);
  }
  if (detailImageStage && scanResult.imageUri) {
    detailImageStage.style.backgroundImage = `linear-gradient(rgba(10,18,14,0.08), rgba(10,18,14,0.18)), url('${scanResult.imageUri}')`;
    detailImageStage.style.backgroundSize = "cover";
    detailImageStage.style.backgroundPosition = "center";
  }
  if (detailConfidencePill) {
    const confidence = scanResult.confidence
      ? `${Math.round(scanResult.confidence * 100)}% confidence`
      : "No AI confidence";
    detailConfidencePill.textContent = confidence;
  }
  if (detailPredictedCondition) {
    detailPredictedCondition.textContent = diseaseDisplayName(scanResult.disease || selectedRecord.type);
  }
  if (detailSuggestedAction) {
    detailSuggestedAction.textContent = treatmentSteps[0] || "Review the record and choose the next operational action.";
  }
  if (detailReviewerNote) {
    detailReviewerNote.textContent =
      selectedReview?.reviewNote ||
      "No admin review note has been stored for this record yet.";
  }

  const latestReading = summary?.nodes?.latestReading;
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
        title: `${selectedRecord.title || "Record submitted"}`,
        note: selectedRecord.description || "Stored in backend records history.",
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
            note: "No live sensor reading is currently available from the backend.",
          },
    ],
  );
}

async function hydrateAdminFromBackend({ silent = false } = {}) {
  if (cloudRefreshInFlight) {
    return latestAdminSummary;
  }
  cloudRefreshInFlight = true;
  if (!silent) {
    setCloudRefreshState("Syncing...", true);
  }
  try {
    const [summaryPayload, usersPayload, jobsPayload, alertsPayload, recordsPayload, rulePayload, profileSettingsPayload] = await Promise.all([
      apiRequest("/api/admin/summary"),
      apiRequest("/api/admin/users"),
      apiRequest("/api/analysis-jobs?limit=25"),
      apiRequest("/api/alerts"),
      apiRequest("/api/records"),
      apiRequest("/api/admin/settings/alert-rules"),
      apiRequest("/api/settings/profile"),
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
    applySettingsView(rulePayload.rule, profileSettingsPayload.settings, summaryPayload);
    if (!silent) {
      setCloudRefreshState("Refresh cloud data", false);
    }
    return summaryPayload;
  } catch (error) {
    setBackendDisconnectedState(error.message || "Backend hydration failed.");
    if (!silent) {
      setCloudRefreshState("Retry cloud data", false);
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
    nodeId: reading.nodeId || "node_demo",
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

  const scanResult = job.scanResult || {};
  const confidence = Number(scanResult.confidence || 0);
  if (analysisResultDisease) {
    analysisResultDisease.textContent = scanResult.disease
      ? diseaseDisplayName(scanResult.disease)
      : "Diagnosis pending";
  }
  if (analysisResultConfidence) {
    analysisResultConfidence.textContent = scanResult.disease
      ? `${Math.round(confidence * 100)}% confidence from ${job.transport || "backend queue"}`
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
            ? "AI analysis completed and the scan record is now available in the backend history."
            : job.error || "The backend could not complete this AI analysis job.";
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
        analysisUploadNote.textContent = "The AI job is still running. Refresh the analysis page to check the latest backend status.";
      }
    }
  }, 2000);
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
      analysisUploadNote.textContent = "No ESP32-CAM frame is available yet.";
    }
    setAnalysisSubmitState("No frame", "warning");
    return;
  }

  setAnalysisControlsBusy(true);
  setAnalysisSubmitState("Queued", "accent");
  if (analysisUploadNote) {
    analysisUploadNote.textContent = "Sending latest ESP32-CAM frame to the AI analysis queue.";
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
    const disease = job.scanResult?.disease || "unknown";
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
    analysisQueueWaiting.textContent = `${queuedJobs.length} jobs waiting for local Ollama processing after upload.`;
  }
  if (analysisQueueRunning) {
    analysisQueueRunning.textContent = `${runningJobs.length} active jobs are still moving through the backend queue.`;
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
        note: alert.message || "Threshold condition recorded.",
      })),
      ...(completedJobs || []).slice(0, 1).map((job) => ({
        time: formatDateTime(job.updatedAt),
        title: `AI job completed: ${diseaseDisplayName(job.scanResult?.disease)}`,
        note: job.liveAssessment?.sensorSummary || job.scanResult?.description || "Structured scan result stored.",
      })),
      ...(records || []).slice(0, 1).map((record) => ({
        time: formatDateTime(record.timestamp),
        title: `${record.title || "Record saved"}`,
        note: record.description || "Activity record captured in the backend.",
      })),
    ],
  );
}

function applyMonitorView(summary) {
  const latestReading = summary.nodes?.latestReading;
  const latestFrame = summary.nodes?.latestFrame;
  const unreadAlerts = summary.alerts?.unread || 0;
  const databaseKind = summary.database?.kind || "database";
  const databaseNote = summary.database?.fallbackActive
    ? `${databaseKind} fallback active; cloud database is degraded`
    : `${databaseKind} connected with admin summary available`;

  if (monitorBackendStatus) {
    monitorBackendStatus.textContent = summary.database?.fallbackActive ? "Degraded" : "Healthy";
  }
  if (monitorBackendNote) {
    monitorBackendNote.textContent = databaseNote;
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
  if (monitorCameraPreview && latestFrame?.imageUrl) {
    monitorCameraPreview.style.backgroundImage = `linear-gradient(rgba(10,18,14,0.08), rgba(10,18,14,0.18)), url('${latestFrame.imageUrl}')`;
    monitorCameraPreview.style.backgroundSize = "cover";
    monitorCameraPreview.style.backgroundPosition = "center";
  }
  renderTimeline(
    monitorFreshnessLog,
    [
      {
        time: summary.generatedAt ? formatDateTime(summary.generatedAt) : "Now",
        title: "Admin summary refreshed",
        note: "Dashboard, monitor, and settings now reflect backend data.",
      },
      latestFrame
        ? {
            time: formatDateTime(latestFrame.uploadedAt || latestFrame.capturedAt),
            title: "Camera frame stored",
            note: "ESP32-CAM image is available through the backend history endpoint.",
          }
        : {
            time: "Pending",
            title: "Camera frame pending",
            note: "Waiting for demo hardware or the real camera module to upload a frame.",
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
            note: "No reading has been posted by the demo or physical hardware yet.",
          },
    ],
  );
}

function applySettingsView(rule, settings, summary) {
  const effectiveRule = { ...DEFAULT_ALERT_RULE_SETTINGS, ...(rule || {}) };
  const effectiveSettings = { ...DEFAULT_PROFILE_SETTINGS, ...(settings || {}) };

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
    settingsAnalysisMode.textContent = effectiveSettings.backendMode
      ? `${effectiveSettings.backendMode} + local relay`
      : "Local + relay";
  }
  if (settingsBackendUrl) {
    settingsBackendUrl.textContent = API_BASE_URL;
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
      ? "Fallback active"
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
    throw new Error("Use an admin account for the web console.");
  }

  const healthPayload = await readBackendHealth();
  adminToken = payload.token || "";
  if (adminToken) {
    window.localStorage.setItem("pineguardAdminToken", adminToken);
  }
  return healthPayload;
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
    const databaseKind =
      healthPayload.databaseDetails?.kind || healthPayload.database || "database";
    const fallbackActive = !!healthPayload.databaseDetails?.fallbackActive;
    setLoginFeedback(
      fallbackActive
        ? `Connected to the backend using ${databaseKind} fallback data.`
        : `Connected to the backend ${databaseKind} data service.`,
      "success",
    );
  } catch (error) {
    adminToken = "";
    clearInterval(cloudRefreshTimer);
    window.localStorage.removeItem("pineguardAdminToken");
    adminView?.classList.add("is-hidden");
    loginView?.classList.remove("is-hidden");
    setLoginFeedback(
      error.message || "Unable to sign in to the admin backend.",
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
    await hydrateAdminFromBackend();
    if (settingsConfigStatus) {
      settingsConfigStatus.textContent = "Saved";
      settingsConfigStatus.classList.remove("warning");
    }
  } catch (error) {
    setBackendDisconnectedState(error.message || "Saving backend settings failed.");
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
    await hydrateAdminFromBackend();
    if (settingsConfigStatus) {
      settingsConfigStatus.textContent = "Defaults restored";
      settingsConfigStatus.classList.remove("warning");
    }
  } catch (error) {
    setBackendDisconnectedState(error.message || "Resetting backend settings failed.");
  } finally {
    setButtonBusy(resetSettingsButton, "", false);
  }
});

refreshCloudDataButton?.addEventListener("click", async () => {
  try {
    await hydrateAdminFromBackend();
  } catch (_) {
    // The backend status cards already show the current failure state.
  }
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
    location: "Trial Plot East",
    note: "Daily crop observation and scan submission for the new farmer demo account.",
  },
};

function applyUserPreset(role) {
  const preset = presetDefaults[role];
  if (!preset || !demoUserRole) {
    return;
  }

  demoUserRole.value = role;
  if (demoUserName) demoUserName.value = preset.name;
  if (demoUserEmail) demoUserEmail.value = preset.email;
  if (demoUserLocation) demoUserLocation.value = preset.location;
  if (demoUserNote) demoUserNote.value = preset.note;
}

presetButtons.forEach((button) => {
  button.addEventListener("click", () => {
    const role = button.dataset.userPreset;
    if (role) {
      applyUserPreset(role);
    }
  });
});

let createdUserCount = 0;

demoUserForm?.addEventListener("submit", async (event) => {
  event.preventDefault();

  const name = demoUserName?.value.trim() || "Demo User";
  const email = demoUserEmail?.value.trim() || "demo@pineguard.local";
  const roleValue = normalizeManagedUserRole(demoUserRole?.value).value;
  const location = demoUserLocation?.value.trim() || "Demo location";
  const note = demoUserNote?.value.trim() || "Prototype role note";

  try {
    if (adminToken) {
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
        }),
      });
      await hydrateAdminFromBackend();
      demoUserForm.reset();
      applyUserPreset("farmer");
      return;
    }
  } catch (error) {
    console.warn("Backend user creation failed, falling back to local row.", error);
  }

  const role = normalizeManagedUserRole(roleValue);

  const row = document.createElement("tr");
  row.innerHTML = `
    <td>${name}<br /><small>${email}</small></td>
    <td>${role.label}</td>
    <td>${location}</td>
    <td>${role.scope}<br /><small>${note}</small></td>
    <td>Just now</td>
    <td><span class="pill accent">Demo added</span></td>
    <td><button class="table-action-button danger" type="button">Remove</button></td>
  `;
  row.querySelector(".table-action-button")?.addEventListener("click", () => {
    row.remove();
    createdUserCount = Math.max(0, createdUserCount - 1);
    if (demoUserCount) {
      demoUserCount.textContent = String(createdUserCount).padStart(2, "0");
    }
  });

  userDirectoryBody?.prepend(row);
  createdUserCount += 1;

  if (demoUserCount) {
    demoUserCount.textContent = String(createdUserCount).padStart(2, "0");
  }
});
