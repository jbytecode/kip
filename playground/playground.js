import { highlightJs, highlightKeywords } from "./kip-syntax.js";

const workerUrl = new URL("./kip-worker.js", import.meta.url);

const sourceEl = document.getElementById("source");
const outputEl = document.getElementById("output");
const terminalBodyEl = document.getElementById("terminal-body");
const terminalInputEl = document.getElementById("terminal-input");
const terminalInputField = document.getElementById("terminal-input-field");
const codeSampleEl = document.getElementById("code-sample");
const sourceHighlightEl = document.getElementById("source-highlight");
const runBtn = document.getElementById("run");
const codegenBtn = document.getElementById("codegen");
const stopBtn = document.getElementById("stop");
const uiLangButtons = Array.from(document.querySelectorAll("[data-ui-lang]"));
const exampleEl = document.getElementById("example");
const codegenPanelEl = document.getElementById("codegen-panel");
const codegenOutputEl = document.getElementById("codegen-output");
const panelsEl = document.querySelector(".playground-panels");
const panelDividerEl = document.getElementById("panel-divider");
const playgroundShellEl = document.querySelector(".playground-shell");
const fullscreenBtn = document.getElementById("fullscreen");

let busyCursor = false;
let currentLanguage = "en";

const supportedLanguages = new Set(["en", "tr"]);
const languageStorageKey = "kip-playground-ui-language";

const translations = {
  en: {
    "meta.title": "Kip",
    "meta.description": "Kip is an experimental programming language in Turkish where grammatical case and mood are part of the type system.",
    "topbar.languageSwitchAria": "Language switch",
    "aria.github": "Kip on GitHub",
    "aria.twitter": "Kip on Twitter",
    "aria.vscode": "Kip on VS Code Marketplace",
    "hero.title": "A programming language in Turkish where <span class=\"accent\">grammatical case</span> and <span class=\"accent\">mood</span> are part of the type system.",
    "hero.subtitle1": "Kip is an experimental language that integrates Turkish morphology into typing, exploring the overlap between linguistics and type theory.",
    "hero.subtitle2": "In Kip, pure function signatures are <span class=\"accent bold\">noun phrases</span>; effectful ones are <span class=\"accent bold\">infinitives</span> invoked in the <span class=\"accent bold\">imperative</span>. Every argument bears a grammatical case (nominative, accusative, dative, and so on), and case is part of the function's type. When cases are distinct, arguments can be supplied in any order.",
    "hero.subtitle3": "Kip also resolves natural-language ambiguity with type-directed disambiguation: it preserves multiple parses through parsing and lets elaboration and type checking select the intended one.",
    "hero.openPlayground": "Open the playground",
    "hero.openLibBrowser": "Browse standard library files",
    "hero.tutorialTr": "<span class=\"badge-dot\"></span>Tutorial (Turkish)",
    "hero.tutorialEn": "<span class=\"badge-dot\"></span>Tutorial (English)",
    "hero.exampleAria": "Kip example",
    "hero.examplePill": "Example",
    "hero.exampleMuted": "defining and using a linked list type",
    "features.title": "Features",
    "features.subtitle": "A small set of ideas that make Kip different.",
    "features.card1.title": "Grammar-aware types",
    "features.card1.body": "Grammatical case and mood are part of the type system, so argument order can vary when cases differ.",
    "features.card2.title": "Type-directed disambiguation",
    "features.card2.body": "Parsing preserves alternatives; type checking selects the intended meaning.",
    "features.card3.title": "Bytecode caches",
    "features.card3.body": "Type-checked modules are cached as <code>.iz</code> files to speed up repeated runs.",
    "features.card4.title": "JavaScript codegen",
    "features.card4.body": "Generate JS with <code>kip --codegen js</code> for experimenting outside the REPL.",
    "playground.title": "Playground",
    "playground.subtitle": "Write custom Kip code, or pick an existing example, and run it.",
    "playground.example": "Example:",
    "playground.custom": "Custom",
    "playground.run": "Run",
    "playground.stop": "Stop",
    "playground.codegen": "Generate JavaScript",
    "playground.fullscreen": "Fullscreen",
    "playground.source": "Source",
    "playground.output": "Output",
    "playground.generatedJs": "Generated JavaScript",
    "playground.resizePanels": "Resize panels",
    "footer.subtitle": "A programming language in Turkish where grammatical case and mood are part of the type system.",
    "messages.stdinUnavailable": "(stdin unavailable)",
    "messages.interactiveRequiresIsolation": "Interactive input requires cross-origin isolation (COOP/COEP).",
    "messages.runningWithoutStdin": "Running without stdin support.",
    "messages.failedToLoad": "Failed to load",
    "messages.unknownError": "Unknown error",
  },
  tr: {
    "meta.title": "Kip",
    "meta.description": "Kip, ismin halleri ve eylem kiplerinin tip sisteminin bir parçası olduğu deneysel bir Türkçe programlama dilidir.",
    "topbar.languageSwitchAria": "Dil seçimi",
    "aria.github": "GitHub'da Kip",
    "aria.twitter": "Twitter'da Kip",
    "aria.vscode": "VS Code Marketplace'te Kip",
    "hero.title": "<span class=\"accent\">İsmin halleri</span> ve <span class=\"accent\">eylem kiplerinin</span> tip sisteminin bir parçası olduğu Türkçe bir programlama dili.",
    "hero.subtitle1": "Kip, Türkçe morfolojiyi tiplemeye entegre eden; dilbilim ile tip kuramı arasındaki kesişimi inceleyen deneysel bir dildir.",
    "hero.subtitle2": "Kip'te saf fonksiyon imzaları <span class=\"accent bold\">isim tamlaması</span>; etkili fonksiyonlar ise <span class=\"accent bold\">mastar</span> yapısında olup <span class=\"accent bold\">emir kipi</span>yle çağrılır. Her argüman bir hal taşır ve bu haller, fonksiyon tipinin bir parçasıdır. Haller farklı olduğunda argümanlar herhangi bir sırada verilebilir.",
    "hero.subtitle3": "Kip ayrıca doğal dil belirsizliğini tür yönlendirmeli ayrıştırma ile çözer: Birden fazla ayrıştırmayı korur, ardından genişletme ve tip denetimi doğru olanı seçer.",
    "hero.openPlayground": "Deneme tahtasını aç",
    "hero.openLibBrowser": "Standart kütüphane dosyalarını aç",
    "hero.tutorialTr": "<span class=\"badge-dot\"></span>Kılavuz (Türkçe)",
    "hero.tutorialEn": "<span class=\"badge-dot\"></span>Tutorial (İngilizce)",
    "hero.exampleAria": "Kip örneği",
    "hero.examplePill": "Örnek",
    "hero.exampleMuted": "bağlı liste tipini tanımlama ve kullanma",
    "features.title": "Özellikler",
    "features.subtitle": "Kip'i farklı yapan fikirlerden kısa bir seçki.",
    "features.card1.title": "Dilbilgisine duyarlı tipler",
    "features.card1.body": "İsmin halleri ve eylem kipleri tip sisteminin parçasıdır; haller farklı olduğunda argüman sırası değişebilir.",
    "features.card2.title": "Tip yönlendirmeli ayrıştırma",
    "features.card2.body": "Ayrıştırma seçenekleri korunur; tip denetimi hedeflenen anlamı seçer.",
    "features.card3.title": "Bytecode önbellekleri",
    "features.card3.body": "Tip denetlenmiş modüller tekrar çalışmaları hızlandırmak için <code>.iz</code> dosyaları olarak önbelleğe alınır.",
    "features.card4.title": "JavaScript üretimi",
    "features.card4.body": "REPL dışında denemek için <code>kip --codegen js</code> ile JS üretin.",
    "playground.title": "Deneme tahtası",
    "playground.subtitle": "Kendi Kip kodunuzu yazın ya da bir örnek seçip çalıştırın.",
    "playground.example": "Örnek:",
    "playground.custom": "Özel",
    "playground.run": "Çalıştır",
    "playground.stop": "Durdur",
    "playground.codegen": "JavaScript üret",
    "playground.fullscreen": "Tam ekran",
    "playground.source": "Kaynak",
    "playground.output": "Çıktı",
    "playground.generatedJs": "Üretilen JavaScript",
    "playground.resizePanels": "Panelleri yeniden boyutlandır",
    "footer.subtitle": "İsmin halleri ve eylem kiplerinin tip sisteminin bir parçası olduğu Türkçe bir programlama dili.",
    "messages.stdinUnavailable": "(stdin kullanılamıyor)",
    "messages.interactiveRequiresIsolation": "Etkileşimli girdi için cross-origin isolation (COOP/COEP) gerekir.",
    "messages.runningWithoutStdin": "stdin desteği olmadan çalıştırılıyor.",
    "messages.failedToLoad": "Yüklenemedi",
    "messages.unknownError": "Bilinmeyen hata",
  },
};

function setBusyCursor(isBusy) {
  if (busyCursor === isBusy) {
    return;
  }
  busyCursor = isBusy;
  if (document.body) {
    document.body.classList.toggle("busy-cursor", isBusy);
  }
}

const examples = [
  { id: "selamlamak", file: "selamlamak.kip", labels: { en: "Greeting", tr: "Selamlamak" } },
  { id: "gün-örneği", file: "gün-örneği.kip", labels: { en: "Day Example", tr: "Gün Örneği" } },
  { id: "bir-fazlası", file: "bir-fazlası.kip", labels: { en: "Increment", tr: "Bir Fazlası" } },
  { id: "fibonacci", file: "fibonacci.kip", labels: { en: "Fibonacci", tr: "Fibonacci" } },
  { id: "asal-sayılar", file: "asal-sayılar.kip", labels: { en: "Prime Numbers", tr: "Asal Sayılar" } },
  { id: "sayı-tahmin-oyunu", file: "sayı-tahmin-oyunu.kip", labels: { en: "Number Guessing Game", tr: "Sayı Tahmin Oyunu" } },
  { id: "ikili-ağaç-araması", file: "ikili-ağaç-araması.kip", labels: { en: "Binary Tree Search", tr: "İkili Ağaç Araması" } },
  { id: "dosya-io", file: "dosya-io.kip", labels: { en: "File I/O", tr: "Dosya G/Ç" } },
];

sourceEl.value = `(bu tam-sayı listesini) bastırmak,\n  bu boşsa,\n    durmaktır;\n  ilkin devama ekiyse,\n    ilki yazıp,\n    devamı bastırmaktır.\n\n((1'in (2'nin boşa ekine) ekinin) tersini) bastır.`;

function t(key) {
  return translations[currentLanguage][key] ?? translations.en[key] ?? key;
}

function updateMetaTags() {
  document.title = t("meta.title");
  const description = t("meta.description");
  const descriptionEl = document.querySelector("meta[name='description']");
  const ogDescriptionEl = document.querySelector("meta[property='og:description']");
  const twitterDescriptionEl = document.querySelector("meta[name='twitter:description']");
  if (descriptionEl) {
    descriptionEl.setAttribute("content", description);
  }
  if (ogDescriptionEl) {
    ogDescriptionEl.setAttribute("content", description);
  }
  if (twitterDescriptionEl) {
    twitterDescriptionEl.setAttribute("content", description);
  }
}

function applyTranslations() {
  document.documentElement.lang = currentLanguage;
  updateMetaTags();
  document.querySelectorAll("[data-i18n]").forEach((node) => {
    const key = node.dataset.i18n;
    node.textContent = t(key);
  });
  document.querySelectorAll("[data-i18n-html]").forEach((node) => {
    const key = node.dataset.i18nHtml;
    node.innerHTML = t(key);
  });
  document.querySelectorAll("[data-i18n-aria-label]").forEach((node) => {
    const key = node.dataset.i18nAriaLabel;
    node.setAttribute("aria-label", t(key));
  });
}

function applyLanguageToggleState() {
  uiLangButtons.forEach((button) => {
    const isActive = button.dataset.uiLang === currentLanguage;
    button.classList.toggle("is-active", isActive);
    button.setAttribute("aria-pressed", String(isActive));
  });
}

function getLanguageFromUrl() {
  const params = new URLSearchParams(window.location.search);
  const lang = params.get("lang");
  const normalized = lang ? lang.toLowerCase() : null;
  if (normalized && supportedLanguages.has(normalized)) {
    return normalized;
  }
  return null;
}

function syncLanguageToUrl(nextLanguage) {
  const url = new URL(window.location.href);
  url.searchParams.set("lang", nextLanguage);
  window.history.replaceState({}, "", url);
}

function detectPreferredLanguage() {
  const urlLanguage = getLanguageFromUrl();
  if (urlLanguage) {
    return urlLanguage;
  }
  try {
    const stored = window.localStorage.getItem(languageStorageKey);
    if (stored && supportedLanguages.has(stored)) {
      return stored;
    }
  } catch (_err) {
    // Ignore storage failures.
  }

  const navigatorLanguages = [
    ...(Array.isArray(navigator.languages) ? navigator.languages : []),
    navigator.language,
  ];
  for (const rawLanguage of navigatorLanguages) {
    if (!rawLanguage) {
      continue;
    }
    const normalized = String(rawLanguage).toLowerCase();
    if (normalized.startsWith("tr")) {
      return "tr";
    }
  }
  return "en";
}

function setLanguage(nextLanguage, options = {}) {
  const { persist = true, syncUrl = true } = options;
  const normalized = supportedLanguages.has(nextLanguage) ? nextLanguage : "en";
  currentLanguage = normalized;
  applyLanguageToggleState();
  applyTranslations();
  buildExampleOptions();
  if (persist) {
    try {
      window.localStorage.setItem(languageStorageKey, normalized);
    } catch (_err) {
      // Ignore storage failures.
    }
  }
  if (syncUrl) {
    syncLanguageToUrl(normalized);
  }
}

async function loadText(path) {
  const res = await fetch(path);
  if (!res.ok) {
    throw new Error(`${t("messages.failedToLoad")} ${path}`);
  }
  return res.text();
}

function syncHighlight() {
  if (sourceHighlightEl) {
    sourceHighlightEl.innerHTML = `${highlightKeywords(sourceEl.value)}\n`;
  }
}

function highlightCodeSample() {
  if (!codeSampleEl) {
    return;
  }
  codeSampleEl.innerHTML = highlightKeywords(codeSampleEl.textContent || "");
}

function clearTerminal() {
  outputEl.textContent = "";
  hideTerminalInput();
}

function appendTerminalLine(line) {
  outputEl.textContent += `${line}\n`;
  if (terminalBodyEl) {
    terminalBodyEl.scrollTop = terminalBodyEl.scrollHeight;
  }
}

function showTerminalInput() {
  if (!terminalInputEl || !terminalInputField) {
    return;
  }
  terminalInputEl.classList.remove("hidden");
  terminalInputField.value = "";
  terminalInputField.focus();
  if (terminalBodyEl) {
    terminalBodyEl.scrollTop = terminalBodyEl.scrollHeight;
  }
}

function hideTerminalInput() {
  if (!terminalInputEl || !terminalInputField) {
    return;
  }
  terminalInputEl.classList.add("hidden");
  terminalInputField.value = "";
}

let activeWorker = null;
let inputSignalView = null;
let inputBufferView = null;
let pendingInput = false;
let interactiveSupported = true;
let activeMode = null;
let codegenLines = [];
let isFullscreen = false;
let activeAction = null;
let workerInitialized = false;
// Optimization: ensure warmup is requested only once per worker lifecycle.
let runtimePreloadRequested = false;
// Runtime mode reported by worker after wasm export inspection.
// Values:
// - "command": start/exit process model per run
// - "reactor": reusable `kip_run` entrypoint on a persistent instance
let workerRuntimeMode = null;

function setRunState(isRunning) {
  runBtn.disabled = isRunning;
  if (codegenBtn) {
    codegenBtn.disabled = isRunning;
  }
  if (stopBtn) {
    stopBtn.disabled = !isRunning;
  }
}

function setActiveAction(nextAction) {
  activeAction = nextAction;
  if (runBtn) {
    const isActive = activeAction === "run";
    runBtn.classList.toggle("is-active", isActive);
    runBtn.setAttribute("aria-pressed", String(isActive));
  }
  if (codegenBtn) {
    const isActive = activeAction === "codegen";
    codegenBtn.classList.toggle("is-active", isActive);
  }
}

function setFullscreen(nextFullscreen) {
  isFullscreen = nextFullscreen;
  if (playgroundShellEl) {
    playgroundShellEl.classList.toggle("fullscreen", isFullscreen);
  }
  if (document.body) {
    document.body.classList.toggle("playground-fullscreen", isFullscreen);
  }
  if (fullscreenBtn) {
    fullscreenBtn.setAttribute("aria-pressed", String(isFullscreen));
    fullscreenBtn.classList.toggle("is-active", isFullscreen);
  }
}

function setCodegenVisible(isVisible) {
  if (!codegenPanelEl) {
    return;
  }
  codegenPanelEl.classList.toggle("hidden", !isVisible);
  if (codegenBtn) {
    codegenBtn.setAttribute("aria-pressed", String(isVisible));
  }
}

function terminateWorker() {
  if (activeWorker) {
    // Explicitly dropping the worker resets all worker-side caches and singleton state.
    activeWorker.terminate();
    activeWorker = null;
    workerInitialized = false;
  }
  runtimePreloadRequested = false;
  inputSignalView = null;
  inputBufferView = null;
  pendingInput = false;
}

function ensureWorker() {
  if (!activeWorker) {
    // Optimization: worker is long-lived and reused across Run/Codegen requests.
    const worker = new Worker(workerUrl, { type: "module" });
    activeWorker = worker;
    worker.addEventListener("message", handleWorkerMessage);
    worker.addEventListener("error", (event) => {
      setBusyCursor(false);
      if (activeMode === "codegen") {
        codegenLines.push(String(event.message || event.error || event));
        if (codegenOutputEl) {
          codegenOutputEl.innerHTML = highlightJs(codegenLines.join("\n"));
        }
      } else {
        appendTerminalLine(String(event.message || event.error || event));
      }
      setRunState(false);
      setActiveAction(null);
    });
    workerInitialized = true;
  }
  return activeWorker;
}

function createInputBuffers() {
  if (typeof SharedArrayBuffer === "undefined" || !crossOriginIsolated) {
    interactiveSupported = false;
    return { signal: null, buffer: null };
  }
  const signal = new SharedArrayBuffer(8);
  const buffer = new SharedArrayBuffer(65536);
  inputSignalView = new Int32Array(signal);
  inputBufferView = new Uint8Array(buffer);
  return { signal, buffer };
}

function requestRuntimePreload() {
  if (runtimePreloadRequested) {
    return;
  }
  // Optimization: start worker + preload on first user interaction so
  // first Run avoids paying full download/compile/init latency.
  runtimePreloadRequested = true;
  const worker = ensureWorker();
  worker.postMessage({ type: "preload" });
}

function handleWorkerMessage(event) {
  const { type, line, error, runtimeMode } = event.data || {};
  switch (type) {
    case "preloaded":
      // Recorded for diagnostics/future UX tuning; execution logic remains worker-driven.
      workerRuntimeMode = runtimeMode ?? workerRuntimeMode;
      break;
    case "preload-error":
      console.warn("Playground preload failed:", error ?? "unknown error");
      break;
    case "stdout":
      setBusyCursor(false);
      if (activeMode === "codegen") {
        codegenLines.push(line ?? "");
        if (codegenOutputEl) {
          codegenOutputEl.innerHTML = highlightJs(codegenLines.join("\n"));
        }
      } else {
        appendTerminalLine(line ?? "");
      }
      break;
    case "stderr":
      setBusyCursor(false);
      if (activeMode === "codegen") {
        codegenLines.push(line ?? "");
        if (codegenOutputEl) {
          codegenOutputEl.innerHTML = highlightJs(codegenLines.join("\n"));
        }
      } else {
        appendTerminalLine(line ?? "");
      }
      break;
    case "stdin-request":
      setBusyCursor(false);
      if (activeMode === "codegen") {
        break;
      }
      if (!interactiveSupported) {
        appendTerminalLine(t("messages.stdinUnavailable"));
        break;
      }
      pendingInput = true;
      showTerminalInput();
      break;
    case "exit":
      pendingInput = false;
      hideTerminalInput();
      setBusyCursor(false);
      setRunState(false);
      setActiveAction(null);
      break;
    case "error":
      setBusyCursor(false);
      if (activeMode === "codegen") {
        codegenLines.push(error ?? t("messages.unknownError"));
        if (codegenOutputEl) {
          codegenOutputEl.innerHTML = highlightJs(codegenLines.join("\n"));
        }
      } else {
        appendTerminalLine(error ?? t("messages.unknownError"));
      }
      pendingInput = false;
      hideTerminalInput();
      setRunState(false);
      setActiveAction(null);
      break;
    default:
      break;
  }
}

function sendInput(value) {
  if (!pendingInput || !inputSignalView || !inputBufferView) {
    return;
  }
  const encoder = new TextEncoder();
  const bytes = encoder.encode(`${value}\n`);
  const limit = Math.min(bytes.length, inputBufferView.length);
  inputBufferView.fill(0);
  inputBufferView.set(bytes.slice(0, limit));
  inputSignalView[1] = limit;
  Atomics.store(inputSignalView, 0, 2);
  Atomics.notify(inputSignalView, 0, 1);
  pendingInput = false;
}

async function runKip() {
  setBusyCursor(true);
  setRunState(true);
  setActiveAction("run");
  clearTerminal();
  activeMode = "run";
  interactiveSupported = true;

  const { signal, buffer } = createInputBuffers();
  if (!interactiveSupported) {
    setBusyCursor(false);
    appendTerminalLine(t("messages.interactiveRequiresIsolation"));
    appendTerminalLine(t("messages.runningWithoutStdin"));
  }

  const worker = ensureWorker();
  const args = ["kip-playground", "--exec", "/main.kip", "--lang", currentLanguage];
  worker.postMessage({
    type: "run",
    source: sourceEl.value,
    args,
    signal,
    buffer,
  });
}

function runCodegen() {
  if (!codegenOutputEl || !codegenPanelEl) {
    return;
  }
  if (!codegenPanelEl.classList.contains("hidden")) {
    setBusyCursor(false);
    pendingInput = false;
    hideTerminalInput();
    activeMode = null;
    setRunState(false);
    setActiveAction(null);
    setCodegenVisible(false);
    return;
  }
  setBusyCursor(true);
  setRunState(true);
  setActiveAction("codegen");
  activeMode = "codegen";
  codegenLines = [];
  codegenOutputEl.textContent = "";
  setCodegenVisible(true);

  const worker = ensureWorker();
  const args = ["kip-playground", "--codegen", "js", "/main.kip", "--lang", currentLanguage];
  worker.postMessage({
    type: "run",
    source: sourceEl.value,
    args,
  });
}

function formatExampleLabel(example) {
  if (example.labels && example.labels[currentLanguage]) {
    return example.labels[currentLanguage];
  }
  return example.id;
}

function buildExampleOptions() {
  if (!exampleEl) {
    return;
  }
  const previous = exampleEl.value;
  exampleEl.innerHTML = "";
  exampleEl.appendChild(new Option(t("playground.custom"), "__custom__"));
  for (const example of examples) {
    exampleEl.appendChild(new Option(formatExampleLabel(example), example.id));
  }
  if (previous && (previous === "__custom__" || examples.some((example) => example.id === previous))) {
    exampleEl.value = previous;
  } else {
    exampleEl.value = "__custom__";
  }
}

async function loadExample(exampleId) {
  const example = examples.find((entry) => entry.id === exampleId);
  if (!example) {
    return;
  }
  const source = await loadText(`./assets/examples/${example.file}`);
  sourceEl.value = source;
  syncHighlight();
}

syncHighlight();
highlightCodeSample();
sourceEl.addEventListener("input", syncHighlight);
sourceEl.addEventListener("scroll", () => {
  if (sourceHighlightEl) {
    sourceHighlightEl.scrollTop = sourceEl.scrollTop;
    sourceHighlightEl.scrollLeft = sourceEl.scrollLeft;
  }
});

function setPanelSplit(ratio) {
  if (!panelsEl) {
    return;
  }
  const clamped = Math.min(0.8, Math.max(0.2, ratio));
  panelsEl.style.setProperty("--panel-left", `${clamped}fr`);
  panelsEl.style.setProperty("--panel-right", `${1 - clamped}fr`);
}

function initPanelResize() {
  if (!panelsEl || !panelDividerEl) {
    return;
  }

  let dragging = false;

  const updateFromEvent = (event) => {
    const rect = panelsEl.getBoundingClientRect();
    const offset = event.clientX - rect.left;
    const ratio = offset / rect.width;
    setPanelSplit(ratio);
  };

  const onPointerMove = (event) => {
    if (!dragging) {
      return;
    }
    updateFromEvent(event);
  };

  const stopDragging = () => {
    if (!dragging) {
      return;
    }
    dragging = false;
    panelsEl.classList.remove("resizing");
    window.removeEventListener("pointermove", onPointerMove);
    window.removeEventListener("pointerup", stopDragging);
  };

  panelDividerEl.addEventListener("pointerdown", (event) => {
    dragging = true;
    panelDividerEl.setPointerCapture(event.pointerId);
    panelsEl.classList.add("resizing");
    updateFromEvent(event);
    window.addEventListener("pointermove", onPointerMove);
    window.addEventListener("pointerup", stopDragging);
  });
}

initPanelResize();

if (exampleEl) {
  exampleEl.addEventListener("change", async (event) => {
    const value = event.target.value;
    if (value === "__custom__") {
      return;
    }
    try {
      await loadExample(value);
    } catch (err) {
      appendTerminalLine(String(err));
    }
  });
}

uiLangButtons.forEach((button) => {
  button.addEventListener("click", () => {
    const nextLanguage = button.dataset.uiLang;
    if (!nextLanguage || nextLanguage === currentLanguage) {
      return;
    }
    setLanguage(nextLanguage);
  });
});

if (terminalInputField) {
  terminalInputField.addEventListener("keydown", (event) => {
    if (event.key !== "Enter") {
      return;
    }
    event.preventDefault();
    const value = terminalInputField.value;
    hideTerminalInput();
    appendTerminalLine(`› ${value}`);
    sendInput(value);
  });
}

runBtn.addEventListener("click", runKip);
if (codegenBtn) {
  codegenBtn.addEventListener("click", runCodegen);
}

if (stopBtn) {
  stopBtn.addEventListener("click", () => {
    setBusyCursor(false);
    terminateWorker();
    pendingInput = false;
    hideTerminalInput();
    activeMode = null;
    setRunState(false);
    setActiveAction(null);
  });
}

if (playgroundShellEl) {
  // Optimization: warm caches on earliest likely interaction signal.
  // `pointerdown` usually fires before `click`, so this starts preload sooner.
  playgroundShellEl.addEventListener("pointerdown", requestRuntimePreload, { capture: true, once: true });
  // Optimization: keyboard-only users should also trigger preload.
  playgroundShellEl.addEventListener("focusin", requestRuntimePreload, { capture: true, once: true });
  // Fallback path for user agents where prior events are not delivered as expected.
  playgroundShellEl.addEventListener("click", requestRuntimePreload, { capture: true, once: true });
}

if (fullscreenBtn) {
  fullscreenBtn.addEventListener("click", () => {
    setFullscreen(!isFullscreen);
  });
}

setLanguage(detectPreferredLanguage(), { persist: false, syncUrl: false });
