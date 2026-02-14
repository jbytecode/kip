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
    "hero.title": "A programming language in Turkish where <span class=\"accent\">grammatical case</span> and <span class=\"accent\">mood</span> are part of the type system.",
    "hero.subtitle1": "Kip is an experimental language that integrates Turkish morphology into typing, exploring the overlap between linguistics and type theory.",
    "hero.subtitle2": "In Kip, pure function signatures are <span class=\"accent bold\">noun phrases</span>; effectful ones are <span class=\"accent bold\">infinitives</span> invoked in the <span class=\"accent bold\">imperative</span>. Every argument bears a grammatical case (nominative, accusative, dative, and so on), and case is part of the function's type. When cases are distinct, arguments can be supplied in any order.",
    "hero.subtitle3": "Kip also resolves natural-language ambiguity with type-directed disambiguation: it preserves multiple parses through parsing and lets elaboration and type checking select the intended one.",
    "hero.openPlayground": "Open the playground",
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
    "hero.title": "<span class=\"accent\">İsmin halleri</span> ve <span class=\"accent\">eylem kiplerinin</span> tip sisteminin bir parçası olduğu Türkçe bir programlama dili.",
    "hero.subtitle1": "Kip, Türkçe morfolojiyi tiplemeye entegre eden; dilbilim ile tip kuramı arasındaki kesişimi inceleyen deneysel bir dildir.",
    "hero.subtitle2": "Kip'te saf fonksiyon imzaları <span class=\"accent bold\">isim tamlaması</span>; etkili fonksiyonlar ise <span class=\"accent bold\">mastar</span> yapısında olup <span class=\"accent bold\">emir kipi</span>yle çağrılır. Her argüman bir hal taşır ve bu haller, fonksiyon tipinin bir parçasıdır. Haller farklı olduğunda argümanlar herhangi bir sırada verilebilir.",
    "hero.subtitle3": "Kip ayrıca doğal dil belirsizliğini tür yönlendirmeli ayrıştırma ile çözer: Birden fazla ayrıştırmayı korur, ardından genişletme ve tip denetimi doğru olanı seçer.",
    "hero.openPlayground": "Deneme tahtasını aç",
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

const keywordList = [
  "Bir",
  "bir",
  "ya",
  "da",
  "olabilir",
  "var",
  "olamaz",
  "değilse",
  "olsun",
  "olarak",
  "dersek",
  "için",
  "yerleşik",
];

const keywordSet = new Set(keywordList);
const letterPattern = /\p{L}/u;

const jsKeywordList = [
  "break",
  "case",
  "catch",
  "class",
  "const",
  "continue",
  "default",
  "else",
  "export",
  "extends",
  "finally",
  "for",
  "function",
  "if",
  "import",
  "in",
  "instanceof",
  "let",
  "new",
  "return",
  "switch",
  "throw",
  "try",
  "typeof",
  "var",
  "void",
  "while",
  "await",
  "async",
  "of",
];

const jsLiteralList = ["true", "false", "null", "undefined"];
const jsKeywordSet = new Set(jsKeywordList);
const jsLiteralSet = new Set(jsLiteralList);

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

function escapeHtml(text) {
  return text
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/\"/g, "&quot;")
    .replace(/'/g, "&#39;");
}

function highlightJsTokens(text) {
  const tokenPattern = /\b[A-Za-z_]\w*\b|\d+(?:\.\d+)?|[()[\]{}]/g;
  let result = "";
  let lastIndex = 0;
  let match;

  while ((match = tokenPattern.exec(text)) !== null) {
    const token = match[0];
    const start = match.index;
    const end = start + token.length;
    result += escapeHtml(text.slice(lastIndex, start));

    if (token === "(" || token === ")" || token === "[" || token === "]" || token === "{" || token === "}") {
      result += `<span class="kip-paren">${token}</span>`;
    } else if (/^\d/.test(token)) {
      result += `<span class="kip-literal">${escapeHtml(token)}</span>`;
    } else if (jsLiteralSet.has(token)) {
      result += `<span class="kip-literal">${escapeHtml(token)}</span>`;
    } else if (jsKeywordSet.has(token)) {
      result += `<span class="kip-keyword">${escapeHtml(token)}</span>`;
    } else {
      result += escapeHtml(token);
    }

    lastIndex = end;
  }

  result += escapeHtml(text.slice(lastIndex));
  return result;
}

function highlightJs(text) {
  let result = "";
  let lastIndex = 0;
  let i = 0;

  while (i < text.length) {
    const ch = text[i];
    const next = text[i + 1];

    if (ch === "/" && next === "/") {
      result += highlightJsTokens(text.slice(lastIndex, i));
      let j = i + 2;
      while (j < text.length && text[j] !== "\n") {
        j += 1;
      }
      result += `<span class="kip-comment">${escapeHtml(text.slice(i, j))}</span>`;
      i = j;
      lastIndex = i;
      continue;
    }

    if (ch === "/" && next === "*") {
      result += highlightJsTokens(text.slice(lastIndex, i));
      let j = i + 2;
      while (j < text.length) {
        if (text[j] === "*" && text[j + 1] === "/") {
          j += 2;
          break;
        }
        j += 1;
      }
      result += `<span class="kip-comment">${escapeHtml(text.slice(i, j))}</span>`;
      i = j;
      lastIndex = i;
      continue;
    }

    if (ch === "\"" || ch === "'" || ch === "`") {
      result += highlightJsTokens(text.slice(lastIndex, i));
      const quote = ch;
      let j = i + 1;
      while (j < text.length) {
        if (text[j] === "\\\\") {
          j += 2;
          continue;
        }
        if (text[j] === quote) {
          j += 1;
          break;
        }
        j += 1;
      }
      result += `<span class="kip-literal">${escapeHtml(text.slice(i, j))}</span>`;
      i = j;
      lastIndex = i;
      continue;
    }

    i += 1;
  }

  result += highlightJsTokens(text.slice(lastIndex));
  return result;
}

function highlightNonString(text) {
  const tokenPattern = /\d+(?:'?\p{L}+)?|\p{L}+(?:'\p{L}+)?(?:-\p{L}+)*|[(),.]/gu;
  const tokens = [];
  let match;

  while ((match = tokenPattern.exec(text)) !== null) {
    const token = match[0];
    const kind =
      token === "(" || token === ")"
        ? "paren"
        : token === ","
          ? "comma"
          : token === "."
            ? "period"
            : /^\d/.test(token)
              ? "number"
              : "word";
    tokens.push({
      token,
      kind,
      start: match.index,
      end: match.index + token.length,
    });
  }

  const typeWordIndices = new Set();

  for (let i = 0; i < tokens.length; i += 1) {
    if (tokens[i].kind !== "word" || tokens[i].token !== "Bir") {
      continue;
    }
    for (let j = i + 1; j < tokens.length; j += 1) {
      if (tokens[j].kind === "word" && tokens[j].token === "ya") {
        for (let k = i + 1; k < j; k += 1) {
          if (tokens[k].kind === "word") {
            typeWordIndices.add(k);
          }
        }
        break;
      }
    }
  }

  for (let i = 0; i < tokens.length - 1; i += 1) {
    if (
      tokens[i].kind === "word" &&
      tokens[i].token === "ya" &&
      tokens[i + 1].kind === "word" &&
      tokens[i + 1].token === "bir"
    ) {
      for (let j = i + 2; j < tokens.length; j += 1) {
        if (tokens[j].kind === "word" && tokens[j].token === "ya") {
          for (let k = i + 2; k < j; k += 1) {
            if (tokens[k].kind === "word") {
              typeWordIndices.add(k);
            }
          }
          break;
        }
      }
    }
  }

  for (let i = 0; i < tokens.length; i += 1) {
    if (tokens[i].kind !== "word" || tokens[i].token !== "ya") {
      continue;
    }
    let start = i + 1;
    if (
      start < tokens.length &&
      tokens[start].kind === "word" &&
      tokens[start].token === "da"
    ) {
      start += 1;
    }
    let endIndex = -1;
    for (let j = start; j < tokens.length; j += 1) {
      if (tokens[j].kind !== "word") {
        continue;
      }
      if (tokens[j].token === "ya" || tokens[j].token === "olabilir") {
        endIndex = j;
        break;
      }
    }
    if (endIndex === -1 || start >= endIndex) {
      continue;
    }
    const wordIndices = [];
    for (let j = start; j < endIndex; j += 1) {
      if (tokens[j].kind === "word") {
        wordIndices.push(j);
      }
    }
    if (wordIndices.length < 2) {
      continue;
    }
    const lastWordIndex = wordIndices[wordIndices.length - 1];
    for (const index of wordIndices) {
      if (index !== lastWordIndex) {
        typeWordIndices.add(index);
      }
    }
  }

  for (let i = 0; i < tokens.length; i += 1) {
    if (tokens[i].kind !== "word" || tokens[i].token !== "olarak") {
      continue;
    }
    let j = i - 1;
    while (j >= 0 && tokens[j].kind === "word") {
      typeWordIndices.add(j);
      j -= 1;
    }
  }

  let defStart = 0;
  for (let i = 0; i <= tokens.length; i += 1) {
    if (i < tokens.length && tokens[i].kind !== "period") {
      continue;
    }
    let commaIndex = -1;
    for (let j = defStart; j < i; j += 1) {
      if (tokens[j].kind === "comma") {
        commaIndex = j;
        break;
      }
    }
    const parenStack = [];
    let seenTopLevelToken = false;
    for (let j = defStart; j < i; j += 1) {
      const token = tokens[j];
      if (token.kind === "paren") {
        if (token.token === "(") {
          const eligible =
            commaIndex !== -1 &&
            j < commaIndex &&
            !seenTopLevelToken;
          parenStack.push({
            eligible,
            wordIndices: [],
            hasNumber: false,
            hasEligibleChild: false,
          });
        } else if (parenStack.length) {
          const top = parenStack.pop();
          const parent = parenStack.length ? parenStack[parenStack.length - 1] : null;
          if (parent && top.eligible) {
            parent.hasEligibleChild = true;
          }
          if (top.eligible && !top.hasNumber && top.wordIndices.length > 1) {
            for (let k = 1; k < top.wordIndices.length; k += 1) {
              typeWordIndices.add(top.wordIndices[k]);
            }
          } else if (
            top.eligible &&
            !top.hasNumber &&
            top.wordIndices.length === 1 &&
            top.hasEligibleChild
          ) {
            typeWordIndices.add(top.wordIndices[0]);
          }
        }
        continue;
      }
      if (!parenStack.length) {
        if (token.kind === "word" || token.kind === "number") {
          seenTopLevelToken = true;
        }
        continue;
      }
      const top = parenStack[parenStack.length - 1];
      if (!top.eligible) {
        continue;
      }
      if (token.kind === "number") {
        top.hasNumber = true;
        continue;
      }
      if (token.kind === "word") {
        top.wordIndices.push(j);
      }
    }
    defStart = i + 1;
  }

  let result = "";
  let lastIndex = 0;
  for (let i = 0; i < tokens.length; i += 1) {
    const token = tokens[i];
    const { start, end } = token;
    result += escapeHtml(text.slice(lastIndex, start));

    if (token.kind === "paren") {
      result += `<span class="kip-paren">${token.token}</span>`;
    } else if (token.kind === "number") {
      const prev = start > 0 ? text[start - 1] : "";
      if (prev && letterPattern.test(prev)) {
        result += escapeHtml(token.token);
      } else {
        result += `<span class="kip-literal">${escapeHtml(token.token)}</span>`;
      }
    } else if (token.kind === "comma" || token.kind === "period") {
      result += `<span class="kip-keyword">${escapeHtml(token.token)}</span>`;
    } else if (token.kind === "word" && keywordSet.has(token.token)) {
      result += `<span class="kip-keyword">${escapeHtml(token.token)}</span>`;
    } else if (token.kind === "word" && typeWordIndices.has(i)) {
      result += `<span class="kip-type">${escapeHtml(token.token)}</span>`;
    } else {
      result += escapeHtml(token.token);
    }

    lastIndex = end;
  }

  result += escapeHtml(text.slice(lastIndex));
  return result;
}

function highlightKeywords(text) {
  let result = "";
  let lastIndex = 0;
  let i = 0;
  let commentDepth = 0;
  let commentStart = 0;
  let mode = "normal";

  while (i < text.length) {
    if (mode === "comment") {
      if (text[i] === "(" && text[i + 1] === "*") {
        commentDepth += 1;
        i += 2;
        continue;
      }
      if (text[i] === "*" && text[i + 1] === ")") {
        commentDepth -= 1;
        i += 2;
        if (commentDepth === 0) {
          const comment = text.slice(commentStart, i);
          result += `<span class="kip-comment">${escapeHtml(comment)}</span>`;
          lastIndex = i;
          mode = "normal";
        }
        continue;
      }
      i += 1;
      continue;
    }

    if (text[i] === "(" && text[i + 1] === "*") {
      result += highlightNonString(text.slice(lastIndex, i));
      commentDepth = 1;
      commentStart = i;
      mode = "comment";
      i += 2;
      continue;
    }

    if (text[i] === "\"") {
      result += highlightNonString(text.slice(lastIndex, i));
      let j = i + 1;
      while (j < text.length) {
        if (text[j] === "\\\\") {
          j += 2;
          continue;
        }
        if (text[j] === "\"") {
          j += 1;
          break;
        }
        j += 1;
      }
      let end = j;
      const suffixMatch = text.slice(end).match(/^'?\p{L}+/u);
      if (suffixMatch) {
        end += suffixMatch[0].length;
      }
      const literal = text.slice(i, end);
      result += `<span class="kip-literal">${escapeHtml(literal)}</span>`;
      i = end;
      lastIndex = i;
      continue;
    }

    i += 1;
  }

  if (mode === "comment") {
    const comment = text.slice(commentStart);
    result += `<span class="kip-comment">${escapeHtml(comment)}</span>`;
    return result;
  }

  result += highlightNonString(text.slice(lastIndex));
  return result;
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
