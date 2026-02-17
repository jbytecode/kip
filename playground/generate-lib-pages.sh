#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
LIB_DIR="${ROOT_DIR}/lib"
DIST_LIB_DIR="${ROOT_DIR}/playground/dist/libs"

repeat_up() {
  local n="$1"
  local out=""
  local i
  for ((i = 0; i < n; i += 1)); do
    out+="../"
  done
  printf "%s" "${out}"
}

lib_files=()
while IFS= read -r src; do
  lib_files+=("${src}")
done < <(find "${LIB_DIR}" -type f -name '*.kip' | sort)

rm -rf "${DIST_LIB_DIR}"
mkdir -p "${DIST_LIB_DIR}"

write_common_head() {
  local title="$1"
  local css_path="$2"
  local body_data_attr="${3:-}"
  cat <<HTML
<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>${title}</title>
    <link rel="icon" type="image/png" href="${LOGO_PATH}" />
    <link rel="stylesheet" href="${css_path}" />
    <style>
      body { background: linear-gradient(180deg, var(--bg2), var(--bg)); }
      .lib-main { padding: 2rem 1.25rem; }
      .lib-main-inner { max-width: 1100px; margin: 0 auto; }
      .lib-title { margin: 0 0 0.8rem; }
      .lib-code { border-radius: 12px; border: 1px solid rgba(148, 163, 184, 0.25); background: rgba(0, 0, 0, 0.28); padding: 1rem; overflow: auto; }
      .lib-code pre { margin: 0; white-space: pre; font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", monospace; font-size: 0.92rem; line-height: 1.5; }
      .lib-list { list-style: none; margin: 0; padding: 0; display: grid; gap: 0.5rem; }
      .lib-list a { color: rgba(226, 232, 240, 0.95); text-decoration: underline; text-underline-offset: 0.2em; }
      .lib-list a:hover { color: var(--accent1); }
    </style>
  </head>
  <body${body_data_attr}>
    <header class="topbar">
      <div class="topbar-inner">
        <a class="brand" href="${HOME_PATH}">
          <img class="brand-logo" src="${LOGO_PATH}" alt="Kip logo" />
          <span class="brand-name">Kip</span>
        </a>

        <div class="topbar-cta">
          <div class="topbar-lang">
            <div class="lang-toggle" role="group" aria-label="Language switch" data-i18n-aria-label="topbar.languageSwitchAria">
              <button type="button" class="lang-toggle-btn" data-ui-lang="en" aria-pressed="false" aria-label="English">
                <span class="lang-label-full">English</span>
                <span class="lang-label-short" aria-hidden="true">En</span>
              </button>
              <button type="button" class="lang-toggle-btn" data-ui-lang="tr" aria-pressed="false" aria-label="Türkçe">
                <span class="lang-label-full">Türkçe</span>
                <span class="lang-label-short" aria-hidden="true">Tr</span>
              </button>
            </div>
          </div>
          <a class="icon-link" href="https://github.com/kip-dili/kip" aria-label="Kip on GitHub" data-i18n-aria-label="aria.github">
            <svg class="icon" viewBox="0 0 24 24" role="img" aria-hidden="true">
              <path d="M12 2a10 10 0 0 0-3.16 19.48c.5.1.68-.22.68-.48v-1.7c-2.77.6-3.35-1.19-3.35-1.19-.45-1.14-1.1-1.45-1.1-1.45-.9-.62.07-.6.07-.6 1 .07 1.52 1.03 1.52 1.03.9 1.52 2.36 1.08 2.94.83.1-.65.35-1.08.64-1.33-2.21-.25-4.54-1.1-4.54-4.9 0-1.08.38-1.96 1.02-2.65-.1-.25-.44-1.27.1-2.65 0 0 .83-.27 2.7 1.01a9.4 9.4 0 0 1 4.92 0c1.87-1.28 2.7-1.01 2.7-1.01.54 1.38.2 2.4.1 2.65.64.7 1.02 1.57 1.02 2.65 0 3.81-2.33 4.65-4.55 4.9.36.32.68.93.68 1.87v2.77c0 .26.18.59.69.48A10 10 0 0 0 12 2z"></path>
            </svg>
          </a>
          <a class="icon-link" href="https://twitter.com/KipDili" aria-label="Kip on Twitter" data-i18n-aria-label="aria.twitter">
            <svg class="icon" viewBox="0 0 24 24" role="img" aria-hidden="true">
              <path d="M19.633 7.997c.013.177.013.355.013.534 0 5.43-4.135 11.69-11.69 11.69-2.322 0-4.482-.68-6.3-1.844.324.038.636.051.973.051 1.92 0 3.687-.648 5.096-1.74a4.124 4.124 0 0 1-3.85-2.86c.25.038.5.063.763.063.363 0 .726-.05 1.064-.138A4.118 4.118 0 0 1 2.3 9.21v-.05c.55.303 1.188.49 1.862.51a4.117 4.117 0 0 1-1.835-3.43c0-.763.2-1.464.55-2.075a11.7 11.7 0 0 0 8.5 4.31 4.63 4.63 0 0 1-.1-.942 4.117 4.117 0 0 1 7.12-2.81 8.09 8.09 0 0 0 2.61-.986 4.1 4.1 0 0 1-1.81 2.27 8.24 8.24 0 0 0 2.36-.636 8.86 8.86 0 0 1-2.06 2.125z"></path>
            </svg>
          </a>
          <a class="icon-link vscode-link" href="https://marketplace.visualstudio.com/items?itemName=kipdili.kipdili" aria-label="Kip on VS Code Marketplace" data-i18n-aria-label="aria.vscode" target="_blank" rel="noopener noreferrer">
            <svg class="icon" viewBox="0 0 24 24" role="img" aria-hidden="true">
              <path d="M17 2 7 6.5v11L17 22l5-2.5V4.5L17 2zm.5 9.3L12 8l5.5-2.5v5.8zm0 7.2L12 16l5.5-3.3v5.8zM9 8.2l6.3 3.8L9 15.8V8.2zM4 8.8 7 11v2L4 15.2V8.8z"></path>
            </svg>
          </a>
        </div>
      </div>
    </header>

    <main id="top" class="lib-main">
      <div class="lib-main-inner">
HTML
}

write_common_footer() {
  cat <<HTML
      </div>
    </main>

    <footer class="footer">
      <div class="footer-inner">
        <div class="footer-left">
          <img class="footer-logo" src="${LOGO_PATH}" alt="Kip logo" />
          <div>
            <div class="footer-title">Kip</div>
            <div class="footer-subtitle" data-i18n="footer.subtitle">A programming language in Turkish where grammatical case and mood are part of the type system.</div>
          </div>
        </div>
        <div class="footer-links">
          <a class="icon-link" href="https://github.com/kip-dili/kip" aria-label="Kip on GitHub" data-i18n-aria-label="aria.github">
            <svg class="icon" viewBox="0 0 24 24" role="img" aria-hidden="true">
              <path d="M12 2a10 10 0 0 0-3.16 19.48c.5.1.68-.22.68-.48v-1.7c-2.77.6-3.35-1.19-3.35-1.19-.45-1.14-1.1-1.45-1.1-1.45-.9-.62.07-.6.07-.6 1 .07 1.52 1.03 1.52 1.03.9 1.52 2.36 1.08 2.94.83.1-.65.35-1.08.64-1.33-2.21-.25-4.54-1.1-4.54-4.9 0-1.08.38-1.96 1.02-2.65-.1-.25-.44-1.27.1-2.65 0 0 .83-.27 2.7 1.01a9.4 9.4 0 0 1 4.92 0c1.87-1.28 2.7-1.01 2.7-1.01.54 1.38.2 2.4.1 2.65.64.7 1.02 1.57 1.02 2.65 0 3.81-2.33 4.65-4.55 4.9.36.32.68.93.68 1.87v2.77c0 .26.18.59.69.48A10 10 0 0 0 12 2z"></path>
            </svg>
          </a>
          <a class="icon-link" href="https://twitter.com/KipDili" aria-label="Kip on Twitter" data-i18n-aria-label="aria.twitter">
            <svg class="icon" viewBox="0 0 24 24" role="img" aria-hidden="true">
              <path d="M19.633 7.997c.013.177.013.355.013.534 0 5.43-4.135 11.69-11.69 11.69-2.322 0-4.482-.68-6.3-1.844.324.038.636.051.973.051 1.92 0 3.687-.648 5.096-1.74a4.124 4.124 0 0 1-3.85-2.86c.25.038.5.063.763.063.363 0 .726-.05 1.064-.138A4.118 4.118 0 0 1 2.3 9.21v-.05c.55.303 1.188.49 1.862.51a4.117 4.117 0 0 1-1.835-3.43c0-.763.2-1.464.55-2.075a11.7 11.7 0 0 0 8.5 4.31 4.63 4.63 0 0 1-.1-.942 4.117 4.117 0 0 1 7.12-2.81 8.09 8.09 0 0 0 2.61-.986 4.1 4.1 0 0 1-1.81 2.27 8.24 8.24 0 0 0 2.36-.636 8.86 8.86 0 0 1-2.06 2.125z"></path>
            </svg>
          </a>
          <a class="icon-link vscode-link" href="https://marketplace.visualstudio.com/items?itemName=kipdili.kipdili" aria-label="Kip on VS Code Marketplace" data-i18n-aria-label="aria.vscode" target="_blank" rel="noopener noreferrer">
            <svg class="icon" viewBox="0 0 24 24" role="img" aria-hidden="true">
              <path d="M17 2 7 6.5v11L17 22l5-2.5V4.5L17 2zm.5 9.3L12 8l5.5-2.5v5.8zm0 7.2L12 16l5.5-3.3v5.8zM9 8.2l6.3 3.8L9 15.8V8.2zM4 8.8 7 11v2L4 15.2V8.8z"></path>
            </svg>
          </a>
        </div>
      </div>
    </footer>
HTML
}

write_i18n_script() {
  cat <<'HTML'
    <script>
      const uiLangButtons = Array.from(document.querySelectorAll("[data-ui-lang]"));
      const supportedLanguages = new Set(["en", "tr"]);
      const languageStorageKey = "kip-playground-ui-language";
      const translations = {
        en: {
          "topbar.languageSwitchAria": "Language switch",
          "aria.github": "Kip on GitHub",
          "aria.twitter": "Kip on Twitter",
          "aria.vscode": "Kip on VS Code Marketplace",
          "libs.indexTitle": "Standard Library Files",
          "libs.pageTitleIndex": "Kip Standard Library Explorer",
          "libs.pageTitleFile": "Kip Standard Library: {file}",
          "footer.subtitle": "A programming language in Turkish where grammatical case and mood are part of the type system.",
        },
        tr: {
          "topbar.languageSwitchAria": "Dil seçimi",
          "aria.github": "GitHub'da Kip",
          "aria.twitter": "Twitter'da Kip",
          "aria.vscode": "VS Code Marketplace'te Kip",
          "libs.indexTitle": "Standart Kütüphane Dosyaları",
          "libs.pageTitleIndex": "Kip Standart Kütüphane Gezgini",
          "libs.pageTitleFile": "Kip Standart Kütüphane: {file}",
          "footer.subtitle": "İsmin halleri ve eylem kiplerinin tip sisteminin bir parçası olduğu Türkçe bir programlama dili.",
        },
      };

      function getLanguageFromUrl() {
        const params = new URLSearchParams(window.location.search);
        const lang = params.get("lang");
        const normalized = lang ? lang.toLowerCase() : null;
        if (normalized && supportedLanguages.has(normalized)) {
          return normalized;
        }
        return null;
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
        } catch (_err) {}

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

      function t(lang, key) {
        return translations[lang][key] ?? translations.en[key] ?? key;
      }

      function applyLanguageToggleState(lang) {
        uiLangButtons.forEach((button) => {
          const isActive = button.dataset.uiLang === lang;
          button.classList.toggle("is-active", isActive);
          button.setAttribute("aria-pressed", String(isActive));
        });
      }

      function applyTranslations(lang) {
        document.documentElement.lang = lang;
        document.querySelectorAll("[data-i18n]").forEach((node) => {
          const key = node.dataset.i18n;
          node.textContent = t(lang, key);
        });
        document.querySelectorAll("[data-i18n-aria-label]").forEach((node) => {
          const key = node.dataset.i18nAriaLabel;
          node.setAttribute("aria-label", t(lang, key));
        });

        const pageKind = document.body.dataset.pageKind;
        if (pageKind === "libs-index") {
          document.title = t(lang, "libs.pageTitleIndex");
        } else if (pageKind === "libs-file") {
          const libPath = document.body.dataset.libPath ?? "";
          document.title = t(lang, "libs.pageTitleFile").replace("{file}", libPath);
        }
      }

      const currentLanguage = detectPreferredLanguage();
      applyLanguageToggleState(currentLanguage);
      applyTranslations(currentLanguage);

      uiLangButtons.forEach((button) => {
        button.addEventListener("click", () => {
          const nextLanguage = button.dataset.uiLang;
          if (!nextLanguage || !supportedLanguages.has(nextLanguage)) {
            return;
          }
          try {
            window.localStorage.setItem(languageStorageKey, nextLanguage);
          } catch (_err) {}
          applyLanguageToggleState(nextLanguage);
          applyTranslations(nextLanguage);
        });
      });
    </script>
HTML
}

# libs index page
HOME_PATH="../index.html"
LOGO_PATH="../logo.png"
{
  write_common_head "Kip Standard Library Explorer" "../style.css" ' data-page-kind="libs-index"'
  cat <<'HTML'
        <h1 class="lib-title" data-i18n="libs.indexTitle">Standard Library Files</h1>
        <ul class="lib-list">
HTML

  for src in "${lib_files[@]}"; do
    rel="${src#"${LIB_DIR}/"}"
    href="${rel%.kip}.html"
    printf '          <li><a href="%s">%s</a></li>\n' "${href}" "${rel}"
  done

  cat <<'HTML'
        </ul>
HTML
  write_common_footer
  write_i18n_script
  cat <<'HTML'
  </body>
</html>
HTML
} > "${DIST_LIB_DIR}/index.html"

# file pages
for src in "${lib_files[@]}"; do
  rel="${src#"${LIB_DIR}/"}"
  out_rel="${rel%.kip}.html"
  out_path="${DIST_LIB_DIR}/${out_rel}"
  out_dir="$(dirname "${out_path}")"
  mkdir -p "${out_dir}"

  slash_only="${out_rel//[^\/]/}"
  depth="${#slash_only}"
  up="$(repeat_up $((depth + 1)))"
  css_path="${up}style.css"
  HOME_PATH="${up}index.html"
  LOGO_PATH="${up}logo.png"
  syntax_path="${up}kip-syntax.js"

  display_path="lib/${rel}"
  source_b64="$(base64 < "${src}" | tr -d '\n')"

  {
    write_common_head "Kip Standard Library: ${display_path}" "${css_path}" " data-page-kind=\"libs-file\" data-lib-path=\"${display_path}\""
    cat <<HTML
        <h1 class="lib-title">${display_path}</h1>
        <div class="lib-code">
          <pre id="lib-source"></pre>
        </div>
HTML
    write_common_footer
    cat <<HTML
    <script type="module">
      import { highlightKeywords } from "${syntax_path}";
      const b64 = "${source_b64}";
      const bytes = Uint8Array.from(atob(b64), (ch) => ch.charCodeAt(0));
      const source = new TextDecoder().decode(bytes);
      document.getElementById("lib-source").innerHTML = highlightKeywords(source);
    </script>
HTML
    write_i18n_script
    cat <<'HTML'
  </body>
</html>
HTML
  } > "${out_path}"
done

echo "Generated ${#lib_files[@]} library pages under ${DIST_LIB_DIR}"
