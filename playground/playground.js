const workerUrl = new URL("./kip-worker.js", import.meta.url);

const sourceEl = document.getElementById("source");
const outputEl = document.getElementById("output");
const terminalBodyEl = document.getElementById("terminal-body");
const terminalInputEl = document.getElementById("terminal-input");
const terminalInputField = document.getElementById("terminal-input-field");
const codeSampleEl = document.getElementById("code-sample");
const sourceHighlightEl = document.getElementById("source-highlight");
const runBtn = document.getElementById("run");
const langEl = document.getElementById("lang");
const exampleEl = document.getElementById("example");

const keywordList = [
  "Bir",
  "bir",
  "ya",
  "da",
  "olabilir",
  "var",
  "olamaz",
  "değilse",
  "yazdır",
  "diyelim",
  "olsun",
  "olarak",
  "yerleşik",
];

const keywordSet = new Set(keywordList);
const letterPattern = /\p{L}/u;

const examples = [
  { id: "selamlamak", file: "selamlamak.kip" },
  { id: "gün-örneği", file: "gün-örneği.kip" },
  { id: "fibonacci", file: "fibonacci.kip" },
  { id: "asal-sayılar", file: "asal-sayılar.kip" },
  { id: "bir-fazlası", file: "bir-fazlası.kip" },
  { id: "ikili-ağaç-araması", file: "ikili-ağaç-araması.kip" },
  { id: "dosya-io", file: "dosya-io.kip" },
];

sourceEl.value = `(bu tam-sayı listesini) bastırmak,\n  bu boşsa,\n    durmaktır,\n  ilkin devama ekiyse,\n    ilki yazıp,\n    devamı bastırmaktır.\n\n((1'in (2'nin boşa ekine) ekinin) tersini) bastır.`;

async function loadText(path) {
  const res = await fetch(path);
  if (!res.ok) {
    throw new Error(`Failed to load ${path}`);
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

function highlightNonString(text) {
  const tokenPattern = /\p{L}[\p{L}\p{N}]*|\d+|[()]/gu;
  let result = "";
  let lastIndex = 0;
  let match;

  while ((match = tokenPattern.exec(text)) !== null) {
    const token = match[0];
    const start = match.index;
    const end = start + token.length;
    result += escapeHtml(text.slice(lastIndex, start));

    if (token === "(" || token === ")") {
      result += `<span class="kip-paren">${token}</span>`;
    } else if (/^\d+$/.test(token)) {
      const prev = start > 0 ? text[start - 1] : "";
      const next = end < text.length ? text[end] : "";
      if ((prev && letterPattern.test(prev)) || (next && letterPattern.test(next))) {
        result += escapeHtml(token);
      } else {
        result += `<span class="kip-literal">${token}</span>`;
      }
    } else if (keywordSet.has(token)) {
      result += `<span class="kip-keyword">${escapeHtml(token)}</span>`;
    } else {
      result += escapeHtml(token);
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
      const literal = text.slice(i, j);
      result += `<span class="kip-literal">${escapeHtml(literal)}</span>`;
      i = j;
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

function terminateWorker() {
  if (activeWorker) {
    activeWorker.terminate();
    activeWorker = null;
  }
  inputSignalView = null;
  inputBufferView = null;
  pendingInput = false;
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

function handleWorkerMessage(event) {
  const { type, line, error } = event.data || {};
  switch (type) {
    case "stdout":
      appendTerminalLine(line ?? "");
      break;
    case "stderr":
      appendTerminalLine(line ?? "");
      break;
    case "stdin-request":
      if (!interactiveSupported) {
        appendTerminalLine("(stdin unavailable)");
        break;
      }
      pendingInput = true;
      showTerminalInput();
      break;
    case "exit":
      pendingInput = false;
      hideTerminalInput();
      runBtn.disabled = false;
      break;
    case "error":
      appendTerminalLine(error ?? "Unknown error");
      pendingInput = false;
      hideTerminalInput();
      runBtn.disabled = false;
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
  runBtn.disabled = true;
  clearTerminal();
  terminateWorker();
  interactiveSupported = true;

  const { signal, buffer } = createInputBuffers();
  if (!interactiveSupported) {
    appendTerminalLine("Interactive input requires cross-origin isolation (COOP/COEP).");
    appendTerminalLine("Running without stdin support.");
  }
  const worker = new Worker(workerUrl, { type: "module" });
  activeWorker = worker;
  worker.addEventListener("message", handleWorkerMessage);
  worker.addEventListener("error", (event) => {
    appendTerminalLine(String(event.message || event.error || event));
    runBtn.disabled = false;
  });

  const args = ["kip-playground", "--exec", "/main.kip", "--lang", langEl.value];
  worker.postMessage({
    type: "run",
    source: sourceEl.value,
    args,
    signal,
    buffer,
  });
}

function formatExampleLabel(example) {
  return example.id;
}

function buildExampleOptions() {
  if (!exampleEl) {
    return;
  }
  exampleEl.innerHTML = "";
  exampleEl.appendChild(new Option("Custom", "__custom__"));
  for (const example of examples) {
    exampleEl.appendChild(new Option(formatExampleLabel(example), example.id));
  }
  exampleEl.value = "__custom__";
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

if (exampleEl) {
  buildExampleOptions();
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
