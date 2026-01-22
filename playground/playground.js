const workerUrl = new URL("./kip-worker.js", import.meta.url);

const sourceEl = document.getElementById("source");
const outputEl = document.getElementById("output");
const terminalBodyEl = document.getElementById("terminal-body");
const terminalEl = document.querySelector(".terminal");
const terminalInputEl = document.getElementById("terminal-input");
const terminalInputField = document.getElementById("terminal-input-field");
const runBtn = document.getElementById("run");
const langEl = document.getElementById("lang");
const exampleEl = document.getElementById("example");

const examples = [
  { id: "selamlamak", file: "selamlamak.kip" },
  { id: "gün-örneği", file: "gün-örneği.kip" },
  { id: "fibonacci", file: "fibonacci.kip" },
  { id: "asal-sayılar", file: "asal-sayılar.kip" },
  { id: "bir-fazlası", file: "bir-fazlası.kip" },
  { id: "ikili-ağaç-araması", file: "ikili-ağaç-araması.kip" },
  { id: "dosya-io", file: "dosya-io.kip" },
];

sourceEl.value = `(bu tam-sayı listesini) bastırmak,
  bu boşsa,
    durmaktır,
  ilkin devama ekiyse,
    ilki yazıp,
    devamı bastırmaktır.

((1'in (2'nin boşa ekine) ekinin) tersini) bastır.`;

async function loadText(path) {
  const res = await fetch(path);
  if (!res.ok) {
    throw new Error(`Failed to load ${path}`);
  }
  return res.text();
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

function syncTerminalHeight() {
  if (!sourceEl || !terminalEl || !terminalBodyEl) {
    return;
  }
  const rect = sourceEl.getBoundingClientRect();
  if (!rect.height) {
    return;
  }
  const styles = getComputedStyle(terminalEl);
  const paddingTop = parseFloat(styles.paddingTop) || 0;
  const paddingBottom = parseFloat(styles.paddingBottom) || 0;
  terminalEl.style.height = `${rect.height}px`;
  terminalBodyEl.style.maxHeight = `${Math.max(0, rect.height - paddingTop - paddingBottom)}px`;
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
}

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

syncTerminalHeight();
if (typeof ResizeObserver !== "undefined" && sourceEl) {
  const observer = new ResizeObserver(() => syncTerminalHeight());
  observer.observe(sourceEl);
} else {
  window.addEventListener("resize", syncTerminalHeight);
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
