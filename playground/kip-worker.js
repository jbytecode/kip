import { WASI, File, Directory, PreopenDirectory, ConsoleStdout, Fd } from "https://esm.sh/@bjorn3/browser_wasi_shim@0.4.2";

// Library modules shipped into the in-memory WASI filesystem.
const libSources = [
  "giriş",
  "temel",
  "temel-doğruluk",
  "temel-dizge",
  "temel-etki",
  "temel-liste",
  "temel-tam-sayı",
  "temel-ondalık-sayı",
];
const libFiles = [
  ...libSources.map((name) => `${name}.kip`),
  ...libSources.map((name) => `${name}.iz`),
];

// Optimization: in-worker asset cache reused across all run requests.
// Values are `File` objects (cache hit) or `null` (negative cache for missing .iz).
const libFileCache = new Map();
const encoder = new TextEncoder();

async function loadText(path) {
  const res = await fetch(path);
  if (!res.ok) {
    throw new Error(`Failed to load ${path}`);
  }
  return res.text();
}

async function loadBinary(path) {
  const res = await fetch(path);
  if (!res.ok) {
    throw new Error(`Failed to load ${path}`);
  }
  return new Uint8Array(await res.arrayBuffer());
}

async function loadLibFile(file) {
  if (libFileCache.has(file)) {
    // Optimization: reuse both successful and negative cache entries.
    return libFileCache.get(file);
  }

  let fileObj;
  if (file.endsWith(".iz")) {
    try {
      const data = await loadBinary(`./assets/lib/${file}`);
      // Keep cache artifacts writable so the runtime can refresh stale .iz files
      // when compiler hash/source metadata changes.
      fileObj = new File(data);
    } catch (err) {
      // Optimization: memoize missing .iz files to avoid repeated 404 fetches.
      libFileCache.set(file, null);
      return null;
    }
  } else {
    const text = await loadText(`./assets/lib/${file}`);
    // Source files should remain immutable in the mounted lib directory.
    fileObj = new File(encoder.encode(text), { readonly: true });
  }

  libFileCache.set(file, fileObj);
  return fileObj;
}

class InteractiveStdin extends Fd {
  constructor(signal = null, buffer = null) {
    super();
    this.signal = signal ? new Int32Array(signal) : null;
    this.buffer = buffer ? new Uint8Array(buffer) : null;
    this.pending = new Uint8Array();
    this.offset = 0;
  }

  setBuffers(signal, buffer) {
    // Optimization: same stdin fd object can be reused in reactor mode.
    this.signal = signal ? new Int32Array(signal) : null;
    this.buffer = buffer ? new Uint8Array(buffer) : null;
    this.pending = new Uint8Array();
    this.offset = 0;
  }

  fd_fdstat_get() {
    const fdstat = {
      write_bytes(view, offset) {
        view.setUint8(offset, 2);
        view.setUint16(offset + 2, 0, true);
        view.setBigUint64(offset + 8, 0n, true);
        view.setBigUint64(offset + 16, 0n, true);
      },
    };
    return { ret: 0, fdstat };
  }

  fd_read(len) {
    if (!this.signal || !this.buffer) {
      return { ret: 0, data: new Uint8Array() };
    }

    if (this.offset < this.pending.length) {
      const end = Math.min(this.pending.length, this.offset + len);
      const chunk = this.pending.slice(this.offset, end);
      this.offset = end;
      return { ret: 0, data: chunk };
    }

    Atomics.store(this.signal, 0, 1);
    postMessage({ type: "stdin-request" });
    Atomics.wait(this.signal, 0, 1);

    if (this.signal[0] !== 2) {
      return { ret: 0, data: new Uint8Array() };
    }

    const length = this.signal[1];
    const next = this.buffer.slice(0, length);
    this.pending = next;
    this.offset = 0;
    this.signal[0] = 0;
    this.signal[1] = 0;

    if (this.pending.length === 0) {
      return { ret: 0, data: new Uint8Array() };
    }

    const end = Math.min(this.pending.length, len);
    const chunk = this.pending.slice(0, end);
    this.offset = end;
    return { ret: 0, data: chunk };
  }
}

class EmptyStdin extends Fd {
  fd_fdstat_get() {
    const fdstat = {
      write_bytes(view, offset) {
        view.setUint8(offset, 2);
        view.setUint16(offset + 2, 0, true);
        view.setBigUint64(offset + 8, 0n, true);
        view.setBigUint64(offset + 16, 0n, true);
      },
    };
    return { ret: 0, fdstat };
  }

  fd_read() {
    return { ret: 0, data: new Uint8Array() };
  }
}

let wasmModulePromise = null;
let runtimeModePromise = null;
// Optimization: dedupe preload work and share results across runs.
let runtimePreloadPromise = null;
// Optimization: singleton reactor instance state (created once, reused many times).
let reactorStatePromise = null;

const runtimeModes = {
  command: "command",
  reactor: "reactor",
};

async function loadWasmModule() {
  if (wasmModulePromise) {
    // Optimization: compile once, instantiate cheaply many times.
    return wasmModulePromise;
  }
  wasmModulePromise = (async () => {
    try {
      return await WebAssembly.compileStreaming(fetch("./kip-playground.wasm"));
    } catch (err) {
      const res = await fetch("./kip-playground.wasm");
      return WebAssembly.compile(await res.arrayBuffer());
    }
  })();
  return wasmModulePromise;
}

async function detectRuntimeMode() {
  if (runtimeModePromise) {
    return runtimeModePromise;
  }
  runtimeModePromise = (async () => {
    const module = await loadWasmModule();
    const exportNames = new Set(WebAssembly.Module.exports(module).map((entry) => entry.name));
    // Runtime mode detection by exports:
    // - command: `_start` style process execution per run
    // - reactor: `kip_run` callable entrypoint reusable in one instance
    if (exportNames.has("kip_run")) {
      return runtimeModes.reactor;
    }
    return runtimeModes.command;
  })();
  return runtimeModePromise;
}

async function preloadRuntime() {
  if (runtimePreloadPromise) {
    // Optimization: dedupe concurrent preload requests.
    return runtimePreloadPromise;
  }
  runtimePreloadPromise = (async () => {
    // Optimization: warm heavy assets before first Run:
    // - compile wasm module
    // - fetch trmorph fst
    // - eagerly initialize singleton reactor state when available
    await loadWasmModule();
    const mode = await detectRuntimeMode();
    if (!libFileCache.has("trmorph.fst")) {
      const fst = await loadBinary("./assets/vendor/trmorph.fst");
      libFileCache.set("trmorph.fst", new File(fst, { readonly: true }));
    }
    if (mode === runtimeModes.reactor) {
      await getReactorState();
    }
  })();
  return runtimePreloadPromise;
}

function extractModeAndLang(args) {
  // Maps existing CLI-like args into small integer ABI for `kip_run`.
  const list = Array.isArray(args) ? args : [];
  const codegenIndex = list.findIndex((arg) => arg === "--codegen");
  const mode = codegenIndex !== -1 && list[codegenIndex + 1] === "js" ? 1 : 0;
  const langIndex = list.findIndex((arg) => arg === "--lang");
  const langText = langIndex !== -1 ? list[langIndex + 1] : "tr";
  const lang = langText === "en" ? 1 : 0;
  return { mode, lang };
}

async function buildLibDirectoryContents() {
  // Materialize immutable lib directory from cached files.
  const libContents = new Map();
  for (const file of libFiles) {
    const fileObj = await loadLibFile(file);
    if (fileObj) {
      libContents.set(file, fileObj);
    }
  }
  return libContents;
}

async function buildVendorDirectoryContents() {
  // Materialize vendor directory (currently only trmorph.fst).
  let fst;
  if (libFileCache.has("trmorph.fst")) {
    fst = libFileCache.get("trmorph.fst").data;
  } else {
    fst = await loadBinary("./assets/vendor/trmorph.fst");
    libFileCache.set("trmorph.fst", new File(fst, { readonly: true }));
  }
  const vendorContents = new Map();
  vendorContents.set("trmorph.fst", new File(fst, { readonly: true }));
  return vendorContents;
}

function createStdoutSink() {
  return ConsoleStdout.lineBuffered((line) => postMessage({ type: "stdout", line }));
}

function createStderrSink() {
  return ConsoleStdout.lineBuffered((line) => postMessage({ type: "stderr", line }));
}

async function getReactorState() {
  if (reactorStatePromise) {
    // Optimization: keep one live instance + one in-memory FS for all runs.
    return reactorStatePromise;
  }
  reactorStatePromise = (async () => {
    const libContents = await buildLibDirectoryContents();
    const vendorContents = await buildVendorDirectoryContents();
    const rootContents = new Map();
    rootContents.set("lib", new Directory(libContents));
    rootContents.set("vendor", new Directory(vendorContents));
    rootContents.set("main.kip", new File(encoder.encode("")));

    const preopen = new PreopenDirectory("/", rootContents);
    const stdinFile = new InteractiveStdin();
    const stdout = createStdoutSink();
    const stderr = createStderrSink();
    const wasi = new WASI(["kip-playground-reactor"], ["KIP_DATADIR=/"], [stdinFile, stdout, stderr, preopen]);

    const module = await loadWasmModule();
    const instance = await WebAssembly.instantiate(module, {
      wasi_snapshot_preview1: wasi.wasiImport,
    });

    // Optimization: initialize the reactor once; subsequent runs call `kip_run`.
    if (typeof wasi.initialize === "function") {
      wasi.initialize(instance);
    } else if (instance.exports && typeof instance.exports._initialize === "function") {
      instance.exports._initialize();
    }
    // Reactor builds with `-no-hs-main` require explicit RTS initialization.
    // Without this, `kip_run` fails with:
    //   "RTS is not initialised; call hs_init() first"
    if (instance.exports && typeof instance.exports.hs_init === "function") {
      try {
        // Common ABI shape for hs_init(argcPtr, argvPtr).
        instance.exports.hs_init(0, 0);
      } catch (err) {
        // Some toolchains expose a zero-arg wrapper; keep compatibility.
        instance.exports.hs_init();
      }
    }

    return { rootContents, stdinFile, instance };
  })();
  return reactorStatePromise;
}

async function runWasmCommand({ args, source, signal, buffer }) {
  // Command mode: process-style execution. Creates a fresh instance each run.
  const rootContents = new Map();
  const libContents = await buildLibDirectoryContents();
  const vendorContents = await buildVendorDirectoryContents();

  rootContents.set("lib", new Directory(libContents));
  rootContents.set("vendor", new Directory(vendorContents));
  rootContents.set("main.kip", new File(encoder.encode(source)));

  const preopen = new PreopenDirectory("/", rootContents);
  const stdout = createStdoutSink();
  const stderr = createStderrSink();
  const stdinFile = signal && buffer ? new InteractiveStdin(signal, buffer) : new EmptyStdin();
  const wasi = new WASI(args, ["KIP_DATADIR=/"], [stdinFile, stdout, stderr, preopen]);

  const module = await loadWasmModule();
  const instance = await WebAssembly.instantiate(module, {
    wasi_snapshot_preview1: wasi.wasiImport,
  });

  try {
    wasi.start(instance);
  } catch (err) {
    if (!(err && err.code !== undefined)) {
      throw err;
    }
  }
}

async function runWasmReactor({ args, source, signal, buffer }) {
  // Reactor mode optimization:
  // - same wasm instance
  // - same in-memory FS tree
  // - replace only /main.kip and stdin buffers per invocation
  const reactorState = await getReactorState();
  reactorState.rootContents.set("main.kip", new File(encoder.encode(source)));
  reactorState.stdinFile.setBuffers(signal, buffer);

  const exportRun = reactorState.instance.exports && reactorState.instance.exports.kip_run;
  if (typeof exportRun !== "function") {
    throw new Error("Reactor module does not export kip_run.");
  }

  const { mode, lang } = extractModeAndLang(args);
  const status = exportRun(mode, lang);
  const exitCode = Number(status);
  if (!Number.isFinite(exitCode) || exitCode !== 0) {
    throw new Error(`kip_run failed with exit code ${exitCode}.`);
  }
}

async function runWasm({ args, source, signal, buffer }) {
  const mode = await detectRuntimeMode();

  // Optimization: ensure first heavy work is done during preload/first interaction.
  await preloadRuntime();
  if (mode === runtimeModes.reactor) {
    await runWasmReactor({ args, source, signal, buffer });
    return;
  }
  await runWasmCommand({ args, source, signal, buffer });
}

self.addEventListener("message", async (event) => {
  const { type, args, source, signal, buffer } = event.data || {};
  if (type === "preload") {
    try {
      await preloadRuntime();
      const runtimeMode = await detectRuntimeMode();
      postMessage({ type: "preloaded", runtimeMode });
    } catch (err) {
      postMessage({ type: "preload-error", error: String(err) });
    }
    return;
  }
  if (type !== "run") {
    return;
  }
  try {
    await runWasm({ args, source, signal, buffer });
    postMessage({ type: "exit" });
  } catch (err) {
    postMessage({ type: "error", error: String(err) });
  }
});
