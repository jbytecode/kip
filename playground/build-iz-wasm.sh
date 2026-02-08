#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DIST_DIR="${ROOT_DIR}/playground/dist"
ASSET_DIR="${DIST_DIR}/assets"
WASMTIME="${WASMTIME:-}"

if [[ ! -d "${ASSET_DIR}/lib" ]]; then
  echo "Missing ${ASSET_DIR}/lib; run ./playground/build-assets.sh first."
  exit 1
fi

if [[ -n "${WASMTIME}" ]]; then
  WASMTIME_BIN="${WASMTIME}"
elif command -v wasmtime >/dev/null 2>&1; then
  WASMTIME_BIN="$(command -v wasmtime)"
elif [[ -x "${ROOT_DIR}/playground/.ghc-wasm/wasmtime/bin/wasmtime" ]]; then
  WASMTIME_BIN="${ROOT_DIR}/playground/.ghc-wasm/wasmtime/bin/wasmtime"
elif [[ -x "${ROOT_DIR}/playground/.ghc-wasm/wasm-run/bin/wasmtime.sh" ]]; then
  WASMTIME_BIN="${ROOT_DIR}/playground/.ghc-wasm/wasm-run/bin/wasmtime.sh"
else
  echo "Note: wasmtime not found (checked PATH and playground/.ghc-wasm); skipping precompilation of .iz caches."
  exit 0
fi

WASMTIME_CMD=("${WASMTIME_BIN}")
if "${WASMTIME_BIN}" --help 2>/dev/null | grep -Eq '(^|[[:space:]])run([[:space:]]|$)'; then
  WASMTIME_CMD=("${WASMTIME_BIN}" "run")
fi

if "${WASMTIME_BIN}" --help 2>/dev/null | grep -q -- "--mapdir"; then
  "${WASMTIME_CMD[@]}" \
    --mapdir "/::${ASSET_DIR}" \
    --env "KIP_DATADIR=/" \
    "${DIST_DIR}/kip-playground.wasm" --build /lib
else
  "${WASMTIME_CMD[@]}" \
    --dir "${ASSET_DIR}" \
    --env "KIP_DATADIR=${ASSET_DIR}" \
    "${DIST_DIR}/kip-playground.wasm" --build "${ASSET_DIR}/lib"
fi

iz_count="$(find "${ASSET_DIR}/lib" -maxdepth 1 -name '*.iz' | wc -l | tr -d '[:space:]')"
if [[ "${iz_count}" == "0" ]]; then
  echo "Error: no .iz files were generated under ${ASSET_DIR}/lib."
  exit 1
fi
