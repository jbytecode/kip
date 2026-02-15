#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Default location used by build-foma-wasm.sh unless overridden by caller.
if [[ -z "${FOMA_WASM_PREFIX:-}" ]]; then
  export FOMA_WASM_PREFIX="${ROOT_DIR}/playground/foma-wasm"
fi

needs_foma_build=false
if [[ ! -f "${FOMA_WASM_PREFIX}/include/fomalib.h" ]]; then
  needs_foma_build=true
fi
if [[ ! -f "${FOMA_WASM_PREFIX}/lib/libfoma.a" ]]; then
  needs_foma_build=true
fi

if [[ "${needs_foma_build}" == "true" ]]; then
  echo "Foma WASM artifacts missing under ${FOMA_WASM_PREFIX}; running build-foma-wasm.sh..."
  "${ROOT_DIR}/playground/build-foma-wasm.sh"
fi

echo "Building playground WASM and web assets..."
"${ROOT_DIR}/playground/build-wasm.sh"

echo "Playground build complete."
