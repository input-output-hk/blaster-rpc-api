#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT_DIR=$(cd "${SCRIPT_DIR}/.." && pwd)
API_DIR="${ROOT_DIR}/api"
OUT_DIR="${ROOT_DIR}/dist"
mkdir -p "${OUT_DIR}"

if [[ "$(uname -s)" != "Darwin" ]]; then
  echo "Warning: not running on macOS; skipping darwin builds." >&2
  exit 0
fi

if ! command -v lake >/dev/null 2>&1; then
  echo "Warning: lake not found; skipping macOS build." >&2
  exit 0
fi

build_flags=(
  "-K=lean.c.flags=-flto"
  "-K=lean.c.flags=-ffunction-sections"
  "-K=lean.c.flags=-fdata-sections"
  "-K=lean.c.flags=-fno-unwind-tables"
  "-K=lean.c.flags=-fno-asynchronous-unwind-tables"
  "-K=lean.c.linkArgs=-Wl,-dead_strip"
  "-K=lean.c.linkArgs=-Wl,-x"
  "-K=lean.c.linkArgs=-Wl,-S"
)
native_arch="$(uname -m)"
if [[ "${native_arch}" == "arm64" ]]; then
  native_suffix="darwin-arm64"
  other_arch="x86_64"
  other_suffix="darwin-x64"
else
  native_suffix="darwin-x64"
  other_arch="arm64"
  other_suffix="darwin-arm64"
fi

echo "Building for darwin/${native_arch}..." >&2
(cd "${API_DIR}" && lake clean && lake build "${build_flags[@]}")
out_bin="${OUT_DIR}/blaster-rpc-api-${native_suffix}"
cp "${API_DIR}/.lake/build/bin/blaster-rpc-api" "${out_bin}"
chmod +x "${out_bin}"
echo "Wrote ${out_bin}" >&2

echo "Building for darwin/${other_arch}..." >&2
clang_path="$(xcrun --sdk macosx --find clang)"
sdk_path="$(xcrun --sdk macosx --show-sdk-path)"
if (cd "${API_DIR}" && lake clean && \
  CC="${clang_path} -target ${other_arch}-apple-darwin" \
  CXX="${clang_path} -target ${other_arch}-apple-darwin" \
  SDKROOT="${sdk_path}" \
  MACOSX_DEPLOYMENT_TARGET="11.0" \
  lake build "${build_flags[@]}"); then
  out_bin="${OUT_DIR}/blaster-rpc-api-${other_suffix}"
  cp "${API_DIR}/.lake/build/bin/blaster-rpc-api" "${out_bin}"
  chmod +x "${out_bin}"
  echo "Wrote ${out_bin}" >&2
else
  echo "Warning: failed to build darwin/${other_arch}; skipping." >&2
fi

echo "Done. Binaries are in ${OUT_DIR}." >&2
