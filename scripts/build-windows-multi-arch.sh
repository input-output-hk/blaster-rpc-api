#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT_DIR=$(cd "${SCRIPT_DIR}/.." && pwd)
API_DIR="${ROOT_DIR}/api"
OUT_DIR="${ROOT_DIR}/dist"

target="${1:-all}"
case "${target}" in
  all|x64|arm64)
    ;;
  *)
    echo "Error: invalid target '${target}'. Use one of: all, x64, arm64." >&2
    exit 1
    ;;
esac

if ! command -v lake >/dev/null 2>&1; then
  echo "Error: lake is required." >&2
  exit 1
fi

mkdir -p "${OUT_DIR}"

build_for_arch() {
  local arch="$1"
  local out_bin="${OUT_DIR}/afv-rpc-api-win32-${arch}.exe"

  echo "Building for win32/${arch}..." >&2
  (
    cd "${API_DIR}"
    lake clean
    lake build
  )

  local src_bin="${API_DIR}/.lake/build/bin/afv-rpc-api.exe"
  if [[ ! -f "${src_bin}" ]]; then
    echo "Error: build output not found for win32/${arch}: ${src_bin}" >&2
    exit 1
  fi

  cp "${src_bin}" "${out_bin}"
  echo "Wrote ${out_bin}" >&2
}

if [[ "${target}" == "all" ]]; then
  build_for_arch "x64"
  build_for_arch "arm64"
else
  build_for_arch "${target}"
fi

echo "Done. Binaries are in ${OUT_DIR}." >&2
