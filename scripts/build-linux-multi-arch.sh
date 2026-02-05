#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT_DIR=$(cd "${SCRIPT_DIR}/.." && pwd)
API_DIR="${ROOT_DIR}/api"
OUT_DIR="${ROOT_DIR}/dist"
DOCKERFILE="${ROOT_DIR}/scripts/Dockerfile"

if ! command -v docker >/dev/null 2>&1; then
  echo "Error: docker is required." >&2
  exit 1
fi

mkdir -p "${OUT_DIR}"

TMP_DIR="$(mktemp -d)"

cleanup() {
  rm -rf "${TMP_DIR}" || true
}
trap cleanup EXIT

platforms=(
  "linux/amd64"
  "linux/arm64"
)

for platform in "${platforms[@]}"; do
  echo "Building for ${platform}..." >&2
  dest="${TMP_DIR}/out-${platform//\//-}"
  mkdir -p "${dest}"

  docker buildx build \
    --platform "${platform}" \
    -f "${DOCKERFILE}" \
    --output "type=local,dest=${dest}" \
    "${API_DIR}"

  src_bin="${dest}/out/afv-rpc-api"
  if [[ ! -f "${src_bin}" ]]; then
    echo "Error: build output not found for ${platform}." >&2
    exit 1
  fi

  out_suffix="${platform//\//-}"
  if [[ "${platform}" == "linux/amd64" ]]; then
    out_suffix="linux-x64"
  elif [[ "${platform}" == "linux/arm64" ]]; then
    out_suffix="linux-arm64"
  fi
  out_bin="${OUT_DIR}/afv-rpc-api-${out_suffix}"
  cp "${src_bin}" "${out_bin}"
  chmod +x "${out_bin}"
  echo "Wrote ${out_bin}" >&2
done

echo "Done. Binaries are in ${OUT_DIR}." >&2
