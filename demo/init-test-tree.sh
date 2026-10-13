#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT_DIR=$(cd "${SCRIPT_DIR}/.." && pwd)
API_BIN="${ROOT_DIR}/api/.lake/build/bin/afv-rpc-api"
EXAMPLE_DIR="${ROOT_DIR}/example"

if [[ ! -x "${API_BIN}" ]]; then
  echo "Error: RPC binary not found or not executable at ${API_BIN}" >&2
  echo "Build it with: (cd ${ROOT_DIR}/api && lake build)" >&2
  exit 1
fi

if [[ ! -d "${EXAMPLE_DIR}" ]]; then
  echo "Error: example test tree not found at ${EXAMPLE_DIR}" >&2
  exit 1
fi

(cd "${EXAMPLE_DIR}" && lake build) || true

if command -v python3 >/dev/null 2>&1; then
  PYTHON=python3
elif command -v python >/dev/null 2>&1; then
  PYTHON=python
else
  echo "Error: python3 or python is required" >&2
  exit 1
fi

TMP_DIR="$(mktemp -d)"
IN_FIFO="${TMP_DIR}/in.fifo"
OUT_FIFO="${TMP_DIR}/out.fifo"

mkfifo "${IN_FIFO}" "${OUT_FIFO}"

"${API_BIN}" <"${IN_FIFO}" >"${OUT_FIFO}" &
RPC_PID=$!

exec 3>"${IN_FIFO}"
exec 4<"${OUT_FIFO}"

cleanup() {
  kill "${RPC_PID}" 2>/dev/null || true
  exec 3>&- || true
  exec 4<&- || true
  rm -rf "${TMP_DIR}" || true
}

trap cleanup EXIT

pretty_print() {
  local payload="$1"
  printf '%s\n' "${payload}" | "${PYTHON}" -m json.tool || printf '%s\n' "${payload}"
}

send_msg() {
  local json="$1"
  local len
  len=$(printf '%s' "${json}" | wc -c | tr -d ' ')
  printf '> Sending request:\n' >&2
  pretty_print "${json}" >&2
  printf '\n' >&2
  printf 'Content-Length: %s\r\n\r\n%s' "${len}" "${json}" >&3
}

read_msg() {
  local line
  local len=""
  while IFS= read -r line <&4; do
    line=${line%$'\r'}
    if [[ -z "${line}" && -n "${len}" ]]; then
      break
    fi
    if [[ "${line}" == Content-Length:* ]]; then
      len=${line#Content-Length: }
      len=${len//[[:space:]]/}
      continue
    fi

    if [[ -z "${len}" && -n "${line}" ]]; then
      continue
    fi
  done

  if [[ -z "${len}" ]]; then
    return 1
  fi

  local body=""
  LC_ALL=C IFS= read -r -n "${len}" body <&4
  printf '%s' "${body}"
}

build_tree_json=$(printf '{"jsonrpc":"2.0","id":1,"method":"buildTestTree","params":{"testTreePaths":["%s"]}}' "${EXAMPLE_DIR}")
exit_json='{"jsonrpc":"2.0","method":"exit","params":null}'

send_msg "${build_tree_json}"

ready_response=$(read_msg)
build_tree_response=$(read_msg)


printf '> Received notification:\n' >&2
pretty_print "${ready_response}"
printf '\n' >&2

printf '> Received response:\n' >&2
pretty_print "${build_tree_response}"
printf '\n' >&2

send_msg "${exit_json}"
