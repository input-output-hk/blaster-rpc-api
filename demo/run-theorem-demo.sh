#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT_DIR=$(cd "${SCRIPT_DIR}/.." && pwd)
API_BIN="${ROOT_DIR}/api/.lake/build/bin/afv-rpc-api"
EXAMPLE_DIR="${ROOT_DIR}/example/AfvExample"

if [[ ! -x "${API_BIN}" ]]; then
  echo "Error: RPC binary not found or not executable at ${API_BIN}" >&2
  echo "Build it with: (cd ${ROOT_DIR}/api && lake build)" >&2
  exit 1
fi

if [[ ! -d "${EXAMPLE_DIR}" ]]; then
  echo "Error: example workspace not found at ${EXAMPLE_DIR}" >&2
  exit 1
fi

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
    if [[ -z "${line}" ]]; then
      break
    fi
    if [[ "${line}" == Content-Length:* ]]; then
      len=${line#Content-Length: }
      len=${len//[[:space:]]/}
    fi
  done

  if [[ -z "${len}" ]]; then
    return 1
  fi

  local body=""
  IFS= read -r -n "${len}" body <&4
  printf '%s' "${body}"
}

init_json=$(printf '{"jsonrpc":"2.0","method":"initializeWorkspace","params":{"workspacePaths":["%s"]}}' "${EXAMPLE_DIR}")
workspace_json='{"jsonrpc":"2.0","id":1,"method":"getWorkspace","params":null}'
exit_json='{"jsonrpc":"2.0","method":"exit","params":null}'

send_msg "${init_json}"
send_msg "${workspace_json}"

notification_response=$(read_msg)
workspace_response=$(read_msg)

printf '> Received notification:\n' >&2
pretty_print "${notification_response}"
printf '\n' >&2

printf '> Received response:\n' >&2
pretty_print "${workspace_response}"
printf '\n' >&2

theorem_id=$(printf '%s' "${workspace_response}" | "${PYTHON}" -c '
import json, sys

TARGET_PATH = "example/AfvExample/Test1.lean"
TARGET_LABEL = "example_theorem_1"

def find_theorem_id(node_or_nodes):
  if isinstance(node_or_nodes, list):
    for node in node_or_nodes:
      found = find_theorem_id(node)
      if found:
        return found
    return None

  node = node_or_nodes or {}
  if node.get("type") == "file":
    path = node.get("path", "")
    if path.endswith(TARGET_PATH):
      for thm in node.get("theorems", []):
        if thm.get("label") == TARGET_LABEL:
          return thm.get("id")

  for child in node.get("children", []):
    found = find_theorem_id(child)
    if found:
      return found
  return None

data = json.load(sys.stdin)
workspaces = data.get("result", [])
found = find_theorem_id(workspaces)
print(found or "")
')

if [[ -z "${theorem_id}" ]]; then
  echo "Error: example_theorem_1 not found in Test1.lean" >&2
  send_msg "${exit_json}"
  exit 1
fi

theorem_json=$(printf '{"jsonrpc":"2.0","method":"runTheorem","params":["%s"]}' "${theorem_id}")
send_msg "${theorem_json}"

theorem_response=$(read_msg)

printf '> Received notification:\n' >&2
pretty_print "${theorem_response}"
printf '\n' >&2

send_msg "${exit_json}"
