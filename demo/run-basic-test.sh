#!/usr/bin/env bash
set -euo pipefail
SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
exec node "${SCRIPT_DIR}/run.mjs" Test1.lean example_test_1 valid