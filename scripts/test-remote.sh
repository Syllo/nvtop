#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
bin=$(mktemp)
logs=$(mktemp -d)
trap 'rm -f "$bin"; rm -rf "$logs"' EXIT
cc -std=gnu11 -g -Wall -Wextra -DINI_CALL_HANDLER_ON_NEW_SECTION=1 -Iinclude \
  ${NVTOP_TEST_CFLAGS:-} tests/remote_regression.c src/remote_wire.c src/ini.c src/time.c \
  -pthread -o "$bin"
HOME="$logs" NVTOP_LOG_DIR="$logs" "$bin"
