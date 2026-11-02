#!/usr/bin/env bash
set -euo pipefail
repo=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
mkdir -p "$HOME/.local/bin"
ln -sfn "$repo/scripts/nvtop-start" "$HOME/.local/bin/nvtop"
printf 'Installed %s/.local/bin/nvtop. Put ~/.local/bin first in PATH.\n' "$HOME"
if [[ ${1:-} == --export-service ]]; then
  # User service preserves HOME/config; linger allows start before login.
  [[ "$repo" != *'"'* && "$repo" != *'%'* ]] || { echo 'Unsupported repository path' >&2; exit 1; }
  mkdir -p "$HOME/.config/systemd/user"
  cat > "$HOME/.config/systemd/user/nvtop-export.service" <<EOF
[Unit]
Description=NVTOP GPU exporter with startup updates
After=network-online.target
Wants=network-online.target

[Service]
ExecStart="$repo/scripts/nvtop-start" --export
Restart=on-failure
RestartSec=10
TimeoutStartSec=600

[Install]
WantedBy=default.target
EOF
  systemctl --user daemon-reload
  systemctl --user enable --now nvtop-export.service
  printf 'For boot before login, run once: sudo loginctl enable-linger %s\n' "$USER"
fi
