# Reliable remote GPU monitoring

The remote dashboard is now merged into `master`.
Update both the dashboard and exporters to this branch for the multi-GPU codec
fixes. The terminal UI remains NVTOP; no web service is required.

## Install the startup updater once

In your existing checkout on each Linux host:

```bash
git switch master
git pull --ff-only
# Ubuntu/Debian build dependencies:
sudo apt-get install -y git cmake build-essential libncurses-dev libdrm-dev libsystemd-dev
./scripts/install-startup.sh
export PATH="$HOME/.local/bin:$PATH"
nvtop
```

Put the PATH line in your shell profile. If a desktop autostart entry or an
existing service launches an absolute binary such as `/usr/local/bin/nvtop`,
change that command to `/home/YOUR_USER/.local/bin/nvtop`. Merely updating the
repository cannot change an already installed binary or existing boot service.

For a GPU host that should export telemetry at boot:

```bash
./scripts/install-startup.sh --export-service
sudo loginctl enable-linger "$USER"
```

This creates and starts a **user** service named `nvtop-export.service`.
Do not run it alongside an existing exporter on port 8765; stop/disable that
older service first. Manage it with `systemctl --user status/restart/stop
nvtop-export.service`. For a custom port or bind address, use `systemctl --user
edit nvtop-export.service` to replace `ExecStart`, keeping the launcher path and
adding `--export=8765 --bind=YOUR_LAN_IP` as appropriate. The transport has no
authentication; keep it on a trusted network.

At each launcher invocation (including a service restart or reboot), it fetches
`origin/master`, builds that exact revision separately, and
atomically selects the new binary after a successful build/version check.
It never resets your checkout or changes your config. A failed fetch/build runs
the previous binary. On the first installation there is no fallback yet, so a
failed build prints the log path and exits. Startup fetch/build has bounded
timeouts; building a new revision can take several minutes. A lock prevents
concurrent builds. Previously built revisions are reused.

Controls:

- `NVTOP_AUTO_UPDATE=0`: launch the last successful build without fetching.
- `NVTOP_UPDATE_BRANCH`: choose another remote branch, deliberately.
- `NVTOP_BUILD_JOBS=2`: default parallel compiler jobs.
- Build logs: `${XDG_STATE_HOME:-~/.local/state}/nvtop/logs/`, hourly files,
  expired after 24 hours on the next launch.
- Built binaries: `${XDG_STATE_HOME:-~/.local/state}/nvtop/releases/`.
  Previous revisions remain available for rollback. To roll back, stop the
  service, point `current` at the desired `releases/COMMIT` directory and set
  `NVTOP_AUTO_UPDATE=0` in the service environment before restarting.

## Offline and recovery behavior

Each configured host gets its own polling worker. DNS lookup, a firewall,
a server that never responds, or a damaged frame cannot block the display
thread or another host's worker. Connect attempts time out after 250 ms per
address; reads time out after 1.5 seconds without progress, with a three-second frame
budget. Poll attempts repeat after a 900 ms pause. One failed poll no longer
marks the host offline. For up to 15 seconds since the last good frame, the
chart and last numeric readings are retained with a retrying status; no stale
samples are added to chart history. Only a sustained outage clears the chart.
Set `NVTOP_OFFLINE_SECONDS=30` for a longer grace period (3–300 seconds; default 15).

After the grace period expires, dynamic readings and process rows clear. A separate GPU plot clears
its history and displays **GPU offline / Reconnecting automatically**. A merged
plot keeps the other GPUs' charts and shows the offline GPU's status across the
top. Recovery uses fresh history rather than connecting a line across the outage.
Raw numeric values remain instantaneous. A GPU slot missing from a valid returned frame instead displays **GPU not reported**.
An idle GPU with zero utilization is still online. GPU identity in this version of NVT1 is its ordinal index;
exporters should keep device order stable.

At startup, all hosts discover in parallel with one shared three-second grace
period. Discovered GPU counts are remembered under `~/.nvtop-logs/host-*.gpus`,
so a previously seen multi-GPU host retains its panels when booted offline.
For first-time setup when a host is offline, declare its expected GPU count:

```ini
[RemoteHost]
Host = 192.168.1.50
Port = 8765
Name = Inference server
GPUs = 4
```

The count is optional, accepts 1–16, and reserves panels when the host is unreachable
at startup. A live startup inventory always takes precedence over saved counts,
so old cached GPU counts do not create phantom panels. The application fixes its panel layout at startup: adding more GPUs
than the discovered/cached/configured count requires restarting the dashboard.
Existing panels reconnect automatically. Configuration uses `-c FILE`, then
`NVTOP_CONFIG_FILE`, then `$XDG_CONFIG_HOME/nvtop/interface.ini` or
`~/.config/nvtop/interface.ini`.

## Chart appearance and logs

Charts use a clean background without dotted grid markers, bounded 0–100 plotting, and
a trailing five-sample average to make bursty LLM workloads easier to read.
This is sample-based, not a fixed five-second window: duration depends on your
UI refresh interval. Numeric headers retain raw readings. Set
`NVTOP_CHART_SAMPLES=1` for raw charts, or choose 2–30 for smoothing.

Connection transitions, initial status, and periodic continued-failure messages
are written to `~/.nvtop-logs/remote-HH.log`. Set `NVTOP_LOG_DIR` to override the
log directory (its parent must exist). There are 24 hourly slots, reused daily;
old slots are removed on the next log write. Quiet periods produce no entries.
Messages include timestamp (Unix seconds), host, port, state, and GPU count;
no process command lines or model inputs are logged. Exporter stdout/stderr is
also available through `journalctl --user -u nvtop-export.service` (journald's
own retention is controlled separately).

## Verification

```bash
./scripts/test-remote.sh
NVTOP_TEST_CFLAGS='-fsanitize=address,undefined -fno-omit-frame-pointer' ./scripts/test-remote.sh
cmake -S . -B build -DBUILD_TESTING=ON
cmake --build build -j2
ctest --test-dir build --output-on-failure
python3 tests/test_dashboard.py
```

The hardware-free regression covers separate processes on two GPUs, every
truncated frame length, an unreachable second host, an accepted connection that
never responds, fast UI callbacks during failure, and automatic recovery.
A live hardware acceptance check is still useful: run two exporters, stop one,
verify the other keeps moving, then start the stopped exporter and verify it
recovers. Reboot once after installing the service to verify your host's actual
boot configuration and network access.

## Which machines need this update?

The grace period, chart history, host labels, and missing-panel fixes run on the
**dashboard**. They are compatible with existing NVT1 exporters; you do not need
to redeploy every GPU host just to get these display fixes. Update each exporter
as well if it is still running a version from before the multi-GPU codec and
bounded-client fixes. Only machines supplying remote GPU readings need an
exporter. The local Intel HD Graphics device is read directly by the dashboard.

Local headers show the local OS hostname. Remote headers show the configured
`Host` (DNS hostname or IP) alongside the friendly `Name`; this does not require
a protocol change or reverse-DNS lookup.

The launcher now follows `master` by default. Existing installations must run
`git switch master && git pull --ff-only` in their checkout once to update the
launcher itself, then restart the dashboard/service. Set `NVTOP_UPDATE_BRANCH=master`
explicitly if an older service environment still overrides the branch.
