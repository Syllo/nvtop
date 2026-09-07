# nvtop Remote GPU Telemetry — Complete Reference & Examples

This document is the single place to learn the **remote GPU** feature:
what it is, how it works at the wire level, how to deploy a whole-fleet
dashboard, and every command and config knob involved. If you run GPUs on
several machines and want **one** nvtop screen to show them all, start
here.

---

## 1. What this is

nvtop can now talk to itself across the network. Two roles:

| Role | Command | What it does |
|------|---------|--------------|
| **Exporter** (runs on each remote GPU host) | `nvtop --export [port]` | Headless TCP server that serves this host's live GPU stats to any consumer that polls it. |
| **Dashboard / consumer** (runs on one host) | `nvtop` (with `[RemoteHost]` INI sections) | Dials each exporter and renders every remote GPU as a normal nvtop device, next to local GPUs. |

The result: a **single dashboard** aggregating a heterogeneous fleet
(workstations, GPU nodes, Docker containers) — live utilization, memory,
clocks, temperature, power, and per-GPU processes.

---

## 2. Architecture

```
                         +--------------------------------------+
                         |   DASHBOARD HOST (consumer)          |
                         |   runs: nvtop  (interactive)        |
                         |   config: [RemoteHost] x N           |
                         +-------------------+------------------+
                                           |  dials each exporter
        +----------------+    TCP:8765     v
        | GPU HOST A     |   NVT1 frames   +--------------------------------------+
        | nvtop --export| ----------------> gpu_vendor_remote renders
        | (exporter)    |   POLL -> frame |  each remote GPU as a normal device   |
        +----------------+                 +--------------------------------------+
```

Each **exporter** runs headless on a GPU machine. Each **dashboard** host
has one or more `[RemoteHost]` sections pointing at those exporters. The
remote GPU appears in nvtop with a synthetic device id `R<hostIdx>_<gpuIdx>`,
so it shows up alongside local GPUs in the same plots and tables.

---

## 3. The NVT1 wire protocol

A snapshot is a single length-prefixed TCP message:

```
+----------------+--------------------------------------------------+
| 4-byte length  |  payload (little-endian)                         |
| (big-endian)   |                                                  |
+----------------+--------------------------------------------------+
payload layout:
  [wire_header]
  [wire_static  x static_count]
  [ (wire_gpu + wire_process x process_count)  x gpu_count ]

wire_header (14 bytes):
  magic "NVT1" (4) | proto_major(1) | proto_minor(1) |
  flags(1) | static_count(1) | gpu_count(2, LE) | reserved(4)
  (flags bit 0 set => payload integers are little-endian)
```

**Key properties**

- The **length prefix is big-endian**; **all payload integers are
  little-endian** (the header `flags` byte declares this).
- `valid[]` bitmaps in `wire_gpu` / `wire_static` / `wire_process` are
  forwarded **verbatim**, so a consumer renders absent fields as N/A
  with zero extra logic.
- The consumer (dashboard) uses a **900 ms cache + last-good fallback**:
  it only re-dials when the cache is stale, and if a host goes down the
  last successfully-read frame keeps showing, so a dead host never stalls
  the UI.

`remote_wire_decode()` validates: correct magic, `proto_major <= 1`,
`static_count <= 1`, `gpu_count <= 16`, and that the frame size matches
the declared length — otherwise it returns 0 (rejects the frame).

---

## 4. Commands & flags

### Exporter side (remote GPU host)

```bash
nvtop --export              # headless exporter on default port 8765
nvtop --export 9100         # custom port
nvtop --export --bind 10.0.0.1   # bind to a specific address (default 0.0.0.0)
nvtop --export 8765 -d 5    # 500ms refresh cadence (tenths of a second)
```

- `--export [port]` — start the headless exporter. Optional port, default
  `8765`. This process runs the poll/encode loop; it does **not** open a
  TUI.
- `--bind <addr>` — address the exporter binds to (default `0.0.0.0`).
- `-d / --delay` — refresh cadence in tenths of a second (the exporter's
  loop uses this for `gpuinfo_refresh_dynamic_info` cadence).

### Dashboard / consumer side

```bash
nvtop                        # interactive UI; remote hosts from INI [RemoteHost]
nvtop -s                    # one-shot snapshot (scripting)
nvtop -l                    # looping snapshot
nvtop -c /path/to/ini       # point at a specific config file
```

Remote hosts are **not** a command-line flag — they come from the
`[RemoteHost]` INI sections described below. The dashboard dials each one
on its 900 ms cache.

### Related nvtop flags (full reference)

| Flag | Long | Meaning |
|------|------|---------|
| `-d N` | `--delay` | Refresh interval in tenths of a second |
| `-v` | `--version` | Print version |
| `-h` | `--help` | Print help |
| `-c <file>` | `--config` | Use a specific config file |
| `-C` | `--no-color` | Disable colors |
| `-f` | `--fahrenheit` | Temperature in Fahrenheit |
| `-i` | `--gpu-info` | Show the GPU info bar |
| `-E <secs>` | `--encode-hide` | Hide encode/decode after N seconds |
| `-p` | `--no-plot` | Hide plots |
| `-P` | `--no-processes` | Hide process list |
| `-r` | `--reverse-abs` | Reverse plot direction |
| `-s` | `--snapshot` | One-shot stats, no ncurses |
| `-l` | `--loop` | Looping stats, no ncurses |
| `-x [port]` | `--export` | **Headless TCP exporter** (default port 8765) |
| `-b <addr>` | `--bind` | **Bind address** for `--export` (default 0.0.0.0) |

---

## 5. Configuration

Remote hosts live in the **existing** nvtop INI file — the same file that
holds your plot/process preferences. Default path:
`~/.config/nvtop/interface.ini` (overridable with `-c`).

```ini
# ~/.config/nvtop/interface.ini

[RemoteHost]
Host = remote-gpu-host-1
Port = 8765
Name = GPU Node 1

[RemoteHost]
Host = remote-gpu-host-2
Port = 8765
Name = GPU Node 2
```

- `Host` — remote host (hostname or IP). **Required** (a block without it is
  skipped).
- `Port` — exporter port. **Optional**, defaults to `8765`.
- `Name` — label shown in the UI. **Optional**, defaults to the host value.

The INI parser must be built with `INI_CALL_HANDLER_ON_NEW_SECTION=1`
(handled automatically by `src/CMakeLists.txt` when `REMOTE_SUPPORT` is on),
because a new section header must fire the handler so each `[RemoteHost]`
block gets committed. Without it, no remote host is ever registered and the
dashboard sees zero remote GPUs.

---

## 6. End-to-end example (a 3-GPU fleet)

The worked example below mirrors a real mixed fleet: a local workstation
GPU, a GPU node, and a containerized GPU.

### Step 1 — Build nvtop with remote support

```bash
# From the repo root
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DREMOTE_SUPPORT=ON
cmake --build build
```

`REMOTE_SUPPORT` defaults to **ON on Linux**, OFF elsewhere.

### Step 2 — Run an exporter on each remote GPU host

On **GPU Node 1** (e.g. a bare-metal or Docker host with an NVIDIA GPU):

```bash
# systemd service (recommended for headless operation)
nvtop --export 8765 --bind 0.0.0.0
```

Or as a systemd unit:

```ini
# /etc/systemd/system/nvtop-exporter.service
[Unit]
Description=nvtop GPU telemetry exporter (NVT1)
After=network-online.target
Wants=network-online.target

[Service]
ExecStart=/usr/local/bin/nvtop --export=8765
Restart=on-failure
RestartSec=5

[Install]
WantedBy=default.target
```

In **Docker** (NVIDIA runtime):

```yaml
# docker-compose.yml
services:
  nvtop-exporter:
    image: ubuntu:24.04
    command: ["/nvtop/nvtop", "--export=8765"]
    ports:
      - "8765:8765"
    volumes:
      - ./nvtop:/nvtop:ro
    runtime: nvidia
    environment:
      - NVIDIA_VISIBLE_DEVICES=all
    restart: unless-stopped
```

### Step 3 — Point the dashboard at the exporters

On the **dashboard** host, edit `~/.config/nvtop/interface.ini`:

```ini
[RemoteHost]
Host = 10.0.0.11
Port = 8765
Name = GPU Node 1

[RemoteHost]
Host = 10.0.0.12
Port = 8765
Name = GPU Node 2
```

Then simply run:

```bash
nvtop
```

Every remote GPU now appears in the same plots/tables as your local GPUs.
If a remote host is unreachable, its last-known values keep displaying
(last-good fallback) rather than vanishing.

### Step 4 — Verify an exporter responds (scripting)

```python
import socket, struct

def poll(host, port=8765):
    s = socket.socket()
    s.settimeout(5)
    s.connect((host, port))
    s.sendall(b'\x01')                      # the POLL byte
    hdr = s.recv(4)
    if len(hdr) < 4:
        s.close(); raise SystemExit('short header')
    length = struct.unpack('>I', hdr)[0]     # big-endian length
    body = b''
    while len(body) < length:
        chunk = s.recv(length - len(body))
        if not chunk:
            raise SystemExit('connection closed mid-frame')
        body += chunk
    s.close()
    # header: magic(4) major(1) minor(1) flags(1) static_count(1) gpu_count(2 LE) reserved(4)
    print('magic =', body[:4].decode('latin1'))
    print('gpu_count =', struct.unpack('<H', body[8:10])[0])
    print('payload bytes =', length)
    return body

poll('10.0.0.11')
```

Expected: `magic = NVT1`, `gpu_count` = number of GPUs on that host, and
`payload` = frame size in bytes.

---

## 7. Troubleshooting

| Symptom | Cause | Fix |
|---------|-------|-----|
| Dashboard shows only local GPUs | No `[RemoteHost]` sections parsed | Build with `REMOTE_SUPPORT` (enables `INI_CALL_HANDLER_ON_NEW_SECTION`); add the INI sections |
| A remote GPU's stats freeze at a stale value | That host's exporter went down | The last-good fallback keeps the last frame; restart the exporter on that host |
| Exporter prints `could not bind the export server on port N` | Port already in use | Kill the stale process or pick another port |
| Poll times out | Exporter running but not encoding | Give the exporter's 100 ms refresh loop a moment; poll in a retry loop |
| A container's exporter accepts connections but returns no frame | Container runs a stale binary | Rebuild and redeploy the new binary into the container |
| Firewall blocks 8765 | Inbound rule missing on the remote host | Allow TCP 8765 (e.g. Windows inbound rule / `ufw allow 8765`) |

---

## 8. Build options

```bash
# Enable / disable remote telemetry
-DREMOTE_SUPPORT=ON|OFF        # default: ON on Linux, OFF otherwise

# Turn on the wire round-trip GTest suite
-DBUILD_TESTING=ON            # builds tests/wireTest.cpp (remote_wire tests)
```

The wire codec is covered by `tests/wireTest.cpp`:
- `RemoteWire.RoundTrip` — encode → decode a multi-GPU, multi-process
  snapshot and assert every field and the `valid[]` bits survive.
- `RemoteWire.TruncatedBufferReturnsError` — a frame shorter than the
  declared length, or a clobbered magic, must be rejected (decode returns 0).
