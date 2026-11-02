"""PTY regression for a brief outage: run after building build/src/nvtop."""
import os
import pty
import select
import socket
import struct
import subprocess
import tempfile
import threading
import time
import fcntl
import termios
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
with tempfile.TemporaryDirectory() as temporary:
    folder = Path(temporary)
    fixture = folder / 'frame.c'
    fixture.write_text('''#include <stdio.h>
#include "nvtop/remote_proto.h"
int main(void) {
 LIST_HEAD(devices);
 struct gpu_info d = {0};
 SET_GPUINFO_DYNAMIC(&d.dynamic_info, gpu_util_rate, 0);
 list_add_tail(&d.list, &devices);
 unsigned char buffer[65536];
 size_t n = remote_wire_encode(&devices, buffer, sizeof(buffer));
 fwrite(buffer, 1, n + 4, stdout);
}
''')
    executable = folder / 'frame'
    subprocess.run(['cc', '-std=gnu11', '-I'+str(ROOT/'include'), str(fixture),
                    str(ROOT/'src/remote_wire.c'), '-o', str(executable)], check=True)
    frame = subprocess.check_output([str(executable)])
    server = socket.socket()
    server.bind(('127.0.0.1', 0)); server.listen(); server.settimeout(.1)
    pause, stop = threading.Event(), threading.Event()
    def serve():
        while not stop.is_set():
            try: connection, _ = server.accept()
            except socket.timeout: continue
            with connection:
                connection.settimeout(1)
                try:
                    connection.recv(1)
                    if not pause.is_set(): connection.sendall(frame)
                except OSError: pass
    thread = threading.Thread(target=serve); thread.start()
    config = folder/'config.ini'
    config.write_text('[RemoteHost]\nHost=127.0.0.1\nName=Idle Intel GPU\nPort=%d\n' % server.getsockname()[1])
    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 40, 140, 0, 0))
    env = dict(os.environ, TERM='xterm-256color', HOME=str(folder))
    process = subprocess.Popen([str(ROOT/'build/src/nvtop'), '-c', str(config)],
                               stdin=slave, stdout=slave, stderr=slave, env=env)
    os.close(slave)
    def read_for(seconds):
        data = b''; deadline = time.monotonic()+seconds
        while time.monotonic() < deadline:
            if select.select([master], [], [], .1)[0]:
                data += os.read(master, 65536)
        return data
    try:
        initial = read_for(2)
        assert b'@ 127.0.0.1' in initial, initial
        pause.set(); interrupted = read_for(2)
        assert b'GPU offline' not in interrupted, interrupted
        assert b'retrying' in interrupted, interrupted
        pause.clear(); recovered = read_for(2)
        assert b'GPU offline' not in recovered, recovered
        os.write(master, b'q'); process.wait(timeout=5)
        assert process.returncode == 0
        print('PASS: idle GPU, host header, brief retry without offline overlay, recovery')
    finally:
        if process.poll() is None: process.kill(); process.wait()
        os.close(master); stop.set(); thread.join(); server.close()
