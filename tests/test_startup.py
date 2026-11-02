"""Exercise the launcher with local Git repositories and a fake compiler."""
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]

class StartupTest(unittest.TestCase):
    def test_update_fallback_and_symlink(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            source, checkout, mock = root/'source', root/'checkout', root/'bin'
            source.mkdir(); mock.mkdir()
            def run(*args, cwd=source, env=None):
                return subprocess.check_output(args, cwd=cwd, env=env, stderr=subprocess.STDOUT, text=True)
            run('git', 'init', '-b', 'feat/remote-gpu-telemetry')
            run('git', 'config', 'user.email', 'test@example.invalid')
            run('git', 'config', 'user.name', 'Test')
            (source/'scripts').mkdir()
            shutil.copy2(ROOT/'scripts/nvtop-start', source/'scripts/nvtop-start')
            (source/'fixture').write_text('#!/bin/sh\necho revision-one "$@"\n')
            run('git', 'add', '.'); run('git', 'commit', '-m', 'one')
            run('git', 'clone', str(source), str(checkout))
            compiler = mock/'cmake'
            compiler.write_text('''#!/usr/bin/env python3
import os, pathlib, sys
if os.getenv('FAIL_BUILD'): sys.exit(1)
args=sys.argv
if '-B' in args:
 b=pathlib.Path(args[args.index('-B')+1]); (b/'src').mkdir(parents=True)
 (b/'src/nvtop').write_bytes((pathlib.Path(args[args.index('-S')+1])/'fixture').read_bytes())
 (b/'src/nvtop').chmod(0o755)
''')
            compiler.chmod(0o755)
            link = mock/'nvtop'; link.symlink_to(checkout/'scripts/nvtop-start')
            env = dict(os.environ, PATH=str(mock)+':'+os.environ['PATH'],
                       XDG_STATE_HOME=str(root/'state'), HOME=str(root))
            self.assertIn('revision-one --version', run(str(link), '--version', env=env))
            (checkout/'fixture').write_text('local dirty work')
            (source/'fixture').write_text('#!/bin/sh\necho revision-two "$@"\n')
            run('git', 'add', '.'); run('git', 'commit', '-m', 'two')
            self.assertIn('revision-one', run(str(link), env=dict(env, FAIL_BUILD='1')))
            self.assertIn('revision-two --export', run(str(link), '--export', env=env))
            self.assertEqual((checkout/'fixture').read_text(), 'local dirty work')
            self.assertIn('revision-two', run(str(link), env=dict(env, NVTOP_UPDATE_BRANCH='missing')))
            self.assertIn('revision-two', run(str(link), env=dict(env, NVTOP_AUTO_UPDATE='0')))
            self.assertTrue(list((root/'state/nvtop/logs').glob('start-*.log')))

if __name__ == '__main__': unittest.main()
