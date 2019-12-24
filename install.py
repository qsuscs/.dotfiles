#!/usr/bin/env python3

import os
import sys
from glob import glob

os.chdir(os.path.dirname(__file__))
exit = 0

for f in glob('dot.*'):
    dst = os.path.expanduser(
        '~/' + f[3:].replace("--", "\ufffd").replace("-", "/").replace("\ufffd", "-"))
    src = os.path.join(os.getcwd(), f)
    src_rel = os.path.relpath(src, os.path.dirname(dst))

    os.makedirs(os.path.dirname(dst), exist_ok=True)

    try:
        os.symlink(src_rel, dst)
    except FileExistsError:
        if not os.path.samefile(src, dst):
            print(dst + " exists and does not link do " + src)
            exit = 1

sys.exit(exit)
