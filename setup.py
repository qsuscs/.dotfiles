#!/usr/bin/env python3

import os
import sys
from glob import glob

os.chdir(os.path.dirname(__file__))
exit = 0

for f in glob('dot.*'):
    dst = os.path.expanduser('~/' + f[3:])
    src = os.path.join(os.getcwd(), f)
    try:
        os.symlink(src, dst)
    except FileExistsError:
        if not os.path.samefile(src, dst):
            print(dst + " exists and does not link do " + dst)
            exit = 1

sys.exit(exit)
