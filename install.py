#!/usr/bin/env python3

import os
import sys
from pathlib import Path

here = Path(__file__).parent.resolve()
home = Path.home().resolve()

exit = 0

for f in here.glob("dot.*"):
    dst = home / Path(
        f.name[3:].replace("--", "\ufffd").replace("-", "/").replace("\ufffd", "-")
    )
    try:
        src = f.relative_to(dst.parent)
    except ValueError:
        src = os.path.relpath(str(f), str(dst.parent))

    dst.parent.mkdir(parents=True, exist_ok=True)

    try:
        dst.symlink_to(src)
    except FileExistsError:
        if not (dst.parent / src).samefile(dst):
            print("{} exists and does not link to {}".format(dst, src))
            exit = 1

sys.exit(exit)
