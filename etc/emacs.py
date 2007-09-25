"""Wrapper for version-specific implementations of python.el helper
functions """

import sys

if sys.version_info[0] == 3:
    from emacs3 import *
else:
    from emacs2 import *

# arch-tag: 894b5227-638f-45fd-8567-0417d5c35900
