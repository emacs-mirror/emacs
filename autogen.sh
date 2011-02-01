#!/bin/sh

echo "Please read INSTALL.BZR for instructions on how to build Emacs from Bazaar."

# Exit with failure, since people may have generic build scripts that
# try things like "autogen.sh && ./configure && make".
exit 1
