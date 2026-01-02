#! /bin/sh

# Generate a new key and update the signatures for tests.

# Copyright (C) 2020-2026 Free Software Foundation, Inc.

# This file is part of GNU Emacs.

# GNU Emacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

export GPG_AGENT=""
KEYRING="./key.ring"
TRUSTDB="./trust.db"
GPG="gpg --no-default-keyring --trustdb-name $TRUSTDB --keyring $KEYRING --yes"

rm $KEYRING
#$GPG --full-generate-key
#$GPG --export --armor > "../key.pub"
#$GPG --export-secret-keys -armor > "../key.sec"
$GPG --import ../key.sec
$GPG --detach-sign --sign "./archive-contents"
$GPG --detach-sign --sign "./elpa-packages.eld"
$GPG --detach-sign --sign "./signed-good-1.0.el"
