#!/bin/bash -e
# Merge ELPA package into the Emacs repository

# Copyright (C) 2024-2025 Free Software Foundation, Inc.

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

# This code merges an ELPA package that lives in a branch of
# https://git.savannah.gnu.org/git/emacs/elpa.git into the Emacs repo.
#
# It attempts to do the following things:
#
# - Move mentioned files to new directories
#
# - Preserve complete history from original repo for the files

# Dependencies required
#
# - https://github.com/newren/git-filter-repo
#    nix shell nixpkgs#git-filter-repo
#    arch: pacman -S git-filter-repo
# - git

# The code is originally from
# https://gist.github.com/2ed97f2ec85958986983d5cb78202770.git

# Authors:
#  Payas Relekar <relekarpayas@gmail.com>
#  João Távora <joaotavora@gmail.com>
#  Pip Cet <pipcet@protonmail.com>

# The ELPA repo will be cloned into the "elpa" subdirectory of the emacs
# repository, unless it's already there.  Don't use a worktree for that,
# though!
#
# Run like this:
#
#   bash -ex ./admin/elpa2emacs.sh 03e668caf878c9b9780356e447e3fd85e0696f77 "benchmarks==>benchmarks/benchmarks" "resources==>benchmarks/resources" "elisp-benchmarks.el==>benchmarks/elisp-benchmarks.el"
#

# arguments
OLDDIR="$PWD"
TMPDIR=`mktemp -d`
COMMIT="$1" # a commit in the ELPA repo
shift
PATHS="$@" # paths of files or directories to be matched.

if ! test -f "$PWD"/etc/JOKES; then
    echo "Run this in the root directory of an Emacs repository"
    exit 1
fi

read -r -p "This script is potentially dangerous.  Enter YES to run it: "
if ! test x"$REPLY" = xYES; then
    echo "Not confirmed."
    exit 1
fi

NONCE=nonce"$(date +'%s')"

pushd "$TMPDIR"
# clone repos
if ! [ -r "$OLDDIR"/elpa/.git ]; then
    git clone https://git.savannah.gnu.org/git/emacs/elpa.git "$OLDDIR"/elpa
fi
git clone "$OLDDIR"/elpa "$TMPDIR"/elpa
COMMIT="$(cd "$TMPDIR/elpa"; git rev-parse "$COMMIT")"

# filter elpa to keep only the appropriate files.  This destroys the
# newly-created copy of the elpa repo.

pushd elpa
git checkout "$COMMIT"
git checkout -b "$NONCE"

> tmp-list
for P in $PATHS; do
    echo "$P" >> tmp-list
done
for P in $PATHS; do
    echo "$P" | sed -e 's/==>.*//g' >> tmp-list
done
git filter-repo -f --paths-from-file tmp-list
popd
popd

# Merge into the emacs repo.  This will not destroy the entire Emacs
# repository, but will add a branch which is visible in all worktrees.

# add filtered elpa as upstream
git remote add elpa2emacs-filtered-elpa-$NONCE $TMPDIR/elpa/
git fetch elpa2emacs-filtered-elpa-$NONCE
git merge remotes/elpa2emacs-filtered-elpa-$NONCE/$NONCE --allow-unrelated-histories --no-commit
git remote remove elpa2emacs-filtered-elpa-$NONCE

rm -rf "$TMPDIR"
echo "You can now commit the merge by running: git commit -n"
echo
echo "After that, you can delete the elpa/ directory in the emacs repository."
