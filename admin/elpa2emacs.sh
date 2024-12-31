#!/bin/bash
# Merge ELPA package into the Emacs repository

# Copyright (C) 2024 Free Software Foundation, Inc.

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
# - Move mentioned files to elisp-benchmarks/ (modify this if you need a
#   different root)
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

# Repos will be cloned in this directory if they don't exist already.
# No repo updates will be performed automatically.
#
# It is recommended to create a worktree of the Emacs repo (but not the
# ELPA one!) here.  This can be done by moving on to any existing emacs
# worktree and typing:
#
#   git worktree add /path-to-this-files-directory/emacs scratch/elpa2emacs
#
# If you want to avoid another ELPA checkout, you can copy an existing
# checkout to the elpa subdirectory of this directory, but you shouldn't
# use a worktree as the repo itself will be destructively modified.
#
# If (when) things go sour, this normally helps:
#
#   rm -rf elpa # will be recloned
#   cd emacs
#   git merge --abort
#   cd ../
#
# Run like this:
#
#   bash ./elpa2emacs externals/elisp-benchmarks elisp-benchmarks.el benchmarks
#

# arguments
BRANCH="$1" # a branch name in the ELPA repo
shift
PATHS="$@" # paths of files or directories to be matched.

# clone repos
test -r elpa/.git || git clone -b "$BRANCH" https://git.savannah.gnu.org/git/emacs/elpa.git
test -r emacs/.git || git clone -b master https://git.savannah.gnu.org/git/emacs.git

# filter elpa to keep only the appropriate files.  This destroys the current
# elpa repo, including copies in other worktrees!

NONCE=nonce"$(date +'%s')"
(
    cd elpa
    git checkout "$BRANCH"
    git checkout -b "$NONCE"

    > tmp-list
    for P in $PATHS; do
	echo "$P" >> tmp-list
    done
    git filter-repo -f --paths-from-file tmp-list --to-subdirectory-filter elisp-benchmarks/
)

# Merge into the emacs repo.  This will not destroy the entire Emacs
# repository, but will add a remote which is propagated to all
# worktrees.

(
    cd emacs
    git reset --hard origin/master
    # add filtered elpa as upstream
    git remote add elpa2emacs-filtered-elpa $PWD/../elpa/
    git fetch --all
    git merge remotes/elpa2emacs-filtered-elpa/$NONCE --allow-unrelated-histories --no-commit
    # make a commit
    git commit --no-verify -m "; Merge from https://git.savannah.gnu.org/git/emacs/elpa.git"
)
