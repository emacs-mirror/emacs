#!/bin/sh

# Copyright (C) 2021 Free Software Foundation, Inc.
#
#  This file is part of GNU Emacs.
#
#  GNU Emacs is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  GNU Emacs is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

# GNU Emacs support for the gitlab-ci.yml template generation.

# The presence of this file does not imply any FSF/GNU endorsement of
# any particular service that uses that protocol.  Also, it is intended for
# evaluation purposes, thus possibly temporary.

# Maintainer: Michael Albinus <michael.albinus@gmx.de>
# URL: https://emba.gnu.org/emacs/emacs

cd test
SUBDIRS=\
$(find lib-src lisp misc src -type d \
  ! \( -path "*resources*" -o -path "*auto-save-list" \) -print | sort -)

for subdir in $SUBDIRS; do
    target=check-$(echo $subdir | tr '/' '-')

    case $target in
        check*-src)
            changes="
        - $subdir/*.{h,c}
        - test/$subdir/*.el"
            ;;
        check-misc)
            changes="
        - admin/*.el
        - test/$subdir/*.el"
            ;;
        *)
            changes="
        - $subdir/*.el
        - test/$subdir/*.el"
            ;;
    esac

    cat <<EOF
include:
  - local: '/test/infra/default-gitlab-ci.yml'

EOF

    cat <<EOF
test${target##check}-inotify:
  stage: normal
  extends: [.job-template, .test-template]
  rules:
    - changes: $changes
  variables:
    target: emacs-inotify
    make_params: "-C test $target"

EOF
done
