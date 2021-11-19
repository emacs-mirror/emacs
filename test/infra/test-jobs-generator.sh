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

# GNU Emacs support for the GitLab-specific build of Docker images.

# The presence of this file does not imply any FSF/GNU endorsement of
# Docker or any other particular tool.  Also, it is intended for
# evaluation purposes, thus possibly temporary.

# Maintainer: Michael Albinus <michael.albinus@gmx.de>
# URL: https://emba.gnu.org/emacs/emacs

for target in $(cd ..; make -s subdir-targets); do
    case $target in
        check-lib-src)
            changes="
        - lib-src/*.{h,c}
        - test/lib-src/*.el"
            ;;
        check-lisp-emacs-lisp)
            changes="
        - lisp/emacs-lisp/*.el
        - test/lisp/emacs-lisp/*.el"
            ;;
        check-lisp-emacs-lisp-eieio-tests)
            changes="
        - lisp/emacs-lisp/eieio-tests/*.el
        - test/lisp/emacs-lisp/eieio-tests/*.el"
            ;;
        check-lisp-emacs-lisp-faceup-tests)
            changes="
        - lisp/emacs-lisp/faceup-tests/*.el
        - test/lisp/emacs-lisp/faceup-tests/*.el"
            ;;
        check-lisp-mh-e)
            changes="
        - lisp/mh-e/*.el
        - test/lisp/mh-e/*.el"
            ;;
        check-lisp-so-long-tests)
            changes="
        - lisp/so-long-tests/*.el
        - test/lisp/so-long-tests/*.el"
            ;;
        check-misc)
            changes="
        - admin/*.el
        - test/misc/*.el"
            ;;
        check-src)
            changes="
        - src/*.{h,c}
        - test/src/*.el"
            ;;
        *)
            changes="
        - $(echo -n ${target##check-}/*.el | tr '-' '/')
        - $(echo -n test${target##check}/*.el | tr '-' '/')"
            ;;
    esac

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
