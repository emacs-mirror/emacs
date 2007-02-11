### emacs.bash --- contact/resume an existing Emacs, or start a new one

## Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007
##   Free Software Foundation, Inc.

## Author: Noah Friedman

## This file is part of GNU Emacs.

## GNU Emacs is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.

## GNU Emacs is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with GNU Emacs; see the file COPYING.  If not, write to the
## Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
## Boston, MA 02110-1301, USA.

### Commentary:

## This defines a bash command named `edit' which contacts/resumes an
## existing emacs or starts a new one if none exists.

## One way or another, any arguments are passed to emacs to specify files
## (provided you have loaded `resume.el').

## This function assumes the emacs program is named `emacs' and is somewhere
## in your load path.  If either of these is not true, the most portable
## (and convenient) thing to do is to make an alias called emacs which
## refers to the real program, e.g.
##
##        alias emacs=/usr/local/bin/gemacs

function edit ()
{
 local windowsys="${WINDOW_PARENT+sun}"

 windowsys="${windowsys:-${DISPLAY+x}}"

 if [ -n "${windowsys:+set}" ]; then
    # Do not just test if these files are sockets.  On some systems
    # ordinary files or fifos are used instead.  Just see if they exist.
    if [ -e "${HOME}/.emacs_server" -o -e "/tmp/emacs${UID}/server" ]; then
       emacsclient "$@"
       return $?
    else
       echo "edit: starting emacs in background..." 1>&2
    fi

    case "${windowsys}" in
      x ) (emacs "$@" &) ;;
      sun ) (emacstool "$@" &) ;;
    esac
 else
    if jobs %emacs 2> /dev/null ; then
       echo "$(pwd)" "$@" >| ${HOME}/.emacs_args && fg %emacs
    else
       emacs "$@"
    fi
 fi
}


# arch-tag: 1e1b74b9-bf2c-4b23-870f-9eebff7515cb
### emacs.bash ends here
