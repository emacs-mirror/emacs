### emacs.csh

## Copyright (C) 1999, 2001, 2002, 2003, 2004, 2005, 2006, 2007
##   Free Software Foundation, Inc.

## Author: Michael DeCorte

## COPYING PERMISSIONS:

## Permission is hereby granted, free of charge, to any person
## obtaining a copy of this file, to deal in the file without
## restriction, including without limitation the rights to use, copy,
## modify, merge, publish, distribute, sublicense, and/or sell copies
## of the file, and to permit persons to whom the file is furnished to
## do so, subject to the following condition:
##
## The above copyright notice and this permission notice shall be
## included in all copies or substantial portions of the file.

### Commentary:

## This defines a csh command named `edit' which resumes an
## existing Emacs or starts a new one if none exists.
## One way or another, any arguments are passed to Emacs to specify files
## (provided you have loaded `resume.el').

## These are the possible values of $whichjob
## 1 = new ordinary emacs (the -nw is so that it doesn't try to do X)
## 2 = resume emacs
## 3 = new emacs under X (-i is so that you get a reasonable icon)
## 4 = resume emacs under X
## 5 = new emacs under suntools
## 6 = resume emacs under suntools
## 7 = new emacs under X and suntools - doesn't make any sense, so use X
## 8 = resume emacs under X and suntools - doesn't make any sense, so use X
set EMACS_PATTERN="^\[[0-9]\]  . Stopped ............ $EMACS"

alias edit 'set emacs_command=("emacs -nw \!*" "fg %emacs" "emacs -i \!* &"\
 "emacsclient \!* &" "emacstool \!* &" "emacsclient \!* &" "emacs -i \!* &"\
 "emacsclient \!* &") ; \
 jobs >! $HOME/.jobs; grep "$EMACS_PATTERN" < $HOME/.jobs >& /dev/null; \
 @ isjob = ! $status; \
 @ whichjob = 1 + $isjob + $?DISPLAY * 2 + $?WINDOW_PARENT * 4; \
 test -S ~/.emacs_server && emacsclient \!* \
 || echo `pwd` \!* >! ~/.emacs_args && eval $emacs_command[$whichjob]'

# arch-tag: 433d58df-15b9-446f-ad37-f0393e3a23d4
