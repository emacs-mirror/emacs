### Makefile for Emacs etc/ directory

## Copyright (C) 1993, 1994, 2001, 2002, 2003, 2004, 2005, 2006,
##   2007 Free Software Foundation, Inc.

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


## Top-level targets.

## PS files included with Emacs.
all: refcards dired-refcards misc-refcards


refcards: refcard.ps cs-refcard.ps de-refcard.ps fr-refcard.ps \
          pl-refcard.ps pt-br-refcard.ps ru-refcard.ps sk-refcard.ps

dired-refcards: dired-ref.ps cs-dired-ref.ps fr-drdref.ps sk-dired-ref.ps

## No PS files: vipcard.tex viperCard.tex survival.tex sk-survival.tex
misc-refcards: calccard.ps gnus-booklet.ps gnus-refcard.ps orgcard.ps

## Following PS files are not included with Emacs.
survival-cards: survival.ps cs-survival.ps sk-survival.ps

viper-cards: vipcard.ps viperCard.ps


## dvi files.

## FIXME the sk-*.tex files say to use csplain, but were not
## traditionally in this rule. Fix the doc or the rule.
cs-refcard.dvi cs-dired-ref.dvi cs-survival.dvi: %.dvi:	%.tex
	csplain $<

pl-refcard.dvi: %.dvi: %.tex
	if ! kpsewhich -format=fmt mex > /dev/null; then \
	  echo "No mex format found."; false; \
	fi
	tex $<

ru-refcard.dvi gnus-refcard.dvi: %.dvi: %.tex
	latex $<

gnus-booklet.dvi: gnus-refcard.tex
	latex '\def\booklettrue{}\def\letterpapertrue{}\input{gnus-refcard}'
	mv gnus-refcard.dvi $@

## Everything not explicitly listed above.
%.dvi: %.tex
	tex $<


## PostScript files.

## Note that some of the tex files (refcard, de-refcard, fr-refcard,
## pt-br-refcard) have settings for letter or a4 paper. Following are
## the default paper sizes (letter for English, A4 for translations).
## FIXME orgcard.ps does not fit on letter (see orgcard.tex).

## A4, landscape.
de-refcard.ps fr-refcard.ps pt-br-refcard.ps orgcard.ps: %.ps: %.dvi
	dvips -t a4 -t landscape $< -o $@

## A4, portrait.
cs-dired-ref.ps cs-refcard.ps cs-survival.ps fr-drdref.ps \
pl-refcard.ps ru-refcard.ps \
sk-dired-ref.ps sk-refcard.ps sk-survival.ps: %.ps: %.dvi
	dvips -t a4 $< -o $@

## letter, landscape.
calccard.ps refcard.ps: %.ps: %.dvi
	dvips -t letter -t landscape $< -o $@

## letter, portrait.
dired-ref.ps gnus-booklet.ps gnus-refcard.ps survival.ps vipcard.ps \
viperCard.ps: %.ps: %.dvi
	dvips -t letter $< -o $@


.PHONY: mostlyclean clean distclean maintainer-clean unlock relock

mostlyclean:
	-rm -f *.dvi *.log

## Note does not delete generated ps files.
clean distclean maintainer-clean: mostlyclean
	-rm -f DOC*


SOURCES = [0-9A-QS-Z]* README *.[ch16] emacs.* ms-* \
	spook-lines termcap.* *.tex

unlock:
	chmod u+w $(SOURCES)

relock:
	chmod u-w $(SOURCES)

# ${etcdir}/e/eterm-color is used by ../lisp/term.el.
# TERMINFO systems use terminfo files compiled by the Terminfo Compiler (tic).
# These files are binary, and depend on the version of tic, but they seem
# to be system-independent and backwardly compatible.
# So there should be no need to recompile the distributed binary version.
TIC=tic
e/eterm-color: e/eterm-color.ti
	TERMINFO=`pwd`; export TERMINFO; $(TIC) e/eterm-color.ti


## arch-tag: 4261f003-cf77-4478-a10a-5284e9d8f797
### Makefile ends here
