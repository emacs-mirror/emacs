# Makefile for "Calc", the GNU Emacs Calculator.
#  Copyright (C) 1991, 1992, 1993 Free Software Foundation.
#  Author: Dave Gillespie.
#  Author's address: daveg@synaptics.com.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation (any version).

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.


# To install Calc for private use, type `make'.
# To install Calc for public use, type `make install'.

# How to read a Makefile:
#   The command `make target' looks for `target:' in the Makefile.
#   First, any sub-targets after the `:' are made.
#   Then, the Unix commands on the following lines are executed.
#   `$(SYMBOL)' expands according to the `SYMBOL =' definition below.


# Programs.
EMACS    = emacs
TEX      = tex
TEXINDEX = texindex
MAKEINFO = makeinfo
MAKE	 = make
ECHO	 = @echo
REMOVE	 = -rm -f
# (The leading `@' tells "make" not to echo the command itself during make;
#  The leading `-' tells "make" to keep going if the command fails.)

# Other macros.
EFLAGS   = -batch
MAINT	 = -l calc-maint.elc

# Control whether intermediate files are kept.
PURGE	 = -rm -f
#PURGE	 = echo Not deleting:



# Do full Calc installation.  (Note that `make' == `make all'.)
# These are written this way instead of `all: compile private info'
# to make the steps more explicit while the `make' is in progress.
all:
	$(MAKE) compile
	$(MAKE) private
	$(MAKE) info
	$(ECHO) "Calc is now installed."

install:
	$(MAKE) compile
	$(MAKE) public
	$(MAKE) info
	$(ECHO) "Calc is now installed."


# Compile Calc.
compile: maint
	$(EMACS) $(EFLAGS) $(MAINT) -f calc-compile


# Add autoload and set-global-key commands to system default file.
public: maint
	$(EMACS) $(EFLAGS) $(MAINT) -f calc-public-autoloads


# Add autoload and set-global-key commands to ~/.emacs file.
private: maint
	$(EMACS) $(EFLAGS) $(MAINT) -f calc-private-autoloads


# Format the Calc manual for the Info system using makeinfo.
info: calc.info
calc.info: calc.texinfo
	-$(MAKEINFO) calc.texinfo
	$(ECHO) "Please ignore warnings for Copying, Getting Started, and Interactive Tutorial."
	$(MAKE) texinfo


# Format the Calc manual for the Info system using texinfo.el.
# (Use this only if you do not have makeinfo.)
texinfo: calc.info-2
calc.info-2: calc.texinfo
	$(EMACS) $(EFLAGS) calc.texinfo -f texinfo-format-buffer -f save-buffer


# Format the Calc manual as one printable volume using TeX.
tex:
	$(REMOVE) calc.aux
	$(TEX) calc.texinfo
	$(TEXINDEX) calc.[cfkptv]?
	$(TEX) calc.texinfo
	$(PURGE) calc.cp calc.fn calc.pg calc.tp calc.vr
	$(PURGE) calc.cps calc.fns calc.kys calc.pgs calc.tps calc.vrs
	$(PURGE) calc.toc
# Note, calc.aux and calc.ky are left behind for the benefit of "make summary".

# Format the Calc manual as two printable volumes (Tutorial and Reference).
tex2: texsplit texvol1 texvol2

# Format the Calc Tutorial volume only.
textut: texsplit1 texvol1

# Format the Calc Reference volume only.
texref: texsplit2 texvol2

texsplit: maint
	$(EMACS) $(EFLAGS) $(MAINT) calc.texinfo -f calc-split-manual

texsplit1: maint
	$(EMACS) $(EFLAGS) $(MAINT) calc.texinfo -f calc-split-tutorial

texsplit2: maint
	$(EMACS) $(EFLAGS) $(MAINT) calc.texinfo -f calc-split-reference

texvol1:
	$(TEX) calctut.tex
	$(TEXINDEX) calctut.??
	$(TEX) calctut.tex
	$(PURGE) calctut.tex calctut.?? calctut.??s calctut.aux calctut.toc

texvol2:
	$(TEX) calcref.tex
	$(TEXINDEX) calcref.??
	$(TEX) calcref.tex
	$(PURGE) calcref.tex calcref.?? calcref.??s calcref.aux calcref.toc


# Format the Calc summary separately using TeX.
summary: texsum
	$(TEX) calcsum.tex
	$(PURGE) calcsum.?? calcsum.aux calcsum.toc

texsum: maint
	$(EMACS) $(EFLAGS) $(MAINT) calc.texinfo -f calc-split-summary

isummary: texisum
	$(TEX) calcsum.tex
	$(PURGE) calcsum.?? calcsum.aux calcsum.toc

texisum: maint
	$(EMACS) $(EFLAGS) $(MAINT) calc.texinfo -f calc-inline-summary


# All this because "-l calc-maint" doesn't work.
maint: calc-maint.elc
calc-maint.elc: calc-maint.el
	cp calc-maint.el calc-maint.elc


# Create an Emacs TAGS file
tags: TAGS
TAGS:
	etags *.el


# Delete .elc files and other reconstructible files.
clean:  clean.elc clean.info clean.tex

clean.elc:
	$(REMOVE) calc-*.elc
	$(REMOVE) macedit.elc

clean.info:
	$(REMOVE) calc.info*

clean.tex:
	$(REMOVE) calc.cp calc.fn calc.ky calc.pg calc.tp calc.vr
	$(REMOVE) calc.cps calc.fns calc.kys calc.pgs calc.tps calc.vrs
	$(REMOVE) calc.aux calc.log calc.toc calc.dvi
	$(REMOVE) calcref.*
	$(REMOVE) calctut.*
	$(REMOVE) calcsum.*


