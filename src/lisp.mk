### lisp.mk --- src/Makefile fragment for GNU Emacs

## Copyright (C) 1985, 1987-1988, 1993-1995, 1999-2013 Free Software
## Foundation, Inc.

## This file is part of GNU Emacs.

## GNU Emacs is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## GNU Emacs is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

### Commentary:

## This is the list of all Lisp files that might be loaded into the
## dumped Emacs.  Some of them are not loaded on all platforms, but
## the DOC file on every platform uses them (because the DOC file is
## supposed to be platform-independent).
## It is arranged like this because it is easier to generate it
## semi-mechanically from loadup.el this way.
## Eg something like:
##   sed -e 's/"[ )].*//' -n -e '/(load "/ s/.*load "//p' loadup.el | \
##   grep -vE 'site-|ldefs-boot'
## minus any duplicates.
## Note that you can generally just add a ".elc" extension to every file
## that does not have an explicit .el extension, but beware of any
## no-byte-compile ones.

## Confusingly, international/cp51932 and international/eucjp-ms are
## unconditionally loaded from language/japanese, instead of being
## loaded directly from loadup.el; FIXME.

## Note that this list should not include lisp files which might not
## be present, like site-load.el and site-init.el; this makefile
## expects them all to be either present or buildable.

## Place loaddefs.el first, so it gets generated first, since it is on
## the critical path (relevant in parallel compilations).

### Code:

## NB: This list is parsed by sed in the main src/Makefile.
## Do not change the formatting.
lisp = \
	$(lispsource)/loaddefs.el \
	$(lispsource)/loadup.el \
	$(lispsource)/emacs-lisp/byte-run.elc \
	$(lispsource)/emacs-lisp/backquote.elc \
	$(lispsource)/subr.elc \
	$(lispsource)/version.elc \
	$(lispsource)/widget.elc \
	$(lispsource)/custom.elc \
	$(lispsource)/emacs-lisp/map-ynp.elc \
	$(lispsource)/cus-start.elc \
	$(lispsource)/international/mule.elc \
	$(lispsource)/international/mule-conf.elc \
	$(lispsource)/env.elc \
	$(lispsource)/format.elc \
	$(lispsource)/bindings.elc \
	$(lispsource)/files.elc \
	$(lispsource)/emacs-lisp/macroexp.elc \
	$(lispsource)/cus-face.elc \
	$(lispsource)/faces.elc \
	$(lispsource)/button.elc \
	$(lispsource)/startup.elc \
	$(lispsource)/minibuffer.elc \
	$(lispsource)/abbrev.elc \
	$(lispsource)/simple.elc \
	$(lispsource)/help.elc \
	$(lispsource)/jka-cmpr-hook.elc \
	$(lispsource)/epa-hook.elc \
	$(lispsource)/international/mule-cmds.elc \
	$(lispsource)/case-table.elc \
	$(lispsource)/international/characters.elc \
	$(lispsource)/composite.elc \
	$(lispsource)/international/charprop.el \
	$(lispsource)/language/chinese.elc \
	$(lispsource)/language/cyrillic.elc \
	$(lispsource)/language/indian.elc \
	$(lispsource)/language/sinhala.elc \
	$(lispsource)/language/english.elc \
	$(lispsource)/language/ethiopic.elc \
	$(lispsource)/language/european.elc \
	$(lispsource)/language/czech.elc \
	$(lispsource)/language/slovak.elc \
	$(lispsource)/language/romanian.elc \
	$(lispsource)/language/greek.elc \
	$(lispsource)/language/hebrew.elc \
	$(lispsource)/language/japanese.elc \
	$(lispsource)/international/cp51932.el \
	$(lispsource)/international/eucjp-ms.el \
	$(lispsource)/language/korean.elc \
	$(lispsource)/language/lao.elc \
	$(lispsource)/language/tai-viet.elc \
	$(lispsource)/language/thai.elc \
	$(lispsource)/language/tibetan.elc \
	$(lispsource)/language/vietnamese.elc \
	$(lispsource)/language/misc-lang.elc \
	$(lispsource)/language/utf-8-lang.elc \
	$(lispsource)/language/georgian.elc \
	$(lispsource)/language/khmer.elc \
	$(lispsource)/language/burmese.elc \
	$(lispsource)/language/cham.elc \
	$(lispsource)/indent.elc \
	$(lispsource)/window.elc \
	$(lispsource)/frame.elc \
	$(lispsource)/term/tty-colors.elc \
	$(lispsource)/font-core.elc \
	$(lispsource)/facemenu.elc \
	$(lispsource)/emacs-lisp/syntax.elc \
	$(lispsource)/font-lock.elc \
	$(lispsource)/jit-lock.elc \
	$(lispsource)/mouse.elc \
	$(lispsource)/scroll-bar.elc \
	$(lispsource)/select.elc \
	$(lispsource)/emacs-lisp/timer.elc \
	$(lispsource)/isearch.elc \
	$(lispsource)/rfn-eshadow.elc \
	$(lispsource)/menu-bar.elc \
	$(lispsource)/emacs-lisp/lisp.elc \
	$(lispsource)/textmodes/page.elc \
	$(lispsource)/register.elc \
	$(lispsource)/textmodes/paragraphs.elc \
	$(lispsource)/emacs-lisp/lisp-mode.elc \
	$(lispsource)/textmodes/text-mode.elc \
	$(lispsource)/textmodes/fill.elc \
	$(lispsource)/newcomment.elc \
	$(lispsource)/replace.elc \
	$(lispsource)/emacs-lisp/tabulated-list.elc \
	$(lispsource)/buff-menu.elc \
	$(lispsource)/fringe.elc \
	$(lispsource)/emacs-lisp/regexp-opt.elc \
	$(lispsource)/image.elc \
	$(lispsource)/international/fontset.elc \
	$(lispsource)/dnd.elc \
	$(lispsource)/tool-bar.elc \
	$(lispsource)/dynamic-setting.elc \
	$(lispsource)/x-dnd.elc \
	$(lispsource)/term/common-win.elc \
	$(lispsource)/term/x-win.elc \
	$(lispsource)/w32-vars.elc \
	$(lispsource)/term/w32-win.elc \
	$(lispsource)/ls-lisp.elc \
	$(lispsource)/disp-table.elc \
	$(lispsource)/dos-w32.elc \
	$(lispsource)/w32-fns.elc \
	$(lispsource)/dos-fns.elc \
	$(lispsource)/dos-vars.elc \
	$(lispsource)/term/pc-win.elc \
	$(lispsource)/term/internal.elc \
	$(lispsource)/term/ns-win.elc \
	$(lispsource)/mwheel.elc \
	$(lispsource)/emacs-lisp/float-sup.elc \
	$(lispsource)/vc/vc-hooks.elc \
	$(lispsource)/vc/ediff-hook.elc \
	$(lispsource)/tooltip.elc


### lisp.mk ends here
