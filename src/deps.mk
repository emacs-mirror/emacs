### deps.mk --- src/Makefile fragment for GNU Emacs

## Copyright (C) 1985, 1987, 1988, 1993, 1994, 1995, 1999, 2000, 2001, 2002,
##   2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
##   Free Software Foundation, Inc.

## This file is part of GNU Emacs.

## GNU Emacs is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## 
## GNU Emacs is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

## Commentary:
## 
## This file is inserted in src/Makefile if AUTO_DEPEND=no.
## It defines static dependencies between the various source files.

## FIXME some of these dependencies are platform-specific.
## Eg callproc.c only depends on w32.h for WINDOWSNT builds.
## One way to fix this would be to replace w32.h (etc) by $(W32_H),
## a variable set by configure.  Does not seem worth the trouble.
## Since the w32 build does not even use this file, you might ask
## why these dependencies are here at all...

## nsgui.h: In fact, every .o file depends directly or indirectly on
## dispextern.h and hence nsgui.h under NS.  But the ones that actually
## use stuff there are more limited.

### Code:

atimer.o: atimer.c atimer.h syssignal.h systime.h lisp.h blockinput.h \
 $(config_h)
bidi.o: bidi.c buffer.h character.h dispextern.h lisp.h \
   biditype.h bidimirror.h $(config_h)
buffer.o: buffer.c buffer.h region-cache.h commands.h window.h \
   $(INTERVALS_H) blockinput.h atimer.h systime.h character.h \
   indent.h keyboard.h coding.h keymap.h frame.h lisp.h $(config_h)
callint.o: callint.c window.h commands.h buffer.h keymap.h \
   keyboard.h dispextern.h systime.h coding.h composite.h lisp.h $(config_h)
callproc.o: callproc.c epaths.h buffer.h commands.h lisp.h $(config_h) \
   process.h systty.h syssignal.h character.h coding.h ccl.h msdos.h \
   composite.h w32.h blockinput.h atimer.h systime.h frame.h termhooks.h \
   buffer.h
casefiddle.o: casefiddle.c syntax.h commands.h buffer.h character.h \
   composite.h keymap.h lisp.h $(config_h)
casetab.o: casetab.c buffer.h character.h lisp.h $(config_h)
category.o: category.c category.h buffer.h charset.h keymap.h	\
   character.h lisp.h $(config_h)
ccl.o: ccl.c ccl.h charset.h character.h coding.h composite.h lisp.h $(config_h)
character.o: character.c character.h buffer.h charset.h composite.h disptab.h \
   lisp.h $(config_h)
charset.o: charset.c charset.h character.h buffer.h coding.h composite.h \
   disptab.h lisp.h $(config_h)
chartab.o: charset.h character.h ccl.h lisp.h $(config_h)
coding.o: coding.c coding.h ccl.h buffer.h character.h charset.h composite.h \
   window.h dispextern.h frame.h termhooks.h lisp.h $(config_h)
cm.o: cm.c frame.h cm.h termhooks.h termchar.h dispextern.h lisp.h $(config_h)
cmds.o: cmds.c syntax.h buffer.h character.h commands.h window.h lisp.h \
   $(config_h) msdos.h dispextern.h keyboard.h keymap.h systime.h \
   coding.h frame.h composite.h
pre-crt0.o: pre-crt0.c
dbusbind.o: dbusbind.c termhooks.h frame.h keyboard.h lisp.h $(config_h)
dired.o: dired.c commands.h buffer.h lisp.h $(config_h) character.h charset.h \
   coding.h regex.h systime.h blockinput.h atimer.h composite.h
dispnew.o: dispnew.c systime.h commands.h process.h frame.h coding.h \
   window.h buffer.h termchar.h termopts.h termhooks.h cm.h \
   disptab.h indent.h $(INTERVALS_H) nsgui.h \
   xterm.h blockinput.h atimer.h character.h msdos.h keyboard.h \
   syssignal.h lisp.h $(config_h)
doc.o: doc.c lisp.h $(config_h) epaths.h buffer.h keyboard.h keymap.h \
   character.h systime.h coding.h composite.h
doprnt.o: doprnt.c character.h lisp.h $(config_h)
dosfns.o: buffer.h termchar.h termhooks.h frame.h blockinput.h window.h \
   msdos.h dosfns.h dispextern.h charset.h coding.h atimer.h systime.h \
   lisp.h $(config_h)
editfns.o: editfns.c window.h buffer.h systime.h $(INTERVALS_H) character.h \
   coding.h frame.h blockinput.h atimer.h lisp.h $(config_h)
emacs.o: emacs.c commands.h systty.h syssignal.h blockinput.h process.h \
   termhooks.h buffer.h atimer.h systime.h $(INTERVALS_H) lisp.h $(config_h) \
   window.h dispextern.h keyboard.h keymap.h frame.h coding.h
fileio.o: fileio.c window.h buffer.h systime.h $(INTERVALS_H) character.h \
   coding.h msdos.h blockinput.h atimer.h lisp.h $(config_h) frame.h commands.h
filelock.o: filelock.c buffer.h character.h coding.h systime.h composite.h \
   lisp.h $(config_h)
filemode.o: filemode.c $(config_h)
frame.o: frame.c xterm.h window.h frame.h termhooks.h commands.h keyboard.h \
   blockinput.h atimer.h systime.h buffer.h character.h fontset.h font.h    \
   msdos.h dosfns.h dispextern.h w32term.h nsgui.h termchar.h coding.h \
   composite.h lisp.h $(config_h) termhooks.h ccl.h
fringe.o: fringe.c dispextern.h nsgui.h frame.h window.h buffer.h termhooks.h \
   blockinput.h atimer.h systime.h lisp.h $(config_h)
font.o: font.c dispextern.h frame.h window.h ccl.h character.h charset.h \
   font.h lisp.h $(config_h) buffer.h composite.h fontset.h xterm.h nsgui.h
ftfont.o: dispextern.h frame.h character.h charset.h composite.h font.h \
   lisp.h $(config_h) blockinput.h atimer.h systime.h coding.h fontset.h \
   ccl.h ftfont.h
fontset.o: fontset.c fontset.h ccl.h buffer.h character.h \
   charset.h frame.h keyboard.h termhooks.h font.h lisp.h $(config_h) \
   blockinput.h atimer.h systime.h coding.h $(INTERVALS_H) nsgui.h \
   window.h xterm.h
getloadavg.o: getloadavg.c $(config_h)
gtkutil.o: gtkutil.c gtkutil.h xterm.h lisp.h frame.h lisp.h $(config_h) \
   blockinput.h window.h atimer.h systime.h termhooks.h keyboard.h \
   charset.h coding.h syssignal.h dispextern.h composite.h
image.o: image.c frame.h window.h dispextern.h blockinput.h atimer.h \
   systime.h xterm.h w32term.h w32gui.h font.h epaths.h character.h coding.h \
   nsterm.h nsgui.h lisp.h $(config_h) composite.h termhooks.h ccl.h
indent.o: indent.c frame.h window.h indent.h buffer.h lisp.h $(config_h) \
   termchar.h termopts.h disptab.h region-cache.h character.h category.h \
   keyboard.h systime.h coding.h $(INTERVALS_H)
insdel.o: insdel.c window.h buffer.h $(INTERVALS_H) blockinput.h character.h \
   dispextern.h atimer.h systime.h region-cache.h lisp.h $(config_h)
keyboard.o: keyboard.c termchar.h termhooks.h termopts.h buffer.h character.h \
   commands.h frame.h window.h macros.h disptab.h keyboard.h syssignal.h \
   systime.h syntax.h $(INTERVALS_H) blockinput.h atimer.h composite.h \
   xterm.h puresize.h msdos.h keymap.h w32term.h nsterm.h nsgui.h coding.h \
   lisp.h $(config_h)
keymap.o: keymap.c buffer.h commands.h keyboard.h termhooks.h blockinput.h \
   atimer.h systime.h puresize.h character.h charset.h $(INTERVALS_H) \
   keymap.h window.h coding.h frame.h lisp.h $(config_h)
lastfile.o: lastfile.c $(config_h)
macros.o: macros.c window.h buffer.h commands.h macros.h keyboard.h \
   dispextern.h lisp.h $(config_h) systime.h coding.h composite.h
gmalloc.o: gmalloc.c $(config_h)
ralloc.o: ralloc.c lisp.h $(config_h)
vm-limit.o: vm-limit.c mem-limits.h lisp.h $(config_h)
marker.o: marker.c buffer.h character.h lisp.h $(config_h)
md5.o: md5.c md5.h $(config_h)
minibuf.o: minibuf.c syntax.h frame.h window.h keyboard.h systime.h \
   buffer.h commands.h character.h msdos.h $(INTERVALS_H) keymap.h \
   termhooks.h lisp.h $(config_h) coding.h
mktime.o: mktime.c $(config_h)
msdos.o: msdos.c msdos.h dosfns.h systime.h termhooks.h dispextern.h frame.h \
   termopts.h termchar.h character.h coding.h ccl.h disptab.h window.h \
   keyboard.h $(INTERVALS_H) buffer.h commands.h blockinput.h atimer.h \
   lisp.h sysselect.h $(config_h)
nsfns.o: nsfns.m charset.h nsterm.h nsgui.h frame.h window.h buffer.h \
   dispextern.h fontset.h $(INTERVALS_H) keyboard.h blockinput.h \
   atimer.h systime.h epaths.h termhooks.h coding.h systime.h lisp.h $(config_h)
nsfont.o: nsterm.h dispextern.h frame.h lisp.h lisp.h $(config_h)
nsimage.o: nsimage.m nsterm.h lisp.h $(config_h)
nsmenu.o: nsmenu.m termhooks.h frame.h window.h dispextern.h \
   nsgui.h keyboard.h blockinput.h atimer.h systime.h buffer.h \
   nsterm.h lisp.h $(config_h)
nsterm.o: nsterm.m blockinput.h atimer.h systime.h syssignal.h nsterm.h \
   nsgui.h frame.h charset.h ccl.h dispextern.h fontset.h termhooks.h \
   termopts.h termchar.h disptab.h buffer.h window.h keyboard.h \
   $(INTERVALS_H) process.h coding.h lisp.h $(config_h)
nsselect.o: nsselect.m blockinput.h nsterm.h nsgui.h frame.h lisp.h $(config_h)
process.o: process.c process.h buffer.h window.h termhooks.h termopts.h \
   commands.h syssignal.h systime.h systty.h syswait.h frame.h dispextern.h \
   blockinput.h atimer.h charset.h coding.h ccl.h msdos.h composite.h \
   keyboard.h lisp.h $(config_h) character.h xgselect.h sysselect.h
regex.o: regex.c syntax.h buffer.h lisp.h $(config_h) regex.h \
   category.h character.h
region-cache.o: region-cache.c buffer.h region-cache.h lisp.h $(config_h)
scroll.o: scroll.c termchar.h dispextern.h frame.h msdos.h keyboard.h \
   termhooks.h lisp.h $(config_h) systime.h coding.h composite.h window.h
search.o: search.c regex.h commands.h buffer.h region-cache.h syntax.h \
   blockinput.h atimer.h systime.h category.h character.h charset.h \
   $(INTERVALS_H) \
   lisp.h $(config_h)
sound.o: sound.c dispextern.h syssignal.h lisp.h $(config_h) atimer.h systime.h
strftime.o: strftime.c $(config_h)
syntax.o: syntax.c syntax.h buffer.h commands.h category.h character.h \
   keymap.h regex.h $(INTERVALS_H) lisp.h $(config_h)
sysdep.o: sysdep.c syssignal.h systty.h systime.h syswait.h blockinput.h \
   process.h dispextern.h termhooks.h termchar.h termopts.h coding.h \
   frame.h atimer.h window.h msdos.h dosfns.h keyboard.h cm.h lisp.h \
   $(config_h) composite.h sysselect.h
term.o: term.c termchar.h termhooks.h termopts.h lisp.h $(config_h) \
   cm.h frame.h disptab.h keyboard.h character.h charset.h coding.h ccl.h \
   xterm.h msdos.h window.h keymap.h blockinput.h atimer.h systime.h \
   systty.h syssignal.h $(INTERVALS_H) buffer.h
termcap.o: termcap.c lisp.h $(config_h)
terminal.o: terminal.c frame.h termchar.h termhooks.h charset.h coding.h \
   keyboard.h lisp.h $(config_h) dispextern.h composite.h systime.h
terminfo.o: terminfo.c lisp.h $(config_h)
tparam.o: tparam.c lisp.h $(config_h)
undo.o: undo.c buffer.h commands.h window.h dispextern.h lisp.h $(config_h)
unexaix.o: unexaix.c lisp.h $(config_h)
unexalpha.o: unexalpha.c $(config_h)
unexcw.o: unexcw.c lisp.h $(config_h)
unexcoff.o: unexcoff.c lisp.h $(config_h)
unexelf.o: unexelf.c $(config_h)
unexhp9k800.o: unexhp9k800.c $(config_h)
unexmacosx.o: unexmacosx.c $(config_h)
unexsol.o: unexsol.c lisp.h $(config_h)
unexw32.o: unexw32.c $(config_h)
w16select.o: w16select.c dispextern.h frame.h blockinput.h atimer.h systime.h \
   msdos.h buffer.h charset.h coding.h composite.h lisp.h $(config_h)
widget.o: widget.c xterm.h frame.h dispextern.h widgetprv.h \
   $(srcdir)/../lwlib/lwlib.h lisp.h $(config_h)
window.o: window.c indent.h commands.h frame.h window.h buffer.h termchar.h \
   disptab.h keyboard.h msdos.h coding.h termhooks.h \
   keymap.h blockinput.h atimer.h systime.h $(INTERVALS_H) \
   xterm.h w32term.h nsterm.h nsgui.h lisp.h $(config_h)
xdisp.o: xdisp.c macros.h commands.h process.h indent.h buffer.h dispextern.h \
   coding.h termchar.h frame.h window.h disptab.h termhooks.h character.h \
   charset.h lisp.h $(config_h) keyboard.h $(INTERVALS_H) region-cache.h \
   xterm.h w32term.h nsterm.h nsgui.h msdos.h composite.h fontset.h ccl.h \
   blockinput.h atimer.h systime.h keymap.h font.h
xfaces.o: xfaces.c dispextern.h frame.h xterm.h buffer.h blockinput.h	\
   window.h character.h charset.h msdos.h dosfns.h composite.h atimer.h	\
   systime.h keyboard.h fontset.h w32term.h nsterm.h coding.h ccl.h \
   $(INTERVALS_H) nsgui.h termchar.h termhooks.h font.h lisp.h $(config_h)
xfns.o: xfns.c buffer.h frame.h window.h keyboard.h xterm.h dispextern.h \
   $(srcdir)/../lwlib/lwlib.h blockinput.h atimer.h systime.h epaths.h \
   character.h charset.h coding.h gtkutil.h lisp.h $(config_h) termhooks.h \
   fontset.h termchar.h font.h xsettings.h $(INTERVALS_H) ccl.h
xfont.o: dispextern.h xterm.h frame.h blockinput.h character.h charset.h \
   font.h lisp.h $(config_h) atimer.h systime.h fontset.h ccl.h
xftfont.o: dispextern.h xterm.h frame.h blockinput.h character.h charset.h \
   font.h lisp.h $(config_h) atimer.h systime.h fontset.h ccl.h ftfont.h
ftxfont.o: dispextern.h xterm.h frame.h blockinput.h character.h charset.h \
   font.h lisp.h $(config_h) atimer.h systime.h fontset.h ccl.h
menu.o: menu.c lisp.h keyboard.h keymap.h frame.h termhooks.h blockinput.h \
   dispextern.h $(srcdir)/../lwlib/lwlib.h xterm.h gtkutil.h menu.h \
   lisp.h $(config_h) systime.h coding.h composite.h window.h atimer.h nsgui.h
xmenu.o: xmenu.c xterm.h termhooks.h window.h dispextern.h frame.h buffer.h \
   charset.h keyboard.h $(srcdir)/../lwlib/lwlib.h blockinput.h atimer.h \
   systime.h gtkutil.h msdos.h coding.h menu.h lisp.h $(config_h) composite.h \
   keymap.h sysselect.h
xterm.o: xterm.c xterm.h termhooks.h termopts.h termchar.h window.h buffer.h \
   dispextern.h frame.h disptab.h blockinput.h atimer.h systime.h syssignal.h \
   keyboard.h emacs-icon.h character.h charset.h ccl.h fontset.h composite.h \
   coding.h process.h gtkutil.h font.h fontset.h lisp.h $(config_h) \
   xsettings.h intervals.h keymap.h xgselect.h sysselect.h
xselect.o: xselect.c process.h dispextern.h frame.h xterm.h blockinput.h \
   buffer.h atimer.h systime.h termhooks.h lisp.h $(config_h) keyboard.h \
   coding.h composite.h
xgselect.o: xgselect.h systime.h sysselect.h lisp.h $(config_h)
xrdb.o: xrdb.c lisp.h $(config_h) epaths.h
xsmfns.o: xsmfns.c lisp.h $(config_h) systime.h sysselect.h termhooks.h \
   xterm.h lisp.h termopts.h frame.h dispextern.h
xsettings.o: xterm.h xsettings.h lisp.h frame.h termhooks.h $(config_h) \
   dispextern.h keyboard.h systime.h coding.h composite.h blockinput.h \
   atimer.h termopts.h

## The files of Lisp proper.
alloc.o: alloc.c process.h frame.h window.h buffer.h  puresize.h syssignal.h \
   keyboard.h blockinput.h atimer.h systime.h character.h lisp.h $(config_h) \
   $(INTERVALS_H) termhooks.h
bytecode.o: bytecode.c buffer.h syntax.h character.h window.h dispextern.h \
  frame.h xterm.h lisp.h $(config_h)
data.o: data.c buffer.h puresize.h character.h syssignal.h keyboard.h frame.h \
   termhooks.h systime.h coding.h composite.h dispextern.h font.h ccl.h \
   lisp.h $(config_h)
eval.o: eval.c commands.h keyboard.h blockinput.h atimer.h systime.h \
   dispextern.h lisp.h $(config_h) coding.h composite.h xterm.h
floatfns.o: floatfns.c syssignal.h lisp.h $(config_h)
fns.o: fns.c commands.h lisp.h $(config_h) frame.h buffer.h character.h \
   keyboard.h keymap.h window.h dispextern.h $(INTERVALS_H) coding.h md5.h \
   blockinput.h atimer.h systime.h xterm.h
print.o: print.c process.h frame.h window.h buffer.h keyboard.h character.h \
   lisp.h $(config_h) termchar.h $(INTERVALS_H) msdos.h termhooks.h \
   blockinput.h atimer.h systime.h font.h charset.h coding.h ccl.h
lread.o: lread.c commands.h keyboard.h buffer.h epaths.h character.h \
   charset.h lisp.h $(config_h) $(INTERVALS_H) termhooks.h coding.h msdos.h \
   systime.h frame.h blockinput.h atimer.h

## Text properties support.
composite.o: composite.c buffer.h character.h coding.h font.h ccl.h \
   frame.h termhooks.h $(INTERVALS_H) window.h lisp.h $(config_h)
intervals.o: intervals.c buffer.h $(INTERVALS_H) keyboard.h puresize.h \
   keymap.h lisp.h $(config_h) systime.h coding.h
textprop.o: textprop.c buffer.h window.h $(INTERVALS_H) \
   lisp.h $(config_h)


### deps.mk ends here
