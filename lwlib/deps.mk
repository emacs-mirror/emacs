### deps.mk --- lwlib/Makefile fragment for GNU Emacs

# Copyright (C) 1992, 1993 Lucid, Inc.
# Copyright (C) 1994, 2001-2020 Free Software Foundation, Inc.
#
# This file is part of the Lucid Widget Library.
#
# The Lucid Widget Library is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 1, or (at your option)
# any later version.
#
# The Lucid Widget Library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

### Commentary:

## This file is included in lwlib/Makefile if AUTO_DEPEND=no.
## It defines static dependencies between the various source files.

### Code:

## Generated files in ../src, non-generated in $(srcdir)/../src.
config_h = ../src/config.h $(srcdir)/../src/conf_post.h
lisp_h = $(srcdir)/../src/lisp.h
## lisp.h includes this.
src_h = $(config_h) $(lisp_h) $(globals_h)

lwlib-utils.o: $(src_h) lwlib-utils.c lwlib-utils.h lwlib.h
lwlib.o:       $(src_h) lwlib.c lwlib.h lwlib-int.h lwlib-utils.h \
  lwlib-Xlw.h lwlib-Xm.h lwlib-Xaw.h
lwlib-Xlw.o:   $(src_h) lwlib-Xlw.c lwlib.h lwlib-int.h lwlib-Xlw.h xlwmenu.h
lwlib-Xaw.o:   $(src_h) lwlib-Xaw.c lwlib-Xaw.h lwlib.h lwlib-int.h
lwlib-Xm.o:    $(src_h) lwlib-Xm.c lwlib-Xm.h lwlib.h lwlib-int.h lwlib-utils.h
xlwmenu.o:     $(src_h) xlwmenu.c xlwmenu.h lwlib.h xlwmenuP.h \
  $(srcdir)/../src/xterm.h

### deps.mk ends here
