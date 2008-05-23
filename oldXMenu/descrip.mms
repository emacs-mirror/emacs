### descrip.mms - port of oldXMenu Makefile to VMS

## Copyright (C) 1993, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
##   2008  Free Software Foundation, Inc.

## Author: Richard Levitte

## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.


### Code:

!# Uncomment following line if linking temacs complains about missing insque.
EXTRA=insque.obj

AS = as
CC = cc
LD = link
TAGS = etags
RM = delete
MV = rename
AR = library/insert
MAKE = mms
STD_DEFINES =
CDEBUGFLAGS = /debug/noopt
RM_CMD = $(RM) *.BAK.*, *.obj.*

SRCS =  Activate.c, -
	AddPane.c, -
	AddSel.c, -
	ChgPane.c, -
	ChgSel.c, -
	Create.c, -
	DelPane.c, -
	DelSel.c, -
	Destroy.c, -
	Error.c, -
	EvHand.c, -
	FindPane.c, -
	FindSel.c, -
	InsPane.c, -
	InsSel.c, -
	Internal.c, -
	Locate.c, -
	Post.c, -
	Recomp.c, -
	SetAEQ.c, -
	SetFrz.c, -
	SetPane.c, -
	SetSel.c, -
        XDelAssoc.c, XLookAssoc.c, XCrAssoc.c, XDestAssoc.c, XMakeAssoc.c

OBJS =  Activate.obj, -
	AddPane.obj, -
	AddSel.obj, -
	ChgPane.obj, -
	ChgSel.obj, -
	Create.obj, -
	DelPane.obj, -
	DelSel.obj, -
	Destroy.obj, -
	Error.obj, -
	EvHand.obj, -
	FindPane.obj, -
	FindSel.obj, -
	InsPane.obj, -
	InsSel.obj, -
	Internal.obj, -
	Locate.obj, -
	Post.obj, -
	Recomp.obj, -
	SetAEQ.obj, -
	SetFrz.obj, -
	SetPane.obj, -
	SetSel.obj, -
        XDelAssoc.obj, XLookAssoc.obj, XCrAssoc.obj, XDestAssoc.obj, -
        XMakeAssoc.obj

.c.obj :
	if f$search("$@") then $(RM) $@.*
	$(CC) /obj=$@ $(CFLAGS) $*.c

all :: libXMenu11.olb
	!

libXMenu11.olb : $(OBJS) $(EXTRA)
	if f$search("$@") then $(RM) $@.*
	$(AR)/create $@ $(OBJS)
	if ("$(EXTRA)" .nes. "") then $(AR) $@ $(EXTRA)
#If running ranlib fails, probably there is none.
#That's ok.  So don't stop the build.

distclean : clean
	!

clean ::
	$(RM_CMD) \#* libXMenu11.a *.obj,
tags ::
	$(TAGS) -t *.[ch]

