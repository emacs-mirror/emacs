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

