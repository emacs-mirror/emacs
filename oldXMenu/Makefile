# Uncomment following line if linking temacs complains about missing insque.
# EXTRA=insque.o

             AS = as
#            CC = cc
            CPP = /lib/cpp
             LD = ld
           TAGS = etags
             RM = rm -f
             MV = mv
             LN = ln -s
         RANLIB = ranlib
             AR = ar clq
             LS = ls
       LINTOPTS = -axz
    LINTLIBFLAG = -C
           MAKE = make
    STD_DEFINES =
    CDEBUGFLAGS = -O
         RM_CMD = $(RM) *.CKP *.ln *.BAK *.bak *.o core errs ,* *~ *.a \
			tags TAGS make.log

SRCS =  Activate.c \
	AddPane.c \
	AddSel.c \
	ChgPane.c \
	ChgSel.c \
	Create.c \
	DelPane.c \
	DelSel.c \
	Destroy.c \
	Error.c \
	EvHand.c \
	FindPane.c \
	FindSel.c \
	InsPane.c \
	InsSel.c \
	Internal.c \
	Locate.c \
	Post.c \
	Recomp.c \
	SetAEQ.c \
	SetFrz.c \
	SetPane.c \
	SetSel.c \
      XDelAssoc.c XLookAssoc.c XCrAssoc.c XDestAssoc.c XMakeAssoc.c

OBJS =  Activate.o \
	AddPane.o \
	AddSel.o \
	ChgPane.o \
	ChgSel.o \
	Create.o \
	DelPane.o \
	DelSel.o \
	Destroy.o \
	Error.o \
	EvHand.o \
	FindPane.o \
	FindSel.o \
	InsPane.o \
	InsSel.o \
	Internal.o \
	Locate.o \
	Post.o \
	Recomp.o \
	SetAEQ.o \
	SetFrz.o \
	SetPane.o \
	SetSel.o \
      XDelAssoc.o XLookAssoc.o XCrAssoc.o XDestAssoc.o XMakeAssoc.o

.c.o:
	$(RM) $@
	$(CC) -c $(CFLAGS) $*.c

all:: libXMenu11.a

libXMenu11.a: $(OBJS) $(EXTRA)
	$(RM) $@
	$(AR) $@ $(OBJS) $(EXTRA)
	-$(RANLIB) $@
#If running ranlib fails, probably there is none.
#That's ok.  So don't stop the build.

distclean: clean
clean::
	$(RM_CMD) \#* libXMenu11.a *.o
tags::
	$(TAGS) -t *.[ch]
