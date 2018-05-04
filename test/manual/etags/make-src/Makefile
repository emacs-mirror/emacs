LATEST=17
RELEASELIST=pot@gnu.org xemacs-review@xemacs.org jcwren@jcwren.com sxemacs-devel@sxemacs.org

ADASRC=etags-test-for.ada 2ataspri.adb 2ataspri.ads waroquiers.ada
ASRC=empty.zz empty.zz.gz
CSRC=abbrev.c ../etags/h.h .//c.c torture.c getopt.h etags.c\
 exit.c exit.strange_suffix sysdep.h tab.c\
 emacs/src/gmalloc.c emacs/src/regex.h emacs/src/keyboard.c dostorture.c\
 machsyscalls.c machsyscalls.h fail.c a/b/b.c
CPSRC=c.C abstract.C abstract.H cfront.H burton.cpp burton.cpp\
 functions.cpp MDiagArray2.h Pctest.h Range.h\
 screen.cpp screen.hpp conway.cpp conway.hpp clheir.cpp.gz clheir.hpp.gz fail.C
ELSRC=TAGTEST.EL emacs/lisp/progmodes/etags.el
ERLSRC=gs_dialog.erl lines.erl lists.erl
FORTHSRC=test-forth.fth
FSRC=entry.for entry.strange_suffix entry.strange
HTMLSRC=softwarelibero.html index.shtml algrthms.html software.html
JAVASRC=AWTEMul.java KeyEve.java SMan.java SysCol.java TG.java
LUASRC=allegro.lua
MAKESRC=Makefile
OBJCSRC=Subprocess.h Subprocess.m PackInsp.h PackInsp.m
OBJCPPSRC=SimpleCalc.H SimpleCalc.M
PASSRC=common.pas
PERLSRC=htlmify-cystic yagrip.pl kai-test.pl mirror.pl
PHPSRC=lce_functions.php ptest.php sendmail.php
PSSRC=rfc1245.ps
PROLSRC=ordsets.prolog natded.prolog
PYTSRC=server.py
TEXSRC=testenv.tex gzip.texi texinfo.tex nonewline.tex
YSRC=parse.y parse.c atest.y cccp.c cccp.y
SRCS=Makefile ${ADASRC} ${ASRC} ${CSRC} ${CPSRC} ${ELSRC} ${ERLSRC} ${FSRC}\
     ${FORTHSRC} ${HTMLSRC} ${JAVASRC} ${LUASRC} ${MAKESRC} ${OBJCSRC}\
     ${OBJCPPSRC} ${PASSRC} ${PHPSRC} ${PERLSRC} ${PSSRC} ${PROLSRC} ${PYTSRC}\
     ${TEXSRC} ${YSRC}
NONSRCS=entry.strange lists.erl clheir.hpp.gz

VHDLFLAGS=--language=none --regex='/[ \t]*\(ARCHITECTURE\|CONFIGURATION\) +[^ ]* +OF/' --regex='/[ \t]*\(ATTRIBUTE\|ENTITY\|FUNCTION\|PACKAGE\( BODY\)?\|PROCEDURE\|PROCESS\|TYPE\)[ \t]+\([^ \t(]+\)/\3/'
COBOLFLAGS=--language=none --regex='/.......[a-zA-Z0-9-]+\./'
POSTSCRIPTFLAGS=--language=none --regex='#/[^ \t{]+#'
TCLFLAGS=--lang=none --regex='/proc[ \t]+\([^ \t]+\)/\1/'

GETOPTOBJS= #getopt.o getopt1.o
RXINCLUDE=-Iemacs/src
REGEXOBJS=regex.o

CHECKOBJS=chkmalloc.o chkxm.o
CHECKFLAGS=-DDEBUG -Wno-unused-function
OBJS=${GETOPTOBJS} ${REGEXOBJS} ${CHECKOBJS}
CPPFLAGS=${CHECKFLAGS} -DSTDC_HEADERS -DHAVE_GETCWD ${RXINCLUDE} -I.
LDFLAGS=#-static -lc_p
WARNINGS=-pedantic -Wall -Wpointer-arith -Winline -Wmissing-prototypes -Wmissing-declarations -Wunused -Wformat -Wno-switch -Wsign-compare -Wpointer-arith -Wshadow -Wstrict-prototypes
CFLAGS=${WARNINGS} -ansi -g3 # -pg -O
#CC=gcc-3.0
#TARGET_ARCH=
FASTCFLAGS=-O3 -finline-functions -ffast-math -funroll-loops
FASTCFLAGSWARN=${WARNINGS} -Werror ${FASTCFLAGS}

FILTER=grep -v '\.[Cchefy][lor]*,[1-9][0-9]*' || true
REGEX=/[ \t]*DEFVAR_[A-Z_ \t\n(]+"\([^"]+\)"/
xx="this line is here because of a fontlock bug

MAKE:=$(MAKE) --no-print-directory
RUN=time --quiet --format '%U + %S: %E'
RUN=
OPTIONS=--members --declarations --regex=@regexfile
ARGS=- < srclist

infiles = $(filter-out ${NONSRCS},${SRCS}) srclist regexfile

quiettest:
	@rm -f /tmp/[0-9][0-9][0-9][0-9][0-9][0-9]malloc
	@-$(MAKE) OPTIONS='--no-members' ${LATEST}ediff
	@-$(MAKE) OPTIONS='--declarations --no-members' ${LATEST}ediff
	@-$(MAKE) OPTIONS='--members' ${LATEST}ediff
	@-$(MAKE) OPTIONS='--regex=@regexfile --no-members' ${LATEST}ediff
	@-$(MAKE) OPTIONS='nonexistent --members --declarations --regex=@regexfile' ${LATEST}ediff
	@-$(MAKE) ${LATEST}cdiff

test:
	@rm -f /tmp/[0-9][0-9][0-9][0-9][0-9][0-9]malloc
	@$(MAKE) OPTIONS='--no-members' ${LATEST}ediff
	@$(MAKE) OPTIONS='--declarations --no-members' ${LATEST}ediff
	@$(MAKE) OPTIONS='--members' ${LATEST}ediff
	@$(MAKE) OPTIONS='--regex=@regexfile --no-members' ${LATEST}ediff
	@$(MAKE) OPTIONS='nonexistent --members --declarations --regex=@regexfile' ${LATEST}ediff
	@$(MAKE) ${LATEST}cdiff

${CHECKOBJS}: CFLAGS=-g3 -DNULLFREECHECK=0

checker:
	@rm -f /tmp/[0-9][0-9][0-9][0-9][0-9][0-9]malloc ${REGEXOBJS}
	@env CHECKEROPTS="--trace --profile --Wfree-null" \
	$(MAKE) CFLAGS= CHECKOBJS= CHECKFLAGS= CC=checkergcc ${LATEST}ediff
	rm -f $REGEXOBJS

standalone:
	rm -f etags ctags
	@$(MAKE) etags "CPPFLAGS=-UVERSION"
	@$(MAKE) ctags "CPPFLAGS=-UVERSION"

prof: ETAGS
	prof -xgs etags

fastetags:
	rm -f etags ${GETOPTOBJS} ${REGEXOBJS}
	@$(MAKE) CHECKOBJS= CHECKFLAGS= etags "CFLAGS=-ansi ${FASTCFLAGSWARN}"

fastctags:
	rm -f ctags ${GETOPTOBJS} ${REGEXOBJS}
	@$(MAKE) CHECKOBJS= CHECKFLAGS= ctags "CFLAGS=-ansi ${FASTCFLAGSWARN}"

staticetags:
	rm -f etags ${GETOPTOBJS} ${REGEXOBJS}
	@$(MAKE) etags CHECKOBJS= CHECKFLAGS= REGEXOBJS= GETOPTOBJS= RXINCLUDE= "CFLAGS=${FASTCFLAGSWARN} -static"

rsynctofly:
	rsync --exclude "*~" --exclude core --exclude etags -zauRv . fly:gnu/etags/

rsyncfromfly:
	rsync --exclude "*~" --exclude core --exclude etags -zauRv fly:gnu/etags/ ../..

web ftp publish:
	@-printf '\ttesting with debugging enabled...\n'; $(MAKE) quiettest
	@-printf '\ttesting standalone...\n'; $(MAKE) standalone quiettest
	@-printf '\ttesting fast versions...\n'; $(MAKE) fastetags fastctags quiettest
	@$(MAKE) /home/www/pub/etags.c.gz
	@$(MAKE) /home/www/pub/software/unix/etags.tar.gz

release distrib: web
	cat xemacs-mail | /usr/sbin/sendmail -f pot@gnu.org ${RELEASELIST}
	mv etags etags${LATEST}
	mv ctags ctags${LATEST}

tags: TAGS

clean:
	rm -f ${OBJS} etags ETAGS

srclist: Makefile
	@for i in $(SRCS); do echo $$i; done > srclist
	@echo srclist remade

regexfile: Makefile
	@echo '	-- This is for GNU Emacs source files' > regexfile
	@echo '${REGEX}' >> regexfile
	@echo '{c}${REGEX}\\1/m' >> regexfile
	@echo regexfile remade

/home/www/pub/etags.c.gz: etags.c
	co -kv etags.c
	gzip --best -c etags.c > $@

#/home/www/pub/software/unix/etags.tar.gz: Makefile staticetags etags.1.man ETAGS.EBNF ETAGS.README
#	tar -hzcf $@ COPYING ChangeLog ETAGS.EBNF etags.c etags.1 etags.1.man etags

/home/www/pub/software/unix/etags.tar.gz: Makefile etags.1.man ETAGS.EBNF ETAGS.README maintaining.texi
	tar -hzcf $@ COPYING ChangeLog ETAGS.EBNF ETAGS.README etags.c etags.1 etags.1.man maintaining.texi

regex.o: emacs/src/regex.c
	$(CC) ${FASTCFLAGS} -c $?

getopt.o: emacs/lib-src/getopt.c
	$(CC) ${FASTCFLAGS} -c $?

getopt1.o: emacs/lib-src/getopt1.c
	$(CC) ${FASTCFLAGS} -c $?

etags: etags.c ${OBJS}
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o etags etags.c ${OBJS}

ctags: etags.c ${OBJS}
	$(CC) $(CFLAGS) $(CPPFLAGS) -DCTAGS $(LDFLAGS) -o ctags etags.c ${OBJS}

man manpage: etags.1.man

etags.1.man: etags.1
	nroff -man -Tman etags.1 > $@

maintaining.info: maintaining.texi
	makeinfo --force --no-split maintaining.texi

TAGS: etags.c
	etags etags.c

%ediff: ETAGS% ETAGS ${infiles}
	sdiff --suppress-common-lines --width=103 ETAGS$* ETAGS

oediff: OTAGS ETAGS ${infiles}
	sdiff --suppress-common-lines --width=103 OTAGS ETAGS

%cdiff: CTAGS% CTAGS ${infiles}
	sdiff --suppress-common-lines --width=103 CTAGS$* CTAGS

xdiff: ETAGS EXTAGS ${infiles}
	sdiff --suppress-common-lines --width=103 ETAGS EXTAGS

ETAGS: FRC etags ${infiles}
	${RUN} ./etags ${OPTIONS} -o $@ ${ARGS}

ETAGS%: FRC etags% ${infiles}
	${RUN} etags$* ${OPTIONS} -o $@ ${ARGS}

ETAGS13 ETAGS14 ETAGS15: etags% ${infiles}
	TEXTAGS=def:newcommand:newenvironment ${RUN} etags$* --regex=@regexfile} -o $@ ${ARGS}

ETAGS12: etags12 ${infiles}
	${RUN} etags12 --members -o $@ --regex='${REGEX}' ${ARGS}

OTAGS: oetags ${SRCS} srclist
	${RUN} ./oetags -o $@ -t ${ARGS}

CTAGS: ctags ${infiles}
	${RUN} ./ctags -o $@ --regex=@regexfile ${ARGS}

CTAGS%: ctags% ${infiles}
	${RUN} ctags$* -wtTd --globals --members -o $@ --regex=@regexfile ${ARGS}

CTAGS13 CTAGS14 CTAGS15: ctags% ${infiles}
	TEXTAGS=def:newcommand:newenvironment ${RUN} ctags$* -wt -o $@ --regex='${REGEX}' ${ARGS}

EXTAGS: extags ${infiles} Makefile
	${RUN} ./extags -e --regex-c='${REGEX}' --c++-types=+x --c-types=+x --if0=yes --line-directives=yes -o $@ -L - < srclist

.PRECIOUS: ETAGS CTAGS ETAGS16 CTAGS16 ETAGS17 CTAGS17

FRC:;
