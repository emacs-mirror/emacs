ADASRC=$(addprefix ./ada-src/,etags-test-for.ada 2ataspri.adb 2ataspri.ads waroquiers.ada)
ASRC=$(addprefix ./a-src/,empty.zz empty.zz.gz)
CSRC=$(addprefix ./c-src/,abbrev.c .//c.c torture.c getopt.h etags.c\
 exit.c exit.strange_suffix sysdep.h tab.c dostorture.c\
 emacs/src/gmalloc.c emacs/src/regex.h emacs/src/keyboard.c emacs/src/lisp.h\
 machsyscalls.c machsyscalls.h fail.c a/b/b.c) ../etags/c-src/h.h
CPSRC=$(addprefix ./cp-src/,c.C x.cc burton.cpp burton.cpp\
 functions.cpp MDiagArray2.h Range.h\
 screen.cpp screen.hpp conway.cpp conway.hpp clheir.cpp.gz clheir.hpp.gz fail.C)
ELSRC=$(addprefix ./el-src/,TAGTEST.EL emacs/lisp/progmodes/etags.el)
ERLSRC=$(addprefix ./erl-src/,gs_dialog.erl)
FORTHSRC=$(addprefix ./forth-src/,test-forth.fth)
FSRC=$(addprefix ./f-src/,entry.for entry.strange_suffix entry.strange)
GOSRC=$(addprefix ./go-src/,test.go test1.go)
HTMLSRC=$(addprefix ./html-src/,softwarelibero.html index.shtml algrthms.html software.html)
#JAVASRC=$(addprefix ./java-src/, )
LUASRC=$(addprefix ./lua-src/,allegro.lua test.lua)
MAKESRC=$(addprefix ./make-src/,Makefile)
MERCSRC=$(addprefix ./merc-src/,accumulator.m)
OBJCSRC=$(addprefix ./objc-src/,Subprocess.h Subprocess.m PackInsp.h PackInsp.m)
OBJCPPSRC=$(addprefix ./objcpp-src/,SimpleCalc.H SimpleCalc.M)
PASSRC=$(addprefix ./pas-src/,common.pas)
PERLSRC=$(addprefix ./perl-src/,htlmify-cystic yagrip.pl kai-test.pl)
PHPSRC=$(addprefix ./php-src/,lce_functions.php ptest.php sendmail.php)
PSSRC=$(addprefix ./ps-src/,rfc1245.ps)
PROLSRC=$(addprefix ./prol-src/,ordsets.prolog natded.prolog)
PYTSRC=$(addprefix ./pyt-src/,server.py)
RBSRC=$(addprefix ./ruby-src/,test.rb test1.ru)
RSSRC=$(addprefix ./rs-src/,test.rs)
SCMSRC=$(addprefix ./scm-src/,test.scm)
TEXSRC=$(addprefix ./tex-src/,testenv.tex gzip.texi texinfo.tex nonewline.tex)
YSRC=$(addprefix ./y-src/,parse.y parse.c atest.y cccp.c cccp.y)
SRCS=${ADASRC} ${ASRC} ${CSRC} ${CPSRC} ${ELSRC} ${ERLSRC} ${FSRC}\
     ${FORTHSRC} ${GOSRC} ${HTMLSRC} ${JAVASRC} ${LUASRC} ${MAKESRC}\
     ${OBJCSRC} ${OBJCPPSRC} ${PASSRC} ${PHPSRC} ${PERLSRC} ${PSSRC}\
     ${PROLSRC} ${PYTSRC} ${RBSRC} ${RSSRC} ${SCMSRC} ${TEXSRC} ${YSRC} ${MERCSRC}
NONSRCS=./f-src/entry.strange ./erl-src/lists.erl ./cp-src/clheir.hpp.gz

ETAGS_PROG=../../../lib-src/etags
CTAGS_PROG=../../../lib-src/etags --ctags

REGEX=/[ \t]*DEFVAR_[A-Z_ \t\n(]+"\([^"]+\)"/
xx="this line is here because of a fontlock bug

RUN=

OPTIONS=--members --declarations --regex=@regexfile
ARGS=- < srclist

infiles = $(filter-out ${NONSRCS},${SRCS}) srclist regexfile

.PHONY: check cdiff ETAGS CTAGS
# Can't make ediff_1 through ediff_5 .PHONY, as they're implicit.

check:
	@$(MAKE) OPTIONS='--no-members' ediff_1
	@$(MAKE) OPTIONS='--declarations --no-members' ediff_2
	@$(MAKE) OPTIONS='--members' ediff_3
	@$(MAKE) OPTIONS='--regex=@regexfile --no-members' ediff_4
	@$(MAKE) OPTIONS='nonexistent --members --declarations --regex=@regexfile' ediff_5
	@$(MAKE) OPTIONS='--class-qualify --members --declarations --regex=@regexfile' ediff_6
	@$(MAKE) OPTIONS='--no-members --no-fallback --no-empty' ediff_7
	@$(MAKE) cdiff
	@$(MAKE) ctags_update

ediff%: ETAGS.good% ETAGS ${infiles}
	diff -u --suppress-common-lines --width=80 ETAGS.good$* ETAGS

cdiff: CTAGS.good CTAGS ${infiles}
	diff -u --suppress-common-lines --width=80 CTAGS.good CTAGS

ctags_update: CTAGS.good_update ${infiles}
	head -n 100 CTAGS.good_update > CTAGS
	tail -n 100 CTAGS.good_update >> CTAGS
	${RUN} ${CTAGS_PROG} -o CTAGS -u ${ARGS}
	sort CTAGS > CTAGS.sorted
	sort CTAGS.good_update > CTAGS.good_update.sorted
	diff -u --suppress-common-lines --width=80 CTAGS.good_update.sorted CTAGS.sorted

	cp crlf CTAGS
	${RUN} ${CTAGS_PROG} -o CTAGS -u ${ARGS}
	sort CTAGS > CTAGS.sorted
	sort CTAGS.good_crlf > CTAGS.good_crlf.sorted
	diff -u --suppress-common-lines --width=80 CTAGS.good_crlf.sorted CTAGS.sorted

ETAGS: ${infiles}
	${RUN} ${ETAGS_PROG} ${OPTIONS} -o $@ ${ARGS}

CTAGS: ${infiles}
	${RUN} ${CTAGS_PROG} -o $@ --regex=@regexfile ${ARGS}

srclist: Makefile
	@for i in $(SRCS); do echo $$i; done > srclist
	@echo "srclist remade"

regexfile: Makefile
	@echo '	-- This is for GNU Emacs source files' > regexfile
	@printf '%s\n' '${REGEX}' >> regexfile
	@printf '%s\n' '{c}${REGEX}\\1/m' >> regexfile
	@echo "regexfile remade"
