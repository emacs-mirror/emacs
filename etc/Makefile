DESTDIR=
LIBDIR=/usr/local/lib
BINDIR=/usr/local/bin
MANDIR=/usr/man/man1
MANEXT=1

all:

mostlyclean:
	-rm -f core

clean distclean maintainer-clean:
	-rm -f DOC* core

SOURCES = [0-9A-QS-Z]* README *.[ch16] emacs.* etags.* ledit.l ms-* \
	news.texi rc2log refcard.tex spook-lines termcap.* ulimit.hack \
	vcdiff vipcard.tex xmouse.doc

unlock:
	chmod u+w $(SOURCES)

relock:
	chmod u-w $(SOURCES)

# ${etcdir}/e/eterm is used by ../lisp/term.el.
# TERMINFO systems use terminfo files compiled by the Terminfo Compiler (tic).
# These files are binary, and depend on the version of tic, but they seem
# to be system-independent and backwardly compatible.
# So there should be no need to recompile the distributed binary version.
TIC=tic
e/eterm: e/eterm.ti
	TERMINFO=`pwd`; export TERMINFO; $(TIC) e/eterm.ti

