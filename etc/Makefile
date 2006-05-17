DESTDIR=
LIBDIR=/usr/local/lib
BINDIR=/usr/local/bin
MANDIR=/usr/man/man1
MANEXT=1

all:

mostlyclean:
	-rm -f core *.dvi *.log

clean distclean maintainer-clean:
	-rm -f DOC* core *.dvi *.log

SOURCES = [0-9A-QS-Z]* README *.[ch16] emacs.* etags.* ledit.l ms-* \
	spook-lines tasks.texi termcap.* ulimit.hack *.tex

unlock:
	chmod u+w $(SOURCES)

relock:
	chmod u-w $(SOURCES)

# ${etcdir}/e/eterm-color is used by ../lisp/term.el.
# TERMINFO systems use terminfo files compiled by the Terminfo Compiler (tic).
# These files are binary, and depend on the version of tic, but they seem
# to be system-independent and backwardly compatible.
# So there should be no need to recompile the distributed binary version.
TIC=tic
e/eterm-color: e/eterm-color.ti
	TERMINFO=`pwd`; export TERMINFO; $(TIC) e/eterm-color.ti

# arch-tag: 4261f003-cf77-4478-a10a-5284e9d8f797
