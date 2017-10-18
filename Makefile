# make all	to compile and build Emacs
# make install	to install it
# make install.sysv   to install on system V.
# make install.xenix  to install on Xenix
# make install.aix    to install on AIX.
# make tags	to update tags tables
#
# make distclean	to delete everything that wasn't in the distribution
#	This is a very dangerous thing to do!
# make clean
#       This is a little less dangerous.

SHELL = /bin/sh

# Where to install things
# Note that on system V you must change MANDIR to /use/local/man/man1.
# This got changed in late 1991 to say /usr/local/lib/emacs,
# but there was no explanation of why, so it seems better to keep this stable.
LIBDIR= /usr/local/emacs
BINDIR= /usr/local/bin
MANDIR= /usr/local/man/man1

# Flags passed down to subdirectory makefiles.
MFLAGS=

# Command used for installation.
# If `install' doesn't work on your system, try `./install.sh'.
INSTALL=install

# Subdirectories to make recursively.  `lisp' is not included
# because the compiled lisp files are part of the distribution
# and you cannot remake them without installing Emacs first.
SUBDIR= etc src

# Subdirectories to install
COPYDIR= etc info lisp

# Subdirectories to clean
CLEANDIR= ${COPYDIR} lisp/term oldXMenu

all:	src/paths.h ${SUBDIR}

src/paths.h: Makefile src/paths.h-dist
	sed 's;/usr/local/emacs;${LIBDIR};g' < src/paths.h-dist > src/paths.h

src:	etc

.RECURSIVE: ${SUBDIR}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS} all

install: all mkdir lockdir
# B option to tar xf removed because some systems don't have it.
# It should work without that as long as the same tar program
# is running on both sides of the pipe.
	-if [ `/bin/pwd` != `(cd ${LIBDIR}; /bin/pwd)` ] ; then \
		tar cf - ${COPYDIR} | (cd ${LIBDIR}; umask 0; tar xf - ) ;\
		for i in ${CLEANDIR}; do \
			(rm -rf ${LIBDIR}/$$i/RCS; \
			 rm -f ${LIBDIR}/$$i/\#*; \
			 rm -f ${LIBDIR}/$$i/*~); \
		done \
	else true; \
	fi
	$(INSTALL) -c etc/emacsclient ${BINDIR}/emacsclient
	$(INSTALL) -c etc/etags ${BINDIR}/etags
	$(INSTALL) -c etc/ctags ${BINDIR}/ctags
	$(INSTALL) -c -m 1755 src/xemacs ${BINDIR}/xemacs
	$(INSTALL) -c -m 444 etc/emacs.1 ${MANDIR}/emacs.1
	-rm -f ${BINDIR}/emacs
	mv ${BINDIR}/xemacs ${BINDIR}/emacs

install.sysv: all mkdir lockdir
	-if [ `/bin/pwd` != `(cd ${LIBDIR}; /bin/pwd)` ] ; then \
		find ${COPYDIR} -print | cpio -pdum ${LIBDIR} ;\
		for i in ${CLEANDIR}; do \
			(rm -rf ${LIBDIR}/$$i/RCS; \
			 rm -f ${LIBDIR}/$$i/\#*; \
			 rm -f ${LIBDIR}/$$i/*~); \
		done \
	else true; \
	fi
	-cpset etc/emacsclient ${BINDIR}/emacsclient 755 bin bin
	-cpset etc/etags ${BINDIR}/etags 755 bin bin
	-cpset etc/ctags ${BINDIR}/ctags 755 bin bin
	-cpset etc/emacs.1 ${MANDIR}/emacs.1 444 bin bin
	-/bin/rm -f ${BINDIR}/emacs
	-cpset src/xemacs ${BINDIR}/emacs 1755 bin bin

install.xenix: all mkdir lockdir
	if [ `pwd` != `(cd ${LIBDIR}; pwd)` ] ; then \
		tar cf - ${COPYDIR} | (cd ${LIBDIR}; umask 0; tar xpf - ) ;\
		for i in ${CLEANDIR}; do \
			(rm -rf ${LIBDIR}/$$i/RCS; \
			 rm -f ${LIBDIR}/$$i/\#*; \
			 rm -f ${LIBDIR}/$$i/*~); \
		done \
	else true; \
	fi
	cp etc/etags etc/ctags etc/emacsclient ${BINDIR}
	chmod 755 ${BINDIR}/etags ${BINDIR}/ctags ${BINDIR}/emacsclient
	cp etc/emacs.1 ${MANDIR}/emacs.1
	chmod 444 ${MANDIR}/emacs.1
	-mv -f ${BINDIR}/emacs ${BINDIR}/emacs.old
	cp src/xemacs ${BINDIR}/emacs
	chmod 1755 ${BINDIR}/emacs
	-rm -f ${BINDIR}/emacs.old

install.aix: all mkdir lockdir
	-if [ `/bin/pwd` != `(cd ${LIBDIR}; /bin/pwd)` ] ; then \
		tar cf - ${COPYDIR} | (cd ${LIBDIR}; umask 0; tar xBf - ) ;\
		for i in ${CLEANDIR}; do \
			(rm -rf ${LIBDIR}/$$i/RCS; \
			 rm -f ${LIBDIR}/$$i/\#*; \
			 rm -f ${LIBDIR}/$$i/*~); \
		done \
	else true; \
	fi
	install -f ${BINDIR} etc/emacsclient
	install -f ${BINDIR} etc/etags
	install -f ${BINDIR} etc/ctags
	install -M 1755 -f ${BINDIR} src/xemacs
	install -M 444 -f ${MANDIR} etc/emacs.1
	-rm -f ${BINDIR}/emacs
	mv ${BINDIR}/xemacs ${BINDIR}/emacs

mkdir: FRC
	-mkdir ${LIBDIR} ${BINDIR} ${MANDIR}

distclean:
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} distclean); done
	cd oldXMenu; make ${MFLAGS} distclean

clean:
	cd src; make clean
	cd oldXMenu; make ${MFLAGS} clean
	if [ `/bin/pwd` != `(cd ${LIBDIR}; /bin/pwd)` ] ; then \
		cd etc; make clean; \
	else true; \
	fi

lockdir:
	-mkdir ${LIBDIR}/lock
	-chmod 777 ${LIBDIR}/lock

FRC:

tags:	etc
	cd src; ../etc/etags *.[ch] ../lisp/*.el ../lisp/term/*.el
