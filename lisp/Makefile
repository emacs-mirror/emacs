#
# Maintenance productions for the Lisp directory
#

# You can specify a different executable on the make command line,
# e.g. "make EMACS=../src/emacs ...".

EMACS = emacs

# Command line flags for Emacs.  This must include --multibyte,
# otherwise some files will not compile.

EMACSOPT = --no-init-file --no-site-file --multibyte -batch

SOURCES = *.el COPYING Makefile
lisptagsfiles1 = [a-zA-Z]*.el
lisptagsfiles2 = [a-zA-Z]*/[a-zA-Z]*.el
ETAGS = ../lib-src/etags

# Files which should not be compiled.  All file names must be relative
# to the `lisp' directory.
# - emacs-lisp/cl-specs.el:  only contains `def-edebug-spec's so there's
#   no point compiling it, although it doesn't hurt.

DONTCOMPILE = bindings.el mail/blessmail.el play/bruce.el cus-load.el \
	cus-start.el forms-d2.el forms-pass.el \
	international/latin-1.el international/latin-2.el \
	international/latin-3.el international/latin-4.el \
	international/latin-5.el \
	loaddefs.el loadup.el international/mule-conf.el patcomp.el \
	paths.el mail/sc.el subdirs.el term-nasty.el version.el \
	generic-x.el international/latin-8.el international/latin-9.el \
	emacs-lisp/cl-specs.el finder-inf.el

# The actual Emacs command run in the targets below.

emacs = $(EMACS) $(EMACSOPT)

# Common command to find subdirectories

setwins=subdirs=`find $$wd -type d -print`; \
	for file in $$subdirs; do \
	   case $$file in */Old | */RCS | */CVS | */CVS/* | */=* ) ;; \
		*) wins="$$wins $$file" ;; \
	   esac; \
        done

doit:

cus-load.el:
	touch $@
custom-deps: cus-load.el doit
	wd=.; $(setwins); \
	echo Directories: $$wins; \
	$(emacs) -l cus-dep -f custom-make-dependencies $$wins

finder-inf.el:
	echo "(provide 'finder-inf)" >> $@
finder-data: finder-inf.el doit
	wd=.; $(setwins); \
	echo Directories: $$wins; \
	$(emacs) -l finder -f finder-compile-keywords-make-dist $$wins

loaddefs.el:
	echo ";;; loaddefs.el --- automatically extracted autoloads" >> $@
	echo ";;" >> $@; echo ";;; Code:" >> $@
	echo "" >> $@
	echo ";;; Local Variables:" >> $@
	echo ";;; version-control: never" >> $@
	echo ";;; no-byte-compile: t" >> $@
	echo ";;; no-update-autoloads: t" >> $@
	echo ";;; End:" >> $@
	echo ";;; loaddefs.el ends here" >> $@
autoloads: loaddefs.el doit
	wd=.; $(setwins); \
	echo Directories: $$wins; \
	$(emacs) --eval '(setq generated-autoload-file "'`pwd`'/loaddefs.el")' -f batch-update-autoloads $$wins

subdirs.el:
	$(MAKE) $(MFLAGS) update-subdirs
update-subdirs: doit
	wd=.; $(setwins); \
	for file in $$wins; do \
	   ../update-subdirs $$file; \
	done;

updates: update-subdirs autoloads finder-data custom-deps

TAGS: $(lisptagsfiles1) $(lisptagsfiles2)
	${ETAGS} $(lisptagsfiles1) $(lisptagsfiles2)

TAGS-LISP: $(lispsource)$(lisptagsfiles1) $(lispsource)$(lisptagsfiles2)
	${ETAGS} -o TAGS-LISP \
	  $(lispsource)$(lisptagsfiles1) $(lispsource)$(lisptagsfiles2)

.el.elc: subdirs.el
	-EMACSLOADPATH=`pwd` $(emacs) -f batch-byte-compile $<

$(DONTCOMPILE:.el=.elc):
	-rm -f $@

# Compile all Lisp files, except those from DONTCOMPILE.  This
# compiles files unconditionally.  All .elc files are made writable
# before compilation in case we checked out read-only (CVS option -r).
# Files must be compiled one by one, otherwise apparently
# eval-when-compile's in some Lisp files make problems in files being
# compiled later.  We also set the load-path of the Emacs used for
# compilation to the current directory and its subdirectories, to
# make sure require's and load's in the files being compiled find
# the right files.

compile-files: subdirs.el doit
	find . -name "*.elc" -print | xargs chmod +w;		\
	wd=.; $(setwins);					\
	elpat=`echo $$wins | tr '[ 	]' '[\012\012]' | 	\
		sed -e 's|\(.\)$$|\1/|' -e 's|^\./||' -e 's|$$|*.el|'`;	\
	els=`echo $$elpat $(DONTCOMPILE) | tr '[	 ]' '[\012\012]' | sort | uniq -u`; \
	echo $$els; \
	EMACSLOADPATH=`pwd` $(emacs) -f batch-byte-compile $$els

# Backup compiled Lisp files in elc.tar.gz.  If that file already
# exists, make a backup of it.

backup-compiled-files:
	-mv elc.tar.gz elc.tar.gz~
	tar czf elc.tar.gz *.elc */*.elc

# Compile Lisp files, but save old compiled files first.

compile: backup-compiled-files compile-files

# Recompile all Lisp files which are newer than their .elc files.
# Note that this doesn't create .elc files.  It only recompiles if an
# .elc is present.

recompile: doit
	$(emacs) -f batch-byte-recompile-directory .

# Remove files for a bootstrap.  Compiled Lisp files must be removed
# because otherwise `bootstrap-emacs' would not be built from sources.

bootstrap-clean:
	-rm -f *.elc */*.elc

# Generate/update files for the bootstrap process.

bootstrap: compile-files autoloads custom-deps

# Makefile ends here.
