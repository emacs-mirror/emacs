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

# Leave this in.  I don't know if some utility depends on it.

dontcompilefiles: bindings.el blessmail.el bruce.el cus-load.el cus-start.el
dontcompilefiles: forms-d2.el forms-pass.el 
dontcompilefiles: latin-1.el latin-2.el latin-3.el latin-4.el latin-5.el
dontcompilefiles: loaddefs.el loadup.el mule-conf.el patcomp.el
dontcompilefiles: paths.el sc.el subdirs.el term-nasty.el version.el 
dontcompilefiles: generic-x.el latin-8.el latin-9.el

# Files which should not be compiled.  All file names must be relative
# to the `lisp' directory.

DONTCOMPILE = bindings.el mail/blessmail.el play/bruce.el cus-load.el \
	cus-start.el forms-d2.el forms-pass.el \
	international/latin-1.el international/latin-2.el \
	international/latin-3.el international/latin-4.el \
	international/latin-5.el \
	loaddefs.el loadup.el international/mule-conf.el patcomp.el \
	paths.el mail/sc.el subdirs.el term-nasty.el version.el \
	generic-x.el international/latin-8.el international/latin-9.el

# All Lisp source files in the current directory.

EL	= $(lisptagsfiles1) $(lisptagsfiles2)

# The actual Emacs command run in the targets below.

emacs = $(EMACS) $(EMACSOPT)


doit:

custom-deps: doit
	subdirs=`find . -type d -print`; \
	for file in $$subdirs; do \
	   case $$file in */Old | */RCS | */CVS | */=* ) ;; \
		*) wins="$$wins $$file" ;; \
	   esac; \
        done; \
	echo Directories: $$wins; \
	$(emacs) -l cus-dep -f custom-make-dependencies $$wins

finder-data: doit
	subdirs=`find . -type d -print`; \
	for file in $$subdirs; do \
	   case $$file in */Old | */RCS | */CVS | */=* ) ;; \
		*) wins="$$wins $$file" ;; \
	   esac; \
        done; \
	echo Directories: $$wins; \
	$(emacs) -l finder -f finder-compile-keywords-make-dist $$wins

autoloads: doit
	subdirs=`find . -type d -print`; \
	for file in $$subdirs; do \
	   case $$file in */Old | */RCS | */CVS | */=* ) ;; \
		*) wins="$$wins $$file" ;; \
	   esac; \
        done; \
	echo Directories: $$wins; \
	$(emacs) -f batch-update-autoloads $$wins

update-subdirs: doit
	subdirs=`find . -type d -print`; \
	for file in $$subdirs; do \
	   case $$file in */Old | */RCS | */CVS | */=* ) ;; \
		*) wins="$$wins $$file" ;; \
	   esac; \
        done; \
	for file in $$wins; do \
	   ../update-subdirs $$file; \
	done;

updates: doit
	subdirs=`find . -type d -print`; \
	for file in $$subdirs; do \
	   case $$file in */Old | */RCS | */CVS | */=* ) ;; \
		*) wins="$$wins $$file" ;; \
	   esac; \
        done; \
	for file in $$wins; do \
	   ../update-subdirs $$file; \
	done; \
	echo Directories: $$wins; \
	$(emacs) -l cus-dep -f custom-make-dependencies $$wins; \
	$(emacs) -l finder -f finder-compile-keywords-make-dist $$wins; \
	$(emacs) -f batch-update-autoloads $$wins

TAGS: $(lisptagsfiles1) $(lisptagsfiles2)
	${ETAGS} $(lisptagsfiles1) $(lisptagsfiles2)

TAGS-LISP: $(lispsource)$(lisptagsfiles1) $(lispsource)$(lisptagsfiles2)
	${ETAGS} -o TAGS-LISP \
	  $(lispsource)$(lisptagsfiles1) $(lispsource)$(lisptagsfiles2)

# Compile all Lisp files, except those from DONTCOMPILE.  This
# compiles files unconditionally.  All .elc files are made writable
# before compilation in case we checked out read-only (CVS option -r).

compile: doit
	dont_compile=`echo $(DONTCOMPILE)`;			\
	for el in $(EL); do					\
	  compile_it=y;						\
	  for dont in $$dont_compile; do			\
	    if test $$el = $$dont; then				\
	      compile_it=n;					\
            fi;							\
          done;							\
          if test $$compile_it = y; then			\
            els="$$el $$els";					\
            echo "Compile $$el";				\
          else							\
            echo "Don't compile $$el";				\
          fi;							\
	done;							\
	chmod +w $$els;						\
        $(emacs) -f batch-byte-compile $$els;

# Recompile all Lisp files which are newer than their .elc files.
# Note that this doesn't create .elc files.  It only recompiles if an
# .elc is present.

recompile: doit
	$(emacs) -f batch-byte-recompile-directory .

# Build loaddefs.el but with an Emacs executable which was not built
# in the current directory tree.  The problem in this case is that
# autoload.el reads and writes loaddefs.el in its `source-directory'.
# If that's different from the current directory tree, we can't build
# loaddefs.el.

autoloads-with-other-emacs: doit
	subdirs=`find . -type d -print`;			\
	for file in $$subdirs; do				\
	   case $$file in */Old | */RCS | */CVS | */=* ) ;;	\
		*) wins="$$wins $$file" ;;			\
	   esac;						\
        done;							\
	echo Directories: $$wins;				\
	$(emacs) --eval '(setq source-directory ".")'		\
		-f batch-update-autoloads $$wins

# Makefile ends here.
