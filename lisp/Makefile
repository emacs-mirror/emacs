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

# The Emacs used to compile Lisp files from scratch.

temacs=../src/temacs -batch -l ./loadup.el 

# Common command to find subdirectories

setwins=subdirs=`find $$wd -type d -print`; \
	for file in $$subdirs; do \
	   case $$file in */Old | */RCS | */CVS | */CVS/* | */=* ) ;; \
		*) wins="$$wins $$file" ;; \
	   esac; \
        done

doit:

custom-deps: doit
	wd=.; $(setwins); \
	echo Directories: $$wins; \
	$(emacs) -l cus-dep -f custom-make-dependencies $$wins

finder-data: doit
	wd=.; $(setwins); \
	echo Directories: $$wins; \
	$(emacs) -l finder -f finder-compile-keywords-make-dist $$wins

autoloads: doit
	wd=.; $(setwins); \
	echo Directories: $$wins; \
	$(emacs) -f batch-update-autoloads $$wins

update-subdirs: doit
	wd=.; $(setwins); \
	for file in $$wins; do \
	   ../update-subdirs $$file; \
	done;

updates: doit
	wd=.; $(setwins); \
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
# Files must be compiled one by one, otherwise apparently
# eval-when-compile's in some Lisp files make problems in files being
# compiled later.  We also set the load-path of the Emacs used for
# compilation to the current directory and its subdirectories, to
# make sure require's and load's in the files being compiled find
# the right files.

compile: doit
	wd=`pwd`;						\
	find $$wd -name "*.elc" -print | xargs chmod +w;	\
	$(setwins)						\
	loadpath="(setq load-path '($$wins))";			\
	dont_compile=`echo $(DONTCOMPILE)`;			\
	for el in $(EL); do					\
	  compile_it=y;						\
	  for dont in $$dont_compile; do			\
	    if test $$el = $$dont; then				\
	      compile_it=n;					\
            fi;							\
          done;							\
          if test $$compile_it = y; then			\
            $(emacs) --eval "$$loadpath" -f batch-byte-compile $$el; \
          else							\
            echo "Don't compile $$el";				\
          fi;							\
	done

# Compile Lisp files when all we have is the temacs in ../src.  All
# .elc files under the current directory are deleted first to make
# sure we start from a clean basis.

bootstrap-compile: doit
	wd=`pwd`;						\
	find $$wd -name "*.elc" -print | xargs rm -f            \
	$(setwins)						\
	loadpath="(setq load-path '($$wins))";			\
	dont_compile=`echo $(DONTCOMPILE)`;			\
	for el in $(EL); do					\
	  compile_it=y;						\
	  for dont in $$dont_compile; do			\
	    if test $$el = $$dont; then				\
	      compile_it=n;					\
            fi;							\
          done;							\
          if test $$compile_it = y; then			\
            $(temacs) --eval "$$loadpath" -f batch-byte-compile $$el; \
          else							\
            echo "Don't compile $$el";				\
          fi;							\
	done


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
#   Can't this rule be used in place of `autoload'? -sm

autoloads-with-other-emacs: doit
	wd=.; $(setwins);						\
	echo Directories: $$wins;				\
	$(emacs) --eval '(setq source-directory ".")'		\
		-f batch-update-autoloads $$wins

# Makefile ends here.
