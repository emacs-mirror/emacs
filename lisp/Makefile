#
# Maintenance productions for the Lisp directory
#
EMACS = emacs
SOURCES = *.el COPYING Makefile
lisptagsfiles1 = [a-zA-Z]*.el
lisptagsfiles2 = [a-zA-Z]*/[a-zA-Z]*.el
ETAGS = ../lib-src/etags

dontcompilefiles: bindings.el blessmail.el bruce.el cus-load.el cus-start.el
dontcompilefiles: forms-d2.el forms-pass.el 
dontcompilefiles: latin-1.el latin-2.el latin-3.el latin-4.el latin-5.el
dontcompilefiles: loaddefs.el loadup.el mule-conf.el patcomp.el
dontcompilefiles: paths.el sc.el subdirs.el term-nasty.el version.el 
dontcompilefiles: generic-x.el latin-8.el latin-9.el

doit:

custom-deps: doit
	subdirs=`find . -type d -print`; \
	for file in $$subdirs; do \
	   case $$file in */Old | */RCS | */CVS | */=* ) ;; \
		*) wins="$$wins $$file" ;; \
	   esac; \
        done; \
	echo Directories: $$wins; \
	$(EMACS) -batch -l cus-dep -f custom-make-dependencies $$wins

finder-data: doit
	subdirs=`find . -type d -print`; \
	for file in $$subdirs; do \
	   case $$file in */Old | */RCS | */CVS | */=* ) ;; \
		*) wins="$$wins $$file" ;; \
	   esac; \
        done; \
	echo Directories: $$wins; \
	$(EMACS) -batch -l finder -f finder-compile-keywords-make-dist $$wins

autoloads: doit
	subdirs=`find . -type d -print`; \
	for file in $$subdirs; do \
	   case $$file in */Old | */RCS | */CVS | */=* ) ;; \
		*) wins="$$wins $$file" ;; \
	   esac; \
        done; \
	echo Directories: $$wins; \
	$(EMACS) -batch -f batch-update-autoloads $$wins

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
	$(EMACS) -batch -l cus-dep -f custom-make-dependencies $$wins; \
	$(EMACS) -batch -l finder -f finder-compile-keywords-make-dist $$wins; \
	$(EMACS) -batch -f batch-update-autoloads $$wins

TAGS: $(lisptagsfiles1) $(lisptagsfiles2)
	${ETAGS} $(lisptagsfiles1) $(lisptagsfiles2)

TAGS-LISP: $(lispsource)$(lisptagsfiles1) $(lispsource)$(lisptagsfiles2)
	${ETAGS} -o TAGS-LISP \
	  $(lispsource)$(lisptagsfiles1) $(lispsource)$(lisptagsfiles2)
