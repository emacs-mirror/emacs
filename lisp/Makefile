#
# Maintenance productions for the Lisp directory
#
EMACS = emacs
SOURCES = *.el COPYING Makefile
lisptagsfiles = [a-zA-Z]*.el [a-zA-Z]*/[a-zA-Z]*.el
ETAGS = ../lib-src/etags

dontcompilefiles: bindings.el blessmail.el bruce.el cus-load.el cus-start.el
dontcompilefiles: forms-d2.el forms-pass.el 
dontcompilefiles: latin-1.el latin-2.el latin-3.el latin-4.el latin-5.el
dontcompilefiles: loaddefs.el loadup.el mule-conf.el patcomp.el
dontcompilefiles: paths.el sc.el subdirs.el term-nasty.el version.el 

doit:

custom-deps: doit
	subdirs=`find . -type d -print`; \
	for file in $$subdirs; do \
	   case $$file in */Old | */RCS) ;; \
		*) wins="$$wins $$file" ;; \
	   esac; \
        done; \
	$(EMACS) -batch -l cus-dep.el -f custom-make-dependencies $$wins

finder-data: doit
	subdirs=`find . -type d -print`; \
	for file in $$subdirs; do \
	   case $$file in */Old | */RCS | */=* ) ;; \
		*) wins="$$wins $$file" ;; \
	   esac; \
        done; \
	$(EMACS) -batch -l finder -f finder-compile-keywords-make-dist $$wins

autoloads: doit
	subdirs=`find . -type d -print`; \
	for file in $$subdirs; do \
	   case $$file in */Old | */RCS | */=* ) ;; \
		*) wins="$$wins $$file" ;; \
	   esac; \
        done; \
	$(EMACS) -batch -f batch-update-autoloads $$wins

update-subdirs: doit
	subdirs=`find . -type d -print`; \
	for file in $$subdirs; do \
	   case $$file in */Old | */RCS | */=* ) ;; \
		*) wins="$$wins $$file" ;; \
	   esac; \
        done; \
	for file in $$wins; do \
	   ../update-subdirs $$file; \
	done;

updates: doit
	subdirs=`find . -type d -print`; \
	for file in $$subdirs; do \
	   case $$file in */Old | */RCS | */=* ) ;; \
		*) wins="$$wins $$file" ;; \
	   esac; \
        done; \
	for file in $$wins; do \
	   ../update-subdirs $$file; \
	done; \
	$(EMACS) -batch -l cus-dep.el -f custom-make-dependencies $$wins; \
	$(EMACS) -batch -l finder -f finder-compile-keywords-make-dist $$wins; \
	$(EMACS) -batch -f batch-update-autoloads $$wins

TAGS: $(lisptagsfiles)
	${ETAGS} $(lisptagsfiles)
