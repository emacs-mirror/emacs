#
# Maintenance productions for the Lisp directory
#
EMACS = emacs
SOURCES = *.el COPYING Makefile
lisptagsfiles = [a-zA-Z]*.el */[a-zA-Z]*.el

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
	   case $$file in */Old | */RCS) ;; \
		*) wins="$$wins $$file" ;; \
	   esac; \
        done; \
	$(EMACS) -batch -l finder -f finder-compile-keywords-make-dist $$wins

autoloads: doit
	subdirs=`find . -type d -print`; \
	for file in $$subdirs; do \
	   case $$file in */Old | */RCS) ;; \
		*) wins="$$wins $$file" ;; \
	   esac; \
        done; \
	$(EMACS) -batch -f batch-update-autoloads $$wins

update-subdirs: doit
	subdirs=`find . -type d -print`; \
	for file in $$subdirs; do \
	   case $$file in */Old | */RCS) ;; \
		*) wins="$$wins $$file" ;; \
	   esac; \
        done; \
	for file in $$wins; do \
	   ../update-subdirs $$file; \
	done;

updates: doit
	subdirs=`find . -type d -print`; \
	for file in $$subdirs; do \
	   case $$file in */Old | */RCS) ;; \
		*) wins="$$wins $$file" ;; \
	   esac; \
        done; \
	for file in $$wins; do \
	   ../update-subdirs $$file; \
	done; \
	$(EMACS) -batch -l cus-dep.el -f custom-make-dependencies $$wins \
	$(EMACS) -batch -l finder -f finder-compile-keywords-make-dist $$wins \
	$(EMACS) -batch -f batch-update-autoloads $$wins

TAGS: $(lisptagsfiles)
	../lib-src/etags $(lisptagsfiles)
