#
# Maintenance productions for the Lisp directory
#
EMACS = emacs
SOURCES = *.el COPYING Makefile
lisptagsfiles = [a-zA-Z]*.el */[a-zA-Z]*.el

doit:

custom-deps: doit
	$(EMACS) -batch -l cus-dep.el -f custom-make-dependencies

finder-data: doit
	$(EMACS) -batch -l finder -f finder-compile-keywords-make-dist

autoloads: doit
	$(EMACS) -batch -f batch-update-autoloads .

TAGS: $(lisptagsfiles)
	../lib-src/etags $(lisptagsfiles)
