#
# Maintenance productions for the Lisp directory
#
EMACS = emacs
SOURCES = *.el COPYING Makefile
lisptagsfiles = [a-zA-Z]*.el term/[a-zA-Z]*.el

unlock:
	chmod u+w $(SOURCES)

relock:
	chmod -w $(SOURCES)
	chmod +w version.el finder-inf.el loaddefs.el

doit:

cus-load.el: doit
	$(EMACS) -batch -l cus-dep.el -f custom-make-dependencies

finder-inf.el: doit
	$(EMACS) -batch -l finder -f finder-compile-keywords-make-dist

autoloads: doit
	$(EMACS) -batch -f batch-update-autoloads .

TAGS: $(lisptagsfiles)
	../lib-src/etags $(lisptagsfiles)
