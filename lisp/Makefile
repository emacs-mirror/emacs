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
	$(EMACS) -batch -l ./cus-dep.el -f custom-make-dependencies

TAGS: $(lisptagsfiles)
	../lib-src/etags $(lisptagsfiles)
