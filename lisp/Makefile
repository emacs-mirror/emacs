#
# Maintenance productions for the Lisp directory
#
SOURCES = *.el COPYING Makefile
lisptagsfiles = [a-zA-Z]*.el term/[a-zA-Z]*.el

unlock:
	chmod u+w $(SOURCES)

relock:
	chmod -w $(SOURCES)
	chmod +w version.el finder-inf.el loaddefs.el

TAGS: $(lisptagsfiles)
	../lib-src/etags $(lisptagsfiles)
