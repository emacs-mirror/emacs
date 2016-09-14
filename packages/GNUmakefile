## This file is called GNUmakefile because Makefile is git ignored. Rename
## when this is autoconf'd


EMACS=../src/emacs
ELPA=../../elpa


DIRS=$(filter-out .,$(subst ./,,$(shell find . -maxdepth 1 -type d)))

## dependent on makefile
all: pkg-all $(EMACS)

check: cleanlog check-packages

check-maybe: check-packages

WRITE_LOG = > $@ 2>&1 || { stat=ERROR; cat $@; }; echo $$stat: $@

gnumakefile-inc.mk: ./admin/package-makefile.el
	$(EMACS) --batch --load admin/package-makefile.el \
		--funcall package-makefile

include gnumakefile-inc.mk

cleanlog:
	find . -name "*log" -exec rm -v {} \;

clean: cleanlog
	find . -name "*pkg.el" -exec rm -v {} \;
	find . -name "*-autoloads.el" -exec rm -v {} \;
	find . -name "*elc" -exec rm -v {} \;


.PHONY: check clean all
