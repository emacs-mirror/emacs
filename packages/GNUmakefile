## This file is called GNUmakefile because Makefile is git ignored. Rename
## when this is autoconf'd


EMACS=../src/emacs

DIRS=$(filter-out .,$(subst ./,,$(shell find . -maxdepth 1 -type d)))

## alas "all" is an ELPA package, so this is going to break
all: build-all

build-all: $(DIRS) $(EMACS)


define package_template
$(1): $(1)/$(1)-pkg.el

$(1)/$(1)-pkg.el:
	$$(EMACS) --batch --load package-build.el --eval '(package-build-prepare "$(1)")'

endef

$(foreach dir,$(DIRS),$(eval $(call package_template,$(dir))))

define test_template
$(1)-test:
	$$(EMACS) --batch --load package-test.el --eval '(assess-discover-run-and-exit-batch-dir "$(1)")'
endef

$(foreach dir,$(DIRS),$(eval $(call test_template,$(dir))))

test: $(patsubst %,%-test,$(DIRS))

clean:
	find . -name "*pkg.el" -exec rm -v {} \;
	find . -name "*-autoloads.el" -exec rm -v {} \;
	find . -name "*elc" -exec rm -v {} \;
