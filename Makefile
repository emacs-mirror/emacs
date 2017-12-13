.PHONY : test

emacs ?= emacs
CASK ?= cask

LOADPATH = -L .

ELPA_DIR = \
	.cask/$(shell $(emacs) -Q --batch --eval '(princ emacs-version)')/elpa

test: elpa
	$(CASK) exec $(emacs) -Q -batch $(LOADPATH) \
		-l which-key-tests.el -f ert-run-tests-batch-and-exit

elpa: $(ELPA_DIR)
$(ELPA_DIR): Cask
	$(CASK) install
	mkdir -p $(ELPA_DIR)
	touch $@
