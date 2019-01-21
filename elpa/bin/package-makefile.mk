FILE = $(realpath $(lastword $(MAKEFILE_LIST)))
DIR = $(dir $(FILE))
DIRS = $(subst /, ,$(DIR))
PACKAGE=$(lastword $(DIRS))

TEST=$(wildcard *-test.el)
TEST:=$(wildcard *-tests.el) $(TEST)
LISP=$(filter-out $(TEST), $(wildcard *.el))


LISP_DIR=../../../../lisp/elpa/$(PACKAGE)/
TEST_DIR=../../../../test/lisp/elpa/$(PACKAGE)/

LISP_TARGETS=$(addprefix $(LISP_DIR),$(LISP))
TEST_TARGETS=$(addprefix $(TEST_DIR),$(TEST))

deploy: $(LISP_TARGETS) $(TEST_TARGETS)
	# LISP $(LISP_TARGETS)
	# TEST $(TEST_TARGETS)

define lisp_template
  $(LISP_DIR)$(1): $(1) prepare
	cp $$< $$@
endef

$(foreach prereq, $(LISP), $(eval $(call lisp_template,$(prereq))))

define test_template
  $(TEST_DIR)$(1): $(1) prepare
	cp $$< $$@
endef

$(foreach prereq, $(TEST), $(eval $(call test_template,$(prereq))))

prepare:
	-mkdir $(LISP_DIR)
	-mkdir $(TEST_DIR)
