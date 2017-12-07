# Build Emacs from a fresh tarball or version-control checkout.

# Copyright (C) 2011-2017 Free Software Foundation, Inc.
#
# This file is part of GNU Emacs.
#
# GNU Emacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
#
# written by Paul Eggert


# This GNUmakefile is for GNU Make.  It is for convenience, so that
# one can run 'make' in an unconfigured source tree.  In such a tree,
# this file causes GNU Make to first create a standard configuration
# with the default options, and then reinvokes itself on the
# newly-built Makefile.  If the source tree is already configured,
# this file defers to the existing Makefile.

# If you are using a non-GNU 'make', or if you want non-default build
# options, or if you want to build in an out-of-source tree, please
# run "configure" by hand.  But run autogen.sh first, if the source
# was checked out directly from the repository.

# If a Makefile already exists, just use it.

ifeq ($(wildcard Makefile),Makefile)
include Makefile
else

# If cleaning and Makefile does not exist, don't bother creating it.
# The source tree is already clean, or is in a weird state that
# requires expert attention.

ifeq ($(filter-out %clean,$(or $(MAKECMDGOALS),default)),)

$(MAKECMDGOALS):
	@echo >&2 'No Makefile; skipping $@.'

else

# No Makefile, and not cleaning.
# If 'configure' does not exist, Emacs must have been checked
# out directly from the repository; run ./autogen.sh.
# Once 'configure' exists, run it.
# Finally, run the actual 'make'.

ORDINARY_GOALS = $(filter-out configure Makefile bootstrap,$(MAKECMDGOALS))

default $(ORDINARY_GOALS): Makefile
	$(MAKE) -f Makefile $(MAKECMDGOALS)
# Execute in sequence, so that multiple user goals don't conflict.
.NOTPARALLEL:

configure:
	@echo >&2 'There seems to be no "configure" file in this directory.'
	@echo >&2 Running ./autogen.sh ...
	./autogen.sh
	@echo >&2 '"configure" file built.'

Makefile: configure
	@echo >&2 'There seems to be no Makefile in this directory.'
	@echo >&2 'Running ./configure ...'
	./configure
	@echo >&2 'Makefile built.'

# 'make bootstrap' in a fresh checkout needn't run 'configure' twice.
bootstrap: Makefile
	$(MAKE) -f Makefile all

.PHONY: bootstrap default $(ORDINARY_GOALS)

endif
endif
