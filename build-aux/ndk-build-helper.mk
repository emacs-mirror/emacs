# ndk-build-helper.mk -- Helper for ndk-build.m4.
# Copyright (C) 2023 Free Software Foundation, Inc.
# This file is part of GNU Emacs.

# GNU Emacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

# This Makefile sets up enough to parse an Android-style Android.mk
# file and return useful information about its contents.

# See the text under ``NDK BUILD SYSTEM IMPLEMENTATION'' in
# INSTALL.android for more details.

# Make NDK_BUILD_DIR absolute.
NDK_BUILD_DIR := $(absname $(NDK_BUILD_DIR))

# my-dir is a function that returns the Android module directory.
my-dir = $(ANDROID_MODULE_DIRECTORY)

# all-subdir-makefiles is a function which returns all Android.mk
# files within this directory.
all-subdir-makefiles = $(shell find . -name "Android.mk")

# These functions are not implemented.
parent-makefile =
grand-parent-makefile =
import-module =

# Print out module information every time BUILD_SHARED_LIBRARY is
# called.

BUILD_SHARED_LIBRARY=$(EMACS_SRCDIR)/build-aux/ndk-build-helper-1.mk
BUILD_STATIC_LIBRARY=$(EMACS_SRCDIR)/build-aux/ndk-build-helper-2.mk
BUILD_EXECUTABLE=$(EMACS_SRCDIR)/build-aux/ndk-build-helper-3.mk
CLEAR_VARS=$(EMACS_SRCDIR)/build-aux/ndk-build-helper-4.mk

# Now include Android.mk.

include $(ANDROID_MAKEFILE)

# Dummy target.
all:
