# ndk-build-helper.mk -- Helper for ndk-build.m4.
# Copyright (C) 2023-2026 Free Software Foundation, Inc.
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
# cross/ndk-build/README for more details.

# TARGET_ARCH_ABI is the ABI that is being built for.
TARGET_ARCH_ABI := $(EMACS_ABI)

# TARGET_ARCH is the architecture that is being built for.
TARGET_ARCH := $(NDK_BUILD_ARCH)

# NDK_LAST_MAKEFILE is the last Makefile that was included.
NDK_LAST_MAKEFILE = $(lastword $(filter %Android.mk,$(MAKEFILE_LIST)))

# local-makefile is the current Makefile being loaded.
local-makefile = $(NDK_LAST_MAKEFILE)

# Make NDK_BUILD_DIR absolute.
NDK_BUILD_DIR := $(absname $(NDK_BUILD_DIR))

# Make EMACS_SRCDIR absolute.  This must be absolute, or nested
# Android.mk files will not be able to find CLEAR_VARS.
EMACS_SRCDIR := $(absname $(EMACS_SRCDIR))

# my-dir is a function that returns the Android module directory.  If
# no Android.mk has been loaded, use ANDROID_MODULE_DIRECTORY.
my-dir = $(patsubst %/,%,$(or $(and $(local-makefile),$(dir $(local-makefile))),$(ANDROID_MODULE_DIRECTORY)))

# Return all Android.mk files under the first arg.
all-makefiles-under = $(wildcard $(1)/*/Android.mk)

# Return all Android.mk files in subdirectories of this Makefile's
# location.
all-subdir-makefiles = $(call all-makefiles-under,$(call my-dir))

# These functions are not implemented.
parent-makefile =
grand-parent-makefile =

NDK_IMPORTS :=

# Add the specified module (arg 1) to NDK_IMPORTS.
import-module = $(eval NDK_IMPORTS += $(1))

# Print out module information every time BUILD_SHARED_LIBRARY is
# called.

BUILD_SHARED_LIBRARY=$(BUILD_AUXDIR)ndk-build-helper-1.mk
BUILD_STATIC_LIBRARY=$(BUILD_AUXDIR)ndk-build-helper-2.mk
BUILD_EXECUTABLE=$(BUILD_AUXDIR)ndk-build-helper-3.mk
CLEAR_VARS=$(BUILD_AUXDIR)ndk-build-helper-4.mk

# Now include Android.mk.

include $(ANDROID_MAKEFILE)

# Finally, print out the imports.
$(info Start Imports)
$(info $(NDK_IMPORTS))
$(info End Imports)

# Dummy target.
all:
