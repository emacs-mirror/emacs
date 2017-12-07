# Configurations for ../lib/gnulib.mk.
#
# Copyright 2017 Free Software Foundation, Inc.
#
# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this file.  If not, see <https://www.gnu.org/licenses/>.


# Gnulib modules to be omitted from Emacs.

# Omit them because they either conflict with MinGW headers or
# headers in nt/inc, or because those modules won't compile with
# MinGW, or because Emacs already has the corresponding facilities as
# part of Emacs sources, and their implementation is incompatible with
# Gnulib's.
#
# In general, do NOT omit modules that
# don't need to be omitted, to minimize the differences from
# upstream gnulib.mk and thus make the maintenance easier.  Every
# header file whose generation is controlled by configure-time tests
# does NOT need to be removed; instead, force the configure script to
# accept whatever MinGW has to offer, by defining the appropriate
# Autoconf variable in the nt/mingw-cfg.site file.  Headers that are
# generated conditionally have the tell-tale "ifneq (,$(GL_GENERATE_foo_H))"
# condition before their Makefile snippet in this file.  Likewise, do
# NOT remove gnulib modules which introduce header files that don't
# exist in MinGW and in nt/inc/, since they cannot possibly clash
# with anything.  Gnulib modules that introduce source *.c files also
# need not be removed; if they define functions that could clash with
# the w32 substitutes in Emacs, disable their compilation by defining
# suitable variables in nt/mingw-cfg.site.
# ----------------------------------------------------------------------

OMIT_GNULIB_MODULE_acl-permissions = true
OMIT_GNULIB_MODULE_allocator = true
OMIT_GNULIB_MODULE_at-internal = true
OMIT_GNULIB_MODULE_careadlinkat = true
OMIT_GNULIB_MODULE_dirent = true
OMIT_GNULIB_MODULE_dirfd = true
OMIT_GNULIB_MODULE_fcntl = true
OMIT_GNULIB_MODULE_fcntl-h = true
OMIT_GNULIB_MODULE_fsusage = true
OMIT_GNULIB_MODULE_inttypes-incomplete = true
OMIT_GNULIB_MODULE_open = true
OMIT_GNULIB_MODULE_pipe2 = true
OMIT_GNULIB_MODULE_secure_getenv = true
OMIT_GNULIB_MODULE_signal-h = true
OMIT_GNULIB_MODULE_stdio = true
OMIT_GNULIB_MODULE_stdlib = true
OMIT_GNULIB_MODULE_sys_select = true
OMIT_GNULIB_MODULE_sys_stat = true
OMIT_GNULIB_MODULE_sys_time = true
OMIT_GNULIB_MODULE_sys_types = true
OMIT_GNULIB_MODULE_unistd = true
