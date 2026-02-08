# Copyright 2023-2026 Free Software Foundation, Inc.

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

# ndk-build works by including a bunch of Makefiles which set
# variables, and then having those Makefiles include another makefile
# which actually builds targets.

# List of system libraries to ignore.
NDK_SYSTEM_LIBRARIES = z libz libc c libdl dl stdc++ libstdc++ stlport libstlport gnustl libgnustl c++ libc++ log liblog android libandroid

# Save information.
NDK_LOCAL_PATH_$(LOCAL_MODULE) := $(LOCAL_PATH)
NDK_LOCAL_STATIC_LIBRARIES_$(LOCAL_MODULE) := $(LOCAL_STATIC_LIBRARIES) $(LOCAL_WHOLE_STATIC_LIBRARIES)
NDK_LOCAL_WHOLE_LIBRARIES_$(LOCAL_MODULE) := $(LOCAL_WHOLE_STATIC_LIBRARIES)
NDK_LOCAL_SHARED_LIBRARIES_$(LOCAL_MODULE) := $(LOCAL_SHARED_LIBRARIES)
NDK_LOCAL_EXPORT_CFLAGS_$(LOCAL_MODULE) := $(LOCAL_EXPORT_CFLAGS)
NDK_LOCAL_EXPORT_C_INCLUDES_$(LOCAL_MODULE) := $(LOCAL_EXPORT_C_INCLUDES) $(LOCAL_EXPORT_C_INCLUDE_DIRS)
NDK_LOCAL_A_NAMES_$(LOCAL_MODULE) :=
NDK_WHOLE_A_NAMES_$(LOCAL_MODULE) :=
NDK_LOCAL_SO_NAMES_$(LOCAL_MODULE) :=

# Linker options enabling 16k page sizes.
NDK_SO_EXTRA_FLAGS_$(LOCAL_MODULE) := $(NDK_BUILD_SO_LDFLAGS)

# List of all dependencies resolved for this module thus far.
# Used to avoid infinite recursion.
# Separate the variable which lists modules for which CFLAGS
# have been resolved from the variable which lists modules
# for which library dependencies have been resolved, in order
# to catch the case where a library dependency is skipped
# despite its CFLAGS being added.
NDK_RESOLVED_$(LOCAL_MODULE) :=
NDK_RESOLVED_CFLAGS_$(LOCAL_MODULE) :=

define ndk-resolve

ifeq ($$(filter $(1)$(and $(3),whole),$$(NDK_RESOLVED_CFLAGS_$(LOCAL_MODULE))),)
# Always mark this module's cflags as having been resolved, even if
# this is a whole library.
NDK_RESOLVED_CFLAGS_$(LOCAL_MODULE) += $(1)
NDK_CFLAGS_$(LOCAL_MODULE) += $(NDK_LOCAL_EXPORT_CFLAGS_$(1))
NDK_CFLAGS_$(LOCAL_MODULE) += $(addprefix -I,$(NDK_LOCAL_EXPORT_C_INCLUDES_$(1)))
endif

ifeq ($$(filter $(1)$(and $(3),whole),$$(NDK_RESOLVED_$(LOCAL_MODULE))),)
# Now append local libraries, as long as this library isn't a shared
# library itself.
ifeq ($(4),)

# Mark this module's library dependencies as having been resolved.
NDK_RESOLVED_$(LOCAL_MODULE) += $(1)

# If this is a whole library, then mark this as resolved too, and
# remove the library from the normal static library list.
ifneq ($(3),)
NDK_RESOLVED_$(LOCAL_MODULE) += $(1)whole
endif

# If the module happens to be zlib, then add -lz to the shared library
# flags.
ifeq ($(strip $(1)),libz)
NDK_SO_EXTRA_FLAGS_$(LOCAL_MODULE) += -lz
endif

ifeq ($(strip $(1)),z)
NDK_SO_EXTRA_FLAGS_$(LOCAL_MODULE) += -lz
endif

# Likewise for libdl.
ifeq ($(strip $(1)),libdl)
NDK_SO_EXTRA_FLAGS_$(LOCAL_MODULE) += -ldl
endif

ifeq ($(strip $(1)),dl)
NDK_SO_EXTRA_FLAGS_$(LOCAL_MODULE) += -ldl
endif

# Likewise for libstdc++.
ifeq ($(strip $(1)),libstdc++)
NDK_SO_EXTRA_FLAGS_$(LOCAL_MODULE) += $(NDK_BUILD_CXX_LDFLAGS)
endif

ifeq ($(strip $(1)),stdc++)
NDK_SO_EXTRA_FLAGS_$(LOCAL_MODULE) += $(NDK_BUILD_CXX_LDFLAGS)
endif

ifeq ($(strip $(1)),libstlport)
NDK_SO_EXTRA_FLAGS_$(LOCAL_MODULE) += $(NDK_BUILD_CXX_LDFLAGS)
endif

ifeq ($(strip $(1)),stlport)
NDK_SO_EXTRA_FLAGS_$(LOCAL_MODULE) += $(NDK_BUILD_CXX_LDFLAGS)
endif

ifeq ($(strip $(1)),libgnustl)
NDK_SO_EXTRA_FLAGS_$(LOCAL_MODULE) += $(NDK_BUILD_CXX_LDFLAGS)
endif

ifeq ($(strip $(1)),gnustl)
NDK_SO_EXTRA_FLAGS_$(LOCAL_MODULE) += $(NDK_BUILD_CXX_LDFLAGS)
endif

ifeq ($(strip $(1)),libc++)
NDK_SO_EXTRA_FLAGS_$(LOCAL_MODULE) += $(NDK_BUILD_CXX_LDFLAGS)
endif

ifeq ($(strip $(1)),c++)
NDK_SO_EXTRA_FLAGS_$(LOCAL_MODULE) += $(NDK_BUILD_CXX_LDFLAGS)
endif

# Likewise for liblog.
ifeq ($(strip $(1)),liblog)
NDK_SO_EXTRA_FLAGS_$(LOCAL_MODULE) += -llog
endif

ifeq ($(strip $(1)),log)
NDK_SO_EXTRA_FLAGS_$(LOCAL_MODULE) += -llog
endif

# Likewise for libandroid.
ifeq ($(strip $(1)),libandroid)
NDK_SO_EXTRA_FLAGS_$(LOCAL_MODULE) += -landroid
endif

ifeq ($(strip $(1)),android)
NDK_SO_EXTRA_FLAGS_$(LOCAL_MODULE) += -landroid
endif

ifeq ($(findstring $(1),$(NDK_SYSTEM_LIBRARIES))$(2)$(3),)
ifeq ($(filter-out lib%,$(1)),)
NDK_LOCAL_SO_NAMES_$(LOCAL_MODULE) += $(1)_emacs.so
else
NDK_LOCAL_SO_NAMES_$(LOCAL_MODULE) += lib$(1)_emacs.so
endif
endif

ifneq ($(2),)
ifeq ($(filter-out lib%,$(1)),)
NDK_LOCAL_A_NAMES_$(LOCAL_MODULE) += $(1).a
else
NDK_LOCAL_A_NAMES_$(LOCAL_MODULE) += lib$(1).a
endif
endif

ifneq ($(3),)
ifeq ($(filter-out lib%,$(1)),)
NDK_WHOLE_A_NAMES_$(LOCAL_MODULE) += $(1).a
else
NDK_WHOLE_A_NAMES_$(LOCAL_MODULE) += lib$(1).a
endif

# Remove this archive from the regular archive list, should it already
# exists.  Any given archive should only appear once, and if an
# archive has been specified as whole it should always be whole.
NDK_LOCAL_A_NAMES_$(LOCAL_MODULE) := $$(filter-out lib$(1).a,$$(NDK_LOCAL_A_NAMES_$(LOCAL_MODULE)))
NDK_LOCAL_A_NAMES_$(LOCAL_MODULE) := $$(filter-out $(1).a,$$(NDK_LOCAL_A_NAMES_$(LOCAL_MODULE)))
endif
endif

$$(foreach module,$$(NDK_LOCAL_STATIC_LIBRARIES_$(1)),$$(eval $$(call ndk-resolve,$$(module),1,,$(or $(4),$(if $(2)$(3),,1)))))
$$(foreach module,$$(NDK_LOCAL_SHARED_LIBRARIES_$(1)),$$(eval $$(call ndk-resolve,$$(module),,,$(or $(4),$(if $(2)$(3),,1)))))
$$(foreach module,$$(NDK_LOCAL_WHOLE_LIBRARIES_$(1)),$$(eval $$(call ndk-resolve,$$(module),,1,$(or $(4),$(if $(2)$(3),,1)))))
endif

endef

# Add shared libraries to the shared object names when they appear as
# a top level dependency.  However, do not recursively add the names
# of this module's shared library dependencies, if it is just a shared
# library, since it will link to those shared libraries itself.
$(foreach module,$(LOCAL_SHARED_LIBRARIES),$(eval $(call ndk-resolve,$(module),,,)))
$(foreach module,$(LOCAL_STATIC_LIBRARIES),$(eval $(call ndk-resolve,$(module),1,,)))
$(foreach module,$(LOCAL_WHOLE_STATIC_LIBRARIES), $(eval $(call ndk-resolve,$(module),,1,)))
