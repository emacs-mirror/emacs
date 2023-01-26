# Copyright 2023 Free Software Foundation, Inc.

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

# Save information.
NDK_LOCAL_PATH_$(LOCAL_MODULE) := $(LOCAL_PATH)
NDK_LOCAL_STATIC_LIBRARIES_$(LOCAL_MODULE) := $(LOCAL_STATIC_LIBRARIES) $(LOCAL_WHOLE_STATIC_LIBRARIES)
NDK_LOCAL_WHOLE_LIBRARIES_$(LOCAL_MODULE) := $(LOCAL_WHOLE_STATIC_LIBRARIES)
NDK_LOCAL_SHARED_LIBRARIES_$(LOCAL_MODULE) := $(LOCAL_SHARED_LIBRARIES)
NDK_LOCAL_EXPORT_CFLAGS_$(LOCAL_MODULE) := $(LOCAL_EXPORT_CFLAGS)
NDK_LOCAL_EXPORT_C_INCLUDES_$(LOCAL_MODULE) := $(LOCAL_EXPORT_C_INCLUDES) $(LOCAL_EXPORT_C_INCLUDE_DIRS)
NDK_LOCAL_A_NAMES_$(LOCAL_MODULE) :=
NDK_WHOLE_A_NAMES_$(LOCAL_MODULE) :=
NDK_SO_EXTRA_FLAGS_$(LOCAL_MODULE) :=

# List of all dependencies resolved for this module thus far.
# Used to avoid infinite recursion.
NDK_RESOLVED_$(LOCAL_MODULE) :=

define ndk-resolve

ifeq ($(patsubst $(1),,$(NDK_RESOLVED$(LOCAL_MODULE))),$(NDK_RESOLVED$(LOCAL_MODULE)))
NDK_RESOLVED_$(LOCAL_MODULE) += $(1)
NDK_CFLAGS_$(LOCAL_MODULE) += $(NDK_LOCAL_EXPORT_CFLAGS_$(1))
NDK_CFLAGS_$(LOCAL_MODULE) += $(addprefix -I,$(NDK_LOCAL_EXPORT_C_INCLUDES_$(1)))

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
NDK_SO_EXTRA_FLAGS_$(LOCAL_MODULE) += -lstdc++
endif

ifeq ($(strip $(1)),dl)
NDK_SO_EXTRA_FLAGS_$(LOCAL_MODULE) += -lstdc++
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

ifneq ($(2),)
ifneq ($(findstring lib,$(1)),)
NDK_LOCAL_A_NAMES_$(LOCAL_MODULE) += $(1).a
else
NDK_LOCAL_A_NAMES_$(LOCAL_MODULE) += lib$(1).a
endif
endif

ifneq ($(3),)
ifneq ($(findstring lib,$(1)),)
NDK_WHOLE_A_NAMES_$(LOCAL_MODULE) += $(1).a
else
NDK_WHOLE_A_NAMES_$(LOCAL_MODULE) += lib$(1).a
endif
endif

$$(foreach module,$$(NDK_LOCAL_STATIC_LIBRARIES_$(1)),$$(eval $$(call ndk-resolve,$$(module),1,)))
$$(foreach module,$$(NDK_LOCAL_SHARED_LIBRARIES_$(1)),$$(eval $$(call ndk-resolve,$$(module),,)))
$$(foreach module,$$(NDK_LOCAL_WHOLE_LIBRARIES_$(1)),$$(eval $$(call ndk-resolve,$$(module),,1)))
endif

endef

$(foreach module,$(LOCAL_SHARED_LIBRARIES),$(eval $(call ndk-resolve,$(module),,)))
$(foreach module,$(LOCAL_STATIC_LIBRARIES),$(eval $(call ndk-resolve,$(module),1,)))
$(foreach module,$(LOCAL_WHOLE_STATIC_LIBRARIES), $(eval $(call ndk-resolve,$(module),,1)))
