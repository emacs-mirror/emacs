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
NDK_LOCAL_SHARED_LIBRARIES_$(LOCAL_MODULE) := $(LOCAL_SHARED_LIBRARIES)
NDK_LOCAL_EXPORT_CFLAGS_$(LOCAL_MODULE) := $(LOCAL_EXPORT_CFLAGS)
NDK_LOCAL_EXPORT_C_INCLUDES_$(LOCAL_MODULE) := $(LOCAL_EXPORT_C_INCLUDES)

# List of all dependencies resolved for this module thus far.
# Used to avoid infinite recursion.
NDK_RESOLVED_$(LOCAL_MODULE) :=

define ndk-resolve

ifeq ($(patsubst $(1),,$(NDK_RESOLVED$(LOCAL_MODULE))),$(NDK_RESOLVED$(LOCAL_MODULE)))
NDK_RESOLVED$(LOCAL_MODULE) += $(1)
NDK_CFLAGS_$(LOCAL_MODULE) += $(NDK_LOCAL_EXPORT_CFLAGS_$(1))
NDK_CFLAGS_$(LOCAL_MODULE) += $(addprefix -I,$(NDK_LOCAL_EXPORT_C_INCLUDES_$(1)))

$$(foreach module,$$(NDK_LOCAL_STATIC_LIBRARIES_$(1)),$$(eval $$(call ndk-resolve,$$(module))))
$$(foreach module,$$(NDK_LOCAL_SHARED_LIBRARIES_$(1)),$$(eval $$(call ndk-resolve,$$(module))))
endif

endef

$(foreach module,$(LOCAL_SHARED_LIBRARIES),$(eval $(call ndk-resolve,$(module))))
$(foreach module,$(LOCAL_STATIC_LIBRARIES) $(LOCAL_WHOLE_STATIC_LIBRARIES),$(eval $(call ndk-resolve,$(module))))
