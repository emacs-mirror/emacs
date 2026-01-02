# ndk-build-helper-2.mk -- Helper for ndk-build.m4.
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

# Say a static library is being built
build_kind = static
NDK_SO_NAMES =
NDK_A_NAMES =

# Record this module's dependencies.  This information is used later
# on to recurse over libraries.
NDK_$(LOCAL_MODULE)_STATIC_LIBRARIES := $(LOCAL_STATIC_LIBRARIES) $(LOCAL_WHOLE_STATIC_LIBRARIES)
NDK_$(LOCAL_MODULE)_SHARED_LIBRARIES := $(LOCAL_SHARED_LIBRARIES)
NDK_$(LOCAL_MODULE)_EXPORT_INCLUDES := $(LOCAL_EXPORT_C_INCLUDE_DIRS) $(LOCAL_EXPORT_C_INCLUDES)
NDK_CXX_FLAG_$(LOCAL_MODULE) :=

$(info Building $(build_kind))
$(info $(LOCAL_MODULE))
$(info $(addprefix $(LOCAL_PATH:%/=%)/,$(LOCAL_SRC_FILES) $(LOCAL_SRC_FILES$(EMACS_ABI))))

ifeq ($(filter-out lib%,$(LOCAL_MODULE)),)
NDK_A_NAMES = $(LOCAL_MODULE).a
else
NDK_A_NAMES = lib$(LOCAL_MODULE).a
endif

define add-a-name
ifeq ($(filter-out lib%,$(1)),)
NDK_A_NAME = $(1).a
else
NDK_A_NAME = lib$(1).a
endif

ifeq ($$(findstring $$(NDK_A_NAME),$$(NDK_A_NAMES)),)
NDK_A_NAMES := $$(NDK_A_NAMES) $$(NDK_A_NAME)

# Now recurse over this module's dependencies.
$$(foreach module,$$(filter-out $$(SYSTEM_LIBRARIES), $$(NDK_$(1)_STATIC_LIBRARIES)),$$(eval $$(call add-a-name,$$(module))))
$$(foreach module,$$(filter-out $$(SYSTEM_LIBRARIES), $$(NDK_$(1)_SHARED_LIBRARIES)),$$(eval $$(call add-so-name,$$(module))))
endif

ifneq ($$(findstring stdc++,$$(NDK_$(1)_SHARED_LIBRARIES)),)
NDK_CXX_FLAG_$(LOCAL_MODULE) := yes
endif
endef

define add-so-name
ifeq ($(filter-out lib%,$(1)),)
NDK_SO_NAME = $(1)_emacs.so
else
NDK_SO_NAME = lib$(1)_emacs.so
endif

ifeq ($$(NDK_SO_NAMES:$$(NDK_SO_NAME)=),$$(NDK_SO_NAMES))
NDK_SO_NAMES := $$(NDK_SO_NAMES) $$(NDK_SO_NAME)

# Now recurse over this module's dependencies.
$$(foreach module,$$(filter-out $$(SYSTEM_LIBRARIES), $$(NDK_$(1)_STATIC_LIBRARIES)),$$(eval $$(call add-a-name,$$(module))))
$$(foreach module,$$(filter-out $$(SYSTEM_LIBRARIES), $$(NDK_$(1)_SHARED_LIBRARIES)),$$(eval $$(call add-so-name,$$(module))))
endif
endef

# Figure out includes from dependencies as well.
NDK_INCLUDES := $(LOCAL_EXPORT_C_INCLUDE_DIRS) $(LOCAL_EXPORT_C_INCLUDES)

define add-includes
ifeq ($$(findstring $$(NDK_$(1)_EXPORT_INCLUDES),$$(NDK_INCLUDES)),)
NDK_INCLUDES += $$(NDK_$(1)_EXPORT_INCLUDES)

$$(foreach module,$$(filter-out $$(SYSTEM_LIBRARIES), $$(NDK_$(1)_SHARED_LIBRARIES)) $$(NDK_$(1)_STATIC_LIBRARIES),$$(eval $$(call add-includes,$$(module))))
endif
endef

# Resolve additional dependencies based on LOCAL_STATIC_LIBRARIES and
# LOCAL_SHARED_LIBRARIES.

SYSTEM_LIBRARIES = z libz libc c libdl dl libstdc++ stdc++ stlport libstlport gnustl libgnustl c++ libc++ log liblog android libandroid

$(foreach module,$(filter-out $(SYSTEM_LIBRARIES), $(LOCAL_STATIC_LIBRARIES) $(LOCAL_WHOLE_STATIC_LIBRARIES)),$(eval $(call add-a-name,$(module))))
$(foreach module,$(filter-out $(SYSTEM_LIBRARIES), $(LOCAL_SHARED_LIBRARIES)),$(eval $(call add-so-name,$(module))))
$(foreach module,$(filter-out $(SYSTEM_LIBRARIES), $(LOCAL_SHARED_LIBRARIES) $(LOCAL_STATIC_LIBRARIES) $(LOCAL_WHOLE_LIBRARIES)),$(eval $(call add-includes,$(module))))

ifneq ($(findstring stdc++,$(LOCAL_SHARED_LIBRARIES)),)
NDK_CXX_FLAG_$(LOCAL_MODULE) := yes
endif

$(info $(foreach dir,$(NDK_INCLUDES),-I$(dir)))
$(info $(LOCAL_EXPORT_CFLAGS))
$(info $(LOCAL_EXPORT_LDFLAGS) $(abspath $(addprefix $(NDK_BUILD_DIR:%/=%)/,$(NDK_A_NAMES))) $(and $(NDK_SO_NAMES), -L$(abspath $(NDK_BUILD_DIR)) $(foreach soname,$(NDK_SO_NAMES),-l:$(soname))))
$(info $(NDK_A_NAMES) $(NDK_SO_NAMES))
$(info $(NDK_CXX_FLAG_$(LOCAL_MODULE)))
$(info End)
