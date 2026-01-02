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

eq = $(and $(findstring $(1),$(2)),$(findstring $(2),$(1)))
objname = $(1)-static-$(subst /,_,$(2).o)
maybe-absolute = $(or $(and $(2),$(1)),$(and $(wildcard $(1)),$(1)),$(LOCAL_PATH)/$(1))

define single-object-target

ifeq (x$(suffix $(1)),x.c)

$(call objname,$(LOCAL_MODULE),$(basename $(1))): $(call maybe-absolute,$(1),$(2))
	$(NDK_BUILD_CC) -c $$< -o $$@ $(NDK_BUILD_CFLAGS) $(NDK_CFLAGS_$(LOCAL_MODULE)) $(call LOCAL_C_ADDITIONAL_FLAGS,$(1))

else
ifeq (x$(suffix $(1)),x.$(or $(LOCAL_CPP_EXTENSION),cpp))

$(call objname,$(LOCAL_MODULE),$(basename $(1))): $(call maybe-absolute,$(1),$(2))
	$(NDK_BUILD_CXX) -c $$< -o $$@ $(NDK_BUILD_CFLAGS_CXX) $(NDK_CFLAGS_$(LOCAL_MODULE)) $(NDK_CXXFLAGS_$(LOCAL_MODULE))

else
ifneq ($(or $(call eq,x$(suffix $(1)),x.s),$(call eq,x$(suffix $(1)),x.S)),)

$(call objname,$(LOCAL_MODULE),$(basename $(1))): $(call maybe-absolute,$(1),$(2))
	$(NDK_BUILD_CC) -c $$< -o $$@ $(NDK_ASFLAGS_$(LOCAL_MODULE))

else
ifneq (x$(suffix $(1)),x.asm)
ifeq (x$(suffix $(1)),x.cc)

$(call objname,$(LOCAL_MODULE),$(basename $(1))): $(call maybe-absolute,$(1),$(2))
	$(NDK_BUILD_CXX) -c $$< -o $$@ $(NDK_BUILD_CFLAGS_CXX) $(NDK_CFLAGS_$(LOCAL_MODULE)) $(NDK_CXXFLAGS_$(LOCAL_MODULE))

else
$$(error Unsupported suffix: $(suffix $(1)))
endif
else
ifneq (x$(LOCAL_ASM_RULE_DEFINED),x)
# Call this function to define a rule that will generate $(1) from
# $(2), a ``.asm'' file.  This is an Emacs extension.

$(call LOCAL_ASM_RULE,$(call objname,$(LOCAL_MODULE),$(basename $(1))),$(LOCAL_PATH)/$(strip $(1)))

else
ifeq ($(findstring x86,$(NDK_BUILD_ARCH)),)
$$(error Trying to build nasm file on non-Intel platform!)
else

$(call objname,$(LOCAL_MODULE),$(basename $(1))): $(call maybe-absolute,$(1),$(2))
	$(NDK_BUILD_NASM) -felf$(findstring 64,$(NDK_BUILD_ARCH)) -o $$@ -i $(LOCAL_PATH) -i $$(dir $$<) $(NDK_ASFLAGS_$(LOCAL_MODULE)) $$<

endif
endif
endif
endif
endif
endif

ALL_OBJECT_FILES$(LOCAL_MODULE) += $(call objname,$(LOCAL_MODULE),$(basename $(1)))
endef

define single-neon-target

# Define rules for the target.
$$(eval $$(call single-object-target,$(patsubst %.neon,%,$(1)),))

endef

NDK_CFLAGS_$(LOCAL_MODULE)  := $(addprefix -I,$(LOCAL_C_INCLUDES))
NDK_CFLAGS_$(LOCAL_MODULE)  += -fPIC -iquote $(LOCAL_PATH) $(LOCAL_EXPORT_CFLAGS) $(LOCAL_CFLAGS) $(LOCAL_CFLAGS_$(NDK_BUILD_ARCH))
NDK_ASFLAGS_$(LOCAL_MODULE) := $(LOCAL_ASFLAGS) $(LOCAL_ASFLAGS_$(NDK_BUILD_ARCH)) $(and $(findstring clang,$(NDK_BUILD_CC)),$(LOCAL_CLANG_ASFLAGS_$(NDK_BUILD_ARCH)))
NDK_LDFLAGS_$(LOCAL_MODULE) := $(LOCAL_LDLIBS) $(LOCAL_LDFLAGS)
NDK_CXXFLAGS_$(LOCAL_MODULE) := $(LOCAL_CPPFLAGS) $(LOCAL_RTTI_FLAG)
ALL_OBJECT_FILES$(LOCAL_MODULE) :=

# Now look for features in LOCAL_CPP_FEATURES and enable them.

ifneq ($(findstring exceptions,$(LOCAL_CPPFLAGS)),)
NDK_CXXFLAGS_$(LOCAL_MODULE) += -fexceptions
endif

ifneq ($(findstring rtti,$(LOCAL_CPPFLAGS)),)
NDK_CXXFLAGS_$(LOCAL_MODULE) += -frtti
endif


ifeq ($(NDK_BUILD_ARCH)$(NDK_ARM_MODE),armarm)
NDK_CFLAGS ::= -marm
else
ifeq ($(NDK_BUILD_ARCH),arm)
NDK_CFLAGS ::= -mthumb
endif
endif

ifeq ($(filter-out lib%,$(LOCAL_MODULE)),)
LOCAL_MODULE_FILENAME := $(LOCAL_MODULE)
else
LOCAL_MODULE_FILENAME := lib$(LOCAL_MODULE)
endif

LOCAL_MODULE_FILENAME := $(LOCAL_MODULE_FILENAME).a

# Record this module's dependencies and exported includes and CFLAGS,
# and then add that of its dependencies.

include $(srcdir)/ndk-resolve.mk

# Then define rules to build all objects.
ALL_SOURCE_FILES := $(LOCAL_SRC_FILES) $(LOCAL_SRC_FILES_$(NDK_BUILD_ARCH))

# Now filter out code that is built with neon.  Define rules to build
# those separately.
NEON_SOURCE_FILES := $(filter %.neon,$(ALL_SOURCE_FILES))
ALL_SOURCE_FILES  := $(filter-out %.neon,$(ALL_SOURCE_FILES))

# This defines all dependencies.
ALL_OBJECT_FILES$(LOCAL_MODULE) =

$(foreach source,$(ALL_SOURCE_FILES),$(eval $(call single-object-target,$(source),)))
$(foreach source,$(NEON_SOURCE_FILES),$(eval $(call single-neon-target,$(source),)))

# Now define the rule to build the library.
$(LOCAL_MODULE_FILENAME): $(ALL_OBJECT_FILES$(LOCAL_MODULE))
	$(NDK_BUILD_AR) r $@ $^
