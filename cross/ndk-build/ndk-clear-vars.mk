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

LOCAL_MODULE :=
LOCAL_MODULE_FILENAME :=
LOCAL_SRC_FILES :=
LOCAL_CPP_EXTENSION :=
LOCAL_CPP_FEATURES :=
LOCAL_C_INCLUDES :=
LOCAL_CFLAGS :=
LOCAL_CPPFLAGS :=
LOCAL_STATIC_LIBRARIES :=
LOCAL_SHARED_LIBRARIES :=
LOCAL_WHOLE_STATIC_LIBRARIES :=
LOCAL_LDLIBS :=
LOCAL_LDFLAGS :=
LOCAL_ALLOW_UNDEFINED_SYMBOLS :=
LOCAL_ARM_MODE :=
LOCAL_ARM_NEON :=
LOCAL_DISABLE_FORMAT_STRING_CHECKS :=
LOCAL_EXPORT_CFLAGS :=
LOCAL_EXPORT_CPPFLAGS :=
LOCAL_EXPORT_C_INCLUDES :=
LOCAL_EXPORT_C_INCLUDE_DIRS :=
LOCAL_EXPORT_LDFLAGS :=
LOCAL_EXPORT_LDLIBS :=

# AOSP extensions.
LOCAL_SRC_FILES_$(NDK_BUILD_ARCH) :=
LOCAL_ASFLAGS_$(NDK_BUILD_ARCH) :=
LOCAL_CFLAGS_$(NDK_BUILD_ARCH) :=
LOCAL_ADDITIONAL_DEPENDENCIES :=
LOCAL_CLANG_ASFLAGS_$(NDK_BUILD_ARCH) :=
LOCAL_IS_HOST_MODULE :=

# Emacs extensions!
LOCAL_ASM_RULE_DEFINED :=
LOCAL_ASM_RULE :=
LOCAL_C_ADDITIONAL_FLAGS :=
