# ndk-build-helper-3.mk -- Helper for ndk-build.m4.
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
build_kind = executable

$(info Building $(build_kind))
$(info $(LOCAL_MODULE))
$(info $(addprefix $(ANDROID_MODULE_DIRECTORY),$(LOCAL_SRC_FILES) $(LOCAL_SRC_FILES$(EMACS_ABI))))

$(info $(foreach dir,$(LOCAL_EXPORT_C_INCLUDE_DIRS) $(LOCAL_EXPORT_C_INCLUDES),-I$(dir)))
$(info $(LOCAL_EXPORT_CFLAGS))
$(info $(LOCAL_EXPORT_LDFLAGS))
$(info End)
