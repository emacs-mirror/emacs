dnl Copyright (C) 2023 Free Software Foundation, Inc.
dnl This file is part of GNU Emacs.

dnl GNU Emacs is free software: you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or
dnl (at your option) any later version.

dnl GNU Emacs is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.

dnl You should have received a copy of the GNU General Public License
dnl along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

# Support for building Emacs with dependencies using the Android NDK
# build system.

AC_ARG_WITH([ndk_path],
  [AS_HELP_STRING([--with-ndk-path],
    [find Android libraries in these directories])])

AC_ARG_WITH([ndk_cxx_shared],
  [AS_HELP_STRING([--with-ndk-cxx-shared],
    [name of the C++ standard library included with the NDK])])

# ndk_INIT(ABI, API, DIR)
# --------
# Initialize the Android NDK.  ABI is the ABI being built for.
# API is the API version being built for.
# As a side effect, set the variable ndk_INITIALIZED to true.
# DIR should be a directory containing the Makefile.in actually
# implementing the Android NDK build system.

AC_DEFUN_ONCE([ndk_INIT],
[
# Look for Android.mk files.
ndk_module_files=
for file in $with_ndk_path; do
  if test -f $file/Android.mk; then
    ndk_module_files="$ndk_module_files$file/Android.mk "
  fi
done

ndk_ABI=$1
ndk_MODULES=
ndk_MAKEFILES=
ndk_INITIALIZED=yes
ndk_API=$2
ndk_DIR=$3
ndk_ANY_CXX=

case "$ndk_ABI" in
 *arm64* )
   ndk_ARCH=arm64
   ;;
 *arm* )
   ndk_ARCH=arm
   ;;
 *x86_64* )
   ndk_ARCH=x86_64
   ;;
 *x86* )
   ndk_ARCH=x86
   ;;
 * )
   AC_MSG_ERROR([Failed to determine Android device architecture])
   ;;
esac

# This is a map between pkg-config style package names and Android
# ones.

ndk_package_map="libwebpdemux:webpdemux libxml-2.0:libxml2 jansson:libjansson"
ndk_package_map="$ndk_package_map sqlite3:libsqlite_static_minimal"

# Replace ndk_module with the appropriate Android module name if it is
# found in ndk_package_map.

ndk_replace_pkg_config_package () {
  for ndk_stuff in $ndk_package_map; do
    ndk_key=${ndk_stuff%%:*}
    ndk_value=${ndk_stuff#*:}

    if test "$ndk_key" = "$ndk_module"; then
      ndk_module="$ndk_value"
      break
    fi
  done
}

# Parse a pkg-config style list of modules.  Place the resulting list
# in ndk_modules.

ndk_parse_pkg_config_string () {
  ndk_input=[$]1
  ndk_modules=
  while test -n "$ndk_input"; do
    ndk_str=$(printf "$ndk_input" | cut -f1 -d' ')
    ndk_input="$(printf "$ndk_input" | cut -s -f2- -d' ')"

    if test "$ndk_str" = ">=" || test "$ndk_str" = "<=" \
      || test "$ndk_str" = ">" || test "$ndk_str" = "<"; then
      ndk_input="$(printf "$ndk_input" | cut -s -f2- -d' ')"
    else
      ndk_modules="$ndk_modules$ndk_str "
    fi
  done
}

# Resolve $1, a single import.  Prepend its makefile to ndk_MAKEFILES
# if found.  Also, prepend all includes to the variable
# ndk_import_includes.

ndk_resolve_import_module () {
  module_name=
  ndk_module=[$]1

  AC_MSG_CHECKING([for imported $ndk_module])

  for ndk_android_mk in $ndk_module_files; do
    # Read this Android.mk file.  Set NDK_ROOT to /tmp: the Android in
    # tree build system sets it to a meaning value, but build files
    # just use it to test whether or not the NDK is being used.
    ndk_commands=$($MAKE -s -f build-aux/ndk-build-helper.mk EMACS_SRCDIR=.  \
		   EMACS_ABI=$ndk_ABI ANDROID_MAKEFILE="$ndk_android_mk"     \
		   ANDROID_MODULE_DIRECTORY=$(dirname "$ndk_android_mk")     \
		   NDK_BUILD_DIR="$ndk_DIR" NDK_ROOT="/tmp"		     \
		   | awk -f build-aux/ndk-module-extract.awk 		     \
		   MODULE="$ndk_module")

    AS_IF([test -n "${ndk_commands//\n }"], [eval "$ndk_commands"])

    if test -n "$module_name"; then
      break;
    fi
  done

  if test -z "$module_name"; then
    AC_MSG_RESULT([no])
    AC_MSG_ERROR([The module currently being built depends on [$]1, but \
that could not be found in the list of directories specified in \
`--with-ndk-path'.])
  fi

  if test -n "$module_cxx_deps"; then
    ndk_ANY_CXX=yes
  fi

  if test "$ndk_ANY_CXX" = "yes" && test -z "$with_ndk_cxx_shared"; then
    AC_MSG_ERROR([The module [$]1 requires the C++ standard library \
(libc++_shared.so), but it was not found.])
  fi

  AC_MSG_RESULT([yes])

  # Make sure the module is prepended.
  ndk_MAKEFILES="$ndk_android_mk $ndk_MAKEFILES"
  ndk_import_includes="$module_includes $ndk_import_includes"

  # Now recursively resolve this module's imports.
  for ndk_module in $module_imports; do
    ndk_resolve_import_module $ndk_module
  done
}

# Look for a suitable ar in the same directory as the C compiler.
ndk_where_cc=$(which $(echo "$CC" | awk -- "{ print \[$]1 }"))
ndk_ar_search_path=$PATH

# First, try to find $host_alias-ar in PATH.
AC_PATH_PROGS([AR], [$host_alias-ar], [], [$ndk_ar_search_path])

if test -z "$AR"; then
  # Next, try finding either that or llvm-ar in the directory holding
  # CC.
  ndk_ar_search_path="$(dirname $ndk_where_cc):$ndk_ar_search_path"
  AC_PATH_PROGS([AR], [$host_alias-ar llvm-ar], [], [$ndk_ar_search_path])
fi

NDK_BUILD_NASM=

# Next, try to find nasm on x86.  This doesn't ship with the NDK.
if test "$ndk_ARCH" = "x86" || test "$ndk_ARCH" = "x86_64"; then
  AC_CHECK_PROGS([NDK_BUILD_NASM], [nasm])
fi

# Look for a file named ``libc++_shared.so'' in a subdirectory of
# $ndk_where_cc if it was not specified.
AC_MSG_CHECKING([for libc++_shared.so])

ndk_where_toolchain=
if test -z "$with_ndk_cxx_shared" && test -n "$ndk_where_cc"; then
  # Find the NDK root directory.  Go to $ndk_where_cc.
  SAVE_PWD=`pwd`
  cd $(dirname "$ndk_where_cc")

  # Now, keep moving backwards until pwd ends with ``toolchains''.
  while :; do
    if test "`pwd`" = "/"; then
      cd "$SAVE_PWD"
      break
    fi

    if test "`basename $(pwd)`" = "toolchains"; then
      ndk_where_toolchain=`pwd`
      cd "$SAVE_PWD"
      break
    fi

    cd ..
  done

  ndk_matching_libcxx_shared_so=

  # The toolchain directory should be in "$ndk_where_toolchain".
  if test -n "$ndk_where_toolchain"; then
    # Now, look in the directory behind it.
    ndk_cxx_shared_so=`find "$ndk_where_toolchain" -name libc++_shared.so`

    # Look for one with the correct architecture.
    for ndk_candidate in $ndk_cxx_shared_so; do
      case "$ndk_candidate" in
        *arm-linux-android* )
	  if test "$ndk_ARCH" = "arm"; then
	    ndk_matching_libcxx_shared_so=$ndk_candidate
	  fi
	  ;;
	*aarch64-linux-android* )
	  if test "$ndk_ARCH" = "arm64"; then
	    ndk_matching_libcxx_shared_so=$ndk_candidate
	  fi
	  ;;
	*i[[3-6]]86-linux-android* )
	  if test "$ndk_ARCH" = "x86"; then
	    ndk_matching_libcxx_shared_so=$ndk_candidate
	  fi
	  ;;
	*x86_64-linux-android* )
	  if test "$ndk_ARCH" = "x86_64"; then
	    ndk_matching_libcxx_shared_so=$ndk_candidate
	  fi
	  ;;
      esac

      if test -n "$ndk_matching_libcxx_shared_so"; then
        with_ndk_cxx_shared=$ndk_matching_libcxx_shared_so
      fi
    done
  fi
fi

if test -z "$with_ndk_cxx_shared"; then
  AC_MSG_RESULT([no])
  AC_MSG_WARN([The C++ standard library could not be found.  \
If you try to build Emacs with a dependency that requires the C++ standard \
library, Emacs will not build correctly, unless you manually specify the \
name of an appropriate ``libc++_shared.so'' binary.])
else
  AC_MSG_RESULT([$with_ndk_cxx_shared])
fi

ndk_CXX_SHARED=$with_ndk_cxx_shared

# These variables have now been found.
])

# ndk_SEARCH_MODULE(MODULE, NAME, ACTION-IF-FOUND, [ACTION-IF-NOT-FOUND])
# -----------------------------------------------------------------------
# Search for a module named MODULE in `with_ndk_path'.  Add the file
# name of the module's Android.mk file to the variable ndk_MAKEFILES.
# Set NAME_CFLAGS and NAME_LIBS to the appropriate values.  Then, call
# ACTION-IF-FOUND, or ACTION-IF-NOT-FOUND upon failure.
#
# Resolve any imports specified by MODULE, and expand AC_MSG_ERROR
# with a suitable error message if imports were not found.
AC_DEFUN([ndk_SEARCH_MODULE],
[
module_name=
ndk_module=$1
ndk_replace_pkg_config_package
AC_MSG_CHECKING([for Android.mk that builds $ndk_module])

for ndk_android_mk in $ndk_module_files; do
  # Read this Android.mk file.  Set NDK_ROOT to /tmp: the Android in
  # tree build system sets it to a meaning value, but build files just
  # use it to test whether or not the NDK is being used.
  ndk_commands=$($MAKE -s -f build-aux/ndk-build-helper.mk EMACS_SRCDIR=.    \
		 EMACS_ABI=$ndk_ABI ANDROID_MAKEFILE="$ndk_android_mk"       \
	         ANDROID_MODULE_DIRECTORY=$(dirname "$ndk_android_mk")       \
	         NDK_BUILD_DIR="$ndk_DIR" NDK_ROOT="/tmp"		     \
	         | awk -f build-aux/ndk-module-extract.awk 		     \
		 MODULE="$ndk_module")

  AS_IF([test -n "${ndk_commands//\n }"], [eval "$ndk_commands"])

  if test -n "$module_name"; then
    break
  fi
done

if test -z "$module_name"; then
  AC_MSG_RESULT([no])
  $4
else
  if test -n "$module_cxx_deps"; then
    ndk_ANY_CXX=yes
  fi

  if test "$ndk_ANY_CXX" = "yes" && test -z "$with_ndk_cxx_shared"; then
    AC_MSG_ERROR([The module $1 requires the C++ standard library \
(libc++_shared.so), but it was not found.])
  fi

  $2[]_CFLAGS="[$]$2[]_CFLAGS $module_cflags $module_includes"
  $2[]_LIBS="[$]$2[]_LIBS $module_ldflags"
  ndk_MAKEFILES="$ndk_MAKEFILES $ndk_android_mk"
  ndk_MODULES="$ndk_MODULES $module_target"
  AC_MSG_RESULT([yes])
  $3

  # Now, resolve imports.  Make sure the imports' Makefiles comes
  # before ndk_MAKEFILES; likewise for its includes.
  ndk_import_includes=
  for ndk_module in $module_imports; do
    ndk_resolve_import_module $ndk_module
    $2[]_CFLAGS="$ndk_import_includes [$]$2[]_CFLAGS"
  done
fi
])

# ndk_CHECK_MODULES(VARIABLE-PREFIX, MODULES, [ACTION-IF-FOUND],
#   [ACTION-IF-NOT-FOUND])
# --------------------------------------------------------------
# Just like `PKG_CHECK_MODULES'.  However, it uses the ndk-build
# system instead.

AC_DEFUN([ndk_CHECK_MODULES],
[
  ndk_modules=
  ndk_parse_pkg_config_string "$2"
  ndk_found=no

  for module in $ndk_modules; do
    ndk_SEARCH_MODULE([$module], [$1], [ndk_found=yes], [ndk_found=no])
  done

  AS_IF([test "$ndk_found" = "yes"],[$3],[$4])
])

# ndk_CONFIG_FILES
# -------------------------------------------------------------
# Write out the NDK build Makefile with the appropriate variables
# set if the NDK has been initialized.

AC_DEFUN_ONCE([ndk_CONFIG_FILES],
[
  if test "$ndk_INITIALIZED" = "yes"; then
    NDK_BUILD_ANDROID_MK="$ndk_MAKEFILES"
    NDK_BUILD_ARCH=$ndk_ARCH
    NDK_BUILD_ABI=$ndk_ABI
    NDK_BUILD_SDK=$ndk_API
    NDK_BUILD_CC=$CC
    NDK_BUILD_AR=$AR
    NDK_BUILD_MODULES="$ndk_MODULES"
    NDK_BUILD_CXX_SHARED="$ndk_CXX_SHARED"
    NDK_BUILD_ANY_CXX_MODULE=$ndk_ANY_CXX

    AC_SUBST([NDK_BUILD_ANDROID_MK])
    AC_SUBST([NDK_BUILD_ARCH])
    AC_SUBST([NDK_BUILD_ABI])
    AC_SUBST([NDK_BUILD_SDK])
    AC_SUBST([NDK_BUILD_CC])
    AC_SUBST([NDK_BUILD_AR])
    AC_SUBST([NDK_BUILD_NASM])
    AC_SUBST([NDK_BUILD_MODULES])
    AC_SUBST([NDK_BUILD_CXX_SHARED])
    AC_SUBST([NDK_BUILD_ANY_CXX_MODULE])

    AC_CONFIG_FILES([$ndk_DIR/Makefile])
    AC_CONFIG_FILES([$ndk_DIR/ndk-build.mk])
  fi
])
