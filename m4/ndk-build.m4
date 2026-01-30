dnl Copyright (C) 2023-2026 Free Software Foundation, Inc.
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

AC_ARG_WITH([ndk_cxx],
  [AS_HELP_STRING([--with-ndk-cxx],
    [name of the C++ compiler included with the NDK])])

# ndk_INIT(ABI, API, DIR, CFLAGS)
# -------------------------------
# Initialize the Android NDK.  ABI is the ABI being built for.
# API is the API version being built for.
# CFLAGS is a list of compiler flags.
# As a side effect, set the variable ndk_INITIALIZED to true.
# DIR should be a directory containing the Makefile.in actually
# implementing the Android NDK build system.

AC_DEFUN([ndk_INIT],
[
# Look for Android.mk files.
ndk_module_files=
for file in $with_ndk_path; do
  if test -f $file/Android.mk; then
    ndk_module_files="$ndk_module_files$file/Android.mk "
  fi
done

AC_REQUIRE_AUX_FILE([ndk-build-helper.mk])
m4_if(m4_version_compare(m4_defn([AC_AUTOCONF_VERSION]), [2.70]), [-1],
  dnl $ac_aux_dir is an internal variable in 2.69 that
  dnl is not guaranteed to be terminated with a separator character.
  [AS_CASE([$ac_aux_dir],
    [*/], [ndk_AUX_DIR=$ac_aux_dir],
    [ndk_AUX_DIR=$ac_aux_dir/])],
  [ndk_AUX_DIR=$ac_aux_dir])
ndk_ABI=$1
ndk_MODULES=
ndk_MAKEFILES=
ndk_INITIALIZED=yes
ndk_API=$2
ndk_DIR=$3
ndk_ANY_CXX=
ndk_BUILD_CFLAGS="$4"
ndk_working_cxx=no
ndk_CXX_SHARED=
ndk_BUILD_SO_LDFLAGS=
ndk_want_16k_page_sizes=no

AS_CASE(["$ndk_ABI"],
  [*arm64*], [ndk_ARCH=arm64; ndk_want_16k_page_sizes=yes],
  [*arm*], [ndk_ARCH=arm],
  [*x86_64*], [ndk_ARCH=x86_64; ndk_want_16k_page_sizes=yes],
  [*x86*], [ndk_ARCH=x86],
  [*mips64*], [ndk_ARCH=mips64],
  [*mips*], [ndk_ARCH=mips],
  [AC_MSG_ERROR([Failed to determine Android device architecture])])

# This is a map between pkg-config style package names and Android
# ones.

ndk_package_map="libwebpdemux:webpdemux libwebp:webp libxml-2.0:libxml2"
ndk_package_map="$ndk_package_map sqlite3:libsqlite_static_minimal"
ndk_package_map="$ndk_package_map MagickWand:libmagickwand-7 lcms2:liblcms2"

# Replace ndk_module with the appropriate Android module name if it is
# found in ndk_package_map.

ndk_replace_pkg_config_package () {
  for ndk_stuff in $ndk_package_map; do
    ndk_key=`AS_ECHO([$ndk_stuff]) | cut -d: -f1`
    ndk_value=`AS_ECHO([$ndk_stuff]) | cut -d: -f2`

    if test "$ndk_key" = "$ndk_module"; then
      ndk_module="$ndk_value"
      break
    fi
  done
}

# Run the Makefile helper script for the Android.mk file.

ndk_run_test () {
  # Figure out where the helper Makefile is.
  ndk_build_helper_file="${ndk_AUX_DIR}ndk-build-helper.mk"
  ndk_module_extract_awk="${ndk_AUX_DIR}ndk-module-extract.awk"
  ndk_dir=`AS_DIRNAME([$ndk_android_mk])`

  # Now call Make with the right arguments.
  "$MAKE" -s -f "$ndk_build_helper_file" EMACS_SRCDIR=`pwd`		\
    EMACS_ABI="$ndk_ABI" ANDROID_MAKEFILE="$ndk_android_mk"		\
    NDK_BUILD_DIR="$ndk_DIR" NDK_ROOT="/tmp"				\
    ANDROID_MODULE_DIRECTORY="$ndk_dir" BUILD_AUXDIR=$ndk_AUX_DIR	\
    NDK_BUILD_ARCH="$ndk_ARCH" 2>&AS_MESSAGE_LOG_FD >conftest.ndk

  # Read the output.
  cat conftest.ndk | awk -f "$ndk_module_extract_awk" MODULE="$ndk_module"

  # Remove the temporary file.
  rm -f conftest.ndk
}

# ndk_parse_pkg_config_string PKG_CONFIG_STRING
# ---------------------------------------------
# Parse a pkg-config style list of modules.  Place the resulting list
# in ndk_modules.

ndk_parse_pkg_config_string () {
  ndk_input=[$]1
  ndk_modules=
  while test -n "$ndk_input"; do
    ndk_str=`AS_ECHO_N(["$ndk_input"]) | cut -f1 -d' '`
    ndk_input=`AS_ECHO_N(["$ndk_input"]) | cut -s -f2- -d' '`

    if test "$ndk_str" = ">=" || test "$ndk_str" = "<=" \
      || test "$ndk_str" = ">" || test "$ndk_str" = "<" \
      || test "$ndk_str" = "!="; then
      ndk_input=`AS_ECHO_N(["$ndk_input"]) | cut -s -f2- -d' '`
    else
      ndk_modules="$ndk_modules$ndk_str "
    fi
  done
}

# ndk_resolve_import_module MODULE
# --------------------------------
# Resolve MODULE, a single import.  Prepend its makefile to
# ndk_MAKEFILES if found.  Also, prepend all includes to the variable
# ndk_import_includes.

ndk_resolve_import_module () {
  module_name=
  ndk_module=[$]1

  AC_MSG_CHECKING([for imported $ndk_module])
  AC_CACHE_VAL([AS_TR_SH([ndk_cv_commands_$ndk_module])],
    [for ndk_android_mk in $ndk_module_files; do
       # Read this Android.mk file.  Set NDK_ROOT to /tmp: the Android in
       # tree build system sets it to a meaningful value, but build files
       # just use it to test whether or not the NDK is being used.
       ndk_commands=`ndk_run_test`
       eval "$ndk_commands"

       if test -n "$module_name"; then
         # Guarantee that evaluation of the cached value will also set
	 # `ndk_android_mk'.
         ndk_commands="$ndk_commands ndk_android_mk=$ndk_android_mk"
	 break;
       fi
     done
     AS_IF([test -z "$module_name"],
       [AS_VAR_SET([AS_TR_SH([ndk_cv_commands_$ndk_module])],
	 [""])],
       [AS_VAR_SET([AS_TR_SH([ndk_cv_commands_$ndk_module])],
	 [$ndk_commands])])])

  # Copy the computed value into ndk_commands.
  AS_VAR_COPY([ndk_commands], [AS_TR_SH([ndk_cv_commands_$ndk_module])])
  eval "$ndk_commands"

  # Print the outcome of the test.
  AS_IF([test -n "$module_name"], [AC_MSG_RESULT([yes])],
    [AC_MSG_RESULT([no])
     AC_MSG_ERROR([The module currently being built has imported [$]1, but \
that could not be found in the list of directories specified in \
`--with-ndk-path'.])])

  if test -n "$module_cxx_deps"; then
    ndk_ANY_CXX=yes
  fi

  AS_IF([test "$module_cxx_deps" = "yes" && test -z "$ndk_CXX_STL" \
         && test -z "$ndk_CXX_LDFLAGS"],
    [AC_MSG_ERROR([The module $1 requires a C++ standard library,
but none were found.])])

  AS_IF([test "$module_cxx_deps" = "yes" && test "$ndk_working_cxx" != "yes"],
    [AC_MSG_ERROR([The module [$]1 requires the C++ standard library,
but a working C++ compiler was not found.])])

  # Make sure the module is prepended.
  ndk_MODULES="$ndk_MODULES $module_target"
  ndk_MAKEFILES="$ndk_android_mk $ndk_MAKEFILES"
  ndk_import_includes="$module_includes $ndk_import_includes"

  # Now recursively resolve this module's imports.
  for ndk_module in $module_imports; do
    ndk_resolve_import_module $ndk_module
  done
}

# ndk_filter_cc_for_cxx
# ---------------------
# Run through $CC, removing any options that are not suitable for
# use in a C++ compiler.

ndk_filter_cc_for_cxx () {
  for ndk_word in $CC; do
    AS_CASE([$ndk_word], [*-std=*], [],
      [AS_ECHO_N(["$ndk_word "])])
  done
}

# ndk_subst_cc_onto_cxx
# ---------------------
# Print the value of $CXX, followed by any innocent looking options
# in $CC.

ndk_subst_cc_onto_cxx () {
  AS_ECHO_N(["$CXX "])
  ndk_flag=
  for ndk_word in `AS_ECHO_N(["$CC"]) | cut -s -f2- -d' '`; do
    AS_IF([test "$ndk_flag" = "yes"],
      [AS_ECHO_N(["$ndk_word "])
       ndk_flag=no],
      [AS_CASE([$ndk_word],
        [*-sysroot=*],
          [AS_ECHO_N(["$ndk_word "])],
	[*-isystem*],
          [AS_ECHO_N(["$ndk_word "])
	   ndk_flag=yes],
	[*-sysroot*],
	  [AS_ECHO_N(["$ndk_word "])
	   ndk_flag=yes],
	[-D__ANDROID_API__*],
	  [AS_ECHO_N(["$ndk_word "])])])
  done
}

# ndk_subst_cflags_onto_cxx
# ---------------------
# Print any options in CFLAGS also suitable for a C++ compiler.

ndk_subst_cflags_onto_cxx () {
  ndk_flag=
  for ndk_word in $CFLAGS; do
    AS_IF([test "$ndk_flag" = "yes"],
      [AS_ECHO_N(["$ndk_word "])
       ndk_flag=no],
      [AS_CASE([$ndk_word],
        [*-sysroot=*],
          [AS_ECHO_N(["$ndk_word "])],
	[*-isystem*],
          [AS_ECHO_N(["$ndk_word "])
	   ndk_flag=yes],
	[*-I*],
          [AS_ECHO_N(["$ndk_word "])
	   ndk_flag=yes],
	[*-sysroot*],
	  [AS_ECHO_N(["$ndk_word "])
	   ndk_flag=yes],
	[-D__ANDROID_API__*],
	  [AS_ECHO_N(["$ndk_word "])])])
  done
}

# Detect the installation directory and type of the NDK being used.

ndk_install_dir=
ndk_toolchain_type=

AC_MSG_CHECKING([for the directory where the NDK is installed])

dnl If the install directory isn't available, repeat the search over
dnl each entry in the programs directory.
ndk_programs_dirs=`$CC -print-search-dirs | sed -n "s/^programs:[[\t ]]*=\?\(.*\)/\1/p"`
ndk_save_IFS=$IFS; IFS=:
for ndk_dir in $ndk_programs_dirs; do
   if test -d "$ndk_dir"; then :; else
     continue
   fi
   ndk_dir=`cd "$ndk_dir"; pwd`
   while test "$ndk_dir" != "/" && test -z "$ndk_toolchain_type"; do
     ndk_dir=`AS_DIRNAME([$ndk_dir])`
     AS_IF([test -d "$ndk_dir/bin" && test -d "$ndk_dir/lib"],
      [dnl The directory reached is most likely either the directory
       dnl holding prebuilt binaries in a combined toolchain or the
       dnl directory holding a standalone toolchain itself.
       dnl
       dnl Distinguish between the two by verifying the name of the
       dnl parent directory (and its parent).
       ndk_dir1=`AS_DIRNAME(["$ndk_dir"])`
       ndk_basename=`AS_BASENAME(["$ndk_dir1"])`
       AS_IF([test "$ndk_basename" = "prebuilt"],
	 [dnl Directories named "prebuilt" are exclusively present in
	  dnl combined toolchains, where they are children of the
	  dnl base directory or, in recent releases, a directory
	  dnl within the base directory.  Continue searching for the
	  dnl base directory.
	  ndk_toolchain_type=combined
	  while test "$ndk_dir1" != "/"; do
	    AS_IF([test -d "$ndk_dir1/toolchains" \
		   && test -d "$ndk_dir1/sources"],
	      [ndk_install_dir=$ndk_dir1
	       break])
	    ndk_dir1=`AS_DIRNAME(["$ndk_dir1"])`
	  done],
	 [ndk_toolchain_type=standalone
	  ndk_install_dir=$ndk_dir])])
   done
   AS_IF([test -n "$ndk_toolchain_type"],
     [break])
done
IFS=$ndk_save_IFS

AS_IF([test -z "$ndk_install_dir"],
  [AC_MSG_RESULT([unknown])
   AC_MSG_WARN([The NDK installation directory could not be \
derived from the compiler.])],
  [AC_MSG_RESULT([$ndk_install_dir ($ndk_toolchain_type)])])

# Look for a suitable ar and ranlib in the same directory as the C
# compiler.
ndk_cc_firstword=`AS_ECHO(["$CC"]) | cut -d' ' -f1`
ndk_where_cc=`which $ndk_cc_firstword`
ndk_ar_search_path=$PATH
ndk_ranlib_search_path=$RANLIB

# First, try to find $host_alias-ar in PATH.
AC_PATH_PROGS([AR], [$host_alias-ar], [], [$ndk_ar_search_path])

AS_IF([test -z "$AR"],[
  # Next, try finding either that or llvm-ar in the directory holding
  # CC.
  ndk_ar_search_path="`AS_DIRNAME([$ndk_where_cc])`:$ndk_ar_search_path"
  AC_PATH_PROGS([AR], [$host_alias-ar llvm-ar], [], [$ndk_ar_search_path])])

# First, try to find $host_alias-ranlib in PATH.
AC_PATH_PROGS([RANLIB], [$host_alias-ranlib], [], [$ndk_ranlib_search_path])

AS_IF([test -z "$RANLIB"],[
  # Next, try finding either that or llvm-ranlib in the directory
  # holding CC.
  ndk_ranlib_search_path="`AS_DIRNAME([$ndk_where_cc])`:$ndk_ranlib_search_path"
  AC_PATH_PROGS([RANLIB], [$host_alias-ranlib llvm-ranlib], [],
    [$ndk_ranlib_search_path])])

NDK_BUILD_NASM=

# Next, try to find nasm on x86.  This doesn't ship with the NDK.
AS_IF([test "$ndk_ARCH" = "x86" || test "$ndk_ARCH" = "x86_64"],
  [AC_CHECK_PROGS([NDK_BUILD_NASM], [nasm])])

# Search for a suitable readelf binary, which is required to generate
# the shared library list loaded on old Android systems.
AC_PATH_PROGS([READELF], [readelf llvm-readelf $host_alias-readelf],
  [], [$ndk_ranlib_search_path:$PATH])
AS_IF([test -z "$READELF"],
  [AC_MSG_ERROR([A suitable `readelf' utility cannot be located.
Please verify that the Android NDK has been installed correctly,
or install a functioning `readelf' yourself.])])
NDK_BUILD_READELF="$READELF"

# Search for a C++ compiler.  Upon failure, pretend the C compiler is a
# C++ compiler and use that instead.

ndk_cc_name=`AS_BASENAME(["${ndk_cc_firstword}"])`
ndk_cxx_name=

AS_CASE([$ndk_cc_name], [*-gcc],
  [ndk_cxx_name=`AS_ECHO([$ndk_cc_name]) | sed 's/gcc/g++/'`],
  [ndk_cxx_name="${ndk_cc_name}++"])

AS_IF([test -n "$with_ndk_cxx"], [CXX=$with_ndk_cxx],
  [AC_PATH_PROGS([CXX], [$ndk_cxx_name],
     [], [`AS_DIRNAME(["$ndk_where_cc"])`:$PATH])
   AS_IF([test -z "$CXX"], [CXX=`ndk_filter_cc_for_cxx`],
     [CXX=`ndk_subst_cc_onto_cxx`])])

# None of the C++ standard libraries installed with Android are
# available to NDK programs, which are expected to select one of several
# standard libraries distributed with the NDK.  This library must be
# extracted from the NDK by the program's build system and copied into
# the application directory, and the build system is also expected to
# provide the compiler with suitable options to enable it.
#
# Emacs, on recent releases of the NDK, prefers the libc++ library, the
# most complete of the libraries available, when it detects the presence
# of its headers and libraries in the compiler's search path.  Next in
# line are the several libraries located in a directory named `cxx-stl'
# inside the NDK distribution, of which Emacs prefers, in this order,
# the GNU libstdc++, stlport, gabi and the system C++ library.  The
# scope of the last two is confined to providing runtime support for
# basic C++ operations, and is useless for compiling most C++
# dependencies whose requirements go beyond such operations.
#
# The NDK comes in two forms.  In a "combined toolchain", all C++
# libraries are present in the NDK directory and the responsibility is
# left to the build system to locate and select the best C++ library,
# whereas in a "standalone toolchain" an STL will have already been
# specified a C++ library, besides which no others will be present.
#
# Though Android.mk files are provided by the NDK for each such library,
# Emacs cannot use any of these, both for lack of prebuilt support in
# its ndk-build implementation, and since they are absent from combined
# toolchains.

ndk_CXX_SHARED=
ndk_CXX_STL=
ndk_CXX_LDFLAGS=

AS_IF([test -n "$CXX" && test -n "$ndk_install_dir"],
  [ndk_library_dirs=`$CXX -print-search-dirs \
                      | sed -n "s/^libraries:[[\t ]]*=\?\(.*\)/\1/p"`
   AS_IF([test "$ndk_toolchain_type" = "standalone"],
    [dnl With a standalone toolchain, just use the first C++ library
     dnl present in the compiler's library search path, that being the
     dnl only C++ library that will ever be present.
     ndk_save_IFS=$IFS; IFS=:
     for ndk_dir in $ndk_library_dirs; do
       if test -d "$ndk_dir"; then :; else
	 continue
       fi
       ndk_dir=`cd "$ndk_dir"; pwd`
       if test -f "$ndk_dir/libc++_shared.so"; then
         ndk_CXX_SHARED="$ndk_dir/libc++_shared.so"
         ndk_CXX_LDFLAGS=-lc++_shared; break
       elif test -f "$ndk_dir/libgnustl_shared.so"; then
         ndk_CXX_SHARED="$ndk_dir/libgnustl_shared.so"
         ndk_CXX_LDFLAGS=-lgnustl_shared; break
       elif test -f "$ndk_dir/libstlport_shared.so"; then
         ndk_CXX_SHARED="$ndk_dir/libstlport_shared.so"
	 ndk_CXX_LDFLAGS=-lstlport_shared; break
       fi
     done
     IFS=$ndk_save_IFS],
    [dnl Otherwise, search for a suitable standard library
     dnl in the order stated above.
     dnl
     dnl Detect if this compiler is configured to link against libc++ by
     dnl default.
     AC_MSG_CHECKING([whether compiler defaults to libc++])
     cat <<_ACEOF >conftest.cc
#include <string>
#ifndef _LIBCPP_VERSION
Not libc++!
#endif /* _LIBCPP_VERSION */

int
main (void)
{

}
_ACEOF
     AS_IF([$CXX conftest.cc -o conftest.o >&AS_MESSAGE_LOG_FD 2>&1],
       [dnl The compiler defaults to libc++.
        AC_MSG_RESULT([yes])
	ndk_save_IFS=$IFS; IFS=:
	for ndk_dir in $ndk_library_dirs; do
	  if test -f "$ndk_dir/libc++_shared.so"; then
	    ndk_CXX_SHARED="$ndk_dir/libc++_shared.so"
	    ndk_CXX_LDFLAGS=-lc++_shared; break
	  fi
	done
	IFS=$ndk_save_IFS],
       [dnl Search for gnustl, stlport, gabi, and failing that, system.
        dnl The name of the gabi system root directory varies by GCC
        dnl version.
        AC_MSG_RESULT([no])
        ndk_gcc_version=`($CXX -v 2>&1) \
	 | sed -n "s/^gcc version \([[0123456789]\+.[0123456789]\+]\).*/\1/p"`
	cxx_stl="$ndk_install_dir/sources/cxx-stl"
	ndk_cxx_stl_base="$cxx_stl/gnu-libstdc++/$ndk_gcc_version"
	AS_IF([test -n "$ndk_gcc_version" \
	       && test -d "$ndk_cxx_stl_base/libs/$ndk_ABI"],
	  [ndk_CXX_LDFLAGS="-L$ndk_cxx_stl_base/libs/$ndk_ABI -lgnustl_shared"
	   ndk_CXX_LDFLAGS="$ndk_CXX_LDFLAGS -lsupc++"
	   ndk_CXX_STL="-isystem $ndk_cxx_stl_base/include"
	   ndk_CXX_STL="$ndk_CXX_STL -isystem $ndk_cxx_stl_base/libs/$ndk_ABI/include"
	   ndk_CXX_SHARED="$ndk_cxx_stl_base/libs/$ndk_ABI/libgnustl_shared.so"])
	AS_IF([test -f "$ndk_CXX_SHARED"], [],
	  [dnl No STL was located or the library is not reachable.
	   dnl Search for alternatives.
	   ndk_CXX_STL=
	   ndk_CXX_SHARED=
	   ndk_CXX_LDFLAGS=
	   ndk_cxx_stl_base="$cxx_stl/stlport"
	   AS_IF([test -d "$ndk_cxx_stl_base"],
	     [ndk_CXX_LDFLAGS="-L$ndk_cxx_stl_base/libs/$ndk_ABI -lstlport_shared"
	      ndk_CXX_STL="-isystem $ndk_cxx_stl_base/stlport"
	      ndk_CXX_SHARED="$ndk_cxx_stl_base/libs/$ndk_ABI/libstlport_shared.so"])
           AS_IF([test -f "$ndk_CXX_SHARED"], [],
	     [ndk_CXX_STL=
	      ndk_CXX_SHARED=
	      ndk_CXX_LDFLAGS=
	      ndk_cxx_stl_base="$cxx_stl/gabi++"
	      AS_IF([test -d "$ndk_cxx_stl_base"],
	       [ndk_CXX_LDFLAGS="-L$ndk_cxx_stl_base/libs/$ndk_ABI -lgabi++_shared"
	        ndk_CXX_STL="$ndk_CXX_STL -isystem $ndk_cxx_stl_base/include"
	        ndk_CXX_SHARED="$ndk_cxx_stl_base/libs/$ndk_ABI/lgabi++_shared.so"])])
           AS_IF([test -f "$ndk_CXX_SHARED"], [],
	     [ndk_CXX_STL=
	      ndk_CXX_SHARED=
	      ndk_CXX_LDFLAGS=
	      ndk_cxx_stl_base="$cxx_stl/system"
	      AS_IF([test -d "$ndk_cxx_stl_base"],
	       [ndk_CXX_LDFLAGS="-L$ndk_cxx_stl_base/libs/$ndk_ABI -lstdc++"
                ndk_CXX_STL="-isystem $ndk_cxx_stl_base/include"
	        dnl The "system" library is distributed with Android and
	        dnl need not be present in app packages.
	        ndk_CXX_SHARED=
		dnl Done.
		])])])])
     rm -f conftest.o])])

AS_ECHO([])
AS_ECHO(["C++ compiler configuration: "])
AS_ECHO([])
AS_ECHO(["Library includes        : $ndk_CXX_STL"])
AS_ECHO(["Linker options          : $ndk_CXX_LDFLAGS"])
AS_ECHO(["Library file (if any)   : $ndk_CXX_SHARED"])
AS_ECHO([])])

# ndk_LATE_EARLY
# --------------
# Call before ndk_LATE to establish certain variables in time for
# ndk_LATE's C++ compiler detection.

AC_DEFUN([ndk_LATE_EARLY],
[ndk_save_LDFLAGS="$LDFLAGS"
 LDFLAGS="$LDFLAGS $ndk_CXX_LDFLAGS"
 CXXFLAGS="$CXXFLAGS `ndk_subst_cflags_onto_cxx` $ndk_CXX_STL"])

# ndk_LATE
# --------
# Perform late initialization of the ndk-build system by checking for
# required C and C++ headers and 16 KB page size support.

AC_DEFUN([ndk_LATE],
[dnl
AS_IF([test "$ndk_INITIALIZED" = "yes"],[
  AS_IF([test -n "$CXX"], [
    AC_LANG_PUSH([C++])
    AC_CHECK_HEADER([string], [ndk_working_cxx=yes],
      [AC_MSG_WARN([Your C++ compiler is not properly configured, as
the standard library headers could not be found.])])
    AC_LANG_POP([C++])])])
LDFLAGS="$ndk_save_LDFLAGS"
dnl Detect whether this version of the NDK supports 16KB page sizes,
dnl which are required on certain architectures to execute under Android
dnl 15 (35) and later, and apply the appropriate linker options if
dnl positive.
AS_IF([test "$ndk_want_16k_page_sizes" = "yes"],
  [AC_CACHE_CHECK([whether toolchain supports configurations with 16k page sizes],
     [ndk_cv_16k_page_sizes],
     [ndk_save_LDFLAGS="$LDFLAGS"
      LDFLAGS="$LDFLAGS -Wl,-z,max-page-size=16384"
      AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])],
        [ndk_cv_16k_page_sizes=yes],
	[ndk_cv_16k_page_sizes=no])
      LDFLAGS="$ndk_save_LDFLAGS"])
   AS_IF([test "$ndk_cv_16k_page_sizes" = "yes"],
     [LDFLAGS="$LDFLAGS -Wl,-z,max-page-size=16384"
      ndk_BUILD_SO_LDFLAGS="-Wl,-z,max-page-size=16384"],
     [AC_MSG_WARN([\
Your toolchain does not support configurations with 16KB page sizes,
and consequently binaries it produces cannot support all devices
running Android 15 or later.])])])
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
AC_MSG_CHECKING([for Android.mk providing $ndk_module])
AC_CACHE_VAL([AS_TR_SH([ndk_cv_commands_$ndk_module])],
  [for ndk_android_mk in $ndk_module_files; do
     # Read this Android.mk file.  Set NDK_ROOT to /tmp: the Android in
     # tree build system sets it to a meaningful value, but build files
     # just use it to test whether or not the NDK is being used.
     ndk_commands=`ndk_run_test`
     eval "$ndk_commands"

     if test -n "$module_name"; then
       # Guarantee that evaluation of the cached value will also set
       # `ndk_android_mk'.
       ndk_commands="$ndk_commands ndk_android_mk=$ndk_android_mk"
       break;
     fi
   done
   AS_IF([test -n "$module_name"],
     [AS_VAR_SET([AS_TR_SH([ndk_cv_commands_$ndk_module])],
       [$ndk_commands])],
     [AS_VAR_SET([AS_TR_SH([ndk_cv_commands_$ndk_module])], [])])])
AS_VAR_COPY([ndk_commands], [AS_TR_SH([ndk_cv_commands_$ndk_module])])
eval "$ndk_commands"

if test -z "$module_name"; then
  AC_MSG_RESULT([no])
  $4
else
  if test -n "$module_cxx_deps"; then
    ndk_ANY_CXX=yes
  fi

  AS_IF([test "$module_cxx_deps" = "yes" && test -z "$ndk_CXX_STL" \
         && test -z "$ndk_CXX_LDFLAGS"],
    [AC_MSG_ERROR([The module $1 requires a C++ standard library,
but none were found.])])

  AS_IF([test "$module_cxx_deps" = "yes" && test "$ndk_working_cxx" != "yes"],
    [AC_MSG_ERROR([The module [$]1 requires the C++ standard library,
but a working C++ compiler was not found.])])

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
    NDK_BUILD_CXX=$CXX
    NDK_BUILD_AR=$AR
    NDK_BUILD_MODULES="$ndk_MODULES"
    NDK_BUILD_CXX_SHARED="$ndk_CXX_SHARED"
    NDK_BUILD_CXX_STL="$ndk_CXX_STL"
    NDK_BUILD_CXX_LDFLAGS="$ndk_CXX_LDFLAGS"
    NDK_BUILD_ANY_CXX_MODULE=$ndk_ANY_CXX
    NDK_BUILD_SO_LDFLAGS="$ndk_BUILD_SO_LDFLAGS"
    NDK_BUILD_CFLAGS="$ndk_BUILD_CFLAGS"

    AC_SUBST([NDK_BUILD_ANDROID_MK])
    AC_SUBST([NDK_BUILD_ARCH])
    AC_SUBST([NDK_BUILD_ABI])
    AC_SUBST([NDK_BUILD_SDK])
    AC_SUBST([NDK_BUILD_CC])
    AC_SUBST([NDK_BUILD_CXX])
    AC_SUBST([NDK_BUILD_AR])
    AC_SUBST([NDK_BUILD_NASM])
    AC_SUBST([NDK_BUILD_MODULES])
    AC_SUBST([NDK_BUILD_CXX_SHARED])
    AC_SUBST([NDK_BUILD_CXX_STL])
    AC_SUBST([NDK_BUILD_CXX_LDFLAGS])
    AC_SUBST([NDK_BUILD_ANY_CXX_MODULE])
    AC_SUBST([NDK_BUILD_SO_LDFLAGS])
    AC_SUBST([NDK_BUILD_CFLAGS])
    AC_SUBST([NDK_BUILD_READELF])

    AC_CONFIG_FILES([$ndk_DIR/Makefile])
    AC_CONFIG_FILES([$ndk_DIR/ndk-build.mk])
  fi
])
