# Check for getloadavg.

# Copyright (C) 1992-1996, 1999-2000, 2002-2003, 2006, 2008-2011 Free Software
# Foundation, Inc.

# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

#serial 2

# Autoconf defines AC_FUNC_GETLOADAVG, but that is obsolescent.
# New applications should use gl_GETLOADAVG instead.

# gl_GETLOADAVG(LIBOBJDIR)
# ------------------------
AC_DEFUN([gl_GETLOADAVG],
[AC_REQUIRE([gl_STDLIB_H_DEFAULTS])

# Persuade glibc <stdlib.h> to declare getloadavg().
AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])

gl_have_func=no # yes means we've found a way to get the load average.

# Make sure getloadavg.c is where it belongs, at configure-time.
test -f "$srcdir/$1/getloadavg.c" ||
  AC_MSG_ERROR([$srcdir/$1/getloadavg.c is missing])

gl_save_LIBS=$LIBS

# Check for getloadavg, but be sure not to touch the cache variable.
(AC_CHECK_FUNC([getloadavg], [exit 0], [exit 1])) && gl_have_func=yes

# On HPUX9, an unprivileged user can get load averages through this function.
AC_CHECK_FUNCS([pstat_getdynamic])

# Solaris has libkstat which does not require root.
AC_CHECK_LIB([kstat], [kstat_open])
test $ac_cv_lib_kstat_kstat_open = yes && gl_have_func=yes

# AIX has libperfstat which does not require root
AC_CHECK_LIB([perfstat], [perfstat_cpu_total])
test $ac_cv_lib_perfstat_perfstat_cpu_total = yes && gl_have_func=yes

# Some systems with -lutil have (and need) -lkvm as well, some do not.
# On Solaris, -lkvm requires nlist from -lelf, so check that first
# to get the right answer into the cache.
# For kstat on solaris, we need to test for libelf and libkvm to force the
# definition of SVR4 below.
if test $gl_have_func = no; then
  AC_CHECK_LIB([elf], [elf_begin], [LIBS="-lelf $LIBS"])
  AC_CHECK_LIB([kvm], [kvm_open], [LIBS="-lkvm $LIBS"])
  # Check for the 4.4BSD definition of getloadavg.
  AC_CHECK_LIB([util], [getloadavg],
    [LIBS="-lutil $LIBS" gl_have_func=yes gl_cv_func_getloadavg_setgid=yes])
fi

if test $gl_have_func = no; then
  # There is a commonly available library for RS/6000 AIX.
  # Since it is not a standard part of AIX, it might be installed locally.
  gl_getloadavg_LIBS=$LIBS
  LIBS="-L/usr/local/lib $LIBS"
  AC_CHECK_LIB([getloadavg], [getloadavg],
               [LIBS="-lgetloadavg $LIBS"], [LIBS=$gl_getloadavg_LIBS])
fi

# Make sure it is really in the library, if we think we found it,
# otherwise set up the replacement function.
AC_CHECK_FUNCS([getloadavg], [],
               [gl_PREREQ_GETLOADAVG])

# Some definitions of getloadavg require that the program be installed setgid.
AC_CACHE_CHECK([whether getloadavg requires setgid],
               gl_cv_func_getloadavg_setgid,
[AC_EGREP_CPP([Yowza Am I SETGID yet],
[#define CONFIGURING_GETLOADAVG
#include "$srcdir/$1/getloadavg.c"
#ifdef LDAV_PRIVILEGED
Yowza Am I SETGID yet
#endif
],
              gl_cv_func_getloadavg_setgid=yes,
              gl_cv_func_getloadavg_setgid=no)])
if test $gl_cv_func_getloadavg_setgid = yes; then
  NEED_SETGID=true
  AC_DEFINE([GETLOADAVG_PRIVILEGED], [1],
            [Define to 1 if the `getloadavg' function needs to be run setuid
             or setgid.])
else
  NEED_SETGID=false
fi
AC_SUBST([NEED_SETGID])dnl

if test $gl_cv_func_getloadavg_setgid = yes; then
  AC_CACHE_CHECK([group of /dev/kmem], [gl_cv_group_kmem],
[ # On Solaris, /dev/kmem is a symlink.  Get info on the real file.
  ac_ls_output=`ls -lgL /dev/kmem 2>/dev/null`
  # If we got an error (system does not support symlinks), try without -L.
  test -z "$ac_ls_output" && ac_ls_output=`ls -lg /dev/kmem`
  gl_cv_group_kmem=`echo $ac_ls_output \
    | sed -ne ['s/[	 ][	 ]*/ /g
               s/^.[sSrwx-]* *[0-9]* *\([^0-9]*\)  *.*/\1/
               / /s/.* //;p']`
])
  AC_SUBST([KMEM_GROUP], [$gl_cv_group_kmem])dnl
fi
if test "x$gl_save_LIBS" = x; then
  GETLOADAVG_LIBS=$LIBS
else
  GETLOADAVG_LIBS=`echo "$LIBS" | sed "s!$gl_save_LIBS!!"`
fi
LIBS=$gl_save_LIBS

AC_SUBST([GETLOADAVG_LIBS])dnl

# Test whether the system declares getloadavg. Solaris has the function
# but declares it in <sys/loadavg.h>, not <stdlib.h>.
AC_CHECK_HEADERS([sys/loadavg.h])
if test $ac_cv_header_sys_loadavg_h = yes; then
  HAVE_SYS_LOADAVG_H=1
else
  HAVE_SYS_LOADAVG_H=0
fi
AC_CHECK_DECL([getloadavg], [], [HAVE_DECL_GETLOADAVG=0],
  [#if HAVE_SYS_LOADAVG_H
   # include <sys/loadavg.h>
   #endif
   #include <stdlib.h>])
])# gl_GETLOADAVG


# gl_PREREQ_GETLOADAVG
# --------------------
# Set up the AC_LIBOBJ replacement of `getloadavg'.
AC_DEFUN([gl_PREREQ_GETLOADAVG],
[AC_LIBOBJ([getloadavg])
AC_DEFINE([C_GETLOADAVG], [1], [Define to 1 if using `getloadavg.c'.])
# Figure out what our getloadavg.c needs.
gl_have_func=no
AC_CHECK_HEADER([sys/dg_sys_info.h],
[gl_have_func=yes
 AC_DEFINE([DGUX], [1], [Define to 1 for DGUX with <sys/dg_sys_info.h>.])
 AC_CHECK_LIB([dgc], [dg_sys_info])])

# We cannot check for <dwarf.h>, because Solaris 2 does not use dwarf (it
# uses stabs), but it is still SVR4.  We cannot check for <elf.h> because
# Irix 4.0.5F has the header but not the library.
if test $gl_have_func = no && test "$ac_cv_lib_elf_elf_begin" = yes \
    && test "$ac_cv_lib_kvm_kvm_open" = yes; then
  gl_have_func=yes
  AC_DEFINE([SVR4], [1], [Define to 1 on System V Release 4.])
fi

if test $gl_have_func = no; then
  AC_CHECK_HEADER([inq_stats/cpustats.h],
  [gl_have_func=yes
   AC_DEFINE([UMAX], [1], [Define to 1 for Encore UMAX.])
   AC_DEFINE([UMAX4_3], [1],
             [Define to 1 for Encore UMAX 4.3 that has <inq_status/cpustats.h>
              instead of <sys/cpustats.h>.])])
fi

if test $gl_have_func = no; then
  AC_CHECK_HEADER([sys/cpustats.h],
  [gl_have_func=yes; AC_DEFINE([UMAX])])
fi

if test $gl_have_func = no; then
  AC_CHECK_HEADERS([mach/mach.h])
fi

AC_CHECK_HEADERS([nlist.h],
[AC_CHECK_MEMBERS([struct nlist.n_un.n_name],
                  [AC_DEFINE([NLIST_NAME_UNION], [1],
                             [Define to 1 if your `struct nlist' has an
                              `n_un' member.  Obsolete, depend on
                              `HAVE_STRUCT_NLIST_N_UN_N_NAME])], [],
                  [@%:@include <nlist.h>])
 AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <nlist.h>]],
                   [[struct nlist x;
                    #ifdef HAVE_STRUCT_NLIST_N_UN_N_NAME
                    x.n_un.n_name = "";
                    #else
                    x.n_name = "";
                    #endif]])],
                [AC_DEFINE([N_NAME_POINTER], [1],
                           [Define to 1 if the nlist n_name member is a pointer])])
])dnl
])# gl_PREREQ_GETLOADAVG
