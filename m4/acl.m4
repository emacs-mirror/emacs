# acl.m4
# serial 40
dnl Copyright (C) 2002, 2004-2025 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

# Check for access control list (ACL) primitives

# Written by Paul Eggert and Jim Meyering.

AC_DEFUN([gl_FUNC_ACL_ARG],
[
  AC_ARG_ENABLE([acl],
    AS_HELP_STRING([[--disable-acl]], [do not support ACLs]),
    , [enable_acl=auto])
  AC_ARG_WITH([libsmack],
    [AS_HELP_STRING([--without-libsmack],
       [do not use libsmack, even on systems that have it])],
    [], [with_libsmack=maybe])
])

# Prerequisites of module acl-permissions.
AC_DEFUN_ONCE([gl_FUNC_ACL],
[
  AC_REQUIRE([gl_FUNC_ACL_ARG])
  AC_CHECK_FUNCS_ONCE([fchmod])
  LIB_ACL=
  use_acl=0
  NEED_ACL_ENTRIES=0
  if test "$enable_acl" != no; then
    dnl On all platforms, the ACL related API is declared in <sys/acl.h>.
    AC_CHECK_HEADERS([sys/acl.h])
    if test $ac_cv_header_sys_acl_h = yes; then
      gl_saved_LIBS=$LIBS

      dnl Test for POSIX-draft-like API (GNU/Linux, FreeBSD, NetBSD >= 10,
      dnl Mac OS X, Cygwin >= 2.5).
      dnl -lacl is needed on GNU/Linux.
      if test $use_acl = 0; then
        AC_SEARCH_LIBS([acl_get_file], [acl],
          [if test "$ac_cv_search_acl_get_file" != "none required"; then
             LIB_ACL=$ac_cv_search_acl_get_file
           fi
           AC_CHECK_FUNCS(
             [acl_get_file acl_get_fd acl_set_file acl_set_fd \
              acl_get_link_np \
              acl_free acl_from_mode acl_from_text \
              acl_delete_def_file acl_extended_file \
              acl_delete_fd_np acl_delete_file_np \
              acl_copy_ext_native acl_create_entry_np \
              acl_to_short_text acl_free_text])
           # If the acl_get_file bug is detected, don't enable the ACL support.
           gl_ACL_GET_FILE([use_acl=1], [])
           if test $use_acl = 1; then
             dnl On GNU/Linux, an additional API is declared in <acl/libacl.h>.
             AC_CHECK_HEADERS([acl/libacl.h])
             AC_CHECK_FUNC([acl_entries],
               [AC_DEFINE([HAVE_ACL_ENTRIES], [1],
                  [Define to 1 if libc or libacl defines the function acl_entries.])],
               [NEED_ACL_ENTRIES=1])
             AC_CACHE_CHECK([for ACL_FIRST_ENTRY],
               [gl_cv_acl_ACL_FIRST_ENTRY],
               [AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
[[#include <sys/types.h>
#include <sys/acl.h>
int type = ACL_FIRST_ENTRY;]])],
                  [gl_cv_acl_ACL_FIRST_ENTRY=yes],
                  [gl_cv_acl_ACL_FIRST_ENTRY=no])])
             if test $gl_cv_acl_ACL_FIRST_ENTRY = yes; then
               AC_DEFINE([HAVE_ACL_FIRST_ENTRY], [1],
                 [Define to 1 if the constant ACL_FIRST_ENTRY exists.])
             fi
             dnl On Mac OS X, other types of ACLs are supported.
             AC_CACHE_CHECK([for ACL_TYPE_EXTENDED],
               [gl_cv_acl_ACL_TYPE_EXTENDED],
               [AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
[[#include <sys/types.h>
#include <sys/acl.h>
int type = ACL_TYPE_EXTENDED;]])],
                  [gl_cv_acl_ACL_TYPE_EXTENDED=yes],
                  [gl_cv_acl_ACL_TYPE_EXTENDED=no])])
             if test $gl_cv_acl_ACL_TYPE_EXTENDED = yes; then
               AC_DEFINE([HAVE_ACL_TYPE_EXTENDED], [1],
                 [Define to 1 if the ACL type ACL_TYPE_EXTENDED exists.])
             fi
           else
             LIB_ACL=
           fi
          ])
      fi

      dnl Test for Solaris API (Solaris, Cygwin).
      if test $use_acl = 0; then
        AC_CHECK_FUNCS([facl])
        if test $ac_cv_func_facl = yes; then
          AC_SEARCH_LIBS([acl_trivial], [sec],
            [if test "$ac_cv_search_acl_trivial" != "none required"; then
               LIB_ACL=$ac_cv_search_acl_trivial
             fi
            ])
          AC_CHECK_FUNCS([acl_trivial])
          use_acl=1
        fi
      fi

      dnl Test for HP-UX API.
      if test $use_acl = 0; then
        AC_CHECK_FUNCS([getacl])
        if test $ac_cv_func_getacl = yes; then
          use_acl=1
        fi
        dnl Test for HP-UX 11.11 API.
        AC_CHECK_HEADERS([aclv.h], [], [], [#include <sys/types.h>])
      fi

      dnl Test for AIX API (AIX 5.3 or newer).
      if test $use_acl = 0; then
        AC_CHECK_FUNCS([aclx_get])
        if test $ac_cv_func_aclx_get = yes; then
          use_acl=1
        fi
      fi

      dnl Test for older AIX API.
      if test $use_acl = 0 || test "$ac_cv_func_aclx_get" = yes; then
        AC_CHECK_FUNCS([statacl])
        if test $ac_cv_func_statacl = yes; then
          use_acl=1
        fi
      fi

      dnl Test for NonStop Kernel API.
      if test $use_acl = 0; then
        AC_CHECK_FUNCS([aclsort])
        if test $ac_cv_func_aclsort = yes; then
          use_acl=1
        fi
      fi

      LIBS=$gl_saved_LIBS
    fi

    if test "$enable_acl$use_acl" = yes0; then
      AC_MSG_ERROR([ACLs enabled but support not detected])
    elif test "$enable_acl$use_acl" = auto0; then
      AC_MSG_WARN([libacl development library was not found or not usable.])
      AC_MSG_WARN([AC_PACKAGE_NAME will be built without ACL support.])
    fi
  fi
  AC_SUBST([LIB_ACL])
  AC_DEFINE_UNQUOTED([USE_ACL], [$use_acl],
    [Define to nonzero if you want access control list support.])
  USE_ACL=$use_acl
  AC_SUBST([USE_ACL])
])

# gl_ACL_GET_FILE(IF-WORKS, IF-NOT)
# ---------------------------------
# If 'acl_get_file' works (does not have a particular bug),
# run IF-WORKS, otherwise, IF-NOT.
# When building natively, test for a Darwin 8.7.0 bug, whereby acl_get_file
# returns NULL, but sets errno = ENOENT for an existing file or directory.
# When cross-compiling, assume that this old bug no longer applies.
AC_DEFUN([gl_ACL_GET_FILE],
[
  AC_CACHE_CHECK([for working acl_get_file], [gl_cv_func_working_acl_get_file],
    [gl_cv_func_working_acl_get_file=no
     AC_LINK_IFELSE(
       [AC_LANG_PROGRAM(
          [[#include <sys/types.h>
           #include <sys/acl.h>
           #include <errno.h>
          ]],
          [[acl_t acl = acl_get_file (".", ACL_TYPE_ACCESS);
            return acl ? acl_free (acl) != 0 : errno == ENOENT;
          ]])],
       [if test $cross_compiling = yes; then
          gl_cv_func_working_acl_get_file="guessing yes"
        elif ./conftest$ac_exeext; then
          gl_cv_func_working_acl_get_file=yes
        fi])])
  AS_IF([test "$gl_cv_func_working_acl_get_file" != no], [$1], [$2])
])

# Prerequisites of module file-has-acl.
AC_DEFUN([gl_FILE_HAS_ACL],
[
  AC_REQUIRE([gl_FUNC_ACL_ARG])
  AC_REQUIRE([gl_FUNC_ACL])
  # On GNU/Linux, testing if a file has an acl can be done with the
  # listxattr and getxattr syscalls, which don't require linking
  # against additional libraries.  Assume this works if linux/attr.h
  # and listxattr are present.
  AC_CHECK_HEADERS_ONCE([linux/xattr.h])
  AC_CHECK_FUNCS_ONCE([listxattr])
  FILE_HAS_ACL_LIB=

  gl_file_has_acl_uses_smack=no
  AS_CASE([$enable_acl,$with_libsmack,$ac_cv_header_linux_xattr_h,$ac_cv_func_listxattr],
    [no,* | *,no,*], [],
    [*,*,yes,yes],
      [AC_CHECK_HEADER([sys/smack.h],
         [gl_saved_LIBS=$LIBS
          AC_SEARCH_LIBS([smack_new_label_from_path], [smack],
            [AC_DEFINE([HAVE_SMACK], [1],
               [Define to 1 if libsmack is usable.])
             AS_CASE([$ac_cv_search_smack_new_label_from_path],
               ["none required"], [],
               [FILE_HAS_ACL_LIB=$ac_cv_search_new_label_from_path])
             gl_file_has_acl_uses_smack=yes],
            [AS_CASE([$with_libsmack],
               [yes], [AC_MSG_ERROR([libsmack not found or unusable])])])
          LIBS=$gl_saved_LIBS])])

  gl_file_has_acl_uses_selinux=no
  AS_CASE([$enable_acl,$with_selinux,$ac_cv_header_linux_xattr_h,$ac_cv_func_listxattr],
    [no,* | *,no,*], [],
    [*,*,yes,yes],
      [AC_REQUIRE([gl_CHECK_HEADER_SELINUX_SELINUX_H])
       AS_IF([test $USE_SELINUX_SELINUX_H = 1],
         [FILE_HAS_ACL_LIB="$FILE_HAS_ACL_LIB $LIB_SELINUX"
          gl_file_has_acl_uses_selinux=yes])])

  AS_CASE([$enable_acl,$gl_file_has_acl_uses_selinux,$gl_file_has_acl_uses_smack],
    [no,* | *,yes,* | *,yes], [],
    [*],
      [FILE_HAS_ACL_LIB=$LIB_ACL])
  AC_SUBST([FILE_HAS_ACL_LIB])
])

# Prerequisites of module qcopy-acl.
AC_DEFUN([gl_QCOPY_ACL],
[
  AC_REQUIRE([gl_FUNC_ACL])
  AC_REQUIRE([gl_FILE_HAS_ACL])
  AC_CHECK_HEADERS_ONCE([linux/xattr.h])
  gl_FUNC_XATTR
  if test "$use_xattr" = yes; then
    QCOPY_ACL_LIB="$LIB_XATTR $FILE_HAS_ACL_LIB"
  else
    QCOPY_ACL_LIB="$LIB_ACL"
  fi
  AC_SUBST([QCOPY_ACL_LIB])
])
