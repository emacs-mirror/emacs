# copy-file-range.m4
dnl Copyright 2019-2022 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_COPY_FILE_RANGE],
[
  AC_REQUIRE([gl_UNISTD_H_DEFAULTS])
  AC_REQUIRE([AC_CANONICAL_HOST])

  dnl Persuade glibc <unistd.h> to declare copy_file_range.
  AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])

  dnl Use AC_LINK_IFELSE, rather than AC_CHECK_FUNCS or a variant,
  dnl since we don't want AC_CHECK_FUNCS's checks for glibc stubs.
  dnl Programs that use copy_file_range must fall back on read+write
  dnl anyway, and there's little point to substituting the Gnulib stub
  dnl for a glibc stub.
  AC_CACHE_CHECK([for copy_file_range], [gl_cv_func_copy_file_range],
    [AC_LINK_IFELSE(
       [AC_LANG_PROGRAM(
          [[#include <unistd.h>
          ]],
          [[ssize_t (*func) (int, off_t *, int, off_t *, size_t, unsigned)
              = copy_file_range;
            return func (0, 0, 0, 0, 0, 0) & 127;
          ]])
       ],
       [gl_cv_func_copy_file_range=yes],
       [gl_cv_func_copy_file_range=no])
    ])

  if test "$gl_cv_func_copy_file_range" != yes; then
    HAVE_COPY_FILE_RANGE=0
  else
    AC_DEFINE([HAVE_COPY_FILE_RANGE], 1,
      [Define to 1 if the function copy_file_range exists.])

    case $host_os in
      linux*)
        AC_CACHE_CHECK([whether copy_file_range is known to work],
          [gl_cv_copy_file_range_known_to_work],
          [AC_COMPILE_IFELSE(
             [AC_LANG_PROGRAM(
                [[#include <linux/version.h>
                ]],
                [[#if LINUX_VERSION_CODE < KERNEL_VERSION (5, 3, 0)
                   #error "copy_file_range is buggy"
                  #endif
                ]])],
             [gl_cv_copy_file_range_known_to_work=yes],
             [gl_cv_copy_file_range_known_to_work=no])])
        if test "$gl_cv_copy_file_range_known_to_work" = no; then
          REPLACE_COPY_FILE_RANGE=1
        fi;;
    esac
  fi
])
