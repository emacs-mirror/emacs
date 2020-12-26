# sh-filename.m4 serial 2
dnl Copyright (C) 2018-2020 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.

AC_DEFUN([gl_SH_FILENAME],
[
  AH_VERBATIM([SH_FILENAME],
[/* File name of the Bourne shell.  */
#if defined __CYGWIN__ || defined __ANDROID__
/* Omit the directory part because
   - For 32-bit Cygwin programs in a 64-bit Cygwin environment, the Cygwin
     mounts are not visible.
   - On Android, /bin/sh does not exist. It's /system/bin/sh instead.  */
# define BOURNE_SHELL "sh"
#else
# define BOURNE_SHELL "/bin/sh"
#endif])
])
