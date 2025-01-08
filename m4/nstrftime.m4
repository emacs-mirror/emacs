# nstrftime.m4
# serial 40
dnl Copyright (C) 1996-1997, 1999-2007, 2009-2025 Free Software Foundation,
dnl Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

# Written by Jim Meyering and Paul Eggert.

AC_DEFUN([gl_FUNC_GNU_STRFTIME],
[
 AC_REQUIRE([AC_C_RESTRICT])

 AC_REQUIRE([gl_TM_GMTOFF])
 AC_CHECK_FUNCS_ONCE([strftime_z])
])
