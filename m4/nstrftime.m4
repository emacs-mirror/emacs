# nstrftime.m4
# serial 38
dnl Copyright (C) 1996-1997, 1999-2007, 2009-2024 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

# Written by Jim Meyering and Paul Eggert.

AC_DEFUN([gl_FUNC_GNU_STRFTIME],
[
 AC_REQUIRE([AC_C_RESTRICT])

 # This defines (or not) HAVE_TZNAME and HAVE_STRUCT_TM_TM_ZONE.
 AC_REQUIRE([AC_STRUCT_TIMEZONE])

 AC_REQUIRE([gl_TM_GMTOFF])
])
