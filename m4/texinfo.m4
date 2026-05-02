# texinfo.m4
dnl Copyright (C) 2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.
dnl
dnl
AC_DEFUN([gl_TEXINFO],[dnl
AC_REQUIRE([gl_SET_MAKEINFO])
AC_REQUIRE([gl_SET_DOCMISC_W32])
])
dnl
dnl gl_FIND_MAKEINFO
AC_DEFUN([gl_SET_MAKEINFO],[dnl
[## Require makeinfo >= 4.13 (last of the 4.x series) to build the manuals.
: ${MAKEINFO:=makeinfo}
case `($MAKEINFO --version) 2>/dev/null` in
  *' (GNU texinfo) '4.1[[3-9]]* | \
  *' (GNU texinfo) '[[5-9]]* | \
  *' (GNU texinfo) '[[1-9][0-9]]* ) ;;
  *) MAKEINFO=no;;
esac

## Makeinfo is unusual.  For a released Emacs, the manuals are
## pre-built, and not deleted by the normal clean rules.  makeinfo is
## therefore in the category of "special tools" not normally required, which
## configure does not have to check for (eg autoconf itself).
## In a repository checkout on the other hand, the manuals are not included.
## So makeinfo is a requirement to build from the repository, and configure
## should test for it as it does for any other build requirement.
## We use the presence of $srcdir/info/emacs to distinguish a release,
## with pre-built manuals, from a repository checkout.
if test "$MAKEINFO" = "no"; then
  MAKEINFO=makeinfo
  if test ! -e "$srcdir/info/emacs" && test ! -e "$srcdir/info/emacs.info"; then
    ]AC_MSG_ERROR( [You do not seem to have makeinfo >= 4.13, and your
source tree does not seem to have pre-built manuals in the 'info' directory.
Please install a suitable version of makeinfo.] )[
  else
    ]AC_MSG_WARN( [You do not seem to have makeinfo >= 4.13.
You will not be able to rebuild the manuals if you delete them or change
their sources.] )[
  fi
fi
]AC_SUBST([MAKEINFO])
])
dnl
dnl gl_SET_DOCMISC_W32
AC_DEFUN([gl_SET_DOCMISC_W32],[dnl
[if test $opsys = mingw32; then
   DOCMISC_W32=efaq-w32
else
   DOCMISC_W32=
fi
]AC_SUBST([DOCMISC_W32])
])
dnl
