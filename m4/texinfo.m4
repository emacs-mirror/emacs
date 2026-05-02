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
AC_REQUIRE([gl_SET_DOCLANGS])
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
dnl gl_dollar is need for $1, $2 etc. in AWK snippets not to be intepreted by
dnl M4 as macros arguments.
m4_define([gl_dollar],[m4_quote([$])])dnl
m4_define([gl_DOCLANGS_FULL],[[default fr]])dnl
dnl This is minimal texinfo.tex version with "-" removed from the ISO date
dnl part from which there is no issue with index key sorting when document
dnl language/encoding corresponding locale is not installer nor active.
m4_define([gl_TEXINFO_TEX_MINVER],[[20260401.0]])dnl
m4_define([gl_TEXINDEX_MINVER],[[7.1]])dnl
m4_define([gl_TEXINFO_INDEXING_IS_LOCALE_DEPENDANT],[dnl
[DOCLANGS='default']
AC_MSG_WARN([texinfo.tex/texindex versions suggest that indexing is locale dependant, manual compilation is restricted to lang 'default', override this by setting DOCLANGS in the environment])
])dnl
m4_define([gl_GET_TEXINFO_TEX_VER],[dnl
[texinfo_tex_ver=`tex -jobname=conftest '\nonstopmode\input texinfo.tex @typeout{TEXINFO_TEX_VER=@texinfoversion}@bye' | awk 'BEGIN{R=1;FS="="};]gl_dollar[1=="TEXINFO_TEX_VER" { gsub("-","");print ]gl_dollar[2; R=0; exit}; END{ exit R}'`
if test $? -ne 0; then
   texinfo_tex_ver_ver=0.0]
   AC_MSG_WARN([Can't find texinfo.tex version, check tex and texinfo.tex are installed.])
[fi
]])dnl
m4_define([gl_GET_TEXINDEX_VER],[dnl
[texindex_ver=`texindex --version | awk 'BEGIN { R=1};NR==1 && ]gl_dollar[1 == "texindex"{ print $NF; R=0; exit}; {exit}; END { exit R}'`
if test $? -ne 0; then
   texindex_ver=0.0]
   AC_MSG_WARN([Can't find texindex version, check texindex is installed.])
[fi
]
])dnl
AC_DEFUN([gl_TEXINFO_VERSION_COMPARE],[dnl
AC_REQUIRE([AS_VERSION_COMPARE])
AS_VERSION_COMPARE([$1],[$2],[gl_TEXINFO_INDEXING_IS_LOCALE_DEPENDANT()],[$3],[$3])dnl
])dnl
dnl gl_SET_DOCLANGS
AC_DEFUN([gl_SET_DOCLANGS],[dnl
AC_REQUIRE([gl_TEXINFO_VERSION_COMPARE])
AC_ARG_VAR([DOCLANGS],[languages for which manuals are compiled, languages supported: ]gl_DOCLANGS_FULL()[, list is space separated])
gl_GET_TEXINFO_TEX_VER()
gl_GET_TEXINDEX_VER()
gl_TEXINFO_VERSION_COMPARE([$texinfo_tex_ver],[gl_TEXINFO_TEX_MINVER()],[dnl
  gl_TEXINFO_VERSION_COMPARE([$texindex_ver],[gl_TEXINDEX_MINVER()],[dnl
  [DOCLANGS=']gl_DOCLANGS_FULL()[']
  AC_MSG_NOTICE([texinfo.tex/texindex versions suggest that indexing is not locale dependant, setting DOCLANGS to ']gl_DOCLANGS_FULL()['])
  ])
])
AC_SUBST([DOCLANGS])
])
dnl
