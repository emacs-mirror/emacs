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
dnl gl_SET_MAKEINFO
dnl set the MAKEINFO precious variable to the suitable makeinfo compiler.
AC_DEFUN([gl_SET_MAKEINFO],[dnl
[## Require makeinfo >= 4.13 (last of the 4.x series) to build the manuals.
: ${MAKEINFO:=makeinfo}
case `($MAKEINFO --version) 2>/dev/null` in
  *' (GNU texinfo) '4.1[3-9]* | \
  *' (GNU texinfo) '[5-9]* | \
  *' (GNU texinfo) '[1-9][0-9]* ) ;;
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
dnl gl_dollar is needed for $1, $2 etc. in AWK snippets not to be intepreted by
dnl M4 as macros arguments.
m4_define([gl_dollar],[m4_quote([$])])dnl
m4_define([gl_DOCLANGS_FULL],[[default fr]])dnl
dnl This is minimal texinfo.tex version with "-" removed from the ISO date
dnl part from which there is no issue with index key sorting when document
dnl language/encoding corresponding locale is not installer nor active.
dnl See texinfo version 836b8924560.
m4_define([gl_TEXINFO_TEX_MINVER],[[20260426.12]])dnl
m4_define([gl_TEXINDEX_MINVER],[[7.1]])dnl
dnl gl_TEXINFO_INDEXING_IS_LOCALE_DEPENDANT
dnl when tex tools are installed & working and that their version suggest that
dnl the PDF/PS/DVI compilation may be broken by UTF-8 encoded manual sources
dnl when the locale is not UTF-8, DOCLANGS is restricted to default language.
m4_define([gl_TEXINFO_INDEXING_IS_LOCALE_DEPENDANT],[dnl
[DOCLANGS='default'
gl_fn_doclangs_info () {]
  AC_MSG_WARN([texinfo.tex/texindex versions suggest that indexing is locale dependant, manual compilation is restricted to lang 'default', override this by setting DOCLANGS in the environment])
[}]])dnl
dnl gl_GET_TEXINFO_TEX_VER
dnl Test tex is installed and read the texinfo version into variable texinfo_tex_ver
m4_define([gl_GET_TEXINFO_TEX_VER],[dnl
[if false | true; then nopipefail=true; set -o pipefail; else nopipefail=false; fi
if texinfo_tex_ver=`tex -jobname=conftest '\nonstopmode\input texinfo.tex @typeout{TEXINFO_TEX_VER=@texinfoversion}@bye' 2>/dev/null | awk 'BEGIN{R=1;FS="="};]gl_dollar[1=="TEXINFO_TEX_VER" { gsub("-","");print ]gl_dollar[2; R=0; exit}; END{ if(R) {print "notfound"}}'`; then
  if test "$texinfo_tex_ver" == "notfound"; then
     texinfo_tex_ver=0.0
     gl_fn_texinfo_info () {]
        AC_MSG_WARN([Can't find texinfo.tex version, check tex and texinfo.tex are installed.])[
     }
  fi
else
   texinfo_tex_ver=0.0
   tex_available=false
   gl_fn_texinfo_info () {]
      AC_MSG_NOTICE([tex not installed or not working.])[
   }
fi
if $nopipefail; then set +o pipefail; fi
]])dnl
dnl gl_GET_TEXINDEX_VER
dnl Test texindex is installed and read its vrson into variable texindex_ver
dnl Test is not carried out when tex_available is false
dnl texindex exiting in error will set tex_available to false
m4_define([gl_GET_TEXINDEX_VER],[dnl
[if $tex_available; then
  if false | true; then nopipefail=true; set -o pipefail; else nopipefail=false; fi
  if texindex_ver=`texindex --version 2> /dev/null | awk 'BEGIN { R=1};NR==1 && ]gl_dollar[1 == "texindex"{ print $NF; R=0; exit}; {exit}; END { if (R) { print "notfound"}}'`; then
    if test "$texindex_ver" == "notfound"; then
      texindex_ver=0.0
      gl_fn_texindex_info () {]
        AC_MSG_WARN([Can't find texindex version, check texindex is installed.])[
      }
    fi
  else
    tex_available=false
    texindex_ver=0.0
    gl_fn_texindex_info () {]
      AC_MSG_NOTICE([texindex not installed or not working.])[
    }
  fi
  if $nopipefail; then set +o pipefail; fi
else
  texindex_ver=0.0
  gl_fn_texindex_info () {]
    AC_MSG_NOTICE([Not testing texindex version for tex not installed or not working])[
  }
fi
]
])dnl
AC_DEFUN([gl_TEXINFO_VERSION_COMPARE],[dnl
AC_REQUIRE([AS_VERSION_COMPARE])
AS_VERSION_COMPARE([$1],[$2],[gl_TEXINFO_INDEXING_IS_LOCALE_DEPENDANT()],[$3],[$3])dnl
])dnl
dnl gl_SET_DOCLANGS
dnl DOCLANGS is restricted to default when tex/texindex are installed but their
dnl version do not guarantee that index compilation won't break on an UTF-8 encoded
dnl manual. Old version of texindex may derive the locale from the environment rather
dnl than from the @documentencoding statement and break multibyte characters into
dnl incorrect UTF-8 byte sequences.
dnl The rationale is that default language is assumed not to use UTF-8 encoding
dnl whereas translations may use it.
dnl If ever you wish an UTF-8 encoded English manual to contain some symbol or
dnl emoji, then create an English translation of a default manual without the symbols.
AC_DEFUN([gl_SET_DOCLANGS],[dnl
AC_REQUIRE([gl_TEXINFO_VERSION_COMPARE])
AC_ARG_VAR([DOCLANGS],[languages for which manuals are compiled, languages supported: ]gl_DOCLANGS_FULL()[, list is space separated])
AC_MSG_CHECKING([for DOCLANGS derivation from texinfo.tex/texindex versions])
dnl By default nothing to inform about
[gl_fn_texinfo_info () { :; }
gl_fn_texindex_info () { :; }
gl_fn_doclangs_info () { :; }
tex_available=true;]
gl_GET_TEXINFO_TEX_VER()
gl_GET_TEXINDEX_VER()
[if $tex_available; then]
  gl_TEXINFO_VERSION_COMPARE([$texinfo_tex_ver],[gl_TEXINFO_TEX_MINVER()],[dnl
    gl_TEXINFO_VERSION_COMPARE([$texindex_ver],[gl_TEXINDEX_MINVER()],[dnl
    [DOCLANGS=']gl_DOCLANGS_FULL()['
    gl_fn_doclangs_info () {]
      AC_MSG_NOTICE([texinfo.tex/texindex versions suggest that indexing is not locale dependant])
   [}]])
])[
else
  DOCLANGS=']gl_DOCLANGS_FULL()['
fi]
AC_MSG_RESULT([[']$DOCLANGS[']])
gl_fn_texinfo_info
gl_fn_texindex_info
gl_fn_doclangs_info
AC_SUBST([DOCLANGS])
])
dnl
