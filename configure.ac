dnl  Autoconf script for GNU Emacs
dnl To rebuild the `configure' script from this, execute the command
dnl 	autoconf
dnl in the directory containing this script.
dnl If you changed any AC_DEFINES, also run autoheader.
dnl
dnl Copyright (C) 1994-1996, 1999-2013 Free Software Foundation, Inc.
dnl
dnl  This file is part of GNU Emacs.
dnl
dnl  GNU Emacs is free software: you can redistribute it and/or modify
dnl  it under the terms of the GNU General Public License as published by
dnl  the Free Software Foundation, either version 3 of the License, or
dnl  (at your option) any later version.
dnl
dnl  GNU Emacs is distributed in the hope that it will be useful,
dnl  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl  GNU General Public License for more details.
dnl
dnl  You should have received a copy of the GNU General Public License
dnl  along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

AC_PREREQ(2.65)
AC_INIT(emacs, 24.3)
AC_CONFIG_HEADER(src/config.h:src/config.in)
AC_CONFIG_SRCDIR(src/lisp.h)
AC_CONFIG_AUX_DIR(build-aux)
dnl Fairly arbitrary, older versions might work too.
AM_INIT_AUTOMAKE(1.11)

dnl Support for --program-prefix, --program-suffix and
dnl --program-transform-name options
AC_ARG_PROGRAM

dnl It is important that variables on the RHS not be expanded here,
dnl hence the single quotes.  This is per the GNU coding standards, see
dnl (autoconf) Installation Directory Variables
dnl See also epaths.h below.
lispdir='${datadir}/emacs/${version}/lisp'
leimdir='${datadir}/emacs/${version}/leim'
standardlisppath='${lispdir}:${leimdir}'
locallisppath='${datadir}/emacs/${version}/site-lisp:'\
'${datadir}/emacs/site-lisp'
lisppath='${locallisppath}:${standardlisppath}'
etcdir='${datadir}/emacs/${version}/etc'
archlibdir='${libexecdir}/emacs/${version}/${configuration}'
docdir='${datadir}/emacs/${version}/etc'
gamedir='${localstatedir}/games/emacs'

dnl Special option to disable the most of other options.
AC_ARG_WITH(all,
[AS_HELP_STRING([--without-all],
		[omit almost all features and build
		small executable with minimal dependencies])],
  with_features=$withval,
  with_features=yes)

dnl OPTION_DEFAULT_OFF(NAME, HELP-STRING)
dnl Create a new --with option that defaults to being disabled.
dnl NAME is the base name of the option.  The shell variable with_NAME
dnl   will be set to either the user's value (if the option is
dnl   specified; 'yes' for a plain --with-NAME) or to 'no' (if the
dnl   option is not specified).  Note that the shell variable name is
dnl   constructed as autoconf does, by replacing non-alphanumeric
dnl   characters with "_".
dnl HELP-STRING is the help text for the option.
AC_DEFUN([OPTION_DEFAULT_OFF], [dnl
  AC_ARG_WITH([$1],[AS_HELP_STRING([--with-$1],[$2])],[],[dnl
    m4_bpatsubst([with_$1], [[^0-9a-z]], [_])=no])dnl
])dnl

dnl OPTION_DEFAULT_ON(NAME, HELP-STRING)
dnl Create a new --with option that defaults to $enable_features.
dnl NAME is the base name of the option.  The shell variable with_NAME
dnl   will be set either to 'no' (for a plain --without-NAME) or to
dnl   'yes' (if the option is not specified).  Note that the shell
dnl   variable name is constructed as autoconf does, by replacing
dnl   non-alphanumeric characters with "_".
dnl HELP-STRING is the help text for the option.
AC_DEFUN([OPTION_DEFAULT_ON], [dnl
  AC_ARG_WITH([$1],[AS_HELP_STRING([--without-$1],[$2])],[],[dnl
   m4_bpatsubst([with_$1], [[^0-9a-z]], [_])=$with_features])dnl
])dnl

OPTION_DEFAULT_ON([pop],[don't support POP mail retrieval with movemail])
if test "$with_pop" = yes; then
   AC_DEFINE(MAIL_USE_POP)
fi
AH_TEMPLATE(MAIL_USE_POP, [Define to support POP mail retrieval.])dnl

OPTION_DEFAULT_OFF([kerberos],[support Kerberos-authenticated POP])
if test "$with_kerberos" != no; then
   AC_DEFINE(KERBEROS)
fi
AH_TEMPLATE(KERBEROS,
	    [Define to support Kerberos-authenticated POP mail retrieval.])dnl

OPTION_DEFAULT_OFF([kerberos5],[support Kerberos version 5 authenticated POP])
if test "${with_kerberos5}" != no; then
  if test "${with_kerberos}" = no; then
    with_kerberos=yes
    AC_DEFINE(KERBEROS)
  fi
  AC_DEFINE(KERBEROS5, 1, [Define to use Kerberos 5 instead of Kerberos 4.])
fi

OPTION_DEFAULT_OFF([hesiod],[support Hesiod to get the POP server host])
dnl FIXME hesiod support may not be present, so it seems like an error
dnl to define, or at least use, this unconditionally.
if test "$with_hesiod" != no; then
  AC_DEFINE(HESIOD, 1, [Define to support using a Hesiod database to find the POP server.])
fi

OPTION_DEFAULT_OFF([mmdf],[support MMDF mailboxes])
if test "$with_mmdf" != no; then
   AC_DEFINE(MAIL_USE_MMDF, 1, [Define to support MMDF mailboxes in movemail.])
fi

OPTION_DEFAULT_OFF([mail-unlink],[unlink, rather than empty, mail spool after reading])
if test "$with_mail_unlink" != no; then
   AC_DEFINE(MAIL_UNLINK_SPOOL, 1, [Define to unlink, rather than empty, mail spool after reading.])
fi

AC_ARG_WITH([mailhost],[AS_HELP_STRING([--with-mailhost=HOSTNAME],
    [string giving default POP mail host])],
    AC_DEFINE_UNQUOTED(MAILHOST, ["$withval"], [String giving fallback POP mail host.]))

OPTION_DEFAULT_ON([sound],[don't compile with sound support])

dnl FIXME currently it is not the last.
dnl This should be the last --with option, because --with-x is
dnl added later on when we find the path of X, and it's best to
dnl keep them together visually.
AC_ARG_WITH([x-toolkit],[AS_HELP_STRING([--with-x-toolkit=KIT],
 [use an X toolkit (KIT one of: yes or gtk, gtk2, gtk3, lucid or athena, motif, no)])],
[	  case "${withval}" in
	    y | ye | yes )	val=gtk ;;
	    n | no )		val=no  ;;
	    l | lu | luc | luci | lucid )	val=lucid ;;
	    a | at | ath | athe | athen | athena )	val=athena ;;
	    m | mo | mot | moti | motif )	val=motif ;;
	    g | gt | gtk  )	val=gtk ;;
	    gtk2  )	val=gtk2 ;;
	    gtk3  )	val=gtk3 ;;
	    * )
AC_MSG_ERROR([`--with-x-toolkit=$withval' is invalid;
this option's value should be `yes', `no', `lucid', `athena', `motif', `gtk',
`gtk2' or `gtk3'.  `yes' and `gtk' are synonyms.
`athena' and `lucid' are synonyms.])
	    ;;
	  esac
	  with_x_toolkit=$val
])

OPTION_DEFAULT_OFF([wide-int], [prefer wide Emacs integers (typically 62-bit)])
if test "$with_wide_int" = yes; then
  AC_DEFINE([WIDE_EMACS_INT], 1, [Use long long for EMACS_INT if available.])
fi

dnl _ON results in a '--without' option in the --help output, so
dnl the help text should refer to "don't compile", etc.
OPTION_DEFAULT_ON([xpm],[don't compile with XPM image support])
OPTION_DEFAULT_ON([jpeg],[don't compile with JPEG image support])
OPTION_DEFAULT_ON([tiff],[don't compile with TIFF image support])
OPTION_DEFAULT_ON([gif],[don't compile with GIF image support])
OPTION_DEFAULT_ON([png],[don't compile with PNG image support])
OPTION_DEFAULT_ON([rsvg],[don't compile with SVG image support])
OPTION_DEFAULT_ON([xml2],[don't compile with XML parsing support])
OPTION_DEFAULT_ON([imagemagick],[don't compile with ImageMagick image support])

OPTION_DEFAULT_ON([xft],[don't use XFT for anti aliased fonts])
OPTION_DEFAULT_ON([libotf],[don't use libotf for OpenType font support])
OPTION_DEFAULT_ON([m17n-flt],[don't use m17n-flt for text shaping])

OPTION_DEFAULT_ON([toolkit-scroll-bars],[don't use Motif or Xaw3d scroll bars])
OPTION_DEFAULT_ON([xaw3d],[don't use Xaw3d])
OPTION_DEFAULT_ON([xim],[don't use X11 XIM])
OPTION_DEFAULT_OFF([ns],[use NeXTstep (Cocoa or GNUstep) windowing system])
OPTION_DEFAULT_OFF([w32], [use native MS Windows GUI])

OPTION_DEFAULT_ON([gpm],[don't use -lgpm for mouse support on a GNU/Linux console])
OPTION_DEFAULT_ON([dbus],[don't compile with D-Bus support])
OPTION_DEFAULT_ON([gconf],[don't compile with GConf support])
OPTION_DEFAULT_ON([gsettings],[don't compile with GSettings support])
OPTION_DEFAULT_ON([selinux],[don't compile with SELinux support])
OPTION_DEFAULT_ON([gnutls],[don't use -lgnutls for SSL/TLS support])

## For the times when you want to build Emacs but don't have
## a suitable makeinfo, and can live without the manuals.
dnl http://lists.gnu.org/archive/html/emacs-devel/2008-04/msg01844.html
OPTION_DEFAULT_ON([makeinfo],[don't require makeinfo for building manuals])

## Makefile.in needs the cache file name.
AC_SUBST(cache_file)

## This is an option because I do not know if all info/man support
## compressed files, nor how to test if they do so.
OPTION_DEFAULT_ON([compress-info],[don't compress the installed Info pages])
if test $with_compress_info = yes; then
   GZIP_INFO=yes
else
   GZIP_INFO=
fi
AC_SUBST(GZIP_INFO)

AC_ARG_WITH([pkg-config-prog],dnl
[AS_HELP_STRING([--with-pkg-config-prog=PATH],
                  [path to pkg-config for finding GTK and librsvg])])
if test "X${with_pkg_config_prog}" != X; then
   if test "${with_pkg_config_prog}" != yes; then
      PKG_CONFIG="${with_pkg_config_prog}"
   fi
fi

CRT_DIR=
AC_ARG_WITH([crt-dir],dnl
[AS_HELP_STRING([--with-crt-dir=DIR],[directory containing crtn.o etc.
The default is /usr/lib, or /usr/lib64 on some platforms.])])
CRT_DIR="${with_crt_dir}"

AC_ARG_WITH(gameuser,dnl
[AS_HELP_STRING([--with-gameuser=USER],[user for shared game score files])])
test "X${with_gameuser}" != X && test "${with_gameuser}" != yes \
  && gameuser="${with_gameuser}"
test "X$gameuser" = X && gameuser=games

AC_ARG_WITH([gnustep-conf],dnl
[AS_HELP_STRING([--with-gnustep-conf=PATH],[path to GNUstep.conf; default $GNUSTEP_CONFIG_FILE, or /etc/GNUstep/GNUstep.conf])])
test "X${with_gnustep_conf}" != X && test "${with_gnustep_conf}" != yes && \
  GNUSTEP_CONFIG_FILE="${with_gnustep_conf}"
test "X$GNUSTEP_CONFIG_FILE" = "X" && \
     GNUSTEP_CONFIG_FILE=/etc/GNUstep/GNUstep.conf

AC_ARG_ENABLE(ns-self-contained,
[AS_HELP_STRING([--disable-ns-self-contained],
                [disable self contained build under NeXTstep])],
   EN_NS_SELF_CONTAINED=$enableval,
   EN_NS_SELF_CONTAINED=yes)

AC_ARG_ENABLE(locallisppath,
[AS_HELP_STRING([--enable-locallisppath=PATH],
                [directories Emacs should search for lisp files specific
		 to this site])],
if test "${enableval}" = "no"; then
  locallisppath=
elif test "${enableval}" != "yes"; then
  locallisppath=${enableval}
fi)

AC_ARG_ENABLE(checking,
[AS_HELP_STRING([--enable-checking@<:@=LIST@:>@],
		[enable expensive run-time checks.  With LIST,
		 enable only specific categories of checks.
		 Categories are: all,yes,no.
		 Flags are: stringbytes, stringoverrun, stringfreelist,
		 xmallocoverrun, conslist, glyphs])],
[ac_checking_flags="${enableval}"],[])
IFS="${IFS= 	}"; ac_save_IFS="$IFS"; IFS="$IFS,"
for check in $ac_checking_flags
do
	case $check in
	# these set all the flags to specific states
	yes)		ac_enable_checking=1 ;;
	no)		ac_enable_checking= ;
			ac_gc_check_stringbytes= ;
	                ac_gc_check_string_overrun= ;
	                ac_gc_check_string_free_list= ;
	                ac_xmalloc_overrun= ;
	                ac_gc_check_cons_list= ;
			ac_glyphs_debug= ;;
	all)		ac_enable_checking=1 ;
			ac_gc_check_stringbytes=1 ;
	                ac_gc_check_string_overrun=1 ;
	                ac_gc_check_string_free_list=1 ;
	                ac_xmalloc_overrun=1 ;
	                ac_gc_check_cons_list=1 ;
			ac_glyphs_debug=1 ;;
	# these enable particular checks
	stringbytes) 	ac_gc_check_stringbytes=1 ;;
	stringoverrun) 	ac_gc_check_string_overrun=1 ;;
	stringfreelist) ac_gc_check_string_free_list=1 ;;
	xmallocoverrun)	ac_xmalloc_overrun=1 ;;
	conslist)	ac_gc_check_cons_list=1 ;;
	glyphs)		ac_glyphs_debug=1 ;;
	*)	AC_MSG_ERROR(unknown check category $check) ;;
	esac
done
IFS="$ac_save_IFS"

if test x$ac_enable_checking != x ; then
  AC_DEFINE(ENABLE_CHECKING, 1,
[Define to 1 if expensive run-time data type and consistency checks are enabled.])
fi
if test x$ac_gc_check_stringbytes != x ; then
  AC_DEFINE(GC_CHECK_STRING_BYTES, 1,
[Define this temporarily to hunt a bug.  If defined, the size of
   strings is redundantly recorded in sdata structures so that it can
   be compared to the sizes recorded in Lisp strings.])
fi
if test x$ac_gc_check_string_overrun != x ; then
  AC_DEFINE(GC_CHECK_STRING_OVERRUN, 1,
[Define this to check for short string overrun.])
fi
if test x$ac_gc_check_string_free_list != x ; then
  AC_DEFINE(GC_CHECK_STRING_FREE_LIST, 1,
[Define this to check the string free list.])
fi
if test x$ac_xmalloc_overrun != x ; then
  AC_DEFINE(XMALLOC_OVERRUN_CHECK, 1,
[Define this to check for malloc buffer overrun.])
fi
if test x$ac_gc_check_cons_list != x ; then
  AC_DEFINE(GC_CHECK_CONS_LIST, 1,
[Define this to check for errors in cons list.])
fi
if test x$ac_glyphs_debug != x ; then
  AC_DEFINE(GLYPH_DEBUG, 1,
[Define this to enable glyphs debugging code.])
fi

AC_ARG_ENABLE(check-lisp-object-type,
[AS_HELP_STRING([--enable-check-lisp-object-type],
                [enable compile time checks for the Lisp_Object data type.
		This is useful for development for catching certain types of bugs.])],
if test "${enableval}" != "no"; then
   AC_DEFINE(CHECK_LISP_OBJECT_TYPE, 1,
   [Define this to enable compile time checks for the Lisp_Object data type.])
fi)


dnl The name of this option is unfortunate.  It predates, and has no
dnl relation to, the "sampling-based elisp profiler" added in 24.3.
dnl Actually, it stops it working.
dnl http://lists.gnu.org/archive/html/emacs-devel/2012-11/msg00393.html
AC_ARG_ENABLE(profiling,
[AS_HELP_STRING([--enable-profiling],
		[build emacs with low-level, gprof profiling support.
                Mainly useful for debugging Emacs itself.  May not work on
                all platforms.  Stops profiler.el working.])],
[ac_enable_profiling="${enableval}"],[])
if test x$ac_enable_profiling != x ; then
   PROFILING_CFLAGS="-DPROFILING=1 -pg"
else
   PROFILING_CFLAGS=
fi
AC_SUBST(PROFILING_CFLAGS)

AC_ARG_ENABLE(autodepend,
[AS_HELP_STRING([--enable-autodepend],
		[automatically generate dependencies to .h-files.
 		 Requires GNU Make and Gcc. Enabled if GNU Make and Gcc is
 		 found])],
[ac_enable_autodepend="${enableval}"],[ac_enable_autodepend=yes])

#### Make srcdir absolute, if it isn't already.  It's important to
#### avoid running the path through pwd unnecessarily, since pwd can
#### give you automounter prefixes, which can go away.  We do all this
#### so Emacs can find its files when run uninstalled.
## Make sure CDPATH doesn't affect cd (in case PWD is relative).
unset CDPATH
case "${srcdir}" in
  /* ) ;;
  . )
    ## We may be able to use the $PWD environment variable to make this
    ## absolute.  But sometimes PWD is inaccurate.
    ## Note: we used to use ${PWD} at the end instead of `pwd`,
    ## but that tested only for a well-formed and valid PWD,
    ## it did not object when PWD was well-formed and valid but just wrong.
    if test ".${PWD}" != "." && test ".`(cd ${PWD} ; sh -c pwd)`" = ".`pwd`"  ;
    then
      srcdir="$PWD"
    else
      srcdir="`(cd ${srcdir}; pwd)`"
    fi
  ;;
  *  ) srcdir="`(cd ${srcdir}; pwd)`" ;;
esac

### Canonicalize the configuration name.

AC_CANONICAL_HOST
canonical=$host
configuration=${host_alias-${build_alias-$host}}

dnl This used to use changequote, but, apart from `changequote is evil'
dnl per the autoconf manual, we can speed up autoconf somewhat by quoting
dnl the great gob of text.  Thus it's not processed for possible expansion.
dnl Just make sure the brackets remain balanced.
dnl
dnl Since Emacs can't find matching pairs of quotes, boundaries are
dnl indicated by comments.
dnl quotation begins
[

### If you add support for a new configuration, add code to this
### switch statement to recognize your configuration name and select
### the appropriate operating system file.

### You would hope that you could choose an s/*.h
### file based on the operating system portion.  However, it turns out
### that each s/*.h file is pretty manufacturer-specific.
### So we basically have to have a special case for each
### configuration name.
###
### As far as handling version numbers on operating systems is
### concerned, make sure things will fail in a fixable way.  If
### /etc/MACHINES doesn't say anything about version numbers, be
### prepared to handle anything reasonably.  If version numbers
### matter, be sure /etc/MACHINES says something about it.

opsys='' unported=no
case "${canonical}" in

  ## GNU/Linux and similar ports
  *-*-linux* )
    opsys=gnu-linux
  ;;

  ## FreeBSD ports
  *-*-freebsd* )
    opsys=freebsd
  ;;

  ## FreeBSD kernel + glibc based userland
  *-*-kfreebsd*gnu* )
    opsys=gnu-kfreebsd
  ;;

  ## NetBSD ports
  *-*-netbsd* )
    opsys=netbsd
  ;;

  ## OpenBSD ports
  *-*-openbsd* )
    opsys=openbsd
  ;;

  ## Apple Darwin / Mac OS X
  *-apple-darwin* )
    case "${canonical}" in
      i[3456]86-* )  ;;
      powerpc-* )    ;;
      x86_64-* )     ;;
      * )            unported=yes ;;
    esac
    opsys=darwin
    ## Use fink packages if available.
    ## FIXME find a better way to do this: http://debbugs.gnu.org/11507
##    if test -d /sw/include && test -d /sw/lib; then
##      GCC_TEST_OPTIONS="-I/sw/include -L/sw/lib"
##      NON_GCC_TEST_OPTIONS=${GCC_TEST_OPTIONS}
##    fi
  ;;

  ## HP 9000 series 700 and 800, running HP/UX
  hppa*-hp-hpux10.2* )
    opsys=hpux10-20
  ;;
  hppa*-hp-hpux1[1-9]* )
    opsys=hpux11
    CFLAGS="-D_INCLUDE__STDC_A1_SOURCE $CFLAGS"
  ;;

  ## IBM machines
  rs6000-ibm-aix4.[23]* )
    opsys=aix4-2
  ;;
  powerpc-ibm-aix4.[23]*  )
    opsys=aix4-2
  ;;
  rs6000-ibm-aix[56]* )
    opsys=aix4-2
  ;;
  powerpc-ibm-aix[56]*  )
    opsys=aix4-2
  ;;

  ## Silicon Graphics machines
  ## Iris 4D
  mips-sgi-irix6.5 )
    opsys=irix6-5
    # Without defining _LANGUAGE_C, things get masked out in the headers
    # so that, for instance, grepping for `free' in stdlib.h fails and
    # AC_HEADER_STD_C fails.   (MIPSPro 7.2.1.2m compilers, Irix 6.5.3m).
    NON_GCC_TEST_OPTIONS="-D_LANGUAGE_C"
  ;;

  ## Suns
  *-sun-solaris* \
    | i[3456]86-*-solaris2* | i[3456]86-*-sunos5* \
    | x86_64-*-solaris2*    | x86_64-*-sunos5*)
    case "${canonical}" in
      i[3456]86-*-* )   ;;
      amd64-*-*|x86_64-*-*) ;;
      sparc* )		;;
      * )		unported=yes ;;
    esac
    case "${canonical}" in
      *-sunos5.6* | *-solaris2.6* )
		opsys=sol2-6
		RANLIB="ar -ts"
		;;
      *-sunos5.[7-9]* | *-solaris2.[7-9]* )
		opsys=sol2-6
		emacs_check_sunpro_c=yes
		;;
      *-sunos5* | *-solaris* )
		opsys=sol2-10
		emacs_check_sunpro_c=yes
		;;
    esac
    ## Watch out for a compiler that we know will not work.
    case "${canonical}" in
     *-solaris* | *-sunos5* )
		if [ "x$CC" = x/usr/ucb/cc ]; then
		  ## /usr/ucb/cc doesn't work;
		  ## we should find some other compiler that does work.
		  unset CC
		fi
		;;
      *) ;;
    esac
  ;;

  ## Intel 386 machines where we don't care about the manufacturer.
  i[3456]86-*-* )
    case "${canonical}" in
      *-cygwin )                opsys=cygwin ;;
      *-darwin* )               opsys=darwin ;;
      *-sysv4.2uw* )	  	opsys=unixware ;;
      *-sysv5uw* )	  	opsys=unixware ;;
      *-sysv5OpenUNIX* )  	opsys=unixware ;;
      ## Otherwise, we'll fall through to the generic opsys code at the bottom.
    esac
  ;;

  * )
    unported=yes
  ;;
esac

### If the code above didn't choose an operating system, just choose
### an operating system based on the configuration name.  You really
### only want to use this when you have no idea what the right
### operating system is; if you know what operating systems a machine
### runs, it's cleaner to make it explicit in the case statement
### above.
if test x"${opsys}" = x; then
  case "${canonical}" in
    *-gnu* )				opsys=gnu ;;
    * )
      unported=yes
    ;;
  esac
fi

]
dnl quotation ends

if test $unported = yes; then
  AC_MSG_ERROR([Emacs hasn't been ported to `${canonical}' systems.
Check `etc/MACHINES' for recognized configuration names.])
fi


#### Choose a compiler.

dnl Sets GCC=yes if using gcc.
AC_PROG_CC
AM_PROG_CC_C_O

if test x$GCC = xyes; then
  test "x$GCC_TEST_OPTIONS" != x && CC="$CC $GCC_TEST_OPTIONS"
else
  test "x$NON_GCC_TEST_OPTIONS" != x && CC="$CC $NON_GCC_TEST_OPTIONS"
fi

# Avoid gnulib's threadlib module, as we do threads our own way.
AC_DEFUN([gl_THREADLIB])

# Initialize gnulib right after choosing the compiler.
dnl Amongst other things, this sets AR and ARFLAGS.
gl_EARLY

# It's helpful to have C macros available to GDB, so prefer -g3 to -g
# if -g3 works and the user does not specify CFLAGS.
# This test must follow gl_EARLY; otherwise AC_LINK_IFELSE complains.
if test "$ac_test_CFLAGS" != set; then
  case $CFLAGS in
    '-g')
      emacs_g3_CFLAGS='-g3';;
    '-g -O2')
      emacs_g3_CFLAGS='-g3 -O2';;
    *)
      emacs_g3_CFLAGS='';;
  esac
  if test -n "$emacs_g3_CFLAGS"; then
    emacs_save_CFLAGS=$CFLAGS
    CFLAGS=$emacs_g3_CFLAGS
    AC_CACHE_CHECK([whether $CC accepts $emacs_g3_CFLAGS],
      [emacs_cv_prog_cc_g3],
      [AC_LINK_IFELSE([AC_LANG_PROGRAM()],
	 [emacs_cv_prog_cc_g3=yes],
         [emacs_cv_prog_cc_g3=no])])
    if test $emacs_cv_prog_cc_g3 = yes; then
      CFLAGS=$emacs_g3_CFLAGS
    else
      CFLAGS=$emacs_save_CFLAGS
    fi
  fi
fi

AC_ARG_ENABLE([gcc-warnings],
  [AS_HELP_STRING([--enable-gcc-warnings],
                  [turn on lots of GCC warnings/errors. This is intended for
		   developers, and may generate false alarms when used
		   with older or non-GNU development tools.])],
  [case $enableval in
     yes|no) ;;
     *)      AC_MSG_ERROR([bad value $enableval for gcc-warnings option]) ;;
   esac
   gl_gcc_warnings=$enableval],
  [gl_gcc_warnings=no]
)

AC_ARG_ENABLE(link-time-optimization,
[AS_HELP_STRING([--enable-link-time-optimization],
                [build emacs with link-time optimization.
                 This is supported only for GCC since 4.5.0.])],
if test "${enableval}" != "no"; then
   AC_MSG_CHECKING([whether link-time optimization is supported])
   ac_lto_supported=no
   if test x$GCC = xyes; then
      CPUS=`getconf _NPROCESSORS_ONLN 2>/dev/null`
      if test x$CPUS != x; then
      	 LTO="-flto=$CPUS"
      else
	 LTO="-flto"
      fi
      old_CFLAGS=$CFLAGS
      CFLAGS="$CFLAGS $LTO"
      AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[]])],
      	[ac_lto_supported=yes], [ac_lto_supported=no])
      CFLAGS="$old_CFLAGS"
   fi
   AC_MSG_RESULT([$ac_lto_supported])
   if test "$ac_lto_supported" = "yes"; then
      CFLAGS="$CFLAGS $LTO"
   fi
fi)

# gl_GCC_VERSION_IFELSE([major], [minor], [run-if-found], [run-if-not-found])
# ------------------------------------------------
# If $CPP is gcc-MAJOR.MINOR or newer, then run RUN-IF-FOUND.
# Otherwise, run RUN-IF-NOT-FOUND.
AC_DEFUN([gl_GCC_VERSION_IFELSE],
  [AC_PREPROC_IFELSE(
    [AC_LANG_PROGRAM(
      [[
#if ($1) < __GNUC__ || (($1) == __GNUC__ && ($2) <= __GNUC_MINOR__)
/* ok */
#else
# error "your version of gcc is older than $1.$2"
#endif
      ]]),
    ], [$3], [$4])
  ]
)

# When compiling with GCC, prefer -isystem to -I when including system
# include files, to avoid generating useless diagnostics for the files.
if test "$gl_gcc_warnings" != yes; then
  isystem='-I'
else
  isystem='-isystem '

  # This, $nw, is the list of warnings we disable.
  nw=

  case $with_x_toolkit in
    lucid | athena | motif)
       # Old toolkits mishandle 'const'.
       nw="$nw -Wwrite-strings"
       ;;
    *)
       gl_WARN_ADD([-Werror], [WERROR_CFLAGS])
       ;;
  esac
  AC_SUBST([WERROR_CFLAGS])

  nw="$nw -Waggregate-return"       # anachronistic
  nw="$nw -Wlong-long"              # C90 is anachronistic
  nw="$nw -Wc++-compat"             # We don't care about C++ compilers
  nw="$nw -Wundef"                  # Warns on '#if GNULIB_FOO' etc in gnulib
  nw="$nw -Wtraditional"            # Warns on #elif which we use often
  nw="$nw -Wcast-qual"              # Too many warnings for now
  nw="$nw -Wconversion"             # Too many warnings for now
  nw="$nw -Wsystem-headers"         # Don't let system headers trigger warnings
  nw="$nw -Wsign-conversion"        # Too many warnings for now
  nw="$nw -Woverlength-strings"     # Not a problem these days
  nw="$nw -Wtraditional-conversion" # Too many warnings for now
  nw="$nw -Wunreachable-code"       # so buggy that it's now silently ignored
  nw="$nw -Wpadded"                 # Our structs are not padded
  nw="$nw -Wredundant-decls"        # we regularly (re)declare functions
  nw="$nw -Wlogical-op"             # any use of fwrite provokes this
  nw="$nw -Wformat-nonliteral"      # we do this a lot
  nw="$nw -Wvla"                    # warnings in gettext.h
  nw="$nw -Wnested-externs"         # use of XARGMATCH/verify_function__
  nw="$nw -Wswitch-enum"            # Too many warnings for now
  nw="$nw -Wswitch-default"         # Too many warnings for now
  nw="$nw -Wfloat-equal"            # warns about high-quality code
  nw="$nw -Winline"                 # OK to ignore 'inline'
  nw="$nw -Wjump-misses-init"       # We sometimes safely jump over init.
  nw="$nw -Wsync-nand"              # irrelevant here, and provokes ObjC warning
  nw="$nw -Wunsafe-loop-optimizations" # OK to suppress unsafe optimizations

  # Emacs doesn't care about shadowing; see
  # <http://lists.gnu.org/archive/html/emacs-diffs/2011-11/msg00265.html>.
  nw="$nw -Wshadow"

  # The following lines should be removable at some point.
  nw="$nw -Wstack-protector"
  nw="$nw -Wstrict-overflow"
  nw="$nw -Wsuggest-attribute=pure"

  gl_MANYWARN_ALL_GCC([ws])
  gl_MANYWARN_COMPLEMENT([ws], [$ws], [$nw])
  for w in $ws; do
    gl_WARN_ADD([$w])
  done
  gl_WARN_ADD([-Wno-missing-field-initializers]) # We need this one
  gl_WARN_ADD([-Wno-sign-compare])     # Too many warnings for now
  gl_WARN_ADD([-Wno-type-limits])      # Too many warnings for now
  gl_WARN_ADD([-Wno-switch])           # Too many warnings for now
  gl_WARN_ADD([-Wno-unused-parameter]) # Too many warnings for now
  gl_WARN_ADD([-Wno-format-nonliteral])

  # In spite of excluding -Wlogical-op above, it is enabled, as of
  # gcc 4.5.0 20090517.
  gl_WARN_ADD([-Wno-logical-op])

  gl_WARN_ADD([-fdiagnostics-show-option])
  gl_WARN_ADD([-funit-at-a-time])

  AC_DEFINE([lint], [1], [Define to 1 if the compiler is checking for lint.])
  AH_VERBATIM([FORTIFY_SOURCE],
  [/* Enable compile-time and run-time bounds-checking, and some warnings,
      without upsetting glibc 2.15+. */
   #if !defined _FORTIFY_SOURCE && defined __OPTIMIZE__ && __OPTIMIZE__
   # define _FORTIFY_SOURCE 2
   #endif
  ])
  AC_DEFINE([GNULIB_PORTCHECK], [1], [enable some gnulib portability checks])

  # We use a slightly smaller set of warning options for lib/.
  # Remove the following and save the result in GNULIB_WARN_CFLAGS.
  nw=
  nw="$nw -Wunused-macros"

  gl_MANYWARN_COMPLEMENT([GNULIB_WARN_CFLAGS], [$WARN_CFLAGS], [$nw])
  AC_SUBST([GNULIB_WARN_CFLAGS])
fi



dnl Some other nice autoconf tests.
dnl These are commented out, since gl_EARLY and/or Autoconf already does them.
dnl AC_PROG_INSTALL
dnl AC_PROG_MKDIR_P
dnl if test "x$RANLIB" = x; then
dnl   AC_PROG_RANLIB
dnl fi
AC_PROG_LN_S

AC_PATH_PROG(INSTALL_INFO, install-info, :,
  $PATH$PATH_SEPARATOR/usr/sbin$PATH_SEPARATOR/sbin)
dnl Don't use GZIP, which is used by gzip for additional parameters.
AC_PATH_PROG(GZIP_PROG, gzip)

if test $opsys = gnu-linux; then
  AC_PATH_PROG(PAXCTL, paxctl,,
    [$PATH$PATH_SEPARATOR/sbin$PATH_SEPARATOR/usr/sbin])
  if test "X$PAXCTL" != X; then
    AC_MSG_CHECKING([whether binaries have a PT_PAX_FLAGS header])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])],
      [if $PAXCTL -v conftest$EXEEXT >/dev/null 2>&1; then AC_MSG_RESULT(yes)
      else AC_MSG_RESULT(no); PAXCTL=""; fi])
  fi
fi

## Need makeinfo >= 4.7 (?) to build the manuals.
AC_PATH_PROG(MAKEINFO, makeinfo, no)
dnl By this stage, configure has already checked for egrep and set EGREP,
dnl or exited with an error if no egrep was found.
if test "$MAKEINFO" != "no" && \
  test x"`$MAKEINFO --version 2> /dev/null | $EGREP 'texinfo[[^0-9]]*([[1-4]][[0-9]]+|[[5-9]]|4\.[[7-9]]|4\.[[1-6]][[0-9]]+)'`" = x; then
   MAKEINFO=no
fi

## Makeinfo is unusual.  For a released Emacs, the manuals are
## pre-built, and not deleted by the normal clean rules.  makeinfo is
## therefore in the category of "special tools" not normally required, which
## configure does not have to check for (eg autoconf itself).
## In a Bazaar checkout on the other hand, the manuals are not included.
## So makeinfo is a requirement to build from Bazaar, and configure
## should test for it as it does for any other build requirement.
## We use the presence of $srcdir/info/emacs to distinguish a release,
## with pre-built manuals, from a Bazaar checkout.
HAVE_MAKEINFO=yes

if test "$MAKEINFO" = "no"; then
  MAKEINFO=makeinfo
  if test "x${with_makeinfo}" = "xno"; then
    HAVE_MAKEINFO=no
  elif test ! -e $srcdir/info/emacs && test ! -e $srcdir/info/emacs.info; then
    AC_MSG_ERROR( [You do not seem to have makeinfo >= 4.7, and your
source tree does not seem to have pre-built manuals in the `info' directory.
Either install a suitable version of makeinfo, or re-run configure
with the `--without-makeinfo' option to build without the manuals.] )
  fi
fi
AC_SUBST(HAVE_MAKEINFO)

dnl Just so that there is only a single place we need to edit.
INFO_EXT=.info
INFO_OPTS=--no-split
AC_SUBST(INFO_EXT)
AC_SUBST(INFO_OPTS)

dnl Add our options to ac_link now, after it is set up.

if test x$GCC = xyes; then
  test "x$GCC_LINK_TEST_OPTIONS" != x && \
    ac_link="$ac_link $GCC_LINK_TEST_OPTIONS"
else
  test "x$NON_GCC_LINK_TEST_OPTIONS" != x && \
    ac_link="$ac_link $NON_GCC_LINK_TEST_OPTIONS"
fi

dnl We need -znocombreloc if we're using a relatively recent GNU ld.
dnl If we can link with the flag, it shouldn't do any harm anyhow.
dnl (Don't use `-z nocombreloc' as -z takes no arg on Irix.)
dnl Treat GCC specially since it just gives a non-fatal `unrecognized option'
dnl if not built to support GNU ld.

late_LDFLAGS=$LDFLAGS
if test x$GCC = xyes; then
  LDFLAGS="$LDFLAGS -Wl,-znocombreloc"
else
  LDFLAGS="$LDFLAGS -znocombreloc"
fi

AC_MSG_CHECKING([for -znocombreloc])
AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])],
  [AC_MSG_RESULT(yes)],
  LDFLAGS=$late_LDFLAGS
  [AC_MSG_RESULT(no)])


dnl The function dump-emacs will not be defined and temacs will do
dnl (load "loadup") automatically unless told otherwise.
test "x$CANNOT_DUMP" = "x" && CANNOT_DUMP=no
case "$opsys" in
  your-opsys-here) CANNOT_DUMP=yes ;;
esac

test "$CANNOT_DUMP" = "yes" && \
  AC_DEFINE(CANNOT_DUMP, 1, [Define if Emacs cannot be dumped on your system.])

AC_SUBST(CANNOT_DUMP)


UNEXEC_OBJ=unexelf.o
case "$opsys" in
  # MSDOS uses unexcoff.o
  # MSWindows uses unexw32.o
  aix4-2)
   UNEXEC_OBJ=unexaix.o
   ;;
  cygwin)
   UNEXEC_OBJ=unexcw.o
   ;;
  darwin)
   UNEXEC_OBJ=unexmacosx.o
   ;;
  hpux10-20 | hpux11)
   UNEXEC_OBJ=unexhp9k800.o
   ;;
  sol2-10)
   # Use the Solaris dldump() function, called from unexsol.c, to dump
   # emacs, instead of the generic ELF dump code found in unexelf.c.
   # The resulting binary has a complete symbol table, and is better
   # for debugging and other observability tools (debuggers, pstack, etc).
   #
   # If you encounter a problem using dldump(), please consider sending
   # a message to the OpenSolaris tools-linking mailing list:
   #      http://mail.opensolaris.org/mailman/listinfo/tools-linking
   #
   # It is likely that dldump() works with older Solaris too, but this has
   # not been tested, so for now this change is for Solaris 10 or newer.
   UNEXEC_OBJ=unexsol.o
   ;;
esac

LD_SWITCH_SYSTEM=
case "$opsys" in
  freebsd)
   ## Let `ld' find image libs and similar things in /usr/local/lib.
   ## The system compiler, GCC, has apparently been modified to not
   ## look there, contrary to what a stock GCC would do.
### It's not our place to do this.  See bug#10313#17.
###   LD_SWITCH_SYSTEM=-L/usr/local/lib
      :
   ;;

  gnu-linux)
   ## cpp test was "ifdef __mips__", but presumably this is equivalent...
   case $host_cpu in mips*) LD_SWITCH_SYSTEM="-G 0";; esac
   ;;

  netbsd)
### It's not our place to do this.  See bug#10313#17.
###   LD_SWITCH_SYSTEM="-Wl,-rpath,/usr/pkg/lib -L/usr/pkg/lib -Wl,-rpath,/usr/local/lib -L/usr/local/lib"
      :
   ;;

  openbsd)
   ## Han Boetes <han@mijncomputer.nl> says this is necessary,
   ## otherwise Emacs dumps core on elf systems.
   LD_SWITCH_SYSTEM="-Z"
   ;;
esac
AC_SUBST(LD_SWITCH_SYSTEM)

ac_link="$ac_link $LD_SWITCH_SYSTEM"

## This setting of LD_SWITCH_SYSTEM references LD_SWITCH_X_SITE_RPATH,
## which has not been defined yet.  When this was handled with cpp,
## it was expanded to null when configure sourced the s/*.h file.
## Thus LD_SWITCH_SYSTEM had different values in configure and the Makefiles.
## FIXME it would be cleaner to put this in LD_SWITCH_SYSTEM_TEMACS
## (or somesuch), but because it is supposed to go at the _front_
## of LD_SWITCH_SYSTEM, we cannot do that in exactly the same way.
## Compare with the gnu-linux case below, which added to the end
## of LD_SWITCH_SYSTEM, and so can instead go at the front of
## LD_SWITCH_SYSTEM_TEMACS.
case "$opsys" in
  netbsd|openbsd)
   LD_SWITCH_SYSTEM="\$(LD_SWITCH_X_SITE_RPATH) $LD_SWITCH_SYSTEM" ;;
esac


C_SWITCH_MACHINE=
case $canonical in
 alpha*)
  AC_CHECK_DECL([__ELF__])
  if test "$ac_cv_have_decl___ELF__" = "yes"; then
    ## With ELF, make sure that all common symbols get allocated to in the
    ## data section.  Otherwise, the dump of temacs may miss variables in
    ## the shared library that have been initialized.  For example, with
    ## GNU libc, __malloc_initialized would normally be resolved to the
    ## shared library's .bss section, which is fatal.
    if test "x$GCC" = "xyes"; then
      C_SWITCH_MACHINE="-fno-common"
    else
      AC_MSG_ERROR([What gives?  Fix me if DEC Unix supports ELF now.])
    fi
  else
    UNEXEC_OBJ=unexalpha.o
  fi
  ;;
esac
AC_SUBST(C_SWITCH_MACHINE)

AC_SUBST(UNEXEC_OBJ)

C_SWITCH_SYSTEM=
## Some programs in src produce warnings saying certain subprograms
## are too complex and need a MAXMEM value greater than 2000 for
## additional optimization.  --nils@exp-math.uni-essen.de
test "$opsys" = "aix4.2" && test "x$GCC" != "xyes" && \
  C_SWITCH_SYSTEM="-ma -qmaxmem=4000"
## gnu-linux might need -D_BSD_SOURCE on old libc5 systems.
## It is redundant in glibc2, since we define _GNU_SOURCE.
AC_SUBST(C_SWITCH_SYSTEM)


LIBS_SYSTEM=
case "$opsys" in
  ## IBM's X11R5 uses -lIM and -liconv in AIX 3.2.2.
  aix4-2) LIBS_SYSTEM="-lrts -lIM -liconv" ;;

  freebsd) LIBS_SYSTEM="-lutil" ;;

  hpux*) LIBS_SYSTEM="-l:libdld.sl" ;;

  sol2*) LIBS_SYSTEM="-lsocket -lnsl" ;;

  ## Motif needs -lgen.
  unixware) LIBS_SYSTEM="-lsocket -lnsl -lelf -lgen" ;;
esac
AC_SUBST(LIBS_SYSTEM)


### Make sure subsequent tests use flags consistent with the build flags.

if test x"${OVERRIDE_CPPFLAGS}" != x; then
  CPPFLAGS="${OVERRIDE_CPPFLAGS}"
else
  CPPFLAGS="$C_SWITCH_SYSTEM $C_SWITCH_MACHINE $CPPFLAGS"
fi

# Suppress obsolescent Autoconf test for size_t; Emacs assumes C89 or better.
AC_DEFUN([AC_TYPE_SIZE_T])
# Likewise for obsolescent test for uid_t, gid_t; Emacs assumes them.
AC_DEFUN([AC_TYPE_UID_T])


LIB_MATH=-lm
LIB_STANDARD=
START_FILES=
dnl Current possibilities handled by sed (aix4-2 -> aix,
dnl gnu-linux -> gnu/linux, etc.):
dnl gnu, gnu/linux, gnu/kfreebsd, aix, cygwin, darwin, hpux, irix.
dnl And special cases: berkeley-unix, usg-unix-v, ms-dos, windows-nt.
SYSTEM_TYPE=`echo $opsys | sed -e 's/[[0-9]].*//' -e 's|-|/|'`

dnl NB do not use CRT_DIR unquoted here, since it might not be set yet.
case $opsys in
  cygwin )
    LIB_MATH=
    START_FILES='pre-crt0.o'
    ;;
  darwin )
    ## Adding -lm confuses the dynamic linker, so omit it.
    LIB_MATH=
    START_FILES='pre-crt0.o'
    ;;
  freebsd )
    LIB_STANDARD='-lgcc -lc -lgcc $(CRT_DIR)/crtn.o'
    START_FILES='pre-crt0.o $(CRT_DIR)/crt1.o $(CRT_DIR)/crti.o'
    SYSTEM_TYPE=berkeley-unix
    ;;
  gnu-linux | gnu-kfreebsd )
    LIB_STANDARD='-lgcc -lc -lgcc $(CRT_DIR)/crtn.o'
    START_FILES='pre-crt0.o $(CRT_DIR)/crt1.o $(CRT_DIR)/crti.o'
    ;;
  hpux10-20 | hpux11 )
    LIB_STANDARD=-lc
    START_FILES='pre-crt0.o $(CRT_DIR)/crt0.o'
    ;;
  dnl NB this may be adjusted below.
  netbsd | openbsd )
    LIB_STANDARD='-lgcc -lc -lgcc $(CRT_DIR)/crtend.o'
    START_FILES='pre-crt0.o $(CRT_DIR)/crt0.o $(CRT_DIR)/crtbegin.o'
    SYSTEM_TYPE=berkeley-unix
    ;;

  sol2* | unixware )
    SYSTEM_TYPE=usg-unix-v
    ;;

esac

AC_SUBST(LIB_MATH)
AC_SUBST(START_FILES)
AC_DEFINE_UNQUOTED(SYSTEM_TYPE, "$SYSTEM_TYPE",
  [The type of system you are compiling for; sets `system-type'.])

dnl Not all platforms use crtn.o files.  Check if the current one does.
crt_files=

for file in x $LIB_STANDARD $START_FILES; do
  case "$file" in
    *CRT_DIR*) crt_files="$crt_files `echo $file | sed -e 's|.*/||'`" ;;
  esac
done

if test "x$crt_files" != x; then

  ## If user specified a crt-dir, use that unconditionally.
  crt_gcc=no

  if test "X$CRT_DIR" = "X"; then

    CRT_DIR=/usr/lib            # default

    case "$canonical" in
      x86_64-*-linux-gnu* | s390x-*-linux-gnu*)
      ## On x86-64 and s390x GNU/Linux distributions, the standard library
      ## can be in a variety of places.  We only try /usr/lib64 and /usr/lib.
      ## For anything else (eg /usr/lib32), it is up the user to specify
      ## the location (bug#5655).
      ## Test for crtn.o, not just the directory, because sometimes the
      ## directory exists but does not have the relevant files (bug#1287).
      ## FIXME better to test for binary compatibility somehow.
      test -e /usr/lib64/crtn.o && CRT_DIR=/usr/lib64
      ;;

      powerpc64-*-linux-gnu* | sparc64-*-linux-gnu*) CRT_DIR=/usr/lib64 ;;
    esac

    case "$opsys" in
      hpux10-20) CRT_DIR=/lib ;;
    esac

    test "x${GCC}" = xyes && crt_gcc=yes

  fi                            # CRT_DIR = ""

  crt_missing=

  for file in $crt_files; do

    ## If we're using gcc, try to determine it automatically by asking
    ## gcc.  [If this doesn't work, CRT_DIR will remain at the
    ## system-dependent default from above.]
    if test $crt_gcc = yes && test ! -e $CRT_DIR/$file; then

       crt_file=`$CC --print-file-name=$file 2>/dev/null`
       case "$crt_file" in
         */*)
           CRT_DIR=`AS_DIRNAME(["$crt_file"])`
           ;;
       esac
    fi

    dnl We expect all the files to be in a single directory, so after the
    dnl first there is no point asking gcc.
    crt_gcc=no

    test -e $CRT_DIR/$file || crt_missing="$crt_missing $file"
  done                          # $crt_files

  test "x$crt_missing" = x || \
    AC_MSG_ERROR([Required file(s) not found:$crt_missing
Try using the --with-crt-dir option.])

fi                              # crt_files != ""

AC_SUBST(CRT_DIR)

case $opsys in
  netbsd | openbsd )
    if test -f $CRT_DIR/crti.o; then

        test -f $CRT_DIR/crtn.o || \
          AC_MSG_ERROR([Required file not found: crtn.o])

        LIB_STANDARD='-lgcc -lc -lgcc $(CRT_DIR)/crtend.o $(CRT_DIR)/crtn.o'
        START_FILES='pre-crt0.o $(CRT_DIR)/crt0.o $(CRT_DIR)/crti.o $(CRT_DIR)/crtbegin.o'
    fi
    ;;
esac


pre_PKG_CONFIG_CFLAGS=$CFLAGS
pre_PKG_CONFIG_LIBS=$LIBS

AC_PATH_PROG(PKG_CONFIG, pkg-config, no)

dnl This function definition taken from Gnome 2.0
dnl PKG_CHECK_MODULES(GSTUFF, gtk+-2.0 >= 1.3 glib = 1.3.4, action-if, action-not)
dnl defines GSTUFF_LIBS, GSTUFF_CFLAGS, see pkg-config man page
dnl also defines GSTUFF_PKG_ERRORS on error
AC_DEFUN([PKG_CHECK_MODULES], [
  succeeded=no

  if test "$PKG_CONFIG" = "no" ; then
     ifelse([$4], , [AC_MSG_ERROR([
      *** The pkg-config script could not be found. Make sure it is in your path, or give the full path to pkg-config with the PKG_CONFIG environment variable or --with-pkg-config-prog.  Or see http://www.freedesktop.org/software/pkgconfig to get pkg-config.])], [$4])
  else
     PKG_CONFIG_MIN_VERSION=0.9.0
     if $PKG_CONFIG --atleast-pkgconfig-version $PKG_CONFIG_MIN_VERSION; then
        AC_MSG_CHECKING(for $2)

        if $PKG_CONFIG --exists "$2" 2>&AS_MESSAGE_LOG_FD &&
	   $1_CFLAGS=`$PKG_CONFIG --cflags "$2" 2>&AS_MESSAGE_LOG_FD` &&
	   $1_LIBS=`$PKG_CONFIG --libs "$2" 2>&AS_MESSAGE_LOG_FD`; then
	    edit_cflags="
	      s,///*,/,g
	      s/^/ /
	      s/ -I/ $isystem/g
	      s/^ //
	    "
	    $1_CFLAGS=`AS_ECHO(["$$1_CFLAGS"]) | sed -e "$edit_cflags"`
	    $1_LIBS=`AS_ECHO(["$$1_LIBS"]) | sed -e 's,///*,/,g'`
            AC_MSG_RESULT([yes CFLAGS='$$1_CFLAGS' LIBS='$$1_LIBS'])
            succeeded=yes
        else
            AC_MSG_RESULT(no)
            $1_CFLAGS=""
            $1_LIBS=""
            ## If we have a custom action on failure, don't print errors, but
	    ## do set a variable so people can do so.  Do it in a subshell
	    ## to capture any diagnostics in invoking pkg-config.
	    $1_PKG_ERRORS=`($PKG_CONFIG --print-errors "$2") 2>&1`
	    ifelse([$4], ,echo "$$1_PKG_ERRORS",)
        fi

        AC_SUBST($1_CFLAGS)
        AC_SUBST($1_LIBS)
     else
        echo "*** Your version of pkg-config is too old. You need version $PKG_CONFIG_MIN_VERSION or newer."
        echo "*** See http://www.freedesktop.org/software/pkgconfig"
     fi
  fi

  if test $succeeded = yes; then
     ifelse([$3], , :, [$3])
  else
     ifelse([$4], , [AC_MSG_ERROR([Library requirements ($2) not met; consider adjusting the PKG_CONFIG_PATH environment variable if your libraries are in a nonstandard prefix so pkg-config can find them.])], [$4])
  fi
])


if test "${with_sound}" != "no"; then
  # Sound support for GNU/Linux and the free BSDs.
  AC_CHECK_HEADERS(machine/soundcard.h sys/soundcard.h soundcard.h,
    have_sound_header=yes)
  # Emulation library used on NetBSD.
  AC_CHECK_LIB(ossaudio, _oss_ioctl, LIBSOUND=-lossaudio, LIBSOUND=)
  AC_SUBST(LIBSOUND)

  ALSA_REQUIRED=1.0.0
  ALSA_MODULES="alsa >= $ALSA_REQUIRED"
  PKG_CHECK_MODULES(ALSA, $ALSA_MODULES, HAVE_ALSA=yes, HAVE_ALSA=no)
  if test $HAVE_ALSA = yes; then
    SAVE_CFLAGS="$CFLAGS"
    SAVE_LIBS="$LIBS"
    CFLAGS="$ALSA_CFLAGS $CFLAGS"
    LIBS="$ALSA_LIBS $LIBS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <asoundlib.h>]], [[snd_lib_error_set_handler (0);]])],
                    emacs_alsa_normal=yes,
	            emacs_alsa_normal=no)
    if test "$emacs_alsa_normal" != yes; then
      AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <alsa/asoundlib.h>]],
                     [[snd_lib_error_set_handler (0);]])],
                     emacs_alsa_subdir=yes,
	             emacs_alsa_subdir=no)
      if test "$emacs_alsa_subdir" != yes; then
        AC_MSG_ERROR([pkg-config found alsa, but it does not compile.  See config.log for error messages.])
      fi
      ALSA_CFLAGS="$ALSA_CFLAGS -DALSA_SUBDIR_INCLUDE"
    fi

    CFLAGS="$SAVE_CFLAGS"
    LIBS="$SAVE_LIBS"
    LIBSOUND="$LIBSOUND $ALSA_LIBS"
    CFLAGS_SOUND="$CFLAGS_SOUND $ALSA_CFLAGS"
    AC_DEFINE(HAVE_ALSA, 1, [Define to 1 if ALSA is available.])
  fi

  dnl Define HAVE_SOUND if we have sound support.  We know it works and
  dnl compiles only on the specified platforms.  For others, it
  dnl probably doesn't make sense to try.
  if test x$have_sound_header = xyes || test $HAVE_ALSA = yes; then
     case "$opsys" in
       dnl defined __FreeBSD__ || defined __NetBSD__ || defined __linux__
       gnu-linux|freebsd|netbsd)
         AC_DEFINE(HAVE_SOUND, 1, [Define to 1 if you have sound support.])
         ;;
     esac
  fi

  AC_SUBST(CFLAGS_SOUND)
fi

dnl checks for header files
AC_CHECK_HEADERS_ONCE(
  linux/version.h sys/systeminfo.h
  stdio_ext.h fcntl.h coff.h pty.h
  sys/vlimit.h sys/resource.h
  sys/utsname.h pwd.h utmp.h dirent.h util.h)

AC_MSG_CHECKING(if personality LINUX32 can be set)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/personality.h>]], [[personality (PER_LINUX32)]])],
               emacs_cv_personality_linux32=yes,
	       emacs_cv_personality_linux32=no)
AC_MSG_RESULT($emacs_cv_personality_linux32)

if test $emacs_cv_personality_linux32 = yes; then
  AC_DEFINE(HAVE_PERSONALITY_LINUX32, 1,
            [Define to 1 if personality LINUX32 can be set.])
fi

dnl On Solaris 8 there's a compilation warning for term.h because
dnl it doesn't define `bool'.
AC_CHECK_HEADERS(term.h, , , -)
AC_HEADER_TIME
AC_CHECK_DECLS([sys_siglist], [], [], [[#include <signal.h>
			              ]])
if test $ac_cv_have_decl_sys_siglist != yes; then
  # For Tru64, at least:
  AC_CHECK_DECLS([__sys_siglist], [], [], [[#include <signal.h>
			                  ]])
  if test $ac_cv_have_decl___sys_siglist = yes; then
    AC_DEFINE(sys_siglist, __sys_siglist,
              [Define to any substitute for sys_siglist.])
  fi
fi
AC_HEADER_SYS_WAIT

dnl Check for speed_t typedef.
AC_CACHE_CHECK(for speed_t, emacs_cv_speed_t,
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <termios.h>]], [[speed_t x = 1;]])],
    emacs_cv_speed_t=yes, emacs_cv_speed_t=no)])
if test $emacs_cv_speed_t = yes; then
  AC_DEFINE(HAVE_SPEED_T, 1,
  	   [Define to 1 if `speed_t' is declared by <termios.h>.])
fi

AC_CHECK_HEADERS_ONCE(sys/socket.h)
AC_CHECK_HEADERS(net/if.h, , , [AC_INCLUDES_DEFAULT
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif])
AC_CHECK_HEADERS(ifaddrs.h, , , [AC_INCLUDES_DEFAULT
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif])
AC_CHECK_HEADERS(net/if_dl.h, , , [AC_INCLUDES_DEFAULT
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif])

dnl checks for structure members
AC_CHECK_MEMBERS([struct ifreq.ifr_flags, struct ifreq.ifr_hwaddr,
		  struct ifreq.ifr_netmask, struct ifreq.ifr_broadaddr,
		  struct ifreq.ifr_addr,
		  struct ifreq.ifr_addr.sa_len], , ,
		 [AC_INCLUDES_DEFAULT
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#if HAVE_NET_IF_H
#include <net/if.h>
#endif])

dnl Check for endianness.
dnl AC_C_BIGENDIAN is done by gnulib.

dnl check for Make feature
dnl AC_PROG_MAKE_SET is done by Automake.

DEPFLAGS=
MKDEPDIR=":"
deps_frag=deps.mk
dnl check for GNU Make if we have GCC and autodepend is on.
if test "$GCC" = yes && test "$ac_enable_autodepend" = yes; then
   AC_MSG_CHECKING([whether we are using GNU Make])
   HAVE_GNU_MAKE=no
   testval=`${MAKE-make} --version 2>/dev/null | grep 'GNU Make'`
   if test "x$testval" != x; then
      HAVE_GNU_MAKE=yes
   else
      ac_enable_autodepend=no
   fi
   AC_MSG_RESULT([$HAVE_GNU_MAKE])
   if test $HAVE_GNU_MAKE = yes; then
      AC_MSG_CHECKING([whether gcc understands -MMD -MF])
      SAVE_CFLAGS="$CFLAGS"
      CFLAGS="$CFLAGS -MMD -MF deps.d -MP"
      AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[]])], , ac_enable_autodepend=no)
      CFLAGS="$SAVE_CFLAGS"
      test -f deps.d || ac_enable_autodepend=no
      rm -rf deps.d
      AC_MSG_RESULT([$ac_enable_autodepend])
   fi
   if test $ac_enable_autodepend = yes; then
      DEPFLAGS='-MMD -MF ${DEPDIR}/$*.d -MP'
      ## MKDIR_P is documented (see AC_PROG_MKDIR_P) to be parallel-safe.
      MKDEPDIR='${MKDIR_P} ${DEPDIR}'
      deps_frag=autodeps.mk
   fi
fi
deps_frag=$srcdir/src/$deps_frag
AC_SUBST(MKDEPDIR)
AC_SUBST(DEPFLAGS)
AC_SUBST_FILE(deps_frag)


lisp_frag=$srcdir/src/lisp.mk
AC_SUBST_FILE(lisp_frag)


dnl checks for operating system services
AC_SYS_LONG_FILE_NAMES

#### Choose a window system.

## We leave window_system equal to none if
## we end up building without one.  Any new window system should
## set window_system to an appropriate value and add objects to
## window-system-specific substs.

window_system=none
AC_PATH_X
if test "$no_x" != yes; then
  window_system=x11
fi

LD_SWITCH_X_SITE_RPATH=
if test "${x_libraries}" != NONE; then
  if test -n "${x_libraries}"; then
    LD_SWITCH_X_SITE=-L`echo ${x_libraries} | sed -e "s/:/ -L/g"`
    LD_SWITCH_X_SITE_RPATH=-Wl,-rpath,`echo ${x_libraries} | sed -e "s/:/ -Wl,-rpath,/g"`
  fi
  x_default_search_path=""
  x_search_path=${x_libraries}
  if test -z "${x_search_path}"; then
    x_search_path=/usr/lib
  fi
  for x_library in `echo ${x_search_path}: | \
		    sed -e "s/:/ /g" -e p -e "s:/lib[[^ /]]* :/share :g"`; do
    x_search_path="\
${x_library}/X11/%L/%T/%N%C%S:\
${x_library}/X11/%l/%T/%N%C%S:\
${x_library}/X11/%T/%N%C%S:\
${x_library}/X11/%L/%T/%N%S:\
${x_library}/X11/%l/%T/%N%S:\
${x_library}/X11/%T/%N%S"
    if test x"${x_default_search_path}" = x; then
      x_default_search_path=${x_search_path}
    else
      x_default_search_path="${x_search_path}:${x_default_search_path}"
    fi
  done
fi
AC_SUBST(LD_SWITCH_X_SITE_RPATH)

if test "${x_includes}" != NONE && test -n "${x_includes}"; then
  C_SWITCH_X_SITE="$isystem"`echo ${x_includes} | sed -e "s/:/ $isystem/g"`
fi

if test x"${x_includes}" = x; then
  bitmapdir=/usr/include/X11/bitmaps
else
  # accumulate include directories that have X11 bitmap subdirectories
  bmd_acc="dummyval"
  for bmd in `echo ${x_includes} | sed -e "s/:/ /g"`; do
    if test -d "${bmd}/X11/bitmaps"; then
      bmd_acc="${bmd_acc}:${bmd}/X11/bitmaps"
    fi
    if test -d "${bmd}/bitmaps"; then
      bmd_acc="${bmd_acc}:${bmd}/bitmaps"
    fi
  done
  if test ${bmd_acc} != "dummyval"; then
    bitmapdir=`echo ${bmd_acc} | sed -e "s/^dummyval://"`
  fi
fi

HAVE_NS=no
NS_IMPL_COCOA=no
NS_IMPL_GNUSTEP=no
tmp_CPPFLAGS="$CPPFLAGS"
tmp_CFLAGS="$CFLAGS"
CPPFLAGS="$CPPFLAGS -x objective-c"
CFLAGS="$CFLAGS -x objective-c"
GNU_OBJC_CFLAGS=
LIBS_GNUSTEP=
if test "${with_ns}" != no; then
  if test "${opsys}" = darwin; then
     NS_IMPL_COCOA=yes
     ns_appdir=`pwd`/nextstep/Emacs.app
     ns_appbindir=${ns_appdir}/Contents/MacOS
     ns_appresdir=${ns_appdir}/Contents/Resources
     ns_appsrc=Cocoa/Emacs.base
  elif test -f $GNUSTEP_CONFIG_FILE; then
     NS_IMPL_GNUSTEP=yes
     ns_appdir=`pwd`/nextstep/Emacs.app
     ns_appbindir=${ns_appdir}
     ns_appresdir=${ns_appdir}/Resources
     ns_appsrc=GNUstep/Emacs.base
     dnl FIXME sourcing this several times in subshells seems inefficient.
     GNUSTEP_SYSTEM_HEADERS="$(. $GNUSTEP_CONFIG_FILE; echo $GNUSTEP_SYSTEM_HEADERS)"
     GNUSTEP_SYSTEM_LIBRARIES="$(. $GNUSTEP_CONFIG_FILE; echo $GNUSTEP_SYSTEM_LIBRARIES)"
     dnl I seemed to need these as well with GNUstep-startup 0.25.
     GNUSTEP_LOCAL_HEADERS="$(. $GNUSTEP_CONFIG_FILE; echo $GNUSTEP_LOCAL_HEADERS)"
     GNUSTEP_LOCAL_LIBRARIES="$(. $GNUSTEP_CONFIG_FILE; echo $GNUSTEP_LOCAL_LIBRARIES)"
     test "x${GNUSTEP_LOCAL_HEADERS}" != "x" && \
       GNUSTEP_LOCAL_HEADERS="-I${GNUSTEP_LOCAL_HEADERS}"
     test "x${GNUSTEP_LOCAL_LIBRARIES}" != "x" && \
       GNUSTEP_LOCAL_LIBRARIES="-L${GNUSTEP_LOCAL_LIBRARIES}"
     CPPFLAGS="$CPPFLAGS -I${GNUSTEP_SYSTEM_HEADERS} ${GNUSTEP_LOCAL_HEADERS}"
     CFLAGS="$CFLAGS -I${GNUSTEP_SYSTEM_HEADERS} ${GNUSTEP_LOCAL_HEADERS}"
     LDFLAGS="$LDFLAGS -L${GNUSTEP_SYSTEM_LIBRARIES} ${GNUSTEP_LOCAL_LIBRARIES}"
     LIBS_GNUSTEP="-lgnustep-gui -lgnustep-base -lobjc -lpthread"
     LIB_STANDARD=
     START_FILES=
     dnl GNUstep defines BASE_NATIVE_OBJC_EXCEPTIONS to 0 or 1.
     dnl If they had chosen to either define it or not, we could have
     dnl just used AC_CHECK_DECL here.
     AC_CACHE_CHECK(if GNUstep defines BASE_NATIVE_OBJC_EXCEPTIONS,
       emacs_cv_objc_exceptions,
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <GNUstepBase/GSConfig.h>]],
[[#if defined BASE_NATIVE_OBJC_EXCEPTIONS && BASE_NATIVE_OBJC_EXCEPTIONS > 0
1;
#else
fail;
#endif]])], emacs_cv_objc_exceptions=yes, emacs_cv_objc_exceptions=no ) )
     if test $emacs_cv_objc_exceptions = yes; then
       dnl _NATIVE_OBJC_EXCEPTIONS is used by the GNUstep headers.
       AC_DEFINE(_NATIVE_OBJC_EXCEPTIONS, 1,
         [Define if GNUstep uses ObjC exceptions.])
       GNU_OBJC_CFLAGS="-fobjc-exceptions"
     fi
  fi

  dnl This is only used while we test the NS headers, it gets reset below.
  CFLAGS="$CFLAGS $GNU_OBJC_CFLAGS"

  AC_CHECK_HEADER([AppKit/AppKit.h], [HAVE_NS=yes],
  		  [AC_MSG_ERROR([`--with-ns' was specified, but the include
  files are missing or cannot be compiled.])])

  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <AppKit/AppKit.h>],
                                     [
#ifdef MAC_OS_X_VERSION_MAX_ALLOWED
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1040
 ; /* OK */
#else
#error "OSX 10.4 or newer required"
#endif
#endif
		    ])],
		    ns_osx_have_104=yes,
		    ns_osx_have_104=no)
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <Foundation/NSObjCRuntime.h>],
                                     [NSInteger i;])],
		    ns_have_nsinteger=yes,
		    ns_have_nsinteger=no)
  if test $ns_osx_have_104 = no; then
     AC_MSG_ERROR([`OSX 10.4 or newer is required']);
  fi
  if test $ns_have_nsinteger = yes; then
    AC_DEFINE(NS_HAVE_NSINTEGER, 1, [Define to 1 if `NSInteger' is defined.])
  fi
fi

AC_SUBST(LIBS_GNUSTEP)

INSTALL_ARCH_INDEP_EXTRA=install-etc
ns_self_contained=no
NS_OBJ=
NS_OBJC_OBJ=
if test "${HAVE_NS}" = yes; then
  if test "$with_toolkit_scroll_bars" = "no"; then
    AC_MSG_ERROR([Non-toolkit scroll bars are not implemented for Nextstep.])
  fi

  window_system=nextstep
  # set up packaging dirs
  if test "${EN_NS_SELF_CONTAINED}" = yes; then
     ns_self_contained=yes
     prefix=${ns_appresdir}
     exec_prefix=${ns_appbindir}
     dnl This one isn't really used, only archlibdir is.
     libexecdir="\${ns_appbindir}/libexec"
     archlibdir="\${ns_appbindir}/libexec"
     docdir="\${ns_appresdir}/etc"
     etcdir="\${ns_appresdir}/etc"
     dnl FIXME maybe set datarootdir instead.
     dnl That would also get applications, icons, man.
     infodir="\${ns_appresdir}/info"
     mandir="\${ns_appresdir}/man"
     lispdir="\${ns_appresdir}/lisp"
     leimdir="\${ns_appresdir}/leim"
     INSTALL_ARCH_INDEP_EXTRA=
  fi
  NS_OBJC_OBJ="nsterm.o nsfns.o nsmenu.o nsselect.o nsimage.o nsfont.o"
fi
CFLAGS="$tmp_CFLAGS"
CPPFLAGS="$tmp_CPPFLAGS"
AC_SUBST(INSTALL_ARCH_INDEP_EXTRA)
AC_SUBST(ns_self_contained)
AC_SUBST(NS_OBJ)
AC_SUBST(NS_OBJC_OBJ)
AC_SUBST(LIB_STANDARD)

HAVE_W32=no
W32_OBJ=
W32_LIBS=
if test "${with_w32}" != no; then
  if test "${opsys}" != "cygwin"; then
    AC_MSG_ERROR([Using w32 with an autotools build is only supported for Cygwin.])
  fi
  AC_CHECK_HEADER([windows.h], [HAVE_W32=yes],
                  [AC_MSG_ERROR([`--with-w32' was specified, but windows.h
                  cannot be found.])])
  AC_DEFINE(HAVE_NTGUI, 1, [Define to use native MS Windows GUI.])
  W32_OBJ="w32fns.o w32menu.o w32reg.o w32font.o w32term.o"
  W32_OBJ="$W32_OBJ w32xfns.o w32select.o w32uniscribe.o"
  W32_LIBS="$W32_LIBS -lkernel32 -luser32 -lgdi32 -lole32 -lcomdlg32"
  W32_LIBS="$W32_LIBS -lusp10 -lcomctl32 -lwinspool"
fi
AC_SUBST(W32_OBJ)
AC_SUBST(W32_LIBS)

if test "${HAVE_W32}" = "yes"; then
  window_system=w32
  with_xft=no
fi

## $window_system is now set to the window system we will
## ultimately use.

term_header=
HAVE_X_WINDOWS=no
HAVE_X11=no
USE_X_TOOLKIT=none

case "${window_system}" in
  x11 )
    HAVE_X_WINDOWS=yes
    HAVE_X11=yes
    term_header=xterm.h
    case "${with_x_toolkit}" in
      athena | lucid ) USE_X_TOOLKIT=LUCID ;;
      motif ) USE_X_TOOLKIT=MOTIF ;;
      gtk ) with_gtk=yes
            term_header=gtkutil.h
dnl Don't set this for GTK.  A lot of tests below assumes Xt when
dnl USE_X_TOOLKIT is set.
            USE_X_TOOLKIT=none ;;
      gtk2 ) with_gtk2=yes
             term_header=gtkutil.h
             USE_X_TOOLKIT=none ;;
      gtk3 ) with_gtk3=yes
             term_header=gtkutil.h
             USE_X_TOOLKIT=none ;;
      no ) USE_X_TOOLKIT=none ;;
dnl If user did not say whether to use a toolkit, make this decision later:
dnl use the toolkit if we have gtk, or X11R5 or newer.
      * ) USE_X_TOOLKIT=maybe ;;
    esac
  ;;
  nextstep )
    term_header=nsterm.h
  ;;
  w32 )
    term_header=w32term.h
  ;;
esac

if test "$window_system" = none && test "X$with_x" != "Xno"; then
   AC_CHECK_PROG(HAVE_XSERVER, X, true, false)
   if test "$HAVE_XSERVER" = true ||
      test -n "$DISPLAY" ||
      test "`echo /usr/lib/libX11.*`" != "/usr/lib/libX11.*"; then
        AC_MSG_ERROR([You seem to be running X, but no X development libraries
were found.  You should install the relevant development files for X
and for the toolkit you want, such as Gtk+, Lesstif or Motif.  Also make
sure you have development files for image handling, i.e.
tiff, gif, jpeg, png and xpm.
If you are sure you want Emacs compiled without X window support, pass
  --without-x
to configure.])
   fi
fi

### If we're using X11, we should use the X menu package.
HAVE_MENUS=no
case ${HAVE_X11} in
  yes ) HAVE_MENUS=yes ;;
esac

# Does the opsystem file prohibit the use of the GNU malloc?
# Assume not, until told otherwise.
GNU_MALLOC=yes

AC_CACHE_CHECK(
  [whether malloc is Doug Lea style],
  [emacs_cv_var_doug_lea_malloc],
  [AC_LINK_IFELSE(
     [AC_LANG_PROGRAM(
        [[#include <malloc.h>
	  static void hook (void) {}]],
        [[malloc_set_state (malloc_get_state ());
	  __after_morecore_hook = hook;
	  __malloc_initialize_hook = hook;]])],
     [emacs_cv_var_doug_lea_malloc=yes],
     [emacs_cv_var_doug_lea_malloc=no])])
doug_lea_malloc=$emacs_cv_var_doug_lea_malloc


dnl See comments in aix4-2.h about maybe using system malloc there.
system_malloc=no
case "$opsys" in
  ## darwin ld insists on the use of malloc routines in the System framework.
  darwin|sol2-10) system_malloc=yes ;;
esac

if test "${system_malloc}" = "yes"; then
  AC_DEFINE(SYSTEM_MALLOC, 1, [Define to use system malloc.])
  GNU_MALLOC=no
  GNU_MALLOC_reason="
    (The GNU allocators don't work with this system configuration.)"
  GMALLOC_OBJ=
  VMLIMIT_OBJ=
else
  test "$doug_lea_malloc" != "yes" && GMALLOC_OBJ=gmalloc.o
  VMLIMIT_OBJ=vm-limit.o
fi
AC_SUBST(GMALLOC_OBJ)
AC_SUBST(VMLIMIT_OBJ)

if test "$doug_lea_malloc" = "yes" ; then
  if test "$GNU_MALLOC" = yes ; then
    GNU_MALLOC_reason="
      (Using Doug Lea's new malloc from the GNU C Library.)"
  fi
  AC_DEFINE(DOUG_LEA_MALLOC, 1,
	    [Define to 1 if you are using the GNU C Library.])

  ## Use mmap directly for allocating larger buffers.
  ## FIXME this comes from src/s/{gnu,gnu-linux}.h:
  ## #ifdef DOUG_LEA_MALLOC; #undef REL_ALLOC; #endif
  ## Does the AC_FUNC_MMAP test below make this check unnecessary?
  case "$opsys" in
    gnu*) REL_ALLOC=no ;;
  esac
fi

if test x"${REL_ALLOC}" = x; then
  REL_ALLOC=${GNU_MALLOC}
fi

use_mmap_for_buffers=no
case "$opsys" in
  cygwin|freebsd|irix6-5) use_mmap_for_buffers=yes ;;
esac

AC_FUNC_MMAP
if test $use_mmap_for_buffers = yes; then
  AC_DEFINE(USE_MMAP_FOR_BUFFERS, 1, [Define to use mmap to allocate buffer text.])
  REL_ALLOC=no
fi

LIBS="$LIBS_SYSTEM $LIBS"

dnl If found, this defines HAVE_LIBDNET, which m/pmax.h checks,
dnl and also adds -ldnet to LIBS, which Autoconf uses for checks.
AC_CHECK_LIB(dnet, dnet_ntoa)
dnl This causes -lresolv to get used in subsequent tests,
dnl which causes failures on some systems such as HPUX 9.
dnl AC_CHECK_LIB(resolv, gethostbyname)

dnl FIXME replace main with a function we actually want from this library.
AC_CHECK_LIB(Xbsd, main, LD_SWITCH_X_SITE="$LD_SWITCH_X_SITE -lXbsd")

dnl Check if pthreads is available.
LIB_PTHREAD=
AC_CHECK_HEADERS_ONCE(pthread.h)
if test "$ac_cv_header_pthread_h"; then
  dnl gmalloc.c uses pthread_atfork, which is not available on older-style
  dnl hosts such as MirBSD 10, so test for pthread_atfork instead of merely
  dnl testing for pthread_self if Emacs uses gmalloc.c.
  if test "$GMALLOC_OBJ" = gmalloc.o; then
    emacs_pthread_function=pthread_atfork
  else
    emacs_pthread_function=pthread_self
  fi
  AC_CHECK_LIB(pthread, $emacs_pthread_function, HAVE_PTHREAD=yes)
fi
if test "$HAVE_PTHREAD" = yes; then
  case "${canonical}" in
    *-hpux*) ;;
    *) LIB_PTHREAD="-lpthread"
       LIBS="$LIB_PTHREAD $LIBS" ;;
  esac
  AC_DEFINE(HAVE_PTHREAD, 1, [Define to 1 if you have pthread (-lpthread).])
fi
AC_SUBST([LIB_PTHREAD])

AC_CHECK_LIB(pthreads, cma_open)

## Note: when using cpp in s/aix4.2.h, this definition depended on
## HAVE_LIBPTHREADS.  That was not defined earlier in configure when
## the system file was sourced.  Hence the value of LIBS_SYSTEM
## added to LIBS in configure would never contain the pthreads part,
## but the value used in Makefiles might.  FIXME?
##
## -lpthreads seems to be necessary for Xlib in X11R6, and should
## be harmless on older versions of X where it happens to exist.
test "$opsys" = "aix4-2" && \
  test $ac_cv_lib_pthreads_cma_open = yes && \
  LIBS_SYSTEM="$LIBS_SYSTEM -lpthreads"

dnl Check for need for bigtoc support on IBM AIX

case ${host_os} in
aix*)
  AC_CACHE_CHECK([for -bbigtoc option], [gdb_cv_bigtoc], [
    case $GCC in
    yes) gdb_cv_bigtoc=-Wl,-bbigtoc ;;
    *) gdb_cv_bigtoc=-bbigtoc ;;
    esac

    LDFLAGS=$LDFLAGS\ $gdb_cv_bigtoc
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[int i;]])], [], [gdb_cv_bigtoc=])
  ])
  ;;
esac

# Change CFLAGS and CPPFLAGS temporarily so that C_SWITCH_X_SITE gets
# used for the tests that follow.  We set them back to REAL_CFLAGS and
# REAL_CPPFLAGS later on.

REAL_CFLAGS="$CFLAGS"
REAL_CPPFLAGS="$CPPFLAGS"

if test "${HAVE_X11}" = "yes"; then
  DEFS="$C_SWITCH_X_SITE $DEFS"
  LDFLAGS="$LDFLAGS $LD_SWITCH_X_SITE"
  LIBS="-lX11 $LIBS"
  CFLAGS="$C_SWITCH_X_SITE $CFLAGS"
  CPPFLAGS="$C_SWITCH_X_SITE $CPPFLAGS"

  # On Solaris, arrange for LD_RUN_PATH to point to the X libraries for tests.
  # This is handled by LD_SWITCH_X_SITE_RPATH during the real build,
  # but it's more convenient here to set LD_RUN_PATH since this
  # also works on hosts that don't understand LD_SWITCH_X_SITE_RPATH.
  if test "${x_libraries}" != NONE && test -n "${x_libraries}"; then
    LD_RUN_PATH=$x_libraries${LD_RUN_PATH+:}$LD_RUN_PATH
    export LD_RUN_PATH
  fi

  if test "${opsys}" = "gnu-linux"; then
    AC_MSG_CHECKING(whether X on GNU/Linux needs -b to link)
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[]],
     [[XOpenDisplay ("foo");]])],
     [xgnu_linux_first_failure=no],
     [xgnu_linux_first_failure=yes])
    if test "${xgnu_linux_first_failure}" = "yes"; then
      OLD_LD_SWITCH_X_SITE="$LD_SWITCH_X_SITE"
      OLD_C_SWITCH_X_SITE="$C_SWITCH_X_SITE"
      OLD_CPPFLAGS="$CPPFLAGS"
      OLD_LIBS="$LIBS"
      LD_SWITCH_X_SITE="$LD_SWITCH_X_SITE -b i486-linuxaout"
      C_SWITCH_X_SITE="$C_SWITCH_X_SITE -b i486-linuxaout"
      CPPFLAGS="$CPPFLAGS -b i486-linuxaout"
      LIBS="$LIBS -b i486-linuxaout"
      AC_LINK_IFELSE([AC_LANG_PROGRAM([[]],
       [[XOpenDisplay ("foo");]])],
       [xgnu_linux_second_failure=no],
       [xgnu_linux_second_failure=yes])
      if test "${xgnu_linux_second_failure}" = "yes"; then
	# If we get the same failure with -b, there is no use adding -b.
	# So take it out.  This plays safe.
	LD_SWITCH_X_SITE="$OLD_LD_SWITCH_X_SITE"
	C_SWITCH_X_SITE="$OLD_C_SWITCH_X_SITE"
	CPPFLAGS="$OLD_CPPFLAGS"
	LIBS="$OLD_LIBS"
        AC_MSG_RESULT(no)
      else
        AC_MSG_RESULT(yes)
      fi
    else
      AC_MSG_RESULT(no)
    fi
  fi

  # Reportedly, some broken Solaris systems have XKBlib.h but are missing
  # header files included from there.
  AC_MSG_CHECKING(for Xkb)
  AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <X11/Xlib.h>
#include <X11/XKBlib.h>]],
	[[XkbDescPtr kb = XkbGetKeyboard (0, XkbAllComponentsMask, XkbUseCoreKbd);]])],
	emacs_xkb=yes, emacs_xkb=no)
  AC_MSG_RESULT($emacs_xkb)
  if test $emacs_xkb = yes; then
    AC_DEFINE(HAVE_XKBGETKEYBOARD, 1, [Define to 1 if you have the XkbGetKeyboard function.])
  fi

  AC_CHECK_FUNCS(XrmSetDatabase XScreenResourceString \
XScreenNumberOfScreen)
fi

if test "${window_system}" = "x11"; then
  AC_MSG_CHECKING(X11 version 6)
  AC_CACHE_VAL(emacs_cv_x11_version_6,
  [AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <X11/Xlib.h>]],
[[#if XlibSpecificationRelease < 6
fail;
#endif
]])], emacs_cv_x11_version_6=yes, emacs_cv_x11_version_6=no)])
  if test $emacs_cv_x11_version_6 = yes; then
    AC_MSG_RESULT(6 or newer)
    AC_DEFINE(HAVE_X11R6, 1,
	      [Define to 1 if you have the X11R6 or newer version of Xlib.])
    AC_DEFINE(HAVE_X_I18N, 1, [Define if you have usable i18n support.])
    ## inoue@ainet.or.jp says Solaris has a bug related to X11R6-style
    ## XIM support.
    case "$opsys" in
      sol2-*) : ;;
      *) AC_DEFINE(HAVE_X11R6_XIM, 1,
           [Define if you have usable X11R6-style XIM support.])
         ;;
    esac
  else
    AC_MSG_RESULT(before 6)
  fi
fi


### Use -lrsvg-2 if available, unless `--with-rsvg=no' is specified.
HAVE_RSVG=no
if test "${HAVE_X11}" = "yes" || test "${HAVE_NS}" = "yes"; then
  if test "${with_rsvg}" != "no"; then
    RSVG_REQUIRED=2.11.0
    RSVG_MODULE="librsvg-2.0 >= $RSVG_REQUIRED"

    PKG_CHECK_MODULES(RSVG, $RSVG_MODULE, HAVE_RSVG=yes, :)
    AC_SUBST(RSVG_CFLAGS)
    AC_SUBST(RSVG_LIBS)

    if test $HAVE_RSVG = yes; then
      AC_DEFINE(HAVE_RSVG, 1, [Define to 1 if using librsvg.])
      CFLAGS="$CFLAGS $RSVG_CFLAGS"
      LIBS="$RSVG_LIBS $LIBS"
    fi
  fi
fi

HAVE_IMAGEMAGICK=no
if test "${HAVE_X11}" = "yes"; then
  if test "${with_imagemagick}" != "no"; then
    ## 6.2.8 is the earliest version known to work, but earlier versions
    ## might work - let us know if you find one.
    ## 6.0.7 does not work.  See bug#7955.
    IMAGEMAGICK_MODULE="Wand >= 6.2.8"
    PKG_CHECK_MODULES(IMAGEMAGICK, $IMAGEMAGICK_MODULE, HAVE_IMAGEMAGICK=yes, :)
    AC_SUBST(IMAGEMAGICK_CFLAGS)
    AC_SUBST(IMAGEMAGICK_LIBS)

    if test $HAVE_IMAGEMAGICK = yes; then
      AC_DEFINE(HAVE_IMAGEMAGICK, 1, [Define to 1 if using imagemagick.])
      CFLAGS="$CFLAGS $IMAGEMAGICK_CFLAGS"
      LIBS="$IMAGEMAGICK_LIBS $LIBS"
      AC_CHECK_FUNCS(MagickExportImagePixels MagickMergeImageLayers)
    fi
  fi
fi


HAVE_GTK=no
GTK_OBJ=
check_gtk2=no
gtk3_pkg_errors=
if test "${with_gtk3}" = "yes" || test "${with_gtk}" = "yes" || test "$USE_X_TOOLKIT" = "maybe"; then
  GLIB_REQUIRED=2.28
  GTK_REQUIRED=3.0
  GTK_MODULES="gtk+-3.0 >= $GTK_REQUIRED glib-2.0 >= $GLIB_REQUIRED"

  dnl Checks for libraries.
  PKG_CHECK_MODULES(GTK, $GTK_MODULES, pkg_check_gtk=yes, pkg_check_gtk=no)
  if test "$pkg_check_gtk" = "no" && test "$with_gtk3" = "yes"; then
     AC_MSG_ERROR($GTK_PKG_ERRORS)
  fi
  if test "$pkg_check_gtk" = "yes"; then
     AC_DEFINE(HAVE_GTK3, 1, [Define to 1 if using GTK 3 or later.])
     GTK_OBJ=emacsgtkfixed.o
     term_header=gtkutil.h
     USE_GTK_TOOLKIT="GTK3"
  else
     check_gtk2=yes
     gtk3_pkg_errors="$GTK_PKG_ERRORS "
  fi
fi

if test "${with_gtk2}" = "yes" || test "$check_gtk2" = "yes"; then
  GLIB_REQUIRED=2.10
  GTK_REQUIRED=2.10
  GTK_MODULES="gtk+-2.0 >= $GTK_REQUIRED glib-2.0 >= $GLIB_REQUIRED"

  dnl Checks for libraries.
  PKG_CHECK_MODULES(GTK, $GTK_MODULES, pkg_check_gtk=yes, pkg_check_gtk=no)
  if test "$pkg_check_gtk" = "no" &&
     { test "$with_gtk" = yes || test "$with_gtk2" = "yes"; }
  then
    AC_MSG_ERROR($gtk3_pkg_errors$GTK_PKG_ERRORS)
  fi
  test "$pkg_check_gtk" = "yes" && USE_GTK_TOOLKIT="GTK2"
fi

if test x"$pkg_check_gtk" = xyes; then

  AC_SUBST(GTK_CFLAGS)
  AC_SUBST(GTK_LIBS)
  C_SWITCH_X_SITE="$C_SWITCH_X_SITE $GTK_CFLAGS"
  CFLAGS="$CFLAGS $GTK_CFLAGS"
  LIBS="$GTK_LIBS $LIBS"
  dnl Try to compile a simple GTK program.
  GTK_COMPILES=no
  AC_CHECK_FUNCS(gtk_main, GTK_COMPILES=yes)
  if test "${GTK_COMPILES}" != "yes"; then
    if test "$USE_X_TOOLKIT" != "maybe"; then
      AC_MSG_ERROR([Gtk+ wanted, but it does not compile, see config.log.  Maybe some x11-devel files missing?]);
    fi
  else
    HAVE_GTK=yes
    AC_DEFINE(USE_GTK, 1, [Define to 1 if using GTK.])
    GTK_OBJ="gtkutil.o $GTK_OBJ"
    USE_X_TOOLKIT=none
    if $PKG_CONFIG --atleast-version=2.10 gtk+-2.0; then
      :
    else
      AC_MSG_WARN([[Your version of Gtk+ will have problems with
       closing open displays.  This is no problem if you just use
       one display, but if you use more than one and close one of them
       Emacs may crash.]])
      sleep 3
    fi
  fi

fi
AC_SUBST(GTK_OBJ)


if test "${HAVE_GTK}" = "yes"; then

  dnl  GTK scrollbars resemble toolkit scrollbars a lot, so to avoid
  dnl  a lot if #ifdef:s, say we have toolkit scrollbars.
  if test "$with_toolkit_scroll_bars" != no; then
    with_toolkit_scroll_bars=yes
  fi

  dnl  Check if we have the old file selection dialog declared and
  dnl  in the link library.  In 2.x it may be in the library,
  dnl  but not declared if deprecated featured has been selected out.
  dnl  AC_CHECK_DECL checks for a macro, so check for GTK_TYPE_FILE_SELECTION.
  HAVE_GTK_FILE_SELECTION=no
  AC_CHECK_DECL(GTK_TYPE_FILE_SELECTION, HAVE_GTK_FILE_SELECTION=yes,
                   HAVE_GTK_FILE_SELECTION=no, [AC_INCLUDES_DEFAULT
#include <gtk/gtk.h>])
  if test "$HAVE_GTK_FILE_SELECTION" = yes; then
    AC_CHECK_FUNCS(gtk_file_selection_new)
  fi


  dnl Check for functions introduced in 2.14 and later.
  AC_CHECK_FUNCS(gtk_widget_get_window gtk_widget_set_has_window \
                 gtk_dialog_get_action_area gtk_widget_get_sensitive \
                 gtk_widget_get_mapped gtk_adjustment_get_page_size \
                 gtk_orientable_set_orientation \
		 gtk_window_set_has_resize_grip)

 term_header=gtkutil.h
fi

dnl D-Bus has been tested under GNU/Linux only.  Must be adapted for
dnl other platforms.
HAVE_DBUS=no
DBUS_OBJ=
if test "${with_dbus}" = "yes"; then
   PKG_CHECK_MODULES(DBUS, dbus-1 >= 1.0, HAVE_DBUS=yes, HAVE_DBUS=no)
   if test "$HAVE_DBUS" = yes; then
     LIBS="$LIBS $DBUS_LIBS"
     AC_DEFINE(HAVE_DBUS, 1, [Define to 1 if using D-Bus.])
     dnl dbus_watch_get_unix_fd has been introduced in D-Bus 1.1.1.
     dnl dbus_type_is_valid and dbus_validate_* have been introduced in
     dnl D-Bus 1.5.12.
     AC_CHECK_FUNCS(dbus_watch_get_unix_fd \
		    dbus_type_is_valid \
		    dbus_validate_bus_name \
                    dbus_validate_path \
		    dbus_validate_interface \
		    dbus_validate_member)
     DBUS_OBJ=dbusbind.o
   fi
fi
AC_SUBST(DBUS_OBJ)

dnl GSettings has been tested under GNU/Linux only.
HAVE_GSETTINGS=no
if test "${HAVE_X11}" = "yes" && test "${with_gsettings}" = "yes"; then
   PKG_CHECK_MODULES(GSETTINGS, gio-2.0 >= 2.26, HAVE_GSETTINGS=yes, HAVE_GSETTINGS=no)
   if test "$HAVE_GSETTINGS" = "yes"; then
      AC_DEFINE(HAVE_GSETTINGS, 1, [Define to 1 if using GSettings.])
      SETTINGS_CFLAGS="$GSETTINGS_CFLAGS"
      SETTINGS_LIBS="$GSETTINGS_LIBS"
   fi
fi

dnl GConf has been tested under GNU/Linux only.
dnl The version is really arbitrary, it is about the same age as Gtk+ 2.6.
HAVE_GCONF=no
if test "${HAVE_X11}" = "yes" && test "${with_gconf}" = "yes"; then
   PKG_CHECK_MODULES(GCONF, gconf-2.0 >= 2.13, HAVE_GCONF=yes, HAVE_GCONF=no)
   if test "$HAVE_GCONF" = yes; then
      AC_DEFINE(HAVE_GCONF, 1, [Define to 1 if using GConf.])
      dnl Newer GConf doesn't link with g_objects, so this is not defined.
      SETTINGS_CFLAGS="$SETTINGS_CFLAGS $GCONF_CFLAGS"
      SETTINGS_LIBS="$SETTINGS_LIBS $GCONF_LIBS"
   fi
fi

if test "$HAVE_GSETTINGS" = "yes" || test "$HAVE_GCONF" = "yes"; then
    PKG_CHECK_MODULES(GOBJECT, gobject-2.0 >= 2.0, HAVE_GOBJECT=yes, HAVE_GOBJECT=no)
    if test "$HAVE_GOBJECT" = "yes"; then
       SETTINGS_CFLAGS="$SETTINGS_CFLAGS $GOBJECT_CFLAGS"
       SETTINGS_LIBS="$SETTINGS_LIBS $GOBJECT_LIBS"
    fi
    SAVE_CFLAGS="$CFLAGS"
    SAVE_LIBS="$LIBS"
    CFLAGS="$SETTINGS_CFLAGS $CFLAGS"
    LIBS="$SETTINGS_LIBS $LIBS"
    AC_CHECK_FUNCS([g_type_init])
    CFLAGS="$SAVE_CFLAGS"
    LIBS="$SAVE_LIBS"
fi
AC_SUBST(SETTINGS_CFLAGS)
AC_SUBST(SETTINGS_LIBS)


dnl SELinux is available for GNU/Linux only.
HAVE_LIBSELINUX=no
LIBSELINUX_LIBS=
if test "${with_selinux}" = "yes"; then
   AC_CHECK_LIB([selinux], [lgetfilecon], HAVE_LIBSELINUX=yes, HAVE_LIBSELINUX=no)
   if test "$HAVE_LIBSELINUX" = yes; then
      AC_DEFINE(HAVE_LIBSELINUX, 1, [Define to 1 if using SELinux.])
      LIBSELINUX_LIBS=-lselinux
   fi
fi
AC_SUBST(LIBSELINUX_LIBS)

HAVE_GNUTLS=no
HAVE_GNUTLS_CALLBACK_CERTIFICATE_VERIFY=no
if test "${with_gnutls}" = "yes" ; then
  PKG_CHECK_MODULES([LIBGNUTLS], [gnutls >= 2.6.6], HAVE_GNUTLS=yes, HAVE_GNUTLS=no)
  if test "${HAVE_GNUTLS}" = "yes"; then
    AC_DEFINE(HAVE_GNUTLS, 1, [Define if using GnuTLS.])
  fi

  CFLAGS="$CFLAGS $LIBGNUTLS_CFLAGS"
  LIBS="$LIBGNUTLS_LIBS $LIBS"
  AC_CHECK_FUNCS(gnutls_certificate_set_verify_function, HAVE_GNUTLS_CALLBACK_CERTIFICATE_VERIFY=yes)

  if test "${HAVE_GNUTLS_CALLBACK_CERTIFICATE_VERIFY}" = "yes"; then
    AC_DEFINE(HAVE_GNUTLS_CALLBACK_CERTIFICATE_VERIFY, 1, [Define if using GnuTLS certificate verification callbacks.])
  fi
fi

AC_SUBST(LIBGNUTLS_LIBS)
AC_SUBST(LIBGNUTLS_CFLAGS)

dnl Do not put whitespace before the #include statements below.
dnl Older compilers (eg sunos4 cc) choke on it.
HAVE_XAW3D=no
LUCID_LIBW=
if test x"${USE_X_TOOLKIT}" = xmaybe || test x"${USE_X_TOOLKIT}" = xLUCID; then
  if test "$with_xaw3d" != no; then
    AC_CACHE_VAL(emacs_cv_xaw3d,
    [AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <X11/Intrinsic.h>
#include <X11/Xaw3d/Simple.h>]],
      [[]])],
      [AC_CHECK_LIB(Xaw3d, XawScrollbarSetThumb,
                    emacs_cv_xaw3d=yes, emacs_cv_xaw3d=no)],
      emacs_cv_xaw3d=no)])
  else
    emacs_cv_xaw3d=no
  fi
  if test $emacs_cv_xaw3d = yes; then
    AC_MSG_CHECKING(for xaw3d)
    AC_MSG_RESULT([yes; using Lucid toolkit])
    USE_X_TOOLKIT=LUCID
    HAVE_XAW3D=yes
    LUCID_LIBW=-lXaw3d
    AC_DEFINE(HAVE_XAW3D, 1,
              [Define to 1 if you have the Xaw3d library (-lXaw3d).])
  else
    AC_MSG_CHECKING(for xaw3d)
    AC_MSG_RESULT(no)
    AC_MSG_CHECKING(for libXaw)
    AC_CACHE_VAL(emacs_cv_xaw,
    [AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <X11/Intrinsic.h>
#include <X11/Xaw/Simple.h>]],
      [[]])],
      emacs_cv_xaw=yes,
      emacs_cv_xaw=no)])
    if test $emacs_cv_xaw = yes; then
      AC_MSG_RESULT([yes; using Lucid toolkit])
      USE_X_TOOLKIT=LUCID
      LUCID_LIBW=-lXaw
    elif test x"${USE_X_TOOLKIT}" = xLUCID; then
      AC_MSG_ERROR([Lucid toolkit requires X11/Xaw include files])
    else
      AC_MSG_ERROR([No X toolkit could be found.
If you are sure you want Emacs compiled without an X toolkit, pass
  --with-x-toolkit=no
to configure.  Otherwise, install the development libraries for the toolkit
that you want to use (e.g. Gtk+) and re-run configure.])
    fi
  fi
fi

X_TOOLKIT_TYPE=$USE_X_TOOLKIT

LIBXTR6=
if test "${USE_X_TOOLKIT}" != "none"; then
  AC_MSG_CHECKING(X11 toolkit version)
  AC_CACHE_VAL(emacs_cv_x11_toolkit_version_6,
  [AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <X11/Intrinsic.h>]],
[[#if XtSpecificationRelease < 6
fail;
#endif
]])], emacs_cv_x11_toolkit_version_6=yes, emacs_cv_x11_toolkit_version_6=no)])
  HAVE_X11XTR6=$emacs_cv_x11_toolkit_version_6
  if test $emacs_cv_x11_toolkit_version_6 = yes; then
    AC_MSG_RESULT(6 or newer)
    AC_DEFINE(HAVE_X11XTR6, 1,
	      [Define to 1 if you have the X11R6 or newer version of Xt.])
    LIBXTR6="-lSM -lICE"
    case "$opsys" in
      ## Use libw.a along with X11R6 Xt.
      unixware) LIBXTR6="$LIBXTR6 -lw" ;;
    esac
  else
    AC_MSG_RESULT(before 6)
  fi

dnl If using toolkit, check whether libXmu.a exists.
dnl tranle@intellicorp.com says libXmu.a can need XtMalloc in libXt.a to link.
  OLDLIBS="$LIBS"
  if test x$HAVE_X11XTR6 = xyes; then
    LIBS="-lXt -lSM -lICE $LIBS"
  else
    LIBS="-lXt $LIBS"
  fi
  AC_CHECK_LIB(Xmu, XmuConvertStandardSelection)
  test $ac_cv_lib_Xmu_XmuConvertStandardSelection = no && LIBS="$OLDLIBS"
  dnl ac_cv_lib_Xmu_XmuConvertStandardSelection is also referenced below.
fi
AC_SUBST(LIBXTR6)

dnl FIXME the logic here seems weird, but this is what cpp was doing.
dnl Why not just test for libxmu in the normal way?
LIBXMU=-lXmu
case $opsys in
  ## These systems don't supply Xmu.
  hpux* | aix4-2 )
    test "X$ac_cv_lib_Xmu_XmuConvertStandardSelection" != "Xyes" && LIBXMU=
    ;;
esac
AC_SUBST(LIBXMU)

# On Irix 6.5, at least, we need XShapeQueryExtension from -lXext for Xaw3D.
if test "${HAVE_X11}" = "yes"; then
  if test "${USE_X_TOOLKIT}" != "none"; then
    AC_CHECK_LIB(Xext, XShapeQueryExtension)
  fi
fi

LIBXP=
if test "${USE_X_TOOLKIT}" = "MOTIF"; then
  # OpenMotif may be installed in such a way on some GNU/Linux systems.
  if test -d /usr/include/openmotif; then
    CPPFLAGS="-I/usr/include/openmotif $CPPFLAGS"
    emacs_cv_openmotif=yes
    case "$canonical" in
      x86_64-*-linux-gnu* | powerpc64-*-linux-gnu* | sparc64-*-linux-gnu*)
      test -d /usr/lib64/openmotif && LDFLAGS="-L/usr/lib64/openmotif $LDFLAGS"
      ;;
      *)
      test -d /usr/lib/openmotif && LDFLAGS="-L/usr/lib/openmotif $LDFLAGS"
    esac
  else
    emacs_cv_openmotif=no
  fi
  AC_CACHE_CHECK(for (Open)Motif version 2.1, emacs_cv_motif_version_2_1,
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <Xm/Xm.h>]],
    [[#if XmVERSION > 2 || (XmVERSION == 2 && XmREVISION >= 1)
int x = 5;
#else
Motif version prior to 2.1.
#endif]])],
    emacs_cv_motif_version_2_1=yes, emacs_cv_motif_version_2_1=no)])
  if test $emacs_cv_motif_version_2_1 = yes; then
    AC_CHECK_LIB(Xp, XpCreateContext, LIBXP=-lXp)
    if test x$emacs_cv_openmotif = xyes; then
      REAL_CPPFLAGS="-I/usr/include/openmotif $REAL_CPPFLAGS"
    fi
  else
    AC_CACHE_CHECK(for LessTif where some systems put it, emacs_cv_lesstif,
    # We put this in CFLAGS temporarily to precede other -I options
    # that might be in CFLAGS temporarily.
    # We put this in CPPFLAGS where it precedes the other -I options.
    OLD_CPPFLAGS=$CPPFLAGS
    OLD_CFLAGS=$CFLAGS
    CPPFLAGS="-I/usr/X11R6/LessTif/Motif1.2/include $CPPFLAGS"
    CFLAGS="-I/usr/X11R6/LessTif/Motif1.2/include $CFLAGS"
    [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include </usr/X11R6/LessTif/Motif1.2/include/Xm/Xm.h>]],
      [[int x = 5;]])],
      emacs_cv_lesstif=yes, emacs_cv_lesstif=no)])
    if test $emacs_cv_lesstif = yes; then
      # Make sure this -I option remains in CPPFLAGS after it is set
      # back to REAL_CPPFLAGS.
      # There is no need to change REAL_CFLAGS, because REAL_CFLAGS does not
      # have those other -I options anyway.  Ultimately, having this
      # directory ultimately in CPPFLAGS will be enough.
      REAL_CPPFLAGS="-I/usr/X11R6/LessTif/Motif1.2/include $REAL_CPPFLAGS"
      LDFLAGS="-L/usr/X11R6/LessTif/Motif1.2/lib $LDFLAGS"
    else
      CFLAGS=$OLD_CFLAGS
      CPPFLAGS=$OLD_CPPFLAGS
    fi
  fi
  AC_CHECK_HEADER([Xm/BulletinB.h], [],
    [AC_MSG_ERROR([Motif toolkit requested but requirements not found.])])
fi

dnl Use toolkit scroll bars if configured for GTK or X toolkit and either
dnl using Motif or Xaw3d is available, and unless
dnl --with-toolkit-scroll-bars=no was specified.

AH_TEMPLATE(USE_TOOLKIT_SCROLL_BARS,
	    [Define to 1 if we should use toolkit scroll bars.])dnl
USE_TOOLKIT_SCROLL_BARS=no
if test "${with_toolkit_scroll_bars}" != "no"; then
  if test "${USE_X_TOOLKIT}" != "none"; then
    if test "${USE_X_TOOLKIT}" = "MOTIF"; then
      AC_DEFINE(USE_TOOLKIT_SCROLL_BARS)
      HAVE_XAW3D=no
      USE_TOOLKIT_SCROLL_BARS=yes
    elif test "${HAVE_XAW3D}" = "yes" || test "${USE_X_TOOLKIT}" = "LUCID"; then
      AC_DEFINE(USE_TOOLKIT_SCROLL_BARS)
      USE_TOOLKIT_SCROLL_BARS=yes
    fi
  elif test "${HAVE_GTK}" = "yes"; then
    AC_DEFINE(USE_TOOLKIT_SCROLL_BARS)
    USE_TOOLKIT_SCROLL_BARS=yes
  elif test "${HAVE_NS}" = "yes"; then
    AC_DEFINE(USE_TOOLKIT_SCROLL_BARS)
    USE_TOOLKIT_SCROLL_BARS=yes
  elif test "${HAVE_W32}" = "yes"; then
    AC_DEFINE(USE_TOOLKIT_SCROLL_BARS)
    USE_TOOLKIT_SCROLL_BARS=yes
  fi
fi

dnl See if XIM is available.
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
	  #include <X11/Xlib.h>
	  #include <X11/Xresource.h>]],
	 [[XIMProc  callback;]])],
	 [HAVE_XIM=yes
	 AC_DEFINE(HAVE_XIM, 1, [Define to 1 if XIM is available])],
	 HAVE_XIM=no)

dnl `--with-xim' now controls only the initial value of use_xim at run time.

if test "${with_xim}" != "no"; then
  AC_DEFINE(USE_XIM, 1,
	    [Define to 1 if we should use XIM, if it is available.])
fi


if test "${HAVE_XIM}" != "no"; then
  late_CFLAGS=$CFLAGS
  if test "$GCC" = yes; then
    CFLAGS="$CFLAGS --pedantic-errors"
  fi
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <X11/Xlib.h>
#include <X11/Xresource.h>]],
[[Display *display;
XrmDatabase db;
char *res_name;
char *res_class;
XIMProc  callback;
XPointer *client_data;
#ifndef __GNUC__
/* If we're not using GCC, it's probably not XFree86, and this is
   probably right, but we can't use something like --pedantic-errors.  */
extern Bool XRegisterIMInstantiateCallback(Display*, XrmDatabase, char*,
                                           char*, XIMProc, XPointer*);
#endif
(void)XRegisterIMInstantiateCallback(display, db, res_name, res_class, callback,
   client_data);]])],
    [emacs_cv_arg6_star=yes])
  AH_TEMPLATE(XRegisterIMInstantiateCallback_arg6,
         [Define to the type of the 6th arg of XRegisterIMInstantiateCallback,
either XPointer or XPointer*.])dnl
  if test "$emacs_cv_arg6_star" = yes; then
    AC_DEFINE(XRegisterIMInstantiateCallback_arg6, [XPointer*])
  else
    AC_DEFINE(XRegisterIMInstantiateCallback_arg6, [XPointer])
  fi
  CFLAGS=$late_CFLAGS
fi

### Start of font-backend (under any platform) section.
# (nothing here yet -- this is a placeholder)
### End of font-backend (under any platform) section.

### Start of font-backend (under X11) section.
if test "${HAVE_X11}" = "yes"; then
   PKG_CHECK_MODULES(FONTCONFIG, fontconfig >= 2.2.0, HAVE_FC=yes, HAVE_FC=no)

   ## Use -lXft if available, unless `--with-xft=no'.
   HAVE_XFT=maybe
    if test "${HAVE_FC}" = "no" || test "x${with_x}" = "xno"; then
      with_xft="no";
    fi
    if test "x${with_xft}" != "xno"; then

      PKG_CHECK_MODULES(XFT, xft >= 0.13.0, , HAVE_XFT=no)
      ## Because xftfont.c uses XRenderQueryExtension, we also
      ## need to link to -lXrender.
      HAVE_XRENDER=no
      AC_CHECK_LIB(Xrender, XRenderQueryExtension, HAVE_XRENDER=yes)
      if test "$HAVE_XFT" != no && test "$HAVE_XRENDER" != no; then
	OLD_CPPFLAGS="$CPPFLAGS"
	OLD_CFLAGS="$CFLAGS"
	OLD_LIBS="$LIBS"
	CPPFLAGS="$CPPFLAGS $XFT_CFLAGS"
	CFLAGS="$CFLAGS $XFT_CFLAGS"
	XFT_LIBS="-lXrender $XFT_LIBS"
	LIBS="$XFT_LIBS $LIBS"
	AC_CHECK_HEADER(X11/Xft/Xft.h,
	  AC_CHECK_LIB(Xft, XftFontOpen, HAVE_XFT=yes, , $XFT_LIBS))

	if test "${HAVE_XFT}" = "yes"; then
	  AC_DEFINE(HAVE_XFT, 1, [Define to 1 if you have the Xft library.])
	    AC_SUBST(XFT_LIBS)
	  C_SWITCH_X_SITE="$C_SWITCH_X_SITE $XFT_CFLAGS"
	else
	  CPPFLAGS="$OLD_CPPFLAGS"
	  CFLAGS="$OLD_CFLAGS"
	  LIBS="$OLD_LIBS"
	fi                        # "${HAVE_XFT}" = "yes"
      fi                          # "$HAVE_XFT" != no
    fi                            # "x${with_xft}" != "xno"

    ## We used to allow building with FreeType and without Xft.
    ## However, the ftx font backend driver is not in good shape.
    if test "$HAVE_XFT" != "yes"; then
       dnl For the "Does Emacs use" message at the end.
       HAVE_XFT=no
       HAVE_FREETYPE=no
    else
       dnl Strict linkers fail with
       dnl ftfont.o: undefined reference to symbol 'FT_New_Face'
       dnl if -lfreetype is not specified.
       dnl The following is needed to set FREETYPE_LIBS.
       PKG_CHECK_MODULES(FREETYPE, freetype2, HAVE_FREETYPE=yes,
			 HAVE_FREETYPE=no)

       test "$HAVE_FREETYPE" = "no" && AC_MSG_ERROR(libxft requires libfreetype)
    fi

    HAVE_LIBOTF=no
    if test "${HAVE_FREETYPE}" = "yes"; then
      AC_DEFINE(HAVE_FREETYPE, 1,
		[Define to 1 if using the freetype and fontconfig libraries.])
      if test "${with_libotf}" != "no"; then
	PKG_CHECK_MODULES(LIBOTF, libotf, HAVE_LIBOTF=yes,
			  HAVE_LIBOTF=no)
	if test "$HAVE_LIBOTF" = "yes"; then
	  AC_DEFINE(HAVE_LIBOTF, 1, [Define to 1 if using libotf.])
	  AC_CHECK_LIB(otf, OTF_get_variation_glyphs,
		       HAVE_OTF_GET_VARIATION_GLYPHS=yes,
		       HAVE_OTF_GET_VARIATION_GLYPHS=no)
	  if test "${HAVE_OTF_GET_VARIATION_GLYPHS}" = "yes"; then
	    AC_DEFINE(HAVE_OTF_GET_VARIATION_GLYPHS, 1,
		      [Define to 1 if libotf has OTF_get_variation_glyphs.])
	  fi
	fi
      fi
    dnl FIXME should there be an error if HAVE_FREETYPE != yes?
    dnl Does the new font backend require it, or can it work without it?
    fi

    HAVE_M17N_FLT=no
    if test "${HAVE_LIBOTF}" = yes; then
      if test "${with_m17n_flt}" != "no"; then
	PKG_CHECK_MODULES(M17N_FLT, m17n-flt, HAVE_M17N_FLT=yes, HAVE_M17N_FLT=no)
	if test "$HAVE_M17N_FLT" = "yes"; then
	  AC_DEFINE(HAVE_M17N_FLT, 1, [Define to 1 if using libm17n-flt.])
	fi
      fi
    fi
else
    HAVE_XFT=no
    HAVE_FREETYPE=no
    HAVE_LIBOTF=no
    HAVE_M17N_FLT=no
fi

### End of font-backend (under X11) section.

AC_SUBST(FREETYPE_CFLAGS)
AC_SUBST(FREETYPE_LIBS)
AC_SUBST(FONTCONFIG_CFLAGS)
AC_SUBST(FONTCONFIG_LIBS)
AC_SUBST(LIBOTF_CFLAGS)
AC_SUBST(LIBOTF_LIBS)
AC_SUBST(M17N_FLT_CFLAGS)
AC_SUBST(M17N_FLT_LIBS)

### Use -lXpm if available, unless `--with-xpm=no'.
HAVE_XPM=no
LIBXPM=

if test "${HAVE_W32}" = "yes"; then
  if test "${with_xpm}" != "no"; then
    SAVE_CPPFLAGS="$CPPFLAGS"
    SAVE_LDFLAGS="$LDFLAGS"
    CPPFLAGS="$CPPFLAGS -I/usr/include/noX"
    LDFLAGS="$LDFLAGS -L/usr/lib/noX"
    AC_CHECK_HEADER(X11/xpm.h,
      [AC_CHECK_LIB(Xpm, XpmReadFileToImage, HAVE_XPM=yes)])
    if test "${HAVE_XPM}" = "yes"; then
      AC_MSG_CHECKING(for XpmReturnAllocPixels preprocessor define)
      AC_EGREP_CPP(no_return_alloc_pixels,
      [#include "X11/xpm.h"
#ifndef XpmReturnAllocPixels
no_return_alloc_pixels
#endif
      ], HAVE_XPM=no, HAVE_XPM=yes)

      if test "${HAVE_XPM}" = "yes"; then
        REAL_CPPFLAGS="$REAL_CPPFLAGS -I/usr/include/noX"
	AC_MSG_RESULT(yes)
      else
	AC_MSG_RESULT(no)
        CPPFLAGS="$SAVE_CPPFLAGS"
        LDFLAGS="$SAVE_LDFLAGS"
      fi
    fi
  fi

  if test "${HAVE_XPM}" = "yes"; then
    AC_DEFINE(HAVE_XPM, 1, [Define to 1 if you have the Xpm library (-lXpm).])
    LIBXPM=-lXpm
  fi
fi

if test "${HAVE_X11}" = "yes"; then
  if test "${with_xpm}" != "no"; then
    AC_CHECK_HEADER(X11/xpm.h,
      [AC_CHECK_LIB(Xpm, XpmReadFileToPixmap, HAVE_XPM=yes, , -lX11)])
    if test "${HAVE_XPM}" = "yes"; then
      AC_MSG_CHECKING(for XpmReturnAllocPixels preprocessor define)
      AC_EGREP_CPP(no_return_alloc_pixels,
      [#include "X11/xpm.h"
#ifndef XpmReturnAllocPixels
no_return_alloc_pixels
#endif
      ], HAVE_XPM=no, HAVE_XPM=yes)

      if test "${HAVE_XPM}" = "yes"; then
	AC_MSG_RESULT(yes)
      else
	AC_MSG_RESULT(no)
      fi
    fi
  fi

  if test "${HAVE_XPM}" = "yes"; then
    AC_DEFINE(HAVE_XPM, 1, [Define to 1 if you have the Xpm library (-lXpm).])
    LIBXPM=-lXpm
  fi
fi

AC_SUBST(LIBXPM)

### Use -ljpeg if available, unless `--with-jpeg=no'.
HAVE_JPEG=no
LIBJPEG=
if test "${HAVE_X11}" = "yes" || test "${HAVE_W32}" = "yes"; then
  if test "${with_jpeg}" != "no"; then
    dnl Checking for jpeglib.h can lose because of a redefinition of
    dnl  HAVE_STDLIB_H.
    AC_CHECK_HEADER(jerror.h,
      [AC_CHECK_LIB(jpeg, jpeg_destroy_compress, HAVE_JPEG=yes)])
  fi

  AH_TEMPLATE(HAVE_JPEG, [Define to 1 if you have the jpeg library (-ljpeg).])dnl
  if test "${HAVE_JPEG}" = "yes"; then
    AC_DEFINE(HAVE_JPEG)
    AC_EGREP_CPP([version= *(6[2-9]|[7-9][0-9])],
        [#include <jpeglib.h>
	 version=JPEG_LIB_VERSION
],
        [AC_DEFINE(HAVE_JPEG)],
        [AC_MSG_WARN([libjpeg found, but not version 6b or later])
        HAVE_JPEG=no])
  fi
  if test "${HAVE_JPEG}" = "yes"; then
    LIBJPEG=-ljpeg
  fi
fi
AC_SUBST(LIBJPEG)

### Use -lpng if available, unless `--with-png=no'.
HAVE_PNG=no
LIBPNG=
if test "${HAVE_X11}" = "yes" || test "${HAVE_W32}" = "yes"; then
  if test "${with_png}" != "no"; then
    # Debian unstable as of July 2003 has multiple libpngs, and puts png.h
    # in /usr/include/libpng.
    AC_CHECK_HEADERS(png.h libpng/png.h, break)
    if test "$ac_cv_header_png_h" = yes || test "$ac_cv_header_libpng_png_h" = yes ; then
      AC_CHECK_LIB(png, png_get_channels, HAVE_PNG=yes, , -lz -lm)
    fi
  fi

  if test "${HAVE_PNG}" = "yes"; then
    AC_DEFINE(HAVE_PNG, 1, [Define to 1 if you have the png library (-lpng).])
    LIBPNG="-lpng -lz -lm"

    AC_CHECK_DECL(png_longjmp,
      [],
      [AC_DEFINE(PNG_DEPSTRUCT, [],
	 [Define to empty to suppress deprecation warnings when building
	  with --enable-gcc-warnings and with libpng versions before 1.5,
	  which lack png_longjmp.])],
      [[#ifdef HAVE_LIBPNG_PNG_H
	# include <libpng/png.h>
	#else
	# include <png.h>
	#endif
      ]])
  fi
fi
AC_SUBST(LIBPNG)

### Use -ltiff if available, unless `--with-tiff=no'.
HAVE_TIFF=no
LIBTIFF=
if test "${HAVE_X11}" = "yes" || test "${HAVE_W32}" = "yes"; then
  if test "${with_tiff}" != "no"; then
    AC_CHECK_HEADER(tiffio.h,
      [tifflibs="-lz -lm"
      # At least one tiff package requires the jpeg library.
      if test "${HAVE_JPEG}" = yes; then tifflibs="-ljpeg $tifflibs"; fi
      AC_CHECK_LIB(tiff, TIFFGetVersion, HAVE_TIFF=yes, , $tifflibs)])
  fi

  if test "${HAVE_TIFF}" = "yes"; then
    AC_DEFINE(HAVE_TIFF, 1, [Define to 1 if you have the tiff library (-ltiff).])
    dnl FIXME -lz -lm, as per libpng?
    LIBTIFF=-ltiff
  fi
fi
AC_SUBST(LIBTIFF)

### Use -lgif or -lungif if available, unless `--with-gif=no'.
HAVE_GIF=no
LIBGIF=
if test "${HAVE_X11}" = "yes" && test "${with_gif}" != "no" \
        || test "${HAVE_W32}" = "yes"; then
  AC_CHECK_HEADER(gif_lib.h,
# EGifPutExtensionLast only exists from version libungif-4.1.0b1.
# Earlier versions can crash Emacs.
    [AC_CHECK_LIB(gif, EGifPutExtensionLast, HAVE_GIF=yes, HAVE_GIF=maybe)])

  if test "$HAVE_GIF" = yes; then
    LIBGIF=-lgif
  elif test "$HAVE_GIF" = maybe; then
# If gif_lib.h but no libgif, try libungif.
    AC_CHECK_LIB(ungif, EGifPutExtensionLast, HAVE_GIF=yes, HAVE_GIF=no)
    test "$HAVE_GIF" = yes && LIBGIF=-lungif
  fi

  if test "${HAVE_GIF}" = "yes"; then
    AC_DEFINE(HAVE_GIF, 1, [Define to 1 if you have a gif (or ungif) library.])
  fi
fi
AC_SUBST(LIBGIF)

dnl Check for required libraries.
if test "${HAVE_X11}" = "yes"; then
  MISSING=""
  WITH_NO=""
  test "${with_xpm}" != "no" && test "${HAVE_XPM}" != "yes" &&
    MISSING="libXpm" && WITH_NO="--with-xpm=no"
  test "${with_jpeg}" != "no" && test "${HAVE_JPEG}" != "yes" &&
    MISSING="$MISSING libjpeg" && WITH_NO="$WITH_NO --with-jpeg=no"
  test "${with_png}" != "no" && test "${HAVE_PNG}" != "yes" &&
    MISSING="$MISSING libpng" && WITH_NO="$WITH_NO --with-png=no"
  test "${with_gif}" != "no" && test "${HAVE_GIF}" != "yes" &&
    MISSING="$MISSING libgif/libungif" && WITH_NO="$WITH_NO --with-gif=no"
  test "${with_tiff}" != "no" && test "${HAVE_TIFF}" != "yes" &&
    MISSING="$MISSING libtiff" && WITH_NO="$WITH_NO --with-tiff=no"

  if test "X${MISSING}" != X; then
    AC_MSG_ERROR([The following required libraries were not found:
    $MISSING
Maybe some development libraries/packages are missing?
If you don't want to link with them give
    $WITH_NO
as options to configure])
  fi
fi

### Use -lgpm if available, unless `--with-gpm=no'.
HAVE_GPM=no
LIBGPM=
if test "${with_gpm}" != "no"; then
  AC_CHECK_HEADER(gpm.h,
    [AC_CHECK_LIB(gpm, Gpm_Open, HAVE_GPM=yes)])

  if test "${HAVE_GPM}" = "yes"; then
    AC_DEFINE(HAVE_GPM, 1, [Define to 1 if you have the gpm library (-lgpm).])
    LIBGPM=-lgpm
  fi
fi
AC_SUBST(LIBGPM)

dnl Check for malloc/malloc.h on darwin
AC_CHECK_HEADERS_ONCE(malloc/malloc.h)

GNUSTEP_CFLAGS=
### Use NeXTstep API to implement GUI.
if test "${HAVE_NS}" = "yes"; then
  AC_DEFINE(HAVE_NS, 1, [Define to 1 if you are using the NeXTstep API, either GNUstep or Cocoa on Mac OS X.])
  if test "${NS_IMPL_COCOA}" = "yes"; then
    AC_DEFINE(NS_IMPL_COCOA, 1, [Define to 1 if you are using NS windowing under MacOS X.])
  fi
  if test "${NS_IMPL_GNUSTEP}" = "yes"; then
    AC_DEFINE(NS_IMPL_GNUSTEP, 1, [Define to 1 if you are using NS windowing under GNUstep.])
    # See also .m.o rule in Makefile.in */
    # FIXME: are all these flags really needed?  Document here why.  */
    GNUSTEP_CFLAGS="-D_REENTRANT -fPIC -fno-strict-aliasing -I${GNUSTEP_SYSTEM_HEADERS} ${GNUSTEP_LOCAL_HEADERS}"
    ## Extra CFLAGS applied to src/*.m files.
    GNU_OBJC_CFLAGS="$GNU_OBJC_CFLAGS -fgnu-runtime -Wno-import -fconstant-string-class=NSConstantString -DGNUSTEP_BASE_LIBRARY=1 -DGNU_GUI_LIBRARY=1 -DGNU_RUNTIME=1 -DGSWARN -DGSDIAGNOSE"
  fi
  # We also have mouse menus.
  HAVE_MENUS=yes
  OTHER_FILES=ns-app
fi

if test "${HAVE_W32}" = "yes"; then
  HAVE_MENUS=yes
fi

### Use session management (-lSM -lICE) if available
HAVE_X_SM=no
LIBXSM=
if test "${HAVE_X11}" = "yes"; then
  AC_CHECK_HEADER(X11/SM/SMlib.h,
    [AC_CHECK_LIB(SM, SmcOpenConnection, HAVE_X_SM=yes, , -lICE)])

  if test "${HAVE_X_SM}" = "yes"; then
    AC_DEFINE(HAVE_X_SM, 1, [Define to 1 if you have the SM library (-lSM).])
    LIBXSM="-lSM -lICE"
    case "$LIBS" in
      *-lSM*) ;;
      *)      LIBS="$LIBXSM $LIBS" ;;
    esac
  fi
fi
AC_SUBST(LIBXSM)

### Use libxml (-lxml2) if available
HAVE_LIBXML2=no
if test "${with_xml2}" != "no"; then
  ### I'm not sure what the version number should be, so I just guessed.
  PKG_CHECK_MODULES(LIBXML2, libxml-2.0 > 2.6.17, HAVE_LIBXML2=yes, HAVE_LIBXML2=no)
  if test "${HAVE_LIBXML2}" = "yes"; then
    LIBS="$LIBXML2_LIBS $LIBS"
    AC_CHECK_LIB(xml2, htmlReadMemory, HAVE_LIBXML2=yes, HAVE_LIBXML2=no)
    if test "${HAVE_LIBXML2}" = "yes"; then
      AC_DEFINE(HAVE_LIBXML2, 1, [Define to 1 if you have the libxml library (-lxml2).])
    else
      LIBXML2_LIBS=""
      LIBXML2_CFLAGS=""
    fi
  fi
fi
AC_SUBST(LIBXML2_LIBS)
AC_SUBST(LIBXML2_CFLAGS)

# If netdb.h doesn't declare h_errno, we must declare it by hand.
AC_CACHE_CHECK(whether netdb declares h_errno,
	       emacs_cv_netdb_declares_h_errno,
[AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <netdb.h>]],
  [[return h_errno;]])],
  emacs_cv_netdb_declares_h_errno=yes, emacs_cv_netdb_declares_h_errno=no)])
if test $emacs_cv_netdb_declares_h_errno = yes; then
  AC_DEFINE(HAVE_H_ERRNO, 1, [Define to 1 if netdb.h declares h_errno.])
fi

# sqrt and other floating-point functions such as fmod and frexp
# are found in -lm on most systems.
AC_CHECK_LIB(m, sqrt)

# Check for mail-locking functions in a "mail" library.  Probably this should
# have the same check as for liblockfile below.
AC_CHECK_LIB(mail, maillock, have_mail=yes, have_mail=no)
if test $have_mail = yes; then
  LIBS_MAIL=-lmail
  LIBS="$LIBS_MAIL $LIBS"
  AC_DEFINE(HAVE_LIBMAIL, 1, [Define to 1 if you have the `mail' library (-lmail).])
else
  LIBS_MAIL=
fi
dnl Debian, at least:
AC_CHECK_LIB(lockfile, maillock, have_lockfile=yes, have_lockfile=no)
if test $have_lockfile = yes; then
   LIBS_MAIL=-llockfile
   LIBS="$LIBS_MAIL $LIBS"
   AC_DEFINE(HAVE_LIBLOCKFILE, 1, [Define to 1 if you have the `lockfile' library (-llockfile).])
else
# If we have the shared liblockfile, assume we must use it for mail
# locking (e.g. Debian).  If we couldn't link against liblockfile
# (no liblockfile.a installed), ensure that we don't need to.
  dnl This works for files generally, not just executables.
  dnl Should we look elsewhere for it?  Maybe examine /etc/ld.so.conf?
  AC_CHECK_PROG(liblockfile, liblockfile.so, yes, no,
                /usr/lib:/lib:/usr/local/lib:$LD_LIBRARY_PATH)
  if test $ac_cv_prog_liblockfile = yes; then
    AC_MSG_ERROR([Shared liblockfile found but can't link against it.
This probably means that movemail could lose mail.
There may be a `development' package to install containing liblockfile.])
  fi
fi
AC_CHECK_HEADERS_ONCE(maillock.h)
AC_SUBST(LIBS_MAIL)

## Define MAIL_USE_FLOCK (or LOCKF) if the mailer uses flock (or lockf) to
## interlock access to the mail spool.  The alternative is a lock file named
## /usr/spool/mail/$USER.lock.
mail_lock=no
case "$opsys" in
  aix4-2) mail_lock="lockf" ;;

  gnu|freebsd|netbsd|openbsd|darwin|irix6-5) mail_lock="flock" ;;

  ## On GNU/Linux systems, both methods are used by various mail programs.
  ## I assume most people are using newer mailers that have heard of flock.
  ## Change this if you need to.
  ## Debian contains a patch which says: ``On Debian/GNU/Linux systems,
  ## configure gets the right answers, and that means *NOT* using flock.
  ## Using flock is guaranteed to be the wrong thing. See Debian Policy
  ## for details.'' and then uses `#ifdef DEBIAN'.  Unfortunately the
  ## Debian maintainer hasn't provided a clean fix for Emacs.
  ## movemail.c will use `maillock' when MAILDIR, HAVE_LIBMAIL and
  ## HAVE_MAILLOCK_H are defined, so the following appears to be the
  ## correct logic.  -- fx
  ## We must check for HAVE_LIBLOCKFILE too, as movemail does.
  ## liblockfile is a Free Software replacement for libmail, used on
  ## Debian systems and elsewhere. -rfr.
  gnu-*)
    mail_lock="flock"
    if test $have_mail = yes || test $have_lockfile = yes; then
      test $ac_cv_header_maillock_h = yes && mail_lock=no
    fi
    ;;
esac

BLESSMAIL_TARGET=
case "$mail_lock" in
  flock) AC_DEFINE(MAIL_USE_FLOCK, 1, [Define if the mailer uses flock to interlock the mail spool.]) ;;

  lockf) AC_DEFINE(MAIL_USE_LOCKF, 1, [Define if the mailer uses lockf to interlock the mail spool.]) ;;

  *) BLESSMAIL_TARGET="need-blessmail" ;;
esac
AC_SUBST(BLESSMAIL_TARGET)


AC_CHECK_FUNCS(gethostname \
closedir getrusage get_current_dir_name \
lrand48 setsid \
fpathconf select euidaccess getpagesize setlocale \
utimes getrlimit setrlimit setpgid getcwd shutdown getaddrinfo \
__fpending strsignal setitimer \
sendto recvfrom getsockname getpeername getifaddrs freeifaddrs \
gai_strerror mkstemp getline getdelim fsync sync \
difftime posix_memalign \
getpwent endpwent getgrent endgrent \
touchlock \
cfmakeraw cfsetspeed copysign __executable_start)

dnl getwd appears to be buggy on SVR4.2, so we don't use it.
if test $opsys = unixware; then
  dnl In case some other test ends up checking for getwd.
  AC_DEFINE(BROKEN_GETWD, 1, [Define if getwd should not be used.])
else
  AC_CHECK_FUNCS(getwd)
fi

## Eric Backus <ericb@lsid.hp.com> says, HP-UX 9.x on HP 700 machines
## has a broken `rint' in some library versions including math library
## version number A.09.05.
## You can fix the math library by installing patch number PHSS_4630.
## But we can fix it more reliably for Emacs by just not using rint.
## We also skip HAVE_RANDOM - see comments in src/conf_post.h.
case $opsys in
   hpux*) : ;;
   *) AC_CHECK_FUNCS(random rint) ;;
esac

dnl Cannot use AC_CHECK_FUNCS
AC_CACHE_CHECK([for __builtin_unwind_init],
	       emacs_cv_func___builtin_unwind_init,
[AC_LINK_IFELSE([AC_LANG_PROGRAM([], [__builtin_unwind_init ();])],
		emacs_cv_func___builtin_unwind_init=yes,
		emacs_cv_func___builtin_unwind_init=no)])
if test $emacs_cv_func___builtin_unwind_init = yes; then
  AC_DEFINE(HAVE___BUILTIN_UNWIND_INIT, 1,
	    [Define to 1 if you have the `__builtin_unwind_init' function.])
fi

AC_CHECK_HEADERS_ONCE(sys/un.h)

AC_FUNC_FSEEKO

AC_FUNC_GETPGRP

# UNIX98 PTYs.
AC_CHECK_FUNCS(grantpt)

# PTY-related GNU extensions.
AC_CHECK_FUNCS(getpt posix_openpt)

# Check this now, so that we will NOT find the above functions in ncurses.
# That is because we have not set up to link ncurses in lib-src.
# It's better to believe a function is not available
# than to expect to find it in ncurses.
# Also we need tputs and friends to be able to build at all.
AC_MSG_CHECKING([for library containing tputs])
# Run a test program that contains a call to tputs, a call that is
# never executed.  This tests whether a pre-'main' dynamic linker
# works with the library.  It's too much trouble to actually call
# tputs in the test program, due to portability hassles.  When
# cross-compiling, assume the test program will run if it links.
AC_DEFUN([tputs_link_source], [
  AC_LANG_SOURCE(
     [[extern void tputs (const char *, int, int (*)(int));
       int main (int argc, char **argv)
       {
	  if (argc == 10000)
	    tputs (argv[0], 0, 0);
	  return 0;
       }]])
])
# Maybe curses should be tried earlier?
# See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=9736#35
for tputs_library in '' tinfo ncurses terminfo termcap curses; do
  OLIBS=$LIBS
  if test -z "$tputs_library"; then
    LIBS_TERMCAP=
    msg='none required'
  else
    LIBS_TERMCAP=-l$tputs_library
    msg=$LIBS_TERMCAP
    LIBS="$LIBS_TERMCAP $LIBS"
  fi
  AC_RUN_IFELSE([tputs_link_source], [], [msg=no],
    [AC_LINK_IFELSE([tputs_link_source], [], [msg=no])])
  LIBS=$OLIBS
  if test "X$msg" != Xno; then
    break
  fi
done
AC_MSG_RESULT([$msg])
if test "X$msg" = Xno; then
  AC_MSG_ERROR([The required function `tputs' was not found in any library.
The following libraries were tried (in order):
  libtinfo, libncurses, libterminfo, libtermcap, libcurses
Please try installing whichever of these libraries is most appropriate
for your system, together with its header files.
For example, a libncurses-dev(el) or similar package.])
fi

## Use termcap instead of terminfo?
## Only true for: freebsd < 40000, ms-w32, msdos, netbsd < 599002500.
TERMINFO=yes
## FIXME?  In the cases below where we unconditionally set
## LIBS_TERMCAP="-lncurses", this overrides LIBS_TERMCAP = -ltinfo,
## if that was found above to have tputs.
## Should we use the gnu* logic everywhere?
case "$opsys" in
  ## darwin: Prevents crashes when running Emacs in Terminal.app under 10.2.
  ##  The ncurses library has been moved out of the System framework in
  ##  Mac OS X 10.2.  So if configure detects it, set the command-line
  ##  option to use it.
  darwin) LIBS_TERMCAP="-lncurses" ;;

  gnu*) test -z "$LIBS_TERMCAP" && LIBS_TERMCAP="-lncurses" ;;

  freebsd)
    AC_MSG_CHECKING([whether FreeBSD is new enough to use terminfo])
    AC_CACHE_VAL(emacs_cv_freebsd_terminfo,
    [AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <osreldate.h>]],
[[#if __FreeBSD_version < 400000
fail;
#endif
]])], emacs_cv_freebsd_terminfo=yes, emacs_cv_freebsd_terminfo=no)])

    AC_MSG_RESULT($emacs_cv_freebsd_terminfo)

    if test $emacs_cv_freebsd_terminfo = yes; then
      LIBS_TERMCAP="-lncurses"
    else
      TERMINFO=no
      LIBS_TERMCAP="-ltermcap"
    fi
    ;;

  netbsd)
    if test "x$LIBS_TERMCAP" != "x-lterminfo"; then
      TERMINFO=no
      LIBS_TERMCAP="-ltermcap"
    fi
    ;;

  openbsd) LIBS_TERMCAP="-lncurses" ;;

  ## hpux: Make sure we get select from libc rather than from libcurses
  ##  because libcurses on HPUX 10.10 has a broken version of select.
  ##  We used to use -lc -lcurses, but this may be cleaner.
  ## FIXME?  But TERMINFO = yes on hpux (it used to be explicitly
  # set that way, now it uses the default).  Isn't this a contradiction?
  hpux*) LIBS_TERMCAP="-ltermcap" ;;

esac

TERMCAP_OBJ=tparam.o
if test $TERMINFO = yes; then
  AC_DEFINE(TERMINFO, 1, [Define to 1 if you use terminfo instead of termcap.])
  TERMCAP_OBJ=terminfo.o
fi
AC_SUBST(LIBS_TERMCAP)
AC_SUBST(TERMCAP_OBJ)


# Do we have res_init, for detecting changes in /etc/resolv.conf?
# On Darwin, res_init appears not to be useful: see bug#562 and
# http://lists.gnu.org/archive/html/emacs-devel/2007-11/msg01467.html
resolv=no

if test $opsys != darwin; then

  AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <netinet/in.h>
#include <arpa/nameser.h>
#include <resolv.h>]],
    [[return res_init();]])],
    have_res_init=yes, have_res_init=no)
  if test "$have_res_init" = no; then
    OLIBS="$LIBS"
    LIBS="$LIBS -lresolv"
    AC_MSG_CHECKING(for res_init with -lresolv)
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <netinet/in.h>
#include <arpa/nameser.h>
#include <resolv.h>]],
      [[return res_init();]])],
      have_res_init=yes, have_res_init=no)
    AC_MSG_RESULT($have_res_init)
    if test "$have_res_init" = yes ; then
      resolv=yes
    fi
    LIBS="$OLIBS"
  fi

  if test "$have_res_init" = yes; then
    AC_DEFINE(HAVE_RES_INIT, 1, [Define to 1 if res_init is available.])
  fi
fi                              dnl !darwin

# Do we need the Hesiod library to provide the support routines?
dnl FIXME?  Should we be skipping this on Darwin too?
LIBHESIOD=
if test "$with_hesiod" != no ; then
  # Don't set $LIBS here -- see comments above.  FIXME which comments?
  AC_CHECK_FUNC(res_send, , [AC_CHECK_FUNC(__res_send, ,
     [AC_CHECK_LIB(resolv, res_send, resolv=yes,
		  [AC_CHECK_LIB(resolv, __res_send, resolv=yes)])])])
  if test "$resolv" = yes ; then
    RESOLVLIB=-lresolv
  else
    RESOLVLIB=
  fi
  AC_CHECK_FUNC(hes_getmailhost, , [AC_CHECK_LIB(hesiod, hes_getmailhost,
	hesiod=yes, :, $RESOLVLIB)])

  if test x"$hesiod" = xyes; then
    AC_DEFINE(HAVE_LIBHESIOD, 1,
              [Define to 1 if you have the hesiod library (-lhesiod).])
    LIBHESIOD=-lhesiod
  fi
fi
AC_SUBST(LIBHESIOD)

# Do we need libresolv (due to res_init or Hesiod)?
if test "$resolv" = yes && test $opsys != darwin; then
  AC_DEFINE(HAVE_LIBRESOLV, 1,
            [Define to 1 if you have the resolv library (-lresolv).])
  LIBRESOLV=-lresolv
else
  LIBRESOLV=
fi
AC_SUBST(LIBRESOLV)

# These tell us which Kerberos-related libraries to use.
COM_ERRLIB=
CRYPTOLIB=
KRB5LIB=
DESLIB=
KRB4LIB=

if test "${with_kerberos}" != no; then
  AC_CHECK_LIB(com_err, com_err, have_com_err=yes, have_com_err=no)
  if test $have_com_err = yes; then
    COM_ERRLIB=-lcom_err
    LIBS="$COM_ERRLIB $LIBS"
    AC_DEFINE(HAVE_LIBCOM_ERR, 1, [Define to 1 if you have the `com_err' library (-lcom_err).])
  fi
  AC_CHECK_LIB(crypto, mit_des_cbc_encrypt, have_crypto=yes, have_crypto=no)
  if test $have_crypto = yes; then
    CRYPTOLIB=-lcrypto
    LIBS="$CRYPTOLIB $LIBS"
    AC_DEFINE(HAVE_LIBCRYPTO, 1, [Define to 1 if you have the `crypto' library (-lcrypto).])
  fi
  AC_CHECK_LIB(k5crypto, mit_des_cbc_encrypt, have_k5crypto=yes, have_k5crypto=no)
  if test $have_k5crypto = yes; then
    CRYPTOLIB=-lk5crypto
    LIBS="$CRYPTOLIB $LIBS"
    AC_DEFINE(HAVE_LIBK5CRYPTO, 1, [Define to 1 if you have the `k5crypto' library (-lk5crypto).])
  fi
  AC_CHECK_LIB(krb5, krb5_init_context, have_krb5=yes, have_krb5=no)
  if test $have_krb5=yes; then
    KRB5LIB=-lkrb5
    LIBS="$KRB5LIB $LIBS"
    AC_DEFINE(HAVE_LIBKRB5, 1, [Define to 1 if you have the `krb5' library (-lkrb5).])
  fi
  dnl FIXME Simplify.  Does not match 22 logic, thanks to default_off?
  if test "${with_kerberos5}" = no; then
    AC_CHECK_LIB(des425, des_cbc_encrypt, have_des425=yes, have_des425=no )
    if test $have_des425 = yes; then
      DESLIB=-ldes425
      LIBS="$DESLIB $LIBS"
      AC_DEFINE(HAVE_LIBDES425, 1, [Define to 1 if you have the `des425' library (-ldes425).])
    else
      AC_CHECK_LIB(des, des_cbc_encrypt, have_des=yes, have_des=no)
      if test $have_des = yes; then
        DESLIB=-ldes
        LIBS="$DESLIB $LIBS"
        AC_DEFINE(HAVE_LIBDES, 1, [Define to 1 if you have the `des' library (-ldes).])
      fi
    fi
    AC_CHECK_LIB(krb4, krb_get_cred, have_krb4=yes, have_krb4=no)
    if test $have_krb4 = yes; then
      KRB4LIB=-lkrb4
      LIBS="$KRB4LIB $LIBS"
      AC_DEFINE(HAVE_LIBKRB4, 1, [Define to 1 if you have the `krb4' library (-lkrb4).])
    else
      AC_CHECK_LIB(krb, krb_get_cred, have_krb=yes, have_krb=no)
      if test $have_krb = yes; then
        KRB4LIB=-lkrb
        LIBS="$KRB4LIB $LIBS"
        AC_DEFINE(HAVE_LIBKRB, 1, [Define to 1 if you have the `krb' library (-lkrb).])
      fi
    fi
  fi

  if test "${with_kerberos5}" != no; then
    AC_CHECK_HEADERS(krb5.h,
      [AC_CHECK_MEMBERS([krb5_error.text, krb5_error.e_text],,,
		        [#include <krb5.h>])])
  else
    AC_CHECK_HEADERS(des.h,,
		     [AC_CHECK_HEADERS(kerberosIV/des.h,,
				       [AC_CHECK_HEADERS(kerberos/des.h)])])
    AC_CHECK_HEADERS(krb.h,,
		     [AC_CHECK_HEADERS(kerberosIV/krb.h,,
				       [AC_CHECK_HEADERS(kerberos/krb.h)])])
  fi
  AC_CHECK_HEADERS(com_err.h)
fi

AC_SUBST(COM_ERRLIB)
AC_SUBST(CRYPTOLIB)
AC_SUBST(KRB5LIB)
AC_SUBST(DESLIB)
AC_SUBST(KRB4LIB)

AC_CHECK_FUNCS_ONCE(tzset)
AC_MSG_CHECKING(whether localtime caches TZ)
AC_CACHE_VAL(emacs_cv_localtime_cache,
[if test x$ac_cv_func_tzset = xyes; then
AC_RUN_IFELSE([AC_LANG_SOURCE([[#include <time.h>
char TZ_GMT0[] = "TZ=GMT0";
char TZ_PST8[] = "TZ=PST8";
main()
{
  time_t now = time ((time_t *) 0);
  int hour_GMT0, hour_unset;
  if (putenv (TZ_GMT0) != 0)
    exit (1);
  hour_GMT0 = localtime (&now)->tm_hour;
  unsetenv("TZ");
  hour_unset = localtime (&now)->tm_hour;
  if (putenv (TZ_PST8) != 0)
    exit (1);
  if (localtime (&now)->tm_hour == hour_GMT0)
    exit (1);
  unsetenv("TZ");
  if (localtime (&now)->tm_hour != hour_unset)
    exit (1);
  exit (0);
}]])], emacs_cv_localtime_cache=no, emacs_cv_localtime_cache=yes,
[# If we have tzset, assume the worst when cross-compiling.
emacs_cv_localtime_cache=yes])
else
	# If we lack tzset, report that localtime does not cache TZ,
	# since we can't invalidate the cache if we don't have tzset.
	emacs_cv_localtime_cache=no
fi])dnl
AC_MSG_RESULT($emacs_cv_localtime_cache)
if test $emacs_cv_localtime_cache = yes; then
  AC_DEFINE(LOCALTIME_CACHE, 1,
	    [Define to 1 if localtime caches TZ.])
fi

ok_so_far=yes
AC_CHECK_FUNC(socket, , ok_so_far=no)
if test $ok_so_far = yes; then
  AC_CHECK_HEADER(netinet/in.h, , ok_so_far=no)
fi
if test $ok_so_far = yes; then
  AC_CHECK_HEADER(arpa/inet.h, , ok_so_far=no)
fi
if test $ok_so_far = yes; then
dnl Fixme: Not used.  Should this be HAVE_SOCKETS?
  AC_DEFINE(HAVE_INET_SOCKETS, 1,
	    [Define to 1 if you have inet sockets.])
fi

if test -f /usr/lpp/X11/bin/smt.exp; then
  AC_DEFINE(HAVE_AIX_SMT_EXP, 1,
	    [Define to 1 if the file /usr/lpp/X11/bin/smt.exp exists.])
fi

AC_MSG_CHECKING(whether system supports dynamic ptys)
if test -d /dev/pts && ls -d /dev/ptmx > /dev/null 2>&1 ; then
  AC_MSG_RESULT(yes)
  AC_DEFINE(HAVE_DEV_PTMX, 1, [Define to 1 if dynamic ptys are supported.])
else
  AC_MSG_RESULT(no)
fi

dnl Check for a Solaris 2.4 vfork bug that Autoconf misses (through 2.69).
dnl This can be removed once we assume Autoconf 2.70.
case $canonical in
  *-solaris2.4 | *-solaris2.4.*)
    dnl Disable the Autoconf-generated vfork test.
    : ${ac_cv_func_vfork_works=no};;
esac

AC_FUNC_FORK

AC_CHECK_FUNCS(snprintf)

dnl Adapted from Haible's version.
AC_CACHE_CHECK([for nl_langinfo and CODESET], emacs_cv_langinfo_codeset,
  [AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <langinfo.h>]],
    [[char* cs = nl_langinfo(CODESET);]])],
    emacs_cv_langinfo_codeset=yes,
    emacs_cv_langinfo_codeset=no)
  ])
if test $emacs_cv_langinfo_codeset = yes; then
  AC_DEFINE(HAVE_LANGINFO_CODESET, 1,
    [Define if you have <langinfo.h> and nl_langinfo(CODESET).])
fi

AC_TYPE_MBSTATE_T

AC_CACHE_CHECK([for C restricted array declarations], emacs_cv_c_restrict_arr,
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[void fred (int x[__restrict]);]], [[]])],
                  emacs_cv_c_restrict_arr=yes, emacs_cv_c_restrict_arr=no)])
if test "$emacs_cv_c_restrict_arr" = yes; then
  AC_DEFINE(__restrict_arr, __restrict,
    [Define to compiler's equivalent of C99 restrict keyword in array
     declarations.  Define as empty for no equivalent.])
fi

dnl Fixme: AC_SYS_POSIX_TERMIOS should probably be used, but it's not clear
dnl how the tty code is related to POSIX and/or other versions of termios.
dnl The following looks like a useful start.
dnl
dnl AC_SYS_POSIX_TERMIOS
dnl if test $ac_cv_sys_posix_termios = yes; then
dnl    AC_DEFINE(HAVE_TERMIOS, 1, [Define to 1 if you have POSIX-style functions
dnl                                and macros for terminal control.])
dnl    AC_DEFINE(HAVE_TCATTR, 1, [Define to 1 if you have tcgetattr and tcsetattr.])
dnl fi

dnl Turned on June 1996 supposing nobody will mind it.
AC_DEFINE(AMPERSAND_FULL_NAME, 1, [Define to use the convention that &
  in the full name stands for the login id.])

dnl Every platform that uses configure (ie every non-MS platform)
dnl supports this.  There is a create-lockfiles option you can
dnl customize if you do not want the lock files to be written.
dnl So it is not clear that this #define still needs to exist.
AC_DEFINE(CLASH_DETECTION, 1, [Define if you want lock files to be written,
  so that Emacs can tell instantly when you try to modify a file that
  someone else has modified in his/her Emacs.])

dnl Everybody supports this, except MS.
dnl Seems like the kind of thing we should be testing for, though.
## Note: PTYs are broken on darwin <6.  Use at your own risk.
AC_DEFINE(HAVE_PTYS, 1, [Define if the system supports pty devices.])

dnl Everybody supports this, except MS-DOS.
dnl Seems like the kind of thing we should be testing for, though.
dnl Compare with HAVE_INET_SOCKETS (which is unused...) above.
AC_DEFINE(HAVE_SOCKETS, 1, [Define if the system supports
  4.2-compatible sockets.])

AH_TEMPLATE(INTERNAL_TERMINAL, [This is substituted when $TERM is "internal".])

AC_DEFINE(NULL_DEVICE, ["/dev/null"], [Name of the file to open to get
  a null file, or a data sink.])

AC_DEFINE(SEPCHAR, [':'], [Character that separates PATH elements.])

dnl Everybody supports this, except MS-DOS.
AC_DEFINE(subprocesses, 1, [Define to enable asynchronous subprocesses.])

AC_DEFINE(USER_FULL_NAME, [pw->pw_gecos], [How to get a user's full name.])


AC_DEFINE(DIRECTORY_SEP, ['/'],
  [Character that separates directories in a file name.])

dnl Only used on MS platforms.
AH_TEMPLATE(DEVICE_SEP, [Character that separates a device in a file name.])

AC_DEFINE(IS_DEVICE_SEP(_c_), 0,
  [Returns true if character is a device separator.])

AC_DEFINE(IS_DIRECTORY_SEP(_c_), [((_c_) == DIRECTORY_SEP)],
  [Returns true if character is a directory separator.])

dnl On MS, this also accepts IS_DEVICE_SEP.
AC_DEFINE(IS_ANY_SEP(_c_), [(IS_DIRECTORY_SEP (_c_))],
  [Returns true if character is any form of separator.])


AH_TEMPLATE(NO_EDITRES, [Define if XEditRes should not be used.])

case $opsys in
  aix4-2)
    dnl Unfortunately without libXmu we cannot support EditRes.
    if test x$ac_cv_lib_Xmu_XmuConvertStandardSelection != xyes; then
      AC_DEFINE(NO_EDITRES, 1)
    fi
    ;;

  hpux*)
    dnl Assar Westerlund <assar@sics.se> says this is necessary for
    dnl HP-UX 10.20, and that it works for HP-UX 0 as well.
    AC_DEFINE(NO_EDITRES, 1)
    ;;
esac


case $opsys in
  irix6-5 | sol2* | unixware )
    dnl Some SVr4s don't define NSIG in sys/signal.h for ANSI environments;
    dnl instead, there's a system variable _sys_nsig.  Unfortunately, we
    dnl need the constant to dimension an array.  So wire in the appropriate
    dnl value here.
    AC_DEFINE(NSIG_MINIMUM, 32, [Minimum value of NSIG.])
    ;;
esac

emacs_broken_SIGIO=no

case $opsys in
  dnl SIGIO exists, but the feature doesn't work in the way Emacs needs.
  dnl See eg <http://article.gmane.org/gmane.os.openbsd.ports/46831>.
  hpux* | irix6-5 | openbsd | sol2* | unixware )
    emacs_broken_SIGIO=yes
    ;;

  aix4-2)
    dnl On AIX Emacs uses the gmalloc.c malloc implementation.  But given
    dnl the way this system works, libc functions that return malloced
    dnl memory use the libc malloc implementation. Calling xfree or
    dnl xrealloc on the results of such functions results in a crash.
    dnl
    dnl One solution for this could be to define SYSTEM_MALLOC in configure,
    dnl but that does not currently work on this system.
    dnl
    dnl It is possible to completely override the malloc implementation on
    dnl AIX, but that involves putting the malloc functions in a shared
    dnl library and setting the MALLOCTYPE environment variable to point to
    dnl that shared library.
    dnl
    dnl Emacs currently calls xrealloc on the results of get_current_dir name,
    dnl to avoid a crash just use the Emacs implementation for that function.
    dnl
    dnl FIXME We could change the AC_CHECK_FUNCS call near the start
    dnl of this file, so that we do not check for get_current_dir_name
    dnl on AIX.  But that might be fragile if something else ends
    dnl up testing for get_current_dir_name as a dependency.
    AC_DEFINE(BROKEN_GET_CURRENT_DIR_NAME, 1, [Define if
      get_current_dir_name should not be used.])
    ;;

  freebsd)
    dnl Circumvent a bug in FreeBSD.  In the following sequence of
    dnl writes/reads on a PTY, read(2) returns bogus data:
    dnl
    dnl write(2)  1022 bytes
    dnl write(2)   954 bytes, get EAGAIN
    dnl read(2)   1024 bytes in process_read_output
    dnl read(2)     11 bytes in process_read_output
    dnl
    dnl That is, read(2) returns more bytes than have ever been written
    dnl successfully.  The 1033 bytes read are the 1022 bytes written
    dnl successfully after processing (for example with CRs added if the
    dnl terminal is set up that way which it is here).  The same bytes will
    dnl be seen again in a later read(2), without the CRs.
    AC_DEFINE(BROKEN_PTY_READ_AFTER_EAGAIN, 1, [Define on FreeBSD to
      work around an issue when reading from a PTY.])
    ;;
esac

case $opsys in
  gnu-* | sol2-10 )
    dnl FIXME Can't we test if this exists (eg /proc/$$)?
    AC_DEFINE(HAVE_PROCFS, 1, [Define if you have the /proc filesystem.])
  ;;
esac

case $opsys in
  darwin | freebsd | netbsd | openbsd )
    AC_DEFINE(DONT_REOPEN_PTY, 1, [Define if process.c does not need to
      close a pty to make it a controlling terminal (it is already a
      controlling terminal of the subprocess, because we did ioctl TIOCSCTTY).])
  ;;
esac

dnl FIXME Surely we can test for this rather than hard-code it.
case $opsys in
  netbsd | openbsd) sound_device="/dev/audio" ;;
  *) sound_device="/dev/dsp" ;;
esac

dnl Used in sound.c
AC_DEFINE_UNQUOTED(DEFAULT_SOUND_DEVICE, "$sound_device",
  [Name of the default sound device.])


dnl Emacs can read input using SIGIO and buffering characters itself,
dnl or using CBREAK mode and making C-g cause SIGINT.
dnl The choice is controlled by the variable interrupt_input.
dnl
dnl Define INTERRUPT_INPUT to make interrupt_input = 1 the default (use SIGIO)
dnl
dnl Emacs uses the presence of the USABLE_SIGIO macro
dnl to indicate whether or not signal-driven I/O is possible.  It uses
dnl INTERRUPT_INPUT to decide whether to use it by default.
dnl
dnl SIGIO can be used only on systems that implement it (4.2 and 4.3).
dnl CBREAK mode has two disadvantages
dnl 1) At least in 4.2, it is impossible to handle the Meta key properly.
dnl I hear that in system V this problem does not exist.
dnl 2) Control-G causes output to be discarded.
dnl I do not know whether this can be fixed in system V.
dnl
dnl Another method of doing input is planned but not implemented.
dnl It would have Emacs fork off a separate process
dnl to read the input and send it to the true Emacs process
dnl through a pipe.
case $opsys in
  darwin | gnu-linux | gnu-kfreebsd )
    AC_DEFINE(INTERRUPT_INPUT, 1, [Define to read input using SIGIO.])
  ;;
esac


dnl If the system's imake configuration file defines `NeedWidePrototypes'
dnl as `NO', we must define NARROWPROTO manually.  Such a define is
dnl generated in the Makefile generated by `xmkmf'.  If we don't define
dnl NARROWPROTO, we will see the wrong function prototypes for X functions
dnl taking float or double parameters.
case $opsys in
  cygwin|gnu|gnu-linux|gnu-kfreebsd|irix6-5|freebsd|netbsd|openbsd)
    AC_DEFINE(NARROWPROTO, 1, [Define if system's imake configuration
      file defines `NeedWidePrototypes' as `NO'.])
  ;;
esac


dnl Used in process.c, this must be a loop, even if it only runs once.
dnl (Except on SGI; see below.  Take that, clarity and consistency!)
AH_TEMPLATE(PTY_ITERATION, [How to iterate over PTYs.])
dnl Only used if !PTY_ITERATION.  Iterate from FIRST_PTY_LETTER to z,
dnl trying suffixes 0-16.
AH_TEMPLATE(FIRST_PTY_LETTER, [Letter to use in finding device name of
  first PTY, if PTYs are supported.])
AH_TEMPLATE(PTY_OPEN, [How to open a PTY, if non-standard.])
AH_TEMPLATE(PTY_NAME_SPRINTF, [How to get the device name of the control
  end of a PTY, if non-standard.])
AH_TEMPLATE(PTY_TTY_NAME_SPRINTF, [How to get device name of the tty
  end of a PTY, if non-standard.])

case $opsys in
  aix4-2 )
    AC_DEFINE(PTY_ITERATION, [int c; for (c = 0; !c ; c++)])
    dnl You allocate a pty by opening /dev/ptc to get the master side.
    dnl To get the name of the slave side, you just ttyname() the master side.
    AC_DEFINE(PTY_NAME_SPRINTF, [strcpy (pty_name, "/dev/ptc");])
    AC_DEFINE(PTY_TTY_NAME_SPRINTF, [strcpy (pty_name, ttyname (fd));])
    ;;

  cygwin )
    AC_DEFINE(PTY_ITERATION, [int i; for (i = 0; i < 1; i++)])
    dnl multi-line AC_DEFINEs are hard. :(
    AC_DEFINE(PTY_OPEN, [ do { int dummy; sigset_t blocked, procmask; sigemptyset (&blocked); sigaddset (&blocked, SIGCHLD); pthread_sigmask (SIG_BLOCK, &blocked, &procmask); if (-1 == openpty (&fd, &dummy, pty_name, 0, 0)) fd = -1; pthread_sigmask (SIG_SETMASK, &procmask, 0); if (fd >= 0) emacs_close (dummy); } while (0)])
    AC_DEFINE(PTY_NAME_SPRINTF, [])
    AC_DEFINE(PTY_TTY_NAME_SPRINTF, [])
    ;;

  dnl FIXME?  Maybe use same as freebsd - see bug#12040.
  darwin )
    AC_DEFINE(PTY_ITERATION, [int i; for (i = 0; i < 1; i++)])
    dnl Not used, because PTY_ITERATION is defined.
    AC_DEFINE(FIRST_PTY_LETTER, ['p'])
    dnl Note that openpty may fork via grantpt on Mac OS X 10.4/Darwin 8.
    dnl But we don't have to block SIGCHLD because it is blocked in the
    dnl implementation of grantpt.
    AC_DEFINE(PTY_OPEN, [ do { int slave; if (openpty (&fd, &slave, pty_name, NULL, NULL) == -1) fd = -1; else emacs_close (slave); } while (0)])
    AC_DEFINE(PTY_NAME_SPRINTF, [])
    AC_DEFINE(PTY_TTY_NAME_SPRINTF, [])
    ;;

  gnu | openbsd )
    AC_DEFINE(FIRST_PTY_LETTER, ['p'])
    ;;

  gnu-linux | gnu-kfreebsd | freebsd | netbsd )
    dnl if HAVE_GRANTPT
    if test "x$ac_cv_func_grantpt" = xyes; then
      AC_DEFINE(UNIX98_PTYS, 1, [Define if the system has Unix98 PTYs.])
      AC_DEFINE(PTY_ITERATION, [int i; for (i = 0; i < 1; i++)])
      dnl Note that grantpt and unlockpt may fork.  We must block SIGCHLD
      dnl to prevent sigchld_handler from intercepting the child's death.
      AC_DEFINE(PTY_TTY_NAME_SPRINTF, [{ char *ptyname = 0; sigset_t blocked; sigemptyset (&blocked); sigaddset (&blocked, SIGCHLD); pthread_sigmask (SIG_BLOCK, &blocked, 0); if (grantpt (fd) != -1 && unlockpt (fd) != -1) ptyname = ptsname(fd); pthread_sigmask (SIG_UNBLOCK, &blocked, 0); if (!ptyname) { close (fd); return -1; } snprintf (pty_name, sizeof pty_name, "%s", ptyname); }])
      dnl if HAVE_POSIX_OPENPT
      if test "x$ac_cv_func_posix_openpt" = xyes; then
        AC_DEFINE(PTY_OPEN, [fd = posix_openpt (O_RDWR | O_NOCTTY)])
        AC_DEFINE(PTY_NAME_SPRINTF, [])
      dnl if HAVE_GETPT
      elif test "x$ac_cv_func_getpt" = xyes; then
        AC_DEFINE(PTY_OPEN, [fd = getpt ()])
        AC_DEFINE(PTY_NAME_SPRINTF, [])
      else
        AC_DEFINE(PTY_NAME_SPRINTF, [strcpy (pty_name, "/dev/ptmx");])
      fi
    else
      AC_DEFINE(FIRST_PTY_LETTER, ['p'])
    fi
    ;;

  hpux*)
    AC_DEFINE(FIRST_PTY_LETTER, ['p'])
    AC_DEFINE(PTY_NAME_SPRINTF, [sprintf (pty_name, "/dev/ptym/pty%c%x", c, i);])
    AC_DEFINE(PTY_TTY_NAME_SPRINTF, [sprintf (pty_name, "/dev/pty/tty%c%x", c, i);])
    ;;

  irix6-5 )
    dnl It looks like this cannot be right, because it is not a loop.
    dnl However, process.c actually does this:
    dnl # ifndef __sgi
    dnl   continue;
    dnl # else
    dnl   return -1;
    dnl # endif
    dnl which presumably makes it OK, since irix == sgi (?).
    dnl FIXME it seems like this special treatment is unnecessary?
    dnl Why can't irix use a single-trip loop like eg cygwin?
    AC_DEFINE(PTY_ITERATION, [])
    dnl Not used, because PTY_ITERATION is defined.
    AC_DEFINE(FIRST_PTY_LETTER, ['q'])
    AC_DEFINE(PTY_OPEN, [ { struct sigaction ocstat, cstat; struct stat stb; char * name; sigemptyset(&cstat.sa_mask); cstat.sa_handler = SIG_DFL; cstat.sa_flags = 0; sigaction(SIGCLD, &cstat, &ocstat); name = _getpty (&fd, O_RDWR | O_NDELAY, 0600, 0); sigaction(SIGCLD, &ocstat, (struct sigaction *)0); if (name == 0) return -1; if (fd < 0) return -1; if (fstat (fd, &stb) < 0) return -1; strcpy (pty_name, name); }])
    dnl No need to get the pty name at all.
    AC_DEFINE(PTY_NAME_SPRINTF, [])
    dnl No need to use sprintf to get the tty name--we get that from _getpty.
    AC_DEFINE(PTY_TTY_NAME_SPRINTF, [])
    ;;

  sol2* )
    dnl On SysVr4, grantpt(3) forks a subprocess, so keep sigchld_handler()
    dnl from intercepting that death.  If any child but grantpt's should die
    dnl within, it should be caught after sigrelse(2).
    AC_DEFINE(PTY_TTY_NAME_SPRINTF, [{ char *ptsname (int), *ptyname; int grantpt_result; sigset_t blocked; sigemptyset (&blocked); sigaddset (&blocked, SIGCLD); pthread_sigmask (SIG_BLOCK, &blocked, 0); grantpt_result = grantpt (fd); pthread_sigmask (SIG_UNBLOCK, &blocked, 0); if (grantpt_result == -1 || unlockpt (fd) == -1 || !(ptyname = ptsname (fd))) { emacs_close (fd); return -1; } snprintf (pty_name, sizeof pty_name, "%s", ptyname); }])
    ;;

  unixware )
    dnl Comments are as per sol2*.
    AC_DEFINE(PTY_TTY_NAME_SPRINTF, [{ char *ptsname (int), *ptyname; int grantpt_result; sigset_t blocked; sigemptyset (&blocked); sigaddset (&blocked, SIGCLD); pthread_sigmask (SIG_BLOCK, &blocked, 0); grantpt_result = grantpt (fd); pthread_sigmask (SIG_UNBLOCK, &blocked, 0); if (grantpt_result == -1) fatal("could not grant slave pty"); if (unlockpt(fd) == -1) fatal("could not unlock slave pty"); if (!(ptyname = ptsname(fd))) fatal ("could not enable slave pty"); snprintf (pty_name, sizeof pty_name, "%s", ptyname); }])
    ;;
esac


case $opsys in
  sol2* | unixware )
    dnl This change means that we don't loop through allocate_pty too
    dnl many times in the (rare) event of a failure.
    AC_DEFINE(FIRST_PTY_LETTER, ['z'])
    AC_DEFINE(PTY_NAME_SPRINTF, [strcpy (pty_name, "/dev/ptmx");])
    dnl Push various streams modules onto a PTY channel.  Used in process.c.
    AC_DEFINE(SETUP_SLAVE_PTY, [if (ioctl (xforkin, I_PUSH, "ptem") == -1) fatal ("ioctl I_PUSH ptem"); if (ioctl (xforkin, I_PUSH, "ldterm") == -1) fatal ("ioctl I_PUSH ldterm"); if (ioctl (xforkin, I_PUSH, "ttcompat") == -1) fatal ("ioctl I_PUSH ttcompat");], [How to set up a slave PTY, if needed.])
    ;;
esac


AH_TEMPLATE(SIGNALS_VIA_CHARACTERS, [Make process_send_signal work by
"typing" a signal character on the pty.])

case $opsys in
  dnl Perry Smith <pedz@ddivt1.austin.ibm.com> says this is correct for AIX.
  dnl thomas@mathematik.uni-bremen.de says this is needed for IRIX.
  aix4-2 | cygwin | gnu | irix6-5 | freebsd | netbsd | openbsd | darwin )
    AC_DEFINE(SIGNALS_VIA_CHARACTERS, 1)
    ;;

  dnl 21 Jun 06: Eric Hanchrow <offby1@blarg.net> says this works.
  dnl FIXME Does gnu-kfreebsd have linux/version.h?  It seems unlikely...
  gnu-linux | gnu-kfreebsd )

    AC_MSG_CHECKING([for signals via characters])
    AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
#include <linux/version.h>
#if LINUX_VERSION_CODE < 0x20400
# error "Linux version too old"
#endif
      ]], [[]])], emacs_signals_via_chars=yes, emacs_signals_via_chars=no)

    AC_MSG_RESULT([$emacs_signals_via_chars])
    test $emacs_signals_via_chars = yes && AC_DEFINE(SIGNALS_VIA_CHARACTERS, 1)
    ;;
esac


dnl Used in dispnew.c
AH_TEMPLATE(PENDING_OUTPUT_COUNT, [Number of chars of output in the
  buffer of a stdio stream.])

AC_MSG_CHECKING([whether we are using the GNU C library])
AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
#include <features.h>
#ifndef __GNU_LIBRARY__
# error "this is not the GNU C library"
#endif
  ]], [[]])], emacs_glibc=yes, emacs_glibc=no)
AC_MSG_RESULT([$emacs_glibc])

if test $emacs_glibc = yes; then

  emacs_pending_output=unknown

  case $opsys in
    gnu | gnu-linux | gnu-kfreebsd )
      AC_MSG_CHECKING([for style of pending output formalism])
      dnl In autoconf 2.67 and later, we could use a single test
      dnl since the preprocessed output is accessible in "conftest.i".
      AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
#include <stdio.h>
#if !defined (_IO_STDIO_H) && !defined (_STDIO_USES_IOSTREAM)
# error "stdio definitions not found"
#endif
        ]], [[]])], emacs_pending_output=new, [])

      if test $emacs_pending_output = unknown; then
        case $opsys in
          gnu-linux | gnu-kfreebsd)
            AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
#include <stdio.h>
#ifndef __UCLIBC__
# error "not using uclibc"
#endif
            ]], [[]])], emacs_pending_output=uclibc, emacs_pending_output=old)
            ;;
        esac
      fi

      AC_MSG_RESULT([$emacs_pending_output])

      case $emacs_pending_output in
        new)
          dnl New C libio names.
          AC_DEFINE(PENDING_OUTPUT_COUNT(FILE),
            [((FILE)->_IO_write_ptr - (FILE)->_IO_write_base)])
          ;;
        uclibc)
          dnl Using the uClibc library.
          AC_DEFINE(PENDING_OUTPUT_COUNT(FILE),
            [((FILE)->__bufpos - (FILE)->__bufstart)])
          ;;
        old)
          dnl Old C++ iostream names.
          AC_DEFINE(PENDING_OUTPUT_COUNT(FILE),
            [((FILE)->_pptr - (FILE)->_pbase)])
          ;;
      esac
    ;;
  esac                          dnl opsys

  if test $emacs_pending_output = unknown; then
    AC_DEFINE(PENDING_OUTPUT_COUNT(FILE), [((FILE)->__bufp - (FILE)->__buffer)])
  fi

else                            dnl !emacs_glibc

  case $opsys in
    cygwin | darwin | freebsd | netbsd | openbsd )
      AC_DEFINE(PENDING_OUTPUT_COUNT(FILE), [((FILE)->_p - (FILE)->_bf._base)])
      ;;

    unixware)
      AC_DEFINE(PENDING_OUTPUT_COUNT(FILE), [((FILE)->__ptr - (FILE)->__base)])
      ;;

    *)
      dnl HAVE_STDIO_EXT_H && HAVE___FPENDING
      if test x$ac_cv_header_stdio_ext_h = xyes && \
        test x$ac_cv_func___fpending = xyes; then
        AC_DEFINE(PENDING_OUTPUT_COUNT(FILE), [__fpending (FILE)])
        AC_DEFINE(DISPNEW_NEEDS_STDIO_EXT, 1,
          [Define if dispnew.c should include stdio_ext.h.])
      else
        AC_DEFINE(PENDING_OUTPUT_COUNT(FILE), [((FILE)->_ptr - (FILE)->_base)])
      fi
      ;;
  esac
fi                              dnl emacs_glibc


dnl Used in vm-limit.c
AH_TEMPLATE(DATA_START, [Address of the start of the data segment.])
dnl Used in lisp.h, emacs.c, mem-limits.h
dnl NEWS.18 describes this as "a number which contains
dnl the high bits to be inclusive or'ed with pointers that are unpacked."
AH_TEMPLATE(DATA_SEG_BITS, [Extra bits to be or'd in with any pointers
stored in a Lisp_Object.])
dnl if Emacs uses fewer than 32 bits for the value field of a LISP_OBJECT.

case $opsys in
  gnu)
    dnl libc defines data_start.
    AC_DEFINE(DATA_START, [({ extern int data_start; (char *) &data_start; })])
    ;;

  aix*)
    dnl This works with 32-bit executables; Emacs doesn't support 64-bit.
    AC_DEFINE(DATA_START, [0x20000000])
    AC_DEFINE(DATA_SEG_BITS, [0x20000000])
    ;;
  hpux*)
    dnl The data segment on this machine always starts at address 0x40000000.
    AC_DEFINE(DATA_START, [0x40000000])
    AC_DEFINE(DATA_SEG_BITS, [0x40000000])
    ;;
  irix6-5)
    AC_DEFINE(DATA_START, [0x10000000])
    AC_DEFINE(DATA_SEG_BITS, [0x10000000])
    ;;
esac


AH_TEMPLATE(ULIMIT_BREAK_VALUE, [Undocumented.])
AH_TEMPLATE(TAB3, [Undocumented.])

case $opsys in
  darwin) AC_DEFINE(TAB3, OXTABS) ;;

  gnu | freebsd | netbsd | openbsd )
    AC_DEFINE(TABDLY, OXTABS, [Undocumented.])
    AC_DEFINE(TAB3, OXTABS)
    ;;

  gnu-linux | gnu-kfreebsd )
    dnl libc-linux/sysdeps/linux/i386/ulimit.c says that due to shared
    dnl library, we cannot get the maximum address for brk.
    AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
#ifndef __i386__
# error "not i386"
#endif
      ]], [[]])], AC_DEFINE(ULIMIT_BREAK_VALUE, [(32*1024*1024)]), [])

    AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
#ifndef __ia64__
# error "not ia64"
#endif
      ]], [[]])], AC_DEFINE(GC_MARK_SECONDARY_STACK(),
        [do { extern void *__libc_ia64_register_backing_store_base; __builtin_ia64_flushrs (); mark_memory (__libc_ia64_register_backing_store_base, __builtin_ia64_bsp ());} while (0)],
        [Mark a secondary stack, like the register stack on the ia64.]), [])
    ;;

  hpux*)
    AC_DEFINE(RUN_TIME_REMAP, 1, [Define if emacs.c needs to call
      run_time_remap; for HPUX.])
    ;;

  irix6-5)
    dnl Ulimit(UL_GMEMLIM) is busted...
    AC_DEFINE(ULIMIT_BREAK_VALUE, [0x14000000])
    ;;
esac


dnl These won't be used automatically yet.  We also need to know, at least,
dnl that the stack is continuous.
AH_TEMPLATE(GC_SETJMP_WORKS, [Define if setjmp is known to save all
  registers relevant for conservative garbage collection in the jmp_buf.])

AH_TEMPLATE(GC_MARK_STACK, [Define to GC_USE_GCPROS_AS_BEFORE if
  conservative garbage collection is not known to work.])


case $opsys in
  hpux* | unixware)
    dnl Conservative garbage collection has not been tested, so for now
    dnl play it safe and stick with the old-fashioned way of marking.
    AC_DEFINE(GC_MARK_STACK, [GC_USE_GCPROS_AS_BEFORE])
    ;;

  dnl Not all the architectures are tested, but there are Debian packages
  dnl for SCM and/or Guile on them, so the technique must work.  See also
  dnl comments in alloc.c concerning setjmp and gcc.
  dnl Fixme: it's probably safe to just use the GCC conditional below.
  gnu-linux | gnu-kfreebsd )
    AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
#if defined __i386__ || defined __sparc__ || defined __mc68000__ \
  || defined __alpha__ || defined __mips__ || defined __s390__ \
  || defined __arm__ || defined __powerpc__ || defined __amd64__ \
  || defined __ia64__ || defined __sh__
/* ok */
#else
# error "setjmp not known to work on this arch"
#endif
    ]], [[]])], AC_DEFINE(GC_SETJMP_WORKS, 1),
      AC_DEFINE(GC_MARK_STACK, [GC_USE_GCPROS_AS_BEFORE]) )
    ;;
esac


if test x$GCC = xyes; then
   dnl GC_SETJMP_WORKS is nearly always appropriate for GCC.
   AC_DEFINE(GC_SETJMP_WORKS, 1)
else
  case $opsys in
    dnl irix: Tested on Irix 6.5.  SCM worked on earlier versions.
    freebsd | netbsd | openbsd | irix6-5 | sol2* )
      AC_DEFINE(GC_SETJMP_WORKS, 1)
      ;;
  esac
fi                              dnl GCC?

AC_CACHE_CHECK([for _setjmp], [emacs_cv_func__setjmp],
  [AC_LINK_IFELSE(
     [AC_LANG_PROGRAM(
       [[#include <setjmp.h>
       ]],
       [[jmp_buf j;
	 if (! _setjmp (j))
	   _longjmp (j, 1);]])],
     [emacs_cv_func__setjmp=yes],
     [emacs_cv_func__setjmp=no])])
if test $emacs_cv_func__setjmp = yes; then
  AC_DEFINE([HAVE__SETJMP], 1, [Define to 1 if _setjmp and _longjmp work.])
else
  AC_CACHE_CHECK([for sigsetjmp], [emacs_cv_func_sigsetjmp],
    [AC_LINK_IFELSE(
       [AC_LANG_PROGRAM(
	 [[#include <setjmp.h>
	 ]],
	 [[sigjmp_buf j;
	   if (! sigsetjmp (j, 1))
	     siglongjmp (j, 1);]])],
       [emacs_cv_func_sigsetjmp=yes],
       [emacs_cv_func_sigsetjmp=no])])
  if test $emacs_cv_func_sigsetjmp = yes; then
    AC_DEFINE([HAVE_SIGSETJMP], 1,
      [Define to 1 if sigsetjmp and siglongjmp work.
       The value of this symbol is irrelevant if HAVE__SETJMP is defined.])
  fi
fi

case $opsys in
  sol2* | unixware )
    dnl TIOCGPGRP is broken in SysVr4, so we can't send signals to PTY
    dnl subprocesses the usual way.  But TIOCSIGNAL does work for PTYs,
    dnl and this is all we need.
    AC_DEFINE(TIOCSIGSEND, TIOCSIGNAL, [Some platforms redefine this.])
    ;;
esac


case $opsys in
  hpux* | sol2* )
    dnl Used in xfaces.c.
    AC_DEFINE(XOS_NEEDS_TIME_H, 1, [Compensate for a bug in Xos.h on
      some systems, where it requires time.h.])
    ;;
esac


dnl Define symbols to identify the version of Unix this is.
dnl Define all the symbols that apply correctly.
AH_TEMPLATE(BSD4_2, [Define if the system is compatible with BSD 4.2.])
AH_TEMPLATE(BSD_SYSTEM, [Define if the system is compatible with BSD 4.2.])
AH_TEMPLATE(DOS_NT, [Define if the system is MS DOS or MS Windows.])
AH_TEMPLATE(MSDOS, [Define if the system is MS DOS.])
AH_TEMPLATE(USG, [Define if the system is compatible with System III.])
AH_TEMPLATE(USG5, [Define if the system is compatible with System V.])
AH_TEMPLATE(USG5_4, [Define if the system is compatible with System V Release 4.])

case $opsys in
  aix4-2)
    AC_DEFINE(USG, [])
    AC_DEFINE(USG5, [])
    dnl This symbol should be defined on AIX Version 3  ???????
    AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
#ifndef _AIX
# error "_AIX not defined"
#endif
    ]], [[]])], [], AC_DEFINE(_AIX, [], [Define if the system is AIX.]))
    ;;

  cygwin)
    AC_DEFINE(CYGWIN, 1, [Define if the system is Cygwin.])
    ;;

  darwin)
    dnl BSD4_3 and BSD4_4 are already defined in sys/param.h.
    AC_DEFINE(BSD4_2, [])
    AC_DEFINE(BSD_SYSTEM, [])
    dnl More specific than the above two.  We cannot use __APPLE__ as this
    dnl may not be defined on non-OSX Darwin, and we cannot define DARWIN
    dnl here because Panther and lower CoreFoundation.h uses DARWIN to
    dnl distinguish OS X from pure Darwin.
    AC_DEFINE(DARWIN_OS, [], [Define if the system is Darwin.])
    ;;

  freebsd)
    AC_DEFINE(BSD4_2, [])
    dnl Hack to avoid calling AC_PREPROC_IFELSE multiple times.
    dnl Would not be needed with autoconf >= 2.67, where the
    dnl preprocessed output is accessible in "conftest.i".
    AC_DEFINE(BSD_SYSTEM_AHB, 1, [Define if AH_BOTTOM should change BSD_SYSTEM.])
    ;;

  gnu | netbsd | openbsd )
    AC_DEFINE(BSD4_2, [])
    AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
#ifndef BSD_SYSTEM
# error "BSD_SYSTEM not defined"
#endif
    ]], [[]])], [], AC_DEFINE(BSD_SYSTEM, 43) )
    ;;

  gnu-linux | gnu-kfreebsd )
    AC_DEFINE(USG, [])
    AC_DEFINE(GNU_LINUX, [], [Define if ths system is compatible with GNU/Linux.])
    ;;

  hpux*)
    AC_DEFINE(USG, [])
    AC_DEFINE(USG5, [])
    AC_DEFINE(HPUX, [], [Define if the system is HPUX.])
    ;;

  irix6-5)
    AC_DEFINE(USG, [])
    AC_DEFINE(USG5, [])
    AC_DEFINE(USG5_4, [])
    AC_DEFINE(IRIX6_5, [], [Define if the system is IRIX.])
    ;;

  sol2*)
    AC_DEFINE(USG, [])
    AC_DEFINE(USG5, [])
    AC_DEFINE(USG5_4, [])
    AC_DEFINE(SOLARIS2, [], [Define if the system is Solaris.])
    ;;

  unixware)
    AC_DEFINE(USG, [])
    AC_DEFINE(USG5, [])
    AC_DEFINE(USG5_4, [])
    ;;
esac

AC_CACHE_CHECK([for usable FIONREAD], [emacs_cv_usable_FIONREAD],
  [case $opsys in
     aix4-2)
       dnl BUILD 9008 - FIONREAD problem still exists in X-Windows.
       emacs_cv_usable_FIONREAD=no
       ;;

     *)
       AC_COMPILE_IFELSE(
	 [AC_LANG_PROGRAM([[#include <sys/types.h>
			    #include <sys/ioctl.h>
			    #ifdef USG5_4
			    # include <sys/filio.h>
			    #endif
			  ]],
			  [[int foo = ioctl (0, FIONREAD, &foo);]])],
	 [emacs_cv_usable_FIONREAD=yes],
	 [emacs_cv_usable_FIONREAD=no])
       ;;
   esac])
if test $emacs_cv_usable_FIONREAD = yes; then
  AC_DEFINE([USABLE_FIONREAD], [1], [Define to 1 if FIONREAD is usable.])

  if test $emacs_broken_SIGIO = no; then
    AC_CACHE_CHECK([for usable SIGIO], [emacs_cv_usable_SIGIO],
      [AC_COMPILE_IFELSE(
	 [AC_LANG_PROGRAM([[#include <fcntl.h>
			    #include <signal.h>
			  ]],
			  [[int foo = SIGIO | F_SETFL | FASYNC;]])],
	 [emacs_cv_usable_SIGIO=yes],
	 [emacs_cv_usable_SIGIO=no])],
      [emacs_cv_usable_SIGIO=yes],
      [emacs_cv_usable_SIGIO=no])
    if test $emacs_cv_usable_SIGIO = yes; then
      AC_DEFINE([USABLE_SIGIO], [1], [Define to 1 if SIGIO is usable.])
    fi
  fi
fi


case $opsys in
  dnl Emacs supplies its own malloc, but glib (part of Gtk+) calls
  dnl memalign and on Cygwin, that becomes the Cygwin-supplied memalign.
  dnl As malloc is not the Cygwin malloc, the Cygwin memalign always
  dnl returns ENOSYS.  A workaround is to set G_SLICE=always-malloc. */
  cygwin)
    AC_DEFINE(G_SLICE_ALWAYS_MALLOC, 1, [Define to set the
      G_SLICE environment variable to "always-malloc" at startup, if
      using GTK.])
    ;;

  hpux11)
    dnl It works to open the pty's tty in the parent (Emacs), then
    dnl close and reopen it in the child.
    AC_DEFINE(USG_SUBTTY_WORKS, 1, [Define for USG systems where it
      works to open a pty's tty in the parent process, then close and
      reopen it in the child.])
    ;;

  irix6-5)
    AC_DEFINE(PREFER_VSUSP, 1, [Define if process_send_signal should
      use VSUSP instead of VSWTCH.])
    AC_DEFINE(SETPGRP_RELEASES_CTTY, 1, [Define if process.c:child_setup
      should not call setpgrp.])
    ;;

  sol2-10)
    AC_DEFINE(_STRUCTURED_PROC, 1, [Needed for system_process_attributes
      on Solaris.])
    ;;
esac

# Set up the CFLAGS for real compilation, so we can substitute it.
CFLAGS="$REAL_CFLAGS"
CPPFLAGS="$REAL_CPPFLAGS"

## Hack to detect a buggy GCC version.
if test "x$GCC" = xyes \
   && test x"`$CC --version 2> /dev/null | grep 'gcc.* 4.5.0'`" != x \
   && test x"`echo $CFLAGS | grep '\-O@<:@23@:>@'`" != x \
   && test x"`echo $CFLAGS | grep '\-fno-optimize-sibling-calls'`" = x; then
   AC_MSG_ERROR([GCC 4.5.0 has problems compiling Emacs; see etc/PROBLEMS'.])
fi

version=$PACKAGE_VERSION

copyright="Copyright (C) 2013 Free Software Foundation, Inc."
AC_DEFINE_UNQUOTED(COPYRIGHT, ["$copyright"],
  [Short copyright string for this version of Emacs.])
AC_SUBST(copyright)

### Specify what sort of things we'll be editing into Makefile and config.h.
### Use configuration here uncanonicalized to avoid exceeding size limits.
AC_SUBST(version)
AC_SUBST(configuration)
## Unused?
AC_SUBST(canonical)
AC_SUBST(srcdir)
AC_SUBST(prefix)
AC_SUBST(exec_prefix)
AC_SUBST(bindir)
AC_SUBST(datadir)
AC_SUBST(sharedstatedir)
AC_SUBST(libexecdir)
AC_SUBST(mandir)
AC_SUBST(infodir)
AC_SUBST(lispdir)
AC_SUBST(leimdir)
AC_SUBST(standardlisppath)
AC_SUBST(locallisppath)
AC_SUBST(lisppath)
AC_SUBST(x_default_search_path)
AC_SUBST(etcdir)
AC_SUBST(archlibdir)
AC_SUBST(docdir)
AC_SUBST(bitmapdir)
AC_SUBST(gamedir)
AC_SUBST(gameuser)
## FIXME? Nothing uses @LD_SWITCH_X_SITE@.
## src/Makefile.in did add LD_SWITCH_X_SITE (as a cpp define) to the
## end of LIBX_BASE, but nothing ever set it.
AC_SUBST(LD_SWITCH_X_SITE)
AC_SUBST(C_SWITCH_X_SITE)
AC_SUBST(GNUSTEP_CFLAGS)
AC_SUBST(CFLAGS)
## Used in lwlib/Makefile.in.
AC_SUBST(X_TOOLKIT_TYPE)
AC_SUBST(ns_appdir)
AC_SUBST(ns_appbindir)
AC_SUBST(ns_appresdir)
AC_SUBST(ns_appsrc)
AC_SUBST(GNU_OBJC_CFLAGS)
AC_SUBST(OTHER_FILES)

if test -n "${term_header}"; then
    AC_DEFINE_UNQUOTED(TERM_HEADER, "${term_header}",
        [Define to the header for the built-in window system.])
fi

AC_DEFINE_UNQUOTED(EMACS_CONFIGURATION,  "${canonical}",
		   [Define to the canonical Emacs configuration name.])
AC_DEFINE_UNQUOTED(EMACS_CONFIG_OPTIONS, "${ac_configure_args}",
		   [Define to the options passed to configure.])
AH_TEMPLATE(config_opsysfile, [Some platforms that do not use configure
  define this to include extra configuration information.])

XMENU_OBJ=
XOBJ=
FONT_OBJ=
if test "${HAVE_X_WINDOWS}" = "yes" ; then
  AC_DEFINE(HAVE_X_WINDOWS, 1,
	    [Define to 1 if you want to use the X window system.])
  XMENU_OBJ=xmenu.o
  XOBJ="xterm.o xfns.o xselect.o xrdb.o xsmfns.o xsettings.o xgselect.o"
  FONT_OBJ=xfont.o
  if test "$HAVE_XFT" = "yes"; then
    FONT_OBJ="$FONT_OBJ ftfont.o xftfont.o ftxfont.o"
  elif test "$HAVE_FREETYPE" = "yes"; then
    FONT_OBJ="$FONT_OBJ ftfont.o ftxfont.o"
  fi
  AC_SUBST(FONT_OBJ)
fi
AC_SUBST(XMENU_OBJ)
AC_SUBST(XOBJ)
AC_SUBST(FONT_OBJ)

WIDGET_OBJ=
MOTIF_LIBW=
if test "${USE_X_TOOLKIT}" != "none" ; then
  WIDGET_OBJ=widget.o
  AC_DEFINE(USE_X_TOOLKIT, 1, [Define to 1 if using an X toolkit.])
  if test "${USE_X_TOOLKIT}" = "LUCID"; then
    AC_DEFINE(USE_LUCID, 1, [Define to 1 if using the Lucid X toolkit.])
  elif test "${USE_X_TOOLKIT}" = "MOTIF"; then
    AC_DEFINE(USE_MOTIF, 1, [Define to 1 if using the Motif X toolkit.])
    MOTIF_LIBW=-lXm
    case "$opsys" in
      gnu-linux)
        ## Paul Abrahams <abrahams at equinox.shaysnet.com> says this is needed.
        MOTIF_LIBW="$MOTIF_LIBW -lXpm"
        ;;

      unixware)
        ## Richard Anthony Ryan <ryanr at ellingtn.ftc.nrcs.usda.gov>
        ## says -lXimp is needed in UNIX_SV ... 4.2 1.1.2.
        MOTIF_LIBW="MOTIF_LIBW -lXimp"
        ;;

      aix4-2)
        ## olson@mcs.anl.gov says -li18n is needed by -lXm.
        MOTIF_LIBW="$MOTIF_LIBW -li18n"
        ;;
    esac
    MOTIF_LIBW="$MOTIF_LIBW $LIBXP"
  fi
fi
AC_SUBST(WIDGET_OBJ)

TOOLKIT_LIBW=
case "$USE_X_TOOLKIT" in
  MOTIF) TOOLKIT_LIBW="$MOTIF_LIBW" ;;
  LUCID) TOOLKIT_LIBW="$LUCID_LIBW" ;;
  none) test "x$HAVE_GTK" = "xyes" && TOOLKIT_LIBW="$GTK_LIBS" ;;
esac
AC_SUBST(TOOLKIT_LIBW)

if test "$USE_X_TOOLKIT" = "none"; then
  LIBXT_OTHER="\$(LIBXSM)"
  OLDXMENU_TARGET="really-oldXMenu"
else
  LIBXT_OTHER="\$(LIBXMU) -lXt \$(LIBXTR6) -lXext"
  OLDXMENU_TARGET="really-lwlib"
fi
AC_SUBST(LIBXT_OTHER)

## The X Menu stuff is present in the X10 distribution, but missing
## from X11.  If we have X10, just use the installed library;
## otherwise, use our own copy.
if test "${HAVE_X11}" = "yes" ; then
  AC_DEFINE(HAVE_X11, 1,
	    [Define to 1 if you want to use version 11 of X windows.
	     Otherwise, Emacs expects to use version 10.])

  if test "$USE_X_TOOLKIT" = "none"; then
    OLDXMENU="\${oldXMenudir}/libXMenu11.a"
  else
    OLDXMENU="\${lwlibdir}/liblw.a"
  fi
  LIBXMENU="\$(OLDXMENU)"
  LIBX_OTHER="\$(LIBXT) \$(LIBX_EXTRA)"
  OLDXMENU_DEPS="\${OLDXMENU} ../src/\${OLDXMENU}"
else
  ## For a syntactically valid Makefile; not actually used for anything.
  ## See comments in src/Makefile.in.
  OLDXMENU=nothing
  ## FIXME This case (!HAVE_X11 && HAVE_X_WINDOWS) is no longer possible(?).
  if test "${HAVE_X_WINDOWS}" = "yes"; then
    LIBXMENU="-lXMenu"
  else
    LIBXMENU=
  fi
  LIBX_OTHER=
  OLDXMENU_DEPS=
fi

if test "$HAVE_GTK" = "yes" || test "$HAVE_MENUS" != "yes"; then
  OLDXMENU_TARGET=
  OLDXMENU=nothing
  LIBXMENU=
  OLDXMENU_DEPS=
fi

AC_SUBST(OLDXMENU_TARGET)
AC_SUBST(OLDXMENU)
AC_SUBST(LIBXMENU)
AC_SUBST(LIBX_OTHER)
AC_SUBST(OLDXMENU_DEPS)

if test "${HAVE_MENUS}" = "yes" ; then
  AC_DEFINE(HAVE_MENUS, 1,
	    [Define to 1 if you have mouse menus.
	     (This is automatic if you use X, but the option to specify it remains.)
	     It is also defined with other window systems that support xmenu.c.])
fi

if test "${GNU_MALLOC}" = "yes" ; then
  AC_DEFINE(GNU_MALLOC, 1,
	    [Define to 1 if you want to use the GNU memory allocator.])
fi

RALLOC_OBJ=
if test "${REL_ALLOC}" = "yes" ; then
  AC_DEFINE(REL_ALLOC, 1,
	    [Define REL_ALLOC if you want to use the relocating allocator for
	     buffer space.])

  test "$system_malloc" != "yes" && RALLOC_OBJ=ralloc.o
fi
AC_SUBST(RALLOC_OBJ)

if test "$opsys" = "cygwin"; then
  CYGWIN_OBJ="sheap.o cygw32.o"
  ## Cygwin differs because of its unexec().
  PRE_ALLOC_OBJ=
  POST_ALLOC_OBJ=lastfile.o
else
  CYGWIN_OBJ=
  PRE_ALLOC_OBJ=lastfile.o
  POST_ALLOC_OBJ=
fi
AC_SUBST(CYGWIN_OBJ)
AC_SUBST(PRE_ALLOC_OBJ)
AC_SUBST(POST_ALLOC_OBJ)

# Configure gnulib.  Although this does not affect CFLAGS or LIBS permanently.
# it temporarily reverts them to their pre-pkg-config values,
# because gnulib needs to work with both src (which uses the
# pkg-config stuff) and lib-src (which does not).  For example, gnulib
# may need to determine whether LIB_CLOCK_GETTIME should contain -lrt,
# and it therefore needs to run in an environment where LIBS does not
# already contain -lrt merely because 'pkg-config --libs' printed '-lrt'
# for some package unrelated to lib-src.
SAVE_CFLAGS=$CFLAGS
SAVE_LIBS=$LIBS
CFLAGS=$pre_PKG_CONFIG_CFLAGS
LIBS="$LIB_PTHREAD $pre_PKG_CONFIG_LIBS"
gl_ASSERT_NO_GNULIB_POSIXCHECK
gl_ASSERT_NO_GNULIB_TESTS
gl_INIT
CFLAGS=$SAVE_CFLAGS
LIBS=$SAVE_LIBS

case "$opsys" in
  aix4-2) LD_SWITCH_SYSTEM_TEMACS="-Wl,-bnodelcsect" ;;

  darwin)
   ## The -headerpad option tells ld (see man page) to leave room at the
   ## end of the header for adding load commands.  Needed for dumping.
   ## 0x690 is the total size of 30 segment load commands (at 56
   ## each); under Cocoa 31 commands are required.
   if test "$HAVE_NS" = "yes"; then
     libs_nsgui="-framework AppKit"
     headerpad_extra=6C8
   else
     libs_nsgui=
     headerpad_extra=690
   fi
   LD_SWITCH_SYSTEM_TEMACS="-fno-pie -prebind $libs_nsgui -Xlinker -headerpad -Xlinker $headerpad_extra"

   ## This is here because src/Makefile.in did some extra fiddling around
   ## with LD_SWITCH_SYSTEM.  The cpp logic was:
   ##   #ifndef LD_SWITCH_SYSTEM
   ##   #if !defined (__GNUC__) && ((defined (BSD_SYSTEM) && !defined (COFF)))
   ## Since all the *bsds define LD_SWITCH_SYSTEM, this simplifies to:
   ## not using gcc, darwin.
   ## Because this was done in src/Makefile.in, the resulting part of
   ## LD_SWITCH_SYSTEM was not used in configure (ie, in ac_link).
   ## It therefore seems cleaner to put this in LD_SWITCH_SYSTEM_TEMACS,
   ## rather than LD_SWITCH_SYSTEM.
   test "x$LD_SWITCH_SYSTEM" = "x" && test "x$GCC" != "xyes" && \
     LD_SWITCH_SYSTEM_TEMACS="-X $LD_SWITCH_SYSTEM_TEMACS"
   ;;

  ## LD_SWITCH_X_SITE_RPATH is a -rpath option saying where to
  ## find X at run-time.
  ## When handled by cpp, this was in LD_SWITCH_SYSTEM.  However, at the
  ## point where configure sourced the s/*.h file, LD_SWITCH_X_SITE_RPATH
  ## had not yet been defined and was expanded to null.  Hence LD_SWITCH_SYSTEM
  ## had different values in configure (in ac_link) and src/Makefile.in.
  ## It seems clearer therefore to put this piece in LD_SWITCH_SYSTEM_TEMACS.
  gnu*) LD_SWITCH_SYSTEM_TEMACS="\$(LD_SWITCH_X_SITE_RPATH)" ;;

  *) LD_SWITCH_SYSTEM_TEMACS= ;;
esac

AC_SUBST(LD_SWITCH_SYSTEM_TEMACS)


LD_FIRSTFLAG=
ORDINARY_LINK=
case "$opsys" in
  ## gnu: GNU needs its own crt0.
  aix4-2|cygwin|darwin|gnu|hpux*|irix6-5|sol2*|unixware) ORDINARY_LINK=yes ;;

  ## On post 1.3 releases of NetBSD, gcc -nostdlib also clears the
  ## library search parth, i.e. it won't search /usr/lib for libc and
  ## friends.  Using -nostartfiles instead avoids this problem, and
  ## will also work on earlier NetBSD releases.
  netbsd|openbsd) LD_FIRSTFLAG="-nostartfiles" ;;

  ## powerpc*: NAKAJI Hiroyuki <nakaji@tutrp.tut.ac.jp> says
  ##   MkLinux/LinuxPPC needs this.
  ## s390x-* only supports opsys = gnu-linux so it can be added here.
  gnu-*)
    case "$canonical" in
      powerpc*|s390x-*) LD_FIRSTFLAG="-nostdlib" ;;
    esac
    ;;
esac


if test "x$ORDINARY_LINK" = "xyes"; then

  LD_FIRSTFLAG=""
  AC_DEFINE(ORDINARY_LINK, 1, [Define if the C compiler is the linker.])

## The system files defining neither ORDINARY_LINK nor LD_FIRSTFLAG are:
## freebsd, gnu-* not on powerpc*|s390x*.
elif test "x$GCC" = "xyes" && test "x$LD_FIRSTFLAG" = "x"; then

  ## Versions of GCC >= 2.0 put their library, libgcc.a, in obscure
  ## places that are difficult to figure out at make time.  Fortunately,
  ## these same versions allow you to pass arbitrary flags on to the
  ## linker, so there is no reason not to use it as a linker.
  ##
  ## Well, it is not quite perfect.  The "-nostdlib" keeps GCC from
  ## searching for libraries in its internal directories, so we have to
  ## ask GCC explicitly where to find libgcc.a (LIB_GCC below).
  LD_FIRSTFLAG="-nostdlib"
fi

## FIXME? What setting of EDIT_LDFLAGS should this have?
test "$NS_IMPL_GNUSTEP" = "yes" && LD_FIRSTFLAG="-rdynamic"

AC_SUBST(LD_FIRSTFLAG)


## FIXME? The logic here is not precisely the same as that above.
## There is no check here for a pre-defined LD_FIRSTFLAG.
## Should we only be setting LIB_GCC if LD ~ -nostdlib?
LIB_GCC=
if test "x$GCC" = "xyes" && test "x$ORDINARY_LINK" != "xyes"; then

  case "$opsys" in
    freebsd|netbsd|openbsd) LIB_GCC= ;;

    gnu-*)
      ## armin76@gentoo.org reported that the lgcc_s flag is necessary to
      ## build on ARM EABI under GNU/Linux.  (Bug#5518)
      case $host_cpu in
      arm*)
        LIB_GCC="-lgcc_s"
	;;
      *)
        ## FIXME? s/gnu-linux.h used to define LIB_GCC as below, then
        ## immediately undefine it again and redefine it to empty.
        ## Was the C_SWITCH_X_SITE part really necessary?
##      LIB_GCC=`$CC $C_SWITCH_X_SITE -print-libgcc-file-name`
        LIB_GCC=
	;;
      esac
      ;;

    ## Ask GCC where to find libgcc.a.
    *) LIB_GCC=`$CC -print-libgcc-file-name 2> /dev/null` ;;
  esac
fi                              dnl if $GCC
AC_SUBST(LIB_GCC)

## Common for all window systems
if test "$window_system" != "none"; then
  AC_DEFINE(HAVE_WINDOW_SYSTEM, 1, [Define if you have a window system.])
  AC_DEFINE(HAVE_MOUSE, 1, [Define if you have mouse support.])
  WINDOW_SYSTEM_OBJ="fontset.o fringe.o image.o"
fi

AC_SUBST(WINDOW_SYSTEM_OBJ)

AH_TOP([/* GNU Emacs site configuration template file.

Copyright (C) 1988, 1993-1994, 1999-2002, 2004-2013
  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


/* No code in Emacs #includes config.h twice, but some bits of code
   intended to work with other packages as well (like gmalloc.c)
   think they can include it as many times as they like.  */
#ifndef EMACS_CONFIG_H
#define EMACS_CONFIG_H
])dnl

AH_BOTTOM([#include <conf_post.h>

#endif /* EMACS_CONFIG_H */

/*
Local Variables:
mode: c
End:
*/
])dnl

#### Report on what we decided to do.
#### Report GTK as a toolkit, even if it doesn't use Xt.
#### It makes printing result more understandable as using GTK sets
#### toolkit_scroll_bars to yes by default.
if test "${HAVE_GTK}" = "yes"; then
  USE_X_TOOLKIT="$USE_GTK_TOOLKIT"
fi

echo "
Configured for \`${canonical}'.

  Where should the build process find the source code?    ${srcdir}
  What compiler should emacs be built with?               ${CC} ${CFLAGS}
  Should Emacs use the GNU version of malloc?             ${GNU_MALLOC}${GNU_MALLOC_reason}
  Should Emacs use a relocating allocator for buffers?    ${REL_ALLOC}
  Should Emacs use mmap(2) for buffer allocation?         $use_mmap_for_buffers
  What window system should Emacs use?                    ${window_system}
  What toolkit should Emacs use?                          ${USE_X_TOOLKIT}"

if test -n "${x_includes}"; then
echo "  Where do we find X Windows header files?                ${x_includes}"
else
echo "  Where do we find X Windows header files?                Standard dirs"
fi
if test -n "${x_libraries}"; then
echo "  Where do we find X Windows libraries?                   ${x_libraries}"
else
echo "  Where do we find X Windows libraries?                   Standard dirs"
fi

echo "  Does Emacs use -lXaw3d?                                 ${HAVE_XAW3D}"
echo "  Does Emacs use -lXpm?                                   ${HAVE_XPM}"
echo "  Does Emacs use -ljpeg?                                  ${HAVE_JPEG}"
echo "  Does Emacs use -ltiff?                                  ${HAVE_TIFF}"
echo "  Does Emacs use a gif library?                           ${HAVE_GIF} $LIBGIF"
echo "  Does Emacs use -lpng?                                   ${HAVE_PNG}"
echo "  Does Emacs use -lrsvg-2?                                ${HAVE_RSVG}"
echo "  Does Emacs use imagemagick?                             ${HAVE_IMAGEMAGICK}"

echo "  Does Emacs use -lgpm?                                   ${HAVE_GPM}"
echo "  Does Emacs use -ldbus?                                  ${HAVE_DBUS}"
echo "  Does Emacs use -lgconf?                                 ${HAVE_GCONF}"
echo "  Does Emacs use GSettings?                               ${HAVE_GSETTINGS}"
echo "  Does Emacs use -lselinux?                               ${HAVE_LIBSELINUX}"
echo "  Does Emacs use -lgnutls?                                ${HAVE_GNUTLS}"
echo "  Does Emacs use -lxml2?                                  ${HAVE_LIBXML2}"

echo "  Does Emacs use -lfreetype?                              ${HAVE_FREETYPE}"
echo "  Does Emacs use -lm17n-flt?                              ${HAVE_M17N_FLT}"
echo "  Does Emacs use -lotf?                                   ${HAVE_LIBOTF}"
echo "  Does Emacs use -lxft?                                   ${HAVE_XFT}"

echo "  Does Emacs use toolkit scroll bars?                     ${USE_TOOLKIT_SCROLL_BARS}"
echo

if test -n "${EMACSDATA}"; then
   echo "  Environment variable EMACSDATA set to:                  $EMACSDATA"
fi
if test -n "${EMACSDOC}"; then
   echo "  Environment variable EMACSDOC set to:                   $EMACSDOC"
fi

echo

if test "$HAVE_NS" = "yes"; then
   echo
   echo "You must run \"${MAKE-make} install\" in order to test the built application.
The installed application will go to nextstep/Emacs.app and can be
run or moved from there."
   if test "$EN_NS_SELF_CONTAINED" = "yes"; then
      echo "The application will be fully self-contained."
    else
      echo "The lisp resources for the application will be installed under ${prefix}.
You may need to run \"${MAKE-make} install\" with sudo.  The application will fail
to run if these resources are not installed."
   fi
   echo
fi

if test "${opsys}" = "cygwin"; then
  case `uname -r` in
    1.5.*) AC_MSG_WARN([[building Emacs on Cygwin 1.5 is not supported.]])
           echo
	   ;;
  esac
fi

# Remove any trailing slashes in these variables.
[test "${prefix}" != NONE &&
  prefix=`echo "${prefix}" | sed 's,\([^/]\)/*$,\1,'`
test "${exec_prefix}" != NONE &&
  exec_prefix=`echo "${exec_prefix}" | sed 's,\([^/]\)/*$,\1,'`]

if test "$HAVE_NS" = "yes"; then
  if test "$NS_IMPL_GNUSTEP" = yes; then
    AC_CONFIG_FILES([nextstep/GNUstep/Emacs.base/Resources/Info-gnustep.plist:nextstep/templates/Info-gnustep.plist.in \
      nextstep/GNUstep/Emacs.base/Resources/Emacs.desktop:nextstep/templates/Emacs.desktop.in])
  else
    AC_CONFIG_FILES([nextstep/Cocoa/Emacs.base/Contents/Info.plist:nextstep/templates/Info.plist.in \
      nextstep/Cocoa/Emacs.base/Contents/Resources/English.lproj/InfoPlist.strings:nextstep/templates/InfoPlist.strings.in])
  fi
fi

dnl Obviously there is duplication here wrt $SUBDIR_MAKEFILES.
dnl You _can_ use that variable in AC_CONFIG_FILES, so long as any directory
dnl using automake (ie lib/) is explicitly listed and not "hidden" in a variable
dnl (else you get "no `Makefile.am' found for any configure output").
dnl This will work, but you get a config.status that is not quite right
dnl (see http://lists.gnu.org/archive/html/bug-autoconf/2008-08/msg00028.html).
dnl That doesn't have any obvious consequences for Emacs, but on the whole
dnl it seems better to just live with the duplication.
SUBDIR_MAKEFILES="lib/Makefile lib-src/Makefile oldXMenu/Makefile doc/emacs/Makefile doc/misc/Makefile doc/lispintro/Makefile doc/lispref/Makefile src/Makefile lwlib/Makefile lisp/Makefile leim/Makefile nextstep/Makefile"

AC_CONFIG_FILES([Makefile lib/Makefile lib-src/Makefile oldXMenu/Makefile \
       doc/emacs/Makefile doc/misc/Makefile doc/lispintro/Makefile \
       doc/lispref/Makefile src/Makefile lwlib/Makefile lisp/Makefile \
       leim/Makefile nextstep/Makefile])

dnl test/ is not present in release tarfiles.
opt_makefile=test/automated/Makefile

if test -f $srcdir/${opt_makefile}.in; then
  SUBDIR_MAKEFILES="$SUBDIR_MAKEFILES $opt_makefile"
  dnl Again, it's best not to use a variable.  Though you can add
  dnl ", [], [opt_makefile='$opt_makefile']" and it should work.
  AC_CONFIG_FILES([test/automated/Makefile])
fi


dnl admin/ may or may not be present.
opt_makefile=admin/unidata/Makefile

if test -f $srcdir/${opt_makefile}.in; then
  SUBDIR_MAKEFILES="$SUBDIR_MAKEFILES $opt_makefile"
  AC_CONFIG_FILES([admin/unidata/Makefile])
fi


SUBDIR_MAKEFILES_IN=`echo " ${SUBDIR_MAKEFILES}" | sed -e 's| | $(srcdir)/|g' -e 's|Makefile|Makefile.in|g'`

AC_SUBST(SUBDIR_MAKEFILES_IN)

dnl Make the necessary directories, if they don't exist.
AC_CONFIG_COMMANDS([mkdirs], [
for dir in etc lisp ; do
  test -d ${dir} || mkdir ${dir}
done
])

dnl You might wonder (I did) why epaths.h is generated by running make,
dnl rather than just letting configure generate it from epaths.in.
dnl One reason is that the various paths are not fully expanded (see above);
dnl eg gamedir=${prefix}/var/games/emacs.
dnl Secondly, the GNU Coding standards require that one should be able
dnl to run `make prefix=/some/where/else' and override the values set
dnl by configure.  This also explains the `move-if-change' test and
dnl the use of force in the `epaths-force' rule in Makefile.in.
AC_CONFIG_COMMANDS([epaths], [
echo creating src/epaths.h
${MAKE-make} MAKEFILE_NAME=do-not-make-Makefile epaths-force
], [GCC="$GCC" CPPFLAGS="$CPPFLAGS"])

AC_CONFIG_COMMANDS([gdbinit], [
if test ! -f src/.gdbinit && test -f $srcdir/src/.gdbinit; then
  echo creating src/.gdbinit
  echo source $srcdir/src/.gdbinit > src/.gdbinit
fi
])

AC_OUTPUT
