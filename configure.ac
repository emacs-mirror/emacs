dnl  Autoconf script for GNU Emacs
dnl To rebuild the 'configure' script from this, execute the command
dnl	autoconf
dnl in the directory containing this script.
dnl If you changed any AC_DEFINES, also run autoheader.
dnl
dnl Copyright (C) 1994-1996, 1999-2020 Free Software Foundation, Inc.
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
dnl  along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

AC_PREREQ(2.65)
dnl Note this is parsed by (at least) make-dist and lisp/cedet/ede/emacs.el.
AC_INIT(GNU Emacs, 27.1, bug-gnu-emacs@gnu.org, , https://www.gnu.org/software/emacs/)

dnl Set emacs_config_options to the options of 'configure', quoted for the shell,
dnl and then quoted again for a C string.  Separate options with spaces.
dnl Add some environment variables, if they were passed via the environment
dnl rather than on the command-line.
emacs_config_options=
optsep=
dnl This is the documented way to record the args passed to configure,
dnl rather than $ac_configure_args.
for opt in "$@" CFLAGS CPPFLAGS LDFLAGS; do
  case $opt in
    -n | --no-create | --no-recursion)
      continue ;;
    CFLAGS | CPPFLAGS | LDFLAGS)
      eval 'test "${'$opt'+set}" = set' || continue
      case " $*" in
	*" $opt="*) continue ;;
      esac
      eval opt=$opt=\$$opt ;;
  esac

  emacs_shell_specials=$IFS\''"#$&()*;<>?@<:@\\`{|~'
  case $opt in
    *[["$emacs_shell_specials"]]*)
      case $opt in
	*\'*)
	  emacs_quote_apostrophes="s/'/'\\\\''/g"
	  opt=`AS_ECHO(["$opt"]) | sed "$emacs_quote_apostrophes"` ;;
      esac
      opt="'$opt'"
      case $opt in
	*[['"\\']]*)
	  emacs_quote_for_c='s/[["\\]]/\\&/g; $!s/$/\\n\\/'
	  opt=`AS_ECHO(["$opt"]) | sed "$emacs_quote_for_c"` ;;
      esac ;;
  esac
  AS_VAR_APPEND([emacs_config_options], ["$optsep$opt"])
  optsep=' '
done

AC_CONFIG_HEADERS(src/config.h:src/config.in)
AC_CONFIG_SRCDIR(src/lisp.h)
AC_CONFIG_AUX_DIR(build-aux)
AC_CONFIG_MACRO_DIR(m4)

xcsdkdir=
AC_CHECK_PROGS(XCRUN, [xcrun])
if test -n "$XCRUN"; then
  if test -z "$MAKE"; then
    dnl Call the variable MAKE_PROG, not MAKE, to avoid confusion with
    dnl the usual MAKE variable that 'make' itself uses.
    AC_CHECK_PROG([MAKE_PROG], [make], [yes])
    if test -z "$MAKE_PROG"; then
      MAKE="$XCRUN MAKE"
      export MAKE
      xcsdkdir=`$XCRUN --show-sdk-path 2>/dev/null`
    fi
  fi
fi

dnl Check for GNU Make and possibly set MAKE.
[emacs_check_gnu_make ()
{
  emacs_makeout=`($1 --version) 2>/dev/null` &&
  case $emacs_makeout in
    'GNU Make '3.8[1-9]* | 'GNU Make '3.9[0-9]* | \
    'GNU Make '3.[1-9][0-9][0-9]* | 'GNU Make '[4-9]* | 'GNU Make '[1-9][0-9]* )
       ac_path_MAKE_found=:;;
  esac
}]
AC_CACHE_CHECK([for GNU Make], [ac_cv_path_MAKE],
  [ac_path_MAKE_found=false
   if test -n "$MAKE"; then
     emacs_check_gnu_make "$MAKE"
     ac_cv_path_MAKE=$MAKE
   else
     emacs_tried_make=false
     emacs_tried_gmake=false
     emacs_tried_gnumake=false
     AC_PATH_PROGS_FEATURE_CHECK([MAKE], [make gmake gnumake],
       [[emacs_check_gnu_make "$ac_path_MAKE"
	 if $ac_path_MAKE_found; then
	   # Use the fully-qualified program name only if the basename
	   # would not resolve to it.
	   if eval \$emacs_tried_$ac_prog; then
	     ac_cv_path_MAKE=$ac_path_MAKE
	   else
	     ac_cv_path_MAKE=$ac_prog
	   fi
	 fi
	 eval emacs_tried_$ac_prog=:]])
   fi])
$ac_path_MAKE_found || {
AC_MSG_ERROR([[Building Emacs requires GNU Make, at least version 3.81.
If you have it installed under another name, configure with 'MAKE=...'.
For example, run '$0 MAKE=gnu-make'.]])
}
MAKE=$ac_cv_path_MAKE
export MAKE

dnl Canonicalize the configuration name.
AC_CANONICAL_HOST

case $host in
 *-mingw*)

  if test -z "$host_alias"; then

      # No --host argument was given to 'configure'; therefore $host
      # was set to a default value based on the build platform.  But
      # this default value may be wrong if we are building from a
      # 64-bit MSYS[2] pre-configured to build 32-bit MinGW programs.
      # Therefore, we'll try to get the right host platform from the
      # compiler's target.

      AC_MSG_CHECKING([the compiler's target])
      if test -z "$CC"; then
	  cc=gcc
      else
	  cc=$CC
      fi
      cc_target=`$cc -v 2>&1 | sed -n 's/Target: //p'`
      case "$cc_target" in
          *-*) host=$cc_target
	      ;;
          "") AC_MSG_ERROR([Impossible to obtain $cc compiler target.
Please explicitly provide --host.])
              ;;
	  *) AC_MSG_WARN([Compiler reported non-standard target.
Defaulting to $host.])
              ;;
      esac
      AC_MSG_RESULT([$host])
  fi

  . $srcdir/nt/mingw-cfg.site

  case $srcdir in
    /* | ?:*)
      # srcdir is an absolute path.  In this case, force the format
      # "/c/foo/bar", to simplify later conversions to native Windows
      # format ("c:/foo/bar").
      srcdir=`cd "${srcdir}" && pwd -W`
      # 'eval' pacifies strict POSIX non-MinGW shells (Bug#18612).
      # We downcase the drive letter to avoid warnings when
      # generating autoloads.
      eval 'srcdir=/`echo ${srcdir:0:1} | sed "y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/"`"${srcdir:2}"'
      ;;
  esac;;
esac

canonical=$host
configuration=${host_alias-${build_alias-$host}}
emacs_uname_r=`uname -r`

dnl Support for --program-prefix, --program-suffix and
dnl --program-transform-name options
AC_ARG_PROGRAM

dnl It is important that variables on the RHS not be expanded here,
dnl hence the single quotes.  This is per the GNU coding standards, see
dnl (autoconf) Installation Directory Variables
dnl See also epaths.h below.
lispdir='${datadir}/emacs/${version}/lisp'
standardlisppath='${lispdir}'
locallisppath='${datadir}/emacs/${version}/site-lisp:'\
'${datadir}/emacs/site-lisp'
lisppath='${locallisppath}:${standardlisppath}'
etcdir='${datadir}/emacs/${version}/etc'
archlibdir='${libexecdir}/emacs/${version}/${configuration}'
etcdocdir='${datadir}/emacs/${version}/etc'
gamedir='${localstatedir}/games/emacs'

dnl Special option to disable the most of other options.
AC_ARG_WITH(all,
[AS_HELP_STRING([--without-all],
		[omit almost all features and build
		small executable with minimal dependencies])],
  [with_features=$withval],
  [with_features=yes])

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
dnl Create a new --with option that defaults to $with_features.
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

# For retrieving mail, unencrypted network connections are the default
# only on native MS-Windows platforms.  (FIXME: These platforms should
# also be secure by default.)

AC_ARG_WITH([mailutils],
  [AS_HELP_STRING([--with-mailutils],
     [rely on GNU Mailutils, so that the --without-pop through --with-mailhost
      options are irrelevant; this is the default if GNU Mailutils is
      installed])],
  [],
  [with_mailutils=$with_features
   if test "$with_mailutils" = yes; then
     (movemail --version) >/dev/null 2>&1 || with_mailutils=no
   fi])
if test "$with_mailutils" = no; then
  with_mailutils=
fi
AC_SUBST([with_mailutils])

AC_ARG_WITH([pop],
  [AS_HELP_STRING([--with-pop],
     [Support POP mail retrieval if Emacs movemail is used (not recommended,
      as Emacs movemail POP is insecure).  This is the default only on
      native MS-Windows.])],
  [],
  [case $host in
     *-mingw*) with_pop=yes;;
     *) with_pop=no-by-default;;
   esac])
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

OPTION_DEFAULT_OFF([mail-unlink],[unlink, rather than empty, mail spool after reading])
if test "$with_mail_unlink" != no; then
   AC_DEFINE(MAIL_UNLINK_SPOOL, 1, [Define to unlink, rather than empty, mail spool after reading.])
fi

AC_ARG_WITH([mailhost],[AS_HELP_STRING([--with-mailhost=HOSTNAME],
    [string giving default POP mail host])],
    AC_DEFINE_UNQUOTED(MAILHOST, ["$withval"], [String giving fallback POP mail host.]))

AC_ARG_WITH([sound],[AS_HELP_STRING([--with-sound=VALUE],
  [compile with sound support (VALUE one of: yes, alsa, oss, bsd-ossaudio, no;
default yes).  Only for GNU/Linux, FreeBSD, NetBSD, MinGW, Cygwin.])],
  [ case "${withval}" in
      yes|no|alsa|oss|bsd-ossaudio) val=$withval ;;
      *) AC_MSG_ERROR(['--with-sound=$withval' is invalid;
this option's value should be 'yes', 'no', 'alsa', 'oss', or 'bsd-ossaudio'.])
      ;;
    esac
    with_sound=$val
  ],
  [with_sound=$with_features])

AC_ARG_WITH([pdumper],
  AS_HELP_STRING(
    [--with-pdumper=VALUE],
    [enable pdumper support unconditionally
      ('yes', 'no', or 'auto': default 'auto')]),
    [ case "${withval}" in
        yes|no|auto) val=$withval ;;
        *) AC_MSG_ERROR(
           ['--with-pdumper=$withval' is invalid;
this option's value should be 'yes' or 'no'.]) ;;
      esac
      with_pdumper=$val
    ],
    [with_pdumper=auto])

AC_ARG_WITH([unexec],
  AS_HELP_STRING(
    [--with-unexec=VALUE],
    [enable unexec support unconditionally
      ('yes', 'no', or 'auto': default 'auto')]),
    [ case "${withval}" in
        yes|no|auto) val=$withval ;;
        *) AC_MSG_ERROR(
           ['--with-unexec=$withval' is invalid;
this option's value should be 'yes' or 'no'.]) ;;
      esac
      with_unexec=$val
    ],
    [with_unexec=auto])

AC_ARG_WITH([dumping],[AS_HELP_STRING([--with-dumping=VALUE],
    [kind of dumping to use for initial Emacs build
(VALUE one of: pdumper, unexec, none; default pdumper)])],
    [ case "${withval}" in
        pdumper|unexec|none) val=$withval ;;
        *) AC_MSG_ERROR(['--with-dumping=$withval is invalid;
this option's value should be 'pdumper', 'unexec', or 'none'.])
        ;;
      esac
      with_dumping=$val
    ],
    [with_dumping=pdumper])

if test "$with_pdumper" = "auto"; then
  if test "$with_dumping" = "pdumper"; then
    with_pdumper=yes
  else
    with_pdumper=no
  fi
fi

if test "$with_unexec" = "auto"; then
  if test "$with_dumping" = "unexec"; then
    with_unexec=yes
  else
    with_unexec=no
  fi
fi

if test "$with_dumping" = "pdumper" && test "$with_pdumper" = "no"; then
  AC_MSG_ERROR(['--with-dumping=pdumper' requires pdumper support])
fi

if test "$with_dumping" = "unexec" && test "$with_unexec" = "no"; then
  AC_MSG_ERROR(['--with-dumping=unexec' requires unexec support])
fi

if test "$with_pdumper" = "yes"; then
  AC_DEFINE([HAVE_PDUMPER], 1, [Define to build with portable dumper support])
  HAVE_PDUMPER=yes
else
  HAVE_PDUMPER=no
fi
AC_SUBST([HAVE_PDUMPER])

DUMPING=$with_dumping
AC_SUBST(DUMPING)

dnl FIXME currently it is not the last.
dnl This should be the last --with option, because --with-x is
dnl added later on when we find the file name of X, and it's best to
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
AC_MSG_ERROR(['--with-x-toolkit=$withval' is invalid;
this option's value should be 'yes', 'no', 'lucid', 'athena', 'motif', 'gtk',
'gtk2' or 'gtk3'.  'yes' and 'gtk' are synonyms.
'athena' and 'lucid' are synonyms.])
	    ;;
	  esac
	  with_x_toolkit=$val
])

OPTION_DEFAULT_OFF([wide-int], [prefer wide Emacs integers (typically 62-bit); allows buffer and string size up to 2GB on 32-bit hosts, at the cost of 10% to 30% slowdown of Lisp interpreter and larger memory footprint])
if test "$with_wide_int" = yes; then
  AC_DEFINE([WIDE_EMACS_INT], 1, [Use long long for EMACS_INT if available.])
fi

dnl _ON results in a '--without' option in the --help output, so
dnl the help text should refer to "don't compile", etc.
with_xpm_set=${with_xpm+set}
OPTION_DEFAULT_ON([xpm],[don't compile with XPM image support])
OPTION_DEFAULT_ON([jpeg],[don't compile with JPEG image support])
OPTION_DEFAULT_ON([tiff],[don't compile with TIFF image support])
OPTION_DEFAULT_ON([gif],[don't compile with GIF image support])
OPTION_DEFAULT_ON([png],[don't compile with PNG image support])
OPTION_DEFAULT_ON([rsvg],[don't compile with SVG image support])
OPTION_DEFAULT_ON([lcms2],[don't compile with Little CMS support])
OPTION_DEFAULT_ON([libsystemd],[don't compile with libsystemd support])
OPTION_DEFAULT_OFF([cairo],[compile with Cairo drawing])
OPTION_DEFAULT_ON([xml2],[don't compile with XML parsing support])
OPTION_DEFAULT_OFF([imagemagick],[compile with ImageMagick image support])
OPTION_DEFAULT_ON([json], [don't compile with native JSON support])

OPTION_DEFAULT_ON([xft],[don't use XFT for anti aliased fonts])
OPTION_DEFAULT_ON([harfbuzz],[don't use HarfBuzz for text shaping])
OPTION_DEFAULT_ON([libotf],[don't use libotf for OpenType font support])
OPTION_DEFAULT_ON([m17n-flt],[don't use m17n-flt for text shaping])

OPTION_DEFAULT_ON([toolkit-scroll-bars],[don't use Motif/Xaw3d/GTK toolkit scroll bars])
OPTION_DEFAULT_ON([xaw3d],[don't use Xaw3d])
OPTION_DEFAULT_ON([xim],[at runtime, default X11 XIM to off])
AC_ARG_WITH([ns],[AS_HELP_STRING([--with-ns],
[use Nextstep (macOS Cocoa or GNUstep) windowing system.
On by default on macOS.])],[],[with_ns=maybe])
OPTION_DEFAULT_OFF([w32], [use native MS Windows GUI in a Cygwin build])

OPTION_DEFAULT_ON([gpm],[don't use -lgpm for mouse support on a GNU/Linux console])
OPTION_DEFAULT_ON([dbus],[don't compile with D-Bus support])
AC_ARG_WITH([gconf],[AS_HELP_STRING([--with-gconf],
[compile with Gconf support (Gsettings replaces this)])],[],
[if test $with_features = yes; then
with_gconf=maybe
else
with_gconf=no
fi])
OPTION_DEFAULT_ON([gsettings],[don't compile with GSettings support])
OPTION_DEFAULT_ON([selinux],[don't compile with SELinux support])
OPTION_DEFAULT_ON([gnutls],[don't use -lgnutls for SSL/TLS support])
OPTION_DEFAULT_ON([zlib],[don't compile with zlib decompression support])
OPTION_DEFAULT_ON([modules],[don't compile with dynamic modules support])
OPTION_DEFAULT_ON([threads],[don't compile with elisp threading support])

AC_ARG_WITH([file-notification],[AS_HELP_STRING([--with-file-notification=LIB],
 [use a file notification library (LIB one of: yes, inotify, kqueue, gfile, w32, no)])],
 [ case "${withval}" in
    y | ye | yes )	val=yes ;;
    n | no )		val=no  ;;
    i | in | ino | inot | inoti | inotif | inotify )	val=inotify ;;
    k | kq | kqu | kque | kqueu | kqueue )	val=kqueue ;;
    g | gf | gfi | gfil | gfile )	val=gfile ;;
    w | w3 | w32 )	val=w32 ;;
    * ) AC_MSG_ERROR(['--with-file-notification=$withval' is invalid;
this option's value should be 'yes', 'no', 'inotify', 'kqueue', 'gfile' or 'w32'.
'yes' is a synonym for 'w32' on MS-Windows, for 'no' on Nextstep,
otherwise for the first of 'inotify', 'kqueue' or 'gfile' that is usable.])
    ;;
   esac
   with_file_notification=$val
 ],
 [with_file_notification=$with_features])

OPTION_DEFAULT_OFF([xwidgets],
  [enable use of some gtk widgets in Emacs buffers (requires gtk3)])

## For the times when you want to build Emacs but don't have
## a suitable makeinfo, and can live without the manuals.
dnl https://lists.gnu.org/r/emacs-devel/2008-04/msg01844.html
OPTION_DEFAULT_ON([makeinfo],[don't require makeinfo for building manuals])

## Makefile.in needs the cache file name.
AC_SUBST(cache_file)

## This is an option because I do not know if all info/man support
## compressed files, nor how to test if they do so.
OPTION_DEFAULT_ON([compress-install],
  [don't compress some files (.el, .info, etc.) when installing.  Equivalent to:
make GZIP_PROG= install])

AC_ARG_WITH(gameuser,dnl
[AS_HELP_STRING([--with-gameuser=USER_OR_GROUP],
		[user for shared game score files.
		An argument prefixed by ':' specifies a group instead.])])
gameuser=
gamegroup=
case ${with_gameuser} in
  '' | no) ;;
  yes) gamegroup=games ;;
  :*) gamegroup=${with_gameuser#:} ;;
  *) gameuser=${with_gameuser} ;;
esac

AC_ARG_WITH([gnustep-conf],dnl
[AS_HELP_STRING([--with-gnustep-conf=FILENAME],
   [name of GNUstep configuration file to use on systems where the command
    'gnustep-config' does not work; default $GNUSTEP_CONFIG_FILE, or
    /etc/GNUstep/GNUstep.conf])])
test "X${with_gnustep_conf}" != X && test "${with_gnustep_conf}" != yes && \
  GNUSTEP_CONFIG_FILE="${with_gnustep_conf}"
test "X$GNUSTEP_CONFIG_FILE" = "X" && \
     GNUSTEP_CONFIG_FILE=/etc/GNUstep/GNUstep.conf

AC_ARG_ENABLE(ns-self-contained,
[AS_HELP_STRING([--disable-ns-self-contained],
                [disable self contained build under NeXTstep])],
   EN_NS_SELF_CONTAINED=$enableval,
   EN_NS_SELF_CONTAINED=yes)

locallisppathset=no
AC_ARG_ENABLE(locallisppath,
[AS_HELP_STRING([--enable-locallisppath=PATH],
                [directories Emacs should search for lisp files specific
		 to this site])],
if test "${enableval}" = "no"; then
  locallisppath=
elif test "${enableval}" != "yes"; then
  locallisppath=${enableval} locallisppathset=yes
fi)

AC_ARG_ENABLE(checking,
[AS_HELP_STRING([--enable-checking@<:@=LIST@:>@],
		[enable expensive checks.  With LIST,
		 enable only specific categories of checks.
		 Categories are: all,yes,no.
		 Flags are: stringbytes, stringoverrun, stringfreelist,
		 structs, glyphs])],
[ac_checking_flags="${enableval}"],[])
IFS="${IFS= 	}"; ac_save_IFS="$IFS"; IFS="$IFS,"
CHECK_STRUCTS=false
for check in $ac_checking_flags
do
	case $check in
	# these set all the flags to specific states
	yes)		ac_enable_checking=1 ;;
	no)		ac_enable_checking= ;
			CHECK_STRUCTS=false
			ac_gc_check_stringbytes= ;
	                ac_gc_check_string_overrun= ;
	                ac_gc_check_string_free_list= ;
			ac_glyphs_debug= ;;
	all)		ac_enable_checking=1 ;
			CHECK_STRUCTS=true
			ac_gc_check_stringbytes=1 ;
	                ac_gc_check_string_overrun=1 ;
	                ac_gc_check_string_free_list=1 ;
			ac_glyphs_debug=1 ;;
	# these enable particular checks
	stringbytes)	ac_gc_check_stringbytes=1 ;;
	stringoverrun)	ac_gc_check_string_overrun=1 ;;
	stringfreelist) ac_gc_check_string_free_list=1 ;;
	structs)	CHECK_STRUCTS=true ;;
	glyphs)		ac_glyphs_debug=1 ;;
	*)	AC_MSG_ERROR(unknown check category $check) ;;
	esac
done
IFS="$ac_save_IFS"

if test x$ac_enable_checking != x ; then
  AC_DEFINE(ENABLE_CHECKING, 1,
[Define to 1 if expensive run-time data type and consistency checks are enabled.])
fi
if $CHECK_STRUCTS; then
  AC_DEFINE([CHECK_STRUCTS], 1,
    [Define this to check whether someone updated the portable dumper
     code after changing the layout of a structure that it uses.
     If you change one of these structures, check that the pdumper.c
     code is still valid, and update the pertinent hash in pdumper.c
     by manually copying the hash from the newly-generated dmpstruct.h.])
fi
AC_SUBST([CHECK_STRUCTS])
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
if test x$ac_glyphs_debug != x ; then
  AC_DEFINE(GLYPH_DEBUG, 1,
[Define this to enable glyphs debugging code.])
fi

dnl The name of this option is unfortunate.  It predates, and has no
dnl relation to, the "sampling-based elisp profiler" added in 24.3.
dnl Actually, it stops it working.
dnl https://lists.gnu.org/r/emacs-devel/2012-11/msg00393.html
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
		 Requires gcc, enabled if found.])],
[ac_enable_autodepend="${enableval}"],[ac_enable_autodepend=yes])

AC_ARG_ENABLE(gtk-deprecation-warnings,
[AS_HELP_STRING([--enable-gtk-deprecation-warnings],
		[Show Gtk+/Gdk deprecation warnings for Gtk+ >= 3.0])],
[ac_enable_gtk_deprecation_warnings="${enableval}"],[])

BUILD_DETAILS=
AC_ARG_ENABLE([build-details],
  [AS_HELP_STRING([--disable-build-details],
		  [Make the build more deterministic by omitting host
		   names, time stamps, etc. from the output.])],
  [test "$enableval" = no && BUILD_DETAILS=--no-build-details])
AC_SUBST([BUILD_DETAILS])

dnl This used to use changequote, but, apart from 'changequote is evil'
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
### the appropriate opsys.

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

  ## DragonFly ports
  *-*-dragonfly* )
    opsys=dragonfly
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
  *-*-openbsd* | *-*-mirbsd* )
    opsys=openbsd
  ;;

  ## Apple Darwin / macOS
  *-apple-darwin* )
    case "${canonical}" in
      *-apple-darwin[0-9].*) unported=yes ;;
      i[3456]86-* | x86_64-* )  ;;
      * )            unported=yes ;;
    esac
    opsys=darwin
    ## FIXME: Find a way to use Fink if available (Bug#11507).
  ;;

  ## Chromium Native Client
  *-nacl )
    opsys=nacl
  ;;

  ## Cygwin ports
  *-*-cygwin )
    opsys=cygwin
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
  powerpc-ibm-aix[5-9]* | powerpc-ibm-aix[1-9][0-9]* )
    opsys=aix4-2
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
      *-sunos5.[1-9][0-9]* | *-solaris2.[1-9][0-9]* )
		opsys=sol2-10
		emacs_check_sunpro_c=yes
		;;
      *-sunos5.[1-5]* | *-solaris2.[1-5]* ) unported=yes ;;
      ## Note that Emacs 23.1's NEWS said the following would be dropped.
      *-sunos5.6* | *-solaris2.6* )
		opsys=sol2-6
		RANLIB="ar -ts"
		;;
      ## 5.7 EOL Aug 2008, 5.8 EOL Mar 2012.
      *-sunos5.[7-9]* | *-solaris2.[7-9]* )
		opsys=sol2-6
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

  ## QNX Neutrino
  *-nto-qnx* )
    opsys=qnxnto
    test -z "$CC" && CC=qcc
    CFLAGS="$CFLAGS -D__NO_EXT_QNX"
    if test "$with_unexec" = yes; then
      LDFLAGS="-N2MB $LDFLAGS"
    fi
  ;;

  ## Intel 386 machines where we don't care about the manufacturer.
  i[3456]86-*-* )
    case "${canonical}" in
      *-darwin* )               opsys=darwin ;;
      *-mingw* )
		opsys=mingw32
		# MinGW overrides and adds some system headers in nt/inc.
		GCC_TEST_OPTIONS="-I $srcdir/nt/inc"
		;;
      *-sysv4.2uw* )		opsys=unixware ;;
      *-sysv5uw* )		opsys=unixware ;;
      *-sysv5OpenUNIX* )	opsys=unixware ;;
      ## Otherwise, we'll fall through to the generic opsys code at the bottom.
    esac
  ;;

  # MinGW64
  x86_64-*-* )
    case "${canonical}" in
      *-mingw* )
		opsys=mingw32
		# MinGW overrides and adds some system headers in nt/inc.
		GCC_TEST_OPTIONS="-I $srcdir/nt/inc"
		;;
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
  AC_MSG_ERROR([Emacs does not support '${canonical}' systems.
If you think it should, please send a report to ${PACKAGE_BUGREPORT}.
Check 'etc/MACHINES' for recognized configuration names.])
fi

#### Choose a compiler.

dnl Don't bother to test for C89.
AC_DEFUN([_AC_PROG_CC_C89], [$2])

dnl Sets GCC=yes if using gcc.
AC_PROG_CC([gcc cc cl clang "$XCRUN gcc" "$XCRUN clang"])
if test -n "$XCRUN"; then
  AC_CHECK_PROGS(AR, [ar "$XCRUN ar"])
  test -n "$AR" && export AR
fi

dnl Emacs needs C99 or later.
gl_PROG_CC_C99

AC_PROG_CC_C_O

if test x$GCC = xyes; then
  test "x$GCC_TEST_OPTIONS" != x && CC="$CC $GCC_TEST_OPTIONS"
fi

# Avoid gnulib's tests for -lcrypto, so that there's no static dependency on it.
AC_DEFUN([gl_CRYPTO_CHECK])
# Avoid gnulib's tests for HAVE_WORKING_O_NOATIME and HAVE_WORKING_O_NOFOLLOW,
# as we don't use them.
AC_DEFUN([gl_FCNTL_O_FLAGS])
# Avoid gnulib's test for pthread_sigmask.
funcs=
for func in $ac_func_list; do
  test $func = pthread_sigmask || AS_VAR_APPEND([funcs], [" $func"])
done
ac_func_list=$funcs
# Use the system putenv even if it lacks GNU features, as we don't need them,
# and the gnulib replacement runs afoul of a FreeBSD 10.1 bug; see Bug#19874.
AC_CHECK_FUNCS_ONCE([putenv])
AC_DEFUN([gl_FUNC_PUTENV],
  [test "$ac_cv_func_putenv" = yes || REPLACE_PUTENV=1])
# Emacs does not use the wchar or wctype-h modules.
AC_DEFUN([gt_TYPE_WINT_T],
  [GNULIB_OVERRIDES_WINT_T=0
   AC_SUBST([GNULIB_OVERRIDES_WINT_T])])

# Initialize gnulib right after choosing the compiler.
dnl Amongst other things, this sets AR and ARFLAGS.
gl_EARLY

if test "$ac_test_CFLAGS" != set; then
  # It's helpful to have C macros available to GDB, so prefer -g3 to -g
  # if -g3 works and the user does not specify CFLAGS.
  # This test must follow gl_EARLY; otherwise AC_LINK_IFELSE complains.
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
    if test $emacs_cv_prog_cc_g3 != yes; then
      CFLAGS=$emacs_save_CFLAGS
    fi
    if test $opsys = mingw32; then
      CFLAGS="$CFLAGS -gdwarf-2"
    fi
  fi

  case $CFLAGS in
    *-O*) ;;
    *)
      # No optimization flag was inferred for this non-GCC compiler.
      # Try -O.  This is needed for xlc on AIX; see Bug#14258.
      emacs_save_CFLAGS=$CFLAGS
      test -z "$CFLAGS" || CFLAGS="$CFLAGS "
      CFLAGS=${CFLAGS}-O
      AC_CACHE_CHECK([whether $CC accepts -O],
        [emacs_cv_prog_cc_o],
	[AC_LINK_IFELSE([AC_LANG_PROGRAM()],
	   [emacs_cv_prog_cc_o=yes],
	   [emacs_cv_prog_cc_o=no])])
      if test $emacs_cv_prog_cc_o != yes; then
	CFLAGS=$emacs_save_CFLAGS
      fi ;;
  esac
fi

# gl_GCC_VERSION_IFELSE([major], [minor], [run-if-found], [run-if-not-found])
# ---------------------------------------------------------------------------
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

AC_ARG_ENABLE([gcc-warnings],
  [AS_HELP_STRING([--enable-gcc-warnings@<:@=TYPE@:>@],
                  [control generation of GCC warnings.  The TYPE 'yes'
		   means to fail if any warnings are issued; 'warn-only'
		   means issue warnings without failing (default for
		   developer builds); 'no' means disable warnings
		   (default for non-developer builds).])],
  [case $enableval in
     yes|no|warn-only) ;;
     *)      AC_MSG_ERROR([bad value $enableval for gcc-warnings option]) ;;
   esac
   gl_gcc_warnings=$enableval],
  [# By default, use 'warn-only' if it looks like the invoker of 'configure'
   # is a developer as opposed to a builder.  This is most likely true
   # if GCC is recent enough and there is a .git directory or file;
   # however, if there is also a .tarball-version file it is probably
   # just a release imported into Git for patch management.
   gl_gcc_warnings=no
   if test -e "$srcdir"/.git && test ! -f "$srcdir"/.tarball-version; then
     gl_GCC_VERSION_IFELSE([5], [3], [gl_gcc_warnings=warn-only])
   fi])

AC_ARG_ENABLE([check-lisp-object-type],
  [AS_HELP_STRING([--enable-check-lisp-object-type],
     [Enable compile time checks for the Lisp_Object data type,
      which can catch some bugs during development.])])
if test "$enable_check_lisp_object_type" = yes; then
  AC_DEFINE([CHECK_LISP_OBJECT_TYPE], 1,
    [Define to enable compile-time checks for the Lisp_Object data type.])
fi

# clang is unduly picky about some things.
AC_CACHE_CHECK([whether the compiler is clang], [emacs_cv_clang],
  [AC_COMPILE_IFELSE(
     [AC_LANG_PROGRAM([[
	  #ifndef __clang__
	    error "not clang";
	  #endif
        ]])],
     [emacs_cv_clang=yes],
     [emacs_cv_clang=no])])

WERROR_CFLAGS=
# When compiling with GCC, prefer -isystem to -I when including system
# include files, to avoid generating useless diagnostics for the files.
AS_IF([test $gl_gcc_warnings = no],
 [
  isystem='-I'
  AS_IF([test "$emacs_cv_clang" = yes],
   [
     # Turn off some warnings if supported.
     gl_WARN_ADD([-Wno-switch])
     gl_WARN_ADD([-Wno-pointer-sign])
     gl_WARN_ADD([-Wno-string-plus-int])
     gl_WARN_ADD([-Wno-unknown-attributes])
   ])
 ],[
  isystem='-isystem '

  # This, $nw, is the list of warnings we disable.
  nw=

  case $with_x_toolkit in
    lucid | athena | motif)
       # Old toolkits mishandle 'const'.
       nw="$nw -Wwrite-strings"
       ;;
  esac
  AS_IF([test $gl_gcc_warnings = yes],
    [WERROR_CFLAGS=-Werror])

  nw="$nw -Wcast-align -Wcast-align=strict" # Emacs is tricky with pointers.
  nw="$nw -Wduplicated-branches"    # Too many false alarms
  nw="$nw -Wformat-overflow=2"      # False alarms due to GCC bug 80776
  nw="$nw -Wsystem-headers"         # Don't let system headers trigger warnings
  nw="$nw -Woverlength-strings"     # Not a problem these days
  nw="$nw -Wformat-nonliteral"      # we do this a lot
  nw="$nw -Wvla"                    # Emacs uses <vla.h>.
  nw="$nw -Wunused-const-variable=2" # lisp.h declares const objects.
  nw="$nw -Winline"                 # OK to ignore 'inline'
  nw="$nw -Wstrict-overflow"        # OK to optimize assuming that
                                    # signed overflow has undefined behavior
  nw="$nw -Wsync-nand"              # irrelevant here, and provokes ObjC warning
  nw="$nw -Wunsafe-loop-optimizations" # OK to suppress unsafe optimizations
  nw="$nw -Wbad-function-cast"      # These casts are no worse than others.
  nw="$nw -Wabi"                    # Not useful, perceived as noise

  # Emacs doesn't care about shadowing; see
  # <https://lists.gnu.org/r/emacs-diffs/2011-11/msg00265.html>.
  nw="$nw -Wshadow"

  # Emacs's use of alloca inhibits protecting the stack.
  nw="$nw -Wstack-protector"

  # Emacs's use of __attribute__ ((cold)) causes false alarms with this option.
  nw="$nw -Wsuggest-attribute=cold"

  # Emacs's use of partly-const functions such as Fgnutls_available_p
  # make this option problematic.
  nw="$nw -Wsuggest-attribute=const"

  # Emacs's use of partly-pure functions such as CHECK_TYPE make this
  # option problematic.
  nw="$nw -Wsuggest-attribute=pure"

  # This part is merely for shortening the command line,
  # since -Wall implies -Wswitch.
  nw="$nw -Wswitch"

  # This part is merely for shortening the command line,
  # since -Wno-FOO needs to be added below regardless.
  nw="$nw -Wmissing-field-initializers"
  nw="$nw -Woverride-init"
  nw="$nw -Wtype-limits"
  nw="$nw -Wunused-parameter"

  if test "$emacs_cv_clang" = yes; then
    nw="$nw -Wcast-align"
    nw="$nw -Wdouble-promotion"
    nw="$nw -Wmissing-braces"
  fi

  # These cause too much noise in the MinGW build
  if test $opsys = mingw32; then
    nw="$nw -Wpointer-sign"
    nw="$nw -Wsuggest-attribute=format"
  fi

  gl_MANYWARN_ALL_GCC([ws])
  gl_MANYWARN_COMPLEMENT([ws], [$ws], [$nw])
  for w in $ws; do
    gl_WARN_ADD([$w])
  done
  gl_WARN_ADD([-Wredundant-decls])     # Prefer this, as we don't use Bison.
  gl_WARN_ADD([-Wno-missing-field-initializers]) # We need this one
  gl_WARN_ADD([-Wno-override-init])    # More trouble than it is worth
  gl_WARN_ADD([-Wno-sign-compare])     # Too many warnings for now
  gl_WARN_ADD([-Wno-type-limits])      # Too many warnings for now
  gl_WARN_ADD([-Wno-unused-parameter]) # Too many warnings for now
  gl_WARN_ADD([-Wno-format-nonliteral])

  # clang is unduly picky about some things.
  if test "$emacs_cv_clang" = yes; then
    gl_WARN_ADD([-Wno-missing-braces])
    gl_WARN_ADD([-Wno-null-pointer-arithmetic])
  fi

  # This causes too much noise in the MinGW build
  if test $opsys = mingw32; then
    gl_WARN_ADD([-Wno-pointer-sign])
  fi

  AC_DEFINE([GCC_LINT], [1], [Define to 1 if --enable-gcc-warnings.])
  AS_IF([test $gl_gcc_warnings = yes],
    [AC_DEFINE([GNULIB_PORTCHECK], [1], [enable some gnulib portability checks])
     AH_VERBATIM([GNULIB_PORTCHECK_FORTIFY_SOURCE],
     [/* Enable compile-time and run-time bounds-checking, and some warnings,
	 without upsetting glibc 2.15+. */
      #if (defined GNULIB_PORTCHECK && !defined _FORTIFY_SOURCE \
	   && defined __OPTIMIZE__ && __OPTIMIZE__)
      # define _FORTIFY_SOURCE 2
      #endif
     ])])
 ])

# clang is picky about these regardless of whether
# --enable-gcc-warnings is specified.
if test "$emacs_cv_clang" = yes; then
  gl_WARN_ADD([-Wno-initializer-overrides])
  gl_WARN_ADD([-Wno-tautological-compare])
  gl_WARN_ADD([-Wno-tautological-constant-out-of-range-compare])
fi

# Use a slightly smaller set of warning options for lib/.
nw=
nw="$nw -Wunused-macros"
gl_MANYWARN_COMPLEMENT([GNULIB_WARN_CFLAGS], [$WARN_CFLAGS], [$nw])

AC_SUBST([WERROR_CFLAGS])
AC_SUBST([GNULIB_WARN_CFLAGS])

edit_cflags="
  s,///*,/,g
  s/^/ /
  s/ -I/ $isystem/g
  s/^ //
"

AC_ARG_ENABLE(link-time-optimization,
[AS_HELP_STRING([--enable-link-time-optimization],
                [build with link-time optimization
		 (experimental; see INSTALL)])],
if test "${enableval}" != "no"; then
   ac_lto_supported=no
   if test "$emacs_cv_clang" = yes; then
      AC_MSG_CHECKING([whether link-time optimization is supported by clang])
      GOLD_PLUGIN=`$CC -print-file-name=LLVMgold.so 2>/dev/null`
      if test -x "$GOLD_PLUGIN"; then
	 LTO="-flto"
      fi
   elif test x$GCC = xyes; then
      AC_MSG_CHECKING([whether link-time optimization is supported by gcc])
      CPUS=`getconf _NPROCESSORS_ONLN 2>/dev/null`
      if test x$CPUS != x; then
	 LTO="-flto=$CPUS"
      else
	 LTO="-flto"
      fi
   else
      AC_MSG_ERROR([Link-time optimization is not supported with your compiler.])
   fi
   if test -z "$LTO"; then
      ac_lto_supported=no
   else
      old_CFLAGS=$CFLAGS
      CFLAGS="$CFLAGS $LTO"
      AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[]])],
         [ac_lto_supported=yes], [ac_lto_supported=no])
         CFLAGS="$old_CFLAGS"
   fi
   AC_MSG_RESULT([$ac_lto_supported])
   if test "$ac_lto_supported" = "yes"; then
      CFLAGS="$CFLAGS $LTO"
      if test "$emacs_cv_clang" = yes; then
	 AC_MSG_WARN([Please read INSTALL before using link-time optimization with clang])
	 # WARNING: 'ar --plugin ...' doesn't work without
	 # command, so plugin name is appended to ARFLAGS.
	 ARFLAGS="cru --plugin $GOLD_PLUGIN"
	 RANLIB="$RANLIB --plugin $GOLD_PLUGIN"
      else
        dnl The following is needed for GCC 4.9.0.  The GCC 4.9.0 release notes
        dnl suggest that instead of -ffat-lto-objects we should use gcc-ar and
        dnl gcc-ranlib in place of ar and ranlib, but gcc-ar makes /usr/bin/ar
        dnl dump core on Fedora 20, so play it safe for now.
        gl_COMPILER_OPTION_IF([-ffat-lto-objects],
          [CFLAGS="$CFLAGS -ffat-lto-objects"])
      fi
   fi
fi)


dnl Automake replacements.
AC_DEFUN([AM_CONDITIONAL],
  [$2 && $1=1 || $1=
   AC_SUBST([$1])])

dnl Prefer silent make output.  For verbose output, use
dnl 'configure --disable-silent-rules' or 'make V=1' .
dnl This code is adapted from Automake.
dnl Although it can be simplified now that GNU Make is assumed,
dnl the simplification hasn't been done yet.
AC_ARG_ENABLE([silent-rules],
  [AS_HELP_STRING(
     [--disable-silent-rules],
     [verbose build output (undo: "make V=0")])])
if test "$enable_silent_rules" = no; then
  AM_DEFAULT_VERBOSITY=1
else
  AM_DEFAULT_VERBOSITY=0
fi
AM_V='$(V)'
AM_DEFAULT_V='$(AM_DEFAULT_VERBOSITY)'
AC_SUBST([AM_V])
AC_SUBST([AM_DEFAULT_V])
AC_SUBST([AM_DEFAULT_VERBOSITY])

dnl Some other nice autoconf tests.
AC_PROG_INSTALL
dnl These are commented out, since gl_EARLY and/or Autoconf already does them.
dnl AC_PROG_MKDIR_P
dnl if test "x$RANLIB" = x; then
dnl   AC_PROG_RANLIB
dnl fi


dnl Sadly, AC_PROG_LN_S is too restrictive.  It also tests whether links
dnl can be made to directories.  This is not relevant for our usage, and
dnl excludes some cases that work fine for us.  Eg MS Windows or files
dnl hosted on AFS, both examples where simple links work, but links to
dnl directories fail.  We use a cut-down version instead.
dnl AC_PROG_LN_S

AC_CACHE_CHECK([command to symlink files in the same directory], [emacs_cv_ln_s_fileonly],
[rm -f conf$$ conf$$.file

emacs_cv_ln_s_fileonly='cp -p'

dnl On MinGW, ensure we will call the MSYS /bin/ln.exe, not some
dnl random program in the current directory.
if (echo >conf$$.file) 2>/dev/null; then
  if ln -s conf$$.file conf$$ 2>/dev/null; then
    if test "$opsys" = "mingw32"; then
      emacs_cv_ln_s_fileonly='/bin/ln -s'
    else
      emacs_cv_ln_s_fileonly='ln -s'
    fi
  elif ln conf$$.file conf$$ 2>/dev/null; then
    if test "$opsys" = "mingw32"; then
      emacs_cv_ln_s_fileonly=/bin/ln
    else
      emacs_cv_ln_s_fileonly=ln
    fi
  fi
fi

rm -f conf$$ conf$$.file])
LN_S_FILEONLY=$emacs_cv_ln_s_fileonly

AC_SUBST(LN_S_FILEONLY)


dnl AC_PROG_LN_S sets LN_S to 'cp -pR' for MinGW, on the premise that 'ln'
dnl doesn't support links to directories, as in "ln file dir".  But that
dnl use is non-portable, and OTOH MinGW wants to use hard links for Emacs
dnl executables at "make install" time.
dnl See https://lists.gnu.org/r/emacs-devel/2013-04/msg00475.html
dnl for more details.
if test "$opsys" = "mingw32"; then
  LN_S="/bin/ln"
fi

dnl On some Debian versions, "install-info" prints irritating messages
dnl "This is not dpkg install-info anymore, but GNU install-info"
dnl if called via an absolute file name.
dnl Use the entirely-identical-but-quieter ginstall-info instead if present.
dnl Sadly some people may have an old ginstall-info installed on
dnl non-Debian systems, so we can't use this.
dnl AC_PATH_PROGS(INSTALL_INFO, [ginstall-info install-info], :,
dnl   $PATH$PATH_SEPARATOR/usr/sbin$PATH_SEPARATOR/sbin)

AC_PATH_PROG(INSTALL_INFO, install-info, :,
  $PATH$PATH_SEPARATOR/usr/sbin$PATH_SEPARATOR/sbin)
dnl Don't use GZIP, which is used by gzip for additional parameters.
AC_PATH_PROG(GZIP_PROG, gzip)

test $with_compress_install != yes && test -n "$GZIP_PROG" && \
   GZIP_PROG=" # $GZIP_PROG # (disabled by configure --without-compress-install)"

if test "$with_dumping" = "unexec" && test "$opsys" = "nacl"; then
  AC_MSG_ERROR([nacl is not compatible with --with-dumping=unexec])
fi

AC_CACHE_CHECK([for 'find' args to delete a file],
  [emacs_cv_find_delete],
  [if touch conftest.tmp && find conftest.tmp -delete 2>/dev/null &&
      test ! -f conftest.tmp
   then emacs_cv_find_delete="-delete"
   else emacs_cv_find_delete="-exec rm -f {} ';'"
   fi])
FIND_DELETE=$emacs_cv_find_delete
AC_SUBST([FIND_DELETE])

PAXCTL_dumped=
PAXCTL_notdumped=
if test $with_unexec = yes && test $opsys = gnu-linux; then
  if test "${SETFATTR+set}" != set; then
    AC_CACHE_CHECK([for setfattr],
      [emacs_cv_prog_setfattr],
      [touch conftest.tmp
       if (setfattr -n user.pax.flags conftest.tmp) >/dev/null 2>&1; then
	 emacs_cv_prog_setfattr=yes
       else
	 emacs_cv_prog_setfattr=no
       fi])
    if test "$emacs_cv_prog_setfattr" = yes; then
      PAXCTL_notdumped='$(SETFATTR) -n user.pax.flags -v er'
      SETFATTR=setfattr
    else
      SETFATTR=
    fi
  fi
  case $opsys,$PAXCTL_notdumped,$emacs_uname_r in
    gnu-linux,,* | netbsd,,[0-7].*)
      AC_PATH_PROG([PAXCTL], [paxctl], [],
	[$PATH$PATH_SEPARATOR/sbin$PATH_SEPARATOR/usr/sbin])
      if test -n "$PAXCTL"; then
	if test "$opsys" = netbsd; then
	  PAXCTL_dumped='$(PAXCTL) +a'
	  PAXCTL_notdumped=$PAXCTL_dumped
	else
	  AC_MSG_CHECKING([whether binaries have a PT_PAX_FLAGS header])
	  AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])],
	    [if $PAXCTL -v conftest$EXEEXT >/dev/null 2>&1; then
	       AC_MSG_RESULT([yes])
	     else
	       AC_MSG_RESULT([no])
	       PAXCTL=
	     fi])
	  if test -n "$PAXCTL"; then
	    PAXCTL_dumped='$(PAXCTL) -zex'
	    PAXCTL_notdumped='$(PAXCTL) -r'
	  fi
	fi
      fi;;
  esac
fi
AC_SUBST([PAXCTL_dumped])
AC_SUBST([PAXCTL_notdumped])
AC_SUBST([SETFATTR])

# Makeinfo on macOS is ancient, check whether there is a more recent
# version installed by Homebrew.
AC_CHECK_PROGS(BREW, [brew])
if test -n "$BREW"; then
  AC_PATH_PROG([MAKEINFO], [makeinfo], [],
    [`$BREW --prefix texinfo 2>/dev/null`/bin$PATH_SEPARATOR$PATH])
fi

## Require makeinfo >= 4.13 (last of the 4.x series) to build the manuals.
if test "${MAKEINFO:=makeinfo}" != "no"; then
  case `($MAKEINFO --version) 2>/dev/null` in
    *' (GNU texinfo) '4.1[[3-9]]* | \
    *' (GNU texinfo) '[[5-9]]* | \
    *' (GNU texinfo) '[[1-9][0-9]]* ) ;;
    *) MAKEINFO=no;;
  esac
fi

## Makeinfo is unusual.  For a released Emacs, the manuals are
## pre-built, and not deleted by the normal clean rules.  makeinfo is
## therefore in the category of "special tools" not normally required, which
## configure does not have to check for (eg autoconf itself).
## In a repository checkout on the other hand, the manuals are not included.
## So makeinfo is a requirement to build from the repository, and configure
## should test for it as it does for any other build requirement.
## We use the presence of $srcdir/info/emacs to distinguish a release,
## with pre-built manuals, from a repository checkout.
HAVE_MAKEINFO=yes

if test "$MAKEINFO" = "no"; then
  MAKEINFO=makeinfo
  if test "x${with_makeinfo}" = "xno"; then
    HAVE_MAKEINFO=no
  elif test ! -e "$srcdir/info/emacs" && test ! -e "$srcdir/info/emacs.info"; then
    AC_MSG_ERROR( [You do not seem to have makeinfo >= 4.13, and your
source tree does not seem to have pre-built manuals in the 'info' directory.
Either install a suitable version of makeinfo, or re-run configure
with the '--without-makeinfo' option to build without the manuals.] )
  fi
fi
AC_SUBST([MAKEINFO])
AC_SUBST(HAVE_MAKEINFO)

if test $opsys = mingw32; then
   DOCMISC_W32=efaq-w32
else
   DOCMISC_W32=
fi
AC_SUBST(DOCMISC_W32)

dnl Add our options to ac_link now, after it is set up.

if test x$GCC = xyes; then
  test "x$GCC_LINK_TEST_OPTIONS" != x && \
    ac_link="$ac_link $GCC_LINK_TEST_OPTIONS"
else
  test "x$NON_GCC_LINK_TEST_OPTIONS" != x && \
    ac_link="$ac_link $NON_GCC_LINK_TEST_OPTIONS"
fi

dnl On some platforms using GNU ld, linking temacs needs -znocombreloc.
dnl Although this has something to do with dumping, the details are unknown.
dnl If the flag is used but not needed,
dnl Emacs should still work (albeit a bit more slowly),
dnl so use the flag everywhere that it is supported.
dnl When testing whether the flag works, treat GCC specially
dnl since it just gives a non-fatal 'unrecognized option'
dnl if not built to support GNU ld.
if test "$GCC" = yes; then
  LDFLAGS_NOCOMBRELOC="-Wl,-znocombreloc"
else
  LDFLAGS_NOCOMBRELOC="-znocombreloc"
fi

AC_CACHE_CHECK([for -znocombreloc], [emacs_cv_znocombreloc],
  [if test $with_unexec = no; then
     emacs_cv_znocombreloc='not needed'
   else
     save_LDFLAGS=$LDFLAGS
     LDFLAGS="$LDFLAGS $LDFLAGS_NOCOMBRELOC"
     AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])],
       [emacs_cv_znocombreloc=yes], [emacs_cv_znocombreloc=no])
     LDFLAGS=$save_LDFLAGS
   fi])

case $emacs_cv_znocombreloc in
  no*)
    LDFLAGS_NOCOMBRELOC= ;;
esac


AC_CACHE_CHECK([whether addresses are sanitized],
  [emacs_cv_sanitize_address],
  [AC_COMPILE_IFELSE(
     [AC_LANG_PROGRAM(
	[[#ifndef __has_feature
	  #define __has_feature(f) 0
	  #endif
	  #if defined __SANITIZE_ADDRESS__ || __has_feature (address_sanitizer)
	  #else
	   error "Addresses are not sanitized.";
	  #endif
	]])],
     [emacs_cv_sanitize_address=yes],
     [emacs_cv_sanitize_address=no])])

if test $with_unexec = yes; then
  AC_DEFINE([HAVE_UNEXEC], 1, [Define if Emacs supports unexec.])
  if test "$emacs_cv_sanitize_address" = yes; then
    AC_MSG_WARN([[Addresses are sanitized; suggest --without-unexec]])
  fi
fi


UNEXEC_OBJ=
test $with_unexec = yes &&
case "$opsys" in
  # MSDOS uses unexcoff.o
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
  mingw32)
   UNEXEC_OBJ=unexw32.o
   ;;
  sol2-10)
   # Use the Solaris dldump() function, called from unexsol.c, to dump
   # emacs, instead of the generic ELF dump code found in unexelf.c.
   # The resulting binary has a complete symbol table, and is better
   # for debugging and other observability tools (debuggers, pstack, etc).
   #
   # It is likely that dldump() works with older Solaris too, but this has
   # not been tested, so for now this change is for Solaris 10 or newer.
   UNEXEC_OBJ=unexsol.o
   ;;
  *)
   UNEXEC_OBJ=unexelf.o
   ;;
esac

LD_SWITCH_SYSTEM=
test "$with_unexec" = no || case "$opsys" in
  freebsd|dragonfly)
   ## Let 'ld' find image libs and similar things in /usr/local/lib.
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
   ## Han Boetes <han@boetes.org> says this is necessary,
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

test $with_unexec = yes &&
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
      AC_MSG_ERROR([Non-GCC compilers are not supported.])
    fi
  else
      dnl This was the unexalpha.c case.  Removed in 24.1, 2010-07-24,
      dnl albeit under the mistaken assumption that said file
      dnl was no longer used.
      AC_MSG_ERROR([Non-ELF systems are not supported since Emacs 24.1.])
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
if test "$opsys" = "mingw32"; then
  case "$canonical" in
    x86_64-*-mingw*) C_SWITCH_SYSTEM="-mtune=generic" ;;
    *) C_SWITCH_SYSTEM="-mtune=pentium4" ;;
  esac
fi
## gnu-linux might need -D_BSD_SOURCE on old libc5 systems.
## It is redundant in glibc2, since we define _GNU_SOURCE.
AC_SUBST(C_SWITCH_SYSTEM)


LIBS_SYSTEM=
case "$opsys" in
  ## IBM's X11R5 uses -lIM and -liconv in AIX 3.2.2.
  aix4-2) LIBS_SYSTEM="-lrts -lIM -liconv" ;;

  freebsd|dragonfly) LIBS_SYSTEM="-lutil" ;;

  hpux*) LIBS_SYSTEM="-l:libdld.sl" ;;

  qnxnto) LIBS_SYSTEM="-lsocket" ;;

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

# Suppress obsolescent Autoconf test for size_t; Emacs assumes C99 or better.
AC_DEFUN([AC_TYPE_SIZE_T])
# Likewise for obsolescent test for uid_t, gid_t; Emacs assumes them.
AC_DEFUN([AC_TYPE_UID_T])

# sqrt and other floating-point functions such as fmod and frexp
# are found in -lm on many systems.
OLD_LIBS=$LIBS
AC_SEARCH_LIBS([sqrt], [m])
if test "X$LIBS" = "X$OLD_LIBS"; then
  LIB_MATH=
else
  LIB_MATH=$ac_cv_search_sqrt
fi
LIBS=$OLD_LIBS

dnl Current possibilities handled by sed (aix4-2 -> aix,
dnl gnu-linux -> gnu/linux, etc.):
dnl gnu, gnu/linux, gnu/kfreebsd, aix, cygwin, darwin, hpux.
dnl And special cases: berkeley-unix, usg-unix-v, ms-dos, windows-nt.
SYSTEM_TYPE=`echo $opsys | sed -e 's/[[0-9]].*//' -e 's|-|/|'`

case $opsys in
  cygwin )
    LIB_MATH=
    ;;
  darwin )
    ## Adding -lm confuses the dynamic linker, so omit it.
    LIB_MATH=
    ;;
  freebsd | dragonfly )
    SYSTEM_TYPE=berkeley-unix
    ;;
  gnu-linux | gnu-kfreebsd )
    ;;
  hpux10-20 | hpux11 )
    ;;
  mingw32 )
    LIB_MATH=
    SYSTEM_TYPE=windows-nt
    ;;
  netbsd | openbsd )
    SYSTEM_TYPE=berkeley-unix
    ;;

  sol2* | unixware )
    SYSTEM_TYPE=usg-unix-v
    ;;

esac

AC_SUBST(LIB_MATH)
AC_DEFINE_UNQUOTED(SYSTEM_TYPE, "$SYSTEM_TYPE",
  [The type of system you are compiling for; sets 'system-type'.])
AC_SUBST([SYSTEM_TYPE])


pre_PKG_CONFIG_CFLAGS=$CFLAGS
pre_PKG_CONFIG_LIBS=$LIBS

PKG_PROG_PKG_CONFIG(0.9.0)

dnl EMACS_CHECK_MODULES(GSTUFF, gtk+-2.0 >= 1.3 glib = 1.3.4)
dnl acts like PKG_CHECK_MODULES(GSTUFF, gtk+-2.0 >= 1.3 glib = 1.3.4,
dnl HAVE_GSTUFF=yes, HAVE_GSTUFF=no) -- see pkg-config man page --
dnl except that it postprocesses CFLAGS as needed for --enable-gcc-warnings.
dnl EMACS_CHECK_MODULES accepts optional 3rd and 4th arguments that
dnl can take the place of the default HAVE_GSTUFF=yes and HAVE_GSTUFF=no
dnl actions.
AC_DEFUN([EMACS_CHECK_MODULES],
  [PKG_CHECK_MODULES([$1], [$2],
     [$1_CFLAGS=`AS_ECHO(["$$1_CFLAGS"]) | sed -e "$edit_cflags"`
      m4_default([$3], [HAVE_$1=yes])],
     [m4_default([$4], [HAVE_$1=no])])])

HAVE_SOUND=no
if test "${with_sound}" != "no"; then
  # Sound support for GNU/Linux, the free BSDs, MinGW, and Cygwin.
  AC_CHECK_HEADERS([machine/soundcard.h sys/soundcard.h soundcard.h mmsystem.h],
    have_sound_header=yes, [], [
    #ifdef __MINGW32__
    #define WIN32_LEAN_AND_MEAN
    #include <windows.h>
    #endif
    ])
  test "${with_sound}" = "oss" && test "${have_sound_header}" != "yes" && \
    AC_MSG_ERROR([OSS sound support requested but not found.])

  if test "${with_sound}" = "bsd-ossaudio" || test "${with_sound}" = "yes"; then
    # Emulation library used on NetBSD.
    AC_CHECK_LIB(ossaudio, _oss_ioctl, LIBSOUND=-lossaudio, LIBSOUND=)
    test "${with_sound}" = "bsd-ossaudio" && test -z "$LIBSOUND" && \
      AC_MSG_ERROR([bsd-ossaudio sound support requested but not found.])
    dnl FIXME?  If we did find ossaudio, should we set with_sound=bsd-ossaudio?
    dnl Traditionally, we go on to check for alsa too.  Does that make sense?
  fi
  AC_SUBST(LIBSOUND)

  if test "${with_sound}" = "alsa" || test "${with_sound}" = "yes"; then
    ALSA_REQUIRED=1.0.0
    ALSA_MODULES="alsa >= $ALSA_REQUIRED"
    EMACS_CHECK_MODULES([ALSA], [$ALSA_MODULES])
    if test $HAVE_ALSA = yes; then
      LIBSOUND="$LIBSOUND $ALSA_LIBS"
      CFLAGS_SOUND="$CFLAGS_SOUND $ALSA_CFLAGS"
      AC_DEFINE(HAVE_ALSA, 1, [Define to 1 if ALSA is available.])
    elif test "${with_sound}" = "alsa"; then
      AC_MSG_ERROR([ALSA sound support requested but not found.])
    fi
  fi                            dnl with_sound = alsa|yes

  dnl Define HAVE_SOUND if we have sound support.  We know it works and
  dnl compiles only on the specified platforms.  For others, it
  dnl probably doesn't make sense to try.
  dnl FIXME So surely we should bypass this whole section if not using
  dnl one of these platforms?
  if test x$have_sound_header = xyes || test $HAVE_ALSA = yes; then
     case "$opsys" in
       dnl defined __FreeBSD__ || defined __NetBSD__ || defined __linux__
       dnl Adjust the --with-sound help text if you change this.
       gnu-linux|freebsd|netbsd|mingw32|cygwin)
         AC_DEFINE(HAVE_SOUND, 1, [Define to 1 if you have sound support.])
         HAVE_SOUND=yes
         ;;
     esac
  fi

  AC_SUBST(CFLAGS_SOUND)
fi

dnl checks for header files
AC_CHECK_HEADERS_ONCE(
  linux/fs.h
  malloc.h
  sys/systeminfo.h
  sys/sysinfo.h
  coff.h pty.h
  sys/resource.h
  sys/utsname.h pwd.h utmp.h util.h)

AC_CACHE_CHECK([for ADDR_NO_RANDOMIZE],
  [emacs_cv_personality_addr_no_randomize],
  [AC_COMPILE_IFELSE(
     [AC_LANG_PROGRAM([[#include <sys/personality.h>]],
		      [[personality (personality (0xffffffff)
				     | ADDR_NO_RANDOMIZE)]])],
     [emacs_cv_personality_addr_no_randomize=yes],
     [emacs_cv_personality_addr_no_randomize=no])])
if test $emacs_cv_personality_addr_no_randomize = yes; then
  AC_DEFINE([HAVE_PERSONALITY_ADDR_NO_RANDOMIZE], [1],
            [Define to 1 if personality flag ADDR_NO_RANDOMIZE exists.])
fi

# Note that Solaris has sys/sysinfo.h which defines struct
# sysinfo as well.  To make sure that we're using GNU/Linux
# sysinfo, we explicitly set one of its fields.
if test "$ac_cv_header_sys_sysinfo_h" = yes; then
  AC_CACHE_CHECK([if Linux sysinfo may be used], [emacs_cv_linux_sysinfo],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/sysinfo.h>]],
                                     [[struct sysinfo si;
                                       si.totalram = 0;
                                       sysinfo (&si)]])],
    emacs_cv_linux_sysinfo=yes, emacs_cv_linux_sysinfo=no)])

  if test $emacs_cv_linux_sysinfo = yes; then
    AC_DEFINE([HAVE_LINUX_SYSINFO], 1, [Define to 1 if you have Linux sysinfo function.])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/sysinfo.h>]],
                                       [[struct sysinfo si; return si.mem_unit]])],
      AC_DEFINE(LINUX_SYSINFO_UNIT, 1,
                [Define to 1 if Linux sysinfo sizes are in multiples of mem_unit bytes.]))
  fi
fi

dnl On Solaris 8 there's a compilation warning for term.h because
dnl it doesn't define 'bool'.
AC_CHECK_HEADERS(term.h, , , -)
AC_HEADER_TIME
AC_CHECK_DECLS([sys_siglist], [], [], [[#include <signal.h>
			              ]])
if test $ac_cv_have_decl_sys_siglist != yes; then
  # For Tru64, at least:
  AC_CHECK_DECLS([__sys_siglist], [], [], [[#include <signal.h>
			                  ]])
fi
AC_HEADER_SYS_WAIT

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

AUTO_DEPEND=no
AUTODEPEND_PARENTS='lib src'
dnl check if we have GCC and autodepend is on.
if test "$GCC" = yes && test "$ac_enable_autodepend" = yes; then
   AC_CACHE_CHECK([whether gcc understands -MMD -MF], [emacs_cv_autodepend],
   [SAVE_CFLAGS="$CFLAGS"
   CFLAGS="$CFLAGS -MMD -MF deps.d -MP"
   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[]])],
     [emacs_cv_autodepend=yes], [emacs_cv_autodepend=no])
   CFLAGS="$SAVE_CFLAGS"
   test -f deps.d || emacs_cv_autodepend=no
   rm -rf deps.d])
   if test $emacs_cv_autodepend = yes; then
      AUTO_DEPEND=yes
   fi
fi
AC_SUBST(AUTO_DEPEND)

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
    LD_SWITCH_X_SITE=-L`AS_ECHO(["$x_libraries"]) | sed -e 's/:/ -L/g'`
    LD_SWITCH_X_SITE_RPATH=-Wl,-rpath,`
      AS_ECHO(["$x_libraries"]) | sed -e 's/:/ -Wl,-rpath,/g'
    `
  fi
  x_default_search_path=""
  x_search_path=${x_libraries}
  if test -z "${x_search_path}"; then
    x_search_path=/usr/lib
  fi
  for x_library in `AS_ECHO(["$x_search_path:"]) | \
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
  C_SWITCH_X_SITE=$isystem`AS_ECHO(["$x_includes"]) | sed -e "s/:/ $isystem/g"`
fi

if test x"${x_includes}" = x; then
  bitmapdir=/usr/include/X11/bitmaps
else
  # accumulate include directories that have X11 bitmap subdirectories
  bmd_acc=
  for bmd in `AS_ECHO(["$x_includes"]) | sed -e 's/:/ /g'`; do
    if test -d "${bmd}/X11/bitmaps"; then
      bmd_acc="${bmd_acc}:${bmd}/X11/bitmaps"
    fi
    if test -d "${bmd}/bitmaps"; then
      bmd_acc="${bmd_acc}:${bmd}/bitmaps"
    fi
  done
  bitmapdir=${bmd_acc#:}
fi

test "${with_ns}" = maybe && test "${opsys}" != darwin && with_ns=no
HAVE_NS=no
NS_GNUSTEP_CONFIG=no
NS_IMPL_COCOA=no
NS_IMPL_GNUSTEP=no
tmp_CPPFLAGS="$CPPFLAGS"
tmp_CFLAGS="$CFLAGS"
CPPFLAGS="$CPPFLAGS -x objective-c"
CFLAGS="$CFLAGS -x objective-c"
GNU_OBJC_CFLAGS=
LIBS_GNUSTEP=
if test "${with_ns}" != no; then
  # macfont.o requires macuvs.h which is absent after 'make extraclean',
  # so avoid NS_IMPL_COCOA if macuvs.h is absent.
  # Even a headless Emacs can build macuvs.h, so this should let you bootstrap.
  if test "${opsys}" = darwin && test -f "$srcdir/src/macuvs.h"; then
     NS_IMPL_COCOA=yes
     ns_appdir=`pwd`/nextstep/Emacs.app
     ns_appbindir=${ns_appdir}/Contents/MacOS
     ns_appresdir=${ns_appdir}/Contents/Resources
     ns_appsrc=Cocoa/Emacs.base
     ns_fontfile=macfont.o
  elif flags=$( (gnustep-config --objc-flags) 2>/dev/null); then
     NS_IMPL_GNUSTEP=yes
     NS_GNUSTEP_CONFIG=yes
     GNU_OBJC_CFLAGS=$flags
     LIBS_GNUSTEP=$(gnustep-config --gui-libs) || exit
  elif test -f $GNUSTEP_CONFIG_FILE; then
     NS_IMPL_GNUSTEP=yes
     dnl FIXME sourcing this several times in subshells seems inefficient.
     GNUSTEP_SYSTEM_HEADERS=$(
       . $GNUSTEP_CONFIG_FILE
       AS_ECHO(["$GNUSTEP_SYSTEM_HEADERS"])
     )
     GNUSTEP_SYSTEM_LIBRARIES=$(
       . $GNUSTEP_CONFIG_FILE
       AS_ECHO(["$GNUSTEP_SYSTEM_LIBRARIES"])
     )
     dnl I seemed to need these as well with GNUstep-startup 0.25.
     GNUSTEP_LOCAL_HEADERS=$(
       . $GNUSTEP_CONFIG_FILE
       AS_ECHO(["$GNUSTEP_LOCAL_HEADERS"])
     )
     GNUSTEP_LOCAL_LIBRARIES=$(
       . $GNUSTEP_CONFIG_FILE
       AS_ECHO(["$GNUSTEP_LOCAL_LIBRARIES"])
     )
     test "x${GNUSTEP_LOCAL_HEADERS}" != "x" && \
       GNUSTEP_LOCAL_HEADERS="-I${GNUSTEP_LOCAL_HEADERS}"
     test "x${GNUSTEP_LOCAL_LIBRARIES}" != "x" && \
       GNUSTEP_LOCAL_LIBRARIES="-L${GNUSTEP_LOCAL_LIBRARIES}"
     CPPFLAGS="$CPPFLAGS -I${GNUSTEP_SYSTEM_HEADERS} ${GNUSTEP_LOCAL_HEADERS}"
     CFLAGS="$CFLAGS -I${GNUSTEP_SYSTEM_HEADERS} ${GNUSTEP_LOCAL_HEADERS}"
     LDFLAGS="$LDFLAGS -L${GNUSTEP_SYSTEM_LIBRARIES} ${GNUSTEP_LOCAL_LIBRARIES}"
     LIBS_GNUSTEP="-lgnustep-gui -lgnustep-base -lobjc -lpthread"
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
  if test $NS_IMPL_GNUSTEP = yes; then
     ns_appdir=`pwd`/nextstep/Emacs.app
     ns_appbindir=${ns_appdir}
     ns_appresdir=${ns_appdir}/Resources
     ns_appsrc=GNUstep/Emacs.base
     ns_fontfile=nsfont.o
  fi

  dnl This is only used while we test the NS headers, it gets reset below.
  CPPFLAGS="$CPPFLAGS $GNU_OBJC_CFLAGS"
  CFLAGS="$CFLAGS $GNU_OBJC_CFLAGS"

  AC_CHECK_HEADER([AppKit/AppKit.h], [HAVE_NS=yes],
		  [AC_MSG_ERROR([The include files (AppKit/AppKit.h etc) that
are required for a Nextstep build are missing or cannot be compiled.
Either fix this, or re-configure with the option '--without-ns'.])])

  macfont_file=""
  if test "${NS_IMPL_COCOA}" = "yes"; then
    AC_MSG_CHECKING([for Mac OS X 10.6 or newer])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <AppKit/AppKit.h>],
                                     [
#ifdef MAC_OS_X_VERSION_MAX_ALLOWED
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1060
 ; /* OK */
#else
 error "Mac OS X 10.6 or newer required";
#endif
#endif
		    ])],
		    ns_osx_have_106=yes,
		    ns_osx_have_106=no)
    AC_MSG_RESULT([$ns_osx_have_106])

    if test $ns_osx_have_106 = no; then
       AC_MSG_ERROR([Mac OS X 10.6 or newer is required]);
    fi
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
     etcdocdir="\${ns_appresdir}/etc"
     etcdir="\${ns_appresdir}/etc"
     dnl FIXME maybe set datarootdir instead.
     dnl That would also get applications, icons, man.
     infodir="\${ns_appresdir}/info"
     mandir="\${ns_appresdir}/man"
     lispdir="\${ns_appresdir}/lisp"
     test "$locallisppathset" = no && locallisppath=""
     INSTALL_ARCH_INDEP_EXTRA=
  fi

  NS_OBJC_OBJ="nsterm.o nsfns.o nsmenu.o nsselect.o nsimage.o $ns_fontfile"
fi
CFLAGS="$tmp_CFLAGS"
CPPFLAGS="$tmp_CPPFLAGS"
AC_SUBST(INSTALL_ARCH_INDEP_EXTRA)
AC_SUBST(ns_self_contained)
AC_SUBST(NS_OBJ)
AC_SUBST(NS_OBJC_OBJ)

if test "${HAVE_NS}" = yes; then
  AC_CACHE_CHECK(
    [if the Objective C compiler supports instancetype],
    [emacs_cv_objc_instancetype],
    [AC_LANG_PUSH([Objective C])
     AC_COMPILE_IFELSE(
       [AC_LANG_SOURCE([[@interface Test
                          + (instancetype)test;
                          @end]])],
       emacs_cv_objc_instancetype=yes,
       emacs_cv_objc_instancetype=no)
     AC_LANG_POP([Objective C])])

  if test x$emacs_cv_objc_instancetype = xyes ; then
    AC_DEFINE(NATIVE_OBJC_INSTANCETYPE, 1,
              [Define if ObjC compiler supports instancetype natively.])
  fi
fi

HAVE_W32=no
W32_OBJ=
W32_LIBS=
EMACSRES=
CLIENTRES=
CLIENTW=
W32_RES_LINK=
EMACS_MANIFEST=
if test "${with_w32}" != no; then
  case "${opsys}" in
    cygwin)
      AC_CHECK_HEADER([windows.h], [HAVE_W32=yes],
             [AC_MSG_ERROR(['--with-w32' was specified, but windows.h
                   cannot be found.])])
    ;;
    mingw32)
    ## Using --with-w32 with MinGW is a no-op, but we allow it.
    ;;
    *)
      AC_MSG_ERROR([Using w32 with an autotools build is only supported for Cygwin and MinGW32.])
    ;;
  esac
fi

if test "${opsys}" = "mingw32"; then
  AC_CACHE_CHECK([whether Windows API headers are recent enough], [emacs_cv_w32api],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
     #include <windows.h>
     #include <usp10.h>]],
   [[PIMAGE_NT_HEADERS pHeader;
     PIMAGE_SECTION_HEADER pSection = IMAGE_FIRST_SECTION(pHeader)]])],
   [emacs_cv_w32api=yes], [emacs_cv_w32api=no])])
  if test "${emacs_cv_w32api}" = "no"; then
    AC_MSG_ERROR([the Windows API headers are too old to support this build.])
  fi
  HAVE_W32=${emacs_cv_w32api}
fi

FIRSTFILE_OBJ=
NTDIR=
LIBS_ECLIENT=
LIB_WSOCK32=
NTLIB=
CM_OBJ="cm.o"
XARGS_LIMIT=
if test "${HAVE_W32}" = "yes"; then
  AC_DEFINE(HAVE_NTGUI, 1, [Define to use native MS Windows GUI.])
  if test "$with_toolkit_scroll_bars" = "no"; then
    AC_MSG_ERROR([Non-toolkit scroll bars are not implemented for w32 build.])
  fi
  AC_CHECK_TOOL(WINDRES, [windres],
                [AC_MSG_ERROR([No resource compiler found.])])
  W32_OBJ="w32fns.o w32menu.o w32reg.o w32font.o w32term.o"
  W32_OBJ="$W32_OBJ w32xfns.o w32select.o w32uniscribe.o w32cygwinx.o"
  EMACSRES="emacs.res"
  case "$canonical" in
    x86_64-*-*) EMACS_MANIFEST="emacs-x64.manifest" ;;
    *) EMACS_MANIFEST="emacs-x86.manifest" ;;
  esac
  dnl Construct something of the form "24,4,0,0" with 4 components.
  comma_version=`echo "${PACKAGE_VERSION}.0.0" | sed -e 's/\./,/g' -e 's/^\([[^,]]*,[[^,]]*,[[^,]]*,[[^,]]*\).*/\1/'`

  comma_space_version=`echo "$comma_version" | sed 's/,/, /g'`
  AC_SUBST(comma_version)
  AC_SUBST(comma_space_version)
  AC_CONFIG_FILES([nt/emacs.rc nt/emacsclient.rc])
  if test "${opsys}" = "cygwin"; then
    W32_LIBS="$W32_LIBS -lkernel32 -luser32 -lusp10 -lgdi32"
    W32_LIBS="$W32_LIBS -lole32 -lcomdlg32 -lcomctl32 -lwinspool"
    # Tell the linker that emacs.res is an object (which we compile from
    # the rc file), not a linker script.
    W32_RES_LINK="-Wl,emacs.res"
  else
    W32_OBJ="$W32_OBJ w32.o w32console.o w32heap.o w32inevt.o w32proc.o"
    W32_LIBS="$W32_LIBS -lwinmm -lusp10 -lgdi32 -lcomdlg32"
    W32_LIBS="$W32_LIBS -lmpr -lwinspool -lole32 -lcomctl32"
    W32_RES_LINK="\$(EMACSRES)"
    CLIENTRES="emacsclient.res"
    CLIENTW="emacsclientw\$(EXEEXT)"
    FIRSTFILE_OBJ=firstfile.o
    NTDIR=nt
    CM_OBJ=
    LIBS_ECLIENT="-lcomctl32"
    LIB_WSOCK32="-lwsock32"
    NTLIB="ntlib.$ac_objext"
    XARGS_LIMIT="-s 10000"
  fi
fi

if test "${HAVE_W32}" = "no" && test "${opsys}" = "cygwin"; then
  W32_LIBS="-lkernel32"
  W32_OBJ="w32cygwinx.o"
fi

AC_SUBST(W32_OBJ)
AC_SUBST(W32_LIBS)
AC_SUBST(EMACSRES)
AC_SUBST(EMACS_MANIFEST)
AC_SUBST(CLIENTRES)
AC_SUBST(CLIENTW)
AC_SUBST(W32_RES_LINK)
AC_SUBST(FIRSTFILE_OBJ)
AC_SUBST(NTDIR)
AC_SUBST(CM_OBJ)
AC_SUBST(LIBS_ECLIENT)
AC_SUBST(LIB_WSOCK32)
AC_SUBST(NTLIB)
AC_SUBST(XARGS_LIMIT)

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
      {
        for emacs_libX11 in /usr/lib/libX11.*; do break; done
        test "$emacs_libX11" != '/usr/lib/libX11.*'
      }
   then
        AC_MSG_ERROR([You seem to be running X, but no X development libraries
were found.  You should install the relevant development files for X
and for the toolkit you want, such as Gtk+ or Motif.  Also make
sure you have development files for image handling, i.e.
tiff, gif, jpeg, png and xpm.
If you are sure you want Emacs compiled without X window support, pass
  --without-x
to configure.])
   fi
fi

# Does the opsystem file prohibit the use of the GNU malloc?
# Assume not, until told otherwise.
GNU_MALLOC=yes

AC_CACHE_CHECK(
  [whether malloc is Doug Lea style],
  [emacs_cv_var_doug_lea_malloc],
  [emacs_cv_var_doug_lea_malloc=no
   dnl Hooks do not work with address sanitization.
   if test "$emacs_cv_sanitize_address" != yes; then
     AC_LINK_IFELSE(
       [AC_LANG_PROGRAM(
	  [[#include <malloc.h>
	    static void hook (void) {}]],
	  [[malloc_set_state (malloc_get_state ());
	    __after_morecore_hook = hook;
	    __malloc_initialize_hook = hook;]])],
       [emacs_cv_var_doug_lea_malloc=yes])
   fi])
doug_lea_malloc=$emacs_cv_var_doug_lea_malloc

hybrid_malloc=
system_malloc=yes

test $with_unexec = yes &&
case "$opsys" in
  ## darwin ld insists on the use of malloc routines in the System framework.
  darwin | mingw32 | nacl | sol2-10) ;;
  cygwin | qnxnto | freebsd)
	  hybrid_malloc=yes
          system_malloc= ;;
  *) test "$ac_cv_func_sbrk" = yes && system_malloc=$emacs_cv_sanitize_address;;
esac

if test "${system_malloc}" != yes && test "${doug_lea_malloc}" != yes \
   && test "${UNEXEC_OBJ}" = unexelf.o; then
  hybrid_malloc=yes
fi

GMALLOC_OBJ=
HYBRID_MALLOC=
if test "${system_malloc}" = "yes"; then
  AC_DEFINE([SYSTEM_MALLOC], 1,
    [Define to 1 to use the system memory allocator, even if it is not
     Doug Lea style.])
  GNU_MALLOC=no
  GNU_MALLOC_reason="
    (The GNU allocators don't work with this system configuration.)"
  VMLIMIT_OBJ=
elif test "$hybrid_malloc" = yes; then
  AC_DEFINE(HYBRID_MALLOC, 1,
    [Define to use gmalloc before dumping and the system malloc after.])
  HYBRID_MALLOC=1
  GNU_MALLOC=no
  GNU_MALLOC_reason=" (only before dumping)"
  GMALLOC_OBJ=gmalloc.o
  VMLIMIT_OBJ=
else
  test "$doug_lea_malloc" != "yes" && GMALLOC_OBJ=gmalloc.o
  VMLIMIT_OBJ=vm-limit.o

  AC_CHECK_HEADERS([sys/vlimit.h])
  AC_CACHE_CHECK([for data_start], [emacs_cv_data_start],
    [AC_LINK_IFELSE(
       [AC_LANG_PROGRAM(
	 [[extern char data_start[]; char ch;]],
	 [[return data_start < &ch;]])],
       [emacs_cv_data_start=yes],
       [emacs_cv_data_start=no])])
  if test $emacs_cv_data_start = yes; then
    AC_DEFINE([HAVE_DATA_START], 1,
      [Define to 1 if data_start is the address of the start
       of the main data segment.])
  fi
fi
AC_SUBST([HYBRID_MALLOC])
AC_SUBST(GMALLOC_OBJ)
AC_SUBST(VMLIMIT_OBJ)

if test "$doug_lea_malloc" = "yes" && test "$hybrid_malloc" != yes; then
  if test "$GNU_MALLOC" = yes ; then
    GNU_MALLOC_reason="
      (Using Doug Lea's new malloc from the GNU C Library.)"
  fi
  AC_DEFINE(DOUG_LEA_MALLOC, 1,
    [Define to 1 if the system memory allocator is Doug Lea style,
     with malloc hooks and malloc_set_state.])

  ## Use mmap directly for allocating larger buffers.
  ## FIXME this comes from src/s/{gnu,gnu-linux}.h:
  ## #ifdef DOUG_LEA_MALLOC; #undef REL_ALLOC; #endif
  ## Does the AC_FUNC_MMAP test below make this check unnecessary?
  case "$opsys" in
    mingw32|gnu*) REL_ALLOC=no ;;
  esac
fi

if test x"${REL_ALLOC}" = x; then
  REL_ALLOC=${GNU_MALLOC}
fi

use_mmap_for_buffers=no
case "$opsys" in
  mingw32) use_mmap_for_buffers=yes ;;
esac

AC_FUNC_MMAP
if test $use_mmap_for_buffers = yes; then
  AC_DEFINE(USE_MMAP_FOR_BUFFERS, 1, [Define to use mmap to allocate buffer text.])
  REL_ALLOC=no
fi

LIBS="$LIBS_SYSTEM $LIBS"

dnl FIXME replace main with a function we actually want from this library.
AC_CHECK_LIB(Xbsd, main, LD_SWITCH_X_SITE="$LD_SWITCH_X_SITE -lXbsd")

dnl Check for the POSIX thread library.
LIB_PTHREAD=
AC_CHECK_HEADERS_ONCE(pthread.h)
if test "$ac_cv_header_pthread_h" && test "$opsys" != "mingw32"; then
  AC_CACHE_CHECK([for pthread library],
    [emacs_cv_pthread_lib],
    [emacs_cv_pthread_lib=no
     OLD_LIBS=$LIBS
     for emacs_pthread_lib in 'none needed' -lpthread; do
       case $emacs_pthread_lib in
	 -*) LIBS="$OLD_LIBS $emacs_pthread_lib";;
       esac
       AC_LINK_IFELSE(
	 [AC_LANG_PROGRAM(
	    [[#include <pthread.h>
	      #include <signal.h>
	      sigset_t old_mask, new_mask;
	      void noop (void) {}]],
	    [[pthread_t th = pthread_self ();
	      int status = 0;
	      status += pthread_create (&th, 0, 0, 0);
	      status += pthread_sigmask (SIG_BLOCK, &new_mask, &old_mask);
	      status += pthread_kill (th, 0);
	      #if ! (defined SYSTEM_MALLOC || defined HYBRID_MALLOC \
		     || defined DOUG_LEA_MALLOC)
	      /* Test for pthread_atfork only if gmalloc uses it,
		 as older-style hosts like MirBSD 10 lack it.  */
	      status += pthread_atfork (noop, noop, noop);
	      #endif
	      return status;]])],
	 [emacs_cv_pthread_lib=$emacs_pthread_lib])
       LIBS=$OLD_LIBS
       if test "$emacs_cv_pthread_lib" != no; then
	 break
       fi
     done])
  if test "$emacs_cv_pthread_lib" != no; then
    AC_DEFINE([HAVE_PTHREAD], 1, [Define to 1 if you have POSIX threads.])
    case $emacs_cv_pthread_lib in
      -*) LIB_PTHREAD=$emacs_cv_pthread_lib;;
    esac
    ac_cv_func_pthread_sigmask=yes
    # Some systems optimize for single-threaded programs by default, and
    # need special flags to disable these optimizations. For example, the
    # definition of 'errno' in <errno.h>.
    case $opsys in
      hpux* | sol*)
	AC_DEFINE([_REENTRANT], 1,
	  [Define to 1 if your system requires this in multithreaded code.]);;
      aix4-2)
	AC_DEFINE([_THREAD_SAFE], 1,
	  [Define to 1 if your system requires this in multithreaded code.]);;
    esac
  fi
fi
AC_SUBST([LIB_PTHREAD])

AC_MSG_CHECKING([for thread support])
threads_enabled=no
if test "$with_threads" = yes; then
   if test "$emacs_cv_pthread_lib" != no; then
      AC_DEFINE(THREADS_ENABLED, 1,
                [Define to 1 if you want elisp thread support.])
      threads_enabled=yes
   elif test "${opsys}" = "mingw32"; then
      dnl MinGW can do native Windows threads even without pthreads
      AC_DEFINE(THREADS_ENABLED, 1,
                [Define to 1 if you want elisp thread support.])
      threads_enabled=yes
   fi
fi
AC_MSG_RESULT([$threads_enabled])

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

# Change CFLAGS, CPPFLAGS, and LIBS temporarily so that C_SWITCH_X_SITE
# is for the tests that follow.  We set them back later on.

REAL_CFLAGS="$CFLAGS"
REAL_CPPFLAGS="$CPPFLAGS"
REAL_LIBS="$LIBS"

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
    AC_CACHE_CHECK([whether X on GNU/Linux needs -b to link], [emacs_cv_b_link],
    [AC_LINK_IFELSE([AC_LANG_PROGRAM([[]],
     [[XOpenDisplay ("foo");]])],
     [xgnu_linux_first_failure=no],
     [xgnu_linux_first_failure=yes])
    if test "${xgnu_linux_first_failure}" = "yes"; then
      OLD_CPPFLAGS="$CPPFLAGS"
      OLD_LIBS="$LIBS"
      CPPFLAGS="$CPPFLAGS -b i486-linuxaout"
      LIBS="$LIBS -b i486-linuxaout"
      AC_LINK_IFELSE([AC_LANG_PROGRAM([[]],
       [[XOpenDisplay ("foo");]])],
       [xgnu_linux_second_failure=no],
       [xgnu_linux_second_failure=yes])
      if test "${xgnu_linux_second_failure}" = "yes"; then
	# If we get the same failure with -b, there is no use adding -b.
	# So leave it out.  This plays safe.
        emacs_cv_b_link=no
      else
        emacs_cv_b_link=yes
      fi
      CPPFLAGS=$OLD_CPPFLAGS
      LIBS=$OLD_LIBS
    else
      emacs_cv_b_link=no
    fi])
    if test "x$emacs_cv_b_link" = xyes ; then
      LD_SWITCH_X_SITE="$LD_SWITCH_X_SITE -b i486-linuxaout"
      C_SWITCH_X_SITE="$C_SWITCH_X_SITE -b i486-linuxaout"
    fi
  fi

  # Reportedly, some broken Solaris systems have XKBlib.h but are missing
  # header files included from there.
  AC_CACHE_CHECK([for Xkb], [emacs_cv_xkb],
  [AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <X11/Xlib.h>
#include <X11/XKBlib.h>]],
	[[XkbDescPtr kb = XkbGetKeyboard (0, XkbAllComponentsMask, XkbUseCoreKbd);]])],
	emacs_cv_xkb=yes, emacs_cv_xkb=no)])
  if test $emacs_cv_xkb = yes; then
    AC_DEFINE(HAVE_XKB, 1, [Define to 1 if you have the Xkb extension.])
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


### Use -lrsvg-2 if available, unless '--with-rsvg=no' is specified.
HAVE_RSVG=no
if test "${HAVE_X11}" = "yes" || test "${HAVE_NS}" = "yes" || test "${opsys}" = "mingw32"; then
  if test "${with_rsvg}" != "no"; then
    RSVG_REQUIRED=2.14.0
    RSVG_MODULE="librsvg-2.0 >= $RSVG_REQUIRED"

    EMACS_CHECK_MODULES([RSVG], [$RSVG_MODULE])
    AC_SUBST(RSVG_CFLAGS)
    AC_SUBST(RSVG_LIBS)

    if test $HAVE_RSVG = yes; then
      AC_DEFINE(HAVE_RSVG, 1, [Define to 1 if using librsvg.])
      CFLAGS="$CFLAGS $RSVG_CFLAGS"
      # Windows loads librsvg dynamically
      if test "${opsys}" = "mingw32"; then
	RSVG_LIBS=
      fi
    fi
  fi
fi

HAVE_IMAGEMAGICK=no
if test "${HAVE_X11}" = "yes" || test "${HAVE_NS}" = "yes" || test "${HAVE_W32}" = "yes"; then
  if test "${with_imagemagick}" != "no"; then
    if test -n "$BREW"; then
      # Homebrew doesn't link ImageMagick 6 by default, so make sure
      # pkgconfig can find it.
      export PKG_CONFIG_PATH="$PKG_CONFIG_PATH$PATH_SEPARATOR`$BREW --prefix imagemagick@6 2>/dev/null`/lib/pkgconfig"
    fi

    EMACS_CHECK_MODULES([IMAGEMAGICK], [MagickWand >= 7])
    if test $HAVE_IMAGEMAGICK = yes; then
       AC_DEFINE([HAVE_IMAGEMAGICK7], 1, [Define to 1 if using ImageMagick7.])
    else
       ## 6.3.5 is the earliest version known to work; see Bug#17339.
       ## 6.8.2 makes Emacs crash; see Bug#13867.
       EMACS_CHECK_MODULES([IMAGEMAGICK], [Wand >= 6.3.5 Wand != 6.8.2])
    fi

    if test $HAVE_IMAGEMAGICK = yes; then
      OLD_CFLAGS=$CFLAGS
      OLD_LIBS=$LIBS
      CFLAGS="$CFLAGS $IMAGEMAGICK_CFLAGS"
      LIBS="$IMAGEMAGICK_LIBS $LIBS"
      AC_CHECK_FUNCS([MagickRelinquishMemory MagickExportImagePixels \
		      MagickMergeImageLayers MagickAutoOrientImage])
      CFLAGS=$OLD_CFLAGS
      LIBS=$OLD_LIBS
      # Check that ImageMagick links.  It does not link on Fedora 25
      # with './configure CC=clang', as pkg-config outputs flags like
      # -lomp that work for GCC but not Clang.
      if test "$ac_cv_func_MagickRelinquishMemory" != yes; then
	HAVE_IMAGEMAGICK=no
      fi
    fi
    if test $HAVE_IMAGEMAGICK = yes; then
      AC_DEFINE([HAVE_IMAGEMAGICK], 1, [Define to 1 if using ImageMagick.])
    else
      if test "${with_imagemagick}" != "no"; then
        AC_MSG_ERROR([ImageMagick wanted, but it does not compile.  Maybe some library files are missing?]);
      fi
      IMAGEMAGICK_CFLAGS=
      IMAGEMAGICK_LIBS=
    fi
    AC_SUBST([IMAGEMAGICK_CFLAGS])
    AC_SUBST([IMAGEMAGICK_LIBS])
  fi
fi

AC_CHECK_LIB(anl, getaddrinfo_a, HAVE_GETADDRINFO_A=yes)
if test "${HAVE_GETADDRINFO_A}" = "yes"; then
  AC_DEFINE(HAVE_GETADDRINFO_A, 1,
[Define to 1 if you have getaddrinfo_a for asynchronous DNS resolution.])
  GETADDRINFO_A_LIBS="-lanl"
  AC_SUBST(GETADDRINFO_A_LIBS)
fi

HAVE_GTK=no
GTK_OBJ=
gtk_term_header=$term_header
check_gtk2=no
gtk3_pkg_errors=
if test "${opsys}" != "mingw32"; then
  if test "${with_gtk3}" = "yes" || test "${with_gtk}" = "yes" || test "$USE_X_TOOLKIT" = "maybe"; then
    GLIB_REQUIRED=2.37.5
    GTK_REQUIRED=3.10
    GTK_MODULES="gtk+-3.0 >= $GTK_REQUIRED glib-2.0 >= $GLIB_REQUIRED"

    dnl Checks for libraries.
    EMACS_CHECK_MODULES([GTK], [$GTK_MODULES],
      [pkg_check_gtk=yes], [pkg_check_gtk=no])
    if test "$pkg_check_gtk" = "no" && test "$with_gtk3" = "yes"; then
       AC_MSG_ERROR($GTK_PKG_ERRORS)
    fi
    if test "$pkg_check_gtk" = "yes"; then
       AC_DEFINE(HAVE_GTK3, 1, [Define to 1 if using GTK 3 or later.])
       GTK_OBJ=emacsgtkfixed.o
       gtk_term_header=gtkutil.h
       USE_GTK_TOOLKIT="GTK3"
       if test "x$ac_enable_gtk_deprecation_warnings" = x; then
	 AC_DEFINE([GDK_DISABLE_DEPRECATION_WARNINGS], [1],
	   [Define to 1 to disable GTK+/GDK deprecation warnings.])
	 AC_DEFINE([GLIB_DISABLE_DEPRECATION_WARNINGS], [1],
	   [Define to 1 to disable Glib deprecation warnings.])
       fi
    else
       check_gtk2=yes
       gtk3_pkg_errors="$GTK_PKG_ERRORS "
    fi
  fi

  if test "${with_gtk2}" = "yes" || test "$check_gtk2" = "yes"; then
    GLIB_REQUIRED=2.28
    GTK_REQUIRED=2.24
    GTK_MODULES="gtk+-2.0 >= $GTK_REQUIRED glib-2.0 >= $GLIB_REQUIRED"

    dnl Checks for libraries.
    EMACS_CHECK_MODULES([GTK], [$GTK_MODULES],
      [pkg_check_gtk=yes], [pkg_check_gtk=no])
    if test "$pkg_check_gtk" = "no" &&
       { test "$with_gtk" = yes || test "$with_gtk2" = "yes"; }
    then
      AC_MSG_ERROR($gtk3_pkg_errors$GTK_PKG_ERRORS)
    fi
    test "$pkg_check_gtk" = "yes" && USE_GTK_TOOLKIT="GTK2"
  fi
fi

OLD_CFLAGS=$CFLAGS
OLD_LIBS=$LIBS

if test x"$pkg_check_gtk" = xyes; then

  AC_SUBST(GTK_LIBS)
  CFLAGS="$CFLAGS $GTK_CFLAGS"
  LIBS="$GTK_LIBS $LIBS"
  dnl Try to compile a simple GTK program.
  AC_CACHE_CHECK([whether GTK compiles], [emacs_cv_gtk_compiles],
  [AC_LINK_IFELSE(
    [AC_LANG_PROGRAM(
       [[/* Check the Gtk and Glib APIs.  */
	 #include <gtk/gtk.h>
	 #include <glib-object.h>
	 static void
	 callback (GObject *go, GParamSpec *spec, gpointer user_data)
	 {}
       ]],
       [[
	 GtkSettings *gs = 0;
	 /* Use G_CALLBACK to make sure function pointers can be cast to void *;
	    strict C prohibits this.  Use gtk_main_iteration to test that the
	    libraries are there.  */
	 if (g_signal_handler_find (G_OBJECT (gs), G_SIGNAL_MATCH_FUNC,
				    0, 0, 0, G_CALLBACK (callback), 0))
	   gtk_main_iteration ();
       ]])],
    [emacs_cv_gtk_compiles=yes], [emacs_cv_gtk_compiles=no])])
  if test "${emacs_cv_gtk_compiles}" != "yes"; then
    GTK_OBJ=
    if test "$USE_X_TOOLKIT" != "maybe"; then
      AC_MSG_ERROR([Gtk+ wanted, but it does not compile, see config.log.  Maybe some x11-devel files missing?]);
    fi
  else
    C_SWITCH_X_SITE="$C_SWITCH_X_SITE $GTK_CFLAGS"
    HAVE_GTK=yes
    AC_DEFINE(USE_GTK, 1, [Define to 1 if using GTK.])
    GTK_OBJ="gtkutil.o $GTK_OBJ"
    term_header=$gtk_term_header
    USE_X_TOOLKIT=none
    AC_MSG_WARN([[Your version of Gtk+ will have problems with
       closing open displays.  This is no problem if you just use
       one display, but if you use more than one and close one of them
       Emacs may crash.
       See https://gitlab.gnome.org/GNOME/gtk/issues/221]])
  fi

fi
AC_SUBST(GTK_OBJ)


if test "${HAVE_GTK}" = "yes"; then

  dnl  GTK scrollbars resemble toolkit scrollbars a lot, so to avoid
  dnl  a lot if #ifdef:s, say we have toolkit scrollbars.
  if test "$with_toolkit_scroll_bars" != no; then
    with_toolkit_scroll_bars=yes
  fi

  term_header=gtkutil.h

  if test "${USE_GTK_TOOLKIT}" = GTK2; then

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

    dnl This procedure causes a bug on certain Ubuntu GTK+2 builds
    AC_CHECK_FUNCS(gtk_window_set_has_resize_grip)
  fi
fi


dnl Enable xwidgets if GTK3 and WebKitGTK+ are available.
HAVE_XWIDGETS=no
XWIDGETS_OBJ=
if test "$with_xwidgets" != "no"; then
  test "$USE_GTK_TOOLKIT" = "GTK3" && test "$window_system" != "none" ||
    AC_MSG_ERROR([xwidgets requested but gtk3 not used.])

  WEBKIT_REQUIRED=2.12
  WEBKIT_MODULES="webkit2gtk-4.0 >= $WEBKIT_REQUIRED"
  EMACS_CHECK_MODULES([WEBKIT], [$WEBKIT_MODULES])
  HAVE_XWIDGETS=$HAVE_WEBKIT
  test $HAVE_XWIDGETS = yes ||
    AC_MSG_ERROR([xwidgets requested but WebKitGTK+ not found.])

  XWIDGETS_OBJ=xwidget.o
  AC_DEFINE([HAVE_XWIDGETS], 1, [Define to 1 if you have xwidgets support.])
fi
AC_SUBST(XWIDGETS_OBJ)

CFLAGS=$OLD_CFLAGS
LIBS=$OLD_LIBS

dnl D-Bus has been tested under GNU/Linux only.  Must be adapted for
dnl other platforms.
HAVE_DBUS=no
DBUS_OBJ=
if test "${with_dbus}" = "yes"; then
   EMACS_CHECK_MODULES([DBUS], [dbus-1 >= 1.0])
   if test "$HAVE_DBUS" = yes; then
     AC_DEFINE(HAVE_DBUS, 1, [Define to 1 if using D-Bus.])
     dnl dbus_watch_get_unix_fd has been introduced in D-Bus 1.1.1.
     dnl dbus_type_is_valid and dbus_validate_* have been introduced in
     dnl D-Bus 1.5.12.
     OLD_LIBS=$LIBS
     LIBS="$LIBS $DBUS_LIBS"
     AC_CHECK_FUNCS(dbus_watch_get_unix_fd \
		    dbus_type_is_valid \
		    dbus_validate_bus_name \
                    dbus_validate_path \
		    dbus_validate_interface \
		    dbus_validate_member)
     LIBS=$OLD_LIBS
     DBUS_OBJ=dbusbind.o
   fi
fi
AC_SUBST(DBUS_CFLAGS)
AC_SUBST(DBUS_LIBS)
AC_SUBST(DBUS_OBJ)

dnl GSettings has been tested under GNU/Linux only.
HAVE_GSETTINGS=no
if test "${HAVE_X11}" = "yes" && test "${with_gsettings}" = "yes"; then
   EMACS_CHECK_MODULES([GSETTINGS], [gio-2.0 >= 2.26])
   if test "$HAVE_GSETTINGS" = "yes"; then
      old_CFLAGS=$CFLAGS
      CFLAGS="$CFLAGS $GSETTINGS_CFLAGS"
      old_LIBS=$LIBS
      LIBS="$LIBS $GSETTINGS_LIBS"
      AC_CACHE_CHECK([whether GSettings is in gio], [emacs_cv_gsettings_in_gio],
      [AC_LINK_IFELSE(
         [AC_LANG_PROGRAM(
	     [[/* Check that gsettings really is present.  */
	     #include <glib-object.h>
	     #include <gio/gio.h>
	     ]],
	     [[
	       GSettings *settings;
	       GVariant *val = g_settings_get_value (settings, "");
	     ]])],
       [emacs_cv_gsettings_in_gio=yes], [emacs_cv_gsettings_in_gio=no])])

      if test "$emacs_cv_gsettings_in_gio" = "yes"; then
        AC_DEFINE(HAVE_GSETTINGS, 1, [Define to 1 if using GSettings.])
	SETTINGS_CFLAGS="$GSETTINGS_CFLAGS"
	SETTINGS_LIBS="$GSETTINGS_LIBS"
	test "$with_gconf" = "yes" || with_gconf=no
      fi
      CFLAGS=$old_CFLAGS
      LIBS=$old_LIBS
   fi
fi

dnl GConf has been tested under GNU/Linux only.
dnl The version is really arbitrary, it is about the same age as Gtk+ 2.6.
HAVE_GCONF=no
if test "${HAVE_X11}" = "yes" && test "${with_gconf}" != "no"; then
   EMACS_CHECK_MODULES([GCONF], [gconf-2.0 >= 2.13])
   if test "$HAVE_GCONF" = yes; then
      AC_DEFINE(HAVE_GCONF, 1, [Define to 1 if using GConf.])
      dnl Newer GConf doesn't link with g_objects, so this is not defined.
      SETTINGS_CFLAGS="$SETTINGS_CFLAGS $GCONF_CFLAGS"
      SETTINGS_LIBS="$SETTINGS_LIBS $GCONF_LIBS"
   fi
fi

if test "$HAVE_GSETTINGS" = "yes" || test "$HAVE_GCONF" = "yes"; then
    EMACS_CHECK_MODULES([GOBJECT], [gobject-2.0 >= 2.0])
    if test "$HAVE_GOBJECT" = "yes"; then
       SETTINGS_CFLAGS="$SETTINGS_CFLAGS $GOBJECT_CFLAGS"
       SETTINGS_LIBS="$SETTINGS_LIBS $GOBJECT_LIBS"
    fi
    SAVE_CFLAGS="$CFLAGS"
    SAVE_LIBS="$LIBS"
    CFLAGS="$SETTINGS_CFLAGS $CFLAGS"
    LIBS="$SETTINGS_LIBS $LIBS"
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
if test "${with_gnutls}" != "no" ; then
  EMACS_CHECK_MODULES([LIBGNUTLS], [gnutls >= 2.12.2],
    [HAVE_GNUTLS=yes], [HAVE_GNUTLS=no])
  if test "${HAVE_GNUTLS}" = "yes"; then
    AC_DEFINE(HAVE_GNUTLS, 1, [Define if using GnuTLS.])
  fi

  # Windows loads GnuTLS dynamically
  if test "${opsys}" = "mingw32"; then
    LIBGNUTLS_LIBS=
  fi
fi

AC_SUBST(LIBGNUTLS_LIBS)
AC_SUBST(LIBGNUTLS_CFLAGS)

HAVE_LIBSYSTEMD=no
if test "${with_libsystemd}" = "yes" ; then
  dnl This code has been tested with libsystemd 222 and later.
  dnl FIXME: Find the earliest version number for which Emacs should work,
  dnl and change '222' to that number.
  EMACS_CHECK_MODULES([LIBSYSTEMD], [libsystemd >= 222],
    [HAVE_LIBSYSTEMD=yes], [HAVE_LIBSYSTEMD=no])
  if test "${HAVE_LIBSYSTEMD}" = "yes"; then
    AC_DEFINE(HAVE_LIBSYSTEMD, 1, [Define if using libsystemd.])
  fi
fi

AC_SUBST(LIBSYSTEMD_LIBS)
AC_SUBST(LIBSYSTEMD_CFLAGS)

HAVE_JSON=no
JSON_OBJ=

if test "${with_json}" = yes; then
  EMACS_CHECK_MODULES([JSON], [jansson >= 2.7],
    [HAVE_JSON=yes], [HAVE_JSON=no])
  if test "${HAVE_JSON}" = yes; then
    AC_DEFINE(HAVE_JSON, 1, [Define if using Jansson.])
    JSON_OBJ=json.o
  fi

  # Windows loads libjansson dynamically
  if test "${opsys}" = "mingw32"; then
    JSON_LIBS=
  fi
fi

AC_SUBST(JSON_LIBS)
AC_SUBST(JSON_CFLAGS)
AC_SUBST(JSON_OBJ)

NOTIFY_OBJ=
NOTIFY_SUMMARY=no

dnl MS Windows native file monitor is available for mingw32 only.
case $with_file_notification,$opsys in
  w32,cygwin)
    AC_MSG_ERROR(['--with-file-notification=w32' was specified, but
    this is only supported on MS-Windows native and MinGW32 builds.
    Consider using gfile instead.])
    ;;
  w32,* | yes,mingw32)
    AC_CHECK_HEADER(windows.h)
    if test "$ac_cv_header_windows_h" = yes ; then
       AC_DEFINE(HAVE_W32NOTIFY, 1, [Define to 1 to use w32notify.])
       NOTIFY_OBJ=w32notify.o
       NOTIFY_SUMMARY="yes (w32)"
    fi ;;
esac

dnl inotify is available only on GNU/Linux.
case $with_file_notification,$NOTIFY_OBJ in
  inotify, | yes,)
    AC_CHECK_HEADER(sys/inotify.h)
    if test "$ac_cv_header_sys_inotify_h" = yes ; then
	AC_CHECK_FUNC(inotify_init1)
	if test "$ac_cv_func_inotify_init1" = yes; then
	  AC_DEFINE(HAVE_INOTIFY, 1, [Define to 1 to use inotify.])
	  NOTIFY_OBJ=inotify.o
	  NOTIFY_SUMMARY="yes -lglibc (inotify)"
       fi
    fi ;;
esac

dnl kqueue is available on BSD-like systems.
case $with_file_notification,$NOTIFY_OBJ in
  kqueue,* | yes,)
    EMACS_CHECK_MODULES([KQUEUE], [libkqueue])
    if test "$HAVE_KQUEUE" = "yes"; then
       AC_DEFINE(HAVE_KQUEUE, 1, [Define to 1 to use kqueue.])
       CPPFLAGS="$CPPFLAGS -I/usr/include/kqueue"
       NOTIFY_CFLAGS=$KQUEUE_CFLAGS
       NOTIFY_LIBS=$KQUEUE_LIBS
       NOTIFY_OBJ=kqueue.o
       NOTIFY_SUMMARY="yes -lkqueue"
    else
       AC_SEARCH_LIBS(kqueue, [])
       if test "$ac_cv_search_kqueue" != no; then
         AC_DEFINE(HAVE_KQUEUE, 1, [Define to 1 to use kqueue.])
	 NOTIFY_OBJ=kqueue.o
	 NOTIFY_SUMMARY="yes (kqueue)"
       fi
    fi ;;
esac

dnl g_file_monitor exists since glib 2.18.  G_FILE_MONITOR_EVENT_MOVED
dnl has been added in glib 2.24.  It has been tested under
dnl GNU/Linux only.
case $with_file_notification,$NOTIFY_OBJ in
  gfile,* | yes,)
    if test "${HAVE_NS}" = yes; then
       AC_MSG_ERROR(['--with-file-notification=gfile' is not supported in NextStep builds.
Consider kqueue instead.])
    else
       EMACS_CHECK_MODULES([GFILENOTIFY], [gio-2.0 >= 2.24])
       if test "$HAVE_GFILENOTIFY" = "yes"; then
	  AC_DEFINE(HAVE_GFILENOTIFY, 1, [Define to 1 if using GFile.])
	  NOTIFY_CFLAGS=$GFILENOTIFY_CFLAGS
	  NOTIFY_LIBS=$GFILENOTIFY_LIBS
	  NOTIFY_OBJ=gfilenotify.o
	  NOTIFY_SUMMARY="yes -lgio (gfile)"
       fi
    fi ;;
esac

case $with_file_notification,$NOTIFY_OBJ in
  yes,* | no,* | *,?*) ;;
  *) AC_MSG_ERROR([File notification '$with_file_notification' requested but requirements not found.]) ;;
esac

if test -n "$NOTIFY_OBJ"; then
   AC_DEFINE(USE_FILE_NOTIFY, 1, [Define to 1 if using file notifications.])
fi
AC_SUBST(NOTIFY_CFLAGS)
AC_SUBST(NOTIFY_LIBS)
AC_SUBST(NOTIFY_OBJ)

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
LIBXMU=
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
    OTHERLIBS='-lXt -lSM -lICE'
  else
    OTHERLIBS='-lXt'
  fi
  AC_SEARCH_LIBS([XmuConvertStandardSelection], [Xmu], [], [], [$OTHERLIBS])
  if test "X$LIBS" != "X$OLDLIBS"; then
    LIBXMU=$ac_cv_search_XmuConvertStandardSelection
  fi
  LIBS=$OLDLIBS
  dnl ac_cv_search_XmuConvertStandardSelection is also referenced below.
fi
AC_SUBST(LIBXTR6)
AC_SUBST(LIBXMU)

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

dnl Note this is non-standard.  --with-xim does not control whether
dnl XIM support is compiled in, it only affects the runtime default of
dnl use_xim in xterm.c.
if test "${with_xim}" != "no"; then
  AC_DEFINE(USE_XIM, 1,
	    [Define to 1 to default runtime use of XIM to on.])
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

# Check for XRender
HAVE_XRENDER=no
if test "${HAVE_X11}" = "yes"; then
  AC_CHECK_HEADER([X11/extensions/Xrender.h],
    [AC_CHECK_LIB([Xrender], [XRenderQueryExtension], [HAVE_XRENDER=yes])])
  if test $HAVE_XRENDER = yes; then
    XRENDER_LIBS="-lXrender"
    AC_SUBST(XRENDER_LIBS)
    AC_DEFINE([HAVE_XRENDER], 1, [Define to 1 if XRender is available.])
  fi
fi

HAVE_CAIRO=no
if test "${HAVE_X11}" = "yes"; then
  if test "${with_cairo}" != "no"; then
    CAIRO_REQUIRED=1.8.0
    CAIRO_MODULE="cairo >= $CAIRO_REQUIRED"
    EMACS_CHECK_MODULES(CAIRO, $CAIRO_MODULE)
    if test $HAVE_CAIRO = yes; then
      AC_DEFINE(USE_CAIRO, 1, [Define to 1 if using cairo.])
    else
      AC_MSG_ERROR([cairo requested but not found.])
    fi

    CFLAGS="$CFLAGS $CAIRO_CFLAGS"
    LIBS="$LIBS $CAIRO_LIBS"
    AC_SUBST(CAIRO_CFLAGS)
    AC_SUBST(CAIRO_LIBS)
  fi
fi

### Start of font-backend (under any platform) section.
# (nothing here yet -- this is a placeholder)
### End of font-backend (under any platform) section.

### Start of font-backend (under X11) section.
if test "${HAVE_X11}" = "yes"; then
  if test $HAVE_CAIRO = yes; then
    dnl Strict linkers fail with
    dnl ftfont.o: undefined reference to symbol 'FT_New_Face'
    dnl if -lfreetype is not specified.
    dnl The following is needed to set FREETYPE_LIBS.
    EMACS_CHECK_MODULES([FREETYPE], [freetype2])

    test "$HAVE_FREETYPE" = "no" && AC_MSG_ERROR(cairo requires libfreetype)

    EMACS_CHECK_MODULES([FONTCONFIG], [fontconfig >= 2.2.0])

    test "$HAVE_FONTCONFIG" = "no" && AC_MSG_ERROR(cairo requires libfontconfig)
    dnl For the "Does Emacs use" message at the end.
    HAVE_XFT=no
  else
    ## Use -lXft if available, unless '--with-xft=no'.
    HAVE_XFT=maybe
    if test "x${with_x}" = "xno"; then
      with_xft="no";
    fi

    if test "$with_xft" != no; then
      EMACS_CHECK_MODULES([FONTCONFIG], [fontconfig >= 2.2.0])
      with_xft=$HAVE_FONTCONFIG
    fi

    if test "x${with_xft}" != "xno"; then

      EMACS_CHECK_MODULES([XFT], [xft >= 0.13.0], [], [HAVE_XFT=no])
      ## Because xterm.c uses XRenderQueryExtension when XFT is
      ## enabled, we also need to link to -lXrender.
      if test "$HAVE_XFT" != no && test "$HAVE_XRENDER" != no; then
	OLD_CPPFLAGS="$CPPFLAGS"
	OLD_CFLAGS="$CFLAGS"
	OLD_LIBS="$LIBS"
	CPPFLAGS="$CPPFLAGS $XFT_CFLAGS"
	CFLAGS="$CFLAGS $XFT_CFLAGS"
	LIBS="$XFT_LIBS $LIBS"
	AC_CHECK_HEADER(X11/Xft/Xft.h,
	  AC_CHECK_LIB(Xft, XftFontOpen, HAVE_XFT=yes, , $XFT_LIBS) , ,
          [[#include <X11/X.h>]])

	if test "${HAVE_XFT}" = "yes"; then
	  AC_DEFINE(HAVE_XFT, 1, [Define to 1 if you have the Xft library.])
	    AC_SUBST(XFT_LIBS)
	  C_SWITCH_X_SITE="$C_SWITCH_X_SITE $XFT_CFLAGS"
	fi                        # "${HAVE_XFT}" = "yes"
	CPPFLAGS=$OLD_CPPFLAGS
	CFLAGS=$OLD_CFLAGS
	LIBS=$OLD_LIBS
      else
      # Make sure XFT is disabled if we found XFT but not XRender
	HAVE_XFT=no
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
       EMACS_CHECK_MODULES([FREETYPE], [freetype2])

       test "$HAVE_FREETYPE" = "no" && AC_MSG_ERROR(libxft requires libfreetype)
    fi
  fi				  # $HAVE_CAIRO != yes

  HAVE_LIBOTF=no
  if test "${HAVE_FREETYPE}" = "yes"; then
    AC_DEFINE(HAVE_FREETYPE, 1,
	      [Define to 1 if using the freetype and fontconfig libraries.])
    OLD_CFLAGS=$CFLAGS
    OLD_LIBS=$LIBS
    CFLAGS="$CFLAGS $FREETYPE_CFLAGS"
    LIBS="$FREETYPE_LIBS $LIBS"
    AC_CHECK_FUNCS(FT_Face_GetCharVariantIndex)
    CFLAGS=$OLD_CFLAGS
    LIBS=$OLD_LIBS
    if test "${with_libotf}" != "no"; then
      EMACS_CHECK_MODULES([LIBOTF], [libotf])
      if test "$HAVE_LIBOTF" = "yes"; then
	AC_DEFINE(HAVE_LIBOTF, 1, [Define to 1 if using libotf.])
	AC_CHECK_LIB(otf, OTF_get_variation_glyphs,
		     HAVE_OTF_GET_VARIATION_GLYPHS=yes,
		     HAVE_OTF_GET_VARIATION_GLYPHS=no)
	if test "${HAVE_OTF_GET_VARIATION_GLYPHS}" = "yes"; then
	  AC_DEFINE(HAVE_OTF_GET_VARIATION_GLYPHS, 1,
		    [Define to 1 if libotf has OTF_get_variation_glyphs.])
	fi
	if ! $PKG_CONFIG --atleast-version=0.9.16 libotf; then
	  AC_DEFINE(HAVE_OTF_KANNADA_BUG, 1,
[Define to 1 if libotf is affected by https://debbugs.gnu.org/28110.])
	fi
      fi
    fi
  dnl FIXME should there be an error if HAVE_FREETYPE != yes?
  dnl Does the new font backend require it, or can it work without it?
  fi

  HAVE_M17N_FLT=no
  if test "${HAVE_LIBOTF}" = yes; then
    if test "${with_m17n_flt}" != "no"; then
      EMACS_CHECK_MODULES([M17N_FLT], [m17n-flt])
      if test "$HAVE_M17N_FLT" = "yes"; then
	AC_DEFINE(HAVE_M17N_FLT, 1, [Define to 1 if using libm17n-flt.])
      fi
    fi
  fi
else # "${HAVE_X11}" != "yes"
  HAVE_XFT=no
  HAVE_FREETYPE=no
  HAVE_LIBOTF=no
  HAVE_M17N_FLT=no
fi   # "${HAVE_X11}" != "yes"

HAVE_HARFBUZZ=no
if test "${HAVE_X11}" = "yes" && test "${HAVE_FREETYPE}" = "yes" \
        || test "${HAVE_W32}" = "yes"; then
  if test "${with_harfbuzz}" != "no"; then
    ### On MS-Windows we use hb_font_get_nominal_glyph, which appeared
    ### in HarfBuzz version 1.2.3
    if test "${HAVE_W32}" = "yes"; then
      EMACS_CHECK_MODULES([HARFBUZZ], [harfbuzz >= 1.2.3])
    else
      EMACS_CHECK_MODULES([HARFBUZZ], [harfbuzz >= 0.9.42])
    fi
    if test "$HAVE_HARFBUZZ" = "yes"; then
      AC_DEFINE(HAVE_HARFBUZZ, 1, [Define to 1 if using HarfBuzz.])
      ### mingw32 and Cygwin-w32 don't use -lharfbuzz, since they load
      ### the library dynamically.
      if test "${HAVE_W32}" = "yes"; then
        HARFBUZZ_LIBS=
      fi
    fi
  fi
fi

### End of font-backend section.

AC_SUBST(FREETYPE_CFLAGS)
AC_SUBST(FREETYPE_LIBS)
AC_SUBST(FONTCONFIG_CFLAGS)
AC_SUBST(FONTCONFIG_LIBS)
AC_SUBST(HARFBUZZ_CFLAGS)
AC_SUBST(HARFBUZZ_LIBS)
AC_SUBST(LIBOTF_CFLAGS)
AC_SUBST(LIBOTF_LIBS)
AC_SUBST(M17N_FLT_CFLAGS)
AC_SUBST(M17N_FLT_LIBS)

if test "${HAVE_X11}" = "yes"; then
  AC_CHECK_HEADER(X11/Xlib-xcb.h,
    AC_CHECK_LIB(xcb, xcb_translate_coordinates, HAVE_XCB=yes))
  if test "${HAVE_XCB}" = "yes"; then
    AC_CHECK_LIB(X11-xcb, XGetXCBConnection, HAVE_X11_XCB=yes)
    if test "${HAVE_X11_XCB}" = "yes"; then
      AC_DEFINE(USE_XCB, 1,
[Define to 1 if you have the XCB library and X11-XCB library for mixed
   X11/XCB programming.])
      XCB_LIBS="-lX11-xcb -lxcb"
      AC_SUBST(XCB_LIBS)
    fi
  fi
fi

### Use -lXpm if available, unless '--with-xpm=no'.
### mingw32 doesn't use -lXpm, since it loads the library dynamically.
### The Cygwin-w32 build uses <noX/xpm.h> instead of <X11/xpm.h>, so
### we need to set LDFLAGS accordingly.
HAVE_XPM=no
LIBXPM=
if test "${HAVE_W32}" = "yes" && test "${opsys}" = "cygwin"; then
  if test "${with_xpm}" != "no"; then
    SAVE_LDFLAGS="$LDFLAGS"
    LDFLAGS="$LDFLAGS -L/usr/lib/noX"
    AC_CHECK_HEADER(noX/xpm.h,
      [AC_CHECK_LIB(Xpm, XpmReadFileToImage, HAVE_XPM=yes)])
    if test "${HAVE_XPM}" = "yes"; then
      AC_CACHE_CHECK([for XpmReturnAllocPixels preprocessor define],
      [emacs_cv_cpp_xpm_return_alloc_pixels],
      [AC_EGREP_CPP(no_return_alloc_pixels,
      [#include "noX/xpm.h"
#ifndef XpmReturnAllocPixels
no_return_alloc_pixels
#endif
      ], emacs_cv_cpp_xpm_return_alloc_pixels=no,
      emacs_cv_cpp_xpm_return_alloc_pixels=yes)])

      if test "$emacs_cv_cpp_xpm_return_alloc_pixels" = "no"; then
        HAVE_XPM=no
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
  dnl Avoid Xpm on AIX unless requested, as it crashes; see Bug#17598.
  case $opsys,$with_xpm_set,$with_xpm in
    aix4-2,set,yes) ;;
    aix4-2,*) with_xpm=no;;
  esac

  if test "${with_xpm}" != "no"; then
    AC_CHECK_HEADER(X11/xpm.h,
      [AC_CHECK_LIB(Xpm, XpmReadFileToPixmap, HAVE_XPM=yes, , -lX11)])
    if test "${HAVE_XPM}" = "yes"; then
      AC_CACHE_CHECK([for XpmReturnAllocPixels preprocessor define],
      [emacs_cv_cpp_xpm_return_alloc_pixels],
      [AC_EGREP_CPP(no_return_alloc_pixels,
      [#include "X11/xpm.h"
#ifndef XpmReturnAllocPixels
no_return_alloc_pixels
#endif
      ], emacs_cv_cpp_xpm_return_alloc_pixels=no,
      emacs_cv_cpp_xpm_return_alloc_pixels=yes)])

      if test "$emacs_cv_cpp_xpm_return_alloc_pixels" = "no"; then
	HAVE_XPM=no
      fi
    fi
  fi

  if test "${HAVE_XPM}" = "yes"; then
    AC_DEFINE(HAVE_XPM, 1, [Define to 1 if you have the Xpm library (-lXpm).])
    LIBXPM=-lXpm
  elif test "$opsys,$LUCID_LIBW" = aix4-2,-lXaw; then
    dnl AIX -lXaw needs -lXpm linked too; see Bug#17598 Message#152.
    LIBXPM=-lXpm
  fi
fi

### FIXME: Perhaps regroup to minimize code duplication due to MinGW's
### slightly different requirements wrt image libraries (it doesn't
### use -lXpm because it loads the xpm shared library dynamically at
### run time).
if test "${opsys}" = "mingw32"; then
  if test "${with_xpm}" != "no"; then
    AC_CHECK_HEADER(X11/xpm.h, HAVE_XPM=yes, HAVE_XPM=no, [
#define FOR_MSW 1])
  fi

  if test "${HAVE_XPM}" = "yes"; then
    AC_DEFINE(HAVE_XPM, 1, [Define to 1 if you have the Xpm library (-lXpm).])
  fi
fi

AC_SUBST(LIBXPM)

### Use -ljpeg if available, unless '--with-jpeg=no'.
HAVE_JPEG=no
LIBJPEG=
if test "${NS_IMPL_COCOA}" = yes; then
  : # Cocoa provides its own jpeg support, so do nothing.
elif test "${HAVE_X11}" = "yes" || test "${HAVE_W32}" = "yes"; then
  if test "${with_jpeg}" != "no"; then
    AC_CACHE_CHECK([for jpeglib 6b or later],
      [emacs_cv_jpeglib],
      [OLD_LIBS=$LIBS
       for emacs_cv_jpeglib in yes -ljpeg no; do
	 case $emacs_cv_jpeglib in
	   yes) ;;
	   no) break;;
	   *) LIBS="$LIBS $emacs_cv_jpeglib";;
	 esac
	 AC_LINK_IFELSE(
	   [AC_LANG_PROGRAM(
	      [[#undef HAVE_STDLIB_H /* Avoid config.h/jpeglib.h collision.  */
		#include <stdio.h> /* jpeglib.h needs FILE and size_t.  */
		#include <jpeglib.h>
		#include <jerror.h>
		char verify[JPEG_LIB_VERSION < 62 ? -1 : 1];
		struct jpeg_decompress_struct cinfo;
	      ]],
	      [[
		jpeg_create_decompress (&cinfo);
		WARNMS (&cinfo, JWRN_JPEG_EOF);
		jpeg_destroy_decompress (&cinfo);
	      ]])],
	   [emacs_link_ok=yes],
	   [emacs_link_ok=no])
	 LIBS=$OLD_LIBS
	 test $emacs_link_ok = yes && break
       done])
    if test "$emacs_cv_jpeglib" != no; then
      HAVE_JPEG=yes
      AC_DEFINE([HAVE_JPEG], 1,
	[Define to 1 if you have the jpeg library (typically -ljpeg).])
      ### mingw32 doesn't use -ljpeg, since it loads the library
      ### dynamically when needed, and doesn't want a run-time
      ### dependency on the jpeglib DLL.
      test "$emacs_cv_jpeglib" != yes && test "${opsys}" != "mingw32" \
      && LIBJPEG=$emacs_cv_jpeglib
    fi
  fi
fi
AC_SUBST(LIBJPEG)

HAVE_LCMS2=no
LCMS2_CFLAGS=
LCMS2_LIBS=
if test "${with_lcms2}" != "no"; then
  EMACS_CHECK_MODULES([LCMS2], [lcms2])
fi
if test "${HAVE_LCMS2}" = "yes"; then
  AC_DEFINE([HAVE_LCMS2], 1, [Define to 1 if you have the lcms2 library (-llcms2).])
  ### mingw32 doesn't use -llcms2, since it loads the library dynamically.
  if test "${opsys}" = "mingw32"; then
     LCMS2_LIBS=
  fi
fi
AC_SUBST(LCMS2_CFLAGS)
AC_SUBST(LCMS2_LIBS)

HAVE_ZLIB=no
LIBZ=
if test "${with_zlib}" != "no"; then
  OLIBS=$LIBS
  AC_SEARCH_LIBS([inflateEnd], [z], [HAVE_ZLIB=yes])
  LIBS=$OLIBS
  case $ac_cv_search_inflateEnd in
    -*) LIBZ=$ac_cv_search_inflateEnd ;;
  esac
fi
if test "${HAVE_ZLIB}" = "yes"; then
  AC_DEFINE([HAVE_ZLIB], 1, [Define to 1 if you have the zlib library (-lz).])
  ### mingw32 doesn't use -lz, since it loads the library dynamically.
  if test "${opsys}" = "mingw32"; then
     LIBZ=
  fi
fi
AC_SUBST(LIBZ)

### Dynamic modules support
LIBMODULES=
HAVE_MODULES=no
MODULES_OBJ=
case $opsys in
  cygwin|mingw32) MODULES_SUFFIX=".dll" ;;
  *) MODULES_SUFFIX=".so" ;;
esac
if test "${with_modules}" != "no"; then
  case $opsys in
    gnu|gnu-linux)
      LIBMODULES="-ldl"
      HAVE_MODULES=yes
      ;;
    cygwin|mingw32|darwin)
      HAVE_MODULES=yes
      ;;
    *)
      # BSD systems have dlopen in libc.
      AC_CHECK_FUNC([dlopen], [HAVE_MODULES=yes])
      ;;
  esac

  if test "${HAVE_MODULES}" = no; then
    AC_MSG_ERROR([Dynamic modules are not supported on your system])
  else
    SAVE_LIBS=$LIBS
    LIBS="$LIBS $LIBMODULES"
    AC_CHECK_FUNCS([dladdr dlfunc])
    LIBS=$SAVE_LIBS
  fi
fi

if test "${HAVE_MODULES}" = yes; then
   MODULES_OBJ="dynlib.o emacs-module.o"
   AC_DEFINE(HAVE_MODULES, 1, [Define to 1 if dynamic modules are enabled])
   AC_DEFINE_UNQUOTED(MODULES_SUFFIX, "$MODULES_SUFFIX",
     [System extension for dynamic libraries])
fi
AC_SUBST(MODULES_OBJ)
AC_SUBST(LIBMODULES)
AC_SUBST(HAVE_MODULES)
AC_SUBST(MODULES_SUFFIX)

AC_CONFIG_FILES([src/emacs-module.h])
AC_SUBST_FILE([module_env_snippet_25])
AC_SUBST_FILE([module_env_snippet_26])
AC_SUBST_FILE([module_env_snippet_27])
module_env_snippet_25="$srcdir/src/module-env-25.h"
module_env_snippet_26="$srcdir/src/module-env-26.h"
module_env_snippet_27="$srcdir/src/module-env-27.h"
emacs_major_version="${PACKAGE_VERSION%%.*}"
AC_SUBST(emacs_major_version)

### Use -lpng if available, unless '--with-png=no'.
HAVE_PNG=no
LIBPNG=
PNG_CFLAGS=
if test "${NS_IMPL_COCOA}" = yes; then
  : # Cocoa provides its own png support, so do nothing.
elif test "${with_png}" != no; then
  # mingw32 loads the library dynamically.
  if test "$opsys" = mingw32; then
    AC_CHECK_HEADER([png.h], [HAVE_PNG=yes])
  elif test "${HAVE_X11}" = "yes" || test "${HAVE_W32}" = "yes"; then
    EMACS_CHECK_MODULES([PNG], [libpng >= 1.0.0])
    if test $HAVE_PNG = yes; then
      LIBPNG=$PNG_LIBS
    else
      # Test old way in case pkg-config doesn't have it (older machines).
      AC_MSG_CHECKING([for libpng not configured by pkg-config])

      png_cflags=`(libpng-config --cflags) 2>&AS_MESSAGE_LOG_FD` &&
      png_ldflags=`(libpng-config --ldflags) 2>&AS_MESSAGE_LOG_FD` || {
	# libpng-config does not work; configure by hand.
	# Debian unstable as of July 2003 has multiple libpngs, and puts png.h
	# in /usr/include/libpng.
	if test -r /usr/include/libpng/png.h &&
	   test ! -r /usr/include/png.h; then
	  png_cflags=-I/usr/include/libpng
	else
	  png_cflags=
	fi
	png_ldflags='-lpng'
      }
      SAVE_CFLAGS=$CFLAGS
      SAVE_LIBS=$LIBS
      CFLAGS="$CFLAGS $png_cflags"
      LIBS="$png_ldflags -lz -lm $LIBS"
      AC_LINK_IFELSE(
	[AC_LANG_PROGRAM([[#include <png.h>]],
	   [[return !png_get_channels (0, 0);]])],
	[HAVE_PNG=yes
	 PNG_CFLAGS=`AS_ECHO(["$png_cflags"]) | sed -e "$edit_cflags"`
	 LIBPNG=$png_ldflags])
      CFLAGS=$SAVE_CFLAGS
      LIBS=$SAVE_LIBS
      AC_MSG_RESULT([$HAVE_PNG])
    fi

    # $LIBPNG requires explicit -lz in some cases.
    # We don't know what those cases are, exactly, so play it safe and
    # append -lz to any nonempty $LIBPNG, unless we're already using LIBZ.
    case " $LIBPNG ",$LIBZ in
      *' -lz '*, | *' ',?*) ;;
      *) LIBPNG="$LIBPNG -lz" ;;
    esac
  fi
fi
if test $HAVE_PNG = yes; then
  AC_DEFINE([HAVE_PNG], [1], [Define to 1 if you have the png library.])

  SAVE_CFLAGS=$CFLAGS
  CFLAGS="$CFLAGS $PNG_CFLAGS"
  AC_CHECK_DECL([png_longjmp],
    [],
    [AC_DEFINE([PNG_DEPSTRUCT], [],
       [Define to empty to suppress deprecation warnings when building
	with --enable-gcc-warnings and with libpng versions before 1.5,
	which lack png_longjmp.])],
    [[#include <png.h>
    ]])
  CFLAGS=$SAVE_CFLAGS
fi
AC_SUBST(LIBPNG)
AC_SUBST(PNG_CFLAGS)

### Use -ltiff if available, unless '--with-tiff=no'.
### mingw32 doesn't use -ltiff, since it loads the library dynamically.
HAVE_TIFF=no
LIBTIFF=
if test "${opsys}" = "mingw32"; then
  if test "${with_tiff}" != "no"; then
    AC_CHECK_HEADER(tiffio.h, HAVE_TIFF=yes, HAVE_TIFF=no)
  fi
  if test "${HAVE_TIFF}" = "yes"; then
    AC_DEFINE(HAVE_TIFF, 1, [Define to 1 if you have the tiff library (-ltiff).])
  fi
elif test "${HAVE_X11}" = "yes" || test "${HAVE_W32}" = "yes"; then
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

### Use -lgif or -lungif if available, unless '--with-gif=no'.
### mingw32 doesn't use -lgif/-lungif, since it loads the library dynamically.
HAVE_GIF=no
LIBGIF=
if test "${opsys}" = "mingw32"; then
  if test "${with_gif}" != "no"; then
    AC_CHECK_HEADER(gif_lib.h, HAVE_GIF=yes, HAVE_GIF=no)
  fi
  if test "${HAVE_GIF}" = "yes"; then
    AC_DEFINE(HAVE_GIF, 1, [Define to 1 if you have a gif (or ungif) library.])
  fi
elif test "${HAVE_X11}" = "yes" && test "${with_gif}" != "no" \
        || test "${HAVE_W32}" = "yes"; then
  AC_CHECK_HEADER(gif_lib.h,
# EGifPutExtensionLast only exists from version libungif-4.1.0b1.
# Earlier versions can crash Emacs, but version 5.0 removes EGifPutExtensionLast.
    [AC_CHECK_LIB(gif, GifMakeMapObject, HAVE_GIF=yes,
        [AC_CHECK_LIB(gif, EGifPutExtensionLast, HAVE_GIF=yes, HAVE_GIF=maybe)])])

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
MISSING=
WITH_IFAVAILABLE=
if test "${HAVE_X11}" = "yes"; then
  case $with_xpm,$HAVE_XPM in
    no,* | ifavailable,* | *,yes) ;;
    *) MISSING="libXpm"
       WITH_IFAVAILABLE="--with-xpm=ifavailable";;
  esac
  case $with_jpeg,$HAVE_JPEG in
    no,* | ifavailable,* | *,yes) ;;
    *) MISSING="$MISSING libjpeg"
       WITH_IFAVAILABLE="$WITH_IFAVAILABLE --with-jpeg=ifavailable";;
  esac
  case $with_png,$HAVE_PNG in
    no,* | ifavailable,* | *,yes) ;;
    *) MISSING="$MISSING libpng"
       WITH_IFAVAILABLE="$WITH_IFAVAILABLE --with-png=ifavailable";;
  esac
  case $with_gif,$HAVE_GIF in
    no,* | ifavailable,* | *,yes) ;;
    *) MISSING="$MISSING libgif/libungif"
       WITH_IFAVAILABLE="$WITH_IFAVAILABLE --with-gif=ifavailable";;
  esac
  case $with_tiff,$HAVE_TIFF in
    no,* | ifavailable,* | *,yes) ;;
    *) MISSING="$MISSING libtiff"
       WITH_IFAVAILABLE="$WITH_IFAVAILABLE --with-tiff=ifavailable";;
  esac
fi
case $with_gnutls,$HAVE_GNUTLS in
  no,* | ifavailable,* | *,yes) ;;
  *) MISSING="$MISSING gnutls"
     WITH_IFAVAILABLE="$WITH_IFAVAILABLE --with-gnutls=ifavailable";;
esac
if test "X${MISSING}" != X; then
  AC_MSG_ERROR([The following required libraries were not found:
    $MISSING
Maybe some development libraries/packages are missing?
To build anyway, give:
    $WITH_IFAVAILABLE
as options to configure.])
fi

### Use -lgpm if available, unless '--with-gpm=no'.
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
  AC_DEFINE(HAVE_NS, 1, [Define to 1 if you are using the NeXTstep API, either GNUstep or Cocoa on macOS.])
  if test "${NS_IMPL_COCOA}" = "yes"; then
    AC_DEFINE(NS_IMPL_COCOA, 1, [Define to 1 if you are using NS windowing under macOS.])
  fi
  if test "${NS_IMPL_GNUSTEP}" = "yes"; then
    AC_DEFINE(NS_IMPL_GNUSTEP, 1, [Define to 1 if you are using NS windowing under GNUstep.])
    if test $NS_GNUSTEP_CONFIG != yes; then
      # See also .m.o rule in src/Makefile.in.  */
      # FIXME: are all these flags really needed?  Document here why.  */
      GNUSTEP_CFLAGS="-D_REENTRANT -fPIC -fno-strict-aliasing -I${GNUSTEP_SYSTEM_HEADERS} ${GNUSTEP_LOCAL_HEADERS}"
      ## Extra CFLAGS applied to src/*.m files.
      GNU_OBJC_CFLAGS="$GNU_OBJC_CFLAGS -fgnu-runtime -Wno-import -fconstant-string-class=NSConstantString -DGNUSTEP_BASE_LIBRARY=1 -DGNU_GUI_LIBRARY=1 -DGNU_RUNTIME=1 -DGSWARN -DGSDIAGNOSE"
    fi
  fi
  OTHER_FILES=ns-app
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
  fi
fi
AC_SUBST(LIBXSM)

### Use XRandr (-lXrandr) if available
HAVE_XRANDR=no
if test "${HAVE_X11}" = "yes"; then
  XRANDR_REQUIRED=1.2.2
  XRANDR_MODULES="xrandr >= $XRANDR_REQUIRED"
  EMACS_CHECK_MODULES([XRANDR], [$XRANDR_MODULES])
  if test $HAVE_XRANDR = no; then
    # Test old way in case pkg-config doesn't have it (older machines).
    # Include Xrender.h by hand to work around bug in older Xrandr.h
    # (e.g. RHEL5) and silence (harmless) configure warning (bug#18465).
    AC_CHECK_HEADER(X11/extensions/Xrandr.h,
      [AC_CHECK_LIB(Xrandr, XRRGetScreenResources, HAVE_XRANDR=yes)],
      [], [AC_INCLUDES_DEFAULT
#include <X11/extensions/Xrender.h>])
    if test $HAVE_XRANDR = yes; then
      XRANDR_LIBS=-lXrandr
    fi
  fi
  if test $HAVE_XRANDR = yes; then
    AC_DEFINE(HAVE_XRANDR, 1, [Define to 1 if you have the XRandr extension.])
  fi
fi
AC_SUBST(XRANDR_CFLAGS)
AC_SUBST(XRANDR_LIBS)

### Use Xinerama (-lXinerama) if available
HAVE_XINERAMA=no
if test "${HAVE_X11}" = "yes"; then
  XINERAMA_REQUIRED=1.0.2
  XINERAMA_MODULES="xinerama >= $XINERAMA_REQUIRED"
  EMACS_CHECK_MODULES([XINERAMA], [$XINERAMA_MODULES])
  if test $HAVE_XINERAMA = no; then
    # Test old way in case pkg-config doesn't have it (older machines).
    AC_CHECK_HEADER(X11/extensions/Xinerama.h,
      [AC_CHECK_LIB(Xinerama, XineramaQueryExtension, HAVE_XINERAMA=yes)])
    if test $HAVE_XINERAMA = yes; then
      XINERAMA_LIBS=-lXinerama
    fi
  fi
  if test $HAVE_XINERAMA = yes; then
    AC_DEFINE(HAVE_XINERAMA, 1, [Define to 1 if you have the Xinerama extension.])
  fi
fi
AC_SUBST(XINERAMA_CFLAGS)
AC_SUBST(XINERAMA_LIBS)

### Use Xfixes (-lXfixes) if available
HAVE_XFIXES=no
if test "${HAVE_X11}" = "yes"; then
  XFIXES_REQUIRED=4.0.0
  XFIXES_MODULES="xfixes >= $XFIXES_REQUIRED"
  EMACS_CHECK_MODULES([XFIXES], [$XFIXES_MODULES])
  if test $HAVE_XFIXES = no; then
    # Test old way in case pkg-config doesn't have it (older machines).
    AC_CHECK_HEADER(X11/extensions/Xfixes.h,
      [AC_CHECK_LIB(Xfixes, XFixesHideCursor, HAVE_XFIXES=yes)])
    if test $HAVE_XFIXES = yes; then
      XFIXES_LIBS=-lXfixes
    fi
  fi
  if test $HAVE_XFIXES = yes; then
    AC_DEFINE(HAVE_XFIXES, 1, [Define to 1 if you have the Xfixes extension.])
  fi
fi
AC_SUBST(XFIXES_CFLAGS)
AC_SUBST(XFIXES_LIBS)

### Use Xdbe (-lXdbe) if available
HAVE_XDBE=no
if test "${HAVE_X11}" = "yes"; then
  AC_CHECK_HEADER(X11/extensions/Xdbe.h,
    [AC_CHECK_LIB(Xext, XdbeAllocateBackBufferName, HAVE_XDBE=yes)],
    [],
    [#include <X11/Xlib.h>
    ])
  if test $HAVE_XDBE = yes; then
    XDBE_LIBS=-lXext
  fi
  if test $HAVE_XDBE = yes; then
    AC_DEFINE(HAVE_XDBE, 1, [Define to 1 if you have the Xdbe extension.])
  fi
fi
AC_SUBST(XDBE_CFLAGS)
AC_SUBST(XDBE_LIBS)

### Use libxml (-lxml2) if available
### mingw32 doesn't use -lxml2, since it loads the library dynamically.
HAVE_LIBXML2=no
if test "${with_xml2}" != "no"; then
  ### I'm not sure what the version number should be, so I just guessed.
  EMACS_CHECK_MODULES([LIBXML2], [libxml-2.0 > 2.6.17])
  # Built-in libxml2 on OS X 10.8 lacks libxml-2.0.pc.
  if test "${HAVE_LIBXML2}" != "yes" && test "$opsys" = "darwin"; then
    SAVE_CPPFLAGS="$CPPFLAGS"
    if test -z "$xcsdkdir" -a -n "$XCRUN" -a ! -d /usr/include; then
      dnl /usr/include is not found.  Try Xcode SDK dir if it is sane.
      xcsdkdir=`$XCRUN --show-sdk-path 2>/dev/null`
      case $xcsdkdir in
	*[[\\\"\#\$\&\'\`$am_lf\ \	]]*)
	xcsdkdir="" ;;
      esac
    fi
    CPPFLAGS="$CPPFLAGS -isystem${xcsdkdir}/usr/include/libxml2"
    AC_CHECK_HEADER(libxml/HTMLparser.h,
      [AC_CHECK_DECL(HTML_PARSE_RECOVER, HAVE_LIBXML2=yes, ,
		     [#include <libxml/HTMLparser.h>])])
    CPPFLAGS="$SAVE_CPPFLAGS"
    if test "${HAVE_LIBXML2}" = "yes"; then
      LIBXML2_CFLAGS="-isystem${xcsdkdir}/usr/include/libxml2"
      LIBXML2_LIBS="-lxml2"
    fi
  fi
  if test "${HAVE_LIBXML2}" = "yes"; then
    if test "${opsys}" != "mingw32"; then
      AC_CHECK_LIB(xml2, htmlReadMemory, HAVE_LIBXML2=yes, HAVE_LIBXML2=no,
        [$LIBXML2_LIBS])
    else
      LIBXML2_LIBS=""
    fi
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

BLESSMAIL_TARGET=
LIBS_MAIL=
if test ! "$with_mailutils"; then
  # Check for mail-locking functions in a "mail" library.  Probably this should
  # have the same check as for liblockfile below.
  AC_CHECK_LIB([mail], [maillock], [have_mail=yes], [have_mail=no])
  if test $have_mail = yes; then
    LIBS_MAIL=-lmail
    AC_DEFINE([HAVE_LIBMAIL], [1],
      [Define to 1 if you have the 'mail' library (-lmail).])

    OLD_LIBS=$LIBS
    LIBS="$LIBS_MAIL $LIBS"
    AC_CHECK_FUNCS([touchlock])
    LIBS=$OLD_LIBS
  fi
  dnl Debian, at least:
  AC_CHECK_LIB([lockfile], [maillock], [have_lockfile=yes], [have_lockfile=no])
  if test $have_lockfile = yes; then
     LIBS_MAIL=-llockfile
     AC_DEFINE([HAVE_LIBLOCKFILE], [1],
       [Define to 1 if you have the 'lockfile' library (-llockfile).])
  else
    # If we have the shared liblockfile, assume we must use it for mail
    # locking (e.g. Debian).  If we couldn't link against liblockfile
    # (no liblockfile.a installed), ensure that we don't need to.
    dnl This works for files generally, not just executables.
    dnl Should we look elsewhere for it?  Maybe examine /etc/ld.so.conf?
    AC_CHECK_PROG([liblockfile], [liblockfile.so], [yes], [no],
		  [/usr/lib:/lib:/usr/local/lib:$LD_LIBRARY_PATH])
    if test "$ac_cv_prog_liblockfile" = yes; then
      AC_MSG_ERROR([Shared liblockfile found but can't link against it.
This probably means that movemail could lose mail.
There may be a 'development' package to install containing liblockfile.])
    fi
  fi
  AC_CHECK_HEADERS([maillock.h])

  ## Define MAIL_USE_FLOCK (or LOCKF) if the mailer uses flock (or lockf) to
  ## interlock access to the mail spool.  The alternative is a lock file named
  ## /usr/spool/mail/$USER.lock.
  mail_lock=no
  case $opsys in
    aix4-2) mail_lock="lockf" ;;

    gnu|freebsd|dragonfly|netbsd|openbsd|darwin) mail_lock="flock" ;;

    ## On GNU/Linux systems, both methods are used by various mail programs.
    ## I assume most people are using newer mailers that have heard of flock.
    ## Change this if you need to.
    ## Debian contains a patch which says: "On Debian/GNU/Linux systems,
    ## configure gets the right answers, and that means *NOT* using flock.
    ## Using flock is guaranteed to be the wrong thing. See Debian Policy
    ## for details." and then uses '#ifdef DEBIAN'.  Unfortunately the
    ## Debian maintainer hasn't provided a clean fix for Emacs.
    ## movemail.c will use 'maillock' when MAILDIR, HAVE_LIBMAIL and
    ## HAVE_MAILLOCK_H are defined, so the following appears to be the
    ## correct logic.  -- fx
    ## We must check for HAVE_LIBLOCKFILE too, as movemail does.
    ## liblockfile is a Free Software replacement for libmail, used on
    ## Debian systems and elsewhere. -rfr.
    gnu-*)
      mail_lock="flock"
      if test $have_mail = yes || test $have_lockfile = yes; then
	test $ac_cv_header_maillock_h = yes && mail_lock=no
      fi ;;

    mingw32)
      mail_lock="none-needed" ;;
  esac

  case $mail_lock in
    flock) AC_DEFINE([MAIL_USE_FLOCK], [1],
	     [Define if the mailer uses flock to interlock the mail spool.]);;

    lockf) AC_DEFINE([MAIL_USE_LOCKF], [1],
	     [Define if the mailer uses lockf to interlock the mail spool.]);;

    none-needed) ;;

    *) BLESSMAIL_TARGET="need-blessmail" ;;
  esac
fi
AC_SUBST([BLESSMAIL_TARGET])
AC_SUBST([LIBS_MAIL])

OLD_LIBS=$LIBS
LIBS="$LIB_PTHREAD $LIB_MATH $LIBS"
AC_CHECK_FUNCS(accept4 fchdir gethostname \
getrusage get_current_dir_name \
lrand48 random rint trunc \
select getpagesize setlocale newlocale \
getrlimit setrlimit shutdown \
pthread_sigmask strsignal setitimer timer_getoverrun \
sendto recvfrom getsockname getifaddrs freeifaddrs \
gai_strerror sync \
getpwent endpwent getgrent endgrent \
cfmakeraw cfsetspeed __executable_start log2 pthread_setname_np)
LIBS=$OLD_LIBS

if test "$ac_cv_func_pthread_setname_np" = "yes"; then
  AC_CACHE_CHECK(
   [whether pthread_setname_np takes a single argument],
   [emacs_cv_pthread_setname_np_1arg],
   [AC_COMPILE_IFELSE(
     [AC_LANG_PROGRAM(
       [[#include <pthread.h>]],
       [[pthread_setname_np ("a");]])],
     [emacs_cv_pthread_setname_np_1arg=yes],
     [emacs_cv_pthread_setname_np_1arg=no])])
  if test "$emacs_cv_pthread_setname_np_1arg" = "yes"; then
    AC_DEFINE(
      HAVE_PTHREAD_SETNAME_NP_1ARG, 1,
      [Define to 1 if pthread_setname_np takes a single argument.])
  else
    AC_CACHE_CHECK(
     [whether pthread_setname_np takes three arguments],
     [emacs_cv_pthread_setname_np_3arg],
     [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM(
         [[#include <pthread.h>]],
         [[pthread_setname_np (0, "%s", "a");]])],
       [emacs_cv_pthread_setname_np_3arg=yes],
       [emacs_cv_pthread_setname_np_3arg=no])])
     if test "$emacs_cv_pthread_setname_np_3arg" = "yes"; then
       AC_DEFINE(
         HAVE_PTHREAD_SETNAME_NP_3ARG, 1,
         [Define to 1 if pthread_setname_np takes three arguments.])
     fi
  fi
fi

dnl No need to check for posix_memalign if aligned_alloc works.
AC_CHECK_FUNCS([aligned_alloc posix_memalign], [break])
AC_CHECK_DECLS([aligned_alloc], [], [], [[#include <stdlib.h>]])

# Dump loading
AC_CHECK_FUNCS([posix_madvise])

dnl Cannot use AC_CHECK_FUNCS
AC_CACHE_CHECK([for __builtin_frame_address],
  [emacs_cv_func___builtin_frame_address],
  [AC_LINK_IFELSE([AC_LANG_PROGRAM([], [__builtin_frame_address (0);])],
     [emacs_cv_func___builtin_frame_address=yes],
     [emacs_cv_func___builtin_frame_address=no])])
if test $emacs_cv_func___builtin_frame_address = yes; then
  AC_DEFINE([HAVE___BUILTIN_FRAME_ADDRESS], 1,
	    [Define to 1 if you have the '__builtin_frame_address' function.])
fi
AC_CACHE_CHECK([for __builtin_unwind_init],
	       emacs_cv_func___builtin_unwind_init,
[AC_LINK_IFELSE([AC_LANG_PROGRAM([], [__builtin_unwind_init ();])],
		emacs_cv_func___builtin_unwind_init=yes,
		emacs_cv_func___builtin_unwind_init=no)])
if test $emacs_cv_func___builtin_unwind_init = yes; then
  AC_DEFINE(HAVE___BUILTIN_UNWIND_INIT, 1,
	    [Define to 1 if you have the '__builtin_unwind_init' function.])
fi

AC_CHECK_HEADERS_ONCE(sys/un.h)

AC_FUNC_FSEEKO

# UNIX98 PTYs.
AC_CHECK_FUNCS(grantpt)

# PTY-related GNU extensions.
AC_CHECK_FUNCS(getpt posix_openpt)

dnl Run a test program that contains a call to tputs, a call that is
dnl never executed.  This tests whether a pre-'main' dynamic linker
dnl works with the library.  It's too much trouble to actually call
dnl tputs in the test program, due to portability hassles.  When
dnl cross-compiling, assume the test program will run if it links.
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
# Check this now, so that we will NOT find the above functions in ncurses.
# That is because we have not set up to link ncurses in lib-src.
# It's better to believe a function is not available
# than to expect to find it in ncurses.
# Also we need tputs and friends to be able to build at all.
AC_CACHE_CHECK([for library containing tputs], [emacs_cv_tputs_lib],
[if test "${opsys}" = "mingw32"; then
  emacs_cv_tputs_lib='none required'
else
  # curses precedes termcap because of AIX (Bug#9736#35) and OpenIndiana.
  tputs_libraries='tinfo ncurses terminfo curses termcap tinfow ncursesw'
  for tputs_library in '' $tputs_libraries; do
    OLIBS=$LIBS
    if test -z "$tputs_library"; then
      emacs_cv_tputs_lib='none required'
    else
      emacs_cv_tputs_lib=-l$tputs_library
      LIBS="$emacs_cv_tputs_lib $LIBS"
    fi
    AC_RUN_IFELSE([tputs_link_source], [], [emacs_cv_tputs_lib=no],
      [AC_LINK_IFELSE([tputs_link_source], [], [emacs_cv_tputs_lib=no])])
    LIBS=$OLIBS
    if test "X$emacs_cv_tputs_lib" != Xno; then
      break
    fi
  done
fi])
AS_CASE(["$emacs_cv_tputs_lib"],
  [no], [AC_MSG_ERROR([The required function 'tputs' was not found in any library.
The following libraries were tried (in order):
  libtinfo, libncurses, libterminfo, libcurses, libtermcap
Please try installing whichever of these libraries is most appropriate
for your system, together with its header files.
For example, a libncurses-dev(el) or similar package.])],
  [-l*], [LIBS_TERMCAP=$emacs_cv_tputs_lib],
  [*], [LIBS_TERMCAP=])

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

  mingw32)
    TERMINFO=no
    LIBS_TERMCAP=
    ;;

  netbsd)
    if test "x$LIBS_TERMCAP" != "x-lterminfo"; then
      TERMINFO=no
      LIBS_TERMCAP="-ltermcap"
    fi
    ;;

  openbsd | dragonfly) LIBS_TERMCAP="-lncurses" ;;

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
if test "X$LIBS_TERMCAP" = "X-lncurses"; then
  AC_DEFINE(USE_NCURSES, 1, [Define to 1 if you use ncurses.])
fi
AC_SUBST(LIBS_TERMCAP)
AC_SUBST(TERMCAP_OBJ)

# GNU/Linux-specific timer functions.
AC_CACHE_CHECK([for timerfd interface], [emacs_cv_have_timerfd],
  [AC_COMPILE_IFELSE(
     [AC_LANG_PROGRAM([[#include <sys/timerfd.h>
		      ]],
		      [[timerfd_create (CLOCK_REALTIME,
					TFD_CLOEXEC | TFD_NONBLOCK);
			timerfd_settime (0, TFD_TIMER_ABSTIME, 0, 0);]])],
     [emacs_cv_have_timerfd=yes],
     [emacs_cv_have_timerfd=no])])
if test "$emacs_cv_have_timerfd" = yes; then
  AC_DEFINE([HAVE_TIMERFD], 1,
    [Define to 1 if timerfd functions are supported as in GNU/Linux.])
fi

# Alternate stack for signal handlers.
AC_CACHE_CHECK([whether signals can be handled on alternate stack],
	       [emacs_cv_alternate_stack],
  [AC_COMPILE_IFELSE(
     [AC_LANG_PROGRAM([[#include <signal.h>
		      ]],
		      [[stack_t ss;
			struct sigaction sa;
			ss.ss_sp = malloc (SIGSTKSZ);
			ss.ss_size = SIGSTKSZ;
			sa.sa_flags = SA_SIGINFO | SA_ONSTACK;
			sigaltstack (&ss, 0);
			sigaction (SIGSEGV, &sa, 0);]])],
     [emacs_cv_alternate_stack=yes],
     [emacs_cv_alternate_stack=no])])

# Do we need the Hesiod library to provide the support routines?
dnl FIXME?  Should we be skipping this on Darwin too?
LIBHESIOD=
LIBRESOLV=
if test "$with_hesiod" != no ; then
  # Don't set $LIBS here -- see comments above.  FIXME which comments?
  resolv=no
  AC_CHECK_FUNC(res_send, , [AC_CHECK_FUNC(__res_send, ,
     [AC_CHECK_LIB(resolv, res_send, resolv=yes,
		  [AC_CHECK_LIB(resolv, __res_send, resolv=yes)])])])
  if test "$resolv" = yes ; then
    RESOLVLIB=-lresolv
  else
    RESOLVLIB=
  fi
  hesiod=no
  AC_CHECK_FUNC(hes_getmailhost, , [AC_CHECK_LIB(hesiod, hes_getmailhost,
	hesiod=yes, :, $RESOLVLIB)])

  if test x"$hesiod" = xyes; then
    LIBHESIOD=-lhesiod
    LIBRESOLV=$RESOLVLIB
  fi
fi
AC_SUBST(LIBHESIOD)
AC_SUBST(LIBRESOLV)

# These tell us which Kerberos-related libraries to use.
COM_ERRLIB=
CRYPTOLIB=
KRB5LIB=
DESLIB=
KRB4LIB=

if test "${with_kerberos}" != no; then
  OLD_LIBS=$LIBS
  AC_CHECK_LIB(com_err, com_err, have_com_err=yes, have_com_err=no)
  if test $have_com_err = yes; then
    COM_ERRLIB=-lcom_err
    LIBS="$COM_ERRLIB $LIBS"
  fi
  AC_CHECK_LIB(crypto, mit_des_cbc_encrypt, have_crypto=yes, have_crypto=no)
  if test $have_crypto = yes; then
    CRYPTOLIB=-lcrypto
    LIBS="$CRYPTOLIB $LIBS"
  fi
  AC_CHECK_LIB(k5crypto, mit_des_cbc_encrypt, have_k5crypto=yes, have_k5crypto=no)
  if test $have_k5crypto = yes; then
    CRYPTOLIB=-lk5crypto
    LIBS="$CRYPTOLIB $LIBS"
  fi
  AC_CHECK_LIB(krb5, krb5_init_context, have_krb5=yes, have_krb5=no)
  if test $have_krb5=yes; then
    KRB5LIB=-lkrb5
    LIBS="$KRB5LIB $LIBS"
  fi
  dnl FIXME Simplify.  Does not match 22 logic, thanks to default_off?
  if test "${with_kerberos5}" = no; then
    AC_CHECK_LIB(des425, des_cbc_encrypt, have_des425=yes, have_des425=no )
    if test $have_des425 = yes; then
      DESLIB=-ldes425
      LIBS="$DESLIB $LIBS"
    else
      AC_CHECK_LIB(des, des_cbc_encrypt, have_des=yes, have_des=no)
      if test $have_des = yes; then
        DESLIB=-ldes
        LIBS="$DESLIB $LIBS"
      fi
    fi
    AC_CHECK_LIB(krb4, krb_get_cred, have_krb4=yes, have_krb4=no)
    if test $have_krb4 = yes; then
      KRB4LIB=-lkrb4
      LIBS="$KRB4LIB $LIBS"
    else
      AC_CHECK_LIB(krb, krb_get_cred, have_krb=yes, have_krb=no)
      if test $have_krb = yes; then
        KRB4LIB=-lkrb
        LIBS="$KRB4LIB $LIBS"
      fi
    fi
  fi

  if test "${with_kerberos5}" != no; then
    AC_CHECK_HEADERS(krb5.h,
      [AC_CHECK_MEMBERS([krb5_error.text, krb5_error.e_text],,,
		        [#include <krb5.h>])])
  else
    AC_CHECK_HEADERS(krb.h,,
		     [AC_CHECK_HEADERS(kerberosIV/krb.h,,
				       [AC_CHECK_HEADERS(kerberos/krb.h)])])
  fi
  AC_CHECK_HEADERS(com_err.h)
  LIBS=$OLD_LIBS
fi

AC_SUBST(COM_ERRLIB)
AC_SUBST(CRYPTOLIB)
AC_SUBST(KRB5LIB)
AC_SUBST(DESLIB)
AC_SUBST(KRB4LIB)

AC_ARG_WITH([libgmp],
  [AS_HELP_STRING([--without-libgmp],
		  [don't use the GNU Multiple Precision (GMP) library;
		   this is the default on systems lacking libgmp.])])
GMP_LIB=
GMP_OBJ=mini-gmp-emacs.o
HAVE_GMP=no
case $with_libgmp in
  no) ;;
  yes) HAVE_GMP=yes GMP_LIB=-lgmp;;
  *) AC_CHECK_HEADERS([gmp.h],
       [OLIBS=$LIBS
	AC_SEARCH_LIBS([__gmpz_roinit_n], [gmp])
	LIBS=$OLIBS
	case $ac_cv_search___gmpz_roinit_n in
	  'none needed') HAVE_GMP=yes;;
	  -*) HAVE_GMP=yes GMP_LIB=$ac_cv_search___gmpz_roinit_n;;
	esac]);;
esac
if test "$HAVE_GMP" = yes; then
  GMP_OBJ=
  AC_DEFINE([HAVE_GMP], 1, [Define to 1 if you have recent-enough GMP.])
fi
AC_SUBST([GMP_LIB])
AC_SUBST([GMP_OBJ])

AC_CHECK_HEADERS(valgrind/valgrind.h)

AC_CHECK_MEMBERS([struct unipair.unicode], [], [], [[#include <linux/kd.h>]])

AC_CHECK_FUNCS_ONCE([sbrk])

AC_FUNC_FORK

AC_CHECK_FUNCS(snprintf)

dnl Check for glib.  This differs from other library checks in that
dnl Emacs need not link to glib unless some other library is already
dnl linking to glib.  Although glib provides no facilities that Emacs
dnl needs for its own purposes, when glib is present Emacs needs to
dnl use primitives like g_main_context_query to avoid clashing with
dnl glib at a low level.
dnl
dnl Check this late, since it depends on $GTK_CFLAGS etc.
XGSELOBJ=
AC_CACHE_CHECK([whether GLib is linked in], [emacs_cv_links_glib],
[OLDCFLAGS="$CFLAGS"
OLDLIBS="$LIBS"
CFLAGS="$CFLAGS $GTK_CFLAGS $RSVG_CFLAGS $DBUS_CFLAGS $SETTINGS_CFLAGS"
LIBS="$LIBS $GTK_LIBS $RSVG_LIBS $DBUS_LIBS $SETTINGS_LIBS"
CFLAGS="$CFLAGS $NOTIFY_CFLAGS $CAIRO_CFLAGS"
LIBS="$LIBS $NOTIFY_LIBS $CAIRO_LIBS"
AC_LINK_IFELSE([AC_LANG_PROGRAM(
	[[#include <glib.h>
	]],
	[[g_print ("Hello world");]])],
     [emacs_cv_links_glib=yes],
     [emacs_cv_links_glib=no])
CFLAGS="$OLDCFLAGS"
LIBS="$OLDLIBS"])
if test "${emacs_cv_links_glib}" = "yes"; then
  AC_DEFINE(HAVE_GLIB, 1, [Define to 1 if GLib is linked in.])
  if test "$HAVE_NS" = no;then
    XGSELOBJ=xgselect.o
  fi
fi
AC_SUBST(XGSELOBJ)

dnl Adapted from Haible's version.
AC_CACHE_CHECK([for nl_langinfo and CODESET], [emacs_cv_langinfo_codeset],
  [AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <langinfo.h>]],
    [[char *cs = nl_langinfo(CODESET);]])],
    [emacs_cv_langinfo_codeset=yes],
    [emacs_cv_langinfo_codeset=no])
  ])
if test "$emacs_cv_langinfo_codeset" = yes; then
  AC_DEFINE([HAVE_LANGINFO_CODESET], 1,
    [Define if you have <langinfo.h> and nl_langinfo (CODESET).])

  AC_CACHE_CHECK([for nl_langinfo and _NL_PAPER_WIDTH],
    [emacs_cv_langinfo__nl_paper_width],
    [AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <langinfo.h>]],
      [[char *cs = nl_langinfo (_NL_PAPER_WIDTH);]])],
      [emacs_cv_langinfo__nl_paper_width=yes],
      [emacs_cv_langinfo__nl_paper_width=no])
    ])
  if test "$emacs_cv_langinfo__nl_paper_width" = yes; then
    AC_DEFINE([HAVE_LANGINFO__NL_PAPER_WIDTH], 1,
      [Define if you have <langinfo.h> and nl_langinfo (_NL_PAPER_WIDTH).])
  fi
fi

AC_TYPE_MBSTATE_T

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
dnl MinGW emulates passwd database, so this feature doesn't make sense there.
if test "${opsys}" != "mingw32"; then
   AC_DEFINE(AMPERSAND_FULL_NAME, 1, [Define to use the convention that &
     in the full name stands for the login id.])
fi

dnl Everybody supports this, except MS.
dnl Seems like the kind of thing we should be testing for, though.
## Note: PTYs are broken on darwin <6.  Use at your own risk.
if test "${opsys}" != "mingw32"; then
  AC_DEFINE(HAVE_PTYS, 1, [Define if the system supports pty devices.])
fi

dnl Everybody supports this, except MS-DOS.
dnl Seems like the kind of thing we should be testing for, though.
AC_DEFINE(HAVE_SOCKETS, 1, [Define if the system supports
  4.2-compatible sockets.])

AH_TEMPLATE(INTERNAL_TERMINAL, [This is substituted when $TERM is "internal".])

AH_TEMPLATE(NULL_DEVICE, [Name of the file to open to get
  a null file, or a data sink.])
if test "${opsys}" = "mingw32"; then
  AC_DEFINE(NULL_DEVICE, ["NUL:"])
else
  AC_DEFINE(NULL_DEVICE, ["/dev/null"])
fi

if test "${opsys}" = "mingw32"; then
  SEPCHAR=';'
else
  SEPCHAR=':'
fi
AC_DEFINE_UNQUOTED(SEPCHAR, ['$SEPCHAR'], [Character that separates PATH elements.])
dnl This is for MinGW, and is used in test/Makefile.in.
dnl The MSYS Bash has heuristics for replacing ':' with ';' when it
dnl decides that a command-line argument to be passed to a MinGW program
dnl is a PATH-style list of directories.  But that heuristics plays it
dnl safe, and only does the replacement when it is _absolutely_ sure it
dnl sees a colon-separated list of file names; e.g. ":." is left alone,
dnl which breaks in-tree builds.  So we do this manually instead.
dnl Note that we cannot rely on PATH_SEPARATOR, as that one will always
dnl be computed as ':' in MSYS Bash.
AC_SUBST(SEPCHAR)

dnl Everybody supports this, except MS-DOS.
AC_DEFINE(subprocesses, 1, [Define to enable asynchronous subprocesses.])

AC_DEFINE(USER_FULL_NAME, [pw->pw_gecos], [How to get a user's full name.])


AC_DEFINE(DIRECTORY_SEP, ['/'],
  [Character that separates directories in a file name.])

if test "${opsys}" = "mingw32"; then
  AC_DEFINE(IS_DEVICE_SEP(_c_), [((_c_) == ':')],
    [Returns true if character is a device separator.])

  AC_DEFINE(IS_DIRECTORY_SEP(_c_), [((_c_) == '/' || (_c_) == '\\')],
    [Returns true if character is a directory separator.])

  AC_DEFINE(IS_ANY_SEP(_c_), [(IS_DIRECTORY_SEP (_c_) || IS_DEVICE_SEP(_c_))],
    [Returns true if character is any form of separator.])
else
  AC_DEFINE(IS_DEVICE_SEP(_c_), 0,
    [Returns true if character is a device separator.])

  AC_DEFINE(IS_DIRECTORY_SEP(_c_), [((_c_) == DIRECTORY_SEP)],
    [Returns true if character is a directory separator.])

  AC_DEFINE(IS_ANY_SEP(_c_), [(IS_DIRECTORY_SEP (_c_))],
    [Returns true if character is any form of separator.])
fi

if test "$USE_X_TOOLKIT" != "none"; then
  have_editres=yes
  case $opsys in
    hpux*)
      dnl Assar Westerlund <assar@sics.se> says this is necessary
      dnl for HP-UX 10.20, and that it works for HP-UX 0 as well.
      have_editres=no
    ;;
  esac
  if test "$have_editres" != no && test ! -z "$LIBXMU"; then
    OLDLIBS="$LIBS"
    dnl See libXmu.a check above.
    if test x$HAVE_X11XTR6 = xyes; then
      LIBS="-lXt -lSM -lICE $LIBXMU"
    else
      OTHERLIBS="-lXt -$LIBXMU"
    fi
    AC_TRY_LINK(
      [#include <X11/Intrinsic.h>
       #include <X11/Xmu/Editres.h>],
      [_XEditResCheckMessages (0, 0, 0, 0);],
      [AC_DEFINE([X_TOOLKIT_EDITRES], 1,
        [Define to 1 if we should use XEditRes.])])
    LIBS=$OLDLIBS
  fi
fi

case $opsys in
  sol2* | unixware )
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
  hpux* | nacl | openbsd | sol2* | unixware )
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
  darwin | dragonfly | freebsd | netbsd | openbsd )
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


dnl If the system's imake configuration file defines 'NeedWidePrototypes'
dnl as 'NO', we must define NARROWPROTO manually.  Such a define is
dnl generated in the Makefile generated by 'xmkmf'.  If we don't define
dnl NARROWPROTO, we will see the wrong function prototypes for X functions
dnl taking float or double parameters.
case $opsys in
  cygwin|gnu|gnu-linux|gnu-kfreebsd|freebsd|netbsd|openbsd)
    AC_DEFINE(NARROWPROTO, 1, [Define if system's imake configuration
      file defines 'NeedWidePrototypes' as 'NO'.])
  ;;
esac


dnl Used in process.c, this must be a loop, even if it only runs once.
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
    AC_DEFINE(PTY_OPEN, [ do { int dummy; sigset_t blocked, procmask; sigemptyset (&blocked); sigaddset (&blocked, SIGCHLD); pthread_sigmask (SIG_BLOCK, &blocked, &procmask); if (-1 == openpty (&fd, &dummy, pty_name, 0, 0)) fd = -1; pthread_sigmask (SIG_SETMASK, &procmask, 0); if (fd >= 0) emacs_close (dummy); } while (false)])
    AC_DEFINE(PTY_NAME_SPRINTF, [])
    AC_DEFINE(PTY_TTY_NAME_SPRINTF, [])
    ;;

  gnu | openbsd | qnxnto )
    AC_DEFINE(FIRST_PTY_LETTER, ['p'])
    ;;

  gnu-linux | gnu-kfreebsd | dragonfly | freebsd | netbsd | darwin | nacl )
    dnl if HAVE_GRANTPT
    if test "x$ac_cv_func_grantpt" = xyes; then
      AC_DEFINE(UNIX98_PTYS, 1, [Define if the system has Unix98 PTYs.])
      AC_DEFINE(PTY_ITERATION, [int i; for (i = 0; i < 1; i++)])
      dnl Note that grantpt and unlockpt may fork.  We must block SIGCHLD
      dnl to prevent sigchld_handler from intercepting the child's death.
      AC_DEFINE(PTY_TTY_NAME_SPRINTF, [{ char *ptyname = 0; sigset_t blocked; sigemptyset (&blocked); sigaddset (&blocked, SIGCHLD); pthread_sigmask (SIG_BLOCK, &blocked, 0); if (grantpt (fd) != -1 && unlockpt (fd) != -1) ptyname = ptsname(fd); pthread_sigmask (SIG_UNBLOCK, &blocked, 0); if (!ptyname) { emacs_close (fd); return -1; } snprintf (pty_name, PTY_NAME_SIZE, "%s", ptyname); }])
      dnl if HAVE_POSIX_OPENPT
      if test "x$ac_cv_func_posix_openpt" = xyes; then
        AC_DEFINE(PTY_OPEN, [do { fd = posix_openpt (O_RDWR | O_CLOEXEC | O_NOCTTY); if (fd < 0 && errno == EINVAL) fd = posix_openpt (O_RDWR | O_NOCTTY); } while (false)])
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

  sol2* )
    dnl On SysVr4, grantpt(3) forks a subprocess, so do not use
    dnl O_CLOEXEC when opening the pty, and keep the SIGCHLD handler
    dnl from intercepting that death.  If any child but grantpt's should die
    dnl within, it should be caught after sigrelse(2).
    AC_DEFINE(PTY_OPEN, [fd = open (pty_name, O_RDWR | O_NONBLOCK)])
    AC_DEFINE(PTY_TTY_NAME_SPRINTF, [{ char *ptsname (int), *ptyname; int grantpt_result; sigset_t blocked; sigemptyset (&blocked); sigaddset (&blocked, SIGCHLD); pthread_sigmask (SIG_BLOCK, &blocked, 0); grantpt_result = grantpt (fd); pthread_sigmask (SIG_UNBLOCK, &blocked, 0); if (grantpt_result == -1 || unlockpt (fd) == -1 || !(ptyname = ptsname (fd))) { emacs_close (fd); return -1; } snprintf (pty_name, PTY_NAME_SIZE, "%s", ptyname); }])
    ;;

  unixware )
    dnl Comments are as per sol2*.
    AC_DEFINE(PTY_OPEN, [fd = open (pty_name, O_RDWR | O_NONBLOCK)])
    AC_DEFINE(PTY_TTY_NAME_SPRINTF, [{ char *ptsname (int), *ptyname; int grantpt_result; sigset_t blocked; sigemptyset (&blocked); sigaddset (&blocked, SIGCHLD); pthread_sigmask (SIG_BLOCK, &blocked, 0); grantpt_result = grantpt (fd); pthread_sigmask (SIG_UNBLOCK, &blocked, 0); if (grantpt_result == -1) fatal("could not grant slave pty"); if (unlockpt(fd) == -1) fatal("could not unlock slave pty"); if (!(ptyname = ptsname(fd))) fatal ("could not enable slave pty"); snprintf (pty_name, PTY_NAME_SIZE, "%s", ptyname); }])
    ;;
esac


case $opsys in
  sol2* | unixware )
    dnl This change means that we don't loop through allocate_pty too
    dnl many times in the (rare) event of a failure.
    AC_DEFINE(FIRST_PTY_LETTER, ['z'])
    AC_DEFINE(PTY_NAME_SPRINTF, [strcpy (pty_name, "/dev/ptmx");])
    dnl Push various streams modules onto a PTY channel.  Used in process.c.
    AC_DEFINE(SETUP_SLAVE_PTY, [if (ioctl (forkin, I_PUSH, "ptem") == -1) fatal ("ioctl I_PUSH ptem"); if (ioctl (forkin, I_PUSH, "ldterm") == -1) fatal ("ioctl I_PUSH ldterm"); if (ioctl (forkin, I_PUSH, "ttcompat") == -1) fatal ("ioctl I_PUSH ttcompat");], [How to set up a slave PTY, if needed.])
    ;;
esac


AH_TEMPLATE(SIGNALS_VIA_CHARACTERS, [Make process_send_signal work by
"typing" a signal character on the pty.])

case $opsys in
  dnl Perry Smith <pedz@ddivt1.austin.ibm.com> says this is correct for AIX.
  aix4-2 | cygwin | gnu | dragonfly | freebsd | netbsd | openbsd | darwin )
    AC_DEFINE(SIGNALS_VIA_CHARACTERS, 1)
    ;;

  dnl 21 Jun 06: Eric Hanchrow <offby1@blarg.net> says this works.
  dnl FIXME Does gnu-kfreebsd have linux/version.h?  It seems unlikely...
  gnu-linux | gnu-kfreebsd )

    AC_CACHE_CHECK([for signals via characters], [emacs_cv_signals_via_chars],
    [AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
#include <linux/version.h>
#if LINUX_VERSION_CODE < 0x20400
# error "Linux version too old"
#endif
      ]], [[]])], emacs_cv_signals_via_chars=yes, emacs_cv_signals_via_chars=no)])

    test "$emacs_cv_signals_via_chars" = yes && AC_DEFINE(SIGNALS_VIA_CHARACTERS, 1)
    ;;
esac


AH_TEMPLATE(TAB3, [Undocumented.])

case $opsys in
  darwin) AC_DEFINE(TAB3, OXTABS) ;;

  gnu | dragonfly | freebsd | netbsd | openbsd )
    AC_DEFINE(TABDLY, OXTABS, [Undocumented.])
    AC_DEFINE(TAB3, OXTABS)
    ;;

  gnu-linux | gnu-kfreebsd )
    AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
#ifndef __ia64__
# error "not ia64"
#endif
      ]], [[]])], AC_DEFINE(GC_MARK_SECONDARY_STACK(),
        [do { extern void *__libc_ia64_register_backing_store_base; __builtin_ia64_flushrs (); mark_memory (__libc_ia64_register_backing_store_base, __builtin_ia64_bsp ());} while (false)],
        [Mark a secondary stack, like the register stack on the ia64.]), [])
    ;;

  hpux*)
    AC_DEFINE(RUN_TIME_REMAP, 1, [Define if emacs.c needs to call
      run_time_remap; for HPUX.])
    ;;
esac


dnl This won't be used automatically yet.  We also need to know, at least,
dnl that the stack is continuous.
AH_TEMPLATE(GC_SETJMP_WORKS, [Define if setjmp is known to save all
  registers relevant for conservative garbage collection in the jmp_buf.])


case $opsys in
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
    ]], [[]])], AC_DEFINE(GC_SETJMP_WORKS, 1))
    ;;
esac


if test x$GCC = xyes; then
   dnl GC_SETJMP_WORKS is nearly always appropriate for GCC.
   AC_DEFINE(GC_SETJMP_WORKS, 1)
else
  case $opsys in
    aix* | dragonfly | freebsd | netbsd | openbsd | sol2* )
      AC_DEFINE(GC_SETJMP_WORKS, 1)
      ;;
  esac
fi                              dnl GCC?

dnl In a weird quirk, MS runtime uses _setjmp and longjmp.
AC_CACHE_CHECK([for _setjmp], [emacs_cv_func__setjmp],
  [AC_LINK_IFELSE(
     [AC_LANG_PROGRAM(
       [[#include <setjmp.h>
         #ifdef __MINGW32__
         # define _longjmp longjmp
         #endif
       ]],
       [[jmp_buf j;
	 if (! _setjmp (j))
	   _longjmp (j, 1);]])],
     [emacs_cv_func__setjmp=yes],
     [emacs_cv_func__setjmp=no])])
if test $emacs_cv_func__setjmp = yes; then
  AC_DEFINE([HAVE__SETJMP], 1, [Define to 1 if _setjmp and _longjmp work.])
fi

# We need to preserve signal mask to handle C stack overflows.
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
    [Define to 1 if sigsetjmp and siglongjmp work.])
fi

case $emacs_cv_func_sigsetjmp,$emacs_cv_alternate_stack,$opsys in
  yes,yes,* | *,*,mingw32)
    AC_DEFINE([HAVE_STACK_OVERFLOW_HANDLING], 1,
      [Define to 1 if C stack overflow can be handled in some cases.]);;
esac

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
AH_TEMPLATE(DOS_NT, [Define if the system is MS DOS or MS Windows.])
AH_TEMPLATE(MSDOS, [Define if the system is MS DOS.])
AH_TEMPLATE(USG, [Define if the system is compatible with System III.])
AH_TEMPLATE(USG5_4, [Define if the system is compatible with System V Release 4.])

case $opsys in
  aix4-2)
    AC_DEFINE(USG, [])
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
    dnl Not __APPLE__, as this may not be defined on non-macOS Darwin.
    dnl Not DARWIN, because Panther and lower CoreFoundation.h use DARWIN to
    dnl distinguish macOS from pure Darwin.
    AC_DEFINE(DARWIN_OS, [], [Define if the system is Darwin.])
    ;;

  gnu-linux | gnu-kfreebsd )
    AC_DEFINE(USG, [])
    AC_DEFINE(GNU_LINUX, [], [Define if ths system is compatible with GNU/Linux.])
    ;;

  hpux*)
    AC_DEFINE(USG, [])
    AC_DEFINE(HPUX, [], [Define if the system is HPUX.])
    ;;

  mingw32)
    AC_DEFINE(DOS_NT, [])
    AC_DEFINE(WINDOWSNT, 1, [Define if compiling for native MS Windows.])
    if test "x$ac_enable_checking" != "x" ; then
      AC_DEFINE(EMACSDEBUG, 1, [Define to 1 to enable w32 debug facilities.])
    fi
    ;;

  sol2*)
    AC_DEFINE(USG, [])
    AC_DEFINE(USG5_4, [])
    AC_DEFINE(SOLARIS2, [], [Define if the system is Solaris.])
    ;;

  unixware)
    AC_DEFINE(USG, [])
    AC_DEFINE(USG5_4, [])
    ;;
esac

AC_CACHE_CHECK([for usable FIONREAD], [emacs_cv_usable_FIONREAD],
  [case $opsys in
     aix4-2 | nacl)
       dnl BUILD 9008 - FIONREAD problem still exists in X-Windows.
       emacs_cv_usable_FIONREAD=no
       ;;

     mingw32)
       emacs_cv_usable_FIONREAD=yes
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
  hpux11)
    dnl It works to open the pty's tty in the parent (Emacs), then
    dnl close and reopen it in the child.
    AC_DEFINE(USG_SUBTTY_WORKS, 1, [Define for USG systems where it
      works to open a pty's tty in the parent process, then close and
      reopen it in the child.])
    ;;

  sol2-10)
    AC_DEFINE(_STRUCTURED_PROC, 1, [Needed for system_process_attributes
      on Solaris.])
    ;;
esac

# Set up the CFLAGS for real compilation, so we can substitute it.
CFLAGS="$REAL_CFLAGS"
CPPFLAGS="$REAL_CPPFLAGS"
LIBS="$REAL_LIBS"

## Hack to detect a buggy GCC version.
if test "$GCC" = yes && \
   $CC --version 2> /dev/null | grep 'gcc.* 4.5.0' >/dev/null; then
  case $CFLAGS in
    *-fno-optimize-sibling-calls*) ;;
    *-O@<:@23@:>@*)
      AC_MSG_ERROR([GCC 4.5.0 has problems compiling Emacs; see etc/PROBLEMS.]);;
  esac
fi

version=$PACKAGE_VERSION

copyright="Copyright (C) 2020 Free Software Foundation, Inc."
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
AC_SUBST(standardlisppath)
AC_SUBST(locallisppath)
AC_SUBST(lisppath)
AC_SUBST(x_default_search_path)
AC_SUBST(etcdir)
AC_SUBST(archlibdir)
AC_SUBST(etcdocdir)
AC_SUBST(bitmapdir)
AC_SUBST(gamedir)
AC_SUBST(gameuser)
AC_SUBST(gamegroup)
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
AC_DEFINE_UNQUOTED(EMACS_CONFIG_OPTIONS, "${emacs_config_options}",
		   [Define to the options passed to configure.])

XMENU_OBJ=
XOBJ=
FONT_OBJ=
if test "${HAVE_X_WINDOWS}" = "yes" ; then
  AC_DEFINE(HAVE_X_WINDOWS, 1,
	    [Define to 1 if you want to use the X window system.])
  XMENU_OBJ=xmenu.o
  XOBJ="xterm.o xfns.o xselect.o xrdb.o xsmfns.o xsettings.o"
  FONT_OBJ=xfont.o
  if test "$HAVE_CAIRO" = "yes"; then
    FONT_OBJ="$FONT_OBJ ftfont.o ftcrfont.o"
  elif test "$HAVE_XFT" = "yes"; then
    FONT_OBJ="$FONT_OBJ ftfont.o xftfont.o ftxfont.o"
  elif test "$HAVE_FREETYPE" = "yes"; then
    FONT_OBJ="$FONT_OBJ ftfont.o ftxfont.o"
  fi
fi
if test "${HAVE_HARFBUZZ}" = "yes" ; then
  FONT_OBJ="$FONT_OBJ hbfont.o"
fi
AC_SUBST(FONT_OBJ)
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
if test "$HAVE_XWIDGETS" = "yes"; then
  TOOLKIT_LIBW="$TOOLKIT_LIBW -lXcomposite"
fi
AC_SUBST(TOOLKIT_LIBW)

if test "${opsys}" != "mingw32"; then
  if test "$USE_X_TOOLKIT" = "none"; then
    LIBXT_OTHER="\$(LIBXSM)"
  else
    LIBXT_OTHER="\$(LIBXMU) -lXt \$(LIBXTR6) -lXext"
  fi
fi
AC_SUBST(LIBXT_OTHER)

if test "${HAVE_X11}" = "yes" ; then
  AC_DEFINE(HAVE_X11, 1,
	    [Define to 1 if you want to use version 11 of X windows.])
  LIBX_OTHER="\$(LIBXT) \$(LIBX_EXTRA)"
else
  LIBX_OTHER=
fi
AC_SUBST(LIBX_OTHER)

HAVE_OLDXMENU=no
if test "$HAVE_GTK" = yes || test "$HAVE_X11" != yes; then
  LIBXMENU=
elif test "$USE_X_TOOLKIT" = none; then
  HAVE_OLDXMENU=yes
  LIBXMENU='$(oldXMenudir)/libXMenu11.a'
  AUTODEPEND_PARENTS="$AUTODEPEND_PARENTS oldXMenu"
else
  LIBXMENU='$(lwlibdir)/liblw.a'
  AUTODEPEND_PARENTS="$AUTODEPEND_PARENTS lwlib"
fi
AC_SUBST(LIBXMENU)

AC_CACHE_CHECK([for struct alignment],
  [emacs_cv_struct_alignment],
  [AC_COMPILE_IFELSE(
     [AC_LANG_PROGRAM([[#include <stddef.h>
			struct s { char c; } __attribute__ ((aligned (8)));
			struct t { char c; struct s s; };
			char verify[offsetof (struct t, s) == 8 ? 1 : -1];
		      ]])],
     [emacs_cv_struct_alignment=yes],
     [emacs_cv_struct_alignment=no])])
if test "$emacs_cv_struct_alignment" = yes; then
  AC_DEFINE([HAVE_STRUCT_ATTRIBUTE_ALIGNED], 1,
    [Define to 1 if 'struct __attribute__ ((aligned (N)))' aligns the
     structure to an N-byte boundary.])
fi

AC_C_TYPEOF

AC_CACHE_CHECK([for statement expressions],
  [emacs_cv_statement_expressions],
  [AC_COMPILE_IFELSE(
     [AC_LANG_PROGRAM([], [[return ({ int x = 5; x-x; });]])],
     [emacs_cv_statement_expressions=yes],
     [emacs_cv_statement_expressions=no])])
if test "$emacs_cv_statement_expressions" = yes; then
  AC_DEFINE([HAVE_STATEMENT_EXPRESSIONS], 1,
    [Define to 1 if statement expressions work.])
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
  CYGWIN_OBJ="cygw32.o"
  ## Cygwin differs because of its unexec().
  PRE_ALLOC_OBJ=
  POST_ALLOC_OBJ=lastfile.o
elif test "$opsys" = "mingw32"; then
  CYGWIN_OBJ=
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

dnl Call this 'FORTIFY_SOUR' so that it sorts before the 'FORTIFY_SOURCE'
dnl verbatim defined above.  The tricky name is apropos, as this hack
dnl makes Fortify go sour!
AH_VERBATIM([FORTIFY_SOUR],
[/* Without the following workaround, Emacs runs slowly on OS X 10.8.
   The workaround disables some useful run-time checking, so it
   should be conditional to the platforms with the performance bug.
   Perhaps Apple will fix this some day; also see m4/extern-inline.m4.  */
#if defined __APPLE__ && defined __GNUC__
# ifndef _DONT_USE_CTYPE_INLINE_
#  define _DONT_USE_CTYPE_INLINE_
# endif
# ifndef _FORTIFY_SOURCE
#  define _FORTIFY_SOURCE 0
# endif
#endif
])

# If user asks to omit features, disable optional features that gnulib
# might otherwise enable.
if test "$with_features" = no && test "$enable_acl" != yes; then
  enable_acl=no
fi

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

if test "${opsys}" = "mingw32"; then
  CPPFLAGS="$CPPFLAGS -DUSE_CRT_DLL=1 -I \${abs_top_srcdir}/nt/inc"
  # Remove unneeded switches from the value of CC that goes to Makefiles
  CC=`AS_ECHO(["$CC"]) | sed -e "s,$GCC_TEST_OPTIONS,,"`
fi

case "$opsys" in
  aix4-2) LD_SWITCH_SYSTEM_TEMACS="-Wl,-bnodelcsect" ;;

  cygwin) LD_SWITCH_SYSTEM_TEMACS="-Wl,-stack,0x00800000" ;;

  darwin)
   if test "$HAVE_NS" = "yes"; then
     libs_nsgui="-framework AppKit"
     if test "$NS_IMPL_COCOA" = "yes"; then
        libs_nsgui="$libs_nsgui -framework IOKit -framework Carbon"
     fi
   else
     libs_nsgui=
   fi
   LD_SWITCH_SYSTEM_TEMACS=$libs_nsgui
   ## The -headerpad option tells ld (see man page) to leave room at the
   ## end of the header for adding load commands.  Needed for dumping.
   ## 0x1000 is enough for roughly 52 load commands on the x86_64
   ## architecture (where they are 78 bytes each). The actual number of
   ## load commands added is not consistent but normally ranges from
   ## about 14 to about 34. Setting it high gets us plenty of slop and
   ## only costs about 1.5K of wasted binary space.
   headerpad_extra=1000
   if test "$with_unexec" = yes; then
     LD_SWITCH_SYSTEM_TEMACS="-fno-pie $LD_SWITCH_SYSTEM_TEMACS -Xlinker -headerpad -Xlinker $headerpad_extra"
   fi

   ## This is here because src/Makefile.in did some extra fiddling around
   ## with LD_SWITCH_SYSTEM.  It seems cleaner to put this in
   ## LD_SWITCH_SYSTEM_TEMACS instead,
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

  mingw32)
   ## Is it any better under MinGW64 to relocate emacs into higher addresses?
   case "$canonical" in
     x86_64-*-*) LD_SWITCH_SYSTEM_TEMACS="-Wl,-stack,0x00800000 -Wl,-heap,0x00100000 -Wl,-image-base,0x400000000 -Wl,-entry,__start -Wl,-Map,./temacs.map" ;;
     *) LD_SWITCH_SYSTEM_TEMACS="-Wl,-stack,0x00800000 -Wl,-heap,0x00100000 -Wl,-image-base,0x01000000 -Wl,-entry,__start -Wl,-Map,./temacs.map" ;;
   esac
   ;;

  *) LD_SWITCH_SYSTEM_TEMACS= ;;
esac

# -no-pie or -nopie fixes a temacs segfault on Gentoo, OpenBSD,
# Ubuntu, and other systems with "hardened" GCC configurations for
# some reason (Bug#18784).  We don't know why this works, but not
# segfaulting is better than segfaulting.  Use ac_c_werror_flag=yes
# when trying the option, otherwise clang keeps warning that it does
# not understand it, and pre-4.6 GCC has a similar problem
# (Bug#20338).  Prefer -no-pie to -nopie, as -no-pie is the
# spelling used by GCC 6.1.0 and later (Bug#24682).
AC_CACHE_CHECK(
  [for $CC option to disable position independent executables],
  [emacs_cv_prog_cc_no_pie],
  [if test $with_unexec = no; then
     emacs_cv_prog_cc_no_pie='not needed'
   else
     emacs_save_c_werror_flag=$ac_c_werror_flag
     emacs_save_LDFLAGS=$LDFLAGS
     ac_c_werror_flag=yes
     for emacs_cv_prog_cc_no_pie in -no-pie -nopie no; do
       test $emacs_cv_prog_cc_no_pie = no && break
       LDFLAGS="$emacs_save_LDFLAGS $emacs_cv_prog_cc_no_pie"
       AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])], [break])
     done
     ac_c_werror_flag=$emacs_save_c_werror_flag
     LDFLAGS=$emacs_save_LDFLAGS
   fi])
case $emacs_cv_prog_cc_no_pie in
  -*)
    LD_SWITCH_SYSTEM_TEMACS="$LD_SWITCH_SYSTEM_TEMACS $emacs_cv_prog_cc_no_pie"
    ;;
esac

if test x$ac_enable_profiling != x ; then
  case $opsys in
    *freebsd | gnu-linux) ;;
    *) LD_SWITCH_SYSTEM_TEMACS="$LD_SWITCH_SYSTEM_TEMACS -pg" ;;
  esac
fi

LD_SWITCH_SYSTEM_TEMACS="$LDFLAGS_NOCOMBRELOC $LD_SWITCH_SYSTEM_TEMACS"

AC_SUBST(LD_SWITCH_SYSTEM_TEMACS)

## Common for all window systems
if test "$window_system" != "none"; then
  AC_DEFINE(HAVE_WINDOW_SYSTEM, 1, [Define if you have a window system.])
  AC_DEFINE(POLL_FOR_INPUT, 1, [Define if you poll periodically to detect C-g.])
  WINDOW_SYSTEM_OBJ="fontset.o fringe.o image.o"
fi

AC_SUBST(WINDOW_SYSTEM_OBJ)

AH_TOP([/* GNU Emacs site configuration template file.

Copyright (C) 1988, 1993-1994, 1999-2002, 2004-2020
  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */


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

if test $USE_ACL -ne 0; then
  ACL_SUMMARY="yes $LIB_ACL"
else
  ACL_SUMMARY=no
fi

emacs_standard_dirs='Standard dirs'
AS_ECHO(["
Configured for '${canonical}'.

  Where should the build process find the source code?    ${srcdir}
  What compiler should emacs be built with?               ${CC} ${CFLAGS}
  Should Emacs use the GNU version of malloc?             ${GNU_MALLOC}${GNU_MALLOC_reason}
  Should Emacs use a relocating allocator for buffers?    ${REL_ALLOC}
  Should Emacs use mmap(2) for buffer allocation?         $use_mmap_for_buffers
  What window system should Emacs use?                    ${window_system}
  What toolkit should Emacs use?                          ${USE_X_TOOLKIT}
  Where do we find X Windows header files?                ${x_includes:-$emacs_standard_dirs}
  Where do we find X Windows libraries?                   ${x_libraries:-$emacs_standard_dirs}"])

optsep=
emacs_config_features=
for opt in XAW3D XPM JPEG TIFF GIF PNG RSVG CAIRO IMAGEMAGICK SOUND GPM DBUS \
  GCONF GSETTINGS GLIB NOTIFY ACL LIBSELINUX GNUTLS LIBXML2 FREETYPE HARFBUZZ M17N_FLT \
  LIBOTF XFT ZLIB TOOLKIT_SCROLL_BARS X_TOOLKIT OLDXMENU X11 XDBE XIM \
  NS MODULES THREADS XWIDGETS LIBSYSTEMD JSON PDUMPER UNEXEC LCMS2 GMP; do

    case $opt in
      PDUMPER) val=${with_pdumper} ;;
      UNEXEC) val=${with_unexec} ;;
      GLIB) val=${emacs_cv_links_glib} ;;
      NOTIFY|ACL) eval val=\${${opt}_SUMMARY} ;;
      TOOLKIT_SCROLL_BARS|X_TOOLKIT) eval val=\${USE_$opt} ;;
      THREADS) val=${threads_enabled} ;;
      *) eval val=\${HAVE_$opt} ;;
    esac
    case x$val in
      xno|xnone|x) continue ;;
    esac
    case $opt in
      X_TOOLKIT)
        case $val in
          GTK*|LUCID|MOTIF) opt=$val ;;
          *) continue ;;
        esac
      ;;
      NOTIFY)
        case $val in
          *lkqueue*) opt="$opt LIBKQUEUE" ;;
          *kqueue*) opt="$opt KQUEUE" ;;
          *inotify*) opt="$opt INOTIFY" ;;
          *gfile*) opt="$opt GFILENOTIFY" ;;
          *w32*) opt="$opt W32NOTIFY" ;;
        esac
      ;;
    esac
    AS_VAR_APPEND([emacs_config_features], ["$optsep$opt"])
    optsep=' '
done
AC_DEFINE_UNQUOTED(EMACS_CONFIG_FEATURES, "${emacs_config_features}",
  [Summary of some of the main features enabled by configure.])

AS_ECHO(["  Does Emacs use -lXaw3d?                                 ${HAVE_XAW3D}
  Does Emacs use -lXpm?                                   ${HAVE_XPM}
  Does Emacs use -ljpeg?                                  ${HAVE_JPEG}
  Does Emacs use -ltiff?                                  ${HAVE_TIFF}
  Does Emacs use a gif library?                           ${HAVE_GIF} $LIBGIF
  Does Emacs use a png library?                           ${HAVE_PNG} $LIBPNG
  Does Emacs use -lrsvg-2?                                ${HAVE_RSVG}
  Does Emacs use cairo?                                   ${HAVE_CAIRO}
  Does Emacs use -llcms2?                                 ${HAVE_LCMS2}
  Does Emacs use imagemagick?                             ${HAVE_IMAGEMAGICK}
  Does Emacs support sound?                               ${HAVE_SOUND}
  Does Emacs use -lgpm?                                   ${HAVE_GPM}
  Does Emacs use -ldbus?                                  ${HAVE_DBUS}
  Does Emacs use -lgconf?                                 ${HAVE_GCONF}
  Does Emacs use GSettings?                               ${HAVE_GSETTINGS}
  Does Emacs use a file notification library?             ${NOTIFY_SUMMARY}
  Does Emacs use access control lists?                    ${ACL_SUMMARY}
  Does Emacs use -lselinux?                               ${HAVE_LIBSELINUX}
  Does Emacs use -lgnutls?                                ${HAVE_GNUTLS}
  Does Emacs use -lxml2?                                  ${HAVE_LIBXML2}
  Does Emacs use -lfreetype?                              ${HAVE_FREETYPE}
  Does Emacs use HarfBuzz?                                ${HAVE_HARFBUZZ}
  Does Emacs use -lm17n-flt?                              ${HAVE_M17N_FLT}
  Does Emacs use -lotf?                                   ${HAVE_LIBOTF}
  Does Emacs use -lxft?                                   ${HAVE_XFT}
  Does Emacs use -lsystemd?                               ${HAVE_LIBSYSTEMD}
  Does Emacs use -ljansson?                               ${HAVE_JSON}
  Does Emacs use -lgmp?                                   ${HAVE_GMP}
  Does Emacs directly use zlib?                           ${HAVE_ZLIB}
  Does Emacs have dynamic modules support?                ${HAVE_MODULES}
  Does Emacs use toolkit scroll bars?                     ${USE_TOOLKIT_SCROLL_BARS}
  Does Emacs support Xwidgets (requires gtk3)?            ${HAVE_XWIDGETS}
  Does Emacs have threading support in lisp?              ${threads_enabled}
  Does Emacs support the portable dumper?                 ${with_pdumper}
  Does Emacs support legacy unexec dumping?               ${with_unexec}
  Which dumping strategy does Emacs use?                  ${with_dumping}
"])

if test -n "${EMACSDATA}"; then
   AS_ECHO(["  Environment variable EMACSDATA set to:                  $EMACSDATA"])
fi
if test -n "${EMACSDOC}"; then
   AS_ECHO(["  Environment variable EMACSDOC set to:                   $EMACSDOC"])
fi

echo

if test "$HAVE_NS" = "yes"; then
   echo
   AS_ECHO(["Run '${MAKE-make}' to build Emacs, then run 'src/emacs' to test it.
Run '${MAKE-make} install' in order to build an application bundle.
The application will go to nextstep/Emacs.app and can be run or moved
from there."])
   if test "$EN_NS_SELF_CONTAINED" = "yes"; then
      echo "The application will be fully self-contained."
    else
      AS_ECHO(["The lisp resources for the application will be installed under ${prefix}.
You may need to run \"${MAKE-make} install\" with sudo.  The application will fail
to run if these resources are not installed."])
   fi
   echo
fi

case $opsys,$emacs_uname_r in
  cygwin,1.5.*)
    AC_MSG_WARN([[building Emacs on Cygwin 1.5 is not supported.]])
           echo
	   ;;
  cygwin,3.0.[[0-7]]'('* | cygwin,3.1.[[0-2]]'('*)
    AC_DEFINE([HAVE_CYGWIN_O_PATH_BUG], 1,
      [Define to 1 if opening a FIFO, socket, or symlink with O_PATH is buggy.]);;
esac

# Remove any trailing slashes in these variables.
case $prefix in
  */) prefix=`AS_DIRNAME(["$prefix."])`;;
esac
case $exec_prefix in
  */) exec_prefix=`AS_DIRNAME(["$exec_prefix."])`;;
esac

if test "$HAVE_NS" = "yes"; then
  if test "$NS_IMPL_GNUSTEP" = yes; then
    AC_CONFIG_FILES([nextstep/GNUstep/Emacs.base/Resources/Info-gnustep.plist:nextstep/templates/Info-gnustep.plist.in \
      nextstep/GNUstep/Emacs.base/Resources/Emacs.desktop:nextstep/templates/Emacs.desktop.in])
    ns_check_file=Resources/Info-gnustep.plist
  else
    AC_CONFIG_FILES([nextstep/Cocoa/Emacs.base/Contents/Info.plist:nextstep/templates/Info.plist.in \
      nextstep/Cocoa/Emacs.base/Contents/Resources/English.lproj/InfoPlist.strings:nextstep/templates/InfoPlist.strings.in])
    ns_check_file=Contents/Info.plist
  fi
  AC_SUBST(ns_check_file)
fi

AC_CONFIG_FILES([Makefile lib/gnulib.mk])

dnl config.status treats $srcdir specially, so I think this is ok...
AC_CONFIG_FILES([$srcdir/doc/man/emacs.1])

m4_define([subdir_makefiles],
  [lib/Makefile lib-src/Makefile oldXMenu/Makefile doc/emacs/Makefile doc/misc/Makefile doc/lispintro/Makefile doc/lispref/Makefile src/Makefile lwlib/Makefile lisp/Makefile leim/Makefile nextstep/Makefile nt/Makefile])
SUBDIR_MAKEFILES="subdir_makefiles"
AC_CONFIG_FILES(subdir_makefiles)

dnl The test/ directory is missing if './make-dist --no-tests' was used.
opt_makefile=test/Makefile
if test -f "$srcdir/$opt_makefile.in"; then
  SUBDIR_MAKEFILES="$SUBDIR_MAKEFILES $opt_makefile"
  dnl Again, it's best not to use a variable.  Though you can add
  dnl ", [], [opt_makefile='$opt_makefile']" and it should work.
  AC_CONFIG_FILES([test/Makefile])
fi


dnl The admin/ directory used to be excluded from tarfiles.
if test -d $srcdir/admin; then
  SUBDIR_MAKEFILES="$SUBDIR_MAKEFILES admin/charsets/Makefile admin/unidata/Makefile admin/grammars/Makefile"
  AC_CONFIG_FILES([admin/charsets/Makefile])
  AC_CONFIG_FILES([admin/unidata/Makefile])
  AC_CONFIG_FILES([admin/grammars/Makefile])
fi                              dnl -d admin


SUBDIR_MAKEFILES_IN=`echo " ${SUBDIR_MAKEFILES}" | sed -e 's| | $(srcdir)/|g' -e 's|Makefile|Makefile.in|g'`

AC_SUBST(SUBDIR_MAKEFILES_IN)

dnl You might wonder (I did) why epaths.h is generated by running make,
dnl rather than just letting configure generate it from epaths.in.
dnl One reason is that the various paths are not fully expanded (see above);
dnl e.g., gamedir='${localstatedir}/games/emacs'.
dnl Secondly, the GNU Coding standards require that one should be able
dnl to run 'make prefix=/some/where/else' and override the values set
dnl by configure.  This also explains the 'move-if-change' test and
dnl the use of force in the 'epaths-force' rule in Makefile.in.
AC_CONFIG_COMMANDS([src/epaths.h], [
if test "${opsys}" = "mingw32"; then
  ${MAKE-make} MAKEFILE_NAME=do-not-make-Makefile epaths-force-w32
else
  ${MAKE-make} MAKEFILE_NAME=do-not-make-Makefile epaths-force
fi || AC_MSG_ERROR(['src/epaths.h' could not be made.])
], [GCC="$GCC" CPPFLAGS="$CPPFLAGS" opsys="$opsys"])

dnl NB we have to cheat and use the ac_... version because abs_top_srcdir
dnl is not yet set, sigh.  Or we could use ../$srcdir/src/.gdbinit,
dnl or a symlink?
AC_CONFIG_COMMANDS([src/.gdbinit], [
if test ! -f src/.gdbinit && test -f "$srcdir/src/.gdbinit"; then
  AS_ECHO(["source $ac_abs_top_srcdir/src/.gdbinit"]) > src/.gdbinit
fi
])

dnl Perhaps this would be better named doc-emacs-emacsver.texi?
dnl See comments for etc-refcards-emacsver.tex.
dnl Since we get a doc/emacs directory generated anyway, for the Makefile,
dnl it is not quite the same.  But we are generating in $srcdir.
AC_CONFIG_COMMANDS([doc/emacs/emacsver.texi], [
${MAKE-make} -s --no-print-directory -C doc/emacs doc-emacsver || \
AC_MSG_ERROR(['doc/emacs/emacsver.texi' could not be made.])
])

dnl If we give this the more natural name, etc/refcards/emacsver.texi,
dnl then a directory etc/refcards is created in the build directory,
dnl which is probably harmless, but confusing (in out-of-tree builds).
dnl (If we were to generate etc/refcards/Makefile, this might change.)
dnl It is really $srcdir/etc/refcards/emacsver.tex that we generate.
AC_CONFIG_COMMANDS([etc-refcards-emacsver.tex], [
${MAKE-make} -s MAKEFILE_NAME=do-not-make-Makefile etc-emacsver || \
AC_MSG_ERROR(['etc/refcards/emacsver.tex' could not be made.])
])

if test $AUTO_DEPEND = yes; then
   for dir in $AUTODEPEND_PARENTS; do
     AS_MKDIR_P([$dir/deps])
   done
fi

AC_OUTPUT

if test ! "$with_mailutils"; then
  if test "$with_pop" = yes; then
    AC_MSG_WARN([This configuration installs a 'movemail' program
that retrieves POP3 email via only insecure channels.
To omit insecure POP3, you can use '$0 --without-pop'.])
  elif test "$with_pop" = no-by-default; then
    AC_MSG_WARN([This configuration installs a 'movemail' program
that does not retrieve POP3 email.  By default, Emacs 25 and earlier
installed a 'movemail' program that retrieved POP3 email via only
insecure channels, a practice that is no longer recommended but that
you can continue to support by using '$0 --with-pop'.])
  fi

  case $opsys in
    mingw32)
      # Don't suggest GNU Mailutils, as it hasn't been ported.
      ;;
    *)
      emacs_fix_movemail="use '$0 --with-mailutils'"
      case `(movemail --version) 2>/dev/null` in
	*Mailutils*) ;;
	*) emacs_fix_movemail="install GNU Mailutils
<https://mailutils.org> and $emacs_fix_movemail";;
      esac
      AC_MSG_NOTICE([You might want to $emacs_fix_movemail.]);;
  esac
fi

# Let plain 'make' work.
test "$MAKE" = make || test -f makefile || cat >makefile <<EOF
.POSIX:
MAKE = $MAKE
all:
	\$(MAKE) -f Makefile \$?
.DEFAULT:
	\$(MAKE) -f Makefile \$<
EOF
