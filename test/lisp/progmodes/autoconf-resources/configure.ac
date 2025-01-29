# Indentation.

AC_PROG_CC
dnl <- font-lock-keyword-face
 AC_PROG_CC
 dnl <- font-lock-keyword-face
  AC_PROG_CC
  dnl <- font-lock-keyword-face

# Quoting.

   [AC_PROG_CC] [[AC_PREREQ([2.70])]]
dnl ^^^^^^^^^^    ^^^^^^^^^ font-lock-keyword-face

# Nesting.

AS_VAR_IF([foo], [bar], [AC_MSG_FAILURE([baz])])
dnl <- font-lock-keyword-face
dnl                      ^^^^^^^^^^^^^^ font-lock-keyword-face
AS_VAR_IF([foo], [bar],
dnl <- font-lock-keyword-face
   [AC_MSG_FAILURE([baz])])
dnl ^^^^^^^^^^^^^^ font-lock-keyword-face

# Autoconf.

AC_PROG_CC AC_PREREQ(2.70) AC_PREREQ([2.70])
dnl <- font-lock-keyword-face
dnl        ^^^^^^^^^       ^^^^^^^^^ font-lock-keyword-face

# Autoheader.

AH_HEADER AH_TOP(foo) AH_TOP([foo])
dnl <- font-lock-keyword-face
dnl       ^^^^^^      ^^^^^^ font-lock-keyword-face

# Automake.

AM_PATH_LISPDIR AM_SILENT_RULES(yes) AM_SILENT_RULES([yes])
dnl <- font-lock-keyword-face
dnl             ^^^^^^^^^^^^^^^      ^^^^^^^^^^^^^^^ font-lock-keyword-face

# M4sh.

AS_INIT AS_ECHO(foo) AS_ECHO([foo])
dnl <- font-lock-keyword-face
dnl     ^^^^^^^      ^^^^^^^ font-lock-keyword-face

# Autoconf Archive.

AX_ADD_FORTIFY_SOURCE AX_SAVE_FLAGS(foo) AX_SAVE_FLAGS([foo])
dnl <- font-lock-keyword-face
dnl                   ^^^^^^^^^^^^^      ^^^^^^^^^^^^^ font-lock-keyword-face

# Libtool.

LT_OUTPUT LT_PREREQ(2.4.6) LT_PREREQ([2.4.6])
dnl <- font-lock-keyword-face
dnl       ^^^^^^^^^        ^^^^^^^^^ font-lock-keyword-face

# Gnulib.

gl_EARLY gl_WARN_ADD(foo) gl_WARN_ADD([foo])
dnl <- font-lock-keyword-face
dnl      ^^^^^^^^^^^      ^^^^^^^^^^^ font-lock-keyword-face

# M4.

changequote(<<, >>) m4_changequote(<<, >>)
dnl <- font-lock-keyword-face
dnl                 ^^^^^^^^^^^^^^ !font-lock-keyword-face
changequote([, ]) m4_changequote([, ])
dnl <- font-lock-keyword-face
dnl               ^^^^^^^^^^^^^^ !font-lock-keyword-face

# AC_DEFINE object-like macro.

AC_DEFINE(a) AC_DEFINE(a, d)
dnl <- font-lock-keyword-face
dnl          ^^^^^^^^^ font-lock-keyword-face
dnl       ^            ^ font-lock-variable-name-face
AC_DEFINE([a]) AC_DEFINE([a], d)
dnl        ^              ^ font-lock-variable-name-face
AC_DEFINE([[a]]) AC_DEFINE([[a]], d)
dnl         ^                ^ font-lock-variable-name-face
AC_DEFINE(bc) AC_DEFINE(bc, d)
dnl       ^^            ^^ font-lock-variable-name-face
AC_DEFINE([bc]) AC_DEFINE([bc], d)
dnl        ^^              ^^ font-lock-variable-name-face
AC_DEFINE([[bc]]) AC_DEFINE([[bc]], d)
dnl         ^^                ^^ font-lock-variable-name-face

# AC_DEFINE function-like macro.

AC_DEFINE(a()) AC_DEFINE(a(), d)
dnl <- font-lock-keyword-face
dnl            ^^^^^^^^^ font-lock-keyword-face
dnl       ^              ^ font-lock-function-name-face
AC_DEFINE([a()]) AC_DEFINE([a()], d)
dnl        ^                ^ font-lock-function-name-face
AC_DEFINE([[a()]]) AC_DEFINE([[a()]], d)
dnl         ^                  ^ font-lock-function-name-face
AC_DEFINE(a(x)) AC_DEFINE(a(x), d)
dnl       ^               ^ font-lock-function-name-face
AC_DEFINE([a(x)]) AC_DEFINE([a(x)], d)
dnl        ^                 ^ font-lock-function-name-face
AC_DEFINE([[a(x)]]) AC_DEFINE([[a(x)]], d)
dnl         ^                   ^ font-lock-function-name-face
AC_DEFINE(a(x, y)) AC_DEFINE(a(x, y), d)
dnl       ^                  ^ font-lock-function-name-face
AC_DEFINE([a(x, y)]) AC_DEFINE([a(x, y)], d)
dnl        ^                    ^ font-lock-function-name-face
AC_DEFINE([[a(x, y)]]) AC_DEFINE([[a(x, y)]], d)
dnl         ^                      ^ font-lock-function-name-face

# AC_DEFINE_UNQUOTED object-like macro.

AC_DEFINE_UNQUOTED(a) AC_DEFINE_UNQUOTED(a, d)
dnl <- font-lock-keyword-face
dnl                   ^^^^^^^^^^^^^^^^^^ font-lock-keyword-face
dnl                ^                     ^ font-lock-variable-name-face
AC_DEFINE_UNQUOTED([a]) AC_DEFINE_UNQUOTED([a], d)
dnl                 ^                       ^ font-lock-variable-name-face
AC_DEFINE_UNQUOTED([[a]]) AC_DEFINE_UNQUOTED([[a]], d)
dnl                  ^                         ^ font-lock-variable-name-face
AC_DEFINE_UNQUOTED(bc) AC_DEFINE_UNQUOTED(bc, d)
dnl                ^^                     ^^ font-lock-variable-name-face
AC_DEFINE_UNQUOTED([bc]) AC_DEFINE_UNQUOTED([bc], d)
dnl                 ^^                       ^^ font-lock-variable-name-face
AC_DEFINE_UNQUOTED([[bc]]) AC_DEFINE_UNQUOTED([[bc]], d)
dnl                  ^^                         ^^ font-lock-variable-name-face
AC_DEFINE_UNQUOTED(\a`b`$c${d})
dnl                ^^^^^^^^^^^ font-lock-variable-name-face
AC_DEFINE_UNQUOTED([\a`b`$c${d}])
dnl                 ^^^^^^^^^^^ font-lock-variable-name-face
AC_DEFINE_UNQUOTED([[\a`b`$c${d}]])
dnl                  ^^^^^^^^^^^ font-lock-variable-name-face
AC_DEFINE_UNQUOTED(\a`b`$c${d}, e)
dnl                ^^^^^^^^^^^ font-lock-variable-name-face
AC_DEFINE_UNQUOTED([\a`b`$c${d}], e)
dnl                 ^^^^^^^^^^^ font-lock-variable-name-face
AC_DEFINE_UNQUOTED([[\a`b`$c${d}]], e)
dnl                  ^^^^^^^^^^^ font-lock-variable-name-face

# Variable definition.

AC_SUBST(a) AC_SUBST(a, b)
dnl <- font-lock-keyword-face
dnl         ^^^^^^^^ font-lock-keyword-face
dnl      ^           ^ font-lock-variable-name-face
AH_TEMPLATE(a, b) AH_VERBATIM(a, b)
dnl <- font-lock-keyword-face
dnl               ^^^^^^^^^^^ font-lock-keyword-face
dnl         ^                 ^font-lock-variable-name-face
AM_CONDITIONAL(a, b) AM_MISSING_PROG(a, b)
dnl <- font-lock-keyword-face
dnl                  ^^^^^^^^^^^^^^^ font-lock-keyword-face
dnl            ^                     ^ font-lock-variable-name-face

# Function definition.

AC_DEFUN(a) AC_DEFUN(a, b)
dnl <- font-lock-keyword-face
dnl         ^^^^^^^^ font-lock-keyword-face
dnl      ^           ^ font-lock-function-name-face
AC_DEFUN_ONCE(a, b)
dnl <- font-lock-keyword-face
dnl           ^ font-lock-function-name-face
AU_ALIAS(a, b) AU_DEFUN(a, b)
dnl <- font-lock-keyword-face
dnl            ^^^^^^^^ font-lock-keyword-face
dnl      ^              ^ font-lock-function-name-face
