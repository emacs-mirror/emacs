;;; compile-tests.el --- Test suite for compile.el.  -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2020 Free Software Foundation, Inc.

;; Author: Chong Yidong <cyd@stupidchicken.com>
;; Keywords:       internal
;; Human-Keywords: internal

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit tests for lisp/progmodes/compile.el.

;;; Code:

(require 'ert)
(require 'compile)

(defconst compile-tests--test-regexps-data
  ;; The computed column numbers are zero-indexed, so subtract 1 from
  ;; what's reported in the string.  The end column numbers are for
  ;; the character after, so it matches what's reported in the string.
  '(;; absoft
    ("Error on line 3 of t.f: Execution error unclassifiable statement"
     1 nil 3 "t.f")
    ("Line 45 of \"foo.c\": bloofle undefined"
     1 nil 45 "foo.c")
    ("error on line 19 of fplot.f: spelling error?"
     1 nil 19 "fplot.f")
    ("warning on line 17 of fplot.f: data type is undefined for variable d"
     1 nil 17 "fplot.f")
    ;; Ada & Mpatrol
    ("foo.adb:61:11:  [...] in call to size declared at foo.ads:11"
     1 11 61 "foo.adb")
    ("foo.adb:61:11:  [...] in call to size declared at foo.ads:11"
     52 nil 11 "foo.ads")
    ("     0x8008621 main+16 at error.c:17"
     23 nil 17 "error.c")
    ;; aix
    ("****** Error number 140 in line 8 of file errors.c ******"
     25 nil 8 "errors.c")
    ;; ant
    ("[javac] /src/DataBaseTestCase.java:27: unreported exception ..."
     13 nil 27 "/src/DataBaseTestCase.java" 2)
    ("[javac] /src/DataBaseTestCase.java:49: warning: finally clause cannot complete normally"
     13 nil 49 "/src/DataBaseTestCase.java" 1)
    ("[jikes]  foo.java:3:5:7:9: blah blah"
     14 (5 . 10) (3 . 7) "foo.java" 2)
    ("[javac] c:/cygwin/Test.java:12: error: foo: bar"
     9 nil 12 "c:/cygwin/Test.java" 2)
    ("[javac] c:\\cygwin\\Test.java:87: error: foo: bar"
     9 nil 87 "c:\\cygwin\\Test.java" 2)
    ;; Checkstyle error, but ant reports a warning (note additional
    ;; severity level after task name)
    ("[checkstyle] [ERROR] /src/Test.java:38: warning: foo"
     22 nil 38 "/src/Test.java" 1)
    ;; bash
    ("a.sh: line 1: ls-l: command not found"
     1 nil 1 "a.sh")
    ;; borland
    ("Error ping.c 15: Unable to open include file 'sys/types.h'"
     1 nil 15 "ping.c")
    ("Warning pong.c 68: Call to function 'func' with no prototype"
     1 nil 68 "pong.c")
    ("Error E2010 ping.c 15: Unable to open include file 'sys/types.h'"
     1 nil 15 "ping.c")
    ("Warning W1022 pong.c 68: Call to function 'func' with no prototype"
     1 nil 68 "pong.c")
    ;; caml
    ("File \"foobar.ml\", lines 5-8, characters 20-155: blah blah"
     1 (20 . 156) (5 . 8) "foobar.ml")
    ("File \"F:\\ocaml\\sorting.ml\", line 65, characters 2-145:\nWarning 26: unused variable equ."
     1 (2 . 146) 65 "F:\\ocaml\\sorting.ml")
    ("File \"/usr/share/gdesklets/display/TargetGauge.py\", line 41, in add_children"
     1 nil 41 "/usr/share/gdesklets/display/TargetGauge.py")
    ("File \\lib\\python\\Products\\PythonScripts\\PythonScript.py, line 302, in _exec"
     1 nil 302 "\\lib\\python\\Products\\PythonScripts\\PythonScript.py")
    ("File \"/tmp/foo.py\", line 10"
     1 nil 10 "/tmp/foo.py")
    ;; clang-include
    ("In file included from foo.cpp:2:"
     1 nil 2 "foo.cpp" 0)
    ;; cmake cmake-info
    ("CMake Error at CMakeLists.txt:23 (hurz):"
     1 nil 23 "CMakeLists.txt")
    ("CMake Warning at cmake/modules/UseUG.cmake:73 (find_package):"
     1 nil 73 "cmake/modules/UseUG.cmake")
    ("  cmake/modules/DuneGridMacros.cmake:19 (include)"
     1 nil 19 "cmake/modules/DuneGridMacros.cmake")
    ;; comma
    ("\"foo.f\", line 3: Error: syntax error near end of statement"
     1 nil 3 "foo.f")
    ("\"vvouch.c\", line 19.5: 1506-046 (S) Syntax error."
     1 5 19 "vvouch.c")
    ("\"foo.c\", line 32 pos 1; (E) syntax error; unexpected symbol: \"lossage\""
     1 1 32 "foo.c")
    ("\"foo.adb\", line 2(11): warning: file name does not match ..."
     1 11 2 "foo.adb")
    ("\"src/swapping.c\", line 30.34: 1506-342 (W) \"/*\" detected in comment."
     1 34 30 "src/swapping.c")
    ;; cucumber
    ("Scenario: undefined step  # features/cucumber.feature:3"
     29 nil 3 "features/cucumber.feature")
    ("      /home/gusev/.rvm/foo/bar.rb:500:in `_wrap_assertion'"
     1 nil 500 "/home/gusev/.rvm/foo/bar.rb")
    ;; edg-1 edg-2
    ("build/intel/debug/../../../struct.cpp(42): error: identifier \"foo\" is undefined"
     1 nil 42 "build/intel/debug/../../../struct.cpp")
    ("build/intel/debug/struct.cpp(44): warning #1011: missing return statement at end of"
     1 nil 44 "build/intel/debug/struct.cpp")
    ("build/intel/debug/iptr.h(302): remark #981: operands are evaluated in unspecified order"
     1 nil 302 "build/intel/debug/iptr.h")
    ("   detected during ... at line 62 of \"build/intel/debug/../../../trace.h\""
     31 nil 62 "build/intel/debug/../../../trace.h")
    ;; epc
    ("Error 24 at (2:progran.f90) : syntax error"
     1 nil 2 "progran.f90")
    ;; ftnchek
    ("    Dummy arg W in module SUBA line 8 file arrayclash.f is array"
     32 nil 8 "arrayclash.f")
    ("    L4 used at line 55 file test/assign.f; never set"
     16 nil 55 "test/assign.f")
    ("Warning near line 10 file arrayclash.f: Module contains no executable"
     1 nil 10 "arrayclash.f")
    ("Nonportable usage near line 31 col 9 file assign.f: mixed default and explicit"
     24 9 31 "assign.f")
    ;; iar
    ("\"foo.c\",3  Error[32]: Error message"
     1 nil 3 "foo.c")
    ("\"foo.c\",3  Warning[32]: Error message"
     1 nil 3 "foo.c")
    ;; ibm
    ("foo.c(2:0) : informational EDC0804: Function foo is not referenced."
     1 0 2 "foo.c")
    ("foo.c(3:8) : warning EDC0833: Implicit return statement encountered."
     1 8 3 "foo.c")
    ("foo.c(5:5) : error EDC0350: Syntax error."
     1 5 5 "foo.c")
    ;; irix
    ("ccom: Error: foo.c, line 2: syntax error"
     1 nil 2 "foo.c")
    ("cc: Severe: /src/Python-2.3.3/Modules/_curses_panel.c, line 17: Cannot find file <panel.h> ..."
     1 nil 17 "/src/Python-2.3.3/Modules/_curses_panel.c")
    ("cc: Info: foo.c, line 27: ..."
     1 nil 27 "foo.c")
    ("cfe: Warning 712: foo.c, line 2: illegal combination of pointer and ..."
     1 nil 2 "foo.c")
    ("cfe: Warning 600: xfe.c: 170: Not in a conditional directive while ..."
     1 nil 170 "xfe.c")
    ("/usr/lib/cmplrs/cc/cfe: Error: foo.c: 1: blah blah"
     1 nil 1 "foo.c")
    ("/usr/lib/cmplrs/cc/cfe: warning: foo.c: 1: blah blah"
     1 nil 1 "foo.c")
    ("foo bar: baz.f, line 27: ..."
     1 nil 27 "baz.f")
    ;; java
    ("\tat org.foo.ComponentGateway.doGet(ComponentGateway.java:172)"
     5 nil 172 "ComponentGateway.java")
    ("\tat javax.servlet.http.HttpServlet.service(HttpServlet.java:740)"
     5 nil 740 "HttpServlet.java")
    ("==1332==    at 0x4040743C: System::getErrorString() (../src/Lib/System.cpp:217)"
     13 nil 217 "../src/Lib/System.cpp")
    ("==1332==    by 0x8008621: main (vtest.c:180)"
     13 nil 180 "vtest.c")
    ;; jikes-file jikes-line
    ("Found 2 semantic errors compiling \"../javax/swing/BorderFactory.java\":"
     1 nil nil "../javax/swing/BorderFactory.java")
    ("Issued 1 semantic warning compiling \"java/awt/Toolkit.java\":"
     1 nil nil "java/awt/Toolkit.java")
    ;; gcc-include
    ("In file included from /usr/include/c++/3.3/backward/warn.h:4,"
     1 nil 4 "/usr/include/c++/3.3/backward/warn.h")
    ("                 from /usr/include/c++/3.3/backward/iostream.h:31:0,"
     1 0 31 "/usr/include/c++/3.3/backward/iostream.h")
    ("                 from test_clt.cc:1:"
     1 nil 1 "test_clt.cc")
    ;; gmake
    ("make: *** [Makefile:20: all] Error 2" 12 nil 20 "Makefile" 0)
    ("make[4]: *** [sub/make.mk:19: all] Error 127" 15 nil 19 "sub/make.mk" 0)
    ("gmake[4]: *** [sub/make.mk:19: all] Error 2" 16 nil 19 "sub/make.mk" 0)
    ("gmake-4.3[4]: *** [make.mk:1119: all] Error 2" 20 nil 1119 "make.mk" 0)
    ("Make-4.3: *** [make.INC:1119: dir/all] Error 2" 16 nil 1119 "make.INC" 0)
    ;; gnu
    ("foo.c:8: message" 1 nil 8 "foo.c")
    ("../foo.c:8: W: message" 1 nil 8 "../foo.c")
    ("/tmp/foo.c:8:warning message" 1 nil 8 "/tmp/foo.c")
    ("foo/bar.py:8: FutureWarning message" 1 nil 8 "foo/bar.py")
    ("foo.py:8: RuntimeWarning message" 1 nil 8 "foo.py")
    ("foo.c:8:I: message" 1 nil 8 "foo.c")
    ("foo.c:8.23: note: message" 1 23 8 "foo.c")
    ("foo.c:8.23: info: message" 1 23 8 "foo.c")
    ("foo.c:8:23:information: message" 1 23 8 "foo.c")
    ("foo.c:8.23-45: Informational: message" 1 (23 . 46) (8 . nil) "foo.c")
    ("foo.c:8-23: message" 1 nil (8 . 23) "foo.c")
    ;; The next one is not in the GNU standards AFAICS.
    ;; Here we seem to interpret it as LINE1-LINE2.COL2.
    ("foo.c:8-45.3: message" 1 (nil . 4) (8 . 45) "foo.c")
    ("foo.c:8.23-9.1: message" 1 (23 . 2) (8 . 9) "foo.c")
    ("jade:dbcommon.dsl:133:17:E: missing argument for function call"
     1 17 133 "dbcommon.dsl")
    ("G:/cygwin/dev/build-myproj.xml:54: Compiler Adapter 'javac' can't be found."
     1 nil 54 "G:/cygwin/dev/build-myproj.xml")
    ("file:G:/cygwin/dev/build-myproj.xml:54: Compiler Adapter 'javac' can't be found."
     1 nil 54 "G:/cygwin/dev/build-myproj.xml")
    ("{standard input}:27041: Warning: end of file not at end of a line; newline inserted"
     1 nil 27041 "{standard input}")
    ("boost/container/detail/flat_tree.hpp:589:25:   [ skipping 5 instantiation contexts, use -ftemplate-backtrace-limit=0 to disable ]"
     1 25 589 "boost/container/detail/flat_tree.hpp" 0)
    ;; gradle-kotlin
    ("e: /src/Test.kt: (34, 15): foo: bar" 4 15 34 "/src/Test.kt" 2)
    ("w: /src/Test.kt: (11, 98): foo: bar" 4 98 11 "/src/Test.kt" 1)
    ("e: e:/cygwin/src/Test.kt: (34, 15): foo: bar" 4 15 34 "e:/cygwin/src/Test.kt" 2)
    ("w: e:/cygwin/src/Test.kt: (11, 98): foo: bar" 4 98 11 "e:/cygwin/src/Test.kt" 1)
    ("e: e:\\src\\Test.kt: (34, 15): foo: bar" 4 15 34 "e:\\src\\Test.kt" 2)
    ("w: e:\\src\\Test.kt: (11, 98): foo: bar" 4 98 11 "e:\\src\\Test.kt" 1)
    ;; Guile
    ("In foo.scm:\n" 1 nil nil "foo.scm")
    ("  63:4 [call-with-prompt prompt0 ...]" 1 4 63 nil)
    ("1038: 1 [main (\"gud-break.scm\")]" 1 1 1038 nil)
    ;; lcc
    ("E, file.cc(35,52) Illegal operation on pointers" 1 52 35 "file.cc")
    ("W, file.cc(36,52) blah blah" 1 52 36 "file.cc")
    ;; makepp
    ("makepp: Scanning `/foo/bar.c'" 19 nil nil "/foo/bar.c")
    ("makepp: warning: bla bla `/foo/bar.c' and `/foo/bar.h'" 27 nil nil "/foo/bar.c")
    ("makepp: bla bla `/foo/Makeppfile:12' bla" 18 nil 12 "/foo/Makeppfile")
    ("makepp: bla bla `/foo/bar.c' and `/foo/bar.h'" 35 nil nil "/foo/bar.h")
    ;; maven
    ("FooBar.java:[111,53] no interface expected here"
     1 53 111 "FooBar.java" 2)
    ("[ERROR] /Users/cinsk/hello.java:[651,96] ';' expected"
     15 96 651 "/Users/cinsk/hello.java" 2) ;Bug#11517.
    ("[WARNING] /foo/bar/Test.java:[27,43] unchecked conversion"
     11 43 27 "/foo/bar/Test.java" 1) ;Bug#20556
    ;; mips-1 mips-2
    ("TrimMask (255) in solomon.c may be indistinguishable from TrimMasks (93) in solomo.c due to truncation"
     11 nil 255 "solomon.c")
    ("TrimMask (255) in solomon.c may be indistinguishable from TrimMasks (93) in solomo.c due to truncation"
     70 nil 93 "solomo.c")
    ("name defined but never used: LinInt in cmap_calc.c(199)"
     40 nil 199 "cmap_calc.c")
    ;; msft
    ("keyboard handler.c(537) : warning C4005: 'min' : macro redefinition"
     1 nil 537 "keyboard handler.c")
    ("d:\\tmp\\test.c(23) : error C2143: syntax error : missing ';' before 'if'"
     1 nil 23 "d:\\tmp\\test.c")
    ("d:\\tmp\\test.c(1145) : see declaration of 'nsRefPtr'"
     1 nil 1145 "d:\\tmp\\test.c")
    ("1>test_main.cpp(29): error C2144: syntax error : 'int' should be preceded by ';'"
     3 nil 29 "test_main.cpp")
    ("1>test_main.cpp(29): error C4430: missing type specifier - int assumed. Note: C++ does not support default-int"
     3 nil 29 "test_main.cpp")
    ;; watcom
    ("..\\src\\ctrl\\lister.c(109): Error! E1009: Expecting ';' but found '{'"
     1 nil 109 "..\\src\\ctrl\\lister.c")
    ("..\\src\\ctrl\\lister.c(120): Warning! W201: Unreachable code"
     1 nil 120 "..\\src\\ctrl\\lister.c")
    ;; omake
    ("      alpha.c:5:15: error: expected ';' after expression"
     1 15 5 "alpha.c")
    ;; oracle
    ("Semantic error at line 528, column 5, file erosacqdb.pc:"
     1 5 528 "erosacqdb.pc")
    ("Error at line 41, column 10 in file /usr/src/sb/ODBI_BHP.hpp"
     1 10 41 "/usr/src/sb/ODBI_BHP.hpp")
    ("PCC-02150: error at line 49, column 27 in file /usr/src/sb/ODBI_dxfgh.pc"
     1 27 49 "/usr/src/sb/ODBI_dxfgh.pc")
    ("PCC-00003: invalid SQL Identifier at column name in line 12 of file /usr/src/sb/ODBI_BHP.hpp"
     1 nil 12 "/usr/src/sb/ODBI_BHP.hpp")
    ("PCC-00004: mismatched IF/ELSE/ENDIF block at line 27 in file /usr/src/sb/ODBI_BHP.hpp"
     1 nil 27 "/usr/src/sb/ODBI_BHP.hpp")
    ("PCC-02151: line 21 column 40 file /usr/src/sb/ODBI_BHP.hpp:"
     1 40 21 "/usr/src/sb/ODBI_BHP.hpp")
    ;; perl
    ("syntax error at automake line 922, near \"':'\""
     14 nil 922 "automake")
    ("Died at test.pl line 27."
     6 nil 27 "test.pl")
    ("store::odrecall('File_A', 'x2') called at store.pm line 90"
     40 nil 90 "store.pm")
    ("\t(in cleanup) something bad at foo.pl line 3 during global destruction."
     29 nil 3 "foo.pl")
    ("GLib-GObject-WARNING **: /build/buildd/glib2.0-2.14.5/gobject/gsignal.c:1741: instance `0x8206790' has no handler with id `1234' at t-compilation-perl-gtk.pl line 3."
     130 nil 3 "t-compilation-perl-gtk.pl")
    ;; php
    ("Parse error: parse error, unexpected $ in main.php on line 59"
     1 nil 59 "main.php")
    ("Fatal error: Call to undefined function: mysql_pconnect() in db.inc on line 66"
     1 nil 66 "db.inc")
    ;; ruby
    ("plain-exception.rb:7:in `fun': unhandled exception"
     1 nil 7 "plain-exception.rb")
    ("\tfrom plain-exception.rb:3:in `proxy'" 2 nil 3 "plain-exception.rb")
    ("\tfrom plain-exception.rb:12" 2 nil 12 "plain-exception.rb")
    ;; ruby-Test::Unit
    ;; FIXME
    ("    [examples/test-unit.rb:28:in `here_is_a_deep_assert'"
     5 nil 28 "examples/test-unit.rb")
    ("     examples/test-unit.rb:19:in `test_a_deep_assert']:"
     6 nil 19 "examples/test-unit.rb")
    ("examples/test-unit.rb:10:in `test_assert_raise'"
     1 nil 10 "examples/test-unit.rb")
    ;; rxp
    ("Error: Mismatched end tag: expected </geroup>, got </group>\nin unnamed entity at line 71 char 8 of file:///home/reto/test/group.xml"
     1 8 71 "/home/reto/test/group.xml")
    ("Warning: Start tag for undeclared element geroup\nin unnamed entity at line 4 char 8 of file:///home/reto/test/group.xml"
     1 8 4 "/home/reto/test/group.xml")
    ;; sparc-pascal-file sparc-pascal-line sparc-pascal-example
    ("Thu May 14 10:46:12 1992  mom3.p:"
     1 nil nil "mom3.p")
    ;; sun
    ("cc-1020 CC: REMARK File = CUI_App.h, Line = 735"
     13 nil 735 "CUI_App.h")
    ("cc-1070 cc: WARNING File = linkl.c, Line = 38"
     13 nil 38 "linkl.c")
    ("cf90-113 f90comp: ERROR NSE, File = Hoved.f90, Line = 16, Column = 3"
     18 3 16 "Hoved.f90")
    ;; sun-ada
    ("/home3/xdhar/rcds_rc/main.a, line 361, char 6:syntax error: \",\" inserted"
     1 6 361 "/home3/xdhar/rcds_rc/main.a")
    ;; 4bsd
    ("/usr/src/foo/foo.c(8): warning: w may be used before set"
     1 nil 8 "/usr/src/foo/foo.c")
    ("/usr/src/foo/foo.c(9): error: w is used before set"
     1 nil 9 "/usr/src/foo/foo.c")
    ("strcmp: variable # of args. llib-lc(359)  ::  /usr/src/foo/foo.c(8)"
     44 nil 8 "/usr/src/foo/foo.c")
    ("bloofle defined( /users/wolfgang/foo.c(4) ), but never used"
     18 nil 4 "/users/wolfgang/foo.c")
    ;; perl--Pod::Checker
    ;; FIXME
    ;; *** ERROR: Spurious text after =cut at line 193 in file foo.pm
    ;; *** ERROR: =over on line 37 without closing =back at line EOF in file bar.pm
    ;; *** ERROR: =over on line 1 without closing =back (at head1) at line 3 in file x.pod
    ;; perl--Test
    ("# Failed test 1 in foo.t at line 6"
     1 nil 6 "foo.t")
    ;; perl--Test::Harness
    ("NOK 1# Test 1 got: \"1234\" (t/foo.t at line 46)"
     1 nil 46 "t/foo.t")
    ;; weblint
    ("index.html (13:1) Unknown element <fdjsk>"
     1 1 13 "index.html"))
  "List of tests for `compilation-error-regexp-alist'.
Each element has the form (STR POS COLUMN LINE FILENAME [TYPE]),
where STR is an error string, POS is the position of the error in
STR, COLUMN and LINE are the reported column and line numbers (or
nil) for that error, FILENAME is the reported filename, and TYPE
is 0 for an information message, 1 for a warning, and 2 for an
error.

LINE can also be of the form (LINE . END-LINE) meaning a range of
lines.  COLUMN can also be of the form (COLUMN . END-COLUMN)
meaning a range of columns starting on LINE and ending on
END-LINE, if that matched.  TYPE can be left out, in which case
any message type is accepted.")

(defconst compile-tests--grep-regexp-testcases
  ;; Bug#32051.
  '(("c:/Users/my.name/src/project\\src\\kbhit.hpp\0\ 29:#include <termios.h>"
     1 nil 29 "c:/Users/my.name/src/project\\src\\kbhit.hpp")
    ("d:/gnu/emacs/branch/src/callproc.c\0\ 214:#ifdef DOS_NT"
     1 nil 214 "d:/gnu/emacs/branch/src/callproc.c")
    ("/gnu/emacs/branch/src/callproc.c\0\ 214:#ifdef DOS_NT"
     1 nil 214 "/gnu/emacs/branch/src/callproc.c"))
  "List of tests for `grep-regexp-list'.
The format is the same as `compile-tests--test-regexps-data', but
the match is expected to be the same when NUL bytes are replaced
with colon.")

(defconst compile-tests--grep-regexp-tricky-testcases
  ;; Bug#7378.
  '(("./x11-libs---nx/3.4.0:0:C.30253.1289557929.792611.C/nx-3.4.0.exheres-0\0\ 42:some text"
     1 nil 42 "./x11-libs---nx/3.4.0:0:C.30253.1289557929.792611.C/nx-3.4.0.exheres-0")
    ("2011-08-31_11:57:03_1\0\ 7:Date: Wed, 31 Aug 2011 11:57:03 +0000"
     1 nil 7 "2011-08-31_11:57:03_1"))
  "List of tricky tests for `grep-regexp-list'.
Same as `compile-tests--grep-regexp-testcases', but these cases
can only work with the NUL byte to disambiguate colons.")

(defun compile--test-error-line (test)
  (erase-buffer)
  (setq compilation-locs (make-hash-table))
  (insert (car test))
  (compilation-parse-errors (point-min) (point-max))
  (let ((msg (get-text-property (nth 1 test) 'compilation-message)))
    (should msg)
    (let ((loc (compilation--message->loc msg))
          (col  (nth 2 test))
          (line (nth 3 test))
          (file (nth 4 test))
          (type (nth 5 test))
          end-col end-line)
      (if (consp col)
          (setq end-col (cdr col) col (car col)))
      (if (consp line)
          (setq end-line (cdr line) line (car line)))
      (should (equal (compilation--loc->col loc) col))
      (should (equal (compilation--loc->line loc) line))
      (when file
        (should (equal (caar (compilation--loc->file-struct loc)) file)))
      (when end-col
        (should (equal (car (cadr (nth 2 (compilation--loc->file-struct loc))))
                       end-col)))
      (should (equal (car (nth 2 (compilation--loc->file-struct loc)))
                     (or end-line line)))
      (when type
        (should (equal type (compilation--message->type msg)))))
    msg))

(ert-deftest compile-test-error-regexps ()
  "Test the `compilation-error-regexp-alist' regexps.
The test data is in `compile-tests--test-regexps-data'."
  (with-temp-buffer
    (font-lock-mode -1)
    (let ((compilation-num-errors-found 0)
          (compilation-num-warnings-found 0)
          (compilation-num-infos-found 0))
      (mapc #'compile--test-error-line compile-tests--test-regexps-data)
      (should (eq compilation-num-errors-found 93))
      (should (eq compilation-num-warnings-found 36))
      (should (eq compilation-num-infos-found 26)))))

(ert-deftest compile-test-grep-regexps ()
  "Test the `grep-regexp-alist' regexps.
The test data is in `compile-tests--grep-regexp-testcases'."
  (with-temp-buffer
    (grep-mode)
    (setq buffer-read-only nil)
    (font-lock-mode -1)
    (dolist (testcase compile-tests--grep-regexp-testcases)
      (let (msg1 msg2)
        (setq msg1 (ert-info ((format "%S" testcase) :prefix "testcase: ")
                     (compile--test-error-line testcase)))
        ;; Make sure replacing the NUL character with a colon still matches.
        (setf (car testcase) (replace-regexp-in-string "\0" ":" (car testcase)))
        (setq msg2 (ert-info ((format "%S" testcase) :prefix "testcase: ")
                     (compile--test-error-line testcase)))
        (should (equal msg1 msg2))))
    (dolist (testcase compile-tests--grep-regexp-tricky-testcases)
      (ert-info ((format "%S" testcase) :prefix "testcase: ")
        (compile--test-error-line testcase)))
    (should (eq compilation-num-errors-found 8))))

;;; compile-tests.el ends here
