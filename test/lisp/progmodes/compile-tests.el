;;; compile-tests.el --- Test suite for compile.el.  -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2023 Free Software Foundation, Inc.

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
  '(;; absoft
    (absoft
     "Error on line 3 of t.f: Execution error unclassifiable statement"
     1 nil 3 "t.f")
    (absoft "Line 45 of \"foo.c\": bloofle undefined"
     1 nil 45 "foo.c")
    (absoft "error on line 19 of fplot.f: spelling error?"
     1 nil 19 "fplot.f")
    (absoft
     "warning on line 17 of fplot.f: data type is undefined for variable d"
     1 nil 17 "fplot.f")
    ;; Ada & Mpatrol
    (gnu "foo.adb:61:11:  [...] in call to size declared at foo.ads:11"
     1 11 61 "foo.adb")
    (ada "foo.adb:61:11:  [...] in call to size declared at foo.ads:11"
     52 nil 11 "foo.ads")
    (ada "     0x8008621 main+16 at error.c:17"
     23 nil 17 "error.c")
    ;; aix
    (aix "****** Error number 140 in line 8 of file errors.c ******"
     25 nil 8 "errors.c")
    ;; ant
    (ant "[javac] /src/DataBaseTestCase.java:27: unreported exception ..."
     13 nil 27 "/src/DataBaseTestCase.java" 2)
    (ant "[javac] /src/DataBaseTestCase.java:49: warning: finally clause cannot complete normally"
     13 nil 49 "/src/DataBaseTestCase.java" 1)
    (ant "[jikes]  foo.java:3:5:7:9: blah blah"
     14 (5 . 9) (3 . 7) "foo.java" 2)
    (ant "[javac] c:/cygwin/Test.java:12: error: foo: bar"
     9 nil 12 "c:/cygwin/Test.java" 2)
    (ant "[javac] c:\\cygwin\\Test.java:87: error: foo: bar"
     9 nil 87 "c:\\cygwin\\Test.java" 2)
    ;; Checkstyle error, but ant reports a warning (note additional
    ;; severity level after task name)
    (ant "[checkstyle] [ERROR] /src/Test.java:38: warning: foo"
     22 nil 38 "/src/Test.java" 1)
    ;; bash
    (bash "a.sh: line 1: ls-l: command not found"
     1 nil 1 "a.sh")
    ;; borland
    (borland "Error ping.c 15: Unable to open include file 'sys/types.h'"
     1 nil 15 "ping.c")
    (borland "Warning pong.c 68: Call to function 'func' with no prototype"
     1 nil 68 "pong.c")
    (borland "Error E2010 ping.c 15: Unable to open include file 'sys/types.h'"
     1 nil 15 "ping.c")
    (borland
     "Warning W1022 pong.c 68: Call to function 'func' with no prototype"
     1 nil 68 "pong.c")
    ;; caml
    (python-tracebacks-and-caml
     "File \"foobar.ml\", lines 5-8, characters 20-155: blah blah"
     1 (20 . 155) (5 . 8) "foobar.ml")
    (python-tracebacks-and-caml
     "File \"F:\\ocaml\\sorting.ml\", line 65, characters 2-145:\nWarning 26: unused variable equ."
     1 (2 . 145) 65 "F:\\ocaml\\sorting.ml")
    (python-tracebacks-and-caml
     "File \"/usr/share/gdesklets/display/TargetGauge.py\", line 41, in add_children"
     1 nil 41 "/usr/share/gdesklets/display/TargetGauge.py")
    (python-tracebacks-and-caml
     "File \\lib\\python\\Products\\PythonScripts\\PythonScript.py, line 302, in _exec"
     1 nil 302 "\\lib\\python\\Products\\PythonScripts\\PythonScript.py")
    (python-tracebacks-and-caml
     "File \"/tmp/foo.py\", line 10"
     1 nil 10 "/tmp/foo.py")
    ;; clang-include
    (clang-include "In file included from foo.cpp:2:"
     1 nil 2 "foo.cpp" 0)
    ;; cmake cmake-info
    (cmake "CMake Error at CMakeLists.txt:23 (hurz):"
     1 nil 23 "CMakeLists.txt")
    (cmake "CMake Warning at cmake/modules/UseUG.cmake:73 (find_package):"
     1 nil 73 "cmake/modules/UseUG.cmake")
    (cmake-info "  cmake/modules/DuneGridMacros.cmake:19 (include)"
     1 nil 19 "cmake/modules/DuneGridMacros.cmake")
    ;; comma
    (comma "\"foo.f\", line 3: Error: syntax error near end of statement"
     1 nil 3 "foo.f")
    (comma "\"vvouch.c\", line 19.5: 1506-046 (S) Syntax error."
     1 5 19 "vvouch.c")
    (comma "\"foo.c\", line 32 pos 1; (E) syntax error; unexpected symbol: \"lossage\""
     1 1 32 "foo.c")
    (comma "\"foo.adb\", line 2(11): warning: file name does not match ..."
     1 11 2 "foo.adb")
    (comma
     "\"src/swapping.c\", line 30.34: 1506-342 (W) \"/*\" detected in comment."
     1 34 30 "src/swapping.c")
    ;; cucumber
    (cucumber "Scenario: undefined step  # features/cucumber.feature:3"
     29 nil 3 "features/cucumber.feature")
    ;; This rule is actually handled by the `cucumber' pattern but when
    ;; `omake' is included, then `gnu' matches it first.
    (gnu "      /home/gusev/.rvm/foo/bar.rb:500:in `_wrap_assertion'"
     1 nil 500 "/home/gusev/.rvm/foo/bar.rb")
    ;; edg-1 edg-2
    (edg-1 "build/intel/debug/../../../struct.cpp(42): error: identifier \"foo\" is undefined"
     1 nil 42 "build/intel/debug/../../../struct.cpp")
    (edg-1 "build/intel/debug/struct.cpp(44): warning #1011: missing return statement at end of"
     1 nil 44 "build/intel/debug/struct.cpp")
    (edg-1 "build/intel/debug/iptr.h(302): remark #981: operands are evaluated in unspecified order"
     1 nil 302 "build/intel/debug/iptr.h")
    (edg-2 "   detected during ... at line 62 of \"build/intel/debug/../../../trace.h\""
     31 nil 62 "build/intel/debug/../../../trace.h")
    ;; epc
    (epc "Error 24 at (2:progran.f90) : syntax error"
     1 nil 2 "progran.f90")
    ;; ftnchek
    (ftnchek "    Dummy arg W in module SUBA line 8 file arrayclash.f is array"
     32 nil 8 "arrayclash.f")
    (ftnchek "    L4 used at line 55 file test/assign.f; never set"
     16 nil 55 "test/assign.f")
    (ftnchek
     "Warning near line 10 file arrayclash.f: Module contains no executable"
     1 nil 10 "arrayclash.f")
    (ftnchek "Nonportable usage near line 31 col 9 file assign.f: mixed default and explicit"
     24 9 31 "assign.f")
    ;; iar
    (iar "\"foo.c\",3  Error[32]: Error message"
     1 nil 3 "foo.c")
    (iar "\"foo.c\",3  Warning[32]: Error message"
     1 nil 3 "foo.c")
    ;; ibm
    (ibm "foo.c(2:0) : informational EDC0804: Function foo is not referenced."
     1 0 2 "foo.c")
    (ibm "foo.c(3:8) : warning EDC0833: Implicit return statement encountered."
     1 8 3 "foo.c")
    (ibm "foo.c(5:5) : error EDC0350: Syntax error."
     1 5 5 "foo.c")
    ;; irix
    (irix "ccom: Error: foo.c, line 2: syntax error"
     1 nil 2 "foo.c")
    (irix "cc: Severe: /src/Python-2.3.3/Modules/_curses_panel.c, line 17: Cannot find file <panel.h> ..."
     1 nil 17 "/src/Python-2.3.3/Modules/_curses_panel.c")
    (irix "cc: Info: foo.c, line 27: ..."
     1 nil 27 "foo.c")
    (irix
     "cfe: Warning 712: foo.c, line 2: illegal combination of pointer and ..."
     1 nil 2 "foo.c")
    (irix
     "cfe: Warning 600: xfe.c: 170: Not in a conditional directive while ..."
     1 nil 170 "xfe.c")
    (irix "/usr/lib/cmplrs/cc/cfe: Error: foo.c: 1: blah blah"
     1 nil 1 "foo.c")
    (irix "/usr/lib/cmplrs/cc/cfe: warning: foo.c: 1: blah blah"
     1 nil 1 "foo.c")
    (irix "foo bar: baz.f, line 27: ..."
     1 nil 27 "baz.f")
    ;; java
    (java "\tat org.foo.ComponentGateway.doGet(ComponentGateway.java:172)"
     5 nil 172 "ComponentGateway.java")
    (java "\tat javax.servlet.http.HttpServlet.service(HttpServlet.java:740)"
     5 nil 740 "HttpServlet.java")
    (java "==1332==    at 0x4040743C: System::getErrorString() (../src/Lib/System.cpp:217)"
     13 nil 217 "../src/Lib/System.cpp")
    (java "==1332==    by 0x8008621: main (vtest.c:180)"
     13 nil 180 "vtest.c")
    ;; javac
    (javac
     "/src/Test.java:5: ';' expected\n        foo foo\n               ^\n"
     1 16 5 "/src/Test.java" 2)
    (javac
     "e:\\src\\Test.java:7: warning: ';' expected\n   foo foo\n          ^\n"
     1 11 7 "e:\\src\\Test.java" 1)
    ;; jikes-file jikes-line
    (jikes-file
     "Found 2 semantic errors compiling \"../javax/swing/BorderFactory.java\":"
     1 nil nil "../javax/swing/BorderFactory.java")
    (jikes-file "Issued 1 semantic warning compiling \"java/awt/Toolkit.java\":"
     1 nil nil "java/awt/Toolkit.java")
    ;; gcc-include
    (gcc-include "In file included from /usr/include/c++/3.3/backward/warn.h:4,"
     1 nil 4 "/usr/include/c++/3.3/backward/warn.h")
    (gcc-include
     "                 from /usr/include/c++/3.3/backward/iostream.h:31:0,"
     1 0 31 "/usr/include/c++/3.3/backward/iostream.h")
    (gcc-include "                 from test_clt.cc:1:"
     1 nil 1 "test_clt.cc")
    ;; gmake
    (gmake "make: *** [Makefile:20: all] Error 2" 12 nil 20 "Makefile" 0)
    (gmake "make[4]: *** [sub/make.mk:19: all] Error 127" 15 nil 19
           "sub/make.mk" 0)
    (gmake "gmake[4]: *** [sub/make.mk:19: all] Error 2" 16 nil 19
           "sub/make.mk" 0)
    (gmake "gmake-4.3[4]: *** [make.mk:1119: all] Error 2" 20 nil 1119
           "make.mk" 0)
    (gmake "Make-4.3: *** [make.INC:1119: dir/all] Error 2" 16 nil 1119
           "make.INC" 0)
    ;; gnu
    (gnu "foo.c:8: message" 1 nil 8 "foo.c")
    (gnu "../foo.c:8: W: message" 1 nil 8 "../foo.c")
    (gnu "/tmp/foo.c:8:warning message" 1 nil 8 "/tmp/foo.c")
    (gnu "foo/bar.py:8: FutureWarning message" 1 nil 8 "foo/bar.py")
    (gnu "foo.py:8: RuntimeWarning message" 1 nil 8 "foo.py")
    (gnu "foo.c:8:I: message" 1 nil 8 "foo.c")
    (gnu "foo.c:8.23: note: message" 1 23 8 "foo.c")
    (gnu "foo.c:8.23: info: message" 1 23 8 "foo.c")
    (gnu "foo.c:8:23:information: message" 1 23 8 "foo.c")
    (gnu "foo.c:8.23-45: Informational: message" 1 (23 . 45) (8 . nil) "foo.c")
    (gnu "foo.c:8-23: message" 1 nil (8 . 23) "foo.c")
    (gnu "   |foo.c:8: message" 1 nil 8 "foo.c")
    ;; The next one is not in the GNU standards AFAICS.
    ;; Here we seem to interpret it as LINE1-LINE2.COL2.
    (gnu "foo.c:8-45.3: message" 1 (nil . 3) (8 . 45) "foo.c")
    (gnu "foo.c:8.23-9.1: message" 1 (23 . 1) (8 . 9) "foo.c")
    (gnu "jade:dbcommon.dsl:133:17:E: missing argument for function call"
     1 17 133 "dbcommon.dsl")
    (gnu "G:/cygwin/dev/build-myproj.xml:54: Compiler Adapter 'javac' can't be found."
     1 nil 54 "G:/cygwin/dev/build-myproj.xml")
    (gnu "file:G:/cygwin/dev/build-myproj.xml:54: Compiler Adapter 'javac' can't be found."
     1 nil 54 "G:/cygwin/dev/build-myproj.xml")
    (gnu "{standard input}:27041: Warning: end of file not at end of a line; newline inserted"
     1 nil 27041 "{standard input}")
    (gnu "boost/container/detail/flat_tree.hpp:589:25:   [ skipping 5 instantiation contexts, use -ftemplate-backtrace-limit=0 to disable ]"
     1 25 589 "boost/container/detail/flat_tree.hpp" 0)
    ;; gradle-kotlin
    (gradle-kotlin
     "e: /src/Test.kt: (34, 15): foo: bar" 4 15 34 "/src/Test.kt" 2)
    (gradle-kotlin
     "w: /src/Test.kt: (11, 98): foo: bar" 4 98 11 "/src/Test.kt" 1)
    (gradle-kotlin
     "e: e:/cygwin/src/Test.kt: (34, 15): foo: bar"
     4 15 34 "e:/cygwin/src/Test.kt" 2)
    (gradle-kotlin
     "w: e:/cygwin/src/Test.kt: (11, 98): foo: bar"
     4 98 11 "e:/cygwin/src/Test.kt" 1)
    (gradle-kotlin
     "e: e:\\src\\Test.kt: (34, 15): foo: bar" 4 15 34 "e:\\src\\Test.kt" 2)
    (gradle-kotlin
     "w: e:\\src\\Test.kt: (11, 98): foo: bar" 4 98 11 "e:\\src\\Test.kt" 1)
    (gradle-android
     "     ERROR:/Users/salutis/src/AndroidSchemeExperiment/app/build/intermediates/incremental/debug/mergeDebugResources/stripped.dir/layout/item.xml:3: AAPT: error: '16dpw' is incompatible with attribute padding (attr) dimension."
     1 nil 3 "/Users/salutis/src/AndroidSchemeExperiment/app/build/intermediates/incremental/debug/mergeDebugResources/stripped.dir/layout/item.xml" 2)
    ;; Guile
    (guile-file "In foo.scm:\n" 1 nil nil "foo.scm")
    (guile-line "  63:4 [call-with-prompt prompt0 ...]" 1 4 63 nil)
    (guile-line "1038: 1 [main (\"gud-break.scm\")]" 1 1 1038 nil)
    ;; lcc
    (lcc "E, file.cc(35,52) Illegal operation on pointers" 1 52 35 "file.cc")
    (lcc "W, file.cc(36,52) blah blah" 1 52 36 "file.cc")
    ;; makepp
    (makepp "makepp: Scanning `/foo/bar.c'" 19 nil nil "/foo/bar.c")
    (makepp "makepp: warning: bla bla `/foo/bar.c' and `/foo/bar.h'"
            27 nil nil "/foo/bar.c")
    (makepp "makepp: bla bla `/foo/Makeppfile:12' bla"
            18 nil 12 "/foo/Makeppfile")
    (nil "makepp: bla bla `/foo/bar.c' and `/foo/bar.h'"
         35 nil nil "/foo/bar.h")
    ;; maven
    (maven "FooBar.java:[111,53] no interface expected here"
     1 53 111 "FooBar.java" 2)
    (maven "[ERROR] /Users/cinsk/hello.java:[651,96] ';' expected"
     15 96 651 "/Users/cinsk/hello.java" 2) ;Bug#11517.
    (maven "[WARNING] /foo/bar/Test.java:[27,43] unchecked conversion"
     11 43 27 "/foo/bar/Test.java" 1) ;Bug#20556
    ;; mips-1 mips-2
    (mips-1 "TrimMask (255) in solomon.c may be indistinguishable from TrimMasks (93) in solomo.c due to truncation"
     11 nil 255 "solomon.c")
    (mips-1 "TrimMask (255) in solomon.c may be indistinguishable from TrimMasks (93) in solomo.c due to truncation"
     70 nil 93 "solomo.c")
    (mips-2 "name defined but never used: LinInt in cmap_calc.c(199)"
     40 nil 199 "cmap_calc.c")
    ;; msft
    (msft "keyboard handler.c(537) : warning C4005: 'min' : macro redefinition"
     1 nil 537 "keyboard handler.c")
    (msft
     "d:\\tmp\\test.c(23) : error C2143: syntax error : missing ';' before 'if'"
     1 nil 23 "d:\\tmp\\test.c")
    (msft "d:\\tmp\\test.c(1145) : see declaration of 'nsRefPtr'"
     1 nil 1145 "d:\\tmp\\test.c")
    (msft "1>test_main.cpp(29): error C2144: syntax error : 'int' should be preceded by ';'"
     3 nil 29 "test_main.cpp")
    (msft "1>test_main.cpp(29): error C4430: missing type specifier - int assumed. Note: C++ does not support default-int"
     3 nil 29 "test_main.cpp")
    (msft "C:\\tmp\\test.cpp(101,11): error C4101: 'bias0123': unreferenced local variable [C:\\tmp\\project.vcxproj]"
     1 11 101 "C:\\tmp\\test.cpp")
    ;; watcom
    (watcom
     "..\\src\\ctrl\\lister.c(109): Error! E1009: Expecting ';' but found '{'"
     1 nil 109 "..\\src\\ctrl\\lister.c")
    (watcom "..\\src\\ctrl\\lister.c(120): Warning! W201: Unreachable code"
     1 nil 120 "..\\src\\ctrl\\lister.c")
    ;; omake
    ;; FIXME: This doesn't actually test the omake rule.
    (gnu "      alpha.c:5:15: error: expected ';' after expression"
     1 15 5 "alpha.c")
    ;; oracle
    (oracle "Semantic error at line 528, column 5, file erosacqdb.pc:"
     1 5 528 "erosacqdb.pc")
    (oracle "Error at line 41, column 10 in file /usr/src/sb/ODBI_BHP.hpp"
     1 10 41 "/usr/src/sb/ODBI_BHP.hpp")
    (oracle
     "PCC-02150: error at line 49, column 27 in file /usr/src/sb/ODBI_dxfgh.pc"
     1 27 49 "/usr/src/sb/ODBI_dxfgh.pc")
    (oracle "PCC-00003: invalid SQL Identifier at column name in line 12 of file /usr/src/sb/ODBI_BHP.hpp"
     1 nil 12 "/usr/src/sb/ODBI_BHP.hpp")
    (oracle "PCC-00004: mismatched IF/ELSE/ENDIF block at line 27 in file /usr/src/sb/ODBI_BHP.hpp"
     1 nil 27 "/usr/src/sb/ODBI_BHP.hpp")
    (oracle "PCC-02151: line 21 column 40 file /usr/src/sb/ODBI_BHP.hpp:"
     1 40 21 "/usr/src/sb/ODBI_BHP.hpp")
    ;; perl
    (perl "syntax error at automake line 922, near \"':'\""
     14 nil 922 "automake")
    (perl "Died at test.pl line 27."
     6 nil 27 "test.pl")
    (perl "store::odrecall('File_A', 'x2') called at store.pm line 90"
     40 nil 90 "store.pm")
    (perl
     "\t(in cleanup) something bad at foo.pl line 3 during global destruction."
     29 nil 3 "foo.pl")
    (perl "GLib-GObject-WARNING **: /build/buildd/glib2.0-2.14.5/gobject/gsignal.c:1741: instance `0x8206790' has no handler with id `1234' at t-compilation-perl-gtk.pl line 3."
     130 nil 3 "t-compilation-perl-gtk.pl")
    ;; php
    (php "Parse error: parse error, unexpected $ in main.php on line 59"
     1 nil 59 "main.php")
    (php "Fatal error: Call to undefined function: mysql_pconnect() in db.inc on line 66"
     1 nil 66 "db.inc")
    ;; ruby (uses gnu)
    (gnu "plain-exception.rb:7:in `fun': unhandled exception"
     1 nil 7 "plain-exception.rb")
    (gcc-include
     "\tfrom plain-exception.rb:3:in `proxy'" 2 nil 3 "plain-exception.rb")
    (gcc-include "\tfrom plain-exception.rb:12" 2 nil 12 "plain-exception.rb")
    ;; ruby-Test::Unit
    ;; FIXME
    (ruby-Test::Unit "    [examples/test-unit.rb:28:in `here_is_a_deep_assert'"
     5 nil 28 "examples/test-unit.rb")
    (ruby-Test::Unit "     examples/test-unit.rb:19:in `test_a_deep_assert']:"
     6 nil 19 "examples/test-unit.rb")
    (gnu "examples/test-unit.rb:10:in `test_assert_raise'"
     1 nil 10 "examples/test-unit.rb")
    ;; rxp
    (rxp "Error: Mismatched end tag: expected </geroup>, got </group>\nin unnamed entity at line 71 char 8 of file:///home/reto/test/group.xml"
     1 8 71 "/home/reto/test/group.xml")
    (rxp "Warning: Start tag for undeclared element geroup\nin unnamed entity at line 4 char 8 of file:///home/reto/test/group.xml"
     1 8 4 "/home/reto/test/group.xml")
    ;; shellcheck
    (shellcheck "In autogen.sh line 48:"
     1 nil 48 "autogen.sh")
    ;; sparc-pascal-file sparc-pascal-line sparc-pascal-example
    (sparc-pascal-file "Thu May 14 10:46:12 1992  mom3.p:"
     1 nil nil "mom3.p")
    ;; sun
    (sun "cc-1020 CC: REMARK File = CUI_App.h, Line = 735"
     13 nil 735 "CUI_App.h")
    (sun "cc-1070 cc: WARNING File = linkl.c, Line = 38"
     13 nil 38 "linkl.c")
    (sun "cf90-113 f90comp: ERROR NSE, File = Hoved.f90, Line = 16, Column = 3"
     18 3 16 "Hoved.f90")
    ;; sun-ada
    (sun-ada "/home3/xdhar/rcds_rc/main.a, line 361, char 6:syntax error: \",\" inserted"
     1 6 361 "/home3/xdhar/rcds_rc/main.a")
    (typescript-tsc-plain "/home/foo/greeter.ts(30,12): error TS2339: Property 'foo' does not exist."
     1 12 30 "/home/foo/greeter.ts")
    (typescript-tsc-pretty "src/resources/document.ts:140:22 - error TS2362: something."
     1 22 140 "src/resources/document.ts")
    ;; 4bsd
    (edg-1 "/usr/src/foo/foo.c(8): warning: w may be used before set"
     1 nil 8 "/usr/src/foo/foo.c")
    (edg-1 "/usr/src/foo/foo.c(9): error: w is used before set"
     1 nil 9 "/usr/src/foo/foo.c")
    (4bsd "strcmp: variable # of args. llib-lc(359)  ::  /usr/src/foo/foo.c(8)"
     44 nil 8 "/usr/src/foo/foo.c")
    (4bsd "bloofle defined( /users/wolfgang/foo.c(4) ), but never used"
     18 nil 4 "/users/wolfgang/foo.c")
    ;; perl--Pod::Checker
    ;; FIXME
    ;; *** ERROR: Spurious text after =cut at line 193 in file foo.pm
    ;; *** ERROR: =over on line 37 without closing =back at line EOF in file bar.pm
    ;; *** ERROR: =over on line 1 without closing =back (at head1) at line 3 in file x.pod
    ;; perl--Test
    (perl--Test "# Failed test 1 in foo.t at line 6"
     1 nil 6 "foo.t")
    ;; perl--Test::Harness
    (perl--Test2 "NOK 1# Test 1 got: \"1234\" (t/foo.t at line 46)"
     1 nil 46 "t/foo.t")
    ;; weblint
    (weblint "index.html (13:1) Unknown element <fdjsk>"
     1 1 13 "index.html"))
  "List of tests for `compilation-error-regexp-alist'.
Each element has the form (RULE STR POS COLUMN LINE FILENAME
[TYPE]), where RULE is the rule (as a symbol), STR is an error
string, POS is the position of the error in STR, COLUMN and LINE
are the reported column and line numbers (or nil) for that error,
FILENAME is the reported filename, and TYPE is 0 for an
information message, 1 for a warning, and 2 for an error.

LINE can also be of the form (LINE . END-LINE) meaning a range of
lines.  COLUMN can also be of the form (COLUMN . END-COLUMN)
meaning a range of columns starting on LINE and ending on
END-LINE, if that matched.  TYPE can be left out, in which case
any message type is accepted.")

(defconst compile-tests--grep-regexp-testcases
  ;; Bug#32051.
  '((nil
     "c:/Users/my.name/src/project\\src\\kbhit.hpp\0\ 29:#include <termios.h>"
     1 nil 29 "c:/Users/my.name/src/project\\src\\kbhit.hpp")
    (nil
     "d:/gnu/emacs/branch/src/callproc.c\0\ 214:#ifdef DOS_NT"
     1 nil 214 "d:/gnu/emacs/branch/src/callproc.c")
    (nil
     "/gnu/emacs/branch/src/callproc.c\0\ 214:#ifdef DOS_NT"
     1 nil 214 "/gnu/emacs/branch/src/callproc.c"))
  "List of tests for `grep-regexp-list'.
The format is the same as `compile-tests--test-regexps-data', but
the match is expected to be the same when NUL bytes are replaced
with colon.")

(defconst compile-tests--grep-regexp-tricky-testcases
  ;; Bug#7378.
  '((nil
     "./x11-libs---nx/3.4.0:0:C.30253.1289557929.792611.C/nx-3.4.0.exheres-0\0\ 42:some text"
     1 nil 42 "./x11-libs---nx/3.4.0:0:C.30253.1289557929.792611.C/nx-3.4.0.exheres-0")
    (nil
     "2011-08-31_11:57:03_1\0\ 7:Date: Wed, 31 Aug 2011 11:57:03 +0000"
     1 nil 7 "2011-08-31_11:57:03_1"))
  "List of tricky tests for `grep-regexp-list'.
Same as `compile-tests--grep-regexp-testcases', but these cases
can only work with the NUL byte to disambiguate colons.")

(defun compile--test-error-line (test)
  (ert-info ((format "%S" test) :prefix "testcase: ")
    (erase-buffer)
    (setq compilation-locs (make-hash-table))
    (let ((rule (nth 0 test))
          (str (nth 1 test))
          (pos (nth 2 test))
          (col  (nth 3 test))
          (line (nth 4 test))
          (file (nth 5 test))
          (type (nth 6 test)))
      (insert str)
      (compilation-parse-errors (point-min) (point-max))
      (let ((msg (get-text-property pos 'compilation-message)))
        (should msg)
        (let ((loc (compilation--message->loc msg))
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
            ;; The computed END-COL is exclusive; subtract one to get the
            ;; number in the error message.
            (should (equal
                     (1- (car (cadr
                               (nth 2 (compilation--loc->file-struct loc)))))
                     end-col)))
          (should (equal (car (nth 2 (compilation--loc->file-struct loc)))
                         (or end-line line)))
          (when type
            (should (equal type (compilation--message->type msg))))
          (should (equal rule (compilation--message->rule msg))))
        msg))))

(ert-deftest compile-test-error-regexps ()
  "Test the `compilation-error-regexp-alist' regexps.
The test data is in `compile-tests--test-regexps-data'."
  (with-temp-buffer
    (font-lock-mode -1)
    (let ((compilation-num-errors-found 0)
          (compilation-num-warnings-found 0)
          (compilation-num-infos-found 0))
      (mapc #'compile--test-error-line compile-tests--test-regexps-data)
      (should (eq compilation-num-errors-found 100))
      (should (eq compilation-num-warnings-found 35))
      (should (eq compilation-num-infos-found 28)))))

(ert-deftest compile-test-grep-regexps ()
  "Test the `grep-regexp-alist' regexps.
The test data is in `compile-tests--grep-regexp-testcases'."
  (with-temp-buffer
    (grep-mode)
    (setq buffer-read-only nil)
    (font-lock-mode -1)
    (dolist (testcase compile-tests--grep-regexp-testcases)
      (let (msg1 msg2)
        (setq msg1 (compile--test-error-line testcase))
        ;; Make sure replacing the NUL character with a colon still matches.
        (let ((testcase2 (copy-sequence testcase)))
          (setf (nth 1 testcase2)
                (string-replace "\0" ":" (nth 1 testcase2)))
          (setq msg2 (compile--test-error-line testcase2)))
        (should (equal msg1 msg2))))
    (dolist (testcase compile-tests--grep-regexp-tricky-testcases)
      (compile--test-error-line testcase))
    (should (eq compilation-num-errors-found 8))))

(ert-deftest compile-test-functions ()
  "Test rules using functions instead of regexp group numbers."
  (let* ((file-fun (lambda () '("my-file")))
         (line-start-fun (lambda () 123))
         (line-end-fun (lambda () 134))
         (col-start-fun (lambda () 39))
         (col-end-fun (lambda () 24))
         (compilation-error-regexp-alist-alist
         `((my-rule
            ,(rx bol "My error message")
            ,file-fun
            (,line-start-fun . ,line-end-fun)
            (,col-start-fun . ,col-end-fun))))
         (compilation-error-regexp-alist '(my-rule)))
  (with-temp-buffer
    (font-lock-mode -1)
    (let ((compilation-num-errors-found 0)
          (compilation-num-warnings-found 0)
          (compilation-num-infos-found 0))
      (compile--test-error-line
       '(my-rule
         "My error message"
         1 (39 . 24) (123 . 134) "my-file" 2))
      (should (eq compilation-num-errors-found 1))
      (should (eq compilation-num-warnings-found 0))
      (should (eq compilation-num-infos-found 0))))))

;;; compile-tests.el ends here
