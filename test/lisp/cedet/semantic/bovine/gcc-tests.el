;;; gcc-tests.el --- Tests for semantic/bovine/gcc.el  -*- lexical-binding:t -*-

;; Copyright (C) 2003-2026 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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

;; Moved here from test/manual/cedet/semantic-tests.el

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'semantic/bovine/gcc)

;;; From bovine-gcc:

(defmacro semantic-gcc-test (str)
  `(let ((fields (semantic-gcc-fields ,str)))
     (let-alist fields
       (message "%S" fields)
       ;; No longer test for prefixes.
       ;; (should .--prefix)
       (should .version)
       (should (or .target
                   .--target
                   .--host)))))

;; A bunch of sample gcc -v outputs from different machines.

(ert-deftest semantic-gcc-test/1 ()
  ;; My old box:
  (semantic-gcc-test "Reading specs from /usr/lib/gcc-lib/i386-redhat-linux/3.2.2/specs
Configured with: ../configure --prefix=/usr --mandir=/usr/share/man --infodir=/usr/share/info --enable-shared --enable-threads=posix --disable-checking --with-system-zlib --enable-__cxa_atexit --host=i386-redhat-linux
Thread model: posix
gcc version 3.2.2 20030222 (Red Hat Linux 3.2.2-5)"))

(ert-deftest semantic-gcc-test/2 ()
  ;; Alex Ott:
  (semantic-gcc-test "Using built-in specs.
Target: i486-linux-gnu
Configured with: ../src/configure -v --with-pkgversion='Ubuntu 4.3.1-9ubuntu1' --with-bugurl=file:///usr/share/doc/gcc-4.3/README.Bugs --enable-languages=c,c++,fortran,objc,obj-c++ --prefix=/usr --enable-shared --with-system-zlib --libexecdir=/usr/lib --without-included-gettext --enable-threads=posix --enable-nls --with-gxx-include-dir=/usr/include/c++/4.3 --program-suffix=-4.3 --enable-clocale=gnu --enable-libstdcxx-debug --enable-objc-gc --enable-mpfr --enable-targets=all --enable-checking=release --build=i486-linux-gnu --host=i486-linux-gnu --target=i486-linux-gnu
Thread model: posix
gcc version 4.3.1 (Ubuntu 4.3.1-9ubuntu1)"))

(ert-deftest semantic-gcc-test/3 ()
  ;; My Debian box:
  (semantic-gcc-test "Using built-in specs.
Target: x86_64-unknown-linux-gnu
Configured with: ../../../sources/gcc/configure --prefix=/usr/local/glibc-2.3.6/x86_64/apps/gcc-4.2.3 --with-gmp=/usr/local/gcc/gmp --with-mpfr=/usr/local/gcc/mpfr --enable-languages=c,c++,fortran --with-as=/usr/local/glibc-2.3.6/x86_64/apps/gcc-4.2.3/bin/as --with-ld=/usr/local/glibc-2.3.6/x86_64/apps/gcc-4.2.3/bin/ld --disable-multilib
Thread model: posix
gcc version 4.2.3"))

(ert-deftest semantic-gcc-test/4 ()
  ;; My mac:
  (semantic-gcc-test "Using built-in specs.
Target: i686-apple-darwin8
Configured with: /private/var/tmp/gcc/gcc-5341.obj~1/src/configure --disable-checking -enable-werror --prefix=/usr --mandir=/share/man --enable-languages=c,objc,c++,obj-c++ --program-transform-name=/^[cg][^.-]*$/s/$/-4.0/ --with-gxx-include-dir=/include/c++/4.0.0 --with-slibdir=/usr/lib --build=powerpc-apple-darwin8 --with-arch=pentium-m --with-tune=prescott --program-prefix= --host=i686-apple-darwin8 --target=i686-apple-darwin8
Thread model: posix
gcc version 4.0.1 (Apple Computer, Inc. build 5341)"))

(ert-deftest semantic-gcc-test/5 ()
  ;; Ubuntu Intrepid
  (semantic-gcc-test "Using built-in specs.
Target: x86_64-linux-gnu
Configured with: ../src/configure -v --with-pkgversion='Ubuntu 4.3.2-1ubuntu12' --with-bugurl=file:///usr/share/doc/gcc-4.3/README.Bugs --enable-languages=c,c++,fortran,objc,obj-c++ --prefix=/usr --enable-shared --with-system-zlib --libexecdir=/usr/lib --without-included-gettext --enable-threads=posix --enable-nls --with-gxx-include-dir=/usr/include/c++/4.3 --program-suffix=-4.3 --enable-clocale=gnu --enable-libstdcxx-debug --enable-objc-gc --enable-mpfr --enable-checking=release --build=x86_64-linux-gnu --host=x86_64-linux-gnu --target=x86_64-linux-gnu
Thread model: posix
gcc version 4.3.2 (Ubuntu 4.3.2-1ubuntu12)"))

(ert-deftest semantic-gcc-test/6 ()
  ;; Red Hat EL4
  (semantic-gcc-test "Reading specs from /usr/lib/gcc/x86_64-redhat-linux/3.4.6/specs
Configured with: ../configure --prefix=/usr --mandir=/usr/share/man --infodir=/usr/share/info --enable-shared --enable-threads=posix --disable-checking --with-system-zlib --enable-__cxa_atexit --disable-libunwind-exceptions --enable-java-awt=gtk --host=x86_64-redhat-linux
Thread model: posix
gcc version 3.4.6 20060404 (Red Hat 3.4.6-10)"))

(ert-deftest semantic-gcc-test/7 ()
  ;; Red Hat EL5
  (semantic-gcc-test "Using built-in specs.
Target: x86_64-redhat-linux
Configured with: ../configure --prefix=/usr --mandir=/usr/share/man --infodir=/usr/share/info --enable-shared --enable-threads=posix --enable-checking=release --with-system-zlib --enable-__cxa_atexit --disable-libunwind-exceptions --enable-libgcj-multifile --enable-languages=c,c++,objc,obj-c++,java,fortran,ada --enable-java-awt=gtk --disable-dssi --enable-plugin --with-java-home=/usr/lib/jvm/java-1.4.2-gcj-1.4.2.0/jre --with-cpu=generic --host=x86_64-redhat-linux
Thread model: posix
gcc version 4.1.2 20080704 (Red Hat 4.1.2-44)"))

(ert-deftest semantic-gcc-test/8 ()
  ;; David Engster's german gcc on ubuntu 4.3
  (semantic-gcc-test "Es werden eingebaute Spezifikationen verwendet.
Ziel: i486-linux-gnu
Konfiguriert mit: ../src/configure -v --with-pkgversion='Ubuntu 4.3.2-1ubuntu12' --with-bugurl=file:///usr/share/doc/gcc-4.3/README.Bugs --enable-languages=c,c++,fortran,objc,obj-c++ --prefix=/usr --enable-shared --with-system-zlib --libexecdir=/usr/lib --without-included-gettext --enable-threads=posix --enable-nls --with-gxx-include-dir=/usr/include/c++/4.3 --program-suffix=-4.3 --enable-clocale=gnu --enable-libstdcxx-debug --enable-objc-gc --enable-mpfr --enable-targets=all --enable-checking=release --build=i486-linux-gnu --host=i486-linux-gnu --target=i486-linux-gnu
Thread-Modell: posix
gcc-Version 4.3.2 (Ubuntu 4.3.2-1ubuntu12)"))

(ert-deftest semantic-gcc-test/9 ()
  ;; Damien Deville bsd
  (semantic-gcc-test "Using built-in specs.
Target: i386-undermydesk-freebsd
Configured with: FreeBSD/i386 system compiler
Thread model: posix
gcc version 4.2.1 20070719  [FreeBSD]"))

(defvar semantic-gcc-test-strings-fail
  '(;; A really old solaris box I found
    "Reading specs from /usr/local/gcc-2.95.2/lib/gcc-lib/sparc-sun-solaris2.6/2.95.2/specs
gcc version 2.95.2 19991024 (release)"
    )
  "A bunch of sample gcc -v outputs that fail to provide the info we want.")

(ert-deftest semantic-gcc-test-output-parser/fail ()
  "Test the output parser against some collected strings."
  (dolist (S semantic-gcc-test-strings-fail)
    (let* ((fields (semantic-gcc-fields S))
           (v (cdr (assoc 'version fields)))
           (h (or (cdr (assoc '--host fields))
                  (cdr (assoc 'target fields))))
           (p (cdr (assoc '--prefix fields)))
           )
      (when (and v h p)
        (error "Negative test failed on %S" S)))))

(ert-deftest semantic-gcc-test-output-parser/this-machine ()
  "Test the output parser against the machine currently running Emacs."
  (skip-unless (and (executable-find "gcc")
                    (not (ert-gcc-is-clang-p))))
  (semantic-gcc-test (semantic-gcc-query "gcc" "-v")))

;;; gcc-tests.el ends here
