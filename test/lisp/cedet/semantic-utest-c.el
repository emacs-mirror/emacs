;;; semantic-utest-c.el --- C based parsing tests.

;; Copyright (C) 2008-2020 Free Software Foundation, Inc.

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
;;
;; Run some C based parsing tests.

(require 'ert)
(require 'semantic)

(defvar semantic-utest-c-comparisons
  '( ("testsppreplace.c" . "testsppreplaced.c")
     )
  "List of files to parse and compare against each other.")

(defvar cedet-utest-directory
  (let* ((C (file-name-directory (locate-library "cedet")))
         (D (expand-file-name "../../test/manual/cedet/" C)))
    D)
  "Location of test files for this test suite.")

(defvar semantic-utest-c-test-directory (expand-file-name "tests" cedet-utest-directory)
  "Location of test files.")

;;; Code:
;;;###autoload
(ert-deftest semantic-test-c-preprocessor-simulation ()
  "Run parsing test for C from the test directory."
  (interactive)
  (semantic-mode 1)
  (dolist (fp semantic-utest-c-comparisons)
    (let* ((semantic-lex-c-nested-namespace-ignore-second nil)
	   (tags-actual
	    (with-current-buffer
	        (find-file-noselect (expand-file-name (car fp) semantic-utest-c-test-directory))
	      (semantic-clear-toplevel-cache)
	      (semantic-fetch-tags)))
	   (tags-expected
	    (with-current-buffer (find-file-noselect (expand-file-name (cdr fp) semantic-utest-c-test-directory))
	      (semantic-clear-toplevel-cache)
	      (semantic-fetch-tags))))
      (when (or (not tags-expected) (not tags-actual))
        (message "Tried to find test files in: %s" semantic-utest-c-test-directory)
        (error "Failed:  Discovered no tags in test files or test file not found."))

      ;; Now that we have the tags, compare them for SPP accuracy.
      (dolist (tag tags-actual)
	(if (and (semantic-tag-of-class-p tag 'variable)
		 (semantic-tag-variable-constant-p tag))
	    nil				; skip the macros.

	  (if (semantic-tag-similar-with-subtags-p tag (car tags-expected))
	      (setq tags-expected (cdr tags-expected))
	    (with-mode-local c-mode
              (should nil) ;; this is a fail condition
	      (message "Error: Found: >> %s << Expected: >>  %s <<"
		       (semantic-format-tag-prototype tag nil t)
		       (semantic-format-tag-prototype (car tags-expected) nil t)
		       )))
	  ))
      )))

(require 'semantic/bovine/gcc)

;; Example output of "gcc -v"
(defvar semantic-gcc-test-strings
  '(;; My old box:
    "Reading specs from /usr/lib/gcc-lib/i386-redhat-linux/3.2.2/specs
Configured with: ../configure --prefix=/usr --mandir=/usr/share/man --infodir=/usr/share/info --enable-shared --enable-threads=posix --disable-checking --with-system-zlib --enable-__cxa_atexit --host=i386-redhat-linux
Thread model: posix
gcc version 3.2.2 20030222 (Red Hat Linux 3.2.2-5)"
    ;; Alex Ott:
    "Using built-in specs.
Target: i486-linux-gnu
Configured with: ../src/configure -v --with-pkgversion='Ubuntu 4.3.1-9ubuntu1' --with-bugurl=file:///usr/share/doc/gcc-4.3/README.Bugs --enable-languages=c,c++,fortran,objc,obj-c++ --prefix=/usr --enable-shared --with-system-zlib --libexecdir=/usr/lib --without-included-gettext --enable-threads=posix --enable-nls --with-gxx-include-dir=/usr/include/c++/4.3 --program-suffix=-4.3 --enable-clocale=gnu --enable-libstdcxx-debug --enable-objc-gc --enable-mpfr --enable-targets=all --enable-checking=release --build=i486-linux-gnu --host=i486-linux-gnu --target=i486-linux-gnu
Thread model: posix
gcc version 4.3.1 (Ubuntu 4.3.1-9ubuntu1)"
    ;; My debian box:
    "Using built-in specs.
Target: x86_64-unknown-linux-gnu
Configured with: ../../../sources/gcc/configure --prefix=/usr/local/glibc-2.3.6/x86_64/apps/gcc-4.2.3 --with-gmp=/usr/local/gcc/gmp --with-mpfr=/usr/local/gcc/mpfr --enable-languages=c,c++,fortran --with-as=/usr/local/glibc-2.3.6/x86_64/apps/gcc-4.2.3/bin/as --with-ld=/usr/local/glibc-2.3.6/x86_64/apps/gcc-4.2.3/bin/ld --disable-multilib
Thread model: posix
gcc version 4.2.3"
    ;; My mac:
    "Using built-in specs.
Target: i686-apple-darwin8
Configured with: /private/var/tmp/gcc/gcc-5341.obj~1/src/configure --disable-checking -enable-werror --prefix=/usr --mandir=/share/man --enable-languages=c,objc,c++,obj-c++ --program-transform-name=/^[cg][^.-]*$/s/$/-4.0/ --with-gxx-include-dir=/include/c++/4.0.0 --with-slibdir=/usr/lib --build=powerpc-apple-darwin8 --with-arch=pentium-m --with-tune=prescott --program-prefix= --host=i686-apple-darwin8 --target=i686-apple-darwin8
Thread model: posix
gcc version 4.0.1 (Apple Computer, Inc. build 5341)"
    ;; Ubuntu Intrepid
    "Using built-in specs.
Target: x86_64-linux-gnu
Configured with: ../src/configure -v --with-pkgversion='Ubuntu 4.3.2-1ubuntu12' --with-bugurl=file:///usr/share/doc/gcc-4.3/README.Bugs --enable-languages=c,c++,fortran,objc,obj-c++ --prefix=/usr --enable-shared --with-system-zlib --libexecdir=/usr/lib --without-included-gettext --enable-threads=posix --enable-nls --with-gxx-include-dir=/usr/include/c++/4.3 --program-suffix=-4.3 --enable-clocale=gnu --enable-libstdcxx-debug --enable-objc-gc --enable-mpfr --enable-checking=release --build=x86_64-linux-gnu --host=x86_64-linux-gnu --target=x86_64-linux-gnu
Thread model: posix
gcc version 4.3.2 (Ubuntu 4.3.2-1ubuntu12)"
    ;; Red Hat EL4
    "Reading specs from /usr/lib/gcc/x86_64-redhat-linux/3.4.6/specs
Configured with: ../configure --prefix=/usr --mandir=/usr/share/man --infodir=/usr/share/info --enable-shared --enable-threads=posix --disable-checking --with-system-zlib --enable-__cxa_atexit --disable-libunwind-exceptions --enable-java-awt=gtk --host=x86_64-redhat-linux
Thread model: posix
gcc version 3.4.6 20060404 (Red Hat 3.4.6-10)"
    ;; Red Hat EL5
    "Using built-in specs.
Target: x86_64-redhat-linux
Configured with: ../configure --prefix=/usr --mandir=/usr/share/man --infodir=/usr/share/info --enable-shared --enable-threads=posix --enable-checking=release --with-system-zlib --enable-__cxa_atexit --disable-libunwind-exceptions --enable-libgcj-multifile --enable-languages=c,c++,objc,obj-c++,java,fortran,ada --enable-java-awt=gtk --disable-dssi --enable-plugin --with-java-home=/usr/lib/jvm/java-1.4.2-gcj-1.4.2.0/jre --with-cpu=generic --host=x86_64-redhat-linux
Thread model: posix
gcc version 4.1.2 20080704 (Red Hat 4.1.2-44)"
    ;; David Engster's german gcc on ubuntu 4.3
    "Es werden eingebaute Spezifikationen verwendet.
Ziel: i486-linux-gnu
Konfiguriert mit: ../src/configure -v --with-pkgversion='Ubuntu 4.3.2-1ubuntu12' --with-bugurl=file:///usr/share/doc/gcc-4.3/README.Bugs --enable-languages=c,c++,fortran,objc,obj-c++ --prefix=/usr --enable-shared --with-system-zlib --libexecdir=/usr/lib --without-included-gettext --enable-threads=posix --enable-nls --with-gxx-include-dir=/usr/include/c++/4.3 --program-suffix=-4.3 --enable-clocale=gnu --enable-libstdcxx-debug --enable-objc-gc --enable-mpfr --enable-targets=all --enable-checking=release --build=i486-linux-gnu --host=i486-linux-gnu --target=i486-linux-gnu
Thread-Modell: posix
gcc-Version 4.3.2 (Ubuntu 4.3.2-1ubuntu12)"
    ;; Damien Deville bsd
    "Using built-in specs.
Target: i386-undermydesk-freebsd
Configured with: FreeBSD/i386 system compiler
Thread model: posix
gcc version 4.2.1 20070719  [FreeBSD]"
    )
  "A bunch of sample gcc -v outputs from different machines.")

(defvar semantic-gcc-test-strings-fail
  '(;; A really old solaris box I found
    "Reading specs from /usr/local/gcc-2.95.2/lib/gcc-lib/sparc-sun-solaris2.6/2.95.2/specs
gcc version 2.95.2 19991024 (release)"
    )
  "A bunch of sample gcc -v outputs that fail to provide the info we want.")

(ert-deftest semantic-test-gcc-output-parser ()
  "Test the output parser against some collected strings."
  (let ((fail nil))
    (dolist (S semantic-gcc-test-strings)
      (let* ((fields (semantic-gcc-fields S))
             (v (cdr (assoc 'version fields)))
             (h (or (cdr (assoc 'target fields))
                    (cdr (assoc '--target fields))
                    (cdr (assoc '--host fields))))
             (p (cdr (assoc '--prefix fields)))
             )
	;; No longer test for prefixes.
        (when (not (and v h))
          (let ((strs (split-string S "\n")))
            (message "Test failed on %S\nV H P:\n%S %S %S" (car strs) v h p)
            ))
        (should (and v h))
        ))
    (dolist (S semantic-gcc-test-strings-fail)
      (let* ((fields (semantic-gcc-fields S))
             (v (cdr (assoc 'version fields)))
             (h (or (cdr (assoc '--host fields))
                    (cdr (assoc 'target fields))))
             (p (cdr (assoc '--prefix fields)))
             )
        ;; negative test
        (should-not (and v h p))
        ))
    ))


(provide 'semantic-utest-c)

;;; semantic-utest-c.el ends here
