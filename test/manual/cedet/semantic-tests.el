;;; semantic-utest.el --- Miscellaneous Semantic tests.

;;; Copyright (C) 2003-2004, 2007-2017 Free Software Foundation, Inc.

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

;; Originally, there are many test functions scattered among the
;; Semantic source files.  This file consolidates them.

(require 'data-debug)

;;; From semantic-complete

(require 'semantic/complete)

(defun semantic-complete-test ()
  "Test completion mechanisms."
  (interactive)
  (message "%S"
   (semantic-format-tag-prototype
    (semantic-complete-read-tag-project "Symbol: "))))

;;; From semanticdb-ebrowse

(require 'semantic/db-ebrowse)

(defun semanticdb-ebrowse-run-tests ()
  "Run some tests of the semanticdb-ebrowse system.
All systems are different.  Ask questions along the way."
  (interactive)
  (let ((doload nil))
    (when (y-or-n-p "Create a system database to test with? ")
      (call-interactively 'semanticdb-create-ebrowse-database)
      (setq doload t))
    ;;  Should we load in caches
    (when (if doload
	      (y-or-n-p "New database created.  Reload system databases? ")
	    (y-or-n-p "Load in all system databases? "))
      (semanticdb-load-ebrowse-caches)))
  ;; Ok, databases were created.  Let's try some searching.
  (when (not (or (eq major-mode 'c-mode)
		 (eq major-mode 'c++-mode)))
    (error "Please make your default buffer be a C or C++ file, then
run the test again")))

(defun semanticdb-ebrowse-dump ()
  "Find the first loaded ebrowse table, and dump out the contents."
  (interactive)
  (let ((db semanticdb-database-list)
	(ab nil))
    (while db
      (when (semanticdb-project-database-ebrowse-p (car db))
	(setq ab (data-debug-new-buffer "*EBROWSE Database*"))
	(data-debug-insert-thing (car db) "*" "")
	(setq db nil)
	)
      (setq db (cdr db)))))

;;; From semanticdb-global:

(require 'semantic/db-global)

(defvar semanticdb-test-gnu-global-startfile "~/src/global-5.7.3/global/global.c"
  "File to use for testing.")

(defun semanticdb-test-gnu-global (searchfor &optional standardfile)
  "Test the GNU Global semanticdb.
Argument SEARCHFOR is the text to search for.
If optional arg STANDARDFILE is non-nil, use a standard file w/ global enabled."
  (interactive "sSearch For Tag: \nP")

  (require 'data-debug)
  (save-excursion
    (when standardfile
      (save-match-data
	(set-buffer (find-file-noselect semanticdb-test-gnu-global-startfile))))

    (condition-case err
	(semanticdb-enable-gnu-global-in-buffer)
      (error (if standardfile
		 (error err)
	       (save-match-data
		 (set-buffer (find-file-noselect semanticdb-test-gnu-global-startfile)))
	       (semanticdb-enable-gnu-global-in-buffer))))

    (let* ((db (semanticdb-project-database-global "global"))
	   (tab (semanticdb-file-table db (buffer-file-name)))
	   (result (semanticdb-deep-find-tags-for-completion-method tab searchfor))
	   )
      (data-debug-new-buffer "*SemanticDB Gnu Global Result*")
      (data-debug-insert-thing result "?" ""))))

;;; From semantic-format

(require 'semantic/format)

(defun semantic-test-all-format-tag-functions (&optional arg)
  "Test all outputs from `semantic-format-tag-functions'.
Output is generated from the function under `point'.
Optional argument ARG specifies not to use color."
  (interactive "P")
  (semantic-fetch-tags)
  (let* ((tag (semantic-current-tag))
	 (par (semantic-current-tag-parent))
	 (fns semantic-format-tag-functions))
    (with-output-to-temp-buffer "*format-tag*"
      (princ "Tag->format function tests:")
      (while fns
	(princ "\n")
	(princ (car fns))
	(princ ":\n ")
	(let ((s (funcall (car fns) tag par (not arg))))
	  (save-excursion
	    (set-buffer "*format-tag*")
	    (goto-char (point-max))
	    (insert s)))
	(setq fns (cdr fns))))
      ))

;;; From semantic-fw:

(require 'semantic/fw)

(defun semantic-test-data-cache ()
  "Test the data cache."
  (interactive)
  (let ((data '(a b c)))
    (save-excursion
      (set-buffer (get-buffer-create " *semantic-test-data-cache*"))
      (erase-buffer)
      (insert "The Moose is Loose")
      (goto-char (point-min))
      (semantic-cache-data-to-buffer (current-buffer) (point) (+ (point) 5)
				     data 'moose 'exit-cache-zone)
      (if (equal (semantic-get-cache-data 'moose) data)
	  (message "Successfully retrieved cached data.")
	(error "Failed to retrieve cached data")))))

(defun semantic-test-throw-on-input ()
  "Test that throw on input will work."
  (interactive)
  (semantic-throw-on-input 'done-die)
  (message "Exit Code: %s"
	   (semantic-exit-on-input 'testing
	     (let ((inhibit-quit nil)
		   (message-log-max nil))
	       (while t
		 (message "Looping ... press a key to test")
		 (semantic-throw-on-input 'test-inner-loop))
	       'exit)))
  (when (input-pending-p)
    (if (fboundp 'read-event)
	(read-event)
      (read-char))))

;;; From semantic-idle:

(require 'semantic/idle)

(defun semantic-idle-pnf-test ()
  "Test `semantic-idle-scheduler-work-parse-neighboring-files' and time it."
  (interactive)
  (let ((start (current-time))
	(junk (semantic-idle-scheduler-work-parse-neighboring-files)))
    (message "Work took %.2f seconds." (semantic-elapsed-time start nil))))

;;; From semantic-lex:

(require 'semantic/lex)

(defun semantic-lex-test-full-depth (arg)
  "Test the semantic lexer in the current buffer parsing through lists.
Usually the lexer parses.
If universal argument ARG, then try the whole buffer."
  (interactive "P")
  (let* ((start (current-time))
	 (result (semantic-lex
		  (if arg (point-min) (point))
		  (point-max)
		  100)))
    (message "Elapsed Time: %.2f seconds."
	     (semantic-elapsed-time start nil))
    (pop-to-buffer "*Lexer Output*")
    (require 'pp)
    (erase-buffer)
    (insert (pp-to-string result))
    (goto-char (point-min))))

(defun semantic-lex-test-region (beg end)
  "Test the semantic lexer in the current buffer.
Analyze the area between BEG and END."
  (interactive "r")
  (let ((result (semantic-lex beg end)))
    (pop-to-buffer "*Lexer Output*")
    (require 'pp)
    (erase-buffer)
    (insert (pp-to-string result))
    (goto-char (point-min))))

;;; From semantic-lex-spp:

(require 'semantic/lex-spp)

(defun semantic-lex-spp-write-test ()
  "Test the semantic tag writer against the current buffer."
  (interactive)
  (with-output-to-temp-buffer "*SPP Write Test*"
    (semantic-lex-spp-table-write-slot-value
     (semantic-lex-spp-save-table))))

(defun semantic-lex-spp-write-utest ()
  "Unit test using the test spp file to test the slot write fcn."
  (interactive)
  (let* ((sem (locate-library "semantic-lex-spp.el"))
	 (dir (file-name-directory sem)))
    (save-excursion
      (set-buffer (find-file-noselect
		   (expand-file-name "tests/testsppreplace.c"
				     dir)))
      (semantic-lex-spp-write-test))))

;;; From semantic-tag-write:

;;; TESTING.

(require 'semantic/tag-write)

(defun semantic-tag-write-test ()
  "Test the semantic tag writer against the tag under point."
  (interactive)
  (with-output-to-temp-buffer "*Tag Write Test*"
    (semantic-tag-write-one-tag (semantic-current-tag))))

(defun semantic-tag-write-list-test ()
  "Test the semantic tag writer against the tag under point."
  (interactive)
  (with-output-to-temp-buffer "*Tag Write Test*"
    (semantic-tag-write-tag-list (semantic-fetch-tags))))

;;; From semantic-symref-filter:

(require 'semantic/symref/filter)

(defun semantic-symref-test-count-hits-in-tag ()
  "Lookup in the current tag the symbol under point.
Then count all the other references to the same symbol within the
tag that contains point, and return that."
  (interactive)
  (let* ((ctxt (semantic-analyze-current-context))
	 (target (car (reverse (oref ctxt prefix))))
	 (tag (semantic-current-tag))
	 (start (current-time))
	 (Lcount 0))
    (when (semantic-tag-p target)
      (semantic-symref-hits-in-region
       target (lambda (start end prefix) (setq Lcount (1+ Lcount)))
       (semantic-tag-start tag)
       (semantic-tag-end tag))
      (when (interactive-p)
	(message "Found %d occurrences of %s in %.2f seconds"
		 Lcount (semantic-tag-name target)
		 (semantic-elapsed-time start nil)))
      Lcount)))

;;; From bovine-gcc:

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

(defun semantic-gcc-test-output-parser ()
  "Test the output parser against some collected strings."
  (interactive)
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
            (message "Test failed on %S\nV H P:\n%S %S %S" (car strs) v h p))
          (setq fail t))
        ))
    (dolist (S semantic-gcc-test-strings-fail)
      (let* ((fields (semantic-gcc-fields S))
             (v (cdr (assoc 'version fields)))
             (h (or (cdr (assoc '--host fields))
                    (cdr (assoc 'target fields))))
             (p (cdr (assoc '--prefix fields)))
             )
        (when (and v h p)
          (message "Negative test failed on %S" S)
          (setq fail t))
        ))
    (if (not fail) (message "Tests passed."))
    ))

(defun semantic-gcc-test-output-parser-this-machine ()
  "Test the output parser against the machine currently running Emacs."
  (interactive)
  (let ((semantic-gcc-test-strings (list (semantic-gcc-query "gcc" "-v"))))
    (semantic-gcc-test-output-parser))
  )
