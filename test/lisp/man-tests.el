;;; man-tests.el --- Test suite for man.

;; Copyright (C) 2013-2017 Free Software Foundation, Inc.

;; Author: Wolfgang Jenkner <wjenkner@inode.at>
;; Keywords: help, internal, unix

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

;;; Code:

(require 'ert)
(require 'man)

(defconst man-tests-parse-man-k-tests
  '(;; GNU/Linux: man-db-2.6.1
    ("\
sin (3)              - sine function
sinf (3)             - sine function
sinl (3)             - sine function"
     . (#("sin(3)" 0 6 (help-echo "sine function")) #("sinf(3)" 0 7 (help-echo "sine function")) #("sinl(3)" 0 7 (help-echo "sine function"))))
    ;; GNU/Linux: man-1.6g
    ("\
sin                  (3)  - sine function
sinf [sin]           (3)  - sine function
sinl [sin]           (3)  - sine function"
     . (#("sin(3)" 0 6 (help-echo "sine function")) #("sinf(3)" 0 7 (help-echo "sine function")) #("sinl(3)" 0 7 (help-echo "sine function"))))
    ;; FreeBSD 9
    ("\
sin(3), sinf(3), sinl(3) - sine functions"
     . (#("sin(3)" 0 6 (help-echo "sine functions")) #("sinf(3)" 0 7 (help-echo "sine functions")) #("sinl(3)" 0 7 (help-echo "sine functions"))))
    ;; SunOS, Solaris
    ;; http://docs.oracle.com/cd/E19455-01/805-6331/usradm-7/index.html
    ;; SunOS 4
    ("\
tset, reset (1)    - establish or restore terminal characteristics"
     . (#("tset(1)" 0 7 (help-echo "establish or restore terminal characteristics")) #("reset(1)" 0 8 (help-echo "establish or restore terminal characteristics"))))
    ;; SunOS 5.7, Solaris
    ("\
reset  tset (1b)   - establish or restore terminal characteristics
tset   tset (1b)   - establish or restore terminal characteristics"
     . (#("reset(1b)" 0 8 (help-echo "establish or restore terminal characteristics")) #("tset(1b)" 0 7 (help-echo "establish or restore terminal characteristics"))))
    ;; Minix 3
    ;; http://www.minix3.org/manpages/html5/whatis.html
    ("\
cawf, nroff (1) - C version of the nroff-like, Amazingly Workable (text) Formatter
whatis (5) - database of online manual pages"
     . (#("cawf(1)" 0 7 (help-echo "C version of the nroff-like, Amazingly Workable (text) Formatter")) #("nroff(1)" 0 8 (help-echo "C version of the nroff-like, Amazingly Workable (text) Formatter")) #("whatis(5)" 0 9 (help-echo "database of online manual pages"))))
    ;; HP-UX
    ;; http://docstore.mik.ua/manuals/hp-ux/en/B2355-60130/man.1.html
    ;; Assuming that the line break in the zgrep description was
    ;; introduced by the man page formatting.
    ("\
grep, egrep, fgrep (1) - search a file for a pattern
zgrep(1) - search possibly compressed files for a regular expression"
     . (#("grep(1)" 0 7 (help-echo "search a file for a pattern")) #("egrep(1)" 0 8 (help-echo "search a file for a pattern")) #("fgrep(1)" 0 8 (help-echo "search a file for a pattern")) #("zgrep(1)" 0 8 (help-echo "search possibly compressed files for a regular expression"))))
    ;; AIX
    ;; http://pic.dhe.ibm.com/infocenter/aix/v7r1/topic/com.ibm.aix.cmds/doc/aixcmds6/whatis.htm
    ("\
ls(1)  -Displays the contents of a directory."
    . (#("ls(1)" 0 5 (help-echo "Displays the contents of a directory."))))
    ;; https://www.ibm.com/developerworks/mydeveloperworks/blogs/cgaix/entry/catman_0703_102_usr_lbin_mkwhatis_the_error_number_is_1?lang=en
    ("\
loopmount(1)    - Associate an image file to a loopback device."
     . (#("loopmount(1)" 0 12 (help-echo "Associate an image file to a loopback device."))))
    )
  "List of tests for `Man-parse-man-k'.
Each element is a cons cell whose car is a string containing
man -k output.  That should result in the table which is stored
in the cdr of the element.")

(defun man-tests-name-equal-p (name description string)
  (and (equal name string)
       (not (next-single-property-change 0 'help-echo string))
       (equal (get-text-property 0 'help-echo string) description)))

(defun man-tests-parse-man-k-test-case (test)
  (let ((temp-buffer (get-buffer-create " *test-man*"))
	(man-k-output (car test)))
    (unwind-protect
	(save-window-excursion
	  (with-current-buffer temp-buffer
	    (erase-buffer)
	    (insert man-k-output)
	    (let ((result (Man-parse-man-k))
		  (checklist (cdr test)))
	      (while (and checklist result
			  (man-tests-name-equal-p
			   (car checklist)
			   (get-text-property 0 'help-echo
					      (car checklist))
			   (pop result)))
		(pop checklist))
	      (and (null checklist) (null result)))))
      (and (buffer-name temp-buffer)
	   (kill-buffer temp-buffer)))))

(ert-deftest man-tests ()
  "Test man."
  (dolist (test man-tests-parse-man-k-tests)
    (should (man-tests-parse-man-k-test-case test))))

(provide 'man-tests)

;;; man-tests.el ends here
