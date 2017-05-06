;;; assess-discover.el --- Test support functions -*- lexical-binding: t -*-

;;; Header:

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@russet.org.uk>
;; Maintainer: Phillip Lord <phillip.lord@russet.org.uk>

;; The contents of this file are subject to the GPL License, Version 3.0.

;; Copyright (C) 2015, 2016, Phillip Lord

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:


;; #+begin_src emacs-lisp
(defun assess-discover-tests (directory)
  "Discover tests in directory.

Tests must conform to one (and only one!) of several naming
schemes.

 - End with -test.el
 - End with -tests.el
 - Start with test-
 - Any .el file in a directory called test
 - Any .el file in a directory called tests

Each of these is tried until one matches. So, a top-level file
called \"blah-test.el\" will prevent discovery of files in a
tests directory."
  (or
   ;; files with
   (directory-files directory nil ".*-test.el$")
   (directory-files directory nil ".*-tests.el$")
   (directory-files directory nil "test-.*.el$")
   (let ((dir-test
          (concat directory "test/")))
     (when (file-exists-p dir-test)
       (mapcar
        (lambda (file)
          (concat dir-test file))
        (directory-files dir-test nil ".*.el"))))
   (let ((dir-tests
          (concat directory "tests/")))
     (when (file-exists-p dir-tests)
       (mapcar
        (lambda (file)
          (concat dir-tests file))
        (directory-files dir-tests nil ".*.el"))))))

(defun assess-discover--load-all-tests (directory)
  (mapc
   'load
   (assess-discover-tests directory)))

(defun assess-discover-load-tests ()
  (interactive)
  (assess-discover--load-all-tests default-directory))

;;;###autoload
(defun assess-discover-run-batch (&optional selector)
  (assess-discover--load-all-tests default-directory)
  (ert-run-tests-batch selector))

;;;###autoload
(defun assess-discover-run-and-exit-batch (&optional selector)
  (assess-discover--load-all-tests default-directory)
  (ert-run-tests-batch-and-exit selector))

(provide 'assess-discover)
;;; assess-discover.el ends here
;; #+end_src
