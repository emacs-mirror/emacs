;;; shadow-tests.el --- Test suite for shadow.  -*- lexical-binding: t -*-

;; Copyright (C) 2018-2026 Free Software Foundation, Inc.

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
(require 'ert-x)
(require 'shadow)
(eval-when-compile (require 'cl-lib))

(ert-deftest shadow-case-insensitive ()
  "Test shadowing for case insensitive filenames."
  ;; Override `file-name-case-insensitive-p' so we test the same thing
  ;; regardless of what file system we're running on.
  (cl-letf (((symbol-function 'file-name-case-insensitive-p) (lambda (_f) t)))
    (should (equal (list (ert-resource-file "p1/foo")
                         (ert-resource-file "p2/FOO"))
                   (load-path-shadows-find
                    (list (ert-resource-file "p1/")
                          (ert-resource-file "p2/"))))))
  (cl-letf (((symbol-function 'file-name-case-insensitive-p) (lambda (_f) nil)))
    (should-not (load-path-shadows-find
                 (list (ert-resource-file "p1/")
                       (ert-resource-file "p2/"))))))

;;; shadow-tests.el ends here.
