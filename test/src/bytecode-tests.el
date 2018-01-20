;;; bytecode-tests.el --- unit tests for src/bytecode.c      -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

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

;; Unit tests for src/bytecode.c.

;;; Code:

(require 'ert)

(defun bctest-throw-something ()
  (throw 'something 23))

(defun bctest-signal ()
  (signal 'error 23))

(ert-deftest bctest-unwind-protect-signal ()
  (let ((val nil))
    (should-error (unwind-protect
                      (bctest-signal)
                    (setq val t)))
    (should val)))

(ert-deftest bctest-unwind-protect-throw ()
  (let ((val nil))
    (should (eq (catch 'something
                  (unwind-protect
                      (bctest-throw-something)
                    (setq val t))
                  'fail)
                23))
    (should val)))

(ert-deftest bctest-unwind-protect-fallthrough ()
  (let ((val nil))
    (unwind-protect
        (setq val 'x)
      (setq val t))
    (should val)))

;;; bytecode-tests.el ends here
