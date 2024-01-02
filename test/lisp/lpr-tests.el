;;; lpr-tests.el --- Tests for lpr.el  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'lpr)

(ert-deftest lpr-test-printify-region ()
  (with-temp-buffer
    (insert "foo\^@-\^h\^k\^n-\^_\177bar")
    (printify-region (point-min) (point-max))
    (should (equal (buffer-string) "foo\\^@-\\^H\\^K\\^N-\\^_\\7fbar"))))

(ert-deftest lpr-test-lpr-eval-switch ()
  (should (equal (lpr-eval-switch "foo") "foo"))
  (should (equal (lpr-eval-switch (lambda () "foo")) "foo"))
  (let ((v "foo"))
    (should (equal (lpr-eval-switch v) "foo")))
  (should (equal (lpr-eval-switch (list #'identity "foo")) "foo"))
  (should (equal (lpr-eval-switch 1) nil)))

;;; lpr-tests.el ends here
