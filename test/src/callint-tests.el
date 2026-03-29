;;; callint-tests.el --- unit tests for callint.c    -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2026 Free Software Foundation, Inc.

;; Author: Philipp Stephani <phst@google.com>

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

;; Unit tests for src/callint.c.

;;; Code:

(require 'ert)

(ert-deftest call-interactively/incomplete-multibyte-sequence ()
  "Check that Bug#30004 is fixed."
  (let* ((text-quoting-style 'grave)
         (data (should-error (call-interactively (lambda () (interactive "\xFF"))))))
    (should
     (equal
      (cdr data)
      '("Invalid control letter `\u00FF' (#o377, #x00ff) in interactive calling string")))))

(ert-deftest call-interactively/embedded-nulls ()
  "Check that Bug#30005 is fixed."
  (should (equal (let ((unread-command-events '(?a ?b)))
                   (call-interactively (lambda (a b)
                                         (interactive "ka\0a: \nkb: ")
                                         (list a b))))
                 '("a" "b"))))

(ert-deftest call-interactively-prune-command-history ()
  "Check that Bug#31211 is fixed."
  (let ((history-length 1)
        (command-history ()))
    (dotimes (_ (1+ history-length))
      (call-interactively #'ignore t))
    (should (= (length command-history) history-length))))

(defun callint-test-int-args (foo bar &optional zot)
  (declare (interactive-args
            (bar 10)
            (zot 11)))
  (interactive (list 1 1 1))
  (+ foo bar zot))

(ert-deftest test-interactive-args ()
  (let ((history-length 1)
        (command-history ()))
    (should (= (call-interactively 'callint-test-int-args t) 3))
    (should (equal command-history '((callint-test-int-args 1 10 11))))))

;;; callint-tests.el ends here
