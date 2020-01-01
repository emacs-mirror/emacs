;;; callint-tests.el --- unit tests for callint.c    -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

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
  (let ((data (should-error (call-interactively (lambda () (interactive "\xFF"))))))
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

;;; callint-tests.el ends here
