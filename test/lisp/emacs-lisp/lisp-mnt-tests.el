;;; lisp-mnt-tests.el --- Tests for lisp-mnt  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>

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
(require 'lisp-mnt)

(ert-deftest lm--tests-crack-address ()
  (should (equal (lm-crack-address
                  "Bob Weiner <rsw@gnu.org>, Mats Lidell <matsl@gnu.org>")
                 '(("Bob Weiner" . "rsw@gnu.org")
                   ("Mats Lidell" . "matsl@gnu.org")))))

(ert-deftest lm--tests-lm-website ()
  (with-temp-buffer
    (insert ";; URL: https://example.org/foo")
    (should (string= (lm-website) "https://example.org/foo")))
  (with-temp-buffer
    (insert  ";; X-URL: <https://example.org/foo>")
    (should (string= (lm-website) "https://example.org/foo"))))

(provide 'lisp-mnt-tests)
;;; lisp-mnt-tests.el ends here
