;;; rfc2368-tests.el --- Tests for rfc2368.el  -*- lexical-binding:t -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

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
(require 'rfc2368)

(ert-deftest rfc2368-unhexify-string ()
  (should (equal (rfc2368-unhexify-string "hello%20there") "hello there")))

(ert-deftest rfc2368-parse-mailto-url ()
  (should (equal (rfc2368-parse-mailto-url "mailto:foo@example.org?subject=Foo&bar=baz")
                 '(("To" . "foo@example.org") ("Subject" . "Foo") ("Bar" . "baz"))))
  (should (equal (rfc2368-parse-mailto-url "mailto:foo@bar.com?to=bar@example.org")
                 '(("To" . "foo@bar.com, bar@example.org"))))
  (should (equal (rfc2368-parse-mailto-url "mailto:foo@bar.com?subject=bar%20baz")
                 '(("To" . "foo@bar.com") ("Subject" . "bar baz")))))

(provide 'rfc2368-tests)
;;; rfc2368-tests.el ends here
