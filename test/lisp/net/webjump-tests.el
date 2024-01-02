;;; webjump-tests.el --- Tests for webjump.el        -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

;; Author: Simen Heggestøyl <simenheg@gmail.com>
;; Keywords:

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

;;

;;; Code:

(require 'ert)
(require 'webjump)

(ert-deftest webjump-tests-builtin ()
  (should (equal (webjump-builtin '[name] "gnu.org") "gnu.org")))

(ert-deftest webjump-tests-builtin-check-args ()
  (should (webjump-builtin-check-args [1 2 3] "Foo" 2))
  (should-error (webjump-builtin-check-args [1 2 3] "Foo" 3)))

(ert-deftest webjump-tests-mirror-default ()
  (should (equal (webjump-mirror-default
                  '("https://ftp.gnu.org/pub/gnu/"
                    "https://ftpmirror.gnu.org"))
                 "https://ftp.gnu.org/pub/gnu/")))

(ert-deftest webjump-tests-null-or-blank-string-p ()
  (should (webjump-null-or-blank-string-p nil))
  (should (webjump-null-or-blank-string-p ""))
  (should (webjump-null-or-blank-string-p " 	 "))
  (should-not (webjump-null-or-blank-string-p " .	")))

(ert-deftest webjump-tests-url-encode ()
  (should (equal (webjump-url-encode "") ""))
  (should (equal (webjump-url-encode "a b c") "a+b+c"))
  (should (equal (webjump-url-encode "foo?") "foo%3F"))
  (should (equal (webjump-url-encode "/foo\\") "/foo%5C"))
  (should (equal (webjump-url-encode "f&o") "f%26o")))

(ert-deftest webjump-tests-url-fix ()
  (should (equal (webjump-url-fix nil) ""))
  (should (equal (webjump-url-fix "/tmp/") "file:///tmp/"))
  (should (equal (webjump-url-fix "gnu.org") "http://gnu.org/"))
  (should (equal (webjump-url-fix "ftp.x.org") "ftp://ftp.x.org/"))
  (should (equal (webjump-url-fix "https://gnu.org")
                 "https://gnu.org/")))

(ert-deftest webjump-tests-url-fix-trailing-slash ()
  (should (equal (webjump-url-fix-trailing-slash "https://gnu.org")
                 "https://gnu.org/"))
  (should (equal (webjump-url-fix-trailing-slash "https://gnu.org/")
                 "https://gnu.org/")))

(provide 'webjump-tests)
;;; webjump-tests.el ends here
