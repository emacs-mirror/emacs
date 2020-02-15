;;; pp-tests.el --- Test suite for pretty printer.  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2020 Free Software Foundation, Inc.

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

(require 'pp)

(ert-deftest pp-print-quote ()
  (should (string= (pp-to-string 'quote) "quote"))
  (should (string= (pp-to-string ''quote) "'quote"))
  (should (string= (pp-to-string '('a 'b)) "('a 'b)\n"))
  (should (string= (pp-to-string '(''quote 'quote)) "(''quote 'quote)\n"))
  (should (string= (pp-to-string '(quote)) "(quote)\n"))
  (should (string= (pp-to-string '(quote . quote)) "(quote . quote)\n"))
  (should (string= (pp-to-string '(quote a b)) "(quote a b)\n"))
  (should (string= (pp-to-string '(quotefoo)) "(quotefoo)\n"))
  (should (string= (pp-to-string '(a b)) "(a b)\n")))

;;; pp-tests.el ends here.
