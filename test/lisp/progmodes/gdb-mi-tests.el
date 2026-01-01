;;; gdb-mi-tests.el --- tests for gdb-mi.el    -*- lexical-binding: t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

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
(require 'gdb-mi)

(ert-deftest gdb-mi-parse-value ()
  ;; Test the GDB/MI result/value parser.
  (should (equal
           (gdb-mi--from-string
            "alpha=\"ab\\ncd\",beta=[\"x\",{gamma=\"y\",delta=[]}]")
           '((alpha . "ab\ncd")
             (beta . ("x" ((gamma . "y") (delta . ())))))))
  (should (equal
           (gdb-mi--from-string
            "alpha=\"ab\\ncd\",beta=[\"x\",{gamma=\"y\",delta=[]}]"
            'gamma)
           '((alpha . "ab\ncd")
             (beta . ("x" ("y" (delta . ())))))))

  (let ((gdb-mi-decode-strings nil))
    (let ((ref `((alpha . ,(string-to-multibyte "a\303\245b")))))
      (should (equal (gdb-mi--from-string "alpha=\"a\\303\\245b\"")
                     ref))))
  (let ((gdb-mi-decode-strings 'utf-8))
    (should (equal (gdb-mi--from-string "alpha=\"a\\303\\245b\"")
                   '((alpha . "aåb")))))
  )

(provide 'gdb-mi-tests)

;;; gdb-mi-tests.el ends here
