;;; obarray-tests.el --- Tests for obarray -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Free Software Foundation, Inc.

;; Author: Przemysław Wojnowski <esperanto@cumego.com>

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

(require 'obarray)
(require 'ert)

(ert-deftest obarrayp-test ()
  "Should assert that given object is an obarray."
  (should-not (obarrayp 42))
  (should-not (obarrayp "aoeu"))
  (should-not (obarrayp '()))
  (should-not (obarrayp []))
  (should-not (obarrayp ["a" "b" "c"]))
  (should-not (obarrayp [1 2 3]))
  (should-not (obarrayp (make-vector 7 0)))
  (should-not (obarrayp (vector (obarray-make))))
  (should (obarrayp (obarray-make)))
  (should (obarrayp (obarray-make 7))))

(ert-deftest obarray-make-with-size-test ()
  ;; FIXME: Actually, `wrong-type-argument' is not the right error to signal,
  ;; so we shouldn't enforce this misbehavior in tests!
  (should-error (obarray-make -1) :type 'wrong-type-argument)
  (should-error (obarray-make 'a) :type 'wrong-type-argument))

(ert-deftest obarray-get-test ()
  (let ((table (obarray-make 3)))
    (should-not (obarray-get table "aoeu"))
    (intern "aoeu" table)
    (should (string= "aoeu" (obarray-get table "aoeu")))))

(ert-deftest obarray-put-test ()
  (let ((table (obarray-make 3)))
    (should-not (obarray-get table "aoeu"))
    (should (string= "aoeu" (obarray-put table "aoeu")))
    (should (string= "aoeu" (obarray-get table "aoeu")))))

(ert-deftest obarray-remove-test ()
  (let ((table (obarray-make 3)))
    (should-not (obarray-get table "aoeu"))
    (should-not (obarray-remove table "aoeu"))
    (should (string= "aoeu" (obarray-put table "aoeu")))
    (should (string= "aoeu" (obarray-get table "aoeu")))
    (should (obarray-remove table "aoeu"))
    (should-not (obarray-get table "aoeu"))))

(ert-deftest obarray-map-test ()
  "Should execute function on all elements of obarray."
  (let* ((table (obarray-make 3))
         (syms '())
         (collect-names (lambda (sym) (push (symbol-name sym) syms))))
    (obarray-map collect-names table)
    (should (null syms))
    (obarray-put table "a")
    (obarray-put table "b")
    (obarray-put table "c")
    (obarray-map collect-names table)
    (should (equal (sort syms #'string<) '("a" "b" "c")))))

(ert-deftest obarray-clear ()
  (let ((o (obarray-make)))
    (intern "a" o)
    (intern "b" o)
    (intern "c" o)
    (obarray-clear o)
    (let ((n 0))
      (mapatoms (lambda (_) (setq n (1+ n))) o)
      (should (equal n 0)))))

(provide 'obarray-tests)
;;; obarray-tests.el ends here
