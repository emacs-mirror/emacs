;;; thunk-tests.el --- Tests for thunk.el -*- lexical-binding: t -*-

;; Copyright (C) 2015-2020 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Maintainer: emacs-devel@gnu.org

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

;; Tests for thunk.el

;;; Code:

(require 'ert)
(require 'thunk)

(ert-deftest thunk-should-be-lazy ()
  (let (x)
    (thunk-delay (setq x t))
    (should (null x))))

(ert-deftest thunk-can-be-evaluated ()
  (let* (x
         (thunk (thunk-delay (setq x t))))
    (should-not (thunk-evaluated-p thunk))
    (should (null x))
    (thunk-force thunk)
    (should (thunk-evaluated-p thunk))
    (should x)))

(ert-deftest thunk-evaluation-is-cached ()
  (let* ((x 0)
        (thunk (thunk-delay (setq x (1+ x)))))
    (thunk-force thunk)
    (should (= x 1))
    (thunk-force thunk)
    (should (= x 1))))



;; thunk-let tests

(ert-deftest thunk-let-basic-test ()
  "Test whether bindings are established."
  (should (equal (thunk-let ((x 1) (y 2)) (+ x y)) 3)))

(ert-deftest thunk-let*-basic-test ()
  "Test whether bindings are established."
  (should (equal (thunk-let* ((x 1) (y (+ 1 x))) (+ x y)) 3)))

(ert-deftest thunk-let-bound-vars-cant-be-set-test ()
  "Test whether setting a `thunk-let' bound variable fails."
  (should-error
   (eval '(thunk-let ((x 1)) (let ((y 7)) (setq x (+ x y)) (* 10 x))) t)))

(ert-deftest thunk-let-laziness-test ()
  "Test laziness of `thunk-let'."
  (should
   (equal (let ((x-evalled nil)
                (y-evalled nil))
            (thunk-let ((x (progn (setq x-evalled t) (+ 1 2)))
                        (y (progn (setq y-evalled t) (+ 3 4))))
              (let ((evalled-y y))
                (list x-evalled y-evalled evalled-y))))
          (list nil t 7))))

(ert-deftest thunk-let*-laziness-test ()
  "Test laziness of `thunk-let*'."
  (should
   (equal (let ((x-evalled nil)
                (y-evalled nil)
                (z-evalled nil)
                (a-evalled nil))
            (thunk-let* ((x (progn (setq x-evalled t) (+ 1 1)))
                         (y (progn (setq y-evalled t) (+ x 1)))
                         (z (progn (setq z-evalled t) (+ y 1)))
                         (a (progn (setq a-evalled t) (+ z 1))))
              (let ((evalled-z z))
                (list x-evalled y-evalled z-evalled a-evalled evalled-z))))
          (list t t t nil 4))))

(ert-deftest thunk-let-bad-binding-test ()
  "Test whether a bad binding causes an error when expanding."
  (should-error (macroexpand '(thunk-let ((x 1 1)) x)))
  (should-error (macroexpand '(thunk-let (27) x)))
  (should-error (macroexpand '(thunk-let x x))))


(provide 'thunk-tests)
;;; thunk-tests.el ends here
