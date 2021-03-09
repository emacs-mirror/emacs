;;; pcase-tests.el --- Test suite for pcase macro.  -*- lexical-binding:t -*-

;; Copyright (C) 2012-2021 Free Software Foundation, Inc.

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
(require 'cl-lib)

(ert-deftest pcase-tests-base ()
  "Test pcase code."
  (should (equal (pcase '(1 . 2) ((app car '2) 6) ((app car '1) 5)) 5)))

(ert-deftest pcase-tests-bugs ()
  (should (equal (pcase '(2 . 3)        ;bug#18554
                   (`(,hd . ,(and (pred atom) tl)) (list hd tl))
                   ((pred consp) nil))
                 '(2 3)))
  (should (equal (pcase '(2 . 3)
                   (`(,hd . ,(and (pred (not consp)) tl)) (list hd tl))
                   ((pred consp) nil))
                 '(2 3))))

(pcase-defmacro pcase-tests-plus (pat n)
  `(app (lambda (v) (- v ,n)) ,pat))

(ert-deftest pcase-tests-macro ()
  (should (equal (pcase 5 ((pcase-tests-plus x 3) x)) 2)))

(defun pcase-tests-grep (fname exp)
  (when (consp exp)
    (or (eq fname (car exp))
        (cl-some (lambda (exp) (pcase-tests-grep fname exp)) (cdr exp)))))

(ert-deftest pcase-tests-tests ()
  (should (pcase-tests-grep 'memq '(or (+ 2 3) (memq x y))))
  (should-not (pcase-tests-grep 'memq '(or (+ 2 3) (- x y)))))

(ert-deftest pcase-tests-member ()
  (should (pcase-tests-grep
           'memq (macroexpand-all '(pcase x ((or 'a 'b 'c) body)))))
  (should (pcase-tests-grep
           'memql (macroexpand-all '(pcase x ((or 1 2 3 'a) body)))))
  (should (pcase-tests-grep
           'member (macroexpand-all '(pcase x ((or "a" 2 3 'a) body)))))
  (should-not (pcase-tests-grep
               'memq (macroexpand-all '(pcase x ((or "a" 2 3) body)))))
  (should-not (pcase-tests-grep
               'memql (macroexpand-all '(pcase x ((or "a" 2 3) body)))))
  (let ((exp (macroexpand-all
                      '(pcase x
                         ("a" body1)
                         (2 body2)
                         ((or "a" 2 3) body)))))
    (should-not (pcase-tests-grep 'memq exp))
    (should-not (pcase-tests-grep 'member exp))))

(ert-deftest pcase-tests-vectors ()
  (should (equal (pcase [1 2] (`[,x] 1) (`[,x ,y] (+ x y))) 3)))

(ert-deftest pcase-tests-bug14773 ()
  (let ((f (lambda (x)
             (pcase 'dummy
               ((and (let var x) (guard var)) 'left)
               ((and (let var (not x)) (guard var)) 'right)))))
    (should (equal (funcall f t) 'left))
    (should (equal (funcall f nil) 'right))))

(ert-deftest pcase-tests-bug46786 ()
  (let ((self 'outer))
    (ignore self)
    (should (equal (cl-macrolet ((show-self () `(list 'self self)))
                     (pcase-let ((`(,self ,_self2) '(inner "2")))
                       (show-self)))
                   '(self inner)))))

(ert-deftest pcase-tests-or-vars ()
  (let ((f (lambda (v)
             (pcase v
               ((or (and 'b1 (let x1 4) (let x2 5))
                    (and 'b2 (let y1 8) (let y2 9)))
                (list x1 x2 y1 y2))))))
    (should (equal (funcall f 'b1) '(4 5 nil nil)))
    (should (equal (funcall f 'b2) '(nil nil 8 9)))))

;;; pcase-tests.el ends here.
