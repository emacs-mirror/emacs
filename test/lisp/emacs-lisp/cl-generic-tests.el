;;; cl-generic-tests.el --- Tests for cl-generic.el functionality  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2018 Free Software Foundation, Inc.

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

;;; Commentary:

;;; Code:

(eval-when-compile (require 'ert)) ;Don't indirectly require cl-lib at run-time.
(require 'cl-generic)

(fmakunbound 'cl--generic-1)
(cl-defgeneric cl--generic-1 (x y))
(cl-defgeneric (setf cl--generic-1) (v y z) "My generic doc.")

(ert-deftest cl-generic-test-00 ()
  (fmakunbound 'cl--generic-1)
  (cl-defgeneric cl--generic-1 (x y))
  (cl-defmethod cl--generic-1 ((x t) y) (cons x y))
  (should (equal (cl--generic-1 'a 'b) '(a . b))))

(ert-deftest cl-generic-test-01-eql ()
  (fmakunbound 'cl--generic-1)
  (cl-defgeneric cl--generic-1 (x y))
  (cl-defmethod cl--generic-1 ((x t) y) (cons x y))
  (cl-defmethod cl--generic-1 ((_x (eql 4)) _y)
    (cons "quatre" (cl-call-next-method)))
  (cl-defmethod cl--generic-1 ((_x (eql 5)) _y)
    (cons "cinq" (cl-call-next-method)))
  (cl-defmethod cl--generic-1 ((_x (eql 6)) y)
    (cons "six" (cl-call-next-method 'a y)))
  (should (equal (cl--generic-1 'a nil) '(a)))
  (should (equal (cl--generic-1 4 nil) '("quatre" 4)))
  (should (equal (cl--generic-1 5 nil) '("cinq" 5)))
  (should (equal (cl--generic-1 6 nil) '("six" a))))

(cl-defstruct cl-generic-struct-parent a b)
(cl-defstruct (cl-generic-struct-child1 (:include cl-generic-struct-parent)) c)
(cl-defstruct (cl-generic-struct-child11 (:include cl-generic-struct-child1)) d)
(cl-defstruct (cl-generic-struct-child2 (:include cl-generic-struct-parent)) e)

(ert-deftest cl-generic-test-02-struct ()
  (fmakunbound 'cl--generic-1)
  (cl-defgeneric cl--generic-1 (x y) "My doc.")
  (cl-defmethod cl--generic-1 ((x t) y) "Doc 1." (cons x y))
  (cl-defmethod cl--generic-1 ((_x cl-generic-struct-parent) y)
    "Doc 2." (cons "parent" (cl-call-next-method 'a y)))
  (cl-defmethod cl--generic-1 ((_x cl-generic-struct-child1) _y)
    (cons "child1" (cl-call-next-method)))
  (cl-defmethod cl--generic-1 :around ((_x t) _y)
    (cons "around" (cl-call-next-method)))
  (cl-defmethod cl--generic-1 :around ((_x cl-generic-struct-child11) _y)
    (cons "child11" (cl-call-next-method)))
  (cl-defmethod cl--generic-1 ((_x cl-generic-struct-child2) _y)
    (cons "child2" (cl-call-next-method)))
  (should (equal (cl--generic-1 (make-cl-generic-struct-child1) nil)
                 '("around" "child1" "parent" a)))
  (should (equal (cl--generic-1 (make-cl-generic-struct-child2) nil)
                 '("around""child2" "parent" a)))
  (should (equal (cl--generic-1 (make-cl-generic-struct-child11) nil)
                 '("child11" "around""child1" "parent" a))))

;; I don't know how to put this inside an `ert-test'.  This tests that `setf'
;; can be used directly inside the body of the setf method.
(cl-defmethod (setf cl--generic-2) (v (y integer) z)
  (setf (cl--generic-2 (nth y z) z) v))

(ert-deftest cl-generic-test-03-setf ()
  (cl-defmethod (setf cl--generic-1) (v (y t) z) (list v y z))
  (cl-defmethod (setf cl--generic-1) (v (_y (eql 4)) z) (list v "four" z))
  (should (equal (setf (cl--generic-1 'a 'b) 'v) '(v a b)))
  (should (equal (setf (cl--generic-1 4 'b) 'v) '(v "four" b)))
  (let ((x ()))
    (should (equal (setf (cl--generic-1 (progn (push 1 x) 'a)
                                        (progn (push 2 x) 'b))
                         (progn (push 3 x) 'v))
                   '(v a b)))
    (should (equal x '(3 2 1)))))

(ert-deftest cl-generic-test-04-overlapping-tagcodes ()
  (fmakunbound 'cl--generic-1)
  (cl-defgeneric cl--generic-1 (x y) "My doc.")
  (cl-defmethod cl--generic-1 ((y t) z) (list y z))
  (cl-defmethod cl--generic-1 ((_y (eql 4)) _z)
                (cons "four" (cl-call-next-method)))
  (cl-defmethod cl--generic-1 ((_y integer) _z)
                (cons "integer" (cl-call-next-method)))
  (cl-defmethod cl--generic-1 ((_y number) _z)
                (cons "number" (cl-call-next-method)))
  (should (equal (cl--generic-1 'a 'b) '(a b)))
  (should (equal (cl--generic-1 1 'b) '("integer" "number" 1 b)))
  (should (equal (cl--generic-1 4 'b) '("four" "integer" "number" 4 b))))

(ert-deftest cl-generic-test-05-alias ()
  (fmakunbound 'cl--generic-1)
  (cl-defgeneric cl--generic-1 (x y) "My doc.")
  (defalias 'cl--generic-2 #'cl--generic-1)
  (cl-defmethod cl--generic-1 ((y t) z) (list y z))
  (cl-defmethod cl--generic-2 ((_y (eql 4)) _z)
                (cons "four" (cl-call-next-method)))
  (should (equal (cl--generic-1 4 'b) '("four" 4 b))))

(ert-deftest cl-generic-test-06-multiple-dispatch ()
  (fmakunbound 'cl--generic-1)
  (cl-defgeneric cl--generic-1 (x y) "My doc.")
  (cl-defmethod cl--generic-1 (x y) (list x y))
  (cl-defmethod cl--generic-1 (_x (_y integer))
    (cons "y-int" (cl-call-next-method)))
  (cl-defmethod cl--generic-1 ((_x integer) _y)
    (cons "x-int" (cl-call-next-method)))
  (cl-defmethod cl--generic-1 ((_x integer) (_y integer))
    (cons "x&y-int" (cl-call-next-method)))
  (should (equal (cl--generic-1 1 2) '("x&y-int" "x-int" "y-int" 1 2))))

(ert-deftest cl-generic-test-07-apo ()
  (fmakunbound 'cl--generic-1)
  (cl-defgeneric cl--generic-1 (x y)
    (:documentation "My doc.") (:argument-precedence-order y x))
  (cl-defmethod cl--generic-1 (x y) (list x y))
  (cl-defmethod cl--generic-1 (_x (_y integer))
    (cons "y-int" (cl-call-next-method)))
  (cl-defmethod cl--generic-1 ((_x integer) _y)
    (cons "x-int" (cl-call-next-method)))
  (cl-defmethod cl--generic-1 ((_x integer) (_y integer))
    (cons "x&y-int" (cl-call-next-method)))
  (should (equal (cl--generic-1 1 2) '("x&y-int" "y-int" "x-int" 1 2))))

(ert-deftest cl-generic-test-08-after/before ()
  (let ((log ()))
    (fmakunbound 'cl--generic-1)
    (cl-defgeneric cl--generic-1 (x y))
    (cl-defmethod cl--generic-1 ((_x t) y) (cons y log))
    (cl-defmethod cl--generic-1 ((_x (eql 4)) _y)
    (cons "quatre" (cl-call-next-method)))
    (cl-defmethod cl--generic-1 :after (x _y)
      (push (list :after x) log))
    (cl-defmethod cl--generic-1 :before (x _y)
      (push (list :before x) log))
    (should (equal (cl--generic-1 4 6) '("quatre" 6 (:before 4))))
    (should (equal log '((:after 4) (:before 4))))))

(defun cl--generic-test-advice (&rest args) (cons "advice" (apply args)))

(ert-deftest cl-generic-test-09-advice ()
  (fmakunbound 'cl--generic-1)
  (cl-defgeneric cl--generic-1 (x y) "My doc.")
  (cl-defmethod cl--generic-1 (x y) (list x y))
  (advice-add 'cl--generic-1 :around #'cl--generic-test-advice)
  (should (equal (cl--generic-1 4 5) '("advice" 4 5)))
  (cl-defmethod cl--generic-1 ((_x integer) _y)
    (cons "integer" (cl-call-next-method)))
  (should (equal (cl--generic-1 4 5) '("advice" "integer" 4 5)))
  (advice-remove 'cl--generic-1 #'cl--generic-test-advice)
  (should (equal (cl--generic-1 4 5) '("integer" 4 5))))

(ert-deftest cl-generic-test-10-weird ()
  (fmakunbound 'cl--generic-1)
  (cl-defgeneric cl--generic-1 (x &rest r) "My doc.")
  (cl-defmethod cl--generic-1 (x &rest r) (cons x r))
  ;; This kind of definition is not valid according to CLHS, but it does show
  ;; up in EIEIO's tests for no-next-method, so we should either
  ;; detect it and signal an error or do something meaningful with it.
  (cl-defmethod cl--generic-1 (x (y integer) &rest r)
    `("integer" ,y ,x ,@r))
  (should (equal (cl--generic-1 'a 'b) '(a b)))
  (should (equal (cl--generic-1 1 2) '("integer" 2 1))))

(ert-deftest cl-generic-test-11-next-method-p ()
  (fmakunbound 'cl--generic-1)
  (cl-defgeneric cl--generic-1 (x y))
  (cl-defmethod cl--generic-1 ((x t) y)
    (list x y (cl-next-method-p)))
  (cl-defmethod cl--generic-1 ((_x (eql 4)) _y)
    (cl-list* "quatre" (cl-next-method-p) (cl-call-next-method)))
  (should (equal (cl--generic-1 4 5) '("quatre" t 4 5 nil))))

(ert-deftest cl-generic-test-12-context ()
  (fmakunbound 'cl--generic-1)
  (cl-defgeneric cl--generic-1 ())
  (cl-defmethod cl--generic-1 (&context (overwrite-mode (eql t)))
    (list 'is-t (cl-call-next-method)))
  (cl-defmethod cl--generic-1 (&context (overwrite-mode (eql nil)))
    (list 'is-nil (cl-call-next-method)))
  (cl-defmethod cl--generic-1 () 'any)
  (should (equal (list (let ((overwrite-mode t))   (cl--generic-1))
                       (let ((overwrite-mode nil)) (cl--generic-1))
                       (let ((overwrite-mode 1))   (cl--generic-1)))
                 '((is-t any) (is-nil any) any))))

(ert-deftest cl-generic-test-13-head ()
  (fmakunbound 'cl--generic-1)
  (cl-defgeneric cl--generic-1 (x y))
  (cl-defmethod cl--generic-1 ((x t) y) (cons x y))
  (cl-defmethod cl--generic-1 ((_x (head 4)) _y)
    (cons "quatre" (cl-call-next-method)))
  (cl-defmethod cl--generic-1 ((_x (head 5)) _y)
    (cons "cinq" (cl-call-next-method)))
  (cl-defmethod cl--generic-1 ((_x (head 6)) y)
    (cons "six" (cl-call-next-method 'a y)))
  (should (equal (cl--generic-1 'a nil) '(a)))
  (should (equal (cl--generic-1 '(4) nil) '("quatre" (4))))
  (should (equal (cl--generic-1 '(5) nil) '("cinq" (5))))
  (should (equal (cl--generic-1 '(6) nil) '("six" a))))

(cl-defgeneric cl-generic-tests--generic (x))
(cl-defmethod cl-generic-tests--generic ((x string))
  (message "%s is a string" x))
(cl-defmethod cl-generic-tests--generic ((x integer))
  (message "%s is a number" x))
(cl-defgeneric cl-generic-tests--generic-without-methods (x y))
(defvar cl-generic-tests--this-file
  (file-truename (or load-file-name buffer-file-name)))

(ert-deftest cl-generic-tests--method-files--finds-methods ()
  "`method-files' returns a list of files and methods for a generic function."
  (let ((retval (cl--generic-method-files 'cl-generic-tests--generic)))
    (should (equal (length retval) 2))
    (mapc (lambda (x)
            (should (equal (car x) cl-generic-tests--this-file))
            (should (equal (cadr x) 'cl-generic-tests--generic)))
          retval)
    (should-not (equal (nth 0 retval) (nth 1 retval)))))

(ert-deftest cl-generic-tests--method-files--nonexistent-methods ()
  "`method-files' returns nil if asked to find a method which doesn't exist."
  (should-not (cl--generic-method-files 'cl-generic-tests--undefined-generic))
  (should-not (cl--generic-method-files 'cl-generic-tests--generic-without-methods)))

(provide 'cl-generic-tests)
;;; cl-generic-tests.el ends here
