;;; generator-tests.el --- Testing generators -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 Free Software Foundation, Inc.

;; Author: Daniel Colascione <dancol@dancol.org>
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

(require 'generator)
(require 'ert)
(require 'cl-lib)

(defun generator-list-subrs ()
  (cl-loop for x being the symbols
        when (and (fboundp x)
                  (cps--special-form-p (symbol-function x)))
        collect x))

(defmacro cps-testcase (name &rest body)
  "Perform a simple test of the continuation-transforming code.

`cps-testcase' defines an ERT testcase called NAME that evaluates
BODY twice: once using ordinary `eval' and once using
lambda-generators.  The test ensures that the two forms produce
identical output.
"
  `(progn
     (ert-deftest ,name ()
       (should
        (equal
         (funcall (lambda () ,@body))
         (iter-next
          (funcall
           (iter-lambda () (iter-yield (progn ,@body))))))))
     (ert-deftest ,(intern (format "%s-noopt" name)) ()
       (should
        (equal
         (funcall (lambda () ,@body))
         (iter-next
          (funcall
           (let ((cps-inhibit-atomic-optimization t))
             (iter-lambda () (iter-yield (progn ,@body)))))))))))

(put 'cps-testcase 'lisp-indent-function 1)

(defvar *cps-test-i* nil)
(defun cps-get-test-i ()
  *cps-test-i*)

(cps-testcase cps-simple-1 (progn 1 2 3))
(cps-testcase cps-empty-progn (progn))
(cps-testcase cps-inline-not-progn (inline 1 2 3))
(cps-testcase cps-prog1-a (prog1 1 2 3))
(cps-testcase cps-prog1-b (prog1 1))
(cps-testcase cps-prog1-c (prog2 1 2 3))
(cps-testcase cps-quote (progn 'hello))
(cps-testcase cps-function (progn #'hello))

(cps-testcase cps-and-fail (and 1 nil 2))
(cps-testcase cps-and-succeed (and 1 2 3))
(cps-testcase cps-and-empty (and))

(cps-testcase cps-or-fallthrough (or nil 1 2))
(cps-testcase cps-or-alltrue (or 1 2 3))
(cps-testcase cps-or-empty (or))

(cps-testcase cps-let* (let* ((i 10)) i))
(cps-testcase cps-let*-shadow-empty (let* ((i 10)) (let (i) i)))
(cps-testcase cps-let (let ((i 10)) i))
(cps-testcase cps-let-shadow-empty (let ((i 10)) (let (i) i)))
(cps-testcase cps-let-novars (let nil 42))
(cps-testcase cps-let*-novars (let* nil 42))

(cps-testcase cps-let-parallel
  (let ((a 5) (b 6)) (let ((a b) (b a)) (list a b))))

(cps-testcase cps-let*-parallel
  (let* ((a 5) (b 6)) (let* ((a b) (b a)) (list a b))))

(cps-testcase cps-while-dynamic
  (setq *cps-test-i* 0)
  (while (< *cps-test-i* 10)
    (setf *cps-test-i* (+ *cps-test-i* 1)))
  *cps-test-i*)

(cps-testcase cps-while-lexical
 (let* ((i 0) (j 10))
   (while (< i 10)
     (setf i (+ i 1))
     (setf j (+ j (* i 10))))
   j))

(cps-testcase cps-while-incf
 (let* ((i 0) (j 10))
   (while (< i 10)
     (cl-incf i)
     (setf j (+ j (* i 10))))
   j))

(cps-testcase cps-dynbind
 (setf *cps-test-i* 0)
 (let* ((*cps-test-i* 5))
   (cps-get-test-i)))

(cps-testcase cps-nested-application
 (+ (+ 3 5) 1))

(cps-testcase cps-unwind-protect
 (setf *cps-test-i* 0)
 (unwind-protect
     (setf *cps-test-i* 1)
   (setf *cps-test-i* 2))
 *cps-test-i*)

(cps-testcase cps-catch-unused
 (catch 'mytag 42))

(cps-testcase cps-catch-thrown
 (1+ (catch 'mytag
       (throw 'mytag (+ 2 2)))))

(cps-testcase cps-loop
 (cl-loop for x from 1 to 10 collect x))

(cps-testcase cps-loop-backquote
 `(a b ,(cl-loop for x from 1 to 10 collect x) -1))

(cps-testcase cps-if-branch-a
 (if t 'abc))

(cps-testcase cps-if-branch-b
 (if t 'abc 'def))

(cps-testcase cps-if-condition-fail
 (if nil 'abc 'def))

(cps-testcase cps-cond-empty
 (cond))

(cps-testcase cps-cond-atomi
 (cond (42)))

(cps-testcase cps-cond-complex
 (cond (nil 22) ((1+ 1) 42) (t 'bad)))

(put 'cps-test-error 'error-conditions '(cps-test-condition))

(cps-testcase cps-condition-case
  (condition-case
      condvar
      (signal 'cps-test-error 'test-data)
    (cps-test-condition condvar)))

(cps-testcase cps-condition-case-no-error
  (condition-case
      condvar
      42
    (cps-test-condition condvar)))

(ert-deftest cps-generator-basic ()
  (let* ((gen (iter-lambda ()
                (iter-yield 1)
                (iter-yield 2)
                (iter-yield 3)
                4))
         (gen-inst (funcall gen)))
    (should (eql (iter-next gen-inst) 1))
    (should (eql (iter-next gen-inst) 2))
    (should (eql (iter-next gen-inst) 3))

    ;; should-error doesn't catch the generator-end condition (which
    ;; isn't an error), so we write our own.
    (let (errored)
      (condition-case x
          (iter-next gen-inst)
        (iter-end-of-sequence
         (setf errored (cdr x))))
      (should (eql errored 4)))))

(iter-defun mygenerator (i)
  (iter-yield 1)
  (iter-yield i)
  (iter-yield 2))

(ert-deftest cps-test-iter-do ()
  (let (mylist)
    (iter-do (x (mygenerator 4))
      (push x mylist))
    (should (equal mylist '(2 4 1)))))

(iter-defun gen-using-yield-value ()
  (let (f)
    (setf f (iter-yield 42))
    (iter-yield f)
    -8))

(ert-deftest cps-yield-value ()
  (let ((it (gen-using-yield-value)))
    (should (eql (iter-next it -1) 42))
    (should (eql (iter-next it -1) -1))))

(ert-deftest cps-loop ()
  (should
   (equal (cl-loop for x iter-by (mygenerator 42)
             collect x)
          '(1 42 2))))

(iter-defun gen-using-yield-from ()
  (let ((sub-iter (gen-using-yield-value)))
    (iter-yield (1+ (iter-yield-from sub-iter)))))

(ert-deftest cps-test-yield-from-works ()
  (let ((it (gen-using-yield-from)))
    (should (eql (iter-next it -1) 42))
    (should (eql (iter-next it -1) -1))
    (should (eql (iter-next it -1) -7))))

(defvar cps-test-closed-flag nil)

(ert-deftest cps-test-iter-close ()
  (garbage-collect)
  (let ((cps-test-closed-flag nil))
    (let ((iter (funcall
                 (iter-lambda ()
                   (unwind-protect (iter-yield 1)
                     (setf cps-test-closed-flag t))))))
      (should (equal (iter-next iter) 1))
      (should (not cps-test-closed-flag))
      (iter-close iter)
      (should cps-test-closed-flag))))

(ert-deftest cps-test-iter-close-idempotent ()
  (garbage-collect)
  (let ((cps-test-closed-flag nil))
    (let ((iter (funcall
                 (iter-lambda ()
                   (unwind-protect (iter-yield 1)
                     (setf cps-test-closed-flag t))))))
      (should (equal (iter-next iter) 1))
      (should (not cps-test-closed-flag))
      (iter-close iter)
      (should cps-test-closed-flag)
      (setf cps-test-closed-flag nil)
      (iter-close iter)
      (should (not cps-test-closed-flag)))))

(ert-deftest cps-test-iter-cleanup-once-only ()
  (let* ((nr-unwound 0)
         (iter
          (funcall (iter-lambda ()
                     (unwind-protect
                          (progn
                            (iter-yield 1)
                            (error "test")
                            (iter-yield 2))
                       (cl-incf nr-unwound))))))
    (should (equal (iter-next iter) 1))
    (should-error (iter-next iter))
    (should (equal nr-unwound 1))))

(iter-defun generator-with-docstring ()
  "Documentation!"
  (declare (indent 5))
  nil)

(ert-deftest cps-test-declarations-preserved ()
  (should (equal (documentation 'generator-with-docstring) "Documentation!"))
  (should (equal (get 'generator-with-docstring 'lisp-indent-function) 5)))

(ert-deftest cps-iter-lambda-with-dynamic-binding ()
  "`iter-lambda' with dynamic binding produces correct result (bug#25965)."
  (should (= 1
             (iter-next
              (funcall (iter-lambda ()
                         (let* ((fill-column 10) ;;any special variable will do
                                (i 0)
                                (j (setq i (1+ i))))
                           (iter-yield i))))))))

(ert-deftest iter-lambda-variable-shadowing ()
  "`iter-lambda' forms which have local variable shadowing (Bug#26073)."
  (should (equal (iter-next
                  (funcall (iter-lambda ()
                             (let ((it 1))
                               (iter-yield (funcall
                                            (lambda (it) (- it))
                                            (1+ it)))))))
                 -2)))
