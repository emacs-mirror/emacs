;;; oclosure-tests.e; --- Tests for Open Closures  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
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
(require 'oclosure)
(require 'cl-lib)
(require 'eieio)

(oclosure-define (oclosure-test
                  (:copier oclosure-test-copy)
                  (:copier oclosure-test-copy1 (fst)))
  "Simple OClosure."
  fst snd (name :mutable t))

(cl-defmethod oclosure-test-gen ((_x compiled-function)) "#<bytecode>")

(cl-defmethod oclosure-test-gen ((_x interpreted-function)) "#<interpreted-function>")

(cl-defmethod oclosure-test-gen ((_x oclosure))
  (format "#<oclosure:%s>" (cl-call-next-method)))

(cl-defmethod oclosure-test-gen ((_x oclosure-test))
  (format "#<oclosure-test:%s>" (cl-call-next-method)))

(ert-deftest oclosure-test ()
  (let* ((i 42)
         (ocl1 (oclosure-lambda (oclosure-test (fst 1) (snd 2) (name "hi"))
                   ()
                 (list fst snd i)))
         (ocl2 (oclosure-lambda (oclosure-test (name (incf i)) (fst (incf i)))
                   ()
                 (list fst snd 152 i))))
    (should (equal (list (oclosure-test--fst ocl1)
                         (oclosure-test--snd ocl1)
                         (oclosure-test--name ocl1))
                   '(1 2 "hi")))
    (should (equal (list (oclosure-test--fst ocl2)
                         (oclosure-test--snd ocl2)
                         (oclosure-test--name ocl2))
                   '(44 nil 43)))
    (should (equal (funcall ocl1) '(1 2 44)))
    (should (equal (funcall ocl2) '(44 nil 152 44)))
    (should (equal (funcall (oclosure-test-copy ocl1 :fst 7)) '(7 2 44)))
    (should (equal (funcall (oclosure-test-copy1 ocl1 9)) '(9 2 44)))
    (should (cl-typep ocl1 'oclosure-test))
    (should (cl-typep ocl1 'oclosure))
    (should (member (oclosure-test-gen ocl1)
                    '("#<oclosure-test:#<oclosure:#<interpreted-function>>>"
                      "#<oclosure-test:#<oclosure:#<bytecode>>>")))
    (should (stringp (documentation #'oclosure-test--fst)))
    ))

(ert-deftest oclosure-test-limits ()
  (defvar byte-compile-debug)
  (should
   (condition-case err
       (let ((lexical-binding t)
             (byte-compile-debug t))
         (byte-compile '(lambda ()
                          (let ((inc-fst nil))
                            (oclosure-lambda (oclosure-test (fst 'foo)) ()
                              (setq inc-fst (lambda () (setq fst (1+ fst))))
                              fst))))
         nil)
     (error
      (and (eq 'error (car err))
           (string-match "fst.*mutated" (cadr err))))))
  (should
   (condition-case err
       (progn (macroexpand-all '(oclosure-define oclosure--foo a a))
              nil)
     (error
      (and (eq 'error (car err))
           (string-match "Duplicate slot name: a$" (cadr err))))))
  (should
   (condition-case err
       (progn (macroexpand-all
               '(oclosure-define (oclosure--foo (:parent oclosure-test)) fst))
              nil)
     (error
      (and (eq 'error (car err))
           (string-match "Duplicate slot name: fst$" (cadr err))))))
  (should
   (condition-case err
       (progn (macroexpand '(oclosure-lambda (oclosure-test (fst 1) (fst 2))
                                () fst))
              nil)
     (error
      (and (eq 'error (car err))
           (string-match "Duplicate slot: fst$" (cadr err)))))))

(cl-defmethod oclosure-interactive-form ((ot oclosure-test))
  (let ((snd (oclosure-test--snd ot)))
    (if (stringp snd) (list 'interactive snd))))

(ert-deftest oclosure-test-interactive-form ()
  (should (equal (interactive-form
                  (oclosure-lambda (oclosure-test (fst 1) (snd 2)) () fst))
                 nil))
  (should (equal (interactive-form
                  (oclosure-lambda (oclosure-test (fst 1) (snd 2)) ()
                    (interactive "r")
                    fst))
                 '(interactive "r")))
  (should (equal (interactive-form
                  (oclosure-lambda (oclosure-test (fst 1) (snd "P")) () fst))
                 '(interactive "P")))
  (should (not (commandp
                (oclosure-lambda (oclosure-test (fst 1) (snd 2)) () fst))))
  (should (commandp
           (oclosure-lambda (oclosure-test (fst 1) (snd "P")) () fst))))

(oclosure-define (oclosure-test-mut
                  (:parent oclosure-test)
                  (:copier oclosure-test-mut-copy))
  "Simple OClosure with a mutable field."
  (mut :mutable t))

(ert-deftest oclosure-test-mutate ()
  (let* ((f (oclosure-lambda (oclosure-test-mut (fst 0) (mut 3))
                (x)
              (+ x fst mut)))
         (f2 (oclosure-test-mut-copy f :fst 50)))
    (should (equal (oclosure-test-mut--mut f) 3))
    (should (equal (funcall f 5) 8))
    (should (equal (funcall f2 5) 58))
    (incf (oclosure-test-mut--mut f) 7)
    (should (equal (oclosure-test-mut--mut f) 10))
    (should (equal (funcall f 5) 15))
    (should (equal (funcall f2 15) 68))))

(ert-deftest oclosure-test-slot-value ()
  (require 'eieio)
  (let ((ocl (oclosure-lambda
                 (oclosure-test (fst 'fst1) (snd 'snd1) (name 'name1))
                 (x)
               (list name fst snd x))))
    (should (equal 'fst1  (slot-value ocl 'fst)))
    (should (equal 'snd1  (slot-value ocl 'snd)))
    (should (equal 'name1  (slot-value ocl 'name)))
    (setf (slot-value ocl 'name) 'new-name)
    (should (equal 'new-name (slot-value ocl 'name)))
    (should (equal '(new-name fst1 snd1 arg) (funcall ocl 'arg)))
    (should-error (setf (slot-value ocl 'fst) 'new-fst) :type 'setting-constant)
    (should (equal 'fst1  (slot-value ocl 'fst)))
    ))

;;; oclosure-tests.el ends here.
