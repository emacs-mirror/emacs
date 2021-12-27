;;; fcr-tests.e; --- Tests for FunCallableRecords  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

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
(require 'fcr)
(require 'cl-lib)

(fcr-defstruct (fcr-test
                (:copier fcr-test-copy)
                (:copier fcr-test-copy1 (fst)))
  "Simple FCR."
  fst snd name)

(cl-defmethod fcr-test-gen ((_x compiled-function)) "#<bytecode>")

(cl-defmethod fcr-test-gen ((_x cons)) "#<cons>")

(cl-defmethod fcr-test-gen ((_x fcr-object))
  (format "#<fcr:%s>" (cl-call-next-method)))

(cl-defmethod fcr-test-gen ((_x fcr-test))
  (format "#<fcr-test:%s>" (cl-call-next-method)))

(ert-deftest fcr-tests ()
  (let* ((i 42)
         (fcr1 (fcr-lambda (fcr-test (fst 1) (snd 2) (name "hi"))
                   ()
                 (list fst snd i)))
         (fcr2 (fcr-lambda (fcr-test (name (cl-incf i)) (fst (cl-incf i)))
                   ()
                 (list fst snd 152 i))))
    (should (equal (list (fcr-test--fst fcr1)
                         (fcr-test--snd fcr1)
                         (fcr-test--name fcr1))
                   '(1 2 "hi")))
    (should (equal (list (fcr-test--fst fcr2)
                         (fcr-test--snd fcr2)
                         (fcr-test--name fcr2))
                   '(44 nil 43)))
    (should (equal (funcall fcr1) '(1 2 44)))
    (should (equal (funcall fcr2) '(44 nil 152 44)))
    (should (equal (funcall (fcr-test-copy fcr1 :fst 7)) '(7 2 44)))
    (should (equal (funcall (fcr-test-copy1 fcr1 9)) '(9 2 44)))
    (should (cl-typep fcr1 'fcr-test))
    (should (cl-typep fcr1 'fcr-object))
    (should (member (fcr-test-gen fcr1)
                    '("#<fcr-test:#<fcr:#<cons>>>"
                      "#<fcr-test:#<fcr:#<bytecode>>>")))
    ))

(ert-deftest fcr-tests--limits ()
  (should
   (condition-case err
       (let ((lexical-binding t)
             (byte-compile-debug t))
         (byte-compile '(lambda ()
                          (let ((inc-where nil))
                            (fcr-lambda (advice (where 'foo)) ()
                              (setq inc-where (lambda () (setq where (1+ where))))
                              where))))
         nil)
     (error
      (and (eq 'error (car err))
           (string-match "where.*mutated" (cadr err))))))
  (should
   (condition-case err
       (progn (macroexpand '(fcr-defstruct fcr--foo a a))
              nil)
     (error
      (and (eq 'error (car err))
           (string-match "Duplicate slot name: a$" (cadr err))))))
  (should
   (condition-case err
       (progn (macroexpand '(fcr-defstruct (fcr--foo (:parent advice)) where))
              nil)
     (error
      (and (eq 'error (car err))
           (string-match "Duplicate slot name: where$" (cadr err))))))
  (should
   (condition-case err
       (progn (macroexpand '(fcr-lambda (advice (where 1) (where 2)) () where))
              nil)
     (error
      (and (eq 'error (car err))
           (string-match "Duplicate slot: where$" (cadr err)))))))

(fcr-defstruct (fcr-test-mut
                (:parent fcr-test)
                (:copier fcr-test-mut-copy))
  "Simple FCR with a mutable field."
  (mut :mutable t))

(ert-deftest fcr-test--mutate ()
  (let* ((f (fcr-lambda (fcr-test-mut (fst 0) (mut 3))
                (x)
              (+ x fst mut)))
         (f2 (fcr-test-mut-copy f :fst 50)))
    (should (equal (fcr-test-mut--mut f) 3))
    (should (equal (funcall f 5) 8))
    (should (equal (funcall f2 5) 58))
    (cl-incf (fcr-test-mut--mut f) 7)
    (should (equal (fcr-test-mut--mut f) 10))
    (should (equal (funcall f 5) 15))
    (should (equal (funcall f2 15) 68))))

;;; fcr-tests.el ends here.
