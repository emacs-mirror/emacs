;;; eval-tests.el --- unit tests for src/eval.c      -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018 Free Software Foundation, Inc.

;; Author: Philipp Stephani <phst@google.com>

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

;; Unit tests for src/eval.c.

;;; Code:

(require 'ert)

(ert-deftest eval-tests--bug24673 ()
  "Check that Bug#24673 has been fixed."
  ;; This should not crash.
  (should-error (funcall '(closure)) :type 'invalid-function))

(defvar byte-compile-debug)

(ert-deftest eval-tests--bugs-24912-and-24913 ()
  "Check that Emacs doesn't accept weird argument lists.
Bug#24912 and Bug#24913."
  (dolist (args '((&optional) (&rest) (&optional &rest) (&rest &optional)
                  (&optional &rest a) (&optional a &rest)
                  (&rest a &optional) (&rest &optional a)
                  (&optional &optional) (&optional &optional a)
                  (&optional a &optional b)
                  (&rest &rest) (&rest &rest a)
                  (&rest a &rest b)))
    (should-error (eval `(funcall (lambda ,args)) t) :type 'invalid-function)
    (should-error (byte-compile-check-lambda-list args))
    (let ((byte-compile-debug t))
      (should-error (eval `(byte-compile (lambda ,args)) t)))))


(dolist (form '(let let*))
  (dolist (arg '(1 "a" [a]))
    (eval
     `(ert-deftest ,(intern (format "eval-tests--%s--%s" form (type-of arg))) ()
        ,(format "Check that the first argument of `%s' cannot be a %s"
                 form (type-of arg))
        (should-error (,form ,arg) :type 'wrong-type-argument))
     t)))

(ert-deftest eval-tests--if-dot-string ()
  "Check that Emacs rejects (if . \"string\")."
  (should-error (eval '(if . "abc")) :type 'wrong-type-argument)
  (let ((if-tail (list '(setcdr if-tail "abc") t)))
    (should-error (eval (cons 'if if-tail))))
  (let ((if-tail (list '(progn (setcdr if-tail "abc") nil) t)))
    (should-error (eval (cons 'if if-tail)))))

(ert-deftest eval-tests--let-with-circular-defs ()
  "Check that Emacs reports an error for (let VARS ...) when VARS is circular."
  (let ((vars (list 'v)))
    (setcdr vars vars)
    (dolist (let-sym '(let let*))
      (should-error (eval (list let-sym vars))))))

(ert-deftest eval-tests--mutating-cond ()
  "Check that Emacs doesn't crash on a cond clause that mutates during eval."
  (let ((clauses (list '((progn (setcdr clauses "ouch") nil)))))
    (should-error (eval (cons 'cond clauses)))))

(defun eval-tests--exceed-specbind-limit ()
  (defvar eval-tests--var1)
  (defvar eval-tests--var2)
  ;; Bind two variables, to make extra sure we hit the
  ;; `max-specpdl-size' limit before the `max-lisp-eval-depth' limit.
  (let ((eval-tests--var1 1)
        (eval-tests--var2 2))
    ;; Recurse until we hit the limit.
    (eval-tests--exceed-specbind-limit)))

(ert-deftest eval-exceed-specbind-with-signal-hook ()
  "Test for Bug#30481.
Check that Emacs doesn't crash when exceeding specbind limit with
`signal-hook-function' bound.  NOTE: Without the fix for
Bug#30481, this test can appear to pass, but cause a
crash/abort/malloc assert failure on the next test."
  (let ((max-specpdl-size (/ max-lisp-eval-depth 2))
        (signal-hook-function #'ignore))
    (should-error (eval-tests--exceed-specbind-limit))))

;;; eval-tests.el ends here
