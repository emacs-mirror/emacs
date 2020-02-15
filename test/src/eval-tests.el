;;; eval-tests.el --- unit tests for src/eval.c      -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2020 Free Software Foundation, Inc.

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
(eval-when-compile (require 'cl-lib))

(ert-deftest eval-tests--bug24673 ()
  "Check that Bug#24673 has been fixed."
  ;; This should not crash.
  (should-error (funcall '(closure)) :type 'invalid-function))

(defvar byte-compile-debug)

(ert-deftest eval-tests--bugs-24912-and-24913 ()
  "Check that Emacs doesn't accept weird argument lists.
Bug#24912 and Bug#24913."
  (dolist (args '((&rest &optional)
                  (&rest a &optional) (&rest &optional a)
                  (&optional &optional) (&optional &optional a)
                  (&optional a &optional b)
                  (&rest &rest) (&rest &rest a)
                  (&rest a &rest b)))
    (should-error (eval `(funcall (lambda ,args)) t) :type 'invalid-function)
    (should-error (byte-compile-check-lambda-list args))
    (let ((byte-compile-debug t))
      (ert-info ((format "bytecomp: args = %S" args))
       (should-error (eval `(byte-compile (lambda ,args)) t))))))

(ert-deftest eval-tests-accept-empty-optional-rest ()
  "Check that Emacs accepts empty &optional and &rest arglists.
Bug#24912."
  (dolist (args '((&optional) (&rest) (&optional &rest)
                  (&optional &rest a) (&optional a &rest)))
    (let ((fun `(lambda ,args 'ok)))
      (ert-info ("eval")
        (should (eq (funcall (eval fun t)) 'ok)))
      (ert-info ("byte comp check")
        (byte-compile-check-lambda-list args))
      (ert-info ("bytecomp")
        (let ((byte-compile-debug t))
          (should (eq (funcall (byte-compile fun)) 'ok)))))))


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

(ert-deftest defvar/bug31072 ()
  "Check that Bug#31072 is fixed."
  (should-error (eval '(defvar 1) t) :type 'wrong-type-argument))

(ert-deftest defvaralias-overwrite-warning ()
  "Test for Bug#5950."
  (defvar eval-tests--foo)
  (setq eval-tests--foo 2)
  (defvar eval-tests--foo-alias)
  (setq eval-tests--foo-alias 1)
  (cl-letf (((symbol-function 'display-warning)
             (lambda (type &rest _)
               (throw 'got-warning type))))
    ;; Warn if we lose a value through aliasing.
    (should (equal
             '(defvaralias losing-value eval-tests--foo-alias)
             (catch 'got-warning
               (defvaralias 'eval-tests--foo-alias 'eval-tests--foo))))
    ;; Don't warn if we don't.
    (makunbound 'eval-tests--foo-alias)
    (should (eq 'no-warning
                (catch 'got-warning
                  (defvaralias 'eval-tests--foo-alias 'eval-tests--foo)
                  'no-warning)))))

(ert-deftest eval-tests-byte-code-being-evaluated-is-protected-from-gc ()
  "Regression test for Bug#33014.
Check that byte-compiled objects being executed by exec-byte-code
are found on the stack and therefore not garbage collected."
  (should (string= (eval-tests-33014-func)
                   "before after: ok foo: (e) bar: (a b c d e) baz: a bop: c")))

(defvar eval-tests-33014-var "ok")
(defun eval-tests-33014-func ()
  "A function which has a non-trivial constants vector when byte-compiled."
  (let ((result "before "))
    (eval-tests-33014-redefine)
    (garbage-collect)
    (setq result (concat result (format "after: %s" eval-tests-33014-var)))
    (let ((vals '(0 1 2 3))
          (things '(a b c d e)))
      (dolist (val vals)
        (setq result
              (concat result " "
                      (cond
                       ((= val 0) (format "foo: %s" (last things)))
                       ((= val 1) (format "bar: %s" things))
                       ((= val 2) (format "baz: %s" (car things)))
                       (t (format "bop: %s" (nth 2 things))))))))
    result))

(defun eval-tests-33014-redefine ()
  "Remove the Lisp reference to the byte-compiled object."
  (setf (symbol-function #'eval-tests-33014-func) nil))

(defun eval-tests-19790-backquote-comma-dot-substitution ()
  "Regression test for Bug#19790.
Don't handle destructive splicing in backquote expressions (like
in Common Lisp).  Instead, make sure substitution in backquote
expressions works for identifiers starting with period."
  (should (equal (let ((.x 'identity)) (eval `(,.x 'ok))) 'ok)))

;;; eval-tests.el ends here
