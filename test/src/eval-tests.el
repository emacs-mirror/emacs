;;; eval-tests.el --- unit tests for src/eval.c      -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2026 Free Software Foundation, Inc.

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
(require 'subr-x)

(ert-deftest eval-tests--bug24673 ()
  "Check that Bug#24673 has been fixed."
  ;; This should not crash.
  (should-error (funcall '(closure)) :type 'invalid-function))

(defvar byte-compile-debug)

(ert-deftest eval-tests--bugs-24912-and-24913 ()
  "Check that Emacs doesn't accept weird argument lists.
Bug#24912 and Bug#24913."
  (dolist (lb '(t false))
    (ert-info ((prin1-to-string lb) :prefix "lexical-binding: ")
      (let ((lexical-binding lb))
        (dolist (args '((&rest &optional)
                        (&rest a &optional) (&rest &optional a)
                        (&optional &optional) (&optional &optional a)
                        (&optional a &optional b)
                        (&rest &rest) (&rest &rest a)
                        (&rest a &rest b)
                        (&rest) (&optional &rest)
                        ))
          (ert-info ((prin1-to-string args) :prefix "args: ")
            (should-error
             (eval `(funcall (lambda ,args)) lb) :type 'invalid-function)
            (should-error (byte-compile-check-lambda-list args))
            (let ((byte-compile-debug t))
              (should-error (eval `(byte-compile (lambda ,args)) lb)))))))))

(ert-deftest eval-tests-accept-empty-optional ()
  "Check that Emacs accepts empty &optional arglists.
Bug#24912."
  (dolist (lb '(t false))
    (ert-info ((prin1-to-string lb) :prefix "lexical-binding: ")
      (let ((lexical-binding lb))
        (dolist (args '((&optional) (&optional &rest a)))
          (ert-info ((prin1-to-string args) :prefix "args: ")
            (let ((fun `(lambda ,args 'ok)))
              (ert-info ("eval")
                (should (eq (funcall (eval fun lb)) 'ok)))
              (ert-info ("byte comp check")
                (byte-compile-check-lambda-list args))
              (ert-info ("bytecomp")
                (let ((byte-compile-debug t))
                  (should (eq (funcall (byte-compile fun)) 'ok)))))))))))


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
  (should-error (eval '(if . "abc") nil) :type 'wrong-type-argument)
  (should-error (eval '(if . "abc") t) :type 'wrong-type-argument)
  (let ((if-tail (list '(setcdr if-tail "abc") t)))
    (should-error (eval (cons 'if if-tail) nil) :type 'void-variable)
    (should-error (eval (cons 'if if-tail) t) :type 'void-variable))
  (let ((if-tail (list '(progn (setcdr if-tail "abc") nil) t)))
    (should-error (eval (cons 'if if-tail) nil) :type 'void-variable)
    (should-error (eval (cons 'if if-tail) t) :type 'void-variable)))

(ert-deftest eval-tests--let-with-circular-defs ()
  "Check that Emacs reports an error for (let VARS ...) when VARS is circular."
  (let ((vars (list 'v)))
    (setcdr vars vars)
    (dolist (let-sym '(let let*))
      (should-error (eval (list let-sym vars) nil)))))

(ert-deftest eval-tests--mutating-cond ()
  "Check that Emacs doesn't crash on a cond clause that mutates during eval."
  (let ((clauses (list '((progn (setcdr clauses "ouch") nil)))))
    (should-error (eval (cons 'cond clauses) nil))
    (should-error (eval (cons 'cond clauses) t))))

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

(ert-deftest eval-tests-19790-backquote-comma-dot-substitution ()
  "Regression test for Bug#19790.
Don't handle destructive splicing in backquote expressions (like
in Common Lisp).  Instead, make sure substitution in backquote
expressions works for identifiers starting with period."
  (should (equal (let ((.x 'identity)) (eval `(,.x 'ok) nil)) 'ok))
  (should (equal (let ((.x 'identity)) (eval `(,.x 'ok) t)) 'ok)))

(ert-deftest eval-tests/backtrace-in-batch-mode ()
  (let ((emacs (expand-file-name invocation-name invocation-directory)))
    (skip-unless (file-executable-p emacs))
    (with-temp-buffer
      (let ((status (call-process emacs nil t nil
                                  "--quick" "--batch"
                                  (concat "--eval="
                                          (prin1-to-string
                                           '(progn
                                              (defun foo () (error "Boo"))
                                              (foo)))))))
        (should (natnump status))
        (should-not (eql status 0)))
      (goto-char (point-min))
      (ert-info ((concat "Process output:\n" (buffer-string)))
        (search-forward "  foo()")
        (search-forward "  normal-top-level()")))))

(ert-deftest eval-tests/backtrace-in-batch-mode/inhibit ()
  (let ((emacs (expand-file-name invocation-name invocation-directory)))
    (skip-unless (file-executable-p emacs))
    (with-temp-buffer
      (let ((status (call-process
                     emacs nil t nil
                     "--quick" "--batch"
                     (concat "--eval="
                             (prin1-to-string
                              '(progn
                                 (defun foo () (error "Boo"))
                                 (let ((backtrace-on-error-noninteractive nil))
                                   (foo))))))))
        (should (natnump status))
        (should-not (eql status 0)))
      (should (equal (string-trim (buffer-string)) "Boo")))))

(ert-deftest eval-tests/backtrace-in-batch-mode/demoted-errors ()
  (let ((emacs (expand-file-name invocation-name invocation-directory)))
    (skip-unless (file-executable-p emacs))
    (with-temp-buffer
      (should (eql  0 (call-process emacs nil t nil
                                    "--quick" "--batch"
                                    (concat "--eval="
                                            (prin1-to-string
                                             '(with-demoted-errors "Error: %S"
                                                (error "Boo")))))))
      (goto-char (point-min))
      (should (equal (string-trim (buffer-string))
                     "Error: (error \"Boo\")")))))

(ert-deftest eval-tests/funcall-with-delayed-message ()
  ;; Check that `funcall-with-delayed-message' displays its message before
  ;; its function terminates if the timeout is short enough.

  ;; This also serves as regression test for bug#55628 where a short
  ;; timeout was rounded up to the next whole second.
  (dolist (params '((0.8 0.4)
                    (0.1 0.8)))
    (let ((timeout (nth 0 params))
          (work-time (nth 1 params)))
      (ert-info ((prin1-to-string params) :prefix "params: ")
        (with-current-buffer "*Messages*"
          (let ((inhibit-read-only t))
            (erase-buffer))
          (let ((stop (+ (float-time) work-time)))
            (funcall-with-delayed-message
             timeout "timed out"
             (lambda ()
               (while (< (float-time) stop))
               (message "finished"))))
          (let ((expected-messages
                 (if (< timeout work-time)
                     "timed out\nfinished"
                   "finished")))
            (should (equal (string-trim (buffer-string))
                           expected-messages))))))))

(defvar-local eval-test--local-var 'global)

(ert-deftest eval-test--bug62419 ()
  (with-temp-buffer
    (setq eval-test--local-var 'first-local)
    (let ((eval-test--local-var t))
      (kill-local-variable 'eval-test--local-var)
      (setq eval-test--local-var 'second-local)
      (should (eq eval-test--local-var 'second-local)))
    ;; FIXME: It's not completely clear if exiting the above `let'
    ;; should restore the buffer-local binding to `first-local'
    ;; (i.e. reset the value of the second buffer-local binding to the
    ;; first's initial value) or should do nothing (on the principle that
    ;; the first buffer-local binding doesn't exists any more so there's
    ;; nothing to restore).  I think both semantics make sense.
    ;;(should (eq eval-test--local-var 'first-local))
    )
  (should (eq eval-test--local-var 'global)))

(ert-deftest eval-tests-defvaralias ()
  (defvar eval-tests--my-var 'coo)
  (defvaralias 'eval-tests--my-var1 'eval-tests--my-var)
  (defvar eval-tests--my-var1)
  (should (equal eval-tests--my-var 'coo))
  (should (equal eval-tests--my-var1 'coo))

  (defvaralias 'eval-tests--my-a 'eval-tests--my-b)
  (defvaralias 'eval-tests--my-b 'eval-tests--my-c)

  (should-error (defvaralias 'eval-tests--my-c 'eval-tests--my-c)
                :type 'cyclic-variable-indirection)
  (defvaralias 'eval-tests--my-d 'eval-tests--my-a)
  (should-error (defvaralias 'eval-tests--my-c 'eval-tests--my-d)
                :type 'cyclic-variable-indirection))

(ert-deftest eval-tests--internal-delete-indirect-variable ()
  (defvar eval-tests--i-d-i-v-var 'foo)
  (defvaralias 'eval-tests--i-d-i-v-var1 'eval-tests--i-d-i-v-var "Doc string.")
  (internal-delete-indirect-variable 'eval-tests--i-d-i-v-var1)

  (should (eq (indirect-variable 'eval-tests--i-d-i-v-var1)
              'eval-tests--i-d-i-v-var1))
  (should-not (boundp 'eval-tests--i-d-i-v-var1))
  (should-not (get 'eval-tests--i-d-i-v-var1 'variable-documentation))

  (should-error (internal-delete-indirect-variable 'eval-tests--i-d-i-v-var)))

(defvar eval-tests/global-var 'global-value)
(defvar-local eval-tests/buffer-local-var 'default-value)
(ert-deftest eval-tests/default-value ()
  ;; `let' overrides the default value for global variables.
  (should (default-boundp 'eval-tests/global-var))
  (should (eq 'global-value (default-value 'eval-tests/global-var)))
  (should (eq 'global-value eval-tests/global-var))
  (let ((eval-tests/global-var 'let-value))
    (should (eq 'let-value (default-value 'eval-tests/global-var)))
    (should (eq 'let-value eval-tests/global-var)))
  ;; `let' overrides the default value everywhere, but leaves
  ;; buffer-local values unchanged in current buffer and in the
  ;; buffers where there is no explicitly set buffer-local value.
  (should (default-boundp 'eval-tests/buffer-local-var))
  (should (eq 'default-value (default-value 'eval-tests/buffer-local-var)))
  (should (eq 'default-value eval-tests/buffer-local-var))
  (with-temp-buffer
    (let ((eval-tests/buffer-local-var 'let-value))
      (should (eq 'let-value (default-value 'eval-tests/buffer-local-var)))
      (should (eq 'let-value eval-tests/buffer-local-var))))
  ;; When current buffer has explicit buffer-local binding, `let' does
  ;; not alter the default binding.
  (with-temp-buffer
    (setq-local eval-tests/buffer-local-var 'local-value)
    (let ((eval-tests/buffer-local-var 'let-value))
      ;; Let in a buffer with local binding does not change the
      ;; default value for variable.
      (should (eq 'default-value (default-value 'eval-tests/buffer-local-var)))
      (should (eq 'let-value eval-tests/buffer-local-var))
      (with-temp-buffer
        ;; We are in a new buffer - `eval-tests/buffer-local-var' has its global default value.
        (should (eq 'default-value (default-value 'eval-tests/buffer-local-var)))
        (should (eq 'default-value eval-tests/buffer-local-var))))))

(ert-deftest eval-tests--handler-bind ()
  ;; A `handler-bind' has no effect if no error is signaled.
  (should (equal (catch 'tag
                   (handler-bind ((error (lambda (_err) (throw 'tag 'wow))))
                     'noerror))
                 'noerror))
  ;; The handler is called from within the dynamic extent where the
  ;; error is signaled, unlike `condition-case'.
  (should (equal (catch 'tag
                   (handler-bind ((error (lambda (_err) (throw 'tag 'err))))
                     (list 'inner-catch
                           (catch 'tag
                             (user-error "hello")))))
                 '(inner-catch err)))
  ;; But inner condition handlers are temporarily muted.
  (should (equal (condition-case nil
                     (handler-bind
                         ((error (lambda (_err)
                                   (signal 'wrong-type-argument nil))))
                       (list 'result
                             (condition-case nil
                                 (user-error "hello")
                               (wrong-type-argument 'inner-handler))))
                   (wrong-type-argument 'wrong-type-argument))
                 'wrong-type-argument))
  ;; Handlers do not apply to the code run within the handlers.
  (should (equal (condition-case nil
                     (handler-bind
                         ((error (lambda (_err)
                                   (signal 'wrong-type-argument nil)))
                          (wrong-type-argument
                           (lambda (_err) (user-error "wrong-type-argument"))))
                       (user-error "hello"))
                   (wrong-type-argument 'wrong-type-argument)
                   (error 'plain-error))
                 'wrong-type-argument)))

(ert-deftest eval-tests--error-id ()
  (let* (inner-error
         (outer-error
          (condition-case err
              (handler-bind ((error (lambda (err) (setq inner-error err))))
                (car 1))
            (error err))))
    (should (eq inner-error outer-error))))

(ert-deftest eval-bad-specbind ()
  (should-error (eval '(let (((a b) 23)) (+ 1 2)) t)
                :type 'wrong-type-argument)
  (should-error (eval '(let* (((a b) 23)) (+ 1 2)) t)
                :type 'wrong-type-argument)
  (should-error (eval '(condition-case (a b) (+ 1 2) (:success 'ok)))
                :type 'wrong-type-argument)
  (should-error (eval '(funcall '(lambda ((a b) 3.15) 84) 5 4))))

;;; eval-tests.el ends here
