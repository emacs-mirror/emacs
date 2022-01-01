;;; subr-tests.el --- Tests for subr.el  -*- lexical-binding:t -*-

;; Copyright (C) 2015-2022 Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>,
;;         Nicolas Petton <nicolas@petton.fr>
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

;;

;;; Code:
(require 'ert)
(eval-when-compile (require 'cl-lib))

(ert-deftest let-when-compile ()
  ;; good case
  (should (equal (macroexpand '(let-when-compile ((foo (+ 2 3)))
                                (setq bar (eval-when-compile (+ foo foo)))
                                (setq boo (eval-when-compile (* foo foo)))))
                 '(progn
                   (setq bar (quote 10))
                   (setq boo (quote 25)))))
  ;; bad case: `eval-when-compile' omitted, byte compiler should catch this
  (should (equal (macroexpand
                  '(let-when-compile ((foo (+ 2 3)))
                    (setq bar (+ foo foo))
                    (setq boo (eval-when-compile (* foo foo)))))
                 '(progn
                   (setq bar (+ foo foo))
                   (setq boo (quote 25)))))
  ;; something practical
  (should (equal (macroexpand
                  '(let-when-compile ((keywords '("true" "false")))
                    (font-lock-add-keywords
                     'c++-mode
                     `((,(eval-when-compile
                           (format "\\<%s\\>" (regexp-opt keywords)))
                         0 font-lock-keyword-face)))))
                 '(font-lock-add-keywords
                   (quote c++-mode)
                   (list
                    (cons (quote
                           "\\<\\(?:\\(?:fals\\|tru\\)e\\)\\>")
                     (quote
                      (0 font-lock-keyword-face))))))))


;;;; List functions.

(ert-deftest subr-test-caaar ()
  (should (null (caaar '())))
  (should (null (caaar '(() (2)))))
  (should (null (caaar '((() (2)) (a b)))))
  (should-error (caaar '(1 2)) :type 'wrong-type-argument)
  (should-error (caaar '((1 2))) :type 'wrong-type-argument)
  (should (=  1 (caaar '(((1 2) (3 4))))))
  (should (null (caaar '((() (3 4)))))))

(ert-deftest subr-test-caadr ()
  (should (null (caadr '())))
  (should (null (caadr '(1))))
  (should-error (caadr '(1 2)) :type 'wrong-type-argument)
  (should (= 2 (caadr '(1 (2 3)))))
  (should (equal '((2) (3)) (caadr '((1) (((2) (3))) (4))))))


;;;; Keymap support.

(ert-deftest subr-test-kbd ()
  (should (equal (kbd "f") "f"))
  (should (equal (kbd "<f1>") [f1]))
  (should (equal (kbd "RET") "\C-m"))
  (should (equal (kbd "C-x a") "\C-xa"))
  ;; Check that kbd handles both new and old style key descriptions
  ;; (bug#45536).
  (should (equal (kbd "s-<return>") [s-return]))
  (should (equal (kbd "<s-return>") [s-return]))
  (should (equal (kbd "C-M-<return>") [C-M-return]))
  (should (equal (kbd "<C-M-return>") [C-M-return])))

(ert-deftest subr-test-define-prefix-command ()
  (define-prefix-command 'foo-prefix-map)
  (defvar foo-prefix-map)
  (declare-function foo-prefix-map "subr-tests")
  (should (keymapp foo-prefix-map))
  (should (fboundp #'foo-prefix-map))
  ;; With optional argument.
  (define-prefix-command 'bar-prefix 'bar-prefix-map)
  (defvar bar-prefix-map)
  (declare-function bar-prefix "subr-tests")
  (should (keymapp bar-prefix-map))
  (should (fboundp #'bar-prefix))
  ;; Returns the symbol.
  (should (eq (define-prefix-command 'foo-bar) 'foo-bar)))

(ert-deftest subr-test-local-key-binding ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (keymapp (local-key-binding [menu-bar])))
    (should-not (local-key-binding [f12]))))

(ert-deftest subr-test-global-key-binding ()
  (should (eq (global-key-binding [f1]) 'help-command))
  (should (eq (global-key-binding "x") 'self-insert-command))
  (should-not (global-key-binding [f12])))


;;;; Mode hooks.

(defalias 'subr-tests--parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

(ert-deftest provided-mode-derived-p ()
  ;; base case: `derived-mode' directly derives `prog-mode'
  (should (progn
            (define-derived-mode derived-mode prog-mode "test")
            (provided-mode-derived-p 'derived-mode 'prog-mode)))
  ;; edge case: `derived-mode' derives an alias of `prog-mode'
  (should (progn
            (define-derived-mode derived-mode subr-tests--parent-mode "test")
            (provided-mode-derived-p 'derived-mode 'prog-mode))))

(ert-deftest number-sequence-test ()
  (should (= (length
              (number-sequence (1- most-positive-fixnum) most-positive-fixnum))
             2))
  (should (= (length
              (number-sequence
               (1+ most-negative-fixnum) most-negative-fixnum -1))
             2)))

(ert-deftest string-comparison-test ()
  (should (string-lessp "abc" "acb"))
  (should (string-lessp "aBc" "abc"))
  (should (string-lessp "abc" "abcd"))
  (should (string-lessp "abc" "abcd"))
  (should-not (string-lessp "abc" "abc"))
  (should-not (string-lessp "" ""))

  (should (string-greaterp "acb" "abc"))
  (should (string-greaterp "abc" "aBc"))
  (should (string-greaterp "abcd" "abc"))
  (should (string-greaterp "abcd" "abc"))
  (should-not (string-greaterp "abc" "abc"))
  (should-not (string-greaterp "" ""))

  ;; Symbols are also accepted
  (should (string-lessp 'abc 'acb))
  (should (string-lessp "abc" 'acb))
  (should (string-greaterp 'acb 'abc))
  (should (string-greaterp "acb" 'abc)))

(ert-deftest subr-test-when ()
  (should (equal (when t 1) 1))
  (should (equal (when t 2) 2))
  (should (equal (when nil 1) nil))
  (should (equal (when nil 2) nil))
  (should (equal (when t 'x 1) 1))
  (should (equal (when t 'x 2) 2))
  (should (equal (when nil 'x 1) nil))
  (should (equal (when nil 'x 2) nil))
  (let ((x 1))
    (should-not (when nil
                  (setq x (1+ x))
                  x))
    (should (= x 1))
    (should (= 2 (when t
                   (setq x (1+ x))
                   x)))
    (should (= x 2)))
  (should (equal (macroexpand-all '(when a b c d))
                 '(if a (progn b c d)))))

(ert-deftest subr-test-xor ()
  "Test `xor'."
  (should-not (xor nil nil))
  (should (eq (xor nil 'true) 'true))
  (should (eq (xor 'true nil) 'true))
  (should-not (xor t t)))

(ert-deftest subr-test-version-parsing ()
  (should (equal (version-to-list ".5") '(0 5)))
  (should (equal (version-to-list "0.9 alpha1") '(0 9 -3 1)))
  (should (equal (version-to-list "0.9 snapshot") '(0  9 -4)))
  (should (equal (version-to-list "0.9-alpha1") '(0 9 -3 1)))
  (should (equal (version-to-list "0.9-snapshot") '(0  9 -4)))
  (should (equal (version-to-list "0.9.snapshot") '(0  9 -4)))
  (should (equal (version-to-list "0.9_snapshot") '(0  9 -4)))
  (should (equal (version-to-list "0.9alpha1") '(0 9 -3 1)))
  (should (equal (version-to-list "0.9snapshot") '(0  9 -4)))
  (should (equal (version-to-list "1.0 git") '(1  0 -4)))
  (should (equal (version-to-list "1.0 pre2") '(1 0 -1 2)))
  (should (equal (version-to-list "1.0-git") '(1  0 -4)))
  (should (equal (version-to-list "1.0-pre2") '(1 0 -1 2)))
  (should (equal (version-to-list "1.0.1-a") '(1 0 1 1)))
  (should (equal (version-to-list "1.0.1-f") '(1 0 1 6)))
  (should (equal (version-to-list "1.0.1.a") '(1 0 1 1)))
  (should (equal (version-to-list "1.0.1.f") '(1 0 1 6)))
  (should (equal (version-to-list "1.0.1_a") '(1 0 1 1)))
  (should (equal (version-to-list "1.0.1_f") '(1 0 1 6)))
  (should (equal (version-to-list "1.0.1a") '(1 0 1 1)))
  (should (equal (version-to-list "1.0.1f") '(1 0 1 6)))
  (should (equal (version-to-list "1.0.7.5") '(1 0 7 5)))
  (should (equal (version-to-list "1.0.git") '(1  0 -4)))
  (should (equal (version-to-list "1.0.pre2") '(1 0 -1 2)))
  (should (equal (version-to-list "1.0_git") '(1  0 -4)))
  (should (equal (version-to-list "1.0_pre2") '(1 0 -1 2)))
  (should (equal (version-to-list "1.0git") '(1  0 -4)))
  (should (equal (version-to-list "1.0pre2") '(1 0 -1 2)))
  (should (equal (version-to-list "22.8 beta3") '(22 8 -2 3)))
  (should (equal (version-to-list "22.8-beta3") '(22 8 -2 3)))
  (should (equal (version-to-list "22.8.beta3") '(22 8 -2 3)))
  (should (equal (version-to-list "22.8_beta3") '(22 8 -2 3)))
  (should (equal (version-to-list "22.8beta3") '(22 8 -2 3)))
  (should (equal (version-to-list "6.9.30 Beta") '(6 9 30 -2)))
  (should (equal (version-to-list "6.9.30-Beta") '(6 9 30 -2)))
  (should (equal (version-to-list "6.9.30.Beta") '(6 9 30 -2)))
  (should (equal (version-to-list "6.9.30Beta") '(6 9 30 -2)))
  (should (equal (version-to-list "6.9.30_Beta") '(6 9 30 -2)))

  (let ((text-quoting-style 'grave))
    (should (equal
             (error-message-string (should-error (version-to-list "OTP-18.1.5")))
             "Invalid version syntax: `OTP-18.1.5' (must start with a number)"))
    (should (equal
             (error-message-string (should-error (version-to-list "")))
             "Invalid version syntax: `' (must start with a number)"))
    (should (equal
             (error-message-string (should-error (version-to-list "1.0..7.5")))
             "Invalid version syntax: `1.0..7.5'"))
    (should (equal
             (error-message-string (should-error (version-to-list "1.0prepre2")))
             "Invalid version syntax: `1.0prepre2'"))
    (should (equal
             (error-message-string (should-error (version-to-list "22.8X3")))
             "Invalid version syntax: `22.8X3'"))
    (should (equal
             (error-message-string (should-error (version-to-list "beta22.8alpha3")))
             "Invalid version syntax: `beta22.8alpha3' (must start with a number)"))
    (should (equal
             (error-message-string (should-error (version-to-list "honk")))
             "Invalid version syntax: `honk' (must start with a number)")))
  (should (equal
            (error-message-string (should-error (version-to-list 9)))
            "Version must be a string"))

  (let ((version-separator "_"))
    (should (equal (version-to-list "_5") '(0 5)))
    (should (equal (version-to-list "0_9 alpha1") '(0 9 -3 1)))
    (should (equal (version-to-list "0_9 snapshot") '(0  9 -4)))
    (should (equal (version-to-list "0_9-alpha1") '(0 9 -3 1)))
    (should (equal (version-to-list "0_9-snapshot") '(0  9 -4)))
    (should (equal (version-to-list "0_9.alpha1") '(0 9 -3 1)))
    (should (equal (version-to-list "0_9.snapshot") '(0  9 -4)))
    (should (equal (version-to-list "0_9alpha1") '(0 9 -3 1)))
    (should (equal (version-to-list "0_9snapshot") '(0  9 -4)))
    (should (equal (version-to-list "1_0 git") '(1  0 -4)))
    (should (equal (version-to-list "1_0 pre2") '(1 0 -1 2)))
    (should (equal (version-to-list "1_0-git") '(1  0 -4)))
    (should (equal (version-to-list "1_0.pre2") '(1 0 -1 2)))
    (should (equal (version-to-list "1_0_1-a") '(1 0 1 1)))
    (should (equal (version-to-list "1_0_1-f") '(1 0 1 6)))
    (should (equal (version-to-list "1_0_1.a") '(1 0 1 1)))
    (should (equal (version-to-list "1_0_1.f") '(1 0 1 6)))
    (should (equal (version-to-list "1_0_1_a") '(1 0 1 1)))
    (should (equal (version-to-list "1_0_1_f") '(1 0 1 6)))
    (should (equal (version-to-list "1_0_1a") '(1 0 1 1)))
    (should (equal (version-to-list "1_0_1f") '(1 0 1 6)))
    (should (equal (version-to-list "1_0_7_5") '(1 0 7 5)))
    (should (equal (version-to-list "1_0_git") '(1  0 -4)))
    (should (equal (version-to-list "1_0pre2") '(1 0 -1 2)))
    (should (equal (version-to-list "22_8 beta3") '(22 8 -2 3)))
    (should (equal (version-to-list "22_8-beta3") '(22 8 -2 3)))
    (should (equal (version-to-list "22_8.beta3") '(22 8 -2 3)))
    (should (equal (version-to-list "22_8beta3") '(22 8 -2 3)))
    (should (equal (version-to-list "6_9_30 Beta") '(6 9 30 -2)))
    (should (equal (version-to-list "6_9_30-Beta") '(6 9 30 -2)))
    (should (equal (version-to-list "6_9_30.Beta") '(6 9 30 -2)))
    (should (equal (version-to-list "6_9_30Beta") '(6 9 30 -2)))

    (let ((text-quoting-style 'grave))
      (should (equal
               (error-message-string (should-error (version-to-list "1_0__7_5")))
               "Invalid version syntax: `1_0__7_5'"))
      (should (equal
               (error-message-string (should-error (version-to-list "1_0prepre2")))
               "Invalid version syntax: `1_0prepre2'"))
      (should (equal
               (error-message-string (should-error (version-to-list "22.8X3")))
               "Invalid version syntax: `22.8X3'"))
      (should (equal
               (error-message-string (should-error (version-to-list "beta22_8alpha3")))
               "Invalid version syntax: `beta22_8alpha3' (must start with a number)")))))

(ert-deftest subr-test-version-list-< ()
  (should (version-list-< '(0) '(1)))
  (should (version-list-< '(0 9) '(1 0)))
  (should (version-list-< '(1 -1) '(1 0)))
  (should (version-list-< '(1 -2) '(1 -1)))
  (should (not (version-list-< '(1) '(0))))
  (should (not (version-list-< '(1 1) '(1 0))))
  (should (not (version-list-< '(1) '(1 0))))
  (should (not (version-list-< '(1 0) '(1 0 0)))))

(ert-deftest subr-test-version-list-= ()
  (should (version-list-= '(1) '(1)))
  (should (version-list-= '(1 0) '(1)))
  (should (not (version-list-= '(0) '(1)))))

(ert-deftest subr-test-version-list-<= ()
  (should (version-list-<= '(0) '(1)))
  (should (version-list-<= '(1) '(1)))
  (should (version-list-<= '(1 0) '(1)))
  (should (not (version-list-<= '(1) '(0)))))

(defun subr-test--backtrace-frames-with-backtrace-frame (base)
  "Reference implementation of `backtrace-frames'."
  (let ((idx 0)
        (frame nil)
        (frames nil))
    (while (setq frame (backtrace-frame idx base))
      (push frame frames)
      (setq idx (1+ idx)))
    (nreverse frames)))

(defun subr-test--frames-2 (base)
  (let ((_dummy nil))
    (progn ;; Add a few frames to top of stack
      (unwind-protect
          (cons (mapcar (pcase-lambda (`(,evald ,func ,args ,_))
                          `(,evald ,func ,@args))
                        (backtrace-frames base))
                (subr-test--backtrace-frames-with-backtrace-frame base))))))

(defun subr-test--frames-1 (base)
  (subr-test--frames-2 base))

(ert-deftest subr-test-backtrace-simple-tests ()
  "Test backtrace-related functions (simple tests).
This exercises `backtrace-frame', and indirectly `mapbacktrace'."
  ;; `mapbacktrace' returns nil
  (should (equal (mapbacktrace #'ignore) nil))
  ;; Unbound BASE is silently ignored
  (let ((unbound (make-symbol "ub")))
    (should (equal (backtrace-frame 0 unbound) nil))
    (should (equal (mapbacktrace #'error unbound) nil)))
  ;; First frame is backtrace-related function
  (should (equal (backtrace-frame 0) '(t backtrace-frame 0)))
  (let ((throw-args (lambda (&rest args) (throw 'ret args))))
    (should (equal (catch 'ret (mapbacktrace throw-args))
                   `(t mapbacktrace (,throw-args) nil))))
  ;; Past-end NFRAMES is silently ignored
  (should (equal (backtrace-frame most-positive-fixnum) nil)))

(ert-deftest subr-test-backtrace-integration-test ()
  "Test backtrace-related functions (integration test).
This exercises `backtrace-frame', `backtrace-frames', and
indirectly `mapbacktrace'."
  ;; Compare two implementations of backtrace-frames
  (let ((frame-lists (subr-test--frames-1 'subr-test--frames-2)))
    (should (equal (car frame-lists) (cdr frame-lists)))))

(ert-deftest subr-tests--string-match-p--blank ()
  "Test that [:blank:] matches horizontal whitespace, cf. Bug#25366."
  (should (equal (string-match-p "\\`[[:blank:]]\\'" " ") 0))
  (should (equal (string-match-p "\\`[[:blank:]]\\'" "\t") 0))
  (should-not (string-match-p "\\`[[:blank:]]\\'" "\n"))
  (should-not (string-match-p "\\`[[:blank:]]\\'" "a"))
  (should (equal (string-match-p "\\`[[:blank:]]\\'" "\N{HAIR SPACE}") 0))
  (should (equal (string-match-p "\\`[[:blank:]]\\'" "\u3000") 0))
  (should-not (string-match-p "\\`[[:blank:]]\\'" "\N{LINE SEPARATOR}")))

(ert-deftest subr-tests--dolist--wrong-number-of-args ()
  "Test that `dolist' doesn't accept wrong types or length of SPEC,
cf. Bug#25477."
  (should-error (eval '(dolist (a)))
                :type 'wrong-number-of-arguments)
  (should-error (eval '(dolist (a () 'result 'invalid)) t)
                :type 'wrong-number-of-arguments)
  (should-error (eval '(dolist "foo") t)
                :type 'wrong-type-argument))

(ert-deftest subr-tests-bug22027 ()
  "Test for https://debbugs.gnu.org/22027 ."
  (let ((default "foo") res)
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &optional _init _hist def _inher-input) def)))
      (setq res (read-passwd "pass: " 'confirm (mapconcat #'string default "")))
      (should (string= default res)))))

(ert-deftest subr-tests--gensym ()
  "Test `gensym' behavior."
  (should (equal (symbol-name (let ((gensym-counter 0)) (gensym)))
                 "g0"))
  (should (eq (string-to-char (symbol-name (gensym))) ?g))
  (should (eq (string-to-char (symbol-name (gensym "X"))) ?X)))

(ert-deftest subr-tests--assq-delete-all ()
  "Test `assq-delete-all' behavior."
  (cl-flet ((new-list-fn
             ()
             (list (cons 'a 1) (cons 'b 2) (cons 'c 3) 'd (cons "foo" "bar"))))
    (should (equal (cdr (new-list-fn)) (assq-delete-all 'a (new-list-fn))))
    (should (equal (new-list-fn) (assq-delete-all 'd (new-list-fn))))
    (should (equal (new-list-fn) (assq-delete-all "foo" (new-list-fn))))))

(ert-deftest subr-tests--assoc-delete-all ()
  "Test `assoc-delete-all' behavior."
  (cl-flet ((new-list-fn
             ()
             (list (cons 'a 1) (cons 'b 2) (cons 'c 3) 'd (cons "foo" "bar"))))
    (should (equal (cdr (new-list-fn)) (assoc-delete-all 'a (new-list-fn))))
    (should (equal (new-list-fn) (assoc-delete-all 'd (new-list-fn))))
    (should (equal (butlast (new-list-fn))
                   (assoc-delete-all "foo" (new-list-fn))))))

(ert-deftest shell-quote-argument-%-on-w32 ()
  "Quoting of `%' in w32 shells isn't perfect.
See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=19350."
  :expected-result :failed
  (skip-unless (and (fboundp 'w32-shell-dos-semantics)
                    (w32-shell-dos-semantics)))
  (let ((process-environment (append '("ca^=with-caret"
                                       "ca=without-caret")
                                     process-environment)))
    ;; It actually results in
    ;;    without-caret with-caret
    (should (equal (shell-command-to-string
                    (format "echo %s %s"
                            "%ca%"
                            (shell-quote-argument "%ca%")))
                   "without-caret %ca%"))))

(ert-deftest subr-tests-flatten-tree ()
  "Test `flatten-tree' behavior."
  (should (equal (flatten-tree '(1 (2 . 3) nil (4 5 (6)) 7))
                 '(1 2 3 4 5 6 7)))
  (should (equal (flatten-tree '((1 . 2)))
                 '(1 2)))
  (should (equal (flatten-tree '(1 nil 2))
                 '(1 2)))
  (should (equal (flatten-tree 42)
                 '(42)))
  (should (equal (flatten-tree t)
                 '(t)))
  (should (equal (flatten-tree nil)
                 nil))
  (should (equal (flatten-tree '((nil) ((((nil)))) nil))
                 nil))
  (should (equal (flatten-tree '(1 ("foo" "bar") 2))
                 '(1 "foo" "bar" 2))))

(ert-deftest subr--tests-letrec ()
  ;; Test that simple cases of `letrec' get optimized back to `let*'.
  (should (equal (macroexpand '(letrec ((subr-tests-var1 1)
                                        (subr-tests-var2 subr-tests-var1))
                                 (+ subr-tests-var1 subr-tests-var2)))
                 '(let* ((subr-tests-var1 1)
                         (subr-tests-var2 subr-tests-var1))
                    (+ subr-tests-var1 subr-tests-var2)))))

(defvar subr-tests--hook nil)

(ert-deftest subr-tests-add-hook-depth ()
  "Test the `depth' arg of `add-hook'."
  (setq-default subr-tests--hook nil)
  (add-hook 'subr-tests--hook 'f1)
  (add-hook 'subr-tests--hook 'f2)
  (should (equal subr-tests--hook '(f2 f1)))
  (add-hook 'subr-tests--hook 'f3 t)
  (should (equal subr-tests--hook '(f2 f1 f3)))
  (add-hook 'subr-tests--hook 'f4 50)
  (should (equal subr-tests--hook '(f2 f1 f4 f3)))
  (add-hook 'subr-tests--hook 'f5 -50)
  (should (equal subr-tests--hook '(f5 f2 f1 f4 f3)))
  (add-hook 'subr-tests--hook 'f6)
  (should (equal subr-tests--hook '(f5 f6 f2 f1 f4 f3)))
  ;; Make sure t is equivalent to 90.
  (add-hook 'subr-tests--hook 'f7 90)
  (add-hook 'subr-tests--hook 'f8 t)
  (should (equal subr-tests--hook '(f5 f6 f2 f1 f4 f3 f7 f8)))
  ;; Make sure nil is equivalent to 0.
  (add-hook 'subr-tests--hook 'f9 0)
  (add-hook 'subr-tests--hook 'f10)
  (should (equal subr-tests--hook '(f5 f10 f9 f6 f2 f1 f4 f3 f7 f8)))
  )

(ert-deftest ignore-error-tests ()
  (should (equal (ignore-error (end-of-file)
                   (read ""))
                 nil))
  (should (equal (ignore-error end-of-file
                   (read ""))
                 nil))
  (should-error (ignore-error foo
                  (read ""))))

(ert-deftest string-replace ()
  (should (equal (string-replace "foo" "bar" "zot")
                 "zot"))
  (should (equal (string-replace "foo" "bar" "foozot")
                 "barzot"))
  (should (equal (string-replace "foo" "bar" "barfoozot")
                 "barbarzot"))
  (should (equal (string-replace "zot" "bar" "barfoozot")
                 "barfoobar"))
  (should (equal (string-replace "z" "bar" "barfoozot")
                 "barfoobarot"))
  (should (equal (string-replace "zot" "bar" "zat")
                 "zat"))
  (should (equal (string-replace "azot" "bar" "zat")
                 "zat"))
  (should (equal (string-replace "azot" "bar" "azot")
                 "bar"))

  (should (equal (string-replace "azot" "bar" "foozotbar")
                 "foozotbar"))

  (should (equal (string-replace "fo" "bar" "lafofofozot")
                 "labarbarbarzot"))

  (should (equal (string-replace "\377" "x" "a\377b")
                 "axb"))
  (should (equal (string-replace "\377" "x" "a\377ø")
                 "axø"))
  (should (equal (string-replace (string-to-multibyte "\377") "x" "a\377b")
                 "axb"))
  (should (equal (string-replace (string-to-multibyte "\377") "x" "a\377ø")
                 "axø"))

  (should (equal (string-replace "ana" "ANA" "ananas") "ANAnas"))

  (should (equal (string-replace "a" "" "") ""))
  (should (equal (string-replace "a" "" "aaaaa") ""))
  (should (equal (string-replace "ab" "" "ababab") ""))
  (should (equal (string-replace "ab" "" "abcabcabc") "ccc"))
  (should (equal (string-replace "a" "aa" "aaa") "aaaaaa"))
  (should (equal (string-replace "abc" "defg" "abc") "defg"))

  (should (equal (should-error (string-replace "" "x" "abc"))
                 '(wrong-length-argument 0))))

(ert-deftest subr-replace-regexp-in-string ()
  (should (equal (replace-regexp-in-string "a+" "xy" "abaabbabaaba")
                 "xybxybbxybxybxy"))
  ;; FIXEDCASE
  (let ((case-fold-search t))
    (should (equal (replace-regexp-in-string "a+" "xy" "ABAABBABAABA")
                   "XYBXYBBXYBXYBXY"))
    (should (equal (replace-regexp-in-string "a+" "xy" "ABAABBABAABA" t)
                   "xyBxyBBxyBxyBxy"))
    (should (equal (replace-regexp-in-string
                    "a[bc]*" "xyz"
                    "a A ab AB Ab aB abc ABC Abc AbC aBc")
                   "xyz XYZ xyz XYZ Xyz xyz xyz XYZ Xyz Xyz xyz"))
    (should (equal (replace-regexp-in-string
                    "a[bc]*" "xyz"
                    "a A ab AB Ab aB abc ABC Abc AbC aBc" t)
                   "xyz xyz xyz xyz xyz xyz xyz xyz xyz xyz xyz")))
  (let ((case-fold-search nil))
    (should (equal (replace-regexp-in-string "a+" "xy" "ABAABBABAABA")
                   "ABAABBABAABA")))
  ;; group substitution
  (should (equal (replace-regexp-in-string
                  "a\\(b*\\)" "<\\1,\\&>" "babbcaabacbab")
                 "b<bb,abb>c<,a><b,ab><,a>cb<b,ab>"))
  (should (equal (replace-regexp-in-string
                  "x\\(?2:..\\)\\(?1:..\\)\\(..\\)\\(..\\)\\(..\\)"
                  "<\\3,\\5,\\4,\\1,\\2>" "yxabcdefghijkl")
                 "y<ef,ij,gh,cd,ab>kl"))
  ;; LITERAL
  (should (equal (replace-regexp-in-string
                  "a\\(b*\\)" "<\\1,\\&>" "babbcaabacbab" nil t)
                 "b<\\1,\\&>c<\\1,\\&><\\1,\\&><\\1,\\&>cb<\\1,\\&>"))
  (should (equal (replace-regexp-in-string
                  "a" "\\\\,\\?" "aba")
                 "\\,\\?b\\,\\?"))
  (should (equal (replace-regexp-in-string
                  "a" "\\\\,\\?" "aba" nil t)
                 "\\\\,\\?b\\\\,\\?"))
  ;; SUBEXP
  (should (equal (replace-regexp-in-string
                  "\\(a\\)\\(b*\\)c" "xy" "babbcdacd" nil nil 2)
                 "baxycdaxycd"))
  ;; START
  (should (equal (replace-regexp-in-string
                  "ab" "x" "abcabdabeabf" nil nil nil 4)
                 "bdxexf"))
  ;; An empty pattern matches once before every character.
  (should (equal (replace-regexp-in-string "" "x" "abc")
                 "xaxbxc"))
  (should (equal (replace-regexp-in-string "y*" "x" "abc")
                 "xaxbxc"))
  ;; replacement function
  (should (equal (replace-regexp-in-string
                  "a\\(b*\\)c"
                  (lambda (s)
                    (format "<%s,%s,%s,%s,%s>"
                            s
                            (match-beginning 0) (match-end 0)
                            (match-beginning 1) (match-end 1)))
                  "babbcaacabc")
                 "b<abbc,0,4,1,3>a<ac,0,2,1,1><abc,0,3,1,2>"))
  ;; anchors (bug#15107, bug#44861)
  (should (equal (replace-regexp-in-string "a\\B" "b" "a aaaa")
                 "a bbba"))
  (should (equal (replace-regexp-in-string "\\`\\|x" "z" "--xx--")
                 "z--zz--")))

(ert-deftest subr-match-substitute-replacement ()
  (with-temp-buffer
    (insert "Alpha Beta Gamma Delta Epsilon")
    (goto-char (point-min))
    (re-search-forward "B\\(..\\)a")
    (should (equal (match-substitute-replacement "carrot")
                   "Carrot"))
    (should (equal (match-substitute-replacement "<\\&>")
                   "<Beta>"))
    (should (equal (match-substitute-replacement "m\\1a")
                   "Meta"))
    (should (equal (match-substitute-replacement "ernin" nil nil nil 1)
                   "Bernina")))
  (let ((s "Tau Beta Gamma Delta Epsilon"))
    (string-match "B\\(..\\)a" s)
    (should (equal (match-substitute-replacement "carrot" nil nil s)
                   "Carrot"))
    (should (equal (match-substitute-replacement "<\\&>" nil nil s)
                   "<Beta>"))
    (should (equal (match-substitute-replacement "m\\1a" nil nil s)
                   "Meta"))
    (should (equal (match-substitute-replacement "ernin" nil nil s 1)
                   "Bernina"))))

(ert-deftest subr-tests--change-group-33341 ()
  (with-temp-buffer
    (buffer-enable-undo)
    (insert "0\n")
    (let ((g (prepare-change-group)))
      (activate-change-group g)
      (insert "b\n")
      (insert "c\n")
      (cancel-change-group g))
    (should (equal (buffer-string) "0\n"))
    (erase-buffer)
    (setq buffer-undo-list nil)
    (insert "0\n")
    (let ((g (prepare-change-group)))
      (activate-change-group g)
      (insert "b\n")
      (insert "c\n")
      (accept-change-group g))
    (should (equal (buffer-string) "0\nb\nc\n"))
    (undo-boundary)
    (undo)
    (should (equal (buffer-string) ""))))

(defvar subr--ordered nil)

(ert-deftest subr--add-to-ordered-list-eq ()
  (setq subr--ordered nil)
  (add-to-ordered-list 'subr--ordered 'b 2)
  (should (equal subr--ordered '(b)))
  (add-to-ordered-list 'subr--ordered 'c 3)
  (should (equal subr--ordered '(b c)))
  (add-to-ordered-list 'subr--ordered 'a 1)
  (should (equal subr--ordered '(a b c)))
  (add-to-ordered-list 'subr--ordered 'e)
  (should (equal subr--ordered '(a b c e)))
  (add-to-ordered-list 'subr--ordered 'd 4)
  (should (equal subr--ordered '(a b c d e)))
  (add-to-ordered-list 'subr--ordered 'e)
  (should (equal subr--ordered '(a b c d e)))
  (add-to-ordered-list 'subr--ordered 'b 5)
  (should (equal subr--ordered '(a c d b e))))


;;; Apropos.

(ert-deftest apropos-apropos-internal ()
  (should (equal (apropos-internal "^next-line$") '(next-line)))
  (should (>= (length (apropos-internal "^help")) 100))
  (should-not (apropos-internal "^test-a-missing-symbol-foo-bar-zot$")))

(ert-deftest apropos-apropos-internal/predicate ()
  (should (equal (apropos-internal "^next-line$" #'commandp) '(next-line)))
  (should (>= (length (apropos-internal "^help" #'commandp)) 15))
  (should-not (apropos-internal "^next-line$" #'keymapp)))


(ert-deftest test-buffer-local-boundp ()
  (let ((buf (generate-new-buffer "boundp")))
    (with-current-buffer buf
      (setq-local test-boundp t))
    (setq test-global-boundp t)
    (should (buffer-local-boundp 'test-boundp buf))
    (should-not (buffer-local-boundp 'test-not-boundp buf))
    (should (buffer-local-boundp 'test-global-boundp buf))))

(ert-deftest test-replace-string-in-region ()
  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should (= (replace-string-in-region "foo" "new" (point-min) (point-max))
               2))
    (should (equal (buffer-string) "new bar zot newbar")))

  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should (= (replace-string-in-region "foo" "new" (point-min) 14)
               1))
    (should (equal (buffer-string) "new bar zot foobar")))

  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should-error (replace-string-in-region "foo" "new" (point-min) 30)))

  (with-temp-buffer
    (insert "Foo bar zot foobar")
    (should (= (replace-string-in-region "Foo" "new" (point-min))
               1))
    (should (equal (buffer-string) "new bar zot foobar"))))

(ert-deftest test-replace-regexp-in-region ()
  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should (= (replace-regexp-in-region "fo+" "new" (point-min) (point-max))
               2))
    (should (equal (buffer-string) "new bar zot newbar")))

  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should (= (replace-regexp-in-region "fo+" "new" (point-min) 14)
               1))
    (should (equal (buffer-string) "new bar zot foobar")))

  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should-error (replace-regexp-in-region "fo+" "new" (point-min) 30)))

  (with-temp-buffer
    (insert "Foo bar zot foobar")
    (should (= (replace-regexp-in-region "Fo+" "new" (point-min))
               1))
    (should (equal (buffer-string) "new bar zot foobar"))))

(ert-deftest test-with-existing-directory ()
  (let ((dir (make-temp-name "/tmp/not-exist-")))
    (let ((default-directory dir))
      (should-not (file-exists-p default-directory)))
    (with-existing-directory
      (should-not (equal dir default-directory))
      (should (file-exists-p default-directory)))))

(ert-deftest subr-test-internal--format-docstring-line ()
  (should
   (string= (let ((fill-column 70))
              (internal--format-docstring-line
               "In addition to any hooks its parent mode might have run, this \
mode runs the hook ‘foo-bar-baz-very-long-name-indeed-mode-hook’, as the final \
or penultimate step during initialization."))
            "In addition to any hooks its parent mode might have run, this mode
runs the hook ‘foo-bar-baz-very-long-name-indeed-mode-hook’, as the
final or penultimate step during initialization.")))

(ert-deftest test-ensure-list ()
  (should (equal (ensure-list nil) nil))
  (should (equal (ensure-list :foo) '(:foo)))
  (should (equal (ensure-list '(1 2 3)) '(1 2 3))))

(provide 'subr-tests)
;;; subr-tests.el ends here
