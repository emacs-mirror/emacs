;;; gv-tests.el --- tests for gv.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 Free Software Foundation, Inc.

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

(require 'edebug)
(require 'ert)
(eval-when-compile (require 'cl-lib))

(cl-defmacro gv-tests--in-temp-dir ((elvar elcvar)
                                    (&rest filebody)
                                    &rest body)
  (declare (indent 2))
  `(let ((default-directory (make-temp-file "gv-test" t)))
     (unwind-protect
         (let ((,elvar "gv-test-deffoo.el")
               (,elcvar "gv-test-deffoo.elc"))
           (with-temp-file ,elvar
             (insert ";; -*- lexical-binding: t; -*-\n")
             (dolist (form ',filebody)
               (pp form (current-buffer))))
           ,@body)
       (delete-directory default-directory t))))

(ert-deftest gv-define-expander-in-file ()
  (gv-tests--in-temp-dir (el elc)
      ((gv-define-setter gv-test-foo (newval cons)
         `(setcar ,cons ,newval))
       (defvar gv-test-pair (cons 1 2))
       (setf (gv-test-foo gv-test-pair) 99)
       (message "%d" (car gv-test-pair)))
    (with-temp-buffer
      (call-process (concat invocation-directory invocation-name)
                    nil '(t t) nil
                    "-Q" "-batch" "--eval" (prin1-to-string `(byte-compile-file ,el))
                    "-l" elc)
      (should (equal (buffer-string) "99\n")))))

(ert-deftest gv-define-expander-in-file-twice ()
  (gv-tests--in-temp-dir (el elc)
      ((gv-define-setter gv-test-foo (newval cons)
         `(setcar ,cons ,newval))
       (defvar gv-test-pair (cons 1 2))
       (setf (gv-test-foo gv-test-pair) 99)
       (gv-define-setter gv-test-foo (newval cons)
         `(setcdr ,cons ,newval))
       (setf (gv-test-foo gv-test-pair) 42)
       (message "%S" gv-test-pair))
    (with-temp-buffer
      (call-process (concat invocation-directory invocation-name)
                    nil '(t t) nil
                    "-Q" "-batch" "--eval" (prin1-to-string `(byte-compile-file ,el))
                    "-l" elc)
      (should (equal (buffer-string) "(99 . 42)\n")))))

(ert-deftest gv-dont-define-expander-in-file ()
  ;; The expander is defined while we are compiling the file, even
  ;; though it's inside (when nil ...) because the compiler won't
  ;; analyze the conditional.
  :expected-result :failed
  (gv-tests--in-temp-dir (el elc)
      ((when nil (gv-define-setter gv-test-foo (newval cons)
                   `(setcar ,cons ,newval)))
       (defvar gv-test-pair (cons 1 2))
       (setf (gv-test-foo gv-test-pair) 99)
       (message "%d" (car gv-test-pair)))
    (with-temp-buffer
      (call-process (concat invocation-directory invocation-name)
                    nil '(t t) nil
                    "-Q" "-batch" "--eval" (prin1-to-string `(byte-compile-file ,el))
                    "-l" elc)
      (should (equal (buffer-string)
                     "Symbol's function definition is void: \\(setf\\ gv-test-foo\\)\n")))))

(ert-deftest gv-define-expander-in-function ()
  ;; The expander is not defined while we are compiling the file, the
  ;; compiler won't handle gv definitions not at top-level.
  :expected-result :failed
  (gv-tests--in-temp-dir (el elc)
      ((defun foo ()
         (gv-define-setter gv-test-foo (newval cons)
           `(setcar ,cons ,newval))
         t)
       (defvar gv-test-pair (cons 1 2))
       (setf (gv-test-foo gv-test-pair) 99)
       (message "%d" (car gv-test-pair)))
    (with-temp-buffer
      (call-process (concat invocation-directory invocation-name)
                    nil '(t t) nil
                    "-Q" "-batch" "--eval" (prin1-to-string `(byte-compile-file ,el))
                    "-l" elc)
      (should (equal (buffer-string) "99\n")))))

(ert-deftest gv-define-expander-out-of-file ()
  (gv-tests--in-temp-dir (el elc)
      ((gv-define-setter gv-test-foo (newval cons)
         `(setcar ,cons ,newval))
       (defvar gv-test-pair (cons 1 2)))
    (with-temp-buffer
      (call-process (concat invocation-directory invocation-name)
                    nil '(t t) nil
                    "-Q" "-batch" "--eval" (prin1-to-string `(byte-compile-file ,el))
                    "-l" elc
                    "--eval"
                    (prin1-to-string '(progn (setf (gv-test-foo gv-test-pair) 99)
                                             (message "%d" (car gv-test-pair)))))
      (should (equal (buffer-string) "99\n")))))

(ert-deftest gv-dont-define-expander-other-file ()
  (gv-tests--in-temp-dir (el elc)
      ((if nil (gv-define-setter gv-test-foo (newval cons)
                 `(setcar ,cons ,newval)))
       (defvar gv-test-pair (cons 1 2)))
    (with-temp-buffer
      (call-process (concat invocation-directory invocation-name)
                    nil '(t t) nil
                    "-Q" "-batch" "--eval" (prin1-to-string `(byte-compile-file ,el))
                    "-l" elc
                    "--eval"
                    (prin1-to-string '(progn (setf (gv-test-foo gv-test-pair) 99)
                                             (message "%d" (car gv-test-pair)))))
      (should (string-match
               "\\`Symbol.s function definition is void: \\\\(setf\\\\ gv-test-foo\\\\)\n\\'"
               (buffer-string))))))

(ert-deftest gv-setter-edebug ()
  "Check that a setter can be defined and edebugged together with
its getter (Bug#41853)."
  (with-temp-buffer
    (let ((edebug-all-defs t)
          (edebug-initial-mode 'Go-nonstop))
      (dolist (form '((defun gv-setter-edebug-help (b) b)
                      (defun gv-setter-edebug-get (a b)
                        (get a (gv-setter-edebug-help b)))
                      (gv-define-setter gv-setter-edebug-get (x a b)
                        `(setf (get ,a (gv-setter-edebug-help ,b)) ,x))
                      (push 123 (gv-setter-edebug-get 'gv-setter-edebug
                                                      'gv-setter-edebug-prop))))
        (print form (current-buffer)))
      ;; Only check whether evaluation works in general.
      (eval-buffer)))
  (should (equal (get 'gv-setter-edebug 'gv-setter-edebug-prop) '(123))))

(ert-deftest gv-plist-get ()
  (require 'cl-lib)

  ;; Simple setf usage for plist-get.
  (should (equal (let ((target '(:a "a" :b "b" :c "c")))
                   (setf (plist-get target :b) "modify")
                   target)
                 '(:a "a" :b "modify" :c "c")))

  ;; Other function (cl-rotatef) usage for plist-get.
  (should (equal (let ((target '(:a "a" :b "b" :c "c")))
                   (cl-rotatef (plist-get target :b) (plist-get target :c))
                   target)
                 '(:a "a" :b "c" :c "b")))

  ;; Add new key value pair at top of list if setf for missing key.
  (should (equal (let ((target '(:a "a" :b "b" :c "c")))
                   (setf (plist-get target :d) "modify")
                   target)
                 '(:d "modify" :a "a" :b "b" :c "c")))

  ;; Rotate with missing value.
  ;; The value corresponding to the missing key is assumed to be nil.
  (should (equal (let ((target '(:a "a" :b "b" :c "c")))
                   (cl-rotatef (plist-get target :b) (plist-get target :d))
                   target)
                 '(:d "b" :a "a" :b nil :c "c")))

  ;; Simple setf usage for plist-get. (symbol plist)
  (should (equal (let ((target '(a "a" b "b" c "c")))
                   (setf (plist-get target 'b) "modify")
                   target)
                 '(a "a" b "modify" c "c")))

  ;; Other function (cl-rotatef) usage for plist-get. (symbol plist)
  (should (equal (let ((target '(a "a" b "b" c "c")))
                   (cl-rotatef (plist-get target 'b) (plist-get target 'c))
                   target)
                 '(a "a" b "c" c "b"))))

;; `ert-deftest' messes up macroexpansion when the test file itself is
;; compiled (see Bug #24402).

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; gv-tests.el ends here
