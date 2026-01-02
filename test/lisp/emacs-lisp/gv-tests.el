;;; gv-tests.el --- tests for gv.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2026 Free Software Foundation, Inc.

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
(require 'ert-x)
(eval-when-compile (require 'cl-lib))

(cl-defmacro gv-tests--in-temp-dir ((elvar elcvar)
                                    (&rest filebody)
                                    &rest body)
  (declare (indent 2))
  `(ert-with-temp-directory default-directory
     (let ((,elvar "gv-test-deffoo.el")
           (,elcvar "gv-test-deffoo.elc"))
       (with-temp-file ,elvar
         (insert ";; -*- lexical-binding: t; -*-\n")
         (dolist (form ',filebody)
           (pp form (current-buffer))))
       ,@body)))

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
                    "-Q" "-batch"
                    "--eval" (prin1-to-string
                              `(let ((backtrace-on-error-noninteractive nil))
                                 (byte-compile-file ,el)))
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
                    (prin1-to-string
                     '(let ((backtrace-on-error-noninteractive nil))
                        (setf (gv-test-foo gv-test-pair) 99)
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
      ;; Silence "Edebug: foo" messages.
      (let ((inhibit-message t))
        ;; Only check whether evaluation works in general.
        (eval-buffer))))
  (should (equal (get 'gv-setter-edebug 'gv-setter-edebug-prop) '(123))))

(defvar gv-test--special 0)

(ert-deftest gv-incf ()
  (setq gv-test--special 0)
  (should (= (incf gv-test--special) 1))
  (should (= gv-test--special 1))
  (should (= (incf gv-test--special 9) 10))
  (should (= gv-test--special 10))
  (let ((var 0))
    (should (= (incf var) 1))
    (should (= var 1))
    (should (= (incf var 9) 10))
    (should (= var 10)))
  (let ((alist))
    (should (= (incf (alist-get 'a alist 0)) 1))
    (should (= (alist-get 'a alist 0) 1))
    (should (= (incf (alist-get 'a alist 0) 9) 10))
    (should (= (alist-get 'a alist 0) 10))))

(ert-deftest gv-decf ()
  (setq gv-test--special 0)
  (should (= (decf gv-test--special) -1))
  (should (= gv-test--special -1))
  (should (= (decf gv-test--special 9) -10))
  (should (= gv-test--special -10))
  (let ((var 1))
    (should (= (decf var) 0))
    (should (= var 0))
    (should (= (decf var 10) -10))
    (should (= var -10)))
  (let ((alist))
    (should (= (decf (alist-get 'a alist 0)) -1))
    (should (= (alist-get 'a alist 0) -1))
    (should (= (decf (alist-get 'a alist 0) 9) -10))
    (should (= (alist-get 'a alist 0) -10))))

(ert-deftest gv-plist-get ()
  ;; Simple `setf' usage for `plist-get'.
  (let ((target (list :a "a" :b "b" :c "c")))
    (setf (plist-get target :b) "modify")
    (should (equal target '(:a "a" :b "modify" :c "c")))
    (setf (plist-get target ":a" #'string=) "mogrify")
    (should (equal target '(:a "mogrify" :b "modify" :c "c"))))

  ;; Other function (`cl-rotatef') usage for `plist-get'.
  (let ((target (list :a "a" :b "b" :c "c")))
    (cl-rotatef (plist-get target :b) (plist-get target :c))
    (should (equal target '(:a "a" :b "c" :c "b")))
    (cl-rotatef (plist-get target ":a" #'string=)
                (plist-get target ":b" #'string=))
    (should (equal target '(:a "c" :b "a" :c "b"))))

  ;; Add new key value pair at top of list if `setf' for missing key.
  (let ((target (list :a "a" :b "b" :c "c")))
    (setf (plist-get target :d) "modify")
    (should (equal target '(:d "modify" :a "a" :b "b" :c "c")))
    (setf (plist-get target :e #'string=) "mogrify")
    (should (equal target '(:e "mogrify" :d "modify" :a "a" :b "b" :c "c"))))

  ;; Rotate with missing value.
  ;; The value corresponding to the missing key is assumed to be nil.
  (let ((target (list :a "a" :b "b" :c "c")))
    (cl-rotatef (plist-get target :b) (plist-get target :d))
    (should (equal target '(:d "b" :a "a" :b nil :c "c")))
    (cl-rotatef (plist-get target ":e" #'string=)
                (plist-get target ":d" #'string=))
    (should (equal target '(":e" "b" :d nil :a "a" :b nil :c "c")))))

;;; gv-tests.el ends here
