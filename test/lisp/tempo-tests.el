;;; tempo-tests.el --- Test suite for tempo.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Federico Tedin <federicotedin@gmail.com>
;; Keywords: abbrev

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

;;; Code:

(require 'tempo)
(eval-when-compile (require 'cl-lib))

(ert-deftest tempo-string-element-test ()
  "Test a template containing a string element."
  (with-temp-buffer
    (tempo-define-template "test" '("GNU Emacs Tempo test"))
    (tempo-insert-template 'tempo-template-test nil)
    (should (equal (buffer-string) "GNU Emacs Tempo test"))))

(ert-deftest tempo-p-bare-element-test ()
  "Test a template containing a bare `p' element."
  (with-temp-buffer
    (tempo-define-template "test" '("abcde" p))
    (tempo-insert-template 'tempo-template-test nil)
    (tempo-forward-mark)
    (should (equal (point) 6))))

(ert-deftest tempo-r-bare-element-test ()
  "Test a template containing a bare `r' element."
  (with-temp-buffer
    (tempo-define-template "test" '("abcde" r "ghijk"))
    (insert "F")
    (set-mark (point))
    (goto-char (point-min))
    (tempo-insert-template 'tempo-template-test t)
    (should (equal (buffer-string) "abcdeFghijk"))))

(ert-deftest tempo-p-element-test ()
  "Testing template containing a `p' (prompt) element."
  (with-temp-buffer
    (tempo-define-template "test" '("hello " (p ">")))
    (let ((tempo-interactive t))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "world")))
        (tempo-insert-template 'tempo-template-test nil))
      (should (equal (buffer-string) "hello world")))))

(ert-deftest tempo-P-element-test ()
  "Testing template containing a `P' (prompt) element."
  (with-temp-buffer
    (tempo-define-template "test" '("hello " (P ">")))
    ;; By default, `tempo-interactive' is nil, `P' should ignore this.
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "world")))
      (tempo-insert-template 'tempo-template-test nil))
    (should (equal (buffer-string) "hello world"))))

(ert-deftest tempo-r-element-test ()
  "Testing template containing an `r' (with prompt) element."
  (with-temp-buffer
    (tempo-define-template "test" '("abcde" (r ">") "ghijk"))
    (let ((tempo-interactive t))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "F")))
        (tempo-insert-template 'tempo-template-test nil))
      (should (equal (buffer-string) "abcdeFghijk")))))

(ert-deftest tempo-s-element-test ()
  "Testing template containing an `s' element."
  (with-temp-buffer
    (tempo-define-template "test" '("hello " (p ">" P1) " " (s P1)))
    (let ((tempo-interactive t))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "world!")))
        (tempo-insert-template 'tempo-template-test nil))
      (should (equal (buffer-string) "hello world! world!")))))

(ert-deftest tempo-&-element-test ()
  "Testing template containing an `&' element."
  (tempo-define-template "test" '(& "test"))
  (with-temp-buffer
    (insert "  ")
    (tempo-insert-template 'tempo-template-test nil)
    (should (equal (buffer-string) "  test")))
  (with-temp-buffer
    (insert "hello")
    (tempo-insert-template 'tempo-template-test nil)
    (should (equal (buffer-string) "hello\ntest"))))

(ert-deftest tempo-%-element-test ()
  "Testing template containing an `%' element."
  (tempo-define-template "test" '("test" %))
  (with-temp-buffer
    (tempo-insert-template 'tempo-template-test nil)
    (should (equal (buffer-string) "test")))
  (with-temp-buffer
    (insert "hello")
    (goto-char (point-min))
    (tempo-insert-template 'tempo-template-test nil)
    (should (equal (buffer-string) "test\nhello"))))

(ert-deftest tempo-n-element-test ()
  "Testing template containing an `n' element."
  (tempo-define-template "test" '("test" n "test"))
  (with-temp-buffer
    (tempo-insert-template 'tempo-template-test nil)
    (should (equal (buffer-string) "test\ntest"))))

(ert-deftest tempo-n>-element-test ()
  "Testing template containing an `n>' element."
  (tempo-define-template "test" '("(progn" n> "(list 1 2 3))"))
  (with-temp-buffer
    (emacs-lisp-mode)
    (tempo-insert-template 'tempo-template-test nil)
    ;; Tempo should have inserted two spaces before (list 1 2 3)
    (should (equal (buffer-string) "(progn\n  (list 1 2 3))"))))

(ert-deftest tempo->-element-test ()
  "Testing template containing a `>' element."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(progn\n)")
    (backward-char)
    (tempo-define-template "test" '("(list 1 2 3)" >))
    (tempo-insert-template 'tempo-template-test nil)
    ;; Tempo should have inserted two spaces before (list 1 2 3)
    (should (equal (buffer-string) "(progn\n  (list 1 2 3))"))))

(ert-deftest tempo-r>-bare-element-test ()
  "Testing template containing a bare `r>' element."
  (with-temp-buffer
    (tempo-define-template "test" '("(progn" n r> ")"))
    (emacs-lisp-mode)
    (insert "(list 1 2 3)")
    (set-mark (point))
    (goto-char (point-min))
    (tempo-insert-template 'tempo-template-test t)
    ;; Tempo should have inserted two spaces before (list 1 2 3)
    (should (equal (buffer-string) "(progn\n  (list 1 2 3))"))))

(ert-deftest tempo-r>-element-test ()
  "Testing template containing an `r>' (with prompt) element."
  (tempo-define-template "test" '("(progn" n (r> ":") ")"))
  (with-temp-buffer
    ;; Test on-region use
    (emacs-lisp-mode)
    (insert "(list 1 2 3)")
    (set-mark (point))
    (goto-char (point-min))
    (tempo-insert-template 'tempo-template-test t)
    (should (equal (buffer-string) "(progn\n  (list 1 2 3))")))
  (with-temp-buffer
    ;; Test interactive use
    (emacs-lisp-mode)
    (let ((tempo-interactive t))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "  (list 1 2 3)")))
        (tempo-insert-template 'tempo-template-test nil))
      (should (equal (buffer-string) "(progn\n  (list 1 2 3))")))))

(ert-deftest tempo-o-element-test ()
  "Testing template containing an `o' element."
  (with-temp-buffer
    (tempo-define-template "test" '("test" o))
    (insert "hello")
    (goto-char (point-min))
    (tempo-insert-template 'tempo-template-test nil)
    (should (equal (buffer-string) "test\nhello"))
    (should (equal (point) 5))))

(ert-deftest tempo-nil-element-test ()
  "Testing template with nil elements."
  (with-temp-buffer
    (tempo-define-template "test" '("Hello," nil " World!"))
    (tempo-insert-template 'tempo-template-test nil)
    (should (equal (buffer-string) "Hello, World!"))))

(ert-deftest tempo-eval-element-test ()
  "Testing template with Emacs Lisp expressions."
  (with-temp-buffer
    (tempo-define-template "test" '((int-to-string (+ 1 1)) "=" (concat "1" "+1")))
    (tempo-insert-template 'tempo-template-test nil)
    (should (equal (buffer-string) "2=1+1"))))

(ert-deftest tempo-l-element-test ()
  "Testing template containing an `l' element."
  (with-temp-buffer
    (tempo-define-template "test" '("list: " (l "1, " "2, " (int-to-string (+ 1 2)))))
    (tempo-insert-template 'tempo-template-test nil)
    (should (equal (buffer-string) "list: 1, 2, 3"))))

(ert-deftest tempo-tempo-user-elements-test ()
  "Testing a template with elements for `tempo-user-elements'."
  (with-temp-buffer
    (make-local-variable 'tempo-user-elements)
    (add-to-list 'tempo-user-elements (lambda (x) (int-to-string (* x x))))
    (tempo-define-template "test" '(1 " " 2 " " 3 " " 4))
    (tempo-insert-template 'tempo-template-test nil)
    (should (equal (buffer-string) "1 4 9 16"))))

(ert-deftest tempo-expand-tag-test ()
  "Testing expansion of a template with a tag."
  (with-temp-buffer
    (tempo-define-template "test" '("Hello, World!") "hello")
    (insert "hello")
    (tempo-complete-tag)
    (should (equal (buffer-string) "Hello, World!"))))

(ert-deftest tempo-expand-partial-tag-test ()
  "Testing expansion of a template with a tag, with a partial match."
  (with-temp-buffer
    (tempo-define-template "test" '("Hello, World!") "hello")
    (insert "hel")
    (tempo-complete-tag)
    (should (equal (buffer-string) "Hello, World!"))))

(provide 'tempo-tests)
;;; tempo-tests.el ends here
