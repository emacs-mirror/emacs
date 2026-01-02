;;; checkdoc-tests.el --- unit tests for checkdoc.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2026 Free Software Foundation, Inc.

;; Author: Philipp Stephani <phst@google.com>

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

;;; Commentary:

;; Unit tests for lisp/emacs-lisp/checkdoc.el.

;;; Code:

(require 'checkdoc)

(require 'elisp-mode)
(require 'ert)

(ert-deftest checkdoc-tests--bug-24998 ()
  "Checks that Bug#24998 is fixed."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun foo())")
    (should-error (checkdoc-defun) :type 'user-error)))

(ert-deftest checkdoc-docstring-avoid-false-positive-ok ()
  "Check that Bug#68002 is fixed."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defvar org-element--cache-interrupt-C-g-count 0
  \"Current number of `org-element--cache-sync' calls.
See `org-element--cache-interrupt-C-g'.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-cl-defmethod-ok ()
  "Checkdoc should be happy with a simple correct cl-defmethod."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(cl-defmethod foo (a) \"Return A.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-cl-defmethod-with-types-ok ()
  "Checkdoc should be happy with a cl-defmethod using types."
  (with-temp-buffer
    (emacs-lisp-mode)
    ;; this method matches if A is the symbol `smthg' and if b is a list:
    (insert "(cl-defmethod foo ((a (eql 'smthg)) (b list)) \"Return A+B.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-cl-defmethod-qualified-ok ()
  "Checkdoc should be happy with a `cl-defmethod' using qualifiers."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(cl-defmethod test :around ((a (eql 'smthg))) \"Return A.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-cl-defmethod-with-extra-qualifier-ok ()
  "Checkdoc should be happy with a :extra qualified `cl-defmethod'."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(cl-defmethod foo :extra \"foo\" ((a (eql 'smthg))) \"Return A.\")")
    (checkdoc-defun))

  (with-temp-buffer
    (emacs-lisp-mode)
    (insert
     "(cl-defmethod foo :extra \"foo\" :after ((a (eql 'smthg))) \"Return A.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-cl-defmethod-with-extra-qualifier-and-nil-args-ok ()
  "Checkdoc should be happy with a 0-arity :extra qualified `cl-defmethod'."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(cl-defmethod foo :extra \"foo\" () \"Return A.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-cl-defun-with-key-ok ()
  "Checkdoc should be happy with a cl-defun using &key."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(cl-defun foo (&key a (b 27)) \"Return :A+:B.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-cl-defun-with-allow-other-keys-ok ()
  "Checkdoc should be happy with a cl-defun using &allow-other-keys."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(cl-defun foo (&key a &allow-other-keys) \"Return :A.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-cl-defun-with-default-optional-value-ok ()
  "Checkdoc should be happy with a cl-defun using default values for optional args."
  (with-temp-buffer
    (emacs-lisp-mode)
    ;; B is optional and equals 1+a if not provided. HAS-BS is non-nil
    ;; if B was provided in the call:
    (insert "(cl-defun foo (a &optional (b (1+ a) has-bs)) \"Return A + B.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-cl-defun-with-destructuring-ok ()
  "Checkdoc should be happy with a cl-defun destructuring its arguments."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(cl-defun foo ((a b &optional c) d) \"Return A+B+C+D.\")")
    (checkdoc-defun)))

(ert-deftest checkdoc-tests--next-docstring ()
  "Checks that the one-argument form of `defvar' works.
See the comments in Bug#24998."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defvar foo)
\(defvar foo bar \"baz\")
\(require 'foo)")
    (goto-char (point-min))
    (should (checkdoc-next-docstring))
    (should (looking-at-p "\"baz\")"))
    (should-not (checkdoc-next-docstring))))

(defun checkdoc-tests--abbrev-test (buffer-contents goto-string)
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert buffer-contents)
    (goto-char (point-min))
    (re-search-forward goto-string)
    (checkdoc-in-abbreviation-p (point))))

(ert-deftest checkdoc-tests-in-abbreviation-p/basic-case ()
  (should (checkdoc-tests--abbrev-test "foo bar e.g. baz" "e.g"))
  (should (checkdoc-tests--abbrev-test "behavior/errors etc. that" "etc"))
  (should (checkdoc-tests--abbrev-test "foo vs. bar" "vs"))
  (should (checkdoc-tests--abbrev-test "spy a.k.a. spy" "a.k.a")))

(ert-deftest checkdoc-tests-in-abbreviation-p/with-parens ()
  (should (checkdoc-tests--abbrev-test "foo bar (e.g. baz)" "e.g")))

(ert-deftest checkdoc-tests-in-abbreviation-p/with-escaped-parens ()
  (should (checkdoc-tests--abbrev-test "foo\n\\(e.g. baz)" "e.g")))

(ert-deftest checkdoc-tests-in-abbreviation-p/single-char ()
  (should (checkdoc-tests--abbrev-test "a. foo bar" "a")))

(ert-deftest checkdoc-tests-in-abbreviation-p/with-em-dash ()
  (should (checkdoc-tests--abbrev-test "foo bar baz---e.g." "e.g")))

(ert-deftest checkdoc-tests-in-abbreviation-p/incorrect-abbreviation ()
  (should-not (checkdoc-tests--abbrev-test "foo bar a.b.c." "a.b.c")))

(defun checkdoc-test-error-format-is-good (msg &optional reverse literal)
  (with-temp-buffer
    (erase-buffer)
    (emacs-lisp-mode)
    (let ((standard-output (current-buffer)))
      (if literal
          (print (format "(error \"%s\")" msg))
        (prin1 `(error ,msg))))
    (goto-char (length "(error \""))
    (if reverse
         (should (checkdoc--error-bad-format-p))
       (should-not (checkdoc--error-bad-format-p)))))

(defun checkdoc-test-error-format-is-bad (msg &optional literal)
  (checkdoc-test-error-format-is-good msg t literal))

(ert-deftest checkdoc-tests-error-message-bad-format-p ()
  (checkdoc-test-error-format-is-good "Foo")
  (checkdoc-test-error-format-is-good "Foo: bar baz")
  (checkdoc-test-error-format-is-good "some-symbol: Foo")
  (checkdoc-test-error-format-is-good "`some-symbol' foo bar")
  (checkdoc-test-error-format-is-good "%sfoo")
  (checkdoc-test-error-format-is-good "avl-tree-enter:\\
 Updated data does not match existing data" nil 'literal))

(ert-deftest checkdoc-tests-error-message-bad-format-p/defined-symbols ()
  (defvar checkdoc-tests--var-symbol nil)
  (checkdoc-test-error-format-is-good "checkdoc-tests--var-symbol foo bar baz")
  (defun checkdoc-tests--fun-symbol ())
  (checkdoc-test-error-format-is-good "checkdoc-tests--fun-symbol foo bar baz"))

(ert-deftest checkdoc-tests-error-message-bad-format-p/not-capitalized ()
  (checkdoc-test-error-format-is-bad "foo")
  (checkdoc-test-error-format-is-bad "some-symbol: foo")
  (checkdoc-test-error-format-is-bad "avl-tree-enter:\
 updated data does not match existing data"))

(ert-deftest checkdoc-tests-fix-y-or-n-p ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((standard-output (current-buffer))
          (checkdoc-autofix-flag 'automatic))
      (prin1 '(y-or-n-p "foo"))         ; "foo"
      (goto-char (length "(y-or-n-p "))
      (checkdoc--fix-y-or-n-p)
      (should (equal (buffer-string) "(y-or-n-p \"foo?\")")))))

(ert-deftest checkdoc-tests-fix-y-or-n-p/no-change ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((standard-output (current-buffer))
          (checkdoc-autofix-flag 'automatic))
      (prin1 '(y-or-n-p "foo?"))        ; "foo?"
      (goto-char (length "(y-or-n-p "))
      (checkdoc--fix-y-or-n-p)
      (should (equal (buffer-string) "(y-or-n-p \"foo?\")")))))

(ert-deftest checkdoc-tests-fix-y-or-n-p/with-space ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((standard-output (current-buffer))
          (checkdoc-autofix-flag 'automatic))
      (prin1 '(y-or-n-p "foo? "))       ; "foo? "
      (goto-char (length "(y-or-n-p "))
      (checkdoc--fix-y-or-n-p)
      (should (equal (buffer-string) "(y-or-n-p \"foo? \")")))))

;;; checkdoc-tests.el ends here
