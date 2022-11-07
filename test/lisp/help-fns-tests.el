;;; help-fns-tests.el --- tests for help-fns.el  -*- lexical-binding: t -*-

;; Copyright (C) 2014-2022 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org

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

;;; Code:

(require 'ert)
(require 'help-fns)

(autoload 'help-fns-test--macro "foo" nil nil t)


;;; Several tests for describe-function

(defun help-fns-tests--describe-function (func)
  "Helper function for `describe-function' tests.
FUNC is the function to describe, a symbol.
Return first line of the output of (describe-function-1 FUNC)."
  (let ((string (with-output-to-string
                  (describe-function-1 func))))
    (string-match "\\(.+\\)\n" string)
    (match-string-no-properties 1 string)))

(ert-deftest help-fns-test-bug17410 ()
  "Test for https://debbugs.gnu.org/17410 ."
  (let ((regexp "autoloaded Lisp macro")
        (result (help-fns-tests--describe-function 'help-fns-test--macro)))
    (should (string-match regexp result))))

(ert-deftest help-fns-test-built-in ()
  (let ((regexp "a built-in function in .C source code")
        (result (help-fns-tests--describe-function 'mapcar)))
    (should (string-match regexp result))))

(ert-deftest help-fns-test-interactive-built-in ()
  (let ((regexp "an interactive built-in function in .C source code")
        (result (help-fns-tests--describe-function 're-search-forward)))
    (should (string-match regexp result))))

(ert-deftest help-fns-test-lisp-macro ()
  (let ((regexp "a Lisp macro in .+subr\\.el")
        (result (help-fns-tests--describe-function 'when)))
    (should (string-match regexp result))))

(ert-deftest help-fns-test-lisp-defun ()
  (let ((regexp (if (featurep 'native-compile)
                    "a native-compiled Lisp function in .+subr\\.el"
                  "a byte-compiled Lisp function in .+subr\\.el"))
        (result (help-fns-tests--describe-function 'last)))
    (should (string-match regexp result))))

(ert-deftest help-fns-test-lisp-defsubst ()
  (let ((regexp "a byte-compiled Lisp function in .+subr\\.el")
        (result (help-fns-tests--describe-function 'posn-window)))
    (should (string-match regexp result))))

(ert-deftest help-fns-test-alias-to-defun ()
  (let ((regexp "an alias for .set-file-modes. in .+subr\\.el")
        (result (help-fns-tests--describe-function 'chmod)))
    (should (string-match regexp result))))

(ert-deftest help-fns-test-bug23887 ()
  "Test for https://debbugs.gnu.org/23887 ."
  (let ((regexp "an alias for .re-search-forward. in .+subr\\.el")
        (result (help-fns-tests--describe-function 'search-forward-regexp)))
    (should (string-match regexp result))))

(ert-deftest help-fns-test-dangling-alias ()
  "Make sure we don't burp on bogus aliases."
  (let ((f (make-symbol "bogus-alias")))
    (define-obsolete-function-alias f 'help-fns-test--undefined-function "past")
    (describe-symbol f)))

;;; Test describe-function over functions with funny names
(defun abc\\\[universal-argument\]b\`c\'d\\e\"f (x)
  "A function with a funny name.

\(fn XYZZY)"
  x)

(defun defgh\\\[universal-argument\]b\`c\'d\\e\"f (x)
  "Another function with a funny name."
  x)

(ert-deftest help-fns-test-funny-names ()
  "Test for help with functions with funny names."
  (describe-function 'abc\\\[universal-argument\]b\`c\'d\\e\"f)
  (with-current-buffer "*Help*"
    (goto-char (point-min))
    (should (search-forward
             "(abc\\\\\\[universal-argument\\]b\\`c\\'d\\\\e\\\"f XYZZY)")))
  (describe-function 'defgh\\\[universal-argument\]b\`c\'d\\e\"f)
  (with-current-buffer "*Help*"
    (goto-char (point-min))
    (should (search-forward
             "(defgh\\\\\\[universal-argument\\]b\\`c\\'d\\\\e\\\"f X)"))))


;;; Test for describe-symbol
(ert-deftest help-fns-test-describe-symbol ()
  "Test the `describe-symbol' function."
  ;; 'describe-symbol' would originally signal an error for
  ;; 'font-lock-comment-face'.
  (describe-symbol 'font-lock-comment-face)
  (with-current-buffer "*Help*"
    (should (> (point-max) 1))
    (goto-char (point-min))
    (should (looking-at "^font-lock-comment-face is "))))

(defvar foo-test-map)
(defvar help-fns-test--describe-keymap-foo)


;;; Tests for describe-keymap
(ert-deftest help-fns-test-find-keymap-name ()
  (should (equal (help-fns-find-keymap-name lisp-mode-map) 'lisp-mode-map))
  ;; Follow aliasing.
  (unwind-protect
      (progn
        (defvaralias 'foo-test-map 'lisp-mode-map)
        (should (equal (help-fns-find-keymap-name foo-test-map) 'lisp-mode-map)))
    (makunbound 'foo-test-map)))

(ert-deftest help-fns-test-describe-keymap/symbol ()
  (describe-keymap 'minibuffer-local-must-match-map)
  (with-current-buffer "*Help*"
    (should (looking-at "^minibuffer-local-must-match-map is"))))

(ert-deftest help-fns-test-describe-keymap/value ()
  (describe-keymap minibuffer-local-must-match-map)
  (with-current-buffer "*Help*"
    (should (looking-at "^key"))))

(ert-deftest help-fns-test-describe-keymap/not-keymap ()
  (should-error (describe-keymap nil))
  (should-error (describe-keymap emacs-version)))

(ert-deftest help-fns-test-describe-keymap/let-bound ()
  (let ((foobar minibuffer-local-must-match-map))
    (describe-keymap foobar)
    (with-current-buffer "*Help*"
      (should (looking-at "^key")))))

(ert-deftest help-fns-test-describe-keymap/dynamically-bound-no-file ()
  (setq help-fns-test--describe-keymap-foo minibuffer-local-must-match-map)
  (describe-keymap 'help-fns-test--describe-keymap-foo)
  (with-current-buffer "*Help*"
    (should (looking-at "^help-fns-test--describe-keymap-foo is"))))

;;; Tests for find-lisp-object-file-name
(ert-deftest help-fns-test-bug24697-function-search ()
  (should-not (find-lisp-object-file-name 'tab-width 1)))

(ert-deftest help-fns-test-bug24697-non-internal-variable ()
  (let ((help-fns--test-var (make-symbol "help-fns--test-var")))
    ;; simulate an internal variable
    (put help-fns--test-var 'variable-documentation 1)
    (should-not (find-lisp-object-file-name help-fns--test-var 'defface))
    (should-not (find-lisp-object-file-name help-fns--test-var 1))))

;;; help-fns-tests.el ends here
