;;; find-func-tests.el --- Unit tests for find-func.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
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

(require 'ert-x)                        ;For `ert-simulate-keys'.
(require 'find-func)

(ert-deftest find-func-tests--library-completion () ;bug#43393
  ;; FIXME: How can we make this work in batch (see also
  ;; `mule-cmds--test-universal-coding-system-argument')?
  ;; (skip-when noninteractive)
  ;; Check that `partial-completion' works when completing library names.
  (should (equal "org/org"
                 (ert-simulate-keys
                     (kbd "o / o r g TAB RET")
                   (read-library-name))))
  ;; Check that absolute file names also work.
  (should (equal (expand-file-name "nxml/" data-directory)
                 (ert-simulate-keys
                     (concat data-directory (kbd "n x / TAB RET"))
                   (read-library-name)))))

(ert-deftest find-func-tests--locate-symbols ()
  ;; C source files are unavailable when testing on Android.
  (skip-when (featurep 'android))
  (should (cdr
           (find-function-search-for-symbol
            #'goto-line nil "simple")))
  (should (cdr
           (find-function-search-for-symbol
            'minibuffer-history 'defvar "simple")))
  (should (cdr
           (find-function-search-for-symbol
            'with-current-buffer nil "subr")))
  (should (cdr
           (find-function-search-for-symbol
            'font-lock-warning-face 'defface "font-lock")))
  (should-not (cdr
               (find-function-search-for-symbol
                'wrong-variable 'defvar "simple")))
  (should-not (cdr
               (find-function-search-for-symbol
                'wrong-function nil "simple")))
  (should (cdr (find-function-noselect #'goto-line)))
  (should (cdr (find-function-noselect #'goto-char)))
  ;; Setting LISP-ONLY and passing a primitive should error.
  (should-error (find-function-noselect #'goto-char t))
  (should-error (find-function-noselect 'wrong-function)))

(defun test-locate-helper (func &optional expected-result)
  "Assert on the result of `find-function-library' for FUNC.
EXPECTED-RESULT is an alist (FUNC . LIBRARY) with the
expected function symbol and function library, respectively."
  (cl-destructuring-bind (orig-function . library)
      (find-function-library func)
    (cl-destructuring-bind (expected-func . expected-library)
        expected-result
      (should (eq orig-function expected-func))
      (should (and
               (not (string-empty-p expected-library))
               (string-match-p expected-library library))))))

(ert-deftest find-func-tests--locate-library ()
  (test-locate-helper #'goto-line '(goto-line . "simple"))
  (test-locate-helper #'forward-char '(forward-char . "cmds.c"))
  (should-error (test-locate-helper 'wrong-function)))

(ert-deftest find-func-tests--locate-advised-symbols ()
  (defun my-message ()
    (message "Hello!"))
  (advice-add #'mark-sexp :around 'my-message)
  (test-locate-helper #'mark-sexp '(mark-sexp . "lisp"))
  (advice-remove #'mark-sexp 'my-message))

(ert-deftest find-func-tests--find-library-verbose ()
  (unwind-protect
      (progn
        (advice-add 'dired :before #'ignore)
        ;; bug#41104
        (should (equal (find-function-library #'dired) '(dired . "dired"))))
    (advice-remove 'dired #'ignore))

  (find-function-library #'join-line nil t)
  (with-current-buffer "*Messages*"
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward "\n")
      (should (string-match-p
               ".join-line. is an alias for .delete-indentation."
               (buffer-substring (pos-bol) (point)))))))

;; Avoid a byte-compilation warning that may confuse people reading
;; the result of the following test.
(declare-function compilation--message->loc nil "compile")

(ert-deftest find-func-tests--locate-macro-generated-symbols () ;bug#45443
  (let ((trusted-content
         (list (abbreviate-file-name (find-library-name "compile"))
               (abbreviate-file-name (find-library-name "cc-mode")))))
    (should (cdr (find-function-search-for-symbol
                  #'compilation--message->loc nil "compile")))
    (should (cdr (find-function-search-for-symbol
                  'c-mode-hook 'defvar "cc-mode")))))

(provide 'find-func-tests)
;;; find-func-tests.el ends here
