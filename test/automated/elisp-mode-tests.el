;;; elisp-mode-tests.el --- Tests for emacs-lisp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Dmitry Gutov <dgutov@yandex.ru>

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'xref)

;;; Completion

(defun elisp--test-completions ()
  (let ((data (elisp-completion-at-point)))
    (all-completions (buffer-substring (nth 0 data) (nth 1 data))
                     (nth 2 data)
                     (plist-get (nthcdr 3 data) :predicate))))

(ert-deftest elisp-completes-functions ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(ba")
    (let ((comps (elisp--test-completions)))
      (should (member "backup-buffer" comps))
      (should-not (member "backup-inhibited" comps)))))

(ert-deftest elisp-completes-variables ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo ba")
    (let ((comps (elisp--test-completions)))
      (should (member "backup-inhibited" comps))
      (should-not (member "backup-buffer" comps)))))

(ert-deftest elisp-completes-anything-quoted ()
  (dolist (text '("`(foo ba" "(foo 'ba"
                  "`(,foo ba" "`,(foo `ba"
                  "'(foo (ba"))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert text)
      (let ((comps (elisp--test-completions)))
        (should (member "backup-inhibited" comps))
        (should (member "backup-buffer" comps))
        (should (member "backup" comps))))))

(ert-deftest elisp-completes-variables-unquoted ()
  (dolist (text '("`(foo ,ba" "`(,(foo ba" "`(,ba"))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert text)
      (let ((comps (elisp--test-completions)))
        (should (member "backup-inhibited" comps))
        (should-not (member "backup-buffer" comps))))))

(ert-deftest elisp-completes-functions-in-special-macros ()
  (dolist (text '("(declare-function ba" "(cl-callf2 ba"))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert text)
      (let ((comps (elisp--test-completions)))
        (should (member "backup-buffer" comps))
        (should-not (member "backup-inhibited" comps))))))

(ert-deftest elisp-completes-local-variables ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(let ((bar 1) baz) (foo ba")
    (let ((comps (elisp--test-completions)))
      (should (member "backup-inhibited" comps))
      (should (member "bar" comps))
      (should (member "baz" comps)))))

(ert-deftest elisp-completest-variables-in-let-bindings ()
  (dolist (text '("(let (ba" "(let* ((ba"))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert text)
      (let ((comps (elisp--test-completions)))
        (should (member "backup-inhibited" comps))
        (should-not (member "backup-buffer" comps))))))

(ert-deftest elisp-completes-functions-after-let-bindings ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(let ((bar 1) (baz 2)) (ba")
    (let ((comps (elisp--test-completions)))
      (should (member "backup-buffer" comps))
      (should-not (member "backup-inhibited" comps)))))

;;; Navigation

(ert-deftest elisp-xref-finds-both-function-and-variable ()
  ;; "system-name" is both: a variable and a function
  (let ((defs (elisp-xref-find 'definitions "system-name")))
    (should (= (length defs) 2))
    (should (string= (xref--xref-description (nth 0 defs))
                     "(defun system-name)"))
    (should (string= (xref--xref-description (nth 1 defs))
                     "(defvar system-name)")))
  ;; It's a minor mode, but the variable is defined in buffer.c
  (let ((defs (elisp-xref-find 'definitions "abbrev-mode")))
    (should (= (length defs) 2))))

(ert-deftest elisp-xref-finds-only-function-for-minor-mode ()
  ;; Both variable and function are defined in the same place.
  (let ((defs (elisp-xref-find 'definitions "visible-mode")))
    (should (= (length defs) 1))
    (should (string= (xref--xref-description (nth 0 defs))
                     "(defun visible-mode)"))))

(provide 'elisp-mode-tests)
;;; elisp-mode-tests.el ends here
