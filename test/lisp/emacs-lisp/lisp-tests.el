;;; lisp-tests.el --- Test Lisp editing commands     -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2017 Free Software Foundation, Inc.

;; Author: Aaron S. Hawley <aaron.s.hawley@gmail.com>
;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Author: Daniel Colascione <dancol@dancol.org>
;; Keywords: internal

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

;;; Commentary:

;; Testing of `forward-sexp' and related functions.

;;; Code:

(require 'ert)
(require 'python)
(require 'cl-lib)

(ert-deftest lisp-forward-sexp-1-empty-parens ()
  "Test basics of \\[forward-sexp]."
  (with-temp-buffer
    (insert "()")
    (goto-char (point-min))
    (should (null
      (forward-sexp 1)))))

(ert-deftest lisp-forward-sexp-1-error-mismatch ()
  "Test basics of \\[forward-sexp]."
  (with-temp-buffer
    (insert "(")
    (goto-char (point-min))
    (should-error
      (forward-sexp 1))))

(ert-deftest lisp-backward-sexp-1-empty-parens ()
  "Test basics of \\[backward-sexp]."
  (with-temp-buffer
    (insert "()")
    (should (null
      (forward-sexp -1)))))

(ert-deftest lisp-backward-sexp-1-error-mismatch ()
  "Test mismatched parens with \\[backward-sexp]."
  (with-temp-buffer
    (insert "(")
    (should-error
      (forward-sexp -1))))

(ert-deftest lisp-forward-sexp-1-eobp ()
  "Test \\[forward-sexp] at `eobp'."
  (with-temp-buffer
    (insert "()")
    (should (null ;; (should-error ;; No, per #13994
      (forward-sexp 1)))))

(ert-deftest lisp-backward-sexp-1-eobp ()
  "Test \\[backward-sexp] at `bobp'."
  (with-temp-buffer
    (insert "()")
    (goto-char (point-min))
    (should (null ;; (should-error ;; No, per #13994
     (forward-sexp -1)))))

(ert-deftest lisp-forward-sexp-2-eobp ()
  "Test \\[forward-sexp] beyond `eobp'."
  (with-temp-buffer
    (insert "()")
    (goto-char (point-min))
    (should (null ;; (should-error ;; No, per #13994
     (forward-sexp 2)))
    (should (eobp))))

(ert-deftest lisp-backward-sexp-2-bobp ()
  "Test \\[backward-sexp] beyond `bobp'."
  (with-temp-buffer
    (insert "()")
    (should (null ;; (should-error ;; No, per #13994
     (forward-sexp -2)))
    (should (bobp))))

(ert-deftest lisp-forward-sexp-2-eobp-and-subsequent ()
  "Test \\[forward-sexp] beyond `eobp' and again."
  (with-temp-buffer
    (insert "()")
    (goto-char (point-min))
    (should (null ;; (should-error ;; No, per #13994
     (forward-sexp 2)))
    (should (eobp))
    (should (null ;; (should-error ;; No, per #13994
     (forward-sexp 1)))))

(ert-deftest lisp-backward-sexp-2-bobp-and-subsequent ()
  "Test \\[backward-sexp] ahead of `bobp' and again."
  (with-temp-buffer
    (insert "()")
    (should (null ;; (should-error ;; No, per #13994
     (forward-sexp -2)))
    (should (bobp))
    (should (null ;; (should-error ;; No, per #13994
     (forward-sexp -1)))))

(ert-deftest lisp-delete-pair-parens ()
  "Test \\[delete-pair] with parens."
  (with-temp-buffer
    (insert "(foo)")
    (goto-char (point-min))
    (delete-pair)
    (should (string-equal "foo" (buffer-string)))))

(ert-deftest lisp-delete-pair-quotation-marks ()
  "Test \\[delete-pair] with quotation marks."
  (with-temp-buffer
    (insert "\"foo\"")
    (goto-char (point-min))
    (delete-pair)
    (should (string-equal "foo" (buffer-string)))))

(ert-deftest lisp-delete-pair-quotes-in-text-mode ()
  "Test \\[delete-pair] against string in Text Mode for #15014."
  (with-temp-buffer
    (text-mode)
    (insert "\"foo\"")
    (goto-char (point-min))
    (delete-pair)
    (should (string-equal "fo\"" (buffer-string)))))

(ert-deftest lisp-delete-pair-quotes-text-mode-syntax-table ()
  "Test \\[delete-pair] with modified Text Mode syntax for #15014."
  (with-temp-buffer
    (text-mode)
    (let ((st (copy-syntax-table text-mode-syntax-table)))
      (with-syntax-table st
        ;; (modify-syntax-entry ?\" "." text-mode-syntax-table)
        (modify-syntax-entry ?\" "$" st)
        (insert "\"foo\"")
        (goto-char (point-min))
        (delete-pair)
        (should (string-equal "foo" (buffer-string)))))))

(ert-deftest lisp-forward-sexp-elisp-inside-symbol ()
  "Test \\[forward-sexp] on symbol in Emacs Lisp Mode for #20492."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "hide-ifdef-env ")
    (insert (concat (number-sequence 32 126)))
    (goto-char (point-min))
    (re-search-forward "hide" nil t) ;; (forward-char 4)
    (should (looking-at "-"))
    (forward-sexp)
    (should (looking-at " "))))

(ert-deftest lisp-forward-sexp-elisp-quoted-symbol ()
  "Test \\[forward-sexp] on symbol in Emacs Lisp Mode for #20492."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "`hide-ifdef-env'.")
    (goto-char (point-min))
    (re-search-forward "hide" nil t) ;; (forward-char 5)
    (should (= ?- (char-after)))
    (forward-sexp)
    (should (= ?. (char-before)))))

(ert-deftest lisp-forward-sexp-python-triple-quoted-string ()
  "Test \\[forward-sexp] on Python doc strings for #11321."
  (with-temp-buffer
    (insert "\"\"\"Triple-quoted string\"\"\"")
    (goto-char (point-min))
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (forward-sexp)
    (should (eobp))))

(ert-deftest lisp-forward-sexp-python-triple-quotes-string ()
  "Test \\[forward-sexp] on Python doc strings for #11321."
  (with-temp-buffer
    (insert "'''Triple-quoted string'''")
    (goto-char (point-min))
    (let ((python-indent-guess-indent-offset nil))
      (python-mode))
    (forward-sexp)
    (should (eobp))))

(ert-deftest lisp-forward-sexp-emacs-lisp-semi-char-error ()
  "Test \\[forward-sexp] on expression with unquoted semicolon per #4030."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(insert ?;)")
    (goto-char (point-min))
    (should-error (forward-sexp)))) ;; FIXME: Shouldn't be an error.

(ert-deftest lisp-forward-sexp-emacs-lisp-quote-char ()
  "Test \\[forward-sexp] on expression with unquoted quote per #4030."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(insert ?\")")
    (goto-char (point-min))
    (should-error (forward-sexp)))) ;; FIXME: Shouldn't be an error.

;; Test some core Elisp rules.
(ert-deftest core-elisp-tests-1-defvar-in-let ()
  "Test some core Elisp rules."
  (with-temp-buffer
    ;; Check that when defvar is run within a let-binding, the toplevel default
    ;; is properly initialized.
    (should (equal (list (let ((c-e-x 1)) (defvar c-e-x 2) c-e-x) c-e-x)
                   '(1 2)))
    (should (equal (list (let ((c-e-x 1))
                           (defcustom c-e-x 2 "doc" :group 'blah :type 'integer) c-e-x)
                         c-e-x)
                   '(1 2)))))

(ert-deftest core-elisp-tests-2-window-configurations ()
  "Test properties of window-configurations."
  (let ((wc (current-window-configuration)))
    (with-current-buffer (window-buffer (frame-selected-window))
      (push-mark)
      (activate-mark))
    (set-window-configuration wc)
    (should (or (not mark-active) (mark)))))

(ert-deftest core-elisp-tests-3-backquote ()
  (should (eq 3 (eval ``,,'(+ 1 2)))))

;; Test up-list and backward-up-list.
(defun lisp-run-up-list-test (fn data start instructions)
  (cl-labels ((posof (thing)
                (and (symbolp thing)
                     (= (length (symbol-name thing)) 1)
                     (- (aref (symbol-name thing) 0) ?a -1))))
    (with-temp-buffer
      (set-syntax-table (make-syntax-table))
      ;; Use a syntax table in which single quote is a string
      ;; character so that we can embed the test data in a lisp string
      ;; literal.
      (modify-syntax-entry ?\' "\"")
      (insert data)
      (goto-char (posof start))
      (dolist (instruction instructions)
        (cond ((posof instruction)
               (funcall fn)
               (should (eql (point) (posof instruction))))
              ((symbolp instruction)
               (should-error (funcall fn)
                             :type instruction))
              (t (cl-assert nil nil "unknown ins")))))))

(defmacro define-lisp-up-list-test (name fn data start &rest expected)
  `(ert-deftest ,name ()
     (lisp-run-up-list-test ,fn ,data ',start ',expected)))

(define-lisp-up-list-test up-list-basic
  (lambda () (up-list))
  (or "(1 1 (1 1) 1 (1 1) 1)")
  ;;   abcdefghijklmnopqrstuv
  i k v scan-error)

(define-lisp-up-list-test up-list-with-forward-sexp-function
  (lambda ()
    (let ((forward-sexp-function
           (lambda (&optional arg)
             (let ((forward-sexp-function nil))
               (forward-sexp arg)))))
      (up-list)))
  (or "(1 1 (1 1) 1 (1 1) 1)")
  ;;   abcdefghijklmnopqrstuv
  i k v scan-error)

(define-lisp-up-list-test up-list-out-of-string
  (lambda () (up-list 1 t))
  (or "1 (1 '2 2 (2 2 2' 1) 1")
  ;;   abcdefghijklmnopqrstuvwxy
  o r u scan-error)

(define-lisp-up-list-test up-list-cross-string
  (lambda () (up-list 1 t))
  (or "(1 '2 ( 2' 1 '2 ) 2' 1)")
  ;;   abcdefghijklmnopqrstuvwxy
  i r u x scan-error)

(define-lisp-up-list-test up-list-no-cross-string
  (lambda () (up-list 1 t t))
  (or "(1 '2 ( 2' 1 '2 ) 2' 1)")
  ;;   abcdefghijklmnopqrstuvwxy
  i k x scan-error)

(define-lisp-up-list-test backward-up-list-basic
  (lambda () (backward-up-list))
  (or "(1 1 (1 1) 1 (1 1) 1)")
  ;;   abcdefghijklmnopqrstuv
  i f a scan-error)

(provide 'lisp-tests)
;;; lisp-tests.el ends here
