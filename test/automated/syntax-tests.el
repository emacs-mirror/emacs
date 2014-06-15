;;; syntax-tests.el --- Testing syntax rules and basic movement -*- lexical-binding: t -*-

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; Author: Daniel Colascione <dancol@dancol.org>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'ert)
(require 'cl-lib)

(defun run-up-list-test (fn data start instructions)
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

(defmacro define-up-list-test (name fn data start &rest expected)
  `(ert-deftest ,name ()
     (run-up-list-test ,fn ,data ',start ',expected)))

(define-up-list-test up-list-basic
  (lambda () (up-list))
  (or "(1 1 (1 1) 1 (1 1) 1)")
  ;;   abcdefghijklmnopqrstuv
  i k v scan-error)

(define-up-list-test up-list-with-forward-sexp-function
  (lambda ()
    (let ((forward-sexp-function
           (lambda (&optional arg)
             (let ((forward-sexp-function nil))
               (forward-sexp arg)))))
      (up-list)))
  (or "(1 1 (1 1) 1 (1 1) 1)")
  ;;   abcdefghijklmnopqrstuv
  i k v scan-error)

(define-up-list-test up-list-out-of-string
  (lambda () (up-list 1 t))
  (or "1 (1 '2 2 (2 2 2' 1) 1")
  ;;   abcdefghijklmnopqrstuvwxy
  o r u scan-error)

(define-up-list-test up-list-cross-string
  (lambda () (up-list 1 t))
  (or "(1 '2 ( 2' 1 '2 ) 2' 1)")
  ;;   abcdefghijklmnopqrstuvwxy
  i r u x scan-error)

(define-up-list-test up-list-no-cross-string
  (lambda () (up-list 1 t t))
  (or "(1 '2 ( 2' 1 '2 ) 2' 1)")
  ;;   abcdefghijklmnopqrstuvwxy
  i k x scan-error)

(define-up-list-test backward-up-list-basic
  (lambda () (backward-up-list))
  (or "(1 1 (1 1) 1 (1 1) 1)")
  ;;   abcdefghijklmnopqrstuv
  i f a scan-error)

(provide 'syntax-tests)
;;; syntax-tests.el ends here
