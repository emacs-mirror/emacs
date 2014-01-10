;;; lexbind-tests.el --- Testing the lexbind byte-compiler

;; Copyright (C) 2011-2014 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
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

(defconst lexbind-tests
  `(
    (let ((f #'car))
      (let ((f (lambda (x) (cons (funcall f x) (cdr x)))))
        (funcall f '(1 . 2))))
    )
  "List of expression for test.
Each element will be executed by interpreter and with
bytecompiled code, and their results compared.")



(defun lexbind-check-1 (pat)
  "Return non-nil if PAT is the same whether directly evalled or compiled."
  (let ((warning-minimum-log-level :emergency)
	(byte-compile-warnings nil)
	(v0 (condition-case nil
		(eval pat t)
	      (error nil)))
	(v1 (condition-case nil
		(funcall (let ((lexical-binding t))
                           (byte-compile `(lambda nil ,pat))))
	      (error nil))))
    (equal v0 v1)))

(put 'lexbind-check-1 'ert-explainer 'lexbind-explain-1)

(defun lexbind-explain-1 (pat)
  (let ((v0 (condition-case nil
		(eval pat t)
	      (error nil)))
	(v1 (condition-case nil
		(funcall (let ((lexical-binding t))
                           (byte-compile (list 'lambda nil pat))))
	      (error nil))))
    (format "Expression `%s' gives `%s' if directly evalled, `%s' if compiled."
	    pat v0 v1)))

(ert-deftest lexbind-tests ()
  "Test the Emacs byte compiler lexbind handling."
  (dolist (pat lexbind-tests)
    (should (lexbind-check-1 pat))))



(provide 'lexbind-tests)
;;; lexbind-tests.el ends here
