;;; imenu-tests.el --- Test suite for imenu.

;; Copyright (C) 2013-2018 Free Software Foundation, Inc.

;; Author: Masatake YAMATO <yamato@redhat.com>
;; Keywords: tools convenience

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

(require 'ert)
(require 'imenu)

;; (imenu-simple-scan-deftest-gather-strings-from-list
;;     '(nil t 'a (0 . "x") ("c" . "d") ("a" 0 "b") ))
;; => ("b" "a" "d" "c" "x")
(defun imenu-simple-scan-deftest-gather-strings-from-list(input)
  "Gather strings from INPUT, a list."
  (let ((result ()))
    (while input
      (cond
       ((stringp input)
	(setq result (cons input result)
	      input nil))
       ((atom input)
	(setq input nil))
       ((listp (car input))
	(setq result (append
		      (imenu-simple-scan-deftest-gather-strings-from-list (car input))
		      result)
	      input (cdr input)))
       ((stringp (car input))
	(setq result (cons (car input) result)
	      input (cdr input)))
       (t
	(setq input (cdr input)))))
    result))

(defmacro imenu-simple-scan-deftest (name doc major-mode content expected-items)
  "Generate an ert test for mode-own imenu expression.
Run `imenu-create-index-function' at the buffer which content is
CONTENT with MAJOR-MODE. A generated test runs `imenu-create-index-function'
at the buffer which content is CONTENT with MAJOR-MODE. Then it compares a list
of strings which are picked up from the result with EXPECTED-ITEMS."
  (let ((xname (intern (concat "imenu-simple-scan-deftest-" (symbol-name name)))))
    `(ert-deftest ,xname ()
	 ,doc
       (with-temp-buffer
	 (insert ,content)
	 (funcall ',major-mode)
	 (let ((result-items (sort (imenu-simple-scan-deftest-gather-strings-from-list
				    (funcall imenu-create-index-function))
				   #'string-lessp))
	       (expected-items (sort (copy-sequence ,expected-items) #'string-lessp)))
	   (should (equal result-items expected-items))
	   )))))

(imenu-simple-scan-deftest sh "Test imenu expression for sh-mode." sh-mode "a()
{
}
function b
{
}
function c()
{
}
function ABC_D()
{
}
" '("a" "b" "c" "ABC_D"))

(ert-deftest imenu--sort-by-position-pairs ()
  (should (imenu--sort-by-position '("a" . 2) '("a" . 3)))
  (should-not (imenu--sort-by-position '("a" . 3) '("a" . 2))))

;; Regression test for bug#26457: 25.2; Cannot pass a function to
;; imenu-generic-expression
(ert-deftest imenu--sort-by-position-list ()
  (should (imenu--sort-by-position '("a" 2 nil) '("a" 3 nil)))
  (should-not (imenu--sort-by-position '("a" 3 nil) '("a" 2 nil))))

(provide 'imenu-tests)

;;; imenu-tests.el ends here
