;;; ob-table.el --- support for calling org-babel functions from tables

;; Copyright (C) 2009, 2010  Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 7.4

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

;;; Commentary:

;; Should allow calling functions from org-mode tables using the
;; function `sbe' as so...

;; #+begin_src emacs-lisp :results silent
;;   (defun fibbd (n) (if (< n 2) 1 (+ (fibbd (- n 1)) (fibbd (- n 2)))))
;; #+end_src

;; #+srcname: fibbd
;; #+begin_src emacs-lisp :var n=2 :results silent
;; (fibbd n)
;; #+end_src

;; | original | fibbd  |
;; |----------+--------|
;; |        0 |        |
;; |        1 |        |
;; |        2 |        |
;; |        3 |        |
;; |        4 |        |
;; |        5 |        |
;; |        6 |        |
;; |        7 |        |
;; |        8 |        |
;; |        9 |        |
;; #+TBLFM: $2='(sbe 'fibbd (n $1))

;;; Code:
(require 'ob)

(defun org-babel-table-truncate-at-newline (string)
  "Replace newline character with ellipses.
If STRING ends in a newline character, then remove the newline
character and replace it with ellipses."
  (if (and (stringp string) (string-match "[\n\r]\\(.\\)?" string))
      (concat (substring string 0 (match-beginning 0))
	      (if (match-string 1 string) "...")) string))

(defmacro sbe (source-block &rest variables)
  "Return the results of calling SOURCE-BLOCK with VARIABLES.
Each element of VARIABLES should be a two
element list, whose first element is the name of the variable and
second element is a string of its value.  The following call to
`sbe' would be equivalent to the following source code block.

 (sbe 'source-block (n $2) (m 3))

#+begin_src emacs-lisp :var results=source-block(n=val_at_col_2, m=3) :results silent
results
#+end_src

NOTE: by default string variable names are interpreted as
references to source-code blocks, to force interpretation of a
cell's value as a string, prefix the identifier with two \"$\"s
rather than a single \"$\" (i.e. \"$$2\" instead of \"$2\" in the
example above."
  (let* (quote
	 (variables
	  (mapcar
	   (lambda (var)
	     ;; ensure that all cells prefixed with $'s are strings
	     (cons (car var)
		   (delq nil (mapcar
			      (lambda (el)
				(if (eq '$ el)
				    (setq quote t)
				  (prog1 (if quote
					     (format "\"%s\"" el)
					   (org-babel-clean-text-properties el))
				    (setq quote nil))))
			      (cdr var)))))
	   variables)))
    (unless (stringp source-block)
      (setq source-block (symbol-name source-block)))
    (org-babel-table-truncate-at-newline ;; org-table cells can't be multi-line
     (if (and source-block (> (length source-block) 0))
         (let ((params
                (eval `(org-babel-parse-header-arguments
                        (concat ":var results="
                                ,source-block
                                "("
                                (mapconcat
				 (lambda (var-spec)
				   (if (> (length (cdr var-spec)) 1)
				       (format "%S='%S"
					       (car var-spec)
					       (mapcar #'read (cdr var-spec)))
				     (format "%S=%s"
					     (car var-spec) (cadr var-spec))))
				 ',variables ", ")
                                ")")))))
           (org-babel-execute-src-block
            nil (list "emacs-lisp" "results" params) '((:results . "silent"))))
       ""))))

(provide 'ob-table)

;; arch-tag: 4234cc7c-4fc8-4e92-abb0-2892de1a493b

;;; ob-table.el ends here
