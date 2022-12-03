;;; ob-sqlite.el --- Babel Functions for SQLite Databases -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2022 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Maintainer: Nick Savage <nick@nicksavage.ca>
;; Keywords: literate programming, reproducible research
;; URL: https://orgmode.org

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

;; Org-Babel support for evaluating sqlite source code.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob)
(require 'ob-sql)

(declare-function org-table-convert-region "org-table"
		  (beg0 end0 &optional separator))
(declare-function orgtbl-to-csv "org-table" (table params))
(declare-function org-table-to-lisp "org-table" (&optional txt))

(defvar org-babel-default-header-args:sqlite '())

(defvar org-babel-header-args:sqlite
  '((db        . :any)
    (header    . :any)
    (echo      . :any)
    (bail      . :any)
    (csv       . :any)
    (column    . :any)
    (html      . :any)
    (line      . :any)
    (list      . :any)
    (separator . :any)
    (nullvalue . :any))
  "Sqlite specific header args.")

(defun org-babel-expand-body:sqlite (body params)
  "Expand BODY according to the values of PARAMS."
  (org-babel-sql-expand-vars
   body (org-babel--get-vars params) t))

(defvar org-babel-sqlite3-command "sqlite3")

(defun org-babel-execute:sqlite (body params)
  "Execute a block of Sqlite code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let ((result-params (split-string (or (cdr (assq :results params)) "")))
	(db (cdr (assq :db params)))
	(separator (cdr (assq :separator params)))
	(nullvalue (cdr (assq :nullvalue params)))
	(headers-p (equal "yes" (cdr (assq :colnames params))))
	(others (delq nil (mapcar
			   (lambda (arg) (car (assq arg params)))
			   (list :header :echo :bail :column
				 :csv :html :line :list)))))
    (unless db (error "ob-sqlite: can't evaluate without a database"))
    (with-temp-buffer
      (insert
       (org-babel-eval
	(org-fill-template
	 "%cmd %header %separator %nullvalue %others %csv %db "
	 (list
	  (cons "cmd" org-babel-sqlite3-command)
	  (cons "header" (if headers-p "-header" "-noheader"))
	  (cons "separator"
		(if separator (format "-separator %s" separator) ""))
	  (cons "nullvalue"
		(if nullvalue (format "-nullvalue %s" nullvalue) ""))
	  (cons "others"
		(mapconcat
		 (lambda (arg) (format "-%s" (substring (symbol-name arg) 1)))
		 others " "))
	  ;; for easy table parsing, default header type should be -csv
	  (cons "csv" (if (or (member :csv others) (member :column others)
			      (member :line others) (member :list others)
			      (member :html others) separator)
			  ""
			"-csv"))
	  (cons "db " db)))
	;; body of the code block
	(org-babel-expand-body:sqlite body params)))
      (org-babel-result-cond result-params
	(buffer-string)
	(if (equal (point-min) (point-max))
	    ""
	  (org-table-convert-region (point-min) (point-max)
				    (if (or (member :csv others)
					    (member :column others)
					    (member :line others)
					    (member :list others)
					    (member :html others) separator)
					nil
				      '(4)))
	  (org-babel-sqlite-table-or-scalar
	   (org-babel-sqlite-offset-colnames
	    (org-table-to-lisp) headers-p)))))))

(defun org-babel-sqlite-expand-vars (body vars)
  "Expand the variables held in VARS in BODY."
  (declare (obsolete "use `org-babel-sql-expand-vars' instead." "9.5"))
  (org-babel-sql-expand-vars body vars t))

(defun org-babel-sqlite-table-or-scalar (result)
  "If RESULT looks like a trivial table, then unwrap it."
  (if (and (equal 1 (length result))
	   (equal 1 (length (car result))))
      (org-babel-read (caar result) t)
    (mapcar (lambda (row)
	      (if (eq 'hline row)
		  'hline
		(mapcar #'org-babel-sqlite--read-cell row)))
	    result)))

(defun org-babel-sqlite-offset-colnames (table headers-p)
  "If HEADERS-P is non-nil then offset the first row as column names."
  (if headers-p
      (cons (car table) (cons 'hline (cdr table)))
    table))

(defun org-babel-prep-session:sqlite (_session _params)
  "Raise an error because support for SQLite sessions isn't implemented.
Prepare SESSION according to the header arguments specified in PARAMS."
  (error "SQLite sessions not yet implemented"))

(defun org-babel-sqlite--read-cell (cell)
  "Process CELL to remove unnecessary characters."
  (org-babel-read cell t))

(provide 'ob-sqlite)

;;; ob-sqlite.el ends here
