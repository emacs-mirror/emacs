;;; awk-mode.el --- AWK code editing commands for Emacs

;; Copyright (C) 1988, 1994, 1996 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: unix, languages

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Sets up C-mode with support for awk-style #-comments and a lightly
;; hacked syntax table.

;;; Code:

(defvar awk-mode-syntax-table nil
  "Syntax table in use in Awk-mode buffers.")

(if awk-mode-syntax-table
    ()
  (setq awk-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" awk-mode-syntax-table)
  (modify-syntax-entry ?\n ">   " awk-mode-syntax-table)
  (modify-syntax-entry ?\f ">   " awk-mode-syntax-table)
  (modify-syntax-entry ?\# "<   " awk-mode-syntax-table)
  (modify-syntax-entry ?/ "." awk-mode-syntax-table)
  (modify-syntax-entry ?* "." awk-mode-syntax-table)
  (modify-syntax-entry ?+ "." awk-mode-syntax-table)
  (modify-syntax-entry ?- "." awk-mode-syntax-table)
  (modify-syntax-entry ?= "." awk-mode-syntax-table)
  (modify-syntax-entry ?% "." awk-mode-syntax-table)
  (modify-syntax-entry ?< "." awk-mode-syntax-table)
  (modify-syntax-entry ?> "." awk-mode-syntax-table)
  (modify-syntax-entry ?& "." awk-mode-syntax-table)
  (modify-syntax-entry ?| "." awk-mode-syntax-table)
  (modify-syntax-entry ?_ "_" awk-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" awk-mode-syntax-table))

(defvar awk-mode-abbrev-table nil
  "Abbrev table in use in Awk-mode buffers.")
(define-abbrev-table 'awk-mode-abbrev-table ())

;; Regexps written with help from Peter Galbraith <galbraith@mixing.qc.dfo.ca>.
(defconst awk-font-lock-keywords
  (eval-when-compile
    (list
     ;;
     ;; Function names.
     '("^[ \t]*\\(function\\)\\>[ \t]*\\(\\sw+\\)?" 
       (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
     ;;
     ;; Variable names.
     (cons (concat "\\<\\("
;		   ("ARGC" "ARGIND" "ARGV" "CONVFMT" "ENVIRON" "ERRNO"
;		    "FIELDWIDTHS" "FILENAME" "FNR" "FS" "IGNORECASE" "NF" "NR"
;		    "OFMT" "OFS" "ORS" "RLENGTH" "RS" "RSTART" "SUBSEP")
		   "ARG\\([CV]\\|IND\\)\\|CONVFMT\\|E\\(NVIRON\\|RRNO\\)\\|"
		   "F\\(I\\(ELDWIDTHS\\|LENAME\\)\\|NR\\|S\\)\\|IGNORECASE\\|"
		   "N[FR]\\|O\\(F\\(MT\\|S\\)\\|RS\\)\\|"
		   "R\\(LENGTH\\|S\\(\\|TART\\)\\)\\|SUBSEP"
		   "\\)\\>")
	   'font-lock-variable-name-face)
     ;;
     ;; Keywords.
     (concat "\\<\\("
;	     ("BEGIN" "END" "break" "continue" "delete" "exit" "for"
;	      "getline" "if" "next" "print" "printf" "return" "while")
	     "BEGIN\\|END\\|break\\|continue\\|delete\\|exit\\|else\\|for\\|"
	     "getline\\|if\\|next\\|printf?\\|return\\|while"
	     "\\)\\>")
     ;;
     ;; Builtins.
     (list (concat "\\<\\("
;		   ("atan2" "close" "cos" "ctime" "exp" "gsub" "index" "int"
;		    "length" "log" "match" "rand" "sin" "split" "sprintf"
;		    "sqrt" "srand" "sub" "substr" "system" "time"
;		    "tolower" "toupper")
		   "atan2\\|c\\(lose\\|os\\|time\\)\\|exp\\|gsub\\|"
		   "in\\(dex\\|t\\)\\|l\\(ength\\|og\\)\\|match\\|rand\\|"
		   "s\\(in\\|p\\(lit\\|rintf\\)\\|qrt\\|rand\\|"
		   "ub\\(\\|str\\)\\|ystem\\)\\|"
		   "t\\(ime\\|o\\(lower\\|upper\\)\\)"
		   "\\)(")
	   1 'font-lock-builtin-face)
     ;;
     ;; Operators.  Is this too much?
     (cons (mapconcat 'identity
		      '("&&" "||" "<=" "<" ">=" ">" "==" "!=" "!~" "~")
		      "\\|")
	   'font-lock-constant-face)
     ))
 "Default expressions to highlight in AWK mode.")

;;;###autoload
(defun awk-mode ()
  "Major mode for editing AWK code.
This is much like C mode except for the syntax of comments.  It uses
the same keymap as C mode and has the same variables for customizing
indentation.  It has its own abbrev table and its own syntax table.

Turning on AWK mode calls the value of the variable `awk-mode-hook'
with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (require 'cc-mode)
  (c-initialize-cc-mode)
  (use-local-map c-mode-map)
  (setq major-mode 'awk-mode)
  (setq mode-name "AWK")
  (setq local-abbrev-table awk-mode-abbrev-table)
  (set-syntax-table awk-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'c-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#+ *")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'c-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(awk-font-lock-keywords nil nil ((?_ . "w"))))
  (run-hooks 'awk-mode-hook))

(provide 'awk-mode)

;;; awk-mode.el ends here
