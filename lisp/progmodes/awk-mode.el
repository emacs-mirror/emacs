;;; awk-mode.el --- AWK code editing commands for Emacs

;; Copyright (C) 1988,94,96,2000  Free Software Foundation, Inc.

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
     (cons (regexp-opt
	    '("ARGC" "ARGIND" "ARGV" "CONVFMT" "ENVIRON" "ERRNO"
	      "FIELDWIDTHS" "FILENAME" "FNR" "FS" "IGNORECASE" "NF" "NR"
	      "OFMT" "OFS" "ORS" "RLENGTH" "RS" "RSTART" "SUBSEP") 'words)
	   'font-lock-variable-name-face)
     ;;
     ;; Keywords.
     (regexp-opt
      '("BEGIN" "END" "break" "continue" "delete" "exit" "else" "for"
	"getline" "if" "next" "print" "printf" "return" "while") 'words)
     ;;
     ;; Builtins.
     (list (regexp-opt
	    '("atan2" "close" "cos" "ctime" "exp" "gsub" "index" "int"
	      "length" "log" "match" "rand" "sin" "split" "sprintf"
	      "sqrt" "srand" "sub" "substr" "system" "time"
	      "tolower" "toupper") 'words)
	   1 'font-lock-builtin-face)
     ;;
     ;; Operators.  Is this too much?
     (cons (regexp-opt '("&&" "||" "<=" "<" ">=" ">" "==" "!=" "!~" "~"))
	   'font-lock-constant-face)
     ))
 "Default expressions to highlight in AWK mode.")

;;;###autoload
(define-derived-mode awk-mode c-mode "AWK"
  "Major mode for editing AWK code.
This is much like C mode except for the syntax of comments.  Its keymap
inherits from C mode's and it has the same variables for customizing
indentation.  It has its own abbrev table and its own syntax table.

Turning on AWK mode runs `awk-mode-hook'."
  (set (make-local-variable 'paragraph-start) (concat "$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#+ *")
  (setq font-lock-defaults '(awk-font-lock-keywords nil nil ((?_ . "w")))))

(provide 'awk-mode)

;;; awk-mode.el ends here
