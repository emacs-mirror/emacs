;;; awk-mode.el --- AWK code editing commands for Emacs

;; Copyright (C) 1988, 1994 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

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
  (modify-syntax-entry ?\' "\"" awk-mode-syntax-table))

(defvar awk-mode-abbrev-table nil
  "Abbrev table in use in Awk-mode buffers.")
(define-abbrev-table 'awk-mode-abbrev-table ())

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
  (run-hooks 'awk-mode-hook))

;;; awk-mode.el ends here
