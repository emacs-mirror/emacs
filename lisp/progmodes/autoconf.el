;;; autoconf.el --- mode for editing Autoconf configure.in files

;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
;; Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: languages

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Provides fairly minimal font-lock, imenu and indentation support
;; for editing configure.in files.  Only Autoconf syntax is processed.
;; There is no attempt to deal with shell text -- probably that will
;; always lose.

;; This is specialized for configure.in files.  It doesn't inherit the
;; general M4 stuff from M4 mode.

;; There is also an autoconf-mode.el in existence.  That appears to be
;; for editing the Autoconf M4 source, rather than configure.in files.

;;; Code:

(defvar font-lock-syntactic-keywords)

(defvar autoconf-mode-map (make-sparse-keymap))

(defvar autoconf-mode-hook nil
  "Hook run by `autoconf-mode'.")

(defconst autoconf-font-lock-syntactic-keywords
  '(("\\<dnl\\>" 0 '(11))))

(defconst autoconf-definition-regexp
  "AC_\\(SUBST\\|DEFINE\\(_UNQUOTED\\)?\\)(\\[*\\(\\sw+\\)\\]*")

(defvar autoconf-font-lock-keywords
  `(("\\_<A[CHMS]_\\sw+" . font-lock-keyword-face)
    (,autoconf-definition-regexp
     3 font-lock-function-name-face)
    ;; Are any other M4 keywords really appropriate for configure.in,
    ;; given that we do `dnl'?
    ("changequote" . font-lock-keyword-face)))

(defvar autoconf-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?# "<" table)
    table))

(defvar autoconf-imenu-generic-expression
  (list (list nil autoconf-definition-regexp 3)))

;; It's not clear how best to implement this.
(defun autoconf-current-defun-function ()
  "Function to use for `add-log-current-defun-function' in Autoconf mode.
This version looks back for an AC_DEFINE or AC_SUBST.  It will stop
searching backwards at another AC_... command."
  (save-excursion
    (with-syntax-table (copy-syntax-table autoconf-mode-syntax-table)
      (modify-syntax-entry ?_ "w")
      (if (re-search-backward autoconf-definition-regexp
			      (save-excursion (beginning-of-defun) (point))
			      t)
	  (match-string-no-properties 3)))))

;;;###autoload
(defun autoconf-mode ()
  "Major mode for editing Autoconf configure.in files."
  (interactive)
  (kill-all-local-variables)
  (use-local-map autoconf-mode-map)
  (setq major-mode 'autoconf-mode)
  (setq mode-name "Autoconf")
  (set-syntax-table autoconf-mode-syntax-table)
  (set (make-local-variable 'parens-require-spaces) nil) ; for M4 arg lists
  (set (make-local-variable 'defun-prompt-regexp)
       "^[ \t]*A[CM]_\\(\\sw\\|\\s_\\)+")
  (set (make-local-variable 'comment-start) "dnl ")
  (set (make-local-variable 'comment-start-skip) "\\(?:\\<dnl\\|#\\) +")
  (set (make-local-variable 'font-lock-syntactic-keywords)
       autoconf-font-lock-syntactic-keywords)
  (set (make-local-variable 'font-lock-defaults)
       `(autoconf-font-lock-keywords nil nil (("_" . "w"))))
  (set (make-local-variable 'imenu-generic-expression)
       autoconf-imenu-generic-expression)
  (set (make-local-variable 'imenu-syntax-alist) '(("_" . "w")))
  (set (make-local-variable 'indent-line-function) #'indent-relative)
  (set (make-local-variable 'add-log-current-defun-function)
	#'autoconf-current-defun-function)
  (run-mode-hooks 'autoconf-mode-hook))

(provide 'autoconf-mode)

;; arch-tag: 4f44778f-2ab3-49a1-a103-f0acb9df2de4
;;; autoconf.el ends here
