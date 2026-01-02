;;; autoconf.el --- mode for editing Autoconf configure.ac files  -*- lexical-binding: t; -*-

;; Copyright (C) 2000-2026 Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: languages

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

;; Provides fairly minimal font-lock, imenu and indentation support
;; for editing configure.ac files.  Only Autoconf syntax is processed.
;; There is no attempt to deal with shell text -- probably that will
;; always lose.

;; This is specialized for configure.ac files.  It doesn't inherit the
;; general M4 stuff from M4 mode.

;; There is also an autoconf-mode.el in existence.  That appears to be
;; for editing the Autoconf M4 source, rather than configure.ac files.

;;; Code:

(defvar-keymap autoconf-mode-map)

(defvar autoconf-mode-hook nil
  "Hook run by `autoconf-mode'.")

(rx-define autoconf--symbol (+ (| (syntax word) (syntax symbol))))

;; Any Autoconf macro name.
(rx-define autoconf--macro
  (: (| "AC"  ;; Autoconf.
        "AH"  ;; Autoheader.
        "AM"  ;; Automake.
        "AS"  ;; M4sh.
        "AU"  ;; Autoupdate.
        "AX"  ;; Autoconf Archive.
        "LT"  ;; Libtool.
        "gl") ;; Gnulib.
     ?_ autoconf--symbol))

(defconst autoconf-definition-regexp
  ;; Historically this `defconst' defined only group #1.
  ;; For internal Font Lock use, the presence of an optional group #2
  ;; identifies a function rather than variable definition.
  (rx-let ((argbeg (: ?\( (* ?\[)))
           (argend (in "]),"))
           (plaindef (: argbeg (group-n 1 autoconf--symbol))))
    (rx symbol-start
        (| (: "AC_DEFINE"
              ;; AC_DEFINE and AC_DEFINE_UNQUOTED can define object- and
              ;; function-like CPP macros.  An open-paren is easy to
              ;; detect in the case of AC_DEFINE.  Doing the same for
              ;; AC_DEFINE_UNQUOTED in the general case requires
              ;; knowledge of shell syntax, so don't bother for now.
              (| (: plaindef (? (group-n 2 ?\()))
                 (: "_UNQUOTED" argbeg (group-n 1 (+ (not argend))))))
           (: (| "AC_SUBST"
                 "AH_TEMPLATE"
                 "AH_VERBATIM"
                 "AM_CONDITIONAL"
                 "AM_MISSING_PROG"
                 (group-n 2 (| "AC_DEFUN"
                               "AC_DEFUN_ONCE"
                               "AU_ALIAS"
                               "AU_DEFUN")))
              plaindef))))
  "Matches Autoconf macro calls that define something.
The thing being defined is captured in the first subexpression group.")

(defvar autoconf-font-lock-keywords
  `(,(rx symbol-start autoconf--macro)
    (,autoconf-definition-regexp
     1 (if (match-beginning 2)
           'font-lock-function-name-face
         'font-lock-variable-name-face))
    ;; Are any other M4 keywords really appropriate for configure.ac,
    ;; given that we do `dnl'?
    "\\_<changequote\\_>"))

(defvar autoconf-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?# "<" table)
    table))

(defvar autoconf-imenu-generic-expression
  ;; This lists both variable-like and function-like definitions in a
  ;; flat list, but they could be distinguished if desired.
  (list (list nil autoconf-definition-regexp 1)))

;; It's not clear how best to implement this.
(defun autoconf-current-defun-function ()
  "Function to use for `add-log-current-defun-function' in Autoconf mode.
This version looks back for a definition such as by AC_DEFINE or
AC_SUBST.  It stops searching when it encounters other Autoconf macros."
  (save-excursion
    (skip-syntax-forward "w_" (pos-eol))
    (if (re-search-backward autoconf-definition-regexp
                            (save-excursion (beginning-of-defun) (point))
                            t)
        (match-string-no-properties 1))))

;;;###autoload
(define-derived-mode autoconf-mode prog-mode "Autoconf"
  "Major mode for editing Autoconf configure.ac files."
  (setq-local parens-require-spaces nil) ; for M4 arg lists
  ;; FIXME: Should indented macro calls really count as defuns?
  (setq-local defun-prompt-regexp (rx bol (* (in "\t ")) autoconf--macro))
  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local comment-start "dnl ")
  ;; We want to avoid matching "dnl" in other text.
  (setq-local comment-start-skip "\\(?:\\(\\W\\|^\\)dnl\\|#\\) +")
  (setq-local syntax-propertize-function
	      (syntax-propertize-rules ("\\<dnl\\>" (0 "<"))))
  (setq-local font-lock-defaults '(autoconf-font-lock-keywords))
  (setq-local imenu-generic-expression autoconf-imenu-generic-expression)
  (setq-local indent-line-function #'indent-relative)
  (setq-local add-log-current-defun-function
	      #'autoconf-current-defun-function))

(provide 'autoconf-mode)
(provide 'autoconf)

;;; autoconf.el ends here
