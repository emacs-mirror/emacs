;;; cwarn.el --- highlight suspicious C and C++ constructions

;; Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.

;; Author: Anders Lindgren <andersl@andersl.com>
;; Keywords: c, languages, faces
;; X-Url: http://www.andersl.com/emacs
;; Version: 1.3.1  1999-12-13

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

;;{{{ Documentation

;; Description:
;;
;; CWarn is a package that highlights suspicious C and C++ constructions.
;;
;; For example, take a look at the following piece of C code:
;;
;;     if (x = 0);
;;       foo();
;;
;; The code contains two, possibly fatal, bugs.  The first is that the
;; assignment operator "=" is used as part of the test; the user
;; probably ment to use the comparison operator "==".
;;
;; The second problem is that an extra semicolon is placed after
;; closing parenthesis of the test expression.  This makes the body of
;; the if statement to be an empty statement, not the call to the
;; function "foo", as the user probably intended.
;;
;; This package is capable of highlighting the following C and C++
;; constructions:
;;
;; * Assignments inside expressions, including variations like "+=".
;; * Semicolon following immediately after `if', `for', and `while'
;;   (except, of course, after a `do .. while' statement).
;; * C++ functions with reference parameters.
;;
;; Note that none of the constructions highlighted (especially not C++
;; reference parameters) are considered errors by the langauage
;; definitions.

;; Usage:
;;
;; CWarn is implemented as two minor modes: `cwarn-mode' and
;; `global-cwarn-mode'.  The former can be applied to individual buffers
;; and the latter to all buffers.
;;
;; Activate this package by Customize, or by placing the following line
;; into the appropriate init file:
;;
;;    (global-cwarn-mode 1)
;;
;; Also, `font-lock-mode' or `global-font-lock-mode' must be enabled.

;; Afterthought:
;;
;; After using this package for several weeks it feels as though I
;; find stupid typo-style bugs while editing rather than at compile-
;; or run-time, if I ever find them.
;;
;; On the other hand, I find myself using assignments inside
;; expressions much more often than I used to do.  The reason is that
;; there is no risk of interpreting an assignment operator as a
;; comparison ("hey, the assignment operator is red, duh!").

;; Reporting bugs:
;;
;;     Out of the last ten bugs you found, how many did you report?
;;
;; When reporting a bug, please:
;;
;; * Send a mail the maintainer of the package, or to the author
;;   if no maintainer exists.
;; * Include the name of the package in the title of the mail, to
;;   simplify for the recipient.
;; * State exactly what you did, what happened, and what you expected
;;   to see when you found the bug.
;; * If the bug cause an error, set the variable `debug-on-error' to t,
;;   repreat the operations that triggered the error and include
;;   the backtrace in the letter.
;; * If possible, include an example that activates the bug.
;; * Should you speculate about the cause of the problem, please
;;   state explicitly that you are guessing.

;;}}}

;;; Code:

;;{{{ Dependencies

(eval-when-compile (require 'cl))

(require 'custom)
(require 'font-lock)
(require 'cc-mode)

;;}}}
;;{{{ Variables

(defgroup cwarn nil
  "Highlight suspicious C and C++ constructions."
  :version "21.1"
  :link '(url-link "http://www.andersl.com/emacs")
  :group 'faces)

(defvar cwarn-mode nil
  "*Non-nil when Cwarn mode is active.

Never set this variable directly, use the command `cwarn-mode'
instead.")

(defcustom global-cwarn-mode nil
  "When on, suspicious C and C++ constructions are highlighted.

Set this variable using \\[customize] or use the command
`global-cwarn-mode'."
  :group 'cwarn
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
	 (global-cwarn-mode (or value 0)))
  :type 'boolean
  :require 'cwarn)

(defcustom cwarn-configuration
  '((c-mode (not reference))
    (c++-mode t))
  "*List of items each describing which features are enable for a mode.
Each item is on the form (mode featurelist), where featurelist can be
on one of three forms:

* A list of enabled features.
* A list starting with the atom `not' followed by the features
  which are not enabled.
* The atom t, that represent that all features are enabled.

See variable `cwarn-font-lock-feature-keywords-alist' for available
features."
  :type '(repeat sexp)
  :group 'cwarn)

(defcustom cwarn-font-lock-feature-keywords-alist
  '((assign    . cwarn-font-lock-assignment-keywords)
    (semicolon . cwarn-font-lock-semicolon-keywords)
    (reference . cwarn-font-lock-reference-keywords))
  "An alist mapping a CWarn feature to font-lock keywords.
The keywords could either a font-lock keyword list or a symbol.
If it is a symbol it is assumed to be a variable containing a font-lock
keyword list."
  :type '(alist :key-type (choice (const assign)
				  (const semicolon)
				  (const reference))
		:value-type (sexp :tag "Value"))
  :group 'cwarn)

(defcustom cwarn-verbose t
  "*When nil, CWarn mode will not generate any messages.

Currently, messages are generated when the mode is activated and
deactivated."
  :group 'cwarn
  :type 'boolean)

(defcustom cwarn-mode-text " CWarn"
  "*String to display in the mode line when CWarn mode is active.

\(When the string is not empty, make sure that it has a leading space.)"
  :tag "CWarn mode text"                ; To separate it from `global-...'
  :group 'cwarn
  :type 'string)

(defcustom cwarn-mode-hook nil
  "*Functions to run when CWarn mode is activated."
  :tag "CWarn mode hook"                ; To separate it from `global-...'
  :group 'cwarn
  :type 'hook)

(defcustom global-cwarn-mode-text ""
  "*String to display when Global CWarn mode is active.

The default is nothing since when this mode is active this text doesn't
vary over time, or between buffers.  Hence mode line text
would only waste precious space."
  :group 'cwarn
  :type 'string)

(defcustom global-cwarn-mode-hook nil
  "*Hook called when Global CWarn mode is activated."
  :group 'cwarn
  :type 'hook)

(defcustom cwarn-load-hook nil
  "*Functions to run when CWarn mode is first loaded."
  :tag "Load Hook"
  :group 'cwarn
  :type 'hook)

;;}}}
;;{{{ The modes

;;;###autoload
(defun cwarn-mode (&optional arg)
  "Minor mode that highlights suspicious C and C++ constructions.

Note, in addition to enabling this minor mode, the major mode must
be included in the variable `cwarn-configuration'.  By default C and
C++ modes are included.

With ARG, turn CWarn mode on if and only if arg is positive."
  (interactive "P")
  (make-local-variable 'cwarn-mode)
  (setq cwarn-mode
	(if (null arg)
	    (not cwarn-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if (and cwarn-verbose
	   (interactive-p))
      (message "Cwarn mode is now %s."
	       (if cwarn-mode "on" "off")))
  (if (not global-cwarn-mode)
      (if cwarn-mode
	  (cwarn-font-lock-add-keywords)
	(cwarn-font-lock-remove-keywords)))
  (font-lock-fontify-buffer)
  (if cwarn-mode
      (run-hooks 'cwarn-mode-hook)))

;;;###autoload
(defun turn-on-cwarn-mode ()
  "Turn on CWarn mode.

This function is designed to be added to hooks, for example:
  (add-hook 'c-mode-hook 'turn-on-cwarn-mode)"
  (cwarn-mode 1))

;;;###autoload
(defun global-cwarn-mode (&optional arg)
  "Hightlight suspicious C and C++ constructions in all buffers.

With ARG, turn CWarn mode on globally if and only if arg is positive."
  (interactive "P")
  (let ((old-global-cwarn-mode global-cwarn-mode))
    (setq global-cwarn-mode
	  (if (null arg)
	      (not global-cwarn-mode)
	    (> (prefix-numeric-value arg) 0)))
    (if (and cwarn-verbose
	     (interactive-p))
	(message "Global CWarn mode is now %s."
		 (if global-cwarn-mode "on" "off")))
    (when (not (eq global-cwarn-mode old-global-cwarn-mode))
      ;; Update for all future buffers.
      (dolist (conf cwarn-configuration)
	(if global-cwarn-mode
	    (cwarn-font-lock-add-keywords (car conf))
	  (cwarn-font-lock-remove-keywords (car conf))))
      ;; Update all existing buffers.
      (save-excursion
	(dolist (buffer (buffer-list))
	  (set-buffer buffer)
	  ;; Update keywords in alive buffers.
	  (when (and font-lock-mode
		     (not cwarn-mode)
		     (cwarn-is-enabled major-mode))
	    (if global-cwarn-mode
		(cwarn-font-lock-add-keywords)
	      (cwarn-font-lock-remove-keywords))
	    (font-lock-fontify-buffer))))))
    ;; Kills all added keywords :-(
    ;; (font-lock-mode 0)
    ;; (makunbound 'font-lock-keywords)
    ;; (font-lock-mode 1))))
  (when global-cwarn-mode
    (run-hooks 'global-cwarn-mode-hook)))

;;}}}
;;{{{ Help functions

(defun cwarn-is-enabled (mode &optional feature)
  "Non-nil if CWarn FEATURE is enabled for MODE.
feature is an atom representing one construction to highlight.

Check if any feature is enabled for MODE if no feature is specified.

The valid features are described by the variable
`cwarn-font-lock-feature-keywords-alist'."
  (let ((mode-configuraion (assq mode cwarn-configuration)))
    (and mode-configuraion
	 (or (null feature)
	     (let ((list-or-t (nth 1 mode-configuraion)))
	       (or (eq list-or-t t)
		   (if (eq (car-safe list-or-t) 'not)
		       (not (memq feature (cdr list-or-t)))
		     (memq feature list-or-t))))))))

(defun cwarn-inside-macro ()
  "True if point is inside a C macro definition."
  (save-excursion
    (beginning-of-line)
    (while (eq (char-before (1- (point))) ?\\)
      (forward-line -1))
    (back-to-indentation)
    (eq (char-after) ?#)))

(defun cwarn-font-lock-add-keywords (&optional mode)
  "Install keywords into major MODE, or into current buffer if nil."
  (dolist (pair cwarn-font-lock-feature-keywords-alist)
    (let ((feature (car pair))
	  (keywords (cdr pair)))
      (if (not (listp keywords))
	  (setq keywords (symbol-value keywords)))
      (if (cwarn-is-enabled (or mode major-mode) feature)
	  (font-lock-add-keywords mode keywords)))))

(defun cwarn-font-lock-remove-keywords (&optional mode)
  "Remove keywords from major MODE, or from current buffer if nil."
  (dolist (pair cwarn-font-lock-feature-keywords-alist)
    (let ((feature (car pair))
	  (keywords (cdr pair)))
      (if (not (listp keywords))
	  (setq keywords (symbol-value keywords)))
      (if (cwarn-is-enabled (or mode major-mode) feature)
	  (font-lock-remove-keywords mode keywords)))))

;;}}}
;;{{{ Backward compatibility

;; This piece of code will be part of CC mode as of Emacs 20.4.
(if (not (fboundp 'c-at-toplevel-p))
(defun c-at-toplevel-p ()
  "Return a determination as to whether point is at the `top-level'.
Being at the top-level means that point is either outside any
enclosing block (such function definition), or inside a class
definition, but outside any method blocks.

If point is not at the top-level (e.g. it is inside a method
definition), then nil is returned.  Otherwise, if point is at a
top-level not enclosed within a class definition, t is returned.
Otherwise, a 2-vector is returned where the zeroth element is the
buffer position of the start of the class declaration, and the first
element is the buffer position of the enclosing class's opening
brace."
  (let ((state (c-parse-state)))
    (or (not (c-most-enclosing-brace state))
	(c-search-uplist-for-classkey state))))
)

;;}}}
;;{{{ Font-lock keywords and match functions

;; This section contains font-lock keywords.  A font lock keyword can
;; either contain a regular expression or a match function.  All
;; keywords defined here use match functions since the C and C++
;; constructions highlighted by CWarn are too complex to be matched by
;; regular expressions.
;;
;; A match function should act like a normal forward search.  They
;; should return non-nil if they found a candidate and the match data
;; should correspond to the highlight part of the font-lock keyword.
;; The functions shold not generate errors, in that case font-lock
;; will fail to highlight the buffer.  A match function takes one
;; argument, LIMIT, that represent the end of area to be searched.
;;
;; The variable `cwarn-font-lock-feature-keywords-alist' contains a
;; mapping from CWarn features to the font-lock keywords defined
;; below.

;;{{{ Assignment in expressions

(defconst cwarn-font-lock-assignment-keywords
  '((cwarn-font-lock-match-assignment-in-expression
     (1 font-lock-warning-face))))

(defun cwarn-font-lock-match-assignment-in-expression (limit)
  "Match assignments inside expressions."
  (let ((res nil))
    (while
	(progn
	  (setq res (re-search-forward
		     "[^!<>=]\\(\\([-+*/%&^|]\\|<<\\|>>\\)?=\\)[^=]"
		     limit t))
	  (and res
	       (save-excursion
		 (goto-char (match-beginning 1))
		 (condition-case nil    ; In case "backward-up-list" barfs.
		     (progn
		       (backward-up-list 1)
		       (or (not (memq (following-char) '(?\( ?\[)))
			   (save-match-data
			     (skip-chars-backward " ")
			     (skip-chars-backward "a-zA-Z0-9_")
			     (or
			      ;; Default parameter of function.
			      (c-at-toplevel-p)
			      (looking-at "for\\>")))))
		   (error t))))))
      res))

;;}}}
;;{{{ Semicolon

(defconst cwarn-font-lock-semicolon-keywords
  '((cwarn-font-lock-match-dangerous-semicolon (0 font-lock-warning-face))))

(defun cwarn-font-lock-match-dangerous-semicolon (limit)
  "Match semicolons directly after `for', `while', and `if'.
Tne semicolon after a `do { ... } while (x);' construction is not matched."
  (let ((res nil))
    (while
	(progn
	  (setq res (search-forward ";" limit t))
	  (and res
	       (save-excursion
		 (condition-case nil    ; In case something barfs.
		     (save-match-data
		       (backward-sexp 2) ; Expression and keyword.
		       (not (or (looking-at "\\(for\\|if\\)\\>")
				(and (looking-at "while\\>")
				     (condition-case nil
					 (progn
					   (backward-sexp 2) ; Body and "do".
					   (not (looking-at "do\\>")))
				       (error t))))))
		   (error t))))))
      res))

;;}}}
;;{{{ Reference

(defconst cwarn-font-lock-reference-keywords
  '((cwarn-font-lock-match-reference (1 font-lock-warning-face))))

(defun cwarn-font-lock-match-reference (limit)
  "Font-lock matcher for C++ reference parameters."
  (let ((res nil))
    (while
	(progn
	  (setq res (re-search-forward "[^&]\\(&\\)[^&=]" limit t))
	  (and res
	       (save-excursion
		 (goto-char (match-beginning 1))
		 (condition-case nil    ; In case something barfs.
		     (save-match-data
		       (backward-up-list 1)
		       (or (not (eq (following-char) ?\())
			   (cwarn-inside-macro)
			   (not (c-at-toplevel-p))))
		   (error t))))))
    res))

;;}}}

;;}}}
;;{{{ The end

(unless (assq 'cwarn-mode minor-mode-alist)
  (push '(cwarn-mode cwarn-mode-text)
	minor-mode-alist))
(unless (assq 'global-cwarn-mode minor-mode-alist)
  (push '(global-cwarn-mode global-cwarn-mode-text)
	minor-mode-alist))

(provide 'cwarn)

(run-hooks 'cwarn-load-hook)

;; This makes it possible to set Global CWarn mode from
;; Customize.
(if global-cwarn-mode
    (global-cwarn-mode 1))

;;}}}

;;; cwarn.el ends here
