;;; warnings.el --- log and display warnings

;; Copyright (C) 2002 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

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

;; This file implements the entry points `warn', `lwarn'
;; and `display-warnings'.

;;; Code:

(defvar warning-levels
  '((:emergency "Emergency: " ding)
    (:error "Error: ")
    (:warning "Warning: ")
    (:debug "Debug: "))
  "List of severity level definitions for `define-warnings'.
Each element looks like (LEVEL STRING FUNCTION) and
defines LEVEL as a severity level.  STRING is the description
to use in the buffer, and FUNCTION (which may be omitted)
if non-nil is a function to call with no arguments
to get the user's attention.

:debug level is ignored by default (see `warning-minimum-level').")
(put 'warning-levels 'risky-local-variable t)

;; These are for compatibility with XEmacs.
;; I don't think there is any chance of finding meaningful distinctions
;; to distinguish so many levels.
(defvar warning-level-aliases
  '((emergency . :emergency)
    (error . :error)
    (warning . :warning)
    (notice . :warning)
    (info . :warning)
    (critical . :emergency)
    (alarm . :emergency))
  "Alist of aliases for severity levels for `display-warning'.
Each element looks like (ALIAS . LEVEL) and defines
ALIAS as equivalent to LEVEL.")

(defcustom warning-minimum-level :warning
  "Minimum severity level for displaying the warning buffer.
If a warning's severity level is lower than this,
the warning is logged in the warnings buffer, but the buffer
is not immediately displayed.  See also `warning-minimum-log-level'."
  :group 'warnings
  :type '(choice (const :emergency) (const :error) (const :warning))
  :version "21.4")
(defvaralias 'display-warning-minimum-level 'warning-minimum-level)

(defcustom warning-minimum-log-level :warning
  "Minimum severity level for logging a warning.
If a warning severity level is lower than this,
the warning is completely ignored."
  :group 'warnings
  :type '(choice (const :emergency) (const :error) (const :warning))
  :version "21.4")
(defvaralias 'log-warning-minimum-level 'warning-minimum-log-level)

(defcustom warning-suppress-log nil
  "List of warning types that should not be logged.
If any element of this list matches the GROUP argument to `display-warning',
the warning is completely ignored.
The element must match the first elements of GROUP.
Thus, (foo bar) as an element matches (foo bar)
or (foo bar ANYTHING...) as GROUP.
If GROUP is a symbol FOO, that is equivalent to the list (FOO)
so only the element (FOO) will match it."
  :group 'warnings
  :type '(repeat (repeat symbol))
  :version "21.4")

(defcustom warning-suppress nil
  "Custom groups for warnings not to display immediately.
If any element of this list matches the GROUP argument to `display-warning',
the warning is logged nonetheless, but the warnings buffer is
not immediately displayed.
The element must match an initial segment of the list GROUP.
Thus, (foo bar) as an element matches (foo bar)
or (foo bar ANYTHING...) as GROUP.
If GROUP is a symbol FOO, that is equivalent to the list (FOO),
so only the element (FOO) will match it.
See also `warning-suppress-log'."
  :group 'warnings
  :type '(repeat (repeat symbol))
  :version "21.4")

(defvar warning-prefix-function nil
  "Function to generate warning prefixes.
This function, if non-nil, is called with two arguments,
the severity level and its entry in `warning-levels',
and should return the entry that should actually be used.
The warnings buffer is current when this function is called
and the function can insert text in it.  This text becomes
the beginning of the warning.")

(defun warning-numeric-level (level)
  "Return a numeric measure of the warning severity level LEVEL."
  (let* ((elt (assq level warning-levels))
	 (link (memq elt warning-levels)))
    (length link)))
  
(defvar warning-series nil
  "Non-nil means treat multiple `display-warning' calls as a series.
An integer is a position in the warnings buffer
which is the start of the current series.
t means the next warning begins a series (and stores an integer here).
A symbol with a function definition is like t, except
also call that function before the next warning.")
(put 'warning-series 'risky-local-variable t)

(defvar warning-fill-prefix nil
  "Non-nil means fill each warning text using this string as `fill-prefix'.")

(defun warning-suppress-p (group suppress-list)
  "Non-nil if a warning with group GROUP should be suppressed.
SUPPRESS-LIST is the list of kinds of warnings to suppress."
  (let (some-match)
    (dolist (elt suppress-list)
      (if (symbolp group)
	  ;; If GROUP is a symbol, the ELT must be (GROUP).
	  (if (and (consp elt)
		   (eq (car elt) group)
		   (null (cdr elt)))
	      (setq some-match t))
	;; If GROUP is a list, ELT must match it or some initial segment of it.
	(let ((tem1 group)
	      (tem2 elt)
	      (match t))
	  ;; Check elements of ELT until we run out of them.
	  (while tem2
	    (if (not (equal (car tem1) (car tem2)))
		(setq match nil))
	    (setq tem1 (cdr tem1)
		  tem2 (cdr tem2)))
	  ;; If ELT is an initial segment of GROUP, MATCH is t now.
	  ;; So set SOME-MATCH.
	  (if match
	      (setq some-match t)))))
    ;; If some element of SUPPRESS-LIST matched,
    ;; we return t.
    some-match))

(defun display-warning (group message &optional level buffer-name)
  "Display a warning message, MESSAGE.
GROUP should be a custom group name (a symbol).
or else a list of symbols whose first element is a custom group name.
\(The rest of the symbols represent subcategories, for warning purposes
only, and you can use whatever symbols you like.)

LEVEL should be either :warning, :error, or :emergency.
:emergency -- a problem that will seriously impair Emacs operation soon
	      if you do not attend to it promptly.
:error     -- data or circumstances that are inherently wrong.
:warning   -- data or circumstances that are not inherently wrong,
	      but raise suspicion of a possible problem.
:debug     -- info for debugging only.

BUFFER-NAME, if specified, is the name of the buffer for logging the
warning.  By default, it is `*Warnings*'.

See the `warnings' custom group for user customization features.

See also `warning-series', `warning-prefix-function' and
`warning-fill-prefix' for additional programming features."
  (unless level
    (setq level :warning))
  (if (assq level warning-level-aliases)
      (setq level (cdr (assq level warning-level-aliases))))
  (or (< (warning-numeric-level level)
	 (warning-numeric-level warning-minimum-log-level))
      (warning-suppress-p group warning-suppress-log)
      (let* ((groupname (if (consp group) (car group) group))
	     (buffer (get-buffer-create (or buffer-name "*Warnings*")))
	     (level-info (assq level warning-levels))
	     start end)
	(with-current-buffer buffer
	  (goto-char (point-max))
	  (when (and warning-series (symbolp warning-series))
	    (setq warning-series
		  (prog1 (point)
		    (unless (eq warning-series t)
		      (funcall warning-series)))))
	  (unless (bolp)
	    (newline))
	  (setq start (point))
	  (if warning-prefix-function
	      (setq level-info (funcall warning-prefix-function
					level level-info)))
	  (insert (nth 1 level-info) message)
	  (newline)
	  (when (and warning-fill-prefix (not (string-match "\n" message)))
	    (let ((fill-prefix warning-fill-prefix)
		  (fill-column 78))
	      (fill-region start (point))))
	  (setq end (point))
	  (when warning-series
	    (goto-char warning-series)))
	(if (nth 2 level-info)
	    (funcall (nth 2 level-info)))
	(if noninteractive
	    ;; Noninteractively, take the text we inserted
	    ;; in the warnings buffer and print it.
	    ;; Do this unconditionally, since there is no way
	    ;; to view logged messages unless we output them.
	    (with-current-buffer buffer
	      (message "%s" (buffer-substring start end)))
	  ;; Interactively, decide whether the warning merits
	  ;; immediate display.
	  (or (< (warning-numeric-level level)
		 (warning-numeric-level warning-minimum-level)) 
	      (warning-suppress-p group warning-suppress)
	      (let ((window (display-buffer buffer)))
		(when warning-series
		  (set-window-start window warning-series))
		(sit-for 0)))))))

(defun lwarn (group level message &rest args)
  "Display a warning message made from (format MESSAGE ARGS...).
Aside from generating the message with `format',
this is equivalent to `display-message'.

GROUP should be a custom group name (a symbol).
or else a list of symbols whose first element is a custom group name.
\(The rest of the symbols represent subcategories and
can be whatever you like.)

LEVEL should be either :warning, :error, or :emergency.
:emergency -- a problem that will seriously impair Emacs operation soon
	      if you do not attend to it promptly.
:error     -- invalid data or circumstances.
:warning   -- suspicious data or circumstances."
  (display-warning group (apply 'format message args) level))

(defun warn (message &rest args)
  "Display a warning message made from (format MESSAGE ARGS...).
Aside from generating the message with `format',
this is equivalent to `display-message', using
`emacs' as the group and `:warning' as the level."
  (display-warning 'emacs (apply 'format message args)))

;;; warnings.el ends here
