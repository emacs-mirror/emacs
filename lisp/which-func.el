;;; which-func.el --- print current function in mode line

;; Copyright (C) 1994, 1997, 1998, 2001 Free Software Foundation, Inc.

;; Author:   Alex Rezinsky <alexr@msil.sps.mot.com>
;;           (doesn't seem to be responsive any more)
;; Keywords: mode-line, imenu, tools

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

;; This package prints name of function where your current point is
;; located in mode line. It assumes that you work with imenu package
;; and imenu--index-alist is up to date.

;; KNOWN BUGS
;; ----------
;; Really this package shows not "function where the current point is
;; located now", but "nearest function which defined above the current
;; point". So if your current point is located after end of function
;; FOO but before begin of function BAR, FOO will be displayed in mode
;; line.
;; - if two windows display the same buffer, both windows
;;   show the same `which-func' information.

;; TODO LIST
;; ---------
;;     1. Dependence on imenu package should be removed.  Separate
;; function determination mechanism should be used to determine the end
;; of a function as well as the beginning of a function.
;;     2. This package should be realized with the help of overlay
;; properties instead of imenu--index-alist variable.

;;; History:

;; THANKS TO
;; ---------
;; Per Abrahamsen   <abraham@iesd.auc.dk>
;;     Some ideas (inserting  in mode-line,  using of post-command  hook
;;     and toggling this  mode) have  been   borrowed from  his  package
;;     column.el
;; Peter Eisenhauer <pipe@fzi.de>
;;     Bug fixing in case nested indexes.
;; Terry Tateyama   <ttt@ursa0.cs.utah.edu>
;;     Suggestion to use find-file-hooks for first imenu
;;     index building.

;;; Code:

;; Variables for customization
;; ---------------------------
;;  
(defvar which-func-unknown "???"
  "String to display in the mode line when current function is unknown.")

(defgroup which-func nil
  "Mode to display the current function name in the modeline."
  :group 'tools
  :version "20.3")

(defcustom which-func-modes
  '(emacs-lisp-mode c-mode c++-mode perl-mode cperl-mode makefile-mode
		    sh-mode fortran-mode)
  "List of major modes for which Which Function mode should be used.
For other modes it is disabled.  If this is equal to t,
then Which Function mode is enabled in any major mode that supports it."
  :group 'which-func
  :type '(choice (const :tag "All modes" t)
		 (repeat (symbol :tag "Major mode"))))

(defcustom which-func-non-auto-modes nil
  "List of major modes where Which Function mode is inactive till Imenu is used.
This means that Which Function mode won't really do anything
until you use Imenu, in these modes.  Note that files
larger than `which-func-maxout' behave in this way too;
Which Function mode doesn't do anything until you use Imenu."
  :group 'which-func
  :type '(repeat (symbol :tag "Major mode")))

(defcustom which-func-maxout 500000
  "Don't automatically compute the Imenu menu if buffer is this big or bigger.
Zero means compute the Imenu menu regardless of size."
  :group 'which-func
  :type 'integer)

(defcustom which-func-format '("[" which-func-current "]")
  "Format for displaying the function in the mode line."
  :group 'which-func
  :type 'sexp)

(defvar which-func-cleanup-function nil
  "Function to transform a string before displaying it in the mode line.
The function is called with one argument, the string to display.
Its return value is displayed in the modeline.
If nil, no function is called.  The default value is nil.

This feature can be useful if Imenu is set up to make more
detailed entries (e.g., containing the argument list of a function),
and you want to simplify them for the mode line
\(e.g., removing the parameter list to just have the function name.)")

;;; Code, nothing to customize below here
;;; -------------------------------------
;;;
(require 'imenu)

(defvar which-func-current  which-func-unknown)
(defvar which-func-previous which-func-unknown)
(make-variable-buffer-local 'which-func-current)
(make-variable-buffer-local 'which-func-previous)

(defvar which-func-mode nil
  "Non-nil means display current function name in mode line.
This makes a difference only if `which-function-mode' is non-nil.")
(make-variable-buffer-local 'which-func-mode)
;;(put 'which-func-mode 'permanent-local t)

(add-hook 'find-file-hooks 'which-func-ff-hook t)

(defun which-func-ff-hook ()
  "File find hook for Which Function mode.
It creates the Imenu index for the buffer, if necessary."
  (setq which-func-mode
	(and which-function-mode
	     (or (eq which-func-modes t)
		 (member major-mode which-func-modes))))

  (condition-case nil
      (if (and which-func-mode
	       (not (member major-mode which-func-non-auto-modes))
	       (or (null which-func-maxout)
		   (< buffer-saved-size which-func-maxout)
		   (= which-func-maxout 0)))
	  (setq imenu--index-alist
		(save-excursion (funcall imenu-create-index-function))))
    (error
     (setq which-func-mode nil))))

(defun which-func-update ()
  "Update the Which-Function mode display for all windows."
  (walk-windows 'which-func-update-1 nil 'visible))

(defun which-func-update-1 (window)
  "Update the Which-Function mode display for window WINDOW."
  (save-selected-window
    (select-window window)
    ;; Update the string containing the current function.
    (when which-func-mode
      (condition-case info
	  (progn
	    (setq which-func-current (or (which-function) which-func-unknown))
	    (unless (string= which-func-current which-func-previous)
	      (force-mode-line-update)
	      (setq which-func-previous which-func-current)))
	(error
	 (which-func-mode -1)
	 (error "Error in which-func-update: %s" info))))))

;;;###autoload
(defalias 'which-func-mode 'which-function-mode)

;; This is the name people would normally expect.
;;;###autoload
(define-minor-mode which-function-mode
  "Toggle Which Function mode, globally.
When Which Function mode is enabled, the current function name is
continuously displayed in the mode line, in certain major modes.

With prefix ARG, turn Which Function mode on iff arg is positive,
and off otherwise."
  :global t :group 'which-func
  (if which-function-mode
      ;;Turn it on
      (progn
        (add-hook 'post-command-idle-hook 'which-func-update)
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (setq which-func-mode
                  (or (eq which-func-modes t)
                      (member major-mode which-func-modes))))))
    ;; Turn it off
    (remove-hook 'post-command-idle-hook 'which-func-update)
    (dolist (buf (buffer-list))
      (with-current-buffer buf (setq which-func-mode nil)))))

(defvar which-function-imenu-failed nil
  "Locally t in a buffer if `imenu--make-index-alist' found nothing there.")

(defun which-function ()
  "Return current function name based on point.
Uses `imenu--index-alist' or `add-log-current-defun-function'.
If no function name is found, return nil."
  (let (name)
    ;; If Imenu is loaded, try to make an index alist with it.
    (when (and (boundp 'imenu--index-alist) (null imenu--index-alist)
	       (null which-function-imenu-failed))
      (imenu--make-index-alist)
      (unless imenu--index-alist
	(make-local-variable 'which-function-imenu-failed)
	(setq which-function-imenu-failed t)))
    ;; If we have an index alist, use it.
    (when (and (boundp 'imenu--index-alist) imenu--index-alist)
      (let ((pair (car-safe imenu--index-alist))
	    (rest (cdr-safe imenu--index-alist)))
	(while (and (or rest pair)
		    (or (not (number-or-marker-p (cdr pair)))
			(> (point) (cdr pair))))
	  (setq name (car pair))
	  (setq pair (car-safe rest))
	  (setq rest (cdr-safe rest)))))
    ;; Try using add-log support.
    (when (and (null name) (boundp 'add-log-current-defun-function)
	       add-log-current-defun-function)
      (setq name (funcall add-log-current-defun-function)))
    ;; Filter the name if requested.
    (when name
      (if which-func-cleanup-function
	  (funcall which-func-cleanup-function name)
	name))))

(provide 'which-func)

;;; which-func.el ends here
