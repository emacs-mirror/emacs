;;; icomplete.el --- minibuffer completion incremental feedback

;; Copyright (C) 1992, 1993, 1994, 1997, 1999, 2001
;;;  Free Software Foundation, Inc.

;; Author: Ken Manheimer <klm@i.am>
;; Maintainer: Ken Manheimer <klm@i.am>
;; Created: Mar 1993 Ken Manheimer, klm@nist.gov - first release to usenet
;; Last update: Ken Manheimer <klm@i.am>, 11/18/1999.
;; Keywords: help, abbrev

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

;; Loading this package implements a more fine-grained minibuffer
;; completion feedback scheme.  Prospective completions are concisely
;; indicated within the minibuffer itself, with each successive
;; keystroke.

;; See `icomplete-completions' docstring for a description of the
;; icomplete display format.

;; See the `icomplete-minibuffer-setup-hook' docstring for a means to
;; customize icomplete setup for interoperation with other
;; minibuffer-oriented packages.

;; To activate icomplete mode, load the package and use the
;; `icomplete-mode' function.  You can subsequently deactivate it by
;; invoking the function icomplete-mode with a negative prefix-arg
;; (C-U -1 ESC-x icomplete-mode).  Also, you can prevent activation of
;; the mode during package load by first setting the variable
;; `icomplete-mode' to nil.  Icompletion can be enabled any time after
;; the package is loaded by invoking icomplete-mode without a prefix
;; arg.

;; Thanks to everyone for their suggestions for refinements of this
;; package.  I particularly have to credit Michael Cook, who
;; implemented an incremental completion style in his 'iswitch'
;; functions that served as a model for icomplete.  Some other
;; contributors: Noah Friedman (restructuring as minor mode), Colin
;; Rafferty (lemacs reconciliation), Lars Lindberg, RMS, and others.

;; klm.

;;; Code:

;;;_* Provide
(provide 'icomplete)


(defgroup icomplete nil
  "Show completions dynamically in minibuffer."
  :prefix "icomplete-"
  :group 'minibuffer)

(defcustom icomplete-mode nil
  "*Toggle incremental minibuffer completion.
As text is typed into the minibuffer, prospective completions are indicated 
in the minibuffer.
Setting this variable directly does not take effect;
use either \\[customize] or the function `icomplete-mode'."
  :set (lambda (symbol value)
	 (icomplete-mode (if value 1 -1)))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'icomplete
  :require 'icomplete)

;;;_* User Customization variables
(defcustom icomplete-prospects-length 80
  "*Length of string displaying the prospects."
  :type 'integer
  :group 'icomplete)

(defcustom icomplete-compute-delay .3
  "*Completions-computation stall, used only with large-number
completions - see `icomplete-delay-completions-threshold'."
  :type 'number
  :group 'icomplete)

(defcustom icomplete-delay-completions-threshold 400
  "*Pending-completions number over which to apply icomplete-compute-delay."
  :type 'integer
  :group 'icomplete)

(defcustom icomplete-max-delay-chars 3
  "*Maximum number of initial chars to apply icomplete compute delay."
  :type 'integer
  :group 'icomplete)

(defcustom icomplete-show-key-bindings t
  "*If non-nil, show key bindings as well as completion for sole matches."
  :type 'boolean
  :group 'icomplete)

(defcustom icomplete-minibuffer-setup-hook nil
  "*Icomplete-specific customization of minibuffer setup.

This hook is run during minibuffer setup iff icomplete will be active.
It is intended for use in customizing icomplete for interoperation
with other features and packages.  For instance:

  \(add-hook 'icomplete-minibuffer-setup-hook
	    \(function
	     \(lambda ()
	       \(make-local-variable 'max-mini-window-height)
	       \(setq max-mini-window-height 3))))

will constrain Emacs to a maximum minibuffer height of 3 lines when
icompletion is occurring."
  :type 'hook
  :group 'icomplete)


;;;_* Initialization

;;;_ + Internal Variables
;;;_  = icomplete-eoinput 1
(defvar icomplete-eoinput 1
  "Point where minibuffer input ends and completion info begins.")
(make-variable-buffer-local 'icomplete-eoinput)
;;;_  = icomplete-pre-command-hook
(defvar icomplete-pre-command-hook nil
  "Incremental-minibuffer-completion pre-command-hook.

Is run in minibuffer before user input when `icomplete-mode' is non-nil.
Use `icomplete-mode' function to set it up properly for incremental
minibuffer completion.")
(add-hook 'icomplete-pre-command-hook 'icomplete-tidy)
;;;_  = icomplete-post-command-hook
(defvar icomplete-post-command-hook nil
  "Incremental-minibuffer-completion post-command-hook.

Is run in minibuffer after user input when `icomplete-mode' is non-nil.
Use `icomplete-mode' function to set it up properly for incremental
minibuffer completion.")
(add-hook 'icomplete-post-command-hook 'icomplete-exhibit)

(defun icomplete-get-keys (func-name)
  "Return strings naming keys bound to `func-name', or nil if none.
Examines the prior, not current, buffer, presuming that current buffer
is minibuffer."
  (if (commandp func-name)
    (save-excursion
      (let* ((sym (intern func-name))
	     (buf (other-buffer))
	     (map (save-excursion (set-buffer buf) (current-local-map)))
	     (keys (where-is-internal sym map)))
	(if keys
	    (concat "<"
		    (mapconcat 'key-description
			       (sort keys
				     #'(lambda (x y)
					 (< (length x) (length y))))
			       ", ")
		    ">"))))))

;;;_ > icomplete-mode (&optional prefix)
;;;###autoload
(defun icomplete-mode (&optional arg)
  "Toggle incremental minibuffer completion for this Emacs session.
With a numeric argument, turn Icomplete mode on iff ARG is positive."
  (interactive "P")
  (let ((on-p (if (null arg)
		  (not icomplete-mode)
		(> (prefix-numeric-value arg) 0))))
    (setq icomplete-mode on-p)
    (when on-p
      ;; The following is not really necessary after first time -
      ;; no great loss.
      (add-hook 'minibuffer-setup-hook 'icomplete-minibuffer-setup))))

;;;_ > icomplete-simple-completing-p ()
(defun icomplete-simple-completing-p ()
  "Non-nil if current window is minibuffer that's doing simple completion.

Conditions are:
   the selected window is a minibuffer,
   and not in the middle of macro execution,
   and minibuffer-completion-table is not a symbol (which would
       indicate some non-standard, non-simple completion mechanism,
       like file-name and other custom-func completions)."

  (and (window-minibuffer-p (selected-window))
       (not executing-kbd-macro)
       (not (symbolp minibuffer-completion-table))))

;;;_ > icomplete-minibuffer-setup ()
;;;###autoload
(defun icomplete-minibuffer-setup ()
  "Run in minibuffer on activation to establish incremental completion.
Usually run by inclusion in `minibuffer-setup-hook'."
  (cond ((and icomplete-mode (icomplete-simple-completing-p))
	 (add-hook 'pre-command-hook
		   (function (lambda ()
			       (run-hooks 'icomplete-pre-command-hook)))
		   nil t)
	 (add-hook 'post-command-hook
		   (function (lambda ()
			       (run-hooks 'icomplete-post-command-hook)))
		   nil t)
	 (run-hooks 'icomplete-minibuffer-setup-hook))))
;


;;;_* Completion

;;;_ > icomplete-tidy ()
(defun icomplete-tidy ()
  "Remove completions display \(if any) prior to new user input.
Should be run in on the minibuffer `pre-command-hook'.  See `icomplete-mode'
and `minibuffer-setup-hook'."
  (if (icomplete-simple-completing-p)
      (if (and (boundp 'icomplete-eoinput)
	       icomplete-eoinput)

	  (if (> icomplete-eoinput (point-max))
	      ;; Oops, got rug pulled out from under us - reinit:
	      (setq icomplete-eoinput (point-max))
	    (let ((buffer-undo-list buffer-undo-list )) ; prevent entry
	      (delete-region icomplete-eoinput (point-max))))

	;; Reestablish the local variable 'cause minibuffer-setup is weird:
	(make-local-variable 'icomplete-eoinput)
	(setq icomplete-eoinput 1))))

;;;_ > icomplete-exhibit ()
(defun icomplete-exhibit ()
  "Insert icomplete completions display.
Should be run via minibuffer `post-command-hook'.  See `icomplete-mode'
and `minibuffer-setup-hook'."
  (if (icomplete-simple-completing-p)
      (let ((contents (buffer-substring (minibuffer-prompt-end)(point-max)))
	    (buffer-undo-list t))
	(save-excursion
	  (goto-char (point-max))
                                        ; Register the end of input, so we
                                        ; know where the extra stuff
                                        ; (match-status info) begins:
	  (if (not (boundp 'icomplete-eoinput))
	      ;; In case it got wiped out by major mode business:
	      (make-local-variable 'icomplete-eoinput))
	  (setq icomplete-eoinput (point))
                                        ; Insert the match-status information:
	  (if (and (> (point-max) 1)
		   (or
		    ;; Don't bother with delay after certain number of chars:
		    (> (point-max) icomplete-max-delay-chars)
		    ;; Don't delay if alternatives number is small enough:
		    (if minibuffer-completion-table
			(cond ((numberp minibuffer-completion-table)
			       (< minibuffer-completion-table
				  icomplete-delay-completions-threshold))
			      ((sequencep minibuffer-completion-table)
			       (< (length minibuffer-completion-table)
				  icomplete-delay-completions-threshold))
			      ))
		    ;; Delay - give some grace time for next keystroke, before
		    ;; embarking on computing completions:
		    (sit-for icomplete-compute-delay)))
	      (insert
	       (icomplete-completions contents
				      minibuffer-completion-table
				      minibuffer-completion-predicate
				      (not
				       minibuffer-completion-confirm))))))))

;;;_ > icomplete-completions (name candidates predicate require-match)
(defun icomplete-completions (name candidates predicate require-match)
  "Identify prospective candidates for minibuffer completion.

The display is updated with each minibuffer keystroke during
minibuffer completion.

Prospective completion suffixes (if any) are displayed, bracketed by
one of \(), \[], or \{} pairs.  The choice of brackets is as follows:

  \(...) - a single prospect is identified and matching is enforced,
  \[...] - a single prospect is identified but matching is optional, or
  \{...} - multiple prospects, separated by commas, are indicated, and
          further input is required to distinguish a single one.

The displays for unambiguous matches have ` [Matched]' appended
\(whether complete or not), or ` \[No matches]', if no eligible
matches exist.  \(Keybindings for uniquely matched commands
are exhibited within the square braces.)"

  ;; 'all-completions' doesn't like empty
  ;; minibuffer-completion-table's (ie: (nil))
  (if (and (listp candidates) (null (car candidates)))
      (setq candidates nil))

  (let ((comps (all-completions name candidates predicate))
                                        ; "-determined" - only one candidate
        (open-bracket-determined (if require-match "(" "["))
        (close-bracket-determined (if require-match ")" "]")))
    ;; `concat'/`mapconcat' is the slow part.  With the introduction of
    ;; `icomplete-prospects-length', there is no need for `catch'/`throw'.
    (if (null comps) (format " %sNo matches%s"
			     open-bracket-determined
			     close-bracket-determined)
      (let* ((most-try (try-completion name (mapcar (function list) comps)))
	     (most (if (stringp most-try) most-try (car comps)))
	     (most-len (length most))
	     (determ (and (> most-len (length name))
			  (concat open-bracket-determined
				  (substring most (length name))
				  close-bracket-determined)))
	     (open-bracket-prospects "{")
	     (close-bracket-prospects "}")
                                        ;"-prospects" - more than one candidate
	     (prospects-len 0)
	     prospects most-is-exact comp)
	(if (eq most-try t)
	    (setq prospects nil)
	  (while (and comps (< prospects-len icomplete-prospects-length))
	    (setq comp (substring (car comps) most-len)
		  comps (cdr comps))
	    (cond ((string-equal comp "") (setq most-is-exact t))
		  ((member comp prospects))
		  (t (setq prospects (cons comp prospects)
			   prospects-len (+ (length comp) 1 prospects-len))))))
	(if prospects
	    (concat determ
		    open-bracket-prospects
		    (and most-is-exact ",")
		    (mapconcat 'identity
			       (sort prospects (function string-lessp))
			       ",")
		    (and comps ",...")
		    close-bracket-prospects)
	  (concat determ
		  " [Matched"
		  (let ((keys (and icomplete-show-key-bindings
				   (commandp (intern-soft most))
				   (icomplete-get-keys most))))
		    (if keys
			(concat "; " keys)
		      ""))
		  "]"))))))

(if icomplete-mode
    (icomplete-mode 1))

;;;_* Local emacs vars.
;;;Local variables:
;;;outline-layout: (-2 :)
;;;End:

;;; icomplete.el ends here
