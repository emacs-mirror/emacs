;;;_. icomplete.el - minibuffer completion incremental feedback

;;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.

;;; Author: Ken Manheimer <klm@nist.gov>
;;; Maintainer: Ken Manheimer <klm@nist.gov>
;;; Version: Id: icomplete.el,v 4.3 1994/08/31 18:48:29 klm Exp 
;;; Created: Mar 1993 klm@nist.gov - first release to usenet
;;; Keywords: help, abbrev

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

;;; Loading this package implements a more fine-grained minibuffer
;;; completion feedback scheme.  Prospective completions are concisely
;;; indicated within the minibuffer itself, with each successive
;;; keystroke.

;;; See 'icomplete-completions' docstring for a description of the
;;; icomplete display format.

;;; See the `icomplete-minibuffer-setup-hook' docstring for a means to
;;; customize icomplete setup for interoperation with other
;;; minibuffer-oriented packages.

;;; To activate icomplete mode, simply load the package.  You can
;;; subsequently deactivate it by invoking the function icomplete-mode
;;; with a negative prefix-arg (C-U -1 ESC-x icomplete-mode).  Also,
;;; you can prevent activation of the mode during package load by
;;; first setting the variable `icomplete-mode' to nil.  Icompletion
;;; can be enabled any time after the package is loaded by invoking
;;; icomplete-mode without a prefix arg.

;;; Thanks to everyone for their suggestions for refinements of this
;;; package.  I particularly have to credit Michael Cook, who
;;; implemented an incremental completion style in his 'iswitch'
;;; functions that served as a model for icomplete.  Some other
;;; contributors: Noah Freidman (restructuring as minor mode), Colin
;;; Rafferty (lemacs reconciliation), Lars Lindberg, RMS, and
;;; others.

;;; klm.

;;; Code:

;;;_* Provide
(provide 'icomplete)

;;;_* User Customization variables

;;;_* Initialization
;;;_  = icomplete-minibuffer-setup-hook
(defvar icomplete-minibuffer-setup-hook nil
  "*Icomplete-specific customization of minibuffer setup.

This hook is run during minibuffer setup iff icomplete will be active.
It is intended for use in customizing icomplete for interoperation
with other packages.  For instance:

  \(add-hook 'icomplete-minibuffer-setup-hook 
	    \(function
	     \(lambda ()
	       \(make-local-variable 'resize-minibuffer-window-max-height)
	       \(setq resize-minibuffer-window-max-height 3))))

will constrain rsz-mini to a maximum minibuffer height of 3 lines when
icompletion is occurring.")

;;;_ + Internal Variables
;;;_  = icomplete-mode
(defvar icomplete-mode t
  "Non-nil enables incremental minibuffer completion, once
`\\[icomplete-mode]' function has set things up.")
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

;;;_ > icomplete-mode (&optional prefix)
;;;###autoload
(defun icomplete-mode (&optional prefix)
  "Activate incremental minibuffer completion for this emacs session,
or deactivate with negative prefix arg."
  (interactive "p")
  (or prefix (setq prefix 0))
  (cond ((>= prefix 0)
	 (setq icomplete-mode t)
	 ;; The following is not really necessary after first time -
	 ;; no great loss.
	 (add-hook 'minibuffer-setup-hook 'icomplete-minibuffer-setup))
	(t (setq icomplete-mode nil))))

;;;_ > icomplete-simple-completing-p ()
(defun icomplete-simple-completing-p ()

  "Non-nil if current window is minibuffer that's doing simple completion.

Conditions are:
   the selected window is a minibuffer,
   and not in the middle of macro execution,
   and minibuffer-completion-table is not a symbol (which would
       indicate some non-standard, non-simple completion mechansm,
       like file-name and other custom-func completions)."

  (and (window-minibuffer-p (selected-window))
       (not executing-macro)
       (not (symbolp minibuffer-completion-table))))
;;;_ > icomplete-minibuffer-setup ()
;;;###autoload
(defun icomplete-minibuffer-setup ()

  "Run in minibuffer on activation to establish incremental completion.

Usually run by inclusion in minibuffer-setup-hook."

  (cond ((and icomplete-mode (icomplete-simple-completing-p))
	 (make-local-variable 'pre-command-hook)
	 (setq pre-command-hook (copy-sequence pre-command-hook))
	 (add-hook 'pre-command-hook
		   (function (lambda ()
			       (run-hooks 'icomplete-pre-command-hook))))
	 (make-local-variable 'post-command-hook)
	 (setq post-command-hook (copy-sequence post-command-hook))
	 (add-hook 'post-command-hook
		   (function (lambda ()
			       (run-hooks 'icomplete-post-command-hook))))
	 (run-hooks 'icomplete-minibuffer-setup-hook))))

;;;_* Completion

;;;_ > icomplete-tidy ()
(defun icomplete-tidy ()
  "Remove completions display \(if any) prior to new user input.

Should be run in on the minibuffer pre-command-hook.  See `icomplete-mode'
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

Should be run via minibuffer post-command-hook.  See `icomplete-mode'
and `minibuffer-setup-hook'."
  (if (icomplete-simple-completing-p)
      (let ((contents (buffer-substring (point-min)(point-max)))
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
	  (if (> (point-max) 1)
	      (insert-string
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
          further input is required to distingish a single one.

The displays for disambiguous matches have \" [Matched]\" appended
\(whether complete or not), or \" \[No matches]\", if no eligible
matches exist."

  (let ((comps (all-completions name candidates predicate))
                                        ; "-determined" - only one candidate
        (open-bracket-determined (if require-match "(" "["))
        (close-bracket-determined (if require-match ")" "]"))
                                        ;"-prospects" - more than one candidate
        (open-bracket-prospects "{")
        (close-bracket-prospects "}")
        )
    (cond ((null comps) (format " %sNo matches%s"
                                open-bracket-determined
                                close-bracket-determined))
          ((null (cdr comps))           ;one match
           (concat (if (and (> (length (car comps))
                               (length name)))
                       (concat open-bracket-determined
                               (substring (car comps) (length name))
                               close-bracket-determined)
                     "")
                   " [Matched]"))
          (t                            ;multiple matches
           (let* ((most (try-completion name candidates predicate))
                  (most-len (length most))
                  most-is-exact
                  (alternatives
                   (apply
                    (function concat)
                    (cdr (apply
			  (function nconc)
			  (mapcar '(lambda (com)
				     (if (= (length com) most-len)
					 ;; Most is one exact match,
					 ;; note that and leave out
					 ;; for later indication:
					 (progn
					   (setq most-is-exact t)
					   ())
				       (list ","
					     (substring com
							most-len))))
				  comps))))))
             (concat (and (> most-len (length name))
                          (concat open-bracket-determined
                                  (substring most (length name))
                                  close-bracket-determined))
                     open-bracket-prospects
                     (if most-is-exact
                         (concat "," alternatives)
                       alternatives)
                     close-bracket-prospects))))))

;;;_ + Initialization
;;; If user hasn't setq-default icomplete-mode to nil, then setup for
;;; activation:
(if icomplete-mode
    (icomplete-mode))


;;;_* Local emacs vars.
;;;Local variables:
;;;outline-layout: (-2 :)
;;;End:

;;; icomplete.el ends here

