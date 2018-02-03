;;; map-ynp.el --- general-purpose boolean question-asker  -*- lexical-binding:t -*-

;; Copyright (C) 1991-1995, 2000-2018 Free Software Foundation, Inc.

;; Author: Roland McGrath <roland@gnu.org>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: lisp, extensions
;; Package: emacs

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

;; map-y-or-n-p is a general-purpose question-asking function.
;; It asks a series of y/n questions (a la y-or-n-p), and decides to
;; apply an action to each element of a list based on the answer.
;; The nice thing is that you also get some other possible answers
;; to use, reminiscent of query-replace: ! to answer y to all remaining
;; questions; ESC or q to answer n to all remaining questions; . to answer
;; y once and then n for the remainder; and you can get help with C-h.

;;; Code:

(declare-function x-popup-dialog "menu.c" (position contents &optional header))

(defun map-y-or-n-p (prompter actor list &optional help action-alist
			      no-cursor-in-echo-area)
  "Ask a series of boolean questions.
Takes args PROMPTER ACTOR LIST, and optional args HELP and ACTION-ALIST.

LIST is a list of objects, or a function of no arguments to return the next
object or nil.

If PROMPTER is a string, the prompt is \(format PROMPTER OBJECT).  If not
a string, PROMPTER is a function of one arg (an object from LIST), which
returns a string to be used as the prompt for that object.  If the return
value is not a string, it may be nil to ignore the object or non-nil to act
on the object without asking the user.

ACTOR is a function of one arg (an object from LIST),
which gets called with each object that the user answers `yes' for.

If HELP is given, it is a list (OBJECT OBJECTS ACTION),
where OBJECT is a string giving the singular noun for an elt of LIST;
OBJECTS is the plural noun for elts of LIST, and ACTION is a transitive
verb describing ACTOR.  The default is \(\"object\" \"objects\" \"act on\").

At the prompts, the user may enter y, Y, or SPC to act on that object;
n, N, or DEL to skip that object; ! to act on all following objects;
ESC or q to exit (skip all following objects); . (period) to act on the
current object and then exit; or \\[help-command] to get help.

If ACTION-ALIST is given, it is an alist (KEY FUNCTION HELP) of extra keys
that will be accepted.  KEY is a character; FUNCTION is a function of one
arg (an object from LIST); HELP is a string.  When the user hits KEY,
FUNCTION is called.  If it returns non-nil, the object is considered
\"acted upon\", and the next object from LIST is processed.  If it returns
nil, the prompt is repeated for the same object.

Final optional argument NO-CURSOR-IN-ECHO-AREA non-nil says not to set
`cursor-in-echo-area' while prompting.

This function uses `query-replace-map' to define the standard responses,
but not all of the responses which `query-replace' understands
are meaningful here.

Returns the number of actions taken."
  (let* ((actions 0)
	 user-keys mouse-event map prompt char elt def
	 ;; Non-nil means we should use mouse menus to ask.
	 use-menus
	 delayed-switch-frame
         ;; Rebind other-window-scroll-buffer so that subfunctions can set
         ;; it temporarily, without risking affecting the caller.
         (other-window-scroll-buffer other-window-scroll-buffer)
	 (next (if (functionp list)
                   (lambda () (setq elt (funcall list)))
                 (lambda () (when list
			      (setq elt (pop list))
			      t))))
	 (try-again (lambda ()
		      (let ((x next))
			(setq next (lambda () (setq next x) elt))))))
    (if (and (listp last-nonmenu-event)
	     use-dialog-box)
	;; Make a list describing a dialog box.
	(let ((objects (if help (capitalize (nth 1 help))))
	      (action (if help (capitalize (nth 2 help)))))
	  (setq map `(("Yes" . act) ("No" . skip)
		      ,@(mapcar (lambda (elt)
				  (cons (with-syntax-table
					    text-mode-syntax-table
					  (capitalize (nth 2 elt)))
					(vector (nth 1 elt))))
				action-alist)
		      (,(if help (concat action " This But No More")
			  "Do This But No More") . act-and-exit)
		      (,(if help (concat action " All " objects)
			  "Do All") . automatic)
		      ("No For All" . exit))
		use-menus t
		mouse-event last-nonmenu-event))
      (setq user-keys (if action-alist
			  (concat (mapconcat (lambda (elt)
                                               (key-description
                                                (vector (car elt))))
					     action-alist ", ")
				  " ")
			"")
	    ;; Make a map that defines each user key as a vector containing
	    ;; its definition.
	    map
            (let ((map (make-sparse-keymap)))
              (set-keymap-parent map query-replace-map)
              (dolist (elt action-alist)
                (define-key map (vector (car elt)) (vector (nth 1 elt))))
              map)))
    (unwind-protect
	(progn
	  (if (stringp prompter)
	      (setq prompter (let ((prompter prompter))
			       (lambda (object)
				 (format prompter object)))))
	  (while (funcall next)
	    (setq prompt (funcall prompter elt))
	    (cond ((stringp prompt)
		   ;; Prompt the user about this object.
		   (setq quit-flag nil)
		   (if use-menus
		       (setq def (or (x-popup-dialog (or mouse-event use-menus)
						     (cons prompt map))
				     'quit))
		     ;; Prompt in the echo area.
		     (let ((cursor-in-echo-area (not no-cursor-in-echo-area)))
		       (message (apply 'propertize "%s(y, n, !, ., q, %sor %s) "
				       minibuffer-prompt-properties)
				prompt user-keys
				(key-description (vector help-char)))
		       (if minibuffer-auto-raise
			   (raise-frame (window-frame (minibuffer-window))))
		       (while (progn
				(setq char (read-event))
				;; If we get -1, from end of keyboard
				;; macro, try again.
                                (equal char -1)))
		       ;; Show the answer to the question.
		       (message "%s(y, n, !, ., q, %sor %s) %s"
				prompt user-keys
				(key-description (vector help-char))
				(single-key-description char)))
		     (setq def (lookup-key map (vector char))))
		   (cond ((eq def 'exit)
			  (setq next (lambda () nil)))
			 ((eq def 'act)
			  ;; Act on the object.
			  (funcall actor elt)
			  (setq actions (1+ actions)))
			 ((eq def 'skip)
			  ;; Skip the object.
			  )
			 ((eq def 'act-and-exit)
			  ;; Act on the object and then exit.
			  (funcall actor elt)
			  (setq actions (1+ actions)
				next (lambda () nil)))
			 ((eq def 'quit)
			  (setq quit-flag t)
			  (funcall try-again))
			 ((eq def 'automatic)
			  ;; Act on this and all following objects.
			  (if (funcall prompter elt)
			      (progn
				(funcall actor elt)
				(setq actions (1+ actions))))
			  (while (funcall next)
			    (if (funcall prompter elt)
				(progn
				  (funcall actor elt)
				  (setq actions (1+ actions))))))
			 ((eq def 'help)
                          (with-help-window (help-buffer)
			    (princ
                             (let ((object  (or (nth 0 help) "object"))
                                   (objects (or (nth 1 help) "objects"))
                                   (action  (or (nth 2 help) "act on")))
			       (concat
                                (format-message
                                 "\
Type SPC or `y' to %s the current %s;
DEL or `n' to skip the current %s;
RET or `q' to skip the current and all remaining %s;
C-g to quit (cancel the whole command);
! to %s all remaining %s;\n"
                                 action object object objects action objects)
                                (mapconcat (lambda (elt)
                                             (format "%s to %s;\n"
                                                     (single-key-description
                                                      (nth 0 elt))
                                                     (nth 2 elt)))
					   action-alist
                                           "")
                                (format
                                 "or . (period) to %s the current %s and exit."
                                 action object)))))

			  (funcall try-again))
			 ((and (symbolp def) (commandp def))
			  (call-interactively def)
			  ;; Regurgitated; try again.
			  (funcall try-again))
			 ((vectorp def)
			  ;; A user-defined key.
			  (if (funcall (aref def 0) elt) ;Call its function.
			      ;; The function has eaten this object.
			      (setq actions (1+ actions))
			    ;; Regurgitated; try again.
			    (funcall try-again)))
			 ((and (consp char)
			       (eq (car char) 'switch-frame))
			  ;; switch-frame event.  Put it off until we're done.
			  (setq delayed-switch-frame char)
			  (funcall try-again))
			 (t
			  ;; Random char.
			  (message "Type %s for help."
				   (key-description (vector help-char)))
			  (beep)
			  (sit-for 1)
			  (funcall try-again))))
		  (prompt
		   (funcall actor elt)
		   (setq actions (1+ actions))))))
      (if delayed-switch-frame
	  (setq unread-command-events
		(cons delayed-switch-frame unread-command-events))))
    ;; Clear the last prompt from the minibuffer.
    (let ((message-log-max nil))
      (message ""))
    ;; Return the number of actions that were taken.
    actions))


;; read-answer is a general-purpose question-asker that supports
;; either long or short answers.

;; For backward compatibility check if short y/n answers are preferred.
(defcustom read-answer-short (eq (symbol-function 'yes-or-no-p) 'y-or-n-p)
  "If non-nil, accept short answers to the question."
  :type 'boolean
  :version "27.1"
  :group 'minibuffer)

(defconst read-answer-map--memoize (make-hash-table :weakness 'key :test 'equal))

(defun read-answer (question answers)
  "Read an answer either as a complete word or its character abbreviation.
Ask user a question and accept an answer from the list of possible answers.

QUESTION should end in a space; this function adds a list of answers to it.

ANSWERS is an alist with elements in the following format:
  (LONG-ANSWER SHORT-ANSWER HELP-MESSAGE)
where
  LONG-ANSWER is a complete answer,
  SHORT-ANSWER is an abbreviated one-character answer,
  HELP-MESSAGE is a string describing the meaning of the answer.

Example:
  \\='((\"yes\"  ?y \"perform the action\")
    (\"no\"   ?n \"skip to the next\")
    (\"all\"  ?! \"accept all remaining without more questions\")
    (\"help\" ?h \"show help\")
    (\"quit\" ?q \"exit\"))

When `read-answer-short' is non-nil, accept short answers.

Return a long answer even in case of accepting short ones.

When `use-dialog-box' is t, pop up a dialog window to get user input."
  (custom-reevaluate-setting 'read-answer-short)
  (let* ((short read-answer-short)
         (answers-with-help
          (if (assoc "help" answers)
              answers
            (append answers '(("help" ?? "show this help message")))))
         (answers-without-help
          (assoc-delete-all "help" (copy-alist answers-with-help)))
         (prompt
          (format "%s(%s) " question
                  (mapconcat (lambda (a)
                               (if short
                                   (format "%c" (nth 1 a))
                                 (nth 0 a)))
                             answers-with-help ", ")))
         (message
          (format "Please answer %s."
                  (mapconcat (lambda (a)
                               (format "`%s'" (if short
                                                  (string (nth 1 a))
                                                (nth 0 a))))
                             answers-with-help " or ")))
         (short-answer-map
          (when short
            (or (gethash answers read-answer-map--memoize)
                (puthash answers
                         (let ((map (make-sparse-keymap)))
                           (set-keymap-parent map minibuffer-local-map)
                           (dolist (a answers-with-help)
                             (define-key map (vector (nth 1 a))
                               (lambda ()
                                 (interactive)
                                 (delete-minibuffer-contents)
                                 (insert (nth 0 a))
                                 (exit-minibuffer))))
                           (define-key map [remap self-insert-command]
                             (lambda ()
                               (interactive)
                               (delete-minibuffer-contents)
                               (beep)
                               (message message)
                               (sleep-for 2)))
                           map)
                         read-answer-map--memoize))))
         answer)
    (while (not (assoc (setq answer (downcase
                                     (cond
                                      ((and (display-popup-menus-p)
                                            last-input-event ; not during startup
                                            (listp last-nonmenu-event)
                                            use-dialog-box)
                                       (x-popup-dialog
                                        t
                                        (cons question
                                              (mapcar (lambda (a)
                                                        (cons (capitalize (nth 0 a))
                                                              (nth 0 a)))
                                                      answers-with-help))))
                                      (short
                                       (read-from-minibuffer
                                        prompt nil short-answer-map nil
                                        'yes-or-no-p-history))
                                      (t
                                       (read-from-minibuffer
                                        prompt nil nil nil
                                        'yes-or-no-p-history)))))
                       answers-without-help))
      (if (string= answer "help")
          (with-help-window "*Help*"
            (with-current-buffer "*Help*"
              (insert "Type:\n"
                      (mapconcat
                       (lambda (a)
                         (format "`%s'%s to %s"
                                 (if short (string (nth 1 a)) (nth 0 a))
                                 (if short (format " (%s)" (nth 0 a)) "")
                                 (nth 2 a)))
                       answers-with-help ",\n")
                      ".\n")))
        (beep)
        (message message)
        (sleep-for 2)))
    answer))

;;; map-ynp.el ends here
