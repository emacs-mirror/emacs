;;; winner.el  --- Restore window configuration or change buffer

;; Copyright (C) 1997 Free Software Foundation. Inc.

;; Author: Ivar Rummelhoff <ivarr@ifi.uio.no>
;; Maintainer: Ivar Rummelhoff <ivarr@ifi.uio.no>
;; Created: 27 Feb 1997
;; Keywords: extensions,windows

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
;; 
;;   winner.el provides a minor mode (`winner-mode') that does
;;   essentially two things:
;;
;;     1) It keeps track of changing window configurations, so that
;;        when you wish to go back to a previous view, all you have
;;        to do is to press C-left a couple of times.
;;
;;     2) It lets you switch to other buffers by pressing C-right.
;;
;; To use Winner mode, put this line in your .emacs file:
;;
;;      (add-hook 'after-init-hook (lambda () (winner-mode 1)))

;; Details:
;;
;;   1. You may of course decide to use other bindings than those
;;      mentioned above.  Just set these variables in your .emacs:
;;
;;          `winner-prev-event'
;;          `winner-next-event'
;;
;;   2. When you have found the view of  your choice
;;      (using your favourite keys), you may press ctrl-space
;;      (`winner-max-event') to `delete-other-windows'.
;;
;;   3. Winner now keeps one configuration stack for each frame.
;;
;;
;;
;;                           Yours sincerely,   Ivar Rummelhoff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:



;;;; Variables you may want to change

(defvar winner-prev-event 'C-left
  "Winner mode binds this event to the command `winner-previous'.")

(defvar winner-next-event 'C-right
  "Winner mode binds this event to the command `winner-next'.")

(defvar winner-max-event 67108896	; CTRL-space
  "Event for deleting other windows
after having selected a view with Winner.

The normal functions of this event will also be performed.
In the default case (CTRL-SPACE) the mark will be set.")

(defvar winner-skip-buffers
  '("*Messages*",
    "*Compile-Log*",
    ".newsrc-dribble",
    "*Completions*",
    "*Buffer list*")
  "Exclude these buffer names
from any \(Winner mode\) list of buffers.")

(defvar winner-skip-regexps '("^ ")
  "Exclude buffers with names matching any of these regexps.
..from any \(Winner mode\) list of buffers.

By default `winner-skip-regexps' is set to \(\"^ \"\),
which excludes \"invisible buffers\".")


(defvar winner-limit 50
  "Winner will save no more than 2 * `winner-limit' window configurations.
\(.. and no less than `winner-limit'.\)")

(defvar winner-mode-hook nil
  "Functions to run whenever Winner mode is turned on.")

(defvar winner-mode-leave-hook nil
  "Functions to run whenever Winner mode is turned off.")

(defvar winner-dont-bind-my-keys nil
  "If non-nil: Do not use `winner-mode-map' in Winner mode.")



;;;; Winner mode

(eval-when-compile (require 'cl))


(defvar winner-mode nil)		; For the modeline.
(defvar winner-mode-map nil "Keymap for Winner mode.")

;;;###autoload
(defun winner-mode (&optional arg)
  "Toggle Winner mode.
With arg, turn Winner mode on if and only if arg is positive."
  (interactive "P")
  (let ((on-p (if arg (> (prefix-numeric-value arg) 0)
		(not winner-mode))))
    (cond
     (on-p (let ((winner-frames-changed (frame-list)))
	     (winner-do-save))		; Save current configurations
	   (add-hook 'window-configuration-change-hook 'winner-save-configuration)
	   (setq winner-mode t)
	   (run-hooks 'winner-mode-hook))
     (t (remove-hook 'window-configuration-change-hook 'winner-save-configuration)
	(when winner-mode
	  (setq winner-mode nil)
	  (run-hooks 'winner-mode-leave-hook))))
    (force-mode-line-update)))


;; List of frames which have changed
(defvar winner-frames-changed nil)

;; Time to save the window configuration.
(defun winner-save-configuration ()
  (push (selected-frame) winner-frames-changed)
  (add-hook 'post-command-hook 'winner-do-save))


(defun winner-do-save ()
  (let ((current (selected-frame)))
    (unwind-protect
	(do ((frames winner-frames-changed (cdr frames)))
	    ((null frames))
	  (unless (memq (car frames) (cdr frames))
	    ;; Process each frame once.
	    (select-frame (car frames))
	    (winner-push (current-window-configuration) (car frames))))
      (setq winner-frames-changed nil)
      (select-frame current)
      (remove-hook 'post-command-hook 'winner-do-save))))





;;;; Configuration stacks (one for each frame)


(defvar winner-stacks nil) ; ------ " ------

;; This works around a bug in defstruct.
(defvar custom-print-functions nil)

;; A stack of window configurations with some additional information.
(defstruct (winner-stack
	    (:constructor winner-stack-new
			  (config &aux
				  (data (list config))
				  (place data))))
  data place (count 1))


;; Return the stack of this frame
(defun winner-stack (frame)
  (let ((stack (cdr (assq frame winner-stacks))))
    (if stack (winner-stack-data stack)
      ;; Else make new stack
      (letf (((selected-frame) frame))
	(let ((config (current-window-configuration)))
	  (push (cons frame (winner-stack-new config))
		winner-stacks)
	  (list config))))))




;; Push this window configuration on the right stack,
;; but make sure the stack doesn't get too large etc...
(defun winner-push (config frame)
  (let ((this (cdr (assq frame winner-stacks))))
    (if (not this) (push (cons frame (winner-stack-new config))
			 winner-stacks)
      (push config (winner-stack-data this))
      (when (> (incf (winner-stack-count this)) winner-limit)
	;; No more than 2*winner-limit configs
	(setcdr (winner-stack-place this) nil)
	(setf   (winner-stack-place this)
	        (winner-stack-data  this))
	(setf   (winner-stack-count this) 1)))))








;;;; Selecting a window configuration


;; Return list of names of other buffers, excluding the current buffer
;; and buffers specified by the user.
(defun winner-other-buffers ()
  (loop for buf in (buffer-list)
	for name = (buffer-name buf)
	unless (or (eq (current-buffer) buf)
		   (member name winner-skip-buffers)
		   (loop for regexp in winner-skip-regexps
			 if (string-match regexp name) return t
			 finally return nil))
	collect name))



(defun winner-select (&optional arg)

  "Change to previous or new window configuration.
With arg start at position 1 if arg is positive, and
at -1 if arg is negative;  else start at position 0.
\(For Winner to record changes in window configurations,
Winner mode must be turned on.\)"
  (interactive "P")

  (setq arg
	(cond
	 ((not arg) nil)
	 ((> (prefix-numeric-value arg) 0) winner-next-event)
	 ((< (prefix-numeric-value arg) 0) winner-prev-event)
	 (t nil)))
  (if arg (push arg unread-command-events))

  (let ((stack (winner-stack (selected-frame)))
	(store nil)
	(buffers (winner-other-buffers))
	(passed nil)
	(config (current-window-configuration))
	(pos 0) event)
    ;; `stack'   and `store'  are stacks of window configuration while
    ;; `buffers' and `passed' are stacks of buffer names.

    (condition-case nil

	(loop
	 (setq event (read-event))
	 (cond

	  ((eq event winner-prev-event)
	   (cond (passed     (push (pop passed) buffers)(decf pos))
		 ((cdr stack)(push (pop stack)  store)  (decf pos))
		 (t (setq stack (append (nreverse store) stack))
		    (setq store nil)
		    (setq pos   0))))

	  ((eq event winner-next-event)
	   (cond (store   (push (pop store)   stack)  (incf pos))
		 (buffers (push (pop buffers) passed) (incf pos))
		 (t (setq buffers (nreverse passed))
		    (setq passed nil)
		    (setq pos 0))))

	  ((eq event winner-max-event)
	   ;; Delete other windows and leave.
	   (delete-other-windows)
	   ;; Let this change be saved.
	   (setq pos -1)
	   ;; Perform other actions of this event.
	   (push event unread-command-events)
	   (return))
	  (t (push event unread-command-events) (return)))

	 (cond
	  ;; Display
	  (passed (set-window-buffer (selected-window) (car passed))
		  (message (concat "Winner\(%d\): [%s] "
				   (mapconcat 'identity buffers " "))
			   pos (car passed)))

	  (t (set-window-configuration (car stack))
	     (if (window-minibuffer-p (selected-window))
		 (other-window 1))
	     (message "Winner\(%d\)" pos))))

      (quit (set-window-configuration config)
	    (setq pos 0)))
    (if (zerop pos)
	;; Do not record these changes.
	(remove-hook 'post-command-hook 'winner-do-save)
      ;; Else update the buffer list and make sure that the displayed
      ;; buffer is the same as the current buffer.
      (switch-to-buffer (window-buffer)))))
					   
					   



(defun winner-previous ()
  "Change to previous window configuration."
  (interactive)
  (winner-select -1))

(defun winner-next ()
  "Change to new window configuration."
  (interactive)
  (winner-select 1))




;;;; To be evaluated when the package is loaded:

(unless winner-mode-map
  (setq winner-mode-map (make-sparse-keymap))
  (define-key winner-mode-map (vector winner-prev-event) 'winner-previous)
  (define-key winner-mode-map (vector winner-next-event) 'winner-next))

(unless (or (assq 'winner-mode minor-mode-map-alist)
	    winner-dont-bind-my-keys)
  (push (cons 'winner-mode winner-mode-map)
	minor-mode-map-alist))

(unless (assq 'winner-mode minor-mode-alist)
  (push '(winner-mode " Win") minor-mode-alist))

(provide 'winner)

;;; winner.el ends here
