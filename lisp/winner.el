;;; winner.el  --- Restore window configuration (or switch buffer)

;; Copyright (C) 1997 Free Software Foundation. Inc.

;; Author: Ivar Rummelhoff <ivarr@ifi.uio.no>
;; Maintainer: Ivar Rummelhoff <ivarr@ifi.uio.no>
;; Created: 27 Feb 1997
;; Keywords: extensions, windows

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

;; Winner mode is a global minor mode that when turned on records
;; changes in window configuration.  This way the changes can be
;; "undone" using the function `winner-undo'.  By default this one is
;; bound to the key sequence ctrl-x left.  If you change your mind
;; (while undoing), you can press ctrl-x right (calling
;; `winner-redo').  Unlike the normal undo, you may have to skip
;; through several identical window configurations in order to find
;; the one you want.  This is a bug due to some techical limitations
;; in Emacs and can maybe be fixed in the future.
;; 
;; In addition to this I have added `winner-switch' which is a program
;; that switches to other buffers without disturbing Winner mode.  If
;; you bind this command to a key sequence, you may step through all
;; your buffers (except the ones mentioned in `winner-skip-buffers' or
;; matched by `winner-skip-regexps').  With a numeric prefix argument
;; skip several buffers at a time.

;;; Code:

(eval-when-compile (require 'cl))
(require 'ring)

(defvar winner-dont-bind-my-keys nil
  "If non-nil: Do not use `winner-mode-map' in Winner mode.")

(defvar winner-ring-size 100
  "Maximum number of stored window configurations per frame.")

(defvar winner-skip-buffers
  '("*Messages*",
    "*Compile-Log*",
    ".newsrc-dribble",
    "*Completions*",
    "*Buffer list*")
  "Exclude these buffer names from any \(Winner switch\) list of buffers.")

(defvar winner-skip-regexps '("^ ")
  "Winner excludes buffers with names matching any of these regexps.
They are not included in any Winner mode list of buffers.

By default `winner-skip-regexps' is set to \(\"^ \"\),
which excludes \"invisible buffers\".")

(defvar winner-ring-alist nil)

(defsubst winner-ring (frame)
  (or (cdr (assq frame winner-ring-alist))
      (progn
	(push (cons frame (make-ring winner-ring-size))
	      winner-ring-alist)
	(cdar winner-ring-alist))))

(defvar winner-modified-list nil)

(defun winner-change-fun ()
  (pushnew (selected-frame) winner-modified-list))

(defun winner-save-new-configurations ()
  (while winner-modified-list
    (ring-insert
     (winner-ring (car winner-modified-list))
     (current-window-configuration (pop winner-modified-list)))))

(defun winner-set (conf)
  (set-window-configuration conf)
  (if (eq (selected-window) (minibuffer-window))
      (other-window 1)))


;;; Winner mode  (a minor mode)

(defvar winner-mode-hook nil
  "Functions to run whenever Winner mode is turned on.")

(defvar winner-mode-leave-hook nil
  "Functions to run whenever Winner mode is turned off.")

(defvar winner-mode nil) ; mode variable
(defvar winner-mode-map nil "Keymap for Winner mode.")

(defun winner-mode (&optional arg)
  "Toggle Winner mode.
With arg, turn Winner mode on if and only if arg is positive."
  (interactive "P")
  (let ((on-p (if arg (> (prefix-numeric-value arg) 0)
		(not winner-mode))))
    (cond
     ;; Turn mode on
     (on-p 
      (setq winner-mode t)
      (add-hook 'window-configuration-change-hook 'winner-change-fun)
      (add-hook 'post-command-hook 'winner-save-new-configurations)
      (setq winner-modified-list (frame-list))
      (winner-save-new-configurations)
      (run-hooks 'winner-mode-hook))
     ;; Turn mode off
     (winner-mode
      (setq winner-mode nil)
      (run-hooks 'winner-mode-leave-hook)))
    (force-mode-line-update)))

;; Inspired by undo (simple.el)
(defun winner-undo (arg)
  "Switch back to an earlier window configuration saved by Winner mode.
In other words, \"undo\" changes in window configuration."
  (interactive "p")
  (cond
   ((not winner-mode) (error "Winner mode is turned off"))
   ((eq (selected-window) (minibuffer-window))
    (error "No winner undo from minibuffer."))
   (t (setq this-command t)
      (if (eq last-command 'winner-undo)
	  ;; This was no new window configuration after all.
	  (ring-remove winner-pending-undo-ring 0)
	(setq winner-pending-undo-ring (winner-ring (selected-frame)))
	(setq winner-undo-counter 0))
      (winner-undo-more (or arg 1))
      (message "Winner undo (%d)!" winner-undo-counter)
      (setq this-command 'winner-undo))))

(defvar winner-pending-undo-ring nil)

(defvar winner-undo-counter nil)

(defun winner-undo-more (count)
  "Undo N window configuration changes beyond what was already undone.
Call `winner-undo-start' to get ready to undo recent changes,
then call `winner-undo-more' one or more times to undo them."
  (let ((len (ring-length winner-pending-undo-ring)))
    (incf winner-undo-counter count)
    (if (>= winner-undo-counter len)
	(error "No further window configuration undo information")
      (winner-set
       (ring-ref winner-pending-undo-ring
		 winner-undo-counter)))))

(defun winner-redo ()
  "Restore a more recent window configuration saved by Winner mode."
  (interactive)
  (cond
   ((eq last-command 'winner-undo)
    (ring-remove winner-pending-undo-ring 0)
    (winner-set
     (ring-remove winner-pending-undo-ring 0))
    (or (eq (selected-window) (minibuffer-window))
	(message "Winner undid undo!")))
   (t (error "Previous command was not a winner-undo"))))

;;; Winner switch

(defun winner-switch-buffer-list ()
  (loop for buf in (buffer-list)
	for name = (buffer-name buf)
	unless (or (eq (current-buffer) buf)
		   (member name winner-skip-buffers)
		   (loop for regexp in winner-skip-regexps
			 if (string-match regexp name) return t
			 finally return nil))
	collect name))
  
(defvar winner-switch-list nil)

(defun winner-switch (count)
  "Step through your buffers without disturbing `winner-mode'.
`winner-switch' does not consider buffers mentioned in the list
`winner-skip-buffers' or matched by `winner-skip-regexps'."
  (interactive "p")
  (decf count)
  (setq this-command t)
  (cond
   ((eq last-command 'winner-switch)
    (if winner-mode (ring-remove (winner-ring (selected-frame)) 0))
    (bury-buffer (current-buffer))
    (mapcar 'bury-buffer winner-switch-list))
   (t (setq winner-switch-list (winner-switch-buffer-list))))
  (setq winner-switch-list (nthcdr count winner-switch-list))
  (or winner-switch-list
      (setq winner-switch-list (winner-switch-buffer-list))
      (error "No more buffers"))
  (switch-to-buffer (pop winner-switch-list))
  (message (concat "Winner: [%s] "
		   (mapconcat 'identity winner-switch-list " "))
	   (buffer-name))
  (setq this-command 'winner-switch))

;;;; To be evaluated when the package is loaded:

(unless winner-mode-map
  (setq winner-mode-map (make-sparse-keymap))
  (define-key winner-mode-map [?\C-x left]  'winner-undo)
  (define-key winner-mode-map [?\C-x right] 'winner-redo))

(unless (or (assq 'winner-mode minor-mode-map-alist)
	    winner-dont-bind-my-keys)
  (push (cons 'winner-mode winner-mode-map)
	minor-mode-map-alist))

(unless (assq 'winner-mode minor-mode-alist)
  (push '(winner-mode " Win") minor-mode-alist))

(provide 'winner)

;;; winner.el ends here
