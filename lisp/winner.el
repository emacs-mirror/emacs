;;; winner.el  --- Restore old window configurations

;; Copyright (C) 1997, 1998 Free Software Foundation. Inc.

;; Author: Ivar Rummelhoff <ivarr@ifi.uio.no>
;; Maintainer: Ivar Rummelhoff <ivarr@ifi.uio.no>
;; Created: 27 Feb 1997
;; Time-stamp: <1998-03-05 19:01:37 ivarr>
;; Keywords: windows

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

;; Winner mode is a global minor mode that records the changes in the
;; window configuration (i.e. how the frames are partitioned into
;; windows).  This way the changes can be "undone" using the function
;; `winner-undo'.  By default this one is bound to the key sequence
;; ctrl-x left.  If you change your mind (while undoing), you can
;; press ctrl-x right (calling `winner-redo').  Even though it uses
;; some features of Emacs20.3, winner.el should also work with
;; Emacs19.34 and XEmacs20, provided that the installed version of
;; custom is not obsolete.

;;; Code:

(eval-when-compile (require 'cl))
(require 'ring)

(when (fboundp 'defgroup)
  (defgroup winner nil ; Customization by Dave Love
    "Restoring window configurations."
    :group 'windows))

(unless (fboundp 'defcustom)
  (defmacro defcustom (symbol &optional initvalue docs &rest rest)
    (list 'defvar symbol initvalue docs)))


;;;###autoload
(defcustom winner-mode nil
  "Toggle winner-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `winner-mode'."
  :set #'(lambda (symbol value)
	   (winner-mode (or value 0)))
  :initialize 'custom-initialize-default
  :type    'boolean
  :group   'winner
  :require 'winner)

(defcustom winner-dont-bind-my-keys nil
  "If non-nil: Do not use `winner-mode-map' in Winner mode."
  :type  'boolean
  :group 'winner)

(defcustom winner-ring-size 200
  "Maximum number of stored window configurations per frame."
  :type  'integer
  :group 'winner)




;;;; Internal variables and subroutines


;; This variable contains the window cofiguration rings.
;; The key in this alist is the frame.
(defvar winner-ring-alist nil)

;; Find the right ring.  If it does not exist, create one.
(defsubst winner-ring (frame)
  (or (cdr (assq frame winner-ring-alist))
      (progn
	(let ((ring (make-ring winner-ring-size)))
	  (ring-insert ring (winner-configuration frame))
	  (push (cons frame ring) winner-ring-alist)
	  ring))))

(defvar winner-last-saviour nil)

;; Save the current window configuration, if it has changed and return
;; frame, else return nil.  If the last change was due to the same
;; command, save only the latest configuration.
(defun winner-insert-if-new (frame)
  (let ((conf (winner-configuration))
	(ring (winner-ring frame)))
    (cond
     ((winner-equal conf (ring-ref ring 0)) nil)
     (t (when (and (eq this-command (car winner-last-saviour))
		   (memq frame (cdr winner-last-saviour)))
	  (ring-remove ring 0))
	(ring-insert ring conf)
	frame))))

(defvar winner-modified-list nil) ; Which frames have changed?

;; This function is called when the window configuration changes.
(defun winner-change-fun ()
  (unless (memq (selected-frame) winner-modified-list)
    (push (selected-frame) winner-modified-list)))

;; For Emacs20
(defun winner-save-new-configurations ()
  (setq winner-last-saviour
	(cons this-command
	      (mapcar 'winner-insert-if-new winner-modified-list)))
  (setq winner-modified-list nil))

;; For compatibility with other emacsen.
(defun winner-save-unconditionally ()
  (setq winner-last-saviour
	(cons this-command
	      (list (winner-insert-if-new (selected-frame))))))

;; Arrgh.  This is storing the same information twice.
(defun winner-configuration (&optional frame)
  (if frame (letf (((selected-frame) frame)) (winner-configuration))
    (cons (current-window-configuration)
	  (loop for w being the windows
		collect (window-buffer w)))))


;; The same as `set-window-configuration',
;; but doesn't touch the minibuffer.
(defun winner-set-conf (winconf)
  (let ((min-sel  (window-minibuffer-p (selected-window)))
	(minibuf  (window-buffer (minibuffer-window)))
	(minipoint (letf ((selected-window) (minibuffer-window))
		     (point)))
	win)
    (set-window-configuration winconf)
    (setq win (selected-window))
    (select-window (minibuffer-window))
    (set-window-buffer (minibuffer-window) minibuf)
    (goto-char minipoint)
    (cond
     (min-sel)
     ((window-minibuffer-p win)
      (other-window 1))
     (t (select-window win)))))

(defun winner-win-data () ; Information about the windows
  (loop for win being the windows
	unless (window-minibuffer-p win)
	collect (list (window-buffer win)
		      (window-width  win)
		      (window-height win))))

;; Make sure point doesn't end up in the minibuffer and
;; delete windows displaying dead buffers.  Return nil
;; if and only if all the windows should have been deleted.
(defun winner-set (conf)
  (let ((origpoints
	 (save-excursion
	   (loop for buf in (cdr conf)
		 collect (if (buffer-name buf)
			     (progn (set-buffer buf) (point))
			   nil)))))
    (winner-set-conf (car conf))
    (let* ((win (selected-window))
	   (xwins (loop for window being the windows
			for pos in origpoints
			unless (window-minibuffer-p window)
			if pos do (progn (select-window window)
					 (goto-char pos))
			else collect window)))
      (select-window win)
      ;; Return t if possible configuration
      (cond
       ((null xwins) t)
       ((progn (mapcar 'delete-window (cdr xwins))
	       (one-window-p t))
	nil) ; No existing buffers
       (t (delete-window (car xwins)))))))


       

;;;; Winner mode  (a minor mode)

(defcustom winner-mode-hook nil
  "Functions to run whenever Winner mode is turned on."
  :type 'hook
  :group 'winner)

(defcustom winner-mode-leave-hook nil
  "Functions to run whenever Winner mode is turned off."
  :type 'hook
  :group 'winner)

(defvar winner-mode-map nil "Keymap for Winner mode.")

;; Is `window-configuration-change-hook' working?
(defun winner-hook-installed-p ()
  (save-window-excursion
    (let ((winner-var nil)
	  (window-configuration-change-hook
	   '((lambda () (setq winner-var t)))))
      (split-window)
      winner-var)))

;;;###autoload
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
      (cond
       ((winner-hook-installed-p)
	(add-hook 'window-configuration-change-hook 'winner-change-fun)
	(add-hook 'post-command-hook 'winner-save-new-configurations))
       (t (add-hook 'post-command-hook 'winner-save-unconditionally)))
      (setq winner-modified-list (frame-list))
      (winner-save-new-configurations)
      (run-hooks 'winner-mode-hook))
     ;; Turn mode off
     (winner-mode
      (setq winner-mode nil)
      (remove-hook 'window-configuration-change-hook 'winner-change-fun)
      (remove-hook 'post-command-hook 'winner-save-new-configurations)
      (remove-hook 'post-command-hook 'winner-save-unconditionally)
      (run-hooks 'winner-mode-leave-hook)))
    (force-mode-line-update)))

;; Inspired by undo (simple.el)

(defvar winner-pending-undo-ring nil
  "The ring currently used by winner undo.")
(defvar winner-undo-counter nil)
(defvar winner-undone-data  nil) ; There confs have been passed.

(defun winner-undo (arg)
  "Switch back to an earlier window configuration saved by Winner mode.
In other words, \"undo\" changes in window configuration.
With prefix arg, undo that many levels."
  (interactive "p")
  (cond
   ((not winner-mode) (error "Winner mode is turned off"))
   ;;   ((eq (selected-window) (minibuffer-window))
   ;;    (error "No winner undo from minibuffer."))
   (t (setq this-command t)
      (unless (eq last-command 'winner-undo)
	(setq winner-pending-undo-ring (winner-ring (selected-frame)))
	(setq winner-undo-counter 0)
	(setq winner-undone-data (list (winner-win-data))))
      (incf winner-undo-counter arg)
      (winner-undo-this)
      (unless (window-minibuffer-p (selected-window))
	(message "Winner undo (%d)" winner-undo-counter))
      (setq this-command 'winner-undo))))

(defun winner-undo-this () ; The heart of winner undo.
  (if (>= winner-undo-counter (ring-length winner-pending-undo-ring))
      (error "No further window configuration undo information")
    (unless (and
	     ;; Possible configuration
	     (winner-set
	      (ring-ref winner-pending-undo-ring
			winner-undo-counter))
	     ;; New configuration
	     (let ((data (winner-win-data)))
	       (if (member data winner-undone-data) nil
		 (push data winner-undone-data))))
      (ring-remove winner-pending-undo-ring winner-undo-counter)
      (winner-undo-this))))

(defun winner-redo () ; If you change your mind.
  "Restore a more recent window configuration saved by Winner mode."
  (interactive)
  (cond
   ((eq last-command 'winner-undo)
    (ring-remove winner-pending-undo-ring 0)
    (winner-set
     (ring-remove winner-pending-undo-ring 0))
    (or (eq (selected-window) (minibuffer-window))
	(message "Winner undid undo")))
   (t (error "Previous command was not a winner-undo"))))

;;;; To be evaluated when the package is loaded:

(if (fboundp 'compare-window-configurations)
    (defalias 'winner-equal 'compare-window-configurations)
  (defalias 'winner-equal 'equal))

(unless winner-mode-map
  (setq winner-mode-map (make-sparse-keymap))
  (define-key winner-mode-map [(control x) left] 'winner-undo)
  (define-key winner-mode-map [(control x) right] 'winner-redo))

(unless (or (assq 'winner-mode minor-mode-map-alist)
	    winner-dont-bind-my-keys)
  (push (cons 'winner-mode winner-mode-map)
	minor-mode-map-alist))

(unless (assq 'winner-mode minor-mode-alist)
  (push '(winner-mode " Win") minor-mode-alist))

(provide 'winner)

;;; winner.el ends here
