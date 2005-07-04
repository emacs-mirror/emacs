;;; xt-mouse.el --- support the mouse when emacs run in an xterm

;; Copyright (C) 1994, 2000, 2001, 2005 Free Software Foundation

;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: mouse, terminals

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Enable mouse support when running inside an xterm.

;; This is actually useful when you are running X11 locally, but is
;; working on remote machine over a modem line or through a gateway.

;; It works by translating xterm escape codes into generic emacs mouse
;; events so it should work with any package that uses the mouse.

;; You don't have to turn off xterm mode to use the normal xterm mouse
;; functionality, it is still available by holding down the SHIFT key
;; when you press the mouse button.

;;; Todo:

;; The xterm mouse escape codes are supposedly also supported by the
;; Linux console, but I have not been able to verify this.

;; Support multi-click -- somehow.

;;; Code:

(define-key function-key-map "\e[M" 'xterm-mouse-translate)

(defvar xterm-mouse-last)

;; Mouse events symbols must have an 'event-kind property with
;; the value 'mouse-click.
(dolist (event-type '(mouse-1 mouse-2 mouse-3))
  (put event-type 'event-kind 'mouse-click))

(defun xterm-mouse-translate (event)
  "Read a click and release event from XTerm."
  (save-excursion
    (save-window-excursion
      (deactivate-mark)
      (let* ((xterm-mouse-last)
	     (down (xterm-mouse-event))
	     (down-command (nth 0 down))
	     (down-data (nth 1 down))
	     (down-where (nth 1 down-data))
	     (down-binding (key-binding (if (symbolp down-where)
					    (vector down-where down-command)
					  (vector down-command))))
	     (is-click (string-match "^mouse" (symbol-name (car down)))))

	(unless is-click
	  (unless (and (eq (read-char) ?\e)
		       (eq (read-char) ?\[)
		       (eq (read-char) ?M))
	    (error "Unexpected escape sequence from XTerm")))

	(let* ((click (if is-click down (xterm-mouse-event)))
	       (click-command (nth 0 click))
	       (click-data (nth 1 click))
	       (click-where (nth 1 click-data)))
	  (if (memq down-binding '(nil ignore))
	      (if (and (symbolp click-where)
		       (consp click-where))
		  (vector (list click-where click-data) click)
		(vector click))
	    (setq unread-command-events
		  (if (eq down-where click-where)
		      (list click)
		    (list
		     ;; Cheat `mouse-drag-region' with move event.
		     (list 'mouse-movement click-data)
		     ;; Generate a drag event.
		     (if (symbolp down-where)
			 0
		       (list (intern (format "drag-mouse-%d"
					     (+ 1 xterm-mouse-last)))
			     down-data click-data)))))
	    (if (and (symbolp down-where)
		     (consp down-where))
		(vector (list down-where down-data) down)
	      (vector down))))))))

(defvar xterm-mouse-x 0
  "Position of last xterm mouse event relative to the frame.")

(defvar xterm-mouse-y 0
  "Position of last xterm mouse event relative to the frame.")

;; Indicator for the xterm-mouse mode.

(defun xterm-mouse-position-function (pos)
  "Bound to `mouse-position-function' in XTerm mouse mode."
  (setcdr pos (cons xterm-mouse-x xterm-mouse-y))
  pos)

;; read xterm sequences above ascii 127 (#x7f)
(defun xterm-mouse-event-read ()
  (let ((c (read-char)))
    (if (< c 0)
        (+ c #x8000000 128)
      c)))

(defun xterm-mouse-event ()
  "Convert XTerm mouse event to Emacs mouse event."
  (let* ((type (- (xterm-mouse-event-read) #o40))
	 (x (- (xterm-mouse-event-read) #o40 1))
	 (y (- (xterm-mouse-event-read) #o40 1))
	 (mouse (intern
		 ;; For buttons > 3, the release-event looks
		 ;; differently (see xc/programs/xterm/button.c,
		 ;; function EditorButton), and there seems to come in
		 ;; a release-event only, no down-event.
		 (cond ((>= type 64)
			(format "mouse-%d" (- type 60)))
		       ((= type 3)
			(format "mouse-%d" (+ 1 xterm-mouse-last)))
		       (t
			(setq xterm-mouse-last type)
			(format "down-mouse-%d" (+ 1 type))))))
	 (w (window-at x y))
         (ltrb (window-edges w))
         (left (nth 0 ltrb))
         (top (nth 1 ltrb)))

    (setq xterm-mouse-x x
	  xterm-mouse-y y)
    (if w
	(list mouse (posn-at-x-y (- x left) (- y top) w t))
      (list mouse
	    (append (list nil 'menu-bar) (nthcdr 2 (posn-at-x-y x y w t)))))))

;;;###autoload
(define-minor-mode xterm-mouse-mode
  "Toggle XTerm mouse mode.
With prefix arg, turn XTerm mouse mode on iff arg is positive.

Turn it on to use Emacs mouse commands, and off to use xterm mouse commands.
This works in terminal emulators compatible with xterm.  It only
works for simple uses of the mouse.  Basically, only non-modified
single clicks are supported.  When turned on, the normal xterm
mouse functionality for such clicks is still available by holding
down the SHIFT key while pressing the mouse button."
  :global t :group 'mouse
  (if xterm-mouse-mode
      ;; Turn it on
      (unless window-system
	(setq mouse-position-function #'xterm-mouse-position-function)
	(turn-on-xterm-mouse-tracking))
    ;; Turn it off
    (turn-off-xterm-mouse-tracking 'force)
    (setq mouse-position-function nil)))

(defun turn-on-xterm-mouse-tracking ()
  "Enable Emacs mouse tracking in xterm."
  (if xterm-mouse-mode
      (send-string-to-terminal "\e[?1000h")))

(defun turn-off-xterm-mouse-tracking (&optional force)
  "Disable Emacs mouse tracking in xterm."
  (if (or force xterm-mouse-mode)
      (send-string-to-terminal "\e[?1000l")))

;; Restore normal mouse behaviour outside Emacs.
(add-hook 'suspend-hook 'turn-off-xterm-mouse-tracking)
(add-hook 'suspend-resume-hook 'turn-on-xterm-mouse-tracking)
(add-hook 'kill-emacs-hook 'turn-off-xterm-mouse-tracking)

(provide 'xt-mouse)

;;; arch-tag: 84962d4e-fae9-4c13-a9d7-ef4925a4ac03
;;; xt-mouse.el ends here
