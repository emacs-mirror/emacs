;;; xt-mouse.el --- Support the mouse when emacs run in an xterm.
;; Copyright (C) 1994 Free Software Foundation

;; Author: Per Abrahamsen <abraham@iesd.auc.dk>
;; Keywords: mouse, terminals

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Comments:

;; Enable mouse support when running inside an xterm. 

;; This is actually useful when you are running X11 locally, but is
;; working on remote machine over a modem line or through a gateway.

;; It works by translating xterm escape codes into generic emacs mouse
;; events so it should work with any package that uses the mouse.

;;; Todo:

;; Support multi-click -- somehow.

;; Clicking on the mode-line does not work, although it should.

;;; Code: 

;; Emacs only generates down events when needed. 
;; This is too hard to emulate, so we cheat instead.
(or (lookup-key global-map [ down-mouse-1 ])
    (define-key global-map [ down-mouse-1 ] 'ignore))
(or (lookup-key global-map [ down-mouse-2 ])
    (define-key global-map [ down-mouse-2 ] 'ignore))
(or (lookup-key global-map [ down-mouse-3 ])
    (define-key global-map [ down-mouse-3 ] 'ignore))

(define-key function-key-map "\e[M" 'xterm-mouse-translate)

(defun xterm-mouse-translate (event)
  ;; Read a click and release event from XTerm.
  (save-excursion
    (save-window-excursion
      (deactivate-mark)
      (let* ((last)
	     (down (xterm-mouse-event)))
	(or (and (eq (read-char) ?\e)
		 (eq (read-char) ?\[)
		 (eq (read-char) ?M))
	    (error "Unexpected escape sequence from XTerm"))
	(let ((click (xterm-mouse-event)))
	  (setq unread-command-events
		(append unread-command-events
			(if (eq (nth 1 (nth 1 down)) (nth 1 (nth 1 click)))
			    (list click)
			  (list
			   ;; Generate move event to cheat `mouse-drag-region'.
			   (list 'mouse-movement (nth 1 click))
			   ;; Generate a drag event.
			   (list (intern (concat "drag-mouse-" (+ 1 last)))
				 (nth 1 down) (nth 1 click)))))))
	(vector down)))))

(defun xterm-mouse-event ()
  ;; Convert XTerm mouse event to Emacs mouse event.
  (let* ((type (- (read-char) ? ))
	 (x (- (read-char) ?  1))
	 (y (- (read-char) ?  1))
	 (point (cons x y))
	 (window (window-at x y))
	 (where (coordinates-in-window-p point window))
	 (pos (if (consp where)
		  (progn
		    (select-window window)
		    (goto-char (window-start window))
		    (move-to-window-line  (cdr where))
		    (move-to-column (+ (car where) (current-column)
				       (max 0 (1- (window-hscroll)))))
		    (point))
		where))
	 (mouse (intern (if (eq type 3)
			    (concat "mouse-" (+ 1 last))
			  (setq last type)
			  (concat "down-mouse-" (+ 1 type))))))
    (list mouse
	  (list window pos point
		(/ (nth 2 (current-time)) 1000)))))

;; Indicator for the xterm-mouse mode.
(defvar xterm-mouse-mode nil)

(or (assq 'xterm-mouse-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(xterm-mouse-mode (" Mouse")) minor-mode-alist)))

;;;###autoload
(defun xterm-mouse-mode (arg)
  "Toggle XTerm mouse mode.
With prefix arg, turn XTerm mouse mode on iff arg is positive.

Turn it on to use emacs mouse commands, and off to use xterm mouse commands."
  (interactive "P")
  (if (or (and (null arg) xterm-mouse-mode)
	  (<= (prefix-numeric-value arg) 0))
      ;; Turn it off
      (if xterm-mouse-mode
	  (progn
	    (turn-off-xterm-mouse-tracking)
	    (setq xterm-mouse-mode nil)
	    (set-buffer-modified-p (buffer-modified-p))))
    ;;Turn it on
    (if xterm-mouse-mode
	()
      (setq xterm-mouse-mode t)
      (turn-on-xterm-mouse-tracking)
      (set-buffer-modified-p (buffer-modified-p)))))

(defun turn-on-xterm-mouse-tracking ()
  ;; Enable emacs mouse tracking in xterm.
  (if xterm-mouse-mode
      (send-string-to-terminal "\e[?1000h")))

(defun turn-off-xterm-mouse-tracking ()
  ;; Disable disable emacs mouse tracking in xterm.
  (if xterm-mouse-mode
      (send-string-to-terminal "\e[?1000l")))

;; Restore normal mouse behaviour outside Emacs.
(add-hook 'suspend-hook 'turn-off-xterm-mouse-tracking)
(add-hook 'suspend-resume-hook 'turn-on-xterm-mouse-tracking)
(add-hook 'kill-emacs-hook 'turn-off-xterm-mouse-tracking)

(provide 'xt-mouse)

;;; xt-mouse.el ends here
