;; GNU Emacs window commands aside from those written in C.
;; Copyright (C) 1985, 1989 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


(defun count-windows (&optional minibuf)
   "Returns the number of visible windows.
Optional arg NO-MINI non-nil means don't count the minibuffer
even if it is active."
   (let ((count 0))
     (walk-windows (function (lambda ()
			       (setq count (+ count 1))))
		   minibuf)
     count))

(defun balance-windows ()
  "Makes all visible windows the same size (approximately)."
  (interactive)
  (let ((count 0))
    (walk-windows (function (lambda (w)
			      (setq count (+ count 1))))
		  'nomini)
    (let ((size (/ (screen-height) count)))
      (walk-windows (function (lambda (w)
				(select-window w)
				(enlarge-window (- size (window-height)))))
		    'nomini))))

(defun split-window-vertically (&optional arg)
  "Split current window into two windows, one above the other.
This window becomes the uppermost of the two, and gets
ARG lines.  No arg means split equally."
  (interactive "P")
  (let ((old-w (selected-window))
	new-w bottom)
    (setq new-w (split-window nil (and arg (prefix-numeric-value arg))))
    (save-excursion
      (set-buffer (window-buffer))
      (goto-char (window-start))
      (vertical-motion (window-height))
      (set-window-start new-w (point))
      (if (> (point) (window-point new-w))
	  (set-window-point new-w (point)))
      (vertical-motion -1)
      (setq bottom (point)))
    (if (<= bottom (point))
	(set-window-point old-w (1- bottom)))))

(defun split-window-horizontally (&optional arg)
  "Split current window into two windows side by side.
This window becomes the leftmost of the two, and gets
ARG columns.  No arg means split equally."
  (interactive "P")
  (split-window nil (and arg (prefix-numeric-value arg)) t))

(defun enlarge-window-horizontally (arg)
  "Make current window ARG columns wider."
  (interactive "p")
  (enlarge-window arg t))

(defun shrink-window-horizontally (arg)
  "Make current window ARG columns narrower."
  (interactive "p")
  (shrink-window arg t))

(defun window-config-to-register (name)
  "Save the current window configuration in register REG (a letter).
It can be later retrieved using \\[M-x register-to-window-config]."
  (interactive "cSave window configuration in register: ")
  (set-register name (current-window-configuration)))

(defun register-to-window-config (name)
  "Restore (make current) the window configuration in register REG (a letter).
Use with a register previously set with \\[window-config-to-register]."
  (interactive "cRestore window configuration from register: ")
  (set-window-configuration (get-register name)))

(define-key ctl-x-map "2" 'split-window-vertically)
(define-key ctl-x-map "5" 'split-window-horizontally)
(define-key ctl-x-map "6" 'window-config-to-register)
(define-key ctl-x-map "7" 'register-to-window-config)
(define-key ctl-x-map "}" 'enlarge-window-horizontally)
(define-key ctl-x-map "{" 'shrink-window-horizontally)
