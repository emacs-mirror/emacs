;; scroll-lock.el -- scroll-locking minor mode

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Gary D. Foster <Gary.Foster@corp.sun.com>
;; Keywords: scroll crisp brief lock

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

;; Commentary
;;    This mode allows multiple buffers to be 'locked' so that scrolling
;;    up or down lines in any buffer causes all the buffers to mirror
;;    the scrolling.  It hooks into the post-command-hook to check for
;;    potential scrolling commands and if we're locked, mirrors them in all
;;    windows.	This allows us to grab line-at-a-time scrolling as well as
;;    screen-at-a-time scrolling, and doesn't remap any of the keyboard
;;    commands to do it.

;; You can disable autoloading of this package by placing
;; (setq crisp-load-scroll-lock nil) in your .emacs before loading
;; the crisp package.  If you want to use this package by itself,
;; you can enable it by placing the following in your .emacs:

;; (require 'scroll-lock)

;; In the first (autoloaded) case, meta-f1 is bound to the command to
;; toggle the scroll-lock mode.  In the second (non-autoloaded) case,
;; you can enable and disable it with the 'scroll-lock-mode' command.

;; Suggestions/ideas from:
;;    Rick Macdonald <rickm@vsl.com>
;;    Anders Lindgren <andersl@csd.uu.se>

(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))
(defvar is-scroll-locked nil
  "Track status of scroll locking.")
(if running-xemacs
    (add-minor-mode 'is-scroll-locked " *SL*")
  (or (assq 'is-scroll-locked-mode minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(is-scroll-locked-mode " *SL*") minor-mode-alist))))

(defun scroll-lock-scroll-down-all (arg)
  "Scroll-down all visible windows."
  (interactive "P")
  (let ((num-windows (count-windows))
	(count 1))
    (if (> num-windows 1)
	( progn (other-window 1)
		(while (< count num-windows)
		  (if (not (eq (point) (point-max)))
		      (progn (call-interactively 'next-line)))
		  (other-window 1)
		  (setq count (1+ count)))))))

(defun scroll-lock-scroll-up-all (arg)
  "Scroll-up all visible windows."
  (interactive "P")
  (let ((num-windows (count-windows))
	(count 1))
    (if (> num-windows 1)
	( progn (other-window 1)
		(while (< count num-windows)
		  (if (not (eq (point) (point-min)))
		      (progn (call-interactively 'previous-line)))
		  (other-window 1)
		  (setq count (1+ count)))))))

(defun scroll-lock-page-down-all (arg)
  "Page-down all visible windows."
  (interactive "P")
  (let ((num-windows (count-windows))
	(count 1))
    (if (> num-windows 1)
	(progn (other-window 1)
	       (while (< count num-windows)
		 (call-interactively 'fkey-scroll-up)
		 (other-window 1)
		 (setq count (1+ count)))))))

(defun scroll-lock-page-up-all (arg)
  "Page-up all visible windows."
  (interactive "P")
  (let ((num-windows (count-windows))
	(count 1))
    (if (> num-windows 1)
	(progn (other-window 1)
	       (while (< count num-windows)
		 (call-interactively 'fkey-scroll-down)
		 (other-window 1)
		 (setq count (1+ count)))))))


(defun scroll-lock-check-to-scroll ()
  "Check last-command to see if a scroll was done."
  (if (eq this-command 'next-line)
      (call-interactively 'scroll-lock-scroll-down-all))
  (if (eq this-command 'previous-line)
      (call-interactively 'scroll-lock-scroll-up-all))
  (if (eq this-command 'fkey-scroll-up)
      (call-interactively 'scroll-lock-page-down-all))
  (if (eq this-command 'fkey-scroll-down)
      (call-interactively 'scroll-lock-page-up-all)))


(defun scroll-lock-mode (arg)
  "Toggle scroll-lock minor mode."
  (interactive "P")
  (setq is-scroll-locked (not is-scroll-locked))
  (cond
   ((eq is-scroll-locked 't)
    (add-hook 'post-command-hook 'scroll-lock-check-to-scroll))
   ((eq is-scroll-locked 'nil)
    (remove-hook 'post-command-hook 'scroll-lock-check-to-scroll))))

(provide 'scroll-lock)

;; scroll-all.el ends here
