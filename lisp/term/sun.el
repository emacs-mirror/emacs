;;; sun.el --- keybinding for standard default sunterm keys

;; Copyright (C) 1987, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007 Free Software Foundation, Inc.

;; Author: Jeff Peck <peck@sun.com>
;; Keywords: terminals

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; The function key sequences for the console have been converted for
;; use with function-key-map, but the *tool stuff hasn't been touched.

;;; Code:

(defun scroll-down-in-place (n)
  (interactive "p")
  (forward-line (- n))
  (scroll-down n))

(defun scroll-up-in-place (n)
  (interactive "p")
  (forward-line n)
  (scroll-up n))

(defun kill-region-and-unmark (beg end)
  "Like kill-region, but pops the mark [which equals point, anyway.]"
  (interactive "r")
  (kill-region beg end)
  (setq this-command 'kill-region-and-unmark)
  (set-mark-command t))

(defun select-previous-complex-command ()
  "Select Previous-complex-command"
  (interactive)
  (if (zerop (minibuffer-depth))
      (repeat-complex-command 1)
    ;; FIXME: this function does not seem to exist.  -stef'01
    (previous-complex-command 1)))

(defun rerun-prev-command ()
  "Repeat Previous-complex-command."
  (interactive)
  (eval (nth 0 command-history)))

(defvar grep-arg nil "Default arg for RE-search")
(defun grep-arg ()
  (if (memq last-command '(research-forward research-backward)) grep-arg
    (let* ((command (car command-history))
	   (command-name (symbol-name (car command)))
	   (search-arg (car (cdr command)))
	   (search-command
	    (and command-name (string-match "search" command-name)))
	   )
      (if (and search-command (stringp search-arg)) (setq grep-arg search-arg)
	(setq search-command this-command
	      grep-arg (read-string "REsearch: " grep-arg)
	      this-command search-command)
	grep-arg))))

(defun research-forward ()
  "Repeat RE search forward."
  (interactive)
  (re-search-forward (grep-arg)))

(defun research-backward ()
  "Repeat RE search backward."
  (interactive)
  (re-search-backward (grep-arg)))

;;
;; handle sun's extra function keys
;; this version for those who run with standard .ttyswrc and no emacstool
;;
;; sunview picks up expose and open on the way UP,
;; so we ignore them on the way down
;;

(defvar sun-raw-prefix (make-sparse-keymap))

;; Since .emacs gets loaded before this file, a hook is supplied
;; for you to put your own bindings in.

(defvar sun-raw-prefix-hooks nil
  "List of forms to evaluate after setting sun-raw-prefix.")



(defun terminal-init-sun ()
  "Terminal initialization function for sun."
  (define-key local-function-key-map "\e[" sun-raw-prefix)

  (define-key sun-raw-prefix "210z" [r3])
  (define-key sun-raw-prefix "213z" [r6])
  (define-key sun-raw-prefix "214z" [r7])
  (define-key sun-raw-prefix "216z" [r9])
  (define-key sun-raw-prefix "218z" [r11])
  (define-key sun-raw-prefix "220z" [r13])
  (define-key sun-raw-prefix "222z" [r15])
  (define-key sun-raw-prefix "193z" [redo])
  (define-key sun-raw-prefix "194z" [props])
  (define-key sun-raw-prefix "195z" [undo])
  ;; (define-key sun-raw-prefix "196z" 'ignore)		; Expose-down
  ;; (define-key sun-raw-prefix "197z" [put])
  ;; (define-key sun-raw-prefix "198z" 'ignore)		; Open-down
  ;; (define-key sun-raw-prefix "199z" [get])
  (define-key sun-raw-prefix "200z" [find])
  ;; (define-key sun-raw-prefix "201z" 'kill-region-and-unmark)	; Delete
  (define-key sun-raw-prefix "224z" [f1])
  (define-key sun-raw-prefix "225z" [f2])
  (define-key sun-raw-prefix "226z" [f3])
  (define-key sun-raw-prefix "227z" [f4])
  (define-key sun-raw-prefix "228z" [f5])
  (define-key sun-raw-prefix "229z" [f6])
  (define-key sun-raw-prefix "230z" [f7])
  (define-key sun-raw-prefix "231z" [f8])
  (define-key sun-raw-prefix "232z" [f9])
  (define-key sun-raw-prefix "233z" [f10])
  (define-key sun-raw-prefix "234z" [f11])
  (define-key sun-raw-prefix "235z" [f12])
  (define-key sun-raw-prefix "A" [up])			; R8
  (define-key sun-raw-prefix "B" [down])		; R14
  (define-key sun-raw-prefix "C" [right])		; R12
  (define-key sun-raw-prefix "D" [left])		; R10

  (global-set-key [r3]	'backward-page)
  (global-set-key [r6]	'forward-page)
  (global-set-key [r7]	'beginning-of-buffer)
  (global-set-key [r9]	'scroll-down)
  (global-set-key [r11]	'recenter)
  (global-set-key [r13]	'end-of-buffer)
  (global-set-key [r15]	'scroll-up)
  (global-set-key [redo]	'redraw-display) ;FIXME: collides with default.
  (global-set-key [props]	'list-buffers)
  (global-set-key [put]	'sun-select-region)
  (global-set-key [get]	'sun-yank-selection)
  (global-set-key [find]	'exchange-point-and-mark)
  (global-set-key [f3]	'scroll-down-in-place)
  (global-set-key [f4]	'scroll-up-in-place)
  (global-set-key [f6]	'shrink-window)
  (global-set-key [f7]	'enlarge-window)

  (when sun-raw-prefix-hooks
    (message "sun-raw-prefix-hooks is obsolete!  Use term-setup-hook instead!")
    (let ((hooks sun-raw-prefix-hooks))
      (while hooks
	(eval (car hooks))
	(setq hooks (cdr hooks))))))

;;; arch-tag: db761d47-fd7d-42b4-aae1-04fa116b6ba6
;;; sun.el ends here
