;;; calc-trail.el --- functions for manipulating the Calc "trail"

;; Copyright (C) 1990, 1991, 1992, 1993, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: David Gillespie <daveg@synaptics.com>
;; Maintainer: Jay Belanger <belanger@truman.edu>

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; Commentary:

;;; Code:

;; This file is autoloaded from calc-ext.el.

(require 'calc-ext)
(require 'calc-macs)

;;; Trail commands.

(defun calc-trail-in ()
  (interactive)
  (let ((win (get-buffer-window (calc-trail-display t))))
    (and win (select-window win))))

(defun calc-trail-out ()
  (interactive)
  (calc-select-buffer)
  (let ((win (get-buffer-window (current-buffer))))
    (if win
	(progn
	  (select-window win)
	  (calc-align-stack-window))
      (calc))))

(defun calc-trail-next (n)
  (interactive "p")
  (calc-with-trail-buffer
   (forward-line n)
   (calc-trail-here)))

(defun calc-trail-previous (n)
  (interactive "p")
  (calc-with-trail-buffer
   (forward-line (- n))
   (calc-trail-here)))

(defun calc-trail-first (n)
  (interactive "p")
  (calc-with-trail-buffer
   (goto-char (point-min))
   (forward-line n)
   (calc-trail-here)))

(defun calc-trail-last (n)
  (interactive "p")
  (calc-with-trail-buffer
   (goto-char (point-max))
   (forward-line (- n))
   (calc-trail-here)))

(defun calc-trail-scroll-left (n)
  (interactive "P")
  (let ((curwin (selected-window)))
    (calc-with-trail-buffer
     (unwind-protect
	 (progn
	   (select-window (get-buffer-window (current-buffer)))
	   (calc-scroll-left n))
       (select-window curwin)))))

(defun calc-trail-scroll-right (n)
  (interactive "P")
  (let ((curwin (selected-window)))
    (calc-with-trail-buffer
     (unwind-protect
	 (progn
	   (select-window (get-buffer-window (current-buffer)))
	   (calc-scroll-right n))
       (select-window curwin)))))

(defun calc-trail-forward (n)
  (interactive "p")
  (calc-with-trail-buffer
   (forward-line (* n (1- (window-height))))
   (calc-trail-here)))

(defun calc-trail-backward (n)
  (interactive "p")
  (calc-with-trail-buffer
   (forward-line (- (* n (1- (window-height)))))
   (calc-trail-here)))

(defun calc-trail-isearch-forward ()
  (interactive)
  (calc-with-trail-buffer
   (save-window-excursion
     (select-window (get-buffer-window (current-buffer)))
     (let ((search-exit-char ?\r))
       (isearch-forward)))
   (calc-trail-here)))

(defun calc-trail-isearch-backward ()
  (interactive)
  (calc-with-trail-buffer
   (save-window-excursion
     (select-window (get-buffer-window (current-buffer)))
     (let ((search-exit-char ?\r))
       (isearch-backward)))
   (calc-trail-here)))

(defun calc-trail-yank (arg)
  (interactive "P")
  (calc-wrapper
   (or arg (calc-set-command-flag 'hold-trail))
   (calc-enter-result 0 "yank"
		      (calc-with-trail-buffer
		       (if arg
			   (forward-line (- (prefix-numeric-value arg))))
		       (if (or (looking-at "Emacs Calc")
			       (looking-at "----")
			       (looking-at " ? ? ?[^ \n]* *$")
			       (looking-at "..?.?$"))
			   (error "Can't yank that line"))
		       (if (looking-at ".*, \\.\\.\\., ")
			   (error "Can't yank (vector was abbreviated)"))
		       (forward-char 4)
		       (search-forward " ")
		       (let* ((next (save-excursion (forward-line 1) (point)))
			      (str (buffer-substring (point) (1- next)))
			      (val (save-excursion
				     (set-buffer save-buf)
				     (math-read-plain-expr str))))
			 (if (eq (car-safe val) 'error)
			     (error "Can't yank that line: %s" (nth 2 val))
			   val))))))

(defun calc-trail-marker (str)
  (interactive "sText to insert in trail: ")
  (calc-with-trail-buffer
   (forward-line 1)
   (let ((buffer-read-only nil))
     (insert "---- " str "\n"))
   (forward-line -1)
   (calc-trail-here)))

(defun calc-trail-kill (n)
  (interactive "p")
  (calc-with-trail-buffer
   (let ((buffer-read-only nil))
     (save-restriction
       (narrow-to-region   ; don't delete "Emacs Trail" header
	(save-excursion
	  (goto-char (point-min))
	  (forward-line 1)
	  (point))
	(point-max))
       (kill-line n)))
   (calc-trail-here)))

(provide 'calc-trail)

;;; arch-tag: 59b76655-d882-4aab-a3ee-b83870e530d0
;;; calc-trail.el ends here
