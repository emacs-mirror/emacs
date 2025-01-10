;;; window-x.el --- extended window commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Pranshu Sharma <pranshu@bauherren.ovh>
;;         Martin Rudalics <rudalics@gmx.at>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: files
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

;; This file defines additional infrequently used window commands that
;; should not be in window.el to not make the dumped image bigger.

;;; Code:

(defun window-tree-normal-sizes (window &optional next)
  "Return normal sizes of all windows rooted at WINDOW.
A list of the form (SPLIT-TYPE PARENT-WIN PARENT-WIN-HEIGHT
PARENT-WIN-WIDTH W1 W2 ...) is returned.  SPLIT-TYPE is non-nil if
PARENT-WIN is split horizontally.  PARENT-WIN is the internal window.
PARENT-WIN-HEIGHT and PARENT-WIN-WIDTH are the normal heights of
PARENT-WIN.  Wn is a list of the form (WINDOW HEIGHT WIDTH) where HEIGHT
and WIDTH are the normal height and width of the window."
  (let (list)
    (while window
      (setq list
	    (cons
	     (cond
	      ((window-top-child window)
	       (append
		(list t window
		      (window-normal-size window nil)
		      (window-normal-size window t))
		(window-tree-normal-sizes (window-top-child window) t)))
	      ((window-left-child window)
	       (append
		(list nil window
			 (window-normal-size window nil)
			 (window-normal-size window t))
		(window-tree-normal-sizes (window-left-child window) t)))
	      (t (list window
		       (window-normal-size window nil)
		       (window-normal-size window t))))
	     list))
      (setq window (when next (window-next-sibling window))))
    (nreverse list)))

(defun window--window-to-transpose (frame-or-window)
  "Return the window to be acted upon by `window--transpose'.
If FRAME-OR-WINDOW is a window return FRAME-OR-WINDOW.  If
FRAME-OR-WINDOW is a frame, return FRAME-OR-WINDOW's main window.  If
FRAME-OR-WINDOW is nil, than the frames main window wil be returned.  If
FRAME-OR-WINDOW is non-nil, and not a frame or a window or a number,
than the return value will be the parent window of the selected window."
  (cond
   ((windowp frame-or-window)
    frame-or-window)
   ((or (framep frame-or-window) (not frame-or-window))
    (window-main-window frame-or-window))
   (frame-or-window
    (window-parent))))

(defun rotate-window-layout-anticlockwise (&optional frame-or-window)
  "Rotate windows of FRAME-OR-WINDOW anticlockwise by 90 degrees.
Transform the layout of windows such that a window on top becomes a
window on the right, a window on the right moves to the bottom, a window
on the bottom moves to the left and a window on the left becomes one on
the top.

If FRAME-OR-WINDOW is nil, rotate the main window of the selected
frame.  If FRAME-OR-WINDOW specifies a live frame, rotate the main
window of that frame.  If FRAME-OR-WINDOW specifies a parent window,
rotate that window.  In any other case and interactively with a prefix
argument rotate the parent window of the selected window."
  (interactive "P")
  (let ((window (window--window-to-transpose frame-or-window)))
    (window--transpose window '(right . above) nil)))

(defun rotate-window-layout-clockwise (&optional frame-or-window)
  "Rotate windows of FRAME-OR-WINDOW clockwise by 90 degrees.
Transform the layout of windows such that a window on top becomes a
window on the right, a window on the right moves to the bottom, a
window on the bottom moves to the left and a window on the left becomes
one on the top.

If FRAME-OR-WINDOW is nil, rotate the main window of the selected frame.
If FRAME-OR-WINDOW specifies a live frame, rotate the main window of
that frame.  If FRAME-OR-WINDOW specifies a parent window, rotate that
window.  In any other case and interactively with a prefix argument
rotate the parent window of the selected window."
  (interactive "P")
  (let ((window (window--window-to-transpose frame-or-window)))
    (window--transpose window '(left . below) nil)))

(defun flip-window-layout-horizontally (&optional frame-or-window)
  "Horizontally flip windows of FRAME-OR-WINDOW.
Flip the window layout so that the window on the right becomes the
window on the left, and vice-versa.

If FRAME-OR-WINDOW is nil, flip the main window of the selected frame.
If FRAME-OR-WINDOW specifies a live frame, rotate the main window of
that frame.  If FRAME-OR-WINDOW specifies a parent window, rotate that
window.  In any other case and interactively with a prefix argument
rotate the parent window of the selected window."
  (interactive "P")
  (let ((window (window--window-to-transpose frame-or-window)))
    (window--transpose window '(below . left) t)))

(defun flip-window-layout-vertically (&optional frame-or-window)
  "Verticlly flip windows of FRAME-OR-WINDOW.
Flip the window layout so that the top window becomes the bottom window
and vice-versa.

If FRAME-OR-WINDOW is nil, flip the main window of the selected frame.
If FRAME-OR-WINDOW specifies a live frame, rotate the main window of
that frame.  If FRAME-OR-WINDOW specifies a parent window, rotate that
window.  In any other case and interactively with a prefix argument
rotate the parent window of the selected window."
  (interactive "P")
  (let ((window (window--window-to-transpose frame-or-window)))
    (window--transpose window '(above . right) t)))

(defun transpose-window-layout (&optional frame-or-window)
  "Transpose windows of FRAME-OR-WINDOW.
Make the windows on FRAME-OR-WINDOW so that every horizontal split
becomes a vertical split, and vice versa.  This is equivalent to
diagonally flipping.

If FRAME-OR-WINDOW is nil, transpose the main window of the selected frame.
If FRAME-OR-WINDOW specifies a live frame, rotate the main window of
that frame.  If FRAME-OR-WINDOW specifies a parent window, rotate that
window.  In any other case and interactively with a prefix argument
rotate the parent window of the selected window."
  (interactive "P")
  (let ((window (window--window-to-transpose frame-or-window)))
    (window--transpose window '(right . below) nil)))

(defun window--depmap(fun ls)
  "Map FUN across all nodes of list LS."
  (if (consp ls)
      (cons
       (if (consp (car ls))
	  (window--depmap fun (car ls))
	 (funcall fun (car ls)))
       (window--depmap fun (cdr ls)))
    (funcall fun ls)))

(defun rotate-windows-back(&optional frame-or-window)
  "Move windows into locations of their predecessors in cyclic ordering.

If FRAME-OR-WINDOW is nil, rotate the main window of the selected frame.
If FRAME-OR-WINDOW specifies a live frame, rotate the main window of
that frame.  If FRAME-OR-WINDOW specifies a parent window, rotate that
window.  In any other case and interactively with a prefix argument
rotate the parent window of the selected window."
  (interactive "P")
  (rotate-windows frame-or-window t))

(defun rotate-windows (&optional frame-or-window reverse)
  "Move windows into locations of their forerunners in cyclic ordering.

Else if FRAME-OR-WINDOW is nil, rotate the main window of the
selected frame.  If FRAME-OR-WINDOW specifies a live frame, rotate the
main window of that frame.  If FRAME-OR-WINDOW specifies a parent
window, rotate that window.  In any other case and interactively with a
prefix argument rotate the parent window of the selected window."
  (interactive "P")
  (let ((window (window--window-to-transpose frame-or-window)))
    (if (or (not window)
	    (window-live-p window))
	(message "No windows to transpose")
      (let* ((frame (window-frame window))
	     (selected-window (frame-selected-window window))
	     (win-tree (car (window-tree-normal-sizes window)))
	     (winls (seq-filter 'window-live-p (flatten-list win-tree)))
	     (rotated-ls (if reverse
			     (append (cdr winls) (list (car winls)))
			   (append (last winls) winls)))
	     (other-window-arg (if reverse 1 -1))
	     (first-window (car rotated-ls))
	     (new-win-tree (window--depmap
			    (lambda (x)
			      (if (window-live-p x)
				  (pop rotated-ls)
				x))
			    win-tree)))
	(if (or (seq-some 'window-atom-root winls)
		(seq-some 'window-fixed-size-p winls))
	    (message "This does not work with fixed size or atom windows.")
	    (progn
	      ;; All child windows need to be recursively deleted.
	      (delete-other-windows-internal first-window window)
	      ;; (delete-dups atom-windows)
	      (window--transpose-1 new-win-tree first-window '(below . right) t nil)
	      (set-frame-selected-window frame selected-window)
	      (other-window other-window-arg)
	      (while (not (memq (selected-window) winls))
		(other-window other-window-arg))))))))

(defun window--transpose (window conf no-resize)
  "Rearrange windows of WINDOW recursively.
CONF should be a cons cell: (HORIZONTAL-SPLIT . VERTICAL-SPLIT) where
HORIZONTAL-SPLIT will be used as the third argument of `split-window'
when splitting a window that was previously horizontally split, and
VERTICAL-SPLIT as third argument of `split-window' for a window that was
previously vertically split.  If NO-RESIZE is nil, the SIDE argument of
the window-split is converted from vertical to horizontal or vice versa,
with the same proportion of the total split."
  (if (or (not window)
	  (window-live-p window))
      (message "No windows to transpose")
    (let* ((frame (window-frame window))
	   (first-window window)
	   (selected-window (frame-selected-window window))
	   (win-tree (car (window-tree-normal-sizes window)))
	   (win-list (seq-filter 'window-live-p (flatten-list win-tree)))
	   (atom-windows
	    (remq nil (mapcar 'window-atom-root
			      win-list))))
      (if (and (not (eq (car atom-windows) window))
	       (or no-resize
		   (and (not atom-windows)
			(not (seq-some 'window-fixed-size-p win-list)))))
	  (progn
	    (delete-dups atom-windows)
	    (while (not (window-live-p first-window))
	      (setq first-window (window-child first-window)))
	    (delete-other-windows-internal first-window window)
	    (window--transpose-1 win-tree first-window conf no-resize atom-windows)
	    ;; Go back to previously selected window.
	    (set-frame-selected-window frame selected-window)
	    (mapc 'window-make-atom atom-windows))
	(message "This does not work with fixed size or atom windows.")))))

(defun window--transpose-1 (subtree cwin conf no-resize atom-windows)
  "Subroutine of `window--transpose'.
SUBTREE must be in the format of the result of
`window-tree-normal-sizes'.  CWIN is the current window through which
the window splits are made.  ATOM-WINDOWS is a list of internal atom
windows.  The CONF and NO-RESIZE arguments are the same as the
ones in `window--transpose'."
  ;; `flen' is max size the window could be converted to the opposite
  ;; of the given split type.
  (let ((parent-window-is-set t)
	(flen (if (funcall (if no-resize 'not 'identity)
			   (car subtree))
		  (float (window-pixel-width cwin))
		(float (window-pixel-height cwin)))))
    (mapc
     (pcase-lambda (`(,window . ,size))
       (prog1
	   (let* ((split-size (- (round (* flen size))))
		  (split-type
		   (funcall (if (car subtree) 'car 'cdr) conf))
		  (return-win
		   (if (listp window)
		       ;; `window' is a window subtree.
		       ;; `first-child' is a live window that is an descended of window
		       (let* ((first-child window)
			      ;; If the window being split is atomic
			      (is-atom
			       ;; cadr will return the internal parent window
			       (memq (cadr first-child) atom-windows)))
			 ;; (caar (cddddr first-child)) is the first window in the
			 ;; list if there is a live window.
			 (while (not (windowp (caar (cddddr first-child))))
			   (setq first-child (car (cddddr first-child))))
			 (window--transpose-1
			  window
			  (let ((window-combination-limit parent-window-is-set))
			    (split-window
			     cwin
			     split-size
			     split-type
			     t
			     (if window-combination-limit
				 (cons (caar (cddddr first-child)) (cadr subtree))
			       (caar (cddddr first-child)))))
			  (if is-atom
			      '(nil . t)
			    conf)
			  no-resize
			  atom-windows))
		     ;; `window' is a window.
		     (split-window
		      cwin
		      split-size
		      split-type t
		      ;; We need to set parent window if it hasn't been set
		      ;; already.
		      (if parent-window-is-set
			  (cons window (cadr subtree))
			window)))))
	     (when (eq window-combination-limit t)
	       (set-window-combination-limit (cadr subtree) nil))
	     return-win)
	 (setq parent-window-is-set nil)))
     (mapcar
      (lambda (e)
	(pcase-let* ((`(,window . ,window-size-info)
		      (if (windowp (car e))
			  (cons (car e) e)
			(cons e (cdr e)))))
	  (cons window
		;; The respective size of the window.
		(if (car subtree)
		    (cadr window-size-info)
		  (caddr window-size-info)))))
      ;; We need to ingore first 5 elements of window list, we ignore
      ;; window split type, sizes and the first window (it's
      ;; implicitly created).  We just have a list of windows.
      (nreverse (cdr (cddddr subtree)))))
    ;; (caar (cddddr subtree)) is the first child window of subtree.
    (unless (windowp (caar (cddddr subtree)))
      (let ((is-atom (memq (cadr (cadr (cddddr subtree))) atom-windows)))
	(window--transpose-1 (car (cddddr subtree)) cwin (if is-atom '(nil . t) conf)
			     no-resize atom-windows)))))

;;; window-x.el ends here
