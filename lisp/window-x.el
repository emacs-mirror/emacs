;;; window-x.el --- Extra window related commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; Author: Pranshu Sharma <pranshu@bauherren.ovh>
;;         Martin Rudalics <rudalics@gmx.at>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: window, convenience
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

;; This file defines less frequently used window organization commands.

;;; Code:

(defcustom rotate-windows-change-selected t
  "If nil the selected window will not change with `rotate-windows'.

The selected window before and after the function call will stay
unchanged if nil.  `rotate-windows-back' is also affected."
  :type 'boolean
  :group 'windows)

(defun window-tree-normal-sizes (window &optional next)
  "Return normal sizes of all windows rooted at WINDOW.

The return value is a list of the form (SPLIT-TYPE PARENT-WIN
PARENT-WIN-HEIGHT PARENT-WIN-WIDTH . WS), where SPLIT-TYPE is non-nil if
PARENT-WIN is split horizontally; PARENT-WIN is the internal window;
PARENT-WIN-HEIGHT and PARENT-WIN-WIDTH are the normal heights of
PARENT-WIN; and WS is a list of lists the form (WINDOW HEIGHT WIDTH)
where HEIGHT and WIDTH are the normal height and width of the window.

(fn WINDOW)"
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

(defsubst window--rotate-interactive-arg ()
  "Return interactive window argument for window rotation commands."
  (if current-prefix-arg (window-parent) (window-main-window)))

;;;###autoload
(defun window-layout-rotate-anticlockwise (&optional window)
  "Rotate window layout of WINDOW counterclockwise by 90 degrees.

If WINDOW is nil, it defaults to the root window of the selected frame.

Interactively, a prefix argument says to rotate the parent window of the
selected window."
  (interactive (list (window--rotate-interactive-arg)))
  (window--transpose window '(right . above) nil))

;;;###autoload
(defun window-layout-rotate-clockwise (&optional window)
  "Rotate window layout under WINDOW clockwise by 90 degrees.

If WINDOW is nil, it defaults to the root window of the selected frame.

Interactively, a prefix argument says to rotate the parent window of the
selected window."
  (interactive (list (window--rotate-interactive-arg)))
  (window--transpose window '(left . below) nil))

;;;###autoload
(defun window-layout-flip-leftright (&optional window)
  "Horizontally flip windows under WINDOW.

Flip the window layout so that the window on the right becomes the
window on the left, and vice-versa.

If WINDOW is nil, it defaults to the root window of the selected frame.

Interactively, a prefix argument says to flip the parent window of the
selected window."
  (interactive (list (window--rotate-interactive-arg)))
  (window--transpose window '(below . left) t))

;;;###autoload
(defun window-layout-flip-topdown (&optional window)
  "Vertically flip windows under WINDOW.

Flip the window layout so that the top window becomes the bottom window,
and vice-versa.

If WINDOW is nil, it defaults to the root window of the selected frame.

Interactively, a prefix argument says to flip the parent window of the
selected window."
  (interactive (list (window--rotate-interactive-arg)))
  (window--transpose window '(above . right) t))

;;;###autoload
(defun window-layout-transpose (&optional window)
  "Transpose windows under WINDOW.

Reorganize the windows under WINDOW so that every horizontal split
becomes a vertical split, and vice versa.  This is equivalent to
diagonally flipping.

If WINDOW is nil, it defaults to the root window of the selected frame.

Interactively, a prefix argument says to transpose the parent window of
the selected window."
  (interactive (list (window--rotate-interactive-arg)))
  (window--transpose window '(right . below) nil))

;;;###autoload
(defun rotate-windows-back (&optional window)
  "Rotate windows under WINDOW backward in cyclic ordering.

If WINDOW is nil, it defaults to the root window of the selected frame.

Interactively, a prefix argument says to rotate the parent window of the
selected window."
  (interactive (list (window--rotate-interactive-arg)))
  (rotate-windows window t))

;;;###autoload
(defun rotate-windows (&optional window reverse)
  "Rotate windows under WINDOW in cyclic ordering.

Optional argument REVERSE says to rotate windows backward, in reverse
cyclic order.

If WINDOW is nil, it defaults to the root window of the selected frame.

Interactively, a prefix argument says to rotate the parent window of the
selected window."
  (interactive (list (window--rotate-interactive-arg)))
  (when (or (not window) (window-live-p window))
    (user-error "No windows to rotate"))
  (let* ((frame (window-frame window))
	 (selected-window (frame-selected-window window))
	 (win-tree (car (window-tree-normal-sizes window)))
	 (winls (or
                 (seq-filter
                  (lambda (win)
                    (and (window-live-p win)
                         (not (window-dedicated-p win))))
                  (flatten-list win-tree))
                 (user-error "All windows are dedicated")))
	 (rotated-ls (if reverse
			 (append (cdr winls) (list (car winls)))
		       (append (last winls) winls)))
	 (other-window-arg (if reverse 1 -1))
	 (first-window (car rotated-ls))
	 (new-win-tree
          ;; Recursively process `win-tree' and construct a new tree
          ;; with the same shape and rotated windows at the leaves.
          (named-let rec ((tree win-tree))
            (cond
             ((consp tree) (cons (rec (car tree)) (rec (cdr tree))))
             ((and (window-live-p tree)
                   (not (window-dedicated-p tree)))
              (pop rotated-ls))
             (t tree)))))
    (when (or (seq-some #'window-atom-root winls)
	      (seq-some #'window-fixed-size-p winls))
      (user-error "Cannot rotate windows due to fixed size or atom windows"))
    (delete-other-windows-internal first-window window)
    (window--transpose-1 new-win-tree first-window '(below . right) t nil)
    (set-frame-selected-window frame selected-window)
    (when rotate-windows-change-selected
      (other-window other-window-arg)
      (while (not (memq (selected-window) winls))
        (other-window other-window-arg)))))

(defun window--transpose (window conf no-resize)
  "Rearrange windows under WINDOW recursively.
CONF should be a cons cell (HORIZONTAL-SPLIT . VERTICAL-SPLIT) where
HORIZONTAL-SPLIT will be used as the third argument of `split-window'
when splitting a window that was previously horizontally split, and
VERTICAL-SPLIT as third argument of `split-window' for a window that was
previously vertically split.  If NO-RESIZE is nil, the SIDE argument of
the window-split is converted from vertical to horizontal or vice versa,
with the same proportion of the total split."
  (when (or (not window) (window-live-p window))
    (user-error "No windows to transpose"))
  (let* ((frame (window-frame window))
	 (first-window window)
	 (selected-window (frame-selected-window window))
	 (win-tree (car (window-tree-normal-sizes window)))
	 (win-list (seq-filter #'window-live-p (flatten-list win-tree)))
	 (atom-windows (seq-keep #'window-atom-root win-list)))
    (unless (and (not (eq (car atom-windows) window))
	         (or no-resize
		     (and (not atom-windows)
		          (not (seq-some #'window-fixed-size-p win-list)))))
      (user-error "Cannot rotate windows due to fixed size or atom windows"))
    (delete-dups atom-windows)
    (while (not (window-live-p first-window))
      (setq first-window (window-child first-window)))
    (delete-other-windows-internal first-window window)
    (window--transpose-1 win-tree first-window conf no-resize atom-windows)
    ;; Go back to previously selected window.
    (set-frame-selected-window frame selected-window)
    (mapc #'window-make-atom atom-windows)))

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
	(flen (if (xor no-resize (car subtree))
		  (float (window-pixel-width cwin))
		(float (window-pixel-height cwin)))))
    (mapc
     (pcase-lambda (`(,window . ,size))
       (prog1
	   (let* ((split-size (- (round (* flen size))))
		  (split-type
		   (funcall (if (car subtree) #'car #'cdr) conf))
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
			  (if is-atom '(nil . t) conf)
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
      ;; We need to ignore first 5 elements of window list, we ignore
      ;; window split type, sizes and the first window (it's
      ;; implicitly created).  We just have a list of windows.
      (nreverse (cdr (cddddr subtree)))))
    ;; (caar (cddddr subtree)) is the first child window of subtree.
    (unless (windowp (caar (cddddr subtree)))
      (let ((is-atom (memq (cadr (cadr (cddddr subtree))) atom-windows)))
	(window--transpose-1 (car (cddddr subtree)) cwin
                             (if is-atom '(nil . t) conf)
			     no-resize atom-windows)))))

;;;###autoload
(defun merge-frames (&optional frame1 frame2 vertical)
  "Merge the main window of FRAME2 into FRAME1.
Split the main window of FRAME1 and make the new window display the main
window of FRAME2.  Both FRAME1 and FRAME2 must be live frames.  If
VERTICAL is non-nil, make the new window below the old main window of
FRAME1.  Otherwise, make the new window on the right of FRAME1's main
window.  Interactively, VERTICAL is the prefix argument, FRAME1 is the
selected frame and FRAME2 is the frame following FRAME1 in the frame
list.  Delete FRAME2 if the merge completed successfully and return
FRAME1."
  (interactive "i\ni\nP")
  (let* ((frame1 (window-normalize-frame frame1))
         (frame2 (or (if frame2
                         (window-normalize-frame frame2)
                       (next-frame frame1))
                     (user-error "Cannot find frame to merge"))))
    (window-state-put
     ;; Source window on frame2.
     (window-state-get (window-main-window frame2))
     ;; Make new window on frame1.
     (split-window (window-main-window frame1) nil (not vertical)))
    (delete-frame frame2)
    frame1))

;;;###autoload
(defun window-get-split-combination (window arg)
  "Return window combination suitable for `split-frame'.

WINDOW is the main window in which the combination should be derived.
ARG is the argument passed to `split-frame'.  Return a
combination of windows `split-frame' is considered to split off."
  (let* ((reverse (< arg 0))
	 ;; This is where the pivot window is.
         (total-window-count (window-child-count window))
	 (pivot-window-pos (- (if reverse
				  (+ total-window-count arg)
				arg)
                              1))
	 (pivot-window (window-child window))
	 (active-window (frame-selected-window window))
         ;; If FRAME's selected window is on the left side of the
         ;; pivot window.
	 (active-window-on-left (eq pivot-window active-window)))
    ;; We want the 2nd level window that the active window is a
    ;; part of.
    (while (not (eq (window-parent active-window) window))
      (setq active-window (window-parent active-window)))

    ;; Now we need to find the pivot window
    (dotimes (_ pivot-window-pos)
      (setq pivot-window (window-next-sibling pivot-window))
      (when (eq active-window pivot-window)
	(setq active-window-on-left t)))

    ;; Now we have pivot-window set, and we just need to
    ;; combine.  We want to split away all windows from the
    ;; side of the pivot that doesn't contain the active
    ;; window.
    (let* ((first (window-child window))
	   (last (window-last-child window))
	   (next-pivot-sib (window-next-sibling pivot-window))
	   (right-comb (if (eq next-pivot-sib last)
			   last
			 (combine-windows next-pivot-sib last)))
	   (left-comb (if (eq first pivot-window)
			  first
			(combine-windows first pivot-window))))
      (if active-window-on-left right-comb left-comb))))

;;;###autoload
(defun split-frame (&optional frame arg)
  "Split windows of specified FRAME into two separate frames.
FRAME must be a live frame and defaults to the selected frame.  ARG
specifies the number of windows to consider for splitting and defaults
to 1.  Interactively, ARG is the prefix argument.

First divide the child windows of FRAME's main window into two parts.
The first part includes the first ARG child windows if ARG is positive,
or -ARG last windows if it's negative.  The second part includes the
remaining child windows of FRAME's main window.  Then clone into a
newly-created frame each of the windows of the part which does not
include FRAME's selected window and delete those windows from FRAME.

Signal an error if ARG is either zero or not a number, if FRAME's main
window is live or does not have more child windows than specified by the
absolute value of ARG.  Return the new frame."
  (interactive "i\nP")
  (let* ((frame (window-normalize-frame frame))
         ;; MAIN is FRAME'S main window.
         (main (window-main-window frame))
	 (total-window-count (window-child-count main))
	 (arg (or arg 1)))
    (cond
     ((window-live-p main)
      (user-error "Cannot split frame with only one window"))
     ((or (not (numberp arg)) (zerop arg))
      (user-error "Invalid ARG %s for splitting frame" arg))
     ((>= (abs arg) total-window-count)
      (user-error "ARG %s exceeds number of windows %s that can be split off"
		  (abs arg) (1- total-window-count)))
     (t
      (let ((comb (window-get-split-combination main arg)))
        (window-state-put (window-state-get comb)
			  (window-main-window (make-frame)))
        (delete-window comb))
      ))))

(provide 'window-x)
;;; window-x.el ends here
