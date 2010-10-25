;;; window.el --- GNU Emacs window commands aside from those written in C

;; Copyright (C) 1985, 1989, 1992, 1993, 1994, 2000, 2001, 2002,
;;   2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;; Use `walk-window-tree' instead of `window-list-1' wherever possible.
;; Or maybe not because writing code with wwt is not very transparent.
;; Or better, rewrite wwt as a macro.

;; Use (pop-up-windows (or pop-up-windows t)) instead of (pop-up-windows
;; t) wherever this is locally rebound (has some twenty hits in Elisp
;; sources).

;;; Code:

(eval-when-compile (require 'cl))

(defmacro save-selected-window (&rest body)
  "Execute BODY, then select the previously selected window.
The value returned is the value of the last form in BODY.

This macro saves and restores the selected window, as well as the
selected window in each frame.  If the previously selected window
is no longer live, then whatever window is selected at the end of
BODY remains selected.  If the previously selected window of some
frame is no longer live at the end of BODY, that frame's selected
window is left alone.

This macro saves and restores the current buffer, since otherwise
its normal operation could make a different buffer current.  The
order of recently selected windows and the buffer list ordering
are not altered by this macro (unless they are altered in BODY)."
  (declare (indent 0) (debug t))
  `(let ((save-selected-window-window (selected-window))
	 ;; It is necessary to save all of these, because calling
	 ;; select-window changes frame-selected-window for whatever
	 ;; frame that window is in.
	 (save-selected-window-alist
	  (mapcar (lambda (frame) (cons frame (frame-selected-window frame)))
		  (frame-list))))
     (save-current-buffer
       (unwind-protect
	   (progn ,@body)
	 (dolist (elt save-selected-window-alist)
	   (and (frame-live-p (car elt))
		(window-live-p (cdr elt))
		(set-frame-selected-window (car elt) (cdr elt) 'norecord)))
	 (when (window-live-p save-selected-window-window)
	   (select-window save-selected-window-window 'norecord))))))

;; The following two functions are like `window-next' and `window-prev'
;; but the WINDOW argument is _not_ optional (so they don't substitute
;; the selected window for nil), and they return nil when WINDOW doesn't
;; have a parent (like a frame's root window or a minibuffer window).
(defsubst window-right (window)
  "Return WINDOW's right sibling.
Return nil if WINDOW is the root window of its frame.  WINDOW can
be any window."
  (and window (window-parent window) (window-next window)))

(defsubst window-left (window)
  "Return WINDOW's left sibling.
Return nil if WINDOW is the root window of its frame.  WINDOW can
be any window."
  (and window (window-parent window) (window-prev window)))

(defsubst window-child (window)
  "Return WINDOW's first child window."
  (or (window-vchild window) (window-hchild window)))

(defsubst window-internal-p (object)
  "Return t if OBJECT is an internal window and nil otherwise.
An internal window is a window that has either a vertical or a
horizontal child window."
  (and (windowp object) (window-child object) t))

(defsubst window-any-p (object)
  "Return t if OBJECT denotes a live or internal window."
  (and (windowp object)
       (or (window-buffer object) (window-child object))
       t))

(defsubst normalize-live-buffer (buffer-or-name)
  "Return buffer specified by BUFFER-OR-NAME.
BUFFER-OR-NAME must be either a buffer or a string naming a live
buffer and defaults to the current buffer."
  (cond
   ((not buffer-or-name)
    (current-buffer))
   ((bufferp buffer-or-name)
    (if (buffer-live-p buffer-or-name)
	buffer-or-name
      (error "Buffer %s is not a live buffer" buffer-or-name)))
   ((get-buffer buffer-or-name))
   (t
    (error "No such buffer %s" buffer-or-name))))

;; This should probably go to frame.el.
(defsubst normalize-live-frame (frame)
  "Return normalized FRAME argument for live frames."
  (if frame
      (if (frame-live-p frame)
	  frame
	(error "%s is not a live frame" frame))
    (selected-frame)))

(defsubst normalize-any-window (window)
  "Return normalized WINDOW argument for any window.
WINDOW defaults to the selected window."
  (if window
      (if (window-any-p window)
	  window
	(error "%s is not a window" window))
    (selected-window)))

(defsubst normalize-live-window (window)
  "Return normalized WINDOW argument for live windows.
WINDOW defaults to the selected window."
  (if window
      (if (and (windowp window) (window-buffer window))
	  window
	(error "%s is not a live window" window))
    (selected-window)))

(defvar ignore-window-parameters nil
  "If non-nil standard functions ignore window parameters.
The functions currently affected by this are `split-window',
`delete-window', `delete-other-windows' and `other-window'.

When this variable equals `pre', parameters are not consulted
before but are updated after performing the requested operation.
When this variable equals `post', parameters are consulted before
but are not updated after performing the requested operation.

The value t means parameters are not consulted before and not
updated after performing the requested operation.  Currently any
other non-nil value is handled like t.

An application may bind this to a non-nil value around calls to
these functions.  If it does so and the value is not `pre', the
application is fully responsible for correctly setting the
parameters of all windows participating in the function called.")

(defconst window-safe-min-height 1
  "The absolut minimum number of lines of a window.
Anything less might crash Emacs.")

(defcustom window-min-height 4
  "The minimum number of lines of any window.
The value has to accomodate a mode- or header-line if present.  A
value less than `window-safe-min-height' is ignored.  The value
of this variable is honored when windows are resized or split.

Applications should never rebind this variable.  To resize a
window to a height less than the one specified here, an
application should instead call `resize-window' with a non-nil
IGNORE argument.  In order to have `split-window' make a window
shorter, explictly specify the SIZE argument of that function."
  :type 'integer
  :version "24.1"
  :group 'windows)

(defconst window-safe-min-width 2
  "The absolut minimum number of columns of a window.
Anything less might crash Emacs.")

(defcustom window-min-width 10
  "The minimum number of columns of any window.
The value has to accomodate margins, fringes, or scrollbars if
present.  A value less than `window-safe-min-width' is ignored.
The value of this variable is honored when windows are resized or
split.

Applications should never rebind this variable.  To resize a
window to a width less than the one specified here, an
application should instead call `resize-window' with a non-nil
IGNORE argument.  In order to have `split-window' make a window
narrower, explictly specify the SIZE argument of that function."
  :type 'integer
  :version "24.1"
  :group 'windows)

(defsubst window-iso-combined-p (&optional window horizontal)
  "Return non-nil if and only if WINDOW is vertically combined.
WINDOW can be any window and defaults to the selected one.
Optional argument HORIZONTAL non-nil means return non-nil if and
only if WINDOW is horizontally combined."
  (setq window (normalize-any-window window))
  (when (window-parent window)
    (if horizontal
	(window-hchild (window-parent window))
      (window-vchild (window-parent window)))))

(defvar window-size-fixed nil
  "Non-nil in a buffer means windows displaying the buffer are fixed-size.
If the value is `height', then only the window's height is fixed.
If the value is `width', then only the window's width is fixed.
Any other non-nil value fixes both the width and the height.
Emacs won't change the size of any window displaying that buffer,
unless you explicitly change the size, or Emacs has no other
choice \(like when deleting a neighboring window).")
(make-variable-buffer-local 'window-size-fixed)

(defsubst window-size-ignore (window ignore)
  "Return non-nil if IGNORE says to ignore size restrictions for WINDOW."
  (if (window-any-p ignore) (eq window ignore) ignore))

(defun window-min-size (&optional window horizontal ignore)
  "Return the minimum number of lines of WINDOW.
WINDOW can be an arbitrary window and defaults to the selected
one.  Optional argument HORIZONTAL non-nil means return the
minimum number of columns of WINDOW.

Optional argument IGNORE non-nil means ignore any restrictions
imposed by fixed size windows, `window-min-height' or
`window-min-width' settings.  IGNORE equal `safe' means live
windows may get as small as `window-safe-min-height' lines and
`window-safe-min-width' columns.  IGNORE a window means ignore
restrictions for that window only."
  (window-min-size-1
   (normalize-any-window window) horizontal ignore))

(defun window-min-size-1 (window horizontal ignore)
  "Internal function of `window-min-size'."
  (let ((sub (window-child window)))
    (if sub
	(let ((value 0))
	  ;; WINDOW is an internal window.
	  (if (window-iso-combined-p sub horizontal)
	      ;; The minimum size of an iso-combination is the sum of
	      ;; the minimum sizes of its subwindows.
	      (while sub
		(setq value (+ value
			       (window-min-size-1 sub horizontal ignore)))
		(setq sub (window-right sub)))
	    ;; The minimum size of an ortho-combination is the maximum of
	    ;; the minimum sizes of its subwindows.
	    (while sub
	      (setq value (max value
			       (window-min-size-1 sub horizontal ignore)))
	      (setq sub (window-right sub))))
	  value)
      (with-current-buffer (window-buffer window)
	(cond
	 ((and (not (window-size-ignore window ignore))
	       (window-size-fixed-p window horizontal))
	  ;; The minimum size of a fixed size window is its size.
	  (window-total-size window horizontal))
	 ((or (eq ignore 'safe) (eq ignore window))
	  ;; If IGNORE equals `safe' or WINDOW return the safe values.
	  (if horizontal window-safe-min-width window-safe-min-height))
	 (horizontal
	  ;; For the minimum width of a window take fringes and
	  ;; scroll-bars into account.  This is questionable and should
	  ;; be removed as soon as we are able to split (and resize)
	  ;; windows such that the new (or resized) windows can get a
	  ;; size less than the user-specified `window-min-height' and
	  ;; `window-min-width'.
	  (let ((frame (window-frame window))
		(fringes (window-fringes window))
		(scroll-bars (window-scroll-bars window)))
	    (max
	     (+ window-safe-min-width
		(ceiling (car fringes) (frame-char-width frame))
		(ceiling (cadr fringes) (frame-char-width frame))
		(cond
		 ((memq (nth 2 scroll-bars) '(left right))
		  (nth 1 scroll-bars))
		 ((memq (frame-parameter frame 'vertical-scroll-bars)
			'(left right))
		  (ceiling (or (frame-parameter frame 'scroll-bar-width) 14)
			   (frame-char-width)))
		 (t 0)))
	     (if (and (not (window-size-ignore window ignore))
		      (numberp window-min-width))
		 window-min-width
	       0))))
	 (t
	  ;; For the minimum height of a window take any mode- or
	  ;; header-line into account.
	  (max (+ window-safe-min-height
		  (if header-line-format 1 0)
		  (if mode-line-format 1 0))
	       (if (and (not (window-size-ignore window ignore))
			(numberp window-min-height))
		   window-min-height
		 0))))))))

(defun window-sizable (window delta &optional horizontal ignore)
  "Return DELTA if DELTA lines can be added to WINDOW.
Optional argument HORIZONTAL non-nil means return DELTA if DELTA
columns can be added to WINDOW.  A return value of zero means
that no lines (or columns) can be added to WINDOW.

This function looks only at WINDOW and its subwindows.  The
function `window-resizable' looks at other windows as well.

DELTA positive means WINDOW shall be enlarged by DELTA lines or
columns.  If WINDOW cannot be enlarged by DELTA lines or columns
return the maximum value in the range 0..DELTA by which WINDOW
can be enlarged.

DELTA negative means WINDOW shall be shrunk by -DELTA lines or
columns.  If WINDOW cannot be shrunk by -DELTA lines or columns,
return the minimum value in the range DELTA..0 by which WINDOW
can be shrunk.

Optional argument IGNORE non-nil means ignore any restrictions
imposed by fixed size windows, `window-min-height' or
`window-min-width' settings.  IGNORE equal `safe' means live
windows may get as small as `window-safe-min-height' lines and
`window-safe-min-width' columns.  IGNORE any window means ignore
restrictions for that window only."
  (setq window (normalize-any-window window))
  (cond
   ((< delta 0)
    (max (- (window-min-size window horizontal ignore)
	    (window-total-size window horizontal))
	 delta))
   ((window-size-ignore window ignore)
    delta)
   ((> delta 0)
    (if (window-size-fixed-p window horizontal)
	0
      delta))
   (t 0)))

(defsubst window-sizable-p (window delta &optional horizontal ignore)
  "Return t if WINDOW can have DELTA lines.
For the meaning of the arguments of this function see the
doc-string of `window-sizable'."
  (setq window (normalize-any-window window))
  (if (> delta 0)
      (>= (window-sizable window delta horizontal ignore) delta)
    (<= (window-sizable window delta horizontal ignore) delta)))

(defun window-size-fixed-p (&optional window horizontal)
  "Return non-nil if WINDOW's height is fixed.
WINDOW can be an arbitrary window and defaults to the selected
window.  Optional argument HORIZONTAL non-nil means return
non-nil if WINDOW's width is fixed.

If this function returns nil, this does not necessarily mean that
WINDOW can be resized in the desired direction.  The functions
`window-resizable' and `window-resizable-p' will tell that."
  (window-size-fixed-1
   (normalize-any-window window) horizontal))

(defun window-size-fixed-1 (window horizontal)
  "Internal function for `window-size-fixed-p'."
  (let ((sub (window-child window)))
    (catch 'fixed
      (if sub
	  ;; WINDOW is an internal window.
	  (if (window-iso-combined-p sub horizontal)
	      ;; An iso-combination is fixed size if all its subwindows
	      ;; are fixed-size.
	      (progn
		(while sub
		  (unless (window-size-fixed-1 sub horizontal)
		    ;; We found a non-fixed-size subwindow, so WINDOW's
		    ;; size is not fixed.
		    (throw 'fixed nil))
		  (setq sub (window-right sub)))
		;; All subwindows are fixed-size, so WINDOW's size is
		;; fixed.
		(throw 'fixed t))
	    ;; An ortho-combination is fixed-size if at least one of its
	    ;; subwindows is fixed-size.
	    (while sub
	      (when (window-size-fixed-1 sub horizontal)
		;; We found a fixed-size subwindow, so WINDOW's size is
		;; fixed.
		(throw 'fixed t))
	      (setq sub (window-right sub))))
	;; WINDOW is a live window.
	(with-current-buffer (window-buffer window)
	  (if horizontal
	      (memq window-size-fixed '(width t))
	    (memq window-size-fixed '(height t))))))))

(defun window-min-delta (&optional window horizontal ignore side noup nodown)
  "Return number of lines by which WINDOW can be shrunk.
WINDOW can be an arbitrary window and defaults to the selected
window.  Return zero if WINDOW cannot be shrunk.

Optional argument HORIZONTAL non-nil means return number of
columns by which WINDOW can be shrunk.

Optional argument IGNORE non-nil means ignore any restrictions
imposed by fixed size windows, `window-min-height' or
`window-min-width' settings.  IGNORE a window means ignore
restrictions for that window only.  IGNORE equal `safe' means
live windows may get as small as `window-safe-min-height' lines
and `window-safe-min-width' columns.

Optional argument SIDE `left' means assume only windows to the
left of or above WINDOW can be enlarged.  Optional argument SIDE
`right' means assumes only windows to the right of or below
WINDOW can be enlarged.

Optional argument NOUP non-nil means don't go up in the window
tree but try to enlarge windows within WINDOW's combination only.

Optional argument NODOWN non-nil means don't check whether WINDOW
and its subwindows can be shrunk."
  (setq window (normalize-any-window window))
  (let ((size (window-total-size window horizontal))
	(minimum (window-min-size window horizontal ignore)))
    (if (and (not nodown) (= size minimum))
	;; Nothing to recover.
	0
      (window-min-delta-1
       ;; Think positive.
       window (- size minimum) horizontal ignore side noup))))

(defun window-min-delta-1 (window delta &optional horizontal ignore side noup)
  "Internal function for `window-min-delta'."
  (if (not (window-parent window))
      0 ; delta
    ;;; (min delta
	   ;;; (- (window-total-size window horizontal)
	      ;;; (window-min-size window horizontal ignore)))
    (let* ((parent (window-parent window))
	   (sub (window-child parent)))
      (catch 'done
	(if (window-iso-combined-p sub horizontal)
	    ;; In an iso-combination throw DELTA if we find at least one
	    ;; subwindow and that subwindow is either non-fixed-size or
	    ;; we can ignore fixed-sizeness.
	    (let ((skip (eq side 'right)))
	      (while sub
		(cond
		 ((eq sub window)
		  (setq skip (eq side 'left)))
		 (skip)
		 ((and (not (window-size-ignore window ignore))
		       (window-size-fixed-p sub horizontal)))
		 (t
		  ;; We found a non-fixed-size subwindow.
		  (throw 'done delta)))
		(setq sub (window-right sub))))
	  ;; In an ortho-combination set DELTA to the minimum value by
	  ;; which other subwindows can shrink.
	  (while sub
	    (unless (eq sub window)
	      (setq delta
		    (min delta
			 (- (window-total-size sub horizontal)
			    (window-min-size sub horizontal ignore)))))
	    (setq sub (window-right sub))))
	(if noup
	    delta
	  (window-min-delta-1 parent delta horizontal ignore side))))))

(defun window-max-delta (&optional window horizontal ignore side noup nodown)
  "Return maximum number of lines WINDOW by which WINDOW can be enlarged.
WINDOW can be an arbitrary window and defaults to the selected
window.  The return value is zero if WINDOW cannot be enlarged.

Optional argument HORIZONTAL non-nil means return maximum number
of columns by which WINDOW can be enlarged.

Optional argument IGNORE non-nil means ignore any restrictions
imposed by fixed size windows, `window-min-height' or
`window-min-width' settings.  IGNORE a window means ignore
restrictions for that window only.  IGNORE equal `safe' means
live windows may get as small as `window-safe-min-height' lines
and `window-safe-min-width' columns.

Optional argument SIDE `left' means assume only windows to the
left of or below WINDOW can be shrunk.  Optional argument SIDE
`right' means assumes only windows to the right of or above
WINDOW can be shrunk.

Optional argument NOUP non-nil means don't go up in the window
tree but try to obtain the entire space from windows within
WINDOW's combination.

Optional argument NODOWN non-nil means do not check whether
WINDOW and its subwindows can be enlarged."
  (setq window (normalize-any-window window))
  (if (and (not (window-size-ignore window ignore))
	   (not nodown) (window-size-fixed-p window horizontal))
      0
    (window-max-delta-1 window 0 horizontal ignore side noup)))

(defun window-max-delta-1 (window delta &optional horizontal ignore side noup)
  "Internal function of `window-max-delta'."
  (if (not (window-parent window))
      ;; Can't go up.  Return DELTA.
      delta
    (let* ((parent (window-parent window))
	   (sub (window-child parent)))
      (catch 'fixed
	(if (window-iso-combined-p sub horizontal)
	    ;; For an iso-combination calculate how much we can get from
	    ;; other subwindows.
	    (let ((skip (eq side 'right)))
	      (while sub
		(cond
		 ((eq sub window)
		  (setq skip (eq side 'left)))
		 (skip)
		 (t
		  (setq delta
			(+ delta
			   (- (window-total-size sub horizontal)
			      (window-min-size sub horizontal ignore))))))
		(setq sub (window-right sub))))
	  ;; For an ortho-combination throw DELTA when at least one
	  ;; subwindow is fixed-size.
	  (while sub
	    (when (and (not (eq sub window))
		       (not (window-size-ignore sub ignore))
		       (window-size-fixed-p sub horizontal))
	      (throw 'fixed delta))
	    (setq sub (window-right sub))))
	(if noup
	    delta
	  ;; Try to go up.
	  (window-max-delta-1 parent delta horizontal ignore side))))))

;; Make NOUP also inhibit the min-size check.
(defun window-resizable (window delta &optional horizontal ignore side noup nodown)
  "Return DELTA if WINDOW can be resized vertically by DELTA lines.
Optional argument HORIZONTAL non-nil means return DELTA if WINDOW
can be resized horizontally by DELTA columns.  A return value of
zero means that WINDOW is not resizable.

DELTA positive means WINDOW shall be enlarged by DELTA lines or
columns.  If WINDOW cannot be enlarged by DELTA lines or columns
return the maximum value in the range 0..DELTA by which WINDOW
can be enlarged.

DELTA negative means WINDOW shall be shrunk by -DELTA lines or
columns.  If WINDOW cannot be shrunk by -DELTA lines or columns,
return the minimum value in the range DELTA..0 that can be used
for shrinking WINDOW.

Optional argument IGNORE non-nil means ignore any restrictions
imposed by fixed size windows, `window-min-height' or
`window-min-width' settings.  IGNORE a window means ignore
restrictions for that window only.  IGNORE equal `safe' means
live windows may get as small as `window-safe-min-height' lines
and `window-safe-min-width' columns.

Optional argument NOUP non-nil means don't go up in the window
tree but try to distribute the space among the other windows
within WINDOW's combination.

Optional argument NODOWN non-nil means don't check whether WINDOW
and its subwindows can be resized."
  (setq window (normalize-any-window window))
  (cond
   ((< delta 0)
    (max (- (window-min-delta window horizontal ignore side noup nodown))
	 delta))
   ((> delta 0)
    (min (window-max-delta window horizontal ignore side noup nodown)
	 delta))
   (t 0)))

(defun window-resizable-p (window delta &optional horizontal ignore side noup nodown)
  "Return t if WINDOW can be resized vertically by DELTA lines.
For the meaning of the arguments of this function see the
doc-string of `window-resizable'."
  (setq window (normalize-any-window window))
  (if (> delta 0)
      (>= (window-resizable window delta horizontal ignore side noup nodown)
	  delta)
    (<= (window-resizable window delta horizontal ignore side noup nodown)
	delta)))

(defsubst window-total-height (&optional window)
  "Return the total number of lines of WINDOW.
WINDOW can be any window and defaults to the selected one.  The
return value includes WINDOW's mode line and header line, if any.
If WINDOW is internal the return value is the sum of the total
number of lines of WINDOW's child windows if these are vertically
combined and the height of WINDOW's first child otherwise.

Note: This function does not take into account the value of
`line-spacing' when calculating the number of lines in WINDOW."
  (window-total-size window))

;; Eventually we should make `window-height' obsolete.
(defalias 'window-height 'window-total-height)

;; See discussion in bug#4543.
(defsubst window-full-height-p (&optional window)
  "Return t if WINDOW is as high as the containing frame.
More precisely, return t if and only if the total height of
WINDOW equals the total height of the root window of WINDOW's
frame.  WINDOW can be any window and defaults to the selected
one."
  (setq window (normalize-any-window window))
  (= (window-total-size window)
     (window-total-size (frame-root-window window))))

(defsubst window-total-width (&optional window)
  "Return the total number of columns of WINDOW.
WINDOW can be any window and defaults to the selected one.  The
return value includes any vertical dividers or scrollbars of
WINDOW.  If WINDOW is internal, the return value is the sum of
the total number of columns of WINDOW's child windows if these
are horizontally combined and the width of WINDOW's first child
otherwise."
  (window-total-size window t))

(defsubst window-full-width-p (&optional window)
  "Return t if WINDOW is as wide as the containing frame.
More precisely, return t if and only if the total width of WINDOW
equals the total width of the root window of WINDOW's frame.
WINDOW can be any window and defaults to the selected one."
  (setq window (normalize-any-window window))
  (= (window-total-size window t)
     (window-total-size (frame-root-window window) t)))

(defsubst window-body-height (&optional window)
  "Return the number of lines of WINDOW's body.
WINDOW must be a live window and defaults to the selected one.

The return value does not include WINDOW's mode line and header
line, if any.  If a line at the bottom of the window is only
partially visible, that line is included in the return value.  If
you do not want to include a partially visible bottom line in the
return value, use `window-text-height' instead."
  (window-body-size window))

(defsubst window-body-width (&optional window)
  "Return the number of columns of WINDOW's body.
WINDOW must be a live window and defaults to the selected one.

The return value does not include any vertical dividers or scroll
bars owned by WINDOW.  On a window-system the return value does
not include the number of columns used for WINDOW's fringes or
display margins either."
  (window-body-size window t))

;; Eventually we should make `window-height' obsolete.
(defalias 'window-width 'window-body-width)

(defun window-current-scroll-bars (&optional window)
  "Return the current scroll bar settings for WINDOW.
WINDOW must be a live window and defaults to the selected one.

The return value is a cons cell (VERTICAL . HORIZONTAL) where
VERTICAL specifies the current location of the vertical scroll
bars (`left', `right', or nil), and HORIZONTAL specifies the
current location of the horizontal scroll bars (`top', `bottom',
or nil).

Unlike `window-scroll-bars', this function reports the scroll bar
type actually used, once frame defaults and `scroll-bar-mode' are
taken into account."
  (setq window (normalize-live-window window))
  (let ((vert (nth 2 (window-scroll-bars window)))
	(hor nil))
    (when (or (eq vert t) (eq hor t))
      (let ((fcsb (frame-current-scroll-bars (window-frame window))))
	(if (eq vert t)
	    (setq vert (car fcsb)))
	(if (eq hor t)
	    (setq hor (cdr fcsb)))))
    (cons vert hor)))

(defun walk-windows (proc &optional minibuf all-frames)
  "Cycle through all live windows, calling PROC for each one.
PROC must specify a function with a window as its sole argument.
The optional arguments MINIBUF and ALL-FRAMES specify the set of
windows to include in the walk.

MINIBUF t means include the minibuffer window even if the
minibuffer is not active.  MINIBUF nil or omitted means include
the minibuffer window only if the minibuffer is active.  Any
other value means do not include the minibuffer window even if
the minibuffer is active.

ALL-FRAMES nil or omitted means consider all windows on WINDOW's
frame, plus the minibuffer window if specified by the MINIBUF
argument.  If the minibuffer counts, consider all windows on all
frames that share that minibuffer too.  The following non-nil
values of ALL-FRAMES have special meanings:

- t means consider all windows on all existing frames.

- `visible' means consider all windows on all visible frames.

- 0 (the number zero) means consider all windows on all visible
  and iconified frames.

- A frame means consider all windows on that frame only.

Anything else means consider all windows on WINDOW's frame and no
others.

This function changes neither the order of recently selected
windows nor the buffer list."
  ;; If we start from the minibuffer window, don't fail to come
  ;; back to it.
  (when (window-minibuffer-p (selected-window))
    (setq minibuf t))
  ;; Make sure to not mess up the order of recently selected
  ;; windows.  Use `save-selected-window' and `select-window'
  ;; with second argument non-nil for this purpose.
  (save-selected-window
    (when (framep all-frames)
      (select-window (frame-first-window all-frames) 'norecord))
    (dolist (walk-windows-window (window-list-1 nil minibuf all-frames))
      (funcall proc walk-windows-window))))

(defun walk-window-tree-1 (proc walk-window-tree-window any)
  "Helper function for `walk-window-tree'."
  (let (walk-window-tree-buffer)
    (while walk-window-tree-window
      (setq walk-window-tree-buffer
	    (window-buffer walk-window-tree-window))
      (when (or walk-window-tree-buffer any)
	(funcall proc walk-window-tree-window))
      (unless walk-window-tree-buffer
	(walk-window-tree-1
	 proc (window-hchild walk-window-tree-window) any)
	(walk-window-tree-1
	 proc (window-vchild walk-window-tree-window) any))
      (setq walk-window-tree-window
	    (window-right walk-window-tree-window)))))

(defun walk-window-tree (proc &optional frame any)
  "Run function PROC on each live window of FRAME.
PROC must be a function with one argument - a window.  FRAME must
be a live frame and defaults to the selected one.  ANY, if
non-nil means to run PROC on all live and internal windows of
FRAME.

This function performs a pre-order, depth-first traversal of the
window tree.  If PROC changes the window tree, the result is
unpredictable."
  (let ((walk-window-tree-frame (normalize-live-frame frame)))
    (walk-window-tree-1
     proc (frame-root-window walk-window-tree-frame) any)))

(defun window-in-direction-2 (window posn &optional horizontal)
  "Support function for `window-in-direction'."
  (if horizontal
      (let ((top (window-top-line window)))
	(if (> top posn)
	    (- top posn)
	  (- posn top (window-total-height window))))
    (let ((left (window-left-column window)))
      (if (> left posn)
	  (- left posn)
	(- posn left (window-total-width window))))))

(defun window-in-direction (direction &optional window ignore)
  "Return window in DIRECTION as seen from WINDOW.
DIRECTION must be one of `above', `below', `left' or `right'.
WINDOW must be a live window and defaults to the selected one.
IGNORE, when non-nil means a window can be returned even if its
`no-other-window' parameter is non-nil."
  (setq window (normalize-live-window window))
  (unless (memq direction '(above below left right))
    (error "Wrong direction %s" direction))
  (let* ((frame (window-frame window))
	 (hor (memq direction '(left right)))
	 (first (if hor
		    (window-left-column window)
		  (window-top-line window)))
	 (last (+ first (if hor
			    (window-total-width window)
			  (window-total-height window))))
	 (posn-cons (nth 6 (posn-at-point (window-point window) window)))
	 (posn (if hor
		   (+ (cdr posn-cons) (window-top-line window))
		 (+ (car posn-cons) (window-left-column window))))
	 (best-edge
	  (cond
	   ((eq direction 'below) (frame-height frame))
	   ((eq direction 'right) (frame-width frame))
	   (t -1)))
	 (best-edge-2 best-edge)
	 (best-diff-2 (if hor (frame-height frame) (frame-width frame)))
	 best best-2 best-diff-2-new)
    (walk-window-tree
     (lambda (w)
       (let* ((w-top (window-top-line w))
	      (w-left (window-left-column w)))
	 (cond
	  ((or (eq window w)
	       ;; Ignore ourselves.
	       (and (window-parameter w 'no-other-window)
		    ;; Ignore W unless IGNORE is non-nil.
		    (not ignore))))
	  (hor
	   (cond
	    ((and (<= w-top posn)
		  (< posn (+ w-top (window-total-height w))))
	     ;; W is to the left or right of WINDOW and covers POSN.
	     (when (or (and (eq direction 'left)
			    (<= w-left first) (> w-left best-edge))
		       (and (eq direction 'right)
			    (>= w-left last) (< w-left best-edge)))
	       (setq best-edge w-left)
	       (setq best w)))
	    ((and (or (and (eq direction 'left)
			   (<= (+ w-left (window-total-width w)) first))
		      (and (eq direction 'right) (<= last w-left)))
		  ;; W is to the left or right of WINDOW but does not
		  ;; cover POSN.
		  (setq best-diff-2-new
			(window-in-direction-2 w posn hor))
		  (or (< best-diff-2-new best-diff-2)
		      (and (= best-diff-2-new best-diff-2)
			   (if (eq direction 'left)
			       (> w-left best-edge-2)
			     (< w-left best-edge-2)))))
	     (setq best-edge-2 w-left)
	     (setq best-diff-2 best-diff-2-new)
	     (setq best-2 w))))
	  (t
	   (cond
	    ((and (<= w-left posn)
		  (< posn (+ w-left (window-total-width w))))
	     ;; W is above or below WINDOW and covers POSN.
	     (when (or (and (eq direction 'above)
			    (<= w-top first) (> w-top best-edge))
		       (and (eq direction 'below)
			    (>= w-top first) (< w-top best-edge)))
	       (setq best-edge w-top)
	       (setq best w)))
	    ((and (or (and (eq direction 'above)
			   (<= (+ w-top (window-total-height w)) first))
		      (and (eq direction 'below) (<= last w-top)))
		  ;; W is above or below WINDOW but does not cover POSN.
		  (setq best-diff-2-new
			(window-in-direction-2 w posn hor))
		  (or (< best-diff-2-new best-diff-2)
		      (and (= best-diff-2-new best-diff-2)
			   (if (eq direction 'above)
			       (> w-top best-edge-2)
			     (< w-top best-edge-2)))))
	     (setq best-edge-2 w-top)
	     (setq best-diff-2 best-diff-2-new)
	     (setq best-2 w)))))))
     (window-frame window))
    (or best best-2)))

(defun get-window-with-predicate (predicate &optional minibuf
					    all-frames default)
  "Return a live window satisfying PREDICATE.
More precisely, cycle through all windows calling the function
PREDICATE on each one of them with the window as its sole
argument.  Return the first window for which PREDICATE returns
non-nil.  If no window satisfies PREDICATE, return DEFAULT.

ALL-FRAMES nil or omitted means consider all windows on WINDOW's
frame, plus the minibuffer window if specified by the MINIBUF
argument.  If the minibuffer counts, consider all windows on all
frames that share that minibuffer too.  The following non-nil
values of ALL-FRAMES have special meanings:

- t means consider all windows on all existing frames.

- `visible' means consider all windows on all visible frames.

- 0 (the number zero) means consider all windows on all visible
  and iconified frames.

- A frame means consider all windows on that frame only.

Anything else means consider all windows on WINDOW's frame and no
others."
  (catch 'found
    (dolist (window (window-list-1 nil minibuf all-frames))
      (when (funcall predicate window)
	(throw 'found window)))
    default))

(defalias 'some-window 'get-window-with-predicate)

(defun get-lru-window (&optional all-frames dedicated)
   "Return the least recently used window on frames specified by ALL-FRAMES.
Return a full-width window if possible.  A minibuffer window is
never a candidate.  A dedicated window is never a candidate
unless DEDICATED is non-nil, so if all windows are dedicated, the
value is nil.  Avoid returning the selected window if possible.

The following non-nil values of the optional argument ALL-FRAMES
have special meanings:

- t means consider all windows on all existing frames.

- `visible' means consider all windows on all visible frames.

- 0 (the number zero) means consider all windows on all visible
    and iconified frames.

- A frame means consider all windows on that frame only.

Any other value of ALL-FRAMES means consider all windows on the
selected frame and no others."
   (let (best-window best-time second-best-window second-best-time time)
    (dolist (window (window-list-1 nil nil all-frames))
      (when (or dedicated (not (window-dedicated-p window)))
	(setq time (window-use-time window))
	(if (or (eq window (selected-window))
		(not (window-full-width-p window)))
	    (when (or (not second-best-time) (< time second-best-time))
	      (setq second-best-time time)
	      (setq second-best-window window))
	  (when (or (not best-time) (< time best-time))
	    (setq best-time time)
	    (setq best-window window)))))
    (or best-window second-best-window)))

(defun get-mru-window (&optional all-frames)
   "Return the least recently used window on frames specified by ALL-FRAMES.
Do not return a minibuffer window.

The following non-nil values of the optional argument ALL-FRAMES
have special meanings:

- t means consider all windows on all existing frames.

- `visible' means consider all windows on all visible frames.

- 0 (the number zero) means consider all windows on all visible
    and iconified frames.

- A frame means consider all windows on that frame only.

Any other value of ALL-FRAMES means consider all windows on the
selected frame and no others."
   (let (best-window best-time time)
    (dolist (window (window-list-1 nil nil all-frames))
      (setq time (window-use-time window))
      (when (or (not best-time) (> time best-time))
	(setq best-time time)
	(setq best-window window)))
    best-window))

(defun get-largest-window (&optional all-frames dedicated)
  "Return the largest window on frames specified by ALL-FRAMES.
A minibuffer window is never a candidate.  A dedicated window is
never a candidate unless DEDICATED is non-nil, so if all windows
are dedicated, the value is nil.

The following non-nil values of the optional argument ALL-FRAMES
have special meanings:

- t means consider all windows on all existing frames.

- `visible' means consider all windows on all visible frames.

- 0 (the number zero) means consider all windows on all visible
    and iconified frames.

- A frame means consider all windows on that frame only.

Any other value of ALL-FRAMES means consider all windows on the
selected frame and no others."
  (let ((best-size 0)
	best-window size)
    (dolist (window (window-list-1 nil nil all-frames))
      (when (or dedicated (not (window-dedicated-p window)))
	(setq size (* (window-total-size window)
		      (window-total-size window t)))
	(when (> size best-size)
	  (setq best-size size)
	  (setq best-window window))))
    best-window))

;; The following is what `get-buffer-window' would look like if it were
;; implemented in Elisp.  Since this function is needed for dumping,
;; leave it in C.

;; (defun get-buffer-window (&optional buffer-or-name all-frames)
;;   "Return a window currently displaying BUFFER-OR-NAME, or nil if none.
;; BUFFER-OR-NAME may be a buffer or a buffer name and defaults to
;; the current buffer.

;; The following non-nil values of the optional argument ALL-FRAMES
;; have special meanings:
;; - t means consider all windows on all existing frames.
;; - `visible' means consider all windows on all visible frames.
;; - 0 (the number zero) means consider all windows on all visible
;;     and iconified frames.
;; - A frame means consider all windows on that frame only.
;; Any other value of ALL-FRAMES means consider all windows on the
;; selected frame and no others."
;;   (let ((buffer (get-buffer buffer-or-name))
;; 	best-window)
;;      (when (bufferp buffer)
;;        (dolist (window (window-list-1 nil t all-frames))
;; 	 (when (and (eq (window-buffer window) buffer)
;; 		    ;; The following SHOULD have been handled by
;; 		    ;; `window-list-1' already ...
;; 		    (or (not (window-minibuffer-p window))
;; 			;; Don't find any minibuffer window except the
;; 			;; one that is currently in use.
;; 			(eq window (minibuffer-window)))
;; 		    (or (not best-window)
;; 			;; Prefer to return selected window.
;; 			(eq window (selected-window))
;; 			;; Prefer windows on selected frame.
;; 			(eq (window-frame window) (selected-frame))))
;; 	   (setq best-window window))))
;;      best-window))

(defun get-buffer-window-list (&optional buffer-or-name minibuf all-frames)
  "Return list of all windows displaying BUFFER-OR-NAME, or nil if none.
BUFFER-OR-NAME may be a buffer or the name of an existing buffer
and defaults to the current buffer.

Any windows showing BUFFER-OR-NAME on the selected frame are listed
first.

MINIBUF t means include the minibuffer window even if the
minibuffer is not active.  MINIBUF nil or omitted means include
the minibuffer window only if the minibuffer is active.  Any
other value means do not include the minibuffer window even if
the minibuffer is active.

ALL-FRAMES nil or omitted means consider all windows on WINDOW's
frame, plus the minibuffer window if specified by the MINIBUF
argument.  If the minibuffer counts, consider all windows on all
frames that share that minibuffer too.  The following non-nil
values of ALL-FRAMES have special meanings:

- t means consider all windows on all existing frames.

- `visible' means consider all windows on all visible frames.

- 0 (the number zero) means consider all windows on all visible
  and iconified frames.

- A frame means consider all windows on that frame only.

Anything else means consider all windows on WINDOW's frame and no
others."
  (let ((buffer (normalize-live-buffer buffer-or-name))
	windows)
    (dolist (window (window-list-1 (frame-first-window) minibuf all-frames))
      (when (eq (window-buffer window) buffer)
	(setq windows (cons window windows))))
    (nreverse windows)))

(defun minibuffer-window-active-p (window)
  "Return t if WINDOW is the currently active minibuffer window."
  (eq window (active-minibuffer-window)))

(defun count-windows (&optional minibuf)
   "Return the number of live windows on the selected frame.
The optional argument MINIBUF specifies whether the minibuffer
window shall be counted.  See `walk-windows' for the precise
meaning of this argument."
   (length (window-list-1 nil minibuf)))

;;; Resizing windows.
(defun resize-window-reset (&optional frame horizontal)
  "Reset resize values for all windows on FRAME.
FRAME defaults to the selected frame.

This function stores the current value of `window-total-size' applied
with argument HORIZONTAL in the new total size of all windows on
FRAME.  It also resets the new normal size of each of these
windows."
  (resize-window-reset-1
   (frame-root-window (normalize-live-frame frame)) horizontal))

(defun resize-window-reset-1 (window horizontal)
  "Internal function of `resize-window-reset'."
  ;; Register old size in the new total size.
  (resize-window-total window (window-total-size window horizontal))
  ;; Reset new normal size.
  (resize-window-normal window)
  (when (window-child window)
    (resize-window-reset-1 (window-child window) horizontal))
  (when (window-right window)
    (resize-window-reset-1 (window-right window) horizontal)))

(defvar resize-window-safe-window nil
  "Internal variable bound by `resize-window'.")

(defun resize-window (window delta &optional horizontal ignore)
  "Resize WINDOW vertically by DELTA lines.
WINDOW can be an arbitrary window and defaults to the selected
one.  An attempt to resize the root window of a frame will raise
an error though.

DELTA a positive number means WINDOW shall be enlarged by DELTA
lines.  DELTA negative means WINDOW shall be shrunk by -DELTA
lines.

Optional argument HORIZONTAL non-nil means resize WINDOW
horizontally by DELTA columns.  In this case a positive DELTA
means enlarge WINDOW by DELTA columns.  DELTA negative means
WINDOW shall be shrunk by -DELTA columns.

Optional argument IGNORE non-nil means ignore any restrictions
imposed by fixed size windows, `window-min-height' or
`window-min-width' settings.  IGNORE any window means ignore
restrictions for that window only.  IGNORE equal `safe' means
live windows may get as small as `window-safe-min-height' lines
and `window-safe-min-width' columns.

This function resizes other windows proportionally and never
deletes any windows.  If you want to move only the low (right)
edge of WINDOW consider using `adjust-window-trailing-edge'
instead."
  (setq window (normalize-any-window window))
  (let* ((frame (window-frame window))
	 right)
    (cond
     ((eq window (frame-root-window frame))
      (error "Cannot resize root window of frame"))
     ((window-resizable-p window delta horizontal ignore)
      (resize-window-reset frame horizontal)
      (resize-this-window window delta horizontal ignore t)
      (if (and (not (eq window-splits 'resize))
	       (window-iso-combined-p window horizontal)
	       (setq right (window-right window))
	       (or (window-size-ignore window ignore)
		   (not (window-size-fixed-p right)))
	       (or (< delta 0)
		   (> (- (window-total-size right horizontal)
			 (window-min-size right horizontal))
		      delta)))
	  ;; Resize window below/on the right of WINDOW - this is the
	  ;; classic Emacs behavior, so retain it for `window-splits'
	  ;; not 'resize, iso-combined windows.  It's a PITA, though.
	  (let ((parent-size
		 (window-total-size (window-parent window) horizontal)))
	    (resize-this-window right (- delta) horizontal nil t)
	    (resize-window-normal
	     window (/ (float (window-new-total-size window)) parent-size))
	    (resize-window-normal
	     right (/ (float (window-new-total-size right)) parent-size)))
	(resize-other-windows window delta horizontal ignore))
      (resize-window-apply frame horizontal))
     (t
      (error "Cannot resize window %s" window)))))

(defsubst resize-subwindows-skip-p (window)
  "Return non-nil if WINDOW shall be skipped by resizing routines."
  (memq (window-new-normal-size window) '(ignore stuck skip)))

(defun resize-subwindows-normal (parent horizontal window delta side)
  "Set new normal height of all subwindows of window PARENT.
HORIZONTAL non-nil means set normal width of these windows.
WINDOW has to specify a subwindow of PARENT that has been resized
by DELTA lines \(columns).  SIDE non-nil means set values for
windows on the specified side of WINDOW only."
  (let* ((parent-new-total (window-new-total-size parent))
	 (window-new-total
	  (+ (window-total-size window horizontal) delta))
	 (window-new-normal
	  (/ (float window-new-total) parent-new-total))
	 (others-old-normal
	  (- 1 (window-normal-size window horizontal)))
	 (others-new-normal (- 1 window-new-normal))
	 (sub (window-child parent))
	 (skip (eq side 'right)))

    (when (memq side '(left right))
      (while sub
	(cond
	 ((eq sub window)
	  (setq skip (eq side 'left)))
	 (skip
	  (setq others-old-normal
		(- others-old-normal
		   (window-normal-size sub horizontal)))
	  (setq others-new-normal
		(- others-new-normal
		   (window-normal-size sub horizontal)))))
	(setq sub (window-right sub)))
      (setq sub (window-child parent))
      (setq skip (eq side 'right)))

    (setq sub (window-child parent))
    (while sub
      (cond
       ((eq sub window)
	(resize-window-normal sub window-new-normal)
	(setq skip (eq side 'left)))
       (skip)
       (t
	(resize-window-normal
	 sub (if (zerop others-old-normal)
		 0
	       (/ (* (window-normal-size sub horizontal)
		     others-new-normal)
		  others-old-normal)))))
      (setq sub (window-right sub)))))

;; Calling the following has
;;  1. SIDE non-nil => WINDOW nil.
;;  2. WINDOW non-nil => resize PARENT and WINDOW by DELTA.
;;  3. WINDOW nil => resize PARENT by DELTA.
(defun resize-subwindows (parent delta &optional horizontal ignore side)
  "Resize subwindows of window PARENT vertically by DELTA lines.
PARENT must be a vertically combined internal window.

Optional argument HORIZONTAL non-nil means resize subwindows of
PARENT horizontally by DELTA columns.  In this case PARENT must
be a horizontally combined internal window.

Optional argument IGNORE non-nil means ignore any restrictions
imposed by fixed size windows, `window-min-height' or
`window-min-width' settings.  IGNORE equal `safe' means live
windows may get as small as `window-safe-min-height' lines and
`window-safe-min-width' columns.  IGNORE any window means ignore
restrictions for that window only.

Optional argument SIDE `left' means try to resize only the last
subwindow of PARENT provided DELTA is greater zero.  SIDE `right'
means try to only resize the first subwindow of PARENT provided
DELTA is greater zero.  Any other value of SIDE is ignored."
  (let* ((first (window-child parent))
	 (sub first)
	 (normal-sum 0.0)
	 (total-sum delta)
	 (failed t)
	 (amount 0)
	 found sub-total sub-normal sub-int sub-float sub-delta sub-amount
	 sub-rest best best-rest)
    ;; `normal-sum' is the sum of the normal sizes of all resizable
    ;; subwindows of PARENT.  `total-sum' is the sum of the total
    ;; sizes of all resizable subwindows of PARENT plus DELTA.
    (catch 'done
      (while sub
	(unless (or (resize-subwindows-skip-p sub)
		    (and (not ignore)
			 ;; Ignore fixed-size subwindows.
			 (window-size-fixed-p sub horizontal)
			 (resize-window-normal sub 'ignore)))
	  (setq normal-sum (+ normal-sum
			      (window-normal-size sub horizontal)))
	  (setq total-sum (+ total-sum
			     (window-total-size sub horizontal)))
	  ;; `found' non-nil tells that there is at least one subwindow
	  ;; left that can be resized (should stay `t' now ;-().
	  (setq found t))
	(setq sub (window-right sub)))

      ;; When SIDE is non-nil and DELTA is greater zero try to resize
      ;; the first subwindow (when SIDE is `right') or the last
      ;; subwindow (when SIDE is `left') first.  This is the behavior
      ;; needed by `adjust-window-trailing-edge' when the edge-adjacent
      ;; subwindow the user wants to enlarge is nested in a combination.
      (when (and (> delta 0)
		 ;; Skip a fixed-size window: This is inherently not
		 ;; TRT because a fixed-size internal window might
		 ;; still have a resizable subwindow which we could
		 ;; enlarge.  But DTRT here is quite non-trivial :-(
		 (or (and (eq side 'left)
			  (progn
			    (setq sub first)
			    (while (window-right sub)
			      (setq sub (window-right sub)))
			    sub))
		     (and (eq side 'right) (setq sub first)))
		 (not (resize-subwindows-skip-p sub)))
	;; DELTA > 0 guarantees that resizing SUB always succeeds.
	(resize-this-window sub delta horizontal ignore t side)
	;; Assign new normal sizes.
	(resize-subwindows-normal parent horizontal sub delta side)
	(throw 'done 0))

      ;; We resize subwindows in "rounds".  We assume that usually a
      ;; resize request succeeds in the first round.  If it fails -
      ;; which means at least one subwindow cannot be resized as desired
      ;; - we need another round.  Failures are recorded in the variable
      ;; `failed' and, for the failed subwindow, by setting that
      ;; window's new normal size to a negative value.

      ;; Note that in each round we record (via `resize-window-total')
      ;; only the amount by which the window shall be resized.  Only
      ;; when we know how each inidvidual subwindow shall be resized
      ;; (that is after the final round) we add the current size of the
      ;; window to the amount recorded previously.
      (while (and failed found)
	;; We try to resize each resizable subwindow `sub' by a value
	;; `sub-delta' individually calculated for `sub'.  `sub-amount'
	;; specifies the actual amount `sub' can be resized to in the
	;; present round.  `amount' represents the sum of the
	;; `sub-amount' for all subwindows we are able to resize in the
	;; present round.  `delta' is de-/increased by the sum of
	;; `sub-amount' for all subwindows we we're not able to resize
	;; completely in the present round.  So `amount' and `delta'
	;; grow/shrink towards each other and we are done when the have
	;; the same value.  `sub-rest' is the remainder when calculating
	;; `sub-delta' and is used when calculating the new normal
	;; sizes.
	(setq amount 0)
	(setq found nil)
	(setq failed nil)
	(setq sub first)
	;; The following loop represents one round.
	(while (and sub (not failed))
	  ;; Ignore subwindows that should be ignored or are stuck.
	  (unless (resize-subwindows-skip-p sub)
	    ;; Set `found' to t to make sure that if this round fails we
	    ;; make another round.
	    (setq found t)
	    ;; `sub-total' records the total size of this subwindow.
	    (setq sub-total (window-total-size sub horizontal))
	    ;; `sub-normal' records the normal of this subwindow.
	    (setq sub-normal (window-normal-size sub horizontal))
	    ;; `sub-delta' records the number of lines or columns by
	    ;; which this subwindow should grow or shrink.  `sub-float'
	    ;; and `sub-int' record the new ideal total size as a float
	    ;; and integer value.
	    (setq sub-float (/ (* sub-normal total-sum) normal-sum))
	    (setq sub-int (floor sub-float))
	    (setq sub-delta (- sub-int sub-total))
	    ;; `sub-rest' is the remainder.
	    (setq sub-rest (abs (- sub-float sub-int)))
	    (if (and side (< delta 0) (>= sub-delta 0))
		;; With `adjust-window-trailing-edge' some window can
		;; get penalized such that its normal size exceeds its
		;; fractional total size considerably.  In that case
		;; dragging a divider in the opposite direction in order
		;; to enlarge some other window may cause this window
		;; get _enlarged_ which looks silly.  We try to avoid
		;; such behavior here.
		(resize-window-total sub sub-total)
	      ;; `sub-amount' records the number of lines or columns by
	      ;; which this subwindow can grow or shrink.
	      (setq sub-amount
		    (window-sizable sub sub-delta horizontal ignore))
	      ;; Register the new total size for this subwindow.
	      (resize-window-total sub (+ sub-total sub-amount))
	      (if (= sub-amount sub-delta)
		  ;; We succeeded to get this subwindow's share.
		  (progn
		    (if (and (< delta 0) (zerop sub-amount))
			;; When shrinking avoid that a window that has
			;; not shrunk gets a remainder before a window
			;; that has shrunk.
			(resize-window-normal sub 'rest)
		      ;; Record remainder.
		      (resize-window-normal sub sub-rest))
		    (setq amount (+ amount sub-amount)))
		;; We failed and need a new round.
		(setq failed t)
		;; Don't consider this subwindow again when calculating
		;; desired sizes.
		(setq normal-sum (- normal-sum sub-normal))
		(setq total-sum (- total-sum sub-total sub-amount))
		(setq delta (- delta sub-amount))
		(resize-window-normal sub 'stuck))))
	  (setq sub (window-right sub))))

      ;; Fix rounding by trying to enlarge non-stuck, non-rest windows
      ;; by one line (column) until `amount' equals `delta'.
      (when found
	(catch 'found
	  (while (< amount delta)
	    (setq sub first)
	    (setq best nil)
	    (setq best-rest 0)
	    (while sub
	      (setq sub-normal (window-new-normal-size sub))
	      (when (and (numberp sub-normal) (>= sub-normal best-rest))
		(setq best sub)
		(setq best-rest sub-normal)
		(setq found t))
	      (setq sub (window-right sub)))
	    (if (not best)
		(throw 'found nil)
	      (resize-window-total best 1 'add)
	      (resize-window-normal best (max 0 (1- best-rest)))
	      (setq amount (1+ amount))))))

      ;; Fix rounding by trying to enlarge "rest" windows by one line
      ;; (column) until `amount' equals `delta'.
      (catch 'found
	(while (< amount delta)
	  (setq sub first)
	  (setq best nil)
	  (when (eq (window-new-normal-size sub) 'rest)
	    (setq best t)
	    (resize-window-total sub 1 'add)
	    (setq amount (1+ amount))
	    (setq sub (window-right sub)))
	  (unless best
	    (throw 'found nil))))

      ;; Fix rounding by trying to enlarge stuck windows by one line
      ;; (column) until `amount' equals `delta'.
      (catch 'found
	(while (< amount delta)
	  (setq sub first)
	  (setq best nil)
	  (when (eq (window-new-normal-size sub) 'stuck)
	    (setq best t)
	    (resize-window-total sub 1 'add)
	    (setq amount (1+ amount))
	    (setq sub (window-right sub)))
	  (unless best
	    (throw 'found nil))))

      ;; Reset new normal size fields so `resize-window-apply' won't use
      ;; them to apply new sizes.
      (setq sub first)
      (while sub
	(when (numberp (window-new-normal-size sub))
	  (resize-window-normal sub))
	(setq sub (window-right sub)))

      ;; Now recursively resize each resized subwindow's subwindows.
      (setq sub first)
      (while sub
	(unless (eq (window-new-normal-size sub) 'ignore)
	  ;; Resize this subwindow's subwindows.  Note that above we
	  ;; recorded (via `resize-window-total') only the amount by
	  ;; which this subwindow had to be resized.  Now we add the old
	  ;; total size.
	  (let ((delta (- (window-new-total-size sub)
			  (window-total-size sub horizontal))))
	    (unless (and (zerop delta) (not side))
	      (resize-this-window sub delta horizontal ignore nil side))))
	(setq sub (window-right sub))))))

(defun resize-other-windows (window delta &optional horizontal ignore side)
  "Resize other windows when WINDOW is resized vertically by DELTA lines.
Optional argument HORIZONTAL non-nil means resize other windows
when WINDOW is resized horizontally by DELTA columns.  WINDOW
itself is not resized by this function.

Optional argument IGNORE non-nil means ignore any restrictions
imposed by fixed size windows, `window-min-height' or
`window-min-width' settings.  IGNORE equal `safe' means live
windows may get as small as `window-safe-min-height' lines and
`window-safe-min-width' columns.  IGNORE any window means ignore
restrictions for that window only.

Optional argument SIDE `left' means resize other windows above
\(on left of) WINDOW only.  SIDE `right' means resize other
windows below \(on right of) WINDOW only.  Any other value of
SIDE is ignored."
  (when (window-parent window)
    (let* ((parent (window-parent window))
	   (sub (window-child parent))
	   non-fixed)
      (if (window-iso-combined-p sub horizontal)
	  ;; In an iso-combination resize WINDOW's siblings.
	  (let ((first sub)
		(skip (eq side 'right))
		this-delta)
	    ;; Decide which windows shall be left alone.
	    (while sub
	      (cond
	       ((eq sub window)
		;; Make sure WINDOW is left alone when
		;; resizing its siblings.
		(resize-window-normal sub 'ignore)
		(setq skip (eq side 'left)))
	       (skip
		;; Make sure this sibling is left alone when
		;; resizing its siblings.
		(resize-window-normal sub 'ignore))
	       ((or (window-size-ignore sub ignore)
		    (not (window-size-fixed-p sub horizontal)))
		(setq non-fixed t)))
	      (setq sub (window-right sub)))
	    (if (= (- delta) (window-total-size window horizontal))
		;; A deletion, presumably.
		(if non-fixed
		    ;; There's at least on resizable sibling.
		    (setq this-delta delta)
		  ;; No resizable sibling present.
		  (setq this-delta 0))
	      (setq this-delta
		    (window-resizable
		     window delta horizontal ignore side t)))
	    (unless (= delta this-delta)
	      (resize-window-total parent (- delta this-delta) 'add))
	    (unless (zerop this-delta)
	      (resize-window-normal window 'ignore)
	      (resize-subwindows
	       parent (- this-delta) horizontal ignore side)
	      ;; Now set the normal sizes.
	      (resize-subwindows-normal
	       parent horizontal window this-delta side)
	      (setq delta (- delta this-delta))))

	;; In an ortho-combination all siblings of WINDOW must be
	;; resized by DELTA.  Store the new total size of parent first.
	(resize-window-total parent delta 'add)
	(while sub
	  (unless (eq sub window)
	    (resize-this-window sub delta horizontal ignore t))
	  (setq sub (window-right sub))))

      (unless (zerop delta)
	;; "Go up."
	(resize-other-windows parent delta horizontal ignore side)))))

(defun resize-this-window (window delta &optional horizontal ignore add-total side)
  "Resize WINDOW vertically by DELTA lines.
Optional argument HORIZONTAL non-nil means resize WINDOW
horizontally by DELTA columns.

Optional argument IGNORE non-nil means ignore any restrictions
imposed by fixed size windows, `window-min-height' or
`window-min-width' settings.  IGNORE equal `safe' means live
windows may get as small as `window-safe-min-height' lines and
`window-safe-min-width' columns.  IGNORE any window means ignore
restrictions for that window only.

Optional argument ADD-TOTAL non-nil means add DELTA to the new
total size of WINDOW.

Optional argument SIDE `left' means resize other windows above
\(on left of) WINDOW only.  SIDE `right' means resize other
windows below \(on right of) WINDOW only.  Any other value of
SIDE is ignored.

This function recursively resizes WINDOW's subwindows to fit the
new size.  Make sure that WINDOW is `window-resizable' before
calling this function.  Note that this function does not resize
siblings of WINDOW or WINDOW's parent window.  You have to
eventually call `resize-window-apply' in order to make resizing
actually take effect."
  (when add-total
    ;; Add DELTA to the new total size of WINDOW.
    (resize-window-total window delta t))

  (let ((sub (window-child window)))
    (cond
     ((not sub))
     ((window-iso-combined-p sub horizontal)
      ;; In an iso-combination resize subwindows according to their
      ;; fractions.
      (resize-subwindows window delta horizontal ignore side))
     ;; In an ortho-combination resize each subwindow by DELTA.
     (t
      (while sub
	(resize-this-window sub delta horizontal ignore t side)
	(setq sub (window-right sub)))))))

(defun resize-root-window (window delta horizontal ignore)
  "Resize root window WINDOW vertically by DELTA lines.
HORIZONTAL non-nil means resize root window WINDOW horizontally
by DELTA columns.

IGNORE non-nil means ignore any restrictions imposed by fixed
size windows, `window-min-height' or `window-min-width' settings.

This function is called by Emacs' frame resizing routines.  It
resizes windows proportionally and never deletes any windows."
  (when (and (windowp window) (numberp delta)
	     (window-sizable-p window delta horizontal ignore))
    (resize-window-reset (window-frame window) horizontal)
    (resize-this-window window delta horizontal ignore t)))

(defun resize-root-window-vertically (window delta)
  "Resize root window WINDOW vertically by DELTA lines.
If DELTA is less than zero and we can't shrink WINDOW by DELTA
lines, shrink it as much as possible.  If DELTA is greater than
zero, this function can resize fixed-size subwindows in order to
recover the necessary lines.

Return the number of lines that were recovered.

This function is called by Emacs' minibuffer resizing routines.
It resizes windows proportionally and never deletes any windows."
  (when (numberp delta)
    (let (ignore)
      (cond
       ((< delta 0)
	(setq delta (window-sizable window delta)))
       ((> delta 0)
	(unless (window-sizable window delta)
	  (setq ignore t))))
      (resize-window-reset (window-frame window))
      (resize-this-window window delta nil ignore t)
      delta)))

(defun adjust-window-trailing-edge (window delta &optional horizontal)
  "Move WINDOW's bottom edge by DELTA lines.
Optional argument HORIZONTAL non-nil means move WINDOW's right
edge by DELTA columns.  WINDOW defaults to the selected window.

If the edge can't be moved by DELTA lines, move it as far as
possible in the desired direction."
  (setq window (normalize-any-window window))
  (let ((frame (window-frame window))
	(right window)
	left this-delta min-delta max-delta failed)
    ;; Find the edge we want to move.
    (while (and (or (not (window-iso-combined-p right horizontal))
		    (not (window-right right)))
		(setq right (window-parent right))))
    (unless (and (setq left right) (setq right (window-right right)))
      (error "No window following this one"))

    ;; Set LEFT to the first resizable window on the left.  This step is
    ;; needed to handle fixed-size windows.
    (while (and left (window-size-fixed-p left horizontal))
      (setq left
	    (or (window-left left)
		(progn
		  (while (and (setq left (window-parent left))
			      (not (window-iso-combined-p left horizontal))))
		  (window-left left)))))
    (unless left
      (error "No resizable window preceding this one"))

    ;; Set RIGHT to the first resizable window on the right.  This step
    ;; is needed to handle fixed-size windows.
    (while (and right (window-size-fixed-p right horizontal))
      (setq right
	    (or (window-right right)
		(progn
		  (while (and (setq right (window-parent right))
			      (not (window-iso-combined-p right horizontal))))
		  (window-right right)))))
    (unless right
      (error "No resizable window following this one"))

    ;; LEFT and RIGHT (which might be both internal windows) are now the
    ;; two windows we want to resize.
    (cond
     ((> delta 0)
      (setq max-delta (window-max-delta-1 left 0 horizontal nil 'right))
      (setq min-delta (window-min-delta-1 right (- delta) horizontal nil 'left))
      (when (or (< max-delta delta) (> min-delta (- delta)))
	;; We can't get the whole DELTA - move as far as possible.
	(setq delta (min max-delta (- min-delta))))
      (unless (zerop delta)
	;; Start resizing.
	(resize-window-reset frame horizontal)
	;; Try to enlarge LEFT first.
	(setq this-delta (window-resizable left delta horizontal))
	(unless (zerop this-delta)
	  (resize-this-window left this-delta horizontal nil t 'left))
	(unless (= this-delta delta)
	  ;; We didn't get it all from LEFT, enlarge windows on left of
	  ;; LEFT (for this purpose make `resize-other-windows' believe
	  ;; that we shrink LEFT).
	  (resize-other-windows
	   left (- this-delta delta) horizontal nil 'left))
	;; Shrink windows on right of LEFT.
	(resize-other-windows left delta horizontal nil 'right)))
     ((< delta 0)
      (setq max-delta (window-max-delta-1 right 0 horizontal nil 'left))
      (setq min-delta (window-min-delta-1 left delta horizontal nil 'right))
      (when (or (< max-delta (- delta)) (> min-delta delta))
	;; We can't get the whole DELTA - move as far as possible.
	(setq delta (max (- max-delta) min-delta)))
      (unless (zerop delta)
	;; Start resizing.
	(resize-window-reset frame horizontal)
	;; Try to enlarge RIGHT.
	(setq this-delta (window-resizable right (- delta) horizontal))
	(unless (zerop this-delta)
	  (resize-this-window right this-delta horizontal nil t 'right))
	(unless (= (- this-delta) delta)
	  ;; We didn't get it all from RIGHT, enlarge windows on right of
	  ;; RIGHT (for this purpose make `resize-other-windows' believe
	  ;; that we grow RIGHT).
	  (resize-other-windows
	   right (- this-delta delta) horizontal nil 'right))
	;; Shrink windows on left of RIGHT.
	(resize-other-windows right (- delta) horizontal nil 'left))))
    (unless (zerop delta)
      ;; Don't report an error in the standard case.
      (unless (resize-window-apply frame horizontal)
	;; But do report an error it applying the changes fails.
	(error "Failed adjusting window %s" window)))))

(defun resize-mini-window (window delta)
  "Resize minibuffer window WINDOW by DELTA lines."
  (when (window-minibuffer-p window)
    (let* ((frame (window-frame window))
	   (root (frame-root-window frame))
	   (height (window-total-size window)))
      (unless (> (- height delta) 0)
	(setq delta (- height 1)))
      (when (window-sizable-p root delta)
	(resize-window-reset frame)
	(resize-this-window root delta nil nil t)
	(resize-window-total window (- height delta))
	(resize-mini-window-internal window)))))

(defun enlarge-window (delta &optional horizontal)
  "Make selected window DELTA lines taller.
Interactively, if no argument is given, make the selected window
one line taller.  If optional argument HORIZONTAL is non-nil,
make selected window wider by DELTA columns.  If DELTA is
negative, shrink selected window by -DELTA lines or columns.
Return nil."
  (interactive "p")
  (resize-window (selected-window) delta horizontal))

(defun shrink-window (delta &optional horizontal)
  "Make selected window DELTA lines smaller.
Interactively, if no argument is given, make the selected window
one line smaller.  If optional argument HORIZONTAL is non-nil,
make selected window narrower by DELTA columns.  If DELTA is
negative, enlarge selected window by -DELTA lines or columns.
Return nil."
  (interactive "p")
  (resize-window (selected-window) (- delta) horizontal))

(defun maximize-window (&optional window)
  "Maximize WINDOW.
Make WINDOW as large as possible without deleting any windows.
WINDOW can be any window and defaults to the selected window."
  (interactive)
  (setq window (normalize-any-window window))
  (resize-window window (window-max-delta window))
  (resize-window window (window-max-delta window t) t))

(defun minimize-window (&optional window)
  "Minimize WINDOW.
Make WINDOW as small as possible without deleting any windows.
WINDOW can be any window and defaults to the selected window."
  (interactive)
  (setq window (normalize-any-window window))
  (resize-window window (- (window-min-delta window)))
  (resize-window window (- (window-min-delta window t)) t))

(defsubst frame-root-window-p (window)
  "Return non-nil if WINDOW is the root window of its frame."
  (eq window (frame-root-window window)))

(defun window-tree-1 (window &optional next)
  "Return window tree rooted at WINDOW.
Optional argument NEXT non-nil means include windows right
siblings in the return value.

See the documentation of `window-tree' for a description of the
return value."
  (let (list)
    (while window
      (setq list
	    (cons
	     (cond
	      ((window-vchild window)
	       (cons t (cons (window-edges window)
			     (window-tree-1 (window-vchild window) t))))
	      ((window-hchild window)
	       (cons nil (cons (window-edges window)
			       (window-tree-1 (window-hchild window) t))))
	      (t window))
	     list))
      (setq window (when next (window-next window))))
    (nreverse list)))

(defun window-tree (&optional frame)
  "Return the window tree of frame FRAME.
FRAME must be a live frame and defaults to the selected frame.
The return value is a list of the form (ROOT MINI), where ROOT
represents the window tree of the frame's root window, and MINI
is the frame's minibuffer window.

If the root window is not split, ROOT is the root window itself.
Otherwise, ROOT is a list (DIR EDGES W1 W2 ...) where DIR is nil
for a horizontal split, and t for a vertical split.  EDGES gives
the combined size and position of the subwindows in the split,
and the rest of the elements are the subwindows in the split.
Each of the subwindows may again be a window or a list
representing a window split, and so on.  EDGES is a list \(LEFT
TOP RIGHT BOTTOM) as returned by `window-edges'."
  (setq frame (normalize-live-frame frame))
  (window-tree-1 (frame-root-window frame) t))

;;; Composite Windows

;; The basic invariant of the composite window code is:

;; \A window \in Windows:
;;    \A sibling \in Siblings [window]:
;;      composite-window-p [window] =>
;;        /\ composite-window-p [sibling]
;;        /\ composite-root-window [window] = composite-root-window [sibling]

;; that is, for any window that is part of a composite window, any
;; sibling of that window is a subwindow of the same composite window.

;; This is usually not called as a "predicate" but it's more consistent
;; to maintain our defsubsts as predicate.
(defsubst composite-window-p (window)
  "Return non-nil if WINDOW is a subwindow of a composite window.
The return value is the value of the `composite' window parameter
of WINDOW."
  (window-parameter window 'composite))

(defsubst composite-root-window-p (window)
  "Return non-nil if WINDOW is the root of a composite window.
The return value is the type of that composite window, either
`compound' or `group'."
  (or (window-parameter window 'compound)
      (window-parameter window 'group)))

(defsubst composite-main-window-p (window)
  "Return t if WINDOW is a main window of a composite window."
  (eq (cdr-safe (composite-window-p window)) 'main))

(defsubst composite-support-window-p (window)
  "Return t if WINDOW is a support window of a composite window."
  (eq (cdr-safe (composite-window-p window)) 'support))

(defun composite-root-window (window)
  "Return root window of the composite window WINDOW is a part of.
Return nil if WINDOW is not part of a composite window or the
path from WINDOW to the root of the composite window is broken."
  (let ((type (car-safe (window-parameter window 'composite))))
    (when type
      (setq window (window-parent window))
      (catch 'done
	(while window
	  (cond
	   ((window-parameter window type)
	    (throw 'done window))
	   ((eq (car-safe (window-parameter window 'composite)) type))
	   (t
	    ;; Broken path.
	    (throw 'done nil)))
	  (setq window (window-parent window)))))))

(defun composite-major-window (window)
  "Return major window of composite window WINDOW belongs to.
The major window is the last main or root window found by
following the path from WINDOW to the root of the composite
window WINDOW belongs to.  Each composite window should have one
and only one major window to make sure that functions on its
component windows behave \"as intended\".

This function returns a meaningful result if and only if WINDOW
is a main window."
  (let ((main window))
    (setq window (window-parent window))
    (while (and window (composite-main-window-p window)
		(not (composite-root-window-p window)))
      (setq main window)
      (setq window (window-parent window)))
    ;; We can't go up any further but maybe the window we're looking at
    ;; is the root window.
    (when (composite-root-window-p window)
      (let ((sibling (window-child window)))
	;; Make sure that all children of the group root window are main
	;; windows.
	(catch 'done
	  (while sibling
	    (if (not (composite-main-window-p sibling))
		(throw 'done nil)
	      (setq sibling (window-right sibling))))
	  (setq main window))))
    main))

(defun composite-main-sibling (window)
  "Return first \"main\" sibling of WINDOW.
A main sibling is a main window of a composite window.  Both,
WINDOW and the main sibling must have the same parent window and
thus be part of one and the same composite window.  Return nil if
no such window can be found."
  (let ((parent (window-parent window))
	sibling)
    (when parent
      (setq sibling (window-child parent))
      (catch 'done
	(while sibling
	  (if (and (not (eq sibling window))
		   (eq (cdr-safe (window-parameter sibling 'composite)) 'main))
	      (throw 'done sibling)
	    (setq sibling (window-right sibling))))))))

(defun composite-lowest-child-role (window)
  "Return lowest \"non-main\" role among WINDOW's children."
  (let ((sibling (window-child window))
	(highest 'main)
	role)
    (catch 'done
      (while sibling
	(setq role (cdr-safe (window-parameter sibling 'composite)))
	(cond
	 ((eq role t)
	  (setq highest t))
	 ((eq role 'support)
	  (throw 'done 'support)))
      (setq sibling (window-right sibling)))
      highest)))

(defsubst compound-window-p (window)
  "Return non-nil if WINDOW is a subwindow of a compound window."
  (eq (car-safe (window-parameter window 'composite)) 'compound))

(defsubst compound-main-window-p (window)
  "Return non-nil if WINDOW is a main window of a compound window."
  (let ((composite (composite-window-p window)))
    (and (eq (car-safe composite) 'compound)
	 (eq (cdr-safe composite) 'main))))

(defun compound-root-window (window)
  "Return topmost root window of compound window WINDOW belongs to."
  (while (and window (compound-window-p window))
    (setq window (window-parent window)))
  (when (window-parameter window 'compound)
    window))

(defsubst group-window-p (window)
  "Return non-nil if WINDOW is a subwindow of a window group."
  (eq (car-safe (window-parameter window 'composite)) 'group))

(defsubst group-window-main-p (window)
  "Return non-nil if WINDOW is a main window of a window group."
  (let ((composite (composite-window-p window)))
    (and (eq (car-safe composite) 'group)
	 (eq (cdr-safe composite) 'main))))

(defun group-root-window (window)
  "Return root window of window group WINDOW belongs to.
If WINDOW is part of a compound window, return the root window of
the group the root of the compound window belongs too."
  (while (and window (compound-window-p window))
    (setq window (window-parent window)))
  (while (and window (group-window-p window)
	      (not (composite-root-window-p window)))
    (setq window (window-parent window)))
  (when (window-parameter window 'group)
    window))

;;; Getting the "other" window.
;; FIXME: Handle `ignore-window-parameters' and some other things maybe.
(defun other-window (count &optional all-frames)
  "Select another window in cyclic ordering of windows.
COUNT specifies the number of windows to skip, starting with the
selected window, before making the selection.  If COUNT is
positive, skip COUNT windows forwards.  If COUNT is negative,
skip -COUNT windows backwards.  COUNT zero means do not skip any
window, so select the selected window.  In an interactive call,
COUNT is the numeric prefix argument.  Return nil.

This function does not select a window whose `no-other-window'
parameter is non-nil.  Also, this function never selects the
support window of a composite window unless the support window's
`maybe-other-window' parameter is non-nil.

This function uses `next-window' for finding the window to
select.  The argument ALL-FRAMES has the same meaning as in
`next-window', but the MINIBUF argument of `next-window' is
always effectively nil."
  (interactive "p")
  (let* ((window (selected-window))
	 (function (window-parameter window 'other-window))
	 old-window old-count)
    (if (functionp function)
	(funcall function count all-frames)
      ;; `next-window' and `previous-window' may return a window we are
      ;; not allowed to select.  Hence we need an exit strategy in case
      ;; all windows are non-selectable.
      (catch 'exit
	(while (> count 0)
	  (setq window (next-window window nil all-frames))
	  (cond
	   ((eq window old-window)
	    (when (= count old-count)
	      ;; Keep out of infinite loops.  When COUNT has not changed
	      ;; since we last looked at `window' we're probably in one.
	      (throw 'exit nil)))
	   ((or (and (composite-support-window-p window)
		     (not (window-parameter window 'maybe-other-window)))
		(window-parameter window 'no-other-window))
	    ;; The first non-selectable window `next-window' got us:
	    ;; Remember it and the current value of COUNT.
	    (unless old-window
	      (setq old-window window)
	      (setq old-count count)))
	   (t
	    (setq count (1- count)))))
	(while (< count 0)
	  (setq window (previous-window window nil all-frames))
	  (cond
	   ((eq window old-window)
	    (when (= count old-count)
	      ;; Keep out of infinite loops.  When COUNT has not changed
	      ;; since we last looked at `window' we're probably in one.
	      (throw 'exit nil)))
	   ((or (and (composite-support-window-p window)
		     (not (window-parameter window 'maybe-other-window)))
		(window-parameter window 'no-other-window))
	    ;; The first non-selectable window `previous-window' got us:
	    ;; Remember it and the current value of COUNT.
	    (unless old-window
	      (setq old-window window)
	      (setq old-count count)))
	   (t
	    (setq count (1+ count)))))
	(select-window window)
	nil))))

;; This should probably return non-nil when the selected window is part
;; of a compound window whose root is the frame's root window.
(defun one-window-p (&optional nomini all-frames)
  "Return non-nil if the selected window is the only window.
Optional arg NOMINI non-nil means don't count the minibuffer
even if it is active.  Otherwise, the minibuffer is counted
when it is active.

Optional argument ALL-FRAMES specifies the set of frames to
consider, see also `next-window'.  ALL-FRAMES nil or omitted
means consider windows on WINDOW's frame only, plus the
minibuffer window if specified by the NOMINI argument.  If the
minibuffer counts, consider all windows on all frames that share
that minibuffer too.  The remaining non-nil values of ALL-FRAMES
with a special meaning are:

- t means consider all windows on all existing frames.

- `visible' means consider all windows on all visible frames.

- 0 (the number zero) means consider all windows on all visible
  and iconified frames.

- A frame means consider all windows on that frame only.

Anything else means consider all windows on WINDOW's frame and no
others."
  (let ((base-window (selected-window)))
    (if (and nomini (eq base-window (minibuffer-window)))
	(setq base-window (next-window base-window)))
    (eq base-window
	(next-window base-window (if nomini 'arg) all-frames))))

;;; Deleting windows.
(defun window-deletable-p (&optional window)
  "Return t if WINDOW can be safely deleted from its frame.
Return `frame' if deleting WINDOW should delete its frame
instead."
  (setq window (normalize-any-window window))
  (let ((frame (window-frame window))
	(dedicated (and (window-buffer window) (window-dedicated-p window)))
	(quit-restore (car-safe (window-parameter window 'quit-restore)))
	composite type role root)
    (cond
     ((frame-root-window-p window)
      (when (and (or dedicated
		     (and (eq quit-restore t)
			  (with-current-buffer (window-buffer window)
			    ;; `view-remove-frame-by-deleting' and
			    ;; `view-mode' are autoloaded.
			    (or (not view-mode)
				view-remove-frame-by-deleting))))
		 (other-visible-frames-p frame))
	;; WINDOW is the root window of its frame.  Return `frame' but
	;; only if WINDOW is (1) either dedicated or quit-restore's car
	;; is t and (2) there are other frames left.
	'frame))
     ((setq composite (window-parameter window 'composite))
      (setq type (car-safe composite))
      (setq role (cdr-safe composite))
      (setq root (composite-root-window window))
      (cond
       ;; When `ignore-window-parameters' or the `delete-window'
       ;; parameter say or WINDOW is part of a broken composite window,
       ;; WINDOW is deletable.  We cannot handle the case where WINDOW's
       ;; `delete-window' parameter is a function (that's impossible).
       ((or (not (memq ignore-window-parameters '(nil post)))
	    (eq (window-parameter window 'delete-window) t)
	    (not root) (not type) (not role))
	t)
       ((eq type 'compound)
	;; A component of a compound window is deletable if and only if
	;; its root is deletable.
	(window-deletable-p root))
       ((eq type 'group)
	;; In a window group only a main window with a main sibling is
	;; deletable.
	(and (eq role 'main) (composite-main-sibling window)))))
     (t))))

(defun window-or-subwindow-p (subwindow window)
  "Return t if SUBWINDOW is either WINDOW or a subwindow of WINDOW."
  (or (eq subwindow window)
      (let ((parent (window-parent subwindow)))
	(catch 'done
	  (while parent
	    (if (eq parent window)
		(throw 'done t)
	      (setq parent (window-parent parent))))))))

(defun delete-window (&optional window)
  "Delete WINDOW.
WINDOW can be an arbitrary window and defaults to the selected
one.  Return nil.

This function respects the variable `ignore-window-parameters'
when processing window parameters so any processing of WINDOW's
parameters may be suppressed.

If the `delete-window' parameter WINDOW equals t, delete WINDOW
ignoring any other window parameters.  If the `delete-window'
parameter specifies a function, call that function with WINDOW as
its sole argument.  It's the responsibility of that function to
adjust the parameters of all remaining windows.

Otherwise, if WINDOW is part of a compound window, call this
function with the root of the compound window as its argument.
If WINDOW is either the only window on its frame, or a support
window or the last main window of a window group, signal an error
and don't delete WINDOW.

This function makes sure that window parameters are reset or
inherited when WINDOW is part of a combination of two windows."
  (interactive)
  (setq window (normalize-any-window window))
  (let* ((function (window-parameter window 'delete-window))
	 ;; COMPOSITE non-nil means WINDOW is part of a composite
	 ;; window.
	 (composite (window-parameter window 'composite))
	 ;; TYPE is the type of the composite window (either `compound'
	 ;; or `group').  ROLE is the role of WINDOW within the
	 ;; composite window (either `main', `support', or t).  ROOT is
	 ;; the root window of the composite window.
	 (type (car-safe composite))
	 (role (cdr-safe composite))
	 (root (and composite (composite-root-window window)))
	 parent)
    (catch 'done
      ;; Handle window parameters.
      (cond
       ;; Ignore window parameters if `ignore-window-parameters' tells
       ;; so or the `delete-window' parameter equals t.
       ((or (not (memq ignore-window-parameters '(nil post)))
	    (eq function t)))
       ((functionp function)
	;; The `delete-window' parameter specifies the function to call
	;; instead.  If that function is `ignore' nothing is done.  It's
	;; up to the function called here to avoid infinite recursion.
	(throw 'done (funcall function window)))
       (composite
	(cond
	 ((or (not root) (not type) (not role))
	  ;; Something is broken in this composite window.  Signal a
	  ;; message but let the deletion pass through (we might signal
	  ;; an error here but for everday work this is too nasty).
	  (message "Broken component %s of composite window" window))
	 ((eq type 'compound)
	  ;; Deleting a component of a compound window deletes the
	  ;; entire compound window.
	  (throw 'done (delete-window root)))
	 ((eq type 'group)
	  (cond
	   ((not (eq role 'main))
	    ;; WINDOW is _not_ a main window of a window group.  There's
	    ;; no rule for deleting such a window so we signal an error.
	    ;; We might swallow this error silently.
	    (error "Cannot delete non-main window of a window group"))
	   ((not (composite-main-sibling window))
	    ;; WINDOW has no main sibling and we can't delete the last
	    ;; main window of a window group.  We might swallow this
	    ;; error silently.
	    (error "Cannot delete last main window of a window group")))))))

      ;; Set PARENT to WINDOW's parent in the window tree.  If there's
      ;; no such parent signal an error.
      (unless (setq parent (window-parent window))
	(error "Attempt to delete minibuffer or sole ordinary window"))

      (let* ((horizontal (window-hchild parent))
	     (size (window-total-size window horizontal))
	     (frame (window-frame window))
	     (frame-selected
	      (window-or-subwindow-p (frame-selected-window frame) window))
	     ;; LEFT is WINDOW's _left_ sibling - traditionally LEFT
	     ;; gets enlarged and is selected after the deletion.
	     (left (window-left window))
	     ;; RIGHT is WINDOW's right sibling.
	     (right (window-right window))
	     ;; SIBLING is WINDOW's sibling provided they are the only
	     ;; child windows of PARENT.
	     (sibling
	      (or (and left (not right) (not (window-left left)) left)
		  (and right (not left) (not (window-right right)) right)))
	     ;; Record some of PARENT's parameters (just in case we have
	     ;; WINDOW replace it in the window tree).
	     (parent-compound
	      (and sibling (window-parameter parent 'compound)))
	     (parent-group
	      (and sibling (window-parameter parent 'group)))
	     (parent-composite
	      (and sibling (window-parameter parent 'composite))))
	(resize-window-reset frame horizontal)
	(cond
	 ((or (and (eq window-splits 'nest)
		   (or (and left (not (window-left left))
			    (not (window-right window)))
		       (and (not left)
			    (setq left (window-right window))
			    (not (window-right left))))
		   (not (window-size-fixed-p left horizontal)))
	      (and left (not window-splits)
		   (not (window-size-fixed-p left horizontal))))
	  ;; Resize WINDOW's left sibling.
	  (resize-this-window left size horizontal nil t)
	  (resize-window-normal
	   left (+ (window-normal-size left horizontal)
		   (window-normal-size window horizontal))))
	 ((let ((sub (window-child parent)))
	    (catch 'found
	      ;; Look for a non-fixed-size sibling.
	      (while sub
		(when (and (not (eq sub window))
			   (not (window-size-fixed-p sub horizontal)))
		  (throw 'found t))
		(setq sub (window-right sub)))))
	  ;; We can do it without resizing fixed-size windows.
	  (resize-other-windows window (- size) horizontal))
	 (t
	  ;; Can't do without resizing fixed-size windows.  We really
	  ;; should signal an error here but who would agree :-(
	  (resize-other-windows window (- size) horizontal t)))
	;; Actually delete WINDOW.
	(delete-window-internal window)
	(when (and frame-selected
		   (window-parameter
		    (frame-selected-window frame) 'no-other-window))
	  ;; `delete-window-internal' has selected a window that should
	  ;; not be selected, fix this here (I hate `other-window').
	  (other-window -1 frame))
	;; Handle composite windows (unless we ignore window
	;; parameters).
	(when (and (memq ignore-window-parameters '(nil pre))
		   sibling (not (eq parent (window-parent sibling)))
		   (or parent-compound parent-group))
	  ;; At this moment we know that WINDOW and SIBLING are part of
	  ;; a composite window and the _sole_ child windows of PARENT.
	  ;; SIBLING replaces PARENT.
	  (when parent-group
	    ;; SIBLING becomes the new root of the window group earlier
	    ;; headed by PARENT.  If PARENT was the root of a compound
	    ;; window that compound window gets dissolved.
	    (set-window-parameter sibling 'group t))
	  (if parent-composite
	      ;; `sibling' inherits composite state of `parent'.
	      (set-window-parameter sibling 'composite parent-composite)
	    ;; `sibling' is no longer part of a composite window.
	    (set-window-parameter sibling 'composite nil)))
	(run-window-configuration-change-hook frame)
	nil))))

(defun delete-other-windows (&optional window)
  "Make WINDOW fill its frame.
WINDOW may be any window and defaults to the selected one.

This function respects the variable `ignore-window-parameters'
when processing window parameters so any processing of WINDOW's
parameters may be suppressed.

If the `delete-other-windows' parameter of WINDOW equals t,
delete WINDOW ignoring any other window parameters.  If the
`delete-other-windows' parameter specifies a function, call that
function with WINDOW as its sole argument.  It's the
responsibility of that function to adjust the parameters of all
remaining windows.

Otherwise, if WINDOW is part of a compound window, call this
function with the root of the compound window as its argument.
If WINDOW is a main window in a window group, make WINDOW the
only main window in this group.  Any support windows of the group
are left alone.  If WINDOW is a support window of a window group,
signal an error and don't delete any windows."
  (interactive)
  (setq window (normalize-any-window window))
  (let* ((function (window-parameter window 'delete-other-windows))
	 (composite (window-parameter window 'composite))
	 ;; COMPOSITE non-nil means WINDOW is part of a composite
	 ;; window.
	 (type (car-safe composite))
	 (role (cdr-safe composite))
	 (root (and composite (composite-root-window window)))
	 ;; TYPE is the type of the composite window (either `compound'
	 ;; or `group').  ROLE is the role of WINDOW within the
	 ;; composite window (either `main', `support', or t).  ROOT is
	 ;; the root window of the composite window.
	 main)
    (catch 'done
      ;; Handle composite window parameter.
      (cond
       ;; Ignore window parameters if `ignore-window-parameters' tells
       ;; so or the `delete-other-windows' parameter equals t.
       ((or (not (memq ignore-window-parameters '(nil post)))
	    (eq function t)))
       ((functionp function)
	;; The `delete-other-windows' parameter specifies the function
	;; to call instead.  If the function is `ignore' no windows are
	;; deleted.  It's up to the function called to avoid infinite
	;; recursion.
	(throw 'done (funcall function window)))
       (composite
	(cond
	 ((or (not root) (not type) (not role))
	  ;; Something is broken in this composite window.  Signal a
	  ;; message but let the deletion pass through (we might signal
	  ;; an error here but for everday work this is too nasty).
	  (message "Broken composite window"))
	 ((eq type 'compound)
	  ;; In a compound window call `delete-other-windows' with the
	  ;; root window as its argument.
	  (throw 'done (delete-other-windows root)))
	 ((eq type 'group)
	  (if (eq role 'main)
	      ;; In a window group we are allowed to delete main windows
	      ;; only.  Moreover we need an ancestor which is the last
	      ;; main window found when following the path to the group
	      ;; root window.
	      (progn
		(setq main (composite-major-window window))
		(when (or (not main) (eq main window))
		  ;; If we don't find an ancestor or the ancestor is
		  ;; WINDOW itself there's nothing we can delete.
		  ;; Swallow this quietly.
		  (throw 'done nil))
		(when (and (eq main root)
			   (memq ignore-window-parameters '(nil pre)))
		  ;; If we delete right up to the root of this group
		  ;; (that is, there are no support windows around) give
		  ;; WINDOW the parameters of `root'.
		  (set-window-parameter window 'group t)
		  (set-window-parameter
		   window 'composite (window-parameter root 'composite))))
	    ;; We might swallow this message.
	    (error
	     "Cannot delete other windows for non-main window %s" window))))))

      (delete-other-windows-internal window main)
      (when (and (memq ignore-window-parameters '(nil pre))
		 (frame-root-window-p window))
	;; Clean up for the case where we did something special.
	(set-window-parameter window 'composite nil))
      nil)))

(defun delete-other-windows-vertically (&optional window)
  "Delete the windows in the same column with WINDOW, but not WINDOW itself.
This may be a useful alternative binding for \\[delete-other-windows]
 if you often split windows horizontally."
  (interactive)
  (let* ((window (or window (selected-window)))
         (edges (window-edges window))
         (w window) delenda)
    (while (not (eq (setq w (next-window w 1)) window))
      (let ((e (window-edges w)))
        (when (and (= (car e) (car edges))
                   (= (caddr e) (caddr edges)))
          (push w delenda))))
    (mapc 'delete-window delenda)))

;;; Windows and buffers.

;; `prev-buffers' and `next-buffers' are two reserved window slots used
;; for (1) determining which buffer to show in the window when its
;; buffer shall be buried or killed and (2) which buffer to show for
;; `switch-to-prev-buffer' and `switch-to-next-buffer'.

;; `prev-buffers' consists of <buffer, window-start, window-point>
;; triples.  The entries on this list are ordered by the time their
;; buffer has been removed from the window, the most recently removed
;; buffer's entry being first.  The window-start and window-point
;; components are `window-start' and `window-point' at the time the
;; buffer was removed from the window which implies that the entry must
;; be added when `set-window-buffer' removes the buffer from the window.

;; `next-buffers' is the list of buffers that have been replaced
;; recently by `switch-to-prev-buffer'.  These buffers are the least
;; preferred candidates of `switch-to-prev-buffer' and the preferred
;; candidates of `switch-to-next-buffer' to switch to.  This list is
;; reset to nil by any action changing the window's buffer with the
;; exception of `switch-to-prev-buffer' and `switch-to-next-buffer'.
;; `switch-to-prev-buffer' pushes the buffer it just replaced on it,
;; `switch-to-next-buffer' pops the last pushed buffer from it.

;; Both `prev-buffers' and `next-buffers' may reference killed buffers
;; if such a buffer was killed while the window was hidden within a
;; window configuration.  Such killed buffers get removed whenever
;; `switch-to-prev-buffer' or `switch-to-next-buffer' encounter them.

;; The following function is called by `set-window-buffer' _before_ it
;; replaces the buffer of the argument window with the new buffer.
(defun record-window-buffer (&optional window)
  "Record WINDOW's buffer.
WINDOW must be a live window and defaults to the selected one."
  (let* ((window (normalize-live-window window))
	 (buffer (window-buffer window))
	 (entry (assq buffer (window-prev-buffers window))))
    ;; Reset WINDOW's next buffers.  If needed, they are resurrected by
    ;; `switch-to-prev-buffer' and `switch-to-next-buffer'.
    (set-window-next-buffers window nil)

    (when entry
      ;; Remove all entries for BUFFER from WINDOW's previous buffers.
      (set-window-prev-buffers
       window (assq-delete-all buffer (window-prev-buffers window))))

    ;; Don't record insignificant buffers.
    (unless (eq (aref (buffer-name buffer) 0) ?\s)
      ;; Add an entry for buffer to WINDOW's previous buffers.
      (with-current-buffer buffer
	(let ((start (window-start window))
	      (point (window-point window)))
	  (setq entry
		(cons buffer
		      (if entry
			  ;; We have an entry, update marker positions.
			  (list (set-marker (nth 1 entry) start)
				(set-marker (nth 2 entry) point))
			;; Make new markers.
			(list (copy-marker start)
			      (copy-marker point)))))

	  (set-window-prev-buffers
	   window (cons entry (window-prev-buffers window))))))))

(defun unrecord-window-buffer (&optional window buffer)
  "Unrecord BUFFER in WINDOW.
WINDOW must be a live window and defaults to the selected one.
BUFFER must be a live buffer and defaults to the buffer of
WINDOW."
  (let* ((window (normalize-live-window window))
	 (buffer (or buffer (window-buffer window))))
    (set-window-prev-buffers
     window (assq-delete-all buffer (window-prev-buffers window)))
    (set-window-next-buffers
     window (delq buffer (window-next-buffers window)))))

(defun set-window-buffer-start-and-point (window buffer &optional start point)
  "Set WINDOW's buffer to BUFFER.
Optional argument START non-nil means set WINDOW's start position
to START.  Optional argument POINT non-nil means set WINDOW's
point to POINT.  If WINDOW is selected this also sets BUFFER's
`point' to POINT.  If WINDOW is selected and the buffer it showed
before was current this also makes BUFFER the current buffer."
  (let ((selected (eq window (selected-window)))
	(current (eq (window-buffer window) (current-buffer))))
    (set-window-buffer window buffer)
    (when (and selected current)
      (set-buffer buffer))
    (when start
      (set-window-start window start))
    (when point
      (if selected
	  (with-current-buffer buffer
	    (goto-char point))
	(set-window-point window point)))))

(defun switch-to-prev-buffer (&optional window bury-or-kill)
  "In WINDOW switch to previous buffer.
WINDOW must be a live window and defaults to the selected one.

Optional argument BURY-OR-KILL non-nil means the buffer currently
shown in WINDOW is about to be buried or killed and consequently
shall not be switched to in future invocations of this command."
  (interactive)
  (let* ((window (normalize-live-window window))
	 (old-buffer (window-buffer window))
	 ;; Save this since it's destroyed by `set-window-buffer'.
	 (next-buffers (window-next-buffers window))
	 entry new-buffer killed-buffers deletable)
    (cond
     ;; When BURY-OR-KILL is non-nil, there's no previous buffer for
     ;; this window, and we can delete the window (or the frame) do
     ;; that.
     ((and bury-or-kill
	   (or (not (window-prev-buffers window))
	       (and (eq (caar (window-prev-buffers window)) old-buffer)
		    (not (cdr (car (window-prev-buffers window))))))
	   (setq deletable (window-deletable-p window)))
      (if (eq deletable 'frame)
	  (delete-frame (window-frame window))
	(delete-window window)))
     ((window-dedicated-p window)
      (error "Window %s is dedicated to buffer %s" window old-buffer)))

    (unless deletable
      (catch 'found
	;; Scan WINDOW's previous buffers first, skipping entries of next
	;; buffers.
	(dolist (entry (window-prev-buffers window))
	  (when (and (setq new-buffer (car entry))
		     (or (buffer-live-p new-buffer)
			 (not (setq killed-buffers
				    (cons new-buffer killed-buffers))))
		     (not (eq new-buffer old-buffer))
		     (or bury-or-kill
			 (not (memq new-buffer next-buffers))))
	    (set-window-buffer-start-and-point
	     window new-buffer (nth 1 entry) (nth 2 entry))
	    (throw 'found t)))
	;; Scan reverted buffer list of WINDOW's frame next, skipping
	;; entries of next buffers.  Note that when we bury or kill a
	;; buffer we don't reverse the global buffer list to avoid showing
	;; a buried buffer instead.  Otherwise, we must reverse the global
	;; buffer list in order to make sure that switching to the
	;; previous/next buffer traverse it in opposite directions.
	(dolist (buffer (if bury-or-kill
			    (buffer-list (window-frame window))
			  (nreverse (buffer-list (window-frame window)))))
	  (when (and (buffer-live-p buffer)
		     (not (eq buffer old-buffer))
		     (not (eq (aref (buffer-name buffer) 0) ?\s))
		     (or bury-or-kill (not (memq buffer next-buffers))))
	    (setq new-buffer buffer)
	    (set-window-buffer-start-and-point window new-buffer)
	    (throw 'found t)))
	(unless bury-or-kill
	  ;; Scan reverted next buffers last (must not use nreverse
	  ;; here!).
	  (dolist (buffer (reverse next-buffers))
	    ;; Actually, buffer _must_ be live here since otherwise it
	    ;; would have been caught in the scan of previous buffers.
	    (when (and (or (buffer-live-p buffer)
			   (not (setq killed-buffers
				      (cons buffer killed-buffers))))
		       (not (eq buffer old-buffer))
		       (setq entry (assq buffer (window-prev-buffers window))))
	      (setq new-buffer buffer)
	      (set-window-buffer-start-and-point
	       window new-buffer (nth 1 entry) (nth 2 entry))
	      (throw 'found t)))))

      (if bury-or-kill
	  ;; Remove `old-buffer' from WINDOW's previous and (restored list
	  ;; of) next buffers.
	  (progn
	    (set-window-prev-buffers
	     window (assq-delete-all old-buffer (window-prev-buffers window)))
	    (set-window-next-buffers window (delq old-buffer next-buffers)))
	;; Move `old-buffer' to head of WINDOW's restored list of next
	;; buffers.
	(set-window-next-buffers
	 window (cons old-buffer (delq old-buffer next-buffers)))))

    ;; Remove killed buffers from WINDOW's previous and next buffers.
    (when killed-buffers
      (dolist (buffer killed-buffers)
	(set-window-prev-buffers
	 window (assq-delete-all buffer (window-prev-buffers window)))
	(set-window-next-buffers
	 window (delq buffer (window-next-buffers window)))))

    ;; Return new-buffer.
    new-buffer))

(defun switch-to-next-buffer (&optional window)
  "In WINDOW switch to next buffer.
WINDOW must be a live window and defaults to the selected one."
  (interactive)
  (let* ((window (normalize-live-window window))
	 (old-buffer (window-buffer window))
	 (next-buffers (window-next-buffers window))
	 new-buffer entry killed-buffers)
    (when (window-dedicated-p window)
      (error "Window %s is dedicated to buffer %s" window old-buffer))

    (catch 'found
      ;; Scan WINDOW's next buffers first.
      (dolist (buffer next-buffers)
	(when (and (or (buffer-live-p buffer)
		       (not (setq killed-buffers
				  (cons buffer killed-buffers))))
		   (not (eq buffer old-buffer))
		   (setq entry (assq buffer (window-prev-buffers window))))
	  (setq new-buffer buffer)
	  (set-window-buffer-start-and-point
	   window new-buffer (nth 1 entry) (nth 2 entry))
	  (throw 'found t)))
      ;; Scan the buffer list of WINDOW's frame next, skipping previous
      ;; buffers entries.
      (dolist (buffer (buffer-list (window-frame window)))
	(when (and (buffer-live-p buffer) (not (eq buffer old-buffer))
		   (not (eq (aref (buffer-name buffer) 0) ?\s))
		   (not (assq buffer (window-prev-buffers window))))
	  (setq new-buffer buffer)
	  (set-window-buffer-start-and-point window new-buffer)
	  (throw 'found t)))
      ;; Scan WINDOW's reverted previous buffers last (must not use
      ;; nreverse here!)
      (dolist (entry (reverse (window-prev-buffers window)))
	(when (and (setq new-buffer (car entry))
		   (or (buffer-live-p new-buffer)
		       (not (setq killed-buffers
				  (cons new-buffer killed-buffers))))
		   (not (eq new-buffer old-buffer)))
	  (set-window-buffer-start-and-point
	   window new-buffer (nth 1 entry) (nth 2 entry))
	  (throw 'found t))))

    ;; Remove `new-buffer' from and restore WINDOW's next buffers.
    (set-window-next-buffers window (delq new-buffer next-buffers))

    ;; Remove killed buffers from WINDOW's previous and next buffers.
    (when killed-buffers
      (dolist (buffer killed-buffers)
	(set-window-prev-buffers
	 window (assq-delete-all buffer (window-prev-buffers window)))
	(set-window-next-buffers
	 window (delq buffer (window-next-buffers window)))))

    ;; Return new-buffer.
    new-buffer))

(defun get-next-valid-buffer (list &optional buffer visible-ok frame)
  "Search LIST for a valid buffer to display in FRAME.
Return nil when all buffers in LIST are undesirable for display,
otherwise return the first suitable buffer in LIST.

Buffers not visible in windows are preferred to visible buffers,
unless VISIBLE-OK is non-nil.
If the optional argument FRAME is nil, it defaults to the selected frame.
If BUFFER is non-nil, ignore occurrences of that buffer in LIST."
  ;; This logic is more or less copied from other-buffer.
  (setq frame (or frame (selected-frame)))
  (let ((pred (frame-parameter frame 'buffer-predicate))
	found buf)
    (while (and (not found) list)
      (setq buf (car list))
      (if (and (not (eq buffer buf))
	       (buffer-live-p buf)
	       (or (null pred) (funcall pred buf))
	       (not (eq (aref (buffer-name buf) 0) ?\s))
	       (or visible-ok (null (get-buffer-window buf 'visible))))
	  (setq found buf)
	(setq list (cdr list))))
    (car list)))

(defun last-buffer (&optional buffer visible-ok frame)
  "Return the last buffer in FRAME's buffer list.
If BUFFER is the last buffer, return the preceding buffer
instead.  Buffers not visible in windows are preferred to visible
buffers, unless optional argument VISIBLE-OK is non-nil.
Optional third argument FRAME nil or omitted means use the
selected frame's buffer list.  If no such buffer exists, return
the buffer `*scratch*', creating it if necessary."
  (setq frame (or frame (selected-frame)))
  (or (get-next-valid-buffer (nreverse (buffer-list frame))
 			     buffer visible-ok frame)
      (get-buffer "*scratch*")
      (let ((scratch (get-buffer-create "*scratch*")))
	(set-buffer-major-mode scratch)
	scratch)))

(defun bury-buffer (&optional buffer-or-name)
  "Put BUFFER-OR-NAME at the end of the list of all buffers.
There it is the least likely candidate for `other-buffer' to
return; thus, the least likely buffer for \\[switch-to-buffer] to
select by default.

You can specify a buffer name as BUFFER-OR-NAME, or an actual
buffer object.  If BUFFER-OR-NAME is nil or omitted, bury the
current buffer.  Also, if BUFFER-OR-NAME is nil or omitted,
remove the current buffer from the selected window if it is
displayed there."
  (interactive)
  (let* ((buffer (normalize-live-buffer buffer-or-name)))
    ;; If `buffer-or-name' is not on the selected frame we unrecord it
    ;; although it's not "here" (call it a feature).
    (unrecord-buffer buffer)
    ;; Handle case where `buffer-or-name' is nil and the current buffer
    ;; is shown in the selected window.
    (cond
     ((or buffer-or-name (not (eq buffer (window-buffer)))))
     ((not (window-dedicated-p))
      (switch-to-prev-buffer nil 'bury))
     ((frame-root-window-p (selected-window))
      (iconify-frame (window-frame (selected-window))))
     ((window-deletable-p)
      (delete-window)))
    ;; Always return nil.
    nil))

(defun unbury-buffer ()
  "Switch to the last buffer in the buffer list."
  (interactive)
  (switch-to-buffer (last-buffer)))

(defun next-buffer ()
  "In selected window switch to next buffer."
  (interactive)
  (switch-to-next-buffer))

(defun previous-buffer ()
  "In selected window switch to previous buffer."
  (interactive)
  (switch-to-prev-buffer))

(defun delete-windows-on (&optional buffer-or-name frame)
  "Delete all windows showing BUFFER-OR-NAME.
BUFFER-OR-NAME may be a buffer or the name of an existing buffer
and defaults to the current buffer.

The following non-nil values of the optional argument FRAME
have special meanings:

- t means consider all windows on the selected frame only.

- `visible' means consider all windows on all visible frames.

- 0 (the number zero) means consider all windows on all visible
    and iconified frames.

- A frame means consider all windows on that frame only.

Any other value of FRAME means consider all windows on all
frames.

When a window showing BUFFER-OR-NAME is dedicated and the only
window of its frame, that frame is deleted when there are other
frames left."
  (interactive "BDelete windows on (buffer):\nP")
  (let ((buffer (normalize-live-buffer buffer-or-name))
	;; Handle the "inverted" meaning of the FRAME argument wrt other
	;; `window-list-1' based function.
	(all-frames (cond ((not frame) t) ((eq frame t) nil) (t frame))))
    (dolist (window (window-list-1 nil nil all-frames))
      (if (eq (window-buffer window) buffer)
	  (let ((deletable (window-deletable-p window)))
	    (cond
	     ((eq deletable 'frame)
	      ;; Delete frame.
	      (delete-frame (window-frame window)))
	     (deletable
	      ;; Delete window only.
	      (delete-window window))
	     (t
	      ;; In window switch to previous buffer.
	      (set-window-dedicated-p window nil)
	      (switch-to-prev-buffer window 'bury))))
	;; If a window doesn't show BUFFER, unrecord it nevertheless.
	(unrecord-window-buffer window buffer)))))

(defun replace-buffer-in-windows (&optional buffer-or-name)
  "Replace BUFFER-OR-NAME with some other buffer in all windows showing it.
BUFFER-OR-NAME may be a buffer or the name of an existing buffer
and defaults to the current buffer.

When a window showing BUFFER-OR-NAME is dedicated that window is
deleted.  If that window is the only window on its frame, that
frame is deleted too when there are other frames left.  If there
are no other frames left, some other buffer is displayed in that
window.

This function removes the buffer denoted by BUFFER-OR-NAME from
all window-local buffer lists."
  (let ((buffer (normalize-live-buffer buffer-or-name)))
    (dolist (window (window-list-1 nil nil t))
      (if (eq (window-buffer window) buffer)
	  (let ((deletable (window-deletable-p window)))
	    (cond
	     ((eq deletable 'frame)
	      ;; Delete frame.
	      (delete-frame (window-frame window)))
	     ((and (window-dedicated-p window) deletable)
	      ;; Delete window.
	      (delete-window window))
	     (t
	      ;; Switch to another buffer in window.
	      (set-window-dedicated-p window nil)
	      (switch-to-prev-buffer window 'kill))))
	;; Unrecord BUFFER in WINDOW.
	(unrecord-window-buffer window buffer)))))

(defun quit-restore-window (&optional window kill)
  "Quit WINDOW in some way.
WINDOW must be a live window and defaults to the selected window.
Return nil.

According to information stored in WINDOW's `quit-restore' window
parameter either \(1) delete WINDOW and its frame, \(2) delete
WINDOW, \(3) restore the buffer previously displayed in WINDOW,
or \(4) make WINDOW display some other buffer than the present
one.  If non-nil, reset `quit-restore' parameter to nil.

Optional argument KILL non-nil means in addition kill WINDOW's
buffer.  If KILL is nil, put WINDOW's buffer at the end of the
buffer list.  Interactively, KILL is the prefix argument."
  (interactive "i\nP")
  (setq window (normalize-live-window window))
  (let ((buffer (window-buffer window))
	(parameters (window-parameter window 'quit-restore))
	deletable)
    (cond
     ((and (eq (car-safe parameters) t)
	   (setq deletable (window-deletable-p window)))
      (unrecord-buffer buffer)
      ;; WINDOW is deletable.
      (if (eq deletable 'frame)
	  ;; WINDOW's frame can be deleted.
	  (delete-frame (window-frame window))
	;; Just delete WINDOW.
	(delete-window window))
      (when (window-live-p (nth 1 parameters))
	(select-window (nth 1 parameters))))
     ((and (buffer-live-p (nth 0 parameters))
	   ;; The buffer currently shown in WINDOW must still be the
	   ;; buffer shown when the `quit-restore' parameter was
	   ;; created in the first place.
	   (eq (window-buffer window) (nth 3 parameters)))
      ;; Unrecord buffer.
      (unrecord-buffer buffer)
      (unrecord-window-buffer window buffer)
      ;; The `quit-restore' parameters tell us the buffer to display in
      ;; WINDOW and how to do that.
      (set-window-dedicated-p window nil)
      (set-window-buffer window (nth 0 parameters))
      (set-window-start window (nth 1 parameters))
      (set-window-point window (nth 2 parameters))
      (unless (= (nth 4 parameters) (window-total-size window))
	(resize-window
	 window (- (nth 4 parameters) (window-total-size window))))
      (set-window-parameter window 'quit-restore nil)
      (when (window-live-p (nth 5 parameters))
	(select-window (nth 5 parameters))))
     ((and (window-dedicated-p window)
	   (setq deletable (window-deletable-p window)))
      (unrecord-buffer buffer)
      ;; WINDOW is dedicated and deletable.
      (if (eq deletable 'frame)
	  ;; WINDOW's frame can be deleted.
	  (delete-frame (window-frame window))
	;; Just delete WINDOW.
	(delete-window window)))
     (t
      ;; Otherwise, show another buffer in WINDOW.
      (set-window-parameter window 'quit-restore nil)
      (unrecord-buffer buffer)
      (switch-to-prev-buffer window 'bury-or-kill)))

    ;; Kill WINDOW's old-buffer if requested
    (when kill (kill-buffer buffer))
    nil))

;;; Splitting windows.
(defsubst window-split-min-size (&optional horflag)
  "Return minimum height of any window.
Optional argument HORFLAG non-nil means return minimum width."
  (if horflag
      (max window-min-width window-safe-min-width)
    (max window-min-height window-safe-min-height)))

(defun window-children-count (window)
  "Return number of child windows of WINDOW."
  (let ((count 0)
	(sub (window-child window)))
    (while sub
      (setq count (1+ count))
      (setq sub (window-right sub)))
    count))

(defun split-window (&optional window size horizontal)
  "Create a new window adjacent to WINDOW.
WINDOW can be any window and defaults to the selected one.  If
WINDOW was selected before invoking this function, it remains
selected.  Return the new window which is always a live window.

Optional argument SIZE a positive number means make WINDOW SIZE
lines/columns tall.  If SIZE is negative, make the new window
-SIZE lines/columns tall.  If and only if SIZE is negative, its
absolute value can be less than `window-min-height' or
`window-min-width'; so this command can make a new window as
small as one line or two columns.  SIZE defaults to half of
WINDOW's size.  The variable `window-splits' determines how the
size of other windows is affected by this function.

Optional third argument HORIZONTAL nil (or `below') specifies
that the new window shall be located below WINDOW.  HORIZONTAL
`above' means the new window shall be located above WINDOW.  In
both cases SIZE specifies the new number of lines for WINDOW \(or
the new window if SIZE is negative) including space reserved for
the mode and/or header line.

HORIZONTAL t (or `right') specifies that the new window shall be
located on the right side of WINDOW.  HORIZONTAL `left' means the
new window shall be located on the left of WINDOW.  In both cases
SIZE specifies the new number of columns for WINDOW \(or the new
window provided SIZE is negative) including space reserved for
fringes and the scrollbar or a divider column.  Any other non-nil
value for HORIZONTAL is currently handled like t (or `right').

If WINDOW is a component of a compound window \"split\" the root
of the compound window instead.  The new window does not become a
member of the compound window.  If WINDOW is a main window of a
window group, the new window becomes a main window in that window
group.  If WINDOW is a non-main component of a window group
signal an error.

If you split a live window, properties of the new window like
margins and scrollbars are inherited from WINDOW.  If you split
an internal window, these properties as well as the buffer
displayed in the new window are inherited from the selected
window on WINDOW's frame."
  (interactive "i")
  (setq window (normalize-any-window window))
  (let* ((horflag (and horizontal (not (memq horizontal '(below above)))))
	 (function (window-parameter window 'split-window))
	 ;; Rebind this locally since in some cases we do have to nest.
	 (window-splits window-splits)
	 ;; COMPOSITE non-nil means WINDOW is part of a composite
	 ;; window.  TYPE is the type of the composite window (either
	 ;; `compound' or `group').  ROLE is the role of WINDOW within
	 ;; the composite window (either `main', `support', or t).  ROOT
	 ;; is the root window of the composite window.
	 (composite (window-parameter window 'composite))
	 (type (car-safe composite))
	 (role (cdr-safe composite))
	 (root (and composite (composite-root-window window)))
	 old-composite new-root new-main)
    (catch 'done
      (cond
       ;; Ignore window parameters if `ignore-window-parameters' tells
       ;; so or the `split-window' window parameter equals t.
       ((or (not (memq ignore-window-parameters '(nil post)))
	    (eq function t)))
       ((functionp function)
	;; The `split-window' parameter specifies the function to call
	;; instead.  If this is `ignore', WINDOW won't be split.
	(throw 'done (funcall function window size horizontal)))
       ((and (not composite) (window-parameter window 'group)
	     (window-live-p window))
	;; WINDOW is a live group root window and not part of a
	;; composite window so we need a new group root window.  Note
	;; that if WINDOW is also the root of a compound window, that
	;; part remains unaffected by what we do here - WINDOW remains
	;; root of the compound window which is now a component of a
	;; window group.
	(setq window-splits 'nest)
	(setq new-root t))
       (composite
	(cond
	 ((or (not root) (not type) (not role))
	  ;; Something is broken in this composite window.  Signal a
	  ;; message but let the split pass through (we might signal
	  ;; an error here but for everday work this is too nasty).
	  (message "Broken component %s of composite window" window))
	 ((eq type 'compound)
	  ;; In a compound window split the root window.
	  (throw 'done (split-window root size horizontal)))
	 ((eq type 'group)
	  (cond
	   ((not (eq role 'main))
	    ;; In a window group we are only allowed to split main
	    ;; windows.  We might swallow this error silently.
	    (error "Cannot split non-main window %s in a window group" window))
	   ((or (not (window-parent window)) ; Should have been handled above.
		(not (eq (composite-lowest-child-role (window-parent window))
			 'main)))
	    (setq new-main t)
	    ;; We must nest since otherwise we might end up with a
	    ;; window group having two dominating main windows.
	    (setq window-splits 'nest)))))))
      ;; The following line is hopefully not needed ...
      ;; (setq window-splits (if (eq window root) 'nest window-splits))
      (let* ((frame (window-frame window))
	     (parent (window-parent window))
	     ;; Size calculations.
	     (parent-size (window-total-size parent horflag))
	     ;; Bind `old-size' to the current size of WINDOW and
	     ;; `new-size' to the size of the new window.
	     (old-size (window-total-size window horflag))
	     (resize
	      (and (eq window-splits 'resize)
		   ;; Resize makes sense in iso-combinations only.
		   (window-iso-combined-p window horflag)))
	     (new-size
	      (cond
	       ((not size)
		(max (window-split-min-size horflag)
		     (if resize
			 ;; For a "resize" split try to give the new
			 ;; window a fitting size (which must be at least
			 ;; as large as what we can get at all).
			 (min (- parent-size
				 (window-min-size parent horflag))
			      (/ parent-size
				 (1+ (window-children-count parent))))
		       ;; Else try to give the new window half the size of
		       ;; WINDOW.
		       (/ old-size 2))))
	       ((>= size 0)
		;; SIZE non-negative specifies the new size of WINDOW.
		(- old-size size))
	       (t
		;; SIZE negative specifies the size of the new window.
		(- size))))
	     (root (window-parameter window 'root)))
	(cond
	 ((and resize (not (window-sizable-p parent (- new-size) horflag))
	       ;; Try agin with minimum acceptable size.
	       (setq new-size
		     (max new-size 
			  (window-split-min-size horflag)))
	       (not (window-sizable-p parent (- new-size) horflag)))
	  (error "Cannot resize %s" window))
	 ((and (not resize)
	       (> (+ new-size (window-min-size window horflag))
		  old-size))
	  (error "Cannot resize %s" window))
	 ((< new-size
	     (if (and size (< size 0))
		 ;; If SIZE explicitly specifies a negative value, respect
		 ;; that.
		 (if horflag window-safe-min-width window-safe-min-height)
	       (if horflag window-min-width window-min-height)))
	  (error "New window too small")))

	(resize-window-reset (window-frame window) horflag)
	(cond
	 (resize
	  ;; Try to get space from OLD's siblings.  We could go "up" and
	  ;; try getting additional space from surrounding windows but we
	  ;; won't be able to return space to those windows when we delete
	  ;; the one we create here.  Hence we do not go up.
	  (resize-subwindows parent (- new-size) horflag)
	  (let* ((parent-size (window-total-size parent horflag))
		 (sub (window-child parent)))
	    ;; Assign new normal sizes.
	    (while sub
	      (resize-window-normal
	       sub (/ (* (float (window-normal-size sub horflag))
			 (- parent-size new-size))
		      parent-size))
	      (setq sub (window-right sub)))))
	 ((eq window-splits 'nest)
	  ;; Get entire space from WINDOW making sure that a new parent
	  ;; windows gets created.
	  (resize-window-total window (- old-size new-size))
	  (resize-this-window window (- new-size) horflag)
	  (resize-window-normal
	   window (/ (float (window-new-total-size window)) old-size)))
	 (t
	  ;; Get entire space from WINDOW making a new parent window only
	  ;; if we need one.
	  (resize-window-total window (- old-size new-size))
	  (resize-this-window window (- new-size) horflag)
	  (resize-window-normal
	   window (/ (float (window-new-total-size window))
		     (window-total-size (window-parent window) horflag)))))

	(let* ((new (split-window-internal window new-size horizontal))
	       (new-parent (window-parent new)))
	  (when (memq ignore-window-parameters '(nil pre))
	    (cond
	     ((and new-root (not (eq parent new-parent)))
	      ;; `new-parent' becomes the new group root window
	      ;; inheriting WINDOW's composite status.  WINDOW and `new'
	      ;; become main windows of that group.
	      (set-window-parameter new-parent 'group t)
	      (set-window-parameter new-parent 'composite composite)
	      (set-window-parameter window 'group nil)
	      (set-window-parameter window 'composite (cons 'group 'main))
	      (set-window-parameter new 'composite (cons 'group 'main)))
	     ((and new-main (not (eq parent new-parent)))
	      ;; `new-parent' becomes the new dominating main window of
	      ;; WINDOW's group.
	      (set-window-parameter new-parent 'composite (cons 'group 'main))
	      (set-window-parameter window 'composite (cons 'group 'main))
	      (set-window-parameter new 'composite (cons 'group 'main)))
	     (composite
	      ;; `new' inherits parameters from WINDOW.
	      (set-window-parameter new 'composite composite)
	      (when (not (eq parent new-parent))
		;; `new-parent' "inherits" the parameters as well
		(set-window-parameter new-parent 'composite composite)))))
	  ;; We have to check once more how often these hooks are run.
	  (run-window-configuration-change-hook frame)
	  ;; Return the new window.
	  new)))))

;; I think this should be the default; I think people will prefer it--rms.
(defcustom split-window-keep-point t
  "If non-nil, \\[split-window-vertically] keeps the original point \
in both children.
This is often more convenient for editing.
If nil, adjust point in each of the two windows to minimize redisplay.
This is convenient on slow terminals, but point can move strangely.

This option applies only to `split-window-vertically' and
functions that call it.  `split-window' always keeps the original
point in both children."
  :type 'boolean
  :group 'windows)

(defun split-window-quit-restore (new-window old-window)
  "Copy `quit-restore' parameter from OLD-WINDOW to NEW-WINDOW.
Do this if and only if NEW-WINDOW's buffer is in `view-mode'."
  (let ((parameter (window-parameter old-window 'quit-restore)))
    (when (and parameter
	       (with-current-buffer (window-buffer new-window)
		 view-mode))
      (set-window-parameter new-window 'quit-restore parameter))))

(defun split-window-vertically (&optional size)
  "Split selected window into two windows, one above the other.
The upper window gets SIZE lines and the lower one gets the rest.
SIZE negative means the lower window gets -SIZE lines and the
upper one the rest.  With no argument, split windows equally or
close to it.  Both windows display the same buffer, now current.

If the variable `split-window-keep-point' is non-nil, both new
windows will get the same value of point as the selected window.
This is often more convenient for editing.  The upper window is
the selected window.

Otherwise, we choose window starts so as to minimize the amount of
redisplay; this is convenient on slow terminals.  The new selected
window is the one that the current value of point appears in.  The
value of point can change if the text around point is hidden by the
new mode line.

Regardless of the value of `split-window-keep-point', the upper
window is the original one and the return value is the new, lower
window."
  (interactive "P")
  (let ((old-window (selected-window))
	(old-point (point))
	(size (and size (prefix-numeric-value size)))
        moved-by-window-height moved new-window bottom)
    (when (and size (< size 0) (< (- size) window-min-height))
      ;; `split-window' would not signal an error here.
      (error "Size of new window too small"))
    (setq new-window (split-window nil size))
    (unless split-window-keep-point
      (with-current-buffer (window-buffer)
	(goto-char (window-start))
	(setq moved (vertical-motion (window-height)))
	(set-window-start new-window (point))
	(when (> (point) (window-point new-window))
	  (set-window-point new-window (point)))
	(when (= moved (window-height))
	  (setq moved-by-window-height t)
	  (vertical-motion -1))
	(setq bottom (point)))
      (and moved-by-window-height
	   (<= bottom (point))
	   (set-window-point old-window (1- bottom)))
      (and moved-by-window-height
	   (<= (window-start new-window) old-point)
	   (set-window-point new-window old-point)
	   (select-window new-window)))
    (split-window-quit-restore new-window old-window)
    new-window))

(defun split-window-horizontally (&optional size)
  "Split selected window into two windows side by side.
The selected window becomes the left one and gets SIZE columns.
SIZE negative means the right window gets -SIZE lines.

SIZE includes the width of the window's scroll bar; if there are
no scroll bars, it includes the width of the divider column to
the window's right, if any.  SIZE omitted or nil means split
window equally.

The selected window remains selected.  Return the new window."
  (interactive "P")
  (let ((old-window (selected-window))
	(size (and size (prefix-numeric-value size)))
	new-window)
    (when (and size (< size 0) (< (- size) window-min-width))
      ;; `split-window' would not signal an error here.
      (error "Size of new window too small"))
    (setq new-window (split-window nil size t))
    (split-window-quit-restore new-window old-window)
    new-window))

;;; Composite windows.
(defun make-compound-window (&optional window main size horizontal)
  "Make WINDOW the main window of a new compound window.
This function creates a new internal window with WINDOW and a new
leaf window as its only children.  WINDOW must be a leaf window
and defaults to the selected window.

Optional argument MAIN non-nil makes the new leaf window a main
window.  MAIN nil or not provided means the new leaf window
becomes a support window.  WINDOW itself becomes a main window.

Optional arguments SIZE and HORIZONTAL are as for `split-window'.

Return the new leaf window."
  (setq window (normalize-any-window window))
  (unless (or (window-live-p window) (composite-root-window-p window))
    (error "Window %s must be live or a composite root window" window))
  (let* ((composite (window-parameter window 'composite))
	 ;; FORCE and NEST.
	 (ignore-window-parameters t)
	 (window-splits 'nest)
	 (new (split-window window size horizontal))
	 (new-parent (window-parent new)))
    (set-window-parameter new-parent 'compound t)
    (when composite (set-window-parameter new-parent 'composite composite))
    (set-window-parameter window 'composite (cons 'compound 'main))
    (set-window-parameter
     new 'composite (cons 'compound (if main 'main 'support)))
    new))

(defun make-window-group (&optional window)
  "Make WINDOW main and root window of a new window group.
WINDOW must be a live window and defaults to the selected one.
Return WINDOW."
  (setq window (normalize-live-window window))
  (set-window-parameter window 'composite (cons 'group 'main)))

(defun make-support-window (window support &optional size horizontal)
  "Add support window of type SUPPORT to WINDOW."
  (let* ((compound (window-parameter window 'compound))
	 (group (window-parameter window 'group))
	 (composite (window-parameter window 'composite))
	 (type (car-safe composite))
	 (role (cdr-safe composite))
	 ;; `type' is the type of the composite window (either
	 ;; `compound' or `group').  `role' is the role of WINDOW within
	 ;; the composite window (either `main', `support', or t).
	 (root (when composite (composite-root-window window)))
	 (parent (window-parent window))
	 (ignore-window-parameters t)
	 (window-splits 'nest)
	 new new-parent)
    (cond
     ((not (memq support '(compound group)))
      (error "Invalid support argument %s" support))
     ((and (eq support 'compound) (not compound) (not (eq type 'compound)))
      (error "Window %s is not a component of a compound window" window))
     ((and (eq support 'group) (not group) (not (eq type 'group)))
      (error "Window %s is not a component of a window group" window))
     ((and (eq type 'main) (not (eq window (composite-major-window window))))
      (error "Can't embed support window in main window")))
    (setq new (split-window window size horizontal))
    (unless (eq parent (window-parent window))
      (setq new-parent (window-parent window)))
    (cond
     ;; This conditional looks incredibly tedious but let's keep the
     ;; distinct cases self-contained to avoid further confusion.
     ((and compound (eq support 'compound))
      (when new-parent
	;; `new-parent' inherits the compound status of `window'
	(set-window-parameter new-parent 'compound t)
	(set-window-parameter window 'compound nil)
	(when composite
	  ;; `new-parent' inherits the composite status of `window'.
	  (set-window-parameter new-parent 'composite composite)
	  (set-window-parameter
	   ;; Give `window' the highest role of its children.
	   window 'composite (cons 'compound
				   (composite-lowest-child-role window)))))
      (when group
	;; `new-parent' does not inherit the group status of `window'
	;; (but make sure `window' retains it).
	(set-window-parameter window 'group t))
      (set-window-parameter new 'composite (cons 'compound 'support)))
     ((and group (eq support 'group))
      (when new-parent
	;; `new-parent' inherits the group status of `window'
	(set-window-parameter new-parent 'group t)
	(set-window-parameter window 'group nil)
	(when composite
	  ;; `new-parent' inherits the composite status of `window'.
	  (set-window-parameter new-parent 'composite composite)
	  (set-window-parameter
	   ;; Give `window' the highest role of its children.
	   window 'composite (cons 'group
				   (composite-lowest-child-role window)))))
      (when compound
	;; `new-parent' does not inherit the compound status of `window'
	;; (but make sure `window' retains it).
	(set-window-parameter window 'compound t))
      (set-window-parameter new 'composite (cons 'group 'support)))
     ((and (eq type 'compound) (eq support 'compound))
      (cond
       (new-parent
	(let ((role (if (eq role 'support) 'support t)))
	  (set-window-parameter new-parent 'composite (cons 'compound role))))
       ((not (compound-window-p parent))
	(let ((role (if (composite-support-window-p parent) 'support t)))
	  (set-window-parameter parent 'composite (cons 'compound role)))))
      (when group
	;; `new-parent' does not inherit the group status of `window'
	;; (but make sure `window' retains it).
	(set-window-parameter window 'group t))
      (set-window-parameter window 'composite composite)
      (set-window-parameter new 'composite (cons 'compound 'support)))
     ((and (eq type 'group) (eq support 'group))
      (cond
       (new-parent
	(let ((role (if (eq role 'support) 'support t)))
	  (set-window-parameter new-parent 'composite (cons 'group role))))
       ((not (compound-window-p parent))
	(let ((role (if (composite-support-window-p parent) 'support t)))
	  (set-window-parameter parent 'composite (cons 'group role)))))
      (when compound
	;; `new-parent' does not inherit the compound status of `window'
	;; (but make sure `window' retains it).
	(set-window-parameter window 'compound t))
      (set-window-parameter window 'composite composite)
      (set-window-parameter new 'composite (cons 'group 'support))))
    new))

;;; Balancing windows.
(defun balance-windows (&optional window-or-frame)
  "Balance the sizes of subwindows of WINDOW-OR-FRAME.
WINDOW-OR-FRAME is optional and defaults to the selected frame.
If WINDOW-OR-FRAME denotes a frame, balance the sizes of all
subwindows of that frame's root window.  If WINDOW-OR-FRAME
denots a window, balance the sizes of all subwindows of that
window."
  (interactive)
  (let* ((window
	  (cond
	   ((or (not window-or-frame)
		(frame-live-p window-or-frame))
	    (frame-root-window window-or-frame))
	   ((or (window-live-p window-or-frame)
		(window-child window-or-frame))
	    window-or-frame)
	   (t
	    (error "Not a window or frame %s" window-or-frame))))
	 (frame (window-frame window)))
    ;; Balance vertically.
    (resize-window-reset (window-frame window))
    (balance-windows-1 window)
    (resize-window-apply frame)
    ;; Balance horizontally.
    (resize-window-reset (window-frame window) t)
    (balance-windows-1 window t)
    (resize-window-apply frame t)))

(defun balance-windows-1 (window &optional horizontal)
  "Subroutine of `balance-windows'."
  (if (window-child window)
      (let ((sub (window-child window)))
	(if (window-iso-combined-p sub horizontal)
	    (balance-windows-2 window horizontal)
	  (let ((size (window-new-total-size window)))
	    (while sub
	      (resize-window-total sub size) 
	      (balance-windows-1 sub horizontal)
	      (setq sub (window-right sub))))))))

(defun balance-windows-2 (window horizontal)
  "Subroutine of `balance-windows-1'.
WINDOW must be an iso-combination."
  (let* ((first (window-child window))
	 (sub first)
	 (number-of-children 0)
	 (parent-size (window-new-total-size window))
	 (total-sum parent-size)
	 found failed size sub-total sub-delta sub-amount rest)
    (while sub
      (setq number-of-children (1+ number-of-children))
      (when (window-size-fixed-p sub horizontal)
	(setq total-sum
	      (- total-sum (window-total-size sub horizontal)))
	(resize-window-normal sub 'ignore))
      (setq sub (window-right sub)))

    (setq failed t)
    (while (and failed (> number-of-children 0))
      (setq size (/ total-sum number-of-children))
      (setq failed nil)
      (setq sub first)
      (while (and sub (not failed))
	;; Ignore subwindows that should be ignored or are stuck.
	(unless (resize-subwindows-skip-p sub)
	  (setq found t)
	  (setq sub-total (window-total-size sub horizontal))
	  (setq sub-delta (- size sub-total))
	  (setq sub-amount
		(window-sizable sub sub-delta horizontal))
	  ;; Register the new total size for this subwindow.
	  (resize-window-total sub (+ sub-total sub-amount))
	  (unless (= sub-amount sub-delta)
	    (setq total-sum (- total-sum sub-total sub-amount))
	    (setq number-of-children (1- number-of-children))
	    ;; We failed and need a new round.
	    (setq failed t)
	    (resize-window-normal sub 'skip)))
	(setq sub (window-right sub))))

    (setq rest (% total-sum number-of-children))
    ;; Fix rounding by trying to enlarge non-stuck windows by one line
    ;; (column) until `rest' is zero.
    (setq sub first)
    (while (and sub (> rest 0))
      (unless (resize-subwindows-skip-p window)
	(resize-window-total sub 1 t)
	(setq rest (1- rest)))
      (setq sub (window-right sub)))

    ;; Fix rounding by trying to enlarge stuck windows by one line
    ;; (column) until `rest' equals zero.
    (setq sub first)
    (while (and sub (> rest 0))
      (unless (eq (window-new-normal-size sub) 'ignore)
	(resize-window-total sub 1 t)
	(setq rest (1- rest)))
      (setq sub (window-right sub)))

    (setq sub first)
    (while sub
      ;; Record new normal sizes.
      (resize-window-normal
       sub (/ (if (eq (window-new-normal-size sub) 'ignore)
		  (window-total-size sub horizontal)
		(window-new-total-size sub))
	      (float parent-size)))
      ;; Recursively balance each subwindow's subwindows.
      (balance-windows-1 sub horizontal)
      (setq sub (window-right sub)))))

(defun window-fixed-size-p (&optional window direction)
  "Return t if WINDOW cannot be resized in DIRECTION.
WINDOW defaults to the selected window.  DIRECTION can be
nil (i.e. any), `height' or `width'."
  (with-current-buffer (window-buffer window)
    (when (and (boundp 'window-size-fixed) window-size-fixed)
      (not (and direction
		(member (cons direction window-size-fixed)
			'((height . width) (width . height))))))))

;;; A different solution to balance-windows.
(defvar window-area-factor 1
  "Factor by which the window area should be over-estimated.
This is used by `balance-windows-area'.
Changing this globally has no effect.")
(make-variable-buffer-local 'window-area-factor)

(defun balance-windows-area-adjust (window delta horizontal)
  "Wrapper around `resize-window' with error checking.
Arguments WINDOW, DELTA and HORIZONTAL are passed on to that function."
  ;; `resize-window' may fail if delta is too large.
  (while (>= (abs delta) 1)
    (condition-case err
        (progn
          (resize-window window delta horizontal)
          (setq delta 0))
      (error
       ;;(message "adjust: %s" (error-message-string err))
       (setq delta (/ delta 2))))))

(defun balance-windows-area ()
  "Make all visible windows the same area (approximately).
See also `window-area-factor' to change the relative size of
specific buffers."
  (interactive)
  (let* ((unchanged 0) (carry 0) (round 0)
         ;; Remove fixed-size windows.
         (wins (delq nil (mapcar (lambda (win)
                                   (if (not (window-fixed-size-p win)) win))
                                 (window-list nil 'nomini))))
         (changelog nil)
         next)
    ;; Resizing a window changes the size of surrounding windows in complex
    ;; ways, so it's difficult to balance them all.  The introduction of
    ;; `adjust-window-trailing-edge' made it a bit easier, but it is still
    ;; very difficult to do.  `balance-window' above takes an off-line
    ;; approach: get the whole window tree, then balance it, then try to
    ;; adjust the windows so they fit the result.
    ;; Here, instead, we take a "local optimization" approach, where we just
    ;; go through all the windows several times until nothing needs to be
    ;; changed.  The main problem with this approach is that it's difficult
    ;; to make sure it terminates, so we use some heuristic to try and break
    ;; off infinite loops.
    ;; After a round without any change, we allow a second, to give a chance
    ;; to the carry to propagate a minor imbalance from the end back to
    ;; the beginning.
    (while (< unchanged 2)
      ;; (message "New round")
      (setq unchanged (1+ unchanged) round (1+ round))
      (dolist (win wins)
        (setq next win)
        (while (progn (setq next (next-window next))
                      (window-fixed-size-p next)))
        ;; (assert (eq next (or (cadr (member win wins)) (car wins))))
        (let* ((horiz
                (< (car (window-edges win)) (car (window-edges next))))
               (areadiff (/ (- (* (window-height next) (window-width next)
                                  (buffer-local-value 'window-area-factor
                                                      (window-buffer next)))
                               (* (window-height win) (window-width win)
                                  (buffer-local-value 'window-area-factor
                                                      (window-buffer win))))
                            (max (buffer-local-value 'window-area-factor
                                                     (window-buffer win))
                                 (buffer-local-value 'window-area-factor
                                                     (window-buffer next)))))
               (edgesize (if horiz
                             (+ (window-height win) (window-height next))
                           (+ (window-width win) (window-width next))))
               (diff (/ areadiff edgesize)))
          (when (zerop diff)
            ;; Maybe diff is actually closer to 1 than to 0.
            (setq diff (/ (* 3 areadiff) (* 2 edgesize))))
          (when (and (zerop diff) (not (zerop areadiff)))
            (setq diff (/ (+ areadiff carry) edgesize))
            ;; Change things smoothly.
            (if (or (> diff 1) (< diff -1)) (setq diff (/ diff 2))))
          (if (zerop diff)
              ;; Make sure negligible differences don't accumulate to
              ;; become significant.
              (setq carry (+ carry areadiff))
	    ;; This used `adjust-window-trailing-edge' before and uses
	    ;; `resize-window' now.  Error wrapping is still needed.
	    (balance-windows-area-adjust win diff horiz)
            ;; (sit-for 0.5)
            (let ((change (cons win (window-edges win))))
              ;; If the same change has been seen already for this window,
              ;; we're most likely in an endless loop, so don't count it as
              ;; a change.
              (unless (member change changelog)
                (push change changelog)
                (setq unchanged 0 carry 0)))))))
    ;; We've now basically balanced all the windows.
    ;; But there may be some minor off-by-one imbalance left over,
    ;; so let's do some fine tuning.
    ;; (bw-finetune wins)
    ;; (message "Done in %d rounds" round)
    ))


;;; Displaying buffers.
(defgroup display-buffer nil
  "Displaying buffers in windows."
  :version "24.1"
  :group 'windows)

(defcustom display-buffer-function nil
  "If non-nil, function to call to display a buffer.
`display-buffer' calls this function with two arguments, the
buffer to display and a flag which if non-nil means that the
selected window is not acceptable for displaying the buffer.  It
should choose or create a window, display the specified buffer in
it, and return the window.

Commands such as `switch-to-buffer-other-window' and
`find-file-other-window' work using this function."
  :type '(choice
	  (const nil)
	  (function :tag "function"))
  :group 'display-buffer)

(defcustom special-display-buffer-names nil
  "List of names of buffers that should be displayed specially.
Displaying a buffer with `display-buffer' or `pop-to-buffer', if
its name is in this list, displays the buffer in a way specified
by `special-display-function'.  `special-display-popup-frame'
\(the default for `special-display-function') usually displays
the buffer in a separate frame made with the parameters specified
by `special-display-frame-alist'.  If `special-display-function'
has been set to some other function, that function is called with
the buffer as first, and nil as second argument.

Alternatively, an element of this list can be specified as
\(BUFFER-NAME FRAME-PARAMETERS), where BUFFER-NAME is a buffer
name and FRAME-PARAMETERS an alist of \(PARAMETER . VALUE) pairs.
`special-display-popup-frame' will interpret such pairs as frame
parameters when it creates a special frame, overriding the
corresponding values from `special-display-frame-alist'.

As a special case, if FRAME-PARAMETERS contains (same-window . t)
`special-display-popup-frame' displays that buffer in the
selected window.  If FRAME-PARAMETERS contains (same-frame . t),
it displays that buffer in a window on the selected frame.

If `special-display-function' specifies some other function than
`special-display-popup-frame', that function is called with the
buffer named BUFFER-NAME as first, and FRAME-PARAMETERS as second
argument.

Finally, an element of this list can be also specified as
\(BUFFER-NAME FUNCTION OTHER-ARGS).  In that case,
`special-display-popup-frame' will call FUNCTION with the buffer
named BUFFER-NAME as first argument, and OTHER-ARGS as the
second.  If `special-display-function' specifies some other
function, that function is called with the buffer named
BUFFER-NAME as first, and the element's cdr as second argument.

If this variable appears \"not to work\", because you added a
name to it but the corresponding buffer is displayed in the
selected window, look at the values of `same-window-buffer-names'
and `same-window-regexps'.  Those variables take precedence over
this one.

See also `special-display-regexps'."
  :type '(repeat
	  (choice :tag "Buffer"
		  :value ""
		  (string :format "%v")
		  (cons :tag "With parameters"
			:format "%v"
			:value ("" . nil)
			(string :format "%v")
			(repeat :tag "Parameters"
				(cons :format "%v"
				      (symbol :tag "Parameter")
				      (sexp :tag "Value"))))
		  (list :tag "With function"
			:format "%v"
			:value ("" . nil)
			(string :format "%v")
			(function :tag "Function")
			(repeat :tag "Arguments" (sexp)))))
  :group 'display-buffer
  :group 'frames)

;;;###autoload
(put 'special-display-buffer-names 'risky-local-variable t)

(defcustom special-display-regexps nil
  "List of regexps saying which buffers should be displayed specially.
Displaying a buffer with `display-buffer' or `pop-to-buffer', if
any regexp in this list matches its name, displays it specially
using `special-display-function'.

The function `special-display-popup-frame' \(the default for
`special-display-function') usually displays the buffer in a
separate frame made with the parameters specified by
`special-display-frame-alist'.  If `special-display-function' has
been set to some other function, that function is called with the
buffer as first, and nil as second argument.

Alternatively, an element of this list can be specified as
\(REGEXP FRAME-PARAMETERS), where REGEXP is a regexp as above and
FRAME-PARAMETERS an alist of (PARAMETER . VALUE) pairs.
`special-display-popup-frame' will then interpret these pairs as
frame parameters when creating a special frame for a buffer whose
name matches REGEXP, overriding the corresponding values from
`special-display-frame-alist'.

As a special case, if FRAME-PARAMETERS contains (same-window . t)
`special-display-popup-frame' displays buffers matching REGEXP in
the selected window.  \(same-frame . t) in FRAME-PARAMETERS means
to display such buffers in a window on the selected frame.

If `special-display-function' specifies some other function than
`special-display-popup-frame', that function is called with the
buffer whose name matched REGEXP as first, and FRAME-PARAMETERS
as second argument.

Finally, an element of this list can be also specified as
\(REGEXP FUNCTION OTHER-ARGS).  `special-display-popup-frame'
will then call FUNCTION with the buffer whose name matched
REGEXP as first, and OTHER-ARGS as second argument.  If
`special-display-function' specifies some other function, that
function is called with the buffer whose name matched REGEXP
as first, and the element's cdr as second argument.

If this variable appears \"not to work\", because you added a
name to it but the corresponding buffer is displayed in the
selected window, look at the values of `same-window-buffer-names'
and `same-window-regexps'.  Those variables take precedence over
this one.

See also `special-display-buffer-names'."
  :type '(repeat
	  (choice :tag "Buffer"
		  :value ""
		  (regexp :format "%v")
		  (cons :tag "With parameters"
			:format "%v"
			:value ("" . nil)
			(regexp :format "%v")
			(repeat :tag "Parameters"
				(cons :format "%v"
				      (symbol :tag "Parameter")
				      (sexp :tag "Value"))))
		  (list :tag "With function"
			:format "%v"
			:value ("" . nil)
			(regexp :format "%v")
			(function :tag "Function")
			(repeat :tag "Arguments" (sexp)))))
  :group 'display-buffer
  :group 'frames)

(defun special-display-p (buffer-name)
  "Return non-nil if a buffer named BUFFER-NAME is displayed specially.
More precisely, return t if `special-display-buffer-names' or
`special-display-regexps' contain a string entry equaling or
matching BUFFER-NAME.  If `special-display-buffer-names' or
`special-display-regexps' contain a list entry whose car equals
or matches BUFFER-NAME, the return value is the cdr of that
entry."
  (let (tmp)
    (cond
     ((not (stringp buffer-name)))
     ((member buffer-name special-display-buffer-names)
      t)
     ((setq tmp (assoc buffer-name special-display-buffer-names))
      (cdr tmp))
     ((catch 'found
	(dolist (regexp special-display-regexps)
	  (cond
	   ((stringp regexp)
	    (when (string-match-p regexp buffer-name)
	      (throw 'found t)))
	   ((and (consp regexp) (stringp (car regexp))
		 (string-match-p (car regexp) buffer-name))
	    (throw 'found (cdr regexp))))))))))

(defcustom special-display-function 'special-display-popup-frame
  "Function to call for displaying special buffers.
This function is called with two arguments - the buffer and,
optionally, a list - and should return a window displaying that
buffer.  The default value usually makes a separate frame for the
buffer using `special-display-frame-alist' to specify the frame
parameters.  See the definition of `special-display-popup-frame'
for how to specify such a function.

A buffer is special when its name is either listed in
`special-display-buffer-names' or matches a regexp in
`special-display-regexps'."
  :type 'function
  :group 'frames)

(defcustom same-window-buffer-names nil
  "List of names of buffers that should appear in the \"same\" window.
`display-buffer' and `pop-to-buffer' show a buffer whose name is
on this list in the selected rather than some other window.

An element of this list can be a cons cell instead of just a
string.  In that case, the cell's car must be a string specifying
the buffer name.  This is for compatibility with
`special-display-buffer-names'; the cdr of the cons cell is
ignored.

See also `same-window-regexps'."
 :type '(repeat (string :format "%v"))
 :group 'display-buffer)

(defcustom same-window-regexps nil
  "List of regexps saying which buffers should appear in the \"same\" window.
`display-buffer' and `pop-to-buffer' show a buffer whose name
matches a regexp on this list in the selected rather than some
other window.

An element of this list can be a cons cell instead of just a
string.  In that case, the cell's car must be a regexp matching
the buffer name.  This is for compatibility with
`special-display-regexps'; the cdr of the cons cell is ignored.

See also `same-window-buffer-names'."
  :type '(repeat (regexp :format "%v"))
  :group 'display-buffer)

(defun same-window-p (buffer-name)
  "Return non-nil if a buffer named BUFFER-NAME would be shown in the \"same\" window.
This function returns non-nil if `display-buffer' or
`pop-to-buffer' would show a buffer named BUFFER-NAME in the
selected rather than \(as usual\) some other window.  See
`same-window-buffer-names' and `same-window-regexps'."
  (cond
   ((not (stringp buffer-name)))
   ;; The elements of `same-window-buffer-names' can be buffer
   ;; names or cons cells whose cars are buffer names.
   ((member buffer-name same-window-buffer-names))
   ((assoc buffer-name same-window-buffer-names))
   ((catch 'found
      (dolist (regexp same-window-regexps)
	;; The elements of `same-window-regexps' can be regexps
	;; or cons cells whose cars are regexps.
	(when (or (and (stringp regexp)
		       (string-match regexp buffer-name))
		  (and (consp regexp) (stringp (car regexp))
		       (string-match-p (car regexp) buffer-name)))
	  (throw 'found t)))))))

(defcustom pop-up-frames nil
  "Whether `display-buffer' should make a separate frame.
If nil, never make a separate frame.
If the value is `graphic-only', make a separate frame
on graphic displays only.
Any other non-nil value means always make a separate frame."
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "On graphic displays only" graphic-only)
	  (const :tag "Always" t))
  :group 'display-buffer
  :group 'frames)

(defcustom display-buffer-reuse-frames nil
  "Non-nil means `display-buffer' should reuse frames.
If the buffer in question is already displayed in a frame, raise
that frame."
  :type 'boolean
  :version "21.1"
  :group 'display-buffer
  :group 'frames)

(defcustom pop-up-windows t
  "Non-nil means `display-buffer' is allowed to make a new window.
A non-empty list specifies the windows `display-buffer' will
consider for splitting.  The following entries are supported
where \"frame\" refers to the frame chosen to display the buffer:

 largest ...... largest window
 lru .......... least recently used window
 selected ..... frame's selected window
 root ......... frame's root window 

The default value t stands for the list `(largest lru)'.  This
means that `display-buffer' will first try to split the largest
window and, if that fails, the least recently used window."
  :type '(choice
	  (const :tag "Disallow" nil)
	  (const :tag "Allow" t)
	  (repeat :tag "Preferences"
		  (choice
		   (const :tag "Largest" largest)
		   (const :tag "Least Recently Used" lru)
		   (const :tag "Selected" selected)
		   (const :tag "Frame Root Window" root))))
  :group 'display-buffer)

(defcustom split-window-preferred-function 'split-window-sensibly
  "Function called by `display-buffer' to split a window.
This function is called with a window as single argument and is
supposed to split that window and return the new window.  If the
window can (or shall) not be split, it is supposed to return nil.

The default is to call the function `split-window-sensibly' which
tries to split the window in a way which seems most suitable.
You can customize the options `split-height-threshold' and/or
`split-width-threshold' in order to have `split-window-sensibly'
prefer either vertical or horizontal splitting.

If you set this to any other function, bear in mind that
`display-buffer' may call that function repeatedly; the option
`pop-up-windows' controls which windows may become the argument
of this function.

The window selected at the time `display-buffer' was invoked is
still selected when this function is called.  Hence you can
compare the window argument with the value of `selected-window'
if you intend to split the selected window instead or if you do
not want to split the selected window."
  :type 'function
  :version "23.1"
  :group 'display-buffer)

(defcustom split-height-threshold 80
  "Minimum height for splitting a window to display a buffer.
If this is an integer, `display-buffer' can split a window
vertically only if it has at least this many lines.  If this is
nil, `display-buffer' does not split windows vertically.  If a
window is the only window on its frame, `display-buffer' may
split it vertically disregarding the value of this variable."
  :type '(choice (const nil) (integer :tag "lines"))
  :version "23.1"
  :group 'display-buffer)

(defcustom split-width-threshold 160
  "Minimum width for splitting a window to display a buffer.
If this is an integer, `display-buffer' can split a window
horizontally only if it has at least this many columns.  If this
is nil, `display-buffer' cannot split windows horizontally."
  :type '(choice (const nil) (integer :tag "columns"))
  :version "23.1"
  :group 'display-buffer)

(defun window-sensibly-splittable-p (window &optional horizontal)
  "Return non-nil if `split-window-sensibly' may split WINDOW.
Optional argument HORIZONTAL nil or omitted means check whether
`split-window-sensibly' may split WINDOW vertically.  HORIZONTAL
non-nil means check whether WINDOW may be split horizontally.

WINDOW may be split vertically when the following conditions
hold:
- `window-size-fixed' is either nil or equals `width' for the
  buffer of WINDOW.
- `split-height-threshold' is an integer and WINDOW is at least as
  high as `split-height-threshold'.
- When WINDOW is split evenly, the emanating windows are at least
  `window-min-height' lines tall and can accommodate at least one
  line plus - if WINDOW has one - a mode line.

WINDOW may be split horizontally when the following conditions
hold:
- `window-size-fixed' is either nil or equals `height' for the
  buffer of WINDOW.
- `split-width-threshold' is an integer and WINDOW is at least as
  wide as `split-width-threshold'.
- When WINDOW is split evenly, the emanating windows are at least
  `window-min-width' or two (whichever is larger) columns wide."
  (let* ((min-size
	 ;; The minimum size of any popped-up window.
	 (if horizontal
	     (and (numberp split-width-threshold)
		  (max (/ split-width-threshold 2)
		       window-min-width))
	   (and (numberp split-height-threshold)
		(max (/ split-height-threshold 2)
		     window-min-height))))
	 (parent (window-parent window))
	 ;; Bind both of these to min-size, only one will matter.
	 (window-min-width min-size)
	 (window-min-height min-size))
    (when min-size
      ;; `display-buffer' is not allowed to override `window-splits'.
      (if (and parent (eq window-splits 'resize)
	       (window-iso-combined-p window horizontal))
	  (>= (- (window-total-size parent horizontal)
		 (window-min-size parent horizontal))
	      min-size)
	(and (not (window-size-fixed-p window horizontal))
	     (>= (window-total-size window horizontal)
		 (+ (max (window-min-size window horizontal)
			 min-size)
		    min-size)))))))

(defun split-window-sensibly (window)
  "Split WINDOW in a way suitable for `display-buffer'.
If `split-height-threshold' specifies an integer, WINDOW is at
least `split-height-threshold' lines tall and can be split
vertically, split WINDOW into two windows one above the other and
return the lower window.  Otherwise, if `split-width-threshold'
specifies an integer, WINDOW is at least `split-width-threshold'
columns wide and can be split horizontally, split WINDOW into two
windows side by side and return the window on the right.  If this
can't be done either and WINDOW is the only window on its frame,
try to split WINDOW vertically disregarding any value specified
by `split-height-threshold'.  If that succeeds, return the lower
window.  Return nil otherwise.

By default `display-buffer' routines call this function to split
the largest or least recently used window.  To change the default
beahvior customize the option `split-window-preferred-function'."
  (or (and (window-sensibly-splittable-p window)
	   ;; Split window vertically.
	   (if (window-live-p window)
	       (with-selected-window window
		 (split-window-vertically))
	     (split-window
	      window (- (max window-min-height 1
			     (/ split-height-threshold 2))))))
      (and (window-sensibly-splittable-p window t)
	   ;; Split window horizontally.
	   (if (window-live-p window)
	       (with-selected-window window
		 (split-window-horizontally))
	     (split-window
	      window (- (max window-min-width 2
			     (/ split-width-threshold 2)))
	      t)))
      (and (frame-root-window-p window)
	   (not (window-minibuffer-p window))
	   ;; If WINDOW is the only window on its frame and is not the
	   ;; minibuffer window, try to split it vertically disregarding
	   ;; the value of `split-height-threshold'.
	   (let ((split-height-threshold 0))
	     (when (window-sensibly-splittable-p window)
	       (with-selected-window window
		 (split-window-vertically)))))))

;; Minibuffer-only frames should be documented better.  They really
;; deserve a separate section in the manual.  Also
;; `last-nonminibuffer-frame' is nowhere documented in the manual.
(defun window--usable-frame (frame)
  "Return FRAME if it is live and not a minibuffer-only frame."
  (and (frame-live-p frame)
       (not (window-minibuffer-p (frame-root-window frame)))
       frame))

(defcustom even-window-heights t
  "If non-nil `display-buffer' will try to even window heights.
Otherwise `display-buffer' will leave the window configuration
alone.  Heights are evened only when `display-buffer' reuses a
window that appears above or below the selected window."
  :type 'boolean
  :group 'display-buffer)

(defvar display-buffer-window-and-buffer nil
  "Window used by `display-buffer' and related information.
After `display-buffer' displays a buffer in some window this
variable is a cons cell whose car denotes the window used to
display the buffer.  The cdr is either nil \(which means the same
buffer was displayed before in that window), t \(which means that
window was created by `display-buffer'), or the buffer displayed
previously in that window.

This variable holds the value produced by the last invocation of
`display-buffer' and is nil if there was no such invocation.")

(defsubst display-buffer-window-and-buffer (window &optional value)
  "Set `display-buffer-window-and-buffer' to cons of WINDOW and VALUE.
VALUE defaults to nil."
  (setq display-buffer-window-and-buffer (cons window value)))

(defun window--display-buffer-in-window (buffer window &optional dedicated)
  "Display BUFFER in WINDOW and maybe raise its frame.
If DEDICATED is non-nil, use it to set the `window-dedicated-p'
flag of WINDOW.  Return WINDOW."
  (when (and (buffer-live-p buffer) (window-live-p window))
    (set-window-buffer window buffer)
    (when dedicated
      (set-window-dedicated-p window dedicated))
    (window--raise-window-frame window)))

(defun window--reuse-window (window buffer not-this-window)
  "Reuse WINDOW for displaying BUFFER.
If NOT-THIS-WINDOW is non-nil and WINDOW is the selected window
do nothing and return nil.  Return nil also if WINDOW is
dedicated to its buffer.  Otherwise, display buffer in WINDOW,
even window heights if requested, and return WINDOW."
  (unless (or (and not-this-window (eq window (selected-window)))
	      (and (window-dedicated-p window)
		   (not (eq (window-buffer window) buffer))))
    ;; Save `quit-restore' information: It's absolutely necessary to do
    ;; this _before_ handling `even-window-heights'.  Do not overwrite
    ;; an existing value.
    (unless (window-parameter window 'quit-restore)
      (set-window-parameter
       window 'quit-restore
       (list (window-buffer window) (window-start window)
	     (window-point window) buffer (window-total-size window)
	     (selected-window))))
    ;; Handle `even-window-heights' option.
    (when (and even-window-heights
	       ;; Not needed (but often WINDOW is the selected window):
	       (not (eq window (selected-window)))
	       ;; Don't resize minibuffer windows.
	       (not (window-minibuffer-p))
	       ;; Resize iff the selected window is higher than WINDOW.
	       (> (window-total-size) (window-total-size window))
	       ;; Resize vertical combinations only.
	       (and (window-parent) (window-iso-combined-p (window-parent))
		    ;; WINDOW must be adjacent to the selected one.
		    (or (eq window (window-prev)) (eq window (window-next)))))
      ;; Don't throw an error if we can't even window heights for
      ;; whatever reason.  In any case, enlarging the selected window
      ;; might fail anyway if there are other windows above or below
      ;; WINDOW and the selected one.  But for a simple two windows
      ;; configuration the present behavior is good enough so why care?
      (condition-case nil
	  (resize-window
	   window (/ (- (window-total-size window) (window-total-size)) 2))
	(error nil)))
    (display-buffer-window-and-buffer window (window-buffer window))
    ;; Display BUFFER in WINDOW.
    (window--display-buffer-in-window buffer window)))

;; This variable should be probably documented in the manual.
(defvar display-buffer-mark-dedicated nil
  "If non-nil, `display-buffer' marks the windows it creates as dedicated.
The actual non-nil value of this variable will be copied to the
`window-dedicated-p' flag.")

(defun window--pop-up-window (buffer frame)
  "Pop up a new window for BUFFER on FRAME.
If FRAME cannot be split, try to pop up a window on the last
nonminibuffer frame instead.  Return the new window if splitting
succeeded, nil otherwise.

See the documentation of the variable `pop-up-windows' how to
choose the window to be split for popping up the new one."
  (when (or (and (window--usable-frame frame)
		 (not (frame-parameter frame 'unsplittable)))
	    (and (setq frame (last-nonminibuffer-frame))
		 (window--usable-frame frame)
		 (not (frame-parameter frame 'unsplittable))))
    (let* ((selected-window (selected-window))
	   (windows (window-list-1 (frame-first-window frame) 'nomini frame))
	   (probes (if (eq pop-up-windows t) '(largest lru) pop-up-windows))
	   window new-window)
      (catch 'done
	;; Try to find a window suiting `pop-up-windows'.
	(dolist (probe probes)
	  (setq window
		(cond
		 ((eq probe 'selected)
		  (frame-selected-window frame))
		 ((eq probe 'largest)
		  (get-largest-window frame t))
		 ((eq probe 'lru)
		  (get-lru-window frame t))
		 ((eq probe 'root)
		  (frame-root-window frame))))
	  ;; Do not consider `window' again.
	  (setq windows (remq window windows))
	  (when (and (eq (window-frame window) frame)
		     (not (window-minibuffer-p window))
		     (not (frame-parameter frame 'unsplittable))
		     (setq new-window
			   ;; Since `split-window-preferred-function' might
			   ;; throw an error use `condition-case'.
			   (condition-case nil
			       (funcall split-window-preferred-function window)
			     (error nil))))
	    (set-window-parameter
	     new-window 'quit-restore (list t selected-window))
	    (display-buffer-window-and-buffer new-window t)
	    (window--display-buffer-in-window
	     buffer new-window display-buffer-mark-dedicated)
	    (throw 'done new-window)))))))

(defun window--pop-up-frame (buffer)
  "Pop up a new frame for displaying BUFFER.
Return the window displaying BUFFER if creating the new frame was
successful, nil otherwise."
  (let* ((selected-window (selected-window))
	 (frame (funcall pop-up-frame-function))
	 (window (when frame (frame-selected-window frame))))
    (when window
      (display-buffer-window-and-buffer window t)
      (window--display-buffer-in-window
       buffer window display-buffer-mark-dedicated)
      ;; Make sure that quitting is allowed to delete the frame.
      (set-window-parameter
       window 'quit-restore (list t selected-window))
      window)))

(defun window--raise-window-frame (&optional window)
  "Raise the frame containing WINDOW.
WINDOW must be a live window and defaults to the selected one.
Return WINDOW.

This function does not raise the selected or an invisible frame."
  (setq window (normalize-live-window window))
  (let* ((frame (window-frame window))
	 (visible (frame-visible-p frame)))
    (unless (or (not visible)
		;; Assume the selected frame is already visible enough.
		(eq frame (selected-frame))
		;; Assume the frame from which we invoked the minibuffer
		;; is visible.
		(and (minibuffer-window-active-p (selected-window))
		     (eq frame (window-frame (minibuffer-selected-window)))))
      (raise-frame frame))
    window))

(defun display-buffer (buffer-or-name &optional not-this-window frame)
  "Make buffer BUFFER-OR-NAME appear in some window but don't select it.
BUFFER-OR-NAME must be a buffer or the name of an existing
buffer.  Return the window chosen to display BUFFER-OR-NAME or
nil if no such window is found.

Optional argument NOT-THIS-WINDOW non-nil means display the
buffer in a window other than the selected one, even if it is
already displayed in the selected window.

Optional argument FRAME specifies which frames to investigate
when the specified buffer is already displayed.  If the buffer is
already displayed in some window on one of these frames simply
return that window.  Possible values of FRAME are:

`visible' - consider windows on all visible frames.

0 - consider windows on all visible or iconified frames.

t - consider windows on all frames.

A specific frame - consider windows on that frame only.

nil - consider windows on the selected frame \(actually the
last non-minibuffer frame\) only.  If, however, either
`display-buffer-reuse-frames' or `pop-up-frames' is non-nil
\(non-nil and not graphic-only on a text-only terminal),
consider all visible or iconified frames."
  (interactive "BDisplay buffer:\nP")
  (let* ((buffer (normalize-live-buffer buffer-or-name))
	 (buffer-name (buffer-name buffer))
	 (can-use-selected-window
	  ;; We can reuse the selected window unless NOT-THIS-WINDOW is
	  ;; non-nil, or the selected window is either dedicated to its
	  ;; buffer, or it is a `minibuffer-window'.
	  (not (or not-this-window
		   (window-dedicated-p)
		   (window-minibuffer-p))))
	 ;; On text-only terminals do not pop up a new frame when
	 ;; `pop-up-frames' equals graphic-only.
	 (pop-up-frame (if (eq pop-up-frames 'graphic-only)
				(display-graphic-p)
			      pop-up-frames))
	 ;; `frame-to-use' is the frame where to show `buffer' - either
	 ;; the selected frame or the last nonminibuffer frame.
	 (frame-to-use
	  (or (window--usable-frame (selected-frame))
	      (window--usable-frame (last-nonminibuffer-frame))))
	 ;; `window-to-use' is the window we use for showing `buffer'.
	 window-to-use)
    (cond
     (display-buffer-function
      ;; Let `display-buffer-function' do the job.
      (funcall display-buffer-function buffer not-this-window))
     ((and (not not-this-window) (eq (window-buffer) buffer))
      ;; The selected window already displays BUFFER and NOT-THIS-WINDOW
      ;; is nil, so reuse the selected window.
      (selected-window))
     ((and can-use-selected-window (same-window-p buffer-name))
      ;; If the buffer's name tells us to use the selected window do so.
      (display-buffer-window-and-buffer (selected-window))
      (window--display-buffer-in-window buffer (selected-window)))
     ((let ((frames (or frame
			(and (or pop-up-frame
				 display-buffer-reuse-frames
				 (not (last-nonminibuffer-frame)))
			     0)
			(last-nonminibuffer-frame))))
	(setq window-to-use
	      (catch 'found
		;; Search frames for a window displaying BUFFER.  Return
		;; the selected window only if we are allowed to do so.
		(dolist (window (get-buffer-window-list buffer 'nomini frames))
		  (when (or can-use-selected-window
			    (not (eq (selected-window) window)))
		    (throw 'found window))))))
      ;; The buffer is already displayed in some window; use that.
      (display-buffer-window-and-buffer window-to-use)
      (window--raise-window-frame window-to-use))
     ((and special-display-function
	   ;; `special-display-p' returns either t or a list of frame
	   ;; parameters to pass to `special-display-function'.
	   (let ((pars (special-display-p buffer-name)))
	     (when pars
	       (funcall
		special-display-function buffer (and (listp pars) pars))))))
     ((and (or pop-up-frame (not frame-to-use))
	   (window--pop-up-frame buffer)))
     ((and pop-up-windows
	   ;; Try popping up a new window.
	   (window--pop-up-window buffer frame-to-use)))
     ((window--reuse-window
       ;; Try reusing least recently used window.
       (get-lru-window frame-to-use) buffer not-this-window))
     ((window--reuse-window
       ;; Try reusing some visible window showing BUFFER.
       (get-buffer-window buffer 'visible) buffer not-this-window))
     ((window--reuse-window
       ;; Try reusing largest visible window.
       (get-largest-window 'visible) buffer not-this-window))
     ((window--reuse-window
       ;; Try reusing some window showing BUFFER on any visible or
       ;; iconified frame.
       (get-buffer-window buffer 0) buffer not-this-window))
     ((window--reuse-window
       ;; Try reusing largest window on any visible or iconified frame.
       (get-largest-window 0) buffer not-this-window))
     ;; As a last resort try popping up a new frame.
     ((window--pop-up-frame buffer)))))

(defun pop-to-buffer (buffer-or-name &optional other-window norecord)
  "Select buffer BUFFER-OR-NAME in some window, preferably a different one.
BUFFER-OR-NAME may be a buffer, a string \(a buffer name), or
nil.  If BUFFER-OR-NAME is a string not naming an existent
buffer, create a buffer with that name.  If BUFFER-OR-NAME is
nil, choose some other buffer.  Return the buffer specified by
BUFFER-OR-NAME.

If optional second arg OTHER-WINDOW is non-nil, insist on finding
another window even if the specified buffer is already visible in
the selected window, and ignore the options `same-window-regexps'
and `same-window-buffer-names'.

Optional argument NORECORD non-nil means do not put the buffer
specified by BUFFER-OR-NAME at the front of the buffer list and
do not make the window displaying it the most recently selected
one.

This uses the function `display-buffer' as a subroutine; see the
documentation of `display-buffer' for additional customization
information."
  (let ((buffer
         ;; FIXME: This behavior is carried over from the previous C
         ;; version of pop-to-buffer, but really we should use just
         ;; `get-buffer' here.
         (if (null buffer-or-name)
	     (other-buffer (current-buffer))
           (or (get-buffer buffer-or-name)
               (let ((buf (get-buffer-create buffer-or-name)))
                 (set-buffer-major-mode buf)
                 buf))))
	(old-frame (selected-frame))
	new-window new-frame)
    (set-buffer buffer)
    (setq new-window (display-buffer buffer other-window))
    ;; Select the window chosen.
    (select-window new-window norecord)
    (setq new-frame (window-frame new-window))
    (unless (eq new-frame old-frame)
      ;; `display-buffer' has chosen another frame, make sure it gets
      ;; input focus and is risen.
      (select-frame-set-input-focus new-frame))
    buffer))

(defun read-buffer-to-switch (prompt)
  "Read the name of a buffer to switch to and return as a string.
It is intended for `switch-to-buffer' family of commands since they
need to omit the name of current buffer from the list of completions
and default values."
  (let ((rbts-completion-table (internal-complete-buffer-except)))
    (minibuffer-with-setup-hook
        (lambda ()
          (setq minibuffer-completion-table rbts-completion-table)
          ;; Since rbts-completion-table is built dynamically, we
          ;; can't just add it to the default value of
          ;; icomplete-with-completion-tables, so we add it
          ;; here manually.
          (if (and (boundp 'icomplete-with-completion-tables)
                   (listp icomplete-with-completion-tables))
              (set (make-local-variable 'icomplete-with-completion-tables)
                   (cons rbts-completion-table
                         icomplete-with-completion-tables))))
      (read-buffer prompt (other-buffer (current-buffer))
                   (confirm-nonexistent-file-or-buffer)))))

(defun switch-to-buffer (buffer-or-name &optional norecord)
  "Switch to buffer BUFFER-OR-NAME in the selected window.
If BUFFER-OR-NAME does not identify an existing buffer, then this
function creates a buffer with that name.

If called interactively, prompt for the buffer name using the
minibuffer.  The variable `confirm-nonexistent-file-or-buffer'
determines whether to request confirmation before creating a new
buffer.

When called from Lisp, BUFFER-OR-NAME may be a buffer, a string
\(a buffer name), or nil.  If BUFFER-OR-NAME is nil, then this
function chooses a buffer using `other-buffer'.

Optional argument NORECORD non-nil means do not put the buffer
specified by BUFFER-OR-NAME at the front of the buffer list and
do not make the window displaying it the most recently selected
one.

WARNING: Do NOT use this function to work on another buffer
temporarily within a Lisp program!  Use `with-current-buffer'
instead.  That avoids messing with the window-buffer
correspondences."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer: ")))
  (let ((buffer (when buffer-or-name (get-buffer buffer-or-name))))
    (cond
     ((eq buffer (window-buffer))
      (unless norecord
	(select-window (selected-window)))
      (set-buffer buffer))
     ((or (window-minibuffer-p) (eq (window-dedicated-p) t))
      ;; Cannot switch to another buffer in a minibuffer or strongly
      ;; dedicated window.  Call `pop-to-buffer' instead.
      (pop-to-buffer buffer nil norecord))
     (t
      (unless buffer
	(if buffer-or-name
	    ;; Create a buffer named BUFFER-OR-NAME.
	    (progn
	      (setq buffer (get-buffer-create buffer-or-name))
	      (set-buffer-major-mode buffer))
	  ;; Use other buffer.
	  (setq buffer (other-buffer (current-buffer)))))
      (set-window-buffer nil buffer)
      (unless norecord
	(select-window (selected-window)))
      (set-buffer buffer)))))

(defun switch-to-buffer-other-window (buffer-or-name &optional norecord)
  "Switch to buffer BUFFER-OR-NAME in another window.
BUFFER-OR-NAME may be a buffer, a string \(a buffer name), or
nil.  Return the buffer switched to.

If called interactively, prompt for the buffer name using the
minibuffer.  The variable `confirm-nonexistent-file-or-buffer'
determines whether to request confirmation before creating a new
buffer.

If BUFFER-OR-NAME is a string and does not identify an existing
buffer, create a new buffer with that name.  If BUFFER-OR-NAME is
nil, switch to the buffer returned by `other-buffer'.

Optional argument NORECORD non-nil means do not put the buffer
specified by BUFFER-OR-NAME at the front of the buffer list and
do not make the window displaying it the most recently selected
one.

This uses the function `display-buffer' as a subroutine; see its
documentation for additional customization information."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in other window: ")))
  (let ((pop-up-windows (or pop-up-windows t))
	same-window-buffer-names same-window-regexps)
    (pop-to-buffer buffer-or-name t norecord)))

(defun switch-to-buffer-other-frame (buffer-or-name &optional norecord)
  "Switch to buffer BUFFER-OR-NAME in another frame.
BUFFER-OR-NAME may be a buffer, a string \(a buffer name), or
nil.  Return the buffer switched to.

If called interactively, prompt for the buffer name using the
minibuffer.  The variable `confirm-nonexistent-file-or-buffer'
determines whether to request confirmation before creating a new
buffer.

If BUFFER-OR-NAME is a string and does not identify an existing
buffer, create a new buffer with that name.  If BUFFER-OR-NAME is
nil, switch to the buffer returned by `other-buffer'.

Optional argument NORECORD non-nil means do not put the buffer
specified by BUFFER-OR-NAME at the front of the buffer list and
do not make the window displaying it the most recently selected
one.

This uses the function `display-buffer' as a subroutine; see its
documentation for additional customization information."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in other frame: ")))
  (let ((pop-up-frames t)
	same-window-buffer-names same-window-regexps)
    (pop-to-buffer buffer-or-name t norecord)))

(defun set-window-text-height (window height)
  "Set the height in lines of the text display area of WINDOW to HEIGHT.
WINDOW must be a live window.  HEIGHT doesn't include the mode
line or header line, if any, or any partial-height lines in the
text display area.

Note that the current implementation of this function cannot
always set the height exactly, but attempts to be conservative,
by allocating more lines than are actually needed in the case
where some error may be present."
  (setq window (normalize-live-window window))
  (let ((delta (- height (window-text-height window))))
    (unless (zerop delta)
      ;; Setting window-min-height to a value like 1 can lead to very
      ;; bizarre displays because it also allows Emacs to make *other*
      ;; windows 1-line tall, which means that there's no more space for
      ;; the modeline.
      (let ((window-min-height (min 2 height))) ; One text line plus a modeline.
	(resize-window window delta)))))

(defun enlarge-window-horizontally (delta)
  "Make selected window DELTA wider.
Interactively, if no argument is given, make selected window one
column wider."
  (interactive "p")
  (enlarge-window delta t))

(defun shrink-window-horizontally (delta)
  "Make selected window DELTA narrower.
Interactively, if no argument is given, make selected window one
column narrower."
  (interactive "p")
  (shrink-window delta t))

(defun count-screen-lines (&optional beg end count-final-newline window)
  "Return the number of screen lines in the region.
The number of screen lines may be different from the number of actual lines,
due to line breaking, display table, etc.

Optional arguments BEG and END default to `point-min' and `point-max'
respectively.

If region ends with a newline, ignore it unless optional third argument
COUNT-FINAL-NEWLINE is non-nil.

The optional fourth argument WINDOW specifies the window used for obtaining
parameters such as width, horizontal scrolling, and so on.  The default is
to use the selected window's parameters.

Like `vertical-motion', `count-screen-lines' always uses the current buffer,
regardless of which buffer is displayed in WINDOW.  This makes possible to use
`count-screen-lines' in any buffer, whether or not it is currently displayed
in some window."
  (unless beg
    (setq beg (point-min)))
  (unless end
    (setq end (point-max)))
  (if (= beg end)
      0
    (save-excursion
      (save-restriction
        (widen)
        (narrow-to-region (min beg end)
                          (if (and (not count-final-newline)
                                   (= ?\n (char-before (max beg end))))
                              (1- (max beg end))
                            (max beg end)))
        (goto-char (point-min))
        (1+ (vertical-motion (buffer-size) window))))))

(defun window-buffer-height (window)
  "Return the height (in screen lines) of the buffer that WINDOW is displaying."
  (with-current-buffer (window-buffer window)
    (max 1
	 (count-screen-lines (point-min) (point-max)
			     ;; If buffer ends with a newline, ignore it when
			     ;; counting height unless point is after it.
			     (eobp)
			     window))))

;;; Resizing buffers to fit their contents exactly.
(defun fit-window-to-buffer (&optional window max-height min-height ignore)
  "Adjust height of WINDOW to display its buffer's contents exactly.
WINDOW can be any live window and defaults to the selected one.

Optional argument MAX-HEIGHT specifies the maximum height of
WINDOW and defaults to the height of WINDOW's frame.  Optional
argument MIN-HEIGHT specifies the minimum height of WINDOW and
defaults to `window-min-height'.  Both, MAX-HEIGHT and MIN-HEIGHT
are specified in lines and include the mode line and header line,
if any.

Optional argument IGNORE non-nil means ignore restrictions
imposed by `window-min-height' and `window-min-width' on the size
of WINDOW.

Return the number of lines by which WINDOW was enlarged or
shrunk.  If an error occurs during resizing, return nil but don't
signal an error.

Note that even if this function makes WINDOW large enough to show
_all_ lines of its buffer you might not see the first lines when
WINDOW was scrolled."
  (interactive)
  ;; Do all the work in WINDOW and its buffer and restore the selected
  ;; window and the current buffer when we're done.
  (setq window (normalize-live-window window))
  ;; Can't resize a full height or fixed-size window.
  (unless (or (window-size-fixed-p window) 
	      (window-full-height-p window))
    ;; `with-selected-window' should orderly restore the current buffer.
    (with-selected-window window
      ;; We are in WINDOW's buffer now.
      (let* ( ;; Adjust MIN-HEIGHT.
	     (min-height
	      (if ignore
		  (window-min-size window nil window)
		(max (or min-height window-min-height)
		     window-safe-min-height)))
	     (max-window-height
	      (window-total-size (frame-root-window window)))
	     ;; Adjust MAX-HEIGHT.
	     (max-height
	      (if (or ignore (not max-height))
		  max-window-height
		(min max-height max-window-height)))
	     ;; Make `desired-height' the height necessary to show
	     ;; all of WINDOW's buffer, constrained by MIN-HEIGHT
	     ;; and MAX-HEIGHT.
	     (desired-height
	      (max
	       (min
		(+ (count-screen-lines)
		   ;; For non-minibuffers count the mode line, if any.
		   (if (and (not (window-minibuffer-p window))
			    mode-line-format)
		       1
		     0)
		   ;; Count the header line, if any.
		   (if header-line-format 1 0))
		max-height)
	       min-height))
	     (desired-delta
	      (- desired-height (window-total-size window)))
	     (delta
	      (if (> desired-delta 0)
		  (min desired-delta
		       (window-max-delta window nil window))
		(max desired-delta
		     (- (window-min-delta window nil window))))))
	;; This `condition-case' shouldn't be necessary, but who knows?
	(condition-case nil
	    (if (zerop delta)
		;; Return zero if DELTA became zero in the proces.
		0
	      ;; Don't try to redisplay with the cursor at the end on its
	      ;; own line--that would force a scroll and spoil things.
	      (when (and (eobp) (bolp) (not (bobp)))
		;; It's silly to put `point' at the end of the previous
		;; line and so maybe force horizontal scrolling.
		(set-window-point window (line-beginning-position 0)))
	      ;; Call `resize-window' with IGNORE argument equal WINDOW.
	      (resize-window window delta nil window)
	      ;; Check if the last line is surely fully visible.  If
	      ;; not, enlarge the window.
	      (let ((end (save-excursion
			   (goto-char (point-max))
			   (when (and (bolp) (not (bobp)))
			     ;; Don't include final newline.
			     (backward-char 1))
			   (when truncate-lines
			     ;; If line-wrapping is turned off, test the
			     ;; beginning of the last line for
			     ;; visibility instead of the end, as the
			     ;; end of the line could be invisible by
			     ;; virtue of extending past the edge of the
			     ;; window.
			     (forward-line 0))
			   (point))))
		(set-window-vscroll window 0)
		;; This loop might in some rare pathological cases raise
		;; an error - another reason for the `condition-case'.
		(while (and (< desired-height max-height)
			    (= desired-height (window-total-size))
			    (not (pos-visible-in-window-p end)))
		  (resize-window window 1 nil window)
		  (setq desired-height (1+ desired-height)))))
	  (error (setq delta nil)))
	delta))))

(defun window-safely-shrinkable-p (&optional window)
  "Return t if WINDOW can be shrunk without shrinking other windows.
WINDOW defaults to the selected window."
  (with-selected-window (or window (selected-window))
    (let ((edges (window-edges)))
      (or (= (nth 2 edges) (nth 2 (window-edges (previous-window))))
	  (= (nth 0 edges) (nth 0 (window-edges (next-window))))))))

(defun shrink-window-if-larger-than-buffer (&optional window)
  "Shrink height of WINDOW if its buffer doesn't need so many lines.
More precisely, shrink WINDOW vertically to be as small as
possible, while still showing the full contents of its buffer.
WINDOW defaults to the selected window.

Do not shrink WINDOW to less than `window-min-height' lines.  Do
nothing if the buffer contains more lines than the present window
height, or if some of the window's contents are scrolled out of
view, or if shrinking this window would also shrink another
window, or if the window is the only window of its frame.

Return non-nil if the window was shrunk, nil otherwise."
  (interactive)
  (setq window (normalize-live-window window))
  ;; Make sure that WINDOW is vertically combined and `point-min' is
  ;; visible (for whatever reason that's needed).  The remaining issues
  ;; should be taken care of by `fit-window-to-buffer'.
  (when (and (window-iso-combined-p window)
	     (pos-visible-in-window-p (point-min) window))
    (fit-window-to-buffer window (window-total-size window))))

(defun kill-buffer-and-window ()
  "Kill the current buffer and delete the selected window."
  (interactive)
  (let ((window-to-delete (selected-window))
	(buffer-to-kill (current-buffer))
	(delete-window-hook (lambda ()
			      (condition-case nil
				  (delete-window)
				(error nil)))))
    (unwind-protect
	(progn
	  (add-hook 'kill-buffer-hook delete-window-hook t t)
	  (if (kill-buffer (current-buffer))
	      ;; If `delete-window' failed before, we rerun it to regenerate
	      ;; the error so it can be seen in the echo area.
	      (when (eq (selected-window) window-to-delete)
		(delete-window))))
      ;; If the buffer is not dead for some reason (probably because
      ;; of a `quit' signal), remove the hook again.
      (condition-case nil
	  (with-current-buffer buffer-to-kill
	    (remove-hook 'kill-buffer-hook delete-window-hook t))
	(error nil)))))

(defun quit-window (&optional kill window)
  "Quit WINDOW and bury its buffer.
With a prefix argument, kill the buffer instead.  WINDOW defaults
to the selected window.

If WINDOW is non-nil, dedicated, or a minibuffer window, delete
it and, if it's alone on its frame, its frame too.  Otherwise, or
if deleting WINDOW fails in any of the preceding cases, display
another buffer in WINDOW using `switch-to-buffer'.

Optional argument KILL non-nil means kill WINDOW's buffer.
Otherwise, bury WINDOW's buffer, see `bury-buffer'."
  (interactive "P")
  (let ((buffer (window-buffer window)))
    (if (or window
	    (window-minibuffer-p window)
	    (window-dedicated-p window))
	;; WINDOW is either non-nil, a minibuffer window, or dedicated;
	;; try to delete it.
	(let* ((window (or window (selected-window)))
	       (frame (window-frame window)))
	  (if (frame-root-window-p window)
	      ;; WINDOW is alone on its frame.
	      (delete-frame frame)
	    ;; There are other windows on its frame, delete WINDOW.
	    (delete-window window)))
      ;; Otherwise, switch to another buffer in the selected window.
      (switch-to-buffer nil))

    ;; Deal with the buffer.
    (if kill
	(kill-buffer buffer)
      (bury-buffer buffer))))

(defvar recenter-last-op nil
  "Indicates the last recenter operation performed.
Possible values: `top', `middle', `bottom', integer or float numbers.")

(defcustom recenter-positions '(middle top bottom)
  "Cycling order for `recenter-top-bottom'.
A list of elements with possible values `top', `middle', `bottom',
integer or float numbers that define the cycling order for
the command `recenter-top-bottom'.

Top and bottom destinations are `scroll-margin' lines the from true
window top and bottom.  Middle redraws the frame and centers point
vertically within the window.  Integer number moves current line to
the specified absolute window-line.  Float number between 0.0 and 1.0
means the percentage of the screen space from the top.  The default
cycling order is middle -> top -> bottom."
  :type '(repeat (choice
		  (const :tag "Top" top)
		  (const :tag "Middle" middle)
		  (const :tag "Bottom" bottom)
		  (integer :tag "Line number")
		  (float :tag "Percentage")))
  :version "23.2"
  :group 'windows)

(defun recenter-top-bottom (&optional arg)
  "Move current buffer line to the specified window line.
With no prefix argument, successive calls place point according
to the cycling order defined by `recenter-positions'.

A prefix argument is handled like `recenter':
 With numeric prefix ARG, move current line to window-line ARG.
 With plain `C-u', move current line to window center."
  (interactive "P")
  (cond
   (arg (recenter arg))			; Always respect ARG.
   (t
    (setq recenter-last-op
	  (if (eq this-command last-command)
	      (car (or (cdr (member recenter-last-op recenter-positions))
		       recenter-positions))
	    (car recenter-positions)))
    (let ((this-scroll-margin
	   (min (max 0 scroll-margin)
		(truncate (/ (window-body-height) 4.0)))))
      (cond ((eq recenter-last-op 'middle)
	     (recenter))
	    ((eq recenter-last-op 'top)
	     (recenter this-scroll-margin))
	    ((eq recenter-last-op 'bottom)
	     (recenter (- -1 this-scroll-margin)))
	    ((integerp recenter-last-op)
	     (recenter recenter-last-op))
	    ((floatp recenter-last-op)
	     (recenter (round (* recenter-last-op (window-height))))))))))

(define-key global-map [?\C-l] 'recenter-top-bottom)

(defun move-to-window-line-top-bottom (&optional arg)
  "Position point relative to window.

With a prefix argument ARG, acts like `move-to-window-line'.

With no argument, positions point at center of window.
Successive calls position point at positions defined
by `recenter-positions'."
  (interactive "P")
  (cond
   (arg (move-to-window-line arg))	; Always respect ARG.
   (t
    (setq recenter-last-op
	  (if (eq this-command last-command)
	      (car (or (cdr (member recenter-last-op recenter-positions))
		       recenter-positions))
	    (car recenter-positions)))
    (let ((this-scroll-margin
	   (min (max 0 scroll-margin)
		(truncate (/ (window-body-height) 4.0)))))
      (cond ((eq recenter-last-op 'middle)
	     (call-interactively 'move-to-window-line))
	    ((eq recenter-last-op 'top)
	     (move-to-window-line this-scroll-margin))
	    ((eq recenter-last-op 'bottom)
	     (move-to-window-line (- -1 this-scroll-margin)))
	    ((integerp recenter-last-op)
	     (move-to-window-line recenter-last-op))
	    ((floatp recenter-last-op)
	     (move-to-window-line (round (* recenter-last-op (window-height))))))))))

(define-key global-map [?\M-r] 'move-to-window-line-top-bottom)

;;; Scrolling commands.

;;; Scrolling commands which does not signal errors at top/bottom
;;; of buffer at first key-press (instead moves to top/bottom
;;; of buffer).

(defcustom scroll-error-top-bottom nil
  "Move point to top/bottom of buffer before signalling a scrolling error.
A value of nil means just signal an error if no more scrolling possible.
A value of t means point moves to the beginning or the end of the buffer
\(depending on scrolling direction) when no more scrolling possible.
When point is already on that position, then signal an error."
  :type 'boolean
  :group 'scrolling
  :version "24.1")

(defun scroll-up-command (&optional arg)
  "Scroll text of selected window upward ARG lines; or near full screen if no ARG.
If `scroll-error-top-bottom' is non-nil and `scroll-up' cannot
scroll window further, move cursor to the bottom line.
When point is already on that position, then signal an error.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll downward.
If ARG is the atom `-', scroll downward by nearly full screen."
  (interactive "^P")
  (cond
   ((null scroll-error-top-bottom)
    (scroll-up arg))
   ((eq arg '-)
    (scroll-down-command nil))
   ((< (prefix-numeric-value arg) 0)
    (scroll-down-command (- (prefix-numeric-value arg))))
   ((eobp)
    (scroll-up arg))			; signal error
   (t
    (condition-case nil
	(scroll-up arg)
      (end-of-buffer
       (if arg
	   ;; When scrolling by ARG lines can't be done,
	   ;; move by ARG lines instead.
	   (forward-line arg)
	 ;; When ARG is nil for full-screen scrolling,
	 ;; move to the bottom of the buffer.
	 (goto-char (point-max))))))))

(put 'scroll-up-command 'scroll-command t)

(defun scroll-down-command (&optional arg)
  "Scroll text of selected window down ARG lines; or near full screen if no ARG.
If `scroll-error-top-bottom' is non-nil and `scroll-down' cannot
scroll window further, move cursor to the top line.
When point is already on that position, then signal an error.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll upward.
If ARG is the atom `-', scroll upward by nearly full screen."
  (interactive "^P")
  (cond
   ((null scroll-error-top-bottom)
    (scroll-down arg))
   ((eq arg '-)
    (scroll-up-command nil))
   ((< (prefix-numeric-value arg) 0)
    (scroll-up-command (- (prefix-numeric-value arg))))
   ((bobp)
    (scroll-down arg))			; signal error
   (t
    (condition-case nil
	(scroll-down arg)
      (beginning-of-buffer
       (if arg
	   ;; When scrolling by ARG lines can't be done,
	   ;; move by ARG lines instead.
	   (forward-line (- arg))
	 ;; When ARG is nil for full-screen scrolling,
	 ;; move to the top of the buffer.
	 (goto-char (point-min))))))))

(put 'scroll-down-command 'scroll-command t)

;;; Scrolling commands which scroll a line instead of full screen.

(defun scroll-up-line (&optional arg)
  "Scroll text of selected window upward ARG lines; or one line if no ARG.
If ARG is omitted or nil, scroll upward by one line.
This is different from `scroll-up-command' that scrolls a full screen."
  (interactive "p")
  (scroll-up (or arg 1)))

(put 'scroll-up-line 'scroll-command t)

(defun scroll-down-line (&optional arg)
  "Scroll text of selected window down ARG lines; or one line if no ARG.
If ARG is omitted or nil, scroll down by one line.
This is different from `scroll-down-command' that scrolls a full screen."
  (interactive "p")
  (scroll-down (or arg 1)))

(put 'scroll-down-line 'scroll-command t)


(defun scroll-other-window-down (lines)
  "Scroll the \"other window\" down.
For more details, see the documentation for `scroll-other-window'."
  (interactive "P")
  (scroll-other-window
   ;; Just invert the argument's meaning.
   ;; We can do that without knowing which window it will be.
   (if (eq lines '-) nil
     (if (null lines) '-
       (- (prefix-numeric-value lines))))))

(defun beginning-of-buffer-other-window (arg)
  "Move point to the beginning of the buffer in the other window.
Leave mark at previous position.
With arg N, put point N/10 of the way from the true beginning."
  (interactive "P")
  (let ((orig-window (selected-window))
	(window (other-window-for-scrolling)))
    ;; We use unwind-protect rather than save-window-excursion
    ;; because the latter would preserve the things we want to change.
    (unwind-protect
	(progn
	  (select-window window)
	  ;; Set point and mark in that window's buffer.
	  (with-no-warnings
	   (beginning-of-buffer arg))
	  ;; Set point accordingly.
	  (recenter '(t)))
      (select-window orig-window))))

(defun end-of-buffer-other-window (arg)
  "Move point to the end of the buffer in the other window.
Leave mark at previous position.
With arg N, put point N/10 of the way from the true end."
  (interactive "P")
  ;; See beginning-of-buffer-other-window for comments.
  (let ((orig-window (selected-window))
	(window (other-window-for-scrolling)))
    (unwind-protect
	(progn
	  (select-window window)
	  (with-no-warnings
	   (end-of-buffer arg))
	  (recenter '(t)))
      (select-window orig-window))))

(defvar mouse-autoselect-window-timer nil
  "Timer used by delayed window autoselection.")

(defvar mouse-autoselect-window-position nil
  "Last mouse position recorded by delayed window autoselection.")

(defvar mouse-autoselect-window-window nil
  "Last window recorded by delayed window autoselection.")

(defvar mouse-autoselect-window-state nil
  "When non-nil, special state of delayed window autoselection.
Possible values are `suspend' \(suspend autoselection after a menu or
scrollbar interaction\) and `select' \(the next invocation of
'handle-select-window' shall select the window immediately\).")

(defun mouse-autoselect-window-cancel (&optional force)
  "Cancel delayed window autoselection.
Optional argument FORCE means cancel unconditionally."
  (unless (and (not force)
	       ;; Don't cancel for select-window or select-frame events
	       ;; or when the user drags a scroll bar.
	       (or (memq this-command
			 '(handle-select-window handle-switch-frame))
		   (and (eq this-command 'scroll-bar-toolkit-scroll)
			(memq (nth 4 (event-end last-input-event))
			      '(handle end-scroll)))))
    (setq mouse-autoselect-window-state nil)
    (when (timerp mouse-autoselect-window-timer)
      (cancel-timer mouse-autoselect-window-timer))
    (remove-hook 'pre-command-hook 'mouse-autoselect-window-cancel)))

(defun mouse-autoselect-window-start (mouse-position &optional window suspend)
  "Start delayed window autoselection.
MOUSE-POSITION is the last position where the mouse was seen as returned
by `mouse-position'.  Optional argument WINDOW non-nil denotes the
window where the mouse was seen.  Optional argument SUSPEND non-nil
means suspend autoselection."
  ;; Record values for MOUSE-POSITION, WINDOW, and SUSPEND.
  (setq mouse-autoselect-window-position mouse-position)
  (when window (setq mouse-autoselect-window-window window))
  (setq mouse-autoselect-window-state (when suspend 'suspend))
  ;; Install timer which runs `mouse-autoselect-window-select' after
  ;; `mouse-autoselect-window' seconds.
  (setq mouse-autoselect-window-timer
	(run-at-time
	 (abs mouse-autoselect-window) nil 'mouse-autoselect-window-select)))

(defun mouse-autoselect-window-select ()
  "Select window with delayed window autoselection.
If the mouse position has stabilized in a non-selected window, select
that window.  The minibuffer window is selected only if the minibuffer is
active.  This function is run by `mouse-autoselect-window-timer'."
  (condition-case nil
      (let* ((mouse-position (mouse-position))
	     (window
	      (condition-case nil
		  (window-at (cadr mouse-position) (cddr mouse-position)
			     (car mouse-position))
		(error nil))))
	(cond
	 ((or (menu-or-popup-active-p)
	      (and window
		   (not (coordinates-in-window-p (cdr mouse-position) window))))
	  ;; A menu / popup dialog is active or the mouse is on the scroll-bar
	  ;; of WINDOW, temporarily suspend delayed autoselection.
	  (mouse-autoselect-window-start mouse-position nil t))
	 ((eq mouse-autoselect-window-state 'suspend)
	  ;; Delayed autoselection was temporarily suspended, reenable it.
	  (mouse-autoselect-window-start mouse-position))
	 ((and window (not (eq window (selected-window)))
	       (or (not (numberp mouse-autoselect-window))
		   (and (> mouse-autoselect-window 0)
			;; If `mouse-autoselect-window' is positive, select
			;; window if the window is the same as before.
			(eq window mouse-autoselect-window-window))
		   ;; Otherwise select window if the mouse is at the same
		   ;; position as before.  Observe that the first test after
		   ;; starting autoselection usually fails since the value of
		   ;; `mouse-autoselect-window-position' recorded there is the
		   ;; position where the mouse has entered the new window and
		   ;; not necessarily where the mouse has stopped moving.
		   (equal mouse-position mouse-autoselect-window-position))
	       ;; The minibuffer is a candidate window if it's active.
	       (or (not (window-minibuffer-p window))
		   (eq window (active-minibuffer-window))))
	  ;; Mouse position has stabilized in non-selected window: Cancel
	  ;; delayed autoselection and try to select that window.
	  (mouse-autoselect-window-cancel t)
	  ;; Select window where mouse appears unless the selected window is the
	  ;; minibuffer.  Use `unread-command-events' in order to execute pre-
	  ;; and post-command hooks and trigger idle timers.  To avoid delaying
	  ;; autoselection again, set `mouse-autoselect-window-state'."
	  (unless (window-minibuffer-p (selected-window))
	    (setq mouse-autoselect-window-state 'select)
	    (setq unread-command-events
		  (cons (list 'select-window (list window))
			unread-command-events))))
	 ((or (and window (eq window (selected-window)))
	      (not (numberp mouse-autoselect-window))
	      (equal mouse-position mouse-autoselect-window-position))
	  ;; Mouse position has either stabilized in the selected window or at
	  ;; `mouse-autoselect-window-position': Cancel delayed autoselection.
	  (mouse-autoselect-window-cancel t))
	 (t
	  ;; Mouse position has not stabilized yet, resume delayed
	  ;; autoselection.
	  (mouse-autoselect-window-start mouse-position window))))
    (error nil)))

(defun handle-select-window (event)
  "Handle select-window events."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (unless (or (not (window-live-p window))
		;; Don't switch if we're currently in the minibuffer.
		;; This tries to work around problems where the
		;; minibuffer gets unselected unexpectedly, and where
		;; you then have to move your mouse all the way down to
		;; the minibuffer to select it.
		(window-minibuffer-p (selected-window))
		;; Don't switch to minibuffer window unless it's active.
		(and (window-minibuffer-p window)
		     (not (minibuffer-window-active-p window)))
		;; Don't switch when autoselection shall be delayed.
		(and (numberp mouse-autoselect-window)
		     (not (zerop mouse-autoselect-window))
		     (not (eq mouse-autoselect-window-state 'select))
		     (progn
		       ;; Cancel any delayed autoselection.
		       (mouse-autoselect-window-cancel t)
		       ;; Start delayed autoselection from current mouse
		       ;; position and window.
		       (mouse-autoselect-window-start (mouse-position) window)
		       ;; Executing a command cancels delayed autoselection.
		       (add-hook
			'pre-command-hook 'mouse-autoselect-window-cancel))))
      (when mouse-autoselect-window
	;; Reset state of delayed autoselection.
	(setq mouse-autoselect-window-state nil)
	;; Run `mouse-leave-buffer-hook' when autoselecting window.
	(run-hooks 'mouse-leave-buffer-hook))
      (select-window window))))

(defun truncated-partial-width-window-p (&optional window)
  "Return non-nil if lines in WINDOW are specifically truncated due to its width.
WINDOW defaults to the selected window.
Return nil if WINDOW is not a partial-width window
 (regardless of the value of `truncate-lines').
Otherwise, consult the value of `truncate-partial-width-windows'
 for the buffer shown in WINDOW."
  (unless window
    (setq window (selected-window)))
  (unless (window-full-width-p window)
    (let ((t-p-w-w (buffer-local-value 'truncate-partial-width-windows
				       (window-buffer window))))
      (if (integerp t-p-w-w)
	  (< (window-width window) t-p-w-w)
	t-p-w-w))))

(define-key ctl-x-map "0" 'delete-window)
(define-key ctl-x-map "1" 'delete-other-windows)
(define-key ctl-x-map "2" 'split-window-vertically)
(define-key ctl-x-map "3" 'split-window-horizontally)
(define-key ctl-x-map "9" 'maximize-window)
(define-key ctl-x-map "o" 'other-window)
(define-key ctl-x-map "^" 'enlarge-window)
(define-key ctl-x-map "}" 'enlarge-window-horizontally)
(define-key ctl-x-map "{" 'shrink-window-horizontally)
(define-key ctl-x-map "-" 'shrink-window-if-larger-than-buffer)
(define-key ctl-x-map "+" 'balance-windows)
(define-key ctl-x-4-map "0" 'kill-buffer-and-window)

;; arch-tag: b508dfcc-c353-4c37-89fa-e773fe10cea9
;;; window.el ends here
