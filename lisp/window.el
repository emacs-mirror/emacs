;;; window.el --- GNU Emacs window commands aside from those written in C

;; Copyright (C) 1985, 1989, 1992, 1993, 1994, 2000, 2001
;;  Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

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

;; Window tree functions.

;;; Code:

(defmacro save-selected-window (&rest body)
  "Execute BODY, then select the window that was selected before BODY.
However, if that window has become dead, don't get an error,
just refrain from switching to it."
  `(let ((save-selected-window-window (selected-window)))
     (unwind-protect
	 (progn ,@body)
       (if (window-live-p save-selected-window-window)
	   (select-window save-selected-window-window)))))

(defun window-body-height (&optional window)
  "Return number of lines in window WINDOW for actual buffer text.
This does not include the mode line (if any) or the header line (if any)."
  (or window (setq window (selected-window)))
  (if (window-minibuffer-p window)
      (window-height window)
    (with-current-buffer (window-buffer window)
      (max 1 (- (window-height window)
		(if mode-line-format 1 0)
		(if header-line-format 1 0))))))

(defun one-window-p (&optional nomini all-frames)
  "Return non-nil if the selected window is the only window (in its frame).
Optional arg NOMINI non-nil means don't count the minibuffer
even if it is active.

The optional arg ALL-FRAMES t means count windows on all frames.
If it is `visible', count windows on all visible frames.
ALL-FRAMES nil or omitted means count only the selected frame,
plus the minibuffer it uses (which may be on another frame).
If ALL-FRAMES is neither nil nor t, count only the selected frame."
  (let ((base-window (selected-window)))
    (if (and nomini (eq base-window (minibuffer-window)))
	(setq base-window (next-window base-window)))
    (eq base-window
	(next-window base-window (if nomini 'arg) all-frames))))

(defun walk-windows (proc &optional minibuf all-frames)
  "Cycle through all visible windows, calling PROC for each one.
PROC is called with a window as argument.

Optional second arg MINIBUF t means count the minibuffer window even
if not active.  MINIBUF nil or omitted means count the minibuffer iff
it is active.  MINIBUF neither t nor nil means not to count the
minibuffer even if it is active.

Several frames may share a single minibuffer; if the minibuffer
counts, all windows on all frames that share that minibuffer count
too.  Therefore, if you are using a separate minibuffer frame
and the minibuffer is active and MINIBUF says it counts,
`walk-windows' includes the windows in the frame from which you
entered the minibuffer, as well as the minibuffer window.

ALL-FRAMES is the optional third argument.
ALL-FRAMES nil or omitted means cycle within the frames as specified above.
ALL-FRAMES = `visible' means include windows on all visible frames.
ALL-FRAMES = 0 means include windows on all visible and iconified frames.
ALL-FRAMES = t means include windows on all frames including invisible frames.
If ALL-FRAMES is a frame, it means include windows on that frame.
Anything else means restrict to the selected frame."
  ;; If we start from the minibuffer window, don't fail to come back to it.
  (if (window-minibuffer-p (selected-window))
      (setq minibuf t))
  (save-selected-window
    (if (framep all-frames)
	(select-window (frame-first-window all-frames)))
    (let* (walk-windows-already-seen
	   (walk-windows-current (selected-window)))
      (while (progn
	       (setq walk-windows-current
		     (next-window walk-windows-current minibuf all-frames))
	       (not (memq walk-windows-current walk-windows-already-seen)))
	(setq walk-windows-already-seen
	      (cons walk-windows-current walk-windows-already-seen))
	(funcall proc walk-windows-current)))))

(defun get-window-with-predicate (predicate &optional minibuf
					    all-frames default)
  "Return a window satisfying PREDICATE.

This function cycles through all visible windows using `walk-windows',
calling PREDICATE on each one.  PREDICATE is called with a window as
argument.  The first window for which PREDICATE returns a non-nil
value is returned.  If no window satisfies PREDICATE, DEFAULT is
returned.

Optional second arg MINIBUF t means count the minibuffer window even
if not active.  MINIBUF nil or omitted means count the minibuffer iff
it is active.  MINIBUF neither t nor nil means not to count the
minibuffer even if it is active.

Several frames may share a single minibuffer; if the minibuffer
counts, all windows on all frames that share that minibuffer count
too.  Therefore, if you are using a separate minibuffer frame
and the minibuffer is active and MINIBUF says it counts,
`walk-windows' includes the windows in the frame from which you
entered the minibuffer, as well as the minibuffer window.

ALL-FRAMES is the optional third argument.
ALL-FRAMES nil or omitted means cycle within the frames as specified above.
ALL-FRAMES = `visible' means include windows on all visible frames.
ALL-FRAMES = 0 means include windows on all visible and iconified frames.
ALL-FRAMES = t means include windows on all frames including invisible frames.
If ALL-FRAMES is a frame, it means include windows on that frame.
Anything else means restrict to the selected frame."
  (catch 'found
    (walk-windows #'(lambda (window)
		      (when (funcall predicate window)
			(throw 'found window)))
		  minibuf all-frames)
    default))

(defalias 'some-window 'get-window-with-predicate)

(defun minibuffer-window-active-p (window)
  "Return t if WINDOW (a minibuffer window) is now active."
  (eq window (active-minibuffer-window)))

(defun count-windows (&optional minibuf)
   "Return the number of visible windows.
This counts the windows in the selected frame and (if the minibuffer is
to be counted) its minibuffer frame (if that's not the same frame).
The optional arg MINIBUF non-nil means count the minibuffer
even if it is inactive."
   (let ((count 0))
     (walk-windows (function (lambda (w)
			       (setq count (+ count 1))))
		   minibuf)
     count))

(defun window-safely-shrinkable-p (&optional window)
  "Non-nil if the WINDOW can be shrunk without shrinking other windows.
If WINDOW is nil or omitted, it defaults to the currently selected window."
  (save-selected-window
    (when window (select-window window))
    (or (and (not (eq window (frame-first-window)))
	     (= (car (window-edges))
		(car (window-edges (previous-window)))))
	(= (car (window-edges))
	   (car (window-edges (next-window)))))))

(defun balance-windows ()
  "Make all visible windows the same height (approximately)."
  (interactive)
  (let ((count -1) levels newsizes level-size
	;; Don't count the lines that are above the uppermost windows.
	;; (These are the menu bar lines, if any.)
	(mbl (nth 1 (window-edges (frame-first-window (selected-frame)))))
	(last-window (previous-window (frame-first-window (selected-frame))))
	;; Don't count the lines that are past the lowest main window.
	total)
    ;; Bottom edge of last window determines what size we have to work with.
    (setq total
	  (+ (window-height last-window)
	     (nth 1 (window-edges last-window))))

    ;; Find all the different vpos's at which windows start,
    ;; then count them.  But ignore levels that differ by only 1.
    (let (tops (prev-top -2))
      (walk-windows (function (lambda (w)
				(setq tops (cons (nth 1 (window-edges w))
						 tops))))
		    'nomini)
      (setq tops (sort tops '<))
      (while tops
	(if (> (car tops) (1+ prev-top))
	    (setq prev-top (car tops)
		  count (1+ count)))
	(setq levels (cons (cons (car tops) count) levels))
	(setq tops (cdr tops)))
      (setq count (1+ count)))
    ;; Subdivide the frame into desired number of vertical levels.
    (setq level-size (/ (- total mbl) count))
    (save-selected-window
      ;; Set up NEWSIZES to map windows to their desired sizes.
      ;; If a window ends at the bottom level, don't include
      ;; it in NEWSIZES.  Those windows get the right sizes
      ;; by adjusting the ones above them.
      (walk-windows (function
		     (lambda (w)
		       (let ((newtop (cdr (assq (nth 1 (window-edges w))
						levels)))
			     (newbot (cdr (assq (+ (window-height w)
						   (nth 1 (window-edges w)))
						levels))))
			 (if newbot
			     (setq newsizes
				   (cons (cons w (* level-size (- newbot newtop)))
					 newsizes))))))
		    'nomini)
      ;; Make walk-windows start with the topmost window.
      (select-window (previous-window (frame-first-window (selected-frame))))
      (let (done (count 0))
	;; Give each window its precomputed size, or at least try.
	;; Keep trying until they all get the intended sizes,
	;; but not more than 3 times (to prevent infinite loop).
	(while (and (not done) (< count 3))
	  (setq done t)
	  (setq count (1+ count))
	  (walk-windows (function (lambda (w)
				    (select-window w)
				    (let ((newsize (cdr (assq w newsizes))))
				      (when newsize
					(enlarge-window (- newsize
							   (window-height))
							nil t)
					(unless (= (window-height) newsize)
					  (setq done nil))))))
			'nomini))))))

;;; I think this should be the default; I think people will prefer it--rms.
(defcustom split-window-keep-point t
  "*If non-nil, split windows keeps the original point in both children.
This is often more convenient for editing.
If nil, adjust point in each of the two windows to minimize redisplay.
This is convenient on slow terminals, but point can move strangely."
  :type 'boolean
  :group 'windows)

(defun split-window-vertically (&optional arg)
  "Split current window into two windows, one above the other.
The uppermost window gets ARG lines and the other gets the rest.
Negative arg means select the size of the lowermost window instead.
With no argument, split equally or close to it.
Both windows display the same buffer now current.

If the variable `split-window-keep-point' is non-nil, both new windows
will get the same value of point as the current window.  This is often
more convenient for editing.

Otherwise, we chose window starts so as to minimize the amount of
redisplay; this is convenient on slow terminals.  The new selected
window is the one that the current value of point appears in.  The
value of point can change if the text around point is hidden by the
new mode line."
  (interactive "P")
  (let ((old-w (selected-window))
	(old-point (point))
	(size (and arg (prefix-numeric-value arg)))
        (window-full-p nil)
	new-w bottom switch moved)
    (and size (< size 0) (setq size (+ (window-height) size)))
    (setq new-w (split-window nil size))
    (or split-window-keep-point
	(progn
	  (save-excursion
	    (set-buffer (window-buffer))
	    (goto-char (window-start))
            (setq moved (vertical-motion (window-height)))
	    (set-window-start new-w (point))
	    (if (> (point) (window-point new-w))
		(set-window-point new-w (point)))
            (and (= moved (window-height))
                 (progn
                   (setq window-full-p t)
                   (vertical-motion -1)))
            (setq bottom (point)))
          (and window-full-p
               (<= bottom (point))
               (set-window-point old-w (1- bottom)))
	  (and window-full-p
               (<= (window-start new-w) old-point)
               (progn
                 (set-window-point new-w old-point)
                 (select-window new-w)))))
    (split-window-save-restore-data new-w old-w)))

;; This is to avoid compiler warnings.
(defvar view-return-to-alist)

(defun split-window-save-restore-data (new-w old-w)
  (save-excursion
    (set-buffer (window-buffer))
    (if view-mode
	(let ((old-info (assq old-w view-return-to-alist)))
	  (setq view-return-to-alist
		(cons (cons new-w (cons (and old-info (car (cdr old-info))) t))
		      view-return-to-alist))))
    new-w))

(defun split-window-horizontally (&optional arg)
  "Split current window into two windows side by side.
This window becomes the leftmost of the two, and gets ARG columns.
Negative arg means select the size of the rightmost window instead.
The argument includes the width of the window's scroll bar; if there
are no scroll bars, it includes the width of the divider column
to the window's right, if any.  No arg means split equally."
  (interactive "P")
  (let ((old-w (selected-window))
	(size (and arg (prefix-numeric-value arg))))
    (and size (< size 0)
	 (setq size (+ (window-width) size)))
    (split-window-save-restore-data (split-window nil size t) old-w)))


(defun set-window-text-height (window height)
  "Sets the height in lines of the text display area of WINDOW to HEIGHT.
This doesn't include the mode-line (or header-line if any) or any
partial-height lines in the text display area.

If WINDOW is nil, the selected window is used.

Note that the current implementation of this function cannot always set
the height exactly, but attempts to be conservative, by allocating more
lines than are actually needed in the case where some error may be present."
  (let ((delta (- height (window-text-height window))))
    (unless (zerop delta)
      (let ((window-min-height 1))
	(if (and window (not (eq window (selected-window))))
	    (save-selected-window
	      (select-window window)
	      (enlarge-window delta))
	  (enlarge-window delta))))))


(defun enlarge-window-horizontally (arg)
  "Make current window ARG columns wider."
  (interactive "p")
  (enlarge-window arg t))

(defun shrink-window-horizontally (arg)
  "Make current window ARG columns narrower."
  (interactive "p")
  (shrink-window arg t))

(defun window-buffer-height (window)
  "Return the height (in screen lines) of the buffer that WINDOW is displaying."
  (save-excursion
    (set-buffer (window-buffer window))
    (goto-char (point-min))
    (let ((ignore-final-newline
           ;; If buffer ends with a newline, ignore it when counting height
           ;; unless point is after it.
           (and (not (eobp)) (eq ?\n (char-after (1- (point-max)))))))
      (+ 1 (nth 2 (compute-motion (point-min)
                                  '(0 . 0)
                                  (- (point-max) (if ignore-final-newline 1 0))
                                  (cons 0 100000000)
                                  (window-width window)
                                  nil
                                  window))))))

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

(defun fit-window-to-buffer (&optional window max-height min-height)
  "Make WINDOW the right size to display its contents exactly.
If WINDOW is omitted or nil, it defaults to the selected window.
If the optional argument MAX-HEIGHT is supplied, it is the maximum height
  the window is allowed to be, defaulting to the frame height.
If the optional argument MIN-HEIGHT is supplied, it is the minimum
  height the window is allowed to be, defaulting to `window-min-height'.

The heights in MAX-HEIGHT and MIN-HEIGHT include the mode-line and/or
header-line."
  (interactive)

  (when (null window)
    (setq window (selected-window)))
  (when (null max-height)
    (setq max-height (frame-height (window-frame window))))

  (let* ((buf
	  ;; Buffer that is displayed in WINDOW
	  (window-buffer window))
	 (window-height
	  ;; The current height of WINDOW
	  (window-height window))
	 (desired-height
	  ;; The height necessary to show the buffer displayed by WINDOW
	  ;; (`count-screen-lines' always works on the current buffer).
	  (with-current-buffer buf
	    (+ (count-screen-lines)
	       ;; If the buffer is empty, (count-screen-lines) is
	       ;; zero.  But, even in that case, we need one text line
	       ;; for cursor.
	       (if (= (point-min) (point-max))
		   1 0)
	       ;; For non-minibuffers, count the mode-line, if any
	       (if (and (not (window-minibuffer-p window))
			mode-line-format)
		   1 0)
	       ;; Count the header-line, if any
	       (if header-line-format 1 0))))
	 (delta
	  ;; Calculate how much the window height has to change to show
	  ;; desired-height lines, constrained by MIN-HEIGHT and MAX-HEIGHT.
	  (- (max (min desired-height max-height)
		  (or min-height window-min-height))
	     window-height))
	 ;; We do our own height checking, so avoid any restrictions due to
	 ;; window-min-height.
	 (window-min-height 1))

    ;; Don't try to redisplay with the cursor at the end
    ;; on its own line--that would force a scroll and spoil things.
    (when (with-current-buffer buf
	    (and (eobp) (bolp) (not (bobp))))
      (set-window-point window (1- (window-point window))))

    (save-selected-window
      (select-window window)

      ;; Adjust WINDOW to the nominally correct size (which may actually
      ;; be slightly off because of variable height text, etc).
      (unless (zerop delta)
	(enlarge-window delta))

      ;; Check if the last line is surely fully visible.  If not,
      ;; enlarge the window.
      (let ((end (with-current-buffer buf
		   (save-excursion
		     (goto-char (point-max))
		     (when (and (bolp) (not (bobp)))
		       ;; Don't include final newline
		       (backward-char 1))
		     (when truncate-lines
		       ;; If line-wrapping is turned off, test the
		       ;; beginning of the last line for visibility
		       ;; instead of the end, as the end of the line
		       ;; could be invisible by virtue of extending past
		       ;; the edge of the window.
		       (forward-line 0))
		     (point)))))
	(set-window-vscroll window 0)
	(while (and (< desired-height max-height)
		    (= desired-height (window-height window))
		    (not (pos-visible-in-window-p end window)))
	  (enlarge-window 1)
	  (setq desired-height (1+ desired-height)))))))

(defun shrink-window-if-larger-than-buffer (&optional window)
  "Shrink the WINDOW to be as small as possible to display its contents.
If WINDOW is omitted or nil, it defaults to the selected window.
Do not shrink to less than `window-min-height' lines.
Do nothing if the buffer contains more lines than the present window height,
or if some of the window's contents are scrolled out of view,
or if shrinking this window would also shrink another window.
or if the window is the only window of its frame.
Return non-nil if the window was shrunk."
  (interactive)
  (when (null window)
    (setq window (selected-window)))
  (let* ((frame (window-frame window))
	 (mini (frame-parameter frame 'minibuffer))
	 (edges (window-edges window)))
    (if (and (not (eq window (frame-root-window frame)))
	     (window-safely-shrinkable-p)
	     (pos-visible-in-window-p (point-min) window)
	     (not (eq mini 'only))
	     (or (not mini)
		 (let ((mini-window (minibuffer-window frame)))
		   (or (null mini-window)
		       (not (eq frame (window-frame mini-window)))
		       (< (nth 3 edges)
			  (nth 1 (window-edges mini-window)))
		       (> (nth 1 edges) 
			  (frame-parameter frame 'menu-bar-lines))))))
	(fit-window-to-buffer window (window-height window)))))

(defun kill-buffer-and-window ()
  "Kill the current buffer and delete the selected window."
  (interactive)
  (if (yes-or-no-p (format "Kill buffer `%s'? " (buffer-name)))
      (let ((buffer (current-buffer)))
	(delete-window (selected-window))
	(kill-buffer buffer))
    (error "Aborted")))

(defun quit-window (&optional kill window)
  "Quit the current buffer.  Bury it, and maybe delete the selected frame.
\(The frame is deleted if it is contains a dedicated window for the buffer.)
With a prefix argument, kill the buffer instead.

Noninteractively, if KILL is non-nil, then kill the current buffer,
otherwise bury it.

If WINDOW is non-nil, it specifies a window; we delete that window,
and the buffer that is killed or buried is the one in that window."
  (interactive "P")
  (let ((buffer (window-buffer window))
	(frame (window-frame (or window (selected-window))))
	(window-solitary
	 (save-selected-window
	   (if window
	       (select-window window))
	   (one-window-p t)))
	window-handled)

    (save-selected-window
      (if window
	  (select-window window))
      (or (window-minibuffer-p)
	  (window-dedicated-p (selected-window))
	  (switch-to-buffer (other-buffer))))

    ;; Get rid of the frame, if it has just one dedicated window
    ;; and other visible frames exist.
    (and (or (window-minibuffer-p) (window-dedicated-p window))
	 (delq frame (visible-frame-list))
	 window-solitary
	 (if (and (eq default-minibuffer-frame frame)
		  (= 1 (length (minibuffer-frame-list))))
	     (setq window nil)
	   (delete-frame frame)
	   (setq window-handled t)))

    ;; Deal with the buffer.
    (if kill
	(kill-buffer buffer)
      (bury-buffer buffer))

    ;; Maybe get rid of the window.
    (and window (not window-handled) (not window-solitary)
	 (delete-window window))))

(defun handle-select-window (event)
  "Handle select-window events."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (if (or (not (window-minibuffer-p window))
	    (minibuffer-window-active-p window))
	(select-window window))))

(define-key ctl-x-map "2" 'split-window-vertically)
(define-key ctl-x-map "3" 'split-window-horizontally)
(define-key ctl-x-map "}" 'enlarge-window-horizontally)
(define-key ctl-x-map "{" 'shrink-window-horizontally)
(define-key ctl-x-map "-" 'shrink-window-if-larger-than-buffer)
(define-key ctl-x-map "+" 'balance-windows)
(define-key ctl-x-4-map "0" 'kill-buffer-and-window)

;;; window.el ends here
