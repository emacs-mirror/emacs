;;; ediff-wind.el --- window manipulation utilities
;;; Copyright (C) 1994, 1995 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.sunysb.edu>

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


(require 'ediff-init)


(defvar ediff-window-setup-function (if (ediff-window-display-p)
					'ediff-setup-windows-multiframe
				      'ediff-setup-windows-plain)
  "*Function called to set up windows.
Ediff provides a choice of two functions: ediff-setup-windows-plain, for
doing everything in one frame, and ediff-setup-windows-multiframe,
which sets the control panel in a separate frame. Also, if the latter
function detects that one of the buffers A/B is seen in some other frame,
it will try to keep that buffer in that frame.

If you don't like the two functions provided---write your own one.
The basic guidelines:
    1. It should leave the control buffer current and the control window
       selected. 
    2. It should set ediff-window-A, ediff-window-B, ediff-window-C,
       and ediff-control-window to contain window objects that display
       the corresponding buffers.
    3. It should accept the following arguments:
       buffer-A, buffer-B, buffer-C, control-buffer
       Buffer C may not be used in jobs that compare only two buffers.
If you plan to do something fancy, take a close look at how the two
provided functions are written.")

;; indicates if we are in a multiframe setup
(ediff-defvar-local ediff-multiframe nil "")

;; Share of the frame occupied by the merge window (buffer C)
(ediff-defvar-local ediff-merge-window-share 0.45 "")

;; The control window.
(ediff-defvar-local ediff-control-window nil "")
;; Official window for buffer A
(ediff-defvar-local ediff-window-A nil "")
;; Official window for buffer B
(ediff-defvar-local ediff-window-B nil "")
;; Official window for buffer C
(ediff-defvar-local ediff-window-C nil "")
;; Ediff's window configuration.
;; Used to minimize the need to rearrange windows.
(ediff-defvar-local ediff-window-config-saved "" "")


(defvar ediff-split-window-function 'split-window-vertically
  "*The function used to split the main window between buffer-A and buffer-B.
You can set it to a horizontal split instead of the default vertical split
by setting this variable to `split-window-horizontally'.
You can also have your own function to do fancy splits.
This variable has no effect when buffer-A/B are shown in different frames.
In this case, Ediff will use those frames to display these buffers.")

(defvar ediff-merge-split-window-function 'split-window-horizontally
  "*The function used to split the main window between buffer-A and buffer-B.
You can set it to a vertical split instead of the default horizontal split
by setting this variable to `split-window-vertically'.
You can also have your own function to do fancy splits.
This variable has no effect when buffer-A/B/C are shown in different frames.
In this case, Ediff will use those frames to display these buffers.")

(defconst ediff-control-frame-parameters
  (if (ediff-window-display-p)
      (list 
       '(name . "Ediff")
       ;;'(unsplittable . t)
       '(minibuffer . nil)
       '(vertical-scroll-bars . nil)  ; Emacs only
       '(scrollbar-width . 0)         ; XEmacs only
       '(menu-bar-lines . 0)          ; Emacs only
       ;; don't lower and auto-raise
       '(auto-lower . nil)
       '(auto-raise . t)
       ;; this blocks queries from  window manager as to where to put
       ;; ediff's control frame. we put the frame outside the display,
       ;; so the initial frame won't jump all over the screen
       '(user-position . t)
       (cons 'top  (if (fboundp 'ediff-display-pixel-height)
		       (1+ (ediff-display-pixel-height))
		     3000))
       (cons 'left (if (fboundp 'ediff-display-pixel-width)
		       (1+ (ediff-display-pixel-width))
		     3000))
       ))
  "Frame parameters for displaying Ediff Control Panel.
Do not specify width and height here. These are computed automatically.")

(defvar ediff-control-frame-position-function 'ediff-make-frame-position
  "Function to call to determine the desired location for the control panel.
Expects three parameters: the control buffer, the desired width and height
of the control frame. It returns an association list
of the form \(\(top . <position>\) \(left . <position>\)\)")

(defvar ediff-control-frame-upward-shift (if ediff-xemacs-p 36 4)
  "*The upward shift of control frame from the top of buffer A's frame.
Measured in pixels.
This is used by the default control frame positioning function,
`ediff-make-frame-position'. This variable is provided for easy
customization of the default.")

(defvar ediff-narrow-control-frame-leftward-shift (if ediff-xemacs-p 7 3)
  "*The leftward shift of control frame from the right edge of buf A's frame.
Measured in characters.
This is used by the default control frame positioning function,
`ediff-make-frame-position' to adjust the position of the control frame
when it shows the short menu. This variable is provided for easy
customization of the default.")

(defvar ediff-wide-control-frame-rightward-shift 7
  "*The rightward shift of control frame from the left edge of buf A's frame.
Measured in characters.
This is used by the default control frame positioning function,
`ediff-make-frame-position' to adjust the position of the control frame
when it shows the full menu. This variable is provided for easy
customization of the default.")


;; Wide frame display

;; t means Ediff is using wide display
(ediff-defvar-local ediff-wide-display-p nil "")
;; keeps frame config for toggling wide display
(ediff-defvar-local ediff-wide-display-orig-parameters nil 
  "Frame parameters to be restored when the user wants to toggle the wide
display off.")
(ediff-defvar-local ediff-wide-display-frame nil
  "Frame to be used for wide display.")
(ediff-defvar-local ediff-make-wide-display-function 'ediff-make-wide-display
  "The value is a function that is called to create a wide display.
The function is called without arguments. It should resize the frame in
which buffers A, B, and C are to be displayed, and it should save the old
frame parameters in `ediff-wide-display-orig-parameters'.
The variable `ediff-wide-display-frame' should be set to contain
the frame used for the wide display.")

;; Frame used for the control panel in a windowing system.
(ediff-defvar-local ediff-control-frame nil "")

(defvar ediff-prefer-iconified-control-frame nil
  "*If t, keep control panel iconified when help message is off.
This has effect only on a windowing system.
If t, hiting `?' to toggle control panel off iconifies it.

This is only useful in Emacs and only for certain kinds of window managers,
such as TWM and its derivatives, since the window manager must permit
keyboard input to go into icons. XEmacs completely ignores keyboard input
into icons, regardless of the window manager.")

;;; Functions

(defun ediff-get-window-by-clicking (wind prev-wind wind-number)
  (let (event)
    (message
     "Select windows by clicking. Please click on Window %d " wind-number)
    (while (not (ediff-mouse-event-p (setq event (ediff-read-event))))
      (if (sit-for 1) ; if sequence of events, wait till the final word
	  (beep 1))
      (message "Please click on Window %d " wind-number))
    (ediff-read-event) ; discard event
    (setq wind (if ediff-xemacs-p
		   (event-window event)
		 (posn-window (event-start event))))
    ))
      

;; Select the lowest window on the frame.
(defun ediff-select-lowest-window ()
  (if ediff-xemacs-p
      (select-window (frame-lowest-window))
    (let* ((lowest-window (selected-window))
	   (bottom-edge (car (cdr (cdr (cdr (window-edges))))))
	   (last-window (save-excursion
			  (other-window -1) (selected-window)))
	   (window-search t))
      (while window-search
	(let* ((this-window (next-window))
	       (next-bottom-edge
		(car (cdr (cdr (cdr (window-edges this-window)))))))
	  (if (< bottom-edge next-bottom-edge)
	      (progn
		(setq bottom-edge next-bottom-edge)
		(setq lowest-window this-window)))
	  
	  (select-window this-window)
	  (if (eq last-window this-window)
	      (progn
		(select-window lowest-window)
		(setq window-search nil))))))))


;;; Common window setup routines

;; Set up the window configuration.  If POS is given, set the points to
;; the beginnings of the buffers.
;; When 3way comparison is added, this will have to choose the appropriate
;; setup function based on ediff-job-name
(defun ediff-setup-windows (buffer-A buffer-B buffer-C control-buffer)
  ;; Make sure we are not in the minibuffer window when we try to delete
  ;; all other windows.
  (run-hooks 'ediff-before-setup-windows-hooks)
  (if (eq (selected-window) (minibuffer-window))
      (other-window 1))
      
  ;; in case user did a no-no on a tty
  (or (ediff-window-display-p)
      (setq ediff-window-setup-function 'ediff-setup-windows-plain))
  
  (or (ediff-keep-window-config control-buffer)
      (funcall 
       (ediff-eval-in-buffer control-buffer ediff-window-setup-function)
       buffer-A buffer-B buffer-C control-buffer))
  (run-hooks 'ediff-after-setup-windows-hooks))

;; Just set up 3 windows.
;; Usually used without windowing systems
;; With windowing, we want to use dedicated frames.
(defun ediff-setup-windows-plain (buffer-A buffer-B buffer-C control-buffer)
  (ediff-eval-in-buffer control-buffer
    (setq ediff-multiframe nil))
  (if ediff-merge-job
      (ediff-setup-windows-plain-merge
       buffer-A buffer-B buffer-C control-buffer)
    (ediff-setup-windows-plain-compare 
     buffer-A buffer-B buffer-C control-buffer)))
     
(defun ediff-setup-windows-plain-merge (buf-A buf-B buf-C control-buffer)
  ;; skip dedicated and unsplittable frames
  (ediff-destroy-control-frame control-buffer)
  (let ((window-min-height 2)
	split-window-function 
	merge-window-share merge-window-lines
	wind-A wind-B wind-C)
    (ediff-eval-in-buffer control-buffer
      (setq merge-window-share ediff-merge-window-share
	    ;; this lets us have local versions of ediff-split-window-function
	    split-window-function ediff-split-window-function))
    (delete-other-windows)
    (split-window-vertically)
    (ediff-select-lowest-window)
    (ediff-setup-control-buffer control-buffer)
    
    ;; go to the upper window and split it betw A, B, and possibly C
    (other-window 1) 
    (setq merge-window-lines
	  (max 2 (round (* (window-height) merge-window-share))))
    (switch-to-buffer buf-A)
    (setq wind-A (selected-window))
    
    ;; XEmacs seems to have a lot of trouble with display
    ;; It won't set things right unless we tell it to sit still
    (if ediff-xemacs-p (sit-for 0))
    
    (split-window-vertically (max 2 (- (window-height) merge-window-lines)))
    (if (eq (selected-window) wind-A) 
	(other-window 1))
    (setq wind-C (selected-window))
    (switch-to-buffer buf-C)
    
    (select-window wind-A)
    (funcall split-window-function)
		  
    (if (eq (selected-window) wind-A)
	(other-window 1))
    (switch-to-buffer buf-B)
    (setq wind-B (selected-window))
	  
    (ediff-eval-in-buffer control-buffer
      (setq ediff-window-A wind-A
	    ediff-window-B wind-B
	    ediff-window-C wind-C))
    
    (ediff-select-lowest-window)
    (ediff-setup-control-buffer control-buffer)
    ))

     
;; This function handles all comparison jobs, including 3way jobs
(defun ediff-setup-windows-plain-compare (buf-A buf-B buf-C control-buffer)
  ;; skip dedicated and unsplittable frames
  (ediff-destroy-control-frame control-buffer)
  (let ((window-min-height 2)
	split-window-function wind-width-or-height
	three-way-comparison
	wind-A-start wind-B-start wind-A wind-B wind-C)
    (ediff-eval-in-buffer control-buffer
      (setq wind-A-start (ediff-overlay-start
			  (ediff-get-value-according-to-buffer-type
			   'A ediff-narrow-bounds))
	    wind-B-start (ediff-overlay-start
			  (ediff-get-value-according-to-buffer-type
			   'B  ediff-narrow-bounds))
	    ;; this lets us have local versions of ediff-split-window-function
	    split-window-function ediff-split-window-function
	    three-way-comparison ediff-3way-comparison-job))
    (delete-other-windows)
    (split-window-vertically)
    (ediff-select-lowest-window)
    (ediff-setup-control-buffer control-buffer)
    
    ;; go to the upper window and split it betw A, B, and possibly C
    (other-window 1) 
    (switch-to-buffer buf-A)
    (setq wind-A (selected-window))
    (if three-way-comparison
	(setq wind-width-or-height
	      (/ (if (eq split-window-function 'split-window-vertically)
		     (window-height wind-A)
		   (window-width wind-A))
		 3)))
    
    ;; XEmacs seems to have a lot of trouble with display
    ;; It won't set things right unless we tell it to sit still
    (if ediff-xemacs-p (sit-for 0))
    
    (funcall split-window-function wind-width-or-height)
		  
    (if (eq (selected-window) wind-A)
	(other-window 1))
    (switch-to-buffer buf-B)
    (setq wind-B (selected-window))
	  
    (if three-way-comparison
	(progn
	  (funcall split-window-function) ; equally
	  (if (eq (selected-window) wind-B)
	      (other-window 1))
	  (switch-to-buffer buf-C)
	  (setq wind-C (selected-window))))
	  
    (ediff-eval-in-buffer control-buffer
      (setq ediff-window-A wind-A
	    ediff-window-B wind-B
	    ediff-window-C wind-C))
    
    ;; It is unlikely that we will want to implement 3way window comparison.
    ;; So, only buffers A and B are used here.
    (if ediff-windows-job
	(progn
	  (set-window-start wind-A wind-A-start)
	  (set-window-start wind-B wind-B-start)))
  
    (ediff-select-lowest-window)
    (ediff-setup-control-buffer control-buffer)
    ))

    
;; dispatch the appropriate window setup function
(defun ediff-setup-windows-multiframe (buf-A buf-B buf-C control-buf)
  (ediff-eval-in-buffer control-buf
    (setq ediff-multiframe t))
  (if ediff-merge-job
      (ediff-setup-windows-multiframe-merge buf-A buf-B buf-C control-buf)
    (ediff-setup-windows-multiframe-compare buf-A buf-B buf-C control-buf)))
    
(defun ediff-setup-windows-multiframe-merge (buf-A buf-B buf-C control-buf)
;;; Algorithm:
;;;   If A and B are in the same frame but C's frame is different--- use one
;;;   frame for A and B and use a separate frame for C. 
;;;   If C's frame is non-existent, then: if the first suitable
;;;   non-dedicated frame  is different from A&B's, then use it for C.
;;;   Otherwise, put A,B, and C in one frame.
;;;   If buffers A, B, C are is separate frames, use them to display these
;;;   buffers.

  ;;   Skip dedicated or iconified frames. 
  ;;   Unsplittable frames are taken care of later.
  (ediff-skip-unsuitable-frames 'ok-unsplittable)
  
  (let* ((window-min-height 2)
	 (wind-A (ediff-get-visible-buffer-window buf-A))
	 (wind-B (ediff-get-visible-buffer-window buf-B))
	 (wind-C (ediff-get-visible-buffer-window buf-C))
	 (frame-A (if wind-A (window-frame wind-A)))
	 (frame-B (if wind-B (window-frame wind-B)))
	 (frame-C (if wind-C (window-frame wind-C)))
	 ;; on wide display, do things in one frame
	 (force-one-frame 
	  (ediff-eval-in-buffer control-buf ediff-wide-display-p))
	 ;; this lets us have local versions of ediff-split-window-function
	 (split-window-function 
	  (ediff-eval-in-buffer control-buf ediff-split-window-function))
	 (orig-wind (selected-window))
	 (orig-frame (selected-frame))
	 (use-same-frame (or force-one-frame
			     (eq frame-A (or frame-C orig-frame))
			     (eq frame-B (or frame-C orig-frame))
			     (not (frame-live-p frame-A))
			     (not (frame-live-p frame-B))
			     (and (eq frame-A frame-B)
				  (not (frame-live-p frame-C)))
			     ))
	 (use-same-frame-for-AB (and (not use-same-frame)
				     (eq frame-A frame-B)))
	 (merge-window-share (ediff-eval-in-buffer control-buf
			       ediff-merge-window-share))
	 merge-window-lines
	 designated-minibuffer-frame
	 done-A done-B done-C)
    
    ;; buf-A on its own
    (if (and (window-live-p wind-A)
	     (null use-same-frame)
	     (null use-same-frame-for-AB))
	(progn
	  (select-window wind-A)
	  (delete-other-windows)
	  (switch-to-buffer buf-A)
	  (setq wind-A (selected-window))
	  (setq done-A t)))
    
    ;; buf-B on its own
    (if (and (window-live-p wind-B) (null use-same-frame)) ; buf B on its own
	(progn
	  (select-window wind-B)
	  (delete-other-windows)
	  (switch-to-buffer buf-B)
	  (setq wind-B (selected-window))
	  (setq done-B t)))
	  
    ;; buf-C on its own
    (if (and (window-live-p wind-C) (null use-same-frame)) ; buf C on its own
	(progn
	  (select-window wind-C)
	  (delete-other-windows)
	  (switch-to-buffer buf-C)
	  (setq wind-C (selected-window))
	  (setq done-C t)))
    
    (if use-same-frame-for-AB
	(progn 
	  (select-frame frame-A)
	  (switch-to-buffer buf-A)
	  (delete-other-windows)
	  (setq wind-A (selected-window))
	  
	  (funcall split-window-function)
	  (if (eq (selected-window) wind-A) 
	      (other-window 1))
	  (switch-to-buffer buf-B)
	  (setq wind-B (selected-window))
	  
	  (setq done-A t
		done-B t)))
    
    (if use-same-frame
	(let ((curr-frame (selected-frame))
	      (window-min-height 2))
	  ;; avoid dedicated and non-splittable windows
	  (ediff-skip-unsuitable-frames)
	  (or (eq curr-frame (selected-frame))
	      (setq wind-A nil
		    wind-B nil
		    wind-C nil
		    orig-wind (selected-window)))

	  ;; set the right frame
	  (cond (wind-A (select-window wind-A))
		(wind-B (select-window wind-B))
		(wind-C (select-window wind-C))
		(t (select-window orig-wind)))
	  (delete-other-windows)
	  (setq merge-window-lines
		(max 2 (round (* (window-height) merge-window-share))))
	  (switch-to-buffer buf-A)
	  (setq wind-A (selected-window))
	  
	  ;; XEmacs seems to have a lot of trouble with display
	  ;; It won't set things right unless we tell it to catch breath
	  (if ediff-xemacs-p (sit-for 0))
	  
	  (split-window-vertically
	   (max 2 (- (window-height) merge-window-lines)))
	  (if (eq (selected-window) wind-A) 
	      (other-window 1))
	  (setq wind-C (selected-window))
	  (switch-to-buffer buf-C)
	  
	  (select-window wind-A)
	  
	  (funcall split-window-function)
	  (if (eq (selected-window) wind-A) 
	      (other-window 1))
	  (switch-to-buffer buf-B)
	  (setq wind-B (selected-window))
	  
	  (setq done-A t
		done-B t
		done-C t)
	  ))
    
    (or done-A  ; Buf A to be set in its own frame
	(progn  ; It was not set up yet as it wasn't visible
	  (select-window orig-wind)
	  (delete-other-windows)
	  (switch-to-buffer buf-A)
	  (setq wind-A (selected-window))
	  ))
    (or done-B  ; Buf B to be set in its own frame
	(progn  ; It was not set up yet as it wasn't visible
	  (select-window orig-wind)
	  (delete-other-windows)
	  (switch-to-buffer buf-B)
	  (setq wind-B (selected-window))
	  ))
    
    (or done-C  ; Buf C to be set in its own frame.
	(progn  ; It was not set up yet as it wasn't visible
	  (select-window orig-wind)
	  (delete-other-windows)
	  (switch-to-buffer buf-C)
	  (setq wind-C (selected-window))
	  ))
    
    (ediff-eval-in-buffer control-buf
      (setq ediff-window-A wind-A
	    ediff-window-B wind-B
	    ediff-window-C wind-C)
      (setq frame-A (window-frame ediff-window-A)
	    designated-minibuffer-frame
	    (window-frame (minibuffer-window frame-A))))
    
    (ediff-setup-control-frame control-buf designated-minibuffer-frame)
    ))

  
;; Window setup for all comparison jobs, including 3way comparisons
(defun ediff-setup-windows-multiframe-compare (buf-A buf-B buf-C control-buf)
;;; Algorithm:
;;;    If a buffer is seen in a frame, use that frame for that buffer.  
;;;    If it is not seen, use the current frame.
;;;    If both buffers are not seen, they share the current frame.  If one
;;;    of the buffers is not seen, it is placed in the current frame (where
;;;    ediff started). If that frame is displaying the other buffer, it is
;;;    shared between the two buffers.
;;;    However, if we decide to put both buffers in one frame
;;;    and the selected frame isn't splittable, we create a new frame and
;;;    put both buffers there, event if one of this buffers is visible in
;;;    another frame.
  
  ;; Skip dedicated or iconified frames.
  ;; Unsplittable frames are taken care of later.
  (ediff-skip-unsuitable-frames 'ok-unsplittable)
  
  (let* ((window-min-height 2)
	 (wind-A (ediff-get-visible-buffer-window buf-A))
	 (wind-B (ediff-get-visible-buffer-window buf-B))
	 (wind-C (ediff-get-visible-buffer-window buf-C))
	 (frame-A (if wind-A (window-frame wind-A)))
	 (frame-B (if wind-B (window-frame wind-B)))
	 (frame-C (if wind-C (window-frame wind-C)))
	 (ctl-frame-exists-p (ediff-eval-in-buffer control-buf
			       (frame-live-p ediff-control-frame)))
	 ;; on wide display, do things in one frame
	 (force-one-frame 
	  (ediff-eval-in-buffer control-buf ediff-wide-display-p))
	 ;; this lets us have local versions of ediff-split-window-function
	 (split-window-function 
	  (ediff-eval-in-buffer control-buf ediff-split-window-function))
	 (three-way-comparison
	  (ediff-eval-in-buffer control-buf ediff-3way-comparison-job))
	 (orig-wind (selected-window))
	 (use-same-frame (or force-one-frame
			     (eq frame-A frame-B)
			     (if three-way-comparison
				 (or (eq frame-A frame-C)
				     (eq frame-B frame-C)
				     (not (frame-live-p frame-A))
				     (not (frame-live-p frame-B))
				     (not (frame-live-p frame-C))))
			     (and (not (frame-live-p frame-B))
				  (or ctl-frame-exists-p
				      (eq frame-A (selected-frame))))
			     (and (not (frame-live-p frame-A))
				  (or ctl-frame-exists-p
				      (eq frame-B (selected-frame))))))
	 wind-A-start wind-B-start 
	 designated-minibuffer-frame
	 done-A done-B done-C)
    
    (ediff-eval-in-buffer control-buf
      (setq wind-A-start (ediff-overlay-start
			  (ediff-get-value-according-to-buffer-type
			   'A ediff-narrow-bounds))
	    wind-B-start (ediff-overlay-start
			  (ediff-get-value-according-to-buffer-type
			   'B ediff-narrow-bounds))))
    
    (if (and (window-live-p wind-A) (null use-same-frame)) ; buf-A on its own
	(progn
	  (select-window wind-A)
	  (delete-other-windows)
	  (switch-to-buffer buf-A)
	  (setq wind-A (selected-window))
	  (setq done-A t)))
    
    (if (and (window-live-p wind-B) (null use-same-frame)) ; buf B on its own
	(progn
	  (select-window wind-B)
	  (delete-other-windows)
	  (switch-to-buffer buf-B)
	  (setq wind-B (selected-window))
	  (setq done-B t)))
	  
    (if (and (window-live-p wind-C) (null use-same-frame)) ; buf C on its own
	(progn
	  (select-window wind-C)
	  (delete-other-windows)
	  (switch-to-buffer buf-C)
	  (setq wind-C (selected-window))
	  (setq done-C t)))
    
    (if use-same-frame
	(let ((curr-frame (selected-frame))
	      ;; this affects 3way setups only
	      wind-width-or-height)
	  ;; avoid dedicated and non-splittable windows
	  (ediff-skip-unsuitable-frames)
	  (or (eq curr-frame (selected-frame))
	      (setq wind-A nil
		    wind-B nil
		    wind-C nil
		    orig-wind (selected-window)))

	  ;; set the right frame
	  (cond (wind-A (select-window wind-A))
		(wind-B (select-window wind-B))
		(wind-C (select-window wind-C))
		(t (select-window orig-wind)))
	  (delete-other-windows)
	  (switch-to-buffer buf-A)
	  (setq wind-A (selected-window))
	  
	  ;; XEmacs seems to have a lot of trouble with display
	  ;; It won't set things right unless we tell it to catch breath
	  (if ediff-xemacs-p (sit-for 0))
	  
	  (if three-way-comparison
	      (setq wind-width-or-height
		    (/
		     (if (eq split-window-function 'split-window-vertically)
			 (window-height wind-A)
		       (window-width wind-A))
		     3)))
	  
	  (funcall split-window-function wind-width-or-height)
	  (if (eq (selected-window) wind-A) 
	      (other-window 1))
	  (switch-to-buffer buf-B)
	  (setq wind-B (selected-window))
	  
	  (if three-way-comparison
	      (progn
		(funcall split-window-function) ; equally
		(if (memq (selected-window) (list wind-A wind-B))
		    (other-window 1))
		(switch-to-buffer buf-C)
		(setq wind-C (selected-window))))
	  (setq done-A t
		done-B t
		done-C t)
	  ))
    
    (or done-A  ; Buf A to be set in its own frame
	(progn  ; It was not set up yet as it wasn't visible
	  (select-window orig-wind)
	  (delete-other-windows)
	  (switch-to-buffer buf-A)
	  (setq wind-A (selected-window))
	  ))
    (or done-B  ; Buf B to be set in its own frame
	(progn  ; It was not set up yet as it wasn't visible
	  (select-window orig-wind)
	  (delete-other-windows)
	  (switch-to-buffer buf-B)
	  (setq wind-B (selected-window))
	  ))
    
    (if three-way-comparison
	(or done-C  ; Buf C to be set in its own frame
	    (progn  ; It was not set up yet as it wasn't visible
	      (select-window orig-wind)
	      (delete-other-windows)
	      (switch-to-buffer buf-C)
	      (setq wind-C (selected-window))
	      )))
    
    (ediff-eval-in-buffer control-buf
      (setq ediff-window-A wind-A
	    ediff-window-B wind-B
	    ediff-window-C wind-C)
      
      (setq frame-A (window-frame ediff-window-A)
	    designated-minibuffer-frame
	    (window-frame (minibuffer-window frame-A))))
    
    ;; It is unlikely that we'll implement ediff-windows that would compare
    ;; 3 windows at once. So, we don't use buffer C here.
    (if ediff-windows-job
	(progn
	  (set-window-start wind-A wind-A-start)
	  (set-window-start wind-B wind-B-start)))
    
    (ediff-setup-control-frame control-buf designated-minibuffer-frame)
    ))

;; skip unsplittable and dedicated windows
;; create a new splittable frame if none is found
(defun ediff-skip-unsuitable-frames (&optional ok-unsplittable)
  (if (ediff-window-display-p)
      (let (last-window)
	(while (and (not (eq (selected-window) last-window))
		    (or
		     (window-dedicated-p (selected-window))
		     (ediff-frame-iconified-p (selected-frame))
		     (if ok-unsplittable
			 nil
		       (ediff-frame-unsplittable-p (selected-frame)))))
	  ;; remember where started
	  (or last-window (setq last-window (selected-window)))
	  ;; try new window
	  (other-window 1 t))
	(if (eq (selected-window) last-window)
	    ;; fed up, no appropriate frame
	    (progn
	      ;;(redraw-display)
	      (select-frame (ediff-make-frame '((unsplittable)))))))))

;; Prepare or refresh control frame
(defun ediff-setup-control-frame (ctl-buffer designated-minibuffer-frame)
  (let ((window-min-height 2)
	ctl-frame-iconified-p dont-iconify-ctl-frame deiconify-ctl-frame
	ctl-frame old-ctl-frame lines 
	fheight fwidth adjusted-parameters) 
	
    (ediff-eval-in-buffer ctl-buffer
      (if ediff-xemacs-p (set-buffer-menubar nil))
      (run-hooks 'ediff-before-setup-control-frame-hooks))
  
    (setq old-ctl-frame (ediff-eval-in-buffer ctl-buffer ediff-control-frame))
    ;; Delete the old ctl frame and get a new ctl frame.
    ;; The old ctl frame is deleted to let emacs reset default minibuffer
    ;; frame or when the ctl frame needs to be moved.
    ;; The old frame isn't reused, since ediff-setup-control-frame is called
    ;; very rarely, so the overhead is minimal.
    (if (frame-live-p old-ctl-frame) (delete-frame old-ctl-frame))
    ;;(redraw-display)
    ;; new ctl frame should be created while ctl-buff is current, so that
    ;; the local default-minibuffer-frame will be consulted and
    ;; that ediff-control-frame-parameters will have the right value.
    (ediff-eval-in-buffer ctl-buffer
      (let ((default-minibuffer-frame designated-minibuffer-frame))
	(setq ctl-frame (make-frame ediff-control-frame-parameters)
	      ediff-control-frame ctl-frame)))
    
    (setq ctl-frame-iconified-p (ediff-frame-iconified-p ctl-frame))
    (select-frame ctl-frame)
    (if (window-dedicated-p (selected-window))
	()
      (delete-other-windows)
      (switch-to-buffer ctl-buffer))
      
    ;; must be before ediff-setup-control-buffer
    (if ediff-xemacs-p
	;; just a precaution--we should be in ctl-buffer already
	(ediff-eval-in-buffer ctl-buffer
	  (make-local-variable 'frame-title-format)
	  (make-local-variable 'frame-icon-title-format)))
    
    (ediff-setup-control-buffer ctl-buffer)
    (setq dont-iconify-ctl-frame
	  (not (string= ediff-help-message ediff-brief-help-message)))
    (setq deiconify-ctl-frame 
	  (and (eq this-command 'ediff-toggle-help)
	       dont-iconify-ctl-frame))
    
    ;; 1 more line for the modeline
    (setq lines (if ediff-xemacs-p
		    (+ 2 (count-lines (point-min) (point-max)))
		  (1+ (count-lines (point-min) (point-max))))
	  fheight lines
	  fwidth (+ (ediff-help-message-line-length) 2)
	  adjusted-parameters (append (list
				       (cons 'width fwidth)
				       (cons 'height fheight))
				      (funcall
				       ediff-control-frame-position-function
				       ctl-buffer fwidth fheight)))
    
    ;; In XEmacs, buffer menubar needs to be killed before frame parameters
    ;; are changed. XEmacs needs to redisplay, as it has trouble setting
    ;; height correctly otherwise.
    (if ediff-xemacs-p
	(progn
	  (set-specifier top-toolbar-height (list ctl-frame 0))
	  (set-specifier bottom-toolbar-height (list ctl-frame 0))
	  (set-specifier left-toolbar-width (list ctl-frame 0))
	  (set-specifier right-toolbar-width (list ctl-frame 0))
	  (sit-for 0)))
    
    ;; Under OS/2 (emx) we have to call modify frame parameters twice, in
    ;; order to make sure that at least once we do it for non-iconified
    ;; frame. If appears that in the OS/2 port of Emacs, one can't modify
    ;; frame parameters of iconified frames.
    (if (eq system-type 'emx)
	(modify-frame-parameters ctl-frame adjusted-parameters))
      
    (goto-char (point-min))
    
    (cond ((and ediff-prefer-iconified-control-frame
		(not ctl-frame-iconified-p)
		(not dont-iconify-ctl-frame))
	   (iconify-frame ctl-frame))
	  ((or deiconify-ctl-frame
	       (not ctl-frame-iconified-p))
	   (raise-frame ctl-frame)))
    
    ;; This works around a bug in 19.25 and earlier. There, if frame gets
    ;; iconified, the current buffer changes to that of the frame that
    ;; becomes exposed as a result of this iconification.
    ;; So, we make sure the current buffer doesn't change.
    (select-frame ctl-frame)
    (ediff-refresh-control-frame)
    
    (modify-frame-parameters ctl-frame adjusted-parameters)
    
    (if ediff-xemacs-p
	(set-window-buffer-dedicated (selected-window) ctl-buffer)
      (set-window-dedicated-p (selected-window) t))
      
    (or ediff-xemacs-p (sit-for 0 200)) ; emacs has trouble here, needs time
    (or (ediff-frame-iconified-p ctl-frame)
	(ediff-reset-mouse ctl-frame))
    (or ediff-xemacs-p (unfocus-frame))
	
    (if ediff-xemacs-p
	(ediff-eval-in-buffer ctl-buffer
	  (make-local-variable 'select-frame-hook)
	  (add-hook 'select-frame-hook 'ediff-xemacs-select-frame-hook)
	  ))
	
    (ediff-eval-in-buffer ctl-buffer
      (run-hooks 'ediff-after-setup-control-frame-hooks))
    ))
    
(defun ediff-destroy-control-frame (ctl-buffer)
  (ediff-eval-in-buffer ctl-buffer
    (if (and (ediff-window-display-p) (frame-live-p ediff-control-frame))
	(let ((ctl-frame ediff-control-frame))
	  (if ediff-xemacs-p
	      (set-buffer-menubar default-menubar))
	  ;;(redraw-display)
	  (setq ediff-control-frame nil)
	  (delete-frame ctl-frame)
	  )))
  (ediff-skip-unsuitable-frames)
  (ediff-reset-mouse))
    

;; finds a good place to clip control frame
(defun ediff-make-frame-position (ctl-buffer ctl-frame-width ctl-frame-height)
  (ediff-eval-in-buffer ctl-buffer
    (let* ((frame-A (window-frame ediff-window-A))
	   (frame-A-parameters (frame-parameters frame-A))
	   (frame-A-top (cdr (assoc 'top frame-A-parameters)))
	   (frame-A-left (cdr (assoc 'left frame-A-parameters)))
	   (frame-A-width (frame-width frame-A))
	   (ctl-frame ediff-control-frame)
	   horizontal-adjustment upward-adjustment
	   ctl-frame-top) 
      
      ;; Multiple control frames are clipped based on the value of
      ;; ediff-control-buffer-number. This is done in order not to obscure
      ;; other active control panels.
      (setq horizontal-adjustment (* 2 ediff-control-buffer-number)
	    upward-adjustment (* -14 ediff-control-buffer-number))

      (setq ctl-frame-top (- frame-A-top
			     upward-adjustment
			     ediff-control-frame-upward-shift))
      (list
       (cons 'top (if (> ctl-frame-top 0) ctl-frame-top 1))
       (cons 'left (+ frame-A-left
		      (if ediff-prefer-long-help-message
			  (* (ediff-frame-char-width ctl-frame)
			     (+ ediff-wide-control-frame-rightward-shift
				horizontal-adjustment))
			(- (* frame-A-width
			      (ediff-frame-char-width frame-A))
			   (* (ediff-frame-char-width ctl-frame)
			      (+ ctl-frame-width
				 ediff-narrow-control-frame-leftward-shift
				 horizontal-adjustment))))))))))
			       
(defun ediff-xemacs-select-frame-hook ()
  (if (equal (selected-frame) ediff-control-frame)
      (raise-frame ediff-control-frame)))
	
(defun ediff-make-wide-display ()
  "Construct an alist of parameters for the wide display.
Saves the old frame parameters in `ediff-wide-display-orig-parameters'.
The frame to be resized is kept in `ediff-wide-display-frame'.
This function modifies only the left margin and the width of the display.
It assumes that it is called from within the control buffer."
  (if (not (fboundp 'ediff-display-pixel-width))
      (error "Can't determine display width."))
  (let* ((frame-A (window-frame ediff-window-A))
	 (frame-A-params (frame-parameters frame-A))
	 (cw (ediff-frame-char-width frame-A))
	 (wd (- (/ (ediff-display-pixel-width) cw) 5)))
    (setq ediff-wide-display-orig-parameters 
	  (list (cons 'left (max 0 (cdr (assoc 'left frame-A-params))))
		(cons 'width (cdr (assoc 'width frame-A-params))))
	  ediff-wide-display-frame frame-A)
    (modify-frame-parameters frame-A (list (cons 'left cw)
						 (cons 'width wd)))))
  
      

;; Revise the mode line to display which difference we have selected
;; Also resets modelines of buffers A/B, since they may be clobbered by
;; anothe invocations of Ediff.
(defun ediff-refresh-mode-lines ()
  (let (buf-A-state-diff buf-B-state-diff buf-C-state-diff buf-C-state-merge)
    
    (if (ediff-valid-difference-p)
	(setq
	 buf-C-state-diff (ediff-get-state-of-diff ediff-current-difference 'C)
	 buf-C-state-merge (ediff-get-state-of-merge ediff-current-difference)
	 buf-A-state-diff (ediff-get-state-of-diff ediff-current-difference 'A)
	 buf-B-state-diff (ediff-get-state-of-diff ediff-current-difference 'B)
	 buf-A-state-diff (if buf-A-state-diff
			      (format "[%s] " buf-A-state-diff)
			    "")
	 buf-B-state-diff (if buf-B-state-diff
			      (format "[%s] " buf-B-state-diff)
			    "")
	 buf-C-state-diff (if (and (ediff-buffer-live-p ediff-buffer-C)
				   (or buf-C-state-diff buf-C-state-merge))
			      (format "[%s%s] "
				      (or buf-C-state-diff "")
				      (if buf-C-state-merge
					  (concat " " buf-C-state-merge)
					""))
			    ""))
      (setq buf-A-state-diff ""
	    buf-B-state-diff ""
	    buf-C-state-diff ""))
    
    ;; control buffer format
    (setq mode-line-format
	  (list (if (ediff-narrow-control-frame-p) "   " "-- ")
		mode-line-buffer-identification
		"               Howdy!"))
    ;; control buffer id
    (setq mode-line-buffer-identification 
	  (if (ediff-narrow-control-frame-p)
	      (ediff-make-narrow-control-buffer-id 'skip-name)
	    (ediff-make-wide-control-buffer-id)))
    ;; Force mode-line redisplay
    (force-mode-line-update)
    
    (if (and (ediff-window-display-p) (frame-live-p ediff-control-frame))
	(ediff-refresh-control-frame))
    
    (ediff-eval-in-buffer ediff-buffer-A
      (setq ediff-diff-status buf-A-state-diff)
      (ediff-strip-mode-line-format)
      (setq mode-line-format
	    (list " A: " 'ediff-diff-status mode-line-format))
      (force-mode-line-update))
    (ediff-eval-in-buffer ediff-buffer-B
      (setq ediff-diff-status buf-B-state-diff)
      (ediff-strip-mode-line-format)
      (setq mode-line-format
	    (list " B: " 'ediff-diff-status mode-line-format))
      (force-mode-line-update))
    (if ediff-3way-job
	(ediff-eval-in-buffer ediff-buffer-C
	  (setq ediff-diff-status buf-C-state-diff)
	  (ediff-strip-mode-line-format)
	  (setq mode-line-format
		(list " C: " 'ediff-diff-status mode-line-format))
	  (force-mode-line-update)))
    ))
    
  
(defun ediff-refresh-control-frame ()
  (if ediff-xemacs-p
      (progn
	(setq frame-title-format (ediff-make-narrow-control-buffer-id)
	      frame-icon-title-format (ediff-make-narrow-control-buffer-id))
	;; this forces update of the frame title
	(modify-frame-parameters ediff-control-frame '(())))
    (modify-frame-parameters
     ediff-control-frame
     (list (cons 'name (ediff-make-narrow-control-buffer-id))))
     ))
   
  
(defun ediff-make-narrow-control-buffer-id (&optional skip-name)
  (concat
   (if skip-name
       " "
     (concat
      (cdr (assoc 'name ediff-control-frame-parameters))
      ediff-control-buffer-suffix))
   (cond ((< ediff-current-difference 0) 
	  (format " _/%d" ediff-number-of-differences))
	 ((>= ediff-current-difference ediff-number-of-differences)
	  (format " $/%d" ediff-number-of-differences))
	 (t
	  (format " %d/%d"
		  (1+ ediff-current-difference)
		  ediff-number-of-differences)))))
		  
(defun ediff-make-wide-control-buffer-id ()
  (cond ((< ediff-current-difference 0)
	 (list (format "%%b   At start of %d diffs"
		       ediff-number-of-differences)))
	((>= ediff-current-difference ediff-number-of-differences)
	 (list (format "%%b   At end of %d diffs"
		       ediff-number-of-differences)))
	(t
	 (list (format "%%b   diff %d of %d"
		       (1+ ediff-current-difference)
		       ediff-number-of-differences)))))



;; If buff is not live, return nil
(defun ediff-get-visible-buffer-window (buff)
  (if (ediff-buffer-live-p buff)
      (if ediff-xemacs-p
	  (get-buffer-window buff t)
	(get-buffer-window buff 'visible))))
	
;;; Functions to decide when to redraw windows

  
(defun ediff-keep-window-config (control-buf)
  (and (eq control-buf (current-buffer))
       (/= (buffer-size) 0)
       (ediff-eval-in-buffer control-buf
	 (let ((ctl-wind ediff-control-window)
	       (A-wind ediff-window-A)
	       (B-wind ediff-window-B)
	       (C-wind ediff-window-C))
	  
	   (and
	    (ediff-window-visible-p A-wind)
	    (ediff-window-visible-p B-wind)
	    ;; if buffer C is defined then take it into account
	    (or (not ediff-3way-job)
		(ediff-window-visible-p C-wind))
	    (eq (window-buffer A-wind) ediff-buffer-A)
	    (eq (window-buffer B-wind) ediff-buffer-B)
	    (or (not ediff-3way-job)
		(eq (window-buffer C-wind) ediff-buffer-C))
	    (string= ediff-window-config-saved
		     (format "%S%S%S%S%S%S%S"
			     ctl-wind A-wind B-wind C-wind
			     ediff-split-window-function
			     (ediff-multiframe-setup-p)
			     ediff-wide-display-p)))))))


(provide 'ediff-wind)


;;; ediff-wind.el ends here
