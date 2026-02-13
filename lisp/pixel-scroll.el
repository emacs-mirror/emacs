;;; pixel-scroll.el --- Scroll a line smoothly  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2026 Free Software Foundation, Inc.
;; Author: Tak Kunihiro <tkk@misasa.okayama-u.ac.jp>
;; Keywords: mouse
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

;; Usage:
;;
;; To interactively toggle the mode:
;;
;;   M-x pixel-scroll-mode RET
;;
;; To make the mode permanent, put this in your Init file:
;;
;;   (pixel-scroll-mode 1)

;;; Commentary:

;; This file contains two somewhat related features.

;; The first is a global minor mode which makes Emacs try to scroll
;; each line smoothly.
;;
;; Scrolling a line up by `set-window-vscroll' and that by `scroll-up'
;; give similar display as shown below.
;;
;;  A: (scroll-up 1)
;;  B: (set-window-vscroll nil (frame-char-height) t)
;;
;; Also scrolling a pixel up by `set-window-vscroll' and that by
;; `scroll-up' give similar display, when vscroll is the last pixel of
;; the line, as shown below.
;;
;;  A: (scroll-up 1)
;;  B: (set-window-vscroll nil (1- (frame-char-height) t)) (scroll-up 1)
;;
;; When point reaches to the top of a window on scroll by
;; `set-window-vscroll', vscroll is set to zero.  To scroll a line
;; smoothly and continuously, this package scrolls a line by following
;; sequences.
;;
;;  (vertical-motion 1)
;;  (dolist (vs (number-sequence 1 (1- (frame-char-height))))
;;    (set-window-vscroll nil vs t) (sit-for 0))
;;  (scroll-up 1)

;; The second is another global minor mode that redefines `wheel-up'
;; and `wheel-down' to a command that tries to scroll the display
;; according to the precise movement of a trackpad or mouse.

;; But it operates in a much more intelligent manner than simply
;; setting the vscroll.  It will set window start to the position
;; closest to the position at the top-left corner of the window if
;; vscroll were set accordingly, in a smart and fast manner, and only
;; set vscroll the rest of the way.  There is no visible difference,
;; but it is much faster, and doesn't move the display by a huge
;; portion if vscroll is reset for some reason.

;; It also tries to move point out of the way, so redisplay will not
;; recenter the display as it scrolls.  This works well almost all of
;; the time, but is impossible to get right with images larger than
;; the window they're displayed in.  A feature that will allow
;; redisplay to skip recentering is in the works, and will completely
;; resolve this problem.

;;; Todo:
;;
;; Allowing pixel-level scrolling in Emacs requires a thorough review
;; of the related functionalities, to make sure none of them zeroes
;; out vscroll where users won't want that.

;;; Code:

(require 'mwheel)
(require 'subr-x)
(require 'ring)

(defvar pixel-wait 0
  "Idle time on each step of pixel scroll specified in second.
More wait will result in slow and gentle scroll.")

(defvar pixel-resolution-fine-flag nil
  "Set scrolling resolution to pixels instead of a line.
When it is t, scrolling resolution is number of pixels obtained
by `frame-char-height' instead of a line.  When it is number,
scrolling resolution is set to number of pixels specified.  In
case you need scrolling resolution of a pixel, set to 1.  After a
pixel scroll, typing \\[next-line] or \\[previous-line] scrolls the window to make it
fully visible, and undoes the effect of the pixel-level scroll.")

(defvar pixel-dead-time 0.1
  "Minimal interval in seconds before next smooth scrolling.
If another scrolling request arrives within this period, scrolling
will be carried out without pixel resolution.  If zero, scrolling
is always with pixel resolution.")

(defvar pixel-last-scroll-time 0
  "Time when the last scrolling was made, in second since the epoch.")

(defvar mwheel-coalesce-scroll-events)

(defvar-keymap pixel-scroll-precision-mode-map
  :doc "The key map used by `pixel-scroll-precision-mode'."
  "<wheel-down>"                       #'pixel-scroll-precision
  "<wheel-up>"                         #'pixel-scroll-precision
  "<touch-end>"                        #'pixel-scroll-start-momentum
  "<mode-line> <wheel-down>"           #'pixel-scroll-precision
  "<mode-line> <wheel-up>"             #'pixel-scroll-precision
  "<mode-line> <touch-end>"            #'pixel-scroll-start-momentum
  "<header-line> <wheel-down>"         #'pixel-scroll-precision
  "<header-line> <wheel-up>"           #'pixel-scroll-precision
  "<header-line> <touch-end>"          #'pixel-scroll-start-momentum
  "<vertical-scroll-bar> <wheel-down>" #'pixel-scroll-precision
  "<vertical-scroll-bar> <wheel-up>"   #'pixel-scroll-precision
  "<vertical-scroll-bar> <touch-end>"  #'pixel-scroll-start-momentum
  "<tool-bar> <wheel-down>"            #'pixel-scroll-precision
  "<tool-bar> <wheel-up>"              #'pixel-scroll-precision
  "<tool-bar> <touch-end>"             #'pixel-scroll-start-momentum
  "<left-margin> <wheel-down>"         #'pixel-scroll-precision
  "<left-margin> <wheel-up>"           #'pixel-scroll-precision
  "<left-margin> <touch-end>"          #'pixel-scroll-start-momentum
  "<right-margin> <wheel-down>"        #'pixel-scroll-precision
  "<right-margin> <wheel-up>"          #'pixel-scroll-precision
  "<right-margin> <touch-end>"         #'pixel-scroll-start-momentum
  "<left-fringe> <wheel-down>"         #'pixel-scroll-precision
  "<left-fringe> <wheel-up>"           #'pixel-scroll-precision
  "<left-fringe> <touch-end>"          #'pixel-scroll-start-momentum
  "<right-fringe> <wheel-down>"        #'pixel-scroll-precision
  "<right-fringe> <wheel-up>"          #'pixel-scroll-precision
  "<right-fringe> <touch-end>"         #'pixel-scroll-start-momentum
  "<next>"                             #'pixel-scroll-interpolate-down
  "<prior>"                            #'pixel-scroll-interpolate-up)

(defgroup pixel-scroll-precision nil
  "Precise pixel scrolling."
  :group 'mouse
  :version "30.1")

(defcustom pixel-scroll-precision-use-momentum nil
  "If non-nil, continue to scroll the display after wheel movement stops.
This is only effective if supported by your mouse or touchpad."
  :type 'boolean
  :version "29.1")

(defcustom pixel-scroll-precision-momentum-tick 0.01
  "Number of seconds between each momentum scroll."
  :type 'number
  :version "29.1")

(defcustom pixel-scroll-precision-momentum-seconds 1.75
  "The maximum duration in seconds of momentum scrolling."
  :type 'number
  :version "29.1")

(defcustom pixel-scroll-precision-momentum-min-velocity 10.0
  "The minimum scrolled pixels per second before momentum scrolling starts."
  :type 'number
  :version "29.1")

(defcustom pixel-scroll-precision-initial-velocity-factor (/ 0.0335 4)
  "Factor applied to the initial velocity before momentum scrolling begins."
  :type 'number
  :version "29.1")

(defcustom pixel-scroll-precision-large-scroll-height nil
  "Pixels that must be scrolled before an animation is performed.
Nil means to not interpolate such scrolls."
  :type '(choice (const :tag "Do not interpolate large scrolls" nil)
                 number)
  :version "29.1")

(defcustom pixel-scroll-precision-interpolation-total-time 0.1
  "The total time in seconds to spend interpolating a large scroll."
  :type 'number
  :version "29.1")

(defcustom pixel-scroll-precision-interpolation-factor 2.0
  "A factor to apply to the distance of an interpolated scroll."
  :type 'number
  :version "29.1")

(defcustom pixel-scroll-precision-interpolation-between-scroll 0.001
  "The number of seconds between each step of an interpolated scroll."
  :type 'number
  :version "29.1")

(defcustom pixel-scroll-precision-interpolate-page nil
  "Whether or not to interpolate scrolling via the Page Down and Page Up keys.
This is only effective when `pixel-scroll-precision-mode' is enabled."
  :type 'boolean
  :version "29.1")

(defcustom pixel-scroll-precision-interpolate-mice t
  "Whether or not to interpolate scrolling from a mouse.
If non-nil, scrolling from the mouse wheel of an actual mouse (as
opposed to a touchpad) will cause Emacs to interpolate the scroll."
  :type 'boolean
  :version "29.1")

(defun pixel-scroll-in-rush-p ()
  "Return non-nil if next scroll should be non-smooth.
When scrolling request is delivered soon after the previous one,
user is in hurry.  When the time since last scroll is larger than
`pixel-dead-time', we are ready for another smooth scroll, and this
function returns nil."
  (let* ((now (current-time))
	 (scroll-in-rush-p (time-less-p
			    (time-subtract now pixel-last-scroll-time)
			    pixel-dead-time)))
    (setq pixel-last-scroll-time (float-time now))
    scroll-in-rush-p))

;;;###autoload
(define-minor-mode pixel-scroll-mode
  "A minor mode to scroll text pixel-by-pixel."
  :init-value nil
  :group 'scrolling
  :global t
  :version "26.1"

  (if pixel-scroll-mode
      (setq mwheel-scroll-up-function 'pixel-scroll-up
            mwheel-scroll-down-function 'pixel-scroll-down)
    (setq mwheel-scroll-up-function 'scroll-up
          mwheel-scroll-down-function 'scroll-down)))

(defun pixel-scroll-up (&optional arg)
  "Scroll text of selected window up ARG lines.
This is an alternative of `scroll-up'.  Scope moves downward."
  (interactive)
  (or arg (setq arg 1))
  (if (pixel-scroll-in-rush-p)
      (scroll-up arg)
    (dotimes (_ arg)                    ; move scope downward
      (let ((amt (if pixel-resolution-fine-flag
                     (if (integerp pixel-resolution-fine-flag)
                         pixel-resolution-fine-flag
                       (frame-char-height))
                   (pixel-line-height))))
        (if (pixel-eob-at-top-p)      ; when end-of-the-buffer is close
            (scroll-up 1)             ; relay on robust method
          (catch 'no-movement
            (while (pixel-point-at-top-p amt) ; prevent too late (multi tries)
              (unless (>= (vertical-motion 1) 1) ; move point downward
                (throw 'no-movement nil)))) ; exit loop when point did not move
          (pixel-scroll-pixel-up amt))))))  ; move scope downward

(defun pixel-scroll-down (&optional arg)
  "Scroll text of selected window down ARG lines.
This is and alternative of `scroll-down'.  Scope moves upward."
  (interactive)
  (or arg (setq arg 1))
  (if (pixel-scroll-in-rush-p)
      (scroll-down arg)
    (dotimes (_ arg)
      (let ((amt (if pixel-resolution-fine-flag
                     (if (integerp pixel-resolution-fine-flag)
                         pixel-resolution-fine-flag
                       (frame-char-height))
                   (pixel-line-height -1))))
        (catch 'no-movement
          (while (pixel-point-at-bottom-p amt) ; prevent too late (multi tries)
            (unless (<= (vertical-motion -1) -1) ; move point upward
              (throw 'no-movement nil)))) ; exit loop when point did not move
        (if (or (pixel-bob-at-top-p amt) ; when beginning-of-the-buffer is seen
                (pixel-eob-at-top-p))    ; for file with a long line
            (scroll-down 1)              ; relay on robust method
          (pixel-scroll-pixel-down amt))))))

;; isearch-scroll support
(put 'pixel-scroll-up 'scroll-command t)
(put 'pixel-scroll-down 'scroll-command t)

(defun pixel-bob-at-top-p (amt)
  "Return non-nil if `window-start' is at beginning of the current buffer.
Window must be vertically scrolled by not more than AMT pixels."
  (and (equal (window-start) (point-min))
       (< (window-vscroll nil t) amt)))

(defun pixel-eob-at-top-p ()
  "Return non-nil if end of buffer is at top of window."
  (<= (count-lines (window-start) (window-end)) 2)) ; count-screen-lines

(defun pixel-posn-y-at-point ()
  "Return y coordinates of point in pixels of current window.
This returns nil when horizontally scrolled."
  (when (equal (window-hscroll) 0)
    (save-excursion
      ;; When there's an overlay string on a line, move
      ;; point by (beginning-of-visual-line).
      (beginning-of-visual-line)
      ;; (- (cadr (pos-visible-in-window-p (point) nil t))
      ;;    (line-pixel-height))
      (cdr (posn-x-y (posn-at-point))))))

(defun pixel-point-at-top-p (amt)
  "Return if point is located at top of a window on coming scroll of AMT pixels.
When location of point was not obtained, this returns if point is at top
of window."
  (let ((y (pixel-posn-y-at-point))
        top-margin)
    (cond
     (y
      (setq top-margin y)
      (< top-margin amt))
     (t
      (<= (count-lines (window-start) (point)) 1)))))

(defun pixel-point-at-bottom-p (amt)
  "Return if point is located at bottom of window on coming scroll of AMT pixels.
When location of point was not obtained, this returns nil."
  (let* ((edges (window-inside-pixel-edges))
         (height (- (nth 3 edges) (nth 1 edges))) ; (- bottom top)
         (y (pixel-posn-y-at-point))
         bottom-margin)
    (when y
      (setq bottom-margin (- height (+ y (pixel-visual-line-height))))
      (< bottom-margin amt)))) ; coming unseen line

(defun pixel-scroll-pixel-up (amt)
  "Scroll text of selected windows up AMT pixels.
Scope moves downward."
  (while (>= (+ (window-vscroll nil t) amt)
             (pixel-line-height))
    (setq amt (- amt (pixel--whistlestop-line-up)))) ; major scroll
  (pixel--whistlestop-pixel-up amt)) ; minor scroll

(defun pixel-scroll-pixel-down (amt)
  "Scroll text of selected windows down AMT pixels.
Scope moves upward."
  (while (> amt 0)
    (let ((vs (window-vscroll nil t)))
      (if (equal vs 0)
          (progn
            ;; On horizontal scrolling, move cursor.
            (when (> (window-hscroll) 0)
              (vertical-motion -1))
            (pixel-scroll-down-and-set-window-vscroll
             (1- (pixel-line-height -1))))
        (set-window-vscroll nil (1- vs) t))
      (setq amt (1- amt))
      (sit-for pixel-wait))))

(defun pixel--whistlestop-line-up ()
  "Scroll text upward a line with each pixel whistlestopped.
When `vscroll' is non-zero, complete scrolling a line.  When
`vscroll' is larger than height of multiple lines, for example
88, this flushes multiple lines.  At the end, `vscroll' will be
zero.  This assumes that the lines are with the same height.
Scope moves downward.  This function returns number of pixels
that was scrolled."
  (let* ((src (window-vscroll nil t))  ; EXAMPLE (initial)      @0   @8  @88
         (height (pixel-line-height))  ;                        25   25   23
         (line (1+ (/ src height)))    ; catch up + one line     1    1    4
         (dst (* line height))         ; goal                  @25  @25  @92
         (delta (- dst src)))          ; pixels to be scrolled  25   17    4
    (pixel--whistlestop-pixel-up (1- delta)) ; until one less  @24  @24  @91
    (dotimes (_ line)
      ;; On horizontal scrolling, move cursor.
      (when (> (window-hscroll) 0)
        (vertical-motion 1))
      (scroll-up 1))
    (sit-for pixel-wait)               ; scroll 1 pixel         @0   @0   @0
    delta))

(defun pixel--whistlestop-pixel-up (n)
  "Scroll text upward by N pixels with each pixel whistlestopped.
Scope moves downward."
  (when (> n 0)
    (let ((vs0 (window-vscroll nil t)))
      (dolist (vs (number-sequence (1+ vs0) (+ vs0 n)))
        (set-window-vscroll nil vs t) (sit-for pixel-wait)))))

(defun pixel-line-height (&optional pos)
  "Return height in pixels of text line at POS in the selected window.
When POS is nil or negative, height of the first line or the coming
unseen line above the first line, respectively, is provided."
  (or pos (setq pos (window-start)))
  (when (< pos 0)
    (setq pos (pixel-point-at-unseen-line)))
  (let ((vs1 (window-vscroll nil t))
        height)
    (set-window-vscroll nil 0 t)
    (save-excursion
      (goto-char pos)
      (setq height (pixel-visual-line-height))) ; line-pixel-height, frame-char-height
    (set-window-vscroll nil vs1 t)
    height))

(defun pixel-visual-line-height ()
  "Return height in pixels of text line where cursor is in the selected window."
  (let ((pos (pixel-visible-pos-in-window)))
    (cond
     ;; When a char of line is shown, obtain height by
     ;; (line-pixel-height).
     (pos (save-excursion (goto-char pos) (line-pixel-height)))
     ;; When no char of line is shown but the line is at the top,
     ;; obtain height by (line-pixel-height).  This is based on
     ;; expected response from display engine.  See following
     ;; discussion.
     ;; https://lists.gnu.org/r/emacs-devel/2017-10/msg00621.html
     ((equal (count-lines (window-start) (point)) 1)
      (line-pixel-height))
     ;; No char of line is shown and the line is not at the top,
     ;; obtain height by (frame-char-height).
     (t (frame-char-height)))))

(defun pixel-visible-pos-in-window ()
  "Return position shown on text line where cursor is in the selected window.
This will look for positions of point and `end-of-visual-line',
then positions from `beginning-of-visual-line' to
`end-of-visual-line'.  When no char in a line is shown, this
returns nil."
  (let* ((beginning-of-visual-line-pos (save-excursion (beginning-of-visual-line) (point)))
         (end-of-visual-line-pos (save-excursion (end-of-visual-line) (point)))
         (pos-list (number-sequence beginning-of-visual-line-pos end-of-visual-line-pos))
         (edges (window-inside-pixel-edges))
         (width (- (nth 2 edges) (nth 0 edges)))
         posn-x
         visible-pos)
    ;; Optimize list of position to be surveyed.
    (push end-of-visual-line-pos pos-list)
    (push (point) pos-list)
    (delete-dups pos-list)
    ;; Find out a char with position X that is more than zero and less
    ;; than width of screen.
    (while (and (not visible-pos)
                pos-list)
      (setq posn-x (car (pos-visible-in-window-p (car pos-list) nil t)))
      (if (and posn-x
               (<= 0 posn-x)
               (< posn-x width))
          (setq visible-pos (car pos-list))
        (setq pos-list (cdr pos-list))))
    visible-pos))

(defun pixel-point-and-height-at-unseen-line ()
  "Return the position and pixel height of line above the selected window.
The returned value is a cons of the position of the first
character on the unseen line just above the scope of current
window, and the pixel height of that line."
  (let* ((pos0 (save-excursion
                 (goto-char (window-start))
                 (unless (bobp)
                   (beginning-of-visual-line))
                 (point)))
         (vscroll0 (window-vscroll nil t))
         (line-height nil)
         (pos
          (save-excursion
            (goto-char pos0)
            (if (bobp)
                (point-min)
              (vertical-motion -1)
              (setq line-height
                    (cdr (window-text-pixel-size nil (point) pos0)))
              (point)))))
    ;; restore initial position
    (set-window-start nil pos0 t)
    (set-window-vscroll nil vscroll0 t)
    (when (and line-height
               (> (car (posn-x-y (posn-at-point pos0)))
                  (line-number-display-width t)))
      (setq line-height (- line-height
                           (save-excursion
                             (goto-char pos0)
                             (line-pixel-height)))))
    (cons pos line-height)))

(defun pixel-point-at-unseen-line ()
  "Return the character position of line above the selected window.
The returned value is the position of the first character on the
unseen line just above the scope of current window."
  (car (pixel-point-and-height-at-unseen-line)))

(defun pixel-scroll-down-and-set-window-vscroll (vscroll)
  "Scroll down a line and set VSCROLL in pixels.
It is important to call `set-window-start' to force the display engine
to use that particular position as the `window-start' point.
Otherwise, redisplay will reset the window's vscroll."
  (set-window-start nil (pixel-point-at-unseen-line) t)
  (set-window-vscroll nil vscroll t))

;;;###autoload
(defun pixel-scroll-precision-scroll-down-page (delta)
  "Scroll the current window down by DELTA pixels.
Note that this function doesn't work if DELTA is larger than or
equal to the text height of the current window in pixels."
  (let* ((desired-pos (posn-at-x-y 0 (+ delta
					(window-tab-line-height)
					(window-header-line-height))))
	 (desired-start (posn-point desired-pos))
         (current-vs (window-vscroll nil t))
         (start-posn (unless (eq desired-start (window-start))
                       (posn-at-point desired-start)))
	 (desired-vscroll (if start-posn
                              (- delta (cdr (posn-x-y start-posn)))
                            (+ current-vs delta)))
         (scroll-preserve-screen-position nil)
         (auto-window-vscroll nil)
         (new-start-position (if (zerop (window-hscroll))
                                 desired-start
                               (save-excursion
                                 (goto-char desired-start)
                                 (beginning-of-visual-line)
                                 (point)))))
    (set-window-start nil new-start-position
                      (not (zerop desired-vscroll)))
    (set-window-vscroll nil desired-vscroll t t)
    ;; Constrain point to a location that will not result in
    ;; recentering, if it is no longer completely visible.
    (unless (pos-visible-in-window-p (point))
      ;; If desired-vscroll is 0, target the window start itself.  But
      ;; in any other case, target the line immediately below the
      ;; window start, unless that line is itself invisible.  This
      ;; improves the appearance of the window by maintaining the
      ;; cursor row in a fully visible state.
      (if (zerop desired-vscroll)
          (goto-char new-start-position)
        (let ((line-after (save-excursion
                            (goto-char new-start-position)
                            (if (zerop (vertical-motion 1))
                                (progn
                                  (set-window-vscroll nil 0 t t)
                                  nil) ; nil means move to new-start-position.
                              (point)))))
          (if (not line-after)
              (progn
                (goto-char new-start-position)
                (signal 'end-of-buffer nil))
            (if (pos-visible-in-window-p line-after nil t)
                (goto-char line-after)
              (goto-char new-start-position))))))))

(defun pixel-scroll-precision-scroll-down (delta)
  "Scroll the current window down by DELTA pixels."
  (let ((max-height (1- (window-text-height nil t))))
    (while (> delta max-height)
      (pixel-scroll-precision-scroll-down-page max-height)
      (setq delta (- delta max-height)))
    (pixel-scroll-precision-scroll-down-page delta)))

;;;###autoload
(defun pixel-scroll-precision-scroll-up-page (delta)
  "Scroll the current window up by DELTA pixels.
Note that this function doesn't work if DELTA is larger than
the height of the current window."
  (let* ((edges (window-edges nil t nil t))
         (max-y (- (nth 3 edges)
                   (nth 1 edges)))
         (posn (posn-at-x-y 0 (+ (window-tab-line-height)
                                 (window-header-line-height)
                                 (- max-y delta))))
         (point (posn-point posn)))
    (let ((current-vscroll (window-vscroll nil t))
          (wanted-pos (window-start)))
      (setq delta (- delta current-vscroll))
      (set-window-vscroll nil 0 t t)
      (when (> delta 0)
        (let* ((start (window-start))
               (dims (window-text-pixel-size nil (cons start (- delta))
                                             start nil nil nil t))
               (height (nth 1 dims))
               (position (nth 2 dims)))
          (setq wanted-pos position)
          (when (or (not position) (eq position start))
            (signal 'beginning-of-buffer nil))
          (setq delta (- delta height))))
      (set-window-start nil wanted-pos
                        (not (zerop delta)))
      (when (< delta 0)
        (set-window-vscroll nil (- delta) t t))
      ;; vscroll and the window start are now set.  Move point to a
      ;; position where redisplay will not recenter, if it is now
      ;; outside the window.
      (unless (pos-visible-in-window-p (point))
        (let ((up-pos (save-excursion
                        (goto-char point)
                        (vertical-motion -1)
                        (point))))
          (if (pos-visible-in-window-p up-pos nil t)
              (goto-char up-pos)
            (goto-char (window-start))))))))

(defun pixel-scroll-precision-interpolate (delta &optional old-window factor)
  "Interpolate a scroll of DELTA pixels.
OLD-WINDOW is the window which will be selected when redisplay
takes place, or nil for the current window.  This results in the
window being scrolled by DELTA pixels with an animation.  FACTOR
is a scale by which DELTA will be modified.  If nil, it defaults
to `pixel-scroll-precision-interpolation-factor'."
  (let ((percentage 0)
        (total-time pixel-scroll-precision-interpolation-total-time)
        (factor (or factor pixel-scroll-precision-interpolation-factor))
        (last-time (float-time))
        (time-elapsed 0)
        (between-scroll pixel-scroll-precision-interpolation-between-scroll)
        (rem (window-parameter nil 'interpolated-scroll-remainder))
        (time (window-parameter nil 'interpolated-scroll-remainder-time))
        (last-delta 0))
    (unless (or (not rem) (eq (< delta 0) (< rem 0)))
      ;; The direction changed.  Clear the remainder.
      (setq rem nil))
    (when (and rem time
               (< (- (float-time) time) 1.0)
               (eq (< delta 0) (< rem 0)))
      (setq delta (+ delta rem)))
    (if (or (null rem)
            (eq (< delta 0) (< rem 0)))
        (while-no-input
          (unwind-protect
              (while (< percentage 1)
                (with-selected-window (or old-window
                                          (selected-window))
                  (redisplay t))
                (sleep-for between-scroll)
                (let ((time (float-time)))
                  (setq time-elapsed (+ time-elapsed
                                        (- time last-time))
                        percentage (/ time-elapsed total-time))
                  (let* ((throw-on-input nil)
                         (absolute-delta (* (min 1 percentage) delta factor))
                         (relative-delta (abs
                                          (round (- absolute-delta last-delta)))))
                    (setq last-delta absolute-delta)
                    (if (< delta 0)
                        (pixel-scroll-precision-scroll-down relative-delta)
                      (pixel-scroll-precision-scroll-up relative-delta)))
                  (setq last-time time)))
            (if (< percentage 1)
                (progn
                  (set-window-parameter nil 'interpolated-scroll-remainder
                                        (* delta (- 1 percentage)))
                  (set-window-parameter nil 'interpolated-scroll-remainder-time
                                        (float-time)))
              (set-window-parameter nil
                                    'interpolated-scroll-remainder
                                    nil)
              (set-window-parameter nil
                                    'interpolated-scroll-remainder-time
                                    nil))))
      (set-window-parameter nil
                            'interpolated-scroll-remainder
                            nil)
      (set-window-parameter nil
                            'interpolated-scroll-remainder-time
                            nil))))

(defun pixel-scroll-precision-scroll-up (delta)
  "Scroll the current window up by DELTA pixels."
  (let ((max-height (window-text-height nil t)))
    (when (> max-height 0)
      (while (> delta max-height)
        (pixel-scroll-precision-scroll-up-page max-height)
        (setq delta (- delta max-height)))
      (pixel-scroll-precision-scroll-up-page delta))))

;; FIXME: This doesn't _always_ work when there's an image above the
;; current line that is taller than the window, and scrolling can
;; sometimes be jumpy in that case.
(defun pixel-scroll-precision (event)
  "Scroll the display vertically by pixels according to EVENT.
Move the display up or down by the pixel deltas in EVENT to
scroll the display according to the user's turning the mouse
wheel."
  (interactive "e")
  (let ((window (mwheel-event-window event))
        (current-window (selected-window)))
    (when (framep window)
      (setq window (frame-selected-window window)))
    (if (and (nth 4 event))
        (let ((delta (round (cdr (nth 4 event)))))
          (unless (zerop delta)
            (if (> (abs delta) (window-text-height window t))
                (mwheel-scroll event nil)
              (with-selected-window window
                (if (or (and pixel-scroll-precision-interpolate-mice
                             (eq (device-class last-event-frame
                                               last-event-device)
                                 'mouse))
                        (and pixel-scroll-precision-large-scroll-height
                             (> (abs delta)
                                pixel-scroll-precision-large-scroll-height)
                             (let* ((kin-state (pixel-scroll-kinetic-state))
                                    (ring (aref kin-state 0))
                                    (time (aref kin-state 1)))
                               (or (null time)
                                   (> (- (float-time) time) 1.0)
                                   (and (consp ring)
                                        (ring-empty-p ring))))))
                    (progn
                      (let ((kin-state (pixel-scroll-kinetic-state)))
                        (aset kin-state 0 (make-ring 30))
                        (aset kin-state 1 nil))
                      (pixel-scroll-precision-interpolate delta current-window))
                  (condition-case nil
                      (progn
                        (if (< delta 0)
	                    (pixel-scroll-precision-scroll-down (- delta))
                          (pixel-scroll-precision-scroll-up delta))
                        (pixel-scroll-accumulate-velocity delta))
                    ;; Do not ding at buffer limits.  Show a message instead.
                    (beginning-of-buffer
                     (message (error-message-string '(beginning-of-buffer))))
                    (end-of-buffer
                     (message (error-message-string '(end-of-buffer))))))))))
      (mwheel-scroll event nil))))

;; isearch-scroll support
(put 'pixel-scroll-precision 'scroll-command t)

(defun pixel-scroll-kinetic-state (&optional window)
  "Return the kinetic scroll state of WINDOW.
If WINDOW is nil, return the state of the current window.
It is a vector of the form [ VELOCITY TIME SIGN ]."
  (or (window-parameter window 'kinetic-state)
      (set-window-parameter window 'kinetic-state
                            (vector (make-ring 30) nil nil))))

(defun pixel-scroll-accumulate-velocity (delta)
  "Accumulate DELTA into the current window's kinetic scroll state."
  (let* ((state (pixel-scroll-kinetic-state))
         (ring (aref state 0))
         (time (aref state 1)))
    (when (or (and time (> (- (float-time) time) 0.5))
              (and (not (ring-empty-p ring))
                   (not (eq (< delta 0)
                            (aref state 2)))))
      (aset state 0 (make-ring 30)))
    (aset state 2 (< delta 0))
    (ring-insert (aref state 0)
                 (cons (aset state 1 (float-time))
                       delta))))

(defun pixel-scroll-calculate-velocity (state)
  "Calculate velocity from the kinetic state vector STATE."
  (let* ((ring (aref state 0))
         (elts (ring-elements ring))
         (total 0))
    (dolist (tem elts)
      (setq total (+ total (cdr tem))))
    (* (/ total (- (float-time) (caar (last elts))))
       pixel-scroll-precision-initial-velocity-factor)))

(defun pixel-scroll-start-momentum (event)
  "Start kinetic scrolling for the touch event EVENT."
  (interactive "e")
  (when pixel-scroll-precision-use-momentum
    (let ((window (mwheel-event-window event))
          ;; The animations are smoother if the GC threshold is
          ;; reduced for the duration of the animation.
          (gc-cons-threshold (min most-positive-fixnum
                                  (* gc-cons-threshold 3)))
          (state nil))
      (when (framep window)
        (setq window (frame-selected-window window)))
      (setq state (pixel-scroll-kinetic-state window))
      (when (and (aref state 1)
                 (listp (aref state 0)))
        (condition-case nil
            (while-no-input
              (unwind-protect
                  (progn
                    (aset state 0 (pixel-scroll-calculate-velocity state))
                    (when (> (abs (aref state 0))
                             pixel-scroll-precision-momentum-min-velocity)
                      (let* ((velocity (aref state 0))
                             (original-velocity velocity)
                             (time-spent 0))
                        (if (> velocity 0)
                            (while (and (> velocity 0)
                                        (<= time-spent
                                            pixel-scroll-precision-momentum-seconds))
                              (when (> (round velocity) 0)
                                (with-selected-window window
                                  (pixel-scroll-precision-scroll-up (round velocity))))
                              (setq velocity (- velocity
                                                (/ original-velocity
                                                   (/ pixel-scroll-precision-momentum-seconds
                                                      pixel-scroll-precision-momentum-tick))))
                              (sit-for pixel-scroll-precision-momentum-tick)
                              (setq time-spent (+ time-spent
                                                  pixel-scroll-precision-momentum-tick))))
                        (while (and (< velocity 0)
                                    (<= time-spent
                                        pixel-scroll-precision-momentum-seconds))
                          (when (> (round (abs velocity)) 0)
                            (with-selected-window window
                              (pixel-scroll-precision-scroll-down (round
                                                                   (abs velocity)))))
                          (setq velocity (+ velocity
                                            (/ (abs original-velocity)
                                               (/ pixel-scroll-precision-momentum-seconds
                                                  pixel-scroll-precision-momentum-tick))))
                          (redisplay t)
                          (sit-for pixel-scroll-precision-momentum-tick)
                          (setq time-spent (+ time-spent
                                              pixel-scroll-precision-momentum-tick))))))
                (aset state 0 (make-ring 30))
                (aset state 1 nil)))
          (beginning-of-buffer
           (message (error-message-string '(beginning-of-buffer))))
          (end-of-buffer
           (message (error-message-string '(end-of-buffer)))))))))

;;;###autoload
(defun pixel-scroll-interpolate-down ()
  "Interpolate a scroll downwards by one page."
  (interactive)
  (if pixel-scroll-precision-interpolate-page
      (pixel-scroll-precision-interpolate (- (window-text-height nil t))
                                          ;; Don't use an
                                          ;; interpolation factor,
                                          ;; since we want exactly 1
                                          ;; page to be scrolled.
                                          nil 1)
    (cond
     ((eobp)
      (scroll-up))  ; signal error
     (t
      (condition-case nil
	  (scroll-up)
        (end-of-buffer (goto-char (point-max))))))))

;;;###autoload
(defun pixel-scroll-interpolate-up ()
  "Interpolate a scroll upwards by one page."
  (interactive)
  (if pixel-scroll-precision-interpolate-page
      (pixel-scroll-precision-interpolate (window-text-height nil t)
                                          nil 1)
    (cond
     ((bobp)
      (scroll-down))  ; signal error
     (t
      (condition-case nil
	  (scroll-down)
        (beginning-of-buffer (goto-char (point-min))))))))

;;;###autoload
(define-minor-mode pixel-scroll-precision-mode
  "Toggle pixel scrolling.
When enabled, this minor mode allows you to scroll the display
precisely, according to the turning of the mouse wheel."
  :global t
  :group 'mouse
  :keymap pixel-scroll-precision-mode-map
  (setq mwheel-coalesce-scroll-events
        (not pixel-scroll-precision-mode))
  ;; This works around some issues described in bug#65214.
  ;; Ideally this would not be needed because it breaks some other things.
  (setq-default make-cursor-line-fully-visible
                (not pixel-scroll-precision-mode)))

(provide 'pixel-scroll)
;;; pixel-scroll.el ends here
