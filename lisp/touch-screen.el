;;; touch-screen.el --- touch screen support for X and Android  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
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

;; This file provides code to recognize simple touch screen gestures.
;; It is used on X, PGTK, Android, and MS-Windows, currently the only
;; systems where Emacs supports touch input.
;;
;; See (elisp)Touchscreen Events for a description of the details of
;; touch events.

;;; Code:

(defvar touch-screen-current-tool nil
  "The touch point currently being tracked, or nil.
If non-nil, this is a list of ten elements, which might be
accessed as follows:

  (nth 0 touch-screen-current-tool)
  The ID of the touch point being tracked.

  (nth 1 touch-screen-current-tool)
  The window where the touch sequence being monitored commenced.

  (nth 2 touch-screen-current-tool)
  A cons holding the last registered position of the touch
  point, relative to that window.

  (nth 3 touch-screen-current-tool)
  A field holding a symbol identifying the gesture being
  observed while tracking the said touch point.

  (nth 4 touch-screen-current-tool)
  The initial position of the touchpoint.

  (nth 5 touch-screen-current-tool)
  (nth 6 touch-screen-current-tool)
  (nth 7 touch-screen-current-tool)
  (nth 8 touch-screen-current-tool)
  A further four fields to used store data while tracking the
  touch point.

  (nth 9 touch-screen-current-tool)
  The last known position of the touch point, relative to the
  window in the second element.

  (nth 10 touch-screen-current-tool)
  The same position, relative to the frame to which the window
  in the second element belongs.


See `touch-screen-handle-point-update' and
`touch-screen-handle-point-up' for the meanings of the fourth
element.

The third and last elements differ in that the former is not
modified until after a gesture is recognized in reaction to an
update, whereas the latter is updated upon each apposite
`touchscreen-update' event.")

(defvar touch-screen-aux-tool nil
  "The ancillary tool being tracked, or nil.
If non-nil, this is a vector of ten elements: the ID of the
touch point being tracked, the window where the touch began, a
cons holding the initial position of the touch point, and the
last known position of the touch point, all in the same format as
in `touch-screen-current-tool', the distance in pixels between
the current tool and the aforementioned initial position, the
center of the line formed between those two points, the ratio
between the present distance between both tools and the aforesaid
initial distance when a pinch gesture was last sent, and three
elements into which commands can save data particular to a tool.

The ancillary tool is a second tool whose movement is interpreted
in unison with that of the current tool to recognize gestures
comprising the motion of both such as \"pinch\" gestures, in
which the text scale is adjusted in proportion to the distance
between both tools.")

(defvar touch-screen-set-point-commands '(mouse-set-point)
  "List of commands known to set the point.
This is used to determine whether or not to display the on-screen
keyboard after a mouse command is executed in response to a
`touchscreen-end' event.")

(defvar touch-screen-current-timer nil
  "Timer used to track long-presses.
This is always cleared upon any significant state change.")

(defvar touch-screen-translate-prompt nil
  "Prompt given to the touch screen translation function.
If non-nil, the touch screen key event translation machinery
is being called from `read-sequence' or some similar function.")

(defgroup touch-screen nil
  "Interact with Emacs from touch screen devices."
  :group 'mouse
  :version "30.1")

(defcustom touch-screen-display-keyboard nil
  "If non-nil, always display the on screen keyboard.
A buffer local value means to always display the on screen
keyboard when the buffer is selected."
  :type 'boolean
  :group 'touch-screen
  :version "30.1")

(defcustom touch-screen-delay 0.7
  "Delay in seconds before Emacs considers a touch to be a long-press."
  :type 'number
  :group 'touch-screen
  :version "30.1")

(defcustom touch-screen-precision-scroll nil
  "Whether or not to use precision scrolling for touch screens.
See `pixel-scroll-precision-mode' for more details."
  :type 'boolean
  :group 'touch-screen
  :version "30.1")

(defcustom touch-screen-word-select nil
  "Whether or not to select whole words while dragging to select.
If non-nil, long-press events (see `touch-screen-delay') followed
by dragging will try to select entire words."
  :type 'boolean
  :group 'touch-screen
  :version "30.1")

(defcustom touch-screen-extend-selection nil
  "If non-nil, restart drag-to-select upon a tap on point or mark.
When enabled, tapping on the character containing the point or
mark will resume dragging where it left off while the region is
active."
  :type 'boolean
  :group 'touch-screen
  :version "30.1")

(defcustom touch-screen-preview-select nil
  "If non-nil, display a preview while selecting text.
When enabled, a preview of the visible line within the window
will be displayed in the echo area while dragging combined with
an indication of the position of point within that line."
  :type 'boolean
  :group 'touch-screen
  :version "30.1")

(defcustom touch-screen-enable-hscroll t
  "If non-nil, hscroll can be changed from the touch screen.
When enabled, tapping on a window and dragging your finger left
or right will scroll that window horizontally."
  :type 'boolean
  :group 'touch-screen
  :version "30.1")

(defvar-local touch-screen-word-select-bounds nil
  "The start and end positions of the word last selected.
Normally a cons of those two positions or nil if no word was
selected.")

(defvar-local touch-screen-word-select-initial-word nil
  "The start and end positions of the first word to be selected.
Used in an attempt to keep this word selected during later
dragging.")

;; Should this variable be documented?
(defvar-local touch-screen-keyboard-function nil
  "Function that decides whether to display the on screen keyboard.
If set, this function is called with point set to the position
of the tap involved when a command listed in
`touch-screen-set-point-commands' is about to be invoked in
response to a tap, the current buffer, or the text beneath
point (in the case of an `inhibit-read-only' text property), is
not read only, and `touch-screen-display-keyboard' is nil, and
should return non-nil if it is appropriate to display the
on-screen keyboard afterwards.")

(defvar touch-screen-simple-mouse-conversion nil
  "Whether to unconditionally enable simple mouse event translation.
If non-nil, touch screen event conversion will always proceed as
though a command was bound to `down-mouse-1' at the position of
the initial tap.  That is to say, taps, mouse motion, and
touchpoint removals will be unconditionally converted into
mouse-down, mouse motion, mouse drag, and mouse button events.")



;;; Scroll gesture.

(defun touch-screen-relative-xy (posn window)
  "Return the coordinates of POSN, a mouse position list, relative to WINDOW.

If (posn-window POSN) is the same as window, simply return the
coordinates in POSN.  Otherwise, translate these coordinates
into that window's frame's coordinate system and from there into
that of WINDOW.

If WINDOW is the symbol `frame', just convert these coordinates
to the coordinate system of the frame containing POSN or its
window."
  (if (or (eq (posn-window posn) window)
          (and (eq window 'frame)
               (framep (posn-window posn))))
      (posn-x-y posn)
    (let ((xy (posn-x-y posn))
          (edges (and (windowp window)
                      (window-inside-pixel-edges window))))
      ;; Make the X and Y positions frame relative.
      (when (windowp (posn-window posn))
        (let ((edges (window-inside-pixel-edges
                      (posn-window posn))))
          (setq xy (cons (+ (car xy) (car edges))
                         (+ (cdr xy) (cadr edges))))))
      (if (eq window 'frame)
          xy
        ;; Make the X and Y positions window relative again.
        (cons (- (car xy) (car edges))
              (- (cdr xy) (cadr edges)))))))

(defun touch-screen-handle-scroll (dx dy)
  "Scroll the display assuming that a touch point has moved by DX and DY.
Perform vertical scrolling by DY, using `pixel-scroll-precision'
if `touch-screen-precision-scroll' is enabled.  Next, perform
horizontal scrolling according to the movement in DX."
  ;; Perform vertical scrolling first.  Do not ding at buffer limits.
  ;; Show a message instead.
  (condition-case nil
      (if touch-screen-precision-scroll
          (progn
            (if (> dy 0)
                (pixel-scroll-precision-scroll-down-page dy)
              (pixel-scroll-precision-scroll-up-page (- dy)))
            ;; Now set `lines-vscrolled' to an value that will result
            ;; in hscroll being disabled if dy looks as if a
            ;; significant amount of scrolling is about to take
            ;; Otherwise, horizontal scrolling may then interfere with
            ;; precision scrolling.
            (when (> (abs dy) 10)
              (setcar (nthcdr 7 touch-screen-current-tool) 10)))
        ;; Start conventional scrolling.  First, determine the
        ;; direction in which the scrolling is taking place.  Load the
        ;; accumulator value.
        (let ((accumulator (or (nth 5 touch-screen-current-tool) 0))
              (window (cadr touch-screen-current-tool))
              (lines-vscrolled (or (nth 7 touch-screen-current-tool) 0)))
          (setq accumulator (+ accumulator dy)) ; Add dy.
          ;; Figure out how much it has scrolled and how much remains
          ;; on the top or bottom of the window.
          (while (catch 'again
                   (let* ((line-height (window-default-line-height window)))
                     (if (and (< accumulator 0)
                              (>= (- accumulator) line-height))
                         (progn
                           (setq accumulator (+ accumulator line-height))
                           (scroll-down 1)
                           (setq lines-vscrolled (1+ lines-vscrolled))
                           (when (not (zerop accumulator))
                             ;; If there is still an outstanding
                             ;; amount to scroll, do this again.
                             (throw 'again t)))
                       (when (and (> accumulator 0)
                                  (>= accumulator line-height))
                         (setq accumulator (- accumulator line-height))
                         (scroll-up 1)
                         (setq lines-vscrolled (1+ lines-vscrolled))
                         (when (not (zerop accumulator))
                           ;; If there is still an outstanding amount
                           ;; to scroll, do this again.
                           (throw 'again t)))))
                   ;; Scrolling is done.  Move the accumulator back to
                   ;; touch-screen-current-tool and break out of the
                   ;; loop.
                   (setcar (nthcdr 5 touch-screen-current-tool) accumulator)
                   (setcar (nthcdr 7 touch-screen-current-tool)
                           lines-vscrolled)
                   nil))))
    (beginning-of-buffer
     (message (error-message-string '(beginning-of-buffer))))
    (end-of-buffer
     (message (error-message-string '(end-of-buffer)))))

  ;; Perform horizontal scrolling by DX, as this does not signal at
  ;; the beginning of the buffer.
  (let ((accumulator (or (nth 6 touch-screen-current-tool) 0))
        (window (cadr touch-screen-current-tool))
        (lines-vscrolled (or (nth 7 touch-screen-current-tool) 0))
        (lines-hscrolled (or (nth 8 touch-screen-current-tool) 0)))
    (setq accumulator (+ accumulator dx)) ; Add dx.
    ;; Figure out how much it has scrolled and how much remains on the
    ;; left or right of the window.  If a line has already been
    ;; vscrolled but no hscrolling has happened, don't hscroll, as
    ;; otherwise it is too easy to hscroll by accident.
    (if (or (> lines-hscrolled 0)
            (< lines-vscrolled 1))
        (while (catch 'again
                 (let* ((column-width (frame-char-width (window-frame window))))
                   (if (and (< accumulator 0)
                            (>= (- accumulator) column-width))
                       (progn
                         (setq accumulator (+ accumulator column-width))
                         ;; Maintain both hscroll counters even when
                         ;; it's disabled to prevent unintentional or
                         ;; patently horizontal gestures from
                         ;; scrolling the window vertically.
                         (when touch-screen-enable-hscroll
                           (scroll-right 1))
                         (setq lines-hscrolled (1+ lines-hscrolled))
                         (when (not (zerop accumulator))
                           ;; If there is still an outstanding amount
                           ;; to scroll, do this again.
                           (throw 'again t)))
                     (when (and (> accumulator 0)
                                (>= accumulator column-width))
                       (setq accumulator (- accumulator column-width))
                       (when touch-screen-enable-hscroll
                         (scroll-left 1))
                       (setq lines-hscrolled (1+ lines-hscrolled))
                       (when (not (zerop accumulator))
                         ;; If there is still an outstanding amount to
                         ;; scroll, do this again.
                         (throw 'again t)))))
                 ;; Scrolling is done.  Move the accumulator back to
                 ;; touch-screen-current-tool and break out of the
                 ;; loop.
                 (setcar (nthcdr 6 touch-screen-current-tool) accumulator)
                 (setcar (nthcdr 8 touch-screen-current-tool) lines-hscrolled)
                 nil)))))

(defun touch-screen-scroll (event)
  "Scroll the window within EVENT, a `touchscreen-scroll' event.
If `touch-screen-precision-scroll', scroll the window vertically
by the number of pixels specified within that event.  Else,
scroll the window by one line for every
`window-default-line-height' pixels worth of movement.

If EVENT also specifies horizontal motion and no significant
amount of vertical scrolling has taken place, also scroll the
window horizontally in conjunction with the number of pixels in
the event."
  (interactive "e")
  (let ((window (nth 1 event))
        (dx (nth 2 event))
        (dy (nth 3 event)))
    (with-selected-window window
      (touch-screen-handle-scroll dx dy))))

(global-set-key [touchscreen-scroll] #'touch-screen-scroll)



;;; Drag-to-select gesture.

;;;###autoload
(defun touch-screen-hold (event)
  "Handle a long press EVENT.
Ding and select the window at EVENT, then activate the mark.  If
`touch-screen-word-select' is enabled, try to select the whole
word around EVENT; otherwise, set point to the location of EVENT."
  (interactive "e")
  (let* ((posn (cadr event))
         (point (posn-point posn))
         (window (posn-window posn)))
    (when (and point
               ;; Make sure WINDOW is not an inactive minibuffer
               ;; window.
               (or (not (eq window
                            (minibuffer-window
                             (window-frame window))))
                   (minibuffer-window-active-p window)))
      (beep)
      (select-window window)
      (if (or (not touch-screen-word-select)
              (when-let* ((char (char-after point))
                          (class (char-syntax char)))
                ;; Don't select words if point isn't inside a word
                ;; constituent or similar.
                (not (or (eq class ?w) (eq class ?_)))))
          (progn
            ;; Set the mark and activate it.
            (setq touch-screen-word-select-initial-word nil
                  touch-screen-word-select-bounds nil)
            (push-mark point)
            (goto-char point)
            (activate-mark)
            (setq deactivate-mark nil))
        ;; Start word selection by trying to obtain the position
        ;; around point.
        (let ((word-start nil)
              (word-end nil))
          (unless (posn-object posn)
            ;; If there's an object under POSN avoid trying to
            ;; ascertain the bounds of the word surrounding it.
            (save-excursion
              (goto-char point)
              (forward-word-strictly)
              ;; Set word-end to ZV if there is no word after this
              ;; one.
              (setq word-end (point))
              ;; Now try to move backwards.  Set word-start to BEGV if
              ;; this word is there.
              (backward-word-strictly)
              (setq word-start (point))))
          ;; Check if word-start and word-end are identical, if there
          ;; is an object under POSN, or if point is looking at or
          ;; outside a word.
          (if (or (eq word-start word-end)
                  (>= word-start point))
              (progn
                ;; If so, clear the bounds and set and activate the
                ;; mark.
                (setq touch-screen-word-select-bounds nil
                      touch-screen-word-select-initial-word nil)
                (push-mark point)
                (goto-char point)
                (activate-mark)
                (setq deactivate-mark nil))
            ;; Otherwise, select the word.  Move point to either the
            ;; end or the start of the word, depending on which is
            ;; closer to EVENT.
            (let ((diff-beg (- point word-start))
                  (diff-end (- word-end point))
                  use-end)
              (if (> diff-beg diff-end)
                  ;; Set the point to the end of the word.
                  (setq use-end t)
                (if (< diff-end diff-beg)
                    (setq use-end nil)
                  ;; POINT is in the middle of the word.  Use its
                  ;; window coordinates to establish whether or not it
                  ;; is closer to the start of the word or to the end
                  ;; of the word.
                  (let ((posn-beg (posn-at-point word-start))
                        (posn-end (posn-at-point word-end)))
                    ;; Give up if there's an object at either of those
                    ;; positions, or they're not on the same row.
                    ;; If one of the positions isn't visible, use the
                    ;; window end.
                    (if (and posn-beg posn-end
                             (not (posn-object posn-beg))
                             (not (posn-object posn-end))
                             (eq (cdr (posn-col-row posn-beg))
                                 (cdr (posn-col-row posn-end))))
                        (setq use-end nil)
                      ;; Compare the pixel positions.
                      (setq point (car (posn-x-y posn))
                            diff-beg (- point (car (posn-x-y posn-beg)))
                            diff-end (- (car (posn-x-y posn-end)) point))
                      ;; Now determine whether or not point should be
                      ;; moved to the end.
                      (setq use-end (>= diff-beg diff-end))))))
              (if use-end
                  (progn
                    (push-mark word-start)
                    (activate-mark)
                    (setq deactivate-mark nil)
                    (goto-char word-end))
                (progn
                    (push-mark word-end)
                    (activate-mark)
                    (setq deactivate-mark nil)
                    (goto-char word-start)))
              ;; Record the bounds of the selected word.
              (setq touch-screen-word-select-bounds
                    (cons word-start word-end)
                    ;; Save this for the benefit of touch-screen-drag.
                    touch-screen-word-select-initial-word
                    (cons word-start word-end)))))))))

(defun touch-screen-preview-select ()
  "Display a preview of the line around point in the echo area.
Unless the minibuffer is an active or the current line is
excessively tall, display an indication of the position of point
and the contents of the visible line around it within the echo
area.

If the selected window is hscrolled or lines may be truncated,
attempt to find the extents of the text between column 0 and the
right most column of the window using `posn-at-x-y'."
  (interactive)
  ;; First, establish that the minibuffer isn't active and the line
  ;; isn't taller than two times the frame character height.
  (unless (or (> (minibuffer-depth) 0)
              ;; The code below doesn't adapt well to buffers
              ;; containing long lines.
              (long-line-optimizations-p)
              (let ((window-line-height (window-line-height))
                    (maximum-height (* 2 (frame-char-height))))
                (unless window-line-height
                  ;; `window-line-height' isn't available.
                  ;; Redisplay first and try to ascertain the height
                  ;; of the line again.
                  (redisplay t)
                  (setq window-line-height (window-line-height)))
                ;; `window-line-height' might still be unavailable.
                (and window-line-height
                     (> (car window-line-height)
                        maximum-height))))
    (catch 'hscrolled-away
      (let ((beg nil) end string y)
        ;; Detect whether or not the window is hscrolled.  If it
        ;; is, set beg to the location of the first column
        ;; instead.
        (when (> (window-hscroll) 0)
          (setq y (+ (or (cdr (posn-x-y (posn-at-point)))
                         (throw 'hscrolled-away t))
                     (window-header-line-height)
                     (window-tab-line-height)))
          (let* ((posn (posn-at-x-y 0 y))
                 (point (posn-point posn)))
            (setq beg point)))
        ;; Check if lines are being truncated; if so, use the
        ;; character at the end of the window as the end of the
        ;; text to be displayed, as the visual line may extend
        ;; past the window.
        (when (or truncate-lines beg) ; truncate-lines or hscroll.
          (setq y (or y (+ (or (cdr (posn-x-y (posn-at-point)))
                               (throw 'hscrolled-away t))
                           (window-header-line-height)
                           (window-tab-line-height))))
          (let* ((posn (posn-at-x-y (1- (window-width nil t)) y))
                 (point (posn-point posn)))
            (setq end point)))
        ;; Now find the rest of the visual line.
        (save-excursion
          (unless beg
            (beginning-of-visual-line)
            (setq beg (point)))
          (unless end
            (end-of-visual-line)
            (setq end (point))))
        ;; Obtain a substring containing the beginning of the
        ;; visual line and the end.
        (setq string (buffer-substring beg end))
        ;; Hack `invisible' properties within the new string.
        ;; Look for each change of the property that is a variable
        ;; name and replace it with its actual value according to
        ;; `buffer-invisibility-spec'.
        (when (listp buffer-invisibility-spec)
          (let ((index 0)
                (property (get-text-property 0
                                             'invisible
                                             string))
                index1 invisible)
            (while index
              ;; Find the end of this text property.
              (setq index1 (next-single-property-change index
                                                        'invisible
                                                        string))
              ;; Replace the property with whether or not it is
              ;; non-nil.
              (when property
                (setq invisible nil)
                (catch 'invisible
                  (dolist (spec buffer-invisibility-spec)
                    ;; Process one element of the buffer
                    ;; invisibility specification.
                    (if (consp spec)
                        (when (eq (cdr spec) 't)
                          ;; (ATOM . t) makes N invisible if N is
                          ;; equal to ATOM or a list containing
                          ;; ATOM.
                          (when (or (eq (car spec) property)
                                    (and (listp spec)
                                         (memq (car spec) invisible)))
                            (throw 'invisible (setq invisible t))))
                      ;; Otherwise, N is invisible if SPEC is
                      ;; equal to N.
                      (when (eq spec property)
                        (throw 'invisible (setq invisible t))))))
                (put-text-property index (or index1
                                             (- end beg))
                                   'invisible invisible string))
              ;; Set index to that of the next text property and
              ;; continue.
              (setq index index1
                    property (and index1
                                  (get-text-property index1
                                                     'invisible
                                                     string))))))
        (let ((resize-mini-windows t) difference width
              (message-log-max nil))
          ;; Find the offset of point from beg and display a cursor
          ;; below.
          (setq difference (- (point) beg)
                width (string-pixel-width
                       (substring string 0 difference)))
          (message "%s\n%s^" string
                   (propertize " "
                               'display (list 'space
                                              :width (list width)))))
        nil))))

(defun touch-screen-drag (event)
  "Handle a drag EVENT by setting the region to its new point.
If `touch-screen-word-select' and EVENT lies outside the last
word that was selected, select the word that now contains POINT.
Scroll the window if EVENT's coordinates are outside its text
area."
  (interactive "e")
  (let* ((posn (cadr event)) ; Position of the tool.
         (point (posn-point posn)) ; Point of the event.
         ;; Window where the tap originated.
         (window (nth 1 touch-screen-current-tool))
         ;; The currently selected window.  Used to redisplay within
         ;; the correct window while scrolling.
         (old-window (selected-window))
         ;; Whether or not text should be selected word-by-word.
         (word-select touch-screen-word-select)
         ;; Cons containing the confines of the word initially
         ;; selected when the touchpoint was first held down.
         (initial touch-screen-word-select-initial-word)
         initial-point)
    ;; Keep dragging.
    (with-selected-window window
      ;; Figure out what character to go to.  If this posn is in the
      ;; window, go to (posn-point posn).  If not, then go to the line
      ;; before either window start or window end.
      (setq initial-point (point))
      (when (or (not point)
                (not (eq point initial-point)))
        (if (and (eq (posn-window posn) window)
                 point
                 ;; point must be visible in the window.  If it isn't,
                 ;; the window must be scrolled.
                 (pos-visible-in-window-p point))
            (let* ((bounds touch-screen-word-select-bounds)
                   (maybe-select-word (or (not touch-screen-word-select)
                                          (or (not bounds)
                                              (> point (cdr bounds))
                                              (< point (car bounds))))))
              (if (and word-select
                       ;; point is now outside the last word selected.
                       maybe-select-word
                       (not (posn-object posn))
                       (when-let* ((char (char-after point))
                                   (class (char-syntax char)))
                         ;; Don't select words if point isn't inside a
                         ;; word constituent or similar.
                         (or (eq class ?w) (eq class ?_))))
                  ;; Determine the confines of the word containing
                  ;; POINT.
                  (let (word-start word-end)
                    (save-excursion
                      (goto-char point)
                      (forward-word-strictly)
                      ;; Set word-end to ZV if there is no word after
                      ;; this one.
                      (setq word-end (point))
                      ;; Now try to move backwards.  Set word-start to
                      ;; BEGV if this word is there.
                      (backward-word-strictly)
                      (setq word-start (point)))
                    (let ((mark (mark)))
                      ;; Extend the region to cover either word-end or
                      ;; word-start; whether to goto word-end or
                      ;; word-start is subject to the position of the
                      ;; mark relative to point.
                      (if (< word-start mark)
                          ;; The start of the word is behind mark.
                          ;; Extend the region towards the start.
                          (goto-char word-start)
                        ;; Else, go to the end of the word.
                        (goto-char word-end))
                      ;; If point is less than mark, which is is less
                      ;; than the end of the word that was originally
                      ;; selected, try to keep it selected by moving
                      ;; mark there.
                      (when (and initial (<= (point) mark)
                                 (< mark (cdr initial)))
                        (set-mark (cdr initial)))
                      ;; Do the opposite when the converse is true.
                      (when (and initial (>= (point) mark)
                                 (> mark (car initial)))
                        (set-mark (car initial))))
                    (if bounds
                        (progn (setcar bounds word-start)
                               (setcdr bounds word-end))
                      (setq touch-screen-word-select-bounds
                            (cons word-start word-end))))
                (when maybe-select-word
                  (goto-char (posn-point posn))
                  (when initial
                    ;; If point is less than mark, which is is less
                    ;; than the end of the word that was originally
                    ;; selected, try to keep it selected by moving
                    ;; mark there.
                    (when (and (<= (point) (mark))
                               (< (mark) (cdr initial)))
                      (set-mark (cdr initial)))
                    ;; Do the opposite when the converse is true.
                    (when (and (>= (point) (mark))
                               (> (mark) (car initial)))
                      (set-mark (car initial))))
                  (setq touch-screen-word-select-bounds nil)))
              ;; Finally, display a preview of the line around point
              ;; if requested by the user.
              (when (and touch-screen-preview-select
                         (not (eq (point) initial-point)))
                (touch-screen-preview-select)))
          ;; POSN is outside the window.  Scroll accordingly.
          (let* ((relative-xy
                  (touch-screen-relative-xy posn window))
                 (xy (posn-x-y posn))
                 ;; The height of the window's text area.
                 (body-height (window-body-height nil t))
                 ;; This is used to find the character closest to
                 ;; POSN's column at the bottom of the window.
                 (height (- body-height
                            ;; Use the last row of the window, not its
                            ;; last pixel.
                            (frame-char-height)))
                 (midpoint (/ body-height 2))
                 (scroll-conservatively 101))
            (cond
             ((< (cdr relative-xy) midpoint)
              ;; POSN is before half the window, yet POINT does not
              ;; exist or is not completely visible within.  Scroll
              ;; downwards.
              (ignore-errors
                ;; Scroll down by a single line.
                (scroll-down 1)
                ;; After scrolling, look up the new posn at EVENT's
                ;; column and go there.
                (setq posn (posn-at-x-y (car xy) 0)
                      point (posn-point posn))
                (if point
                    (goto-char point)
                  ;; If there's no buffer position at that column, go
                  ;; to the window start.
                  (goto-char (window-start)))
                ;; If word selection is enabled, now try to keep the
                ;; initially selected word within the active region.
                (when word-select
                  (when initial
                    ;; If point is less than mark, which is is less
                    ;; than the end of the word that was originally
                    ;; selected, try to keep it selected by moving
                    ;; mark there.
                    (when (and (<= (point) (mark))
                               (< (mark) (cdr initial)))
                      (set-mark (cdr initial)))
                    ;; Do the opposite when the converse is true.
                    (when (and (>= (point) (mark))
                               (> (mark) (car initial)))
                      (set-mark (car initial))))
                  (setq touch-screen-word-select-bounds nil))
                ;; Display a preview of the line now around point if
                ;; requested by the user.
                (when touch-screen-preview-select
                  (touch-screen-preview-select))
                ;; Select old-window, so that redisplay doesn't
                ;; display WINDOW as selected if it isn't already.
                (with-selected-window old-window
                  ;; Now repeat this every `mouse-scroll-delay' until
                  ;; input becomes available, but scroll down a few
                  ;; more lines.
                  (while (sit-for mouse-scroll-delay)
                    ;; Select WINDOW again.
                    (with-selected-window window
                      ;; Keep scrolling down until input becomes
                      ;; available.
                      (scroll-down 4)
                      ;; After scrolling, look up the new posn at
                      ;; EVENT's column and go there.
                      (setq posn (posn-at-x-y (car xy) 0)
                            point (posn-point posn))
                      (if point
                          (goto-char point)
                        ;; If there's no buffer position at that
                        ;; column, go to the window start.
                        (goto-char (window-start)))
                      ;; If word selection is enabled, now try to keep
                      ;; the initially selected word within the active
                      ;; region.
                      (when word-select
                        (when initial
                          ;; If point is less than mark, which is is
                          ;; less than the end of the word that was
                          ;; originally selected, try to keep it
                          ;; selected by moving mark there.
                          (when (and (<= (point) (mark))
                                     (< (mark) (cdr initial)))
                            (set-mark (cdr initial)))
                          ;; Do the opposite when the converse is true.
                          (when (and (>= (point) (mark))
                                     (> (mark) (car initial)))
                            (set-mark (car initial))))
                        (setq touch-screen-word-select-bounds nil))
                      ;; Display a preview of the line now around
                      ;; point if requested by the user.
                      (when touch-screen-preview-select
                        (touch-screen-preview-select))))))
              (setq touch-screen-word-select-bounds nil))
             ((>= (cdr relative-xy) midpoint)
              ;; Default to scrolling upwards even if POSN is still
              ;; within the confines of the window.  If POINT is
              ;; partially visible, and the branch above hasn't been
              ;; taken it must be somewhere at the bottom of the
              ;; window, so scroll downwards.
              (ignore-errors
                ;; Scroll up by a single line.
                (scroll-up 1)
                ;; After scrolling, look up the new posn at EVENT's
                ;; column and go there.
                (setq posn (posn-at-x-y (car xy) height)
                      point (posn-point posn))
                (if point
                    (goto-char point)
                  ;; If there's no buffer position at that column, go
                  ;; to the window start.
                  (goto-char (window-end nil t)))
                ;; If word selection is enabled, now try to keep
                ;; the initially selected word within the active
                ;; region.
                (when word-select
                  (when initial
                    ;; If point is less than mark, which is is less
                    ;; than the end of the word that was originally
                    ;; selected, try to keep it selected by moving
                    ;; mark there.
                    (when (and (<= (point) (mark))
                               (< (mark) (cdr initial)))
                      (set-mark (cdr initial)))
                    ;; Do the opposite when the converse is true.
                    (when (and (>= (point) (mark))
                               (> (mark) (car initial)))
                      (set-mark (car initial))))
                  (setq touch-screen-word-select-bounds nil))
                ;; Display a preview of the line now around point if
                ;; requested by the user.
                (when touch-screen-preview-select
                  (touch-screen-preview-select))
                ;; Select old-window, so that redisplay doesn't
                ;; display WINDOW as selected if it isn't already.
                (with-selected-window old-window
                  ;; Now repeat this every `mouse-scroll-delay' until
                  ;; input becomes available, but scroll down a few
                  ;; more lines.
                  (while (sit-for mouse-scroll-delay)
                    ;; Select WINDOW again.
                    (with-selected-window window
                      ;; Keep scrolling down until input becomes
                      ;; available.
                      (scroll-up 4)
                      ;; After scrolling, look up the new posn at
                      ;; EVENT's column and go there.
                      (setq posn (posn-at-x-y (car xy) height)
                            point (posn-point posn))
                      (if point
                          (goto-char point)
                        ;; If there's no buffer position at that
                        ;; column, go to the window start.
                        (goto-char (window-end nil t)))
                      ;; If word selection is enabled, now try to keep
                      ;; the initially selected word within the active
                      ;; region.
                      (when word-select
                        (when initial
                          ;; If point is less than mark, which is is less
                          ;; than the end of the word that was originally
                          ;; selected, try to keep it selected by moving
                          ;; mark there.
                          (when (and (<= (point) (mark))
                                     (< (mark) (cdr initial)))
                            (set-mark (cdr initial)))
                          ;; Do the opposite when the converse is true.
                          (when (and (>= (point) (mark))
                                     (> (mark) (car initial)))
                            (set-mark (car initial))))
                        (setq touch-screen-word-select-bounds nil))
                      ;; Display a preview of the line now around
                      ;; point if requested by the user.
                      (when touch-screen-preview-select
                        (touch-screen-preview-select)))))))))))
      (setq deactivate-mark nil))))

(defun touch-screen-restart-drag (event)
  "Restart dragging to select text.
Set point to the location of EVENT within its window while
keeping the bounds of the region intact, and set up state for
`touch-screen-drag'."
  (interactive "e")
  (let* ((posn (event-start event))
         (window (posn-window posn))
         (point (posn-point posn)))
    (with-selected-window window
      (let ((current-point (point))
            (current-mark (mark)))
        ;; Ensure that mark and point haven't changed since EVENT was
        ;; generated, and the region is still active.
        (when (or (eq point current-point)
                  (eq point current-mark)
                  (region-active-p))
          (when (eq point current-mark)
            ;; Exchange point and mark.
            (exchange-point-and-mark))
          ;; Clear the state necessary to set up dragging.  Don't try
          ;; to select entire words immediately after dragging starts,
          ;; to allow for fine grained selection inside a word.
          (setq touch-screen-word-select-bounds nil
                touch-screen-word-select-initial-word nil))))))

(global-set-key [touchscreen-hold] #'touch-screen-hold)
(global-set-key [touchscreen-drag] #'touch-screen-drag)
(global-set-key [touchscreen-restart-drag] #'touch-screen-restart-drag)



;; Pinch gesture.

(defvar text-scale-mode)
(defvar text-scale-mode-amount)
(defvar text-scale-mode-step)

(defun touch-screen-scroll-point-to-y (target-point target-y)
  "Move the row surrounding TARGET-POINT to TARGET-Y.
Scroll the current window such that the position of TARGET-POINT
within it on the Y axis approaches TARGET-Y."
  (condition-case nil
      (let* ((last-point (point))
	     (current-y (cadr (pos-visible-in-window-p target-point
                                                       nil t)))
	     (direction (if (if current-y
                                (< target-y current-y)
			      (< (window-start) target-point))
			    -1 1)))
	(while (< 0 (* direction (if current-y
				     (- target-y current-y)
				   (- (window-start) target-point))))
	  (scroll-down direction)
	  (setq last-point (point))
	  (setq current-y (cadr (pos-visible-in-window-p target-point nil t))))
	(unless (and (< direction 0) current-y)
	  (scroll-up direction)
	  (goto-char last-point)))
    ;; Ignore BOB and EOB.
    ((beginning-of-buffer end-of-buffer) nil)))

(defun touch-screen-pinch (event)
  "Scroll the window in the touchscreen-pinch event EVENT.
Pan the display by the pan deltas in EVENT, and adjust the
text scale by the ratio therein."
  (interactive "e")
  (require 'face-remap)
  (let* ((posn (cadr event))
         (window (posn-window posn))
         (scale (nth 2 event))
         (ratio-diff (nth 5 event))
         current-scale start-scale)
    (when (windowp window)
      (with-selected-window window
        (setq current-scale (if text-scale-mode
                                text-scale-mode-amount
                              0)
              start-scale (or (aref touch-screen-aux-tool 7)
                              (aset touch-screen-aux-tool 7
                                    current-scale)))
        ;; Set the text scale.
        (text-scale-set (floor (+ (round (log scale text-scale-mode-step))
                                  start-scale)))
        ;; Subsequently move the row which was at the centrum to its Y
        ;; position.
        (if (and (not (eq current-scale
                          text-scale-mode-amount))
                 (posn-point posn)
                 (cdr (posn-x-y posn)))
            (touch-screen-scroll-point-to-y (posn-point posn)
                                            (cdr (posn-x-y posn)))
          ;; Rather than scroll POSN's point to its old row, scroll the
          ;; display by the Y axis deltas within EVENT.
          (let ((height (window-default-line-height))
                (y-accumulator (or (aref touch-screen-aux-tool 8) 0)))
            (setq y-accumulator (+ y-accumulator (nth 4 event)))
            (when (or (> y-accumulator height)
                      (< y-accumulator (- height)))
              (ignore-errors
                (if (> y-accumulator 0)
                    (scroll-down 1)
                  (scroll-up 1)))
              (setq y-accumulator 0))
            (aset touch-screen-aux-tool 8 y-accumulator))
          ;; Likewise for the X axis deltas.
          (let ((width (frame-char-width))
                (x-accumulator (or (aref touch-screen-aux-tool 9) 0)))
            (setq x-accumulator (+ x-accumulator (nth 3 event)))
            (when (or (> x-accumulator width)
                      (< x-accumulator (- width)))
              ;; Do not hscroll if the ratio has shrunk, for that is
              ;; generally attended by the centerpoint moving left,
              ;; and Emacs can hscroll left even when no lines are
              ;; truncated.
              (unless (and (< x-accumulator 0)
                           (< ratio-diff 0))
                (if (> x-accumulator 0)
                    (scroll-right 1)
                  (scroll-left 1)))
              (setq x-accumulator 0))
            (aset touch-screen-aux-tool 9 x-accumulator)))))))

(define-key global-map [touchscreen-pinch] #'touch-screen-pinch)



;; Touch screen event translation.  The code here translates raw touch
;; screen events into `touchscreen-scroll' events and mouse events in
;; a ``DWIM'' fashion, consulting the keymaps at the position of the
;; mouse event to determine the best course of action, while also
;; recognizing drag-to-select and other gestures.

(defun touch-screen-handle-timeout (arg)
  "Start the touch screen timeout or handle it depending on ARG.
When ARG is nil, start the `touch-screen-current-timer' to go off
in `touch-screen-delay' seconds, and call this function with ARG
t.

When ARG is t, set the fourth element of
`touch-screen-current-tool' to `held', and generate a
`touchscreen-hold' event at the original position of that tool."
  (if (not arg)
      ;; Cancel the touch screen long-press timer, if it is still
      ;; there by any chance.
      (progn
        (when touch-screen-current-timer
          (cancel-timer touch-screen-current-timer))
        (setq touch-screen-current-timer
              (run-at-time touch-screen-delay nil
                           #'touch-screen-handle-timeout
                           t)))
    ;; Set touch-screen-current-timer to nil.
    (setq touch-screen-current-timer nil)
    (when touch-screen-current-tool
      ;; Set the state to `held'.
      (setcar (nthcdr 3 touch-screen-current-tool) 'held)
      ;; Generate an input event at the original position of the mark.
      ;; This assumes that the timer is running within
      ;; `touch-screen-translate-touch'.
      (let ((posn (nth 4 touch-screen-current-tool)))
        (throw 'input-event (list 'touchscreen-hold posn))))))

(declare-function remember-mouse-glyph "xdisp.c")

(defun touch-screen-handle-point-update (point)
  "Notice that the touch point POINT has changed position.
Perform the editing operations or throw to the input translation
function with an input event tied to any gesture that is
recognized.

Update the tenth element of `touch-screen-current-tool' with
POINT relative to the window it was placed on.  Update the third
element in like fashion, once sufficient motion has accumulated
that an event is generated.

POINT must be the touch point currently being tracked as
`touch-screen-current-tool'.

If the fourth element of `touch-screen-current-tool' is nil, then
the touch has just begun.  In a related case, if it is
`ancillary-tool', then the ancillary tool has been removed and
gesture translation must be resumed.  Determine how much POINT
has moved.  If POINT has moved upwards or downwards by a
significant amount, then set the fourth element to `scroll'.
Then, generate a `touchscreen-scroll' event with the window that
POINT was initially placed upon, and pixel deltas describing how
much point has moved relative to its previous position in the X
and Y axes.

If the fourth element of `touch-screen-current-tool' is `scroll',
then generate a `touchscreen-scroll' event with the window that
POINT was initially placed upon, and pixel deltas describing how
much point has moved relative to its previous position in the X
and Y axes.

If the fourth element of `touch-screen-current-tool' is
`mouse-drag' and `track-mouse' is non-nil, then generate a
`mouse-movement' event with the position of POINT.

If the fourth element of `touch-screen-current-tool' is `held',
then the touch has been held down for some time.  If motion
happens, set the field to `drag'.  Then, generate a
`touchscreen-drag' event.

If the fourth element of `touch-screen-current-tool' is
`restart-drag', set the field to `drag' and generate a
`touchscreen-drag'.

If the fourth element of `touch-screen-current-tool' is `drag',
then move point to the position of POINT."
  (let* ((window (nth 1 touch-screen-current-tool))
         (what (nth 3 touch-screen-current-tool))
         (posn (cdr point))
         ;; Now get the position of X and Y relative to WINDOW.
         (relative-xy (touch-screen-relative-xy posn window)))
    ;; Update the 10th field of the tool list with RELATIVE-XY.
    (setcar (nthcdr 9 touch-screen-current-tool) relative-xy)
    ;; And the 11th with the absolute position of POSN relative to its
    ;; frame.
    (setcar (nthcdr 10 touch-screen-current-tool)
            (touch-screen-relative-xy posn 'frame))
    (cond ((or (null what)
               (eq what 'ancillary-tool))
           (let* ((last-posn (nth 2 touch-screen-current-tool))
                  (diff-x (- (car last-posn) (car relative-xy)))
                  (diff-y (- (cdr last-posn) (cdr relative-xy))))
             (when (or (> diff-y 10)
                       (> diff-x (frame-char-width))
                       (< diff-y -10)
                       (< diff-x (- (frame-char-width))))
               (setcar (nthcdr 3 touch-screen-current-tool)
                       'scroll)
               (setcar (nthcdr 2 touch-screen-current-tool)
                       relative-xy)
               ;; Cancel the touch screen long-press timer, if it is
               ;; still there by any chance.
               (when touch-screen-current-timer
                 (cancel-timer touch-screen-current-timer)
                 (setq touch-screen-current-timer nil))
               ;; Generate a `touchscreen-scroll' event with `diff-x'
               ;; and `diff-y'.
               (throw 'input-event
                      (list 'touchscreen-scroll
                            window diff-x diff-y)))))
          ((eq what 'scroll)
           ;; Cancel the touch screen long-press timer, if it is still
           ;; there by any chance.
           (when touch-screen-current-timer
             (cancel-timer touch-screen-current-timer)
             (setq touch-screen-current-timer nil))
           (let* ((last-posn (nth 2 touch-screen-current-tool))
                  (diff-x (- (car last-posn) (car relative-xy)))
                  (diff-y (- (cdr last-posn) (cdr relative-xy))))
             (setcar (nthcdr 3 touch-screen-current-tool)
                     'scroll)
             (setcar (nthcdr 2 touch-screen-current-tool)
                     relative-xy)
             (unless (and (zerop diff-x) (zerop diff-y))
               (throw 'input-event
                      ;; Generate a `touchscreen-scroll' event with
                      ;; `diff-x' and `diff-y'.
                      (list 'touchscreen-scroll
                            window diff-x diff-y)))))
          ((eq what 'mouse-drag)
           ;; There was a `down-mouse-1' event bound at the starting
           ;; point of the event.  Generate a mouse-motion event if
           ;; mouse movement is being tracked.
           (when track-mouse
             (let ((mouse-rect (nth 5 touch-screen-current-tool))
                   (edges (window-inside-pixel-edges window)))
               ;; If fine-grained tracking is enabled, disregard the
               ;; mouse rect.  Apply the same criteria as
               ;; `remember_mouse_glyph', which see.
               (if (or mouse-fine-grained-tracking
                       window-resize-pixelwise)
                   (throw 'input-event (list 'mouse-movement posn))
                 ;; Otherwise, generate an event only if POINT falls
                 ;; outside the extents of the mouse rect, and record
                 ;; the extents of the glyph beneath point as the next
                 ;; mouse rect.
                 (let ((point relative-xy)
                       (frame-offsets (if (framep window)
                                          '(0 . 0)
                                        (cons (car edges) (cadr edges)))))
                   (when (or (not mouse-rect)
                             (< (car point) (- (car mouse-rect)
                                               (car frame-offsets)))
                             (> (car point) (+ (- (car mouse-rect) 1
                                                  (car frame-offsets))
                                               (caddr mouse-rect)))
                             (< (cdr point) (- (cadr mouse-rect)
                                               (cdr frame-offsets)))
                             (> (cdr point) (+ (- (cadr mouse-rect) 1
                                                  (cdr frame-offsets))
                                               (cadddr mouse-rect))))
                     ;; Record the extents of this glyph.
                     (setcar (nthcdr 5 touch-screen-current-tool)
                             (remember-mouse-glyph (or (and (framep window) window)
                                                       (window-frame window))
                                                   (+ (car point)
                                                      (car frame-offsets))
                                                   (+ (cdr point)
                                                      (cdr frame-offsets))))
                     ;; Generate the movement.
                     (throw 'input-event (list 'mouse-movement posn))))))))
          ((eq what 'held)
           (let* ((posn (cdr point)))
             ;; Now start dragging.
             (setcar (nthcdr 3 touch-screen-current-tool)
                     'drag)
             ;; Generate a (touchscreen-drag POSN) event.
             ;; `touchscreen-hold' was generated when the timeout
             ;; fired.
             (throw 'input-event (list 'touchscreen-drag posn))))
          ((eq what 'restart-drag)
           (let* ((posn (cdr point)))
             ;; Now start dragging.
             (setcar (nthcdr 3 touch-screen-current-tool)
                     'drag)
             ;; Generate a (touchscreen-drag POSN) event.
             ;; `touchscreen-restart-drag' was generated when the
             ;; timeout fired.
             (throw 'input-event (list 'touchscreen-drag posn))))
          ((eq what 'drag)
           (let* ((posn (cdr point)))
             ;; Generate a (touchscreen-drag POSN) event.
             (throw 'input-event (list 'touchscreen-drag posn)))))))

(defsubst touch-screen-distance (pos1 pos2)
  "Compute the distance in pixels between POS1 and POS2.
Each is a coordinate whose car and cdr are respectively its X and
Y values."
  (let ((v1 (- (cdr pos2) (cdr pos1)))
        (v2 (- (car pos2) (car pos1))))
    (abs (sqrt (+ (* v1 v1) (* v2 v2))))))

(defsubst touch-screen-centrum (pos1 pos2)
  "Compute the center of a line between the points POS1 and POS2.
Each, and value, is a coordinate whose car and cdr are
respectively its X and Y values."
  (let ((v1 (+ (cdr pos2) (cdr pos1)))
        (v2 (+ (car pos2) (car pos1))))
    (cons (/ v2 2) (/ v1 2))))

(defun touch-screen-handle-aux-point-update (point number)
  "Notice that a point being observed has moved.
Register motion from either the current or ancillary tool while
an ancillary tool is present.

POINT must be the cdr of an element of a `touchscreen-update'
event's list of touch points.  NUMBER must be its touch ID.

Calculate the distance between POINT's position and that of the
other tool (which is to say the ancillary tool of POINT is the
current tool, and vice versa).  Compare this distance to that
between both points at the time they were placed on the screen,
and signal a pinch event to adjust the text scale and scroll the
window by the factor so derived.  Such events are lists formed as
so illustrated:

    (touchscreen-pinch CENTRUM RATIO PAN-X PAN-Y RATIO-DIFF)

in which CENTRUM is a posn representing the midpoint of a line
between the present locations of both tools, RATIO is the said
factor, PAN-X is the number of pixels on the X axis that centrum
has moved since the last event, PAN-Y is that on the Y axis, and
RATIO-DIFF is the difference between RATIO and the ratio in the
last such event."
  (let (this-point-position
        other-point-position
        (window (cadr touch-screen-current-tool)))
    (when (windowp window)
      (if (eq number (aref touch-screen-aux-tool 0))
          (progn
            ;; The point pressed is the ancillary tool.  Set
            ;; other-point-position to that of the current tool.
            (setq other-point-position (nth 9 touch-screen-current-tool))
            ;; Update the position within touch-screen-aux-tool.
            (aset touch-screen-aux-tool 3
                  (setq this-point-position
                        (touch-screen-relative-xy point window))))
        (setq other-point-position (aref touch-screen-aux-tool 3))
        (setcar (nthcdr 2 touch-screen-current-tool)
                (setq this-point-position
                      (touch-screen-relative-xy point window)))
        (setcar (nthcdr 9 touch-screen-current-tool)
                this-point-position))
      ;; Now compute, and take the absolute of, this distance.
      (let ((distance (touch-screen-distance this-point-position
                                             other-point-position))
            (centrum (touch-screen-centrum this-point-position
                                           other-point-position))
            (initial-distance (aref touch-screen-aux-tool 4))
            (initial-centrum (aref touch-screen-aux-tool 5)))
        (let* ((ratio (/ distance initial-distance))
               (ratio-diff (- ratio (aref touch-screen-aux-tool 6))))
          ;; Update the internal record of its position and generate an
          ;; event.
          (aset touch-screen-aux-tool 5 centrum)
          (aset touch-screen-aux-tool 6 ratio)
          (throw 'input-event
                 (list 'touchscreen-pinch
                       (if (or (<= (car centrum) 0)
                               (<= (cdr centrum) 0))
                           (list window nil centrum nil nil
                                 nil nil nil nil nil)
                         (let ((posn (posn-at-x-y (car centrum)
                                                  (cdr centrum)
                                                  window)))
                           (if (eq (posn-window posn)
                                   window)
                               posn
                             ;; Return a placeholder outside the window
                             ;; if the centrum has moved beyond the
                             ;; confines of the window where the gesture
                             ;; commenced.
                             (list window nil centrum nil nil
                                   nil nil nil nil nil))))
                       ratio
                       (- (car centrum)
                          (car initial-centrum))
                       (- (cdr centrum)
                          (cdr initial-centrum))
                       ratio-diff)))))))

(defun touch-screen-window-selection-changed (frame)
  "Notice that FRAME's selected window has changed.
Cancel any timer that is supposed to hide the keyboard in
response to the minibuffer being closed."
  (with-selected-frame frame
    (unless (and (or buffer-read-only
                     (get-text-property (point) 'read-only))
                 ;; Don't hide the on-screen keyboard if it's always
                 ;; supposed to be displayed.
                 (not touch-screen-display-keyboard))
      ;; Prevent hiding the minibuffer from hiding the on screen
      ;; keyboard.
      (when minibuffer-on-screen-keyboard-timer
        (cancel-timer minibuffer-on-screen-keyboard-timer)
        (setq minibuffer-on-screen-keyboard-timer nil)))))

(defun touch-screen-handle-point-up (point prefix canceled)
  "Notice that POINT has been removed from the screen.
POINT should be the point currently tracked as
`touch-screen-current-tool'.
PREFIX should be a virtual function key used to look up key
bindings.
CANCELED should indicate whether the touch point was removed by
window-system intervention rather than user action.

If an ancillary touch point is being observed, transfer touch
information from `touch-screen-aux-tool' to
`touch-screen-current-tool' and set the former to nil, thereby
resuming gesture recognition with that tool replacing the tool
removed.

Otherwise:

If the fourth element of `touch-screen-current-tool' is nil or
`restart-drag', move point to the position of POINT, selecting
the window under POINT as well, and deactivate the mark; if there
is a button or link at POINT, call the command bound to `mouse-2'
there.  Otherwise, call the command bound to `mouse-1'.

If the fourth element of `touch-screen-current-tool' is
`mouse-drag', then generate either a `mouse-1' or a
`drag-mouse-1' event depending on how far the position of POINT
is from the starting point of the touch.

If the fourth element of `touch-screen-current-tool' is
`mouse-1-menu', then generate a `down-mouse-1' event at the
original position of the tool to display its bound keymap as a
menu.

If the fourth element of `touch-screen-current-tool' is `drag' or
`held', the region is active, and the tool's initial window's
selected buffer isn't read-only, display the on screen keyboard.

If the command being executed is listed in
`touch-screen-set-point-commands' also display the on-screen
keyboard if the current buffer and the character at the new point
is not read-only."
  (if touch-screen-aux-tool
      (progn
        (let ((point-no (aref touch-screen-aux-tool 0))
              (relative-xy (aref touch-screen-aux-tool 3)))
          ;; Replace the current position of touch-screen-current-tool
          ;; with relative-xy and its number with point-no, but leave
          ;; other information (such as its starting position) intact:
          ;; this touchpoint is meant to continue the gesture
          ;; interrupted by the removal of the last, not to commence a
          ;; new one.
          (setcar touch-screen-current-tool point-no)
          (setcar (nthcdr 2 touch-screen-current-tool)
                  relative-xy)
          (setcar (nthcdr 9 touch-screen-current-tool)
                  relative-xy))
        (setq touch-screen-aux-tool nil))
    (let ((what (nth 3 touch-screen-current-tool))
          (posn (cdr point)) window point)
      (cond ((or (null what)
                 ;; If dragging has been restarted but the touch point
                 ;; hasn't been moved, translate the sequence into a
                 ;; regular mouse click.
                 (eq what 'restart-drag))
             ;; Don't attempt to execute commands bound to mouse events
             ;; if the touch sequence has been canceled.
             (unless canceled
               (when (windowp (posn-window posn))
                 (setq point (posn-point posn)
                       window (posn-window posn))
                 ;; Select the window that was tapped given that it
                 ;; isn't an inactive minibuffer window.
                 (when (or (not (eq window
                                    (minibuffer-window
                                     (window-frame window))))
                           (minibuffer-window-active-p window))
                   (select-window window))
                 ;; Now simulate a mouse click there.  If there is a
                 ;; link or a button, use mouse-2 to push it.
                 (let* ((event (list (if (or (mouse-on-link-p posn)
                                             (and point
                                                  (get-char-property
                                                   point 'button)))
                                         'mouse-2
                                       'mouse-1)
                                     posn))
                        ;; Look for the command bound to this event.
                        (command (key-binding (if prefix
                                                  (vector prefix
                                                          (car event))
                                                (vector (car event)))
                                              t nil posn)))
                   (deactivate-mark)
                   (when point
                     ;; This is necessary for following links.
                     (goto-char point))
                   ;; Figure out if the on screen keyboard needs to be
                   ;; displayed.
                   (when command
                     (if (or (memq command touch-screen-set-point-commands)
                             ;; Users of packages that redefine
                             ;; `mouse-set-point', or other commands
                             ;; recognized as defining the point, should
                             ;; not find the on screen keyboard
                             ;; inaccessible even with
                             ;; `touch-screen-display-keyboard' enabled.
                             touch-screen-display-keyboard)
                         (if touch-screen-translate-prompt
                             ;; Forgo displaying the virtual keyboard
                             ;; should `touch-screen-translate-prompt' be
                             ;; set, for then the key won't be delivered
                             ;; to the command loop, but rather to a
                             ;; caller of `read-key-sequence' such as
                             ;; `describe-key'.
                             (throw 'input-event event)
                           (if (or touch-screen-display-keyboard
                                   (and (or (not buffer-read-only)
                                            inhibit-read-only
                                            ;; Display the on screen
                                            ;; keyboard even if just the
                                            ;; text under point is not
                                            ;; read-only.
                                            (get-text-property
                                             point 'inhibit-read-only))
                                        ;; If the major mode has defined
                                        ;; bespoke criteria for
                                        ;; displaying the on screen
                                        ;; keyboard, consult it here.
                                        (or (not touch-screen-keyboard-function)
                                            (funcall
                                             touch-screen-keyboard-function))))
                               ;; Once the on-screen keyboard has been
                               ;; opened, add
                               ;; `touch-screen-window-selection-changed'
                               ;; as a window selection change function
                               ;; This then prevents it from being
                               ;; hidden after exiting the minibuffer.
                               (progn
                                 (add-hook
                                  'window-selection-change-functions
                                  #'touch-screen-window-selection-changed)
                                 (frame-toggle-on-screen-keyboard
                                  (selected-frame) nil))
                             ;; Otherwise, hide the on screen keyboard
                             ;; now.
                             (frame-toggle-on-screen-keyboard (selected-frame)
                                                              t))
                           ;; But if it's being called from `describe-key'
                           ;; or some such, return it as a key sequence.
                           (throw 'input-event event)))
                     ;; If not, return the event.
                     (throw 'input-event event))))))
            ((eq what 'mouse-drag)
             ;; Generate a corresponding `mouse-1' event.
             ;; Alternatively, quit if the touch sequence was canceled.
             (if canceled
                 (keyboard-quit)
               (let* ((new-window (posn-window posn))
                      (new-point (posn-point posn))
                      (old-posn (nth 4 touch-screen-current-tool))
                      (old-window (posn-window posn))
                      (old-point (posn-point posn))
                      (new-relative-xy (touch-screen-relative-xy
                                        posn new-window))
                      (old-relative-xy (touch-screen-relative-xy
                                        old-posn new-window)))
                 (throw 'input-event
                        ;; If the position of the touch point has
                        ;; changed, or it has moved significantly, as
                        ;; measured by reference to double-click-fuzz...
                        (if (or (let ((xdiff (- (car new-relative-xy)
                                                (car old-relative-xy)))
                                      (ydiff (- (cdr new-relative-xy)
                                                (cdr old-relative-xy))))
                                  (and (>= (abs xdiff) double-click-fuzz)
                                       (>= (abs ydiff) double-click-fuzz)))
                                (not (eq old-window new-window))
                                (not (eq old-point new-point)))
                            ;; ... generate a drag-mouse-1 event...
                            (list 'drag-mouse-1 old-posn posn)
                          ;; ... otherwise, generate a mouse-1 event.
                          (list 'mouse-1 posn))))))
            ((eq what 'mouse-1-menu)
             ;; Generate a `down-mouse-1' event at the position the tap
             ;; took place, unless the touch sequence was canceled.
             (unless canceled
               (throw 'input-event
                      (list 'down-mouse-1
                            (nth 4 touch-screen-current-tool)))))
            ((or (eq what 'drag)
                 ;; Merely initiating a drag is sufficient to select a
                 ;; word if word selection is enabled.
                 (eq what 'held))
             (unless canceled
               ;; Display the on screen keyboard if the region is now
               ;; active.  Check this within the window where the tool
               ;; was first place.
               (setq window (nth 1 touch-screen-current-tool))
               (when window
                 (with-selected-window window
                   (when (and (region-active-p)
                              (not buffer-read-only))
                     ;; Once the on-screen keyboard has been opened, add
                     ;; `touch-screen-window-selection-changed' as a
                     ;; window selection change function.  This then
                     ;; prevents it from being hidden after exiting the
                     ;; minibuffer.
                     (progn
                       (add-hook 'window-selection-change-functions
                                 #'touch-screen-window-selection-changed)
                       (frame-toggle-on-screen-keyboard (selected-frame)
                                                        nil)))))))))))

(defun touch-screen-handle-touch (event prefix &optional interactive)
  "Handle a single touch EVENT, and perform associated actions.
EVENT can either be a `touchscreen-begin', `touchscreen-update' or
`touchscreen-end' event.
PREFIX is either nil, or a symbol specifying a virtual function
key to apply to EVENT.

If INTERACTIVE, execute the command associated with any event
generated instead of throwing `input-event'.  Otherwise, throw
`input-event' with a single input event if that event should take
the place of EVENT within the key sequence being translated, or
nil if all tools have been released.

Set `touch-screen-events-received' to t to indicate that touch
screen events have been received, and thus by extension require
functions undertaking event management themselves to call
`read-key' rather than `read-event'."
  (interactive "e\ni\np")
  (unless touch-screen-events-received
    (setq touch-screen-events-received t))
  (if interactive
      ;; Called interactively (probably from wid-edit.el.)
      ;; Add any event generated to `unread-command-events'.
      (let ((event1
             (let ((current-key-remap-sequence (vector event)))
               (touch-screen-translate-touch nil))))
        (when (vectorp event1)
          (setq unread-command-events
                (nconc unread-command-events
                       (nreverse (append event1 nil))))))
    (cond
     ((eq (car event) 'touchscreen-begin)
      ;; A tool was just pressed against the screen.  Figure out the
      ;; window where it is and make it the tool being tracked on the
      ;; window.
      (let* ((touchpoint (caadr event))
             (position (cdadr event))
             (window (posn-window position))
             (point (posn-point position))
             (frame-or-window-frame
              (if (framep window) window (window-frame window)))
             binding tool-list)
        ;; Cancel the touch screen timer, if it is still there by any
        ;; chance.
        (when touch-screen-current-timer
          (cancel-timer touch-screen-current-timer)
          (setq touch-screen-current-timer nil))
        ;; If a tool already exists...
        (if (and touch-screen-current-tool
                 ;; ..and the number of this tool is at variance with
                 ;; that of the current tool: if a `touchscreen-end'
                 ;; event is delivered that is somehow withheld from
                 ;; this function and the system does not assign
                 ;; monotonically increasing touch point identifiers,
                 ;; then the ancillary tool will be set to a tool
                 ;; bearing the same number as the current tool, and
                 ;; consequently the mechanism for detecting
                 ;; erroneously retained touch points upon the
                 ;; registration of `touchscreen-update' events will
                 ;; not be activated.
                 (not (eq touchpoint (car touch-screen-current-tool))))
            ;; Then record this tool as the ``auxiliary tool''.
            ;; Updates to the auxiliary tool are considered in unison
            ;; with those to the current tool; the distance between
            ;; both tools is measured and compared with that when the
            ;; auxiliary tool was first pressed, then interpreted as a
            ;; scale by which to adjust text within the current tool's
            ;; window.
            (when (eq frame-or-window-frame
                      ;; Verify that the new tool was placed on the
                      ;; same frame the current tool has, so as not to
                      ;; consider events distributed across distinct
                      ;; frames components of a single gesture.
                      (window-frame (nth 1 touch-screen-current-tool)))
              ;; Set touch-screen-aux-tool as is proper.  Mind that
              ;; the last field is always relative to the current
              ;; tool's window.
              (let* ((window (nth 1 touch-screen-current-tool))
                     (relative-x-y (touch-screen-relative-xy position
                                                             window))
                     (initial-pos (nth 4 touch-screen-current-tool))
                     (initial-x-y (touch-screen-relative-xy initial-pos
                                                            window))
                     computed-distance computed-centrum)
                ;; Calculate the distance and centrum from this point
                ;; to the initial position of the current tool.
                (setq computed-distance (touch-screen-distance relative-x-y
                                                               initial-x-y)
                      computed-centrum (touch-screen-centrum relative-x-y
                                                             initial-x-y))
                ;; If computed-distance is zero, ignore this tap.
                (unless (zerop computed-distance)
                  (setq touch-screen-aux-tool (vector touchpoint window
                                                      position relative-x-y
                                                      computed-distance
                                                      computed-centrum
                                                      1.0 nil nil nil)))
                ;; When an auxiliary tool is pressed, any gesture
                ;; previously in progress must be terminated, so long
                ;; as it represents a gesture recognized from the
                ;; current tool's motion rather than ones detected by
                ;; this function from circumstances surrounding its
                ;; first press, such as the presence of a menu or
                ;; down-mouse-1 button beneath its first press.
                (unless (memq (nth 3 touch-screen-current-tool)
                              '(mouse-drag mouse-1-menu))
                  ;; Set the what field to the symbol `ancillary-tool'
                  ;; rather than nil, that mouse events may not be
                  ;; generated if no gesture is subsequently
                  ;; recognized; this, among others, prevents
                  ;; undesirable point movement (through the execution
                  ;; of `mouse-set-point') after both points are
                  ;; released without any gesture being detected.
                  (setcar (nthcdr 3 touch-screen-current-tool)
                          'ancillary-tool))))
          ;; Replace any previously ongoing gesture.  If POSITION has no
          ;; window or position, make it nil instead.
          (setq tool-list (and (windowp window)
                               (list touchpoint window
                                     (posn-x-y position)
                                     nil position
                                     nil nil nil nil
                                     (posn-x-y position)
                                     (touch-screen-relative-xy position
                                                               'frame)))
                touch-screen-current-tool tool-list)
          ;; Select the window underneath the event as the checks below
          ;; will look up keymaps and markers inside its buffer.
          (save-selected-window
            ;; Check if `touch-screen-extend-selection' is enabled,
            ;; the tap lies on the point or the mark, and the region
            ;; is active.  If that's the case, set the fourth element
            ;; of `touch-screen-current-tool' to `restart-drag', then
            ;; generate a `touchscreen-restart-drag' event.
            (when tool-list
              ;; tool-list is always non-nil where the selected window
              ;; matters.
              (select-window window)
              (when (and touch-screen-extend-selection
                         (or (eq point (point))
                             (eq point (mark)))
                         (region-active-p)
                         ;; Only restart drag-to-select if the tap
                         ;; falls on the same row as the selection.
                         ;; This prevents dragging from starting if
                         ;; the tap is below the last window line with
                         ;; text and `point' is at ZV, as the user
                         ;; most likely meant to scroll the window
                         ;; instead.
                         (when-let* ((posn-point (posn-at-point point))
                                     (posn-row (cdr
                                                (posn-col-row posn-point))))
                           (eq (cdr (posn-col-row position)) posn-row)))
                ;; Indicate that a drag is about to restart.
                (setcar (nthcdr 3 tool-list) 'restart-drag)
                ;; Generate the `restart-drag' event.
                (throw 'input-event (list 'touchscreen-restart-drag
                                          position))))
            ;; Determine whether there is a command bound to
            ;; `down-mouse-1' at the position of the tap and that
            ;; command is not a command whose functionality is replaced
            ;; by the long-press mechanism.  If so, set the fourth
            ;; element of `touch-screen-current-tool' to `mouse-drag'
            ;; and generate an emulated `mouse-1' event.  Likewise if
            ;; touch event translation is being invoked by a caller of
            ;; `read-key' that expects unprocessed mouse input,
            ;;
            ;; If the command in question is a keymap, set that element
            ;; to `mouse-1-menu' instead of `mouse-drag', and don't
            ;; generate a `down-mouse-1' event immediately, but wait for
            ;; the touch point to be released, so that the menu bar may
            ;; not be displayed before the user has released the touch
            ;; point and the window system is ready to display a menu.
            (if (and tool-list
                     (or (and (setq binding
                                    (key-binding (if prefix
                                                     (vector prefix
                                                             'down-mouse-1)
                                                   [down-mouse-1])
                                                 t nil position))
                              (not (and (symbolp binding)
                                        (get binding 'ignored-mouse-command))))
                         touch-screen-simple-mouse-conversion))
                (if (and (not touch-screen-simple-mouse-conversion)
                         (or (keymapp binding)
                             (and (symbolp binding)
                                  (get binding 'mouse-1-menu-command))))
                    ;; binding is a keymap, or a command that does
                    ;; almost the same thing.  If a `mouse-1' event is
                    ;; generated after the keyboard command loop
                    ;; displays it as a menu, that event could cause
                    ;; unwanted commands to be run.  Set what to
                    ;; `mouse-1-menu' instead and wait for the up
                    ;; event to display the menu.
                    (setcar (nthcdr 3 tool-list) 'mouse-1-menu)
                  (progn
                    (setcar (nthcdr 3 tool-list) 'mouse-drag)
                    ;; Record the extents of the glyph beneath this
                    ;; touch point to avoid generating extraneous events
                    ;; when it next moves.
                    (setcar
                     (nthcdr 5 touch-screen-current-tool)
                     (let* ((edges (window-inside-pixel-edges window))
                            (point (posn-x-y position))
                            (frame-offsets (if (framep window)
                                               '(0 . 0)
                                             (cons (car edges)
                                                   (cadr edges)))))
                       (remember-mouse-glyph (or (and (framep window) window)
                                                 (window-frame window))
                                             (+ (car point)
                                                (car frame-offsets))
                                             (+ (cdr point)
                                                (cdr frame-offsets)))))
                    (throw 'input-event (list 'down-mouse-1 position))))
              (and point
                   ;; Start the long-press timer.
                   (touch-screen-handle-timeout nil)))))))
     ((eq (car event) 'touchscreen-update)
      (unless touch-screen-current-tool
        ;; If a stray touchscreen-update event arrives (most likely
        ;; from the menu bar), stop translating this sequence.
        (throw 'input-event nil))
      ;; The positions of tools currently pressed against the screen
      ;; have changed.  If there is a tool being tracked as part of a
      ;; gesture, look it up in the list of tools.
      (if-let* ((new-point (assq (car touch-screen-current-tool)
                                 (cadr event))))
          (if touch-screen-aux-tool
              (touch-screen-handle-aux-point-update (cdr new-point)
                                                    (car new-point))
            (touch-screen-handle-point-update new-point))
        ;; If the current tool exists no longer, a touchscreen-end
        ;; event is certain to have been disregarded.  So that
        ;; touchscreen gesture translation might continue as usual
        ;; after this aberration to the normal flow of events, delete
        ;; the current tool now.
        (when touch-screen-current-timer
          ;; Cancel the touch screen long-press timer, if it is still
          ;; there by any chance.
          (cancel-timer touch-screen-current-timer)
          (setq touch-screen-current-timer nil))
        ;; Don't call `touch-screen-handle-point-up' when terminating
        ;; translation abnormally.
        (setq touch-screen-current-tool nil
              ;; Delete the ancillary tool while at it.
              touch-screen-aux-tool nil)
        (message "Current touch screen tool vanished!"))
      ;; Check for updates to any ancillary point being monitored.
      (when touch-screen-aux-tool
        (let ((new-point (assq (aref touch-screen-aux-tool 0)
                               (cadr event))))
          (when new-point
            (touch-screen-handle-aux-point-update (cdr new-point)
                                                  (car new-point))))))
     ((eq (car event) 'touchscreen-end)
      ;; A tool has been removed from the screen.  If it is the tool
      ;; currently being tracked, clear `touch-screen-current-tool'.
      (when (eq (caadr event) (car touch-screen-current-tool))
        ;; Cancel the touch screen long-press timer, if it is still
        ;; there by any chance.
        (when touch-screen-current-timer
          (cancel-timer touch-screen-current-timer)
          (setq touch-screen-current-timer nil))
        (let ((old-aux-tool touch-screen-aux-tool))
          (unwind-protect
              (touch-screen-handle-point-up (cadr event) prefix
                                            (caddr event))
            ;; If an ancillary tool is present the function call above
            ;; will simply transfer information from it into the current
            ;; tool list, rendering the new current tool, until such
            ;; time as it too is released.
            (when (not (and old-aux-tool (not touch-screen-aux-tool)))
              ;; Make sure the tool list is cleared even if
              ;; `touch-screen-handle-point-up' throws.
              (setq touch-screen-current-tool nil)))))
      ;; If it is rather the ancillary tool, delete its vector.  No
      ;; further action is required, for the next update received will
      ;; resume regular gesture recognition.
      ;;
      ;; The what field in touch-screen-current-tool is set to a
      ;; signal value when the ancillary tool is pressed, so gesture
      ;; recognition will commence with a clean slate, save for when
      ;; the first touch landed atop a menu or some other area
      ;; down-mouse-1 was bound.
      ;;
      ;; Gesture recognition will be inhibited in that case, so that
      ;; mouse menu or mouse motion events are generated in its place
      ;; as they would be were no ancillary tool ever pressed.
      (when (and touch-screen-aux-tool
                 (eq (caadr event) (aref touch-screen-aux-tool 0)))
        (setq touch-screen-aux-tool nil))
      ;; Throw to the key translation function.
      (throw 'input-event nil)))))

;; Mark `mouse-drag-region' as ignored for the purposes of mouse click
;; emulation.

(put 'mouse-drag-region 'ignored-mouse-command t)

;;;###autoload
(defun touch-screen-translate-touch (prompt)
  "Translate touch screen events into a sequence of mouse events.
PROMPT is the prompt string given to `read-key-sequence', or nil
if this function is being called from the keyboard command loop.
Value is a new key sequence.

Read the touch screen event within `current-key-remap-sequence'
and give it to `touch-screen-handle-touch'.  Return any key
sequence signaled.

If `touch-screen-handle-touch' does not signal for an event to be
returned after the last element of the key sequence is read,
continue reading touch screen events until
`touch-screen-handle-touch' signals.  Return a sequence
consisting of the first event encountered that is not a touch
screen event.

In addition to non-touchscreen events read, key sequences
returned may contain any one of the following events:

  (touchscreen-scroll WINDOW DX DY)

where WINDOW specifies a window to scroll, and DX and DY are
integers describing how many pixels to be scrolled horizontally
and vertically,

  (touchscreen-hold POSN)
  (touchscreen-drag POSN)

where POSN is the position of the long-press or touchpoint
motion,

  (touchscreen-restart-drag POSN)

where POSN is the position of the tap,

  (down-mouse-1 POSN)
  (drag-mouse-1 POSN)

where POSN is the position of the mouse button press or click,

  (mouse-1 POSN)
  (mouse-2 POSN)

where POSN is the position of the mouse click, either `mouse-2'
if POSN is on a link or a button, or `mouse-1' otherwise."
  (unwind-protect
      ;; Save the virtual function key if this is a mode line event.
      (let* ((prefix-specified
              ;; Virtual prefix keys can be nil for events that fall
              ;; outside a frame or within its internal border.
              (> (length current-key-remap-sequence) 1))
             (prefix (and prefix-specified
                          (aref current-key-remap-sequence 0)))
             (touch-screen-translate-prompt prompt)
             (event (catch 'input-event
                      ;; First, process the one event already within
                      ;; `current-key-remap-sequence'.
                      (touch-screen-handle-touch
                       (aref current-key-remap-sequence
                             (if prefix-specified 1 0))
                       prefix)
                      ;; Next, continue reading input events.
                      (while t
                        (let ((event1 (read-event)))
                          ;; If event1 is a virtual function key, make
                          ;; it the new prefix.
                          (if (memq event1 '(mode-line tab-line nil
                                             vertical-line
                                             header-line tool-bar tab-bar
                                             left-fringe right-fringe
                                             left-margin right-margin
                                             right-divider bottom-divider))
                              (setq prefix event1)
                            ;; If event1 is not a touch screen event,
                            ;; return it.
                            (if (not (memq (car-safe event1)
                                           '(touchscreen-begin
                                             touchscreen-end
                                             touchscreen-update)))
                                (throw 'input-event event1)
                              ;; Process this event as well.
                              (touch-screen-handle-touch event1 prefix))))))))
        ;; Return a key sequence consisting of event
        ;; or an empty vector if it is nil, meaning that
        ;; no key events have been translated.
        (if event (or (and prefix (consp event)
                           ;; Only generate virtual function keys for
                           ;; mouse events...
                           (memq (car event)
                                 '(down-mouse-1 mouse-1
                                   mouse-2 mouse-movement))
                           ;; .. and provided that Emacs has never
                           ;; previously encountered an event of this
                           ;; description, so that its `event-kind'
                           ;; property has yet to be initialized and
                           ;; keyboard.c will not understand whether and
                           ;; how to append a function key prefix.
                           (null (get (car event) 'event-kind))
                           ;; If this is a mode line event, then
                           ;; generate the appropriate function key.
                           (vector prefix event))
                      (vector event))
          ""))
    ;; Cancel the touch screen long-press timer, if it is still there
    ;; by any chance.  If the timer is to operate correctly, it must
    ;; fire within the catch block above.
    (when touch-screen-current-timer
      (cancel-timer touch-screen-current-timer)
      (setq touch-screen-current-timer nil))))

(define-key function-key-map [touchscreen-begin]
            #'touch-screen-translate-touch)
(define-key function-key-map [touchscreen-update]
            #'touch-screen-translate-touch)
(define-key function-key-map [touchscreen-end]
            #'touch-screen-translate-touch)

(define-key function-key-map [mode-line touchscreen-begin]
            #'touch-screen-translate-touch)
(define-key function-key-map [mode-line touchscreen-end]
            #'touch-screen-translate-touch)

;; These are used to translate events sent from the internal border or
;; from outside the frame.

(define-key function-key-map [nil touchscreen-begin]
            #'touch-screen-translate-touch)
(define-key function-key-map [nil touchscreen-end]
            #'touch-screen-translate-touch)

(define-key function-key-map [header-line touchscreen-begin]
            #'touch-screen-translate-touch)
(define-key function-key-map [header-line touchscreen-end]
            #'touch-screen-translate-touch)

(define-key function-key-map [bottom-divider touchscreen-begin]
            #'touch-screen-translate-touch)
(define-key function-key-map [bottom-divider touchscreen-end]
            #'touch-screen-translate-touch)

(define-key function-key-map [right-divider touchscreen-begin]
            #'touch-screen-translate-touch)
(define-key function-key-map [right-divider touchscreen-end]
            #'touch-screen-translate-touch)

(define-key function-key-map [right-divider touchscreen-begin]
            #'touch-screen-translate-touch)
(define-key function-key-map [right-divider touchscreen-end]
            #'touch-screen-translate-touch)

(define-key function-key-map [left-fringe touchscreen-begin]
            #'touch-screen-translate-touch)
(define-key function-key-map [left-fringe touchscreen-end]
            #'touch-screen-translate-touch)

(define-key function-key-map [right-fringe touchscreen-begin]
            #'touch-screen-translate-touch)
(define-key function-key-map [right-fringe touchscreen-end]
            #'touch-screen-translate-touch)

(define-key function-key-map [left-margin touchscreen-begin]
            #'touch-screen-translate-touch)
(define-key function-key-map [left-margin touchscreen-end]
            #'touch-screen-translate-touch)

(define-key function-key-map [right-margin touchscreen-begin]
            #'touch-screen-translate-touch)
(define-key function-key-map [right-margin touchscreen-end]
            #'touch-screen-translate-touch)

(define-key function-key-map [tool-bar touchscreen-begin]
            #'touch-screen-translate-touch)
(define-key function-key-map [tool-bar touchscreen-end]
            #'touch-screen-translate-touch)

(define-key function-key-map [tab-bar touchscreen-begin]
            #'touch-screen-translate-touch)
(define-key function-key-map [tab-bar touchscreen-end]
            #'touch-screen-translate-touch)

(define-key function-key-map [tab-line touchscreen-begin]
            #'touch-screen-translate-touch)
(define-key function-key-map [tab-line touchscreen-end]
            #'touch-screen-translate-touch)

(define-key function-key-map [vertical-line touchscreen-begin]
            #'touch-screen-translate-touch)
(define-key function-key-map [vertical-line touchscreen-end]
            #'touch-screen-translate-touch)

(define-key function-key-map [nil touchscreen-begin]
            #'touch-screen-translate-touch)
(define-key function-key-map [nil touchscreen-end]
            #'touch-screen-translate-touch)


;; Exports.  These functions are intended for use externally.

;;;###autoload
(defun touch-screen-track-tap (event &optional update data threshold)
  "Track a single tap starting from EVENT.
EVENT should be a `touchscreen-begin' event.

Read touch screen events until a `touchscreen-end' event is
received with the same ID as in EVENT.  If UPDATE is non-nil and
a `touchscreen-update' event is received in the mean time and
contains a touch point with the same ID as in EVENT, call UPDATE
with that event and DATA.

If THRESHOLD is non-nil, enforce a threshold of movement that is
either itself or 10 pixels when it is not a number.  If the
aforementioned touch point moves beyond that threshold on any
axis, return nil immediately, and further resume mouse event
translation for the touch point at hand.

Return nil immediately if any other kind of event is received;
otherwise, return t once the `touchscreen-end' event arrives."
  (let ((disable-inhibit-text-conversion t)
        (threshold (and threshold (or (and (numberp threshold)
                                           threshold)
                                      10)))
        (original-x-y (posn-x-y (cdadr event)))
        (original-window (posn-window (cdadr event))))
    (catch 'finish
      (while t
        (let ((new-event (read-event nil))
              touch-point)
          (cond
           ((eq (car-safe new-event) 'touchscreen-update)
            (when (setq touch-point (assq (caadr event) (cadr new-event)))
              (when update
                (funcall update new-event data))
              (when threshold
                (setq touch-point (cdr touch-point))
                ;; Detect the touch point moving past the threshold.
                (let* ((x-y (touch-screen-relative-xy touch-point
                                                      original-window))
                       (x (car x-y)) (y (cdr x-y)))
                  (when (or (> (abs (- x (car original-x-y))) threshold)
                            (> (abs (- y (cdr original-x-y))) threshold))
                    ;; Resume normal touch-screen to mouse event
                    ;; translation for this touch sequence by
                    ;; supplying both the event starting it and the
                    ;; motion event that overstepped the threshold to
                    ;; touch-screen-handle-touch.
                    (touch-screen-handle-touch event nil t)
                    (touch-screen-handle-touch new-event nil t)
                    (throw 'finish nil))))))
           ((eq (car-safe new-event) 'touchscreen-end)
            (throw 'finish
                   ;; Now determine whether or not the `touchscreen-end'
                   ;; event has the same ID as EVENT.  If it doesn't,
                   ;; then this is another touch, so return nil.
                   (eq (caadr event) (caadr new-event))))
           (t (throw 'finish nil))))))))

;;;###autoload
(defun touch-screen-track-drag (event update &optional data)
  "Track a single drag starting from EVENT.
EVENT should be a `touchscreen-begin' event.

Read touch screen events until a `touchscreen-end' event is
received with the same ID as in EVENT.  For each
`touchscreen-update' event received in the mean time containing a
touch point with the same ID as in EVENT, call UPDATE with the
touch point in event and DATA, once the touch point has moved
significantly by at least 5 pixels from where it was in EVENT.

Return nil immediately if any other kind of event is received;
otherwise, return either t or `no-drag' once the
`touchscreen-end' event arrives; return `no-drag' returned if the
touch point in EVENT did not move significantly, and t otherwise."
  (let ((return-value 'no-drag)
        (start-xy (touch-screen-relative-xy (cdadr event)
                                            'frame))
        (disable-inhibit-text-conversion t))
    (catch 'finish
      (while t
        (let ((new-event (read-event nil)))
          (cond
           ((eq (car-safe new-event) 'touchscreen-update)
            (when-let* ((tool (assq (caadr event) (nth 1 new-event)))
                        (xy (touch-screen-relative-xy (cdr tool) 'frame)))
              (when (or (> (- (car xy) (car start-xy)) 5)
                        (< (- (car xy) (car start-xy)) -5)
                        (> (- (cdr xy) (cdr start-xy)) 5)
                        (< (- (cdr xy) (cdr start-xy)) -5))
                (setq return-value t))
              (when (and update tool (eq return-value t))
                (funcall update new-event data))))
           ((eq (car-safe new-event) 'touchscreen-end)
            (throw 'finish
                   ;; Now determine whether or not the `touchscreen-end'
                   ;; event has the same ID as EVENT.  If it doesn't,
                   ;; then this is another touch, so return nil.
                   (and (eq (caadr event) (caadr new-event))
                        return-value)))
           (t (throw 'finish nil))))))))



;;; Event handling exports.  These functions are intended for use by
;;; Lisp commands bound to touch screen gesture events.

;;;###autoload
(defun touch-screen-inhibit-drag ()
  "Inhibit subsequent `touchscreen-drag' events from being sent.
Prevent `touchscreen-drag' and translated mouse events from being
sent until the touch sequence currently being translated ends.
Must be called from a command bound to a `touchscreen-hold' or
`touchscreen-drag' event."
  (let* ((tool touch-screen-current-tool)
         (current-what (nth 3 tool)))
    ;; Signal an error if no hold and no drag is in progress.
    (when (and (not (eq current-what 'held))
               (not (eq current-what 'drag)))
      (error "Calling `touch-screen-inhibit-drag' outside hold or drag"))
    ;; Now set the fourth element of tool to `command-inhibit'.
    (setcar (nthcdr 3 tool) 'command-inhibit)))

;;;###autoload
(defun touch-screen-last-drag-position ()
  "Return the last attested position of the current touch screen tool.
Value is a pair of integers (X . Y) representing the pixel
position of the said tool relative to the frame where it was
placed (not the selected frame), or nil if this function was
not invoked after the generation of a `mouse-movement' or
`down-mouse-1' event by touch screen event translation.

This function must be consulted in preference to
`mouse-absolute-pixel-position' if the latter is required in any
command that handles `mouse-movement' or `down-mouse-1' events."
  (when-let* ((tool touch-screen-current-tool)
              (window (nth 1 tool))
              (pos (nth 10 tool)))
    (and (eq (nth 3 tool) 'mouse-drag)
         (window-live-p window) pos)))



(provide 'touch-screen)

;;; touch-screen.el ends here
