;;; touch-screen.el --- touch screen support for X and Android  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

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
;; It is used on X and Android, where the platform cannot recognize
;; them for us.

;;; Code:

(defvar touch-screen-current-tool nil
  "The touch point currently being tracked, or nil.
If non-nil, this is a list of nine elements: the ID of the touch
point being tracked, the window where the touch began, a cons
containing the last known position of the touch point, relative
to that window, a field used to store data while tracking the
touch point, the initial position of the touchpoint, and another
four fields to used store data while tracking the touch point.
See `touch-screen-handle-point-update' for the meanings of the
fourth element.")

(defvar touch-screen-set-point-commands '(mouse-set-point)
  "List of commands known to set the point.
This is used to determine whether or not to display the on-screen
keyboard after a mouse command is executed in response to a
`touchscreen-end' event.")

(defvar touch-screen-current-timer nil
  "Timer used to track long-presses.
This is always cleared upon any significant state change.")

(defcustom touch-screen-display-keyboard nil
  "If non-nil, always display the on screen keyboard.
A buffer local value means to always display the on screen
keyboard when the buffer is selected."
  :type 'boolean
  :group 'mouse
  :version "30.1")

(defcustom touch-screen-delay 0.7
  "Delay in seconds before Emacs considers a touch to be a long-press."
  :type 'number
  :group 'mouse
  :version "30.1")

(defcustom touch-screen-precision-scroll nil
  "Whether or not to use precision scrolling for touch screens.
See `pixel-scroll-precision-mode' for more details."
  :type 'boolean
  :group 'mouse
  :version "30.1")

(defun touch-screen-relative-xy (posn window)
  "Return the coordinates of POSN, a mouse position list.
However, return the coordinates relative to WINDOW.

If (posn-window posn) is the same as window, simply return the
coordinates in POSN.  Otherwise, convert them to the frame, and
then back again.

If WINDOW is the symbol `frame', simply convert the coordinates
to the frame that they belong in."
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
          (if (> dy 0)
              (pixel-scroll-precision-scroll-down-page dy)
            (pixel-scroll-precision-scroll-up-page (- dy)))
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
    (setq accumulator (+ accumulator dx)) ; Add dx;
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
                         (scroll-right 1)
                         (setq lines-hscrolled (1+ lines-hscrolled))
                         (when (not (zerop accumulator))
                           ;; If there is still an outstanding amount to
                           ;; scroll, do this again.
                           (throw 'again t)))
                     (when (and (> accumulator 0)
                                (>= accumulator column-width))
                       (setq accumulator (- accumulator column-width))
                       (scroll-left 1)
                       (setq lines-hscrolled (1+ lines-hscrolled))
                       (when (not (zerop accumulator))
                         ;; If there is still an outstanding amount to
                         ;; scroll, do this again.
                         (throw 'again t)))))
                 ;; Scrolling is done.  Move the accumulator back to
                 ;; touch-screen-current-tool and break out of the loop.
                 (setcar (nthcdr 6 touch-screen-current-tool) accumulator)
                 (setcar (nthcdr 8 touch-screen-current-tool) lines-hscrolled)
                 nil)))))

(defun touch-screen-handle-timeout (arg)
  "Start the touch screen timeout or handle it depending on ARG.
When ARG is nil, start the `touch-screen-current-timer' to go off
in `touch-screen-delay' seconds, and call this function with ARG
t.

When ARG is t, beep.  Then, set the fourth element of
touch-screen-current-tool to `held', and the mark to the last
known position of the tool."
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
    ;; Beep.
    (beep)
    ;; Set touch-screen-current-timer to nil.
    (setq touch-screen-current-timer nil)
    (when touch-screen-current-tool
      ;; Set the state to `held'.
      (setcar (nthcdr 3 touch-screen-current-tool) 'held)
      ;; Go to the initial position of the touchpoint and activate the
      ;; mark.
      (select-window (cadr touch-screen-current-tool))
      (set-mark (posn-point (nth 4 touch-screen-current-tool)))
      (goto-char (mark))
      (activate-mark))))

(defun touch-screen-handle-point-update (point)
  "Notice that the touch point POINT has changed position.
POINT must be the touch point currently being tracked as
`touch-screen-current-tool'.

If the fourth element of `touch-screen-current-tool' is nil, then
the touch has just begun.  Determine how much POINT has moved.
If POINT has moved upwards or downwards by a significant amount,
then set the fourth element to `scroll'.  Then, call
`touch-screen-handle-scroll' to scroll the display by that
amount.

If the fourth element of `touch-screen-current-tool' is `scroll',
then scroll the display by how much POINT has moved in the Y
axis.

If the fourth element of `touch-screen-current-tool' is `held',
then the touch has been held down for some time.  If motion
happens, cancel `touch-screen-current-timer', and set the field
to `drag'.  Then, activate the mark and start dragging.

If the fourth element of `touch-screen-current-tool' is `drag',
then move point to the position of POINT."
  (let ((window (nth 1 touch-screen-current-tool))
        (what (nth 3 touch-screen-current-tool)))
    (cond ((null what)
           (let* ((posn (cdr point))
                  (last-posn (nth 2 touch-screen-current-tool))
                  ;; Now get the position of X and Y relative to
                  ;; WINDOW.
                  (relative-xy
                   (touch-screen-relative-xy posn window))
                  (diff-x (- (car last-posn) (car relative-xy)))
                  (diff-y (- (cdr last-posn) (cdr relative-xy))))
             ;; Decide whether or not to start scrolling.
             (when (or (> diff-y 10) (> diff-x 10)
                       (< diff-y -10) (< diff-x -10))
               (setcar (nthcdr 3 touch-screen-current-tool)
                       'scroll)
               (setcar (nthcdr 2 touch-screen-current-tool)
                       relative-xy)
               (with-selected-window window
                 (touch-screen-handle-scroll diff-x diff-y))
               ;; Cancel the touch screen long-press timer, if it is
               ;; still there by any chance.
               (when touch-screen-current-timer
                 (cancel-timer touch-screen-current-timer)
                 (setq touch-screen-current-timer nil)))))
          ((eq what 'scroll)
           ;; Cancel the touch screen long-press timer, if it is still
           ;; there by any chance.
           (when touch-screen-current-timer
             (cancel-timer touch-screen-current-timer)
             (setq touch-screen-current-timer nil))
           (let* ((posn (cdr point))
                  (last-posn (nth 2 touch-screen-current-tool))
                  ;; Now get the position of X and Y relative to
                  ;; WINDOW.
                  (relative-xy
                   (touch-screen-relative-xy posn window))
                  (diff-x (- (car last-posn) (car relative-xy)))
                  (diff-y (- (cdr last-posn) (cdr relative-xy))))
             (setcar (nthcdr 3 touch-screen-current-tool)
                     'scroll)
             (setcar (nthcdr 2 touch-screen-current-tool)
                     relative-xy)
             (unless (and (zerop diff-x) (zerop diff-y))
               (with-selected-window window
                 (touch-screen-handle-scroll diff-x diff-y)))))
          ((eq what 'held)
           (let* ((posn (cdr point))
                  (relative-xy
                   (touch-screen-relative-xy posn window)))
             (when touch-screen-current-timer
               (cancel-timer touch-screen-current-timer)
               (setq touch-screen-current-timer nil))
             ;; Now start dragging.
             (setcar (nthcdr 3 touch-screen-current-tool)
                     'drag)
             (setcar (nthcdr 2 touch-screen-current-tool)
                     relative-xy)
             (with-selected-window window
               ;; Activate the mark.  It should have been set by the
               ;; time `touch-screen-timeout' was called.
               (activate-mark)

               ;; Figure out what character to go to.  If this posn is
               ;; in the window, go to (posn-point posn).  If not,
               ;; then go to the line before either window start or
               ;; window end.
               (if (and (eq (posn-window posn) window)
                        (posn-point posn))
                   (goto-char (posn-point posn))
                 (let ((relative-xy
                        (touch-screen-relative-xy posn window)))
                   (let ((scroll-conservatively 101))
                     (cond
                      ((< (cdr relative-xy) 0)
                       (ignore-errors
                         (goto-char (1- (window-start))))
                       (redisplay))
                      ((> (cdr relative-xy)
                          (let ((edges (window-inside-pixel-edges)))
                            (- (nth 3 edges) (cadr edges))))
                       (ignore-errors
                         (goto-char (1+ (window-end nil t))))
                       (redisplay)))))))))
          ((eq what 'drag)
           (let* ((posn (cdr point)))
             ;; Keep dragging.
             (with-selected-window window
               ;; Figure out what character to go to.  If this posn is
               ;; in the window, go to (posn-point posn).  If not,
               ;; then go to the line before either window start or
               ;; window end.
               (if (and (eq (posn-window posn) window)
                        (posn-point posn))
                   (goto-char (posn-point posn))
                 (let ((relative-xy
                        (touch-screen-relative-xy posn window)))
                   (let ((scroll-conservatively 101))
                     (cond
                      ((< (cdr relative-xy) 0)
                       (ignore-errors
                         (goto-char (1- (window-start))))
                       (redisplay))
                      ((> (cdr relative-xy)
                          (let ((edges (window-inside-pixel-edges)))
                            (- (nth 3 edges) (cadr edges))))
                       (ignore-errors
                         (goto-char (1+ (window-end nil t))))
                       (redisplay))))))))))))

(defun touch-screen-window-selection-changed (frame)
  "Notice that FRAME's selected window has changed.
If point is now on read only text, hide the on screen keyboard.
Otherwise, cancel any timer that is supposed to hide the keyboard
in response to the minibuffer being closed."
  (with-selected-frame frame
    (if (or buffer-read-only
            (get-text-property (point) 'read-only))
        (frame-toggle-on-screen-keyboard (selected-frame) t)
      ;; Prevent hiding the minibuffer from hiding the on screen
      ;; keyboard.
      (when minibuffer-on-screen-keyboard-timer
        (cancel-timer minibuffer-on-screen-keyboard-timer)
        (setq minibuffer-on-screen-keyboard-timer nil)))))

(defun touch-screen-handle-point-up (point)
  "Notice that POINT has been removed from the screen.
POINT should be the point currently tracked as
`touch-screen-current-tool'.

If the fourth argument of `touch-screen-current-tool' is nil,
move point to the position of POINT, selecting the window under
POINT as well, and deactivate the mark; if there is a button or
link at POINT, call the command bound to `mouse-2' there.
Otherwise, call the command bound to `mouse-1'.

If the command being executed is listed in
`touch-screen-set-point-commands' also display the on-screen
keyboard if the current buffer and the character at the new point
is not read-only."
  (let ((what (nth 3 touch-screen-current-tool)))
    (cond ((null what)
           (when (windowp (posn-window (cdr point)))
             ;; Select the window that was tapped.
             (select-window (posn-window (cdr point)))
             ;; Now simulate a mouse click there.  If there is a link
             ;; or a button, use mouse-2 to push it.
             (let ((event (list (if (or (mouse-on-link-p (cdr point))
                                        (button-at (posn-point (cdr point))))
                                    'mouse-2
                                  'mouse-1)
                                (cdr point)))
                   ;; Look for an extra keymap to look in.
                   (keymap (and (posn-object (cdr point))
                                (stringp
                                 (posn-object (cdr point)))
                                (get-text-property
                                 0 'keymap
                                 (posn-object (cdr point)))))
                   command)
               (save-excursion
                 (when (posn-point (cdr point))
                   (goto-char (posn-point (cdr point))))
                 (if keymap
                     (setq keymap (cons keymap (current-active-maps t)))
                   (setq keymap (current-active-maps t)))
                 (setq command (lookup-key keymap (vector (car event)))))
               (deactivate-mark)
               ;; This is necessary for following links.
               (goto-char (posn-point (cdr point)))
               ;; Figure out if the on screen keyboard needs to be
               ;; displayed.
               (when command
                 (call-interactively command nil
                                     (vector event))
                 (when (memq command touch-screen-set-point-commands)
                   (if (and (or (not buffer-read-only)
                                touch-screen-display-keyboard)
                            ;; Detect the splash screen and avoid
                            ;; displaying the on screen keyboard
                            ;; there.
                            (not (equal (buffer-name) "*GNU Emacs*")))
                       ;; Once the on-screen keyboard has been opened,
                       ;; add `touch-screen-window-selection-changed'
                       ;; as a window selection change function This
                       ;; allows the on screen keyboard to be hidden
                       ;; if the selected window's point becomes read
                       ;; only at some point in the future.
                       (progn
                         (add-hook 'window-selection-change-functions
                                   #'touch-screen-window-selection-changed)
                         (frame-toggle-on-screen-keyboard (selected-frame) nil))
                     ;; Otherwise, hide the on screen keyboard now.
                     (frame-toggle-on-screen-keyboard (selected-frame) t))))))))))

(defun touch-screen-handle-touch (event)
  "Handle a single touch EVENT, and perform associated actions.
EVENT can either be a touchscreen-begin, touchscreen-update or
touchscreen-end event."
  (interactive "e")
  (cond
   ((eq (car event) 'touchscreen-begin)
    ;; A tool was just pressed against the screen.  Figure out the
    ;; window where it is and make it the tool being tracked on the
    ;; window.
    (let ((touchpoint (caadr event))
          (position (cdadr event)))
      ;; Cancel the touch screen timer, if it is still there by any
      ;; chance.
      (when touch-screen-current-timer
        (cancel-timer touch-screen-current-timer)
        (setq touch-screen-current-timer nil))
      ;; Replace any previously ongoing gesture.  If POSITION has no
      ;; window or position, make it nil instead.
      (setq touch-screen-current-tool (and (windowp (posn-window position))
                                           (posn-point position)
                                           (list touchpoint
                                                 (posn-window position)
                                                 (posn-x-y position)
                                                 nil position nil nil
                                                 nil nil)))
      ;; Start the long-press timer.
      (touch-screen-handle-timeout nil)))
   ((eq (car event) 'touchscreen-update)
    ;; The positions of tools currently pressed against the screen
    ;; have changed.  If there is a tool being tracked as part of a
    ;; gesture, look it up in the list of tools.
    (let ((new-point (assq (car touch-screen-current-tool)
                           (cadr event))))
      (when new-point
        (touch-screen-handle-point-update new-point))))
   ((eq (car event) 'touchscreen-end)
    ;; A tool has been removed from the screen.  If it is the tool
    ;; currently being tracked, clear `touch-screen-current-tool'.
    (when (eq (caadr event) (car touch-screen-current-tool))
      ;; Cancel the touch screen long-press timer, if it is still there
      ;; by any chance.
      (when touch-screen-current-timer
        (cancel-timer touch-screen-current-timer)
        (setq touch-screen-current-timer nil))
      (touch-screen-handle-point-up (cadr event))
      (setq touch-screen-current-tool nil)))))

(define-key global-map [touchscreen-begin] #'touch-screen-handle-touch)
(define-key global-map [touchscreen-update] #'touch-screen-handle-touch)
(define-key global-map [touchscreen-end] #'touch-screen-handle-touch)


;; Exports.  These functions are intended for use externally.

(defun touch-screen-track-tap (event &optional update data)
  "Track a single tap starting from EVENT.
EVENT should be a `touchscreen-begin' event.

Read touch screen events until a `touchscreen-end' event is
received with the same ID as in EVENT.  If UPDATE is non-nil and
a `touchscreen-update' event is received in the mean time and
contains a touch point with the same ID as in EVENT, call UPDATE
with that event and DATA.

Return nil immediately if any other kind of event is received;
otherwise, return t once the `touchscreen-end' event arrives."
  (let ((disable-inhibit-text-conversion t))
    (catch 'finish
      (while t
        (let ((new-event (read-event nil)))
          (cond
           ((eq (car-safe new-event) 'touchscreen-update)
            (when (and update (assq (caadr event) (cadr new-event)))
              (funcall update new-event data)))
           ((eq (car-safe new-event) 'touchscreen-end)
            (throw 'finish
                   ;; Now determine whether or not the `touchscreen-end'
                   ;; event has the same ID as EVENT.  If it doesn't,
                   ;; then this is another touch, so return nil.
                   (eq (caadr event) (caadr new-event))))
           (t (throw 'finish nil))))))))

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



;; Modeline dragging.

(defun touch-screen-drag-mode-line-1 (event)
  "Internal helper for `touch-screen-drag-mode-line'.
This is called when that function determines that no drag really
happened.  EVENT is the same as in `touch-screen-drag-mode-line'."
  ;; If there is an object at EVENT, then look either a keymap bound
  ;; to [down-mouse-1] or a command bound to [mouse-1].  Then, if a
  ;; keymap was found, pop it up as a menu.  Otherwise, wait for a tap
  ;; to complete and run the command found.
  ;; Also, select the window in EVENT.
  (select-window (posn-window (cdadr event)))
  (let* ((object (posn-object (cdadr event)))
         (object-keymap (and (consp object)
                             (stringp (car object))
                             (or (get-text-property (cdr object)
                                                    'keymap
                                                    (car object))
                                 (get-text-property (cdr object)
                                                    'local-map
                                                    (car object)))))
         (keymap (lookup-key object-keymap [mode-line down-mouse-1]))
         (command (or (lookup-key object-keymap [mode-line mouse-1])
                      keymap)))
    (when (or (keymapp keymap) command)
      (if (keymapp keymap)
          (when-let* ((command (x-popup-menu event keymap))
                      (tem (lookup-key keymap
                                       (if (consp command)
                                           (apply #'vector command)
                                         (vector command))
                                       t)))
            (call-interactively tem))
        (when (commandp command)
          (call-interactively command nil
                              (vector (list 'mouse-1 (cdadr event)))))))))

(defun touch-screen-drag-mode-line (event)
  "Begin dragging the mode line in response to a touch EVENT.
Change the height of the window based on where the touch point in
EVENT moves.

If it does not actually move anywhere and the touch point is
removed, and EVENT lies on top of text with a mouse command
bound, run that command instead."
  (interactive "e")
  ;; Find the window that should be dragged and the starting position.
  (let* ((window (posn-window (cdadr event)))
         (relative-xy (touch-screen-relative-xy (cdadr event)
                                                'frame))
         (last-position (cdr relative-xy)))
    (when (window-resizable window 0)
      (when (eq
             (touch-screen-track-drag
              event (lambda (new-event &optional _data)
                      ;; Find the position of the touchpoint in
                      ;; NEW-EVENT.
                      (let* ((touchpoint (assq (caadr event)
                                               (cadr new-event)))
                             (new-relative-xy
                              (touch-screen-relative-xy (cdr touchpoint) 'frame))
                             (position (cdr new-relative-xy))
                             (window-resize-pixelwise t)
                             growth)
                        ;; Now set the new height of the window.  If
                        ;; new-relative-y is above relative-xy, then
                        ;; make the window that much shorter.
                        ;; Otherwise, make it bigger.
                        (unless (or (zerop (setq growth
                                                 (- position last-position)))
                                    (and (> growth 0)
                                         (< position
                                            (+ (window-pixel-top window)
                                               (window-pixel-height window))))
                                    (and (< growth 0)
                                         (> position
                                            (+ (window-pixel-top window)
                                               (window-pixel-height window)))))
                          (when (ignore-errors
                                  (adjust-window-trailing-edge window growth nil t) t)
                            (setq last-position position))))))
             'no-drag)
        ;; Dragging did not actually happen, so try to run any command
        ;; necessary.
        (touch-screen-drag-mode-line-1 event)))))

(global-set-key [mode-line touchscreen-begin]
                #'touch-screen-drag-mode-line)
(global-set-key [bottom-divider touchscreen-begin]
                #'touch-screen-drag-mode-line)

(provide 'touch-screen)

;;; touch-screen ends here
