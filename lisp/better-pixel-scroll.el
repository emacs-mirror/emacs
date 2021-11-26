;;; better-pixel-scroll.el --- Pixel scrolling support  -*- lexical-binding:t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

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

;; This enables the use of smooth scroll events provided by XInput 2
;; or NS to scroll the display according to the user's precise turning
;; of the mouse wheel.

;;; Code:

(require 'mwheel)
(require 'subr-x)

(defvar x-coalesce-scroll-events)

(defvar better-pixel-scroll-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [wheel-down] #'better-pixel-scroll)
    (define-key map [wheel-up] #'better-pixel-scroll)
    map)
  "The key map used by `better-pixel-scroll-mode'.")

(defun better-pixel-scroll-scroll-down (delta)
  "Scroll the current window down by DELTA pixels.
Note that this function doesn't work if DELTA is larger than
the height of the current window."
  (when-let* ((posn (posn-at-point))
	      (current-y (cdr (posn-x-y posn)))
	      (min-y (+ (window-tab-line-height)
		        (window-header-line-height)))
              (cursor-height (line-pixel-height))
              (window-height (window-text-height nil t))
              (next-height (save-excursion
                             (vertical-motion 1)
                             (line-pixel-height))))
    (if (and (> delta 0)
             (<= cursor-height window-height))
	(while (< (- current-y min-y) delta)
	  (vertical-motion 1)
          (setq current-y (+ current-y
                             (line-pixel-height)))
	  (when (eobp)
	    (error "End of buffer")))
      (when (< (- (cdr (posn-object-width-height posn))
                  (cdr (posn-object-x-y posn)))
               (- window-height next-height))
        (vertical-motion 1)
        (setq posn (posn-at-point)
              current-y (cdr (posn-x-y posn)))
        (while (< (- current-y min-y) delta)
	  (vertical-motion 1)
          (setq current-y (+ current-y
                             (line-pixel-height)))
	  (when (eobp)
	    (error "End of buffer")))))
    (let* ((desired-pos (posn-at-x-y 0 (+ delta
					  (window-tab-line-height)
					  (window-header-line-height))))
	   (desired-start (posn-point desired-pos))
	   (desired-vscroll (cdr (posn-object-x-y desired-pos))))
      (unless (eq (window-start) desired-start)
        (set-window-start nil desired-start t))
      (set-window-vscroll nil desired-vscroll t))))

(defun better-pixel-scroll-scroll-up (delta)
  "Scroll the current window up by DELTA pixels."
  (when-let* ((max-y (- (window-text-height nil t)
		        (window-tab-line-height)
		        (window-header-line-height)))
	      (posn (posn-at-point))
	      (current-y (+ (cdr (posn-x-y posn))
		            (cdr (posn-object-width-height posn)))))
    (while (< (- max-y current-y) delta)
      (when (zerop (vertical-motion -1))
	(set-window-vscroll nil 0)
	(signal 'beginning-of-buffer nil))
      (setq current-y (- current-y (line-pixel-height)))))
  (while (> delta 0)
    (set-window-start nil (save-excursion
                            (goto-char (window-start))
                            (when (zerop (vertical-motion -1))
			      (set-window-vscroll nil 0)
			      (signal 'beginning-of-buffer nil))
                            (setq delta (- delta (line-pixel-height)))
                            (point))
		      t))
  (when (< delta 0)
    (when-let* ((desired-pos (posn-at-x-y 0 (+ (- delta)
					  (window-tab-line-height)
					  (window-header-line-height))))
	        (desired-start (posn-point desired-pos))
	        (desired-vscroll (cdr (posn-object-x-y desired-pos))))
      (unless (eq (window-start) desired-start)
        (set-window-start nil desired-start t))
      (set-window-vscroll nil desired-vscroll t))))

(defun better-pixel-scroll (event &optional arg)
  "Scroll the display according to EVENT.
Take into account any pixel deltas in EVENT to scroll the display
according to the user's turning the mouse wheel.  If EVENT does
not have precise scrolling deltas, call `mwheel-scroll' instead.
ARG is passed to `mwheel-scroll', should that be called."
  (interactive (list last-input-event current-prefix-arg))
  (let ((window (mwheel-event-window event)))
    (if (and (nth 4 event)
             (zerop (window-hscroll window)))
        (let ((delta (round (cdr (nth 4 event)))))
          (if (> (abs delta) (window-text-height window t))
              (mwheel-scroll event arg)
            (with-selected-window window
              (if (< delta 0)
	          (better-pixel-scroll-scroll-down (- delta))
                (better-pixel-scroll-scroll-up delta)))))
      (mwheel-scroll event arg))))

;;;###autoload
(define-minor-mode better-pixel-scroll-mode
  "Toggle pixel scrolling.
When enabled, this minor mode allows to scroll the display
precisely, according to the turning of the mouse wheel."
  :global t
  :group 'mouse
  :keymap better-pixel-scroll-mode-map
  (setq x-coalesce-scroll-events
        (not better-pixel-scroll-mode)))

(provide 'better-pixel-scroll)

;;; better-pixel-scroll.el ends here.
