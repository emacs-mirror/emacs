;;; lucid.el --- emulate some Lucid Emacs functions

;; Copyright (C) 1993, 1995, 2001 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: emulations

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

;;; Code:

;; XEmacs autoloads CL so we might as well make use of it.
(require 'cl)

(defalias 'current-time-seconds 'current-time)

(defun map-keymap (function keymap &optional sort-first)
  "Call FUNCTION for every binding in KEYMAP.
This does not include bindings inherited from a parent keymap.
FUNCTION receives two arguments each time it is called:
the character (more generally, the event type) that is bound,
and the binding it has.

Note that passing the event type directly to `define-key' does not work
in Emacs 19.  We do not emulate that particular feature of Lucid Emacs.
If your code does that, modify it to make a vector containing the event
type that you get.  That will work in both versions of Emacs."
  (if sort-first
      (let (list)
	(cl-map-keymap (lambda (a b) (push (cons a b) list))
		       keymap)
	(setq list (sort list
			 (lambda (a b)
			   (setq a (car a) b (car b))
			   (if (integerp a)
			       (if (integerp b) (< a b)
				 t)
			     (if (integerp b) t
			       (string< a b))))))
	(dolist (p list)
	  (funcall function (car p) (cdr p))))
    (cl-map-keymap function keymap)))

(defun read-number (prompt &optional integers-only)
  "Read a number from the minibuffer.
Keep reentering the minibuffer until we get suitable input.
If optional argument INTEGERS-ONLY is non-nil, insist on an integer."
  (interactive)
  (let (success
	(number nil)
	(predicate (if integers-only 'integerp 'numberp)))
    (while (not success)
      (let ((input-string (read-string prompt)))
	(condition-case ()
	    (setq number (read input-string))
	  (error))
	(if (funcall predicate number)
	    (setq success t)
	  (let ((cursor-in-echo-area t))
	    (message "Please type %s"
		     (if integers-only "an integer" "a number"))
	    (sit-for 1)))))
    number))

(defun real-path-name (name &optional default)
  (file-truename (expand-file-name name default)))

;; It's not clear what to return if the mouse is not in FRAME.
(defun read-mouse-position (frame)
  (let ((pos (mouse-position)))
    (if (eq (car pos) frame)
	(cdr pos))))

(defun switch-to-other-buffer (arg)
  "Switch to the previous buffer.
With a numeric arg N, switch to the Nth most recent buffer.
With an arg of 0, buries the current buffer at the
bottom of the buffer stack."
  (interactive "p")
  (if (eq arg 0)
      (bury-buffer (current-buffer)))
  (switch-to-buffer
   (if (<= arg 1) (other-buffer (current-buffer))
     (nth arg
	  (apply 'nconc
		 (mapcar
		  (lambda (buf)
		    (if (= ?\  (string-to-char (buffer-name buf)))
			nil
		      (list buf)))
		  (buffer-list)))))))

(defun device-class (&optional device)
  "Return the class (color behavior) of DEVICE.
This will be one of 'color, 'grayscale, or 'mono.
This function exists for compatibility with XEmacs."
  (cond
   ((display-color-p device) 'color)
   ((display-grayscale-p device) 'grayscale)
   (t 'mono)))

(defalias 'find-face 'internal-find-face)
(defalias 'get-face 'internal-get-face)
(defalias 'try-face-font 'internal-try-face-font)

(defalias 'exec-to-string 'shell-command-to-string)


;; Buffer context

(defun buffer-syntactic-context (&optional buffer)
  "Syntactic context at point in BUFFER.
Either of `string', `comment' or `nil'.
This is an XEmacs compatibility function."
  (with-current-buffer (or buffer (current-buffer))
    (let ((state (syntax-ppss (point))))
      (cond
       ((nth 3 state) 'string)
       ((nth 4 state) 'comment)))))


(defun buffer-syntactic-context-depth (&optional buffer)
  "Syntactic parenthesis depth at point in BUFFER.
This is an XEmacs compatibility function."
  (with-current-buffer (or buffer (current-buffer))
    (nth 0 (syntax-ppss (point)))))


;; Extents
(defun make-extent (beg end &optional buffer)
  (make-overlay beg end buffer))

(defun extent-properties (extent) (overlay-properties extent))
(unless (fboundp 'extent-property) (defalias 'extent-property 'overlay-get))

(defun extent-at (pos &optional object property before)
  (with-current-buffer (or object (current-buffer))
    (let ((overlays (overlays-at pos)))
      (when property
	(let (filtered)
	  (while overlays
	    (if (overlay-get (car overlays) property)
		(setq filtered (cons (car overlays) filtered)))
	    (setq overlays (cdr overlays)))
	  (setq overlays filtered)))
      (setq overlays
	    (sort overlays
		  (function (lambda (o1 o2)
			      (let ((p1 (or (overlay-get o1 'priority) 0))
				    (p2 (or (overlay-get o2 'priority) 0)))
				(or (> p1 p2)
				    (and (= p1 p2)
					 (> (overlay-start o1) (overlay-start o2)))))))))
      (if before
	  (nth 1 (memq before overlays))
	(car overlays)))))

(defun set-extent-property (extent prop value)
  ;; Make sure that separate adjacent extents
  ;; with the same mouse-face value
  ;; do not run together as one extent.
  (and (eq prop 'mouse-face)
       (symbolp value)
       (setq value (list value)))
  (if (eq prop 'duplicable)
      (cond ((and value (not (overlay-get extent prop)))
	     ;; If becoming duplicable, copy all overlayprops to text props.
	     (add-text-properties (overlay-start extent)
				  (overlay-end extent)
				  (overlay-properties extent)
				  (overlay-buffer extent)))
	    ;; If becoming no longer duplicable, remove these text props.
	    ((and (not value) (overlay-get extent prop))
	     (remove-text-properties (overlay-start extent)
				     (overlay-end extent)
				     (overlay-properties extent)
				     (overlay-buffer extent))))
    ;; If extent is already duplicable, put this property
    ;; on the text as well as on the overlay.
    (if (overlay-get extent 'duplicable)
	(put-text-property  (overlay-start extent)
			    (overlay-end extent)
			    prop value (overlay-buffer extent))))
  (overlay-put extent prop value))

(defun set-extent-face (extent face)
  (set-extent-property extent 'face face))

(defun set-extent-end-glyph (extent glyph)
  (set-extent-property extent 'after-string glyph))

(defun delete-extent (extent)
  (set-extent-property extent 'duplicable nil)
  (delete-overlay extent))

;; Support the Lucid names with `screen' instead of `frame'.

(defalias 'current-screen-configuration 'current-frame-configuration)
(defalias 'delete-screen 'delete-frame)
(defalias 'find-file-new-screen 'find-file-other-frame)
(defalias 'find-file-read-only-new-screen 'find-file-read-only-other-frame)
(defalias 'find-tag-new-screen 'find-tag-other-frame)
;;(defalias 'focus-screen 'focus-frame)
(defalias 'iconify-screen 'iconify-frame)
(defalias 'mail-new-screen 'mail-other-frame)
(defalias 'make-screen-invisible 'make-frame-invisible)
(defalias 'make-screen-visible 'make-frame-visible)
;; (defalias 'minibuffer-screen-list 'minibuffer-frame-list)
(defalias 'modify-screen-parameters 'modify-frame-parameters)
(defalias 'next-screen 'next-frame)
;; (defalias 'next-multiscreen-window 'next-multiframe-window)
;; (defalias 'previous-multiscreen-window 'previous-multiframe-window)
;; (defalias 'redirect-screen-focus 'redirect-frame-focus)
(defalias 'redraw-screen 'redraw-frame)
;; (defalias 'screen-char-height 'frame-char-height)
;; (defalias 'screen-char-width 'frame-char-width)
;; (defalias 'screen-configuration-to-register 'frame-configuration-to-register)
;; (defalias 'screen-focus 'frame-focus)
(defalias 'screen-list 'frame-list)
;; (defalias 'screen-live-p 'frame-live-p)
(defalias 'screen-parameters 'frame-parameters)
(defalias 'screen-pixel-height 'frame-pixel-height)
(defalias 'screen-pixel-width 'frame-pixel-width)
(defalias 'screen-root-window 'frame-root-window)
(defalias 'screen-selected-window 'frame-selected-window)
(defalias 'lower-screen 'lower-frame)
(defalias 'raise-screen 'raise-frame)
(defalias 'screen-visible-p 'frame-visible-p)
(defalias 'screenp 'framep)
(defalias 'select-screen 'select-frame)
(defalias 'selected-screen 'selected-frame)
;; (defalias 'set-screen-configuration 'set-frame-configuration)
;; (defalias 'set-screen-height 'set-frame-height)
(defalias 'set-screen-position 'set-frame-position)
(defalias 'set-screen-size 'set-frame-size)
;; (defalias 'set-screen-width 'set-frame-width)
(defalias 'switch-to-buffer-new-screen 'switch-to-buffer-other-frame)
;; (defalias 'unfocus-screen 'unfocus-frame)
(defalias 'visible-screen-list 'visible-frame-list)
(defalias 'window-screen 'window-frame)
(defalias 'x-create-screen 'x-create-frame)
(defalias 'x-new-screen 'make-frame)

(provide 'lucid)

;;; lucid.el ends here
