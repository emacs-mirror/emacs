;;; lucid.el --- Emulate some Lucid Emacs functions.
;; Copyright (C) 1993, 1995 Free Software Foundation, Inc.

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


(defun add-timeout (secs function object &optional resignal)
  (run-at-time secs resignal function object))

(defun disable-timeout (timeout)
  (cancel-timer timeout))

(defun copy-tree (tree)
  (if (consp tree)
      (cons (copy-tree (car tree))
	    (copy-tree (cdr tree)))
    (if (vectorp tree)
	(let* ((new (copy-sequence tree))
	       (i (1- (length new))))
	  (while (>= i 0)
	    (aset new i (copy-tree (aref new i)))
	    (setq i (1- i)))
	  new)
      tree)))

(defalias 'current-time-seconds 'current-time)

(defun keymap-parent (keymap)
  (let ((tail (cdr keymap)))
    (while (and tail (not (eq (car tail) 'keymap)))
      (setq tail (cdr tail)))
    tail))

(defun set-keymap-parent (keymap new-parent)
  (let ((tail keymap))
    (while (and tail (cdr tail) (not (eq (car (cdr tail)) 'keymap)))
      (setq tail (cdr tail)))
    (if tail
	(setcdr tail new-parent))))

(defun remprop (symbol prop)
  (let ((plist (symbol-plist symbol)))
    (while (eq (car plist) prop)
      (setplist symbol (setq plist (cdr (cdr plist)))))
    (while plist
      (if (eq (nth 2 plist) prop)
	  (setcdr (cdr plist) (nthcdr 4 plist)))
      (setq plist (cdr (cdr plist))))))

(defun map-keymap (function keymap &optional sort-first)
  "Call FUNCTION for every binding in KEYMAP.
This includes bindings inherited from a parent keymap.
FUNCTION receives two arguments each time it is called:
the character (more generally, the event type) that is bound,
and the binding it has.

Note that passing the event type directly to `define-key' does not work
in Emacs 19.  We do not emulate that particular feature of Lucid Emacs.
If your code does that, modify it to make a vector containing the event
type that you get.  That will work in both versions of Emacs."
  (if sort-first
      (let (list)
	(map-keymap (function (lambda (a b)
				(setq list (cons (cons a b) list))))
		    keymap)
	(setq list (sort list
			 (function (lambda (a b)
				     (setq a (car a) b (car b))
				     (if (integerp a)
					 (if (integerp b) (< a b)
					   t)
				       (if (integerp b) t
					 (string< a b)))))))
	(while list
	  (funcall function (car (car list)) (cdr (car list)))
	  (setq list (cdr list))))
    (while (consp keymap)
      (if (consp (car keymap))
	  (funcall function (car (car keymap)) (cdr (car keymap)))
	(if (vectorp (car keymap))
	    (let ((i (1- (length (car keymap))))
		  (vector (car keymap)))
	      (while (>= i 0)
		(funcall function i (aref vector i))
		(setq i (1- i))))))
      (setq keymap (cdr keymap)))))

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

(defalias 'find-face 'internal-find-face)
(defalias 'get-face 'internal-get-face)
(defalias 'try-face-font 'internal-try-face-font)

(defun make-extent (beg end &optional buffer)
  (make-overlay beg end buffer))

(defun set-extent-property (extent prop value)
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
(defalias 'screen-height 'frame-height)
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
(defalias 'screen-width 'frame-width)
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

;;; end of lucid.el
