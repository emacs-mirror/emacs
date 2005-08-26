;;; lselect.el --- Lucid interface to X Selections

;; Copyright (C) 1990, 1993, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: emulations

;; This won't completely work until we support or emulate Lucid-style extents.
;; Based on Lucid's selection code.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

;; The selection code requires us to use certain symbols whose names are
;; all upper-case; this may seem tasteless, but it makes there be a 1:1
;; correspondence between these symbols and X Atoms (which are upcased.)

(defalias 'x-get-cutbuffer 'x-get-cut-buffer)
(defalias 'x-store-cutbuffer 'x-set-cut-buffer)

(or (facep 'primary-selection)
    (make-face 'primary-selection))

(or (facep 'secondary-selection)
    (make-face 'secondary-selection))

(defun x-get-secondary-selection ()
  "Return text selected from some X window."
  (x-get-selection-internal 'SECONDARY 'STRING))

(defvar primary-selection-extent nil
  "The extent of the primary selection; don't use this.")

(defvar secondary-selection-extent nil
  "The extent of the secondary selection; don't use this.")


(defun x-select-make-extent-for-selection (selection previous-extent face)
  ;; Given a selection, this makes an extent in the buffer which holds that
  ;; selection, for highlighting purposes.  If the selection isn't associated
  ;; with a buffer, this does nothing.
  (let ((buffer nil)
	(valid (and (extentp previous-extent)
		    (extent-buffer previous-extent)
		    (buffer-name (extent-buffer previous-extent))))
	start end)
    (cond ((stringp selection)
	   ;; if we're selecting a string, lose the previous extent used
	   ;; to highlight the selection.
	   (setq valid nil))
	  ((consp selection)
	   (setq start (min (car selection) (cdr selection))
		 end (max (car selection) (cdr selection))
		 valid (and valid
			    (eq (marker-buffer (car selection))
				(extent-buffer previous-extent)))
		 buffer (marker-buffer (car selection))))
	  ((extentp selection)
	   (setq start (extent-start-position selection)
		 end (extent-end-position selection)
		 valid (and valid
			    (eq (extent-buffer selection)
				(extent-buffer previous-extent)))
		 buffer (extent-buffer selection)))
	  )
    (if (and (not valid)
	     (extentp previous-extent)
	     (extent-buffer previous-extent)
	     (buffer-name (extent-buffer previous-extent)))
	(delete-extent previous-extent))
    (if (not buffer)
	;; string case
	nil
      ;; normal case
      (if valid
	  (set-extent-endpoints previous-extent start end)
	(setq previous-extent (make-extent start end buffer))
	;; use same priority as mouse-highlighting so that conflicts between
	;; the selection extent and a mouse-highlighted extent are resolved
	;; by the usual size-and-endpoint-comparison method.
	(set-extent-priority previous-extent mouse-highlight-priority)
	(set-extent-face previous-extent face)))))


(defun x-own-selection (selection &optional type)
  "Make a primary X Selection of the given argument.
The argument may be a string, a cons of two markers, or an extent.
In the latter cases the selection is considered to be the text
between the markers, or the between extents endpoints."
  (interactive (if (not current-prefix-arg)
		   (list (read-string "Store text for pasting: "))
		 (list (cons ;; these need not be ordered.
			(copy-marker (point-marker))
			(copy-marker (mark-marker))))))
  (or type (setq type 'PRIMARY))
  (x-set-selection selection type)
  (cond ((eq type 'PRIMARY)
	 (setq primary-selection-extent
	       (x-select-make-extent-for-selection
		selection primary-selection-extent 'primary-selection)))
	((eq type 'SECONDARY)
	 (setq secondary-selection-extent
	       (x-select-make-extent-for-selection
		selection secondary-selection-extent 'secondary-selection))))
  selection)


(defun x-own-secondary-selection (selection &optional type)
  "Make a secondary X Selection of the given argument.  The argument may be a
string or a cons of two markers (in which case the selection is considered to
be the text between those markers.)"
  (interactive (if (not current-prefix-arg)
		   (list (read-string "Store text for pasting: "))
		 (list (cons ;; these need not be ordered.
			(copy-marker (point-marker))
			(copy-marker (mark-marker))))))
  (x-own-selection selection 'SECONDARY))


(defun x-own-clipboard (string)
  "Paste the given string to the X Clipboard."
  (x-own-selection string 'CLIPBOARD))


(defun x-disown-selection (&optional secondary-p)
  "Assuming we own the selection, disown it.  With an argument, discard the
secondary selection instead of the primary selection."
  (x-disown-selection-internal (if secondary-p 'SECONDARY 'PRIMARY)))

(defun x-dehilight-selection (selection)
  "for use as a value of `x-lost-selection-functions'."
  (cond ((eq selection 'PRIMARY)
	 (if primary-selection-extent
	     (let ((inhibit-quit t))
	       (delete-extent primary-selection-extent)
	       (setq primary-selection-extent nil)))
	 (if zmacs-regions (zmacs-deactivate-region)))
	((eq selection 'SECONDARY)
	 (if secondary-selection-extent
	     (let ((inhibit-quit t))
	       (delete-extent secondary-selection-extent)
	       (setq secondary-selection-extent nil)))))
  nil)

(setq x-lost-selection-functions 'x-dehilight-selection)

(defun x-notice-selection-requests (selection type successful)
  "for possible use as the value of `x-sent-selection-functions'."
  (if (not successful)
      (message "Selection request failed to convert %s to %s"
	       selection type)
    (message "Sent selection %s as %s" selection type)))

(defun x-notice-selection-failures (selection type successful)
  "for possible use as the value of `x-sent-selection-functions'."
  (or successful
      (message "Selection request failed to convert %s to %s"
	       selection type)))

;(setq x-sent-selection-functions 'x-notice-selection-requests)
;(setq x-sent-selection-functions 'x-notice-selection-failures)


;; Random utility functions

(defun x-kill-primary-selection ()
  "If there is a selection, delete the text it covers, and copy it to
both the kill ring and the Clipboard."
  (interactive)
  (or (x-selection-owner-p) (error "emacs does not own the primary selection"))
  (setq last-command nil)
  (or primary-selection-extent
      (error "the primary selection is not an extent?"))
  (save-excursion
    (set-buffer (extent-buffer primary-selection-extent))
    (kill-region (extent-start-position primary-selection-extent)
		 (extent-end-position primary-selection-extent)))
  (x-disown-selection nil))

(defun x-delete-primary-selection ()
  "If there is a selection, delete the text it covers *without* copying it to
the kill ring or the Clipboard."
  (interactive)
  (or (x-selection-owner-p) (error "emacs does not own the primary selection"))
  (setq last-command nil)
  (or primary-selection-extent
      (error "the primary selection is not an extent?"))
  (save-excursion
    (set-buffer (extent-buffer primary-selection-extent))
    (delete-region (extent-start-position primary-selection-extent)
		   (extent-end-position primary-selection-extent)))
  (x-disown-selection nil))

(defun x-copy-primary-selection ()
  "If there is a selection, copy it to both the kill ring and the Clipboard."
  (interactive)
  (setq last-command nil)
  (or (x-selection-owner-p) (error "emacs does not own the primary selection"))
  (or primary-selection-extent
      (error "the primary selection is not an extent?"))
  (save-excursion
    (set-buffer (extent-buffer primary-selection-extent))
    (copy-region-as-kill (extent-start-position primary-selection-extent)
			 (extent-end-position primary-selection-extent))))

(defun x-yank-clipboard-selection ()
  "If someone owns a Clipboard selection, insert it at point."
  (interactive)
  (setq last-command nil)
  (let ((clip (x-get-clipboard)))
    (or clip (error "there is no clipboard selection"))
    (push-mark)
    (insert clip)))

(provide 'lselect)

;; arch-tag: 92fa54d4-c5d1-4e9b-ad58-cf1e13930556
;;; lselect.el ends here
