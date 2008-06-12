;;; face-remap.el --- Functions for managing `face-remapping-alist'
;;
;; Copyright (C) 2008 Free Software Foundation, Inc.
;;
;; Author: Miles Bader <miles@gnu.org>
;; Keywords: faces face display user commands
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;

;;; Commentary:

;;
;; This file defines some simple operations that can be used for
;; maintaining the `face-remapping-alist' in a cooperative way.  This is
;; especially important for the `default' face.
;;
;; Each face-remapping definition in `face-remapping-alist' added by
;; this code uses the form:
;;
;;   (face RELATIVE_SPECS_1 RELATIVE_SPECS_2 ... BASE_SPECS)
;;
;; The "specs" values are a lists of face names or face attribute-value
;; pairs, and are merged together, with earlier values taking precedence.
;;
;; The RELATIVE_SPECS_* values are added by `add-relative-face-remapping'
;; (and removed by `remove-relative-face-remapping', and are intended for
;; face "modifications" (such as increasing the size).  Typical users of
;; relative specs would be minor modes.
;;
;; BASE_SPECS is the lowest-priority value, and by default is just the
;; face name, which causes the global definition of that face to be used.
;;
;; A non-default value of BASE_SPECS may also be set using
;; `set-base-face-remapping'.  Because this _overwrites_ the default
;; value inheriting from the global face definition, it is up to the
;; caller of set-base-face-remapping to add such inheritance if it is
;; desired.  A typical use of set-base-face-remapping would be a major
;; mode setting face remappings, e.g., of the default face.
;;
;; All modifications cause face-remapping-alist to be made buffer-local.
;;


;;; Code:


;; ----------------------------------------------------------------
;; Utility functions

;;;### autoload
(defun add-relative-face-remapping (face &rest specs)
  "Add a face remapping entry of FACE to SPECS in the current buffer.

Return a cookie which can be used to delete the remapping with
`remove-relative-face-remapping'.

SPECS can be any value suitable for the `face' text property,
including a face name, a list of face names, or a face-attribute
property list.  The attributes given by SPECS will be merged with
any other currently active face remappings of FACE, and with the
global definition of FACE, with the most recently added relative
remapping taking precedence.

The base (lowest priority) remapping may be set to a specific
value, instead of the default of the global face definition,
using `set-base-face-remapping'."
  (make-local-variable 'face-remapping-alist)
  (let ((entry (assq face face-remapping-alist)))
    (when (null entry)
      (setq entry (list face face))	; explicitly merge with global def
      (push entry face-remapping-alist))
    (setcdr entry (cons specs (cdr entry)))
    (cons face specs)))

(defun remove-relative-face-remapping (cookie)
  "Remove a face remapping previously added by `add-relative-face-remapping'.
COOKIE should be the return value from that function."
  (let ((remapping (assq (car cookie) face-remapping-alist)))
    (when remapping
      (let ((updated-entries (remq (cdr cookie) (cdr remapping))))
	(unless (eq updated-entries (cdr remapping))
	  (setcdr remapping updated-entries)
	  (when (or (null updated-entries)
		    (and (eq (car-safe updated-entries) (car cookie))
			 (null (cdr updated-entries))))
	    (setq face-remapping-alist
		  (remq remapping face-remapping-alist)))
	  (cdr cookie))))))

;;;### autoload
(defun set-default-base-face-remapping (face)
  "Set the base remapping of FACE to inherit from FACE's global definition."
  (let ((entry (assq face face-remapping-alist)))
    (when entry
      ;; If there's nothing except a base remapping, we simply remove
      ;; the entire remapping entry, as setting the base to the default
      ;; would be the same as the global definition.  Otherwise, we
      ;; modify the base remapping.
      (if (null (cddr entry))		; nothing except base remapping
	  (setq face-remapping-alist	; so remove entire entry
		(remq entry face-remapping-alist))
	(setcar (last entry) face)))))  ; otherwise, just inherit global def

;;;### autoload
(defun set-base-face-remapping (face &rest specs)
  "Set the base remapping of FACE in the current buffer to SPECS.
If SPECS is empty, the default base remapping is restored, which
inherits from the global definition of FACE; note that this is
different from SPECS containing a single value `nil', which does
not inherit from the global definition of FACE."
  (if (or (null specs)
	  (and (eq (car specs) face) (null (cdr specs)))) ; default
      ;; Set entry back to default
      (set-default-base-face-remapping face)
    ;; Set the base remapping
    (make-local-variable 'face-remapping-alist)
    (let ((entry (assq face face-remapping-alist)))
      (if entry
	  (setcar (last entry) specs)	; overwrite existing base entry
	(push (list face specs) face-remapping-alist)))))
  

;; ----------------------------------------------------------------
;; text-scale-mode

(defcustom text-scale-mode-step 1.2
  "Scale factor used by `text-scale-mode'.
Each positive or negative step scales the default face height by this amount."
  :group 'display
  :type 'number)

;; current remapping cookie for text-scale-mode
(defvar text-scale-mode-remapping nil)
(make-variable-buffer-local 'text-scale-mode-remapping)

;; Lighter displayed for text-scale-mode in mode-line minor-mode list
(defvar text-scale-mode-lighter "+0")
(make-variable-buffer-local 'text-scale-mode-lighter)

;; Number of steps that text-scale-mode will increase/decrease text height
(defvar text-scale-mode-amount 0)
(make-variable-buffer-local 'text-scale-mode-amount)

(define-minor-mode text-scale-mode
  "Minor mode for displaying buffer text in a larger/smaller font than usual.

The amount of scaling is determined by the variable
`text-scale-mode-amount':  one step scales the global default
face size by the value of the variable `text-scale-mode-step' (a
negative amount shrinks the text).

The `increase-buffer-face-height' and
`decrease-buffer-face-height' functions may be used to
interactively modify the variable `text-scale-mode-amount' (they
also enable or disable `text-scale-mode' as necessary."
  :lighter (" " text-scale-mode-lighter)
  (when text-scale-mode-remapping
    (remove-relative-face-remapping text-scale-mode-remapping))
  (setq text-scale-mode-lighter
	(format (if (>= text-scale-mode-amount 0) "+%d" "%d")
		text-scale-mode-amount))
  (setq text-scale-mode-remapping
	(and text-scale-mode
	     (add-relative-face-remapping 'default
					  :height
					  (expt text-scale-mode-step
						text-scale-mode-amount))))
  (force-window-update (current-buffer)))

;;;###autoload
(defun increase-buffer-face-height (&optional inc)
  "Increase the height of the default face in the current buffer by INC steps.
If the new height is other than the default, `text-scale-mode' is enabled.

Each step scales the height of the default face by the variable
`text-scale-mode-step' (a negative number of steps decreases the
height by the same amount).  As a special case, an argument of 0
will remove any scaling currently active."
  (interactive "p")
  (setq text-scale-mode-amount (if (= inc 0) 0 (+ text-scale-mode-amount inc)))
  (text-scale-mode (if (zerop text-scale-mode-amount) -1 1)))

;;;###autoload
(defun decrease-buffer-face-height (&optional dec)
  "Decrease the height of the default face in the current buffer by DEC steps.
See `increase-buffer-face-height' for more details."
  (interactive "p")
  (increase-buffer-face-height (- dec)))

;;;###autoload (define-key ctl-x-map [(control ?+)] 'adjust-buffer-face-height)
;;;###autoload (define-key ctl-x-map [(control ?-)] 'adjust-buffer-face-height)
;;;###autoload (define-key ctl-x-map [(control ?=)] 'adjust-buffer-face-height)
;;;###autoload (define-key ctl-x-map [(control ?0)] 'adjust-buffer-face-height)
;;;###autoload
(defun adjust-buffer-face-height (&optional inc)
  "Increase or decrease the height of the default face in the current buffer.

The actual adjustment made depends on the final component of the
key-binding used to invoke the command, with all modifiers
removed:

   +, =   Increase the default face height by one step
   -      Decrease the default face height by one step
   0      Reset the default face height to the global default

Then, continue to read input events and further adjust the face
height as long as the input event read (with all modifiers
removed) is one the above.

Each step scales the height of the default face by the variable
`text-scale-mode-step' (a negative number of steps decreases the
height by the same amount).  As a special case, an argument of 0
will remove any scaling currently active.

This command is a special-purpose wrapper around the
`increase-buffer-face-height' command which makes repetition
convenient even when it is bound in a non-top-level keymap.  For
binding in a top-level keymap, `increase-buffer-face-height' or
`decrease-default-face-height' may be more appropriate."
  (interactive "p")
  (let ((first t) 
	(step t)
	(ev last-command-event))
    (while step
      (let ((base (event-basic-type ev)))
	(cond ((or (eq base ?+) (eq base ?=))
	       (setq step inc))
	      ((eq base ?-)
	       (setq step (- inc)))
	      ((eq base ?0)
	       (setq step 0))
	      (first 
	       (setq step inc))
	      (t
	       (setq step nil))))
      (when step
	(increase-buffer-face-height step)
	(setq inc 1 first nil)
	(setq ev (read-event))))
    (push ev unread-command-events)))


;; ----------------------------------------------------------------
;; variable-pitch-mode

;; suggested key binding: (global-set-key "\C-cv" 'variable-pitch-mode)

;; current remapping cookie for  variable-pitch-mode
(defvar variable-pitch-mode-remapping nil)
(make-variable-buffer-local 'variable-pitch-mode-remapping)

(define-minor-mode variable-pitch-mode
  "Variable-pitch default-face mode.  When active, causes the
buffer text to be displayed using the `variable-pitch' face."
  :lighter " VarPitch"
  (when variable-pitch-mode-remapping
    (remove-relative-face-remapping variable-pitch-mode-remapping))
  (setq variable-pitch-mode-remapping
	(and variable-pitch-mode
	     (add-relative-face-remapping 'default 'variable-pitch)))
  (force-window-update (current-buffer)))


(provide 'face-remap)

;; arch-tag: 5c5f034b-8d58-4967-82bd-d61fd364e686
;;; face-remap.el ends here
