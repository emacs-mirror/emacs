;;; pulse.el --- Pulsing Overlays  -*- lexical-binding: t; -*-

;; Copyright (C) 2007-2022 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 1.0

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
;;
;; Manage temporary pulsing of faces and overlays.
;;
;; This is a temporal decoration technique where something is to be
;; highlighted briefly.  This adds a gentle pulsing style to the text
;; decorated this way.
;;
;; The following are useful entry points:
;;
;; `pulse-tick' - Cause `pulse-highlight-face' to shift toward background color.
;;      Assumes you are using a version of Emacs that supports pulsing.
;;
;; `pulse-momentary-highlight-one-line' - Pulse a single line at POINT.
;; `pulse-momentary-highlight-region' - Pulse a region.
;; `pulse-momentary-highlight-overlay' - Pulse an overlay.
;;      These three functions will just blink the specified area if
;;      the version of Emacs you are using doesn't support pulsing.
;;
;; `pulse-line-hook-function' - A simple function that can be used in a
;;      hook that will pulse whatever line the cursor is on.
;;
;;; History:
;;
;; The original pulse code was written for semantic tag highlighting.
;; It has been extracted, and adapted for general purpose pulsing.
;;
;; Pulse is a part of CEDET.  http://cedet.sf.net

(require 'color)

(defun pulse-available-p ()
  "Return non-nil if pulsing is available on the current frame."
  (condition-case nil
      (let ((v (color-values (face-background 'default))))
	(numberp (car-safe v)))
    (error nil)))

(defcustom pulse-flag (pulse-available-p)
  "Whether to use pulsing for momentary highlighting.
Pulsing involves a bright highlight that slowly shifts to the
background color.

If the value is nil, highlight with an unchanging color until a
key is pressed.
If the value is `never', do no coloring at all.
Any other value means to do the default pulsing behavior.

If `pulse-flag' is non-nil, but `pulse-available-p' is nil, then
this flag is ignored."
  :group 'pulse
  :type 'boolean)

(defface pulse-highlight-start-face
  '((((class color) (background dark))
     (:background "#AAAA33"))
    (((class color) (background light))
     (:background "#FFFFAA")))
  "Face used at beginning of a highlight."
  :group 'pulse)

(defface pulse-highlight-face
  '((((class color) (background dark))
     (:background "#AAAA33"))
    (((class color) (background light))
     (:background "#FFFFAA")))
  "Face used during a pulse for display.  *DO NOT CUSTOMIZE*
Face used for temporary highlighting of tags for effect."
  :group 'pulse)

;;; Code:

(defcustom pulse-iterations 10
  "Number of iterations in a pulse operation."
  :group 'pulse
  :type 'number)

(defcustom pulse-delay .03
  "Delay between face lightening iterations."
  :group 'pulse
  :type 'number)

;;; Convenience Functions
;;
(defvar pulse-momentary-overlay nil
  "The current pulsing overlay.")

(defvar pulse-momentary-timer nil
  "The current pulsing timer.")

(defvar pulse-momentary-iteration 0
  "The current pulsing iteration.")

(defun pulse-reset-face (&optional face)
  "Reset the pulse highlighting FACE."
  (set-face-background 'pulse-highlight-face
		       (if face
			   (face-background face nil t)
			 (face-background 'pulse-highlight-start-face)
			 ))
  (set-face-extend 'pulse-highlight-face
                   (face-extend-p (or face 'pulse-highlight-start-face)
                                  nil t))
  (put 'pulse-highlight-face :startface (or face
					    'pulse-highlight-start-face))
  (setq pulse-momentary-iteration 0))

(defun pulse-momentary-highlight-overlay (o &optional face)
  "Pulse the overlay O, unhighlighting before next command.
Optional argument FACE specifies the face to do the highlighting."
  ;; We don't support simultaneous highlightings.
  (pulse-momentary-unhighlight)
  (overlay-put o 'original-face (overlay-get o 'face))
  ;; Make this overlay take priority over the `transient-mark-mode'
  ;; overlay.
  (overlay-put o 'original-priority (overlay-get o 'priority))
  (overlay-put o 'priority 1)
  (setq pulse-momentary-overlay o)
  (if (eq pulse-flag 'never)
      nil
    (if (or (not pulse-flag) (not (pulse-available-p)))
	;; Provide a face... clear on next command
	(progn
	  (overlay-put o 'face (or face 'pulse-highlight-start-face))
	  (add-hook 'pre-command-hook
		    #'pulse-momentary-unhighlight))
      ;; Pulse it.
      (overlay-put o 'face 'pulse-highlight-face)
      ;; The pulse function puts FACE onto 'pulse-highlight-face.
      ;; Thus above we put our face on the overlay, but pulse
      ;; with a reference face needed for the color.
      (pulse-reset-face face)
      (let* ((start (color-name-to-rgb
                     (face-background 'pulse-highlight-face nil 'default)))
             (stop (color-name-to-rgb (face-background 'default)))
             (colors (mapcar (apply-partially 'apply 'color-rgb-to-hex)
                             (color-gradient start stop pulse-iterations))))
        (setq pulse-momentary-timer
              (run-with-timer 0 pulse-delay #'pulse-tick
                              colors
                              (time-add nil
                                        (* pulse-delay pulse-iterations))))))))

(defun pulse-tick (colors stop-time)
  (if (time-less-p nil stop-time)
      (when-let (color (elt colors pulse-momentary-iteration))
        (set-face-background 'pulse-highlight-face color)
        (setq pulse-momentary-iteration (1+ pulse-momentary-iteration)))
    (pulse-momentary-unhighlight)))

(defun pulse-momentary-unhighlight ()
  "Unhighlight a line recently highlighted."
  (when pulse-momentary-overlay
    ;; clear the starting face
    (let ((ol pulse-momentary-overlay))
      (overlay-put ol 'face (overlay-get ol 'original-face))
      (overlay-put ol 'original-face nil)
      (overlay-put ol 'priority (overlay-get ol 'original-priority))
      ;; Clear the overlay if it needs deleting.
      (when (overlay-get ol 'pulse-delete) (delete-overlay ol)))

    ;; Clear the variable.
    (setq pulse-momentary-overlay nil)

    ;; Reset the pulsing face.
    (pulse-reset-face))

  ;; Cancel the timer.
  (when pulse-momentary-timer
    (cancel-timer pulse-momentary-timer))

  ;; Remove this hook.
  (remove-hook 'pre-command-hook #'pulse-momentary-unhighlight))

;;;###autoload
(defun pulse-momentary-highlight-one-line (&optional point face)
  "Highlight the line around POINT, unhighlighting before next command.
If POINT is nil or missing, the current point is used instead.

Optional argument FACE specifies the face to do the highlighting."
  (save-excursion
    (goto-char (or point (point)))
    (let ((start (point-at-bol))
          (end (save-excursion
                 (end-of-line)
                 (when (not (eobp))
                   (forward-char 1))
                 (point))))
      (pulse-momentary-highlight-region start end face))))

;;;###autoload
(defun pulse-momentary-highlight-region (start end &optional face)
  "Highlight between START and END, unhighlighting before next command.
Optional argument FACE specifies the face to do the highlighting."
  (let ((o (make-overlay start end)))
    ;; Mark it for deletion
    (overlay-put o 'pulse-delete t)
    (pulse-momentary-highlight-overlay o face)))

;;; Random integration with other tools

(defvar pulse-command-advice-flag nil)

(defun pulse-line-hook-function ()
  "Function used in hooks to pulse the current line.
Only pulses the line if `pulse-command-advice-flag' is non-nil."
  (when pulse-command-advice-flag
    (pulse-momentary-highlight-one-line (point))))

(provide 'pulse)

;;; pulse.el ends here
