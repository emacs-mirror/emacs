;;; face-remap.el --- Functions for managing `face-remapping-alist'  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2008-2023 Free Software Foundation, Inc.
;;
;; Author: Miles Bader <miles@gnu.org>
;; Keywords: faces, face remapping, display, user commands
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;

;;; Commentary:

;; This file defines some simple operations that can be used for
;; maintaining the `face-remapping-alist' in a cooperative way.  This is
;; especially important for the `default' face.
;;
;; Each face-remapping definition in `face-remapping-alist' added by
;; this code uses the form:
;;
;;   (face RELATIVE_SPECS_1 RELATIVE_SPECS_2 ... BASE_SPECS)
;;
;; The "specs" values are lists of face names or face attribute-value
;; pairs, and are merged together, with earlier values taking precedence.
;;
;; The RELATIVE_SPECS_* values are added by `face-remap-add-relative'
;; (and removed by `face-remap-remove-relative', and are intended for
;; face "modifications" (such as increasing the size).  Typical users of
;; relative specs would be minor modes.
;;
;; BASE_SPECS is the lowest-priority value, and by default is just the
;; face name, which causes the global definition of that face to be used.
;;
;; A non-default value of BASE_SPECS may also be set using
;; `face-remap-set-base'.  Because this _overwrites_ the default
;; value inheriting from the global face definition, it is up to the
;; caller of face-remap-set-base to add such inheritance if it is
;; desired.  A typical use of face-remap-set-base would be a major
;; mode setting face remappings, e.g., of the default face.
;;
;; All modifications cause face-remapping-alist to be made buffer-local.

;;; Code:


;; ----------------------------------------------------------------
;; Utility functions

;; Names of face attributes corresponding to lisp face-vector positions.
;; This variable should probably be defined in C code where the actual
;; definitions are available.
;; :vector must be always at the end as a guard
;;
(defvar internal-lisp-face-attributes
  [nil
   :family :foundry :width :height :weight :slant :underline
   :inverse-video
   :foreground :background :stipple :overline :strike-through :box
   :font :inherit :fontset :distant-foreground :extend :vector])

(defun face-remap--copy-face (val)
  "Return a copy of the `face' property value VAL."
  ;; A `face' property can be either a face name (a symbol), or a face
  ;; property list like (:foreground "red" :inherit default),
  ;; or a list of such things.
  ;; FIXME: This should probably be shared to some extent with
  ;; `add-face-text-property'.
  (if (or (not (listp val)) (keywordp (car val)))
      val
    (copy-sequence val)))

(defun face-attrs--make-indirect-safe ()
  "Deep-copy the buffer's `face-remapping-alist' upon cloning the buffer."
  (setq-local face-remapping-alist
              (mapcar #'face-remap--copy-face face-remapping-alist)))

(add-hook 'clone-indirect-buffer-hook #'face-attrs--make-indirect-safe)

(defun face-attrs-more-relative-p (attrs1 attrs2)
  "Return non-nil if ATTRS1 is \"more relative\" than ATTRS2.
We define this as meaning that ATTRS1 contains a greater number of
relative face-attributes than ATTRS2.  A face attribute is considered
relative if `face-attribute-relative-p' returns non-nil.

ATTRS1 and ATTRS2 may be any value suitable for a `face' text
property, including face names, lists of face names,
face-attribute plists, etc.

This function can be used as a predicate with `sort', to sort
face lists so that more specific faces are located near the end."
  (unless (vectorp attrs1)
    (setq attrs1 (face-attributes-as-vector attrs1)))
  (unless (vectorp attrs2)
    (setq attrs2 (face-attributes-as-vector attrs2)))
  (let ((rel1-count 0) (rel2-count 0))
    (dotimes (i (length attrs1))
      (let ((attr (aref internal-lisp-face-attributes i)))
	(when attr
	  (when (face-attribute-relative-p attr (aref attrs1 i))
	    (setq rel1-count (+ rel1-count 1)))
	  (when (face-attribute-relative-p attr (aref attrs2 i))
	    (setq rel2-count (+ rel2-count 1))))))
    (< rel1-count rel2-count)))

(defun face-remap-order (entry)
  "Order ENTRY so that more relative face specs are near the beginning.
The list structure of ENTRY may be destructively modified."
  (setq entry (nreverse entry))
  (setcdr entry (sort (cdr entry) #'face-attrs-more-relative-p))
  (nreverse entry))

;;;###autoload
(defun face-remap-add-relative (face &rest specs)
  "Add a face remapping entry of FACE to SPECS in the current buffer.
Return a cookie which can be used to delete this remapping with
`face-remap-remove-relative'.

The remaining arguments, SPECS, should form a list of faces.
Each list element should be either a face name or a property list
of face attribute/value pairs.  If more than one face is listed,
that specifies an aggregate face, in the same way as in a `face'
text property, except for possible priority changes noted below.

The face remapping specified by SPECS takes effect alongside the
remappings from other calls to `face-remap-add-relative' for the
same FACE, as well as the normal definition of FACE (at lowest
priority).  This function tries to sort multiple remappings for
the same face, so that remappings specifying relative face
attributes are applied after remappings specifying absolute face
attributes.

The base (lowest priority) remapping may be set to something
other than the normal definition of FACE via `face-remap-set-base'."
  (while (and (consp specs) (null (cdr specs)))
    (setq specs (car specs)))
  (make-local-variable 'face-remapping-alist)
  (let ((entry (assq face face-remapping-alist)))
    (when (null entry)
      (setq entry (list face face))	; explicitly merge with global def
      (push entry face-remapping-alist))
    (let ((faces (cdr entry)))
      (if (symbolp faces)
	  (setq faces (list faces)))
      (setcdr entry (face-remap-order (cons specs faces)))
      ;; Force redisplay of this buffer.
      (force-mode-line-update))
    (cons face specs)))

(defun face-remap-remove-relative (cookie)
  "Remove a face remapping previously added by `face-remap-add-relative'.
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
		  (remq remapping face-remapping-alist))
	    ;; Force redisplay of this buffer.
	    (force-mode-line-update))
	  (cdr cookie))))))

;;;###autoload
(defun face-remap-reset-base (face)
  "Set the base remapping of FACE to the normal definition of FACE.
This causes the remappings specified by `face-remap-add-relative'
to apply on top of the normal definition of FACE."
  (let ((entry (assq face face-remapping-alist)))
    (when entry
      ;; If there's nothing except a base remapping, we simply remove
      ;; the entire remapping entry, as setting the base to the default
      ;; would be the same as the global definition.  Otherwise, we
      ;; modify the base remapping.
      (if (null (cddr entry))		; nothing except base remapping
	  (setq face-remapping-alist	; so remove entire entry
		(remq entry face-remapping-alist))
	(setcar (last entry) face))
      ;; Force redisplay of this buffer.
      (force-mode-line-update))))  ; otherwise, just inherit global def

;;;###autoload
(defun face-remap-set-base (face &rest specs)
  "Set the base remapping of FACE in the current buffer to SPECS.
This causes the remappings specified by `face-remap-add-relative'
to apply on top of the face specification given by SPECS.

The remaining arguments, SPECS, specify the base of the remapping.
Each one of SPECS should be either a face name or a property list
of face attribute/value pairs, like in a `face' text property.

If SPECS is empty or a single face `eq' to FACE, call `face-remap-reset-base'
to use the normal definition of FACE as the base remapping; note that
this is different from SPECS containing a single value nil, which means
not to inherit from the global definition of FACE at all."
  ;; Simplify the specs in the case where it's just a single face (and
  ;; it's not a list with just a nil).
  (while (and (consp specs) (not (null (car specs))) (null (cdr specs)))
    (setq specs (car specs)))
  (if (or (null specs)
	  (eq specs face)) ; default
      ;; Set entry back to default
      (face-remap-reset-base face)
    ;; Set the base remapping
    (make-local-variable 'face-remapping-alist)
    (let ((entry (assq face face-remapping-alist)))
      (if entry
	  (setcar (last entry) specs)	; overwrite existing base entry
	(push (list face specs) face-remapping-alist)))
    ;; Force redisplay of this buffer.
    (force-mode-line-update)))


;; ----------------------------------------------------------------
;; text-scale-mode

(defcustom text-scale-mode-step 1.2
  "Scale factor used by `text-scale-mode'.
Each positive or negative step scales the size of the `default'
face's font by this amount."
  :group 'display
  :type 'number
  :version "23.1")

(defvar-local text-scale-mode-remapping nil
  "Current remapping cookie for `text-scale-mode'.")

(defvar-local text-scale-mode-lighter "+0"
  "Lighter displayed for `text-scale-mode' in mode-line minor-mode list.")

(defvar-local text-scale-mode-amount 0
  "Number of steps that `text-scale-mode' will increase/decrease text height.")

(defvar-local text-scale-remap-header-line nil
  "If non-nil, text scaling may change font size of header lines too.")

(defun face-remap--clear-remappings ()
  (dolist (remapping
           ;; This is a bit messy to stay backwards compatible.
           ;; In the future, this can be simplified to just use
           ;; `text-scale-mode-remapping'.
           (if (consp (car-safe text-scale-mode-remapping))
               text-scale-mode-remapping
             (list text-scale-mode-remapping)))
    (face-remap-remove-relative remapping))
  (setq text-scale-mode-remapping nil))

(defun face-remap--remap-face (sym)
  (push (face-remap-add-relative sym
                       :height
                       (expt text-scale-mode-step
                             text-scale-mode-amount))
        text-scale-mode-remapping))

(define-minor-mode text-scale-mode
  "Minor mode for displaying buffer text in a larger/smaller font.

The amount of scaling is determined by the variable
`text-scale-mode-amount': one step scales the global default
face size by the value of the variable `text-scale-mode-step'
\(a negative amount shrinks the text).

The `text-scale-increase', `text-scale-decrease', and
`text-scale-set' functions may be used to interactively modify
the variable `text-scale-mode-amount' (they also enable or
disable `text-scale-mode' as necessary).

If `text-scale-remap-header-line' is non-nil, also change
the font size of the header line."
  :lighter (" " text-scale-mode-lighter)
  (face-remap--clear-remappings)
  (setq text-scale-mode-lighter
	(format (if (>= text-scale-mode-amount 0) "+%d" "%d")
		text-scale-mode-amount))
  (when text-scale-mode
    (face-remap--remap-face 'default)
    (when text-scale-remap-header-line
      (face-remap--remap-face 'header-line)))
  (force-window-update (current-buffer)))

(defun text-scale--refresh (symbol newval operation where)
  "Watcher for `text-scale-remap-header-line'.
See `add-variable-watcher'."
  (when (and (eq symbol 'text-scale-remap-header-line)
             (eq operation 'set)
             text-scale-mode)
    (with-current-buffer where
      (let ((text-scale-remap-header-line newval))
        (text-scale-mode 1)))))
(add-variable-watcher 'text-scale-remap-header-line #'text-scale--refresh)

(defun text-scale-min-amount ()
  "Return the minimum amount of text-scaling we allow."
  ;; When the resulting pixel-height of characters will become smaller
  ;; than 1 pixel, we can expect trouble from the display engine.
  ;; E.g., it requires that the character glyph's ascent is
  ;; non-negative.
  (log (/ 1.0 (frame-char-height)) text-scale-mode-step))

(defun text-scale-max-amount ()
  "Return the maximum amount of text-scaling we allow."
  ;; The display engine uses a 16-bit short for pixel-width of
  ;; characters, thus the 0xffff limitation.  It also makes no sense
  ;; to have characters wider than the display.
  (log (/ (min (display-pixel-width) #xffff)
          (frame-char-width))
       text-scale-mode-step))

;;;###autoload
(defun text-scale-set (level)
  "Set the scale factor of the default face in the current buffer to LEVEL.
If LEVEL is non-zero, `text-scale-mode' is enabled, otherwise it is disabled.

LEVEL is a number of steps, with 0 representing the default size.
Each step scales the height of the default face by the variable
`text-scale-mode-step' (a negative number decreases the height by
the same amount)."
  (interactive "p")
  (setq text-scale-mode-amount
        (max (min level (text-scale-max-amount)) (text-scale-min-amount)))
  (text-scale-mode (if (zerop text-scale-mode-amount) -1 1)))

;;;###autoload
(defun text-scale-increase (inc)
  "Increase the font size of the default face in current buffer by INC steps.
If the new height is other than the default, `text-scale-mode' is enabled.

Each step scales the height of the default face by the variable
`text-scale-mode-step' (a negative number of steps decreases the
height by the same amount).  As a special case, an argument of 0
will remove any scaling currently active."
  (interactive "p")
  (let* ((current-value (if text-scale-mode text-scale-mode-amount 0))
         (new-value (if (= inc 0) 0 (+ current-value inc))))
    (if (or (> new-value (text-scale-max-amount))
            (< new-value (text-scale-min-amount)))
        (user-error "Cannot %s the font size any further"
                    (if (> inc 0) "increase" "decrease")))
    (setq text-scale-mode-amount new-value))
  (text-scale-mode (if (zerop text-scale-mode-amount) -1 1)))

;;;###autoload
(defun text-scale-decrease (dec)
  "Decrease the font size of the default face in the current buffer by DEC steps.
See `text-scale-increase' for more details."
  (interactive "p")
  (text-scale-increase (- dec)))

;;;###autoload (define-key ctl-x-map [(control ?+)] 'text-scale-adjust)
;;;###autoload (define-key ctl-x-map [(control ?-)] 'text-scale-adjust)
;;;###autoload (define-key ctl-x-map [(control ?=)] 'text-scale-adjust)
;;;###autoload (define-key ctl-x-map [(control ?0)] 'text-scale-adjust)
;;;###autoload
(defun text-scale-adjust (inc)
  "Adjust the font size in the current buffer by INC steps.
Interactively, INC is the prefix numeric argument, and defaults to 1.

The actual adjustment made depends on the final component of the
keybinding used to invoke the command, with all modifiers removed:

   \\`+', \\`='   Increase font size in current buffer by one step
   \\`-'      Decrease font size in current buffer by one step
   \\`0'      Reset the font size to the global default

After adjusting, continue to read input events and further adjust
the font size as long as the input event (with all modifiers removed)
is one of the above characters.

Each step scales the height of the default face by the factor that
is the value of `text-scale-mode-step' (a negative number of steps
decreases the height by that factor).  As a special case, an argument
of 0 will remove any scaling currently active, thus resetting the
font size to the original value.

This command is a special-purpose wrapper around the
`text-scale-increase' command which makes repetition convenient
even when it is bound in a non-top-level keymap.  For binding in
a top-level keymap, `text-scale-increase' or
`text-scale-decrease' may be more appropriate.

Most faces are affected by these font size changes, but not faces
that have an explicit `:height' setting.  The two exceptions to
this are the `default' and `header-line' faces: they will both be
scaled even if they have an explicit `:height' setting.

See also the related command `global-text-scale-adjust'.  Unlike
that command, which scales the font size with a increment,
`text-scale-adjust' scales the font size with a factor,
`text-scale-mode-step'.  With a small `text-scale-mode-step'
factor, the two commands behave similarly."
  (interactive "p")
  (let ((ev last-command-event)
	(echo-keystrokes nil))
    (let* ((base (event-basic-type ev))
           (step
            (pcase base
              ((or ?+ ?=) inc)
              (?- (- inc))
              (?0 0)
              (_ inc))))
      (text-scale-increase step)
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (dolist (mods '(() (control)))
           (dolist (key '(?+ ?= ?- ?0)) ;; = is often unshifted +.
             (define-key map (vector (append mods (list key)))
               (lambda () (interactive) (text-scale-adjust (abs inc))))))
         map)
       nil nil
       "Use %k for further adjustment"))))

(defvar-local text-scale--pinch-start-scale 0
  "The text scale at the start of a pinch sequence.")

;;;###autoload (define-key global-map [pinch] 'text-scale-pinch)
;;;###autoload
(defun text-scale-pinch (event)
  "Adjust the height of the default face by the scale in the pinch event EVENT."
  (interactive "e")
  (when (not (eq (event-basic-type event) 'pinch))
    (error "`text-scale-pinch' bound to bad event type"))
  (let ((window (posn-window (nth 1 event)))
        (scale (nth 4 event))
        (dx (nth 2 event))
        (dy (nth 3 event))
        (angle (nth 5 event)))
    (with-selected-window window
      (when (and (zerop dx)
                 (zerop dy)
                 (zerop angle))
        (setq text-scale--pinch-start-scale
              (if text-scale-mode text-scale-mode-amount 0)))
      (text-scale-set
       (+ text-scale--pinch-start-scale
          (round (log scale text-scale-mode-step)))))))

(defcustom global-text-scale-adjust-resizes-frames nil
  "Whether `global-text-scale-adjust' resizes the frames."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t))
  :group 'display
  :version "29.1")

(defcustom global-text-scale-adjust-limits '(10 . 500)
  "Min/max values for `global-text-scale-adjust'.
This is a cons cell where the `car' has the minimum font size and
the `cdr' has the maximum font size, in units of 1/10 pt."
  :version "29.1"
  :group 'display
  :type '(cons (integer :tag "Min")
               (integer :tag "Max")))

(defvar global-text-scale-adjust--default-height nil)

(defvar global-text-scale-adjust--increment-factor 5)

;;;###autoload (define-key ctl-x-map [(control meta ?+)] 'global-text-scale-adjust)
;;;###autoload (define-key ctl-x-map [(control meta ?=)] 'global-text-scale-adjust)
;;;###autoload (define-key ctl-x-map [(control meta ?-)] 'global-text-scale-adjust)
;;;###autoload (define-key ctl-x-map [(control meta ?0)] 'global-text-scale-adjust)
;;;###autoload
(defun global-text-scale-adjust (increment)
  "Change (a.k.a. \"adjust\") the font size of all faces by INCREMENT.

Interactively, INCREMENT is the prefix numeric argument, and defaults
to 1.  Positive values of INCREMENT increase the font size, negative
values decrease it.

When you invoke this command, it performs the initial change of the
font size, and after that allows further changes by typing one of the
following keys immediately after invoking the command:

   \\`+', \\`='   Globally increase the height of the default face
   \\`-'      Globally decrease the height of the default face
   \\`0'      Globally reset the height of the default face

(The change of the font size produced by these keys depends on the
final component of the key sequence, with all modifiers removed.)

Buffer-local face adjustments have higher priority than global
face adjustments.

The variable `global-text-scale-adjust-resizes-frames' controls
whether the frames are resized to keep the same number of lines
and characters per line when the font size is adjusted.

See also the related command `text-scale-adjust'.  Unlike that
command, which scales the font size with a factor,
`global-text-scale-adjust' scales the font size with an
increment."
  (interactive "p")
  (when (display-graphic-p)
    (unless global-text-scale-adjust--default-height
      (setq global-text-scale-adjust--default-height
            (face-attribute 'default :height)))
    (let* ((key (event-basic-type last-command-event))
           (echo-keystrokes nil)
           (cur (face-attribute 'default :height))
           (inc
            (pcase key
              (?- (* (- increment)
                     global-text-scale-adjust--increment-factor))
              (?0 (- global-text-scale-adjust--default-height cur))
              (_ (* increment
                    global-text-scale-adjust--increment-factor))))
           (new (+ cur inc)))
      (when (< (car global-text-scale-adjust-limits)
               new
               (cdr global-text-scale-adjust-limits))
        (let ((frame-inhibit-implied-resize
               (not global-text-scale-adjust-resizes-frames)))
          (set-face-attribute 'default nil :height new)
          (redisplay 'force)
          (when (and (not (and (characterp key) (= key ?0)))
                     (= cur (face-attribute 'default :height)))
            (setq global-text-scale-adjust--increment-factor
                  (1+ global-text-scale-adjust--increment-factor))
            (global-text-scale-adjust increment))))
      (when (characterp key)
        (set-transient-map
         (let ((map (make-sparse-keymap)))
           (dolist (mod '(() (control meta)))
             (dolist (key '(?+ ?= ?- ?0))
               (define-key map (vector (append mod (list key)))
                 'global-text-scale-adjust)))
           map)
       nil nil
       "Use %k for further adjustment")))))


;; ----------------------------------------------------------------
;; buffer-face-mode

(defcustom buffer-face-mode-face 'variable-pitch
  "The face specification used by `buffer-face-mode'.
It may contain any value suitable for a `face' text property,
including a face name, a list of face names, a face attribute
plist, etc."
  :type '(choice (face)
		 (repeat :tag "List of faces" face)
		 (plist :tag "Face property list"))
  :group 'display
  :version "23.1")

;; current remapping cookie for  buffer-face-mode
(defvar-local buffer-face-mode-remapping nil)

;;;###autoload
(define-minor-mode buffer-face-mode
  "Minor mode for a buffer-specific default face.

When enabled, the face specified by the variable
`buffer-face-mode-face' is used to display the buffer text."
  :lighter " BufFace"
  (when buffer-face-mode-remapping
    (face-remap-remove-relative buffer-face-mode-remapping))
  (setq buffer-face-mode-remapping
	(and buffer-face-mode
	     (face-remap-add-relative 'default buffer-face-mode-face)))
  (force-window-update (current-buffer)))

;;;###autoload
(defun buffer-face-set (&rest specs)
  "Enable `buffer-face-mode', using face specs SPECS.
Each argument in SPECS should be a face, i.e. either a face name
or a property list of face attributes and values.  If more than
one face is listed, that specifies an aggregate face, like in a
`face' text property.  If SPECS is nil or omitted, disable
`buffer-face-mode'.

This function makes the variable `buffer-face-mode-face' buffer
local, and sets it to FACE."
  (interactive (list (read-face-name "Set buffer face" (face-at-point t))))
  (while (and (consp specs) (null (cdr specs)))
    (setq specs (car specs)))
  (if (null specs)
      (buffer-face-mode 0)
    (setq-local buffer-face-mode-face specs)
    (buffer-face-mode t)))

;;;###autoload
(defun buffer-face-toggle (&rest specs)
  "Toggle `buffer-face-mode', using face specs SPECS.
Each argument in SPECS should be a face, i.e. either a face name
or a property list of face attributes and values.  If more than
one face is listed, that specifies an aggregate face, like in a
`face' text property.

If `buffer-face-mode' is already enabled, and is currently using
the face specs SPECS, then it is disabled; if `buffer-face-mode'
is disabled, or is enabled and currently displaying some other
face, then is left enabled, but the face changed to reflect SPECS.

This function will make the variable `buffer-face-mode-face'
buffer local, and set it to SPECS."
  (interactive (list buffer-face-mode-face))
  (while (and (consp specs) (null (cdr specs)))
    (setq specs (car specs)))
  (if (or (null specs)
	  (and buffer-face-mode (equal buffer-face-mode-face specs)))
      (buffer-face-mode 0)
    (setq-local buffer-face-mode-face specs)
    (buffer-face-mode t)))

(defun buffer-face-mode-invoke (specs arg &optional interactive)
  "Enable or disable `buffer-face-mode' using face specs SPECS.
ARG controls whether the mode is enabled or disabled, and is
interpreted in the usual manner for minor-mode commands.

SPECS can be any value suitable for a `face' text property,
including a face name, a plist of face attributes and values,
or a list of faces.

If INTERACTIVE is non-nil, display a message describing the
result.

This is a wrapper function which calls `buffer-face-set' or
`buffer-face-toggle' (depending on ARG), and prints a status
message in the echo area.  In many cases one of those functions
may be more appropriate."
  (let ((last-message (current-message)))
    (if (or (eq arg 'toggle) (not arg))
	(buffer-face-toggle specs)
      (buffer-face-set (and (> (prefix-numeric-value arg) 0) specs)))
    (when interactive
      (unless (and (current-message)
		   (not (equal last-message (current-message))))
	(message "Buffer-Face mode %sabled"
		 (if buffer-face-mode "en" "dis"))))))


;; ----------------------------------------------------------------
;; variable-pitch-mode

;;;###autoload
(defun variable-pitch-mode (&optional arg)
  "Variable-pitch default-face mode.
An interface to `buffer-face-mode' which uses the `variable-pitch' face.
Besides the choice of face, it is the same as `buffer-face-mode'."
  (interactive (list (or current-prefix-arg 'toggle)))
  (buffer-face-mode-invoke 'variable-pitch (or arg t)
			   (called-interactively-p 'interactive)))


(provide 'face-remap)

;;; face-remap.el ends here
