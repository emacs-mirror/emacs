;;; faces.el --- Lisp faces

;; Copyright (C) 1992, 1993, 1994, 1995, 1996, 1998, 1999, 2000
;;   Free Software Foundation, Inc.

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

(eval-when-compile
  (require 'cl))

(require 'cus-face)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Font selection.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup font-selection nil
  "Influencing face font selection."
  :group 'faces)


(defcustom face-font-selection-order
  '(:width :height :weight :slant)
  "*A list specifying how face font selection chooses fonts.
Each of the four symbols `:width', `:height', `:weight', and `:slant'
must appear once in the list, and the list must not contain any other
elements.  Font selection tries to find a best matching font for
those face attributes first that appear first in the list.  For
example, if `:slant' appears before `:height', font selection first
tries to find a font with a suitable slant, even if this results in
a font height that isn't optimal."
  :tag "Font selection order."
  :group 'font-selection
  :set #'(lambda (symbol value)
	   (set-default symbol value)
	   (internal-set-font-selection-order value)))


(defcustom face-font-family-alternatives
  '(("courier" "fixed")
    ("helv" "helvetica" "fixed"))
  "*Alist of alternative font family names.
Each element has the the form (FAMILY ALTERNATIVE1 ALTERNATIVE2 ...).
If fonts of family FAMILY can't be loaded, try ALTERNATIVE1, then
ALTERNATIVE2 etc."
  :tag "Alternative font families to try."
  :group 'font-selection
  :set #'(lambda (symbol value)
	   (set-default symbol value)
	   (internal-set-alternative-font-family-alist value)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creation, copying.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun face-list ()
  "Return a list of all defined face names."
  (mapcar #'car face-new-frame-defaults))


;;; ### If not frame-local initialize by what X resources?

(defun make-face (face &optional no-init-from-resources)
  "Define a new face with name FACE, a symbol.
NO-INIT-FROM-RESOURCES non-nil means don't initialize frame-local
variants of FACE from X resources.  (X resources recognized are found
in the global variable `face-x-resources'.)  If FACE is already known
as a face, leave it unmodified.  Value is FACE."
  (interactive "SMake face: ")
  (unless (facep face)
    ;; Make frame-local faces (this also makes the global one).
    (dolist (frame (frame-list))
      (internal-make-lisp-face face frame))
    ;; Add the face to the face menu.
    (when (fboundp 'facemenu-add-new-face)
      (facemenu-add-new-face face))
    ;; Define frame-local faces for all frames from X resources.
    (unless no-init-from-resources
      (make-face-x-resource-internal face)))
  face)


(defun make-empty-face (face)
  "Define a new, empty face with name FACE.
If the face already exists, it is left unmodified.  Value is FACE."
  (interactive "SMake empty face: ")
  (make-face face 'no-init-from-resources))


(defun copy-face (old-face new-face &optional frame new-frame)
  "Define a face just like OLD-FACE, with name NEW-FACE.

If NEW-FACE already exists as a face, it is modified to be like
OLD-FACE.  If it doesn't already exist, it is created.

If the optional argument FRAME is given as a frame,  NEW-FACE is
changed on FRAME only.
If FRAME is t, the frame-independent default specification for OLD-FACE
is copied to NEW-FACE.
If FRAME is nil, copying is done for the frame-independent defaults
and for each existing frame.

If the optional fourth argument NEW-FRAME is given,
copy the information from face OLD-FACE on frame FRAME
to NEW-FACE on frame NEW-FRAME."
  (let ((inhibit-quit t))
    (if (null frame)
	(progn
	  (dolist (frame (frame-list))
	    (copy-face old-face new-face frame))
	  (copy-face old-face new-face t))
      (internal-copy-lisp-face old-face new-face frame new-frame))
    new-face))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Obsolete functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The functions in this section are defined because Lisp packages use
;; them, despite the prefix `internal-' suggesting that they are
;; private to the face implementation.  

(defun internal-find-face (name &optional frame)
  "Retrieve the face named NAME.
Return nil if there is no such face.
If the optional argument FRAME is given, this gets the face NAME for
that frame; otherwise, it uses the selected frame.
If FRAME is the symbol t, then the global, non-frame face is returned.
If NAME is already a face, it is simply returned.

This function is defined for compatibility with Emacs 20.2.  It
should not be used anymore."
  (facep name))


(defun internal-get-face (name &optional frame)
  "Retrieve the face named NAME; error if there is none.
If the optional argument FRAME is given, this gets the face NAME for
that frame; otherwise, it uses the selected frame.
If FRAME is the symbol t, then the global, non-frame face is returned.
If NAME is already a face, it is simply returned.

This function is defined for compatibility with Emacs 20.2.  It
should not be used anymore."
  (or (internal-find-face name frame)
      (check-face name)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Predicates, type checks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun facep (face)
  "Return non-nil if FACE is a face name."
  (internal-lisp-face-p face))


(defun check-face (face)
  "Signal an error if FACE doesn't name a face.
Value is FACE."
  (unless (facep face)
    (error "Not a face: %s" face))
  face)


;; The ID returned is not to be confused with the internally used IDs
;; of realized faces.  The ID assigned to Lisp faces is used to
;; support faces in display table entries.

(defun face-id (face &optional frame)
  "Return the interNal ID of face with name FACE.
If optional argument FRAME is nil or omitted, use the selected frame."
  (check-face face)
  (get face 'face))


(defun face-equal (face1 face2 &optional frame)
  "Non-nil if faces FACE1 and FACE2 are equal.
Faces are considered equal if all their attributes are equal.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
  (internal-lisp-face-equal-p face1 face2 frame))


(defun face-differs-from-default-p (face &optional frame)
  "Non-nil if FACE displays differently from the default face.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame.
A face is considered to be ``the same'' as the default face if it is
actually specified in the same way (equal attributes) or if it is
fully-unspecified, and thus inherits the attributes of any face it
is displayed on top of."
  (cond ((eq frame t) (setq frame nil))
	((null frame) (setq frame (selected-frame))))
  (let* ((v1 (internal-lisp-face-p face frame))
	 (n (if v1 (length v1) 0))
	 (v2 (internal-lisp-face-p 'default frame))
	 (i 1))
    (unless v1
      (error "Not a face: %S" face))
    (while (and (< i n)
		(or (eq 'unspecified (aref v1 i))
		    (equal (aref v1 i) (aref v2 i))))
      (setq i (1+ i)))
    (< i n)))


(defun face-nontrivial-p (face &optional frame)
  "True if face FACE has some non-nil attribute.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
  (not (internal-lisp-face-empty-p face frame)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setting face attributes from X resources.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom face-x-resources
  '((:family (".attributeFamily" . "Face.AttributeFamily"))
    (:width (".attributeWidth" . "Face.AttributeWidth"))
    (:height (".attributeHeight" . "Face.AttributeHeight"))
    (:weight (".attributeWeight" . "Face.AttributeWeight"))
    (:slant (".attributeSlant" . "Face.AttributeSlant"))
    (:foreground (".attributeForeground" . "Face.AttributeForeground"))
    (:background (".attributeBackground" . "Face.AttributeBackground"))
    (:overline (".attributeOverline" . "Face.AttributeOverline"))
    (:strike-through (".attributeStrikeThrough" . "Face.AttributeStrikeThrough"))
    (:box (".attributeBox" . "Face.AttributeBox"))
    (:underline (".attributeUnderline" . "Face.AttributeUnderline"))
    (:inverse-video (".attributeInverse" . "Face.AttributeInverse"))
    (:stipple
     (".attributeStipple" . "Face.AttributeStipple")
     (".attributeBackgroundPixmap" . "Face.AttributeBackgroundPixmap"))
    (:font (".attributeFont" . "Face.AttributeFont"))
    (:bold (".attributeBold" . "Face.AttributeBold"))
    (:italic (".attributeItalic" . "Face.AttributeItalic"))
    (:font (".attributeFont" . "Face.AttributeFont")))
  "*List of X resources and classes for face attributes.
Each element has the form (ATTRIBUTE ENTRY1 ENTRY2...) where ATTRIBUTE is
the name of a face attribute, and each ENTRY is a cons of the form
(RESOURCE . CLASS) with RESOURCE being the resource and CLASS being the
X resource class for the attribute."
  :type 'sexp
  :group 'faces)


(defun set-face-attribute-from-resource (face attribute resource class frame)
  "Set FACE's ATTRIBUTE from X resource RESOURCE, class CLASS on FRAME.
Value is the attribute value specified by the resource, or nil
if not present.  This function displays a message if the resource
specifies an invalid attribute."
  (let* ((face-name (face-name face))
	 (value (internal-face-x-get-resource (concat face-name resource)
					      class frame)))
    (when value
      (condition-case ()
	  (internal-set-lisp-face-attribute-from-resource
	   face attribute (downcase value) frame)
	(error
	 (message "Face %s, frame %s: invalid attribute %s %s from X resource"
		  face-name frame attribute value))))
    value))


(defun set-face-attributes-from-resources (face frame)
  "Set attributes of FACE from X resources for FRAME."
  (when (memq (framep frame) '(x w32))
    (dolist (definition face-x-resources)
      (let ((attribute (car definition)))
	(dolist (entry (cdr definition))
	  (set-face-attribute-from-resource face attribute (car entry)
					    (cdr entry) frame))))))
    
  
(defun make-face-x-resource-internal (face &optional frame)
  "Fill frame-local FACE on FRAME from X resources.
FRAME nil or not specified means do it for all frames."
  (if (null frame)
      (dolist (frame (frame-list))
	(set-face-attributes-from-resources face frame))
    (set-face-attributes-from-resources face frame)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Retrieving face attributes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun face-name (face)
  "Return the name of face FACE."
  (symbol-name (check-face face)))


(defun face-attribute (face attribute &optional frame)
  "Return the value of FACE's ATTRIBUTE on FRAME.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
  (internal-get-lisp-face-attribute face attribute frame))


(defun face-foreground (face &optional frame)
  "Return the foreground color name of FACE, or nil if unspecified.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
  (internal-get-lisp-face-attribute face :foreground frame))


(defun face-background (face &optional frame)
  "Return the background color name of FACE, or nil if unspecified.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
  (internal-get-lisp-face-attribute face :background frame))


(defun face-stipple (face &optional frame)
 "Return the stipple pixmap name of FACE, or nil if unspecified.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
  (internal-get-lisp-face-attribute face :stipple frame))


(defalias 'face-background-pixmap 'face-stipple)


(defun face-underline-p (face &optional frame)
 "Return non-nil if FACE is underlined.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
 (eq (face-attribute face :underline frame) t))


(defun face-inverse-video-p (face &optional frame)
 "Return non-nil if FACE is in inverse video on FRAME.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
 (eq (face-attribute face :inverse-video frame) t))


(defun face-bold-p (face &optional frame)
  "Return non-nil if the font of FACE is bold on FRAME.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame.
Use `face-attribute' for finer control."
  (let ((bold (face-attribute face :weight frame)))
    (memq bold '(semi-bold bold extra-bold ultra-bold))))


(defun face-italic-p (face &optional frame)
  "Return non-nil if the font of FACE is italic on FRAME.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame.
Use `face-attribute' for finer control."
  (let ((italic (face-attribute face :slant frame)))
    (memq italic '(italic oblique))))
    




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Face documentation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun face-documentation (face)
  "Get the documentation string for FACE."
  (get face 'face-documentation))


(defun set-face-documentation (face string)
  "Set the documentation string for FACE to STRING."
  ;; Perhaps the text should go in DOC.
  (put face 'face-documentation (purecopy string)))


(defalias 'face-doc-string 'face-documentation)
(defalias 'set-face-doc-string 'set-face-documentation)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting face attributes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun set-face-attribute (face frame &rest args)
  "Set attributes of FACE on FRAME from ARGS.

FRAME nil means change attributes on all frames.  FRAME t means change
the default for new frames (this is done automatically each time an
attribute is changed on all frames).

ARGS must come in pairs ATTRIBUTE VALUE.  ATTRIBUTE must be a valid
face attribute name.  All attributes can be set to `unspecified';
this fact is not further mentioned below.

The following attributes are recognized:

`:family'

VALUE must be a string specifying the font family, e.g. ``courier'',
or a fontset alias name.  If a font family is specified, wild-cards `*'
and `?' are allowed.

`:width'

VALUE specifies the relative proportionate width of the font to use.
It must be one of the symbols `ultra-condensed', `extra-condensed',
`condensed', `semi-condensed', `normal', `semi-expanded', `expanded',
`extra-expanded', or `ultra-expanded'.

`:height'

VALUE must be an integer specifying the height of the font to use in
1/10 pt.

`:weight'

VALUE specifies the weight of the font to use.  It must be one of the
symbols `ultra-bold', `extra-bold', `bold', `semi-bold', `normal',
`semi-light', `light', `extra-light', `ultra-light'.

`:slant'

VALUE specifies the slant of the font to use.  It must be one of the
symbols `italic', `oblique', `normal', `reverse-italic', or
`reverse-oblique'.

`:foreground', `:background'

VALUE must be a color name, a string.

`:underline'

VALUE specifies whether characters in FACE should be underlined.  If
VALUE is t, underline with foreground color of the face.  If VALUE is
a string, underline with that color.  If VALUE is nil, explicitly
don't underline.

`:overline'

VALUE specifies whether characters in FACE should be overlined.  If
VALUE is t, overline with foreground color of the face.  If VALUE is a
string, overline with that color.  If VALUE is nil, explicitly don't
overline.

`:strike-through'

VALUE specifies whether characters in FACE should be drawn with a line
striking through them.  If VALUE is t, use the foreground color of the
face.  If VALUE is a string, strike-through with that color.  If VALUE
is nil, explicitly don't strike through.

`:box'

VALUE specifies whether characters in FACE should have a box drawn
around them.  If VALUE is nil, explicitly don't draw boxes.  If
VALUE is t, draw a box with lines of width 1 in the foreground color
of the face.  If VALUE is a string, the string must be a color name,
and the box is drawn in that color with a line width of 1.  Otherwise,
VALUE must be a property list of the form `(:line-width WIDTH
:color COLOR :style STYLE)'.  If a keyword/value pair is missing from
the property list, a default value will be used for the value, as
specified below.  WIDTH specifies the width of the lines to draw; it
defaults to 1.  COLOR is the name of the color to draw in, default is
the foreground color of the face for simple boxes, and the background
color of the face for 3D boxes.  STYLE specifies whether a 3D box
should be draw.  If STYLE is `released-button', draw a box looking
like a released 3D button.  If STYLE is `pressed-button' draw a box
that appears like a pressed button.  If STYLE is nil, the default if
the property list doesn't contain a style specification, draw a 2D
box.

`:inverse-video'

VALUE specifies whether characters in FACE should be displayed in
inverse video. VALUE must be one of t or nil.

`:stipple'

If VALUE is a string, it must be the name of a file of pixmap data.
The directories listed in the `x-bitmap-file-path' variable are
searched.  Alternatively, VALUE may be a list of the form (WIDTH
HEIGHT DATA) where WIDTH and HEIGHT are the size in pixels, and DATA
is a string containing the raw bits of the bitmap.  VALUE nil means
explicitly don't use a stipple pattern.

For convenience, attributes `:family', `:width', `:height', `:weight',
and `:slant' may also be set in one step from an X font name:

`:font'

Set font-related face attributes from VALUE.  VALUE must be a valid
XLFD font name.  If it is a font name pattern, the first matching font
will be used.

For compatibility with Emacs 20, keywords `:bold' and `:italic' can
be used to specify that a bold or italic font should be used.  VALUE
must be t or nil in that case.  A value of `unspecified' is not allowed."
  (setq args (purecopy args))
  (cond ((null frame)
	 ;; Change face on all frames.
	 (dolist (frame (frame-list))
	   (apply #'set-face-attribute face frame args))
	 ;; Record that as a default for new frames.
	 (apply #'set-face-attribute face t args))
	(t
	 (while args
	   (internal-set-lisp-face-attribute face (car args)
					     (purecopy (cadr args))
					     frame)
	   (setq args (cdr (cdr args)))))))


(defun make-face-bold (face &optional frame noerror)
  "Make the font of FACE be bold, if possible.
FRAME nil or not specified means change face on all frames.
Argument NOERROR is ignored and retained for compatibility.
Use `set-face-attribute' for finer control of the font weight."
  (interactive (list (read-face-name "Make which face bold ")))
  (set-face-attribute face frame :weight 'bold))


(defun make-face-unbold (face &optional frame noerror)
  "Make the font of FACE be non-bold, if possible.
FRAME nil or not specified means change face on all frames.
Argument NOERROR is ignored and retained for compatibility."
  (interactive (list (read-face-name "Make which face non-bold ")))
  (set-face-attribute face frame :weight 'normal))

  
(defun make-face-italic (face &optional frame noerror)
  "Make the font of FACE be italic, if possible.
FRAME nil or not specified means change face on all frames.
Argument NOERROR is ignored and retained for compatibility.
Use `set-face-attribute' for finer control of the font slant."
  (interactive (list (read-face-name "Make which face italic ")))
  (set-face-attribute face frame :slant 'italic))


(defun make-face-unitalic (face &optional frame noerror)
  "Make the font of FACE be non-italic, if possible.
FRAME nil or not specified means change face on all frames."
  (interactive (list (read-face-name "Make which face non-italic ")))
  (set-face-attribute face frame :slant 'normal))

  
(defun make-face-bold-italic (face &optional frame noerror)
  "Make the font of FACE be bold and italic, if possible.
FRAME nil or not specified means change face on all frames.
Argument NOERROR is ignored and retained for compatibility.
Use `set-face-attribute' for finer control of font weight and slant."
  (interactive (list (read-face-name "Make which face bold-italic: ")))
  (set-face-attribute face frame :weight 'bold :slant 'italic))


(defun set-face-font (face font &optional frame)
  "Change font-related attributes of FACE to those of FONT (a string).
FRAME nil or not specified means change face on all frames.
This sets the attributes `:family', `:width', `:height', `:weight',
and `:slant'.  When called interactively, prompt for the face and font."
  (interactive (read-face-and-attribute :font))
  (set-face-attribute face frame :font font))


;; Implementation note: Emulating gray background colors with a
;; stipple pattern is now part of the face realization process, and is
;; done in C depending on the frame on which the face is realized.

(defun set-face-background (face color &optional frame)
  "Change the background color of face FACE to COLOR (a string).
FRAME nil or not specified means change face on all frames.
When called interactively, prompt for the face and color."
  (interactive (read-face-and-attribute :background))
  (set-face-attribute face frame :background color))


(defun set-face-foreground (face color &optional frame)
  "Change the foreground color of face FACE to COLOR (a string).
FRAME nil or not specified means change face on all frames.
When called interactively, prompt for the face and color."
  (interactive (read-face-and-attribute :foreground))
  (set-face-attribute face frame :foreground color))


(defun set-face-stipple (face stipple &optional frame)
  "Change the stipple pixmap of face FACE to STIPPLE.
FRAME nil or not specified means change face on all frames.
STIPPLE. should be a string, the name of a file of pixmap data.
The directories listed in the `x-bitmap-file-path' variable are searched.

Alternatively, STIPPLE may be a list of the form (WIDTH HEIGHT DATA)
where WIDTH and HEIGHT are the size in pixels,
and DATA is a string, containing the raw bits of the bitmap."
  (interactive (read-face-and-attribute :stipple))
  (set-face-attribute face frame :stipple stipple))


(defun set-face-underline (face underline &optional frame)
  "Specify whether face FACE is underlined.
UNDERLINE nil means FACE explicitly doesn't underline.
UNDERLINE non-nil means FACE explicitly does underlining
with the same of the foreground color.
If UNDERLINE is a string, underline with the color named UNDERLINE.
FRAME nil or not specified means change face on all frames.
Use `set-face-attribute' to ``unspecify'' underlining."
  (interactive
   (let ((list (read-face-and-attribute :underline)))
     (list (car list) (eq (car (cdr list)) t))))
  (set-face-attribute face frame :underline underline))


(defun set-face-underline-p (face underline-p &optional frame)
  "Specify whether face FACE is underlined.
UNDERLINE-P nil means FACE explicitly doesn't underline.
UNDERLINE-P non-nil means FACE explicitly does underlining.
FRAME nil or not specified means change face on all frames.
Use `set-face-attribute' to ``unspecify'' underlining."
  (interactive
   (let ((list (read-face-and-attribute :underline)))
     (list (car list) (eq (car (cdr list)) t))))
  (set-face-attribute face frame :underline underline-p))


(defun set-face-inverse-video-p (face inverse-video-p &optional frame)
  "Specify whether face FACE is in inverse video.
INVERSE-VIDEO-P non-nil means FACE displays explicitly in inverse video.
INVERSE-VIDEO-P nil means FACE explicitly is not in inverse video.
FRAME nil or not specified means change face on all frames.
Use `set-face-attribute' to ``unspecify'' the inverse video attribute."
  (interactive
   (let ((list (read-face-and-attribute :inverse-video)))
     (list (car list) (eq (car (cdr list)) t))))
  (set-face-attribute face frame :inverse-video inverse-video-p))


(defun set-face-bold-p (face bold-p &optional frame)
  "Specify whether face FACE is bold.
BOLD-P non-nil means FACE should explicitly display bold.
BOLD-P nil means FACE should explicitly display non-bold.
FRAME nil or not specified means change face on all frames.
Use `set-face-attribute' or `modify-face' for finer control."
  (if (null bold-p)
      (make-face-unbold face frame)
    (make-face-bold face frame)))


(defun set-face-italic-p (face italic-p &optional frame)
  "Specify whether face FACE is italic.
ITALIC-P non-nil means FACE should explicitly display italic.
ITALIC-P nil means FACE should explicitly display non-italic.
FRAME nil or not specified means change face on all frames.
Use `set-face-attribute' or `modify-face' for finer control."
  (if (null italic-p)
      (make-face-unitalic face frame)
    (make-face-italic face frame)))


(defalias 'set-face-background-pixmap 'set-face-stipple)


(defun invert-face (face &optional frame)
  "Swap the foreground and background colors of FACE.
FRAME nil or not specified means change face on all frames.
If FACE specifies neither foreground nor background color,
set its foreground and background to the background and foreground
of the default face.  Value is FACE."
  (interactive (list (read-face-name "Invert face ")))
  (let ((fg (face-attribute face :foreground frame))
	(bg (face-attribute face :background frame)))
    (if (or fg bg)
	(set-face-attribute face frame :foreground bg :background fg)
      (set-face-attribute face frame
			  :foreground
			  (face-attribute 'default :background frame)
			  :background
			  (face-attribute 'default :foreground frame))))
  face)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactively modifying faces.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-face-name (prompt)
  "Read and return a face symbol, prompting with PROMPT.
Value is a symbol naming a known face."
  (let ((face-list (mapcar #'(lambda (x) (cons (symbol-name x) x))
			   (face-list)))
	(def (thing-at-point 'symbol))
	face)
    (cond ((assoc def face-list)
	   (setq prompt (concat prompt "(default " def "): ")))
	  (t (setq def nil)
	     (setq prompt (concat prompt ": "))))
    (while (equal "" (setq face (completing-read
				 prompt face-list nil t nil nil def))))
    (intern face)))


(defun face-valid-attribute-values (attribute &optional frame)
  "Return valid values for face attribute ATTRIBUTE.
The optional argument FRAME is used to determine available fonts
and colors.  If it is nil or not specified, the selected frame is
used.  Value is an alist of (NAME . VALUE) if ATTRIBUTE expects a value
out of a set of discrete values.  Value is `integerp' if ATTRIBUTE expects
an integer value."
  (let (valid)
    (setq valid
	  (case attribute
	    (:family
	     (if window-system
		 (mapcar #'(lambda (x) (cons (car x) (car x)))
			 (x-font-family-list))
	       ;; Only one font on TTYs.
	       (list (cons "default" "default"))))
	    ((:width :weight :slant :inverse-video)
	     (mapcar #'(lambda (x) (cons (symbol-name x) x))
		     (internal-lisp-face-attribute-values attribute)))
	    ((:underline :overline :strike-through :box)
	     (if window-system
		 (nconc (mapcar #'(lambda (x) (cons (symbol-name x) x))
				(internal-lisp-face-attribute-values attribute))
			(mapcar #'(lambda (c) (cons c c))
				(x-defined-colors frame)))
	       (mapcar #'(lambda (x) (cons (symbol-name x) x))
		       (internal-lisp-face-attribute-values attribute))))
	    ((:foreground :background)
	     (mapcar #'(lambda (c) (cons c c))
		     (defined-colors frame)))
	    ((:height)
	     'integerp)
	    (:stipple
	     (and (memq window-system '(x w32))
		  (mapcar #'list
			  (apply #'nconc (mapcar #'directory-files
						 x-bitmap-file-path)))))
	    (t
	     (error "Internal error"))))
    (if (listp valid)
	(nconc (list (cons "unspecified" 'unspecified)) valid)
      valid)))
	       


(defvar face-attribute-name-alist
  '((:family . "font family")
    (:width . "character set width")
    (:height . "height in 1/10 pt")
    (:weight . "weight")
    (:slant . "slant")
    (:underline . "underline")
    (:overline . "overline")
    (:strike-through . "strike-through")
    (:box . "box")
    (:inverse-video . "inverse-video display")
    (:foreground . "foreground color")
    (:background . "background color")
    (:stipple . "background stipple"))
  "An alist of descriptive names for face attributes.
Each element has the form (ATTRIBUTE-NAME . DESCRIPTION) where
ATTRIBUTE-NAME is a face attribute name (a keyword symbol), and
DESCRIPTION is a descriptive name for ATTRIBUTE-NAME.")


(defun face-descriptive-attribute-name (attribute)
  "Return a descriptive name for ATTRIBUTE."
  (cdr (assq attribute face-attribute-name-alist)))


(defun face-read-string (face default name &optional completion-alist)
  "Interactively read a face attribute string value.
FACE is the face whose attribute is read.  DEFAULT is the default
value to return if no new value is entered.  NAME is a descriptive
name of the attribute for prompting.  COMPLETION-ALIST is an alist
of valid values, if non-nil.

Entering nothing accepts the default value DEFAULT.
Value is the new attribute value."
  (let* ((completion-ignore-case t)
	 (value (completing-read
		 (if default
		     (format "Set face %s %s (default %s): "
			     face name (downcase (if (symbolp default)
						     (symbol-name default)
						   default)))
		   (format "Set face %s %s: " face name))
		 completion-alist)))
    (if (equal value "") default value)))


(defun face-read-integer (face default name)
  "Interactively read an integer face attribute value.
FACE is the face whose attribute is read.  DEFAULT is the default
value to return if no new value is entered.  NAME is a descriptive
name of the attribute for prompting.  Value is the new attribute value."
  (let ((new-value
	 (face-read-string face
			   (if (memq default
				     '(unspecified
				       "unspecified-fg"
				       "unspecified-bg"))
			       default
			     (int-to-string default))
			   name
			   (list (cons "unspecified" 'unspecified)))))
    (if (memq new-value '(unspecified "unspecified-fg" "unspecified-bg"))
	new-value
      (string-to-int new-value))))


(defun read-face-attribute (face attribute &optional frame)
  "Interactively read a new value for FACE's ATTRIBUTE.
Optional argument FRAME nil or unspecified means read an attribute value
of a global face.  Value is the new attribute value."
  (let* ((old-value (face-attribute face attribute frame))
	 (attribute-name (face-descriptive-attribute-name attribute))
	 (valid (face-valid-attribute-values attribute frame))
	 new-value)
    ;; Represent complex attribute values as strings by printing them
    ;; out.  Stipple can be a vector; (WIDTH HEIGHT DATA).  Box can be
    ;; a list `(:width WIDTH :color COLOR)' or `(:width WIDTH :shadow
    ;; SHADOW)'.
    (when (and (or (eq attribute :stipple)
		   (eq attribute :box))
	       (or (consp old-value)
		   (vectorp old-value)))
      (setq old-value (prin1-to-string old-value)))
    (cond ((listp valid)
	   (setq new-value
		 (face-read-string face old-value attribute-name valid))
	   ;; Terminal frames can support colors that don't appear
	   ;; explicitly in VALID, using color approximation code
	   ;; in tty-colors.el.
	   (if (and (memq attribute '(:foreground :background))
		    (not (memq window-system '(x w32 mac)))
		    (not (memq new-value
			       '(unspecified
				 "unspecified-fg"
				 "unspecified-bg"))))
	       (setq new-value (car (tty-color-desc new-value frame))))
	   (unless (eq new-value 'unspecified)
	     (setq new-value (cdr (assoc new-value valid)))))
	  ((eq valid 'integerp)
	   (setq new-value (face-read-integer face old-value attribute-name)))
	  (t (error "Internal error")))
    ;; Convert stipple and box value text we read back to a list or
    ;; vector if it looks like one.  This makes the assumption that a
    ;; pixmap file name won't start with an open-paren.
    (when (and (or (eq attribute :stipple)
		   (eq attribute :box))
	       (stringp new-value)
	       (string-match "^[[(]" new-value))
      (setq new-value (read new-value)))
    new-value))


(defun read-face-font (face &optional frame)
  "Read the name of a font for FACE on FRAME.
If optional argument FRAME Is nil or omitted, use the selected frame."
  (let ((completion-ignore-case t))
    (completing-read "Set font attributes of face %s from font: "
		     face (x-list-fonts "*" nil frame))))


(defun read-all-face-attributes (face &optional frame)
  "Interactively read all attributes for FACE.
If optional argument FRAME Is nil or omitted, use the selected frame.
Value is a property list of attribute names and new values."
  (let (result)
    (dolist (attribute face-attribute-name-alist result)
      (setq result (cons (car attribute)
			 (cons (read-face-attribute face (car attribute) frame)
			       result))))))

    
(defun modify-face (&optional frame)
  "Modify attributes of faces interactively.
If optional argument FRAME is nil or omitted, modify the face used
for newly created frame, i.e. the global face."
  (interactive)
  (let ((face (read-face-name "Modify face ")))
    (apply #'set-face-attribute face frame
	   (read-all-face-attributes face frame))))


(defun read-face-and-attribute (attribute &optional frame)
  "Read face name and face attribute value.
ATTRIBUTE is the attribute whose new value is read.
FRAME nil or unspecified means read attribute value of global face.
Value is a list (FACE NEW-VALUE) where FACE is the face read
(a symbol), and NEW-VALUE is value read."
  (cond ((eq attribute :font)
	 (let* ((prompt (format "Set font-related attributes of face "))
		(face (read-face-name prompt))
		(font (read-face-font face frame)))
	   (list face font)))
	(t
	 (let* ((attribute-name (face-descriptive-attribute-name attribute))
		(prompt (format "Set %s of face " attribute-name))
		(face (read-face-name prompt))
		(new-value (read-face-attribute face attribute frame)))
	   (list face new-value)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Listing faces.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar list-faces-sample-text
  "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "*Text string to display as the sample text for `list-faces-display'.")


;; The name list-faces would be more consistent, but let's avoid a
;; conflict with Lucid, which uses that name differently.

(defun list-faces-display ()
  "List all faces, using the same sample text in each.
The sample text is a string that comes from the variable
`list-faces-sample-text'."
  (interactive)
  (let ((faces (sort (face-list) #'string-lessp))
	(face nil)
	(frame (selected-frame))
	disp-frame window face-name)
    (with-output-to-temp-buffer "*Faces*"
      (save-excursion
	(set-buffer standard-output)
	(setq truncate-lines t)
	(insert
	 (substitute-command-keys
	  (concat
	   "Use "
	   (if (display-mouse-p) "\\[help-follow-mouse] or ")
	   "\\[help-follow] or on a face name to customize it\n"
	   "or on its sample text for a decription of the face.\n\n")))
	(setq help-xref-stack nil)
	(while faces
	  (setq face (car faces))
	  (setq faces (cdr faces))
	  (setq face-name (symbol-name face))
	  (insert (format "%25s " face-name))
	  ;; Hyperlink to a customization buffer for the face.  Using
	  ;; the help xref mechanism may not be the best way.
	  (save-excursion
	    (save-match-data
	      (search-backward face-name)
	      (help-xref-button 0 #'customize-face face-name)))
	  (let ((beg (point)))
	    (insert list-faces-sample-text)
	    ;; Hyperlink to a help buffer for the face.
	    (save-excursion
	      (save-match-data
		(search-backward list-faces-sample-text)
		(help-xref-button 0 #'describe-face face)))
	    (insert "\n")
	    (put-text-property beg (1- (point)) 'face face)
	    ;; If the sample text has multiple lines, line up all of them.
	    (goto-char beg)
	    (forward-line 1)
	    (while (not (eobp))
	      (insert "                          ")
	      (forward-line 1))))
	(goto-char (point-min)))
      (print-help-return-message))
    ;; If the *Faces* buffer appears in a different frame,
    ;; copy all the face definitions from FRAME,
    ;; so that the display will reflect the frame that was selected.
    (setq window (get-buffer-window (get-buffer "*Faces*") t))
    (setq disp-frame (if window (window-frame window)
		       (car (frame-list))))
    (or (eq frame disp-frame)
	(let ((faces (face-list)))
	  (while faces
	    (copy-face (car faces) (car faces) frame disp-frame)
	    (setq faces (cdr faces)))))))


(defun describe-face (face &optional frame)
  "Display the properties of face FACE on FRAME.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
  (interactive (list (read-face-name "Describe face ")))
  (let* ((attrs '((:family . "Family")
		  (:width . "Width")
		  (:height . "Height")
		  (:weight . "Weight")
		  (:slant . "Slant")
		  (:foreground . "Foreground")
		  (:background . "Background")
		  (:underline . "Underline")
		  (:overline . "Overline")
		  (:strike-through . "Strike-through")
		  (:box . "Box")
		  (:inverse-video . "Inverse")
		  (:stipple . "Stipple")))
	(max-width (apply #'max (mapcar #'(lambda (x) (length (cdr x)))
					attrs))))
    (with-output-to-temp-buffer "*Help*"
      (save-excursion
	(set-buffer standard-output)
	(dolist (a attrs)
	  (let ((attr (face-attribute face (car a) frame)))
	    (insert (make-string (- max-width (length (cdr a))) ?\ )
		    (cdr a) ": " (format "%s" attr) "\n")))
	(insert "\nDocumentation:\n\n"
		(or (face-documentation face)
		    "not documented as a face.")))
      (print-help-return-message))))
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Face specifications (defface).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parameter FRAME Is kept for call compatibility to with previous
;; face implementation.

(defun face-attr-construct (face &optional frame)
  "Return a defface-style attribute list for FACE on FRAME.
Value is a property list of pairs ATTRIBUTE VALUE for all specified
face attributes of FACE where ATTRIBUTE is the attribute name and
VALUE is the specified value of that attribute."
  (let (result)
    (dolist (entry face-attribute-name-alist result)
      (let* ((attribute (car entry))
	     (value (face-attribute face attribute)))
	(unless (eq value 'unspecified)
	  (setq result (nconc (list attribute value) result)))))))
    

(defun face-spec-set-match-display (display frame)
  "Non-nil if DISPLAY matches FRAME.
DISPLAY is part of a spec such as can be used in `defface'.
If FRAME is nil, the current FRAME is used."
  (let* ((conjuncts display)
	 conjunct req options
	 ;; t means we have succeeded against all the conjuncts in
	 ;; DISPLAY that have been tested so far.
	 (match t))
    (if (eq conjuncts t)
	(setq conjuncts nil))
    (while (and conjuncts match)
      (setq conjunct (car conjuncts)
	    conjuncts (cdr conjuncts)
	    req (car conjunct)
	    options (cdr conjunct)
	    match (cond ((eq req 'type)
			 (or (memq window-system options)
			     (and (null window-system)
				  (memq 'tty options))
			     (and (memq 'motif options)
				  (featurep 'motif))
			     (and (memq 'lucid options)
				  (featurep 'x-toolkit)
				  (not (featurep 'motif)))
			     (and (memq 'x-toolkit options)
				  (featurep 'x-toolkit))))
			((eq req 'class)
			 (memq (frame-parameter frame 'display-type) options))
			((eq req 'background)
			 (memq (frame-parameter frame 'background-mode)
			       options))
			(t (error "Unknown req `%S' with options `%S'" 
				  req options)))))
    match))


(defun face-spec-choose (spec &optional frame)
  "Choose the proper attributes for FRAME, out of SPEC."
  (unless frame
    (setq frame (selected-frame)))
  (let ((tail spec)
	result)
    (while tail
      (let* ((entry (car tail))
	     (display (nth 0 entry))
	     (attrs (nth 1 entry)))
	(setq tail (cdr tail))
	(when (face-spec-set-match-display display frame)
	  (setq result attrs tail nil))))
    result))


(defun face-spec-reset-face (face &optional frame)
  "Reset all attributes of FACE on FRAME to unspecified."
  (let ((attrs face-attribute-name-alist)
	params)
    (while attrs
      (let ((attr-and-name (car attrs)))
	(setq params (cons (car attr-and-name) (cons 'unspecified params))))
      (setq attrs (cdr attrs)))
    (apply #'set-face-attribute face frame params)))


(defun face-spec-set (face spec &optional frame)
  "Set FACE's attributes according to the first matching entry in SPEC.
FRAME is the frame whose frame-local face is set.  FRAME nil means
do it on all frames.  See `defface' for information about SPEC."
  (let ((attrs (face-spec-choose spec frame))
	params)
    (while attrs
      (let ((attribute (car attrs))
	    (value (car (cdr attrs))))
	;; Support some old-style attribute names and values.
	(case attribute
	  (:bold (setq attribute :weight value (if value 'bold 'normal)))
	  (:italic (setq attribute :slant value (if value 'italic 'normal))))
	(setq params (cons attribute (cons value params))))
      (setq attrs (cdr (cdr attrs))))
    (face-spec-reset-face face frame)
    (apply #'set-face-attribute face frame params)))


(defun face-attr-match-p (face attrs &optional frame)
  "Value is non-nil if attributes of FACE match values in plist ATTRS.
Optional parameter FRAME is the frame whose definition of FACE
is used.  If nil or omitted, use the selected frame."
  (unless frame
    (setq frame (selected-frame)))
  (let ((list face-attribute-name-alist)
	(match t))
    (while (and match (not (null list)))
      (let* ((attr (car (car list)))
	     (specified-value (plist-get attrs attr))
	     (value-now (face-attribute face attr frame)))
	(when specified-value
	  (setq match (equal specified-value value-now)))
	(setq list (cdr list))))
    match))


(defun face-spec-match-p (face spec &optional frame)
  "Return t if FACE, on FRAME, matches what SPEC says it should look like."
  (face-attr-match-p face (face-spec-choose spec frame) frame))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Frame-type independent color support.
;;; We keep the old x-* names as aliases for back-compatibility.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun defined-colors (&optional frame)
  "Return a list of colors supported for a particular frame.
The argument FRAME specifies which frame to try.
The value may be different for frames on different display types.
If FRAME doesn't support colors, the value is nil."
  (if (memq (framep (or frame (selected-frame))) '(x w32))
      (xw-defined-colors frame)
    (mapcar 'car (tty-color-alist frame))))
(defalias 'x-defined-colors 'defined-colors)

(defun color-defined-p (color &optional frame)
  "Return non-nil if color COLOR is supported on frame FRAME.
If FRAME is omitted or nil, use the selected frame.
If COLOR is the symbol `unspecified' or one of the strings
\"unspecified-fg\" or \"unspecified-bg\", the value is nil."
  (if (memq color '(unspecified "unspecified-bg" "unspecified-fg"))
      nil
    (if (memq (framep (or frame (selected-frame))) '(x w32))
	(xw-color-defined-p color frame)
      (numberp (tty-color-translate color frame)))))
(defalias 'x-color-defined-p 'color-defined-p)

(defun color-values (color &optional frame)
  "Return a description of the color named COLOR on frame FRAME.
The value is a list of integer RGB values--\(RED GREEN BLUE\).
These values appear to range from 0 to 65280 or 65535, depending
on the system; white is \(65280 65280 65280\) or \(65535 65535 65535\).
If FRAME is omitted or nil, use the selected frame.
If FRAME cannot display COLOR, the value is nil.
If COLOR is the symbol `unspecified' or one of the strings
\"unspecified-fg\" or \"unspecified-bg\", the value is nil."
  (if (memq color '(unspecified "unspecified-fg" "unspecified-bg"))
      nil
    (if (memq (framep (or frame (selected-frame))) '(x w32))
	(xw-color-values color frame)
      (tty-color-values color frame))))
(defalias 'x-color-values 'color-values)

(defun display-color-p (&optional display)
  "Return t if DISPLAY supports color.
The optional argument DISPLAY specifies which display to ask about.
DISPLAY should be either a frame or a display name (a string).
If omitted or nil, that stands for the selected frame's display."
  (if (memq (framep-on-display display) '(x w32))
      (xw-display-color-p display)
    (tty-display-color-p display)))
(defalias 'x-display-color-p 'display-color-p)

(defun display-grayscale-p (&optional display)
  "Return non-nil if frames on DISPLAY can display shades of gray."
  (let ((frame-type (framep-on-display display)))
    (cond
     ((memq frame-type '(x w32 mac))
      (x-display-grayscale-p display))
     (t
      (> (tty-color-gray-shades display) 2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Background mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom frame-background-mode nil
  "*The brightness of the background.
Set this to the symbol `dark' if your background color is dark, `light' if
your background is light, or nil (default) if you want Emacs to
examine the brightness for you."
  :group 'faces
  :set #'(lambda (var value)
	   (set var value)
	   (mapcar 'frame-set-background-mode (frame-list)))
  :initialize 'custom-initialize-changed
  :type '(choice (choice-item dark) 
		 (choice-item light)
		 (choice-item :tag "default" nil)))


(defun frame-set-background-mode (frame)
  "Set up the `background-mode' and `display-type' frame parameters for FRAME."
  (let* ((bg-resource
	  (and window-system
	       (x-get-resource ".backgroundMode" "BackgroundMode")))
	 (params (frame-parameters frame))
	 (bg-mode (cond (frame-background-mode)
			((null window-system)
			 ;; No way to determine this automatically (?).
			 'dark)
			(bg-resource
			 (intern (downcase bg-resource)))
			((< (apply '+ (x-color-values
				       (cdr (assq 'background-color
						  params))
				       frame))
			    ;; Just looking at the screen, colors whose
			    ;; values add up to .6 of the white total
			    ;; still look dark to me.
			    (* (apply '+ (x-color-values "white" frame)) .6))
			 'dark)
			(t 'light)))
	 (display-type (cond ((null window-system)
			      (if (tty-display-color-p frame) 'color 'mono))
			     ((x-display-color-p frame)
			      'color)
			     ((x-display-grayscale-p frame)
			      'grayscale)
			     (t 'mono))))
    (modify-frame-parameters frame
			     (list (cons 'background-mode bg-mode)
				   (cons 'display-type display-type))))
  
  ;; For all named faces, choose face specs matching the new frame
  ;; parameters.
  (let ((face-list (face-list)))
    (while face-list
      (let* ((face (car face-list))
	     (spec (get face 'face-defface-spec)))
	(when spec
	  (face-spec-set face spec frame))
      (setq face-list (cdr face-list))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Frame creation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun x-handle-named-frame-geometry (parameters)
  "Add geometry parameters for a named frame to parameter list PARAMETERS.
Value is the new parameter list."
  (let* ((name (or (cdr (assq 'name parameters))
		   (cdr (assq 'name default-frame-alist))))
	 (x-resource-name name)
	 (res-geometry (if name (x-get-resource "geometry" "Geometry"))))
    (when res-geometry
      (let ((parsed (x-parse-geometry res-geometry)))
	;; If the resource specifies a position, call the position
	;; and size "user-specified".
	(when (or (assq 'top parsed)
		  (assq 'left parsed))
	  (setq parsed (append '((user-position . t) (user-size . t)) parsed)))
	;; Put the geometry parameters at the end.  Copy
	;; default-frame-alist so that they go after it.
	(setq parameters (append parameters default-frame-alist parsed))))
    parameters))


(defun x-handle-reverse-video (frame parameters)
  "Handle the reverse-video frame parameter and X resource.
`x-create-frame' does not handle this one."
  (when (cdr (or (assq 'reverse parameters)
		 (assq 'reverse default-frame-alist)
		 (let ((resource (x-get-resource "reverseVideo"
						 "ReverseVideo")))
		   (if resource
		       (cons nil (member (downcase resource)
					 '("on" "true")))))))
      (let* ((params (frame-parameters frame))
	     (bg (cdr (assq 'foreground-color params)))
	     (fg (cdr (assq 'background-color params))))
	(modify-frame-parameters frame
				 (list (cons 'foreground-color fg)
				       (cons 'background-color bg)))
	(if (equal bg (cdr (assq 'border-color params)))
	    (modify-frame-parameters frame
				     (list (cons 'border-color fg))))
	(if (equal bg (cdr (assq 'mouse-color params)))
	    (modify-frame-parameters frame
				     (list (cons 'mouse-color fg))))
	(if (equal bg (cdr (assq 'cursor-color params)))
	    (modify-frame-parameters frame
				     (list (cons 'cursor-color fg)))))))


(defun x-create-frame-with-faces (&optional parameters)
  "Create a frame from optional frame parameters PARAMETERS.
Parameters not specified by PARAMETERS are taken from
`default-frame-alist'.  If PARAMETERS specify a frame name,
handle X geometry resources for that name.  If either PARAMETERS
or `default-frame-alist' contains a `reverse' parameter, or
the X resource ``reverseVideo'' is present, handle that.
Value is the new frame created."
  (setq parameters (x-handle-named-frame-geometry parameters))
  (let ((visibility-spec (assq 'visibility parameters))
	(frame-list (frame-list))
	(frame (x-create-frame (cons '(visibility . nil) parameters)))
	success)
    (unwind-protect
	(progn
	  (x-handle-reverse-video frame parameters)
	  (frame-set-background-mode frame)
	  (face-set-after-frame-default frame)
	  (if (or (null frame-list) (null visibility-spec))
	      (make-frame-visible frame)
	    (modify-frame-parameters frame (list visibility-spec)))
	  (setq success t))
      (unless success
	(delete-frame frame)))
    frame))


(defun face-set-after-frame-default (frame)
  "Set frame-local faces of FRAME from face specs and resources.
Initialize colors of certain faces from frame parameters."
  (dolist (face (face-list))
    (let ((spec (or (get face 'saved-face)
		    (get face 'face-defface-spec))))
      (when spec
	(face-spec-set face spec frame))
      (internal-merge-in-global-face face frame)
      (when (memq window-system '(x w32))
	(make-face-x-resource-internal face frame))))

  ;; Initialize attributes from frame parameters.
  (let ((params '((foreground-color default :foreground)
		  (background-color default :background)
		  (border-color border :background)
		  (cursor-color cursor :background)
		  (scroll-bar-foreground scroll-bar :foreground)
		  (scroll-bar-background scroll-bar :background)
		  (mouse-color mouse :background))))
    (while params
      (let ((param-name (nth 0 (car params)))
	    (face (nth 1 (car params)))
	    (attr (nth 2 (car params)))
	    value)
	(when (setq value (frame-parameter frame param-name))
	  (set-face-attribute face frame attr value)))
      (setq params (cdr params)))))


(defun tty-create-frame-with-faces (&optional parameters)
  "Create a frame from optional frame parameters PARAMETERS.
Parameters not specified by PARAMETERS are taken from
`default-frame-alist'.  If either PARAMETERS or `default-frame-alist'
contains a `reverse' parameter, handle that.  Value is the new frame
created."
  (let ((frame (make-terminal-frame parameters))
	success)
    (unwind-protect
	(progn
	  (frame-set-background-mode frame)
	  (face-set-after-frame-default frame)
	  (setq success t))
      (unless success
	(delete-frame frame)))
    frame))


;; Called from C function init_display to initialize faces of the
;; dumped terminal frame on startup.

(defun tty-set-up-initial-frame-faces ()
  (let ((frame (selected-frame)))
    (frame-set-background-mode frame)
    (face-set-after-frame-default frame)))
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compatiblity with 20.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Update a frame's faces when we change its default font.

(defun frame-update-faces (frame)
  nil)


;; Update the colors of FACE, after FRAME's own colors have been
;; changed.

(defun frame-update-face-colors (frame)
  (frame-set-background-mode frame))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Standard faces.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup basic-faces nil
  "The standard faces of Emacs."
  :group 'faces)


(defface default
  '((t nil))
  "Basic default face."
  :group 'basic-faces)


(defface mode-line
  '((((type x) (class color))
     (:box (:line-width 2 :style released-button) :background "grey75"))
    (t
     (:inverse-video t)))
  "Basic mode line face."
  :version "21.1"
  :group 'modeline
  :group 'basic-faces)

;; Make `modeline' an alias for `mode-line', for compatibility.
(put 'modeline 'face-alias 'mode-line)

(defface header-line
  '((((type x) (class color))
     (:box (:line-width 2 :style released-button) :background "grey75"))
    (t
     (:inverse-video t)))
  "Basic header-line face."
  :version "21.1"
  :group 'basic-faces)


(defface tool-bar
  '((((type x) (class color))
     (:box (:line-width 1 :style released-button) :background "grey75"))
    (((type x) (class mono))
     (:box (:line-width 1 :style released-button) :background "grey"))
    (t
     ()))
  "Basic tool-bar face."
  :version "21.1"
  :group 'basic-faces)


(defface region
  '((((type tty) (class color))
     (:background "blue" :foreground "white"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "blue"))
    (((class color) (background light))
     (:background "lightblue"))
    (t (:background "gray")))
  "Basic face for highlighting the region."
  :group 'basic-faces)


(defface fringe
  '((((class color))
     (:background "grey95"))
    (t
     (:background "gray")))
  "Basic face for the fringes to the left and right of windows under X."
  :version "21.1"
  :group 'frames
  :group 'basic-faces)


(defface scroll-bar '()
  "Basic face for the scroll bar colors under X."
  :version "21.1"
  :group 'frames
  :group 'basic-faces)


(defface menu
  '((((type x-toolkit)) ())
    (t (:inverse-video t)))
  "Basic menu face."
  :version "21.1"
  :group 'menu
  :group 'basic-faces)


(defface border '()
  "Basic face for the frame border under X."
  :version "21.1"
  :group 'frames
  :group 'basic-faces)


(defface cursor '()
  "Basic face for the cursor color under X."
  :version "21.1"
  :group 'cursor
  :group 'basic-faces)


(defface mouse '()
  "Basic face for the mouse color under X."
  :version "21.1"
  :group 'mouse
  :group 'basic-faces)


(defface bold '((t (:weight bold)))
  "Basic bold face."
  :group 'basic-faces)


(defface italic '((t (:slant italic)))
  "Basic italic font."
  :group 'basic-faces)


(defface bold-italic '((t (:weight bold :slant italic)))
  "Basic bold-italic face."
  :group 'basic-faces)


(defface underline '((t (:underline t)))
  "Basic underlined face."
  :group 'basic-faces)


(defface highlight
  '((((type tty) (class color))
     (:background "green"))
    (((class color) (background light))
     (:background "darkseagreen2"))
    (((class color) (background dark))
     (:background "darkolivegreen"))
    (t (:inverse-video t)))
  "Basic face for highlighting."
  :group 'basic-faces)


(defface secondary-selection
  '((((type tty) (class color))
     (:background "cyan"))
    (((class color) (background light))
     (:background "yellow"))
    (((class color) (background dark))
     (:background "yellow"))
    (t (:inverse-video t)))
  "Basic face for displaying the secondary selection."
  :group 'basic-faces)


(defface fixed-pitch '((t (:family "courier*")))
  "The basic fixed-pitch face."
  :group 'basic-faces)


(defface variable-pitch '((t (:family "helv*")))
  "The basic variable-pitch face."
  :group 'basic-faces)


(defface trailing-whitespace
  '((((class color) (background light))
     (:background "red"))
    (((class color) (background dark))
     (:background "red"))
    (t (:inverse-video t)))
  "Basic face for highlighting trailing whitespace."
  :version "21.1"
  :group 'font-lock			; like `show-trailing-whitespace'
  :group 'basic-faces)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Manipulating font names.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is here for compatibilty with Emacs 20.2.  For example,
;; international/fontset.el uses these functions to manipulate font
;; names.  The following functions are not used in the face
;; implementation itself.

(defvar x-font-regexp nil)
(defvar x-font-regexp-head nil)
(defvar x-font-regexp-weight nil)
(defvar x-font-regexp-slant nil)

(defconst x-font-regexp-weight-subnum 1)
(defconst x-font-regexp-slant-subnum 2)
(defconst x-font-regexp-swidth-subnum 3)
(defconst x-font-regexp-adstyle-subnum 4)

;;; Regexps matching font names in "Host Portable Character Representation."
;;;
(let ((- 		"[-?]")
      (foundry		"[^-]+")
      (family 		"[^-]+")
      (weight		"\\(bold\\|demibold\\|medium\\)")		; 1
;     (weight\?		"\\(\\*\\|bold\\|demibold\\|medium\\|\\)")	; 1
      (weight\?		"\\([^-]*\\)")					; 1
      (slant		"\\([ior]\\)")					; 2
;     (slant\?		"\\([ior?*]?\\)")				; 2
      (slant\?		"\\([^-]?\\)")					; 2
;     (swidth		"\\(\\*\\|normal\\|semicondensed\\|\\)")	; 3
      (swidth		"\\([^-]*\\)")					; 3
;     (adstyle		"\\(\\*\\|sans\\|\\)")				; 4
      (adstyle		"\\([^-]*\\)")					; 4
      (pixelsize	"[0-9]+")
      (pointsize	"[0-9][0-9]+")
      (resx		"[0-9][0-9]+")
      (resy		"[0-9][0-9]+")
      (spacing		"[cmp?*]")
      (avgwidth		"[0-9]+")
      (registry		"[^-]+")
      (encoding		"[^-]+")
      )
  (setq x-font-regexp
	(concat "\\`\\*?[-?*]"
		foundry - family - weight\? - slant\? - swidth - adstyle -
		pixelsize - pointsize - resx - resy - spacing - avgwidth -
		registry - encoding "\\*?\\'"
		))
  (setq x-font-regexp-head
	(concat "\\`[-?*]" foundry - family - weight\? - slant\?
		"\\([-*?]\\|\\'\\)"))
  (setq x-font-regexp-slant (concat - slant -))
  (setq x-font-regexp-weight (concat - weight -))
  nil)	    


(defun x-resolve-font-name (pattern &optional face frame)
  "Return a font name matching PATTERN.
All wildcards in PATTERN become substantiated.
If PATTERN is nil, return the name of the frame's base font, which never
contains wildcards.
Given optional arguments FACE and FRAME, return a font which is
also the same size as FACE on FRAME, or fail."
  (or (symbolp face)
      (setq face (face-name face)))
  (and (eq frame t)
       (setq frame nil))
  (if pattern
      ;; Note that x-list-fonts has code to handle a face with nil as its font.
      (let ((fonts (x-list-fonts pattern face frame 1)))
	(or fonts
	    (if face
		(if (string-match "\\*" pattern)
		    (if (null (face-font face))
			(error "No matching fonts are the same height as the frame default font")
		      (error "No matching fonts are the same height as face `%s'" face))
		  (if (null (face-font face))
		      (error "Height of font `%s' doesn't match the frame default font"
			     pattern)
		    (error "Height of font `%s' doesn't match face `%s'"
			   pattern face)))
	      (error "No fonts match `%s'" pattern)))
	(car fonts))
    (cdr (assq 'font (frame-parameters (selected-frame))))))


(defun x-frob-font-weight (font which)
  (let ((case-fold-search t))
    (cond ((string-match x-font-regexp font)
	   (concat (substring font 0
			      (match-beginning x-font-regexp-weight-subnum))
		   which
		   (substring font (match-end x-font-regexp-weight-subnum)
			      (match-beginning x-font-regexp-adstyle-subnum))
		   ;; Replace the ADD_STYLE_NAME field with *
		   ;; because the info in it may not be the same
		   ;; for related fonts.
		   "*"
		   (substring font (match-end x-font-regexp-adstyle-subnum))))
	  ((string-match x-font-regexp-head font)
	   (concat (substring font 0 (match-beginning 1)) which
		   (substring font (match-end 1))))
	  ((string-match x-font-regexp-weight font)
	   (concat (substring font 0 (match-beginning 1)) which
		   (substring font (match-end 1)))))))


(defun x-frob-font-slant (font which)
  (let ((case-fold-search t))
    (cond ((string-match x-font-regexp font)
	   (concat (substring font 0
			      (match-beginning x-font-regexp-slant-subnum))
		   which
		   (substring font (match-end x-font-regexp-slant-subnum)
			      (match-beginning x-font-regexp-adstyle-subnum))
		   ;; Replace the ADD_STYLE_NAME field with *
		   ;; because the info in it may not be the same
		   ;; for related fonts.
		   "*"
		   (substring font (match-end x-font-regexp-adstyle-subnum))))
	  ((string-match x-font-regexp-head font)
	   (concat (substring font 0 (match-beginning 2)) which
		   (substring font (match-end 2))))
	  ((string-match x-font-regexp-slant font)
	   (concat (substring font 0 (match-beginning 1)) which
		   (substring font (match-end 1)))))))


(defun x-make-font-bold (font)
  "Given an X font specification, make a bold version of it.
If that can't be done, return nil."
  (x-frob-font-weight font "bold"))


(defun x-make-font-demibold (font)
  "Given an X font specification, make a demibold version of it.
If that can't be done, return nil."
  (x-frob-font-weight font "demibold"))


(defun x-make-font-unbold (font)
  "Given an X font specification, make a non-bold version of it.
If that can't be done, return nil."
  (x-frob-font-weight font "medium"))


(defun x-make-font-italic (font)
  "Given an X font specification, make an italic version of it.
If that can't be done, return nil."
  (x-frob-font-slant font "i"))


(defun x-make-font-oblique (font) ; you say tomayto...
  "Given an X font specification, make an oblique version of it.
If that can't be done, return nil."
  (x-frob-font-slant font "o"))


(defun x-make-font-unitalic (font)
  "Given an X font specification, make a non-italic version of it.
If that can't be done, return nil."
  (x-frob-font-slant font "r"))


(defun x-make-font-bold-italic (font)
  "Given an X font specification, make a bold and italic version of it.
If that can't be done, return nil."
  (and (setq font (x-make-font-bold font))
       (x-make-font-italic font)))


(provide 'faces)

;;; end of faces.el
