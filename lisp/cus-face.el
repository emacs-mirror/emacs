;;; cus-face.el -- customization support for faces.
;;
;; Copyright (C) 1996, 1997, 1999 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: help, faces
;; Version: Emacs
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/ (probably obsolete)

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
;;
;; See `custom.el'.

;;; Code:

(defalias 'custom-facep 'facep)

;;; Declaring a face.

;;;###autoload
(defun custom-declare-face (face spec doc &rest args)
  "Like `defface', but FACE is evaluated as a normal argument."
  (unless (get face 'face-defface-spec)
    (put face 'face-defface-spec spec)
    (when (fboundp 'facep)
      (unless (facep face)
	;; If the user has already created the face, respect that.
	(let ((value (or (get face 'saved-face) spec))
	      (frames (frame-list))
	      frame)
	  ;; Create global face.
	  (make-empty-face face)
	  ;; Create frame local faces
	  (while frames
	    (setq frame (car frames)
		  frames (cdr frames))
	    (face-spec-set face value frame)))
	;; When making a face after frames already exist
	(if (memq window-system '(x w32))
	    (make-face-x-resource-internal face))))
    (when (and doc (null (face-documentation face)))
      (set-face-documentation face doc))
    (custom-handle-all-keywords face args 'custom-face)
    (run-hooks 'custom-define-hook))
  face)

;;; Face attributes.

;; Below, nil is used in widget specifications for `unspecified' face
;; attributes and `off' is used instead of nil attribute values.  The
;; reason for this is that nil corresponds to the result you get when
;; looking up an attribute in a defface spec that isn't contained in
;; the spec.

(defconst custom-face-attributes
  '((:family
     (choice :tag "Font family"
	     :help-echo "Font family or fontset alias name."
	     (const :tag "*" nil)
	     (string :tag "Family"))
     (lambda (face value &optional frame)
       (set-face-attribute face frame :family (or value 'unspecified)))
     (lambda (face &optional frame)
       (let ((family (face-attribute face :family frame)))
	 (if (eq family 'unspecified) nil family))))
    
    (:width
     (choice :tag "Width"
	     :help-echo "Font width."
	     (const :tag "*" nil)
	     (const :tag "compressed" condensed)
	     (const :tag "condensed" condensed)
	     (const :tag "demiexpanded" semi-expanded)
	     (const :tag "expanded" expanded)
	     (const :tag "extracondensed" extra-condensed)
	     (const :tag "extraexpanded" extra-expanded)
	     (const :tag "medium" normal)
	     (const :tag "narrow" condensed)
	     (const :tag "normal" normal)
	     (const :tag "regular" normal)
	     (const :tag "semicondensed" semi-condensed)
	     (const :tag "semiexpanded" semi-expanded)
	     (const :tag "ultracondensed" ultra-condensed)
	     (const :tag "ultraexpanded" ultra-expanded)
	     (const :tag "wide" extra-expanded))
     (lambda (face value &optional frame)
       (set-face-attribute face frame :width (or value 'unspecified)))
     (lambda (face &optional frame)
       (let ((width (face-attribute face :width frame)))
	 (if (eq width 'unspecified) nil width))))
    
    (:height
     (choice :tag "Height"
	     :help-echo "Face's font height."
	     (const :tag "*" nil)
	     (integer :tag "Height in 1/10 pt"))
     (lambda (face value &optional frame)
       (set-face-attribute face frame :height (or value 'unspecified)))
     (lambda (face &optional frame)
       (let ((height (face-attribute face :height frame)))
	 (if (eq height 'unspecified) nil height))))
    
    (:weight
     (choice :tag "Weight"
	     :help-echo "Font weight."
	     (const :tag "*" nil)
	     (const :tag "black" ultra_bold)
	     (const :tag "bold" bold)
	     (const :tag "book" semi-light)
	     (const :tag "demibold" semi-bold)
	     (const :tag "extralight" extra-light)
	     (const :tag "extrabold" extra-bold)
	     (const :tag "heavy" extra-bold)
	     (const :tag "light" light)
	     (const :tag "medium" normal)
	     (const :tag "normal" normal)
	     (const :tag "regular" normal)
	     (const :tag "semibold" semi-bold)
	     (const :tag "semilight" semi-light)
	     (const :tag "ultralight" ultra-light)
	     (const :tag "ultrabold" ultra-bold))
     (lambda (face value &optional frame)
       (set-face-attribute face frame :weight (or value 'unspecified)))
     (lambda (face &optional frame)
       (let ((weight (face-attribute face :weight frame)))
	 (if (eq weight 'unspecified) nil weight))))
    
    (:slant
     (choice :tag "Slant"
	     :help-echo "Font slant."
	     (const :tag "*" nil)
	     (const :tag "italic" italic)
	     (const :tag "oblique" oblique)
	     (const :tag "normal" normal))
     (lambda (face value &optional frame)
       (set-face-attribute face frame :slant (or value 'unspecified)))
     (lambda (face &optional frame)
       (let ((slant (face-attribute face :slant frame)))
	 (if (eq slant 'unspecified) nil slant))))
    
    (:underline
     (choice :tag "Underline"
	     :help-echo "Control text underlining."
	     (const :tag "*" nil)
	     (const :tag "On" t)
	     (const :tag "Off" off)
	     (color :tag "Colored"))
     (lambda (face value &optional frame)
       (cond ((eq value 'off) (setq value nil))
	     ((null value) (setq value 'unspecified)))
       (set-face-attribute face frame :underline value))
     (lambda (face &optional frame)
       (let ((underline (face-attribute face :underline frame)))
	 (cond ((eq underline 'unspecified) nil)
	       ((null underline) 'off)))))
    
    (:overline
     (choice :tag "Overline"
	     :help-echo "Control text overlining."
	     (const :tag "*" nil)
	     (const :tag "On" t)
	     (const :tag "Off" off)
	     (color :tag "Colored"))
     (lambda (face value &optional frame)
       (cond ((eq value 'off) (setq value nil))
	     ((null value) (setq value 'unspecified)))
       (set-face-attribute face frame :overline value))
     (lambda (face &optional frame)
       (let ((overline (face-attribute face :overline frame)))
	 (cond ((eq overline 'unspecified) nil)
	       ((null overline) 'off)))))
    
    (:strike-through
     (choice :tag "Strike-through"
	     :help-echo "Control text strike-through."
	     (const :tag "*" nil)
	     (const :tag "On" t)
	     (const :tag "Off" off)
	     (color :tag "Colored"))
     (lambda (face value &optional frame)
       (cond ((eq value 'off) (setq value nil))
	     ((null value) (setq value 'unspecified)))
       (set-face-attribute face frame :strike-through value))
     (lambda (face &optional frame)
       (let ((value (face-attribute face :strike-through frame)))
	 (cond ((eq value 'unspecified) (setq value nil))
	       ((null value) (setq value 'off)))
	 value)))
    
    (:box
     ;; Fixme: this can probably be done better.
     (choice :tag "Box around text"
	     :help-echo "Control box around text."
	     (const :tag "*" t)
	     (const :tag "Off" nil)
	     (list :tag "Box"
		   :value (:line-width 2 :color "grey75"
				       :style released-button)
		   (const :format "" :value :line-width)
		   (integer :tag "Width")
		   (const :format "" :value :color)
		   (choice :tag "Color" (const :tag "*" nil) color)
		   (const :format "" :value :style)
		   (choice :tag "Style"
			   (const :tag "Raised" released-button)
			   (const :tag "Sunken" pressed-button)
			   (const :tag "None" nil))))
     (lambda (face value &optional frame)
       (set-face-attribute face frame :box value))
     (lambda (face &optional frame)
       (let ((value (face-attribute face :box frame)))
	 (if (consp value)
	     (list :line-width (or (plist-get value :line-width) 1)
		   :color (plist-get value :color)
		   :style (plist-get value :style))
	   value))))
    
    (:inverse-video
     (choice :tag "Inverse-video"
	     :help-echo "Control whether text should be in inverse-video."
	     (const :tag "*" nil)
	     (const :tag "On" t)
	     (const :tag "Off" off))
     (lambda (face value &optional frame)
       (cond ((eq value 'off) (setq value nil))
	     ((null value) (setq value 'unspecified)))
       (set-face-attribute face frame :inverse-video value))
     (lambda (face &optional frame)
       (let ((value (face-attribute face :inverse-video frame)))
	 (cond ((eq value 'unspecified)
		nil)
	       ((null value)'off)))))
    
    (:foreground
     (choice :tag "Foreground"
	     :help-echo "Set foreground color."
	     (const :tag "*" nil)
	     (color :tag "Color"))
     (lambda (face value &optional frame)
       (set-face-attribute face frame :foreground (or value 'unspecified)))
     (lambda (face &optional frame)
       (let ((value (face-attribute face :foreground frame)))
	 (if (eq value 'unspecified) nil value))))
    
    (:background
     (choice :tag "Background"
	     :help-echo "Set background color."
	     (const :tag "*" nil)
	     (color :tag "Color"))
     (lambda (face value &optional frame)
       (set-face-attribute face frame :background (or value 'unspecified)))
     (lambda (face &optional frame)
       (let ((value (face-attribute face :background frame)))
	 (if (eq value 'unspecified) nil value))))
    
    (:stipple
     (choice :tag "Stipple"
	     :help-echo "Name of background bitmap file."
	     (const :tag "*" nil)
	     (file :tag "File" :must-match t))
     (lambda (face value &optional frame)
       (set-face-attribute face frame :stipple (or value 'unspecified)))
     (lambda (face &optional frame)
       (let ((value (face-attribute face :stipple frame)))
	 (if (eq value 'unspecified) nil value)))))
       
  "Alist of face attributes.

The elements are of the form (KEY TYPE SET GET), where KEY is the name
of the attribute, TYPE is a widget type for editing the attibute, SET
is a function for setting the attribute value, and GET is a function
for getiing the attribute value.

The SET function should take three arguments, the face to modify, the
value of the attribute, and optionally the frame where the face should
be changed.

The GET function should take two arguments, the face to examine, and
optionally the frame where the face should be examined.")


(defun custom-face-attributes-get (face frame)
  "For FACE on FRAME, return an alternating list describing its attributes.
The list has the form (KEYWORD VALUE KEYWORD VALUE...).
Each keyword should be listed in `custom-face-attributes'.

If FRAME is nil, use the global defaults for FACE."
  (let ((attrs custom-face-attributes)
	plist)
    (while attrs
      (let* ((attribute (car (car attrs)))
	     (value (face-attribute face attribute frame)))
	(setq attrs (cdr attrs))
	(unless (eq value 'unspecified)
	  (setq plist (cons attribute (cons value plist))))))
    plist))

;;; Initializing.

;;;###autoload
(defun custom-set-faces (&rest args)
  "Initialize faces according to user preferences.
The arguments should be a list where each entry has the form:

  (FACE SPEC [NOW [COMMENT]])

SPEC is stored as the saved value for FACE.
If NOW is present and non-nil, FACE is created now, according to SPEC.
COMMENT is a string comment about FACE.

See `defface' for the format of SPEC."
  (while args
    (let ((entry (car args)))
      (if (listp entry)
	  (let ((face (nth 0 entry))
		(spec (nth 1 entry))
		(now (nth 2 entry))
		(comment (nth 3 entry)))
	    (put face 'saved-face spec)
	    (put face 'saved-face-comment comment)
	    (when now
	      (put face 'force-face t))
	    (when (or now (facep face))
	      (put face 'face-comment comment)
	      (make-empty-face face)
	      (face-spec-set face spec))
	    (setq args (cdr args)))
	;; Old format, a plist of FACE SPEC pairs.
	(let ((face (nth 0 args))
	      (spec (nth 1 args)))
	  (put face 'saved-face spec))
	(setq args (cdr (cdr args)))))))

;;; The End.

(provide 'cus-face)

;;; cus-face.el ends here
