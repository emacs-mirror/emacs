;;; cus-face.el --- customization support for faces
;;
;; Copyright (C) 1996, 1997, 1999, 2000 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: help, faces
;; Version: Emacs

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
      (set-face-documentation face (purecopy doc)))
    (custom-handle-all-keywords face args 'custom-face)
    (run-hooks 'custom-define-hook))
  face)

;;; Face attributes.

(defconst custom-face-attributes
  '((:family
     (string :tag "Font Family"
	     :help-echo "Font family or fontset alias name."))
    
    (:width
     (choice :tag "Width"
	     :help-echo "Font width."
	     :value normal		; default
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
	     (const :tag "wide" extra-expanded)))
    
    (:height
     (choice :tag "Height"
	     :help-echo "Face's font height."
	     :value 1.0			; default
	     (integer :tag "Height in 1/10 pt")
	     (number :tag "Scale" 1.0)))

    (:weight
     (choice :tag "Weight"
	     :help-echo "Font weight."
	     :value normal		; default
	     (const :tag "black" ultra-bold)
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
	     (const :tag "ultrabold" ultra-bold)))
    
    (:slant
     (choice :tag "Slant"
	     :help-echo "Font slant."
	     :value normal		; default
	     (const :tag "italic" italic)
	     (const :tag "oblique" oblique)
	     (const :tag "normal" normal)))
    
    (:underline
     (choice :tag "Underline"
	     :help-echo "Control text underlining."
	     (const :tag "Off" nil)
	     (const :tag "On" t)
	     (color :tag "Colored")))
    
    (:overline
     (choice :tag "Overline"
	     :help-echo "Control text overlining."
	     (const :tag "Off" nil)
	     (const :tag "On" t)
	     (color :tag "Colored")))
    
    (:strike-through
     (choice :tag "Strike-through"
	     :help-echo "Control text strike-through."
	     (const :tag "Off" nil)
	     (const :tag "On" t)
	     (color :tag "Colored")))
    
    (:box
     ;; Fixme: this can probably be done better.
     (choice :tag "Box around text"
	     :help-echo "Control box around text."
	     (const :tag "Off" nil)
	     (list :tag "Box"
		   :value (:line-width 2 :color "grey75" :style released-button)
		   (const :format "" :value :line-width)
		   (integer :tag "Width")
		   (const :format "" :value :color)
		   (choice :tag "Color" (const :tag "*" nil) color)
		   (const :format "" :value :style)
		   (choice :tag "Style"
			   (const :tag "Raised" released-button)
			   (const :tag "Sunken" pressed-button)
			   (const :tag "None" nil))))
     ;; filter to make value suitable for customize
     (lambda (real-value)
       (and real-value
	    (let ((lwidth
		   (or (and (consp real-value)
			    (plist-get real-value :line-width))
		       (and (integerp real-value) real-value)
		       1))
		  (color
		   (or (and (consp real-value) (plist-get real-value :color))
		       (and (stringp real-value) real-value)
		       nil))
		  (style
		   (and (consp real-value) (plist-get real-value :style))))
	      (list :line-width lwidth :color color :style style))))
     ;; filter to make customized-value suitable for storing
     (lambda (cus-value)
       (and cus-value
	    (let ((lwidth (plist-get cus-value :line-width))
		  (color (plist-get cus-value :color))
		  (style (plist-get cus-value :style)))
	      (cond ((and (null color) (null style))
		     lwidth)
		    ((and (null lwidth) (null style))
		     ;; actually can't happen, because LWIDTH is always an int
		     color)
		    (t
		     ;; Keep as a plist, but remove null entries
		     (nconc (and lwidth `(:line-width ,lwidth))
			    (and color  `(:color ,color))
			    (and style  `(:style ,style)))))))))
    
    (:inverse-video
     (choice :tag "Inverse-video"
	     :help-echo "Control whether text should be in inverse-video."
	     (const :tag "Off" nil)
	     (const :tag "On" t)))
    
    (:foreground
     (color :tag "Foreground"
	    :help-echo "Set foreground color."))
    
    (:background
     (color :tag "Background"
	    :help-echo "Set background color."))
    
    (:stipple
     (choice :tag "Stipple"
	     :help-echo "Background bit-mask"
	     (const :tag "None" nil)
	     (file :tag "File"
		   :help-echo "Name of bitmap file."
		   :must-match t)))

    (:inherit
     (repeat :tag "Inherit"
	     :help-echo "List of faces to inherit attributes from."
	     (face :Tag "Face" default))
     ;; filter to make value suitable for customize
     (lambda (real-value)
       (cond ((or (null real-value) (eq real-value 'unspecified))
	      nil)
	     ((symbolp real-value)
	      (list real-value))
	     (t
	      real-value)))
     ;; filter to make customized-value suitable for storing
     (lambda (cus-value)
       (if (and (consp cus-value) (null (cdr cus-value)))
	   (car cus-value)
	 cus-value))))
       
  "Alist of face attributes.

The elements are of the form (KEY TYPE PRE-FILTER POST-FILTER),
where KEY is the name of the attribute, TYPE is a widget type for
editing the attribute, PRE-FILTER is a function to make the attribute's
value suitable for the customization widget, and POST-FILTER is a
function to make the customized value suitable for storing.  PRE-FILTER
and POST-FILTER are optional.

The PRE-FILTER should take a single argument, the attribute value as
stored, and should return a value for customization (using the
customization type TYPE).

The POST-FILTER should also take a single argument, the value after
being customized, and should return a value suitable for setting the
given face attribute.")


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
	(unless (or (eq value 'unspecified)
		    (and (null value) (memq attribute '(:inherit))))
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
