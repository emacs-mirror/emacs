;;; cus-face.el -- customization support for faces.
;;
;; Copyright (C) 1996, 1997 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: help, faces
;; Version: Emacs
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/

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

(defalias 'custom-facep 
  (if (fboundp 'facep) 'facep
    '(lambda (face) nil)))

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

;;; Font Attributes.

(defconst custom-face-attributes
  '((:bold (boolean :tag "Bold"
		    :help-echo "Control whether a bold font should be used.")
	   set-face-bold-p
	   face-bold-p)
    (:italic (boolean :tag "Italic"
		      :help-echo "\
Control whether an italic font should be used.")
	     set-face-italic-p
	     face-italic-p)
    (:underline (boolean :tag "Underline"
			 :help-echo "\
Control whether the text should be underlined.")
		set-face-underline-p
		face-underline-p)
    (:inverse-video (boolean :tag "Inverse Video"
			     :help-echo "\
Control whether the text should be in inverse video.")
		set-face-inverse-video-p
		face-inverse-video-p)
    (:foreground (color :tag "Foreground"
			:value ""
			:help-echo "Set foreground color.")
		 set-face-foreground
		 face-foreground)
    (:background (color :tag "Background"
			:value ""
			:help-echo "Set background color.")
		 set-face-background
		 face-background)
    (:stipple (editable-field :format "Stipple: %v"
			      :help-echo "Name of background bitmap file.")
	      set-face-stipple
	      face-stipple))
  "Alist of face attributes. 
The elements are of the form (KEY TYPE SET GET),
where KEY is the name of the attribute,
TYPE is a widget type for editing the attibute,
SET is a function for setting the attribute value,
and GET is a function for getiing the attribute value. 

The SET function should take three arguments, the face to modify, the
value of the attribute, and optionally the frame where the face should
be changed.

The GET function should take two arguments, the face to examine, and
optionally the frame where the face should be examined.")

(defun custom-face-attributes-get (face frame)
  "For FACE on FRAME, return an alternating list describing its attributes.
The list has the form (KEYWORD VALUE KEYWORD VALUE...).
Each keyword should be listed in `custom-face-attributes'.
We include only those attributes that differ from the default face.

If FRAME is nil, use the global defaults for FACE."
  (let ((atts custom-face-attributes)
	att result get)
    (while atts
      (setq att (car atts)
	    atts (cdr atts)
	    get (nth 3 att))
      (when get 
	(let ((answer (funcall get face frame)))
	  (if (and (not (equal answer (funcall get 'default frame)))
		   (widget-apply (nth 1 att) :match answer))
	      (setq result (cons (nth 0 att) (cons answer result)))))))
    result))

;;; Initializing.

;;;###autoload
(defun custom-set-faces (&rest args)
  "Initialize faces according to user preferences.
The arguments should be a list where each entry has the form:

  (FACE SPEC [NOW])

SPEC is stored as the saved value for FACE.
If NOW is present and non-nil, FACE is created now, according to SPEC.

See `defface' for the format of SPEC."
  (while args
    (let ((entry (car args)))
      (if (listp entry)
	  (let ((face (nth 0 entry))
		(spec (nth 1 entry))
		(now (nth 2 entry)))
	    (put face 'saved-face spec)
	    (when now
	      (put face 'force-face t))
	    (when (or now (facep face))
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

;; cus-face.el ends here
