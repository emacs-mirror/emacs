;;; faces.el --- Lisp interface to the c "face" structure

;; Copyright (C) 1992, 1993 Free Software Foundation, Inc.

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

;;; Commentary:

;; Mostly derived from Lucid.

;;; Code:


;;;; Functions for manipulating face vectors.

;;; A face vector is a vector of the form:
;;;    [face ID FONT FOREGROUND BACKGROUND BACKGROUND-PIXMAP UNDERLINE]

;;; Type checkers.
(defsubst internal-facep (x)
  (and (vectorp x) (= (length x) 8) (eq (aref x 0) 'face)))

(defmacro internal-check-face (face)
  (` (while (not (internal-facep (, face)))
       (setq (, face) (signal 'wrong-type-argument (list 'internal-facep (, face)))))))

;;; Accessors.
(defsubst face-name (face)
  "Return the name of face FACE."
  (aref (internal-get-face face) 1))

(defsubst face-id (face)
  "Return the internal ID number of face FACE."
  (aref (internal-get-face face) 2))

(defsubst face-font (face &optional frame)
  "Return the font name of face FACE, or nil if it is unspecified.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
  The font default for a face is either nil, or a list
  of the form (bold), (italic) or (bold italic).
If FRAME is omitted or nil, use the selected frame."
  (aref (internal-get-face face frame) 3))

(defsubst face-foreground (face &optional frame)
  "Return the foreground color name of face FACE, or nil if unspecified.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
  (aref (internal-get-face face frame) 4))

(defsubst face-background (face &optional frame)
  "Return the background color name of face FACE, or nil if unspecified.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
  (aref (internal-get-face face frame) 5))

;;(defsubst face-background-pixmap (face &optional frame)
;; "Return the background pixmap name of face FACE, or nil if unspecified.
;;If the optional argument FRAME is given, report on face FACE in that frame.
;;Otherwise report on the defaults for face FACE (for new frames)."
;; (aref (internal-get-face face frame) 6))

(defsubst face-underline-p (face &optional frame)
 "Return t if face FACE is underlined.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
 (aref (internal-get-face face frame) 7))


;;; Mutators.

(defsubst set-face-font (face font &optional frame)
  "Change the font of face FACE to FONT (a string).
If the optional FRAME argument is provided, change only
in that frame; otherwise change each frame."
  (interactive (internal-face-interactive "font"))
  (if (stringp font) (setq font (x-resolve-font-name font face frame)))
  (internal-set-face-1 face 'font font 3 frame))

(defsubst set-face-foreground (face color &optional frame)
  "Change the foreground color of face FACE to COLOR (a string).
If the optional FRAME argument is provided, change only
in that frame; otherwise change each frame."
  (interactive (internal-face-interactive "foreground"))
  (internal-set-face-1 face 'foreground color 4 frame))

(defsubst set-face-background (face color &optional frame)
  "Change the background color of face FACE to COLOR (a string).
If the optional FRAME argument is provided, change only
in that frame; otherwise change each frame."
  (interactive (internal-face-interactive "background"))
  (internal-set-face-1 face 'background color 5 frame))

;;(defsubst set-face-background-pixmap (face name &optional frame)
;;  "Change the background pixmap of face FACE to PIXMAP.
;;PIXMAP should be a string, the name of a file of pixmap data.
;;The directories listed in the `x-bitmap-file-path' variable are searched.

;;Alternatively, PIXMAP may be a list of the form (WIDTH HEIGHT DATA)
;;where WIDTH and HEIGHT are the size in pixels,
;;and DATA is a string, containing the raw bits of the bitmap.  

;;If the optional FRAME argument is provided, change only
;;in that frame; otherwise change each frame."
;;  (interactive (internal-face-interactive "background-pixmap"))
;;  (internal-set-face-1 face 'background-pixmap name 6 frame))

(defsubst set-face-underline-p (face underline-p &optional frame)
  "Specify whether face FACE is underlined.  (Yes if UNDERLINE-P is non-nil.)
If the optional FRAME argument is provided, change only
in that frame; otherwise change each frame."
  (interactive (internal-face-interactive "underline-p" "underlined"))
  (internal-set-face-1 face 'underline underline-p 7 frame))


;;;; Associating face names (symbols) with their face vectors.

(defvar global-face-data nil
  "Internal data for face support functions.  Not for external use.
This is an alist associating face names with the default values for
their parameters.  Newly created frames get their data from here.")

(defun face-list ()
  "Returns a list of all defined face names."
  (mapcar 'car global-face-data))

(defun internal-find-face (name &optional frame)
  "Retrieve the face named NAME.  Return nil if there is no such face.
If the optional argument FRAME is given, this gets the face NAME for
that frame; otherwise, it uses the selected frame.
If FRAME is the symbol t, then the global, non-frame face is returned.
If NAME is already a face, it is simply returned."
  (if (and (eq frame t) (not (symbolp name)))
      (setq name (face-name name)))
  (if (symbolp name)
      (cdr (assq name
		 (if (eq frame t)
		     global-face-data
		   (frame-face-alist (or frame (selected-frame))))))
    (internal-check-face name)
    name))

(defun internal-get-face (name &optional frame)
  "Retrieve the face named NAME; error if there is none.
If the optional argument FRAME is given, this gets the face NAME for
that frame; otherwise, it uses the selected frame.
If FRAME is the symbol t, then the global, non-frame face is returned.
If NAME is already a face, it is simply returned."
  (or (internal-find-face name frame)
      (internal-check-face name)))


(defun internal-set-face-1 (face name value index frame)
  (let ((inhibit-quit t))
    (if (null frame)
	(let ((frames (frame-list)))
	  (while frames
	    (internal-set-face-1 (face-name face) name value index (car frames))
	    (setq frames (cdr frames)))
	  (aset (internal-get-face (if (symbolp face) face (face-name face)) t)
		index value)
	  value)
      (or (eq frame t)
	  (set-face-attribute-internal (face-id face) name value frame))
      (aset (internal-get-face face frame) index value))))


(defun read-face-name (prompt)
  (let (face)
    (while (= (length face) 0)
      (setq face (completing-read prompt
				  (mapcar '(lambda (x) (list (symbol-name x)))
					  (face-list))
				  nil t)))
    (intern face)))

(defun internal-face-interactive (what &optional bool)
  (let* ((fn (intern (concat "face-" what)))
	 (prompt (concat "Set " what " of face"))
	 (face (read-face-name (concat prompt ": ")))
	 (default (if (fboundp fn)
		      (or (funcall fn face (selected-frame))
			  (funcall fn 'default (selected-frame)))))
	 (value (if bool
		    (y-or-n-p (concat "Should face " (symbol-name face)
				      " be " bool "? "))
		  (read-string (concat prompt " " (symbol-name face) " to: ")
			       default))))
    (list face (if (equal value "") nil value))))



(defun make-face (name)
  "Define a new FACE on all frames.  
You can modify the font, color, etc of this face with the set-face- functions.
If the face already exists, it is unmodified."
  (interactive "SMake face: ")
  (or (internal-find-face name)
      (let ((face (make-vector 8 nil)))
	(aset face 0 'face)
	(aset face 1 name)
	(let* ((frames (frame-list))
	       (inhibit-quit t)
	       (id (internal-next-face-id)))
	  (make-face-internal id)
	  (aset face 2 id)
	  (while frames
	    (set-frame-face-alist (car frames)
				  (cons (cons name (copy-sequence face))
					(frame-face-alist (car frames))))
	    (setq frames (cdr frames)))
	  (setq global-face-data (cons (cons name face) global-face-data)))
	;; when making a face after frames already exist
	(if (eq window-system 'x)
	    (make-face-x-resource-internal face))
	face)))

;; Fill in a face by default based on X resources, for all existing frames.
;; This has to be done when a new face is made.
(defun make-face-x-resource-internal (face &optional frame set-anyway)
  (cond ((null frame)
	 (let ((frames (frame-list)))
	   (while frames
	     (make-face-x-resource-internal (face-name face)
					    (car frames) set-anyway)
	     (setq frames (cdr frames)))))
	(t
	 (setq face (internal-get-face (face-name face) frame))
	 ;;
	 ;; These are things like "attributeForeground" instead of simply
	 ;; "foreground" because people tend to do things like "*foreground",
	 ;; which would cause all faces to be fully qualified, making faces
	 ;; inherit attributes in a non-useful way.  So we've made them slightly
	 ;; less obvious to specify in order to make them work correctly in
	 ;; more random environments.
	 ;;
	 ;; I think these should be called "face.faceForeground" instead of
	 ;; "face.attributeForeground", but they're the way they are for
	 ;; hysterical reasons.
	 ;; 
	 (let* ((name (symbol-name (face-name face)))
		(fn  (or (x-get-resource (concat name ".attributeFont")
					 "Face.AttributeFont")
			 (and set-anyway (face-font face))))
		(fg  (or (x-get-resource (concat name ".attributeForeground")
					 "Face.AttributeForeground")
			 (and set-anyway (face-foreground face))))
		(bg  (or (x-get-resource (concat name ".attributeBackground")
					 "Face.AttributeBackground")
			 (and set-anyway (face-background face))))
;;		(bgp (or (x-get-resource (concat name ".attributeBackgroundPixmap")
;;					 "Face.AttributeBackgroundPixmap")
;;			 (and set-anyway (face-background-pixmap face))))
		(ulp (or (x-get-resource (concat name ".attributeUnderline")
					 "Face.AttributeUnderline")
			 (and set-anyway (face-underline-p face))))
		)
	   (if fn
	       (condition-case ()
		   (set-face-font face fn frame)
		 (error (message "font `%s' not found for face `%s'" fn name))))
	   (if fg
	       (condition-case ()
		   (set-face-foreground face fg frame)
		 (error (message "color `%s' not allocated for face `%s'" fg name))))
	   (if bg
	       (condition-case ()
		   (set-face-background face bg frame)
		 (error (message "color `%s' not allocated for face `%s'" bg name))))
;;	   (if bgp
;;	       (condition-case ()
;;		   (set-face-background-pixmap face bgp frame)
;;		 (error (message "pixmap `%s' not found for face `%s'" bgp name))))
	   (if (or ulp set-anyway)
	       (set-face-underline-p face ulp frame))
	   )))
  face)

(defun copy-face (old-face new-name &optional frame new-frame)
  "Define a face just like OLD-FACE, with name NEW-NAME.
If NEW-NAME already exists as a face, it is modified to be like OLD-FACE.
If the optional argument FRAME is given, this applies only to that frame.
Otherwise it applies to each frame separately.
If the optional fourth argument NEW-FRAME is given, 
copy the information from face OLD-FACE on frame FRAME
to face NEW-NAME on frame NEW-FRAME."
  (or new-frame (setq new-frame frame))
  (setq old-face (internal-get-face old-face frame))
  (let* ((inhibit-quit t)
	 (new-face (or (internal-find-face new-name new-frame)
		       (make-face new-name))))
    (if (null frame)
	(let ((frames (frame-list)))
	  (while frames
	    (copy-face old-face new-name (car frames))
	    (setq frames (cdr frames)))
	  (copy-face old-face new-name t))
      (set-face-font new-face (face-font old-face frame) new-frame)
      (set-face-foreground new-face (face-foreground old-face frame) new-frame)
      (set-face-background new-face (face-background old-face frame) new-frame)
;;;      (set-face-background-pixmap
;;;       new-face (face-background-pixmap old-face frame) new-frame)
      (set-face-underline-p new-face (face-underline-p old-face frame)
			    new-frame))
    new-face))

(defun face-equal (face1 face2 &optional frame)
  "True if the faces FACE1 and FACE2 display in the same way."
  (setq face1 (internal-get-face face1 frame)
	face2 (internal-get-face face2 frame))
  (and (equal (face-foreground face1 frame) (face-foreground face2 frame))
       (equal (face-background face1 frame) (face-background face2 frame))
       (equal (face-font face1 frame) (face-font face2 frame))
;;       (equal (face-background-pixmap face1 frame)
;;	      (face-background-pixmap face2 frame))
       ))

(defun face-differs-from-default-p (face &optional frame)
  "True if face FACE displays differently from the default face, on FRAME.
A face is considered to be ``the same'' as the default face if it is 
actually specified in the same way (equivalent fonts, etc) or if it is 
fully unspecified, and thus inherits the attributes of any face it 
is displayed on top of."
  (let ((default (internal-get-face 'default frame)))
    (setq face (internal-get-face face frame))
    (not (and (or (equal (face-foreground default frame)
			 (face-foreground face frame))
		  (null (face-foreground face frame)))
	      (or (equal (face-background default frame)
			 (face-background face frame))
		  (null (face-background face frame)))
	      (or (equal (face-font default frame) (face-font face frame))
		  (null (face-font face frame)))
;;;	      (or (equal (face-background-pixmap default frame)
;;;			 (face-background-pixmap face frame))
;;;		  (null (face-background-pixmap face frame)))
	      (equal (face-underline-p default frame)
		     (face-underline-p face frame))
	      ))))


(defun invert-face (face &optional frame)
  "Swap the foreground and background colors of face FACE.
If the face doesn't specify both foreground and background, then
set its foreground and background to the default background and foreground."
  (interactive (list (read-face-name "Invert face: ")))
  (setq face (internal-get-face face frame))
  (let ((fg (face-foreground face frame))
	(bg (face-background face frame)))
    (if (or fg bg)
	(progn
	  (set-face-foreground face bg frame)
	  (set-face-background face fg frame))
      (set-face-foreground face (or (face-background 'default frame)
				    (cdr (assq 'background-color (frame-parameters frame))))
			   frame)
      (set-face-background face (or (face-foreground 'default frame)
				    (cdr (assq 'foreground-color (frame-parameters frame))))
			   frame)))
  face)


(defun internal-try-face-font (face font &optional frame)
  "Like set-face-font, but returns nil on failure instead of an error."
  (condition-case ()
      (set-face-font face font frame)
    (error nil)))

;; Manipulating font names.

(defconst x-font-regexp nil)
(defconst x-font-regexp-head nil)
(defconst x-font-regexp-weight nil)
(defconst x-font-regexp-slant nil)

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
      (adstyle		"[^-]*")					; 4
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
		pixelsize - pointsize - resx - resy - spacing - registry -
		encoding "[-?*]\\*?\\'"
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
Given optional arguments FACE and FRAME, try to return a font which is
also the same size as FACE on FRAME."
  (or (symbolp face)
      (setq face (face-name face)))
  (and (eq frame t)
       (setq frame nil))
  (if pattern
      ;; Note that x-list-fonts has code to handle a face with nil as its font.
      (let ((fonts (x-list-fonts pattern face frame)))
	(or fonts
	    (if face
		(error "No fonts matching pattern are the same size as `%s'"
		       face)
	      (error "No fonts match `%s'" pattern)))
	(car fonts))
    (cdr (assq 'font (frame-parameters (selected-frame))))))

(defun x-frob-font-weight (font which)
  (if (or (string-match x-font-regexp font)
	  (string-match x-font-regexp-head font)
	  (string-match x-font-regexp-weight font))
      (concat (substring font 0 (match-beginning 1)) which
	      (substring font (match-end 1)))
    nil))

(defun x-frob-font-slant (font which)
  (cond ((or (string-match x-font-regexp font)
	     (string-match x-font-regexp-head font))
	 (concat (substring font 0 (match-beginning 2)) which
		 (substring font (match-end 2))))
	((string-match x-font-regexp-slant font)
	 (concat (substring font 0 (match-beginning 1)) which
		 (substring font (match-end 1))))
	(t nil)))


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

;;; non-X-specific interface

(defun make-face-bold (face &optional frame noerror)
  "Make the font of the given face be bold, if possible.  
If NOERROR is non-nil, return nil on failure."
  (interactive (list (read-face-name "Make which face bold: ")))
  (if (and (eq frame t) (listp (face-font face t)))
      (set-face-font face (if (memq 'italic (face-font face t))
			      '(bold italic) '(bold))
		     t)
    (let ((ofont (face-font face frame))
	  font f2)
      (if (null frame)
	  (let ((frames (frame-list)))
	    ;; Make this face bold in global-face-data.
	    (make-face-bold face t noerror)
	    ;; Make this face bold in each frame.
	    (while frames
	      (make-face-bold face (car frames) noerror)
	      (setq frames (cdr frames))))
	(setq face (internal-get-face face frame))
	(setq font (or (face-font face frame)
		       (face-font face t)))
	(if (listp font)
	    (setq font nil))
	(setq font (or font
		       (face-font 'default frame)
		       (cdr (assq 'font (frame-parameters frame)))))
	(make-face-bold-internal face frame))
      (or (not (equal ofont (face-font face)))
	  (and (not noerror)
	       (error "No bold version of %S" font))))))

(defun make-face-bold-internal (face frame)
  (or (and (setq f2 (x-make-font-bold font))
	   (internal-try-face-font face f2 frame))
      (and (setq f2 (x-make-font-demibold font))
	   (internal-try-face-font face f2 frame))))

(defun make-face-italic (face &optional frame noerror)
  "Make the font of the given face be italic, if possible.  
If NOERROR is non-nil, return nil on failure."
  (interactive (list (read-face-name "Make which face italic: ")))
  (if (and (eq frame t) (listp (face-font face t)))
      (set-face-font face (if (memq 'bold (face-font face t))
			      '(bold italic) '(italic))
		     t)
    (let ((ofont (face-font face frame))
	  font f2)
      (if (null frame)
	  (let ((frames (frame-list)))
	    ;; Make this face italic in global-face-data.
	    (make-face-italic face t noerror)
	    ;; Make this face italic in each frame.
	    (while frames
	      (make-face-italic face (car frames) noerror)
	      (setq frames (cdr frames))))
	(setq face (internal-get-face face frame))
	(setq font (or (face-font face frame)
		       (face-font face t)))
	(if (listp font)
	    (setq font nil))
	(setq font (or font
		       (face-font 'default frame)
		       (cdr (assq 'font (frame-parameters frame)))))
	(make-face-italic-internal face frame))
      (or (not (equal ofont (face-font face)))
	  (and (not noerror)
	       (error "No italic version of %S" font))))))

(defun make-face-italic-internal (face frame)
  (or (and (setq f2 (x-make-font-italic font))
	   (internal-try-face-font face f2 frame))
      (and (setq f2 (x-make-font-oblique font))
	   (internal-try-face-font face f2 frame))))

(defun make-face-bold-italic (face &optional frame noerror)
  "Make the font of the given face be bold and italic, if possible.  
If NOERROR is non-nil, return nil on failure."
  (interactive (list (read-face-name "Make which face bold-italic: ")))
  (if (and (eq frame t) (listp (face-font face t)))
      (set-face-font face '(bold italic) t)
    (let ((ofont (face-font face frame))
	  font)
      (if (null frame)
	  (let ((frames (frame-list)))
	    ;; Make this face bold-italic in global-face-data.
	    (make-face-bold-italic face t noerror)
	    ;; Make this face bold in each frame.
	    (while frames
	      (make-face-bold-italic face (car frames) noerror)
	      (setq frames (cdr frames))))
	(setq face (internal-get-face face frame))
	(setq font (or (face-font face frame)
		       (face-font face t)))
	(if (listp font)
	    (setq font nil))
	(setq font (or font
		       (face-font 'default frame)
		       (cdr (assq 'font (frame-parameters frame)))))
	(make-face-bold-italic-internal face frame))
      (or (not (equal ofont (face-font face)))
	  (and (not noerror)
	       (error "No bold italic version of %S" font))))))

(defun make-face-bold-italic-internal (face frame)
  (let (f2 f3)
    (or (and (setq f2 (x-make-font-italic font))
	     (not (equal font f2))
	     (setq f3 (x-make-font-bold f2))
	     (not (equal f2 f3))
	     (internal-try-face-font face f3 frame))
	(and (setq f2 (x-make-font-oblique font))
	     (not (equal font f2))
	     (setq f3 (x-make-font-bold f2))
	     (not (equal f2 f3))
	     (internal-try-face-font face f3 frame))
	(and (setq f2 (x-make-font-italic font))
	     (not (equal font f2))
	     (setq f3 (x-make-font-demibold f2))
	     (not (equal f2 f3))
	     (internal-try-face-font face f3 frame))
	(and (setq f2 (x-make-font-oblique font))
	     (not (equal font f2))
	     (setq f3 (x-make-font-demibold f2))
	     (not (equal f2 f3))
	     (internal-try-face-font face f3 frame)))))

(defun make-face-unbold (face &optional frame noerror)
  "Make the font of the given face be non-bold, if possible.  
If NOERROR is non-nil, return nil on failure."
  (interactive (list (read-face-name "Make which face non-bold: ")))
  (if (and (eq frame t) (listp (face-font face t)))
      (set-face-font face (if (memq 'italic (face-font face t))
			      '(italic) nil)
		     t)
    (let ((ofont (face-font face frame))
	  font font1)
      (if (null frame)
	  (let ((frames (frame-list)))
	    ;; Make this face unbold in global-face-data.
	    (make-face-unbold face t noerror)
	    ;; Make this face unbold in each frame.
	    (while frames
	      (make-face-unbold face (car frames) noerror)
	      (setq frames (cdr frames))))
	(setq face (internal-get-face face frame))
	(setq font1 (or (face-font face frame)
			(face-font face t)))
	(if (listp font1)
	    (setq font1 nil))
	(setq font1 (or font1
			(face-font 'default frame)
			(cdr (assq 'font (frame-parameters frame)))))
	(setq font (x-make-font-unbold font1))
	(if font (internal-try-face-font face font frame)))
      (or (not (equal ofont (face-font face)))
	  (and (not noerror)
	       (error "No unbold version of %S" font1))))))

(defun make-face-unitalic (face &optional frame noerror)
  "Make the font of the given face be non-italic, if possible.  
If NOERROR is non-nil, return nil on failure."
  (interactive (list (read-face-name "Make which face non-italic: ")))
  (if (and (eq frame t) (listp (face-font face t)))
      (set-face-font face (if (memq 'bold (face-font face t))
			      '(bold) nil)
		     t)
    (let ((ofont (face-font face frame))
	  font font1)
      (if (null frame)
	  (let ((frames (frame-list)))
	    ;; Make this face unitalic in global-face-data.
	    (make-face-unitalic face t noerror)
	    ;; Make this face unitalic in each frame.
	    (while frames
	      (make-face-unitalic face (car frames) noerror)
	      (setq frames (cdr frames))))
	(setq face (internal-get-face face frame))
	(setq font1 (or (face-font face frame)
			(face-font face t)))
	(if (listp font1)
	    (setq font1 nil))
	(setq font1 (or font1
			(face-font 'default frame)
			(cdr (assq 'font (frame-parameters frame)))))
	(setq font (x-make-font-unitalic font1))
	(if font (internal-try-face-font face font frame)))
      (or (not (equal ofont (face-font face)))
	  (and (not noerror)
	       (error "No unitalic version of %S" font1))))))

(defvar list-faces-sample-text
  "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "*Text string to display as the sample text for `list-faces-display'.")

;; The name list-faces would be more consistent, but let's avoid a conflict
;; with Lucid, which uses that name differently.
(defun list-faces-display ()
  "List all faces, using the same sample text in each.
The sample text is a string that comes from the variable
`list-faces-sample-text'.

It is possible to give a particular face name different appearances in
different frames.  This command shows the appearance in the
selected frame."
  (interactive)
  (let ((faces (sort (face-list) (function string-lessp)))
	(face nil)
	(frame (selected-frame))
	disp-frame window)
    (with-output-to-temp-buffer "*Faces*"
      (save-excursion
	(set-buffer standard-output)
	(setq truncate-lines t)
	(while faces
	  (setq face (car faces))
	  (setq faces (cdr faces))
	  (insert (format "%25s " (symbol-name face)))
	  (let ((beg (point)))
	    (insert list-faces-sample-text)
	    (insert "\n")
	    (put-text-property beg (1- (point)) 'face face)))
	(goto-char (point-min))))
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

;;; Make the default and modeline faces; the C code knows these as
;;; faces 0 and 1, respectively, so they must be the first two faces
;;; made.
(defun face-initialize ()
  (make-face 'default)
  (make-face 'modeline)
  (make-face 'highlight)
  ;;
  ;; These aren't really special in any way, but they're nice to have around.
  ;; The X-specific code is clever at them.
  ;;
  (make-face 'bold)
  (make-face 'italic)
  (make-face 'bold-italic)
  (make-face 'region)
  (make-face 'secondary-selection)
  (make-face 'underline)

  (setq region-face (face-id 'region))

  ;; Set up the faces of all existing X Window frames.
  (let ((frames (frame-list)))
    (while frames
      (if (eq (framep (car frames)) 'x)
	  (x-initialize-frame-faces (car frames)))
      (setq frames (cdr frames)))))


;;; This really belongs in setting a frame's own font.
;;;     ;;
;;;     ;; No font specified in the resource database; try to cope.
;;;     ;;
;;;     (internal-try-face-font default "-*-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-*"
;;;			     frame)
;;;     (internal-try-face-font default "-*-courier-*-r-*-*-*-120-*-*-*-*-iso8859-*"
;;;			     frame)
;;;     (internal-try-face-font default "-*-*-medium-r-*-*-*-120-*-*-m-*-iso8859-*" frame)
;;;     (internal-try-face-font default "-*-*-medium-r-*-*-*-120-*-*-c-*-iso8859-*" frame)
;;;     (internal-try-face-font default "-*-*-*-r-*-*-*-120-*-*-m-*-iso8859-*" frame)
;;;     (internal-try-face-font default "-*-*-*-r-*-*-*-120-*-*-c-*-iso8859-*" frame)
;;;     (internal-try-face-font default "-*-*-*-r-*-*-*-120-*-*-*-*-iso8859-*" frame)


;;; This is called from make-screen-initial-faces to make sure that the
;;; "default" and "modeline" faces for this screen have enough attributes
;;; specified for emacs to be able to display anything on it.  This had
;;; better not signal an error.
;;;
(defun x-initialize-frame-faces (frame)
  (or (face-differs-from-default-p 'bold frame)
      (make-face-bold 'bold frame t)
      ;; if default font is bold, then make the `bold' face be unbold.
      (make-face-unbold 'bold frame t)
      ;; otherwise the luser specified one of the bogus font names
      (internal-x-complain-about-font 'bold frame)
      )

  (or (face-differs-from-default-p 'italic frame)
      (make-face-italic 'italic frame t)
      (progn
	(make-face-bold 'italic frame t)
	(internal-x-complain-about-font 'italic frame))
      )

  (or (face-differs-from-default-p 'bold-italic frame)
      (make-face-bold-italic 'bold-italic frame t)
      ;; if we couldn't get a bold-italic version, try just bold.
      (make-face-bold 'bold-italic frame t)
      ;; if we couldn't get bold or bold-italic, then that's probably because
      ;; the default font is bold, so make the `bold-italic' face be unbold.
      (and (make-face-unbold 'bold-italic frame t)
	   (make-face-italic 'bold-italic frame t))
      ;; if that didn't work, try italic (can this ever happen? what the hell.)
      (progn
	(make-face-italic 'bold-italic frame t)
	;; then bitch and moan.
	(internal-x-complain-about-font 'bold-italic frame))
      )

  (or (face-differs-from-default-p 'highlight frame)
      (if (or (not (x-display-color-p))
	      (= (x-display-planes) 1))
	  (invert-face 'highlight frame)
	(condition-case ()
	    (condition-case ()
		(set-face-background 'highlight "darkseagreen2" frame)
	      (error (set-face-background 'highlight "green" frame)))
;;;	    (set-face-background-pixmap 'highlight "gray1" frame)
	  (error (invert-face 'highlight frame)))))

  (or (face-differs-from-default-p 'region frame)
      (if (= (x-display-planes) 1)
	  (invert-face 'region frame)
	(condition-case ()
	    (set-face-background 'region "gray" frame)
	  (error (invert-face 'region frame)))))

  (or (face-differs-from-default-p 'modeline frame)
      (invert-face 'modeline frame))

  (or (face-differs-from-default-p 'underline frame)
      (set-face-underline-p 'underline t frame))

  (or (face-differs-from-default-p 'secondary-selection frame)
      (if (or (not (x-display-color-p))
	      (= (x-display-planes) 1))
	  (invert-face 'secondary-selection frame)
	(condition-case ()
	    (condition-case ()
		;; some older X servers don't have this one.
		(set-face-background 'secondary-selection "paleturquoise"
				     frame)
	      (error
	       (set-face-background 'secondary-selection "green" frame)))
;;;	    (set-face-background-pixmap 'secondary-selection "gray1" frame)
	  (error (invert-face 'secondary-selection frame)))))
  )

(defun internal-x-complain-about-font (face frame)
;;; It's annoying to bother the user about this,
;;; since it happens under normal circumstances.
;;;  (message "No %s version of %S"
;;;	   face
;;;	   (or (face-font face frame)
;;;	       (face-font face t)
;;;	       (face-font 'default frame)
;;;	       (cdr (assq 'font (frame-parameters frame)))))
;;;  (sit-for 1)
  )

;; Like x-create-frame but also set up the faces.

(defun x-create-frame-with-faces (&optional parameters)
  (if (null global-face-data)
      (x-create-frame parameters)
    (let* ((frame (x-create-frame parameters))
	   (faces (copy-alist global-face-data))
	   (rest faces))
      (set-frame-face-alist frame faces)

      (if (cdr (or (assq 'reverse parameters)
		   (assq 'reverse default-frame-alist)
		   (cons nil
			 (member (x-get-resource "reverseVideo" "ReverseVideo")
				 '("on" "true")))))
	  (let ((params (frame-parameters frame)))
	    (modify-frame-parameters
	     frame
	     (list (cons 'foreground-color (cdr (assq 'background-color params)))
		   (cons 'background-color (cdr (assq 'foreground-color params)))
		   (cons 'mouse-color (cdr (assq 'background-color params)))
		   (cons 'cursor-color (cdr (assq 'background-color params)))
		   (cons 'border-color (cdr (assq 'background-color params)))))))

      ;; Copy the vectors that represent the faces.
      ;; Also fill them in from X resources.
      (while rest
	(setcdr (car rest) (copy-sequence (cdr (car rest))))
	(condition-case nil
	    (if (listp (face-font (cdr (car rest))))
		(let ((bold (memq 'bold (face-font (cdr (car rest)))))
		      (italic (memq 'italic (face-font (cdr (car rest))))))
		  (if (and bold italic)
		      (make-face-bold-italic (car (car rest)) frame)
		    (if bold
			(make-face-bold (car (car rest)) frame)
		      (if italic
			  (make-face-italic (car (car rest)) frame))))))
	  (error nil))
	(make-face-x-resource-internal (cdr (car rest)) frame t)
	(setq rest (cdr rest)))

      (x-initialize-frame-faces frame)

      frame)))

;; If we are already using x-window frames, initialize faces for them.
(if (eq (framep (selected-frame)) 'x)
    (face-initialize))

(provide 'faces)

;;; faces.el ends here
