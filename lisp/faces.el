;;; faces.el --- Lisp interface to the c "face" structure

;; Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.

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

(eval-when-compile
 ;; These used to be defsubsts, now they're subrs.  Avoid losing if we're
 ;; being compiled with an old Emacs that still has defsubrs in it.
 (put 'face-name 'byte-optimizer nil)
 (put 'face-id 'byte-optimizer nil)
 (put 'face-font 'byte-optimizer nil)
 (put 'face-foreground 'byte-optimizer nil)
 (put 'face-background 'byte-optimizer nil)
 (put 'face-stipple 'byte-optimizer nil)
 (put 'face-underline-p 'byte-optimizer nil)
 (put 'set-face-font 'byte-optimizer nil)
 (put 'set-face-foreground 'byte-optimizer nil)
 (put 'set-face-background 'byte-optimizer nil)
 (put 'set-face-stipple 'byte-optimizer nil)
 (put 'set-face-underline-p 'byte-optimizer nil))

;;;; Functions for manipulating face vectors.

;;; A face vector is a vector of the form:
;;;    [face NAME ID FONT FOREGROUND BACKGROUND STIPPLE UNDERLINE]

;;; Type checkers.
(defsubst internal-facep (x)
  (and (vectorp x) (= (length x) 8) (eq (aref x 0) 'face)))

(defun facep (x)
  "Return t if X is a face name or an internal face vector."
  (and (or (internal-facep x)
	   (and (symbolp x) (assq x global-face-data)))
       t))
      
(defmacro internal-check-face (face)
  (` (or (internal-facep (, face))
	 (signal 'wrong-type-argument (list 'internal-facep (, face))))))

;;; Accessors.
(defun face-name (face)
  "Return the name of face FACE."
  (aref (internal-get-face face) 1))

(defun face-id (face)
  "Return the internal ID number of face FACE."
  (aref (internal-get-face face) 2))

(defun face-font (face &optional frame)
  "Return the font name of face FACE, or nil if it is unspecified.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
  The font default for a face is either nil, or a list
  of the form (bold), (italic) or (bold italic).
If FRAME is omitted or nil, use the selected frame."
  (aref (internal-get-face face frame) 3))

(defun face-foreground (face &optional frame)
  "Return the foreground color name of face FACE, or nil if unspecified.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
  (aref (internal-get-face face frame) 4))

(defun face-background (face &optional frame)
  "Return the background color name of face FACE, or nil if unspecified.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
  (aref (internal-get-face face frame) 5))

(defun face-stipple (face &optional frame)
 "Return the stipple pixmap name of face FACE, or nil if unspecified.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
 (aref (internal-get-face face frame) 6))

(defalias 'face-background-pixmap 'face-stipple)

(defun face-underline-p (face &optional frame)
 "Return t if face FACE is underlined.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
 (aref (internal-get-face face frame) 7))


;;; Mutators.

(defun set-face-font (face font &optional frame)
  "Change the font of face FACE to FONT (a string).
If the optional FRAME argument is provided, change only
in that frame; otherwise change each frame."
  (interactive (internal-face-interactive "font"))
  (if (stringp font) (setq font (x-resolve-font-name font 'default frame)))
  (internal-set-face-1 face 'font font 3 frame))

(defun set-face-foreground (face color &optional frame)
  "Change the foreground color of face FACE to COLOR (a string).
If the optional FRAME argument is provided, change only
in that frame; otherwise change each frame."
  (interactive (internal-face-interactive "foreground"))
  (internal-set-face-1 face 'foreground color 4 frame))

(defun set-face-background (face color &optional frame)
  "Change the background color of face FACE to COLOR (a string).
If the optional FRAME argument is provided, change only
in that frame; otherwise change each frame."
  (interactive (internal-face-interactive "background"))
  ;; For a specific frame, use gray stipple instead of gray color
  ;; if the display does not support a gray color.
  (if (and frame (not (eq frame t))
	   (member color '("gray" "gray1" "gray3"))
	   (not (x-display-color-p frame))
	   (not (x-display-grayscale-p frame)))
      (set-face-stipple face color frame)
    (if (null frame)
	(let ((frames (frame-list)))
	  (while frames
	    (set-face-background (face-name face) color (car frames))
	    (setq frames (cdr frames)))
	  (set-face-background face color t)
	  color)
      (internal-set-face-1 face 'background color 5 frame))))

(defun set-face-stipple (face name &optional frame)
  "Change the stipple pixmap of face FACE to PIXMAP.
PIXMAP should be a string, the name of a file of pixmap data.
The directories listed in the `x-bitmap-file-path' variable are searched.

Alternatively, PIXMAP may be a list of the form (WIDTH HEIGHT DATA)
where WIDTH and HEIGHT are the size in pixels,
and DATA is a string, containing the raw bits of the bitmap.  

If the optional FRAME argument is provided, change only
in that frame; otherwise change each frame."
  (interactive (internal-face-interactive "stipple"))
  (internal-set-face-1 face 'background-pixmap name 6 frame))

(defalias 'set-face-background-pixmap 'set-face-stipple)

(defun set-face-underline-p (face underline-p &optional frame)
  "Specify whether face FACE is underlined.  (Yes if UNDERLINE-P is non-nil.)
If the optional FRAME argument is provided, change only
in that frame; otherwise change each frame."
  (interactive (internal-face-interactive "underline-p" "underlined"))
  (internal-set-face-1 face 'underline underline-p 7 frame))

(defun modify-face-read-string (face default name alist)
  (let ((value
	 (completing-read
	  (if default
	      (format "Set face %s %s (default %s): "
		      face name (downcase default))
	    (format "Set face %s %s: " face name))
	  alist)))
    (cond ((equal value "none")
	   nil)
	  ((equal value "")
	   default)
	  (t value))))

(defun modify-face (face foreground background stipple
			 bold-p italic-p underline-p)
  "Change the display attributes for face FACE.
FOREGROUND and BACKGROUND should be color strings or nil.
STIPPLE should be a stipple pattern name or nil.
BOLD-P, ITALIC-P, and UNDERLINE-P specify whether the face should be set bold,
in italic, and underlined, respectively.  (Yes if non-nil.)
If called interactively, prompts for a face and face attributes."
  (interactive
   (let* ((completion-ignore-case t)
	  (face	       (symbol-name (read-face-name "Modify face: ")))
	  (colors      (mapcar 'list x-colors))
	  (stipples    (mapcar 'list
			       (apply 'nconc
				      (mapcar 'directory-files
					      x-bitmap-file-path))))
	  (foreground  (modify-face-read-string
			face (face-foreground (intern face))
			"foreground" colors))
	  (background  (modify-face-read-string
			face (face-background (intern face))
			"background" colors))
	  (stipple     (modify-face-read-string
			face (face-stipple (intern face))
			"stipple" stipples))
	  (bold-p      (y-or-n-p (concat "Set face " face " bold ")))
	  (italic-p    (y-or-n-p (concat "Set face " face " italic ")))
	  (underline-p (y-or-n-p (concat "Set face " face " underline "))))
     (message "Face %s: %s" face
      (mapconcat 'identity
       (delq nil
	(list (and foreground (concat (downcase foreground) " foreground"))
	      (and background (concat (downcase background) " background"))
	      (and stipple (concat (downcase stipple) " stipple"))
	      (and bold-p "bold") (and italic-p "italic")
	      (and underline-p "underline"))) ", "))
     (list (intern face) foreground background stipple
	   bold-p italic-p underline-p)))
  (condition-case nil (set-face-foreground face foreground) (error nil))
  (condition-case nil (set-face-background face background) (error nil))
  (condition-case nil (set-face-stipple face stipple) (error nil))
  (funcall (if bold-p 'make-face-bold 'make-face-unbold) face nil t)
  (funcall (if italic-p 'make-face-italic 'make-face-unitalic) face nil t)
  (set-face-underline-p face underline-p)
  (and (interactive-p) (redraw-display)))

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
	;; add to menu
	(if (fboundp 'facemenu-add-new-face)
	    (facemenu-add-new-face name))
	face))
  name)

;; Fill in a face by default based on X resources, for all existing frames.
;; This has to be done when a new face is made.
(defun make-face-x-resource-internal (face &optional frame set-anyway)
  (cond ((null frame)
	 (let ((frames (frame-list)))
	   (while frames
	     (if (eq (framep (car frames)) 'x)
		 (make-face-x-resource-internal (face-name face)
						(car frames) set-anyway))
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
		(bgp (or (x-get-resource (concat name ".attributeStipple")
					 "Face.AttributeStipple")
			 (x-get-resource (concat name ".attributeBackgroundPixmap")
					 "Face.AttributeBackgroundPixmap")
			 (and set-anyway (face-stipple face))))
		(ulp (let ((resource (x-get-resource
				      (concat name ".attributeUnderline")
				      "Face.AttributeUnderline")))
		       (if resource
			   (member (downcase resource) '("on" "true"))
			 (and set-anyway (face-underline-p face)))))
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
	   (if bgp
	       (condition-case ()
		   (set-face-stipple face bgp frame)
		 (error (message "pixmap `%s' not found for face `%s'" bgp name))))
	   (if (or ulp set-anyway)
	       (set-face-underline-p face ulp frame))
	   )))
  face)

(defun copy-face (old-face new-face &optional frame new-frame)
  "Define a face just like OLD-FACE, with name NEW-FACE.
If NEW-FACE already exists as a face, it is modified to be like OLD-FACE.
If it doesn't already exist, it is created.

If the optional argument FRAME is given as a frame,
NEW-FACE is changed on FRAME only.
If FRAME is t, the frame-independent default specification for OLD-FACE
is copied to NEW-FACE.
If FRAME is nil, copying is done for the frame-independent defaults
and for each existing frame.
If the optional fourth argument NEW-FRAME is given, 
copy the information from face OLD-FACE on frame FRAME
to NEW-FACE on frame NEW-FRAME."
  (or new-frame (setq new-frame frame))
  (let ((inhibit-quit t))
    (if (null frame)
	(let ((frames (frame-list)))
	  (while frames
	    (copy-face old-face new-face (car frames))
	    (setq frames (cdr frames)))
	  (copy-face old-face new-face t))
      (setq old-face (internal-get-face old-face frame))
      (setq new-face (or (internal-find-face new-face new-frame)
			 (make-face new-face)))
      (condition-case nil
	  ;; A face that has a global symbolic font modifier such as `bold'
	  ;; might legitimately get an error here.
	  ;; Use the frame's default font in that case.
	  (set-face-font new-face (face-font old-face frame) new-frame)
	(error
	 (set-face-font new-face nil new-frame)))
      (set-face-foreground new-face (face-foreground old-face frame) new-frame)
      (set-face-background new-face (face-background old-face frame) new-frame)
      (set-face-stipple new-face
			(face-stipple old-face frame)
			new-frame)
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
       (eq (face-underline-p face1 frame) (face-underline-p face2 frame))
       (equal (face-stipple face1 frame)
	      (face-stipple face2 frame))))

(defun face-differs-from-default-p (face &optional frame)
  "True if face FACE displays differently from the default face, on FRAME.
A face is considered to be ``the same'' as the default face if it is 
actually specified in the same way (equivalent fonts, etc) or if it is 
fully unspecified, and thus inherits the attributes of any face it 
is displayed on top of.

The optional argument FRAME specifies which frame to test;
if FRAME is t, test the default for new frames.
If FRAME is nil or omitted, test the selected frame."
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
	      (or (equal (face-stipple default frame)
			 (face-stipple face frame))
		  (null (face-stipple face frame)))
	      (equal (face-underline-p default frame)
		     (face-underline-p face frame))
	      ))))

(defun face-nontrivial-p (face &optional frame)
  "True if face FACE has some non-nil attribute.
The optional argument FRAME specifies which frame to test;
if FRAME is t, test the default for new frames.
If FRAME is nil or omitted, test the selected frame."
  (setq face (internal-get-face face frame))
  (or (face-foreground face frame)
      (face-background face frame)
      (face-font face frame)
      (face-stipple face frame)
      (face-underline-p face frame)))


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
Given optional arguments FACE and FRAME, return a font which is
also the same size as FACE on FRAME, or fail."
  (or (symbolp face)
      (setq face (face-name face)))
  (and (eq frame t)
       (setq frame nil))
  (if pattern
      ;; Note that x-list-fonts has code to handle a face with nil as its font.
      (let ((fonts (x-list-fonts pattern face frame)))
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
	  font)
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
	(and font (make-face-bold-internal face frame font)))
      (or (not (equal ofont (face-font face)))
	  (and (not noerror)
	       (error "No bold version of %S" font))))))

(defun make-face-bold-internal (face frame font)
  (let (f2)
    (or (and (setq f2 (x-make-font-bold font))
	     (internal-try-face-font face f2 frame))
	(and (setq f2 (x-make-font-demibold font))
	     (internal-try-face-font face f2 frame)))))

(defun make-face-italic (face &optional frame noerror)
  "Make the font of the given face be italic, if possible.  
If NOERROR is non-nil, return nil on failure."
  (interactive (list (read-face-name "Make which face italic: ")))
  (if (and (eq frame t) (listp (face-font face t)))
      (set-face-font face (if (memq 'bold (face-font face t))
			      '(bold italic) '(italic))
		     t)
    (let ((ofont (face-font face frame))
	  font)
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
	(and font (make-face-italic-internal face frame font)))
      (or (not (equal ofont (face-font face)))
	  (and (not noerror)
	       (error "No italic version of %S" font))))))

(defun make-face-italic-internal (face frame font)
  (let (f2)
    (or (and (setq f2 (x-make-font-italic font))
	     (internal-try-face-font face f2 frame))
	(and (setq f2 (x-make-font-oblique font))
	     (internal-try-face-font face f2 frame)))))

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
	(and font (make-face-bold-italic-internal face frame font)))
      (or (not (equal ofont (face-font face)))
	  (and (not noerror)
	       (error "No bold italic version of %S" font))))))

(defun make-face-bold-italic-internal (face frame font)
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
	(setq font (and font1 (x-make-font-unbold font1)))
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
	(setq font (and font1 (x-make-font-unitalic font1)))
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
	    (put-text-property beg (1- (point)) 'face face)
	    ;; If the sample text has multiple lines, line up all of them.
	    (goto-char beg)
	    (forward-line 1)
	    (while (not (eobp))
	      (insert "                          ")
	      (forward-line 1))))
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

;;; Make the standard faces.
;;; The C code knows the default and modeline faces as faces 0 and 1,
;;; so they must be the first two faces made.
(defun face-initialize ()
  (make-face 'default)
  (make-face 'modeline)
  (make-face 'highlight)

  ;; These aren't really special in any way, but they're nice to have around.

  (make-face 'bold)
  (make-face 'italic)
  (make-face 'bold-italic)
  (make-face 'region)
  (make-face 'secondary-selection)
  (make-face 'underline)

  (setq region-face (face-id 'region))

  ;; Specify the global properties of these faces
  ;; so they will come out right on new frames.

  (make-face-bold 'bold t)
  (make-face-italic 'italic t)
  (make-face-bold-italic 'bold-italic t)

  (set-face-background 'highlight '("darkseagreen2" "green" t) t)
  (set-face-background 'region '("gray" underline) t)
  (set-face-background 'secondary-selection '("paleturquoise" "green" t) t)
  (set-face-background 'modeline '(t) t)
  (set-face-underline-p 'underline t t)

  ;; Set up the faces of all existing X Window frames
  ;; from those global properties, unless already set in a given frame.

  (let ((frames (frame-list)))
    (while frames
      (if (not (memq (framep (car frames)) '(t nil)))
	  (let ((frame (car frames))
		(rest global-face-data))
	    (while rest
	      (let ((face (car (car rest))))
		(or (face-differs-from-default-p face)
		    (face-fill-in face (cdr (car rest)) frame)))
	      (setq rest (cdr rest)))))
      (setq frames (cdr frames)))))


;; Like x-create-frame but also set up the faces.

(defun x-create-frame-with-faces (&optional parameters)
  ;; Read this frame's geometry resource, if it has an explicit name,
  ;; and put the specs into PARAMETERS.
  (let* ((name (or (cdr (assq 'name parameters))
		   (cdr (assq 'name default-frame-alist))
		   (cdr (assq 'name initial-frame-alist))))
	 (x-resource-name name)
	 (res-geometry (if name (x-get-resource "geometry" "Geometry")))
	 parsed)
    (if res-geometry
	(progn
	  (setq parsed (x-parse-geometry res-geometry))
	  ;; If the resource specifies a position,
	  ;; call the position and size "user-specified".
	  (if (or (assq 'top parsed) (assq 'left parsed))
	      (setq parsed (cons '(user-position . t)
				 (cons '(user-size . t) parsed))))
	  ;; All geometry parms apply to the initial frame.
	  (setq parameters (append parameters parsed)))))
  (if (null global-face-data)
      (x-create-frame parameters)
    (let* ((visibility-spec (assq 'visibility parameters))
	   (frame (x-create-frame (cons '(visibility . nil) parameters)))
	   (faces (copy-alist global-face-data))
	   success
	   (rest faces))
      (unwind-protect
	  (progn
	    (set-frame-face-alist frame faces)

	    (if (cdr (or (assq 'reverse parameters)
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
					       (list (cons 'cursor-color fg))))))
	    ;; Copy the vectors that represent the faces.
	    ;; Also fill them in from X resources.
	    (while rest
	      (let ((global (cdr (car rest))))
		(setcdr (car rest) (vector 'face
					   (face-name (cdr (car rest)))
					   (face-id (cdr (car rest)))
					   nil nil nil nil nil))
		(face-fill-in (car (car rest)) global frame))
	      (make-face-x-resource-internal (cdr (car rest)) frame t)
	      (setq rest (cdr rest)))
	    (if (null visibility-spec)
		(make-frame-visible frame)
	      (modify-frame-parameters frame (list visibility-spec)))
	    (setq success t)
	    frame)
	(or success
	    (delete-frame frame))))))

;; Update a frame's faces when we change its default font.
(defun frame-update-faces (frame)
  (let* ((faces global-face-data)
	 (rest faces))
    (while rest
      (let* ((face (car (car rest)))
	     (font (face-font face t)))
	(if (listp font)
	    (let ((bold (memq 'bold font))
		  (italic (memq 'italic font)))
	      ;; Ignore any previous (string-valued) font, it might not even
	      ;; be the right size anymore.
	      (set-face-font face nil frame)
	      (cond ((and bold italic)
		     (make-face-bold-italic face frame t))
		    (bold
		     (make-face-bold face frame t))
		    (italic
		     (make-face-italic face frame t)))))
      (setq rest (cdr rest)))
    frame)))

;; Update the colors of FACE, after FRAME's own colors have been changed.
;; This applies only to faces with global color specifications
;; that are not simple constants.
(defun frame-update-face-colors (frame)
  (let ((faces global-face-data))
    (while faces
      (condition-case nil
	  (let* ((data (cdr (car faces)))
		 (face (car (car faces)))
		 (foreground (face-foreground data))
		 (background (face-background data)))
	    ;; If the global spec is a specific color,
	    ;; which doesn't depend on the frame's attributes,
	    ;; we don't need to recalculate it now.
	    (or (listp foreground)
		(setq foreground nil))
	    (or (listp background)
		(setq background nil))
	    ;; If we are going to frob this face at all,
	    ;; reinitialize it first.
	    (if (or foreground background)
		(progn (set-face-foreground face nil frame)
		       (set-face-background face nil frame)))
	    (if foreground
		(face-try-color-list 'set-face-foreground
				     face foreground frame))
	    (if background
		(face-try-color-list 'set-face-background
				     face background frame)))
	(error nil))
      (setq faces (cdr faces)))))

;; Fill in the face FACE from frame-independent face data DATA.
;; DATA should be the non-frame-specific ("global") face vector
;; for the face.  FACE should be a face name or face object.
;; FRAME is the frame to act on; it must be an actual frame, not nil or t.
(defun face-fill-in (face data frame)
  (condition-case nil
      (let ((foreground (face-foreground data))
	    (background (face-background data))
	    (font (face-font data))
	    (stipple (face-stipple data)))
	(set-face-underline-p face (face-underline-p data) frame)
	(if foreground
	    (face-try-color-list 'set-face-foreground
				 face foreground frame))
	(if background
	    (face-try-color-list 'set-face-background
				 face background frame))
	(if (listp font)
	    (let ((bold (memq 'bold font))
		  (italic (memq 'italic font)))
	      (cond ((and bold italic)
		     (make-face-bold-italic face frame))
		    (bold
		     (make-face-bold face frame))
		    (italic
		     (make-face-italic face frame))))
	  (if font
	      (set-face-font face font frame)))
	(if stipple
	    (set-face-stipple face stipple frame)))
    (error nil)))

;; Assuming COLOR is a valid color name,
;; return t if it can be displayed on FRAME.
(defun face-color-supported-p (frame color background-p)
  (or (x-display-color-p frame)
      ;; A black-and-white display can implement these.
      (member color '("black" "white"))
      ;; A black-and-white display can fake these for background.
      (and background-p
	   (member color '("gray" "gray1" "gray3")))
      ;; A grayscale display can implement colors that are gray (more or less).
      (and (x-display-grayscale-p frame)
	   (let* ((values (x-color-values color frame))
		  (r (nth 0 values))
		  (g (nth 1 values))
		  (b (nth 2 values)))
	     (and (< (abs (- r g)) (/ (abs (+ r g)) 20))
		  (< (abs (- g b)) (/ (abs (+ g b)) 20))
		  (< (abs (- b r)) (/ (abs (+ b r)) 20)))))))

;; Use FUNCTION to store a color in FACE on FRAME.
;; COLORS is either a single color or a list of colors.
;; If it is a list, try the colors one by one until one of them
;; succeeds.  We signal an error only if all the colors failed.
;; t as COLORS or as an element of COLORS means to invert the face.
;; That can't fail, so any subsequent elements after the t are ignored.
(defun face-try-color-list (function face colors frame)
  (if (stringp colors)
      (if (face-color-supported-p frame colors
				  (eq function 'set-face-background))
	  (funcall function face colors frame))
    (if (eq colors t)
	(invert-face face frame)
      (let (done)
	(while (and colors (not done))
	  (if (or (memq (car colors) '(t underline))
		  (face-color-supported-p frame (car colors)
					  (eq function 'set-face-background)))
	      (if (cdr colors)
		  ;; If there are more colors to try, catch errors
		  ;; and set `done' if we succeed.
		  (condition-case nil
		      (progn
			(cond ((eq (car colors) t)
			       (invert-face face frame))
			      ((eq (car colors) 'underline)
			       (set-face-underline-p face t frame))
			      (t
			       (funcall function face (car colors) frame)))
			(setq done t))
		    (error nil))
		;; If this is the last color, let the error get out if it fails.
		;; If it succeeds, we will exit anyway after this iteration.
		(cond ((eq (car colors) t)
		       (invert-face face frame))
		      ((eq (car colors) 'underline)
		       (set-face-underline-p face t frame))
		      (t
		       (funcall function face (car colors) frame)))))
	  (setq colors (cdr colors)))))))

;; If we are already using x-window frames, initialize faces for them.
(if (eq (framep (selected-frame)) 'x)
    (face-initialize))

(provide 'faces)

;;; faces.el ends here
