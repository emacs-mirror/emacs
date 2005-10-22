;;; image.el --- image API

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003,
;;   2004, 2005 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: multimedia

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


(defgroup image ()
  "Image support."
  :group 'multimedia)


(defconst image-type-header-regexps
  '(("\\`/[\t\n\r ]*\\*.*XPM.\\*/" . xpm)
    ("\\`P[1-6]" . pbm)
    ("\\`GIF8" . gif)
    ("\\`\211PNG\r\n" . png)
    ("\\`[\t\n\r ]*#define" . xbm)
    ("\\`\\(MM\0\\*\\|II\\*\0\\)" . tiff)
    ("\\`[\t\n\r ]*%!PS" . postscript)
    ("\\`\xff\xd8" . (image-jpeg-p . jpeg)))
  "Alist of (REGEXP . IMAGE-TYPE) pairs used to auto-detect image types.
When the first bytes of an image file match REGEXP, it is assumed to
be of image type IMAGE-TYPE if IMAGE-TYPE is a symbol.  If not a symbol,
IMAGE-TYPE must be a pair (PREDICATE . TYPE).  PREDICATE is called
with one argument, a string containing the image data.  If PREDICATE returns
a non-nil value, TYPE is the image's type.")

(defconst image-type-file-name-regexps
  '(("\\.png\\'" . png)
    ("\\.gif\\'" . gif)
    ("\\.jpe?g\\'" . jpeg)
    ("\\.bmp\\'" . bmp)
    ("\\.xpm\\'" . xpm)
    ("\\.pbm\\'" . pbm)
    ("\\.xbm\\'" . xbm)
    ("\\.ps\\'" . postscript)
    ("\\.tiff?\\'" . tiff))
  "Alist of (REGEXP . IMAGE-TYPE) pairs used to identify image files.
When the name of an image file match REGEXP, it is assumed to
be of image type IMAGE-TYPE.")


(defvar image-load-path nil
  "List of locations in which to search for image files.
If an element is a string, it defines a directory to search.
If an element is a variable symbol whose value is a string, that
value defines a directory to search.
If an element is a variable symbol whose value is a list, the
value is used as a list of directories to search.")

(eval-at-startup
 (setq image-load-path
       (list (file-name-as-directory (expand-file-name "images" data-directory))
	     'data-directory 'load-path)))

(defun image-jpeg-p (data)
  "Value is non-nil if DATA, a string, consists of JFIF image data.
We accept the tag Exif because that is the same format."
  (when (string-match "\\`\xff\xd8" data)
    (catch 'jfif
      (let ((len (length data)) (i 2))
	(while (< i len)
	  (when (/= (aref data i) #xff)
	    (throw 'jfif nil))
	  (setq i (1+ i))
	  (when (>= (+ i 2) len)
	    (throw 'jfif nil))
	  (let ((nbytes (+ (lsh (aref data (+ i 1)) 8)
			   (aref data (+ i 2))))
		(code (aref data i)))
	    (when (and (>= code #xe0) (<= code #xef))
	      ;; APP0 LEN1 LEN2 "JFIF\0"
	      (throw 'jfif
		     (string-match "JFIF\\|Exif"
				   (substring data i (min (+ i nbytes) len)))))
	    (setq i (+ i 1 nbytes))))))))


;;;###autoload
(defun image-type-from-data (data)
  "Determine the image type from image data DATA.
Value is a symbol specifying the image type or nil if type cannot
be determined."
  (let ((types image-type-header-regexps)
	type)
    (while types
      (let ((regexp (car (car types)))
	    (image-type (cdr (car types))))
	(if (or (and (symbolp image-type)
		     (string-match regexp data))
		(and (consp image-type)
		     (funcall (car image-type) data)
		     (setq image-type (cdr image-type))))
	    (setq type image-type
		  types nil)
	  (setq types (cdr types)))))
    type))


;;;###autoload
(defun image-type-from-buffer ()
  "Determine the image type from data in the current buffer.
Value is a symbol specifying the image type or nil if type cannot
be determined."
  (let ((types image-type-header-regexps)
	type
	(opoint (point)))
    (goto-char (point-min))
    (while types
      (let ((regexp (car (car types)))
	    (image-type (cdr (car types)))
	    data)
	(if (or (and (symbolp image-type)
		     (looking-at regexp))
		(and (consp image-type)
		     (funcall (car image-type)
			      (or data
				  (setq data
					(buffer-substring
					 (point-min)
					 (min (point-max)
					      (+ (point-min) 256))))))
		     (setq image-type (cdr image-type))))
	    (setq type image-type
		  types nil)
	  (setq types (cdr types)))))
    (goto-char opoint)
    type))


;;;###autoload
(defun image-type-from-file-header (file)
  "Determine the type of image file FILE from its first few bytes.
Value is a symbol specifying the image type, or nil if type cannot
be determined."
  (unless (or (file-readable-p file)
	      (file-name-absolute-p file))
    (setq file (image-search-load-path file)))
  (and file
       (file-readable-p file)
       (with-temp-buffer
	 (set-buffer-multibyte nil)
	 (insert-file-contents-literally file nil 0 256)
	 (image-type-from-buffer))))


;;;###autoload
(defun image-type-from-file-name (file)
  "Determine the type of image file FILE from its name.
Value is a symbol specifying the image type, or nil if type cannot
be determined."
  (let ((types image-type-file-name-regexps)
	type)
    (while types
      (if (string-match (car (car types)) file)
	  (setq type (cdr (car types))
		types nil)
	(setq types (cdr types))))
    type))


;;;###autoload
(defun image-type-available-p (type)
  "Return non-nil if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'."
  (and (fboundp 'init-image-library)
       (init-image-library type image-library-alist)))


;;;###autoload
(defun create-image (file-or-data &optional type data-p &rest props)
  "Create an image.
FILE-OR-DATA is an image file name or image data.
Optional TYPE is a symbol describing the image type.  If TYPE is omitted
or nil, try to determine the image type from its first few bytes
of image data.  If that doesn't work, and FILE-OR-DATA is a file name,
use its file extension as image type.
Optional DATA-P non-nil means FILE-OR-DATA is a string containing image data.
Optional PROPS are additional image attributes to assign to the image,
like, e.g. `:mask MASK'.
Value is the image created, or nil if images of type TYPE are not supported.

Images should not be larger than specified by `max-image-size'."
  (when (and (not data-p) (not (stringp file-or-data)))
    (error "Invalid image file name `%s'" file-or-data))
  (cond ((null data-p)
	 ;; FILE-OR-DATA is a file name.
	 (unless (or type
		     (setq type (image-type-from-file-header file-or-data)))
	   (let ((extension (file-name-extension file-or-data)))
	     (unless extension
	       (error "Cannot determine image type"))
	     (setq type (intern extension)))))
	(t
	 ;; FILE-OR-DATA contains image data.
	 (unless type
	   (setq type (image-type-from-data file-or-data)))))
  (unless type
    (error "Cannot determine image type"))
  (unless (symbolp type)
    (error "Invalid image type `%s'" type))
  (when (image-type-available-p type)
    (append (list 'image :type type (if data-p :data :file) file-or-data)
	    props)))


;;;###autoload
(defun put-image (image pos &optional string area)
  "Put image IMAGE in front of POS in the current buffer.
IMAGE must be an image created with `create-image' or `defimage'.
IMAGE is displayed by putting an overlay into the current buffer with a
`before-string' STRING that has a `display' property whose value is the
image.  STRING is defaulted if you omit it.
POS may be an integer or marker.
AREA is where to display the image.  AREA nil or omitted means
display it in the text area, a value of `left-margin' means
display it in the left marginal area, a value of `right-margin'
means display it in the right marginal area."
  (unless string (setq string "x"))
  (let ((buffer (current-buffer)))
    (unless (eq (car-safe image) 'image)
      (error "Not an image: %s" image))
    (unless (or (null area) (memq area '(left-margin right-margin)))
      (error "Invalid area %s" area))
    (setq string (copy-sequence string))
    (let ((overlay (make-overlay pos pos buffer))
	  (prop (if (null area) image (list (list 'margin area) image))))
      (put-text-property 0 (length string) 'display prop string)
      (overlay-put overlay 'put-image t)
      (overlay-put overlay 'before-string string))))


;;;###autoload
(defun insert-image (image &optional string area slice)
  "Insert IMAGE into current buffer at point.
IMAGE is displayed by inserting STRING into the current buffer
with a `display' property whose value is the image.  STRING is
defaulted if you omit it.
AREA is where to display the image.  AREA nil or omitted means
display it in the text area, a value of `left-margin' means
display it in the left marginal area, a value of `right-margin'
means display it in the right marginal area.
SLICE specifies slice of IMAGE to insert.  SLICE nil or omitted
means insert whole image.  SLICE is a list (X Y WIDTH HEIGHT)
specifying the X and Y positions and WIDTH and HEIGHT of image area
to insert.  A float value 0.0 - 1.0 means relative to the width or
height of the image; integer values are taken as pixel values."
  ;; Use a space as least likely to cause trouble when it's a hidden
  ;; character in the buffer.
  (unless string (setq string " "))
  (unless (eq (car-safe image) 'image)
    (error "Not an image: %s" image))
  (unless (or (null area) (memq area '(left-margin right-margin)))
    (error "Invalid area %s" area))
  (if area
      (setq image (list (list 'margin area) image))
    ;; Cons up a new spec equal but not eq to `image' so that
    ;; inserting it twice in a row (adjacently) displays two copies of
    ;; the image.  Don't try to avoid this by looking at the display
    ;; properties on either side so that we DTRT more often with
    ;; cut-and-paste.  (Yanking killed image text next to another copy
    ;; of it loses anyway.)
    (setq image (cons 'image (cdr image))))
  (let ((start (point)))
    (insert string)
    (add-text-properties start (point)
			 `(display ,(if slice
					(list (cons 'slice slice) image)
				      image) rear-nonsticky (display)))))


;;;###autoload
(defun insert-sliced-image (image &optional string area rows cols)
  "Insert IMAGE into current buffer at point.
IMAGE is displayed by inserting STRING into the current buffer
with a `display' property whose value is the image.  STRING is
defaulted if you omit it.
AREA is where to display the image.  AREA nil or omitted means
display it in the text area, a value of `left-margin' means
display it in the left marginal area, a value of `right-margin'
means display it in the right marginal area.
The image is automatically split into ROW x COLS slices."
  (unless string (setq string " "))
  (unless (eq (car-safe image) 'image)
    (error "Not an image: %s" image))
  (unless (or (null area) (memq area '(left-margin right-margin)))
    (error "Invalid area %s" area))
  (if area
      (setq image (list (list 'margin area) image))
    ;; Cons up a new spec equal but not eq to `image' so that
    ;; inserting it twice in a row (adjacently) displays two copies of
    ;; the image.  Don't try to avoid this by looking at the display
    ;; properties on either side so that we DTRT more often with
    ;; cut-and-paste.  (Yanking killed image text next to another copy
    ;; of it loses anyway.)
    (setq image (cons 'image (cdr image))))
  (let ((x 0.0) (dx (/ 1.0001 (or cols 1)))
	 (y 0.0) (dy (/ 1.0001 (or rows 1))))
    (while (< y 1.0)
      (while (< x 1.0)
	(let ((start (point)))
	  (insert string)
	  (add-text-properties start (point)
			       `(display ,(list (list 'slice x y dx dy) image)
					 rear-nonsticky (display)))
	  (setq x (+ x dx))))
      (setq x 0.0
	    y (+ y dy))
      (insert (propertize "\n" 'line-height t)))))



;;;###autoload
(defun remove-images (start end &optional buffer)
  "Remove images between START and END in BUFFER.
Remove only images that were put in BUFFER with calls to `put-image'.
BUFFER nil or omitted means use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (let ((overlays (overlays-in start end)))
    (while overlays
      (let ((overlay (car overlays)))
	(when (overlay-get overlay 'put-image)
	  (delete-overlay overlay)))
      (setq overlays (cdr overlays)))))

(defun image-search-load-path (file &optional path)
  (unless path
    (setq path image-load-path))
  (let (element found filename)
    (while (and (not found) (consp path))
      (setq element (car path))
      (cond
       ((stringp element)
	(setq found
	      (file-readable-p
	       (setq filename (expand-file-name file element)))))
       ((and (symbolp element) (boundp element))
	(setq element (symbol-value element))
	(cond
	 ((stringp element)
	  (setq found
		(file-readable-p
		 (setq filename (expand-file-name file element)))))
	 ((consp element)
	  (if (setq filename (image-search-load-path file element))
	      (setq found t))))))
      (setq path (cdr path)))
    (if found filename)))

;;;###autoload
(defun find-image (specs)
  "Find an image, choosing one of a list of image specifications.

SPECS is a list of image specifications.

Each image specification in SPECS is a property list.  The contents of
a specification are image type dependent.  All specifications must at
least contain the properties `:type TYPE' and either `:file FILE' or
`:data DATA', where TYPE is a symbol specifying the image type,
e.g. `xbm', FILE is the file to load the image from, and DATA is a
string containing the actual image data.  The specification whose TYPE
is supported, and FILE exists, is used to construct the image
specification to be returned.  Return nil if no specification is
satisfied.

The image is looked for in `image-load-path'.

Image files should not be larger than specified by `max-image-size'."
  (let (image)
    (while (and specs (null image))
      (let* ((spec (car specs))
	     (type (plist-get spec :type))
	     (data (plist-get spec :data))
	     (file (plist-get spec :file))
	     found)
	(when (image-type-available-p type)
	  (cond ((stringp file)
		 (if (setq found (image-search-load-path file))
		     (setq image
			   (cons 'image (plist-put (copy-sequence spec)
						   :file found)))))
		((not (null data))
		 (setq image (cons 'image spec)))))
	(setq specs (cdr specs))))
    image))


;;;###autoload
(defmacro defimage (symbol specs &optional doc)
  "Define SYMBOL as an image.

SPECS is a list of image specifications.  DOC is an optional
documentation string.

Each image specification in SPECS is a property list.  The contents of
a specification are image type dependent.  All specifications must at
least contain the properties `:type TYPE' and either `:file FILE' or
`:data DATA', where TYPE is a symbol specifying the image type,
e.g. `xbm', FILE is the file to load the image from, and DATA is a
string containing the actual image data.  The first image
specification whose TYPE is supported, and FILE exists, is used to
define SYMBOL.

Example:

   (defimage test-image ((:type xpm :file \"~/test1.xpm\")
                         (:type xbm :file \"~/test1.xbm\")))"
  `(defvar ,symbol (find-image ',specs) ,doc))


(provide 'image)

;;; arch-tag: 8e76a07b-eb48-4f3e-a7a0-1a7ba9f096b3
;;; image.el ends here
