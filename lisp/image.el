;;; image.el --- image API

;; Copyright (C) 1998 Free Software Foundation, Inc.
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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defconst image-type-regexps
  '(("^/\\*.*XPM.\\*/" . xpm)
    ("^P[1-6]" . pbm)
    ("^GIF8" . gif)
    ("JFIF" . jpeg)
    ("^\211PNG\r\n" . png)
    ("^#define" . xbm)
    ("^\\(MM\0\\*\\)\\|\\(II\\*\0\\)" . tiff)
    ("^%!PS" . ghostscript))
  "Alist of (REGEXP . IMAGE-TYPE) pairs used to auto-detect image types.
When the first bytes of an image file match REGEXP, it is assumed to
be of image type IMAGE-TYPE.")


;;;###autoload
(defun image-type-from-file-header (file)
  "Determine the type of image file FILE from its first few bytes.
Value is a symbol specifying the image type, or nil if type cannot
be determined."
  (unless (file-name-directory file)
    (setq file (concat data-directory file)))
  (setq file (expand-file-name file))
  (let ((header (with-temp-buffer
		  (insert-file-contents-literally file nil 0 256)
		  (buffer-string)))
	(types image-type-regexps)
	type)
    (while (and types (null type))
      (let ((regexp (car (car types)))
	    (image-type (cdr (car types))))
	(when (string-match regexp header)
	  (setq type image-type))
	(setq types (cdr types))))
    type))


;;;###autoload
(defun image-type-available-p (type)
  "Value is non-nil if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'."
  (not (null (memq type image-types))))


;;;###autoload
(defun create-image (file &optional type &rest props)
  "Create an image which will be loaded from FILE.
Optional TYPE is a symbol describing the image type.  If TYPE is omitted
or nil, try to determine the image file type from its first few bytes.
If that doesn't work, use FILE's extension.as image type.
Optional PROPS are additional image attributes to assign to the image,
like, e.g. `:heuristic-mask t'.
Value is the image created, or nil if images of type TYPE are not supported."
  (unless (stringp file)
    (error "Invalid image file name %s" file))
  (unless (or type
	      (setq type (image-type-from-file-header file)))
    (let ((extension (file-name-extension file)))
      (unless extension
	(error "Cannot determine image type"))
      (setq type (intern extension))))
  (unless (symbolp type)
    (error "Invalid image type %s" type))
  (when (image-type-available-p type)
    (append (list 'image :type type :file file) props)))


;;;###autoload
(defun put-image (image pos &optional area)
  "Put image IMAGE in front of POS in the current buffer.
IMAGE must be an image created with `create-image' or `defimage'.
POS may be an integer or marker.
AREA is where to display the image.  AREA nil or omitted means
display it in the text area, a value of `left-margin' means
display it in the left marginal area, a value of `right-margin'
means display it in the right marginal area.
IMAGE is displayed by putting an overlay into the current buffer with a
`before-string' that has a `display' property whose value is the
image."
  (let ((buffer (current-buffer)))
    (unless (eq (car image) 'image)
      (error "Not an image: %s" image))
    (unless (or (null area) (memq area '(left-margin right-margin)))
      (error "Invalid area %s" area))
    (let ((overlay (make-overlay pos pos buffer))
	  (string (make-string 1 ?x))
	  (prop (if (null area) image (cons area image))))
      (put-text-property 0 1 'display prop string)
      (overlay-put overlay 'put-image t)
      (overlay-put overlay 'before-string string))))


;;;###autoload
(defun insert-image (image &optional area)
  "Insert IMAGE into current buffer at point.
AREA is where to display the image.  AREA nil or omitted means
display it in the text area, a value of `left-margin' means
display it in the left marginal area, a value of `right-margin'
means display it in the right marginal area.
IMAGE is displayed by inserting an \"x\" into the current buffer
having a `display' property whose value is the image."
  (unless (eq (car image) 'image)
    (error "Not an image: %s" image))
  (unless (or (null area) (memq area '(left-margin right-margin)))
    (error "Invalid area %s" area))
  (insert "x")
  (add-text-properties (1- (point)) (point)
		       (list 'display (if (null area) image (cons area image))
			     'rear-nonsticky (list 'display))))
	

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
	  (delete-overlay overlay)
	(setq overlays (cdr overlays)))))))


;;;###autoload
(defmacro defimage (symbol specs &optional doc)
  "Define SYMBOL as an image.

SPECS is a list of image specifications.  DOC is an optional
documentation string.

Each image specification in SPECS is a property list.  The contents of
a specification are image type dependent.  All specifications must at
least contain the properties `:type TYPE' and `:file FILE', where TYPE
is a symbol specifying the image type, e.g. `xbm', and FILE is the
file to load the image from.  The first image specification whose TYPE
is supported, and FILE exists, is used to define SYMBOL.

Example:

   (defimage test-image ((:type xpm :file \"~/test1.xpm\")
                         (:type xbm :file \"~/test1.xbm\")))"
  (let (image)
    (while (and specs (null image))
      (let* ((spec (car specs))
	     (type (plist-get spec :type))
	     (file (plist-get spec :file)))
	(when (and (image-type-available-p type) (stringp file))
	  (setq file (expand-file-name file))
	  (unless (file-name-absolute-p file)
	    (setq file (concat data-directory "/" file)))
	  (when (file-exists-p file)
	    (setq image (cons 'image spec))))
	(setq specs (cdr specs))))
    `(defvar ,symbol ',image ,doc)))


(provide 'image)

  ;; image.el ends here.




