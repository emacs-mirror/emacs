;;; image-crop.el --- Image Cropping  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Keywords: multimedia

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

;; This package provides an interface for cropping images
;; interactively, but relies on external programs to do the actual
;; modifications to files.

;;; Code:

(require 'svg)
(require 'text-property-search)
(eval-when-compile (require 'subr-x))

(defvar image-scaling-factor)
(declare-function image-property "image.el" (image property))
(declare-function image-size "image.c" (spec &optional pixels frame))
(declare-function imagep "image.c" (spec))
(declare-function image--get-image "image.el" (&optional position))

(defgroup image-crop ()
  "Image cropping."
  :group 'image)

(defvar image-crop-exif-rotate nil
  "If non-nil, rotate images by updating Exif data.
If nil, rotate the images \"physically\".")

(defcustom image-crop-resize-command '("convert" "-resize" "%wx" "-" "%f:-")
  "List of command and command-line arguments to resize an image.
The following `format-spec' elements are allowed in the value:

%w: Width.
%f: File type to produce."
  :type '(repeat string)
  :version "29.1")

(defcustom image-crop-cut-command '("convert" "-draw" "rectangle %l,%t %r,%b"
                                    "-fill" "%c"
                                    "-" "%f:-")
  "List of command and its command-line arguments to cut a rectangle out of image.

The following `format-spec' elements are allowed in the value:
%l: Left.
%t: Top.
%r: Right.
%b: Bottom.
%c: Color.
%f: File type to produce."
  :type '(repeat string)
  :version "29.1")

(defcustom image-crop-crop-command '("convert" "+repage" "-crop" "%wx%h+%l+%t"
	                             "-" "%f:-")
  "List of command and its command-line arguments to crop an image.

The following `format-spec' elements are allowed in the value:
%l: Left.
%t: Top.
%w: Width.
%h: Height.
%f: File type to produce."
  :type '(repeat string)
  :version "29.1")

(defcustom image-crop-rotate-command '("convert" "-rotate" "%r" "-" "%f:-")
  "List of command and its command-line arguments to rotate an image.

The following `format-spec' elements are allowed in the value:
%r: Rotation (in degrees).
%f: File type to produce."
  :type '(repeat string)
  :version "29.1")

(defvar image-crop-buffer-text-function #'image-crop--default-buffer-text
  "Function to return the buffer text corresponding to the cropped image.
After cropping an image, the displayed image in the buffer will be updated
to show the cropped image.  Different modes will have different ways to
represent this image data in a buffer, but that's up to the mode.  For
instance, an HTML-based mode might want to represent the image with
<img src=\"data:...base64...\">.

The default action is to not alter the image's text in the buffer, and
just return it.

The function is called with two arguments: the original buffer text,
and the cropped image data.")

(defcustom image-cut-color "black"
  "Color to use for the rectangle that was cut from the image."
  :type 'string
  :version "29.1")

;;;###autoload
(defun image-cut (&optional color)
  "Cut a rectangle from the image under point, filling it with COLOR.
COLOR defaults to the value of `image-cut-color'.
Interactively, with prefix argument, prompt for COLOR to use.

This command presents the image with a rectangular area superimposed
on it, and allows moving and resizing the area to define which
part of it to cut.

While moving/resizing the cutting area, the following key bindings
are available:

`q':   Exit without changing anything.
`RET': Crop/cut the image.
`m':   Make mouse movements move the rectangle instead of altering the
       rectangle shape.
`s':   Same as `m', but make the rectangle into a square first.

After cutting the image, you can save it by `M-x image-save' or
\\<image-map>\\[image-save] when point is over the image."
  (interactive (list (and current-prefix-arg
                          (read-color "Color to use for filling: "))))
  (image-crop (if (zerop (length color)) image-cut-color color)))

;;;###autoload
(defun image-crop (&optional cut)
  "Crop the image under point.
This command presents the image with a rectangular area superimposed
on it, and allows moving and resizing the area to define which
part of it to crop.

While moving/resizing the cropping area, the following key bindings
are available:

`q':   Exit without changing anything.
`RET': Crop/cut the image.
`m':   Make mouse movements move the rectangle instead of altering the
       rectangle shape.
`s':   Same as `m', but make the rectangle into a square first.

After cropping the image, you can save it by `M-x image-save' or
\\<image-map>\\[image-save] when point is over the image.

When called from Lisp, if CUT is non-nil, remove a rectangle from
the image instead of cropping the image.  In that case, CUT should
be the name of a color to fill the rectangle."
  (interactive)
  (unless (image-type-available-p 'svg)
    (error "SVG support is needed to crop and cut images"))
  (let* ((crop-cmd (car image-crop-crop-command))
         (found (executable-find crop-cmd)))
    (unless found
      (error "Couldn't find `%s' command to crop/cut the image" crop-cmd))
    (if (and (memq system-type '(windows-nt ms-dos))
             ;; MS-Windows has an incompatible convert.exe, used to
             ;; convert filesystems...
             (string-equal crop-cmd "convert")
             (= 0 (string-search "Invalid drive specification."
                                 (shell-command-to-string
                                  (format "%s %s" crop-cmd null-device)))))
        (error "The program `%s' is not an image conversion program"
               found)))
  (let ((image (image--get-image)))
    (unless (imagep image)
      (user-error "No image under point"))
    (when (overlays-at (point))
      (user-error "Can't edit images that have overlays"))
    ;; We replace the image under point with an SVG image that looks
    ;; just like that image.  That allows us to draw lines over it.
    ;; At the end, we replace that SVG with a cropped version of the
    ;; original image.
    (let* ((data (cl-getf (cdr image) :data))
	   (undo-handle (prepare-change-group))
	   (type (cond
		  ((cl-getf (cdr image) :format)
		   (format "%s" (cl-getf (cdr image) :format)))
		  (data
		   (image-crop--content-type data))))
	   (image-scaling-factor 1)
           (orig-point (point))
	   (size (image-size image t))
	   (svg (svg-create (car size) (cdr size)
			    :xmlns:xlink "http://www.w3.org/1999/xlink"
			    :stroke-width 5))
           ;; We want to get the original text that's covered by the
           ;; image so that we can restore it.
           (image-start
            (save-excursion
              (let ((match (text-property-search-backward 'display image)))
                (if match
                    (prop-match-end match)
                  (point-min)))))
           (image-end
            (save-excursion
              (let ((match (text-property-search-forward 'display image)))
                (if match
                    (prop-match-beginning match)
                  (point-max)))))
	   (text (buffer-substring image-start image-end))
	   (inhibit-read-only t)
           orig-data svg-end)
      (with-temp-buffer
	(set-buffer-multibyte nil)
	(if (null data)
	    (insert-file-contents-literally (cl-getf (cdr image) :file))
	  (insert data))
	(let ((image-crop-exif-rotate nil))
	  (image-crop--possibly-rotate-buffer image))
	(setq orig-data (buffer-string))
	(setq type (image-crop--content-type orig-data))
        (image-crop--process image-crop-resize-command
                             `((?w . 600)
                               (?f . ,(cadr (split-string type "/")))))
	(setq data (buffer-string)))
      (svg-embed svg data type t
		 :width (car size)
		 :height (cdr size))
      (with-buffer-unmodified-if-unchanged
        (delete-region image-start image-end)
        (svg-insert-image svg)
        (setq svg-end (point))
        (let ((area (condition-case _
		        (save-excursion
			  (forward-line 1)
			  (image-crop--crop-image-1
                           svg (if cut "cut" "crop")))
                      (quit nil))))
          (message (substitute-command-keys
                    "Type \\[image-save] to save %s image to file")
                   (if cut "cut" "cropped"))
	  (delete-region image-start svg-end)
	  (if area
	      (image-crop--crop-image-update
               area orig-data size type cut text)
	    ;; If the user didn't complete the crop, re-insert the
	    ;; original image (and text).
	    (insert text)
            (goto-char orig-point))
	  (undo-amalgamate-change-group undo-handle))))))

(defun image-crop--crop-image-update (area data size type cut text)
  (let* ((image-scaling-factor 1)
	 (osize (image-size (create-image data nil t) t))
	 (factor (/ (float (car osize)) (car size)))
	 ;; width x height + left + top
	 (width (abs (truncate (* factor (- (cl-getf area :right)
					    (cl-getf area :left))))))
	 (height (abs (truncate (* factor (- (cl-getf area :bottom)
					     (cl-getf area :top))))))
	 (left (truncate (* factor (min (cl-getf area :left)
					(cl-getf area :right)))))
	 (top (truncate (* factor (min (cl-getf area :top)
				       (cl-getf area :bottom))))))
    (image-crop--insert-image-data
     (with-temp-buffer
       (set-buffer-multibyte nil)
       (insert data)
       (if cut
	   (image-crop--process image-crop-cut-command
                                `((?l . ,left)
                                  (?t . ,top)
                                  (?r . ,(+ left width))
                                  (?b . ,(+ top height))
                                  (?c . ,cut)
                                  (?f . ,(cadr (split-string type "/")))))
	 (image-crop--process image-crop-crop-command
                              `((?l . ,left)
                                (?t . ,top)
                                (?w . ,width)
                                (?h . ,height)
                                (?f . ,(cadr (split-string type "/"))))))
       (buffer-string))
     text)))

(defun image-crop--width (area)
  (- (plist-get area :right) (plist-get area :left)))

(defun image-crop--height (area)
  (- (plist-get area :bottom) (plist-get area :top)))

(defun image-crop--crop-image-1 (svg op)
  (track-mouse
    (cl-loop
     with prompt = (format
                    (substitute-command-keys
                     "Select area for %s (click \\`mouse-1' and drag)")
                    op)
     and state = 'begin
     and area = (list :left 0
		      :top 0
		      :right 0
		      :bottom 0)
     and corner = nil
     for event = (read-event prompt)
     do (cond
         ;; Go to "square" mode.
         ((eql event ?s)
          (setq state 'move-unclick
                prompt (format "Move square for %s" op))
          (let ((size (min (image-crop--width area) (image-crop--height area))))
            (setf (plist-get area :right) (+ (plist-get area :left) size)
                  (plist-get area :bottom) (+ (plist-get area :top) size))))
         ;; Go to "move" move.
         ((eql event ?m)
          (setq state 'move-unclick
                prompt (format "Move for %s" op)))
         ;; We have a (relevant) mouse event.
         ((and (consp event)
               (consp (cadr event))
               (nth 7 (cadr event))
	       ;; Only do things if point is over the SVG being
	       ;; tracked.
               (eq (cl-getf (cdr (nth 7 (cadr event))) :type)
		   'svg))
	  (let ((pos (nth 8 (cadr event))))
	    (cl-case state
	      (begin
	       (cond
		((eq (car event) 'down-mouse-1)
		 (setq state 'stretch
                       prompt (format "Stretch to end point for %s" op))
		 (setf (cl-getf area :left) (car pos)
		       (cl-getf area :top) (cdr pos)
		       (cl-getf area :right) (car pos)
		       (cl-getf area :bottom) (cdr pos)))))
	      (stretch
	       (cond
		((eq (car event) 'mouse-movement)
		 (setf (cl-getf area :right) (car pos)
		       (cl-getf area :bottom) (cdr pos)))
		((memq (car event) '(mouse-1 drag-mouse-1))
		 (setq state 'corner
                       prompt (format
                               (substitute-command-keys
                                (concat
                                 "Type \\`RET' to %s, or click and drag "
                                 "\\`mouse-1' to adjust corners"))
                               op)))))
	      (corner
	       (cond
		((eq (car event) 'down-mouse-1)
		 ;; Find out what corner we're close to.
		 (setq corner (image-crop--find-corner
			       area pos
			       '((:left :top)
				 (:left :bottom)
				 (:right :top)
				 (:right :bottom))))
		 (when corner
		   (setq state 'adjust
                         prompt (format
                                 (substitute-command-keys
                                  "Adjusting %s area (release \\`mouse-1' to confirm)")
                                 op))))))
	      (adjust
	       (cond
		((memq (car event) '(mouse drag-mouse-1))
		 (setq state 'corner
                       prompt (format "Choose corner to adjust area for %s" op)))
		((eq (car event) 'mouse-movement)
		 (setf (cl-getf area (car corner)) (car pos)
		       (cl-getf area (cadr corner)) (cdr pos)))))
	      (move-unclick
	       (cond
		((eq (car event) 'down-mouse-1)
		 (setq state 'move-click
                       prompt (format "Move for %s" op)))))
	      (move-click
	       (cond
		((eq (car event) 'mouse-movement)
		 (setf (cl-getf area :right)
                       (+ (car pos) (image-crop--width area)))
                 (setf (cl-getf area :left) (car pos))
                 (setf (cl-getf area :bottom)
                       (+ (cdr pos) (image-crop--height area)))
                 (setf (cl-getf area :top) (cdr pos)))
		((memq (car event) '(mouse-1 drag-mouse-1))
		 (setq state 'move-unclick
                       prompt (format "Click to move for %s" op)))))))))
     do (svg-rectangle svg (cl-getf area :left) (cl-getf area :top)
                       (image-crop--width area) (image-crop--height area)
                       :stroke-color "red" :stroke-width 2
                       :fill-opacity 0.3 :fill "black" :id "rect")
     while (not (member event '(return ?q)))
     finally (return (and (eq event 'return)
			  area)))))

(defun image-crop--find-corner (area pos corners)
  (cl-loop for corner in corners
	   ;; We accept 10 pixels off.
	   when (and (< (- (car pos) 10)
			(cl-getf area (car corner))
			(+ (car pos) 10))
		     (< (- (cdr pos) 10)
			(cl-getf area (cadr corner))
			(+ (cdr pos) 10)))
	   return corner))

(defun image-crop--content-type (image)
  ;; Get the MIME type by running "file" over it.
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert image)
    (call-process-region (point-min) (point-max)
			 "file" t (current-buffer) nil
			 "--mime-type" "-")
    (cadr (split-string (buffer-string)))))

(defun image-crop--possibly-rotate-buffer (image)
  (when (imagep image)
    (let ((content-type (image-crop--content-type (buffer-string))))
      (when (image-property image :rotation)
	(cond
	 ;; We can rotate jpegs losslessly by setting the correct
	 ;; orientation.
	 ((and image-crop-exif-rotate
	       (equal content-type "image/jpeg")
	       (executable-find "exiftool"))
	  (call-process-region
	   (point-min) (point-max) "exiftool" t (list (current-buffer) nil) nil
	   (format "-Orientation#=%d"
		   (cl-case (truncate (image-property image :rotation))
		     (0 0)
		     (90 6)
		     (180 3)
		     (270 8)
		     (otherwise 0)))
	   "-o" "-" "-"))
	 ;; Most other image formats have to be reencoded to do
	 ;; rotation.
	 (t
          (image-crop--process
           image-crop-rotate-command
           `((?r . ,(image-property image :rotation))
             (?f . ,(cadr (split-string content-type "/")))))
	  (when (and (equal content-type "image/jpeg")
		     (executable-find "exiftool"))
	    (call-process-region
	     (point-min) (point-max) "exiftool"
             t (list (current-buffer) nil) nil
	     "-Orientation#=0"
	     "-o" "-" "-")))))
      (when (image-property image :width)
        (image-crop--process
         image-crop-resize-command
         `((?w . ,(image-property image :width))
           (?f . ,(cadr (split-string content-type "/")))))))))

(defun image-crop--insert-image-data (image text)
  (insert-image
   (create-image image nil t
		 :max-width (- (frame-pixel-width) 50)
		 :max-height (- (frame-pixel-height) 150))
   (funcall image-crop-buffer-text-function text image)
   nil nil t))

(defun image-crop--process (command expansions)
  "Use `call-process-region' with COMMAND expanded with EXPANSIONS."
  (apply
   #'call-process-region (point-min) (point-max)
   (format-spec (car command) expansions)
   t (list (current-buffer) nil) nil
   (mapcar (lambda (elem)
             (format-spec elem expansions))
           (cdr command))))

(defun image-crop--default-buffer-text (text _image)
  (substring-no-properties text))

(provide 'image-crop)

;;; image-crop.el ends here
