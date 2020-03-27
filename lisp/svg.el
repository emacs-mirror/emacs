;;; svg.el --- SVG image creation functions -*- lexical-binding: t -*-

;; Copyright (C) 2014-2020 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;         Felix E. Klee <felix.klee@inka.de>
;; Keywords: image
;; Version: 1.0
;; Package-Requires: ((emacs "25"))

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

;; This package allows creating SVG images in Emacs.  SVG images are
;; vector-based XML files, really, so you could create them directly
;; as XML.  However, that's really tedious, as there are some fiddly
;; bits.

;; In addition, the `svg-insert-image' function allows inserting an
;; SVG image into a buffer that's updated "on the fly" as you
;; add/alter elements to the image, which is useful when composing the
;; images.

;; Here are some usage examples:

;; Create the base image structure, add a gradient spec, and insert it
;; into the buffer:
;;
;;     (setq svg (svg-create 800 800 :stroke "orange" :stroke-width 5))
;;     (svg-gradient svg "gradient" 'linear '(0 . "red") '(100 . "blue"))
;;     (save-excursion (goto-char (point-max)) (svg-insert-image svg))

;; Then add various elements to the structure:
;;
;;     (svg-rectangle svg 100 100 500 500 :gradient "gradient" :id "rec1")
;;     (svg-circle svg 500 500 100 :id "circle1")
;;     (svg-ellipse svg 100 100 50 90 :stroke "red" :id "ellipse1")
;;     (svg-line svg 100 190 50 100 :id "line1" :stroke "yellow")
;;     (svg-polyline svg '((200 . 100) (500 . 450) (80 . 100))
;;                   :stroke "green" :id "poly1")
;;     (svg-polygon svg '((100 . 100) (200 . 150) (150 . 90))
;;                  :stroke "blue" :fill "red" :id "gon1")

;;; Code:

(require 'cl-lib)
(require 'xml)
(require 'dom)

(defun svg-create (width height &rest args)
  "Create a new, empty SVG image with dimensions WIDTH x HEIGHT.
ARGS can be used to provide `stroke' and `stroke-width' parameters to
any further elements added."
  (dom-node 'svg
	    `((width . ,width)
	      (height . ,height)
	      (version . "1.1")
	      (xmlns . "http://www.w3.org/2000/svg")
              ,@(unless (plist-get args :xmlns:xlink)
                  '((xmlns:xlink . "http://www.w3.org/1999/xlink")))
              ,@(svg--arguments nil args))))

(defun svg-gradient (svg id type stops)
  "Add a gradient with ID to SVG.
TYPE is `linear' or `radial'.
STOPS is a list of percentage/color pairs."
  (svg--def
   svg
   (apply
    'dom-node
    (if (eq type 'linear)
	'linearGradient
      'radialGradient)
    `((id . ,id)
      (x1 . 0)
      (x2 . 0)
      (y1 . 0)
      (y2 . 1))
    (mapcar
     (lambda (stop)
       (dom-node 'stop `((offset . ,(format "%s%%" (car stop)))
			 (stop-color . ,(cdr stop)))))
     stops))))

(defun svg-rectangle (svg x y width height &rest args)
  "Create a rectangle on SVG, starting at position X/Y, of WIDTH/HEIGHT.
ARGS is a plist of modifiers.  Possible values are

:stroke-width PIXELS   The line width.
:stroke-color COLOR    The line color.
:gradient ID           The gradient ID to use."
  (svg--append
   svg
   (dom-node 'rect
	     `((width . ,width)
	       (height . ,height)
	       (x . ,x)
	       (y . ,y)
	       ,@(svg--arguments svg args)))))

(defun svg-circle (svg x y radius &rest args)
  "Create a circle of RADIUS on SVG.
X/Y denote the center of the circle."
  (svg--append
   svg
   (dom-node 'circle
	     `((cx . ,x)
	       (cy . ,y)
	       (r . ,radius)
	       ,@(svg--arguments svg args)))))

(defun svg-ellipse (svg x y x-radius y-radius &rest args)
  "Create an ellipse of X-RADIUS/Y-RADIUS on SVG.
X/Y denote the center of the ellipse."
  (svg--append
   svg
   (dom-node 'ellipse
	     `((cx . ,x)
	       (cy . ,y)
	       (rx . ,x-radius)
	       (ry . ,y-radius)
	       ,@(svg--arguments svg args)))))

(defun svg-line (svg x1 y1 x2 y2 &rest args)
  "Create a line starting in X1/Y1, ending at X2/Y2 on SVG."
  (svg--append
   svg
   (dom-node 'line
	     `((x1 . ,x1)
	       (x2 . ,x2)
	       (y1 . ,y1)
	       (y2 . ,y2)
	       ,@(svg--arguments svg args)))))

(defun svg-polyline (svg points &rest args)
  "Create a polyline going through POINTS on SVG.
POINTS is a list of x/y pairs."
  (svg--append
   svg
   (dom-node
    'polyline
    `((points . ,(mapconcat (lambda (pair)
			      (format "%s %s" (car pair) (cdr pair)))
			    points
			    ", "))
      ,@(svg--arguments svg args)))))

(defun svg-polygon (svg points &rest args)
  "Create a polygon going through POINTS on SVG.
POINTS is a list of x/y pairs."
  (svg--append
   svg
   (dom-node
    'polygon
    `((points . ,(mapconcat (lambda (pair)
			      (format "%s %s" (car pair) (cdr pair)))
			    points
			    ", "))
      ,@(svg--arguments svg args)))))

(defun svg-embed (svg image image-type datap &rest args)
  "Insert IMAGE into the SVG structure.
IMAGE should be a file name if DATAP is nil, and a binary string
otherwise.  IMAGE-TYPE should be a MIME image type, like
\"image/jpeg\" or the like."
  (svg--append
   svg
   (dom-node
    'image
    `((xlink:href . ,(svg--image-data image image-type datap))
      ,@(svg--arguments svg args)))))

(defun svg-text (svg text &rest args)
  "Add TEXT to SVG."
  (svg--append
   svg
   (dom-node
    'text
    `(,@(svg--arguments svg args))
    (svg--encode-text text))))

(defun svg--encode-text (text)
  ;; Apparently the SVG renderer needs to have all non-ASCII
  ;; characters encoded, and only certain special characters.
  (with-temp-buffer
    (insert text)
    (dolist (substitution '(("&" . "&amp;")
			    ("<" . "&lt;")
			    (">" . "&gt;")))
      (goto-char (point-min))
      (while (search-forward (car substitution) nil t)
	(replace-match (cdr substitution) t t nil)))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((char (following-char)))
        (if (< char 128)
            (forward-char 1)
          (delete-char 1)
          (insert "&#" (format "%d" char) ";"))))
    (buffer-string)))

(defun svg--append (svg node)
  (let ((old (and (dom-attr node 'id)
		  (dom-by-id svg
                             (concat "\\`" (regexp-quote (dom-attr node 'id))
                                     "\\'")))))
    (if old
        ;; FIXME: This was (dom-set-attributes old (dom-attributes node))
        ;; and got changed by commit f7ea7aa11f6211b5142bbcfc41c580d75485ca56
        ;; without any explanation.
	(setcdr (car old) (cdr node))
      (dom-append-child svg node)))
  (svg-possibly-update-image svg))

(defun svg--image-data (image image-type datap)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (if datap
        (insert image)
      (insert-file-contents image))
    (base64-encode-region (point-min) (point-max) t)
    (goto-char (point-min))
    (insert "data:" image-type ";base64,")
    (buffer-string)))

(defun svg--arguments (svg args)
  (let ((stroke-width (or (plist-get args :stroke-width)
			  (dom-attr svg 'stroke-width)))
	(stroke-color (or (plist-get args :stroke-color)
                          (dom-attr svg 'stroke-color)))
        (fill-color (plist-get args :fill-color))
	attr)
    (when stroke-width
      (push (cons 'stroke-width stroke-width) attr))
    (when stroke-color
      (push (cons 'stroke stroke-color) attr))
    (when fill-color
      (push (cons 'fill fill-color) attr))
    (when (plist-get args :gradient)
      (setq attr
	    (append
	     ;; We need a way to specify the gradient direction here...
	     `((x1 . 0)
	       (x2 . 0)
	       (y1 . 0)
	       (y2 . 1)
	       (fill . ,(format "url(#%s)"
				(plist-get args :gradient))))
	     attr)))
    (cl-loop for (key value) on args by #'cddr
	     unless (memq key '(:stroke-color :stroke-width :gradient
                                              :fill-color))
	     ;; Drop the leading colon.
	     do (push (cons (intern (substring (symbol-name key) 1) obarray)
			    value)
		      attr))
    attr))

(defun svg--def (svg def)
  (dom-append-child
   (or (dom-by-tag svg 'defs)
       (let ((node (dom-node 'defs)))
	 (dom-add-child-before svg node)
	 node))
   def)
  svg)

(defun svg-image (svg &rest props)
  "Return an image object from SVG.
PROPS is passed on to `create-image' as its PROPS list."
  (apply
   #'create-image
   (with-temp-buffer
     (svg-print svg)
     (buffer-string))
   'svg t props))

(defun svg-insert-image (svg)
  "Insert SVG as an image at point.
If the SVG is later changed, the image will also be updated."
  (let ((image (svg-image svg))
	(marker (point-marker)))
    (insert-image image)
    (dom-set-attribute svg :image marker)))

(defun svg-possibly-update-image (svg)
  (let ((marker (dom-attr svg :image)))
    (when (and marker
	       (buffer-live-p (marker-buffer marker)))
      (with-current-buffer (marker-buffer marker)
	(put-text-property marker (1+ marker) 'display (svg-image svg))))))

(defun svg-print (dom)
  "Convert DOM into a string containing the xml representation."
  (if (stringp dom)
      (insert dom)
    (insert (format "<%s" (car dom)))
    (dolist (attr (nth 1 dom))
      ;; Ignore attributes that start with a colon.
      (unless (= (aref (format "%s" (car attr)) 0) ?:)
        (insert (format " %s=\"%s\"" (car attr) (cdr attr)))))
    (insert ">")
    (dolist (elem (nthcdr 2 dom))
      (insert " ")
      (svg-print elem))
    (insert (format "</%s>" (car dom)))))

(defun svg-remove (svg id)
  "Remove the element identified by ID from SVG."
  (let* ((node (car (dom-by-id
                     svg
                     (concat "\\`" (regexp-quote id)
                             "\\'")))))
    (when node (dom-remove-node svg node))))

;; Function body copied from `org-plist-delete' in Emacs 26.1.
(defun svg--plist-delete (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (not (eq property (car plist)))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun svg--path-command-symbol (command-symbol command-args)
  (let ((char (symbol-name command-symbol))
        (relative (if (plist-member command-args :relative)
                      (plist-get command-args :relative)
                    (plist-get command-args :default-relative))))
    (intern (if relative (downcase char) (upcase char)))))

(defun svg--elliptical-arc-coordinates
    (rx ry x y &rest args)
  (list
   rx ry
   (or (plist-get args :x-axis-rotation) 0)
   (if (plist-get args :large-arc) 1 0)
   (if (plist-get args :sweep) 1 0)
   x y))

(defun svg--elliptical-arc-command (coordinates-list &rest args)
  (cons
   (svg--path-command-symbol 'a args)
   (apply 'append
          (mapcar
           (lambda (coordinates)
             (apply 'svg--elliptical-arc-coordinates
                    coordinates))
           coordinates-list))))

(defun svg--moveto-command (coordinates-list &rest args)
  (cons
   (svg--path-command-symbol 'm args)
   (apply 'append
          (mapcar
           (lambda (coordinates)
             (list (car coordinates) (cdr coordinates)))
           coordinates-list))))

(defun svg--closepath-command (&rest args)
  (list (svg--path-command-symbol 'z args)))

(defun svg--lineto-command (coordinates-list &rest args)
  (cons
   (svg--path-command-symbol 'l args)
   (apply 'append
          (mapcar
           (lambda (coordinates)
             (list (car coordinates) (cdr coordinates)))
           coordinates-list))))

(defun svg--horizontal-lineto-command (coordinate-list &rest args)
  (cons
   (svg--path-command-symbol 'h args)
   coordinate-list))

(defun svg--vertical-lineto-command (coordinate-list &rest args)
  (cons
   (svg--path-command-symbol 'v args)
   coordinate-list))

(defun svg--curveto-command (coordinates-list &rest args)
  (cons
   (svg--path-command-symbol 'c args)
   (apply 'append coordinates-list)))

(defun svg--smooth-curveto-command (coordinates-list &rest args)
  (cons
   (svg--path-command-symbol 's args)
   (apply 'append coordinates-list)))

(defun svg--quadratic-bezier-curveto-command (coordinates-list
                                              &rest args)
  (cons
   (svg--path-command-symbol 'q args)
   (apply 'append coordinates-list)))

(defun svg--smooth-quadratic-bezier-curveto-command (coordinates-list
                                                     &rest args)
  (cons
   (svg--path-command-symbol 't args)
   (apply 'append coordinates-list)))

(defun svg--eval-path-command (command default-relative)
  (cl-letf
      (((symbol-function 'moveto) #'svg--moveto-command)
       ((symbol-function 'closepath) #'svg--closepath-command)
       ((symbol-function 'lineto) #'svg--lineto-command)
       ((symbol-function 'horizontal-lineto)
        #'svg--horizontal-lineto-command)
       ((symbol-function 'vertical-lineto)
        #'svg--vertical-lineto-command)
       ((symbol-function 'curveto) #'svg--curveto-command)
       ((symbol-function 'smooth-curveto)
        #'svg--smooth-curveto-command)
       ((symbol-function 'quadratic-bezier-curveto)
        #'svg--quadratic-bezier-curveto-command)
       ((symbol-function 'smooth-quadratic-bezier-curveto)
        #'svg--smooth-quadratic-bezier-curveto-command)
       ((symbol-function 'elliptical-arc)
        #'svg--elliptical-arc-command)
       (extended-command (append command (list :default-relative
                                               default-relative))))
    (mapconcat 'prin1-to-string (apply extended-command) " ")))

(defun svg-path (svg commands &rest args)
  "Add the outline of a shape to SVG according to COMMANDS.
Coordinates by default are absolute.  ARGS is a plist of
modifiers.  If :relative is t, then coordinates are relative to
the last position, or -- initially -- to the origin."
  (let* ((default-relative (plist-get args :relative))
         (stripped-args (svg--plist-delete args :relative))
         (d (mapconcat 'identity
                       (mapcar
                        (lambda (command)
                          (svg--eval-path-command command
                                                  default-relative))
                        commands) " ")))
    (svg--append
     svg
     (dom-node 'path
               `((d . ,d)
                 ,@(svg--arguments svg stripped-args))))))

(defun svg-clip-path (svg &rest args)
  "Add a clipping path to SVG, where ARGS is a plist of modifiers.
If applied to a shape via the :clip-path property, parts of that
shape which lie outside of the clipping path are not drawn."
  (let ((new-dom-node (dom-node 'clipPath
                                `(,@(svg--arguments svg args)))))
    (svg--append svg new-dom-node)
    new-dom-node))

(defun svg-node (svg tag &rest args)
  "Add the custom node TAG to SVG."
  (let ((new-dom-node (dom-node tag
                                `(,@(svg--arguments svg args)))))
    (svg--append svg new-dom-node)
    new-dom-node))

(provide 'svg)

;;; svg.el ends here
