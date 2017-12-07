;;; svg.el --- SVG image creation functions -*- lexical-binding: t -*-

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: image

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

;;; Code:

(require 'cl-lib)
(require 'xml)
(require 'dom)
(eval-when-compile (require 'subr-x))

(defun svg-create (width height &rest args)
  "Create a new, empty SVG image with dimensions WIDTHxHEIGHT.
ARGS can be used to provide `stroke' and `stroke-width' parameters to
any further elements added."
  (dom-node 'svg
	    `((width . ,width)
	      (height . ,height)
	      (version . "1.1")
	      (xmlns . "http://www.w3.org/2000/svg")
	      ,@(svg--arguments nil args))))

(defun svg-gradient (svg id type stops)
  "Add a gradient with ID to SVG.
TYPE is `linear' or `radial'.  STOPS is a list of percentage/color
pairs."
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

:stroke-width PIXELS.  The line width.
:stroke-color COLOR.  The line color.
:gradient ID.  The gradient ID to use."
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
  "Create a line of starting in X1/Y1, ending at X2/Y2 in SVG."
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
  (when-let* ((node (car (dom-by-id
                          svg
                          (concat "\\`" (regexp-quote id)
                                  "\\'")))))
    (dom-remove-node svg node)))

(provide 'svg)

;;; svg.el ends here
