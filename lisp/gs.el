;;; gs.el --- interface to Ghostscript

;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

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

;; This code is experimental.  Don't use it.

;;; Code:

(defvar gs-program "gs"
  "The name of the Ghostscript interpreter.")


(defvar gs-device "x11"
  "The Ghostscript device to use to produce images.")


(defvar gs-options 
  '("-q"
    ;"-dNOPAUSE"
    "-dBATCH"
    "-sDEVICE=<device>"
    "<file>")
  "List of command line arguments to pass to Ghostscript.
Arguments may contain place-holders `<file>' for the name of the
input file, and `<device>' for the device to use.")


(defun gs-replace-in-string (string find repl)
  "Return STRING with all occurrences of FIND replaced by REPL.
FIND is a regular expression."
  (while (string-match find string)
    (setq string (replace-match repl nil t string)))
  string)


(defun gs-options (device file)
  "Return a list of command line options with place-holders replaced.
DEVICE is the value to substitute for the place-holder `<device>',
FILE is the value to substitute for the place-holder `<file>'."
  (mapcar #'(lambda (option)
	      (setq option (gs-replace-in-string option "<device>" device)
		    option (gs-replace-in-string option "<file>" file)))
	  gs-options))
  

;; The GHOSTVIEW property (taken from gv 3.5.8).
;; 
;; Type:
;;
;; STRING
;; 
;; Parameters:
;; 
;; BPIXMAP ORIENT LLX LLY URX URY XDPI YDPI [LEFT BOTTOM TOP RIGHT]
;; 
;; Scanf format: "%d %d %d %d %d %d %f %f %d %d %d %d"
;; 
;; Explanation of parameters:
;; 
;; BPIXMAP: pixmap id of the backing pixmap for the window.  If no
;; pixmap is to be used, this parameter should be zero.  This
;; parameter must be zero when drawing on a pixmap.
;; 
;; ORIENT: orientation of the page.  The number represents clockwise
;; rotation of the paper in degrees.  Permitted values are 0, 90, 180,
;; 270.
;; 
;; LLX, LLY, URX, URY: Bounding box of the drawable.  The bounding box
;; is specified in PostScript points in default user coordinates.
;; 
;; XDPI, YDPI: Resolution of window.  (This can be derived from the
;; other parameters, but not without roundoff error.  These values are
;; included to avoid this error.)
;; 
;; LEFT, BOTTOM, TOP, RIGHT: (optional) Margins around the window.
;; The margins extend the imageable area beyond the boundaries of the
;; window.  This is primarily used for popup zoom windows.  I have
;; encountered several instances of PostScript programs that position
;; themselves with respect to the imageable area.  The margins are
;; specified in PostScript points.  If omitted, the margins are
;; assumed to be 0.

(defun gs-width-in-pt (frame pixel-width)
  "Return, on FRAME, pixel width PIXEL-WIDTH tranlated to pt."
  (let ((mm (* (float pixel-width)
	       (/ (float (x-display-mm-width frame))
		  (float (x-display-pixel-width frame))))))
    (/ (* 25.4 mm) 72.0)))


(defun gs-height-in-pt (frame pixel-height)
  "Return, on FRAME, pixel height PIXEL-HEIGHT tranlated to pt."
  (let ((mm (* (float pixel-height)
	       (/ (float (x-display-mm-height frame))
		  (float (x-display-pixel-height frame))))))
    (/ (* 25.4 mm) 72.0)))
	

(defun gs-set-ghostview-window-prop (frame spec img-width img-height)
  "Set the `GHOSTVIEW' window property of FRAME.
SPEC is a GS image specification.  IMG-WIDTH is the width of the
requested image, and IMG-HEIGHT is the height of the requested
image in pixels."
  (let* ((box (plist-get (cdr spec) :bounding-box))
	 (llx (nth 0 box))
	 (lly (nth 1 box))
	 (urx (nth 2 box))
	 (ury (nth 3 box))
	 (rotation (or (plist-get (cdr spec) :rotate) 0))
	 ;; The pixel width IMG-WIDTH of the pixmap gives the
	 ;; dots, URX - LLX give the inch.
	 (in-width (/ (- urx llx) 72.0))
	 (in-height (/ (- ury lly) 72.0))
	 (xdpi (/ img-width in-width))
	 (ydpi (/ img-height in-height)))
    (x-change-window-property "GHOSTVIEW"
			      (format "0 %d %d %d %d %d %g %g"
				      rotation llx lly urx ury xdpi ydpi)
			      frame)))


(defun gs-set-ghostview-colors-window-prop (frame pixel-colors)
  "Set the `GHOSTVIEW_COLORS' environment variable depending on FRAME."
  (let ((mode (cond ((x-display-color-p frame) "Color")
		    ((x-display-grayscale-p frame) "Grayscale")
		    (t "Monochrome"))))
    (x-change-window-property "GHOSTVIEW_COLORS"
			      (format "%s %s" mode pixel-colors))))
	      

;
;;;###autoload
(defun gs-load-image (frame spec img-width img-height window-and-pixmap-id
			    pixel-colors)
  "Load a PS image for display on FRAME.
SPEC is an image specification, IMG-HEIGHT and IMG-WIDTH are width
and height of the image in pixels.  WINDOW-AND-PIXMAP-ID is a string of
the form \"WINDOW-ID PIXMAP-ID\".  Value is non-nil if successful."
  (unwind-protect
      (let ((file (plist-get (cdr spec) :file))
	    gs)
	(gs-set-ghostview-window-prop frame spec img-width img-height)
	(gs-set-ghostview-colors-window-prop frame pixel-colors)
	(setenv "GHOSTVIEW" window-and-pixmap-id)
	(setq gs (apply 'start-process "gs" "*GS*" gs-program
			(gs-options gs-device file)))
	(process-kill-without-query gs)
	gs)
    nil))


;(defun gs-put-tiger ()
;  (let* ((ps-file "/usr/local/share/ghostscript/5.10/examples/tiger.ps")
;	  (spec `(image :type ghostscript
;			:pt-width 200 :pt-height 200
;			:bounding-box (22 171 567 738)
;			:file ,ps-file)))
;    (put-text-property 1 2 'display spec)))
;    

(provide 'gs)

;; gs.el ends here.
