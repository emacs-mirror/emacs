;;; image-mode.el --- support for visiting image files
;;
;; Copyright (C) 2005 Free Software Foundation, Inc.
;;
;; Author: Richard Stallman <rms@gnu.org>
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

;; Defines a major mode for visiting image files
;; that allows conversion between viewing the text of the file
;; and viewing the file as an image.  Viewing the image
;; works by putting a `display' text-property on the
;; image data, with the image-data still present underneath; if the
;; resulting buffer file is saved to another name it will correctly save
;; the image data to the new file.

;;; Code:

(require 'image)

;;;###autoload (push '("\\.jpg\\'" . image-mode) auto-mode-alist)
;;;###autoload (push '("\\.jpeg\\'" . image-mode) auto-mode-alist)
;;;###autoload (push '("\\.gif\\'" . image-mode) auto-mode-alist)
;;;###autoload (push '("\\.png\\'" . image-mode) auto-mode-alist)
;;;###autoload (push '("\\.tiff\\'" . image-mode) auto-mode-alist)
;;;###autoload (push '("\\.tif\\'" . image-mode) auto-mode-alist)
;;;###autoload (push '("\\.xbm\\'" . image-mode) auto-mode-alist)
;;;###autoload (push '("\\.pbm\\'" . image-mode) auto-mode-alist)
;;;###autoload (push '("\\.pgm\\'" . image-mode) auto-mode-alist)
;;;###autoload (push '("\\.ppm\\'" . image-mode) auto-mode-alist)
;;;###autoload (push '("\\.pnm\\'" . image-mode) auto-mode-alist)

(defvar image-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'image-toggle-display)
    map)
  "Major mode keymap for Image mode.")

;;;###autoload
(defun image-mode ()
  "Major mode for image files.
You can use \\<image-mode-map>\\[image-toggle-display]
to toggle between display as an image and display as text."
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Image")
  (setq major-mode 'image-mode)
  (use-local-map image-mode-map)
  (run-mode-hooks 'image-mode-hook)
  (message (substitute-command-keys
	    "Type \\[image-toggle-display] to view the image as an image.")))

(defun image-toggle-display ()
  "Start or stop displaying an image file as the actual image.
This command toggles between showing the text of the image file
and showing the image as an image."
  (interactive)
  (if (get-text-property (point-min) 'display)
      (let ((inhibit-read-only t)
	    (buffer-undo-list t))
	(remove-list-of-text-properties (point-min) (point-max)
					'(display intangible read-nonsticky
						  read-only front-sticky))
	(kill-local-variable 'cursor-type)
	(kill-local-variable 'truncate-lines)
	(message "Repeat this command to go back to displaying the image"))
    ;; Turn the image data into a real image, but only if the whole file
    ;; was inserted
    (let* ((data
	    (string-make-unibyte
	     (buffer-substring-no-properties (point-min) (point-max))))
	   (image
	    (create-image data nil t))
	   (props
	    `(display ,image
		      intangible ,image
		      rear-nonsticky (display intangible)
		      ;; This a cheap attempt to make the whole buffer
		      ;; read-only when we're visiting the file (as
		      ;; opposed to just inserting it).
		      read-only t front-sticky (read-only)))
	   (buffer-undo-list t))
      (add-text-properties (point-min) (point-max) props)
      ;; Inhibit the cursor when the buffer contains only an image,
      ;; because cursors look very strange on top of images.
      (setq cursor-type nil)
      ;; This just makes the arrow displayed in the right fringe
      ;; area look correct when the image is wider than the window.
      (setq truncate-lines t)
      (message "Repeat this command to go back to displaying the file as text"))))

(provide 'image-mode)

;;; image-mode.el ends here
