;;; image-mode.el --- support for visiting image files
;;
;; Copyright (C) 2005, 2006, 2007 Free Software Foundation, Inc.
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

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

;;;###autoload (push '("\\.jpe?g\\'"    . image-mode) auto-mode-alist)
;;;###autoload (push '("\\.png\\'"      . image-mode) auto-mode-alist)
;;;###autoload (push '("\\.gif\\'"      . image-mode) auto-mode-alist)
;;;###autoload (push '("\\.tiff?\\'"    . image-mode) auto-mode-alist)
;;;###autoload (push '("\\.p[bpgn]m\\'" . image-mode) auto-mode-alist)
;;;###autoload (push '("\\.x[bp]m\\'"   . image-mode-maybe) auto-mode-alist)

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
  (add-hook 'change-major-mode-hook 'image-toggle-display-text nil t)
  (if (and (display-images-p)
	   (not (get-text-property (point-min) 'display)))
      (image-toggle-display)
    ;; Set next vars when image is already displayed but local
    ;; variables were cleared by kill-all-local-variables
    (setq cursor-type nil truncate-lines t))
  (run-mode-hooks 'image-mode-hook)
  (if (display-images-p)
      (message "%s" (concat
		     (substitute-command-keys
		      "Type \\[image-toggle-display] to view as ")
		     (if (get-text-property (point-min) 'display)
			 "text" "an image") "."))))

;;;###autoload
(define-minor-mode image-minor-mode
  "Toggle Image minor mode.
With arg, turn Image minor mode on if arg is positive, off otherwise.
See the command `image-mode' for more information on this mode."
  nil " Image" image-mode-map
  :group 'image
  :version "22.1"
  (if (not image-minor-mode)
      (image-toggle-display-text)
    (if (get-text-property (point-min) 'display)
	(setq cursor-type nil truncate-lines t))
    (add-hook 'change-major-mode-hook (lambda () (image-minor-mode -1)) nil t)
    (message "%s" (concat (substitute-command-keys
		      "Type \\[image-toggle-display] to view the image as ")
		     (if (get-text-property (point-min) 'display)
			 "text" "an image") "."))))

;;;###autoload
(defun image-mode-maybe ()
  "Set major or minor mode for image files.
Set Image major mode only when there are no other major modes
associated with a filename in `auto-mode-alist'.  When an image
filename matches another major mode in `auto-mode-alist' then
set that major mode and Image minor mode.

See commands `image-mode' and `image-minor-mode' for more
information on these modes."
  (interactive)
  (let* ((mode-alist
	  (delq nil (mapcar
		     (lambda (elt)
		       (unless (memq (or (car-safe (cdr elt)) (cdr elt))
				     '(image-mode image-mode-maybe))
			 elt))
		     auto-mode-alist))))
    (if (assoc-default buffer-file-name mode-alist 'string-match)
	(let ((auto-mode-alist mode-alist)
	      (magic-mode-alist nil))
	  (set-auto-mode)
	  (image-minor-mode t))
      (image-mode))))

(defun image-toggle-display-text ()
  "Showing the text of the image file."
  (if (get-text-property (point-min) 'display)
      (image-toggle-display)))

(defvar archive-superior-buffer)
(defvar tar-superior-buffer)

(defun image-toggle-display ()
  "Start or stop displaying an image file as the actual image.
This command toggles between showing the text of the image file
and showing the image as an image."
  (interactive)
  (if (get-text-property (point-min) 'display)
      (let ((inhibit-read-only t)
	    (buffer-undo-list t)
	    (modified (buffer-modified-p)))
	(remove-list-of-text-properties (point-min) (point-max)
					'(display intangible read-nonsticky
						  read-only front-sticky))
	(set-buffer-modified-p modified)
	(kill-local-variable 'cursor-type)
	(kill-local-variable 'truncate-lines)
	(if (called-interactively-p)
	    (message "Repeat this command to go back to displaying the image")))
    ;; Turn the image data into a real image, but only if the whole file
    ;; was inserted
    (let* ((image
	    (if (and (buffer-file-name)
		     (not (file-remote-p (buffer-file-name)))
		     (not (buffer-modified-p))
		     (not (and (boundp 'archive-superior-buffer)
			       archive-superior-buffer))
		     (not (and (boundp 'tar-superior-buffer)
			       tar-superior-buffer)))
		(progn (clear-image-cache)
		       (create-image (buffer-file-name)))
	      (create-image
	       (string-make-unibyte
		(buffer-substring-no-properties (point-min) (point-max)))
	       nil t)))
	   (props
	    `(display ,image
		      intangible ,image
		      rear-nonsticky (display intangible)
		      ;; This a cheap attempt to make the whole buffer
		      ;; read-only when we're visiting the file (as
		      ;; opposed to just inserting it).
		      read-only t front-sticky (read-only)))
	   (inhibit-read-only t)
	   (buffer-undo-list t)
	   (modified (buffer-modified-p)))
      (add-text-properties (point-min) (point-max) props)
      (set-buffer-modified-p modified)
      ;; Inhibit the cursor when the buffer contains only an image,
      ;; because cursors look very strange on top of images.
      (setq cursor-type nil)
      ;; This just makes the arrow displayed in the right fringe
      ;; area look correct when the image is wider than the window.
      (setq truncate-lines t)
      (if (called-interactively-p)
	  (message "Repeat this command to go back to displaying the file as text")))))

(provide 'image-mode)

;; arch-tag: b5b2b7e6-26a7-4b79-96e3-1546b5c4c6cb
;;; image-mode.el ends here
