;;; image-file.el --- Support for visiting image files
;;
;; Copyright (C) 2000 Free Software Foundation, Inc.
;;
;; Author: Miles Bader <miles@gnu.org>
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

;; Defines a file-name-handler hook that transforms visited (or
;; inserted) image files so that they are by displayed as emacs as
;; images.  This is done by putting a `display' text-property on the
;; image data, with the image-data still present underneath; if the
;; resulting buffer file is saved to another name it will correctly save
;; the image data to the new file.

;;; Code:

(require 'image)

;;;###autoload
(defcustom image-file-handler-enabled nil
  "True if visiting an image file will actually display the image.
A file is considered an image file if its filename matches one of the
regexps in `image-file-regexps'.

Setting this variable directly does not take effect;
use either \\[customize] or the function `set-image-file-handler-enabled'."
  :type 'boolean
  :set (lambda (sym val) (set-image-file-handler-enabled val))
  :initialize 'custom-initialize-default
  :require 'image-file
  :group 'image)

(defcustom image-file-regexps
  '("\\.png$" "\\.jpeg$" "\\.jpg$" "\\.gif$" "\\.tiff$" "\\.x[bp]m$")
  "*A list of regexps matching files that should be displayed as images.

Setting this variable directly does not take effect until the next time
`set-image-file-handler-enabled' is called (which happens automatically
when using \\[customize]."
  :type '(repeat regexp)
  :set (lambda (sym val)
	 (set-default sym val)
	 (when image-file-handler-enabled
	   ;; Re-initialize the image-file handler
	   (set-image-file-handler-enabled t)))
  :initialize 'custom-initialize-default
  :group 'image)

;;;###autoload
(defun insert-image-file (file &optional visit beg end replace)
  "Insert the image file FILE into the current buffer.
Optional arguments VISIT, BEG, END, and REPLACE are interpreted as for
the command `insert-file-contents'."
  (let ((rval
	 (image-file-call-underlying #'insert-file-contents-literally
				     'insert-file-contents
				     file visit beg end replace)))
    (when (and image-file-handler-enabled (or (null beg) (zerop beg)) (null end))
      ;; Make image into a picture, but only if the whole file was inserted
      (let* ((ibeg (point))
	     (iend (+ (point) (cadr rval)))
	     (data
	      (string-make-unibyte (buffer-substring-no-properties ibeg iend)))
	     (image
	      (create-image data nil t))
	     (props
	      `(display ,image
			intangible ,image
			rear-nonsticky (display)
			;; This a cheap attempt to make the whole buffer
			;; read-only when we're visiting the file.
			,@(and visit
			       (= ibeg (point-min))
			       (= iend (point-max))
			       '(read-only t front-sticky (read-only))))))
	(add-text-properties ibeg iend props)))
    rval))

(defun image-file-handler (operation &rest args)
  "File name handler for inserting image files.
OPERATION is the operation to perform, on ARGS.
See `file-name-handler-alist' for details."
  (if (eq operation 'insert-file-contents)
      (apply #'insert-image-file args)
    ;; We don't handle OPERATION, use another handler or the default
    (apply #'image-file-call-underlying operation operation args)))

(defun image-file-call-underlying (function operation &rest args)
  "Call FUNCTION with `image-file-handler' and OPERATION inhibited.
Optional argument ARGS are the arguments to call FUNCTION with."
  (let ((inhibit-file-name-handlers
	 (cons 'image-file-handler
	       (and (eq inhibit-file-name-operation operation)
		    inhibit-file-name-handlers)))
	(inhibit-file-name-operation operation))
    (apply function args)))

;;;###autoload
(defun set-image-file-handler-enabled (enabled)
  "Enable or disable visiting image files as real images, as per ENABLED.
The regexp in `image-file-regexp' is used to determine which filenames are
considered image files."
  ;; Remove existing handler
  (let ((existing-entry (rassq 'image-file-handler file-name-handler-alist)))
    (when existing-entry
      (setq file-name-handler-alist
	    (delq existing-entry file-name-handler-alist))))
  ;; Add new handler
  (when enabled
    (let ((regexp
	   (concat "\\("
		   (mapconcat 'identity image-file-regexps "\\|")
		   "\\)")))
      (setq file-name-handler-alist
	    (cons (cons regexp 'image-file-handler) file-name-handler-alist))))
  (setq-default image-file-handler-enabled enabled))

;;;###autoload
(defun enable-image-file-handler ()
  "Enable visiting image files as real images.
The regexp in `image-file-regexp' is used to determine which filenames are
considered image files."
  (interactive)
  (set-image-file-handler-enabled t))

(defun disable-image-file-handler ()
  "Disable visiting image files as real images."
  (interactive)
  (set-image-file-handler-enabled nil))

(provide 'image-file)

;;; image-file.el ends here
