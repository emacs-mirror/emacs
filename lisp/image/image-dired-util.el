;;; image-dired-util.el --- util functions for Image-Dired  -*- lexical-binding: t -*-

;; Copyright (C) 2005-2022 Free Software Foundation, Inc.

;; Author: Mathias Dahl <mathias.rem0veth1s.dahl@gmail.com>

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

(require 'xdg)
(eval-when-compile (require 'cl-lib))

(defvar image-dired-dir)
(defvar image-dired-thumbnail-storage)

(defconst image-dired--thumbnail-standard-sizes
  '( standard standard-large
     standard-x-large standard-xx-large)
  "List of symbols representing thumbnail sizes in Thumbnail Managing Standard.")

(defvar image-dired-debug nil
  "Non-nil means enable debug messages.")

(defun image-dired-debug-message (&rest args)
  "Display debug message ARGS when `image-dired-debug' is non-nil."
  (when image-dired-debug
    (apply #'message args)))

(defun image-dired-dir ()
  "Return the current thumbnail directory (from variable `image-dired-dir').
Create the thumbnail directory if it does not exist."
  (let ((image-dired-dir (file-name-as-directory
                          (expand-file-name image-dired-dir))))
    (unless (file-directory-p image-dired-dir)
      (with-file-modes #o700
        (make-directory image-dired-dir t))
      (message "Thumbnail directory created: %s" image-dired-dir))
    image-dired-dir))

(defun image-dired-thumb-name (file)
  "Return absolute file name for thumbnail FILE.
Depending on the value of `image-dired-thumbnail-storage', the
file name of the thumbnail will vary:
- For `use-image-dired-dir', make a SHA1-hash of the image file's
  directory name and add that to make the thumbnail file name
  unique.
- For `per-directory' storage, just add a subdirectory.
- For `standard' storage, produce the file name according to the
  Thumbnail Managing Standard.  Among other things, an MD5-hash
  of the image file's directory name will be added to the
  filename.
See also `image-dired-thumbnail-storage'."
  (cond ((memq image-dired-thumbnail-storage
               image-dired--thumbnail-standard-sizes)
         (let ((thumbdir (cl-case image-dired-thumbnail-storage
                           (standard "thumbnails/normal")
                           (standard-large "thumbnails/large")
                           (standard-x-large "thumbnails/x-large")
                           (standard-xx-large "thumbnails/xx-large"))))
           (expand-file-name
            ;; MD5 is mandated by the Thumbnail Managing Standard.
            (concat (md5 (concat "file://" (expand-file-name file))) ".png")
            (expand-file-name thumbdir (xdg-cache-home)))))
        ((eq 'use-image-dired-dir image-dired-thumbnail-storage)
         (let* ((f (expand-file-name file))
                (hash
                 (md5 (file-name-as-directory (file-name-directory f)))))
           (format "%s%s%s.thumb.%s"
                   (file-name-as-directory (expand-file-name (image-dired-dir)))
                   (file-name-base f)
                   (if hash (concat "_" hash) "")
                   (file-name-extension f))))
        ((eq 'per-directory image-dired-thumbnail-storage)
         (let ((f (expand-file-name file)))
           (format "%s.image-dired/%s.thumb.%s"
                   (file-name-directory f)
                   (file-name-base f)
                   (file-name-extension f))))))

(defvar image-dired-thumbnail-buffer "*image-dired*"
  "Image-Dired's thumbnail buffer.")

(defvar image-dired-display-image-buffer "*image-dired-display-image*"
  "Where larger versions of the images are display.")

(defun image-dired-original-file-name ()
  "Get original file name for thumbnail or display image at point."
  (get-text-property (point) 'original-file-name))

(defun image-dired-file-name-at-point ()
  "Get abbreviated file name for thumbnail or display image at point."
  (when-let ((f (image-dired-original-file-name)))
    (abbreviate-file-name f)))

(defun image-dired-associated-dired-buffer ()
  "Get associated Dired buffer at point."
  (get-text-property (point) 'associated-dired-buffer))

(defun image-dired-get-buffer-window (buf)
  "Return window where buffer BUF is."
  (get-window-with-predicate
   (lambda (window)
     (equal (window-buffer window) buf))
   nil t))

(defun image-dired-display-window ()
  "Return window where `image-dired-display-image-buffer' is visible."
  (get-window-with-predicate
   (lambda (window)
     (equal (buffer-name (window-buffer window)) image-dired-display-image-buffer))
   nil t))

(defun image-dired-thumbnail-window ()
  "Return window where `image-dired-thumbnail-buffer' is visible."
  (get-window-with-predicate
   (lambda (window)
     (equal (buffer-name (window-buffer window)) image-dired-thumbnail-buffer))
   nil t))

(defun image-dired-associated-dired-buffer-window ()
  "Return window where associated Dired buffer is visible."
  (let (buf)
    (if (image-dired-image-at-point-p)
        (progn
          (setq buf (image-dired-associated-dired-buffer))
          (get-window-with-predicate
           (lambda (window)
             (equal (window-buffer window) buf))))
      (error "No thumbnail image at point"))))

(defun image-dired-image-at-point-p ()
  "Return non-nil if there is an `image-dired' thumbnail at point."
  (get-text-property (point) 'image-dired-thumbnail))

(defun image-dired-window-width-pixels (window)
  "Calculate WINDOW width in pixels."
  (declare (obsolete window-body-width "29.1"))
  (* (window-width window) (frame-char-width)))

(provide 'image-dired-util)

;; Local Variables:
;; nameless-current-name: "image-dired"
;; End:

;;; image-dired-util.el ends here
