;;; image-dired-dired.el --- Dired specific commands for Image-Dired  -*- lexical-binding: t -*-

;; Copyright (C) 2005-2024 Free Software Foundation, Inc.

;; Author: Mathias Dahl <mathias.rem0veth1s.dahl@gmail.com>
;; Maintainer: Stefan Kangas <stefankangas@gmail.com>
;; Keywords: multimedia
;; Package: image-dired

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

;; See the description of the `image-dired' package.

;;; Code:

(require 'image-dired)

(defgroup image-dired-dired nil
  "Dired specific commands for Image-Dired."
  :prefix "image-dired-dired-"
  :link '(info-link "(emacs) Image-Dired")
  :group 'image-dired)

(define-obsolete-variable-alias 'image-dired-append-when-browsing
  'image-dired-dired-append-when-browsing "29.1")
(defcustom image-dired-dired-append-when-browsing nil
  "Append thumbnails in thumbnail buffer when browsing.
If non-nil, using `image-dired-next-line-and-display' and
`image-dired-previous-line-and-display' will leave a trail of thumbnail
images in the thumbnail buffer.  If you enable this and want to clean
the thumbnail buffer because it is filled with too many thumbnails,
just call `image-dired-display-thumb' to display only the image at point.
This value can be toggled using `image-dired-toggle-append-browsing'."
  :type 'boolean)

(defcustom image-dired-dired-disp-props t
  "If non-nil, display properties for Dired file when browsing.
Used by `image-dired-next-line-and-display',
`image-dired-previous-line-and-display' and `image-dired-mark-and-display-next'.
If the database file is large, this can slow down image browsing in
Dired and you might want to turn it off."
  :type 'boolean)

;;;###autoload
(defun image-dired-dired-toggle-marked-thumbs (&optional arg)
  "Toggle thumbnails in front of marked file names in the Dired buffer.
If no file is marked, toggle display of thumbnail on the current file's line.
ARG, if non-nil (interactively, the prefix argument), specifies the files
whose thumbnail display to toggle instead of the marked files: if ARG is an
integer, use the next ARG (or previous -ARG, if ARG<0) files; any other
value of ARG means toggle thumbnail display of the current line's file."
  (interactive "P" dired-mode)
  (setq image-dired--generate-thumbs-start  (current-time))
  (dired-map-over-marks
   (let ((image-pos  (dired-move-to-filename))
         (image-file (dired-get-filename nil t))
         thumb-file
         overlay)
     (when (and image-file
                (string-match-p (image-dired--file-name-regexp) image-file))
       (setq thumb-file (create-image
                         (image-dired--get-create-thumbnail-file image-file)))
       ;; If image is not already added, then add it.
       (let ((thumb-ov (cl-loop for ov in (overlays-in (point) (1+ (point)))
                                if (overlay-get ov 'thumb-file) return ov)))
         (if thumb-ov
             (delete-overlay thumb-ov)
           (put-image thumb-file image-pos)
           (setq overlay
                 (cl-loop for ov in (overlays-in (point) (1+ (point)))
                          if (overlay-get ov 'put-image) return ov))
           (overlay-put overlay 'image-file image-file)
           (overlay-put overlay 'thumb-file thumb-file)))))
   ;; Show or hide thumbnail on ARG next files.
   arg)
  (add-hook 'dired-after-readin-hook
            'image-dired-dired-after-readin-hook nil t))

(defun image-dired-dired-after-readin-hook ()
  "Relocate existing thumbnail overlays in Dired buffer after reverting.
Move each overlay to its corresponding file if it still exists.
Otherwise, delete the overlay.
Used by `image-dired-dired-toggle-marked-thumbs'."
  (mapc (lambda (overlay)
          (when (overlay-get overlay 'put-image)
            (let* ((image-file (overlay-get overlay 'image-file))
                   (image-pos (dired-goto-file image-file)))
              (if image-pos
                  (move-overlay overlay image-pos image-pos)
                (delete-overlay overlay)))))
        (overlays-in (point-min) (point-max))))

(defun image-dired-next-line-and-display ()
  "Move to next Dired line and display its thumbnail image."
  (interactive nil dired-mode)
  (dired-next-line 1)
  (image-dired-display-thumbs t image-dired-dired-append-when-browsing t)
  (if image-dired-dired-disp-props
      (image-dired-dired-display-properties)))

(defun image-dired-previous-line-and-display ()
  "Move to previous Dired line and display its thumbnail image."
  (interactive nil dired-mode)
  (dired-previous-line 1)
  (image-dired-display-thumbs t image-dired-dired-append-when-browsing t)
  (if image-dired-dired-disp-props
      (image-dired-dired-display-properties)))

(defun image-dired-toggle-append-browsing ()
  "Toggle `image-dired-dired-append-when-browsing'."
  (interactive nil dired-mode)
  (setq image-dired-dired-append-when-browsing
        (not image-dired-dired-append-when-browsing))
  (message "Append browsing %s"
           (if image-dired-dired-append-when-browsing
               "on"
             "off")))

(defun image-dired-mark-and-display-next ()
  "Mark current file in Dired and display the next thumbnail image."
  (interactive nil dired-mode)
  (dired-mark 1)
  (image-dired-display-thumbs t image-dired-dired-append-when-browsing t)
  (if image-dired-dired-disp-props
      (image-dired-dired-display-properties)))

(defun image-dired-toggle-dired-display-properties ()
  "Toggle `image-dired-dired-disp-props'."
  (interactive nil dired-mode)
  (setq image-dired-dired-disp-props
        (not image-dired-dired-disp-props))
  (message "Dired display properties %s"
           (if image-dired-dired-disp-props
               "on"
             "off")))

(defun image-dired-track-thumbnail ()
  "Move to thumbnail of the current Dired file in `image-dired-thumbnail-buffer'.
This is almost the same as what `image-dired-track-original-file' does,
but the other way around."
  (let ((file (dired-get-filename))
        prop-val found window)
    (when (get-buffer image-dired-thumbnail-buffer)
      (with-current-buffer image-dired-thumbnail-buffer
        (goto-char (point-min))
        (while (and (not (eobp))
                    (not found))
          (if (and (setq prop-val
                         (get-text-property (point) 'original-file-name))
                   (string= prop-val file))
              (setq found t))
          (if (not found)
              (forward-char 1)))
        (when found
          (if (setq window (image-dired-thumbnail-window))
              (set-window-point window (point)))
          (image-dired--update-header-line))))))

(defun image-dired-dired-next-line (&optional arg)
  "Call `dired-next-line', while tracking the file's thumbnail.
This can safely replace `dired-next-line'.
With prefix argument ARG, move that many lines."
  (interactive "P" dired-mode)
  (dired-next-line (or arg 1))
  (if image-dired-track-movement
      (image-dired-track-thumbnail)))

(defun image-dired-dired-previous-line (&optional arg)
  "Call `dired-previous-line', while tracking the file's thumbnail.
This can safely replace `dired-previous-line'.
With prefix argument ARG, move that many lines."
  (interactive "P" dired-mode)
  (dired-previous-line (or arg 1))
  (if image-dired-track-movement
      (image-dired-track-thumbnail)))

;;;###autoload
(defun image-dired-jump-thumbnail-buffer ()
  "Jump to thumbnail buffer."
  (interactive nil dired-mode)
  (let ((window (image-dired-thumbnail-window))
        frame)
    (if window
        (progn
          (if (not (equal (selected-frame) (setq frame (window-frame window))))
              (select-frame-set-input-focus frame))
          (select-window window))
      (message "Thumbnail buffer not visible"))))

(defvar-keymap image-dired-minor-mode-map
  :doc "Keymap for `image-dired-minor-mode'."
  "<remap> <dired-previous-line>" #'image-dired-dired-previous-line
  "<remap> <dired-next-line>"     #'image-dired-dired-next-line
  "C-S-n"  #'image-dired-next-line-and-display
  "C-S-p"  #'image-dired-previous-line-and-display
  "C-S-m"  #'image-dired-mark-and-display-next
  "<tab>"  #'image-dired-jump-thumbnail-buffer

  :menu
  '("Image-Dired"
    ["Display thumb for next file" image-dired-next-line-and-display]
    ["Display thumb for previous file" image-dired-previous-line-and-display]
    ["Mark and display next" image-dired-mark-and-display-next]
    "---"
    ["Create thumbnails for marked files" image-dired-create-thumbs]
    "---"
    ["Display thumbnails append" image-dired-display-thumbs-append]
    ["Display this thumbnail" image-dired-display-thumb]
    ["Display image" image-dired-dired-display-image]
    ["Display in external viewer" image-dired-dired-display-external]
    "---"
    ["Toggle display properties" image-dired-toggle-dired-display-properties
     :style toggle
     :selected image-dired-dired-disp-props]
    ["Toggle append browsing" image-dired-toggle-append-browsing
     :style toggle
     :selected image-dired-dired-append-when-browsing]
    ["Toggle movement tracking" image-dired-toggle-movement-tracking
     :style toggle
     :selected image-dired-track-movement]
    "---"
    ["Jump to thumbnail buffer" image-dired-jump-thumbnail-buffer]
    ["Mark tagged files" image-dired-mark-tagged-files]
    ["Comment files" image-dired-dired-comment-files]
    ["Copy with EXIF file name" image-dired-copy-with-exif-file-name]))

;;;###autoload
(define-minor-mode image-dired-minor-mode
  "Setup easy-to-use keybindings for Image-Dired in Dired mode.

This minor mode adds these additional bindings:
\\<image-dired-minor-mode-map>
  \\[image-dired-next-line-and-display]		Move to next line and display \
thumbnail image.
  \\[image-dired-previous-line-and-display]		Move to previous line \
and display thumbnail image.
  \\[image-dired-mark-and-display-next]		Mark current file and display \
next thumbnail image.
  \\[image-dired-jump-thumbnail-buffer]		Jump to thumbnail buffer.

For reference, these are the default Image-Dired bindings that
are always available in Dired:
\\<dired-mode-map>
  \\[image-dired-display-thumbs]		Display thumbnails of all marked files.
  \\[image-dired-tag-files]		Tag marked file(s).
  \\[image-dired-delete-tag]		Remove tag for selected file(s).
  \\[image-dired-jump-thumbnail-buffer]		Jump to thumbnail buffer.
  \\[image-dired-dired-display-image]		Display current image file.
  \\[image-dired-dired-display-external]		Display file at point \
using an external viewer.
  \\[image-dired-display-thumbs-append]		Append thumbnails to \
thumbnail buffer.
  \\[image-dired-display-thumb]		Display thumbnails of all marked files.
  \\[image-dired-dired-comment-files]		Add comment to current or \
marked files in Dired.
  \\[image-dired-mark-tagged-files]		Use REGEXP to mark files with \
matching tag.
  \\[image-dired-dired-toggle-marked-thumbs]	Toggle thumbnails in \
front of file names.
  \\[image-dired-dired-edit-comment-and-tags]		Edit comment and tags \
of marked images."
  :keymap image-dired-minor-mode-map)

(declare-function clear-image-cache "image.c" (&optional filter))

(defun image-dired-create-thumbs (&optional arg)
  "Create thumbnail images for all marked files in Dired.
With prefix argument ARG, create thumbnails even if they already exist
\(i.e. use this to refresh your thumbnails)."
  (interactive "P" dired-mode)
  (let (thumb-name)
    (dolist (curr-file (dired-get-marked-files))
      (setq thumb-name (image-dired-thumb-name curr-file))
      ;; If the user overrides the exist check, we must clear the
      ;; image cache so that if the user wants to display the
      ;; thumbnail, it is not fetched from cache.
      (when arg
        (clear-image-cache (expand-file-name thumb-name)))
      (when (or (not (file-exists-p thumb-name))
                arg)
        (image-dired-create-thumb curr-file thumb-name)))))

;;;###autoload
(defun image-dired-display-thumbs-append ()
  "Append thumbnails to `image-dired-thumbnail-buffer'."
  (interactive nil dired-mode)
  (image-dired-display-thumbs nil t t))

;;;###autoload
(defun image-dired-display-thumb ()
  "Shorthand for `image-dired-display-thumbs' with prefix argument."
  (interactive nil dired-mode)
  (image-dired-display-thumbs t nil t))

;;;###autoload
(defun image-dired-dired-display-external ()
  "Display file at point using an external viewer.
The viewer is specified by the value of `image-dired-external-viewer'."
  (interactive nil dired-mode)
  (let ((file (dired-get-filename)))
    (start-process "image-dired-external" nil
                   image-dired-external-viewer file)))

;;;###autoload
(defun image-dired-dired-display-image (&optional _)
  "Display current image file.
See documentation for `image-dired-display-image' for more information."
  (declare (advertised-calling-convention () "29.1"))
  (interactive nil dired-mode)
  (image-dired-display-image (dired-get-filename)))

(defun image-dired-copy-with-exif-file-name ()
  "Copy file with unique name to main image directory.
Copy current or all files marked in Dired to new file(s) in your
main image directory, using file name(s) generated by
`image-dired-get-exif-file-name'.  A typical usage for this if when
copying images from a digital camera into the image directory.

Typically, you would open up the folder with the incoming
digital images, mark the files to be copied, and execute this
command.  The result is one or more new files in
`image-dired-main-image-directory', named like
2005_05_08_12_52_00_dscn0319.jpg,
2005_05_08_14_27_45_dscn0320.jpg etc."
  (interactive nil dired-mode)
  (let (new-name
        (files (dired-get-marked-files)))
    (mapc
     (lambda (curr-file)
       (setq new-name
             (format "%s/%s"
                     (file-name-as-directory
                      (expand-file-name image-dired-main-image-directory))
                     (image-dired-get-exif-file-name curr-file)))
       (message "Copying %s to %s" curr-file new-name)
       (copy-file curr-file new-name))
     files)))

;;;###autoload
(defun image-dired-mark-tagged-files (regexp)
  "Mark files whose tag matches REGEXP.
A `tag' is a keyword, a piece of meta data, associated with an
image file and stored in image-dired's database file.  This command
prompts for a regexp, and then matches it against all the tags
of all the image files in the database file.  The files that have a
matching tag will be marked in the Dired buffer."
  (interactive "sMark tagged files (regexp): " dired-mode)
  (image-dired-sane-db-file)
  (let ((hits 0)
        files)
    (image-dired--with-db-file
      ;; Collect matches
      (while (search-forward-regexp "\\(^[^;\n]+\\);\\(.*\\)" nil t)
        (let ((file (match-string 1))
              (tags (split-string (match-string 2) ";")))
          (when (seq-find (lambda (tag)
                            (string-match-p regexp tag))
                          tags)
            (push file files)))))
    ;; Mark files
    (dolist (curr-file files)
      ;; I tried using `dired-mark-files-regexp' but it was waaaay to
      ;; slow.  Don't bother about hits found in other directories
      ;; than the current one.
      (when (string= (file-name-as-directory
                      (expand-file-name default-directory))
                     (file-name-as-directory
                      (file-name-directory curr-file)))
        (setq curr-file (file-name-nondirectory curr-file))
        (goto-char (point-min))
        (when (search-forward-regexp (format "\\s %s$" curr-file) nil t)
          (setq hits (+ hits 1))
          (dired-mark 1))))
    (message "%d files with matching tag marked" hits)))

(defun image-dired-dired-display-properties ()
  "Show in the echo area the image-related properties of a file in Dired buffer."
  (interactive nil dired-mode)
  (let* ((file-name (dired-get-filename))
         (dired-buf (buffer-name (current-buffer)))
         (image-count "")               ; TODO
         (props (string-join (image-dired-list-tags file-name) ", "))
         (comment (image-dired-get-comment file-name))
         (message-log-max nil))
    (if file-name
        (message "%s"
                 (image-dired-format-properties-string
                  dired-buf
                  file-name
                  image-count
                  props
                  comment)))))

(provide 'image-dired-dired)

;;; image-dired-dired.el ends here
