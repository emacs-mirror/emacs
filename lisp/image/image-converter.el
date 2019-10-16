;;; image-converter.el --- Converting images from exotic formats -*- lexical-binding: t -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: images

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

;; The main interface function here is `image-convert'.

;;; Code:

(require 'cl-generic)
(eval-when-compile (require 'cl-lib))

(defcustom image-converter nil
  "Type of the external image converter to use.
The value should a symbol, either `imagemagick', `graphicsmagick',
or `ffmpeg'.
If nil, Emacs will try to find one of the supported converters
installed on the system."
  :group 'image
  :type 'symbol
  :version "27.1")

(defvar image-converter-regexp nil
  "A regexp that matches the file name suffixes that can be converted.")

(defvar image-converter--converters
  '((graphicsmagick :command ("gm" "convert") :probe ("-list" "format"))
    (ffmpeg :command "ffmpeg" :probe "-decoders")
    (imagemagick :command "convert" :probe ("-list" "format")))
  "List of supported image converters to try.")

(defun image-convert-p (file)
  "Return `image-convert' if FILE is an image file that can be converted."
  ;; Find an installed image converter.
  (unless image-converter
    (image-converter--find-converter))
  (and image-converter
       (string-match image-converter-regexp file)
       'image-convert))

(defun image-convert (image)
  "Convert IMAGE file to the PNG format.
IMAGE can either be a file name, which will make the return value
a string with the image data.  It can also be an image object as
returned by `create-image'.  If so, it has to be an image object
where created with DATA-P nil (i.e., it has to refer to a file)."
  ;; Find an installed image converter.
  (unless image-converter
    (image-converter--find-converter))
  (unless image-converter
    (error "No external image converters available"))
  (when (and (listp image)
             (not (plist-get (cdr image) :file)))
    (error "Only images that refer to files can be converted"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (when-let ((err (image-converter--convert
                     image-converter
                     (if (listp image)
                         (plist-get (cdr image) :file)
                       image))))
      (error "%s" err))
    (if (listp image)
        ;; Return an image object that's the same as we were passed,
        ;; but ignore the :type and :file values.
        (apply #'create-image (buffer-string) 'png t
               (cl-loop for (key val) on (cdr image) by #'cddr
                        unless (memq key '(:type :file))
                        append (list key val)))
      (buffer-string))))

(defun image-converter--value (type elem)
  "Return the value of ELEM of image converter TYPE."
  (let ((value (plist-get (cdr (assq type image-converter--converters)) elem)))
    (if (stringp value)
        (list value)
      value)))

(cl-defmethod image-converter--probe ((type (eql graphicsmagick)))
  "Check whether the system has GraphicsMagick installed."
  (with-temp-buffer
    (let ((command (image-converter--value type :command))
          formats)
      (when (zerop (apply #'call-process (car command) nil '(t nil) nil
                          (append (cdr command)
                                  (image-converter--value type :probe))))
        (goto-char (point-min))
        (when (re-search-forward "^-" nil t)
          (forward-line 1)
          ;; Lines look like
          ;; "   8BIM P  rw-  Photoshop resource format".
          (while (re-search-forward "^ *\\([A-Z0-9]+\\) +. +r" nil t)
            (push (downcase (match-string 1)) formats)))
        (nreverse formats)))))

(cl-defmethod image-converter--probe ((type (eql imagemagick)))
  "Check whether the system has ImageMagick installed."
  (with-temp-buffer
    (let ((command (image-converter--value type :command))
          formats)
      ;; Can't check return value; ImageMagick convert usually returns
      ;; a non-zero result on "-list format".
      (apply #'call-process (car command) nil '(t nil) nil
             (append (cdr command) (image-converter--value type :probe)))
      (goto-char (point-min))
      (when (re-search-forward "^-" nil t)
        (forward-line 1)
        ;; Lines look like
        ;; "      WPG* r--   Word Perfect Graphics".
        (while (re-search-forward "^ *\\([A-Z0-9]+\\)\\*? +r" nil t)
          (push (downcase (match-string 1)) formats)))
      (nreverse formats))))

(cl-defmethod image-converter--probe ((type (eql ffmpeg)))
  "Check whether the system has ffmpeg installed."
  (with-temp-buffer
    (let ((command (image-converter--value type :command))
          formats)
      (when (zerop (apply #'call-process (car command) nil '(t nil) nil
                          (append (cdr command)
                                  (image-converter--value type :probe))))
        (goto-char (point-min))
        (when (re-search-forward "^ *-" nil t)
          (forward-line 1)
          ;; Lines look like
          ;; " V....D alias_pix            Alias/Wavefront PIX image"
          (while (re-search-forward "^ *V[^ ]+ +\\([a-z0-9_]+\\)" nil t)
            (push (match-string 1) formats)))
        (nreverse formats)))))

(defun image-converter--find-converter ()
  "Find an installed image converter."
  (dolist (elem image-converter--converters)
    (when-let ((formats (image-converter--probe (car elem))))
      (setq image-converter (car elem)
            image-converter-regexp (concat "\\." (regexp-opt formats) "\\'")))))

(cl-defmethod image-converter--convert ((type (eql graphicsmagick)) file)
  "Convert using GraphicsMagick."
  (image-converter--convert-magick type file))

(cl-defmethod image-converter--convert ((type (eql imagemagick)) file)
  "Convert using ImageMagick."
  (image-converter--convert-magick type file))

(defun image-converter--convert-magick (type file)
  (let ((command (image-converter--value type :command)))
    (unless (zerop (apply #'call-process (car command)
                          nil t nil
                          (append (cdr command)
                                  (list (expand-file-name file) "png:-"))))
      ;; If the command failed, hopefully the buffer contains the
      ;; error message.
      (buffer-string))))

(cl-defmethod image-converter--convert ((type (eql ffmpeg)) file)
  "Convert using ffmpeg."
  (let ((command (image-converter--value type :command)))
    (unless (zerop (apply #'call-process
                          (car command)
                          nil '(t nil) nil
                          (append (cdr command)
                                  (list "-i" (expand-file-name file)
                                        "-c:v" "png" "-f" "image2pipe" "-"))))
      "ffmpeg error when converting")))

(provide 'image-converter)

;;; image-converter.el ends here
