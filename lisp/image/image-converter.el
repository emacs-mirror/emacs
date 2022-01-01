;;; image-converter.el --- Converting images from exotic formats -*- lexical-binding: t -*-

;; Copyright (C) 2019-2022 Free Software Foundation, Inc.

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
installed on the system.

The actual range of image formats that will be converted depends
on what image formats the chosen converter reports being able to
handle.  `auto-mode-alist' is then used to further filter what
formats that are to be supported: Only the suffixes that map to
`image-mode' will be handled."
  :group 'image
  :type 'symbol
  :version "27.1")

(defvar image-converter-regexp nil
  "A regexp that matches the file name suffixes that can be converted.")

(defvar image-converter-file-name-extensions nil
  "A list of file name suffixes that can be converted.")

(defvar image-converter--converters
  '((graphicsmagick :command ("gm" "convert") :probe ("-list" "format"))
    (ffmpeg :command "ffmpeg" :probe "-decoders")
    (imagemagick :command "convert" :probe ("-list" "format")))
  "List of supported image converters to try.")

(defun image-convert-p (source &optional data-p)
  "Return `image-convert' if SOURCE is an image that can be converted.
SOURCE can either be a file name or a string containing image
data.  In the latter case, DATA-P should be non-nil.  If DATA-P
is a string, it should be a MIME format string like
\"image/gif\"."
  ;; Find an installed image converter.
  (unless image-converter
    (image-converter--find-converter))
  ;; When image-converter was customized
  (when (and image-converter (not image-converter-regexp))
    (when-let ((formats (image-converter--probe image-converter)))
      (setq image-converter-regexp
            (concat "\\." (regexp-opt formats) "\\'"))
      (setq image-converter-file-name-extensions formats)))
  (and image-converter
       (or (and (not data-p)
                (string-match image-converter-regexp source))
           (and data-p
                (symbolp data-p)
                (string-search "/" (symbol-name data-p))
                (string-match
                 image-converter-regexp
                 (concat "foo." (image-converter--mime-type data-p)))))
       'image-convert))

(defun image-convert (image &optional image-format)
  "Convert IMAGE file to the PNG format.
IMAGE can either be a file name or image data.

To pass in image data, IMAGE should a string containing the image
data, and IMAGE-FORMAT should be a symbol with a MIME format name
like \"image/webp\".  For instance:

  (image-convert data-string 'image/bmp)

IMAGE can also be an image object as returned by `create-image'.

This function converts the image to PNG, and the converted image
data is returned as a string."
  ;; Find an installed image converter.
  (unless image-converter
    (image-converter--find-converter))
  (unless image-converter
    (error "No external image converters available"))
  (when (and image-format
             (not (= (length (split-string (symbol-name image-format) "/")) 2)))
    (error "IMAGE-FORMAT should be a symbol like `image/png'"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (when-let ((err (image-converter--convert
                     image-converter
                     (if (listp image)
                         (plist-get (cdr image) :file)
                       image)
                     (if (listp image)
                         (plist-get (cdr image) :data-p)
                       image-format))))
      (error "%s" err))
    (if (listp image)
        ;; Return an image object that's the same as we were passed,
        ;; but ignore the :type value.
        (apply #'create-image (buffer-string) 'png t
               (cl-loop for (key val) on (cdr image) by #'cddr
                        unless (eq key :type)
                        append (list key val)))
      (buffer-string))))

(defun image-converter--value (type elem)
  "Return the value of ELEM of image converter TYPE."
  (let ((value (plist-get (cdr (assq type image-converter--converters)) elem)))
    (if (stringp value)
        (list value)
      value)))

(cl-defmethod image-converter--probe ((type (eql 'graphicsmagick)))
  "Check whether the system has GraphicsMagick installed."
  (with-temp-buffer
    (let ((command (image-converter--value type :command))
          formats)
      (when (and (executable-find (car command))
                 (zerop (apply #'call-process (car command) nil '(t nil) nil
                               (append (cdr command)
                                       (image-converter--value type :probe)))))
        (goto-char (point-min))
        (when (re-search-forward "^-" nil t)
          (forward-line 1)
          ;; Lines look like
          ;; "   8BIM P  rw-  Photoshop resource format".
          (while (re-search-forward "^ *\\([A-Z0-9]+\\) +. +r" nil t)
            (push (downcase (match-string 1)) formats)))
        (nreverse formats)))))

(cl-defmethod image-converter--probe ((type (eql 'imagemagick)))
  "Check whether the system has ImageMagick installed."
  (with-temp-buffer
    (let ((command (image-converter--value type :command))
          formats)
      ;; Can't check return value; ImageMagick convert usually returns
      ;; a non-zero result on "-list format".
      (when (executable-find (car command))
        (apply #'call-process (car command) nil '(t nil) nil
               (append (cdr command) (image-converter--value type :probe))))
      (goto-char (point-min))
      (when (re-search-forward "^-" nil t)
        (forward-line 1)
        ;; Lines look like
        ;; "      WPG* r--   Word Perfect Graphics" or
        ;; "      WPG* WPG       r--   Word Perfect Graphics".
        (while (re-search-forward "^ *\\([A-Z0-9]+\\)\\*?\\(?: +[A-Z0-9]+\\)? +r" nil t)
          (push (downcase (match-string 1)) formats)))
      (nreverse formats))))

(cl-defmethod image-converter--probe ((type (eql 'ffmpeg)))
  "Check whether the system has ffmpeg installed."
  (with-temp-buffer
    (let ((command (image-converter--value type :command))
          formats)
      (when (and (executable-find (car command))
                 (zerop (apply #'call-process (car command) nil '(t nil) nil
                               (append (cdr command)
                                       (image-converter--value type :probe)))))
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
  (catch 'done
    (dolist (elem image-converter--converters)
      (when-let ((formats (image-converter--filter-formats
                           (image-converter--probe (car elem)))))
        (setq image-converter (car elem)
              image-converter-regexp (concat "\\." (regexp-opt formats) "\\'")
              image-converter-file-name-extensions formats)
        (throw 'done image-converter)))))

(defun image-converter--filter-formats (suffixes)
  "Filter SUFFIXES based on `auto-mode-alist'.
Only suffixes that map to `image-mode' are returned."
  (cl-loop with case-fold-search = (if (not auto-mode-case-fold)
                                       nil
                                     t)
           for suffix in suffixes
           when (eq (cdr (assoc (concat "foo." suffix) auto-mode-alist
                                #'string-match))
                    'image-mode)
           collect suffix))

(cl-defmethod image-converter--convert ((type (eql 'graphicsmagick)) source
                                        image-format)
  "Convert using GraphicsMagick."
  (image-converter--convert-magick type source image-format))

(cl-defmethod image-converter--convert ((type (eql 'imagemagick)) source
                                        image-format)
  "Convert using ImageMagick."
  (image-converter--convert-magick type source image-format))

(defun image-converter--mime-type (image-format)
  (and (symbolp image-format)
       (cadr (split-string (symbol-name image-format) "/"))))

(defun image-converter--convert-magick (type source image-format)
  (let ((command (image-converter--value type :command)))
    (unless (zerop (if image-format
                       ;; We have the image data in SOURCE.
                       (progn
                         (insert source)
                         (apply #'call-process-region (point-min) (point-max)
                                (car command) t t nil
                                (append
                                 (cdr command)
                                 (list (format "%s:-"
                                               (image-converter--mime-type
                                                image-format))
                                       "png:-"))))
                     ;; SOURCE is a file name.
                     (apply #'call-process (car command)
                            nil t nil
                            (append (cdr command)
                                    (list (expand-file-name source) "png:-")))))
      ;; If the command failed, hopefully the buffer contains the
      ;; error message.
      (buffer-string))))

(cl-defmethod image-converter--convert ((type (eql 'ffmpeg)) source
                                        image-format)
  "Convert using ffmpeg."
  (let ((command (image-converter--value type :command)))
    (unless (zerop (if image-format
                       (progn
                         (insert source)
                         (apply #'call-process-region
                                (point-min) (point-max) (car command)
                                t '(t nil) nil
                                (append
                                 (cdr command)
                                 (list "-i" "-"
                                       "-c:v" "png"
                                       "-f" "image2pipe" "-"))))
                     (apply #'call-process
                            (car command)
                            nil '(t nil) nil
                            (append (cdr command)
                                    (list "-i" (expand-file-name source)
                                          "-c:v" "png" "-f" "image2pipe"
                                          "-")))))
      "ffmpeg error when converting")))

(provide 'image-converter)

;;; image-converter.el ends here
