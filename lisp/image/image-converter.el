;;; image-converter.el --- Converting images from exotic formats -*- lexical-binding: t -*-

;; Copyright (C) 2019-2026 Free Software Foundation, Inc.

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
on the image formats which the chosen converter is able to
handle.  `auto-mode-alist' is then used to further filter the
formats that are to be supported: only the suffixes that map to
`image-mode' will be handled."
  :group 'image
  :type 'symbol
  :version "27.1")

(defcustom image-convert-to-format "png"
  "The image format to convert to.
This should be a string like \"png\" or \"ppm\", or some
other (preferably lossless) format that Emacs understands
natively.  The converter chosen has to support this format; if
not, the conversion will fail."
  :group 'image
  :version "29.1"
  :type 'string)

(defvar image-converter-regexp nil
  "A regexp that matches the file name suffixes which can be converted.")

(defvar image-converter-file-name-extensions nil
  "A list of file name suffixes that can be converted.")

(defvar image-converter--converters
  '((graphicsmagick :command ("gm" "convert") :probe ("-list" "format"))
    (ffmpeg :command "ffmpeg" :probe "-decoders")
    ;; "-layers merge" flattens visible layers in e.g. Gimp XCF files.
    (imagemagick :command ("convert" "-layers" "merge") :probe ("-list" "format")))
  "List of supported image converters to try and required command-line switches.")

(defvar image-converter--extra-converters (make-hash-table :test #'equal))

(defun image-converter-initialize ()
  "Determine the external image converter to be used.
This also determines which external formats we can parse."
  (unless image-converter
    (image-converter--find-converter)))

(defun image-convert-p (source &optional data-p)
  "Return `image-convert' if SOURCE is an image that can be converted.
SOURCE can either be a file name or a string containing image
data.  In the latter case, DATA-P should be non-nil.  If DATA-P
is a string, it should be a MIME format string specifying the image type,
like \"image/gif\"."
  (image-converter-initialize)
  ;; When image-converter was customized
  (when (and image-converter (not image-converter-regexp))
    (when-let* ((formats (image-converter--probe image-converter)))
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
  "Convert IMAGE to an image format which Emacs understands.
This will usually be \"png\", but is controlled by the value
of the `image-convert-to-format' user option.

IMAGE can either be a file name, an image object returned
by `create-image', or a string with image data.  In the latter
case, IMAGE-FORMAT should be a symbol whose name is a MIME
specification of image format, such as \"image/webp\".
For instance:

  (image-convert data-string \\='image/bmp)

This function converts the image to the preferred format, per
the value of `image-convert-to-format', and returns the
converted image data as a string."
  (image-converter-initialize)
  (unless image-converter
    (error "No external image converters available"))
  (when (and image-format
             (not (= (length (split-string (symbol-name image-format) "/")) 2)))
    (error "IMAGE-FORMAT should be a symbol like `image/png'"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let* ((source (if (listp image)
                       (plist-get (cdr image) :file)
                     image))
           (format (if (listp image)
                       (plist-get (cdr image) :data-p)
                     image-format))
           (type (if format
                     (image-converter--mime-type format)
                   (file-name-extension source)))
           (extra-converter (gethash type image-converter--extra-converters)))
      (if extra-converter
          (funcall extra-converter source format)
        (when-let* ((err (image-converter--convert
                          image-converter source format)))
          (error "%s" err))))
    (if (listp image)
        ;; Return an image object that's the same as we were passed,
        ;; but ignore the :type value.
        (apply #'create-image (buffer-string)
               (intern image-convert-to-format)
               t
               (cl-loop for (key val) on (cdr image) by #'cddr
                        unless (eq key :type)
                        append (list key val)))
      (buffer-string))))

(defun image-converter--value (type elem)
  "Return the value of property ELEM for image converter TYPE."
  (let ((value (plist-get (cdr (assq type image-converter--converters)) elem)))
    (if (stringp value)
        (list value)
      value)))

(cl-defmethod image-converter--probe ((type (eql 'graphicsmagick)))
  "Check whether the system has GraphicsMagick installed that's usable converter."
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
  "Check whether the system has ImageMagick installed that's a usable converter."
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
  "Check whether the system has ffmpeg installed that's a usable converter."
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
  "Find an installed image converter Emacs can use."
  (catch 'done
    (dolist (elem image-converter--converters)
      (when-let* ((formats (image-converter--filter-formats
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
  "Convert image in SOURCE using GraphicsMagick."
  (image-converter--convert-magick type source image-format))

(cl-defmethod image-converter--convert ((type (eql 'imagemagick)) source
                                        image-format)
  "Convert image in SOURCE using ImageMagick."
  (image-converter--convert-magick type source image-format))

(defun image-converter--mime-type (image-format)
  (and (symbolp image-format)
       (cadr (split-string (symbol-name image-format) "/"))))

(defun image-converter--convert-magick (type source image-format)
  (let ((command (image-converter--value type :command))
        (coding-system-for-read 'no-conversion))
    (unless (zerop (if image-format
                       ;; We have the image data in SOURCE.
                       (progn
                         (insert source)
                         (let ((coding-system-for-write 'no-conversion))
                           (apply #'call-process-region (point-min) (point-max)
                                  (car command) t t nil
                                  (append
                                   (cdr command)
                                   (list (format "%s:-"
                                                 (image-converter--mime-type
                                                  image-format))
                                       (concat image-convert-to-format
                                               ":-"))))))
                     ;; SOURCE is a file name.
                     (apply #'call-process (car command)
                            nil t nil
                            (append (cdr command)
                                    (list (expand-file-name source)
                                          (concat image-convert-to-format
                                                  ":-"))))))
      ;; If the command failed, hopefully the buffer contains the
      ;; error message.
      (buffer-string))))

(cl-defmethod image-converter--convert ((type (eql 'ffmpeg)) source
                                        image-format)
  "Convert image in SOURCE using ffmpeg."
  (let ((command (image-converter--value type :command))
        (coding-system-for-read 'no-conversion))
    (unless (zerop (if image-format
                       (progn
                         (insert source)
                         (let ((coding-system-for-write 'no-conversion))
                           (apply #'call-process-region
                                  (point-min) (point-max) (car command)
                                  t '(t nil) nil
                                  (append
                                   (cdr command)
                                   (list "-i" "-"
                                         "-c:v" image-convert-to-format
                                         "-f" "image2pipe" "-")))))
                     (apply #'call-process
                            (car command)
                            nil '(t nil) nil
                            (append (cdr command)
                                    (list "-i" (expand-file-name source)
                                          "-c:v" image-convert-to-format
                                          "-f" "image2pipe"
                                          "-")))))
      "ffmpeg error when converting")))

;;;###autoload
(defun image-converter-add-handler (suffix converter)
  "Make Emacs use CONVERTER to parse image files whose names end with SUFFIX.
CONVERTER is a function with two arguments, the file name or a string
with the image data, and a non-nil value if the first argument is image data.
The converter should produce the image in the current buffer, converted to
the format given by `image-convert-to-format'.
SUFFIX should not include the leading dot."
  (cl-pushnew suffix image-converter-file-name-extensions :test #'equal)
  (setq image-converter-file-name-extensions
        (sort image-converter-file-name-extensions #'string<))
  (setq image-converter-regexp
        (concat "\\." (regexp-opt image-converter-file-name-extensions) "\\'"))
  (setf (gethash suffix image-converter--extra-converters) converter))

(provide 'image-converter)

;;; image-converter.el ends here
