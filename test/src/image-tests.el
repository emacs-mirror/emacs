;;; image-tests.el --- Tests for image.c  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefankangas@gmail.com>

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

;;; Code:

(require 'ert)

(declare-function image-size "image.c" (spec &optional pixels frame))
(declare-function image-mask-p "image.c" (spec &optional frame))
(declare-function image-metadata "image.c" (spec &optional frame))

(defconst image-tests--images
  `((gif . ,(expand-file-name "test/data/image/black.gif"
                              source-directory))
    (jpeg . ,(expand-file-name "test/data/image/black.jpg"
                               source-directory))
    (pbm . ,(find-image '((:file "splash.svg" :type svg))))
    (png . ,(find-image '((:file "splash.png" :type png))))
    (svg . ,(find-image '((:file "splash.pbm" :type pbm))))
    (tiff . ,(expand-file-name
              "nextstep/GNUstep/Emacs.base/Resources/emacs.tiff"
              source-directory))
    (webp . ,(expand-file-name "test/data/image/black.webp"
                               source-directory))
    (xbm . ,(find-image '((:file "gnus/gnus.xbm" :type xbm))))
    (xpm . ,(find-image '((:file "splash.xpm" :type xpm))))))

(ert-deftest image-tests-image-size/error-on-nongraphical-display ()
  (skip-unless (not (display-images-p)))
  (should-error (image-size 'invalid-spec)))

(ert-deftest image-tests-image-mask-p/error-on-nongraphical-display ()
  (skip-unless (not (display-images-p)))
  (should-error (image-mask-p (cdr (assq 'xpm image-tests--images)))))

(ert-deftest image-tests-image-metadata/error-on-nongraphical-display ()
  (skip-unless (not (display-images-p)))
  (should-error (image-metadata (cdr (assq 'xpm image-tests--images)))))

(ert-deftest image-tests-imagemagick-types ()
  (skip-unless (fboundp 'imagemagick-types))
  (when (fboundp 'imagemagick-types)
    (should (listp (imagemagick-types)))))

(ert-deftest image-tests-init-image-library ()
  (skip-unless (fboundp 'init-image-library))
  (declare-function init-image-library "image.c" (type))
  (should (init-image-library 'pbm)) ; built-in
  (should-not (init-image-library 'invalid-image-type)))

;;; image-tests.el ends here
