;;; image-tests.el --- tests for image.el -*- lexical-binding: t -*-

;; Copyright (C) 2019-2024 Free Software Foundation, Inc.

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
(require 'image)
(eval-when-compile
  (require 'cl-lib))

(defconst image-tests--emacs-images-directory
  (expand-file-name "images" data-directory)
  "Directory containing Emacs images.")

(defconst image-tests--files
  `((gif . ,(expand-file-name "test/data/image/black.gif"
                               source-directory))
    (jpeg . ,(expand-file-name "test/data/image/black.jpg"
                               source-directory))
    (pbm . ,(expand-file-name "splash.pbm"
                              image-tests--emacs-images-directory))
    (png . ,(expand-file-name "splash.png"
                              image-tests--emacs-images-directory))
    (svg . ,(expand-file-name "splash.svg"
                              image-tests--emacs-images-directory))
    (tiff . ,(expand-file-name
              "nextstep/GNUstep/Emacs.base/Resources/emacs.tiff"
              source-directory))
    (webp . ,(expand-file-name "test/data/image/black.webp"
                               source-directory))
    (xbm . ,(expand-file-name "gnus/gnus.xbm"
                              image-tests--emacs-images-directory))
    (xpm . ,(expand-file-name "splash.xpm"
                              image-tests--emacs-images-directory))))

(ert-deftest image--set-property ()
  "Test `image--set-property' behavior."
  (let ((image (list 'image)))
    ;; Add properties.
    (setf (image-property image :scale) 1)
    (should (equal image '(image :scale 1)))
    (setf (image-property image :width) 8)
    (should (equal image '(image :scale 1 :width 8)))
    (setf (image-property image :height) 16)
    (should (equal image '(image :scale 1 :width 8 :height 16)))
    ;; Delete properties.
    (setf (image-property image :type) nil)
    (should (equal image '(image :scale 1 :width 8 :height 16)))
    (setf (image-property image :scale) nil)
    (should (equal image '(image :width 8 :height 16)))
    (setf (image-property image :height) nil)
    (should (equal image '(image :width 8)))
    (setf (image-property image :width) nil)
    (should (equal image '(image)))))

(ert-deftest image-find-image ()
  (should (listp (find-image '((:type xpm :file "undo.xpm")))))
  (should (listp (find-image '((:type png :file "newsticker/rss-feed.png" :ascent center)))))
  (should-not (find-image '((:type png :file "does-not-exist-foo-bar.png")))))

(ert-deftest image-supported-file-p/built-in ()
  ;; (skip-unless (image-type-available-p 'pbm)) ; Always built-in
  (skip-unless (display-images-p))               ; (except in nox builds).
  (should (eq (image-supported-file-p "foo.pbm") 'pbm)))

(ert-deftest image-supported-file-p/optional ()
  (if (image-type-available-p 'jpeg)
      (should (eq (image-supported-file-p "foo.jpg") 'jpeg))
    (should-not (image-supported-file-p "foo.jpg"))))

(ert-deftest image-supported-file-p/unsupported-returns-nil ()
  (should-not (image-supported-file-p "foo.some-unsupported-format")))

(ert-deftest image-type-from-file-name ()
  (with-suppressed-warnings ((obsolete image-type-from-file-name))
    (should (eq (image-type-from-file-name "foo.jpg") 'jpeg))
    (should (eq (image-type-from-file-name "foo.png") 'png))
    (should (eq (image-type-from-file-name "foo.webp") 'webp))))

(ert-deftest image-type/from-filename ()
  ;; On emba, `image-types' and `image-load-path' do not exist.
  (skip-unless (and (bound-and-true-p image-types)
                    (bound-and-true-p image-load-path)
                    (image-type-available-p 'jpeg)))
  (should (eq (image-type "foo.jpg") 'jpeg)))

(defun image-tests--type-from-file-header (type)
  "Test image-type-from-file-header."
  (should (eq (if (image-type-available-p type) type)
              (image-type-from-file-header (cdr (assq type image-tests--files))))))

(ert-deftest image-type-from-file-header-test/gif ()
  (image-tests--type-from-file-header 'gif))

(ert-deftest image-type-from-file-header-test/jpeg ()
  (image-tests--type-from-file-header 'jpeg))

(ert-deftest image-type-from-file-header-test/pbm ()
  (image-tests--type-from-file-header 'pbm))

(ert-deftest image-type-from-file-header-test/png ()
  (image-tests--type-from-file-header 'png))

(ert-deftest image-type-from-file-header-test/svg ()
  (image-tests--type-from-file-header 'svg))

(ert-deftest image-type-from-file-header-test/tiff ()
  (image-tests--type-from-file-header 'tiff))

(ert-deftest image-type-from-file-header-test/webp ()
  (image-tests--type-from-file-header 'webp))

(ert-deftest image-type-from-file-header-test/xbm ()
  (image-tests--type-from-file-header 'xbm))

(ert-deftest image-type-from-file-header-test/xpm ()
  (image-tests--type-from-file-header 'xpm))

(ert-deftest image-rotate ()
  "Test `image-rotate'."
  (cl-letf* ((image (list 'image))
             ((symbol-function 'image--get-imagemagick-and-warn)
              (lambda () image)))
    (let ((current-prefix-arg '(4)))
      (call-interactively #'image-rotate))
    (should (equal image '(image :rotation 270.0)))
    (call-interactively #'image-rotate)
    (should (equal image '(image :rotation 0.0)))
    (image-rotate)
    (should (equal image '(image :rotation 90.0)))
    (image-rotate 0)
    (should (equal image '(image :rotation 90.0)))
    (image-rotate 1)
    (should (equal image '(image :rotation 91.0)))
    (image-rotate 1234.5)
    (should (equal image '(image :rotation 245.5)))
    (image-rotate -154.5)
    (should (equal image '(image :rotation 91.0)))))

;;; image-tests.el ends here
