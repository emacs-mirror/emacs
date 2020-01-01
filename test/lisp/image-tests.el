;;; image-tests.el --- tests for image.el -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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
  (expand-file-name "../etc/images" (getenv "EMACS_TEST_DIRECTORY"))
  "Directory containing Emacs images.")

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

(ert-deftest image-type-from-file-header-test ()
  "Test image-type-from-file-header."
  (should (eq (if (image-type-available-p 'svg) 'svg)
	      (image-type-from-file-header
	       (expand-file-name "splash.svg"
				 image-tests--emacs-images-directory)))))

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
