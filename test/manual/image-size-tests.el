;;; image-size-tests.el -- tests for image scaling

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; To test: Load the file and eval (image-size-tests).
;; A non-erroring result is a success.

;;; Code:

(defmacro im-should (width height &rest props)
  `(unless (im-compare (im-image ,@props) ,width ,height)
     (error "%s didn't succeed" ',props)))

(defun im-image (&rest props)
  (let ((image-scaling-factor 1))
    (apply
     #'create-image
     (expand-file-name "test/data/image/blank-200x100.png" source-directory)
     'imagemagick nil props)))

(defun im-compare (image width height)
  (let ((size (image-size image t)))
    (and (= (car size) width)
         (= (cdr size) height))))

(defun image-size-tests ()
  (unless (imagemagick-types)
    (error "This only makes sense if ImageMagick is installed"))
  ;; Default sizes.
  (im-should 200 100)
  ;; Changing one dimension changes the other.
  (im-should 100 50 :width 100)
  (im-should 100 50 :height 50)
  ;; The same with :max-width etc.
  (im-should 100 50 :max-width 100)
  (im-should 100 50 :max-height 50)
  ;; :width wins over :max-width etc
  (im-should 300 150 :width 300 :max-width 100)
  (im-should 400 200 :height 200 :max-height 100)
  ;; Specifying both width and height is fine.
  (im-should 300 50 :width 300 :height 50)
  ;; A too-large :max-width (etc) has no effect.
  (im-should 200 100 :max-width 300)
  (im-should 200 100 :max-height 300)
  ;; Both max-width/height.
  (im-should 100 50 :max-width 100 :max-height 75)
  (im-should 50 25 :max-width 100 :max-height 25))

;;; image-size-tests.el ends here
