;;; image-size-tests.el -- tests for image scaling

;; Copyright (C) 2017-2020 Free Software Foundation, Inc.

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; To test: Load the file and eval (image-size-tests).
;; A non-erroring result is a success.

;;; Code:

(defmacro im-should (image width height &rest props)
  `(let ((im (im-image ,image ,@props)))
     (unless (im-compare im ,width ,height)
       (error "%s %s didn't succeed; size is %s"
              ',image ',props (image-size im t)))))

(defun im-image (type &rest props)
  (let ((image-scaling-factor 1))
    (apply
     #'create-image
     (expand-file-name
      (if (eq type :w)
          "test/data/image/blank-200x100.png"
          "test/data/image/blank-100x200.png")
      source-directory)
     'imagemagick nil props)))

(defun im-compare (image width height)
  (let ((size (image-size image t)))
    (and (= (car size) width)
         (= (cdr size) height))))

(defun image-size-tests ()
  (unless (imagemagick-types)
    (error "This only makes sense if ImageMagick is installed"))
  ;; Test the image that's wider than it is tall.
  ;; Default sizes.
  (im-should :w 200 100)
  ;; Changing one dimension changes the other.
  (im-should :w 100 50 :width 100)
  (im-should :w 100 50 :height 50)
  ;; The same with :max-width etc.
  (im-should :w 100 50 :max-width 100)
  (im-should :w 100 50 :max-height 50)
  ;; :width wins over :max-width etc
  (im-should :w 300 150 :width 300 :max-width 100)
  (im-should :w 400 200 :height 200 :max-height 100)
  ;; Specifying both width and height is fine.
  (im-should :w 300 50 :width 300 :height 50)
  ;; A too-large :max-width (etc) has no effect.
  (im-should :w 200 100 :max-width 300)
  (im-should :w 200 100 :max-height 300)
  ;; Both max-width/height.
  (im-should :w 100 50 :max-width 100 :max-height 75)
  (im-should :w 50 25 :max-width 100 :max-height 25)
  ;; :width and :max-height (max-height wins).
  (im-should :w 400 200 :width 400 :max-height 200)
  (im-should :w 400 200 :width 500 :max-height 200)

  ;; Test the image that's taller than it is wide.
  (im-should :h 100 200)
  ;; Changing one dimension changes the other.
  (im-should :h 50 100 :width 50)
  (im-should :h 50 100 :height 100)
  ;; The same with :max-width etc.
  (im-should :h 50 100 :max-width 50)
  (im-should :h 50 100 :max-height 100)
  ;; :width wins over :max-width etc
  (im-should :h 300 600 :width 300 :max-width 100)
  (im-should :h 150 300 :height 300 :max-height 100)
  ;; Specifying both width and height is fine.
  (im-should :h 300 50 :width 300 :height 50)
  ;; A too-large :max-width (etc) has no effect.
  (im-should :h 100 200 :max-width 300)
  (im-should :h 100 200 :max-height 300)
  ;; Both max-width/height.
  (im-should :h 50 100 :max-width 75 :max-height 100)
  (im-should :h 25 50 :max-width 25 :max-height 100)
  ;; :height and :max-width (max-width wins).
  (im-should :h 200 400 :height 400 :max-width 200)
  (im-should :h 200 400 :height 500 :max-width 200)
  )

;;; image-size-tests.el ends here
