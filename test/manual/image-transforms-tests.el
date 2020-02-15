;;; image-transform-tests.el --- Test suite for image transforms.

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Alan Third <alan@idiocy.org>
;; Keywords:       internal
;; Human-Keywords: internal

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

;; Type M-x test-transforms RET to generate the test buffer.

;;; Code:

(defun test-rotation ()
  (let ((up "<svg height='9' width='9'><polygon points='0,8 4,0 8,8'/></svg>")
        (down "<svg height='9' width='9'><polygon points='0,0 4,8 8,0'/></svg>")
        (left "<svg height='9' width='9'><polygon points='8,0 0,4 8,8'/></svg>")
        (right "<svg height='9' width='9'><polygon points='0,0 8,4 0,8'/></svg>"))
    (insert-header "Test Rotation: rotating an image")
    (insert-test "0" up up '(:rotation 0))
    (insert-test "360" up up '(:rotation 360))
    (insert-test "180" down up '(:rotation 180))
    (insert-test "-90" left up '(:rotation -90))
    (insert-test "90" right up '(:rotation 90))
    (insert-test "90.0" right up '(:rotation 90.0))

    ;; This should log a message and display the unrotated image.
    (insert-test "45" up up '(:rotation 45)))
  (insert "\n\n"))

(defun test-cropping ()
  (let ((image "<svg height='30' width='30'>
                  <rect x='0' y='0' width='10' height='10'/>
                  <rect x='10' y='10' width='10' height='10'
                        style='fill:none;stroke-width:1;stroke:#000'/>
                  <line x1='10' y1='10' x2='20' y2='20' style='stroke:#000'/>
                  <line x1='20' y1='10' x2='10' y2='20' style='stroke:#000'/>
                  <rect x='20' y='20' width='10' height='10'
                        style='fill:none;stroke-width:1;stroke:#000'/>
                </svg>")
        (top-left "<svg height='10' width='10'>
                     <rect x='0' y='0' width='10' height='10'/>
                   </svg>")
        (middle "<svg height='10' width='10'>
                   <rect x='0' y='0' width='10' height='10'
                         style='fill:none;stroke-width:1;stroke:#000'/>
                   <line x1='0' y1='0' x2='10' y2='10' style='stroke:#000'/>
                   <line x1='10' y1='0' x2='0' y2='10' style='stroke:#000'/>
                 </svg>")
        (bottom-right "<svg height='10' width='10'>
                         <rect x='0' y='0' width='10' height='10'
                               style='fill:none;stroke-width:1;stroke:#000'/>
                       </svg>"))
    (insert-header "Test Crop: cropping an image (only works with ImageMagick)")
    (insert-test "all params" top-left image '(:crop (10 10 0 0)))
    (insert-test "width/height only" middle image '(:crop (10 10)))
    (insert-test "negative x y" middle image '(:crop (10 10 -10 -10)))
    (insert-test "all params" bottom-right image '(:crop (10 10 20 20))))
  (insert "\n\n"))

(defun test-scaling ()
  (let ((image "<svg height='10' width='10'>
                  <rect x='0' y='0' width='10' height='10'
                        style='fill:none;stroke-width:1;stroke:#000'/>
                  <line x1='0' y1='0' x2='10' y2='10' style='stroke:#000'/>
                  <line x1='10' y1='0' x2='0' y2='10' style='stroke:#000'/>
                </svg>")
        (large "<svg height='20' width='20'>
                  <rect x='0' y='0' width='20' height='20'
                        style='fill:none;stroke-width:2;stroke:#000'/>
                  <line x1='0' y1='0' x2='20' y2='20'
                        style='stroke-width:2;stroke:#000'/>
                  <line x1='20' y1='0' x2='0' y2='20'
                        style='stroke-width:2;stroke:#000'/>
                </svg>")
        (small "<svg height='5' width='5'>
                  <rect x='0' y='0' width='4' height='4'
                        style='fill:none;stroke-width:1;stroke:#000'/>
                  <line x1='0' y1='0' x2='4' y2='4' style='stroke:#000'/>
                  <line x1='4' y1='0' x2='0' y2='4' style='stroke:#000'/>
                </svg>"))
    (insert-header "Test Scaling: resize an image (pixelization may occur)")
    (insert-test "1x" image image '(:scale 1))
    (insert-test "2x" large image '(:scale 2))
    (insert-test "0.5x" image large '(:scale 0.5))
    (insert-test ":max-width" image large '(:max-width 10))
    (insert-test ":max-height" image large '(:max-height 10))
    (insert-test "width, height" image large '(:width 10 :height 10)))
  (insert "\n\n"))

(defun test-scaling-rotation ()
  (let ((image "<svg height='20' width='20'>
                  <rect x='0' y='0' width='20' height='20'
                        style='fill:none;stroke-width:1;stroke:#000'/>
                  <rect x='0' y='0' width='10' height='10'
                        style='fill:#000'/>
                </svg>")
        (x2-90 "<svg height='40' width='40'>
                  <rect x='0' y='0' width='40' height='40'
                        style='fill:none;stroke-width:1;stroke:#000'/>
                  <rect x='20' y='0' width='20' height='20'
                        style='fill:#000'/>
                </svg>")
        (x2--90 "<svg height='40' width='40'>
                   <rect x='0' y='0' width='40' height='40'
                         style='fill:none;stroke-width:1;stroke:#000'/>
                   <rect x='0' y='20' width='20' height='20'
                         style='fill:#000'/>
                 </svg>")
        (x0.5-180 "<svg height='10' width='10'>
                     <rect x='0' y='0' width='10' height='10'
                           style='fill:none;stroke-width:1;stroke:#000'/>
                     <rect x='5' y='5' width='5' height='5'
                           style='fill:#000'/>
                   </svg>"))
    (insert-header "Test Scaling and Rotation: resize and rotate an image (pixelization may occur)")
    (insert-test "1x, 0 degrees" image image '(:scale 1 :rotation 0))
    (insert-test "2x, 90 degrees" x2-90 image '(:scale 2 :rotation 90.0))
    (insert-test "2x, -90 degrees" x2--90 image '(:scale 2 :rotation -90.0))
    (insert-test "0.5x, 180 degrees" x0.5-180 image '(:scale 0.5 :rotation 180.0)))
  (insert "\n\n"))

(defun insert-header (description)
  (insert description)
  (insert "\n")
  (indent-to 38)
  (insert "expected")
  (indent-to 48)
  (insert "result")
  (when (fboundp #'imagemagick-types)
    (indent-to 58)
    (insert "ImageMagick"))
  (insert "\n"))

(defun insert-test (description expected image params)
  (indent-to 2)
  (insert description)
  (indent-to 40)
  (insert-image (create-image expected 'svg t))
  (indent-to 50)
  (insert-image (apply #'create-image image 'svg t params))
  (when (fboundp #'imagemagick-types)
    (indent-to 60)
    (insert-image (apply #'create-image image 'imagemagick t params)))
  (insert "\n"))

(defun test-transforms ()
  (interactive)
  (let ((buf (get-buffer "*Image Transform Test*")))
    (if buf
	(kill-buffer buf))
    (switch-to-buffer (get-buffer-create "*Image Transform Test*"))
    (erase-buffer)
    (unless #'imagemagick-types
      (insert "ImageMagick not detected.  ImageMagick tests will be skipped.\n\n"))
    (test-rotation)
    (test-cropping)
    (test-scaling)
    (test-scaling-rotation)
    (goto-char (point-min))))
