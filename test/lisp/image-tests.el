;;; image-tests.el --- tests for image.el -*- lexical-binding: t -*-

;; Copyright (C) 2019-2026 Free Software Foundation, Inc.

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
  (cond ((image-type-available-p 'jpeg)
         (should (eq (image-supported-file-p "foo.jpg") 'jpeg)))
        ((fboundp 'imagemagick-types)
         (should (eq (image-supported-file-p "foo.jpg") 'imagemagick)))
        (nil
         (should-not (image-supported-file-p "foo.jpg")))))

(ert-deftest image-supported-file-p/unsupported-returns-nil ()
  (should-not (image-supported-file-p "foo.some-unsupported-format")))

(ert-deftest image-type-from-file-name ()
  (skip-unless (and (image-type-available-p 'jpeg)
                    (image-type-available-p 'png)
                    (image-type-available-p 'webp)))
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

;;;; Transforming maps

(ert-deftest image-create-image-with-map ()
  "Test that `create-image' correctly adds :map and/or :original-map."
  (skip-unless (display-images-p))
  (let ((data "<svg width=\"30\" height=\"30\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"></svg>")
        (map '(((circle (1 .  1) .  1) a)))
        (original-map '(((circle (2 .  2) .  2) a)))
        (original-map-other '(((circle (3 . 3) . 3) a))))
    ;; Generate :original-map from :map.
    (let* ((image (create-image data 'svg t :map map :scale 0.5))
           (got-original-map (image-property image :original-map)))
      (should (equal got-original-map original-map)))
    ;; Generate :map from :original-map.
    (let* ((image (create-image
                   data 'svg t :original-map original-map :scale 0.5))
           (got-map (image-property image :map)))
      (should (equal got-map map)))
    ;; Use :original-map if both it and :map are specified.
    (let* ((image (create-image
                   data 'svg t :map map
                   :original-map original-map-other :scale 0.5))
           (got-original-map (image-property image :original-map)))
      (should (equal got-original-map original-map-other)))))

(defun image-tests--map-equal (a b &optional tolerance)
  "Return t if maps A and B have the same coordinates within TOLERANCE.
Since image sizes calculations vary on different machines, this function
allows for each image map coordinate in A to be within TOLERANCE to the
corresponding coordinate in B.  When nil, TOLERANCE defaults to 5."
  (unless tolerance (setq tolerance 5))
  (catch 'different
    (cl-labels ((check-tolerance
                  (coord-a coord-b)
                  (unless (>= tolerance (abs (- coord-a coord-b)))
                    (throw 'different nil))))
      (dotimes (i (length a))
        (pcase-let ((`((,type-a . ,coords-a) ,_id ,_plist) (nth i a))
                    (`((,type-b . ,coords-b) ,_id ,_plist) (nth i b)))
          (unless (eq type-a type-b)
            (throw 'different nil))
          (pcase-exhaustive type-a
            ('rect
             (check-tolerance (caar coords-a) (caar coords-b))
             (check-tolerance (cdar coords-a) (cdar coords-b))
             (check-tolerance (cadr coords-a) (cadr coords-b))
             (check-tolerance (cddr coords-a) (cddr coords-b)))
            ('circle
             (check-tolerance (caar coords-a) (caar coords-b))
             (check-tolerance (cdar coords-a) (cdar coords-b))
             (check-tolerance (cdar coords-a) (cdar coords-b)))
            ('poly
             (dotimes (i (length coords-a))
               (check-tolerance (aref coords-a i) (aref coords-b i))))))))
    t))

(ert-deftest image--compute-map-and-original-map ()
  "Test `image--compute-map' and `image--compute-original-map'."
  (skip-unless (display-images-p))
  (let* ((svg-string "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><svg width=\"125pt\" height=\"116pt\" viewBox=\"0.00 0.00 125.00 116.00\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><g transform=\"scale(1 1) rotate(0) translate(4 112)\"><polygon fill=\"white\" stroke=\"transparent\" points=\"-4,4 -4,-112 121,-112 121,4 -4,4\"/><a xlink:href=\"a\"><ellipse fill=\"none\" stroke=\"black\" cx=\"27\" cy=\"-90\" rx=\"18\" ry=\"18\"/><text text-anchor=\"middle\" x=\"27\" y=\"-86.3\" fill=\"#000000\">A</text></a><a xlink:href=\"b\"><polygon fill=\"none\" stroke=\"black\" points=\"54,-36 0,-36 0,0 54,0 54,-36\"/><text text-anchor=\"middle\" x=\"27\" y=\"-14.3\" fill=\"#000000\">B</text></a><a xlink:href=\"c\"><ellipse fill=\"none\" stroke=\"black\" cx=\"90\" cy=\"-90\" rx=\"27\" ry=\"18\"/><text text-anchor=\"middle\" x=\"90\" y=\"-86.3\" fill=\"#000000\">C</text></a></g></svg>")
         (original-map
          '(((circle (41 . 29) . 24) "a" (help-echo "A"))
            ((rect (5 . 101) 77 . 149) "b" (help-echo "B"))
            ((poly . [161 29 160 22 154 15 146 10 136 7 125 5 114 7 104 10 96 15 91 22 89 29 91 37 96 43 104 49 114 52 125 53 136 52 146 49 154 43 160 37]) "c" (help-echo "C"))))
         (scaled-map
          '(((circle (82 . 58) . 48) "a" (help-echo "A"))
            ((rect (10 . 202) 154 . 298) "b" (help-echo "B"))
            ((poly . [322 58 320 44 308 30 292 20 272 14 250 10 228 14 208 20 192 30 182 44 178 58 182 74 192 86 208 98 228 104 250 106 272 104 292 98 308 86 320 74]) "c" (help-echo "C"))))
         (flipped-map
          '(((circle (125 . 29) . 24) "a" (help-echo "A"))
            ((rect (89 . 101) 161 . 149) "b" (help-echo "B"))
            ((poly . [5 29 6 22 12 15 20 10 30 7 41 5 52 7 62 10 70 15 75 22 77 29 75 37 70 43 62 49 52 52 41 53 30 52 20 49 12 43 6 37]) "c" (help-echo "C"))))
         (rotated-map
          '(((circle (126 . 41) . 24) "a" (help-echo "A"))
            ((rect (6 . 5) 54 . 77) "b" (help-echo "B"))
            ((poly . [126 161 133 160 140 154 145 146 148 136 150 125 148 114 145 104 140 96 133 91 126 89 118 91 112 96 106 104 103 114 102 125 103 136 106 146 112 154 118 160]) "c" (help-echo "C"))))
         (scaled-rotated-flipped-map
          '(((circle (58 . 82) . 48) "a" (help-echo "A"))
            ((rect (202 . 10) 298 . 154) "b" (help-echo "B"))
            ((poly . [58 322 44 320 30 308 20 292 14 272 10 250 14 228 20 208 30 192 44 182 58 178 74 182 86 192 98 208 104 228 106 250 104 272 98 292 86 308 74 320]) "c" (help-echo "C"))))
         (image (create-image svg-string 'svg t :map scaled-rotated-flipped-map
                              :scale 2 :rotation 90 :flip t)))
    ;; Test that `image--compute-original-map' correctly generates
    ;; original-map when creating an already transformed image.
    (should (image-tests--map-equal (image-property image :original-map)
                                    original-map))
    (setf (image-property image :flip) nil)
    (setf (image-property image :rotation) 0)
    (setf (image-property image :scale) 2)
    (should (image-tests--map-equal (image--compute-map image)
                                    scaled-map))
    (setf (image-property image :scale) 1)
    (setf (image-property image :rotation) 90)
    (should (image-tests--map-equal (image--compute-map image)
                                    rotated-map))
    (setf (image-property image :rotation) 0)
    (setf (image-property image :flip) t)
    (should (image-tests--map-equal (image--compute-map image)
                                    flipped-map))
    (setf (image-property image :scale) 2)
    (setf (image-property image :rotation) 90)
    (should (image-tests--map-equal (image--compute-map image)
                                    scaled-rotated-flipped-map))

    ;; Uncomment to test manually by interactively transforming the
    ;; image and checking the map boundaries by hovering them.

    ;; (with-current-buffer (get-buffer-create "*test image map*")
    ;;   (erase-buffer)
    ;;   (insert-image image)
    ;;   (goto-char (point-min))
    ;;   (pop-to-buffer (current-buffer)))
    ))

(ert-deftest image-transform-map ()
  "Test functions related to transforming image maps."
  (let ((map '(((circle (4 . 3) . 2) "circle")
               ((rect (3 . 6) 8 . 8) "rect")
               ((poly . [6 11 7 13 2 14]) "poly")))
        (width 10)
        (height 15))
    (should (equal (image--scale-map (copy-tree map t) 2)
                   '(((circle (8 . 6) . 4) "circle")
                     ((rect (6 . 12) 16 . 16) "rect")
                     ((poly . [12 22 14 26 4 28]) "poly"))))
    (should (equal (image--rotate-map (copy-tree map t) 90 `(,width . ,height))
                   '(((circle (12 . 4) . 2) "circle")
                     ((rect (7 . 3) 9 . 8) "rect")
                     ((poly . [4 6 2 7 1 2]) "poly"))))
    (should (equal (image--flip-map (copy-tree map t) `(,width . ,height))
                   '(((circle (6 . 3) . 2) "circle")
                     ((rect (2 . 6) 7 . 8) "rect")
                     ((poly . [4 11 3 13 8 14]) "poly"))))
    (let ((copy (copy-tree map t)))
      (image--scale-map copy 2)
      ;; Scale size because the map has been scaled.
      (image--rotate-map copy 90 `(,(* 2 width) . ,(* 2 height)))
      ;; Swap width and height because the map has been flipped.
      (image--flip-map copy `(,(* 2 height) . ,(* 2 width)))
      (should (equal copy
                     '(((circle (6 . 8) . 4) "circle")
                       ((rect (12 . 6) 16 . 16) "rect")
                       ((poly . [22 12 26 14 28 4]) "poly")))))))

;;; image-tests.el ends here
