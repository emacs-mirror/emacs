;;; lcms-tests.el --- tests for Little CMS interface -*- lexical-binding: t -*-

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org

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

;; Some reference values computed using the colorspacious python
;; library, assimilated from its test suite, or adopted from its
;; aggregation of gold values.
;; See https://colorspacious.readthedocs.io/en/v1.1.0/ and
;; https://github.com/njsmith/colorspacious

;; Other references:
;; http://www.babelcolor.com/index_htm_files/A%20review%20of%20RGB%20color%20spaces.pdf

;;; Code:

(require 'ert)
(require 'color)

(defconst lcms-colorspacious-d65 '(0.95047 1.0 1.08883)
  "D65 white point from colorspacious.")

(defun lcms-approx-p (a b &optional delta)
  "Check if A and B are within relative error DELTA of one another.
B is considered the exact value."
  (> (or delta 0.001) (abs (1- (/ a b)))))

(defun lcms-triple-approx-p (a b &optional delta)
  "Like `lcms-approx-p' except for color triples."
  (pcase-let ((`(,a1 ,a2 ,a3) a)
              (`(,b1 ,b2 ,b3) b))
   (and (lcms-approx-p a1 b1 delta)
        (lcms-approx-p a2 b2 delta)
        (lcms-approx-p a3 b3 delta))))

(defun lcms-rgb255->xyz (rgb)
  "Return XYZ tristimulus values corresponding to RGB."
  (let ((rgb1 (mapcar (lambda (x) (/ x 255.0)) rgb)))
    (apply #'color-srgb-to-xyz rgb1)))

(ert-deftest lcms-cri-cam02-ucs ()
  "Test use of `lcms-cam02-ucs'."
  (skip-unless (featurep 'lcms2))
  (should-error (lcms-cam02-ucs '(0 0 0) '(0 0 0) "error"))
  (should-error (lcms-cam02-ucs '(0 0 0) 'error))
  (should-not
   (lcms-approx-p
    (let ((wp '(0.44757 1.0 0.40745)))
      (lcms-cam02-ucs '(0.5 0.5 0.5) '(0 0 0) wp))
    (lcms-cam02-ucs '(0.5 0.5 0.5) '(0 0 0))))
  (should (eql 0.0 (lcms-cam02-ucs '(0.5 0.5 0.5) '(0.5 0.5 0.5))))
  (should
   (lcms-approx-p (lcms-cam02-ucs lcms-colorspacious-d65
                                  '(0 0 0)
                                  lcms-colorspacious-d65)
                  100.0)))

(ert-deftest lcms-whitepoint ()
  "Test use of `lcms-temp->white-point'."
  (skip-unless (featurep 'lcms2))
  (should-error (lcms-temp->white-point 3999))
  (should-error (lcms-temp->white-point 25001))
  ;; D55
  (should
   (lcms-triple-approx-p
    (apply #'color-xyz-to-xyy (lcms-temp->white-point 5503))
    '(0.33242 0.34743 1.0)))
  ;; D65
  (should
   (lcms-triple-approx-p
    (apply #'color-xyz-to-xyy (lcms-temp->white-point 6504))
    '(0.31271 0.32902 1.0)))
  ;; D75
  (should
   (lcms-triple-approx-p
    (apply #'color-xyz-to-xyy (lcms-temp->white-point 7504))
    '(0.29902 0.31485 1.0))))

(ert-deftest lcms-roundtrip ()
  "Test accuracy of converting to and from different color spaces"
  (skip-unless (featurep 'lcms2))
  (should
   (let ((color '(.5 .3 .7)))
     (lcms-triple-approx-p (lcms-jch->xyz (lcms-xyz->jch color))
                           color
                           0.0001)))
  (should
   (let ((color '(.8 -.2 .2)))
     (lcms-triple-approx-p (lcms-jch->jab (lcms-jab->jch color))
                           color
                           0.0001))))

(ert-deftest lcms-ciecam02-gold ()
  "Test CIE CAM02 JCh gold values"
  (skip-unless (featurep 'lcms2))
  (should
   (lcms-triple-approx-p
    (lcms-xyz->jch '(0.1931 0.2393 0.1014)
                   '(0.9888 0.900 0.3203)
                   '(18 200 1 1.0))
    '(48.0314 38.7789 191.0452)
    0.02))
  (should
   (lcms-triple-approx-p
    (lcms-xyz->jch '(0.1931 0.2393 0.1014)
                   '(0.9888 0.90 0.3203)
                   '(18 20 1 1.0))
    '(47.6856 36.0527 185.3445)
    0.09)))

(ert-deftest lcms-dE-cam02-ucs-silver ()
  "Test CRI-CAM02-UCS deltaE metric values from colorspacious."
  (skip-unless (featurep 'lcms2))
  (should
   (lcms-approx-p
    (lcms-cam02-ucs (lcms-rgb255->xyz '(173 52 52))
                    (lcms-rgb255->xyz '(59 120 51))
                    lcms-colorspacious-d65
                    (list 20 (/ 64 float-pi 5) 1 1))
    44.698469808449964
    0.03))
  (should
   (lcms-approx-p
    (lcms-cam02-ucs (lcms-rgb255->xyz '(69 100 52))
                    (lcms-rgb255->xyz '(59 120 51))
                    lcms-colorspacious-d65
                    (list 20 (/ 64 float-pi 5) 1 1))
    8.503323264883667
    0.04)))

(ert-deftest lcms-jmh->cam02-ucs-silver ()
  "Compare JCh conversion to CAM02-UCS to values from colorspacious."
  (skip-unless (featurep 'lcms2))
  (should
   (lcms-triple-approx-p (lcms-jch->jab '(50 20 10))
                         '(62.96296296 16.22742674 2.86133316)
                         0.05))
  (should
   (lcms-triple-approx-p (lcms-jch->jab '(10 60 100))
                         '(15.88785047 -6.56546789 37.23461867)
                         0.04)))

;;; lcms-tests.el ends here
