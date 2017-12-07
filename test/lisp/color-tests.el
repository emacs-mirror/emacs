;;; color-tests.el --- Tests for color.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Simen Heggestøyl <simenheg@gmail.com>
;; Keywords:

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

;;

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'ert)
(require 'seq)

(defun color-tests--approx-equal (color1 color2)
  "Return t if COLOR1 and COLOR2 are approximately equal."
  (seq-every-p
   (lambda (x) (< (abs x) 0.00001))
   (cl-mapcar #'- color1 color2)))

(ert-deftest color-tests-name-to-rgb ()
  (should (equal (color-name-to-rgb "black") '(0.0 0.0 0.0)))
  (should (equal (color-name-to-rgb "white") '(1.0 1.0 1.0)))
  (should (equal (color-name-to-rgb "red") '(1.0 0.0 0.0)))
  (should (equal (color-name-to-rgb "green") '(0.0 1.0 0.0)))
  (should (equal (color-name-to-rgb "blue") '(0.0 0.0 1.0)))
  (should (equal (color-name-to-rgb "#000000000000") '(0.0 0.0 0.0)))
  (should (equal (color-name-to-rgb "#ffffffffffff") '(1.0 1.0 1.0)))
  (should (equal (color-name-to-rgb "#ffff00000000") '(1.0 0.0 0.0)))
  (should (equal (color-name-to-rgb "#0000ffff0000") '(0.0 1.0 0.0)))
  (should (equal (color-name-to-rgb "#00000000ffff") '(0.0 0.0 1.0))))

(ert-deftest color-tests-rgb-to-hex ()
  (should (equal (color-rgb-to-hex 0 0 0) "#000000000000"))
  (should (equal (color-rgb-to-hex 0 0 0 2) "#000000"))
  (should (equal (color-rgb-to-hex 1 0 0) "#ffff00000000"))
  (should (equal (color-rgb-to-hex 1 0 0 2) "#ff0000"))
  (should (equal (color-rgb-to-hex 0.1 0.2 0.3) "#199933334ccc"))
  (should (equal (color-rgb-to-hex 0.1 0.2 0.3 2) "#19334c")))

(ert-deftest color-tests-complement ()
  (should (equal (color-complement "white") '(0.0 0.0 0.0)))
  (should (equal (color-complement "#ffffffffffff") '(0.0 0.0 0.0)))
  (should (equal (color-complement "red") '(0.0 1.0 1.0))))

(ert-deftest color-tests-gradient ()
  (should-not (color-gradient '(0 0 0) '(255 255 255) 0))
  (should
   (equal (color-gradient '(0 0 0) '(255 255 255) 1)
          '((127.5 127.5 127.5))))
  (should
   (equal (color-gradient '(0 0 0) '(255 255 255) 2)
          '((85.0 85.0 85.0) (170.0 170.0 170.0))))
  (should
   (equal
    (color-gradient '(255 192 203) '(250 128 114) 3)
    '((253.75 176.0 180.75) (252.5 160.0 158.5) (251.25 144.0 136.25)))))

(ert-deftest color-tests-hsl-to-rgb ()
  (should (equal (color-hsl-to-rgb 0 0 0) '(0 0 0)))
  (should (equal (color-hsl-to-rgb 360 0.5 0.5) '(0.75 0.25 0.25)))
  (should (equal (color-hsl-to-rgb 123 0.2 0.9) '(0.92 0.88 0.88))))

(ert-deftest color-tests-complement-hex ()
  (should
   (equal (color-complement-hex "#000000000000") "#ffffffffffff"))
  (should
   (equal (color-complement-hex "#ffff00000000") "#0000ffffffff")))

(ert-deftest color-tests-rgb-to-hsv ()
  (should (equal (color-rgb-to-hsv 0 0 0) '(0.0 0.0 0.0)))
  (should (equal (color-rgb-to-hsv 1 1 1) '(0.0 0.0 1.0)))
  (should (equal (color-rgb-to-hsv 1 0 0) '(0.0 1.0 1.0)))
  (should (equal (color-rgb-to-hsv 0.5 0.3 0.3) '(0.0 0.4 0.5))))

(ert-deftest color-tests-rgb-to-hsl ()
  (should (equal (color-rgb-to-hsl 0 0 0) '(0.0 0.0 0.0)))
  (should (equal (color-rgb-to-hsl 1 1 1) '(0.0 0.0 1.0)))
  (should (equal (color-rgb-to-hsl 1 0 0) '(0.0 1 0.5)))
  (should (equal (color-rgb-to-hsl 0.5 0.3 0.3) '(0.0 0.25 0.4))))

(ert-deftest color-tests-srgb-to-xyz ()
  (should (equal (color-srgb-to-xyz 0 0 0) '(0.0 0.0 0.0)))
  (should
   (equal (color-srgb-to-xyz 0 0 1) '(0.1804375 0.072175 0.9503041)))
  (should
   (color-tests--approx-equal
    (color-srgb-to-xyz 0.1 0.2 0.3) '(0.0291865 0.031092 0.073738))))

(ert-deftest color-tests-xyz-to-srgb ()
  (should (equal (color-xyz-to-srgb 0 0 0) '(0.0 0.0 0.0)))
  (should
   (color-tests--approx-equal
    (color-xyz-to-srgb 0.1804375 0.072175 0.9503041) '(0 0 1)))
  (should
   (color-tests--approx-equal
    (color-xyz-to-srgb 0.0291865 0.031092 0.073738) '(0.1 0.2 0.3))))

(ert-deftest color-tests-xyz-to-lab ()
  (should (equal (color-xyz-to-lab 0 0 0) '(0.0 0.0 0.0)))
  (should
   (color-tests--approx-equal
    (color-xyz-to-lab 0.1804375 0.072175 0.9503041)
    '(32.2970109 79.1890315 -107.8646674)))
  (should
   (color-tests--approx-equal
    (color-xyz-to-lab 0.1804375 0.072175 0.9503041 '(1 1 1))
    '(32.2970109 74.3625763 -113.3597823)))
  (should
   (color-tests--approx-equal
    (color-xyz-to-lab 0.0291865 0.031092 0.073738)
    '(20.4760281 -0.6500752 -18.6340169))))

(ert-deftest color-tests-lab-to-xyz ()
  (should (equal (color-lab-to-xyz 0 0 0) '(0.0 0.0 0.0)))
  (should
   (color-tests--approx-equal
    (color-lab-to-xyz 32.2970109 79.1890315 -107.8646674)
    '(0.1804375 0.072175 0.9503041)))
  (should
   (color-tests--approx-equal
    (color-lab-to-xyz 32.2970109 74.3625763 -113.3597823 '(1 1 1))
    '(0.1804375 0.072175 0.9503041)))
  (should
   (color-tests--approx-equal
    (color-lab-to-xyz 20.4760281 -0.6500752 -18.6340169)
    '(0.0291865 0.031092 0.073738))))

(ert-deftest color-tests-srgb-to-lab ()
  (should (equal (color-srgb-to-lab 0 0 0) '(0.0 0.0 0.0)))
  (should
   (color-tests--approx-equal
    (color-srgb-to-lab 0 1 0) '(87.7347223 -86.1808176 83.1770651)))
  (should
   (color-tests--approx-equal
    (color-srgb-to-lab 0.1 0.2 0.3)
    '(20.4762218 -0.6508996 -18.6340085))))

(ert-deftest color-tests-lab-to-srgb ()
  (should (equal (color-lab-to-srgb 0 0 0) '(0.0 0.0 0.0)))
  (should
   (color-tests--approx-equal
    (color-lab-to-srgb 87.7347223 -86.1808176 83.1770651) '(0 1 0)))
  (should
   (color-tests--approx-equal
    (color-lab-to-srgb 20.4762218 -0.6508996 -18.6340085)
    '(0.1 0.2 0.3))))

(ert-deftest color-tests-cie-de2000 ()
  (should (= (color-cie-de2000 '(0 0 0) '(0 0 0)) 0.0))
  (should
   (color-tests--approx-equal
    (list
     (color-cie-de2000
      (color-srgb-to-lab 1 0 0) (color-srgb-to-lab 0 0 1)))
    '(52.8803934)))
  (should
   (color-tests--approx-equal
    (list
     (color-cie-de2000
      (color-srgb-to-lab 0.8 0 0) (color-srgb-to-lab 0.9 0 0)))
    '(5.3844503))))

(ert-deftest color-tests-clamp ()
  (should (= (color-clamp 0) 0.0))
  (should (= (color-clamp -1) 0.0))
  (should (= (color-clamp 0.5) 0.5))
  (should (= (color-clamp 1) 1.0))
  (should (= (color-clamp 1.1) 1.0)))

(ert-deftest color-tests-saturate-hsl ()
  (should (equal (color-saturate-hsl 360 0.5 0.5 0) '(360 0.5 0.5)))
  (should (equal (color-saturate-hsl 360 0.5 0.5 -10) '(360 0.4 0.5)))
  (should
   (equal (color-saturate-hsl 360 0.5 0.5 -500) '(360 0.0 0.5)))
  (should (equal (color-saturate-hsl 120 0.5 0.8 5) '(120 0.55 0.8)))
  (should
   (equal (color-saturate-hsl 120 0.5 0.8 500) '(120 1.0 0.8))))

(ert-deftest color-tests-saturate-name ()
  (should (equal (color-saturate-name "black" 100) "#000000000000"))
  (should (equal (color-saturate-name "white" 100) "#ffffffffffff"))
  (should (equal (color-saturate-name "red" 0) "#ffff00000000"))
  (should (equal (color-saturate-name "red" 50) "#ffff00000000")))

(ert-deftest color-tests-desaturate-hsl ()
  (should (equal (color-desaturate-hsl 360 0.5 0.5 0) '(360 0.5 0.5)))
  (should
   (equal (color-desaturate-hsl 360 0.5 0.5 -10) '(360 0.6 0.5)))
  (should
   (equal (color-desaturate-hsl 360 0.5 0.5 -500) '(360 1.0 0.5)))
  (should
   (equal (color-desaturate-hsl 120 0.5 0.8 5) '(120 0.45 0.8)))
  (should
   (equal (color-desaturate-hsl 120 0.5 0.8 500) '(120 0.0 0.8))))

(ert-deftest color-tests-desaturate-name ()
  (should (equal (color-desaturate-name "black" 100) "#000000000000"))
  (should (equal (color-desaturate-name "white" 100) "#ffffffffffff"))
  (should (equal (color-desaturate-name "red" 0) "#ffff00000000")))

(ert-deftest color-tests-lighten-hsl ()
  (should (equal (color-lighten-hsl 360 0.5 0.5 0) '(360 0.5 0.5)))
  (should (equal (color-lighten-hsl 360 0.5 0.5 -10) '(360 0.5 0.4)))
  (should (equal (color-lighten-hsl 360 0.5 0.5 -500) '(360 0.5 0.0)))
  (should
   (color-tests--approx-equal
    (color-lighten-hsl 120 0.5 0.8 5) '(120 0.5 0.85)))
  (should
   (equal (color-lighten-hsl 120 0.5 0.8 500) '(120 0.5 1.0))))

(ert-deftest color-tests-lighten-name ()
  (should (equal (color-lighten-name "black" 100) "#ffffffffffff"))
  (should (equal (color-lighten-name "white" 100) "#ffffffffffff"))
  (should (equal (color-lighten-name "red" 0) "#ffff00000000"))
  (should (equal (color-lighten-name "red" 10) "#ffff33323332")))

(ert-deftest color-tests-darken-hsl ()
  (should (equal (color-darken-hsl 360 0.5 0.5 0) '(360 0.5 0.5)))
  (should (equal (color-darken-hsl 360 0.5 0.5 -10) '(360 0.5 0.6)))
  (should (equal (color-darken-hsl 360 0.5 0.5 -500) '(360 0.5 1.0)))
  (should (equal (color-darken-hsl 120 0.5 0.8 5) '(120 0.5 0.75)))
  (should (equal (color-darken-hsl 120 0.5 0.8 500) '(120 0.5 0.0))))

(ert-deftest color-tests-darken-name ()
  (should (equal (color-darken-name "black" 100) "#000000000000"))
  (should (equal (color-darken-name "white" 100) "#000000000000"))
  (should (equal (color-darken-name "red" 0) "#ffff00000000"))
  (should (equal (color-darken-name "red" 10) "#cccc00000000")))

(provide 'color-tests)
;;; color-tests.el ends here
