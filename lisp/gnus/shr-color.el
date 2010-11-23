;;; shr-color.el --- Simple HTML Renderer color management

;; Copyright (C) 2010 Free Software Foundation, Inc.

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: html

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package handles colors display for shr.

;;; Code:

(require 'color-lab)

(defgroup shr-color nil
  "Simple HTML Renderer colors"
  :group 'shr)

(defcustom shr-color-visible-luminance-min 40
  "Minimum luminance distance between two colors to be considered visible.
Must be between 0 and 100."
  :group 'shr
  :type 'float)

(defcustom shr-color-visible-distance-min 5
  "Minimum color distance between two colors to be considered visible.
This value is used to compare result for `ciede2000'. Its an
absolute value without any unit."
  :group 'shr
  :type 'integer)

(defun shr-color-relative-to-absolute (number)
  "Convert a relative NUMBER to absolute. If NUMBER is absolute, return NUMBER.
This will convert \"80 %\" to 204, \"100 %\" to 255 but \"123\" to \"123\"."
  (let ((string-length (- (length number) 1)))
    ;; Is this a number with %?
    (if (eq (elt number string-length) ?%)
        (/ (* (string-to-number (substring number 0 string-length)) 255) 100)
      (string-to-number number))))

(defun shr-color-hue-to-rgb (x y h)
  "Convert X Y H to RGB value."
  (when (< h 0) (incf h))
  (when (> h 1) (decf h))
  (cond ((< h (/ 1 6.0)) (+ x (* (- y x) h 6)))
        ((< h 0.5) y)
        ((< h (/ 2.0 3.0)) (+ x (* (- y x) (- (/ 2.0 3.0) h) 6)))
        (t x)))

(defun shr-color-hsl-to-rgb-fractions (h s l)
  "Convert H S L to fractional RGB values."
  (let (m1 m2)
    (if (<= l 0.5)
        (setq m2 (* l (+ s 1)))
        (setq m2 (- (+ l s) (* l s))))
    (setq m1 (- (* l 2) m2))
    (list (shr-color-hue-to-rgb m1 m2 (+ h (/ 1 3.0)))
	  (shr-color-hue-to-rgb m1 m2 h)
	  (shr-color-hue-to-rgb m1 m2 (- h (/ 1 3.0))))))

(defun shr-color->hexadecimal (color)
  "Convert any color format to hexadecimal representation.
Like rgb() or hsl()."
  (when color
    (cond ((or (string-match
                "rgb(\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*)"
                color)
               (string-match
                "rgba(\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*[0-9]*\.?[0-9]+\s*%?\s*)"
                color))
           (format "#%02X%02X%02X"
                   (shr-color-relative-to-absolute (match-string-no-properties 1 color))
                   (shr-color-relative-to-absolute (match-string-no-properties 2 color))
                   (shr-color-relative-to-absolute (match-string-no-properties 3 color))))
          ((or (string-match
                "hsl(\s*\\([0-9]\\{1,3\\}\\)\s*,\s*\\([0-9]\\{1,3\\}\\)\s*%\s*,\s*\\([0-9]\\{1,3\\}\\)\s*%\s*)"
                color)
               (string-match
                "hsla(\s*\\([0-9]\\{1,3\\}\\)\s*,\s*\\([0-9]\\{1,3\\}\\)\s*%\s*,\s*\\([0-9]\\{1,3\\}\\)\s*%\s*,\s*[0-9]*\.?[0-9]+\s*%?\s*)"
                color))
           (let ((h (/ (string-to-number (match-string-no-properties 1 color)) 360.0))
                 (s (/ (string-to-number (match-string-no-properties 2 color)) 100.0))
                 (l (/ (string-to-number (match-string-no-properties 3 color)) 100.0)))
             (destructuring-bind (r g b)
                 (shr-color-hsl-to-rgb-fractions h s l)
               (format "#%02X%02X%02X" (* r 255) (* g 255) (* b 255)))))
          (t
           color))))

(defun set-minimum-interval (val1 val2 min max interval &optional fixed)
  "Set minimum interval between VAL1 and VAL2 to INTERVAL.
The values are bound by MIN and MAX.
If FIXED is t, then val1 will not be touched."
  (let ((diff (abs (- val1 val2))))
    (unless (>= diff interval)
      (if fixed
          (let* ((missing (- interval diff))
                 ;; If val2 > val1, try to increase val2
                 ;; That's the "good direction"
                 (val2-good-direction
                  (if (> val2 val1)
                      (min max (+ val2 missing))
                    (max min (- val2 missing))))
                 (diff-val2-good-direction-val1 (abs (- val2-good-direction val1))))
            (if (>= diff-val2-good-direction-val1 interval)
                (setq val2 val2-good-direction)
              ;; Good-direction is not so good, compute bad-direction
              (let* ((val2-bad-direction
                      (if (> val2 val1)
                          (max min (- val1 interval))
                        (min max (+ val1 interval))))
                     (diff-val2-bad-direction-val1 (abs (- val2-bad-direction val1))))
                (if (>= diff-val2-bad-direction-val1 interval)
                    (setq val2 val2-bad-direction)
                  ;; Still not good, pick the best and prefer good direction
                  (setq val2
                        (if (>= diff-val2-good-direction-val1 diff-val2-bad-direction-val1)
                            val2-good-direction
                          val2-bad-direction))))))
        ;; No fixed, move val1 and val2
        (let ((missing (/ (- interval diff) 2.0)))
          (if (< val1 val2)
              (setq val1 (max min (- val1 missing))
                    val2 (min max (+ val2 missing)))
            (setq val2 (max min (- val2 missing))
                  val1 (min max (+ val1 missing))))
          (setq diff (abs (- val1 val2)))   ; Recompute diff
          (unless (>= diff interval)
            ;; Not ok, we hit a boundary
            (let ((missing (- interval diff)))
              (cond ((= val1 min)
                     (setq val2 (+ val2 missing)))
                    ((= val2 min)
                     (setq val1 (+ val1 missing)))
                    ((= val1 max)
                     (setq val2 (- val2 missing)))
                    ((= val2 max)
                     (setq val1 (- val1 missing)))))))))
    (list val1 val2)))

(defun shr-color-visible (bg fg &optional fixed-background)
  "Check that BG and FG colors are visible if they are drawn on each other.
Return t if they are. If they are too similar, two new colors are
returned instead.
If FIXED-BACKGROUND is set, and if the color are not visible, a
new background color will not be computed. Only the foreground
color will be adapted to be visible on BG."
  ;; Convert fg and bg to CIE Lab
  (let* ((fg-lab (apply 'rgb->lab (rgb->normalize fg)))
         (bg-lab (apply 'rgb->lab (rgb->normalize bg)))
         ;; Compute color distance using CIE DE 2000
         (fg-bg-distance (color-lab-ciede2000 fg-lab bg-lab))
         ;; Compute luminance distance (substract L component)
         (luminance-distance (abs (- (car fg-lab) (car bg-lab)))))
    (if (and (>= fg-bg-distance shr-color-visible-distance-min)
             (>= luminance-distance shr-color-visible-luminance-min))
        (list bg fg)
      ;; Not visible, try to change luminance to make them visible
      (let ((Ls (set-minimum-interval (car bg-lab) (car fg-lab) 0 100
                                      shr-color-visible-luminance-min
                                      fixed-background)))
        (setcar bg-lab (car Ls))
        (setcar fg-lab (cadr Ls))
        (list
         (apply 'format "#%02x%02x%02x"
                (mapcar (lambda (x) (* (max (min 1 x) 0) 255)) (apply 'lab->rgb bg-lab)))
         (apply 'format "#%02x%02x%02x"
                (mapcar (lambda (x) (* (max (min 1 x) 0) 255)) (apply 'lab->rgb fg-lab))))))))

(provide 'shr-color)

;;; shr-color.el ends here
