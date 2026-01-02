;;; xfaces-tests.el --- tests for xfaces.c           -*- lexical-binding: t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

(ert-deftest xfaces-color-distance ()
  ;; Check symmetry (bug#41544).
  (should (equal (color-distance "#222222" "#ffffff")
                 (color-distance "#ffffff" "#222222"))))

(ert-deftest xfaces-internal-color-values-from-color-spec ()
  (should (equal (color-values-from-color-spec "#f05")
                 '(#xffff #x0000 #x5555)))
  (should (equal (color-values-from-color-spec "#1fb0C5")
                 '(#x1f1f #xb0b0 #xc5c5)))
  (should (equal (color-values-from-color-spec "#1f8b0AC5e")
                 '(#x1f81 #xb0aa #xc5eb)))
  (should (equal (color-values-from-color-spec "#1f83b0ADC5e2")
                 '(#x1f83 #xb0ad #xc5e2)))
  (should (equal (color-values-from-color-spec "#1f83b0ADC5e2g") nil))
  (should (equal (color-values-from-color-spec "#1f83b0ADC5e20") nil))
  (should (equal (color-values-from-color-spec "#12345") nil))
  (should (equal (color-values-from-color-spec "rgb:f/23/28a")
                 '(#xffff #x2323 #x28a2)))
  (should (equal (color-values-from-color-spec "rgb:1234/5678/09ab")
                 '(#x1234 #x5678 #x09ab)))
  (should (equal (color-values-from-color-spec "rgb:0//0") nil))
  (should (equal (color-values-from-color-spec "rgbi:0/0.5/0.1")
                 '(0 32768 6554)))
  (should (equal (color-values-from-color-spec "rgbi:1e-3/1.0e-2/1e0")
                 '(66 655 65535)))
  (should (equal (color-values-from-color-spec "rgbi:0/0.5/10") nil))
  (should (equal (color-values-from-color-spec "rgbi:0/0/ 0") nil))
  (should (equal (color-values-from-color-spec "rgbi:0/0x0/0") nil))
  (should (equal (color-values-from-color-spec "rgbi:0/+0x1/0") nil)))

(ert-deftest xfaces-test-circular-inheritance ()
  "Test that bug#79672 remains solved."
  (let ((buf (get-buffer-create "xfaces-test")))
    (with-current-buffer buf
      (font-lock-mode -1)
      (set-face-attribute 'button nil :inherit 'link)
      (should (equal
               (should-error (set-face-attribute 'link nil :inherit 'button))
               (list 'error
                     "Face inheritance results in inheritance cycle"
                     'button))))
    (kill-buffer buf)))

(provide 'xfaces-tests)

;;; xfaces-tests.el ends here
