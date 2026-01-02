;;; visual-wrap-tests.el --- Tests for `visual-wrap-prefix-mode'  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

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

;; Tests for `visual-wrap-prefix-mode'.

;;; Code:

(require 'visual-wrap)
(require 'ert)

;;; Tests:

(ert-deftest visual-wrap-tests/simple ()
  "Test adding wrapping properties to text without display properties."
  (with-temp-buffer
    (insert "greetings\n* hello\n* hi")
    (visual-wrap-prefix-function (point-min) (point-max))
    (should (equal-including-properties
             (buffer-string)
             #("greetings\n* hello\n* hi"
               10 12 ( wrap-prefix (space :align-to (2 . width))
                       display (min-width ((2 . width))))
               12 17 ( wrap-prefix (space :align-to (2 . width)))
               18 20 ( wrap-prefix (space :align-to (2 . width))
                       display (min-width ((2 . width))))
               20 22 ( wrap-prefix (space :align-to (2 . width))))))))

(ert-deftest visual-wrap-tests/safe-display ()
  "Test adding wrapping properties to text with safe display properties."
  (with-temp-buffer
    (insert #("* hello" 2 7 (display (raise 1))))
    (visual-wrap-prefix-function (point-min) (point-max))
    (should (equal-including-properties
             (buffer-string)
             #("* hello"
               0 2 ( wrap-prefix (space :align-to (2 . width))
                     display (min-width ((2 . width))))
               2 7 ( wrap-prefix (space :align-to (2 . width))
                     display (raise 1)))))))

(ert-deftest visual-wrap-tests/unsafe-display/within-line ()
  "Test adding wrapping properties to text with unsafe display properties.
When these properties don't extend across multiple lines,
`visual-wrap-prefix-mode' can still add wrapping properties."
  (with-temp-buffer
    (insert #("* [img]" 2 7 (display (image :type bmp))))
    (visual-wrap-prefix-function (point-min) (point-max))
    (should (equal-including-properties
             (buffer-string)
             #("* [img]"
               0 2 ( wrap-prefix (space :align-to (2 . width))
                     display (min-width ((2 . width))))
               2 7 ( wrap-prefix (space :align-to (2 . width))
                     display (image :type bmp)))))))

(ert-deftest visual-wrap-tests/unsafe-display/spanning-lines ()
  "Test adding wrapping properties to text with unsafe display properties.
When these properties do extend across multiple lines,
`visual-wrap-prefix-mode' must avoid adding wrapping properties."
  (with-temp-buffer
    (insert #("* a\n* b" 0 7 (display (image :type bmp))))
    (visual-wrap-prefix-function (point-min) (point-max))
    (should (equal-including-properties
             (buffer-string)
             #("* a\n* b" 0 7 (display (image :type bmp)))))))

(ert-deftest visual-wrap-tests/unsafe-display/multiple-1 ()
  "Test adding wrapping properties to text with unsafe display properties.
This tests a multi-line unsafe display prop immediately followed by a
single-line unsafe display prop.  `visual-wrap-prefix-mode' should *not*
add wrapping properties to either block."
  (with-temp-buffer
    (insert #("* a\n* b"
              0 4 (display ((image :type bmp)))
              4 7 (display ((image :type bmp) (height 1.5)))))
    (visual-wrap-prefix-function (point-min) (point-max))
    (should (equal-including-properties
             (buffer-string)
             ;; NOTE: See the note in `visual-wrap-prefix-function'.  If
             ;; applying the change mentioned there, then this case
             ;; should add wrapping properties to the second block.
             #("* a\n* b"
              0 4 (display ((image :type bmp)))
              4 7 (display ((image :type bmp) (height 1.5))))))))

(ert-deftest visual-wrap-tests/unsafe-display/multiple-2 ()
  "Test adding wrapping properties to text with unsafe display properties.
This tests a multi-line unsafe display prop immediately followed by
another multi-line unsafe display prop.  `visual-wrap-prefix-mode'
should *not* add wrapping properties to either block."
  (with-temp-buffer
    (insert #("* a\n* b\n"
              0 4 (display ((image :type bmp)))
              4 8 (display ((image :type bmp) (height 1.5)))))
    (visual-wrap-prefix-function (point-min) (point-max))
    (should (equal-including-properties
             (buffer-string)
             #("* a\n* b\n"
              0 4 (display ((image :type bmp)))
              4 8 (display ((image :type bmp) (height 1.5))))))))

(ert-deftest visual-wrap-tests/wrap-prefix-stickiness ()
  "Test that `wrap-prefix' doesn't persist across multiple lines when typing.
See bug#76018."
  (with-temp-buffer
    (insert "* this zoo contains goats")
    (visual-wrap-prefix-function (point-min) (point-max))
    (should (equal-including-properties
             (buffer-string)
             #("* this zoo contains goats"
               0  2 ( wrap-prefix (space :align-to (2 . width))
                      display (min-width ((2 . width))))
               2 25 ( wrap-prefix (space :align-to (2 . width))))))
    (let ((start (point)))
      (insert-and-inherit "\n\nit also contains pandas")
      (visual-wrap-prefix-function start (point-max)))
    (should (equal-including-properties
             (buffer-string)
             #("* this zoo contains goats\n\nit also contains pandas"
               0  2 ( wrap-prefix (space :align-to (2 . width))
                      display (min-width ((2 . width))))
               2 25 ( wrap-prefix (space :align-to (2 . width))))))))

(ert-deftest visual-wrap-tests/cleanup ()
  "Test that deactivating `visual-wrap-prefix-mode' cleans up text properties."
  (with-temp-buffer
    (insert "* hello\n* hi")
    (visual-wrap-prefix-function (point-min) (point-max))
    ;; Make sure we've added the visual-wrapping properties.
    (should (equal (text-properties-at (point-min))
                   '( wrap-prefix (space :align-to (2 . width))
                      display (min-width ((2 . width))))))
    (visual-wrap-prefix-mode -1)
    (should (equal-including-properties
             (buffer-string)
             "* hello\n* hi"))))

;; visual-wrap-tests.el ends here
