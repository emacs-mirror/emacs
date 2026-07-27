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
;;
;; Expected `wrap-prefix' values use `string-pixel-width', because
;; `visual-wrap--content-prefix' records the rendered width of the
;; prefix.  This is 1 pixel per character cell in batch/TTY, but depends
;; on the selected frame font in graphical frames.

;;; Code:

(require 'visual-wrap)
(require 'ert)

(defun visual-wrap-tests--wrap-prefix (prefix &optional extra-indent)
  "Return the expected `wrap-prefix' for PREFIX and EXTRA-INDENT."
  `(space :align-to (+ (,(string-pixel-width prefix (current-buffer)))
                       (,(or extra-indent 0) . width))))

(defun visual-wrap-tests--string-with-properties (string ranges)
  "Return STRING with RANGES of text properties added.
Each element in RANGES has the form (START END PROPERTIES)."
  (let ((string (copy-sequence string)))
    (dolist (range ranges string)
      (add-text-properties (nth 0 range) (nth 1 range) (nth 2 range)
                           string))))

;;; Tests:

(ert-deftest visual-wrap-tests/simple ()
  "Test adding wrapping properties to text without display properties."
  (with-temp-buffer
    (insert "greetings\n* hello\n* hi")
    (let ((wrap-prefix (visual-wrap-tests--wrap-prefix "* ")))
      (visual-wrap-prefix-function (point-min) (point-max))
      (should (equal-including-properties
               (buffer-string)
               (visual-wrap-tests--string-with-properties
                "greetings\n* hello\n* hi"
                `((10 17 (wrap-prefix ,wrap-prefix))
                  (18 22 (wrap-prefix ,wrap-prefix)))))))))

(ert-deftest visual-wrap-tests/safe-display ()
  "Test adding wrapping properties to text with safe display properties."
  (with-temp-buffer
    (insert #("* hello" 2 7 (display (raise 1))))
    (let ((wrap-prefix (visual-wrap-tests--wrap-prefix "* ")))
      (visual-wrap-prefix-function (point-min) (point-max))
      (should (equal-including-properties
               (buffer-string)
               (visual-wrap-tests--string-with-properties
                "* hello"
                `((0 2 (wrap-prefix ,wrap-prefix))
                  (2 7 (wrap-prefix ,wrap-prefix display (raise 1))))))))))

(ert-deftest visual-wrap-tests/unsafe-display/within-line ()
  "Test adding wrapping properties to text with unsafe display properties.
When these properties don't extend across multiple lines,
`visual-wrap-prefix-mode' can still add wrapping properties."
  (with-temp-buffer
    (insert #("* [img]" 2 7 (display (image :type bmp))))
    (let ((wrap-prefix (visual-wrap-tests--wrap-prefix "* ")))
      (visual-wrap-prefix-function (point-min) (point-max))
      (should (equal-including-properties
               (buffer-string)
               (visual-wrap-tests--string-with-properties
                "* [img]"
                `((0 2 (wrap-prefix ,wrap-prefix))
                  (2 7 (wrap-prefix ,wrap-prefix
                        display (image :type bmp))))))))))

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
    (let ((wrap-prefix (visual-wrap-tests--wrap-prefix "* ")))
      (visual-wrap-prefix-function (point-min) (point-max))
      (should (equal-including-properties
               (buffer-string)
               (visual-wrap-tests--string-with-properties
                "* this zoo contains goats"
                `((0 25 (wrap-prefix ,wrap-prefix))))))
      (let ((start (point)))
        (insert-and-inherit "\n\nit also contains pandas")
        (visual-wrap-prefix-function start (point-max)))
      (should (equal-including-properties
               (buffer-string)
               (visual-wrap-tests--string-with-properties
                "* this zoo contains goats\n\nit also contains pandas"
                `((0 25 (wrap-prefix ,wrap-prefix)))))))))

(ert-deftest visual-wrap-tests/cleanup ()
  "Test that deactivating `visual-wrap-prefix-mode' cleans up text properties."
  (with-temp-buffer
    (insert "* hello\n* hi")
    (let ((wrap-prefix (visual-wrap-tests--wrap-prefix "* ")))
      (visual-wrap-prefix-function (point-min) (point-max))
      ;; Make sure we've added the visual-wrapping properties.
      (should (equal (text-properties-at (point-min))
                     `(wrap-prefix ,wrap-prefix))))
    (visual-wrap-prefix-mode -1)
    (should (equal-including-properties
             (buffer-string)
             "* hello\n* hi"))))

(ert-deftest visual-wrap-tests/negative-extra-indent ()
  "A large negative `visual-wrap-extra-indent' does not break alignment.
The mixed-unit `:align-to' sum may go negative, but the display engine
clamps the stretch width to zero (xdisp.c), so the continuation starts
at the left margin."
  (with-temp-buffer
    (setq-local visual-wrap-extra-indent -20)
    (insert "* hello")
    (let ((wrap-prefix (visual-wrap-tests--wrap-prefix
                        "* " visual-wrap-extra-indent)))
      (visual-wrap-prefix-function (point-min) (point-max))
      ;; The sum is negative in batch mode (2 - 20 = -18), but the
      ;; display engine clamps to zero.
      (should (equal (get-text-property (point-min) 'wrap-prefix)
                     wrap-prefix)))))

(ert-deftest visual-wrap-tests/invisible-prefix ()
  "Invisible prefix characters do not reserve column space.
The natural pixel width of a fully invisible prefix is zero, so the
continuation `wrap-prefix' aligns to pixel 0 and no `min-width' display
property is installed on line 1.  See bug#81039."
  (with-temp-buffer
    (insert (propertize "### " 'invisible t))
    (insert "Heading")
    (visual-wrap-prefix-function (point-min) (point-max))
    (should (equal (get-text-property (point-min) 'wrap-prefix)
                   '(space :align-to (+ (0) (0 . width)))))
    ;; The original bug was that `min-width' got installed on the
    ;; invisible prefix region, padding line 1 even though the prefix
    ;; rendered at zero pixels.  The redesign installs no `min-width'
    ;; at all.
    (should-not (memq 'min-width
                      (ensure-list
                       (get-text-property (point-min) 'display))))))

(ert-deftest visual-wrap-tests/line-numbers-align-to-wrap-prefix ()
  "With line numbers, `wrap-prefix' `:align-to' aligns from text start."
  ;; `posn-point' returns nil in batch sessions.
  (skip-when (frame-initial-p))
  (let ((buffer (generate-new-buffer " *visual-wrap-test*")))
    (unwind-protect
        (let ((window (display-buffer buffer)))
          (with-selected-window window
            (setq-local display-line-numbers t)
            (visual-line-mode 1)
            (let* ((columns (window-width))
                   ;; Make the first word fit on visual line 1, but leave
                   ;; too little room for the second word so word wrapping
                   ;; moves it to visual line 2.
                   (first-word-width (max 10 (- columns 20))))
              (insert "- "
                      (make-string first-word-width ?n)
                      " "
                      (make-string 20 ?n)))
            (visual-wrap-prefix-function (point-min) (point-max))
            (redisplay t)
            (let* ((bol-x (car (posn-x-y (posn-at-point (point-min)))))
                   (prefix-width (string-pixel-width "- " (current-buffer)))
                   (second-word-pos
                    (save-excursion
                      (goto-char (point-min))
                      (search-forward " " nil t 2)
                      (point)))
                   (second-word-x
                    (car (posn-x-y (posn-at-point second-word-pos)))))
              (should (= second-word-x (+ bol-x prefix-width))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;; visual-wrap-tests.el ends here
