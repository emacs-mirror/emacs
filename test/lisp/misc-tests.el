;;; misc-tests.el --- Tests for misc.el  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefankangas@gmail.com>

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

;;; Code:

(require 'ert)
(require 'misc)
(require 'mule-util)

(defmacro with-misc-test (original result &rest body)
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,original)
     ,@body
     (should (equal (buffer-string) ,result))))

(ert-deftest misc-test-copy-from-above-command ()
  (with-misc-test "abc\n" "abc\nabc"
    (copy-from-above-command))
  (with-misc-test "abc\n" "abc\nab"
    (copy-from-above-command 2)))

(ert-deftest misc-test-zap-up-to-char ()
  (with-misc-test "abcde" "cde"
    (goto-char (point-min))
    (zap-up-to-char 1 ?c))
  (with-misc-test "abcde abc123" "c123"
    (goto-char (point-min))
    (zap-up-to-char 2 ?c))
  (let ((case-fold-search t))
    (with-misc-test "abcdeCXYZ" "cdeCXYZ"
      (goto-char (point-min))
      (zap-up-to-char 1 ?C))
    (with-misc-test "abcdeCXYZ" "CXYZ"
      (goto-char (point-min))
      (zap-up-to-char 1 ?C 'interactive))))

(ert-deftest misc-test-upcase-char ()
  (with-misc-test "abcde" "aBCDe"
    (goto-char (1+ (point-min)))
    (upcase-char 3)))

(ert-deftest misc-test-forward-to-word ()
  (with-temp-buffer
    (insert "    -   abc")
    (goto-char (point-min))
    (forward-to-word 1)
    (should (equal (point) 9)))
  (with-temp-buffer
    (insert "a b c")
    (goto-char (point-min))
    (forward-to-word 3)
    (should (equal (point) 6))))

(ert-deftest misc-test-backward-to-word ()
  (with-temp-buffer
    (insert "abc    -   ")
    (backward-to-word 1)
    (should (equal (point) 4)))
  (with-temp-buffer
    (insert "a b c")
    (backward-to-word 3)
    (should (equal (point) 1))))

(ert-deftest misc--duplicate-line ()
  ;; Duplicate a line (twice).
  (with-temp-buffer
    (insert "abc\ndefg\nh\n")
    (goto-char 7)
    (duplicate-line 2)
    (should (equal (buffer-string) "abc\ndefg\ndefg\ndefg\nh\n"))
    (should (equal (point) 7)))
  ;; Duplicate a line (twice) and move point to the first duplicated line.
  (with-temp-buffer
    (insert "abc\ndefg\nh\n")
    (goto-char 7)
    (let ((duplicate-line-final-position 1)) (duplicate-line 2))
    (should (equal (buffer-string) "abc\ndefg\ndefg\ndefg\nh\n"))
    (should (equal (point) 12)))
  ;; Duplicate a line (twice) and move point to the last duplicated line.
  (with-temp-buffer
    (insert "abc\ndefg\nh\n")
    (goto-char 7)
    (let ((duplicate-line-final-position -1)) (duplicate-line 2))
    (should (equal (buffer-string) "abc\ndefg\ndefg\ndefg\nh\n"))
    (should (equal (point) 17)))
  ;; Duplicate a non-terminated line.
  (with-temp-buffer
    (insert "abc")
    (goto-char 2)
    (duplicate-line)
    (should (equal (buffer-string) "abc\nabc\n"))
    (should (equal (point) 2))))

(require 'rect)

(ert-deftest misc--duplicate-dwim ()
  (let ((duplicate-line-final-position 0)
        (duplicate-region-final-position 0))
    ;; Duplicate a line.
    (dolist (final-pos '(0 -1 1))
      (ert-info ((prin1-to-string final-pos) :prefix "final-pos: ")
        (with-temp-buffer
          (insert "abc\ndefg\nh\n")
          (goto-char 7)
          (let ((duplicate-line-final-position final-pos))
            (duplicate-dwim 3))
          (should (equal (buffer-string) "abc\ndefg\ndefg\ndefg\ndefg\nh\n"))
          (let ((delta (* 5 (if (< final-pos 0) 3 final-pos))))
            (should (equal (point) (+ 7 delta)))))))

    ;; Duplicate a region.
    (dolist (final-pos '(0 -1 1))
      (ert-info ((prin1-to-string final-pos) :prefix "final-pos: ")
        (with-temp-buffer
          (insert "abCDEFghi")
          (set-mark 3)
          (goto-char 7)
          (transient-mark-mode)
          (should (use-region-p))
          (let ((duplicate-region-final-position final-pos))
            (duplicate-dwim 3))
          (should (equal (buffer-string) "abCDEFCDEFCDEFCDEFghi"))
          (should (region-active-p))
          (let ((delta (* 4 (if (< final-pos 0) 3 final-pos))))
            (should (equal (point) (+ 7 delta)))
            (should (equal (mark) (+ 3 delta)))))))

    ;; Duplicate a rectangular region (sparse).
    (with-temp-buffer
      (insert "x\n>a\n>bcde\n>fg\nyz\n")
      (goto-char 4)
      (rectangle-mark-mode)
      (goto-char 15)
      (rectangle-forward-char 1)
      (duplicate-dwim)
      (should (equal (buffer-string) "x\n>a  a  \n>bcdbcde\n>fg fg \nyz\n"))
      (should (equal (point) 24))
      (should (region-active-p))
      (should rectangle-mark-mode)
      (should (equal (mark) 4)))

    ;; Idem (dense).
    (dolist (final-pos '(0 -1 1))
      (ert-info ((prin1-to-string final-pos) :prefix "final-pos: ")
        (with-temp-buffer
          (insert "aBCd\neFGh\niJKl\n")
          (goto-char 2)
          (rectangle-mark-mode)
          (goto-char 14)
          (let ((duplicate-region-final-position final-pos))
            (duplicate-dwim 3))
          (should (equal (buffer-string)
                         "aBCBCBCBCd\neFGFGFGFGh\niJKJKJKJKl\n"))
          (should (region-active-p))
          (should rectangle-mark-mode)
          (let ((hdelta (* 2 (if (< final-pos 0) 3 final-pos)))
                (vdelta 12))
            (should (equal (point) (+ 14 vdelta hdelta)))
            (should (equal (mark) (+ 2 hdelta)))))))))

;; Check that `string-pixel-width' returns a consistent result in the
;; various situations that can lead to erroneous results.
(ert-deftest misc-test-string-pixel-width-char-property-alias-alist ()
  "Test `string-pixel-width' with `char-property-alias-alist'."
  (with-temp-buffer
    (let ((text0 (propertize "This text"
                             'display "xxxx"
                             'face 'variable-pitch))
          (text1 (propertize "This text"
                             'my-display "xxxx"
                             'my-face 'variable-pitch)))
      (setq-local char-property-alias-alist '((display my-display)
                                              (face my-face)))
      (should (= (string-pixel-width text0 (current-buffer))
                 (string-pixel-width text1 (current-buffer)))))))

;; This test never fails in batch mode.
(ert-deftest misc-test-string-pixel-width-face-remapping-alist ()
  "Test `string-pixel-width' with `face-remapping-alist'."
  (with-temp-buffer
    (setq-local face-remapping-alist '((variable-pitch . default)))
    (let ((text0 (propertize "This text" 'face 'default))
          (text1 (propertize "This text" 'face 'variable-pitch)))
      (should (= (string-pixel-width text0 (current-buffer))
                 (string-pixel-width text1 (current-buffer)))))))

(ert-deftest misc-test-string-pixel-width-default-text-properties ()
  "Test `string-pixel-width' with `default-text-properties'."
  (with-temp-buffer
    (setq-local default-text-properties '(display "XXXX"))
    (let ((text0 (propertize "This text" 'display "XXXX"))
          (text1 "This text"))
      (should (= (string-pixel-width text0 (current-buffer))
                 (string-pixel-width text1 (current-buffer)))))))

(ert-deftest misc-test-string-pixel-width-line-and-wrap-prefix ()
  "Test `string-pixel-width' with `line-prefix' and `wrap-prefix'."
  (let ((lp (default-value 'line-prefix))
        (wp (default-value 'line-prefix))
        (text (make-string 2000 ?X))
        w0 w1)
    (unwind-protect
        (progn
          (setq-default line-prefix nil wrap-prefix nil)
          (setq w0 (string-pixel-width text))
          (setq-default line-prefix "PPPP" wrap-prefix "WWWW")
          (setq w1 (string-pixel-width text)))
      (setq-default line-prefix lp wrap-prefix wp))
    (should (= w0 w1))))

;; This test never fails in batch mode.
(ert-deftest misc-test-string-pixel-width-display-line-numbers ()
  "Test `string-pixel-width' with `display-line-numbers'."
  (let ((dln (default-value 'display-line-numbers))
        (text "This text")
        w0 w1)
    (unwind-protect
        (progn
          (setq-default display-line-numbers nil)
          (setq w0 (string-pixel-width text))
          (setq-default display-line-numbers t)
          (setq w1 (string-pixel-width text)))
      (setq-default display-line-numbers dln))
    (should (= w0 w1))))

;; Exercise `truncate-string-pixelwise' with strings of the same
;; characters of differing widths, with and without ellipses, in varying
;; faces, and varying face heights and compare results to each
;; character's measured width.
(ert-deftest misc-test-truncate-string-pixelwise ()
  (dolist (c '(?W ?X ?y ?1))
    (dolist (ellipsis `(nil "..." ,(truncate-string-ellipsis)))
      (dolist (face '(fixed-pitch variable-pitch))
        (dolist (height '(1.0 0.5 1.5))
          (with-temp-buffer
            (setq-local face-remapping-alist `((,face . default)))
            (face-remap-add-relative 'default :height height)
            (let ((char-pixels (string-pixel-width
                                (make-string 1 c) (current-buffer))))
              (dotimes (i 20)
                (setq i (1+ i))
                (should (eq i (length
                               (truncate-string-pixelwise
                                (make-string (* i 2) c)
                                (* i char-pixels)
                                (current-buffer)
                                ellipsis))))))))))))

;; Exercise `truncate-string-pixelwise' with varying unicode strings, in
;; varying faces, and varying face heights and compare results to a
;; naive `string-pixel-width' based string truncate function.
(ert-deftest misc-test-truncate-string-pixelwise-unicode ()
  :tags '(:expensive-test)
  (skip-when noninteractive)
  (let ((max-pixels 500)
        (truncate-string-naive (lambda (string pixels buffer)
                                 (while (and (length> string 0)
                                             (> (string-pixel-width string buffer) pixels))
                                   (setq string (substring string 0 (1- (length string)))))
                                 string))
        (strings (list
                  "foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz"
                  (concat "話說天下大勢，分久必合，合久必分：周末七國分爭，并入於秦。"
                          "及秦滅之後，楚、漢分爭，又并入於漢。漢朝自高祖斬白蛇而起義，"
                          "一統天下。後來光武中興，傳至獻帝，遂分為三國。推其致亂之由，"
                          "殆始於桓、靈二帝。桓帝禁錮善類，崇信宦官。及桓帝崩，靈帝即位，"
                          "大將軍竇武、太傅陳蕃，共相輔佐。時有宦官曹節等弄權，竇武、陳蕃謀誅之，"
                          "作事不密，反為所害。中涓自此愈橫")
                  (concat "короче теперь если по русски написать все четко или все равно"
                          " короче теперь если по русски написать все четко или все равно"
                          " короче теперь если по русски написать все четко или все равно"
                          " короче теперь если по русски написать все четко или все равно")
                  "будет разрыв строки непонятно где🏁🚩🎌🏴🏳️ 🏳️ <200d>🌈🏳️ <200d>⚧️🏴<200d>☠️"
                  (apply #'concat (make-list 200 "\u0065\u0301 ")) ; composed é \u00E9
                  (let ((woman-loves-man ; 👩‍❤️‍👨
                         (concat "\N{WOMAN}"
                                 "\N{ZERO WIDTH JOINER}"
                                 "\N{HEAVY BLACK HEART}"
                                 "\N{VARIATION SELECTOR-16}"
                                 "\N{ZERO WIDTH JOINER}"
                                 "\N{MAN}"
                                 " ")))
                    (apply #'concat (make-list 200 woman-loves-man)))
                  (propertize (let ((varying-height-string
                                     (mapconcat
                                      #'identity
                                      (list "AWi!"
                                            (propertize "foo" 'face '(:height 2.5))
                                            (propertize "bar" 'face '(:height 0.5))
                                            (propertize "baz" 'face '(:height 1.0)))
                                      " ")))
                                (apply #'concat (make-list 100 varying-height-string)))
                              'face 'variable-pitch))))
    (dolist (face '(fixed-pitch variable-pitch))
      (dolist (height '(1.0 0.5 1.5))
        (with-temp-buffer
          (setq-local face-remapping-alist `((,face . default)))
          (face-remap-add-relative 'default :height height)
          (dolist (string strings)
            (should (eq (length (funcall truncate-string-naive
                                         string max-pixels (current-buffer)))
                        (length (truncate-string-pixelwise
                                 string max-pixels (current-buffer)))))))))))

(provide 'misc-tests)
;;; misc-tests.el ends here
