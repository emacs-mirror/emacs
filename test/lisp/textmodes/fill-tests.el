;;; fill-tests.el --- ERT tests for fill.el -*- lexical-binding: t -*-

;; Copyright (C) 2017-2025 Free Software Foundation, Inc.

;; Author:     Marcin Borkowski <mbork@mbork.pl>
;; Keywords:   text

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

;; This package defines tests for the filling feature, specifically
;; the `fill-polish-nobreak-p' function.

;;; Code:

(require 'ert)
(require 'ert-x)

(ert-deftest fill-test-no-fill-polish-nobreak-p nil
  "Tests of the `fill-polish-nobreak-p' function."
  (with-temp-buffer
    (insert "Abc d efg (h ijk).")
    (setq fill-column 8)
    (setq-local fill-nobreak-predicate '())
    (fill-paragraph)
    (should (string= (buffer-string) "Abc d\nefg (h\nijk).")))
  (with-temp-buffer
    (insert "Abc d efg (h ijk).")
    (setq fill-column 8)
    (setq-local fill-nobreak-predicate '(fill-polish-nobreak-p))
    (fill-paragraph)
    (should (string= (buffer-string) "Abc\nd efg\n(h ijk)."))))

(ert-deftest fill-test-unbreakable-paragraph ()
  ;; See bug#45720 and bug#53537.
  :expected-result :failed
  (with-temp-buffer
    (let ((string "aaa =   baaaaaaaaaaaaaaaaaaaaaaaaaaaa\n"))
      (insert string)
      (goto-char (point-min))
      (search-forward "b")
      (let* ((pos (point))
             (beg (pos-bol))
             (end (pos-eol))
             (fill-prefix (make-string (- pos beg) ?\s))
             ;; `fill-column' is too small to accommodate the current line
             (fill-column (- end beg 10)))
        (fill-region-as-paragraph beg end nil nil pos))
      (should (equal (buffer-string) string)))))

(ert-deftest fill-test-breakable-paragraph ()
  (with-temp-buffer
    (let ((string "aaa =   baaaaaaaa aaaaaaaaaa aaaaaaaaaa\n"))
      (insert string)
      (goto-char (point-min))
      (search-forward "b")
      (let* ((pos (point))
             (beg (pos-bol))
             (end (pos-eol))
             (fill-prefix (make-string (- pos beg) ?\s))
             ;; `fill-column' is too small to accommodate the current line
             (fill-column (- end beg 10)))
        (fill-region-as-paragraph beg end nil nil pos))
      (should (equal
               (buffer-string)
               "aaa =   baaaaaaaa aaaaaaaaaa\n         aaaaaaaaaa\n")))))

(ert-deftest test-fill-end-period ()
  (should
   (equal
    (with-temp-buffer
      (text-mode)
      (auto-fill-mode)
      (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eius.")
      (self-insert-command 1 ?\s)
      (buffer-string))
    "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eius. "))
  (should
   (equal
    (with-temp-buffer
      (text-mode)
      (auto-fill-mode)
      (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eius.Foo")
      (forward-char -3)
      (self-insert-command 1 ?\s)
      (buffer-string))
    "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
eius. Foo")))

(ert-deftest test-fill-haskell ()
  (should
   (equal
    (with-temp-buffer
      (asm-mode)
      (dolist (line '("  ;; a b c"
                      "  ;; d e f"
                      "  ;; x y z"
                      "  ;; w"))
        (insert line "\n"))
      (goto-char (point-min))
      (end-of-line)
      (setf fill-column 10)
      (fill-paragraph nil)
      (buffer-string))
    "  ;; a b c
  ;; d e f
  ;; x y z
  ;; w
")))


(ert-deftest fill-test-fill-region-as-paragraph-semlf ()
  "Test the `fill-region-as-paragraph-semlf' function."
  (ert-test-erts-file (ert-resource-file "semlf-fill-region-as-paragraph.erts")
                      (lambda ()
			(setq-local fill-column 35)
			(fill-region-as-paragraph-semlf
                         (point)
                         (progn
                           (goto-char (point-max))
                           (forward-line -1)
                           (beginning-of-line)
                           (point))))))

(ert-deftest fill-test-fill-paragraph-semlf ()
  "Test the `fill-paragraph-semlf' function."
  (ert-test-erts-file (ert-resource-file "semlf.erts")
                      (lambda ()
			(fill-paragraph-semlf))))

(ert-deftest fill-test-fill-paragraph-semlf-fill-paragraph-function ()
  "Test `fill-paragraph-semlf' as `fill-paragraph-function'."
  (ert-test-erts-file (ert-resource-file "semlf-fill-paragraph-function.erts")
                      (lambda ()
			(setq-local fill-paragraph-function #'fill-paragraph-semlf)
			(fill-paragraph))))

(ert-deftest fill-test-fill-paragraph-semlf-justify ()
  "Test the JUSTIFY parameter of the `fill-paragraph-semlf' function."
  (ert-test-erts-file (ert-resource-file "semlf-justify.erts")
                      (lambda ()
			(fill-paragraph-semlf 'justify))))

(ert-deftest fill-test-fill-paragraph-semlf-sentence-end-double-space ()
  "Test the `fill-paragraph-semlf' function with `sentence-end-double-space'."
  (ert-test-erts-file (ert-resource-file "semlf-sentence-end-double-space.erts")
                      (lambda ()
			(setq-local sentence-end-double-space nil)
			(fill-paragraph-semlf))))

(ert-deftest fill-test-fill-paragraph-semlf-fill-column ()
  "Test the `fill-paragraph-semlf' function with `fill-column'."
  (ert-test-erts-file (ert-resource-file "semlf-fill-column.erts")
                      (lambda ()
			(setq-local fill-column 35)
			(fill-paragraph-semlf))))

(ert-deftest fill-test-fill-paragraph-semlf-punctuation-marks ()
  "Test the `fill-paragraph-semlf' function with different punctuation marks."
  (ert-test-erts-file (ert-resource-file "semlf-punctuation-marks.erts")
                      (lambda ()
			(fill-paragraph-semlf))))

(ert-deftest fill-test-fill-paragraph-semlf-twice ()
  "Test to run the `fill-paragraph-semlf' function twice."
  (ert-test-erts-file (ert-resource-file "semlf-twice.erts")
                      (lambda ()
			(goto-char (point-min))
			(fill-paragraph-semlf)
			(goto-char (point-min))
			(fill-paragraph-semlf))))

(ert-deftest fill-test-fill-paragraph-semlf-fill-prefix ()
  "Test the `fill-paragraph-semlf' function with different fill prefixes."
  (ert-test-erts-file (ert-resource-file "semlf-fill-prefix.erts")
                      (lambda ()
			(fill-paragraph-semlf))))

(ert-deftest fill-test-fill-paragraph-semlf-indented-block ()
  "Test the `fill-paragraph-semlf' function with an indented block."
  (ert-test-erts-file (ert-resource-file "semlf-indented-block.erts")
                      (lambda ()
			(fill-paragraph-semlf))))

(ert-deftest fill-test-fill-paragraph-semlf-revert ()
  "Test that the `fill-paragraph-semlf' function can be reverted."
  (ert-test-erts-file (ert-resource-file "semlf-revert.erts")
                      (lambda ()
                        (fill-paragraph)
			(fill-paragraph-semlf)
                        (fill-paragraph))))

(ert-deftest fill-test-fill-paragraph-semlf-emacs-lisp-mode ()
  "Test the `fill-paragraph-semlf' function with `emacs-lisp-mode'."
  (ert-test-erts-file (ert-resource-file "semlf-emacs-lisp-mode.erts")
                      (lambda ()
			(emacs-lisp-mode)
			(fill-paragraph-semlf))))

(ert-deftest fill-test-fill-paragraph-semlf-c-mode ()
  "Test the `fill-paragraph-semlf' function with `c-mode'."
  (ert-test-erts-file (ert-resource-file "semlf-c-mode.erts")
                      (lambda ()
			(c-mode)
			(fill-paragraph-semlf))))

(ert-deftest fill-test-fill-paragraph-semlf-org-mode ()
  "Test the `fill-paragraph-semlf' function with `org-mode'."
  (ert-test-erts-file (ert-resource-file "semlf-org-mode.erts")
                      (lambda ()
			(org-mode)
			(fill-paragraph-semlf))))

(ert-deftest fill-test-fill-paragraph-semlf-markdown-mode ()
  "Test the `fill-paragraph-semlf' function with `markdown-mode'."
  (skip-unless (functionp 'markdown-mode))
  (ert-test-erts-file (ert-resource-file "semlf-markdown-mode.erts")
                      (lambda ()
			(markdown-mode)
			(fill-paragraph-semlf))))

(provide 'fill-tests)

;;; fill-tests.el ends here
