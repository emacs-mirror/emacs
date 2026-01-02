;;; fill-tests.el --- ERT tests for fill.el -*- lexical-binding: t -*-

;; Copyright (C) 2017-2026 Free Software Foundation, Inc.

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

(ert-deftest fill-test-end-period ()
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

(ert-deftest fill-test-haskell ()
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

(ert-deftest fill-test-fill-region-as-paragraph-default ()
  "Test the `fill-region-as-paragraph-default' function."
  (ert-test-erts-file (ert-resource-file "fill-region-as-paragraph-default.erts")))

(ert-deftest fill-test-fill-region-as-paragraph-semlf ()
  "Test the `fill-region-as-paragraph-semlf' function."
  (ert-test-erts-file (ert-resource-file "fill-region-as-paragraph-semlf.erts")))

(ert-deftest fill-test-fill-region-as-paragraph ()
  "Test the `fill-region-as-paragraph' function."
  (ert-test-erts-file (ert-resource-file "fill-region-as-paragraph.erts")))

(ert-deftest fill-test-fill-region ()
  "Test the `fill-region' function."
  (ert-test-erts-file (ert-resource-file "fill-region.erts")))

(ert-deftest fill-test-fill-paragraph ()
  "Test the `fill-paragraph' function."
  (ert-test-erts-file (ert-resource-file "fill-paragraph.erts")))

(ert-deftest fill-test-fill-paragraph-semlf ()
  "Test the `fill-paragraph-semlf' function."
  (ert-test-erts-file (ert-resource-file "fill-paragraph-semlf.erts")))

(ert-deftest fill-test-fill-paragraph-semlf-emacs-lisp-mode ()
  "Test the `fill-paragraph-semlf' function in `emacs-lisp-mode'."
  (ert-test-erts-file (ert-resource-file "fill-paragraph-semlf-emacs-lisp-mode.erts")))

(ert-deftest fill-test-fill-paragraph-semlf-c-mode ()
  "Test the `fill-paragraph-semlf' function in `c-mode'."
  (ert-test-erts-file (ert-resource-file "fill-paragraph-semlf-c-mode.erts")))

(ert-deftest fill-test-fill-paragraph-semlf-org-mode ()
  "Test the `fill-paragraph-semlf' function in `org-mode'."
  (ert-test-erts-file (ert-resource-file "fill-paragraph-semlf-org-mode.erts")))

(ert-deftest fill-test-fill-paragraph-semlf-markdown-mode ()
  "Test the `fill-paragraph-semlf' function in `markdown-mode'."
  (skip-unless (functionp 'markdown-mode))
  (ert-test-erts-file (ert-resource-file "fill-paragraph-semlf-markdown-mode.erts")))

(provide 'fill-tests)

;;; fill-tests.el ends here
