;;; paragraphs-tests.el --- Tests for paragraphs.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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
;; (require 'paragraphs) ; loaded by default

(ert-deftest paragraphs-tests-sentence-end ()
  (should (> (length (sentence-end)) 0))
  (let ((sentence-end "override works"))
    (should (equal (sentence-end) sentence-end))))

(ert-deftest paragraphs-tests-forward-backward-paragraph ()
  (with-temp-buffer
    (insert "AA\nAA\n\nBB\nBB\n")
    (goto-char (point-min))
    (forward-paragraph)
    (should (equal (point) 7))
    (forward-paragraph)
    (should (equal (point) 14))
    (backward-paragraph)
    (should (equal (point) 7))
    (backward-paragraph)
    (should (equal (point) (point-min)))))

(ert-deftest paragraphs-tests-mark-paragraph ()
  (with-temp-buffer
    (insert "AA\nAA\n\nBB\nBB\n")
    (goto-char (point-min))
    (mark-paragraph)
    (should mark-active)
    (should (equal (mark) 7)))
  (should-error (mark-paragraph 0)))

(ert-deftest paragraphs-tests-kill-paragraph ()
  (with-temp-buffer
    (insert "AA\nAA\n\nBB\nBB\n")
    (goto-char (point-min))
    (kill-paragraph nil)
    (should (equal (buffer-string) "\nBB\nBB\n"))))

(ert-deftest paragraphs-tests-backward-kill-paragraph ()
  (with-temp-buffer
    (insert "AA\nAA\n\nBB\nBB\n")
    (goto-char 7)
    (backward-kill-paragraph nil)
    (should (equal (buffer-string) "\nBB\nBB\n"))))

(ert-deftest paragraphs-tests-transpose-paragraphs ()
  (with-temp-buffer
    (insert "AA\nAA\n\nBB\nBB\n")
    (goto-char (point-min))
    (transpose-paragraphs 1)
    (should (equal (buffer-string) "\nBB\nBB\nAA\nAA\n"))))

(ert-deftest paragraphs-tests-start-of-paragraph-text ()
  (with-temp-buffer
    (insert "AA\nAA\n\nBB\nBB\n")
    (goto-char (point-max))
    (start-of-paragraph-text)
    (should (equal (point) 8))))

(ert-deftest paragraphs-tests-end-of-paragraph-text ()
  (with-temp-buffer
    (insert "AA\nAA\n\nBB\nBB\n")
    (goto-char (point-min))
    (end-of-paragraph-text)
    (should (equal (point) 6))))

(ert-deftest paragraphs-tests-forward-sentence ()
  (with-temp-buffer
    (insert "First sentence.  Second sentence.")
    (goto-char (point-min))
    (forward-sentence)
    (should (equal (point) 16))
    (goto-char (point-min))
    (forward-sentence 2)
    (should (equal (point) 34))))

(ert-deftest paragraphs-tests-repunctuate-sentences ()
  (with-temp-buffer
    (insert "Just. Some. Sentences.")
    (goto-char (point-min))
    (repunctuate-sentences t)
    (should (equal (buffer-string) "Just.  Some.  Sentences."))))

(ert-deftest paragraphs-tests-backward-sentence ()
  (with-temp-buffer
    (insert "First sentence.  Second sentence.")
    (goto-char (point-max))
    (backward-sentence)
    (should (equal (point) 18))))

(ert-deftest paragraphs-tests-kill-sentence ()
  (with-temp-buffer
    (insert "First sentence.  Second sentence.")
    (goto-char (point-min))
    (kill-sentence)
    (should (equal (buffer-string) "  Second sentence."))))

(ert-deftest paragraphs-tests-backward-kill-sentence ()
  (with-temp-buffer
    (insert "Should not be killed.  Should be killed.")
    (goto-char (point-max))
    (backward-kill-sentence)
    (should (equal (buffer-string) "Should not be killed.  "))))

(ert-deftest paragraphs-tests-mark-end-of-sentence ()
  (with-temp-buffer
    (insert "Example sentence.  Followed by another one.")
    (goto-char (point-min))
    (mark-end-of-sentence 1)
    (should mark-active)
    (should (equal (mark) 18)))
  (with-temp-buffer
    (insert "Example sentence.  Followed by another one.")
    (goto-char (point-min))
    (mark-end-of-sentence 2)
    (should mark-active)
    (should (equal (mark) 44)))
  ;; FIXME: This does not work -- how do I do it?
  (with-temp-buffer ; test repeating the command
    (insert "Example sentence.  Followed by another one.")
    (goto-char (point-min))
    (mark-end-of-sentence 1)
    (setq last-command 'mark-end-of-sentence) ; hack
    (mark-end-of-sentence 1)
    (should mark-active)
    (should (equal (mark) 18))))

(ert-deftest paragraphs-tests-transpose-sentences ()
  (with-temp-buffer
    (insert "First sentence.  Second sentence.  Third sentence.")
    (goto-char (point-min))
    (transpose-sentences 1)
    (should (equal (buffer-string)
                   "Second sentence.  First sentence.  Third sentence."))
    (goto-char (point-min))
    (transpose-sentences 2)
    (should (equal (buffer-string)
                   "First sentence.  Third sentence.  Second sentence."))))

(provide 'paragraphs-tests)
;;; paragraphs-tests.el ends here
