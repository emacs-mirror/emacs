;;; savehist-tests.el --- Tests for savehist.el  -*- lexical-binding:t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Stephane Marks <shipmints@gmail.com>

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

;; These tests emulate what `read-from-minibuffer' would do via
;; `savehist-minibuffer-hook' without calling `read-from-minibuffer'.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'savehist)

(ert-deftest savehist-test-saved-variables ()
  ;; These accommodate symbol-value.
  (defvar t1)
  (defvar t2)
  (ert-with-temp-file tmpfile
    (let* ((savehist-file tmpfile)
           (savehist-save-minibuffer-history t)
           (savehist-save-hook)
           (savehist-loaded)
           (savehist-minibuffer-history-variables)
           (savehist-additional-variables '(t2))
           (savehist-ignored-variables '(t3))
           (t1 '("t1-value"))
           (t2 '("t2-value"))
           (t3 '("t3-value"))
           (t1-copy (copy-tree t1))
           (t2-copy (copy-tree t2))
           (t3-copy (copy-tree t3))
           (save-var (lambda (x)
                       (let ((minibuffer-history-variable x))
                         (savehist-minibuffer-hook)))))
      (savehist-mode)
      (funcall save-var 't1)
      (funcall save-var 't2)
      (funcall save-var 't3) ; should be ignored
      (savehist-save)
      (setq t1 nil t2 nil t3 nil)
      (progn
        ;; Force reloading the file.
        (savehist-mode -1)
        (setq savehist-loaded nil)
        (savehist-mode))
      (should (equal t1 t1-copy))
      (should (equal t2 t2-copy))
      (should (equal t3 nil)))))

(ert-deftest savehist-test-duplicated-saved-symbols ()
  (defvar t1)
  (defvar t2)
  (ert-with-temp-file tmpfile
    (let* ((savehist-file tmpfile)
           (savehist-save-minibuffer-history t)
           (savehist-save-hook)
           (savehist-loaded)
           (savehist-minibuffer-history-variables) ; will be '(t2 t1)
           (savehist-additional-variables '(t2)) ; t2 should not be saved twice
           (t1 '("t1-value"))
           (t2 '("t2-value"))
           (save-var (lambda (x)
                       (let ((minibuffer-history-variable x))
                         (savehist-minibuffer-hook)))))
      (savehist-mode)
      (funcall save-var 't1)
      (funcall save-var 't2)
      (savehist-save)
      (progn
        ;; Force reloading the file.
        (savehist-mode -1)
        (setq savehist-loaded nil)
        (savehist-mode))
      (let ((saved-variables))
        (with-temp-buffer
          (insert-file-contents tmpfile)
          (goto-char 1)
          ;; alnum bypasses savehist-minibuffer-history-variables
          (while (re-search-forward "(setq \\([[:alnum:]]+\\) " nil t 1)
            (push (match-string 1) saved-variables)))
        (should (= (length saved-variables)
                   (length (seq-uniq saved-variables #'equal))))))))

(provide 'savehist-tests)
;;; savehist-tests.el ends here
