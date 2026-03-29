;;; savehist-tests.el --- Tests for savehist.el  -*- lexical-binding:t -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

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
  (defvar savehist-tests--t1)
  (defvar savehist-tests--t2)
  (ert-with-temp-file tmpfile
    (with-suppressed-warnings ((obsolete savehist-loaded))
      (let* ((savehist-file tmpfile)
             (savehist-save-minibuffer-history t)
             (savehist-save-hook)
             (savehist-loaded)
             (savehist-minibuffer-history-variables)
             (savehist-additional-variables '(savehist-tests--t2))
             (savehist-ignored-variables '(t3))
             (savehist-tests--t1 '("t1-value"))
             (savehist-tests--t2 '("t2-value"))
             (t3 '("t3-value"))
             (t1-copy (copy-tree savehist-tests--t1))
             (t2-copy (copy-tree savehist-tests--t2))
             (_t3-copy (copy-tree t3))
             (save-var (lambda (x)
                         (let ((minibuffer-history-variable x))
                           (savehist-minibuffer-hook)))))
        (savehist-mode)
        (funcall save-var 'savehist-tests--t1)
        (funcall save-var 'savehist-tests--t2)
        (funcall save-var 't3) ; should be ignored
        (savehist-save)
        (setq savehist-tests--t1 nil savehist-tests--t2 nil t3 nil)
        (progn
          ;; Force reloading the file.
          (savehist-mode -1)
          (setq savehist-loaded nil)
          (savehist-mode))
        (should (equal savehist-tests--t1 t1-copy))
        (should (equal savehist-tests--t2 t2-copy))
        (should (equal t3 nil))))))

(ert-deftest savehist-test-duplicated-saved-symbols ()
  (defvar savehist-tests--t1)
  (defvar savehist-tests--t2)
  (ert-with-temp-file tmpfile
    (with-suppressed-warnings ((obsolete savehist-loaded))
      (let* ((savehist-file tmpfile)
             (savehist-save-minibuffer-history t)
             (savehist-save-hook)
             (savehist-loaded)
             ;; Will be '(savehist-tests--t2 savehist-tests--t1)
             (savehist-minibuffer-history-variables)
             ;; `savehist-tests--t2' should not be saved twice.
             (savehist-additional-variables '(savehist-tests--t2))
             (savehist-tests--t1 '("t1-value"))
             (savehist-tests--t2 '("t2-value"))
             (save-var (lambda (x)
                         (let ((minibuffer-history-variable x))
                           (savehist-minibuffer-hook)))))
        (savehist-mode)
        (funcall save-var 'savehist-tests--t1)
        (funcall save-var 'savehist-tests--t2)
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
                     (length (seq-uniq saved-variables #'equal)))))))))

(provide 'savehist-tests)
;;; savehist-tests.el ends here
