;;; easy-mmode-tests.el --- tests for easy-mmode.el  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
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
(require 'easy-mmode)
(require 'message)

(ert-deftest easy-mmode--globalized-predicate ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (eq (easy-mmode--globalized-predicate-p nil) nil))
    (should (eq (easy-mmode--globalized-predicate-p t) t))
    (should (eq (easy-mmode--globalized-predicate-p '(not text-mode)) t))
    (should (eq (easy-mmode--globalized-predicate-p '(not text-mode)) t))
    (should (eq (easy-mmode--globalized-predicate-p '((not text-mode))) nil))
    (should (eq (easy-mmode--globalized-predicate-p '((not text-mode) t)) t))
    (should (eq (easy-mmode--globalized-predicate-p
                 '(c-mode emacs-lisp-mode))
                t))
    (mail-mode)
    (should (eq (easy-mmode--globalized-predicate-p
                 '(c-mode (not message-mode mail-mode) text-mode))
                nil))
    (text-mode)
    (should (eq (easy-mmode--globalized-predicate-p
                 '(c-mode (not message-mode mail-mode) text-mode))
                t))))

(define-minor-mode easy-mmode-test-mode "A test.")

(ert-deftest easy-mmode--minor-mode ()
  (with-temp-buffer
    (should (eq easy-mmode-test-mode nil))
    (easy-mmode-test-mode nil)
    (should (eq easy-mmode-test-mode t))
    (easy-mmode-test-mode -33)
    (should (eq easy-mmode-test-mode nil))
    (easy-mmode-test-mode 33)
    (should (eq easy-mmode-test-mode t))
    (easy-mmode-test-mode 'toggle)
    (should (eq easy-mmode-test-mode nil))
    (easy-mmode-test-mode 'toggle)
    (should (eq easy-mmode-test-mode t))))


;;;; Globalized minor modes

;; Globalized minor modes can either enable or decline to enable their
;; minor mode for a given buffer.  That means you can have more than one
;; globalized version of a single minor mode, where they enable it in
;; different buffers.  This combination is legitimate because globalized
;; minor modes don't ever *disable* the minor mode in a buffer, so the
;; only possibilities are that either one or more of them enable the
;; minor mode, or none of them do.
;;
;; We have had problems with a particular case of this with two
;; globalized versions of a minor mode where the buffers in which one
;; globalized mode enables the minor mode are a strict superset of the
;; buffers in which the other globalized mode enables the minor mode:
;; `magit-auto-revert-mode' enables `auto-revert-mode' for all
;; Git-managed files and `vc-auto-revert-mode' enables it for all VCS
;; managed files with any VC backend.  These tests are to assert that
;; this combination still works.
;;
;; As an additional complication, `auto-revert-mode' is marked
;; permanent-local, so we involve that in the test, again to assert that
;; the combination still works.

(put 'easy-mmode-test-mode 'permanent-local t)

(with-suppressed-warnings ((redefine easy-mmode-test-mode--set-explicitly))

 ;; More selective globalized major mode: like `magit-auto-revert-mode'.
 (define-globalized-minor-mode easy-mmode-test-globalized-mode-1
   easy-mmode-test-mode
   (lambda ()
     (when (string-search "both" (buffer-name))
       (easy-mmode-test-mode 1)))
   :group 'test-group)

 ;; Less selective globalized major mode: like `vc-auto-revert-mode'.
 (define-globalized-minor-mode easy-mmode-test-globalized-mode-2
   easy-mmode-test-mode
   (lambda ()
     (when (string-match "both\\|only" (buffer-name))
       (easy-mmode-test-mode 1)))
   :group 'test-group))

(defun easy-mmode--test-two-globalized-modes ()
  (let ((only-file (expand-file-name (make-temp-name "only")
                                     temporary-file-directory))
        (both-file (expand-file-name (make-temp-name "both")
                                     temporary-file-directory)))
    (unwind-protect
        (progn
          (with-current-buffer (find-file-noselect only-file)
            (should easy-mmode-test-mode))
          (with-current-buffer (find-file-noselect both-file)
            (should easy-mmode-test-mode)))
      (delete-file only-file)
      (delete-file both-file))))

;; This is the case that the introduction of the
;; MODE-suppress-set-explicitly mechanism was intended to fix.
(ert-deftest easy-mmode--more-selective-first ()
  "Test with the more selective globalized mode going first."
  (easy-mmode-test-globalized-mode-1 -1)
  (easy-mmode-test-globalized-mode-2 -1)
  (easy-mmode-test-globalized-mode-2 1)
  (easy-mmode-test-globalized-mode-1 1)
  (should (memq 'easy-mmode-test-globalized-mode-2-enable-in-buffer
                (memq 'easy-mmode-test-globalized-mode-1-enable-in-buffer
                      after-change-major-mode-hook)))
  (easy-mmode--test-two-globalized-modes))

(ert-deftest easy-mmode--less-selective-first ()
  "Test with the less selective globalized mode going first."
  (easy-mmode-test-globalized-mode-1 -1)
  (easy-mmode-test-globalized-mode-2 -1)
  (easy-mmode-test-globalized-mode-1 1)
  (easy-mmode-test-globalized-mode-2 1)
  (should (memq 'easy-mmode-test-globalized-mode-1-enable-in-buffer
                (memq 'easy-mmode-test-globalized-mode-2-enable-in-buffer
                      after-change-major-mode-hook)))
  (easy-mmode--test-two-globalized-modes))

;;; easy-mmode-tests.el ends here
