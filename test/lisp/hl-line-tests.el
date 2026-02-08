;;; hl-line-tests.el --- Test suite for hl-line. -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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
(require 'hl-line)

(defsubst hl-line-tests-verify (_label on-p)
  (if on-p
      (cl-some (apply-partially #'eq hl-line-overlay)
               (overlays-at (point)))
    (not (cl-some (apply-partially #'eq hl-line-overlay)
                  (overlays-at (point))))))

(ert-deftest hl-line-tests-sticky-across-frames ()
  (skip-unless (display-graphic-p))
  (customize-set-variable 'global-hl-line-sticky-flag t)
  (call-interactively #'global-hl-line-mode)
  (let ((first-frame (selected-frame))
        (first-buffer "foo")
        (second-buffer "bar")
        second-frame)
    (unwind-protect
        (progn
          (switch-to-buffer first-buffer)
          (save-excursion
            (insert (buffer-name)))
          (run-hooks 'post-command-hook)
          (should (hl-line-tests-verify 111 t))
          (select-frame (setq second-frame (make-frame)))
          (switch-to-buffer second-buffer)
          (save-excursion
            (insert (buffer-name)))
          (run-hooks 'post-command-hook)
          (should (hl-line-tests-verify 762 t))
          (with-current-buffer first-buffer
            (should (hl-line-tests-verify 534 t)))
          (call-interactively #'global-hl-line-mode)
          (should (hl-line-tests-verify 125 nil))
          (with-current-buffer first-buffer
            (should (hl-line-tests-verify 892 nil)))

          ;; now do unsticky
          (customize-set-variable 'hl-line-sticky-flag nil)
          (call-interactively #'global-hl-line-mode)
          (run-hooks 'post-command-hook)
          (should (hl-line-tests-verify 467 t))
          (with-current-buffer first-buffer
            (should (hl-line-tests-verify 765 nil)))
          (select-frame first-frame)
          (should (equal (buffer-name) first-buffer))
          (run-hooks 'post-command-hook)
          (should (hl-line-tests-verify 423 t))
          (with-current-buffer second-buffer
            (should (hl-line-tests-verify 897 nil))))
      (let (kill-buffer-query-functions)
        (ignore-errors (kill-buffer first-buffer))
        (ignore-errors (kill-buffer second-buffer))
        (ignore-errors (delete-frame second-frame))))))

(ert-deftest hl-line-tests-sticky ()
  (customize-set-variable 'hl-line-sticky-flag t)
  (let ((first-buffer "foo")
        (second-buffer "bar"))
    (unwind-protect
        (progn
          (switch-to-buffer first-buffer)
          (hl-line-mode 1)
          (save-excursion
            (insert (buffer-name)))
          (run-hooks 'post-command-hook)
          (should (hl-line-tests-verify 123 t))
          (switch-to-buffer second-buffer)
          (hl-line-mode 1)
          (save-excursion
            (insert (buffer-name)))
          (run-hooks 'post-command-hook)
          (should (hl-line-tests-verify 56 t))
          (with-current-buffer first-buffer
            (should (hl-line-tests-verify 67 t)))

          ;; now do unsticky
          (customize-set-variable 'hl-line-sticky-flag nil)
          (should (hl-line-tests-verify 234 t))
          (with-current-buffer first-buffer
            (should (hl-line-tests-verify 231 nil)))
          (switch-to-buffer first-buffer)
          (run-hooks 'post-command-hook)
          (should (hl-line-tests-verify 257 t))
          (with-current-buffer second-buffer
            (should (hl-line-tests-verify 999 nil))))
      (let (kill-buffer-query-functions)
        (ignore-errors (kill-buffer first-buffer))
        (ignore-errors (kill-buffer second-buffer))))))

(provide 'hl-line-tests)

;;; hl-line-tests.el ends here
