;;; hl-line-tests.el --- Test suite for hl-line. -*- lexical-binding: t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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

(ert-deftest hl-line-sticky ()
  (should hl-line-sticky-flag)
  (with-temp-buffer
    (let ((from-buffer (current-buffer)))
      (hl-line-mode 1)
      (save-excursion
        (insert "foo"))
      (hl-line-highlight)
      (should (cl-some (apply-partially #'eq hl-line--overlay)
                       (overlays-at (point))))
      (switch-to-buffer (get-buffer-create "*scratch*"))
      (hl-line-mode 1)
      (save-excursion
        (insert "bar"))
      (hl-line-highlight)
      (should (cl-some (apply-partially #'eq hl-line--overlay)
                       (overlays-at (point))))
      (should (buffer-local-value 'hl-line--overlay from-buffer))
      (should-not (eq (buffer-local-value 'hl-line--overlay from-buffer)
                      hl-line--overlay))
      (customize-set-variable 'hl-line-sticky-flag nil)
      (should hl-line--overlay)
      (should (buffer-live-p from-buffer))
      (should-not (buffer-local-value 'hl-line--overlay from-buffer)))))

(provide 'hl-line-tests)

;;; hl-line-tests.el ends here
