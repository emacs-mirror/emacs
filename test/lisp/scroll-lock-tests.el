;;; scroll-lock-tests.el --- Test suite for scroll-lock -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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
(require 'scroll-lock)


(defun point-in-window-line-p (n)
  "Return if point is in window line N.
Meaning of N as in `move-to-window-line'.
Precondition: the line N must be available in the window."
  (save-excursion
    (let ((point (progn (beginning-of-line) (point))))
      (let ((moved-to-line (move-to-window-line n)))
        (cl-assert (= n moved-to-line) t "precondition violation"))
      (= point (progn (beginning-of-line) (point))))))


(ert-deftest scroll-lock-next-line-always-scroll-1 ()
  "Point stays in top line."
  (with-temp-buffer
    (insert "\n\n\n")
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))
    (scroll-lock-next-line-always-scroll)
    (should (point-in-window-line-p 0))))

(ert-deftest scroll-lock-next-line-always-scroll-2 ()
  "Point stays in second line."
  (with-temp-buffer
    (scroll-lock-mode)
    (insert "\n\n\n")
    (goto-char (1+ (point-min)))
    (switch-to-buffer (current-buffer))
    (scroll-lock-next-line-always-scroll)
    (should (point-in-window-line-p 1))))

(ert-deftest scroll-lock-next-line-always-scroll-3 ()
  "Point stays in second line when scrolling beyond the number of buffer lines."
  (with-temp-buffer
    (scroll-lock-mode)
    (insert (make-string 1000 ?\n))
    (goto-char (1+ (point-min)))
    (switch-to-buffer (current-buffer))
    (scroll-lock-next-line-always-scroll 1234)
    (should (point-in-window-line-p 1))))

(provide 'scroll-lock-tests)

;;; scroll-lock-tests.el ends here
