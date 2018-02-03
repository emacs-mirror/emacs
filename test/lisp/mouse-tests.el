;;; mouse-tests.el --- unit tests for mouse.el       -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018 Free Software Foundation, Inc.

;; Author: Philipp Stephani <phst@google.com>

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

;; Unit tests for lisp/mouse.el.

;;; Code:

(ert-deftest bug23288-use-return-value ()
  "If `mouse-on-link-p' returns a string, its first character is used."
  (cl-letf ((unread-command-events '((down-mouse-1 nil 1) (mouse-1 nil 1)))
            (mouse-1-click-follows-link t)
            (mouse-1-click-in-non-selected-windows t)
            ((symbol-function 'mouse-on-link-p) (lambda (_pos) "abc")))
    (should (eq 'down-mouse-1 (car-safe (aref (read-key-sequence "") 0))))
    (should (eq ?a (aref (read-key-sequence "") 0)))))

(ert-deftest bug23288-translate-to-mouse-2 ()
  "If `mouse-on-link-p' doesn't return a string or vector,
translate `mouse-1' events into `mouse-2' events."
  (cl-letf ((unread-command-events '((down-mouse-1 nil 1) (mouse-1 nil 1)))
            (mouse-1-click-follows-link t)
            (mouse-1-click-in-non-selected-windows t)
            ((symbol-function 'mouse-on-link-p) (lambda (_pos) t)))
    (should (eq 'down-mouse-1 (car-safe (aref (read-key-sequence "") 0))))
    (should (eq 'mouse-2 (car-safe (aref (read-key-sequence "") 0))))))

(ert-deftest bug26816-mouse-frame-movement ()
  "Mouse moves relative to frame."
  (skip-unless (display-graphic-p))
  (let ((frame (selected-frame)))
    (set-mouse-position frame 0 0)
    (should (equal (mouse-position)
                   (cons frame (cons 0 0))))))


;;; mouse-tests.el ends here
