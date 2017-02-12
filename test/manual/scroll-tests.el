;;; scroll-tests.el -- tests for scrolling -*- lexical-binding: t -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; These are mostly automated ert tests, but they don't work in batch
;; mode which is why they are under test/manual.

;;; Code:

(require 'ert)
(eval-when-compile (require 'cl-lib))

(defun scroll-tests-up-and-down (margin &optional effective-margin)
  (unless effective-margin
    (setq effective-margin margin))
  (erase-buffer)
  (insert (mapconcat #'number-to-string
                     (number-sequence 1 200) "\n"))
  (goto-char 1)
  (sit-for 0)
  (let ((scroll-margin margin)
        (wstart (window-start)))
    ;; Stopping before `scroll-margin' so we shouldn't have
    ;; scrolled.
    (let ((current-prefix-arg (- (window-text-height) 1 effective-margin)))
      (call-interactively 'next-line))
    (sit-for 0)
    (should (= wstart (window-start)))
    ;; Passing `scroll-margin' should trigger scrolling.
    (call-interactively 'next-line)
    (sit-for 0)
    (should (/= wstart (window-start)))
    ;; Scroll back to top.
    (let ((current-prefix-arg (window-start)))
      (call-interactively 'scroll-down-command))
    (sit-for 0)
    (should (= 1 (window-start)))))

(defmacro scroll-tests-with-buffer-window (&rest body)
  (declare (debug t))
  `(with-temp-buffer
     (with-selected-window (display-buffer (current-buffer))
       ,@body)))

(ert-deftest scroll-tests-scroll-margin-0 ()
  (skip-unless (not noninteractive))
  (scroll-tests-with-buffer-window
   (scroll-tests-up-and-down 0)))

(ert-deftest scroll-tests-scroll-margin-negative ()
  "A negative `scroll-margin' should be the same as 0."
  (skip-unless (not noninteractive))
  (scroll-tests-with-buffer-window
   (scroll-tests-up-and-down -10 0)))

(ert-deftest scroll-tests-scroll-margin-max ()
  (skip-unless (not noninteractive))
  (scroll-tests-with-buffer-window
   (let ((max-margin (/ (window-text-height) 4)))
     (scroll-tests-up-and-down max-margin))))

(ert-deftest scroll-tests-scroll-margin-over-max ()
  "A `scroll-margin' more than max should be the same as max."
  (skip-unless (not noninteractive))
  (scroll-tests-with-buffer-window
   (set-window-text-height nil 7)
   (let ((max-margin (/ (window-text-height) 4)))
     (scroll-tests-up-and-down (+ max-margin 1) max-margin)
     (scroll-tests-up-and-down (+ max-margin 2) max-margin))))

(defun scroll-tests--point-in-middle-of-window-p ()
  (= (count-lines (window-start) (window-point))
     (/ (1- (window-text-height)) 2)))

(cl-defun scroll-tests--scroll-margin-whole-window (&key with-line-spacing)
  "Test `maximum-scroll-margin' at 0.5.
With a high `scroll-margin', this should keep cursor in the
middle of the window."
  (let  ((maximum-scroll-margin 0.5)
         (scroll-margin 100))
    (scroll-tests-with-buffer-window
     (setq-local line-spacing with-line-spacing)
     ;; Choose an odd number, so there is one line in the middle.
     (set-window-text-height nil 7)
     ;; `set-window-text-height' doesn't count `line-spacing'.
     (when with-line-spacing
       (window-resize nil (* line-spacing 7) nil nil 'pixels))
     (erase-buffer)
     (insert (mapconcat #'number-to-string
                        (number-sequence 1 200) "\n"))
     (goto-char 1)
     (sit-for 0)
     (call-interactively 'scroll-up-command)
     (sit-for 0)
     (should (scroll-tests--point-in-middle-of-window-p))
     (call-interactively 'scroll-up-command)
     (sit-for 0)
     (should (scroll-tests--point-in-middle-of-window-p))
     (call-interactively 'scroll-down-command)
     (sit-for 0)
     (should (scroll-tests--point-in-middle-of-window-p)))))

(ert-deftest scroll-tests-scroll-margin-whole-window ()
  (skip-unless (not noninteractive))
  (scroll-tests--scroll-margin-whole-window))

(ert-deftest scroll-tests-scroll-margin-whole-window-line-spacing ()
  ;; `line-spacing' has no effect on tty displays.
  (skip-unless (display-graphic-p))
  (scroll-tests--scroll-margin-whole-window :with-line-spacing 3))


;;; scroll-tests.el ends here
