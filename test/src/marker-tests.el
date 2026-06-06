;;; marker-tests.el --- tests for marker.c functions -*- lexical-binding: t -*-

;; Copyright (C) 2016-2026 Free Software Foundation, Inc.

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

;; The following three tests assert that Emacs survives operations
;; copying a marker whose character position differs from its byte
;; position into a buffer whose character size equals its byte size
;; (Bug#24368).

(ert-deftest marker-set-window-start-from-other-buffer ()
  "`set-window-start' from other buffer's marker."
  (let ((text-quoting-style 'curve))
    (describe-function 'describe-function))
  (let* ((help (get-buffer "*Help*"))
         (marker (with-current-buffer help
                   (copy-marker (point-max)))))
    (should (set-window-start (selected-window) marker))))

(ert-deftest marker-set-window-point-from-other-buffer ()
  "`set-window-point' from another buffer's marker."
  (let ((text-quoting-style 'curve))
    (describe-function 'describe-function))
  (let* ((help (get-buffer "*Help*"))
         (marker (with-current-buffer help
                   (copy-marker (point-max)))))
    (with-selected-window (get-buffer-window help)
      (should (set-window-point (get-buffer-window "*scratch*") marker)))))

(ert-deftest marker-goto-char-from-other-buffer ()
  "`goto-char' from another buffer's marker."
  (let ((text-quoting-style 'curve))
    (describe-function 'describe-function))
  (let ((marker-1 (make-marker))
        (marker-2 (make-marker)))
    (describe-function 'describe-function)
    (with-current-buffer "*Help*"
      (set-marker marker-1 (point-max)))
    (set-marker marker-2 marker-1)
    (should (goto-char marker-2))))

(ert-deftest marker-markers-in ()
  (with-temp-buffer
    (insert "hello ")
    (let ((m2 (point-marker)))
      (insert "world")
      (let ((m1 (point-min-marker))
            (m3 (point-max-marker))
            (ms (markers-in (1+ (point-min)) (1- (point-max)))))
        (should (memq m2 ms))
        (should-not (memq m1 ms))
        (should-not (memq m3 ms))
        (should (all (lambda (m) (eq (marker-buffer m) (current-buffer))) ms))))))

;;; marker-tests.el ends here
