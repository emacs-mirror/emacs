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

(ert-deftest marker-tests--insertion-type ()
  (with-temp-buffer
    (insert "ab")
    (goto-char 2)
    (let ((m1 (point-marker))
          (m2 (copy-marker (point) t)))
      (should-not (marker-insertion-type m1))
      (should (marker-insertion-type m2))
      (insert "X")
      (should (= (marker-position m1) 2))
      (should (= (marker-position m2) 3)))))

(ert-deftest marker-tests--set-marker-insertion-type ()
  (with-temp-buffer
    (insert "ab")
    (goto-char 2)
    (let ((m (point-marker)))
      (should-not (marker-insertion-type m))
      (should (eq (set-marker-insertion-type m t) t))
      (should (marker-insertion-type m))
      (insert "X")
      (should (= (marker-position m) 3))
      (goto-char (marker-position m))
      (should (eq (set-marker-insertion-type m nil) nil))
      (should-not (marker-insertion-type m))
      (insert "Y")
      (should (= (marker-position m) 3)))))

(ert-deftest marker-tests--copy-marker-nil ()
  (let ((m (copy-marker nil)))
    (should-not (marker-buffer m))
    (should-not (marker-position m))))

(ert-deftest marker-tests--marker-buffer ()
  (with-temp-buffer
    (let ((m (make-marker)))
      (should-not (marker-buffer m))
      (set-marker m (point) (current-buffer))
      (should (eq (marker-buffer m) (current-buffer)))
      (set-marker m nil)
      (should-not (marker-buffer m)))))

(ert-deftest marker-tests--last-position-after-kill ()
  (let (marker pos)
    (with-temp-buffer
      (insert "abc")
      (setq marker (point-marker))
      (setq pos (point))
      (should (= (marker-position marker) pos)))
    (should-not (marker-buffer marker))
    (should-not (marker-position marker))
    (should (= (marker-last-position marker) pos))))

(ert-deftest marker-tests--copy-marker ()
  (with-temp-buffer
    (insert "abc")
    (goto-char 2)
    (let ((m1 (point-marker))
          (m2 (copy-marker (point) t))
          (m3 (copy-marker (point) nil))
          (m4 (copy-marker 1)))
      (should (equal m1 m2))
      (should (eq (marker-buffer m1) (marker-buffer m2)))
      (should (marker-insertion-type m2))
      (should-not (marker-insertion-type m3))
      (should (eq (marker-buffer m4) (current-buffer)))
      (should (= (marker-position m4) 1)))))

(ert-deftest marker-tests--set-marker-and-move-marker ()
  (let ((m (make-marker))
        (m2 (make-marker)))
    (with-temp-buffer
      (insert "abc")
      (should (eq (set-marker m 2 (current-buffer)) m))
      (should (eq (marker-buffer m) (current-buffer)))
      (should (= (marker-position m) 2))
      (should (eq (move-marker m 1 (current-buffer)) m))
      (should (= (marker-position m) 1))
      (set-marker m2 nil)
      (set-marker m m2)
      (should-not (marker-buffer m))
      (should-not (marker-position m))
      (set-marker m 1 (current-buffer))
      (set-marker m nil)
      (should-not (marker-buffer m))
      (should-not (marker-position m)))))

(ert-deftest marker-tests--point-min-max-marker-narrowing ()
  (with-temp-buffer
    (insert "abcd")
    (narrow-to-region 2 3)
    (let ((minm (point-min-marker))
          (maxm (point-max-marker)))
      (should (= (marker-position minm) 2))
      (should (= (marker-position maxm) 3))
      (should (eq (marker-buffer minm) (current-buffer))))))

(ert-deftest marker-tests--move-marker-between-buffers ()
  (let ((buf-1 (generate-new-buffer " *marker-tests-1*"))
        (buf-2 (generate-new-buffer " *marker-tests-2*")))
    (unwind-protect
        (let ((m (make-marker)))
          (with-current-buffer buf-1
            (insert "abc")
            (set-marker m 2 (current-buffer)))
          (should (eq (marker-buffer m) buf-1))
          (should (= (marker-position m) 2))
          (with-current-buffer buf-2
            (insert "xyz")
            (set-marker m 1 (current-buffer)))
          (should (eq (marker-buffer m) buf-2))
          (should (= (marker-position m) 1)))
      (kill-buffer buf-1)
      (kill-buffer buf-2))))

;;; marker-tests.el ends here
