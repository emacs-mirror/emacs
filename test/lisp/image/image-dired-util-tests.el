;;; image-dired-util-tests.el --- Tests for image-dired.el  -*- lexical-binding: t -*-

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
(require 'ert-x)
(require 'image-dired)
(require 'image-dired-util)
(require 'xdg)

(ert-deftest image-dired-thumb-name/standard ()
  (let ((image-dired-thumbnail-storage 'standard))
    (should (file-name-absolute-p (image-dired-thumb-name "foo.jpg")))
    (should (file-name-absolute-p (image-dired-thumb-name "/tmp/foo.jpg")))
    (should (equal
             (file-name-directory (image-dired-thumb-name "foo.jpg"))
             (file-name-directory (image-dired-thumb-name "/tmp/foo.jpg"))))
    (should (string-search (xdg-cache-home)
                           (image-dired-thumb-name "foo.jpg")))
    (should (string-match (rx (in "0-9a-f") ".png")
                          (image-dired-thumb-name "foo.jpg")))))

(ert-deftest image-dired-thumb-name/image-dired ()
  ;; Avoid trying to create `image-dired-dir'.
  (ert-with-temp-directory dir
    (let ((image-dired-dir dir)
          (image-dired-thumbnail-storage 'image-dired))
      (should (file-name-absolute-p (image-dired-thumb-name "foo.jpg")))
      (should (file-name-absolute-p (image-dired-thumb-name "/tmp/foo.jpg")))
      (should (equal
               (file-name-directory (image-dired-thumb-name "foo.jpg"))
               (file-name-directory (image-dired-thumb-name "/tmp/foo.jpg"))))
      (should
       (let* ((test-fn "/some/path/foo.jpg")
              (thumb-fn (image-dired-thumb-name test-fn)))
         (equal (file-name-nondirectory thumb-fn)
                (concat (sha1 (expand-file-name test-fn)) ".jpg"))))
      (should (equal (file-name-extension
                      (image-dired-thumb-name "foo.gif"))
                     "jpg")))))

(ert-deftest image-dired-thumb-name/per-directory ()
  (let ((image-dired-thumbnail-storage 'per-directory))
    (should (file-name-absolute-p (image-dired-thumb-name "foo.jpg")))
    (should (file-name-absolute-p (image-dired-thumb-name "/tmp/foo.jpg")))
    (should (equal
             (file-name-nondirectory (image-dired-thumb-name "foo.jpg"))
             (file-name-nondirectory (image-dired-thumb-name "/tmp/foo.jpg"))))
    ;; The cdr below avoids the system dependency in the car of the
    ;; list returned by 'file-name-split': it's "" on Posix systems,
    ;; but the drive letter on MS-Windows.
    (should (equal (cdr (file-name-split
                         (image-dired-thumb-name "/tmp/foo.jpg")))
                   '("tmp" ".image-dired" "foo.jpg.thumb.jpg")))
    (should (equal (file-name-nondirectory
                    (image-dired-thumb-name "foo.jpg"))
                   "foo.jpg.thumb.jpg"))))

;;; image-dired-util-tests.el ends here
