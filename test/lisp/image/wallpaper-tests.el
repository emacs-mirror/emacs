;;; wallpaper-tests.el --- tests for wallpaper.el  -*- lexical-binding: t -*-

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
(require 'ert-x)
(require 'wallpaper)

(ert-deftest wallpaper--get-default-file/empty-gives-nil ()
  (with-temp-buffer
    (should-not (wallpaper--get-default-file))))

(ert-deftest wallpaper--get-default-file/visiting-file ()
  (ert-with-temp-file _
    :buffer buf
    :suffix (format ".%s" (car image-file-name-extensions))
    (with-current-buffer buf
      (should (wallpaper--get-default-file)))))

(ert-deftest wallpaper--get-default-file/file-at-point ()
  ;; ffap needs the file to exist
  (ert-with-temp-file fil
    :buffer buf
    :suffix (format ".%s" (car image-file-name-extensions))
    (with-current-buffer buf
      (insert fil)
      (should (stringp (wallpaper--get-default-file))))))

;;; wallpaper-tests.el ends here
