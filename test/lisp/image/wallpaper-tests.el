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

(ert-deftest wallpaper--find-command/return-string ()
  (should (and (wallpaper--find-command)
               (stringp (wallpaper--find-command)))))

(ert-deftest wallpaper--find-command-args/return-list ()
  (should (and (wallpaper--find-command-args)
               (listp (wallpaper--find-command-args)))))

(ert-deftest wallpaper--image-file-regexp/return-string ()
  (should (stringp (wallpaper--image-file-regexp))))

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

(ert-deftest wallpaper--format-arg/filename ()
  (should (file-name-absolute-p (wallpaper--format-arg "%f" "foo.jpg"))))

(ert-deftest wallpaper--format-arg/filename-hex ()
  (should (equal (wallpaper--format-arg "%F" "foo bar åäö.jpg")
                 "foo%20bar%20%C3%A5%C3%A4%C3%B6.jpg")))

(ert-deftest wallpaper--format-arg/width ()
  (skip-unless noninteractive)
  (should (equal (wallpaper--format-arg "%w" "foo.jpg")
                 (number-to-string wallpaper-default-width))))

(ert-deftest wallpaper--format-arg/height ()
  (skip-unless noninteractive)
  (should (equal (wallpaper--format-arg "%h" "foo.jpg")
                 (number-to-string wallpaper-default-height))))

(ert-deftest wallpaper--format-arg/screen ()
  (skip-unless noninteractive)
  (should (equal (wallpaper--format-arg "%S" "foo.jpg") "0")))

(ert-deftest wallpaper--format-arg/monitor ()
  (skip-unless noninteractive)
  (should (equal (wallpaper--format-arg "%M" "foo.jpg") "0")))

(ert-deftest wallpaper--format-arg/workspace ()
  (skip-unless noninteractive)
  (should (equal (wallpaper--format-arg "%W" "foo.jpg") "0")))

;;; wallpaper-tests.el ends here
