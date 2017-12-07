;;; xdg-tests.el --- tests for xdg.el -*- lexical-binding: t -*-

;; Copyright (C) 2017  Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Author: Mark Oteiza <mvoteiza@udel.edu>

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

;;; Code:

(require 'ert)
(require 'xdg)

(defconst xdg-tests-data-dir
  (expand-file-name "test/data/xdg" source-directory))

(ert-deftest xdg-desktop-parsing ()
  "Test `xdg-desktop-read-file' parsing of .desktop files."
  (let ((tab1 (xdg-desktop-read-file
               (expand-file-name "test.desktop" xdg-tests-data-dir)))
        (tab2 (xdg-desktop-read-file
               (expand-file-name "test.desktop" xdg-tests-data-dir)
               "Another Section")))
    (should (equal (gethash "Name" tab1) "Test"))
    (should (eq 'default (gethash "Exec" tab1 'default)))
    (should (equal "frobnicate" (gethash "Exec" tab2))))
  (should-error
   (xdg-desktop-read-file
    (expand-file-name "malformed.desktop" xdg-tests-data-dir)))
  (let ((tab (xdg-desktop-read-file
              (expand-file-name "l10n.desktop" xdg-tests-data-dir)))
        (env (getenv "LC_MESSAGES")))
    (unwind-protect
        (progn
          (setenv "LC_MESSAGES" nil)
          (should (equal (gethash "Comment" tab) "Cheers"))
          ;; l10n omitted
          (setenv "LC_MESSAGES" "sv_SE.UTF-8")
          (should-not (equal (gethash "Comment" tab) "Skål")))
      (setenv "LC_MESSAGES" env))))

(ert-deftest xdg-desktop-strings-type ()
  "Test desktop \"string(s)\" type: strings delimited by \";\"."
  (should (equal (xdg-desktop-strings " a") '("a")))
  (should (equal (xdg-desktop-strings "a;b") '("a" "b")))
  (should (equal (xdg-desktop-strings "a;b;") '("a" "b")))
  (should (equal (xdg-desktop-strings "\\;") '(";")))
  (should (equal (xdg-desktop-strings ";") '("")))
  (should (equal (xdg-desktop-strings " ") nil))
  (should (equal (xdg-desktop-strings "a; ;") '("a" " "))))

(ert-deftest xdg-mime-associations ()
  "Test reading MIME associations from files."
  (let* ((apps (expand-file-name "mimeapps.list" xdg-tests-data-dir))
         (cache (expand-file-name "mimeinfo.cache" xdg-tests-data-dir))
         (fs (list apps cache)))
    (should (equal (xdg-mime-collect-associations "x-test/foo" fs)
                   '("a.desktop" "b.desktop")))
    (should (equal (xdg-mime-collect-associations "x-test/bar" fs)
                   '("a.desktop" "c.desktop")))
    (should (equal (xdg-mime-collect-associations "x-test/baz" fs)
                   '("a.desktop" "b.desktop" "d.desktop")))))

;;; xdg-tests.el ends here
