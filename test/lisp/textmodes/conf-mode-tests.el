;;; conf-mode-tests.el --- Test suite for conf mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2020 Free Software Foundation, Inc.

;; Author: J. Alexander Branham <alex.branham@gmail.com>
;; Keywords: internal

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'conf-mode)
(require 'ert)

(ert-deftest conf-test-align-assignments ()
  "Test for `conf-align-assignments'."
  (with-temp-buffer
    (insert "foo: bar\nbar: baz")
    (conf-colon-mode)
    (conf-align-assignments)
    (should (equal (buffer-string)
                   "foo:			bar\nbar:			baz"))))

(ert-deftest conf-test-font-lock ()
  (with-temp-buffer
    (insert "foo: bar\nbar: baz")
    (conf-colon-mode)
    (font-lock-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (should (equal (face-at-point) 'font-lock-variable-name-face))
    (search-forward "bar")
    (should-not (face-at-point))))

(ert-deftest conf-test-windows-mode ()
  (with-temp-buffer
    ;; from `conf-windows-mode' docstring:
    (insert "[ExtShellFolderViews]
Default={5984FFE0-28D4-11CF-AE66-08002B2E1262}
{5984FFE0-28D4-11CF-AE66-08002B2E1262}={5984FFE0-28D4-11CF-AE66-08002B2E1262}

[{5984FFE0-28D4-11CF-AE66-08002B2E1262}]
PersistMoniker=file://Folder.htt")
    (goto-char (point-min))
    (conf-windows-mode)
    (font-lock-mode)
    (font-lock-ensure)
    (search-forward "ExtShell")
    (should (equal (face-at-point) 'font-lock-type-face))
    (search-forward "Defau")
    (should (equal (face-at-point) 'font-lock-variable-name-face))
    (forward-line)
    (beginning-of-line)
    (should (equal (face-at-point) 'font-lock-variable-name-face))
    (forward-line 2)
    (should-not (face-at-point))
    (forward-char)
    (should (equal (face-at-point) 'font-lock-type-face))))

(ert-deftest conf-test-javaprop-mode ()
  (with-temp-buffer
    ;; From `conf-javaprop-mode' docstring
    (insert "// another kind of comment
/* yet another */

name:value
name=value
name value
x.1 =
x.2.y.1.z.1 =
x.2.y.1.z.2.zz =")
    (goto-char (point-min))
    (conf-javaprop-mode)
    (font-lock-mode)
    (font-lock-ensure)
    (should (equal (face-at-point) 'font-lock-comment-delimiter-face))
    (forward-char 3)
    (should (equal (face-at-point) 'font-lock-comment-face))
    (search-forward "*")
    (should (equal (face-at-point) 'font-lock-comment-delimiter-face))
    (while (search-forward "nam" nil t)
      (should (equal (face-at-point) 'font-lock-variable-name-face))
      (search-forward "val")
      (should-not (face-at-point)))
    (while (re-search-forward "a-z" nil t)
      (backward-char)
      (should (equal (face-at-point) 'font-lock-variable-name-face))
      (re-search-forward "[0-0]" nil t)
      (backward-char)
      (should (equal (face-at-point) 'font-lock-constant-face)))))

(ert-deftest conf-test-space-mode ()
  ;; From `conf-space-mode' docstring.
  (with-temp-buffer
    (insert "image/jpeg			jpeg jpg jpe
image/png			png
image/tiff			tiff tif
")
    (goto-char (point-min))
    (conf-space-mode)
    (font-lock-mode)
    (font-lock-ensure)
    (should (equal (face-at-point) 'font-lock-variable-name-face))
    (forward-char 15)
    (should-not (face-at-point))))

(ert-deftest conf-test-colon-mode ()
  ;; From `conf-colon-mode' docstring.
  (with-temp-buffer
    (insert "<Multi_key> <exclam> <exclam>		: \"\\241\"	exclamdown
<Multi_key> <c> <slash>			: \"\\242\"	cent")
    (goto-char (point-min))
    (conf-colon-mode)
    (font-lock-mode)
    (font-lock-ensure)
    (should (equal (face-at-point) 'font-lock-variable-name-face))
    (search-forward "24")
    (should (equal (face-at-point) 'font-lock-string-face))
    (forward-line)
    (should (equal (face-at-point) 'font-lock-variable-name-face))))

(ert-deftest conf-test-ppd-mode ()
  ;; From `conf-ppd-mode' docstring.
  (with-temp-buffer
    (insert "*DefaultTransfer: Null
*Transfer Null.Inverse: \"{ 1 exch sub }\"")
    (goto-char (point-min))
    (conf-ppd-mode)
    (font-lock-mode)
    (font-lock-ensure)
    (should (equal (face-at-point) 'font-lock-variable-name-face))
    (search-forward "Nul")
    (should-not (face-at-point))))

(ert-deftest conf-test-xdefaults-mode ()
  ;; From `conf-xdefaults-mode' docstring.
  (with-temp-buffer
    (insert "*background:			gray99
*foreground:			black")
    (goto-char (point-min))
    (conf-xdefaults-mode)
    (font-lock-mode)
    (font-lock-ensure)
    (should (equal (face-at-point) 'font-lock-variable-name-face))
    (search-forward "gray")
    (should-not (face-at-point))))

(ert-deftest conf-test-toml-mode ()
  ;; From `conf-toml-mode' docstring.
  (with-temp-buffer
    (insert "\[entry]
value = \"some string\"")
    (goto-char (point-min))
    (conf-toml-mode)
    (font-lock-mode)
    (font-lock-ensure)
    (should-not (face-at-point))
    (forward-char)
    (should (equal (face-at-point) 'font-lock-type-face))
    (forward-line)
    (should (equal (face-at-point) 'font-lock-variable-name-face))
    (search-forward "som")
    (should (equal (face-at-point) 'font-lock-string-face))))

(ert-deftest conf-test-desktop-mode ()
  ;; From `conf-desktop-mode' dostring.
  (with-temp-buffer
    (insert "	[Desktop Entry]
	Name=GNU Image Manipulation Program
	Name[oc]=Editor d'imatge GIMP
	Exec=gimp-2.8 %U
	Terminal=false")
    (goto-char (point-min))
    (conf-desktop-mode)
    (font-lock-mode)
    (font-lock-ensure)
    (search-forward "Desk")
    (should (equal (face-at-point) 'font-lock-type-face))
    (search-forward "Nam")
    (should (equal (face-at-point) 'font-lock-variable-name-face))
    (forward-char 2)
    (should-not (face-at-point))
    (search-forward "[")
    (should (equal (face-at-point) 'font-lock-constant-face))))



(provide 'conf-mode-tests)

;;; conf-mode-tests.el ends here
