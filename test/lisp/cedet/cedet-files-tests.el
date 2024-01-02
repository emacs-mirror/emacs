;;; cedet-files-tests.el --- Tests for cedet-files.el  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2024 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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

;; Moved here from test/manual/cedet/cedet-utests.el

;;; Code:

(require 'ert)
(require 'cedet-files)

(defvar cedet-files-utest-list
  '(("/home/me/src/myproj/src/foo.c" . "!home!me!src!myproj!src!foo.c")
    ("c:/work/myproj/foo.el" . "!drive_c!work!myproj!foo.el")
    ("//windows/proj/foo.java" . "!!windows!proj!foo.java")
    ("/home/me/proj!bang/foo.c" . "!home!me!proj!!bang!foo.c"))
  "List of file names to test.
Each entry is a cons cell of (FNAME . CONVERTED)
where FNAME is some file name, and CONVERTED is what it should be
converted into.")

(ert-deftest cedet-files-utest ()
  "Test some file name conversions."
  (dolist (FT cedet-files-utest-list)
    (let ((dir->file (cedet-directory-name-to-file-name (car FT) t))
          (file->dir (cedet-file-name-to-directory-name (cdr FT) t)))
      (should (string= (cdr FT) dir->file))
      (should (string= file->dir (car FT))))))

(provide 'cedet-files-tests)

;;; cedet-files-tests.el ends here
