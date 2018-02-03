;;; unit tests for src/fileio.c      -*- lexical-binding: t; -*-

;; Copyright 2017-2018 Free Software Foundation, Inc.

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

(require 'ert)

(defun try-link (target link)
  (make-symbolic-link target link)
  (let* ((read-link (file-symlink-p link))
         (failure (unless (string-equal target read-link)
                    (list 'string-equal target read-link))))
    (delete-file link)
    failure))

(defun fileio-tests--symlink-failure ()
  (let* ((dir (make-temp-file "fileio" t))
         (link (expand-file-name "link" dir)))
    (unwind-protect
        (let (failure
              (char 0))
          (while (and (not failure) (< char 127))
            (setq char (1+ char))
            (when (and (eq system-type 'cygwin) (eq char 92))
              (setq char (1+ char)))
            (setq failure (try-link (string char) link)))
          (or failure
              (try-link "/:" link)))
      (delete-directory dir t))))

(ert-deftest fileio-tests--odd-symlink-chars ()
  "Check that any non-NULL ASCII character can appear in a symlink.
Also check that an encoding error can appear in a symlink."
  ;; Some Windows versions don't support symlinks, and those which do
  ;; will pop up UAC elevation prompts, so we disable this test on
  ;; MS-Windows.
  (skip-unless (not (eq system-type 'windows-nt)))
  (should (equal nil (fileio-tests--symlink-failure))))

(ert-deftest fileio-tests--directory-file-name ()
  (should (equal (directory-file-name "/") "/"))
  (should (equal (directory-file-name "//") "//"))
  (should (equal (directory-file-name "///") "/"))
  (should (equal (directory-file-name "////") "/"))
  (should (equal (directory-file-name "/abc") "/abc"))
  (should (equal (directory-file-name "/abc/") "/abc"))
  (should (equal (directory-file-name "/abc//") "/abc")))

(ert-deftest fileio-tests--directory-file-name-dos-nt ()
  "Like fileio-tests--directory-file-name, but for DOS_NT systems."
  (skip-unless (memq system-type '(ms-dos windows-nt)))
  (should (equal (directory-file-name "d:/") "d:/"))
  (should (equal (directory-file-name "d://") "d:/"))
  (should (equal (directory-file-name "d:///") "d:/"))
  (should (equal (directory-file-name "d:////") "d:/"))
  (should (equal (directory-file-name "d:/abc") "d:/abc"))
  (should (equal (directory-file-name "d:/abc/") "d:/abc"))
  (should (equal (directory-file-name "d:/abc//") "d:/abc")))

(ert-deftest fileio-tests--file-name-as-directory ()
  (should (equal (file-name-as-directory "") "./"))
  (should (equal (file-name-as-directory "/") "/"))
  (should (equal (file-name-as-directory "//") "//"))
  (should (equal (file-name-as-directory "///") "///"))
  (should (equal (file-name-as-directory "////") "////"))
  (should (equal (file-name-as-directory "/abc") "/abc/"))
  (should (equal (file-name-as-directory "/abc/") "/abc/"))
  (should (equal (file-name-as-directory "/abc//") "/abc//")))

(ert-deftest fileio-tests--file-name-as-directory-dos-nt ()
  "Like fileio-tests--file-name-as-directory, but for DOS_NT systems."
  (skip-unless (memq system-type '(ms-dos windows-nt)))
  (should (equal (file-name-as-directory "d:/") "d:/"))
  (should (equal (file-name-as-directory "d:\\") "d:/"))
  (should (equal (file-name-as-directory "d://") "d://"))
  (should (equal (file-name-as-directory "d:///") "d:///"))
  (should (equal (file-name-as-directory "d:////") "d:////"))
  (should (equal (file-name-as-directory "d:\\\\\\\\") "d:////"))
  (should (equal (file-name-as-directory "d:/abc") "d:/abc/"))
  (should (equal (file-name-as-directory "D:\\abc") "d:/abc/"))
  (should (equal (file-name-as-directory "d:/abc/") "d:/abc/"))
  (should (equal (file-name-as-directory "D:\\abc/") "d:/abc/"))
  (should (equal (file-name-as-directory "D:/abc//") "d:/abc//")))
