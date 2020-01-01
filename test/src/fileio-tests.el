;;; unit tests for src/fileio.c      -*- lexical-binding: t; -*-

;; Copyright 2017-2020 Free Software Foundation, Inc.

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

(ert-deftest fileio-tests--relative-HOME ()
  "Test that expand-file-name works even when HOME is relative."
  (let ((old-home (getenv "HOME")))
    (setenv "HOME" "a/b/c")
    (should (equal (expand-file-name "~/foo")
                   (expand-file-name "a/b/c/foo")))
    (when (memq system-type '(ms-dos windows-nt))
      ;; Test expansion of drive-relative file names.
      (setenv "HOME" "x:foo")
      (should (equal (expand-file-name "~/bar") "x:/foo/bar")))
    (setenv "HOME" old-home)))

(ert-deftest fileio-tests--insert-file-interrupt ()
  (let ((text "-*- coding: binary -*-\n\xc3\xc3help")
        f)
    (unwind-protect
        (progn
          (setq f (make-temp-file "ftifi"))
          (write-region text nil f nil 'silent)
          (with-temp-buffer
            (catch 'toto
              (let ((set-auto-coding-function (lambda (&rest _) (throw 'toto nil))))
                (insert-file-contents f)))
            (goto-char (point-min))
            (unless (eobp)
              (forward-line 1)
              (let ((c1 (char-after)))
                (forward-char 1)
                (should (equal c1 (char-before)))
                (should (equal c1 (char-after)))))))
      (if f (delete-file f)))))

(ert-deftest fileio-tests--relative-default-directory ()
  "Test expand-file-name when default-directory is relative."
  (let ((default-directory "some/relative/name"))
    (should (file-name-absolute-p (expand-file-name "foo"))))
  (let* ((default-directory "~foo")
         (name (expand-file-name "bar")))
    (should (and (file-name-absolute-p name)
                 (not (eq (aref name 0) ?~))))))

(ert-deftest fileio-tests--file-name-absolute-p ()
  "Test file-name-absolute-p."
  (dolist (suffix '("" "/" "//" "/foo" "/foo/" "/foo//" "/foo/bar"))
    (unless (string-equal suffix "")
      (should (file-name-absolute-p suffix)))
    (should (file-name-absolute-p (concat "~" suffix)))
    (when (user-full-name user-login-name)
      (should (file-name-absolute-p (concat "~" user-login-name suffix))))
    (unless (user-full-name "nosuchuser")
      (should (not (file-name-absolute-p (concat "~nosuchuser" suffix)))))))

(ert-deftest fileio-tests--circular-after-insert-file-functions ()
  "Test after-insert-file-functions as a circular list."
  (let ((f (make-temp-file "fileio"))
        (after-insert-file-functions (list 'identity)))
    (setcdr after-insert-file-functions after-insert-file-functions)
    (write-region "hello\n" nil f nil 'silent)
    (should-error (insert-file-contents f) :type 'circular-list)
    (delete-file f)))
