;;; fileio-tests.el --- unit tests for src/fileio.c      -*- lexical-binding: t; -*-

;; Copyright 2017-2026 Free Software Foundation, Inc.

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

(defun try-link (target link)
  (make-symbolic-link target link)
  (let* ((read-link (file-symlink-p link))
         (failure (unless (string-equal target read-link)
                    (list 'string-equal target read-link))))
    (delete-file link)
    failure))

(defun fileio-tests--symlink-failure ()
  (ert-with-temp-directory dir
    (let* ((link (expand-file-name "link" dir)))
      (let (failure
            (char 0))
        (while (and (not failure) (< char 127))
          (setq char (1+ char))
          (when (and (eq system-type 'cygwin) (eq char 92))
            (setq char (1+ char)))
          (setq failure (try-link (string char) link)))
        (or failure
            (try-link "/:" link))))))

(ert-deftest fileio-tests--odd-symlink-chars ()
  "Check that any non-NULL ASCII character can appear in a symlink.
Also check that an encoding error can appear in a symlink."
  ;; Some Windows versions don't support symlinks, and those which do
  ;; will pop up UAC elevation prompts, so we disable this test on
  ;; MS-Windows.
  (skip-when (eq system-type 'windows-nt))
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
  "Test that `expand-file-name' works even when HOME is relative."
  (with-environment-variables (("HOME" "a/b/c"))
    (should (equal (expand-file-name "~/foo")
                   (expand-file-name "a/b/c/foo")))
    (when (memq system-type '(ms-dos windows-nt))
      ;; Test expansion of drive-relative file names.
      (setenv "HOME" "x:foo")
      (should (equal (expand-file-name "~/bar") "x:/foo/bar")))))

(ert-deftest fileio-tests--insert-file-interrupt ()
  (ert-with-temp-file f
    (let ((text "-*- coding: binary -*-\n\xc3\xc3help"))
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
            (should (equal c1 (char-after)))))))))

(ert-deftest fileio-tests--relative-default-directory ()
  "Test `expand-file-name' when `default-directory' is relative."
  (let ((default-directory "some/relative/name"))
    (should (file-name-absolute-p (expand-file-name "foo"))))
  (let* ((default-directory "~foo")
         (name (expand-file-name "bar")))
    (should (and (file-name-absolute-p name)
                 (not (eq (aref name 0) ?~))))))

(ert-deftest fileio-tests--expand-file-name-null-bytes ()
  "Test that `expand-file-name' checks for null bytes in filenames."
  (should-error (expand-file-name (concat "file" (char-to-string ?\0) ".txt"))
                :type 'wrong-type-argument)
  (should-error (expand-file-name "file.txt" (concat "dir" (char-to-string ?\0)))
                :type 'wrong-type-argument)
  (let ((default-directory (concat "dir" (char-to-string ?\0))))
    (should-error (expand-file-name "file.txt") :type 'wrong-type-argument)))

(ert-deftest fileio-tests--file-name-absolute-p ()
  "Test `file-name-absolute-p'."
  (dolist (suffix '("" "/" "//" "/foo" "/foo/" "/foo//" "/foo/bar"))
    (unless (string-equal suffix "")
      (should (file-name-absolute-p suffix)))
    (should (file-name-absolute-p (concat "~" suffix)))
    (when (user-full-name user-login-name)
      (should (file-name-absolute-p (concat "~" user-login-name suffix))))
    (unless (user-full-name "nosuchuser")
      (should (not (file-name-absolute-p (concat "~nosuchuser" suffix)))))))

(ert-deftest fileio-tests--circular-after-insert-file-functions ()
  "Test `after-insert-file-functions' as a circular list."
  (ert-with-temp-file f
    :suffix "fileio"
    (let ((after-insert-file-functions (list 'identity)))
      (setcdr after-insert-file-functions after-insert-file-functions)
      (write-region "hello\n" nil f nil 'silent)
      (should-error (insert-file-contents f) :type 'circular-list))))

(ert-deftest fileio-tests/null-character ()
  (should-error (file-exists-p "/foo\0bar")
                :type 'wrong-type-argument))

(ert-deftest fileio-tests/file-name-concat ()
  (should (equal (file-name-concat "foo" "bar") "foo/bar"))
  (should (equal (file-name-concat "foo" "bar") "foo/bar"))
  (should (equal (file-name-concat "foo" "bar" "zot") "foo/bar/zot"))
  (should (equal (file-name-concat "foo/" "bar") "foo/bar"))
  (should (equal (file-name-concat "foo//" "bar") "foo//bar"))
  (should (equal (file-name-concat "foo/" "bar/" "zot") "foo/bar/zot"))
  (should (equal (file-name-concat "fóo" "bar") "fóo/bar"))
  (should (equal (file-name-concat "foo" "bár") "foo/bár"))
  (should (equal (file-name-concat "fóo" "bár") "fóo/bár"))
  (let ((string (make-string 5 ?a)))
    (should (not (multibyte-string-p string)))
    (aset string 2 255)
    (should (not (multibyte-string-p string)))
    (should (equal (file-name-concat "fóo" string) "fóo/aa\377aa")))
  (should (equal (file-name-concat "foo") "foo"))
  (should (equal (file-name-concat "foo/") "foo/"))
  (should (equal (file-name-concat "foo" "") "foo"))
  (should (equal (file-name-concat "foo" "" "" "" nil) "foo"))
  (should (equal (file-name-concat "" "bar") "bar"))
  (should (equal (file-name-concat "" "") "")))

(ert-deftest fileio-tests--non-regular-insert ()
  (skip-unless (file-exists-p "/dev/urandom"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (should-error (insert-file-contents "/dev/urandom" nil 5 10))
    (insert-file-contents "/dev/urandom" nil nil 10)
    (should (= (buffer-size) 10))))

(ert-deftest fileio-tests--read-directory ()
  "Make sure insertring a directory fails with a platform-independent error."
  (ert-with-temp-directory dir
    (let* ((dir-name (directory-file-name dir))
           (err (should-error (insert-file-contents dir-name)))
           (desc-string
            ;; On MS-Windows we fail trying to 'open' a directory.
            (if (eq system-type 'windows-nt)
                "Opening input file"
              "Read error")))
      (should (equal err
                     (list 'file-error
                           desc-string
                           "Is a directory"
                           dir-name))))))

(defun fileio-tests--identity-expand-handler (_ file &rest _)
  file)
(put 'fileio-tests--identity-expand-handler 'operations '(expand-file-name))

(ert-deftest fileio--file-name-case-insensitive-p ()
  ;; Check that we at least don't crash if given nonexisting files
  ;; without a directory (bug#56443).

  ;; Use an identity file-name handler, as if called by `ffap'.
  (let* ((file-name-handler-alist
          '(("^mailto:" . fileio-tests--identity-expand-handler)))
         (file "mailto:snowball@hell.com"))
    ;; Check that `expand-file-name' is identity for this name.
    (should (equal (expand-file-name file nil) file))
    (file-name-case-insensitive-p file)))

(ert-deftest fileio-tests-invalid-UNC ()
  (skip-unless (eq system-type 'windows-nt))
  ;; These should not crash, see bug#70914.
  (should-not (file-exists-p "//"))
  (should (file-attributes "//")))

(ert-deftest fileio-tests-w32-time-stamp-granularity ()
  "Test 100-nsec granularity of file time stamps on MS-Windows."
  (skip-unless (eq system-type 'windows-nt))
  ;; FIXME: This only works on NTFS volumes, so should skip the test if
  ;; not NTFS.  But we don't expose the filesystem type to Lisp.
  (let ((tfile (make-temp-file "tstamp")))
    (unwind-protect
        (progn
          (set-file-times tfile (encode-time '(59.123456789 15 23 01 02 2025)))
          (should
           (equal (format-time-string "%Y/%m/%d %H:%M:%S.%N"
		                      (file-attribute-modification-time
                                       (file-attributes tfile)))
                  ;; Last 2 digits of seconds must be zero due to
                  ;; 100-nsec resolution of Windows file time stamps.
                  "2025/02/01 23:15:59.123456700")))
      (delete-file tfile))))

(defconst ert--tests-dir
  (file-name-directory (macroexp-file-name)))

(ert-deftest fileio-tests--insert-file-contents-supersession ()
  (ert-with-temp-file file
    (write-region "foo" nil file)
    (let* ((asked nil)
           (buf (find-file-noselect file))
           (auast (lambda (&rest _) (setq asked t))))
      (unwind-protect
          (with-current-buffer buf
            ;; Pretend someone else edited the file.
            (write-region "bar" nil file 'append)
            ;; Use `advice-add' rather than `cl-letf' because the function
            ;; may not be defined yet.
            (advice-add 'ask-user-about-supersession-threat :override auast)
            ;; Modify the local buffer via `insert-file-contents'.
            (insert-file-contents
             (expand-file-name "lread-resources/somelib.el"
                               ert--tests-dir)
             nil nil nil 'replace))
        (advice-remove 'ask-user-about-supersession-threat auast)
        (kill-buffer buf))
      ;; We should have prompted about the supersession threat.
      (should asked))))


;;; fileio-tests.el ends here
