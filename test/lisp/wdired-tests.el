;;; wdired-tests.el --- tests for wdired.el          -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2026 Free Software Foundation, Inc.

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
(require 'dired)
(require 'dired-x)
(require 'wdired)

(defvar dired-query)                    ; Pacify byte compiler.

(ert-deftest wdired-test-bug32173-01 ()
  "Test using non-nil wdired-use-interactive-rename.
Partially modifying a file name should succeed."
  (ert-with-temp-directory test-dir
    ;; The call to file-truename is for MS-Windows, where numeric
    ;; tails or some other feature (like SUBST) could cause file names
    ;; to fail to compare 'equal'.
    (setq test-dir (file-truename test-dir))
    (let* ((test-file (concat (file-name-as-directory test-dir) "foo.c"))
           (replace "bar")
           (new-file (string-replace "foo" replace test-file))
           (wdired-use-interactive-rename t))
      (write-region "" nil test-file nil 'silent)
      (advice-add 'dired-query ; Don't ask confirmation to overwrite a file.
                  :override
                  (lambda (_sym _prompt &rest _args) (setq dired-query t))
                  '((name . "advice-dired-query")))
      (let ((buf (find-file-noselect test-dir)))
        (unwind-protect
            (with-current-buffer buf
              (should (equal (dired-file-name-at-point) test-file))
              (dired-toggle-read-only)
              (kill-region (point) (progn (search-forward ".")
                                          (forward-char -1) (point)))
              (insert replace)
              (wdired-finish-edit)
              (should (equal (dired-file-name-at-point) new-file)))
          (if buf (kill-buffer buf)))))))

(ert-deftest wdired-test-bug32173-02 ()
  "Test using non-nil wdired-use-interactive-rename.
Aborting an edit should leaving original file name unchanged."
  (ert-with-temp-directory test-dir
    (setq test-dir (file-truename test-dir))
    (let* ((test-file (concat (file-name-as-directory test-dir) "foo.c"))
           (wdired-use-interactive-rename t))
      (write-region "" nil test-file nil 'silent)
      ;; Make dired-do-create-files-regexp a noop to mimic typing C-g
      ;; at its prompt before wdired-finish-edit returns.
      (advice-add 'dired-do-create-files-regexp
                  :override
                  (lambda (&rest _) (ignore))
                  '((name . "advice-dired-do-create-files-regexp")))
      (let ((buf (find-file-noselect test-dir)))
        (unwind-protect
            (with-current-buffer buf
              (should (equal (dired-file-name-at-point) test-file))
              (dired-toggle-read-only)
              (kill-region (point) (progn (search-forward ".")
                                          (forward-char -1) (point)))
              (insert "bar")
              (wdired-finish-edit)
              (should (equal (dired-get-filename) test-file)))
          (if buf (kill-buffer buf)))))))

(ert-deftest wdired-test-symlink-name ()
  "Test the file name of a symbolic link.
The Dired and WDired functions returning the name should include
only the name before the link arrow."
  (ert-with-temp-directory test-dir
    (let* ((link-name "foo"))
      (let ((buf (find-file-noselect test-dir)))
        (unwind-protect
            (with-current-buffer buf
              (skip-unless
               ;; This check is for wdired, not symbolic links, so skip
               ;; it when make-symbolic-link fails for any reason (like
               ;; insufficient privileges).
               (ignore-errors (make-symbolic-link "./bar/baz" link-name) t))
              (revert-buffer)
              (let* ((file-name (dired-get-filename))
                     (dir-part (file-name-directory file-name))
                     (lf-name (concat dir-part link-name)))
                (should (equal file-name lf-name))
                (dired-toggle-read-only)
                (should (equal (wdired-get-filename) lf-name))
                (dired-toggle-read-only)))
          (if buf (kill-buffer buf)))))))

(ert-deftest wdired-test-unfinished-edit-01 ()
  "Test editing a file name without saving the change.
Finding the new name should be possible while still in
wdired-mode."
  (ert-with-temp-directory test-dir
    (setq test-dir (file-truename test-dir))
    (let* ((test-file (concat (file-name-as-directory test-dir) "foo.c"))
           (replace "bar")
           (new-file (string-replace "/foo" (concat "/" replace) test-file)))
      (write-region "" nil test-file nil 'silent)
      (let ((buf (find-file-noselect test-dir)))
        (unwind-protect
            (with-current-buffer buf
              (should (equal (dired-file-name-at-point) test-file))
              (dired-toggle-read-only)
              (kill-region (point) (progn (search-forward ".")
                                          (forward-char -1) (point)))
              (insert replace)
              (should (equal (dired-get-filename) new-file)))
          (when buf
            (with-current-buffer buf
              ;; Prevent kill-buffer-query-functions from chiming in.
              (set-buffer-modified-p nil)
              (kill-buffer buf))))))))

(ert-deftest wdired-test-bug34915 ()
  "Test editing when dired-listing-switches includes -F.
Appended file indicators should not count as part of the file
name, either before or after editing.  Since
dired-move-to-end-of-filename handles indicator characters, it
suffices to compare the return values of dired-get-filename and
wdired-get-filename before and after editing."
  ;; FIXME: Add a test for a door (indicator ">") only under Solaris?
  (ert-with-temp-directory test-dir
    (let* ((dired-listing-switches "-Fl")
           (dired-ls-F-marks-symlinks
            (or (and (memq system-type '(berkeley-unix darwin))
		     (not (string= insert-directory-program "gls")))
                (featurep 'ls-lisp)))
           (buf (find-file-noselect test-dir))
           proc)
      (unwind-protect
          (progn
            (with-current-buffer buf
              ;; Create a .bat file so that MS-Windows, where the 'x'
              ;; bit is not recorded in the filesystem, considers it an
              ;; executable.
              (dired-create-empty-file "foo.bat")
              (set-file-modes "foo.bat" (file-modes-symbolic-to-number "+x"))
              (skip-unless
               ;; This check is for wdired, not symbolic links, so skip
               ;; it when make-symbolic-link fails for any reason (like
               ;; insufficient privileges).
               (ignore-errors (make-symbolic-link "foo.bat" "bar") t))
              (make-directory "foodir")
              (unless (memq system-type '(windows-nt ms-dos))
                (dired-smart-shell-command "mkfifo foopipe"))
              (when (featurep 'make-network-process '(:family local))
                (setq proc (make-network-process
                            :name "foo"
                            :family 'local
                            :server t
                            :service (expand-file-name "foosocket" test-dir))))
              (kill-buffer buf))
            (dired test-dir)
            (dired-toggle-read-only)
            (let (names)
              ;; Test that the file names are the same in Dired and WDired.
              (while (not (eobp))
                (should (equal (dired-get-filename 'no-dir t)
                               (wdired-get-filename t)))
                (insert "w")
                (push (wdired-get-filename t) names)
                (dired-next-line 1))
              (wdired-finish-edit)
              ;; Test that editing the file names ignores the indicator
              ;; character.
              (let (dir)
                (while (and (dired-previous-line 1)
                            (setq dir (dired-get-filename 'no-dir t)))
                  (should (equal dir (pop names)))))))
        (kill-buffer (get-buffer test-dir))
        (ignore-errors (delete-process proc))))))

(ert-deftest wdired-test-bug39280 ()
  "Test for https://debbugs.gnu.org/39280."
  (ert-with-temp-directory test-dir
    (setq test-dir (file-truename test-dir))
    (let* ((fname "foo")
           (full-fname (expand-file-name fname test-dir)))
      (make-empty-file full-fname)
      (let ((buf (find-file-noselect test-dir)))
        (unwind-protect
            (with-current-buffer buf
              (dired-toggle-read-only)
              (dolist (old '(t nil))
                (should (equal fname (wdired-get-filename 'nodir old)))
                (should (equal full-fname (wdired-get-filename nil old))))
              (wdired-finish-edit))
          (if buf (kill-buffer buf)))))))

(ert-deftest wdired-test-bug61510 ()
  "Test visibility of symlink target on leaving wdired-mode.
When dired-hide-details-mode is enabled and
dired-hide-details-hide-symlink-targets is non-nil (the default),
the link target becomes invisible.  When wdired-mode is enabled
the target becomes visible, but on returning to dired-mode, it
should be invisible again."
  (ert-with-temp-directory test-dir
    (let ((buf (find-file-noselect test-dir))
          ;; Default value is t, but set it anyway, to be sure.
          (dired-hide-details-hide-symlink-targets t))
      (unwind-protect
          (with-current-buffer buf
            (skip-unless
             ;; This check is for wdired, not symbolic links, so skip
             ;; it when make-symbolic-link fails for any reason (like
             ;; insufficient privileges).
             (ignore-errors (make-symbolic-link "bar" "foo") t))
            (dired-hide-details-mode)
            (should (memq 'dired-hide-details-link buffer-invisibility-spec))
            (dired-toggle-read-only)
            (should-not (memq 'dired-hide-details-link
                              buffer-invisibility-spec))
            (wdired-finish-edit)
            (should (memq 'dired-hide-details-link buffer-invisibility-spec)))
        (if buf (kill-buffer buf))))))

(provide 'wdired-tests)
;;; wdired-tests.el ends here
