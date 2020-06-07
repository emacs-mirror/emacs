;;; wdired-tests.el --- tests for wdired.el          -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'dired)
(require 'wdired)

(defvar dired-query)                    ; Pacify byte compiler.

(ert-deftest wdired-test-bug32173-01 ()
  "Test using non-nil wdired-use-interactive-rename.
Partially modifying a file name should succeed."
  (let* ((test-dir (make-temp-file "test-dir-" t))
	 (test-file (concat (file-name-as-directory test-dir) "foo.c"))
	 (replace "bar")
	 (new-file (replace-regexp-in-string "foo" replace test-file))
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
	(if buf (kill-buffer buf))
	(delete-directory test-dir t)))))

(ert-deftest wdired-test-bug32173-02 ()
  "Test using non-nil wdired-use-interactive-rename.
Aborting an edit should leaving original file name unchanged."
  (let* ((test-dir (make-temp-file "test-dir-" t))
	 (test-file (concat (file-name-as-directory test-dir) "foo.c"))
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
	(if buf (kill-buffer buf))
	(delete-directory test-dir t)))))

(ert-deftest wdired-test-symlink-name ()
  "Test the file name of a symbolic link.
The Dired and WDired functions returning the name should include
only the name before the link arrow."
  (let* ((test-dir (make-temp-file "test-dir-" t))
         (link-name "foo"))
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
	(if buf (kill-buffer buf))
	(delete-directory test-dir t)))))

(ert-deftest wdired-test-unfinished-edit-01 ()
  "Test editing a file name without saving the change.
Finding the new name should be possible while still in
wdired-mode."
  :expected-result (if (< emacs-major-version 27) :failed :passed)
  (let* ((test-dir (make-temp-file "test-dir-" t))
	 (test-file (concat (file-name-as-directory test-dir) "foo.c"))
	 (replace "bar")
	 (new-file (replace-regexp-in-string "foo" replace test-file)))
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
	    (kill-buffer buf)))
	(delete-directory test-dir t)))))

(defvar server-socket-dir)
(declare-function dired-smart-shell-command "dired-x"
                  (command &optional output-buffer error-buffer))

(ert-deftest wdired-test-bug34915 ()
  "Test editing when dired-listing-switches includes -F.
Appended file indicators should not count as part of the file
name, either before or after editing.  Since
dired-move-to-end-of-filename handles indicator characters, it
suffices to compare the return values of dired-get-filename and
wdired-get-filename before and after editing."
  ;; FIXME: Add a test for a door (indicator ">") only under Solaris?
  (let* ((test-dir (make-temp-file "test-dir-" t))
         (server-socket-dir test-dir)
         (dired-listing-switches "-Fl")
         (buf (find-file-noselect test-dir)))
    (unwind-protect
        (progn
	  (with-current-buffer buf
            (dired-create-empty-file "foo")
            (set-file-modes "foo" (file-modes-symbolic-to-number "+x"))
            (make-symbolic-link "foo" "bar")
            (make-directory "foodir")
            (require 'dired-x)
            (dired-smart-shell-command "mkfifo foopipe")
            (server-force-delete)
            ;; FIXME?  This seems a heavy-handed way of making a socket.
            (server-start)              ; Add a socket file.
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
      (server-force-delete)
      (delete-directory test-dir t))))


(provide 'wdired-tests)
;;; wdired-tests.el ends here
