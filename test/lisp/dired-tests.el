;;; dired-tests.el --- Test suite. -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 Free Software Foundation, Inc.

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
(require 'dired)
(require 'nadvice)

(ert-deftest dired-autoload ()
  "Tests to see whether dired-x has been autoloaded"
  (should
   (fboundp 'dired-jump))
  (should
   (autoloadp
    (symbol-function
     'dired-jump))))

(ert-deftest dired-test-bug22694 ()
  "Test for https://debbugs.gnu.org/22694 ."
  (let* ((dir       (expand-file-name "bug22694" default-directory))
         (file      "test")
         (full-name (expand-file-name file dir))
         (regexp    "bar")
         (dired-always-read-filesystem t) buffers)
    (if (file-exists-p dir)
        (delete-directory dir 'recursive))
    (make-directory dir)
    (with-temp-file full-name (insert "foo"))
    (push (find-file-noselect full-name) buffers)
    (push (dired dir) buffers)
    (with-temp-file full-name (insert "bar"))
    (dired-mark-files-containing-regexp regexp)
    (unwind-protect
        (should (equal (dired-get-marked-files nil nil nil 'distinguish-1-mark)
                       `(t ,full-name)))
      ;; Clean up
      (dolist (buf buffers)
        (when (buffer-live-p buf) (kill-buffer buf)))
      (delete-directory dir 'recursive))))

(defvar dired-dwim-target)
(ert-deftest dired-test-bug25609 ()
  "Test for https://debbugs.gnu.org/25609 ."
  (let* ((from (make-temp-file "foo" 'dir))
         ;; Make sure we have long file-names in 'from' and 'to', not
         ;; their 8+3 short aliases, because the latter will confuse
         ;; Dired commands invoked below.
         (from (if (memq system-type '(ms-dos windows-nt))
                   (file-truename from)
                 from))
         (to (make-temp-file "bar" 'dir))
         (to (if (memq system-type '(ms-dos windows-nt))
                 (file-truename to)
                 to))
         (target (expand-file-name (file-name-nondirectory from) to))
         (nested (expand-file-name (file-name-nondirectory from) target))
         (dired-dwim-target t)
         (dired-recursive-copies 'always) ; Don't prompt me.
         buffers)
    (advice-add 'dired-query ; Don't ask confirmation to overwrite a file.
                :override
                (lambda (_sym _prompt &rest _args) (setq dired-query t))
                '((name . "advice-dired-query")))
    (advice-add 'completing-read ; Don't prompt me: just return init.
                :override
                (lambda (_prompt _coll &optional _pred _match init _hist _def _inherit _keymap)
                  init)
                '((name . "advice-completing-read")))
    (delete-other-windows) ; We don't want to display any other dired buffers.
    (push (dired to) buffers)
    (push (dired-other-window temporary-file-directory) buffers)
    (unwind-protect
        (let ((ok-fn
	       (lambda ()
		 (let ((win-buffers (mapcar #'window-buffer (window-list))))
		   (and (memq (car buffers) win-buffers)
			(memq (cadr buffers) win-buffers))))))
	  (dired-goto-file from)
	  ;; Right before `dired-do-copy' call, to reproduce the bug conditions,
	  ;; ensure we have windows displaying the two dired buffers.
	  (and (funcall ok-fn) (dired-do-copy))
	  ;; Call `dired-do-copy' again: this must overwrite `target'; if the bug
	  ;; still exists, then it creates `nested' instead.
	  (when (funcall ok-fn)
	    (dired-do-copy)
            (should (file-exists-p target))
            (should-not (file-exists-p nested))))
      (dolist (buf buffers)
        (when (buffer-live-p buf) (kill-buffer buf)))
      (delete-directory from 'recursive)
      (delete-directory to 'recursive)
      (advice-remove 'dired-query "advice-dired-query")
      (advice-remove 'completing-read "advice-completing-read"))))

;; (ert-deftest dired-test-bug27243 ()
;;   "Test for https://debbugs.gnu.org/27243 ."
;;   (let ((test-dir (make-temp-file "test-dir-" t))
;;         (dired-auto-revert-buffer t) buffers)
;;     (with-current-buffer (find-file-noselect test-dir)
;;       (make-directory "test-subdir"))
;;     (push (dired test-dir) buffers)
;;     (unwind-protect
;;         (let ((buf (current-buffer))
;;               (pt1 (point))
;;               (test-file (concat (file-name-as-directory "test-subdir")
;;                                  "test-file")))
;;           (write-region "Test" nil test-file nil 'silent nil 'excl)
;;           ;; Sanity check: point should now be on the subdirectory.
;;           (should (equal (dired-file-name-at-point)
;;                          (concat (file-name-as-directory test-dir)
;;                                  (file-name-as-directory "test-subdir"))))
;;           (push (dired-find-file) buffers)
;;           (let ((pt2 (point)))          ; Point is on test-file.
;;             (switch-to-buffer buf)
;;             ;; Sanity check: point should now be back on the subdirectory.
;;             (should (eq (point) pt1))
;;             ;; Case 1: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27243#5
;;             (push (dired-find-file) buffers)
;;             (should (eq (point) pt2))
;;             ;; Case 2: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27243#28
;;             (push (dired test-dir) buffers)
;;             (should (eq (point) pt1))))
;;       (dolist (buf buffers)
;;         (when (buffer-live-p buf) (kill-buffer buf)))
;;       (delete-directory test-dir t))))

(ert-deftest dired-test-bug27243-01 ()
  "Test for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27243#5 ."
  (let* ((test-dir (file-name-as-directory (make-temp-file "test-dir-" t)))
         (save-pos (lambda ()
                     (with-current-buffer (car (dired-buffers-for-dir test-dir))
                       (dired-save-positions))))
         (dired-auto-revert-buffer t) buffers)
    ;; On MS-Windows, get rid of 8+3 short names in test-dir, if the
    ;; corresponding long file names exist, otherwise such names trip
    ;; dired-buffers-for-dir.
    (if (eq system-type 'windows-nt)
        (setq test-dir (file-truename test-dir)))
    (should-not (dired-buffers-for-dir test-dir))
    (with-current-buffer (find-file-noselect test-dir)
      (make-directory "test-subdir"))
    (message "Saved pos: %S" (funcall save-pos))
    ;; Point must be at end-of-buffer.
    (with-current-buffer (car (dired-buffers-for-dir test-dir))
      (should (eobp)))
    (push (dired test-dir) buffers)
    (message "Saved pos: %S" (funcall save-pos))
    ;; Previous dired call shouldn't create a new buffer: must visit the one
    ;; created by `find-file-noselect' above.
    (should (eq 1 (length (dired-buffers-for-dir test-dir))))
    (unwind-protect
        (let ((buf (current-buffer))
              (pt1 (point))
              (test-file (concat (file-name-as-directory "test-subdir")
                                 "test-file")))
          (message "Saved pos: %S" (funcall save-pos))
          (write-region "Test" nil test-file nil 'silent nil 'excl)
          (message "Saved pos: %S" (funcall save-pos))
          ;; Sanity check: point should now be on the subdirectory.
          (should (equal (dired-file-name-at-point)
                         (concat test-dir (file-name-as-directory "test-subdir"))))
          (message "Saved pos: %S" (funcall save-pos))
          (push (dired-find-file) buffers)
          (let ((pt2 (point)))          ; Point is on test-file.
            (pop-to-buffer-same-window buf)
            ;; Sanity check: point should now be back on the subdirectory.
            (should (eq (point) pt1))
            (push (dired-find-file) buffers)
            (should (eq (point) pt2))))
      (dolist (buf buffers)
        (when (buffer-live-p buf) (kill-buffer buf)))
      (delete-directory test-dir t))))

(ert-deftest dired-test-bug27243-02 ()
  "Test for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27243#28 ."
  (let ((test-dir (make-temp-file "test-dir-" t))
        (dired-auto-revert-buffer t) buffers)
    ;; On MS-Windows, get rid of 8+3 short names in test-dir, if the
    ;; corresponding long file names exist, otherwise such names trip
    ;; string comparisons below.
    (if (eq system-type 'windows-nt)
        (setq test-dir (file-truename test-dir)))
    (with-current-buffer (find-file-noselect test-dir)
      (make-directory "test-subdir"))
    (push (dired test-dir) buffers)
    (unwind-protect
        (let ((buf (current-buffer))
              (pt1 (point))
              (test-file (concat (file-name-as-directory "test-subdir")
                                 "test-file")))
          (write-region "Test" nil test-file nil 'silent nil 'excl)
          ;; Sanity check: point should now be on the subdirectory.
          (should (equal (dired-file-name-at-point)
                         (concat (file-name-as-directory test-dir)
                                 (file-name-as-directory "test-subdir"))))
          (push (dired-find-file) buffers)
          (let ((pt2 (point)))          ; Point is on test-file.
            (switch-to-buffer buf)
            ;; Sanity check: point should now be back on the subdirectory.
            (should (eq (point) pt1))
            (push (dired test-dir) buffers)
            (should (eq (point) pt1))))
      (dolist (buf buffers)
        (when (buffer-live-p buf) (kill-buffer buf)))
      (delete-directory test-dir t))))

(ert-deftest dired-test-bug27243-03 ()
  "Test for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27243#61 ."
  (let ((test-dir (make-temp-file "test-dir-" t))
        (dired-auto-revert-buffer t)
        test-subdir1 test-subdir2 allbufs)
    (unwind-protect
        (progn
          (with-current-buffer (find-file-noselect test-dir)
            (push (current-buffer) allbufs)
            (make-directory "test-subdir1")
            (make-directory "test-subdir2")
            (let ((test-file1 "test-file1")
                  (test-file2 "test-file2"))
              (with-current-buffer (find-file-noselect "test-subdir1")
                (push (current-buffer) allbufs)
                (write-region "Test1" nil test-file1 nil 'silent nil 'excl))
              (with-current-buffer (find-file-noselect "test-subdir2")
                (push (current-buffer) allbufs)
                (write-region "Test2" nil test-file2 nil 'silent nil 'excl))))
          ;; Call find-file with a wild card and test point in each file.
          (let ((buffers (find-file (concat (file-name-as-directory test-dir)
                                            "*")
                                    t)))
            (dolist (buf buffers)
              (let ((pt (with-current-buffer buf (point))))
                (switch-to-buffer (find-file-noselect test-dir))
                (find-file (buffer-name buf))
                (should (equal (point) pt))))
            (append buffers allbufs)))
      (dolist (buf allbufs)
        (when (buffer-live-p buf) (kill-buffer buf)))
      (delete-directory test-dir t))))

(ert-deftest dired-test-bug7131 ()
  "Test for https://debbugs.gnu.org/7131 ."
  (let* ((dir (expand-file-name "lisp" source-directory))
         (buf (dired dir)))
    (unwind-protect
        (progn
          (setq buf (dired (list dir "simple.el")))
          (dired-toggle-marks)
          (should-not (cdr (dired-get-marked-files)))
          (kill-buffer buf)
          (setq buf (dired (list dir "simple.el"))
                buf (dired dir))
          (dired-toggle-marks)
          (should (cdr (dired-get-marked-files))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest dired-test-bug27631 ()
  "Test for https://debbugs.gnu.org/27631 ."
  ;; For dired using 'ls' emulation we test for this bug in
  ;; ls-lisp-tests.el and em-ls-tests.el.
  (skip-unless (and (not (featurep 'ls-lisp))
                    (not (featurep 'eshell))))
  (let* ((dir (make-temp-file "bug27631" 'dir))
         (dir1 (expand-file-name "dir1" dir))
         (dir2 (expand-file-name "dir2" dir))
         (default-directory dir)
         buf)
    (unwind-protect
        (progn
          (make-directory dir1)
          (make-directory dir2)
          (with-temp-file (expand-file-name "a.txt" dir1))
          (with-temp-file (expand-file-name "b.txt" dir2))
          (setq buf (dired (expand-file-name "dir*/*.txt" dir)))
          (dired-toggle-marks)
          (should (cdr (dired-get-marked-files))))
      (delete-directory dir 'recursive)
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest dired-test-bug27899 ()
  "Test for https://debbugs.gnu.org/27899 ."
  (let* ((dir (expand-file-name "src" source-directory))
	 (buf (dired (list dir "cygw32.c" "alloc.c" "w32xfns.c" "xdisp.c")))
         (orig dired-hide-details-mode))
    (dired-goto-file (expand-file-name "cygw32.c"))
    (forward-line 0)
    (unwind-protect
        (progn
          (let ((inhibit-read-only t))
            (dired-align-file (point) (point-max)))
          (dired-hide-details-mode t)
          (dired-move-to-filename)
          (should (eq 2 (current-column))))
      (dired-hide-details-mode orig))))

(ert-deftest dired-test-bug27968 ()
  "Test for https://debbugs.gnu.org/27968 ."
  (let* ((top-dir (make-temp-file "top-dir" t))
         (subdir (expand-file-name "subdir" top-dir))
         (header-len-fn (lambda ()
                          (save-excursion
                            (goto-char 1)
                            (forward-line 1)
                            (- (point-at-eol) (point)))))
         orig-len len diff pos line-nb)
    (make-directory subdir 'parents)
    (unwind-protect
        (with-current-buffer (dired-noselect subdir)
          (setq orig-len (funcall header-len-fn)
                pos (point)
                line-nb (line-number-at-pos))
          ;; Bug arises when the header line changes its length; this may
          ;; happen if the used space has changed: for instance, with the
          ;; creation of additional files.
          (make-directory "subdir" t)
          (dired-revert)
          ;; Change the header line.
          (save-excursion
            (goto-char 1)
            (forward-line 1)
            (let ((inhibit-read-only t)
                  (new-header "  test-bug27968"))
              (delete-region (point) (point-at-eol))
              (when (= orig-len (length new-header))
                ;; Wow lucky guy! I must buy lottery today.
                (setq new-header (concat new-header " :-)")))
              (insert new-header)))
          (setq len (funcall header-len-fn)
                diff (- len orig-len))
          (should-not (zerop diff)) ; Header length has changed.
          ;; If diff > 0, then the point moves back.
          ;; If diff < 0, then the point moves forward.
          ;; If diff = 0, then the point doesn't move.
          ;; Sometimes this point movement causes
          ;; line-nb != (line-number-at-pos pos), so that we get
          ;; an unexpected file at point if we store buffer points.
          ;; Note that the line number before/after revert
          ;; doesn't change.
          (should (= line-nb
                     (line-number-at-pos)
                     (line-number-at-pos (+ pos diff))))
          ;; After revert, the point must be in 'subdir' line.
          (should (equal "subdir" (dired-get-filename 'local t))))
      (delete-directory top-dir t))))


(defmacro dired-test-with-temp-dirs (just-empty-dirs &rest body)
  "Helper macro for Bug#27940 test."
  (declare (indent 1) (debug body))
  (let ((dir (make-symbol "dir"))
        (ignore-funcs (make-symbol "ignore-funcs")))
    `(let* ((,dir (make-temp-file "bug27940" t))
            (dired-deletion-confirmer (lambda (_) "yes")) ; Suppress prompts.
            (inhibit-message t)
            (default-directory ,dir))
       (dotimes (i 5) (make-directory (format "empty-dir-%d" i)))
       (unless ,just-empty-dirs
         (dotimes (i 5) (make-directory (format "non-empty-%d/foo" i) 'parents)))
       (make-directory "zeta-empty-dir")
       (unwind-protect
           (progn
             ,@body)
         (delete-directory ,dir t)
         (kill-buffer (current-buffer))))))

(ert-deftest dired-test-bug27940 ()
  "Test for https://debbugs.gnu.org/27940 ."
  ;; If just empty dirs we shouldn't be prompted.
  (dired-test-with-temp-dirs
   'just-empty-dirs
   (let (asked)
     (advice-add 'read-answer
                 :override
                 (lambda (_q _a) (setq asked t) "")
                 '((name . dired-test-bug27940-advice)))
     (dired default-directory)
     (dired-toggle-marks)
     (dired-do-delete nil)
     (unwind-protect
         (progn
           (should-not asked)
           (should-not (dired-get-marked-files))) ; All dirs deleted.
       (advice-remove 'read-answer 'dired-test-bug27940-advice))))
  ;; Answer yes
  (dired-test-with-temp-dirs
   nil
   (advice-add 'read-answer :override (lambda (_q _a) "yes")
               '((name . dired-test-bug27940-advice)))
   (dired default-directory)
   (dired-toggle-marks)
   (dired-do-delete nil)
   (unwind-protect
       (should-not (dired-get-marked-files)) ; All dirs deleted.
     (advice-remove 'read-answer 'dired-test-bug27940-advice)))
  ;; Answer no
  (dired-test-with-temp-dirs
   nil
   (advice-add 'read-answer :override (lambda (_q _a) "no")
               '((name . dired-test-bug27940-advice)))
   (dired default-directory)
   (dired-toggle-marks)
   (dired-do-delete nil)
   (unwind-protect
       (should (= 5 (length (dired-get-marked-files)))) ; Just the empty dirs deleted.
     (advice-remove 'read-answer 'dired-test-bug27940-advice)))
  ;; Answer all
  (dired-test-with-temp-dirs
   nil
   (advice-add 'read-answer :override (lambda (_q _a) "all")
               '((name . dired-test-bug27940-advice)))
   (dired default-directory)
   (dired-toggle-marks)
   (dired-do-delete nil)
   (unwind-protect
       (should-not (dired-get-marked-files)) ; All dirs deleted.
     (advice-remove 'read-answer 'dired-test-bug27940-advice)))
  ;; Answer quit
  (dired-test-with-temp-dirs
   nil
   (advice-add 'read-answer :override (lambda (_q _a) "quit")
               '((name . dired-test-bug27940-advice)))
   (dired default-directory)
   (dired-toggle-marks)
   (let ((inhibit-message t))
     (dired-do-delete nil))
   (unwind-protect
       (should (= 6 (length (dired-get-marked-files)))) ; All empty dirs but zeta-empty-dir deleted.
     (advice-remove 'read-answer 'dired-test-bug27940-advice))))


(provide 'dired-tests)
;; dired-tests.el ends here
