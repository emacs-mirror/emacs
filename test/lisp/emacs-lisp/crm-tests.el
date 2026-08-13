;;; crm-tests.el --- Tests for crm.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026-2026 Free Software Foundation, Inc.

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

;;

;;; Code:

(require 'crm)
(require 'ert)

;; Copied from minibuffer-tests.el
(defmacro with-minibuffer-setup (completing-read &rest body)
  (declare (indent 1) (debug t))
  `(catch 'result
     (minibuffer-with-setup-hook
         (lambda ()
           (let ((redisplay-skip-initial-frame nil)
                 (executing-kbd-macro nil)) ; Don't skip redisplay
             (throw 'result (progn . ,body))))
       (let ((executing-kbd-macro t)) ; Force the real minibuffer
         ,completing-read))))

(defmacro crm-test-with-file-name-table (initial-input &rest body)
  (declare (indent 1) (debug t))
  `(let* ((root (make-temp-file "crm-boundary-" 'directory))
          (default-directory root))
     (unwind-protect
         (progn
           (make-directory "dir")
           (make-directory "dir/subdir")
           (with-temp-file "dir/alpha" (insert ""))
           (with-temp-file "dir/beta" (insert ""))
           (with-temp-file "dir/subdir/gamma" (insert ""))
           (with-minibuffer-setup
               (completing-read-multiple "Pick: "
                                         #'completion-file-name-table
                                         nil t ,initial-input)
             ,@body))
       (delete-directory root t))))

(ert-deftest crm-test-complete-uses-boundaries ()
  (crm-test-with-file-name-table "dir/a"
    (save-excursion (insert ",d/b"))
    (should (equal (minibuffer-contents) "dir/a,d/b"))
    (execute-kbd-macro (kbd "TAB"))
    (should (equal (minibuffer-contents) "dir/alpha,d/b"))
    ;; complete between d and /
    (execute-kbd-macro (kbd "C-f C-f TAB"))
    (should (equal (minibuffer-contents) "dir/alpha,dir/b"))
    (execute-kbd-macro (kbd "C-e TAB"))
    (should (equal (minibuffer-contents) "dir/alpha,dir/beta"))
    (execute-kbd-macro (kbd ", d/s/g TAB"))
    (should (equal (minibuffer-contents) "dir/alpha,dir/beta,dir/subdir/gamma"))))

(ert-deftest crm-test-choose-completion-uses-boundaries ()
  "Regression test for Bug#81411."
  (crm-test-with-file-name-table "dir/a"
    (save-excursion (insert ",d/b"))
    (should (equal (minibuffer-contents) "dir/a,d/b"))
    (execute-kbd-macro (kbd "? M-<down> M-RET"))
    (should (equal (minibuffer-contents) "dir/alpha,d/b"))
    ;; complete between d and /
    (execute-kbd-macro (kbd "C-f C-f ? M-<down> M-RET"))
    (should (equal (minibuffer-contents) "dir/alpha,dir/b"))
    (execute-kbd-macro (kbd "C-e ? M-<down> M-RET"))
    (should (equal (minibuffer-contents) "dir/alpha,dir/beta"))
    (execute-kbd-macro (kbd ", d/s/g ? M-<down> M-RET"))
    (should (equal (minibuffer-contents) "dir/alpha,dir/beta,dir/subdir/gamma"))))

(ert-deftest crm-test-choose-completion-multi-char-separator ()
  "Regression test for Bug#81411."
  (crm-test-with-file-name-table "dir/a"
    (save-excursion (insert " ,  d/b"))
    (should (equal (minibuffer-contents) "dir/a ,  d/b"))
    (execute-kbd-macro (kbd "? M-<down> M-RET"))
    (should (equal (minibuffer-contents) "dir/alpha ,  d/b"))
    ;; complete between d and /
    (forward-char 5)
    (execute-kbd-macro (kbd "? M-<down> M-RET"))
    (should (equal (minibuffer-contents) "dir/alpha ,  dir/b"))
    (execute-kbd-macro (kbd "C-e ? M-<down> M-RET"))
    (should (equal (minibuffer-contents) "dir/alpha ,  dir/beta"))))

(provide 'crm-tests)
;;; crm-tests.el ends here
