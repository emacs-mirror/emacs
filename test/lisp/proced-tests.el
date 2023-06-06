;;; proced-tests.el --- Test suite for proced.el -*- lexical-binding: t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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
(require 'proced)
(require 'thingatpt)

(cl-defmacro proced--within-buffer (format filter &body body)
  "Execute BODY within a proced buffer using format FORMAT and filter FILTER."
  `(let ((proced-format ,format)
         (proced-filter ,filter)
         (proced-auto-update-flag nil)
         (inhibit-message t))
     (proced)
     (unwind-protect
         (with-current-buffer "*Proced*"
           ,@body)
       (kill-buffer "*Proced*"))))

(defun proced--assert-emacs-pid-in-buffer ()
  "Fail unless the process ID of the current Emacs process exists in buffer."
  (should (string-match-p
           (number-to-string (emacs-pid))
           (buffer-substring-no-properties (point-min) (point-max)))))

(defun proced--move-to-column (attribute)
  "Move to the column under ATTRIBUTE in the current proced buffer."
  (move-to-column (string-match attribute proced-header-line)))

(ert-deftest proced-format-test ()
  (dolist (format '(short medium long verbose))
    (proced--within-buffer
     format
     'user
     (proced--assert-emacs-pid-in-buffer))))

(ert-deftest proced-update-test ()
  (proced--within-buffer
   'short
   'user
   (proced-update)
   (proced--assert-emacs-pid-in-buffer)))

(ert-deftest proced-revert-test ()
  (proced--within-buffer
   'short
   'user
   (proced-revert)
   (proced--assert-emacs-pid-in-buffer)))

(ert-deftest proced-color-test ()
  (let ((proced-enable-color-flag t))
    (proced--within-buffer
     'short
     'user
     (proced--assert-emacs-pid-in-buffer))))

(ert-deftest proced-refine-test ()
  ;;(skip-unless (memq system-type '(gnu/linux gnu/kfreebsd darwin)))
  (proced--within-buffer
   'medium
   'user
   ;; When refining on PID for process A, a process is kept if and only
   ;; if its PID are the same as process A, which more or less guarentees
   ;; the refinement will remove some processes.
   (proced--move-to-column "PID")
   (let ((pid (word-at-point)))
     (proced-refine)
     (while (not (eobp))
       (proced--move-to-column "PID")
       (should (string= pid (word-at-point)))
       (forward-line)))))

(ert-deftest proced-refine-with-update-test ()
  (proced--within-buffer
   'medium
   'user
   (proced--move-to-column "PID")
   (let ((pid (word-at-point)))
     (proced-refine)
     ;; Don't use (proced-update t) since this will reset `proced-process-alist'
     ;; and it's possible the process refined on would have exited by that
     ;; point.  In this case proced will skip the refinement and show all
     ;; processes again, causing the test to fail.
     (proced-update)
     (while (not (eobp))
       (proced--move-to-column "PID")
       (should (string= pid (word-at-point)))
       (forward-line)))))

(ert-deftest proced-update-preserves-pid-at-point-test ()
  (proced--within-buffer
   'medium
   'user
   (goto-char (point-min))
   (search-forward (number-to-string (emacs-pid)))
   (proced--move-to-column "PID")
   (save-window-excursion
     (let ((pid (proced-pid-at-point))
           (new-window (split-window))
           (old-window (get-buffer-window)))
       (select-window new-window)
       (with-current-buffer "*Proced*"
         (proced-update t t))
       (select-window old-window)
       (should (= pid (proced-pid-at-point)))))))

(provide 'proced-tests)
;;; proced-tests.el ends here
