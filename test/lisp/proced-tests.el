;;; proced-tests.el --- Test suite for proced.el -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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

(defun proced--cpu-at-point ()
  "Return as an integer the current CPU value at point."
  (if (string-suffix-p "nan" (thing-at-point 'sexp))
      (ert-skip
       (format
        "Found NaN value for %%CPU at point for process with PID %s"
        (substring-no-properties (thing-at-point 'sexp))))
    (thing-at-point 'number)))

(defun proced--assert-emacs-pid-in-buffer ()
  "Fail unless the process ID of the current Emacs process exists in buffer."
  (should (string-match-p
           (number-to-string (emacs-pid))
           (buffer-substring-no-properties (point-min) (point-max)))))

(defun proced--move-to-column (attribute)
  "Move to the column under ATTRIBUTE in the current proced buffer."
  (move-to-column (string-match attribute proced-header-line))
  ;; Sometimes the column entry does not fill the whole column.
  (while (= (char-after (point)) ?\s) (forward-char)))

(defun proced--assert-process-valid-cpu-refinement (cpu)
  "Fail unless the process at point could be present after a refinement using CPU."
  (proced--move-to-column "%CPU")
  (condition-case err
      (>= (proced--cpu-at-point) cpu)
    (ert-test-skipped (signal (car err) (cdr err)))
    (error
     (ert-fail
      (list err (proced--assert-process-valid-cpu-refinement-explainer cpu))))))

(defun proced--assert-process-valid-cpu-refinement-explainer (cpu)
  "Explain the result of `proced--assert-process-valid-cpu-refinement'.

CPU is as in `proced--assert-process-valid-cpu-refinement'."
  `(unexpected-refinement
    (header-line
     ,(substring-no-properties
       (string-replace "%%" "%" (cadr (proced-header-line)))))
    (buffer-process-line ,(thing-at-point 'line t))
    (process-attributes ,(format "%s" (process-attributes (proced-pid-at-point))))
    (refined-value ,cpu)
    (process-value
     ,(save-excursion
        (proced--move-to-column "%CPU")
        (or (thing-at-point 'number)
            (substring-no-properties (thing-at-point 'sexp)))))))

(put #'proced--assert-process-valid-cpu-refinement 'ert-explainer
     #'proced--assert-process-valid-cpu-refinement-explainer)

(ert-deftest proced-format-test ()
  (skip-when (eq system-type 'darwin)) ; Bug#76898
  (dolist (format '(short medium long verbose))
    (proced--within-buffer
     format
     'user
     (proced--assert-emacs-pid-in-buffer))))

(ert-deftest proced-update-test ()
  (skip-when (eq system-type 'darwin)) ; Bug#76898
  (proced--within-buffer
   'short
   'user
   (proced-update)
   (proced--assert-emacs-pid-in-buffer)))

(ert-deftest proced-revert-test ()
  (skip-when (eq system-type 'darwin)) ; Bug#76898
  (proced--within-buffer
   'short
   'user
   (proced-revert)
   (proced--assert-emacs-pid-in-buffer)))

(ert-deftest proced-color-test ()
  (skip-when (eq system-type 'darwin)) ; Bug#76898
  (let ((proced-enable-color-flag t))
    (proced--within-buffer
     'short
     'user
     (proced--assert-emacs-pid-in-buffer))))

(ert-deftest proced-refine-test ()
  ;; %CPU is not implemented on macOS
  (skip-when (eq system-type 'darwin))
  (proced--within-buffer
   'verbose
   'user
   ;; When refining on %CPU for process A, a process is kept if and only
   ;; if its %CPU is greater than or equal to that of process A.
   (proced--move-to-column "%CPU")
   (let ((cpu (proced--cpu-at-point)))
     (proced-refine)
     (while (not (eobp))
       (should (proced--assert-process-valid-cpu-refinement cpu))
       (forward-line)))))

(ert-deftest proced-refine-with-update-test ()
  (skip-when (eq system-type 'darwin))
  (proced--within-buffer
   'verbose
   'user
   (proced--move-to-column "%CPU")
   (let ((cpu (proced--cpu-at-point)))
     (proced-refine)
     ;; Don't use (proced-update t) since this will reset `proced-process-alist'
     ;; and it's possible the process refined on would have exited by that
     ;; point.  In this case proced will skip the refinement and show all
     ;; processes again, causing the test to fail.
     (proced-update)
     (while (not (eobp))
       (should (proced--assert-process-valid-cpu-refinement cpu))
       (forward-line)))))

(ert-deftest proced-update-preserves-pid-at-point-test ()
  ;; FIXME: Occasionally the cursor inexplicably changes to the first line which
  ;; causes the test to fail when the line isn't the Emacs process.
  :tags '(:unstable)
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
         (proced-update))
       (select-window old-window)
       (should (= pid (proced-pid-at-point)))))))

(provide 'proced-tests)
;;; proced-tests.el ends here
