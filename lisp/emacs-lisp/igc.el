;; -*- lexical-binding: t; symbol-packagaes: nil -*-
;;; memory-report.el --- Short function summaries  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Keywords: lisp, help

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

;; Todo (possibly): Font cache, regexp cache, bidi cache, various
;; buffer caches (newline cache, free_region_cache, etc), composition
;; cache, face cache.

;;; Code:

(require 'cl-lib)

(defun igc--diff (i1 i2)
  (cl-loop for (t1 n1 s1) in i1
	   for (_t2 n2 s2) in i2
	   unless (= n1 n2)
	   collect (list t1 (- n1 n2) (and s1 (- s1 s2)))))

(defvar igc--a nil)
(defvar igc--b nil)
(defvar igc--display-mode 'a)

(defun igc-snapshot ()
  (interactive)
  (if (eq igc--display-mode 'a)
      (setq igc--a (igc-info))
    (setq igc--b (igc-info)))
  (igc-stats))

(defun igc--info-to-display ()
  (cl-ecase igc--display-mode
    (diff (igc--diff igc--b igc--a))
    (a igc--a)
    (b igc--b)))

(defun igc-display-diff ()
  (interactive)
  (setq igc--display-mode 'diff)
  (igc-stats))

(defun igc-display-a ()
  (interactive)
  (setq igc--display-mode 'a)
  (igc-stats))

(defun igc-display-b ()
  (interactive)
  (setq igc--display-mode 'b)
  (igc-stats))

;;;###autoload
(defun igc-collect ()
  "GC, then set snapsort B to current `igc-info'."
  (interactive)
  (let ((garbage-collection-messages t))
    (igc--collect)))

(defun igc-clear ()
  "GC, then set snapsort B to current `igc-info'."
  (interactive)
  (setq igc--a nil igc--b nil)
  (igc-stats))

(define-derived-mode igc-stats-mode special-mode "Statistics"
  (keymap-local-set "a" #'igc-display-a)
  (keymap-local-set "b" #'igc-display-b)
  (keymap-local-set "c" #'igc-collect)
  (keymap-local-set "d" #'igc-display-diff)
  (keymap-local-set "s" #'igc-snapshot)
  (keymap-local-set "x" #'igc-clear)
  (display-line-numbers-mode -1)
  (setq header-line-format
	'((:eval (format " %-35s %10s %15s %10s"
			 (concat "Display "
				 (symbol-name igc--display-mode))
			 "Objects"
			 "Bytes"
			 "Avg"))))
  (setq-local revert-buffer-function
	      (lambda (&rest _)
		(setq igc--display-mode 'diff)
		(igc-snapshot)
		(igc-stats))))

;;;###autoload
(defun igc-stats ()
  "Display memory statistics from `igc-info'.
You can display two snapshots A nd B containing the info from `igc-info'
at different times. These can be displayed either as-is, or the
difference between them. To take a snapshot, display it then take
a snapshort. By reverting the buffer, take snapshot A, and display
the changes to snapshot A. See the modes's help."
  (interactive)
  (with-current-buffer (get-buffer-create "*igc*")
    (igc-stats-mode)
    (setq buffer-read-only t buffer-file-name nil)
    (let ((old-line (line-number-at-pos))
          (info (igc--info-to-display))
	  (inhibit-read-only t)
	  (inhibit-modification-hooks t)
	  (standard-output (current-buffer)))
      (erase-buffer)
      (delete-all-overlays)
      (when info
	(cl-loop for (title n bytes) in info do
                 (insert (format "%-35s %10s %15s %10s\n"
                                 title n bytes
                                 (and bytes n
                                      (if (zerop n)
                                          0
                                        (abs (/ bytes n)))))))
	(sort-lines nil (point-min) (point-max)))
      (goto-line old-line)))
  (display-buffer "*igc*"))

(defun igc--roots-diff (i1 i2)
  (cl-loop for (t1 n1 s1) in i1
	   for (_t2 n2 s2) in i2
	   unless (= n1 n2)
	   collect (list t1 (- n1 n2) (and s1 (- s1 s2)))))

(defvar igc--roots-a nil)
(defvar igc--roots-b nil)
(defvar igc--roots-display-mode 'a)

(defun igc-roots-display-diff ()
  (interactive)
  (setq igc--roots-display-mode 'diff)
  (igc-roots-stats))

(defun igc-roots-display-a ()
  (interactive)
  (setq igc--roots-display-mode 'a)
  (igc-roots-stats))

(defun igc-roots-display-b ()
  (interactive)
  (setq igc--roots-display-mode 'b)
  (igc-roots-stats))

(defun igc--roots-info ()
  (let ((h (make-hash-table :test 'equal)))
    (cl-loop for (label type start end) in (igc--roots)
             for (found _ n size) = (gethash label h) do
             (if found
                 (puthash label
                          (list label type (1+ n)
                                (and size
                                     (+ size (- end start))) h)
                          h)
               (puthash label
                        (list label type 1 (and end (- end start)))
                        h)))
    (cl-loop for i being the hash-values of h collect i)))

(defun igc--roots-snapshot ()
  (interactive)
  (if (eq igc--roots-display-mode 'a)
      (setq igc--roots-a (igc--roots-info))
    (setq igc--roots-b (igc--roots-info)))
  (igc-roots-stats))

(defun igc--roots-info-to-display ()
  (cl-ecase igc--roots-display-mode
    (diff (igc--roots-diff igc--b igc--a))
    (a igc--roots-a)
    (b igc--roots-b)))

(defun igc-roots-clear ()
  "GC, then set snapsort B to current `igc-info'."
  (interactive)
  (setq igc--roots-a nil igc--roots-b nil)
  (igc-roots-stats))

(define-derived-mode igc-roots-mode special-mode "Roots"
  (keymap-local-set "a" #'igc-roots-display-a)
  (keymap-local-set "b" #'igc-roots-display-b)
  (keymap-local-set "c" #'igc-collect)
  (keymap-local-set "d" #'igc-roots-display-diff)
  (keymap-local-set "s" #'igc--roots-snapshot)
  (keymap-local-set "x" #'igc-roots-clear)
  (display-line-numbers-mode -1)
  (setq header-line-format
	'((:eval (format " %-25s %10s %15s %15s"
			 (concat "Display "
				 (symbol-name igc--roots-display-mode))
			 "Type" "N" "Bytes"))))
  (setq-local revert-buffer-function
	      (lambda (&rest _)
		(setq igc--roots-display-mode 'diff)
		(igc--roots-snapshot)
		(igc-roots-stats))))

;;;###autoload
(defun igc-roots-stats ()
  "Display root statistics from `igc--roots'.
You can display two snapshots A nd B containing the info from `igc--roots'
at different times. These can be displayed either as-is, or the
difference between them. To take a snapshot, display it then take
a snapshort. By reverting the buffer, take snapshot A, and display
the changes to snapshot A. See the modes's help."
  (interactive)
  (with-current-buffer (get-buffer-create "*igc roots*")
    (igc-roots-mode)
    (setq buffer-read-only t buffer-file-name nil)
    (let ((info (igc--roots-info-to-display))
	  (inhibit-read-only t)
	  (inhibit-modification-hooks t)
	  (standard-output (current-buffer)))
      (erase-buffer)
      (delete-all-overlays)
      (when info
	(cl-loop for (label type n size) in info
		 do (insert (format "%-25s %10s %15s %15s\n"
                                    label type n size)))
	(sort-lines nil (point-min) (point-max)))
      (goto-char (point-min))))
  (display-buffer "*igc roots*"))

(defvar igc--collect-timer nil)
(defvar igc--collect-file nil)

(defun igc-stop-collecting-stats ()
  (interactive)
  (when igc--collect-timer
    (cancel-timer igc--collect-timer)
    (setq igc--collect-timer nil)))

(defun igc--collect-stats ()
  (let ((buffer (get-file-buffer igc--collect-file)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-max))
        (when (= (point-min) (point-max))
          (insert (format "\"%s\",\"%s\",\"%s\",\"%s\"\n"
                          "Time" "Type" "N" "Bytes")))
        (cl-loop with time = (current-time-string)
                 for (title n bytes) in (igc-info) do
                 (insert (format "\"%s\",\"%s\",\"%s\",\"%s\"\n"
                                 time title n bytes))))
      (save-buffer))))

(defun igc-start-collecting-stats (file secs)
  "Start collecting statistics every SECS seconds."
  (interactive "FOutput file: \nnInterval (seconds): ")
  (igc-stop-collecting-stats)
  (setq igc--collect-file file)
  (find-file-noselect file)
  (setq igc--collect-timer (run-at-time nil secs #'igc--collect-stats)))

(provide 'igc)
