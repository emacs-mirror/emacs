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
  (keymap-local-set "r" #'igc-roots)
  (keymap-local-set "s" #'igc-snapshot)
  (keymap-local-set "x" #'igc-clear)
  (display-line-numbers-mode -1)
  (setq header-line-format
	'((:eval (format " %-35s %10s %15s"
			 (concat "Display "
				 (symbol-name igc--display-mode))
			 "Objects"
			 "Bytes"))))
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
    (let ((info (igc--info-to-display))
	  (inhibit-read-only t)
	  (inhibit-modification-hooks t)
	  (standard-output (current-buffer)))
      (erase-buffer)
      (delete-all-overlays)
      (when info
	(cl-loop for (title n bytes) in info
		 do (insert (format "%-35s %10s %15s\n" title n
				    bytes)))
	(sort-lines nil (point-min) (point-max)))
      (goto-char (point-min))))
  (display-buffer "*igc*"))

(provide 'igc)
