;;; w32-feature.el --- Check Availability of Emacs Features  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Free Software Foundation, Inc.

;; Author: Phillip Lord <phillip.lord@russet.org.uk>

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

;; This file provides tests for various features of Emacs. It is
;; designed to check whether bundled binary distributions of Emacs on
;; windows are fully functional.

;; By default is checks whether the features that we are expect to be
;; available on Emacs for Windows are reported to be available. It
;; should be possible to run these tests from a distributed version of
;; Emacs.

;; In addition, it provides a single command
;; `w32-feature-load-tests'. If the full source repository of Emacs is
;; available, this will load selected files from the repository which
;; test these features.

;;; Code:
(defvar w32-msys-process-log "*w32-process-log*")
(defvar w32-msys-buffer "*w32-install*")

(defvar w32-install-queue nil)

(defun w32-msys-run (command)
  (with-current-buffer
      (get-buffer-create w32-msys-process-log)
    (goto-char (point-max))
    (insert (format "%s\n\n\n"
             (mapconcat 'identity command " "))))
  (make-process
   :name "w32-msys-install"
   :buffer w32-msys-process-log
   :sentinel 'w32-msys-install-sentinel
   :command command))

(defun w32-msys-install-sentinel (p state)
  (cond
   ((equal state "finished\n")
    (if w32-install-queue
        (w32-msys-install-next-queue)
      (w32-msys-complete)))
   (t
    (message "unexepect state: %s" state))))

(defun w32-msys-install-add-to-site-start ()
  (write-region
   "(load-library (expand-file-name \"w32-msys-site-start.el\" data-directory))"
   nil
   (expand-file-name "../site-lisp/site-start.el" data-directory)))

(defun w32-msys-install-next-queue ()
  (let ((c (car w32-install-queue)))
    (setq w32-install-queue (cdr w32-install-queue))
    (w32-msys-run c)))

(defun w32-msys-run-queue ()
  (setq w32-install-queue (reverse w32-install-queue))
  (w32-msys-install-next-queue))

(defun w32-msys-command (command-line)
  (setq w32-install-queue (cons (split-string command-line " ")
                                w32-install-queue)))

(defun w32-msys-install ()
  (interactive)
  (w32-msys-install-add-to-site-start)
  ;; Assume for now that msys has been set up correctly
  (w32-msys-command "pacman --noconfirm -Su")
  (w32-msys-command "pacman --noconfirm -S git")
  (w32-msys-run-queue))

(defun w32-msys-install-dialog ()
  (interactive)
  (setq mode-line-format nil)
  (scroll-bar-mode 0)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (w32-msys-install))

(defun w32-msys-complete ()
  (message "complete"))

;; (setq mode-line-format nil)
;; (scroll-bar-mode 0)

;;; w32-msys-install.el ends here
