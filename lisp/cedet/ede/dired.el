;;; ede/dired.el --- EDE extensions to dired.

;; Copyright (C) 1998, 1999, 2000, 2003, 2009, 2010
;;   Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.4
;; Keywords: project, make

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This provides a dired interface to EDE, allowing users to modify
;; their project file by adding files (or whatever) directly from a
;; dired buffer.

(require 'easymenu)
(require 'dired)
(require 'ede)

;;; Code:
(defvar ede-dired-minor-mode nil
  "Non-nil when in ede dired minor mode.")
(make-variable-buffer-local 'ede-dired-minor-mode)

(defvar ede-dired-keymap nil
  "Keymap used for ede dired minor mode.")

(if ede-dired-keymap
    nil
  (setq ede-dired-keymap (make-sparse-keymap))
  (define-key ede-dired-keymap ".a" 'ede-dired-add-to-target)
  (define-key ede-dired-keymap ".t" 'ede-new-target)
  (define-key ede-dired-keymap ".s" 'ede-speedbar)
  (define-key ede-dired-keymap ".C" 'ede-compile-project)
  (define-key ede-dired-keymap ".d" 'ede-make-dist)

  (easy-menu-define
   ede-dired-menu ede-dired-keymap "EDE Dired Minor Mode Menu"
   '("Project"
     [ "Add files to target" ede-dired-add-to-target (ede-current-project) ]
     ( "Build" :filter ede-build-forms-menu)
     "-"
     [ "Create Project" ede-new (not (ede-current-project)) ]
     [ "Create Target" ede-new-target (ede-current-project) ]
     "-"
     ( "Customize Project" :filter ede-customize-forms-menu )
     [ "View Project Tree" ede-speedbar (ede-current-project) ]
     ))
  )

(defun ede-dired-minor-mode (&optional arg)
  "A minor mode that should only be activated in DIRED buffers.
If ARG is nil, toggle, if it is a positive number, force on, if
negative, force off."
  (interactive "P")
  (if (not (or (eq major-mode 'dired-mode)
	       (eq major-mode 'vc-dired-mode)))
      (error "Not in DIRED mode"))
  (setq ede-dired-minor-mode
	(not (or (and (null arg) ede-dired-minor-mode)
		 (<= (prefix-numeric-value arg) 0))))
  (if (and (not (ede-directory-project-p default-directory))
	   (not (interactive-p)))
      (setq ede-dired-minor-mode nil))
  )

(defun ede-dired-add-to-target (target)
  "Add a file, or all marked files into a TARGET."
  (interactive (list
		(let ((ede-object (ede-current-project)))
		  (ede-invoke-method 'project-interactive-select-target
				     "Add files to Target: "))))
  (let ((files (dired-get-marked-files t)))
    (while files
      (project-add-file target (car files))
      ;; Find the buffer for this files, and set its ede-object
      (if (get-file-buffer (car files))
	  (with-current-buffer (get-file-buffer (car files))
	    (setq ede-object nil)
	    (setq ede-object (ede-buffer-object (current-buffer)))))
      ;; Increment.
      (setq files (cdr files)))))

;; Minor mode management.
(add-to-list 'minor-mode-alist '(ede-dired-minor-mode " EDE"))
(let ((a (assoc 'ede-dired-minor-mode minor-mode-map-alist)))
  (if a
      (setcdr a ede-dired-keymap)
    (add-to-list 'minor-mode-map-alist (cons 'ede-dired-minor-mode
					     ede-dired-keymap))))

(provide 'ede/dired)

;; arch-tag: 95d3e0a7-a8b7-43a9-b7df-ba647e4c56f6
;;; ede/dired.el ends here
