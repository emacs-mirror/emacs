;;; vc-hooks.el -- resident support for version-control

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Version: 4.0

;;	$Id: vc-hooks.el,v 1.6 1992/10/24 20:07:08 rms Exp rms $	

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; See the commentary of vc.el.

;;; Code:

(defvar vc-master-templates
  '(("%sRCS/%s,v" . RCS) ("%s%s,v" . RCS) ("%sRCS/%s" . RCS)
    ("%sSCCS/s.%s" . SCCS) ("%ss.%s". SCCS))
  "*Where to look for version-control master files.
The first pair corresponding to a given back end is used as a template
when creating new masters.")

(defvar vc-make-backup-files nil
  "*If non-nil, backups of registered files are made according to
the make-backup-files variable.  Otherwise, prevents backups being made.")

;; Tell Emacs about this new kind of minor mode
(if (not (assoc 'vc-mode-string minor-mode-alist))
    (setq minor-mode-alist (cons '(vc-mode-string vc-mode-string)
				 minor-mode-alist)))

(make-variable-buffer-local 'vc-mode-string)

;; We need a notion of per-file properties because the version
;; control state of a file is expensive to derive --- we don't
;; want to recompute it even on every find.

(defvar vc-file-prop-obarray [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
  "Obarray for per-file properties.")

(defun vc-file-setprop (file property value)
  ;; set per-file property
  (put (intern file vc-file-prop-obarray) property value))

(defun vc-file-getprop (file property)
  ;; get per-file property
  (get (intern file vc-file-prop-obarray) property))

;;; actual version-control code starts here

(defun vc-registered (file)
  (let (handler handlers)
    (if (boundp 'file-name-handler-alist)
	(save-match-data
	  (setq handlers file-name-handler-alist)
	  (while (and (consp handlers) (null handler))
	    (if (and (consp (car handlers))
		     (stringp (car (car handlers)))
		     (string-match (car (car handlers)) file))
		(setq handler (cdr (car handlers))))
	    (setq handlers (cdr handlers)))))
    (if handler
	(funcall handler 'vc-registered file)
      ;; Search for a master corresponding to the given file
      (let ((dirname (or (file-name-directory file) ""))
	    (basename (file-name-nondirectory file)))
	(catch 'found
	  (mapcar
	   (function (lambda (s)
	      (let ((trial (format (car s) dirname basename)))
		(if (and (file-exists-p trial)
			 ;; Make sure the file we found with name
			 ;; TRIAL is not the source file itself.
			 ;; That can happen with RCS-style names
			 ;; if the file name is truncated
			 ;; (e.g. to 14 chars).  See if either
			 ;; directory or attributes differ.
			 (or (not (string= dirname
					   (file-name-directory trial)))
			     (not (equal
				   (file-attributes file)
				   (file-attributes trial)))))
		    (throw 'found (cons trial (cdr s)))))))
	   vc-master-templates)
	  nil)))))

(defun vc-backend-deduce (file)
  "Return the version-control type of a file, nil if it is not registered"
  (and file
       (or (vc-file-getprop file 'vc-backend)
	   (vc-file-setprop file 'vc-backend (cdr (vc-registered file))))))

(defun vc-toggle-read-only ()
  "If the file in the current buffer is under version control, perform the
logical next version-control action; otherwise, just toggle the buffer's
read-only flag."
  (interactive)
  (if (vc-backend-deduce (buffer-file-name))
      (vc-next-action nil)
    (toggle-read-only)))

(defun vc-mode-line (file &optional label)
  "Set `vc-mode-string' to display type of version control for FILE.
The value is set in the current buffer, which should be the buffer
visiting FILE."
  (interactive (list buffer-file-name nil))
  (let ((vc-type (vc-backend-deduce file)))
    (if vc-type
	(progn
	  (if (null (current-local-map))
	      (use-local-map (make-sparse-keymap)))
	  (define-key (current-local-map) "\C-x\C-q" 'vc-toggle-read-only)
	  (setq vc-mode-string
		(concat " " (or label (symbol-name vc-type))))))
    ;; force update of mode line
    (set-buffer-modified-p (buffer-modified-p))
    vc-type))

;;; install a call to the above as a find-file hook
(defun vc-find-file-hook ()
  ;; Recompute whether file is version controlled,
  ;; if user has killed the buffer and revisited.
  (vc-file-setprop buffer-file-name 'vc-backend nil)
  (if (and (vc-mode-line buffer-file-name) (not vc-make-backup-files))
      (progn
	(make-local-variable 'make-backup-files)
	(setq make-backup-files nil))))

(or (memq 'vc-find-file-hook find-file-hooks)
    (setq find-file-hooks
	  (cons 'vc-find-file-hook find-file-hooks)))

;;; more hooks, this time for file-not-found
(defun vc-file-not-found-hook ()
  "When file is not found, try to check it out from RCS or SCCS.
Returns t if checkout was successful, nil otherwise."
  (if (vc-backend-deduce buffer-file-name)
      (progn
	(require 'vc)
	(not (vc-error-occurred (vc-checkout buffer-file-name))))))

(or (memq 'vc-file-not-found-hook find-file-not-found-hooks)
    (setq find-file-not-found-hooks
	  (cons 'vc-file-not-found-hook find-file-not-found-hooks)))

;;; Now arrange for bindings and autoloading of the main package.
;;; Bindings for this have to go in the global map, as it may have
;;; to coexist with a lot of different major modes.

(setq vc-prefix-map (lookup-key global-map "\C-xv"))
(if (not (keymapp vc-prefix-map))
    (progn
      (setq vc-prefix-map (make-sparse-keymap))
      (define-key global-map "\C-xv" vc-prefix-map)
      (define-key vc-prefix-map "a" 'vc-update-change-log)
      (define-key vc-prefix-map "c" 'vc-cancel-version)
      (define-key vc-prefix-map "d" 'vc-directory)
      (define-key vc-prefix-map "h" 'vc-insert-headers)
      (define-key vc-prefix-map "i" 'vc-register)
      (define-key vc-prefix-map "l" 'vc-print-log)
      (define-key vc-prefix-map "r" 'vc-retrieve-snapshot)
      (define-key vc-prefix-map "s" 'vc-create-snapshot)
      (define-key vc-prefix-map "u" 'vc-revert-buffer)
      (define-key vc-prefix-map "v" 'vc-next-action)
      (define-key vc-prefix-map "=" 'vc-diff)
      ))

(provide 'vc-hooks)

;;; vc-hooks.el ends here
