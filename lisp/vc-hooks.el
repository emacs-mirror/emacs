;;; vc-hooks.el --- resident support for version-control

;; Copyright (C) 1992, 1993 Free Software Foundation, Inc.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Version: 5.3

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
  "*If non-nil, backups of registered files are made as with other files.
If nil (the default), for files covered by version control don't get backups.")

(defvar vc-rcs-status t
  "*If non-nil, revision and locks on RCS working file displayed in modeline.
Otherwise, not displayed.")

;; Tell Emacs about this new kind of minor mode
(if (not (assoc 'vc-mode minor-mode-alist))
    (setq minor-mode-alist (cons '(vc-mode vc-mode)
				 minor-mode-alist)))

(make-variable-buffer-local 'vc-mode)
(put 'vc-mode 'permanent-local t)

;; We need a notion of per-file properties because the version
;; control state of a file is expensive to derive --- we don't
;; want to recompute it even on every find.

(defmacro vc-error-occurred (&rest body)
  (list 'condition-case nil (cons 'progn (append body '(nil))) '(error t)))

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

(defun vc-name (file)
  "Return the master name of a file, nil if it is not registered."
  (or (vc-file-getprop file 'vc-name)
      (let ((name-and-type (vc-registered file)))
	(if name-and-type
	    (progn
	      (vc-file-setprop file 'vc-backend (cdr name-and-type))
	      (vc-file-setprop file 'vc-name (car name-and-type)))))))

(defun vc-backend-deduce (file)
  "Return the version-control type of a file, nil if it is not registered."
  (and file
       (or (vc-file-getprop file 'vc-backend)
	   (let ((name-and-type (vc-registered file)))
	     (if name-and-type
		 (progn
		   (vc-file-setprop file 'vc-name (car name-and-type))
		   (vc-file-setprop file 'vc-backend (cdr name-and-type))))))))

(defun vc-toggle-read-only ()
  "Change read-only status of current buffer, perhaps via version control.
If the buffer is visiting a file registered with version control,
then check the file in or out.  Otherwise, just change the read-only flag
of the buffer."
  (interactive)
  (if (vc-backend-deduce (buffer-file-name))
      (vc-next-action nil)
    (toggle-read-only)))
(define-key global-map "\C-x\C-q" 'vc-toggle-read-only)

(defun vc-mode-line (file &optional label)
  "Set `vc-mode' to display type of version control for FILE.
The value is set in the current buffer, which should be the buffer
visiting FILE."
  (interactive (list buffer-file-name nil))
  (let ((vc-type (vc-backend-deduce file)))
    (setq vc-mode
	  (and vc-type
	       (concat " " (or label (symbol-name vc-type))
		       (if (and vc-rcs-status (eq vc-type 'RCS))
			   (vc-rcs-status file)))))
    ;; Even root shouldn't modify a registered file without locking it first.
    (and vc-type
	 (not buffer-read-only)
	 (zerop (user-uid))
	 (require 'vc)
	 (not (string-equal (user-login-name) (vc-locking-user file)))
	 (setq buffer-read-only t))
    ;; force update of mode line
    (set-buffer-modified-p (buffer-modified-p))
    vc-type))

(defun vc-rcs-status (file)
  ;; Return string for placement in modeline by `vc-mode-line'.
  ;; If FILE is not registered under RCS, return nil.
  ;; If FILE is registered but not locked, return " REV" if there is a head
  ;; revision and " @@" otherwise.
  ;; If FILE is locked then return all locks in a string of the
  ;; form " LOCKER1:REV1 LOCKER2:REV2 ...", where "LOCKERi:" is empty if you
  ;; are the locker, and otherwise is the name of the locker followed by ":".

  ;; Algorithm: 

  ;; 1. Check for master file corresponding to FILE being visited.
  ;; 
  ;; 2. Insert the first few characters of the master file into a work
  ;; buffer.
  ;;  
  ;; 3. Search work buffer for "locks...;" phrase; if not found, then
  ;; keep inserting more characters until the phrase is found.
  ;; 
  ;; 4. Extract the locks, and remove control characters
  ;; separating them, like newlines; the string " user1:revision1
  ;; user2:revision2 ..." is returned.

  ;; Limitations:

  ;; The output doesn't show which version you are actually looking at.
  ;; The modeline can get quite cluttered when there are multiple locks.
  ;; The head revision is probably not what you want if you've used `rcs -b'.

  (let ((master (vc-name file))
	found)

    ;; If master file exists, then parse its contents, otherwise we return the 
    ;; nil value of this if form.
    (if master
        (save-excursion

          ;; Create work buffer.
          (set-buffer (get-buffer-create " *vc-rcs-status*"))
          (setq buffer-read-only nil
                default-directory (file-name-directory master))
          (erase-buffer)

          ;; Check if we have enough of the header.
	  ;; If not, then keep including more.
          (while
	      (not (or found
		       (let ((s (buffer-size)))
			 (goto-char (1+ s))
			 (zerop (car (cdr (insert-file-contents
					   master nil s (+ s 8192))))))))
	    (beginning-of-line)
	    (setq found (re-search-forward "^locks\\([^;]*\\);" nil t)))

          (if found
	      ;; Clean control characters and self-locks from text.
	      (let* ((lock-pattern
		      (concat "[ \b\t\n\v\f\r]+\\("
			      (regexp-quote (user-login-name))
			      ":\\)?"))
		     (locks
		      (save-restriction
			(narrow-to-region (match-beginning 1) (match-end 1))
			(goto-char (point-min))
			(while (re-search-forward lock-pattern nil t)
			  (replace-match (if (eobp) "" ":") t t))
			(buffer-string)))
		     (status
		      (if (not (string-equal locks ""))
			  locks
			(goto-char (point-min))
			(if (looking-at "head[ \b\t\n\v\f\r]+\\([.0-9]+\\)")
			    (concat "-" (buffer-substring (match-beginning 1)
							  (match-end 1)))
			  " @@"))))
		;; Clean work buffer.
		(erase-buffer)
		(set-buffer-modified-p nil)
		status))))))

;;; install a call to the above as a find-file hook
(defun vc-find-file-hook ()
  ;; Recompute whether file is version controlled,
  ;; if user has killed the buffer and revisited.
  (if buffer-file-name
      (vc-file-setprop buffer-file-name 'vc-backend nil))
  (if (and (vc-mode-line buffer-file-name) (not vc-make-backup-files))
      (progn
	(make-local-variable 'make-backup-files)
	(setq make-backup-files nil))))

(add-hook 'find-file-hooks 'vc-find-file-hook)

;;; more hooks, this time for file-not-found
(defun vc-file-not-found-hook ()
  "When file is not found, try to check it out from RCS or SCCS.
Returns t if checkout was successful, nil otherwise."
  (if (vc-backend-deduce buffer-file-name)
      (progn
	(require 'vc)
	(not (vc-error-occurred (vc-checkout buffer-file-name))))))

(add-hook 'find-file-not-found-hooks 'vc-file-not-found-hook)

;;; Now arrange for bindings and autoloading of the main package.
;;; Bindings for this have to go in the global map, as we'll often
;;; want to call them from random buffers.

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
      (define-key vc-prefix-map "~" 'vc-version-other-window)
      ))

(provide 'vc-hooks)

;;; vc-hooks.el ends here
