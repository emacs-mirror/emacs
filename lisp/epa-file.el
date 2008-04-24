;;; epa-file.el --- the EasyPG Assistant, transparent file encryption
;; Copyright (C) 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: PGP, GnuPG

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'epa)

(defgroup epa-file nil
  "The EasyPG Assistant hooks for transparent file encryption"
  :version "23.1"
  :group 'epa)

(defun epa-file--file-name-regexp-set (variable value)
  (set-default variable value)
  (if (fboundp 'epa-file-name-regexp-update)
      (epa-file-name-regexp-update)))

(defcustom epa-file-name-regexp "\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'"
  "Regexp which matches filenames to be encrypted with GnuPG.

If you set this outside Custom while epa-file is already enabled, you
have to call `epa-file-name-regexp-update' after setting it to
properly update file-name-handler-alist.  Setting this through Custom
does that automatically."
  :type 'regexp
  :group 'epa-file
  :set 'epa-file--file-name-regexp-set)

(defcustom epa-file-cache-passphrase-for-symmetric-encryption nil
  "If non-nil, cache passphrase for symmetric encryption."
  :type 'boolean
  :group 'epa-file)

(defcustom epa-file-inhibit-auto-save t
  "If non-nil, disable auto-saving when opening an encrypted file."
  :type 'boolean
  :group 'epa-file)

(defcustom epa-file-select-keys nil
  "If non-nil, always asks user to select recipients."
  :type 'boolean
  :group 'epa-file)

(defvar epa-file-encrypt-to nil
  "*Recipient(s) used for encrypting files.
May either be a string or a list of strings.")

;;;###autoload
(put 'epa-file-encrypt-to 'safe-local-variable
     (lambda (val)
       (or (stringp val)
	   (and (listp val)
		(catch 'safe
		  (mapc (lambda (elt)
			  (unless (stringp elt)
			    (throw 'safe nil)))
			val)
		  t)))))

;;;###autoload
(put 'epa-file-encrypt-to 'permanent-local t)

(defvar epa-file-handler
  (cons epa-file-name-regexp 'epa-file-handler))

(defvar epa-file-auto-mode-alist-entry
  (list epa-file-name-regexp nil 'epa-file))

(defvar epa-file-passphrase-alist nil)

(eval-and-compile
  (if (fboundp 'encode-coding-string)
      (defalias 'epa-file--encode-coding-string 'encode-coding-string)
    (defalias 'epa-file--encode-coding-string 'identity)))

(eval-and-compile
  (if (fboundp 'decode-coding-string)
      (defalias 'epa-file--decode-coding-string 'decode-coding-string)
    (defalias 'epa-file--decode-coding-string 'identity)))

(defun epa-file-name-regexp-update ()
  (interactive)
  (unless (equal (car epa-file-handler) epa-file-name-regexp)
    (setcar epa-file-handler epa-file-name-regexp)))

(defun epa-file-passphrase-callback-function (context key-id file)
  (if (and epa-file-cache-passphrase-for-symmetric-encryption
	   (eq key-id 'SYM))
      (progn
	(setq file (file-truename file))
	(let ((entry (assoc file epa-file-passphrase-alist))
	      passphrase)
	  (or (copy-sequence (cdr entry))
	      (progn
		(unless entry
		  (setq entry (list file)
			epa-file-passphrase-alist
			(cons entry
			      epa-file-passphrase-alist)))
		(setq passphrase (epa-passphrase-callback-function context
								   key-id nil))
		(setcdr entry (copy-sequence passphrase))
		passphrase))))
    (epa-passphrase-callback-function context key-id nil)))

(defun epa-file-handler (operation &rest args)
  (save-match-data
    (let ((op (get operation 'epa-file)))
      (if op
	  (apply op args)
	(epa-file-run-real-handler operation args)))))

(defun epa-file-run-real-handler (operation args)
  (let ((inhibit-file-name-handlers
	 (cons 'epa-file-handler
	       (and (eq inhibit-file-name-operation operation)
		    inhibit-file-name-handlers)))
	(inhibit-file-name-operation operation))
    (apply operation args)))

(defun epa-file-decode-and-insert (string file visit beg end replace)
  (if (fboundp 'decode-coding-inserted-region)
      (save-restriction
	(narrow-to-region (point) (point))
	(let ((multibyte enable-multibyte-characters))
	  (set-buffer-multibyte nil)
	  (insert string)
	  (set-buffer-multibyte multibyte)
	  (decode-coding-inserted-region
	   (point-min) (point-max)
	   (substring file 0 (string-match epa-file-name-regexp file))
	   visit beg end replace)))
    (insert (epa-file--decode-coding-string string (or coding-system-for-read
						       'undecided)))))

(defvar last-coding-system-used)
(defun epa-file-insert-file-contents (file &optional visit beg end replace)
  (barf-if-buffer-read-only)
  (if (and visit (or beg end))
      (error "Attempt to visit less than an entire file"))
  (setq file (expand-file-name file))
  (let* ((local-copy
	  (condition-case inl
	      (epa-file-run-real-handler #'file-local-copy (list file))
	    (error)))
	 (local-file (or local-copy file))
	 (context (epg-make-context))
	 string length entry)
    (if visit
	(setq buffer-file-name file))
    (epg-context-set-passphrase-callback
     context
     (cons #'epa-file-passphrase-callback-function
	   local-file))
    (epg-context-set-progress-callback context
				       #'epa-progress-callback-function)
    (unwind-protect
	(progn
	  (if replace
	      (goto-char (point-min)))
	  (condition-case error
	      (setq string (epg-decrypt-file context local-file nil))
	    (error
	     (if (setq entry (assoc file epa-file-passphrase-alist))
		 (setcdr entry nil))
	     (signal 'file-error
		     (cons "Opening input file" (cdr error)))))
	  (make-local-variable 'epa-file-encrypt-to)
	  (setq epa-file-encrypt-to
		(mapcar #'car (epg-context-result-for context 'encrypted-to)))
	  (if (or beg end)
	      (setq string (substring string (or beg 0) end)))
	  (save-excursion
	    (save-restriction
	      (narrow-to-region (point) (point))
	      (epa-file-decode-and-insert string file visit beg end replace)
	      (setq length (- (point-max) (point-min))))
	    (if replace
		(delete-region (point) (point-max)))))
      (if (and local-copy
	       (file-exists-p local-copy))
	  (delete-file local-copy)))
    (list file length)))
(put 'insert-file-contents 'epa-file 'epa-file-insert-file-contents)

(defun epa-file-write-region (start end file &optional append visit lockname
				    mustbenew)
  (if append
      (error "Can't append to the file."))
  (setq file (expand-file-name file))
  (let* ((coding-system (or coding-system-for-write
			    (if (fboundp 'select-safe-coding-system)
				;; This is needed since Emacs 22 has
				;; no-conversion setting for *.gpg in
				;; `auto-coding-alist'.
			        (let ((buffer-file-name
				       (file-name-sans-extension file)))
				  (select-safe-coding-system
				   (point-min) (point-max)))
			      buffer-file-coding-system)))
	 (context (epg-make-context))
	 (coding-system-for-write 'binary)
	 string entry
	 (recipients
	  (cond
	   ((listp epa-file-encrypt-to) epa-file-encrypt-to)
	   ((stringp epa-file-encrypt-to) (list epa-file-encrypt-to)))))
    (epg-context-set-passphrase-callback
     context
     (cons #'epa-file-passphrase-callback-function
	   file))
    (epg-context-set-progress-callback context
				       #'epa-progress-callback-function)
    (epg-context-set-armor context epa-armor)
    (condition-case error
	(setq string
	      (epg-encrypt-string
	       context
	       (if (stringp start)
		   (epa-file--encode-coding-string start coding-system)
		 (epa-file--encode-coding-string (buffer-substring start end)
						 coding-system))
	       (if (or epa-file-select-keys
		       (not (local-variable-p 'epa-file-encrypt-to
					      (current-buffer))))
		   (epa-select-keys
		    context
		    "Select recipents for encryption.
If no one is selected, symmetric encryption will be performed.  "
		    recipients)
		 (if epa-file-encrypt-to
		     (epg-list-keys context recipients)))))
      (error
       (if (setq entry (assoc file epa-file-passphrase-alist))
	   (setcdr entry nil))
       (signal 'file-error (cons "Opening output file" (cdr error)))))
    (epa-file-run-real-handler
     #'write-region
     (list string nil file append visit lockname mustbenew))
    (if (boundp 'last-coding-system-used)
	(setq last-coding-system-used coding-system))
    (if (eq visit t)
	(progn
	  (setq buffer-file-name file)
	  (set-visited-file-modtime))
      (if (stringp visit)
	  (progn
	    (set-visited-file-modtime)
	    (setq buffer-file-name visit))))
    (if (or (eq visit t)
	    (eq visit nil)
	    (stringp visit))
	(message "Wrote %s" buffer-file-name))))
(put 'write-region 'epa-file 'epa-file-write-region)

(defun epa-file-find-file-hook ()
  (if (and buffer-file-name
	   (string-match epa-file-name-regexp buffer-file-name)
	   epa-file-inhibit-auto-save)
      (auto-save-mode 0))
  (set-buffer-modified-p nil))

(defun epa-file-select-keys ()
  "Select recipients for encryption."
  (interactive)
  (make-local-variable 'epa-file-encrypt-to)
  (setq epa-file-encrypt-to
	(mapcar
	 (lambda (key)
	   (epg-sub-key-id (car (epg-key-sub-key-list key))))
	(epa-select-keys
	 (epg-make-context)
	 "Select recipents for encryption.
If no one is selected, symmetric encryption will be performed.  "))))

;;;###autoload
(defun epa-file-enable ()
  (interactive)
  (if (memq epa-file-handler file-name-handler-alist)
      (message "`epa-file' already enabled")
    (setq file-name-handler-alist
	  (cons epa-file-handler file-name-handler-alist))
    (add-hook 'find-file-hooks 'epa-file-find-file-hook)
    (setq auto-mode-alist (cons epa-file-auto-mode-alist-entry auto-mode-alist))
    (message "`epa-file' enabled")))

;;;###autoload
(defun epa-file-disable ()
  (interactive)
  (if (memq epa-file-handler file-name-handler-alist)
      (progn
	(setq file-name-handler-alist
	      (delq epa-file-handler file-name-handler-alist))
	(remove-hook 'find-file-hooks 'epa-file-find-file-hook)
	(setq auto-mode-alist (delq epa-file-auto-mode-alist-entry
				    auto-mode-alist))
	(message "`epa-file' disabled"))
    (message "`epa-file' already disabled")))

;;;###autoload
(define-minor-mode epa-file-mode
  "Toggle automatic file encryption and decryption.
With prefix argument ARG, turn auto encryption on if positive, else off.
Return the new status of auto encryption (non-nil means on)."
  :global t :init-value nil :group 'epa-file :version "23.1"
  (setq file-name-handler-alist
	(delq epa-file-handler file-name-handler-alist))
  (remove-hook 'find-file-hooks 'epa-file-find-file-hook)
  (setq auto-mode-alist (delq epa-file-auto-mode-alist-entry
			      auto-mode-alist))
  (when epa-file-mode
    (setq file-name-handler-alist
	  (cons epa-file-handler file-name-handler-alist))
    (add-hook 'find-file-hooks 'epa-file-find-file-hook)
    (setq auto-mode-alist (cons epa-file-auto-mode-alist-entry
				auto-mode-alist))))

(provide 'epa-file)

;; arch-tag: 5715152f-0eb1-4dbc-9008-07098775314d
;;; epa-file.el ends here
