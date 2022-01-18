;;; epa-file.el --- the EasyPG Assistant, transparent file encryption -*- lexical-binding: t -*-

;; Copyright (C) 2006-2022 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: PGP, GnuPG
;; Package: epa

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

;;; Code:

(require 'epa)
(require 'epa-hook)
(eval-when-compile (require 'subr-x))

;;; Options

(defcustom epa-file-cache-passphrase-for-symmetric-encryption nil
  "If non-nil, cache passphrase for symmetric encryption.

For security reasons, this option is turned off by default and
not recommended to use.  Instead, consider using gpg-agent which
does the same job in a safer way.  See Info node `(epa) Caching
Passphrases' for more information.

Note that this option has no effect if you use GnuPG 2.0."
  :type 'boolean
  :group 'epa-file)

(defcustom epa-file-select-keys nil
  "Control whether or not to pop up the key selection dialog.

If t, always ask user to select recipients.
If nil, query user only when `epa-file-encrypt-to' is not set.
If neither t nor nil, don't ask user.  In this case, symmetric
encryption is used."
  :type '(choice (const :tag "Ask always" t)
		 (const :tag "Ask when recipients are not set" nil)
		 (const :tag "Don't ask" silent))
  :group 'epa-file)

;;; Other

(defvar epa-file-passphrase-alist nil)

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
		  (setq entry (list file))
		  (setq epa-file-passphrase-alist
			(cons entry
			      epa-file-passphrase-alist)))
		(setq passphrase (epa-passphrase-callback-function context
								   key-id
								   file))
		(setcdr entry (copy-sequence passphrase))
		passphrase))))
    (epa-passphrase-callback-function context key-id file)))

;;; File Handler

(defvar epa-inhibit nil
  "Non-nil means don't try to decrypt .gpg files when operating on them.")

;;;###autoload
(defun epa-file-handler (operation &rest args)
  (save-match-data
    (let ((op (get operation 'epa-file)))
      (if (and op (not epa-inhibit))
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
  (save-restriction
    (narrow-to-region (point) (point))
    (insert string)
    (decode-coding-inserted-region
     (point-min) (point-max)
     (substring file 0 (string-match epa-file-name-regexp file))
     visit beg end replace)
    (goto-char (point-max))
    (- (point-max) (point-min))))

(defvar epa-file-error nil)
(defun epa-file--find-file-not-found-function ()
  (let ((error epa-file-error))
    (save-window-excursion
      (kill-buffer))
    (if (nth 3 error)
        (user-error "Wrong passphrase: %s" (nth 3 error))
      (signal 'file-missing
	      (cons "Opening input file" (cdr error))))))

(defun epa--wrong-password-p (context)
  (let ((error-string (epg-context-error-output context)))
    (and (string-match
          "decryption failed: \\(Bad session key\\|No secret key\\)"
          error-string)
         (match-string 1 error-string))))

(defvar last-coding-system-used)
(defun epa-file-insert-file-contents (file &optional visit beg end replace)
  (barf-if-buffer-read-only)
  (if (and visit (or beg end))
      (error "Attempt to visit less than an entire file"))
  (setq file (expand-file-name file))
  (let* ((local-copy
	  (condition-case nil
	      (epa-file-run-real-handler #'file-local-copy (list file))
	    (error)))
	 (local-file (or local-copy file))
	 (context (epg-make-context))
         (buf (current-buffer))
	 string length entry)
    (if visit
	(setq buffer-file-name file))
    (epg-context-set-passphrase-callback
     context
     (cons #'epa-file-passphrase-callback-function
	   local-file))
    (epg-context-set-progress-callback
     context
     (cons #'epa-progress-callback-function
	   (format "Decrypting %s" file)))
    (unwind-protect
	(progn
	  (condition-case error
	      (setq string (epg-decrypt-file context local-file nil))
	    (error
	     (if (setq entry (assoc file epa-file-passphrase-alist))
		 (setcdr entry nil))
	     ;; If the decryption program can't be found,
	     ;; signal that as a non-file error
	     ;; so that find-file-noselect-1 won't handle it.
	     ;; Borrowed from jka-compr.el.
	     (if (and (memq 'file-error (get (car error) 'error-conditions))
		      (equal (cadr error) "Searching for program"))
		 (error "Decryption program `%s' not found"
			(nth 3 error)))
	     (let ((exists (file-exists-p local-file)))
	       (when exists
                 (if-let ((wrong-password (epa--wrong-password-p context)))
                     ;; Don't display the *error* buffer if we just
                     ;; have a wrong password; let the later error
                     ;; handler notify the user.
                     (setq error (append error (list wrong-password)))
		   (epa-display-error context))
                 ;; When the .gpg file isn't an encrypted file (e.g.,
                 ;; it's a keyring.gpg file instead), then gpg will
                 ;; say "Unexpected exit" as the error message.  In
                 ;; that case, just display the bytes.
                 (if (equal (caddr error) "Unexpected; Exit")
                     (setq string (with-temp-buffer
                                    (insert-file-contents-literally local-file)
                                    (buffer-string)))
		   ;; Hack to prevent find-file from opening empty buffer
		   ;; when decryption failed (bug#6568).  See the place
		   ;; where `find-file-not-found-functions' are called in
		   ;; `find-file-noselect-1'.
		   (setq-local epa-file-error error)
		   (add-hook 'find-file-not-found-functions
			     'epa-file--find-file-not-found-function
			     nil t)))
	       (signal (if exists 'file-error 'file-missing)
		       (cons "Opening input file" (cdr error))))))
          (set-buffer buf) ;In case timer/filter changed/killed it (bug#16029)!
	  (setq-local epa-file-encrypt-to
                      (mapcar #'car (epg-context-result-for
                                     context 'encrypted-to)))
	  (if (or beg end)
              (setq string (substring string
                                      (or beg 0)
                                      (and end (min end (length string))))))
	  (save-excursion
	    ;; If visiting, bind off buffer-file-name so that
	    ;; file-locking will not ask whether we should
	    ;; really edit the buffer.
	    (let ((buffer-file-name
		   (if visit nil buffer-file-name)))
              (setq length
                    (if replace
                        (epa-file--replace-text string file visit beg end)
		      (epa-file-decode-and-insert
                       string file visit beg end replace))))
	    (if visit
		(set-visited-file-modtime))))
      (if (and local-copy
	       (file-exists-p local-copy))
	  (delete-file local-copy)))
    (list file length)))
(put 'insert-file-contents 'epa-file 'epa-file-insert-file-contents)

(defun epa-file--replace-text (string file visit beg end)
  ;; The idea here is that we want to replace the text in the buffer
  ;; (for instance, for a `revert-buffer'), but we want to touch as
  ;; little of the text as possible.  So we compare the new and the
  ;; old text and only starts replacing when the text changes.
  (let ((orig-point (point))
        new-start length)
    (goto-char (point-max))
    (setq new-start (point))
    (setq length
	  (epa-file-decode-and-insert
           string file visit beg end t))
    (if (equal (buffer-substring (point-min) new-start)
               (buffer-substring new-start (point-max)))
        ;; The new text is equal to the old, so just keep the old.
        (delete-region new-start (point-max))
      ;; Compute the region the hard way.
      (let ((p1 (point-min))
            (p2 new-start))
        (while (and (< p1 new-start)
                    (< p2 (point-max))
                    (eql (char-after p1) (char-after p2)))
          (cl-incf p1)
          (cl-incf p2))
        (delete-region new-start p2)
        (delete-region p1 new-start)))
    ;; Restore point, if possible.
    (if (< orig-point (point-max))
        (goto-char orig-point)
      (goto-char (point-max)))
    length))

(defun epa-file-write-region (start end file &optional append visit lockname
				    mustbenew)
  (if append
      (error "Can't append to the file"))
  (setq file (expand-file-name file))
  (let* ((coding-system (or coding-system-for-write
			    (if (fboundp 'select-safe-coding-system)
			        (let ((buffer-file-name file))
				  (select-safe-coding-system
				   (point-min) (point-max)))
			      buffer-file-coding-system)))
	 (context (epg-make-context))
	 (coding-system-for-write 'binary)
	 string entry
	 (recipients
	  (cond
	   ((listp epa-file-encrypt-to) epa-file-encrypt-to)
	   ((stringp epa-file-encrypt-to) (list epa-file-encrypt-to))))
	 buffer)
    (epg-context-set-passphrase-callback
     context
     (cons #'epa-file-passphrase-callback-function
	   file))
    (epg-context-set-progress-callback
     context
     (cons #'epa-progress-callback-function
	   (format "Encrypting %s" file)))
    (setf (epg-context-armor context) epa-armor)
    (condition-case error
	(setq string
	      (epg-encrypt-string
	       context
	       (if (stringp start)
		   (encode-coding-string start coding-system)
		 (unless start
		   (setq start (point-min)
			 end (point-max)))
		 (setq buffer (current-buffer))
		 (with-temp-buffer
		   (insert-buffer-substring buffer start end)
		   ;; Translate the region according to
		   ;; `buffer-file-format', as `write-region' would.
		   ;; We can't simply do `write-region' (into a
		   ;; temporary file) here, since it writes out
		   ;; decrypted contents.
		   (format-encode-buffer (with-current-buffer buffer
					   buffer-file-format))
		   (encode-coding-string (buffer-string)
					 coding-system)))
	       (if (or (eq epa-file-select-keys t)
		       (and (null epa-file-select-keys)
			    (not (local-variable-p 'epa-file-encrypt-to
						   (current-buffer)))))
		   (epa-select-keys
		    context
		    "Select recipients for encryption.
If no one is selected, symmetric encryption will be performed.  "
		    recipients)
		 (if epa-file-encrypt-to
                     (epg--filter-revoked-keys
		      (epg-list-keys context recipients))))))
      (error
       (epa-display-error context)
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

;;; Commands

(defun epa-file-select-keys ()
  "Select recipients for encryption."
  (interactive)
  (setq-local epa-file-encrypt-to
              (mapcar
               (lambda (key)
                 (epg-sub-key-id (car (epg-key-sub-key-list key))))
               (epa-select-keys
                (epg-make-context)
                "Select recipients for encryption.
If no one is selected, symmetric encryption will be performed.  "))))

;;;###autoload
(defun epa-file-enable ()
  (interactive)
  (if (memq epa-file-handler file-name-handler-alist)
      (message "`epa-file' already enabled")
    (setq file-name-handler-alist
	  (cons epa-file-handler file-name-handler-alist))
    (add-hook 'find-file-hook 'epa-file-find-file-hook)
    (setq auto-mode-alist (cons epa-file-auto-mode-alist-entry auto-mode-alist))
    (message "`epa-file' enabled")))

;;;###autoload
(defun epa-file-disable ()
  (interactive)
  (if (memq epa-file-handler file-name-handler-alist)
      (progn
	(setq file-name-handler-alist
	      (delq epa-file-handler file-name-handler-alist))
	(remove-hook 'find-file-hook 'epa-file-find-file-hook)
	(setq auto-mode-alist (delq epa-file-auto-mode-alist-entry
				    auto-mode-alist))
	(message "`epa-file' disabled"))
    (message "`epa-file' already disabled")))

(provide 'epa-file)

;;; epa-file.el ends here
