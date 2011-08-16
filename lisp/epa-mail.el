;;; epa-mail.el --- the EasyPG Assistant, minor-mode for mail composer -*- lexical-binding: t -*-
;; Copyright (C) 2006-2011 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: PGP, GnuPG, mail, message
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'epa)
(require 'mail-utils)

(defvar epa-mail-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\C-ed" 'epa-mail-decrypt)
    (define-key keymap "\C-c\C-ev" 'epa-mail-verify)
    (define-key keymap "\C-c\C-es" 'epa-mail-sign)
    (define-key keymap "\C-c\C-ee" 'epa-mail-encrypt)
    (define-key keymap "\C-c\C-ei" 'epa-mail-import-keys)
    (define-key keymap "\C-c\C-eo" 'epa-insert-keys)
    (define-key keymap "\C-c\C-e\C-d" 'epa-mail-decrypt)
    (define-key keymap "\C-c\C-e\C-v" 'epa-mail-verify)
    (define-key keymap "\C-c\C-e\C-s" 'epa-mail-sign)
    (define-key keymap "\C-c\C-e\C-e" 'epa-mail-encrypt)
    (define-key keymap "\C-c\C-e\C-i" 'epa-mail-import-keys)
    (define-key keymap "\C-c\C-e\C-o" 'epa-insert-keys)
    keymap))

(defvar epa-mail-mode-hook nil)
(defvar epa-mail-mode-on-hook nil)
(defvar epa-mail-mode-off-hook nil)

;;;###autoload
(define-minor-mode epa-mail-mode
  "A minor-mode for composing encrypted/clearsigned mails."
  nil " epa-mail" epa-mail-mode-map)

;;; ??? Could someone please clarify this doc string?
;;; In particular, what does USAGE look like
;;; and what does it mean?  -- rms
(defun epa-mail--find-usable-key (keys usage)
  "Find a usable key from KEYS for USAGE."
  (catch 'found
    (while keys
      (let ((pointer (epg-key-sub-key-list (car keys))))
	(while pointer
	  (if (and (memq usage (epg-sub-key-capability (car pointer)))
		   (not (memq (epg-sub-key-validity (car pointer))
			      '(revoked expired))))
	      (throw 'found (car keys)))
	  (setq pointer (cdr pointer))))
      (setq keys (cdr keys)))))

(defvar epa-mail-group-alist nil
  "Alist of GnuPG mail groups (`group' commands in `.gnupg/gpg.conf').
Each element has the form (GROUPNAME ADDRESSES...).
t means the list is not yet read in.")

(defvar epa-mail-group-modtime nil
  "The modification time of `~/.gnupg/gpg.conf' file when last examined.")

(defvar epa-mail-gnupg-conf-file "~/.gnupg/gpg.conf"
  "File name of GnuPG configuration file that specifies recipient groups.")

(defun epa-mail-parse-groups ()
  "Parse `~/.gnupg/gpg.conf' and set `epa-mail-group-alist' from it."
  (let (aliases)
    (with-temp-buffer
      (insert-file-contents-literally epa-mail-gnupg-conf-file)

      (while (re-search-forward "^[ \t]*group[ \t]*" nil t)
	(if (looking-at "\\([^= \t]+\\)[ \t]*=[ \t]*\\([^ \t\n]+\\)")
	    (push (cons (match-string-no-properties 1)
			(split-string (match-string-no-properties 2)))
		  aliases))))
    (setq epa-mail-group-alist aliases)))

(defun epa-mail-sync-groups ()
  "Update GnuPG groups from file if necessary."
  (if (file-exists-p epa-mail-gnupg-conf-file)
      (let ((modtime (nth 5 (file-attributes epa-mail-gnupg-conf-file))))
	(if (not (equal epa-mail-group-modtime modtime))
	    (progn
	      (setq epa-mail-group-modtime modtime)
	      (epa-mail-parse-groups))))
    (setq epa-mail-group-alist nil)))

(defun epa-mail-expand-recipient-1 (recipient)
  "Expand RECIPIENT once thru `epa-mail-group-alist'.
Returns the list of names it stands for, or nil if it isn't a group."
  ;; Load the alias list if not loaded before.
  (let (alist-elt)
    (setq alist-elt (assoc recipient epa-mail-group-alist))
    (cdr alist-elt)))

(defun epa-mail-expand-recipients-2 (recipients)
  "Expand list RECIPIENTS once thru `epa-mail-group-alist'.
Returns the list of names they stand for."
  ;; Load the alias list if not loaded before.
  (let (output)
    (dolist (r recipients)
      (let ((expanded (epa-mail-expand-recipient-1 r)))
	(if expanded
	    (dolist (xr expanded)
	      (unless (member xr output)
		(push xr output)))
	  (unless (member r output)
	    (push r output)))))
    (nreverse output)))

(defun epa-mail-expand-recipients (recipients)
  "Expand RECIPIENTS thru `epa-mail-group-alist' until it stops changing."
  (epa-mail-sync-groups)
  (while (not (equal recipients
		     (setq recipients
			   (epa-mail-expand-recipients-2 recipients)))))
  recipients)

;;;###autoload
(defun epa-mail-decrypt ()
  "Decrypt OpenPGP armors in the current buffer.
The buffer is expected to contain a mail message.

Don't use this command in Lisp programs!"
  (interactive)
  (epa-decrypt-armor-in-region (point-min) (point-max)))

;;;###autoload
(defun epa-mail-verify ()
  "Verify OpenPGP cleartext signed messages in the current buffer.
The buffer is expected to contain a mail message.

Don't use this command in Lisp programs!"
  (interactive)
  (epa-verify-cleartext-in-region (point-min) (point-max)))

;;;###autoload
(defun epa-mail-sign (start end signers mode)
  "Sign the current buffer.
The buffer is expected to contain a mail message.

Don't use this command in Lisp programs!"
  (interactive
   (save-excursion
     (goto-char (point-min))
     (if (search-forward mail-header-separator nil t)
	 (forward-line))
     (setq epa-last-coding-system-specified
	   (or coding-system-for-write
	       (epa--select-safe-coding-system (point) (point-max))))
     (let ((verbose current-prefix-arg))
       (list (point) (point-max)
	     (if verbose
		 (epa-select-keys (epg-make-context epa-protocol)
				  "Select keys for signing.
If no one is selected, default secret key is used.  "
				  nil t))
	     (if verbose
		 (epa--read-signature-type)
	       'clear)))))
  (epa-sign-region start end signers mode))

;;;###autoload
(defun epa-mail-encrypt (start end recipients sign signers)
  "Encrypt the current buffer.
The buffer is expected to contain a mail message.

Don't use this command in Lisp programs!"
  (interactive
   (save-excursion
     (let ((verbose current-prefix-arg)
	   (context (epg-make-context epa-protocol))
	   recipients-string recipients recipient-key sign)
       (goto-char (point-min))
       (save-restriction
	 (narrow-to-region (point)
			   (if (search-forward mail-header-separator nil 0)
			       (match-beginning 0)
			     (point)))
	 (setq recipients-string
	       (mapconcat #'identity
			  (nconc (mail-fetch-field "to" nil nil t)
				 (mail-fetch-field "cc" nil nil t)
				 (mail-fetch-field "bcc" nil nil t))
			  ","))
	 (setq recipients
	       (mail-strip-quoted-names
		(with-temp-buffer
		  (insert "to: " recipients-string "\n")
		  (expand-mail-aliases (point-min) (point-max))
		  (car (mail-fetch-field "to" nil nil t))))))
       (if recipients
	   (setq recipients (delete ""
				    (split-string recipients
						  "[ \t\n]*,[ \t\n]*"))))

       ;; Process all the recipients thru the list of GnuPG groups.
       ;; Expand GnuPG group names to what they stand for.
       ;; The code below, and elsewhere, that checks that names have keys
       ;; does not know about these group names.
       (setq recipients (epa-mail-expand-recipients recipients))

       (goto-char (point-min))
       (if (search-forward mail-header-separator nil t)
	   (forward-line))
       (setq epa-last-coding-system-specified
	     (or coding-system-for-write
		 (epa--select-safe-coding-system (point) (point-max))))
       (list (point) (point-max)
	     (if verbose
		 (epa-select-keys
		  context
		  "Select recipients for encryption.
If no one is selected, symmetric encryption will be performed.  "
		  recipients)
	       (if recipients
		   (mapcar
		    (lambda (recipient)
		      (setq recipient-key
			    (epa-mail--find-usable-key
			     (epg-list-keys
			      (epg-make-context epa-protocol)
			      (if (string-match "@" recipient)
				  (concat "<" recipient ">")
				recipient))
			     'encrypt))
		      (unless (or recipient-key
				  (y-or-n-p
				   (format
				    "No public key for %s; skip it? "
				    recipient)))
			(error "No public key for %s" recipient))
		      recipient-key)
		    recipients)))
	     (setq sign (if verbose (y-or-n-p "Sign? ")))
	     (if sign
		 (epa-select-keys context
				  "Select keys for signing.  "))))))
  (epa-encrypt-region start end recipients sign signers))

;;;###autoload
(defun epa-mail-import-keys ()
  "Import keys in the OpenPGP armor format in the current buffer.
The buffer is expected to contain a mail message.

Don't use this command in Lisp programs!"
  (interactive)
  (epa-import-armor-in-region (point-min) (point-max)))

;;;###autoload
(define-minor-mode epa-global-mail-mode
  "Minor mode to hook EasyPG into Mail mode."
  :global t :init-value nil :group 'epa-mail :version "23.1"
  (remove-hook 'mail-mode-hook 'epa-mail-mode)
  (if epa-global-mail-mode
      (add-hook 'mail-mode-hook 'epa-mail-mode)))

(provide 'epa-mail)

;;; epa-mail.el ends here
