;;; org-crypt.el --- Public Key Encryption for Org Entries -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2007-2022 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

;; This file is part of GNU Emacs.
;;
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

;; Right now this is just a set of functions to play with.  It depends
;; on the epg library.  Here's how you would use it:
;;
;; 1. To mark an entry for encryption, tag the heading with "crypt".
;;    You can change the tag to any complex tag matching string by
;;    setting the `org-crypt-tag-matcher' variable.
;;
;; 2. Set the encryption key to use in the `org-crypt-key' variable,
;;    or use `M-x org-set-property' to set the property CRYPTKEY to
;;    any address in your public keyring.  The text of the entry (but
;;    not its properties or headline) will be encrypted for this user.
;;    For them to read it, the corresponding secret key must be
;;    located in the secret key ring of the account where you try to
;;    decrypt it.  This makes it possible to leave secure notes that
;;    only the intended recipient can read in a shared-org-mode-files
;;    scenario.
;;    If the key is not set, org-crypt will default to symmetric encryption.
;;
;; 3. To later decrypt an entry, use `org-decrypt-entries' or
;;    `org-decrypt-entry'.  It might be useful to bind this to a key,
;;    like C-c C-/.
;;
;; 4. To automatically encrypt all necessary entries when saving a
;;    file, call `org-crypt-use-before-save-magic' after loading
;;    org-crypt.el.

;;; Thanks:

;; - Carsten Dominik
;; - Vitaly Ostanin

;;; Code:

(require 'org-macs)
(require 'org-compat)

(declare-function epg-decrypt-string "epg" (context cipher))
(declare-function epg-list-keys "epg" (context &optional name mode))
(declare-function epg-make-context "epg"
		  (&optional protocol armor textmode include-certs
			     cipher-algorithm digest-algorithm
			     compress-algorithm))
(declare-function epg-encrypt-string "epg"
		  (context plain recipients &optional sign always-trust))
(defvar epg-context)

(declare-function org-back-over-empty-lines "org" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-before-first-heading-p "org" ())
(declare-function org-end-of-meta-data "org" (&optional full))
(declare-function org-end-of-subtree "org" (&optional invisible-ok to-heading))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-flag-subtree "org" (flag))
(declare-function org-make-tags-matcher "org" (match))
(declare-function org-previous-visible-heading "org" (arg))
(declare-function org-scan-tags "org" (action matcher todo-only &optional start-level))
(declare-function org-set-property "org" (property value))

(defgroup org-crypt nil
  "Org Crypt."
  :tag "Org Crypt"
  :group 'org)

(defcustom org-crypt-tag-matcher "crypt"
  "The tag matcher used to find headings whose contents should be encrypted.

See the \"Match syntax\" section of the org manual for more details."
  :type 'string
  :group 'org-crypt)

(defcustom org-crypt-key ""
  "The default key to use when encrypting the contents of a heading.

If this variable is nil, always use symmetric encryption, unconditionally.

Otherwise, The string is matched against all keys in the key ring.
In particular, the empty string matches no key.  If no key is found,
look for the `epa-file-encrypt-to' local variable.  Ultimately fall back
to symmetric encryption.

This setting can be overridden in the CRYPTKEY property."
  :group 'org-crypt
  :type '(choice
	  (string :tag "Public key(s) matching")
	  (const :tag "Symmetric encryption" nil)))

(defcustom org-crypt-disable-auto-save 'ask
  "What org-decrypt should do if `auto-save-mode' is enabled.

t        : Disable auto-save-mode for the current buffer
           prior to decrypting an entry.

nil      : Leave auto-save-mode enabled.
           This may cause data to be written to disk unencrypted!

`ask'    : Ask user whether or not to disable auto-save-mode
           for the current buffer.

`encrypt': Leave auto-save-mode enabled for the current buffer,
           but automatically re-encrypt all decrypted entries
           *before* auto-saving.
           NOTE: This only works for entries which have a tag
           that matches `org-crypt-tag-matcher'."
  :group 'org-crypt
  :version "24.1"
  :type '(choice (const :tag "Always"  t)
                 (const :tag "Never"   nil)
                 (const :tag "Ask"     ask)
                 (const :tag "Encrypt" encrypt)))

(defun org-crypt--encrypted-text (beg end)
  "Return encrypted text in between BEG and END."
  ;; Ignore indentation.
  (replace-regexp-in-string
   "^[ \t]*" ""
   (buffer-substring-no-properties beg end)))

(defun org-at-encrypted-entry-p ()
  "Is the current entry encrypted?
When the entry is encrypted, return a pair (BEG . END) where BEG
and END are buffer positions delimiting the encrypted area."
  (org-with-wide-buffer
   (unless (org-before-first-heading-p)
     (org-back-to-heading t)
     (org-end-of-meta-data 'standard)
     (let ((case-fold-search nil)
	   (banner-start (rx (seq bol
				  (zero-or-more (any "\t "))
				  "-----BEGIN PGP MESSAGE-----"
				  eol))))
       (when (looking-at banner-start)
	 (let ((start (point))
	       (banner-end (rx (seq bol
				    (or (group (zero-or-more (any "\t "))
					       "-----END PGP MESSAGE-----"
					       eol)
					(seq (one-or-more "*") " "))))))
	   (when (and (re-search-forward banner-end nil t) (match-string 1))
	     (cons start (line-beginning-position 2)))))))))

(defun org-crypt-check-auto-save ()
  "Check whether auto-save-mode is enabled for the current buffer.

`auto-save-mode' may cause leakage when decrypting entries, so
check whether it's enabled, and decide what to do about it.

See `org-crypt-disable-auto-save'."
  (when buffer-auto-save-file-name
    (cond
     ((or
       (eq org-crypt-disable-auto-save t)
       (and
	(eq org-crypt-disable-auto-save 'ask)
	(y-or-n-p "org-decrypt: auto-save-mode may cause leakage.  Disable it for current buffer? ")))
      (message "org-decrypt: Disabling auto-save-mode for %s"
               (or (buffer-file-name) (current-buffer)))
      ;; The argument to auto-save-mode has to be "-1", since
      ;; giving a "nil" argument toggles instead of disabling.
      (auto-save-mode -1))
     ((eq org-crypt-disable-auto-save nil)
      (message "org-decrypt: Decrypting entry with auto-save-mode enabled.  This may cause leakage."))
     ((eq org-crypt-disable-auto-save 'encrypt)
      (message "org-decrypt: Enabling re-encryption on auto-save.")
      (add-hook 'auto-save-hook
		(lambda ()
		  (message "org-crypt: Re-encrypting all decrypted entries due to auto-save.")
		  (org-encrypt-entries))
		nil t))
     (t nil))))

(defun org-crypt-key-for-heading ()
  "Return the encryption key(s) for the current heading.
Assume `epg-context' is set."
  (and org-crypt-key
       (or (epg-list-keys epg-context
			  (or (org-entry-get nil "CRYPTKEY" 'selective)
			      org-crypt-key))
	   (bound-and-true-p epa-file-encrypt-to)
	   (progn
	     (message "No crypt key set, using symmetric encryption.")
	     nil))))

;;;###autoload
(defun org-encrypt-entry ()
  "Encrypt the content of the current headline."
  (interactive)
  (unless (org-at-encrypted-entry-p)
    (require 'epg)
    (setq-local epg-context (epg-make-context nil t t))
    (org-with-wide-buffer
     (org-back-to-heading t)
     (let ((start-heading (point))
	   (crypt-key (org-crypt-key-for-heading))
	   (folded? (org-invisible-p (line-beginning-position))))
       (org-end-of-meta-data 'standard)
       (let ((beg (point))
	     (folded-heading
	      (and folded?
		   (save-excursion
		     (org-previous-visible-heading 1)
		     (point)))))
	 (goto-char start-heading)
	 (org-end-of-subtree t t)
	 (org-back-over-empty-lines)
	 (let* ((contents (delete-and-extract-region beg (point)))
		(key (get-text-property 0 'org-crypt-key contents))
		(checksum (get-text-property 0 'org-crypt-checksum contents)))
	   (condition-case err
	       (insert
		;; Text and key have to be identical, otherwise we
		;; re-crypt.
		(if (and (equal crypt-key key)
			 (string= checksum (sha1 contents)))
		    (get-text-property 0 'org-crypt-text contents)
		  (epg-encrypt-string epg-context contents crypt-key)))
	     ;; If encryption failed, make sure to insert back entry
	     ;; contents in the buffer.
	     (error
	      (insert contents)
	      (error (error-message-string err)))))
	 (when folded-heading
	   (goto-char folded-heading)
	   (org-flag-subtree t))
	 nil)))))

;;;###autoload
(defun org-decrypt-entry ()
  "Decrypt the content of the current headline."
  (interactive)
  (pcase (org-at-encrypted-entry-p)
    (`(,beg . ,end)
     (require 'epg)
     (setq-local epg-context (epg-make-context nil t t))
     (org-with-point-at beg
       (org-crypt-check-auto-save)
       (let* ((folded-heading
	       (and (org-invisible-p)
		    (save-excursion
		      (org-previous-visible-heading 1)
		      (point))))
	      (encrypted-text (org-crypt--encrypted-text beg end))
	      (decrypted-text
	       (decode-coding-string
		(epg-decrypt-string epg-context encrypted-text)
		'utf-8)))
	 ;; Delete region starting just before point, because the
	 ;; outline property starts at the \n of the heading.
	 (delete-region (1- (point)) end)
	 ;; Store a checksum of the decrypted and the encrypted text
	 ;; value.  This allows reusing the same encrypted text if the
	 ;; text does not change, and therefore avoid a re-encryption
	 ;; process.
	 (insert "\n"
		 (propertize decrypted-text
			     'org-crypt-checksum (sha1 decrypted-text)
			     'org-crypt-key (org-crypt-key-for-heading)
			     'org-crypt-text encrypted-text))
	 (when folded-heading
	   (goto-char folded-heading)
	   (org-flag-subtree t))
	 nil)))
    (_ nil)))

(defvar org--matcher-tags-todo-only)

;;;###autoload
(defun org-encrypt-entries ()
  "Encrypt all top-level entries in the current buffer."
  (interactive)
  (let ((org--matcher-tags-todo-only nil))
    (org-scan-tags
     'org-encrypt-entry
     (cdr (org-make-tags-matcher org-crypt-tag-matcher))
     org--matcher-tags-todo-only)))

;;;###autoload
(defun org-decrypt-entries ()
  "Decrypt all entries in the current buffer."
  (interactive)
  (let ((org--matcher-tags-todo-only nil))
    (org-scan-tags
     'org-decrypt-entry
     (cdr (org-make-tags-matcher org-crypt-tag-matcher))
     org--matcher-tags-todo-only)))

;;;###autoload
(defun org-crypt-use-before-save-magic ()
  "Add a hook to automatically encrypt entries before a file is saved to disk."
  (add-hook
   'org-mode-hook
   (lambda () (add-hook 'before-save-hook 'org-encrypt-entries nil t))))

(add-hook 'org-reveal-start-hook 'org-decrypt-entry)

(provide 'org-crypt)

;;; org-crypt.el ends here
