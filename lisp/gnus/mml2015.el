;;; mml2015.el --- MIME Security with Pretty Good Privacy (PGP)

;; Copyright (C) 2000-2020 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: PGP MIME MML

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

;; RFC 2015 is updated by RFC 3156, this file should be compatible
;; with both.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mm-decode)
(require 'mm-util)
(require 'mml)
(require 'mml-sec)
(require 'epg-config)

(defvar mc-pgp-always-sign)

(declare-function epg-check-configuration "ext:epg-config"
                  (config &optional minimum-version))
(declare-function epg-configuration "ext:epg-config" ())

;; Maybe this should be in eg mml-sec.el (and have a different name).
;; Then mml1991 would not need to require mml2015, and mml1991-use
;; could be removed.
(defvar mml2015-use 'epg
  "The package used for PGP/MIME.
Valid packages include `epg', `pgg' and `mailcrypt'.")

;; Something is not RFC2015.
(defvar mml2015-function-alist
  '((mailcrypt mml2015-mailcrypt-sign
	       mml2015-mailcrypt-encrypt
	       mml2015-mailcrypt-verify
	       mml2015-mailcrypt-decrypt
	       mml2015-mailcrypt-clear-verify
	       mml2015-mailcrypt-clear-decrypt)
    (pgg mml2015-pgg-sign
	 mml2015-pgg-encrypt
	 mml2015-pgg-verify
	 mml2015-pgg-decrypt
	 mml2015-pgg-clear-verify
	 mml2015-pgg-clear-decrypt)
    (epg mml2015-epg-sign
	 mml2015-epg-encrypt
	 mml2015-epg-verify
	 mml2015-epg-decrypt
	 mml2015-epg-clear-verify
	 mml2015-epg-clear-decrypt))
  "Alist of PGP/MIME functions.")

(defvar mml2015-result-buffer nil)

(defcustom mml2015-unabbrev-trust-alist
  '(("TRUST_UNDEFINED" . nil)
    ("TRUST_NEVER"     . nil)
    ("TRUST_MARGINAL"  . t)
    ("TRUST_FULLY"     . t)
    ("TRUST_ULTIMATE"  . t))
  "Map GnuPG trust output values to a boolean saying if you trust the key."
  :version "22.1"
  :group 'mime-security
  :type '(repeat (cons (regexp :tag "GnuPG output regexp")
		       (boolean :tag "Trust key"))))

(defcustom mml2015-cache-passphrase mml-secure-cache-passphrase
  "If t, cache passphrase."
  :group 'mime-security
  :type 'boolean)
(make-obsolete-variable 'mml2015-cache-passphrase
			'mml-secure-cache-passphrase
			"25.1")

(defcustom mml2015-passphrase-cache-expiry mml-secure-passphrase-cache-expiry
  "How many seconds the passphrase is cached.
Whether the passphrase is cached at all is controlled by
`mml2015-cache-passphrase'."
  :group 'mime-security
  :type 'integer)
(make-obsolete-variable 'mml2015-passphrase-cache-expiry
			'mml-secure-passphrase-cache-expiry
			"25.1")

(defcustom mml2015-signers nil
  "A list of your own key ID(s) which will be used to sign a message.
If set, it overrides the setting of `mml2015-sign-with-sender'."
  :group 'mime-security
  :type '(repeat (string :tag "Key ID")))

(defcustom mml2015-sign-with-sender nil
  "If t, use message sender so find a key to sign with."
  :group 'mime-security
  :type 'boolean
  :version "24.1")

(defcustom mml2015-encrypt-to-self nil
  "If t, add your own key ID to recipient list when encryption."
  :group 'mime-security
  :type 'boolean)

(defcustom mml2015-always-trust t
  "If t, GnuPG skip key validation on encryption."
  :group 'mime-security
  :type 'boolean)

(defcustom mml2015-maximum-key-image-dimension 64
  "The maximum dimension (width or height) of any key images."
  :version "24.4"
  :group 'mime-security
  :type 'integer)

(defcustom mml2015-display-key-image t
  "If t, try to display key images."
  :version "24.5"
  :group 'mime-security
  :type 'boolean)

;; Extract plaintext from cleartext signature.  IMO, this kind of task
;; should be done by GnuPG rather than Elisp, but older PGP backends
;; (such as Mailcrypt, and PGG) discard the output from GnuPG.
(defun mml2015-extract-cleartext-signature ()
  ;; Daiki Ueno in
  ;; <54a15d860801080142l70b95d7dkac4bf51a86196011@mail.gmail.com>: ``I still
  ;; believe that the right way is to use the plaintext output from GnuPG as
  ;; it is, and mml2015-extract-cleartext-signature is just a kludge for
  ;; misdesigned libraries like PGG, which have no ability to do that.  So, I
  ;; think it should not have descriptive documentation.''
  ;;
  ;; This function doesn't handle NotDashEscaped correctly.  EasyPG handles it
  ;; correctly.
  ;; http://thread.gmane.org/gmane.emacs.gnus.general/66062/focus=66082
  ;; http://thread.gmane.org/gmane.emacs.gnus.general/65087/focus=65109
  (goto-char (point-min))
  (forward-line)
  ;; We need to be careful not to strip beyond the armor headers.
  ;; Previously, an attacker could replace the text inside our
  ;; markup with trailing garbage by injecting whitespace into the
  ;; message.
  (while (looking-at "Hash:")		; The only header allowed in cleartext
    (forward-line))			; signatures according to RFC2440.
  (when (looking-at "[\t ]*$")
    (forward-line))
  (delete-region (point-min) (point))
  (if (re-search-forward "^-----BEGIN PGP SIGNATURE-----" nil t)
      (delete-region (match-beginning 0) (point-max)))
  (goto-char (point-min))
  (while (re-search-forward "^- " nil t)
    (replace-match "" t t)
    (forward-line 1)))

;;; mailcrypt wrapper

(autoload 'mailcrypt-decrypt "mailcrypt")
(autoload 'mailcrypt-verify "mailcrypt")
(autoload 'mc-pgp-always-sign "mailcrypt")
(autoload 'mc-encrypt-generic "mc-toplev")
(autoload 'mc-cleanup-recipient-headers "mc-toplev")
(autoload 'mc-sign-generic "mc-toplev")

(defvar mml2015-decrypt-function 'mailcrypt-decrypt)
(defvar mml2015-verify-function 'mailcrypt-verify)

(defun mml2015-format-error (err)
  (if (stringp (cadr err))
      (cadr err)
    (format "%S" (cdr err))))

(defun mml2015-mailcrypt-decrypt (handle ctl)
  (catch 'error
    (let (child handles result)
      (unless (setq child (mm-find-part-by-type
			   (cdr handle)
			   "application/octet-stream" nil t))
	(mm-sec-error 'gnus-info "Corrupted")
	(throw 'error handle))
      (with-temp-buffer
	(mm-insert-part child)
	(setq result
	      (condition-case err
		  (funcall mml2015-decrypt-function)
		(error
		 (mm-sec-error 'gnus-details (mml2015-format-error err))
		 nil)
		(quit
		 (mm-sec-error 'gnus-details "Quit.")
		 nil)))
	(unless (car result)
	  (mm-sec-error 'gnus-info "Failed")
	  (throw 'error handle))
	(setq handles (mm-dissect-buffer t)))
      (mm-destroy-parts handle)
      (mm-sec-status
       'gnus-info
       (concat "OK"
	       (let ((sig (with-current-buffer mml2015-result-buffer
			    (mml2015-gpg-extract-signature-details))))
		 (concat ", Signer: " sig))))
      (if (listp (car handles))
	  handles
	(list handles)))))

(defun mml2015-gpg-pretty-print-fpr (fingerprint)
  (let* ((result "")
	 (fpr-length (string-width fingerprint))
	 (n-slice 0)
	 slice)
    (setq fingerprint (string-to-list fingerprint))
    (while fingerprint
      (setq fpr-length (- fpr-length 4))
      (setq slice (butlast fingerprint fpr-length))
      (setq fingerprint (nthcdr 4 fingerprint))
      (setq n-slice (1+ n-slice))
      (setq result
	    (concat
	     result
	     (cl-case n-slice
	       (1  slice)
	       (otherwise (concat " " slice))))))
    result))

(defun mml2015-gpg-extract-signature-details ()
  (goto-char (point-min))
  (let* ((expired (re-search-forward
		   "^\\[GNUPG:\\] SIGEXPIRED$"
		   nil t))
	 (signer (and (re-search-forward
		       "^\\[GNUPG:\\] GOODSIG \\([0-9A-Za-z]*\\) \\(.*\\)$"
		       nil t)
		      (cons (match-string 1) (match-string 2))))
	 (fprint (and (re-search-forward
		       "^\\[GNUPG:\\] VALIDSIG \\([0-9a-zA-Z]*\\) "
		       nil t)
		      (match-string 1)))
	 (trust  (and (re-search-forward
		       "^\\[GNUPG:\\] \\(TRUST_.*\\)$"
		       nil t)
		      (match-string 1)))
	 (trust-good-enough-p
	  (cdr (assoc trust mml2015-unabbrev-trust-alist))))
    (cond ((and signer fprint)
	   (concat (cdr signer)
		   (unless trust-good-enough-p
		     (concat "\nUntrusted, Fingerprint: "
			     (mml2015-gpg-pretty-print-fpr fprint)))
		   (when expired
		     (format "\nWARNING: Signature from expired key (%s)"
			     (car signer)))))
	  ((re-search-forward
	    "^\\(gpg: \\)?Good signature from \"\\(.*\\)\"$" nil t)
	   (match-string 2))
	  (t
	   "From unknown user"))))

(defun mml2015-mailcrypt-clear-decrypt ()
  (let (result)
    (setq result
	  (condition-case err
	      (funcall mml2015-decrypt-function)
	    (error
	     (mm-sec-error 'gnus-details (mml2015-format-error err))
	     nil)
	    (quit
	     (mm-sec-error 'gnus-details "Quit.")
	     nil)))
    (if (car result)
	(mm-sec-status 'gnus-info "OK")
      (mm-sec-error 'gnus-info "Failed"))))

(defun mml2015-fix-micalg (alg)
  (and alg
       ;; Mutt/1.2.5i has seen sending micalg=php-sha1
       (upcase (if (string-match "^p[gh]p-" alg)
		   (substring alg (match-end 0))
		 alg))))

(defun mml2015-mailcrypt-verify (handle ctl)
  (catch 'error
    (let (part)
      (unless (setq part (mm-find-raw-part-by-type
			  ctl (or (mm-handle-multipart-ctl-parameter
				   ctl 'protocol)
				  "application/pgp-signature")
			  t))
	(mm-sec-error 'gnus-info "Corrupted")
	(throw 'error handle))
      (with-temp-buffer
	(insert "-----BEGIN PGP SIGNED MESSAGE-----\n")
	(insert (format "Hash: %s\n\n"
			(or (mml2015-fix-micalg
			     (mm-handle-multipart-ctl-parameter
			      ctl 'micalg))
			    "SHA1")))
	(save-restriction
	  (narrow-to-region (point) (point))
	  (insert part "\n")
	  (goto-char (point-min))
	  (while (not (eobp))
	    (if (looking-at "^-")
		(insert "- "))
	    (forward-line)))
	(unless (setq part (mm-find-part-by-type
			    (cdr handle) "application/pgp-signature" nil t))
	  (mm-sec-error 'gnus-info "Corrupted")
	  (throw 'error handle))
	(save-restriction
	  (narrow-to-region (point) (point))
	  (mm-insert-part part)
	  (goto-char (point-min))
	  (if (re-search-forward "^-----BEGIN PGP [^-]+-----\r?$" nil t)
	      (replace-match "-----BEGIN PGP SIGNATURE-----" t t))
	  (if (re-search-forward "^-----END PGP [^-]+-----\r?$" nil t)
	      (replace-match "-----END PGP SIGNATURE-----" t t)))
	(let ((mc-gpg-debug-buffer (get-buffer-create " *gnus gpg debug*")))
	  (unless (condition-case err
		      (prog1
			  (funcall mml2015-verify-function)
			(if (get-buffer " *mailcrypt stderr temp")
			    (mm-sec-error
			     'gnus-details
			     (with-current-buffer " *mailcrypt stderr temp"
			       (buffer-string))))
			(if (get-buffer " *mailcrypt stdout temp")
			    (kill-buffer " *mailcrypt stdout temp"))
			(if (get-buffer " *mailcrypt stderr temp")
			    (kill-buffer " *mailcrypt stderr temp"))
			(if (get-buffer " *mailcrypt status temp")
			    (kill-buffer " *mailcrypt status temp"))
			(if (get-buffer mc-gpg-debug-buffer)
			    (kill-buffer mc-gpg-debug-buffer)))
		    (error
		     (mm-sec-error 'gnus-details (mml2015-format-error err))
		     nil)
		    (quit
		     (mm-sec-error 'gnus-details "Quit.")
		     nil))
	    (mm-sec-error 'gnus-info "Failed")
	    (throw 'error handle))))
      (mm-sec-status 'gnus-info "OK")
      handle)))

(defun mml2015-mailcrypt-clear-verify ()
  (let ((mc-gpg-debug-buffer (get-buffer-create " *gnus gpg debug*")))
    (if (condition-case err
	    (prog1
		(funcall mml2015-verify-function)
	      (if (get-buffer " *mailcrypt stderr temp")
		  (mm-sec-error
		   'gnus-details
		   (with-current-buffer " *mailcrypt stderr temp"
		     (buffer-string))))
	      (if (get-buffer " *mailcrypt stdout temp")
		  (kill-buffer " *mailcrypt stdout temp"))
	      (if (get-buffer " *mailcrypt stderr temp")
		  (kill-buffer " *mailcrypt stderr temp"))
	      (if (get-buffer " *mailcrypt status temp")
		  (kill-buffer " *mailcrypt status temp"))
	      (if (get-buffer mc-gpg-debug-buffer)
		  (kill-buffer mc-gpg-debug-buffer)))
	  (error
	   (mm-sec-error 'gnus-details (mml2015-format-error err))
	   nil)
	  (quit
	   (mm-sec-error 'gnus-details "Quit.")
	   nil))
	(mm-sec-status 'gnus-info "OK")
      (mm-sec-error 'gnus-info "Failed")))
  (mml2015-extract-cleartext-signature))

(defun mml2015-mailcrypt-sign (cont)
  (mc-sign-generic (message-options-get 'message-sender)
		   nil nil nil nil)
  (let ((boundary (mml-compute-boundary cont))
	hash point)
    (goto-char (point-min))
    (unless (re-search-forward "^-----BEGIN PGP SIGNED MESSAGE-----\r?$" nil t)
      (error "Cannot find signed begin line"))
    (goto-char (match-beginning 0))
    (forward-line 1)
    (unless (looking-at "Hash:[ \t]*\\([a-zA-Z0-9]+\\)")
      (error "Cannot not find PGP hash"))
    (setq hash (match-string 1))
    (unless (re-search-forward "^$" nil t)
      (error "Cannot not find PGP message"))
    (forward-line 1)
    (delete-region (point-min) (point))
    (insert (format "Content-Type: multipart/signed; boundary=\"%s\";\n"
		    boundary))
    (insert (format "\tmicalg=pgp-%s; protocol=\"application/pgp-signature\"\n"
		    (downcase hash)))
    (insert (format "\n--%s\n" boundary))
    (setq point (point))
    (goto-char (point-max))
    (unless (re-search-backward "^-----END PGP SIGNATURE-----\r?$" nil t)
      (error "Cannot find signature part"))
    (replace-match "-----END PGP MESSAGE-----" t t)
    (goto-char (match-beginning 0))
    (unless (re-search-backward "^-----BEGIN PGP SIGNATURE-----\r?$"
				nil t)
      (error "Cannot find signature part"))
    (replace-match "-----BEGIN PGP MESSAGE-----" t t)
    (goto-char (match-beginning 0))
    (save-restriction
      (narrow-to-region point (point))
      (goto-char point)
      (while (re-search-forward "^- -" nil t)
	(replace-match "-" t t))
      (goto-char (point-max)))
    (insert (format "--%s\n" boundary))
    (insert "Content-Type: application/pgp-signature\n\n")
    (goto-char (point-max))
    (insert (format "--%s--\n" boundary))
    (goto-char (point-max))))

;; We require mm-decode, which requires mm-bodies, which autoloads
;; message-options-get (!).
(declare-function message-options-set "message" (symbol value))

(defun mml2015-mailcrypt-encrypt (cont &optional sign)
  (let ((mc-pgp-always-sign
	 (or mc-pgp-always-sign
	     sign
	     (eq t (or (message-options-get 'message-sign-encrypt)
		       (message-options-set
			'message-sign-encrypt
			(or (y-or-n-p "Sign the message? ")
			    'not))))
	     'never)))
    (insert
     (with-temp-buffer
       (set-buffer-multibyte nil)
       (mc-encrypt-generic
	(or (message-options-get 'message-recipients)
	    (message-options-set 'message-recipients
				 (mc-cleanup-recipient-headers
				  (read-string "Recipients: "))))
	nil nil nil
	(message-options-get 'message-sender))
       (buffer-string))))
  (goto-char (point-min))
  (unless (looking-at "-----BEGIN PGP MESSAGE-----")
    (error "Fail to encrypt the message"))
  (let ((boundary (mml-compute-boundary cont)))
    (insert (format "Content-Type: multipart/encrypted; boundary=\"%s\";\n"
		    boundary))
    (insert "\tprotocol=\"application/pgp-encrypted\"\n\n")
    (insert (format "--%s\n" boundary))
    (insert "Content-Type: application/pgp-encrypted\n\n")
    (insert "Version: 1\n\n")
    (insert (format "--%s\n" boundary))
    (insert "Content-Type: application/octet-stream\n\n")
    (goto-char (point-max))
    (insert (format "--%s--\n" boundary))
    (goto-char (point-max))))

;;; pgg wrapper

(defvar pgg-default-user-id)
(defvar pgg-errors-buffer)
(defvar pgg-output-buffer)

(autoload 'pgg-decrypt-region "pgg")
(autoload 'pgg-verify-region "pgg")
(autoload 'pgg-sign-region "pgg")
(autoload 'pgg-encrypt-region "pgg")
(autoload 'pgg-parse-armor "pgg-parse")

(defun mml2015-pgg-decrypt (handle ctl)
  (catch 'error
    (let ((pgg-errors-buffer mml2015-result-buffer)
	  child handles result decrypt-status)
      (unless (setq child (mm-find-part-by-type
			   (cdr handle)
			   "application/octet-stream" nil t))
	(mm-sec-error 'gnus-info "Corrupted")
	(throw 'error handle))
      (with-temp-buffer
	(mm-insert-part child)
	(if (condition-case err
		(prog1
		    (pgg-decrypt-region (point-min) (point-max))
		  (setq decrypt-status
			(with-current-buffer mml2015-result-buffer
			  (buffer-string)))
		  (mm-sec-status 'gnus-details decrypt-status))
	      (error
	       (mm-sec-error 'gnus-details (mml2015-format-error err))
	       nil)
	      (quit
	       (mm-sec-error 'gnus-details "Quit.")
	       nil))
	    (with-current-buffer pgg-output-buffer
	      (goto-char (point-min))
	      (while (search-forward "\r\n" nil t)
		(replace-match "\n" t t))
	      (setq handles (mm-dissect-buffer t))
	      (mm-destroy-parts handle)
	      (mm-sec-status 'gnus-info "OK"
			     'gnus-details
			     (concat decrypt-status
				     (when (stringp (car handles))
				       "\n" (mm-handle-multipart-ctl-parameter
					     handles 'gnus-details))))
	      (if (listp (car handles))
		  handles
		(list handles)))
	  (mm-sec-error 'gnus-info "Failed")
	  (throw 'error handle))))))

(defun mml2015-pgg-clear-decrypt ()
  (let ((pgg-errors-buffer mml2015-result-buffer))
    (if (prog1
	    (pgg-decrypt-region (point-min) (point-max))
	  (mm-sec-status
	   'gnus-details
	   (with-current-buffer mml2015-result-buffer
	     (buffer-string))))
	(progn
	  (erase-buffer)
	  ;; Treat data which pgg returns as a unibyte string.
	  (mm-disable-multibyte)
	  (insert-buffer-substring pgg-output-buffer)
	  (goto-char (point-min))
	  (while (search-forward "\r\n" nil t)
	    (replace-match "\n" t t))
	  (mm-sec-status 'gnus-info "OK"))
      (mm-sec-error 'gnus-info "Failed"))))

(defun mml2015-pgg-verify (handle ctl)
  (let ((pgg-errors-buffer mml2015-result-buffer)
	signature-file part signature)
    (if (or (null (setq part (mm-find-raw-part-by-type
			      ctl (or (mm-handle-multipart-ctl-parameter
				       ctl 'protocol)
				      "application/pgp-signature")
			      t)))
	    (null (setq signature
			(mm-find-part-by-type
			 (cdr handle) "application/pgp-signature" nil t))))
	(progn
	  (mm-sec-error 'gnus-info "Corrupted")
	  handle)
      (with-temp-buffer
	(insert part)
	;; Convert <LF> to <CR><LF> in signed text.  If --textmode is
	;; specified when signing, the conversion is not necessary.
	(goto-char (point-min))
	(end-of-line)
	(while (not (eobp))
	  (unless (eq (char-before) ?\r)
	    (insert "\r"))
	  (forward-line)
	  (end-of-line))
	(with-temp-file (setq signature-file (make-temp-file "pgg"))
	  (mm-insert-part signature))
	(if (condition-case err
		(prog1
		    (pgg-verify-region (point-min) (point-max)
				       signature-file t)
		  (goto-char (point-min))
		  (while (search-forward "\r\n" nil t)
		    (replace-match "\n" t t))
		  (mm-sec-status
		   'gnus-details
		   (concat (with-current-buffer pgg-output-buffer
			     (buffer-string))
			   (with-current-buffer pgg-errors-buffer
			     (buffer-string)))))
	      (error
	       (mm-sec-error 'gnus-details (mml2015-format-error err))
	       nil)
	      (quit
	       (mm-sec-error 'gnus-details "Quit.")
	       nil))
	    (progn
	      (delete-file signature-file)
	      (mm-sec-error
	       'gnus-info
	       (with-current-buffer pgg-errors-buffer
		 (mml2015-gpg-extract-signature-details))))
	  (delete-file signature-file)
	  (mm-sec-error 'gnus-info "Failed")))))
  handle)

(defun mml2015-pgg-clear-verify ()
  (let ((pgg-errors-buffer mml2015-result-buffer)
	(text (buffer-string))
	(coding-system buffer-file-coding-system))
    (if (condition-case err
	    (prog1
		(mm-with-unibyte-buffer
		  (insert (encode-coding-string text coding-system))
		  (pgg-verify-region (point-min) (point-max) nil t))
	      (goto-char (point-min))
	      (while (search-forward "\r\n" nil t)
		(replace-match "\n" t t))
	      (mm-sec-status
	       'gnus-details
	       (concat (with-current-buffer pgg-output-buffer
			 (buffer-string))
		       (with-current-buffer pgg-errors-buffer
			 (buffer-string)))))
	  (error
	   (mm-sec-error 'gnus-details (mml2015-format-error err))
	   nil)
	  (quit
	   (mm-sec-error 'gnus-details "Quit.")
	   nil))
	(mm-sec-status
	 'gnus-info
	 (with-current-buffer pgg-errors-buffer
	   (mml2015-gpg-extract-signature-details)))
      (mm-sec-error 'gnus-info "Failed")))
  (mml2015-extract-cleartext-signature))

(defun mml2015-pgg-sign (cont)
  (let ((pgg-errors-buffer mml2015-result-buffer)
	(boundary (mml-compute-boundary cont))
	(pgg-default-user-id (or (message-options-get 'mml-sender)
				 pgg-default-user-id))
	(pgg-text-mode t)
	entry)
    (unless (pgg-sign-region (point-min) (point-max))
      (pop-to-buffer mml2015-result-buffer)
      (error "Sign error"))
    (goto-char (point-min))
    (insert (format "Content-Type: multipart/signed; boundary=\"%s\";\n"
		    boundary))
    (if (setq entry (assq 2 (pgg-parse-armor
			     (with-current-buffer pgg-output-buffer
			       (buffer-string)))))
	(setq entry (assq 'hash-algorithm (cdr entry))))
    (insert (format "\tmicalg=%s; "
		    (if (cdr entry)
			(downcase (format "pgp-%s" (cdr entry)))
		      "pgp-sha1")))
    (insert "protocol=\"application/pgp-signature\"\n")
    (insert (format "\n--%s\n" boundary))
    (goto-char (point-max))
    (insert (format "\n--%s\n" boundary))
    (insert "Content-Type: application/pgp-signature\n\n")
    (insert-buffer-substring pgg-output-buffer)
    (goto-char (point-max))
    (insert (format "--%s--\n" boundary))
    (goto-char (point-max))))

(defun mml2015-pgg-encrypt (cont &optional sign)
  (let ((pgg-errors-buffer mml2015-result-buffer)
	(pgg-text-mode t)
	(boundary (mml-compute-boundary cont)))
    (unless (pgg-encrypt-region (point-min) (point-max)
				(split-string
				 (or
				  (message-options-get 'message-recipients)
				  (message-options-set 'message-recipients
						       (read-string "Recipients: ")))
				 "[ \f\t\n\r\v,]+")
				sign)
      (pop-to-buffer mml2015-result-buffer)
      (error "Encrypt error"))
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (insert (format "Content-Type: multipart/encrypted; boundary=\"%s\";\n"
		    boundary))
    (insert "\tprotocol=\"application/pgp-encrypted\"\n\n")
    (insert (format "--%s\n" boundary))
    (insert "Content-Type: application/pgp-encrypted\n\n")
    (insert "Version: 1\n\n")
    (insert (format "--%s\n" boundary))
    (insert "Content-Type: application/octet-stream\n\n")
    (insert-buffer-substring pgg-output-buffer)
    (goto-char (point-max))
    (insert (format "--%s--\n" boundary))
    (goto-char (point-max))))

;;; epg wrapper

(defvar epg-user-id-alist)
(defvar epg-digest-algorithm-alist)
(defvar epg-gpg-program)
(defvar inhibit-redisplay)

(autoload 'epg-make-context "epg")
(autoload 'epg-context-set-armor "epg")
(autoload 'epg-context-set-textmode "epg")
(autoload 'epg-context-set-signers "epg")
(autoload 'epg-context-result-for "epg")
(autoload 'epg-new-signature-digest-algorithm "epg")
(autoload 'epg-list-keys "epg")
(autoload 'epg-decrypt-string "epg")
(autoload 'epg-verify-string "epg")
(autoload 'epg-sign-string "epg")
(autoload 'epg-encrypt-string "epg")
(autoload 'epg-passphrase-callback-function "epg")
(autoload 'epg-context-set-passphrase-callback "epg")
(autoload 'epg-key-sub-key-list "epg")
(autoload 'epg-sub-key-capability "epg")
(autoload 'epg-sub-key-validity "epg")
(autoload 'epg-sub-key-fingerprint "epg")
(autoload 'epg-signature-key-id "epg")
(autoload 'epg-signature-to-string "epg")
(autoload 'epg-key-user-id-list "epg")
(autoload 'epg-user-id-string "epg")
(autoload 'epg-user-id-validity "epg")
(autoload 'epg-configuration "epg-config")
(autoload 'epg-expand-group "epg-config")
(autoload 'epa-select-keys "epa")

(defun mml2015-epg-key-image (key-id)
  "Return the image of a key, if any."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let* ((coding-system-for-write 'binary)
           (coding-system-for-read 'binary)
           (data (shell-command-to-string
                  (format "%s --list-options no-show-photos --attribute-fd 3 --list-keys %s 3>&1 >/dev/null 2>&1"
                          (shell-quote-argument epg-gpg-program) key-id))))
      (when (> (length data) 0)
        (insert (substring data 16))
	(condition-case nil
	    (gnus-create-image (buffer-string) nil t)
	  (error))))))

(autoload 'gnus-rescale-image "gnus-util")

(defun mml2015-epg-key-image-to-string (key-id)
  "Return a string with the image of a key, if any."
  (let ((key-image (mml2015-epg-key-image key-id)))
    (if (not key-image)
	""
      (condition-case error
	  (let ((result "  "))
	    (put-text-property
	     1 2 'display
	     (gnus-rescale-image key-image
				 (cons mml2015-maximum-key-image-dimension
				       mml2015-maximum-key-image-dimension))
	     result)
	    result)
	(error "")))))

(defun mml2015-epg-signature-to-string (signature)
  (concat (epg-signature-to-string signature)
          (when mml2015-display-key-image
            (mml2015-epg-key-image-to-string (epg-signature-key-id signature)))))

(defun mml2015-epg-verify-result-to-string (verify-result)
  (mapconcat #'mml2015-epg-signature-to-string verify-result "\n"))

(defun mml2015-epg-decrypt (handle ctl)
  (catch 'error
    (let ((inhibit-redisplay t)
	  context plain child handles result decrypt-status)
      (unless (setq child (mm-find-part-by-type
			   (cdr handle)
			   "application/octet-stream" nil t))
	(mm-sec-error 'gnus-info "Corrupted")
	(throw 'error handle))
      (setq context (epg-make-context))
      (if (or mml2015-cache-passphrase mml-secure-cache-passphrase)
	  (epg-context-set-passphrase-callback
	   context
	   (cons 'mml-secure-passphrase-callback 'OpenPGP)))
      (condition-case error
	  (setq plain (epg-decrypt-string context (mm-get-part child))
		mml-secure-secret-key-id-list nil)
	(error
	 (mml-secure-clear-secret-key-id-list)
	 (mm-sec-error 'gnus-info "Failed")
	 (if (eq (car error) 'quit)
	     (mm-sec-status 'gnus-details "Quit.")
	   (mm-sec-status 'gnus-details (mml2015-format-error error)))
	 (throw 'error handle)))
      (with-temp-buffer
	(insert plain)
	(goto-char (point-min))
	(while (search-forward "\r\n" nil t)
	  (replace-match "\n" t t))
	(setq handles (mm-dissect-buffer t))
	(mm-destroy-parts handle)
	(if (epg-context-result-for context 'verify)
	    (mm-sec-status
	     'gnus-info
	     (concat "OK\n"
		     (mml2015-epg-verify-result-to-string
		      (epg-context-result-for context 'verify))))
	  (mm-sec-status 'gnus-info "OK"))
	(if (stringp (car handles))
	    (mm-sec-status
	     'gnus-details
	     (mm-handle-multipart-ctl-parameter handles 'gnus-details))))
	(if (listp (car handles))
	    handles
	  (list handles)))))

(defun mml2015-epg-clear-decrypt ()
  (let ((inhibit-redisplay t)
	(context (epg-make-context))
	plain)
    (if (or mml2015-cache-passphrase mml-secure-cache-passphrase)
	(epg-context-set-passphrase-callback
	 context
	 (cons 'mml-secure-passphrase-callback 'OpenPGP)))
    (condition-case error
	(setq plain (epg-decrypt-string context (buffer-string))
	      mml-secure-secret-key-id-list nil)
      (error
       (mml-secure-clear-secret-key-id-list)
       (mm-sec-error 'gnus-info "Failed")
       (if (eq (car error) 'quit)
	   (mm-sec-status 'gnus-details "Quit.")
	 (mm-sec-status 'gnus-details (mml2015-format-error error)))))
    (when plain
      (erase-buffer)
      ;; Treat data which epg returns as a unibyte string.
      (mm-disable-multibyte)
      (insert plain)
      (goto-char (point-min))
      (while (search-forward "\r\n" nil t)
	(replace-match "\n" t t))
      (mm-sec-status 'gnus-info "OK")
      (if (epg-context-result-for context 'verify)
	  (mm-sec-status
	   'gnus-details
	   (mml2015-epg-verify-result-to-string
	    (epg-context-result-for context 'verify)))))))

(defun mml2015-epg-verify (handle ctl)
  (catch 'error
    (let ((inhibit-redisplay t)
	  context plain signature-file part signature)
      (when (or (null (setq part (mm-find-raw-part-by-type
				  ctl (or (mm-handle-multipart-ctl-parameter
					   ctl 'protocol)
					  "application/pgp-signature")
				  t)))
		(null (setq signature (mm-find-part-by-type
				       (cdr handle) "application/pgp-signature"
				       nil t))))
	(mm-sec-error 'gnus-info "Corrupted")
	(throw 'error handle))
      (setq part (replace-regexp-in-string "\n" "\r\n" part)
	    signature (mm-get-part signature)
	    context (epg-make-context))
      (condition-case error
	  (setq plain (epg-verify-string context signature part))
	(error
	 (mm-sec-error 'gnus-info "Failed")
	 (if (eq (car error) 'quit)
	     (mm-sec-status 'gnus-details "Quit.")
	   (mm-sec-status 'gnus-details (mml2015-format-error error)))
	 (throw 'error handle)))
      (mm-sec-status 'gnus-info
       (mml2015-epg-verify-result-to-string
	(epg-context-result-for context 'verify)))
      handle)))

(defun mml2015-epg-clear-verify ()
  (let ((inhibit-redisplay t)
	(context (epg-make-context))
	(signature (encode-coding-string (buffer-string)
					 coding-system-for-write))
	plain)
    (condition-case error
	(setq plain (epg-verify-string context signature))
      (error
       (mm-sec-error 'gnus-info "Failed")
       (if (eq (car error) 'quit)
	   (mm-sec-status 'gnus-details "Quit.")
	 (mm-sec-status 'gnus-details (mml2015-format-error error)))))
    (if plain
	(progn
	  (mm-sec-status
	   'gnus-info
	   (mml2015-epg-verify-result-to-string
	    (epg-context-result-for context 'verify)))
	  (delete-region (point-min) (point-max))
	  (insert (decode-coding-string plain coding-system-for-read)))
      (mml2015-extract-cleartext-signature))))

(defun mml2015-epg-sign (cont)
  (let ((inhibit-redisplay t)
	(boundary (mml-compute-boundary cont)))
    ;; Signed data must end with a newline (RFC 3156, 5).
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (let* ((pair (mml-secure-epg-sign 'OpenPGP t))
	   (signature (car pair))
	   (micalg (cdr pair)))
      (unless (stringp signature)
        (error "Signature failed"))
      (goto-char (point-min))
      (insert (format "Content-Type: multipart/signed; boundary=\"%s\";\n"
		      boundary))
      (if micalg
	  (insert (format "\tmicalg=pgp-%s; "
			  (downcase
			   (cdr (assq micalg
				      epg-digest-algorithm-alist))))))
      (insert "protocol=\"application/pgp-signature\"\n")
      (insert (format "\n--%s\n" boundary))
      (goto-char (point-max))
      (insert (format "\n--%s\n" boundary))
      (insert "Content-Type: application/pgp-signature; name=\"signature.asc\"\n\n")
      (insert signature)
      (goto-char (point-max))
      (insert (format "--%s--\n" boundary))
      (goto-char (point-max)))))

(defun mml2015-epg-encrypt (cont &optional sign)
  (let* ((inhibit-redisplay t)
	 (boundary (mml-compute-boundary cont))
	 (cipher (mml-secure-epg-encrypt 'OpenPGP cont sign)))
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (insert (format "Content-Type: multipart/encrypted; boundary=\"%s\";\n"
		    boundary))
    (insert "\tprotocol=\"application/pgp-encrypted\"\n\n")
    (insert (format "--%s\n" boundary))
    (insert "Content-Type: application/pgp-encrypted\n\n")
    (insert "Version: 1\n\n")
    (insert (format "--%s\n" boundary))
    (insert "Content-Type: application/octet-stream\n\n")
    (insert cipher)
    (goto-char (point-max))
    (insert (format "--%s--\n" boundary))
    (goto-char (point-max))))

;;; General wrapper

(autoload 'gnus-buffer-live-p "gnus-util")
(autoload 'gnus-get-buffer-create "gnus")

(defun mml2015-clean-buffer ()
  (if (gnus-buffer-live-p mml2015-result-buffer)
      (with-current-buffer mml2015-result-buffer
	(erase-buffer)
	t)
    (setq mml2015-result-buffer
	  (gnus-get-buffer-create " *MML2015 Result*"))
    nil))

(defsubst mml2015-clear-decrypt-function ()
  (nth 6 (assq mml2015-use mml2015-function-alist)))

(defsubst mml2015-clear-verify-function ()
  (nth 5 (assq mml2015-use mml2015-function-alist)))

;;;###autoload
(defun mml2015-decrypt (handle ctl)
  (mml2015-clean-buffer)
  (let ((func (nth 4 (assq mml2015-use mml2015-function-alist))))
    (if func
	(funcall func handle ctl)
      handle)))

;;;###autoload
(defun mml2015-decrypt-test (handle ctl)
  mml2015-use)

;;;###autoload
(defun mml2015-verify (handle ctl)
  (mml2015-clean-buffer)
  (let ((func (nth 3 (assq mml2015-use mml2015-function-alist))))
    (if func
	(funcall func handle ctl)
      handle)))

;;;###autoload
(defun mml2015-verify-test (handle ctl)
  mml2015-use)

;;;###autoload
(defun mml2015-encrypt (cont &optional sign)
  (mml2015-clean-buffer)
  (let ((func (nth 2 (assq mml2015-use mml2015-function-alist))))
    (if func
	(funcall func cont sign)
      (error "Cannot find encrypt function"))))

;;;###autoload
(defun mml2015-sign (cont)
  (mml2015-clean-buffer)
  (let ((func (nth 1 (assq mml2015-use mml2015-function-alist))))
    (if func
	(funcall func cont)
      (error "Cannot find sign function"))))

;;;###autoload
(defun mml2015-self-encrypt ()
  (mml2015-encrypt nil))

(provide 'mml2015)

;;; mml2015.el ends here
