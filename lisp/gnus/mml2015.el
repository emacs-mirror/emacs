;;; mml2015.el --- MIME Security with Pretty Good Privacy (PGP)

;; Copyright (C) 2000, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: PGP MIME MML

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; RFC 2015 is updated by RFC 3156, this file should be compatible
;; with both.

;;; Code:

(eval-when-compile (require 'cl))
(require 'mm-decode)
(require 'mm-util)
(require 'mml)

(defvar mml2015-use (or
		     (progn
		       (ignore-errors
			 (require 'pgg))
		       (and (fboundp 'pgg-sign-region)
			    'pgg))
		     (progn
		       (ignore-errors
			 (require 'gpg))
		       (and (fboundp 'gpg-sign-detached)
			    'gpg))
		     (progn (ignore-errors
			      (load "mc-toplev"))
			    (and (fboundp 'mc-encrypt-generic)
				 (fboundp 'mc-sign-generic)
				 (fboundp 'mc-cleanup-recipient-headers)
				 'mailcrypt)))
  "The package used for PGP/MIME.")

;; Something is not RFC2015.
(defvar mml2015-function-alist
  '((mailcrypt mml2015-mailcrypt-sign
	       mml2015-mailcrypt-encrypt
	       mml2015-mailcrypt-verify
	       mml2015-mailcrypt-decrypt
	       mml2015-mailcrypt-clear-verify
	       mml2015-mailcrypt-clear-decrypt)
    (gpg mml2015-gpg-sign
	 mml2015-gpg-encrypt
	 mml2015-gpg-verify
	 mml2015-gpg-decrypt
	 mml2015-gpg-clear-verify
	 mml2015-gpg-clear-decrypt)
  (pgg mml2015-pgg-sign
       mml2015-pgg-encrypt
       mml2015-pgg-verify
       mml2015-pgg-decrypt
       mml2015-pgg-clear-verify
       mml2015-pgg-clear-decrypt))
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

;;; mailcrypt wrapper

(eval-and-compile
  (autoload 'mailcrypt-decrypt "mailcrypt")
  (autoload 'mailcrypt-verify "mailcrypt")
  (autoload 'mc-pgp-always-sign "mailcrypt")
  (autoload 'mc-encrypt-generic "mc-toplev")
  (autoload 'mc-cleanup-recipient-headers "mc-toplev")
  (autoload 'mc-sign-generic "mc-toplev"))

(eval-when-compile
  (defvar mc-default-scheme)
  (defvar mc-schemes))

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
	(mm-set-handle-multipart-parameter
	 mm-security-handle 'gnus-info "Corrupted")
	(throw 'error handle))
      (with-temp-buffer
	(mm-insert-part child)
	(setq result
	      (condition-case err
		  (funcall mml2015-decrypt-function)
		(error
		 (mm-set-handle-multipart-parameter
		  mm-security-handle 'gnus-details (mml2015-format-error err))
		 nil)
		(quit
		 (mm-set-handle-multipart-parameter
		  mm-security-handle 'gnus-details "Quit.")
		 nil)))
	(unless (car result)
	  (mm-set-handle-multipart-parameter
	   mm-security-handle 'gnus-info "Failed")
	  (throw 'error handle))
	(setq handles (mm-dissect-buffer t)))
      (mm-destroy-parts handle)
      (mm-set-handle-multipart-parameter
       mm-security-handle 'gnus-info
       (concat "OK"
	       (let ((sig (with-current-buffer mml2015-result-buffer
			    (mml2015-gpg-extract-signature-details))))
		 (concat ", Signer: " sig))))
      (if (listp (car handles))
	  handles
	(list handles)))))

(defun mml2015-mailcrypt-clear-decrypt ()
  (let (result)
    (setq result
	  (condition-case err
	      (funcall mml2015-decrypt-function)
	    (error
	     (mm-set-handle-multipart-parameter
	      mm-security-handle 'gnus-details (mml2015-format-error err))
	     nil)
	    (quit
	     (mm-set-handle-multipart-parameter
	      mm-security-handle 'gnus-details "Quit.")
	     nil)))
    (if (car result)
	(mm-set-handle-multipart-parameter
	 mm-security-handle 'gnus-info "OK")
      (mm-set-handle-multipart-parameter
       mm-security-handle 'gnus-info "Failed"))))

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
	(mm-set-handle-multipart-parameter
	 mm-security-handle 'gnus-info "Corrupted")
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
	  (mm-set-handle-multipart-parameter
	   mm-security-handle 'gnus-info "Corrupted")
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
			    (mm-set-handle-multipart-parameter
			     mm-security-handle 'gnus-details
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
		     (mm-set-handle-multipart-parameter
		      mm-security-handle 'gnus-details (mml2015-format-error err))
		     nil)
		    (quit
		     (mm-set-handle-multipart-parameter
		      mm-security-handle 'gnus-details "Quit.")
		     nil))
	    (mm-set-handle-multipart-parameter
	     mm-security-handle 'gnus-info "Failed")
	    (throw 'error handle))))
      (mm-set-handle-multipart-parameter
       mm-security-handle 'gnus-info "OK")
      handle)))

(defun mml2015-mailcrypt-clear-verify ()
  (let ((mc-gpg-debug-buffer (get-buffer-create " *gnus gpg debug*")))
    (if (condition-case err
	    (prog1
		(funcall mml2015-verify-function)
	      (if (get-buffer " *mailcrypt stderr temp")
		  (mm-set-handle-multipart-parameter
		   mm-security-handle 'gnus-details
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
	   (mm-set-handle-multipart-parameter
	    mm-security-handle 'gnus-details (mml2015-format-error err))
	   nil)
	  (quit
	   (mm-set-handle-multipart-parameter
	    mm-security-handle 'gnus-details "Quit.")
	   nil))
	(mm-set-handle-multipart-parameter
	 mm-security-handle 'gnus-info "OK")
      (mm-set-handle-multipart-parameter
       mm-security-handle 'gnus-info "Failed"))))

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
    (mm-with-unibyte-current-buffer
      (mc-encrypt-generic
       (or (message-options-get 'message-recipients)
	   (message-options-set 'message-recipients
			      (mc-cleanup-recipient-headers
			       (read-string "Recipients: "))))
       nil nil nil
       (message-options-get 'message-sender))))
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

;;; gpg wrapper

(eval-and-compile
  (autoload 'gpg-decrypt "gpg")
  (autoload 'gpg-verify "gpg")
  (autoload 'gpg-verify-cleartext "gpg")
  (autoload 'gpg-sign-detached "gpg")
  (autoload 'gpg-sign-encrypt "gpg")
  (autoload 'gpg-encrypt "gpg")
  (autoload 'gpg-passphrase-read "gpg"))

(defun mml2015-gpg-passphrase ()
  (or (message-options-get 'gpg-passphrase)
      (message-options-set 'gpg-passphrase (gpg-passphrase-read))))

(defun mml2015-gpg-decrypt-1 ()
  (let ((cipher (current-buffer)) plain result)
    (if (with-temp-buffer
	  (prog1
	      (gpg-decrypt cipher (setq plain (current-buffer))
			   mml2015-result-buffer nil)
	    (mm-set-handle-multipart-parameter
	     mm-security-handle 'gnus-details
	     (with-current-buffer mml2015-result-buffer
	       (buffer-string)))
	    (set-buffer cipher)
	    (erase-buffer)
	    (insert-buffer-substring plain)
	    (goto-char (point-min))
	    (while (search-forward "\r\n" nil t)
	      (replace-match "\n" t t))))
	'(t)
      ;; Some wrong with the return value, check plain text buffer.
      (if (> (point-max) (point-min))
	  '(t)
	nil))))

(defun mml2015-gpg-decrypt (handle ctl)
  (let ((mml2015-decrypt-function 'mml2015-gpg-decrypt-1))
    (mml2015-mailcrypt-decrypt handle ctl)))

(defun mml2015-gpg-clear-decrypt ()
  (let (result)
    (setq result (mml2015-gpg-decrypt-1))
    (if (car result)
	(mm-set-handle-multipart-parameter
	 mm-security-handle 'gnus-info "OK")
      (mm-set-handle-multipart-parameter
       mm-security-handle 'gnus-info "Failed"))))

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
	     (case n-slice
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

(defun mml2015-gpg-verify (handle ctl)
  (catch 'error
    (let (part message signature info-is-set-p)
      (unless (setq part (mm-find-raw-part-by-type
			  ctl (or (mm-handle-multipart-ctl-parameter
				   ctl 'protocol)
				  "application/pgp-signature")
			  t))
	(mm-set-handle-multipart-parameter
	 mm-security-handle 'gnus-info "Corrupted")
	(throw 'error handle))
      (with-temp-buffer
	(setq message (current-buffer))
	(insert part)
	;; Convert <LF> to <CR><LF> in verify mode.  Sign and
	;; clearsign use --textmode. The conversion is not necessary.
	;; In clearverify, the conversion is not necessary either.
	(goto-char (point-min))
	(end-of-line)
	(while (not (eobp))
	  (unless (eq (char-before) ?\r)
	    (insert "\r"))
	  (forward-line)
	  (end-of-line))
	(with-temp-buffer
	  (setq signature (current-buffer))
	  (unless (setq part (mm-find-part-by-type
			      (cdr handle) "application/pgp-signature" nil t))
	    (mm-set-handle-multipart-parameter
	     mm-security-handle 'gnus-info "Corrupted")
	    (throw 'error handle))
	  (mm-insert-part part)
	  (unless (condition-case err
		      (prog1
			  (gpg-verify message signature mml2015-result-buffer)
			(mm-set-handle-multipart-parameter
			 mm-security-handle 'gnus-details
			 (with-current-buffer mml2015-result-buffer
			   (buffer-string))))
		    (error
		     (mm-set-handle-multipart-parameter
		      mm-security-handle 'gnus-details (mml2015-format-error err))
		     (mm-set-handle-multipart-parameter
		      mm-security-handle 'gnus-info "Error.")
		     (setq info-is-set-p t)
		     nil)
		    (quit
		     (mm-set-handle-multipart-parameter
		      mm-security-handle 'gnus-details "Quit.")
		     (mm-set-handle-multipart-parameter
		      mm-security-handle 'gnus-info "Quit.")
		     (setq info-is-set-p t)
		     nil))
	    (unless info-is-set-p
	      (mm-set-handle-multipart-parameter
	       mm-security-handle 'gnus-info "Failed"))
	    (throw 'error handle)))
	(mm-set-handle-multipart-parameter
	 mm-security-handle 'gnus-info
	 (with-current-buffer mml2015-result-buffer
	   (mml2015-gpg-extract-signature-details))))
      handle)))

(defun mml2015-gpg-clear-verify ()
  (if (condition-case err
	  (prog1
	      (gpg-verify-cleartext (current-buffer) mml2015-result-buffer)
	    (mm-set-handle-multipart-parameter
	     mm-security-handle 'gnus-details
	     (with-current-buffer mml2015-result-buffer
	       (buffer-string))))
	(error
	 (mm-set-handle-multipart-parameter
	  mm-security-handle 'gnus-details (mml2015-format-error err))
	 nil)
	(quit
	 (mm-set-handle-multipart-parameter
	  mm-security-handle 'gnus-details "Quit.")
	 nil))
      (mm-set-handle-multipart-parameter
       mm-security-handle 'gnus-info
       (with-current-buffer mml2015-result-buffer
	 (mml2015-gpg-extract-signature-details)))
    (mm-set-handle-multipart-parameter
     mm-security-handle 'gnus-info "Failed")))

(defun mml2015-gpg-sign (cont)
  (let ((boundary (mml-compute-boundary cont))
	(text (current-buffer)) signature)
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (with-temp-buffer
      (unless (gpg-sign-detached text (setq signature (current-buffer))
				 mml2015-result-buffer
				 nil
				 (message-options-get 'message-sender)
				 t t) ; armor & textmode
	(unless (> (point-max) (point-min))
	  (pop-to-buffer mml2015-result-buffer)
	  (error "Sign error")))
      (goto-char (point-min))
      (while (re-search-forward "\r+$" nil t)
	(replace-match "" t t))
      (set-buffer text)
      (goto-char (point-min))
      (insert (format "Content-Type: multipart/signed; boundary=\"%s\";\n"
		      boundary))
      ;;; FIXME: what is the micalg?
      (insert "\tmicalg=pgp-sha1; protocol=\"application/pgp-signature\"\n")
      (insert (format "\n--%s\n" boundary))
      (goto-char (point-max))
      (insert (format "\n--%s\n" boundary))
      (insert "Content-Type: application/pgp-signature\n\n")
      (insert-buffer-substring signature)
      (goto-char (point-max))
      (insert (format "--%s--\n" boundary))
      (goto-char (point-max)))))

(defun mml2015-gpg-encrypt (cont &optional sign)
  (let ((boundary (mml-compute-boundary cont))
	(text (current-buffer))
	cipher)
    (mm-with-unibyte-current-buffer
      (with-temp-buffer
	;; set up a function to call the correct gpg encrypt routine
	;; with the right arguments. (FIXME: this should be done
	;; differently.)
	(flet ((gpg-encrypt-func
		 (sign plaintext ciphertext result recipients &optional
		       passphrase sign-with-key armor textmode)
		 (if sign
		     (gpg-sign-encrypt
		      plaintext ciphertext result recipients passphrase
		      sign-with-key armor textmode)
		   (gpg-encrypt
		    plaintext ciphertext result recipients passphrase
		    armor textmode))))
	  (unless (gpg-encrypt-func
		    sign ; passed in when using signencrypt
		    text (setq cipher (current-buffer))
		    mml2015-result-buffer
		    (split-string
		     (or
		      (message-options-get 'message-recipients)
		      (message-options-set 'message-recipients
					   (read-string "Recipients: ")))
		     "[ \f\t\n\r\v,]+")
		    nil
		    (message-options-get 'message-sender)
		    t t) ; armor & textmode
	    (unless (> (point-max) (point-min))
	      (pop-to-buffer mml2015-result-buffer)
	      (error "Encrypt error"))))
	(goto-char (point-min))
	(while (re-search-forward "\r+$" nil t)
	  (replace-match "" t t))
	(set-buffer text)
	(delete-region (point-min) (point-max))
	(insert (format "Content-Type: multipart/encrypted; boundary=\"%s\";\n"
			boundary))
	(insert "\tprotocol=\"application/pgp-encrypted\"\n\n")
	(insert (format "--%s\n" boundary))
	(insert "Content-Type: application/pgp-encrypted\n\n")
	(insert "Version: 1\n\n")
	(insert (format "--%s\n" boundary))
	(insert "Content-Type: application/octet-stream\n\n")
	(insert-buffer-substring cipher)
	(goto-char (point-max))
	(insert (format "--%s--\n" boundary))
	(goto-char (point-max))))))

;;; pgg wrapper

(eval-when-compile
  (defvar pgg-default-user-id)
  (defvar pgg-errors-buffer)
  (defvar pgg-output-buffer))

(eval-and-compile
  (autoload 'pgg-decrypt-region "pgg")
  (autoload 'pgg-verify-region "pgg")
  (autoload 'pgg-sign-region "pgg")
  (autoload 'pgg-encrypt-region "pgg")
  (autoload 'pgg-parse-armor "pgg-parse"))

(defun mml2015-pgg-decrypt (handle ctl)
  (catch 'error
    (let ((pgg-errors-buffer mml2015-result-buffer)
	  child handles result decrypt-status)
      (unless (setq child (mm-find-part-by-type
			   (cdr handle)
			   "application/octet-stream" nil t))
	(mm-set-handle-multipart-parameter
	 mm-security-handle 'gnus-info "Corrupted")
	(throw 'error handle))
      (with-temp-buffer
	(mm-insert-part child)
	(if (condition-case err
		(prog1
		    (pgg-decrypt-region (point-min) (point-max))
		  (setq decrypt-status
			(with-current-buffer mml2015-result-buffer
			  (buffer-string)))
		  (mm-set-handle-multipart-parameter
		   mm-security-handle 'gnus-details
		   decrypt-status))
	      (error
	       (mm-set-handle-multipart-parameter
		mm-security-handle 'gnus-details (mml2015-format-error err))
	       nil)
	      (quit
	       (mm-set-handle-multipart-parameter
		mm-security-handle 'gnus-details "Quit.")
	       nil))
	    (with-current-buffer pgg-output-buffer
	      (goto-char (point-min))
	      (while (search-forward "\r\n" nil t)
		(replace-match "\n" t t))
	      (setq handles (mm-dissect-buffer t))
	      (mm-destroy-parts handle)
	      (mm-set-handle-multipart-parameter
	       mm-security-handle 'gnus-info "OK")
	      (mm-set-handle-multipart-parameter
	       mm-security-handle 'gnus-details
	       (concat decrypt-status
		       (when (stringp (car handles))
			 "\n" (mm-handle-multipart-ctl-parameter
			       handles 'gnus-details))))
	      (if (listp (car handles))
		  handles
		(list handles)))
	  (mm-set-handle-multipart-parameter
	   mm-security-handle 'gnus-info "Failed")
	  (throw 'error handle))))))

(defun mml2015-pgg-clear-decrypt ()
  (let ((pgg-errors-buffer mml2015-result-buffer))
    (if (prog1
	    (pgg-decrypt-region (point-min) (point-max))
	  (mm-set-handle-multipart-parameter
	   mm-security-handle 'gnus-details
	   (with-current-buffer mml2015-result-buffer
	     (buffer-string))))
	(progn
	  (erase-buffer)
	  (insert-buffer-substring pgg-output-buffer)
	  (goto-char (point-min))
	  (while (search-forward "\r\n" nil t)
	    (replace-match "\n" t t))
	  (mm-set-handle-multipart-parameter
	   mm-security-handle 'gnus-info "OK"))
      (mm-set-handle-multipart-parameter
       mm-security-handle 'gnus-info "Failed"))))

(defun mml2015-pgg-verify (handle ctl)
  (let ((pgg-errors-buffer mml2015-result-buffer)
	signature-file part signature)
    (if (or (null (setq part (mm-find-raw-part-by-type
			      ctl (or (mm-handle-multipart-ctl-parameter
				       ctl 'protocol)
				      "application/pgp-signature")
			      t)))
	    (null (setq signature (mm-find-part-by-type
				   (cdr handle) "application/pgp-signature" nil t))))
	(progn
	  (mm-set-handle-multipart-parameter
	   mm-security-handle 'gnus-info "Corrupted")
	  handle)
      (with-temp-buffer
	(insert part)
	;; Convert <LF> to <CR><LF> in verify mode.  Sign and
	;; clearsign use --textmode. The conversion is not necessary.
	;; In clearverify, the conversion is not necessary either.
	(goto-char (point-min))
	(end-of-line)
	(while (not (eobp))
	  (unless (eq (char-before) ?\r)
	    (insert "\r"))
	  (forward-line)
	  (end-of-line))
	(with-temp-file (setq signature-file (mm-make-temp-file "pgg"))
	  (mm-insert-part signature))
	(if (condition-case err
		(prog1
		    (pgg-verify-region (point-min) (point-max)
				       signature-file t)
		  (goto-char (point-min))
		  (while (search-forward "\r\n" nil t)
		    (replace-match "\n" t t))
		  (mm-set-handle-multipart-parameter
		   mm-security-handle 'gnus-details
		   (concat (with-current-buffer pgg-output-buffer
			     (buffer-string))
			   (with-current-buffer pgg-errors-buffer
			     (buffer-string)))))
	      (error
	       (mm-set-handle-multipart-parameter
		mm-security-handle 'gnus-details (mml2015-format-error err))
	       nil)
	      (quit
	       (mm-set-handle-multipart-parameter
		mm-security-handle 'gnus-details "Quit.")
	       nil))
	    (progn
	      (delete-file signature-file)
	      (mm-set-handle-multipart-parameter
	       mm-security-handle 'gnus-info
	       (with-current-buffer pgg-errors-buffer
		 (mml2015-gpg-extract-signature-details))))
	  (delete-file signature-file)
	  (mm-set-handle-multipart-parameter
	   mm-security-handle 'gnus-info "Failed")))))
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
	      (mm-set-handle-multipart-parameter
	       mm-security-handle 'gnus-details
	       (concat (with-current-buffer pgg-output-buffer
			 (buffer-string))
		       (with-current-buffer pgg-errors-buffer
			 (buffer-string)))))
	  (error
	   (mm-set-handle-multipart-parameter
	    mm-security-handle 'gnus-details (mml2015-format-error err))
	   nil)
	  (quit
	   (mm-set-handle-multipart-parameter
	    mm-security-handle 'gnus-details "Quit.")
	   nil))
	(mm-set-handle-multipart-parameter
	 mm-security-handle 'gnus-info
	 (with-current-buffer pgg-errors-buffer
	   (mml2015-gpg-extract-signature-details)))
      (mm-set-handle-multipart-parameter
       mm-security-handle 'gnus-info "Failed"))))

(defun mml2015-pgg-sign (cont)
  (let ((pgg-errors-buffer mml2015-result-buffer)
	(boundary (mml-compute-boundary cont))
	(pgg-default-user-id (or (message-options-get 'mml-sender)
				 pgg-default-user-id))
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

;;; General wrapper

(defun mml2015-clean-buffer ()
  (if (gnus-buffer-live-p mml2015-result-buffer)
      (with-current-buffer mml2015-result-buffer
	(erase-buffer)
	t)
    (setq mml2015-result-buffer
	  (gnus-get-buffer-create "*MML2015 Result*"))
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

;;; arch-tag: b04701d5-0b09-44d8-bed8-de901bf435f2
;;; mml2015.el ends here
