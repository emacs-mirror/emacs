;;; pgg-gpg.el --- GnuPG support for PGG.

;; Copyright (C) 1999, 2000, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Symmetric encryption added by: Sascha Wilde <wilde@sha-bang.de>
;; Created: 1999/10/28
;; Keywords: PGP, OpenPGP, GnuPG

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(eval-when-compile
  (require 'cl)				; for gpg macros
  (require 'pgg))

(defgroup pgg-gpg ()
  "GnuPG interface."
  :group 'pgg)

(defcustom pgg-gpg-program "gpg"
  "The GnuPG executable."
  :group 'pgg-gpg
  :type 'string)

(defcustom pgg-gpg-extra-args nil
  "Extra arguments for every GnuPG invocation."
  :group 'pgg-gpg
  :type '(repeat (string :tag "Argument")))

(defcustom pgg-gpg-recipient-argument "--recipient"
  "GnuPG option to specify recipient."
  :group 'pgg-gpg
  :type '(choice (const :tag "New `--recipient' option" "--recipient")
		 (const :tag "Old `--remote-user' option" "--remote-user")))

(defvar pgg-gpg-user-id nil
  "GnuPG ID of your default identity.")

(defun pgg-gpg-process-region (start end passphrase program args)
  (let* ((output-file-name (pgg-make-temp-file "pgg-output"))
	 (args
	  `("--status-fd" "2"
	    ,@(if passphrase '("--passphrase-fd" "0"))
	    "--yes" ; overwrite
	    "--output" ,output-file-name
	    ,@pgg-gpg-extra-args ,@args))
	 (output-buffer pgg-output-buffer)
	 (errors-buffer pgg-errors-buffer)
	 (orig-mode (default-file-modes))
	 (process-connection-type nil)
	 exit-status)
    (with-current-buffer (get-buffer-create errors-buffer)
      (buffer-disable-undo)
      (erase-buffer))
    (unwind-protect
	(progn
	  (set-default-file-modes 448)
	  (let ((coding-system-for-write 'binary)
		(input (buffer-substring-no-properties start end))
		(default-enable-multibyte-characters nil))
	    (with-temp-buffer
	      (when passphrase
		(insert passphrase "\n"))
	      (insert input)
	      (setq exit-status
		    (apply #'call-process-region (point-min) (point-max) program
			   nil errors-buffer nil args))))
	  (with-current-buffer (get-buffer-create output-buffer)
	    (buffer-disable-undo)
	    (erase-buffer)
	    (if (file-exists-p output-file-name)
		(let ((coding-system-for-read 'raw-text-dos))
		  (insert-file-contents output-file-name)))
	    (set-buffer errors-buffer)
	    (if (not (equal exit-status 0))
		(insert (format "\n%s exited abnormally: '%s'\n"
				program exit-status)))))
      (if (file-exists-p output-file-name)
	  (delete-file output-file-name))
      (set-default-file-modes orig-mode))))

(defun pgg-gpg-possibly-cache-passphrase (passphrase &optional key notruncate)
  (if (and pgg-cache-passphrase
	   (progn
	     (goto-char (point-min))
	     (re-search-forward "^\\[GNUPG:] \\(GOOD_PASSPHRASE\\>\\)\\|\\(SIG_CREATED\\)" nil t)))
      (pgg-add-passphrase-to-cache
       (or key
	   (progn
	     (goto-char (point-min))
	     (if (re-search-forward
		  "^\\[GNUPG:] NEED_PASSPHRASE\\(_PIN\\)? \\w+ ?\\w*" nil t)
		 (substring (match-string 0) -8))))
       passphrase
       notruncate)))

(defvar pgg-gpg-all-secret-keys 'unknown)

(defun pgg-gpg-lookup-all-secret-keys ()
  "Return all secret keys present in secret key ring."
  (when (eq pgg-gpg-all-secret-keys 'unknown)
    (setq pgg-gpg-all-secret-keys '())
    (let ((args (list "--with-colons" "--no-greeting" "--batch"
		      "--list-secret-keys")))
      (with-temp-buffer
	(apply #'call-process pgg-gpg-program nil t nil args)
	(goto-char (point-min))
	(while (re-search-forward
		"^\\(sec\\|pub\\):[^:]*:[^:]*:[^:]*:\\([^:]*\\)" nil t)
	  (push (substring (match-string 2) 8)
		pgg-gpg-all-secret-keys)))))
  pgg-gpg-all-secret-keys)

(defun pgg-gpg-lookup-key (string &optional type)
  "Search keys associated with STRING."
  (let ((args (list "--with-colons" "--no-greeting" "--batch"
		    (if type "--list-secret-keys" "--list-keys")
		    string)))
    (with-temp-buffer
      (apply #'call-process pgg-gpg-program nil t nil args)
      (goto-char (point-min))
      (if (re-search-forward "^\\(sec\\|pub\\):[^:]*:[^:]*:[^:]*:\\([^:]*\\)"
			     nil t)
	  (substring (match-string 2) 8)))))

(defun pgg-gpg-lookup-key-owner (string &optional all)
  "Search keys associated with STRING and return owner of identified key.

The value may be just the bare key id, or it may be a combination of the
user name associated with the key and the key id, with the key id enclosed
in \"<...>\" angle brackets.

Optional ALL non-nil means search all keys, including secret keys."
  (let ((args (list "--with-colons" "--no-greeting" "--batch"
		    (if all "--list-secret-keys" "--list-keys")
		    string))
        (key-regexp (concat "^\\(sec\\|pub\\)"
                            ":[^:]*:[^:]*:[^:]*:\\([^:]*\\):[^:]*"
                            ":[^:]*:[^:]*:[^:]*:\\([^:]*\\):"))
        )
    (with-temp-buffer
      (apply #'call-process pgg-gpg-program nil t nil args)
      (goto-char (point-min))
      (if (re-search-forward key-regexp
                             nil t)
          (match-string 3)))))

(defun pgg-gpg-key-id-from-key-owner (key-owner)
  (cond ((not key-owner) nil)
        ;; Extract bare key id from outermost paired angle brackets, if any:
        ((string-match "[^<]*<\\(.+\\)>[^>]*" key-owner)
         (substring key-owner (match-beginning 1)(match-end 1)))
        (key-owner))
  )

(defun pgg-gpg-encrypt-region (start end recipients &optional sign passphrase)
  "Encrypt the current region between START and END.

If optional argument SIGN is non-nil, do a combined sign and encrypt.

If optional PASSPHRASE is not specified, it will be obtained from the
passphrase cache or user."
  (let* ((pgg-gpg-user-id (or pgg-gpg-user-id pgg-default-user-id))
	 (passphrase (or passphrase
                         (when sign
                           (pgg-read-passphrase
                            (format "GnuPG passphrase for %s: "
                                    pgg-gpg-user-id)
                            pgg-gpg-user-id))))
	 (args
	  (append
	   (list "--batch" "--textmode" "--armor" "--always-trust" "--encrypt")
	   (if sign (list "--sign" "--local-user" pgg-gpg-user-id))
	   (if recipients
	       (apply #'nconc
		      (mapcar (lambda (rcpt)
				(list pgg-gpg-recipient-argument rcpt))
			      (append recipients
				      (if pgg-encrypt-for-me
					  (list pgg-gpg-user-id)))))))))
    (pgg-as-lbt start end 'CRLF
      (pgg-gpg-process-region start end passphrase pgg-gpg-program args))
    (when sign
      (with-current-buffer pgg-errors-buffer
	;; Possibly cache passphrase under, e.g. "jas", for future sign.
	(pgg-gpg-possibly-cache-passphrase passphrase pgg-gpg-user-id)
	;; Possibly cache passphrase under, e.g. B565716F, for future decrypt.
	(pgg-gpg-possibly-cache-passphrase passphrase)))
    (pgg-process-when-success)))

(defun pgg-gpg-encrypt-symmetric-region (start end &optional passphrase)
  "Encrypt the current region between START and END with symmetric cipher.

If optional PASSPHRASE is not specified, it will be obtained from the
passphrase cache or user."
  (let* ((passphrase (or passphrase
                         (pgg-read-passphrase
                          "GnuPG passphrase for symmetric encryption: ")))
	 (args
	  (append (list "--batch" "--textmode" "--armor" "--symmetric" ))))
    (pgg-as-lbt start end 'CRLF
      (pgg-gpg-process-region start end passphrase pgg-gpg-program args))
    (pgg-process-when-success)))

(defun pgg-gpg-decrypt-region (start end &optional passphrase)
  "Decrypt the current region between START and END.

If optional PASSPHRASE is not specified, it will be obtained from the
passphrase cache or user."
  (let* ((current-buffer (current-buffer))
	 (message-keys (with-temp-buffer
			 (insert-buffer-substring current-buffer)
			 (pgg-decode-armor-region (point-min) (point-max))))
	 (secret-keys (pgg-gpg-lookup-all-secret-keys))
         ;; XXX the user is stuck if they need to use the passphrase for
         ;;     any but the first secret key for which the message is
         ;;     encrypted.  ideally, we would incrementally give them a
         ;;     chance with subsequent keys each time they fail with one.
	 (key (pgg-gpg-select-matching-key message-keys secret-keys))
         (key-owner (and key (pgg-gpg-lookup-key-owner key t)))
	 (key-id (pgg-gpg-key-id-from-key-owner key-owner))
	 (pgg-gpg-user-id (or key-id key
	                      pgg-gpg-user-id pgg-default-user-id))
	 (passphrase (or passphrase
                         (pgg-read-passphrase
                          (format (if (pgg-gpg-symmetric-key-p message-keys)
                                      "Passphrase for symmetric decryption: "
                                    "GnuPG passphrase for %s: ")
                                  (or key-owner "??"))
                          pgg-gpg-user-id)))
	 (args '("--batch" "--decrypt")))
    (pgg-gpg-process-region start end passphrase pgg-gpg-program args)
    (with-current-buffer pgg-errors-buffer
      (pgg-gpg-possibly-cache-passphrase passphrase pgg-gpg-user-id)
      (goto-char (point-min))
      (re-search-forward "^\\[GNUPG:] DECRYPTION_OKAY\\>" nil t))))

;;;###autoload
(defun pgg-gpg-symmetric-key-p (message-keys)
  "True if decoded armor MESSAGE-KEYS has symmetric encryption indicator."
  (let (result)
    (dolist (key message-keys result)
      (when (and (eq (car key) 3)
		 (member '(symmetric-key-algorithm) key))
	(setq result key)))))

(defun pgg-gpg-select-matching-key (message-keys secret-keys)
  "Choose a key from MESSAGE-KEYS that matches one of the keys in SECRET-KEYS."
  (loop for message-key in message-keys
	for message-key-id = (and (equal (car message-key) 1)
				  (cdr (assq 'key-identifier
                                             (cdr message-key))))
	for key = (and message-key-id (pgg-lookup-key message-key-id 'encrypt))
	when (and key (member key secret-keys)) return key))

(defun pgg-gpg-sign-region (start end &optional cleartext passphrase)
  "Make detached signature from text between START and END."
  (let* ((pgg-gpg-user-id (or pgg-gpg-user-id pgg-default-user-id))
	 (passphrase (or passphrase
                         (pgg-read-passphrase
                          (format "GnuPG passphrase for %s: " pgg-gpg-user-id)
                          pgg-gpg-user-id)))
	 (args
	  (list (if cleartext "--clearsign" "--detach-sign")
		"--armor" "--batch" "--verbose"
		"--local-user" pgg-gpg-user-id))
	 (inhibit-read-only t)
	 buffer-read-only)
    (pgg-as-lbt start end 'CRLF
      (pgg-gpg-process-region start end passphrase pgg-gpg-program args))
    (with-current-buffer pgg-errors-buffer
      ;; Possibly cache passphrase under, e.g. "jas", for future sign.
      (pgg-gpg-possibly-cache-passphrase passphrase pgg-gpg-user-id)
      ;; Possibly cache passphrase under, e.g. B565716F, for future decrypt.
      (pgg-gpg-possibly-cache-passphrase passphrase))
    (pgg-process-when-success)))

(defun pgg-gpg-verify-region (start end &optional signature)
  "Verify region between START and END as the detached signature SIGNATURE."
  (let ((args '("--batch" "--verify")))
    (when (stringp signature)
      (setq args (append args (list signature))))
    (setq args (append args '("-")))
    (pgg-gpg-process-region start end nil pgg-gpg-program args)
    (with-current-buffer pgg-errors-buffer
      (goto-char (point-min))
      (while (re-search-forward "^gpg: \\(.*\\)\n" nil t)
	(with-current-buffer pgg-output-buffer
	  (insert-buffer-substring pgg-errors-buffer
				   (match-beginning 1) (match-end 0)))
	(delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (re-search-forward "^\\[GNUPG:] GOODSIG\\>" nil t))))

(defun pgg-gpg-insert-key ()
  "Insert public key at point."
  (let* ((pgg-gpg-user-id (or pgg-gpg-user-id pgg-default-user-id))
	 (args (list "--batch" "--export" "--armor"
		     pgg-gpg-user-id)))
    (pgg-gpg-process-region (point)(point) nil pgg-gpg-program args)
    (insert-buffer-substring pgg-output-buffer)))

(defun pgg-gpg-snarf-keys-region (start end)
  "Add all public keys in region between START and END to the keyring."
  (let ((args '("--import" "--batch" "-")) status)
    (pgg-gpg-process-region start end nil pgg-gpg-program args)
    (set-buffer pgg-errors-buffer)
    (goto-char (point-min))
    (when (re-search-forward "^\\[GNUPG:] IMPORT_RES\\>" nil t)
      (setq status (buffer-substring (match-end 0)
				     (progn (end-of-line)(point)))
	    status (vconcat (mapcar #'string-to-number (split-string status))))
      (erase-buffer)
      (insert (format "Imported %d key(s).
\tArmor contains %d key(s) [%d bad, %d old].\n"
		      (+ (aref status 2)
			 (aref status 10))
		      (aref status 0)
		      (aref status 1)
		      (+ (aref status 4)
			 (aref status 11)))
	      (if (zerop (aref status 9))
		  ""
		"\tSecret keys are imported.\n")))
    (append-to-buffer pgg-output-buffer (point-min)(point-max))
    (pgg-process-when-success)))

(provide 'pgg-gpg)

;;; arch-tag: 2aa5d5d8-93a0-4865-9312-33e29830e000
;;; pgg-gpg.el ends here
