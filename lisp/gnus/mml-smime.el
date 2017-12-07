;;; mml-smime.el --- S/MIME support for MML

;; Copyright (C) 2000-2017 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>
;; Keywords: Gnus, MIME, S/MIME, MML

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

(eval-when-compile (require 'cl))

(require 'smime)
(require 'mm-decode)
(require 'mml-sec)
(autoload 'message-narrow-to-headers "message")
(autoload 'message-fetch-field "message")

;; Prefer epg over openssl as epg uses GnuPG's gpgsm,
;; which features full-fledged certificate management, while openssl requires
;; major manual efforts for certificate revocation and expiry and has bugs
;; as documented under man smime(1).
(require 'epg)

(defcustom mml-smime-use 'epg
  "Whether to use OpenSSL or EasyPG (EPG) to handle S/MIME messages.
If you're thinking about using OpenSSL, please first read the BUGS section
in the manual for the `smime' command that comes with OpenSSL.
We recommend EasyPG."
  :group 'mime-security
  :type '(choice (const :tag "EPG" epg)
                 (const :tag "OpenSSL" openssl)))

(defvar mml-smime-function-alist
  '((openssl mml-smime-openssl-sign
	     mml-smime-openssl-encrypt
	     mml-smime-openssl-sign-query
	     mml-smime-openssl-encrypt-query
	     mml-smime-openssl-verify
	     mml-smime-openssl-verify-test)
    (epg mml-smime-epg-sign
	 mml-smime-epg-encrypt
	 nil
	 nil
	 mml-smime-epg-verify
	 mml-smime-epg-verify-test)))

(defcustom mml-smime-cache-passphrase mml-secure-cache-passphrase
  "If t, cache passphrase."
  :group 'mime-security
  :type 'boolean)
(make-obsolete-variable 'mml-smime-cache-passphrase
			'mml-secure-cache-passphrase
			"25.1")

(defcustom mml-smime-passphrase-cache-expiry mml-secure-passphrase-cache-expiry
  "How many seconds the passphrase is cached.
Whether the passphrase is cached at all is controlled by
`mml-smime-cache-passphrase'."
  :group 'mime-security
  :type 'integer)
(make-obsolete-variable 'mml-smime-passphrase-cache-expiry
			'mml-secure-passphrase-cache-expiry
			"25.1")

(defcustom mml-smime-signers nil
  "A list of your own key ID which will be used to sign a message."
  :group 'mime-security
  :type '(repeat (string :tag "Key ID")))

(defcustom mml-smime-sign-with-sender nil
  "If t, use message sender so find a key to sign with."
  :group 'mime-security
  :version "24.4"
  :type 'boolean)

(defcustom mml-smime-encrypt-to-self nil
  "If t, add your own key ID to recipient list when encryption."
  :group 'mime-security
  :version "24.4"
  :type 'boolean)

(defun mml-smime-sign (cont)
  (let ((func (nth 1 (assq mml-smime-use mml-smime-function-alist))))
    (if func
	(funcall func cont)
      (error "Cannot find sign function"))))

(defun mml-smime-encrypt (cont)
  (let ((func (nth 2 (assq mml-smime-use mml-smime-function-alist))))
    (if func
	(funcall func cont)
      (error "Cannot find encrypt function"))))

(defun mml-smime-sign-query ()
  (let ((func (nth 3 (assq mml-smime-use mml-smime-function-alist))))
    (if func
	(funcall func))))

(defun mml-smime-encrypt-query ()
  (let ((func (nth 4 (assq mml-smime-use mml-smime-function-alist))))
    (if func
	(funcall func))))

(defun mml-smime-verify (handle ctl)
  (let ((func (nth 5 (assq mml-smime-use mml-smime-function-alist))))
    (if func
	(funcall func handle ctl)
      handle)))

(defun mml-smime-verify-test (handle ctl)
  (let ((func (nth 6 (assq mml-smime-use mml-smime-function-alist))))
    (if func
	(funcall func handle ctl))))

(defun mml-smime-openssl-sign (cont)
  (when (null smime-keys)
    (customize-variable 'smime-keys)
    (error "No S/MIME keys configured, use customize to add your key"))
  (smime-sign-buffer (cdr (assq 'keyfile cont)))
  (goto-char (point-min))
  (while (search-forward "\r\n" nil t)
    (replace-match "\n" t t))
  (goto-char (point-max)))

(defun mml-smime-openssl-encrypt (cont)
  (let (certnames certfiles tmp file tmpfiles)
    ;; xxx tmp files are always an security issue
    (while (setq tmp (pop cont))
      (if (and (consp tmp) (eq (car tmp) 'certfile))
	  (push (cdr tmp) certnames)))
    (while (setq tmp (pop certnames))
      (if (not (and (not (file-exists-p tmp))
		    (get-buffer tmp)))
	  (push tmp certfiles)
	(setq file (make-temp-file (expand-file-name "mml." mm-tmp-directory)))
	(with-current-buffer tmp
	  (write-region (point-min) (point-max) file))
	(push file certfiles)
	(push file tmpfiles)))
    (if (smime-encrypt-buffer certfiles)
	(progn
	  (while (setq tmp (pop tmpfiles))
	    (delete-file tmp))
	  t)
      (while (setq tmp (pop tmpfiles))
	(delete-file tmp))
      nil))
  (goto-char (point-max)))

(defvar gnus-extract-address-components)

(defun mml-smime-openssl-sign-query ()
  ;; query information (what certificate) from user when MML tag is
  ;; added, for use later by the signing process
  (when (null smime-keys)
    (customize-variable 'smime-keys)
    (error "No S/MIME keys configured, use customize to add your key"))
  (list 'keyfile
	(if (= (length smime-keys) 1)
	    (cadar smime-keys)
	  (or (let ((from (cadr (mail-extract-address-components
				 (or (save-excursion
				       (save-restriction
					 (message-narrow-to-headers)
					 (message-fetch-field "from")))
				     "")))))
		(and from (smime-get-key-by-email from)))
	      (smime-get-key-by-email
	       (gnus-completing-read "Sign this part with what signature"
                                     (mapcar 'car smime-keys) nil nil nil
                                     (and (listp (car-safe smime-keys))
                                          (caar smime-keys))))))))

(defun mml-smime-get-file-cert ()
  (ignore-errors
    (list 'certfile (read-file-name
		     "File with recipient's S/MIME certificate: "
		     smime-certificate-directory nil t ""))))

(defun mml-smime-get-dns-cert ()
  ;; todo: deal with comma separated multiple recipients
  (let (result who bad cert)
    (condition-case ()
	(while (not result)
	  (setq who (read-from-minibuffer
		     (format "%sLookup certificate for: " (or bad ""))
		     (cadr (mail-extract-address-components
			    (or (save-excursion
				  (save-restriction
				    (message-narrow-to-headers)
				    (message-fetch-field "to")))
				"")))))
	  (if (setq cert (smime-cert-by-dns who))
	      (setq result (list 'certfile (buffer-name cert)))
	    (setq bad (format-message "`%s' not found. " who))))
      (quit))
    result))

(defun mml-smime-get-ldap-cert ()
  ;; todo: deal with comma separated multiple recipients
  (let (result who bad cert)
    (condition-case ()
	(while (not result)
	  (setq who (read-from-minibuffer
		     (format "%sLookup certificate for: " (or bad ""))
		     (cadr (funcall gnus-extract-address-components
				    (or (save-excursion
					  (save-restriction
					    (message-narrow-to-headers)
					    (message-fetch-field "to")))
					"")))))
	  (if (setq cert (smime-cert-by-ldap who))
	      (setq result (list 'certfile (buffer-name cert)))
	    (setq bad (format-message "`%s' not found. " who))))
      (quit))
    result))

(autoload 'gnus-completing-read "gnus-util")

(defun mml-smime-openssl-encrypt-query ()
  ;; todo: try dns/ldap automatically first, before prompting user
  (let (certs done)
    (while (not done)
      (ecase (read (gnus-completing-read
		    "Fetch certificate from"
		    '("dns" "ldap" "file") t nil nil
                    "ldap"))
	(dns (setq certs (append certs
				 (mml-smime-get-dns-cert))))
	(ldap (setq certs (append certs
				  (mml-smime-get-ldap-cert))))
	(file (setq certs (append certs
				  (mml-smime-get-file-cert)))))
      (setq done (not (y-or-n-p "Add more recipients? "))))
    certs))

(defun mml-smime-openssl-verify (handle ctl)
  (with-temp-buffer
    (insert-buffer-substring (mm-handle-multipart-original-buffer ctl))
    (goto-char (point-min))
    (insert (format "Content-Type: %s; " (mm-handle-media-type ctl)))
    (insert (format "protocol=\"%s\"; "
		    (mm-handle-multipart-ctl-parameter ctl 'protocol)))
    (insert (format "micalg=\"%s\"; "
		    (mm-handle-multipart-ctl-parameter ctl 'micalg)))
    (insert (format "boundary=\"%s\"\n\n"
		    (mm-handle-multipart-ctl-parameter ctl 'boundary)))
    (when (get-buffer smime-details-buffer)
      (kill-buffer smime-details-buffer))
    (let ((buf (current-buffer))
	  (good-signature (smime-noverify-buffer))
	  (good-certificate (and (or smime-CA-file smime-CA-directory)
				 (smime-verify-buffer)))
	  addresses openssl-output)
      (setq openssl-output (with-current-buffer smime-details-buffer
			     (buffer-string)))
      (if (not good-signature)
	  (progn
	    ;; we couldn't verify message, fail with openssl output as message
	    (mm-set-handle-multipart-parameter
	     mm-security-handle 'gnus-info "Failed")
	    (mm-set-handle-multipart-parameter
	     mm-security-handle 'gnus-details
	     (concat "OpenSSL failed to verify message integrity:\n"
		     "-------------------------------------------\n"
		     openssl-output)))
	;; verify mail addresses in mail against those in certificate
	(when (and (smime-pkcs7-region (point-min) (point-max))
		   (smime-pkcs7-certificates-region (point-min) (point-max)))
	  (with-temp-buffer
	    (insert-buffer-substring buf)
	    (goto-char (point-min))
	    (while (re-search-forward "-----END CERTIFICATE-----" nil t)
	      (when (smime-pkcs7-email-region (point-min) (point))
		(setq addresses (append (smime-buffer-as-string-region
					 (point-min) (point)) addresses)))
	      (delete-region (point-min) (point)))
	    (setq addresses (mapcar 'downcase addresses))))
	(if (not (member (downcase (or (mm-handle-multipart-from ctl) "")) addresses))
	    (mm-set-handle-multipart-parameter
	     mm-security-handle 'gnus-info "Sender address forged")
	  (if good-certificate
	      (mm-set-handle-multipart-parameter
	       mm-security-handle 'gnus-info "Ok (sender authenticated)")
	    (mm-set-handle-multipart-parameter
	     mm-security-handle 'gnus-info "Ok (sender not trusted)")))
	(mm-set-handle-multipart-parameter
	 mm-security-handle 'gnus-details
	 (concat "Sender claimed to be: " (mm-handle-multipart-from ctl) "\n"
		 (if addresses
		     (concat "Addresses in certificate: "
			     (mapconcat 'identity addresses ", "))
		   "No addresses found in certificate. (Requires OpenSSL 0.9.6 or later.)")
		 "\n" "\n"
		 "OpenSSL output:\n"
		 "---------------\n" openssl-output "\n"
		 "Certificate(s) inside S/MIME signature:\n"
		 "---------------------------------------\n"
		 (buffer-string) "\n")))))
  handle)

(defun mml-smime-openssl-verify-test (handle ctl)
  smime-openssl-program)

(defvar epg-user-id-alist)
(defvar epg-digest-algorithm-alist)
(defvar inhibit-redisplay)
(defvar password-cache-expiry)

(eval-when-compile
  (autoload 'epg-make-context "epg")
  (autoload 'epg-context-set-armor "epg")
  (autoload 'epg-context-set-signers "epg")
  (autoload 'epg-context-result-for "epg")
  (autoload 'epg-new-signature-digest-algorithm "epg")
  (autoload 'epg-verify-result-to-string "epg")
  (autoload 'epg-list-keys "epg")
  (autoload 'epg-decrypt-string "epg")
  (autoload 'epg-verify-string "epg")
  (autoload 'epg-sign-string "epg")
  (autoload 'epg-encrypt-string "epg")
  (autoload 'epg-passphrase-callback-function "epg")
  (autoload 'epg-context-set-passphrase-callback "epg")
  (autoload 'epg-sub-key-fingerprint "epg")
  (autoload 'epg-configuration "epg-config")
  (autoload 'epg-expand-group "epg-config")
  (autoload 'epa-select-keys "epa"))

(declare-function epg-key-sub-key-list   "epg" (key) t)
(declare-function epg-sub-key-capability "epg" (sub-key) t)
(declare-function epg-sub-key-validity   "epg" (sub-key) t)

(autoload 'mml-compute-boundary "mml")

(defun mml-smime-epg-sign (cont)
  (let ((inhibit-redisplay t)
	(boundary (mml-compute-boundary cont)))
    (goto-char (point-min))
    (let* ((pair (mml-secure-epg-sign 'CMS cont))
	   (signature (car pair))
	   (micalg (cdr pair)))
      (insert (format "Content-Type: multipart/signed; boundary=\"%s\";\n"
		      boundary))
      (if micalg
	  (insert (format "\tmicalg=%s; "
			  (downcase
			   (cdr (assq micalg
				      epg-digest-algorithm-alist))))))
      (insert "protocol=\"application/pkcs7-signature\"\n")
      (insert (format "\n--%s\n" boundary))
      (goto-char (point-max))
      (insert (format "\n--%s\n" boundary))
      (insert "Content-Type: application/pkcs7-signature; name=smime.p7s
Content-Transfer-Encoding: base64
Content-Disposition: attachment; filename=smime.p7s

")
      (insert (base64-encode-string signature) "\n")
      (goto-char (point-max))
      (insert (format "--%s--\n" boundary))
      (goto-char (point-max)))))

(defun mml-smime-epg-encrypt (cont)
  (let* ((inhibit-redisplay t)
	 (boundary (mml-compute-boundary cont))
	 (cipher (mml-secure-epg-encrypt 'CMS cont)))
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (insert "\
Content-Type: application/pkcs7-mime;
 smime-type=enveloped-data;
 name=smime.p7m
Content-Transfer-Encoding: base64
Content-Disposition: attachment; filename=smime.p7m

")
    (insert (base64-encode-string cipher))
    (goto-char (point-max))))

(defun mml-smime-epg-verify (handle ctl)
  (catch 'error
    (let ((inhibit-redisplay t)
	  context plain signature-file part signature)
      (when (or (null (setq part (mm-find-raw-part-by-type
				  ctl (or (mm-handle-multipart-ctl-parameter
					   ctl 'protocol)
					  "application/pkcs7-signature")
				  t)))
		(null (setq signature (or (mm-find-part-by-type
					   (cdr handle)
					   "application/pkcs7-signature"
					   nil t)
					  (mm-find-part-by-type
					   (cdr handle)
					   "application/x-pkcs7-signature"
					   nil t)))))
	(mm-set-handle-multipart-parameter
	 mm-security-handle 'gnus-info "Corrupted")
	(throw 'error handle))
      (setq part (replace-regexp-in-string "\n" "\r\n" part)
	    context (epg-make-context 'CMS))
      (condition-case error
	  (setq plain (epg-verify-string context (mm-get-part signature) part))
	(error
	 (mm-set-handle-multipart-parameter
	  mm-security-handle 'gnus-info "Failed")
	 (if (eq (car error) 'quit)
	     (mm-set-handle-multipart-parameter
	      mm-security-handle 'gnus-details "Quit.")
	   (mm-set-handle-multipart-parameter
	    mm-security-handle 'gnus-details (format "%S" error)))
	 (throw 'error handle)))
      (mm-set-handle-multipart-parameter
       mm-security-handle 'gnus-info
       (epg-verify-result-to-string (epg-context-result-for context 'verify)))
      handle)))

(defun mml-smime-epg-verify-test (handle ctl)
  t)

(provide 'mml-smime)

;;; mml-smime.el ends here
