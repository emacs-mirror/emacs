;;; mml-smime.el --- S/MIME support for MML
;; Copyright (c) 2000, 2001, 2003 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>
;; Keywords: Gnus, MIME, S/MIME, MML

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

;;; Code:

(eval-when-compile (require 'cl))

(require 'smime)
(require 'mm-decode)
(autoload 'message-narrow-to-headers "message")
(autoload 'message-fetch-field "message")

(defun mml-smime-sign (cont)
  (when (null smime-keys)
    (customize-variable 'smime-keys)
    (error "No S/MIME keys configured, use customize to add your key"))
  (smime-sign-buffer (cdr (assq 'keyfile cont)))
  (goto-char (point-min))
  (while (search-forward "\r\n" nil t)
    (replace-match "\n" t t))
  (goto-char (point-max)))

(defun mml-smime-encrypt (cont)
  (let (certnames certfiles tmp file tmpfiles)
    ;; xxx tmp files are always an security issue
    (while (setq tmp (pop cont))
      (if (and (consp tmp) (eq (car tmp) 'certfile))
	  (push (cdr tmp) certnames)))
    (while (setq tmp (pop certnames))
      (if (not (and (not (file-exists-p tmp))
		    (get-buffer tmp)))
	  (push tmp certfiles)
	(setq file (mm-make-temp-file (expand-file-name "mml." 
							mm-tmp-directory)))
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

(defun mml-smime-sign-query ()
  ;; query information (what certificate) from user when MML tag is
  ;; added, for use later by the signing process
  (when (null smime-keys)
    (customize-variable 'smime-keys)
    (error "No S/MIME keys configured, use customize to add your key"))
  (list 'keyfile
	(if (= (length smime-keys) 1)
	    (cadar smime-keys)
	  (or (let ((from (cadr (funcall gnus-extract-address-components
					 (or (save-excursion
					       (save-restriction
						 (message-narrow-to-headers)
						 (message-fetch-field "from")))
					     "")))))
		(and from (smime-get-key-by-email from)))
	      (smime-get-key-by-email
	       (completing-read "Sign this part with what signature? "
				smime-keys nil nil
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
		     (cadr (funcall gnus-extract-address-components
				    (or (save-excursion
					  (save-restriction
					    (message-narrow-to-headers)
					    (message-fetch-field "to")))
					"")))))
	  (if (setq cert (smime-cert-by-dns who))
	      (setq result (list 'certfile (buffer-name cert)))
	    (setq bad (format "`%s' not found. " who))))
      (quit))
    result))

(defun mml-smime-encrypt-query ()
  ;; todo: add ldap support (xemacs ldap api?)
  ;; todo: try dns/ldap automatically first, before prompting user
  (let (certs done)
    (while (not done)
      (ecase (read (gnus-completing-read-with-default
		    "dns" "Fetch certificate from"
		    '(("dns") ("file")) nil t))
	(dns (setq certs (append certs
				 (mml-smime-get-dns-cert))))
	(file (setq certs (append certs
				  (mml-smime-get-file-cert)))))
      (setq done (not (y-or-n-p "Add more recipients? "))))
    certs))

(defun mml-smime-verify (handle ctl)
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

(defun mml-smime-verify-test (handle ctl)
  smime-openssl-program)

(provide 'mml-smime)

;;; arch-tag: f1bf94d4-f2cd-4c6f-b059-ad69492817e2
;;; mml-smime.el ends here
