;;; mml1991.el --- Old PGP message format (RFC 1991) support for MML

;; Copyright (C) 1998-2018 Free Software Foundation, Inc.

;; Author: Sascha Lüdecke <sascha@meta-x.de>,
;;	Simon Josefsson <simon@josefsson.org> (Mailcrypt interface, Gnus glue)
;; Keywords: PGP

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

(eval-when-compile
  (require 'cl)
  (require 'mm-util))

(require 'mm-encode)
(require 'mml-sec)

(defvar mc-pgp-always-sign)

(autoload 'quoted-printable-decode-region "qp")
(autoload 'quoted-printable-encode-region "qp")

(autoload 'mm-decode-content-transfer-encoding "mm-bodies")
(autoload 'mm-encode-content-transfer-encoding "mm-bodies")
(autoload 'message-options-get "message")
(autoload 'message-options-set "message")

(require 'mml2015)

(defvar mml1991-use mml2015-use
  "The package used for PGP.")

(defvar mml1991-function-alist
  '((mailcrypt mml1991-mailcrypt-sign
	       mml1991-mailcrypt-encrypt)
    (pgg mml1991-pgg-sign
	 mml1991-pgg-encrypt)
    (epg mml1991-epg-sign
	 mml1991-epg-encrypt))
  "Alist of PGP functions.")

(defvar mml1991-cache-passphrase mml-secure-cache-passphrase
  "If t, cache passphrase.")
(make-obsolete-variable 'mml1991-cache-passphrase
			'mml-secure-cache-passphrase
			"25.1")

(defvar mml1991-passphrase-cache-expiry mml-secure-passphrase-cache-expiry
  "How many seconds the passphrase is cached.
Whether the passphrase is cached at all is controlled by
`mml1991-cache-passphrase'.")
(make-obsolete-variable 'mml1991-passphrase-cache-expiry
			'mml-secure-passphrase-cache-expiry
			"25.1")

(defvar mml1991-signers nil
  "A list of your own key ID which will be used to sign a message.")

(defvar mml1991-encrypt-to-self nil
  "If t, add your own key ID to recipient list when encryption.")


;;; mailcrypt wrapper

(autoload 'mc-sign-generic "mc-toplev")

(defvar mml1991-decrypt-function 'mailcrypt-decrypt)
(defvar mml1991-verify-function 'mailcrypt-verify)

(defun mml1991-mailcrypt-sign (cont)
  (let ((text (current-buffer))
	headers signature
	(result-buffer (get-buffer-create "*GPG Result*")))
    ;; Save MIME Content[^ ]+: headers from signing
    (goto-char (point-min))
    (while (looking-at "^Content[^ ]+:") (forward-line))
    (unless (bobp)
      (setq headers (buffer-string))
      (delete-region (point-min) (point)))
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (quoted-printable-decode-region (point-min) (point-max))
    (with-temp-buffer
      (setq signature (current-buffer))
      (insert-buffer-substring text)
      (unless (mc-sign-generic (message-options-get 'message-sender)
			       nil nil nil nil)
	(unless (> (point-max) (point-min))
	  (pop-to-buffer result-buffer)
	  (error "Sign error")))
      (goto-char (point-min))
      (while (re-search-forward "\r+$" nil t)
	(replace-match "" t t))
      (quoted-printable-encode-region (point-min) (point-max))
      (set-buffer text)
      (delete-region (point-min) (point-max))
      (if headers (insert headers))
      (insert "\n")
      (insert-buffer-substring signature)
      (goto-char (point-max)))))

(declare-function mc-encrypt-generic "ext:mc-toplev"
                  (&optional recipients scheme start end from sign))

(defun mml1991-mailcrypt-encrypt (cont &optional sign)
  (let ((text (current-buffer))
	(mc-pgp-always-sign
	 (or mc-pgp-always-sign
	     sign
	     (eq t (or (message-options-get 'message-sign-encrypt)
		       (message-options-set
			'message-sign-encrypt
			(or (y-or-n-p "Sign the message? ")
			    'not))))
	     'never))
	cipher
	(result-buffer (get-buffer-create "*GPG Result*")))
    ;; Strip MIME Content[^ ]: headers since it will be ASCII ARMORED
    (goto-char (point-min))
    (while (looking-at "^Content[^ ]+:") (forward-line))
    (unless (bobp)
      (delete-region (point-min) (point)))
    (with-temp-buffer
      (inline (mm-disable-multibyte))
      (setq cipher (current-buffer))
      (insert-buffer-substring text)
      (unless (mc-encrypt-generic
               (or
                (message-options-get 'message-recipients)
                (message-options-set 'message-recipients
                                     (read-string "Recipients: ")))
               nil
               (point-min) (point-max)
               (message-options-get 'message-sender)
               'sign)
        (unless (> (point-max) (point-min))
          (pop-to-buffer result-buffer)
          (error "Encrypt error")))
      (goto-char (point-min))
      (while (re-search-forward "\r+$" nil t)
        (replace-match "" t t))
      (set-buffer text)
      (delete-region (point-min) (point-max))
      ;;(insert "Content-Type: application/pgp-encrypted\n\n")
      ;;(insert "Version: 1\n\n")
      (insert "\n")
      (insert-buffer-substring cipher)
      (goto-char (point-max)))))

;; pgg wrapper

(autoload 'pgg-sign-region "pgg")
(autoload 'pgg-encrypt-region "pgg")

(defvar pgg-default-user-id)
(defvar pgg-errors-buffer)
(defvar pgg-output-buffer)

(defun mml1991-pgg-sign (cont)
  (let ((pgg-text-mode t)
	(pgg-default-user-id (or (message-options-get 'mml-sender)
				 pgg-default-user-id))
	headers cte)
    ;; Don't sign headers.
    (goto-char (point-min))
    (when (re-search-forward "^$" nil t)
      (setq headers (buffer-substring (point-min) (point)))
      (save-restriction
	(narrow-to-region (point-min) (point))
	(setq cte (mail-fetch-field "content-transfer-encoding")))
      (forward-line 1)
      (delete-region (point-min) (point))
      (when cte
	(setq cte (intern (downcase cte)))
	(mm-decode-content-transfer-encoding cte)))
    (unless (pgg-sign-region (point-min) (point-max) t)
      (pop-to-buffer pgg-errors-buffer)
      (error "Encrypt error"))
    (delete-region (point-min) (point-max))
    (insert
     (with-temp-buffer
       (set-buffer-multibyte nil)
       (insert-buffer-substring pgg-output-buffer)
       (goto-char (point-min))
       (while (re-search-forward "\r+$" nil t)
	 (replace-match "" t t))
       (when cte
	 (mm-encode-content-transfer-encoding cte))
       (goto-char (point-min))
       (when headers
	 (insert headers))
       (insert "\n")
       (buffer-string)))
    t))

(defun mml1991-pgg-encrypt (cont &optional sign)
  (goto-char (point-min))
  (when (re-search-forward "^$" nil t)
    (let ((cte (save-restriction
		 (narrow-to-region (point-min) (point))
		 (mail-fetch-field "content-transfer-encoding"))))
      ;; Strip MIME headers since it will be ASCII armored.
      (forward-line 1)
      (delete-region (point-min) (point))
      (when cte
	(mm-decode-content-transfer-encoding (intern (downcase cte))))))
  (unless (let ((pgg-text-mode t))
	    (pgg-encrypt-region
	     (point-min) (point-max)
	     (split-string
	      (or
	       (message-options-get 'message-recipients)
	       (message-options-set 'message-recipients
				    (read-string "Recipients: ")))
	      "[ \f\t\n\r\v,]+")
	     sign))
    (pop-to-buffer pgg-errors-buffer)
    (error "Encrypt error"))
  (delete-region (point-min) (point-max))
  (insert "\n")
  (insert-buffer-substring pgg-output-buffer)
  t)

;; epg wrapper

(defvar epg-user-id-alist)

(autoload 'epg-make-context "epg")
(autoload 'epg-passphrase-callback-function "epg")
(autoload 'epa-select-keys "epa")
(autoload 'epg-list-keys "epg")
(autoload 'epg-context-set-armor "epg")
(autoload 'epg-context-set-textmode "epg")
(autoload 'epg-context-set-signers "epg")
(autoload 'epg-context-set-passphrase-callback "epg")
(autoload 'epg-key-sub-key-list "epg")
(autoload 'epg-sub-key-capability "epg")
(autoload 'epg-sub-key-validity "epg")
(autoload 'epg-sub-key-fingerprint "epg")
(autoload 'epg-sign-string "epg")
(autoload 'epg-encrypt-string "epg")
(autoload 'epg-configuration "epg-config")
(autoload 'epg-expand-group "epg-config")

(defun mml1991-epg-sign (cont)
  (let ((inhibit-redisplay t)
	headers cte)
    ;; Don't sign headers.
    (goto-char (point-min))
    (when (re-search-forward "^$" nil t)
      (setq headers (buffer-substring (point-min) (point)))
      (save-restriction
	(narrow-to-region (point-min) (point))
	(setq cte (mail-fetch-field "content-transfer-encoding")))
      (forward-line 1)
      (delete-region (point-min) (point))
      (when cte
	(setq cte (intern (downcase cte)))
	(mm-decode-content-transfer-encoding cte)))
    (let* ((pair (mml-secure-epg-sign 'OpenPGP 'clear))
	   (signature (car pair)))
      (delete-region (point-min) (point-max))
      (insert
       (with-temp-buffer
	 (set-buffer-multibyte nil)
	 (insert signature)
	 (goto-char (point-min))
	 (while (re-search-forward "\r+$" nil t)
	   (replace-match "" t t))
	 (when cte
	   (mm-encode-content-transfer-encoding cte))
	 (goto-char (point-min))
	 (when headers
	   (insert headers))
	 (insert "\n")
	 (buffer-string)))
      t)))

(defun mml1991-epg-encrypt (cont &optional sign)
  (goto-char (point-min))
  (when (re-search-forward "^$" nil t)
    (let ((cte (save-restriction
		 (narrow-to-region (point-min) (point))
		 (mail-fetch-field "content-transfer-encoding"))))
      ;; Strip MIME headers since it will be ASCII armored.
      (forward-line 1)
      (delete-region (point-min) (point))
      (when cte
	(mm-decode-content-transfer-encoding (intern (downcase cte))))))
  (let ((cipher (mml-secure-epg-encrypt 'OpenPGP cont sign)))
    (delete-region (point-min) (point-max))
    (insert "\n" cipher))
  t)

;;;###autoload
(defun mml1991-encrypt (cont &optional sign)
  (let ((func (nth 2 (assq mml1991-use mml1991-function-alist))))
    (if func
	(funcall func cont sign)
      (error "Cannot find encrypt function"))))

;;;###autoload
(defun mml1991-sign (cont)
  (let ((func (nth 1 (assq mml1991-use mml1991-function-alist))))
    (if func
	(funcall func cont)
      (error "Cannot find sign function"))))

(provide 'mml1991)

;; Local Variables:
;; coding: utf-8
;; End:

;;; mml1991.el ends here
