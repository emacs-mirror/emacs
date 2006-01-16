;;; mml1991.el --- Old PGP message format (RFC 1991) support for MML

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Sascha Lüdecke <sascha@meta-x.de>,
;;	Simon Josefsson <simon@josefsson.org> (Mailcrypt interface, Gnus glue)
;; Keywords PGP

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'mm-util))

(defvar mc-pgp-always-sign)

(autoload 'quoted-printable-decode-region "qp")
(autoload 'quoted-printable-encode-region "qp")

(defvar mml1991-use mml2015-use
  "The package used for PGP.")

(defvar mml1991-function-alist
  '((mailcrypt mml1991-mailcrypt-sign
	       mml1991-mailcrypt-encrypt)
    (gpg mml1991-gpg-sign
	 mml1991-gpg-encrypt)
    (pgg mml1991-pgg-sign
	 mml1991-pgg-encrypt))
  "Alist of PGP functions.")

;;; mailcrypt wrapper

(eval-and-compile
  (autoload 'mc-sign-generic "mc-toplev"))

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
    ;; Strip MIME Content[^ ]: headers since it will be ASCII ARMOURED
    (goto-char (point-min))
    (while (looking-at "^Content[^ ]+:") (forward-line))
    (unless (bobp)
      (delete-region (point-min) (point)))
    (mm-with-unibyte-current-buffer
      (with-temp-buffer
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
	(goto-char (point-max))))))

;;; gpg wrapper

(eval-and-compile
  (autoload 'gpg-sign-cleartext "gpg"))

(defun mml1991-gpg-sign (cont)
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
      (unless (gpg-sign-cleartext text (setq signature (current-buffer))
				  result-buffer
				  nil
				  (message-options-get 'message-sender))
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

(defun mml1991-gpg-encrypt (cont &optional sign)
  (let ((text (current-buffer))
	cipher
	(result-buffer (get-buffer-create "*GPG Result*")))
    ;; Strip MIME Content[^ ]: headers since it will be ASCII ARMOURED
    (goto-char (point-min))
    (while (looking-at "^Content[^ ]+:") (forward-line))
    (unless (bobp)
      (delete-region (point-min) (point)))
    (mm-with-unibyte-current-buffer
      (with-temp-buffer
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
		   sign
		   text (setq cipher (current-buffer))
		   result-buffer
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
	      (pop-to-buffer result-buffer)
	      (error "Encrypt error"))))
	(goto-char (point-min))
	(while (re-search-forward "\r+$" nil t)
	  (replace-match "" t t))
	(set-buffer text)
	(delete-region (point-min) (point-max))
	;;(insert "Content-Type: application/pgp-encrypted\n\n")
	;;(insert "Version: 1\n\n")
	(insert "\n")
	(insert-buffer-substring cipher)
	(goto-char (point-max))))))

;; pgg wrapper

(eval-when-compile
  (defvar pgg-default-user-id)
  (defvar pgg-errors-buffer)
  (defvar pgg-output-buffer))

(defun mml1991-pgg-sign (cont)
  (let (headers cte)
    ;; Don't sign headers.
    (goto-char (point-min))
    (while (not (looking-at "^$"))
      (forward-line))
    (unless (eobp) ;; no headers?
      (setq headers (buffer-substring (point-min) (point)))
      (forward-line) ;; skip header/body separator
      (delete-region (point-min) (point)))
    (when (string-match "^Content-Transfer-Encoding: \\(.+\\)" headers)
      (setq cte (intern (match-string 1 headers))))
    (mm-decode-content-transfer-encoding cte)
    (unless (let ((pgg-default-user-id
		   (or (message-options-get 'mml-sender)
		       pgg-default-user-id)))
	      (pgg-sign-region (point-min) (point-max) t))
      (pop-to-buffer pgg-errors-buffer)
      (error "Encrypt error"))
    (delete-region (point-min) (point-max))
    (mm-with-unibyte-current-buffer
      (insert-buffer-substring pgg-output-buffer)
      (goto-char (point-min))
      (while (re-search-forward "\r+$" nil t)
	(replace-match "" t t))
      (mm-encode-content-transfer-encoding cte)
      (goto-char (point-min))
      (when headers
	(insert headers))
      (insert "\n"))
    t))

(defun mml1991-pgg-encrypt (cont &optional sign)
  (let (cte)
    ;; Strip MIME Content[^ ]: headers since it will be ASCII ARMOURED
    (goto-char (point-min))
    (while (looking-at "^Content[^ ]+:")
      (when (looking-at "^Content-Transfer-Encoding: \\(.+\\)")
	(setq cte (intern (match-string 1))))
      (forward-line))
    (unless (bobp)
      (delete-region (point-min) (point)))
    (mm-decode-content-transfer-encoding cte)
    (unless (pgg-encrypt-region
	     (point-min) (point-max)
	     (split-string
	      (or
	       (message-options-get 'message-recipients)
	       (message-options-set 'message-recipients
				    (read-string "Recipients: ")))
	      "[ \f\t\n\r\v,]+")
	     sign)
      (pop-to-buffer pgg-errors-buffer)
      (error "Encrypt error"))
    (delete-region (point-min) (point-max))
    ;;(insert "Content-Type: application/pgp-encrypted\n\n")
    ;;(insert "Version: 1\n\n")
    (insert "\n")
    (insert-buffer-substring pgg-output-buffer)
    t))

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
;; coding: iso-8859-1
;; End:

;;; arch-tag: e542be18-ab28-4393-9b33-97fe9cf30706
;;; mml1991.el ends here
