;;; encrypt.el --- file encryption routines

;; Copyright (C) 2002, 2003, 2004, 2005, 2007  Free Software Foundation, Inc.

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Created: 2003/01/24
;; Keywords: files

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;;; This module addresses data encryption.  Page breaks are used for
;;; grouping declarations and documentation relating to each
;;; particular aspect.

;;; Use in Gnus like this:
;;; (setq
;;;   nnimap-authinfo-file "~/.authinfo.enc"
;;;   nntp-authinfo-file "~/.authinfo.enc"
;;;   smtpmail-auth-credentials "~/.authinfo.enc"
;;;   ;; using the AES256 cipher, feel free to use your own favorite
;;;   encrypt-file-alist (quote (("~/.authinfo.enc" (gpg "AES256"))))
;;;   password-cache-expiry 600)

;;; Then write ~/.authinfo.enc:

;;; 1) open the old authinfo
;;; C-x C-f ~/.authinfo

;;; 2) write the new authinfo.enc
;;; M-x encrypt-write-file-contents RET ~/.authinfo.enc

;;; 3) verify the new authinfo is correct (this will show the contents in the minibuffer)
;;; M-: (encrypt-get-file-contents "~/.authinfo.enc")


;;; Code:

(require 'password)

(defgroup encrypt '((password-cache custom-variable)
		    (password-cache-expiry custom-variable))
  "File encryption configuration."
  :group 'applications)

(defcustom encrypt-file-alist nil
  "List of file names or regexes matched with encryptions.
Format example:
 '((\"beta\"
    (gpg \"AES\"))
   (\"/home/tzz/alpha\"
    (encrypt-xor \"Semi-Secret\")))"

  :type '(repeat
	  (list :tag "Encryption entry"
	   (radio :tag "What to encrypt"
		  (file :tag "Filename")
		  (regexp :tag "Regular expression match"))
	   (radio :tag "How to encrypt it"
		  (list
		   :tag "GPG Encryption"
		   (const :tag "GPG Program" gpg)
		   (radio :tag "Choose a cipher"
			  (const :tag "3DES Encryption" "3DES")
			  (const :tag "CAST5 Encryption" "CAST5")
			  (const :tag "Blowfish Encryption" "BLOWFISH")
			  (const :tag "AES Encryption" "AES")
			  (const :tag "AES192 Encryption" "AES192")
			  (const :tag "AES256 Encryption" "AES256")
			  (const :tag "Twofish Encryption" "TWOFISH")
			  (string :tag "Cipher Name")))
		  (list
		   :tag "Built-in simple XOR"
		   (const :tag "XOR Encryption" encrypt-xor)
		   (string :tag "XOR Cipher Value (seed value)")))))
  :group 'encrypt)

;; TODO: now, load gencrypt.el and if successful, modify the
;; custom-type of encrypt-file-alist to add the gencrypt.el options

;; (plist-get (symbol-plist 'encrypt-file-alist) 'custom-type)
;; then use plist-put

(defcustom encrypt-gpg-path (executable-find "gpg")
  "Path to the GPG program."
  :type '(radio
	  (file :tag "Location of the GPG executable")
	  (const :tag "GPG is not installed" nil))
  :group 'encrypt)

(defvar encrypt-temp-prefix "encrypt"
  "Prefix for temporary filenames")

;;;###autoload
(defun encrypt-find-model (filename)
  "Given a filename, find a encrypt-file-alist entry"
  (dolist (entry encrypt-file-alist)
    (let ((match (nth 0 entry))
	  (model (nth 1 entry)))
      (when (or (eq match filename)
		(string-match match filename))
	(return model)))))

;;;###autoload
(defun encrypt-insert-file-contents (file &optional model)
  "Decrypt FILE into the current buffer."
  (interactive "fFile to insert: ")
  (let* ((model (or model (encrypt-find-model file)))
	 (method (nth 0 model))
	 (cipher (nth 1 model))
	 (password-key (format "encrypt-password-%s-%s %s"
			       (symbol-name method) cipher file))
	 (passphrase
	  (password-read-and-add
	   (format "%s password for cipher %s (file %s)? "
		   file (symbol-name method) cipher)
	   password-key))
	  (buffer-file-coding-system 'binary)
	 (coding-system-for-read 'binary)
	 outdata)

    ;; note we only insert-file-contents if the method is known to be valid
    (cond
     ((eq method 'gpg)
      (insert-file-contents file)
      (setq outdata (encrypt-gpg-decode-buffer passphrase cipher)))
     ((eq method 'encrypt-xor)
      (insert-file-contents file)
      (setq outdata (encrypt-xor-decode-buffer passphrase cipher))))

    (if outdata
	(progn
	  (message "%s was decrypted with %s (cipher %s)"
		   file (symbol-name method) cipher)
	  (delete-region (point-min) (point-max))
	  (goto-char (point-min))
	  (insert outdata))
      ;; the decryption failed, alas
      (password-cache-remove password-key)
      (gnus-error 5 "%s was NOT decrypted with %s (cipher %s)"
		  file (symbol-name method) cipher))))

(defun encrypt-get-file-contents (file &optional model)
  "Decrypt FILE and return the contents."
  (interactive "fFile to decrypt: ")
  (with-temp-buffer
    (encrypt-insert-file-contents file model)
    (buffer-string)))

(defun encrypt-put-file-contents (file data &optional model)
  "Encrypt the DATA to FILE, then continue normally."
  (with-temp-buffer
    (insert data)
    (encrypt-write-file-contents file model)))

(defun encrypt-write-file-contents (file &optional model)
  "Encrypt the current buffer to FILE, then continue normally."
  (interactive "sFile to write: ")
  (setq model (or model (encrypt-find-model file)))
  (if model
      (let* ((method (nth 0 model))
	     (cipher (nth 1 model))
	     (password-key (format "encrypt-password-%s-%s %s"
				   (symbol-name method) cipher file))
	     (passphrase
	      (password-read
	       (format "%s password for cipher %s? "
		       (symbol-name method) cipher)
	       password-key))
	     outdata)

	(cond
	 ((eq method 'gpg)
	  (setq outdata (encrypt-gpg-encode-buffer passphrase cipher)))
	 ((eq method 'encrypt-xor)
	  (setq outdata (encrypt-xor-encode-buffer passphrase cipher))))

	(if outdata
	    (progn
	      (message "%s was encrypted with %s (cipher %s)"
		       file (symbol-name method) cipher)
	      (delete-region (point-min) (point-max))
	      (goto-char (point-min))
	      (insert outdata)
	      ;; do not confirm overwrites
	      (write-file file nil))
	  ;; the decryption failed, alas
	  (password-cache-remove password-key)
	  (gnus-error 5 "%s was NOT encrypted with %s (cipher %s)"
		      file (symbol-name method) cipher)))
    (gnus-error 1 "%s has no associated encryption model!  See encrypt-file-alist." file)))

(defun encrypt-xor-encode-buffer (passphrase cipher)
  (encrypt-xor-process-buffer passphrase cipher t))

(defun encrypt-xor-decode-buffer (passphrase cipher)
  (encrypt-xor-process-buffer passphrase cipher nil))

(defun encrypt-xor-process-buffer (passphrase
					cipher
					&optional encode)
  "Given PASSPHRASE, xor-encode or decode the contents of the current buffer."
  (let* ((bs (buffer-substring-no-properties (point-min) (point-max)))
	 ;; passphrase-sum is a simple additive checksum of the
	 ;; passphrase and the cipher
	(passphrase-sum
	 (when (stringp passphrase)
	   (apply '+ (append cipher passphrase nil))))
	new-list)

    (with-temp-buffer
      (if encode
	  (progn
	    (dolist (x (append bs nil))
	      (setq new-list (cons (logxor x passphrase-sum) new-list)))

	    (dolist (x new-list)
	      (insert (format "%d " x))))
	(progn
	  (setq new-list (reverse (split-string bs)))
	  (dolist (x new-list)
	    (setq x (string-to-number x))
	    (insert (format "%c" (logxor x passphrase-sum))))))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun encrypt-gpg-encode-buffer (passphrase cipher)
  (encrypt-gpg-process-buffer passphrase cipher t))

(defun encrypt-gpg-decode-buffer (passphrase cipher)
  (encrypt-gpg-process-buffer passphrase cipher nil))

(defun encrypt-gpg-process-buffer (passphrase 
					cipher 
					&optional encode)
  "With PASSPHRASE, use GPG to encode or decode the current buffer."
  (let* ((program encrypt-gpg-path)
	 (input (buffer-substring-no-properties (point-min) (point-max)))
	 (temp-maker (if (fboundp 'make-temp-file) 
			 'make-temp-file 
		       'make-temp-name))
	 (temp-file (funcall temp-maker encrypt-temp-prefix))
	 (default-enable-multibyte-characters nil)
	 (args `("--cipher-algo" ,cipher
		 "--status-fd" "2"
		 "--logger-fd" "2"
		 "--passphrase-fd" "0"
		 "--no-tty"))
	 exit-status exit-data)
    
    (when encode
      (setq args
	    (append args
		    '("--symmetric"
		      "--armor"))))

    (if program
	(with-temp-buffer
	  (when passphrase
	    (insert passphrase "\n"))
	  (insert input)
	  (setq exit-status
		(apply #'call-process-region (point-min) (point-max) program
		       t `(t ,temp-file) nil args))
	  (if (equal exit-status 0)
	      (setq exit-data
		    (buffer-substring-no-properties (point-min) (point-max)))
	    (with-temp-buffer
	      (when (file-exists-p temp-file)
		(insert-file-contents temp-file))
	      (gnus-error 5 (format "%s exited abnormally: '%s' [%s]"
				    program exit-status (buffer-string)))))
	  (delete-file temp-file))
      (gnus-error 5 "GPG is not installed."))
    exit-data))

(provide 'encrypt)
;;; encrypt.el ends here

;; arch-tag: d907e4f1-71b5-42b1-a180-fc7b84ff0648
