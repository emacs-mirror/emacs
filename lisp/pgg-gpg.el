;;; pgg-gpg.el --- GnuPG support for PGG.

;; Copyright (C) 1999, 2000, 2002, 2003, 2004,
;;   2005, 2006 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Symmetric encryption support added by: Sascha Wilde <wilde@sha-bang.de>
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

(defcustom pgg-gpg-use-agent nil
  "Whether to use gnupg agent for key caching."
  :group 'pgg-gpg
  :type 'boolean)

(defvar pgg-gpg-user-id nil
  "GnuPG ID of your default identity.")

(defvar pgg-gpg-user-id-alist nil
  "An alist mapping from key ID to user ID.")

(defvar pgg-gpg-read-point nil)
(defvar pgg-gpg-output-file-name nil)
(defvar pgg-gpg-pending-status-list nil)
(defvar pgg-gpg-key-id nil)
(defvar pgg-gpg-passphrase nil)
(defvar pgg-gpg-debug nil)

(defun pgg-gpg-start-process (args)
  (let* ((output-file-name (pgg-make-temp-file "pgg-output"))
	 (args
	  (append (list "--no-tty"
			"--status-fd" "1"
			"--command-fd" "0"
			"--yes" ; overwrite
			"--output" output-file-name)
		  (if pgg-gpg-use-agent '("--use-agent"))
		  pgg-gpg-extra-args
		  args))
	 (coding-system-for-write 'binary)
	 (process-connection-type nil)
	 (orig-mode (default-file-modes))
	 (buffer (generate-new-buffer " *pgg-gpg*"))
	 process)
    (with-current-buffer buffer
      (make-local-variable 'pgg-gpg-read-point)
      (setq pgg-gpg-read-point (point-min))
      (make-local-variable 'pgg-gpg-output-file-name)
      (setq pgg-gpg-output-file-name output-file-name)
      (make-local-variable 'pgg-gpg-pending-status-list)
      (setq pgg-gpg-pending-status-list nil)
      (make-local-variable 'pgg-gpg-key-id)
      (setq pgg-gpg-key-id nil)
      (make-local-variable 'pgg-gpg-passphrase)
      (setq pgg-gpg-passphrase nil))
    (unwind-protect
	(progn
	  (set-default-file-modes 448)
	  (setq process
		(apply #'start-process "pgg-gpg" buffer pgg-gpg-program args)))
      (set-default-file-modes orig-mode))
    (set-process-filter process #'pgg-gpg-process-filter)
    (set-process-sentinel process #'pgg-gpg-process-sentinel)
    process))

(defun pgg-gpg-process-filter (process input)
  (if pgg-gpg-debug
      (save-excursion
	(set-buffer (get-buffer-create  " *pgg-gpg-debug*"))
	(goto-char (point-max))
	(insert input)))
  (if (buffer-live-p (process-buffer process))
      (save-excursion
	(set-buffer (process-buffer process))
	(goto-char (point-max))
	(insert input)
	(goto-char pgg-gpg-read-point)
	(beginning-of-line)
	(while (looking-at ".*\n")	;the input line is finished
	  (save-excursion
	    (if (looking-at "\\[GNUPG:] \\([A-Z_]+\\)\\>.*")
		(let* ((status (match-string 1))
		       (symbol (intern-soft (concat "pgg-gpg-status-" status)))
		       (entry (member status pgg-gpg-pending-status-list)))
		  (if entry
		      (setq pgg-gpg-pending-status-list
			    (delq (car entry)
				  pgg-gpg-pending-status-list)))
		  (if (and symbol
			   (fboundp symbol))
		      (funcall symbol process (buffer-substring
					       (match-beginning 1)
					       (match-end 0)))))))
	  (forward-line))
	(setq pgg-gpg-read-point (point)))))

(defun pgg-gpg-process-sentinel (process status)
  (if (buffer-live-p (process-buffer process))
      (save-excursion
	(set-buffer (process-buffer process))
	(when pgg-gpg-passphrase
	  (fillarray pgg-gpg-passphrase 0)
	  (setq pgg-gpg-passphrase nil))
	;; Copy the contents of process-buffer to pgg-errors-buffer.
	(set-buffer (get-buffer-create pgg-errors-buffer))
	(buffer-disable-undo)
	(erase-buffer)
	(insert-buffer-substring (process-buffer process))
	;; Read the contents of the output file to pgg-output-buffer.
	(set-buffer (get-buffer-create pgg-output-buffer))
	(buffer-disable-undo)
	(erase-buffer)
	(if (equal status "finished\n")
	    (let ((output-file-name
		   (with-current-buffer (process-buffer process)
		     pgg-gpg-output-file-name)))
	      (when (file-exists-p output-file-name)
		(let ((coding-system-for-read (if pgg-text-mode
						  'raw-text
						'binary)))
		  (insert-file-contents output-file-name))
		(delete-file output-file-name))))
	(kill-buffer (process-buffer process)))))

(defun pgg-gpg-wait-for-status (process status-list)
  (with-current-buffer (process-buffer process)
    (setq pgg-gpg-pending-status-list status-list)
    (while (and (eq (process-status process) 'run)
		pgg-gpg-pending-status-list)
      (accept-process-output process 1))))

(defun pgg-gpg-wait-for-completion (process)
  (process-send-eof process)
  (while (eq (process-status process) 'run)
    ;; We can't use accept-process-output instead of sit-for here
    ;; because it may cause an interrupt during the sentinel execution.
    (sit-for 0.1)))

(defun pgg-gpg-status-USERID_HINT (process line)
  (if (string-match "\\`USERID_HINT \\([^ ]+\\) \\(.*\\)" line)
      (let* ((key-id (match-string 1 line))
	     (user-id (match-string 2 line))
	     (entry (assoc key-id pgg-gpg-user-id-alist)))
	(if entry
	    (setcdr entry user-id)
	  (setq pgg-gpg-user-id-alist (cons (cons key-id user-id)
					    pgg-gpg-user-id-alist))))))

(defun pgg-gpg-status-NEED_PASSPHRASE (process line)
  (if (string-match "\\`NEED_PASSPHRASE \\([^ ]+\\)" line)
      (setq pgg-gpg-key-id (match-string 1 line))))

(defun pgg-gpg-status-NEED_PASSPHRASE_SYM (process line)
  (setq pgg-gpg-key-id 'SYM))

(defun pgg-gpg-status-NEED_PASSPHRASE_PIN (process line)
  (setq pgg-gpg-key-id 'PIN))

(defun pgg-gpg-status-GET_HIDDEN (process line)
  (let ((entry (assoc pgg-gpg-key-id pgg-gpg-user-id-alist)))
    (if (setq pgg-gpg-passphrase
	      (if (eq pgg-gpg-key-id 'SYM)
		  (pgg-read-passphrase
		   "GnuPG passphrase for symmetric encryption: ")
		(pgg-read-passphrase
		 (format "GnuPG passphrase for %s: "
			 (if entry
			     (cdr entry)
			   pgg-gpg-key-id))
		 (if (eq pgg-gpg-key-id 'PIN)
		     "PIN"
		   pgg-gpg-key-id))))
	(process-send-string process (concat pgg-gpg-passphrase "\n")))))

(defun pgg-gpg-status-GOOD_PASSPHRASE (process line)
  (when (and pgg-gpg-passphrase
	     (stringp pgg-gpg-key-id))
    (pgg-add-passphrase-to-cache pgg-gpg-key-id pgg-gpg-passphrase)
    (setq pgg-gpg-passphrase nil)))

(defun pgg-gpg-status-BAD_PASSPHRASE (process line)
  (when pgg-gpg-passphrase
    (fillarray pgg-gpg-passphrase 0)
    (setq pgg-gpg-passphrase nil)))

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

(defun pgg-gpg-encrypt-region (start end recipients &optional sign passphrase)
  "Encrypt the current region between START and END.

If optional argument SIGN is non-nil, do a combined sign and encrypt."
  (let* ((pgg-gpg-user-id (or pgg-gpg-user-id pgg-default-user-id))
	 (args
	  (append
	   '("--armor" "--always-trust" "--encrypt")
	   (if pgg-text-mode '("--textmode"))
	   (if sign (list "--sign" "--local-user" pgg-gpg-user-id))
	   (if recipients
	       (apply #'nconc
		      (mapcar (lambda (rcpt)
				(list pgg-gpg-recipient-argument rcpt))
			      (append recipients
				      (if pgg-encrypt-for-me
					  (list pgg-gpg-user-id))))))))
	 (process (pgg-gpg-start-process args)))
    (if (and sign (not pgg-gpg-use-agent))
	(pgg-gpg-wait-for-status process '("BEGIN_SIGNING" "GOOD_PASSPHRASE")))
    (process-send-region process start end)
    (pgg-gpg-wait-for-completion process)
    (save-excursion
      (set-buffer (get-buffer-create pgg-errors-buffer))
      (goto-char (point-max))
      (not (null (re-search-backward "^\\[GNUPG:] END_ENCRYPTION\\>"
				     nil t))))))

(defun pgg-gpg-encrypt-symmetric-region (start end &optional passphrase)
  "Encrypt the current region between START and END with symmetric cipher."
  (let* ((args
	  (append '("--armor" "--symmetric")
		  (if pgg-text-mode '("--textmode"))))
	 (process (pgg-gpg-start-process args)))
    (pgg-gpg-wait-for-status process '("BEGIN_ENCRYPTION"))
    (process-send-region process start end)
    (pgg-gpg-wait-for-completion process)
    (save-excursion
      (set-buffer (get-buffer-create pgg-errors-buffer))
      (goto-char (point-max))
      (not (null (re-search-backward "^\\[GNUPG:] END_ENCRYPTION\\>"
				     nil t))))))

(defun pgg-gpg-decrypt-region (start end &optional passphrase)
  "Decrypt the current region between START and END."
  (let* ((args '("--decrypt"))
	 (process (pgg-gpg-start-process args)))
    (process-send-region process start end)
    (pgg-gpg-wait-for-status process '("BEGIN_DECRYPTION"))
    (pgg-gpg-wait-for-completion process)
    (save-excursion
      (set-buffer (get-buffer-create pgg-errors-buffer))
      (goto-char (point-max))
      (not (null (re-search-backward "^\\[GNUPG:] DECRYPTION_OKAY\\>"
				     nil t))))))

(defun pgg-gpg-sign-region (start end &optional cleartext passphrase)
  "Make detached signature from text between START and END."
  (let* ((pgg-gpg-user-id (or pgg-gpg-user-id pgg-default-user-id))
	 (args
	  (append (list (if cleartext "--clearsign" "--detach-sign")
			"--armor" "--verbose"
			"--local-user" pgg-gpg-user-id)
		  (if pgg-text-mode '("--textmode"))))
	 (process (pgg-gpg-start-process args)))
    (unless pgg-gpg-use-agent
      (pgg-gpg-wait-for-status process '("BEGIN_SIGNING" "GOOD_PASSPHRASE")))
    (process-send-region process start end)
    (pgg-gpg-wait-for-completion process)
    (save-excursion
      (set-buffer (get-buffer-create pgg-errors-buffer))
      (goto-char (point-max))
      (not (null (re-search-backward "^\\[GNUPG:] SIG_CREATED\\>"
				     nil t))))))

(defun pgg-gpg-verify-region (start end &optional signature)
  "Verify region between START and END as the detached signature SIGNATURE."
  (let ((args '("--verify"))
	process)
    (when (stringp signature)
      (setq args (append args (list signature))))
    (setq process (pgg-gpg-start-process (append args '("-"))))
    (process-send-region process start end)
    (pgg-gpg-wait-for-completion process)
    (save-excursion
      (set-buffer (get-buffer-create pgg-errors-buffer))
      (goto-char (point-max))
      (not (null (re-search-backward "^\\[GNUPG:] GOODSIG\\>"
				     nil t))))))

(defun pgg-gpg-insert-key ()
  "Insert public key at point."
  (let* ((pgg-gpg-user-id (or pgg-gpg-user-id pgg-default-user-id))
	 (args (list "--export" "--armor"
		     pgg-gpg-user-id))
	 (process (pgg-gpg-start-process args)))
    (pgg-gpg-wait-for-completion process)
    (insert-buffer-substring pgg-output-buffer)))

(defun pgg-gpg-snarf-keys-region (start end)
  "Add all public keys in region between START and END to the keyring."
  (let* ((args '("--import" "-"))
	 (process (pgg-gpg-start-process args))
	 status)
    (process-send-region process start end)
    (pgg-gpg-wait-for-completion process)
    (save-excursion
      (set-buffer (get-buffer-create pgg-errors-buffer))
      (goto-char (point-max))
      (not (null (re-search-backward "^\\[GNUPG:] IMPORT_RES\\>"
				     nil t))))))

(provide 'pgg-gpg)

;;; arch-tag: 2aa5d5d8-93a0-4865-9312-33e29830e000
;;; pgg-gpg.el ends here
