;;; pgg.el --- glue for the various PGP implementations.

;; Copyright (C) 1999, 2000, 2003, 2005 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 1999/10/28
;; Keywords: PGP

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

;;; Commentary:

;;; Code:

(require 'pgg-def)
(require 'pgg-parse)
(autoload 'run-at-time "timer")

;; Don't merge these two `eval-when-compile's.
(eval-when-compile
  (require 'cl))
;; Fixme: This would be better done with an autoload for
;; `url-insert-file-contents', and the url stuff rationalized.
;; (`locate-library' can say whether the url code is available.)
(eval-when-compile
  (ignore-errors
    (require 'w3)
    (require 'url)))

;;; @ utility functions
;;;

(defvar pgg-fetch-key-function (if (fboundp 'url-insert-file-contents)
				   (function pgg-fetch-key-with-w3)))

(defun pgg-invoke (func scheme &rest args)
  (progn
    (require (intern (format "pgg-%s" scheme)))
    (apply 'funcall (intern (format "pgg-%s-%s" scheme func)) args)))

(put 'pgg-save-coding-system 'lisp-indent-function 2)

(defmacro pgg-save-coding-system (start end &rest body)
  `(if (interactive-p)
       (let ((buffer (current-buffer)))
	 (with-temp-buffer
	   (let (buffer-undo-list)
	     (insert-buffer-substring buffer ,start ,end)
	     (encode-coding-region (point-min)(point-max)
				   buffer-file-coding-system)
	     (prog1 (save-excursion ,@body)
	       (push nil buffer-undo-list)
	       (ignore-errors (undo))))))
     (save-restriction
       (narrow-to-region ,start ,end)
       ,@body)))

(defun pgg-temp-buffer-show-function (buffer)
  (let ((window (or (get-buffer-window buffer 'visible)
		    (split-window-vertically))))
    (set-window-buffer window buffer)
    (shrink-window-if-larger-than-buffer window)))

(defun pgg-display-output-buffer (start end status)
  (if status
      (progn
	(delete-region start end)
	(insert-buffer-substring pgg-output-buffer)
	(decode-coding-region start (point) buffer-file-coding-system))
    (let ((temp-buffer-show-function
	   (function pgg-temp-buffer-show-function)))
      (with-output-to-temp-buffer pgg-echo-buffer
	(set-buffer standard-output)
	(insert-buffer-substring pgg-errors-buffer)))))

(defvar pgg-passphrase-cache (make-vector 7 0))

(defun pgg-read-passphrase (prompt &optional key)
  (or (and pgg-cache-passphrase
	   key (setq key (pgg-truncate-key-identifier key))
	   (symbol-value (intern-soft key pgg-passphrase-cache)))
      (read-passwd prompt)))

(eval-when-compile
  (defmacro pgg-run-at-time-1 (time repeat function args)
    (when (featurep 'xemacs)
      (if (condition-case nil
	      (let ((delete-itimer 'delete-itimer)
		    (itimer-driver-start 'itimer-driver-start)
		    (itimer-value 'itimer-value)
		    (start-itimer 'start-itimer))
		(unless (or (symbol-value 'itimer-process)
			    (symbol-value 'itimer-timer))
		  (funcall itimer-driver-start))
		;; Check whether there is a bug to which the difference of
		;; the present time and the time when the itimer driver was
		;; woken up is subtracted from the initial itimer value.
		(let* ((inhibit-quit t)
		       (ctime (current-time))
		       (itimer-timer-last-wakeup
			(prog1
			    ctime
			  (setcar ctime (1- (car ctime)))))
		       (itimer-list nil)
		       (itimer (funcall start-itimer "pgg-run-at-time"
					'ignore 5)))
		  (sleep-for 0.1) ;; Accept the timeout interrupt.
		  (prog1
		      (> (funcall itimer-value itimer) 0)
		    (funcall delete-itimer itimer))))
	    (error nil))
	  `(let ((time ,time))
	     (apply #'start-itimer "pgg-run-at-time"
		    ,function (if time (max time 1e-9) 1e-9)
		    ,repeat nil t ,args)))
      `(let ((time ,time)
	     (itimers (list nil)))
	 (setcar
	  itimers
	  (apply #'start-itimer "pgg-run-at-time"
		 (lambda (itimers repeat function &rest args)
		   (let ((itimer (car itimers)))
		     (if repeat
			 (progn
			   (set-itimer-function
			    itimer
			    (lambda (itimer repeat function &rest args)
			      (set-itimer-restart itimer repeat)
			      (set-itimer-function itimer function)
			      (set-itimer-function-arguments itimer args)
			      (apply function args)))
			   (set-itimer-function-arguments
			    itimer
			    (append (list itimer repeat function) args)))
		       (set-itimer-function
			itimer
			(lambda (itimer function &rest args)
			  (delete-itimer itimer)
			  (apply function args)))
		       (set-itimer-function-arguments
			itimer
			(append (list itimer function) args)))))
		 1e-9 (if time (max time 1e-9) 1e-9)
		 nil t itimers ,repeat ,function ,args))))))

(eval-and-compile
  (if (featurep 'xemacs)
      (defun pgg-run-at-time (time repeat function &rest args)
	"Emulating function run as `run-at-time'.
TIME should be nil meaning now, or a number of seconds from now.
Return an itimer object which can be used in either `delete-itimer'
or `cancel-timer'."
	(pgg-run-at-time-1 time repeat function args))
    (defalias 'pgg-run-at-time 'run-at-time)))

(defun pgg-add-passphrase-cache (key passphrase)
  (setq key (pgg-truncate-key-identifier key))
  (set (intern key pgg-passphrase-cache)
       passphrase)
  (pgg-run-at-time pgg-passphrase-cache-expiry nil
		   #'pgg-remove-passphrase-cache
		   key))

(defun pgg-remove-passphrase-cache (key)
  (let ((passphrase (symbol-value (intern-soft key pgg-passphrase-cache))))
    (when passphrase
      (fillarray passphrase ?_)
      (unintern key pgg-passphrase-cache))))

(defmacro pgg-convert-lbt-region (start end lbt)
  `(let ((pgg-conversion-end (set-marker (make-marker) ,end)))
     (goto-char ,start)
     (case ,lbt
       (CRLF
	(while (progn
		 (end-of-line)
		 (> (marker-position pgg-conversion-end) (point)))
	  (insert "\r")
	  (forward-line 1)))
       (LF
	(while (re-search-forward "\r$" pgg-conversion-end t)
	  (replace-match ""))))))

(put 'pgg-as-lbt 'lisp-indent-function 3)

(defmacro pgg-as-lbt (start end lbt &rest body)
  `(let ((inhibit-read-only t)
	 buffer-read-only
	 buffer-undo-list)
     (pgg-convert-lbt-region ,start ,end ,lbt)
     (let ((,end (point)))
       ,@body)
     (push nil buffer-undo-list)
     (ignore-errors (undo))))

(put 'pgg-process-when-success 'lisp-indent-function 0)

(defmacro pgg-process-when-success (&rest body)
  `(with-current-buffer pgg-output-buffer
     (if (zerop (buffer-size)) nil ,@body t)))

(defalias 'pgg-make-temp-file
  (if (fboundp 'make-temp-file)
      'make-temp-file
    (lambda (prefix &optional dir-flag)
      (let ((file (expand-file-name
		   (make-temp-name prefix)
		   (if (fboundp 'temp-directory)
		       (temp-directory)
		     temporary-file-directory))))
	(if dir-flag
	    (make-directory file))
	file))))

;;; @ interface functions
;;;

;;;###autoload
(defun pgg-encrypt-region (start end rcpts &optional sign)
  "Encrypt the current region between START and END for RCPTS.
If optional argument SIGN is non-nil, do a combined sign and encrypt."
  (interactive
   (list (region-beginning)(region-end)
	 (split-string (read-string "Recipients: ") "[ \t,]+")))
  (let ((status
	 (pgg-save-coding-system start end
	   (pgg-invoke "encrypt-region" (or pgg-scheme pgg-default-scheme)
		       (point-min) (point-max) rcpts sign))))
    (when (interactive-p)
      (pgg-display-output-buffer start end status))
    status))

;;;###autoload
(defun pgg-encrypt (rcpts &optional sign start end)
  "Encrypt the current buffer for RCPTS.
If optional argument SIGN is non-nil, do a combined sign and encrypt.
If optional arguments START and END are specified, only encrypt within
the region."
  (interactive (list (split-string (read-string "Recipients: ") "[ \t,]+")))
  (let* ((start (or start (point-min)))
	 (end (or end (point-max)))
	 (status (pgg-encrypt-region start end rcpts sign)))
    (when (interactive-p)
      (pgg-display-output-buffer start end status))
    status))

;;;###autoload
(defun pgg-decrypt-region (start end)
  "Decrypt the current region between START and END."
  (interactive "r")
  (let* ((buf (current-buffer))
	 (status
	  (pgg-save-coding-system start end
	    (pgg-invoke "decrypt-region" (or pgg-scheme pgg-default-scheme)
			(point-min) (point-max)))))
    (when (interactive-p)
      (pgg-display-output-buffer start end status))
    status))

;;;###autoload
(defun pgg-decrypt (&optional start end)
  "Decrypt the current buffer.
If optional arguments START and END are specified, only decrypt within
the region."
  (interactive "")
  (let* ((start (or start (point-min)))
	 (end (or end (point-max)))
	 (status (pgg-decrypt-region start end)))
    (when (interactive-p)
      (pgg-display-output-buffer start end status))
    status))

;;;###autoload
(defun pgg-sign-region (start end &optional cleartext)
  "Make the signature from text between START and END.
If the optional 3rd argument CLEARTEXT is non-nil, it does not create
a detached signature.
If this function is called interactively, CLEARTEXT is enabled
and the the output is displayed."
  (interactive "r")
  (let ((status (pgg-save-coding-system start end
		  (pgg-invoke "sign-region" (or pgg-scheme pgg-default-scheme)
			      (point-min) (point-max)
			      (or (interactive-p) cleartext)))))
    (when (interactive-p)
      (pgg-display-output-buffer start end status))
    status))

;;;###autoload
(defun pgg-sign (&optional cleartext start end)
  "Sign the current buffer.
If the optional argument CLEARTEXT is non-nil, it does not create a
detached signature.
If optional arguments START and END are specified, only sign data
within the region.
If this function is called interactively, CLEARTEXT is enabled
and the the output is displayed."
  (interactive "")
  (let* ((start (or start (point-min)))
	 (end (or end (point-max)))
	 (status (pgg-sign-region start end (or (interactive-p) cleartext))))
    (when (interactive-p)
      (pgg-display-output-buffer start end status))
    status))
  
;;;###autoload
(defun pgg-verify-region (start end &optional signature fetch)
  "Verify the current region between START and END.
If the optional 3rd argument SIGNATURE is non-nil, it is treated as
the detached signature of the current region.

If the optional 4th argument FETCH is non-nil, we attempt to fetch the
signer's public key from `pgg-default-keyserver-address'."
  (interactive "r")
  (let* ((packet
	  (if (null signature) nil
	    (with-temp-buffer
	      (buffer-disable-undo)
	      (if (fboundp 'set-buffer-multibyte)
		  (set-buffer-multibyte nil))
	      (insert-file-contents signature)
	      (cdr (assq 2 (pgg-decode-armor-region
			    (point-min)(point-max)))))))
	 (key (cdr (assq 'key-identifier packet)))
	 status keyserver)
    (and (stringp key)
	 pgg-query-keyserver
	 (setq key (concat "0x" (pgg-truncate-key-identifier key)))
	 (null (pgg-lookup-key key))
	 (or fetch (interactive-p))
	 (y-or-n-p (format "Key %s not found; attempt to fetch? " key))
	 (setq keyserver
	       (or (cdr (assq 'preferred-key-server packet))
		   pgg-default-keyserver-address))
	 (pgg-fetch-key keyserver key))
    (setq status 
	  (pgg-save-coding-system start end
	    (pgg-invoke "verify-region" (or pgg-scheme pgg-default-scheme)
			(point-min) (point-max) signature)))
    (when (interactive-p)
      (let ((temp-buffer-show-function
	     (function pgg-temp-buffer-show-function)))
	(with-output-to-temp-buffer pgg-echo-buffer
	  (set-buffer standard-output)
	  (insert-buffer-substring (if status pgg-output-buffer
				     pgg-errors-buffer)))))
    status))

;;;###autoload
(defun pgg-verify (&optional signature fetch start end)
  "Verify the current buffer.
If the optional argument SIGNATURE is non-nil, it is treated as
the detached signature of the current region.
If the optional argument FETCH is non-nil, we attempt to fetch the
signer's public key from `pgg-default-keyserver-address'.
If optional arguments START and END are specified, only verify data
within the region."
  (interactive "")
  (let* ((start (or start (point-min)))
	 (end (or end (point-max)))
	 (status (pgg-verify-region start end signature fetch)))
    (when (interactive-p)
      (let ((temp-buffer-show-function
	     (function pgg-temp-buffer-show-function)))
	(with-output-to-temp-buffer pgg-echo-buffer
	  (set-buffer standard-output)
	  (insert-buffer-substring (if status pgg-output-buffer
				     pgg-errors-buffer)))))
    status))

;;;###autoload
(defun pgg-insert-key ()
  "Insert the ASCII armored public key."
  (interactive)
  (pgg-invoke "insert-key" (or pgg-scheme pgg-default-scheme)))

;;;###autoload
(defun pgg-snarf-keys-region (start end)
  "Import public keys in the current region between START and END."
  (interactive "r")
  (pgg-save-coding-system start end
    (pgg-invoke "snarf-keys-region" (or pgg-scheme pgg-default-scheme)
		start end)))

;;;###autoload
(defun pgg-snarf-keys ()
  "Import public keys in the current buffer."
  (interactive "")
  (pgg-snarf-keys-region (point-min) (point-max)))

(defun pgg-lookup-key (string &optional type)
  (pgg-invoke "lookup-key" (or pgg-scheme pgg-default-scheme) string type))

(defvar pgg-insert-url-function  (function pgg-insert-url-with-w3))

(defun pgg-insert-url-with-w3 (url)
  (ignore-errors
    (require 'url)
    (let (buffer-file-name)
      (url-insert-file-contents url))))

(defvar pgg-insert-url-extra-arguments nil)
(defvar pgg-insert-url-program nil)

(defun pgg-insert-url-with-program (url)
  (let ((args (copy-sequence pgg-insert-url-extra-arguments))
	process)
    (insert
     (with-temp-buffer
       (setq process
	     (apply #'start-process " *PGG url*" (current-buffer)
		    pgg-insert-url-program (nconc args (list url))))
       (set-process-sentinel process #'ignore)
       (while (eq 'run (process-status process))
	 (accept-process-output process 5))
       (delete-process process)
       (if (and process (eq 'run (process-status process)))
	   (interrupt-process process))
       (buffer-string)))))

(defun pgg-fetch-key (keyserver key)
  "Attempt to fetch a KEY from KEYSERVER for addition to PGP or GnuPG keyring."
  (with-current-buffer (get-buffer-create pgg-output-buffer)
    (buffer-disable-undo)
    (erase-buffer)
    (let ((proto (if (string-match "^[a-zA-Z\\+\\.\\\\-]+:" keyserver)
		     (substring keyserver 0 (1- (match-end 0))))))
      (save-excursion
	(funcall pgg-insert-url-function
		 (if proto keyserver
		   (format "http://%s:11371/pks/lookup?op=get&search=%s"
			   keyserver key))))
      (when (re-search-forward "^-+BEGIN" nil 'last)
	(delete-region (point-min) (match-beginning 0))
	(when (re-search-forward "^-+END" nil t)
	  (delete-region (progn (end-of-line) (point))
			 (point-max)))
	(insert "\n")
	(with-temp-buffer
	  (insert-buffer-substring pgg-output-buffer)
	  (pgg-snarf-keys-region (point-min)(point-max)))))))


(provide 'pgg)

;;; arch-tag: 9cc705dd-1e6a-4c90-8dce-c3561f9a2cf4
;;; pgg.el ends here
