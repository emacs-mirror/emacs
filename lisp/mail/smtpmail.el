;; Simple SMTP protocol (RFC 821) for sending mail

;; Copyright (C) 1995, 1996 Free Software Foundation, Inc.

;; Author: Tomoji Kagatani <kagatani@rbc.ncl.omron.co.jp>
;; Maintainer: Brian D. Carlstrom <bdc@ai.mit.edu>
;; Keywords: mail

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Send Mail to smtp host from smtpmail temp buffer.

;; Please add these lines in your .emacs(_emacs).
;;
;;(setq send-mail-function 'smtpmail-send-it)
;;(setq smtpmail-default-smtp-server "YOUR SMTP HOST")
;;(setq smtpmail-smtp-service "smtp")
;;(setq smtpmail-local-domain "YOUR DOMAIN NAME")
;;(setq smtpmail-debug-info t)
;;(load-library "smtpmail")
;;(setq smtpmail-code-conv-from nil)
;;(setq user-full-name "YOUR NAME HERE")

;;; Code:

(require 'sendmail)

;;;
(defvar smtpmail-default-smtp-server nil
  "*Specify default SMTP server.")

(defvar smtpmail-smtp-server 
  (or (getenv "SMTPSERVER") smtpmail-default-smtp-server)
  "*The name of the host running SMTP server.")

(defvar smtpmail-smtp-service 25
  "*SMTP service port number. smtp or 25 .")

(defvar smtpmail-local-domain nil
  "*Local domain name without a host name.
If the function (system-name) returns the full internet address,
don't define this value.")

(defvar smtpmail-debug-info nil
  "*smtpmail debug info printout. messages and process buffer.")

(defvar smtpmail-code-conv-from nil ;; *junet*
  "*smtpmail code convert from this code to *internal*..for tiny-mime..")

;;;
;;;
;;;

(defun smtpmail-send-it ()
  (require 'mail-utils)
  (let ((errbuf (if mail-interactive
		    (generate-new-buffer " smtpmail errors")
		  0))
	(tembuf (generate-new-buffer " smtpmail temp"))
	(case-fold-search nil)
	resend-to-addresses
	delimline
	(mailbuf (current-buffer)))
    (unwind-protect
	(save-excursion
	  (set-buffer tembuf)
	  (erase-buffer)
	  (insert-buffer-substring mailbuf)
	  (goto-char (point-max))
	  ;; require one newline at the end.
	  (or (= (preceding-char) ?\n)
	      (insert ?\n))
	  ;; Change header-delimiter to be what sendmail expects.
	  (goto-char (point-min))
	  (re-search-forward
	    (concat "^" (regexp-quote mail-header-separator) "\n"))
	  (replace-match "\n")
	  (backward-char 1)
	  (setq delimline (point-marker))
;;	  (sendmail-synch-aliases)
	  (if mail-aliases
	      (expand-mail-aliases (point-min) delimline))
	  (goto-char (point-min))
	  ;; ignore any blank lines in the header
	  (while (and (re-search-forward "\n\n\n*" delimline t)
		      (< (point) delimline))
	    (replace-match "\n"))
	  (let ((case-fold-search t))
	    (goto-char (point-min))
	    (goto-char (point-min))
	    (while (re-search-forward "^Resent-to:" delimline t)
	      (setq resend-to-addresses
		    (save-restriction
		      (narrow-to-region (point)
					(save-excursion
					  (end-of-line)
					  (point)))
		      (append (mail-parse-comma-list)
			      resend-to-addresses))))
;;; Apparently this causes a duplicate Sender.
;;;	    ;; If the From is different than current user, insert Sender.
;;;	    (goto-char (point-min))
;;;	    (and (re-search-forward "^From:"  delimline t)
;;;		 (progn
;;;		   (require 'mail-utils)
;;;		   (not (string-equal
;;;			 (mail-strip-quoted-names
;;;			  (save-restriction
;;;			    (narrow-to-region (point-min) delimline)
;;;			    (mail-fetch-field "From")))
;;;			 (user-login-name))))
;;;		 (progn
;;;		   (forward-line 1)
;;;		   (insert "Sender: " (user-login-name) "\n")))
	    ;; Don't send out a blank subject line
	    (goto-char (point-min))
	    (if (re-search-forward "^Subject:[ \t]*\n" delimline t)
		(replace-match ""))
	    ;; Put the "From:" field in unless for some odd reason
	    ;; they put one in themselves.
	    (goto-char (point-min))
	    (if (not (re-search-forward "^From:" delimline t))
		(let* ((login user-mail-address)
		       (fullname (user-full-name)))
		  (cond ((eq mail-from-style 'angles)
			 (insert "From: " fullname)
			 (let ((fullname-start (+ (point-min) 6))
			       (fullname-end (point-marker)))
			   (goto-char fullname-start)
			   ;; Look for a character that cannot appear unquoted
			   ;; according to RFC 822.
			   (if (re-search-forward "[^- !#-'*+/-9=?A-Z^-~]"
						  fullname-end 1)
			       (progn
				 ;; Quote fullname, escaping specials.
				 (goto-char fullname-start)
				 (insert "\"")
				 (while (re-search-forward "[\"\\]"
							   fullname-end 1)
				   (replace-match "\\\\\\&" t))
				 (insert "\""))))
			 (insert " <" login ">\n"))
			((eq mail-from-style 'parens)
			 (insert "From: " login " (")
			 (let ((fullname-start (point)))
			   (insert fullname)
			   (let ((fullname-end (point-marker)))
			     (goto-char fullname-start)
			     ;; RFC 822 says \ and nonmatching parentheses
			     ;; must be escaped in comments.
			     ;; Escape every instance of ()\ ...
			     (while (re-search-forward "[()\\]" fullname-end 1)
			       (replace-match "\\\\\\&" t))
			     ;; ... then undo escaping of matching parentheses,
			     ;; including matching nested parentheses.
			     (goto-char fullname-start)
			     (while (re-search-forward 
				     "\\(\\=\\|[^\\]\\(\\\\\\\\\\)*\\)\\\\(\\(\\([^\\]\\|\\\\\\\\\\)*\\)\\\\)"
				     fullname-end 1)
			       (replace-match "\\1(\\3)" t)
			       (goto-char fullname-start))))
			 (insert ")\n"))
			((null mail-from-style)
			 (insert "From: " login "\n")))))
	    ;; Insert an extra newline if we need it to work around
	    ;; Sun's bug that swallows newlines.
	    (goto-char (1+ delimline))
	    (if (eval mail-mailer-swallows-blank-line)
		(newline))
	    ;; Find and handle any FCC fields.
	    (goto-char (point-min))
	    (if (re-search-forward "^FCC:" delimline t)
		(mail-do-fcc delimline))
	    (if mail-interactive
		(save-excursion
		  (set-buffer errbuf)
		  (erase-buffer))))
	  ;;
	  ;;
	  ;;
	  (setq smtpmail-address-buffer (generate-new-buffer "*smtp-mail*"))
	  (setq smtpmail-recipient-address-list
		(or resend-to-addresses
		    (smtpmail-deduce-address-list tembuf (point-min) delimline)))
	  (kill-buffer smtpmail-address-buffer)

	  (smtpmail-do-bcc delimline)

	  (if (not (null smtpmail-recipient-address-list))
	      (if (not (smtpmail-via-smtp smtpmail-recipient-address-list tembuf))
		  (error "Sending failed; SMTP protocol error"))
	    (error "Sending failed; no recipients"))
	  )
      (kill-buffer tembuf)
      (if (bufferp errbuf)
	  (kill-buffer errbuf)))))


;(defun smtpmail-via-smtp (host,port,sender,destination,smtpmail-text-buffer)

(defun smtpmail-fqdn ()
  (if smtpmail-local-domain
      (concat (system-name) "." smtpmail-local-domain)
    (system-name)))

(defun smtpmail-via-smtp (recipient smtpmail-text-buffer)
  (let ((process nil)
	(host smtpmail-smtp-server)
	(port smtpmail-smtp-service)
	response-code
	greeting
	process-buffer)
    (unwind-protect
	(catch 'done
	  ;; get or create the trace buffer
	  (setq process-buffer
		(get-buffer-create (format "*trace of SMTP session to %s*" host)))

	  ;; clear the trace buffer of old output
	  (save-excursion
	    (set-buffer process-buffer)
	    (erase-buffer))

	  ;; open the connection to the server
	  (setq process (open-network-stream "SMTP" process-buffer host port))
	  (and (null process) (throw 'done nil))

	  ;; set the send-filter
	  (set-process-filter process 'smtpmail-process-filter)

	  (save-excursion
	    (set-buffer process-buffer)
	    (make-local-variable 'smtpmail-read-point)
	    (setq smtpmail-read-point (point-min))

	    
	    (if (or (null (car (setq greeting (smtpmail-read-response process))))
		    (not (integerp (car greeting)))
		    (>= (car greeting) 400))
		(throw 'done nil)
	      )

	    ;; HELO
	    (smtpmail-send-command process (format "HELO %s" (smtpmail-fqdn)))

	    (if (or (null (car (setq response-code (smtpmail-read-response process))))
		    (not (integerp (car response-code)))
		    (>= (car response-code) 400))
		(throw 'done nil)
	      )

	    ;; MAIL FROM: <sender>
;	    (smtpmail-send-command process (format "MAIL FROM:%s@%s" (user-login-name) (smtpmail-fqdn)))
	    (smtpmail-send-command process (format "MAIL FROM: <%s>" user-mail-address))

	    (if (or (null (car (setq response-code (smtpmail-read-response process))))
		    (not (integerp (car response-code)))
		    (>= (car response-code) 400))
		(throw 'done nil)
	      )
	    
	    ;; RCPT TO: <recipient>
	    (let ((n 0))
	      (while (not (null (nth n recipient)))
		(smtpmail-send-command process (format "RCPT TO: <%s>" (nth n recipient)))
		(setq n (1+ n))

		(if (or (null (car (setq response-code (smtpmail-read-response process))))
			(not (integerp (car response-code)))
			(>= (car response-code) 400))
		    (throw 'done nil)
		  )
		))
	    
	    ;; DATA
	    (smtpmail-send-command process "DATA")

	    (if (or (null (car (setq response-code (smtpmail-read-response process))))
		    (not (integerp (car response-code)))
		    (>= (car response-code) 400))
		(throw 'done nil)
	      )

	    ;; Mail contents
	    (smtpmail-send-data process smtpmail-text-buffer)

	    ;;DATA end "."
	    (smtpmail-send-command process ".")

	    (if (or (null (car (setq response-code (smtpmail-read-response process))))
		    (not (integerp (car response-code)))
		    (>= (car response-code) 400))
		(throw 'done nil)
	      )

	    ;;QUIT
;	    (smtpmail-send-command process "QUIT")
;	    (and (null (car (smtpmail-read-response process)))
;		 (throw 'done nil))
	    t ))
      (if process
	  (save-excursion
	    (set-buffer (process-buffer process))
	    (smtpmail-send-command process "QUIT")
	    (smtpmail-read-response process)

;	    (if (or (null (car (setq response-code (smtpmail-read-response process))))
;		    (not (integerp (car response-code)))
;		    (>= (car response-code) 400))
;		(throw 'done nil)
;	      )
	    (delete-process process))))))


(defun smtpmail-process-filter (process output)
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert output)))

(defun smtpmail-read-response (process)
  (let ((case-fold-search nil)
	(response-string nil)
	(response-continue t)
	(return-value '(nil ""))
	match-end)

;    (setq response-string nil)
;    (setq response-continue t)
;    (setq return-value '(nil ""))

    (while response-continue
      (goto-char smtpmail-read-point)
      (while (not (search-forward "\r\n" nil t))
	(accept-process-output process)
	(goto-char smtpmail-read-point))

      (setq match-end (point))
      (if (null response-string)
	  (setq response-string
		(buffer-substring smtpmail-read-point (- match-end 2))))
	
      (goto-char smtpmail-read-point)
      (if (looking-at "[0-9]+ ")
	  (progn (setq response-continue nil)
;		 (setq return-value response-string)

		 (if smtpmail-debug-info
		     (message response-string))

		 (setq smtpmail-read-point match-end)
		 (setq return-value
		       (cons (string-to-int 
			      (buffer-substring (match-beginning 0) (match-end 0))) 
			     response-string)))
	
	(if (looking-at "[0-9]+-")
	    (progn (setq smtpmail-read-point match-end)
		   (setq response-continue t))
	  (progn
	    (setq smtpmail-read-point match-end)
	    (setq response-continue nil)
	    (setq return-value 
		  (cons nil response-string))
	    )
	  )))
    (setq smtpmail-read-point match-end)
    return-value))


(defun smtpmail-send-command (process command)
  (goto-char (point-max))
  (if (= (aref command 0) ?P)
      (insert "PASS <omitted>\r\n")
    (insert command "\r\n"))
  (setq smtpmail-read-point (point))
  (process-send-string process command)
  (process-send-string process "\r\n"))

(defun smtpmail-send-data-1 (process data)
  (goto-char (point-max))

  (if (not (null smtpmail-code-conv-from))
      (setq data (code-convert-string data smtpmail-code-conv-from *internal*)))
	
  (if smtpmail-debug-info
      (insert data "\r\n"))

  (setq smtpmail-read-point (point))
  ;; Escape "." at start of a line
  (if (eq (string-to-char data) ?.)
      (process-send-string process "."))
  (process-send-string process data)
  (process-send-string process "\r\n")
  )

(defun smtpmail-send-data (process buffer)
  (let
      ((data-continue t)
       (sending-data nil)
       this-line
       this-line-end)

    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min)))

    (while data-continue
      (save-excursion
	(set-buffer buffer)
	(beginning-of-line)
	(setq this-line (point))
	(end-of-line)
	(setq this-line-end (point))
	(setq sending-data nil)
	(setq sending-data (buffer-substring this-line this-line-end))
	(if (/= (forward-line 1) 0)
	    (setq data-continue nil)))

      (smtpmail-send-data-1 process sending-data)
      )
    )
  )
    

(defun smtpmail-deduce-address-list (smtpmail-text-buffer header-start header-end)
  "Get address list suitable for smtp RCPT TO: <address>."
  (require 'mail-utils)  ;; pick up mail-strip-quoted-names
  (let
      ((case-fold-search t)
       (simple-address-list "")
       this-line
       this-line-end
       addr-regexp)
    
    (unwind-protect
	(save-excursion
	  ;;
	  (set-buffer smtpmail-address-buffer) (erase-buffer)
	  (insert-buffer-substring smtpmail-text-buffer header-start header-end)
	  (goto-char (point-min))
	  ;; RESENT-* fields should stop processing of regular fields.
	  (save-excursion
	    (if (re-search-forward "^RESENT-TO:" header-end t)
		(setq addr-regexp "^\\(RESENT-TO:\\|RESENT-CC:\\|RESENT-BCC:\\)")
	      (setq addr-regexp  "^\\(TO:\\|CC:\\|BCC:\\)")))

	  (while (re-search-forward addr-regexp header-end t)
	    (replace-match "")
	    (setq this-line (match-beginning 0))
	    (forward-line 1)
	    ;; get any continuation lines
	    (while (and (looking-at "^[ \t]+") (< (point) header-end))
	      (forward-line 1))
	    (setq this-line-end (point-marker))
	    (setq simple-address-list
		  (concat simple-address-list " "
			  (mail-strip-quoted-names (buffer-substring this-line this-line-end))))
	    )
	  (erase-buffer)
	  (insert-string " ")
	  (insert-string simple-address-list)
	  (insert-string "\n")
	  (subst-char-in-region (point-min) (point-max) 10 ?  t);; newline --> blank
	  (subst-char-in-region (point-min) (point-max) ?, ?  t);; comma   --> blank
	  (subst-char-in-region (point-min) (point-max)  9 ?  t);; tab     --> blank

	  (goto-char (point-min))
	  ;; tidyness in case hook is not robust when it looks at this
	  (while (re-search-forward "[ \t]+" header-end t) (replace-match " "))

	  (goto-char (point-min))
	  (let (recipient-address-list)
	    (while (re-search-forward " \\([^ ]+\\) " (point-max) t)
	      (backward-char 1)
	      (setq recipient-address-list (cons (buffer-substring (match-beginning 1) (match-end 1))
						 recipient-address-list))
	      )
	    (setq smtpmail-recipient-address-list recipient-address-list))

	  )
      )
    )
  )


(defun smtpmail-do-bcc (header-end)
  "Delete BCC: and their continuation lines from the header area.
There may be multiple BCC: lines, and each may have arbitrarily
many continuation lines."
  (let ((case-fold-search t))
	(save-excursion (goto-char (point-min))
	  ;; iterate over all BCC: lines
	  (while (re-search-forward "^BCC:" header-end t)
	        (delete-region (match-beginning 0) (progn (forward-line 1) (point)))
		;; get rid of any continuation lines
		(while (and (looking-at "^[ \t].*\n") (< (point) header-end))
		  (replace-match ""))
		)
	  ) ;; save-excursion
	) ;; let
  )



(provide 'smtpmail)

;; smtpmail.el ends here
