;;; mail-source.el --- functions for fetching mail
;; Copyright (C) 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news, mail

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

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'imap)
  (eval-when-compile (defvar display-time-mail-function)))
(eval-and-compile
  (autoload 'pop3-movemail "pop3")
  (autoload 'pop3-get-message-count "pop3")
  (autoload 'nnheader-cancel-timer "nnheader"))
(require 'format-spec)
(require 'mm-util)

(defgroup mail-source nil
  "The mail-fetching library."
  :version "21.1"
  :group 'gnus)

;; Define these at compile time to avoid dragging in imap always.
(defconst mail-source-imap-authenticators
  (eval-when-compile
    (mapcar (lambda (a)
	      (list 'const (car a)))
     imap-authenticator-alist)))
(defconst mail-source-imap-streams
  (eval-when-compile
    (mapcar (lambda (a)
	      (list 'const (car a)))
     imap-stream-alist)))

(defcustom mail-sources nil
  "*Where the mail backends will look for incoming mail.
This variable is a list of mail source specifiers.
See Info node `(gnus)Mail Source Specifiers'."
  :group 'mail-source
  :type `(repeat
	  (choice :format "%[Value Menu%] %v"
		  :value (file)
		  (cons :tag "Spool file"
			(const :format "" file)
			(checklist :tag "Options" :greedy t
				   (group :inline t
					  (const :format "" :value :path)
					  file)))
		  (cons :tag "Several files in a directory"
			(const :format "" directory)
			(checklist :tag "Options" :greedy t
				   (group :inline t
					  (const :format "" :value :path)
					  (directory :tag "Path"))
				   (group :inline t
					  (const :format "" :value :suffix)
					  (string :tag "Suffix"))
				   (group :inline t
					  (const :format "" :value :predicate)
					  (function :tag "Predicate"))
				   (group :inline t
					  (const :format "" :value :prescript)
					  (string :tag "Prescript"))
				   (group :inline t
					  (const :format "" :value :postscript)
					  (string :tag "Postscript"))
				   (group :inline t
					  (const :format "" :value :plugged)
					  (boolean :tag "Plugged"))))
		  (cons :tag "POP3 server"
			(const :format "" pop)
			(checklist :tag "Options" :greedy t
				   (group :inline t
					  (const :format "" :value :server) 
					  (string :tag "Server"))
				   (group :inline t
					  (const :format "" :value :port) 
					  (choice :tag "Port"
						  :value "pop3" 
						  (number :format "%v")
						  (string :format "%v")))
				   (group :inline t
					  (const :format "" :value :user)
					  (string :tag "User"))
				   (group :inline t
					  (const :format "" :value :password)
					  (string :tag "Password"))
				   (group :inline t
					  (const :format "" :value :program)
					  (string :tag "Program"))
				   (group :inline t
					  (const :format "" :value :prescript)
					  (string :tag "Prescript"))
				   (group :inline t
					  (const :format "" :value :postscript)
					  (string :tag "Postscript"))
				   (group :inline t
					  (const :format "" :value :function)
					  (function :tag "Function"))
				   (group :inline t
					  (const :format "" 
						 :value :authentication)
					  (choice :tag "Authentication"
						  :value apop
						  (const password)
						  (const apop)))
				   (group :inline t
					  (const :format "" :value :plugged)
					  (boolean :tag "Plugged"))))
		  (cons :tag "Maildir (qmail, postfix...)"
			(const :format "" maildir)
			(checklist :tag "Options" :greedy t
				   (group :inline t
					  (const :format "" :value :path)
					  (directory :tag "Path"))
				   (group :inline t
					  (const :format "" :value :plugged)
					  (boolean :tag "Plugged"))))
		  (cons :tag "IMAP server"
			(const :format "" imap)
			(checklist :tag "Options" :greedy t
				   (group :inline t
					  (const :format "" :value :server)
					  (string :tag "Server"))
				   (group :inline t
					  (const :format "" :value :port)
					  (choice :tag "Port" 
						  :value 143 
						  number string))
				   (group :inline t
					  (const :format "" :value :user)
					  (string :tag "User"))
				   (group :inline t
					  (const :format "" :value :password)
					  (string :tag "Password"))
				   (group :inline t
					  (const :format "" :value :stream)
					  (choice :tag "Stream"
						  :value network
						  ,@mail-source-imap-streams))
				   (group :inline t
					  (const :format ""
						 :value :authenticator)
					  (choice :tag "Authenticator"
						  :value login
						  ,@mail-source-imap-authenticators))
				   (group :inline t
					  (const :format "" :value :mailbox)
					  (string :tag "Mailbox"
						  :value "INBOX"))
				   (group :inline t
					  (const :format "" :value :predicate)
					  (string :tag "Predicate" 
						  :value "UNSEEN UNDELETED"))
				   (group :inline t
					  (const :format "" :value :fetchflag)
					  (string :tag "Fetchflag"
						  :value  "\\Deleted"))
				   (group :inline t
					  (const :format ""
						 :value :dontexpunge)
					  (boolean :tag "Dontexpunge"))
				   (group :inline t
					  (const :format "" :value :plugged)
					  (boolean :tag "Plugged"))))
		  (cons :tag "Webmail server"
			(const :format "" webmail)
			(checklist :tag "Options" :greedy t
				   (group :inline t 
					 (const :format "" :value :subtype)
					 ;; Should be generated from
					 ;; `webmail-type-definition', but we
					 ;; can't require webmail without W3.
					 (choice :tag "Subtype"
						 :value hotmail
						 (const hotmail)
						 (const yahoo)
						 (const netaddress)
						 (const netscape)
						 (const my-deja)))
				   (group :inline t
					  (const :format "" :value :user)
					  (string :tag "User"))
				   (group :inline t
					  (const :format "" :value :password)
					  (string :tag "Password"))
				   (group :inline t
					  (const :format ""
						 :value :dontexpunge)
					  (boolean :tag "Dontexpunge"))
				   (group :inline t
					  (const :format "" :value :plugged)
					  (boolean :tag "Plugged")))))))

(defcustom mail-source-primary-source nil
  "*Primary source for incoming mail.
If non-nil, this maildrop will be checked periodically for new mail."
  :group 'mail-source
  :type 'sexp)

(defcustom mail-source-crash-box "~/.emacs-mail-crash-box"
  "File where mail will be stored while processing it."
  :group 'mail-source
  :type 'file)

(defcustom mail-source-directory "~/Mail/"
  "Directory where files (if any) will be stored."
  :group 'mail-source
  :type 'directory)

(defcustom mail-source-default-file-modes 384
  "Set the mode bits of all new mail files to this integer."
  :group 'mail-source
  :type 'integer)

(defcustom mail-source-delete-incoming t
  "*If non-nil, delete incoming files after handling."
  :group 'mail-source
  :type 'boolean)

(defcustom mail-source-incoming-file-prefix "Incoming"
  "Prefix for file name for storing incoming mail"
  :group 'mail-source
  :type 'string)

(defcustom mail-source-report-new-mail-interval 5
  "Interval in minutes between checks for new mail."
  :group 'mail-source
  :type 'number)

(defcustom mail-source-idle-time-delay 5
  "Number of idle seconds to wait before checking for new mail."
  :group 'mail-source
  :type 'number)

;;; Internal variables.

(defvar mail-source-string ""
  "A dynamically bound string that says what the current mail source is.")

(defvar mail-source-new-mail-available nil
  "Flag indicating when new mail is available.")

(eval-and-compile
  (defvar mail-source-common-keyword-map
    '((:plugged))
    "Mapping from keywords to default values.
Common keywords should be listed here.")

  (defvar mail-source-keyword-map
    '((file
       (:prescript)
       (:prescript-delay)
       (:postscript)
       (:path (or (getenv "MAIL")
		  (expand-file-name (user-login-name) rmail-spool-directory))))
      (directory
       (:prescript)
       (:prescript-delay)
       (:postscript)
       (:path)
       (:suffix ".spool")
       (:predicate identity))
      (pop
       (:prescript)
       (:prescript-delay)
       (:postscript)
       (:server (getenv "MAILHOST"))
       (:port 110)
       (:user (or (user-login-name) (getenv "LOGNAME") (getenv "USER")))
       (:program)
       (:function)
       (:password)
       (:authentication password))
      (maildir
       (:path (or (getenv "MAILDIR") "~/Maildir/"))
       (:subdirs ("new" "cur"))
       (:function))
      (imap
       (:server (getenv "MAILHOST"))
       (:port)
       (:stream)
       (:authentication)
       (:user (or (user-login-name) (getenv "LOGNAME") (getenv "USER")))
       (:password)
       (:mailbox "INBOX")
       (:predicate "UNSEEN UNDELETED")
       (:fetchflag "\\Deleted")
       (:dontexpunge))
      (webmail
       (:subtype hotmail)
       (:user (or (user-login-name) (getenv "LOGNAME") (getenv "USER")))
       (:password)
       (:dontexpunge)
       (:authentication password)))
    "Mapping from keywords to default values.
All keywords that can be used must be listed here."))

(defvar mail-source-fetcher-alist
  '((file mail-source-fetch-file)
    (directory mail-source-fetch-directory)
    (pop mail-source-fetch-pop)
    (maildir mail-source-fetch-maildir)
    (imap mail-source-fetch-imap)
    (webmail mail-source-fetch-webmail))
  "A mapping from source type to fetcher function.")

(defvar mail-source-password-cache nil)

(defvar mail-source-plugged t)

;;; Functions

(eval-and-compile
  (defun mail-source-strip-keyword (keyword)
    "Strip the leading colon off the KEYWORD."
    (intern (substring (symbol-name keyword) 1))))

(eval-and-compile
  (defun mail-source-bind-1 (type)
    (let* ((defaults (cdr (assq type mail-source-keyword-map)))
	   default bind)
      (while (setq default (pop defaults))
	(push (list (mail-source-strip-keyword (car default))
		    nil)
	      bind))
      bind)))

(defmacro mail-source-bind (type-source &rest body)
  "Return a `let' form that binds all variables in source TYPE.
TYPE-SOURCE is a list where the first element is the TYPE, and
the second variable is the SOURCE.
At run time, the mail source specifier SOURCE will be inspected,
and the variables will be set according to it.  Variables not
specified will be given default values.

After this is done, BODY will be executed in the scope
of the `let' form.

The variables bound and their default values are described by
the `mail-source-keyword-map' variable."
  `(let ,(mail-source-bind-1 (car type-source))
     (mail-source-set-1 ,(cadr type-source))
     ,@body))

(put 'mail-source-bind 'lisp-indent-function 1)
(put 'mail-source-bind 'edebug-form-spec '(form body))

(defun mail-source-set-1 (source)
  (let* ((type (pop source))
	 (defaults (cdr (assq type mail-source-keyword-map)))
	 default value keyword)
    (while (setq default (pop defaults))
      (set (mail-source-strip-keyword (setq keyword (car default)))
	   (if (setq value (plist-get source keyword))
	       (mail-source-value value)
	     (mail-source-value (cadr default)))))))

(eval-and-compile
  (defun mail-source-bind-common-1 ()
    (let* ((defaults mail-source-common-keyword-map)
	   default bind)
      (while (setq default (pop defaults))
	(push (list (mail-source-strip-keyword (car default))
		    nil)
	      bind))
      bind)))

(defun mail-source-set-common-1 (source)
  (let* ((type (pop source))
	 (defaults mail-source-common-keyword-map)
	 (defaults-1 (cdr (assq type mail-source-keyword-map)))
	 default value keyword)
    (while (setq default (pop defaults))
      (set (mail-source-strip-keyword (setq keyword (car default)))
	   (if (setq value (plist-get source keyword))
	       (mail-source-value value)
	     (if (setq value (assq  keyword defaults-1))
		 (mail-source-value (cadr value))
	       (mail-source-value (cadr default))))))))

(defmacro mail-source-bind-common (source &rest body)
  "Return a `let' form that binds all common variables.
See `mail-source-bind'."
  `(let ,(mail-source-bind-common-1)
     (mail-source-set-common-1 source)
     ,@body))

(put 'mail-source-bind-common 'lisp-indent-function 1)
(put 'mail-source-bind-common 'edebug-form-spec '(form body))

(defun mail-source-value (value)
  "Return the value of VALUE."
  (cond
   ;; String
   ((stringp value)
    value)
   ;; Function
   ((and (listp value)
	 (functionp (car value)))
    (eval value))
   ;; Just return the value.
   (t
    value)))

(defun mail-source-fetch (source callback)
  "Fetch mail from SOURCE and call CALLBACK zero or more times.
CALLBACK will be called with the name of the file where (some of)
the mail from SOURCE is put.
Return the number of files that were found."
  (mail-source-bind-common source
    (if (or mail-source-plugged plugged)
	(save-excursion
	  (let ((function (cadr (assq (car source) mail-source-fetcher-alist)))
		(found 0))
	    (unless function
	      (error "%S is an invalid mail source specification" source))
	    ;; If there's anything in the crash box, we do it first.
	    (when (file-exists-p mail-source-crash-box)
	      (message "Processing mail from %s..." mail-source-crash-box)
	      (setq found (mail-source-callback
			   callback mail-source-crash-box)))
	    (+ found
	       (condition-case err
		   (funcall function source callback)
		 (error
		  (unless (yes-or-no-p
			   (format "Mail source error (%s).  Continue? " err))
		    (error "Cannot get new mail"))
		  0))))))))

(eval-and-compile
  (if (fboundp 'make-temp-file)
      (defalias 'mail-source-make-complex-temp-name 'make-temp-file)
    (defun mail-source-make-complex-temp-name (prefix)
      (let ((newname (make-temp-name prefix))
	    (newprefix prefix))
	(while (file-exists-p newname)
	  (setq newprefix (concat newprefix "x"))
	  (setq newname (make-temp-name newprefix)))
	newname))))

(defun mail-source-callback (callback info)
  "Call CALLBACK on the mail file, and then remove the mail file.
Pass INFO on to CALLBACK."
  (if (or (not (file-exists-p mail-source-crash-box))
	  (zerop (nth 7 (file-attributes mail-source-crash-box))))
      (progn
	(when (file-exists-p mail-source-crash-box)
	  (delete-file mail-source-crash-box))
	0)
    (prog1
	(funcall callback mail-source-crash-box info)
      (when (file-exists-p mail-source-crash-box)
	;; Delete or move the incoming mail out of the way.
	(if mail-source-delete-incoming
	    (delete-file mail-source-crash-box)
	  (let ((incoming
		 (mail-source-make-complex-temp-name
		  (expand-file-name
		   mail-source-incoming-file-prefix
		   mail-source-directory))))
	    (unless (file-exists-p (file-name-directory incoming))
	      (make-directory (file-name-directory incoming) t))
	    (rename-file mail-source-crash-box incoming t)))))))

(defun mail-source-movemail (from to)
  "Move FROM to TO using movemail."
  (if (not (file-writable-p to))
      (error "Can't write to crash box %s.  Not moving mail" to)
    (let ((to (file-truename (expand-file-name to)))
	  errors result)
      (setq to (file-truename to)
	    from (file-truename from))
      ;; Set TO if have not already done so, and rename or copy
      ;; the file FROM to TO if and as appropriate.
      (cond
       ((file-exists-p to)
	;; The crash box exists already.
	t)
       ((not (file-exists-p from))
	;; There is no inbox.
	(setq to nil))
       ((zerop (nth 7 (file-attributes from)))
	;; Empty file.
	(setq to nil))
       (t
	;; If getting from mail spool directory, use movemail to move
	;; rather than just renaming, so as to interlock with the
	;; mailer.
	(unwind-protect
	    (save-excursion
	      (setq errors (generate-new-buffer " *mail source loss*"))
	      (let ((default-directory "/"))
		(setq result
		      (apply
		       'call-process
		       (append
			(list
			 (expand-file-name "movemail" exec-directory)
			 nil errors nil from to)))))
	      (when (file-exists-p to)
		(set-file-modes to mail-source-default-file-modes))
	      (if (and (not (buffer-modified-p errors))
		       (zerop result))
		  ;; No output => movemail won.
		  t
		(set-buffer errors)
		;; There may be a warning about older revisions.  We
		;; ignore that.
		(goto-char (point-min))
		(if (search-forward "older revision" nil t)
		    t
		  ;; Probably a real error.
		  (subst-char-in-region (point-min) (point-max) ?\n ?\  )
		  (goto-char (point-max))
		  (skip-chars-backward " \t")
		  (delete-region (point) (point-max))
		  (goto-char (point-min))
		  (when (looking-at "movemail: ")
		    (delete-region (point-min) (match-end 0)))
		  (unless (yes-or-no-p
			   (format "movemail: %s (%d return).  Continue? "
				   (buffer-string) result))
		    (error "%s" (buffer-string)))
		  (setq to nil)))))))
      (when (and errors
		 (buffer-name errors))
	(kill-buffer errors))
      ;; Return whether we moved successfully or not.
      to)))

(defun mail-source-movemail-and-remove (from to)
  "Move FROM to TO using movemail, then remove FROM if empty."
  (or (not (mail-source-movemail from to))
      (not (zerop (nth 7 (file-attributes from))))
      (delete-file from)))

(defvar mail-source-read-passwd nil)
(defun mail-source-read-passwd (prompt &rest args)
  "Read a password using PROMPT.
If ARGS, PROMPT is used as an argument to `format'."
  (let ((prompt
	 (if args
	     (apply 'format prompt args)
	   prompt)))
    (unless mail-source-read-passwd
      (if (or (fboundp 'read-passwd) (load "passwd" t))
	  (setq mail-source-read-passwd 'read-passwd)
	(unless (fboundp 'ange-ftp-read-passwd)
	  (autoload 'ange-ftp-read-passwd "ange-ftp"))
	(setq mail-source-read-passwd 'ange-ftp-read-passwd)))
    (funcall mail-source-read-passwd prompt)))

(defun mail-source-fetch-with-program (program)
  (zerop (call-process shell-file-name nil nil nil
 		       shell-command-switch program)))

(defun mail-source-run-script (script spec &optional delay)
  (when script
    (if (and (symbolp script) (fboundp script))
	(funcall script)
      (mail-source-call-script
       (format-spec script spec))))
  (when delay
    (sleep-for delay)))

(defun mail-source-call-script (script)
  (let ((background nil))
    (when (string-match "& *$" script)
      (setq script (substring script 0 (match-beginning 0))
	    background 0))
    (call-process shell-file-name nil background nil
		  shell-command-switch script)))

;;;
;;; Different fetchers
;;;

(defun mail-source-fetch-file (source callback)
  "Fetcher for single-file sources."
  (mail-source-bind (file source)
    (mail-source-run-script
     prescript (format-spec-make ?t mail-source-crash-box)
     prescript-delay)
    (let ((mail-source-string (format "file:%s" path)))
      (if (mail-source-movemail path mail-source-crash-box)
	  (prog1
	      (mail-source-callback callback path)
	    (mail-source-run-script
	     postscript (format-spec-make ?t mail-source-crash-box)))
	0))))

(defun mail-source-fetch-directory (source callback)
  "Fetcher for directory sources."
  (mail-source-bind (directory source)
    (mail-source-run-script
     prescript (format-spec-make ?t path)
     prescript-delay)
    (let ((found 0)
	  (mail-source-string (format "directory:%s" path)))
      (dolist (file (directory-files
		     path t (concat (regexp-quote suffix) "$")))
	(when (and (file-regular-p file)
		   (funcall predicate file)
		   (mail-source-movemail file mail-source-crash-box))
	  (incf found (mail-source-callback callback file))))
      (mail-source-run-script
       postscript (format-spec-make ?t path))
      found)))

(defun mail-source-fetch-pop (source callback)
  "Fetcher for single-file sources."
  (mail-source-bind (pop source)
    (mail-source-run-script
     prescript
     (format-spec-make ?p password ?t mail-source-crash-box
		       ?s server ?P port ?u user)
     prescript-delay)
    (let ((from (format "%s:%s:%s" server user port))
	  (mail-source-string (format "pop:%s@%s" user server))
	  result)
      (when (eq authentication 'password)
	(setq password
	      (or password
		  (cdr (assoc from mail-source-password-cache))
		  (mail-source-read-passwd
		   (format "Password for %s at %s: " user server)))))
      (when server
	(setenv "MAILHOST" server))
      (setq result
	    (cond
	     (program
	      (mail-source-fetch-with-program
	       (format-spec
		program
		(format-spec-make ?p password ?t mail-source-crash-box
				  ?s server ?P port ?u user))))
	     (function
	      (funcall function mail-source-crash-box))
	     ;; The default is to use pop3.el.
	     (t
	      (let ((pop3-password password)
		    (pop3-maildrop user)
		    (pop3-mailhost server)
		    (pop3-port port)
		    (pop3-authentication-scheme
		     (if (eq authentication 'apop) 'apop 'pass)))
		(save-excursion (pop3-movemail mail-source-crash-box))))))
      (if result
	  (progn
	    (when (eq authentication 'password)
	      (unless (assoc from mail-source-password-cache)
		(push (cons from password) mail-source-password-cache)))
	    (prog1
		(mail-source-callback callback server)
	      ;; Update display-time's mail flag, if relevant.
	      (if (equal source mail-source-primary-source)
		  (setq mail-source-new-mail-available nil))
	      (mail-source-run-script
	       postscript
	       (format-spec-make ?p password ?t mail-source-crash-box
				 ?s server ?P port ?u user))))
	;; We nix out the password in case the error
	;; was because of a wrong password being given.
	(setq mail-source-password-cache
	      (delq (assoc from mail-source-password-cache)
		    mail-source-password-cache))
	0))))

(defun mail-source-check-pop (source)
  "Check whether there is new mail."
  (mail-source-bind (pop source)
    (let ((from (format "%s:%s:%s" server user port))
	  (mail-source-string (format "pop:%s@%s" user server))
	  result)
      (when (eq authentication 'password)
	(setq password
	      (or password
		  (cdr (assoc from mail-source-password-cache))
		  (mail-source-read-passwd
		   (format "Password for %s at %s: " user server))))
	(unless (assoc from mail-source-password-cache)
	  (push (cons from password) mail-source-password-cache)))
      (when server
	(setenv "MAILHOST" server))
      (setq result
	    (cond
	     ;; No easy way to check whether mail is waiting for these.
	     (program)
	     (function)
	     ;; The default is to use pop3.el.
	     (t
	      (let ((pop3-password password)
		    (pop3-maildrop user)
		    (pop3-mailhost server)
		    (pop3-port port)
		    (pop3-authentication-scheme
		     (if (eq authentication 'apop) 'apop 'pass)))
		(save-excursion (pop3-get-message-count))))))
      (if result
	  ;; Inform display-time that we have new mail.
	  (setq mail-source-new-mail-available (> result 0))
	;; We nix out the password in case the error
	;; was because of a wrong password being given.
	(setq mail-source-password-cache
	      (delq (assoc from mail-source-password-cache)
		    mail-source-password-cache)))
      result)))

(defun mail-source-new-mail-p ()
  "Handler for `display-time' to indicate when new mail is available."
  ;; Only report flag setting; flag is updated on a different schedule.
  mail-source-new-mail-available)


(defvar mail-source-report-new-mail nil)
(defvar mail-source-report-new-mail-timer nil)
(defvar mail-source-report-new-mail-idle-timer nil)

(eval-when-compile 
  (if (featurep 'xemacs)
      (require 'itimer)
    (require 'timer)))

(defun mail-source-start-idle-timer ()
  ;; Start our idle timer if necessary, so we delay the check until the
  ;; user isn't typing.
  (unless mail-source-report-new-mail-idle-timer
    (setq mail-source-report-new-mail-idle-timer
	  (run-with-idle-timer
	   mail-source-idle-time-delay
	   nil
	   (lambda ()
	     (setq mail-source-report-new-mail-idle-timer nil)
	     (mail-source-check-pop mail-source-primary-source))))
    ;; Since idle timers created when Emacs is already in the idle
    ;; state don't get activated until Emacs _next_ becomes idle, we
    ;; need to force our timer to be considered active now.  We do
    ;; this by being naughty and poking the timer internals directly
    ;; (element 0 of the vector is nil if the timer is active).
    (aset mail-source-report-new-mail-idle-timer 0 nil)))

(defun mail-source-report-new-mail (arg)
  "Toggle whether to report when new mail is available.
This only works when `display-time' is enabled."
  (interactive "P")
  (if (not mail-source-primary-source)
      (error "Need to set `mail-source-primary-source' to check for new mail"))
  (let ((on (if (null arg)
		(not mail-source-report-new-mail)
	      (> (prefix-numeric-value arg) 0))))
    (setq mail-source-report-new-mail on)
    (and mail-source-report-new-mail-timer
	 (nnheader-cancel-timer mail-source-report-new-mail-timer))
    (and mail-source-report-new-mail-idle-timer
	 (nnheader-cancel-timer mail-source-report-new-mail-idle-timer))
    (setq mail-source-report-new-mail-timer nil)
    (setq mail-source-report-new-mail-idle-timer nil)
    (if on
	(progn
	  (require 'time)
	  ;; display-time-mail-function is an Emacs 21 feature.
	  (setq display-time-mail-function #'mail-source-new-mail-p)
	  ;; Set up the main timer.
	  (setq mail-source-report-new-mail-timer
		(run-at-time t (* 60 mail-source-report-new-mail-interval)
			     #'mail-source-start-idle-timer))
	  ;; When you get new mail, clear "Mail" from the mode line.
	  (add-hook 'nnmail-post-get-new-mail-hook
		    'display-time-event-handler)
	  (message "Mail check enabled"))
      (setq display-time-mail-function nil)
      (remove-hook 'nnmail-post-get-new-mail-hook
		   'display-time-event-handler)
      (message "Mail check disabled"))))

(defun mail-source-fetch-maildir (source callback)
  "Fetcher for maildir sources."
  (mail-source-bind (maildir source)
    (let ((found 0)
	  mail-source-string)
      (unless (string-match "/$" path)
	(setq path (concat path "/")))
      (dolist (subdir subdirs)
	(when (file-directory-p (concat path subdir))
	  (setq mail-source-string (format "maildir:%s%s" path subdir))
	  (dolist (file (directory-files (concat path subdir) t))
	    (when (and (not (file-directory-p file))
		       (not (if function
				(funcall function file mail-source-crash-box)
			      (let ((coding-system-for-write 
				     mm-text-coding-system)
				    (coding-system-for-read 
				     mm-text-coding-system))
				(with-temp-file mail-source-crash-box
				  (insert-file-contents file)
				  (goto-char (point-min))
;;;                               ;; Unix mail format
;;; 				  (unless (looking-at "\n*From ")
;;; 				    (insert "From maildir " 
;;; 					    (current-time-string) "\n"))
;;; 				  (while (re-search-forward "^From " nil t)
;;; 				    (replace-match ">From "))
;;;                               (goto-char (point-max))
;;;				  (insert "\n\n")
				  ;; MMDF mail format
				  (insert "\001\001\001\001\n"))
				(delete-file file)))))
	      (incf found (mail-source-callback callback file))))))
      found)))

(eval-and-compile
  (autoload 'imap-open "imap")
  (autoload 'imap-authenticate "imap")
  (autoload 'imap-mailbox-select "imap")
  (autoload 'imap-mailbox-unselect "imap")
  (autoload 'imap-mailbox-close "imap")
  (autoload 'imap-search "imap")
  (autoload 'imap-fetch "imap")
  (autoload 'imap-close "imap")
  (autoload 'imap-error-text "imap")
  (autoload 'imap-message-flags-add "imap")
  (autoload 'imap-list-to-message-set "imap")
  (autoload 'imap-range-to-message-set "imap")
  (autoload 'nnheader-ms-strip-cr "nnheader"))

(defvar mail-source-imap-file-coding-system 'binary
  "Coding system for the crashbox made by `mail-source-fetch-imap'.")

(defun mail-source-fetch-imap (source callback)
  "Fetcher for imap sources."
  (mail-source-bind (imap source)
    (let ((from (format "%s:%s:%s" server user port))
	  (found 0)
	  (buf (get-buffer-create (generate-new-buffer-name " *imap source*")))
	  (mail-source-string (format "imap:%s:%s" server mailbox))
	  remove)
      (if (and (imap-open server port stream authentication buf)
	       (imap-authenticate
		user (or (cdr (assoc from mail-source-password-cache))
			 password) buf)
	       (imap-mailbox-select mailbox nil buf))
	  (let ((coding-system-for-write mail-source-imap-file-coding-system)
		str)
	    (with-temp-file mail-source-crash-box
	      ;; Avoid converting 8-bit chars from inserted strings to
	      ;; multibyte.
	      (mm-disable-multibyte)
	      ;; remember password
	      (with-current-buffer buf
		(when (or imap-password
			  (assoc from mail-source-password-cache))
		  (push (cons from imap-password) mail-source-password-cache)))
	      ;; if predicate is nil, use all uids
	      (dolist (uid (imap-search (or predicate "1:*") buf))
		(when (setq str (imap-fetch uid "RFC822.PEEK" 'RFC822 nil buf))
		  (push uid remove)
		  (insert "From imap " (current-time-string) "\n")
		  (save-excursion
		    (insert str "\n\n"))
		  (while (re-search-forward "^From " nil t)
		    (replace-match ">From "))
		  (goto-char (point-max))))
	      (nnheader-ms-strip-cr))
	    (incf found (mail-source-callback callback server))
	    (when (and remove fetchflag)
	      (imap-message-flags-add
	       (imap-range-to-message-set (gnus-compress-sequence remove))
	       fetchflag nil buf))
	    (if dontexpunge
		(imap-mailbox-unselect buf)
	      (imap-mailbox-close buf))
	    (imap-close buf))
	(imap-close buf)
	;; We nix out the password in case the error
	;; was because of a wrong password being given.
	(setq mail-source-password-cache
	      (delq (assoc from mail-source-password-cache)
		    mail-source-password-cache))
	(error (imap-error-text buf)))
      (kill-buffer buf)
      found)))

(eval-and-compile
  (autoload 'webmail-fetch "webmail"))

(defun mail-source-fetch-webmail (source callback)
  "Fetch for webmail source."
  (mail-source-bind (webmail source)
    (let ((mail-source-string (format "webmail:%s:%s" subtype user))
	  (webmail-newmail-only dontexpunge)
	  (webmail-move-to-trash-can (not dontexpunge)))
      (when (eq authentication 'password)
	(setq password
	      (or password
		  (cdr (assoc (format "webmail:%s:%s" subtype user) 
			      mail-source-password-cache))
		  (mail-source-read-passwd
		   (format "Password for %s at %s: " user subtype))))
	(when (and password
		   (not (assoc (format "webmail:%s:%s" subtype user) 
			       mail-source-password-cache)))
	  (push (cons (format "webmail:%s:%s" subtype user) password) 
		mail-source-password-cache)))
      (webmail-fetch mail-source-crash-box subtype user password)
      (mail-source-callback callback (symbol-name subtype)))))

(provide 'mail-source)

;;; mail-source.el ends here
