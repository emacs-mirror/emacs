;;; paths.el --- define pathnames for use by various Emacs commands.

;; Copyright (C) 1986, 1988, 1994 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; These are default settings for names of certain files and directories
;; that Emacs needs to refer to from time to time.

;; If these settings are not right, override them with `setq'
;; in site-init.el.  Do not change this file.

;;; Code:

(defvar Info-default-directory-list
  (let ((start (list "/usr/local/lib/info/"
		     ;; This comes second so that, if it is the same
		     ;; as configure-info-directory (which is usually true)
		     ;; and Emacs has been installed (also usually true)
		     ;; then the list will end with two copies of this;
		     ;; which means that the last dir file Info-insert-dir
		     ;; finds will be the one in this directory.
		     "/usr/local/info/"))
	(configdir (file-name-as-directory configure-info-directory)))
    (setq start (nconc start (list configdir)))
    start)
  "List of directories to search for Info documentation files.
They are searched in the order they are given in this list.
Therefore, the directory of Info files that come with Emacs
normally should come last (so that local files override standard ones).")

(defvar news-path "/usr/spool/news/"
  "The root directory below which all news files are stored.")

(defvar news-inews-program
  (cond ((file-exists-p "/usr/bin/inews") "/usr/bin/inews")
	((file-exists-p "/usr/local/inews") "/usr/local/inews")
	((file-exists-p "/usr/local/bin/inews") "/usr/local/bin/inews")
	((file-exists-p "/usr/lib/news/inews") "/usr/lib/news/inews")
	(t "inews"))
  "Program to post news.")

(defvar gnus-default-nntp-server ""
  ;; set this to your local server
  "The name of the host running an NNTP server.
The null string means use the local host as the server site.")

(defvar gnus-nntp-service "nntp"
  "NNTP service name, usually \"nntp\" or 119).
Go to a local news spool if its value is nil, in which case `gnus-nntp-server'
should be set to `(system-name)'.")

(defvar gnus-local-domain nil
  "*Your domain name without a host name: for example, \"ai.mit.edu\".
The DOMAINNAME environment variable is used instead if defined.
If the function `system-name' returns a fully qualified domain name,
there is no need to set this variable.")

(defvar gnus-local-organization nil
  "*The name of your organization, as a string.
The `ORGANIZATION' environment variable is used instead if defined.")

(defvar gnus-startup-file "~/.newsrc"
  "The file listing groups to which user is subscribed.
Will use `gnus-startup-file'-SERVER instead if exists.")

(defvar rmail-file-name "~/RMAIL"
  "Name of user's primary mail file.")

(defconst rmail-spool-directory
  (cond ((string-match "^[^-]+-[^-]+-sco3.2v4" system-configuration)
	 "/usr/spool/mail/")
	;; On The Bull DPX/2 /usr/spool/mail is used although 
	;; it is usg-unix-v.
	((string-match "^m68k-bull-sysv3" system-configuration)
	 "/usr/spool/mail/")
	;; SVR4 and recent BSD are said to use this.
	;; Rather than trying to know precisely which systems use it,
	;; let's assume this dir is never used for anything else.
	((file-exists-p "/var/mail")
	 "/var/mail/")
	((memq system-type '(dgux hpux usg-unix-v unisoft-unix rtu irix))
	 "/usr/mail/")
	(t "/usr/spool/mail/"))
  "Name of directory used by system mailer for delivering new mail.
Its name should end with a slash.")

(defconst sendmail-program
  (cond
    ((file-exists-p "/usr/lib/sendmail") "/usr/lib/sendmail")
    ((file-exists-p "/usr/sbin/sendmail") "/usr/sbin/sendmail")
    ((file-exists-p "/usr/ucblib/sendmail") "/usr/ucblib/sendmail")
    (t "fakemail"))			;In ../etc, to interface to /bin/mail.
  "Program used to send messages.")

(defconst remote-shell-program
  (cond
   ;; Some systems use rsh for the remote shell; others use that name for the
   ;; restricted shell and use remsh for the remote shell.  Let's try to guess
   ;; based on what we actually find out there.  The restricted shell is
   ;; almost certainly in /bin or /usr/bin, so it's probably safe to assume
   ;; that an rsh found elsewhere is the remote shell program.  The converse
   ;; is not true: /usr/bin/rsh could be either one, so check that last.
   ((file-exists-p "/usr/ucb/remsh") "/usr/ucb/remsh")
   ((file-exists-p "/usr/bsd/remsh") "/usr/bsd/remsh")
   ((file-exists-p "/bin/remsh") "/bin/remsh")
   ((file-exists-p "/usr/bin/remsh") "/bin/remsh")
   ((file-exists-p "/usr/local/bin/remsh") "/usr/local/bin/remsh")
   ((file-exists-p "/usr/ucb/rsh") "/usr/ucb/rsh")
   ((file-exists-p "/usr/bsd/rsh") "/usr/bsd/rsh")
   ((file-exists-p "/usr/local/bin/rsh") "/usr/local/bin/rsh")
   ((file-exists-p "/bin/rsh") "/bin/rsh")
   ((file-exists-p "/usr/bin/rsh") "/usr/bin/rsh")
   (t "rsh")))

(defconst term-file-prefix (if (eq system-type 'vax-vms) "[.term]" "term/")
  "If non-nil, Emacs startup does (load (concat term-file-prefix (getenv \"TERM\")))
You may set this variable to nil in your `.emacs' file if you do not wish
the terminal-initialization file to be loaded.")

(defconst abbrev-file-name 
  (if (eq system-type 'vax-vms)
      "~/abbrev.def"
    "~/.abbrev_defs")
  "*Default name of file to read abbrevs from.")

;;; paths.el ends here
