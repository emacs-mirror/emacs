;;; paths.el --- define pathnames for use by various Emacs commands.

;; Copyright (C) 1986, 1988 Free Software Foundation, Inc.

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
  (let ((start (list "/usr/local/info/"
		     "/usr/local/lib/info/"))
	(configdir (file-name-as-directory configure-info-directory)))
    (or (member configdir start)
	(setq start (nconc start (list configdir))))
    (or (member (expand-file-name "../info/" data-directory) start)
	(setq start
	      (nconc start
		     (list (expand-file-name "../info/" data-directory)))))
    start)
  "List of directories to search for Info documentation files.")

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
If it is a string such as \":DIRECTORY\", then ~/DIRECTORY
is used as a news spool.  `gnus-nntp-server' is initialised from NNTPSERVER
environment variable or, if none, this value.")

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

(defvar mh-progs
  (cond ((file-directory-p "/usr/bin/mh/") "/usr/bin/mh/") ;Ultrix 4.2
	((file-directory-p "/usr/new/mh/") "/usr/new/mh/") ;Ultrix <4.2
        ((file-directory-p "/usr/local/bin/mh/") "/usr/local/bin/mh/")
        ((file-directory-p "/usr/local/mh/") "/usr/local/mh/")
        (t "/usr/local/bin/"))
  "Directory containing MH commands.")

(defvar mh-lib
  (cond ((file-directory-p "/usr/lib/mh/") "/usr/lib/mh/") ;Ultrix 4.2
	((file-directory-p "/usr/new/lib/mh/") "/usr/new/lib/mh/") ;Ultrix <4.2
        ((file-directory-p "/usr/local/lib/mh/") "/usr/local/lib/mh/")
        (t "/usr/local/bin/mh/"))
  "Directory of MH library.")

(defvar rmail-file-name "~/RMAIL"
  "Name of user's primary mail file.")

(defvar gnus-startup-file "~/.newsrc"
  "The file listing groups to which user is subscribed.
Will use `gnus-startup-file'-SERVER instead if exists.")

(defconst rmail-spool-directory
  (cond ((memq system-type '(dgux-unix hpux usg-unix-v unisoft-unix rtu
				       irix))
	 "/usr/mail/")
	((eq system-type 'netbsd)
	 "/var/mail/")
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

(defconst term-file-prefix (if (eq system-type 'vax-vms) "[.term]" "term/")
  "If non-nil, Emacs startup does (load (concat term-file-prefix (getenv \"TERM\")))
You may set this variable to nil in your `.emacs' file if you do not wish
the terminal-initialization file to be loaded.")

;; Solaris 2 has both of these files; prefer /usr/ucb/man
;; because the other has nonstandard argument conventions.
(defconst manual-program (if (file-exists-p "/usr/ucb/man")
			     "/usr/ucb/man" "/usr/bin/man")
  "Program to run to print man pages.")

(defconst abbrev-file-name 
  (if (eq system-type 'vax-vms)
      "~/abbrev.def"
    "~/.abbrev_defs")
  "*Default name of file to read abbrevs from.")

;;; paths.el ends here
