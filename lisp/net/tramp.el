;;; tramp.el --- Transparent Remote Access, Multiple Protocol
;;; -*- mode: Emacs-Lisp; coding: utf-8; -*-

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; (copyright statements below in code to be updated with the above notice)

;; Author: Kai Groﬂjohann <kai.grossjohann@gmx.net>
;;         Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides remote file editing, similar to ange-ftp.
;; The difference is that ange-ftp uses FTP to transfer files between
;; the local and the remote host, whereas tramp.el uses a combination
;; of rsh and rcp or other work-alike programs, such as ssh/scp.
;;
;; For more detailed instructions, please see the info file.
;;
;; Notes:
;; -----
;;
;; This package only works for Emacs 21.1 and higher, and for XEmacs 21.4
;; and higher.  For XEmacs 21, you need the package `fsf-compat' for
;; the `with-timeout' macro.)
;;
;; This version might not work with pre-Emacs 21 VC unless VC is
;; loaded before tramp.el.  Could you please test this and tell me about
;; the result?  Thanks.
;;
;; Also see the todo list at the bottom of this file.
;;
;; The current version of Tramp can be retrieved from the following URL:
;;            http://ftp.gnu.org/gnu/tramp/
;;
;; There's a mailing list for this, as well.  Its name is:
;;            tramp-devel@gnu.org
;; You can use the Web to subscribe, under the following URL:
;;            http://lists.gnu.org/mailman/listinfo/tramp-devel
;;
;; For the adventurous, the current development sources are available
;; via CVS.  You can find instructions about this at the following URL:
;;            http://savannah.gnu.org/projects/tramp/
;; Click on "CVS" in the navigation bar near the top.
;;
;; Don't forget to put on your asbestos longjohns, first!

;;; Code:

;; The Tramp version number and bug report address, as prepared by configure.
(require 'trampver)
(add-hook 'tramp-unload-hook
	  '(lambda ()
	     (when (featurep 'trampver)
	       (unload-feature 'trampver 'force))))

(require 'tramp-compat)
(add-hook 'tramp-unload-hook
	  '(lambda ()
	     (when (featurep 'tramp-compat)
	       (unload-feature 'tramp-compat 'force))))

(require 'format-spec)                  ; from Gnus 5.8, also in tar ball
;; As long as password.el is not part of (X)Emacs, it shouldn't
;; be mandatory
(if (featurep 'xemacs)
    (load "password" 'noerror)
  (or (require 'password-cache nil 'noerror)
      (require 'password nil 'noerror))) ; from No Gnus, also in tar ball

(require 'shell)
(require 'advice)

;; Requiring 'tramp-cache results in an endless loop.
(autoload 'tramp-get-file-property "tramp-cache")
(autoload 'tramp-set-file-property "tramp-cache")
(autoload 'tramp-flush-file-property "tramp-cache")
(autoload 'tramp-flush-directory-property "tramp-cache")
(autoload 'tramp-get-connection-property "tramp-cache")
(autoload 'tramp-set-connection-property "tramp-cache")
(autoload 'tramp-flush-connection-property "tramp-cache")
(autoload 'tramp-parse-connection-properties "tramp-cache")
(add-hook 'tramp-unload-hook
	  '(lambda ()
	     (when (featurep 'tramp-cache)
	       (unload-feature 'tramp-cache 'force))))

(autoload 'tramp-uuencode-region "tramp-uu"
  "Implementation of `uuencode' in Lisp.")
(add-hook 'tramp-unload-hook
	  '(lambda ()
	     (when (featurep 'tramp-uu)
	       (unload-feature 'tramp-uu 'force))))

(autoload 'uudecode-decode-region "uudecode")

;; The following Tramp packages must be loaded after tramp.el, because
;; they require it as well.
(eval-after-load "tramp"
  '(dolist
       (feature
	(list

	 ;; Tramp interactive commands.
	 'tramp-cmds

	 ;; Load foreign FTP method.
	 (if (featurep 'xemacs) 'tramp-efs 'tramp-ftp)

	 ;; tramp-smb uses "smbclient" from Samba.  Not available
	 ;; under Cygwin and Windows, because they don't offer
	 ;; "smbclient".  And even not necessary there, because Emacs
	 ;; supports UNC file names like "//host/share/localname".
	 (unless (memq system-type '(cygwin windows-nt)) 'tramp-smb)

	 ;; Load foreign FISH method.
	 'tramp-fish

	 ;; Load gateways.  It needs `make-network-process' from Emacs 22.
	 (when (functionp 'make-network-process) 'tramp-gw)))

     (when feature
       (require feature)
       (add-hook 'tramp-unload-hook
		 `(lambda ()
		    (when (featurep ,feature)
		      (unload-feature ,feature 'force)))))))

;;; User Customizable Internal Variables:

(defgroup tramp nil
  "Edit remote files with a combination of rsh and rcp or similar programs."
  :group 'files
  :version "22.1")

(defcustom tramp-verbose 3
  "*Verbosity level for Tramp.
Any level x includes messages for all levels 1 .. x-1.  The levels are

 0  silent (no tramp messages at all)
 1  errors
 2  warnings
 3  connection to remote hosts (default level)
 4  activities
 5  internal
 6  sent and received strings
 7  file caching
 8  connection properties
10  traces (huge)."
  :group 'tramp
  :type 'integer)

;; Emacs case
(eval-and-compile
  (when (boundp 'backup-directory-alist)
    (defcustom tramp-backup-directory-alist nil
      "Alist of filename patterns and backup directory names.
Each element looks like (REGEXP . DIRECTORY), with the same meaning like
in `backup-directory-alist'.  If a Tramp file is backed up, and DIRECTORY
is a local file name, the backup directory is prepended with Tramp file
name prefix \(method, user, host\) of file.

\(setq tramp-backup-directory-alist backup-directory-alist\)

gives the same backup policy for Tramp files on their hosts like the
policy for local files."
      :group 'tramp
      :type '(repeat (cons (regexp :tag "Regexp matching filename")
			   (directory :tag "Backup directory name"))))))

;; XEmacs case.  We cannot check for `bkup-backup-directory-info', because
;; the package "backup-dir" might not be loaded yet.
(eval-and-compile
  (when (featurep 'xemacs)
    (defcustom tramp-bkup-backup-directory-info nil
      "*Alist of (FILE-REGEXP BACKUP-DIR OPTIONS ...))
It has the same meaning like `bkup-backup-directory-info' from package
`backup-dir'.  If a Tramp file is backed up, and BACKUP-DIR is a local
file name, the backup directory is prepended with Tramp file name prefix
\(method, user, host\) of file.

\(setq tramp-bkup-backup-directory-info bkup-backup-directory-info\)

gives the same backup policy for Tramp files on their hosts like the
policy for local files."
      :type '(repeat
	      (list (regexp :tag "File regexp")
		    (string :tag "Backup Dir")
		    (set :inline t
			 (const ok-create)
			 (const full-path)
			 (const prepend-name)
			 (const search-upward))))
      :group 'tramp)))

(defcustom tramp-auto-save-directory nil
  "*Put auto-save files in this directory, if set.
The idea is to use a local directory so that auto-saving is faster."
  :group 'tramp
  :type '(choice (const nil) string))

(defcustom tramp-encoding-shell
  (if (memq system-type '(windows-nt))
      (getenv "COMSPEC")
    "/bin/sh")
  "*Use this program for encoding and decoding commands on the local host.
This shell is used to execute the encoding and decoding command on the
local host, so if you want to use `~' in those commands, you should
choose a shell here which groks tilde expansion.  `/bin/sh' normally
does not understand tilde expansion.

For encoding and deocding, commands like the following are executed:

    /bin/sh -c COMMAND < INPUT > OUTPUT

This variable can be used to change the \"/bin/sh\" part.  See the
variable `tramp-encoding-command-switch' for the \"-c\" part.

Note that this variable is not used for remote commands.  There are
mechanisms in tramp.el which automatically determine the right shell to
use for the remote host."
  :group 'tramp
  :type '(file :must-match t))

(defcustom tramp-encoding-command-switch
  (if (string-match "cmd\\.exe" tramp-encoding-shell)
      "/c"
    "-c")
  "*Use this switch together with `tramp-encoding-shell' for local commands.
See the variable `tramp-encoding-shell' for more information."
  :group 'tramp
  :type 'string)

(defcustom tramp-copy-size-limit 10240
  "*The maximum file size where inline copying is preferred over an out-of-the-band copy."
  :group 'tramp
  :type 'integer)

(defcustom tramp-terminal-type "dumb"
  "*Value of TERM environment variable for logging in to remote host.
Because Tramp wants to parse the output of the remote shell, it is easily
confused by ANSI color escape sequences and suchlike.  Often, shell init
files conditionalize this setup based on the TERM environment variable."
  :group 'tramp
  :type 'string)

(defvar tramp-methods
  `(("rcp"   (tramp-login-program        "rsh")
             (tramp-login-args           (("%h") ("-l" "%u")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         "rcp")
	     (tramp-copy-args            (("-p" "%k")))
	     (tramp-copy-keep-date       t)
	     (tramp-password-end-of-line nil))
    ("scp"   (tramp-login-program        "ssh")
             (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					  ("-e" "none")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         "scp")
	     (tramp-copy-args            (("-P" "%p") ("-p" "%k") ("-q")))
	     (tramp-copy-keep-date       t)
	     (tramp-password-end-of-line nil)
	     (tramp-gw-args              (("-o"
					   "GlobalKnownHostsFile=/dev/null")
					  ("-o" "UserKnownHostsFile=/dev/null")
					  ("-o" "StrictHostKeyChecking=no")))
	     (tramp-default-port         22))
    ("scp1"  (tramp-login-program        "ssh")
             (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					  ("-1" "-e" "none")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         "scp")
	     (tramp-copy-args            (("-1") ("-P" "%p") ("-p" "%k")
					  ("-q")))
	     (tramp-copy-keep-date       t)
	     (tramp-password-end-of-line nil)
	     (tramp-gw-args              (("-o"
					   "GlobalKnownHostsFile=/dev/null")
					  ("-o" "UserKnownHostsFile=/dev/null")
					  ("-o" "StrictHostKeyChecking=no")))
	     (tramp-default-port         22))
    ("scp2"  (tramp-login-program        "ssh")
             (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					  ("-2" "-e" "none")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         "scp")
	     (tramp-copy-args            (("-2") ("-P" "%p") ("-p" "%k")
					  ("-q")))
	     (tramp-copy-keep-date       t)
	     (tramp-password-end-of-line nil)
	     (tramp-gw-args              (("-o"
					   "GlobalKnownHostsFile=/dev/null")
					  ("-o" "UserKnownHostsFile=/dev/null")
					  ("-o" "StrictHostKeyChecking=no")))
	     (tramp-default-port         22))
    ("scp1_old"
             (tramp-login-program        "ssh1")
	     (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					  ("-e" "none")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         "scp1")
	     (tramp-copy-args            (("-p" "%k")))
	     (tramp-copy-keep-date       t)
	     (tramp-password-end-of-line nil))
    ("scp2_old"
             (tramp-login-program        "ssh2")
	     (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					  ("-e" "none")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         "scp2")
	     (tramp-copy-args            (("-p" "%k")))
	     (tramp-copy-keep-date       t)
	     (tramp-password-end-of-line nil))
    ("sftp"  (tramp-login-program        "ssh")
             (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					  ("-e" "none")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         "sftp")
	     (tramp-copy-args            nil)
	     (tramp-copy-keep-date       nil)
	     (tramp-password-end-of-line nil))
    ("rsync" (tramp-login-program        "ssh")
             (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					  ("-e" "none")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         "rsync")
	     (tramp-copy-args            (("-e" "ssh") ("-t" "%k")))
	     (tramp-copy-keep-date       t)
	     (tramp-password-end-of-line nil))
    ("remcp" (tramp-login-program        "remsh")
             (tramp-login-args           (("%h") ("-l" "%u")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         "rcp")
	     (tramp-copy-args            (("-p" "%k")))
	     (tramp-copy-keep-date       t)
	     (tramp-password-end-of-line nil))
    ("rsh"   (tramp-login-program        "rsh")
             (tramp-login-args           (("%h") ("-l" "%u")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         nil)
	     (tramp-copy-args            nil)
	     (tramp-copy-keep-date       nil)
	     (tramp-password-end-of-line nil))
    ("ssh"   (tramp-login-program        "ssh")
             (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					  ("-e" "none")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         nil)
	     (tramp-copy-args            nil)
	     (tramp-copy-keep-date       nil)
	     (tramp-password-end-of-line nil)
	     (tramp-gw-args              (("-o"
					   "GlobalKnownHostsFile=/dev/null")
					  ("-o" "UserKnownHostsFile=/dev/null")
					  ("-o" "StrictHostKeyChecking=no")))
	     (tramp-default-port         22))
    ("ssh1"  (tramp-login-program        "ssh")
             (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					  ("-1" "-e" "none")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         nil)
	     (tramp-copy-args            nil)
	     (tramp-copy-keep-date       nil)
	     (tramp-password-end-of-line nil)
	     (tramp-gw-args              (("-o"
					   "GlobalKnownHostsFile=/dev/null")
					  ("-o" "UserKnownHostsFile=/dev/null")
					  ("-o" "StrictHostKeyChecking=no")))
	     (tramp-default-port         22))
    ("ssh2"  (tramp-login-program        "ssh")
             (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					  ("-2" "-e" "none")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         nil)
	     (tramp-copy-args            nil)
	     (tramp-copy-keep-date       nil)
	     (tramp-password-end-of-line nil)
	     (tramp-gw-args              (("-o"
					   "GlobalKnownHostsFile=/dev/null")
					  ("-o" "UserKnownHostsFile=/dev/null")
					  ("-o" "StrictHostKeyChecking=no")))
	     (tramp-default-port         22))
    ("ssh1_old"
             (tramp-login-program        "ssh1")
	     (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					  ("-e" "none")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         nil)
	     (tramp-copy-args            nil)
	     (tramp-copy-keep-date       nil)
	     (tramp-password-end-of-line nil))
    ("ssh2_old"
             (tramp-login-program        "ssh2")
	     (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					  ("-e" "none")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         nil)
	     (tramp-copy-args            nil)
	     (tramp-copy-keep-date       nil)
	     (tramp-password-end-of-line nil))
    ("remsh" (tramp-login-program        "remsh")
             (tramp-login-args           (("%h") ("-l" "%u")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         nil)
	     (tramp-copy-args            nil)
	     (tramp-copy-keep-date       nil)
	     (tramp-password-end-of-line nil))
    ("telnet"
             (tramp-login-program        "telnet")
	     (tramp-login-args           (("%h") ("%p")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         nil)
	     (tramp-copy-args            nil)
	     (tramp-copy-keep-date       nil)
	     (tramp-password-end-of-line nil)
	     (tramp-default-port         23))
    ("su"    (tramp-login-program        "su")
             (tramp-login-args           (("-") ("%u")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         nil)
	     (tramp-copy-args            nil)
	     (tramp-copy-keep-date       nil)
	     (tramp-password-end-of-line nil))
    ("sudo"  (tramp-login-program        "sudo")
             (tramp-login-args           (("-u" "%u")
					  ("-s") ("-H") ("-p" "Password:")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         nil)
	     (tramp-copy-args            nil)
	     (tramp-copy-keep-date       nil)
	     (tramp-password-end-of-line nil))
    ("scpc"  (tramp-login-program        "ssh")
             (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					  ("-o" "ControlPath=%t.%%r@%%h:%%p")
					  ("-o" "ControlMaster=yes")
					  ("-e" "none")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         "scp")
	     (tramp-copy-args            (("-P" "%p") ("-p" "%k") ("-q")
					  ("-o" "ControlPath=%t.%%r@%%h:%%p")
					  ("-o" "ControlMaster=auto")))
	     (tramp-copy-keep-date       t)
	     (tramp-password-end-of-line nil)
	     (tramp-gw-args              (("-o"
					   "GlobalKnownHostsFile=/dev/null")
					  ("-o" "UserKnownHostsFile=/dev/null")
					  ("-o" "StrictHostKeyChecking=no")))
	     (tramp-default-port         22))
    ("scpx"  (tramp-login-program        "ssh")
             (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					  ("-e" "none" "-t" "-t" "/bin/sh")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         "scp")
	     (tramp-copy-args            (("-p" "%k")))
	     (tramp-copy-keep-date       t)
	     (tramp-password-end-of-line nil)
	     (tramp-gw-args              (("-o"
					   "GlobalKnownHostsFile=/dev/null")
					  ("-o" "UserKnownHostsFile=/dev/null")
					  ("-o" "StrictHostKeyChecking=no")))
	     (tramp-default-port         22))
    ("sshx"  (tramp-login-program        "ssh")
             (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					  ("-e" "none" "-t" "-t" "/bin/sh")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         nil)
	     (tramp-copy-args            nil)
	     (tramp-copy-keep-date       nil)
	     (tramp-password-end-of-line nil)
	     (tramp-gw-args              (("-o"
					   "GlobalKnownHostsFile=/dev/null")
					  ("-o" "UserKnownHostsFile=/dev/null")
					  ("-o" "StrictHostKeyChecking=no")))
	     (tramp-default-port         22))
    ("krlogin"
	     (tramp-login-program        "krlogin")
	     (tramp-login-args           (("%h") ("-l" "%u") ("-x")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         nil)
	     (tramp-copy-args            nil)
	     (tramp-copy-keep-date       nil)
	     (tramp-password-end-of-line nil))
    ("plink" (tramp-login-program        "plink")
	     (tramp-login-args           (("%h") ("-l" "%u") ("-P" "%p")
					  ("-ssh")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         nil)
	     (tramp-copy-args            nil)
	     (tramp-copy-keep-date       nil)
	     (tramp-password-end-of-line "xy") ;see docstring for "xy"
	     (tramp-default-port         22))
    ("plink1"
	     (tramp-login-program        "plink")
	     (tramp-login-args           (("%h") ("-l" "%u") ("-P" "%p")
					  ("-1" "-ssh")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         nil)
	     (tramp-copy-args            nil)
	     (tramp-copy-keep-date       nil)
	     (tramp-password-end-of-line "xy") ;see docstring for "xy"
	     (tramp-default-port         22))
    ("plinkx"
             (tramp-login-program        "plink")
	     ;; ("%h") must be a single element, see
	     ;; `tramp-compute-multi-hops'.
	     (tramp-login-args           (("-load") ("%h") ("-t")
					  (,(format
					     "env 'TERM=%s' 'PROMPT_COMMAND=' 'PS1=$ '"
					     tramp-terminal-type))
					  ("/bin/sh")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         nil)
	     (tramp-copy-args            nil)
	     (tramp-copy-keep-date       nil)
	     (tramp-password-end-of-line nil))
    ("pscp"  (tramp-login-program        "plink")
	     (tramp-login-args           (("%h") ("-l" "%u") ("-P" "%p")
					  ("-ssh")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         "pscp")
	     (tramp-copy-args            (("-scp") ("-p" "%k")))
	     (tramp-copy-keep-date       t)
	     (tramp-password-end-of-line "xy") ;see docstring for "xy"
	     (tramp-default-port         22))
    ("psftp" (tramp-login-program        "plink")
	     (tramp-login-args           (("%h") ("-l" "%u") ("-P" "%p")
					  ("-ssh")))
	     (tramp-remote-sh            "/bin/sh")
	     (tramp-copy-program         "pscp")
	     (tramp-copy-args            (("-psftp") ("-p" "%k")))
	     (tramp-copy-keep-date       t)
	     (tramp-password-end-of-line "xy")) ;see docstring for "xy"
    ("fcp"   (tramp-login-program        "fsh")
             (tramp-login-args           (("%h") ("-l" "%u") ("sh" "-i")))
	     (tramp-remote-sh            "/bin/sh -i")
	     (tramp-copy-program         "fcp")
	     (tramp-copy-args            (("-p" "%k")))
	     (tramp-copy-keep-date       t)
	     (tramp-password-end-of-line nil)))
  "*Alist of methods for remote files.
This is a list of entries of the form (NAME PARAM1 PARAM2 ...).
Each NAME stands for a remote access method.  Each PARAM is a
pair of the form (KEY VALUE).  The following KEYs are defined:
  * `tramp-remote-sh'
    This specifies the Bourne shell to use on the remote host.  This
    MUST be a Bourne-like shell.  It is normally not necessary to set
    this to any value other than \"/bin/sh\": Tramp wants to use a shell
    which groks tilde expansion, but it can search for it.  Also note
    that \"/bin/sh\" exists on all Unixen, this might not be true for
    the value that you decide to use.  You Have Been Warned.
  * `tramp-login-program'
    This specifies the name of the program to use for logging in to the
    remote host.  This may be the name of rsh or a workalike program,
    or the name of telnet or a workalike, or the name of su or a workalike.
  * `tramp-login-args'
    This specifies the list of arguments to pass to the above
    mentioned program.  Please note that this is a list of list of arguments,
    that is, normally you don't want to put \"-a -b\" or \"-f foo\"
    here.  Instead, you want a list (\"-a\" \"-b\"), or (\"-f\" \"foo\").
    There are some patterns: \"%h\" in this list is replaced by the host
    name, \"%u\" is replaced by the user name, \"%p\" is replaced by the
    port number, and \"%%\" can be used to obtain a literal percent character.
    If a list containing \"%h\", \"%u\" or \"%p\" is unchanged during
    expansion (i.e. no host or no user specified), this list is not used as
    argument.  By this, arguments like (\"-l\" \"%u\") are optional.
    \"%t\" is replaced by the temporary file name produced with
    `tramp-make-tramp-temp-file'.  \"%k\" indicates the keep-date
    parameter of a program, if exists.
  * `tramp-copy-program'
    This specifies the name of the program to use for remotely copying
    the file; this might be the absolute filename of rcp or the name of
    a workalike program.
  * `tramp-copy-args'
    This specifies the list of parameters to pass to the above mentioned
    program, the hints for `tramp-login-args' also apply here.
  * `tramp-copy-keep-date'
    This specifies whether the copying program when the preserves the
    timestamp of the original file.
  * `tramp-default-port'
    The default port of a method is needed in case of gateway connections.
    Additionally, it is used as indication which method is prepared for
    passing gateways.
  * `tramp-gw-args'
    As the attribute name says, additional arguments are specified here
    when a method is applied via a gateway.
  * `tramp-password-end-of-line'
    This specifies the string to use for terminating the line after
    submitting the password.  If this method parameter is nil, then the
    value of the normal variable `tramp-default-password-end-of-line'
    is used.  This parameter is necessary because the \"plink\" program
    requires any two characters after sending the password.  These do
    not have to be newline or carriage return characters.  Other login
    programs are happy with just one character, the newline character.
    We use \"xy\" as the value for methods using \"plink\".

What does all this mean?  Well, you should specify `tramp-login-program'
for all methods; this program is used to log in to the remote site.  Then,
there are two ways to actually transfer the files between the local and the
remote side.  One way is using an additional rcp-like program.  If you want
to do this, set `tramp-copy-program' in the method.

Another possibility for file transfer is inline transfer, i.e. the
file is passed through the same buffer used by `tramp-login-program'.  In
this case, the file contents need to be protected since the
`tramp-login-program' might use escape codes or the connection might not
be eight-bit clean.  Therefore, file contents are encoded for transit.
See the variables `tramp-local-coding-commands' and
`tramp-remote-coding-commands' for details.

So, to summarize: if the method is an out-of-band method, then you
must specify `tramp-copy-program' and `tramp-copy-args'.  If it is an
inline method, then these two parameters should be nil.  Methods which
are fit for gateways must have `tramp-default-port' at least.

Notes:

When using `su' or `sudo' the phrase `open connection to a remote
host' sounds strange, but it is used nevertheless, for consistency.
No connection is opened to a remote host, but `su' or `sudo' is
started on the local host.  You should specify a remote host
`localhost' or the name of the local host.  Another host name is
useful only in combination with `tramp-default-proxies-alist'.")

(defcustom tramp-default-method
  ;; An external copy method seems to be preferred, because it is much
  ;; more performant for large files, and it hasn't too serious delays
  ;; for small files.  But it must be ensured that there aren't
  ;; permanent password queries.  Either a password agent like
  ;; "ssh-agent" or "Pageant" shall run, or the optional password.el
  ;; package shall be active for password caching.  "scpc" would be
  ;; another good choice because of the "ControlMaster" option, but
  ;; this is a more modern alternative in OpenSSH 4, which cannot be
  ;; taken as default.
  (cond
   ;; PuTTY is installed.
   ((executable-find "pscp")
    (if	(or (fboundp 'password-read)
	    ;; Pageant is running.
	    (and (fboundp 'w32-window-exists-p)
		 (funcall (symbol-function 'w32-window-exists-p)
			  "Pageant" "Pageant")))
	"pscp"
      "plink"))
   ;; There is an ssh installation.
   ((executable-find "scp")
    (if	(or (fboundp 'password-read)
	    ;; ssh-agent is running.
	    (getenv "SSH_AUTH_SOCK")
	    (getenv "SSH_AGENT_PID"))
	"scp"
      "ssh"))
   ;; Fallback.
   (t "ftp"))
  "*Default method to use for transferring files.
See `tramp-methods' for possibilities.
Also see `tramp-default-method-alist'."
  :group 'tramp
  :type 'string)

(defcustom tramp-default-method-alist
  '(("\\`localhost\\'" "\\`root\\'" "su"))
  "*Default method to use for specific host/user pairs.
This is an alist of items (HOST USER METHOD).  The first matching item
specifies the method to use for a file name which does not specify a
method.  HOST and USER are regular expressions or nil, which is
interpreted as a regular expression which always matches.  If no entry
matches, the variable `tramp-default-method' takes effect.

If the file name does not specify the user, lookup is done using the
empty string for the user name.

See `tramp-methods' for a list of possibilities for METHOD."
  :group 'tramp
  :type '(repeat (list (regexp :tag "Host regexp")
		       (regexp :tag "User regexp")
		       (string :tag "Method"))))

(defcustom tramp-default-user
  nil
  "*Default user to use for transferring files.
It is nil by default; otherwise settings in configuration files like
\"~/.ssh/config\" would be overwritten.  Also see `tramp-default-user-alist'.

This variable is regarded as obsolete, and will be removed soon."
  :group 'tramp
  :type '(choice (const nil) string))

(defcustom tramp-default-user-alist
  `(("\\`su\\(do\\)?\\'" nil "root")
    ("\\`r\\(em\\)?\\(cp\\|sh\\)\\|telnet\\|plink1?\\'"
     nil ,(user-login-name)))
  "*Default user to use for specific method/host pairs.
This is an alist of items (METHOD HOST USER).  The first matching item
specifies the user to use for a file name which does not specify a
user.  METHOD and USER are regular expressions or nil, which is
interpreted as a regular expression which always matches.  If no entry
matches, the variable `tramp-default-user' takes effect.

If the file name does not specify the method, lookup is done using the
empty string for the method name."
  :group 'tramp
  :type '(repeat (list (regexp :tag "Method regexp")
		       (regexp :tag "Host regexp")
		       (string :tag "User"))))

(defcustom tramp-default-host
  (system-name)
  "*Default host to use for transferring files.
Useful for su and sudo methods mostly."
  :group 'tramp
  :type 'string)

(defcustom tramp-default-proxies-alist nil
  "*Route to be followed for specific host/user pairs.
This is an alist of items (HOST USER PROXY).  The first matching
item specifies the proxy to be passed for a file name located on
a remote target matching USER@HOST.  HOST and USER are regular
expressions or nil, which is interpreted as a regular expression
which always matches.  PROXY must be a Tramp filename without a
localname part.  Method and user name on PROXY are optional,
which is interpreted with the default values.  PROXY can contain
the patterns %h and %u, which are replaced by the strings
matching HOST or USER, respectively."
  :group 'tramp
  :type '(repeat (list (regexp :tag "Host regexp")
		       (regexp :tag "User regexp")
		       (string :tag "Proxy remote name"))))

(defconst tramp-completion-function-alist-rsh
  '((tramp-parse-rhosts "/etc/hosts.equiv")
    (tramp-parse-rhosts "~/.rhosts"))
  "Default list of (FUNCTION FILE) pairs to be examined for rsh methods.")

(defconst tramp-completion-function-alist-ssh
  '((tramp-parse-rhosts      "/etc/hosts.equiv")
    (tramp-parse-rhosts      "/etc/shosts.equiv")
    (tramp-parse-shosts      "/etc/ssh_known_hosts")
    (tramp-parse-sconfig     "/etc/ssh_config")
    (tramp-parse-shostkeys   "/etc/ssh2/hostkeys")
    (tramp-parse-sknownhosts "/etc/ssh2/knownhosts")
    (tramp-parse-rhosts      "~/.rhosts")
    (tramp-parse-rhosts      "~/.shosts")
    (tramp-parse-shosts      "~/.ssh/known_hosts")
    (tramp-parse-sconfig     "~/.ssh/config")
    (tramp-parse-shostkeys   "~/.ssh2/hostkeys")
    (tramp-parse-sknownhosts "~/.ssh2/knownhosts"))
  "Default list of (FUNCTION FILE) pairs to be examined for ssh methods.")

(defconst tramp-completion-function-alist-telnet
  '((tramp-parse-hosts "/etc/hosts"))
  "Default list of (FUNCTION FILE) pairs to be examined for telnet methods.")

(defconst tramp-completion-function-alist-su
  '((tramp-parse-passwd "/etc/passwd"))
  "Default list of (FUNCTION FILE) pairs to be examined for su methods.")

(defconst tramp-completion-function-alist-putty
  '((tramp-parse-putty
     "HKEY_CURRENT_USER\\Software\\SimonTatham\\PuTTY\\Sessions"))
  "Default list of (FUNCTION REGISTRY) pairs to be examined for putty methods.")

(defvar tramp-completion-function-alist nil
  "*Alist of methods for remote files.
This is a list of entries of the form (NAME PAIR1 PAIR2 ...).
Each NAME stands for a remote access method.  Each PAIR is of the form
\(FUNCTION FILE).  FUNCTION is responsible to extract user names and host
names from FILE for completion.  The following predefined FUNCTIONs exists:

 * `tramp-parse-rhosts'      for \"~/.rhosts\" like files,
 * `tramp-parse-shosts'      for \"~/.ssh/known_hosts\" like files,
 * `tramp-parse-sconfig'     for \"~/.ssh/config\" like files,
 * `tramp-parse-shostkeys'   for \"~/.ssh2/hostkeys/*\" like files,
 * `tramp-parse-sknownhosts' for \"~/.ssh2/knownhosts/*\" like files,
 * `tramp-parse-hosts'       for \"/etc/hosts\" like files,
 * `tramp-parse-passwd'      for \"/etc/passwd\" like files.
 * `tramp-parse-netrc'       for \"~/.netrc\" like files.
 * `tramp-parse-putty'       for PuTTY registry keys.

FUNCTION can also be a customer defined function.  For more details see
the info pages.")

(eval-after-load "tramp"
  '(progn
     (tramp-set-completion-function
      "rcp" tramp-completion-function-alist-rsh)
     (tramp-set-completion-function
      "scp" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "scp1" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "scp2" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "scp1_old" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "scp2_old" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "rsync" tramp-completion-function-alist-rsh)
     (tramp-set-completion-function
      "remcp" tramp-completion-function-alist-rsh)
     (tramp-set-completion-function
      "rsh" tramp-completion-function-alist-rsh)
     (tramp-set-completion-function
      "ssh" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "ssh1" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "ssh2" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "ssh1_old" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "ssh2_old" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "remsh" tramp-completion-function-alist-rsh)
     (tramp-set-completion-function
      "telnet" tramp-completion-function-alist-telnet)
     (tramp-set-completion-function
      "su" tramp-completion-function-alist-su)
     (tramp-set-completion-function
      "sudo" tramp-completion-function-alist-su)
     (tramp-set-completion-function
      "scpx" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "sshx" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "krlogin" tramp-completion-function-alist-rsh)
     (tramp-set-completion-function
      "plink" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "plink1" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "plinkx" tramp-completion-function-alist-putty)
     (tramp-set-completion-function
      "pscp" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "fcp" tramp-completion-function-alist-ssh)))

(defconst tramp-echo-mark "_echo\b\b\b\b\b"
  "String mark to be transmitted around shell commands.
Used to separate their echo from the output they produce.  This
will only be used if we cannot disable remote echo via stty.
This string must have no effect on the remote shell except for
producing some echo which can later be detected by
`tramp-echoed-echo-mark-regexp'.  Using some characters followed
by an equal number of backspaces to erase them will usually
suffice.")

(defconst tramp-echoed-echo-mark-regexp "_echo\\(\b\\( \b\\)?\\)\\{5\\}"
  "Regexp which matches `tramp-echo-mark' as it gets echoed by
the remote shell.")

(defcustom tramp-rsh-end-of-line "\n"
  "*String used for end of line in rsh connections.
I don't think this ever needs to be changed, so please tell me about it
if you need to change this.
Also see the method parameter `tramp-password-end-of-line' and the normal
variable `tramp-default-password-end-of-line'."
  :group 'tramp
  :type 'string)

(defcustom tramp-default-password-end-of-line
  tramp-rsh-end-of-line
  "*String used for end of line after sending a password.
This variable provides the default value for the method parameter
`tramp-password-end-of-line', see `tramp-methods' for more details.

It seems that people using plink under Windows need to send
\"\\r\\n\" (carriage-return, then newline) after a password, but just
\"\\n\" after all other lines.  This variable can be used for the
password, see `tramp-rsh-end-of-line' for the other cases.

The default value is to use the same value as `tramp-rsh-end-of-line'."
  :group 'tramp
  :type 'string)

;; "getconf PATH" yields:
;; HP-UX: /usr/bin:/usr/ccs/bin:/opt/ansic/bin:/opt/langtools/bin:/opt/fortran/bin
;; Solaris: /usr/xpg4/bin:/usr/ccs/bin:/usr/bin:/opt/SUNWspro/bin
;; GNU/Linux (Debian, Suse): /bin:/usr/bin
;; FreeBSD: /usr/bin:/bin:/usr/sbin:/sbin: - beware trailing ":"!
(defcustom tramp-remote-path
  '(tramp-default-remote-path "/usr/sbin" "/usr/local/bin"
    "/local/bin" "/local/freeware/bin" "/local/gnu/bin"
    "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin")
  "*List of directories to search for executables on remote host.
For every remote host, this variable will be set buffer local,
keeping the list of existing directories on that host.

You can use `~' in this list, but when searching for a shell which groks
tilde expansion, all directory names starting with `~' will be ignored.

`Default Directories' represent the list of directories given by
the command \"getconf PATH\".  It is recommended to use this
entry on top of this list, because these are the default
directories for POSIX compatible commands."
  :group 'tramp
  :type '(repeat (choice
		  (const :tag "Default Directories" tramp-default-remote-path)
		  (string :tag "Directory"))))

(defcustom tramp-remote-process-environment
  `("HISTFILE=$HOME/.tramp_history" "HISTSIZE=1" "LC_ALL=C"
    ,(concat "TERM=" tramp-terminal-type)
    "CDPATH=" "HISTORY=" "MAIL=" "MAILCHECK=" "MAILPATH="
    "autocorrect=" "correct=")

  "*List of environment variables to be set on the remote host.

Each element should be a string of the form ENVVARNAME=VALUE.  An
entry ENVVARNAME= diables the corresponding environment variable,
which might have been set in the init files like ~/.profile.

Special handling is applied to the PATH environment, which should
not be set here. Instead of, it should be set via `tramp-remote-path'."
  :group 'tramp
  :type '(repeat string))

(defcustom tramp-login-prompt-regexp
  ".*ogin\\( .*\\)?: *"
  "*Regexp matching login-like prompts.
The regexp should match at end of buffer.

Sometimes the prompt is reported to look like \"login as:\"."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-shell-prompt-pattern
  "^[^#$%>\n]*[#$%>] *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*"
  "Regexp to match prompts from remote shell.
Normally, Tramp expects you to configure `shell-prompt-pattern'
correctly, but sometimes it happens that you are connecting to a
remote host which sends a different kind of shell prompt.  Therefore,
Tramp recognizes things matched by `shell-prompt-pattern' as prompt,
and also things matched by this variable.  The default value of this
variable is similar to the default value of `shell-prompt-pattern',
which should work well in many cases."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-password-prompt-regexp
  "^.*\\([pP]assword\\|[pP]assphrase\\).*:\^@? *"
  "*Regexp matching password-like prompts.
The regexp should match at end of buffer.

The `sudo' program appears to insert a `^@' character into the prompt."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-wrong-passwd-regexp
  (concat "^.*"
	  ;; These strings should be on the last line
	  (regexp-opt '("Permission denied"
			"Login incorrect"
			"Login Incorrect"
			"Connection refused"
			"Connection closed"
			"Sorry, try again."
			"Name or service not known"
			"Host key verification failed."
			"No supported authentication methods left to try!"
			"Tramp connection closed") t)
	  ".*"
	  "\\|"
	  "^.*\\("
	  ;; Here comes a list of regexes, separated by \\|
	  "Received signal [0-9]+"
	  "\\).*")
  "*Regexp matching a `login failed' message.
The regexp should match at end of buffer."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-yesno-prompt-regexp
  (concat
   (regexp-opt '("Are you sure you want to continue connecting (yes/no)?") t)
   "\\s-*")
  "Regular expression matching all yes/no queries which need to be confirmed.
The confirmation should be done with yes or no.
The regexp should match at end of buffer.
See also `tramp-yn-prompt-regexp'."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-yn-prompt-regexp
  (concat
   (regexp-opt '("Store key in cache? (y/n)"
		 "Update cached key? (y/n, Return cancels connection)") t)
   "\\s-*")
  "Regular expression matching all y/n queries which need to be confirmed.
The confirmation should be done with y or n.
The regexp should match at end of buffer.
See also `tramp-yesno-prompt-regexp'."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-terminal-prompt-regexp
  (concat "\\("
	  "TERM = (.*)"
	  "\\|"
	  "Terminal type\\? \\[.*\\]"
	  "\\)\\s-*")
  "Regular expression matching all terminal setting prompts.
The regexp should match at end of buffer.
The answer will be provided by `tramp-action-terminal', which see."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-operation-not-permitted-regexp
  (concat "\\(" "preserving times.*" "\\|" "set mode" "\\)" ":\\s-*"
	  (regexp-opt '("Operation not permitted") t))
  "Regular expression matching keep-date problems in (s)cp operations.
Copying has been performed successfully already, so this message can
be ignored safely."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-copy-failed-regexp
  (concat "\\(.+: "
          (regexp-opt '("Permission denied"
                        "not a regular file"
                        "is a directory"
                        "No such file or directory") t)
          "\\)\\s-*")
  "Regular expression matching copy problems in (s)cp operations."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-process-alive-regexp
  ""
  "Regular expression indicating a process has finished.
In fact this expression is empty by intention, it will be used only to
check regularly the status of the associated process.
The answer will be provided by `tramp-action-process-alive',
`tramp-action-out-of-band', which see."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-temp-name-prefix "tramp."
  "*Prefix to use for temporary files.
If this is a relative file name (such as \"tramp.\"), it is considered
relative to the directory name returned by the function
`tramp-compat-temporary-file-directory' (which see).  It may also be an
absolute file name; don't forget to include a prefix for the filename
part, though."
  :group 'tramp
  :type 'string)

(defcustom tramp-sh-extra-args '(("/bash\\'" . "-norc -noprofile"))
  "*Alist specifying extra arguments to pass to the remote shell.
Entries are (REGEXP . ARGS) where REGEXP is a regular expression
matching the shell file name and ARGS is a string specifying the
arguments.

This variable is only used when Tramp needs to start up another shell
for tilde expansion.  The extra arguments should typically prevent the
shell from reading its init file."
  :group 'tramp
  ;; This might be the wrong way to test whether the widget type
  ;; `alist' is available.  Who knows the right way to test it?
  :type (if (get 'alist 'widget-type)
	    '(alist :key-type string :value-type string)
	  '(repeat (cons string string))))

;; XEmacs is distributed with few Lisp packages.  Further packages are
;; installed using EFS.  If we use a unified filename format, then
;; Tramp is required in addition to EFS.  (But why can't Tramp just
;; disable EFS when Tramp is loaded?  Then XEmacs can ship with EFS
;; just like before.)  Another reason for using a separate filename
;; syntax on XEmacs is that EFS hooks into XEmacs in many places, but
;; Tramp only knows how to deal with `file-name-handler-alist', not
;; the other places.

;; Currently, we have the choice between 'ftp, 'sep, and 'url.
;;;###autoload
(defcustom tramp-syntax
  (if (featurep 'xemacs) 'sep 'ftp)
  "Tramp filename syntax to be used.

It can have the following values:

  'ftp -- Ange-FTP respective EFS like syntax (GNU Emacs default)
  'sep -- Syntax as defined for XEmacs (not available yet for GNU Emacs)
  'url -- URL-like syntax."
  :group 'tramp
  :type (if (featurep 'xemacs)
	    '(choice (const :tag "EFS"    ftp)
		     (const :tag "XEmacs" sep)
		     (const :tag "URL"    url))
	  '(choice (const :tag "Ange-FTP" ftp)
		   (const :tag "URL"      url))))

(defconst tramp-prefix-format
  (cond ((equal tramp-syntax 'ftp) "/")
	((equal tramp-syntax 'sep) "/[")
	((equal tramp-syntax 'url) "/")
	(t (error "Wrong `tramp-syntax' defined")))
  "*String matching the very beginning of Tramp file names.
Used in `tramp-make-tramp-file-name'.")

(defconst tramp-prefix-regexp
  (concat "^" (regexp-quote tramp-prefix-format))
  "*Regexp matching the very beginning of Tramp file names.
Should always start with \"^\". Derived from `tramp-prefix-format'.")

(defconst tramp-method-regexp
  "[a-zA-Z_0-9-]+"
  "*Regexp matching methods identifiers.")

(defconst tramp-postfix-method-format
  (cond ((equal tramp-syntax 'ftp) ":")
	((equal tramp-syntax 'sep) "/")
	((equal tramp-syntax 'url) "://")
	(t (error "Wrong `tramp-syntax' defined")))
  "*String matching delimeter between method and user or host names.
Used in `tramp-make-tramp-file-name'.")

(defconst tramp-postfix-method-regexp
  (regexp-quote tramp-postfix-method-format)
  "*Regexp matching delimeter between method and user or host names.
Derived from `tramp-postfix-method-format'.")

(defconst tramp-user-regexp
  "[^:/ \t]+"
  "*Regexp matching user names.")

(defconst tramp-postfix-user-format
  "@"
  "*String matching delimeter between user and host names.
Used in `tramp-make-tramp-file-name'.")

(defconst tramp-postfix-user-regexp
  (regexp-quote tramp-postfix-user-format)
  "*Regexp matching delimeter between user and host names.
Derived from `tramp-postfix-user-format'.")

(defconst tramp-host-regexp
  "[a-zA-Z0-9_.-]+"
  "*Regexp matching host names.")

(defconst tramp-prefix-port-format
  (cond ((equal tramp-syntax 'ftp) "#")
	((equal tramp-syntax 'sep) "#")
	((equal tramp-syntax 'url) ":")
	(t (error "Wrong `tramp-syntax' defined")))
  "*String matching delimeter between host names and port numbers.")

(defconst tramp-prefix-port-regexp
  (regexp-quote tramp-prefix-port-format)
  "*Regexp matching delimeter between host names and port numbers.
Derived from `tramp-prefix-port-format'.")

(defconst tramp-port-regexp
  "[0-9]+"
  "*Regexp matching port numbers.")

(defconst tramp-host-with-port-regexp
  (concat "\\(" tramp-host-regexp "\\)"
	        tramp-prefix-port-regexp
	  "\\(" tramp-port-regexp "\\)")
  "*Regexp matching host names with port numbers.")

(defconst tramp-postfix-host-format
  (cond ((equal tramp-syntax 'ftp) ":")
	((equal tramp-syntax 'sep) "]")
	((equal tramp-syntax 'url) "")
	(t (error "Wrong `tramp-syntax' defined")))
  "*String matching delimeter between host names and localnames.
Used in `tramp-make-tramp-file-name'.")

(defconst tramp-postfix-host-regexp
  (regexp-quote tramp-postfix-host-format)
  "*Regexp matching delimeter between host names and localnames.
Derived from `tramp-postfix-host-format'.")

(defconst tramp-localname-regexp
  ".*$"
  "*Regexp matching localnames.")

;; File name format.

(defconst tramp-file-name-structure
  (list
   (concat
    tramp-prefix-regexp
    "\\(" "\\(" tramp-method-regexp "\\)" tramp-postfix-method-regexp "\\)?"
    "\\(" "\\(" tramp-user-regexp "\\)"   tramp-postfix-user-regexp   "\\)?"
    "\\(" tramp-host-regexp
          "\\(" tramp-prefix-port-regexp  tramp-port-regexp "\\)?" "\\)?"
    tramp-postfix-host-regexp
    "\\(" tramp-localname-regexp "\\)")
   2 4 5 7)

  "*List of five elements (REGEXP METHOD USER HOST FILE), detailing \
the Tramp file name structure.

The first element REGEXP is a regular expression matching a Tramp file
name.  The regex should contain parentheses around the method name,
the user name, the host name, and the file name parts.

The second element METHOD is a number, saying which pair of
parentheses matches the method name.  The third element USER is
similar, but for the user name.  The fourth element HOST is similar,
but for the host name.  The fifth element FILE is for the file name.
These numbers are passed directly to `match-string', which see.  That
means the opening parentheses are counted to identify the pair.

See also `tramp-file-name-regexp'.")

;;;###autoload
(defconst tramp-file-name-regexp-unified
  "\\`/[^/:]+:"
  "Value for `tramp-file-name-regexp' for unified remoting.
Emacs (not XEmacs) uses a unified filename syntax for Ange-FTP and
Tramp.  See `tramp-file-name-structure' for more explanations.")

;;;###autoload
(defconst tramp-file-name-regexp-separate
  "\\`/\\[.*\\]"
  "Value for `tramp-file-name-regexp' for separate remoting.
XEmacs uses a separate filename syntax for Tramp and EFS.
See `tramp-file-name-structure' for more explanations.")

;;;###autoload
(defconst tramp-file-name-regexp-url
  "\\`/[^/:]+://"
  "Value for `tramp-file-name-regexp' for URL-like remoting.
See `tramp-file-name-structure' for more explanations.")

;;;###autoload
(defconst tramp-file-name-regexp
  (cond ((equal tramp-syntax 'ftp) tramp-file-name-regexp-unified)
	((equal tramp-syntax 'sep) tramp-file-name-regexp-separate)
	((equal tramp-syntax 'url) tramp-file-name-regexp-url)
	(t (error "Wrong `tramp-syntax' defined")))
  "*Regular expression matching file names handled by Tramp.
This regexp should match Tramp file names but no other file names.
\(When tramp.el is loaded, this regular expression is prepended to
`file-name-handler-alist', and that is searched sequentially.  Thus,
if the Tramp entry appears rather early in the `file-name-handler-alist'
and is a bit too general, then some files might be considered Tramp
files which are not really Tramp files.

Please note that the entry in `file-name-handler-alist' is made when
this file (tramp.el) is loaded.  This means that this variable must be set
before loading tramp.el.  Alternatively, `file-name-handler-alist' can be
updated after changing this variable.

Also see `tramp-file-name-structure'.")

;;;###autoload
(defconst tramp-completion-file-name-regexp-unified
  (if (memq system-type '(cygwin windows-nt))
      "^\\([a-zA-Z]:\\)?/$\\|^\\([a-zA-Z]:\\)?/[^/:][^/]*$"
    "^/$\\|^/[^/:][^/]*$")
  "Value for `tramp-completion-file-name-regexp' for unified remoting.
Emacs (not XEmacs) uses a unified filename syntax for Ange-FTP and
Tramp.  See `tramp-file-name-structure' for more explanations.")

;;;###autoload
(defconst tramp-completion-file-name-regexp-separate
  (if (memq system-type '(cygwin windows-nt))
      "^\\([a-zA-Z]:\\)?/\\([[][^]]*\\)?$"
    "^/\\([[][^]]*\\)?$")
  "Value for `tramp-completion-file-name-regexp' for separate remoting.
XEmacs uses a separate filename syntax for Tramp and EFS.
See `tramp-file-name-structure' for more explanations.")

;;;###autoload
(defconst tramp-completion-file-name-regexp-url
  (if (memq system-type '(cygwin windows-nt))
      "^\\([a-zA-Z]:\\)?/$\\|^\\([a-zA-Z]:\\)?/[^/:]+\\(:\\(/\\(/[^/]*\\)?\\)?\\)?$"
    "^/$\\|^/[^/:]+\\(:\\(/\\(/[^/]*\\)?\\)?\\)?$")
  "Value for `tramp-completion-file-name-regexp' for URL-like remoting.
See `tramp-file-name-structure' for more explanations.")

;;;###autoload
(defconst tramp-completion-file-name-regexp
  (cond ((equal tramp-syntax 'ftp) tramp-completion-file-name-regexp-unified)
	((equal tramp-syntax 'sep) tramp-completion-file-name-regexp-separate)
	((equal tramp-syntax 'url) tramp-completion-file-name-regexp-url)
	(t (error "Wrong `tramp-syntax' defined")))
  "*Regular expression matching file names handled by Tramp completion.
This regexp should match partial Tramp file names only.

Please note that the entry in `file-name-handler-alist' is made when
this file (tramp.el) is loaded.  This means that this variable must be set
before loading tramp.el.  Alternatively, `file-name-handler-alist' can be
updated after changing this variable.

Also see `tramp-file-name-structure'.")

(defconst tramp-actions-before-shell
  '((tramp-login-prompt-regexp tramp-action-login)
    (tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (shell-prompt-pattern tramp-action-succeed)
    (tramp-shell-prompt-pattern tramp-action-succeed)
    (tramp-yesno-prompt-regexp tramp-action-yesno)
    (tramp-yn-prompt-regexp tramp-action-yn)
    (tramp-terminal-prompt-regexp tramp-action-terminal)
    (tramp-process-alive-regexp tramp-action-process-alive))
  "List of pattern/action pairs.
Whenever a pattern matches, the corresponding action is performed.
Each item looks like (PATTERN ACTION).

The PATTERN should be a symbol, a variable.  The value of this
variable gives the regular expression to search for.  Note that the
regexp must match at the end of the buffer, \"\\'\" is implicitly
appended to it.

The ACTION should also be a symbol, but a function.  When the
corresponding PATTERN matches, the ACTION function is called.")

(defconst tramp-actions-copy-out-of-band
  '((tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (tramp-copy-failed-regexp tramp-action-permission-denied)
    (tramp-process-alive-regexp tramp-action-out-of-band))
  "List of pattern/action pairs.
This list is used for copying/renaming with out-of-band methods.

See `tramp-actions-before-shell' for more info.")

;; Chunked sending kludge.  We set this to 500 for black-listed constellations
;; known to have a bug in `process-send-string'; some ssh connections appear
;; to drop bytes when data is sent too quickly.  There is also a connection
;; buffer local variable, which is computed depending on remote host properties
;; when `tramp-chunksize' is zero or nil.
(defcustom tramp-chunksize
  (when (and (not (featurep 'xemacs))
	     (memq system-type '(hpux)))
    500)
;; Parentheses in docstring starting at beginning of line are escaped.
;; Fontification is messed up when
;; `open-paren-in-column-0-is-defun-start' set to t.
  "*If non-nil, chunksize for sending input to local process.
It is necessary only on systems which have a buggy `process-send-string'
implementation.  The necessity, whether this variable must be set, can be
checked via the following code:

  (with-temp-buffer
    (let* ((user \"xxx\") (host \"yyy\")
           (init 0) (step 50)
           (sent init) (received init))
      (while (= sent received)
        (setq sent (+ sent step))
        (erase-buffer)
        (let ((proc (start-process (buffer-name) (current-buffer)
                                   \"ssh\" \"-l\" user host \"wc\" \"-c\")))
          (when (memq (process-status proc) '(run open))
            (process-send-string proc (make-string sent ?\\ ))
            (process-send-eof proc)
            (process-send-eof proc))
          (while (not (progn (goto-char (point-min))
                             (re-search-forward \"\\\\w+\" (point-max) t)))
            (accept-process-output proc 1))
          (when (memq (process-status proc) '(run open))
            (setq received (string-to-number (match-string 0)))
            (delete-process proc)
            (message \"Bytes sent: %s\\tBytes received: %s\" sent received)
            (sit-for 0))))
      (if (> sent (+ init step))
          (message \"You should set `tramp-chunksize' to a maximum of %s\"
                   (- sent step))
        (message \"Test does not work\")
        (display-buffer (current-buffer))
        (sit-for 30))))

In the Emacs normally running Tramp, evaluate the above code
\(replace \"xxx\" and \"yyy\" by the remote user and host name,
respectively).  You can do this, for example, by pasting it into
the `*scratch*' buffer and then hitting C-j with the cursor after the
last closing parenthesis.  Note that it works only if you have configured
\"ssh\" to run without password query, see ssh-agent(1).

You will see the number of bytes sent successfully to the remote host.
If that number exceeds 1000, you can stop the execution by hitting
C-g, because your Emacs is likely clean.

When it is necessary to set `tramp-chunksize', you might consider to
use an out-of-the-band method (like \"scp\") instead of an internal one
\(like \"ssh\"), because setting `tramp-chunksize' to non-nil decreases
performance.

If your Emacs is buggy, the code stops and gives you an indication
about the value `tramp-chunksize' should be set.  Maybe you could just
experiment a bit, e.g. changing the values of `init' and `step'
in the third line of the code.

Please raise a bug report via \"M-x tramp-bug\" if your system needs
this variable to be set as well."
  :group 'tramp
  :type '(choice (const nil) integer))

;; Logging in to a remote host normally requires obtaining a pty.  But
;; Emacs on MacOS X has process-connection-type set to nil by default,
;; so on those systems Tramp doesn't obtain a pty.  Here, we allow
;; for an override of the system default.
(defcustom tramp-process-connection-type t
  "Overrides `process-connection-type' for connections from Tramp.
Tramp binds process-connection-type to the value given here before
opening a connection to a remote host."
  :group 'tramp
  :type '(choice (const nil) (const t) (const pty)))

(defcustom tramp-completion-reread-directory-timeout 10
  "Defines seconds since last remote command before rereading a directory.
A remote directory might have changed its contents.  In order to
make it visible during file name completion in the minibuffer,
Tramp flushes its cache and rereads the directory contents when
more than `tramp-completion-reread-directory-timeout' seconds
have been gone since last remote command execution.  A value of 0
would require an immediate reread during filename completion, nil
means to use always cached values for the directory contents."
  :group 'tramp
  :type '(choice (const nil) integer))

;;; Internal Variables:

(defvar tramp-end-of-output
  (format
   "%s///%s%s"
   tramp-rsh-end-of-line
   (md5 (concat (prin1-to-string process-environment) (current-time-string)))
   tramp-rsh-end-of-line)
  "String used to recognize end of output.")

(defvar tramp-current-method nil
  "Connection method for this *tramp* buffer.")

(defvar tramp-current-user nil
  "Remote login name for this *tramp* buffer.")

(defvar tramp-current-host nil
  "Remote host for this *tramp* buffer.")

(defconst tramp-uudecode
  "(echo begin 600 /tmp/tramp.$$; tail +2) | uudecode
cat /tmp/tramp.$$
rm -f /tmp/tramp.$$"
  "Shell function to implement `uudecode' to standard output.
Many systems support `uudecode -o /dev/stdout' or `uudecode -o -'
for this or `uudecode -p', but some systems don't, and for them
we have this shell function.")

;; Perl script to implement `file-attributes' in a Lisp `read'able
;; output.  If you are hacking on this, note that you get *no* output
;; unless this spits out a complete line, including the '\n' at the
;; end.
;; The device number is returned as "-1", because there will be a virtual
;; device number set in `tramp-handle-file-attributes'
(defconst tramp-perl-file-attributes
  "%s -e '
@stat = lstat($ARGV[0]);
if (($stat[2] & 0170000) == 0120000)
{
    $type = readlink($ARGV[0]);
    $type = \"\\\"$type\\\"\";
}
elsif (($stat[2] & 0170000) == 040000)
{
    $type = \"t\";
}
else
{
    $type = \"nil\"
};
$uid = ($ARGV[1] eq \"integer\") ? $stat[4] : \"\\\"\" . getpwuid($stat[4]) . \"\\\"\";
$gid = ($ARGV[1] eq \"integer\") ? $stat[5] : \"\\\"\" . getgrgid($stat[5]) . \"\\\"\";
printf(
    \"(%%s %%u %%s %%s (%%u %%u) (%%u %%u) (%%u %%u) %%u.0 %%u t (%%u . %%u) -1)\\n\",
    $type,
    $stat[3],
    $uid,
    $gid,
    $stat[8] >> 16 & 0xffff,
    $stat[8] & 0xffff,
    $stat[9] >> 16 & 0xffff,
    $stat[9] & 0xffff,
    $stat[10] >> 16 & 0xffff,
    $stat[10] & 0xffff,
    $stat[7],
    $stat[2],
    $stat[1] >> 16 & 0xffff,
    $stat[1] & 0xffff
);' \"$1\" \"$2\" \"$3\" 2>/dev/null"
  "Perl script to produce output suitable for use with `file-attributes'
on the remote file system.
Escape sequence %s is replaced with name of Perl binary.
This string is passed to `format', so percent characters need to be doubled.")

(defconst tramp-perl-directory-files-and-attributes
  "%s -e '
chdir($ARGV[0]) or printf(\"\\\"Cannot change to $ARGV[0]: $''!''\\\"\\n\"), exit();
opendir(DIR,\".\") or printf(\"\\\"Cannot open directory $ARGV[0]: $''!''\\\"\\n\"), exit();
@list = readdir(DIR);
closedir(DIR);
$n = scalar(@list);
printf(\"(\\n\");
for($i = 0; $i < $n; $i++)
{
    $filename = $list[$i];
    @stat = lstat($filename);
    if (($stat[2] & 0170000) == 0120000)
    {
        $type = readlink($filename);
        $type = \"\\\"$type\\\"\";
    }
    elsif (($stat[2] & 0170000) == 040000)
    {
        $type = \"t\";
    }
    else
    {
        $type = \"nil\"
    };
    $uid = ($ARGV[1] eq \"integer\") ? $stat[4] : \"\\\"\" . getpwuid($stat[4]) . \"\\\"\";
    $gid = ($ARGV[1] eq \"integer\") ? $stat[5] : \"\\\"\" . getgrgid($stat[5]) . \"\\\"\";
    printf(
        \"(\\\"%%s\\\" %%s %%u %%s %%s (%%u %%u) (%%u %%u) (%%u %%u) %%u.0 %%u t (%%u . %%u) (%%u %%u))\\n\",
        $filename,
        $type,
        $stat[3],
        $uid,
        $gid,
        $stat[8] >> 16 & 0xffff,
        $stat[8] & 0xffff,
        $stat[9] >> 16 & 0xffff,
        $stat[9] & 0xffff,
        $stat[10] >> 16 & 0xffff,
        $stat[10] & 0xffff,
        $stat[7],
        $stat[2],
        $stat[1] >> 16 & 0xffff,
        $stat[1] & 0xffff,
        $stat[0] >> 16 & 0xffff,
        $stat[0] & 0xffff);
}
printf(\")\\n\");' \"$1\" \"$2\" \"$3\" 2>/dev/null"
  "Perl script implementing `directory-files-attributes' as Lisp `read'able
output.
Escape sequence %s is replaced with name of Perl binary.
This string is passed to `format', so percent characters need to be doubled.")

;; ;; These two use uu encoding.
;; (defvar tramp-perl-encode "%s -e'\
;; print qq(begin 644 xxx\n);
;; my $s = q();
;; my $res = q();
;; while (read(STDIN, $s, 45)) {
;;     print pack(q(u), $s);
;; }
;; print qq(`\n);
;; print qq(end\n);
;; '"
;;   "Perl program to use for encoding a file.
;; Escape sequence %s is replaced with name of Perl binary.")

;; (defvar tramp-perl-decode "%s -ne '
;; print unpack q(u), $_;
;; '"
;;   "Perl program to use for decoding a file.
;; Escape sequence %s is replaced with name of Perl binary.")

;; These two use base64 encoding.
(defconst tramp-perl-encode-with-module
  "%s -MMIME::Base64 -0777 -ne 'print encode_base64($_)' 2>/dev/null"
  "Perl program to use for encoding a file.
Escape sequence %s is replaced with name of Perl binary.
This string is passed to `format', so percent characters need to be doubled.
This implementation requires the MIME::Base64 Perl module to be installed
on the remote host.")

(defconst tramp-perl-decode-with-module
  "%s -MMIME::Base64 -0777 -ne 'print decode_base64($_)' 2>/dev/null"
  "Perl program to use for decoding a file.
Escape sequence %s is replaced with name of Perl binary.
This string is passed to `format', so percent characters need to be doubled.
This implementation requires the MIME::Base64 Perl module to be installed
on the remote host.")

(defconst tramp-perl-encode
  "%s -e '
# This script contributed by Juanma Barranquero <lektu@terra.es>.
# Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008
#   Free Software Foundation, Inc.
use strict;

my %%trans = do {
    my $i = 0;
    map {(substr(unpack(q(B8), chr $i++), 2, 6), $_)}
      split //, q(ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/);
};

binmode(\\*STDIN);

# We read in chunks of 54 bytes, to generate output lines
# of 72 chars (plus end of line)
$/ = \\54;

while (my $data = <STDIN>) {
    my $pad = q();

    # Only for the last chunk, and only if did not fill the last three-byte packet
    if (eof) {
        my $mod = length($data) %% 3;
        $pad = q(=) x (3 - $mod) if $mod;
    }

    # Not the fastest method, but it is simple: unpack to binary string, split
    # by groups of 6 bits and convert back from binary to byte; then map into
    # the translation table
    print
      join q(),
        map($trans{$_},
            (substr(unpack(q(B*), $data) . q(00000), 0, 432) =~ /....../g)),
              $pad,
                qq(\\n);
}' 2>/dev/null"
  "Perl program to use for encoding a file.
Escape sequence %s is replaced with name of Perl binary.
This string is passed to `format', so percent characters need to be doubled.")

(defconst tramp-perl-decode
  "%s -e '
# This script contributed by Juanma Barranquero <lektu@terra.es>.
# Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008
#   Free Software Foundation, Inc.
use strict;

my %%trans = do {
    my $i = 0;
    map {($_, substr(unpack(q(B8), chr $i++), 2, 6))}
      split //, q(ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/)
};

my %%bytes = map {(unpack(q(B8), chr $_), chr $_)} 0 .. 255;

binmode(\\*STDOUT);

# We are going to accumulate into $pending to accept any line length
# (we do not check they are <= 76 chars as the RFC says)
my $pending = q();

while (my $data = <STDIN>) {
    chomp $data;

    # If we find one or two =, we have reached the end and
    # any following data is to be discarded
    my $finished = $data =~ s/(==?).*/$1/;
    $pending .= $data;

    my $len = length($pending);
    my $chunk = substr($pending, 0, $len & ~3);
    $pending = substr($pending, $len & ~3 + 1);

    # Easy method: translate from chars to (pregenerated) six-bit packets, join,
    # split in 8-bit chunks and convert back to char.
    print join q(),
      map $bytes{$_},
        ((join q(), map {$trans{$_} || q()} split //, $chunk) =~ /......../g);

    last if $finished;
}' 2>/dev/null"
  "Perl program to use for decoding a file.
Escape sequence %s is replaced with name of Perl binary.
This string is passed to `format', so percent characters need to be doubled.")

(defconst tramp-file-mode-type-map
  '((0  . "-")  ; Normal file (SVID-v2 and XPG2)
    (1  . "p")  ; fifo
    (2  . "c")  ; character device
    (3  . "m")  ; multiplexed character device (v7)
    (4  . "d")  ; directory
    (5  . "?")  ; Named special file (XENIX)
    (6  . "b")  ; block device
    (7  . "?")  ; multiplexed block device (v7)
    (8  . "-")  ; regular file
    (9  . "n")  ; network special file (HP-UX)
    (10 . "l")  ; symlink
    (11 . "?")  ; ACL shadow inode (Solaris, not userspace)
    (12 . "s")  ; socket
    (13 . "D")  ; door special (Solaris)
    (14 . "w")) ; whiteout (BSD)
  "A list of file types returned from the `stat' system call.
This is used to map a mode number to a permission string.")

;; New handlers should be added here.  The following operations can be
;; handled using the normal primitives: file-name-as-directory,
;; file-name-sans-versions, get-file-buffer.
(defconst tramp-file-name-handler-alist
  '((load . tramp-handle-load)
    (make-symbolic-link . tramp-handle-make-symbolic-link)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    (file-truename . tramp-handle-file-truename)
    (file-exists-p . tramp-handle-file-exists-p)
    (file-directory-p . tramp-handle-file-directory-p)
    (file-executable-p . tramp-handle-file-executable-p)
    (file-readable-p . tramp-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-writable-p . tramp-handle-file-writable-p)
    (file-ownership-preserved-p . tramp-handle-file-ownership-preserved-p)
    (file-newer-than-file-p . tramp-handle-file-newer-than-file-p)
    (file-attributes . tramp-handle-file-attributes)
    (file-modes . tramp-handle-file-modes)
    (directory-files . tramp-handle-directory-files)
    (directory-files-and-attributes . tramp-handle-directory-files-and-attributes)
    (file-name-all-completions . tramp-handle-file-name-all-completions)
    (file-name-completion . tramp-handle-file-name-completion)
    (add-name-to-file . tramp-handle-add-name-to-file)
    (copy-file . tramp-handle-copy-file)
    (rename-file . tramp-handle-rename-file)
    (set-file-modes . tramp-handle-set-file-modes)
    (set-file-times . tramp-handle-set-file-times)
    (make-directory . tramp-handle-make-directory)
    (delete-directory . tramp-handle-delete-directory)
    (delete-file . tramp-handle-delete-file)
    (directory-file-name . tramp-handle-directory-file-name)
    ;; `executable-find' is not official yet.
    (executable-find . tramp-handle-executable-find)
    (start-file-process . tramp-handle-start-file-process)
    (process-file . tramp-handle-process-file)
    (shell-command . tramp-handle-shell-command)
    (insert-directory . tramp-handle-insert-directory)
    (expand-file-name . tramp-handle-expand-file-name)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (file-local-copy . tramp-handle-file-local-copy)
    (file-remote-p . tramp-handle-file-remote-p)
    (insert-file-contents . tramp-handle-insert-file-contents)
    (insert-file-contents-literally
     . tramp-handle-insert-file-contents-literally)
    (write-region . tramp-handle-write-region)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (unhandled-file-name-directory . tramp-handle-unhandled-file-name-directory)
    (dired-compress-file . tramp-handle-dired-compress-file)
    (dired-recursive-delete-directory
     . tramp-handle-dired-recursive-delete-directory)
    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime))
  "Alist of handler functions.
Operations not mentioned here will be handled by the normal Emacs functions.")

;; Handlers for partial Tramp file names.  For Emacs just
;; `file-name-all-completions' is needed.
;;;###autoload
(defconst tramp-completion-file-name-handler-alist
  '((file-name-all-completions . tramp-completion-handle-file-name-all-completions)
    (file-name-completion . tramp-completion-handle-file-name-completion))
  "Alist of completion handler functions.
Used for file names matching `tramp-file-name-regexp'. Operations not
mentioned here will be handled by `tramp-file-name-handler-alist' or the
normal Emacs functions.")

;; Handlers for foreign methods, like FTP or SMB, shall be plugged here.
(defvar tramp-foreign-file-name-handler-alist
  ;; (identity . tramp-sh-file-name-handler) should always be the last
  ;; entry, since `identity' always matches.
  '((identity . tramp-sh-file-name-handler))
  "Alist of elements (FUNCTION . HANDLER) for foreign methods handled specially.
If (FUNCTION FILENAME) returns non-nil, then all I/O on that file is done by
calling HANDLER.")

;;; Internal functions which must come first:

(defsubst tramp-debug-message (vec fmt-string &rest args)
  "Append message to debug buffer.
Message is formatted with FMT-STRING as control string and the remaining
ARGS to actually emit the message (if applicable)."
  (when (get-buffer (tramp-buffer-name vec))
    (with-current-buffer (tramp-get-debug-buffer vec)
      (goto-char (point-max))
      (unless (bolp)
	(insert "\n"))
      ;; Timestamp
      (insert (format-time-string "%T "))
      ;; Calling function
      (let ((btn 1) btf fn)
	(while (not fn)
	  (setq btf (nth 1 (backtrace-frame btn)))
	  (if (not btf)
	      (setq fn "")
	    (when (symbolp btf)
	      (setq fn (symbol-name btf))
	      (unless (and (string-match "^tramp" fn)
			   (not (string-match
				 "^tramp\\(-debug\\)?\\(-message\\|-error\\)$"
				 fn)))
		(setq fn nil)))
	    (setq btn (1+ btn))))
	;; The following code inserts filename and line number.
	;; Should be deactivated by default, because it is time
	;; consuming.
;	(let ((ffn (find-function-noselect (intern fn))))
;	  (insert
;	   (format
;	    "%s:%d: "
;	    (file-name-nondirectory (buffer-file-name (car ffn)))
;	    (with-current-buffer (car ffn)
;	      (1+ (count-lines (point-min) (cdr ffn)))))))
	(insert (format "%s " fn)))
      ;; The message
      (insert (apply 'format fmt-string args)))))

(defsubst tramp-message (vec-or-proc level fmt-string &rest args)
  "Emit a message depending on verbosity level.
VEC-OR-PROC identifies the Tramp buffer to use.  It can be either a
vector or a process.  LEVEL says to be quiet if `tramp-verbose' is
less than LEVEL.  The message is emitted only if `tramp-verbose' is
greater than or equal to LEVEL.

The message is also logged into the debug buffer when `tramp-verbose'
is greater than or equal 4.

Calls functions `message' and `tramp-debug-message' with FMT-STRING as
control string and the remaining ARGS to actually emit the message (if
applicable)."
  (condition-case nil
      (when (<= level tramp-verbose)
	;; Match data must be preserved!
	(save-match-data
	  ;; Display only when there is a minimum level.
	  (when (<= level 3)
	    (apply 'message
		   (concat
		    (cond
		     ((= level 0) "")
		     ((= level 1) "")
		     ((= level 2) "Warning: ")
		     (t           "Tramp: "))
		    fmt-string)
		   args))
	  ;; Log only when there is a minimum level.
	  (when (>= tramp-verbose 4)
	    (when (and vec-or-proc
		       (processp vec-or-proc)
		       (buffer-name (process-buffer vec-or-proc)))
	      (with-current-buffer (process-buffer vec-or-proc)
		;; Translate proc to vec.
		(setq vec-or-proc (tramp-dissect-file-name default-directory))))
	    (when (and vec-or-proc (vectorp vec-or-proc))
	      (apply 'tramp-debug-message
		     vec-or-proc
		     (concat (format "(%d) # " level) fmt-string)
		     args)))))
    ;; Suppress all errors.
    (error nil)))

(defsubst tramp-error (vec-or-proc signal fmt-string &rest args)
  "Emit an error.
VEC-OR-PROC identifies the connection to use, SIGNAL is the
signal identifier to be raised, remaining args passed to
`tramp-message'.  Finally, signal SIGNAL is raised."
  (tramp-message
   vec-or-proc 1 "%s"
   (error-message-string
    (list signal (get signal 'error-message) (apply 'format fmt-string args))))
  (signal signal (list (apply 'format fmt-string args))))

(defsubst tramp-error-with-buffer
  (buffer vec-or-proc signal fmt-string &rest args)
  "Emit an error, and show BUFFER.
If BUFFER is nil, show the connection buffer.  Wait for 30\", or until
an input event arrives.  The other arguments are passed to `tramp-error'."
  (save-window-excursion
    (unwind-protect
	(apply 'tramp-error vec-or-proc signal fmt-string args)
      (when (and vec-or-proc (not (zerop tramp-verbose)))
	(let ((enable-recursive-minibuffers t))
	  (pop-to-buffer
	   (or (and (bufferp buffer) buffer)
	       (and (processp vec-or-proc) (process-buffer vec-or-proc))
	       (tramp-get-buffer vec-or-proc)))
	  (sit-for 30))))))

(defmacro with-parsed-tramp-file-name (filename var &rest body)
  "Parse a Tramp filename and make components available in the body.

First arg FILENAME is evaluated and dissected into its components.
Second arg VAR is a symbol.  It is used as a variable name to hold
the filename structure.  It is also used as a prefix for the variables
holding the components.  For example, if VAR is the symbol `foo', then
`foo' will be bound to the whole structure, `foo-method' will be bound to
the method component, and so on for `foo-user', `foo-host', `foo-localname'.

Remaining args are Lisp expressions to be evaluated (inside an implicit
`progn').

If VAR is nil, then we bind `v' to the structure and `method', `user',
`host', `localname' to the components."
  `(let* ((,(or var 'v) (tramp-dissect-file-name ,filename))
	  (,(if var (intern (concat (symbol-name var) "-method")) 'method)
	   (tramp-file-name-method ,(or var 'v)))
	  (,(if var (intern (concat (symbol-name var) "-user")) 'user)
	   (tramp-file-name-user ,(or var 'v)))
	  (,(if var (intern (concat (symbol-name var) "-host")) 'host)
	   (tramp-file-name-host ,(or var 'v)))
	  (,(if var (intern (concat (symbol-name var) "-localname")) 'localname)
	   (tramp-file-name-localname ,(or var 'v))))
     ,@body))

(put 'with-parsed-tramp-file-name 'lisp-indent-function 2)
(put 'with-parsed-tramp-file-name 'edebug-form-spec '(form symbolp body))
(font-lock-add-keywords 'emacs-lisp-mode '("\\<with-parsed-tramp-file-name\\>"))

(defmacro with-file-property (vec file property &rest body)
  "Check in Tramp cache for PROPERTY, otherwise execute BODY and set cache.
FILE must be a local file name on a connection identified via VEC."
  `(if (file-name-absolute-p ,file)
      (let ((value (tramp-get-file-property ,vec ,file ,property 'undef)))
	(when (eq value 'undef)
	  ;; We cannot pass @body as parameter to
	  ;; `tramp-set-file-property' because it mangles our
	  ;; debug messages.
	  (setq value (progn ,@body))
	  (tramp-set-file-property ,vec ,file ,property value))
	value)
     ,@body))

(put 'with-file-property 'lisp-indent-function 3)
(put 'with-file-property 'edebug-form-spec t)
(font-lock-add-keywords 'emacs-lisp-mode '("\\<with-file-property\\>"))

(defmacro with-connection-property (key property &rest body)
  "Checks in Tramp for property PROPERTY, otherwise executes BODY and set."
  `(let ((value (tramp-get-connection-property ,key ,property 'undef)))
    (when (eq value 'undef)
      ;; We cannot pass ,@body as parameter to
      ;; `tramp-set-connection-property' because it mangles our debug
      ;; messages.
      (setq value (progn ,@body))
      (tramp-set-connection-property ,key ,property value))
    value))

(put 'with-connection-property 'lisp-indent-function 2)
(put 'with-connection-property 'edebug-form-spec t)
(font-lock-add-keywords 'emacs-lisp-mode '("\\<with-connection-property\\>"))

(defmacro tramp-let-maybe (variable value &rest body)
  "Let-bind VARIABLE to VALUE in BODY, but only if VARIABLE is not obsolete.
BODY is executed whether or not the variable is obsolete.
The intent is to protect against `obsolete variable' warnings."
  `(if (get ',variable 'byte-obsolete-variable)
       (progn ,@body)
     (let ((,variable ,value))
       ,@body)))
(put 'tramp-let-maybe 'lisp-indent-function 2)
(put 'tramp-let-maybe 'edebug-form-spec t)

(defsubst tramp-make-tramp-temp-file (vec)
  "Create a temporary file on the remote host identified by VEC.
Return the local name of the temporary file."
  (let ((prefix
	 (tramp-make-tramp-file-name
	  (tramp-file-name-method vec)
	  (tramp-file-name-user vec)
	  (tramp-file-name-host vec)
	  (expand-file-name
	   tramp-temp-name-prefix (tramp-get-remote-tmpdir vec))))
	result)
    (while (not result)
      ;; `make-temp-file' would be the natural choice for
      ;; implementation.  But it calls `write-region' internally,
      ;; which also needs a temporary file - we would end in an
      ;; infinite loop.
      (setq result (make-temp-name prefix))
      (if (file-exists-p result)
	  (setq result nil)
	;; This creates the file by side effect.
	(set-file-times result)
	(set-file-modes result (tramp-octal-to-decimal "0700"))))

    ;; Return the local part.
    (with-parsed-tramp-file-name result nil localname)))


;;; Config Manipulation Functions:

(defun tramp-set-completion-function (method function-list)
  "Sets the list of completion functions for METHOD.
FUNCTION-LIST is a list of entries of the form (FUNCTION FILE).
The FUNCTION is intended to parse FILE according its syntax.
It might be a predefined FUNCTION, or a user defined FUNCTION.
Predefined FUNCTIONs are `tramp-parse-rhosts', `tramp-parse-shosts',
`tramp-parse-sconfig',`tramp-parse-hosts', `tramp-parse-passwd',
and `tramp-parse-netrc'.

Example:

    (tramp-set-completion-function
     \"ssh\"
     '((tramp-parse-sconfig \"/etc/ssh_config\")
       (tramp-parse-sconfig \"~/.ssh/config\")))"

  (let ((r function-list)
	(v function-list))
    (setq tramp-completion-function-alist
	  (delete (assoc method tramp-completion-function-alist)
		  tramp-completion-function-alist))

    (while v
      ;; Remove double entries.
      (when (member (car v) (cdr v))
	(setcdr v (delete (car v) (cdr v))))
      ;; Check for function and file or registry key.
      (unless (and (functionp (nth 0 (car v)))
		   (if (string-match "^HKEY_CURRENT_USER" (nth 1 (car v)))
		       ;; Windows registry.
		       (and (memq system-type '(cygwin windows-nt))
			    (zerop
			     (tramp-local-call-process
			      "reg" nil nil nil "query" (nth 1 (car v)))))
		     ;; Configuration file.
		     (file-exists-p (nth 1 (car v)))))
	(setq r (delete (car v) r)))
      (setq v (cdr v)))

    (when r
      (add-to-list 'tramp-completion-function-alist
		   (cons method r)))))

(defun tramp-get-completion-function (method)
  "Returns a list of completion functions for METHOD.
For definition of that list see `tramp-set-completion-function'."
  (cons
   ;; Hosts visited once shall be remembered.
   `(tramp-parse-connection-properties ,method)
   ;; The method related defaults.
   (cdr (assoc method tramp-completion-function-alist))))


;;; Fontification of `read-file-name':

;; rfn-eshadow.el is part of Emacs 22.  It is autoloaded.
(defvar tramp-rfn-eshadow-overlay)
(make-variable-buffer-local 'tramp-rfn-eshadow-overlay)

(defun tramp-rfn-eshadow-setup-minibuffer ()
  "Set up a minibuffer for `file-name-shadow-mode'.
Adds another overlay hiding filename parts according to Tramp's
special handling of `substitute-in-file-name'."
  (when (symbol-value 'minibuffer-completing-file-name)
    (setq tramp-rfn-eshadow-overlay
	  (funcall (symbol-function 'make-overlay)
		   (funcall (symbol-function 'minibuffer-prompt-end))
		   (funcall (symbol-function 'minibuffer-prompt-end))))
    ;; Copy rfn-eshadow-overlay properties.
    (let ((props (funcall (symbol-function 'overlay-properties)
			  (symbol-value 'rfn-eshadow-overlay))))
      (while props
	(funcall (symbol-function 'overlay-put)
		 tramp-rfn-eshadow-overlay (pop props) (pop props))))))

(when (boundp 'rfn-eshadow-setup-minibuffer-hook)
  (add-hook 'rfn-eshadow-setup-minibuffer-hook
	    'tramp-rfn-eshadow-setup-minibuffer))

(defun tramp-rfn-eshadow-update-overlay ()
  "Update `rfn-eshadow-overlay' to cover shadowed part of minibuffer input.
This is intended to be used as a minibuffer `post-command-hook' for
`file-name-shadow-mode'; the minibuffer should have already
been set up by `rfn-eshadow-setup-minibuffer'."
  ;; In remote files name, there is a shadowing just for the local part.
  (let ((end (or (funcall (symbol-function 'overlay-end)
			  (symbol-value 'rfn-eshadow-overlay))
		 (funcall (symbol-function 'minibuffer-prompt-end)))))
    (when (file-remote-p (buffer-substring-no-properties end (point-max)))
      (save-excursion
	(save-restriction
	  (narrow-to-region
	   (1+ (or (string-match "/" (buffer-string) end) end)) (point-max))
	  (let ((rfn-eshadow-overlay tramp-rfn-eshadow-overlay)
		(rfn-eshadow-update-overlay-hook nil))
	    (funcall (symbol-function 'rfn-eshadow-update-overlay))))))))

(when (boundp 'rfn-eshadow-update-overlay-hook)
  (add-hook 'rfn-eshadow-update-overlay-hook
	    'tramp-rfn-eshadow-update-overlay))


;;; File Name Handler Functions:

(defun tramp-handle-make-symbolic-link
  (filename linkname &optional ok-if-already-exists)
  "Like `make-symbolic-link' for Tramp files.
If LINKNAME is a non-Tramp file, it is used verbatim as the target of
the symlink.  If LINKNAME is a Tramp file, only the localname component is
used as the target of the symlink.

If LINKNAME is a Tramp file and the localname component is relative, then
it is expanded first, before the localname component is taken.  Note that
this can give surprising results if the user/host for the source and
target of the symlink differ."
  (with-parsed-tramp-file-name linkname l
    (let ((ln (tramp-get-remote-ln l))
	  (cwd (file-name-directory l-localname)))
      (unless ln
	(tramp-error
	 l 'file-error
	 "Making a symbolic link. ln(1) does not exist on the remote host."))

      ;; Do the 'confirm if exists' thing.
      (when (file-exists-p linkname)
	;; What to do?
	(if (or (null ok-if-already-exists) ; not allowed to exist
		(and (numberp ok-if-already-exists)
		     (not (yes-or-no-p
			   (format
			    "File %s already exists; make it a link anyway? "
			    l-localname)))))
	    (tramp-error
	     l 'file-already-exists "File %s already exists" l-localname)
	  (delete-file linkname)))

      ;; If FILENAME is a Tramp name, use just the localname component.
      (when (tramp-tramp-file-p filename)
	(setq filename
	      (tramp-file-name-localname
	       (tramp-dissect-file-name (expand-file-name filename)))))

      ;; Right, they are on the same host, regardless of user, method, etc.
      ;; We now make the link on the remote machine. This will occur as the user
      ;; that FILENAME belongs to.
      (zerop
       (tramp-send-command-and-check
	l (format "cd %s && %s -sf %s %s" cwd ln filename l-localname) t)))))


(defun tramp-handle-load (file &optional noerror nomessage nosuffix must-suffix)
  "Like `load' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name file) nil
    (unless nosuffix
      (cond ((file-exists-p (concat file ".elc"))
	     (setq file (concat file ".elc")))
	    ((file-exists-p (concat file ".el"))
	     (setq file (concat file ".el")))))
    (when must-suffix
      ;; The first condition is always true for absolute file names.
      ;; Included for safety's sake.
      (unless (or (file-name-directory file)
		  (string-match "\\.elc?\\'" file))
	(tramp-error
	 v 'file-error
	 "File `%s' does not include a `.el' or `.elc' suffix" file)))
    (unless noerror
      (when (not (file-exists-p file))
	(tramp-error v 'file-error "Cannot load nonexistent file `%s'" file)))
    (if (not (file-exists-p file))
	nil
      (unless nomessage (tramp-message v 0 "Loading %s..." file))
      (let ((local-copy (file-local-copy file)))
	;; MUST-SUFFIX doesn't exist on XEmacs, so let it default to nil.
	(load local-copy noerror t t)
	(delete-file local-copy))
      (unless nomessage (tramp-message v 0 "Loading %s...done" file))
      t)))

;; Localname manipulation functions that grok Tramp localnames...
(defun tramp-handle-file-name-directory (file)
  "Like `file-name-directory' but aware of Tramp files."
  ;; Everything except the last filename thing is the directory.  We
  ;; cannot apply `with-parsed-tramp-file-name', because this expands
  ;; the remote file name parts.  This is a problem when we are in
  ;; file name completion.
  (let ((v (tramp-dissect-file-name file t)))
    ;; Run the command on the localname portion only.
    (tramp-make-tramp-file-name
     (tramp-file-name-method v)
     (tramp-file-name-user v)
     (tramp-file-name-host v)
     (file-name-directory (or (tramp-file-name-localname v) "")))))

(defun tramp-handle-file-name-nondirectory (file)
  "Like `file-name-nondirectory' but aware of Tramp files."
  (with-parsed-tramp-file-name file nil
    (file-name-nondirectory localname)))

(defun tramp-handle-file-truename (filename &optional counter prev-dirs)
  "Like `file-truename' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-file-property v localname "file-truename"
      (let* ((steps        (tramp-split-string localname "/"))
	     (localnamedir (tramp-let-maybe directory-sep-char ?/ ;for XEmacs
			     (file-name-as-directory localname)))
	     (is-dir (string= localname localnamedir))
	     (thisstep nil)
	     (numchase 0)
	     ;; Don't make the following value larger than necessary.
	     ;; People expect an error message in a timely fashion when
	     ;; something is wrong; otherwise they might think that Emacs
	     ;; is hung.  Of course, correctness has to come first.
	     (numchase-limit 20)
	     (result nil)			;result steps in reverse order
	     symlink-target)
	(tramp-message v 4 "Finding true name for `%s'" filename)
	(while (and steps (< numchase numchase-limit))
	  (setq thisstep (pop steps))
	  (tramp-message
	   v 5 "Check %s"
	   (mapconcat 'identity
		      (append '("") (reverse result) (list thisstep))
		      "/"))
	  (setq symlink-target
		(nth 0 (file-attributes
			(tramp-make-tramp-file-name
			 method user host
			 (mapconcat 'identity
				    (append '("")
					    (reverse result)
					    (list thisstep))
				    "/")))))
	  (cond ((string= "." thisstep)
		 (tramp-message v 5 "Ignoring step `.'"))
		((string= ".." thisstep)
		 (tramp-message v 5 "Processing step `..'")
		 (pop result))
		((stringp symlink-target)
		 ;; It's a symlink, follow it.
		 (tramp-message v 5 "Follow symlink to %s" symlink-target)
		 (setq numchase (1+ numchase))
		 (when (file-name-absolute-p symlink-target)
		   (setq result nil))
		 ;; If the symlink was absolute, we'll get a string like
		 ;; "/user@host:/some/target"; extract the
		 ;; "/some/target" part from it.
		 (when (tramp-tramp-file-p symlink-target)
		   (unless (tramp-equal-remote filename symlink-target)
		     (tramp-error
		      v 'file-error
		      "Symlink target `%s' on wrong host" symlink-target))
		   (setq symlink-target localname))
		 (setq steps
		       (append (tramp-split-string symlink-target "/")
			       steps)))
		(t
		 ;; It's a file.
		 (setq result (cons thisstep result)))))
	(when (>= numchase numchase-limit)
	  (tramp-error
	   v 'file-error
	   "Maximum number (%d) of symlinks exceeded" numchase-limit))
	(setq result (reverse result))
	;; Combine list to form string.
	(setq result
	      (if result
		  (mapconcat 'identity (cons "" result) "/")
		"/"))
	(when (and is-dir (or (string= "" result)
			      (not (string= (substring result -1) "/"))))
	  (setq result (concat result "/")))
	(tramp-message v 4 "True name of `%s' is `%s'" filename result)
	(tramp-make-tramp-file-name method user host result)))))

;; Basic functions.

(defun tramp-handle-file-exists-p (filename)
  "Like `file-exists-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-file-property v localname "file-exists-p"
      (zerop (tramp-send-command-and-check
	      v
	      (format
	       "%s %s"
	       (tramp-get-file-exists-command v)
	       (tramp-shell-quote-argument localname)))))))

;; Inodes don't exist for some file systems.  Therefore we must
;; generate virtual ones.  Used in `find-buffer-visiting'.  The method
;; applied might be not so efficient (Ange-FTP uses hashes). But
;; performance isn't the major issue given that file transfer will
;; take time.
(defvar tramp-inodes nil
  "Keeps virtual inodes numbers.")

;; Devices must distinguish physical file systems.  The device numbers
;; provided by "lstat" aren't unique, because we operate on different hosts.
;; So we use virtual device numbers, generated by Tramp.  Both Ange-FTP and
;; EFS use device number "-1".  In order to be different, we use device number
;; (-1 x), whereby "x" is unique for a given (method user host).
(defvar tramp-devices nil
  "Keeps virtual device numbers.")

;; CCC: This should check for an error condition and signal failure
;;      when something goes wrong.
;; Daniel Pittman <daniel@danann.net>
(defun tramp-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for Tramp files."
  (unless id-format (setq id-format 'integer))
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-file-property v localname (format "file-attributes-%s" id-format)
      (when (file-exists-p filename)
	;; file exists, find out stuff
	(save-excursion
	  (tramp-convert-file-attributes
	   v
	   (if (tramp-get-remote-stat v)
	       (tramp-handle-file-attributes-with-stat v localname id-format)
	     (if (tramp-get-remote-perl v)
		 (tramp-handle-file-attributes-with-perl v localname id-format)
	       (tramp-handle-file-attributes-with-ls
		v localname id-format)))))))))

(defun tramp-handle-file-attributes-with-ls (vec localname &optional id-format)
  "Implement `file-attributes' for Tramp files using the ls(1) command."
  (let (symlinkp dirp
		 res-inode res-filemodes res-numlinks
		 res-uid res-gid res-size res-symlink-target)
    (tramp-message vec 5 "file attributes with ls: %s" localname)
    (tramp-send-command
     vec
     (format "%s %s %s"
	     (tramp-get-ls-command vec)
	     (if (eq id-format 'integer) "-ildn" "-ild")
	     (tramp-shell-quote-argument localname)))
    ;; parse `ls -l' output ...
    (with-current-buffer (tramp-get-buffer vec)
      (goto-char (point-min))
      ;; ... inode
      (setq res-inode
	    (condition-case err
		(read (current-buffer))
	      (invalid-read-syntax
	       (when (and (equal (cadr err)
				 "Integer constant overflow in reader")
			  (string-match
			   "^[0-9]+\\([0-9][0-9][0-9][0-9][0-9]\\)\\'"
			   (car (cddr err))))
		 (let* ((big (read (substring (car (cddr err)) 0
					      (match-beginning 1))))
			(small (read (match-string 1 (car (cddr err)))))
			(twiddle (/ small 65536)))
		   (cons (+ big twiddle)
			 (- small (* twiddle 65536))))))))
      ;; ... file mode flags
      (setq res-filemodes (symbol-name (read (current-buffer))))
      ;; ... number links
      (setq res-numlinks (read (current-buffer)))
      ;; ... uid and gid
      (setq res-uid (read (current-buffer)))
      (setq res-gid (read (current-buffer)))
      (if (eq id-format 'integer)
	  (progn
	    (unless (numberp res-uid) (setq res-uid -1))
	    (unless (numberp res-gid) (setq res-gid -1)))
	(progn
	  (unless (stringp res-uid) (setq res-uid (symbol-name res-uid)))
	  (unless (stringp res-gid) (setq res-gid (symbol-name res-gid)))))
      ;; ... size
      (setq res-size (read (current-buffer)))
      ;; From the file modes, figure out other stuff.
      (setq symlinkp (eq ?l (aref res-filemodes 0)))
      (setq dirp (eq ?d (aref res-filemodes 0)))
      ;; if symlink, find out file name pointed to
      (when symlinkp
	(search-forward "-> ")
	(setq res-symlink-target
	      (buffer-substring (point) (tramp-compat-line-end-position))))
      ;; return data gathered
      (list
       ;; 0. t for directory, string (name linked to) for symbolic
       ;; link, or nil.
       (or dirp res-symlink-target)
       ;; 1. Number of links to file.
       res-numlinks
       ;; 2. File uid.
       res-uid
       ;; 3. File gid.
       res-gid
       ;; 4. Last access time, as a list of two integers. First
       ;; integer has high-order 16 bits of time, second has low 16
       ;; bits.
       ;; 5. Last modification time, likewise.
       ;; 6. Last status change time, likewise.
       '(0 0) '(0 0) '(0 0)		;CCC how to find out?
       ;; 7. Size in bytes (-1, if number is out of range).
       res-size
       ;; 8. File modes, as a string of ten letters or dashes as in ls -l.
       res-filemodes
       ;; 9. t if file's gid would change if file were deleted and
       ;; recreated.  Will be set in `tramp-convert-file-attributes'
       t
       ;; 10. inode number.
       res-inode
       ;; 11. Device number.  Will be replaced by a virtual device number.
       -1
       ))))

(defun tramp-handle-file-attributes-with-perl
  (vec localname &optional id-format)
  "Implement `file-attributes' for Tramp files using a Perl script."
  (tramp-message vec 5 "file attributes with perl: %s" localname)
  (tramp-maybe-send-script
   vec tramp-perl-file-attributes "tramp_perl_file_attributes")
  (tramp-send-command-and-read
   vec
   (format "tramp_perl_file_attributes %s %s"
	   (tramp-shell-quote-argument localname) id-format)))

(defun tramp-handle-file-attributes-with-stat
  (vec localname &optional id-format)
  "Implement `file-attributes' for Tramp files using stat(1) command."
  (tramp-message vec 5 "file attributes with stat: %s" localname)
  (tramp-send-command-and-read
   vec
   (format
    "%s -c '((\"%%N\") %%h %s %s %%X.0 %%Y.0 %%Z.0 %%s.0 \"%%A\" t %%i.0 -1)' %s"
    (tramp-get-remote-stat vec)
    (if (eq id-format 'integer) "%u" "\"%U\"")
    (if (eq id-format 'integer) "%g" "\"%G\"")
    (tramp-shell-quote-argument localname))))

(defun tramp-handle-set-visited-file-modtime (&optional time-list)
  "Like `set-visited-file-modtime' for Tramp files."
  (unless (buffer-file-name)
    (error "Can't set-visited-file-modtime: buffer `%s' not visiting a file"
	   (buffer-name)))
  (if time-list
      (tramp-run-real-handler 'set-visited-file-modtime (list time-list))
    (let ((f (buffer-file-name))
	  coding-system-used)
      (with-parsed-tramp-file-name f nil
	(let* ((attr (file-attributes f))
	       ;; '(-1 65535) means file doesn't exists yet.
	       (modtime (or (nth 5 attr) '(-1 65535))))
	  (when (boundp 'last-coding-system-used)
	    (setq coding-system-used (symbol-value 'last-coding-system-used)))
	  ;; We use '(0 0) as a don't-know value.  See also
	  ;; `tramp-handle-file-attributes-with-ls'.
	  (if (not (equal modtime '(0 0)))
	      (tramp-run-real-handler 'set-visited-file-modtime (list modtime))
	    (progn
	      (tramp-send-command
	       v
	       (format "%s -ild %s"
		       (tramp-get-ls-command v)
		       (tramp-shell-quote-argument localname)))
	      (setq attr (buffer-substring (point)
					   (progn (end-of-line) (point)))))
	    (tramp-set-file-property
	     v localname "visited-file-modtime-ild" attr))
	  (when (boundp 'last-coding-system-used)
	    (set 'last-coding-system-used coding-system-used))
	  nil)))))

;; CCC continue here

;; This function makes the same assumption as
;; `tramp-handle-set-visited-file-modtime'.
(defun tramp-handle-verify-visited-file-modtime (buf)
  "Like `verify-visited-file-modtime' for Tramp files.
At the time `verify-visited-file-modtime' calls this function, we
already know that the buffer is visiting a file and that
`visited-file-modtime' does not return 0.  Do not call this
function directly, unless those two cases are already taken care
of."
  (with-current-buffer buf
    ;; There is no file visiting the buffer, or the buffer has no
    ;; recorded last modification time.
    (if (or (not (buffer-file-name))
	    (eq (visited-file-modtime) 0))
	t
      (let ((f (buffer-file-name)))
	(with-parsed-tramp-file-name f nil
	  (tramp-flush-file-property v localname)
	  (let* ((attr (file-attributes f))
		 (modtime (nth 5 attr))
		 (mt (visited-file-modtime)))

 	    (cond
	     ;; file exists, and has a known modtime.
	     ((and attr (not (equal modtime '(0 0))))
	      (< (abs (tramp-time-diff
		       modtime
		       ;; For compatibility, deal with both the old
		       ;; (HIGH . LOW) and the new (HIGH LOW)
		       ;; return values of `visited-file-modtime'.
		       (if (atom (cdr mt))
			   (list (car mt) (cdr mt))
			 mt)))
		 2))
	     ;; modtime has the don't know value.
	     (attr
	      (tramp-send-command
	       v
	       (format "%s -ild %s"
		       (tramp-get-ls-command v)
		       (tramp-shell-quote-argument localname)))
	      (with-current-buffer (tramp-get-buffer v)
		(setq attr (buffer-substring
			    (point) (progn (end-of-line) (point)))))
	      (equal
	       attr
	       (tramp-get-file-property
		v localname "visited-file-modtime-ild" "")))
	     ;; If file does not exist, say it is not modified
	     ;; if and only if that agrees with the buffer's record.
	     (t (equal mt '(-1 65535))))))))))

(defun tramp-handle-set-file-modes (filename mode)
  "Like `set-file-modes' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (tramp-flush-file-property v localname)
    (unless (zerop (tramp-send-command-and-check
		    v
		    (format "chmod %s %s"
			    (tramp-decimal-to-octal mode)
			    (tramp-shell-quote-argument localname))))
      ;; FIXME: extract the proper text from chmod's stderr.
      (tramp-error
       v 'file-error "Error while changing file's mode %s" filename))))

(defun tramp-handle-set-file-times (filename &optional time)
  "Like `set-file-times' for Tramp files."
  (zerop
   (if (file-remote-p filename)
       (with-parsed-tramp-file-name filename nil
	 (tramp-flush-file-property v localname)
	 (let ((time (if (or (null time) (equal time '(0 0)))
			 (current-time)
		       time))
	       (utc
		;; With GNU Emacs, `format-time-string' has an
		;; optional parameter UNIVERSAL.  This is preferred,
		;; because we could handle the case when the remote
		;; host is located in a different time zone as the
		;; local host.
		(and (functionp 'subr-arity)
		     (subrp (symbol-function 'format-time-string))
		     (= 3 (cdr (funcall (symbol-function 'subr-arity)
					(symbol-function
					 'format-time-string)))))))
	   (tramp-send-command-and-check
	    v (format "%s touch -t %s %s"
		      (if utc "TZ=UTC; export TZ;" "")
		      (if utc
			  (format-time-string "%Y%m%d%H%M.%S" time t)
			(format-time-string "%Y%m%d%H%M.%S" time))
		      (tramp-shell-quote-argument localname)))))

     ;; We handle also the local part, because in older Emacsen,
     ;; without `set-file-times', this function is an alias for this.
     ;; We are local, so we don't need the UTC settings.
     (tramp-local-call-process
      "touch" nil nil nil "-t"
      (format-time-string "%Y%m%d%H%M.%S" time)
      (tramp-shell-quote-argument filename)))))

(defun tramp-set-file-uid-gid (filename &optional uid gid)
  "Set the ownership for FILENAME.
If UID and GID are provided, these values are used; otherwise uid
and gid of the corresponding user is taken.  Both parameters must be integers."
  ;; CCC: Modern Unices allow chown only for root.  So we might need
  ;;      another implementation, see `dired-do-chown'.  OTOH, it is
  ;;      mostly working with su(do)? when it is needed, so it shall
  ;;      succeed in the majority of cases.
  (if (file-remote-p filename)
      (with-parsed-tramp-file-name filename nil
	(let ((uid (or (and (integerp uid) uid)
		       (tramp-get-remote-uid v 'integer)))
	      (gid (or (and (integerp gid) gid)
		       (tramp-get-remote-gid v 'integer))))
	  (tramp-send-command
	   v (format
	      "chown %d:%d %s" uid gid
	      (tramp-shell-quote-argument localname)))))

    ;; We handle also the local part, because there doesn't exist
    ;; `set-file-uid-gid'.  On Win32 "chown" might not work.
    (let ((uid (or (and (integerp uid) uid) (tramp-get-local-uid 'integer)))
	  (gid (or (and (integerp gid) gid) (tramp-get-local-gid 'integer))))
      (tramp-local-call-process
       "chown" nil nil nil
       (format "%d:%d" uid gid) (tramp-shell-quote-argument filename)))))

;; Simple functions using the `test' command.

(defun tramp-handle-file-executable-p (filename)
  "Like `file-executable-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-file-property v localname "file-executable-p"
      (zerop (tramp-run-test "-x" filename)))))

(defun tramp-handle-file-readable-p (filename)
  "Like `file-readable-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-file-property v localname "file-readable-p"
      (zerop (tramp-run-test "-r" filename)))))

;; When the remote shell is started, it looks for a shell which groks
;; tilde expansion.  Here, we assume that all shells which grok tilde
;; expansion will also provide a `test' command which groks `-nt' (for
;; newer than).  If this breaks, tell me about it and I'll try to do
;; something smarter about it.
(defun tramp-handle-file-newer-than-file-p (file1 file2)
  "Like `file-newer-than-file-p' for Tramp files."
  (cond ((not (file-exists-p file1))
         nil)
        ((not (file-exists-p file2))
         t)
        ;; We are sure both files exist at this point.
        (t
         (save-excursion
	   ;; We try to get the mtime of both files.  If they are not
	   ;; equal to the "dont-know" value, then we subtract the times
	   ;; and obtain the result.
	   (let ((fa1 (file-attributes file1))
		 (fa2 (file-attributes file2)))
	     (if (and (not (equal (nth 5 fa1) '(0 0)))
		      (not (equal (nth 5 fa2) '(0 0))))
		 (> 0 (tramp-time-diff (nth 5 fa2) (nth 5 fa1)))
	       ;; If one of them is the dont-know value, then we can
	       ;; still try to run a shell command on the remote host.
	       ;; However, this only works if both files are Tramp
	       ;; files and both have the same method, same user, same
	       ;; host.
	       (unless (tramp-equal-remote file1 file2)
		 (with-parsed-tramp-file-name
		     (if (tramp-tramp-file-p file1) file1 file2) nil
		   (tramp-error
		    v 'file-error
		    "Files %s and %s must have same method, user, host"
		    file1 file2)))
	       (with-parsed-tramp-file-name file1 nil
		 (zerop (tramp-run-test2
			 (tramp-get-test-nt-command v) file1 file2)))))))))

;; Functions implemented using the basic functions above.

(defun tramp-handle-file-modes (filename)
  "Like `file-modes' for Tramp files."
  (when (file-exists-p filename)
    (tramp-mode-string-to-int
     (nth 8 (file-attributes filename)))))

(defun tramp-handle-file-directory-p (filename)
  "Like `file-directory-p' for Tramp files."
  ;; Care must be taken that this function returns `t' for symlinks
  ;; pointing to directories.  Surely the most obvious implementation
  ;; would be `test -d', but that returns false for such symlinks.
  ;; CCC: Stefan Monnier says that `test -d' follows symlinks.  And
  ;; I now think he's right.  So we could be using `test -d', couldn't
  ;; we?
  ;;
  ;; Alternatives: `cd %s', `test -d %s'
  (with-parsed-tramp-file-name filename nil
    (with-file-property v localname "file-directory-p"
      (zerop (tramp-run-test "-d" filename)))))

(defun tramp-handle-file-regular-p (filename)
  "Like `file-regular-p' for Tramp files."
  (and (file-exists-p filename)
       (eq ?- (aref (nth 8 (file-attributes filename)) 0))))

(defun tramp-handle-file-symlink-p (filename)
  "Like `file-symlink-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (let ((x (car (file-attributes filename))))
      (when (stringp x)
	;; When Tramp is running on VMS, then `file-name-absolute-p'
	;; might do weird things.
	(if (file-name-absolute-p x)
	    (tramp-make-tramp-file-name method user host x)
	  x)))))

(defun tramp-handle-file-writable-p (filename)
  "Like `file-writable-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-file-property v localname "file-writable-p"
      (if (file-exists-p filename)
	  ;; Existing files must be writable.
	  (zerop (tramp-run-test "-w" filename))
	;; If file doesn't exist, check if directory is writable.
	(and (zerop (tramp-run-test
		     "-d" (file-name-directory filename)))
	     (zerop (tramp-run-test
		     "-w" (file-name-directory filename))))))))

(defun tramp-handle-file-ownership-preserved-p (filename)
  "Like `file-ownership-preserved-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-file-property v localname "file-ownership-preserved-p"
      (let ((attributes (file-attributes filename)))
	;; Return t if the file doesn't exist, since it's true that no
	;; information would be lost by an (attempted) delete and create.
	(or (null attributes)
	    (= (nth 2 attributes) (tramp-get-remote-uid v 'integer)))))))

;; Other file name ops.

(defun tramp-handle-directory-file-name (directory)
  "Like `directory-file-name' for Tramp files."
  ;; If localname component of filename is "/", leave it unchanged.
  ;; Otherwise, remove any trailing slash from localname component.
  ;; Method, host, etc, are unchanged.  Does it make sense to try
  ;; to avoid parsing the filename?
  (with-parsed-tramp-file-name directory nil
    (if (and (not (zerop (length localname)))
	     (eq (aref localname (1- (length localname))) ?/)
	     (not (string= localname "/")))
	(substring directory 0 -1)
      directory)))

;; Directory listings.

(defun tramp-handle-directory-files
  (directory &optional full match nosort files-only)
  "Like `directory-files' for Tramp files."
  ;; FILES-ONLY is valid for XEmacs only.
  (when (file-directory-p directory)
    (setq directory (expand-file-name directory))
    (let ((temp (nreverse (file-name-all-completions "" directory)))
	  result item)

      (while temp
	(setq item (directory-file-name (pop temp)))
	(when (and (or (null match) (string-match match item))
		   (or (null files-only)
		       ;; files only
		       (and (equal files-only t) (file-regular-p item))
		       ;; directories only
		       (file-directory-p item)))
	  (push (if full (expand-file-name item directory) item)
		result)))
      result)))

(defun tramp-handle-directory-files-and-attributes
  (directory &optional full match nosort id-format)
  "Like `directory-files-and-attributes' for Tramp files."
  (unless id-format (setq id-format 'integer))
  (when (file-directory-p directory)
    (setq directory (expand-file-name directory))
    (let* ((temp
	    (tramp-compat-copy-tree
	     (with-parsed-tramp-file-name directory nil
	       (with-file-property
		   v localname
		   (format "directory-files-and-attributes-%s" id-format)
		 (save-excursion
		   (mapcar
		    '(lambda (x)
		       (cons (car x)
			     (tramp-convert-file-attributes v (cdr x))))
		    (if (tramp-get-remote-stat v)
			(tramp-handle-directory-files-and-attributes-with-stat
			 v localname id-format)
		      (if (tramp-get-remote-perl v)
			  (tramp-handle-directory-files-and-attributes-with-perl
			   v localname id-format)))))))))
	   result item)

      (while temp
	(setq item (pop temp))
	(when (or (null match) (string-match match (car item)))
	  (when full
	    (setcar item (expand-file-name (car item) directory)))
	  (push item result)))

      (if nosort
	  result
	(sort result (lambda (x y) (string< (car x) (car y))))))))

(defun tramp-handle-directory-files-and-attributes-with-perl
  (vec localname &optional id-format)
  "Implement `directory-files-and-attributes' for Tramp files using a Perl script."
  (tramp-message vec 5 "directory-files-and-attributes with perl: %s" localname)
  (tramp-maybe-send-script
   vec tramp-perl-directory-files-and-attributes
   "tramp_perl_directory_files_and_attributes")
  (let ((object
	 (tramp-send-command-and-read
	  vec
	  (format "tramp_perl_directory_files_and_attributes %s %s"
		  (tramp-shell-quote-argument localname) id-format))))
    (when (stringp object) (tramp-error vec 'file-error object))
    object))

(defun tramp-handle-directory-files-and-attributes-with-stat
  (vec localname &optional id-format)
  "Implement `directory-files-and-attributes' for Tramp files using stat(1) command."
  (tramp-message vec 5 "directory-files-and-attributes with stat: %s" localname)
  (tramp-send-command-and-read
   vec
   (format
    (concat
     "cd %s; echo \"(\"; (%s -ab | xargs "
     "%s -c '(\"%%n\" (\"%%N\") %%h %s %s %%X.0 %%Y.0 %%Z.0 %%s.0 \"%%A\" t %%i.0 -1)'); "
     "echo \")\"")
    (tramp-shell-quote-argument localname)
    (tramp-get-ls-command vec)
    (tramp-get-remote-stat vec)
    (if (eq id-format 'integer) "%u" "\"%U\"")
    (if (eq id-format 'integer) "%g" "\"%G\""))))

;; This function should return "foo/" for directories and "bar" for
;; files.
(defun tramp-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for Tramp files."
  (unless (save-match-data (string-match "/" filename))
    (with-parsed-tramp-file-name (expand-file-name directory) nil
      ;; Flush the directory cache.  There could be changed directory
      ;; contents.
      (when (and (integerp tramp-completion-reread-directory-timeout)
		 (> (tramp-time-diff
		     (current-time)
		     (tramp-get-file-property
		      v localname "last-completion" '(0 0 0)))
		    tramp-completion-reread-directory-timeout))
	(tramp-flush-file-property v localname))

      (all-completions
       filename
       (mapcar
	'list
	(with-file-property v localname "file-name-all-completions"
	  (let (result)
	   (tramp-barf-unless-okay
	    v
	    (format "cd %s" (tramp-shell-quote-argument localname))
	    "tramp-handle-file-name-all-completions: Couldn't `cd %s'"
	    (tramp-shell-quote-argument localname))

	   ;; Get a list of directories and files, including reliably
	   ;; tagging the directories with a trailing '/'.  Because I
	   ;; rock.  --daniel@danann.net
	   (tramp-send-command
	    v
	    (format (concat "%s -ab 2>/dev/null | while read f; do "
			    "if %s -d \"$f\" 2>/dev/null; "
			    "then echo \"$f/\"; else echo \"$f\"; fi; done")
		    (tramp-get-ls-command v)
		    (tramp-get-test-command v)))

	   ;; Now grab the output.
	   (with-current-buffer (tramp-get-buffer v)
	     (goto-char (point-max))
	     (while (zerop (forward-line -1))
	       (push (buffer-substring
		      (point) (tramp-compat-line-end-position))
		     result)))

	   (tramp-set-file-property
	    v localname "last-completion" (current-time))
	   result)))))))

;; The following isn't needed for Emacs 20 but for 19.34?
(defun tramp-handle-file-name-completion
  (filename directory &optional predicate)
  "Like `file-name-completion' for Tramp files."
  (unless (tramp-tramp-file-p directory)
    (error
     "tramp-handle-file-name-completion invoked on non-tramp directory `%s'"
     directory))
  (try-completion
   filename
   (mapcar 'list (file-name-all-completions filename directory))
   (when predicate
     (lambda (x) (funcall predicate (expand-file-name (car x) directory))))))

;; cp, mv and ln

(defun tramp-handle-add-name-to-file
  (filename newname &optional ok-if-already-exists)
  "Like `add-name-to-file' for Tramp files."
  (unless (tramp-equal-remote filename newname)
    (with-parsed-tramp-file-name
	(if (tramp-tramp-file-p filename) filename newname) nil
      (tramp-error
       v 'file-error
       "add-name-to-file: %s"
       "only implemented for same method, same user, same host")))
  (with-parsed-tramp-file-name filename v1
    (with-parsed-tramp-file-name newname v2
      (let ((ln (when v1 (tramp-get-remote-ln v1))))
	(when (and (not ok-if-already-exists)
		   (file-exists-p newname)
		   (not (numberp ok-if-already-exists))
		   (y-or-n-p
		    (format
		     "File %s already exists; make it a new name anyway? "
		     newname)))
	  (tramp-error
	   v2 'file-error
	   "add-name-to-file: file %s already exists" newname))
	(tramp-flush-file-property v2 v2-localname)
	(tramp-barf-unless-okay
	 v1
	 (format "%s %s %s" ln (tramp-shell-quote-argument v1-localname)
		 (tramp-shell-quote-argument v2-localname))
	 "error with add-name-to-file, see buffer `%s' for details"
	 (buffer-name))))))

(defun tramp-handle-copy-file
  (filename newname &optional ok-if-already-exists keep-date preserve-uid-gid)
  "Like `copy-file' for Tramp files."
  ;; Check if both files are local -- invoke normal copy-file.
  ;; Otherwise, use Tramp from local system.
  (setq filename (expand-file-name filename))
  (setq newname (expand-file-name newname))
  (cond
   ;; At least one file a Tramp file?
   ((or (tramp-tramp-file-p filename)
	(tramp-tramp-file-p newname))
    (tramp-do-copy-or-rename-file
     'copy filename newname ok-if-already-exists keep-date preserve-uid-gid))
   ;; Compat section.
   (preserve-uid-gid
    (tramp-run-real-handler
     'copy-file
     (list filename newname ok-if-already-exists keep-date preserve-uid-gid)))
   (t
    (tramp-run-real-handler
     'copy-file (list filename newname ok-if-already-exists keep-date)))))

(defun tramp-handle-rename-file
  (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for Tramp files."
  ;; Check if both files are local -- invoke normal rename-file.
  ;; Otherwise, use Tramp from local system.
  (setq filename (expand-file-name filename))
  (setq newname (expand-file-name newname))
  ;; At least one file a Tramp file?
  (if (or (tramp-tramp-file-p filename)
          (tramp-tramp-file-p newname))
      (tramp-do-copy-or-rename-file
       'rename filename newname ok-if-already-exists t t)
    (tramp-run-real-handler
     'rename-file (list filename newname ok-if-already-exists))))

(defun tramp-do-copy-or-rename-file
  (op filename newname &optional ok-if-already-exists keep-date preserve-uid-gid)
  "Copy or rename a remote file.
OP must be `copy' or `rename' and indicates the operation to perform.
FILENAME specifies the file to copy or rename, NEWNAME is the name of
the new file (for copy) or the new name of the file (for rename).
OK-IF-ALREADY-EXISTS means don't barf if NEWNAME exists already.
KEEP-DATE means to make sure that NEWNAME has the same timestamp
as FILENAME.  PRESERVE-UID-GID, when non-nil, instructs to keep
the uid and gid if both files are on the same host.

This function is invoked by `tramp-handle-copy-file' and
`tramp-handle-rename-file'.  It is an error if OP is neither of `copy'
and `rename'.  FILENAME and NEWNAME must be absolute file names."
  (unless (memq op '(copy rename))
    (error "Unknown operation `%s', must be `copy' or `rename'" op))
  (let ((t1 (tramp-tramp-file-p filename))
	(t2 (tramp-tramp-file-p newname)))

    (unless ok-if-already-exists
      (when (and t2 (file-exists-p newname))
	(with-parsed-tramp-file-name newname nil
	  (tramp-error
	   v 'file-already-exists "File %s already exists" newname))))

    (prog1
	(cond
	 ;; Both are Tramp files.
	 ((and t1 t2)
	  (with-parsed-tramp-file-name filename v1
	    (with-parsed-tramp-file-name newname v2
	      (cond
	       ;; Shortcut: if method, host, user are the same for both
	       ;; files, we invoke `cp' or `mv' on the remote host
	       ;; directly.
	       ((tramp-equal-remote filename newname)
		(tramp-do-copy-or-rename-file-directly
		 op filename newname
		 ok-if-already-exists keep-date preserve-uid-gid))

	       ;; If both source and target are Tramp files,
	       ;; both are using the same copy-program, then we
	       ;; can invoke rcp directly.  Note that
	       ;; default-directory should point to a local
	       ;; directory if we want to invoke rcp.
	       ((and (equal v1-method v2-method)
		     (tramp-method-out-of-band-p v1)
		     (> (nth 7 (file-attributes filename))
			tramp-copy-size-limit))
		(tramp-do-copy-or-rename-file-out-of-band
		 op filename newname keep-date))

	       ;; No shortcut was possible.  So we copy the
	       ;; file first.  If the operation was `rename', we go
	       ;; back and delete the original file (if the copy was
	       ;; successful).  The approach is simple-minded: we
	       ;; create a new buffer, insert the contents of the
	       ;; source file into it, then write out the buffer to
	       ;; the target file.  The advantage is that it doesn't
	       ;; matter which filename handlers are used for the
	       ;; source and target file.
	       (t
		(tramp-do-copy-or-rename-file-via-buffer
		 op filename newname keep-date))))))

	 ;; One file is a Tramp file, the other one is local.
	 ((or t1 t2)
	  (with-parsed-tramp-file-name (if t1 filename newname) nil
	    (cond
	     ;; Fast track on local machine.
	     ((tramp-local-host-p v)
	      (tramp-do-copy-or-rename-file-directly
	       op filename newname
	       ok-if-already-exists keep-date preserve-uid-gid))

	     ;; If the Tramp file has an out-of-band method, the corresponding
	     ;; copy-program can be invoked.
	     ((and (tramp-method-out-of-band-p v)
		   (> (nth 7 (file-attributes filename))
		      tramp-copy-size-limit))
	      (tramp-do-copy-or-rename-file-out-of-band
	       op filename newname keep-date))

	     ;; Use the inline method via a Tramp buffer.
	     (t (tramp-do-copy-or-rename-file-via-buffer
		 op filename newname keep-date)))))

	 (t
	  ;; One of them must be a Tramp file.
	  (error "Tramp implementation says this cannot happen")))

      ;; In case of `rename', we must flush the cache of the source file.
      (when (and t1 (eq op 'rename))
	(with-parsed-tramp-file-name filename nil
	  (tramp-flush-file-property v localname)))

      ;; When newname did exist, we have wrong cached values.
      (when t2
	(with-parsed-tramp-file-name newname nil
	  (tramp-flush-file-property v localname))))))

(defun tramp-do-copy-or-rename-file-via-buffer (op filename newname keep-date)
  "Use an Emacs buffer to copy or rename a file.
First arg OP is either `copy' or `rename' and indicates the operation.
FILENAME is the source file, NEWNAME the target file.
KEEP-DATE is non-nil if NEWNAME should have the same timestamp as FILENAME."
  (let ((modtime (nth 5 (file-attributes filename))))
    (unwind-protect
	(with-temp-buffer
	  (let ((coding-system-for-read 'binary))
	    (insert-file-contents-literally filename))
	  ;; We don't want the target file to be compressed, so we
	  ;; let-bind `jka-compr-inhibit' to t.
	  (let ((coding-system-for-write 'binary)
		(jka-compr-inhibit t))
	    (write-region (point-min) (point-max) newname))))
    ;; KEEP-DATE handling.
    (when keep-date (set-file-times newname modtime))
    ;; Set the mode.
    (set-file-modes newname (file-modes filename))
    ;; If the operation was `rename', delete the original file.
    (unless (eq op 'copy)
      (delete-file filename))))

(defun tramp-do-copy-or-rename-file-directly
 (op filename newname ok-if-already-exists keep-date preserve-uid-gid)
  "Invokes `cp' or `mv' on the remote system.
OP must be one of `copy' or `rename', indicating `cp' or `mv',
respectively.  FILENAME specifies the file to copy or rename,
NEWNAME is the name of the new file (for copy) or the new name of
the file (for rename).  Both files must reside on the same host.
KEEP-DATE means to make sure that NEWNAME has the same timestamp
as FILENAME.  PRESERVE-UID-GID, when non-nil, instructs to keep
the uid and gid from FILENAME."
  (let ((t1 (tramp-tramp-file-p filename))
	(t2 (tramp-tramp-file-p newname)))
    (with-parsed-tramp-file-name (if t1 filename newname) nil
      (let* ((cmd (cond ((and (eq op 'copy) preserve-uid-gid) "cp -f -p")
			((eq op 'copy) "cp -f")
			((eq op 'rename) "mv -f")
			(t (tramp-error
			    v 'file-error
			    "Unknown operation `%s', must be `copy' or `rename'"
			    op))))
	     (localname1
	      (if t1 (tramp-handle-file-remote-p filename 'localname) filename))
	     (localname2
	      (if t2 (tramp-handle-file-remote-p newname 'localname) newname))
	     (prefix (file-remote-p (if t1 filename newname))))

	(cond
	 ;; Both files are on a remote host, with same user.
	 ((and t1 t2)
	  (tramp-send-command
	   v
	   (format "%s %s %s" cmd
		   (tramp-shell-quote-argument localname1)
		   (tramp-shell-quote-argument localname2)))
	  (with-current-buffer (tramp-get-buffer v)
	    (goto-char (point-min))
	    (unless
		(or
		 (and keep-date
		      ;; Mask cp -f error.
		      (re-search-forward
		       tramp-operation-not-permitted-regexp nil t))
		 (zerop (tramp-send-command-and-check v nil)))
	      (tramp-error-with-buffer
	       nil v 'file-error
	       "Copying directly failed, see buffer `%s' for details."
	       (buffer-name)))))

	 ;; We are on the local host.
	 ((or t1 t2)
	  (cond
	   ;; We can do it directly.
	   ((and (file-readable-p localname1)
		 (file-writable-p (file-name-directory localname2))
		 (or (file-directory-p localname2)
		     (file-writable-p localname2)))
	    (if (eq op 'copy)
		(tramp-compat-copy-file
		 localname1 localname2 ok-if-already-exists
		 keep-date preserve-uid-gid)
	      (rename-file localname1 localname2 ok-if-already-exists)))

	   ;; We can do it directly with `tramp-send-command'
	   ((and (file-readable-p (concat prefix localname1))
		 (file-writable-p
		  (file-name-directory (concat prefix localname2))))
	    (tramp-do-copy-or-rename-file-directly
	     op (concat prefix localname1) (concat prefix localname2)
	     ok-if-already-exists keep-date t)
	    ;; We must change the ownership to the local user.
	    (tramp-set-file-uid-gid
	     (concat prefix localname2)
	     (tramp-get-local-uid 'integer)
	     (tramp-get-local-gid 'integer)))

	   ;; We need a temporary file in between.
	   (t
	    ;; Create the temporary file.
	    (let ((tmpfile (tramp-compat-make-temp-file localname1)))
	      (cond
	       (t1
		(tramp-send-command
		 v (format
		    "%s %s %s" cmd
		    (tramp-shell-quote-argument localname1)
		    (tramp-shell-quote-argument tmpfile)))
		;; We must change the ownership as remote user.
		(tramp-set-file-uid-gid
		 (concat prefix tmpfile)
		 (tramp-get-local-uid 'integer)
		 (tramp-get-local-gid 'integer)))
	       (t2
		(if (eq op 'copy)
		    (tramp-compat-copy-file
		     localname1 tmpfile ok-if-already-exists
		     keep-date preserve-uid-gid)
		  (rename-file localname1 tmpfile ok-if-already-exists))
		;; We must change the ownership as local user.
		(tramp-set-file-uid-gid
		 tmpfile
		 (tramp-get-remote-uid v 'integer)
		 (tramp-get-remote-gid v 'integer))))

	      ;; Move the temporary file to its destination.
	      (cond
	       (t2
		(tramp-send-command
		 v (format
		    "mv -f %s %s"
		    (tramp-shell-quote-argument tmpfile)
		    (tramp-shell-quote-argument localname2))))
	       (t1
		(rename-file tmpfile localname2 ok-if-already-exists)))))))))

      ;; Set the time and mode. Mask possible errors.
      ;; Won't be applied for 'rename.
      (condition-case nil
	  (when (and keep-date (not preserve-uid-gid))
	    (set-file-times newname (nth 5 (file-attributes filename)))
	    (set-file-modes newname (file-modes filename)))
	(error)))))


(defun tramp-do-copy-or-rename-file-out-of-band (op filename newname keep-date)
  "Invoke rcp program to copy.
One of FILENAME and NEWNAME must be a Tramp name, the other must
be a local filename.  The method used must be an out-of-band method."
  (let ((t1 (tramp-tramp-file-p filename))
	(t2 (tramp-tramp-file-p newname))
	copy-program copy-args copy-keep-date port spec
	source target)

    (with-parsed-tramp-file-name (if t1 filename newname) nil

      ;; Expand hops.  Might be necessary for gateway methods.
      (setq v (car (tramp-compute-multi-hops v)))
      (aset v 3 localname)

      ;; Check which ones of source and target are Tramp files.
      (setq source (if t1 (tramp-make-copy-program-file-name v) filename)
	    target (if t2 (tramp-make-copy-program-file-name v) newname))

      ;; Check for port number.  Until now, there's no need for handling
      ;; like method, user, host.
      (setq host (tramp-file-name-real-host v)
	    port (tramp-file-name-port v)
	    port (or (and port (number-to-string port)) ""))

      ;; Compose copy command.
      (setq spec `((?h . ,host) (?u . ,user) (?p . ,port)
		   (?t . ,(tramp-get-connection-property
			   (tramp-get-connection-process v) "temp-file" ""))
		   (?k . ,(if keep-date " " "")))
	    copy-program (tramp-get-method-parameter
			  method 'tramp-copy-program)
	    copy-keep-date (tramp-get-method-parameter
			    method 'tramp-copy-keep-date)
	    copy-args
	    (delq
	     nil
	     (mapcar
	      '(lambda (x)
		 (setq
		  ;; " " is indication for keep-date argument.
		  x (delete " " (mapcar '(lambda (y) (format-spec y spec)) x)))
		 (unless (member "" x) (mapconcat 'identity x " ")))
	      (tramp-get-method-parameter method 'tramp-copy-args))))

      ;; Check for program.
      (when (and (fboundp 'executable-find)
		 (not (let ((default-directory
			      (tramp-compat-temporary-file-directory)))
			(executable-find copy-program))))
	(tramp-error
	 v 'file-error "Cannot find copy program: %s" copy-program))

      (tramp-message v 0 "Transferring %s to %s..." filename newname)

      (unwind-protect
	  (with-temp-buffer
	    ;; The default directory must be remote.
	    (let ((default-directory
		    (file-name-directory (if t1 filename newname))))
	      ;; Set the transfer process properties.
	      (tramp-set-connection-property
	       v "process-name" (buffer-name (current-buffer)))
	      (tramp-set-connection-property
	       v "process-buffer" (current-buffer))

	      ;; Use an asynchronous process.  By this, password can
	      ;; be handled.  The default directory must be local, in
	      ;; order to apply the correct `copy-program'.  We don't
	      ;; set a timeout, because the copying of large files can
	      ;; last longer than 60 secs.
	      (let ((p (let ((default-directory
			       (tramp-compat-temporary-file-directory)))
			 (apply 'start-process
				(tramp-get-connection-property
				 v "process-name" nil)
				(tramp-get-connection-property
				 v "process-buffer" nil)
				copy-program
				(append copy-args (list source target))))))
		(tramp-message
		 v 6 "%s" (mapconcat 'identity (process-command p) " "))
		(set-process-sentinel p 'tramp-process-sentinel)
		(tramp-set-process-query-on-exit-flag p nil)
		(tramp-process-actions p v tramp-actions-copy-out-of-band))))

	;; Reset the transfer process properties.
	(tramp-set-connection-property v "process-name" nil)
	(tramp-set-connection-property v "process-buffer" nil))

      (tramp-message v 0 "Transferring %s to %s...done" filename newname)

      ;; Handle KEEP-DATE argument.
      (when (and keep-date (not copy-keep-date))
	(set-file-times newname (nth 5 (file-attributes filename))))

      ;; Set the mode.
      (unless (and keep-date copy-keep-date)
	(set-file-modes newname (file-modes filename))))

    ;; If the operation was `rename', delete the original file.
    (unless (eq op 'copy)
      (delete-file filename))))

(defun tramp-handle-make-directory (dir &optional parents)
  "Like `make-directory' for Tramp files."
  (setq dir (expand-file-name dir))
  (with-parsed-tramp-file-name dir nil
    (save-excursion
      (tramp-barf-unless-okay
       v
       (format "%s %s"
	       (if parents "mkdir -p" "mkdir")
	       (tramp-shell-quote-argument localname))
       "Couldn't make directory %s" dir))))

(defun tramp-handle-delete-directory (directory)
  "Like `delete-directory' for Tramp files."
  (setq directory (expand-file-name directory))
  (with-parsed-tramp-file-name directory nil
    (tramp-flush-directory-property v localname)
    (unless (zerop (tramp-send-command-and-check
		    v
		    (format "rmdir %s" (tramp-shell-quote-argument localname))))
      (tramp-error v 'file-error "Couldn't delete %s" directory))))

(defun tramp-handle-delete-file (filename)
  "Like `delete-file' for Tramp files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (tramp-flush-file-property v localname)
    (unless (zerop (tramp-send-command-and-check
		    v
		    (format "rm -f %s"
			    (tramp-shell-quote-argument localname))))
      (tramp-error v 'file-error "Couldn't delete %s" filename))))

;; Dired.

;; CCC: This does not seem to be enough. Something dies when
;;      we try and delete two directories under Tramp :/
(defun tramp-handle-dired-recursive-delete-directory (filename)
  "Recursively delete the directory given.
This is like `dired-recursive-delete-directory' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (tramp-flush-directory-property v filename)
    ;; Run a shell command 'rm -r <localname>'
    ;; Code shamelessly stolen for the dired implementation and, um, hacked :)
    (unless (file-exists-p filename)
      (tramp-error v 'file-error "No such directory: %s" filename))
    ;; Which is better, -r or -R? (-r works for me <daniel@danann.net>)
    (tramp-send-command
     v
     (format "rm -rf %s" (tramp-shell-quote-argument localname))
     ;; Don't read the output, do it explicitely.
     nil t)
    ;; Wait for the remote system to return to us...
    ;; This might take a while, allow it plenty of time.
    (tramp-wait-for-output (tramp-get-connection-process v) 120)
    ;; Make sure that it worked...
    (and (file-exists-p filename)
	 (tramp-error
	  v 'file-error "Failed to recursively delete %s" filename))))

(defun tramp-handle-dired-compress-file (file &rest ok-flag)
  "Like `dired-compress-file' for Tramp files."
  ;; OK-FLAG is valid for XEmacs only, but not implemented.
  ;; Code stolen mainly from dired-aux.el.
  (with-parsed-tramp-file-name file nil
    (tramp-flush-file-property v localname)
    (save-excursion
      (let ((suffixes
	     (if (not (featurep 'xemacs))
		 ;; Emacs case
		 (symbol-value 'dired-compress-file-suffixes)
	       ;; XEmacs has `dired-compression-method-alist', which is
	       ;; transformed into `dired-compress-file-suffixes' structure.
	       (mapcar
		'(lambda (x)
		   (list (concat (regexp-quote (nth 1 x)) "\\'")
			 nil
			 (mapconcat 'identity (nth 3 x) " ")))
		(symbol-value 'dired-compression-method-alist))))
	    suffix)
	;; See if any suffix rule matches this file name.
	(while suffixes
	  (let (case-fold-search)
	    (if (string-match (car (car suffixes)) localname)
		(setq suffix (car suffixes) suffixes nil))
	    (setq suffixes (cdr suffixes))))

	(cond ((file-symlink-p file)
	       nil)
	      ((and suffix (nth 2 suffix))
	       ;; We found an uncompression rule.
	       (tramp-message v 0 "Uncompressing %s..." file)
	       (when (zerop (tramp-send-command-and-check
			     v (concat (nth 2 suffix) " " localname)))
		 (tramp-message v 0 "Uncompressing %s...done" file)
		 ;; `dired-remove-file' is not defined in XEmacs
		 (funcall (symbol-function 'dired-remove-file) file)
		 (string-match (car suffix) file)
		 (concat (substring file 0 (match-beginning 0)))))
	      (t
	       ;; We don't recognize the file as compressed, so compress it.
	       ;; Try gzip.
	       (tramp-message v 0 "Compressing %s..." file)
	       (when (zerop (tramp-send-command-and-check
			     v (concat "gzip -f " localname)))
		 (tramp-message v 0 "Compressing %s...done" file)
		 ;; `dired-remove-file' is not defined in XEmacs
		 (funcall (symbol-function 'dired-remove-file) file)
		 (cond ((file-exists-p (concat file ".gz"))
			(concat file ".gz"))
		       ((file-exists-p (concat file ".z"))
			(concat file ".z"))
		       (t nil)))))))))

;; Pacify byte-compiler.  The function is needed on XEmacs only.  I'm
;; not sure at all that this is the right way to do it, but let's hope
;; it works for now, and wait for a guru to point out the Right Way to
;; achieve this.
;;(eval-when-compile
;;  (unless (fboundp 'dired-insert-set-properties)
;;    (fset 'dired-insert-set-properties 'ignore)))
;; Gerd suggests this:
(eval-when-compile (require 'dired))
;; Note that dired is required at run-time, too, when it is needed.
;; It is only needed on XEmacs for the function
;; `dired-insert-set-properties'.

(defun tramp-handle-insert-directory
  (filename switches &optional wildcard full-directory-p)
  "Like `insert-directory' for Tramp files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (tramp-flush-file-property v localname)
    (if (and (featurep 'ls-lisp)
	     (not (symbol-value 'ls-lisp-use-insert-directory-program)))
	(tramp-run-real-handler
	 'insert-directory (list filename switches wildcard full-directory-p))
      ;; For the moment, we assume that the remote "ls" program does not
      ;; grok "--dired".  In the future, we should detect this on
      ;; connection setup.
      (when (string-match "^--dired\\s-+" switches)
	(setq switches (replace-match "" nil t switches)))
      (tramp-message
       v 4 "Inserting directory `ls %s %s', wildcard %s, fulldir %s"
       switches filename (if wildcard "yes" "no")
       (if full-directory-p "yes" "no"))
      (when wildcard
        (setq wildcard (file-name-nondirectory localname))
        (setq localname (file-name-directory localname)))
      (when (listp switches)
        (setq switches (mapconcat 'identity switches " ")))
      (unless full-directory-p
        (setq switches (concat "-d " switches)))
      (when wildcard
        (setq switches (concat switches " " wildcard)))
      ;; If `full-directory-p', we just say `ls -l FILENAME'.
      ;; Else we chdir to the parent directory, then say `ls -ld BASENAME'.
      (if full-directory-p
	  (tramp-send-command
	   v
	   (format "%s %s %s"
		   (tramp-get-ls-command v)
		   switches
		   (if wildcard
		       localname
		     (tramp-shell-quote-argument (concat localname ".")))))
	(tramp-barf-unless-okay
	 v
	 (format "cd %s" (tramp-shell-quote-argument
			  (file-name-directory localname)))
	 "Couldn't `cd %s'"
	 (tramp-shell-quote-argument (file-name-directory localname)))
	(tramp-send-command
	 v
	 (format "%s %s %s"
		 (tramp-get-ls-command v)
		 switches
		 (if (or wildcard
			 (zerop (length (file-name-nondirectory localname))))
		     ""
		   (tramp-shell-quote-argument
		    (file-name-nondirectory localname))))))
      ;; We cannot use `insert-buffer-substring' because the Tramp buffer
      ;; changes its contents before insertion due to calling
      ;; `expand-file' and alike.
      (insert
       (with-current-buffer (tramp-get-buffer v)
	 (buffer-string))))))

;; CCC is this the right thing to do?
(defun tramp-handle-unhandled-file-name-directory (filename)
  "Like `unhandled-file-name-directory' for Tramp files."
  (expand-file-name "~/"))

;; Canonicalization of file names.

(defun tramp-drop-volume-letter (name)
  "Cut off unnecessary drive letter from file NAME.
The function `tramp-handle-expand-file-name' calls `expand-file-name'
locally on a remote file name.  When the local system is a W32 system
but the remote system is Unix, this introduces a superfluous drive
letter into the file name.  This function removes it.

Doesn't do anything if the NAME does not start with a drive letter."
  (if (and (> (length name) 1)
           (char-equal (aref name 1) ?:)
           (let ((c1 (aref name 0)))
             (or (and (>= c1 ?A) (<= c1 ?Z))
                 (and (>= c1 ?a) (<= c1 ?z)))))
      (substring name 2)
    name))

(defun tramp-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for Tramp files.
If the localname part of the given filename starts with \"/../\" then
the result will be a local, non-Tramp, filename."
  ;; If DIR is not given, use DEFAULT-DIRECTORY or "/".
  (setq dir (or dir default-directory "/"))
  ;; Unless NAME is absolute, concat DIR and NAME.
  (unless (file-name-absolute-p name)
    (setq name (concat (file-name-as-directory dir) name)))
  ;; If NAME is not a Tramp file, run the real handler.
  (if (not (tramp-tramp-file-p name))
      (tramp-run-real-handler 'expand-file-name (list name nil))
    ;; Dissect NAME.
    (with-parsed-tramp-file-name name nil
      (unless (file-name-absolute-p localname)
	(setq localname (concat "~/" localname)))
      ;; Tilde expansion if necessary.  This needs a shell which
      ;; groks tilde expansion!  The function `tramp-find-shell' is
      ;; supposed to find such a shell on the remote host.  Please
      ;; tell me about it when this doesn't work on your system.
      (when (string-match "\\`\\(~[^/]*\\)\\(.*\\)\\'" localname)
	(let ((uname (match-string 1 localname))
	      (fname (match-string 2 localname)))
	  ;; We cannot simply apply "~/", because under sudo "~/" is
	  ;; expanded to the local user home directory but to the
	  ;; root home directory.  On the other hand, using always
	  ;; the default user name for tilde expansion is not
	  ;; appropriate either, because ssh and companions might
	  ;; use a user name from the config file.
	  (when (and (string-equal uname "~")
		     (string-match "\\`su\\(do\\)?\\'" method))
	    (setq uname (concat uname user)))
	  (setq uname
	    (with-connection-property v uname
	      (tramp-send-command v (format "cd %s; pwd" uname))
	      (with-current-buffer (tramp-get-buffer v)
		(goto-char (point-min))
		(buffer-substring (point) (tramp-compat-line-end-position)))))
	  (setq localname (concat uname fname))))
      ;; There might be a double slash, for example when "~/"
      ;; expands to "/". Remove this.
      (while (string-match "//" localname)
	(setq localname (replace-match "/" t t localname)))
      ;; No tilde characters in file name, do normal
      ;; expand-file-name (this does "/./" and "/../").  We bind
      ;; `directory-sep-char' here for XEmacs on Windows, which
      ;; would otherwise use backslash.  `default-directory' is
      ;; bound, because on Windows there would be problems with UNC
      ;; shares or Cygwin mounts.
      (tramp-let-maybe directory-sep-char ?/
	(let ((default-directory (tramp-compat-temporary-file-directory)))
	  (tramp-make-tramp-file-name
	   method user host
	   (tramp-drop-volume-letter
	    (tramp-run-real-handler 'expand-file-name
				    (list localname)))))))))

(defun tramp-handle-substitute-in-file-name (filename)
  "Like `substitute-in-file-name' for Tramp files.
\"//\" and \"/~\" substitute only in the local filename part.
If the URL Tramp syntax is chosen, \"//\" as method delimeter and \"/~\" at
beginning of local filename are not substituted."
  (with-parsed-tramp-file-name filename nil
    (if (equal tramp-syntax 'url)
	;; We need to check localname only.  The other parts cannot contain
	;; "//" or "/~".
	(if (and (> (length localname) 1)
		 (or (string-match "//" localname)
		     (string-match "/~" localname 1)))
	    (tramp-run-real-handler 'substitute-in-file-name (list filename))
	  (tramp-make-tramp-file-name
	   (when method (substitute-in-file-name method))
	   (when user (substitute-in-file-name user))
	   (when host (substitute-in-file-name host))
	   (when localname (substitute-in-file-name localname))))
      ;; Ignore in LOCALNAME everything before "//" or "/~".
      (when (and (stringp localname) (string-match ".+?/\\(/\\|~\\)" localname))
	(setq filename
	      (concat (file-remote-p filename)
		      (replace-match "\\1" nil nil localname)))
	;; "/m:h:~" does not work for completion.  We use "/m:h:~/".
	(when (string-match "~$" filename)
	  (setq filename (concat filename "/"))))
      (tramp-run-real-handler 'substitute-in-file-name (list filename)))))

;; In XEmacs, electricity is implemented via a key map for ?/ and ?~,
;; which calls corresponding functions (see minibuf.el).
(when (fboundp 'minibuffer-electric-separator)
  (mapc
   '(lambda (x)
      (eval
       `(defadvice ,x
	  (around ,(intern (format "tramp-advice-%s" x)) activate)
	  "Invoke `substitute-in-file-name' for Tramp files."
	  (if (and (symbol-value 'minibuffer-electric-file-name-behavior)
		   (tramp-tramp-file-p (buffer-substring)))
	      ;; We don't need to handle `last-input-event', because
	      ;; due to the key map we know it must be ?/ or ?~.
	      (let ((s (concat (buffer-substring (point-min) (point))
			       (string last-command-char))))
		(delete-region (point-min) (point))
		(insert (substitute-in-file-name s))
		(setq ad-return-value last-command-char))
	    ad-do-it))))

   '(minibuffer-electric-separator
     minibuffer-electric-tilde)))


;;; Remote commands:

(defun tramp-handle-executable-find (command)
  "Like `executable-find' for Tramp files."
  (with-parsed-tramp-file-name default-directory nil
    (tramp-find-executable v command (tramp-get-remote-path v) t)))

;; We use BUFFER also as connection buffer during setup. Because of
;; this, its original contents must be saved, and restored once
;; connection has been setup.
(defun tramp-handle-start-file-process (name buffer program &rest args)
  "Like `start-file-process' for Tramp files."
  (with-parsed-tramp-file-name default-directory nil
    (unwind-protect
	(progn
	  ;; Set the new process properties.
	  (tramp-set-connection-property v "process-name" name)
	  (tramp-set-connection-property
	   v "process-buffer"
	   ;; BUFFER can be nil.
	   (get-buffer-create (or buffer (current-buffer))))
	  ;; Activate narrowing in order to save BUFFER contents.
	  (with-current-buffer (tramp-get-connection-buffer v)
	    (narrow-to-region (point-max) (point-max)))
	  ;; Goto working directory.  `tramp-send-command' opens a new
	  ;; connection.
	  (tramp-send-command
	   v (format "cd %s" (tramp-shell-quote-argument localname)))
	  ;; Send the command.
	  (tramp-send-command
	   v
	   (format "%s; echo %s; exit"
		   (mapconcat 'tramp-shell-quote-argument
			      (cons program args) " ")
		   (tramp-shell-quote-argument tramp-end-of-output))
	   nil t) ; nooutput
	  ;; Return process.
	  (tramp-get-connection-process v))
      ;; Save exit.
      (with-current-buffer (tramp-get-connection-buffer v)
	(widen)
	(goto-char (point-max)))
      (tramp-set-connection-property v "process-name" nil)
      (tramp-set-connection-property v "process-buffer" nil))))

(defun tramp-handle-process-file
  (program &optional infile destination display &rest args)
  "Like `process-file' for Tramp files."
  ;; The implementation is not complete yet.
  (when (and (numberp destination) (zerop destination))
    (error "Implementation does not handle immediate return"))

  (with-parsed-tramp-file-name default-directory nil
    (let (command input tmpinput stderr tmpstderr outbuf ret)
      ;; Compute command.
      (setq command (mapconcat 'tramp-shell-quote-argument
			       (cons program args) " "))
      ;; Determine input.
      (if (null infile)
	  (setq input "/dev/null")
	(setq infile (expand-file-name infile))
	(if (tramp-equal-remote default-directory infile)
	    ;; INFILE is on the same remote host.
	    (setq input (with-parsed-tramp-file-name infile nil localname))
	  ;; INFILE must be copied to remote host.
	  (setq input (tramp-make-tramp-temp-file v)
		tmpinput (tramp-make-tramp-file-name method user host input))
	  (copy-file infile tmpinput t)))
      (when input (setq command (format "%s <%s" command input)))

      ;; Determine output.
      (cond
       ;; Just a buffer
       ((bufferp destination)
	(setq outbuf destination))
       ;; A buffer name
       ((stringp destination)
	(setq outbuf (get-buffer-create destination)))
       ;; (REAL-DESTINATION ERROR-DESTINATION)
       ((consp destination)
	;; output
	(cond
	 ((bufferp (car destination))
	  (setq outbuf (car destination)))
	 ((stringp (car destination))
	  (setq outbuf (get-buffer-create (car destination))))
	 ((car destination)
	  (setq outbuf (current-buffer))))
	;; stderr
	(cond
	 ((stringp (cadr destination))
	  (setcar (cdr destination) (expand-file-name (cadr destination)))
	  (if (tramp-equal-remote default-directory (cadr destination))
	      ;; stderr is on the same remote host.
	      (setq stderr (with-parsed-tramp-file-name
			       (cadr destination) nil localname))
	    ;; stderr must be copied to remote host.  The temporary
	    ;; file must be deleted after execution.
	    (setq stderr (tramp-make-tramp-temp-file v)
		  tmpstderr (tramp-make-tramp-file-name
			     method user host stderr))))
	 ;; stderr to be discarded
	 ((null (cadr destination))
	  (setq stderr "/dev/null"))))
       ;; 't
       (destination
	(setq outbuf (current-buffer))))
      (when stderr (setq command (format "%s 2>%s" command stderr)))

      ;; Goto working directory.
      (tramp-send-command
       v (format "cd %s" (tramp-shell-quote-argument localname)))
      ;; Send the command.  It might not return in time, so we protect it.
      (condition-case nil
	  (unwind-protect
	      (tramp-send-command v command)
	    ;; We should show the output anyway.
	    (when outbuf
	      (let ((output-string
		     (with-current-buffer (tramp-get-connection-buffer v)
		       (buffer-substring (point-min) (point-max)))))
		(with-current-buffer outbuf
		  (insert output-string)))
	      (when display (display-buffer outbuf))))
	;; When the user did interrupt, we should do it also.
	(error
	 (kill-buffer (tramp-get-connection-buffer v))
	 (setq ret 1)))

      ;; Check return code.
      (unless ret (setq ret (tramp-send-command-and-check v nil)))
      ;; Provide error file.
      (when tmpstderr (rename-file tmpstderr (cadr destination) t))
      ;; Cleanup.
      (when tmpinput (delete-file tmpinput))
      ;; Return exit status.
      ret)))

(defun tramp-local-call-process
  (program &optional infile destination display &rest args)
  "Calls `call-process' on the local host.
This is needed because for some Emacs flavors Tramp has
defadviced `call-process' to behave like `process-file'.  The
Lisp error raised when PROGRAM is nil is trapped also, returning 1."
  (let ((default-directory
	  (if (file-remote-p default-directory)
	      (tramp-compat-temporary-file-directory)
	    default-directory)))
    (if (executable-find program)
	(apply 'call-process program infile destination display args)
      1)))

(defun tramp-handle-call-process-region
  (start end program &optional delete buffer display &rest args)
  "Like `call-process-region' for Tramp files."
  (let ((tmpfile (tramp-compat-make-temp-file "")))
    (write-region start end tmpfile)
    (when delete (delete-region start end))
    (unwind-protect
	(apply 'call-process program tmpfile buffer display args)
      (delete-file tmpfile))))

(defun tramp-handle-shell-command
  (command &optional output-buffer error-buffer)
  "Like `shell-command' for Tramp files."
  (let* ((asynchronous (string-match "[ \t]*&[ \t]*\\'" command))
	 ;; We cannot use `shell-file-name' and `shell-command-switch',
	 ;; they are variables of the local host.
	 (args (list "/bin/sh" "-c" (substring command 0 asynchronous)))
	 (output-buffer
	  (cond
	   ((bufferp output-buffer) output-buffer)
	   ((stringp output-buffer) (get-buffer-create output-buffer))
	   (output-buffer (current-buffer))
	   (t (get-buffer-create
	       (if asynchronous
		   "*Async Shell Command*"
		 "*Shell Command Output*")))))
	 (error-buffer
	  (cond
	   ((bufferp error-buffer) error-buffer)
	   ((stringp error-buffer) (get-buffer-create error-buffer))))
	 (buffer
	  (if (and (not asynchronous) error-buffer)
	      (with-parsed-tramp-file-name default-directory nil
		(list output-buffer (tramp-make-tramp-temp-file v)))
	    output-buffer))
	 (p (get-buffer-process output-buffer)))

    ;; Check whether there is another process running.  Tramp does not
    ;; support 2 (asynchronous) processes in parallel.
    (when p
      (if (yes-or-no-p "A command is running.  Kill it? ")
	  (ignore-errors (kill-process p))
	(error "Shell command in progress")))

    (with-current-buffer output-buffer
      (setq buffer-read-only nil
	    buffer-undo-list t)
      (erase-buffer))

    (if (integerp asynchronous)
	(prog1
	    ;; Run the process.
	    (apply 'start-file-process "*Async Shell*" buffer args)
	  ;; Display output.
	  (pop-to-buffer output-buffer)
	  (setq mode-line-process '(":%s"))
	  (require 'shell) (shell-mode))

      (prog1
	  ;; Run the process.
	  (apply 'process-file (car args) nil buffer nil (cdr args))
	;; Insert error messages if they were separated.
	(when (listp buffer)
	  (with-current-buffer error-buffer
	    (insert-file-contents (cadr buffer)))
	  (delete-file (cadr buffer)))
	;; There's some output, display it.
	(when (with-current-buffer output-buffer (> (point-max) (point-min)))
	  (if (functionp 'display-message-or-buffer)
	      (funcall (symbol-function 'display-message-or-buffer)
		       output-buffer)
	    (pop-to-buffer output-buffer)))))))

;; File Editing.

(defvar tramp-handle-file-local-copy-hook nil
  "Normal hook to be run at the end of `tramp-handle-file-local-copy'.")

(defun tramp-handle-file-local-copy (filename)
  "Like `file-local-copy' for Tramp files."

  (with-parsed-tramp-file-name filename nil
    (let ((rem-enc (tramp-get-remote-coding v "remote-encoding"))
	  (loc-dec (tramp-get-local-coding v "local-decoding"))
	  (tmpfile (tramp-compat-make-temp-file filename)))
      (unless (file-exists-p filename)
	(tramp-error
	 v 'file-error
	 "Cannot make local copy of non-existing file `%s'" filename))

      (cond
       ;; `copy-file' handles direct copy and out-of-band methods.
       ((or (tramp-local-host-p v)
	    (and (tramp-method-out-of-band-p v)
		 (> (nth 7 (file-attributes filename)) tramp-copy-size-limit)))
	(copy-file filename tmpfile t t))

       ;; Use inline encoding for file transfer.
       (rem-enc
	(save-excursion
	  (tramp-message v 5 "Encoding remote file %s..." filename)
	  (tramp-barf-unless-okay
	   v (format "%s < %s" rem-enc (tramp-shell-quote-argument localname))
	   "Encoding remote file failed")
	  (tramp-message v 5 "Encoding remote file %s...done" filename)

	  (tramp-message v 5 "Decoding remote file %s..." filename)
	  (if (and (symbolp loc-dec) (fboundp loc-dec))
	      ;; If local decoding is a function, we call it.  We must
	      ;; disable multibyte, because `uudecode-decode-region'
	      ;; doesn't handle it correctly.
	      (unwind-protect
		  (with-temp-buffer
		    (set-buffer-multibyte nil)
		    (insert-buffer-substring (tramp-get-buffer v))
		    (tramp-message
		     v 5 "Decoding remote file %s with function %s..."
		     filename loc-dec)
		    (funcall loc-dec (point-min) (point-max))
		    (let ((coding-system-for-write 'binary))
		      (write-region (point-min) (point-max) tmpfile))))
	    ;; If tramp-decoding-function is not defined for this
	    ;; method, we invoke tramp-decoding-command instead.
	    (let ((tmpfile2 (tramp-compat-make-temp-file filename)))
	      (let ((coding-system-for-write 'binary))
		(write-region (point-min) (point-max) tmpfile2))
	      (tramp-message
	       v 5 "Decoding remote file %s with command %s..."
	       filename loc-dec)
	      (tramp-call-local-coding-command loc-dec tmpfile2 tmpfile)
	      (delete-file tmpfile2)))
	  (tramp-message v 5 "Decoding remote file %s...done" filename)
	  ;; Set proper permissions.
	  (set-file-modes tmpfile (file-modes filename))
	  ;; Set local user ownership.
	  (tramp-set-file-uid-gid tmpfile)))

       ;; Oops, I don't know what to do.
       (t (tramp-error
	   v 'file-error "Wrong method specification for `%s'" method)))

      (run-hooks 'tramp-handle-file-local-copy-hook)
      tmpfile)))

(defun tramp-handle-file-remote-p (filename &optional identification connected)
  "Like `file-remote-p' for Tramp files."
  (when (tramp-tramp-file-p filename)
    (with-parsed-tramp-file-name filename nil
      (and (or (not connected)
	       (let ((p (tramp-get-connection-process v)))
		 (and p (processp p) (memq (process-status p) '(run open)))))
	   (cond
	    ((eq identification 'method) method)
	    ((eq identification 'user) user)
	    ((eq identification 'host) host)
	    ((eq identification 'localname) localname)
	    (t (tramp-make-tramp-file-name method user host "")))))))

(defun tramp-handle-insert-file-contents
  (filename &optional visit beg end replace)
  "Like `insert-file-contents' for Tramp files."
  (barf-if-buffer-read-only)
  (setq filename (expand-file-name filename))
  (let (coding-system-used result)
    (with-parsed-tramp-file-name filename nil

      (if (not (file-exists-p filename))
	  (progn
	    (when visit
	      (setq buffer-file-name filename)
	      (set-visited-file-modtime)
	      (set-buffer-modified-p nil))
	    ;; We don't raise a Tramp error, because it might be
	    ;; suppressed, like in `find-file-noselect-1'.
	    (signal 'file-error (list "File not found on remote host" filename))
	    (list (expand-file-name filename) 0))

	(if (and (tramp-local-host-p v)
		 (file-readable-p localname))
	    ;; Short track: if we are on the local host, we can run directly.
	    (setq result (insert-file-contents localname visit beg end replace))

	  ;; `insert-file-contents-literally' takes care to avoid calling
	  ;; jka-compr.  By let-binding inhibit-file-name-operation, we
	  ;; propagate that care to the file-local-copy operation.
	  (let ((local-copy
		 (let ((inhibit-file-name-operation
			(when (eq inhibit-file-name-operation
				  'insert-file-contents)
			  'file-local-copy)))
		   (file-local-copy filename))))
	    (tramp-message v 4 "Inserting local temp file `%s'..." local-copy)
	    (setq result (insert-file-contents local-copy nil beg end replace))
	    ;; Now `last-coding-system-used' has right value.  Remember it.
	    (when (boundp 'last-coding-system-used)
	      (setq coding-system-used (symbol-value 'last-coding-system-used)))
	    (tramp-message v 4 "Inserting local temp file `%s'...done" local-copy)
	    (delete-file local-copy)
	    (when (boundp 'last-coding-system-used)
	      (set 'last-coding-system-used coding-system-used))))

	(when visit
	  (setq buffer-read-only (file-writable-p filename))
	  (setq buffer-file-name filename)
	  (set-visited-file-modtime)
	  (set-buffer-modified-p nil))
	(list (expand-file-name filename)
	      (cadr result))))))

;; This is needed for XEmacs only.  Code stolen from files.el.
(defun tramp-handle-insert-file-contents-literally
  (filename &optional visit beg end replace)
  "Like `insert-file-contents-literally' for Tramp files."
  (let ((format-alist nil)
	(after-insert-file-functions nil)
	(coding-system-for-read 'no-conversion)
	(coding-system-for-write 'no-conversion)
	(find-buffer-file-type-function
	 (if (fboundp 'find-buffer-file-type)
	     (symbol-function 'find-buffer-file-type)
	   nil))
	(inhibit-file-name-handlers '(jka-compr-handler image-file-handler))
	(inhibit-file-name-operation 'insert-file-contents))
    (unwind-protect
	(progn
	  (fset 'find-buffer-file-type (lambda (filename) t))
	  (insert-file-contents filename visit beg end replace))
      (if find-buffer-file-type-function
	  (fset 'find-buffer-file-type find-buffer-file-type-function)
	(fmakunbound 'find-buffer-file-type)))))

(defun tramp-handle-find-backup-file-name (filename)
  "Like `find-backup-file-name' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    ;; We set both variables. It doesn't matter whether it is
    ;; Emacs or XEmacs
    (let ((backup-directory-alist
	   ;; Emacs case
	   (when (boundp 'backup-directory-alist)
	     (if (boundp 'tramp-backup-directory-alist)
		 (mapcar
		  '(lambda (x)
		     (cons
		      (car x)
		      (if (and (stringp (cdr x))
			       (file-name-absolute-p (cdr x))
			       (not (tramp-file-name-p (cdr x))))
			  (tramp-make-tramp-file-name method user host (cdr x))
			(cdr x))))
		  (symbol-value 'tramp-backup-directory-alist))
	       (symbol-value 'backup-directory-alist))))

	  (bkup-backup-directory-info
	   ;; XEmacs case
	   (when (boundp 'bkup-backup-directory-info)
	     (if (boundp 'tramp-bkup-backup-directory-info)
		 (mapcar
		  '(lambda (x)
		     (nconc
		      (list (car x))
		      (list
		       (if (and (stringp (car (cdr x)))
				(file-name-absolute-p (car (cdr x)))
				(not (tramp-file-name-p (car (cdr x)))))
			   (tramp-make-tramp-file-name
			    method user host (car (cdr x)))
			 (car (cdr x))))
		      (cdr (cdr x))))
		  (symbol-value 'tramp-bkup-backup-directory-info))
	       (symbol-value 'bkup-backup-directory-info)))))

      (tramp-run-real-handler 'find-backup-file-name (list filename)))))

(defun tramp-handle-make-auto-save-file-name ()
  "Like `make-auto-save-file-name' for Tramp files.
Returns a file name in `tramp-auto-save-directory' for autosaving this file."
  (let ((tramp-auto-save-directory tramp-auto-save-directory)
	(buffer-file-name
	 (tramp-subst-strs-in-string
	  '(("_" . "|")
	    ("/" . "_a")
	    (":" . "_b")
	    ("|" . "__")
	    ("[" . "_l")
	    ("]" . "_r"))
	  (buffer-file-name))))
    ;; File name must be unique.  This is ensured with Emacs 22 (see
    ;; UNIQUIFY element of `auto-save-file-name-transforms'); but for
    ;; all other cases we must do it ourselves.
    (when (boundp 'auto-save-file-name-transforms)
      (mapc
       '(lambda (x)
	  (when (and (string-match (car x) buffer-file-name)
		     (not (car (cddr x))))
	    (setq tramp-auto-save-directory
		  (or tramp-auto-save-directory
		      (tramp-compat-temporary-file-directory)))))
       (symbol-value 'auto-save-file-name-transforms)))
    ;; Create directory.
    (when tramp-auto-save-directory
      (setq buffer-file-name
	    (expand-file-name buffer-file-name tramp-auto-save-directory))
      (unless (file-exists-p tramp-auto-save-directory)
	(make-directory tramp-auto-save-directory t)))
    ;; Run plain `make-auto-save-file-name'.  There might be an advice when
    ;; it is not a magic file name operation (since Emacs 22).
    ;; We must deactivate it temporarily.
    (if (not (ad-is-active 'make-auto-save-file-name))
	(tramp-run-real-handler 'make-auto-save-file-name nil)
      ;; else
      (ad-deactivate 'make-auto-save-file-name)
      (prog1
       (tramp-run-real-handler 'make-auto-save-file-name nil)
       (ad-activate 'make-auto-save-file-name)))))

(defvar tramp-handle-write-region-hook nil
  "Normal hook to be run at the end of `tramp-handle-write-region'.")

;; CCC grok APPEND, LOCKNAME
(defun tramp-handle-write-region
  (start end filename &optional append visit lockname confirm)
  "Like `write-region' for Tramp files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (unless (null append)
      (tramp-error
       v 'file-error "Cannot append to file using Tramp (`%s')" filename))
    ;; Following part commented out because we don't know what to do about
    ;; file locking, and it does not appear to be a problem to ignore it.
    ;; Ange-ftp ignores it, too.
    ;;  (when (and lockname (stringp lockname))
    ;;    (setq lockname (expand-file-name lockname)))
    ;;  (unless (or (eq lockname nil)
    ;;              (string= lockname filename))
    ;;    (error
    ;;     "tramp-handle-write-region: LOCKNAME must be nil or equal FILENAME"))

    ;; XEmacs takes a coding system as the seventh argument, not `confirm'.
    (when (and (not (featurep 'xemacs)) confirm (file-exists-p filename))
      (unless (y-or-n-p (format "File %s exists; overwrite anyway? " filename))
	(tramp-error v 'file-error "File not overwritten")))

    (let ((uid (or (nth 2 (tramp-compat-file-attributes filename 'integer))
		   (tramp-get-remote-uid v 'integer)))
	  (gid (or (nth 3 (tramp-compat-file-attributes filename 'integer))
		   (tramp-get-remote-gid v 'integer))))

      (if (and (tramp-local-host-p v)
	       (file-writable-p (file-name-directory localname))
	       (or (file-directory-p localname)
		   (file-writable-p localname)))
	  ;; Short track: if we are on the local host, we can run directly.
	  (write-region start end localname append 'no-message lockname confirm)

	(let ((rem-dec (tramp-get-remote-coding v "remote-decoding"))
	      (loc-enc (tramp-get-local-coding v "local-encoding"))
	      (modes (save-excursion (file-modes filename)))
	      ;; We use this to save the value of
	      ;; `last-coding-system-used' after writing the tmp file.
	      ;; At the end of the function, we set
	      ;; `last-coding-system-used' to this saved value.  This
	      ;; way, any intermediary coding systems used while
	      ;; talking to the remote shell or suchlike won't hose
	      ;; this variable.  This approach was snarfed from
	      ;; ange-ftp.el.
	      coding-system-used
	      ;; Write region into a tmp file.  This isn't really
	      ;; needed if we use an encoding function, but currently
	      ;; we use it always because this makes the logic
	      ;; simpler.
	      (tmpfile (tramp-compat-make-temp-file filename)))

	  ;; We say `no-message' here because we don't want the
	  ;; visited file modtime data to be clobbered from the temp
	  ;; file.  We call `set-visited-file-modtime' ourselves later
	  ;; on.
	  (tramp-run-real-handler
	   'write-region
	   (list start end tmpfile append 'no-message lockname confirm))
	  ;; Now, `last-coding-system-used' has the right value.  Remember it.
	  (when (boundp 'last-coding-system-used)
	    (setq coding-system-used (symbol-value 'last-coding-system-used)))
	  ;; The permissions of the temporary file should be set.  If
	  ;; filename does not exist (eq modes nil) it has been
	  ;; renamed to the backup file.  This case `save-buffer'
	  ;; handles permissions.
	  (when modes (set-file-modes tmpfile modes))

	  ;; This is a bit lengthy due to the different methods
	  ;; possible for file transfer.  First, we check whether the
	  ;; method uses an rcp program.  If so, we call it.
	  ;; Otherwise, both encoding and decoding command must be
	  ;; specified.  However, if the method _also_ specifies an
	  ;; encoding function, then that is used for encoding the
	  ;; contents of the tmp file.
	  (cond
	   ;; `rename-file' handles direct copy and out-of-band methods.
	   ((or (tramp-local-host-p v)
		(and (tramp-method-out-of-band-p v)
		     (integerp start)
		     (> (- end start) tramp-copy-size-limit)))
	    (rename-file tmpfile filename t))

	   ;; Use inline file transfer
	   (rem-dec
	    ;; Encode tmpfile
	    (tramp-message v 5 "Encoding region...")
	    (unwind-protect
		(with-temp-buffer
		  ;; Use encoding function or command.
		  (if (and (symbolp loc-enc) (fboundp loc-enc))
		      (progn
			(tramp-message
			 v 5 "Encoding region using function `%s'..."
			 (symbol-name loc-enc))
			(let ((coding-system-for-read 'binary))
			  (insert-file-contents-literally tmpfile))
			;; CCC.  The following `let' is a workaround
			;; for the base64.el that comes with
			;; pgnus-0.84.  If both of the following
			;; conditions are satisfied, it tries to write
			;; to a local file in default-directory, but
			;; at this point, default-directory is remote.
			;; (`call-process-region' can't write to remote
			;; files, it seems.)  The file in question is
			;; a tmp file anyway.
			(let ((default-directory
				(tramp-compat-temporary-file-directory)))
			  (funcall loc-enc (point-min) (point-max))))

		    (tramp-message
		     v 5 "Encoding region using command `%s'..." loc-enc)
		    (unless (equal 0 (tramp-call-local-coding-command
				      loc-enc tmpfile t))
		      (tramp-error
		       v 'file-error
		       "Cannot write to `%s', local encoding command `%s' failed"
		       filename loc-enc)))

		  ;; Send buffer into remote decoding command which
		  ;; writes to remote file.  Because this happens on
		  ;; the remote host, we cannot use the function.
		  (goto-char (point-max))
		  (unless (bolp) (newline))
		  (tramp-message
		   v 5 "Decoding region into remote file %s..." filename)
		  (tramp-send-command
		   v
		   (format
		    "%s >%s <<'EOF'\n%sEOF"
		    rem-dec
		    (tramp-shell-quote-argument localname)
		    (buffer-string)))
		  (tramp-barf-unless-okay
		   v nil
		   "Couldn't write region to `%s', decode using `%s' failed"
		   filename rem-dec)
		  ;; When `file-precious-flag' is set, the region is
		  ;; written to a temporary file.  Check that the
		  ;; checksum is equal to that from the local tmpfile.
		  (when file-precious-flag
		    (erase-buffer)
		    (and
		     ;; cksum runs locally, if possible.
		     (zerop (tramp-local-call-process "cksum" tmpfile t))
		     ;; cksum runs remotely.
		     (zerop
		      (tramp-send-command-and-check
		       v
		       (format
			"cksum <%s" (tramp-shell-quote-argument localname))))
		     ;; ... they are different.
		     (not
		      (string-equal
		       (buffer-string)
		       (with-current-buffer (tramp-get-buffer v)
			 (buffer-string))))
		     (tramp-error
		      v 'file-error
		      (concat "Couldn't write region to `%s',"
			      " decode using `%s' failed")
		      filename rem-dec)))
		  (tramp-message
		   v 5 "Decoding region into remote file %s...done" filename)
		  (tramp-flush-file-property v localname))

	      ;; Save exit.
	      (delete-file tmpfile)))

	   ;; That's not expected.
	   (t
	    (tramp-error
	     v 'file-error
	     (concat "Method `%s' should specify both encoding and "
		     "decoding command or an rcp program")
	     method)))

	  ;; Make `last-coding-system-used' have the right value.
	  (when coding-system-used
	    (set 'last-coding-system-used coding-system-used))))

      ;; Set file modification time.
      (when (or (eq visit t) (stringp visit))
	(set-visited-file-modtime
	 ;; We must pass modtime explicitely, because filename can
	 ;; be different from (buffer-file-name), f.e. if
	 ;; `file-precious-flag' is set.
	 (nth 5 (file-attributes filename))))

      ;; Set the ownership.
      (tramp-set-file-uid-gid filename uid gid)
      (when (or (eq visit t) (null visit) (stringp visit))
	(tramp-message v 0 "Wrote %s" filename))
      (run-hooks 'tramp-handle-write-region-hook))))

;;;###autoload
(progn (defun tramp-run-real-handler (operation args)
  "Invoke normal file name handler for OPERATION.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION."
  (let* ((inhibit-file-name-handlers
	  `(tramp-file-name-handler
	    tramp-completion-file-name-handler
	    cygwin-mount-name-hook-function
	    cygwin-mount-map-drive-hook-function
	    .
	    ,(and (eq inhibit-file-name-operation operation)
		  inhibit-file-name-handlers)))
	 (inhibit-file-name-operation operation))
    (apply operation args))))

;;;###autoload
(progn (defun tramp-completion-run-real-handler (operation args)
  "Invoke `tramp-file-name-handler' for OPERATION.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION."
  (let* ((inhibit-file-name-handlers
	  `(tramp-completion-file-name-handler
	    cygwin-mount-name-hook-function
	    cygwin-mount-map-drive-hook-function
	    .
	    ,(and (eq inhibit-file-name-operation operation)
		  inhibit-file-name-handlers)))
	 (inhibit-file-name-operation operation))
    (apply operation args))))

;; We handle here all file primitives.  Most of them have the file
;; name as first parameter; nevertheless we check for them explicitly
;; in order to be signalled if a new primitive appears.  This
;; scenario is needed because there isn't a way to decide by
;; syntactical means whether a foreign method must be called.  It would
;; ease the life if `file-name-handler-alist' would support a decision
;; function as well but regexp only.
(defun tramp-file-name-for-operation (operation &rest args)
  "Return file name related to OPERATION file primitive.
ARGS are the arguments OPERATION has been called with."
  (cond
   ; FILE resp DIRECTORY
   ((member operation
	    (list 'access-file 'byte-compiler-base-file-name 'delete-directory
		  'delete-file 'diff-latest-backup-file 'directory-file-name
		  'directory-files 'directory-files-and-attributes
		  'dired-compress-file 'dired-uncache
		  'file-accessible-directory-p 'file-attributes
		  'file-directory-p 'file-executable-p 'file-exists-p
		  'file-local-copy 'file-remote-p 'file-modes
		  'file-name-as-directory 'file-name-directory
		  'file-name-nondirectory 'file-name-sans-versions
		  'file-ownership-preserved-p 'file-readable-p
		  'file-regular-p 'file-symlink-p 'file-truename
		  'file-writable-p 'find-backup-file-name 'find-file-noselect
		  'get-file-buffer 'insert-directory 'insert-file-contents
		  'load 'make-directory 'make-directory-internal
		  'set-file-modes 'substitute-in-file-name
		  'unhandled-file-name-directory 'vc-registered
		  ; Emacs 22 only
		  'set-file-times
		  ; XEmacs only
		  'abbreviate-file-name 'create-file-buffer
		  'dired-file-modtime 'dired-make-compressed-filename
		  'dired-recursive-delete-directory 'dired-set-file-modtime
		  'dired-shell-unhandle-file-name 'dired-uucode-file
		  'insert-file-contents-literally 'recover-file
		  'vm-imap-check-mail 'vm-pop-check-mail 'vm-spool-check-mail))
    (if (file-name-absolute-p (nth 0 args))
	(nth 0 args)
      (expand-file-name (nth 0 args))))
   ; FILE DIRECTORY resp FILE1 FILE2
   ((member operation
	    (list 'add-name-to-file 'copy-file 'expand-file-name
		  'file-name-all-completions 'file-name-completion
		  'file-newer-than-file-p 'make-symbolic-link 'rename-file
		  ; XEmacs only
		  'dired-make-relative-symlink
		  'vm-imap-move-mail 'vm-pop-move-mail 'vm-spool-move-mail))
    (save-match-data
      (cond
       ((string-match tramp-file-name-regexp (nth 0 args)) (nth 0 args))
       ((string-match tramp-file-name-regexp (nth 1 args)) (nth 1 args))
       (t (buffer-file-name (current-buffer))))))
   ; START END FILE
   ((eq operation 'write-region)
    (nth 2 args))
   ; BUF
   ((member operation
	    (list 'set-visited-file-modtime 'verify-visited-file-modtime
                  ; since Emacs 22 only
		  'make-auto-save-file-name
	          ; XEmacs only
		  'backup-buffer))
    (buffer-file-name
     (if (bufferp (nth 0 args)) (nth 0 args) (current-buffer))))
   ; COMMAND
   ((member operation
	    (list ; not in Emacs 23
	          'dired-call-process
                  ; Emacs only
		  'shell-command
                  ; since Emacs 22 only
                  'process-file
                  ; since Emacs 23 only
                  'start-file-process
	          ; XEmacs only
		  'dired-print-file 'dired-shell-call-process
		  ; nowhere yet
		  'executable-find 'start-process 'call-process))
    default-directory)
   ; unknown file primitive
   (t (error "unknown file I/O primitive: %s" operation))))

(defun tramp-find-foreign-file-name-handler (filename)
  "Return foreign file name handler if exists."
  (when (and (stringp filename) (tramp-tramp-file-p filename))
    (let ((v (tramp-dissect-file-name filename t))
	  (handler tramp-foreign-file-name-handler-alist)
	  elt res)
      ;; When we are not fully sure that filename completion is safe,
      ;; we should not return a handler.
      (when (or (tramp-file-name-method v) (tramp-file-name-user v)
		(and (tramp-file-name-host v)
		     (not (member (tramp-file-name-host v)
				  (mapcar 'car tramp-methods))))
		(not (tramp-completion-mode-p)))
	(while handler
	  (setq elt (car handler)
		handler (cdr handler))
	  (when (funcall (car elt) filename)
	    (setq handler nil
		  res (cdr elt))))
	res))))

;; Main function.
;;;###autoload
(defun tramp-file-name-handler (operation &rest args)
  "Invoke Tramp file name handler.
Falls back to normal file name handler if no Tramp file name handler exists."
  (save-match-data
    (let* ((filename (apply 'tramp-file-name-for-operation operation args))
	   (completion (tramp-completion-mode-p))
	   (foreign (tramp-find-foreign-file-name-handler filename)))
      (with-parsed-tramp-file-name filename nil
	(cond
	 ;; When we are in completion mode, some operations shouldn't be
	 ;; handled by backend.
	 ((and completion (zerop (length localname))
	       (memq operation '(file-exists-p file-directory-p)))
	  t)
	 ((and completion (zerop (length localname))
	       (memq operation '(file-name-as-directory)))
	  filename)
	 ;; Call the backend function.
	 (foreign (apply foreign operation args))
	 ;; Nothing to do for us.
	 (t (tramp-run-real-handler operation args)))))))

;; In Emacs, there is some concurrency due to timers.  If a timer
;; interrupts Tramp and wishes to use the same connection buffer as
;; the "main" Emacs, then garbage might occur in the connection
;; buffer.  Therefore, we need to make sure that a timer does not use
;; the same connection buffer as the "main" Emacs.  We implement a
;; cheap global lock, instead of locking each connection buffer
;; separately.  The global lock is based on two variables,
;; `tramp-locked' and `tramp-locker'.  `tramp-locked' is set to true
;; (with setq) to indicate a lock.  But Tramp also calls itself during
;; processing of a single file operation, so we need to allow
;; recursive calls.  That's where the `tramp-locker' variable comes in
;; -- it is let-bound to t during the execution of the current
;; handler.  So if `tramp-locked' is t and `tramp-locker' is also t,
;; then we should just proceed because we have been called
;; recursively.  But if `tramp-locker' is nil, then we are a timer
;; interrupting the "main" Emacs, and then we signal an error.

(defvar tramp-locked nil
  "If non-nil, then Tramp is currently busy.
Together with `tramp-locker', this implements a locking mechanism
preventing reentrant calls of Tramp.")

(defvar tramp-locker nil
  "If non-nil, then a caller has locked Tramp.
Together with `tramp-locked', this implements a locking mechanism
preventing reentrant calls of Tramp.")

(defun tramp-sh-file-name-handler (operation &rest args)
  "Invoke remote-shell Tramp file name handler.
Fall back to normal file name handler if no Tramp handler exists."
  (when (and tramp-locked (not tramp-locker))
    (signal 'file-error (list "Forbidden reentrant call of Tramp")))
  (let ((tl tramp-locked))
    (unwind-protect
	(progn
	  (setq tramp-locked t)
	  (let ((tramp-locker t))
	    (save-match-data
	      (let ((fn (assoc operation tramp-file-name-handler-alist)))
		(if fn
		    (apply (cdr fn) args)
		  (tramp-run-real-handler operation args))))))
      (setq tramp-locked tl))))

;;;###autoload
(progn (defun tramp-completion-file-name-handler (operation &rest args)
  "Invoke Tramp file name completion handler.
Falls back to normal file name handler if no Tramp file name handler exists."
;;  (setq edebug-trace t)
;;  (edebug-trace "%s" (with-output-to-string (backtrace)))

;;  (mapcar 'trace-function-background
;;	  (mapcar 'intern
;;		  (all-completions "tramp-" obarray 'functionp)))

  (let ((fn (assoc operation tramp-completion-file-name-handler-alist)))
    (if fn
	(save-match-data (apply (cdr fn) args))
      (tramp-completion-run-real-handler operation args)))))

;;;###autoload
(defsubst tramp-register-file-name-handler ()
  "Add Tramp file name handler to `file-name-handler-alist'."
  ;; Remove autoloaded handler from file name handler alist.  Useful,
  ;; if `tramp-syntax' has been changed.
  (let ((a1 (rassq 'tramp-file-name-handler file-name-handler-alist)))
    (setq file-name-handler-alist (delete a1 file-name-handler-alist)))
  ;; Add the handler.
  (add-to-list 'file-name-handler-alist
	       (cons tramp-file-name-regexp 'tramp-file-name-handler))
  ;; If jka-compr is already loaded, move it to the front of
  ;; `file-name-handler-alist'.
  (let ((jka (rassoc 'jka-compr-handler file-name-handler-alist)))
    (when jka
      (setq file-name-handler-alist
	    (cons jka (delete jka file-name-handler-alist))))))

;; `tramp-file-name-handler' must be registered before evaluation of
;; site-start and init files, because there might exist remote files
;; already, f.e. files kept via recentf-mode.
;;;###autoload(tramp-register-file-name-handler)
(tramp-register-file-name-handler)

;;;###autoload
(defsubst tramp-register-completion-file-name-handler ()
  "Add Tramp completion file name handler to `file-name-handler-alist'."
  ;; Remove autoloaded handler from file name handler alist.  Useful,
  ;; if `tramp-syntax' has been changed.
  (let ((a1 (rassq
	     'tramp-completion-file-name-handler file-name-handler-alist)))
    (setq file-name-handler-alist (delete a1 file-name-handler-alist)))
  ;; `partial-completion-mode' is unknown in XEmacs.  So we should
  ;; load it unconditionally there.  In the GNU Emacs case, method/
  ;; user/host name completion shall be bound to `partial-completion-mode'.
  ;; `ido-mode' and `icy-mode' are other packages which extend file
  ;; name completion.
  (when (or (not (boundp 'partial-completion-mode))
	    (symbol-value 'partial-completion-mode)
	    (featurep 'ido)
	    (featurep 'icicles))
    (add-to-list 'file-name-handler-alist
		 (cons tramp-completion-file-name-regexp
		       'tramp-completion-file-name-handler))
    (put 'tramp-completion-file-name-handler 'safe-magic t))
  ;; If jka-compr is already loaded, move it to the front of
  ;; `file-name-handler-alist'.
  (let ((jka (rassoc 'jka-compr-handler file-name-handler-alist)))
    (when jka
      (setq file-name-handler-alist
	    (cons jka (delete jka file-name-handler-alist))))))

;; During autoload, it shall be checked whether
;; `partial-completion-mode' is active.  Therefore registering of
;; `tramp-completion-file-name-handler' will be delayed.
;;;###autoload(add-hook
;;;###autoload 'after-init-hook
;;;###autoload '(lambda () (tramp-register-completion-file-name-handler)))
(tramp-register-completion-file-name-handler)

;;;###autoload
(defun tramp-unload-file-name-handlers ()
  (setq file-name-handler-alist
	(delete (rassoc 'tramp-file-name-handler
			file-name-handler-alist)
		(delete (rassoc 'tramp-completion-file-name-handler
				file-name-handler-alist)
			file-name-handler-alist))))

(add-hook 'tramp-unload-hook 'tramp-unload-file-name-handlers)

;;; File name handler functions for completion mode:

(defvar tramp-completion-mode nil
  "If non-nil, external packages signal that they are in file name completion.

This is necessary, because Tramp uses a heuristic depending on last
input event.  This fails when external packages use other characters
but <TAB>, <SPACE> or ?\\? for file name completion.  This variable
should never be set globally, the intention is to let-bind it.")

;; Necessary because `tramp-file-name-regexp-unified' and
;; `tramp-completion-file-name-regexp-unified' aren't different.  If
;; nil, `tramp-completion-run-real-handler' is called (i.e. forwarding
;; to `tramp-file-name-handler'). Otherwise, it takes
;; `tramp-run-real-handler'.  Using `last-input-event' is a little bit
;; risky, because completing a file might require loading other files,
;; like "~/.netrc", and for them it shouldn't be decided based on that
;; variable. On the other hand, those files shouldn't have partial
;; Tramp file name syntax. Maybe another variable should be introduced
;; overwriting this check in such cases. Or we change Tramp file name
;; syntax in order to avoid ambiguities, like in XEmacs ...
(defun tramp-completion-mode-p ()
  "Checks whether method / user name / host name completion is active."
  (or
   ;; Signal from outside.
   tramp-completion-mode
   ;; Emacs.
   (equal last-input-event 'tab)
   (and (natnump last-input-event)
	(or
	 ;; ?\t has event-modifier 'control.
	 (char-equal last-input-event ?\t)
	 (and (not (event-modifiers last-input-event))
	      (or (char-equal last-input-event ?\?)
		  (char-equal last-input-event ?\ )))))
   ;; XEmacs.
   (and (featurep 'xemacs)
	;; `last-input-event' might be nil.
	(not (null last-input-event))
	;; `last-input-event' may have no character approximation.
	(funcall (symbol-function 'event-to-character) last-input-event)
	(or
	 ;; ?\t has event-modifier 'control.
	 (char-equal
	  (funcall (symbol-function 'event-to-character)
		   last-input-event) ?\t)
	 (and (not (event-modifiers last-input-event))
	      (or (char-equal
		   (funcall (symbol-function 'event-to-character)
			    last-input-event) ?\?)
		  (char-equal
		   (funcall (symbol-function 'event-to-character)
			    last-input-event) ?\ )))))))

;; Method, host name and user name completion.
;; `tramp-completion-dissect-file-name' returns a list of
;; tramp-file-name structures. For all of them we return possible completions.
;;;###autoload
(defun tramp-completion-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for partial Tramp files."

  (let* ((fullname (tramp-drop-volume-letter
		    (expand-file-name filename directory)))
	 ;; Possible completion structures.
	 (v (tramp-completion-dissect-file-name fullname))
	 result result1)

    (while v
      (let* ((car (car v))
	     (method (tramp-file-name-method car))
	     (user (tramp-file-name-user car))
	     (host (tramp-file-name-host car))
	     (localname (tramp-file-name-localname car))
	     (m (tramp-find-method method user host))
	     (tramp-current-user user) ; see `tramp-parse-passwd'
	     all-user-hosts)

	(unless localname        ;; Nothing to complete.

	  (if (or user host)

	      ;; Method dependent user / host combinations.
	      (progn
		(mapc
		 (lambda (x)
		   (setq all-user-hosts
			 (append all-user-hosts
				 (funcall (nth 0 x) (nth 1 x)))))
		 (tramp-get-completion-function m))

		(setq result
		      (append result
			      (mapcar
			       (lambda (x)
				 (tramp-get-completion-user-host
				  method user host (nth 0 x) (nth 1 x)))
			       (delq nil all-user-hosts)))))

	    ;; Possible methods.
	    (setq result
		  (append result (tramp-get-completion-methods m)))))

	(setq v (cdr v))))

    ;; Unify list, remove nil elements.
    (while result
      (let ((car (car result)))
	(when car
	  (add-to-list
	   'result1
	   (substring car (length (tramp-drop-volume-letter directory)))))
	(setq result (cdr result))))

    ;; Complete local parts.
    (append
     result1
     (condition-case nil
	 (tramp-completion-run-real-handler
	  'file-name-all-completions (list filename directory))
       (error nil)))))

;; Method, host name and user name completion for a file.
;;;###autoload
(defun tramp-completion-handle-file-name-completion
  (filename directory &optional predicate)
  "Like `file-name-completion' for Tramp files."
  (try-completion
   filename
   (mapcar 'list (file-name-all-completions filename directory))
   (when predicate
     (lambda (x) (funcall predicate (expand-file-name (car x) directory))))))

;; I misuse a little bit the tramp-file-name structure in order to handle
;; completion possibilities for partial methods / user names / host names.
;; Return value is a list of tramp-file-name structures according to possible
;; completions. If "localname" is non-nil it means there
;; shouldn't be a completion anymore.

;; Expected results:

;; "/x" "/[x"           "/x@" "/[x@"         "/x@y" "/[x@y"
;; [nil nil "x" nil]    [nil "x" nil nil]    [nil "x" "y" nil]
;; [nil "x" nil nil]
;; ["x" nil nil nil]

;; "/x:"                "/x:y"               "/x:y:"
;; [nil nil "x" ""]     [nil nil "x" "y"]    ["x" nil "y" ""]
;; "/[x/"               "/[x/y"
;; ["x" nil "" nil]     ["x" nil "y" nil]
;; ["x" "" nil nil]     ["x" "y" nil nil]

;; "/x:y@"              "/x:y@z"             "/x:y@z:"
;; [nil nil "x" "y@"]   [nil nil "x" "y@z"]  ["x" "y" "z" ""]
;; "/[x/y@"             "/[x/y@z"
;; ["x" nil "y" nil]    ["x" "y" "z" nil]
(defun tramp-completion-dissect-file-name (name)
  "Returns a list of `tramp-file-name' structures.
They are collected by `tramp-completion-dissect-file-name1'."

  (let* ((result)
	 (x-nil "\\|\\(\\)")
	 ;; "/method" "/[method"
	 (tramp-completion-file-name-structure1
	  (list (concat tramp-prefix-regexp "\\(" tramp-method-regexp x-nil "\\)$")
		1 nil nil nil))
	 ;; "/user" "/[user"
	 (tramp-completion-file-name-structure2
	  (list (concat tramp-prefix-regexp "\\(" tramp-user-regexp x-nil   "\\)$")
		nil 1 nil nil))
	 ;; "/host" "/[host"
	 (tramp-completion-file-name-structure3
	  (list (concat tramp-prefix-regexp "\\(" tramp-host-regexp x-nil   "\\)$")
		nil nil 1 nil))
	 ;; "/user@host" "/[user@host"
	 (tramp-completion-file-name-structure4
	  (list (concat tramp-prefix-regexp
			"\\(" tramp-user-regexp "\\)"   tramp-postfix-user-regexp
			"\\(" tramp-host-regexp x-nil   "\\)$")
		nil 1 2 nil))
	 ;; "/method:user" "/[method/user" "/method://user"
	 (tramp-completion-file-name-structure5
	  (list (concat tramp-prefix-regexp
			"\\(" tramp-method-regexp "\\)"	tramp-postfix-method-regexp
			"\\(" tramp-user-regexp x-nil   "\\)$")
		1 2 nil nil))
	 ;; "/method:host" "/[method/host" "/method://host"
	 (tramp-completion-file-name-structure6
	  (list (concat tramp-prefix-regexp
			"\\(" tramp-method-regexp "\\)" tramp-postfix-method-regexp
			"\\(" tramp-host-regexp x-nil   "\\)$")
		1 nil 2 nil))
	 ;; "/method:user@host" "/[method/user@host" "/method://user@host"
	 (tramp-completion-file-name-structure7
	  (list (concat tramp-prefix-regexp
			"\\(" tramp-method-regexp "\\)" tramp-postfix-method-regexp
			"\\(" tramp-user-regexp "\\)"   tramp-postfix-user-regexp
			"\\(" tramp-host-regexp x-nil   "\\)$")
		1 2 3 nil))
	 ;; "/method: "/method:/"
	 (tramp-completion-file-name-structure8
	  (list
	   (if (equal tramp-syntax 'url)
	       (concat tramp-prefix-regexp
		       "\\(" tramp-method-regexp "\\)"
		       "\\(" (substring tramp-postfix-method-regexp 0 1)
		       "\\|" (substring tramp-postfix-method-regexp 1 2) "\\)"
		       "\\(" "\\)$")
	     ;; Should not match if not URL syntax.
	     (concat tramp-prefix-regexp "/$"))
	   1 3 nil nil))
	 ;; "/method: "/method:/"
	 (tramp-completion-file-name-structure9
	  (list
	   (if (equal tramp-syntax 'url)
	       (concat tramp-prefix-regexp
		       "\\(" tramp-method-regexp "\\)"
		       "\\(" (substring tramp-postfix-method-regexp 0 1)
		       "\\|" (substring tramp-postfix-method-regexp 1 2) "\\)"
		       "\\(" "\\)$")
	     ;; Should not match if not URL syntax.
	     (concat tramp-prefix-regexp "/$"))
	   1 nil 3 nil)))

    (mapc (lambda (regexp)
      (add-to-list 'result
	(tramp-completion-dissect-file-name1 regexp name)))
      (list
       tramp-completion-file-name-structure1
       tramp-completion-file-name-structure2
       tramp-completion-file-name-structure3
       tramp-completion-file-name-structure4
       tramp-completion-file-name-structure5
       tramp-completion-file-name-structure6
       tramp-completion-file-name-structure7
       tramp-completion-file-name-structure8
       tramp-completion-file-name-structure9
       tramp-file-name-structure))

    (delq nil result)))

(defun tramp-completion-dissect-file-name1 (structure name)
  "Returns a `tramp-file-name' structure matching STRUCTURE.
The structure consists of remote method, remote user,
remote host and localname (filename on remote host)."

  (save-match-data
    (when (string-match (nth 0 structure) name)
      (let ((method    (and (nth 1 structure)
			    (match-string (nth 1 structure) name)))
	    (user      (and (nth 2 structure)
			    (match-string (nth 2 structure) name)))
	    (host      (and (nth 3 structure)
			    (match-string (nth 3 structure) name)))
	    (localname (and (nth 4 structure)
			    (match-string (nth 4 structure) name))))
	(vector method user host localname)))))

;; This function returns all possible method completions, adding the
;; trailing method delimeter.
(defun tramp-get-completion-methods (partial-method)
  "Returns all method completions for PARTIAL-METHOD."
  (mapcar
   (lambda (method)
     (and method
	  (string-match (concat "^" (regexp-quote partial-method)) method)
	  (tramp-completion-make-tramp-file-name method nil nil nil)))
   (mapcar 'car tramp-methods)))

;; Compares partial user and host names with possible completions.
(defun tramp-get-completion-user-host (method partial-user partial-host user host)
  "Returns the most expanded string for user and host name completion.
PARTIAL-USER must match USER, PARTIAL-HOST must match HOST."
  (cond

   ((and partial-user partial-host)
    (if	(and host
	     (string-match (concat "^" (regexp-quote partial-host)) host)
	     (string-equal partial-user (or user partial-user)))
	(setq user partial-user)
      (setq user nil
	    host nil)))

   (partial-user
    (setq host nil)
    (unless
	(and user (string-match (concat "^" (regexp-quote partial-user)) user))
      (setq user nil)))

   (partial-host
    (setq user nil)
    (unless
	(and host (string-match (concat "^" (regexp-quote partial-host)) host))
      (setq host nil)))

   (t (setq user nil
	    host nil)))

  (unless (zerop (+ (length user) (length host)))
    (tramp-completion-make-tramp-file-name method user host nil)))

(defun tramp-parse-rhosts (filename)
  "Return a list of (user host) tuples allowed to access.
Either user or host may be nil."
  ;; On Windows, there are problems in completion when
  ;; `default-directory' is remote.
  (let ((default-directory (tramp-compat-temporary-file-directory))
	res)
    (when (file-readable-p filename)
      (with-temp-buffer
	(insert-file-contents filename)
	(goto-char (point-min))
	(while (not (eobp))
	  (push (tramp-parse-rhosts-group) res))))
    res))

(defun tramp-parse-rhosts-group ()
   "Return a (user host) tuple allowed to access.
Either user or host may be nil."
   (let ((result)
	 (regexp
	  (concat
	   "^\\(" tramp-host-regexp "\\)"
	   "\\([ \t]+" "\\(" tramp-user-regexp "\\)" "\\)?")))
     (narrow-to-region (point) (tramp-compat-line-end-position))
     (when (re-search-forward regexp nil t)
       (setq result (append (list (match-string 3) (match-string 1)))))
     (widen)
     (forward-line 1)
     result))

(defun tramp-parse-shosts (filename)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  ;; On Windows, there are problems in completion when
  ;; `default-directory' is remote.
  (let ((default-directory (tramp-compat-temporary-file-directory))
	res)
    (when (file-readable-p filename)
      (with-temp-buffer
	(insert-file-contents filename)
	(goto-char (point-min))
	(while (not (eobp))
	  (push (tramp-parse-shosts-group) res))))
    res))

(defun tramp-parse-shosts-group ()
   "Return a (user host) tuple allowed to access.
User is always nil."
   (let ((result)
	 (regexp (concat "^\\(" tramp-host-regexp "\\)")))
     (narrow-to-region (point) (tramp-compat-line-end-position))
     (when (re-search-forward regexp nil t)
       (setq result (list nil (match-string 1))))
     (widen)
     (or
      (> (skip-chars-forward ",") 0)
      (forward-line 1))
     result))

(defun tramp-parse-sconfig (filename)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  ;; On Windows, there are problems in completion when
  ;; `default-directory' is remote.
  (let ((default-directory (tramp-compat-temporary-file-directory))
	res)
    (when (file-readable-p filename)
      (with-temp-buffer
	(insert-file-contents filename)
	(goto-char (point-min))
	(while (not (eobp))
	  (push (tramp-parse-sconfig-group) res))))
    res))

(defun tramp-parse-sconfig-group ()
   "Return a (user host) tuple allowed to access.
User is always nil."
   (let ((result)
	 (regexp (concat "^[ \t]*Host[ \t]+" "\\(" tramp-host-regexp "\\)")))
     (narrow-to-region (point) (tramp-compat-line-end-position))
     (when (re-search-forward regexp nil t)
       (setq result (list nil (match-string 1))))
     (widen)
     (or
      (> (skip-chars-forward ",") 0)
      (forward-line 1))
     result))

(defun tramp-parse-shostkeys (dirname)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  ;; On Windows, there are problems in completion when
  ;; `default-directory' is remote.
  (let* ((default-directory (tramp-compat-temporary-file-directory))
	 (regexp (concat "^key_[0-9]+_\\(" tramp-host-regexp "\\)\\.pub$"))
	 (files (when (file-directory-p dirname) (directory-files dirname)))
	 result)
    (while files
      (when (string-match regexp (car files))
	(push (list nil (match-string 1 (car files))) result))
      (setq files (cdr files)))
    result))

(defun tramp-parse-sknownhosts (dirname)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  ;; On Windows, there are problems in completion when
  ;; `default-directory' is remote.
  (let* ((default-directory (tramp-compat-temporary-file-directory))
	 (regexp (concat "^\\(" tramp-host-regexp
			 "\\)\\.ssh-\\(dss\\|rsa\\)\\.pub$"))
	 (files (when (file-directory-p dirname) (directory-files dirname)))
	 result)
    (while files
      (when (string-match regexp (car files))
	(push (list nil (match-string 1 (car files))) result))
      (setq files (cdr files)))
    result))

(defun tramp-parse-hosts (filename)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  ;; On Windows, there are problems in completion when
  ;; `default-directory' is remote.
  (let ((default-directory (tramp-compat-temporary-file-directory))
	res)
    (when (file-readable-p filename)
      (with-temp-buffer
	(insert-file-contents filename)
	(goto-char (point-min))
	(while (not (eobp))
	  (push (tramp-parse-hosts-group) res))))
    res))

(defun tramp-parse-hosts-group ()
   "Return a (user host) tuple allowed to access.
User is always nil."
   (let ((result)
	 (regexp (concat "^\\(" tramp-host-regexp "\\)")))
     (narrow-to-region (point) (tramp-compat-line-end-position))
     (when (re-search-forward regexp nil t)
       (unless (char-equal (or (char-after) ?\n) ?:) ; no IPv6
	 (setq result (list nil (match-string 1)))))
     (widen)
     (or
      (> (skip-chars-forward " \t") 0)
      (forward-line 1))
     result))

;; For su-alike methods it would be desirable to return "root@localhost"
;; as default.  Unfortunately, we have no information whether any user name
;; has been typed already.  So we use `tramp-current-user' as indication,
;; assuming it is set in `tramp-completion-handle-file-name-all-completions'.
(defun tramp-parse-passwd (filename)
  "Return a list of (user host) tuples allowed to access.
Host is always \"localhost\"."
  ;; On Windows, there are problems in completion when
  ;; `default-directory' is remote.
  (let ((default-directory (tramp-compat-temporary-file-directory))
	res)
    (if (zerop (length tramp-current-user))
	'(("root" nil))
      (when (file-readable-p filename)
	(with-temp-buffer
	  (insert-file-contents filename)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (push (tramp-parse-passwd-group) res))))
      res)))

(defun tramp-parse-passwd-group ()
   "Return a (user host) tuple allowed to access.
Host is always \"localhost\"."
   (let ((result)
	 (regexp (concat "^\\(" tramp-user-regexp "\\):")))
     (narrow-to-region (point) (tramp-compat-line-end-position))
     (when (re-search-forward regexp nil t)
       (setq result (list (match-string 1) "localhost")))
     (widen)
     (forward-line 1)
     result))

(defun tramp-parse-netrc (filename)
  "Return a list of (user host) tuples allowed to access.
User may be nil."
  ;; On Windows, there are problems in completion when
  ;; `default-directory' is remote.
  (let ((default-directory (tramp-compat-temporary-file-directory))
	res)
    (when (file-readable-p filename)
      (with-temp-buffer
	(insert-file-contents filename)
	(goto-char (point-min))
	(while (not (eobp))
	  (push (tramp-parse-netrc-group) res))))
    res))

(defun tramp-parse-netrc-group ()
   "Return a (user host) tuple allowed to access.
User may be nil."
   (let ((result)
	 (regexp
	  (concat
	   "^[ \t]*machine[ \t]+" "\\(" tramp-host-regexp "\\)"
	   "\\([ \t]+login[ \t]+" "\\(" tramp-user-regexp "\\)" "\\)?")))
     (narrow-to-region (point) (tramp-compat-line-end-position))
     (when (re-search-forward regexp nil t)
       (setq result (list (match-string 3) (match-string 1))))
     (widen)
     (forward-line 1)
     result))

(defun tramp-parse-putty (registry)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  ;; On Windows, there are problems in completion when
  ;; `default-directory' is remote.
  (let ((default-directory (tramp-compat-temporary-file-directory))
	res)
    (with-temp-buffer
      (when (zerop (tramp-local-call-process "reg" nil t nil "query" registry))
	(goto-char (point-min))
	(while (not (eobp))
	  (push (tramp-parse-putty-group registry) res))))
    res))

(defun tramp-parse-putty-group (registry)
   "Return a (user host) tuple allowed to access.
User is always nil."
   (let ((result)
	 (regexp (concat (regexp-quote registry) "\\\\\\(.+\\)")))
     (narrow-to-region (point) (tramp-compat-line-end-position))
     (when (re-search-forward regexp nil t)
       (setq result (list nil (match-string 1))))
     (widen)
     (forward-line 1)
     result))

;;; Internal Functions:

(defun tramp-maybe-send-script (vec script name)
  "Define in remote shell function NAME implemented as SCRIPT.
Only send the definition if it has not already been done."
  (let* ((p (tramp-get-connection-process vec))
	 (scripts (tramp-get-connection-property p "scripts" nil)))
    (unless (member name scripts)
      (tramp-message vec 5 "Sending script `%s'..." name)
      ;; The script could contain a call of Perl.  This is masked with `%s'.
      (tramp-send-command-and-check
       vec
       (format "%s () {\n%s\n}" name
	       (format script (tramp-get-remote-perl vec))))
      (tramp-set-connection-property p "scripts" (cons name scripts))
      (tramp-message vec 5 "Sending script `%s'...done." name))))

(defun tramp-set-auto-save ()
  (when (and ;; ange-ftp has its own auto-save mechanism
	     (eq (tramp-find-foreign-file-name-handler (buffer-file-name))
		 'tramp-sh-file-name-handler)
             auto-save-default)
    (auto-save-mode 1)))
(add-hook 'find-file-hooks 'tramp-set-auto-save t)
(add-hook 'tramp-unload-hook
	  '(lambda ()
	     (remove-hook 'find-file-hooks 'tramp-set-auto-save)))

(defun tramp-run-test (switch filename)
  "Run `test' on the remote system, given a SWITCH and a FILENAME.
Returns the exit code of the `test' program."
  (with-parsed-tramp-file-name filename nil
    (tramp-send-command-and-check
     v
     (format
      "%s %s %s"
      (tramp-get-test-command v)
      switch
      (tramp-shell-quote-argument localname)))))

(defun tramp-run-test2 (format-string file1 file2)
  "Run `test'-like program on the remote system, given FILE1, FILE2.
FORMAT-STRING contains the program name, switches, and place holders.
Returns the exit code of the `test' program.  Barfs if the methods,
hosts, or files, disagree."
  (unless (tramp-equal-remote file1 file2)
    (with-parsed-tramp-file-name (if (tramp-tramp-file-p file1) file1 file2) nil
      (tramp-error
       v 'file-error
       "tramp-run-test2 only implemented for same method, user, host")))
  (with-parsed-tramp-file-name file1 v1
    (with-parsed-tramp-file-name file1 v2
      (tramp-send-command-and-check
       v1
       (format format-string
	       (tramp-shell-quote-argument v1-localname)
	       (tramp-shell-quote-argument v2-localname))))))

(defun tramp-buffer-name (vec)
  "A name for the connection buffer VEC."
  ;; We must use `tramp-file-name-real-host', because for gateway
  ;; methods the default port will be expanded later on, which would
  ;; tamper the name.
  (let ((method (tramp-file-name-method vec))
	(user   (tramp-file-name-user vec))
	(host   (tramp-file-name-real-host vec)))
    (if (not (zerop (length user)))
	(format "*tramp/%s %s@%s*" method user host)
      (format "*tramp/%s %s*" method host))))

(defun tramp-get-buffer (vec)
  "Get the connection buffer to be used for VEC."
  (or (get-buffer (tramp-buffer-name vec))
      (with-current-buffer (get-buffer-create (tramp-buffer-name vec))
	(setq buffer-undo-list t)
	(setq default-directory
	      (tramp-make-tramp-file-name
	       (tramp-file-name-method vec)
	       (tramp-file-name-user vec)
	       (tramp-file-name-host vec)
	       "/"))
	(current-buffer))))

(defun tramp-get-connection-buffer (vec)
  "Get the connection buffer to be used for VEC.
In case a second asynchronous communication has been started, it is different
from `tramp-get-buffer'."
  (or (tramp-get-connection-property vec "process-buffer" nil)
      (tramp-get-buffer vec)))

(defun tramp-get-connection-process (vec)
  "Get the connection process to be used for VEC.
In case a second asynchronous communication has been started, it is different
from the default one."
  (get-process
   (or (tramp-get-connection-property vec "process-name" nil)
       (tramp-buffer-name vec))))

(defun tramp-debug-buffer-name (vec)
  "A name for the debug buffer for VEC."
  ;; We must use `tramp-file-name-real-host', because for gateway
  ;; methods the default port will be expanded later on, which would
  ;; tamper the name.
  (let ((method (tramp-file-name-method vec))
	(user   (tramp-file-name-user vec))
	(host   (tramp-file-name-real-host vec)))
    (if (not (zerop (length user)))
	(format "*debug tramp/%s %s@%s*" method user host)
      (format "*debug tramp/%s %s*" method host))))

(defun tramp-get-debug-buffer (vec)
  "Get the debug buffer for VEC."
  (with-current-buffer
      (get-buffer-create (tramp-debug-buffer-name vec))
    (when (bobp)
      (setq buffer-undo-list t)
      ;; Activate outline-mode.  This runs `text-mode-hook' and
      ;; `outline-mode-hook'.  We must prevent that local processes
      ;; die.  Yes: I've seen `flyspell-mode', which starts "ispell"
      ;; ...
      (let ((default-directory (tramp-compat-temporary-file-directory)))
	(outline-mode))
      (set (make-local-variable 'outline-regexp)
	   "[0-9]+:[0-9]+:[0-9]+ [a-z0-9-]+ (\\([0-9]+\\)) #")
;      (set (make-local-variable 'outline-regexp)
;	   "[a-z.-]+:[0-9]+: [a-z0-9-]+ (\\([0-9]+\\)) #")
      (set (make-local-variable 'outline-level) 'tramp-outline-level))
    (current-buffer)))

(defun tramp-outline-level ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line.

The outline level is equal to the verbosity of the Tramp message."
  (1+ (string-to-number (match-string 1))))

(defun tramp-find-executable
  (vec progname dirlist &optional ignore-tilde ignore-path)
  "Searches for PROGNAME in $PATH and all directories mentioned in DIRLIST.
First arg VEC specifies the connection, PROGNAME is the program
to search for, and DIRLIST gives the list of directories to
search.  If IGNORE-TILDE is non-nil, directory names starting
with `~' will be ignored. If IGNORE-PATH is non-nil, searches
only in DIRLIST.

Returns the absolute file name of PROGNAME, if found, and nil otherwise.

This function expects to be in the right *tramp* buffer."
  (with-current-buffer (tramp-get-buffer vec)
    (let (result)
      ;; Check whether the executable is in $PATH. "which(1)" does not
      ;; report always a correct error code; therefore we check the
      ;; number of words it returns.
      (unless ignore-path
	(tramp-send-command vec (format "which \\%s | wc -w" progname))
	(goto-char (point-min))
	(if (looking-at "^1$")
	    (setq result (concat "\\" progname))))
      (unless result
	(when ignore-tilde
	  ;; Remove all ~/foo directories from dirlist.  In Emacs 20,
	  ;; `remove' is in CL, and we want to avoid CL dependencies.
	  (let (newdl d)
	    (while dirlist
	      (setq d (car dirlist))
	      (setq dirlist (cdr dirlist))
	      (unless (char-equal ?~ (aref d 0))
		(setq newdl (cons d newdl))))
	    (setq dirlist (nreverse newdl))))
	(tramp-send-command
	 vec
	 (format (concat "while read d; "
			 "do if test -x $d/%s -a -f $d/%s; "
			 "then echo tramp_executable $d/%s; "
			 "break; fi; done <<'EOF'\n"
			 "%s\nEOF")
		 progname progname progname (mapconcat 'identity dirlist "\n")))
	(goto-char (point-max))
	(when (search-backward "tramp_executable " nil t)
	  (skip-chars-forward "^ ")
	  (skip-chars-forward " ")
	  (setq result (buffer-substring
			(point) (tramp-compat-line-end-position)))))
    result)))

(defun tramp-set-remote-path (vec)
  "Sets the remote environment PATH to existing directories.
I.e., for each directory in `tramp-remote-path', it is tested
whether it exists and if so, it is added to the environment
variable PATH."
  (tramp-message vec 5 (format "Setting $PATH environment variable"))
  (tramp-send-command
   vec (format "PATH=%s; export PATH"
	       (mapconcat 'identity (tramp-get-remote-path vec) ":"))))

;; ------------------------------------------------------------
;; -- Communication with external shell --
;; ------------------------------------------------------------

(defun tramp-find-file-exists-command (vec)
  "Find a command on the remote host for checking if a file exists.
Here, we are looking for a command which has zero exit status if the
file exists and nonzero exit status otherwise."
  (let ((existing "/")
        (nonexisting
	 (tramp-shell-quote-argument "/ this file does not exist "))
	result)
    ;; The algorithm is as follows: we try a list of several commands.
    ;; For each command, we first run `$cmd /' -- this should return
    ;; true, as the root directory always exists.  And then we run
    ;; `$cmd /this\ file\ does\ not\ exist ', hoping that the file indeed
    ;; does not exist.  This should return false.  We use the first
    ;; command we find that seems to work.
    ;; The list of commands to try is as follows:
    ;; `ls -d'            This works on most systems, but NetBSD 1.4
    ;;                    has a bug: `ls' always returns zero exit
    ;;                    status, even for files which don't exist.
    ;; `test -e'          Some Bourne shells have a `test' builtin
    ;;                    which does not know the `-e' option.
    ;; `/bin/test -e'     For those, the `test' binary on disk normally
    ;;                    provides the option.  Alas, the binary
    ;;                    is sometimes `/bin/test' and sometimes it's
    ;;                    `/usr/bin/test'.
    ;; `/usr/bin/test -e' In case `/bin/test' does not exist.
    (unless (or
             (and (setq result (format "%s -e" (tramp-get-test-command vec)))
		  (zerop (tramp-send-command-and-check
			  vec (format "%s %s" result existing)))
                  (not (zerop (tramp-send-command-and-check
			       vec (format "%s %s" result nonexisting)))))
             (and (setq result "/bin/test -e")
		  (zerop (tramp-send-command-and-check
			  vec (format "%s %s" result existing)))
                  (not (zerop (tramp-send-command-and-check
			       vec (format "%s %s" result nonexisting)))))
             (and (setq result "/usr/bin/test -e")
		  (zerop (tramp-send-command-and-check
			  vec (format "%s %s" result existing)))
                  (not (zerop (tramp-send-command-and-check
			       vec (format "%s %s" result nonexisting)))))
             (and (setq result (format "%s -d" (tramp-get-ls-command vec)))
		  (zerop (tramp-send-command-and-check
			  vec (format "%s %s" result existing)))
                  (not (zerop (tramp-send-command-and-check
			       vec (format "%s %s" result nonexisting))))))
      (tramp-error
       vec 'file-error "Couldn't find command to check if file exists"))
    result))

;; CCC test ksh or bash found for tilde expansion?
(defun tramp-find-shell (vec)
  "Opens a shell on the remote host which groks tilde expansion."
  (unless (tramp-get-connection-property vec "remote-shell" nil)
    (let (shell)
      (with-current-buffer (tramp-get-buffer vec)
	(tramp-send-command vec "echo ~root" t)
	(cond
	 ((string-match "^~root$" (buffer-string))
	  (setq shell
		(or (tramp-find-executable
		     vec "bash" (tramp-get-remote-path vec) t)
		    (tramp-find-executable
		     vec "ksh" (tramp-get-remote-path vec) t)))
	  (unless shell
	    (tramp-error
	     vec 'file-error
	     "Couldn't find a shell which groks tilde expansion"))
	  ;; Find arguments for this shell.
	  (let ((alist tramp-sh-extra-args)
		item extra-args)
	    (while (and alist (null extra-args))
	      (setq item (pop alist))
	      (when (string-match (car item) shell)
		(setq extra-args (cdr item))))
	    (when extra-args (setq shell (concat shell " " extra-args))))
	  (tramp-message
	   vec 5 "Starting remote shell `%s' for tilde expansion..." shell)
	  (let ((tramp-end-of-output "$ "))
	    (tramp-send-command
	     vec
	     (format "PROMPT_COMMAND='' PS1='$ ' PS2='' PS3='' exec %s" shell)
	     t))
	  ;; Setting prompts.
	  (tramp-message vec 5 "Setting remote shell prompt...")
	  (tramp-send-command vec (format "PS1='%s'" tramp-end-of-output) t)
	  (tramp-send-command vec "PS2=''" t)
	  (tramp-send-command vec "PS3=''" t)
	  (tramp-send-command vec "PROMPT_COMMAND=''" t)
	  (tramp-message vec 5 "Setting remote shell prompt...done"))

	 (t (tramp-message
	     vec 5 "Remote `%s' groks tilde expansion, good"
	     (tramp-get-method-parameter
	      (tramp-file-name-method vec) 'tramp-remote-sh))
	    (tramp-set-connection-property
	     vec "remote-shell"
	     (tramp-get-method-parameter
	      (tramp-file-name-method vec) 'tramp-remote-sh))))))))

;; ------------------------------------------------------------
;; -- Functions for establishing connection --
;; ------------------------------------------------------------

;; The following functions are actions to be taken when seeing certain
;; prompts from the remote host.  See the variable
;; `tramp-actions-before-shell' for usage of these functions.

(defun tramp-action-login (proc vec)
  "Send the login name."
  (when (not (stringp tramp-current-user))
    (save-window-excursion
      (let ((enable-recursive-minibuffers t))
	(pop-to-buffer (tramp-get-connection-buffer vec))
	(setq tramp-current-user (read-string (match-string 0))))))
  (tramp-message vec 3 "Sending login name `%s'" tramp-current-user)
  (with-current-buffer (tramp-get-connection-buffer vec)
    (tramp-message vec 6 "\n%s" (buffer-string)))
  (tramp-send-string vec tramp-current-user))

(defun tramp-action-password (proc vec)
  "Query the user for a password."
  (tramp-message vec 3 "Sending password")
  (tramp-enter-password proc))

(defun tramp-action-succeed (proc vec)
  "Signal success in finding shell prompt."
  (throw 'tramp-action 'ok))

(defun tramp-action-permission-denied (proc vec)
  "Signal permission denied."
  (kill-process proc)
  (throw 'tramp-action 'permission-denied))

(defun tramp-action-yesno (proc vec)
  "Ask the user for confirmation using `yes-or-no-p'.
Send \"yes\" to remote process on confirmation, abort otherwise.
See also `tramp-action-yn'."
  (save-window-excursion
    (let ((enable-recursive-minibuffers t))
      (save-match-data (pop-to-buffer (tramp-get-connection-buffer vec)))
      (unless (yes-or-no-p (match-string 0))
	(kill-process proc)
	(throw 'tramp-action 'permission-denied))
      (with-current-buffer (tramp-get-connection-buffer vec)
	(tramp-message vec 6 "\n%s" (buffer-string)))
      (tramp-send-string vec "yes"))))

(defun tramp-action-yn (proc vec)
  "Ask the user for confirmation using `y-or-n-p'.
Send \"y\" to remote process on confirmation, abort otherwise.
See also `tramp-action-yesno'."
  (save-window-excursion
    (let ((enable-recursive-minibuffers t))
      (save-match-data (pop-to-buffer (tramp-get-connection-buffer vec)))
      (unless (y-or-n-p (match-string 0))
	(kill-process proc)
	(throw 'tramp-action 'permission-denied))
      (with-current-buffer (tramp-get-connection-buffer vec)
	(tramp-message vec 6 "\n%s" (buffer-string)))
      (tramp-send-string vec "y"))))

(defun tramp-action-terminal (proc vec)
  "Tell the remote host which terminal type to use.
The terminal type can be configured with `tramp-terminal-type'."
  (tramp-message vec 5 "Setting `%s' as terminal type." tramp-terminal-type)
  (with-current-buffer (tramp-get-connection-buffer vec)
    (tramp-message vec 6 "\n%s" (buffer-string)))
  (tramp-send-string vec tramp-terminal-type))

(defun tramp-action-process-alive (proc vec)
  "Check whether a process has finished."
  (unless (memq (process-status proc) '(run open))
    (throw 'tramp-action 'process-died)))

(defun tramp-action-out-of-band (proc vec)
  "Check whether an out-of-band copy has finished."
  (cond ((and (memq (process-status proc) '(stop exit))
	      (zerop (process-exit-status proc)))
	 (tramp-message	vec 3 "Process has finished.")
	 (throw 'tramp-action 'ok))
	((or (and (memq (process-status proc) '(stop exit))
		  (not (zerop (process-exit-status proc))))
	     (memq (process-status proc) '(signal)))
	 ;; `scp' could have copied correctly, but set modes could have failed.
	 ;; This can be ignored.
	 (with-current-buffer (process-buffer proc)
	   (goto-char (point-min))
	   (if (re-search-forward tramp-operation-not-permitted-regexp nil t)
	       (progn
		 (tramp-message vec 5 "'set mode' error ignored.")
		 (tramp-message vec 3 "Process has finished.")
		 (throw 'tramp-action 'ok))
	     (tramp-message vec 3 "Process has died.")
	     (throw 'tramp-action 'process-died))))
	(t nil)))

;; Functions for processing the actions.

(defun tramp-process-one-action (proc vec actions)
  "Wait for output from the shell and perform one action."
  (let (found todo item pattern action)
    (while (not found)
      ;; Reread output once all actions have been performed.
      ;; Obviously, the output was not complete.
      (tramp-accept-process-output proc 1)
      (setq todo actions)
      (while todo
	(setq item (pop todo))
	(setq pattern (concat (symbol-value (nth 0 item)) "\\'"))
	(setq action (nth 1 item))
	(tramp-message
	 vec 5 "Looking for regexp \"%s\" from remote shell" pattern)
	(when (tramp-check-for-regexp proc pattern)
	  (tramp-message vec 5 "Call `%s'" (symbol-name action))
	  (setq found (funcall action proc vec)))))
    found))

(defun tramp-process-actions (proc vec actions &optional timeout)
  "Perform actions until success or TIMEOUT."
  (let (exit)
    (while (not exit)
      (tramp-message proc 3 "Waiting for prompts from remote shell")
      (setq exit
	    (catch 'tramp-action
	      (if timeout
		  (with-timeout (timeout)
		    (tramp-process-one-action proc vec actions))
		(tramp-process-one-action proc vec actions)))))
    (with-current-buffer (tramp-get-connection-buffer vec)
      (tramp-message vec 6 "\n%s" (buffer-string)))
    (unless (eq exit 'ok)
      (tramp-clear-passwd vec)
      (tramp-error-with-buffer
       nil vec 'file-error
       (cond
	((eq exit 'permission-denied) "Permission denied")
	((eq exit 'process-died) "Process died")
	(t "Login failed"))))))

;; Utility functions.

(defun tramp-accept-process-output (&optional proc timeout timeout-msecs)
  "Like `accept-process-output' for Tramp processes.
This is needed in order to hide `last-coding-system-used', which is set
for process communication also."
  (with-current-buffer (process-buffer proc)
    (tramp-message proc 10 "%s %s" proc (process-status proc))
    (let (buffer-read-only last-coding-system-used)
      ;; Under Windows XP, accept-process-output doesn't return
      ;; sometimes.  So we add an additional timeout.
      (with-timeout ((or timeout 1))
	(accept-process-output proc timeout timeout-msecs)))
    (tramp-message proc 10 "\n%s" (buffer-string))))

(defun tramp-check-for-regexp (proc regexp)
  "Check whether REGEXP is contained in process buffer of PROC.
Erase echoed commands if exists."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-min))
    ;; Check whether we need to remove echo output.
    (when (and (tramp-get-connection-property proc "check-remote-echo" nil)
	       (re-search-forward tramp-echoed-echo-mark-regexp nil t))
      (let ((begin (match-beginning 0)))
	(when (re-search-forward tramp-echoed-echo-mark-regexp nil t)
	  ;; Discard echo from remote output.
	  (tramp-set-connection-property proc "check-remote-echo" nil)
	  (tramp-message proc 5 "echo-mark found")
	  (forward-line)
	  (delete-region begin (point))
	  (goto-char (point-min)))))
    ;; No echo to be handled, now we can look for the regexp.
    (when (not (tramp-get-connection-property proc "check-remote-echo" nil))
      (re-search-forward regexp nil t))))

(defun tramp-wait-for-regexp (proc timeout regexp)
  "Wait for a REGEXP to appear from process PROC within TIMEOUT seconds.
Expects the output of PROC to be sent to the current buffer.  Returns
the string that matched, or nil.  Waits indefinitely if TIMEOUT is
nil."
  (with-current-buffer (process-buffer proc)
    (let ((found (tramp-check-for-regexp proc regexp))
	  (start-time (current-time)))
      (cond (timeout
	     ;; Work around a bug in XEmacs 21, where the timeout
	     ;; expires faster than it should.  This degenerates
	     ;; to polling for buggy XEmacsen, but oh, well.
	     (while (and (not found)
			 (< (tramp-time-diff (current-time) start-time)
			    timeout))
	       (with-timeout (timeout)
		 (while (not found)
		   (tramp-accept-process-output proc 1)
		   (unless (memq (process-status proc) '(run open))
		     (tramp-error-with-buffer
		      nil proc 'file-error "Process has died"))
		   (setq found (tramp-check-for-regexp proc regexp))))))
	    (t
	     (while (not found)
	       (tramp-accept-process-output proc 1)
	       (unless (memq (process-status proc) '(run open))
		 (tramp-error-with-buffer
		  nil proc 'file-error "Process has died"))
	       (setq found (tramp-check-for-regexp proc regexp)))))
      (tramp-message proc 6 "\n%s" (buffer-string))
      (when (not found)
	(if timeout
	    (tramp-error
	     proc 'file-error "[[Regexp `%s' not found in %d secs]]"
	     regexp timeout)
	  (tramp-error proc 'file-error "[[Regexp `%s' not found]]" regexp)))
      found)))

(defun tramp-barf-if-no-shell-prompt (proc timeout &rest error-args)
  "Wait for shell prompt and barf if none appears.
Looks at process PROC to see if a shell prompt appears in TIMEOUT
seconds.  If not, it produces an error message with the given ERROR-ARGS."
  (unless
      (tramp-wait-for-regexp
       proc timeout
       (format
	"\\(%s\\|%s\\)\\'" shell-prompt-pattern tramp-shell-prompt-pattern))
    (apply 'tramp-error-with-buffer nil proc 'file-error error-args)))

;; We don't call `tramp-send-string' in order to hide the password
;; from the debug buffer, and because end-of-line handling of the
;; string.
(defun tramp-enter-password (proc)
  "Prompt for a password and send it to the remote end."
  (process-send-string
   proc (concat (tramp-read-passwd proc)
		(or (tramp-get-method-parameter
		     tramp-current-method
		     'tramp-password-end-of-line)
		    tramp-default-password-end-of-line))))

(defun tramp-process-sentinel (proc event)
  "Process sentinel for Tramp processes."
  (when (memq (process-status proc) '(stop exit signal))
    (tramp-flush-connection-property proc)
    ;; The "Connection closed" and "exit" messages disturb the output
    ;; for asynchronous processes. That's why we have echoed the Tramp
    ;; prompt at the end.  Trailing messages can be removed.
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (re-search-backward
       (mapconcat 'identity (split-string tramp-end-of-output "\n") "\r?\n")
       (line-beginning-position -8) t)
      (delete-region (point) (point-max)))))

(defun tramp-open-connection-setup-interactive-shell (proc vec)
  "Set up an interactive shell.
Mainly sets the prompt and the echo correctly.  PROC is the shell
process to set up.  VEC specifies the connection."
  (let ((tramp-end-of-output "$ "))
    ;; It is useful to set the prompt in the following command because
    ;; some people have a setting for $PS1 which /bin/sh doesn't know
    ;; about and thus /bin/sh will display a strange prompt.  For
    ;; example, if $PS1 has "${CWD}" in the value, then ksh will
    ;; display the current working directory but /bin/sh will display
    ;; a dollar sign.  The following command line sets $PS1 to a sane
    ;; value, and works under Bourne-ish shells as well as csh-like
    ;; shells.  Daniel Pittman reports that the unusual positioning of
    ;; the single quotes makes it work under `rc', too.  We also unset
    ;; the variable $ENV because that is read by some sh
    ;; implementations (eg, bash when called as sh) on startup; this
    ;; way, we avoid the startup file clobbering $PS1.  $PROMP_COMMAND
    ;; is another way to set the prompt in /bin/bash, it must be
    ;; discarded as well.
    (tramp-send-command
     vec
     (format
      "exec env ENV='' PROMPT_COMMAND='' PS1='$ ' PS2='' PS3='' %s"
      (tramp-get-method-parameter
       (tramp-file-name-method vec) 'tramp-remote-sh))
     t)

    ;; Disable echo.
    (tramp-message vec 5 "Setting up remote shell environment")
    (tramp-send-command vec "stty -inlcr -echo kill '^U' erase '^H'" t)
    ;; Check whether the echo has really been disabled.  Some
    ;; implementations, like busybox of embedded GNU/Linux, don't
    ;; support disabling.
    (tramp-send-command vec "echo foo" t)
    (with-current-buffer (process-buffer proc)
      (goto-char (point-min))
      (when (looking-at "echo foo")
	(tramp-set-connection-property proc "remote-echo" t)
	(tramp-message vec 5 "Remote echo still on. Ok.")
	;; Make sure backspaces and their echo are enabled and no line
	;; width magic interferes with them.
	(tramp-send-command vec "stty icanon erase ^H cols 32767" t))))

  (tramp-message vec 5 "Setting shell prompt")
  ;; We can set $PS1 to `tramp-end-of-output' only when the echo has
  ;; been disabled.  Otherwise, the echo of the command would be
  ;; regarded as prompt already.
  (tramp-send-command vec (format "PS1='%s'" tramp-end-of-output) t)
  (tramp-send-command vec "PS2=''" t)
  (tramp-send-command vec "PS3=''" t)
  (tramp-send-command vec "PROMPT_COMMAND=''" t)

  ;; Try to set up the coding system correctly.
  ;; CCC this can't be the right way to do it.  Hm.
  (tramp-message vec 5 "Determining coding system")
  (tramp-send-command vec "echo foo ; echo bar" t)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-min))
    (if (featurep 'mule)
	;; Use MULE to select the right EOL convention for communicating
	;; with the process.
	(let* ((cs (or (funcall (symbol-function 'process-coding-system) proc)
		       (cons 'undecided 'undecided)))
	       cs-decode cs-encode)
	  (when (symbolp cs) (setq cs (cons cs cs)))
	  (setq cs-decode (car cs))
	  (setq cs-encode (cdr cs))
	  (unless cs-decode (setq cs-decode 'undecided))
	  (unless cs-encode (setq cs-encode 'undecided))
	  (setq cs-encode (tramp-coding-system-change-eol-conversion
			   cs-encode 'unix))
	  (when (search-forward "\r" nil t)
	    (setq cs-decode (tramp-coding-system-change-eol-conversion
			     cs-decode 'dos)))
	  (funcall (symbol-function 'set-buffer-process-coding-system)
		   cs-decode cs-encode))
      ;; Look for ^M and do something useful if found.
      (when (search-forward "\r" nil t)
	;; We have found a ^M but cannot frob the process coding system
	;; because we're running on a non-MULE Emacs.  Let's try
	;; stty, instead.
	(tramp-send-command vec "stty -onlcr" t))))
  (tramp-send-command vec "set +o vi +o emacs" t)

  ;; Check whether the output of "uname -sr" has been changed.  If
  ;; yes, this is a strong indication that we must expire all
  ;; connection properties.
  (tramp-message vec 5 "Checking system information")
  (let ((old-uname (tramp-get-connection-property vec "uname" nil))
	(new-uname
	 (tramp-set-connection-property
	  vec "uname"
	  (tramp-send-command-and-read vec "echo \\\"`uname -sr`\\\""))))
    (when (and (stringp old-uname) (not (string-equal old-uname new-uname)))
      (funcall (symbol-function 'tramp-cleanup-connection) vec)
      (signal
       'quit
       (list (format
	      "Connection reset, because remote host changed from `%s' to `%s'"
	      old-uname new-uname)))))

  ;; Check whether the remote host suffers from buggy
  ;; `send-process-string'.  This is known for FreeBSD (see comment in
  ;; `send_process', file process.c).  I've tested sending 624 bytes
  ;; successfully, sending 625 bytes failed.  Emacs makes a hack when
  ;; this host type is detected locally.  It cannot handle remote
  ;; hosts, though.
  (with-connection-property proc "chunksize"
    (cond
     ((and (integerp tramp-chunksize) (> tramp-chunksize 0))
      tramp-chunksize)
     (t
      (tramp-message
       vec 5 "Checking remote host type for `send-process-string' bug")
      (if (string-match
	   "^FreeBSD" (tramp-get-connection-property vec "uname" ""))
	  500 0))))

  ;; Set remote PATH variable.
  (tramp-set-remote-path vec)

  ;; Search for a good shell before searching for a command which
  ;; checks if a file exists. This is done because Tramp wants to use
  ;; "test foo; echo $?" to check if various conditions hold, and
  ;; there are buggy /bin/sh implementations which don't execute the
  ;; "echo $?"  part if the "test" part has an error.  In particular,
  ;; the Solaris /bin/sh is a problem.  I'm betting that all systems
  ;; with buggy /bin/sh implementations will have a working bash or
  ;; ksh.  Whee...
  (tramp-find-shell vec)

  ;; Disable unexpected output.
  (tramp-send-command vec "mesg n; biff n" t)

  ;; Set the environment.
  (tramp-message vec 5 "Setting default environment")
  (let ((env (copy-sequence tramp-remote-process-environment))
	unset item)
    (while env
      (setq item (split-string (car env) "="))
      (if (and (stringp (cadr item)) (not (string-equal (cadr item) "")))
	  (tramp-send-command
	   vec (format "%s=%s; export %s" (car item) (cadr item) (car item)) t)
	(push (car item) unset))
      (setq env (cdr env)))
    (when unset
      (tramp-send-command
       vec (format "unset %s" (mapconcat 'identity unset " "))))) t)

;; CCC: We should either implement a Perl version of base64 encoding
;; and decoding.  Then we just use that in the last item.  The other
;; alternative is to use the Perl version of UU encoding.  But then
;; we need a Lisp version of uuencode.
;;
;; Old text from documentation of tramp-methods:
;; Using a uuencode/uudecode inline method is discouraged, please use one
;; of the base64 methods instead since base64 encoding is much more
;; reliable and the commands are more standardized between the different
;; Unix versions.  But if you can't use base64 for some reason, please
;; note that the default uudecode command does not work well for some
;; Unices, in particular AIX and Irix.  For AIX, you might want to use
;; the following command for uudecode:
;;
;;     sed '/^begin/d;/^[` ]$/d;/^end/d' | iconv -f uucode -t ISO8859-1
;;
;; For Irix, no solution is known yet.

(defconst tramp-local-coding-commands
  '((b64 base64-encode-region base64-decode-region)
    (uu  tramp-uuencode-region uudecode-decode-region)
    (pack
     "perl -e 'binmode STDIN; binmode STDOUT; print pack(q{u*}, join q{}, <>)'"
     "perl -e 'binmode STDIN; binmode STDOUT; print unpack(q{u*}, join q{}, <>)'"))
  "List of local coding commands for inline transfer.
Each item is a list that looks like this:

\(FORMAT ENCODING DECODING)

FORMAT is  symbol describing the encoding/decoding format.  It can be
`b64' for base64 encoding, `uu' for uu encoding, or `pack' for simple packing.

ENCODING and DECODING can be strings, giving commands, or symbols,
giving functions.  If they are strings, then they can contain
the \"%s\" format specifier.  If that specifier is present, the input
filename will be put into the command line at that spot.  If the
specifier is not present, the input should be read from standard
input.

If they are functions, they will be called with two arguments, start
and end of region, and are expected to replace the region contents
with the encoded or decoded results, respectively.")

(defconst tramp-remote-coding-commands
  '((b64 "mimencode -b" "mimencode -u -b")
    (b64 "mmencode -b" "mmencode -u -b")
    (b64 "recode data..base64" "recode base64..data")
    (b64 tramp-perl-encode-with-module tramp-perl-decode-with-module)
    (b64 tramp-perl-encode tramp-perl-decode)
    (uu  "uuencode xxx" "uudecode -o /dev/stdout")
    (uu  "uuencode xxx" "uudecode -o -")
    (uu  "uuencode xxx" "uudecode -p")
    (uu  "uuencode xxx" tramp-uudecode)
    (pack
     "perl -e 'binmode STDIN; binmode STDOUT; print pack(q{u*}, join q{}, <>)'"
     "perl -e 'binmode STDIN; binmode STDOUT; print unpack(q{u*}, join q{}, <>)'"))
  "List of remote coding commands for inline transfer.
Each item is a list that looks like this:

\(FORMAT ENCODING DECODING)

FORMAT is  symbol describing the encoding/decoding format.  It can be
`b64' for base64 encoding, `uu' for uu encoding, or `pack' for simple packing.

ENCODING and DECODING can be strings, giving commands, or symbols,
giving variables.  If they are strings, then they can contain
the \"%s\" format specifier.  If that specifier is present, the input
filename will be put into the command line at that spot.  If the
specifier is not present, the input should be read from standard
input.

If they are variables, this variable is a string containing a Perl
implementation for this functionality.  This Perl program will be transferred
to the remote host, and it is avalible as shell function with the same name.")

(defun tramp-find-inline-encoding (vec)
  "Find an inline transfer encoding that works.
Goes through the list `tramp-local-coding-commands' and
`tramp-remote-coding-commands'."
  (save-excursion
    (let ((local-commands tramp-local-coding-commands)
	  (magic "xyzzy")
	  loc-enc loc-dec rem-enc rem-dec litem ritem found)
      (while (and local-commands (not found))
	(setq litem (pop local-commands))
	(catch 'wont-work-local
	  (let ((format (nth 0 litem))
		(remote-commands tramp-remote-coding-commands))
	    (setq loc-enc (nth 1 litem))
	    (setq loc-dec (nth 2 litem))
	    ;; If the local encoder or decoder is a string, the
	    ;; corresponding command has to work locally.
	    (if (not (stringp loc-enc))
		(tramp-message
		 vec 5 "Checking local encoding function `%s'" loc-enc)
	      (tramp-message
	       vec 5 "Checking local encoding command `%s' for sanity" loc-enc)
	      (unless (zerop (tramp-call-local-coding-command
			      loc-enc nil nil))
		(throw 'wont-work-local nil)))
	    (if (not (stringp loc-dec))
		(tramp-message
		 vec 5 "Checking local decoding function `%s'" loc-dec)
	      (tramp-message
	       vec 5 "Checking local decoding command `%s' for sanity" loc-dec)
	      (unless (zerop (tramp-call-local-coding-command
			      loc-dec nil nil))
		(throw 'wont-work-local nil)))
	    ;; Search for remote coding commands with the same format
	    (while (and remote-commands (not found))
	      (setq ritem (pop remote-commands))
	      (catch 'wont-work-remote
		(when (equal format (nth 0 ritem))
		  (setq rem-enc (nth 1 ritem))
		  (setq rem-dec (nth 2 ritem))
		  ;; Check if remote encoding and decoding commands can be
		  ;; called remotely with null input and output.  This makes
		  ;; sure there are no syntax errors and the command is really
		  ;; found.  Note that we do not redirect stdout to /dev/null,
		  ;; for two reasons: when checking the decoding command, we
		  ;; actually check the output it gives.  And also, when
		  ;; redirecting "mimencode" output to /dev/null, then as root
		  ;; it might change the permissions of /dev/null!
		  (when (not (stringp rem-enc))
		    (let ((name (symbol-name rem-enc)))
		      (while (string-match (regexp-quote "-") name)
			(setq name (replace-match "_" nil t name)))
		      (tramp-maybe-send-script vec (symbol-value rem-enc) name)
		      (setq rem-enc name)))
		  (tramp-message
		   vec 5
		   "Checking remote encoding command `%s' for sanity" rem-enc)
		  (unless (zerop (tramp-send-command-and-check
				  vec (format "%s </dev/null" rem-enc) t))
		    (throw 'wont-work-remote nil))

		  (when (not (stringp rem-dec))
		    (let ((name (symbol-name rem-dec)))
		      (while (string-match (regexp-quote "-") name)
			(setq name (replace-match "_" nil t name)))
		      (tramp-maybe-send-script vec (symbol-value rem-dec) name)
		      (setq rem-dec name)))
		  (tramp-message
		   vec 5
		   "Checking remote decoding command `%s' for sanity" rem-dec)
		  (unless (zerop (tramp-send-command-and-check
				  vec
				  (format "echo %s | %s | %s"
					  magic rem-enc rem-dec) t))
		    (throw 'wont-work-remote nil))

		  (with-current-buffer (tramp-get-buffer vec)
		    (goto-char (point-min))
		    (unless (looking-at (regexp-quote magic))
		      (throw 'wont-work-remote nil)))

		  ;; `rem-enc' and `rem-dec' could be a string meanwhile.
		  (setq rem-enc (nth 1 ritem))
		  (setq rem-dec (nth 2 ritem))
		  (setq found t)))))))

      ;; Did we find something?  If not, issue an error.
      (unless found
	(kill-process (tramp-get-connection-process vec))
	(tramp-error
	 vec 'file-error "Couldn't find an inline transfer encoding"))

      ;; Set connection properties.
      (tramp-message vec 5 "Using local encoding `%s'" loc-enc)
      (tramp-set-connection-property vec "local-encoding" loc-enc)
      (tramp-message vec 5 "Using local decoding `%s'" loc-dec)
      (tramp-set-connection-property vec "local-decoding" loc-dec)
      (tramp-message vec 5 "Using remote encoding `%s'" rem-enc)
      (tramp-set-connection-property vec "remote-encoding" rem-enc)
      (tramp-message vec 5 "Using remote decoding `%s'" rem-dec)
      (tramp-set-connection-property vec "remote-decoding" rem-dec))))

(defun tramp-call-local-coding-command (cmd input output)
  "Call the local encoding or decoding command.
If CMD contains \"%s\", provide input file INPUT there in command.
Otherwise, INPUT is passed via standard input.
INPUT can also be nil which means `/dev/null'.
OUTPUT can be a string (which specifies a filename), or t (which
means standard output and thus the current buffer), or nil (which
means discard it)."
  (tramp-local-call-process
   tramp-encoding-shell
   (when (and input (not (string-match "%s" cmd))) input)
   (if (eq output t) t nil)
   nil
   tramp-encoding-command-switch
   (concat
    (if (string-match "%s" cmd) (format cmd input) cmd)
    (if (stringp output) (concat "> " output) ""))))

(defun tramp-compute-multi-hops (vec)
  "Expands VEC according to `tramp-default-proxies-alist'.
Gateway hops are already opened."
  (let ((target-alist `(,vec))
	(choices tramp-default-proxies-alist)
	item proxy)

    ;; Look for proxy hosts to be passed.
    (while choices
      (setq item (pop choices)
	    proxy (nth 2 item))
      (when (and
	     ;; host
	     (string-match (or (nth 0 item) "")
			   (or (tramp-file-name-host (car target-alist)) ""))
	     ;; user
	     (string-match (or (nth 1 item) "")
			   (or (tramp-file-name-user (car target-alist)) "")))
	(if (null proxy)
	    ;; No more hops needed.
	    (setq choices nil)
	  ;; Replace placeholders.
	  (setq proxy
		(format-spec
		 proxy
		 `((?u . ,(or (tramp-file-name-user (car target-alist)) ""))
		   (?h . ,(or (tramp-file-name-host (car target-alist)) "")))))
	  (with-parsed-tramp-file-name proxy l
	    ;; Add the hop.
	    (add-to-list 'target-alist l)
	    ;; Start next search.
	    (setq choices tramp-default-proxies-alist)))))

    ;; Handle gateways.
    (when (and (boundp 'tramp-gw-tunnel-method)
	       (string-match (format
			      "^\\(%s\\|%s\\)$"
			      (symbol-value 'tramp-gw-tunnel-method)
			      (symbol-value 'tramp-gw-socks-method))
			     (tramp-file-name-method (car target-alist))))
      (let ((gw (pop target-alist))
	    (hop (pop target-alist)))
	;; Is the method prepared for gateways?
	(unless (tramp-get-method-parameter
		 (tramp-file-name-method hop) 'tramp-default-port)
	  (tramp-error
	   vec 'file-error
	   "Method `%s' is not supported for gateway access."
	   (tramp-file-name-method hop)))
	;; Add default port if needed.
	(unless
	    (string-match
	     tramp-host-with-port-regexp (tramp-file-name-host hop))
	  (aset hop 2
		(concat
		 (tramp-file-name-host hop) tramp-prefix-port-format
		 (number-to-string
		  (tramp-get-method-parameter
		   (tramp-file-name-method hop) 'tramp-default-port)))))
	;; Open the gateway connection.
	(add-to-list
	 'target-alist
	 (vector
	  (tramp-file-name-method hop) (tramp-file-name-user hop)
	  (funcall (symbol-function 'tramp-gw-open-connection) vec gw hop) nil))
	;; For the password prompt, we need the correct values.
	;; Therefore, we must remember the gateway vector.  But we
	;; cannot do it as connection property, because it shouldn't
	;; be persistent.  And we have no started process yet either.
	(tramp-set-file-property (car target-alist) "" "gateway" hop)))

    ;; Foreign and out-of-band methods are not supported for multi-hops.
    (when (cdr target-alist)
      (setq choices target-alist)
      (while choices
	(setq item (pop choices))
	(when
	    (or
	     (not
	      (tramp-get-method-parameter
	       (tramp-file-name-method item) 'tramp-login-program))
	     (tramp-get-method-parameter
	      (tramp-file-name-method item) 'tramp-copy-program))
	  (tramp-error
	   vec 'file-error
	   "Method `%s' is not supported for multi-hops."
	   (tramp-file-name-method item)))))

    ;; In case the host name is not used for the remote shell
    ;; command, the user could be misguided by applying a random
    ;; hostname.
    (let* ((v (car target-alist))
	   (method (tramp-file-name-method v))
	   (host (tramp-file-name-host v)))
      (unless
	  (or
	   ;; There are multi-hops.
	   (cdr target-alist)
	   ;; The host name is used for the remote shell command.
	   (member
	    '("%h") (tramp-get-method-parameter method 'tramp-login-args))
	   ;; The host is local.  We cannot use `tramp-local-host-p'
	   ;; here, because it opens a connection as well.
	   (string-match
	    (concat "^" (regexp-opt (list "localhost" (system-name)) t) "$")
	    host))
	(tramp-error
	 v 'file-error
	 "Host `%s' looks like a remote host, `%s' can only use the local host"
	 host method)))

    ;; Result.
    target-alist))

(defun tramp-maybe-open-connection (vec)
  "Maybe open a connection VEC.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
  (let ((p (tramp-get-connection-process vec))
	(process-environment (copy-sequence process-environment)))

    ;; If too much time has passed since last command was sent, look
    ;; whether process is still alive.  If it isn't, kill it.  When
    ;; using ssh, it can sometimes happen that the remote end has hung
    ;; up but the local ssh client doesn't recognize this until it
    ;; tries to send some data to the remote end.  So that's why we
    ;; try to send a command from time to time, then look again
    ;; whether the process is really alive.
    (condition-case nil
	(when (and (> (tramp-time-diff
		       (current-time)
		       (tramp-get-connection-property
			p "last-cmd-time" '(0 0 0)))
		      60)
		   p (processp p) (memq (process-status p) '(run open)))
	  (tramp-send-command vec "echo are you awake" t t)
	  (unless (and (memq (process-status p) '(run open))
		       (tramp-wait-for-output p 10))
	    ;; The error will be catched locally.
	    (tramp-error vec 'file-error "Awake did fail")))
      (file-error
       (tramp-flush-connection-property vec nil)
       (tramp-flush-connection-property p nil)
       (delete-process p)
       (setq p nil)))

    ;; New connection must be opened.
    (unless (and p (processp p) (memq (process-status p) '(run open)))

      ;; We call `tramp-get-buffer' in order to get a debug buffer for
      ;; messages from the beginning.
      (tramp-get-buffer vec)
      (if (zerop (length (tramp-file-name-user vec)))
	  (tramp-message
	   vec 3 "Opening connection for %s using %s..."
	   (tramp-file-name-host vec)
	   (tramp-file-name-method vec))
	(tramp-message
	 vec 3 "Opening connection for %s@%s using %s..."
	 (tramp-file-name-user vec)
	 (tramp-file-name-host vec)
	 (tramp-file-name-method vec)))

      ;; Start new process.
      (when (and p (processp p))
	(delete-process p))
      (setenv "TERM" tramp-terminal-type)
      (setenv "LC_ALL" "C")
      (setenv "PROMPT_COMMAND")
      (setenv "PS1" "$ ")
      (let* ((target-alist (tramp-compute-multi-hops vec))
	     (process-connection-type tramp-process-connection-type)
	     (process-adaptive-read-buffering nil)
	     (coding-system-for-read nil)
	     ;; This must be done in order to avoid our file name handler.
	     (p (let ((default-directory
			(tramp-compat-temporary-file-directory)))
		  (start-process
		   (or (tramp-get-connection-property vec "process-name" nil)
		       (tramp-buffer-name vec))
		   (tramp-get-connection-buffer vec)
		   tramp-encoding-shell)))
	     (first-hop t))

	(tramp-message
	 vec 6 "%s" (mapconcat 'identity (process-command p) " "))

	;; Check whether process is alive.
	(set-process-sentinel p 'tramp-process-sentinel)
	(tramp-set-process-query-on-exit-flag p nil)
	(tramp-message vec 3 "Waiting 60s for local shell to come up...")
	(tramp-barf-if-no-shell-prompt
	 p 60 "Couldn't find local shell prompt %s" tramp-encoding-shell)

	;; Now do all the connections as specified.
	(while target-alist
	  (let* ((hop (car target-alist))
		 (l-method (tramp-file-name-method hop))
		 (l-user (tramp-file-name-user hop))
		 (l-host (tramp-file-name-host hop))
		 (l-port nil)
		 (login-program
		  (tramp-get-method-parameter l-method 'tramp-login-program))
		 (login-args
		  (tramp-get-method-parameter l-method 'tramp-login-args))
		 (gw-args
		  (tramp-get-method-parameter l-method 'tramp-gw-args))
		 (gw (tramp-get-file-property hop "" "gateway" nil))
		 (g-method (and gw (tramp-file-name-method gw)))
		 (g-user (and gw (tramp-file-name-user gw)))
		 (g-host (and gw (tramp-file-name-host gw)))
		 (command login-program)
		 ;; We don't create the temporary file.  In fact, it
		 ;; is just a prefix for the ControlPath option of
		 ;; ssh; the real temporary file has another name, and
		 ;; it is created and protected by ssh.  It is also
		 ;; removed by ssh, when the connection is closed.
		 (tmpfile
		  (tramp-set-connection-property
		   p "temp-file"
		   (make-temp-name
		    (expand-file-name
		     tramp-temp-name-prefix
		     (tramp-compat-temporary-file-directory)))))
		 spec)

	    ;; Add gateway arguments if necessary.
	    (when (and gw gw-args)
	      (setq login-args (append login-args gw-args)))

	    ;; Check for port number.  Until now, there's no need for handling
	    ;; like method, user, host.
	    (when (string-match tramp-host-with-port-regexp l-host)
	      (setq l-port (match-string 2 l-host)
		    l-host (match-string 1 l-host)))

	    ;; Set variables for computing the prompt for reading password.
	    ;; They can also be derived from a gatewy.
	    (setq tramp-current-method (or g-method l-method)
		  tramp-current-user   (or g-user   l-user)
		  tramp-current-host   (or g-host   l-host))

	    ;; Replace login-args place holders.
	    (setq
	     l-host (or l-host "")
	     l-user (or l-user "")
	     l-port (or l-port "")
	     spec `((?h . ,l-host) (?u . ,l-user) (?p . ,l-port)
		    (?t . ,tmpfile))
	     command
	     (concat
	      command " "
	      (mapconcat
	       '(lambda (x)
		  (setq x (mapcar '(lambda (y) (format-spec y spec)) x))
		  (unless (member "" x) (mapconcat 'identity x " ")))
	       login-args " ")
	      ;; String to detect failed connection.  Every single word must
	      ;; be enclosed with '\"'; otherwise it is detected
	      ;; during connection setup.
	      ;; Local shell could be a Windows COMSPEC.  It doesn't know
	      ;; the ";" syntax, but we must exit always for `start-process'.
	      ;; "exec" does not work either.
	      (if first-hop
		  " && exit || exit"
		"; echo \"Tramp\" \"connection\" \"closed\"; sleep 1"))
	     ;; We don't reach a Windows shell.  Could be initial only.
	     first-hop nil)

	    ;; Send the command.
	    (tramp-message vec 3 "Sending command `%s'" command)
	    (tramp-send-command vec command t t)
	    (tramp-process-actions p vec tramp-actions-before-shell 60)
	    (tramp-message vec 3 "Found remote shell prompt on `%s'" l-host))
	  ;; Next hop.
	  (setq target-alist (cdr target-alist)))

	;; Make initial shell settings.
	(tramp-open-connection-setup-interactive-shell p vec)))))

(defun tramp-send-command (vec command &optional neveropen nooutput)
  "Send the COMMAND to connection VEC.
Erases temporary buffer before sending the command.  If optional
arg NEVEROPEN is non-nil, never try to open the connection.  This
is meant to be used from `tramp-maybe-open-connection' only.  The
function waits for output unless NOOUTPUT is set."
  (unless neveropen (tramp-maybe-open-connection vec))
  (let ((p (tramp-get-connection-process vec)))
    (when (tramp-get-connection-property p "remote-echo" nil)
      ;; We mark the command string that it can be erased in the output buffer.
      (tramp-set-connection-property p "check-remote-echo" t)
      (setq command (format "%s%s%s" tramp-echo-mark command tramp-echo-mark)))
    (tramp-message vec 6 "%s" command)
    (tramp-send-string vec command)
    (unless nooutput (tramp-wait-for-output p))))

(defun tramp-wait-for-output (proc &optional timeout)
  "Wait for output from remote rsh command."
  (with-current-buffer (process-buffer proc)
    ;; Initially, `tramp-end-of-output' is "$ ".  There might be
    ;; leading escape sequences, which must be ignored.
    (let* ((regexp
	    (if (string-match (regexp-quote "\n") tramp-end-of-output)
		(mapconcat
		 'identity (split-string tramp-end-of-output "\n") "\r?\n")
	      (format "^[^$\n]*%s\r?$" (regexp-quote tramp-end-of-output))))
	   (found (tramp-wait-for-regexp proc timeout regexp)))
      (if found
	  (let (buffer-read-only)
	    (goto-char (point-max))
	    (re-search-backward regexp nil t)
	    (delete-region (point) (point-max)))
	(if timeout
	    (tramp-error
	     proc 'file-error
	     "[[Remote prompt `%s' not found in %d secs]]"
	     tramp-end-of-output timeout)
	  (tramp-error
	   proc 'file-error
	   "[[Remote prompt `%s' not found]]" tramp-end-of-output)))
      ;; Return value is whether end-of-output sentinel was found.
      found)))

(defun tramp-send-command-and-check (vec command &optional subshell)
  "Run COMMAND and check its exit status.
Sends `echo $?' along with the COMMAND for checking the exit status.  If
COMMAND is nil, just sends `echo $?'.  Returns the exit status found.

If the optional argument SUBSHELL is non-nil, the command is executed in
a subshell, ie surrounded by parentheses."
  (tramp-send-command
   vec
   (concat (if subshell "( " "")
	   command
	   (if command " 2>/dev/null; " "")
	   "echo tramp_exit_status $?"
	   (if subshell " )" " ")))
  (with-current-buffer (tramp-get-connection-buffer vec)
    (goto-char (point-max))
    (unless (re-search-backward "tramp_exit_status [0-9]+" nil t)
      (tramp-error
       vec 'file-error "Couldn't find exit status of `%s'" command))
    (skip-chars-forward "^ ")
    (prog1
     (read (current-buffer))
     (let (buffer-read-only) (delete-region (match-beginning 0) (point-max))))))

(defun tramp-barf-unless-okay (vec command fmt &rest args)
  "Run COMMAND, check exit status, throw error if exit status not okay.
Similar to `tramp-send-command-and-check' but accepts two more arguments
FMT and ARGS which are passed to `error'."
  (unless (zerop (tramp-send-command-and-check vec command))
    (apply 'tramp-error vec 'file-error fmt args)))

(defun tramp-send-command-and-read (vec command)
  "Run COMMAND and return the output, which must be a Lisp expression.
In case there is no valid Lisp expression, it raises an error"
  (tramp-barf-unless-okay vec command "`%s' returns with error" command)
  (with-current-buffer (tramp-get-connection-buffer vec)
    ;; Read the expression.
    (goto-char (point-min))
    (condition-case nil
	(prog1 (read (current-buffer))
	  ;; Error handling.
	  (when (re-search-forward "\\S-" (tramp-compat-line-end-position) t)
	    (error nil)))
      (error (tramp-error
	      vec 'file-error
	      "`%s' does not return a valid Lisp expression: `%s'"
	      command (buffer-string))))))

;; It seems that Tru64 Unix does not like it if long strings are sent
;; to it in one go.  (This happens when sending the Perl
;; `file-attributes' implementation, for instance.)  Therefore, we
;; have this function which sends the string in chunks.
(defun tramp-send-string (vec string)
  "Send the STRING via connection VEC.

The STRING is expected to use Unix line-endings, but the lines sent to
the remote host use line-endings as defined in the variable
`tramp-rsh-end-of-line'.  The communication buffer is erased before sending."
  (let* ((p (tramp-get-connection-process vec))
	 (chunksize (tramp-get-connection-property p "chunksize" nil)))
    (unless p
      (tramp-error
       vec 'file-error "Can't send string to remote host -- not logged in"))
    (tramp-set-connection-property p "last-cmd-time" (current-time))
    (tramp-message vec 10 "%s" string)
    (with-current-buffer (tramp-get-connection-buffer vec)
      ;; Clean up the buffer.  We cannot call `erase-buffer' because
      ;; narrowing might be in effect.
      (let (buffer-read-only) (delete-region (point-min) (point-max)))
      ;; Replace "\n" by `tramp-rsh-end-of-line'.
      (setq string
	    (mapconcat 'identity
		       (split-string string "\n")
		       tramp-rsh-end-of-line))
      (unless (or (string= string "")
		  (string-equal (substring string -1) tramp-rsh-end-of-line))
	(setq string (concat string tramp-rsh-end-of-line)))
      ;; Send the string.
      (if (and chunksize (not (zerop chunksize)))
	  (let ((pos 0)
		(end (length string)))
	    (while (< pos end)
	      (tramp-message
	       vec 10 "Sending chunk from %s to %s"
	       pos (min (+ pos chunksize) end))
	      (process-send-string
	       p (substring string pos (min (+ pos chunksize) end)))
	      (setq pos (+ pos chunksize))))
	(process-send-string p string)))))

(defun tramp-mode-string-to-int (mode-string)
  "Converts a ten-letter `drwxrwxrwx'-style mode string into mode bits."
  (let* (case-fold-search
	 (mode-chars (string-to-vector mode-string))
         (owner-read (aref mode-chars 1))
         (owner-write (aref mode-chars 2))
         (owner-execute-or-setid (aref mode-chars 3))
         (group-read (aref mode-chars 4))
         (group-write (aref mode-chars 5))
         (group-execute-or-setid (aref mode-chars 6))
         (other-read (aref mode-chars 7))
         (other-write (aref mode-chars 8))
         (other-execute-or-sticky (aref mode-chars 9)))
    (save-match-data
      (logior
       (cond
	((char-equal owner-read ?r) (tramp-octal-to-decimal "00400"))
	((char-equal owner-read ?-) 0)
	(t (error "Second char `%c' must be one of `r-'" owner-read)))
       (cond
	((char-equal owner-write ?w) (tramp-octal-to-decimal "00200"))
	((char-equal owner-write ?-) 0)
	(t (error "Third char `%c' must be one of `w-'" owner-write)))
       (cond
	((char-equal owner-execute-or-setid ?x)
	 (tramp-octal-to-decimal "00100"))
	((char-equal owner-execute-or-setid ?S)
	 (tramp-octal-to-decimal "04000"))
	((char-equal owner-execute-or-setid ?s)
	 (tramp-octal-to-decimal "04100"))
	((char-equal owner-execute-or-setid ?-) 0)
	(t (error "Fourth char `%c' must be one of `xsS-'"
		  owner-execute-or-setid)))
       (cond
	((char-equal group-read ?r) (tramp-octal-to-decimal "00040"))
	((char-equal group-read ?-) 0)
	(t (error "Fifth char `%c' must be one of `r-'" group-read)))
       (cond
	((char-equal group-write ?w) (tramp-octal-to-decimal "00020"))
	((char-equal group-write ?-) 0)
	(t (error "Sixth char `%c' must be one of `w-'" group-write)))
       (cond
	((char-equal group-execute-or-setid ?x)
	 (tramp-octal-to-decimal "00010"))
	((char-equal group-execute-or-setid ?S)
	 (tramp-octal-to-decimal "02000"))
	((char-equal group-execute-or-setid ?s)
	 (tramp-octal-to-decimal "02010"))
	((char-equal group-execute-or-setid ?-) 0)
	(t (error "Seventh char `%c' must be one of `xsS-'"
		  group-execute-or-setid)))
       (cond
	((char-equal other-read ?r)
	 (tramp-octal-to-decimal "00004"))
	((char-equal other-read ?-) 0)
	(t (error "Eighth char `%c' must be one of `r-'" other-read)))
       (cond
         ((char-equal other-write ?w) (tramp-octal-to-decimal "00002"))
	 ((char-equal other-write ?-) 0)
         (t (error "Nineth char `%c' must be one of `w-'" other-write)))
       (cond
	((char-equal other-execute-or-sticky ?x)
	 (tramp-octal-to-decimal "00001"))
	((char-equal other-execute-or-sticky ?T)
	 (tramp-octal-to-decimal "01000"))
	((char-equal other-execute-or-sticky ?t)
	 (tramp-octal-to-decimal "01001"))
	((char-equal other-execute-or-sticky ?-) 0)
	(t (error "Tenth char `%c' must be one of `xtT-'"
		  other-execute-or-sticky)))))))

(defun tramp-convert-file-attributes (vec attr)
  "Convert file-attributes ATTR generated by perl script, stat or ls.
Convert file mode bits to string and set virtual device number.
Return ATTR."
  ;; Convert last access time.
  (unless (listp (nth 4 attr))
    (setcar (nthcdr 4 attr)
	    (list (floor (nth 4 attr) 65536)
		  (floor (mod (nth 4 attr) 65536)))))
  ;; Convert last modification time.
  (unless (listp (nth 5 attr))
    (setcar (nthcdr 5 attr)
	    (list (floor (nth 5 attr) 65536)
		  (floor (mod (nth 5 attr) 65536)))))
  ;; Convert last status change time.
  (unless (listp (nth 6 attr))
    (setcar (nthcdr 6 attr)
	    (list (floor (nth 6 attr) 65536)
		  (floor (mod (nth 6 attr) 65536)))))
  ;; Convert file size.
  (when (< (nth 7 attr) 0)
    (setcar (nthcdr 7 attr) -1))
  (when (and (floatp (nth 7 attr))
	     (<= (nth 7 attr) (tramp-compat-most-positive-fixnum)))
    (setcar (nthcdr 7 attr) (round (nth 7 attr))))
  ;; Convert file mode bits to string.
  (unless (stringp (nth 8 attr))
    (setcar (nthcdr 8 attr) (tramp-file-mode-from-int (nth 8 attr))))
  ;; Convert directory indication bit.
  (if (string-match "^d" (nth 8 attr))
      (setcar attr t)
    (if (and (listp (car attr)) (stringp (caar attr))
	     (string-match ".+ -> .\\(.+\\)." (caar attr)))
	(setcar attr (match-string 1 (caar attr)))
      (setcar attr nil)))
  ;; Set file's gid change bit.
  (setcar (nthcdr 9 attr)
	  (if (numberp (nth 3 attr))
	      (not (= (nth 3 attr)
		      (tramp-get-remote-gid vec 'integer)))
	    (not (string-equal
		  (nth 3 attr)
		  (tramp-get-remote-gid vec 'string)))))
  ;; Convert inode.
  (unless (listp (nth 10 attr))
    (setcar (nthcdr 10 attr)
	    (condition-case nil
		(list (floor (nth 10 attr) 65536)
		      (floor (mod (nth 10 attr) 65536)))
	      ;; Inodes can be incredible huge.  We must hide this.
	      (error (tramp-get-inode vec)))))
  ;; Set virtual device number.
  (setcar (nthcdr 11 attr)
          (tramp-get-device vec))
  attr)

(defun tramp-get-inode (vec)
  "Returns the virtual inode number.
If it doesn't exist, generate a new one."
  (let ((string (tramp-make-tramp-file-name
		 (tramp-file-name-method vec)
		 (tramp-file-name-user vec)
		 (tramp-file-name-host vec)
		 "")))
    (unless (assoc string tramp-inodes)
      (add-to-list 'tramp-inodes
		   (list string (length tramp-inodes))))
    (nth 1 (assoc string tramp-inodes))))

(defun tramp-get-device (vec)
  "Returns the virtual device number.
If it doesn't exist, generate a new one."
  (let ((string (tramp-make-tramp-file-name
		 (tramp-file-name-method vec)
		 (tramp-file-name-user vec)
		 (tramp-file-name-host vec)
		 "")))
    (unless (assoc string tramp-devices)
      (add-to-list 'tramp-devices
		   (list string (length tramp-devices))))
    (list -1 (nth 1 (assoc string tramp-devices)))))

(defun tramp-file-mode-from-int (mode)
  "Turn an integer representing a file mode into an ls(1)-like string."
  (let ((type	(cdr (assoc (logand (lsh mode -12) 15) tramp-file-mode-type-map)))
	(user	(logand (lsh mode -6) 7))
	(group	(logand (lsh mode -3) 7))
	(other	(logand (lsh mode -0) 7))
	(suid	(> (logand (lsh mode -9) 4) 0))
	(sgid	(> (logand (lsh mode -9) 2) 0))
	(sticky	(> (logand (lsh mode -9) 1) 0)))
    (setq user  (tramp-file-mode-permissions user  suid "s"))
    (setq group (tramp-file-mode-permissions group sgid "s"))
    (setq other (tramp-file-mode-permissions other sticky "t"))
    (concat type user group other)))

(defun tramp-file-mode-permissions (perm suid suid-text)
  "Convert a permission bitset into a string.
This is used internally by `tramp-file-mode-from-int'."
  (let ((r (> (logand perm 4) 0))
	(w (> (logand perm 2) 0))
	(x (> (logand perm 1) 0)))
    (concat (or (and r "r") "-")
	    (or (and w "w") "-")
	    (or (and suid x suid-text)	; suid, execute
		(and suid (upcase suid-text)) ; suid, !execute
		(and x "x") "-"))))	; !suid

(defun tramp-decimal-to-octal (i)
  "Return a string consisting of the octal digits of I.
Not actually used.  Use `(format \"%o\" i)' instead?"
  (cond ((< i 0) (error "Cannot convert negative number to octal"))
        ((not (integerp i)) (error "Cannot convert non-integer to octal"))
        ((zerop i) "0")
        (t (concat (tramp-decimal-to-octal (/ i 8))
                   (number-to-string (% i 8))))))


;; Kudos to Gerd Moellmann for this suggestion.
(defun tramp-octal-to-decimal (ostr)
  "Given a string of octal digits, return a decimal number."
  (let ((x (or ostr "")))
    ;; `save-match' is in `tramp-mode-string-to-int' which calls this.
    (unless (string-match "\\`[0-7]*\\'" x)
      (error "Non-octal junk in string `%s'" x))
    (string-to-number ostr 8)))

(defun tramp-shell-case-fold (string)
  "Converts STRING to shell glob pattern which ignores case."
  (mapconcat
   (lambda (c)
     (if (equal (downcase c) (upcase c))
         (vector c)
       (format "[%c%c]" (downcase c) (upcase c))))
   string
   ""))


;; ------------------------------------------------------------
;; -- Tramp file names --
;; ------------------------------------------------------------
;; Conversion functions between external representation and
;; internal data structure.  Convenience functions for internal
;; data structure.

(defun tramp-file-name-p (vec)
  "Check whether VEC is a Tramp object."
  (and (vectorp vec) (= 4 (length vec))))

(defun tramp-file-name-method (vec)
  "Return method component of VEC."
  (and (tramp-file-name-p vec) (aref vec 0)))

(defun tramp-file-name-user (vec)
  "Return user component of VEC."
  (and (tramp-file-name-p vec) (aref vec 1)))

(defun tramp-file-name-host (vec)
  "Return host component of VEC."
  (and (tramp-file-name-p vec) (aref vec 2)))

(defun tramp-file-name-localname (vec)
  "Return localname component of VEC."
  (and (tramp-file-name-p vec) (aref vec 3)))

;; The host part of a Tramp file name vector can be of kind
;; "host#port".  Sometimes, we must extract these parts.
(defun tramp-file-name-real-host (vec)
  "Return the host name of VEC without port."
  (let ((host (tramp-file-name-host vec)))
    (if (and (stringp host)
	     (string-match tramp-host-with-port-regexp host))
	(match-string 1 host)
      host)))

(defun tramp-file-name-port (vec)
  "Return the port number of VEC."
  (let ((host (tramp-file-name-host vec)))
    (and (stringp host)
	 (string-match tramp-host-with-port-regexp host)
	 (string-to-number (match-string 2 host)))))

(defun tramp-tramp-file-p (name)
  "Return t if NAME is a Tramp file."
  (save-match-data
    (string-match tramp-file-name-regexp name)))

(defun tramp-find-method (method user host)
  "Return the right method string to use.
This is METHOD, if non-nil. Otherwise, do a lookup in
`tramp-default-method-alist'."
  (or method
      (let ((choices tramp-default-method-alist)
	    lmethod item)
	(while choices
	  (setq item (pop choices))
	  (when (and (string-match (or (nth 0 item) "") (or host ""))
		     (string-match (or (nth 1 item) "") (or user "")))
	    (setq lmethod (nth 2 item))
	    (setq choices nil)))
	lmethod)
      tramp-default-method))

(defun tramp-find-user (method user host)
  "Return the right user string to use.
This is USER, if non-nil. Otherwise, do a lookup in
`tramp-default-user-alist'."
  (or user
      (let ((choices tramp-default-user-alist)
	    luser item)
	(while choices
	  (setq item (pop choices))
	  (when (and (string-match (or (nth 0 item) "") (or method ""))
		     (string-match (or (nth 1 item) "") (or host "")))
	    (setq luser (nth 2 item))
	    (setq choices nil)))
	luser)
      tramp-default-user))

(defun tramp-find-host (method user host)
  "Return the right host string to use.
This is HOST, if non-nil. Otherwise, it is `tramp-default-host'."
  (or (and (> (length host) 0) host)
      tramp-default-host))

(defun tramp-dissect-file-name (name &optional nodefault)
  "Return a `tramp-file-name' structure.
The structure consists of remote method, remote user, remote host
and localname (file name on remote host).  If NODEFAULT is
non-nil, the file name parts are not expanded to their default
values."
  (save-match-data
    (let ((match (string-match (nth 0 tramp-file-name-structure) name)))
      (unless match (error "Not a Tramp file name: %s" name))
      (let ((method    (match-string (nth 1 tramp-file-name-structure) name))
	    (user      (match-string (nth 2 tramp-file-name-structure) name))
	    (host      (match-string (nth 3 tramp-file-name-structure) name))
	    (localname (match-string (nth 4 tramp-file-name-structure) name)))
	(when (member method '("multi" "multiu"))
	  (error
	   "`%s' method is no longer supported, see (info \"(tramp)Multi-hops\")"
	   method))
	(if nodefault
	    (vector method user host localname)
	  (vector
	   (tramp-find-method method user host)
	   (tramp-find-user   method user host)
	   (tramp-find-host   method user host)
	   localname))))))

(defun tramp-equal-remote (file1 file2)
  "Checks, whether the remote parts of FILE1 and FILE2 are identical.
The check depends on method, user and host name of the files.  If
one of the components is missing, the default values are used.
The local file name parts of FILE1 and FILE2 are not taken into
account.

Example:

  (tramp-equal-remote \"/ssh::/etc\" \"/<your host name>:/home\")

would yield `t'.  On the other hand, the following check results in nil:

  (tramp-equal-remote \"/sudo::/etc\" \"/su::/etc\")"
  (and (stringp (file-remote-p file1))
       (stringp (file-remote-p file2))
       (string-equal (file-remote-p file1) (file-remote-p file2))))

(defun tramp-make-tramp-file-name (method user host localname)
  "Constructs a Tramp file name from METHOD, USER, HOST and LOCALNAME."
  (concat tramp-prefix-format
	  (when (not (zerop (length method)))
	    (concat method tramp-postfix-method-format))
	  (when (not (zerop (length user)))
	    (concat user tramp-postfix-user-format))
	  (when host host) tramp-postfix-host-format
	  (when localname localname)))

(defun tramp-completion-make-tramp-file-name (method user host localname)
  "Constructs a Tramp file name from METHOD, USER, HOST and LOCALNAME.
It must not be a complete Tramp file name, but as long as there are
necessary only.  This function will be used in file name completion."
  (concat tramp-prefix-format
	  (when (not (zerop (length method)))
	    (concat method tramp-postfix-method-format))
	  (when (not (zerop (length user)))
	    (concat user tramp-postfix-user-format))
	  (when (not (zerop (length host)))
	    (concat host tramp-postfix-host-format))
	  (when localname localname)))

(defun tramp-make-copy-program-file-name (vec)
  "Create a file name suitable to be passed to `rcp' and workalikes."
  (let ((user (tramp-file-name-user vec))
	(host (tramp-file-name-real-host vec))
	(localname (tramp-shell-quote-argument
		    (tramp-file-name-localname vec))))
    (if (not (zerop (length user)))
        (format "%s@%s:%s" user host localname)
      (format "%s:%s" host localname))))

(defun tramp-method-out-of-band-p (vec)
  "Return t if this is an out-of-band method, nil otherwise."
  (tramp-get-method-parameter (tramp-file-name-method vec) 'tramp-copy-program))

(defun tramp-local-host-p (vec)
  "Return t if this points to the local host, nil otherwise."
  ;; We cannot use `tramp-file-name-real-host'.  A port is an
  ;; indication for an ssh tunnel or alike.
  (let ((host (tramp-file-name-host vec)))
    (and
     (stringp host)
     (string-match
      (concat "^" (regexp-opt (list "localhost" (system-name)) t) "$") host)
     ;; The local temp directory must be writable for the other user.
     (file-writable-p
      (tramp-make-tramp-file-name
       (tramp-file-name-method vec)
       (tramp-file-name-user vec)
       host
       (tramp-compat-temporary-file-directory))))))

;; Variables local to connection.

(defun tramp-get-remote-path (vec)
  (with-connection-property vec "remote-path"
    (let* ((remote-path (tramp-compat-copy-tree tramp-remote-path))
	   (elt (memq 'tramp-default-remote-path remote-path))
	   (default-remote-path
	     (when elt
	       (condition-case nil
		   (symbol-name
		    (tramp-send-command-and-read vec "getconf PATH"))
		 ;; Default if "getconf" is not available.
		 (error
		  (tramp-message
		   vec 3
		   "`getconf PATH' not successful, using default value \"%s\"."
		   "/bin:/usr/bin")
		  "/bin:/usr/bin")))))
      (when elt
	;; Replace place holder `tramp-default-remote-path'.
	(setcdr elt
		(append
 		 (tramp-split-string default-remote-path ":")
		 (cdr elt)))
	(setq remote-path (delq 'tramp-default-remote-path remote-path)))

      ;; Remove non-existing directories.
      (delq
       nil
       (mapcar
	(lambda (x)
	  (and
	   (with-connection-property vec x
	     (file-directory-p
	      (tramp-make-tramp-file-name
	       (tramp-file-name-method vec)
	       (tramp-file-name-user vec)
	       (tramp-file-name-host vec)
	       x)))
	   x))
	remote-path)))))

(defun tramp-get-remote-tmpdir (vec)
  (with-connection-property vec "tmp-directory"
    (let ((dir (tramp-shell-quote-argument "/tmp")))
      (if (and (zerop
		(tramp-send-command-and-check
		 vec (format "%s -d %s" (tramp-get-test-command vec) dir)))
	       (zerop
		(tramp-send-command-and-check
		 vec (format "%s -w %s" (tramp-get-test-command vec) dir))))
	  dir
	(tramp-error vec 'file-error "Directory %s not accessible" dir)))))

(defun tramp-get-ls-command (vec)
  (with-connection-property vec "ls"
    (with-current-buffer (tramp-get-buffer vec)
      (tramp-message vec 5 "Finding a suitable `ls' command")
      (or
       (catch 'ls-found
	 (dolist (cmd '("ls" "gnuls" "gls"))
	   (let ((dl (tramp-get-remote-path vec))
		 result)
	     (while
		 (and
		  dl
		  (setq result
			(tramp-find-executable vec cmd dl t t)))
	       ;; Check parameter.
	       (when (zerop (tramp-send-command-and-check
			     vec (format "%s -lnd /" result)))
		 (throw 'ls-found result))
	       (setq dl (cdr dl))))))
       (tramp-error vec 'file-error "Couldn't find a proper `ls' command")))))

(defun tramp-get-test-command (vec)
  (with-connection-property vec "test"
    (with-current-buffer (tramp-get-buffer vec)
      (tramp-message vec 5 "Finding a suitable `test' command")
      (if (zerop (tramp-send-command-and-check vec "test 0"))
	  "test"
	(tramp-find-executable vec "test" (tramp-get-remote-path vec))))))

(defun tramp-get-test-nt-command (vec)
  ;; Does `test A -nt B' work?  Use abominable `find' construct if it
  ;; doesn't.  BSD/OS 4.0 wants the parentheses around the command,
  ;; for otherwise the shell crashes.
  (with-connection-property vec "test-nt"
    (or
     (progn
       (tramp-send-command
	vec (format "( %s / -nt / )" (tramp-get-test-command vec)))
       (with-current-buffer (tramp-get-buffer vec)
	 (goto-char (point-min))
	 (when (looking-at (regexp-quote tramp-end-of-output))
	   (format "%s %%s -nt %%s" (tramp-get-test-command vec)))))
     (progn
       (tramp-send-command
	vec
	(format
	 "tramp_test_nt () {\n%s -n \"`find $1 -prune -newer $2 -print`\"\n}"
	 (tramp-get-test-command vec)))
       "tramp_test_nt %s %s"))))

(defun tramp-get-file-exists-command (vec)
  (with-connection-property vec "file-exists"
    (with-current-buffer (tramp-get-buffer vec)
      (tramp-message vec 5 "Finding command to check if file exists")
      (tramp-find-file-exists-command vec))))

(defun tramp-get-remote-ln (vec)
  (with-connection-property vec "ln"
    (with-current-buffer (tramp-get-buffer vec)
      (tramp-message vec 5 "Finding a suitable `ln' command")
      (tramp-find-executable vec "ln" (tramp-get-remote-path vec)))))

(defun tramp-get-remote-perl (vec)
  (with-connection-property vec "perl"
    (with-current-buffer (tramp-get-buffer vec)
      (tramp-message vec 5 "Finding a suitable `perl' command")
      (or (tramp-find-executable vec "perl5" (tramp-get-remote-path vec))
	  (tramp-find-executable vec "perl" (tramp-get-remote-path vec))))))

(defun tramp-get-remote-stat (vec)
  (with-connection-property vec "stat"
    (with-current-buffer (tramp-get-buffer vec)
      (tramp-message vec 5 "Finding a suitable `stat' command")
      (let ((result (tramp-find-executable
		     vec "stat" (tramp-get-remote-path vec)))
	    tmp)
	;; Check whether stat(1) returns usable syntax.
	(when result
	  (setq tmp
		;; We don't want to display an error message.
		(with-temp-message (or (current-message) "")
		  (condition-case nil
		      (tramp-send-command-and-read
		       vec (format "%s -c '(\"%%N\")' /" result))
		    (error nil))))
	  (unless (and (listp tmp) (stringp (car tmp))
		       (string-match "^./.$" (car tmp)))
	    (setq result nil)))
	result))))

(defun tramp-get-remote-id (vec)
  (with-connection-property vec "id"
    (with-current-buffer (tramp-get-buffer vec)
      (tramp-message vec 5 "Finding POSIX `id' command")
      (or
       (catch 'id-found
	 (let ((dl (tramp-get-remote-path vec))
	       result)
	   (while
	       (and
		dl
		(setq result
		      (tramp-find-executable vec "id" dl t t)))
	     ;; Check POSIX parameter.
	     (when (zerop (tramp-send-command-and-check
			   vec (format "%s -u" result)))
	       (throw 'id-found result))
	     (setq dl (cdr dl)))))
       (tramp-error vec 'file-error "Couldn't find a POSIX `id' command")))))

(defun tramp-get-remote-uid (vec id-format)
  (with-connection-property vec (format "uid-%s" id-format)
    (let ((res (tramp-send-command-and-read
		vec
		(format "%s -u%s %s"
			(tramp-get-remote-id vec)
			(if (equal id-format 'integer) "" "n")
			(if (equal id-format 'integer)
			    "" "| sed -e s/^/\\\"/ -e s/\$/\\\"/")))))
      ;; The command might not always return a number.
      (if (and (equal id-format 'integer) (not (integerp res))) -1 res))))

(defun tramp-get-remote-gid (vec id-format)
  (with-connection-property vec (format "gid-%s" id-format)
    (let ((res (tramp-send-command-and-read
		vec
		(format "%s -g%s %s"
			(tramp-get-remote-id vec)
			(if (equal id-format 'integer) "" "n")
			(if (equal id-format 'integer)
			    "" "| sed -e s/^/\\\"/ -e s/\$/\\\"/")))))
      ;; The command might not always return a number.
      (if (and (equal id-format 'integer) (not (integerp res))) -1 res))))

(defun tramp-get-local-uid (id-format)
  (if (equal id-format 'integer) (user-uid) (user-login-name)))

(defun tramp-get-local-gid (id-format)
  (nth 3 (tramp-compat-file-attributes "~/" id-format)))

;; Some predefined connection properties.
(defun tramp-get-remote-coding (vec prop)
  ;; Local coding handles properties like remote coding.  So we could
  ;; call it without pain.
  (let ((ret (tramp-get-local-coding vec prop)))
    ;; The connection property might have been cached.  So we must send
    ;; the script - maybe.
    (when (not (stringp ret))
      (let ((name (symbol-name ret)))
	(while (string-match (regexp-quote "-") name)
	  (setq name (replace-match "_" nil t name)))
	(tramp-maybe-send-script vec (symbol-value ret) name)
	(setq ret name)))
    ;; Return the value.
    ret))

(defun tramp-get-local-coding (vec prop)
  (or
   (tramp-get-connection-property vec prop nil)
   (progn
     (tramp-find-inline-encoding vec)
     (tramp-get-connection-property vec prop nil))))

(defun tramp-get-method-parameter (method param)
  "Return the method parameter PARAM.
If the `tramp-methods' entry does not exist, return NIL."
  (let ((entry (assoc param (assoc method tramp-methods))))
    (when entry (cadr entry))))

;; Auto saving to a special directory.

(defun tramp-exists-file-name-handler (operation &rest args)
  "Checks whether OPERATION runs a file name handler."
  ;; The file name handler is determined on base of either an
  ;; argument, `buffer-file-name', or `default-directory'.
  (condition-case nil
      (let* ((buffer-file-name "/")
	     (default-directory "/")
	     (fnha file-name-handler-alist)
	     (check-file-name-operation operation)
	     (file-name-handler-alist
	      (list
	       (cons "/"
		     '(lambda (operation &rest args)
			"Returns OPERATION if it is the one to be checked."
			(if (equal check-file-name-operation operation)
			    operation
			  (let ((file-name-handler-alist fnha))
			    (apply operation args))))))))
	(equal (apply operation args) operation))
    (error nil)))

(unless (tramp-exists-file-name-handler 'make-auto-save-file-name)
  (defadvice make-auto-save-file-name
    (around tramp-advice-make-auto-save-file-name () activate)
    "Invoke `tramp-handle-make-auto-save-file-name' for Tramp files."
    (if (and (buffer-file-name) (tramp-tramp-file-p (buffer-file-name)))
	(setq ad-return-value (tramp-handle-make-auto-save-file-name))
      ad-do-it))
  (add-hook 'tramp-unload-hook
	    '(lambda () (ad-unadvise 'make-auto-save-file-name))))

;; In Emacs < 22 and XEmacs < 21.5 autosaved remote files have
;; permission 0666 minus umask. This is a security threat.

(defun tramp-set-auto-save-file-modes ()
  "Set permissions of autosaved remote files to the original permissions."
  (let ((bfn (buffer-file-name)))
    (when (and (stringp bfn)
	       (tramp-tramp-file-p bfn)
	       (buffer-modified-p)
	       (stringp buffer-auto-save-file-name)
	       (not (equal bfn buffer-auto-save-file-name)))
      (unless (file-exists-p buffer-auto-save-file-name)
	(write-region "" nil buffer-auto-save-file-name))
      ;; Permissions should be set always, because there might be an old
      ;; auto-saved file belonging to another original file.  This could
      ;; be a security threat.
      (set-file-modes buffer-auto-save-file-name
		      (or (file-modes bfn) (tramp-octal-to-decimal "0600"))))))

(unless (or (> emacs-major-version 21)
	    (and (featurep 'xemacs)
		 (= emacs-major-version 21)
		 (> emacs-minor-version 4)))
  (add-hook 'auto-save-hook 'tramp-set-auto-save-file-modes)
  (add-hook 'tramp-unload-hook
	    '(lambda ()
	       (remove-hook 'auto-save-hook 'tramp-set-auto-save-file-modes))))

(defun tramp-subst-strs-in-string (alist string)
  "Replace all occurrences of the string FROM with TO in STRING.
ALIST is of the form ((FROM . TO) ...)."
  (save-match-data
    (while alist
      (let* ((pr (car alist))
             (from (car pr))
             (to (cdr pr)))
        (while (string-match (regexp-quote from) string)
          (setq string (replace-match to t t string)))
        (setq alist (cdr alist))))
    string))

;; ------------------------------------------------------------
;; -- Compatibility functions section --
;; ------------------------------------------------------------

(defun tramp-read-passwd (proc &optional prompt)
  "Read a password from user (compat function).
Invokes `password-read' if available, `read-passwd' else."
  (let* ((key (tramp-make-tramp-file-name
	       tramp-current-method tramp-current-user
	       tramp-current-host ""))
	 (pw-prompt
	  (or prompt
	      (with-current-buffer (process-buffer proc)
		(tramp-check-for-regexp proc tramp-password-prompt-regexp)
		(format "%s for %s " (capitalize (match-string 1)) key)))))
    (if (functionp 'password-read)
	(let ((password (funcall (symbol-function 'password-read)
				 pw-prompt key)))
	  (funcall (symbol-function 'password-cache-add) key password)
	  password)
      (read-passwd pw-prompt))))

(defun tramp-clear-passwd (vec)
  "Clear password cache for connection related to VEC."
  (when (functionp 'password-cache-remove)
    (funcall
     (symbol-function 'password-cache-remove)
     (tramp-make-tramp-file-name
      (tramp-file-name-method vec)
      (tramp-file-name-user vec)
      (tramp-file-name-host vec)
      ""))))

;; Snarfed code from time-date.el and parse-time.el

(defconst tramp-half-a-year '(241 17024)
"Evaluated by \"(days-to-time 183)\".")

(defconst tramp-parse-time-months
  '(("jan" . 1) ("feb" . 2) ("mar" . 3)
    ("apr" . 4) ("may" . 5) ("jun" . 6)
    ("jul" . 7) ("aug" . 8) ("sep" . 9)
    ("oct" . 10) ("nov" . 11) ("dec" . 12))
  "Alist mapping month names to integers.")

(defun tramp-time-less-p (t1 t2)
  "Say whether time value T1 is less than time value T2."
  (unless t1 (setq t1 '(0 0)))
  (unless t2 (setq t2 '(0 0)))
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

(defun tramp-time-subtract (t1 t2)
  "Subtract two time values.
Return the difference in the format of a time value."
  (unless t1 (setq t1 '(0 0)))
  (unless t2 (setq t2 '(0 0)))
  (let ((borrow (< (cadr t1) (cadr t2))))
    (list (- (car t1) (car t2) (if borrow 1 0))
	  (- (+ (if borrow 65536 0) (cadr t1)) (cadr t2)))))

(defun tramp-time-diff (t1 t2)
  "Return the difference between the two times, in seconds.
T1 and T2 are time values (as returned by `current-time' for example)."
  ;; Pacify byte-compiler with `symbol-function'.
  (cond ((and (fboundp 'subtract-time)
	      (fboundp 'float-time))
         (funcall (symbol-function 'float-time)
		  (funcall (symbol-function 'subtract-time) t1 t2)))
	((and (fboundp 'subtract-time)
	      (fboundp 'time-to-seconds))
         (funcall (symbol-function 'time-to-seconds)
		  (funcall (symbol-function 'subtract-time) t1 t2)))
        ((fboundp 'itimer-time-difference)
	 (funcall (symbol-function 'itimer-time-difference)
		  (if (< (length t1) 3) (append t1 '(0)) t1)
		  (if (< (length t2) 3) (append t2 '(0)) t2)))
        (t
	 (let ((time (tramp-time-subtract t1 t2)))
	   (+ (* (car time) 65536.0)
	      (cadr time)
	      (/ (or (nth 2 time) 0) 1000000.0))))))

(defun tramp-coding-system-change-eol-conversion (coding-system eol-type)
  "Return a coding system like CODING-SYSTEM but with given EOL-TYPE.
EOL-TYPE can be one of `dos', `unix', or `mac'."
  (cond ((fboundp 'coding-system-change-eol-conversion)
         (funcall (symbol-function 'coding-system-change-eol-conversion)
		  coding-system eol-type))
        ((fboundp 'subsidiary-coding-system)
         (funcall (symbol-function 'subsidiary-coding-system)
		  coding-system
		  (cond ((eq eol-type 'dos) 'crlf)
			((eq eol-type 'unix) 'lf)
			((eq eol-type 'mac) 'cr)
			(t
			 (error "Unknown EOL-TYPE `%s', must be %s"
				eol-type
				"`dos', `unix', or `mac'")))))
        (t (error "Can't change EOL conversion -- is MULE missing?"))))

(defun tramp-split-string (string pattern)
  "Like `split-string' but omit empty strings.
In Emacs, (split-string \"/foo/bar\" \"/\") returns (\"foo\" \"bar\").
This is, the first, empty, element is omitted.  In XEmacs, the first
element is not omitted.

Note: this function has been written for `tramp-handle-file-truename'.
If you want to use it for something else, you'll have to check whether
it does the right thing."
  (delete "" (split-string string pattern)))

(defun tramp-set-process-query-on-exit-flag (process flag)
  "Specify if query is needed for process when Emacs is exited.
If the second argument flag is non-nil, Emacs will query the user before
exiting if process is running."
  (if (fboundp 'set-process-query-on-exit-flag)
      (funcall (symbol-function 'set-process-query-on-exit-flag) process flag)
    (funcall (symbol-function 'process-kill-without-query) process flag)))


;; ------------------------------------------------------------
;; -- Kludges section --
;; ------------------------------------------------------------

;; Currently (as of Emacs 20.5), the function `shell-quote-argument'
;; does not deal well with newline characters.  Newline is replaced by
;; backslash newline.  But if, say, the string `a backslash newline b'
;; is passed to a shell, the shell will expand this into "ab",
;; completely omitting the newline.  This is not what was intended.
;; It does not appear to be possible to make the function
;; `shell-quote-argument' work with newlines without making it
;; dependent on the shell used.  But within this package, we know that
;; we will always use a Bourne-like shell, so we use an approach which
;; groks newlines.
;;
;; The approach is simple: we call `shell-quote-argument', then
;; massage the newline part of the result.
;;
;; This function should produce a string which is grokked by a Unix
;; shell, even if the Emacs is running on Windows.  Since this is the
;; kludges section, we bind `system-type' in such a way that
;; `shell-quote-arguments'  behaves as if on Unix.
;;
;; Thanks to Mario DeWeerd for the hint that it is sufficient for this
;; function to work with Bourne-like shells.
;;
;; CCC: This function should be rewritten so that
;; `shell-quote-argument' is not used.  This way, we are safe from
;; changes in `shell-quote-argument'.
(defun tramp-shell-quote-argument (s)
  "Similar to `shell-quote-argument', but groks newlines.
Only works for Bourne-like shells."
  (let ((system-type 'not-windows))
    (save-match-data
      (let ((result (shell-quote-argument s))
	    (nl (regexp-quote (format "\\%s" tramp-rsh-end-of-line))))
	(when (and (>= (length result) 2)
		   (string= (substring result 0 2) "\\~"))
	  (setq result (substring result 1)))
	(while (string-match nl result)
	  (setq result (replace-match (format "'%s'" tramp-rsh-end-of-line)
				      t t result)))
	result))))

;; We currently (sometimes) use "[" and "]" in the filename format.
;; This means that Emacs wants to expand wildcards if
;; `find-file-wildcards' is non-nil, and then barfs because no
;; expansion could be found.  We detect this situation and do
;; something really awful: we have `file-expand-wildcards' return the
;; original filename if it can't expand anything.  Let's just hope
;; that this doesn't break anything else.
;; CCC: This check is now also really awful; we should search all
;; of the filename format, not just the prefix.
(when (string-match "\\[" tramp-prefix-format)
  (defadvice file-expand-wildcards
    (around tramp-advice-file-expand-wildcards activate)
    (let ((name (ad-get-arg 0)))
      (if (tramp-tramp-file-p name)
	  ;; If it's a Tramp file, dissect it and look if wildcards
	  ;; need to be expanded at all.
	  (if (string-match
	       "[[*?]"
	       (tramp-file-name-localname (tramp-dissect-file-name name)))
	      (setq ad-return-value (or ad-do-it (list name)))
	    (setq ad-return-value (list name)))
	;; If it is not a Tramp file, just run the original function.
	(setq ad-return-value (or ad-do-it (list name))))))
  (add-hook 'tramp-unload-hook
	    '(lambda () (ad-unadvise 'file-expand-wildcards))))

;; Checklist for `tramp-unload-hook'
;; - Unload all `tramp-*' packages
;; - Reset `file-name-handler-alist'
;; - Cleanup hooks where Tramp functions are in
;; - Cleanup advised functions
;; - Cleanup autoloads
;;;###autoload
(defun tramp-unload-tramp ()
  "Discard Tramp from loading remote files."
  (interactive)
  ;; When Tramp is not loaded yet, its autoloads are still active.
  (tramp-unload-file-name-handlers)
  ;; ange-ftp settings must be enabled.
  (when (functionp 'tramp-ftp-enable-ange-ftp)
    (funcall (symbol-function 'tramp-ftp-enable-ange-ftp)))
  ;; Maybe its not loaded yet.
  (condition-case nil
      (unload-feature 'tramp 'force)
    (error nil)))

(provide 'tramp)

;;; TODO:

;; * Allow putting passwords in the filename.
;;   This should be implemented via a general mechanism to add
;;   parameters in filenames.  There is currently a kludge for
;;   putting the port number into the filename for ssh and ftp
;;   files.  This could be subsumed by the new mechanism as well.
;;   Another approach is to read a netrc file like ~/.authinfo
;;   from Gnus.
;; * Handle nonlocal exits such as C-g.
;; * But it would probably be better to use with-local-quit at the
;;   place where it's actually needed: around any potentially
;;   indefinitely blocking piece of code.  In this case it would be
;;   within Tramp around one of its calls to accept-process-output (or
;;   around one of the loops that calls accept-process-output)
;;   (Stefan Monnier).
;; * Autodetect if remote `ls' groks the "--dired" switch.
;; * Rewrite `tramp-shell-quote-argument' to abstain from using
;;   `shell-quote-argument'.
;; * Completion gets confused when you leave out the method name.
;; * In Emacs 21, `insert-directory' shows total number of bytes used
;;   by the files in that directory.  Add this here.
;; * Avoid screen blanking when hitting `g' in dired.  (Eli Tziperman)
;; * Make ffap.el grok Tramp filenames.  (Eli Tziperman)
;; * When logging in, keep looking for questions according to an alist
;;   and then invoke the right function.
;; * Case-insensitive filename completion.  (Norbert Goevert.)
;; * Running CVS remotely doesn't appear to work right.  It thinks
;;   files are locked by somebody else even if I'm the locking user.
;;   Sometimes, one gets `No CVSROOT specified' errors from CVS.
;;   (Skip Montanaro)
;; * Don't use globbing for directories with many files, as this is
;;   likely to produce long command lines, and some shells choke on
;;   long command lines.
;; * `vc-directory' does not work.  It never displays any files, even
;;   if it does show files when run locally.
;; * Allow correction of passwords, if the remote end allows this.
;;   (Mark Hershberger)
;; * How to deal with MULE in `insert-file-contents' and `write-region'?
;; * Grok `append' parameter for `write-region'.
;; * Test remote ksh or bash for tilde expansion in `tramp-find-shell'?
;; * abbreviate-file-name
;; * better error checking.  At least whenever we see something
;;   strange when doing zerop, we should kill the process and start
;;   again.  (Greg Stark)
;; * Provide a local cache of old versions of remote files for the rsync
;;   transfer method to use.  (Greg Stark)
;; * Remove unneeded parameters from methods.
;; * Invoke rsync once for copying a whole directory hierarchy.
;;   (Francesco PotortÏ)
;; * Make it work for different encodings, and for different file name
;;   encodings, too.  (Daniel Pittman)
;; * Progress reports while copying files.  (Michael Kifer)
;; * Don't search for perl5 and perl.  Instead, only search for perl and
;;   then look if it's the right version (with `perl -v').
;; * When editing a remote CVS controlled file as a different user, VC
;;   gets confused about the file locking status.  Try to find out why
;;   the workaround doesn't work.
;; * Username and hostname completion.
;; ** Try to avoid usage of `last-input-event' in `tramp-completion-mode-p'.
;; ** Unify `tramp-parse-{rhosts,shosts,sconfig,hosts,passwd,netrc}'.
;;    Code is nearly identical.
;; * Allow out-of-band methods as _last_ multi-hop.  Open a connection
;;   until the last but one hop via `start-file-process'.  Apply it
;;   also for ftp and smb.
;; * WIBNI if we had a command "trampclient"?  If I was editing in
;;   some shell with root priviledges, it would be nice if I could
;;   just call
;;     trampclient filename.c
;;   as an editor, and the _current_ shell would connect to an Emacs
;;   server and would be used in an existing non-priviledged Emacs
;;   session for doing the editing in question.
;;   That way, I need not tell Emacs my password again and be afraid
;;   that it makes it into core dumps or other ugly stuff (I had Emacs
;;   once display a just typed password in the context of a keyboard
;;   sequence prompt for a question immediately following in a shell
;;   script run within Emacs -- nasty).
;;   And if I have some ssh session running to a different computer,
;;   having the possibility of passing a local file there to a local
;;   Emacs session (in case I can arrange for a connection back) would
;;   be nice.
;;   Likely the corresponding Tramp server should not allow the
;;   equivalent of the emacsclient -eval option in order to make this
;;   reasonably unproblematic.  And maybe trampclient should have some
;;   way of passing credentials, like by using an SSL socket or
;;   something. (David Kastrup)
;; * Could Tramp reasonably look for a prompt after ^M rather than
;;   only after ^J ? (Stefan Monnier)
;; * Reconnect directly to a compliant shell without first going
;;   through the user's default shell. (Pete Forman)
;; * Make `tramp-default-user' obsolete.
;; * Tramp shall reconnect automatically to its ssh connection when it
;;   detects that the process "has died". (David Reitter)

;; Functions for file-name-handler-alist:
;; diff-latest-backup-file -- in diff.el
;; dired-uncache -- this will be needed when we do insert-directory caching
;; file-name-as-directory -- use primitive?
;; file-name-sans-versions -- use primitive?
;; get-file-buffer -- use primitive
;; vc-registered

;;; arch-tag: 3a21a994-182b-48fa-b0cd-c1d9fede424a
;;; tramp.el ends here
