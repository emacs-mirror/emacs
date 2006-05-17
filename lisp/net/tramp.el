;;; -*- mode: Emacs-Lisp; coding: iso-2022-7bit; -*-
;;; tramp.el --- Transparent Remote Access, Multiple Protocol

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Kai Gro,A_(Bjohann <kai.grossjohann@gmx.net>
;;         Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes

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
;; This package only works for Emacs 20 and higher, and for XEmacs 21
;; and higher.  (XEmacs 20 is missing the `with-timeout' macro.  Emacs
;; 19 is reported to have other problems.  For XEmacs 21, you need the
;; package `fsf-compat' for the `with-timeout' macro.)
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

(require 'timer)
(require 'format-spec)                  ;from Gnus 5.8, also in tar ball
;; As long as password.el is not part of (X)Emacs, it shouldn't
;; be mandatory
(if (featurep 'xemacs)
    (load "password" 'noerror)
  (require 'password nil 'noerror))     ;from No Gnus, also in tar ball

;; The explicit check is not necessary in Emacs, which provides the
;; feature even if implemented in C, but it appears to be necessary
;; in XEmacs.
(unless (and (fboundp 'base64-encode-region)
	     (fboundp 'base64-decode-region))
  (require 'base64))                       ;for the mimencode methods
(require 'shell)
(require 'advice)

(autoload 'tramp-uuencode-region "tramp-uu"
  "Implementation of `uuencode' in Lisp.")

(unless (fboundp 'uudecode-decode-region)
  (autoload 'uudecode-decode-region "uudecode"))

;; XEmacs is distributed with few Lisp packages.  Further packages are
;; installed using EFS.  If we use a unified filename format, then
;; Tramp is required in addition to EFS.  (But why can't Tramp just
;; disable EFS when Tramp is loaded?  Then XEmacs can ship with EFS
;; just like before.)  Another reason for using a separate filename
;; syntax on XEmacs is that EFS hooks into XEmacs in many places, but
;; Tramp only knows how to deal with `file-name-handler-alist', not
;; the other places.
;;;###autoload
(defvar tramp-unified-filenames (not (featurep 'xemacs))
  "Non-nil means to use unified Ange-FTP/Tramp filename syntax.
Nil means to use a separate filename syntax for Tramp.")

;; Load foreign methods.  Because they do require Tramp internally, this
;; must be done with the `eval-after-load' trick.

;; tramp-ftp supports Ange-FTP only.  Not suited for XEmacs therefore.
(unless (featurep 'xemacs)
  (eval-after-load "tramp"
    '(require 'tramp-ftp)))
(when (and tramp-unified-filenames (featurep 'xemacs))
  (eval-after-load "tramp"
    '(require 'tramp-efs)))

;; tramp-smb uses "smbclient" from Samba.
;; Not available under Cygwin and Windows, because they don't offer
;; "smbclient".  And even not necessary there, because Emacs supports
;; UNC file names like "//host/share/localname".
(unless (memq system-type '(cygwin windows-nt))
  (eval-after-load "tramp"
    '(require 'tramp-smb)))

(eval-when-compile
  (require 'cl)
  (require 'custom)
  ;; Emacs 19.34 compatibility hack -- is this needed?
  (or (>= emacs-major-version 20)
      (load "cl-seq")))

(unless (boundp 'custom-print-functions)
  (defvar custom-print-functions nil))	; not autoloaded before Emacs 20.4

;; Avoid byte-compiler warnings if the byte-compiler supports this.
;; Currently, XEmacs supports this.
(eval-when-compile
  (when (featurep 'xemacs)
    (let (unused-vars) ; Pacify Emacs byte-compiler
      (defalias 'warnings 'identity) ; Pacify Emacs byte-compiler
      (byte-compiler-options (warnings (- unused-vars))))))

;; `directory-sep-char' is an obsolete variable in Emacs.  But it is
;; used in XEmacs, so we set it here and there.  The following is needed
;; to pacify Emacs byte-compiler.
(eval-when-compile
  (when (boundp 'byte-compile-not-obsolete-var)
    (setq byte-compile-not-obsolete-var 'directory-sep-char)))

;;; User Customizable Internal Variables:

(defgroup tramp nil
  "Edit remote files with a combination of rsh and rcp or similar programs."
  :group 'files
  :version "22.1")

(defcustom tramp-verbose 9
  "*Verbosity level for tramp.el.  0 means be silent, 10 is most verbose."
  :group 'tramp
  :type 'integer)

(defcustom tramp-debug-buffer nil
  "*Whether to send all commands and responses to a debug buffer."
  :group 'tramp
  :type 'boolean)

;; Emacs case
(eval-and-compile
  (when (boundp 'backup-directory-alist)
    (defcustom tramp-backup-directory-alist nil
      "Alist of filename patterns and backup directory names.
Each element looks like (REGEXP . DIRECTORY), with the same meaning like
in `backup-directory-alist'.  If a Tramp file is backed up, and DIRECTORY
is a local file name, the backup directory is prepended with Tramp file
name prefix \(multi-method, method, user, host\) of file.

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
\(multi-method, method, user, host\) of file.

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
  :type '(choice (const nil)
                 string))

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
variable `tramp-encoding-command-switch' for the \"-c\" part.  Also, see the
variable `tramp-encoding-reads-stdin' to specify whether the commands read
standard input or a file.

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

(defcustom tramp-encoding-reads-stdin t
  "*If non-nil, encoding commands read from standard input.
If nil, the filename is the last argument.

Note that the commands always must write to standard output."
  :group 'tramp
  :type 'boolean)

(defcustom tramp-multi-sh-program
  tramp-encoding-shell
  "*Use this program for bootstrapping multi-hop connections.
This variable is similar to `tramp-encoding-shell', but it is only used
when initializing a multi-hop connection.  Therefore, the set of
commands sent to this shell is quite restricted, and if you are
careful it works to use CMD.EXE under Windows (instead of a Bourne-ish
shell which does not normally exist on Windows anyway).

To use multi-hop methods from Windows, you also need suitable entries
in `tramp-multi-connection-function-alist' for the first hop.

This variable defaults to the value of `tramp-encoding-shell'."
  :group 'tramp
  :type '(file :must-match t))

;; CCC I have changed all occurrences of comint-quote-filename with
;; tramp-shell-quote-argument, except in tramp-handle-expand-many-files.
;; There, comint-quote-filename was removed altogether.  If it turns
;; out to be necessary there, something will need to be done.
;;-(defcustom tramp-file-name-quote-list
;;-  '(?] ?[ ?\| ?& ?< ?> ?\( ?\) ?\; ?\  ?\* ?\? ?\! ?\" ?\' ?\` ?# ?\@ ?\+ )
;;-  "*Protect these characters from the remote shell.
;;-Any character in this list is quoted (preceded with a backslash)
;;-because it means something special to the shell.  This takes effect
;;-when sending file and directory names to the remote shell.
;;-
;;-See `comint-file-name-quote-list' for details."
;;-  :group 'tramp
;;-  :type '(repeat character))

(defcustom tramp-methods
  '( ("rcp"   (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-login-program        "rsh")
              (tramp-copy-program         "rcp")
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           nil)
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line nil))
     ("scp"   (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-login-program        "ssh")
              (tramp-copy-program         "scp")
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           ("-e" "none"))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line nil))
     ("scp1"  (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-login-program        "ssh")
              (tramp-copy-program         "scp")
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           ("-1" "-e" "none"))
              (tramp-copy-args            ("-1"))
              (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line nil))
     ("scp2"  (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-login-program        "ssh")
              (tramp-copy-program         "scp")
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           ("-2" "-e" "none"))
              (tramp-copy-args            ("-2"))
              (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line nil))
     ("scp1_old"
              (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-login-program        "ssh1")
              (tramp-copy-program         "scp1")
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           ("-e" "none"))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line nil))
     ("scp2_old"
              (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-login-program        "ssh2")
              (tramp-copy-program         "scp2")
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           ("-e" "none"))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line nil))
     ("rsync" (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-login-program        "ssh")
              (tramp-copy-program         "rsync")
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           ("-e" "none"))
              (tramp-copy-args            ("-e" "ssh"))
              (tramp-copy-keep-date-arg   "-t")
	      (tramp-password-end-of-line nil))
     ("remcp" (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-login-program        "remsh")
              (tramp-copy-program         "rcp")
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           nil)
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line nil))
     ("rsh"   (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-login-program        "rsh")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           nil)
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("ssh"   (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-login-program        "ssh")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           ("-e" "none"))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("ssh1"  (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-login-program        "ssh")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           ("-1" "-e" "none"))
              (tramp-copy-args            ("-1"))
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("ssh2"  (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-login-program        "ssh")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           ("-2" "-e" "none"))
              (tramp-copy-args            ("-2"))
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("ssh1_old"
              (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-login-program        "ssh1")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           ("-e" "none"))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("ssh2_old"
              (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-login-program        "ssh2")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           ("-e" "none"))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("remsh" (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-login-program        "remsh")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           nil)
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("telnet"
              (tramp-connection-function  tramp-open-connection-telnet)
              (tramp-login-program        "telnet")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           nil)
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("su"    (tramp-connection-function  tramp-open-connection-su)
              (tramp-login-program        "su")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           ("-" "%u"))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("sudo"  (tramp-connection-function  tramp-open-connection-su)
              (tramp-login-program        "sudo")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           ("-u" "%u" "-s"
					   "-p" "Password:"))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("multi" (tramp-connection-function  tramp-open-connection-multi)
              (tramp-login-program        nil)
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           nil)
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("scpx"  (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-login-program        "ssh")
              (tramp-copy-program         "scp")
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           ("-e" "none" "-t" "-t" "/bin/sh"))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line nil))
     ("sshx"  (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-login-program        "ssh")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           ("-e" "none" "-t" "-t" "/bin/sh"))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("krlogin"
              (tramp-connection-function  tramp-open-connection-rsh)
	      (tramp-login-program        "krlogin")
	      (tramp-copy-program         nil)
	      (tramp-remote-sh            "/bin/sh")
	      (tramp-login-args           ("-x"))
	      (tramp-copy-args            nil)
	      (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("plink"
              (tramp-connection-function  tramp-open-connection-rsh)
	      (tramp-login-program        "plink")
	      (tramp-copy-program         nil)
	      (tramp-remote-sh            "/bin/sh")
	      (tramp-login-args           ("-ssh")) ;optionally add "-v"
	      (tramp-copy-args            nil)
	      (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line "xy")) ;see docstring for "xy"
     ("plink1"
              (tramp-connection-function  tramp-open-connection-rsh)
	      (tramp-login-program        "plink")
	      (tramp-copy-program         nil)
	      (tramp-remote-sh            "/bin/sh")
	      (tramp-login-args           ("-1" "-ssh")) ;optionally add "-v"
	      (tramp-copy-args            nil)
	      (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line "xy")) ;see docstring for "xy"
     ("pscp"
              (tramp-connection-function  tramp-open-connection-rsh)
	      (tramp-login-program        "plink")
	      (tramp-copy-program         "pscp")
	      (tramp-remote-sh            "/bin/sh")
	      (tramp-login-args           ("-ssh"))
	      (tramp-copy-args            nil)
	      (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line "xy")) ;see docstring for "xy"
     ("fcp"
	      (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-login-program        "fsh")
              (tramp-copy-program         "fcp")
              (tramp-remote-sh            "/bin/sh -i")
              (tramp-login-args           ("sh" "-i"))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line nil))
     )
  "*Alist of methods for remote files.
This is a list of entries of the form (NAME PARAM1 PARAM2 ...).
Each NAME stands for a remote access method.  Each PARAM is a
pair of the form (KEY VALUE).  The following KEYs are defined:
 * `tramp-connection-function'
    This specifies the function to use to connect to the remote host.
    Currently, `tramp-open-connection-rsh', `tramp-open-connection-telnet'
    and `tramp-open-connection-su' are defined.  See the documentation
    of these functions for more details.
  * `tramp-remote-sh'
    This specifies the Bourne shell to use on the remote host.  This
    MUST be a Bourne-like shell.  It is normally not necessary to set
    this to any value other than \"/bin/sh\": tramp wants to use a shell
    which groks tilde expansion, but it can search for it.  Also note
    that \"/bin/sh\" exists on all Unixen, this might not be true for
    the value that you decide to use.  You Have Been Warned.
  * `tramp-login-program'
    This specifies the name of the program to use for logging in to the
    remote host.  Depending on `tramp-connection-function', this may be
    the name of rsh or a workalike program (when
    `tramp-connection-function' is `tramp-open-connection-rsh'), or the
    name of telnet or a workalike (for `tramp-open-connection-telnet'),
    or the name of su or a workalike (for `tramp-open-connection-su').
  * `tramp-login-args'
    This specifies the list of arguments to pass to the above
    mentioned program.  Please note that this is a list of arguments,
    that is, normally you don't want to put \"-a -b\" or \"-f foo\"
    here.  Instead, you want two list elements, one for \"-a\" and one
    for \"-b\", or one for \"-f\" and one for \"foo\".
    If `tramp-connection-function' is `tramp-open-connection-su', then
    \"%u\" in this list is replaced by the user name, and \"%%\" can
    be used to obtain a literal percent character.
  * `tramp-copy-program'
    This specifies the name of the program to use for remotely copying
    the file; this might be the absolute filename of rcp or the name of
    a workalike program.
  * `tramp-copy-args'
    This specifies the list of parameters to pass to the above mentioned
    program, the hints for `tramp-login-args' also apply here.
  * `tramp-copy-keep-date-arg'
    This specifies the parameter to use for the copying program when the
    timestamp of the original file should be kept.  For `rcp', use `-p', for
    `rsync', use `-t'.
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
See the variable `tramp-coding-commands' for details.

So, to summarize: if the method is an out-of-band method, then you
must specify `tramp-copy-program' and `tramp-copy-args'.  If it is an
inline method, then these two parameters should be nil.  Every method,
inline or out of band, must specify `tramp-connection-function' plus
the associated arguments (for example, the login program if you chose
`tramp-open-connection-telnet').

Notes:

When using `tramp-open-connection-su' the phrase `open connection to a
remote host' sounds strange, but it is used nevertheless, for
consistency.  No connection is opened to a remote host, but `su' is
started on the local host.  You are not allowed to specify a remote
host other than `localhost' or the name of the local host."
  :group 'tramp
  :type '(repeat
          (cons string
                (set (list (const tramp-connection-function) function)
                     (list (const tramp-login-program)
			   (choice (const nil) string))
                     (list (const tramp-copy-program)
			   (choice (const nil) string))
                     (list (const tramp-remote-sh)
			   (choice (const nil) string))
                     (list (const tramp-login-args) (repeat string))
                     (list (const tramp-copy-args) (repeat string))
                     (list (const tramp-copy-keep-date-arg)
			   (choice (const nil) string))
                     (list (const tramp-encoding-command)
			   (choice (const nil) string))
                     (list (const tramp-decoding-command)
			   (choice (const nil) string))
                     (list (const tramp-encoding-function)
			   (choice (const nil) function))
                     (list (const tramp-decoding-function)
			   (choice (const nil) function))
		     (list (const tramp-password-end-of-line)
			   (choice (const nil) string))))))

(defcustom tramp-multi-methods '("multi" "multiu")
  "*List of multi-hop methods.
Each entry in this list should be a method name as mentioned in the
variable `tramp-methods'."
  :group 'tramp
  :type '(repeat string))

(defcustom tramp-multi-connection-function-alist
  '(("telnet" tramp-multi-connect-telnet "telnet %h%n")
    ("rsh"    tramp-multi-connect-rlogin "rsh %h -l %u%n")
    ("remsh"  tramp-multi-connect-rlogin "remsh %h -l %u%n")
    ("ssh"    tramp-multi-connect-rlogin "ssh %h -l %u%n")
    ("ssht"   tramp-multi-connect-rlogin "ssh %h -e none -t -t -l %u%n")
    ("su"     tramp-multi-connect-su     "su - %u%n")
    ("sudo"   tramp-multi-connect-su     "sudo -u %u -s -p Password:%n"))
  "*List of connection functions for multi-hop methods.
Each list item is a list of three items (METHOD FUNCTION COMMAND),
where METHOD is the name as used in the file name, FUNCTION is the
function to be executed, and COMMAND is the shell command used for
connecting.

COMMAND may contain percent escapes.  `%u' will be replaced with the
user name, `%h' will be replaced with the host name, and `%n' will be
replaced with an end-of-line character, as specified in the variable
`tramp-rsh-end-of-line'.  Use `%%' for a literal percent character.
Note that the interpretation of the percent escapes also depends on
the FUNCTION.  For example, the `%u' escape is forbidden with the
function `tramp-multi-connect-telnet'.  See the documentation of the
various functions for details."
  :group 'tramp
  :type '(repeat (list string function string)))

(defcustom tramp-default-method
  (if (and (fboundp 'executable-find)
	   (executable-find "plink"))
      "plink"
    "ssh")
  "*Default method to use for transferring files.
See `tramp-methods' for possibilities.
Also see `tramp-default-method-alist'."
  :group 'tramp
  :type 'string)

(defcustom tramp-default-method-alist
  '(("\\`localhost\\'" "\\`root\\'" "su"))
  "*Default method to use for specific user/host pairs.
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

;; Default values for non-Unices seeked
(defconst tramp-completion-function-alist-rsh
  (unless (memq system-type '(windows-nt))
    '((tramp-parse-rhosts "/etc/hosts.equiv")
      (tramp-parse-rhosts "~/.rhosts")))
  "Default list of (FUNCTION FILE) pairs to be examined for rsh methods.")

;; Default values for non-Unices seeked
(defconst tramp-completion-function-alist-ssh
  (unless (memq system-type '(windows-nt))
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
      (tramp-parse-sknownhosts "~/.ssh2/knownhosts")))
  "Default list of (FUNCTION FILE) pairs to be examined for ssh methods.")

;; Default values for non-Unices seeked
(defconst tramp-completion-function-alist-telnet
  (unless (memq system-type '(windows-nt))
    '((tramp-parse-hosts "/etc/hosts")))
  "Default list of (FUNCTION FILE) pairs to be examined for telnet methods.")

;; Default values for non-Unices seeked
(defconst tramp-completion-function-alist-su
  (unless (memq system-type '(windows-nt))
    '((tramp-parse-passwd "/etc/passwd")))
  "Default list of (FUNCTION FILE) pairs to be examined for su methods.")

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
      "multi" nil)
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
      "pscp" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "fcp" tramp-completion-function-alist-ssh)))

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

(defcustom tramp-remote-path
  '("/bin" "/usr/bin" "/usr/sbin" "/usr/local/bin" "/usr/ccs/bin"
    "/local/bin" "/local/freeware/bin" "/local/gnu/bin"
    "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin")
  "*List of directories to search for executables on remote host.
Please notify me about other semi-standard directories to include here.

You can use `~' in this list, but when searching for a shell which groks
tilde expansion, all directory names starting with `~' will be ignored."
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
  "^.*\\([pP]assword\\|passphrase\\).*:\^@? *"
  "*Regexp matching password-like prompts.
The regexp should match at end of buffer.

The `sudo' program appears to insert a `^@' character into the prompt."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-wrong-passwd-regexp
  (concat "^.*"
	  ;; These strings should be on the last line
	  (regexp-opt '("Permission denied."
			"Login incorrect"
			"Login Incorrect"
			"Connection refused"
			"Connection closed"
			"Sorry, try again."
			"Name or service not known"
			"Host key verification failed.") t)
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
  (concat (regexp-opt '("Store key in cache? (y/n)") t)
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

(defcustom tramp-process-alive-regexp
  ""
  "Regular expression indicating a process has finished.
In fact this expression is empty by intention, it will be used only to
check regularly the status of the associated process.
The answer will be provided by `tramp-action-process-alive',
`tramp-multi-action-process-alive' and`tramp-action-out-of-band', which see."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-temp-name-prefix "tramp."
  "*Prefix to use for temporary files.
If this is a relative file name (such as \"tramp.\"), it is considered
relative to the directory name returned by the function
`tramp-temporary-file-directory' (which see).  It may also be an
absolute file name; don't forget to include a prefix for the filename
part, though."
  :group 'tramp
  :type 'string)

(defcustom tramp-discard-garbage nil
  "*If non-nil, try to discard garbage sent by remote shell.
Some shells send such garbage upon connection setup."
  :group 'tramp
  :type 'boolean)

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

(defcustom tramp-prefix-format
  (if tramp-unified-filenames "/" "/[")
  "*String matching the very beginning of tramp file names.
Used in `tramp-make-tramp-file-name' and `tramp-make-tramp-multi-file-name'."
  :group 'tramp
  :type 'string)

(defcustom tramp-prefix-regexp
  (concat "^" (regexp-quote tramp-prefix-format))
  "*Regexp matching the very beginning of tramp file names.
Should always start with \"^\". Derived from `tramp-prefix-format'."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-method-regexp
  "[a-zA-Z_0-9-]+"
  "*Regexp matching methods identifiers."
  :group 'tramp
  :type 'regexp)

;; It is a little bit annoying that in XEmacs case this delimeter is different
;; for single-hop and multi-hop cases.
(defcustom tramp-postfix-single-method-format
  (if tramp-unified-filenames ":" "/")
  "*String matching delimeter between method and user or host names.
Applicable for single-hop methods.
Used in `tramp-make-tramp-file-name'."
  :group 'tramp
  :type 'string)

(defcustom tramp-postfix-single-method-regexp
  (regexp-quote tramp-postfix-single-method-format)
  "*Regexp matching delimeter between method and user or host names.
Applicable for single-hop methods.
Derived from `tramp-postfix-single-method-format'."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-postfix-multi-method-format
  ":"
  "*String matching delimeter between method and user or host names.
Applicable for multi-hop methods.
Used in `tramp-make-tramp-multi-file-name'."
  :group 'tramp
  :type 'string)

(defcustom tramp-postfix-multi-method-regexp
  (regexp-quote tramp-postfix-multi-method-format)
  "*Regexp matching delimeter between method and user or host names.
Applicable for multi-hop methods.
Derived from `tramp-postfix-multi-method-format'."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-postfix-multi-hop-format
  (if tramp-unified-filenames ":" "/")
  "*String matching delimeter between host and next method.
Applicable for multi-hop methods.
Used in `tramp-make-tramp-multi-file-name'."
  :group 'tramp
  :type 'string)

(defcustom tramp-postfix-multi-hop-regexp
  (regexp-quote tramp-postfix-multi-hop-format)
  "*Regexp matching delimeter between host and next method.
Applicable for multi-hop methods.
Derived from `tramp-postfix-multi-hop-format'."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-user-regexp
  "[^:/ \t]*"
  "*Regexp matching user names."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-postfix-user-format
  "@"
  "*String matching delimeter between user and host names.
Used in `tramp-make-tramp-file-name' and `tramp-make-tramp-multi-file-name'."
  :group 'tramp
  :type 'string)

(defcustom tramp-postfix-user-regexp
  (regexp-quote tramp-postfix-user-format)
  "*Regexp matching delimeter between user and host names.
Derived from `tramp-postfix-user-format'."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-host-regexp
  "[a-zA-Z0-9_.-]*"
  "*Regexp matching host names."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-host-with-port-regexp
  "[a-zA-Z0-9_.#-]*"
  "*Regexp matching host names."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-postfix-host-format
  (if tramp-unified-filenames ":" "]")
  "*String matching delimeter between host names and localnames.
Used in `tramp-make-tramp-file-name' and `tramp-make-tramp-multi-file-name'."
  :group 'tramp
  :type 'string)

(defcustom tramp-postfix-host-regexp
  (regexp-quote tramp-postfix-host-format)
  "*Regexp matching delimeter between host names and localnames.
Derived from `tramp-postfix-host-format'."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-localname-regexp
  ".*$"
  "*Regexp matching localnames."
  :group 'tramp
  :type 'regexp)

;; File name format.

(defcustom tramp-file-name-structure
  (list
   (concat
    tramp-prefix-regexp
    "\\(" "\\(" tramp-method-regexp "\\)" tramp-postfix-single-method-regexp "\\)?"
    "\\(" "\\(" tramp-user-regexp "\\)" tramp-postfix-user-regexp   "\\)?"
          "\\(" tramp-host-with-port-regexp "\\)" tramp-postfix-host-regexp
	  "\\(" tramp-localname-regexp "\\)")
   2 4 5 6)

  "*List of five elements (REGEXP METHOD USER HOST FILE), detailing \
the tramp file name structure.

The first element REGEXP is a regular expression matching a tramp file
name.  The regex should contain parentheses around the method name,
the user name, the host name, and the file name parts.

The second element METHOD is a number, saying which pair of
parentheses matches the method name.  The third element USER is
similar, but for the user name.  The fourth element HOST is similar,
but for the host name.  The fifth element FILE is for the file name.
These numbers are passed directly to `match-string', which see.  That
means the opening parentheses are counted to identify the pair.

See also `tramp-file-name-regexp'."
  :group 'tramp
  :type '(list (regexp :tag "File name regexp")
               (integer :tag "Paren pair for method name")
               (integer :tag "Paren pair for user name  ")
               (integer :tag "Paren pair for host name  ")
               (integer :tag "Paren pair for file name  ")))

;;;###autoload
(defconst tramp-file-name-regexp-unified
  "\\`/[^/:]+:"
  "Value for `tramp-file-name-regexp' for unified remoting.
Emacs (not XEmacs) uses a unified filename syntax for Ange-FTP and
Tramp.  See `tramp-file-name-structure-unified' for more explanations.")

;;;###autoload
(defconst tramp-file-name-regexp-separate
  "\\`/\\[.*\\]"
  "Value for `tramp-file-name-regexp' for separate remoting.
XEmacs uses a separate filename syntax for Tramp and EFS.
See `tramp-file-name-structure-separate' for more explanations.")

;;;###autoload
(defcustom tramp-file-name-regexp
  (if tramp-unified-filenames
      tramp-file-name-regexp-unified
    tramp-file-name-regexp-separate)
  "*Regular expression matching file names handled by tramp.
This regexp should match tramp file names but no other file names.
\(When tramp.el is loaded, this regular expression is prepended to
`file-name-handler-alist', and that is searched sequentially.  Thus,
if the tramp entry appears rather early in the `file-name-handler-alist'
and is a bit too general, then some files might be considered tramp
files which are not really tramp files.

Please note that the entry in `file-name-handler-alist' is made when
this file (tramp.el) is loaded.  This means that this variable must be set
before loading tramp.el.  Alternatively, `file-name-handler-alist' can be
updated after changing this variable.

Also see `tramp-file-name-structure'."
  :group 'tramp
  :type 'regexp)

;;;###autoload
(defconst tramp-completion-file-name-regexp-unified
  "^/$\\|^/[^/:][^/]*$"
  "Value for `tramp-completion-file-name-regexp' for unified remoting.
Emacs (not XEmacs) uses a unified filename syntax for Ange-FTP and
Tramp.  See `tramp-file-name-structure-unified' for more explanations.")

;;;###autoload
(defconst tramp-completion-file-name-regexp-separate
  "^/\\([[][^]]*\\)?$"
  "Value for `tramp-completion-file-name-regexp' for separate remoting.
XEmacs uses a separate filename syntax for Tramp and EFS.
See `tramp-file-name-structure-separate' for more explanations.")

;;;###autoload
(defcustom tramp-completion-file-name-regexp
  (if tramp-unified-filenames
      tramp-completion-file-name-regexp-unified
    tramp-completion-file-name-regexp-separate)
  "*Regular expression matching file names handled by tramp completion.
This regexp should match partial tramp file names only.

Please note that the entry in `file-name-handler-alist' is made when
this file (tramp.el) is loaded.  This means that this variable must be set
before loading tramp.el.  Alternatively, `file-name-handler-alist' can be
updated after changing this variable.

Also see `tramp-file-name-structure'."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-multi-file-name-structure
  (list
   (concat
    tramp-prefix-regexp
    "\\(" "\\(" tramp-method-regexp "\\)" "\\)?"
    "\\(" "\\(" tramp-postfix-multi-hop-regexp "%s" "\\)+" "\\)?"
    tramp-postfix-host-regexp "\\(" tramp-localname-regexp "\\)")
   2 3 -1)
  "*Describes the file name structure of `multi' files.
Multi files allow you to contact a remote host in several hops.
This is a list of four elements (REGEXP METHOD HOP LOCALNAME).

The first element, REGEXP, gives a regular expression to match against
the file name.  In this regular expression, `%s' is replaced with the
value of `tramp-multi-file-name-hop-structure'.  (Note: in order to
allow multiple hops, you normally want to use something like
\"\\\\(\\\\(%s\\\\)+\\\\)\" in the regular expression.  The outer pair
of parentheses is used for the HOP element, see below.)

All remaining elements are numbers.  METHOD gives the number of the
paren pair which matches the method name.  HOP gives the number of the
paren pair which matches the hop sequence.  LOCALNAME gives the number of
the paren pair which matches the localname (pathname) on the remote host.

LOCALNAME can also be negative, which means to count from the end.  Ie, a
value of -1 means the last paren pair.

I think it would be good if the regexp matches the whole of the
string, but I haven't actually tried what happens if it doesn't..."
  :group 'tramp
  :type '(list (regexp :tag "File name regexp")
               (integer :tag "Paren pair for method name")
               (integer :tag "Paren pair for hops")
               (integer :tag "Paren pair to match localname")))

(defcustom tramp-multi-file-name-hop-structure
  (list
   (concat
    "\\(" tramp-method-regexp "\\)" tramp-postfix-multi-method-regexp
    "\\(" tramp-user-regexp "\\)" tramp-postfix-user-regexp
    "\\(" tramp-host-with-port-regexp "\\)")
   1 2 3)
  "*Describes the structure of a hop in multi files.
This is a list of four elements (REGEXP METHOD USER HOST).  First
element REGEXP is used to match against the hop.  Pair number METHOD
matches the method of one hop, pair number USER matches the user of
one hop, pair number HOST matches the host of one hop.

This regular expression should match exactly all of one hop."
  :group 'tramp
  :type '(list (regexp :tag "Hop regexp")
               (integer :tag "Paren pair for method name")
               (integer :tag "Paren pair for user name")
               (integer :tag "Paren pair for host name")))

(defcustom tramp-make-multi-tramp-file-format
  (list
   (concat tramp-prefix-format "%m")
   (concat tramp-postfix-multi-hop-format
    "%m" tramp-postfix-multi-method-format
    "%u" tramp-postfix-user-format
    "%h")
   (concat tramp-postfix-host-format "%p"))
  "*Describes how to construct a `multi' file name.
This is a list of three elements PREFIX, HOP and LOCALNAME.

The first element PREFIX says how to construct the prefix, the second
element HOP specifies what each hop looks like, and the final element
LOCALNAME says how to construct the localname (pathname).

In PREFIX, `%%' means `%' and `%m' means the method name.

In HOP, `%%' means `%' and `%m', `%u', `%h' mean the hop method, hop
user and hop host, respectively.

In LOCALNAME, `%%' means `%' and `%p' means the localname.

The resulting file name always contains one copy of PREFIX and one
copy of LOCALNAME, but there is one copy of HOP for each hop in the file
name.

Note: the current implementation requires the prefix to contain the
method name, followed by all the hops, and the localname must come
last."
  :group 'tramp
  :type '(list string string string))

(defcustom tramp-terminal-type "dumb"
  "*Value of TERM environment variable for logging in to remote host.
Because Tramp wants to parse the output of the remote shell, it is easily
confused by ANSI color escape sequences and suchlike.  Often, shell init
files conditionalize this setup based on the TERM environment variable."
  :group 'tramp
  :type 'string)

(defcustom tramp-completion-without-shell-p nil
  "*If nil, use shell wildcards for completion, else rely on Lisp only.
Using shell wildcards for completions has the advantage that it can be
fast even in large directories, but completion is always
case-sensitive.  Relying on Lisp only means that case-insensitive
completion is possible (subject to the variable `completion-ignore-case'),
but it might be slow on large directories."
  :group 'tramp
  :type 'boolean)

(defcustom tramp-actions-before-shell
  '((tramp-password-prompt-regexp tramp-action-password)
    (tramp-login-prompt-regexp tramp-action-login)
    (shell-prompt-pattern tramp-action-succeed)
    (tramp-shell-prompt-pattern tramp-action-succeed)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
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
corresponding PATTERN matches, the ACTION function is called."
  :group 'tramp
  :type '(repeat (list variable function)))

(defcustom tramp-actions-copy-out-of-band
  '((tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (tramp-process-alive-regexp tramp-action-out-of-band))
  "List of pattern/action pairs.
This list is used for copying/renaming with out-of-band methods.
See `tramp-actions-before-shell' for more info."
  :group 'tramp
  :type '(repeat (list variable function)))

(defcustom tramp-multi-actions
  '((tramp-password-prompt-regexp tramp-multi-action-password)
    (tramp-login-prompt-regexp tramp-multi-action-login)
    (shell-prompt-pattern tramp-multi-action-succeed)
    (tramp-shell-prompt-pattern tramp-multi-action-succeed)
    (tramp-wrong-passwd-regexp tramp-multi-action-permission-denied)
    (tramp-process-alive-regexp tramp-multi-action-process-alive))
  "List of pattern/action pairs.
This list is used for each hop in multi-hop connections.
See `tramp-actions-before-shell' for more info."
  :group 'tramp
  :type '(repeat (list variable function)))

(defcustom tramp-initial-commands
  '("unset HISTORY"
    "unset correct"
    "unset autocorrect")
  "List of commands to send to the first remote shell that we see.
These commands will be sent to any shell, and thus they should be
designed to work in such circumstances.  Also, restrict the commands
to the bare necessity for getting the remote shell into a state
where it is possible to execute the Bourne-ish shell.

At the moment, the command to execute the Bourne-ish shell uses strange
quoting which `tcsh' tries to correct, so we send the command \"unset
autocorrect\" to the remote host."
  :group 'tramp
  :type '(repeat string))

;; Chunked sending kluge.  We set this to 500 for black-listed constellations
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

If your Emacs is buggy, the code stops and gives you an indication
about the value `tramp-chunksize' should be set.  Maybe you could just
experiment a bit, e.g. changing the values of `init' and `step'
in the third line of the code.

When it is necessary to set `tramp-chunksize', you might consider to
use an out-of-the-band method (like \"scp\") instead of an internal one
\(like \"ssh\"), because setting `tramp-chunksize' to non-nil decreases
performance.

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

;;; Internal Variables:

(defvar tramp-buffer-file-attributes nil
  "Holds the `ls -ild' output for the current buffer.
This variable is local to each buffer.  It is not used if the remote
machine groks Perl.  If it is used, it's used as an emulation for
the visited file modtime.")
(make-variable-buffer-local 'tramp-buffer-file-attributes)

(defvar tramp-md5-function
  (cond ((and (require 'md5) (fboundp 'md5)) 'md5)
	((fboundp 'md5-encode)
	 (lambda (x) (base64-encode-string
		      (funcall (symbol-function 'md5-encode) x))))
	(t (error "Coulnd't find an `md5' function")))
  "Function to call for running the MD5 algorithm.")

(defvar tramp-end-of-output
  (concat "///"
	  (funcall tramp-md5-function
		   (concat
		    (prin1-to-string process-environment)
		    (current-time-string)
;; 		    (prin1-to-string
;; 		     (if (fboundp 'directory-files-and-attributes)
;; 			 (funcall 'directory-files-and-attributes
;; 				  (or (getenv "HOME")
;; 				      (tramp-temporary-file-directory)))
;; 		       (mapcar
;; 			(lambda (x)
;; 			  (cons x (file-attributes x)))
;; 			(directory-files (or (getenv "HOME")
;; 					     (tramp-temporary-file-directory))
;; 					 t))))
		    )))
  "String used to recognize end of output.")

(defvar tramp-connection-function nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-remote-sh nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-login-program nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-login-args nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-copy-program nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-copy-args nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-copy-keep-date-arg nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-encoding-command nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-decoding-command nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-encoding-function nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-decoding-function nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-password-end-of-line nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

;; CCC `local in each buffer'?
(defvar tramp-ls-command nil
  "This command is used to get a long listing with numeric user and group ids.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

(defvar tramp-current-multi-method nil
  "Name of `multi' connection method for this *tramp* buffer, or nil if not multi.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

(defvar tramp-current-method nil
  "Connection method for this *tramp* buffer.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

(defvar tramp-current-user nil
  "Remote login name for this *tramp* buffer.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

(defvar tramp-current-host nil
  "Remote host for this *tramp* buffer.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

(defvar tramp-test-groks-nt nil
  "Whether the `test' command groks the `-nt' switch.
\(`test A -nt B' tests if file A is newer than file B.)
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

(defvar tramp-file-exists-command nil
  "Command to use for checking if a file exists.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

(defconst tramp-uudecode "\
tramp_uudecode () {
\(echo begin 600 /tmp/tramp.$$; tail +2) | uudecode
cat /tmp/tramp.$$
rm -f /tmp/tramp.$$
}"
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
(defconst tramp-perl-file-attributes "\
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
    \"(%s %u %s %s (%u %u) (%u %u) (%u %u) %u %u t (%u . %u) -1)\\n\",
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
);"
  "Perl script to produce output suitable for use with `file-attributes'
on the remote file system.")

(defconst tramp-perl-directory-files-and-attributes "\
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
        \"(\\\"%s\\\" %s %u %s %s (%u %u) (%u %u) (%u %u) %u %u t (%u . %u) (%u %u))\\n\",
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
printf(\")\\n\");"
  "Perl script implementing `directory-files-attributes' as Lisp `read'able
output.")

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
(defvar tramp-perl-encode-with-module
  "perl -MMIME::Base64 -0777 -ne 'print encode_base64($_)'"
  "Perl program to use for encoding a file.
Escape sequence %s is replaced with name of Perl binary.
This string is passed to `format', so percent characters need to be doubled.
This implementation requires the MIME::Base64 Perl module to be installed
on the remote host.")

(defvar tramp-perl-decode-with-module
  "perl -MMIME::Base64 -0777 -ne 'print decode_base64($_)'"
  "Perl program to use for decoding a file.
Escape sequence %s is replaced with name of Perl binary.
This string is passed to `format', so percent characters need to be doubled.
This implementation requires the MIME::Base64 Perl module to be installed
on the remote host.")

(defvar tramp-perl-encode
  "%s -e '
# This script contributed by Juanma Barranquero <lektu@terra.es>.
# Copyright (C) 2002 Free Software Foundation, Inc.
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
}
'"
  "Perl program to use for encoding a file.
Escape sequence %s is replaced with name of Perl binary.
This string is passed to `format', so percent characters need to be doubled.")

(defvar tramp-perl-decode
  "%s -e '
# This script contributed by Juanma Barranquero <lektu@terra.es>.
# Copyright (C) 2002 Free Software Foundation, Inc.
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
}
'"
  "Perl program to use for decoding a file.
Escape sequence %s is replaced with name of Perl binary.
This string is passed to `format', so percent characters need to be doubled.")

; These values conform to `file-attributes' from XEmacs 21.2.
; GNU Emacs and other tools not checked.
(defconst tramp-file-mode-type-map '((0  . "-")  ; Normal file (SVID-v2 and XPG2)
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

(defvar tramp-dos-coding-system
  (if (and (fboundp 'coding-system-p)
           (funcall 'coding-system-p '(dos)))
      'dos
    'undecided-dos)
  "Some Emacsen know the `dos' coding system, others need `undecided-dos'.")

(defvar tramp-last-cmd nil
  "Internal Tramp variable recording the last command sent.
This variable is buffer-local in every buffer.")
(make-variable-buffer-local 'tramp-last-cmd)

(defvar tramp-process-echoes nil
  "Whether to process echoes from the remote shell.")

(defvar tramp-last-cmd-time nil
  "Internal Tramp variable recording the time when the last cmd was sent.
This variable is buffer-local in every buffer.")
(make-variable-buffer-local 'tramp-last-cmd-time)

;; This variable does not have the right value in XEmacs.  What should
;; I use instead of find-operation-coding-system in XEmacs?
(defvar tramp-feature-write-region-fix
  (when (fboundp 'find-operation-coding-system)
    (let ((file-coding-system-alist '(("test" emacs-mule))))
      (funcall (symbol-function 'find-operation-coding-system)
	       'write-region 0 0 "" nil "test")))
    "Internal variable to say if `write-region' chooses the right coding.
Older versions of Emacs chose the coding system for `write-region' based
on the FILENAME argument, even if VISIT was a string.")

;; New handlers should be added here.  The following operations can be
;; handled using the normal primitives: file-name-as-directory,
;; file-name-directory, file-name-nondirectory,
;; file-name-sans-versions, get-file-buffer.
(defconst tramp-file-name-handler-alist
  '(
    (load . tramp-handle-load)
    (make-symbolic-link . tramp-handle-make-symbolic-link)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    (file-truename . tramp-handle-file-truename)
    (file-exists-p . tramp-handle-file-exists-p)
    (file-directory-p . tramp-handle-file-directory-p)
    (file-executable-p . tramp-handle-file-executable-p)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
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
    (make-directory . tramp-handle-make-directory)
    (delete-directory . tramp-handle-delete-directory)
    (delete-file . tramp-handle-delete-file)
    (directory-file-name . tramp-handle-directory-file-name)
    (shell-command . tramp-handle-shell-command)
    (process-file . tramp-handle-process-file)
    (insert-directory . tramp-handle-insert-directory)
    (expand-file-name . tramp-handle-expand-file-name)
    (file-local-copy . tramp-handle-file-local-copy)
    (file-remote-p . tramp-handle-file-remote-p)
    (insert-file-contents . tramp-handle-insert-file-contents)
    (write-region . tramp-handle-write-region)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (unhandled-file-name-directory . tramp-handle-unhandled-file-name-directory)
    (dired-compress-file . tramp-handle-dired-compress-file)
    (dired-call-process . tramp-handle-dired-call-process)
    (dired-recursive-delete-directory
     . tramp-handle-dired-recursive-delete-directory)
    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime))
  "Alist of handler functions.
Operations not mentioned here will be handled by the normal Emacs functions.")

;; Handlers for partial tramp file names. For GNU Emacs just
;; `file-name-all-completions' is needed. The other ones are necessary
;; for XEmacs.
(defconst tramp-completion-file-name-handler-alist
  '(
    (file-name-directory . tramp-completion-handle-file-name-directory)
    (file-name-nondirectory . tramp-completion-handle-file-name-nondirectory)
    (file-exists-p . tramp-completion-handle-file-exists-p)
    (file-name-all-completions . tramp-completion-handle-file-name-all-completions)
    (file-name-completion . tramp-completion-handle-file-name-completion)
    (expand-file-name . tramp-completion-handle-expand-file-name))
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

;;; Internal functions which must come first.

(defsubst tramp-message (level fmt-string &rest args)
  "Emit a message depending on verbosity level.
First arg LEVEL says to be quiet if `tramp-verbose' is less than LEVEL.  The
message is emitted only if `tramp-verbose' is greater than or equal to LEVEL.
Calls function `message' with FMT-STRING as control string and the remaining
ARGS to actually emit the message (if applicable).

This function expects to be called from the tramp buffer only!"
  (when (<= level tramp-verbose)
    (apply #'message (concat "tramp: " fmt-string) args)
    (when tramp-debug-buffer
      (save-excursion
        (set-buffer
         (tramp-get-debug-buffer
	  tramp-current-multi-method tramp-current-method
	  tramp-current-user tramp-current-host))
        (goto-char (point-max))
	(unless (bolp)
	  (insert "\n"))
	(tramp-insert-with-face
         'italic
         (concat "# " (apply #'format fmt-string args) "\n"))))))

(defun tramp-message-for-buffer
  (multi-method method user host level fmt-string &rest args)
  "Like `tramp-message' but temporarily switches to the tramp buffer.
First three args METHOD, USER, and HOST identify the tramp buffer to use,
remaining args passed to `tramp-message'."
  (save-excursion
    (set-buffer (tramp-get-buffer multi-method method user host))
    (apply 'tramp-message level fmt-string args)))

(defsubst tramp-line-end-position nil
  "Return point at end of line.
Calls `line-end-position' or `point-at-eol' if defined, else
own implementation."
  (cond
   ((fboundp 'line-end-position) (funcall (symbol-function 'line-end-position)))
   ((fboundp 'point-at-eol) 	 (funcall (symbol-function 'point-at-eol)))
   (t (save-excursion (end-of-line) (point)))))

(defmacro with-parsed-tramp-file-name (filename var &rest body)
  "Parse a Tramp filename and make components available in the body.

First arg FILENAME is evaluated and dissected into its components.
Second arg VAR is a symbol.  It is used as a variable name to hold
the filename structure.  It is also used as a prefix for the variables
holding the components.  For example, if VAR is the symbol `foo', then
`foo' will be bound to the whole structure, `foo-multi-method' will
be bound to the multi-method component, and so on for `foo-method',
`foo-user', `foo-host', `foo-localname'.

Remaining args are Lisp expressions to be evaluated (inside an implicit
`progn').

If VAR is nil, then we bind `v' to the structure and `multi-method',
`method', `user', `host', `localname' to the components."
  `(let* ((,(or var 'v) (tramp-dissect-file-name ,filename))
	  (,(if var (intern (concat (symbol-name var) "-multi-method")) 'multi-method)
	   (tramp-file-name-multi-method ,(or var 'v)))
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
;; To be activated for debugging containing this macro
;; It works only when VAR is nil.  Otherwise, it can be deactivated by
;; (put 'with-parsed-tramp-file-name 'edebug-form-spec 0)
;; I'm too stupid to write a precise SPEC for it.
(put 'with-parsed-tramp-file-name 'edebug-form-spec t)

(defmacro tramp-let-maybe (variable value &rest body)
  "Let-bind VARIABLE to VALUE in BODY, but only if VARIABLE is not obsolete.
BODY is executed whether or not the variable is obsolete.
The intent is to protect against `obsolete variable' warnings."
  `(if (get ',variable 'byte-obsolete-variable)
       (progn ,@body)
     (let ((,variable ,value))
       ,@body)))
(put 'tramp-let-maybe 'lisp-indent-function 2)

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
      ;; Remove double entries
      (when (member (car v) (cdr v))
	(setcdr v (delete (car v) (cdr v))))
      ;; Check for function and file
      (unless (and (functionp (nth 0 (car v)))
		   (file-exists-p (nth 1 (car v))))
	(setq r (delete (car v) r)))
      (setq v (cdr v)))

    (when r
      (add-to-list 'tramp-completion-function-alist
		   (cons method r)))))

(defun tramp-get-completion-function (method)
  "Returns list of completion functions for METHOD.
For definition of that list see `tramp-set-completion-function'."
  (cdr (assoc method tramp-completion-function-alist)))

;;; File Name Handler Functions:

(defun tramp-handle-make-symbolic-link
  (filename linkname &optional ok-if-already-exists)
  "Like `make-symbolic-link' for tramp files.
If LINKNAME is a non-Tramp file, it is used verbatim as the target of
the symlink.  If LINKNAME is a Tramp file, only the localname component is
used as the target of the symlink.

If LINKNAME is a Tramp file and the localname component is relative, then
it is expanded first, before the localname component is taken.  Note that
this can give surprising results if the user/host for the source and
target of the symlink differ."
  (with-parsed-tramp-file-name linkname l
    (let ((ln (tramp-get-remote-ln l-multi-method l-method l-user l-host))
	  (cwd (file-name-directory l-localname)))
      (unless ln
	(signal 'file-error
		(list "Making a symbolic link."
		      "ln(1) does not exist on the remote host.")))

      ;; Do the 'confirm if exists' thing.
      (when (file-exists-p linkname)
	;; What to do?
	(if (or (null ok-if-already-exists) ; not allowed to exist
		(and (numberp ok-if-already-exists)
		     (not (yes-or-no-p
			   (format
			    "File %s already exists; make it a link anyway? "
			    l-localname)))))
	    (signal 'file-already-exists (list "File already exists" l-localname))
	  (delete-file linkname)))

      ;; If FILENAME is a Tramp name, use just the localname component.
      (when (tramp-tramp-file-p filename)
	(setq filename (tramp-file-name-localname
			(tramp-dissect-file-name
			 (expand-file-name filename)))))

      ;; Right, they are on the same host, regardless of user, method, etc.
      ;; We now make the link on the remote machine. This will occur as the user
      ;; that FILENAME belongs to.
      (zerop
       (tramp-send-command-and-check
	l-multi-method l-method l-user l-host
	(format "cd %s && %s -sf %s %s"
		cwd ln
		filename
		l-localname)
	t)))))


(defun tramp-handle-load (file &optional noerror nomessage nosuffix must-suffix)
  "Like `load' for tramp files.  Not implemented!"
  (unless (file-name-absolute-p file)
    (error "Tramp cannot `load' files without absolute file name"))
  (with-parsed-tramp-file-name file nil
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
	(error "File `%s' does not include a `.el' or `.elc' suffix"
	       file)))
    (unless noerror
      (when (not (file-exists-p file))
	(error "Cannot load nonexistent file `%s'" file)))
    (if (not (file-exists-p file))
	nil
      (unless nomessage
	(message "Loading %s..." file))
      (let ((local-copy (file-local-copy file)))
	;; MUST-SUFFIX doesn't exist on XEmacs, so let it default to nil.
	(load local-copy noerror t t)
	(delete-file local-copy))
      (unless nomessage
	(message "Loading %s...done" file))
      t)))

;; Localname manipulation functions that grok TRAMP localnames...
(defun tramp-handle-file-name-directory (file)
  "Like `file-name-directory' but aware of TRAMP files."
  ;; everything except the last filename thing is the directory
  (with-parsed-tramp-file-name file nil
    ;; For the following condition, two possibilities should be tried:
    ;; (1) (string= localname "")
    ;; (2) (or (string= localname "") (string= localname "/"))
    ;; The second variant fails when completing a "/" directory on
    ;; the remote host, that is a filename which looks like
    ;; "/user@host:/".  But maybe wildcards fail with the first variant.
    ;; We should do some investigation.
    (if (string= localname "")
	;; For a filename like "/[foo]", we return "/".  The `else'
	;; case would return "/[foo]" unchanged.  But if we do that,
	;; then `file-expand-wildcards' ceases to work.  It's not
	;; quite clear to me what's the intuition that tells that this
	;; behavior is the right behavior, but oh, well.
	"/"
      ;; run the command on the localname portion only
      ;; CCC: This should take into account the remote machine type, no?
      ;;  --daniel <daniel@danann.net>
      (tramp-make-tramp-file-name multi-method method user host
				  ;; This will not recurse...
				  (or (file-name-directory localname) "")))))

(defun tramp-handle-file-name-nondirectory (file)
  "Like `file-name-nondirectory' but aware of TRAMP files."
  (with-parsed-tramp-file-name file nil
    (file-name-nondirectory localname)))

(defun tramp-handle-file-truename (filename &optional counter prev-dirs)
  "Like `file-truename' for tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (let* ((steps        (tramp-split-string localname "/"))
	   (localnamedir (tramp-let-maybe directory-sep-char ?/	;for XEmacs
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
      (tramp-message-for-buffer
       multi-method method user host
       10 "Finding true name for `%s'" filename)
      (while (and steps (< numchase numchase-limit))
	(setq thisstep (pop steps))
	(tramp-message-for-buffer
	 multi-method method user host
	 10 "Check %s"
	 (mapconcat 'identity
		    (append '("") (reverse result) (list thisstep))
		    "/"))
	(setq symlink-target
	      (nth 0 (file-attributes
		      (tramp-make-tramp-file-name
		       multi-method method user host
		       (mapconcat 'identity
				  (append '("")
					  (reverse result)
					  (list thisstep))
				  "/")))))
	(cond ((string= "." thisstep)
	       (tramp-message-for-buffer multi-method method user host
					 10 "Ignoring step `.'"))
	      ((string= ".." thisstep)
	       (tramp-message-for-buffer multi-method method user host
					 10 "Processing step `..'")
	       (pop result))
	      ((stringp symlink-target)
	       ;; It's a symlink, follow it.
	       (tramp-message-for-buffer
		multi-method method user host
		10 "Follow symlink to %s" symlink-target)
	       (setq numchase (1+ numchase))
	       (when (file-name-absolute-p symlink-target)
		 (setq result nil))
	       ;; If the symlink was absolute, we'll get a string like
	       ;; "/user@host:/some/target"; extract the
	       ;; "/some/target" part from it.
	       (when (tramp-tramp-file-p symlink-target)
		 (with-parsed-tramp-file-name symlink-target sym
		   (unless (equal (list multi-method method user host)
				  (list sym-multi-method sym-method
					sym-user sym-host))
		     (error "Symlink target `%s' on wrong host"
			    symlink-target))
		   (setq symlink-target localname)))
	       (setq steps
		     (append (tramp-split-string symlink-target "/") steps)))
	      (t
	       ;; It's a file.
	       (setq result (cons thisstep result)))))
      (when (>= numchase numchase-limit)
	(error "Maximum number (%d) of symlinks exceeded" numchase-limit))
      (setq result (reverse result))
      ;; Combine list to form string.
      (setq result
	    (if result
		(mapconcat 'identity (cons "" result) "/")
	      "/"))
      (when (and is-dir (or (string= "" result)
			    (not (string= (substring result -1) "/"))))
	(setq result (concat result "/")))
      (tramp-message-for-buffer
       multi-method method user host
       10 "True name of `%s' is `%s'" filename result)
      (tramp-make-tramp-file-name
       multi-method method user host result))))

;; Basic functions.

(defun tramp-handle-file-exists-p (filename)
  "Like `file-exists-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (save-excursion
      (zerop (tramp-send-command-and-check
	      multi-method method user host
	      (format
	       (tramp-get-file-exists-command multi-method method user host)
	       (tramp-shell-quote-argument localname)))))))

;; Devices must distinguish physical file systems.  The device numbers
;; provided by "lstat" aren't unique, because we operate on different hosts.
;; So we use virtual device numbers, generated by Tramp.  Both Ange-FTP and
;; EFS use device number "-1".  In order to be different, we use device number
;; (-1 x), whereby "x" is unique for a given (multi-method method user host).
(defvar tramp-devices nil
  "Keeps virtual device numbers.")

;; CCC: This should check for an error condition and signal failure
;;      when something goes wrong.
;; Daniel Pittman <daniel@danann.net>
(defun tramp-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for tramp files."
  (when (file-exists-p filename)
    ;; file exists, find out stuff
    (unless id-format (setq id-format 'integer))
    (with-parsed-tramp-file-name filename nil
      (save-excursion
        (tramp-convert-file-attributes
         multi-method method user host
         (if (tramp-get-remote-perl multi-method method user host)
             (tramp-handle-file-attributes-with-perl multi-method method user host
                                                     localname id-format)
           (tramp-handle-file-attributes-with-ls multi-method method user host
                                                 localname id-format)))))))

(defun tramp-handle-file-attributes-with-ls
  (multi-method method user host localname &optional id-format)
  "Implement `file-attributes' for tramp files using the ls(1) command."
  (let (symlinkp dirp
		 res-inode res-filemodes res-numlinks
		 res-uid res-gid res-size res-symlink-target)
    (tramp-message-for-buffer multi-method method user host 10
			      "file attributes with ls: %s"
			      (tramp-make-tramp-file-name
			       multi-method method user host localname))
    (tramp-send-command
     multi-method method user host
     (format "%s %s %s"
	     (tramp-get-ls-command multi-method method user host)
	     (if (eq id-format 'integer) "-ildn" "-ild")
	     (tramp-shell-quote-argument localname)))
    (tramp-wait-for-output)
    ;; parse `ls -l' output ...
    ;; ... inode
    (setq res-inode
	  (condition-case err
	      (read (current-buffer))
	    (invalid-read-syntax
	     (when (and (equal (cadr err)
			       "Integer constant overflow in reader")
			(string-match
			 "^[0-9]+\\([0-9][0-9][0-9][0-9][0-9]\\)\\'"
			 (caddr err)))
	       (let* ((big (read (substring (caddr err) 0
					    (match-beginning 1))))
		      (small (read (match-string 1 (caddr err))))
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
    (when (eq id-format 'integer)
      (unless (numberp res-uid) (setq res-uid -1))
      (unless (numberp res-gid) (setq res-gid -1)))
    ;; ... size
    (setq res-size (read (current-buffer)))
    ;; From the file modes, figure out other stuff.
    (setq symlinkp (eq ?l (aref res-filemodes 0)))
    (setq dirp (eq ?d (aref res-filemodes 0)))
    ;; if symlink, find out file name pointed to
    (when symlinkp
      (search-forward "-> ")
      (setq res-symlink-target
	    (buffer-substring (point)
			      (tramp-line-end-position))))
    ;; return data gathered
    (list
     ;; 0. t for directory, string (name linked to) for symbolic
     ;; link, or nil.
     (or dirp res-symlink-target nil)
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
     ;; 9. t iff file's gid would change if file were deleted and
     ;; recreated.
     nil				;hm?
     ;; 10. inode number.
     res-inode
     ;; 11. Device number.  Will be replaced by a virtual device number.
     -1
     )))

(defun tramp-handle-file-attributes-with-perl
  (multi-method method user host localname &optional id-format)
  "Implement `file-attributes' for tramp files using a Perl script."
  (tramp-message-for-buffer multi-method method user host 10
			    "file attributes with perl: %s"
			    (tramp-make-tramp-file-name
			     multi-method method user host localname))
  (tramp-maybe-send-perl-script multi-method method user host
				tramp-perl-file-attributes
                                "tramp_file_attributes")
  (tramp-send-command multi-method method user host
                      (format "tramp_file_attributes %s %s"
                              (tramp-shell-quote-argument localname) id-format))
  (tramp-wait-for-output)
  (read (current-buffer)))

(defun tramp-handle-set-visited-file-modtime (&optional time-list)
  "Like `set-visited-file-modtime' for tramp files."
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
	    (save-excursion
	      (tramp-send-command
	       multi-method method user host
	       (format "%s -ild %s"
		       (tramp-get-ls-command multi-method method user host)
		       (tramp-shell-quote-argument localname)))
	      (tramp-wait-for-output)
	      (setq attr (buffer-substring (point)
					   (progn (end-of-line) (point)))))
	    (setq tramp-buffer-file-attributes attr))
	  (when (boundp 'last-coding-system-used)
	    (set 'last-coding-system-used coding-system-used))
	  nil)))))

;; CCC continue here

;; This function makes the same assumption as
;; `tramp-handle-set-visited-file-modtime'.
(defun tramp-handle-verify-visited-file-modtime (buf)
  "Like `verify-visited-file-modtime' for tramp files.
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
	      (save-excursion
		(tramp-send-command
		 multi-method method user host
		 (format "%s -ild %s"
			 (tramp-get-ls-command multi-method method user host)
			 (tramp-shell-quote-argument localname)))
		(tramp-wait-for-output)
		(setq attr (buffer-substring
			    (point) (progn (end-of-line) (point)))))
	      (equal tramp-buffer-file-attributes attr))
	     ;; If file does not exist, say it is not modified
	     ;; if and only if that agrees with the buffer's record.
	     (t (equal mt '(-1 65535))))))))))

(defun tramp-handle-set-file-modes (filename mode)
  "Like `set-file-modes' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (save-excursion
      (unless (zerop (tramp-send-command-and-check
		      multi-method method user host
		      (format "chmod %s %s"
			      (tramp-decimal-to-octal mode)
			      (tramp-shell-quote-argument localname))))
	(signal 'file-error
		(list "Doing chmod"
		      ;; FIXME: extract the proper text from chmod's stderr.
		      "error while changing file's mode"
		      filename))))))

;; Simple functions using the `test' command.

(defun tramp-handle-file-executable-p (filename)
  "Like `file-executable-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (zerop (tramp-run-test "-x" filename))))

(defun tramp-handle-file-readable-p (filename)
  "Like `file-readable-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (zerop (tramp-run-test "-r" filename))))

(defun tramp-handle-file-accessible-directory-p (filename)
  "Like `file-accessible-directory-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (and (zerop (tramp-run-test "-d" filename))
	 (zerop (tramp-run-test "-r" filename))
	 (zerop (tramp-run-test "-x" filename)))))

;; When the remote shell is started, it looks for a shell which groks
;; tilde expansion.  Here, we assume that all shells which grok tilde
;; expansion will also provide a `test' command which groks `-nt' (for
;; newer than).  If this breaks, tell me about it and I'll try to do
;; something smarter about it.
(defun tramp-handle-file-newer-than-file-p (file1 file2)
  "Like `file-newer-than-file-p' for tramp files."
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
	       (unless (and (tramp-tramp-file-p file1)
			    (tramp-tramp-file-p file2))
		 (signal
		  'file-error
		  (list
		   "Cannot check if Tramp file is newer than non-Tramp file"
		   file1 file2)))
	       (with-parsed-tramp-file-name file1 v1
		 (with-parsed-tramp-file-name file2 v2
		   (unless (and (equal v1-multi-method v2-multi-method)
				(equal v1-method v2-method)
				(equal v1-user v2-user)
				(equal v1-host v2-host))
		     (signal 'file-error
			     (list "Files must have same method, user, host"
				   file1 file2)))
		   (unless (and (tramp-tramp-file-p file1)
				(tramp-tramp-file-p file2))
		     (signal 'file-error
			     (list "Files must be tramp files on same host"
				   file1 file2)))
		   (if (tramp-get-test-groks-nt
			v1-multi-method v1-method v1-user v1-host)
		       (zerop (tramp-run-test2 "test" file1 file2 "-nt"))
		     (zerop (tramp-run-test2
			     "tramp_test_nt" file1 file2)))))))))))

;; Functions implemented using the basic functions above.

(defun tramp-handle-file-modes (filename)
  "Like `file-modes' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (when (file-exists-p filename)
      (tramp-mode-string-to-int
       (nth 8 (file-attributes filename))))))

(defun tramp-handle-file-directory-p (filename)
  "Like `file-directory-p' for tramp files."
  ;; Care must be taken that this function returns `t' for symlinks
  ;; pointing to directories.  Surely the most obvious implementation
  ;; would be `test -d', but that returns false for such symlinks.
  ;; CCC: Stefan Monnier says that `test -d' follows symlinks.  And
  ;; I now think he's right.  So we could be using `test -d', couldn't
  ;; we?
  ;;
  ;; Alternatives: `cd %s', `test -d %s'
  (with-parsed-tramp-file-name filename nil
    (save-excursion
      (zerop
       (tramp-send-command-and-check
	multi-method method user host
	(format "test -d %s"
		(tramp-shell-quote-argument localname))
	t)))))				;run command in subshell

(defun tramp-handle-file-regular-p (filename)
  "Like `file-regular-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (and (file-exists-p filename)
	 (eq ?- (aref (nth 8 (file-attributes filename)) 0)))))

(defun tramp-handle-file-symlink-p (filename)
  "Like `file-symlink-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (let ((x (car (file-attributes filename))))
      (when (stringp x)
	;; When Tramp is running on VMS, then `file-name-absolute-p'
	;; might do weird things.
	(if (file-name-absolute-p x)
	    (tramp-make-tramp-file-name
	     multi-method method user host x)
	  x)))))

(defun tramp-handle-file-writable-p (filename)
  "Like `file-writable-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (if (file-exists-p filename)
	;; Existing files must be writable.
	(zerop (tramp-run-test "-w" filename))
      ;; If file doesn't exist, check if directory is writable.
      (and (zerop (tramp-run-test
		   "-d" (file-name-directory filename)))
	   (zerop (tramp-run-test
		   "-w" (file-name-directory filename)))))))

(defun tramp-handle-file-ownership-preserved-p (filename)
  "Like `file-ownership-preserved-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (or (not (file-exists-p filename))
	;; Existing files must be writable.
	(zerop (tramp-run-test "-O" filename)))))

;; Other file name ops.

;; ;; Matthias K,Av(Bppe <mkoeppe@mail.math.uni-magdeburg.de>
;; (defun tramp-handle-directory-file-name (directory)
;;   "Like `directory-file-name' for tramp files."
;;   (if (and (eq (aref directory (- (length directory) 1)) ?/)
;; 	   (not (eq (aref directory (- (length directory) 2)) ?:)))
;;       (substring directory 0 (- (length directory) 1))
;;     directory))

;; ;; Philippe Troin <phil@fifi.org>
;; (defun tramp-handle-directory-file-name (directory)
;;   "Like `directory-file-name' for tramp files."
;;   (with-parsed-tramp-file-name directory nil
;;     (let ((directory-length-1 (1- (length directory))))
;;       (save-match-data
;; 	(if (and (eq (aref directory directory-length-1) ?/)
;; 		 (eq (string-match tramp-file-name-regexp directory) 0)
;; 		 (/= (match-end 0) directory-length-1))
;; 	    (substring directory 0 directory-length-1)
;; 	  directory)))))

(defun tramp-handle-directory-file-name (directory)
  "Like `directory-file-name' for tramp files."
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

(defun tramp-handle-directory-files (directory
				     &optional full match nosort files-only)
  "Like `directory-files' for tramp files."
  (with-parsed-tramp-file-name directory nil
    (let (result x)
      (save-excursion
	(tramp-barf-unless-okay
	 multi-method method user host
	 (concat "cd " (tramp-shell-quote-argument localname))
	 nil
	 'file-error
	 "tramp-handle-directory-files: couldn't `cd %s'"
	 (tramp-shell-quote-argument localname))
	(tramp-send-command
	 multi-method method user host
	 (concat (tramp-get-ls-command multi-method method user host)
		 " -a | cat"))
	(tramp-wait-for-output)
	(goto-char (point-max))
	(while (zerop (forward-line -1))
	  (setq x (buffer-substring (point)
				    (tramp-line-end-position)))
	  (when (or (not match) (string-match match x))
	    (if full
		(push (concat (file-name-as-directory directory)
			      x)
		      result)
	      (push x result))))
	(tramp-send-command multi-method method user host "cd")
	(tramp-wait-for-output)
	;; Remove non-files or non-directories if necessary.  Using
	;; the remote shell for this would probably be way faster.
	;; Maybe something could be adapted from
	;; tramp-handle-file-name-all-completions.
	(when files-only
	  (let ((temp (nreverse result))
		item)
	    (setq result nil)
	    (if (equal files-only t)
		;; files only
		(while temp
		  (setq item (pop temp))
		  (when (file-regular-p item)
		    (push item result)))
	      ;; directories only
	      (while temp
		(setq item (pop temp))
		(when (file-directory-p item)
		  (push item result)))))))
      result)))

(defun tramp-handle-directory-files-and-attributes
  (directory &optional full match nosort id-format)
  "Like `directory-files-and-attributes' for tramp files."
  (when (tramp-handle-file-exists-p directory)
    (save-excursion
      (setq directory (tramp-handle-expand-file-name directory))
      (with-parsed-tramp-file-name directory nil
        (tramp-maybe-send-perl-script multi-method method user host
				      tramp-perl-directory-files-and-attributes
                                      "tramp_directory_files_and_attributes")
        (tramp-send-command multi-method method user host
                            (format "tramp_directory_files_and_attributes %s %s"
                                    (tramp-shell-quote-argument localname)
                                    (or id-format 'integer)))
        (tramp-wait-for-output)
        (let* ((root (cons nil (let ((object (read (current-buffer))))
                                 (when (stringp object)
                                   (error object))
                                 object)))
               (cell root))
          (while (cdr cell)
            (if (and match (not (string-match match (caadr cell))))
                ;; Remove from list
                (setcdr cell (cddr cell))
              ;; Include in list
              (setq cell (cdr cell))
              (let ((l (car cell)))
                (tramp-convert-file-attributes multi-method method user host
                                               (cdr l))
                ;; If FULL, make file name absolute
                (when full (setcar l (concat directory "/" (car l)))))))
          (if nosort
              (cdr root)
            (sort (cdr root) (lambda (x y) (string< (car x) (car y))))))))))

;; This function should return "foo/" for directories and "bar" for
;; files.  We use `ls -ad' to get a list of files (including
;; directories), and `find . -type d \! -name . -prune' to get a list
;; of directories.
(defun tramp-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for tramp files."
  (with-parsed-tramp-file-name directory nil
    (unless (save-match-data (string-match "/" filename))
      (let* ((nowild tramp-completion-without-shell-p)
	     result)
	(save-excursion
	  (tramp-barf-unless-okay
	   multi-method method user host
	   (format "cd %s" (tramp-shell-quote-argument localname))
	   nil 'file-error
	   "tramp-handle-file-name-all-completions: Couldn't `cd %s'"
	   (tramp-shell-quote-argument localname))

	  ;; Get a list of directories and files, including reliably
	  ;; tagging the directories with a trailing '/'.  Because I
	  ;; rock.  --daniel@danann.net
	  (tramp-send-command
	   multi-method method user host
	   (format (concat "%s -a %s 2>/dev/null | while read f; do "
			   "if test -d \"$f\" 2>/dev/null; "
			   "then echo \"$f/\"; else echo \"$f\"; fi; done")
		   (tramp-get-ls-command multi-method method user host)
		   (if (or nowild (zerop (length filename)))
		       ""
		     (format "-d %s*"
			     (tramp-shell-quote-argument filename)))))

	  ;; Now grab the output.
	  (tramp-wait-for-output)
	  (goto-char (point-max))
	  (while (zerop (forward-line -1))
	    (push (buffer-substring (point)
				    (tramp-line-end-position))
		  result))

	  (tramp-send-command multi-method method user host "cd")
	  (tramp-wait-for-output)

	  ;; Return the list.
	  (if nowild
	      (all-completions filename (mapcar 'list result))
	    result))))))


;; The following isn't needed for Emacs 20 but for 19.34?
(defun tramp-handle-file-name-completion (filename directory)
  "Like `file-name-completion' for tramp files."
  (unless (tramp-tramp-file-p directory)
    (error
     "tramp-handle-file-name-completion invoked on non-tramp directory `%s'"
     directory))
  (with-parsed-tramp-file-name directory nil
    (try-completion
     filename
     (mapcar (lambda (x) (cons x nil))
	     (file-name-all-completions filename directory)))))

;; cp, mv and ln

(defun tramp-handle-add-name-to-file
  (filename newname &optional ok-if-already-exists)
  "Like `add-name-to-file' for tramp files."
  (with-parsed-tramp-file-name filename v1
    (with-parsed-tramp-file-name newname v2
      (let ((ln (when v1 (tramp-get-remote-ln
			  v1-multi-method v1-method v1-user v1-host))))
	(unless (and v1-method v2-method v1-user v2-user v1-host v2-host
		     (equal v1-multi-method v2-multi-method)
		     (equal v1-method v2-method)
		     (equal v1-user v2-user)
		     (equal v1-host v2-host))
	  (error "add-name-to-file: %s"
		 "only implemented for same method, same user, same host"))
	(when (and (not ok-if-already-exists)
		   (file-exists-p newname)
		   (not (numberp ok-if-already-exists))
		   (y-or-n-p
		    (format
		     "File %s already exists; make it a new name anyway? "
		     newname)))
	  (error "add-name-to-file: file %s already exists" newname))
	(tramp-barf-unless-okay
	 v1-multi-method v1-method v1-user v1-host
	 (format "%s %s %s" ln (tramp-shell-quote-argument v1-localname)
		 (tramp-shell-quote-argument v2-localname))
	 nil 'file-error
	 "error with add-name-to-file, see buffer `%s' for details"
	 (buffer-name))))))

(defun tramp-handle-copy-file
  (filename newname &optional ok-if-already-exists keep-date)
  "Like `copy-file' for tramp files."
  ;; Check if both files are local -- invoke normal copy-file.
  ;; Otherwise, use tramp from local system.
  (setq filename (expand-file-name filename))
  (setq newname (expand-file-name newname))
  ;; At least one file a tramp file?
  (if (or (tramp-tramp-file-p filename)
          (tramp-tramp-file-p newname))
      (tramp-do-copy-or-rename-file
       'copy filename newname ok-if-already-exists keep-date)
    (tramp-run-real-handler
     'copy-file
     (list filename newname ok-if-already-exists keep-date))))

(defun tramp-handle-rename-file
  (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for tramp files."
  ;; Check if both files are local -- invoke normal rename-file.
  ;; Otherwise, use tramp from local system.
  (setq filename (expand-file-name filename))
  (setq newname (expand-file-name newname))
  ;; At least one file a tramp file?
  (if (or (tramp-tramp-file-p filename)
          (tramp-tramp-file-p newname))
      (tramp-do-copy-or-rename-file
       'rename filename newname ok-if-already-exists)
    (tramp-run-real-handler 'rename-file
                          (list filename newname ok-if-already-exists))))

(defun tramp-do-copy-or-rename-file
  (op filename newname &optional ok-if-already-exists keep-date)
  "Copy or rename a remote file.
OP must be `copy' or `rename' and indicates the operation to perform.
FILENAME specifies the file to copy or rename, NEWNAME is the name of
the new file (for copy) or the new name of the file (for rename).
OK-IF-ALREADY-EXISTS means don't barf if NEWNAME exists already.
KEEP-DATE means to make sure that NEWNAME has the same timestamp
as FILENAME.

This function is invoked by `tramp-handle-copy-file' and
`tramp-handle-rename-file'.  It is an error if OP is neither of `copy'
and `rename'.  FILENAME and NEWNAME must be absolute file names."
  (unless (memq op '(copy rename))
    (error "Unknown operation `%s', must be `copy' or `rename'" op))
  (unless ok-if-already-exists
    (when (file-exists-p newname)
      (signal 'file-already-exists
              (list newname))))
  (let ((t1 (tramp-tramp-file-p filename))
	(t2 (tramp-tramp-file-p newname))
	v1-multi-method v1-method v1-user v1-host v1-localname
	v2-multi-method v2-method v2-user v2-host v2-localname)

    ;; Check which ones of source and target are Tramp files.
    ;; We cannot invoke `with-parsed-tramp-file-name';
    ;; it fails if the file isn't a Tramp file name.
    (if t1
	(with-parsed-tramp-file-name filename l
	  (setq v1-multi-method l-multi-method
		v1-method l-method
		v1-user l-user
		v1-host l-host
		v1-localname l-localname))
      (setq v1-localname filename))
    (if t2
	(with-parsed-tramp-file-name newname l
	  (setq v2-multi-method l-multi-method
		v2-method l-method
		v2-user l-user
		v2-host l-host
		v2-localname l-localname))
      (setq v2-localname newname))

    (cond
     ;; Both are Tramp files.
     ((and t1 t2)
      (cond
       ;; Shortcut: if method, host, user are the same for both
       ;; files, we invoke `cp' or `mv' on the remote host
       ;; directly.
       ((and (equal v1-multi-method v2-multi-method)
	     (equal v1-method v2-method)
	     (equal v1-user v2-user)
	     (equal v1-host v2-host))
	(tramp-do-copy-or-rename-file-directly
	 op v1-multi-method v1-method v1-user v1-host
	 v1-localname v2-localname keep-date))
       ;; If both source and target are Tramp files,
       ;; both are using the same copy-program, then we
       ;; can invoke rcp directly.  Note that
       ;; default-directory should point to a local
       ;; directory if we want to invoke rcp.
       ((and (not v1-multi-method)
	     (not v2-multi-method)
	     (equal v1-method v2-method)
	     (tramp-method-out-of-band-p
	      v1-multi-method v1-method v1-user v1-host)
	     (not (string-match "\\([^#]*\\)#\\(.*\\)" v1-host))
	     (not (string-match "\\([^#]*\\)#\\(.*\\)" v2-host)))
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
	 op filename newname keep-date))))

     ;; One file is a Tramp file, the other one is local.
     ((or t1 t2)
      ;; If the Tramp file has an out-of-band method, the corresponding
      ;; copy-program can be invoked.
      (if (and (not v1-multi-method)
	       (not v2-multi-method)
	       (or (tramp-method-out-of-band-p
		    v1-multi-method v1-method v1-user v1-host)
		   (tramp-method-out-of-band-p
		    v2-multi-method v2-method v2-user v2-host)))
	  (tramp-do-copy-or-rename-file-out-of-band
	   op filename newname keep-date)
	;; Use the generic method via a Tramp buffer.
	(tramp-do-copy-or-rename-file-via-buffer
	 op filename newname keep-date)))

     (t
      ;; One of them must be a Tramp file.
      (error "Tramp implementation says this cannot happen")))))

(defun tramp-do-copy-or-rename-file-via-buffer (op filename newname keep-date)
  "Use an Emacs buffer to copy or rename a file.
First arg OP is either `copy' or `rename' and indicates the operation.
FILENAME is the source file, NEWNAME the target file.
KEEP-DATE is non-nil if NEWNAME should have the same timestamp as FILENAME."
  (let ((trampbuf (get-buffer-create "*tramp output*"))
	(modtime (nth 5 (file-attributes filename))))
    (when (and keep-date (or (null modtime) (equal modtime '(0 0))))
      (tramp-message
       1 (concat "Warning: cannot preserve file time stamp"
		 " with inline copying across machines")))
    (save-excursion
      (set-buffer trampbuf) (erase-buffer)
      (insert-file-contents-literally filename)
      ;; We don't want the target file to be compressed, so we let-bind
      ;; `jka-compr-inhibit' to t.
      (let ((coding-system-for-write 'binary)
	    (jka-compr-inhibit t))
	(write-region (point-min) (point-max) newname))
      ;; KEEP-DATE handling.
      (when keep-date
	(when (and (not (null modtime))
		   (not (equal modtime '(0 0))))
	  (tramp-touch newname modtime)))
      ;; Set the mode.
      (set-file-modes newname (file-modes filename)))
    ;; If the operation was `rename', delete the original file.
    (unless (eq op 'copy)
      (delete-file filename))))

(defun tramp-do-copy-or-rename-file-directly
  (op multi-method method user host localname1 localname2 keep-date)
  "Invokes `cp' or `mv' on the remote system.
OP must be one of `copy' or `rename', indicating `cp' or `mv',
respectively.  METHOD, USER, and HOST specify the connection.
LOCALNAME1 and LOCALNAME2 specify the two arguments of `cp' or `mv'.
If KEEP-DATE is non-nil, preserve the time stamp when copying."
  ;; CCC: What happens to the timestamp when renaming?
  (let ((cmd (cond ((and (eq op 'copy) keep-date) "cp -f -p")
                   ((eq op 'copy) "cp -f")
                   ((eq op 'rename) "mv -f")
                   (t (error
                       "Unknown operation `%s', must be `copy' or `rename'"
                       op)))))
    (save-excursion
      (tramp-send-command
       multi-method method user host
       (format "%s %s %s"
               cmd
               (tramp-shell-quote-argument localname1)
               (tramp-shell-quote-argument localname2)))
      (tramp-wait-for-output)
      (goto-char (point-min))
      (unless
	  (or
	   (and (eq op 'copy) keep-date
		;; Mask cp -f error.
		(re-search-forward tramp-operation-not-permitted-regexp nil t))
	   (zerop (tramp-send-command-and-check
		   multi-method method user host nil nil)))
	(pop-to-buffer (current-buffer))
	(signal 'file-error
		(format "Copying directly failed, see buffer `%s' for details."
			(buffer-name)))))
    ;; Set the mode.
    ;; CCC: Maybe `chmod --reference=localname1 localname2' could be used
    ;;      where available?
    (unless (or (eq op 'rename) keep-date)
      (set-file-modes
       (tramp-make-tramp-file-name multi-method method user host localname2)
       (file-modes
	(tramp-make-tramp-file-name
	 multi-method method user host localname1))))))

(defun tramp-do-copy-or-rename-file-out-of-band (op filename newname keep-date)
  "Invoke rcp program to copy.
One of FILENAME and NEWNAME must be a Tramp name, the other must
be a local filename.  The method used must be an out-of-band method."
  (let ((t1 (tramp-tramp-file-p filename))
	(t2 (tramp-tramp-file-p newname))
	v1-multi-method v1-method v1-user v1-host v1-localname
	v2-multi-method v2-method v2-user v2-host v2-localname
	multi-method method user host copy-program copy-args
	source target trampbuf)

    ;; Check which ones of source and target are Tramp files.
    ;; We cannot invoke `with-parsed-tramp-file-name';
    ;; it fails if the file isn't a Tramp file name.
    (if t1
	(with-parsed-tramp-file-name filename l
	  (setq v1-multi-method l-multi-method
		v1-method l-method
		v1-user l-user
		v1-host l-host
		v1-localname l-localname
		multi-method l-multi-method
		method (tramp-find-method
			v1-multi-method v1-method v1-user v1-host)
		user l-user
		host l-host
		copy-program (tramp-get-method-parameter
			      v1-multi-method method
			      v1-user v1-host 'tramp-copy-program)
		copy-args (tramp-get-method-parameter
				 v1-multi-method method
				 v1-user v1-host 'tramp-copy-args)))
      (setq v1-localname filename))

    (if t2
	(with-parsed-tramp-file-name newname l
	  (setq v2-multi-method l-multi-method
		v2-method l-method
		v2-user l-user
		v2-host l-host
		v2-localname l-localname
		multi-method l-multi-method
		method (tramp-find-method
			v2-multi-method v2-method v2-user v2-host)
		user l-user
		host l-host
		copy-program (tramp-get-method-parameter
			      v2-multi-method method
			      v2-user v2-host 'tramp-copy-program)
		copy-args (tramp-get-method-parameter
				 v2-multi-method method
				 v2-user v2-host 'tramp-copy-args)))
      (setq v2-localname newname))

    ;; The following should be changed.  We need a more general
    ;; mechanism to parse extra host args.
    (if (not t1)
	(setq source v1-localname)
      (when (string-match "\\([^#]*\\)#\\(.*\\)" v1-host)
	(setq copy-args (cons "-P" (cons (match-string 2 v1-host) copy-args)))
	(setq v1-host (match-string 1 v1-host)))
      (setq source
	     (tramp-make-copy-program-file-name
	      v1-user v1-host
	      (tramp-shell-quote-argument v1-localname))))

    (if (not t2)
	(setq target v2-localname)
      (when (string-match "\\([^#]*\\)#\\(.*\\)" v2-host)
	(setq copy-args (cons "-P" (cons (match-string 2 v2-host) copy-args)))
	(setq v2-host (match-string 1 v2-host)))
      (setq target
	     (tramp-make-copy-program-file-name
	      v2-user v2-host
	      (tramp-shell-quote-argument v2-localname))))

    ;; Handle keep-date argument
    (when keep-date
      (if t1
	  (setq copy-args
		(cons (tramp-get-method-parameter
		       v1-multi-method method
		       v1-user v1-host 'tramp-copy-keep-date-arg)
		      copy-args))
	(setq copy-args
	      (cons (tramp-get-method-parameter
		     v2-multi-method method
		     v2-user v2-host 'tramp-copy-keep-date-arg)
		    copy-args))))

    (setq copy-args (append copy-args (list source target))
	  trampbuf (generate-new-buffer
		    (tramp-buffer-name multi-method method user host)))

    ;; Use an asynchronous process.  By this, password can be handled.
    (save-excursion

      ;; Check for program.
      (when (and (fboundp 'executable-find)
		 (not (executable-find copy-program)))
	(error "Cannot find copy program: %s" copy-program))

      (set-buffer trampbuf)
      (setq tramp-current-multi-method multi-method
	    tramp-current-method method
	    tramp-current-user user
	    tramp-current-host host)
      (message "Transferring %s to %s..." filename newname)

      ;; Use rcp-like program for file transfer.
      (let ((p (apply 'start-process (buffer-name trampbuf) trampbuf
		      copy-program copy-args)))
	(tramp-set-process-query-on-exit-flag p nil)
	(tramp-process-actions p multi-method method user host
			       tramp-actions-copy-out-of-band))
      (kill-buffer trampbuf)
      (message "Transferring %s to %s...done" filename newname)

      ;; Set the mode.
      (unless keep-date
	(set-file-modes newname (file-modes filename))))

    ;; If the operation was `rename', delete the original file.
    (unless (eq op 'copy)
      (delete-file filename))))

;; mkdir
(defun tramp-handle-make-directory (dir &optional parents)
  "Like `make-directory' for tramp files."
  (setq dir (expand-file-name dir))
  (with-parsed-tramp-file-name dir nil
    (save-excursion
      (tramp-barf-unless-okay
       multi-method method user host
       (format " %s %s"
	       (if parents "mkdir -p" "mkdir")
	       (tramp-shell-quote-argument localname))
       nil 'file-error
       "Couldn't make directory %s" dir))))

;; CCC error checking?
(defun tramp-handle-delete-directory (directory)
  "Like `delete-directory' for tramp files."
  (setq directory (expand-file-name directory))
  (with-parsed-tramp-file-name directory nil
    (save-excursion
      (tramp-send-command
       multi-method method user host
       (format "rmdir %s ; echo ok"
	       (tramp-shell-quote-argument localname)))
      (tramp-wait-for-output))))

(defun tramp-handle-delete-file (filename)
  "Like `delete-file' for tramp files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (save-excursion
      (unless (zerop (tramp-send-command-and-check
		      multi-method method user host
		      (format "rm -f %s"
			      (tramp-shell-quote-argument localname))))
	(signal 'file-error "Couldn't delete Tramp file")))))

;; Dired.

;; CCC: This does not seem to be enough. Something dies when
;;      we try and delete two directories under TRAMP :/
(defun tramp-handle-dired-recursive-delete-directory (filename)
  "Recursively delete the directory given.
This is like `dired-recursive-delete-directory' for tramp files."
  (with-parsed-tramp-file-name filename nil
    ;; run a shell command 'rm -r <localname>'
    ;; Code shamelessly stolen for the dired implementation and, um, hacked :)
    (or (file-exists-p filename)
	(signal
	 'file-error
	 (list "Removing old file name" "no such directory" filename)))
    ;; Which is better, -r or -R? (-r works for me <daniel@danann.net>)
    (tramp-send-command multi-method method user host
			(format "rm -r %s" (tramp-shell-quote-argument localname)))
    ;; Wait for the remote system to return to us...
    ;; This might take a while, allow it plenty of time.
    (tramp-wait-for-output 120)
    ;; Make sure that it worked...
    (and (file-exists-p filename)
	 (error "Failed to recursively delete %s" filename))))

(defun tramp-handle-dired-call-process (program discard &rest arguments)
  "Like `dired-call-process' for tramp files."
  (with-parsed-tramp-file-name default-directory nil
    (save-excursion
      (tramp-barf-unless-okay
       multi-method method user host
       (format "cd %s" (tramp-shell-quote-argument localname))
       nil 'file-error
       "tramp-handle-dired-call-process: Couldn't `cd %s'"
       (tramp-shell-quote-argument localname))
      (tramp-send-command
       multi-method method user host
       (mapconcat #'tramp-shell-quote-argument (cons program arguments) " "))
      (tramp-wait-for-output))
    (unless discard
      ;; We cannot use `insert-buffer' because the tramp buffer
      ;; changes its contents before insertion due to calling
      ;; `expand-file' and alike.
      (insert
       (with-current-buffer
	   (tramp-get-buffer multi-method method user host)
	 (buffer-string))))
    (save-excursion
      (prog1
	  (tramp-send-command-and-check multi-method method user host nil)
	(tramp-send-command multi-method method user host "cd")
	(tramp-wait-for-output)))))

(defun tramp-handle-dired-compress-file (file &rest ok-flag)
  "Like `dired-compress-file' for tramp files."
  ;; OK-FLAG is valid for XEmacs only, but not implemented.
  ;; Code stolen mainly from dired-aux.el.
  (with-parsed-tramp-file-name file nil
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
	       (message "Uncompressing %s..." file)
	       (when (zerop (tramp-send-command-and-check
			     multi-method method user host
			     (concat (nth 2 suffix) " " localname)))
		 (message "Uncompressing %s...done" file)
		 ;; `dired-remove-file' is not defined in XEmacs
		 (funcall (symbol-function 'dired-remove-file) file)
		 (string-match (car suffix) file)
		 (concat (substring file 0 (match-beginning 0)))))
	      (t
	       ;; We don't recognize the file as compressed, so compress it.
	       ;; Try gzip.
	       (message "Compressing %s..." file)
	       (when (zerop (tramp-send-command-and-check
			     multi-method method user host
			     (concat "gzip -f " localname)))
		 (message "Compressing %s...done" file)
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
  "Like `insert-directory' for tramp files."
  (if (and (boundp 'ls-lisp-use-insert-directory-program)
           (not (symbol-value 'ls-lisp-use-insert-directory-program)))
      (tramp-run-real-handler 'insert-directory
                              (list filename switches wildcard full-directory-p))
    ;; For the moment, we assume that the remote "ls" program does not
    ;; grok "--dired".  In the future, we should detect this on
    ;; connection setup.
    (when (string-match "^--dired\\s-+" switches)
      (setq switches (replace-match "" nil t switches)))
    (setq filename (expand-file-name filename))
    (with-parsed-tramp-file-name filename nil
      (tramp-message-for-buffer
       multi-method method user host 10
       "Inserting directory `ls %s %s', wildcard %s, fulldir %s"
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
      (save-excursion
        ;; If `full-directory-p', we just say `ls -l FILENAME'.
        ;; Else we chdir to the parent directory, then say `ls -ld BASENAME'.
        (if full-directory-p
            (tramp-send-command
             multi-method method user host
             (format "%s %s %s"
                     (tramp-get-ls-command multi-method method user host)
                     switches
                     (if wildcard
                         localname
                       (tramp-shell-quote-argument (concat localname ".")))))
          (tramp-barf-unless-okay
           multi-method method user host
           (format "cd %s" (tramp-shell-quote-argument
                            (file-name-directory localname)))
           nil 'file-error
           "Couldn't `cd %s'"
           (tramp-shell-quote-argument (file-name-directory localname)))
          (tramp-send-command
           multi-method method user host
           (format "%s %s %s"
                   (tramp-get-ls-command multi-method method user host)
                   switches
                   (if wildcard
                       localname
		     (if (zerop (length (file-name-nondirectory localname)))
			 ""
		       (tramp-shell-quote-argument
			(file-name-nondirectory localname)))))))
        (sit-for 1)			;needed for rsh but not ssh?
        (tramp-wait-for-output))
      ;; The following let-binding is used by code that's commented
      ;; out.  Let's leave the let-binding in for a while to see
      ;; that the commented-out code is really not needed.  Commenting-out
      ;; happened on 2003-03-13.
      (let ((old-pos (point)))
	;; We cannot use `insert-buffer' because the tramp buffer
	;; changes its contents before insertion due to calling
	;; `expand-file' and alike.
	(insert
	 (with-current-buffer
	     (tramp-get-buffer multi-method method user host)
	   (buffer-string)))
        ;; On XEmacs, we want to call (exchange-point-and-mark t), but
        ;; that doesn't exist on Emacs, so we use this workaround instead.
        ;; Since zmacs-region-stays doesn't exist in Emacs, this ought to
        ;; be safe.  Thanks to Daniel Pittman <daniel@danann.net>.
        ;;     (let ((zmacs-region-stays t))
        ;;       (exchange-point-and-mark))
        (save-excursion
          (tramp-send-command multi-method method user host "cd")
          (tramp-wait-for-output))
        ;; For the time being, the XEmacs kludge is commented out.
        ;; Please test it on various XEmacs versions to see if it works.
        ;;       ;; Another XEmacs specialty follows.  What's the right way to do
        ;;       ;; it?
        ;;       (when (and (featurep 'xemacs)
        ;; 		 (eq major-mode 'dired-mode))
        ;; 	(save-excursion
        ;; 	  (require 'dired)
        ;; 	  (dired-insert-set-properties old-pos (point))))
        ))))

;; Continuation of kluge to pacify byte-compiler.
;;(eval-when-compile
;;  (when (eq (symbol-function 'dired-insert-set-properties) 'ignore)
;;    (fmakunbound 'dired-insert-set-properties)))

;; CCC is this the right thing to do?
(defun tramp-handle-unhandled-file-name-directory (filename)
  "Like `unhandled-file-name-directory' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (expand-file-name "~/")))

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
  "Like `expand-file-name' for tramp files.
If the localname part of the given filename starts with \"/../\" then
the result will be a local, non-Tramp, filename."
  ;; If DIR is not given, use DEFAULT-DIRECTORY or "/".
  (setq dir (or dir default-directory "/"))
  ;; Unless NAME is absolute, concat DIR and NAME.
  (unless (file-name-absolute-p name)
    (setq name (concat (file-name-as-directory dir) name)))
  ;; If NAME is not a tramp file, run the real handler
  (if (not (tramp-tramp-file-p name))
      (tramp-run-real-handler 'expand-file-name
                              (list name nil))
    ;; Dissect NAME.
    (with-parsed-tramp-file-name name nil
      (unless (file-name-absolute-p localname)
	(setq localname (concat "~/" localname)))
      (save-excursion
	;; Tilde expansion if necessary.  This needs a shell which
	;; groks tilde expansion!  The function `tramp-find-shell' is
	;; supposed to find such a shell on the remote host.  Please
	;; tell me about it when this doesn't work on your system.
	(when (string-match "\\`\\(~[^/]*\\)\\(.*\\)\\'" localname)
	  (let ((uname (match-string 1 localname))
		(fname (match-string 2 localname)))
	    ;; CCC fanatic error checking?
	    (set-buffer (tramp-get-buffer multi-method method user host))
	    (erase-buffer)
	    (tramp-send-command
	     multi-method method user host
	     (format "cd %s; pwd" uname)
	     t)
	    (tramp-wait-for-output)
	    (goto-char (point-min))
	    (setq uname (buffer-substring (point) (tramp-line-end-position)))
	    (setq localname (concat uname fname))
	    (erase-buffer)))
	;; No tilde characters in file name, do normal
	;; expand-file-name (this does "/./" and "/../").  We bind
	;; directory-sep-char here for XEmacs on Windows, which
	;; would otherwise use backslash.
	(tramp-let-maybe directory-sep-char ?/
	  (tramp-make-tramp-file-name
	   multi-method (or method (tramp-find-default-method user host))
	   user host
	   (tramp-drop-volume-letter
	    (tramp-run-real-handler 'expand-file-name
				    (list localname)))))))))

;; old version follows.  it uses ".." to cross file handler
;; boundaries.
;; 	;; Look if localname starts with "/../" construct.  If this is
;; 	;; the case, then we return a local name instead of a remote name.
;; 	(if (string-match "^/\\.\\./" localname)
;; 	    (expand-file-name (substring localname 3))
;; 	  ;; No tilde characters in file name, do normal
;; 	  ;; expand-file-name (this does "/./" and "/../").  We bind
;; 	  ;; directory-sep-char here for XEmacs on Windows, which
;; 	  ;; would otherwise use backslash.
;; 	  (let ((directory-sep-char ?/))
;; 	    (tramp-make-tramp-file-name
;; 	     multi-method method user host
;; 	     (tramp-drop-volume-letter
;; 	      (tramp-run-real-handler 'expand-file-name
;; 				      (list localname))))))))))

;; Remote commands.

(defvar tramp-async-proc nil
  "Global variable keeping asynchronous process object.
Used in `tramp-handle-shell-command'")

(defun tramp-handle-shell-command (command &optional output-buffer error-buffer)
  "Like `shell-command' for tramp files.
This will break if COMMAND prints a newline, followed by the value of
`tramp-end-of-output', followed by another newline."
  ;; Asynchronous processes are far from being perfect.  But it works at least
  ;; for `find-grep-dired' and `find-name-dired' in Emacs 22.
  (if (tramp-tramp-file-p default-directory)
      (with-parsed-tramp-file-name default-directory nil
	(let ((asynchronous (string-match "[ \t]*&[ \t]*\\'" command))
	      status)
	  (unless output-buffer
	    (setq output-buffer
		  (get-buffer-create
		   (if asynchronous
		       "*Async Shell Command*"
		     "*Shell Command Output*")))
	    (set-buffer output-buffer)
	    (erase-buffer))
	  (unless (bufferp output-buffer)
	    (setq output-buffer (current-buffer)))
	  (set-buffer output-buffer)
	  ;; Tramp doesn't handle the asynchronous case by an asynchronous
	  ;; process.  Instead of, another asynchronous process is opened
	  ;; which gets the output of the (synchronous) Tramp process
	  ;; via process-filter.  ERROR-BUFFER is disabled.
	  (when asynchronous
	    (setq command (substring command 0 (match-beginning 0))
		  error-buffer nil
		  tramp-async-proc (start-process (buffer-name output-buffer)
						  output-buffer "cat")))
	  (save-excursion
	    (tramp-barf-unless-okay
	     multi-method method user host
	     (format "cd %s" (tramp-shell-quote-argument localname))
	     nil 'file-error
	     "tramp-handle-shell-command: Couldn't `cd %s'"
	     (tramp-shell-quote-argument localname))
	    ;; Define the process filter
	    (when asynchronous
	      (set-process-filter
	       (get-buffer-process
		(tramp-get-buffer multi-method method user host))
	       '(lambda (process string)
		  ;; Write the output into the Tramp Process
		  (save-current-buffer
		    (set-buffer (process-buffer process))
		    (goto-char (point-max))
		    (insert string))
		  ;; Hand-over output to asynchronous process.
		  (let ((end
			 (string-match
			  (regexp-quote tramp-end-of-output) string)))
		    (when end
		      (setq string
			    (substring string 0 (1- (match-beginning 0)))))
		    (process-send-string tramp-async-proc string)
		    (when end
		      (set-process-filter process nil)
		      (process-send-eof tramp-async-proc))))))
	    ;; Send the command
	    (tramp-send-command
	     multi-method method user host
	     (if error-buffer
		 (format "( %s ) 2>/tmp/tramp.$$.err; tramp_old_status=$?"
			 command)
	       (format "%s; tramp_old_status=$?" command)))
	    (unless asynchronous
	      (tramp-wait-for-output)))
	  (unless asynchronous
	    ;; We cannot use `insert-buffer' because the tramp buffer
	    ;; changes its contents before insertion due to calling
	    ;; `expand-file' and alike.
	    (insert
	     (with-current-buffer
		 (tramp-get-buffer multi-method method user host)
	       (buffer-string))))
	  (when error-buffer
	    (save-excursion
	      (unless (bufferp error-buffer)
		(setq error-buffer (get-buffer-create error-buffer)))
	      (tramp-send-command
	       multi-method method user host
	       "cat /tmp/tramp.$$.err")
	      (tramp-wait-for-output)
	      (set-buffer error-buffer)
	      ;; Same comment as above
	      (insert
	       (with-current-buffer
		   (tramp-get-buffer multi-method method user host)
		 (buffer-string)))
	      (tramp-send-command-and-check
	       multi-method method user host "rm -f /tmp/tramp.$$.err")))
	  (save-excursion
	    (tramp-send-command multi-method method user host "cd")
	    (unless asynchronous
	      (tramp-wait-for-output))
	    (tramp-send-command
	     multi-method method user host
	     (concat "tramp_set_exit_status $tramp_old_status;"
		     " echo tramp_exit_status $?"))
	    (unless asynchronous
	      (tramp-wait-for-output)
	      (goto-char (point-max))
	      (unless (search-backward "tramp_exit_status " nil t)
		(error "Couldn't find exit status of `%s'" command))
	      (skip-chars-forward "^ ")
	      (setq status (read (current-buffer)))))
	  (unless (zerop (buffer-size))
	    (display-buffer output-buffer))
	  status))
    ;; The following is only executed if something strange was
    ;; happening.  Emit a helpful message and do it anyway.
    (message "tramp-handle-shell-command called with non-tramp directory: `%s'"
	     default-directory)
    (tramp-run-real-handler 'shell-command
			    (list command output-buffer error-buffer))))

(defun tramp-handle-process-file (program &optional infile buffer display &rest args)
  "Like `process-file' for Tramp files."
  (when infile (error "Implementation does not handle input from file"))
  (when (and (numberp buffer) (zerop buffer))
    (error "Implementation does not handle immediate return"))
  (when (consp buffer) (error "Implementation does not handle error files"))
  (shell-command
   (mapconcat 'tramp-shell-quote-argument
              (cons program args)
              " ")
   buffer))

;; File Editing.

(defsubst tramp-make-temp-file ()
  (funcall (if (fboundp 'make-temp-file) 'make-temp-file 'make-temp-name)
	   (expand-file-name tramp-temp-name-prefix
			     (tramp-temporary-file-directory))))

(defun tramp-handle-file-local-copy (filename)
  "Like `file-local-copy' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (let ((tramp-buf (tramp-get-buffer multi-method method user host))
	  ;; We used to bind the following as late as possible.
	  ;; loc-enc and loc-dec were bound directly before the if
	  ;; statement that checks them.  But the functions
	  ;; tramp-get-* might invoke the "are you awake" check in
	  ;; tramp-maybe-open-connection, which is an unfortunate time
	  ;; since we rely on the buffer contents at that spot.
	  (rem-enc (tramp-get-remote-encoding multi-method method user host))
	  (rem-dec (tramp-get-remote-decoding multi-method method user host))
	  (loc-enc (tramp-get-local-encoding multi-method method user host))
	  (loc-dec (tramp-get-local-decoding multi-method method user host))
	  tmpfil)
      (unless (file-exists-p filename)
	(error "Cannot make local copy of non-existing file `%s'"
	       filename))
      (setq tmpfil (tramp-make-temp-file))

      (cond ((tramp-method-out-of-band-p multi-method method user host)
	     ;; `copy-file' handles out-of-band methods
	     (copy-file filename tmpfil t t))

	    ((and rem-enc rem-dec)
	     ;; Use inline encoding for file transfer.
	     (save-excursion
	       ;; Following line for setting tramp-current-method,
	       ;; tramp-current-user, tramp-current-host.
	       (set-buffer tramp-buf)
	       (tramp-message 5 "Encoding remote file %s..." filename)
	       (tramp-barf-unless-okay
		multi-method method user host
		(concat rem-enc " < " (tramp-shell-quote-argument localname))
		nil 'file-error
		"Encoding remote file failed, see buffer `%s' for details"
		tramp-buf)
	       ;; Remove trailing status code
	       (goto-char (point-max))
	       (delete-region (point) (progn (forward-line -1) (point)))

	       (tramp-message 5 "Decoding remote file %s..." filename)

	       ;; Here is where loc-enc and loc-dec used to be let-bound.
	       (if (and (symbolp loc-dec) (fboundp loc-dec))
		   ;; If local decoding is a function, we call it.
		   (let ((tmpbuf (get-buffer-create " *tramp tmp*")))
		     (set-buffer tmpbuf)
		     (erase-buffer)
		     (insert-buffer-substring tramp-buf)
		     (tramp-message-for-buffer
		      multi-method method user host
		      6 "Decoding remote file %s with function %s..."
		      filename loc-dec)
		     (set-buffer tmpbuf)
		     ;; Douglas Gray Stephens <DGrayStephens@slb.com>
		     ;; says that we need to strip tramp_exit_status
		     ;; line from the output here.  Go to point-max,
		     ;; search backward for tramp_exit_status, delete
		     ;; between point and point-max if found.
		     (let ((coding-system-for-write 'binary))
		       (funcall loc-dec (point-min) (point-max))
		       (write-region (point-min) (point-max) tmpfil))
		     (kill-buffer tmpbuf))
		 ;; If tramp-decoding-function is not defined for this
		 ;; method, we invoke tramp-decoding-command instead.
		 (let ((tmpfil2 (tramp-make-temp-file)))
		   (write-region (point-min) (point-max) tmpfil2)
		   (tramp-message
		    6 "Decoding remote file %s with command %s..."
		    filename loc-dec)
		   (tramp-call-local-coding-command
		    loc-dec tmpfil2 tmpfil)
		   (delete-file tmpfil2)))
	       (tramp-message-for-buffer
		multi-method method user host
		5 "Decoding remote file %s...done" filename)
	       ;; Set proper permissions.
	       (set-file-modes tmpfil (file-modes filename))))

	    (t (error "Wrong method specification for `%s'" method)))
      tmpfil)))

(defun tramp-handle-file-remote-p (filename)
  "Like `file-remote-p' for tramp files."
  (when (tramp-tramp-file-p filename)
    (with-parsed-tramp-file-name filename nil
      (make-tramp-file-name
       :multi-method multi-method
       :method method
       :user user
       :host host
       :localname ""))))

(defun tramp-handle-insert-file-contents
  (filename &optional visit beg end replace)
  "Like `insert-file-contents' for tramp files."
  (barf-if-buffer-read-only)
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (if (not (file-exists-p filename))
	(progn
	  (when visit
	    (setq buffer-file-name filename)
	    (set-visited-file-modtime)
	    (set-buffer-modified-p nil))
	  (signal 'file-error
		  (format "File `%s' not found on remote host" filename))
	  (list (expand-file-name filename) 0))
      ;; `insert-file-contents-literally' takes care to avoid calling
      ;; jka-compr.  By let-binding inhibit-file-name-operation, we
      ;; propagate that care to the file-local-copy operation.
      (let ((local-copy
	     (let ((inhibit-file-name-operation
		    (when (eq inhibit-file-name-operation
			      'insert-file-contents)
		      'file-local-copy)))
	       (file-local-copy filename)))
	    coding-system-used result)
	(when visit
	  (setq buffer-file-name filename)
	  (set-visited-file-modtime)
	  (set-buffer-modified-p nil))
	(tramp-message-for-buffer
	 multi-method method user host
	 9 "Inserting local temp file `%s'..." local-copy)
	(setq result (insert-file-contents local-copy nil beg end replace))
	;; Now `last-coding-system-used' has right value.  Remember it.
	(when (boundp 'last-coding-system-used)
	  (setq coding-system-used (symbol-value 'last-coding-system-used)))
	(tramp-message-for-buffer
	 multi-method method user host
	 9 "Inserting local temp file `%s'...done" local-copy)
	(delete-file local-copy)
	(when (boundp 'last-coding-system-used)
	  (set 'last-coding-system-used coding-system-used))
	(list (expand-file-name filename)
	      (second result))))))


(defun tramp-handle-find-backup-file-name (filename)
  "Like `find-backup-file-name' for tramp files."
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
			  (tramp-make-tramp-file-name
			   multi-method method user host (cdr x))
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
			    multi-method method user host (car (cdr x)))
			 (car (cdr x))))
		      (cdr (cdr x))))
		  (symbol-value 'tramp-bkup-backup-directory-info))
	       (symbol-value 'bkup-backup-directory-info)))))

      (tramp-run-real-handler 'find-backup-file-name (list filename)))))

(defun tramp-handle-make-auto-save-file-name ()
  "Like `make-auto-save-file-name' for tramp files.
Returns a file name in `tramp-auto-save-directory' for autosaving this file."
  (when tramp-auto-save-directory
    (unless (file-exists-p tramp-auto-save-directory)
      (make-directory tramp-auto-save-directory t)))
  ;; jka-compr doesn't like auto-saving, so by appending "~" to the
  ;; file name we make sure that jka-compr isn't used for the
  ;; auto-save file.
  (let ((buffer-file-name
	 (if tramp-auto-save-directory
	     (expand-file-name
	      (tramp-subst-strs-in-string
	       '(("_" . "|")
		 ("/" . "_a")
		 (":" . "_b")
		 ("|" . "__")
		 ("[" . "_l")
		 ("]" . "_r"))
	       (buffer-file-name))
	      tramp-auto-save-directory)
	   (buffer-file-name))))
    ;; Run plain `make-auto-save-file-name'.  There might be an advice when
    ;; it is not a magic file name operation (since Emacs 22).
    ;; We must deactivate it temporarily.
    (if (not (ad-is-active 'make-auto-save-file-name))
	(tramp-run-real-handler
	 'make-auto-save-file-name nil)
      ;; else
      (ad-deactivate 'make-auto-save-file-name)
      (prog1
       (tramp-run-real-handler
	'make-auto-save-file-name nil)
       (ad-activate 'make-auto-save-file-name)))))


;; CCC grok APPEND, LOCKNAME, CONFIRM
(defun tramp-handle-write-region
  (start end filename &optional append visit lockname confirm)
  "Like `write-region' for tramp files."
  (unless (eq append nil)
    (error "Cannot append to file using tramp (`%s')" filename))
  (setq filename (expand-file-name filename))
  ;; Following part commented out because we don't know what to do about
  ;; file locking, and it does not appear to be a problem to ignore it.
  ;; Ange-ftp ignores it, too.
  ;;  (when (and lockname (stringp lockname))
  ;;    (setq lockname (expand-file-name lockname)))
  ;;  (unless (or (eq lockname nil)
  ;;              (string= lockname filename))
  ;;    (error
  ;;     "tramp-handle-write-region: LOCKNAME must be nil or equal FILENAME"))
  ;; XEmacs takes a coding system as the seventh argument, not `confirm'
  (when (and (not (featurep 'xemacs))
	     confirm (file-exists-p filename))
    (unless (y-or-n-p (format "File %s exists; overwrite anyway? "
                              filename))
      (error "File not overwritten")))
  (with-parsed-tramp-file-name filename nil
    (let ((curbuf (current-buffer))
	  (rem-enc (tramp-get-remote-encoding multi-method method user host))
	  (rem-dec (tramp-get-remote-decoding multi-method method user host))
	  (loc-enc (tramp-get-local-encoding multi-method method user host))
	  (loc-dec (tramp-get-local-decoding multi-method method user host))
	  (trampbuf (get-buffer-create "*tramp output*"))
	  (modes (file-modes filename))
	  ;; We use this to save the value of `last-coding-system-used'
	  ;; after writing the tmp file.  At the end of the function,
	  ;; we set `last-coding-system-used' to this saved value.
	  ;; This way, any intermediary coding systems used while
	  ;; talking to the remote shell or suchlike won't hose this
	  ;; variable.  This approach was snarfed from ange-ftp.el.
	  coding-system-used
	  tmpfil)
      ;; Write region into a tmp file.  This isn't really needed if we
      ;; use an encoding function, but currently we use it always
      ;; because this makes the logic simpler.
      (setq tmpfil (tramp-make-temp-file))
      ;; Set current buffer.  If connection wasn't open, `file-modes' has
      ;; changed it accidently.
      (set-buffer curbuf)
      ;; We say `no-message' here because we don't want the visited file
      ;; modtime data to be clobbered from the temp file.  We call
      ;; `set-visited-file-modtime' ourselves later on.
      (tramp-run-real-handler
       'write-region
       (if confirm ; don't pass this arg unless defined for backward compat.
	   (list start end tmpfil append 'no-message lockname confirm)
	 (list start end tmpfil append 'no-message lockname)))
      ;; Now, `last-coding-system-used' has the right value.  Remember it.
      (when (boundp 'last-coding-system-used)
	(setq coding-system-used (symbol-value 'last-coding-system-used)))
      ;; The permissions of the temporary file should be set.  If
      ;; filename does not exist (eq modes nil) it has been renamed to
      ;; the backup file.  This case `save-buffer' handles
      ;; permissions.
      (when modes (set-file-modes tmpfil modes))
      ;; This is a bit lengthy due to the different methods possible for
      ;; file transfer.  First, we check whether the method uses an rcp
      ;; program.  If so, we call it.  Otherwise, both encoding and
      ;; decoding command must be specified.  However, if the method
      ;; _also_ specifies an encoding function, then that is used for
      ;; encoding the contents of the tmp file.
      (cond ((tramp-method-out-of-band-p multi-method method user host)
	     ;; `copy-file' handles out-of-band methods
	     (copy-file tmpfil filename t t))

	    ((and rem-enc rem-dec)
	     ;; Use inline file transfer
	     (let ((tmpbuf (get-buffer-create " *tramp file transfer*")))
	       (save-excursion
		 ;; Encode tmpfil into tmpbuf
		 (tramp-message-for-buffer multi-method method user host
					   5 "Encoding region...")
		 (set-buffer tmpbuf)
		 (erase-buffer)
		 ;; Use encoding function or command.
		 (if (and (symbolp loc-enc) (fboundp loc-enc))
		     (progn
		       (tramp-message-for-buffer
			multi-method method user host
			6 "Encoding region using function `%s'..."
			(symbol-name loc-enc))
		       (insert-file-contents-literally tmpfil)
		       ;; CCC.  The following `let' is a workaround for
		       ;; the base64.el that comes with pgnus-0.84.  If
		       ;; both of the following conditions are
		       ;; satisfied, it tries to write to a local file
		       ;; in default-directory, but at this point,
		       ;; default-directory is remote.
		       ;; (CALL-PROCESS-REGION can't write to remote
		       ;; files, it seems.)  The file in question is a
		       ;; tmp file anyway.
		       (let ((default-directory
			       (tramp-temporary-file-directory)))
			 (funcall loc-enc (point-min) (point-max)))
		       (goto-char (point-max))
		       (unless (bolp)
			 (newline)))
		   (tramp-message-for-buffer
		    multi-method method user host
		    6 "Encoding region using command `%s'..." loc-enc)
		   (unless (equal 0 (tramp-call-local-coding-command
				     loc-enc tmpfil t))
		     (pop-to-buffer trampbuf)
		     (error (concat "Cannot write to `%s', local encoding"
				    " command `%s' failed")
			    filename loc-enc)))
		 ;; Send tmpbuf into remote decoding command which
		 ;; writes to remote file.  Because this happens on the
		 ;; remote host, we cannot use the function.
		 (tramp-message-for-buffer
		  multi-method method user host
		  5 "Decoding region into remote file %s..." filename)
		 (tramp-send-command
		  multi-method method user host
		  (format "%s >%s <<'EOF'"
			  rem-dec
			  (tramp-shell-quote-argument localname)))
		 (set-buffer tmpbuf)
		 (tramp-message-for-buffer
		  multi-method method user host
		  6 "Sending data to remote host...")
		 (tramp-send-string multi-method method user host
				    (buffer-string))
		 ;; wait for remote decoding to complete
		 (tramp-message-for-buffer
		  multi-method method user host
		  6 "Sending end of data token...")
		 (tramp-send-command
		  multi-method method user host "EOF" nil t)
		 (tramp-message-for-buffer
		  multi-method method user host 6
		  "Waiting for remote host to process data...")
		 (set-buffer (tramp-get-buffer multi-method method user host))
		 (tramp-wait-for-output)
		 (tramp-barf-unless-okay
		  multi-method method user host nil nil 'file-error
		  (concat "Couldn't write region to `%s',"
			  " decode using `%s' failed")
		  filename rem-dec)
		 (tramp-message 5 "Decoding region into remote file %s...done"
				filename)
		 (kill-buffer tmpbuf))))
	    (t
	     (error
	      (concat "Method `%s' should specify both encoding and "
		      "decoding command or an rcp program")
	      method)))
      (delete-file tmpfil)
      (unless (equal curbuf (current-buffer))
	(error "Buffer has changed from `%s' to `%s'"
	       curbuf (current-buffer)))
      (when (or (eq visit t) (stringp visit))
	(set-visited-file-modtime
	 ;; We must pass modtime explicitely, because filename can be different
	 ;; from (buffer-file-name), f.e. if `file-precious-flag' is set.
	 (nth 5 (file-attributes filename))))
      ;; Make `last-coding-system-used' have the right value.
      (when (boundp 'last-coding-system-used)
	(set 'last-coding-system-used coding-system-used))
      (when (or (eq visit t)
		(eq visit nil)
		(stringp visit))
	(message "Wrote %s" filename)))))

;; Call down to the real handler.
;; Because EFS does not play nicely with TRAMP (both systems match a
;; TRAMP file name) it is needed to disable efs as well as tramp for the
;; operation.
;;
;; Other than that, this is the canon file-handler code that the doco
;; says should be used here. Which is nice.
;;
;; Under XEmacs current, EFS also hooks in as
;; efs-sifn-handler-function to handle any filename with environment
;; variables. This has two implications:
;; 1) That EFS may not be completely dead (yet) for TRAMP filenames
;; 2) That TRAMP might want to do the same thing.
;; Details as they come in.
;;
;; Daniel Pittman <daniel@danann.net>

;; (defun tramp-run-real-handler (operation args)
;;   "Invoke normal file name handler for OPERATION.
;; This inhibits EFS and Ange-FTP, too, because they conflict with tramp.
;; First arg specifies the OPERATION, remaining ARGS are passed to the
;; OPERATION."
;;   (let ((inhibit-file-name-handlers
;;          (list 'tramp-file-name-handler
;; 	       'efs-file-handler-function
;;                'ange-ftp-hook-function
;;                (and (eq inhibit-file-name-operation operation)
;;                     inhibit-file-name-handlers)))
;;         (inhibit-file-name-operation operation))
;;     (apply operation args)))

(defun tramp-run-real-handler (operation args)
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
    (apply operation args)))

;; This function is used from `tramp-completion-file-name-handler' functions
;; only, if `tramp-completion-mode' is true. But this cannot be checked here
;; because the check is based on a full filename, not available for all
;; basic I/O operations.
(defun tramp-completion-run-real-handler (operation args)
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
    (apply operation args)))

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
	    (list 'make-auto-save-file-name
	          'set-visited-file-modtime 'verify-visited-file-modtime
		  ; XEmacs only
		  'backup-buffer))
    (buffer-file-name
     (if (bufferp (nth 0 args)) (nth 0 args) (current-buffer))))
   ; COMMAND
   ((member operation
	    (list 'dired-call-process
                  ; Emacs only
		  'shell-command
                  ; Emacs 22 only
                  'process-file
	          ; XEmacs only
		  'dired-print-file 'dired-shell-call-process))
    default-directory)
   ; unknown file primitive
   (t (error "unknown file I/O primitive: %s" operation))))

(defun tramp-find-foreign-file-name-handler (filename)
  "Return foreign file name handler if exists."
  (when (tramp-tramp-file-p filename)
    (let (elt
	  res
	  (handler-alist tramp-foreign-file-name-handler-alist))
      (while handler-alist
	(setq elt (car handler-alist)
	      handler-alist (cdr handler-alist))
	(when (funcall (car elt) filename)
	  (setq handler-alist nil)
	  (setq res (cdr elt))))
      res)))

;; Main function.
;;;###autoload
(defun tramp-file-name-handler (operation &rest args)
  "Invoke Tramp file name handler.
Falls back to normal file name handler if no tramp file name handler exists."
  (save-match-data
    (let* ((filename (apply 'tramp-file-name-for-operation operation args))
	   (foreign (tramp-find-foreign-file-name-handler filename)))
      (cond
       (foreign (apply foreign operation args))
       (t (tramp-run-real-handler operation args))))))


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
    (signal 'file-error "Forbidden reentrant call of Tramp"))
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
(defun tramp-completion-file-name-handler (operation &rest args)
  "Invoke tramp file name completion handler.
Falls back to normal file name handler if no tramp file name handler exists."
;;   (setq tramp-debug-buffer t)
;;   (tramp-message 1 "%s %s" operation args)
;;   (tramp-message 1 "%s %s\n%s"
;; 		 operation args (with-output-to-string (backtrace)))
  (let ((fn (assoc operation tramp-completion-file-name-handler-alist)))
    (if fn
	(save-match-data (apply (cdr fn) args))
      (tramp-completion-run-real-handler operation args))))

;;;###autoload
(put 'tramp-completion-file-name-handler 'safe-magic t)

;; Register in file name handler alist
;;;###autoload
(add-to-list 'file-name-handler-alist
	     (cons tramp-file-name-regexp 'tramp-file-name-handler))
(add-to-list 'file-name-handler-alist
	     (cons tramp-completion-file-name-regexp
		   'tramp-completion-file-name-handler))

(defun tramp-repair-jka-compr ()
  "If jka-compr is already loaded, move it to the front of
`file-name-handler-alist'.  On Emacs 22 or so this will not be
necessary anymore."
  (let ((jka (rassoc 'jka-compr-handler file-name-handler-alist)))
    (when jka
      (setq file-name-handler-alist
	    (cons jka (delete jka file-name-handler-alist))))))
(tramp-repair-jka-compr)


;;; Interactions with other packages:

;; -- complete.el --

;; This function contributed by Ed Sabol
(defun tramp-handle-expand-many-files (name)
  "Like `PC-expand-many-files' for tramp files."
  (with-parsed-tramp-file-name name nil
    (save-match-data
      (if (or (string-match "\\*" name)
	      (string-match "\\?" name)
	      (string-match "\\[.*\\]" name))
	  (save-excursion
	    (let (bufstr)
	      ;; CCC: To do it right, we should quote certain characters
	      ;; in the file name, but since the echo command is going to
	      ;; break anyway when there are spaces in the file names, we
	      ;; don't bother.
	      ;;-(let ((comint-file-name-quote-list
	      ;;-       (set-difference tramp-file-name-quote-list
	      ;;-                       '(?\* ?\? ?[ ?]))))
	      ;;-  (tramp-send-command
	      ;;-   multi-method method user host
	      ;;-   (format "echo %s" (comint-quote-filename localname)))
	      ;;-  (tramp-wait-for-output))
	      (tramp-send-command multi-method method user host
				  (format "echo %s" localname))
	      (tramp-wait-for-output)
	      (setq bufstr (buffer-substring (point-min)
					     (tramp-line-end-position)))
	      (goto-char (point-min))
	      (if (string-equal localname bufstr)
		  nil
		(insert "(\"")
		(while (search-forward " " nil t)
		  (delete-backward-char 1)
		  (insert "\" \""))
		(goto-char (point-max))
		(delete-backward-char 1)
		(insert "\")")
		(goto-char (point-min))
		(mapcar
		 (function (lambda (x)
			     (tramp-make-tramp-file-name multi-method method
							 user host x)))
		 (read (current-buffer))))))
	(list (expand-file-name name))))))

;; Check for complete.el and override PC-expand-many-files if appropriate.
(eval-and-compile
  (defun tramp-save-PC-expand-many-files (name))); avoid compiler warning

(defun tramp-setup-complete ()
  (fset 'tramp-save-PC-expand-many-files
        (symbol-function 'PC-expand-many-files))
  (defun PC-expand-many-files (name)
    (if (tramp-tramp-file-p name)
        (funcall (symbol-function 'expand-many-files) name)
      (tramp-save-PC-expand-many-files name))))

;; Why isn't eval-after-load sufficient?
(if (fboundp 'PC-expand-many-files)
    (tramp-setup-complete)
  (eval-after-load "complete" '(tramp-setup-complete)))

;;; File name handler functions for completion mode

(defvar tramp-completion-mode nil
  "If non-nil, we are in file name completion mode.")

;; Necessary because `tramp-file-name-regexp-unified' and
;; `tramp-completion-file-name-regexp-unified' aren't different.
;; If nil, `tramp-completion-run-real-handler' is called (i.e. forwarding to
;; `tramp-file-name-handler'). Otherwise, it takes `tramp-run-real-handler'.
;; Using `last-input-event' is a little bit risky, because completing a file
;; might require loading other files, like "~/.netrc", and for them it
;; shouldn't be decided based on that variable. On the other hand, those files
;; shouldn't have partial tramp file name syntax. Maybe another variable should
;; be introduced overwriting this check in such cases. Or we change tramp
;; file name syntax in order to avoid ambiguities, like in XEmacs ...
;; In case of non unified file names it can be always true (and wouldn't be
;; necessary, because there are different regexp).
(defun tramp-completion-mode (file)
  "Checks whether method / user name / host name completion is active."
  (cond
   (tramp-completion-mode t)
   ((not tramp-unified-filenames) t)
   ((string-match "^/.*:.*:$" file) nil)
   ((string-match
     (concat tramp-prefix-regexp
      "\\(" tramp-method-regexp  "\\)" tramp-postfix-single-method-regexp "$")
     file)
    (member (match-string 1 file) (mapcar 'car tramp-methods)))
   ((or (equal last-input-event 'tab)
	;; Emacs
	(and (integerp last-input-event)
	     (not (event-modifiers last-input-event))
	     (or (char-equal last-input-event ?\?)
		 (char-equal last-input-event ?\t) ; handled by 'tab already?
		 (char-equal last-input-event ?\ )))
	;; XEmacs
	(and (featurep 'xemacs)
	     (not (event-modifiers last-input-event))
	     (or (char-equal
		  (funcall (symbol-function 'event-to-character)
			   last-input-event) ?\?)
		 (char-equal
		  (funcall (symbol-function 'event-to-character)
			   last-input-event) ?\t)
		 (char-equal
		  (funcall (symbol-function 'event-to-character)
			   last-input-event) ?\ ))))
    t)))

(defun tramp-completion-handle-file-exists-p (filename)
  "Like `file-exists-p' for tramp files."
  (if (tramp-completion-mode filename)
      (tramp-run-real-handler
       'file-exists-p (list filename))
    (tramp-completion-run-real-handler
     'file-exists-p (list filename))))

;; Localname manipulation in case of partial TRAMP file names.
(defun tramp-completion-handle-file-name-directory (file)
  "Like `file-name-directory' but aware of TRAMP files."
  (if (tramp-completion-mode file)
      "/"
    (tramp-completion-run-real-handler
     'file-name-directory (list file))))

;; Localname manipulation in case of partial TRAMP file names.
(defun tramp-completion-handle-file-name-nondirectory (file)
  "Like `file-name-nondirectory' but aware of TRAMP files."
  (substring
   file (length (tramp-completion-handle-file-name-directory file))))

;; Method, host name and user name completion.
;; `tramp-completion-dissect-file-name' returns a list of
;; tramp-file-name structures. For all of them we return possible completions.
(defun tramp-completion-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for partial tramp files."

  (unwind-protect
      ;; We need to reset `tramp-completion-mode'.
      (progn
	(setq tramp-completion-mode t)
	(let*
	    ((fullname (concat directory filename))
	     ;; possible completion structures
	     (v (tramp-completion-dissect-file-name fullname))
	     result result1)

	  (while v
	    (let* ((car (car v))
		   (multi-method (tramp-file-name-multi-method car))
		   (method (tramp-file-name-method car))
		   (user (tramp-file-name-user car))
		   (host (tramp-file-name-host car))
		   (localname (tramp-file-name-localname car))
		   (m (tramp-find-method multi-method method user host))
		   (tramp-current-user user) ; see `tramp-parse-passwd'
		   all-user-hosts)

	      (unless (or multi-method ;; Not handled (yet).
			  localname)   ;; Nothing to complete

		(if (or user host)

		    ;; Method dependent user / host combinations
		    (progn
		      (mapcar
		       (lambda (x)
			 (setq all-user-hosts
			       (append all-user-hosts
				       (funcall (nth 0 x) (nth 1 x)))))
		       (tramp-get-completion-function m))

		      (setq result (append result
	                (mapcar
			 (lambda (x)
			   (tramp-get-completion-user-host
			    method user host (nth 0 x) (nth 1 x)))
			 (delq nil all-user-hosts)))))

		  ;; Possible methods
		  (setq result
			(append result (tramp-get-completion-methods m)))))

	      (setq v (cdr v))))

	  ;; unify list, remove nil elements
	  (while result
	    (let ((car (car result)))
	      (when car (add-to-list 'result1 car))
	      (setq result (cdr result))))

	  ;; Complete local parts
	  (append
	   result1
	   (condition-case nil
	       (if result1
		   ;; "/ssh:" does not need to be expanded as hostname.
		   (tramp-run-real-handler
		    'file-name-all-completions (list filename directory))
		 ;; No method/user/host found to be expanded.
		 (tramp-completion-run-real-handler
		  'file-name-all-completions (list filename directory)))
	     (error nil)))))
    ;; unwindform
    (setq tramp-completion-mode nil)))

;; Method, host name and user name completion for a file.
(defun tramp-completion-handle-file-name-completion (filename directory)
  "Like `file-name-completion' for tramp files."
  (try-completion filename
   (mapcar 'list (file-name-all-completions filename directory))))

;; I misuse a little bit the tramp-file-name structure in order to handle
;; completion possibilities for partial methods / user names / host names.
;; Return value is a list of tramp-file-name structures according to possible
;; completions. If "multi-method" or "localname" is non-nil it means there
;; shouldn't be a completion anymore.

;; Expected results:

;; "/x" "/[x"               "/x@" "/[x@"             "/x@y" "/[x@y"
;; [nil nil nil "x" nil]    [nil nil "x" nil nil]    [nil nil "x" "y" nil]
;; [nil nil "x" nil nil]
;; [nil "x" nil nil nil]

;; "/x:"                    "/x:y"                   "/x:y:"
;; [nil nil nil "x" ""]     [nil nil nil "x" "y"]    [nil "x" nil "y" ""]
;;       "/[x/"                   "/[x/y"
;; [nil "x" nil "" nil]     [nil "x" nil "y" nil]
;; [nil "x" "" nil nil]     [nil "x" "y" nil nil]

;; "/x:y@"                  "/x:y@z"                 "/x:y@z:"
;; [nil nil nil "x" "y@"]   [nil nil nil "x" "y@z"]  [nil "x" "y" "z" ""]
;;       "/[x/y@"                 "/[x/y@z"
;; [nil "x" nil "y" nil]    [nil "x" "y" "z" nil]
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
	 ;; "/method:user" "/[method/user"
	 (tramp-completion-file-name-structure5
	  (list (concat tramp-prefix-regexp
			"\\(" tramp-method-regexp "\\)" tramp-postfix-single-method-regexp
			"\\(" tramp-user-regexp x-nil   "\\)$")
		1 2 nil nil))
	 ;; "/method:host" "/[method/host"
	 (tramp-completion-file-name-structure6
	  (list (concat tramp-prefix-regexp
			"\\(" tramp-method-regexp "\\)" tramp-postfix-single-method-regexp
			"\\(" tramp-host-regexp x-nil   "\\)$")
		1 nil 2 nil))
	 ;; "/method:user@host" "/[method/user@host"
	 (tramp-completion-file-name-structure7
	  (list (concat tramp-prefix-regexp
			"\\(" tramp-method-regexp "\\)" tramp-postfix-single-method-regexp
			"\\(" tramp-user-regexp "\\)"   tramp-postfix-user-regexp
			"\\(" tramp-host-regexp x-nil   "\\)$")
		1 2 3 nil)))

    (mapcar (lambda (regexp)
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
       tramp-file-name-structure))

    (delq nil result)))

(defun tramp-completion-dissect-file-name1 (structure name)
  "Returns a `tramp-file-name' structure matching STRUCTURE.
The structure consists of multi-method, remote method, remote user,
remote host and localname (filename on remote host)."

  (let (method)
    (save-match-data
      (when (string-match (nth 0 structure) name)
	(setq method (and (nth 1 structure)
			  (match-string (nth 1 structure) name)))
	(if (and method (member method tramp-multi-methods))
	    ;; Not handled (yet).
	    (make-tramp-file-name
	     :multi-method method
	     :method nil
	     :user nil
	     :host nil
	     :localname nil)
	  (let ((user   (and (nth 2 structure)
			     (match-string (nth 2 structure) name)))
		(host   (and (nth 3 structure)
			     (match-string (nth 3 structure) name)))
		(localname   (and (nth 4 structure)
			     (match-string (nth 4 structure) name))))
	    (make-tramp-file-name
	     :multi-method nil
	     :method method
	     :user user
	     :host host
	     :localname localname)))))))

;; This function returns all possible method completions, adding the
;; trailing method delimeter.
(defun tramp-get-completion-methods (partial-method)
  "Returns all method completions for PARTIAL-METHOD."
  (mapcar
   (lambda (method)
     (and method
	  (string-match (concat "^" (regexp-quote partial-method)) method)
	  ;; we must remove leading "/".
	  (substring (tramp-make-tramp-file-name nil method nil nil nil) 1)))
   (delete "multi" (mapcar 'car tramp-methods))))

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
    ;; we must remove leading "/".
    (substring (tramp-make-tramp-file-name nil method user host nil) 1)))

(defun tramp-parse-rhosts (filename)
  "Return a list of (user host) tuples allowed to access.
Either user or host may be nil."

  (let (res)
    (when (file-readable-p filename)
      (with-temp-buffer
	(insert-file-contents filename)
	(goto-char (point-min))
	(while (not (eobp))
	  (push (tramp-parse-rhosts-group) res))))
    res))

;; Taken from gnus/netrc.el
(eval-and-compile
  (defalias 'tramp-point-at-eol
    (if (fboundp 'point-at-eol)
	'point-at-eol
      'line-end-position)))

(defun tramp-parse-rhosts-group ()
   "Return a (user host) tuple allowed to access.
Either user or host may be nil."

   (let ((result)
	 (regexp
	  (concat
	   "^\\(" tramp-host-regexp "\\)"
	   "\\([ \t]+" "\\(" tramp-user-regexp "\\)" "\\)?")))

     (narrow-to-region (point) (tramp-point-at-eol))
     (when (re-search-forward regexp nil t)
       (setq result (append (list (match-string 3) (match-string 1)))))
     (widen)
     (forward-line 1)
     result))

(defun tramp-parse-shosts (filename)
  "Return a list of (user host) tuples allowed to access.
User is always nil."

  (let (res)
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

     (narrow-to-region (point) (tramp-point-at-eol))
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

  (let (res)
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

     (narrow-to-region (point) (tramp-point-at-eol))
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

  (let ((regexp (concat "^key_[0-9]+_\\(" tramp-host-regexp "\\)\\.pub$"))
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

  (let ((regexp (concat "^\\(" tramp-host-regexp
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

  (let (res)
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

     (narrow-to-region (point) (tramp-point-at-eol))
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
;; has been typed already.  So we (mis-)use tramp-current-user as indication,
;; assuming it is set in `tramp-completion-handle-file-name-all-completions'.
(defun tramp-parse-passwd (filename)
  "Return a list of (user host) tuples allowed to access.
Host is always \"localhost\"."

  (let (res)
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

     (narrow-to-region (point) (tramp-point-at-eol))
     (when (re-search-forward regexp nil t)
       (setq result (list (match-string 1) "localhost")))
     (widen)
     (forward-line 1)
     result))

(defun tramp-parse-netrc (filename)
  "Return a list of (user host) tuples allowed to access.
User may be nil."

  (let (res)
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

     (narrow-to-region (point) (tramp-point-at-eol))
     (when (re-search-forward regexp nil t)
       (setq result (list (match-string 3) (match-string 1))))
     (widen)
     (forward-line 1)
     result))

(defun tramp-completion-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for tramp files."
  (let ((fullname (concat (or dir default-directory) name)))
    (if (tramp-completion-mode fullname)
	(tramp-run-real-handler
	 'expand-file-name (list name dir))
      (tramp-completion-run-real-handler
       'expand-file-name (list name dir)))))

;;; Internal Functions:

(defun tramp-maybe-send-perl-script (multi-method method user host script name)
  "Define in remote shell function NAME implemented as perl SCRIPT.
Only send the definition if it has not already been done.
Function may have 0-3 parameters."
  (let ((remote-perl (tramp-get-remote-perl multi-method method user host)))
    (unless remote-perl (error "No remote perl"))
    (let ((perl-scripts (tramp-get-connection-property "perl-scripts" nil
                                                       multi-method method user host)))
      (unless (memq name perl-scripts)
        (with-current-buffer (tramp-get-buffer multi-method method user host)
          (tramp-message 5 (concat "Sending the Perl script `" name "'..."))
          (tramp-send-string multi-method method user host
                             (concat name
                                     " () {\n"
                                     remote-perl
                                     " -e '"
                                     script
                                     "' \"$1\" \"$2\" \"$3\" 2>/dev/null\n}"))
          (tramp-wait-for-output)
          (tramp-set-connection-property "perl-scripts" (cons name perl-scripts)
                                         multi-method method user host)
          (tramp-message 5 (concat "Sending the Perl script `" name "'...done.")))))))

(defun tramp-set-auto-save ()
  (when (and (buffer-file-name)
             (tramp-tramp-file-p (buffer-file-name))
	     ;; ange-ftp has its own auto-save mechanism
	     (eq (tramp-find-foreign-file-name-handler (buffer-file-name))
		 'tramp-sh-file-name-handler)
             auto-save-default)
    (auto-save-mode 1)))
(add-hook 'find-file-hooks 'tramp-set-auto-save t)

(defun tramp-run-test (switch filename)
  "Run `test' on the remote system, given a SWITCH and a FILENAME.
Returns the exit code of the `test' program."
  (let ((v (tramp-dissect-file-name filename)))
    (save-excursion
      (tramp-send-command-and-check
       (tramp-file-name-multi-method v) (tramp-file-name-method v)
       (tramp-file-name-user v) (tramp-file-name-host v)
       (format "test %s %s" switch
               (tramp-shell-quote-argument (tramp-file-name-localname v)))))))

(defun tramp-run-test2 (program file1 file2 &optional switch)
  "Run `test'-like PROGRAM on the remote system, given FILE1, FILE2.
The optional SWITCH is inserted between the two files.
Returns the exit code of the `test' PROGRAM.  Barfs if the methods,
hosts, or files, disagree."
  (let* ((v1 (tramp-dissect-file-name file1))
         (v2 (tramp-dissect-file-name file2))
         (mmethod1 (tramp-file-name-multi-method v1))
         (mmethod2 (tramp-file-name-multi-method v2))
         (method1 (tramp-file-name-method v1))
         (method2 (tramp-file-name-method v2))
         (user1 (tramp-file-name-user v1))
         (user2 (tramp-file-name-user v2))
         (host1 (tramp-file-name-host v1))
         (host2 (tramp-file-name-host v2))
         (localname1 (tramp-file-name-localname v1))
         (localname2 (tramp-file-name-localname v2)))
    (unless (and method1 method2 host1 host2
                 (equal mmethod1 mmethod2)
                 (equal method1 method2)
                 (equal user1 user2)
                 (equal host1 host2))
      (error "tramp-run-test2: %s"
             "only implemented for same method, same user, same host"))
    (save-excursion
      (tramp-send-command-and-check
       mmethod1 method1 user1 host1
       (format "%s %s %s %s"
               program
               (tramp-shell-quote-argument localname1)
               (or switch "")
               (tramp-shell-quote-argument localname2))))))

(defun tramp-touch (file time)
  "Set the last-modified timestamp of the given file.
TIME is an Emacs internal time value as returned by `current-time'."
  (let ((touch-time (format-time-string "%Y%m%d%H%M.%S" time)))
    (if (tramp-tramp-file-p file)
	(with-parsed-tramp-file-name file nil
	  (let ((buf (tramp-get-buffer multi-method method user host)))
	    (unless (zerop (tramp-send-command-and-check
			    multi-method method user host
			    (format "touch -t %s %s"
				    touch-time
				    localname)))
	      (pop-to-buffer buf)
	      (error "tramp-touch: touch failed, see buffer `%s' for details"
		     buf))))
      ;; It's a local file
      (with-temp-buffer
	(unless (zerop (call-process
			"touch" nil (current-buffer) nil "-t" touch-time file))
	      (pop-to-buffer (current-buffer))
	      (error "tramp-touch: touch failed"))))))

(defun tramp-buffer-name (multi-method method user host)
  "A name for the connection buffer for USER at HOST using METHOD."
  (if multi-method
      (tramp-buffer-name-multi-method "tramp" multi-method method user host)
    (let ((method (tramp-find-method multi-method method user host)))
      (if user
	  (format "*tramp/%s %s@%s*" method user host)
	(format "*tramp/%s %s*" method host)))))

(defun tramp-buffer-name-multi-method (prefix multi-method method user host)
  "A name for the multi method connection buffer.
MULTI-METHOD gives the multi method, METHOD the array of methods,
USER the array of user names, HOST the array of host names."
  (unless (and (= (length method) (length user))
               (= (length method) (length host)))
    (error "Syntax error in multi method (implementation error)"))
  (let ((len (length method))
        (i 0)
        string-list)
    (while (< i len)
      (setq string-list
            (cons (if (aref user i)
                      (format "%s#%s@%s:" (aref method i)
                              (aref user i) (aref host i))
                    (format "%s@%s:" (aref method i) (aref host i)))
                  string-list))
      (incf i))
    (format "*%s/%s %s*"
            prefix multi-method
            (apply 'concat (reverse string-list)))))

(defun tramp-get-buffer (multi-method method user host)
  "Get the connection buffer to be used for USER at HOST using METHOD."
  (with-current-buffer
      (get-buffer-create (tramp-buffer-name multi-method method user host))
    (setq buffer-undo-list t)
    (current-buffer)))

(defun tramp-debug-buffer-name (multi-method method user host)
  "A name for the debug buffer for USER at HOST using METHOD."
  (if multi-method
      (tramp-buffer-name-multi-method "debug tramp"
				      multi-method method user host)
    (let ((method (tramp-find-method multi-method method user host)))
      (if user
	  (format "*debug tramp/%s %s@%s*" method user host)
	(format "*debug tramp/%s %s*" method host)))))

(defun tramp-get-debug-buffer (multi-method method user host)
  "Get the debug buffer for USER at HOST using METHOD."
  (with-current-buffer
      (get-buffer-create
       (tramp-debug-buffer-name multi-method method user host))
    (setq buffer-undo-list t)
    (current-buffer)))

(defun tramp-find-executable (multi-method method user host
                                         progname dirlist ignore-tilde)
  "Searches for PROGNAME in all directories mentioned in DIRLIST.
First args METHOD, USER and HOST specify the connection, PROGNAME
is the program to search for, and DIRLIST gives the list of directories
to search.  If IGNORE-TILDE is non-nil, directory names starting
with `~' will be ignored.

Returns the absolute file name of PROGNAME, if found, and nil otherwise.

This function expects to be in the right *tramp* buffer."
  (let (result)
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
     multi-method method user host
     (format (concat "while read d; "
                     "do if test -x $d/%s -a -f $d/%s; "
                     "then echo tramp_executable $d/%s; "
                     "break; fi; done <<'EOF'")
             progname progname progname))
    (mapcar (lambda (d)
              (tramp-send-command multi-method method user host d))
            dirlist)
    (tramp-send-command multi-method method user host "EOF")
    (tramp-wait-for-output)
    (goto-char (point-max))
    (when (search-backward "tramp_executable " nil t)
      (skip-chars-forward "^ ")
      (skip-chars-forward " ")
      (buffer-substring (point) (tramp-line-end-position)))))

(defun tramp-set-remote-path (multi-method method user host var dirlist)
  "Sets the remote environment VAR to existing directories from DIRLIST.
I.e., for each directory in DIRLIST, it is tested whether it exists and if
so, it is added to the environment variable VAR."
  (let ((existing-dirs
         (mapcar
          (lambda (x)
            (when (and
                   (file-exists-p
                    (tramp-make-tramp-file-name multi-method method user host x))
                   (file-directory-p
                    (tramp-make-tramp-file-name multi-method method user host x)))
              x))
          dirlist)))
    (tramp-send-command
     multi-method method user host
     (concat var "="
             (mapconcat 'identity (delq nil existing-dirs) ":")
             "; export " var))
  (tramp-wait-for-output)))

;; -- communication with external shell --

(defun tramp-find-file-exists-command (multi-method method user host)
  "Find a command on the remote host for checking if a file exists.
Here, we are looking for a command which has zero exit status if the
file exists and nonzero exit status otherwise."
  (make-local-variable 'tramp-file-exists-command)
  (tramp-message 9 "Finding command to check if file exists")
  (let ((existing
         (tramp-make-tramp-file-name
          multi-method method user host
          "/"))                         ;assume this file always exists
        (nonexisting
         (tramp-make-tramp-file-name
          multi-method method user host
          "/ this file does not exist "))) ;assume this never exists
    ;; The algorithm is as follows: we try a list of several commands.
    ;; For each command, we first run `$cmd /' -- this should return
    ;; true, as the root directory always exists.  And then we run
    ;; `$cmd /this\ file\ does\ not\ exist', hoping that the file indeed
    ;; does not exist.  This should return false.  We use the first
    ;; command we find that seems to work.
    ;; The list of commands to try is as follows:
    ;; `ls -d'          This works on most systems, but NetBSD 1.4
    ;;                  has a bug: `ls' always returns zero exit
    ;;                  status, even for files which don't exist.
    ;; `test -e'        Some Bourne shells have a `test' builtin
    ;;                  which does not know the `-e' option.
    ;; `/bin/test -e'   For those, the `test' binary on disk normally
    ;;                  provides the option.  Alas, the binary
    ;;                  is sometimes `/bin/test' and sometimes it's
    ;;                  `/usr/bin/test'.
    ;; `/usr/bin/test -e'       In case `/bin/test' does not exist.
    (unless (or
             (and (setq tramp-file-exists-command "test -e %s")
                  (file-exists-p existing)
                  (not (file-exists-p nonexisting)))
             (and (setq tramp-file-exists-command "/bin/test -e %s")
                  (file-exists-p existing)
                  (not (file-exists-p nonexisting)))
             (and (setq tramp-file-exists-command "/usr/bin/test -e %s")
                  (file-exists-p existing)
                  (not (file-exists-p nonexisting)))
             (and (setq tramp-file-exists-command "ls -d %s")
                  (file-exists-p existing)
                  (not (file-exists-p nonexisting))))
      (error "Couldn't find command to check if file exists"))))


;; CCC test ksh or bash found for tilde expansion?
(defun tramp-find-shell (multi-method method user host)
  "Find a shell on the remote host which groks tilde expansion."
  (let ((shell nil))
    (tramp-send-command multi-method method user host "echo ~root")
    (tramp-wait-for-output)
    (cond
     ((string-match "^~root$" (buffer-string))
      (setq shell
            (or (tramp-find-executable multi-method method user host
				       "bash"  tramp-remote-path t)
                (tramp-find-executable multi-method method user host
				       "ksh" tramp-remote-path t)))
      (unless shell
        (error "Couldn't find a shell which groks tilde expansion"))
      ;; Find arguments for this shell.
      (let ((alist tramp-sh-extra-args)
	    item extra-args)
	(while (and alist (null extra-args))
	  (setq item (pop alist))
	  (when (string-match (car item) shell)
	    (setq extra-args (cdr item))))
	(when extra-args (setq shell (concat shell " " extra-args))))
      (tramp-message
       5 "Starting remote shell `%s' for tilde expansion..." shell)
      (tramp-send-command
       multi-method method user host
       (concat "PS1='$ ' exec " shell)) ;
      (tramp-barf-if-no-shell-prompt
       (get-buffer-process (current-buffer))
       60 "Couldn't find remote `%s' prompt" shell)
      (tramp-message
       9 "Setting remote shell prompt...")
      ;; Douglas Gray Stephens <DGrayStephens@slb.com> says that we
      ;; must use "\n" here, not tramp-rsh-end-of-line.  Kai left the
      ;; last tramp-rsh-end-of-line, Douglas wanted to replace that,
      ;; as well.
      (process-send-string nil (format "PS1='%s%s%s'; PS2=''; PS3=''%s"
				       tramp-rsh-end-of-line
                                       tramp-end-of-output
				       tramp-rsh-end-of-line
                                       tramp-rsh-end-of-line))
      (tramp-wait-for-output)
      (tramp-message
       9 "Setting remote shell prompt...done")
      )
     (t (tramp-message 5 "Remote `%s' groks tilde expansion, good"
		       (tramp-get-method-parameter
			multi-method method user host 'tramp-remote-sh))))))

(defun tramp-check-ls-command (multi-method method user host cmd)
  "Checks whether the given `ls' executable groks `-n'.
METHOD, USER and HOST specify the connection, CMD (the absolute file name of)
the `ls' executable.  Returns t if CMD supports the `-n' option, nil
otherwise."
  (tramp-message 9 "Checking remote `%s' command for `-n' option" cmd)
  (when (file-executable-p
         (tramp-make-tramp-file-name multi-method method user host cmd))
    (let ((result nil))
      (tramp-message 7 "Testing remote command `%s' for -n..." cmd)
      (setq result
            (tramp-send-command-and-check
             multi-method method user host
             (format "%s -lnd / >/dev/null"
                     cmd)))
      (tramp-message 7 "Testing remote command `%s' for -n...%s"
                   cmd
                   (if (zerop result) "okay" "failed"))
      (zerop result))))

(defun tramp-check-ls-commands (multi-method method user host cmd dirlist)
  "Checks whether the given `ls' executable in one of the dirs groks `-n'.
Returns nil if none was found, else the command is returned."
  (let ((dl dirlist)
        (result nil))
    (tramp-let-maybe directory-sep-char ?/ ;for XEmacs
      ;; It would be better to use the CL function `find', but
      ;; we don't want run-time dependencies on CL.
      (while (and dl (not result))
	(let ((x (concat (file-name-as-directory (car dl)) cmd)))
	  (when (tramp-check-ls-command multi-method method user host x)
	    (setq result x)))
	(setq dl (cdr dl)))
      result)))

(defun tramp-find-ls-command (multi-method method user host)
  "Finds an `ls' command which groks the `-n' option, returning nil if failed.
\(This option prints numeric user and group ids in a long listing.)"
  (tramp-message 9 "Finding a suitable `ls' command")
  (or
   (tramp-check-ls-commands multi-method method user host "ls" tramp-remote-path)
   (tramp-check-ls-commands multi-method method user host "gnuls" tramp-remote-path)
   (tramp-check-ls-commands multi-method method user host "gls" tramp-remote-path)))

;; ------------------------------------------------------------
;; -- Functions for establishing connection --
;; ------------------------------------------------------------

;; The following functions are actions to be taken when seeing certain
;; prompts from the remote host.  See the variable
;; `tramp-actions-before-shell' for usage of these functions.

(defun tramp-action-login (p multi-method method user host)
  "Send the login name."
  (tramp-message 9 "Sending login name `%s'"
		 (or user (user-login-name)))
  (erase-buffer)
  (process-send-string nil (concat (or user (user-login-name))
				   tramp-rsh-end-of-line)))

(defun tramp-action-password (p multi-method method user host)
  "Query the user for a password."
  (let ((pw-prompt
	 (format "Password for %s "
		 (tramp-make-tramp-file-name
		  nil method user host ""))))
    (tramp-message 9 "Sending password")
    (tramp-enter-password p pw-prompt user host)))

(defun tramp-action-succeed (p multi-method method user host)
  "Signal success in finding shell prompt."
  (tramp-message 9 "Found remote shell prompt.")
  (erase-buffer)
  (throw 'tramp-action 'ok))

(defun tramp-action-permission-denied (p multi-method method user host)
  "Signal permission denied."
  (pop-to-buffer (tramp-get-buffer multi-method method user host))
  (tramp-message 9 "Permission denied by remote host.")
  (kill-process p)
  (throw 'tramp-action 'permission-denied))

(defun tramp-action-yesno (p multi-method method user host)
  "Ask the user for confirmation using `yes-or-no-p'.
Send \"yes\" to remote process on confirmation, abort otherwise.
See also `tramp-action-yn'."
  (save-window-excursion
    (pop-to-buffer (tramp-get-buffer multi-method method user host))
    (unless (yes-or-no-p (match-string 0))
      (kill-process p)
      (erase-buffer)
      (throw 'tramp-action 'permission-denied))
    (process-send-string p (concat "yes" tramp-rsh-end-of-line))
    (erase-buffer)))

(defun tramp-action-yn (p multi-method method user host)
  "Ask the user for confirmation using `y-or-n-p'.
Send \"y\" to remote process on confirmation, abort otherwise.
See also `tramp-action-yesno'."
  (save-window-excursion
    (pop-to-buffer (tramp-get-buffer multi-method method user host))
    (unless (y-or-n-p (match-string 0))
      (kill-process p)
      (throw 'tramp-action 'permission-denied))
    (erase-buffer)
    (process-send-string p (concat "y" tramp-rsh-end-of-line))))

(defun tramp-action-terminal (p multi-method method user host)
  "Tell the remote host which terminal type to use.
The terminal type can be configured with `tramp-terminal-type'."
  (tramp-message 9 "Setting `%s' as terminal type."
		 tramp-terminal-type)
  (erase-buffer)
  (process-send-string nil (concat tramp-terminal-type
				   tramp-rsh-end-of-line)))

(defun tramp-action-process-alive (p multi-method method user host)
  "Check whether a process has finished."
  (unless (memq (process-status p) '(run open))
    (throw 'tramp-action 'process-died)))

(defun tramp-action-out-of-band (p multi-method method user host)
  "Check whether an out-of-band copy has finished."
  (cond ((and (memq (process-status p) '(stop exit))
	      (zerop (process-exit-status p)))
	 (tramp-message 9 "Process has finished.")
	 (throw 'tramp-action 'ok))
	((or (and (memq (process-status p) '(stop exit))
		  (not (zerop (process-exit-status p))))
	     (memq (process-status p) '(signal)))
	 ;; `scp' could have copied correctly, but set modes could have failed.
	 ;; This can be ignored.
	 (goto-char (point-min))
	 (if (re-search-forward tramp-operation-not-permitted-regexp nil t)
	     (progn
	       (tramp-message 10 "'set mode' error ignored.")
	       (tramp-message 9 "Process has finished.")
	       (throw 'tramp-action 'ok))
	   (goto-char (point-min))
	   (when (re-search-forward "^.cp.?: \\(.+: Permission denied.?\\)$" nil t)
	     (error "Remote host: %s" (match-string 1)))
	   (tramp-message 9 "Process has died.")
	   (throw 'tramp-action 'process-died)))
	(t nil)))

;; The following functions are specifically for multi connections.

(defun tramp-multi-action-login (p method user host)
  "Send the login name."
  (tramp-message 9 "Sending login name `%s'" user)
  (erase-buffer)
  (process-send-string p (concat user tramp-rsh-end-of-line)))

(defun tramp-multi-action-password (p method user host)
  "Query the user for a password."
  (let ((pw-prompt
	 (format "Password for %s "
		 (tramp-make-tramp-file-name
		  nil method user host ""))))
    (tramp-message 9 "Sending password")
    (tramp-enter-password p pw-prompt user host)))

(defun tramp-multi-action-succeed (p method user host)
  "Signal success in finding shell prompt."
  (tramp-message 9 "Found shell prompt on `%s'" host)
  (erase-buffer)
  (throw 'tramp-action 'ok))

(defun tramp-multi-action-permission-denied (p method user host)
  "Signal permission denied."
  (tramp-message 9 "Permission denied by remote host `%s'" host)
  (kill-process p)
  (erase-buffer)
  (throw 'tramp-action 'permission-denied))

(defun tramp-multi-action-process-alive (p method user host)
  "Check whether a process has finished."
  (unless (memq (process-status p) '(run open))
    (throw 'tramp-action 'process-died)))

;; Functions for processing the actions.

(defun tramp-process-one-action (p multi-method method user host actions)
  "Wait for output from the shell and perform one action."
  (let (found item pattern action todo)
    (erase-buffer)
    (tramp-message 9 "Waiting 60s for prompt from remote shell")
    (with-timeout (60 (throw 'tramp-action 'timeout))
      (while (not found)
	(tramp-accept-process-output p 1)
	(goto-char (point-min))
	(setq todo actions)
	(while todo
	  (goto-char (point-min))
	  (setq item (pop todo))
	  (setq pattern (symbol-value (nth 0 item)))
	  (setq action (nth 1 item))
	  (tramp-message 10 "Looking for regexp \"%s\" from remote shell"
			 pattern)
	  (when (re-search-forward (concat pattern "\\'") nil t)
	    (setq found (funcall action p multi-method method user host)))))
      found)))

(defun tramp-process-actions (p multi-method method user host actions)
  "Perform actions until success."
  (let (exit)
    (while (not exit)
      (tramp-message 9 "Waiting for prompts from remote shell")
      (setq exit
	    (catch 'tramp-action
	      (tramp-process-one-action
	       p multi-method method user host actions)
	      nil)))
    (unless (eq exit 'ok)
      (tramp-clear-passwd user host)
      (error "Login failed"))))

;; For multi-actions.

(defun tramp-process-one-multi-action (p method user host actions)
  "Wait for output from the shell and perform one action."
  (let (found item pattern action todo)
    (erase-buffer)
    (tramp-message 9 "Waiting 60s for prompt from remote shell")
    (with-timeout (60 (throw 'tramp-action 'timeout))
      (while (not found)
	(tramp-accept-process-output p 1)
	(setq todo actions)
	(goto-char (point-min))
	(while todo
	  (goto-char (point-min))
	  (setq item (pop todo))
	  (setq pattern (symbol-value (nth 0 item)))
	  (setq action (nth 1 item))
	  (tramp-message 10 "Looking for regexp \"%s\" from remote shell"
			 pattern)
	  (when (re-search-forward (concat pattern "\\'") nil t)
	    (setq found (funcall action p method user host)))))
      found)))

(defun tramp-process-multi-actions (p method user host actions)
  "Perform actions until success."
  (let (exit)
    (while (not exit)
      (tramp-message 9 "Waiting for prompts from remote shell")
      (setq exit
	    (catch 'tramp-action
	      (tramp-process-one-multi-action p method user host actions)
	      nil)))
    (unless (eq exit 'ok)
      (tramp-clear-passwd user host)
      (error "Login failed"))))

;; Functions to execute when we have seen the remote shell prompt but
;; before we exec the Bourne-ish shell.  Note that these commands
;; might be sent to any shell, not just a Bourne-ish shell.  This
;; means that the commands need to work in all shells.  (It is also
;; okay for some commands to just fail with an error message, but
;; please make sure that they at least don't crash the odd shell people
;; might be running...)
(defun tramp-process-initial-commands (p
				       multi-method method user host
				       commands)
  "Send list of commands to remote host, in order."
  (let (cmd)
    (while commands
      (setq cmd (pop commands))
      (erase-buffer)
      (tramp-message 10 "Sending command to remote shell: %s"
		     cmd)
      (tramp-send-command multi-method method user host cmd nil t)
      (tramp-barf-if-no-shell-prompt
       p 60 "Remote shell command failed: %s" cmd))
    (erase-buffer)))

;; The actual functions for opening connections.

(defun tramp-open-connection-telnet (multi-method method user host)
  "Open a connection using a telnet METHOD.
This starts the command `telnet HOST ARGS'[*], then waits for a remote
login prompt, then sends the user name USER, then waits for a remote
password prompt.  It queries the user for the password, then sends the
password to the remote host.

If USER is nil, uses value returned by `(user-login-name)' instead.

Recognition of the remote shell prompt is based on the variables
`shell-prompt-pattern' and `tramp-shell-prompt-pattern' which must be
set up correctly.

Please note that it is NOT possible to use this connection method
together with an out-of-band transfer method!  You must use an inline
transfer method.

Maybe the different regular expressions need to be tuned.

* Actually, the telnet program as well as the args to be used can be
  specified in the method parameters, see the variable `tramp-methods'."
  (save-match-data
    (when (tramp-method-out-of-band-p multi-method method user host)
      (error "Cannot use out-of-band method `%s' with telnet connection method"
             method))
    (when multi-method
      (error "Cannot multi-connect using telnet connection method"))
    (tramp-pre-connection multi-method method user host tramp-chunksize)
    (tramp-message 7 "Opening connection for %s@%s using %s..."
		   (or user (user-login-name)) host method)
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "TERM" tramp-terminal-type)
      (let* ((default-directory (tramp-temporary-file-directory))
	     ;; If we omit the conditional here, then we would use
	     ;; `undecided-dos' in some cases.  With the conditional,
	     ;; we use nil in these cases.  Which one is right?
             (coding-system-for-read (unless (and (not (featurep 'xemacs))
                                                  (> emacs-major-version 20))
                                       tramp-dos-coding-system))
             (p (apply 'start-process
                       (tramp-buffer-name multi-method method user host)
                       (tramp-get-buffer multi-method method user host)
		       (tramp-get-method-parameter
			multi-method
			(tramp-find-method multi-method method user host)
			user host 'tramp-login-program)
                       host
		       (tramp-get-method-parameter
			multi-method
			(tramp-find-method multi-method method user host)
			user host 'tramp-login-args)))
             (found nil)
             (pw nil))
        (tramp-set-process-query-on-exit-flag p nil)
	(set-buffer (tramp-get-buffer multi-method method user host))
	(erase-buffer)
	(tramp-process-actions p multi-method method user host
			       tramp-actions-before-shell)
        (tramp-open-connection-setup-interactive-shell
         p multi-method method user host)
        (tramp-post-connection multi-method method user host)))))


(defun tramp-open-connection-rsh (multi-method method user host)
  "Open a connection using an rsh METHOD.
This starts the command `rsh HOST -l USER'[*], then waits for a remote
password or shell prompt.  If a password prompt is seen, the user is
queried for a password, this function sends the password to the remote
host and waits for a shell prompt.

If USER is nil, start the command `rsh HOST'[*] instead

Recognition of the remote shell prompt is based on the variables
`shell-prompt-pattern' and `tramp-shell-prompt-pattern' which must be
set up correctly.

Kludgy feature: if HOST has the form \"xx#yy\", then yy is assumed to
be a port number for ssh, and \"-p yy\" will be added to the list of
arguments, and xx will be used as the host name to connect to.

* Actually, the rsh program to be used can be specified in the
  method parameters, see the variable `tramp-methods'."
  (save-match-data
    (when multi-method
      (error "Cannot multi-connect using rsh connection method"))
    (tramp-pre-connection multi-method method user host tramp-chunksize)
    (if (and user (not (string= user "")))
	(tramp-message 7 "Opening connection for %s@%s using %s..."
		       user host method)
      (tramp-message 7 "Opening connection at %s using %s..." host method))
    (let ((process-environment (copy-sequence process-environment))
	  (bufnam (tramp-buffer-name multi-method method user host))
	  (buf (tramp-get-buffer multi-method method user host))
	  (login-program (tramp-get-method-parameter
			multi-method
			(tramp-find-method multi-method method user host)
			user host 'tramp-login-program))
	  (login-args (tramp-get-method-parameter
		     multi-method
		     (tramp-find-method multi-method method user host)
		     user host 'tramp-login-args))
	  (real-host host))
      ;; The following should be changed.  We need a more general
      ;; mechanism to parse extra host args.
      (when (string-match "\\([^#]*\\)#\\(.*\\)" host)
	(setq login-args (cons "-p" (cons (match-string 2 host) login-args)))
	(setq real-host (match-string 1 host)))
      (setenv "TERM" tramp-terminal-type)
      (let* ((default-directory (tramp-temporary-file-directory))
	     ;; If we omit the conditional, we would use
	     ;; `undecided-dos' in some cases.  With the conditional,
	     ;; we use nil in these cases.  Which one is right?
             (coding-system-for-read (unless (and (not (featurep 'xemacs))
                                                  (> emacs-major-version 20))
                                       tramp-dos-coding-system))
             (p (if (and user (not (string= user "")))
                    (apply #'start-process bufnam buf login-program
                           real-host "-l" user login-args)
                  (apply #'start-process bufnam buf login-program
                         real-host login-args)))
             (found nil))
        (tramp-set-process-query-on-exit-flag p nil)

	(set-buffer buf)
	(tramp-process-actions p multi-method method user host
			       tramp-actions-before-shell)
        (tramp-message 7 "Initializing remote shell")
        (tramp-open-connection-setup-interactive-shell
         p multi-method method user host)
        (tramp-post-connection multi-method method user host)))))

(defun tramp-open-connection-su (multi-method method user host)
  "Open a connection using the `su' program with METHOD.
This starts `su - USER', then waits for a password prompt.  The HOST
name must be equal to the local host name or to `localhost'.

If USER is nil, uses value returned by user-login-name instead.

Recognition of the remote shell prompt is based on the variables
`shell-prompt-pattern' and `tramp-shell-prompt-pattern' which must be
set up correctly.  Note that the other user may have a different shell
prompt than you do, so it is not at all unlikely that the variable
`shell-prompt-pattern' is set up wrongly!"
  (save-match-data
    (when (tramp-method-out-of-band-p multi-method method user host)
      (error "Cannot use out-of-band method `%s' with `su' connection method"
             method))
    (unless (or (string-match (concat "^" (regexp-quote host))
                              (system-name))
                (string= "localhost" host)
		(string= "" host))
      (error
       "Cannot connect to different host `%s' with `su' connection method"
       host))
    (tramp-pre-connection multi-method method user host tramp-chunksize)
    (tramp-message 7 "Opening connection for `%s' using `%s'..."
		   (or user "<root>") method)
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "TERM" tramp-terminal-type)
      (let* ((default-directory (tramp-temporary-file-directory))
	     ;; If we omit the conditional, we use `undecided-dos' in
	     ;; some cases.  With the conditional, we use nil in these
	     ;; cases.  What's the difference?  Which one is right?
             (coding-system-for-read (unless (and (not (featurep 'xemacs))
                                                  (> emacs-major-version 20))
                                       tramp-dos-coding-system))
             (p (apply 'start-process
                       (tramp-buffer-name multi-method method user host)
                       (tramp-get-buffer multi-method method user host)
		       (tramp-get-method-parameter
			multi-method
			(tramp-find-method multi-method method user host)
			user host 'tramp-login-program)
                       (mapcar
                        (lambda (x)
			  (format-spec x `((?u . ,(or user "root")))))
                        (tramp-get-method-parameter
			 multi-method
			 (tramp-find-method multi-method method user host)
			 user host 'tramp-login-args))))
             (found nil)
             (pw nil))
        (tramp-set-process-query-on-exit-flag p nil)
	(set-buffer (tramp-get-buffer multi-method method user host))
	(tramp-process-actions p multi-method method user host
			       tramp-actions-before-shell)
        (tramp-open-connection-setup-interactive-shell
         p multi-method method user host)
        (tramp-post-connection multi-method method
                               user host)))))

;; HHH: Not Changed.  Multi method.  It is not clear to me how this can
;;      handle not giving a user name in the "file name".
;;
;;      This is more difficult than for the single-hop method.  In the
;;      multi-hop-method, the desired behaviour should be that the
;;      user must specify names for the telnet hops of which the user
;;      name is different than the "original" name (or different from
;;      the previous hop.
(defun tramp-open-connection-multi (multi-method method user host)
  "Open a multi-hop connection using METHOD.
This uses a slightly changed file name syntax.  The idea is to say
    [multi/telnet:u1@h1/rsh:u2@h2]/path/to/file
This will use telnet to log in as u1 to h1, then use rsh from there to
log in as u2 to h2."
  (save-match-data
    (unless multi-method
      (error "Multi-hop open connection function called on non-multi method"))
    (when (tramp-method-out-of-band-p multi-method method user host)
      (error "No out of band multi-hop connections"))
    (unless (and (arrayp method) (not (stringp method)))
      (error "METHOD must be an array of strings for multi methods"))
    (unless (and (arrayp user) (not (stringp user)))
      (error "USER must be an array of strings for multi methods"))
    (unless (and (arrayp host) (not (stringp host)))
      (error "HOST must be an array of strings for multi methods"))
    (unless (and (= (length method) (length user))
                 (= (length method) (length host)))
      (error "Arrays METHOD, USER, HOST must have equal length"))
    (tramp-pre-connection multi-method method user host tramp-chunksize)
    (tramp-message 7 "Opening `%s' connection..." multi-method)
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "TERM" tramp-terminal-type)
      (let* ((default-directory (tramp-temporary-file-directory))
	     ;; If we omit the conditional, we use `undecided-dos' in
	     ;; some cases.  With the conditional, we use nil in these
	     ;; cases.  What's the difference?  Which one is right?
             (coding-system-for-read (unless (and (not (featurep 'xemacs))
                                                  (> emacs-major-version 20))
                                       tramp-dos-coding-system))
             (p (start-process (tramp-buffer-name multi-method method user host)
                               (tramp-get-buffer multi-method method user host)
                               tramp-multi-sh-program))
             (num-hops (length method))
             (i 0))
        (tramp-set-process-query-on-exit-flag p nil)
        (tramp-message 9 "Waiting 60s for local shell to come up...")
        (unless (tramp-wait-for-regexp
		 p 60 (format "\\(%s\\)\\'\\|\\(%s\\)\\'"
			      shell-prompt-pattern tramp-shell-prompt-pattern))
          (pop-to-buffer (buffer-name))
          (kill-process p)
          (error "Couldn't find local shell prompt"))
        ;; Now do all the connections as specified.
        (while (< i num-hops)
          (let* ((m (aref method i))
                 (u (aref user i))
                 (h (aref host i))
                 (entry (assoc m tramp-multi-connection-function-alist))
                 (multi-func (nth 1 entry))
                 (command (nth 2 entry)))
	    ;; The multi-funcs don't need to do save-match-data, as that
            ;; is done here.
            (funcall multi-func p m u h command)
            (erase-buffer)
            (incf i)))
        (tramp-open-connection-setup-interactive-shell
         p multi-method method user host)
        (tramp-post-connection multi-method method user host)))))

;; HHH: Changed.  Multi method.  Don't know how to handle this in the case
;;      of no user name provided.  Hack to make it work as it did before:
;;      changed `user' to `(or user (user-login-name))' in the places where
;;      the value is actually used.
(defun tramp-multi-connect-telnet (p method user host command)
  "Issue `telnet' command.
Uses shell COMMAND to issue a `telnet' command to log in as USER to
HOST.  You can use percent escapes in COMMAND: `%h' is replaced with
the host name, and `%n' is replaced with an end of line character, as
set in `tramp-rsh-end-of-line'.  Use `%%' if you want a literal percent
character.

If USER is nil, uses the return value of (user-login-name) instead."
  (let ((cmd (format-spec command
			  `((?h . ,host) (?n . ,tramp-rsh-end-of-line))))
        (cmd1 (format-spec command `((?h . ,host) (?n . ""))))
        found pw)
    (erase-buffer)
    (tramp-message 9 "Sending telnet command `%s'" cmd1)
    (process-send-string p cmd)
    (tramp-process-multi-actions p method user host
				 tramp-multi-actions)))

;; HHH: Changed.  Multi method.  Don't know how to handle this in the case
;;      of no user name provided.  Hack to make it work as it did before:
;;      changed `user' to `(or user (user-login-name))' in the places where
;;      the value is actually used.
(defun tramp-multi-connect-rlogin (p method user host command)
  "Issue `rlogin' command.
Uses shell COMMAND to issue an `rlogin' command to log in as USER to
HOST.  You can use percent escapes in COMMAND.  `%u' will be replaced
with the user name, `%h' will be replaced with the host name, and `%n'
will be replaced with the value of `tramp-rsh-end-of-line'.  You can use
`%%' if you want to use a literal percent character.

If USER is nil, uses the return value of (user-login-name) instead."
  (let ((cmd (format-spec command `((?h . ,host)
				    (?u . ,(or user (user-login-name)))
				    (?n . ,tramp-rsh-end-of-line))))
        (cmd1 (format-spec command `((?h . ,host)
				     (?u . ,(or user (user-login-name)))
				     (?n . ""))))
        found)
    (erase-buffer)
    (tramp-message 9 "Sending rlogin command `%s'" cmd1)
    (process-send-string p cmd)
    (tramp-process-multi-actions p method user host
				 tramp-multi-actions)))

;; HHH: Changed.  Multi method.  Don't know how to handle this in the case
;;      of no user name provided.  Hack to make it work as it did before:
;;      changed `user' to `(or user (user-login-name))' in the places where
;;      the value is actually used.
(defun tramp-multi-connect-su (p method user host command)
  "Issue `su' command.
Uses shell COMMAND to issue a `su' command to log in as USER on
HOST.  The HOST name is ignored, this just changes the user id on the
host currently logged in to.

If USER is nil, uses the return value of (user-login-name) instead.

You can use percent escapes in the COMMAND.  `%u' is replaced with the
user name, and `%n' is replaced with the value of
`tramp-rsh-end-of-line'.  Use `%%' if you want a literal percent
character."
  (let ((cmd (format-spec command `((?u . ,(or user (user-login-name)))
				    (?n . ,tramp-rsh-end-of-line))))
        (cmd1 (format-spec command `((?u . ,(or user (user-login-name)))
				     (?n . ""))))
        found)
    (erase-buffer)
    (tramp-message 9 "Sending su command `%s'" cmd1)
    (process-send-string p cmd)
    (tramp-process-multi-actions p method user host
				 tramp-multi-actions)))

;; Utility functions.

(defun tramp-accept-process-output
  (&optional process timeout timeout-msecs)
  "Like `accept-process-output' for Tramp processes.
This is needed in order to hide `last-coding-system-used', which is set
for process communication also."
  (let (last-coding-system-used)
    (accept-process-output process timeout timeout-msecs)))

(defun tramp-wait-for-regexp (proc timeout regexp)
  "Wait for a REGEXP to appear from process PROC within TIMEOUT seconds.
Expects the output of PROC to be sent to the current buffer.  Returns
the string that matched, or nil.  Waits indefinitely if TIMEOUT is
nil."
  (let ((found nil)
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
		   (error "Process has died"))
                 (goto-char (point-min))
                 (setq found (re-search-forward regexp nil t))))))
          (t
           (while (not found)
             (tramp-accept-process-output proc 1)
	     (unless (memq (process-status proc) '(run open))
	       (error "Process has died"))
             (goto-char (point-min))
             (setq found (re-search-forward regexp nil t)))))
    (when tramp-debug-buffer
      (append-to-buffer
       (tramp-get-debug-buffer tramp-current-multi-method tramp-current-method
                             tramp-current-user tramp-current-host)
       (point-min) (point-max))
      (when (not found)
        (save-excursion
          (set-buffer
           (tramp-get-debug-buffer tramp-current-multi-method tramp-current-method
                             tramp-current-user tramp-current-host))
          (goto-char (point-max))
          (insert "[[Regexp `" regexp "' not found"
                  (if timeout (format " in %d secs" timeout) "")
                  "]]"))))
    found))

(defun tramp-wait-for-shell-prompt (proc timeout)
  "Wait for the shell prompt to appear from process PROC within TIMEOUT seconds.
See `tramp-wait-for-regexp' for more details.
Shell prompt pattern is determined by variables `shell-prompt-pattern'
and `tramp-shell-prompt-pattern'."
  (tramp-wait-for-regexp
   proc timeout
   (format "\\(%s\\|%s\\)\\'"
	   shell-prompt-pattern tramp-shell-prompt-pattern)))

(defun tramp-barf-if-no-shell-prompt (proc timeout &rest error-args)
  "Wait for shell prompt and barf if none appears.
Looks at process PROC to see if a shell prompt appears in TIMEOUT
seconds.  If not, it produces an error message with the given ERROR-ARGS."
  (unless (tramp-wait-for-shell-prompt proc timeout)
    (pop-to-buffer (buffer-name))
    (apply 'error error-args)))

(defun tramp-enter-password (p prompt user host)
  "Prompt for a password and send it to the remote end.
Uses PROMPT as a prompt and sends the password to process P."
  (let ((pw (tramp-read-passwd user host prompt)))
    (erase-buffer)
    (process-send-string
     p (concat pw
	       (or (tramp-get-method-parameter
		    tramp-current-multi-method
		    tramp-current-method
		    tramp-current-user
		    tramp-current-host
		    'tramp-password-end-of-line)
		   tramp-default-password-end-of-line)))))

;; HHH: Not Changed.  This might handle the case where USER is not
;;      given in the "File name" very poorly.  Then, the local
;;      variable tramp-current-user will be set to nil.
(defun tramp-pre-connection (multi-method method user host chunksize)
  "Do some setup before actually logging in.
METHOD, USER and HOST specify the connection."
  (set-buffer (tramp-get-buffer multi-method method user host))
  (set (make-local-variable 'tramp-current-multi-method) multi-method)
  (set (make-local-variable 'tramp-current-method) method)
  (set (make-local-variable 'tramp-current-user)   user)
  (set (make-local-variable 'tramp-current-host)   host)
  (set (make-local-variable 'tramp-chunksize)      chunksize)
  (set (make-local-variable 'inhibit-eol-conversion) nil)
  (erase-buffer))

(defun tramp-open-connection-setup-interactive-shell
  (p multi-method method user host)
  "Set up an interactive shell.
Mainly sets the prompt and the echo correctly.  P is the shell process
to set up.  METHOD, USER and HOST specify the connection."
  ;; Wait a bit in case the remote end feels like sending a little
  ;; junk first.  It seems that fencepost.gnu.org does this when doing
  ;; a Kerberos login.
  (sit-for 1)
  (tramp-discard-garbage-erase-buffer p multi-method method user host)
  (tramp-process-initial-commands p multi-method method user host
				  tramp-initial-commands)
  ;; It is useful to set the prompt in the following command because
  ;; some people have a setting for $PS1 which /bin/sh doesn't know
  ;; about and thus /bin/sh will display a strange prompt.  For
  ;; example, if $PS1 has "${CWD}" in the value, then ksh will display
  ;; the current working directory but /bin/sh will display a dollar
  ;; sign.  The following command line sets $PS1 to a sane value, and
  ;; works under Bourne-ish shells as well as csh-like shells.  Daniel
  ;; Pittman reports that the unusual positioning of the single quotes
  ;; makes it work under `rc', too.  We also unset the variable $ENV
  ;; because that is read by some sh implementations (eg, bash when
  ;; called as sh) on startup; this way, we avoid the startup file
  ;; clobbering $PS1.
  (tramp-send-command-internal
   multi-method method user host
   (format "exec env 'ENV=' 'PS1=$ ' %s"
	   (tramp-get-method-parameter
	    multi-method method user host 'tramp-remote-sh))
   (format "remote `%s' to come up"
	   (tramp-get-method-parameter
	    multi-method method user host 'tramp-remote-sh)))
  (tramp-barf-if-no-shell-prompt
   p 30
   "Remote `%s' didn't come up.  See buffer `%s' for details"
   (tramp-get-method-parameter multi-method method user host 'tramp-remote-sh)
   (buffer-name))
  (tramp-message 8 "Setting up remote shell environment")
  (tramp-discard-garbage-erase-buffer p multi-method method user host)
  (tramp-send-command-internal multi-method method user host
			       "stty -inlcr -echo kill '^U'")
  (erase-buffer)
  ;; Ignore garbage after stty command.
  (tramp-send-command-internal multi-method method user host
			       "echo foo")
  (erase-buffer)
  (tramp-send-command-internal multi-method method user host
			       "TERM=dumb; export TERM")
  (erase-buffer)
  ;; Check whether the remote host suffers from buggy `send-process-string'.
  ;; This is known for FreeBSD (see comment in `send_process', file process.c).
  ;; I've tested sending 624 bytes successfully, sending 625 bytes failed.
  ;; Emacs makes a hack when this host type is detected locally.  It cannot
  ;; handle remote hosts, though.
  (when (or (not tramp-chunksize) (zerop tramp-chunksize))
    (tramp-message 9 "Checking remote host type for `send-process-string' bug")
    (tramp-send-command-internal multi-method method user host
				 "(uname -sr) 2>/dev/null")
    (goto-char (point-min))
    (when (looking-at "FreeBSD")
      (setq tramp-chunksize 500)))

  ;; Try to set up the coding system correctly.
  ;; CCC this can't be the right way to do it.  Hm.
  (save-excursion
    (erase-buffer)
    (tramp-message 9 "Determining coding system")
    (tramp-send-command-internal multi-method method user host
				 "echo foo ; echo bar")
    (goto-char (point-min))
    (if (featurep 'mule)
        ;; Use MULE to select the right EOL convention for communicating
        ;; with the process.
        (let* ((cs (or (process-coding-system p) (cons 'undecided 'undecided)))
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
          (set-buffer-process-coding-system cs-decode cs-encode))
      ;; Look for ^M and do something useful if found.
      (when (search-forward "\r" nil t)
        ;; We have found a ^M but cannot frob the process coding system
        ;; because we're running on a non-MULE Emacs.  Let's try
        ;; stty, instead.
	(erase-buffer)
        (tramp-message 9 "Trying `stty -onlcr'")
	(tramp-send-command-internal multi-method method user host
				     "stty -onlcr"))))
  (erase-buffer)
  (tramp-message
   9 "Waiting 30s for `HISTFILE=$HOME/.tramp_history; HISTSIZE=1; export HISTFILE; export HISTSIZE'")
  (tramp-send-command-internal
   multi-method method user host
   "HISTFILE=$HOME/.tramp_history; HISTSIZE=1; export HISTFILE; export HISTSIZE")
  (erase-buffer)
  (tramp-message 9 "Waiting 30s for `set +o vi +o emacs'")
  (tramp-send-command-internal multi-method method user host
			       "set +o vi +o emacs")
  (erase-buffer)
  (tramp-message 9 "Waiting 30s for `unset MAIL MAILCHECK MAILPATH'")
  (tramp-send-command-internal
   multi-method method user host
   "unset MAIL MAILCHECK MAILPATH 1>/dev/null 2>/dev/null")
  (erase-buffer)
  (tramp-message 9 "Waiting 30s for `unset CDPATH'")
  (tramp-send-command-internal multi-method method user host
			       "unset CDPATH")
  (erase-buffer)
  (tramp-message 9 "Setting shell prompt")
  ;; Douglas Gray Stephens <DGrayStephens@slb.com> says that we must
  ;; use "\n" here, not tramp-rsh-end-of-line.  We also manually frob
  ;; the last time we sent a command, to avoid tramp-send-command to send
  ;; "echo are you awake".
  (setq tramp-last-cmd-time (current-time))
  (tramp-send-command
   multi-method method user host
   (format "PS1='%s%s%s'; PS2=''; PS3=''"
	   tramp-rsh-end-of-line
           tramp-end-of-output
	   tramp-rsh-end-of-line))
  (tramp-wait-for-output))

(defun tramp-post-connection (multi-method method user host)
  "Prepare a remote shell before being able to work on it.
METHOD, USER and HOST specify the connection.
Among other things, this finds a shell which groks tilde expansion,
tries to find an `ls' command which groks the `-n' option, sets the
locale to C and sets up the remote shell search path."
  ;; Search for a good shell before searching for a command which
  ;; checks if a file exists. This is done because Tramp wants to use
  ;; "test foo; echo $?" to check if various conditions hold, and
  ;; there are buggy /bin/sh implementations which don't execute the
  ;; "echo $?"  part if the "test" part has an error.  In particular,
  ;; the Solaris /bin/sh is a problem.  I'm betting that all systems
  ;; with buggy /bin/sh implementations will have a working bash or
  ;; ksh.  Whee...
  (tramp-find-shell multi-method method user host)
  ;; Without (sit-for 0.1) at least, my machine will almost always blow
  ;; up on 'not numberp /root' - a race that causes the 'echo ~root'
  ;; output of (tramp-find-shell) to show up along with the output of
  ;; (tramp-find-ls-command) testing.
  ;;
  ;; I can't work out why this is a problem though. The (tramp-wait-for-output)
  ;; call in (tramp-find-shell) *should* make this not happen, I thought.
  ;;
  ;; After much debugging I couldn't find any problem with the implementation
  ;; of that function though. The workaround stays for me at least. :/
  ;;
  ;; Daniel Pittman <daniel@danann.net>
  (sleep-for 1)
  (erase-buffer)
  (tramp-find-file-exists-command multi-method method user host)
  (make-local-variable 'tramp-ls-command)
  (setq tramp-ls-command (tramp-find-ls-command multi-method method user host))
  (unless tramp-ls-command
    (tramp-message
     1
     "Danger!  Couldn't find ls which groks -n.  Muddling through anyway")
    (setq tramp-ls-command
          (tramp-find-executable multi-method method user host
                               "ls" tramp-remote-path nil)))
  (unless tramp-ls-command
    (error "Fatal error: Couldn't find remote executable `ls'"))
  (tramp-message 5 "Using remote command `%s' for getting directory listings"
               tramp-ls-command)
  (tramp-send-command multi-method method user host
                    (concat "tramp_set_exit_status () {" tramp-rsh-end-of-line
                            "return $1" tramp-rsh-end-of-line
                            "}"))
  (tramp-wait-for-output)
  ;; Set remote PATH variable.
  (tramp-set-remote-path multi-method method user host "PATH" tramp-remote-path)
  ;; Tell remote shell to use standard time format, needed for
  ;; parsing `ls -l' output.
  (tramp-send-command multi-method method user host
                    "LC_TIME=C; export LC_TIME; echo huhu")
  (tramp-wait-for-output)
  (tramp-send-command multi-method method user host
                    "mesg n; echo huhu")
  (tramp-wait-for-output)
  (tramp-send-command multi-method method user host
                    "biff n ; echo huhu")
  (tramp-wait-for-output)
  ;; Unalias ls(1) to work around issues with those silly people who make it
  ;; spit out ANSI escapes or whatever.
  (tramp-send-command multi-method method user host
                    "unalias ls; echo huhu")
  (tramp-wait-for-output)
  ;; Does `test A -nt B' work?  Use abominable `find' construct if it
  ;; doesn't.  BSD/OS 4.0 wants the parentheses around the command,
  ;; for otherwise the shell crashes.
  (erase-buffer)
  (make-local-variable 'tramp-test-groks-nt)
  (tramp-send-command multi-method method user host
                    "( test / -nt / )")
  (tramp-wait-for-output)
  (goto-char (point-min))
  (setq tramp-test-groks-nt
        (looking-at (format "\n%s\r?\n" (regexp-quote tramp-end-of-output))))
  (unless tramp-test-groks-nt
    (tramp-send-command
     multi-method method user host
     (concat "tramp_test_nt () {" tramp-rsh-end-of-line
             "test -n \"`find $1 -prune -newer $2 -print`\"" tramp-rsh-end-of-line
             "}")))
  (tramp-wait-for-output)
  ;; Send the fallback `uudecode' script.
  (erase-buffer)
  (tramp-send-string multi-method method user host tramp-uudecode)
  (tramp-wait-for-output)
  ;; Find a `perl'.
  (erase-buffer)
  (tramp-set-connection-property "perl-scripts" nil multi-method method user host)
  (let ((tramp-remote-perl
	 (or (tramp-find-executable multi-method method user host
				    "perl5" tramp-remote-path nil)
	     (tramp-find-executable multi-method method user host
				    "perl" tramp-remote-path nil))))
    (when tramp-remote-perl
      (tramp-set-connection-property "perl" tramp-remote-perl
				     multi-method method user host)
      (unless (tramp-method-out-of-band-p multi-method method user host)
        (tramp-message 5 "Sending the Perl `mime-encode' implementations.")
        (tramp-send-string
         multi-method method user host
         (concat "tramp_encode () {\n"
                 (format tramp-perl-encode tramp-remote-perl)
                 " 2>/dev/null"
                 "\n}"))
        (tramp-wait-for-output)
        (tramp-send-string
         multi-method method user host
         (concat "tramp_encode_with_module () {\n"
                 (format tramp-perl-encode-with-module tramp-remote-perl)
                 " 2>/dev/null"
                 "\n}"))
        (tramp-wait-for-output)
        (tramp-message 5 "Sending the Perl `mime-decode' implementations.")
        (tramp-send-string
         multi-method method user host
         (concat "tramp_decode () {\n"
                 (format tramp-perl-decode tramp-remote-perl)
                 " 2>/dev/null"
                 "\n}"))
        (tramp-wait-for-output)
        (tramp-send-string
         multi-method method user host
         (concat "tramp_decode_with_module () {\n"
                 (format tramp-perl-decode-with-module tramp-remote-perl)
                 " 2>/dev/null"
                 "\n}"))
        (tramp-wait-for-output))))
  ;; Find ln(1)
  (erase-buffer)
  (let ((ln (tramp-find-executable multi-method method user host
				   "ln" tramp-remote-path nil)))
    (when ln
      (tramp-set-connection-property "ln" ln multi-method method user host)))
  (erase-buffer)
  ;; Find the right encoding/decoding commands to use.
  (unless (tramp-method-out-of-band-p multi-method method user host)
    (tramp-find-inline-encoding multi-method method user host))
  ;; If encoding/decoding command are given, test to see if they work.
  ;; CCC: Maybe it would be useful to run the encoder both locally and
  ;; remotely to see if they produce the same result.
  (let ((rem-enc (tramp-get-remote-encoding multi-method method user host))
	(rem-dec (tramp-get-remote-decoding multi-method method user host))
	(magic-string "xyzzy"))
    (when (and (or rem-dec rem-enc) (not (and rem-dec rem-enc)))
      (tramp-kill-process multi-method method user host)
      ;; Improve error message and/or error check.
      (error
       "Must give both decoding and encoding command in method definition"))
    (when (and rem-enc rem-dec)
      (tramp-message
       5
       "Checking to see if encoding/decoding commands work on remote host...")
      (tramp-send-command
       multi-method method user host
       (format "echo %s | %s | %s"
	       (tramp-shell-quote-argument magic-string) rem-enc rem-dec))
      (tramp-wait-for-output)
      (unless (looking-at (regexp-quote magic-string))
	(tramp-kill-process multi-method method user host)
	(error "Remote host cannot execute de/encoding commands.  See buffer `%s' for details"
	       (buffer-name)))
      (erase-buffer)
      (tramp-message
       5 "Checking to see if encoding/decoding commands work on remote host...done"))))

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

(defvar tramp-coding-commands
  '(("mimencode -b" "mimencode -u -b"
     base64-encode-region base64-decode-region)
    ("mmencode -b" "mmencode -u -b"
     base64-encode-region base64-decode-region)
    ("recode data..base64" "recode base64..data"
     base64-encode-region base64-decode-region)
    ("uuencode xxx" "uudecode -o /dev/stdout"
     tramp-uuencode-region uudecode-decode-region)
    ("uuencode xxx" "uudecode -o -"
     tramp-uuencode-region uudecode-decode-region)
    ("uuencode xxx" "uudecode -p"
     tramp-uuencode-region uudecode-decode-region)
    ("uuencode xxx" "tramp_uudecode"
     tramp-uuencode-region uudecode-decode-region)
    ("tramp_encode_with_module" "tramp_decode_with_module"
     base64-encode-region base64-decode-region)
    ("tramp_encode" "tramp_decode"
     base64-encode-region base64-decode-region))
  "List of coding commands for inline transfer.
Each item is a list that looks like this:

\(REMOTE-ENCODING REMOTE-DECODING LOCAL-ENCODING LOCAL-DECODING)

The REMOTE-ENCODING should be a string, giving a command accepting a
plain file on standard input and writing the encoded file to standard
output.  The REMOTE-DECODING should also be a string, giving a command
accepting an encoded file on standard input and writing the decoded
file to standard output.

LOCAL-ENCODING and LOCAL-DECODING can be strings, giving commands, or
symbols, giving functions.  If they are strings, then they can contain
the \"%s\" format specifier.  If that specifier is present, the input
filename will be put into the command line at that spot.  If the
specifier is not present, the input should be read from standard
input.

If they are functions, they will be called with two arguments, start
and end of region, and are expected to replace the region contents
with the encoded or decoded results, respectively.")

(defun tramp-find-inline-encoding (multi-method method user host)
  "Find an inline transfer encoding that works.
Goes through the list `tramp-coding-commands'."
  (let ((commands tramp-coding-commands)
	(magic "xyzzy")
	item found)
    (while (and commands (null found))
      (setq item (pop commands))
      (catch 'wont-work
	(let ((rem-enc (nth 0 item))
	      (rem-dec (nth 1 item))
	      (loc-enc (nth 2 item))
	      (loc-dec (nth 3 item)))
	  ;; Check if remote encoding and decoding commands can be
	  ;; called remotely with null input and output.  This makes
	  ;; sure there are no syntax errors and the command is really
	  ;; found.  Note that we do not redirect stdout to /dev/null,
	  ;; for two reaons: when checking the decoding command, we
	  ;; actually check the output it gives.  And also, when
	  ;; redirecting "mimencode" output to /dev/null, then as root
	  ;; it might change the permissions of /dev/null!
	  (tramp-message-for-buffer
	   multi-method method user host 9
	   "Checking remote encoding command `%s' for sanity" rem-enc)
	  (unless (zerop (tramp-send-command-and-check
			  multi-method method user host
			  (format "%s </dev/null" rem-enc) t))
	    (throw 'wont-work nil))
	  (tramp-message-for-buffer
	   multi-method method user host 9
	   "Checking remote decoding command `%s' for sanity" rem-dec)
	  (unless (zerop (tramp-send-command-and-check
			  multi-method method user host
			  (format "echo %s | %s | %s"
				  magic rem-enc rem-dec) t))
	    (throw 'wont-work nil))
	  (save-excursion
	    (goto-char (point-min))
	    (unless (looking-at (regexp-quote magic))
	      (throw 'wont-work nil)))
	  ;; If the local encoder or decoder is a string, the
	  ;; corresponding command has to work locally.
	  (when (stringp loc-enc)
	    (tramp-message-for-buffer
	     multi-method method user host 9
	     "Checking local encoding command `%s' for sanity" loc-enc)
	    (unless (zerop (tramp-call-local-coding-command
			    loc-enc nil nil))
	      (throw 'wont-work nil)))
	  (when (stringp loc-dec)
	    (tramp-message-for-buffer
	     multi-method method user host 9
	     "Checking local decoding command `%s' for sanity" loc-dec)
	    (unless (zerop (tramp-call-local-coding-command
			    loc-dec nil nil))
	      (throw 'wont-work nil)))
	  ;; CCC: At this point, maybe we should check that the output
	  ;; of the commands is correct.  But for the moment we will
	  ;; assume that commands working on empty input will also
	  ;; work in practice.
	  (setq found item))))
    ;; Did we find something?  If not, issue error.  If so,
    ;; set connection properties.
    (unless found
      (error "Couldn't find an inline transfer encoding"))
    (let ((rem-enc (nth 0 found))
	  (rem-dec (nth 1 found))
	  (loc-enc (nth 2 found))
	  (loc-dec (nth 3 found)))
      (tramp-message 10 "Using remote encoding %s" rem-enc)
      (tramp-set-remote-encoding multi-method method user host rem-enc)
      (tramp-message 10 "Using remote decoding %s" rem-dec)
      (tramp-set-remote-decoding multi-method method user host rem-dec)
      (tramp-message 10 "Using local encoding %s" loc-enc)
      (tramp-set-local-encoding multi-method method user host loc-enc)
      (tramp-message 10 "Using local decoding %s" loc-dec)
      (tramp-set-local-decoding multi-method method user host loc-dec))))

(defun tramp-call-local-coding-command (cmd input output)
  "Call the local encoding or decoding command.
If CMD contains \"%s\", provide input file INPUT there in command.
Otherwise, INPUT is passed via standard input.
INPUT can also be nil which means `/dev/null'.
OUTPUT can be a string (which specifies a filename), or t (which
means standard output and thus the current buffer), or nil (which
means discard it)."
  (call-process
   tramp-encoding-shell			;program
   (when (and input (not (string-match "%s" cmd)))
     input)				;input
   (if (eq output t) t nil)		;output
   nil					;redisplay
   tramp-encoding-command-switch
   ;; actual shell command
   (concat
    (if (string-match "%s" cmd) (format cmd input) cmd)
    (if (stringp output) (concat "> " output) ""))))

(defun tramp-maybe-open-connection (multi-method method user host)
  "Maybe open a connection to HOST, logging in as USER, using METHOD.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
  (let ((p (get-buffer-process
	    (tramp-get-buffer multi-method method user host)))
	last-cmd-time)
    ;; If too much time has passed since last command was sent, look
    ;; whether process is still alive.  If it isn't, kill it.  When
    ;; using ssh, it can sometimes happen that the remote end has hung
    ;; up but the local ssh client doesn't recognize this until it
    ;; tries to send some data to the remote end.  So that's why we
    ;; try to send a command from time to time, then look again
    ;; whether the process is really alive.
    (save-excursion
      (set-buffer (tramp-get-buffer multi-method method user host))
      (when (and tramp-last-cmd-time
		 (> (tramp-time-diff (current-time) tramp-last-cmd-time) 60)
		 p (processp p) (memq (process-status p) '(run open)))
	(tramp-send-command
	 multi-method method user host "echo are you awake" nil t)
	(unless (tramp-wait-for-output 10)
	  (delete-process p)
	  (setq p nil))
	(erase-buffer)))
    (unless (and p (processp p) (memq (process-status p) '(run open)))
      (when (and p (processp p))
        (delete-process p))
      (let ((process-connection-type tramp-process-connection-type))
	(funcall (tramp-get-method-parameter
		  multi-method
		  (tramp-find-method multi-method method user host)
		  user host 'tramp-connection-function)
		 multi-method method user host)))))

(defun tramp-send-command
  (multi-method method user host command &optional noerase neveropen)
  "Send the COMMAND to USER at HOST (logged in using METHOD).
Erases temporary buffer before sending the command (unless NOERASE
is true).
If optional seventh arg NEVEROPEN is non-nil, never try to open the
connection.  This is meant to be used from
`tramp-maybe-open-connection' only."
  (or neveropen
      (tramp-maybe-open-connection multi-method method user host))
  (setq tramp-last-cmd-time (current-time))
  (setq tramp-last-cmd command)
  (when tramp-debug-buffer
    (save-excursion
      (set-buffer (tramp-get-debug-buffer multi-method method user host))
      (goto-char (point-max))
      (tramp-insert-with-face 'bold (format "$ %s\n" command))))
  (let ((proc nil))
    (set-buffer (tramp-get-buffer multi-method method user host))
    (unless noerase (erase-buffer))
    (setq proc (get-buffer-process (current-buffer)))
    (process-send-string proc
                         (concat command tramp-rsh-end-of-line))))

(defun tramp-send-command-internal
  (multi-method method user host command &optional msg)
  "Send command to remote host and wait for success.
Sends COMMAND, then waits 30 seconds for shell prompt."
  (tramp-send-command multi-method method user host command t t)
  (when msg
    (tramp-message 9 "Waiting 30s for %s..." msg))
  (tramp-barf-if-no-shell-prompt
   nil 30
   "Couldn't `%s', see buffer `%s'" command (buffer-name)))

(defun tramp-wait-for-output (&optional timeout)
  "Wait for output from remote rsh command."
  (let ((proc (get-buffer-process (current-buffer)))
        (found nil)
        (start-time (current-time))
	(start-point (point))
        (end-of-output (concat "^"
                               (regexp-quote tramp-end-of-output)
                               "\r?$")))
    ;; Algorithm: get waiting output.  See if last line contains
    ;; end-of-output sentinel.  If not, wait a bit and again get
    ;; waiting output.  Repeat until timeout expires or end-of-output
    ;; sentinel is seen.  Will hang if timeout is nil and
    ;; end-of-output sentinel never appears.
    (save-match-data
      (cond (timeout
             ;; Work around an XEmacs bug, where the timeout expires
             ;; faster than it should.  This degenerates into polling
             ;; for buggy XEmacsen, but oh, well.
             (while (and (not found)
                         (< (tramp-time-diff (current-time) start-time)
                            timeout))
               (with-timeout (timeout)
                 (while (not found)
                   (tramp-accept-process-output proc 1)
		   (unless (memq (process-status proc) '(run open))
		     (error "Process has died"))
                   (goto-char (point-max))
                   (forward-line -1)
                   (setq found (looking-at end-of-output))))))
            (t
             (while (not found)
               (tramp-accept-process-output proc 1)
	       (unless (memq (process-status proc) '(run open))
		 (error "Process has died"))
               (goto-char (point-max))
               (forward-line -1)
               (setq found (looking-at end-of-output))))))
    ;; At this point, either the timeout has expired or we have found
    ;; the end-of-output sentinel.
    (when found
      (goto-char (point-max))
      (forward-line -2)
      (delete-region (point) (point-max)))
    ;; If processing echoes, look for it in the first line and delete.
    (when tramp-process-echoes
      (save-excursion
	(goto-char start-point)
	(when (looking-at (regexp-quote tramp-last-cmd))
	  (delete-region (point) (progn (forward-line 1) (point))))))
    ;; Add output to debug buffer if appropriate.
    (when tramp-debug-buffer
      (append-to-buffer
       (tramp-get-debug-buffer tramp-current-multi-method tramp-current-method
                             tramp-current-user tramp-current-host)
       (point-min) (point-max))
      (when (not found)
        (save-excursion
          (set-buffer
           (tramp-get-debug-buffer tramp-current-multi-method tramp-current-method
                                 tramp-current-user tramp-current-host))
          (goto-char (point-max))
          (insert "[[Remote prompt `" end-of-output "' not found"
                  (if timeout (format " in %d secs" timeout) "")
                  "]]"))))
    (goto-char (point-min))
    ;; Return value is whether end-of-output sentinel was found.
    found))

(defun tramp-send-command-and-check (multi-method method user host command
                                                  &optional subshell)
  "Run COMMAND and check its exit status.
MULTI-METHOD and METHOD specify how to log in (as USER) to the remote HOST.
Sends `echo $?' along with the COMMAND for checking the exit status.  If
COMMAND is nil, just sends `echo $?'.  Returns the exit status found.

If the optional argument SUBSHELL is non-nil, the command is executed in
a subshell, ie surrounded by parentheses."
  (tramp-send-command multi-method method user host
                      (concat (if subshell "( " "")
                              command
                              (if command " 2>/dev/null; " "")
                              "echo tramp_exit_status $?"
                              (if subshell " )" " ")))
  (tramp-wait-for-output)
  (goto-char (point-max))
  (unless (search-backward "tramp_exit_status " nil t)
    (error "Couldn't find exit status of `%s'" command))
  (skip-chars-forward "^ ")
  (read (current-buffer)))

(defun tramp-barf-unless-okay (multi-method method user host command subshell
                                            signal fmt &rest args)
  "Run COMMAND, check exit status, throw error if exit status not okay.
Similar to `tramp-send-command-and-check' but accepts two more arguments
FMT and ARGS which are passed to `error'."
  (unless (zerop (tramp-send-command-and-check
                  multi-method method user host command subshell))
    ;; CCC: really pop-to-buffer?  Maybe it's appropriate to be more
    ;; silent.
    (pop-to-buffer (current-buffer))
    (funcall 'signal signal (apply 'format fmt args))))

;; It seems that Tru64 Unix does not like it if long strings are sent
;; to it in one go.  (This happens when sending the Perl
;; `file-attributes' implementation, for instance.)  Therefore, we
;; have this function which waits a bit at each line.
(defun tramp-send-string
  (multi-method method user host string)
  "Send the STRING to USER at HOST using METHOD.

The STRING is expected to use Unix line-endings, but the lines sent to
the remote host use line-endings as defined in the variable
`tramp-rsh-end-of-line'."
  (let ((proc (get-buffer-process
               (tramp-get-buffer multi-method method user host))))
    (unless proc
      (error "Can't send string to remote host -- not logged in"))
    ;; debug message
    (when tramp-debug-buffer
      (save-excursion
	(set-buffer (tramp-get-debug-buffer multi-method method user host))
	(goto-char (point-max))
	(tramp-insert-with-face 'bold (format "$ %s\n" string))))
    ;; replace "\n" by `tramp-rsh-end-of-line'
    (setq string
	  (mapconcat 'identity
		     (split-string string "\n")
		     tramp-rsh-end-of-line))
    (unless (or (string= string "")
		(string-equal (substring string -1) tramp-rsh-end-of-line))
      (setq string (concat string tramp-rsh-end-of-line)))
    ;; send the string
    (if (and tramp-chunksize (not (zerop tramp-chunksize)))
	(let ((pos 0)
	      (end (length string)))
	  (while (< pos end)
	    (tramp-message-for-buffer
	     multi-method method user host 10
	     "Sending chunk from %s to %s"
	     pos (min (+ pos tramp-chunksize) end))
	    (process-send-string
	     proc (substring string pos (min (+ pos tramp-chunksize) end)))
	    (setq pos (+ pos tramp-chunksize))
	    (sleep-for 0.1)))
      (process-send-string proc string))))

(defun tramp-send-eof (multi-method method user host)
  "Send EOF to the remote end.
METHOD, HOST and USER specify the connection."
  (let ((proc (get-buffer-process
               (tramp-get-buffer multi-method method user host))))
    (unless proc
      (error "Can't send EOF to remote host -- not logged in"))
    (process-send-eof proc)))
;    (process-send-string proc "\^D")))

(defun tramp-kill-process (multi-method method user host)
  "Kill the connection process used by Tramp.
MULTI-METHOD, METHOD, USER, and HOST specify the connection."
  (let ((proc (get-buffer-process
	       (tramp-get-buffer multi-method method user host))))
    (kill-process proc)))

(defun tramp-discard-garbage-erase-buffer (p multi-method method user host)
  "Erase buffer, then discard subsequent garbage.
If `tramp-discard-garbage' is nil, just erase buffer."
  (if (not tramp-discard-garbage)
      (erase-buffer)
    (while (prog1 (erase-buffer) (tramp-accept-process-output p 0.25))
      (when tramp-debug-buffer
        (save-excursion
          (set-buffer (tramp-get-debug-buffer multi-method method user host))
          (goto-char (point-max))
          (tramp-insert-with-face
           'bold (format "Additional characters detected\n")))))))

(defun tramp-mode-string-to-int (mode-string)
  "Converts a ten-letter `drwxrwxrwx'-style mode string into mode bits."
  (let* ((mode-chars (string-to-vector mode-string))
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
       (case owner-read
         (?r (tramp-octal-to-decimal "00400")) (?- 0)
         (t (error "Second char `%c' must be one of `r-'" owner-read)))
       (case owner-write
         (?w (tramp-octal-to-decimal "00200")) (?- 0)
         (t (error "Third char `%c' must be one of `w-'" owner-write)))
       (case owner-execute-or-setid
         (?x (tramp-octal-to-decimal "00100"))
         (?S (tramp-octal-to-decimal "04000"))
         (?s (tramp-octal-to-decimal "04100"))
         (?- 0)
         (t (error "Fourth char `%c' must be one of `xsS-'"
                   owner-execute-or-setid)))
       (case group-read
         (?r (tramp-octal-to-decimal "00040")) (?- 0)
         (t (error "Fifth char `%c' must be one of `r-'" group-read)))
       (case group-write
         (?w (tramp-octal-to-decimal "00020")) (?- 0)
         (t (error "Sixth char `%c' must be one of `w-'" group-write)))
       (case group-execute-or-setid
         (?x (tramp-octal-to-decimal "00010"))
         (?S (tramp-octal-to-decimal "02000"))
         (?s (tramp-octal-to-decimal "02010"))
         (?- 0)
         (t (error "Seventh char `%c' must be one of `xsS-'"
                   group-execute-or-setid)))
       (case other-read
         (?r (tramp-octal-to-decimal "00004")) (?- 0)
         (t (error "Eighth char `%c' must be one of `r-'" other-read)))
       (case other-write
         (?w (tramp-octal-to-decimal "00002")) (?- 0)
         (t (error "Nineth char `%c' must be one of `w-'" other-write)))
       (case other-execute-or-sticky
         (?x (tramp-octal-to-decimal "00001"))
         (?T (tramp-octal-to-decimal "01000"))
         (?t (tramp-octal-to-decimal "01001"))
         (?- 0)
         (t (error "Tenth char `%c' must be one of `xtT-'"
                   other-execute-or-sticky)))))))

(defun tramp-convert-file-attributes (multi-method method user host attr)
  "Convert file-attributes ATTR generated by perl script or ls.
Convert file mode bits to string and set virtual device number.
Return ATTR."
  (unless (stringp (nth 8 attr))
    ;; Convert file mode bits to string.
    (setcar (nthcdr 8 attr) (tramp-file-mode-from-int (nth 8 attr))))
  ;; Set virtual device number.
  (setcar (nthcdr 11 attr)
          (tramp-get-device multi-method method user host))
  attr)

(defun tramp-get-device (multi-method method user host)
  "Returns the virtual device number.
If it doesn't exist, generate a new one."
  (let ((string (tramp-make-tramp-file-name multi-method method user host "")))
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


;;(defun tramp-octal-to-decimal (ostr)
;;  "Given a string of octal digits, return a decimal number."
;;  (cond ((null ostr) 0)
;;        ((string= "" ostr) 0)
;;        (t (let ((last (aref ostr (1- (length ostr))))
;;                 (rest (substring ostr 0 (1- (length ostr)))))
;;             (unless (and (>= last ?0)
;;                          (<= last ?7))
;;               (error "Not an octal digit: %c" last))
;;             (+ (- last ?0) (* 8 (tramp-octal-to-decimal rest)))))))
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
;; -- TRAMP file names --
;; ------------------------------------------------------------
;; Conversion functions between external representation and
;; internal data structure.  Convenience functions for internal
;; data structure.

(defstruct tramp-file-name multi-method method user host localname)

(defun tramp-tramp-file-p (name)
  "Return t iff NAME is a tramp file."
  (save-match-data
    (string-match tramp-file-name-regexp name)))

;; HHH: Changed.  Used to assign the return value of (user-login-name)
;;      to the `user' part of the structure if a user name was not
;;      provided, now it assigns nil.
(defun tramp-dissect-file-name (name)
  "Return an `tramp-file-name' structure.
The structure consists of remote method, remote user, remote host and
localname (file name on remote host)."
  (save-match-data
    (let* ((match (string-match (nth 0 tramp-file-name-structure) name))
	   (method
	    ; single-hop
	    (if match (match-string (nth 1 tramp-file-name-structure) name)
	      ; maybe multi-hop
	      (string-match
	       (format (nth 0 tramp-multi-file-name-structure)
		       (nth 0 tramp-multi-file-name-hop-structure)) name)
	      (match-string (nth 1 tramp-multi-file-name-structure) name))))
      (if (and method (member method tramp-multi-methods))
          ;; If it's a multi method, the file name structure contains
          ;; arrays of method, user and host.
          (tramp-dissect-multi-file-name name)
        ;; Normal method.  First, find out default method.
	(unless match (error "Not a tramp file name: %s" name))
	(let ((user (match-string (nth 2 tramp-file-name-structure) name))
	      (host (match-string (nth 3 tramp-file-name-structure) name))
	      (localname (match-string (nth 4 tramp-file-name-structure) name)))
	  (make-tramp-file-name
	   :multi-method nil
	   :method method
	   :user (or user nil)
	   :host host
	   :localname localname))))))

(defun tramp-find-default-method (user host)
  "Look up the right method to use in `tramp-default-method-alist'."
  (let ((choices tramp-default-method-alist)
	(method tramp-default-method)
	item)
    (while choices
      (setq item (pop choices))
      (when (and (string-match (nth 0 item) (or host ""))
		 (string-match (nth 1 item) (or user "")))
	(setq method (nth 2 item))
	(setq choices nil)))
    method))

(defun tramp-find-method (multi-method method user host)
  "Return the right method string to use.
This is MULTI-METHOD, if non-nil.  Otherwise, it is METHOD, if non-nil.
If both MULTI-METHOD and METHOD are nil, do a lookup in
`tramp-default-method-alist'."
  (or multi-method method (tramp-find-default-method user host)))

;; HHH: Not Changed.  Multi method.  Will probably not handle the case where
;;      a user name is not provided in the "file name" very well.
(defun tramp-dissect-multi-file-name (name)
  "Not implemented yet."
  (let ((regexp           (nth 0 tramp-multi-file-name-structure))
        (method-index     (nth 1 tramp-multi-file-name-structure))
        (hops-index       (nth 2 tramp-multi-file-name-structure))
        (localname-index       (nth 3 tramp-multi-file-name-structure))
        (hop-regexp       (nth 0 tramp-multi-file-name-hop-structure))
        (hop-method-index (nth 1 tramp-multi-file-name-hop-structure))
        (hop-user-index   (nth 2 tramp-multi-file-name-hop-structure))
        (hop-host-index   (nth 3 tramp-multi-file-name-hop-structure))
        method hops len hop-methods hop-users hop-hosts localname)
    (unless (string-match (format regexp hop-regexp) name)
      (error "Not a multi tramp file name: %s" name))
    (setq method (match-string method-index name))
    (setq hops (match-string hops-index name))
    (setq len (/ (length (match-data t)) 2))
    (when (< localname-index 0) (incf localname-index len))
    (setq localname (match-string localname-index name))
    (let ((index 0))
      (while (string-match hop-regexp hops index)
        (setq index (match-end 0))
        (setq hop-methods
              (cons (match-string hop-method-index hops) hop-methods))
        (setq hop-users
              (cons (match-string hop-user-index hops) hop-users))
        (setq hop-hosts
              (cons (match-string hop-host-index hops) hop-hosts))))
    (make-tramp-file-name
     :multi-method method
     :method       (apply 'vector (reverse hop-methods))
     :user         (apply 'vector (reverse hop-users))
     :host         (apply 'vector (reverse hop-hosts))
     :localname         localname)))

(defun tramp-make-tramp-file-name (multi-method method user host localname)
  "Constructs a tramp file name from METHOD, USER, HOST and LOCALNAME."
  (if multi-method
      (tramp-make-tramp-multi-file-name multi-method method user host localname)
    (format-spec
     (concat tramp-prefix-format
      (when method (concat "%m" tramp-postfix-single-method-format))
      (when user   (concat "%u" tramp-postfix-user-format))
      (when host   (concat "%h" tramp-postfix-host-format))
      (when localname   (concat "%p")))
    `((?m . ,method) (?u . ,user) (?h . ,host) (?p . ,localname)))))

;; CCC: Henrik Holm: Not Changed.  Multi Method.  What should be done
;; with this when USER is nil?
(defun tramp-make-tramp-multi-file-name (multi-method method user host localname)
  "Constructs a tramp file name for a multi-hop method."
  (unless tramp-make-multi-tramp-file-format
    (error "`tramp-make-multi-tramp-file-format' is nil"))
  (let* ((prefix-format (nth 0 tramp-make-multi-tramp-file-format))
         (hop-format    (nth 1 tramp-make-multi-tramp-file-format))
         (localname-format   (nth 2 tramp-make-multi-tramp-file-format))
         (prefix (format-spec prefix-format `((?m . ,multi-method))))
         (hops "")
         (localname (format-spec localname-format `((?p . ,localname))))
         (i 0)
         (len (length method)))
    (while (< i len)
      (let ((m (aref method i)) (u (aref user i)) (h (aref host i)))
        (setq hops (concat hops (format-spec hop-format
					     `((?m . ,m) (?u . ,u) (?h . ,h)))))
        (incf i)))
    (concat prefix hops localname)))

(defun tramp-make-copy-program-file-name (user host localname)
  "Create a file name suitable to be passed to `rcp' and workalikes."
  (if user
      (format "%s@%s:%s" user host localname)
    (format "%s:%s" host localname)))

(defun tramp-method-out-of-band-p (multi-method method user host)
  "Return t if this is an out-of-band method, nil otherwise."
  (tramp-get-method-parameter
   multi-method
   (tramp-find-method multi-method method user host)
   user host 'tramp-copy-program))

;; Variables local to connection.

(defun tramp-get-ls-command (multi-method method user host)
  (save-excursion
    (tramp-maybe-open-connection multi-method method user host)
    (set-buffer (tramp-get-buffer multi-method method user host))
    tramp-ls-command))

(defun tramp-get-test-groks-nt (multi-method method user host)
  (save-excursion
    (tramp-maybe-open-connection multi-method method user host)
    (set-buffer (tramp-get-buffer multi-method method user host))
    tramp-test-groks-nt))

(defun tramp-get-file-exists-command (multi-method method user host)
  (save-excursion
    (tramp-maybe-open-connection multi-method method user host)
    (set-buffer (tramp-get-buffer multi-method method user host))
    tramp-file-exists-command))

(defun tramp-get-remote-perl (multi-method method user host)
  (tramp-get-connection-property "perl" nil multi-method method user host))

(defun tramp-get-remote-ln (multi-method method user host)
  (tramp-get-connection-property "ln" nil multi-method method user host))

;; Get a property of a TRAMP connection.
(defun tramp-get-connection-property
  (property default multi-method method user host)
  "Get the named property for the connection.
If the value is not set for the connection, return `default'"
  (tramp-maybe-open-connection multi-method method user host)
  (with-current-buffer (tramp-get-buffer multi-method method user host)
    (let (error)
      (condition-case nil
	  (symbol-value (intern (concat "tramp-connection-property-" property)))
	(error	default)))))

;; Set a property of a TRAMP connection.
(defun tramp-set-connection-property
  (property value multi-method method user host)
  "Set the named property of a TRAMP connection."
  (tramp-maybe-open-connection multi-method method user host)
  (with-current-buffer (tramp-get-buffer multi-method method user host)
    (set (make-local-variable
	  (intern (concat "tramp-connection-property-" property)))
	  value)))

;; Some predefined connection properties.
(defun tramp-set-remote-encoding (multi-method method user host rem-enc)
  (tramp-set-connection-property "remote-encoding" rem-enc
				 multi-method method user host))
(defun tramp-get-remote-encoding (multi-method method user host)
  (tramp-get-connection-property "remote-encoding" nil
				 multi-method method user host))

(defun tramp-set-remote-decoding (multi-method method user host rem-dec)
  (tramp-set-connection-property "remote-decoding" rem-dec
				 multi-method method user host))
(defun tramp-get-remote-decoding (multi-method method user host)
  (tramp-get-connection-property "remote-decoding" nil
				 multi-method method user host))

(defun tramp-set-local-encoding (multi-method method user host loc-enc)
  (tramp-set-connection-property "local-encoding" loc-enc
				 multi-method method user host))
(defun tramp-get-local-encoding (multi-method method user host)
  (tramp-get-connection-property "local-encoding" nil
				 multi-method method user host))

(defun tramp-set-local-decoding (multi-method method user host loc-dec)
  (tramp-set-connection-property "local-decoding" loc-dec
				 multi-method method user host))
(defun tramp-get-local-decoding (multi-method method user host)
  (tramp-get-connection-property "local-decoding" nil
				 multi-method method user host))

(defun tramp-get-method-parameter (multi-method method user host param)
  "Return the method parameter PARAM.
If the `tramp-methods' entry does not exist, use the variable PARAM
as default."
  (unless (boundp param)
    (error "Non-existing method parameter `%s'" param))
  (let ((entry (assoc param
		      (assoc (tramp-find-method multi-method method user host)
			     tramp-methods))))
    (if entry
	(second entry)
      (symbol-value param))))


;; Auto saving to a special directory.

(defun tramp-exists-file-name-handler (operation &rest args)
  (let ((buffer-file-name "/")
	(fnha file-name-handler-alist)
	(check-file-name-operation operation)
	(file-name-handler-alist
	 (list
	  (cons "/"
		'(lambda (operation &rest args)
		   "Returns OPERATION if it is the one to be checked"
		   (if (equal check-file-name-operation operation)
		       operation
		     (let ((file-name-handler-alist fnha))
		       (apply operation args))))))))
    (eq (apply operation args) operation)))

(unless (tramp-exists-file-name-handler 'make-auto-save-file-name)
  (defadvice make-auto-save-file-name
    (around tramp-advice-make-auto-save-file-name () activate)
    "Invoke `tramp-handle-make-auto-save-file-name' for tramp files."
    (if (and (buffer-file-name) (tramp-tramp-file-p (buffer-file-name)))
	(setq ad-return-value (tramp-handle-make-auto-save-file-name))
      ad-do-it)))

;; In Emacs < 22 and XEmacs < 21.5 autosaved remote files have
;; permission 0666 minus umask. This is a security threat.

(defun tramp-set-auto-save-file-modes ()
  "Set permissions of autosaved remote files to the original permissions."
  (let ((bfn (buffer-file-name)))
    (when (and (stringp bfn)
	       (tramp-tramp-file-p bfn)
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
  (add-hook 'auto-save-hook 'tramp-set-auto-save-file-modes))

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

(defun tramp-insert-with-face (face string)
  "Insert text with a specific face."
  (let ((start (point)))
    (insert string)
    (add-text-properties start (point) (list 'face face))))

;; ------------------------------------------------------------
;; -- Compatibility functions section --
;; ------------------------------------------------------------

(defun tramp-temporary-file-directory ()
  "Return name of directory for temporary files (compat function).
For Emacs, this is the variable `temporary-file-directory', for XEmacs
this is the function `temp-directory'."
  (cond ((boundp 'temporary-file-directory)
         (symbol-value 'temporary-file-directory))
        ((fboundp 'temp-directory)
         (funcall (symbol-function 'temp-directory))) ;pacify byte-compiler
        ((let ((d (getenv "TEMP"))) (and d (file-directory-p d)))
         (file-name-as-directory (getenv "TEMP")))
        ((let ((d (getenv "TMP"))) (and d (file-directory-p d)))
         (file-name-as-directory (getenv "TMP")))
        ((let ((d (getenv "TMPDIR"))) (and d (file-directory-p d)))
         (file-name-as-directory (getenv "TMPDIR")))
        ((file-exists-p "c:/temp") (file-name-as-directory "c:/temp"))
        (t (message (concat "Neither `temporary-file-directory' nor "
                            "`temp-directory' is defined -- using /tmp."))
           (file-name-as-directory "/tmp"))))

(defun tramp-read-passwd (user host prompt)
  "Read a password from user (compat function).
Invokes `password-read' if available, `read-passwd' else."
  (if (functionp 'password-read)
      (let* ((key (concat (or user (user-login-name)) "@" host))
	     (password (apply #'password-read (list prompt key))))
	(apply #'password-cache-add (list key password))
	password)
    (read-passwd prompt)))

(defun tramp-clear-passwd (&optional user host)
  "Clear password cache for connection related to current-buffer."
  (interactive)
  (let ((filename (or buffer-file-name list-buffers-directory "")))
    (when (and (functionp 'password-cache-remove)
	       (or (and user host) (tramp-tramp-file-p filename)))
      (let* ((v (when (tramp-tramp-file-p filename)
		  (tramp-dissect-file-name filename)))
	     (luser (or user (tramp-file-name-user v) (user-login-name)))
	     (lhost (or host (tramp-file-name-host v) (system-name)))
	     (key (concat luser "@" lhost)))
	(apply #'password-cache-remove (list key))))))

(defun tramp-time-diff (t1 t2)
  "Return the difference between the two times, in seconds.
T1 and T2 are time values (as returned by `current-time' for example).

NOTE: This function will fail if the time difference is too large to
fit in an integer."
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
         (floor (funcall
		 (symbol-function 'itimer-time-difference)
		 (if (< (length t1) 3) (append t1 '(0)) t1)
		 (if (< (length t2) 3) (append t2 '(0)) t2))))
        (t
         ;; snarfed from Emacs 21 time-date.el; combining
	 ;; time-to-seconds and subtract-time
	 (let ((time  (let ((borrow (< (cadr t1) (cadr t2))))
                 (list (- (car t1) (car t2) (if borrow 1 0))
                       (- (+ (if borrow 65536 0) (cadr t1)) (cadr t2))))))
	   (+ (* (car time) 65536.0)
	      (cadr time)
	      (/ (or (nth 2 time) 0) 1000000.0))))))

(defun tramp-coding-system-change-eol-conversion (coding-system eol-type)
  "Return a coding system like CODING-SYSTEM but with given EOL-TYPE.
EOL-TYPE can be one of `dos', `unix', or `mac'."
  (cond ((fboundp 'coding-system-change-eol-conversion)
         (apply #'coding-system-change-eol-conversion
                (list coding-system eol-type)))
        ((fboundp 'subsidiary-coding-system)
         (apply
          #'subsidiary-coding-system
          (list coding-system
                (cond ((eq eol-type 'dos) 'crlf)
                      ((eq eol-type 'unix) 'lf)
                      ((eq eol-type 'mac) 'cr)
                      (t
                       (error "Unknown EOL-TYPE `%s', must be %s"
                              eol-type
                              "`dos', `unix', or `mac'"))))))
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
 (funcall
  (if (fboundp 'set-process-query-on-exit-flag)
      (symbol-function 'set-process-query-on-exit-flag)
    (symbol-function 'process-kill-without-query))
  process flag))


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

;; ;; EFS hooks itself into the file name handling stuff in more places
;; ;; than just `file-name-handler-alist'. The following tells EFS to stay
;; ;; away from tramp.el file names.
;; ;;
;; ;; This is needed because EFS installs (efs-dired-before-readin) into
;; ;; 'dired-before-readin-hook'. This prevents EFS from opening an FTP
;; ;; connection to help it's dired process. Not that I have any real
;; ;; idea *why* this is helpful to dired.
;; ;;
;; ;; Anyway, this advice fixes the problem (with a sledgehammer :)
;; ;;
;; ;; Daniel Pittman <daniel@danann.net>
;; ;;
;; ;; CCC: when the other defadvice calls have disappeared, make sure
;; ;; not to call defadvice unless it's necessary.  How do we find out whether
;; ;; it is necessary?  (featurep 'efs) is surely the wrong way --
;; ;; EFS might nicht be loaded yet.
;; (defadvice efs-ftp-path (around dont-match-tramp-localname activate protect)
;;   "Cause efs-ftp-path to fail when the path is a TRAMP localname."
;;   (if (tramp-tramp-file-p (ad-get-arg 0))
;;       nil
;;     ad-do-it))

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
  (defadvice file-expand-wildcards (around tramp-fix activate)
    (let ((name (ad-get-arg 0)))
      (if (tramp-tramp-file-p name)
	  ;; If it's a Tramp file, dissect it and look if wildcards
	  ;; need to be expanded at all.
	  (let ((v (tramp-dissect-file-name name)))
	    (if (string-match "[[*?]" (tramp-file-name-localname v))
		(let ((res ad-do-it))
		  (setq ad-return-value (or res (list name))))
	      (setq ad-return-value (list name))))
	;; If it is not a Tramp file, just run the original function.
	(let ((res ad-do-it))
	  (setq ad-return-value (or res (list name))))))))

;; Tramp version is useful in a number of situations.

(defun tramp-version (arg)
  "Print version number of tramp.el in minibuffer or current buffer."
  (interactive "P")
  (if arg (insert tramp-version) (message tramp-version)))

;; Make the `reporter` functionality available for making bug reports about
;; the package. A most useful piece of code.

(unless (fboundp 'reporter-submit-bug-report)
  (autoload 'reporter-submit-bug-report "reporter"))

(defun tramp-bug ()
  "Submit a bug report to the TRAMP developers."
  (interactive)
  (require 'reporter)
  (catch 'dont-send
    (let ((reporter-prompt-for-summary-p t))
      (reporter-submit-bug-report
       tramp-bug-report-address		; to-address
       (format "tramp (%s)" tramp-version) ; package name and version
       (delq nil
	     `(;; Current state
	       tramp-ls-command
	       tramp-test-groks-nt
	       tramp-file-exists-command
	       tramp-current-multi-method
	       tramp-current-method
	       tramp-current-user
	       tramp-current-host

	       ;; System defaults
	       tramp-auto-save-directory        ; vars to dump
	       tramp-default-method
	       tramp-rsh-end-of-line
	       tramp-default-password-end-of-line
	       tramp-remote-path
	       tramp-login-prompt-regexp
	       ;; Mask non-7bit characters
	       (tramp-password-prompt-regexp . tramp-reporter-dump-variable)
	       tramp-wrong-passwd-regexp
	       tramp-yesno-prompt-regexp
	       tramp-yn-prompt-regexp
	       tramp-terminal-prompt-regexp
	       tramp-temp-name-prefix
	       tramp-file-name-structure
	       tramp-file-name-regexp
	       tramp-multi-file-name-structure
	       tramp-multi-file-name-hop-structure
	       tramp-multi-methods
	       tramp-multi-connection-function-alist
	       tramp-methods
	       tramp-end-of-output
	       tramp-coding-commands
	       tramp-actions-before-shell
	       tramp-actions-copy-out-of-band
	       tramp-multi-actions
	       tramp-terminal-type
	       ;; Mask non-7bit characters
	       (tramp-shell-prompt-pattern . tramp-reporter-dump-variable)
	       tramp-chunksize
	       ,(when (boundp 'tramp-backup-directory-alist)
		  'tramp-backup-directory-alist)
	       ,(when (boundp 'tramp-bkup-backup-directory-info)
		  'tramp-bkup-backup-directory-info)

	       ;; Non-tramp variables of interest
	       ;; Mask non-7bit characters
	       (shell-prompt-pattern . tramp-reporter-dump-variable)
	       backup-by-copying
	       backup-by-copying-when-linked
	       backup-by-copying-when-mismatch
	       ,(when (boundp 'backup-by-copying-when-privileged-mismatch)
		  'backup-by-copying-when-privileged-mismatch)
	       ,(when (boundp 'password-cache)
		  'password-cache)
	       ,(when (boundp 'password-cache-expiry)
		  'password-cache-expiry)
	       ,(when (boundp 'backup-directory-alist)
		  'backup-directory-alist)
	       ,(when (boundp 'bkup-backup-directory-info)
		  'bkup-backup-directory-info)
	       file-name-handler-alist))

       'tramp-load-report-modules	; pre-hook
       'tramp-append-tramp-buffers	; post-hook
       "\
Enter your bug report in this message, including as much detail as you
possibly can about the problem, what you did to cause it and what the
local and remote machines are.

If you can give a simple set of instructions to make this bug happen
reliably, please include those.  Thank you for helping kill bugs in
TRAMP.

Another useful thing to do is to put (setq tramp-debug-buffer t) in
the ~/.emacs file and to repeat the bug.  Then, include the contents
of the *tramp/foo* buffer and the *debug tramp/foo* buffer in your bug
report.

--bug report follows this line--
"))))

(defun tramp-reporter-dump-variable (varsym mailbuf)
  "Pretty-print the value of the variable in symbol VARSYM.
Used for non-7bit chars in strings."
  (let* ((reporter-eval-buffer (symbol-value 'reporter-eval-buffer))
	 (val (with-current-buffer reporter-eval-buffer
		(symbol-value varsym))))

    ;; There are characters to be masked.
    (when (and (boundp 'mm-7bit-chars)
	       (string-match
		(concat "[^" (symbol-value 'mm-7bit-chars) "]") val))
      (with-current-buffer reporter-eval-buffer
	(set varsym (concat "(base64-decode-string \""
			    (base64-encode-string val)
			    "\")"))))

    ;; Dump variable.
    (funcall (symbol-function 'reporter-dump-variable) varsym mailbuf)

    ;; Remove string quotation.
    (forward-line -1)
    (when (looking-at
	   (concat "\\(^.*\\)" "\""                       ;; \1 "
		   "\\((base64-decode-string \\)" "\\\\"  ;; \2 \
		   "\\(\".*\\)" "\\\\"                    ;; \3 \
		   "\\(\")\\)" "\"$"))                    ;; \4 "
      (replace-match "\\1\\2\\3\\4")
      (beginning-of-line)
      (insert " ;; variable encoded due to non-printable characters\n"))
    (forward-line 1)

    ;; Reset VARSYM to old value.
    (with-current-buffer reporter-eval-buffer
      (set varsym val))))

(defun tramp-load-report-modules ()
  "Load needed modules for reporting."

  ;; We load message.el and mml.el from Gnus.
  (if (featurep 'xemacs)
      (progn
	(load "message" 'noerror)
	(load "mml" 'noerror))
    (require 'message nil 'noerror)
    (require 'mml nil 'noerror))
  (when (functionp 'message-mode)
    (funcall (symbol-function 'message-mode)))
  (when (functionp 'mml-mode)
    (funcall (symbol-function 'mml-mode) t)))

(defun tramp-append-tramp-buffers ()
  "Append Tramp buffers into the bug report."

  (when (and
	 (eq major-mode 'message-mode)
	 (boundp 'mml-mode)
	 (symbol-value 'mml-mode))

    (let* ((tramp-buf-regexp "\\*\\(debug \\)?tramp/")
	   (buffer-list
	    (delq nil
		  (mapcar '(lambda (b)
		     (when (string-match tramp-buf-regexp (buffer-name b)) b))
			  (buffer-list))))
	   (curbuf (current-buffer)))

      ;; There is at least one Tramp buffer.
      (when buffer-list
	(switch-to-buffer (list-buffers-noselect nil))
	(delete-other-windows)
	(setq buffer-read-only nil)
	(goto-char (point-min))
	(while (not (eobp))
	  (if (re-search-forward tramp-buf-regexp (tramp-point-at-eol) t)
	      (forward-line 1)
	    (forward-line 0)
	    (let ((start (point)))
	      (forward-line 1)
	      (kill-region start (point)))))
	(insert "
The buffer(s) above will be appended to this message.  If you don't want
to append a buffer because it contains sensible data, or because the buffer
is too large, you should delete the respective buffer.  The buffer(s) will
contain user and host names.  Passwords will never be included there.")

	(when (and tramp-debug-buffer (> tramp-verbose 9))
	  (insert "\n\n")
	  (let ((start (point)))
	    (insert "\
Please note that you have set `tramp-verbose' to a value greater than 9.
Therefore, the contents of files might be included in the debug buffer(s).")
	    (add-text-properties start (point) (list 'face 'italic))))

	(set-buffer-modified-p nil)
	(setq buffer-read-only t)
	(goto-char (point-min))

	(if (y-or-n-p "Do you want to append the buffer(s)? ")
	    ;; OK, let's send.  First we delete the buffer list.
	    (progn
	      (kill-buffer nil)
	      (switch-to-buffer curbuf)
	      (goto-char (point-max))
	      (insert "\n\n")
	      (dolist (buffer buffer-list)
		(funcall (symbol-function 'mml-insert-empty-tag)
			 'part 'type "text/plain" 'encoding "base64"
			 'disposition "attachment" 'buffer (buffer-name buffer)
			 'description (buffer-name buffer)))
	      (set-buffer-modified-p nil))

	  ;; Don't send.  Delete the message buffer.
	  (set-buffer curbuf)
	  (set-buffer-modified-p nil)
	  (kill-buffer nil)
	  (throw 'dont-send nil))))))

(defalias 'tramp-submit-bug 'tramp-bug)

(provide 'tramp)

;; Make sure that we get integration with the VC package.
;; When it is loaded, we need to pull in the integration module.
;; This must come after (provide 'tramp) because tramp-vc.el
;; requires tramp.
(eval-after-load "vc"
  '(require 'tramp-vc))

;;; TODO:

;; * Allow putting passwords in the filename.
;;   This should be implemented via a general mechanism to add
;;   parameters in filenames.  There is currently a kludge for
;;   putting the port number into the filename for ssh and ftp
;;   files.  This could be subsumed by the new mechanism as well.
;;   Another approach is to read a netrc file like ~/.authinfo
;;   from Gnus.
;; * Handle nonlocal exits such as C-g.
;; * Autodetect if remote `ls' groks the "--dired" switch.
;; * Add fallback for inline encodings.  This should be used
;;   if the remote end doesn't support mimencode or a similar program.
;;   For reading files from the remote host, we can just parse the output
;;   of `od -b'.  For writing files to the remote host, we construct
;;   a shell program which contains only "safe" ascii characters
;;   and which writes the right bytes to the file.  We can use printf(1)
;;   or "echo -e" or the printf function in awk and use octal escapes
;;   for the "dangerous" characters.  The null byte might be a problem.
;;   On some systems, the octal escape doesn't work.  So we try the following
;;   two commands to write a null byte:
;;   dd if=/dev/zero bs=1 count=1
;;   echo | tr '\n' '\000'
;; * Separate local `tramp-coding-commands' from remote ones.  Connect
;;   the two via a format which can be `uu' or `b64'.  Then we can search
;;   for the right local commands and the right remote commands separately.
;; * Cooperate with PCL-CVS.  It uses start-process, which doesn't
;;   work for remote files.
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
;; * Find out about the new auto-save mechanism in Emacs 21 and
;;   do the right thing.
;; * `vc-directory' does not work.  It never displays any files, even
;;   if it does show files when run locally.
;; * Allow correction of passwords, if the remote end allows this.
;;   (Mark Hershberger)
;; * How to deal with MULE in `insert-file-contents' and `write-region'?
;; * Do asynchronous `shell-command's.
;; * Grok `append' parameter for `write-region'.
;; * Test remote ksh or bash for tilde expansion in `tramp-find-shell'?
;; * abbreviate-file-name
;; * grok ~ in tramp-remote-path  (Henrik Holm <henrikh@tele.ntnu.no>)
;; * Also allow to omit user names when doing multi-hop.  Not sure yet
;;   what the user names should default to, though.
;; * better error checking.  At least whenever we see something
;;   strange when doing zerop, we should kill the process and start
;;   again.  (Greg Stark)
;; * Add caching for filename completion.  (Greg Stark)
;;   Of course, this has issues with usability (stale cache bites)
;;      -- <daniel@danann.net>
;; * Provide a local cache of old versions of remote files for the rsync
;;   transfer method to use.  (Greg Stark)
;; * Remove unneeded parameters from methods.
;; * Invoke rsync once for copying a whole directory hierarchy.
;;   (Francesco Potort,Al(B)
;; * Should we set PATH ourselves or should we rely on the remote end
;;   to do it?
;; * Make it work for XEmacs 20, which is missing `with-timeout'.
;; * Make it work for different encodings, and for different file name
;;   encodings, too.  (Daniel Pittman)
;; * Change applicable functions to pass a struct tramp-file-name rather
;;   than the individual items MULTI-METHOD, METHOD, USER, HOST, LOCALNAME.
;; * Implement asynchronous shell commands.
;; * Clean up unused *tramp/foo* buffers after a while.  (Pete Forman)
;; * Progress reports while copying files.  (Michael Kifer)
;; * `Smart' connection method that uses inline for small and out of
;;   band for large files.  (Michael Kifer)
;; * Don't search for perl5 and perl.  Instead, only search for perl and
;;   then look if it's the right version (with `perl -v').
;; * When editing a remote CVS controlled file as a different user, VC
;;   gets confused about the file locking status.  Try to find out why
;;   the workaround doesn't work.
;; * Change `copy-file' to grok the case where the filename handler
;;   for the source and the target file are different.  Right now,
;;   it looks at the source file and then calls that handler, if
;;   there is one.  But since ange-ftp, for instance, does not know
;;   about Tramp, it does not do the right thing if the target file
;;   name is a Tramp name.
;; * Username and hostname completion.
;; ** If `partial-completion-mode' isn't loaded, "/foo:bla" tries to
;;    connect to host "blabla" already if that host is unique. No idea
;;    how to suppress. Maybe not an essential problem.
;; ** Try to avoid usage of `last-input-event' in `tramp-completion-mode'.
;; ** Extend `tramp-get-completion-su' for NIS and shadow passwords.
;; ** Unify `tramp-parse-{rhosts,shosts,sconfig,hosts,passwd,netrc}'.
;;    Code is nearly identical.
;; ** Decide whiche files to take for searching user/host names depending on
;;    operating system (windows-nt) in `tramp-completion-function-alist'.
;; ** Enhance variables for debug.
;; ** Implement "/multi:" completion.
;; ** Add a learning mode for completion. Make results persistent.
;; * Allow out-of-band methods as _last_ multi-hop.

;; Functions for file-name-handler-alist:
;; diff-latest-backup-file -- in diff.el
;; dired-uncache -- this will be needed when we do insert-directory caching
;; file-name-as-directory -- use primitive?
;; file-name-sans-versions -- use primitive?
;; get-file-buffer -- use primitive
;; vc-registered

;;; arch-tag: 3a21a994-182b-48fa-b0cd-c1d9fede424a
;;; tramp.el ends here
