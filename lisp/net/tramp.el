;;; tramp.el --- Transparent Remote Access, Multiple Protocol -*- coding: iso-8859-1; -*- 

;; Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

;; Author: Kai.Grossjohann@CS.Uni-Dortmund.DE 
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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides remote file editing, similar to ange-ftp.
;; The difference is that ange-ftp uses FTP to transfer files between
;; the local and the remote host, whereas tramp.el uses a combination
;; of rsh and rcp or other work-alike programs, such as ssh/scp.
;;
;; For more detailed instructions, please see the info file, which is
;; included in the file `tramp.tar.gz' mentioned below.
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
;; The current version of tramp.el can be retrieved from the following
;; URL:  ftp://ls6-ftp.cs.uni-dortmund.de/pub/src/emacs/tramp.tar.gz
;; For your convenience, the *.el file is available separately from
;; the same directory.
;;
;; There's a mailing list for this, as well.  Its name is:
;;                tramp-devel@mail.freesoftware.fsf.org
;; Send a mail with `help' in the subject (!) to the administration
;; address for instructions on joining the list.  The administration
;; address is:
;;            tramp-devel-request@mail.freesoftware.fsf.org
;; You can also use the Web to subscribe, under the following URL:
;;            http://mail.freesoftware.fsf.org/mailman/listinfo/tramp-devel
;;
;; For the adventurous, the current development sources are available
;; via CVS.  You can find instructions about this at the following URL:
;;            http://savannah.gnu.org/projects/tramp/
;; Click on "CVS" in the navigation bar near the top.
;;
;; Don't forget to put on your asbestos longjohns, first!

;;; Code:

(defconst tramp-version "2.0.5"
  "This version of tramp.")

(defconst tramp-bug-report-address "tramp-devel@mail.freesoftware.fsf.org"
  "Email address to send bug reports to.")

(require 'timer)
(require 'format-spec)                  ;from Gnus 5.8, also in tar ball
(require 'base64)                       ;for the mimencode methods
(require 'shell)
(require 'advice)

;; ;; It does not work to load EFS after loading TRAMP.  
;; (when (fboundp 'efs-file-handler-function)
;;   (require 'efs))

(eval-when-compile
  (require 'cl)
  (require 'custom)
  ;; Emacs 19.34 compatibility hack -- is this needed?
  (or (>= emacs-major-version 20)
      (load "cl-seq")))

(unless (boundp 'custom-print-functions)
  (defvar custom-print-functions nil))	; not autoloaded before Emacs 20.4

;;; User Customizable Internal Variables:

(defgroup tramp nil
  "Edit remote files with a combination of rsh and rcp or similar programs."
  :group 'files)

(defcustom tramp-verbose 10
  "*Verbosity level for tramp.el.  0 means be silent, 10 is most verbose."
  :group 'tramp
  :type 'integer)

(defcustom tramp-debug-buffer nil
  "*Whether to send all commands and responses to a debug buffer."
  :group 'tramp
  :type 'boolean)

(defcustom tramp-auto-save-directory nil
  "*Put auto-save files in this directory, if set.
The idea is to use a local directory so that auto-saving is faster."
  :group 'tramp
  :type '(choice (const nil)
                 string))

(defcustom tramp-sh-program "/bin/sh"
  "*Use this program for shell commands on the local host.
This MUST be a Bourne-like shell.  This shell is used to execute
the encoding and decoding command on the local host, so if you
want to use `~' in those commands, you should choose a shell here
which groks tilde expansion.  `/bin/sh' normally does not
understand tilde expansion.

Note that this variable is not used for remote commands.  There are
mechanisms in tramp.el which automatically determine the right shell to
use for the remote host."
  :group 'tramp
  :type '(file :must-match t))

(defcustom tramp-multi-sh-program
  (if (memq system-type '(windows-nt))
      "cmd.exe"
    tramp-sh-program)
  "*Use this program for bootstrapping multi-hop connections.
This variable is similar to `tramp-sh-program', but it is only used
when initializing a multi-hop connection.  Therefore, the set of
commands sent to this shell is quite restricted, and if you are
careful it works to use CMD.EXE under Windows (instead of a Bourne-ish
shell which does not normally exist on Windows anyway).

To use multi-hop methods from Windows, you also need suitable entries
in `tramp-multi-connection-function-alist' for the first hop.

This variable defaults to CMD.EXE on Windows NT, and to the value of
`tramp-sh-program' on other systems."
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
              (tramp-rsh-program          "rsh")
              (tramp-rcp-program          "rcp")
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             nil)
              (tramp-rcp-args             nil)
              (tramp-rcp-keep-date-arg    "-p")
              (tramp-su-program           nil)
              (tramp-su-args              nil)
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("scp"   (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-rsh-program          "ssh")
              (tramp-rcp-program          "scp")
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             ("-e" "none"))
              (tramp-rcp-args             nil)
              (tramp-rcp-keep-date-arg    "-p")
              (tramp-su-program           nil)
              (tramp-su-args              nil)
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("scp1"  (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-rsh-program          "ssh")
              (tramp-rcp-program          "scp")
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             ("-1" "-e" "none"))
              (tramp-rcp-args             ("-1"))
              (tramp-rcp-keep-date-arg    "-p")
              (tramp-su-program           nil)
              (tramp-su-args              nil)
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("scp2"  (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-rsh-program          "ssh")
              (tramp-rcp-program          "scp")
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             ("-2" "-e" "none"))
              (tramp-rcp-args             ("-2"))
              (tramp-rcp-keep-date-arg    "-p")
              (tramp-su-program           nil)
              (tramp-su-args              nil)
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("scp1-old"
              (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-rsh-program          "ssh1")
              (tramp-rcp-program          "scp1")
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             ("-e" "none"))
              (tramp-rcp-args             nil)
              (tramp-rcp-keep-date-arg    "-p")
              (tramp-su-program           nil)
              (tramp-su-args              nil)
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("scp2-old"
              (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-rsh-program          "ssh2")
              (tramp-rcp-program          "scp2")
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             ("-e" "none"))
              (tramp-rcp-args             nil)
              (tramp-rcp-keep-date-arg    "-p")
              (tramp-su-program           nil)
              (tramp-su-args              nil)
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("rsync" (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-rsh-program          "ssh")
              (tramp-rcp-program          "rsync")
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             ("-e" "none"))
              (tramp-rcp-args             ("-e" "ssh"))
              (tramp-rcp-keep-date-arg    "-t")
              (tramp-su-program           nil)
              (tramp-su-args              nil)
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("rsh"   (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-rsh-program          "rsh")
              (tramp-rcp-program          nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             nil)
              (tramp-rcp-args             nil)
              (tramp-rcp-keep-date-arg    nil)
              (tramp-su-program           nil)
              (tramp-su-args              nil)
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("ssh"   (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-rsh-program          "ssh")
              (tramp-rcp-program          nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             ("-e" "none"))
              (tramp-rcp-args             nil)
              (tramp-rcp-keep-date-arg    nil)
              (tramp-su-program           nil)
              (tramp-su-args              nil)
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("ssh1"  (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-rsh-program          "ssh")
              (tramp-rcp-program          nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             ("-1" "-e" "none"))
              (tramp-rcp-args             ("-1"))
              (tramp-rcp-keep-date-arg    nil)
              (tramp-su-program           nil)
              (tramp-su-args              nil)
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("ssh2"  (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-rsh-program          "ssh")
              (tramp-rcp-program          nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             ("-2" "-e" "none"))
              (tramp-rcp-args             ("-2"))
              (tramp-rcp-keep-date-arg    nil)
              (tramp-su-program           nil)
              (tramp-su-args              nil)
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("ssh1-old"
              (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-rsh-program          "ssh1")
              (tramp-rcp-program          nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             ("-e" "none"))
              (tramp-rcp-args             nil)
              (tramp-rcp-keep-date-arg    nil)
              (tramp-su-program           nil)
              (tramp-su-args              nil)
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("ssh2-old"
              (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-rsh-program          "ssh2")
              (tramp-rcp-program          nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             ("-e" "none"))
              (tramp-rcp-args             nil)
              (tramp-rcp-keep-date-arg    nil)
              (tramp-su-program           nil)
              (tramp-su-args              nil)
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("ssh1-old"
              (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-rsh-program          "ssh1")
              (tramp-rcp-program          nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             ("-e" "none"))
              (tramp-rcp-args             nil)
              (tramp-rcp-keep-date-arg    nil)
              (tramp-su-program           nil)
              (tramp-su-args              nil)
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("ssh2-old"
              (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-rsh-program          "ssh2")
              (tramp-rcp-program          nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             ("-e" "none"))
              (tramp-rcp-args             nil)
              (tramp-rcp-keep-date-arg    nil)
              (tramp-su-program           nil)
              (tramp-su-args              nil)
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("telnet"
              (tramp-connection-function  tramp-open-connection-telnet)
              (tramp-rsh-program          nil)
              (tramp-rcp-program          nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             nil)
              (tramp-rcp-args             nil)
              (tramp-rcp-keep-date-arg    nil)
              (tramp-su-program           nil)
              (tramp-su-args              nil)
              (tramp-telnet-program       "telnet")
              (tramp-telnet-args          nil))
     ("su"    (tramp-connection-function  tramp-open-connection-su)
              (tramp-rsh-program          nil)
              (tramp-rcp-program          nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             nil)
              (tramp-rcp-args             nil)
              (tramp-rcp-keep-date-arg    nil)
              (tramp-su-program           "su")
              (tramp-su-args              ("-" "%u"))
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("sudo"  (tramp-connection-function  tramp-open-connection-su)
              (tramp-rsh-program          nil)
              (tramp-rcp-program          nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             nil)
              (tramp-rcp-args             nil)
              (tramp-rcp-keep-date-arg    nil)
              (tramp-su-program           "sudo")
              (tramp-su-args              ("-u" "%u" "-s"))
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("multi" (tramp-connection-function  tramp-open-connection-multi)
              (tramp-rsh-program          nil)
              (tramp-rcp-program          nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             nil)
              (tramp-rcp-args             nil)
              (tramp-rcp-keep-date-arg    nil)
              (tramp-su-program           nil)
              (tramp-su-args              nil)
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("scpx"  (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-rsh-program          "ssh")
              (tramp-rcp-program          "scp")
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             ("-e" "none" "-t" "-t" "/bin/sh"))
              (tramp-rcp-args             nil)
              (tramp-rcp-keep-date-arg    "-p")
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("sshx"  (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-rsh-program          "ssh")
              (tramp-rcp-program          nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-rsh-args             ("-e" "none" "-t" "-t" "/bin/sh"))
              (tramp-rcp-args             nil)
              (tramp-rcp-keep-date-arg    nil)
              (tramp-su-program           nil)
              (tramp-su-args              nil)
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("krlogin"
              (tramp-connection-function  tramp-open-connection-rsh)
	      (tramp-rsh-program          "krlogin")
	      (tramp-rcp-program          nil)
	      (tramp-remote-sh            "/bin/sh")
	      (tramp-rsh-args             ("-x"))
	      (tramp-rcp-args             nil)
	      (tramp-rcp-keep-date-arg    nil)
	      (tramp-su-program           nil)
	      (tramp-su-args              nil)
	      (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("plink"
              (tramp-connection-function  tramp-open-connection-rsh)
	      (tramp-rsh-program          "plink")
	      (tramp-rcp-program          nil)
	      (tramp-remote-sh            "/bin/sh")
	      (tramp-rsh-args             ("-ssh")) ;optionally add "-v"
	      (tramp-rcp-args             nil)
	      (tramp-rcp-keep-date-arg    nil)
	      (tramp-su-program           nil)
	      (tramp-su-args              nil)
	      (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("pscp"
              (tramp-connection-function  tramp-open-connection-rsh)
	      (tramp-rsh-program          "plink")
	      (tramp-rcp-program          "pscp")
	      (tramp-remote-sh            "/bin/sh")
	      (tramp-rsh-args             ("-ssh"))
	      (tramp-rcp-args             nil)
	      (tramp-rcp-keep-date-arg    "-p")
	      (tramp-su-program           nil)
	      (tramp-su-args              nil)
	      (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
     ("fcp"   
	      (tramp-connection-function  tramp-open-connection-rsh)
              (tramp-rsh-program          "fsh")
              (tramp-rcp-program          "fcp")
              (tramp-remote-sh            "/bin/sh -i")
              (tramp-rsh-args             ("sh" "-i"))
              (tramp-rcp-args             nil)
              (tramp-rcp-keep-date-arg    "-p")
              (tramp-su-program           nil)
              (tramp-su-args              nil)
              (tramp-telnet-program       nil)
              (tramp-telnet-args          nil))
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
  * `tramp-rsh-program'
    This specifies the name of the program to use for rsh; this might be
    the full path to rsh or the name of a workalike program.
  * `tramp-rsh-args'
    This specifies the list of arguments to pass to the above
    mentioned program.  Please note that this is a list of arguments,
    that is, normally you don't want to put \"-a -b\" or \"-f foo\"
    here.  Instead, you want two list elements, one for \"-a\" and one
    for \"-b\", or one for \"-f\" and one for \"foo\".
  * `tramp-rcp-program'
    This specifies the name of the program to use for rcp; this might be
    the full path to rcp or the name of a workalike program.
  * `tramp-rcp-args'
    This specifies the list of parameters to pass to the above mentioned
    program, the hints for `tramp-rsh-args' also apply here.
  * `tramp-rcp-keep-date-arg'
    This specifies the parameter to use for `rcp' when the timestamp
    of the original file should be kept.  For `rcp', use `-p', for
    `rsync', use `-t'.
  * `tramp-su-program'
    This specifies the name of the program to use for `su'.
  * `tramp-su-args'
    This specifies the list of arguments to pass to `su'.
    \"%u\" is replaced by the user name, use \"%%\" for a literal
    percent character.
  * `tramp-encoding-command'
    This specifies a command to use to encode the file contents for
    transfer.  The command should read the raw file contents from
    standard input and write the encoded file contents to standard
    output.  In this string, the percent escape \"%f\" should be used
    to indicate the file to convert.  Use \"%%\" if you need a literal
    percent character in your command.
  * `tramp-decoding-command'
    This specifies a command to use to decode file contents encoded
    with `tramp-encoding-command'.  The command should read from standard
    input and write to standard output.
  * `tramp-encoding-function'
    This specifies a function to be called to encode the file contents
    on the local side.  This function should accept two arguments
    START and END, the beginning and end of the region to encode.  The
    region should be replaced with the encoded contents.
  * `tramp-decoding-function'
    Same for decoding on the local side.
  * `tramp-telnet-program'
    Specifies the telnet program to use when using
    `tramp-open-connection-telnet' to log in.
  * `tramp-telnet-args'
    Specifies list of arguments to pass to `telnet'.  The hints for
    `tramp-rsh-args' also apply here.

What does all this mean?  Well, you should specify `tramp-rsh-program',
`tramp-telnet-program' or `tramp-su-program' for all methods; this program
is used to log in to the remote site.  Then, there are two ways to
actually transfer the files between the local and the remote side.
One way is using an additional rcp-like program.  If you want to do
this, set `tramp-rcp-program' in the method.

Another possibility for file transfer is inline transfer, i.e. the
file is passed through the same buffer used by `tramp-rsh-program'.  In
this case, the file contents need to be protected since the
`tramp-rsh-program' might use escape codes or the connection might not
be eight-bit clean.  Therefore, file contents are encoded for transit.

Two possibilities for encoding are uuencode/uudecode and mimencode.
For uuencode/uudecode you want to set `tramp-encoding-command' to
something like \"uuencode\" and `tramp-decoding-command' to \"uudecode
-p\".  For mimencode you want to set `tramp-encoding-command' to
something like \"mimencode -b\" and `tramp-decoding-command' to
\"mimencode -b -u\".

When using inline transfer, you can use a program or a Lisp function
on the local side to encode or decode the file contents.  Set the
`tramp-encoding-function' and `tramp-decoding-function' parameters to nil
in order to use the commands or to the function to use.  It is
possible to specify one function and the other parameter as nil.

So, to summarize: if the method is an inline method, you must specify
`tramp-encoding-command' and `tramp-decoding-command', and
`tramp-rcp-program' must be nil.  If the method is out of band, then
you must specify `tramp-rcp-program' and `tramp-rcp-args' and
`tramp-encoding-command' and `tramp-decoding-command' must be nil.
Every method, inline or out of band, must specify
`tramp-connection-function' plus the associated arguments (for
example, the telnet program if you chose
`tramp-open-connection-telnet').

Notes:

When using `tramp-open-connection-su' the phrase `open connection to a
remote host' sounds strange, but it is used nevertheless, for
consistency.  No connection is opened to a remote host, but `su' is
started on the local host.  You are not allowed to specify a remote
host other than `localhost' or the name of the local host.

Using a uuencode/uudecode inline method is discouraged, please use one
of the base64 methods instead since base64 encoding is much more
reliable and the commands are more standardized between the different
Unix versions.  But if you can't use base64 for some reason, please
note that the default uudecode command does not work well for some
Unices, in particular AIX and Irix.  For AIX, you might want to use
the following command for uudecode:

    sed '/^begin/d;/^[` ]$/d;/^end/d' | iconv -f uucode -t ISO8859-1

For Irix, no solution is known yet."
  :group 'tramp
  :type '(repeat
          (cons string
                (set (list (const tramp-connection-function) function)
                     (list (const tramp-rsh-program)
			   (choice (const nil) string))
                     (list (const tramp-rcp-program)
			   (choice (const nil) string))
                     (list (const tramp-remote-sh)
			   (choice (const nil) string))
                     (list (const tramp-rsh-args) (repeat string))
                     (list (const tramp-rcp-args) (repeat string))
                     (list (const tramp-rcp-keep-date-arg)
			   (choice (const nil) string))
                     (list (const tramp-su-program)
			   (choice (const nil) string))
                     (list (const tramp-su-args) (repeat string))
                     (list (const tramp-encoding-command)
			   (choice (const nil) string))
                     (list (const tramp-decoding-command)
			   (choice (const nil) string))
                     (list (const tramp-encoding-function)
			   (choice (const nil) function))
                     (list (const tramp-decoding-function)
			   (choice (const nil) function))
                     (list (const tramp-telnet-program)
			   (choice (const nil) string))
                     (list (const tramp-telnet-args) (repeat string))))))

(defcustom tramp-multi-methods '("multi" "multiu")
  "*List of multi-hop methods.
Each entry in this list should be a method name as mentioned in the
variable `tramp-methods'."
  :group 'tramp
  :type '(repeat string))

(defcustom tramp-multi-connection-function-alist
  '(("telnet" tramp-multi-connect-telnet "telnet %h%n")
    ("rsh"    tramp-multi-connect-rlogin "rsh %h -l %u%n")
    ("ssh"    tramp-multi-connect-rlogin "ssh %h -l %u%n")
    ("su"     tramp-multi-connect-su     "su - %u%n")
    ("sudo"   tramp-multi-connect-su     "sudo -u %u -s%n"))
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

(defcustom tramp-default-method "sm"
  ;;(if (featurep 'xemacs) "sm" "ftp")
  "*Default method to use for transferring files.
See `tramp-methods' for possibilities.
Also see `tramp-default-method-alist'.

Emacs uses a unified filename syntax for Tramp and Ange-FTP.
For backward compatibility, the default value of this variable
is \"ftp\" on Emacs.  But XEmacs uses a separate filename syntax
for Tramp and EFS, so there the default method is \"sm\"."
  :group 'tramp
  :type 'string)

(defcustom tramp-default-method-alist
  (if (featurep 'xemacs)
      nil
    '(("\\`ftp\\." "" "ftp")
      ("" "\\`\\(anonymous\\|ftp\\)\\'" "ftp")))
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

(defcustom tramp-ftp-method "ftp"
  "*When this method name is used, forward all calls to Ange-FTP."
  :group 'tramp
  :type 'string)

(defcustom tramp-rsh-end-of-line "\n"
  "*String used for end of line in rsh connections.
I don't think this ever needs to be changed, so please tell me about it
if you need to change this."
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
  ".*ogin: *"
  "*Regexp matching login-like prompts.
The regexp should match at end of buffer."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-password-prompt-regexp
  "^.*\\([pP]assword\\|passphrase.*\\):\^@? *"
  "*Regexp matching password-like prompts.
The regexp should match at end of buffer.

The `sudo' program appears to insert a `^@' character into the prompt."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-wrong-passwd-regexp
  (concat "^.*\\(Permission denied.\\|Login [Ii]ncorrect\\|"
          "Received signal [0-9]+\\|Connection \\(refused\\|closed\\)\\|"
          "Sorry, try again.\\|Name or service not known\\).*")
  "*Regexp matching a `login failed' message.
The regexp should match at end of buffer."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-yesno-prompt-regexp
  "Are you sure you want to continue connecting (yes/no)\\? *"
  "Regular expression matching all queries which need to be confirmed.
The confirmation should be done with yes or no.
The regexp should match at end of buffer."
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

(defcustom tramp-sh-extra-args '(("/bash\\'" . "--norc"))
  "*Alist specifying extra arguments to pass to the remote shell.
Entries are (REGEXP . ARGS) where REGEXP is a regular expression
matching the shell file name and ARGS is a string specifying the
arguments.

This variable is only used when Tramp needs to start up another shell
for tilde expansion.  The extra arguments should typically prevent the
shell from reading its init file."
  :group 'tramp
  :type '(alist :key-type string :value-type string))

;; File name format.

(defconst tramp-file-name-structure-unified
  (list (concat "\\`/\\(\\([a-zA-Z0-9-]+\\):\\)?" ;method
		      "\\(\\([^:@/]+\\)@\\)?" ;user
		      "\\([^:/]+\\):"	;host
		      "\\(.*\\)\\'")	;path
	      2 4 5 6)
  "Default value for `tramp-file-name-structure' for unified remoting.
On Emacs (not XEmacs), the Tramp and Ange-FTP packages use a unified
filename space.  This value is used for this unified namespace.")

(defconst tramp-file-name-structure-separate
  (list (concat "\\`/\\[\\(\\([a-zA-Z0-9-]+\\)/\\)?" ;method
		"\\(\\([-a-zA-Z0-9_#/:]+\\)@\\)?" ;user
		"\\([-a-zA-Z0-9_#/:@.]+\\)\\]" ;host
		"\\(.*\\)\\'")		;path
        2 4 5 6)
  "Default value for `tramp-file-name-structure' for separate remoting.
On XEmacs, the Tramp and EFS packages use a separate namespace for
remote filenames.  This value is used in that case.  It is designed
not to clash with the EFS filename syntax.")

(defcustom tramp-file-name-structure
  (if (featurep 'xemacs)
      tramp-file-name-structure-separate
    tramp-file-name-structure-unified)
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

See also `tramp-file-name-regexp' and `tramp-make-tramp-file-format'."
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
  (if (featurep 'xemacs)
      tramp-file-name-regexp-separate
    tramp-file-name-regexp-unified)
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

Also see `tramp-file-name-structure' and `tramp-make-tramp-file-format'."
  :group 'tramp
  :type 'regexp)

(defconst tramp-make-tramp-file-format-unified
   "/%m:%u@%h:%p"
   "Value for `tramp-make-tramp-file-format' for unified remoting.
Emacs (not XEmacs) uses a unified filename syntax for Ange-FTP and Tramp.
See `tramp-file-name-structure-unified' for more details.")

(defconst tramp-make-tramp-file-format-separate
  "/[%m/%u@%h]%p"
  "Value for `tramp-make-tramp-file-format' for separate remoting.
XEmacs uses a separate filename syntax for EFS and Tramp.
See `tramp-file-name-structure-separate' for more details.")

(defcustom tramp-make-tramp-file-format
  (if (featurep 'xemacs)
      tramp-make-tramp-file-format-separate
    tramp-make-tramp-file-format-unified)
  "*Format string saying how to construct tramp file name.
`%m' is replaced by the method name.
`%u' is replaced by the user name.
`%h' is replaced by the host name.
`%p' is replaced by the file name.
`%%' is replaced by %.

Also see `tramp-file-name-structure' and `tramp-file-name-regexp'."
  :group 'tramp
  :type 'string)

(defconst tramp-make-tramp-file-user-nil-format-unified
  "/%m:%h:%p"
  "Value of `tramp-make-tramp-file-user-nil-format' for unified remoting.
Emacs (not XEmacs) uses a unified filename syntax for Ange-FTP and Tramp.
See `tramp-file-name-structure-unified' for details.")

(defconst tramp-make-tramp-file-user-nil-format-separate
  "/[%m/%h]%p"
  "Value of `tramp-make-tramp-file-user-nil-format' for separate remoting.
XEmacs uses a separate filename syntax for EFS and Tramp.
See `tramp-file-name-structure-separate' for details.")

(defcustom tramp-make-tramp-file-user-nil-format
  (if (featurep 'xemacs)
      tramp-make-tramp-file-user-nil-format-separate
    tramp-make-tramp-file-user-nil-format-unified)
  "*Format string saying how to construct tramp file name when the user name is not known.
`%m' is replaced by the method name.
`%h' is replaced by the host name.
`%p' is replaced by the file name.
`%%' is replaced by %.

Also see `tramp-make-tramp-file-format', `tramp-file-name-structure', and `tramp-file-name-regexp'."
  :group 'tramp
  :type 'string)

(defconst tramp-multi-file-name-structure-unified
  (list (concat "\\`/\\(\\([a-zA-Z0-9]+\\)?:\\)" ;method
		"\\(\\(%s\\)+\\)"	;hops
		"\\(.*\\)\\'")		;path
	2 3 -1)
  "Value for `tramp-multi-file-name-structure' for unified remoting.
Emacs (not XEmacs) uses a unified filename syntax for Ange-FTP and Tramp.
See `tramp-file-name-structure-unified' for details.")

(defconst tramp-multi-file-name-structure-separate
  (list (concat
         ;; prefix
         "\\`/\\[\\(\\([a-z0-9]+\\)?\\)"
         ;; regexp specifying the hops
         "\\(\\(%s\\)+\\)"
         ;; path name
         "\\]\\(.*\\)\\'")
        2                               ;number of pair to match method
        3                               ;number of pair to match hops
        -1)                             ;number of pair to match path
  "Value of `tramp-multi-file-name-structure' for separate remoting.
XEmacs uses a separate filename syntax for EFS and Tramp.
See `tramp-file-name-structure-separate' for details.")

(defcustom tramp-multi-file-name-structure
  (if (featurep 'xemacs)
      tramp-multi-file-name-structure-separate
    tramp-multi-file-name-structure-unified)
  "*Describes the file name structure of `multi' files.
Multi files allow you to contact a remote host in several hops.
This is a list of four elements (REGEXP METHOD HOP PATH).

The first element, REGEXP, gives a regular expression to match against
the file name.  In this regular expression, `%s' is replaced with the
value of `tramp-multi-file-name-hop-structure'.  (Note: in order to
allow multiple hops, you normally want to use something like
\"\\\\(\\\\(%s\\\\)+\\\\)\" in the regular expression.  The outer pair
of parentheses is used for the HOP element, see below.)

All remaining elements are numbers.  METHOD gives the number of the
paren pair which matches the method name.  HOP gives the number of the
paren pair which matches the hop sequence.  PATH gives the number of
the paren pair which matches the path name on the remote host.

PATH can also be negative, which means to count from the end.  Ie, a
value of -1 means the last paren pair.

I think it would be good if the regexp matches the whole of the
string, but I haven't actually tried what happens if it doesn't..."
  :group 'tramp
  :type '(list (regexp :tag "File name regexp")
               (integer :tag "Paren pair for method name")
               (integer :tag "Paren pair for hops")
               (integer :tag "Paren pair to match path")))

(defconst tramp-multi-file-name-hop-structure-unified
  (list (concat "\\([a-zA-z0-9_]+\\):" ;hop method
		"\\([^@:/]+\\)@"	;user
		"\\([^:/]+\\):")	;host
	1 2 3)
  "Value of `tramp-multi-file-name-hop-structure' for unified remoting.
Emacs (not XEmacs) uses a unified filename syntax for Ange-FTP and Tramp.
See `tramp-file-name-structure-unified' for details.")

(defconst tramp-multi-file-name-hop-structure-separate
  (list (concat "/\\([a-z0-9_]+\\):"	;hop method
		"\\([a-z0-9_]+\\)@"	;user
		"\\([a-z0-9.-]+\\)")	;host
        1 2 3)
  "Value of `tramp-multi-file-name-hop-structure' for separate remoting.
XEmacs uses a separate filename syntax for EFS and Tramp.
See `tramp-file-name-structure-separate' for details.")

(defcustom tramp-multi-file-name-hop-structure
  (if (featurep 'xemacs)
      tramp-multi-file-name-hop-structure-separate
    tramp-multi-file-name-hop-structure-unified)
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

(defconst tramp-make-multi-tramp-file-format-unified
  (list "/%m" ":%m:%u@%h" ":%p")
  "Value of `tramp-make-multi-tramp-file-format' for unified remoting.
Emacs (not XEmacs) uses a unified filename syntax for Ange-FTP and Tramp.
See `tramp-file-name-structure-unified' for details.")

(defconst tramp-make-multi-tramp-file-format-separate
  (list "/[%m" "/%m:%u@%h" "]%p")
  "Value of `tramp-make-multi-tramp-file-format' for separate remoting.
XEmacs uses a separate filename syntax for EFS and Tramp.
See `tramp-file-name-structure-separate' for details.")

(defcustom tramp-make-multi-tramp-file-format
  (if (featurep 'xemacs)
      tramp-make-multi-tramp-file-format-separate
    tramp-make-multi-tramp-file-format-unified)
  "*Describes how to construct a `multi' file name.
This is a list of three elements PREFIX, HOP and PATH.

The first element PREFIX says how to construct the prefix, the second
element HOP specifies what each hop looks like, and the final element
PATH says how to construct the path name.

In PREFIX, `%%' means `%' and `%m' means the method name.

In HOP, `%%' means `%' and `%m', `%u', `%h' mean the hop method, hop
user and hop host, respectively.

In PATH, `%%' means `%' and `%p' means the path name.

The resulting file name always contains one copy of PREFIX and one
copy of PATH, but there is one copy of HOP for each hop in the file
name.

Note: the current implementation requires the prefix to contain the
method name, followed by all the hops, and the path name must come
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
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (tramp-yesno-prompt-regexp tramp-action-yesno))
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

(defcustom tramp-multi-actions
  '((tramp-password-prompt-regexp tramp-multi-action-password)
    (tramp-login-prompt-regexp tramp-multi-action-login)
    (shell-prompt-pattern tramp-multi-action-succeed)
    (tramp-wrong-passwd-regexp tramp-multi-action-permission-denied))
  "List of pattern/action pairs.
This list is used for each hop in multi-hop connections.
See `tramp-actions-before-shell' for more info."
  :group 'tramp
  :type '(repeat (list variable function)))

;;; Internal Variables:

(defvar tramp-buffer-file-attributes nil
  "Holds the `ls -ild' output for the current buffer.
This variable is local to each buffer.  It is not used if the remote
machine groks Perl.  If it is used, it's used as an emulation for
the visited file modtime.")
(make-variable-buffer-local 'tramp-buffer-file-attributes)

(defvar tramp-end-of-output "/////"
  "String used to recognize end of output.")

(defvar tramp-connection-function nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-remote-sh nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-rsh-program nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-rsh-args nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-rcp-program nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-rcp-args nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-rcp-keep-date-arg nil
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

(defvar tramp-telnet-program nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-telnet-args nil
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

;; Perl script to implement `file-attributes' in a Lisp `read'able output.
;; If you are hacking on this, note that you get *no* output unless this
;; spits out a complete line, including the '\n' at the end.
(defconst tramp-perl-file-attributes (concat
 "$f = $ARGV[0];
@s = lstat($f);
if (($s[2] & 0170000) == 0120000) { $l = readlink($f); $l = \"\\\"$l\\\"\"; }
elsif (($s[2] & 0170000) == 040000) { $l = \"t\"; }
else { $l = \"nil\" };
printf(\"(%s %u %u %u (%u %u) (%u %u) (%u %u) %u %u t (%u . %u) (%u %u))\\n\",
$l, $s[3], $s[4], $s[5], $s[8] >> 16 & 0xffff, $s[8] & 0xffff,
$s[9] >> 16 & 0xffff, $s[9] & 0xffff, $s[10] >> 16 & 0xffff, $s[10] & 0xffff,
$s[7], $s[2], $s[1] >> 16 & 0xffff, $s[1] & 0xffff, $s[0] >> 16 & 0xffff, $s[0] & 0xffff);"
 )
  "Perl script to produce output suitable for use with `file-attributes'
on the remote file system.")

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
(defvar tramp-perl-encode
  "perl -MMIME::Base64 -0777 -ne 'print encode_base64($_)'"
  "Perl program to use for encoding a file.
Escape sequence %s is replaced with name of Perl binary.")

(defvar tramp-perl-decode
  "perl -MMIME::Base64 -0777 -ne 'print decode_base64($_)'"
  "Perl program to use for decoding a file.
Escape sequence %s is replaced with name of Perl binary.")

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

(defvar tramp-last-cmd-time nil
  "Internal Tramp variable recording the time when the last cmd was sent.
This variable is buffer-local in every buffer.")
(make-variable-buffer-local 'tramp-last-cmd-time)

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
    (file-directory-files . tramp-handle-file-directory-files)
    (directory-files . tramp-handle-directory-files)
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
    (insert-directory . tramp-handle-insert-directory)
    (expand-file-name . tramp-handle-expand-file-name)
    (file-local-copy . tramp-handle-file-local-copy)
    (insert-file-contents . tramp-handle-insert-file-contents)
    (write-region . tramp-handle-write-region)
    (unhandled-file-name-directory . tramp-handle-unhandled-file-name-directory)
    (dired-call-process . tramp-handle-dired-call-process)
    (dired-recursive-delete-directory
     . tramp-handle-dired-recursive-delete-directory)
    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime))
        "Alist of handler functions.
Operations not mentioned here will be handled by the normal Emacs functions.")

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
   ((fboundp 'line-end-position) (funcall 'line-end-position))
   ((fboundp 'point-at-eol) 	 (funcall 'point-at-eol))
   (t (save-excursion (end-of-line) (point)))))

(defmacro with-parsed-tramp-file-name (filename var &rest body)
  "Parse a Tramp filename and make components available in the body.

First arg FILENAME is evaluated and dissected into its components.
Second arg VAR is a symbol.  It is used as a variable name to hold
the filename structure.  It is also used as a prefix for the variables
holding the components.  For example, if VAR is the symbol `foo', then
`foo' will be bound to the whole structure, `foo-multi-method' will
be bound to the multi-method component, and so on for `foo-method',
`foo-user', `foo-host', `foo-path'.

Remaining args are Lisp expressions to be evaluated (inside an implicit
`progn').

If VAR is nil, then we bind `v' to the structure and `multi-method',
`method', `user', `host', `path' to the components."
  `(let* ((,(or var 'v) (tramp-dissect-file-name ,filename))
	  (,(if var (intern (concat (symbol-name var) "-multi-method")) 'multi-method)
	   (tramp-file-name-multi-method ,(or var 'v)))
	  (,(if var (intern (concat (symbol-name var) "-method")) 'method)
	   (tramp-file-name-method ,(or var 'v)))
	  (,(if var (intern (concat (symbol-name var) "-user")) 'user)
	   (tramp-file-name-user ,(or var 'v)))
	  (,(if var (intern (concat (symbol-name var) "-host")) 'host)
	   (tramp-file-name-host ,(or var 'v)))
	  (,(if var (intern (concat (symbol-name var) "-path")) 'path)
	   (tramp-file-name-path ,(or var 'v))))
     ,@body))

(put 'with-parsed-tramp-file-name 'lisp-indent-function 2)

;;; File Name Handler Functions:

(defun tramp-handle-make-symbolic-link
  (filename linkname &optional ok-if-already-exists)
  "Like `make-symbolic-link' for tramp files.
If LINKNAME is a non-Tramp file, it is used verbatim as the target of
the symlink.  If LINKNAME is a Tramp file, only the path component is
used as the target of the symlink.

If LINKNAME is a Tramp file and the path component is relative, then
it is expanded first, before the path component is taken.  Note that
this can give surprising results if the user/host for the source and
target of the symlink differ."
  (with-parsed-tramp-file-name linkname l
    (when (tramp-ange-ftp-file-name-p l-multi-method l-method)
      (tramp-invoke-ange-ftp 'make-symbolic-link
			     filename linkname ok-if-already-exists))
    (let ((ln (tramp-get-remote-ln l-multi-method l-method l-user l-host))
	  (cwd (file-name-directory l-path)))
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
			    l-path)))))
	    (signal 'file-already-exists (list "File already exists" l-path))
	  (delete-file linkname)))

      ;; If FILENAME is a Tramp name, use just the path component.
      (when (tramp-tramp-file-p filename)
	(setq filename (tramp-file-name-path
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
		l-path)
	t)))))


(defun tramp-handle-load (file &optional noerror nomessage nosuffix must-suffix)
  "Like `load' for tramp files.  Not implemented!"
  (unless (file-name-absolute-p file)
    (error "Tramp cannot `load' files without absolute path name"))
  (with-parsed-tramp-file-name file nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'load
			     file noerror nomessage nosuffix must-suffix))
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
	(error "Cannot load nonexistant file `%s'" file)))
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

;; Path manipulation functions that grok TRAMP paths...
(defun tramp-handle-file-name-directory (file)
  "Like `file-name-directory' but aware of TRAMP files."
  ;; everything except the last filename thing is the directory
  (with-parsed-tramp-file-name file nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'file-name-directory file))
    ;; For the following condition, two possibilities should be tried:
    ;; (1) (string= path "")
    ;; (2) (or (string= path "") (string= path "/"))
    ;; The second variant fails when completing a "/" directory on
    ;; the remote host, that is a filename which looks like
    ;; "/user@host:/".  But maybe wildcards fail with the first variant.
    ;; We should do some investigation.
    (if (string= path "")
	;; For a filename like "/[foo]", we return "/".  The `else'
	;; case would return "/[foo]" unchanged.  But if we do that,
	;; then `file-expand-wildcards' ceases to work.  It's not
	;; quite clear to me what's the intuition that tells that this
	;; behavior is the right behavior, but oh, well.
	"/"
      ;; run the command on the path portion only
      ;; CCC: This should take into account the remote machine type, no?
      ;;  --daniel <daniel@danann.net>
      (tramp-make-tramp-file-name multi-method method user host
				  ;; This will not recurse...
				  (or (file-name-directory path) "")))))

(defun tramp-handle-file-name-nondirectory (file)
  "Like `file-name-nondirectory' but aware of TRAMP files."
  (with-parsed-tramp-file-name file nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'file-name-nondirectory file))
    (file-name-nondirectory path)))

(defun tramp-handle-file-truename (filename &optional counter prev-dirs)
  "Like `file-truename' for tramp files."
  (with-parsed-tramp-file-name filename nil
    ;; Ange-FTP does not support truename processing.  It returns the
    ;; file name as-is.  So that's what we do, too.
    (when (tramp-ange-ftp-file-name-p multi-method method)
      filename)
    (let* ((steps        (tramp-split-string path "/"))
	   (pathdir (let ((directory-sep-char ?/))
		      (file-name-as-directory path)))
	   (is-dir (string= path pathdir))
	   (thisstep nil)
	   (numchase 0)
	   ;; Don't make the following value larger than necessary.
	   ;; People expect an error message in a timely fashion when
	   ;; something is wrong; otherwise they might think that Emacs
	   ;; is hung.  Of course, correctness has to come first.
	   (numchase-limit 20)
	   (result nil)			;result steps in reverse order
	   (curstri "")
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
	      (nth 0 (tramp-handle-file-attributes
		      (tramp-make-tramp-file-name
		       multi-method method user host
		       (mapconcat 'identity
				  (append '("") (reverse result) (list thisstep))
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
	       (setq steps
		     (append (tramp-split-string symlink-target "/") steps)))
	      (t
	       ;; It's a file.
	       (setq result (cons thisstep result)))))
      (when (>= numchase numchase-limit)
	(error "Maximum number (%d) of symlinks exceeded" numchase-limit))
      (setq result (reverse result))
      (tramp-message-for-buffer
       multi-method method user host
       10 "True name of `%s' is `%s'"
       filename (mapconcat 'identity (cons "" result) "/"))
      (tramp-make-tramp-file-name
       multi-method method user host
       (concat (mapconcat 'identity (cons "" result) "/")
	       (if is-dir "/" ""))))))

;; Basic functions.

(defun tramp-handle-file-exists-p (filename)
  "Like `file-exists-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'file-exists-p filename))
    (save-excursion
      (zerop (tramp-send-command-and-check
	      multi-method method user host
	      (format
	       (tramp-get-file-exists-command multi-method method user host)
	       (tramp-shell-quote-argument path)))))))

;; CCC: This should check for an error condition and signal failure
;;      when something goes wrong.
;; Daniel Pittman <daniel@danann.net>
(defun tramp-handle-file-attributes (filename &optional nonnumeric)
  "Like `file-attributes' for tramp files.
Optional argument NONNUMERIC means return user and group name
rather than as numbers."
  (let (result)
    (with-parsed-tramp-file-name filename nil
      (when (tramp-ange-ftp-file-name-p multi-method method)
	(tramp-invoke-ange-ftp 'file-attributes filename))
      (when (tramp-handle-file-exists-p filename)
	;; file exists, find out stuff
	(save-excursion
	  (if (tramp-get-remote-perl multi-method method user host)
	      (setq result
		    (tramp-handle-file-attributes-with-perl
		     multi-method method user host path nonnumeric))
	    (setq result
		  (tramp-handle-file-attributes-with-ls
		   multi-method method user host path nonnumeric))))))
    result))


(defun tramp-handle-file-attributes-with-ls
  (multi-method method user host path &optional nonnumeric)
  "Implement `file-attributes' for tramp files using the ls(1) command."
  (let (symlinkp dirp
		 res-inode res-filemodes res-numlinks
		 res-uid res-gid res-size res-symlink-target)
    (tramp-message-for-buffer multi-method method user host 10
			      "file attributes with ls: %s"
			      (tramp-make-tramp-file-name
			       multi-method method user host path))
    (tramp-send-command
     multi-method method user host
     (format "%s %s %s"
	     (tramp-get-ls-command multi-method method user host)
	     (if nonnumeric "-ild" "-ildn")
	     (tramp-shell-quote-argument path)))
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
    (unless nonnumeric
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
     ;; 11. Device number.
     -1					;hm?
     )))

(defun tramp-handle-file-attributes-with-perl
  (multi-method method user host path &optional nonnumeric)
  "Implement `file-attributes' for tramp files using a Perl script.

The Perl command is sent to the remote machine when the connection
is initially created and is kept cached by the remote shell."
  (tramp-message-for-buffer multi-method method user host 10
			    "file attributes with perl: %s"
			    (tramp-make-tramp-file-name
			     multi-method method user host path))
  (tramp-send-command
   multi-method method user host
   (format "tramp_file_attributes %s" 
	   (tramp-shell-quote-argument path)))
  (tramp-wait-for-output)
  (let ((result (read (current-buffer))))
    (setcar (nthcdr 8 result)
	    (tramp-file-mode-from-int (nth 8 result)))
    result))

(defun tramp-handle-set-visited-file-modtime (&optional time-list)
  "Like `set-visited-file-modtime' for tramp files."
  (unless (buffer-file-name)
    (error "Can't set-visited-file-modtime: buffer `%s' not visiting a file"
	   (buffer-name)))
  (when time-list
    (tramp-run-real-handler 'set-visited-file-modtime (list time-list)))
  (let ((f (buffer-file-name))
	(coding-system-used nil))
    (with-parsed-tramp-file-name f nil
      ;; This operation is not handled by Ange-FTP!
      (when (tramp-ange-ftp-file-name-p multi-method method)
	(throw 'tramp-forward-to-ange-ftp
	       (tramp-run-real-handler 'set-visited-file-modtime
				       (list time-list))))
      (let* ((attr (file-attributes f))
	     (modtime (nth 5 attr)))
	;; We use '(0 0) as a don't-know value.  See also
	;; `tramp-handle-file-attributes-with-ls'.
	(when (boundp 'last-coding-system-used)
	  (setq coding-system-used last-coding-system-used))
	(if (not (equal modtime '(0 0)))
	    (tramp-run-real-handler 'set-visited-file-modtime (list modtime))
	  (save-excursion
	    (tramp-send-command
	     multi-method method user host
	     (format "%s -ild %s"
		     (tramp-get-ls-command multi-method method user host)
		     (tramp-shell-quote-argument path)))
	    (tramp-wait-for-output)
	    (setq attr (buffer-substring (point)
					 (progn (end-of-line) (point)))))
	  (setq tramp-buffer-file-attributes attr))
	(when (boundp 'last-coding-system-used)
	  (setq last-coding-system-used coding-system-used))
	nil))))

;; CCC continue here

;; This function makes the same assumption as
;; `tramp-handle-set-visited-file-modtime'.
(defun tramp-handle-verify-visited-file-modtime (buf)
  "Like `verify-visited-file-modtime' for tramp files."
  (with-current-buffer buf
    (let ((f (buffer-file-name)))
      (with-parsed-tramp-file-name f nil
	(when (tramp-ange-ftp-file-name-p multi-method method)
	  ;; This one requires a hack since the file name is not passed
	  ;; on the arg list.
	  (let ((buffer-file-name (tramp-make-ange-ftp-file-name
				   user host path)))
	    (tramp-invoke-ange-ftp 'verify-visited-file-modtime buf)))
	(let* ((attr (file-attributes f))
	       (modtime (nth 5 attr)))
	  (cond ((and attr (not (equal modtime '(0 0))))
		 ;; Why does `file-attributes' return a list (HIGH
		 ;; LOW), but `visited-file-modtime' returns a cons
		 ;; (HIGH . LOW)?
		 (let ((mt (visited-file-modtime)))
		   (< (abs (tramp-time-diff
			    modtime (list (car mt) (cdr mt)))) 2)))
		(attr
		 (save-excursion
		   (tramp-send-command
		    multi-method method user host
		    (format "%s -ild %s"
			    (tramp-get-ls-command multi-method method
						  user host)
			    (tramp-shell-quote-argument path)))
		   (tramp-wait-for-output)
		   (setq attr (buffer-substring
			       (point) (progn (end-of-line) (point)))))
		 (equal tramp-buffer-file-attributes attr))
		;; If file does not exist, say it is not modified.
		(t nil)))))))

(defadvice clear-visited-file-modtime (after tramp activate)
  "Set `tramp-buffer-file-attributes' back to nil.
Tramp uses this variable as an emulation for the actual modtime of the file,
if the remote host can't provide the modtime."
  (setq tramp-buffer-file-attributes nil))

(defun tramp-handle-set-file-modes (filename mode)
  "Like `set-file-modes' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'set-file-modes filename mode))
    (save-excursion
      (unless (zerop (tramp-send-command-and-check
		      multi-method method user host
		      (format "chmod %s %s"
			      (tramp-decimal-to-octal mode)
			      (tramp-shell-quote-argument path))))
	(signal 'file-error
		(list "Doing chmod"
		      ;; FIXME: extract the proper text from chmod's stderr.
		      "error while changing file's mode"
		      filename))))))

;; Simple functions using the `test' command.

(defun tramp-handle-file-executable-p (filename)
  "Like `file-executable-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'file-executable-p filename))
    (zerop (tramp-run-test "-x" filename))))

(defun tramp-handle-file-readable-p (filename)
  "Like `file-readable-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'file-readable-p filename))
    (zerop (tramp-run-test "-r" filename))))

(defun tramp-handle-file-accessible-directory-p (filename)
  "Like `file-accessible-directory-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'file-accessible-directory-p filename))
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
        ;; We are sure both files exist at this point.  We assume that
	;; both files are Tramp files, otherwise we issue an error
	;; message.  Todo: make a better error message.
        (t
         (save-excursion
	   (with-parsed-tramp-file-name file1 v1
	     (with-parsed-tramp-file-name file2 v2
	       (when (and (tramp-ange-ftp-file-name-p v1-multi-method v1-method)
			  (tramp-ange-ftp-file-name-p v2-multi-method v2-method))
		 (tramp-invoke-ange-ftp 'file-newer-than-file-p
					file1 file2))
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
		 (zerop (tramp-run-test2 "tramp_test_nt" file1 file2)))))))))

;; Functions implemented using the basic functions above.

(defun tramp-handle-file-modes (filename)
  "Like `file-modes' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'file-modes filename))
    (when (file-exists-p filename)
      (tramp-mode-string-to-int
       (nth 8 (tramp-handle-file-attributes filename))))))

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
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'file-directory-p filename))
    (save-excursion
      (zerop
       (tramp-send-command-and-check
	multi-method method user host
	(format "test -d %s"
		(tramp-shell-quote-argument path))
	t)))))				;run command in subshell

(defun tramp-handle-file-regular-p (filename)
  "Like `file-regular-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'file-regular-p filename))
    (and (tramp-handle-file-exists-p filename)
	 (eq ?- (aref (nth 8 (tramp-handle-file-attributes filename)) 0)))))

(defun tramp-handle-file-symlink-p (filename)
  "Like `file-symlink-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'file-symlink-p filename))
    (let ((x (car (tramp-handle-file-attributes filename))))
      (when (stringp x) x))))

(defun tramp-handle-file-writable-p (filename)
  "Like `file-writable-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'file-writable-p filename))
    (if (tramp-handle-file-exists-p filename)
	;; Existing files must be writable.
	(zerop (tramp-run-test "-w" filename))
      ;; If file doesn't exist, check if directory is writable.
      (and (zerop (tramp-run-test
		   "-d" (tramp-handle-file-name-directory filename)))
	   (zerop (tramp-run-test
		   "-w" (tramp-handle-file-name-directory filename)))))))

(defun tramp-handle-file-ownership-preserved-p (filename)
  "Like `file-ownership-preserved-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'file-ownership-preserved-p filename))
    (or (not (tramp-handle-file-exists-p filename))
	;; Existing files must be writable.
	(zerop (tramp-run-test "-O" filename)))))

;; Other file name ops.

;; ;; Matthias Köppe <mkoeppe@mail.math.uni-magdeburg.de>
;; (defun tramp-handle-directory-file-name (directory)
;;   "Like `directory-file-name' for tramp files."
;;   (if (and (eq (aref directory (- (length directory) 1)) ?/)
;; 	   (not (eq (aref directory (- (length directory) 2)) ?:)))
;;       (substring directory 0 (- (length directory) 1))
;;     directory))

;; Philippe Troin <phil@fifi.org>
(defun tramp-handle-directory-file-name (directory)
  "Like `directory-file-name' for tramp files."
  (with-parsed-tramp-file-name directory nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'directory-file-name directory))
    (let ((directory-length-1 (1- (length directory))))
      (save-match-data
	(if (and (eq (aref directory directory-length-1) ?/)
		 (eq (string-match tramp-file-name-regexp directory) 0)
		 (/= (match-end 0) directory-length-1))
	    (substring directory 0 directory-length-1)
	  directory)))))

;; Directory listings.

(defun tramp-handle-directory-files (directory
				     &optional full match nosort files-only)
  "Like `directory-files' for tramp files."
  (with-parsed-tramp-file-name directory nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'directory-files
			     directory full match nosort files-only))
    (let (result x)
      (save-excursion
	(tramp-barf-unless-okay
	 multi-method method user host
	 (concat "cd " (tramp-shell-quote-argument path))
	 nil
	 'file-error
	 "tramp-handle-directory-files: couldn't `cd %s'"
	 (tramp-shell-quote-argument path))
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

;; This function should return "foo/" for directories and "bar" for
;; files.  We use `ls -ad' to get a list of files (including
;; directories), and `find . -type d \! -name . -prune' to get a list
;; of directories.
(defun tramp-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for tramp files."
  (with-parsed-tramp-file-name directory nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'file-name-all-completions
			     filename directory))
    (unless (save-match-data (string-match "/" filename))
      (let* ((nowild tramp-completion-without-shell-p)
	     result)
	(save-excursion
	  (tramp-barf-unless-okay
	   multi-method method user host
	   (format "cd %s" (tramp-shell-quote-argument path))
	   nil 'file-error
	   "tramp-handle-file-name-all-completions: Couldn't `cd %s'"
	   (tramp-shell-quote-argument path))

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
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'file-name-completion
			     filename directory))
    (try-completion
     filename
     (mapcar (lambda (x) (cons x nil))
	     (tramp-handle-file-name-all-completions filename directory)))))

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
	(when (and (tramp-ange-ftp-file-name-p v1-multi-method v1-method)
		   (tramp-ange-ftp-file-name-p v2-multi-method v2-method))
	  (tramp-invoke-ange-ftp 'add-name-to-file
				 filename newname ok-if-already-exists))
	(when (tramp-ange-ftp-file-name-p v1-multi-method v1-method)
	  (tramp-invoke-ange-ftp 'add-name-to-file
				 filename newname ok-if-already-exists))
	(when (tramp-ange-ftp-file-name-p v2-multi-method v2-method)
	  (tramp-invoke-ange-ftp 'add-name-to-file
				 filename newname ok-if-already-exists))
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
	 (format "%s %s %s" ln (tramp-shell-quote-argument v1-path)
		 (tramp-shell-quote-argument v2-path))
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
	(t2 (tramp-tramp-file-p newname)))
    ;; Check which ones of source and target are Tramp files.
    (cond
     ((and t1 t2)
      ;; Both are Tramp files.
      (with-parsed-tramp-file-name filename v1
	(with-parsed-tramp-file-name newname v2
	  ;; Possibly invoke Ange-FTP.
	  (when (and (tramp-ange-ftp-file-name-p v1-multi-method v1-method)
		     (tramp-ange-ftp-file-name-p v2-multi-method v2-method))
	    (tramp-invoke-ange-ftp
	     (if (eq op 'copy) 'copy-file 'rename-file)
	     filename newname ok-if-already-exists keep-date))
	  ;; Check if we can use a shortcut.
	  (if (and (equal v1-multi-method v2-multi-method)
		   (equal v1-method v2-method)
		   (equal v1-host v2-host)
		   (equal v1-user v2-user))
	      ;; Shortcut: if method, host, user are the same for both
	      ;; files, we invoke `cp' or `mv' on the remote host
	      ;; directly.
	      (tramp-do-copy-or-rename-file-directly
	       op v1-multi-method v1-method v1-user v1-host
	       v1-path v2-path keep-date)
	    ;; The shortcut was not possible.  So we copy the
	    ;; file first.  If the operation was `rename', we go
	    ;; back and delete the original file (if the copy was
	    ;; successful).  The approach is simple-minded: we
	    ;; create a new buffer, insert the contents of the
	    ;; source file into it, then write out the buffer to
	    ;; the target file.  The advantage is that it doesn't
	    ;; matter which filename handlers are used for the
	    ;; source and target file.

	    ;; CCC: If both source and target are Tramp files,
	    ;; and both are using the same rcp-program, then we
	    ;; can invoke rcp directly.  Note that
	    ;; default-directory should point to a local
	    ;; directory if we want to invoke rcp.
	    (tramp-do-copy-or-rename-via-buffer
	     op filename newname keep-date)))))
	  ((or t1 t2)
	   ;; Use the generic method via a Tramp buffer.
	   (tramp-do-copy-or-rename-via-buffer op filename newname keep-date))
	  (t
	   ;; One of them must be a Tramp file.
	   (error "Tramp implementation says this cannot happen")))))

(defun tramp-do-copy-or-rename-via-buffer (op filename newname keep-date)
  "Use an Emacs buffer to copy or rename a file.
First arg OP is either `copy' or `rename' and indicates the operation.
FILENAME is the source file, NEWNAME the target file.
KEEP-DATE is non-nil if NEWNAME should have the same timestamp as FILENAME."
  (let ((trampbuf (get-buffer-create "*tramp output*")))
    (when keep-date
      (tramp-message
       1 (concat "Warning: cannot preserve file time stamp"
		 " with inline copying across machines")))
    (save-excursion
      (set-buffer trampbuf) (erase-buffer)
      (insert-file-contents-literally filename)
      (let ((coding-system-for-write 'no-conversion))
	(write-region (point-min) (point-max) newname)))
    ;; If the operation was `rename', delete the original file.
    (unless (eq op 'copy)
      (delete-file filename))))

(defun tramp-do-copy-or-rename-file-directly
  (op multi-method method user host path1 path2 keep-date)
  "Invokes `cp' or `mv' on the remote system.
OP must be one of `copy' or `rename', indicating `cp' or `mv',
respectively.  METHOD, USER, and HOST specify the connection.
PATH1 and PATH2 specify the two arguments of `cp' or `mv'.
If KEEP-DATE is non-nil, preserve the time stamp when copying."
  ;; CCC: What happens to the timestamp when renaming?
  (let ((cmd (cond ((and (eq op 'copy) keep-date) "cp -f -p")
                   ((eq op 'copy) "cp -f")
                   ((eq op 'rename) "mv -f")
                   (t (error
                       "Unknown operation `%s', must be `copy' or `rename'"
                       op)))))
    (save-excursion
      (tramp-barf-unless-okay
       multi-method method user host
       (format "%s %s %s"
               cmd
               (tramp-shell-quote-argument path1)
               (tramp-shell-quote-argument path2))
       nil 'file-error
       "Copying directly failed, see buffer `%s' for details."
       (buffer-name)))))

;; mkdir
(defun tramp-handle-make-directory (dir &optional parents)
  "Like `make-directory' for tramp files."
  (setq dir (expand-file-name dir))
  (with-parsed-tramp-file-name dir nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'make-directory dir parents))
    (tramp-barf-unless-okay
     multi-method method user host
     (format " %s %s"
	     (if parents "mkdir -p" "mkdir")
	     (tramp-shell-quote-argument path))
     nil 'file-error
     "Couldn't make directory %s" dir)))

;; CCC error checking?
(defun tramp-handle-delete-directory (directory)
  "Like `delete-directory' for tramp files."
  (setq directory (expand-file-name directory))
  (with-parsed-tramp-file-name directory nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'delete-directory directory))
    (save-excursion
      (tramp-send-command
       multi-method method user host
       (format "rmdir %s ; echo ok"
	       (tramp-shell-quote-argument path)))
      (tramp-wait-for-output))))

(defun tramp-handle-delete-file (filename)
  "Like `delete-file' for tramp files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'delete-file filename))
    (save-excursion
      (unless (zerop (tramp-send-command-and-check
		      multi-method method user host
		      (format "rm -f %s"
			      (tramp-shell-quote-argument path))))
	(signal 'file-error "Couldn't delete Tramp file")))))

;; Dired.

;; CCC: This does not seem to be enough. Something dies when
;;      we try and delete two directories under TRAMP :/
(defun tramp-handle-dired-recursive-delete-directory (filename)
  "Recursively delete the directory given.
This is like `dired-recursive-delete-directory' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'dired-recursive-delete-directory
			     filename))
    ;; run a shell command 'rm -r <path>'
    ;; Code shamelessly stolen for the dired implementation and, um, hacked :)
    (or (tramp-handle-file-exists-p filename)
	(signal
	 'file-error
	 (list "Removing old file name" "no such directory" filename)))
    ;; Which is better, -r or -R? (-r works for me <daniel@danann.net>)
    (tramp-send-command multi-method method user host 
			(format "rm -r %s" (tramp-shell-quote-argument path)))
    ;; Wait for the remote system to return to us...
    ;; This might take a while, allow it plenty of time.
    (tramp-wait-for-output 120)
    ;; Make sure that it worked...
    (and (tramp-handle-file-exists-p filename)
	 (error "Failed to recusively delete %s" filename))))
	 

(defun tramp-handle-dired-call-process (program discard &rest arguments)
  "Like `dired-call-process' for tramp files."
  (with-parsed-tramp-file-name default-directory nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (let ((default-directory
	      (tramp-make-ange-ftp-file-name user host path)))
	(tramp-invoke-ange-ftp 'dired-call-process
			       program discard arguments)))
    (save-excursion
      (tramp-barf-unless-okay
       multi-method method user host
       (format "cd %s" (tramp-shell-quote-argument path))
       nil 'file-error
       "tramp-handle-dired-call-process: Couldn't `cd %s'"
       (tramp-shell-quote-argument path))
      (tramp-send-command
       multi-method method user host
       (mapconcat #'tramp-shell-quote-argument (cons program arguments) " "))
      (tramp-wait-for-output))
    (unless discard
      (insert-buffer (tramp-get-buffer multi-method method user host)))
    (save-excursion
      (prog1
	  (tramp-send-command-and-check multi-method method user host nil)
	(tramp-send-command multi-method method user host "cd")
	(tramp-wait-for-output)))))

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
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'insert-directory
			     filename switches wildcard full-directory-p))
    (tramp-message-for-buffer
     multi-method method user host 10
     "Inserting directory `ls %s %s', wildcard %s, fulldir %s"
     switches filename (if wildcard "yes" "no")
     (if full-directory-p "yes" "no"))
    (when wildcard
      (setq wildcard (file-name-nondirectory path))
      (setq path (file-name-directory path)))
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
		       path
		     (tramp-shell-quote-argument (concat path ".")))))
	(tramp-barf-unless-okay
	 multi-method method user host
	 (format "cd %s" (tramp-shell-quote-argument
			  (file-name-directory path)))
	 nil 'file-error
	 "Couldn't `cd %s'"
	 (tramp-shell-quote-argument (file-name-directory path)))
	(tramp-send-command
	 multi-method method user host
	 (format "%s %s %s"
		 (tramp-get-ls-command multi-method method user host)
		 switches
		 (if full-directory-p
		     ;; Add "/." to make sure we got complete dir
		     ;; listing for symlinks, too.
		     (concat (file-name-as-directory
			      (file-name-nondirectory path)) ".")
		   (file-name-nondirectory path)))))
      (sit-for 1)			;needed for rsh but not ssh?
      (tramp-wait-for-output))
    (insert-buffer (tramp-get-buffer multi-method method user host))
    ;; On XEmacs, we want to call (exchange-point-and-mark t), but
    ;; that doesn't exist on Emacs, so we use this workaround instead.
    ;; Since zmacs-region-stays doesn't exist in Emacs, this ought to
    ;; be safe.  Thanks to Daniel Pittman <daniel@danann.net>.
    (let ((zmacs-region-stays t))
      (exchange-point-and-mark))
    (save-excursion
      (tramp-send-command multi-method method user host "cd")
      (tramp-wait-for-output))
    ;; Another XEmacs specialty follows.  What's the right way to do
    ;; it?
    (when (and (featurep 'xemacs)
	       (eq major-mode 'dired-mode))
      (save-excursion
	(require 'dired)
	(dired-insert-set-properties (point) (mark t))))))

;; Continuation of kluge to pacify byte-compiler.
;;(eval-when-compile
;;  (when (eq (symbol-function 'dired-insert-set-properties) 'ignore)
;;    (fmakunbound 'dired-insert-set-properties)))

;; CCC is this the right thing to do?
(defun tramp-handle-unhandled-file-name-directory (filename)
  "Like `unhandled-file-name-directory' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'unhandled-file-name-directory
			     filename))
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
  "Like `expand-file-name' for tramp files."
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
      (when (tramp-ange-ftp-file-name-p multi-method method)
	(tramp-invoke-ange-ftp 'expand-file-name name nil))
      (unless (file-name-absolute-p path)
	(setq path (concat "~/" path)))
      (save-excursion
	;; Tilde expansion if necessary.  This needs a shell which
	;; groks tilde expansion!  The function `tramp-find-shell' is
	;; supposed to find such a shell on the remote host.  Please
	;; tell me about it when this doesn't work on your system.
	(when (string-match "\\`\\(~[^/]*\\)\\(.*\\)\\'" path)
	  (let ((uname (match-string 1 path))
		(fname (match-string 2 path)))
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
	    (setq path (concat uname fname))
	    (erase-buffer)))
	;; No tilde characters in file name, do normal
	;; expand-file-name (this does "/./" and "/../").  We bind
	;; directory-sep-char here for XEmacs on Windows, which would
	;; otherwise use backslash.
	(let ((directory-sep-char ?/))
	  (tramp-make-tramp-file-name
	   multi-method method user host
	   (tramp-drop-volume-letter
	    (tramp-run-real-handler 'expand-file-name (list path)))))))))

;; Remote commands.

(defun tramp-handle-shell-command (command &optional output-buffer error-buffer)
  "Like `shell-command' for tramp files.
This will break if COMMAND prints a newline, followed by the value of
`tramp-end-of-output', followed by another newline."
  (when (tramp-tramp-file-p default-directory)
    (with-parsed-tramp-file-name default-directory nil
      (when (tramp-ange-ftp-file-name-p multi-method method)
	(let ((default-directory (tramp-make-ange-ftp-file-name
				  user host path)))
	  (tramp-invoke-ange-ftp 'shell-command
				 command output-buffer error-buffer)))
      (let (status)
	(when (string-match "&[ \t]*\\'" command)
	  (error "Tramp doesn't grok asynchronous shell commands, yet"))
	(when error-buffer
	  (error "Tramp doesn't grok optional third arg ERROR-BUFFER, yet"))
	(save-excursion
	  (tramp-barf-unless-okay
	   multi-method method user host
	   (format "cd %s" (tramp-shell-quote-argument path))
	   nil 'file-error
	   "tramp-handle-shell-command: Couldn't `cd %s'"
	   (tramp-shell-quote-argument path))
	  (tramp-send-command multi-method method user host
			      (concat command "; tramp_old_status=$?"))
	  ;; This will break if the shell command prints "/////"
	  ;; somewhere.  Let's just hope for the best...
	  (tramp-wait-for-output))
	(unless output-buffer
	  (setq output-buffer (get-buffer-create "*Shell Command Output*"))
	  (set-buffer output-buffer)
	  (erase-buffer))
	(unless (bufferp output-buffer)
	  (setq output-buffer (current-buffer)))
	(set-buffer output-buffer)
	(insert-buffer (tramp-get-buffer multi-method method user host))
	(save-excursion
	  (tramp-send-command multi-method method user host "cd")
	  (tramp-wait-for-output)
	  (tramp-send-command
	   multi-method method user host
	   (concat "tramp_set_exit_status $tramp_old_status;"
		   " echo tramp_exit_status $?"))
	  (tramp-wait-for-output)
	  (goto-char (point-max))
	  (unless (search-backward "tramp_exit_status " nil t)
	    (error "Couldn't find exit status of `%s'" command))
	  (skip-chars-forward "^ ")
	  (setq status (read (current-buffer))))
	(unless (zerop (buffer-size))
	  (pop-to-buffer output-buffer))
	status)))
  ;; The following is only executed if something strange was
  ;; happening.  Emit a helpful message and do it anyway.
  (message "tramp-handle-shell-command called with non-tramp directory: `%s'"
	   default-directory)
  (tramp-run-real-handler 'shell-command
			  (list command output-buffer error-buffer)))

;; File Editing.

(defsubst tramp-make-temp-file ()
  (funcall (if (fboundp 'make-temp-file) 'make-temp-file 'make-temp-name)
	   (expand-file-name tramp-temp-name-prefix
			     (tramp-temporary-file-directory))))

(defun tramp-handle-file-local-copy (filename)
  "Like `file-local-copy' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'file-local-copy filename))
    (let ((trampbuf (get-buffer-create "*tramp output*"))
	  tmpfil)
      (unless (file-exists-p filename)
	(error "Cannot make local copy of non-existing file `%s'"
	       filename))
      (setq tmpfil (tramp-make-temp-file))
      (cond ((tramp-get-rcp-program multi-method method)
	     ;; Use rcp-like program for file transfer.
	     (tramp-message-for-buffer
	      multi-method method user host
	      5 "Fetching %s to tmp file %s..." filename tmpfil)
	     (save-excursion (set-buffer trampbuf) (erase-buffer))
	     (unless (equal
		      0
		      (apply #'call-process
			     (tramp-get-rcp-program multi-method method)
			     nil trampbuf nil
			     (append (tramp-get-rcp-args multi-method method)
				     (list
				      (tramp-make-rcp-program-file-name
				       user host
				       (tramp-shell-quote-argument path))
				      tmpfil))))
	       (pop-to-buffer trampbuf)
	       (error
		(concat "tramp-handle-file-local-copy: `%s' didn't work, "
			"see buffer `%s' for details")
		(tramp-get-rcp-program multi-method method) trampbuf))
	     (tramp-message-for-buffer
	      multi-method method user host
	      5 "Fetching %s to tmp file %s...done" filename tmpfil))
	    ((and (tramp-get-encoding-command multi-method method user host)
		  (tramp-get-decoding-command multi-method method user host))
	     ;; Use inline encoding for file transfer.
	     (save-excursion
	       ;; Following line for setting tramp-current-method,
	       ;; tramp-current-user, tramp-current-host.
	       (set-buffer (tramp-get-buffer multi-method method user host))
	       (tramp-message 5 "Encoding remote file %s..." filename)
	       (tramp-barf-unless-okay
		multi-method method user host
		(concat (tramp-get-encoding-command multi-method method user host)
			" < " (tramp-shell-quote-argument path))
		nil 'file-error
		"Encoding remote file failed, see buffer `%s' for details"
		(tramp-get-buffer multi-method method user host))
	       ;; Remove trailing status code
	       (goto-char (point-max))
	       (delete-region (point) (progn (forward-line -1) (point)))

	       (tramp-message 5 "Decoding remote file %s..." filename)
	       (if (and (tramp-get-decoding-function multi-method method user host)
			(fboundp (tramp-get-decoding-function
				  multi-method method user host)))
		   ;; If tramp-decoding-function is defined for this
		   ;; method, we call it.
		   (let ((tmpbuf (get-buffer-create " *tramp tmp*")))
		     (set-buffer tmpbuf)
		     (erase-buffer)
		     (insert-buffer (tramp-get-buffer multi-method method
						      user host))
		     (tramp-message-for-buffer
		      multi-method method user host
		      6 "Decoding remote file %s with function %s..."
		      filename
		      (tramp-get-decoding-function multi-method method user host))
		     (set-buffer tmpbuf)
		     (let ((coding-system-for-write 'no-conversion))
		       (funcall (tramp-get-decoding-function
				 multi-method method user host)
				(point-min)
				(point-max))
		       (write-region (point-min) (point-max) tmpfil))
		     (kill-buffer tmpbuf))
		 ;; If tramp-decoding-function is not defined for this
		 ;; method, we invoke tramp-decoding-command instead.
		 (let ((tmpfil2 (tramp-make-temp-file)))
		   (write-region (point-min) (point-max) tmpfil2)
		   (tramp-message
		    6 "Decoding remote file %s with command %s..."
		    filename
		    (tramp-get-decoding-command multi-method method user host))
		   (call-process
		    tramp-sh-program
		    tmpfil2		;input
		    nil			;output
		    nil			;display
		    "-c" (concat (tramp-get-decoding-command
				  multi-method method user host)
				 " > " tmpfil))
		   (delete-file tmpfil2)))
	       (tramp-message-for-buffer
		multi-method method user host
		5 "Decoding remote file %s...done" filename)))

	    (t (error "Wrong method specification for `%s'" method)))
      tmpfil)))


(defun tramp-handle-insert-file-contents
  (filename &optional visit beg end replace)
  "Like `insert-file-contents' for tramp files."
  (barf-if-buffer-read-only)
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'insert-file-contents
			     filename visit beg end replace))
    (if (not (tramp-handle-file-exists-p filename))
	(progn
	  (when visit
	    (setq buffer-file-name filename)
	    (set-visited-file-modtime)
	    (set-buffer-modified-p nil))
	  (signal 'file-error
		  (format "File `%s' not found on remote host" filename))
	  (list (tramp-handle-expand-file-name filename) 0))
      (let ((local-copy (tramp-handle-file-local-copy filename))
	    (coding-system-used nil)
	    (result nil))
	(when visit
	  (setq buffer-file-name filename)
	  (set-visited-file-modtime)
	  (set-buffer-modified-p nil))
	(tramp-message-for-buffer
	 multi-method method user host
	 9 "Inserting local temp file `%s'..." local-copy)
	(setq result
	      (tramp-run-real-handler 'insert-file-contents
				      (list local-copy nil beg end replace)))
	;; Now `last-coding-system-used' has right value.  Remember it.
	(when (boundp 'last-coding-system-used)
	  (setq coding-system-used last-coding-system-used))
	(tramp-message 9 "Inserting local temp file `%s'...done" local-copy)
	(delete-file local-copy)
	(when (boundp 'last-coding-system-used)
	  (setq last-coding-system-used coding-system-used))
	(list (expand-file-name filename)
	      (second result))))))

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
  ;; XEmacs takes a coding system as the sevent argument, not `confirm'
  (when (and (not (featurep 'xemacs))
	     confirm (file-exists-p filename))
    (unless (y-or-n-p (format "File %s exists; overwrite anyway? "
                              filename))
      (error "File not overwritten")))
  (with-parsed-tramp-file-name filename nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'write-region
			     start end filename append visit lockname confirm))
    (let ((curbuf (current-buffer))
	  (rcp-program (tramp-get-rcp-program multi-method method))
	  (rcp-args (tramp-get-rcp-args multi-method method))
	  (encoding-command
	   (tramp-get-encoding-command multi-method method user host))
	  (encoding-function
	   (tramp-get-encoding-function multi-method method user host))
	  (decoding-command
	   (tramp-get-decoding-command multi-method method user host))
	  (trampbuf (get-buffer-create "*tramp output*"))
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
	(setq coding-system-used last-coding-system-used))
      ;; This is a bit lengthy due to the different methods possible for
      ;; file transfer.  First, we check whether the method uses an rcp
      ;; program.  If so, we call it.  Otherwise, both encoding and
      ;; decoding command must be specified.  However, if the method
      ;; _also_ specifies an encoding function, then that is used for
      ;; encoding the contents of the tmp file.
      (cond (rcp-program
	     ;; use rcp-like program for file transfer
	     (let ((argl (append rcp-args
				 (list
				  tmpfil
				  (tramp-make-rcp-program-file-name
				   user host
				   (tramp-shell-quote-argument path))))))
	       (tramp-message-for-buffer
		multi-method method user host
		6 "Writing tmp file using `%s'..." rcp-program)
	       (save-excursion (set-buffer trampbuf) (erase-buffer))
	       (when tramp-debug-buffer
		 (save-excursion
		   (set-buffer (tramp-get-debug-buffer multi-method
						       method user host))
		   (goto-char (point-max))
		   (tramp-insert-with-face
		    'bold (format "$ %s %s\n" rcp-program
				  (mapconcat 'identity argl " ")))))
	       (unless (equal 0
			      (apply #'call-process
				     rcp-program nil trampbuf nil argl))
		 (pop-to-buffer trampbuf)
		 (error
		  "Cannot write region to file `%s', command `%s' failed"
		  filename rcp-program))
	       (tramp-message-for-buffer
		multi-method method user host
		6 "Transferring file using `%s'...done"
		rcp-program)))
	    ((and encoding-command decoding-command)
	     ;; Use inline file transfer
	     (let ((tmpbuf (get-buffer-create " *tramp file transfer*")))
	       (save-excursion
		 ;; Encode tmpfil into tmpbuf
		 (tramp-message-for-buffer multi-method method user host
					   5 "Encoding region...")
		 (set-buffer tmpbuf)
		 (erase-buffer)
		 ;; Use encoding function or command.
		 (if (and encoding-function
			  (fboundp encoding-function))
		     (progn
		       (tramp-message-for-buffer
			multi-method method user host
			6 "Encoding region using function...")
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
			 (funcall encoding-function (point-min) (point-max)))
		       (goto-char (point-max))
		       (unless (bolp)
			 (newline)))
		   (tramp-message-for-buffer
		    multi-method method user host
		    6 "Encoding region using command...")
		   (unless (equal 0
				  (call-process
				   tramp-sh-program
				   tmpfil ;input = local tmp file
				   t	;output is current buffer
				   nil	;don't redisplay
				   "-c"
				   encoding-command))
		     (pop-to-buffer trampbuf)
		     (error (concat "Cannot write to `%s', local encoding"
				    " command `%s' failed")
			    filename encoding-command)))
		 ;; Send tmpbuf into remote decoding command which
		 ;; writes to remote file.  Because this happens on the
		 ;; remote host, we cannot use the function.
		 (tramp-message-for-buffer
		  multi-method method user host
		  5 "Decoding region into remote file %s..." filename)
		 (tramp-send-command
		  multi-method method user host
		  (format "%s >%s <<'EOF'"
			  decoding-command
			  (tramp-shell-quote-argument path)))
		 (set-buffer tmpbuf)
		 (tramp-message-for-buffer
		  multi-method method user host
		  6 "Sending data to remote host...")
		 (tramp-send-region multi-method method user host
				    (point-min) (point-max))
		 ;; wait for remote decoding to complete
		 (tramp-message-for-buffer
		  multi-method method user host
		  6 "Sending end of data token...")
		 (tramp-send-command
		  multi-method method user host "EOF")
		 (tramp-message-for-buffer
		  multi-method method user host 6
		  "Waiting for remote host to process data...")
		 (set-buffer (tramp-get-buffer multi-method method user host))
		 (tramp-wait-for-output)
		 (tramp-barf-unless-okay
		  multi-method method user host nil nil 'file-error
		  (concat "Couldn't write region to `%s',"
			  " decode using `%s' failed")
		  filename decoding-command)
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
      (when (eq visit t)
	(set-visited-file-modtime))
      ;; Make `last-coding-system-used' have the right value.
      (when (boundp 'last-coding-system-used)
	(setq last-coding-system-used coding-system-used))
      (when (or (eq visit t)
		(eq visit nil)
		(stringp visit))
	(message "Wrote %s" filename)))))

;; Call down to the real handler.
;; Because EFS does not play nicely with TRAMP (both systems match an
;; TRAMP path) it is needed to disable efs as well as tramp for the
;; operation.
;;
;; Other than that, this is the canon file-handler code that the doco
;; says should be used here. Which is nice.
;;
;; Under XEmacs current, EFS also hooks in as
;; efs-sifn-handler-function to handle any path with environment
;; variables. This has two implications:
;; 1) That EFS may not be completely dead (yet) for TRAMP paths
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
  (let ((inhibit-file-name-handlers
         (list 'tramp-file-name-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

;; Main function.
;;;###autoload
(defun tramp-file-name-handler (operation &rest args)
  "Invoke tramp file name handler.
Falls back to normal file name handler if no tramp file name handler exists."
  (let ((fn (assoc operation tramp-file-name-handler-alist)))
    (if fn
	(catch 'tramp-forward-to-ange-ftp
	  (save-match-data (apply (cdr fn) args)))
      (tramp-run-real-handler operation args))))

;; Register in file name handler alist
;;;###autoload
(add-to-list 'file-name-handler-alist
	     (cons tramp-file-name-regexp 'tramp-file-name-handler))

;; To handle EFS, the following functions need to be dealt with:
;;
;; * dired-before-readin-hook contains efs-dired-before-readin
;; * file-name-handler-alist contains efs-file-handler-function
;;   and efs-root-handler-function and efs-sifn-handler-function
;; * find-file-hooks contains efs-set-buffer-mode
;;
;; But it won't happen for EFS since the XEmacs maintainers
;; don't want to use a unified filename syntax.
(defun tramp-disable-ange-ftp ()
  "Turn Ange-FTP off.
This is useful for unified remoting.  See
`tramp-file-name-structure-unified' and
`tramp-file-name-structure-separate' for details.  Requests suitable
for Ange-FTP will be forwarded to Ange-FTP.  Also see the variables
`tramp-ftp-method', `tramp-default-method', and
`tramp-default-method-alist'.

This function is not needed in Emacsen which include Tramp, but is
present for backward compatibility."
  (let ((a1 (rassq 'ange-ftp-hook-function file-name-handler-alist))
	(a2 (rassq 'ange-ftp-completion-hook-function file-name-handler-alist)))
    (setq file-name-handler-alist
	  (delete a1 (delete a2 file-name-handler-alist)))))
(tramp-disable-ange-ftp)

(defun tramp-repair-jka-compr ()
  "If jka-compr is already loaded, move it to the front of
`file-name-handler-alist'.  On Emacs 21.4 or so this will not be
necessary anymore."
  (let ((jka (rassoc 'jka-compr-handler file-name-handler-alist)))
    (when jka
      (setq file-name-handler-alist
	    (cons jka (delete jka file-name-handler-alist))))))
(tramp-repair-jka-compr)

(defun tramp-invoke-ange-ftp (operation &rest args)
  "Invoke the Ange-FTP handler function and throw."
  (or ange-ftp-name-format (require 'ange-ftp))
  (let ((ange-ftp-name-format
	 (list (nth 0 tramp-file-name-structure)
	       (nth 3 tramp-file-name-structure)
	       (nth 2 tramp-file-name-structure)
	       (nth 4 tramp-file-name-structure))))
    (throw 'tramp-forward-to-ange-ftp
	   (apply 'ange-ftp-hook-function operation args))))

(defun tramp-ange-ftp-file-name-p (multi-method method)
  "Check if it's a filename that should be forwarded to Ange-FTP."
  (and (null multi-method) (string= method tramp-ftp-method)))


;;; Interactions with other packages:

;; -- complete.el --

;; This function contributed by Ed Sabol
(defun tramp-handle-expand-many-files (name)
  "Like `PC-expand-many-files' for tramp files."
  (with-parsed-tramp-file-name name nil
    (when (tramp-ange-ftp-file-name-p multi-method method)
      (tramp-invoke-ange-ftp 'expand-many-files name))
    (save-match-data
      (if (or (string-match "\\*" name)
	      (string-match "\\?" name)
	      (string-match "\\[.*\\]" name))
	  (save-excursion
	    ;; Dissect NAME.
	    (let (bufstr)
	      ;; Perhaps invoke Ange-FTP.
	      (when (string= method tramp-ftp-method)
		(signal 'tramp-run-ange-ftp (list 0)))
	      ;; CCC: To do it right, we should quote certain characters
	      ;; in the file name, but since the echo command is going to
	      ;; break anyway when there are spaces in the file names, we
	      ;; don't bother.
	      ;;-(let ((comint-file-name-quote-list
	      ;;-       (set-difference tramp-file-name-quote-list
	      ;;-                       '(?\* ?\? ?[ ?]))))
	      ;;-  (tramp-send-command
	      ;;-   multi-method method user host
	      ;;-   (format "echo %s" (comint-quote-filename path)))
	      ;;-  (tramp-wait-for-output))
	      (tramp-send-command multi-method method user host
				  (format "echo %s" path))
	      (tramp-wait-for-output)
	      (setq bufstr (buffer-substring (point-min)
					     (tramp-line-end-position)))
	      (goto-char (point-min))
	      (if (string-equal path bufstr)
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
	(list (tramp-handle-expand-file-name name))))))

;; Check for complete.el and override PC-expand-many-files if appropriate.
(eval-when-compile
  (defun tramp-save-PC-expand-many-files (name))); avoid compiler warning

(defun tramp-setup-complete ()
  (fset 'tramp-save-PC-expand-many-files
        (symbol-function 'PC-expand-many-files))
  (defun PC-expand-many-files (name)
    (if (tramp-tramp-file-p name)
        (tramp-handle-expand-many-files name)
      (tramp-save-PC-expand-many-files name))))

;; Why isn't eval-after-load sufficient?
(if (fboundp 'PC-expand-many-files)
    (tramp-setup-complete)
  (eval-after-load "complete" '(tramp-setup-complete)))




;;; Internal Functions:

(defun tramp-set-auto-save ()
  (when (and (buffer-file-name)
             (tramp-tramp-file-p (buffer-file-name))
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
               (tramp-shell-quote-argument (tramp-file-name-path v)))))))

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
         (path1 (tramp-file-name-path v1))
         (path2 (tramp-file-name-path v2)))
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
               (tramp-shell-quote-argument path1)
               (or switch "")
               (tramp-shell-quote-argument path2))))))

(defun tramp-buffer-name (multi-method method user host)
  "A name for the connection buffer for USER at HOST using METHOD."
  (cond (multi-method
         (tramp-buffer-name-multi-method "tramp" multi-method method user host))
        (user
         (format "*tramp/%s %s@%s*" method user host))
        (t
         (format "*tramp/%s %s*" method host))))

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
  (get-buffer-create (tramp-buffer-name multi-method method user host)))

(defun tramp-debug-buffer-name (multi-method method user host)
  "A name for the debug buffer for USER at HOST using METHOD."
  (cond (multi-method
         (tramp-buffer-name-multi-method "debug tramp"
                                         multi-method method user host))
        (user
         (format "*debug tramp/%s %s@%s*" method user host))
        (t
         (format "*debug tramp/%s %s*" method host))))

(defun tramp-get-debug-buffer (multi-method method user host)
  "Get the debug buffer for USER at HOST using METHOD."
  (get-buffer-create (tramp-debug-buffer-name multi-method method user host)))

(defun tramp-find-executable (multi-method method user host
                                         progname dirlist ignore-tilde)
  "Searches for PROGNAME in all directories mentioned in DIRLIST.
First args METHOD, USER and HOST specify the connection, PROGNAME
is the program to search for, and DIRLIST gives the list of directories
to search.  If IGNORE-TILDE is non-nil, directory names starting
with `~' will be ignored.

Returns the full path name of PROGNAME, if found, and nil otherwise.

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
  (tramp-message 10 "Finding command to check if file exists")
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
             (and (setq tramp-file-exists-command "ls -d %s")
                  (tramp-handle-file-exists-p existing)
                  (not (tramp-handle-file-exists-p nonexisting)))
             (and (setq tramp-file-exists-command "test -e %s")
                  (tramp-handle-file-exists-p existing)
                  (not (tramp-handle-file-exists-p nonexisting)))
             (and (setq tramp-file-exists-command "/bin/test -e %s")
                  (tramp-handle-file-exists-p existing)
                  (not (tramp-handle-file-exists-p nonexisting)))
             (and (setq tramp-file-exists-command "/usr/bin/test -e %s")
                  (tramp-handle-file-exists-p existing)
                  (not (tramp-handle-file-exists-p nonexisting))))
      (error "Couldn't find command to check if file exists."))))
    

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
       (concat "PS1='$ ' ; exec " shell)) ;
      (unless (tramp-wait-for-regexp
               (get-buffer-process (current-buffer))
               60 (format "\\(\\$ *\\|\\(%s\\)\\'\\)" shell-prompt-pattern))
        (pop-to-buffer (buffer-name))
        (error "Couldn't find remote `%s' prompt." shell))
      (process-send-string nil (format "PS1='%s%s%s'; PS2=''; PS3=''%s"
                                       tramp-rsh-end-of-line
                                       tramp-end-of-output
                                       tramp-rsh-end-of-line
                                       tramp-rsh-end-of-line))
      (tramp-wait-for-output)
      (tramp-send-command multi-method method user host "echo hello")
      (tramp-message 5 "Waiting for remote `%s' to start up..." shell)
      (unless (tramp-wait-for-output 5)
        (unless (tramp-wait-for-output 5)
          (pop-to-buffer (buffer-name))
          (error "Couldn't start remote `%s', see buffer `%s' for details"
                 shell (buffer-name))))
      (tramp-message 5 "Waiting for remote `%s' to start up...done" shell))
     (t (tramp-message 5 "Remote `%s' groks tilde expansion, good"
		       (tramp-get-remote-sh multi-method method))))))

(defun tramp-check-ls-command (multi-method method user host cmd)
  "Checks whether the given `ls' executable groks `-n'.
METHOD, USER and HOST specify the connection, CMD (the full path name of)
the `ls' executable.  Returns t if CMD supports the `-n' option, nil
otherwise."
  (tramp-message 9 "Checking remote `%s' command for `-n' option"
               cmd)
  (when (tramp-handle-file-executable-p
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
    ;; It would be better to use the CL function `find', but
    ;; we don't want run-time dependencies on CL.
    (while (and dl (not result))
      (let ((x (concat (file-name-as-directory (car dl)) cmd)))
        (when (tramp-check-ls-command multi-method method user host x)
          (setq result x)))
      (setq dl (cdr dl)))
    result))

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
  (when (tramp-method-out-of-band-p multi-method method)
    (kill-process (get-buffer-process (current-buffer)))
    (error (concat "Out of band method `%s' not applicable "
		   "for remote shell asking for a password")
	   method))
  (tramp-enter-password p (match-string 0)))

(defun tramp-action-succeed (p multi-method method user host)
  "Signal success in finding shell prompt."
  (tramp-message 9 "Found remote shell prompt.")
  (erase-buffer)
  (throw 'tramp-action 'ok))

(defun tramp-action-permission-denied (p multi-method method user host)
  "Signal permission denied."
  (tramp-message 9 "Permission denied by remote host.")
  (kill-process p)
  (erase-buffer)
  (throw 'tramp-action 'permission-denied))

(defun tramp-action-yesno (p multi-method method user host)
  "Ask the user if he is sure."
  (save-window-excursion
    (pop-to-buffer (tramp-get-buffer multi-method method user host))
    (unless (yes-or-no-p (match-string 0))
      (kill-process p)
      (erase-buffer)
      (throw 'tramp-action 'permission-denied))
    (process-send-string p (concat "yes" tramp-rsh-end-of-line))
    (erase-buffer)))

;; The following functions are specifically for multi connections.

(defun tramp-multi-action-login (p method user host)
  "Send the login name."
  (tramp-message 9 "Sending login name `%s'" user)
  (erase-buffer)
  (process-send-string p (concat user tramp-rsh-end-of-line)))

(defun tramp-multi-action-password (p method user host)
  "Query the user for a password."
  (tramp-enter-password p (match-string 0)))

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

;; Functions for processing the actions.

(defun tramp-process-one-action (p multi-method method user host actions)
  "Wait for output from the shell and perform one action."
  (let (found item pattern action todo)
    (erase-buffer)
    (tramp-message 9 "Waiting 60s for prompt from remote shell")
    (with-timeout (60 (throw 'tramp-action 'timeout))
      (while (not found)
	(accept-process-output p 1)
	(goto-char (point-min))
	(setq todo actions)
	(while todo
	  (goto-char (point-min))
	  (setq item (pop todo))
	  (setq pattern (symbol-value (nth 0 item)))
	  (setq action (nth 1 item))
	  (tramp-message 10 "Looking for pattern %s" pattern)
	  (when (re-search-forward (concat pattern "\\'") nil t)
	    (setq found (funcall action p multi-method method user host)))))
      found)))

(defun tramp-process-actions (p multi-method method user host actions)
  "Perform actions until success."
  (let (exit)
    (while (not exit)
      (tramp-message 10 "Processing actions")
      (setq exit
	    (catch 'tramp-action
	      (tramp-process-one-action
	       p multi-method method user host actions)
	      nil)))
    (unless (eq exit 'ok)
      (error "Login failed"))))

;; For multi-actions.

(defun tramp-process-one-multi-action (p method user host actions)
  "Wait for output from the shell and perform one action."
  (let (found item pattern action todo)
    (erase-buffer)
    (tramp-message 9 "Waiting 60s for prompt from remote shell")
    (with-timeout (60 (throw 'tramp-action 'timeout))
      (while (not found)
	(accept-process-output p 1)
	(setq todo actions)
	(goto-char (point-min))
	(while todo
	  (goto-char (point-min))
	  (setq item (pop todo))
	  (setq pattern (symbol-value (nth 0 item)))
	  (setq action (nth 1 item))
	  (tramp-message 10 "Looking for pattern %s" pattern)
	  (when (re-search-forward (concat pattern "\\'") nil t)
	    (setq found (funcall action p method user host)))))
      found)))

(defun tramp-process-multi-actions (p method user host actions)
  "Perform actions until success."
  (let (exit)
    (while (not exit)
      (setq exit
	    (catch 'tramp-action
	      (tramp-process-one-multi-action p method user host actions)
	      nil)))
    (unless (eq exit 'ok)
      (error "Login failed"))))

;; The actual functions for opening connections.

(defun tramp-open-connection-telnet (multi-method method user host)
  "Open a connection using a telnet METHOD.
This starts the command `telnet HOST ARGS'[*], then waits for a remote
login prompt, then sends the user name USER, then waits for a remote
password prompt.  It queries the user for the password, then sends the
password to the remote host.

If USER is nil, uses value returned by `(user-login-name)' instead.

Recognition of the remote shell prompt is based on the variable
`shell-prompt-pattern' which must be set up correctly.

Please note that it is NOT possible to use this connection method
together with an out-of-band transfer method!  You must use an inline
transfer method.

Maybe the different regular expressions need to be tuned.

* Actually, the telnet program as well as the args to be used can be
  specified in the method parameters, see the variable `tramp-methods'."
  (save-match-data
    (when (tramp-method-out-of-band-p multi-method method)
      (error "Cannot use out-of-band method `%s' with telnet connection method"
             method))
    (when multi-method
      (error "Cannot multi-connect using telnet connection method"))
    (tramp-pre-connection multi-method method user host)
    (tramp-message 7 "Opening connection for %s@%s using %s..." 
		   (or user (user-login-name)) host method)
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "TERM" tramp-terminal-type)
      (let* ((default-directory (tramp-temporary-file-directory))
             (coding-system-for-read (unless (and (not (featurep 'xemacs))
                                                  (> emacs-major-version 20))
                                       tramp-dos-coding-system))
             (p (apply 'start-process
                       (tramp-buffer-name multi-method method user host)
                       (tramp-get-buffer multi-method method user host)
                       (tramp-get-telnet-program multi-method method)
                       host
                       (tramp-get-telnet-args multi-method method)))
             (found nil)
             (pw nil))
        (process-kill-without-query p)
	(set-buffer (tramp-get-buffer multi-method method user host))
	(erase-buffer)
	(tramp-process-actions p multi-method method user host
			       tramp-actions-before-shell)

;;         (tramp-message 9 "Waiting for login prompt...")
;;         (unless (tramp-wait-for-regexp p nil tramp-login-prompt-regexp)
;;           (pop-to-buffer (buffer-name))
;;           (kill-process p)
;;           (error "Couldn't find remote login prompt"))
;;         (erase-buffer)
;;         ;; Remote login defaults to local one.
;;         (tramp-message 9 "Sending login name %s" (or user (user-login-name)))
;;         (process-send-string p (concat (or user (user-login-name)) 
;;                                        tramp-rsh-end-of-line))
;;         (tramp-message 9 "Waiting for password prompt...")
;;         (unless (setq found (tramp-wait-for-regexp
;;                              p nil tramp-password-prompt-regexp))
;;           (pop-to-buffer (buffer-name))
;;           (kill-process p)
;;           (error "Couldn't find remote password prompt"))
;;         (erase-buffer)
;;         (setq pw (tramp-read-passwd (car found)))
;;         (tramp-message 9 "Sending password")
;;         (process-send-string p (concat pw tramp-rsh-end-of-line))
;;         (tramp-message 9 "Waiting 30s for remote shell to come up...")
;;         (unless (setq found
;;                       (tramp-wait-for-regexp
;;                        p 30 (format "\\(%s\\)\\|\\(%s\\)\\'"
;;                                     tramp-wrong-passwd-regexp
;;                                     shell-prompt-pattern)))
;;           (pop-to-buffer (buffer-name))
;;           (kill-process p)
;;           (error "Couldn't find remote shell prompt"))
;;         (when (nth 1 found)
;;           (pop-to-buffer (buffer-name))
;;           (kill-process p)
;;           (error "Login failed: %s" (nth 1 found)))

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

Recognition of the remote shell prompt is based on the variable
`shell-prompt-pattern' which must be set up correctly.

Please note that it is NOT possible to use this connection method with
an out-of-band transfer method if this function asks the user for a
password!  You must use an inline transfer method in this case.
Sadly, the transfer method cannot be switched on the fly, instead you
must specify the right method in the file name.

Kludgy feature: if HOST has the form \"xx#yy\", then yy is assumed to
be a port number for ssh, and \"-p yy\" will be added to the list of
arguments, and xx will be used as the host name to connect to.

* Actually, the rsh program to be used can be specified in the
  method parameters, see the variable `tramp-methods'."
  (save-match-data
    (when multi-method
      (error "Cannot multi-connect using rsh connection method"))
    (tramp-pre-connection multi-method method user host)
    (if user 
	(tramp-message 7 "Opening connection for %s@%s using %s..." 
		       user host method)
      (tramp-message 7 "Opening connection at %s using %s..." host method))
    (let ((process-environment (copy-sequence process-environment))
	  (bufnam (tramp-buffer-name multi-method method user host))
	  (buf (tramp-get-buffer multi-method method user host))
	  (rsh-program (tramp-get-rsh-program multi-method method))
	  (rsh-args (tramp-get-rsh-args multi-method method)))
      ;; The following should be changed.  We need a more general
      ;; mechanism to parse extra host args.
      (when (string-match "\\([^#]*\\)#\\(.*\\)" host)
	(setq rsh-args (cons "-p" (cons (match-string 2 host) rsh-args)))
	(setq host (match-string 1 host)))
      (setenv "TERM" tramp-terminal-type)
      (let* ((default-directory (tramp-temporary-file-directory))
             (coding-system-for-read (unless (and (not (featurep 'xemacs))
                                                  (> emacs-major-version 20))
                                       tramp-dos-coding-system))
             (p (if user
                    (apply #'start-process bufnam buf rsh-program  
                           host "-l" user rsh-args)
                  (apply #'start-process bufnam buf rsh-program 
                         host rsh-args)))
             (found nil))
        (process-kill-without-query p)

	(set-buffer buf)
	(tramp-process-actions p multi-method method user host
			       tramp-actions-before-shell)

;;         (tramp-message 9 "Waiting 60s for shell or passwd prompt from %s" host)
;;         (setq found
;;               (tramp-wait-for-regexp
;;                p 60
;;                (format
;;                 "\\(%s\\)\\|\\(%s\\)\\'"
;;                 tramp-password-prompt-regexp
;;                 shell-prompt-pattern)))
;;         (unless found
;;           (pop-to-buffer (buffer-name))
;;           (kill-process p)
;;           (error "Couldn't find remote shell or passwd prompt"))
;;         (when (nth 1 found)
;;           (when (tramp-method-out-of-band-p multi-method method)
;;             (pop-to-buffer (buffer-name))
;;             (kill-process p)
;;             (error (concat "Out of band method `%s' not applicable"
;;                            " for remote shell asking for a password")
;;                    method))
;;           (erase-buffer)
;;           (tramp-message 9 "Sending password...")
;;           (tramp-enter-password p (nth 1 found))
;;           (tramp-message 9 "Sent password, waiting 60s for remote shell prompt")
;;           (setq found (tramp-wait-for-regexp p 60
;;                                              (format "\\(%s\\)\\|\\(%s\\)\\'"
;;                                                      tramp-wrong-passwd-regexp
;;                                                      shell-prompt-pattern))))
;;         (unless found
;;           (pop-to-buffer (buffer-name))
;;           (kill-process p)
;;           (error "Couldn't find remote shell prompt"))
;;         (when (nth 1 found)
;;           (pop-to-buffer (buffer-name))
;;           (kill-process p)
;;           (error "Login failed: %s" (nth 1 found)))

        (tramp-message 7 "Initializing remote shell")
        (tramp-open-connection-setup-interactive-shell
         p multi-method method user host)
        (tramp-post-connection multi-method method user host)))))

(defun tramp-open-connection-su (multi-method method user host)
  "Open a connection using the `su' program with METHOD.
This starts `su - USER', then waits for a password prompt.  The HOST
name must be equal to the local host name or to `localhost'.

If USER is nil, uses value returned by user-login-name instead.

Recognition of the remote shell prompt is based on the variable
`shell-prompt-pattern' which must be set up correctly.  Note that the
other user may have a different shell prompt than you do, so it is not
at all unlikely that this variable is set up wrongly!"
  (save-match-data
    (when (tramp-method-out-of-band-p multi-method method)
      (error "Cannot use out-of-band method `%s' with `su' connection method"
             method))
    (unless (or (string-match (concat "^" (regexp-quote host))
                              (system-name))
                (string= "localhost" host))
      (error
       "Cannot connect to different host `%s' with `su' connection method"
       host))
    (when (not user)
      (error "Must give user name for `su' connection method"))
    (tramp-pre-connection multi-method method user host)
    (tramp-message 7 "Opening connection for `%s' using `%s'..." 
		   (or user (user-login-name)) method)
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "TERM" tramp-terminal-type)
      (let* ((default-directory (tramp-temporary-file-directory))
             (coding-system-for-read (unless (and (not (featurep 'xemacs))
                                                  (> emacs-major-version 20))
                                       tramp-dos-coding-system))
             (p (apply 'start-process
                       (tramp-buffer-name multi-method method user host)
                       (tramp-get-buffer multi-method method user host)
                       (tramp-get-su-program multi-method method)
                       (mapcar
                        '(lambda (x)
                           (format-spec x `((?u . ,user))))
                        (tramp-get-su-args multi-method method))))
             (found nil)
             (pw nil))
        (process-kill-without-query p)
	(set-buffer (tramp-get-buffer multi-method method user host))
	(tramp-process-actions p multi-method method user host
			       tramp-actions-before-shell)

;;         (tramp-message 9 "Waiting 30s for shell or password prompt...")
;;         (unless (setq found (tramp-wait-for-regexp
;;                              p 30
;;                              (format "\\(%s\\)\\|\\(%s\\)\\'"
;;                                      tramp-password-prompt-regexp
;;                                      shell-prompt-pattern)))
;;           (pop-to-buffer (buffer-name))
;;           (kill-process p)
;;           (error "Couldn't find shell or password prompt"))
;;         (when (nth 1 found)
;;           (erase-buffer)
;;           (setq pw (tramp-read-passwd (car found)))
;;           (tramp-message 9 "Sending password")
;;           (process-send-string p (concat pw tramp-rsh-end-of-line))
;;           (tramp-message 9 "Waiting 30s for remote shell to come up...")
;;           (unless (setq found
;;                         (tramp-wait-for-regexp
;;                          p 30 (format "\\(%s\\)\\|\\(%s\\)\\'"
;;                                       tramp-wrong-passwd-regexp
;;                                       shell-prompt-pattern)))
;;             (pop-to-buffer (buffer-name))
;;             (kill-process p)
;;             (error "Couldn't find remote shell prompt"))
;;           (when (nth 1 found)
;;             (pop-to-buffer (buffer-name))
;;             (kill-process p)
;;             (error "`su' failed: %s" (nth 1 found))))

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
    (when (tramp-method-out-of-band-p multi-method method)
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
    (tramp-pre-connection multi-method method user host)
    (tramp-message 7 "Opening `%s' connection..." multi-method)
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "TERM" tramp-terminal-type)
      (let* ((default-directory (tramp-temporary-file-directory))
             (coding-system-for-read (unless (and (not (featurep 'xemacs))
                                                  (> emacs-major-version 20))
                                       tramp-dos-coding-system))
             (p (start-process (tramp-buffer-name multi-method method user host)
                               (tramp-get-buffer multi-method method user host)
                               tramp-multi-sh-program))
             (num-hops (length method))
             (i 0))
        (process-kill-without-query p)
        (tramp-message 9 "Waiting 60s for local shell to come up...")
        (unless (tramp-wait-for-regexp
		 p 60 (format "%s\\'" shell-prompt-pattern))
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
				 tramp-multi-actions)

;;     (tramp-message 9 "Waiting 30s for login prompt from %s" host)
;;     (unless (tramp-wait-for-regexp p 30 tramp-login-prompt-regexp)
;;       (pop-to-buffer (buffer-name))
;;       (kill-process p)
;;       (error "Couldn't find login prompt from host %s" host))
;;     (erase-buffer)
;;     (tramp-message 9 "Sending login name %s" (or user (user-login-name)))
;;     (process-send-string p (concat (or user (user-login-name)) tramp-rsh-end-of-line))
;;     (tramp-message 9 "Waiting for password prompt")
;;     (unless (setq found (tramp-wait-for-regexp p nil tramp-password-prompt-regexp))
;;       (pop-to-buffer (buffer-name))
;;       (kill-process p)
;;       (error "Couldn't find password prompt from host %s" host))
;;     (erase-buffer)
;;     (setq pw (tramp-read-passwd
;;               (format "Password for %s@%s, %s" (or user (user-login-name)) host found)))
;;     (tramp-message 9 "Sending password")
;;     (process-send-string p (concat pw tramp-rsh-end-of-line))
;;     (tramp-message 9 "Waiting 60s for remote shell to come up...")
;;     (unless (setq found (tramp-wait-for-regexp
;;                          p 60 (format "\\(%s\\)\\|\\(%s\\)\\'"
;;                                       tramp-wrong-passwd-regexp
;;                                       shell-prompt-pattern)))
;;       (pop-to-buffer (buffer-name))
;;       (kill-process p)
;;       (error "Couldn't find shell prompt from host %s" host))
;;     (when (nth 1 found)
;;       (pop-to-buffer (buffer-name))
;;       (kill-process p)
;;       (error "Login to %s failed: %s" (nth 2 found)))
    ))

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
				 tramp-multi-actions)
;;     (tramp-message 9 "Waiting 60s for shell or passwd prompt from %s" host)
;;     (unless (setq found
;;                   (tramp-wait-for-regexp p 60
;;                                        (format "\\(%s\\)\\|\\(%s\\)\\'"
;;                                                tramp-password-prompt-regexp
;;                                                shell-prompt-pattern)))
;;       (pop-to-buffer (buffer-name))
;;       (kill-process p)
;;       (error "Couldn't find remote shell or passwd prompt"))
;;     (when (nth 1 found)
;;       (erase-buffer)
;;       (tramp-message 9 "Sending password...")
;;       (tramp-enter-password p (nth 1 found))
;;       (tramp-message 9 "Sent password, waiting 60s for remote shell prompt")
;;       (setq found (tramp-wait-for-regexp p 60
;;                                          (format "\\(%s\\)\\|\\(%s\\)\\'"
;;                                                  tramp-wrong-passwd-regexp
;;                                                  shell-prompt-pattern))))
;;     (unless found
;;       (pop-to-buffer (buffer-name))
;;       (kill-process p)
;;       (error "Couldn't find remote shell prompt"))
;;     (when (nth 1 found)
;;       (pop-to-buffer (buffer-name))
;;       (kill-process p)
;;       (error "Login failed: %s" (nth 1 found)))
    ))

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
				 tramp-multi-actions)
;;     (tramp-message 9 "Waiting 60s for shell or passwd prompt for %s" (or user (user-login-name)))
;;     (unless (setq found (tramp-wait-for-regexp
;;                          p 60 (format "\\(%s\\)\\|\\(%s\\)\\'"
;;                                       tramp-password-prompt-regexp
;;                                       shell-prompt-pattern)))
;;       (pop-to-buffer (buffer-name))
;;       (kill-process p)
;;       (error "Couldn't find shell or passwd prompt for %s" 
;; 	     (or user (user-login-name))))
;;     (when (nth 1 found)
;;       (tramp-message 9 "Sending password...")
;;       (tramp-enter-password p (nth 1 found))
;;       (erase-buffer)
;;       (tramp-message 9 "Sent password, waiting 60s for remote shell prompt")
;;       (setq found (tramp-wait-for-regexp p 60
;;                                        (format "\\(%s\\)\\|\\(%s\\)\\'"
;;                                                tramp-wrong-passwd-regexp
;;                                                shell-prompt-pattern))))
;;     (unless found
;;       (pop-to-buffer (buffer-name))
;;       (kill-process p)
;;       (error "Couldn't find remote shell prompt"))
;;     (when (nth 1 found)
;;       (pop-to-buffer (buffer-name))
;;       (kill-process p)
;;       (error "Login failed: %s" (nth 1 found)))
    ))

;; Utility functions.

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
                 (accept-process-output proc 1)
                 (goto-char (point-min))
                 (setq found (when (re-search-forward regexp nil t)
                               (tramp-match-string-list)))))))
          (t
           (while (not found)
             (accept-process-output proc 1)
             (goto-char (point-min))
             (setq found (when (re-search-forward regexp nil t)
                           (tramp-match-string-list))))))
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

(defun tramp-enter-password (p prompt)
  "Prompt for a password and send it to the remote end.
Uses PROMPT as a prompt and sends the password to process P."
  (let ((pw (tramp-read-passwd prompt)))
    (erase-buffer)
    (process-send-string p (concat pw tramp-rsh-end-of-line))))

;; HHH: Not Changed.  This might handle the case where USER is not
;;      given in the "File name" very poorly.  Then, the local
;;      variable tramp-current user will be set to nil.
(defun tramp-pre-connection (multi-method method user host)
  "Do some setup before actually logging in.
METHOD, USER and HOST specify the connection."
  (set-buffer (tramp-get-buffer multi-method method user host))
  (set (make-local-variable 'tramp-current-multi-method) multi-method)
  (set (make-local-variable 'tramp-current-method) method)
  (set (make-local-variable 'tramp-current-user)   user)
  (set (make-local-variable 'tramp-current-host)   host)
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
  (process-send-string nil (format "exec %s%s"
                                   (tramp-get-remote-sh multi-method method)
                                   tramp-rsh-end-of-line))
  (when tramp-debug-buffer
    (save-excursion
      (set-buffer (tramp-get-debug-buffer multi-method method user host))
      (goto-char (point-max))
      (tramp-insert-with-face
       'bold (format "$ exec %s\n" (tramp-get-remote-sh multi-method method)))))
  (tramp-message 9 "Waiting 30s for remote `%s' to come up..."
               (tramp-get-remote-sh multi-method method))
  (unless (tramp-wait-for-regexp
	   p 30 (format "\\(\\$ *\\|%s\\)\\'" shell-prompt-pattern))
    (pop-to-buffer (buffer-name))
    (error "Remote `%s' didn't come up.  See buffer `%s' for details"
           (tramp-get-remote-sh multi-method method) (buffer-name)))
  (tramp-message 9 "Setting up remote shell environment")
  (tramp-discard-garbage-erase-buffer p multi-method method user host)
  (process-send-string
   nil (format "stty -inlcr -echo kill '^U'%s" tramp-rsh-end-of-line))
  (unless (tramp-wait-for-regexp
	   p 30 (format "\\(\\$ *\\|%s\\)\\'" shell-prompt-pattern))
    (pop-to-buffer (buffer-name))
    (error "Couldn't `stty -echo', see buffer `%s'" (buffer-name)))
  (erase-buffer)
  (process-send-string nil (format "TERM=dumb; export TERM%s"
                                   tramp-rsh-end-of-line))
  (unless (tramp-wait-for-regexp
	   p 30 (format "\\(\\$ *\\|%s\\)\\'" shell-prompt-pattern))
    (pop-to-buffer (buffer-name))
    (error "Couldn't `TERM=dumb; export TERM', see buffer `%s'" (buffer-name)))
  ;; Try to set up the coding system correctly.
  ;; CCC this can't be the right way to do it.  Hm.
  (save-excursion
    (erase-buffer)
    (tramp-message 9 "Determining coding system")
    (process-send-string nil (format "echo foo ; echo bar %s"
                                     tramp-rsh-end-of-line))
    (unless (tramp-wait-for-regexp
             p 30 (format "\\(\\$ *\\|%s\\)\\'" shell-prompt-pattern))
      (pop-to-buffer (buffer-name))
      (error "Couldn't `echo foo; echo bar' to determine line endings'"))
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
        (tramp-message 9 "Trying `stty -onlcr'")
        (process-send-string nil (format "stty -onlcr%s" tramp-rsh-end-of-line))
        (unless (tramp-wait-for-regexp
                 p 30 (format "\\(\\$ *\\|%s\\)\\'" shell-prompt-pattern))
          (pop-to-buffer (buffer-name))
          (error "Couldn't `stty -onlcr', see buffer `%s'" (buffer-name))))))
  (erase-buffer)
  (tramp-message
   9 "Waiting 30s for `HISTFILE=$HOME/.tramp_history; HISTSIZE=1'")
  (process-send-string
   nil (format "HISTFILE=$HOME/.tramp_history; HISTSIZE=1%s"
               tramp-rsh-end-of-line))
  (unless (tramp-wait-for-regexp
           p 30 (format "\\(\\$ *\\|%s\\)\\'" shell-prompt-pattern))
    (pop-to-buffer (buffer-name))
    (error (concat "Couldn't `HISTFILE=$HOME/.tramp_history; "
                   "HISTSIZE=1', see buffer `%s'")
           (buffer-name)))
  (erase-buffer)
  (tramp-message 9 "Waiting 30s for `set +o vi +o emacs'")
  (process-send-string
   nil (format "set +o vi +o emacs%s"      ;mustn't `>/dev/null' with AIX?
               tramp-rsh-end-of-line))
  (unless (tramp-wait-for-regexp
           p 30 (format "\\(\\$ *\\|%s\\)\\'" shell-prompt-pattern))
    (pop-to-buffer (buffer-name))
    (error "Couldn't `set +o vi +o emacs', see buffer `%s'"
           (buffer-name)))
  (erase-buffer)
  (tramp-message 9 "Waiting 30s for `unset MAIL MAILCHECK MAILPATH'")
  (process-send-string
   nil (format "unset MAIL MAILCHECK MAILPATH 1>/dev/null 2>/dev/null%s"
               tramp-rsh-end-of-line))
  (unless (tramp-wait-for-regexp
           p 30 (format "\\(\\$ *\\|%s\\)\\'" shell-prompt-pattern))
    (pop-to-buffer (buffer-name))
    (error "Couldn't `unset MAIL MAILCHECK MAILPATH', see buffer `%s'"
           (buffer-name)))
  (erase-buffer)
  (tramp-message 9 "Waiting 30s for `unset CDPATH'")
  (process-send-string
   nil (format "unset CDPATH%s" tramp-rsh-end-of-line))
  (unless (tramp-wait-for-regexp
           p 30 (format "\\(\\$ *\\|%s\\)\\'" shell-prompt-pattern))
    (pop-to-buffer (buffer-name))
    (error "Couldn't `unset CDPATH', see buffer `%s'"
           (buffer-name)))
  (erase-buffer)
  (tramp-message 9 "Setting shell prompt")
  (tramp-send-command
   multi-method method user host
   (format "PS1='%s%s%s'; PS2=''; PS3=''"
           tramp-rsh-end-of-line
           tramp-end-of-output
           tramp-rsh-end-of-line))
  (tramp-wait-for-output)
  (tramp-send-command multi-method method user host "echo hello")
  (tramp-message 9 "Waiting for remote `%s' to come up..."
               (tramp-get-remote-sh multi-method method))
  (unless (tramp-wait-for-output 5)
    (unless (tramp-wait-for-output 5)
      (pop-to-buffer (buffer-name))
      (error "Couldn't set remote shell prompt.  See buffer `%s' for details"
             (buffer-name))))
  (tramp-message 7 "Waiting for remote `%s' to come up...done"
               (tramp-get-remote-sh multi-method method)))

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
  (tramp-find-file-exists-command multi-method method user host)
  (sit-for 1)
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
        (looking-at (format "\n%s\n" (regexp-quote tramp-end-of-output))))
  (unless tramp-test-groks-nt
    (tramp-send-command
     multi-method method user host
     (concat "tramp_test_nt () {" tramp-rsh-end-of-line
             "test -n \"`find $1 -prune -newer $2 -print`\"" tramp-rsh-end-of-line
             "}")))
  (tramp-wait-for-output)
  ;; Find a `perl'.
  (erase-buffer)
  (let ((tramp-remote-perl
	 (or (tramp-find-executable multi-method method user host
				  "perl5" tramp-remote-path nil)
	     (tramp-find-executable multi-method method user host
				  "perl" tramp-remote-path nil))))
    (when tramp-remote-perl
      (tramp-set-connection-property "perl" tramp-remote-perl multi-method method user host)
      ;; Set up stat in Perl if we can.
      (when tramp-remote-perl
	(tramp-message 5 "Sending the Perl `file-attributes' implementation.")
	(tramp-send-linewise
	 multi-method method user host
	 (concat "tramp_file_attributes () {\n"
		 tramp-remote-perl
		 " -e '" tramp-perl-file-attributes "' $1 2>/dev/null\n"
		 "}"))
	(tramp-wait-for-output)
	(tramp-message 5 "Sending the Perl `mime-encode' implementation.")
	(tramp-send-linewise
	 multi-method method user host
	 (concat "tramp_encode () {\n"
		 (format tramp-perl-encode tramp-remote-perl)
		 " 2>/dev/null"
		 "\n}"))
	(tramp-wait-for-output)
	(tramp-message 5 "Sending the Perl `mime-decode' implementation.")
	(tramp-send-linewise
	 multi-method method user host
	 (concat "tramp_decode () {\n"
		 (format tramp-perl-decode tramp-remote-perl)
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
  (unless (tramp-get-rcp-program multi-method method)
    (tramp-find-inline-encoding multi-method method user host))
  ;; If encoding/decoding command are given, test to see if they work.
  ;; CCC: Maybe it would be useful to run the encoder both locally and
  ;; remotely to see if they produce the same result.
  (let ((decoding (tramp-get-decoding-command multi-method method user host))
	(encoding (tramp-get-encoding-command multi-method method user host))
	(magic-string "xyzzy"))
    (when (and (or decoding encoding) (not (and decoding encoding)))
      (tramp-kill-process multi-method method user host)
      (error
       "Must give both decoding and encoding command in method definition"))
    (when (and decoding encoding)
      (tramp-message
       5
       "Checking to see if encoding/decoding commands work on remote host...")
      (tramp-send-command
       multi-method method user host
       (format "echo %s | %s | %s"
	       (tramp-shell-quote-argument magic-string) encoding decoding))
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
(defvar tramp-coding-commands
  '(("mimencode -b" "mimencode -u -b"
     base64-encode-region base64-decode-region)
    ("mmencode -b" "mmencode -u -b"
     base64-encode-region base64-decode-region)
    ("recode data..base64" "recode base64..data"
     base64-encode-region base64-decode-region)
    ("uuencode xxx" "uudecode -o -"
     nil uudecode-decode-region)
    ("uuencode xxx" "uudecode -p"
     nil uudecode-decode-region)
    ("tramp_encode" "tramp_decode"
     base64-encode-region base64-decode-region))
  "List of coding commands for inline transfer.
Each item is a list (ENCODING-COMMAND DECODING-COMMAND
ENCODING-FUNCTION DECODING-FUNCTION).

Each item can be a string, giving a command, or a symbol, giving
a function.

The ENCODING-COMMAND should be a command accepting a plain file on
standard input and writing the encoded file to standard output.  The
DECODING-COMMAND should be a command accepting an encoded file on
standard input and writing the decoded file to standard output.

The ENCODING-FUNCTION and DECODING-FUNCTION functions will be called
with two arguments, start and end of region, and are expected to
replace the region contents with the encoded or decoded results,
respectively.")

(defun tramp-find-inline-encoding (multi-method method user host)
  "Find an inline transfer encoding that works.
Goes through the list `tramp-coding-commands'."
  (let ((commands tramp-coding-commands)
	item found)
    (while (and commands (null found))
      (setq item (pop commands))
      (catch 'wont-work
	(let ((ec (nth 0 item))
	      (dc (nth 1 item))
	      (ef (nth 2 item))
	      (df (nth 3 item)))
	  ;; Check if encoding and decoding commands can be called
	  ;; remotely with null input and output.  This makes sure there
	  ;; are no syntax errors and the command is really found.
	  (tramp-message-for-buffer
	   multi-method method user host 10
	   "Checking remote encoding command `%s' for sanity" ec)
	  (unless (zerop (tramp-send-command-and-check
			  multi-method method user host
			  (format "%s </dev/null >/dev/null" ec) t))
	    (throw 'wont-work nil))
	  (tramp-message-for-buffer
	   multi-method method user host 10
	   "Checking remote decoding command `%s' for sanity" dc)
	  (unless (zerop (tramp-send-command-and-check
			  multi-method method user host
			  (format "%s </dev/null >/dev/null" dc) t))
	    (throw 'wont-work nil))
	  ;; If no encoding/decoding function is given, the
	  ;; corresponding encoding/decoding command also has to work
	  ;; locally.
	  (when (not (fboundp ef))
	    (tramp-message-for-buffer
	     multi-method method user host 10
	     "Checking local encoding command `%s' for sanity" ec)
	    (unless (zerop (call-process
			    tramp-sh-program ;program
			    nil		;input
			    nil		;output buffer
			    nil		;redisplay
			    "-c"
			    (format "%s </dev/null >/dev/null" ec)))
	      (throw 'wont-work nil)))
	  (when (not (fboundp df))
	    (tramp-message-for-buffer
	     multi-method method user host 10
	     "Checking local decoding command `%s' for sanity" dc)
	    (unless (zerop (call-process
			    tramp-sh-program ;program
			    nil		;input file
			    nil		;output buffer
			    nil		;redisplay
			    "-c"
			    (format "%s </dev/null >/dev/null" dc)))
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
    (let ((ec (nth 0 found))
	  (dc (nth 1 found))
	  (ef (nth 2 found))
	  (df (nth 3 found)))
      (tramp-set-encoding-command multi-method method user host ec)
      (tramp-set-decoding-command multi-method method user host dc)
      (tramp-set-encoding-function multi-method method user host ef)
      (tramp-set-decoding-function multi-method method user host df))))
	  

(defun tramp-maybe-open-connection (multi-method method user host)
  "Maybe open a connection to HOST, logging in as USER, using METHOD.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
  (let ((p (get-buffer-process (tramp-get-buffer multi-method method user host)))
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
		 (> (tramp-time-diff tramp-last-cmd-time (current-time)) 60))
	(process-send-string p (concat "echo hello" tramp-rsh-end-of-line))
	(unless (accept-process-output p 2)
	  (delete-process p)
	  (setq p nil))
	(erase-buffer)))
    (unless (and p (processp p) (memq (process-status p) '(run open)))
      (when (and p (processp p))
        (delete-process p))
      (funcall (tramp-get-connection-function multi-method method)
               multi-method method user host))))

(defun tramp-send-command
  (multi-method method user host command &optional noerase)
  "Send the COMMAND to USER at HOST (logged in using METHOD).
Erases temporary buffer before sending the command (unless NOERASE
is true)."
  (tramp-maybe-open-connection multi-method method user host)
  (setq tramp-last-cmd-time (current-time))
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

;; It seems that Tru64 Unix does not like it if long strings are sent
;; to it in one go.  (This happens when sending the Perl
;; `file-attributes' implementation, for instance.)  Therefore, we
;; have this function which waits a bit at each line.
(defun tramp-send-linewise
  (multi-method method user host string &optional noerase)
  "Send the STRING to USER at HOST linewise.
Erases temporary buffer before sending the STRING (unless NOERASE
is true).

The STRING is expected to use Unix line-endings, but the lines sent to
the remote host use line-endings as defined in the variable
`tramp-rsh-end-of-line'."
  (tramp-maybe-open-connection multi-method method user host)
  (when tramp-debug-buffer
    (save-excursion
      (set-buffer (tramp-get-debug-buffer multi-method method user host))
      (goto-char (point-max))
      (tramp-insert-with-face 'bold (format "$ %s\n" string))))
  (let ((proc nil)
	(lines (split-string string "\n")))
    (set-buffer (tramp-get-buffer multi-method method user host))
    (unless noerase (erase-buffer))
    (setq proc (get-buffer-process (current-buffer)))
    (mapcar (lambda (x)
	      (sleep-for 0.1)
	      (process-send-string proc
				   (concat x tramp-rsh-end-of-line)))
	    lines)))

(defun tramp-wait-for-output (&optional timeout)
  "Wait for output from remote rsh command."
  (let ((proc (get-buffer-process (current-buffer)))
        (found nil)
        (start-time (current-time))
        (end-of-output (concat "^"
                               (regexp-quote tramp-end-of-output)
                               "$")))
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
                   (accept-process-output proc 1)
                   (goto-char (point-max))
                   (forward-line -1)
                   (setq found (looking-at end-of-output))))))
            (t
             (while (not found)
               (accept-process-output proc 1)
               (goto-char (point-max))
               (forward-line -1)
               (setq found (looking-at end-of-output))))))
    ;; At this point, either the timeout has expired or we have found
    ;; the end-of-output sentinel.
    (when found
      (goto-char (point-max))
      (forward-line -2)
      (delete-region (point) (point-max)))
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

(defun tramp-match-string-list (&optional string)
  "Returns list of all match strings.
That is, (list (match-string 0) (match-string 1) ...), according to the
number of matches."
  (let* ((nmatches (/ (length (match-data)) 2))
         (i (- nmatches 1))
         (res nil))
    (while (>= i 0)
      (setq res (cons (match-string i string) res))
      (setq i (- i 1)))
    res))

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

(defun tramp-send-region (multi-method method user host start end)
  "Send the region from START to END to remote command
running as USER on HOST using METHOD."
  (let ((proc (get-buffer-process
               (tramp-get-buffer multi-method method user host))))
    (unless proc
      (error "Can't send region to remote host -- not logged in"))
    (process-send-region proc start end)
    (when tramp-debug-buffer
      (append-to-buffer
       (tramp-get-debug-buffer multi-method method user host)
       start end))))

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
    (while (prog1 (erase-buffer) (accept-process-output p 0.25))
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

(defstruct tramp-file-name multi-method method user host path)

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
remote path name."
  (let (method)
    (save-match-data
      (unless (string-match (nth 0 tramp-file-name-structure) name)
        (error "Not a tramp file name: %s" name))
      (setq method (match-string (nth 1 tramp-file-name-structure) name))
      (if (and method (member method tramp-multi-methods))
          ;; If it's a multi method, the file name structure contains
          ;; arrays of method, user and host.
          (tramp-dissect-multi-file-name name)
        ;; Normal method.  First, find out default method.
	(let ((user (match-string (nth 2 tramp-file-name-structure) name))
	      (host (match-string (nth 3 tramp-file-name-structure) name))
	      (path (match-string (nth 4 tramp-file-name-structure) name)))
	  (when (not method)
	    (setq method (tramp-find-default-method user host)))
	  (make-tramp-file-name
	   :multi-method nil
	   :method method
	   :user (or user nil)
	   :host host
	   :path path))))))

(defun tramp-find-default-method (user host)
  "Look up the right method to use in `tramp-default-method-alist'."
  (let ((choices tramp-default-method-alist)
	(method tramp-default-method)
	item)
    (while choices
      (setq item (pop choices))
      (when (and (string-match (nth 0 item) host)
		 (string-match (nth 1 item) (or user "")))
	(setq method (nth 2 item))
	(setq choices nil)))
    method))
    
;; HHH: Not Changed.  Multi method.  Will probably not handle the case where
;;      a user name is not provided in the "file name" very well.
(defun tramp-dissect-multi-file-name (name)
  "Not implemented yet."
  (let ((regexp           (nth 0 tramp-multi-file-name-structure))
        (method-index     (nth 1 tramp-multi-file-name-structure))
        (hops-index       (nth 2 tramp-multi-file-name-structure))
        (path-index       (nth 3 tramp-multi-file-name-structure))
        (hop-regexp       (nth 0 tramp-multi-file-name-hop-structure))
        (hop-method-index (nth 1 tramp-multi-file-name-hop-structure))
        (hop-user-index   (nth 2 tramp-multi-file-name-hop-structure))
        (hop-host-index   (nth 3 tramp-multi-file-name-hop-structure))
        method hops len hop-methods hop-users hop-hosts path)
    (unless (string-match (format regexp hop-regexp) name)
      (error "Not a multi tramp file name: %s" name))
    (setq method (match-string method-index name))
    (setq hops (match-string hops-index name))
    (setq len (/ (length (match-data t)) 2))
    (when (< path-index 0) (incf path-index len))
    (setq path (match-string path-index name))
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
     :path         path)))

(defun tramp-make-tramp-file-name (multi-method method user host path)
  "Constructs a tramp file name from METHOD, USER, HOST and PATH."
  (unless tramp-make-tramp-file-format
    (error "`tramp-make-tramp-file-format' is nil"))
  (if multi-method
      (tramp-make-tramp-multi-file-name multi-method method user host path)
    (if user
        (format-spec tramp-make-tramp-file-format
                     `((?m . ,method) (?u . ,user) (?h . ,host) (?p . ,path)))
      (format-spec tramp-make-tramp-file-user-nil-format
                   `((?m . ,method) (?h . ,host) (?p . ,path))))))

;; CCC: Henrik Holm: Not Changed.  Multi Method.  What should be done
;; with this when USER is nil?
(defun tramp-make-tramp-multi-file-name (multi-method method user host path)
  "Constructs a tramp file name for a multi-hop method."
  (unless tramp-make-multi-tramp-file-format
    (error "`tramp-make-multi-tramp-file-format' is nil"))
  (let* ((prefix-format (nth 0 tramp-make-multi-tramp-file-format))
         (hop-format    (nth 1 tramp-make-multi-tramp-file-format))
         (path-format   (nth 2 tramp-make-multi-tramp-file-format))
         (prefix (format-spec prefix-format `((?m . ,multi-method))))
         (hops "")
         (path (format-spec path-format `((?p . ,path))))
         (i 0)
         (len (length method)))
    (while (< i len)
      (let ((m (aref method i)) (u (aref user i)) (h (aref host i)))
        (setq hops (concat hops (format-spec hop-format
					     `((?m . ,m) (?u . ,u) (?h . ,h)))))
        (incf i)))
    (concat prefix hops path)))

(defun tramp-make-rcp-program-file-name (user host path)
  "Create a file name suitable to be passed to `rcp'."
  (if user
      (format "%s@%s:%s" user host path)
    (format "%s:%s" host path)))

(defun tramp-make-ange-ftp-file-name (user host path)
  "Given user, host, and path, return an Ange-FTP filename."
  (if user
      (format "/%s@%s:%s" user host path)
    (format "/%s:%s" host path)))

(defun tramp-method-out-of-band-p (multi-method method)
  "Return t if this is an out-of-band method, nil otherwise.
It is important to check for this condition, since it is not possible
to enter a password for the `tramp-rcp-program'."
  (tramp-get-rcp-program multi-method method))

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
(defun tramp-get-encoding-command (multi-method method user host)
  (tramp-get-connection-property "encoding-command" nil
				 multi-method method user host))
(defun tramp-set-encoding-command (multi-method method user host command)
  (tramp-set-connection-property "encoding-command" command
				 multi-method method user host))
(defun tramp-get-decoding-command (multi-method method user host)
  (tramp-get-connection-property "decoding-command" nil
				 multi-method method user host))
(defun tramp-set-decoding-command (multi-method method user host command)
  (tramp-set-connection-property "decoding-command" command
				 multi-method method user host))
(defun tramp-get-encoding-function (multi-method method user host)
  (tramp-get-connection-property "encoding-function" nil
				 multi-method method user host))
(defun tramp-set-encoding-function (multi-method method user host func)
  (tramp-set-connection-property "encoding-function" func
				 multi-method method user host))
(defun tramp-get-decoding-function (multi-method method user host)
  (tramp-get-connection-property "decoding-function" nil
				 multi-method method user host))
(defun tramp-set-decoding-function (multi-method method user host func)
  (tramp-set-connection-property "decoding-function" func
				 multi-method method user host))


(defun tramp-get-connection-function (multi-method method)
  (second (or (assoc 'tramp-connection-function
                     (assoc (or multi-method method tramp-default-method)
                            tramp-methods))
              (error "Method `%s' didn't specify a connection function"
                     (or multi-method method)))))

(defun tramp-get-remote-sh (multi-method method)
  (second (or (assoc 'tramp-remote-sh
                     (assoc (or multi-method method tramp-default-method)
                            tramp-methods))
              (error "Method `%s' didn't specify a remote shell"
                     (or multi-method method)))))

(defun tramp-get-rsh-program (multi-method method)
  (second (or (assoc 'tramp-rsh-program
                     (assoc (or multi-method method tramp-default-method)
                            tramp-methods))
              (error "Method `%s' didn't specify an rsh program"
                     (or multi-method method)))))

(defun tramp-get-rsh-args (multi-method method)
  (second (or (assoc 'tramp-rsh-args
                     (assoc (or multi-method method tramp-default-method)
                            tramp-methods))
              (error "Method `%s' didn't specify rsh args"
                     (or multi-method method)))))

(defun tramp-get-rcp-program (multi-method method)
  (second (or (assoc 'tramp-rcp-program
                     (assoc (or multi-method method tramp-default-method)
                            tramp-methods))
              (error "Method `%s' didn't specify an rcp program"
                     (or multi-method method)))))

(defun tramp-get-rcp-args (multi-method method)
  (second (or (assoc 'tramp-rcp-args
                     (assoc (or multi-method method tramp-default-method)
                            tramp-methods))
              (error "Method `%s' didn't specify rcp args"
                     (or multi-method method)))))

(defun tramp-get-rcp-keep-date-arg (multi-method method)
  (second (or (assoc 'tramp-rcp-keep-date-arg
                     (assoc (or multi-method method tramp-default-method)
                            tramp-methods))
              (error "Method `%s' didn't specify `keep-date' arg for tramp"
                     (or multi-method method)))))

(defun tramp-get-su-program (multi-method method)
  (second (or (assoc 'tramp-su-program
                     (assoc (or multi-method method tramp-default-method)
                            tramp-methods))
              (error "Method `%s' didn't specify a su program"
                     (or multi-method method)))))

(defun tramp-get-su-args (multi-method method)
  (second (or (assoc 'tramp-su-args
                     (assoc (or multi-method method tramp-default-method)
                            tramp-methods))
              (error "Method `%s' didn't specify su args"
                     (or multi-method method)))))

(defun tramp-get-telnet-program (multi-method method)
  (second (or (assoc 'tramp-telnet-program
                     (assoc (or multi-method method tramp-default-method)
                            tramp-methods))
              (error "Method `%s' didn't specify a telnet program"
                     (or multi-method method)))))

(defun tramp-get-telnet-args (multi-method method)
  (second (or (assoc 'tramp-telnet-args
                     (assoc (or multi-method method tramp-default-method)
                            tramp-methods))
              (error "Method `%s' didn't specify telnet args"
                     (or multi-method method)))))

;; (defun tramp-get-encoding-command (multi-method method)
;;   (second (or (assoc 'tramp-encoding-command
;;                      (assoc (or multi-method method tramp-default-method)
;;                             tramp-methods))
;;               (error "Method `%s' didn't specify an encoding command"
;;                      (or multi-method method)))))

;; (defun tramp-get-decoding-command (multi-method method)
;;   (second (or (assoc 'tramp-decoding-command
;;                      (assoc (or multi-method method tramp-default-method)
;;                             tramp-methods))
;;               (error "Method `%s' didn't specify a decoding command"
;;                      (or multi-method method)))))

;; (defun tramp-get-encoding-function (multi-method method)
;;   (second (or (assoc 'tramp-encoding-function
;;                      (assoc (or multi-method method tramp-default-method)
;;                             tramp-methods))
;;               (error "Method `%s' didn't specify an encoding function"
;;                      (or multi-method method)))))

;; (defun tramp-get-decoding-function (multi-method method)
;;   (second (or (assoc 'tramp-decoding-function
;;                      (assoc (or multi-method method tramp-default-method)
;;                             tramp-methods))
;;               (error "Method `%s' didn't specify a decoding function"
;;                      (or multi-method method)))))

;; Auto saving to a special directory.

(defun tramp-make-auto-save-file-name (fn)
  "Returns a file name in `tramp-auto-save-directory' for autosaving this file."
  (when tramp-auto-save-directory
    (unless (file-exists-p tramp-auto-save-directory)
      (make-directory tramp-auto-save-directory t)))
  ;; jka-compr doesn't like auto-saving, so by appending "~" to the
  ;; file name we make sure that jka-compr isn't used for the
  ;; auto-save file.
  (let ((buffer-file-name (expand-file-name
			   (tramp-subst-strs-in-string '(("_" . "|")
							 ("/" . "_a")
							 (":" . "_b")
							 ("|" . "__")
							 ("[" . "_l")
							 ("]" . "_r"))
						       fn)
			   tramp-auto-save-directory)))
    (make-auto-save-file-name)))

(defadvice make-auto-save-file-name
  (around tramp-advice-make-auto-save-file-name () activate)
  "Invoke `tramp-make-auto-save-file-name' for tramp files."
  (if (and (buffer-file-name) (tramp-tramp-file-p (buffer-file-name))
	   tramp-auto-save-directory)
      (setq ad-return-value
            (tramp-make-auto-save-file-name (buffer-file-name)))
    ad-do-it))

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

(defun tramp-read-passwd (prompt)
  "Read a password from user (compat function).
Invokes `read-passwd' if that is defined, else `ange-ftp-read-passwd'."
  (apply
   (if (fboundp 'read-passwd) #'read-passwd #'ange-ftp-read-passwd)
   (list prompt)))

(defun tramp-time-diff (t1 t2)
  "Return the difference between the two times, in seconds.
T1 and T2 are time values (as returned by `current-time' for example).

NOTE: This function will fail if the time difference is too large to
fit in an integer."
  ;; Pacify byte-compiler with `symbol-function'.
  (cond ((fboundp 'subtract-time)
         (cadr (funcall (symbol-function 'subtract-time) t1 t2)))
        ((fboundp 'itimer-time-difference)
         (floor (funcall
		 (symbol-function 'itimer-time-difference)
		 (if (< (length t1) 3) (append t1 '(0)) t1)
		 (if (< (length t2) 3) (append t2 '(0)) t2))))
        (t
         ;; snarfed from Emacs 21 time-date.el
         (cadr (let ((borrow (< (cadr t1) (cadr t2))))
                 (list (- (car t1) (car t2) (if borrow 1 0))
                       (- (+ (if borrow 65536 0) (cadr t1)) (cadr t2))))))))

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
;; ;; away from tramp.el paths.
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
;; (defadvice efs-ftp-path (around dont-match-tramp-path activate protect)
;;   "Cause efs-ftp-path to fail when the path is a TRAMP path."
;;   (if (tramp-tramp-file-p (ad-get-arg 0))
;;       nil
;;     ad-do-it))

;; We currently use "[" and "]" in the filename format.  In Emacs
;; 20.x, this means that Emacs wants to expand wildcards if
;; `find-file-wildcards' is non-nil, and then barfs because no
;; expansion could be found.  We detect this situation and do
;; something really awful: we have `file-expand-wildcards' return the
;; original filename if it can't expand anything.  Let's just hope
;; that this doesn't break anything else.
;;
;; Another problem is that the check is done by Emacs version, which
;; is really not what we want to do.  Oh, well.

;;(when (and (not (featurep 'xemacs))
;;	   (= emacs-major-version 20))
;; It seems that this advice is needed in Emacs 21, too.
(defadvice file-expand-wildcards (around tramp-fix activate)
  (let ((name (ad-get-arg 0)))
    (if (tramp-tramp-file-p name)
	;; If it's a Tramp file, dissect it and look if wildcards
	;; need to be expanded at all.
	(let ((v (tramp-dissect-file-name name)))
	  (if (string-match "[[*?]" (tramp-file-name-path v))
	      (let ((res ad-do-it))
		(setq ad-return-value (or res (list name))))
	    (setq ad-return-value (list name))))
      ;; If it is not a Tramp file, just run the original function.
      (let ((res ad-do-it))
	(setq ad-return-value (or res (list name)))))))
;;  )

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
  (let ((reporter-prompt-for-summary-p	t))
    (reporter-submit-bug-report
     tramp-bug-report-address		; to-address
     (format "tramp (%s)" tramp-version) ; package name and version
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
       tramp-remote-path
       tramp-login-prompt-regexp
       tramp-password-prompt-regexp
       tramp-wrong-passwd-regexp
       tramp-temp-name-prefix
       tramp-file-name-structure
       tramp-file-name-regexp
       tramp-multi-file-name-structure
       tramp-multi-file-name-hop-structure
       tramp-multi-methods
       tramp-multi-connection-function-alist
       tramp-make-tramp-file-format
       tramp-end-of-output

       ;; Non-tramp variables of interest
       shell-prompt-pattern
       backup-by-copying
       backup-by-copying-when-linked
       backup-by-copying-when-mismatch
       ,(when (boundp 'backup-by-copying-when-privileged-mismatch)
          'backup-by-copying-when-privileged-mismatch)
       file-name-handler-alist)
     nil				; pre-hook
     nil				; post-hook
     "\
Enter your bug report in this message, including as much detail as you
possibly can about the problem, what you did to cause it and what the
local and remote machines are.

If you can give a simple set of instructions to make this bug happen
reliably, please include those.  Thank you for helping kill bugs in
TRAMP.
--bug report follows this line--")))

(defalias 'tramp-submit-bug 'tramp-bug)

(provide 'tramp)

;; Make sure that we get integration with the VC package.
;; When it is loaded, we need to pull in the integration module.
;; This must come after (provide 'tramp) because tramp-vc.el
;; requires tramp.
(eval-after-load "vc"
  '(require 'tramp-vc))

;;; TODO:

;; * Revise the comments near the beginning of the file.
;; * Cooperate with PCL-CVS.  It uses start-process, which doesn't
;;   work for remote files.
;; * Rewrite `tramp-shell-quote-argument' to abstain from using
;; `shell-quote-argument'.
;; * Completion gets confused when you leave out the method name.
;; * Support `dired-compress-file' filename handler.
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
;; * Make sure permissions of tmp file are good.
;;   (Nelson Minar <nelson@media.mit.edu>)
;; * Grok passwd prompts with scp?  (David Winter
;;   <winter@nevis1.nevis.columbia.edu>).  Maybe just do `ssh -l user
;;   host', then wait a while for the passwd or passphrase prompt.  If
;;   there is one, remember the passwd/phrase.
;; * How to deal with MULE in `insert-file-contents' and `write-region'?
;; * Do asynchronous `shell-command's.
;; * Grok `append' parameter for `write-region'.
;; * Test remote ksh or bash for tilde expansion in `tramp-find-shell'?
;; * abbreviate-file-name
;; * grok ~ in tramp-remote-path  (Henrik Holm <henrikh@tele.ntnu.no>)
;; * `C' in dired gives error `not tramp file name'.
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
;;   (Francesco Potortì)
;; * Should we set PATH ourselves or should we rely on the remote end
;;   to do it?
;; * Do the autoconf thing.
;; * Make it work for XEmacs 20, which is missing `with-timeout'.
;; * Allow non-Unix remote systems.  (More a long-term thing.)
;; * Make it work for different encodings, and for different file name
;;   encodings, too.  (Daniel Pittman)
;; * Change applicable functions to pass a struct tramp-file-name rather
;;   than the individual items MULTI-METHOD, METHOD, USER, HOST, PATH.
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
;; * When user is running ssh-agent, it would be useful to add the
;;   passwords typed by the user to that agent.  This way, the next time
;;   round, the users don't have to type all this in again.
;;   This would be especially useful for start-process, I think.
;;   An easy way to implement start-process is to open a second shell
;;   connection which is inconvenient if the user has to reenter
;;   passwords.
;; * Change `copy-file' to grok the case where the filename handler
;;   for the source and the target file are different.  Right now,
;;   it looks at the source file and then calls that handler, if
;;   there is one.  But since ange-ftp, for instance, does not know
;;   about Tramp, it does not do the right thing if the target file
;;   name is a Tramp name.

;; Functions for file-name-handler-alist:
;; diff-latest-backup-file -- in diff.el
;; dired-compress-file
;; dired-uncache -- this will be needed when we do insert-directory caching
;; file-name-as-directory -- use primitive?
;; file-name-directory -- use primitive?
;; file-name-nondirectory -- use primitive?
;; file-name-sans-versions -- use primitive?
;; file-newer-than-file-p
;; find-backup-file-name
;; get-file-buffer -- use primitive
;; load
;; unhandled-file-name-directory
;; vc-registered

;;; tramp.el ends here
