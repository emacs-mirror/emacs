;;; tramp.el --- Transparent Remote Access, Multiple Protocol  -*- lexical-binding:t -*-

;; Copyright (C) 1998-2018 Free Software Foundation, Inc.

;; Author: Kai Großjohann <kai.grossjohann@gmx.net>
;;         Michael Albinus <michael.albinus@gmx.de>
;; Maintainer: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes
;; Package: tramp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

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
;; This package only works for Emacs 24.1 and higher.
;;
;; Also see the todo list at the bottom of this file.
;;
;; The current version of Tramp can be retrieved from the following URL:
;;            https://ftp.gnu.org/gnu/tramp/
;;
;; There's a mailing list for this, as well.  Its name is:
;;            tramp-devel@gnu.org
;; You can use the Web to subscribe, under the following URL:
;;            https://lists.gnu.org/mailman/listinfo/tramp-devel
;;
;; For the adventurous, the current development sources are available
;; via Git.  You can find instructions about this at the following URL:
;;            https://savannah.gnu.org/projects/tramp/
;;
;; Don't forget to put on your asbestos longjohns, first!

;;; Code:

(require 'tramp-compat)
(require 'trampver)

;; Pacify byte-compiler.
(require 'cl-lib)
(defvar auto-save-file-name-transforms)
(defvar eshell-path-env)
(defvar ls-lisp-use-insert-directory-program)
(defvar outline-regexp)

;;; User Customizable Internal Variables:

(defgroup tramp nil
  "Edit remote files with a combination of ssh, scp, etc."
  :group 'files
  :group 'comm
  :link '(custom-manual "(tramp)Top")
  :version "22.1")

;; Maybe we need once a real Tramp mode, with key bindings etc.
;;;###autoload
(defcustom tramp-mode t
  "Whether Tramp is enabled.
If it is set to nil, all remote file names are used literally."
  :group 'tramp
  :type 'boolean
  :require 'tramp)

(defcustom tramp-verbose 3
  "Verbosity level for Tramp messages.
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
 9  test commands
10  traces (huge)."
  :group 'tramp
  :type 'integer
  :require 'tramp)

(defcustom tramp-backup-directory-alist nil
  "Alist of filename patterns and backup directory names.
Each element looks like (REGEXP . DIRECTORY), with the same meaning like
in `backup-directory-alist'.  If a Tramp file is backed up, and DIRECTORY
is a local file name, the backup directory is prepended with Tramp file
name prefix \(method, user, host) of file.

\(setq tramp-backup-directory-alist backup-directory-alist)

gives the same backup policy for Tramp files on their hosts like the
policy for local files."
  :group 'tramp
  :type '(repeat (cons (regexp :tag "Regexp matching filename")
		       (directory :tag "Backup directory name")))
  :require 'tramp)

(defcustom tramp-auto-save-directory nil
  "Put auto-save files in this directory, if set.
The idea is to use a local directory so that auto-saving is faster.
This setting has precedence over `auto-save-file-name-transforms'."
  :group 'tramp
  :type '(choice (const :tag "Use default" nil)
		 (directory :tag "Auto save directory name"))
  :require 'tramp)

(defcustom tramp-encoding-shell
  (or (tramp-compat-funcall 'w32-shell-name) "/bin/sh")
  "Use this program for encoding and decoding commands on the local host.
This shell is used to execute the encoding and decoding command on the
local host, so if you want to use `~' in those commands, you should
choose a shell here which groks tilde expansion.  `/bin/sh' normally
does not understand tilde expansion.

For encoding and decoding, commands like the following are executed:

    /bin/sh -c COMMAND < INPUT > OUTPUT

This variable can be used to change the \"/bin/sh\" part.  See the
variable `tramp-encoding-command-switch' for the \"-c\" part.

If the shell must be forced to be interactive, see
`tramp-encoding-command-interactive'.

Note that this variable is not used for remote commands.  There are
mechanisms in tramp.el which automatically determine the right shell to
use for the remote host."
  :group 'tramp
  :type '(file :must-match t)
  :require 'tramp)

(defcustom tramp-encoding-command-switch
  (if (tramp-compat-funcall 'w32-shell-dos-semantics) "/c" "-c")
  "Use this switch together with `tramp-encoding-shell' for local commands.
See the variable `tramp-encoding-shell' for more information."
  :group 'tramp
  :type 'string
  :require 'tramp)

(defcustom tramp-encoding-command-interactive
  (unless (tramp-compat-funcall 'w32-shell-dos-semantics) "-i")
  "Use this switch together with `tramp-encoding-shell' for interactive shells.
See the variable `tramp-encoding-shell' for more information."
  :version "24.1"
  :group 'tramp
  :type '(choice (const nil) string)
  :require 'tramp)

;;;###tramp-autoload
(defvar tramp-methods nil
  "Alist of methods for remote files.
This is a list of entries of the form (NAME PARAM1 PARAM2 ...).
Each NAME stands for a remote access method.  Each PARAM is a
pair of the form (KEY VALUE).  The following KEYs are defined:
  * `tramp-remote-shell'
    This specifies the shell to use on the remote host.  This
    MUST be a Bourne-like shell.  It is normally not necessary to
    set this to any value other than \"/bin/sh\": Tramp wants to
    use a shell which groks tilde expansion, but it can search
    for it.  Also note that \"/bin/sh\" exists on all Unixen,
    this might not be true for the value that you decide to use.
    You Have Been Warned.
  * `tramp-remote-shell-login'
    This specifies the arguments to let `tramp-remote-shell' run
    as a login shell.  It defaults to (\"-l\"), but some shells,
    like ksh, require another argument.  See
    `tramp-connection-properties' for a way to overwrite the
    default value.
  * `tramp-remote-shell-args'
    For implementation of `shell-command', this specifies the
    arguments to let `tramp-remote-shell' run a single command.
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
    parameter of a program, if exists.  \"%c\" adds additional
    `tramp-ssh-controlmaster-options' options for the first hop.
  * `tramp-login-env'
     A list of environment variables and their values, which will
     be set when calling `tramp-login-program'.
  * `tramp-async-args'
    When an asynchronous process is started, we know already that
    the connection works.  Therefore, we can pass additional
    parameters to suppress diagnostic messages, in order not to
    tamper the process output.
  * `tramp-copy-program'
    This specifies the name of the program to use for remotely copying
    the file; this might be the absolute filename of scp or the name of
    a workalike program.  It is always applied on the local host.
  * `tramp-copy-args'
    This specifies the list of parameters to pass to the above mentioned
    program, the hints for `tramp-login-args' also apply here.
  * `tramp-copy-env'
     A list of environment variables and their values, which will
     be set when calling `tramp-copy-program'.
  * `tramp-remote-copy-program'
    The listener program to be applied on remote side, if needed.
  * `tramp-remote-copy-args'
    The list of parameters to pass to the listener program, the hints
    for `tramp-login-args' also apply here.  Additionally, \"%r\" could
    be used here and in `tramp-copy-args'.  It denotes a randomly
    chosen port for the remote listener.
  * `tramp-copy-keep-date'
    This specifies whether the copying program when the preserves the
    timestamp of the original file.
  * `tramp-copy-keep-tmpfile'
    This specifies whether a temporary local file shall be kept
    for optimization reasons (useful for \"rsync\" methods).
  * `tramp-copy-recursive'
    Whether the operation copies directories recursively.
  * `tramp-default-port'
    The default port of a method.
  * `tramp-tmpdir'
    A directory on the remote host for temporary files.  If not
    specified, \"/tmp\" is taken as default.
  * `tramp-connection-timeout'
    This is the maximum time to be spent for establishing a connection.
    In general, the global default value shall be used, but for
    some methods, like \"su\" or \"sudo\", a shorter timeout
    might be desirable.
  * `tramp-case-insensitive'
    Whether the remote file system handles file names case insensitive.
    Only a non-nil value counts, the default value nil means to
    perform further checks on the remote host.  See
    `tramp-connection-properties' for a way to overwrite this.

What does all this mean?  Well, you should specify `tramp-login-program'
for all methods; this program is used to log in to the remote site.  Then,
there are two ways to actually transfer the files between the local and the
remote side.  One way is using an additional scp-like program.  If you want
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
inline method, then these two parameters should be nil.

Notes:

When using `su' or `sudo' the phrase \"open connection to a remote
host\" sounds strange, but it is used nevertheless, for consistency.
No connection is opened to a remote host, but `su' or `sudo' is
started on the local host.  You should specify a remote host
`localhost' or the name of the local host.  Another host name is
useful only in combination with `tramp-default-proxies-alist'.")

(defcustom tramp-default-method
  ;; An external copy method seems to be preferred, because it performs
  ;; much better for large files, and it hasn't too serious delays
  ;; for small files.  But it must be ensured that there aren't
  ;; permanent password queries.  Either a password agent like
  ;; "ssh-agent" or "Pageant" shall run, or the optional
  ;; password-cache.el or auth-sources.el packages shall be active for
  ;; password caching.  If we detect that the user is running OpenSSH
  ;; 4.0 or newer, we could reuse the connection, which calls also for
  ;; an external method.
  (cond
   ;; PuTTY is installed.  We don't take it, if it is installed on a
   ;; non-windows system, or pscp from the pssh (parallel ssh) package
   ;; is found.
   ((and (eq system-type 'windows-nt) (executable-find "pscp")) "pscp")
   ;; There is an ssh installation.
   ((executable-find "scp") "scp")
   ;; Fallback.
   (t "ftp"))
  "Default method to use for transferring files.
See `tramp-methods' for possibilities.
Also see `tramp-default-method-alist'."
  :group 'tramp
  :type 'string
  :require 'tramp)

;;;###tramp-autoload
(defcustom tramp-default-method-alist nil
  "Default method to use for specific host/user pairs.
This is an alist of items (HOST USER METHOD).  The first matching item
specifies the method to use for a file name which does not specify a
method.  HOST and USER are regular expressions or nil, which is
interpreted as a regular expression which always matches.  If no entry
matches, the variable `tramp-default-method' takes effect.

If the file name does not specify the user, lookup is done using the
empty string for the user name.

See `tramp-methods' for a list of possibilities for METHOD."
  :group 'tramp
  :type '(repeat (list (choice :tag "Host regexp" regexp sexp)
		       (choice :tag "User regexp" regexp sexp)
		       (choice :tag "Method name" string (const nil))))
  :require 'tramp)

(defconst tramp-default-method-marker "-"
  "Marker for default method in remote file names.")

(defcustom tramp-default-user nil
  "Default user to use for transferring files.
It is nil by default; otherwise settings in configuration files like
\"~/.ssh/config\" would be overwritten.  Also see `tramp-default-user-alist'.

This variable is regarded as obsolete, and will be removed soon."
  :group 'tramp
  :type '(choice (const nil) string)
  :require 'tramp)

;;;###tramp-autoload
(defcustom tramp-default-user-alist nil
  "Default user to use for specific method/host pairs.
This is an alist of items (METHOD HOST USER).  The first matching item
specifies the user to use for a file name which does not specify a
user.  METHOD and HOST are regular expressions or nil, which is
interpreted as a regular expression which always matches.  If no entry
matches, the variable `tramp-default-user' takes effect.

If the file name does not specify the method, lookup is done using the
empty string for the method name."
  :group 'tramp
  :type '(repeat (list (choice :tag "Method regexp" regexp sexp)
		       (choice :tag "  Host regexp" regexp sexp)
		       (choice :tag "    User name" string (const nil))))
  :require 'tramp)

(defcustom tramp-default-host (system-name)
  "Default host to use for transferring files.
Useful for su and sudo methods mostly."
  :group 'tramp
  :type 'string
  :require 'tramp)

;;;###tramp-autoload
(defcustom tramp-default-host-alist nil
  "Default host to use for specific method/user pairs.
This is an alist of items (METHOD USER HOST).  The first matching item
specifies the host to use for a file name which does not specify a
host.  METHOD and USER are regular expressions or nil, which is
interpreted as a regular expression which always matches.  If no entry
matches, the variable `tramp-default-host' takes effect.

If the file name does not specify the method, lookup is done using the
empty string for the method name."
  :group 'tramp
  :version "24.4"
  :type '(repeat (list (choice :tag "Method regexp" regexp sexp)
		       (choice :tag "  User regexp" regexp sexp)
		       (choice :tag "    Host name" string (const nil))))
  :require 'tramp)

(defcustom tramp-default-proxies-alist nil
  "Route to be followed for specific host/user pairs.
This is an alist of items (HOST USER PROXY).  The first matching
item specifies the proxy to be passed for a file name located on
a remote target matching USER@HOST.  HOST and USER are regular
expressions.  PROXY must be a Tramp filename without a localname
part.  Method and user name on PROXY are optional, which is
interpreted with the default values.  PROXY can contain the
patterns %h and %u, which are replaced by the strings matching
HOST or USER, respectively.

HOST, USER or PROXY could also be Lisp forms, which will be
evaluated.  The result must be a string or nil, which is
interpreted as a regular expression which always matches."
  :group 'tramp
  :type '(repeat (list (choice :tag "Host regexp" regexp sexp)
		       (choice :tag "User regexp" regexp sexp)
		       (choice :tag " Proxy name" string (const nil))))
  :require 'tramp)

(defcustom tramp-save-ad-hoc-proxies nil
  "Whether to save ad-hoc proxies persistently."
  :group 'tramp
  :version "24.3"
  :type 'boolean
  :require 'tramp)

(defcustom tramp-restricted-shell-hosts-alist
  (when (memq system-type '(windows-nt))
    (list (concat "\\`" (regexp-quote (system-name)) "\\'")))
  "List of hosts, which run a restricted shell.
This is a list of regular expressions, which denote hosts running
a registered shell like \"rbash\".  Those hosts can be used as
proxies only, see `tramp-default-proxies-alist'.  If the local
host runs a registered shell, it shall be added to this list, too."
  :version "24.3"
  :group 'tramp
  :type '(repeat (regexp :tag "Host regexp"))
  :require 'tramp)

;;;###tramp-autoload
(defconst tramp-local-host-regexp
  (concat
   "\\`"
   (regexp-opt
    (list "localhost" "localhost6" (system-name) "127.0.0.1" "::1") t)
   "\\'")
  "Host names which are regarded as local host.")

(defvar tramp-completion-function-alist nil
  "Alist of methods for remote files.
This is a list of entries of the form \(NAME PAIR1 PAIR2 ...).
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
 * `tramp-parse-etc-group'   for \"/etc/group\" like files.
 * `tramp-parse-netrc'       for \"~/.netrc\" like files.
 * `tramp-parse-putty'       for PuTTY registered sessions.

FUNCTION can also be a user defined function.  For more details see
the info pages.")

(defconst tramp-echo-mark-marker "_echo"
  "String marker to surround echoed commands.")

(defconst tramp-echo-mark-marker-length (length tramp-echo-mark-marker)
  "String length of `tramp-echo-mark-marker'.")

(defconst tramp-echo-mark
  (concat tramp-echo-mark-marker
	  (make-string tramp-echo-mark-marker-length ?\b))
  "String mark to be transmitted around shell commands.
Used to separate their echo from the output they produce.  This
will only be used if we cannot disable remote echo via stty.
This string must have no effect on the remote shell except for
producing some echo which can later be detected by
`tramp-echoed-echo-mark-regexp'.  Using `tramp-echo-mark-marker',
followed by an equal number of backspaces to erase them will
usually suffice.")

(defconst tramp-echoed-echo-mark-regexp
  (format "%s\\(\b\\( \b\\)?\\)\\{%d\\}"
	  tramp-echo-mark-marker tramp-echo-mark-marker-length)
  "Regexp which matches `tramp-echo-mark' as it gets echoed by
the remote shell.")

(defcustom tramp-local-end-of-line
  (if (memq system-type '(windows-nt)) "\r\n" "\n")
  "String used for end of line in local processes."
  :version "24.1"
  :group 'tramp
  :type 'string
  :require 'tramp)

(defcustom tramp-rsh-end-of-line "\n"
  "String used for end of line in rsh connections.
I don't think this ever needs to be changed, so please tell me about it
if you need to change this."
  :group 'tramp
  :type 'string
  :require 'tramp)

(defcustom tramp-login-prompt-regexp
  ".*\\(user\\|login\\)\\( .*\\)?: *"
  "Regexp matching login-like prompts.
The regexp should match at end of buffer.

Sometimes the prompt is reported to look like \"login as:\"."
  :group 'tramp
  :type 'regexp
  :require 'tramp)

(defcustom tramp-shell-prompt-pattern
  ;; Allow a prompt to start right after a ^M since it indeed would be
  ;; displayed at the beginning of the line (and Zsh uses it).  This
  ;; regexp works only for GNU Emacs.
  ;; Allow also [] style prompts.  They can appear only during
  ;; connection initialization; Tramp redefines the prompt afterwards.
  (concat "\\(?:^\\|\r\\)"
	  "[^]#$%>\n]*#?[]#$%>] *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*")
  "Regexp to match prompts from remote shell.
Normally, Tramp expects you to configure `shell-prompt-pattern'
correctly, but sometimes it happens that you are connecting to a
remote host which sends a different kind of shell prompt.  Therefore,
Tramp recognizes things matched by `shell-prompt-pattern' as prompt,
and also things matched by this variable.  The default value of this
variable is similar to the default value of `shell-prompt-pattern',
which should work well in many cases.

This regexp must match both `tramp-initial-end-of-output' and
`tramp-end-of-output'."
  :group 'tramp
  :type 'regexp
  :require 'tramp)

(defcustom tramp-password-prompt-regexp
  (format "^.*\\(%s\\).*:\^@? *"
	  ;; `password-word-equivalents' has been introduced with Emacs 24.4.
          (regexp-opt (or (bound-and-true-p password-word-equivalents)
                          '("password" "passphrase"))))
  "Regexp matching password-like prompts.
The regexp should match at end of buffer.

The `sudo' program appears to insert a `^@' character into the prompt."
  :version "24.4"
  :group 'tramp
  :type 'regexp
  :require 'tramp)

(defcustom tramp-wrong-passwd-regexp
  (concat "^.*"
	  ;; These strings should be on the last line
	  (regexp-opt '("Permission denied"
			"Login incorrect"
			"Login Incorrect"
			"Connection refused"
			"Connection closed"
			"Timeout, server not responding."
			"Sorry, try again."
			"Name or service not known"
			"Host key verification failed."
			"No supported authentication methods left to try!")
		      t)
	  ".*"
	  "\\|"
	  "^.*\\("
	  ;; Here comes a list of regexes, separated by \\|
	  "Received signal [0-9]+"
	  "\\).*")
  "Regexp matching a `login failed' message.
The regexp should match at end of buffer."
  :group 'tramp
  :type 'regexp
  :require 'tramp)

(defcustom tramp-yesno-prompt-regexp
  (concat
   (regexp-opt '("Are you sure you want to continue connecting (yes/no)?") t)
   "\\s-*")
  "Regular expression matching all yes/no queries which need to be confirmed.
The confirmation should be done with yes or no.
The regexp should match at end of buffer.
See also `tramp-yn-prompt-regexp'."
  :group 'tramp
  :type 'regexp
  :require 'tramp)

(defcustom tramp-yn-prompt-regexp
  (concat
   (regexp-opt '("Store key in cache? (y/n)"
		 "Update cached key? (y/n, Return cancels connection)")
               t)
   "\\s-*")
  "Regular expression matching all y/n queries which need to be confirmed.
The confirmation should be done with y or n.
The regexp should match at end of buffer.
See also `tramp-yesno-prompt-regexp'."
  :group 'tramp
  :type 'regexp
  :require 'tramp)

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
  :type 'regexp
  :require 'tramp)

(defcustom tramp-operation-not-permitted-regexp
  (concat "\\(" "preserving times.*" "\\|" "set mode" "\\)" ":\\s-*"
	  (regexp-opt '("Operation not permitted") t))
  "Regular expression matching keep-date problems in (s)cp operations.
Copying has been performed successfully already, so this message can
be ignored safely."
  :group 'tramp
  :type 'regexp
  :require 'tramp)

(defcustom tramp-copy-failed-regexp
  (concat "\\(.+: "
          (regexp-opt '("Permission denied"
                        "not a regular file"
                        "is a directory"
                        "No such file or directory")
                      t)
          "\\)\\s-*")
  "Regular expression matching copy problems in (s)cp operations."
  :group 'tramp
  :type 'regexp
  :require 'tramp)

(defcustom tramp-process-alive-regexp
  ""
  "Regular expression indicating a process has finished.
In fact this expression is empty by intention, it will be used only to
check regularly the status of the associated process.
The answer will be provided by `tramp-action-process-alive',
`tramp-action-out-of-band', which see."
  :group 'tramp
  :type 'regexp
  :require 'tramp)

(defconst tramp-temp-name-prefix "tramp."
  "Prefix to use for temporary files.
If this is a relative file name (such as \"tramp.\"), it is considered
relative to the directory name returned by the function
`tramp-compat-temporary-file-directory' (which see).  It may also be an
absolute file name; don't forget to include a prefix for the filename
part, though.")

(defconst tramp-temp-buffer-name " *tramp temp*"
  "Buffer name for a temporary buffer.
It shall be used in combination with `generate-new-buffer-name'.")

(defvar tramp-temp-buffer-file-name nil
  "File name of a persistent local temporary file.
Useful for \"rsync\" like methods.")
(make-variable-buffer-local 'tramp-temp-buffer-file-name)
(put 'tramp-temp-buffer-file-name 'permanent-local t)

;;;###tramp-autoload
(defcustom tramp-syntax 'default
  "Tramp filename syntax to be used.

It can have the following values:

  `default'    -- Default syntax
  `simplified' -- Ange-FTP like syntax
  `separate'   -- Syntax as defined for XEmacs originally

Do not change the value by `setq', it must be changed only via
Customize.  See also `tramp-change-syntax'."
  :group 'tramp
  :version "26.1"
  :package-version '(Tramp . "2.3.3")
  :type '(choice (const :tag "Default" default)
		 (const :tag "Ange-FTP" simplified)
		 (const :tag "XEmacs" separate))
  :require 'tramp
  :initialize 'custom-initialize-set
  :set 'tramp-set-syntax)

(defun tramp-set-syntax (symbol value)
  "Set SYMBOL to value VALUE.
Used in user option `tramp-syntax'.  There are further variables
to be set, depending on VALUE."
  ;; Check allowed values.
  (unless (memq value (tramp-syntax-values))
    (tramp-compat-user-error "Wrong `tramp-syntax' %s" tramp-syntax))
  ;; Cleanup existing buffers.
  (unless (eq (symbol-value symbol) value)
    (tramp-cleanup-all-buffers))
  ;; Set the value:
  (set-default symbol value)
  ;; Reset the depending variables.
  (with-no-warnings
    (setq tramp-prefix-format (tramp-build-prefix-format)
	  tramp-prefix-regexp (tramp-build-prefix-regexp)
	  tramp-method-regexp (tramp-build-method-regexp)
	  tramp-postfix-method-format (tramp-build-postfix-method-format)
	  tramp-postfix-method-regexp (tramp-build-postfix-method-regexp)
	  tramp-prefix-ipv6-format (tramp-build-prefix-ipv6-format)
	  tramp-prefix-ipv6-regexp (tramp-build-prefix-ipv6-regexp)
	  tramp-postfix-ipv6-format (tramp-build-postfix-ipv6-format)
	  tramp-postfix-ipv6-regexp (tramp-build-postfix-ipv6-regexp)
	  tramp-postfix-host-format (tramp-build-postfix-host-format)
	  tramp-postfix-host-regexp (tramp-build-postfix-host-regexp)
	  tramp-remote-file-name-spec-regexp
          (tramp-build-remote-file-name-spec-regexp)
	  tramp-file-name-structure (tramp-build-file-name-structure)
	  tramp-file-name-regexp (tramp-build-file-name-regexp)
	  tramp-completion-file-name-regexp
          (tramp-build-completion-file-name-regexp)))
  ;; Rearrange file name handlers.
  (tramp-register-file-name-handlers))

;; Initialize the Tramp syntax variables.  We want to override initial
;; value of `tramp-file-name-regexp'.  Other Tramp syntax variables
;; must be initialized as well to proper values.  We do not call
;; `custom-set-variable', this would load Tramp via custom.el.
(eval-after-load 'tramp
  '(tramp-set-syntax 'tramp-syntax (tramp-compat-tramp-syntax)))

(defun tramp-syntax-values ()
  "Return possible values of `tramp-syntax', a list"
  (let ((values (cdr (get 'tramp-syntax 'custom-type))))
    (setq values (mapcar 'last values)
	  values (mapcar 'car values))))

(defun tramp-lookup-syntax (alist)
  "Look up a syntax string in ALIST according to `tramp-compat-tramp-syntax.'
Raise an error if `tramp-syntax' is invalid."
  (or (cdr (assq (tramp-compat-tramp-syntax) alist))
      (error "Wrong `tramp-syntax' %s" tramp-syntax)))

(defconst tramp-prefix-format-alist
  '((default    . "/")
    (simplified . "/")
    (separate   . "/["))
  "Alist mapping Tramp syntax to strings beginning Tramp file names.")

(defun tramp-build-prefix-format ()
  (tramp-lookup-syntax tramp-prefix-format-alist))

(defvar tramp-prefix-format (tramp-build-prefix-format)
  "String matching the very beginning of Tramp file names.
Used in `tramp-make-tramp-file-name'.")

(defun tramp-build-prefix-regexp ()
  (concat "^" (regexp-quote tramp-prefix-format)))

(defvar tramp-prefix-regexp (tramp-build-prefix-regexp)
  "Regexp matching the very beginning of Tramp file names.
Should always start with \"^\". Derived from `tramp-prefix-format'.")

(defconst tramp-method-regexp-alist
  '((default    . "[a-zA-Z0-9-]+")
    (simplified . "")
    (separate   . "[a-zA-Z0-9-]*"))
  "Alist mapping Tramp syntax to regexps matching methods identifiers.")

(defun tramp-build-method-regexp ()
  (tramp-lookup-syntax tramp-method-regexp-alist))

(defvar tramp-method-regexp (tramp-build-method-regexp)
  "Regexp matching methods identifiers.
The `ftp' syntax does not support methods.")

(defconst tramp-postfix-method-format-alist
  '((default    . ":")
    (simplified . "")
    (separate   . "/"))
  "Alist mapping Tramp syntax to the delimiter after the method.")

(defun tramp-build-postfix-method-format ()
  (tramp-lookup-syntax tramp-postfix-method-format-alist))

(defvar tramp-postfix-method-format (tramp-build-postfix-method-format)
  "String matching delimiter between method and user or host names.
The `ftp' syntax does not support methods.
Used in `tramp-make-tramp-file-name'.")

(defun tramp-build-postfix-method-regexp ()
  (regexp-quote tramp-postfix-method-format))

(defvar tramp-postfix-method-regexp (tramp-build-postfix-method-regexp)
  "Regexp matching delimiter between method and user or host names.
Derived from `tramp-postfix-method-format'.")

(defconst tramp-user-regexp "[^/|: \t]+"
  "Regexp matching user names.")

;;;###tramp-autoload
(defconst tramp-prefix-domain-format "%"
  "String matching delimiter between user and domain names.")

;;;###tramp-autoload
(defconst tramp-prefix-domain-regexp (regexp-quote tramp-prefix-domain-format)
  "Regexp matching delimiter between user and domain names.
Derived from `tramp-prefix-domain-format'.")

(defconst tramp-domain-regexp "[a-zA-Z0-9_.-]+"
  "Regexp matching domain names.")

(defconst tramp-user-with-domain-regexp
  (concat "\\(" tramp-user-regexp "\\)"
	        tramp-prefix-domain-regexp
	  "\\(" tramp-domain-regexp "\\)")
  "Regexp matching user names with domain names.")

(defconst tramp-postfix-user-format "@"
  "String matching delimiter between user and host names.
Used in `tramp-make-tramp-file-name'.")

(defconst tramp-postfix-user-regexp (regexp-quote tramp-postfix-user-format)
  "Regexp matching delimiter between user and host names.
Derived from `tramp-postfix-user-format'.")

(defconst tramp-host-regexp "[a-zA-Z0-9_.%-]+"
  "Regexp matching host names.")

(defconst tramp-prefix-ipv6-format-alist
  '((default    . "[")
    (simplified . "[")
    (separate   . ""))
  "Alist mapping Tramp syntax to strings prefixing IPv6 addresses.")

(defun tramp-build-prefix-ipv6-format ()
  (tramp-lookup-syntax tramp-prefix-ipv6-format-alist))

(defvar tramp-prefix-ipv6-format (tramp-build-prefix-ipv6-format)
  "String matching left hand side of IPv6 addresses.
Used in `tramp-make-tramp-file-name'.")

(defun tramp-build-prefix-ipv6-regexp ()
  (regexp-quote tramp-prefix-ipv6-format))

(defvar tramp-prefix-ipv6-regexp (tramp-build-prefix-ipv6-regexp)
  "Regexp matching left hand side of IPv6 addresses.
Derived from `tramp-prefix-ipv6-format'.")

;; The following regexp is a bit sloppy.  But it shall serve our
;; purposes.  It covers also IPv4 mapped IPv6 addresses, like in
;; "::ffff:192.168.0.1".
(defconst tramp-ipv6-regexp "\\(?:\\(?:[a-zA-Z0-9]+\\)?:\\)+[a-zA-Z0-9.]+"
  "Regexp matching IPv6 addresses.")

(defconst tramp-postfix-ipv6-format-alist
  '((default    . "]")
    (simplified . "]")
    (separate   . ""))
  "Alist mapping Tramp syntax to suffix for IPv6 addresses.")

(defun tramp-build-postfix-ipv6-format ()
  (tramp-lookup-syntax tramp-postfix-ipv6-format-alist))

(defvar tramp-postfix-ipv6-format (tramp-build-postfix-ipv6-format)
  "String matching right hand side of IPv6 addresses.
Used in `tramp-make-tramp-file-name'.")

(defun tramp-build-postfix-ipv6-regexp ()
  (regexp-quote tramp-postfix-ipv6-format))

(defvar tramp-postfix-ipv6-regexp (tramp-build-postfix-ipv6-regexp)
  "Regexp matching right hand side of IPv6 addresses.
Derived from `tramp-postfix-ipv6-format'.")

(defconst tramp-prefix-port-format "#"
  "String matching delimiter between host names and port numbers.")

(defconst tramp-prefix-port-regexp (regexp-quote tramp-prefix-port-format)
  "Regexp matching delimiter between host names and port numbers.
Derived from `tramp-prefix-port-format'.")

(defconst tramp-port-regexp "[0-9]+"
  "Regexp matching port numbers.")

(defconst tramp-host-with-port-regexp
  (concat "\\(" tramp-host-regexp "\\)"
	        tramp-prefix-port-regexp
	  "\\(" tramp-port-regexp "\\)")
  "Regexp matching host names with port numbers.")

(defconst tramp-postfix-hop-format "|"
  "String matching delimiter after ad-hoc hop definitions.")

(defconst tramp-postfix-hop-regexp (regexp-quote tramp-postfix-hop-format)
  "Regexp matching delimiter after ad-hoc hop definitions.
Derived from `tramp-postfix-hop-format'.")

(defconst tramp-postfix-host-format-alist
  '((default    . ":")
    (simplified . ":")
    (separate   . "]"))
  "Alist mapping Tramp syntax to strings between host and local names.")

(defun tramp-build-postfix-host-format ()
  (tramp-lookup-syntax tramp-postfix-host-format-alist))

(defvar tramp-postfix-host-format (tramp-build-postfix-host-format)
  "String matching delimiter between host names and localnames.
Used in `tramp-make-tramp-file-name'.")

(defun tramp-build-postfix-host-regexp ()
  (regexp-quote tramp-postfix-host-format))

(defvar tramp-postfix-host-regexp (tramp-build-postfix-host-regexp)
  "Regexp matching delimiter between host names and localnames.
Derived from `tramp-postfix-host-format'.")

(defconst tramp-localname-regexp ".*$"
  "Regexp matching localnames.")

(defconst tramp-unknown-id-string "UNKNOWN"
  "String used to denote an unknown user or group")

(defconst tramp-unknown-id-integer -1
  "Integer used to denote an unknown user or group")

;;; File name format:

(defun tramp-build-remote-file-name-spec-regexp ()
  "Construct a regexp matching a Tramp file name for a Tramp syntax.
It is expected, that `tramp-syntax' has the proper value."
  (concat
           "\\("   tramp-method-regexp "\\)" tramp-postfix-method-regexp
   "\\(?:" "\\("   tramp-user-regexp   "\\)" tramp-postfix-user-regexp   "\\)?"
   "\\("   "\\(?:" tramp-host-regexp   "\\|"
	           tramp-prefix-ipv6-regexp  "\\(?:" tramp-ipv6-regexp "\\)?"
					     tramp-postfix-ipv6-regexp "\\)"
	   "\\(?:" tramp-prefix-port-regexp  tramp-port-regexp "\\)?" "\\)?"))

(defvar tramp-remote-file-name-spec-regexp
  (tramp-build-remote-file-name-spec-regexp)
  "Regular expression matching a Tramp file name between prefix and postfix.")

(defun tramp-build-file-name-structure ()
  "Construct the Tramp file name structure for a Tramp syntax.
It is expected, that `tramp-syntax' has the proper value.
See `tramp-file-name-structure'."
  (list
   (concat
    tramp-prefix-regexp
    "\\(" "\\(?:" tramp-remote-file-name-spec-regexp
                  tramp-postfix-hop-regexp "\\)+" "\\)?"
    tramp-remote-file-name-spec-regexp tramp-postfix-host-regexp
    "\\(" tramp-localname-regexp "\\)")
   5 6 7 8 1))

(defvar tramp-file-name-structure (tramp-build-file-name-structure)
  "List of six elements (REGEXP METHOD USER HOST FILE HOP), detailing \
the Tramp file name structure.

The first element REGEXP is a regular expression matching a Tramp file
name.  The regex should contain parentheses around the method name,
the user name, the host name, and the file name parts.

The second element METHOD is a number, saying which pair of
parentheses matches the method name.  The third element USER is
similar, but for the user name.  The fourth element HOST is similar,
but for the host name.  The fifth element FILE is for the file name.
The last element HOP is the ad-hoc hop definition, which could be a
cascade of several hops.

These numbers are passed directly to `match-string', which see.  That
means the opening parentheses are counted to identify the pair.

See also `tramp-file-name-regexp'.")

(defun tramp-build-file-name-regexp ()
  (car tramp-file-name-structure))

;;;###autoload
(defconst tramp-initial-file-name-regexp "\\`/.+:.*:"
  "Value for `tramp-file-name-regexp' for autoload.
It must match the initial `tramp-syntax' settings.")

;;;###autoload
(defvar tramp-file-name-regexp tramp-initial-file-name-regexp
  "Regular expression matching file names handled by Tramp.
This regexp should match Tramp file names but no other file
names.  When calling `tramp-register-file-name-handlers', the
initial value is overwritten by the car of `tramp-file-name-structure'.")

(defconst tramp-completion-file-name-regexp-default
  (concat
   "\\`/\\("
   ;; Optional multi hop.
   "\\([^/|:]+:[^/|:]*|\\)*"
   ;; Last hop.
   (if (memq system-type '(cygwin windows-nt))
       ;; The method is either "-", or at least two characters.
       "\\(-\\|[^/|:]\\{2,\\}\\)"
     ;; At least one character for method.
     "[^/|:]+")
   ;; Method separator, user name and host name.
   "\\(:[^/|:]*\\)?"
   "\\)?\\'")
  "Value for `tramp-completion-file-name-regexp' for default remoting.
See `tramp-file-name-structure' for more explanations.

On W32 systems, the volume letter must be ignored.")

(defconst tramp-completion-file-name-regexp-simplified
  (concat
   "\\`/\\("
   ;; Optional multi hop.
   "\\([^/|:]*|\\)*"
   ;; Last hop.
   (if (memq system-type '(cygwin windows-nt))
       ;; At least two characters.
       "[^/|:]\\{2,\\}"
     ;; At least one character.
     "[^/|:]+")
   "\\)?\\'")
  "Value for `tramp-completion-file-name-regexp' for simplified style remoting.
See `tramp-file-name-structure' for more explanations.

On W32 systems, the volume letter must be ignored.")

(defconst tramp-completion-file-name-regexp-separate
  "\\`/\\(\\[[^]]*\\)?\\'"
  "Value for `tramp-completion-file-name-regexp' for separate remoting.
See `tramp-file-name-structure' for more explanations.")

(defconst tramp-completion-file-name-regexp-alist
  `((default    . ,tramp-completion-file-name-regexp-default)
    (simplified . ,tramp-completion-file-name-regexp-simplified)
    (separate   . ,tramp-completion-file-name-regexp-separate))
  "Alist mapping incomplete Tramp file names.")

(defun tramp-build-completion-file-name-regexp ()
  (tramp-lookup-syntax tramp-completion-file-name-regexp-alist))

(defvar tramp-completion-file-name-regexp
  (tramp-build-completion-file-name-regexp)
  "Regular expression matching file names handled by Tramp completion.
This regexp should match partial Tramp file names only.

Please note that the entry in `file-name-handler-alist' is made when
this file \(tramp.el) is loaded.  This means that this variable must be set
before loading tramp.el.  Alternatively, `file-name-handler-alist' can be
updated after changing this variable.

Also see `tramp-file-name-structure'.")

;;;###autoload
(defconst tramp-autoload-file-name-regexp
  (concat
   "\\`/"
   (if (memq system-type '(cygwin windows-nt))
       ;; The method is either "-", or at least two characters.
       "\\(-\\|[^/|:]\\{2,\\}\\)"
     ;; At least one character for method.
     "[^/|:]+")
   ":")
  "Regular expression matching file names handled by Tramp autoload.
It must match the initial `tramp-syntax' settings.  It should not
match file names at root of the underlying local file system,
like \"/sys\" or \"/C:\".")

;; Chunked sending kludge.  We set this to 500 for black-listed constellations
;; known to have a bug in `process-send-string'; some ssh connections appear
;; to drop bytes when data is sent too quickly.  There is also a connection
;; buffer local variable, which is computed depending on remote host properties
;; when `tramp-chunksize' is zero or nil.
(defcustom tramp-chunksize (when (memq system-type '(hpux)) 500)
;; Parentheses in docstring starting at beginning of line are escaped.
;; Fontification is messed up when
;; `open-paren-in-column-0-is-defun-start' set to t.
  "If non-nil, chunksize for sending input to local process.
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
          (when (process-live-p proc)
            (process-send-string proc (make-string sent ?\\ ))
            (process-send-eof proc)
            (process-send-eof proc))
          (while (not (progn (goto-char (point-min))
                             (re-search-forward \"\\\\w+\" (point-max) t)))
            (accept-process-output proc 1))
          (when (process-live-p proc)
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
use an out-of-the-band method \(like \"scp\") instead of an internal one
\(like \"ssh\"), because setting `tramp-chunksize' to non-nil decreases
performance.

If your Emacs is buggy, the code stops and gives you an indication
about the value `tramp-chunksize' should be set.  Maybe you could just
experiment a bit, e.g. changing the values of `init' and `step'
in the third line of the code.

Please raise a bug report via \"M-x tramp-bug\" if your system needs
this variable to be set as well."
  :group 'tramp
  :type '(choice (const nil) integer)
  :require 'tramp)

;; Logging in to a remote host normally requires obtaining a pty.  But
;; Emacs on macOS has process-connection-type set to nil by default,
;; so on those systems Tramp doesn't obtain a pty.  Here, we allow
;; for an override of the system default.
(defcustom tramp-process-connection-type t
  "Overrides `process-connection-type' for connections from Tramp.
Tramp binds `process-connection-type' to the value given here before
opening a connection to a remote host."
  :group 'tramp
  :type '(choice (const nil) (const t) (const pty))
  :require 'tramp)

(defcustom tramp-connection-timeout 60
  "Defines the max time to wait for establishing a connection (in seconds).
This can be overwritten for different connection types in `tramp-methods'.

The timeout does not include the time reading a password."
  :group 'tramp
  :version "24.4"
  :type 'integer
  :require 'tramp)

(defcustom tramp-connection-min-time-diff 5
  "Defines seconds between two consecutive connection attempts.
This is necessary as self defense mechanism, in order to avoid
yo-yo connection attempts when the remote host is unavailable.

A value of 0 or nil suppresses this check.  This might be
necessary, when several out-of-order copy operations are
performed, or when several asynchronous processes will be started
in a short time frame.  In those cases it is recommended to
let-bind this variable."
  :group 'tramp
  :version "24.4"
  :type '(choice (const nil) integer)
  :require 'tramp)

(defcustom tramp-completion-reread-directory-timeout 10
  "Defines seconds since last remote command before rereading a directory.
A remote directory might have changed its contents.  In order to
make it visible during file name completion in the minibuffer,
Tramp flushes its cache and rereads the directory contents when
more than `tramp-completion-reread-directory-timeout' seconds
have been gone since last remote command execution.  A value of t
would require an immediate reread during filename completion, nil
means to use always cached values for the directory contents."
  :group 'tramp
  :type '(choice (const nil) (const t) integer)
  :require 'tramp)

;;; Internal Variables:

(defvar tramp-current-connection nil
  "Last connection timestamp.")

(defconst tramp-completion-file-name-handler-alist
  '((file-name-all-completions
     . tramp-completion-handle-file-name-all-completions)
    (file-name-completion . tramp-completion-handle-file-name-completion))
  "Alist of completion handler functions.
Used for file names matching `tramp-completion-file-name-regexp'.
Operations not mentioned here will be handled by Tramp's file
name handler functions, or the normal Emacs functions.")

;; Handlers for foreign methods, like FTP or SMB, shall be plugged here.
;;;###tramp-autoload
(defvar tramp-foreign-file-name-handler-alist nil
  "Alist of elements (FUNCTION . HANDLER) for foreign methods handled specially.
If (FUNCTION FILENAME) returns non-nil, then all I/O on that file is done by
calling HANDLER.")

;;; Internal functions which must come first:

;; Conversion functions between external representation and
;; internal data structure.  Convenience functions for internal
;; data structure.

;; The basic structure for remote file names.  We use a list :type,
;; in order to be compatible with Emacs 24 and 25.
(cl-defstruct (tramp-file-name (:type list) :named)
  method user domain host port localname hop)

(defun tramp-file-name-user-domain (vec)
  "Return user and domain components of VEC."
  (when (or (tramp-file-name-user vec) (tramp-file-name-domain vec))
    (concat (tramp-file-name-user vec)
	    (and (tramp-file-name-domain vec)
		 tramp-prefix-domain-format)
	    (tramp-file-name-domain vec))))

(defun tramp-file-name-host-port (vec)
  "Return host and port components of VEC."
  (when (or (tramp-file-name-host vec) (tramp-file-name-port vec))
    (concat (tramp-file-name-host vec)
	    (and (tramp-file-name-port vec)
		 tramp-prefix-port-format)
	    (tramp-file-name-port vec))))

(defun tramp-file-name-port-or-default (vec)
  "Return port component of VEC.
If nil, return `tramp-default-port'."
  (or (tramp-file-name-port vec)
      (tramp-get-method-parameter vec 'tramp-default-port)))

(defun tramp-file-name-equal-p (vec1 vec2)
  "Check, whether VEC1 and VEC2 denote the same `tramp-file-name'."
  (and (tramp-file-name-p vec1) (tramp-file-name-p vec2)
       (string-equal (tramp-file-name-method vec1)
		     (tramp-file-name-method vec2))
       (string-equal (tramp-file-name-user-domain vec1)
		     (tramp-file-name-user-domain vec2))
       (string-equal (tramp-file-name-host-port vec1)
		     (tramp-file-name-host-port vec2))))

(defun tramp-get-method-parameter (vec param)
  "Return the method parameter PARAM.
If VEC is a vector, check first in connection properties.
Afterwards, check in `tramp-methods'.  If the `tramp-methods'
entry does not exist, return nil."
  (let ((hash-entry
	 (replace-regexp-in-string "^tramp-" "" (symbol-name param))))
    (if (tramp-connection-property-p vec hash-entry)
	;; We use the cached property.
	(tramp-get-connection-property vec hash-entry nil)
      ;; Use the static value from `tramp-methods'.
      (let ((methods-entry
	     (assoc param (assoc (tramp-file-name-method vec) tramp-methods))))
	(when methods-entry (cadr methods-entry))))))

;; The localname can be quoted with "/:".  Extract this.
(defun tramp-file-name-unquote-localname (vec)
  "Return unquoted localname component of VEC."
  (tramp-compat-file-name-unquote (tramp-file-name-localname vec)))

;;;###tramp-autoload
(defun tramp-tramp-file-p (name)
  "Return t if NAME is a string with Tramp file name syntax."
  (and (stringp name)
       ;; No "/:" and "/c:".  This is not covered by `tramp-file-name-regexp'.
       (not (string-match-p
	     (if (memq system-type '(cygwin windows-nt))
		 "^/[[:alpha:]]?:" "^/:")
	     name))
       (string-match-p tramp-file-name-regexp name)
       t))

(defun tramp-find-method (method user host)
  "Return the right method string to use.
This is METHOD, if non-nil. Otherwise, do a lookup in
`tramp-default-method-alist'."
  (when (and method
	     (or (string-equal method "")
		 (string-equal method tramp-default-method-marker)))
    (setq method nil))
  (let ((result
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
	     tramp-default-method)))
    ;; We must mark, whether a default value has been used.
    (if (or method (null result))
	result
      (propertize result 'tramp-default t))))

(defun tramp-find-user (method user host)
  "Return the right user string to use.
This is USER, if non-nil. Otherwise, do a lookup in
`tramp-default-user-alist'."
  (let ((result
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
	     tramp-default-user)))
    ;; We must mark, whether a default value has been used.
    (if (or user (null result))
	result
      (propertize result 'tramp-default t))))

(defun tramp-find-host (method user host)
  "Return the right host string to use.
This is HOST, if non-nil. Otherwise, it is `tramp-default-host'."
  (or (and (> (length host) 0) host)
      (let ((choices tramp-default-host-alist)
	    lhost item)
	(while choices
	  (setq item (pop choices))
	  (when (and (string-match (or (nth 0 item) "") (or method ""))
		     (string-match (or (nth 1 item) "") (or user "")))
	    (setq lhost (nth 2 item))
	    (setq choices nil)))
	lhost)
      tramp-default-host))

(defun tramp-dissect-file-name (name &optional nodefault)
  "Return a `tramp-file-name' structure.
The structure consists of remote method, remote user, remote host,
localname (file name on remote host) and hop.  If NODEFAULT is
non-nil, the file name parts are not expanded to their default
values."
  (save-match-data
    (unless (tramp-tramp-file-p name)
      (tramp-compat-user-error nil "Not a Tramp file name: \"%s\"" name))
    (if (not (string-match (nth 0 tramp-file-name-structure) name))
        (error "`tramp-file-name-structure' didn't match!")
      (let ((method    (match-string (nth 1 tramp-file-name-structure) name))
	    (user      (match-string (nth 2 tramp-file-name-structure) name))
	    (host      (match-string (nth 3 tramp-file-name-structure) name))
	    (localname (match-string (nth 4 tramp-file-name-structure) name))
	    (hop       (match-string (nth 5 tramp-file-name-structure) name))
	    domain port)
	(when user
	  (when (string-match tramp-user-with-domain-regexp user)
	    (setq domain (match-string 2 user)
		  user (match-string 1 user))))

	(when host
	  (when (string-match tramp-host-with-port-regexp host)
	    (setq port (match-string 2 host)
		  host (match-string 1 host)))
	  (when (string-match tramp-prefix-ipv6-regexp host)
	    (setq host (replace-match "" nil t host)))
	  (when (string-match tramp-postfix-ipv6-regexp host)
	    (setq host (replace-match "" nil t host))))

	(unless nodefault
	  (setq method (tramp-find-method method user host)
		user (tramp-find-user method user host)
		host (tramp-find-host method user host)))

	(make-tramp-file-name
	 :method method :user user :domain domain :host host :port port
	 :localname localname :hop hop)))))

(defun tramp-buffer-name (vec)
  "A name for the connection buffer VEC."
  (let ((method (tramp-file-name-method vec))
	(user-domain (tramp-file-name-user-domain vec))
	(host-port (tramp-file-name-host-port vec)))
    (if (not (zerop (length user-domain)))
	(format "*tramp/%s %s@%s*" method user-domain host-port)
      (format "*tramp/%s %s*" method host-port))))

(defun tramp-make-tramp-file-name (&rest args)
  "Construct a Tramp file name from ARGS.

ARGS could have two different signatures.  The first one is of
type (VEC &optional LOCALNAME HOP).
If LOCALNAME is nil, the value in VEC is used.  If it is a
symbol, a null localname will be used.  Otherwise, LOCALNAME is
expected to be a string, which will be used.
If HOP is nil, the value in VEC is used.  If it is a symbol, a
null hop will be used.  Otherwise, HOP is expected to be a
string, which will be used.

The other signature exists for backward compatibility.  It has
the form (METHOD USER DOMAIN HOST PORT LOCALNAME &optional HOP)."
  (let (method user domain host port localname hop)
    (cond
     ((tramp-file-name-p (car args))
      (setq method (tramp-file-name-method (car args))
	    user (tramp-file-name-user (car args))
	    domain (tramp-file-name-domain (car args))
	    host (tramp-file-name-host (car args))
	    port (tramp-file-name-port (car args))
	    localname (tramp-file-name-localname (car args))
	    hop (tramp-file-name-hop (car args)))
      (when (cadr args)
	(setq localname (and (stringp (cadr args)) (cadr args))))
      (when (cl-caddr args)
	(setq hop (and (stringp (cl-caddr args)) (cl-caddr args)))))

     (t (setq method (nth 0 args)
	      user (nth 1 args)
	      domain (nth 2 args)
	      host (nth 3 args)
	      port (nth 4 args)
	      localname (nth 5 args)
	      hop (nth 6 args))))

    (when (zerop (length method))
      (signal 'wrong-type-argument (list 'stringp method)))
    (concat tramp-prefix-format hop
	    (unless (zerop (length tramp-postfix-method-format))
	      (concat method tramp-postfix-method-format))
	    user
	    (unless (zerop (length domain))
	      (concat tramp-prefix-domain-format domain))
	    (unless (zerop (length user))
	      tramp-postfix-user-format)
	    (when host
	      (if (string-match tramp-ipv6-regexp host)
		  (concat
		   tramp-prefix-ipv6-format host tramp-postfix-ipv6-format)
		host))
	    (unless (zerop (length port))
	      (concat tramp-prefix-port-format port))
	    tramp-postfix-host-format
	    localname)))

(defun tramp-completion-make-tramp-file-name (method user host localname)
  "Construct a Tramp file name from METHOD, USER, HOST and LOCALNAME.
It must not be a complete Tramp file name, but as long as there are
necessary only.  This function will be used in file name completion."
  (concat tramp-prefix-format
	  (unless (or (zerop (length method))
                      (zerop (length tramp-postfix-method-format)))
            (concat method tramp-postfix-method-format))
          (unless (zerop (length user))
	    (concat user tramp-postfix-user-format))
	  (unless (zerop (length host))
	    (concat
	     (if (string-match tramp-ipv6-regexp host)
		 (concat
		  tramp-prefix-ipv6-format host tramp-postfix-ipv6-format)
	       host)
	     tramp-postfix-host-format))
	  (when localname localname)))

(defun tramp-get-buffer (vec)
  "Get the connection buffer to be used for VEC."
  (or (get-buffer (tramp-buffer-name vec))
      (with-current-buffer (get-buffer-create (tramp-buffer-name vec))
	;; We use the existence of connection property "process-buffer"
	;; as indication, whether a connection is active.
	(tramp-set-connection-property
	 vec "process-buffer"
	 (tramp-get-connection-property vec "process-buffer" nil))
	(setq buffer-undo-list t
	      default-directory (tramp-make-tramp-file-name vec "/" 'nohop))
	(current-buffer))))

(defun tramp-get-connection-buffer (vec)
  "Get the connection buffer to be used for VEC.
In case a second asynchronous communication has been started, it is different
from `tramp-get-buffer'."
  (or (tramp-get-connection-property vec "process-buffer" nil)
      (tramp-get-buffer vec)))

(defun tramp-get-connection-name (vec)
  "Get the connection name to be used for VEC.
In case a second asynchronous communication has been started, it is different
from the default one."
  (or (tramp-get-connection-property vec "process-name" nil)
      (tramp-buffer-name vec)))

(defun tramp-get-connection-process (vec)
  "Get the connection process to be used for VEC.
In case a second asynchronous communication has been started, it is different
from the default one."
  (and (tramp-file-name-p vec) (get-process (tramp-get-connection-name vec))))

(defun tramp-set-connection-local-variables (vec)
  "Set connection-local variables in the connection buffer used for VEC.
If connection-local variables are not supported by this Emacs
version, the function does nothing."
  (with-current-buffer (tramp-get-connection-buffer vec)
    ;; `hack-connection-local-variables-apply' exists since Emacs 26.1.
    (tramp-compat-funcall
     'hack-connection-local-variables-apply
     `(:application tramp
       :protocol    ,(tramp-file-name-method vec)
       :user        ,(tramp-file-name-user-domain vec)
       :machine     ,(tramp-file-name-host-port vec)))))

(defun tramp-set-connection-local-variables-for-buffer ()
  "Set connection-local variables in the current buffer.
If connection-local variables are not supported by this Emacs
version, the function does nothing."
  (when (file-remote-p default-directory)
    ;; `hack-connection-local-variables-apply' exists since Emacs 26.1.
    (tramp-compat-funcall
     'hack-connection-local-variables-apply
     `(:application tramp
       :protocol    ,(file-remote-p default-directory 'method)
       :user        ,(file-remote-p default-directory 'user)
       :machine     ,(file-remote-p default-directory 'host)))))

(defun tramp-debug-buffer-name (vec)
  "A name for the debug buffer for VEC."
  (let ((method (tramp-file-name-method vec))
	(user-domain (tramp-file-name-user-domain vec))
	(host-port (tramp-file-name-host-port vec)))
    (if (not (zerop (length user-domain)))
	(format "*debug tramp/%s %s@%s*" method user-domain host-port)
      (format "*debug tramp/%s %s*" method host-port))))

(defconst tramp-debug-outline-regexp
  "[0-9]+:[0-9]+:[0-9]+\\.[0-9]+ [a-z0-9-]+ (\\([0-9]+\\)) #"
  "Used for highlighting Tramp debug buffers in `outline-mode'.")

(defun tramp-debug-outline-level ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line.

The outline level is equal to the verbosity of the Tramp message."
  (1+ (string-to-number (match-string 1))))

(defun tramp-get-debug-buffer (vec)
  "Get the debug buffer for VEC."
  (with-current-buffer
      (get-buffer-create (tramp-debug-buffer-name vec))
    (when (bobp)
      (setq buffer-undo-list t)
      ;; So it does not get loaded while `outline-regexp' is let-bound.
      (require 'outline)
      ;; Activate `outline-mode'.  This runs `text-mode-hook' and
      ;; `outline-mode-hook'.  We must prevent that local processes
      ;; die.  Yes: I've seen `flyspell-mode', which starts "ispell".
      ;; Furthermore, `outline-regexp' must have the correct value
      ;; already, because it is used by `font-lock-compile-keywords'.
      (let ((default-directory (tramp-compat-temporary-file-directory))
	    (outline-regexp tramp-debug-outline-regexp))
	(outline-mode))
      (set (make-local-variable 'outline-regexp) tramp-debug-outline-regexp)
      (set (make-local-variable 'outline-level) 'tramp-debug-outline-level)
      ;; Do not edit the debug buffer.
      (set-keymap-parent (current-local-map) special-mode-map))
    (current-buffer)))

(defsubst tramp-debug-message (vec fmt-string &rest arguments)
  "Append message to debug buffer.
Message is formatted with FMT-STRING as control string and the remaining
ARGUMENTS to actually emit the message (if applicable)."
  (with-current-buffer (tramp-get-debug-buffer vec)
    (goto-char (point-max))
    ;; Headline.
    (when (bobp)
      (insert
       (format
	";; Emacs: %s Tramp: %s -*- mode: outline; -*-"
	emacs-version tramp-version))
      (when (>= tramp-verbose 10)
	(insert
	 (format
	  "\n;; Location: %s Git: %s"
	  (locate-library "tramp") (tramp-repository-get-version)))))
    (unless (bolp)
      (insert "\n"))
    ;; Timestamp.
    (let ((now (current-time)))
      (insert (format-time-string "%T." now))
      (insert (format "%06d " (nth 2 now))))
    ;; Calling Tramp function.  We suppress compat and trace functions
    ;; from being displayed.
    (let ((btn 1) btf fn)
      (while (not fn)
	(setq btf (nth 1 (backtrace-frame btn)))
	(if (not btf)
	    (setq fn "")
	  (when (symbolp btf)
	    (setq fn (symbol-name btf))
	    (unless
		(and
		 (string-match "^tramp" fn)
		 (not
		  (string-match
		   (concat
		    "^"
		    (regexp-opt
		     '("tramp-backtrace"
		       "tramp-compat-funcall"
		       "tramp-compat-user-error"
		       "tramp-condition-case-unless-debug"
		       "tramp-debug-message"
		       "tramp-error"
		       "tramp-error-with-buffer"
		       "tramp-message")
		     t)
		    "$")
		   fn)))
	      (setq fn nil)))
	  (setq btn (1+ btn))))
      ;; The following code inserts filename and line number.  Should
      ;; be inactive by default, because it is time consuming.
;      (let ((ffn (find-function-noselect (intern fn))))
;	(insert
;	 (format
;	  "%s:%d: "
;	  (file-name-nondirectory (buffer-file-name (car ffn)))
;	  (with-current-buffer (car ffn)
;	    (1+ (count-lines (point-min) (cdr ffn)))))))
      (insert (format "%s " fn)))
    ;; The message.
    (insert (apply #'format-message fmt-string arguments))))

(defvar tramp-message-show-message (null noninteractive)
  "Show Tramp message in the minibuffer.
This variable is used to suppress progress reporter output, and
to disable messages from `tramp-error'.  Those messages are
visible anyway, because an error is raised.")

(defsubst tramp-message (vec-or-proc level fmt-string &rest arguments)
  "Emit a message depending on verbosity level.
VEC-OR-PROC identifies the Tramp buffer to use.  It can be either a
vector or a process.  LEVEL says to be quiet if `tramp-verbose' is
less than LEVEL.  The message is emitted only if `tramp-verbose' is
greater than or equal to LEVEL.

The message is also logged into the debug buffer when `tramp-verbose'
is greater than or equal 4.

Calls functions `message' and `tramp-debug-message' with FMT-STRING as
control string and the remaining ARGUMENTS to actually emit the message (if
applicable)."
  (ignore-errors
    (when (<= level tramp-verbose)
      ;; Match data must be preserved!
      (save-match-data
	;; Display only when there is a minimum level.
	(when (and tramp-message-show-message (<= level 3))
	  (apply 'message
		 (concat
		  (cond
		   ((= level 0) "")
		   ((= level 1) "")
		   ((= level 2) "Warning: ")
		   (t           "Tramp: "))
		  fmt-string)
		 arguments))
	;; Log only when there is a minimum level.
	(when (>= tramp-verbose 4)
	  (let ((tramp-verbose 0))
	    ;; Append connection buffer for error messages.
	    (when (= level 1)
	      (with-current-buffer
		  (if (processp vec-or-proc)
		      (process-buffer vec-or-proc)
		    (tramp-get-connection-buffer vec-or-proc))
		(setq fmt-string (concat fmt-string "\n%s")
		      arguments (append arguments (list (buffer-string))))))
	    ;; Translate proc to vec.
	    (when (processp vec-or-proc)
	      (setq vec-or-proc (process-get vec-or-proc 'vector))))
	  ;; Do it.
	  (when (tramp-file-name-p vec-or-proc)
	    (apply 'tramp-debug-message
		   vec-or-proc
		   (concat (format "(%d) # " level) fmt-string)
		   arguments)))))))

(defsubst tramp-backtrace (&optional vec-or-proc)
  "Dump a backtrace into the debug buffer.
If VEC-OR-PROC is nil, the buffer *debug tramp* is used.  This
function is meant for debugging purposes."
  (if vec-or-proc
      (tramp-message vec-or-proc 10 "\n%s" (with-output-to-string (backtrace)))
    (if (>= tramp-verbose 10)
	(with-output-to-temp-buffer "*debug tramp*" (backtrace)))))

(defsubst tramp-error (vec-or-proc signal fmt-string &rest arguments)
  "Emit an error.
VEC-OR-PROC identifies the connection to use, SIGNAL is the
signal identifier to be raised, remaining arguments passed to
`tramp-message'.  Finally, signal SIGNAL is raised."
  (let (tramp-message-show-message)
    (tramp-backtrace vec-or-proc)
    (unless arguments
      ;; FMT-STRING could be just a file name, as in
      ;; `file-already-exists' errors.  It could contain the ?\%
      ;; character, as in smb domain spec.
      (setq arguments (list fmt-string)
	    fmt-string "%s"))
    (when vec-or-proc
      (tramp-message
       vec-or-proc 1 "%s"
       (error-message-string
	(list signal
	      (get signal 'error-message)
	      (apply #'format-message fmt-string arguments)))))
    (signal signal (list (apply #'format-message fmt-string arguments)))))

(defsubst tramp-error-with-buffer
  (buf vec-or-proc signal fmt-string &rest arguments)
  "Emit an error, and show BUF.
If BUF is nil, show the connection buf.  Wait for 30\", or until
an input event arrives.  The other arguments are passed to `tramp-error'."
  (save-window-excursion
    (let* ((buf (or (and (bufferp buf) buf)
		    (and (processp vec-or-proc) (process-buffer vec-or-proc))
		    (and (tramp-file-name-p vec-or-proc)
			 (tramp-get-connection-buffer vec-or-proc))))
	   (vec (or (and (tramp-file-name-p vec-or-proc) vec-or-proc)
		    (and buf (with-current-buffer buf
			       (tramp-dissect-file-name default-directory))))))
      (unwind-protect
	  (apply 'tramp-error vec-or-proc signal fmt-string arguments)
	;; Save exit.
	(when (and buf
		   tramp-message-show-message
		   (not (zerop tramp-verbose))
		   ;; Do not show when flagged from outside.
		   (not (tramp-completion-mode-p))
		   ;; Show only when Emacs has started already.
		   (current-message))
	  (let ((enable-recursive-minibuffers t))
	    ;; `tramp-error' does not show messages.  So we must do it
	    ;; ourselves.
	    (apply 'message fmt-string arguments)
	    ;; Show buffer.
	    (pop-to-buffer buf)
	    (discard-input)
	    (sit-for 30)))
	;; Reset timestamp.  It would be wrong after waiting for a while.
	(when (tramp-file-name-equal-p vec (car tramp-current-connection))
	  (setcdr tramp-current-connection (current-time)))))))

(defmacro tramp-with-demoted-errors (vec-or-proc format &rest body)
  "Execute BODY while redirecting the error message to `tramp-message'.
BODY is executed like wrapped by `with-demoted-errors'.  FORMAT
is a format-string containing a %-sequence meaning to substitute
the resulting error message."
  (declare (debug (symbolp body))
           (indent 2))
  (let ((err (make-symbol "err")))
    `(condition-case-unless-debug ,err
         (progn ,@body)
       (error (tramp-message ,vec-or-proc 3 ,format ,err) nil))))

(defmacro with-parsed-tramp-file-name (filename var &rest body)
  "Parse a Tramp filename and make components available in the body.

First arg FILENAME is evaluated and dissected into its components.
Second arg VAR is a symbol.  It is used as a variable name to hold
the filename structure.  It is also used as a prefix for the variables
holding the components.  For example, if VAR is the symbol `foo', then
`foo' will be bound to the whole structure, `foo-method' will be bound to
the method component, and so on for `foo-user', `foo-domain', `foo-host',
`foo-port', `foo-localname', `foo-hop'.

Remaining args are Lisp expressions to be evaluated (inside an implicit
`progn').

If VAR is nil, then we bind `v' to the structure and `method', `user',
`domain', `host', `port', `localname', `hop' to the components."
  (let ((bindings
         (mapcar (lambda (elem)
                   `(,(if var (intern (format "%s-%s" var elem)) elem)
                     (,(intern (format "tramp-file-name-%s" elem))
                      ,(or var 'v))))
		 `,(tramp-compat-tramp-file-name-slots))))
    `(let* ((,(or var 'v) (tramp-dissect-file-name ,filename))
            ,@bindings)
       ;; We don't know which of those vars will be used, so we bind them all,
       ;; and then add here a dummy use of all those variables, so we don't get
       ;; flooded by warnings about those vars `body' didn't use.
       (ignore ,@(mapcar #'car bindings))
       ,@body)))

(put 'with-parsed-tramp-file-name 'lisp-indent-function 2)
(put 'with-parsed-tramp-file-name 'edebug-form-spec '(form symbolp body))
(font-lock-add-keywords 'emacs-lisp-mode '("\\<with-parsed-tramp-file-name\\>"))

(defun tramp-progress-reporter-update (reporter &optional value)
  "Report progress of an operation for Tramp."
  (let* ((parameters (cdr reporter))
	 (message (aref parameters 3)))
    (when (string-match message (or (current-message) ""))
      (progress-reporter-update reporter value))))

(defmacro with-tramp-progress-reporter (vec level message &rest body)
  "Executes BODY, spinning a progress reporter with MESSAGE.
If LEVEL does not fit for visible messages, there are only traces
without a visible progress reporter."
  (declare (indent 3) (debug t))
  `(progn
     (tramp-message ,vec ,level "%s..." ,message)
     (let ((cookie "failed")
           (tm
            ;; We start a pulsing progress reporter after 3 seconds.
            (when (and tramp-message-show-message
                       ;; Display only when there is a minimum level.
                       (<= ,level (min tramp-verbose 3)))
	      (let ((pr (make-progress-reporter ,message nil nil)))
		(when pr
		  (run-at-time
		   3 0.1 #'tramp-progress-reporter-update pr))))))
       (unwind-protect
           ;; Execute the body.
           (prog1 (progn ,@body) (setq cookie "done"))
         ;; Stop progress reporter.
         (if tm (cancel-timer tm))
         (tramp-message ,vec ,level "%s...%s" ,message cookie)))))

(font-lock-add-keywords
 'emacs-lisp-mode '("\\<with-tramp-progress-reporter\\>"))

(defmacro with-tramp-file-property (vec file property &rest body)
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

(put 'with-tramp-file-property 'lisp-indent-function 3)
(put 'with-tramp-file-property 'edebug-form-spec t)
(font-lock-add-keywords 'emacs-lisp-mode '("\\<with-tramp-file-property\\>"))

(defmacro with-tramp-connection-property (key property &rest body)
  "Check in Tramp for property PROPERTY, otherwise executes BODY and set."
  `(let ((value (tramp-get-connection-property ,key ,property 'undef)))
    (when (eq value 'undef)
      ;; We cannot pass ,@body as parameter to
      ;; `tramp-set-connection-property' because it mangles our debug
      ;; messages.
      (setq value (progn ,@body))
      (tramp-set-connection-property ,key ,property value))
    value))

(put 'with-tramp-connection-property 'lisp-indent-function 2)
(put 'with-tramp-connection-property 'edebug-form-spec t)
(font-lock-add-keywords
 'emacs-lisp-mode '("\\<with-tramp-connection-property\\>"))

(defun tramp-drop-volume-letter (name)
  "Cut off unnecessary drive letter from file NAME.
The functions `tramp-*-handle-expand-file-name' call `expand-file-name'
locally on a remote file name.  When the local system is a W32 system
but the remote system is Unix, this introduces a superfluous drive
letter into the file name.  This function removes it."
  (save-match-data
    (funcall
     (if (tramp-compat-file-name-quoted-p name)
	 'tramp-compat-file-name-quote 'identity)
     (let ((name (tramp-compat-file-name-unquote name)))
       (if (string-match "\\`[a-zA-Z]:/" name)
	   (replace-match "/" nil t name)
	 name)))))

;;; Config Manipulation Functions:

;;;###tramp-autoload
(defun tramp-set-completion-function (method function-list)
  "Sets the list of completion functions for METHOD.
FUNCTION-LIST is a list of entries of the form (FUNCTION FILE).
The FUNCTION is intended to parse FILE according its syntax.
It might be a predefined FUNCTION, or a user defined FUNCTION.
For the list of predefined FUNCTIONs see `tramp-completion-function-alist'.

Example:

    (tramp-set-completion-function
     \"ssh\"
     \\='((tramp-parse-sconfig \"/etc/ssh_config\")
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
		   (cond
		    ;; Windows registry.
		    ((string-match "^HKEY_CURRENT_USER" (nth 1 (car v)))
		     (and (memq system-type '(cygwin windows-nt))
			  (zerop
			   (tramp-call-process
			    v "reg" nil nil nil "query" (nth 1 (car v))))))
		    ;; Zeroconf service type.
		    ((string-match
		      "^_[[:alpha:]]+\\._[[:alpha:]]+$" (nth 1 (car v))))
		    ;; Configuration file.
		    (t (file-exists-p (nth 1 (car v))))))
	(setq r (delete (car v) r)))
      (setq v (cdr v)))

    (when r
      (add-to-list 'tramp-completion-function-alist
		   (cons method r)))))

(defun tramp-get-completion-function (method)
  "Returns a list of completion functions for METHOD.
For definition of that list see `tramp-set-completion-function'."
  (append
   `(;; Default settings are taken into account.
     (tramp-parse-default-user-host ,method)
     ;; Hosts visited once shall be remembered.
     (tramp-parse-connection-properties ,method))
   ;; The method related defaults.
   (cdr (assoc method tramp-completion-function-alist))))


;;; Fontification of `read-file-name':

(defvar tramp-rfn-eshadow-overlay)
(make-variable-buffer-local 'tramp-rfn-eshadow-overlay)

(defun tramp-rfn-eshadow-setup-minibuffer ()
  "Set up a minibuffer for `file-name-shadow-mode'.
Adds another overlay hiding filename parts according to Tramp's
special handling of `substitute-in-file-name'."
  (when (symbol-value 'minibuffer-completing-file-name)
    (setq tramp-rfn-eshadow-overlay
	  (make-overlay (minibuffer-prompt-end) (minibuffer-prompt-end)))
    ;; Copy rfn-eshadow-overlay properties.
    (let ((props (overlay-properties (symbol-value 'rfn-eshadow-overlay))))
      (while props
 	;; The `field' property prevents correct minibuffer
 	;; completion; we exclude it.
 	(if (not (eq (car props) 'field))
            (overlay-put tramp-rfn-eshadow-overlay (pop props) (pop props))
 	  (pop props) (pop props))))))

(add-hook 'rfn-eshadow-setup-minibuffer-hook
	  'tramp-rfn-eshadow-setup-minibuffer)
(add-hook 'tramp-unload-hook
	  (lambda ()
	    (remove-hook 'rfn-eshadow-setup-minibuffer-hook
			 'tramp-rfn-eshadow-setup-minibuffer)))

(defun tramp-rfn-eshadow-update-overlay-regexp ()
  (format "[^%s/~]*\\(/\\|~\\)" tramp-postfix-host-format))

(defun tramp-rfn-eshadow-update-overlay ()
  "Update `rfn-eshadow-overlay' to cover shadowed part of minibuffer input.
This is intended to be used as a minibuffer `post-command-hook' for
`file-name-shadow-mode'; the minibuffer should have already
been set up by `rfn-eshadow-setup-minibuffer'."
  ;; In remote files name, there is a shadowing just for the local part.
  (ignore-errors
    (let ((end (or (overlay-end (symbol-value 'rfn-eshadow-overlay))
		   (minibuffer-prompt-end)))
	  ;; We do not want to send any remote command.
	  (non-essential t))
      (when
	  (tramp-tramp-file-p
	   (buffer-substring-no-properties end (point-max)))
	(save-excursion
	  (save-restriction
	    (narrow-to-region
	     (1+ (or (string-match
		      (tramp-rfn-eshadow-update-overlay-regexp)
		      (buffer-string) end)
		     end))
	     (point-max))
	    (let ((rfn-eshadow-overlay tramp-rfn-eshadow-overlay)
		  (rfn-eshadow-update-overlay-hook nil)
		  file-name-handler-alist)
	      (move-overlay rfn-eshadow-overlay (point-max) (point-max))
	      (rfn-eshadow-update-overlay))))))))

(add-hook 'rfn-eshadow-update-overlay-hook
	  'tramp-rfn-eshadow-update-overlay)
(add-hook 'tramp-unload-hook
	  (lambda ()
	    (remove-hook 'rfn-eshadow-update-overlay-hook
			 'tramp-rfn-eshadow-update-overlay)))

;; Inodes don't exist for some file systems.  Therefore we must
;; generate virtual ones.  Used in `find-buffer-visiting'.  The method
;; applied might be not so efficient (Ange-FTP uses hashes). But
;; performance isn't the major issue given that file transfer will
;; take time.
(defvar tramp-inodes 0
  "Keeps virtual inodes numbers.")

;; Devices must distinguish physical file systems.  The device numbers
;; provided by "lstat" aren't unique, because we operate on different hosts.
;; So we use virtual device numbers, generated by Tramp.  Both Ange-FTP and
;; EFS use device number "-1".  In order to be different, we use device number
;; (-1 . x), whereby "x" is unique for a given (method user host).
(defvar tramp-devices 0
  "Keeps virtual device numbers.")

(defun tramp-default-file-modes (filename)
  "Return file modes of FILENAME as integer.
If the file modes of FILENAME cannot be determined, return the
value of `default-file-modes', without execute permissions."
  (or (file-modes filename)
      (logand (default-file-modes) (string-to-number "0666" 8))))

(defun tramp-replace-environment-variables (filename)
 "Replace environment variables in FILENAME.
Return the string with the replaced variables."
 (or (ignore-errors
       ;; Optional arg has been introduced with Emacs 24.4.
       (tramp-compat-funcall 'substitute-env-vars filename 'only-defined))
     ;; We need an own implementation.
     (save-match-data
       (let ((idx (string-match "$\\(\\w+\\)" filename)))
	 ;; `$' is coded as `$$'.
	 (when (and idx
		    (or (zerop idx) (not (eq ?$ (aref filename (1- idx)))))
		    (getenv (match-string 1 filename)))
	   (setq filename
		 (replace-match
		  (substitute-in-file-name (match-string 0 filename))
		  t nil filename)))
	 filename))))

(defun tramp-find-file-name-coding-system-alist (filename tmpname)
  "Like `find-operation-coding-system' for Tramp filenames.
Tramp's `insert-file-contents' and `write-region' work over
temporary file names.  If `file-coding-system-alist' contains an
expression, which matches more than the file name suffix, the
coding system might not be determined.  This function repairs it."
  (let (result)
    (dolist (elt file-coding-system-alist (nreverse result))
      (when (and (consp elt) (string-match (car elt) filename))
	;; We found a matching entry in `file-coding-system-alist'.
	;; So we add a similar entry, but with the temporary file name
	;; as regexp.
	(push (cons (regexp-quote tmpname) (cdr elt)) result)))))

(defun tramp-run-real-handler (operation args)
  "Invoke normal file name handler for OPERATION.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION."
  (let* ((inhibit-file-name-handlers
	  `(tramp-file-name-handler
	    tramp-vc-file-name-handler
	    tramp-completion-file-name-handler
	    tramp-archive-file-name-handler
	    cygwin-mount-name-hook-function
	    cygwin-mount-map-drive-hook-function
	    .
	    ,(and (eq inhibit-file-name-operation operation)
		  inhibit-file-name-handlers)))
	 (inhibit-file-name-operation operation))
    (apply operation args)))

;; We handle here all file primitives.  Most of them have the file
;; name as first parameter; nevertheless we check for them explicitly
;; in order to be signaled if a new primitive appears.  This
;; scenario is needed because there isn't a way to decide by
;; syntactical means whether a foreign method must be called.  It would
;; ease the life if `file-name-handler-alist' would support a decision
;; function as well but regexp only.
(defun tramp-file-name-for-operation (operation &rest args)
  "Return file name related to OPERATION file primitive.
ARGS are the arguments OPERATION has been called with."
  (cond
   ;; FILE resp DIRECTORY.
   ((member operation
	    '(access-file byte-compiler-base-file-name delete-directory
	      delete-file diff-latest-backup-file directory-file-name
	      directory-files directory-files-and-attributes
	      dired-compress-file dired-uncache file-acl
	      file-accessible-directory-p file-attributes
	      file-directory-p file-executable-p file-exists-p
	      file-local-copy file-modes file-name-as-directory
	      file-name-directory file-name-nondirectory
	      file-name-sans-versions file-notify-add-watch
	      file-ownership-preserved-p file-readable-p
	      file-regular-p file-remote-p file-selinux-context
	      file-symlink-p file-truename file-writable-p
	      find-backup-file-name find-file-noselect get-file-buffer
	      insert-directory insert-file-contents load
	      make-directory make-directory-internal set-file-acl
	      set-file-modes set-file-selinux-context set-file-times
	      substitute-in-file-name unhandled-file-name-directory
	      vc-registered
	      ;; Emacs 26+ only.
	      file-name-case-insensitive-p
	      ;; Emacs 27+ only.
	      file-system-info))
    (if (file-name-absolute-p (nth 0 args))
	(nth 0 args)
      default-directory))
   ;; FILE DIRECTORY resp FILE1 FILE2.
   ((member operation
	    '(add-name-to-file copy-directory copy-file expand-file-name
	      file-equal-p file-in-directory-p
	      file-name-all-completions file-name-completion
	      ;; Starting with Emacs 26.1, just the 2nd argument of
	      ;; `make-symbolic-link' matters.  For backward
	      ;; compatibility, we still accept the first argument as
	      ;; file name to be checked.  Handled properly in
	      ;; `tramp-handle-*-make-symbolic-link'.
	      file-newer-than-file-p make-symbolic-link rename-file))
    (save-match-data
      (cond
       ((tramp-tramp-file-p (nth 0 args)) (nth 0 args))
       ((tramp-tramp-file-p (nth 1 args)) (nth 1 args))
       (t default-directory))))
   ;; START END FILE.
   ((eq operation 'write-region)
    (if (file-name-absolute-p (nth 2 args))
	(nth 2 args)
      default-directory))
   ;; BUFFER.
   ((member operation
	    '(make-auto-save-file-name
	      set-visited-file-modtime verify-visited-file-modtime))
    (buffer-file-name
     (if (bufferp (nth 0 args)) (nth 0 args) (current-buffer))))
   ;; COMMAND.
   ((member operation
	    '(process-file shell-command start-file-process
	      ;; Emacs 26+ only.
	      make-nearby-temp-file temporary-file-directory))
    default-directory)
   ;; PROC.
   ((member operation
	    '(file-notify-rm-watch
	      ;; Emacs 25+ only.
	      file-notify-valid-p))
    (when (processp (nth 0 args))
      (with-current-buffer (process-buffer (nth 0 args))
	default-directory)))
   ;; Unknown file primitive.
   (t (error "unknown file I/O primitive: %s" operation))))

(defun tramp-find-foreign-file-name-handler (filename &optional _operation)
  "Return foreign file name handler if exists."
  (when (tramp-tramp-file-p filename)
    (let ((handler tramp-foreign-file-name-handler-alist)
	  elt res)
      (while handler
	(setq elt (car handler)
	      handler (cdr handler))
	(when (funcall (car elt) filename)
	  (setq handler nil
		res (cdr elt))))
      res)))

(defvar tramp-debug-on-error nil
  "Like `debug-on-error' but used Tramp internal.")

(defmacro tramp-condition-case-unless-debug
  (var bodyform &rest handlers)
  "Like `condition-case-unless-debug' but `tramp-debug-on-error'."
  `(let ((debug-on-error tramp-debug-on-error))
     (condition-case-unless-debug ,var ,bodyform ,@handlers)))

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

;; Main function.
(defun tramp-file-name-handler (operation &rest args)
  "Invoke Tramp file name handler.
Falls back to normal file name handler if no Tramp file name handler exists."
  (let ((filename (apply 'tramp-file-name-for-operation operation args)))
    (if (and tramp-mode (tramp-tramp-file-p filename))
	(save-match-data
          (setq filename (tramp-replace-environment-variables filename))
          (with-parsed-tramp-file-name filename nil
            (let ((completion (tramp-completion-mode-p))
		  (foreign
		   (tramp-find-foreign-file-name-handler filename operation))
		  result)
	      ;; Call the backend function.
	      (if foreign
		  (tramp-condition-case-unless-debug err
		    (let ((sf (symbol-function foreign)))
		      ;; Some packages set the default directory to a
		      ;; remote path, before respective Tramp packages
		      ;; are already loaded.  This results in
		      ;; recursive loading.  Therefore, we load the
		      ;; Tramp packages locally.
		      (when (autoloadp sf)
			(let ((default-directory
				(tramp-compat-temporary-file-directory)))
			  (load (cadr sf) 'noerror 'nomessage)))
;;		      (tramp-message
;;		       v 4 "Running `%s'..." (cons operation args))
		      ;; If `non-essential' is non-nil, Tramp shall
		      ;; not open a new connection.
		      ;; If Tramp detects that it shouldn't continue
		      ;; to work, it throws the `suppress' event.
		      ;; This could happen for example, when Tramp
		      ;; tries to open the same connection twice in a
		      ;; short time frame.
		      ;; In both cases, we try the default handler then.
		      (setq result
			    (catch 'non-essential
			      (catch 'suppress
				(when (and tramp-locked (not tramp-locker))
				  (setq tramp-locked nil)
				  (tramp-error
				   (car-safe tramp-current-connection)
				   'file-error
				   "Forbidden reentrant call of Tramp"))
				(let ((tl tramp-locked))
				  (setq tramp-locked t)
				  (unwind-protect
				      (let ((tramp-locker t))
					(apply foreign operation args))
				    (setq tramp-locked tl))))))
;;		      (tramp-message
;;		       v 4 "Running `%s'...`%s'" (cons operation args) result)
		      (cond
		       ((eq result 'non-essential)
			(tramp-message
			 v 5 "Non-essential received in operation %s"
			 (cons operation args))
			(tramp-run-real-handler operation args))
		       ((eq result 'suppress)
			(let (tramp-message-show-message)
			  (tramp-message
			   v 1 "Suppress received in operation %s"
			   (cons operation args))
			  (tramp-cleanup-connection v t)
			  (tramp-run-real-handler operation args)))
		       (t result)))

		    ;; Trace that somebody has interrupted the operation.
		    ((debug quit)
		     (let (tramp-message-show-message)
		       (tramp-message
			v 1 "Interrupt received in operation %s"
			(cons operation args)))
		     ;; Propagate the quit signal.
		     (signal (car err) (cdr err)))

		    ;; When we are in completion mode, some failed
		    ;; operations shall return at least a default
		    ;; value in order to give the user a chance to
		    ;; correct the file name in the minibuffer.
		    ;; In order to get a full backtrace, one could apply
		    ;;   (setq tramp-debug-on-error t)
		    (error
		     (cond
		      ((and completion (zerop (length localname))
			    (memq operation '(file-exists-p file-directory-p)))
		       t)
		      ((and completion (zerop (length localname))
			    (memq operation
				  '(expand-file-name file-name-as-directory)))
		       filename)
		      ;; Propagate the error.
		      (t (signal (car err) (cdr err))))))

		;; Nothing to do for us.  However, since we are in
		;; `tramp-mode', we must suppress the volume letter on
		;; MS Windows.
		(setq result (tramp-run-real-handler operation args))
		(if (stringp result)
		    (tramp-drop-volume-letter result)
		  result)))))

      ;; When `tramp-mode' is not enabled, or the file name is quoted,
      ;; we don't do anything.
      (tramp-run-real-handler operation args))))

(defun tramp-completion-file-name-handler (operation &rest args)
  "Invoke Tramp file name completion handler.
Falls back to normal file name handler if no Tramp file name handler exists."
  (let ((fn (assoc operation tramp-completion-file-name-handler-alist)))
    (if (and fn tramp-mode)
	(save-match-data (apply (cdr fn) args))
      (tramp-run-real-handler operation args))))

;;;###autoload
(progn (defun tramp-autoload-file-name-handler (operation &rest args)
  "Load Tramp file name handler, and perform OPERATION."
  (if tramp-mode
      (let ((default-directory temporary-file-directory))
	(load "tramp" 'noerror 'nomessage))
    (tramp-unload-file-name-handlers))
  (apply operation args)))

;; `tramp-autoload-file-name-handler' must be registered before
;; evaluation of site-start and init files, because there might exist
;; remote files already, f.e. files kept via recentf-mode.
;;;###autoload
(progn (defun tramp-register-autoload-file-name-handlers ()
  "Add Tramp file name handlers to `file-name-handler-alist' during autoload."
  (add-to-list 'file-name-handler-alist
	       (cons tramp-autoload-file-name-regexp
		     'tramp-autoload-file-name-handler))
  (put 'tramp-autoload-file-name-handler 'safe-magic t)))

;;;###autoload (tramp-register-autoload-file-name-handlers)

(defun tramp-use-absolute-autoload-file-names ()
  "Change Tramp autoload objects to use absolute file names.
This avoids problems during autoload, when `load-path' contains
remote file names."
  ;; We expect all other Tramp files in the same directory as tramp.el.
  (let* ((dir (expand-file-name (file-name-directory (locate-library "tramp"))))
	 (files-regexp
	  (format
	   "^%s$"
	   (regexp-opt
	    (mapcar
	     'file-name-sans-extension
	     (directory-files dir nil "^tramp.+\\.elc?$"))
	    'paren))))
    (mapatoms
     (lambda (atom)
       (when (and (functionp atom)
		  (autoloadp (symbol-function atom))
		  (string-match files-regexp (cadr (symbol-function atom))))
	 (ignore-errors
	   (setf (cadr (symbol-function atom))
		 (expand-file-name (cadr (symbol-function atom)) dir))))))))

(eval-after-load 'tramp (tramp-use-absolute-autoload-file-names))

(defun tramp-register-file-name-handlers ()
  "Add Tramp file name handlers to `file-name-handler-alist'."
  ;; Remove autoloaded handlers from file name handler alist.  Useful,
  ;; if `tramp-syntax' has been changed.  We cannot call
  ;; `tramp-unload-file-name-handlers', this would result in recursive
  ;; loading of Tramp.
  (dolist (fnh '(tramp-file-name-handler
		 tramp-completion-file-name-handler
		 tramp-archive-file-name-handler
		 tramp-autoload-file-name-handler))
    (let ((a1 (rassq fnh file-name-handler-alist)))
      (setq file-name-handler-alist (delq a1 file-name-handler-alist))))

  ;; Add the handlers.  We do not add anything to the `operations'
  ;; property of `tramp-file-name-handler' and
  ;; `tramp-archive-file-name-handler', this shall be done by the
  ;; respective foreign handlers.
  (add-to-list 'file-name-handler-alist
	       (cons tramp-file-name-regexp 'tramp-file-name-handler))
  (put 'tramp-file-name-handler 'safe-magic t)

  (add-to-list 'file-name-handler-alist
	       (cons tramp-completion-file-name-regexp
		     'tramp-completion-file-name-handler))
  (put 'tramp-completion-file-name-handler 'safe-magic t)
  ;; Mark `operations' the handler is responsible for.
  (put 'tramp-completion-file-name-handler 'operations
       (mapcar 'car tramp-completion-file-name-handler-alist))

  (add-to-list 'file-name-handler-alist
	       (cons tramp-archive-file-name-regexp
		     'tramp-archive-file-name-handler))
  (put 'tramp-archive-file-name-handler 'safe-magic t)

  ;; If jka-compr or epa-file are already loaded, move them to the
  ;; front of `file-name-handler-alist'.
  (dolist (fnh '(epa-file-handler jka-compr-handler))
    (let ((entry (rassoc fnh file-name-handler-alist)))
      (when entry
	(setq file-name-handler-alist
	      (cons entry (delete entry file-name-handler-alist)))))))

(eval-after-load 'tramp (tramp-register-file-name-handlers))

;;;###tramp-autoload
(progn (defun tramp-register-foreign-file-name-handler
    (func handler &optional append)
  "Register (FUNC . HANDLER) in `tramp-foreign-file-name-handler-alist'.
FUNC is the function, which determines whether HANDLER is to be called.
Add operations defined in `HANDLER-alist' to `tramp-file-name-handler'."
  (add-to-list
   'tramp-foreign-file-name-handler-alist `(,func . ,handler) append)
  ;; Mark `operations' the handler is responsible for.
  (put 'tramp-file-name-handler
       'operations
       (delete-dups
        (append
         (get 'tramp-file-name-handler 'operations)
         (mapcar
          'car
          (symbol-value (intern (concat (symbol-name handler) "-alist")))))))))

(defun tramp-exists-file-name-handler (operation &rest args)
  "Check, whether OPERATION runs a file name handler."
  ;; The file name handler is determined on base of either an
  ;; argument, `buffer-file-name', or `default-directory'.
  (ignore-errors
    (let* ((buffer-file-name "/")
	   (default-directory "/")
	   (fnha file-name-handler-alist)
	   (check-file-name-operation operation)
	   (file-name-handler-alist
	    (list
	     (cons "/"
		   (lambda (operation &rest args)
		     "Returns OPERATION if it is the one to be checked."
		     (if (equal check-file-name-operation operation)
			 operation
		       (let ((file-name-handler-alist fnha))
			 (apply operation args))))))))
      (equal (apply operation args) operation))))

;;;###autoload
(progn (defun tramp-unload-file-name-handlers ()
  "Unload Tramp file name handlers from `file-name-handler-alist'."
  (dolist (fnh '(tramp-file-name-handler
		 tramp-completion-file-name-handler
		 tramp-archive-file-name-handler
		 tramp-autoload-file-name-handler))
    (let ((a1 (rassq fnh file-name-handler-alist)))
      (setq file-name-handler-alist (delq a1 file-name-handler-alist))))))

(add-hook 'tramp-unload-hook 'tramp-unload-file-name-handlers)

;;; File name handler functions for completion mode:

;;;###autoload
(defvar tramp-completion-mode nil
  "If non-nil, external packages signal that they are in file name completion.")
(make-obsolete-variable 'tramp-completion-mode 'non-essential "26.1")

(defun tramp-completion-mode-p ()
  "Check, whether method / user name / host name completion is active."
  (or
   ;; Signal from outside.
   non-essential
   ;; This variable has been obsoleted in Emacs 26.
   tramp-completion-mode))

(defun tramp-connectable-p (filename)
  "Check, whether it is possible to connect the remote host w/o side-effects.
This is true, if either the remote host is already connected, or if we are
not in completion mode."
  (let (tramp-verbose)
    (and (tramp-tramp-file-p filename)
	 (or (not (tramp-completion-mode-p))
	     (process-live-p
	      (tramp-get-connection-process
	       (tramp-dissect-file-name filename)))))))

;; Method, host name and user name completion.
;; `tramp-completion-dissect-file-name' returns a list of
;; `tramp-file-name' structures.  For all of them we return possible
;; completions.
(defun tramp-completion-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for partial Tramp files."

  (let ((fullname
	 (tramp-drop-volume-letter (expand-file-name filename directory)))
	hop result result1)

    ;; Suppress hop from completion.
    (when (string-match
	   (concat
	    tramp-prefix-regexp
	    "\\(" "\\(" tramp-remote-file-name-spec-regexp
	                tramp-postfix-hop-regexp
	    "\\)+" "\\)")
	   fullname)
      (setq hop (match-string 1 fullname)
	    fullname (replace-match "" nil nil fullname 1)))

    ;; Possible completion structures.
    (dolist (elt (tramp-completion-dissect-file-name fullname))
      (let* ((method (tramp-file-name-method elt))
	     (user (tramp-file-name-user elt))
	     (host (tramp-file-name-host elt))
	     (localname (tramp-file-name-localname elt))
	     (m (tramp-find-method method user host))
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
		  (append result (tramp-get-completion-methods m)))))))

    ;; Unify list, add hop, remove nil elements.
    (dolist (elt result)
      (when elt
	(string-match tramp-prefix-regexp elt)
	(setq elt (replace-match (concat tramp-prefix-format hop) nil nil elt))
	(push
	 (substring elt (length (tramp-drop-volume-letter directory)))
	 result1)))

    ;; Complete local parts.
    (append
     result1
     (ignore-errors
       (tramp-run-real-handler
	'file-name-all-completions (list filename directory))))))

;; Method, host name and user name completion for a file.
(defun tramp-completion-handle-file-name-completion
  (filename directory &optional predicate)
  "Like `file-name-completion' for Tramp files."
  (try-completion
   filename
   (mapcar 'list (file-name-all-completions filename directory))
   (when (and predicate
	      (tramp-connectable-p (expand-file-name filename directory)))
     (lambda (x) (funcall predicate (expand-file-name (car x) directory))))))

;; I misuse a little bit the `tramp-file-name' structure in order to
;; handle completion possibilities for partial methods / user names /
;; host names.  Return value is a list of `tramp-file-name' structures
;; according to possible completions. If "localname" is non-nil it
;; means there shouldn't be a completion anymore.

;; Expected results:

;; "/x" "/[x"
;; ["x" nil nil nil]

;; "/x:" "/[x/"         "/x:y" "/[x/y"       "/x:y:" "/[x/y]"
;; ["x" nil "" nil]     ["x" nil "y" nil]    ["x" nil "y" ""]
;; ["x" "" nil nil]     ["x" "y" nil nil]

;; "/x:y@""/[x/y@"      "/x:y@z" "/[x/y@z"   "/x:y@z:" "/[x/y@z]"
;;["x" "y" nil nil]     ["x" "y" "z" nil]    ["x" "y" "z" ""]
(defun tramp-completion-dissect-file-name (name)
  "Returns a list of `tramp-file-name' structures.
They are collected by `tramp-completion-dissect-file-name1'."

  (let* ((x-nil "\\|\\(\\)")
	 (tramp-completion-ipv6-regexp
	  (format
	   "[^%s]*"
	   (if (zerop (length tramp-postfix-ipv6-format))
	       tramp-postfix-host-format
	     tramp-postfix-ipv6-format)))
	 ;; "/method" "/[method"
	 (tramp-completion-file-name-structure1
	  (list
	   (concat
	    tramp-prefix-regexp
	    "\\(" tramp-method-regexp x-nil "\\)$")
	   1 nil nil nil))
	 ;; "/method:user" "/[method/user"
	 (tramp-completion-file-name-structure2
	  (list
	   (concat
	    tramp-prefix-regexp
	    "\\(" tramp-method-regexp "\\)" tramp-postfix-method-regexp
	    "\\(" tramp-user-regexp x-nil   "\\)$")
	   1 2 nil nil))
	 ;; "/method:host" "/[method/host"
	 (tramp-completion-file-name-structure3
	  (list
	   (concat
	    tramp-prefix-regexp
	    "\\(" tramp-method-regexp "\\)" tramp-postfix-method-regexp
	    "\\(" tramp-host-regexp x-nil   "\\)$")
	   1 nil 2 nil))
	 ;; "/method:[ipv6" "/[method/ipv6"
	 (tramp-completion-file-name-structure4
	  (list
	   (concat
	    tramp-prefix-regexp
	    "\\(" tramp-method-regexp "\\)" tramp-postfix-method-regexp
	    tramp-prefix-ipv6-regexp
	    "\\(" tramp-completion-ipv6-regexp x-nil "\\)$")
	   1 nil 2 nil))
	 ;; "/method:user@host" "/[method/user@host"
	 (tramp-completion-file-name-structure5
	  (list
	   (concat
	    tramp-prefix-regexp
	    "\\(" tramp-method-regexp "\\)" tramp-postfix-method-regexp
	    "\\(" tramp-user-regexp "\\)"   tramp-postfix-user-regexp
	    "\\(" tramp-host-regexp x-nil   "\\)$")
	   1 2 3 nil))
	 ;; "/method:user@[ipv6" "/[method/user@ipv6"
	 (tramp-completion-file-name-structure6
	  (list
	   (concat
	    tramp-prefix-regexp
	    "\\(" tramp-method-regexp "\\)" tramp-postfix-method-regexp
	    "\\(" tramp-user-regexp "\\)"   tramp-postfix-user-regexp
	    tramp-prefix-ipv6-regexp
	    "\\(" tramp-completion-ipv6-regexp x-nil "\\)$")
	   1 2 3 nil)))
    (delq
     nil
     (mapcar
      (lambda (structure) (tramp-completion-dissect-file-name1 structure name))
      (list
       tramp-completion-file-name-structure1
       tramp-completion-file-name-structure2
       tramp-completion-file-name-structure3
       tramp-completion-file-name-structure4
       tramp-completion-file-name-structure5
       tramp-completion-file-name-structure6)))))

(defun tramp-completion-dissect-file-name1 (structure name)
  "Returns a `tramp-file-name' structure matching STRUCTURE.
The structure consists of remote method, remote user,
remote host and localname (filename on remote host)."

  (save-match-data
    (when (string-match (nth 0 structure) name)
      (make-tramp-file-name
       :method (and (nth 1 structure)
		    (match-string (nth 1 structure) name))
       :user (and (nth 2 structure)
		  (match-string (nth 2 structure) name))
       :host (and (nth 3 structure)
		  (match-string (nth 3 structure) name))))))

;; This function returns all possible method completions, adding the
;; trailing method delimiter.
(defun tramp-get-completion-methods (partial-method)
  "Returns all method completions for PARTIAL-METHOD."
  (mapcar
   (lambda (method)
     (and method
	  (string-match (concat "^" (regexp-quote partial-method)) method)
	  (tramp-completion-make-tramp-file-name method nil nil nil)))
   (mapcar 'car tramp-methods)))

;; Compares partial user and host names with possible completions.
(defun tramp-get-completion-user-host
  (method partial-user partial-host user host)
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

(defun tramp-parse-default-user-host (method)
  "Return a list of (user host) tuples allowed to access for METHOD.
This function is added always in `tramp-get-completion-function'
for all methods.  Resulting data are derived from default settings."
  `((,(tramp-find-user method nil nil) ,(tramp-find-host method nil nil))))

;; Generic function.
(defun tramp-parse-group (regexp match-level skip-regexp)
   "Return a (user host) tuple allowed to access.
User is always nil."
   (let (result)
     (when (re-search-forward regexp (point-at-eol) t)
       (setq result (list nil (match-string match-level))))
     (or
      (> (skip-chars-forward skip-regexp) 0)
      (forward-line 1))
     result))

;; Generic function.
(defun tramp-parse-file (filename function)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  ;; On Windows, there are problems in completion when
  ;; `default-directory' is remote.
  (let ((default-directory (tramp-compat-temporary-file-directory)))
    (when (file-readable-p filename)
      (with-temp-buffer
	(insert-file-contents filename)
	(goto-char (point-min))
        (cl-loop while (not (eobp)) collect (funcall function))))))

;;;###tramp-autoload
(defun tramp-parse-rhosts (filename)
  "Return a list of (user host) tuples allowed to access.
Either user or host may be nil."
  (tramp-parse-file filename 'tramp-parse-rhosts-group))

(defun tramp-parse-rhosts-group ()
   "Return a (user host) tuple allowed to access.
Either user or host may be nil."
   (let ((result)
	 (regexp
	  (concat
	   "^\\(" tramp-host-regexp "\\)"
	   "\\([ \t]+" "\\(" tramp-user-regexp "\\)" "\\)?")))
     (when (re-search-forward regexp (point-at-eol) t)
       (setq result (append (list (match-string 3) (match-string 1)))))
     (forward-line 1)
     result))

;;;###tramp-autoload
(defun tramp-parse-shosts (filename)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  (tramp-parse-file filename 'tramp-parse-shosts-group))

(defun tramp-parse-shosts-group ()
   "Return a (user host) tuple allowed to access.
User is always nil."
   (tramp-parse-group (concat "^\\(" tramp-host-regexp "\\)") 1 ","))

;;;###tramp-autoload
(defun tramp-parse-sconfig (filename)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  (tramp-parse-file filename 'tramp-parse-sconfig-group))

(defun tramp-parse-sconfig-group ()
   "Return a (user host) tuple allowed to access.
User is always nil."
   (tramp-parse-group
    (concat "^[ \t]*Host[ \t]+" "\\(" tramp-host-regexp "\\)") 1 ","))

;; Generic function.
(defun tramp-parse-shostkeys-sknownhosts (dirname regexp)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  ;; On Windows, there are problems in completion when
  ;; `default-directory' is remote.
  (let* ((default-directory (tramp-compat-temporary-file-directory))
	 (files (and (file-directory-p dirname) (directory-files dirname))))
    (cl-loop
     for f in files
     when (and (not (string-match "^\\.\\.?$" f)) (string-match regexp f))
     collect (list nil (match-string 1 f)))))

;;;###tramp-autoload
(defun tramp-parse-shostkeys (dirname)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  (tramp-parse-shostkeys-sknownhosts
   dirname (concat "^key_[0-9]+_\\(" tramp-host-regexp "\\)\\.pub$")))

;;;###tramp-autoload
(defun tramp-parse-sknownhosts (dirname)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  (tramp-parse-shostkeys-sknownhosts
   dirname
   (concat "^\\(" tramp-host-regexp "\\)\\.ssh-\\(dss\\|rsa\\)\\.pub$")))

;;;###tramp-autoload
(defun tramp-parse-hosts (filename)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  (tramp-parse-file filename 'tramp-parse-hosts-group))

(defun tramp-parse-hosts-group ()
   "Return a (user host) tuple allowed to access.
User is always nil."
   (tramp-parse-group
    (concat "^\\(" tramp-ipv6-regexp "\\|" tramp-host-regexp "\\)") 1 " \t"))

;;;###tramp-autoload
(defun tramp-parse-passwd (filename)
  "Return a list of (user host) tuples allowed to access.
Host is always \"localhost\"."
  (with-tramp-connection-property nil "parse-passwd"
    (if (executable-find "getent")
	(with-temp-buffer
	  (when (zerop (tramp-call-process nil "getent" nil t nil "passwd"))
	    (goto-char (point-min))
	    (cl-loop while (not (eobp)) collect
		     (tramp-parse-etc-group-group))))
      (tramp-parse-file filename 'tramp-parse-passwd-group))))

(defun tramp-parse-passwd-group ()
   "Return a (user host) tuple allowed to access.
Host is always \"localhost\"."
   (let ((result)
	 (regexp (concat "^\\(" tramp-user-regexp "\\):")))
     (when (re-search-forward regexp (point-at-eol) t)
       (setq result (list (match-string 1) "localhost")))
     (forward-line 1)
     result))

;;;###tramp-autoload
(defun tramp-parse-etc-group (filename)
  "Return a list of (group host) tuples allowed to access.
Host is always \"localhost\"."
  (with-tramp-connection-property nil "parse-group"
    (if (executable-find "getent")
	(with-temp-buffer
	  (when (zerop (tramp-call-process nil "getent" nil t nil "group"))
	    (goto-char (point-min))
	    (cl-loop while (not (eobp)) collect
		     (tramp-parse-etc-group-group))))
      (tramp-parse-file filename 'tramp-parse-etc-group-group))))

(defun tramp-parse-etc-group-group ()
   "Return a (group host) tuple allowed to access.
Host is always \"localhost\"."
   (let ((result)
	 (split (split-string (buffer-substring (point) (point-at-eol)) ":")))
     (when (member (user-login-name) (split-string (nth 3 split) "," 'omit))
       (setq result (list (nth 0 split) "localhost")))
     (forward-line 1)
     result))

;;;###tramp-autoload
(defun tramp-parse-netrc (filename)
  "Return a list of (user host) tuples allowed to access.
User may be nil."
  (tramp-parse-file filename 'tramp-parse-netrc-group))

(defun tramp-parse-netrc-group ()
   "Return a (user host) tuple allowed to access.
User may be nil."
   (let ((result)
	 (regexp
	  (concat
	   "^[ \t]*machine[ \t]+" "\\(" tramp-host-regexp "\\)"
	   "\\([ \t]+login[ \t]+" "\\(" tramp-user-regexp "\\)" "\\)?")))
     (when (re-search-forward regexp (point-at-eol) t)
       (setq result (list (match-string 3) (match-string 1))))
     (forward-line 1)
     result))

;;;###tramp-autoload
(defun tramp-parse-putty (registry-or-dirname)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  (if (memq system-type '(windows-nt))
      (with-tramp-connection-property nil "parse-putty"
	(with-temp-buffer
	  (when (zerop (tramp-call-process
			nil "reg" nil t nil "query" registry-or-dirname))
	    (goto-char (point-min))
	    (cl-loop while (not (eobp)) collect
		     (tramp-parse-putty-group registry-or-dirname)))))
    ;; UNIX case.
    (tramp-parse-shostkeys-sknownhosts
     registry-or-dirname (concat "^\\(" tramp-host-regexp "\\)$"))))

(defun tramp-parse-putty-group (registry)
   "Return a (user host) tuple allowed to access.
User is always nil."
   (let ((result)
	 (regexp (concat (regexp-quote registry) "\\\\\\(.+\\)")))
     (when (re-search-forward regexp (point-at-eol) t)
       (setq result (list nil (match-string 1))))
     (forward-line 1)
     result))

;;; Common file name handler functions for different backends:

(defvar tramp-handle-file-local-copy-hook nil
  "Normal hook to be run at the end of `tramp-*-handle-file-local-copy'.")

(defvar tramp-handle-write-region-hook nil
  "Normal hook to be run at the end of `tramp-*-handle-write-region'.")

(defun tramp-handle-add-name-to-file
  (filename newname &optional ok-if-already-exists)
  "Like `add-name-to-file' for Tramp files."
  (with-parsed-tramp-file-name
      (if (tramp-tramp-file-p newname) newname filename) nil
    (unless (tramp-equal-remote filename newname)
      (tramp-error
       v 'file-error
       "add-name-to-file: %s"
       "only implemented for same method, same user, same host"))
    ;; Do the 'confirm if exists' thing.
    (when (file-exists-p newname)
      ;; What to do?
      (if (or (null ok-if-already-exists) ; not allowed to exist
	      (and (numberp ok-if-already-exists)
		   (not (yes-or-no-p
			 (format
			  "File %s already exists; make it a link anyway? "
			  localname)))))
	  (tramp-error v 'file-already-exists newname)
	(delete-file newname)))
    (tramp-flush-file-properties v (file-name-directory localname))
    (tramp-flush-file-properties v localname)
    (copy-file
     filename newname 'ok-if-already-exists 'keep-time
     'preserve-uid-gid 'preserve-permissions)))

(defun tramp-handle-directory-file-name (directory)
  "Like `directory-file-name' for Tramp files."
  ;; If localname component of filename is "/", leave it unchanged.
  ;; Otherwise, remove any trailing slash from localname component.
  ;; Method, host, etc, are unchanged.
  (while (with-parsed-tramp-file-name directory nil
	   (and (not (zerop (length localname)))
		(eq (aref localname (1- (length localname))) ?/)
		(not (string= localname "/"))))
    (setq directory (substring directory 0 -1)))
  directory)

(defun tramp-handle-directory-files (directory &optional full match nosort)
  "Like `directory-files' for Tramp files."
  (when (file-directory-p directory)
    (setq directory (file-name-as-directory (expand-file-name directory)))
    (let ((temp (nreverse (file-name-all-completions "" directory)))
	  result item)

      (while temp
	(setq item (directory-file-name (pop temp)))
	(when (or (null match) (string-match match item))
	  (push (if full (concat directory item) item)
		result)))
      (if nosort result (sort result 'string<)))))

(defun tramp-handle-directory-files-and-attributes
  (directory &optional full match nosort id-format)
  "Like `directory-files-and-attributes' for Tramp files."
  (mapcar
   (lambda (x)
     (cons x (file-attributes
	      (if full x (expand-file-name x directory)) id-format)))
   (directory-files directory full match nosort)))

(defun tramp-handle-dired-uncache (dir)
  "Like `dired-uncache' for Tramp files."
  (with-parsed-tramp-file-name
      (if (file-directory-p dir) dir (file-name-directory dir)) nil
    (tramp-flush-directory-properties v localname)))

(defun tramp-handle-file-accessible-directory-p (filename)
  "Like `file-accessible-directory-p' for Tramp files."
  (and (file-directory-p filename)
       (file-readable-p filename)))

(defun tramp-handle-file-directory-p (filename)
  "Like `file-directory-p' for Tramp files."
  (eq (tramp-compat-file-attribute-type
       (file-attributes (file-truename filename)))
      t))

(defun tramp-handle-file-equal-p (filename1 filename2)
  "Like `file-equalp-p' for Tramp files."
  ;; Native `file-equalp-p' calls `file-truename', which requires a
  ;; remote connection.  This can be avoided, if FILENAME1 and
  ;; FILENAME2 are not located on the same remote host.
  (when (string-equal
	 (file-remote-p (expand-file-name filename1))
	 (file-remote-p (expand-file-name filename2)))
    (tramp-run-real-handler 'file-equal-p (list filename1 filename2))))

(defun tramp-handle-file-exists-p (filename)
  "Like `file-exists-p' for Tramp files."
  (not (null (file-attributes filename))))

(defun tramp-handle-file-in-directory-p (filename directory)
  "Like `file-in-directory-p' for Tramp files."
  ;; Native `file-in-directory-p' calls `file-truename', which
  ;; requires a remote connection.  This can be avoided, if FILENAME
  ;; and DIRECTORY are not located on the same remote host.
  (when (string-equal
	 (file-remote-p (expand-file-name filename))
	 (file-remote-p (expand-file-name directory)))
    (tramp-run-real-handler 'file-in-directory-p (list filename directory))))

(defun tramp-handle-file-modes (filename)
  "Like `file-modes' for Tramp files."
  (let ((truename (or (file-truename filename) filename)))
    (when (file-exists-p truename)
      (tramp-mode-string-to-int
       (tramp-compat-file-attribute-modes (file-attributes truename))))))

;; Localname manipulation functions that grok Tramp localnames...
(defun tramp-handle-file-name-as-directory (file)
  "Like `file-name-as-directory' but aware of Tramp files."
  ;; `file-name-as-directory' would be sufficient except localname is
  ;; the empty string.
  (let ((v (tramp-dissect-file-name file t)))
    ;; Run the command on the localname portion only unless we are in
    ;; completion mode.
    (tramp-make-tramp-file-name
     v (unless (and (zerop (length (tramp-file-name-localname v)))
		    (not (tramp-connectable-p file)))
	 (tramp-run-real-handler
	  'file-name-as-directory
	  (list (or (tramp-file-name-localname v) "")))))))

(defun tramp-handle-file-name-case-insensitive-p (filename)
  "Like `file-name-case-insensitive-p' for Tramp files."
  ;; We make it a connection property, assuming that all file systems
  ;; on the remote host behave similar.  This might be wrong for
  ;; mounted NFS directories or SMB/AFP shares; such more granular
  ;; tests will be added in case they are needed.
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (or ;; Maybe there is a default value.
     (tramp-get-method-parameter v 'tramp-case-insensitive)

     ;; There isn't. So we must check, in case there's a connection already.
     (and (file-remote-p filename nil 'connected)
          (with-tramp-connection-property v "case-insensitive"
	    (ignore-errors
	      (with-tramp-progress-reporter v 5 "Checking case-insensitive"
		;; The idea is to compare a file with lower case
		;; letters with the same file with upper case letters.
		(let ((candidate
		       (tramp-compat-file-name-unquote
			(directory-file-name filename)))
		      tmpfile)
		  ;; Check, whether we find an existing file with
		  ;; lower case letters.  This avoids us to create a
		  ;; temporary file.
		  (while (and (string-match
			       "[a-z]" (file-remote-p candidate 'localname))
			      (not (file-exists-p candidate)))
		    (setq candidate
			  (directory-file-name
			   (file-name-directory candidate))))
		  ;; Nothing found, so we must use a temporary file
		  ;; for comparison.  `make-nearby-temp-file' is added
		  ;; to Emacs 26+ like `file-name-case-insensitive-p',
		  ;; so there is no compatibility problem calling it.
		  (unless
		      (string-match
		       "[a-z]" (file-remote-p candidate 'localname))
		    (setq tmpfile
			  (let ((default-directory
				  (file-name-directory filename)))
			    (tramp-compat-funcall
			     'make-nearby-temp-file "tramp."))
			  candidate tmpfile))
		  ;; Check for the existence of the same file with
		  ;; upper case letters.
		  (unwind-protect
		      (file-exists-p
		       (concat
			(file-remote-p candidate)
			(upcase (file-remote-p candidate 'localname))))
		    ;; Cleanup.
		    (when tmpfile (delete-file tmpfile)))))))))))

(defun tramp-handle-file-name-completion
  (filename directory &optional predicate)
  "Like `file-name-completion' for Tramp files."
  (let (hits-ignored-extensions)
    (or
     (try-completion
      filename (file-name-all-completions filename directory)
      (lambda (x)
	(when (funcall (or predicate 'identity) (expand-file-name x directory))
	  (not
	   (and
	    completion-ignored-extensions
	    (string-match
	     (concat (regexp-opt completion-ignored-extensions 'paren) "$") x)
	    ;; We remember the hit.
	    (push x hits-ignored-extensions))))))
     ;; No match.  So we try again for ignored files.
     (try-completion filename hits-ignored-extensions))))

(defun tramp-handle-file-name-directory (file)
  "Like `file-name-directory' but aware of Tramp files."
  ;; Everything except the last filename thing is the directory.  We
  ;; cannot apply `with-parsed-tramp-file-name', because this expands
  ;; the remote file name parts.  This is a problem when we are in
  ;; file name completion.
  (let ((v (tramp-dissect-file-name file t)))
    ;; Run the command on the localname portion only.
    (tramp-make-tramp-file-name
     v (tramp-run-real-handler
	'file-name-directory (list (or (tramp-file-name-localname v) ""))))))

(defun tramp-handle-file-name-nondirectory (file)
  "Like `file-name-nondirectory' but aware of Tramp files."
  (with-parsed-tramp-file-name file nil
    (tramp-run-real-handler 'file-name-nondirectory (list localname))))

(defun tramp-handle-file-newer-than-file-p (file1 file2)
  "Like `file-newer-than-file-p' for Tramp files."
  (cond
   ((not (file-exists-p file1)) nil)
   ((not (file-exists-p file2)) t)
   (t (time-less-p (tramp-compat-file-attribute-modification-time
		    (file-attributes file2))
		   (tramp-compat-file-attribute-modification-time
		    (file-attributes file1))))))

(defun tramp-handle-file-regular-p (filename)
  "Like `file-regular-p' for Tramp files."
  (and (file-exists-p filename)
       (eq ?-
	   (aref (tramp-compat-file-attribute-modes (file-attributes filename))
		 0))))

(defun tramp-handle-file-remote-p (filename &optional identification connected)
  "Like `file-remote-p' for Tramp files."
  ;; We do not want traces in the debug buffer.
  (let ((tramp-verbose (min tramp-verbose 3)))
    (when (tramp-tramp-file-p filename)
      (let* ((v (tramp-dissect-file-name filename))
	     (p (tramp-get-connection-process v))
	     (c (and (process-live-p p)
		     (tramp-get-connection-property p "connected" nil))))
	;; We expand the file name only, if there is already a connection.
	(with-parsed-tramp-file-name
	    (if c (expand-file-name filename) filename) nil
	  (and (or (not connected) c)
	       (cond
		((eq identification 'method) method)
		;; Domain and port are appended to user and host,
		;; respectively.
		((eq identification 'user) (tramp-file-name-user-domain v))
		((eq identification 'host) (tramp-file-name-host-port v))
		((eq identification 'localname) localname)
		((eq identification 'hop) hop)
		(t (tramp-make-tramp-file-name
		    method user domain host port "" hop)))))))))

(defun tramp-handle-file-selinux-context (_filename)
  "Like `file-selinux-context' for Tramp files."
  ;; Return nil context.
  '(nil nil nil nil))

(defun tramp-handle-file-symlink-p (filename)
  "Like `file-symlink-p' for Tramp files."
  (let ((x (tramp-compat-file-attribute-type (file-attributes filename))))
    (and (stringp x) x)))

(defun tramp-handle-file-truename (filename)
  "Like `file-truename' for Tramp files."
  (let ((result (expand-file-name filename))
	(numchase 0)
	;; Don't make the following value larger than
	;; necessary.  People expect an error message in a
	;; timely fashion when something is wrong;
	;; otherwise they might think that Emacs is hung.
	;; Of course, correctness has to come first.
	(numchase-limit 20)
	symlink-target)
    (format
     "%s%s"
     (with-parsed-tramp-file-name result v1
       (with-tramp-file-property v1 v1-localname "file-truename"
	 (while (and (setq symlink-target (file-symlink-p result))
		     (< numchase numchase-limit))
	   (setq numchase (1+ numchase)
		 result
		 (with-parsed-tramp-file-name (expand-file-name result) v2
		   (tramp-make-tramp-file-name
		    v2-method v2-user v2-domain v2-host v2-port
		    (funcall
		     (if (tramp-compat-file-name-quoted-p v2-localname)
			 'tramp-compat-file-name-quote 'identity)

		     (if (stringp symlink-target)
			 (if (file-remote-p symlink-target)
			     (let (file-name-handler-alist)
			       (tramp-compat-file-name-quote symlink-target))
			   (expand-file-name
			    symlink-target (file-name-directory v2-localname)))
		       v2-localname)))))
	   (when (>= numchase numchase-limit)
	     (tramp-error
	      v1 'file-error
	      "Maximum number (%d) of symlinks exceeded" numchase-limit)))
	 (directory-file-name result)))

     ;; Preserve trailing "/".
     (if (string-equal (file-name-nondirectory filename) "") "/" ""))))

(defun tramp-handle-find-backup-file-name (filename)
  "Like `find-backup-file-name' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (let ((backup-directory-alist
	   (if tramp-backup-directory-alist
	       (mapcar
		(lambda (x)
		  (cons
		   (car x)
		   (if (and (stringp (cdr x))
			    (file-name-absolute-p (cdr x))
			    (not (tramp-tramp-file-p (cdr x))))
		       (tramp-make-tramp-file-name
			method user domain host port (cdr x) hop)
		     (cdr x))))
		tramp-backup-directory-alist)
	     backup-directory-alist)))
      (tramp-run-real-handler 'find-backup-file-name (list filename)))))

(defun tramp-handle-insert-directory
  (filename switches &optional wildcard full-directory-p)
  "Like `insert-directory' for Tramp files."
  (unless switches (setq switches ""))
  ;; Mark trailing "/".
  (when (and (zerop (length (file-name-nondirectory filename)))
	     (not full-directory-p))
    (setq switches (concat switches "F")))
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-progress-reporter v 0 (format "Opening directory %s" filename)
      (require 'ls-lisp)
      (let (ls-lisp-use-insert-directory-program start)
	(tramp-run-real-handler
	 'insert-directory
	 (list filename switches wildcard full-directory-p))
	;; `ls-lisp' always returns full listings.  We must remove
	;; superfluous parts.
	(unless (string-match "l" switches)
	  (save-excursion
	    (goto-char (point-min))
	    (while (setq start
			 (text-property-not-all
			  (point) (point-at-eol) 'dired-filename t))
	      (delete-region
	       start
	       (or (text-property-any start (point-at-eol) 'dired-filename t)
		   (point-at-eol)))
	      (if (=  (point-at-bol) (point-at-eol))
		  ;; Empty line.
		  (delete-region (point) (progn (forward-line) (point)))
		(forward-line)))))))))

(defun tramp-handle-insert-file-contents
  (filename &optional visit beg end replace)
  "Like `insert-file-contents' for Tramp files."
  (barf-if-buffer-read-only)
  (setq filename (expand-file-name filename))
  (let (result local-copy remote-copy)
    (with-parsed-tramp-file-name filename nil
      (unwind-protect
	  (if (not (file-exists-p filename))
	      (tramp-error
	       v tramp-file-missing
	       "File `%s' not found on remote host" filename)

	    (with-tramp-progress-reporter
		v 3 (format-message "Inserting `%s'" filename)
	      (condition-case err
		  (if (and (tramp-local-host-p v)
			   (let (file-name-handler-alist)
			     (file-readable-p localname)))
		      ;; Short track: if we are on the local host, we can
		      ;; run directly.
		      (setq result
			    (tramp-run-real-handler
			     'insert-file-contents
			     (list localname visit beg end replace)))

		    ;; When we shall insert only a part of the file, we
		    ;; copy this part.  This works only for the shell file
		    ;; name handlers.
		    (when (and (or beg end)
			       (tramp-get-method-parameter
				v 'tramp-login-program))
		      (setq remote-copy (tramp-make-tramp-temp-file v))
		      ;; This is defined in tramp-sh.el.  Let's assume
		      ;; this is loaded already.
		      (tramp-compat-funcall
		       'tramp-send-command
		       v
		       (cond
			((and beg end)
			 (format "dd bs=1 skip=%d if=%s count=%d of=%s"
				 beg (tramp-shell-quote-argument localname)
				 (- end beg) remote-copy))
			(beg
			 (format "dd bs=1 skip=%d if=%s of=%s"
				 beg (tramp-shell-quote-argument localname)
				 remote-copy))
			(end
			 (format "dd bs=1 count=%d if=%s of=%s"
				 end (tramp-shell-quote-argument localname)
				 remote-copy))))
		      (setq tramp-temp-buffer-file-name nil beg nil end nil))

		    ;; `insert-file-contents-literally' takes care to
		    ;; avoid calling jka-compr.el and epa.el.  By
		    ;; let-binding `inhibit-file-name-operation', we
		    ;; propagate that care to the `file-local-copy'
		    ;; operation.
		    (setq local-copy
			  (let ((inhibit-file-name-operation
				 (when (eq inhibit-file-name-operation
					   'insert-file-contents)
				   'file-local-copy)))
			    (cond
			     ((stringp remote-copy)
			      (file-local-copy
			       (tramp-make-tramp-file-name
				method user domain host port remote-copy)))
			     ((stringp tramp-temp-buffer-file-name)
			      (copy-file
			       filename tramp-temp-buffer-file-name 'ok)
			      tramp-temp-buffer-file-name)
			     (t (file-local-copy filename)))))

		    ;; When the file is not readable for the owner, it
		    ;; cannot be inserted, even if it is readable for the
		    ;; group or for everybody.
		    (set-file-modes local-copy (string-to-number "0600" 8))

		    (when (and (null remote-copy)
			       (tramp-get-method-parameter
				v 'tramp-copy-keep-tmpfile))
		      ;; We keep the local file for performance reasons,
		      ;; useful for "rsync".
		      (setq tramp-temp-buffer-file-name local-copy))

		    ;; We must ensure that `file-coding-system-alist'
		    ;; matches `local-copy'.
		    (let ((file-coding-system-alist
			   (tramp-find-file-name-coding-system-alist
			    filename local-copy)))
		      (setq result
			    (insert-file-contents
			     local-copy visit beg end replace))))
		(error
		 (add-hook 'find-file-not-found-functions
			   `(lambda () (signal ',(car err) ',(cdr err)))
			   nil t)
		 (signal (car err) (cdr err))))))

	;; Save exit.
	(progn
	  (when visit
	    (setq buffer-file-name filename)
	    (setq buffer-read-only (not (file-writable-p filename)))
	    (set-visited-file-modtime)
	    (set-buffer-modified-p nil))
	  (when (and (stringp local-copy)
		     (or remote-copy (null tramp-temp-buffer-file-name)))
	    (delete-file local-copy))
	  (when (stringp remote-copy)
	    (delete-file
	     (tramp-make-tramp-file-name
	      method user domain host port remote-copy)))))

      ;; Result.
      (list (expand-file-name filename)
	    (cadr result)))))

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
	(tramp-error
	 v tramp-file-missing "Cannot load nonexistent file `%s'" file)))
    (if (not (file-exists-p file))
	nil
      (let ((tramp-message-show-message (not nomessage)))
	(with-tramp-progress-reporter v 0 (format "Loading %s" file)
	  (let ((local-copy (file-local-copy file)))
	    (unwind-protect
		(load local-copy noerror t nosuffix must-suffix)
	      (delete-file local-copy)))))
      t)))

(defun tramp-handle-make-symbolic-link
  (target linkname &optional ok-if-already-exists)
  "Like `make-symbolic-link' for Tramp files.
This is the fallback implementation for backends which do not
support symbolic links."
  (if (tramp-tramp-file-p (expand-file-name linkname))
      (tramp-error
       (tramp-dissect-file-name (expand-file-name linkname)) 'file-error
       "make-symbolic-link not supported")
    ;; This is needed prior Emacs 26.1, where TARGET has also be
    ;; checked for a file name handler.
    (tramp-run-real-handler
     'make-symbolic-link (list target linkname ok-if-already-exists))))

(defun tramp-handle-shell-command
  (command &optional output-buffer error-buffer)
  "Like `shell-command' for Tramp files."
  (let* ((asynchronous (string-match "[ \t]*&[ \t]*\\'" command))
	 ;; We cannot use `shell-file-name' and `shell-command-switch',
	 ;; they are variables of the local host.
	 (args (append
		(cons
		 (tramp-get-method-parameter
		  (tramp-dissect-file-name default-directory)
		  'tramp-remote-shell)
		 (tramp-get-method-parameter
		  (tramp-dissect-file-name default-directory)
		  'tramp-remote-shell-args))
		(list (substring command 0 asynchronous))))
	 current-buffer-p
	 (output-buffer
	  (cond
	   ((bufferp output-buffer) output-buffer)
	   ((stringp output-buffer) (get-buffer-create output-buffer))
	   (output-buffer
	    (setq current-buffer-p t)
	    (current-buffer))
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
	(tramp-compat-user-error p "Shell command in progress")))

    (if current-buffer-p
	(progn
	  (barf-if-buffer-read-only)
	  (push-mark nil t))
      (with-current-buffer output-buffer
	(setq buffer-read-only nil)
	(erase-buffer)))

    (if (and (not current-buffer-p) (integerp asynchronous))
	(prog1
	    ;; Run the process.
	    (setq p (apply 'start-file-process "*Async Shell*" buffer args))
	  ;; Display output.
	  (with-current-buffer output-buffer
	    (display-buffer output-buffer '(nil (allow-no-window . t)))
	    (setq mode-line-process '(":%s"))
	    (shell-mode)
	    (set-process-sentinel p 'shell-command-sentinel)
	    (set-process-filter p 'comint-output-filter)))

      (prog1
	  ;; Run the process.
	  (apply 'process-file (car args) nil buffer nil (cdr args))
	;; Insert error messages if they were separated.
	(when (listp buffer)
	  (with-current-buffer error-buffer
	    (insert-file-contents (cadr buffer)))
	  (delete-file (cadr buffer)))
	(if current-buffer-p
	    ;; This is like exchange-point-and-mark, but doesn't
	    ;; activate the mark.  It is cleaner to avoid activation,
	    ;; even though the command loop would deactivate the mark
	    ;; because we inserted text.
	    (goto-char (prog1 (mark t)
			 (set-marker (mark-marker) (point)
				     (current-buffer))))
	  ;; There's some output, display it.
	  (when (with-current-buffer output-buffer (> (point-max) (point-min)))
	    (display-message-or-buffer output-buffer)))))))

(defun tramp-handle-substitute-in-file-name (filename)
  "Like `substitute-in-file-name' for Tramp files.
\"//\" and \"/~\" substitute only in the local filename part."
  ;; Check, whether the local part is a quoted file name.
  (if (tramp-compat-file-name-quoted-p filename)
      filename
    ;; First, we must replace environment variables.
    (setq filename (tramp-replace-environment-variables filename))
    (with-parsed-tramp-file-name filename nil
      ;; Ignore in LOCALNAME everything before "//" or "/~".
      (when (and (stringp localname) (string-match ".+?/\\(/\\|~\\)" localname))
	(setq filename
	      (concat (file-remote-p filename)
		      (replace-match "\\1" nil nil localname)))
	;; "/m:h:~" does not work for completion.  We use "/m:h:~/".
	(when (string-match "~$" filename)
	  (setq filename (concat filename "/"))))
      ;; We do not want to replace environment variables, again.
      (let (process-environment)
	(tramp-run-real-handler 'substitute-in-file-name (list filename))))))

(defun tramp-handle-set-visited-file-modtime (&optional time-list)
  "Like `set-visited-file-modtime' for Tramp files."
  (unless (buffer-file-name)
    (error "Can't set-visited-file-modtime: buffer `%s' not visiting a file"
	   (buffer-name)))
  (unless time-list
    (let ((remote-file-name-inhibit-cache t))
      ;; '(-1 65535) means file doesn't exists yet.
      (setq time-list
	    (or (tramp-compat-file-attribute-modification-time
		 (file-attributes (buffer-file-name)))
		'(-1 65535)))))
  ;; We use '(0 0) as a don't-know value.
  (unless (equal time-list '(0 0))
    (tramp-run-real-handler 'set-visited-file-modtime (list time-list))))

(defun tramp-handle-verify-visited-file-modtime (&optional buf)
  "Like `verify-visited-file-modtime' for Tramp files.
At the time `verify-visited-file-modtime' calls this function, we
already know that the buffer is visiting a file and that
`visited-file-modtime' does not return 0.  Do not call this
function directly, unless those two cases are already taken care
of."
  (with-current-buffer (or buf (current-buffer))
    (let ((f (buffer-file-name)))
      ;; There is no file visiting the buffer, or the buffer has no
      ;; recorded last modification time, or there is no established
      ;; connection.
      (if (or (not f)
	      (eq (visited-file-modtime) 0)
	      (not (file-remote-p f nil 'connected)))
	  t
	(let* ((remote-file-name-inhibit-cache t)
	       (attr (file-attributes f))
	       (modtime (tramp-compat-file-attribute-modification-time attr))
	       (mt (visited-file-modtime)))

	  (cond
	   ;; File exists, and has a known modtime.
	   ((and attr (not (equal modtime '(0 0))))
	    (< (abs (tramp-time-diff
		     modtime
		     ;; For compatibility, deal with both the old
		     ;; (HIGH . LOW) and the new (HIGH LOW) return
		     ;; values of `visited-file-modtime'.
		     (if (atom (cdr mt))
			 (list (car mt) (cdr mt))
		       mt)))
	       2))
	   ;; Modtime has the don't know value.
	   (attr t)
	   ;; If file does not exist, say it is not modified if and
	   ;; only if that agrees with the buffer's record.
	   (t (equal mt '(-1 65535)))))))))

(defun tramp-handle-file-notify-add-watch (filename _flags _callback)
  "Like `file-notify-add-watch' for Tramp files."
  ;; This is the default handler.  tramp-gvfs.el and tramp-sh.el have
  ;; their own one.
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (tramp-error
     v 'file-notify-error "File notification not supported for `%s'" filename)))

(defun tramp-handle-file-notify-rm-watch (proc)
  "Like `file-notify-rm-watch' for Tramp files."
  ;; The descriptor must be a process object.
  (unless (processp proc)
    (tramp-error proc 'file-notify-error "Not a valid descriptor %S" proc))
  (tramp-message proc 6 "Kill %S" proc)
  (delete-process proc))

(defun tramp-handle-file-notify-valid-p (proc)
  "Like `file-notify-valid-p' for Tramp files."
  (and (process-live-p proc)
       ;; Sometimes, the process is still in status `run' when the
       ;; file or directory to be watched is deleted already.
       (with-current-buffer (process-buffer proc)
	 (file-exists-p
	  (concat (file-remote-p default-directory)
		  (process-get proc 'watch-name))))))

;;; Functions for establishing connection:

;; The following functions are actions to be taken when seeing certain
;; prompts from the remote host.  See the variable
;; `tramp-actions-before-shell' for usage of these functions.

(defun tramp-action-login (_proc vec)
  "Send the login name."
  (let ((user (or (tramp-file-name-user vec)
		  (with-tramp-connection-property vec "login-as"
		    (save-window-excursion
		      (let ((enable-recursive-minibuffers t))
			(pop-to-buffer (tramp-get-connection-buffer vec))
			(read-string (match-string 0))))))))
    (with-current-buffer (tramp-get-connection-buffer vec)
      (tramp-message vec 6 "\n%s" (buffer-string)))
    (tramp-message vec 3 "Sending login name `%s'" user)
    (tramp-send-string vec (concat user tramp-local-end-of-line))))

(defun tramp-action-password (proc vec)
  "Query the user for a password."
  (with-current-buffer (process-buffer proc)
    (let ((enable-recursive-minibuffers t)
	  (case-fold-search t))
      ;; Let's check whether a wrong password has been sent already.
      ;; Sometimes, the process returns a new password request
      ;; immediately after rejecting the previous (wrong) one.
      (unless (tramp-get-connection-property vec "first-password-request" nil)
	(tramp-clear-passwd vec))
      (goto-char (point-min))
      (tramp-check-for-regexp proc tramp-password-prompt-regexp)
      (tramp-message vec 3 "Sending %s" (match-string 1))
      ;; We don't call `tramp-send-string' in order to hide the
      ;; password from the debug buffer.
      (process-send-string
       proc (concat (tramp-read-passwd proc) tramp-local-end-of-line))
      ;; Hide password prompt.
      (narrow-to-region (point-max) (point-max)))))

(defun tramp-action-succeed (_proc _vec)
  "Signal success in finding shell prompt."
  (throw 'tramp-action 'ok))

(defun tramp-action-permission-denied (proc _vec)
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
      (tramp-send-string vec (concat "yes" tramp-local-end-of-line)))))

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
      (tramp-send-string vec (concat "y" tramp-local-end-of-line)))))

(defun tramp-action-terminal (_proc vec)
  "Tell the remote host which terminal type to use.
The terminal type can be configured with `tramp-terminal-type'."
  (tramp-message vec 5 "Setting `%s' as terminal type." tramp-terminal-type)
  (with-current-buffer (tramp-get-connection-buffer vec)
    (tramp-message vec 6 "\n%s" (buffer-string)))
  (tramp-send-string vec (concat tramp-terminal-type tramp-local-end-of-line)))

(defun tramp-action-process-alive (proc _vec)
  "Check, whether a process has finished."
  (unless (process-live-p proc)
    (throw 'tramp-action 'process-died)))

(defun tramp-action-out-of-band (proc vec)
  "Check, whether an out-of-band copy has finished."
  ;; There might be pending output for the exit status.
  (tramp-accept-process-output proc 0.1)
  (cond ((and (not (process-live-p proc))
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
	     (throw 'tramp-action 'out-of-band-failed))))
	(t nil)))

;;; Functions for processing the actions:

(defun tramp-process-one-action (proc vec actions)
  "Wait for output from the shell and perform one action."
  (let ((case-fold-search t)
	found todo item pattern action)
    (while (not found)
      ;; Reread output once all actions have been performed.
      ;; Obviously, the output was not complete.
      (tramp-accept-process-output proc 1)
      (setq todo actions)
      (while todo
	(setq item (pop todo))
	(setq pattern (format "\\(%s\\)\\'" (symbol-value (nth 0 item))))
	(setq action (nth 1 item))
	(tramp-message
	 vec 5 "Looking for regexp \"%s\" from remote shell" pattern)
	(when (tramp-check-for-regexp proc pattern)
	  (tramp-message vec 5 "Call `%s'" (symbol-name action))
	  (setq found (funcall action proc vec)))))
    found))

(defun tramp-process-actions (proc vec pos actions &optional timeout)
  "Perform ACTIONS until success or TIMEOUT.
PROC and VEC indicate the remote connection to be used.  POS, if
set, is the starting point of the region to be deleted in the
connection buffer."
  ;; Enable `auth-source', unless "emacs -Q" has been called.  We must
  ;; use the "password-vector" property in case we have several hops.
  (tramp-set-connection-property
   (tramp-get-connection-property
    proc "password-vector" (process-get proc 'vector))
   "first-password-request" tramp-cache-read-persistent-data)
  (save-restriction
    (with-tramp-progress-reporter
	proc 3 "Waiting for prompts from remote shell"
      (let (exit)
	(if timeout
	    (with-timeout (timeout (setq exit 'timeout))
	      (while (not exit)
		(setq exit
		      (catch 'tramp-action
			(tramp-process-one-action proc vec actions)))))
	  (while (not exit)
	    (setq exit
		  (catch 'tramp-action
		    (tramp-process-one-action proc vec actions)))))
	(with-current-buffer (tramp-get-connection-buffer vec)
	  (widen)
	  (tramp-message vec 6 "\n%s" (buffer-string)))
	(unless (eq exit 'ok)
	  (tramp-clear-passwd vec)
	  (delete-process proc)
	  (tramp-error-with-buffer
	   (tramp-get-connection-buffer vec) vec 'file-error
	   (cond
	    ((eq exit 'permission-denied) "Permission denied")
	    ((eq exit 'out-of-band-failed)
	     (format-message
	      "Copy failed, see buffer `%s' for details"
	      (tramp-get-connection-buffer vec)))
	    ((eq exit 'process-died)
             (substitute-command-keys
              (concat
               "Tramp failed to connect.  If this happens repeatedly, try\n"
               "    `\\[tramp-cleanup-this-connection]'")))
	    ((eq exit 'timeout)
	     (format-message
	      "Timeout reached, see buffer `%s' for details"
	      (tramp-get-connection-buffer vec)))
	    (t "Login failed")))))
      (when (numberp pos)
	(with-current-buffer (tramp-get-connection-buffer vec)
	  (let (buffer-read-only) (delete-region pos (point))))))))

;;; Utility functions:

(defun tramp-accept-process-output (proc timeout)
  "Like `accept-process-output' for Tramp processes.
This is needed in order to hide `last-coding-system-used', which is set
for process communication also."
  (with-current-buffer (process-buffer proc)
    (let (buffer-read-only last-coding-system-used
	  ;; We do not want to run timers.
	  timer-list timer-idle-list)
      ;; Under Windows XP, `accept-process-output' doesn't return
      ;; sometimes.  So we add an additional timeout.  JUST-THIS-ONE
      ;; is set due to Bug#12145.  It is an integer, in order to avoid
      ;; running timers as well.
      (tramp-message
       proc 10 "%s %s %s\n%s"
       proc (process-status proc)
       (with-timeout (timeout)
	 (accept-process-output proc timeout nil 0))
       (buffer-string)))))

(defun tramp-check-for-regexp (proc regexp)
  "Check, whether REGEXP is contained in process buffer of PROC.
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
	  (forward-line 1)
	  (delete-region begin (point))
	  (goto-char (point-min)))))

    (when (or (not (tramp-get-connection-property proc "check-remote-echo" nil))
	      ;; Sometimes, the echo string is suppressed on the remote side.
	      (not (string-equal
		    (substring-no-properties
		     tramp-echo-mark-marker
		     0 (min tramp-echo-mark-marker-length (1- (point-max))))
		    (buffer-substring-no-properties
		     (point-min)
		     (min (+ (point-min) tramp-echo-mark-marker-length)
			  (point-max))))))
      ;; No echo to be handled, now we can look for the regexp.
      ;; Sometimes, lines are much too long, and we run into a "Stack
      ;; overflow in regexp matcher".  For example, //DIRED// lines of
      ;; directory listings with some thousand files.  Therefore, we
      ;; look from the end.
      (goto-char (point-max))
      (ignore-errors (re-search-backward regexp nil t)))))

(defun tramp-wait-for-regexp (proc timeout regexp)
  "Wait for a REGEXP to appear from process PROC within TIMEOUT seconds.
Expects the output of PROC to be sent to the current buffer.  Returns
the string that matched, or nil.  Waits indefinitely if TIMEOUT is
nil."
  (with-current-buffer (process-buffer proc)
    (let ((found (tramp-check-for-regexp proc regexp)))
      (cond (timeout
	     (with-timeout (timeout)
	       (while (not found)
		 (tramp-accept-process-output proc 1)
		 (unless (process-live-p proc)
		   (tramp-error-with-buffer
		    nil proc 'file-error "Process has died"))
		 (setq found (tramp-check-for-regexp proc regexp)))))
	    (t
	     (while (not found)
	       (tramp-accept-process-output proc 1)
	       (unless (process-live-p proc)
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
	    (mapconcat
	     'identity (split-string string "\n") tramp-rsh-end-of-line))
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

(defun tramp-get-inode (vec)
  "Returns the virtual inode number.
If it doesn't exist, generate a new one."
  (with-tramp-file-property vec (tramp-file-name-localname vec) "inode"
    (setq tramp-inodes (1+ tramp-inodes))))

(defun tramp-get-device (vec)
  "Returns the virtual device number.
If it doesn't exist, generate a new one."
  (with-tramp-connection-property (tramp-get-connection-process vec) "device"
    (cons -1 (setq tramp-devices (1+ tramp-devices)))))

(defun tramp-equal-remote (file1 file2)
  "Check, whether the remote parts of FILE1 and FILE2 are identical.
The check depends on method, user and host name of the files.  If
one of the components is missing, the default values are used.
The local file name parts of FILE1 and FILE2 are not taken into
account.

Example:

  (tramp-equal-remote \"/ssh::/etc\" \"/<your host name>:/home\")

would yield t.  On the other hand, the following check results in nil:

  (tramp-equal-remote \"/sudo::/etc\" \"/su::/etc\")"
  (and (tramp-tramp-file-p file1)
       (tramp-tramp-file-p file2)
       (string-equal (file-remote-p file1) (file-remote-p file2))))

;;;###tramp-autoload
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
	((char-equal owner-read ?r) (string-to-number "00400" 8))
	((char-equal owner-read ?-) 0)
	(t (error "Second char `%c' must be one of `r-'" owner-read)))
       (cond
	((char-equal owner-write ?w) (string-to-number "00200" 8))
	((char-equal owner-write ?-) 0)
	(t (error "Third char `%c' must be one of `w-'" owner-write)))
       (cond
	((char-equal owner-execute-or-setid ?x) (string-to-number "00100" 8))
	((char-equal owner-execute-or-setid ?S) (string-to-number "04000" 8))
	((char-equal owner-execute-or-setid ?s) (string-to-number "04100" 8))
	((char-equal owner-execute-or-setid ?-) 0)
	(t (error "Fourth char `%c' must be one of `xsS-'"
		  owner-execute-or-setid)))
       (cond
	((char-equal group-read ?r) (string-to-number "00040" 8))
	((char-equal group-read ?-) 0)
	(t (error "Fifth char `%c' must be one of `r-'" group-read)))
       (cond
	((char-equal group-write ?w) (string-to-number "00020" 8))
	((char-equal group-write ?-) 0)
	(t (error "Sixth char `%c' must be one of `w-'" group-write)))
       (cond
	((char-equal group-execute-or-setid ?x) (string-to-number "00010" 8))
	((char-equal group-execute-or-setid ?S) (string-to-number "02000" 8))
	((char-equal group-execute-or-setid ?s) (string-to-number "02010" 8))
	((char-equal group-execute-or-setid ?-) 0)
	(t (error "Seventh char `%c' must be one of `xsS-'"
		  group-execute-or-setid)))
       (cond
	((char-equal other-read ?r) (string-to-number "00004" 8))
	((char-equal other-read ?-) 0)
	(t (error "Eighth char `%c' must be one of `r-'" other-read)))
       (cond
	((char-equal other-write ?w) (string-to-number "00002" 8))
	((char-equal other-write ?-) 0)
	(t (error "Ninth char `%c' must be one of `w-'" other-write)))
       (cond
	((char-equal other-execute-or-sticky ?x) (string-to-number "00001" 8))
	((char-equal other-execute-or-sticky ?T) (string-to-number "01000" 8))
	((char-equal other-execute-or-sticky ?t) (string-to-number "01001" 8))
	((char-equal other-execute-or-sticky ?-) 0)
	(t (error "Tenth char `%c' must be one of `xtT-'"
		  other-execute-or-sticky)))))))

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

;;;###tramp-autoload
(defun tramp-file-mode-from-int (mode)
  "Turn an integer representing a file mode into an ls(1)-like string."
  (let ((type	(cdr
		 (assoc (logand (lsh mode -12) 15) tramp-file-mode-type-map)))
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

;;;###tramp-autoload
(defun tramp-get-local-uid (id-format)
  "The uid of the local user, in ID-FORMAT.
ID-FORMAT valid values are `string' and `integer'."
  (if (equal id-format 'integer) (user-uid) (user-login-name)))

;;;###tramp-autoload
(defun tramp-get-local-gid (id-format)
  "The gid of the local user, in ID-FORMAT.
ID-FORMAT valid values are `string' and `integer'."
  ;; `group-gid' has been introduced with Emacs 24.4.
  (if (and (fboundp 'group-gid) (equal id-format 'integer))
      (tramp-compat-funcall 'group-gid)
    (tramp-compat-file-attribute-group-id (file-attributes "~/" id-format))))

(defun tramp-get-local-locale (&optional vec)
  "Determine locale, supporting UTF8 if possible.
VEC is used for tracing."
  ;; We use key nil for local connection properties.
  (with-tramp-connection-property nil "locale"
    (let ((candidates '("en_US.utf8" "C.utf8" "en_US.UTF-8"))
	  locale)
      (with-temp-buffer
	(unless (or (memq system-type '(windows-nt))
                    (not (zerop (tramp-call-process
                                 nil "locale" nil t nil "-a"))))
	  (while candidates
	    (goto-char (point-min))
	    (if (string-match (format "^%s\r?$" (regexp-quote (car candidates)))
			      (buffer-string))
		(setq locale (car candidates)
		      candidates nil)
	      (setq candidates (cdr candidates))))))
      ;; Return value.
      (when vec (tramp-message vec 7 "locale %s" (or locale "C")))
      (or locale "C"))))

;;;###tramp-autoload
(defun tramp-check-cached-permissions (vec access)
  "Check `file-attributes' caches for VEC.
Return t if according to the cache access type ACCESS is known to
be granted."
  (let ((result nil)
        (offset (cond
                 ((eq ?r access) 1)
                 ((eq ?w access) 2)
                 ((eq ?x access) 3))))
    (dolist (suffix '("string" "integer") result)
      (setq
       result
       (or
        result
        (let ((file-attr
	       (or
		(tramp-get-file-property
		 vec (tramp-file-name-localname vec)
		 (concat "file-attributes-" suffix) nil)
		(file-attributes
		 (tramp-make-tramp-file-name vec) (intern suffix))))
              (remote-uid
               (tramp-get-connection-property
                vec (concat "uid-" suffix) nil))
              (remote-gid
               (tramp-get-connection-property
                vec (concat "gid-" suffix) nil))
	      (unknown-id
	       (if (string-equal suffix "string")
		   tramp-unknown-id-string tramp-unknown-id-integer)))
          (and
           file-attr
           (or
            ;; Not a symlink.
            (eq t (tramp-compat-file-attribute-type file-attr))
            (null (tramp-compat-file-attribute-type file-attr)))
           (or
            ;; World accessible.
            (eq access
		(aref (tramp-compat-file-attribute-modes file-attr)
		      (+ offset 6)))
            ;; User accessible and owned by user.
            (and
             (eq access
		 (aref (tramp-compat-file-attribute-modes file-attr) offset))
	     (or (equal remote-uid
			(tramp-compat-file-attribute-user-id file-attr))
		 (equal unknown-id
			(tramp-compat-file-attribute-user-id file-attr))))
            ;; Group accessible and owned by user's principal group.
            (and
             (eq access
		 (aref (tramp-compat-file-attribute-modes file-attr)
		       (+ offset 3)))
             (or (equal remote-gid
			(tramp-compat-file-attribute-group-id file-attr))
		 (equal unknown-id
			(tramp-compat-file-attribute-group-id
			 file-attr))))))))))))

;;;###tramp-autoload
(defun tramp-local-host-p (vec)
  "Return t if this points to the local host, nil otherwise."
  (let ((host (tramp-file-name-host vec))
	(port (tramp-file-name-port vec)))
    (and
     (stringp host)
     (string-match tramp-local-host-regexp host)
     ;; A port is an indication for an ssh tunnel or alike.
     (null port)
     ;; The method shall be applied to one of the shell file name
     ;; handlers.  `tramp-local-host-p' is also called for "smb" and
     ;; alike, where it must fail.
     (tramp-get-method-parameter vec 'tramp-login-program)
     ;; The local temp directory must be writable for the other user.
     (file-writable-p
      (tramp-make-tramp-file-name
       vec (tramp-compat-temporary-file-directory) 'nohop))
     ;; On some systems, chown runs only for root.
     (or (zerop (user-uid))
	 ;; This is defined in tramp-sh.el.  Let's assume this is
	 ;; loaded already.
	 (zerop (tramp-compat-funcall 'tramp-get-remote-uid vec 'integer))))))

(defun tramp-get-remote-tmpdir (vec)
  "Return directory for temporary files on the remote host identified by VEC."
  (with-tramp-connection-property vec "tmpdir"
    (let ((dir
	   (tramp-make-tramp-file-name
	    vec (or (tramp-get-method-parameter vec 'tramp-tmpdir) "/tmp"))))
      (or (and (file-directory-p dir) (file-writable-p dir)
	       (file-remote-p dir 'localname))
	  (tramp-error vec 'file-error "Directory %s not accessible" dir))
      dir)))

;;;###tramp-autoload
(defun tramp-make-tramp-temp-file (vec)
  "Create a temporary file on the remote host identified by VEC.
Return the local name of the temporary file."
  (let ((prefix (expand-file-name
		 tramp-temp-name-prefix (tramp-get-remote-tmpdir vec)))
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
	(set-file-modes result (string-to-number "0700" 8))))

    ;; Return the local part.
    (with-parsed-tramp-file-name result nil localname)))

(defun tramp-delete-temp-file-function ()
  "Remove temporary files related to current buffer."
  (when (stringp tramp-temp-buffer-file-name)
    (ignore-errors (delete-file tramp-temp-buffer-file-name))))

(add-hook 'kill-buffer-hook 'tramp-delete-temp-file-function)
(add-hook 'tramp-unload-hook
	  (lambda ()
	    (remove-hook 'kill-buffer-hook
			 'tramp-delete-temp-file-function)))

(defun tramp-handle-make-auto-save-file-name ()
  "Like `make-auto-save-file-name' for Tramp files.
Returns a file name in `tramp-auto-save-directory' for autosaving
this file, if that variable is non-nil."
  (when (stringp tramp-auto-save-directory)
    (setq tramp-auto-save-directory
	  (expand-file-name tramp-auto-save-directory)))
  ;; Create directory.
  (unless (or (null tramp-auto-save-directory)
	      (file-exists-p tramp-auto-save-directory))
    (make-directory tramp-auto-save-directory t))

  (let ((system-type
	 (if (and (stringp tramp-auto-save-directory)
		  (file-remote-p tramp-auto-save-directory))
	     'not-windows
	   system-type))
	(auto-save-file-name-transforms
	 (if (null tramp-auto-save-directory)
	     auto-save-file-name-transforms))
	(buffer-file-name
	 (if (null tramp-auto-save-directory)
	     buffer-file-name
	   (expand-file-name
	    (tramp-subst-strs-in-string
	     '(("_" . "|")
	       ("/" . "_a")
	       (":" . "_b")
	       ("|" . "__")
	       ("[" . "_l")
	       ("]" . "_r"))
	     (tramp-compat-file-name-unquote (buffer-file-name)))
	    tramp-auto-save-directory))))
    ;; Run plain `make-auto-save-file-name'.
    (tramp-run-real-handler 'make-auto-save-file-name nil)))

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

(defun tramp-handle-temporary-file-directory ()
  "Like `temporary-file-directory' for Tramp files."
  (catch 'result
    (dolist (dir `(,(ignore-errors
		      (tramp-get-remote-tmpdir
		       (tramp-dissect-file-name default-directory)))
		   ,default-directory))
      (when (and (stringp dir) (file-directory-p dir) (file-writable-p dir))
	(throw 'result (expand-file-name dir))))))

(defun tramp-handle-make-nearby-temp-file (prefix &optional dir-flag suffix)
  "Like `make-nearby-temp-file' for Tramp files."
  (let ((temporary-file-directory
	 (tramp-compat-temporary-file-directory-function)))
    (make-temp-file prefix dir-flag suffix)))

;;; Compatibility functions section:

(defun tramp-call-process
  (vec program &optional infile destination display &rest args)
  "Calls `call-process' on the local host.
It always returns a return code.  The Lisp error raised when
PROGRAM is nil is trapped also, returning 1.  Furthermore, traces
are written with verbosity of 6."
  (let ((default-directory  (tramp-compat-temporary-file-directory))
	(destination (if (eq destination t) (current-buffer) destination))
	output error result)
    (tramp-message
     vec 6 "`%s %s' %s %s"
     program (mapconcat 'identity args " ") infile destination)
    (condition-case err
	(with-temp-buffer
	  (setq result
		(apply
		 'call-process program infile (or destination t) display args))
	  ;; `result' could also be an error string.
	  (when (stringp result)
	    (setq error result
		  result 1))
	  (with-current-buffer
	      (if (bufferp destination) destination (current-buffer))
	    (setq output (buffer-string))))
      (error
       (setq error (error-message-string err)
	     result 1)))
    (if (zerop (length error))
	(tramp-message vec 6 "%d\n%s" result output)
      (tramp-message vec 6 "%d\n%s\n%s" result output error))
    result))

(defun tramp-call-process-region
  (vec start end program &optional delete buffer display &rest args)
  "Calls `call-process-region' on the local host.
It always returns a return code.  The Lisp error raised when
PROGRAM is nil is trapped also, returning 1.  Furthermore, traces
are written with verbosity of 6."
  (let ((default-directory  (tramp-compat-temporary-file-directory))
	(buffer (if (eq buffer t) (current-buffer) buffer))
	result)
    (tramp-message
     vec 6 "`%s %s' %s %s %s %s"
     program (mapconcat 'identity args " ") start end delete buffer)
    (condition-case err
	(progn
	  (setq result
		(apply
		 'call-process-region
		 start end program delete buffer display args))
	  ;; `result' could also be an error string.
	  (when (stringp result)
	    (signal 'file-error (list result)))
	  (with-current-buffer (if (bufferp buffer) buffer (current-buffer))
            (if (zerop result)
                (tramp-message vec 6 "%d" result)
              (tramp-message vec 6 "%d\n%s" result (buffer-string)))))
      (error
       (setq result 1)
       (tramp-message vec 6 "%d\n%s" result (error-message-string err))))
    result))

;;;###tramp-autoload
(defun tramp-read-passwd (proc &optional prompt)
  "Read a password from user (compat function).
Consults the auth-source package.
Invokes `password-read' if available, `read-passwd' else."
  (let* ((case-fold-search t)
	 (key (tramp-make-tramp-file-name
	       ;; In tramp-sh.el, we must use "password-vector" due to
	       ;; multi-hop.
	       (tramp-get-connection-property
		proc "password-vector" (process-get proc 'vector))
	       'noloc 'nohop))
	 (pw-prompt
	  (or prompt
	      (with-current-buffer (process-buffer proc)
		(tramp-check-for-regexp proc tramp-password-prompt-regexp)
		(format "%s for %s " (capitalize (match-string 1)) key))))
	 ;; We suspend the timers while reading the password.
         (stimers (with-timeout-suspend))
	 auth-info auth-passwd)

    (unwind-protect
	(with-parsed-tramp-file-name key nil
	  (setq user
		(or user (tramp-get-connection-property key "login-as" nil)))
	  (prog1
	      (or
	       ;; See if auth-sources contains something useful.
	       (ignore-errors
		 (and (tramp-get-connection-property
		       v "first-password-request" nil)
		      ;; Try with Tramp's current method.
		      (setq auth-info
			    (auth-source-search
			     :max 1
			     (and user :user)
			     (if domain
				 (concat user tramp-prefix-domain-format domain)
			       user)
			     :host
			     (if port
				 (concat host tramp-prefix-port-format port)
			       host)
			     :port method
			     :require (cons :secret (and user '(:user))))
			    auth-passwd (plist-get
					 (nth 0 auth-info) :secret)
			    auth-passwd (if (functionp auth-passwd)
					    (funcall auth-passwd)
					  auth-passwd))))
	       ;; Try the password cache.
	       (let ((password (password-read pw-prompt key)))
		 (password-cache-add key password)
		 password)
	       ;; Else, get the password interactively.
	       (read-passwd pw-prompt))
	    (tramp-set-connection-property v "first-password-request" nil)))
      ;; Reenable the timers.
      (with-timeout-unsuspend stimers))))

;;;###tramp-autoload
(defun tramp-clear-passwd (vec)
  "Clear password cache for connection related to VEC."
  (let ((method (tramp-file-name-method vec))
	(user-domain (tramp-file-name-user-domain vec))
	(host-port (tramp-file-name-host-port vec))
	(hop (tramp-file-name-hop vec)))
    (when hop
      ;; Clear also the passwords of the hops.
      (tramp-clear-passwd
       (tramp-dissect-file-name
	(concat
	 tramp-prefix-format
	 (replace-regexp-in-string
	  (concat tramp-postfix-hop-regexp "$")
	  tramp-postfix-host-format hop)))))
    (auth-source-forget
     `(:max 1 ,(and user-domain :user) ,user-domain
       :host ,host-port :port ,method))
    (password-cache-remove (tramp-make-tramp-file-name vec 'noloc 'nohop))))

;; Snarfed code from time-date.el.

(defconst tramp-half-a-year '(241 17024)
"Evaluated by \"(days-to-time 183)\".")

;;;###tramp-autoload
(defun tramp-time-diff (t1 t2)
  "Return the difference between the two times, in seconds.
T1 and T2 are time values (as returned by `current-time' for example)."
  ;; Starting with Emacs 25.1, we could change this to use `time-subtract'.
  (float-time (tramp-compat-funcall 'subtract-time t1 t2)))

(defun tramp-unquote-shell-quote-argument (s)
  "Remove quotation prefix \"/:\" from string S, and quote it then for shell."
  (shell-quote-argument (tramp-compat-file-name-unquote s)))

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
;; `shell-quote-argument' behaves as if on Unix.
;;
;; Thanks to Mario DeWeerd for the hint that it is sufficient for this
;; function to work with Bourne-like shells.
;;;###tramp-autoload
(defun tramp-shell-quote-argument (s)
  "Similar to `shell-quote-argument', but groks newlines.
Only works for Bourne-like shells."
  (let ((system-type 'not-windows))
    (save-match-data
      (let ((result (tramp-unquote-shell-quote-argument s))
	    (nl (regexp-quote (format "\\%s" tramp-rsh-end-of-line))))
	(when (and (>= (length result) 2)
		   (string= (substring result 0 2) "\\~"))
	  (setq result (substring result 1)))
	(while (string-match nl result)
	  (setq result (replace-match (format "'%s'" tramp-rsh-end-of-line)
				      t t result)))
	result))))

;;; Signal handling.  This works for remote processes, which have set
;;; the process property `remote-pid'.

(defun tramp-interrupt-process (&optional process _current-group)
  "Interrupt remote process PROC."
  ;; CURRENT-GROUP is not implemented yet.
  (let ((proc (cond
	       ((processp process) process)
	       ((bufferp process)  (get-buffer-process process))
	       ((stringp process)  (or (get-process process)
				       (get-buffer-process process)))
	       ((null process)     (get-buffer-process (current-buffer)))
	       (t                  process)))
	pid)
    ;; If it's a Tramp process, send the INT signal remotely.
    (when (and (processp proc) (setq pid (process-get proc 'remote-pid)))
      (if (not  (process-live-p proc))
	  (tramp-error proc 'error "Process %s is not active" proc)
	(tramp-message proc 5 "Interrupt process %s with pid %s" proc pid)
	;; This is for tramp-sh.el.  Other backends do not support this (yet).
	(tramp-compat-funcall
	 'tramp-send-command
	 (process-get proc 'vector)
	 (format "kill -2 %d" pid))
	;; Wait, until the process has disappeared.  If it doesn't,
	;; fall back to the default implementation.
	(with-timeout (1 (ignore))
	  (while (process-live-p proc)
	    ;; We cannot run `tramp-accept-process-output', it blocks timers.
	    (accept-process-output proc 0.1))
	  ;; Report success.
	  proc)))))

;; `interrupt-process-functions' exists since Emacs 26.1.
(when (boundp 'interrupt-process-functions)
  (add-hook 'interrupt-process-functions 'tramp-interrupt-process)
  (add-hook
   'tramp-unload-hook
   (lambda ()
     (remove-hook 'interrupt-process-functions 'tramp-interrupt-process))))

;;; Integration of eshell.el:

;; eshell.el keeps the path in `eshell-path-env'.  We must change it
;; when `default-directory' points to another host.
(defun tramp-eshell-directory-change ()
  "Set `eshell-path-env' to $PATH of the host related to `default-directory'."
  (setq eshell-path-env
	(if (tramp-tramp-file-p default-directory)
	    (with-parsed-tramp-file-name default-directory nil
	      (mapconcat
	       'identity
	       (or
		;; When `tramp-own-remote-path' is in `tramp-remote-path',
		;; the remote path is only set in the session cache.
		(tramp-get-connection-property
		 (tramp-get-connection-process v) "remote-path" nil)
		(tramp-get-connection-property v "remote-path" nil))
	       ":"))
	  (getenv "PATH"))))

(eval-after-load "esh-util"
  '(progn
     (add-hook 'eshell-mode-hook
	       'tramp-eshell-directory-change)
     (add-hook 'eshell-directory-change-hook
	       'tramp-eshell-directory-change)
     (add-hook 'tramp-unload-hook
	       (lambda ()
		 (remove-hook 'eshell-mode-hook
			      'tramp-eshell-directory-change)
		 (remove-hook 'eshell-directory-change-hook
			      'tramp-eshell-directory-change)))))

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
  ;; ange-ftp settings must be enabled.
  (tramp-compat-funcall 'tramp-ftp-enable-ange-ftp)
  ;; Maybe it's not loaded yet.
  (ignore-errors (unload-feature 'tramp 'force)))

(provide 'tramp)

;;; TODO:
;;
;; * Avoid screen blanking when hitting `g' in dired.  (Eli Tziperman)
;;
;; * Better error checking.  At least whenever we see something
;;   strange when doing zerop, we should kill the process and start
;;   again.  (Greg Stark)
;;
;; * Make shadowfile.el grok Tramp filenames.  (Bug#4526, Bug#4846)
;;
;; * I was wondering if it would be possible to use tramp even if I'm
;;   actually using sshfs.  But when I launch a command I would like
;;   to get it executed on the remote machine where the files really
;;   are.  (Andrea Crotti)
;;
;; * Run emerge on two remote files.  Bug is described here:
;;   <https://www.mail-archive.com/tramp-devel@nongnu.org/msg01041.html>.
;;   (Bug#6850)
;;
;; * Refactor code from different handlers.  Start with
;;   *-process-file.  One idea is to generalize `tramp-send-command'
;;   and friends, for most of the handlers this is the major
;;   difference between the different backends.  Other handlers but
;;   *-process-file would profit from this as well.

;;; tramp.el ends here

;; Local Variables:
;; mode: Emacs-Lisp
;; coding: utf-8
;; End:
