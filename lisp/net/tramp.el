;;; tramp.el --- Transparent Remote Access, Multiple Protocol  -*- lexical-binding:t -*-

;; Copyright (C) 1998-2022 Free Software Foundation, Inc.

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
;; ------
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
(require 'tramp-integration)
(require 'trampver)

;; Pacify byte-compiler.
(require 'cl-lib)
(declare-function file-notify-rm-watch "filenotify")
(declare-function netrc-parse "netrc")
(defvar auto-save-file-name-transforms)

;; Reload `tramp-compat' when we reload `tramp-autoloads' of the GNU ELPA package.
;;;###autoload (when (featurep 'tramp-compat)
;;;###autoload   (load "tramp-compat" 'noerror 'nomessage))

;;; User Customizable Internal Variables:

(defgroup tramp nil
  "Edit remote files with a combination of ssh, scp, etc."
  :group 'files
  :group 'comm
  :link '(custom-manual "(tramp)Top")
  :version "22.1")

(eval-and-compile ;; So it's also available in tramp-loaddefs.el!
  (defvar tramp--startup-hook nil
    "Forms to be executed at the end of tramp.el.")
  (put 'tramp--startup-hook 'tramp-suppress-trace t)

  (defmacro tramp--with-startup (&rest body)
    "Schedule BODY to be executed at the end of tramp.el."
    `(add-hook 'tramp--startup-hook (lambda () ,@body))))

(require 'tramp-loaddefs)

;; Maybe we need once a real Tramp mode, with key bindings etc.
;;;###autoload
(defcustom tramp-mode t
  "Whether Tramp is enabled.
If it is set to nil, all remote file names are used literally."
  :type 'boolean)

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
10  traces (huge)
11  call traces (maintainer only)."
  :type 'integer)

(defcustom tramp-debug-to-file nil
  "Whether Tramp debug messages shall be saved to file.
The debug file has the same name as the debug buffer, written to
`temporary-file-directory'."
  :version "28.1"
  :type 'boolean)

(defcustom tramp-backup-directory-alist nil
  "Alist of filename patterns and backup directory names.
Each element looks like (REGEXP . DIRECTORY), with the same meaning like
in `backup-directory-alist'.  If a Tramp file is backed up, and DIRECTORY
is a local file name, the backup directory is prepended with Tramp file
name prefix \(method, user, host) of file.

  (setq tramp-backup-directory-alist backup-directory-alist)

gives the same backup policy for Tramp files on their hosts like the
policy for local files."
  :type '(repeat (cons (regexp :tag "Regexp matching filename")
		       (directory :tag "Backup directory name"))))

(defcustom tramp-auto-save-directory nil
  "Put auto-save files in this directory, if set.
The idea is to use a local directory so that auto-saving is faster.
This setting has precedence over `auto-save-file-name-transforms'."
  :type '(choice (const :tag "Use default" nil)
		 (directory :tag "Auto save directory name")))

;; Suppress `shell-file-name' for w32 systems.
(defcustom tramp-encoding-shell
  (let (shell-file-name)
    (or (tramp-compat-funcall 'w32-shell-name) "/bin/sh"))
  "Use this program for encoding and decoding commands on the local host.
This shell is used to execute the encoding and decoding command on the
local host, so if you want to use \"~\" in those commands, you should
choose a shell here which groks tilde expansion.  \"/bin/sh\" normally
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
  :type '(file :must-match t))

;; Suppress `shell-file-name' for w32 systems.
(defcustom tramp-encoding-command-switch
  (let (shell-file-name)
    (if (tramp-compat-funcall 'w32-shell-dos-semantics) "/c" "-c"))
  "Use this switch together with `tramp-encoding-shell' for local commands.
See the variable `tramp-encoding-shell' for more information."
  :type 'string)

;; Suppress `shell-file-name' for w32 systems.
(defcustom tramp-encoding-command-interactive
  (let (shell-file-name)
    (unless (tramp-compat-funcall 'w32-shell-dos-semantics) "-i"))
  "Use this switch together with `tramp-encoding-shell' for interactive shells.
See the variable `tramp-encoding-shell' for more information."
  :version "24.1"
  :type '(choice (const nil) string))

;; Since Emacs 26.1, `system-name' can return nil at build time if
;; Emacs is compiled with "--no-build-details".  We do expect it to be
;; a string.  (Bug#44481)
(defconst tramp-system-name (or (system-name) "")
  "The system name Tramp is running locally.")

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
    This specifies a list of lists of arguments to pass to the
    above mentioned program.  You normally want to put each
    argument in an individual string, i.e.
    (\"-a\" \"-b\") rather than (\"-a -b\").

    \"%\" followed by a letter are expanded in the arguments as
    follows:

    - \"%h\" is replaced by the host name
    - \"%u\" is replaced by the user name
    - \"%p\" is replaced by the port number
    - \"%%\" can be used to obtain a literal percent character.

    If a sub-list containing \"%h\", \"%u\" or \"%p\" is
    unchanged after expansion (i.e. no host, no user or no port
    were specified), that sublist is not used.  For e.g.

    '((\"-a\" \"-b\") (\"-l\" \"%u\"))

    that means that (\"-l\" \"%u\") is used only if the user was
    specified, and it is thus effectively optional.

    Other expansions are:

    - \"%l\" is replaced by the login shell `tramp-remote-shell'
      and its parameters.
    - \"%t\" is replaced by the temporary file name produced with
      `tramp-make-tramp-temp-file'.
    - \"%k\" indicates the keep-date parameter of a program, if exists.
    - \"%c\" adds additional `tramp-ssh-controlmaster-options'
      options for the first hop.
    - \"%n\" expands to \"2>/dev/null\".
    - \"%x\" is replaced by the `tramp-scp-strict-file-name-checking'
      argument if it is supported.

    The existence of `tramp-login-args', combined with the
    absence of `tramp-copy-args', is an indication that the
    method is capable of multi-hops.

  * `tramp-async-args'
    When an asynchronous process is started, we know already that
    the connection works.  Therefore, we can pass additional
    parameters to suppress diagnostic messages, in order not to
    tamper the process output.

  * `tramp-direct-async'
    Whether the method supports direct asynchronous processes.
    Until now, just \"ssh\"-based and \"adb\"-based methods do.

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

  * `tramp-session-timeout'
    How long a Tramp connection keeps open before being disconnected.
    This is useful for methods like \"su\" or \"sudo\", which
    shouldn't run an open connection in the background forever.

  * `tramp-case-insensitive'
    Whether the remote file system handles file names case insensitive.
    Only a non-nil value counts, the default value nil means to
    perform further checks on the remote host.  See
    `tramp-connection-properties' for a way to overwrite this.

  * `tramp-mount-args'
  * `tramp-copyto-args'
  * `tramp-moveto-args'
  * `tramp-about-args'
    These parameters, a list of list like `tramp-login-args', are used
    for the \"rclone\" method, and are appended to the respective
    \"rclone\" commands.  In general, they shouldn't be changed inside
    `tramp-methods'; it is recommended to change their values via
    `tramp-connection-properties'.  Unlike `tramp-login-args' there is
     no pattern replacement.

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

All these arguments can be overwritten by connection properties.
See Info node `(tramp) Predefined connection information'.

When using `su', `sudo' or `doas' the phrase \"open connection to
a remote host\" sounds strange, but it is used nevertheless, for
consistency.  No connection is opened to a remote host, but `su',
`sudo' or `doas' is started on the local host.  You should
specify a remote host `localhost' or the name of the local host.
Another host name is useful only in combination with
`tramp-default-proxies-alist'.")

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
  :type 'string)

(defcustom tramp-default-method-alist nil
  ;; FIXME: This is not an "alist", because its elements are not of
  ;; the form (KEY . VAL) but (KEY1 KEY2 VAL).
  "Default method to use for specific host/user pairs.
This is an alist of items (HOST USER METHOD).  The first matching item
specifies the method to use for a file name which does not specify a
method.  HOST and USER are regular expressions or nil, which is
interpreted as a regular expression which always matches.  If no entry
matches, the variable `tramp-default-method' takes effect.

If the file name does not specify the user, lookup is done using the
empty string for the user name.

See `tramp-methods' for a list of possibilities for METHOD."
  :type '(repeat (list (choice :tag "Host regexp" regexp sexp)
		       (choice :tag "User regexp" regexp sexp)
		       (choice :tag "Method name" string (const nil)))))

(defconst tramp-default-method-marker "-"
  "Marker for default method in remote file names.")

(defcustom tramp-default-user nil
  "Default user to use for transferring files.
It is nil by default; otherwise settings in configuration files like
\"~/.ssh/config\" would be overwritten.  Also see `tramp-default-user-alist'.

This variable is regarded as obsolete, and will be removed soon."
  :type '(choice (const nil) string))

(defcustom tramp-default-user-alist nil
  ;; FIXME: This is not an "alist", because its elements are not of
  ;; the form (KEY . VAL) but (KEY1 KEY2 VAL).
  "Default user to use for specific method/host pairs.
This is an alist of items (METHOD HOST USER).  The first matching item
specifies the user to use for a file name which does not specify a
user.  METHOD and HOST are regular expressions or nil, which is
interpreted as a regular expression which always matches.  If no entry
matches, the variable `tramp-default-user' takes effect.

If the file name does not specify the method, lookup is done using the
empty string for the method name."
  :type '(repeat (list (choice :tag "Method regexp" regexp sexp)
		       (choice :tag "  Host regexp" regexp sexp)
		       (choice :tag "    User name" string (const nil)))))

(defcustom tramp-default-host tramp-system-name
  "Default host to use for transferring files.
Useful for su and sudo methods mostly."
  :type 'string)

(defcustom tramp-default-host-alist nil
  ;; FIXME: This is not an "alist", because its elements are not of
  ;; the form (KEY . VAL) but (KEY1 KEY2 VAL).
  "Default host to use for specific method/user pairs.
This is an alist of items (METHOD USER HOST).  The first matching item
specifies the host to use for a file name which does not specify a
host.  METHOD and USER are regular expressions or nil, which is
interpreted as a regular expression which always matches.  If no entry
matches, the variable `tramp-default-host' takes effect.

If the file name does not specify the method, lookup is done using the
empty string for the method name."
  :version "24.4"
  :type '(repeat (list (choice :tag "Method regexp" regexp sexp)
		       (choice :tag "  User regexp" regexp sexp)
		       (choice :tag "    Host name" string (const nil)))))

(defcustom tramp-default-proxies-alist nil
  ;; FIXME: This is not an "alist", because its elements are not of
  ;; the form (KEY . VAL) but (KEY1 KEY2 VAL).
  "Route to be followed for specific host/user pairs.
This is an alist of items (HOST USER PROXY).  The first matching
item specifies the proxy to be passed for a file name located on
a remote target matching USER@HOST.  HOST and USER are regular
expressions, which could also cover a domain (USER%DOMAIN) or
port (HOST#PORT).  PROXY must be a Tramp filename without a
localname part.  Method and user name on PROXY are optional,
which is interpreted with the default values.

PROXY can contain the patterns %h and %u, which are replaced by
the strings matching HOST or USER (without DOMAIN and PORT parts),
respectively.

If an entry is added while parsing ad-hoc hop definitions, PROXY
carries the non-nil text property `tramp-ad-hoc'.

HOST, USER or PROXY could also be Lisp forms, which will be
evaluated.  The result must be a string or nil, which is
interpreted as a regular expression which always matches."
  :type '(repeat (list (choice :tag "Host regexp" regexp sexp)
		       (choice :tag "User regexp" regexp sexp)
		       (choice :tag " Proxy name" string (const nil)))))

(defcustom tramp-save-ad-hoc-proxies nil
  "Whether to save ad-hoc proxies persistently."
  :version "24.3"
  :type 'boolean)

;; For some obscure technical reasons, `system-name' on w32 returns
;; either lower case or upper case letters.  See
;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=38079#20>.
(defcustom tramp-restricted-shell-hosts-alist
  (when (eq system-type 'windows-nt)
    (list (format "\\`\\(%s\\|%s\\)\\'"
		  (regexp-quote (downcase tramp-system-name))
		  (regexp-quote (upcase tramp-system-name)))))
  "List of hosts, which run a restricted shell.
This is a list of regular expressions, which denote hosts running
a restricted shell like \"rbash\".  Those hosts can be used as
proxies only, see `tramp-default-proxies-alist'.  If the local
host runs a restricted shell, it shall be added to this list, too."
  :version "27.1"
  :type '(repeat (regexp :tag "Host regexp")))

(defcustom tramp-local-host-regexp
  (concat
   "\\`"
   (regexp-opt
    (list "localhost" "localhost6" tramp-system-name "127.0.0.1" "::1") t)
   "\\'")
  "Host names which are regarded as local host.
If the local host runs a chrooted environment, set this to nil."
  :version "27.1"
  :type '(choice (const :tag "Chrooted environment" nil)
		 (regexp :tag "Host regexp")))

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
  "Regexp which matches `tramp-echo-mark' as it gets echoed by \
the remote shell.")

(defcustom tramp-local-end-of-line
  (if (eq system-type 'windows-nt) "\r\n" "\n")
  "String used for end of line in local processes."
  :version "24.1"
  :type 'string)

(defcustom tramp-rsh-end-of-line "\n"
  "String used for end of line in rsh connections.
I don't think this ever needs to be changed, so please tell me about it
if you need to change this."
  :type 'string)

(defcustom tramp-login-prompt-regexp
  ".*\\(user\\|login\\)\\( .*\\)?: *"
  "Regexp matching login-like prompts.
The regexp should match at end of buffer.

Sometimes the prompt is reported to look like \"login as:\"."
  :type 'regexp)

(defcustom tramp-shell-prompt-pattern
  ;; Allow a prompt to start right after a ^M since it indeed would be
  ;; displayed at the beginning of the line (and Zsh uses it).
  ;; Allow also [] style prompts.  They can appear only during
  ;; connection initialization; Tramp redefines the prompt afterwards.
  (concat "\\(?:^\\|\r\\)"
	  "[^]#$%>\n]*#?[]#$%>] *\\(\e\\[[[:digit:];]*[[:alpha:]] *\\)*")
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
  :type 'regexp)

(defcustom tramp-password-prompt-regexp
  (format "^.*\\(%s\\).*:\^@? *" (regexp-opt password-word-equivalents))
  "Regexp matching password-like prompts.
The regexp should match at end of buffer.

This variable is, by default, initialised from
`password-word-equivalents' when Tramp is loaded, and it is
usually more convenient to add new passphrases to that variable
instead of altering this variable.

The `sudo' program appears to insert a `^@' character into the prompt."
  :version "24.4"
  :type 'regexp)

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
	  "Received signal [[:digit:]]+"
	  "\\).*")
  "Regexp matching a `login failed' message.
The regexp should match at end of buffer."
  :type 'regexp)

(defcustom tramp-yesno-prompt-regexp
  (concat
   (regexp-opt
    '("Are you sure you want to continue connecting (yes/no)?"
      "Are you sure you want to continue connecting (yes/no/[fingerprint])?")
    t)
   "\\s-*")
  "Regular expression matching all yes/no queries which need to be confirmed.
The confirmation should be done with yes or no.
The regexp should match at end of buffer.
See also `tramp-yn-prompt-regexp'."
  :type 'regexp)

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
  :type 'regexp)

(defcustom tramp-terminal-type "dumb"
  "Value of TERM environment variable for logging in to remote host.
Because Tramp wants to parse the output of the remote shell, it is easily
confused by ANSI color escape sequences and suchlike.  Often, shell init
files conditionalize this setup based on the TERM environment variable."
  :group 'tramp
  :type 'string)

(defcustom tramp-terminal-prompt-regexp
  (concat "\\("
	  "TERM = (.*)"
	  "\\|"
	  "Terminal type\\? \\[.*\\]"
	  "\\)\\s-*")
  "Regular expression matching all terminal setting prompts.
The regexp should match at end of buffer.
The answer will be provided by `tramp-action-terminal', which see."
  :type 'regexp)

;; Plink 0.71 has added an additional anti-spoofing prompt after
;; authentication.  This could be discarded with the argument
;; "-no-antispoof".  However, since we don't know which PuTTY
;; version is installed, we must react interactively.
(defcustom tramp-antispoof-regexp
  (regexp-quote "Access granted. Press Return to begin session. ")
  "Regular expression matching plink's anti-spoofing message.
The regexp should match at end of buffer."
  :version "27.1"
  :type 'regexp)

;; A security key requires the user physically to touch the device
;; with their finger.  We must tell it to the user.
;; Added in OpenSSH 8.2.  I've tested it with yubikey.
(defcustom tramp-security-key-confirm-regexp
  "^\r*Confirm user presence for key .*[\r\n]*"
  "Regular expression matching security key confirmation message.
The regexp should match at end of buffer."
  :version "28.1"
  :type 'regexp)

(defcustom tramp-security-key-confirmed-regexp
  "^\r*User presence confirmed[\r\n]*"
  "Regular expression matching security key confirmation message.
The regexp should match at end of buffer."
  :version "28.1"
  :type 'regexp)

(defcustom tramp-security-key-timeout-regexp
  "^\r*sign_and_send_pubkey: signing failed for .*[\r\n]*"
  "Regular expression matching security key timeout message.
The regexp should match at end of buffer."
  :version "28.1"
  :type 'regexp)

(defcustom tramp-operation-not-permitted-regexp
  (concat "\\(" "preserving times.*" "\\|" "set mode" "\\)" ":\\s-*"
	  (regexp-opt '("Operation not permitted") t))
  "Regular expression matching keep-date problems in (s)cp operations.
Copying has been performed successfully already, so this message can
be ignored safely."
  :type 'regexp)

(defcustom tramp-copy-failed-regexp
  (concat "\\(.+: "
          (regexp-opt '("Permission denied"
                        "not a regular file"
                        "is a directory"
                        "No such file or directory")
                      t)
          "\\)\\s-*")
  "Regular expression matching copy problems in (s)cp operations."
  :type 'regexp)

(defcustom tramp-process-alive-regexp
  ""
  "Regular expression indicating a process has finished.
In fact this expression is empty by intention, it will be used only to
check regularly the status of the associated process.
The answer will be provided by `tramp-action-process-alive',
`tramp-action-out-of-band', which see."
  :type 'regexp)

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

(defcustom tramp-syntax 'default
  "Tramp filename syntax to be used.

It can have the following values:

  `default'    -- Default syntax
  `simplified' -- Ange-FTP like syntax
  `separate'   -- Syntax as defined for XEmacs originally

Do not change the value by `setq', it must be changed only via
Customize.  See also `tramp-change-syntax'."
  :version "26.1"
  :package-version '(Tramp . "2.3.3")
  :type '(choice (const :tag "Default" default)
		 (const :tag "Ange-FTP" simplified)
		 (const :tag "XEmacs" separate))
  :require 'tramp
  :initialize #'custom-initialize-default
  :set #'tramp-set-syntax)

(defun tramp-set-syntax (symbol value)
  "Set SYMBOL to value VALUE.
Used in user option `tramp-syntax'.  There are further variables
to be set, depending on VALUE."
  ;; Check allowed values.
  (unless (memq value (tramp-syntax-values))
    (tramp-user-error nil "Wrong `tramp-syntax' %s" value))
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
(tramp--with-startup
  (tramp-set-syntax 'tramp-syntax (tramp-compat-tramp-syntax)))

(defun tramp-syntax-values ()
  "Return possible values of `tramp-syntax', a list."
  (let ((values (cdr (get 'tramp-syntax 'custom-type))))
    (setq values (mapcar #'last values)
	  values (mapcar #'car values))
    values))

(defun tramp-lookup-syntax (alist)
  "Look up a syntax string in ALIST according to `tramp-compat-tramp-syntax'.
Raise an error if `tramp-syntax' is invalid."
  (or (cdr (assq (tramp-compat-tramp-syntax) alist))
      (error "Wrong `tramp-syntax' %s" tramp-syntax)))

(defconst tramp-prefix-format-alist
  '((default    . "/")
    (simplified . "/")
    (separate   . "/["))
  "Alist mapping Tramp syntax to strings beginning Tramp file names.")

(defun tramp-build-prefix-format ()
  "Return `tramp-prefix-format' according to `tramp-syntax'."
  (tramp-lookup-syntax tramp-prefix-format-alist))

(defvar tramp-prefix-format nil ;Initialized when defining `tramp-syntax'!
  "String matching the very beginning of Tramp file names.
Used in `tramp-make-tramp-file-name'.")

(defun tramp-build-prefix-regexp ()
  "Return `tramp-prefix-regexp'."
  (concat "^" (regexp-quote tramp-prefix-format)))

(defvar tramp-prefix-regexp nil ;Initialized when defining `tramp-syntax'!
  "Regexp matching the very beginning of Tramp file names.
Should always start with \"^\".  Derived from `tramp-prefix-format'.")

(defconst tramp-method-regexp-alist
  '((default    . "[[:alnum:]-]+")
    (simplified . "")
    (separate   . "[[:alnum:]-]*"))
  "Alist mapping Tramp syntax to regexps matching methods identifiers.")

(defun tramp-build-method-regexp ()
  "Return `tramp-method-regexp' according to `tramp-syntax'."
  (tramp-lookup-syntax tramp-method-regexp-alist))

(defvar tramp-method-regexp nil ;Initialized when defining `tramp-syntax'!
  "Regexp matching methods identifiers.
The `ftp' syntax does not support methods.")

(defconst tramp-postfix-method-format-alist
  '((default    . ":")
    (simplified . "")
    (separate   . "/"))
  "Alist mapping Tramp syntax to the delimiter after the method.")

(defun tramp-build-postfix-method-format ()
  "Return `tramp-postfix-method-format' according to `tramp-syntax'."
  (tramp-lookup-syntax tramp-postfix-method-format-alist))

(defvar tramp-postfix-method-format nil ;Init'd when defining `tramp-syntax'!
  "String matching delimiter between method and user or host names.
The `ftp' syntax does not support methods.
Used in `tramp-make-tramp-file-name'.")

(defun tramp-build-postfix-method-regexp ()
  "Return `tramp-postfix-method-regexp'."
  (regexp-quote tramp-postfix-method-format))

(defvar tramp-postfix-method-regexp nil ;Init'd when defining `tramp-syntax'!
  "Regexp matching delimiter between method and user or host names.
Derived from `tramp-postfix-method-format'.")

(defconst tramp-user-regexp "[^/|: \t]+"
  "Regexp matching user names.")

(defconst tramp-prefix-domain-format "%"
  "String matching delimiter between user and domain names.")

(defconst tramp-prefix-domain-regexp (regexp-quote tramp-prefix-domain-format)
  "Regexp matching delimiter between user and domain names.
Derived from `tramp-prefix-domain-format'.")

(defconst tramp-domain-regexp "[[:alnum:]_.-]+"
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

(defconst tramp-host-regexp "[[:alnum:]_.%-]+"
  "Regexp matching host names.")

(defconst tramp-prefix-ipv6-format-alist
  '((default    . "[")
    (simplified . "[")
    (separate   . ""))
  "Alist mapping Tramp syntax to strings prefixing IPv6 addresses.")

(defun tramp-build-prefix-ipv6-format ()
  "Return `tramp-prefix-ipv6-format' according to `tramp-syntax'."
  (tramp-lookup-syntax tramp-prefix-ipv6-format-alist))

(defvar tramp-prefix-ipv6-format nil ;Initialized when defining `tramp-syntax'!
  "String matching left hand side of IPv6 addresses.
Used in `tramp-make-tramp-file-name'.")

(defun tramp-build-prefix-ipv6-regexp ()
  "Return `tramp-prefix-ipv6-regexp'."
  (regexp-quote tramp-prefix-ipv6-format))

(defvar tramp-prefix-ipv6-regexp nil ;Initialized when defining `tramp-syntax'!
  "Regexp matching left hand side of IPv6 addresses.
Derived from `tramp-prefix-ipv6-format'.")

;; The following regexp is a bit sloppy.  But it shall serve our
;; purposes.  It covers also IPv4 mapped IPv6 addresses, like in
;; "::ffff:192.168.0.1".
(defconst tramp-ipv6-regexp "\\(?:[[:alnum:]]*:\\)+[[:alnum:].]+"
  "Regexp matching IPv6 addresses.")

(defconst tramp-postfix-ipv6-format-alist
  '((default    . "]")
    (simplified . "]")
    (separate   . ""))
  "Alist mapping Tramp syntax to suffix for IPv6 addresses.")

(defun tramp-build-postfix-ipv6-format ()
  "Return `tramp-postfix-ipv6-format' according to `tramp-syntax'."
  (tramp-lookup-syntax tramp-postfix-ipv6-format-alist))

(defvar tramp-postfix-ipv6-format nil ;Initialized when defining `tramp-syntax'!
  "String matching right hand side of IPv6 addresses.
Used in `tramp-make-tramp-file-name'.")

(defun tramp-build-postfix-ipv6-regexp ()
  "Return `tramp-postfix-ipv6-regexp'."
  (regexp-quote tramp-postfix-ipv6-format))

(defvar tramp-postfix-ipv6-regexp nil ;Initialized when defining `tramp-syntax'!
  "Regexp matching right hand side of IPv6 addresses.
Derived from `tramp-postfix-ipv6-format'.")

(defconst tramp-prefix-port-format "#"
  "String matching delimiter between host names and port numbers.")

(defconst tramp-prefix-port-regexp (regexp-quote tramp-prefix-port-format)
  "Regexp matching delimiter between host names and port numbers.
Derived from `tramp-prefix-port-format'.")

(defconst tramp-port-regexp "[[:digit:]]+"
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
  "Return `tramp-postfix-host-format' according to `tramp-syntax'."
  (tramp-lookup-syntax tramp-postfix-host-format-alist))

(defvar tramp-postfix-host-format nil ;Initialized when defining `tramp-syntax'!
  "String matching delimiter between host names and localnames.
Used in `tramp-make-tramp-file-name'.")

(defun tramp-build-postfix-host-regexp ()
  "Return `tramp-postfix-host-regexp'."
  (regexp-quote tramp-postfix-host-format))

(defvar tramp-postfix-host-regexp nil ;Initialized when defining `tramp-syntax'!
  "Regexp matching delimiter between host names and localnames.
Derived from `tramp-postfix-host-format'.")

(defconst tramp-localname-regexp "[^\n\r]*\\'"
  "Regexp matching localnames.")

(defconst tramp-unknown-id-string "UNKNOWN"
  "String used to denote an unknown user or group.")

(defconst tramp-unknown-id-integer -1
  "Integer used to denote an unknown user or group.")

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
   nil ;Initialized when defining `tramp-syntax'!
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

(defvar tramp-file-name-structure nil ;Initialized when defining `tramp-syntax'!
  "List detailing the Tramp file name structure.
This is a list of six elements (REGEXP METHOD USER HOST FILE HOP).

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
  "Return `tramp-file-name-regexp'."
  (car tramp-file-name-structure))

;;;###autoload
(defconst tramp-initial-file-name-regexp "\\`/[^/:]+:[^/:]*:"
  "Value for `tramp-file-name-regexp' for autoload.
It must match the initial `tramp-syntax' settings.")

;;;###autoload
(defvar tramp-file-name-regexp tramp-initial-file-name-regexp
  "Regular expression matching file names handled by Tramp.
This regexp should match Tramp file names but no other file
names.  When calling `tramp-register-file-name-handlers', the
initial value is overwritten by the car of `tramp-file-name-structure'.")

;;;###autoload
(defcustom tramp-ignored-file-name-regexp nil
  "Regular expression matching file names that are not under Tramp's control."
  :version "27.1"
  :type '(choice (const nil) regexp))

(defconst tramp-completion-file-name-regexp-default
  (concat
   "\\`"
   ;; `file-name-completion' uses absolute paths for matching.  This
   ;; means that on W32 systems, something like "/ssh:host:~/path"
   ;; becomes "c:/ssh:host:~/path".  See also `tramp-drop-volume-letter'.
   (when (eq system-type 'windows-nt)
       "\\(?:[[:alpha:]]:\\)?")
   "/\\("
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
   "\\`"
   ;; Allow the volume letter at the beginning of the path.  See the
   ;; comment in `tramp-completion-file-name-regexp-default' for more
   ;; details.
   (when (eq system-type 'windows-nt)
     "\\(?:[[:alpha:]]:\\)?")
   "/\\("
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
  (concat
   "\\`"
   ;; Allow the volume letter at the beginning of the path.  See the
   ;; comment in `tramp-completion-file-name-regexp-default' for more
   ;; details.
   (when (eq system-type 'windows-nt)
     "\\(?:[[:alpha:]]:\\)?")
   "/\\(\\[[^]]*\\)?\\'")
  "Value for `tramp-completion-file-name-regexp' for separate remoting.
See `tramp-file-name-structure' for more explanations.")

(defconst tramp-completion-file-name-regexp-alist
  `((default    . ,tramp-completion-file-name-regexp-default)
    (simplified . ,tramp-completion-file-name-regexp-simplified)
    (separate   . ,tramp-completion-file-name-regexp-separate))
  "Alist mapping incomplete Tramp file names.")

(defun tramp-build-completion-file-name-regexp ()
  "Return `tramp-completion-file-name-regexp' according to `tramp-syntax'."
  (tramp-lookup-syntax tramp-completion-file-name-regexp-alist))

(defvar tramp-completion-file-name-regexp
   nil ;Initialized when defining `tramp-syntax'!
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
the `*scratch*' buffer and then hitting `C-j' with the cursor after the
last closing parenthesis.  Note that it works only if you have configured
\"ssh\" to run without password query, see ssh-agent(1).

You will see the number of bytes sent successfully to the remote host.
If that number exceeds 1000, you can stop the execution by hitting
`C-g', because your Emacs is likely clean.

When it is necessary to set `tramp-chunksize', you might consider to
use an out-of-the-band method \(like \"scp\") instead of an internal one
\(like \"ssh\"), because setting `tramp-chunksize' to non-nil decreases
performance.

If your Emacs is buggy, the code stops and gives you an indication
about the value `tramp-chunksize' should be set.  Maybe you could just
experiment a bit, e.g. changing the values of `init' and `step'
in the third line of the code.

Please raise a bug report via \\[tramp-bug] if your system needs
this variable to be set as well."
  :type '(choice (const nil) integer))

;; Logging in to a remote host normally requires obtaining a pty.  But
;; Emacs on macOS has `process-connection-type' set to nil by default,
;; so on those systems Tramp doesn't obtain a pty.  Here, we allow
;; for an override of the system default.
(defcustom tramp-process-connection-type t
  "Overrides `process-connection-type' for connections from Tramp.
Tramp binds `process-connection-type' to the value given here before
opening a connection to a remote host."
  :type '(choice (const nil) (const t) (const pipe) (const pty)))

(defcustom tramp-connection-timeout 60
  "Defines the max time to wait for establishing a connection (in seconds).
This can be overwritten for different connection types in `tramp-methods'.

The timeout does not include the time reading a password."
  :version "24.4"
  :type 'integer)

(defcustom tramp-connection-min-time-diff 5
  "Defines seconds between two consecutive connection attempts.
This is necessary as self defense mechanism, in order to avoid
yo-yo connection attempts when the remote host is unavailable.

A value of 0 or nil suppresses this check.  This might be
necessary, when several out-of-order copy operations are
performed, or when several asynchronous processes will be started
in a short time frame.  In those cases it is recommended to
let-bind this variable."
  :version "24.4"
  :type '(choice (const nil) integer))

;; "getconf PATH" yields:
;; HP-UX: /usr/bin:/usr/ccs/bin:/opt/ansic/bin:/opt/langtools/bin:/opt/fortran/bin
;; Solaris: /usr/xpg4/bin:/usr/ccs/bin:/usr/bin:/opt/SUNWspro/bin
;; GNU/Linux (Debian, Suse, RHEL, Cygwin, MINGW64): /bin:/usr/bin
;; FreeBSD, DragonFly: /usr/bin:/bin:/usr/sbin:/sbin: - beware trailing ":"!
;; FreeBSD 12.1, Darwin: /usr/bin:/bin:/usr/sbin:/sbin
;; IRIX64: /usr/bin
;; QNAP QTS: ---
;; Hydra: /run/current-system/sw/bin:/bin:/usr/bin
(defcustom tramp-remote-path
  '(tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin"
    "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin"
    "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin"
    "/opt/bin" "/opt/sbin" "/opt/local/bin")
  "List of directories to search for executables on remote host.
For every remote host, this variable will be set buffer local,
keeping the list of existing directories on that host.

You can use \"~\" in this list, but when searching for a shell which groks
tilde expansion, all directory names starting with \"~\" will be ignored.

`Default Directories' represent the list of directories given by
the command \"getconf PATH\".  It is recommended to use this
entry on head of this list, because these are the default
directories for POSIX compatible commands.  On remote hosts which
do not offer the getconf command, the value \"/bin:/usr/bin\" is
used instead.  This entry is represented in the list by the
special value `tramp-default-remote-path'.

`Private Directories' are the settings of the $PATH environment,
as given in your `~/.profile'.  This entry is represented in
the list by the special value `tramp-own-remote-path'."
  :group 'tramp
  :type '(repeat (choice
		  (const :tag "Default Directories" tramp-default-remote-path)
		  (const :tag "Private Directories" tramp-own-remote-path)
		  (string :tag "Directory"))))

(defcustom tramp-remote-process-environment
  '("ENV=''" "TMOUT=0" "LC_CTYPE=''"
    "CDPATH=" "HISTORY=" "MAIL=" "MAILCHECK=" "MAILPATH=" "PAGER=cat"
    "autocorrect=" "correct=")
  "List of environment variables to be set on the remote host.

Each element should be a string of the form ENVVARNAME=VALUE.  An
entry ENVVARNAME= disables the corresponding environment variable,
which might have been set in the init files like ~/.profile.

Special handling is applied to some environment variables,
which should not be set here:

The PATH environment variable should be set via `tramp-remote-path'.

The TERM environment variable should be set via `tramp-terminal-type'.

The INSIDE_EMACS environment variable will automatically be set
based on the Tramp and Emacs versions, and should not be set here."
  :group 'tramp
  :version "26.1"
  :type '(repeat string))

(defcustom tramp-completion-reread-directory-timeout 10
  "Defines seconds since last remote command before rereading a directory.
A remote directory might have changed its contents.  In order to
make it visible during file name completion in the minibuffer,
Tramp flushes its cache and rereads the directory contents when
more than `tramp-completion-reread-directory-timeout' seconds
have been gone since last remote command execution.  A value of t
would require an immediate reread during filename completion, nil
means to use always cached values for the directory contents."
  :type '(choice (const nil) (const t) integer))
(make-obsolete-variable
 'tramp-completion-reread-directory-timeout 'remote-file-name-inhibit-cache "27.2")

;;; Internal Variables:

(defvar tramp-current-connection nil
  "Last connection timestamp.
It is a cons cell of the actual `tramp-file-name-structure', and
the (optional) timestamp of last activity on this connection.")

(defvar tramp-password-save-function nil
  "Password save function.
Will be called once the password has been verified by successful
authentication.")
(put 'tramp-password-save-function 'tramp-suppress-trace t)

(defconst tramp-completion-file-name-handler-alist
  '((file-name-all-completions
     . tramp-completion-handle-file-name-all-completions)
    (file-name-completion . tramp-completion-handle-file-name-completion))
  "Alist of completion handler functions.
Used for file names matching `tramp-completion-file-name-regexp'.
Operations not mentioned here will be handled by Tramp's file
name handler functions, or the normal Emacs functions.")

;; Handlers for foreign methods, like FTP or SMB, shall be plugged here.
(defvar tramp-foreign-file-name-handler-alist nil
  "Alist of elements (FUNCTION . HANDLER) for foreign methods handled specially.
If (FUNCTION FILENAME) returns non-nil, then all I/O on that file is done by
calling HANDLER.")

;;; Internal functions which must come first:

;; Conversion functions between external representation and
;; internal data structure.  Convenience functions for internal
;; data structure.

;; The basic structure for remote file names.  We use a list :type,
;; in order to be compatible with Emacs 25.
(cl-defstruct (tramp-file-name (:type list) :named)
  method user domain host port localname hop)

(put #'tramp-file-name-method 'tramp-suppress-trace t)
(put #'tramp-file-name-user 'tramp-suppress-trace t)
(put #'tramp-file-name-domain 'tramp-suppress-trace t)
(put #'tramp-file-name-host 'tramp-suppress-trace t)
(put #'tramp-file-name-port 'tramp-suppress-trace t)
(put #'tramp-file-name-localname 'tramp-suppress-trace t)
(put #'tramp-file-name-hop 'tramp-suppress-trace t)

(defun tramp-file-name-user-domain (vec)
  "Return user and domain components of VEC."
  (when (or (tramp-file-name-user vec) (tramp-file-name-domain vec))
    (concat (tramp-file-name-user vec)
	    (and (tramp-file-name-domain vec)
		 tramp-prefix-domain-format)
	    (tramp-file-name-domain vec))))

(put #'tramp-file-name-user-domain 'tramp-suppress-trace t)

(defun tramp-file-name-host-port (vec)
  "Return host and port components of VEC."
  (when (or (tramp-file-name-host vec) (tramp-file-name-port vec))
    (concat (tramp-file-name-host vec)
	    (and (tramp-file-name-port vec)
		 tramp-prefix-port-format)
	    (tramp-file-name-port vec))))

(put #'tramp-file-name-host-port 'tramp-suppress-trace t)

(defun tramp-file-name-port-or-default (vec)
  "Return port component of VEC.
If nil, return `tramp-default-port'."
  (or (tramp-file-name-port vec)
      (tramp-get-method-parameter vec 'tramp-default-port)))

(put #'tramp-file-name-port-or-default 'tramp-suppress-trace t)

(defun tramp-file-name-unify (vec)
  "Unify VEC by removing localname and hop from `tramp-file-name' structure.
Objects returned by this function compare `equal' if they refer to the
same connection.  Make a copy in order to avoid side effects."
  (when (tramp-file-name-p vec)
    (setq vec (copy-tramp-file-name vec))
    (setf (tramp-file-name-localname vec) nil
	  (tramp-file-name-hop vec) nil))
  vec)

(put #'tramp-file-name-unify 'tramp-suppress-trace t)

;; Comparison of file names is performed by `tramp-equal-remote'.
(defun tramp-file-name-equal-p (vec1 vec2)
  "Check, whether VEC1 and VEC2 denote the same `tramp-file-name'."
  (and (tramp-file-name-p vec1) (tramp-file-name-p vec2)
       (equal (tramp-file-name-unify vec1)
	      (tramp-file-name-unify vec2))))

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
      (when-let ((methods-entry
		  (assoc
		   param (assoc (tramp-file-name-method vec) tramp-methods))))
	(cadr methods-entry)))))

;; The localname can be quoted with "/:".  Extract this.
(defun tramp-file-name-unquote-localname (vec)
  "Return unquoted localname component of VEC."
  (tramp-compat-file-name-unquote (tramp-file-name-localname vec)))

(defun tramp-tramp-file-p (name)
  "Return t if NAME is a string with Tramp file name syntax."
  (and tramp-mode (stringp name)
       ;; No "/:" and "/c:".  This is not covered by `tramp-file-name-regexp'.
       (not (string-match-p
	     (if (memq system-type '(cygwin windows-nt))
		 "^/[[:alpha:]]?:" "^/:")
	     name))
       ;; Excluded file names.
       (or (null tramp-ignored-file-name-regexp)
	   (not (string-match-p tramp-ignored-file-name-regexp name)))
       (string-match-p tramp-file-name-regexp name)
       t))

(put #'tramp-tramp-file-p 'tramp-suppress-trace t)

;; This function bypasses the file name handler approach.  It is NOT
;; recommended to use it in any package if not absolutely necessary.
;; However, it is more performant than `file-local-name', and might be
;; useful where performance matters, like in operations over a bulk
;; list of file names.
(defun tramp-file-local-name (name)
  "Return the local name component of NAME.
This function removes from NAME the specification of the remote
host and the method of accessing the host, leaving only the part
that identifies NAME locally on the remote system.  If NAME does
not match `tramp-file-name-regexp', just `file-local-name' is
called.  The returned file name can be used directly as argument
of `process-file', `start-file-process', or `shell-command'."
  (or (and (tramp-tramp-file-p name)
           (string-match (nth 0 tramp-file-name-structure) name)
           (match-string (nth 4 tramp-file-name-structure) name))
      (tramp-compat-file-local-name name)))

;; The localname can be quoted with "/:".  Extract this.
(defun tramp-unquote-file-local-name (name)
  "Return unquoted localname of NAME."
  (tramp-compat-file-name-unquote (tramp-file-local-name name)))

(defun tramp-find-method (method user host)
  "Return the right method string to use depending on USER and HOST.
This is METHOD, if non-nil.  Otherwise, do a lookup in
`tramp-default-method-alist' and `tramp-default-method'."
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
		 (when (and (string-match-p (or (nth 0 item) "") (or host ""))
			    (string-match-p (or (nth 1 item) "") (or user "")))
		   (setq lmethod (nth 2 item)
			 choices nil)))
	       lmethod)
	     tramp-default-method)))
    ;; We must mark, whether a default value has been used.
    (if (or method (null result))
	result
      (propertize result 'tramp-default t))))

(put #'tramp-find-method 'tramp-suppress-trace t)

(defun tramp-find-user (method user host)
  "Return the right user string to use depending on METHOD and HOST.
This is USER, if non-nil.  Otherwise, do a lookup in
`tramp-default-user-alist' and `tramp-default-user'."
  (let ((result
	 (or user
	     (let ((choices tramp-default-user-alist)
		   luser item)
	       (while choices
		 (setq item (pop choices))
		 (when (and (string-match-p (or (nth 0 item) "") (or method ""))
			    (string-match-p (or (nth 1 item) "") (or host "")))
		   (setq luser (nth 2 item)
			 choices nil)))
	       luser)
	     tramp-default-user)))
    ;; We must mark, whether a default value has been used.
    (if (or user (null result))
	result
      (propertize result 'tramp-default t))))

(put #'tramp-find-user 'tramp-suppress-trace t)

(defun tramp-find-host (method user host)
  "Return the right host string to use depending on METHOD and USER.
This is HOST, if non-nil.  Otherwise, do a lookup in
`tramp-default-host-alist' and `tramp-default-host'."
  (let ((result
	 (or (and (> (length host) 0) host)
	     (let ((choices tramp-default-host-alist)
		   lhost item)
	       (while choices
		 (setq item (pop choices))
		 (when (and (string-match-p (or (nth 0 item) "") (or method ""))
			    (string-match-p (or (nth 1 item) "") (or user "")))
		   (setq lhost (nth 2 item)
			 choices nil)))
	       lhost)
	     tramp-default-host)))
    ;; We must mark, whether a default value has been used.
    (if (or (> (length host) 0) (null result))
	result
      (propertize result 'tramp-default t))))

(put #'tramp-find-host 'tramp-suppress-trace t)

(defun tramp-dissect-file-name (name &optional nodefault)
  "Return a `tramp-file-name' structure of NAME, a remote file name.
The structure consists of method, user, domain, host, port,
localname (file name on remote host), and hop.

Unless NODEFAULT is non-nil, method, user and host are expanded
to their default values.  For the other file name parts, no
default values are used."
  (save-match-data
    (unless (tramp-tramp-file-p name)
      (tramp-user-error nil "Not a Tramp file name: \"%s\"" name))
    (if (not (string-match (nth 0 tramp-file-name-structure) name))
        (error "`tramp-file-name-structure' didn't match!")
      (let ((method    (match-string (nth 1 tramp-file-name-structure) name))
	    (user      (match-string (nth 2 tramp-file-name-structure) name))
	    (host      (match-string (nth 3 tramp-file-name-structure) name))
	    (localname (match-string (nth 4 tramp-file-name-structure) name))
	    (hop       (match-string (nth 5 tramp-file-name-structure) name))
	    domain port v)
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
	  (when hop
	    (setq v (tramp-dissect-hop-name hop)
		  hop (and hop (tramp-make-tramp-hop-name v))))
	  (let ((tramp-default-host
		 (or (and v (not (tramp-compat-string-search
				  "%h" (tramp-file-name-host v)))
			  (tramp-file-name-host v))
		     tramp-default-host)))
	    (setq method (tramp-find-method method user host)
		  user (tramp-find-user method user host)
		  host (tramp-find-host method user host)
		  hop
		  (and hop
		       (format-spec hop (format-spec-make ?h host ?u user))))))

	;; Return result.
	(prog1
	    (setq v (make-tramp-file-name
		     :method method :user user :domain domain :host host
		     :port port :localname localname :hop hop))
	  ;; The method must be known.
	  (unless (or nodefault non-essential
		      (string-equal method tramp-default-method-marker)
		      (assoc method tramp-methods))
	    (tramp-user-error
	     v "Method `%s' is not known." method))
	  ;; Only some methods from tramp-sh.el do support multi-hops.
	  (unless (or (null hop) nodefault non-essential (tramp-multi-hop-p v))
	    (tramp-user-error
	     v "Method `%s' is not supported for multi-hops." method)))))))

(put #'tramp-dissect-file-name 'tramp-suppress-trace t)

(defun tramp-dissect-hop-name (name &optional nodefault)
  "Return a `tramp-file-name' structure of `hop' part of NAME.
See `tramp-dissect-file-name' for details."
  (let ((v (tramp-dissect-file-name
	    (concat tramp-prefix-format
		    (replace-regexp-in-string
		     (concat tramp-postfix-hop-regexp "$")
		     tramp-postfix-host-format name))
	    nodefault)))
    ;; Only some methods from tramp-sh.el do support multi-hops.
    (unless (or nodefault non-essential (tramp-multi-hop-p v))
      (tramp-user-error
       v "Method `%s' is not supported for multi-hops."
       (tramp-file-name-method v)))
    ;; Return result.
    v))

(put #'tramp-dissect-hop-name 'tramp-suppress-trace t)

(defun tramp-buffer-name (vec)
  "A name for the connection buffer VEC."
  (let ((method (tramp-file-name-method vec))
	(user-domain (tramp-file-name-user-domain vec))
	(host-port (tramp-file-name-host-port vec)))
    (if (not (zerop (length user-domain)))
	(format "*tramp/%s %s@%s*" method user-domain host-port)
      (format "*tramp/%s %s*" method host-port))))

(put #'tramp-buffer-name 'tramp-suppress-trace t)

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

    ;; Unless `tramp-syntax' is `simplified', we need a method.
    (when (and (not (zerop (length tramp-postfix-method-format)))
	       (zerop (length method)))
      (signal 'wrong-type-argument (list #'stringp method)))
    (concat tramp-prefix-format hop
	    (unless (zerop (length tramp-postfix-method-format))
	      (concat method tramp-postfix-method-format))
	    user
	    (unless (zerop (length domain))
	      (concat tramp-prefix-domain-format domain))
	    (unless (zerop (length user))
	      tramp-postfix-user-format)
	    (when host
	      (if (string-match-p tramp-ipv6-regexp host)
		  (concat
		   tramp-prefix-ipv6-format host tramp-postfix-ipv6-format)
		host))
	    (unless (zerop (length port))
	      (concat tramp-prefix-port-format port))
	    tramp-postfix-host-format
	    localname)))

(set-advertised-calling-convention
 #'tramp-make-tramp-file-name '(vec &optional localname hop) "27.1")

(defun tramp-make-tramp-hop-name (vec)
  "Construct a Tramp hop name from VEC."
  (replace-regexp-in-string
   tramp-prefix-regexp ""
   (replace-regexp-in-string
    (concat tramp-postfix-host-regexp "$") tramp-postfix-hop-format
    (tramp-make-tramp-file-name vec 'noloc))))

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
	     (if (string-match-p tramp-ipv6-regexp host)
		 (concat
		  tramp-prefix-ipv6-format host tramp-postfix-ipv6-format)
	       host)
	     tramp-postfix-host-format))
	  localname))

(defun tramp-get-buffer (vec &optional dont-create)
  "Get the connection buffer to be used for VEC.
Unless DONT-CREATE, the buffer is created when it doesn't exist yet."
  (or (get-buffer (tramp-buffer-name vec))
      (unless dont-create
	(with-current-buffer (get-buffer-create (tramp-buffer-name vec))
	  ;; We use the existence of connection property "process-buffer"
	  ;; as indication, whether a connection is active.
	  (tramp-set-connection-property
	   vec "process-buffer"
	   (tramp-get-connection-property vec "process-buffer" nil))
	  (setq buffer-undo-list t
		default-directory
		(tramp-make-tramp-file-name vec 'noloc 'nohop))
	  (current-buffer)))))

(defun tramp-get-connection-buffer (vec &optional dont-create)
  "Get the connection buffer to be used for VEC.
Unless DONT-CREATE, the buffer is created when it doesn't exist yet.
In case a second asynchronous communication has been started, it is different
from `tramp-get-buffer'."
  (or (tramp-get-connection-property vec "process-buffer" nil)
      (tramp-get-buffer vec dont-create)))

(defun tramp-get-connection-name (vec)
  "Get the connection name to be used for VEC.
In case a second asynchronous communication has been started, it is different
from the default one."
  (or (tramp-get-connection-property vec "process-name" nil)
      (tramp-buffer-name vec)))

(defun tramp-get-process (vec-or-proc)
  "Get the default connection process to be used for VEC-OR-PROC.
Return `tramp-cache-undefined' in case it doesn't exist."
  (or (and (tramp-file-name-p vec-or-proc)
	   (get-buffer-process (tramp-buffer-name vec-or-proc)))
      (and (processp vec-or-proc)
	   (tramp-get-process (process-get vec-or-proc 'vector)))
      tramp-cache-undefined))

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
  (when (tramp-tramp-file-p default-directory)
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

(put #'tramp-debug-buffer-name 'tramp-suppress-trace t)

(defconst tramp-debug-outline-regexp
  (concat
   "[[:digit:]]+:[[:digit:]]+:[[:digit:]]+\\.[[:digit:]]+ " ;; Timestamp.
   "\\(?:\\(#<thread .+>\\) \\)?" ;; Thread.
   "[[:alnum:]-]+ (\\([[:digit:]]+\\)) #") ;; Function name, verbosity.
  "Used for highlighting Tramp debug buffers in `outline-mode'.")

(defconst tramp-debug-font-lock-keywords
  ;; FIXME: Make it a function instead of an ELisp expression, so you
  ;; can evaluate it with `funcall' rather than `eval'!
  ;; Also, in `font-lock-defaults' you can specify a function name for
  ;; the "KEYWORDS" part, so font-lock calls it to get the actual keywords!
  '(list
    (concat "^\\(?:" tramp-debug-outline-regexp "\\).+")
    '(1 font-lock-warning-face t t)
    '(0 (outline-font-lock-face) keep t))
  "Used for highlighting Tramp debug buffers in `outline-mode'.")

(defun tramp-debug-outline-level ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line.

The outline level is equal to the verbosity of the Tramp message."
  (1+ (string-to-number (match-string 2))))

(put #'tramp-debug-outline-level 'tramp-suppress-trace t)

(defun tramp-get-debug-buffer (vec)
  "Get the debug buffer for VEC."
  (with-current-buffer (get-buffer-create (tramp-debug-buffer-name vec))
    (when (bobp)
      (set-buffer-file-coding-system 'utf-8)
      (setq buffer-undo-list t)
      ;; Activate `outline-mode'.  This runs `text-mode-hook' and
      ;; `outline-mode-hook'.  We must prevent that local processes
      ;; die.  Yes: I've seen `flyspell-mode', which starts "ispell".
      ;; `(custom-declare-variable outline-minor-mode-prefix ...)'
      ;; raises on error in `(outline-mode)', we don't want to see it
      ;; in the traces.
      (let ((default-directory tramp-compat-temporary-file-directory))
	(outline-mode))
      (setq-local outline-level 'tramp-debug-outline-level)
      (setq-local font-lock-keywords
                  ;; FIXME: This `(t FOO . BAR)' representation in
                  ;; `font-lock-keywords' is supposed to be an
                  ;; internal implementation "detail".  Don't abuse it here!
                  `(t (eval ,tramp-debug-font-lock-keywords t)
                      ,(eval tramp-debug-font-lock-keywords t)))
      ;; Do not edit the debug buffer.
      (use-local-map special-mode-map))
    (current-buffer)))

(put #'tramp-get-debug-buffer 'tramp-suppress-trace t)

(defun tramp-get-debug-file-name (vec)
  "Get the debug file name for VEC."
  (expand-file-name
   (tramp-compat-string-replace "/" " " (tramp-debug-buffer-name vec))
   tramp-compat-temporary-file-directory))

(put #'tramp-get-debug-file-name 'tramp-suppress-trace t)

(defun tramp-trace-buffer-name (vec)
  "A name for the trace buffer for VEC."
  (tramp-compat-string-replace "debug" "trace" (tramp-debug-buffer-name vec)))

(put #'tramp-trace-buffer-name 'tramp-suppress-trace t)

(defvar tramp-trace-functions nil
  "A list of non-Tramp functions to be traced with `tramp-verbose' > 10.")

(defun tramp-debug-message (vec fmt-string &rest arguments)
  "Append message to debug buffer of VEC.
Message is formatted with FMT-STRING as control string and the remaining
ARGUMENTS to actually emit the message (if applicable)."
  (let ((inhibit-message t)
	create-lockfiles file-name-handler-alist message-log-max
	signal-hook-function)
    (with-current-buffer (tramp-get-debug-buffer vec)
      (goto-char (point-max))
      (let ((point (point)))
	(when (bobp)
	  ;; Headline.
	  (insert
	   (format
	    ";; Emacs: %s Tramp: %s -*- mode: outline; coding: utf-8; -*-"
	    emacs-version tramp-version))
	  (when (>= tramp-verbose 10)
	    (let ((tramp-verbose 0))
	      (insert
	       (format
		"\n;; Location: %s Git: %s/%s"
		(locate-library "tramp")
		(or tramp-repository-branch "")
		(or tramp-repository-version "")))))
	  ;; Traces.
	  (when (>= tramp-verbose 11)
	    (dolist
		(elt
		 (append
		  (mapcar
		   #'intern (all-completions "tramp-" obarray #'functionp))
		  tramp-trace-functions))
	      (unless (get elt 'tramp-suppress-trace)
		(trace-function-background elt))))
	  ;; Delete debug file.
	  (when (and tramp-debug-to-file (tramp-get-debug-file-name vec))
	    (ignore-errors (delete-file (tramp-get-debug-file-name vec)))))
	(unless (bolp)
	  (insert "\n"))
	;; Timestamp.
	(let ((now (current-time)))
	  (insert (format-time-string "%T." now))
	  (insert (format "%06d " (nth 2 now))))
	;; Calling Tramp function.  We suppress compat and trace
	;; functions from being displayed.
	(let ((btn 1) btf fn)
	  (while (not fn)
	    (setq btf (nth 1 (backtrace-frame btn)))
	    (if (not btf)
		(setq fn "")
	      (and (symbolp btf) (setq fn (symbol-name btf))
		   (or (not (string-prefix-p "tramp" fn))
		       (get btf 'tramp-suppress-trace))
		   (setq fn nil))
	      (setq btn (1+ btn))))
	  ;; The following code inserts filename and line number.
	  ;; Should be inactive by default, because it is time consuming.
	  ;; (let ((ffn (find-function-noselect (intern fn))))
	  ;;   (insert
	  ;;    (format
	  ;;     "%s:%d: "
	  ;;     (file-name-nondirectory (buffer-file-name (car ffn)))
	  ;;     (with-current-buffer (car ffn)
	  ;;       (1+ (count-lines (point-min) (cdr ffn)))))))
	  (insert (format "%s " fn)))
	;; The message.
	(insert (apply #'format-message fmt-string arguments))
	;; Write message to debug file.
	(when tramp-debug-to-file
	  (ignore-errors
	    (write-region
	     point (point-max) (tramp-get-debug-file-name vec) 'append)))))))

(put #'tramp-debug-message 'tramp-suppress-trace t)

(defvar tramp-inhibit-progress-reporter nil
  "Show Tramp progress reporter in the minibuffer.
This variable is used to disable concurrent progress reporter messages.")

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
      ;; Display only when there is a minimum level, and the progress
      ;; reporter doesn't suppress further messages.
      (when (and (<= level 3) (null tramp-inhibit-progress-reporter))
	(apply #'message
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
	  ;; Append connection buffer for error messages, if exists.
	  (when (= level 1)
	    (ignore-errors
	      (with-current-buffer
		  (if (processp vec-or-proc)
		      (process-buffer vec-or-proc)
		    (tramp-get-connection-buffer vec-or-proc 'dont-create))
		(setq fmt-string (concat fmt-string "\n%s")
		      arguments (append arguments (list (buffer-string)))))))
	  ;; Translate proc to vec.
	  (when (processp vec-or-proc)
	    (setq vec-or-proc (process-get vec-or-proc 'vector))))
	;; Do it.
	(when (tramp-file-name-p vec-or-proc)
	  (apply #'tramp-debug-message
		 vec-or-proc
		 (concat (format "(%d) # " level) fmt-string)
		 arguments))))))

(put #'tramp-message 'tramp-suppress-trace t)

(defsubst tramp-backtrace (&optional vec-or-proc)
  "Dump a backtrace into the debug buffer.
If VEC-OR-PROC is nil, the buffer *debug tramp* is used.  This
function is meant for debugging purposes."
  (when (>= tramp-verbose 10)
    (if vec-or-proc
	(tramp-message
	 vec-or-proc 10 "\n%s" (with-output-to-string (backtrace)))
      (with-output-to-temp-buffer "*debug tramp*" (backtrace)))))

(put #'tramp-backtrace 'tramp-suppress-trace t)

(defun tramp-error (vec-or-proc signal fmt-string &rest arguments)
  "Emit an error.
VEC-OR-PROC identifies the connection to use, SIGNAL is the
signal identifier to be raised, remaining arguments passed to
`tramp-message'.  Finally, signal SIGNAL is raised with
FMT-STRING and ARGUMENTS."
  (let (signal-hook-function)
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
    (signal signal (list (substring-no-properties
			  (apply #'format-message fmt-string arguments))))))

(put #'tramp-error 'tramp-suppress-trace t)

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
	  (apply #'tramp-error vec-or-proc signal fmt-string arguments)
	;; Save exit.
	(when (and buf
		   (not (zerop tramp-verbose))
		   ;; Do not show when flagged from outside.
		   (not non-essential)
		   ;; Show only when Emacs has started already.
		   (current-message))
	  (let ((enable-recursive-minibuffers t)
		inhibit-message)
	    ;; `tramp-error' does not show messages.  So we must do it
	    ;; ourselves.
	    (apply #'message fmt-string arguments)
	    ;; Show buffer.
	    (pop-to-buffer buf)
	    (discard-input)
	    (sit-for 30)))
	;; Reset timestamp.  It would be wrong after waiting for a while.
	(when (tramp-file-name-equal-p vec (car tramp-current-connection))
	  (setcdr tramp-current-connection (current-time)))))))

(put #'tramp-error-with-buffer 'tramp-suppress-trace t)

;; We must make it a defun, because it is used earlier already.
(defun tramp-user-error (vec-or-proc fmt-string &rest arguments)
  "Signal a user error (or \"pilot error\")."
  (unwind-protect
      (apply #'tramp-error vec-or-proc 'user-error fmt-string arguments)
    ;; Save exit.
    (when (and (not (zerop tramp-verbose))
	       ;; Do not show when flagged from outside.
	       (not non-essential)
	       ;; Show only when Emacs has started already.
	       (current-message))
      (let ((enable-recursive-minibuffers t)
	    inhibit-message)
	;; `tramp-error' does not show messages.  So we must do it ourselves.
	(apply #'message fmt-string arguments)
	(discard-input)
	(sit-for 30)
	;; Reset timestamp.  It would be wrong after waiting for a while.
	(when
	    (tramp-file-name-equal-p vec-or-proc (car tramp-current-connection))
	  (setcdr tramp-current-connection (current-time)))))))

(put #'tramp-user-error 'tramp-suppress-trace t)

(defmacro tramp-with-demoted-errors (vec-or-proc format &rest body)
  "Execute BODY while redirecting the error message to `tramp-message'.
BODY is executed like wrapped by `with-demoted-errors'.  FORMAT
is a format-string containing a %-sequence meaning to substitute
the resulting error message."
  (declare (indent 2) (debug (symbolp form body)))
  (let ((err (make-symbol "err")))
    `(condition-case-unless-debug ,err
         (progn ,@body)
       (error (tramp-message ,vec-or-proc 3 ,format ,err) nil))))

(put #'tramp-with-demoted-errors 'tramp-suppress-trace t)

(defun tramp-test-message (fmt-string &rest arguments)
  "Emit a Tramp message according `default-directory'."
  (if (tramp-tramp-file-p default-directory)
      (apply #'tramp-message
	     (tramp-dissect-file-name default-directory) 0 fmt-string arguments)
    (apply #'message fmt-string arguments)))

(put #'tramp-test-message 'tramp-suppress-trace t)

;; This function provides traces in case of errors not triggered by
;; Tramp functions.
(defun tramp-signal-hook-function (error-symbol data)
  "Function to be called via `signal-hook-function'."
  ;; `custom-initialize-*' functions provoke `void-variable' errors.
  ;; We don't want to see them in the backtrace.
  (unless (eq error-symbol 'void-variable)
    (let ((inhibit-message t))
      (tramp-error
       (car tramp-current-connection) error-symbol
       (mapconcat (lambda (x) (format "%s" x)) data " ")))))

(put #'tramp-signal-hook-function 'tramp-suppress-trace t)

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
  (declare (indent 2) (debug (form symbolp body)))
  (let ((bindings
         (mapcar
	  (lambda (elem)
            `(,(if var (intern (format "%s-%s" var elem)) elem)
              (,(intern (format "tramp-file-name-%s" elem))
               ,(or var 'v))))
	  (cdr (mapcar #'car (cl-struct-slot-info 'tramp-file-name))))))
    `(let* ((,(or var 'v) (tramp-dissect-file-name ,filename))
            ,@bindings)
       ;; We don't know which of those vars will be used, so we bind them all,
       ;; and then add here a dummy use of all those variables, so we don't get
       ;; flooded by warnings about those vars `body' didn't use.
       (ignore ,@(mapcar #'car bindings))
       ,@body)))

(font-lock-add-keywords 'emacs-lisp-mode '("\\<with-parsed-tramp-file-name\\>"))

(defun tramp-progress-reporter-update (reporter &optional value suffix)
  "Report progress of an operation for Tramp."
  (let* ((parameters (cdr reporter))
	 (message (aref parameters 3)))
    (when (tramp-compat-string-search message (or (current-message) ""))
      (tramp-compat-progress-reporter-update reporter value suffix))))

(defmacro with-tramp-progress-reporter (vec level message &rest body)
  "Execute BODY, spinning a progress reporter with MESSAGE in interactive mode.
If LEVEL does not fit for visible messages, there are only traces
without a visible progress reporter."
  (declare (indent 3) (debug t))
  `(if (or noninteractive inhibit-message)
       (progn ,@body)
     (tramp-message ,vec ,level "%s..." ,message)
     (let ((cookie "failed")
           (tm
            ;; We start a pulsing progress reporter after 3 seconds.
            ;; Start only when there is no other progress reporter
            ;; running, and when there is a minimum level.
	    (when-let ((pr (and (null tramp-inhibit-progress-reporter)
				(<= ,level (min tramp-verbose 3))
				(make-progress-reporter ,message nil nil))))
	      (run-at-time 3 0.1 #'tramp-progress-reporter-update pr))))
       (unwind-protect
           ;; Execute the body.
           (prog1
	       ;; Suppress concurrent progress reporter messages.
	       (let ((tramp-inhibit-progress-reporter
		      (or tramp-inhibit-progress-reporter tm)))
		 ,@body)
	     (setq cookie "done"))
         ;; Stop progress reporter.
         (if tm (cancel-timer tm))
         (tramp-message ,vec ,level "%s...%s" ,message cookie)))))

(font-lock-add-keywords
 'emacs-lisp-mode '("\\<with-tramp-progress-reporter\\>"))

(defmacro with-tramp-file-property (vec file property &rest body)
  "Check in Tramp cache for PROPERTY, otherwise execute BODY and set cache.
FILE must be a local file name on a connection identified via VEC."
  (declare (indent 3) (debug t))
  `(if (file-name-absolute-p ,file)
       (let ((value (tramp-get-file-property
		     ,vec ,file ,property tramp-cache-undefined)))
	 (when (eq value tramp-cache-undefined)
	   ;; We cannot pass @body as parameter to
	   ;; `tramp-set-file-property' because it mangles our debug
	   ;; messages.
	   (setq value (progn ,@body))
	   (tramp-set-file-property ,vec ,file ,property value))
	 value)
     ,@body))

(font-lock-add-keywords 'emacs-lisp-mode '("\\<with-tramp-file-property\\>"))

(defmacro with-tramp-connection-property (key property &rest body)
  "Check in Tramp for property PROPERTY, otherwise execute BODY and set."
  (declare (indent 2) (debug t))
  `(let ((value (tramp-get-connection-property
		 ,key ,property tramp-cache-undefined)))
     (when (eq value tramp-cache-undefined)
       ;; We cannot pass ,@body as parameter to
       ;; `tramp-set-connection-property' because it mangles our debug
       ;; messages.
       (setq value (progn ,@body))
       (tramp-set-connection-property ,key ,property value))
     value))

(font-lock-add-keywords
 'emacs-lisp-mode '("\\<with-tramp-connection-property\\>"))

(defun tramp-drop-volume-letter (name)
  "Cut off unnecessary drive letter from file NAME.
The functions `tramp-*-handle-expand-file-name' call `expand-file-name'
locally on a remote file name.  When the local system is a W32 system
but the remote system is Unix, this introduces a superfluous drive
letter into the file name.  This function removes it."
  (save-match-data
    (let ((quoted (tramp-compat-file-name-quoted-p name 'top))
	  (result (tramp-compat-file-name-unquote name 'top)))
      (setq result (if (string-match "\\`[[:alpha:]]:/" result)
		     (replace-match "/" nil t result) result))
      (if quoted (tramp-compat-file-name-quote result 'top) result))))

;;; Config Manipulation Functions:

(defconst tramp-dns-sd-service-regexp "^_[-[:alnum:]]+\\._tcp$"
  "DNS-SD service regexp.")

(defun tramp-set-completion-function (method function-list)
  "Set the list of completion functions for METHOD.
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
		    ((string-prefix-p "HKEY_CURRENT_USER" (nth 1 (car v)))
		     (and (memq system-type '(cygwin windows-nt))
			  (zerop
			   (tramp-call-process
			    v "reg" nil nil nil "query" (nth 1 (car v))))))
		    ;; DNS-SD service type.
		    ((string-match-p
		      tramp-dns-sd-service-regexp (nth 1 (car v))))
		    ;; Configuration file or empty string.
		    (t (file-exists-p (nth 1 (car v))))))
	(setq r (delete (car v) r)))
      (setq v (cdr v)))

    (when r
      (add-to-list 'tramp-completion-function-alist
		   (cons method r)))))

(defun tramp-get-completion-function (method)
  "Return a list of completion functions for METHOD.
For definition of that list see `tramp-set-completion-function'."
  (append
   `(;; Default settings are taken into account.
     (tramp-parse-default-user-host ,method)
     ;; Hits from auth-sources.
     (tramp-parse-auth-sources ,method)
     ;; Hosts visited once shall be remembered.
     (tramp-parse-connection-properties ,method))
   ;; The method related defaults.
   (cdr (assoc method tramp-completion-function-alist))))

;; Inodes don't exist for some file systems.  Therefore we must
;; generate virtual ones.  Used in `find-buffer-visiting'.  The method
;; applied might be not so efficient (Ange-FTP uses hashes).  But
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

(defun tramp-default-file-modes (filename &optional flag)
  "Return file modes of FILENAME as integer.
If optional FLAG is ‘nofollow’, do not follow FILENAME if it is a
symbolic link.  If the file modes of FILENAME cannot be
determined, return the value of `default-file-modes', without
execute permissions."
  (or (tramp-compat-file-modes filename flag)
      (logand (default-file-modes) #o0666)))

(defun tramp-replace-environment-variables (filename)
 "Replace environment variables in FILENAME.
Return the string with the replaced variables."
 (substitute-env-vars filename 'only-defined))

(defun tramp-find-file-name-coding-system-alist (filename tmpname)
  "Like `find-operation-coding-system' for Tramp filenames.
Tramp's `insert-file-contents' and `write-region' work over
temporary file names.  If `file-coding-system-alist' contains an
expression, which matches more than the file name suffix, the
coding system might not be determined.  This function repairs it."
  (let (result)
    (dolist (elt file-coding-system-alist (nreverse result))
      (when (and (consp elt) (string-match-p (car elt) filename))
	;; We found a matching entry in `file-coding-system-alist'.
	;; So we add a similar entry, but with the temporary file name
	;; as regexp.
	(push (cons (regexp-quote tmpname) (cdr elt)) result)))))

(defun tramp-run-real-handler (operation args)
  "Invoke normal file name handler for OPERATION.
First arg specifies the OPERATION, second arg ARGS is a list of
arguments to pass to the OPERATION."
  (let* ((inhibit-file-name-handlers
	  `(tramp-file-name-handler
	    tramp-vc-file-name-handler
	    tramp-completion-file-name-handler
	    tramp-archive-file-name-handler
	    tramp-crypt-file-name-handler
	    cygwin-mount-name-hook-function
	    cygwin-mount-map-drive-hook-function
	    .
	    ,(and (eq inhibit-file-name-operation operation)
		  inhibit-file-name-handlers)))
	 (inhibit-file-name-operation operation)
	 signal-hook-function)
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
ARGS are the arguments OPERATION has been called with.

It does not always return a Tramp file name, for example if the
first argument of `expand-file-name' is absolute and not remote.
Must be handled by the callers."
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
	      find-backup-file-name get-file-buffer
	      insert-directory insert-file-contents load
	      make-directory make-directory-internal set-file-acl
	      set-file-modes set-file-selinux-context set-file-times
	      substitute-in-file-name unhandled-file-name-directory
	      vc-registered
	      ;; Emacs 26+ only.
	      file-name-case-insensitive-p
	      ;; Emacs 27+ only.
	      file-system-info
	      ;; Emacs 28+ only.
	      file-locked-p lock-file make-lock-file-name unlock-file
	      ;; Emacs 29+ only.
	      abbreviate-file-name
	      ;; Tramp internal magic file name function.
	      tramp-set-file-uid-gid))
    (if (file-name-absolute-p (nth 0 args))
	(nth 0 args)
      default-directory))
   ;; STRING FILE.
   ;; Starting with Emacs 26.1, just the 2nd argument of
   ;; `make-symbolic-link' matters.
   ((eq operation 'make-symbolic-link) (nth 1 args))
   ;; FILE DIRECTORY resp FILE1 FILE2.
   ((member operation
	    '(add-name-to-file copy-directory copy-file
	      file-equal-p file-in-directory-p
	      file-name-all-completions file-name-completion
	      file-newer-than-file-p rename-file))
    (cond
     ((tramp-tramp-file-p (nth 0 args)) (nth 0 args))
     ((file-name-absolute-p (nth 1 args)) (nth 1 args))
     (t default-directory)))
   ;; FILE DIRECTORY resp FILE1 FILE2.
   ((eq operation 'expand-file-name)
    (cond
     ((file-name-absolute-p (nth 0 args)) (nth 0 args))
     ((tramp-tramp-file-p (nth 1 args)) (nth 1 args))
     (t default-directory)))
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
	      make-nearby-temp-file temporary-file-directory
	      ;; Emacs 27+ only.
	      exec-path make-process))
    default-directory)
   ;; PROC.
   ((member operation '(file-notify-rm-watch file-notify-valid-p))
    (when (processp (nth 0 args))
      (with-current-buffer (process-buffer (nth 0 args))
	default-directory)))
   ;; VEC.
   ((member operation '(tramp-get-remote-gid tramp-get-remote-uid))
    (tramp-make-tramp-file-name (nth 0 args)))
   ;; Unknown file primitive.
   (t (error "Unknown file I/O primitive: %s" operation))))

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

;; Main function.
(defun tramp-file-name-handler (operation &rest args)
  "Invoke Tramp file name handler for OPERATION and ARGS.
Fall back to normal file name handler if no Tramp file name handler exists."
  (let ((filename (apply #'tramp-file-name-for-operation operation args))
	;; `file-remote-p' is called for everything, even for symbolic
	;; links which look remote.  We don't want to get an error.
	(non-essential (or non-essential (eq operation 'file-remote-p))))
    (if (tramp-tramp-file-p filename)
	(save-match-data
          (setq filename (tramp-replace-environment-variables filename))
          (with-parsed-tramp-file-name filename nil
            (let ((current-connection tramp-current-connection)
		  (foreign
		   (tramp-find-foreign-file-name-handler filename operation))
		  (signal-hook-function #'tramp-signal-hook-function)
		  result)
	      ;; Set `tramp-current-connection'.
	      (unless
		  (tramp-file-name-equal-p v (car tramp-current-connection))
		(setq tramp-current-connection (list v)))

	      ;; Call the backend function.
	      (unwind-protect
	          (if foreign
		      (let ((sf (symbol-function foreign)))
		        ;; Some packages set the default directory to
		        ;; a remote path, before respective Tramp
		        ;; packages are already loaded.  This results
		        ;; in recursive loading.  Therefore, we load
		        ;; the Tramp packages locally.
		        (when (autoloadp sf)
                          ;; FIXME: Not clear why we need these bindings here.
                          ;; The explanation above is not convincing and
                          ;; the bug#9114 for which it was added doesn't
                          ;; clarify the core of the problem.
			  (let ((default-directory
                                  tramp-compat-temporary-file-directory)
			        file-name-handler-alist)
			    (autoload-do-load sf foreign)))
                        ;; (tramp-message
                        ;;  v 4 "Running `%s'..." (cons operation args))
                        ;; If `non-essential' is non-nil, Tramp shall
		        ;; not open a new connection.
		        ;; If Tramp detects that it shouldn't continue
		        ;; to work, it throws the `suppress' event.
		        ;; This could happen for example, when Tramp
		        ;; tries to open the same connection twice in
		        ;; a short time frame.
		        ;; In both cases, we try the default handler then.
		        (setq result
			      (catch 'non-essential
			        (catch 'suppress
				  (apply foreign operation args))))
                        ;; (tramp-message
                        ;;  v 4 "Running `%s'...`%s'" (cons operation args) result)
		        (cond
		         ((eq result 'non-essential)
			  (tramp-message
			   v 5 "Non-essential received in operation %s"
			   (cons operation args))
			  (tramp-run-real-handler operation args))
		         ((eq result 'suppress)
			  (let ((inhibit-message t))
			    (tramp-message
			     v 1 "Suppress received in operation %s"
			     (cons operation args))
			    (tramp-cleanup-connection v t)
			    (tramp-run-real-handler operation args)))
		         (t result)))

		    ;; Nothing to do for us.  However, since we are in
		    ;; `tramp-mode', we must suppress the volume
		    ;; letter on MS Windows.
		    (setq result (tramp-run-real-handler operation args))
		    (if (stringp result)
		        (tramp-drop-volume-letter result)
		      result))

		;; Reset `tramp-current-connection'.
		(unless
		    (tramp-file-name-equal-p
		     (car current-connection) (car tramp-current-connection))
		  (setq tramp-current-connection current-connection))))))

      ;; When `tramp-mode' is not enabled, or the file name is quoted,
      ;; we don't do anything.
      (tramp-run-real-handler operation args))))

(defun tramp-completion-file-name-handler (operation &rest args)
  "Invoke Tramp file name completion handler for OPERATION and ARGS.
Falls back to normal file name handler if no Tramp file name handler exists."
  (if-let
      ((fn (and tramp-mode
		(assoc operation tramp-completion-file-name-handler-alist))))
      (save-match-data (apply (cdr fn) args))
    (tramp-run-real-handler operation args)))

;;;###autoload
(progn (defun tramp-autoload-file-name-handler (operation &rest args)
  "Load Tramp file name handler, and perform OPERATION."
  (tramp-unload-file-name-handlers)
  (when tramp-mode
    ;; We cannot use `tramp-compat-temporary-file-directory' here due
    ;; to autoload.  When installing Tramp's GNU ELPA package, there
    ;; might be an older, incompatible version active.  We try to
    ;; overload this.
    (let ((default-directory temporary-file-directory))
      (when (bound-and-true-p tramp-archive-autoload)
	(load "tramp-archive" 'noerror 'nomessage))
      (load "tramp" 'noerror 'nomessage)))
  (apply operation args)))

;; `tramp-autoload-file-name-handler' must be registered before
;; evaluation of site-start and init files, because there might exist
;; remote files already, f.e. files kept via recentf-mode.
;;;###autoload
(progn (defun tramp-register-autoload-file-name-handlers ()
  "Add Tramp file name handlers to `file-name-handler-alist' during autoload."
  (add-to-list 'file-name-handler-alist
	       (cons tramp-autoload-file-name-regexp
		     #'tramp-autoload-file-name-handler))
  (put #'tramp-autoload-file-name-handler 'safe-magic t)))

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
	     #'file-name-sans-extension
	     (directory-files dir nil "\\`tramp.+\\.elc?\\'"))
	    'paren))))
    (mapatoms
     (lambda (atom)
       (when (and (functionp atom)
		  (autoloadp (symbol-function atom))
		  (string-match-p files-regexp (cadr (symbol-function atom))))
	 (ignore-errors
	   (setf (cadr (symbol-function atom))
		 (expand-file-name (cadr (symbol-function atom)) dir))))))))

(tramp--with-startup (tramp-use-absolute-autoload-file-names))

(defun tramp-register-file-name-handlers ()
  "Add Tramp file name handlers to `file-name-handler-alist'."
  ;; Remove autoloaded handlers from file name handler alist.  Useful,
  ;; if `tramp-syntax' has been changed.
  (tramp-unload-file-name-handlers)

  ;; Add the handlers.  We do not add anything to the `operations'
  ;; property of `tramp-file-name-handler',
  ;; `tramp-archive-file-name-handler' and
  ;; `tramp-crypt-file-name-handler', this shall be done by the
  ;; respective foreign handlers.
  (add-to-list 'file-name-handler-alist
	       (cons tramp-file-name-regexp #'tramp-file-name-handler))
  (put #'tramp-file-name-handler 'safe-magic t)

  (tramp-register-crypt-file-name-handler)

  (add-to-list 'file-name-handler-alist
	       (cons tramp-completion-file-name-regexp
		     #'tramp-completion-file-name-handler))
  (put #'tramp-completion-file-name-handler 'safe-magic t)
  ;; Mark `operations' the handler is responsible for.
  (put #'tramp-completion-file-name-handler 'operations
       (mapcar #'car tramp-completion-file-name-handler-alist))

  (when (bound-and-true-p tramp-archive-enabled)
    (add-to-list 'file-name-handler-alist
	         (cons tramp-archive-file-name-regexp
		       #'tramp-archive-file-name-handler))
    (put #'tramp-archive-file-name-handler 'safe-magic t))

  ;; If jka-compr or epa-file are already loaded, move them to the
  ;; front of `file-name-handler-alist'.
  (dolist (fnh '(epa-file-handler jka-compr-handler))
    (when-let ((entry (rassoc fnh file-name-handler-alist)))
      (setq file-name-handler-alist
	    (cons entry (delete entry file-name-handler-alist))))))

(tramp--with-startup (tramp-register-file-name-handlers))

(defun tramp-register-foreign-file-name-handler
    (func handler &optional append)
  "Register (FUNC . HANDLER) in `tramp-foreign-file-name-handler-alist'.
FUNC is the function, which determines whether HANDLER is to be called.
Add operations defined in `HANDLER-alist' to `tramp-file-name-handler'."
  (add-to-list
   'tramp-foreign-file-name-handler-alist `(,func . ,handler) append)
  ;; Mark `operations' the handler is responsible for.
  (put #'tramp-file-name-handler
       'operations
       (delete-dups
        (append
         (get 'tramp-file-name-handler 'operations)
         (mapcar
          #'car
          (symbol-value (intern (concat (symbol-name handler) "-alist"))))))))

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
  (dolist (fnh file-name-handler-alist)
    (when (and (symbolp (cdr fnh))
	       (string-prefix-p "tramp-" (symbol-name (cdr fnh))))
      (setq file-name-handler-alist (delq fnh file-name-handler-alist))))))

(add-hook 'tramp-unload-hook #'tramp-unload-file-name-handlers)

;;; File name handler functions for completion mode:

;; This function takes action since Emacs 28.1, when
;; `read-extended-command-predicate' is set to
;; `command-completion-default-include-p'.
(defun tramp-command-completion-p (_symbol buffer)
  "A predicate for Tramp interactive commands.
They are completed by \"M-x TAB\" only if the current buffer is remote."
  (with-current-buffer buffer (tramp-tramp-file-p default-directory)))

(defun tramp-connectable-p (vec-or-filename)
  "Check, whether it is possible to connect the remote host w/o side-effects.
This is true, if either the remote host is already connected, or if we are
not in completion mode."
  (let ((tramp-verbose 0)
	(vec
	 (cond
	  ((tramp-file-name-p vec-or-filename) vec-or-filename)
	  ((tramp-tramp-file-p vec-or-filename)
	   (tramp-dissect-file-name vec-or-filename)))))
    (or ;; We check this for the process related to
	;; `tramp-buffer-name'; otherwise `start-file-process'
	;; wouldn't run ever when `non-essential' is non-nil.
        (and vec (process-live-p (get-process (tramp-buffer-name vec))))
	(not non-essential))))

;; Method, host name and user name completion.
;; `tramp-completion-dissect-file-name' returns a list of
;; `tramp-file-name' structures.  For all of them we return possible
;; completions.
(defun tramp-completion-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for partial Tramp files."
  (let ((fullname
	 (tramp-drop-volume-letter (expand-file-name filename directory)))
	;; When `tramp-syntax' is `simplified', we need a default method.
	(tramp-default-method
	 (and (zerop (length tramp-postfix-method-format))
	      tramp-default-method))
	(tramp-default-method-alist
	 (and (zerop (length tramp-postfix-method-format))
	      tramp-default-method-alist))
	tramp-default-user tramp-default-user-alist
	tramp-default-host tramp-default-host-alist
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
	#'file-name-all-completions (list filename directory))))))

;; Method, host name and user name completion for a file.
(defun tramp-completion-handle-file-name-completion
  (filename directory &optional predicate)
  "Like `file-name-completion' for partial Tramp files."
  ;; Suppress eager completion on not connected hosts.
  (let ((non-essential t))
    (try-completion
     filename
     (mapcar #'list (file-name-all-completions filename directory))
     (when (and predicate (tramp-connectable-p directory))
       (lambda (x) (funcall predicate (expand-file-name (car x) directory)))))))

;; I misuse a little bit the `tramp-file-name' structure in order to
;; handle completion possibilities for partial methods / user names /
;; host names.  Return value is a list of `tramp-file-name' structures
;; according to possible completions.  If "localname" is non-nil it
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
  "Return a list of `tramp-file-name' structures for NAME.
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
  "Return a `tramp-file-name' structure for NAME matching STRUCTURE.
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
  "Return all method completions for PARTIAL-METHOD."
  (mapcar
   (lambda (method)
     (and method (string-prefix-p (or partial-method "") method)
	  (tramp-completion-make-tramp-file-name method nil nil nil)))
   (mapcar #'car tramp-methods)))

;; Compares partial user and host names with possible completions.
(defun tramp-get-completion-user-host
  (method partial-user partial-host user host)
  "Return the most expanded string for user and host name completion.
PARTIAL-USER must match USER, PARTIAL-HOST must match HOST."
  (cond

   ((and partial-user partial-host)
    (if	(and host (string-prefix-p partial-host host)
	     (string-equal partial-user (or user partial-user)))
	(setq user partial-user)
      (setq user nil
	    host nil)))

   (partial-user
    (setq host nil)
    (unless (and user (string-prefix-p partial-user user))
      (setq user nil)))

   (partial-host
    (setq user nil)
    (unless (and host (string-prefix-p partial-host host))
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

(defcustom tramp-completion-use-auth-sources auth-source-do-cache
  "Whether to use `auth-source-search' for completion of user and host names.
This could be disturbing, if it requires a password / passphrase,
as for \"~/.authinfo.gpg\"."
  :version "27.1"
  :type 'boolean)

(defun tramp-parse-auth-sources (method)
  "Return a list of (user host) tuples allowed to access for METHOD.
This function is added always in `tramp-get-completion-function'
for all methods.  Resulting data are derived from default settings."
  (and tramp-completion-use-auth-sources
       (mapcar
	(lambda (x) `(,(plist-get x :user) ,(plist-get x :host)))
	(auth-source-search
	 :port method :require '(:port) :max most-positive-fixnum))))

;; Generic function.
(defun tramp-parse-group (regexp match-level skip-chars)
   "Return a (user host) tuple allowed to access.
User is always nil."
   (let (result)
     (when (re-search-forward regexp (point-at-eol) t)
       (setq result (list nil (match-string match-level))))
     (or
      (> (skip-chars-forward skip-chars) 0)
      (forward-line 1))
     result))

;; Generic function.
(defun tramp-parse-file (filename function)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  ;; On Windows, there are problems in completion when
  ;; `default-directory' is remote.
  (let ((default-directory tramp-compat-temporary-file-directory))
    (when (file-readable-p filename)
      (with-temp-buffer
	(insert-file-contents-literally filename)
	(goto-char (point-min))
        (cl-loop while (not (eobp)) collect (funcall function))))))

(defun tramp-parse-rhosts (filename)
  "Return a list of (user host) tuples allowed to access.
Either user or host may be nil."
  (tramp-parse-file filename #'tramp-parse-rhosts-group))

(defun tramp-parse-rhosts-group ()
   "Return a (user host) tuple allowed to access.
Either user or host may be nil."
   (let (result
	 (regexp
	  (concat
	   "^\\(" tramp-host-regexp "\\)"
	   "\\([ \t]+" "\\(" tramp-user-regexp "\\)" "\\)?")))
     (when (re-search-forward regexp (point-at-eol) t)
       (setq result (append (list (match-string 3) (match-string 1)))))
     (forward-line 1)
     result))

(defun tramp-parse-shosts (filename)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  (tramp-parse-file filename #'tramp-parse-shosts-group))

(defun tramp-parse-shosts-group ()
   "Return a (user host) tuple allowed to access.
User is always nil."
   (tramp-parse-group (concat "^\\(" tramp-host-regexp "\\)") 1 ","))

(defun tramp-parse-sconfig (filename)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  (tramp-parse-file filename #'tramp-parse-sconfig-group))

(defun tramp-parse-sconfig-group ()
   "Return a (user host) tuple allowed to access.
User is always nil."
   (tramp-parse-group
    (concat "\\(?:^[ \t]*Host\\)" "\\|" "\\(?:^.+\\)"
	    "\\|" "\\(" tramp-host-regexp "\\)")
    1 " \t"))

;; Generic function.
(defun tramp-parse-shostkeys-sknownhosts (dirname regexp)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  ;; On Windows, there are problems in completion when
  ;; `default-directory' is remote.
  (let* ((default-directory tramp-compat-temporary-file-directory)
	 (files (and (file-directory-p dirname) (directory-files dirname))))
    (cl-loop
     for f in files
     when (and (not (string-match "^\\.\\.?$" f)) (string-match regexp f))
     collect (list nil (match-string 1 f)))))

(defun tramp-parse-shostkeys (dirname)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  (tramp-parse-shostkeys-sknownhosts
   dirname (concat "^key_[[:digit:]]+_\\(" tramp-host-regexp "\\)\\.pub$")))

(defun tramp-parse-sknownhosts (dirname)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  (tramp-parse-shostkeys-sknownhosts
   dirname
   (concat "^\\(" tramp-host-regexp "\\)\\.ssh-\\(dss\\|rsa\\)\\.pub$")))

(defun tramp-parse-hosts (filename)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  (tramp-parse-file filename #'tramp-parse-hosts-group))

(defun tramp-parse-hosts-group ()
   "Return a (user host) tuple allowed to access.
User is always nil."
   (tramp-parse-group
    (concat "^\\(" tramp-ipv6-regexp "\\|" tramp-host-regexp "\\)") 1 " \t"))

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
      (tramp-parse-file filename #'tramp-parse-passwd-group))))

(defun tramp-parse-passwd-group ()
   "Return a (user host) tuple allowed to access.
Host is always \"localhost\"."
   (let (result
	 (regexp (concat "^\\(" tramp-user-regexp "\\):")))
     (when (re-search-forward regexp (point-at-eol) t)
       (setq result (list (match-string 1) "localhost")))
     (forward-line 1)
     result))

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
      (tramp-parse-file filename #'tramp-parse-etc-group-group))))

(defun tramp-parse-etc-group-group ()
   "Return a (group host) tuple allowed to access.
Host is always \"localhost\"."
   (let (result
	 (split (split-string (buffer-substring (point) (point-at-eol)) ":")))
     (when (member (user-login-name) (split-string (nth 3 split) "," 'omit))
       (setq result (list (nth 0 split) "localhost")))
     (forward-line 1)
     result))

(defun tramp-parse-netrc (filename)
  "Return a list of (user host) tuples allowed to access.
User may be nil."
  ;; The declaration is not sufficient at runtime, because netrc.el is
  ;; not autoloaded.
  (autoload 'netrc-parse "netrc")
  (mapcar
   (lambda (item)
     (and (assoc "machine" item)
	  `(,(cdr (assoc "login" item)) ,(cdr (assoc "machine" item)))))
   (netrc-parse filename)))

(defun tramp-parse-putty (registry-or-dirname)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  (if (eq system-type 'windows-nt)
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
   (let (result
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

(defun tramp-handle-access-file (filename string)
  "Like `access-file' for Tramp files."
  (setq filename (file-truename filename))
  (with-parsed-tramp-file-name filename v
    (if (file-exists-p filename)
	(unless
	    (funcall
	     (if (file-directory-p filename)
		 #'file-accessible-directory-p #'file-readable-p)
	     filename)
	  (tramp-error
	   v 'file-error (format "%s: Permission denied, %s" string filename)))
      (tramp-compat-file-missing
       v (format "%s: No such file or directory, %s" string filename)))))

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
			  "File %s already exists; make it a link anyway?"
			  localname)))))
	  (tramp-error v 'file-already-exists newname)
	(delete-file newname)))
    (tramp-flush-file-properties v localname)
    (copy-file
     filename newname 'ok-if-already-exists 'keep-time
     'preserve-uid-gid 'preserve-permissions)))

(defun tramp-handle-copy-directory
  (directory newname &optional keep-date parents copy-contents)
  "Like `copy-directory' for Tramp files."
  ;; `copy-directory' creates NEWNAME before running this check.  So
  ;; we do it ourselves.
  (unless (file-exists-p directory)
    (tramp-compat-file-missing (tramp-dissect-file-name directory) directory))
  ;; We must do it file-wise.
  (tramp-run-real-handler
   #'copy-directory
   (list directory newname keep-date parents copy-contents)))

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

(defun tramp-handle-directory-files (directory &optional full match nosort count)
  "Like `directory-files' for Tramp files."
  (unless (file-exists-p directory)
    (tramp-compat-file-missing (tramp-dissect-file-name directory) directory))
  (when (file-directory-p directory)
    (setq directory (file-name-as-directory (expand-file-name directory)))
    (let ((temp (nreverse (file-name-all-completions "" directory)))
	  result item)

      (while temp
	(setq item (directory-file-name (pop temp)))
	(when (or (null match) (string-match-p match item))
	  (push (if full (concat directory item) item)
		result)))
      (unless nosort
        (setq result (sort result #'string<)))
      (when (and (natnump count) (> count 0))
	(setq result (nbutlast result (- (length result) count))))
      result)))

(defun tramp-handle-directory-files-and-attributes
  (directory &optional full match nosort id-format count)
  "Like `directory-files-and-attributes' for Tramp files."
  (mapcar
   (lambda (x)
     (cons x (file-attributes
	      (if full x (expand-file-name x directory)) id-format)))
   (tramp-compat-directory-files directory full match nosort count)))

(defun tramp-handle-dired-uncache (dir)
  "Like `dired-uncache' for Tramp files."
  (with-parsed-tramp-file-name
      (if (file-directory-p dir) dir (file-name-directory dir)) nil
    (tramp-flush-directory-properties v localname)))

(defun tramp-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for Tramp files."
  ;; If DIR is not given, use DEFAULT-DIRECTORY or "/".
  (setq dir (or dir default-directory "/"))
  ;; Handle empty NAME.
  (when (zerop (length name)) (setq name "."))
  ;; Unless NAME is absolute, concat DIR and NAME.
  (unless (file-name-absolute-p name)
    (setq name (tramp-compat-file-name-concat dir name)))
  ;; If NAME is not a Tramp file, run the real handler.
  (if (not (tramp-tramp-file-p name))
      (tramp-run-real-handler #'expand-file-name (list name nil))
    ;; Dissect NAME.
    (with-parsed-tramp-file-name name nil
      (unless (tramp-run-real-handler #'file-name-absolute-p (list localname))
	(setq localname (concat "/" localname)))
      ;; Do not keep "/..".
      (when (string-match-p "^/\\.\\.?$" localname)
	(setq localname "/"))
      ;; Do normal `expand-file-name' (this does "/./" and "/../").
      ;; `default-directory' is bound, because on Windows there would
      ;; be problems with UNC shares or Cygwin mounts.
      (let ((default-directory tramp-compat-temporary-file-directory))
	(tramp-make-tramp-file-name
	 v (tramp-drop-volume-letter
	    (tramp-run-real-handler #'expand-file-name (list localname))))))))

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
  (when (tramp-equal-remote
	 (expand-file-name filename1) (expand-file-name filename2))
    (tramp-run-real-handler #'file-equal-p (list filename1 filename2))))

(defun tramp-handle-file-exists-p (filename)
  "Like `file-exists-p' for Tramp files."
  ;; `file-exists-p' is used as predicate in file name completion.
  ;; We don't want to run it when `non-essential' is t, or there is
  ;; no connection process yet.
  (when (tramp-connectable-p filename)
    (not (null (file-attributes filename)))))

(defun tramp-handle-file-in-directory-p (filename directory)
  "Like `file-in-directory-p' for Tramp files."
  ;; Native `file-in-directory-p' calls `file-truename', which
  ;; requires a remote connection.  This can be avoided, if FILENAME
  ;; and DIRECTORY are not located on the same remote host.
  (when (tramp-equal-remote
	 (expand-file-name filename) (expand-file-name directory))
    (tramp-run-real-handler #'file-in-directory-p (list filename directory))))

(defun tramp-handle-file-local-copy (filename)
  "Like `file-local-copy' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (unless (file-exists-p filename)
      (tramp-compat-file-missing v filename))
    (let ((tmpfile (tramp-compat-make-temp-file filename)))
      (copy-file filename tmpfile 'ok-if-already-exists 'keep-time)
      tmpfile)))

(defun tramp-handle-file-modes (filename &optional flag)
  "Like `file-modes' for Tramp files."
  (when-let ((attrs (file-attributes filename))
	     (mode-string (tramp-compat-file-attribute-modes attrs)))
    (if (and (not (eq flag 'nofollow)) (eq ?l (aref mode-string 0)))
	(file-modes (file-truename filename))
      (tramp-mode-string-to-int mode-string))))

;; Localname manipulation functions that grok Tramp localnames...
(defun tramp-handle-file-name-as-directory (file)
  "Like `file-name-as-directory' for Tramp files."
  ;; `file-name-as-directory' would be sufficient except localname is
  ;; the empty string.
  (let ((v (tramp-dissect-file-name file t)))
    ;; Run the command on the localname portion only unless we are in
    ;; completion mode.
    (tramp-make-tramp-file-name
     v (or (and (zerop (length (tramp-file-name-localname v)))
		(not (tramp-connectable-p file)))
	   (tramp-run-real-handler
	    #'file-name-as-directory
	    (list (tramp-file-name-localname v)))))))

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

     ;; There isn't.  So we must check, in case there's a connection already.
     (and (file-remote-p filename nil 'connected)
          (with-tramp-connection-property v "case-insensitive"
	    (ignore-errors
	      (with-tramp-progress-reporter v 5 "Checking case-insensitive"
		;; The idea is to compare a file with lower case
		;; letters with the same file with upper case letters.
		(let ((candidate
		       (tramp-compat-file-name-unquote
			(directory-file-name filename)))
		      case-fold-search
		      tmpfile)
		  ;; Check, whether we find an existing file with
		  ;; lower case letters.  This avoids us to create a
		  ;; temporary file.
		  (while (and (string-match-p
			       "[[:lower:]]" (tramp-file-local-name candidate))
			      (not (file-exists-p candidate)))
		    (setq candidate
			  (directory-file-name
			   (file-name-directory candidate))))
		  ;; Nothing found, so we must use a temporary file
		  ;; for comparison.  `make-nearby-temp-file' is added
		  ;; to Emacs 26+ like `file-name-case-insensitive-p',
		  ;; so there is no compatibility problem calling it.
		  (unless (string-match-p
			   "[[:lower:]]" (tramp-file-local-name candidate))
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
			(upcase (tramp-file-local-name candidate))))
		    ;; Cleanup.
		    (when tmpfile (delete-file tmpfile)))))))))))

(defun tramp-handle-file-name-completion
  (filename directory &optional predicate)
  "Like `file-name-completion' for Tramp files."
  (let (hits-ignored-extensions fnac)
    (setq fnac (file-name-all-completions filename directory))
    ;; "." and ".." are never interesting as completions, and are
    ;; actually in the way in a directory with only one file.  See
    ;; file_name_completion() in dired.c.
    (when (and (consp fnac) (= (length (delete "./" (delete "../" fnac))) 1))
      (setq fnac (delete "./" (delete "../" fnac))))
    (or
     (try-completion
      filename fnac
      (lambda (x)
	(when (funcall (or predicate #'identity) (expand-file-name x directory))
	  (not
	   (and
	    completion-ignored-extensions
	    (string-match-p
	     (concat (regexp-opt completion-ignored-extensions 'paren) "$") x)
	    ;; We remember the hit.
	    (push x hits-ignored-extensions))))))
     ;; No match.  So we try again for ignored files.
     (try-completion filename hits-ignored-extensions))))

(defun tramp-handle-file-name-directory (file)
  "Like `file-name-directory' for Tramp files."
  ;; Everything except the last filename thing is the directory.  We
  ;; cannot apply `with-parsed-tramp-file-name', because this expands
  ;; the remote file name parts.
  (let ((v (tramp-dissect-file-name file t)))
    ;; Run the command on the localname portion only.  If this returns
    ;; nil, mark also the localname part of `v' as nil.
    (tramp-make-tramp-file-name
     v (or (tramp-run-real-handler
	    #'file-name-directory (list (tramp-file-name-localname v)))
	   'noloc))))

(defun tramp-handle-file-name-nondirectory (file)
  "Like `file-name-nondirectory' for Tramp files."
  (with-parsed-tramp-file-name file nil
    (tramp-run-real-handler #'file-name-nondirectory (list localname))))

(defun tramp-handle-file-newer-than-file-p (file1 file2)
  "Like `file-newer-than-file-p' for Tramp files."
  (cond
   ((not (file-exists-p file1)) nil)
   ((not (file-exists-p file2)) t)
   (t (time-less-p
       (tramp-compat-file-attribute-modification-time (file-attributes file2))
       (tramp-compat-file-attribute-modification-time
	(file-attributes file1))))))

(defun tramp-handle-file-readable-p (filename)
  "Like `file-readable-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-readable-p"
      (or (tramp-check-cached-permissions v ?r)
	  ;; `tramp-check-cached-permissions' doesn't handle symbolic
	  ;; links.
	  (when-let ((symlink (file-symlink-p filename)))
	    (and (stringp symlink)
		 (file-readable-p (concat (file-remote-p filename) symlink))))))))

(defun tramp-handle-file-regular-p (filename)
  "Like `file-regular-p' for Tramp files."
  (and (file-exists-p filename)
       ;; Sometimes, `file-attributes' does not return a proper value
       ;; even if `file-exists-p' does.
       (when-let ((attr (file-attributes filename)))
	 (eq ?- (aref (tramp-compat-file-attribute-modes attr) 0)))))

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
		(t (tramp-make-tramp-file-name v 'noloc)))))))))

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
  ;; Preserve trailing "/".
  (funcall
   (if (directory-name-p filename) #'file-name-as-directory #'identity)
   ;; Quote properly.
   (funcall
    (if (tramp-compat-file-name-quoted-p filename)
	#'tramp-compat-file-name-quote #'identity)
    (let ((result (tramp-compat-file-name-unquote (expand-file-name filename)))
	  (numchase 0)
	  ;; Don't make the following value larger than necessary.
	  ;; People expect an error message in a timely fashion when
	  ;; something is wrong; otherwise they might think that Emacs
	  ;; is hung.  Of course, correctness has to come first.
	  (numchase-limit 20)
	  ;; Unquoting could enable encryption.
	  tramp-crypt-enabled
	  symlink-target)
      (with-parsed-tramp-file-name result v1
	;; We cache only the localname.
	(tramp-make-tramp-file-name
	 v1
	 (with-tramp-file-property v1 v1-localname "file-truename"
	   (while (and (setq symlink-target (file-symlink-p result))
		       (< numchase numchase-limit))
	     (setq numchase (1+ numchase)
		   result
		   (with-parsed-tramp-file-name (expand-file-name result) v2
		     (tramp-make-tramp-file-name
		      v2
		      (if (stringp symlink-target)
			  (if (file-remote-p symlink-target)
			      (tramp-compat-file-name-quote symlink-target 'top)
			    (tramp-drop-volume-letter
			     (expand-file-name
			      symlink-target
			      (file-name-directory v2-localname))))
			v2-localname)
		      'nohop)))
	     (when (>= numchase numchase-limit)
	       (tramp-error
		v1 'file-error
		"Maximum number (%d) of symlinks exceeded" numchase-limit)))
	   (tramp-file-local-name (directory-file-name result)))))))))

(defun tramp-handle-file-writable-p (filename)
  "Like `file-writable-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-writable-p"
      (if (file-exists-p filename)
	  (tramp-check-cached-permissions v ?w)
	;; If file doesn't exist, check if directory is writable.
	(and (file-directory-p (file-name-directory filename))
	     (file-writable-p (file-name-directory filename)))))))

(defcustom tramp-allow-unsafe-temporary-files nil
  "Whether root-owned auto-save, backup or lock files can be written to \"/tmp\"."
  :version "28.1"
  :type 'boolean)

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
		       (tramp-make-tramp-file-name v (cdr x))
		     (cdr x))))
		tramp-backup-directory-alist)
	     backup-directory-alist))
	  result)
      (prog1 ;; Run plain `find-backup-file-name'.
	  (setq result
		(tramp-run-real-handler
		 #'find-backup-file-name (list filename)))
        ;; Protect against security hole.
	(when (and (not tramp-allow-unsafe-temporary-files)
		   (not backup-inhibited)
		   (file-in-directory-p (car result) temporary-file-directory)
		   (zerop (or (tramp-compat-file-attribute-user-id
			       (file-attributes filename 'integer))
			      tramp-unknown-id-integer))
		   (not (with-tramp-connection-property
			    (tramp-get-process v) "unsafe-temporary-file"
			  (yes-or-no-p
			   (concat
			    "Backup file on local temporary directory, "
			    "do you want to continue?")))))
	  (tramp-error v 'file-error "Unsafe backup file name"))))))

(defun tramp-handle-insert-directory
  (filename switches &optional wildcard full-directory-p)
  "Like `insert-directory' for Tramp files."
  (unless switches (setq switches ""))
  ;; Mark trailing "/".
  (when (and (directory-name-p filename)
	     (not full-directory-p))
    (setq switches (concat switches "F")))
  ;; Check, whether directory is accessible.
  (unless wildcard
    (access-file filename "Reading directory"))
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-progress-reporter v 0 (format "Opening directory %s" filename)
      (let (ls-lisp-use-insert-directory-program start)
	;; Silence byte compiler.
	(ignore ls-lisp-use-insert-directory-program)
	(tramp-run-real-handler
	 #'insert-directory
	 (list filename switches wildcard full-directory-p))
	;; `ls-lisp' always returns full listings.  We must remove
	;; superfluous parts.
	(unless (tramp-compat-string-search "l" switches)
	  (save-excursion
	    (goto-char (point-min))
	    (while (setq start
			 (text-property-not-all
			  (point) (point-at-eol) 'dired-filename t))
	      (delete-region
	       start
	       (or (text-property-any start (point-at-eol) 'dired-filename t)
		   (point-at-eol)))
	      (if (= (point-at-bol) (point-at-eol))
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
              (let ((tramp-verbose (if visit 0 tramp-verbose)))
	        (tramp-compat-file-missing v filename))

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
			     #'insert-file-contents
			     (list localname visit beg end replace)))

		    ;; When we shall insert only a part of the file, we
		    ;; copy this part.  This works only for the shell file
		    ;; name handlers.  It doesn't work for crypted files.
		    (when (and (or beg end)
			       (tramp-sh-file-name-handler-p v)
			       (null tramp-crypt-enabled))
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
				v remote-copy 'nohop)))
			     ((stringp tramp-temp-buffer-file-name)
			      (copy-file
			       filename tramp-temp-buffer-file-name 'ok)
			      tramp-temp-buffer-file-name)
			     (t (file-local-copy filename)))))

		    ;; When the file is not readable for the owner, it
		    ;; cannot be inserted, even if it is readable for the
		    ;; group or for everybody.
		    (set-file-modes local-copy #o0600)

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
	(when visit
	  (setq buffer-file-name filename
		buffer-read-only (not (file-writable-p filename)))
	  (set-visited-file-modtime)
	  (set-buffer-modified-p nil))
	(when (and (stringp local-copy)
		   (or remote-copy (null tramp-temp-buffer-file-name)))
	  (delete-file local-copy))
	(when (stringp remote-copy)
	  (delete-file (tramp-make-tramp-file-name v remote-copy 'nohop))))

      ;; Result.
      (cons filename (cdr result)))))

(defun tramp-get-lock-file (file)
  "Read lockfile info of FILE.
Return nil when there is no lockfile."
  (when-let ((lockname (tramp-compat-make-lock-file-name file)))
    (or (file-symlink-p lockname)
	(and (file-readable-p lockname)
	     (with-temp-buffer
	       (insert-file-contents-literally lockname)
	       (buffer-string))))))

(defun tramp-get-lock-pid (file)
  "Determine pid for lockfile of FILE."
  ;; Some Tramp methods do not offer a connection process, but just a
  ;; network process as a place holder.  Those processes use the
  ;; "lock-pid" connection property as fake pid, in fact it is the
  ;; time stamp the process is created.
  (let ((p (tramp-get-process  (tramp-dissect-file-name file))))
    (number-to-string
     (or (process-id p)
	 (tramp-get-connection-property p "lock-pid" (emacs-pid))))))

(defconst tramp-lock-file-info-regexp
  ;; USER@HOST.PID[:BOOT_TIME]
  "\\`\\(.+\\)@\\(.+\\)\\.\\([[:digit:]]+\\)\\(?::\\([[:digit:]]+\\)\\)?\\'"
  "The format of a lock file.")

(defun tramp-handle-file-locked-p (file)
  "Like `file-locked-p' for Tramp files."
  (when-let ((info (tramp-get-lock-file file))
	     (match (string-match tramp-lock-file-info-regexp info)))
    (or (and (string-equal (match-string 1 info) (user-login-name))
	     (string-equal (match-string 2 info) (system-name))
	     (string-equal (match-string 3 info) (tramp-get-lock-pid file)))
	(match-string 1 info))))

(defun tramp-handle-lock-file (file)
  "Like `lock-file' for Tramp files."
  ;; See if this file is visited and has changed on disk since it
  ;; was visited.
  (catch 'dont-lock
    (unless (eq (file-locked-p file) t) ;; Locked by me.
      (when-let ((info (tramp-get-lock-file file))
		 (match (string-match tramp-lock-file-info-regexp info)))
	(unless (ask-user-about-lock
		 file (format
		       "%s@%s (pid %s)" (match-string 1 info)
		       (match-string 2 info) (match-string 3 info)))
	  (throw 'dont-lock nil)))

      (when-let ((lockname (tramp-compat-make-lock-file-name file))
	         ;; USER@HOST.PID[:BOOT_TIME]
	         (info
	          (format
	           "%s@%s.%s" (user-login-name) (system-name)
	           (tramp-get-lock-pid file))))

	;; Protect against security hole.
	(with-parsed-tramp-file-name file nil
	  (when (and (not tramp-allow-unsafe-temporary-files)
		     create-lockfiles
		     (file-in-directory-p lockname temporary-file-directory)
		     (zerop (or (tramp-compat-file-attribute-user-id
				 (file-attributes file 'integer))
				tramp-unknown-id-integer))
		     (not (with-tramp-connection-property
			      (tramp-get-process v) "unsafe-temporary-file"
			    (yes-or-no-p
			     (concat
			      "Lock file on local temporary directory, "
			      "do you want to continue?")))))
	    (tramp-error v 'file-error "Unsafe lock file name")))

	;; Do the lock.
        (let ((tramp-verbose 0)
              create-lockfiles signal-hook-function)
	  (condition-case nil
	      (make-symbolic-link info lockname 'ok-if-already-exists)
	    (error
	     (with-file-modes #o0644
               (write-region info nil lockname)))))))))

(defun tramp-handle-make-lock-file-name (file)
  "Like `make-lock-file-name' for Tramp files."
  (and create-lockfiles
       ;; This variable has been introduced with Emacs 28.1.
       (not (bound-and-true-p remote-file-name-inhibit-locks))
       (tramp-run-real-handler 'make-lock-file-name (list file))))

(defun tramp-handle-unlock-file (file)
  "Like `unlock-file' for Tramp files."
  (when-let ((lockname (tramp-compat-make-lock-file-name file)))
    (condition-case err
        (delete-file lockname)
      ;; `userlock--handle-unlock-error' exists since Emacs 28.1.
      (error (tramp-compat-funcall 'userlock--handle-unlock-error err)))))

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
		  (string-match-p "\\.elc?\\'" file))
	(tramp-error
	 v 'file-error
	 "File `%s' does not include a `.el' or `.elc' suffix" file)))
    (unless (or noerror (file-exists-p file))
      (tramp-compat-file-missing v file))
    (if (not (file-exists-p file))
	nil
      (let ((signal-hook-function (unless noerror signal-hook-function))
	    (inhibit-message (or inhibit-message nomessage)))
	(with-tramp-progress-reporter v 0 (format "Loading %s" file)
	  (let ((local-copy (file-local-copy file)))
	    (unwind-protect
		(load local-copy noerror t nosuffix must-suffix)
	      (delete-file local-copy)))))
      t)))

(defun tramp-multi-hop-p (vec)
  "Whether the method of VEC is capable of multi-hops."
  (and (tramp-sh-file-name-handler-p vec)
       (not (tramp-get-method-parameter vec 'tramp-copy-program))))

(defun tramp-compute-multi-hops (vec)
  "Expands VEC according to `tramp-default-proxies-alist'."
  (let ((saved-tdpa tramp-default-proxies-alist)
	(target-alist `(,vec))
	(hops (or (tramp-file-name-hop vec) ""))
	(item vec)
	choices proxy)

    ;; Ad-hoc proxy definitions.
    (dolist (proxy (reverse (split-string hops tramp-postfix-hop-regexp 'omit)))
      (let* ((host-port (tramp-file-name-host-port item))
	     (user-domain (tramp-file-name-user-domain item))
	     (proxy (concat
		     tramp-prefix-format proxy tramp-postfix-host-format))
	     (entry
	      (list (and (stringp host-port)
			 (concat "^" (regexp-quote host-port) "$"))
		    (and (stringp user-domain)
			 (concat "^" (regexp-quote user-domain) "$"))
		    (propertize proxy 'tramp-ad-hoc t))))
	(tramp-message vec 5 "Add %S to `tramp-default-proxies-alist'" entry)
	;; Add the hop.
	(add-to-list 'tramp-default-proxies-alist entry)
	(setq item (tramp-dissect-file-name proxy))))
    ;; Save the new value.
    (when (and hops tramp-save-ad-hoc-proxies)
      (customize-save-variable
       'tramp-default-proxies-alist tramp-default-proxies-alist))

    ;; Look for proxy hosts to be passed.
    (setq choices tramp-default-proxies-alist)
    (while choices
      (setq item (pop choices)
	    proxy (eval (nth 2 item) t))
      (when (and
	     ;; Host.
	     (string-match-p
	      (or (eval (nth 0 item) t) "")
	      (or (tramp-file-name-host-port (car target-alist)) ""))
	     ;; User.
	     (string-match-p
	      (or (eval (nth 1 item) t) "")
	      (or (tramp-file-name-user-domain (car target-alist)) "")))
	(if (null proxy)
	    ;; No more hops needed.
	    (setq choices nil)
	  ;; Replace placeholders.
	  (setq proxy
		(format-spec
		 proxy
		 (format-spec-make
		  ?u (or (tramp-file-name-user (car target-alist)) "")
		  ?h (or (tramp-file-name-host (car target-alist)) ""))))
	  (with-parsed-tramp-file-name proxy l
	    ;; Add the hop.
	    (push l target-alist)
	    ;; Start next search.
	    (setq choices tramp-default-proxies-alist)))))

    ;; Foreign and out-of-band methods are not supported for multi-hops.
    (when (cdr target-alist)
      (setq choices target-alist)
      (while (setq item (pop choices))
	(unless (tramp-multi-hop-p item)
	  (setq tramp-default-proxies-alist saved-tdpa)
	  (tramp-user-error
	   vec "Method `%s' is not supported for multi-hops."
	   (tramp-file-name-method item)))))

    ;; Some methods ("su", "sg", "sudo", "doas", "ksu") do not use the
    ;; host name in their command template.  In this case, the remote
    ;; file name must use either a local host name (first hop), or a
    ;; host name matching the previous hop.
    (let ((previous-host (or tramp-local-host-regexp "")))
      (setq choices target-alist)
      (while (setq item (pop choices))
	(let ((host (tramp-file-name-host item)))
	  (unless
	      (or
	       ;; The host name is used for the remote shell command.
	       (member
		'("%h") (tramp-get-method-parameter item 'tramp-login-args))
	       ;; The host name must match previous hop.
	       (string-match-p previous-host host))
	    (setq tramp-default-proxies-alist saved-tdpa)
	    (tramp-user-error
	     vec "Host name `%s' does not match `%s'" host previous-host))
	  (setq previous-host (concat "^" (regexp-quote host) "$")))))

    ;; Result.
    target-alist))

(defun tramp-expand-args (vec parameter &rest spec-list)
  "Expand login arguments as given by PARAMETER in `tramp-methods'.
PARAMETER is a symbol like `tramp-login-args', denoting a list of
list of strings from `tramp-methods', containing %-sequences for
substitution.  SPEC-LIST is a list of char/value pairs used for
`format-spec-make'."
  (let ((args (tramp-get-method-parameter vec parameter))
	(spec (apply 'format-spec-make spec-list)))
    ;; Expand format spec.
    (tramp-compat-flatten-tree
     (mapcar
      (lambda (x)
	(setq x (mapcar (lambda (y) (format-spec y spec)) x))
	(unless (member "" x) x))
      args))))

(defun tramp-direct-async-process-p (&rest args)
  "Whether direct async `make-process' can be called."
  (let ((v (tramp-dissect-file-name default-directory))
	(buffer (plist-get args :buffer))
	(stderr (plist-get args :stderr)))
    (and ;; The method supports it.
         (tramp-get-method-parameter v 'tramp-direct-async)
	 ;; It has been indicated.
         (tramp-get-connection-property v "direct-async-process" nil)
	 ;; There's no multi-hop.
	 (or (not (tramp-multi-hop-p v))
	     (= (length (tramp-compute-multi-hops v)) 1))
	 ;; There's no remote stdout or stderr file.
	 (or (not (stringp buffer)) (not (tramp-tramp-file-p buffer)))
	 (or (not (stringp stderr)) (not (tramp-tramp-file-p stderr))))))

(defun tramp-handle-make-process (&rest args)
  "An alternative `make-process' implementation for Tramp files."
  (when args
    (with-parsed-tramp-file-name (expand-file-name default-directory) nil
      (let ((default-directory tramp-compat-temporary-file-directory)
	    (name (plist-get args :name))
	    (buffer (plist-get args :buffer))
	    (command (plist-get args :command))
	    (coding (plist-get args :coding))
	    (noquery (plist-get args :noquery))
	    (connection-type
	     (or (plist-get args :connection-type) process-connection-type))
	    (filter (plist-get args :filter))
	    (sentinel (plist-get args :sentinel))
	    (stderr (plist-get args :stderr)))
	(unless (stringp name)
	  (signal 'wrong-type-argument (list #'stringp name)))
	(unless (or (bufferp buffer) (string-or-null-p buffer))
	  (signal 'wrong-type-argument (list #'bufferp buffer)))
	(unless (consp command)
	  (signal 'wrong-type-argument (list #'consp command)))
	(unless (or (null coding)
		    (and (symbolp coding) (memq coding coding-system-list))
		    (and (consp coding)
			 (memq (car coding) coding-system-list)
			 (memq (cdr coding) coding-system-list)))
	  (signal 'wrong-type-argument (list #'symbolp coding)))
	(when (eq connection-type t)
	  (setq connection-type 'pty))
	(unless (memq connection-type '(nil pipe pty))
	  (signal 'wrong-type-argument (list #'symbolp connection-type)))
	(unless (or (null filter) (eq filter t) (functionp filter))
	  (signal 'wrong-type-argument (list #'functionp filter)))
	(unless (or (null sentinel) (functionp sentinel))
	  (signal 'wrong-type-argument (list #'functionp sentinel)))
	(unless (or (null stderr) (bufferp stderr))
	  (signal 'wrong-type-argument (list #'bufferp stderr)))

	(let* ((buffer
		(if buffer
		    (get-buffer-create buffer)
		  ;; BUFFER can be nil.  We use a temporary buffer.
		  (generate-new-buffer tramp-temp-buffer-name)))
	       (env (mapcar
		     (lambda (elt)
		       (when (tramp-compat-string-search "=" elt) elt))
		     tramp-remote-process-environment))
	       ;; We use as environment the difference to toplevel
	       ;; `process-environment'.
	       (env (dolist (elt process-environment env)
		      (when
			  (and
			   (tramp-compat-string-search "=" elt)
			   (not
			    (member
			     elt (default-toplevel-value 'process-environment))))
			(setq env (cons elt env)))))
	       (env (setenv-internal
		     env "INSIDE_EMACS" (tramp-inside-emacs) 'keep))
	       (env (mapcar #'tramp-shell-quote-argument (delq nil env)))
	       ;; Quote command.
	       (command (mapconcat #'tramp-shell-quote-argument command " "))
	       ;; Set cwd and environment variables.
	       (command
	        (append `("cd" ,localname "&&" "(" "env") env `(,command ")"))))

	  ;; Check for `tramp-sh-file-name-handler', because something
	  ;; is different between tramp-sh.el, and tramp-adb.el or
	  ;; tramp-sshfs.el.
	  (let* ((sh-file-name-handler-p (tramp-sh-file-name-handler-p v))
		 (login-program
		  (tramp-get-method-parameter v 'tramp-login-program))
		 ;; We don't create the temporary file.  In fact, it
		 ;; is just a prefix for the ControlPath option of
		 ;; ssh; the real temporary file has another name, and
		 ;; it is created and protected by ssh.  It is also
		 ;; removed by ssh when the connection is closed.  The
		 ;; temporary file name is cached in the main
		 ;; connection process, therefore we cannot use
		 ;; `tramp-get-connection-process'.
		 (tmpfile
		  (when sh-file-name-handler-p
		    (with-tramp-connection-property
			(tramp-get-process v) "temp-file"
		      (tramp-compat-make-temp-name))))
		 (options
		  (when sh-file-name-handler-p
		    (tramp-compat-funcall
		     'tramp-ssh-controlmaster-options v)))
		 login-args p)

	    ;; Replace `login-args' place holders.  Split
	    ;; ControlMaster options.
	    (setq
	     login-args
	     (append
	      (tramp-compat-flatten-tree
	       (tramp-get-method-parameter v 'tramp-async-args))
	      (tramp-compat-flatten-tree
	       (mapcar
		(lambda (x) (split-string x " "))
		(tramp-expand-args
		 v 'tramp-login-args
		 ?h (or host "") ?u (or user "") ?p (or port "")
		 ?c (format-spec (or options "") (format-spec-make ?t tmpfile))
		 ?l ""))))
	     p (make-process
		:name name :buffer buffer
		:command (append `(,login-program) login-args command)
		:coding coding :noquery noquery :connection-type connection-type
		:sentinel sentinel :stderr stderr))
	    ;; Set filter.  Prior Emacs 29.1, it doesn't work reliable
	    ;; to provide it as `make-process' argument when filter is
	    ;; t.  See Bug#51177.
	    (when filter
	      (set-process-filter p filter))

	    (tramp-message v 6 "%s" (string-join (process-command p) " "))
	    p))))))

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
     #'make-symbolic-link (list target linkname ok-if-already-exists))))

(defun tramp-handle-shell-command (command &optional output-buffer error-buffer)
  "Like `shell-command' for Tramp files."
  (let* ((asynchronous (string-match-p "[ \t]*&[ \t]*\\'" command))
	 (command (substring command 0 asynchronous))
	 current-buffer-p
	 (output-buffer-p output-buffer)
	 (output-buffer
	  (cond
	   ((bufferp output-buffer)
	    (setq current-buffer-p (eq (current-buffer) output-buffer))
	    output-buffer)
	   ((stringp output-buffer)
	    (setq current-buffer-p
		  (eq (buffer-name (current-buffer)) output-buffer))
	    (get-buffer-create output-buffer))
	   (output-buffer
	    (setq current-buffer-p t)
	    (current-buffer))
	   (t (get-buffer-create
	       ;; These variables have been introduced with Emacs 28.1.
	       (if asynchronous
		   (or (bound-and-true-p shell-command-buffer-name-async)
		       "*Async Shell Command*")
		 (or (bound-and-true-p shell-command-buffer-name)
		     "*Shell Command Output*"))))))
	 (error-buffer
	  (cond
	   ((bufferp error-buffer) error-buffer)
	   ((stringp error-buffer) (get-buffer-create error-buffer))))
	 (error-file
	  (and error-buffer
	       (with-parsed-tramp-file-name default-directory nil
		 (tramp-make-tramp-file-name
		  v (tramp-make-tramp-temp-file v)))))
	 (bname (buffer-name output-buffer))
	 (p (get-buffer-process output-buffer))
	 (dir default-directory)
	 buffer)

    ;; The following code is taken from `shell-command', slightly
    ;; adapted.  Shouldn't it be factored out?
    (when (and (integerp asynchronous) p)
      (cond
       ((eq async-shell-command-buffer 'confirm-kill-process)
	;; If will kill a process, query first.
	(if (yes-or-no-p
	     "A command is running in the default buffer.  Kill it?")
	    (kill-process p)
	  (tramp-user-error p "Shell command in progress")))
       ((eq async-shell-command-buffer 'confirm-new-buffer)
	;; If will create a new buffer, query first.
	(if (yes-or-no-p
	     "A command is running in the default buffer.  Use a new buffer?")
            (setq output-buffer (generate-new-buffer bname))
	  (tramp-user-error p "Shell command in progress")))
       ((eq async-shell-command-buffer 'new-buffer)
	;; It will create a new buffer.
        (setq output-buffer (generate-new-buffer bname)))
       ((eq async-shell-command-buffer 'confirm-rename-buffer)
	;; If will rename the buffer, query first.
	(if (yes-or-no-p
	     "A command is running in the default buffer.  Rename it?")
	    (progn
	      (with-current-buffer output-buffer
		(rename-uniquely))
              (setq output-buffer (get-buffer-create bname)))
	  (tramp-user-error p "Shell command in progress")))
       ((eq async-shell-command-buffer 'rename-buffer)
	;; It will rename the buffer.
	(with-current-buffer output-buffer
	  (rename-uniquely))
        (setq output-buffer (get-buffer-create bname)))))

    (unless output-buffer-p
      (with-current-buffer output-buffer
	(setq default-directory dir)))

    (setq buffer (if error-file (list output-buffer error-file) output-buffer))

    (with-current-buffer output-buffer
      (when current-buffer-p
	(barf-if-buffer-read-only)
	(push-mark nil t))
      ;; `shell-command-save-pos-or-erase' has been introduced with
      ;; Emacs 27.1.
      (if (fboundp 'shell-command-save-pos-or-erase)
	  (tramp-compat-funcall
	   'shell-command-save-pos-or-erase current-buffer-p)
	(setq buffer-read-only nil)
	(erase-buffer)))

    (if (integerp asynchronous)
	(let ((tramp-remote-process-environment
	       ;; `async-shell-command-width' has been introduced with
	       ;; Emacs 27.1.
	       (if (natnump (bound-and-true-p async-shell-command-width))
		   (cons (format "COLUMNS=%d"
				 (bound-and-true-p async-shell-command-width))
			 tramp-remote-process-environment)
		 tramp-remote-process-environment)))
	  (prog1
	      ;; Run the process.
	      (setq p (start-file-process-shell-command
		       (buffer-name output-buffer) buffer command))
	    ;; Insert error messages if they were separated.
	    (when error-file
	      (with-current-buffer error-buffer
		(insert-file-contents-literally error-file)))
	    (if (process-live-p p)
	      ;; Display output.
	      (with-current-buffer output-buffer
		(setq mode-line-process '(":%s"))
		(unless (eq major-mode 'shell-mode)
		  (shell-mode))
		(set-process-filter p #'comint-output-filter)
		(set-process-sentinel p #'shell-command-sentinel)
		(when error-file
		  (add-function
		   :after (process-sentinel p)
		   (lambda (_proc _string)
		     (with-current-buffer error-buffer
		       (insert-file-contents-literally
			error-file nil nil nil 'replace))
		     (delete-file error-file))))
		(display-buffer output-buffer '(nil (allow-no-window . t))))

	      (when error-file
		(delete-file error-file)))))

      (prog1
	  ;; Run the process.
	  (process-file-shell-command command nil buffer nil)
	;; Insert error messages if they were separated.
	(when error-file
	  (with-current-buffer error-buffer
	    (insert-file-contents-literally error-file))
	  (delete-file error-file))
	(if current-buffer-p
	    ;; This is like exchange-point-and-mark, but doesn't
	    ;; activate the mark.  It is cleaner to avoid activation,
	    ;; even though the command loop would deactivate the mark
	    ;; because we inserted text.
	    (progn
	      (goto-char (prog1 (mark t)
			   (set-marker (mark-marker) (point)
				       (current-buffer))))
              ;; `shell-command-set-point-after-cmd' has been
	      ;; introduced with Emacs 27.1.
	      (if (fboundp 'shell-command-set-point-after-cmd)
		  (tramp-compat-funcall
		   'shell-command-set-point-after-cmd)))
	  ;; There's some output, display it.
	  (when (with-current-buffer output-buffer (> (point-max) (point-min)))
	    (display-message-or-buffer output-buffer)))))))

(defun tramp-handle-start-file-process (name buffer program &rest args)
  "Like `start-file-process' for Tramp files.
BUFFER might be a list, in this case STDERR is separated."
  ;; `make-process' knows the `:file-handler' argument since Emacs
  ;; 27.1 only.  Therefore, we invoke it via `tramp-file-name-handler'.
  (tramp-file-name-handler
   'make-process
   :name name
   :buffer (if (consp buffer) (car buffer) buffer)
   :command (and program (cons program args))
   ;; `shell-command' adds an errfile to `buffer'.
   :stderr (when (consp buffer) (cadr buffer))
   :noquery nil
   :file-handler t))

(defun tramp-handle-substitute-in-file-name (filename)
  "Like `substitute-in-file-name' for Tramp files.
\"//\" and \"/~\" substitute only in the local filename part."
  ;; Check, whether the local part is a quoted file name.
  (if (tramp-compat-file-name-quoted-p filename)
      filename
    ;; First, we must replace environment variables.
    (setq filename (tramp-replace-environment-variables filename))
    (with-parsed-tramp-file-name filename nil
      ;; We do not want to replace environment variables, again.  "//"
      ;; has a special meaning at the beginning of a file name on
      ;; Cygwin and MS-Windows, we must remove it.
      (let (process-environment)
	;; Ignore in LOCALNAME everything before "//" or "/~".
	(when (stringp localname)
	  (if (string-match "//\\(/\\|~\\)" localname)
	      (setq filename
                    (replace-regexp-in-string
                     "\\`/+" "/" (substitute-in-file-name localname)))
	    (setq filename
		  (concat (file-remote-p filename)
			  (replace-regexp-in-string
                           "\\`/+" "/"
			   ;; We must disable cygwin-mount file name
			   ;; handlers and alike.
			   (tramp-run-real-handler
			    #'substitute-in-file-name (list localname))))))))
      ;; "/m:h:~" does not work for completion.  We use "/m:h:~/".
      (if (and (stringp localname) (string-equal "~" localname))
	  (concat filename "/")
	filename))))

(defconst tramp-time-dont-know '(0 0 0 1000)
  "An invalid time value, used as \"Don't know\" value.")

(defconst tramp-time-doesnt-exist '(-1 65535)
  "An invalid time value, used as \"Doesn't exist\" value.")

(defun tramp-handle-set-visited-file-modtime (&optional time-list)
  "Like `set-visited-file-modtime' for Tramp files."
  (unless (buffer-file-name)
    (error "Can't set-visited-file-modtime: buffer `%s' not visiting a file"
	   (buffer-name)))
  (unless time-list
    (let ((remote-file-name-inhibit-cache t))
      (setq time-list
	    (or (tramp-compat-file-attribute-modification-time
		 (file-attributes (buffer-file-name)))
		tramp-time-doesnt-exist))))
  (unless (tramp-compat-time-equal-p time-list tramp-time-dont-know)
    (tramp-run-real-handler #'set-visited-file-modtime (list time-list))))

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
	   ((and attr
		 (not (tramp-compat-time-equal-p modtime tramp-time-dont-know)))
	    (< (abs (tramp-time-diff modtime mt)) 2))
	   ;; Modtime has the don't know value.
	   (attr t)
	   ;; If file does not exist, say it is not modified if and
	   ;; only if that agrees with the buffer's record.
	   (t (tramp-compat-time-equal-p mt tramp-time-doesnt-exist))))))))

(defun tramp-handle-write-region
  (start end filename &optional append visit lockname mustbenew)
  "Like `write-region' for Tramp files."
  (setq filename (expand-file-name filename)
	lockname (file-truename (or lockname filename)))
  (with-parsed-tramp-file-name filename nil
    (when (and mustbenew (file-exists-p filename)
	       (or (eq mustbenew 'excl)
		   (not
		    (y-or-n-p
		     (format "File %s exists; overwrite anyway?" filename)))))
      (tramp-error v 'file-already-exists filename))

    (let ((file-locked (eq (file-locked-p lockname) t))
	  (tmpfile (tramp-compat-make-temp-file filename))
	  (modes (tramp-default-file-modes
		  filename (and (eq mustbenew 'excl) 'nofollow)))
	  (uid (or (tramp-compat-file-attribute-user-id
		    (file-attributes filename 'integer))
		   (tramp-get-remote-uid v 'integer)))
	  (gid (or (tramp-compat-file-attribute-group-id
		    (file-attributes filename 'integer))
		   (tramp-get-remote-gid v 'integer))))

      ;; Lock file.
      (when (and (not (auto-save-file-name-p (file-name-nondirectory filename)))
		 (file-remote-p lockname)
		 (not file-locked))
	(setq file-locked t)
	;; `lock-file' exists since Emacs 28.1.
	(tramp-compat-funcall 'lock-file lockname))

      (when (and append (file-exists-p filename))
	(copy-file filename tmpfile 'ok))
      ;; The permissions of the temporary file should be set.  If
      ;; FILENAME does not exist (eq modes nil) it has been
      ;; renamed to the backup file.  This case `save-buffer'
      ;; handles permissions.
      ;; Ensure that it is still readable.
      (set-file-modes tmpfile (logior (or modes 0) #o0400))
      ;; We say `no-message' here because we don't want the visited file
      ;; modtime data to be clobbered from the temp file.  We call
      ;; `set-visited-file-modtime' ourselves later on.
      (let (create-lockfiles)
        (write-region start end tmpfile append 'no-message))
      (condition-case nil
	  (rename-file tmpfile filename 'ok-if-already-exists)
	(error
	 (delete-file tmpfile)
	 (tramp-error
	  v 'file-error "Couldn't write region to `%s'" filename)))

      (tramp-flush-file-properties v localname)

      ;; Set file modification time.
      (when (or (eq visit t) (stringp visit))
	(set-visited-file-modtime
	 (or (tramp-compat-file-attribute-modification-time
	      (file-attributes filename))
	     (current-time))))

      ;; Set the ownership.
      (tramp-set-file-uid-gid filename uid gid)

      ;; Unlock file.
      (when file-locked
	;; `unlock-file' exists since Emacs 28.1.
	(tramp-compat-funcall 'unlock-file lockname))

      ;; The end.
      (when (and (null noninteractive)
		 (or (eq visit t) (string-or-null-p visit)))
	(tramp-message v 0 "Wrote %s" filename))
      (run-hooks 'tramp-handle-write-region-hook))))

;; This is used in tramp-sh.el and tramp-sudoedit.el.
(defconst tramp-stat-marker "/////"
  "Marker in stat commands for file attributes.")

(defconst tramp-stat-quoted-marker "\\/\\/\\/\\/\\/"
  "Quoted marker in stat commands for file attributes.")

;; This is used in tramp-gvfs.el and tramp-sh.el.
(defconst tramp-gio-events
  '("attribute-changed" "changed" "changes-done-hint"
    "created" "deleted" "moved" "pre-unmount" "unmounted")
  "List of events \"gio monitor\" could send.")

;; This is the default handler.  tramp-gvfs.el and tramp-sh.el have
;; their own one.
(defun tramp-handle-file-notify-add-watch (filename _flags _callback)
  "Like `file-notify-add-watch' for Tramp files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (tramp-error
     v 'file-notify-error "File notification not supported for `%s'" filename)))

(defun tramp-handle-file-notify-rm-watch (proc)
  "Like `file-notify-rm-watch' for Tramp files."
  ;; The descriptor must be a process object.
  (unless (processp proc)
    (tramp-error proc 'file-notify-error "Not a valid descriptor %S" proc))
  ;; There might be pending output.
  (while (tramp-accept-process-output proc 0))
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

(defun tramp-file-notify-process-sentinel (proc event)
  "Call `file-notify-rm-watch'."
  (unless (process-live-p proc)
    (tramp-message proc 5 "Sentinel called: `%S' `%s'" proc event)
    (file-notify-rm-watch proc)))

;;; Functions for establishing connection:

;; The following functions are actions to be taken when seeing certain
;; prompts from the remote host.  See the variable
;; `tramp-actions-before-shell' for usage of these functions.

(defvar tramp-process-action-regexp nil
  "The regexp used to invoke an action in `tramp-process-one-action'.")

(defun tramp-action-login (_proc vec)
  "Send the login name."
  (let ((user (or (tramp-file-name-user vec)
		  (with-tramp-connection-property vec "login-as"
		    (save-window-excursion
		      (pop-to-buffer (tramp-get-connection-buffer vec))
		      (read-string (match-string 0)))))))
    (with-current-buffer (tramp-get-connection-buffer vec)
      (tramp-message vec 6 "\n%s" (buffer-string)))
    (tramp-message vec 3 "Sending login name `%s'" user)
    (tramp-send-string vec (concat user tramp-local-end-of-line)))
  t)

(defun tramp-action-password (proc vec)
  "Query the user for a password."
  (with-current-buffer (process-buffer proc)
    (let ((case-fold-search t))
      ;; Let's check whether a wrong password has been sent already.
      ;; Sometimes, the process returns a new password request
      ;; immediately after rejecting the previous (wrong) one.
      (unless (tramp-get-connection-property vec "first-password-request" nil)
	(tramp-clear-passwd vec))
      (goto-char (point-min))
      (tramp-check-for-regexp proc tramp-process-action-regexp)
      (tramp-message vec 3 "Sending %s" (match-string 1))
      ;; We don't call `tramp-send-string' in order to hide the
      ;; password from the debug buffer and the traces.
      (process-send-string
       proc (concat (tramp-read-passwd proc) tramp-local-end-of-line))
      ;; Hide password prompt.
      (narrow-to-region (point-max) (point-max))))
  t)

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
    (pop-to-buffer (tramp-get-connection-buffer vec))
    (unless (yes-or-no-p (match-string 0))
      (kill-process proc)
      (throw 'tramp-action 'permission-denied))
    (with-current-buffer (tramp-get-connection-buffer vec)
      (tramp-message vec 6 "\n%s" (buffer-string)))
    (tramp-send-string vec (concat "yes" tramp-local-end-of-line)))
  t)

(defun tramp-action-yn (proc vec)
  "Ask the user for confirmation using `y-or-n-p'.
Send \"y\" to remote process on confirmation, abort otherwise.
See also `tramp-action-yesno'."
  (save-window-excursion
    (pop-to-buffer (tramp-get-connection-buffer vec))
    (unless (y-or-n-p (match-string 0))
      (kill-process proc)
      (throw 'tramp-action 'permission-denied))
    (with-current-buffer (tramp-get-connection-buffer vec)
      (tramp-message vec 6 "\n%s" (buffer-string)))
    (tramp-send-string vec (concat "y" tramp-local-end-of-line)))
  t)

(defun tramp-action-terminal (_proc vec)
  "Tell the remote host which terminal type to use.
The terminal type can be configured with `tramp-terminal-type'."
  (tramp-message vec 5 "Setting `%s' as terminal type." tramp-terminal-type)
  (with-current-buffer (tramp-get-connection-buffer vec)
    (tramp-message vec 6 "\n%s" (buffer-string)))
  (tramp-send-string vec (concat tramp-terminal-type tramp-local-end-of-line))
  t)

(defun tramp-action-confirm-message (_proc vec)
  "Return RET in order to confirm the message."
  (with-current-buffer (tramp-get-connection-buffer vec)
    (tramp-message vec 6 "\n%s" (buffer-string)))
  (tramp-send-string vec tramp-local-end-of-line)
  t)

(defun tramp-action-show-and-confirm-message (proc vec)
  "Show the user a message for confirmation.
Wait, until the connection buffer changes."
  (with-current-buffer (process-buffer proc)
    (let ((stimers (with-timeout-suspend))
	  (cursor-in-echo-area t)
	  set-message-function clear-message-function)
      ;; Silence byte compiler.
      (ignore set-message-function clear-message-function)
      (tramp-message vec 6 "\n%s" (buffer-string))
      (tramp-check-for-regexp proc tramp-process-action-regexp)
      (with-temp-message (replace-regexp-in-string "[\r\n]" "" (match-string 0))
	;; Hide message in buffer.
	(narrow-to-region (point-max) (point-max))
	;; Wait for new output.
	(while (not (tramp-compat-ignore-error 'file-error
		      (tramp-wait-for-regexp
		       proc 0.1 tramp-security-key-confirmed-regexp)))
	  (when (tramp-check-for-regexp proc tramp-security-key-timeout-regexp)
	    (throw 'tramp-action 'timeout))
	  (redisplay 'force)))
      ;; Reenable the timers.
      (with-timeout-unsuspend stimers)))
  t)

(defun tramp-action-process-alive (proc _vec)
  "Check, whether a process has finished."
  (unless (process-live-p proc)
    ;; There might be pending output.
    (while (tramp-accept-process-output proc 0))
    (throw 'tramp-action 'process-died)))

(defun tramp-action-out-of-band (proc vec)
  "Check, whether an out-of-band copy has finished."
  ;; There might be pending output for the exit status.
  (while (tramp-accept-process-output proc 0))
  (cond ((and (not (process-live-p proc))
	      (zerop (process-exit-status proc)))
	 (tramp-message	vec 3 "Process has finished.")
	 (throw 'tramp-action 'ok))
	((or (and (memq (process-status proc) '(stop exit))
		  (not (zerop (process-exit-status proc))))
	     (eq (process-status proc) 'signal))
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
  "Wait for output from the shell and perform one action.
See `tramp-process-actions' for the format of ACTIONS."
  (let ((case-fold-search t)
	tramp-process-action-regexp
	found todo item pattern action)
    (while (not found)
      ;; Reread output once all actions have been performed.
      ;; Obviously, the output was not complete.
      (while (tramp-accept-process-output proc 0))
      (setq todo actions)
      (while todo
	(setq item (pop todo)
	      tramp-process-action-regexp (symbol-value (nth 0 item))
	      pattern (format "\\(%s\\)\\'" tramp-process-action-regexp)
	      action (nth 1 item))
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
connection buffer.

ACTIONS is a list of (PATTERN ACTION).  The PATTERN should be a
symbol, a variable.  The value of this variable gives the regular
expression to search for.  Note that the regexp must match at the
end of the buffer, \"\\'\" is implicitly appended to it.

The ACTION should also be a symbol, but a function.  When the
corresponding PATTERN matches, the ACTION function is called.

An ACTION function has two arguments (PROC VEC).  If it returns
nil, nothing has been done, and the next action shall be called.
A non-nil return value indicates that the process output has been
consumed, and new output shall be retrieved, before starting to
process all ACTIONs, again.  The same happens after calling the
last ACTION.

If an action determines, that all processing has been done (e.g.,
because the shell prompt has been detected), it shall throw a
result.  The symbol `ok' means that all ACTIONs have been
performed successfully.  Any other value means an error."
  ;; Enable `auth-source', unless "emacs -Q" has been called.  We must
  ;; use the "password-vector" property in case we have several hops.
  (tramp-set-connection-property
   (tramp-get-connection-property
    proc "password-vector" (process-get proc 'vector))
   "first-password-request" tramp-cache-read-persistent-data)
  (save-restriction
    (with-tramp-progress-reporter
	proc 3 "Waiting for prompts from remote shell"
      (let ((enable-recursive-minibuffers t)
	    exit)
	(if timeout
	    (with-timeout (timeout (setq exit 'timeout))
	      (while (not exit)
		(setq exit
		      (catch 'tramp-action
			(tramp-process-one-action proc vec actions)))))
	  (while (not exit)
	    (setq exit (catch 'tramp-action
			 (tramp-process-one-action proc vec actions)))))
	(with-current-buffer (tramp-get-connection-buffer vec)
	  (widen)
	  (tramp-message vec 6 "\n%s" (buffer-string)))
	(if (eq exit 'ok)
	    (ignore-errors
	      (and (functionp tramp-password-save-function)
		   (funcall tramp-password-save-function)))
	  ;; Not successful.
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
	  (let ((inhibit-read-only t)) (delete-region pos (point))))))))

;;; Utility functions:

;; In Emacs, there is some concurrency due to timers.  If a timer
;; interrupts Tramp and wishes to use the same connection buffer as
;; the "main" Emacs, then garbage might occur in the connection
;; buffer.  Therefore, we need to make sure that a timer does not use
;; the same connection buffer as the "main" Emacs.  We lock each
;; connection process separately by a connection property.

(defmacro with-tramp-locked-connection (proc &rest body)
  "Lock PROC for other communication, and run BODY.
Mostly useful to protect BODY from being interrupted by timers."
  (declare (indent 1) (debug t))
  `(if (tramp-get-connection-property ,proc "locked" nil)
       ;; Be kind for older Emacsen.
       (if (member 'remote-file-error debug-ignored-errors)
	   (throw 'non-essential 'non-essential)
	 (tramp-error
	  ,proc 'remote-file-error "Forbidden reentrant call of Tramp"))
     (unwind-protect
	 (progn
	   (tramp-set-connection-property ,proc "locked" t)
	   ,@body)
       (tramp-flush-connection-property ,proc "locked"))))

(font-lock-add-keywords
 'emacs-lisp-mode '("\\<with-tramp-locked-connection\\>"))

(defun tramp-accept-process-output (proc &optional timeout)
  "Like `accept-process-output' for Tramp processes.
This is needed in order to hide `last-coding-system-used', which is set
for process communication also.
If the user quits via `C-g', it is propagated up to `tramp-file-name-handler'."
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t)
	  last-coding-system-used
	  result)
      ;; This must be protected by the "locked" property.
      (with-tramp-locked-connection proc
	;; JUST-THIS-ONE is set due to Bug#12145.  `with-local-quit'
	;; returns t in order to report success.
	(if (with-local-quit
	      (setq result (accept-process-output proc timeout nil t)) t)
	    (tramp-message
	     proc 10 "%s %s %s %s\n%s"
	     proc timeout (process-status proc) result (buffer-string))
	  ;; Propagate quit.
	  (keyboard-quit)))
      result)))

(defun tramp-search-regexp (regexp)
  "Search for REGEXP backwards, starting at point-max.
If found, set point to the end of the occurrence found, and return point.
Otherwise, return nil."
  (goto-char (point-max))
  ;; We restrict ourselves to the last 256 characters.  There were
  ;; reports of a shell command "git ls-files -zco --exclude-standard"
  ;; with 85k files involved, which has blocked Tramp forever.
  (re-search-backward regexp (max (point-min) (- (point) 256)) 'noerror))

(defun tramp-check-for-regexp (proc regexp)
  "Check, whether REGEXP is contained in process buffer of PROC.
Erase echoed commands if exists."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-min))

    ;; Check whether we need to remove echo output.  The max length of
    ;; the echo mark regexp is taken for search.  We restrict the
    ;; search for the second echo mark to PIPE_BUF characters.
    (when (and (tramp-get-connection-property proc "check-remote-echo" nil)
	       (re-search-forward
		tramp-echoed-echo-mark-regexp
		(+ (point) (* 5 tramp-echo-mark-marker-length)) t))
      (let ((begin (match-beginning 0)))
	(when
	    (re-search-forward
	     tramp-echoed-echo-mark-regexp
	     (+ (point) (tramp-get-connection-property proc "pipe-buf" 4096)) t)
	  ;; Discard echo from remote output.
	  (tramp-flush-connection-property proc "check-remote-echo")
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
      (tramp-search-regexp regexp))))

(defun tramp-wait-for-regexp (proc timeout regexp)
  "Wait for a REGEXP to appear from process PROC within TIMEOUT seconds.
Expects the output of PROC to be sent to the current buffer.  Returns
the string that matched, or nil.  Waits indefinitely if TIMEOUT is
nil."
  (let ((found (tramp-check-for-regexp proc regexp)))
    (cond (timeout
	   (with-timeout (timeout)
	     (while (not found)
	       (tramp-accept-process-output proc)
	       (unless (process-live-p proc)
		 (tramp-error-with-buffer
		  nil proc 'file-error "Process has died"))
	       (setq found (tramp-check-for-regexp proc regexp)))))
	  (t
	   (while (not found)
	     (tramp-accept-process-output proc)
	     (unless (process-live-p proc)
	       (tramp-error-with-buffer
		nil proc 'file-error "Process has died"))
	     (setq found (tramp-check-for-regexp proc regexp)))))
    ;; The process could have timed out, for example due to session
    ;; timeout of sudo.  The process buffer does not exist any longer then.
    (ignore-errors
      (with-current-buffer (process-buffer proc)
	(tramp-message proc 6 "\n%s" (buffer-string))))
    (unless found
      (if timeout
	  (tramp-error
	   proc 'file-error "[[Regexp `%s' not found in %d secs]]"
	   regexp timeout)
	(tramp-error proc 'file-error "[[Regexp `%s' not found]]" regexp)))
    found))

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
      (let ((inhibit-read-only t)) (delete-region (point-min) (point-max)))
      ;; Replace "\n" by `tramp-rsh-end-of-line'.
      (setq string
	    (mapconcat
	     #'identity (split-string string "\n") tramp-rsh-end-of-line))
      (unless (or (string-empty-p string)
		  (string-equal (substring string -1) tramp-rsh-end-of-line))
	(setq string (concat string tramp-rsh-end-of-line)))
      ;; This must be protected by the "locked" property.
      (with-tramp-locked-connection p
	;; Send the string.
	(with-local-quit
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
	    (process-send-string p string)))))))

(defun tramp-process-sentinel (proc event)
  "Flush file caches and remove shell prompt."
  (unless (process-live-p proc)
    (let ((vec (process-get proc 'vector))
	  (buf (process-buffer proc))
	  (prompt (tramp-get-connection-property proc "prompt" nil)))
      (when vec
	(tramp-message vec 5 "Sentinel called: `%S' `%s'" proc event)
        (tramp-flush-connection-properties proc)
        (tramp-flush-directory-properties vec ""))
      (when (buffer-live-p buf)
	(with-current-buffer buf
          (when (and prompt (tramp-search-regexp (regexp-quote prompt)))
	    (delete-region (point) (point-max))))))))

(defun tramp-get-inode (vec)
  "Return the virtual inode number.
If it doesn't exist, generate a new one."
  (with-tramp-file-property vec (tramp-file-name-localname vec) "inode"
    (setq tramp-inodes (1+ tramp-inodes))))

(defun tramp-get-device (vec)
  "Return the virtual device number.
If it doesn't exist, generate a new one."
  (with-tramp-connection-property (tramp-get-connection-process vec) "device"
    (cons -1 (setq tramp-devices (1+ tramp-devices)))))

;; Comparison of vectors is performed by `tramp-file-name-equal-p'.
(defun tramp-equal-remote (file1 file2)
  "Check, whether the remote parts of FILE1 and FILE2 are identical.
The check depends on method, user and host name of the files.  If
one of the components is missing, the default values are used.
The local file name parts of FILE1 and FILE2 are not taken into
account.

Example:

  (tramp-equal-remote \"/ssh::/etc\" \"/-:<your host name>:/home\")

would yield t.  On the other hand, the following check results in nil:

  (tramp-equal-remote \"/sudo::/etc\" \"/su::/etc\")

If both files are local, the function returns t."
  (or (and (null (tramp-tramp-file-p file1)) (null (tramp-tramp-file-p file2)))
      (and (tramp-tramp-file-p file1) (tramp-tramp-file-p file2)
	   (string-equal (file-remote-p file1) (file-remote-p file2)))))

;; See also `file-modes-symbolic-to-number'.
(defun tramp-mode-string-to-int (mode-string)
  "Convert a ten-letter \"drwxrwxrwx\"-style MODE-STRING into mode bits."
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
    (logior
     (cond
      ((char-equal owner-read ?r) #o0400)
      ((char-equal owner-read ?-) 0)
      (t (error "Second char `%c' must be one of `r-'" owner-read)))
     (cond
      ((char-equal owner-write ?w) #o0200)
      ((char-equal owner-write ?-) 0)
      (t (error "Third char `%c' must be one of `w-'" owner-write)))
     (cond
      ((char-equal owner-execute-or-setid ?x) #o0100)
      ((char-equal owner-execute-or-setid ?S) #o4000)
      ((char-equal owner-execute-or-setid ?s) #o4100)
      ((char-equal owner-execute-or-setid ?-) 0)
      (t (error "Fourth char `%c' must be one of `xsS-'"
		owner-execute-or-setid)))
     (cond
      ((char-equal group-read ?r) #o0040)
      ((char-equal group-read ?-) 0)
      (t (error "Fifth char `%c' must be one of `r-'" group-read)))
     (cond
      ((char-equal group-write ?w) #o0020)
      ((char-equal group-write ?-) 0)
      (t (error "Sixth char `%c' must be one of `w-'" group-write)))
     (cond
      ((char-equal group-execute-or-setid ?x) #o0010)
      ((char-equal group-execute-or-setid ?S) #o2000)
      ((char-equal group-execute-or-setid ?s) #o2010)
      ((char-equal group-execute-or-setid ?-) 0)
      (t (error "Seventh char `%c' must be one of `xsS-'"
		group-execute-or-setid)))
     (cond
      ((char-equal other-read ?r) #o0004)
      ((char-equal other-read ?-) 0)
      (t (error "Eighth char `%c' must be one of `r-'" other-read)))
     (cond
      ((char-equal other-write ?w) #o0002)
      ((char-equal other-write ?-) 0)
      (t (error "Ninth char `%c' must be one of `w-'" other-write)))
     (cond
      ((char-equal other-execute-or-sticky ?x) #o0001)
      ((char-equal other-execute-or-sticky ?T) #o1000)
      ((char-equal other-execute-or-sticky ?t) #o1001)
      ((char-equal other-execute-or-sticky ?-) 0)
      (t (error "Tenth char `%c' must be one of `xtT-'"
		other-execute-or-sticky))))))

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

;; See also `file-modes-number-to-symbolic'.
(defun tramp-file-mode-from-int (mode)
  "Turn an integer representing a file MODE into an ls(1)-like string."
  (let ((type	(cdr
		 (assoc (logand (ash mode -12) 15) tramp-file-mode-type-map)))
	(user	(logand (ash mode -6) 7))
	(group	(logand (ash mode -3) 7))
	(other	(logand (ash mode -0) 7))
	(suid	(> (logand (ash mode -9) 4) 0))
	(sgid	(> (logand (ash mode -9) 2) 0))
	(sticky	(> (logand (ash mode -9) 1) 0)))
    (setq user  (tramp-file-mode-permissions user  suid "s")
	  group (tramp-file-mode-permissions group sgid "s")
	  other (tramp-file-mode-permissions other sticky "t"))
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

;; This is a Tramp internal function.  A general `set-file-uid-gid'
;; outside Tramp is not needed, I believe.
(defun tramp-set-file-uid-gid (filename &optional uid gid)
  "Set the ownership for FILENAME.
If UID and GID are provided, these values are used; otherwise uid
and gid of the corresponding remote or local user is taken,
depending whether FILENAME is remote or local.  Both parameters
must be non-negative integers.
The setgid bit of the upper directory is respected.
If FILENAME is remote, a file name handler is called."
  (let* ((dir (file-name-directory filename))
	 (modes (file-modes dir)))
    (when (and modes (not (zerop (logand modes #o2000))))
      (setq gid (tramp-compat-file-attribute-group-id (file-attributes dir)))))

  (if-let ((handler (find-file-name-handler filename 'tramp-set-file-uid-gid)))
      (funcall handler #'tramp-set-file-uid-gid filename uid gid)
    ;; On W32 systems, "chown" does not work.
    (unless (memq system-type '(ms-dos windows-nt))
      (let ((uid (or (and (natnump uid) uid) (tramp-get-local-uid 'integer)))
	    (gid (or (and (natnump gid) gid) (tramp-get-local-gid 'integer))))
	(tramp-call-process
	 nil "chown" nil nil nil (format "%d:%d" uid gid)
	 (tramp-unquote-shell-quote-argument filename))))))

(defun tramp-get-local-uid (id-format)
  "The uid of the local user, in ID-FORMAT.
ID-FORMAT valid values are `string' and `integer'."
  ;; We use key nil for local connection properties.
  (with-tramp-connection-property nil (format "uid-%s" id-format)
    (if (equal id-format 'integer) (user-uid) (user-login-name))))

(defun tramp-get-local-gid (id-format)
  "The gid of the local user, in ID-FORMAT.
ID-FORMAT valid values are `string' and `integer'."
  ;; We use key nil for local connection properties.
  (with-tramp-connection-property nil (format "gid-%s" id-format)
    (cond
     ((equal id-format 'integer) (group-gid))
     ;; `group-name' has been introduced with Emacs 27.1.
     ((and (fboundp 'group-name) (equal id-format 'string))
      (tramp-compat-funcall 'group-name (group-gid)))
     ((tramp-compat-file-attribute-group-id
       (file-attributes "~/" id-format))))))

(defun tramp-get-local-locale (&optional vec)
  "Determine locale, supporting UTF8 if possible.
VEC is used for tracing."
  ;; We use key nil for local connection properties.
  (with-tramp-connection-property nil "locale"
    (let ((candidates '("en_US.utf8" "C.utf8" "en_US.UTF-8"))
	  locale)
      (with-temp-buffer
	(unless (or (eq system-type 'windows-nt)
                    (not (zerop (tramp-call-process
                                 nil "locale" nil t nil "-a"))))
	  (while candidates
	    (goto-char (point-min))
	    (if (string-match-p
		 (format "^%s\r?$" (regexp-quote (car candidates)))
		 (buffer-string))
		(setq locale (car candidates)
		      candidates nil)
	      (setq candidates (cdr candidates))))))
      ;; Return value.
      (when vec (tramp-message vec 7 "locale %s" (or locale "C")))
      (or locale "C"))))

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
              (remote-uid (tramp-get-remote-uid vec (intern suffix)))
              (remote-gid (tramp-get-remote-gid vec (intern suffix)))
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

(defun tramp-get-remote-uid (vec id-format)
  "The uid of the remote connection VEC, in ID-FORMAT.
ID-FORMAT valid values are `string' and `integer'."
  (with-tramp-connection-property vec (format "uid-%s" id-format)
    (or (when-let
	    ((handler
	      (find-file-name-handler
	       (tramp-make-tramp-file-name vec) 'tramp-get-remote-uid)))
	  (funcall handler #'tramp-get-remote-uid vec id-format))
	;; Ensure there is a valid result.
	(and (equal id-format 'integer) tramp-unknown-id-integer)
	(and (equal id-format 'string) tramp-unknown-id-string))))

(defun tramp-get-remote-gid (vec id-format)
  "The gid of the remote connection VEC, in ID-FORMAT.
ID-FORMAT valid values are `string' and `integer'."
  (with-tramp-connection-property vec (format "gid-%s" id-format)
    (or (when-let
	    ((handler
	      (find-file-name-handler
	       (tramp-make-tramp-file-name vec) 'tramp-get-remote-gid)))
	  (funcall handler #'tramp-get-remote-gid vec id-format))
	;; Ensure there is a valid result.
	(and (equal id-format 'integer) tramp-unknown-id-integer)
	(and (equal id-format 'string) tramp-unknown-id-string))))

(defun tramp-local-host-p (vec)
  "Return t if this points to the local host, nil otherwise.
This handles also chrooted environments, which are not regarded as local."
  (let ((host (tramp-file-name-host vec))
	(port (tramp-file-name-port vec)))
    (and
     (stringp tramp-local-host-regexp) (stringp host)
     (string-match-p tramp-local-host-regexp host)
     ;; A port is an indication for an ssh tunnel or alike.
     (null port)
     ;; The method shall be applied to one of the shell file name
     ;; handlers.  `tramp-local-host-p' is also called for "smb" and
     ;; alike, where it must fail.
     (tramp-sh-file-name-handler-p vec)
     ;; Direct actions aren't possible for crypted directories.
     (null tramp-crypt-enabled)
     ;; The local temp directory must be writable for the other user.
     (file-writable-p
      (tramp-make-tramp-file-name
       vec tramp-compat-temporary-file-directory 'nohop))
     ;; On some systems, chown runs only for root.
     (or (zerop (user-uid))
	 (zerop (tramp-get-remote-uid vec 'integer))))))

(defun tramp-get-remote-tmpdir (vec)
  "Return directory for temporary files on the remote host identified by VEC."
  (with-tramp-connection-property vec "tmpdir"
    (let ((dir
	   (tramp-make-tramp-file-name
	    vec (or (tramp-get-method-parameter vec 'tramp-tmpdir) "/tmp"))))
      (or (and (file-directory-p dir) (file-writable-p dir)
	       (tramp-file-local-name dir))
	  (tramp-error vec 'file-error "Directory %s not accessible" dir))
      dir)))

(defun tramp-make-tramp-temp-name (vec)
  "Generate a temporary file name on the remote host identified by VEC."
  (make-temp-name
   (expand-file-name tramp-temp-name-prefix (tramp-get-remote-tmpdir vec))))

(defun tramp-make-tramp-temp-file (vec)
  "Create a temporary file on the remote host identified by VEC.
Return the local name of the temporary file."
  (let (result)
    (while (not result)
      ;; `make-temp-file' would be the natural choice for
      ;; implementation.  But it calls `write-region' internally,
      ;; which also needs a temporary file - we would end in an
      ;; infinite loop.
      (setq result (tramp-make-tramp-temp-name vec))
      (if (file-exists-p result)
	  (setq result nil)
	;; This creates the file by side effect.
	(set-file-times result)
	(set-file-modes result #o0700)))

    ;; Return the local part.
    (tramp-file-local-name result)))

(defun tramp-delete-temp-file-function ()
  "Remove temporary files related to current buffer."
  (when (stringp tramp-temp-buffer-file-name)
    (ignore-errors (delete-file tramp-temp-buffer-file-name))))

(add-hook 'kill-buffer-hook #'tramp-delete-temp-file-function)
(add-hook 'tramp-unload-hook
	  (lambda ()
	    (remove-hook 'kill-buffer-hook
			 #'tramp-delete-temp-file-function)))

(defun tramp-handle-make-auto-save-file-name ()
  "Like `make-auto-save-file-name' for Tramp files.
Returns a file name in `tramp-auto-save-directory' for autosaving
this file, if that variable is non-nil."
  (with-parsed-tramp-file-name buffer-file-name nil
    (when (stringp tramp-auto-save-directory)
      (setq tramp-auto-save-directory
	    (expand-file-name tramp-auto-save-directory)))
    ;; Create directory.
    (unless (or (null tramp-auto-save-directory)
		(file-exists-p tramp-auto-save-directory))
      (with-file-modes #o0700
        (make-directory tramp-auto-save-directory t)))

    (let ((system-type
	   (if (and (stringp tramp-auto-save-directory)
		    (tramp-tramp-file-p tramp-auto-save-directory))
	       'not-windows
	     system-type))
	  (auto-save-file-name-transforms
	   (if (null tramp-auto-save-directory)
	       auto-save-file-name-transforms))
	  (filename buffer-file-name)
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
	      tramp-auto-save-directory)))
	  result)
      (prog1 ;; Run plain `make-auto-save-file-name'.
	  (setq result (tramp-run-real-handler #'make-auto-save-file-name nil))
	;; Protect against security hole.
	(when (and (not tramp-allow-unsafe-temporary-files)
		   auto-save-default
		   (file-in-directory-p result temporary-file-directory)
		   (zerop (or (tramp-compat-file-attribute-user-id
			       (file-attributes filename 'integer))
			      tramp-unknown-id-integer))
		   (not (with-tramp-connection-property
			    (tramp-get-process v) "unsafe-temporary-file"
			  (yes-or-no-p
			   (concat
			    "Autosave file on local temporary directory, "
			    "do you want to continue?")))))
	  (tramp-error v 'file-error "Unsafe autosave file name"))))))

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
  "Call `call-process' on the local host.
It always returns a return code.  The Lisp error raised when
PROGRAM is nil is trapped also, returning 1.  Furthermore, traces
are written with verbosity of 6."
  (let ((default-directory tramp-compat-temporary-file-directory)
	(process-environment (default-toplevel-value 'process-environment))
	(destination (if (eq destination t) (current-buffer) destination))
	(vec (or vec (car tramp-current-connection)))
	output error result)
    (tramp-message
     vec 6 "`%s %s' %s %s"
     program (string-join args " ") infile destination)
    (condition-case err
	(with-temp-buffer
	  (setq result
		(apply
		 #'call-process program infile (or destination t) display args))
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
	(tramp-message vec 6 "%s\n%s" result output)
      (tramp-message vec 6 "%s\n%s\n%s" result output error))
    result))

(defun tramp-call-process-region
  (vec start end program &optional delete buffer display &rest args)
  "Call `call-process-region' on the local host.
It always returns a return code.  The Lisp error raised when
PROGRAM is nil is trapped also, returning 1.  Furthermore, traces
are written with verbosity of 6."
  (let ((default-directory tramp-compat-temporary-file-directory)
	(process-environment (default-toplevel-value 'process-environment))
	(buffer (if (eq buffer t) (current-buffer) buffer))
	result)
    (tramp-message
     vec 6 "`%s %s' %s %s %s %s"
     program (string-join args " ") start end delete buffer)
    (condition-case err
	(progn
	  (setq result
		(apply
		 #'call-process-region
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

(defun tramp-process-lines
  (vec program &rest args)
  "Call `process-lines' on the local host.
If an error occurs, it returns nil.  Traces are written with
verbosity of 6."
  (let ((default-directory tramp-compat-temporary-file-directory)
	(process-environment (default-toplevel-value 'process-environment))
	(vec (or vec (car tramp-current-connection)))
	result)
    (if args
	(tramp-message vec 6 "%s %s" program (string-join args " "))
      (tramp-message vec 6 "%s" program))
    (setq result
	  (condition-case err
	      (apply #'process-lines program args)
	    (error
	     (tramp-error vec (car err) (cdr err)))))
    (tramp-message vec 6 "%s" result)
    result))

(defun tramp-process-running-p (process-name)
  "Return t if system process PROCESS-NAME is running for `user-login-name'."
  (when (stringp process-name)
    (catch 'result
      (dolist (pid (list-system-processes))
	(when-let ((attributes (process-attributes pid))
		   (comm (cdr (assoc 'comm attributes))))
	  (and (string-equal (cdr (assoc 'user attributes)) (user-login-name))
               ;; The returned command name could be truncated to 15
               ;; characters.  Therefore, we cannot check for `string-equal'.
	       (string-prefix-p comm process-name)
	       (throw 'result t)))))))

;; When calling "emacs -Q", `auth-source-search' won't be called.  If
;; you want to debug exactly this case, call "emacs -Q --eval '(setq
;; tramp-cache-read-persistent-data t)'" instead.
(defun tramp-read-passwd (proc &optional prompt)
  "Read a password from user (compat function).
Consults the auth-source package.
Invokes `password-read' if available, `read-passwd' else."
  (let* (;; If `auth-sources' contains "~/.authinfo.gpg", and
	 ;; `exec-path' contains a relative file name like ".", it
	 ;; could happen that the "gpg" command is not found.  So we
	 ;; adapt `default-directory'.  (Bug#39389, Bug#39489)
	 (default-directory tramp-compat-temporary-file-directory)
	 (case-fold-search t)
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
	 (auth-source-creation-prompts `((secret . ,pw-prompt)))
	 ;; Use connection-local value.
	 (auth-sources (with-current-buffer (process-buffer proc) auth-sources))
	 ;; We suspend the timers while reading the password.
         (stimers (with-timeout-suspend))
	 auth-info auth-passwd)

    (unwind-protect
	(with-parsed-tramp-file-name key nil
	  (setq tramp-password-save-function nil
		user
		(or user (tramp-get-connection-property key "login-as" nil)))
	  (prog1
	      (or
	       ;; See if auth-sources contains something useful.
	       (ignore-errors
		 (and (tramp-get-connection-property
		       v "first-password-request" nil)
		      ;; Try with Tramp's current method.
		      (setq auth-info
			    (car
			     (auth-source-search
			      :max 1
			      (and user :user)
			      (if domain
				  (concat
				   user tramp-prefix-domain-format domain)
				user)
			      :host
			      (if port
				  (concat
				   host tramp-prefix-port-format port)
				host)
			      :port method
			      :require (cons :secret (and user '(:user)))
			      :create t))
			    tramp-password-save-function
			    (plist-get auth-info :save-function)
			    auth-passwd (plist-get auth-info :secret)))
		 (while (functionp auth-passwd)
		   (setq auth-passwd (funcall auth-passwd)))
		 auth-passwd)

	       ;; Try the password cache.  Exists since Emacs 26.1.
	       (progn
		 (setq auth-passwd (password-read pw-prompt key)
		       tramp-password-save-function
		       (lambda () (password-cache-add key auth-passwd)))
		 auth-passwd)

	       ;; Else, get the password interactively w/o cache.
	       (read-passwd pw-prompt))

	    ;; Workaround.  Prior Emacs 28.1, auth-source has saved
	    ;; empty passwords.  See discussion in Bug#50399.
	    (when (zerop (length auth-passwd))
	      (setq tramp-password-save-function nil))
	    (tramp-set-connection-property v "first-password-request" nil)))

      ;; Reenable the timers.
      (with-timeout-unsuspend stimers))))

(put #'tramp-read-passwd 'tramp-suppress-trace t)

(defun tramp-clear-passwd (vec)
  "Clear password cache for connection related to VEC."
  (let ((method (tramp-file-name-method vec))
	(user-domain (tramp-file-name-user-domain vec))
	(host-port (tramp-file-name-host-port vec))
	(hop (tramp-file-name-hop vec)))
    (when hop
      ;; Clear also the passwords of the hops.
      (tramp-clear-passwd (tramp-dissect-hop-name hop)))
    (auth-source-forget
     `(:max 1 ,(and user-domain :user) ,user-domain
       :host ,host-port :port ,method))
    (password-cache-remove (tramp-make-tramp-file-name vec 'noloc 'nohop))))

(put #'tramp-clear-passwd 'tramp-suppress-trace t)

(defun tramp-time-diff (t1 t2)
  "Return the difference between the two times, in seconds.
T1 and T2 are time values (as returned by `current-time' for example)."
  (float-time (time-subtract t1 t2)))

(defun tramp-unquote-shell-quote-argument (s)
  "Remove quotation prefix \"/:\" from string S, and quote it then for shell.
Suppress `shell-file-name'.  This is needed on w32 systems, which
would use a wrong quoting for local file names.  See `w32-shell-name'."
  (let (shell-file-name)
    (shell-quote-argument (tramp-compat-file-name-unquote s))))

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
  "Interrupt remote PROCESS.
PROCESS can be a process, a buffer with an associated process, the
name of a process or buffer, or nil to default to the current buffer."
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
      (if (not (process-live-p proc))
	  (tramp-error proc 'error "Process %s is not active" proc)
	(tramp-message proc 5 "Interrupt process %s with pid %s" proc pid)
	;; This is for tramp-sh.el.  Other backends do not support this (yet).
	;; Not all "kill" implementations support process groups by
	;; negative pid, so we try both variants.
	(tramp-compat-funcall
	 'tramp-send-command
	 (process-get proc 'vector)
	 (format "(\\kill -2 -%d || \\kill -2 %d) 2>%s"
                 pid pid
                 (tramp-get-remote-null-device (process-get proc 'vector))))
	;; Wait, until the process has disappeared.  If it doesn't,
	;; fall back to the default implementation.
        (while (tramp-accept-process-output proc 0))
	(not (process-live-p proc))))))

;; `interrupt-process-functions' exists since Emacs 26.1.
(when (boundp 'interrupt-process-functions)
  (add-hook 'interrupt-process-functions #'tramp-interrupt-process)
  (add-hook
   'tramp-unload-hook
   (lambda ()
     (remove-hook 'interrupt-process-functions #'tramp-interrupt-process))))

(defun tramp-get-remote-null-device (vec)
  "Return null device on the remote host identified by VEC.
If VEC is nil, return local null device."
  (if (null vec)
      null-device
    (with-tramp-connection-property vec "null-device"
      (let ((default-directory (tramp-make-tramp-file-name vec)))
        (tramp-compat-null-device)))))

(defmacro tramp-skeleton-delete-directory (directory recursive trash &rest body)
  "Skeleton for `tramp-*-handle-delete-directory'.
BODY is the backend specific code."
  (declare (indent 3) (debug t))
  `(with-parsed-tramp-file-name (expand-file-name ,directory) nil
    (if (and delete-by-moving-to-trash ,trash)
	;; Move non-empty dir to trash only if recursive deletion was
	;; requested.
	(if (not (or ,recursive (tramp-compat-directory-empty-p ,directory)))
	    (tramp-error
	     v 'file-error "Directory is not empty, not moving to trash")
	  (move-file-to-trash ,directory))
      ,@body)
    (tramp-flush-directory-properties v localname)))

(put #'tramp-skeleton-delete-directory 'tramp-suppress-trace t)

;; Checklist for `tramp-unload-hook'
;; - Unload all `tramp-*' packages
;; - Reset `file-name-handler-alist'
;; - Cleanup hooks where Tramp functions are in
;; - Cleanup autoloads
;; We must autoload the function body.  Otherwise, Tramp would be
;; loaded unconditionally if somebody calls `tramp-unload-tramp'.
;;;###autoload
(progn (defun tramp-unload-tramp ()
  "Discard Tramp from loading remote files."
  (interactive)
  ;; Maybe it's not loaded yet.
  (ignore-errors (unload-feature 'tramp 'force))))

(provide 'tramp)

(run-hooks 'tramp--startup-hook)
(setq tramp--startup-hook nil)

;;; TODO:
;;
;; * Avoid screen blanking when hitting `g' in dired.  (Eli Tziperman)
;;
;; * Better error checking.  At least whenever we see something
;;   strange when doing zerop, we should kill the process and start
;;   again.  (Greg Stark)
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
