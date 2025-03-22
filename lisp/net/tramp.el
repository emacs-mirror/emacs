;;; tramp.el --- Transparent Remote Access, Multiple Protocol  -*- lexical-binding:t -*-

;; Copyright (C) 1998-2025 Free Software Foundation, Inc.

;; Author: Kai Großjohann <kai.grossjohann@gmx.net>
;;         Michael Albinus <michael.albinus@gmx.de>
;; Maintainer: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes
;; Package: tramp
;; Version: 0
;; Package-Requires: ()
;; Package-Type: multi
;; URL: https://www.gnu.org/software/tramp/

;; This is also a GNU ELPA package.  Avoid functionality that is not
;; compatible with the version of Emacs recorded in trampver.el.
;; Version and Package-Requires are place holders.  They are updated
;; when the GNU ELPA package is released.

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
(require 'tramp-message)
(require 'tramp-integration)
(require 'trampver)

;; Pacify byte-compiler.
(require 'cl-lib)
(declare-function file-notify-rm-watch "filenotify")
(declare-function netrc-parse "netrc")
(defvar auto-save-file-name-transforms)
(defvar tramp-prefix-format)
(defvar tramp-prefix-regexp)
(defvar tramp-method-regexp)
(defvar tramp-postfix-method-format)
(defvar tramp-postfix-method-regexp)
(defvar tramp-prefix-ipv6-format)
(defvar tramp-prefix-ipv6-regexp)
(defvar tramp-postfix-ipv6-format)
(defvar tramp-postfix-ipv6-regexp)
(defvar tramp-postfix-host-format)
(defvar tramp-postfix-host-regexp)
(defvar tramp-host-with-port-regexp)
(defvar tramp-remote-file-name-spec-regexp)
(defvar tramp-file-name-structure)
(defvar tramp-file-name-regexp)
(defvar tramp-completion-method-regexp)
(defvar tramp-completion-file-name-regexp)

;; Reload `tramp-compat' when we reload `tramp-autoloads' of the GNU
;; ELPA package.
;;;###autoload (when (featurep 'tramp-compat)
;;;###autoload   (load "tramp-compat" 'noerror 'nomessage))

;;;###tramp-autoload
(progn
  (defvar tramp--startup-hook nil
    "Forms to be executed at the end of tramp.el.")

  (put 'tramp--startup-hook 'tramp-suppress-trace t)

  (defmacro tramp--with-startup (&rest body)
    "Schedule BODY to be executed at the end of tramp.el."
    `(add-hook 'tramp--startup-hook (lambda () ,@body)))

  (eval-and-compile
    (defalias 'tramp-byte-run--set-suppress-trace
      #'(lambda (f _args val)
	  (list 'function-put (list 'quote f)
		''tramp-suppress-trace val)))

    (add-to-list
     'defun-declarations-alist
     (list 'tramp-suppress-trace #'tramp-byte-run--set-suppress-trace))))

;;; User Customizable Internal Variables:

(defgroup tramp nil
  "Edit remote files with a combination of ssh, scp, etc."
  :group 'files
  :group 'comm
  :version "22.1"
  :link '(info-link :tag "Tramp manual" "(tramp) Top"))

;;;###autoload
(defvar tramp-mode (fboundp 'make-process) ; Disable on MS-DOS.
  "Whether Tramp is enabled.
If it is set to nil, all remote file names are used literally.  Don't
set it manually, use `inhibit-remote-files' or `without-remote-files'
instead.")

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
		       (directory :tag "Backup directory name")))
  :link '(tramp-info-link :tag "Tramp manual" tramp-backup-directory-alist))

(defcustom tramp-auto-save-directory nil
  "Put auto-save files in this directory, if set.
The idea is to use a local directory so that auto-saving is faster.
This setting has precedence over `auto-save-file-name-transforms'."
  :type '(choice (const :tag "Use default" nil)
		 (directory :tag "Auto save directory name"))
  :link '(tramp-info-link :tag "Tramp manual" tramp-auto-save-directory))

;; Suppress `shell-file-name' for w32 systems.
(defcustom tramp-encoding-shell
  (let (shell-file-name)
    (or (tramp-compat-funcall 'w32-shell-name)
        (if (eq system-type 'android)
            ;; The shell is located at /system/bin/sh on Android
            ;; systems.
            "/system/bin/sh"
          "/bin/sh")))
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
  :type '(file :must-match t)
  :link '(info-link :tag "Tramp manual" "(tramp) Remote shell setup"))

;; Suppress `shell-file-name' for w32 systems.
(defcustom tramp-encoding-command-switch
  (let (shell-file-name)
    (if (tramp-compat-funcall 'w32-shell-dos-semantics) "/c" "-c"))
  "Use this switch together with `tramp-encoding-shell' for local commands.
See the variable `tramp-encoding-shell' for more information."
  :type 'string
  :link '(info-link :tag "Tramp manual" "(tramp) Remote shell setup"))

;; Suppress `shell-file-name' for w32 systems.
(defcustom tramp-encoding-command-interactive
  (let (shell-file-name)
    (unless (tramp-compat-funcall 'w32-shell-dos-semantics) "-i"))
  "Use this switch together with `tramp-encoding-shell' for interactive shells.
See the variable `tramp-encoding-shell' for more information."
  :version "24.1"
  :type '(choice (const nil) string)
  :link '(info-link :tag "Tramp manual" "(tramp) Remote shell setup"))

;; Since Emacs 26.1, `system-name' can return nil at build time if
;; Emacs is compiled with "--no-build-details".  We do expect it to be
;; a string.  (Bug#44481, Bug#54294)
;;;###tramp-autoload
(defconst tramp-system-name (or (system-name) "")
  "The system name Tramp is running locally.")

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
    for it.  Also note that \"/bin/sh\" exists on all Unixen
    except Android, this might not be true for the value that you
    decide to use.  You Have Been Warned.

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

    - \"%h\" is replaced by the host name.
    - \"%u\" is replaced by the user name.
    - \"%p\" is replaced by the port number.
    - \"%%\" can be used to obtain a literal percent character.

    If a sub-list containing \"%h\", \"%u\" or \"%p\" is
    unchanged after expansion (i.e. no host, no user or no port
    were specified), that sublist is not used.  For e.g.

    \\='((\"-a\" \"-b\") (\"-l\" \"%u\"))

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
    - \"%y\" is replaced by the `tramp-scp-force-scp-protocol'
      argument if it is supported.
    - \"%z\" is replaced by the `tramp-scp-direct-remote-copying'
      argument if it is supported.
    - \"%d\" is replaced by the device detected by `tramp-adb-get-device'.
    - \"%a\" adds the pseudo-terminal allocation argument \"-t\" in
       asynchronous processes, if the connection type is not `pipe'.

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
    Until now, just \"ssh\"-based, \"sshfs\"-based, \"adb\"-based
    and container methods do.  If it is a list of strings, they
    are used to construct the remote command.

  * `tramp-copy-program'
    This specifies the name of the program to use for remotely copying
    the file; this might be the absolute filename of scp or the name of
    a workalike program.  It is always applied on the local host.

  * `tramp-copy-args'
    This specifies the list of parameters to pass to the above mentioned
    program, the hints for `tramp-login-args' also apply here.

  * `tramp-copy-file-name'
    The remote source or destination file name for out-of-band methods.
    You can use \"%u\" and \"%h\" like in `tramp-login-args'.
    Additionally, \"%f\" denotes the local file name part.  This list
    will be expanded to a string without spaces between the elements of
    the list.

    The default value is `tramp-default-copy-file-name'.

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
    This specifies whether the copying program preserves the timestamp
    of the original file.

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
    some methods, like \"doas\", \"su\" or \"sudo\", a shorter
    timeout might be desirable.

  * `tramp-session-timeout'
    How long a Tramp connection keeps open before being disconnected.
    This is useful for methods like \"doas\" or \"sudo\", which
    shouldn't run an open connection in the background forever.

  * `tramp-password-previous-hop'
    The password for this connection is the same like the
    password for the previous hop.  If there is no previous hop,
    the password of the local user is applied.  This is needed
    for methods like \"doas\", \"sudo\" or \"sudoedit\".

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
   ;; non-Windows system, or pscp from the pssh (parallel ssh) package
   ;; is found.
   ((and (eq system-type 'windows-nt) (executable-find "pscp")) "pscp")
   ;; There is an ssh installation.
   ((executable-find "scp") "scp")
   ;; Fallback.
   (t "ftp"))
  "Default method to use for transferring files.
See `tramp-methods' for possibilities.
Also see `tramp-default-method-alist'."
  :type 'string
  :link '(info-link :tag "Tramp manual" "(tramp) Default Method"))

;;;###tramp-autoload
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
		       (choice :tag "Method name" string (const nil))))
  :link '(info-link :tag "Tramp manual" "(tramp) Default Method"))

(defconst tramp-default-method-marker "-"
  "Marker for default method in remote file names.")

(add-to-list 'tramp-methods `(,tramp-default-method-marker))

(defcustom tramp-default-user nil
  "Default user to use for transferring files.
It is nil by default; otherwise settings in configuration files like
\"~/.ssh/config\" would be overwritten.  Also see `tramp-default-user-alist'."
  :type '(choice (const nil) string)
  :link '(info-link :tag "Tramp manual" "(tramp) Default User"))

;;;###tramp-autoload
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
		       (choice :tag "    User name" string (const nil))))
  :link '(info-link :tag "Tramp manual" "(tramp) Default User"))

(defcustom tramp-default-host tramp-system-name
  "Default host to use for transferring files.
Useful for su and sudo methods mostly."
  :type 'string
  :link '(info-link :tag "Tramp manual" "(tramp) Default Host"))

;;;###tramp-autoload
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
		       (choice :tag "    Host name" string (const nil))))
  :link '(info-link :tag "Tramp manual" "(tramp) Default Host"))

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
		       (choice :tag " Proxy name" string (const nil))))
  :link '(info-link :tag "Tramp manual" "(tramp) Multi-hops"))

(defcustom tramp-save-ad-hoc-proxies nil
  "Whether to save ad-hoc proxies persistently."
  :version "24.3"
  :type 'boolean
  :link '(info-link :tag "Tramp manual" "(tramp) Ad-hoc multi-hops"))

(defcustom tramp-show-ad-hoc-proxies nil
  "Whether to show ad-hoc proxies in file names."
  :version "29.2"
  :type 'boolean
  :link '(info-link :tag "Tramp manual" "(tramp) Ad-hoc multi-hops"))

;; For some obscure technical reasons, `system-name' on w32 returns
;; either lower case or upper case letters.  See
;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=38079#20>.
(defcustom tramp-restricted-shell-hosts-alist
  (when (and (eq system-type 'windows-nt)
             (not (string-match-p (rx "sh" eol) tramp-encoding-shell)))
    (list (rx
	   bos (| (literal (downcase tramp-system-name))
		  (literal (upcase tramp-system-name)))
	   eos)))
  "List of hosts, which run a restricted shell.
This is a list of regular expressions, which denote hosts running
a restricted shell like \"rbash\".  Those hosts can be used as
proxies only, see `tramp-default-proxies-alist'.  If the local
host runs a restricted shell, it shall be added to this list, too."
  :version "27.1"
  :type '(repeat (regexp :tag "Host regexp"))
  :link '(info-link :tag "Tramp manual" "(tramp) Multi-hops"))

;;;###tramp-autoload
(defcustom tramp-local-host-regexp
  (rx bos
      (| (literal tramp-system-name)
	 (| "localhost" "127.0.0.1" "::1"
	    ;; Fedora.
	    "localhost4" "localhost6"
	    ;; Ubuntu.
	    "ip6-localhost" "ip6-loopback"
	    ;; OpenSUSE.
	    "ipv6-localhost" "ipv6-loopback"))
      eos)
  "Host names which are regarded as local host.
If the local host runs a chrooted environment, set this to nil."
  :version "30.1"
  :type '(choice (const :tag "Chrooted environment" nil)
		 (regexp :tag "Host regexp"))
  :link '(tramp-info-link :tag "Tramp manual" tramp-local-host-regexp))

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
  (rx-to-string
   `(: ,tramp-echo-mark-marker
       (= ,tramp-echo-mark-marker-length "\b" (? " \b"))))
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
  (rx (* nonl) (| "user" "login") (? blank (* nonl)) ":" (* blank))
  "Regexp matching login-like prompts.
The regexp should match at end of buffer.

Sometimes the prompt is reported to look like \"login as:\"."
  :type 'regexp)

(defcustom tramp-shell-prompt-pattern
  ;; Allow a prompt to start right after a ^M since it indeed would be
  ;; displayed at the beginning of the line (and Zsh uses it).
  ;; Allow also [] style prompts.  They can appear only during
  ;; connection initialization; Tramp redefines the prompt afterwards.
  (rx (| bol "\r")
      (* (not (any "\n#$%>]")))
      (? "#") (any "#$%>]") (* blank))
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
  :type 'regexp
  :link '(tramp-info-link :tag "Tramp manual" tramp-shell-prompt-pattern))

(defcustom tramp-password-prompt-regexp
  (rx-to-string
   `(: bol (* nonl)
       (group (| . ,password-word-equivalents))
       (* nonl) (any . ,tramp-compat-password-colon-equivalents)
       (? "\^@") (* blank)))
  "Regexp matching password-like prompts.
The regexp should match at end of buffer.

This variable is, by default, initialized from
`password-word-equivalents' when Tramp is loaded, and it is
usually more convenient to add new passphrases to that variable
instead of altering this variable.

The `sudo' program appears to insert a `^@' character into the prompt."
  :version "29.1"
  :type 'regexp
  :link '(tramp-info-link :tag "Tramp manual" tramp-password-prompt-regexp))

(defcustom tramp-otp-password-prompt-regexp
  (rx-to-string
   `(: bol (* nonl)
       (group (|
	 ;; JumpCloud.
	 "Verification code"
	 ;; TACC HPC.  <https://docs.tacc.utexas.edu/basics/mfa/>
	 "TACC Token Code"))
       (* nonl) (any . ,tramp-compat-password-colon-equivalents) (* blank)))
  "Regexp matching one-time password prompts.
The regexp should match at end of buffer."
  :version "30.2"
  :type 'regexp
  :link '(tramp-info-link :tag "Tramp manual" tramp-otp-password-prompt-regexp))

(defcustom tramp-wrong-passwd-regexp
  (rx bol (* nonl)
      (| "Permission denied"
	 "Timeout, server not responding."
	 "Sorry, try again."
	 "Name or service not known"
	 "Host key verification failed."
	 "Authentication failed"
	 "No supported authentication methods left to try!"
	 (: "Login " (| "Incorrect" "incorrect"))
	 (: "Connection " (| "refused" "closed"))
	 (: "Received signal " (+ digit))
	 ;; Fingerprint.
	 "Verification timed out"
	 "Failed to match fingerprint"
	 "An unknown error occurred")
      (* nonl))
  "Regexp matching a `login failed' message.
The regexp should match at end of buffer."
  :type 'regexp
  :link '(tramp-info-link :tag "Tramp manual" tramp-wrong-passwd-regexp))

;; <https://gitlab.freedesktop.org/libfprint/fprintd/-/blob/master/pam/fingerprint-strings.h?ref_type=heads>
(defcustom tramp-fingerprint-prompt-regexp
  (rx (| "Place your finger on"
	 "Swipe your finger across"
	 "Place your left thumb on"
	 "Swipe your left thumb across"
	 "Place your left index finger on"
	 "Swipe your left index finger across"
	 "Place your left middle finger on"
	 "Swipe your left middle finger across"
	 "Place your left ring finger on"
	 "Swipe your left ring finger across"
	 "Place your left little finger on"
	 "Swipe your left little finger across"
	 "Place your right thumb on"
	 "Swipe your right thumb across"
	 "Place your right index finger on"
	 "Swipe your right index finger across"
	 "Place your right middle finger on"
	 "Swipe your right middle finger across"
	 "Place your right ring finger on"
	 "Swipe your right ring finger across"
	 "Place your right little finger on"
	 "Swipe your right little finger across"
	 "Place your finger on the reader again"
	 "Swipe your finger again"
	 "Swipe was too short, try again"
	 "Your finger was not centred, try swiping your finger again"
	 "Remove your finger, and try swiping your finger again")
      (* nonl) (* (any "\r\n")))
  "Regexp matching fingerprint prompts.
The regexp should match at end of buffer."
  :version "30.2"
  :type 'regexp)

(defcustom tramp-yesno-prompt-regexp
  (rx "Are you sure you want to continue connecting (yes/no"
      (? "/[fingerprint]") ")?"
      (* blank))
  "Regular expression matching all yes/no queries which need to be confirmed.
The confirmation should be done with yes or no.
The regexp should match at end of buffer.
See also `tramp-yn-prompt-regexp'."
  :type 'regexp)

(defcustom tramp-yn-prompt-regexp
  (rx (| (: "Store key in cache? (y/n" (* nonl) ")")
	 "Update cached key? (y/n, Return cancels connection)")
      (* blank))
  "Regular expression matching all y/n queries which need to be confirmed.
The confirmation should be done with y or n.
The regexp should match at end of buffer.
See also `tramp-yesno-prompt-regexp'."
  :type 'regexp)

;;;###tramp-autoload
(defcustom tramp-terminal-type "dumb"
  "Value of TERM environment variable for logging in to remote host.
Because Tramp wants to parse the output of the remote shell, it is easily
confused by ANSI control escape sequences and suchlike.  Often, shell init
files conditionalize this setup based on the TERM environment variable."
  :group 'tramp
  :type 'string
  :link '(tramp-info-link :tag "Tramp manual" tramp-terminal-type))

(defcustom tramp-terminal-prompt-regexp
  (rx (| (: "TERM = (" (* nonl) ")")
	 (: "Terminal type? [" (* nonl) "]"))
      (* blank))
  "Regular expression matching all terminal setting prompts.
The regexp should match at end of buffer.
The answer will be provided by `tramp-action-terminal', which see."
  :type 'regexp)

;; Plink 0.71 has added an additional anti-spoofing prompt after
;; authentication.  This could be discarded with the argument
;; "-no-antispoof".  However, since we don't know which PuTTY
;; version is installed, we must react interactively.
(defcustom tramp-antispoof-regexp
  (rx "Access granted. Press Return to begin session. ")
  "Regular expression matching plink's anti-spoofing message.
The regexp should match at end of buffer."
  :version "27.1"
  :type 'regexp)

;; A security key requires the user physically to touch the device
;; with their finger.  We must tell it to the user.
;; Added in OpenSSH 8.2.  I've tested it with Nitrokey, Titankey, and
;; Yubikey.
(defcustom tramp-security-key-confirm-regexp
  (rx bol (* "\r") "Confirm user presence for key " (* nonl) (* (any "\r\n")))
  "Regular expression matching security key confirmation message.
The regexp should match at end of buffer."
  :version "28.1"
  :type 'regexp)

(defcustom tramp-security-key-confirmed-regexp
  (rx bol (* "\r") "User presence confirmed" (* (any "\r\n")))
  "Regular expression matching security key confirmation message.
The regexp should match at end of buffer."
  :version "28.1"
  :type 'regexp)

(defcustom tramp-security-key-timeout-regexp
  (rx bol (* "\r") "sign_and_send_pubkey: signing failed for "
      (* nonl) (* (any "\r\n")))
  "Regular expression matching security key timeout message.
The regexp should match at end of buffer."
  :version "28.1"
  :type 'regexp)

;; Needed only for FIDO2 (residential) keys.  Tested with Nitrokey and Yubikey.
(defcustom tramp-security-key-pin-regexp
  (rx bol (* "\r") (group "Enter PIN for " (* nonl)) (* (any "\r\n")))
  "Regular expression matching security key PIN prompt.
The regexp should match at end of buffer."
  :version "29.3"
  :type 'regexp)

(defcustom tramp-operation-not-permitted-regexp
  (rx (| (: "preserving times" (* nonl)) "set mode") ":" (* blank)
      "Operation not permitted")
  "Regular expression matching keep-date problems in (s)cp operations.
Copying has been performed successfully already, so this message can
be ignored safely."
  :type 'regexp)

(defcustom tramp-copy-failed-regexp
  (rx (+ nonl) ": "
      (| "No such file or directory"
	 "Permission denied"
	 "is a directory"
	 "not a regular file")
      (* blank))
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
If this is a relative file name (such as \"tramp.\"), it is
considered relative to the directory name returned by the
function `temporary-file-directory' (which see).  It may also be
an absolute file name; don't forget to include a prefix for the
filename part, though.")

(defconst tramp-temp-buffer-name " *tramp temp*"
  "Buffer name for a temporary buffer.
It shall be used in combination with `generate-new-buffer-name'.")

(defvar-local tramp-temp-buffer-file-name nil
  "File name of a persistent local temporary file.
Useful for \"rsync\" like methods.")
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
  :set #'tramp-set-syntax
  :link '(info-link :tag "Tramp manual" "(tramp) Change file name syntax"))

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
	tramp-host-with-port-regexp (tramp-build-host-with-port-regexp)
	tramp-remote-file-name-spec-regexp
	(tramp-build-remote-file-name-spec-regexp)
	tramp-file-name-structure (tramp-build-file-name-structure)
	tramp-file-name-regexp (tramp-build-file-name-regexp)
	tramp-completion-method-regexp
        (tramp-build-completion-method-regexp)
	tramp-completion-file-name-regexp
        (tramp-build-completion-file-name-regexp))
  ;; Rearrange file name handlers.
  (tramp-register-file-name-handlers))

;; Initialize the Tramp syntax variables.  We want to override initial
;; value of `tramp-file-name-regexp'.  We do not call
;; `custom-set-variable', this would load Tramp via custom.el.
(tramp--with-startup
  (tramp-set-syntax 'tramp-syntax tramp-syntax))

(defun tramp-syntax-values ()
  "Return possible values of `tramp-syntax', a list."
  (let ((values (cdr (get 'tramp-syntax 'custom-type))))
    (setq values (mapcar #'last values)
	  values (mapcar #'car values))
    values))

(defun tramp-lookup-syntax (alist)
  "Look up a syntax string in ALIST according to `tramp-syntax'.
Raise an error if it is invalid."
  (or (cdr (assq tramp-syntax alist))
      (error "Wrong `tramp-syntax' %s" tramp-syntax)))

(defconst tramp-prefix-format-alist
  '((default    . "/")
    (simplified . "/")
    (separate   . "/["))
  "Alist mapping Tramp syntax to strings beginning Tramp file names.")

(defun tramp-build-prefix-format ()
  "Return `tramp-prefix-format' according to `tramp-syntax'."
  (tramp-lookup-syntax tramp-prefix-format-alist))

(defvar tramp-prefix-format nil ; Initialized when defining `tramp-syntax'!
  "String matching the very beginning of Tramp file names.
Used in `tramp-make-tramp-file-name'.")

(defun tramp-build-prefix-regexp ()
  "Return `tramp-prefix-regexp'."
  (rx bol (literal (tramp-build-prefix-format))))

(defvar tramp-prefix-regexp nil ; Initialized when defining `tramp-syntax'!
  "Regexp matching the very beginning of Tramp file names.
Should always start with \"^\".  Derived from `tramp-prefix-format'.")

(defconst tramp-method-regexp-alist
  `((default . ,(rx (| (literal tramp-default-method-marker) (>= 2 alnum))))
    (simplified . "")
    (separate
     . ,(rx (? (| (literal tramp-default-method-marker) (>= 2 alnum))))))
  "Alist mapping Tramp syntax to regexps matching methods identifiers.")

(defun tramp-build-method-regexp ()
  "Return `tramp-method-regexp' according to `tramp-syntax'."
  (tramp-lookup-syntax tramp-method-regexp-alist))

(defvar tramp-method-regexp nil ; Initialized when defining `tramp-syntax'!
  "Regexp matching method identifiers.
The `ftp' syntax does not support methods.")

(defconst tramp-postfix-method-format-alist
  '((default    . ":")
    (simplified . "")
    (separate   . "/"))
  "Alist mapping Tramp syntax to the delimiter after the method.")

(defun tramp-build-postfix-method-format ()
  "Return `tramp-postfix-method-format' according to `tramp-syntax'."
  (tramp-lookup-syntax tramp-postfix-method-format-alist))

(defvar tramp-postfix-method-format
  nil ; Initialized when defining `tramp-syntax'!
  "String matching delimiter between method and user or host names.
The `ftp' syntax does not support methods.
Used in `tramp-make-tramp-file-name'.")

(defun tramp-build-postfix-method-regexp ()
  "Return `tramp-postfix-method-regexp'."
  (rx (literal (tramp-build-postfix-method-format))))

(defvar tramp-postfix-method-regexp
  nil ; Initialized when defining `tramp-syntax'!
  "Regexp matching delimiter between method and user or host names.
Derived from `tramp-postfix-method-format'.")

(defconst tramp-user-regexp (rx (+ (not (any "/:|" blank))))
  "Regexp matching user names.")

(defconst tramp-prefix-domain-format "%"
  "String matching delimiter between user and domain names.")

(defconst tramp-prefix-domain-regexp (rx (literal tramp-prefix-domain-format))
  "Regexp matching delimiter between user and domain names.
Derived from `tramp-prefix-domain-format'.")

(defconst tramp-domain-regexp (rx (+ (any "._-" alnum)))
  "Regexp matching domain names.")

(defconst tramp-user-with-domain-regexp
  (rx
   (group (regexp tramp-user-regexp))
   (regexp tramp-prefix-domain-regexp)
   (group (regexp tramp-domain-regexp)))
  "Regexp matching user names with domain names.")

(defconst tramp-postfix-user-format "@"
  "String matching delimiter between user and host names.
Used in `tramp-make-tramp-file-name'.")

(defconst tramp-postfix-user-regexp (rx (literal tramp-postfix-user-format))
  "Regexp matching delimiter between user and host names.
Derived from `tramp-postfix-user-format'.")

(defconst tramp-host-regexp (rx (+ (any "%._-" alnum)))
  "Regexp matching host names.")

(defconst tramp-prefix-ipv6-format-alist
  '((default    . "[")
    (simplified . "[")
    (separate   . ""))
  "Alist mapping Tramp syntax to strings prefixing IPv6 addresses.")

(defun tramp-build-prefix-ipv6-format ()
  "Return `tramp-prefix-ipv6-format' according to `tramp-syntax'."
  (tramp-lookup-syntax tramp-prefix-ipv6-format-alist))

(defvar tramp-prefix-ipv6-format nil ; Initialized when defining `tramp-syntax'!
  "String matching left hand side of IPv6 addresses.
Used in `tramp-make-tramp-file-name'.")

(defun tramp-build-prefix-ipv6-regexp ()
  "Return `tramp-prefix-ipv6-regexp'."
  (rx (literal tramp-prefix-ipv6-format)))

(defvar tramp-prefix-ipv6-regexp nil ; Initialized when defining `tramp-syntax'!
  "Regexp matching left hand side of IPv6 addresses.
Derived from `tramp-prefix-ipv6-format'.")

;; The following regexp is a bit sloppy.  But it shall serve our
;; purposes.  It covers also IPv4 mapped IPv6 addresses, like in
;; "::ffff:192.168.0.1".
(defconst tramp-ipv6-regexp (rx (+ (* alnum) ":") (* (any "." alnum)))
  "Regexp matching IPv6 addresses.")

(defconst tramp-postfix-ipv6-format-alist
  '((default    . "]")
    (simplified . "]")
    (separate   . ""))
  "Alist mapping Tramp syntax to suffix for IPv6 addresses.")

(defun tramp-build-postfix-ipv6-format ()
  "Return `tramp-postfix-ipv6-format' according to `tramp-syntax'."
  (tramp-lookup-syntax tramp-postfix-ipv6-format-alist))

(defvar tramp-postfix-ipv6-format nil ; Initialized when defining `tramp-syntax'!
  "String matching right hand side of IPv6 addresses.
Used in `tramp-make-tramp-file-name'.")

(defun tramp-build-postfix-ipv6-regexp ()
  "Return `tramp-postfix-ipv6-regexp'."
  (rx (literal tramp-postfix-ipv6-format)))

(defvar tramp-postfix-ipv6-regexp nil ; Initialized when defining `tramp-syntax'!
  "Regexp matching right hand side of IPv6 addresses.
Derived from `tramp-postfix-ipv6-format'.")

(defconst tramp-prefix-port-format "#"
  "String matching delimiter between host names and port numbers.")

(defconst tramp-prefix-port-regexp (rx (literal tramp-prefix-port-format))
  "Regexp matching delimiter between host names and port numbers.
Derived from `tramp-prefix-port-format'.")

(defconst tramp-port-regexp (rx (+ digit))
  "Regexp matching port numbers.")

(defun tramp-build-host-with-port-regexp ()
  "Return `tramp-host-with-port-regexp'."
  (rx
   (group (| (regexp tramp-host-regexp)
	     (: (regexp tramp-prefix-ipv6-regexp)
		(? (regexp tramp-ipv6-regexp))
		(regexp tramp-postfix-ipv6-regexp))))
   (regexp tramp-prefix-port-regexp)
   (group (regexp tramp-port-regexp))))

(defvar tramp-host-with-port-regexp
  nil ; Initialized when defining `tramp-syntax'!
  "Regexp matching host names with port numbers.")

(defconst tramp-postfix-hop-format "|"
  "String matching delimiter after ad-hoc hop definitions.")

(defconst tramp-postfix-hop-regexp (rx (literal tramp-postfix-hop-format))
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

(defvar tramp-postfix-host-format nil ; Initialized when defining `tramp-syntax'!
  "String matching delimiter between host names and localnames.
Used in `tramp-make-tramp-file-name'.")

(defun tramp-build-postfix-host-regexp ()
  "Return `tramp-postfix-host-regexp'."
  (rx (literal tramp-postfix-host-format)))

(defvar tramp-postfix-host-regexp nil ; Initialized when defining `tramp-syntax'!
  "Regexp matching delimiter between host names and localnames.
Derived from `tramp-postfix-host-format'.")

(defconst tramp-localname-regexp (rx (* (not (any "\r\n"))) eos)
  "Regexp matching localnames.")

(defvar tramp-unknown-id-string "UNKNOWN"
  "String used to denote an unknown user or group.")

(defvar tramp-unknown-id-integer -1
  "Integer used to denote an unknown user or group.")

;;;###tramp-autoload
(defconst tramp-root-id-string "root"
  "String used to denote the root user or group.")

(defconst tramp-root-id-integer 0
  "Integer used to denote the root user or group.")

;;; File name format:

(defun tramp-build-remote-file-name-spec-regexp ()
  "Construct a regexp matching a Tramp file name for a Tramp syntax.
It is expected, that `tramp-syntax' has the proper value."
  (rx
   ;; Method.
   (group (regexp tramp-method-regexp)) (regexp tramp-postfix-method-regexp)
   ;; Optional user.  This includes domain.
   (? (group (regexp tramp-user-regexp)) (regexp tramp-postfix-user-regexp))
   ;; Optional host.
   (? (group (| (regexp tramp-host-regexp)
                (: (regexp tramp-prefix-ipv6-regexp)
		   (? (regexp tramp-ipv6-regexp))
		   (regexp tramp-postfix-ipv6-regexp)))
   ;; Optional port.
   (? (regexp tramp-prefix-port-regexp) (regexp tramp-port-regexp))))))

(defvar tramp-remote-file-name-spec-regexp
  nil ; Initialized when defining `tramp-syntax'!
  "Regular expression matching a Tramp file name between prefix and postfix.")

(defun tramp-build-file-name-structure ()
  "Construct the Tramp file name structure for a Tramp syntax.
It is expected, that `tramp-syntax' has the proper value.
See `tramp-file-name-structure'."
  (list
   (rx
    (regexp tramp-prefix-regexp)
    (? (group (+ (regexp tramp-remote-file-name-spec-regexp)
		 (regexp tramp-postfix-hop-regexp))))
    (regexp tramp-remote-file-name-spec-regexp)
    (regexp tramp-postfix-host-regexp)
    (group (regexp tramp-localname-regexp)))
   5 6 7 8 1))

(defvar tramp-file-name-structure nil ; Initialized when defining `tramp-syntax'!
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
(defconst tramp-initial-file-name-regexp
  ;; We shouldn't use `rx' in autoloaded objects, because we don't
  ;; know whether it does exist already.  (Bug#74490)
  ;; (rx bos "/" (+ (not (any "/:"))) ":" (* (not (any "/:"))) ":")
  "\\`/[^/:]+:[^/:]*:"
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
  :type '(choice (const nil) regexp)
  :link '(tramp-info-link :tag "Tramp manual" tramp-ignored-file-name-regexp))

(defconst tramp-volume-letter-regexp
  (if (eq system-type 'windows-nt)
      (rx bos alpha ":") "")
  "Volume letter on MS Windows.")

;; `tramp-method-regexp' needs at least two characters, in order to
;; distinguish from volume letter.  This is in the way when completing.
(defconst tramp-completion-method-regexp-alist
  `((default . ,(rx (| (literal tramp-default-method-marker) (+ alnum))))
    (simplified . "")
    (separate . ,(rx (| (literal tramp-default-method-marker) (* alnum)))))
  "Alist mapping Tramp syntax to regexps matching completion methods.")

(defun tramp-build-completion-method-regexp ()
  "Return `tramp-completion-method-regexp' according to `tramp-syntax'."
  (tramp-lookup-syntax tramp-completion-method-regexp-alist))

(defvar tramp-completion-method-regexp
  nil ; Initialized when defining `tramp-syntax'!
  "Regexp matching completion method identifiers.
The `ftp' syntax does not support methods.")

(defun tramp-build-completion-file-name-regexp ()
  "Return `tramp-completion-file-name-regexp' according to `tramp-syntax'."
  (if (eq tramp-syntax 'separate)
      ;; FIXME: This shouldn't be necessary.
      (rx bos "/" (? "[" (* (not "]"))) eos)
    (rx
     (regexp tramp-prefix-regexp)

     ;; Optional multi-hops.
     (* (regexp tramp-remote-file-name-spec-regexp)
        (regexp tramp-postfix-hop-regexp))

     ;; Last hop.
     (? (regexp tramp-completion-method-regexp)
	;; Method separator, user name and host name.
	(? (regexp tramp-postfix-method-regexp)
	   (? (regexp tramp-user-regexp)
	      (regexp tramp-postfix-user-regexp))
	   (? (| (regexp tramp-host-regexp) ;; This includes a user.
                 (: (regexp tramp-prefix-ipv6-regexp)
		    (? (regexp tramp-ipv6-regexp)
		       (? (regexp tramp-postfix-ipv6-regexp))))))))
     eos)))

(defvar tramp-completion-file-name-regexp
   nil ; Initialized when defining `tramp-syntax'!
  "Regular expression matching file names handled by Tramp completion.
This regexp should match partial Tramp file names only.

Please note that the entry in `file-name-handler-alist' is made when
this file \(tramp.el) is loaded.  This means that this variable must be set
before loading tramp.el.  Alternatively, `file-name-handler-alist' can be
updated after changing this variable.

Also see `tramp-file-name-structure'.")

;;;###autoload
(defconst tramp-autoload-file-name-regexp
  ;; The method is either "-", or at least two characters.
  ;; We shouldn't use `rx' in autoloaded objects, because we don't
  ;; know whether it does exist already.  (Bug#74490)
  ;; (rx bos "/" (| "-" (>= 2 (not (any "/:|")))) ":")
  "\\`/\\(?:-\\|[^/:|]\\{2,\\}\\):"
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
                             (search-forward-regexp \"\\\\w+\" (point-max) t)))
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
  :type '(choice (const nil) integer)
  :link '(tramp-info-link :tag "Tramp manual" tramp-chunksize))

;; Logging in to a remote host normally requires obtaining a pty.  But
;; Emacs on macOS has `process-connection-type' set to nil by default,
;; so on those systems Tramp doesn't obtain a pty.  Here, we allow
;; for an override of the system default.
(defcustom tramp-process-connection-type t
  "Overrides `process-connection-type' for connections from Tramp.
Tramp binds `process-connection-type' to the value given here before
opening a connection to a remote host."
  :type '(choice (const nil) (const t) (const pipe) (const pty))
  :link '(tramp-info-link :tag "Tramp manual" tramp-process-connection-type))

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
;; NetBSD 9.3: /usr/bin:/bin:/usr/sbin:/sbin:/usr/pkg/bin:/usr/pkg/sbin:/usr/local/bin:/usr/local/sbin
;; IRIX64: /usr/bin
;; QNAP QTS: ---
;; Hydra: /run/current-system/sw/bin:/bin:/usr/bin
;;;###tramp-autoload
(defcustom tramp-remote-path
  '(tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin"
    "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin"
    "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin"
    "/opt/bin" "/opt/sbin" "/opt/local/bin"
    "/opt/homebrew/bin" "/opt/homebrew/sbin")
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
the list by the special value `tramp-own-remote-path'.

For a full discussion, see Info node `(tramp) Remote programs'."
  :group 'tramp
  :type '(repeat (choice
		  (const :tag "Default Directories" tramp-default-remote-path)
		  (const :tag "Private Directories" tramp-own-remote-path)
		  (string :tag "Directory")))
  :link '(info-link :tag "Tramp manual" "(tramp) Remote programs"))

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
  :version "26.1"
  :type '(repeat string)
  :link '(info-link :tag "Tramp manual" "(tramp) Remote processes"))

;;; Internal Variables:

;;;###tramp-autoload
(defvar tramp-current-connection nil
  "Last connection timestamp.
It is a cons cell of the actual `tramp-file-name-structure', and
the (optional) timestamp of last activity on this connection.")

(defvar tramp-password-save-function nil
  "Password save function.
Will be called once the password has been verified by successful
authentication.")

(put 'tramp-password-save-function 'tramp-suppress-trace t)

(defvar tramp-password-prompt-not-unique nil
  "Whether several passwords might be requested.
This shouldn't be set explicitly.  It is let-bound, for example
during direct remote copying with scp.")

(defconst tramp-completion-file-name-handler-alist
  '((expand-file-name . tramp-completion-handle-expand-file-name)
    (file-directory-p . tramp-completion-handle-file-directory-p)
    (file-exists-p . tramp-completion-handle-file-exists-p)
    (file-name-all-completions
     . tramp-completion-handle-file-name-all-completions)
    (file-name-completion . tramp-completion-handle-file-name-completion)
    (file-name-directory . tramp-completion-handle-file-name-directory)
    (file-name-nondirectory . tramp-completion-handle-file-name-nondirectory))
  "Alist of completion handler functions.
Used for file names matching `tramp-completion-file-name-regexp'.
Operations not mentioned here will be handled by Tramp's file
name handler functions, or the normal Emacs functions.")

;; Handlers for foreign methods, like FTP or SMB, shall be plugged here.
;;;###autoload
(defvar tramp-foreign-file-name-handler-alist nil
  "Alist of elements (FUNCTION . HANDLER) for foreign methods handled specially.
If (FUNCTION FILENAME) returns non-nil, then all I/O on that file is done by
calling HANDLER.")

;;; Internal functions which must come first:

;; Conversion functions between external representation and
;; internal data structure.  Convenience functions for internal
;; data structure.

;; The basic structure for remote file names.

;; Note: We started autoloading it in tramp-loaddefs.el, because some
;; functions, which needed it, wouldn't work otherwise when unloading
;; / reloading Tramp (Bug#50869).
;; This bug is fixed in Emacs 29, but other parts of Tramp have grown
;; dependencies on having this in tramp-loaddefs.el in the mean time,
;; so .... here we are.
;;;###tramp-autoload(require 'cl-lib)
;;;###tramp-autoload
(progn
  (cl-defstruct (tramp-file-name (:type list) :named)
    method user domain host port localname hop))

(tramp--with-startup
 (function-put #'tramp-file-name-method 'tramp-suppress-trace t)
 (function-put #'tramp-file-name-user 'tramp-suppress-trace t)
 (function-put #'tramp-file-name-domain 'tramp-suppress-trace t)
 (function-put #'tramp-file-name-host 'tramp-suppress-trace t)
 (function-put #'tramp-file-name-port 'tramp-suppress-trace t)
 (function-put #'tramp-file-name-localname 'tramp-suppress-trace t)
 (function-put #'tramp-file-name-hop 'tramp-suppress-trace t)
 (function-put #'make-tramp-file-name 'tramp-suppress-trace t))

;;;###tramp-autoload
(defconst tramp-null-hop
  (make-tramp-file-name
   :method "local" :user (user-login-name) :host tramp-system-name)
  "Connection hop which identifies the virtual hop before the first one.
Used also for caching properties of the local machine.")

(defun tramp-file-name-user-domain (vec)
  "Return user and domain components of VEC."
  (declare (tramp-suppress-trace t))
  (when (or (tramp-file-name-user vec) (tramp-file-name-domain vec))
    (concat (tramp-file-name-user vec)
	    (and (tramp-file-name-domain vec)
		 tramp-prefix-domain-format)
	    (tramp-file-name-domain vec))))

(defun tramp-file-name-host-port (vec)
  "Return host and port components of VEC."
  (declare (tramp-suppress-trace t))
  (when (or (tramp-file-name-host vec) (tramp-file-name-port vec))
    (concat (tramp-file-name-host vec)
	    (and (tramp-file-name-port vec)
		 tramp-prefix-port-format)
	    (tramp-file-name-port vec))))

(defun tramp-file-name-port-or-default (vec)
  "Return port component of VEC.
If nil, return `tramp-default-port'."
  (declare (tramp-suppress-trace t))
  (or (tramp-file-name-port vec)
      (tramp-get-method-parameter vec 'tramp-default-port)))

;;;###tramp-autoload
(defun tramp-file-name-unify (vec &optional localname)
  "Unify VEC by removing localname and hop from `tramp-file-name' structure.
IF VEC is nil, set it to `tramp-null-hop'.
If LOCALNAME is an absolute file name, set it as localname.
If LOCALNAME is a relative file name, return `tramp-cache-undefined'.
Objects returned by this function compare `equal' if they refer
to the same connection.  Make a copy in order to avoid side
effects."
  ;; (declare (tramp-suppress-trace t))
  (if (and (stringp localname)
	   (not (file-name-absolute-p localname)))
      (setq vec tramp-cache-undefined)
    (unless vec (setq vec tramp-null-hop))
    (when (tramp-file-name-p vec)
      (setq vec (copy-tramp-file-name vec))
      (setf (tramp-file-name-localname vec)
	    (and (stringp localname)
		 (file-name-unquote (directory-file-name localname)))
	    (tramp-file-name-hop vec) nil))
    vec))

;; We cannot use the `declare' form for `tramp-suppress-trace' in
;; autoloaded functions, because the tramp-loaddefs.el generation
;; would fail.
(function-put #'tramp-file-name-unify 'tramp-suppress-trace t)

;; Comparison of file names is performed by `tramp-equal-remote'.
(defun tramp-file-name-equal-p (vec1 vec2)
  "Check, whether VEC1 and VEC2 denote the same `tramp-file-name'.
LOCALNAME and HOP do not count."
  (declare (tramp-suppress-trace t))
  (and (tramp-file-name-p vec1) (tramp-file-name-p vec2)
       (equal (tramp-file-name-unify vec1)
	      (tramp-file-name-unify vec2))))

(defun tramp-get-method-parameter (vec param &optional default)
  "Return the method parameter PARAM.
If VEC is a vector, check first in connection properties.
Afterwards, check in `tramp-methods'.  If the `tramp-methods'
entry does not exist, return DEFAULT."
  (let ((hash-entry
	 (replace-regexp-in-string (rx bos "tramp-") "" (symbol-name param))))
    (if (tramp-connection-property-p vec hash-entry)
	;; We use the cached property.
	(tramp-get-connection-property vec hash-entry)
      ;; Use the static value from `tramp-methods'.
      (if-let* ((methods-entry
		 (assoc
		  param (assoc (tramp-file-name-method vec) tramp-methods))))
	  (cadr methods-entry)
	;; Return the default value.
	default))))

;; The localname can be quoted with "/:".  Extract this.
(defun tramp-file-name-unquote-localname (vec)
  "Return unquoted localname component of VEC."
  (file-name-unquote (tramp-file-name-localname vec)))

;;;###tramp-autoload
(defun tramp-tramp-file-p (name)
  "Return t if NAME is a string with Tramp file name syntax."
  ;; (declare (tramp-suppress-trace t))
  (and tramp-mode (stringp name)
       ;; No "/:" and "/c:".  This is not covered by `tramp-file-name-regexp'.
       (not (string-match-p (rx bos "/" (? alpha) ":") name))
       ;; Excluded file names.
       (or (null tramp-ignored-file-name-regexp)
	   (not (string-match-p tramp-ignored-file-name-regexp name)))
       (string-match-p tramp-file-name-regexp name)
       t))

;; We cannot use the `declare' form for `tramp-suppress-trace' in
;; autoloaded functions, because the tramp-loaddefs.el generation
;; would fail.
(function-put #'tramp-tramp-file-p 'tramp-suppress-trace t)

;; This function bypasses the file name handler approach.  It is NOT
;; recommended to use it in any package if not absolutely necessary.
;; However, it is more performant than `file-local-name', and might be
;; useful where performance matters, like in operations over a bulk
;; list of file names.
;;;###tramp-autoload
(defun tramp-file-local-name (name)
  "Return the local name component of NAME.
This function removes from NAME the specification of the remote host and
the method of accessing the host, leaving only the part that identifies
NAME locally on the remote system.  If NAME does not match
`tramp-file-name-regexp', just `file-local-name' is called.  The
returned file name can be used directly as argument of `make-process',
`process-file', `start-file-process', or `shell-command'."
  (or (and (tramp-tramp-file-p name)
           (string-match (nth 0 tramp-file-name-structure) name)
           (match-string (nth 4 tramp-file-name-structure) name))
      (file-local-name name)))

;; The localname can be quoted with "/:".  Extract this.
(defun tramp-unquote-file-local-name (name)
  "Return unquoted localname of NAME."
  (file-name-unquote (tramp-file-local-name name)))

(defun tramp-find-method (method user host)
  "Return the right method string to use depending on USER and HOST.
This is METHOD, if non-nil.  Otherwise, do a lookup in
`tramp-default-method-alist' and `tramp-default-method'."
  (declare (tramp-suppress-trace t))
  (when (and method
	     (or (string-empty-p method)
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

(defun tramp-find-user (method user host)
  "Return the right user string to use depending on METHOD and HOST.
This is USER, if non-nil.  Otherwise, do a lookup in
`tramp-default-user-alist' and `tramp-default-user'."
  (declare (tramp-suppress-trace t))
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

(defun tramp-find-host (method user host)
  "Return the right host string to use depending on METHOD and USER.
This is HOST, if non-nil.  Otherwise, do a lookup in
`tramp-default-host-alist' and `tramp-default-host'."
  (declare (tramp-suppress-trace t))
  (let ((result
	 (or (and (length> host 0) host)
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
    (if (or (length> host 0) (null result))
	result
      (propertize result 'tramp-default t))))

;;;###tramp-autoload
(defun tramp-dissect-file-name (name &optional nodefault)
  "Return a `tramp-file-name' structure of NAME, a remote file name.
The structure consists of method, user, domain, host, port,
localname (file name on remote host), and hop.

Unless NODEFAULT is non-nil, method, user and host are expanded
to their default values.  For the other file name parts, no
default values are used."
  ;; (declare (tramp-suppress-trace t))
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
		  hop (tramp-make-tramp-hop-name v)))
	  (let ((tramp-default-host
		 (or (and v (not (string-search "%h" (tramp-file-name-host v)))
			  (tramp-file-name-host v))
		     tramp-default-host)))
	    (setq method (tramp-find-method method user host)
		  user (tramp-find-user method user host)
		  host (tramp-find-host method user host))
	    (when hop
	      ;; Replace placeholders.
	      (setq
	       hop (tramp-format-spec hop (format-spec-make ?h host ?u user))))))

	;; Return result.
	(prog1
	    (setq v (make-tramp-file-name
		     :method method :user user :domain domain :host host
		     :port port :localname localname :hop hop))
	  ;; The method must be known.
	  (unless (or nodefault non-essential
		      (assoc method tramp-methods))
	    (tramp-user-error
	     v "Method `%s' is not known" method))
	  ;; Only some methods from tramp-sh.el do support multi-hops.
	  (unless (or (null hop) nodefault non-essential (tramp-multi-hop-p v))
	    (tramp-user-error
	     v "Method `%s' is not supported for multi-hops" method)))))))

;; We cannot use the `declare' form for `tramp-suppress-trace' in
;; autoloaded functions, because the tramp-loaddefs.el generation
;; would fail.
(function-put #'tramp-dissect-file-name 'tramp-suppress-trace t)

;;;###tramp-autoload
(defun tramp-ensure-dissected-file-name (vec-or-filename)
  "Return a `tramp-file-name' structure for VEC-OR-FILENAME.

VEC-OR-FILENAME may be either a string or a `tramp-file-name'.
If it's not a Tramp filename, return nil."
  ;; (declare (tramp-suppress-trace t))
  (cond
   ((tramp-file-name-p vec-or-filename) vec-or-filename)
   ((tramp-tramp-file-p vec-or-filename)
    (tramp-dissect-file-name vec-or-filename))))

;; We cannot use the `declare' form for `tramp-suppress-trace' in
;; autoloaded functions, because the tramp-loaddefs.el generation
;; would fail.
(function-put #'tramp-ensure-dissected-file-name 'tramp-suppress-trace t)

(defun tramp-dissect-hop-name (name &optional nodefault)
  "Return a `tramp-file-name' structure of `hop' part of NAME.
See `tramp-dissect-file-name' for details."
  (declare (tramp-suppress-trace t))
  (let ((v (tramp-dissect-file-name
	    (concat tramp-prefix-format
		    (replace-regexp-in-string
		     (rx (regexp tramp-postfix-hop-regexp) eos)
		     tramp-postfix-host-format name))
	    nodefault)))
    ;; Only some methods from tramp-sh.el do support multi-hops.
    (unless (or nodefault non-essential (tramp-multi-hop-p v))
      (tramp-user-error
       v "Method `%s' is not supported for multi-hops"
       (tramp-file-name-method v)))
    ;; Return result.
    v))

;;;###tramp-autoload
(defsubst tramp-string-empty-or-nil-p (string)
  "Check whether STRING is empty or nil."
  ;; (declare (tramp-suppress-trace t))
  (or (null string) (string= string "")))

;; We cannot use the `declare' form for `tramp-suppress-trace' in
;; autoloaded functions, because the tramp-loaddefs.el generation
;; would fail.
(function-put #'tramp-string-empty-or-nil-p 'tramp-suppress-trace t)

(defun tramp-buffer-name (vec)
  "A name for the connection buffer VEC."
  (declare (tramp-suppress-trace t))
  (let ((method (tramp-file-name-method vec))
	(user-domain (tramp-file-name-user-domain vec))
	(host-port (tramp-file-name-host-port vec)))
    (if (tramp-string-empty-or-nil-p user-domain)
	(format "*tramp/%s %s*" method host-port)
      (format "*tramp/%s %s@%s*" method user-domain host-port))))

;;;###tramp-autoload
(defun tramp-make-tramp-file-name (&rest args)
  "Construct a Tramp file name from ARGS.
If LOCALNAME is nil, the value in VEC is used.  If it is a
symbol, a null localname will be used.  Otherwise, LOCALNAME is
expected to be a string, which will be used."
  (declare (advertised-calling-convention (vec &optional localname) "29.1"))
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
      (when hop
	;; Keep hop in file name for completion or when indicated.
	(unless (or minibuffer-completing-file-name tramp-show-ad-hoc-proxies)
	  (setq hop nil))
	;; Assure that the hops are in `tramp-default-proxies-alist'.
	;; In tramp-archive.el, the slot `hop' is used for the archive
	;; file name.
	(unless (or minibuffer-completing-file-name
		    (string-equal method tramp-archive-method))
	  (tramp-add-hops (car args)))))

     (t (setq method (nth 0 args)
	      user (nth 1 args)
	      domain (nth 2 args)
	      host (nth 3 args)
	      port (nth 4 args)
	      localname (nth 5 args)
	      hop (nth 6 args))))

    ;; Unless `tramp-syntax' is `simplified', we need a method.
    (when (and (not (string-empty-p tramp-postfix-method-format))
	       (tramp-string-empty-or-nil-p method))
      (signal 'wrong-type-argument (list #'stringp method)))
    (concat tramp-prefix-format hop
	    (unless (string-empty-p tramp-postfix-method-format)
	      (concat method tramp-postfix-method-format))
	    user
	    (unless (tramp-string-empty-or-nil-p domain)
	      (concat tramp-prefix-domain-format domain))
	    (unless (tramp-string-empty-or-nil-p user)
	      tramp-postfix-user-format)
	    (when host
	      (if (string-match-p tramp-ipv6-regexp host)
		  (concat
		   tramp-prefix-ipv6-format host tramp-postfix-ipv6-format)
		host))
	    (unless (tramp-string-empty-or-nil-p port)
	      (concat tramp-prefix-port-format port))
	    tramp-postfix-host-format
	    localname)))

(defun tramp-make-tramp-hop-name (vec)
  "Construct a Tramp hop name from VEC."
  (concat
   (tramp-file-name-hop vec)
   (replace-regexp-in-string
    tramp-prefix-regexp ""
    (replace-regexp-in-string
     (rx (regexp tramp-postfix-host-regexp) eos)
     tramp-postfix-hop-format
     (tramp-make-tramp-file-name (tramp-file-name-unify vec))))))

(defun tramp-completion-make-tramp-file-name (method user host localname)
  "Construct a Tramp file name from METHOD, USER, HOST and LOCALNAME.
It must not be a complete Tramp file name, but as long as there are
necessary only.  This function will be used in file name completion."
  (concat tramp-prefix-format
	  (unless (or (tramp-string-empty-or-nil-p method)
                      (string-empty-p tramp-postfix-method-format))
            (concat method tramp-postfix-method-format))
          (unless (tramp-string-empty-or-nil-p user)
	    (concat user tramp-postfix-user-format))
	  (unless (tramp-string-empty-or-nil-p host)
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
	  ;; We use the existence of connection property " connected"
	  ;; as indication, whether a connection is active.  It keeps
	  ;; the connection buffer, for cleanup.
	  (tramp-set-connection-property
	   vec " connected"
	   (tramp-get-connection-property vec " process-buffer"))
	  (setq buffer-undo-list t
		default-directory
		(tramp-make-tramp-file-name vec 'noloc))
	  (current-buffer)))))

;;;###tramp-autoload
(defun tramp-get-connection-buffer (vec &optional dont-create)
  "Get the connection buffer to be used for VEC.
Unless DONT-CREATE, the buffer is created when it doesn't exist yet.
In case a second asynchronous communication has been started, it is different
from `tramp-get-buffer'."
  (or (tramp-get-connection-property vec " process-buffer")
      (tramp-get-buffer vec dont-create)))

(defun tramp-get-connection-name (vec)
  "Get the connection name to be used for VEC.
In case a second asynchronous communication has been started, it is different
from the default one."
  (or (tramp-get-connection-property vec " process-name")
      (tramp-buffer-name vec)))

(defun tramp-get-unique-process-name (name)
  "Return a unique process name, based on NAME."
  (let ((name1 name)
	(i 0))
    (while (get-process name1)
      ;; NAME must be unique as process name.
      (setq i (1+ i)
	    name1 (format "%s<%d>" name i)))
    name1))

(defun tramp-get-process (vec-or-proc)
  "Get the default connection process to be used for VEC-OR-PROC.
Return `tramp-cache-undefined' in case it doesn't exist."
  (or (and (tramp-file-name-p vec-or-proc)
	   (get-buffer-process (tramp-buffer-name vec-or-proc)))
      (and (processp vec-or-proc)
	   (tramp-get-process (process-get vec-or-proc 'tramp-vector)))
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
    (hack-connection-local-variables-apply
     `(:application tramp
       :protocol    ,(tramp-file-name-method vec)
       :user        ,(tramp-file-name-user-domain vec)
       :machine     ,(tramp-file-name-host-port vec)))))

(defun tramp-set-connection-local-variables-for-buffer ()
  "Set connection-local variables in the current buffer.
If connection-local variables are not supported by this Emacs
version, the function does nothing."
  (when (tramp-tramp-file-p default-directory)
    (hack-connection-local-variables-apply
     `(:application tramp
       :protocol    ,(file-remote-p default-directory 'method)
       :user        ,(file-remote-p default-directory 'user)
       :machine     ,(file-remote-p default-directory 'host)))))

(defsubst tramp-get-default-directory (buffer)
  "Return `default-directory' of BUFFER."
  (buffer-local-value 'default-directory buffer))

;;;###tramp-autoload
(defsubst tramp-get-buffer-string (&optional buffer)
  "Return contents of BUFFER.
If BUFFER is not a buffer or a buffer name, return the contents
of `current-buffer'."
  (with-current-buffer
      (if (or (bufferp buffer) (and (stringp buffer) (get-buffer buffer)))
	  buffer (current-buffer))
    (substring-no-properties (buffer-string))))

;; This macro shall optimize the cases where a `file-exists-p' call is
;; invoked first.  Often, the file exists, so the remote command is
;; superfluous.
(defmacro tramp-barf-if-file-missing (vec filename &rest body)
  "Execute BODY and return the result.
In case of an error, raise a `file-missing' error if FILENAME
does not exist, otherwise propagate the error."
  (declare (indent 2) (debug (symbolp form body)))
  (let ((err (make-symbol "err")))
    `(condition-case ,err
         (progn ,@body)
       (error
	(if (not (or (file-exists-p ,filename) (file-symlink-p ,filename)))
	    (tramp-error ,vec 'file-missing ,filename)
	  (signal (car ,err) (cdr ,err)))))))

;; This function provides traces in case of errors not triggered by
;; Tramp functions.
(defun tramp-signal-hook-function (error-symbol data)
  "Function to be called via `signal-hook-function'."
  ;; `custom-initialize-*' functions provoke `void-variable' errors.
  ;; We don't want to see them in the backtrace.
  (declare (tramp-suppress-trace t))
  (unless (eq error-symbol 'void-variable)
    (let ((inhibit-message t))
      (tramp-error
       (car tramp-current-connection) error-symbol
       (mapconcat (lambda (x) (format "%s" x)) data " ")))))

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

(defun tramp-progress-reporter-update (reporter &optional value suffix)
  "Report progress of an operation for Tramp."
  (let* ((parameters (cdr reporter))
	 (message (aref parameters 3)))
    (when (string-search message (or (current-message) ""))
      (progress-reporter-update reporter value suffix))))

;;;###tramp-autoload
(defvar tramp-inhibit-progress-reporter nil
  "Show Tramp progress reporter in the minibuffer.
This variable is used to disable concurrent progress reporter messages.")

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
	    (when-let* ((pr (and (null tramp-inhibit-progress-reporter)
				 (<= ,level (min tramp-verbose 3))
				 (make-progress-reporter ,message))))
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

(defmacro with-tramp-timeout (list &rest body)
  "Like `with-timeout', but allow SECONDS to be nil.

\(fn (SECONDS TIMEOUT-FORMS...) BODY)"
  (declare (indent 1) (debug ((form body) body)))
  (let ((seconds (car list))
	(timeout-forms (cdr list)))
    ;; If non-nil, `seconds' must be a positive number.
    `(if-let* (((natnump ,seconds))
	       ((not (zerop timeout))))
         (with-timeout (,seconds ,@timeout-forms) ,@body)
       ,@body)))

(defvar tramp-dont-suspend-timers nil
  "Don't suspend timers when checking reentrant calls.
This shouldn't be changed globally, but let-bind where needed.")

(defmacro with-tramp-suspended-timers (&rest body)
  "Run BODY with suspended timers.
Obey `tramp-dont-suspend-timers'."
  (declare (indent 0) (debug ((form body) body)))
  `(if tramp-dont-suspend-timers
       (progn ,@body)
     (let ((stimers (with-timeout-suspend))
	   timer-list timer-idle-list)
       (unwind-protect
	   (progn ,@body)
	 (with-timeout-unsuspend stimers)))))

(defun tramp-drop-volume-letter (name)
  "Cut off unnecessary drive letter from file NAME.
The functions `tramp-*-handle-expand-file-name' call `expand-file-name'
locally on a remote file name.  When the local system is a W32 system
but the remote system is Unix, this introduces a superfluous drive
letter into the file name.  This function removes it."
  (save-match-data
    (let ((quoted (file-name-quoted-p name 'top))
	  (result (file-name-unquote name 'top)))
      (setq result
	    (replace-regexp-in-string
	     (rx (regexp tramp-volume-letter-regexp) "/") "/" result))
      (if quoted (file-name-quote result 'top) result))))

(defun tramp-format-spec (format specification)
  "Implement `format-spec' in Tramp.
FORMAT could contain \"%\" which is not intended as format character,
for example in USER%DOMAIN or POD%NAMESPACE."
  (format-spec
   (replace-regexp-in-string (rx "%" (group (= 2 alnum))) "%%\\1" format)
   specification))

;;; Config Manipulation Functions:

(defconst tramp-dns-sd-service-regexp
  (rx bol "_" (+ (any "-" alnum)) "._tcp" eol)
  "DNS-SD service regexp.")

;;;###tramp-autoload
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
		   (stringp (nth 1 (car v)))
		   (cond
		    ;; Windows registry.
		    ((string-prefix-p "HKEY_CURRENT_USER" (nth 1 (car v)))
		     (and (memq system-type '(cygwin windows-nt))
			  (zerop
			   (tramp-call-process
			    nil "reg" nil nil nil "query" (nth 1 (car v))))))
		    ;; DNS-SD service type.
		    ((string-match-p
		      tramp-dns-sd-service-regexp (nth 1 (car v))))
		    ;; Method.
		    ((string-equal method (nth 1 (car v))))
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
If optional FLAG is `nofollow', do not follow FILENAME if it is a
symbolic link.  If the file modes of FILENAME cannot be
determined, return the value of `default-file-modes', without
execute permissions."
  (or (file-modes filename flag)
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
	(push (cons (rx (literal tmpname)) (cdr elt)) result)))))

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
	 (args (if (tramp-file-name-p (car args)) (cons nil (cdr args)) args))
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
	      directory-files directory-files-and-attributes dired-compress-file
	      dired-uncache file-acl file-accessible-directory-p file-attributes
	      file-directory-p file-executable-p file-exists-p file-local-copy
	      file-locked-p file-modes file-name-as-directory
	      file-name-case-insensitive-p file-name-directory
	      file-name-nondirectory file-name-sans-versions
	      file-notify-add-watch file-ownership-preserved-p file-readable-p
	      file-regular-p file-remote-p file-selinux-context file-symlink-p
	      file-system-info file-truename file-writable-p
	      find-backup-file-name get-file-buffer
	      insert-directory insert-file-contents load lock-file make-directory
	      make-lock-file-name set-file-acl set-file-modes
	      set-file-selinux-context set-file-times substitute-in-file-name
	      unhandled-file-name-directory unlock-file vc-registered
	      ;; Emacs 28- only.
	      make-directory-internal
	      ;; Emacs 29+ only.
	      abbreviate-file-name
	      ;; Tramp internal magic file name function.
	      tramp-set-file-uid-gid))
    (if (file-name-absolute-p (nth 0 args))
	(nth 0 args)
      default-directory))
   ;; STRING FILE.
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
	    '(exec-path make-nearby-temp-file make-process process-file
	      shell-command start-file-process temporary-file-directory
	      ;; Emacs 29+ only.
              list-system-processes memory-info process-attributes
              ;; Emacs 30+ only.
	      file-group-gid file-user-uid))
    default-directory)
   ;; PROC.
   ((member operation '(file-notify-rm-watch file-notify-valid-p))
    (when (processp (nth 0 args))
      (tramp-get-default-directory (process-buffer (nth 0 args)))))
   ;; VEC.
   ((member operation
	    '(tramp-get-home-directory tramp-get-remote-gid
	      tramp-get-remote-groups tramp-get-remote-uid))
    (tramp-make-tramp-file-name (nth 0 args)))
   ;; Unknown file primitive.
   (t (error "Unknown file I/O primitive: %s" operation))))

(defun tramp-find-foreign-file-name-handler (vec &optional _operation)
  "Return foreign file name handler if exists."
  (when (tramp-file-name-p vec)
    (let ((handler tramp-foreign-file-name-handler-alist)
	  elt func res)
      (while handler
	(setq elt (car handler)
	      handler (cdr handler))
        ;; Previously, this function was called with FILENAME, but now
        ;; it's called with the VEC.
        (when (condition-case nil
		  (funcall (setq func (car elt)) vec)
		(error
		 (setcar elt #'ignore)
		 (unless (member 'remote-file-error debug-ignored-errors)
		   (tramp-error
		    vec 'remote-file-error
		    "Not a valid Tramp file name function `%s'" func))))
	  (setq handler nil
		res (cdr elt))))
      res)))

;; Main function.
;;;###autoload
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
		   (tramp-find-foreign-file-name-handler v operation))
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
			(with-tramp-debug-message
			    v (format "Running `%S'" (cons operation args))
			  ;; We flush connection properties
			  ;; " process-name" and " process-buffer",
			  ;; because the operations shall be applied
			  ;; in the main connection process.
			  ;; If `non-essential' is non-nil, Tramp shall
		          ;; not open a new connection.
		          ;; If Tramp detects that it shouldn't continue
		          ;; to work, it throws the `suppress' event.
		          ;; This could happen for example, when Tramp
		          ;; tries to open the same connection twice in
		          ;; a short time frame.
		          ;; In both cases, we try the default handler then.
			  (with-tramp-saved-connection-properties
			      v '(" process-name" " process-buffer")
			    (tramp-flush-connection-property v " process-name")
			    (tramp-flush-connection-property v " process-buffer")
		            (setq result
				  (catch 'non-essential
			            (catch 'suppress
				      (apply foreign operation args))))))
		        (cond
		         ((eq result 'non-essential)
			  (tramp-message
			   v 5 "Non-essential received in operation %s"
			   (cons operation args))
			  (let ((tramp-verbose 10)) (tramp-backtrace v))
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
  (if-let*
      ((fn (and tramp-mode minibuffer-completing-file-name
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

(put #'tramp-autoload-file-name-handler 'tramp-autoload t)

;; `tramp-autoload-file-name-handler' must be registered before
;; evaluation of site-start and init files, because there might exist
;; remote files already, f.e. files kept via recentf-mode.
;;;###autoload
(progn (defun tramp-register-autoload-file-name-handlers ()
  "Add Tramp file name handlers to `file-name-handler-alist' during autoload."
  (unless (rassq #'tramp-file-name-handler file-name-handler-alist)
    (add-to-list 'file-name-handler-alist
	         (cons tramp-autoload-file-name-regexp
		       #'tramp-autoload-file-name-handler))
    (put #'tramp-autoload-file-name-handler 'safe-magic t))))

(put #'tramp-register-autoload-file-name-handlers 'tramp-autoload t)
;;;###autoload (tramp-register-autoload-file-name-handlers)

(defun tramp-use-absolute-autoload-file-names ()
  "Change Tramp autoload objects to use absolute file names.
This avoids problems during autoload, when `load-path' contains
remote file names."
  ;; We expect all other Tramp files in the same directory as tramp.el.
  (let* ((dir (expand-file-name (file-name-directory (locate-library "tramp"))))
	 (files (delete-dups
		 (mapcar
		  #'file-name-sans-extension
		  (directory-files
		   dir nil (rx bos "tramp" (+ nonl) ".el" (? "c") eos)))))
	 (files-regexp (rx bol (regexp (regexp-opt files)) eol)))
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

  ;; After unloading, `tramp-archive-enabled' might not be defined.
  (when (bound-and-true-p tramp-archive-enabled)
    (add-to-list 'file-name-handler-alist
	         (cons tramp-archive-file-name-regexp
		       #'tramp-archive-file-name-handler))
    (put #'tramp-archive-file-name-handler 'safe-magic t))

  ;; If jka-compr or epa-file are already loaded, move them to the
  ;; front of `file-name-handler-alist'.
  (dolist (fnh '(epa-file-handler jka-compr-handler))
    (when-let* ((entry (rassoc fnh file-name-handler-alist)))
      (setq file-name-handler-alist
	    (cons entry (delete entry file-name-handler-alist))))))

(tramp--with-startup (tramp-register-file-name-handlers))

;;;###tramp-autoload
(defun tramp-register-foreign-file-name-handler
    (func handler &optional append)
  "Register (FUNC . HANDLER) in `tramp-foreign-file-name-handler-alist'.
FUNC is the function, which takes a dissected filename and determines
whether HANDLER is to be called.  Add operations defined in
`HANDLER-alist' to `tramp-file-name-handler'."
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

(put #'tramp-unload-file-name-handlers 'tramp-autoload t)
(add-hook 'tramp-unload-hook #'tramp-unload-file-name-handlers)

;;;###autoload
(progn (defun inhibit-remote-files ()
  "Deactivate remote file names."
  (interactive)
  (when (fboundp 'tramp-cleanup-all-connections)
    (funcall 'tramp-cleanup-all-connections))
  (tramp-unload-file-name-handlers)
  (setq tramp-mode nil)))

;;;###autoload
(progn (defmacro without-remote-files (&rest body)
  "Deactivate remote file names temporarily.
Run BODY."
  (declare (indent 0) (debug ((form body) body)))
  `(let ((file-name-handler-alist (copy-tree file-name-handler-alist))
         tramp-mode)
     (tramp-unload-file-name-handlers)
     ,@body)))

;;; File name handler functions for completion mode:

;; This function takes action, when `read-extended-command-predicate'
;; is set to `command-completion-default-include-p'.
(defun tramp-command-completion-p (_symbol buffer)
  "A predicate for Tramp interactive commands.
They are completed by `M-x TAB' only if the current buffer is remote."
  (tramp-tramp-file-p (tramp-get-default-directory buffer)))

;; This function takes action, when `read-extended-command-predicate'
;; is set to `command-completion-default-include-p'.
;;;###tramp-autoload
(defun tramp-active-command-completion-p (_symbol _buffer)
  "A predicate for Tramp interactive commands.
They are completed by `M-x TAB' only if there's an active connection or buffer."
  ;; (declare (tramp-suppress-trace t))
  (or (and (hash-table-p tramp-cache-data)
	   (not (zerop (hash-table-count tramp-cache-data))))
      (tramp-list-remote-buffers)))

;; We cannot use the `declare' form for `tramp-suppress-trace' in
;; autoloaded functions, because the tramp-loaddefs.el generation
;; would fail.
(function-put #'tramp-active-command-completion-p 'tramp-suppress-trace t)

(defun tramp-connectable-p (vec-or-filename)
  "Check if it is possible to connect the remote host without side-effects.
This is true, if either the remote host is already connected, or if we are
not in completion mode."
  (let ((tramp-verbose 0)
	(vec (tramp-ensure-dissected-file-name vec-or-filename)))
    (or ;; We check this for the process related to
	;; `tramp-buffer-name'; otherwise `make-process' wouldn't run
	;; ever when `non-essential' is non-nil.
        (process-live-p (tramp-get-process vec))
	(not non-essential))))

(defun tramp-completion-handle-expand-file-name (filename &optional directory)
  "Like `expand-file-name' for partial Tramp files."
  ;; We need special handling only when a method is needed.  Then we
  ;; check, whether DIRECTORY is "/method:" or "/[method/".
  (let ((dir (or directory default-directory "/")))
    (cond
     ((file-name-absolute-p filename) filename)
     ((and (eq tramp-syntax 'simplified)
           (string-match-p (rx (regexp tramp-postfix-host-regexp) eos) dir))
      (concat dir filename))
     ((string-match-p
       (rx (regexp tramp-prefix-regexp)
	   (* (regexp tramp-remote-file-name-spec-regexp)
	      (regexp tramp-postfix-hop-regexp))
	   (? (regexp tramp-method-regexp) (regexp tramp-postfix-method-regexp)
	      (? (regexp tramp-user-regexp) (regexp tramp-postfix-user-regexp)))
	   eos)
       dir)
      (concat dir filename))
     (t (tramp-run-real-handler #'expand-file-name (list filename directory))))))

;; This is needed in pcomplete.el.
(defun tramp-completion-handle-file-directory-p (filename)
  "Like `file-directory-p' for partial Tramp files."
  ;; We need special handling only when a method is needed.  Then we
  ;; regard all files "/method:" or "/[method/" as existent, if
  ;; "method" is a valid Tramp method.
  (or (string-equal filename "/")
      (and ;; Is it a valid method?
           (not (string-empty-p tramp-postfix-method-format))
           (string-match
	    (rx
	     (regexp tramp-prefix-regexp)
	     (* (regexp tramp-remote-file-name-spec-regexp)
		(regexp tramp-postfix-hop-regexp))
	     (group-n 9 (regexp tramp-method-regexp))
	     (? (regexp tramp-postfix-method-regexp))
             eos)
            filename)
	   (assoc (match-string 9 filename) tramp-methods)
	   t)

      (tramp-run-real-handler #'file-directory-p (list filename))))

(defun tramp-completion-handle-file-exists-p (filename)
  "Like `file-exists-p' for partial Tramp files."
  ;; We need special handling only when a method is needed.  Then we
  ;; regard all files "/method:" or "/[method/" as existent, if
  ;; "method" is a valid Tramp method.  And we regard all files
  ;; "/method:user@", "/user@" or "/[method/user@" as existent, if
  ;; "user@" is a valid file name completion.  Host completion is
  ;; performed in the respective backend operation.
  (or (and (cond
            ;; Completion styles like `flex' and `substring' check for
            ;; the file name "/".  This does exist.
            ((string-equal filename "/"))
            ;; Is it a valid method?
            ((and (not (string-empty-p tramp-postfix-method-format))
                  (string-match
	           (rx
	            (regexp tramp-prefix-regexp)
		    (* (regexp tramp-remote-file-name-spec-regexp)
		       (regexp tramp-postfix-hop-regexp))
	            (group-n 9 (regexp tramp-method-regexp))
	            (? (regexp tramp-postfix-method-regexp))
                    eos)
                   filename))
             (assoc (match-string 9 filename) tramp-methods))
            ;; Is it a valid user?
            ((string-match
	      (rx
               (regexp tramp-prefix-regexp)
	       (* (regexp tramp-remote-file-name-spec-regexp)
		  (regexp tramp-postfix-hop-regexp))
               (group-n 10
		 (regexp tramp-method-regexp)
		 (regexp tramp-postfix-method-regexp))
	       (group-n 11
		 (regexp tramp-user-regexp)
		 (regexp tramp-postfix-user-regexp))
               eos)
              filename)
	     (member
	      (match-string 11 filename)
	      (file-name-all-completions
	       "" (concat tramp-prefix-format (match-string 10 filename))))))
	   t)

      (tramp-run-real-handler #'file-exists-p (list filename))))

(defmacro tramp-skeleton-file-name-all-completions
    (filename directory &rest body)
  "Skeleton for `tramp-*-handle-filename-all-completions'.
BODY is the backend specific code."
  (declare (indent 2) (debug t))
  `(ignore-error file-missing
     (delete-dups (delq nil
       (let* ((case-fold-search read-file-name-completion-ignore-case)
	      (result (progn ,@body)))
	 ;; Some storage systems do not return "." and "..".
	 (when (tramp-tramp-file-p ,directory)
	   (dolist (elt '(".." "."))
	     (when (string-prefix-p ,filename elt)
	       (setq result (cons (concat elt "/") result)))))
	 (if (consp completion-regexp-list)
	     ;; Discriminate over `completion-regexp-list'.
	     (mapcar
	      (lambda (x)
		(when (stringp x)
		  (catch 'match
		    (dolist (elt completion-regexp-list x)
		      (unless (string-match-p elt x) (throw 'match nil))))))
	      result)
	   result))))))

(defvar tramp--last-hop-directory nil
  "Tracks the directory from which to run login programs.")

;; Method, host name and user name completion.
;; `tramp-completion-dissect-file-name' returns a list of
;; `tramp-file-name' structures.  For all of them we return possible
;; completions.
(defun tramp-completion-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for partial Tramp files."
  (tramp-skeleton-file-name-all-completions filename directory
    (let ((fullname
	   (tramp-drop-volume-letter (expand-file-name filename directory)))
	  (directory (tramp-drop-volume-letter directory))
	  tramp--last-hop-directory hop result result1)

      ;; Suppress hop from completion.
      (when (string-match
	     (rx
	      (regexp tramp-prefix-regexp)
	      (group (+ (regexp tramp-remote-file-name-spec-regexp)
			(regexp tramp-postfix-hop-regexp))))
	     fullname)
	(setq hop (match-string 1 fullname)
	      fullname (replace-match "" nil nil fullname 1)
	      tramp--last-hop-directory
	      (tramp-make-tramp-file-name (tramp-dissect-hop-name hop))))

      (let (;; When `tramp-syntax' is `simplified', we need a default method.
	    (tramp-default-method
	     (and (string-empty-p tramp-postfix-method-format)
		  tramp-default-method))
	    (tramp-default-method-alist
	     (and (string-empty-p tramp-postfix-method-format)
		  tramp-default-method-alist))
	    tramp-default-user tramp-default-user-alist
	    tramp-default-host tramp-default-host-alist)

	;; Possible completion structures.
	(dolist (elt (tramp-completion-dissect-file-name fullname))
	  (let* ((method (tramp-file-name-method elt))
		 (user (tramp-file-name-user elt))
		 (host (tramp-file-name-host elt))
		 (localname (tramp-file-name-localname elt))
		 (m (tramp-find-method method user host))
		 all-user-hosts)

	    (unless localname ;; Nothing to complete.
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
				   all-user-hosts))))

		;; Possible methods.
		(setq result
		      (append result (tramp-get-completion-methods m hop)))))))

	;; Add hop.
	(dolist (elt result)
          (when elt
	    (setq elt (replace-regexp-in-string
		       tramp-prefix-regexp (concat tramp-prefix-format hop) elt))
	    (push (substring elt (length directory)) result1)))

	;; Complete local parts.
	(append
         result1
         (ignore-errors
           (tramp-run-real-handler
	    #'file-name-all-completions (list filename directory))))))))

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

;; "/x:y@" "/[x/y@"     "/x:y@z" "/[x/y@z"   "/x:y@z:" "/[x/y@z]"
;; ["x" "y" nil nil]    ["x" "y" "z" nil]    ["x" "y" "z" ""]
(defun tramp-completion-dissect-file-name (name)
  "Return a list of `tramp-file-name' structures for NAME.
They are collected by `tramp-completion-dissect-file-name1'."
  (let (;; "/method" "/[method"
	(tramp-completion-file-name-structure1
	 (list
	  (rx
	   (regexp tramp-prefix-regexp)
	   (group (? (regexp tramp-completion-method-regexp))) eol)
	  1 nil nil nil))
	;; "/method:user" "/[method/user"
	(tramp-completion-file-name-structure2
	 (list
	  (rx
	   (regexp tramp-prefix-regexp)
	   (group (regexp tramp-method-regexp))
	   (regexp tramp-postfix-method-regexp)
	   (group (? (regexp tramp-user-regexp))) eol)
	  1 2 nil nil))
	;; "/method:host" "/[method/host"
	(tramp-completion-file-name-structure3
	 (list
	  (rx
	   (regexp tramp-prefix-regexp)
	   (group (regexp tramp-method-regexp))
	   (regexp tramp-postfix-method-regexp)
	   (group (? (regexp tramp-host-regexp))) eol)
	  1 nil 2 nil))
	;; "/method:[ipv6" "/[method/ipv6"
	(tramp-completion-file-name-structure4
	 (list
	  (rx
	   (regexp tramp-prefix-regexp)
	   (group (regexp tramp-method-regexp))
	   (regexp tramp-postfix-method-regexp)
	   (regexp tramp-prefix-ipv6-regexp)
	   (group (? (regexp tramp-ipv6-regexp))) eol)
	  1 nil 2 nil))
	;; "/method:user@host" "/[method/user@host"
	(tramp-completion-file-name-structure5
	 (list
	  (rx
	   (regexp tramp-prefix-regexp)
	   (group (regexp tramp-method-regexp))
	   (regexp tramp-postfix-method-regexp)
	   (group (regexp tramp-user-regexp))
	   (regexp tramp-postfix-user-regexp)
	   (group (? (regexp tramp-host-regexp))) eol)
	  1 2 3 nil))
	;; "/method:user@[ipv6" "/[method/user@ipv6"
	(tramp-completion-file-name-structure6
	 (list
	  (rx
	   (regexp tramp-prefix-regexp)
	   (group (regexp tramp-method-regexp))
	   (regexp tramp-postfix-method-regexp)
	   (group (regexp tramp-user-regexp))
	   (regexp tramp-postfix-user-regexp)
	   (regexp tramp-prefix-ipv6-regexp)
	   (group (? (regexp tramp-ipv6-regexp))) eol)
	  1 2 3 nil)))
    (tramp-compat-seq-keep
     (lambda (structure) (tramp-completion-dissect-file-name1 structure name))
     (list
      tramp-completion-file-name-structure1
      tramp-completion-file-name-structure2
      tramp-completion-file-name-structure3
      tramp-completion-file-name-structure4
      tramp-completion-file-name-structure5
      tramp-completion-file-name-structure6))))

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
(defun tramp-get-completion-methods (partial-method &optional multi-hop)
  "Return all method completions for PARTIAL-METHOD.
If MULTI-HOP is non-nil, return only multi-hop capable methods."
  (mapcar
   (lambda (method)
     (and method (string-prefix-p (or partial-method "") method)
	  (or (not multi-hop)
	      (tramp-multi-hop-p (make-tramp-file-name :method method)))
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

(defun tramp-completion-handle-file-name-directory (filename)
  "Like `file-name-directory' for partial Tramp files."
  ;; We need special handling only when a method is needed.  Then we
  ;; return "/method:" or "/[method/", if "method" is a valid Tramp
  ;; method.  In the `separate' file name syntax, we return "/[" when
  ;; `filename' is "/[string" w/o a trailing method separator "/".
  (cond
   ((string-match
     (rx (group (regexp tramp-prefix-regexp)
		(* (regexp tramp-remote-file-name-spec-regexp)
		   (regexp tramp-postfix-hop-regexp)))
         (? (regexp tramp-completion-method-regexp)) eos)
     filename)
    (match-string 1 filename))
   ((and (string-match
          (rx (group
	       (regexp tramp-prefix-regexp)
	       (* (regexp tramp-remote-file-name-spec-regexp)
		  (regexp tramp-postfix-hop-regexp))
               (group (regexp tramp-method-regexp))
	       (regexp tramp-postfix-method-regexp)
	       (? (regexp tramp-user-regexp)
	          (regexp tramp-postfix-user-regexp)))
	      (? (| (regexp tramp-host-regexp)
                    (: (regexp tramp-prefix-ipv6-regexp)
		       (? (regexp tramp-ipv6-regexp)
			  (? (regexp tramp-postfix-ipv6-regexp))))))
	      eos)
          filename)
         ;; Is it a valid method?
	 (or (tramp-string-empty-or-nil-p (match-string 2 filename))
             (assoc (match-string 2 filename) tramp-methods)))
    (match-string 1 filename))
   (t (tramp-run-real-handler #'file-name-directory (list filename)))))

(defun tramp-completion-handle-file-name-nondirectory (filename)
  "Like `file-name-nondirectory' for partial Tramp files."
  (string-replace (file-name-directory filename) "" filename))

(defun tramp-parse-default-user-host (method)
  "Return a list of (user host) tuples allowed to access for METHOD.
This function is added always in `tramp-get-completion-function'
for all methods.  Resulting data are derived from default settings."
  `((,(tramp-find-user method nil nil) ,(tramp-find-host method nil nil))))

;;;###tramp-autoload
(defcustom tramp-completion-multi-hop-methods nil
  "Methods for which to provide completions over multi-hop connections."
  :version "30.1"
  :type '(repeat (string :tag "Method name"))
  :link '(info-link :tag "Tramp manual" "(tramp) Ad-hoc multi-hops"))

(defcustom tramp-completion-use-auth-sources auth-source-do-cache
  "Whether to use `auth-source-search' for completion of user and host names.
This could be disturbing, if it requires a password / passphrase,
as for \"~/.authinfo.gpg\"."
  :version "27.1"
  :type 'boolean
  :link '(info-link :tag "Tramp manual" "(tramp) File name completion"))

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
     (when (search-forward-regexp regexp (line-end-position) t)
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
	  (rx
	   bol (group (regexp tramp-host-regexp))
	   (? (+ blank) (group (regexp tramp-user-regexp))))))
     (when (search-forward-regexp regexp (line-end-position) t)
       (setq result (append (list (match-string 2) (match-string 1)))))
     (forward-line 1)
     result))

(defun tramp-parse-shosts (filename)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  (tramp-parse-file filename #'tramp-parse-shosts-group))

(defun tramp-parse-shosts-group ()
   "Return a (user host) tuple allowed to access.
User is always nil."
   (tramp-parse-group (rx bol (group (regexp tramp-host-regexp))) 1 ","))

(defun tramp-parse-sconfig (filename)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  (tramp-parse-file filename #'tramp-parse-sconfig-group))

(defun tramp-parse-sconfig-group ()
   "Return a (user host) tuple allowed to access.
User is always nil."
   (tramp-parse-group
    (rx
     (| (: bol (* blank) "Host")
	(: bol (+ nonl)) ;; ???
	(group (regexp tramp-host-regexp))))
    1 (rx blank)))

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
     when (and (not (string-match-p (rx bol (** 1 2 ".") eol) f))
	       (string-match regexp f))
     collect (list nil (match-string 1 f)))))

(defun tramp-parse-shostkeys (dirname)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  (tramp-parse-shostkeys-sknownhosts
   dirname
   (rx bol "key_" (+ digit) "_" (group (regexp tramp-host-regexp)) ".pub" eol)))

(defun tramp-parse-sknownhosts (dirname)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  (tramp-parse-shostkeys-sknownhosts
   dirname
   (rx
    bol (group (regexp tramp-host-regexp)) ".ssh-" (| "dss" "rsa") ".pub" eol)))

(defun tramp-parse-hosts (filename)
  "Return a list of (user host) tuples allowed to access.
User is always nil."
  (tramp-parse-file filename #'tramp-parse-hosts-group))

(defun tramp-parse-hosts-group ()
   "Return a (user host) tuple allowed to access.
User is always nil."
   (tramp-parse-group
    (rx bol (group (| (regexp tramp-ipv6-regexp) (regexp tramp-host-regexp))))
    1 (rx blank)))

(defun tramp-parse-passwd (filename)
  "Return a list of (user host) tuples allowed to access.
Host is always \"localhost\"."
  (with-tramp-connection-property nil "parse-passwd"
    (if (executable-find "getent")
	(with-temp-buffer
	  (when (zerop (tramp-call-process nil "getent" nil t nil "passwd"))
	    (goto-char (point-min))
	    (cl-loop while (not (eobp)) collect
		     (tramp-parse-passwd-group))))
      (tramp-parse-file filename #'tramp-parse-passwd-group))))

(defun tramp-parse-passwd-group ()
   "Return a (user host) tuple allowed to access.
Host is always \"localhost\"."
   (let (result
	 (regexp (rx bol (group (regexp tramp-user-regexp)) ":")))
     (when (search-forward-regexp regexp (line-end-position) t)
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
	 (split
	  (split-string (buffer-substring (point) (line-end-position)) ":")))
     (when (member (user-login-name) (split-string (nth 3 split) "," 'omit))
       (setq result (list (nth 0 split) "localhost")))
     (forward-line 1)
     result))

(defun tramp-parse-netrc (filename)
  "Return a list of (user host) tuples allowed to access.
User may be nil."
  (mapcar
   (lambda (item)
     (and (assoc "machine" item)
	  `(,(cdr (assoc "login" item)) ,(cdr (assoc "machine" item)))))
   (tramp-compat-auth-source-netrc-parse-all filename)))

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
     registry-or-dirname (rx bol (group (regexp tramp-host-regexp)) eol))))

(defun tramp-parse-putty-group (registry)
  "Return a (user host) tuple allowed to access.
User is always nil."
  (let (result
	(regexp (rx (literal registry) "\\" (group (+ nonl)))))
    (when (search-forward-regexp regexp (line-end-position) t)
      (setq result (list nil (match-string 1))))
    (forward-line 1)
    result))

;;; Skeleton macros for file name handler functions.

(defmacro tramp-skeleton-copy-directory
  (directory _newname &optional _keep-date _parents _copy-contents &rest body)
  "Skeleton for `tramp-*-handle-copy-directory'.
BODY is the backend specific code."
  (declare (indent 5) (debug t))
  ;; `copy-directory' creates NEWNAME before running this check.  So
  ;; we do it ourselves.  Therefore, we cannot also run
  ;; `tramp-barf-if-file-missing'.
  `(progn
     (unless (file-exists-p ,directory)
       (tramp-error
	(tramp-dissect-file-name ,directory) 'file-missing ,directory))
     ,@body))

(defmacro tramp-skeleton-delete-directory (directory recursive trash &rest body)
  "Skeleton for `tramp-*-handle-delete-directory'.
BODY is the backend specific code."
  (declare (indent 3) (debug t))
  `(with-parsed-tramp-file-name (expand-file-name ,directory) nil
     (let ((delete-by-moving-to-trash
	    (and delete-by-moving-to-trash
		 ;; This variable exists since Emacs 30.1.
		 (not (bound-and-true-p
		       remote-file-name-inhibit-delete-by-moving-to-trash)))))
       (if (and delete-by-moving-to-trash ,trash)
	   ;; Move non-empty dir to trash only if recursive deletion was
	   ;; requested.
	   (if (not (or ,recursive (directory-empty-p ,directory)))
	       (tramp-error
		v 'file-error "Directory is not empty, not moving to trash")
	     (move-file-to-trash ,directory))
	 ,@body)
       (tramp-flush-directory-properties v localname))))

(defmacro tramp-skeleton-delete-file (filename &optional trash &rest body)
  "Skeleton for `tramp-*-handle-delete-file'.
BODY is the backend specific code."
  (declare (indent 2) (debug t))
  `(with-parsed-tramp-file-name (expand-file-name ,filename) nil
     (let ((delete-by-moving-to-trash
	    (and delete-by-moving-to-trash
		 ;; This variable exists since Emacs 30.1.
		 (not (bound-and-true-p
		       remote-file-name-inhibit-delete-by-moving-to-trash)))))
       (if (and delete-by-moving-to-trash ,trash)
	   (move-file-to-trash ,filename)
	 ,@body)
       (tramp-flush-file-properties v localname))))

(defmacro tramp-skeleton-directory-files
    (directory &optional full match nosort count &rest body)
  "Skeleton for `tramp-*-handle-directory-files'.
BODY is the backend specific code."
  (declare (indent 5) (debug t))
  `(or
    (with-parsed-tramp-file-name (expand-file-name ,directory) nil
      (tramp-barf-if-file-missing v ,directory
	(when (file-directory-p ,directory)
	  (setf ,directory
		(file-name-as-directory (expand-file-name ,directory)))
	  (let ((temp
		 (with-tramp-file-property v localname "directory-files" ,@body))
		result item)
	    (while temp
	      (setq item (directory-file-name (pop temp)))
	      (when (or (null ,match) (string-match-p ,match item))
		(push (if ,full (concat ,directory item) item)
		      result)))
	    (unless ,nosort
              (setq result (sort result #'string<)))
	    (when (and (natnump ,count) (> ,count 0))
	      (setq result (tramp-compat-ntake ,count result)))
	    result))))

    ;; Error handling.
    (if (not (file-exists-p ,directory))
	(tramp-error
	 (tramp-dissect-file-name ,directory) 'file-missing ,directory)
      nil)))

(defmacro tramp-skeleton-directory-files-and-attributes
    (directory &optional full match nosort id-format count &rest body)
  "Skeleton for `tramp-*-handle-directory-files-and-attributes'.
BODY is the backend specific code."
  (declare (indent 6) (debug t))
  `(or
    (with-parsed-tramp-file-name (expand-file-name ,directory) nil
      (tramp-barf-if-file-missing v ,directory
	(when (file-directory-p ,directory)
	  (let ((temp
		 (copy-tree
		  (mapcar
		   (lambda (x)
		     (cons
		      (car x)
		      (tramp-convert-file-attributes
			  v (expand-file-name (car x) localname)
			  ,id-format (cdr x))))
		   (with-tramp-file-property
		       v localname "directory-files-and-attributes"
		     ,@body))))
		result item)

	    (while temp
	      (setq item (pop temp))
	      (when (or (null ,match) (string-match-p ,match (car item)))
		(when ,full
		  (setcar item (expand-file-name (car item) ,directory)))
		(push item result)))

	    (unless ,nosort
	      (setq result
		    (sort result (lambda (x y) (string< (car x) (car y))))))

	    (when (and (natnump ,count) (> ,count 0))
	      (setq result (tramp-compat-ntake ,count result)))

	    (or result
		;; The scripts could fail, for example with huge file size.
		(tramp-handle-directory-files-and-attributes
		 ,directory ,full ,match ,nosort ,id-format ,count))))))

    ;; Error handling.
    (if (not (file-exists-p ,directory))
	(tramp-error
	 (tramp-dissect-file-name ,directory) 'file-missing ,directory)
      nil)))

(defcustom tramp-use-file-attributes t
  "Whether to use \"file-attributes\" file property for check.
This is relevant for read, write, and execute permissions.  On some file
systems using NFS4_ACL, the permission string as returned from `stat' or
`ls', is not sufficient to provide more fine-grained information.
This variable is intended as connection-local variable."
  :version "30.1"
  :type 'boolean
  :link '(tramp-info-link :tag "Tramp manual" tramp-use-file-attributes))

(defsubst tramp-use-file-attributes (vec)
  "Whether to use \"file-attributes\" file property for check."
  (and ;; We assume, that connection-local variables are set in this buffer.
       (with-current-buffer (tramp-get-connection-buffer vec)
	 tramp-use-file-attributes)
       (tramp-file-property-p
	vec (tramp-file-name-localname vec) "file-attributes")))

(defmacro tramp-skeleton-file-exists-p (filename &rest body)
  "Skeleton for `tramp-*-handle-file-exists-p'.
BODY is the backend specific code."
  (declare (indent 1) (debug t))
  ;; `file-exists-p' is used as predicate in file name completion.
  `(or (and minibuffer-completing-file-name
	    (file-name-absolute-p ,filename)
	    (tramp-string-empty-or-nil-p
	     (tramp-file-name-localname (tramp-dissect-file-name ,filename))))
       ;; We don't want to run it when `non-essential' is t, or there
       ;; is no connection process yet.
       (when (tramp-connectable-p ,filename)
	 (with-parsed-tramp-file-name (expand-file-name ,filename) nil
	   (with-tramp-file-property v localname "file-exists-p"
	     (cond
	      ;; Examine `file-attributes' cache to see if request can
	      ;; be satisfied without remote operation.
	      ((and-let*
		   (((tramp-file-property-p v localname "file-attributes"))
		    (fa (tramp-get-file-property v localname "file-attributes"))
		    ((not (stringp (car fa)))))))
	      ;; Symlink to a non-existing target counts as nil.
	      ;; Protect against cyclic symbolic links.
	      ((file-symlink-p ,filename)
	       (ignore-errors
		 (file-exists-p (file-truename ,filename))))
	      (t ,@body)))))))

(defmacro tramp-skeleton-file-local-copy (filename &rest body)
  "Skeleton for `tramp-*-handle-file-local-copy'.
BODY is the backend specific code."
  (declare (indent 1) (debug t))
  `(with-parsed-tramp-file-name (file-truename ,filename) nil
     (tramp-barf-if-file-missing v ,filename
       (or
	(let ((tmpfile (tramp-compat-make-temp-file ,filename)))
	  ,@body
          (run-hooks 'tramp-handle-file-local-copy-hook)
	  tmpfile)

	;; Trigger the `file-missing' error.
	(signal 'error nil)))))

(defmacro tramp-skeleton-file-truename (filename &rest body)
  "Skeleton for `tramp-*-handle-file-truename'.
BODY is the backend specific code."
  (declare (indent 1) (debug (form body)))
  ;; Preserve trailing "/".
  `(funcall
    (if (directory-name-p ,filename) #'file-name-as-directory #'identity)
    ;; Quote properly.
    (funcall
     (if (file-name-quoted-p ,filename) #'file-name-quote #'identity)
     (with-parsed-tramp-file-name
	 (file-name-unquote (expand-file-name ,filename)) nil
       (tramp-make-tramp-file-name
	v
	(with-tramp-file-property v localname "file-truename"
	  (let (result)
	    (setq result (progn ,@body))
	    ;; Detect cycle.
	    (when (and (file-symlink-p ,filename)
		       (string-equal result localname))
	      (tramp-error
	       v 'file-error
	       "Apparent cycle of symbolic links for %s" ,filename))
	    ;; If the resulting localname looks remote, we must quote
	    ;; it for security reasons.
	    (when (tramp-tramp-file-p result)
	      (setq result (file-name-quote result 'top)))
	    result)))))))

(defmacro tramp-skeleton-make-directory (dir &optional parents &rest body)
  "Skeleton for `tramp-*-handle-make-directory'.
BODY is the backend specific code."
  ;; Since Emacs 29.1, PARENTS isn't propagated to the handlers
  ;; anymore.  And the return values are specified since then as well.
  (declare (indent 2) (debug t))
  `(let* ((dir (directory-file-name (expand-file-name ,dir)))
	  (par (file-name-directory dir)))
     (with-parsed-tramp-file-name dir nil
       (when (and (null ,parents) (file-exists-p dir))
	 (tramp-error v 'file-already-exists dir))
       ;; Make missing directory parts.
       (when ,parents
	 (unless (file-directory-p par)
	   (make-directory par ,parents)))
       ;; Just do it.
       (if (file-exists-p dir) t
	 (tramp-flush-file-properties v localname)
	 ,@body
	 nil))))

(defmacro tramp-skeleton-make-process (args null-command stderr-file &rest body)
  "Skeleton for `tramp-*-handle-make-process'.
NULL-COMMAND indicates a possible empty command.  STDERR-FILE means,
that a stederr file is supported.  BODY is the backend specific code."
  (declare (indent 3) (debug t))
  `(when ,args
     (with-parsed-tramp-file-name (expand-file-name default-directory) nil
       (let ((name (plist-get ,args :name))
	     (buffer (plist-get ,args :buffer))
	     (command (plist-get ,args :command))
	     (coding (plist-get ,args :coding))
	     (noquery (plist-get ,args :noquery))
	     (connection-type
	      (or (plist-get ,args :connection-type) process-connection-type))
	     (filter (plist-get ,args :filter))
	     (sentinel (plist-get ,args :sentinel))
	     (stderr (plist-get ,args :stderr)))
	 (unless (stringp name)
	   (signal 'wrong-type-argument (list #'stringp name)))
	 (unless (or (bufferp buffer) (string-or-null-p buffer))
	   (signal 'wrong-type-argument (list #'bufferp buffer)))
	 (unless (or (consp command) (and ,null-command (null command)))
	   (signal 'wrong-type-argument (list #'consp command)))
	 (unless (or (null coding)
		     (and (symbolp coding) (memq coding coding-system-list))
		     (and (consp coding)
			  (memq (car coding) coding-system-list)
			  (memq (cdr coding) coding-system-list)))
	   (signal 'wrong-type-argument (list #'symbolp coding)))
	 (when (eq connection-type t)
	   (setq connection-type 'pty))
	 (unless (or (and (consp connection-type)
			  (memq (car connection-type) '(nil pipe pty))
			  (memq (cdr connection-type) '(nil pipe pty)))
		     (memq connection-type '(nil pipe pty)))
	   (signal 'wrong-type-argument (list #'symbolp connection-type)))
	 (unless (or (null filter) (eq filter t) (functionp filter))
	   (signal 'wrong-type-argument (list #'functionp filter)))
	 (unless (or (null sentinel) (functionp sentinel))
	   (signal 'wrong-type-argument (list #'functionp sentinel)))
	 (unless (or (null stderr) (bufferp stderr)
		     (and ,stderr-file (stringp stderr)))
	   (signal 'wrong-type-argument (list #'bufferp stderr)))
	 (when (and (stringp stderr)
		    (not (tramp-equal-remote default-directory stderr)))
	   (signal 'file-error (list "Wrong stderr" stderr)))

	 (let ((name (tramp-get-unique-process-name name))
	       (buffer
		(if buffer
		    (get-buffer-create buffer)
		  ;; BUFFER can be nil.  We use a temporary buffer.
		  (generate-new-buffer tramp-temp-buffer-name)))
	       (orig-command command))

	   ,@body)))))

(defmacro tramp-skeleton-make-symbolic-link
  (target linkname &optional ok-if-already-exists &rest body)
  "Skeleton for `tramp-*-handle-make-symbolic-link'.
BODY is the backend specific code.
If TARGET is a non-Tramp file, it is used verbatim as the target
of the symlink.  If TARGET is a Tramp file, only the localname
component is used as the target of the symlink if it is located
on the same host.  Otherwise, TARGET is quoted."
  (declare (indent 3) (debug t))
  `(with-parsed-tramp-file-name  (expand-file-name ,linkname) nil
     ;; If TARGET is a Tramp name, use just the localname component.
     ;; Don't check for a proper method.
     (let ((non-essential t))
       (when (and (tramp-tramp-file-p ,target)
		  (tramp-file-name-equal-p v (tramp-dissect-file-name ,target)))
	 (setf ,target (tramp-file-local-name (expand-file-name ,target))))
       ;; There could be a cyclic link.
       (tramp-flush-file-properties
	v (tramp-drop-volume-letter
	   (expand-file-name
	    ,target (tramp-file-local-name default-directory)))))

     ;; If TARGET is still remote, quote it.
     (if (tramp-tramp-file-p ,target)
	 (make-symbolic-link
	  (file-name-quote ,target 'top) ,linkname ,ok-if-already-exists)

       ;; Do the 'confirm if exists' thing.
       (when (file-exists-p ,linkname)
	 ;; What to do?
	 (if (or (null ,ok-if-already-exists) ; not allowed to exist
		 (and (numberp ,ok-if-already-exists)
		      (not (yes-or-no-p
			    (format
			     "File %s already exists; make it a link anyway?"
			     localname)))))
	     (tramp-error v 'file-already-exists localname)
	   (delete-file ,linkname)))

       ;; We must also flush the cache of the directory, because
       ;; `file-attributes' reads the values from there.
       (tramp-flush-file-properties v localname)

       ,@body)))

(defmacro tramp-skeleton-process-file
    (_program &optional infile destination _display _args &rest body)
  "Skeleton for `tramp-*-handle-process-file'.
BODY is the backend specific code."
  (declare (indent 5) (debug t))
  `(with-parsed-tramp-file-name (expand-file-name default-directory) nil
     ;; The implementation is not complete yet.
     (when (and (numberp ,destination) (zerop ,destination))
       (tramp-error
	v 'file-error "Implementation does not handle immediate return"))

     (let (command input tmpinput stderr tmpstderr outbuf ret)
       ;; Determine input.
       (if (null ,infile)
	   (setq input (tramp-get-remote-null-device v))
	 (setq ,infile (file-name-unquote (expand-file-name ,infile)))
	 (if (tramp-equal-remote default-directory ,infile)
	     ;; INFILE is on the same remote host.
	     (setq input (tramp-unquote-file-local-name ,infile))
	   ;; ,INFILE must be copied to remote host.
	   (setq input (tramp-make-tramp-temp-file v)
		 tmpinput (tramp-make-tramp-file-name v input))
	   (copy-file ,infile tmpinput t)))

       ;; Determine output.
       (cond
	;; Just a buffer.
	((bufferp ,destination)
	 (setq outbuf ,destination))
	;; A buffer name.
	((stringp ,destination)
	 (setq outbuf (get-buffer-create ,destination)))
	;; (REAL-,DESTINATION ERROR-,DESTINATION)
	((consp ,destination)
	 ;; output.
	 (cond
	  ((bufferp (car ,destination))
	   (setq outbuf (car ,destination)))
	  ((stringp (car ,destination))
	   (setq outbuf (get-buffer-create (car ,destination))))
	  ((car ,destination)
	   (setq outbuf (current-buffer))))
	 ;; stderr.
	 (cond
	  ((stringp (cadr ,destination))
	   (setcar (cdr ,destination) (expand-file-name (cadr ,destination)))
	   (if (tramp-equal-remote default-directory (cadr ,destination))
	       ;; stderr is on the same remote host.
	       (setq stderr (tramp-unquote-file-local-name (cadr ,destination)))
	     ;; stderr must be copied to remote host.  The temporary
	     ;; file must be deleted after execution.
	     (setq stderr (tramp-make-tramp-temp-file v)
		   tmpstderr (tramp-make-tramp-file-name v stderr))))
	  ;; stderr to be discarded.
	  ((null (cadr ,destination))
	   (setq stderr (tramp-get-remote-null-device v)))
	  ((eq (cadr ,destination) tramp-cache-undefined)
	   ;; stderr is not impelmemted.
	   (tramp-warning v "%s" "STDERR not supported"))))
	;; t
	(,destination
	 (setq outbuf (current-buffer))))

       ,@body

       ;; Provide error file.
       (when tmpstderr (rename-file tmpstderr (cadr ,destination) t))

       ;; Cleanup.  We remove all file cache values for the connection,
       ;; because the remote process could have changed them.
       (when tmpinput (delete-file tmpinput))
       (when process-file-side-effects
         (tramp-flush-directory-properties v "/"))

       ;; Return exit status.
       (if (equal ret -1)
	   (keyboard-quit)
	 ret))))

(defcustom tramp-inhibit-errors-if-setting-file-attributes-fail nil
  "Whether to warn only if `tramp-*-set-file-{modes,times,uid-gid}' fails."
  :version "30.1"
  :type 'boolean
  :link '(tramp-info-link :tag "Tramp manual"
			  tramp-inhibit-errors-if-setting-file-attributes-fail))

(defmacro tramp-skeleton-set-file-modes-times-uid-gid
    (filename &rest body)
  "Skeleton for `tramp-*-set-file-{modes,times,uid-gid}'.
BODY is the backend specific code."
  (declare (indent 1) (debug t))
  `(with-parsed-tramp-file-name (expand-file-name ,filename) nil
     (when (not (file-exists-p ,filename))
       (tramp-error v 'file-missing ,filename))
     (with-tramp-saved-file-properties
	 v localname
	 ;; We cannot add "file-attributes", "file-executable-p",
	 ;; "file-ownership-preserved-p", "file-readable-p",
	 ;; "file-writable-p".
	 '("file-directory-p" "file-exists-p" "file-symlink-p" "file-truename")
       (tramp-flush-file-properties v localname))
     (condition-case err
	 (progn ,@body)
       (error (if tramp-inhibit-errors-if-setting-file-attributes-fail
		  (display-warning 'tramp (error-message-string err))
		(signal (car err) (cdr err)))))))

(defmacro tramp-skeleton-write-region
  (start end filename append visit lockname mustbenew &rest body)
  "Skeleton for `tramp-*-handle-write-region'.
BODY is the backend specific code."
  (declare (indent 7) (debug t))
  ;; Sometimes, there is another file name handler responsible for
  ;; VISIT, for example `jka-compr-handler'.  We must respect this.
  ;; See Bug#55166.
  `(let* ((filename (expand-file-name ,filename))
	  (lockname (file-truename (or ,lockname filename)))
	  (handler (and (stringp ,visit)
			(let ((inhibit-file-name-handlers
			       `(tramp-file-name-handler
				 tramp-crypt-file-name-handler
				 . ,inhibit-file-name-handlers))
			      (inhibit-file-name-operation 'write-region))
			  (find-file-name-handler ,visit 'write-region))))
	  ;; We use this to save the value of
	  ;; `last-coding-system-used' after writing the tmp file.  At
	  ;; the end of the function, we set `last-coding-system-used'
	  ;; to this saved value.  This way, any intermediary coding
	  ;; systems used while talking to the remote shell or
	  ;; suchlike won't hose this variable.  This approach was
	  ;; snarfed from ange-ftp.el.
	  coding-system-used)
     (with-parsed-tramp-file-name filename nil
       (if handler
	   (progn
	     (tramp-message
	      v 5 "Calling handler `%s' for visiting `%s'" handler ,visit)
	     (funcall
	      handler 'write-region
	      ,start ,end filename ,append ,visit lockname ,mustbenew))

	 (when (and ,mustbenew (file-exists-p filename)
		    (or (eq ,mustbenew 'excl)
			(not
			 (y-or-n-p
			  (format
			   "File %s exists; overwrite anyway?" filename)))))
	   (tramp-error v 'file-already-exists filename))

	 (let ((file-locked (eq (file-locked-p lockname) t))
	       (uid (or (file-attribute-user-id
			 (file-attributes filename 'integer))
			(tramp-get-remote-uid v 'integer)))
	       (gid (or (file-attribute-group-id
			 (file-attributes filename 'integer))
			(tramp-get-remote-gid v 'integer)))
	       (attributes (file-extended-attributes filename))
	       (curbuf (current-buffer)))

	   ;; Lock file.
	   (when (and (not (auto-save-file-name-p
			    (file-name-nondirectory filename)))
		      (tramp-tramp-file-p lockname)
		      (not file-locked))
	     (setq file-locked t)
	     (lock-file lockname))

	   ;; The body.
	   ,@body

	   ;; We must also flush the cache of the directory, because
	   ;; `file-attributes' reads the values from there.
	   (tramp-flush-file-properties v localname)
	   ;; Set the "file-exists-p" file property, because it is
	   ;; likely that it is needed shortly after `write-region'.
	   (tramp-set-file-property v localname "file-exists-p" t)

	   (let (last-coding-system-used (need-chown t))
	     ;; Set file modification time.
	     (when (or (eq ,visit t) (stringp ,visit))
	       (when-let* ((file-attr (file-attributes filename 'integer)))
		 (set-visited-file-modtime
		  ;; We must pass modtime explicitly, because FILENAME
		  ;; can be different from (buffer-file-name), f.e. if
		  ;; `file-precious-flag' is set.
		  (or (file-attribute-modification-time file-attr)
		      (current-time)))
		 (when (and (= (file-attribute-user-id file-attr) uid)
			    (= (file-attribute-group-id file-attr) gid))
		   (setq need-chown nil))))

	     ;; Set the ownership.
             (when need-chown
               (tramp-set-file-uid-gid filename uid gid))

	     ;; Set extended attributes.  We ignore possible errors,
	     ;; because ACL strings or SELinux contexts could be incompatible.
	     (when attributes
	       (ignore-errors
		 (set-file-extended-attributes filename attributes)))

	     ;; Unlock file.
	     (when file-locked
	       (unlock-file lockname))

	     ;; Sanity check.
	     (unless (equal curbuf (current-buffer))
	       (tramp-error
		v 'file-error
		"Buffer has changed from `%s' to `%s'" curbuf (current-buffer)))

	     (when (and (null noninteractive)
			(or (eq ,visit t) (string-or-null-p ,visit)))
	       (tramp-message v 0 "Wrote %s" filename))
	     (run-hooks 'tramp-handle-write-region-hook))))

       ;; Make `last-coding-system-used' have the right value.
       (when coding-system-used
	 (setq last-coding-system-used coding-system-used)))))

;;; Common file name handler functions for different backends:

(defvar tramp-handle-file-local-copy-hook nil
  "Normal hook to be run at the end of `tramp-*-handle-file-local-copy'.")

(defvar tramp-handle-write-region-hook nil
  "Normal hook to be run at the end of `tramp-*-handle-write-region'.")

(defvar tramp-tolerate-tilde nil
  "Indicator, that not expandable tilde shall be tolerated.
Let-bind it when necessary.")

;; `directory-abbrev-apply' and `directory-abbrev-make-regexp' exists
;; since Emacs 29.1.  Since this handler isn't called for older
;; Emacs, it is save to invoke them via `tramp-compat-funcall'.
(defun tramp-handle-abbreviate-file-name (filename)
  "Like `abbreviate-file-name' for Tramp files."
  (let* ((case-fold-search (file-name-case-insensitive-p filename))
	 (vec (tramp-dissect-file-name filename))
	 (tramp-tolerate-tilde t)
         (home-dir
          (if (let ((non-essential t)) (tramp-connectable-p vec))
              ;; If a connection has already been established, get the
              ;; home directory.
	      (tramp-get-home-directory vec)
            ;; Otherwise, just use the cached value.
            (tramp-get-connection-property vec "~"))))
    (when home-dir
      (setq home-dir
	    (tramp-compat-funcall
	     'directory-abbrev-apply
	     (tramp-make-tramp-file-name vec home-dir))))
    ;; If any elt of `directory-abbrev-alist' matches this name,
    ;; abbreviate accordingly.
    (setq filename (tramp-compat-funcall 'directory-abbrev-apply filename))
    ;; Abbreviate home directory.
    (if (and home-dir
             (string-match
	      (tramp-compat-funcall 'directory-abbrev-make-regexp home-dir)
              filename))
        (tramp-make-tramp-file-name
	 vec (concat "~" (substring filename (match-beginning 1))))
      (tramp-make-tramp-file-name (tramp-dissect-file-name filename)))))

(defun tramp-handle-file-user-uid ()
  "Like `file-user-uid' for Tramp files."
  (let ((v (tramp-dissect-file-name default-directory)))
    (or (tramp-get-remote-uid v 'integer)
        ;; Some handlers for `tramp-get-remote-uid' return nil if they
        ;; can't get the UID; always return -1 in this case for
        ;; consistency.
        tramp-unknown-id-integer)))

(defun tramp-handle-file-group-gid ()
  "Like `file-group-gid' for Tramp files."
  (let ((v (tramp-dissect-file-name default-directory)))
    (or (tramp-get-remote-gid v 'integer)
        ;; Some handlers for `tramp-get-remote-gid' return nil if they
        ;; can't get the GID; always return -1 in this case for
        ;; consistency.
        tramp-unknown-id-integer)))

(defun tramp-handle-access-file (filename string)
  "Like `access-file' for Tramp files."
  (let ((timeout
	 ;; This variable exists since Emacs 30.1.
	 (bound-and-true-p remote-file-name-access-timeout))
	(v (tramp-dissect-file-name
	    (if (file-name-absolute-p filename) filename default-directory)))
	;; We rely on timers, so don't suspend them.
	(tramp-dont-suspend-timers t))
    (with-tramp-timeout
	(timeout
	 (unless (and-let* ((p (tramp-get-connection-process v))
			    ((process-live-p p))
			    ((tramp-get-connection-property p "connected"))))
	   (tramp-cleanup-connection v 'keep-debug 'keep-password))
	 (tramp-error
	  v 'file-error
	  (format
	   "%s: Timeout %s second(s) accessing %s" string timeout filename)))
      (setq filename (file-truename filename))
      (if (file-exists-p filename)
	  (unless
	      (funcall
	       (if (file-directory-p filename)
		   #'file-accessible-directory-p #'file-readable-p)
	       filename)
	    (tramp-compat-permission-denied
	     v (format "%s: Permission denied, %s" string filename)))
	(tramp-error
	 v 'file-missing
	 (format "%s: No such file or directory, %s" string filename))))))

(defun tramp-handle-add-name-to-file
  (filename newname &optional ok-if-already-exists)
  "Like `add-name-to-file' for Tramp files."
  (with-parsed-tramp-file-name
      (expand-file-name (if (tramp-tramp-file-p newname) newname filename)) nil
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
  (tramp-skeleton-copy-directory
      directory newname keep-date parents copy-contents
   ;; We must do it file-wise.
   (tramp-run-real-handler
    #'copy-directory
    (list directory newname keep-date parents copy-contents))))

(defun tramp-handle-directory-file-name (directory)
  "Like `directory-file-name' for Tramp files."
  ;; If localname component of filename is "/", leave it unchanged.
  ;; Otherwise, remove any trailing slash from localname component.
  ;; Method, host, etc, are unchanged.
  (while (with-parsed-tramp-file-name directory nil
	   (and (length> localname 0)
		(eq (aref localname (1- (length localname))) ?/)
		(not (string= localname "/"))))
    (setq directory (substring directory 0 -1)))
  directory)

(defun tramp-handle-directory-files (directory &optional full match nosort count)
  "Like `directory-files' for Tramp files."
  (tramp-skeleton-directory-files directory full match nosort count
    (nreverse (file-name-all-completions "" directory))))

(defun tramp-handle-directory-files-and-attributes
  (directory &optional full match nosort id-format count)
  "Like `directory-files-and-attributes' for Tramp files."
  (mapcar
   (lambda (x)
     (cons x (file-attributes
	      (if full x (expand-file-name x directory)) id-format)))
   (directory-files directory full match nosort count)))

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
  (when (string-empty-p name)
    (setq name "."))
  ;; Unless NAME is absolute, concat DIR and NAME.
  (unless (file-name-absolute-p name)
    (setq name (file-name-concat dir name)))
  ;; If NAME is not a Tramp file, run the real handler.
  (if (not (tramp-tramp-file-p name))
      (tramp-run-real-handler #'expand-file-name (list name))
    ;; Dissect NAME.
    (with-parsed-tramp-file-name name nil
      (unless (tramp-run-real-handler #'file-name-absolute-p (list localname))
	(setq localname (concat "/" localname)))
      ;; Tilde expansion shall be possible also for quoted localname.
      (when (string-prefix-p "~" (file-name-unquote localname))
	(setq localname (file-name-unquote localname)))
      ;; Expand tilde.  Usually, the methods applying this handler do
      ;; not support tilde expansion.  But users could declare a
      ;; respective connection property.  (Bug#53847)
      (when (string-match
	     (rx bos "~" (group (* (not "/"))) (group (* nonl)) eos) localname)
	(let ((uname (match-string 1 localname))
	      (fname (match-string 2 localname))
	      hname)
	  (when (tramp-string-empty-or-nil-p uname)
	    (setq uname user))
	  (when (setq hname (tramp-get-home-directory v uname))
	    (setq localname (concat hname fname)))))
      ;; Tilde expansion is not possible.
      (when (and (not tramp-tolerate-tilde)
		 (string-prefix-p "~" localname))
	(tramp-error v 'file-error "Cannot expand tilde in file `%s'" name))
      ;; Do not keep "/..".
      (when (string-match-p (rx bos "/" (** 1 2 ".") eos) localname)
	(setq localname "/"))
      ;; Do normal `expand-file-name' (this does "/./" and "/../").
      ;; `default-directory' is bound, because on Windows there would
      ;; be problems with UNC shares or Cygwin mounts.
      (let ((default-directory tramp-compat-temporary-file-directory))
	(tramp-make-tramp-file-name
	 v (tramp-drop-volume-letter
	    (if (string-prefix-p "~" localname)
		localname
	      (tramp-run-real-handler #'expand-file-name (list localname)))))))))

(defun tramp-handle-file-accessible-directory-p (filename)
  "Like `file-accessible-directory-p' for Tramp files."
  (and (file-directory-p filename)
       (file-readable-p filename)))

(defun tramp-handle-file-directory-p (filename)
  "Like `file-directory-p' for Tramp files."
  ;; `file-truename' could raise an error, for example due to a cyclic
  ;; symlink.
  (ignore-errors
    (eq (file-attribute-type (file-attributes (file-truename filename))) t)))

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
  (tramp-skeleton-file-exists-p filename
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
  (tramp-skeleton-file-local-copy filename
    (copy-file filename tmpfile 'ok-if-already-exists 'keep-time)))

(defun tramp-handle-file-modes (filename &optional flag)
  "Like `file-modes' for Tramp files."
  (when-let* ((attrs (file-attributes filename))
	      (mode-string (file-attribute-modes attrs)))
    (if (and (not (eq flag 'nofollow)) (eq ?l (aref mode-string 0)))
	(file-modes (file-truename filename))
      (tramp-mode-string-to-int mode-string))))

;; Localname manipulation functions that grok Tramp localnames...
(defun tramp-handle-file-name-as-directory (file)
  "Like `file-name-as-directory' for Tramp files."
  ;; `file-name-as-directory' would be sufficient except localname is
  ;; the empty string.  Suppress adding a hop to
  ;; `tramp-default-proxies-alist' due to non-expanded default values.
  (let ((v (tramp-dissect-file-name file t))
	(tramp-default-proxies-alist tramp-cache-undefined))
    ;; Run the command on the localname portion only unless we are in
    ;; completion mode.
    (tramp-make-tramp-file-name
     v (or (and (tramp-string-empty-or-nil-p (tramp-file-name-localname v))
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
     ;; Note: We cannot use it as DEFAULT value of
     ;; `tramp-get-method-parameter', because it would be evalled
     ;; during the call.
     (and (let ((non-essential t)) (tramp-connectable-p v))
          (with-tramp-connection-property v "case-insensitive"
	    (ignore-errors
	      (with-tramp-progress-reporter v 5 "Checking case-insensitive"
		;; The idea is to compare a file with lower case
		;; letters with the same file with upper case letters.
		(let ((candidate (directory-file-name filename))
		      case-fold-search
		      tmpfile)
		  ;; Check, whether we find an existing file with
		  ;; lower case letters.  This avoids us to create a
		  ;; temporary file.
		  (while (and (string-match-p
			       (rx lower) (tramp-file-local-name candidate))
			      (not (file-exists-p candidate)))
		    (setq candidate
			  (directory-file-name
			   (file-name-directory candidate))))
		  ;; Nothing found, so we must use a temporary file
		  ;; for comparison.
		  (unless (string-match-p
			   (rx lower) (tramp-file-local-name candidate))
		    (setq tmpfile
			  (let ((default-directory
				 (file-name-directory filename)))
			    (make-nearby-temp-file "tramp."))
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
    (when (and (consp fnac) (length= (delete "./" (delete "../" fnac)) 1))
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
	     (rx (regexp (regexp-opt completion-ignored-extensions)) eos) x)
	    ;; We remember the hit.
	    (push x hits-ignored-extensions))))))
     ;; No match.  So we try again for ignored files.
     (try-completion filename hits-ignored-extensions))))

(defun tramp-handle-file-name-directory (file)
  "Like `file-name-directory' for Tramp files."
  ;; Everything except the last filename thing is the directory.  We
  ;; cannot apply `with-parsed-tramp-file-name', because this expands
  ;; the remote file name parts.  Suppress adding a hop to
  ;; `tramp-default-proxies-alist' due to non-expanded default values.
  (let ((v (tramp-dissect-file-name file t))
	(tramp-default-proxies-alist tramp-cache-undefined))
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
   ;; Tramp reads and writes timestamps on second level.  So we round
   ;; the timestamps to seconds without fractions.
   (t (time-less-p
       (time-convert
	(file-attribute-modification-time (file-attributes file2)) 'integer)
       (time-convert
	(file-attribute-modification-time (file-attributes file1)) 'integer)))))

(defun tramp-handle-file-readable-p (filename)
  "Like `file-readable-p' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-readable-p"
      (or (tramp-check-cached-permissions v ?r)
	  ;; `tramp-check-cached-permissions' doesn't handle symbolic
	  ;; links.
	  (and-let* ((symlink (file-symlink-p filename))
		     ((stringp symlink))
		     ((file-readable-p
		       (concat (file-remote-p filename) symlink)))))))))

(defun tramp-handle-file-regular-p (filename)
  "Like `file-regular-p' for Tramp files."
  (and (file-exists-p filename)
       ;; Sometimes, `file-attributes' does not return a proper value
       ;; even if `file-exists-p' does.  Protect by `ignore-errors',
       ;; because `file-truename' could raise an error for cyclic
       ;; symlinks.
       (ignore-errors
	 (when-let* ((attr (file-attributes filename)))
	   (cond
	    ((eq ?- (aref (file-attribute-modes attr) 0)))
	    ((eq ?l (aref (file-attribute-modes attr) 0))
	     (file-regular-p (file-truename filename))))))))

(defun tramp-handle-file-remote-p (filename &optional identification connected)
  "Like `file-remote-p' for Tramp files.
It supports the additional IDENTIFICATION `hop'.
For the `host' IDENTIFICATION, both host name and port number (if
existing) are returned."
  ;; We do not want traces in the debug buffer.
  (let ((tramp-verbose (min tramp-verbose 3)))
    (when (tramp-tramp-file-p filename)
      (let* ((o (tramp-dissect-file-name filename))
	     (p (and (not (eq connected 'never))
                     (tramp-get-connection-process o)))
	     (c (and (process-live-p p)
		     (tramp-get-connection-property p "connected"))))
	;; We expand the file name only, if there is already a connection.
	(with-parsed-tramp-file-name
	    (if c (expand-file-name filename) filename) nil
	  (and (or (memq connected '(nil never)) c)
	       (cond
		((eq identification 'method) method)
		;; Domain and port are appended to user and host,
		;; respectively.
		((eq identification 'user) (tramp-file-name-user-domain v))
		((eq identification 'host) (tramp-file-name-host-port v))
		((eq identification 'localname) localname)
		;; Hop exists only in original dissected file name.
		((eq identification 'hop) (tramp-file-name-hop o))
		(t (tramp-make-tramp-file-name v 'noloc)))))))))

(defun tramp-handle-file-selinux-context (_filename)
  "Like `file-selinux-context' for Tramp files."
  ;; Return nil context.
  '(nil nil nil nil))

(defun tramp-handle-file-symlink-p (filename)
  "Like `file-symlink-p' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    ;; Some operations, like `file-truename', set the file property
    ;; "file-symlink-marker".  We can use it as indicator, and avoid a
    ;; possible call of `file-attributes'.
    (when (or (tramp-get-file-property v localname "file-symlink-marker")
	      (not (tramp-file-property-p v localname "file-symlink-marker")))
      (let ((x (file-attribute-type (file-attributes filename))))
	(and (stringp x) x)))))

(defun tramp-handle-file-truename (filename)
  "Like `file-truename' for Tramp files."
  (tramp-skeleton-file-truename filename
    (let ((result (directory-file-name localname))
	  (numchase 0)
	  ;; Don't make the following value larger than necessary.
	  ;; People expect an error message in a timely fashion when
	  ;; something is wrong; otherwise they might think that Emacs
	  ;; is hung.  Of course, correctness has to come first.
	  (numchase-limit 20)
	  ;; Unquoting could enable encryption.
	  tramp-crypt-enabled
	  symlink-target)
      (while (and (setq symlink-target
			(file-symlink-p (tramp-make-tramp-file-name v result)))
		  (< numchase numchase-limit))
	(setq numchase (1+ numchase)
	      result
	      (if (tramp-tramp-file-p symlink-target)
		  (file-name-quote symlink-target 'top)
		(tramp-drop-volume-letter
		 (expand-file-name
		  symlink-target (file-name-directory result)))))
	(when (>= numchase numchase-limit)
	  (tramp-error
	   v 'file-error
	   "Maximum number (%d) of symlinks exceeded" numchase-limit)))
      (directory-file-name result))))

(defun tramp-handle-file-writable-p (filename)
  "Like `file-writable-p' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-writable-p"
      (if (file-exists-p filename)
	  (tramp-check-cached-permissions v ?w)
	;; If file doesn't exist, check if directory is writable.
	(and (file-directory-p (file-name-directory filename))
	     (file-writable-p (file-name-directory filename)))))))

(defcustom tramp-allow-unsafe-temporary-files nil
  "Whether root-owned auto-save, backup or lock files can be written to \"/tmp\"."
  :version "28.1"
  :type 'boolean
  :link '(tramp-info-link :tag "Tramp manual"
			  tramp-allow-unsafe-temporary-files))

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
		   (= (or (file-attribute-user-id
			   (file-attributes filename 'integer))
			  tramp-unknown-id-integer)
		      tramp-root-id-integer)
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
  (require 'ls-lisp)
  (defvar ls-lisp-dirs-first)
  (defvar ls-lisp-emulation)
  (defvar ls-lisp-ignore-case)
  (defvar ls-lisp-use-insert-directory-program)
  (defvar ls-lisp-verbosity)
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
      ;; We bind `ls-lisp-emulation' to nil (which is GNU).
      ;; `ls-lisp-set-options' modifies `ls-lisp-ignore-case',
      ;; `ls-lisp-dirs-first' and `ls-lisp-verbosity', so we bind them
      ;; as well.  We don't want to use `insert-directory-program'.
      (let (ls-lisp-emulation ls-lisp-ignore-case ls-lisp-dirs-first
	    ls-lisp-verbosity ls-lisp-use-insert-directory-program start)
	;; Set proper options based on `ls-lisp-emulation'.
	(tramp-compat-funcall 'ls-lisp-set-options)
	(tramp-run-real-handler
	 #'insert-directory
	 (list filename switches wildcard full-directory-p))
	;; `ls-lisp' always returns full listings.  We must remove
	;; superfluous parts.
	(unless (string-search "l" switches)
	  (save-excursion
	    (goto-char (point-min))
	    (while (setq start
			 (text-property-not-all
			  (point) (line-end-position) 'dired-filename t))
	      (delete-region
	       start
	       (or (text-property-any
		    start (line-end-position) 'dired-filename t)
		   (line-end-position)))
	      (if (= (line-beginning-position) (line-end-position))
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
	  (condition-case err
	      (tramp-barf-if-file-missing v filename
		(with-tramp-progress-reporter
		    v 3 (format-message "Inserting `%s'" filename)
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
		    ;; name handlers.  It doesn't work for encrypted files.
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
			       (tramp-make-tramp-file-name v remote-copy)))
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
			     local-copy visit beg end replace))))))

	    (file-error
	     (let ((tramp-verbose (if visit 0 tramp-verbose)))
	       (tramp-error v 'file-missing filename)))
	    (error
	     (add-hook 'find-file-not-found-functions
		       `(lambda () (signal ',(car err) ',(cdr err)))
		       nil t)
	     (signal (car err) (cdr err))))

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
	  (delete-file (tramp-make-tramp-file-name v remote-copy))))

      ;; Result.
      (cons filename (cdr result)))))

(defun tramp-ps-time ()
  "Read printed time oif \"ps\" in format \"[[DD-]hh:]mm:ss\".
Return it as number of seconds.  Used in `tramp-process-attributes-ps-format'."
  (search-forward-regexp (rx (+ blank)))
  (search-forward-regexp (rx (? (? (group (+ digit)) "-")
				   (group (+ digit)) ":")
			           (group (+ digit)) ":"
			           ;; Seconds can also be a floating point num.
			           (group (+ (any "." digit))))
			 (line-end-position) 'noerror)
  (+ (* 24 60 60 (string-to-number (or (match-string 1) "0")))
        (* 60 60 (string-to-number (or (match-string 2) "0")))
           (* 60 (string-to-number (or (match-string 3) "0")))
                 (string-to-number (or (match-string 4) "0"))))

(defconst tramp-process-attributes-ps-args
  `("-eww"
    "-o"
    ,(string-join
     '("pid"
       "euid"
       "euser"
       "egid"
       "egroup"
       "comm:80"
       "state"
       "ppid"
       "pgrp"
       "sess"
       "tname"
       "tpgid"
       "min_flt"
       "maj_flt"
       "times"
       "pri"
       "nice"
       "thcount"
       "vsize"
       "rss"
       "etimes"
       "pcpu"
       "pmem"
       "args")
     ","))
  "List of arguments for calling \"ps\".
See `tramp-get-process-attributes'.

This list is the default value on remote GNU/Linux systems.")

(defconst tramp-process-attributes-ps-format
  '((pid . number)
    (euid . number)
    (user . string)
    (egid . number)
    (group . string)
    (comm . 80)
    (state . string)
    (ppid . number)
    (pgrp . number)
    (sess . number)
    (ttname . string)
    (tpgid . number)
    (minflt . number)
    (majflt . number)
    (time . number)
    (pri . number)
    (nice . number)
    (thcount . number)
    (vsize . number)
    (rss . number)
    (etime . number)
    (pcpu . number)
    (pmem . number)
    (args . nil))
  "Alist where each element is a cons cell of the form `\(KEY . TYPE)'.
KEY is a key (symbol) used in `process-attributes'.  TYPE is the
printed result for KEY of the \"ps\" command, it can be `number',
`string', a number (string of that length), a symbol (a function
to be applied), or nil (for the last column of the \"ps\" output.

This alist is used to parse the output of calling \"ps\" in
`tramp-get-process-attributes'.

This alist is the default value on remote GNU/Linux systems.")

(defun tramp-get-process-attributes (vec)
  "Return all process attributes for connection VEC.
Parsing the remote \"ps\" output is controlled by
`tramp-process-attributes-ps-args' and
`tramp-process-attributes-ps-format'.

It is not guaranteed, that all process attributes as described in
`process-attributes' are returned.  The additional attribute
`pid' shall be returned always."
  (with-tramp-connection-property vec " process-attributes"
    (ignore-errors
      (with-temp-buffer
        (hack-connection-local-variables-apply
         (connection-local-criteria-for-default-directory))
        ;; (pop-to-buffer (current-buffer))
        (when (zerop
               (apply
                #'process-file "ps" nil t nil tramp-process-attributes-ps-args))
          (let (result res)
            (goto-char (point-min))
            (while (not (eobp))
              ;; (tramp-test-message
              ;;  "%s" (buffer-substring (point) (line-end-position)))
              (when (save-excursion
                      (search-forward-regexp
		       (rx digit) (line-end-position) 'noerror))
                (setq res nil)
                (dolist (elt tramp-process-attributes-ps-format)
                  (push
                   (cons
                    (car elt)
                    (cond
                     ((eq (cdr elt) 'number) (read (current-buffer)))
                     ((eq (cdr elt) 'string)
                      (search-forward-regexp (rx (+ (not blank))))
                      (match-string 0))
                     ((numberp (cdr elt))
                      (search-forward-regexp (rx (+ blank)))
                      (search-forward-regexp (rx (+ nonl)) (+ (point) (cdr elt)))
                      (string-trim (match-string 0)))
                     ((fboundp (cdr elt))
                      (funcall (cdr elt)))
                     ((null (cdr elt))
                      (search-forward-regexp (rx (+ blank)))
                      (buffer-substring (point) (line-end-position)))))
                   res))
                ;; `nice' could be `-'.
                (setq res (rassq-delete-all '- res))
                (push (append res) result))
              (forward-line))
            ;; Return result.
            result))))))

(defun tramp-handle-list-system-processes ()
  "Like `list-system-processes' for Tramp files."
  (let ((v (tramp-dissect-file-name default-directory)))
    (tramp-flush-connection-property v " process-attributes")
    (mapcar (lambda (x) (cdr (assq 'pid x))) (tramp-get-process-attributes v))))

(defun tramp-get-lock-file (file)
  "Read lockfile info of FILE.
Return nil when there is no lockfile."
  (when-let* ((lockname (make-lock-file-name file)))
    (or (file-symlink-p lockname)
	(and (file-readable-p lockname)
	     (with-temp-buffer
	       (insert-file-contents-literally lockname)
	       (buffer-string))))))

(defvar tramp-lock-pid nil
  "A random number local for every connection.
Do not set it manually, it is used buffer-local in `tramp-get-lock-pid'.")

(defun tramp-get-lock-pid (file)
  "Determine pid for lockfile of FILE."
  ;; Not all Tramp methods use an own process.  So we use a random
  ;; number, which is as good as a process id.
  (with-current-buffer
      (tramp-get-connection-buffer (tramp-dissect-file-name file))
    (or tramp-lock-pid
	(setq-local
	 tramp-lock-pid (number-to-string (random most-positive-fixnum))))))

(defconst tramp-lock-file-info-regexp
  ;; USER@HOST.PID[:BOOT_TIME]
  (rx bos (group (+ nonl))
      "@" (group (+ nonl))
      "." (group (+ digit))
      (? ":" (? "-") (+ digit)) eos)
  "The format of a lock file.")

(defun tramp-handle-file-locked-p (file)
  "Like `file-locked-p' for Tramp files."
  (when-let* ((info (tramp-get-lock-file file))
	      (match (string-match tramp-lock-file-info-regexp info)))
    (or ; Locked by me.
        (and (string-equal (match-string 1 info) (user-login-name))
	     (string-equal (match-string 2 info) tramp-system-name)
	     (string-equal (match-string 3 info) (tramp-get-lock-pid file)))
	; User name.
	(match-string 1 info))))

(defun tramp-handle-lock-file (file)
  "Like `lock-file' for Tramp files."
  ;; See if this file is visited and has changed on disk since it
  ;; was visited.
  (catch 'dont-lock
    (unless (eq (file-locked-p file) t) ;; Locked by me.
      (when (and buffer-file-truename
		 (not (verify-visited-file-modtime))
		 (file-exists-p file))
	;; In filelock.c, `userlock--ask-user-about-supersession-threat'
	;; is called, which also checks file contents.  This is unwise
	;; for remote files.
	(ask-user-about-supersession-threat file))

      (when-let* ((info (tramp-get-lock-file file))
		  (match (string-match tramp-lock-file-info-regexp info)))
	(unless (ask-user-about-lock
		 file (format
		       "%s@%s (pid %s)" (match-string 1 info)
		       (match-string 2 info) (match-string 3 info)))
	  (throw 'dont-lock nil)))

      (when-let* ((lockname (make-lock-file-name file))
	          ;; USER@HOST.PID[:BOOT_TIME]
	          (info
	           (format
	            "%s@%s.%s" (user-login-name) tramp-system-name
	            (tramp-get-lock-pid file))))

	;; Protect against security hole.
	(with-parsed-tramp-file-name file nil
	  (when (and (not tramp-allow-unsafe-temporary-files)
		     create-lockfiles
		     (file-in-directory-p lockname temporary-file-directory)
		     (= (or (file-attribute-user-id
			     (file-attributes file 'integer))
			    tramp-unknown-id-integer)
			tramp-root-id-integer)
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
               (write-region info nil lockname nil 'no-message)))))))))

(defun tramp-handle-make-lock-file-name (file)
  "Like `make-lock-file-name' for Tramp files."
  (and create-lockfiles
       (not remote-file-name-inhibit-locks)
       (tramp-run-real-handler 'make-lock-file-name (list file))))

(defun tramp-handle-unlock-file (file)
  "Like `unlock-file' for Tramp files."
  (condition-case err
      ;; When there is no connection, we don't do it.  Otherwise,
      ;; functions like `kill-buffer' would try to reestablish the
      ;; connection.  See Bug#61663.
      (if-let* ((v (tramp-dissect-file-name file))
		((process-live-p (tramp-get-process v)))
		(lockname (make-lock-file-name file)))
          (delete-file lockname)
	;; Trigger the unlock error.  Be quiet if user isn't
	;; interested in lock files.  See Bug#70900.
	(unless (or (not create-lockfiles)
		    (bound-and-true-p remote-file-name-inhibit-locks))
	  (signal 'file-error `("Cannot remove lock file for" ,file))))
    ;; `userlock--handle-unlock-error' checks for `create-lockfiles'
    ;; since Emacs 30.1, we don't need this check here, then.
    (error (unless (or (not create-lockfiles) remote-file-name-inhibit-locks)
             (userlock--handle-unlock-error err)))))

(defun tramp-handle-load (file &optional noerror nomessage nosuffix must-suffix)
  "Like `load' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name file) nil
    (unless nosuffix
      (cond ((file-exists-p (concat file ".elc"))
	     (setq file (concat file ".elc")))
	    ((file-exists-p (concat file ".el"))
	     (setq file (concat file ".el")))))
    (when (and must-suffix (not (string-match-p (rx ".el" (? "c") eos) file)))
      (tramp-error
       v 'file-error "File `%s' does not include a `.el' or `.elc' suffix" file))
    (unless (or noerror (file-exists-p file))
      (tramp-error v 'file-missing file))
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
  (let ((tramp-verbose 0))
    (and (tramp-sh-file-name-handler-p vec)
	 (not (tramp-get-method-parameter vec 'tramp-copy-program)))))

(defun tramp-add-hops (vec)
  "Add ad-hoc proxy definitions to `tramp-default-proxies-alist'."
  ;; `tramp-default-proxies-alist' is bound to `tramp-cache-undefined'
  ;; in `tramp-handle-file-name-as-directory' and
  ;; `tramp-handle-file-name-directory' suppressing to add a hop.
  (when-let* (((not (eq	tramp-default-proxies-alist tramp-cache-undefined)))
	      (hops (tramp-file-name-hop vec))
	      (item vec))
    (let (signal-hook-function changed)
      (dolist
	  (proxy (reverse (split-string hops tramp-postfix-hop-regexp 'omit)))
	(let* ((host-port (tramp-file-name-host-port item))
	       (host-port (and (stringp host-port)
			       (rx bol (literal host-port) eol)))
	       (user-domain (tramp-file-name-user-domain item))
	       (user-domain (and (stringp user-domain)
				 (rx bol (literal user-domain) eol)))
	       (proxy (concat
		       tramp-prefix-format proxy tramp-postfix-host-format))
	       (entry
		(list host-port user-domain (propertize proxy 'tramp-ad-hoc t))))
	  ;; Remove superfluous entries.
	  (when tramp-show-ad-hoc-proxies
	    (dolist (entry1 tramp-default-proxies-alist)
	      (when (and (equal host-port (car entry1))
			 (equal user-domain (cadr entry1))
			 (not (equal proxy (caddr entry1))))
		(tramp-message
		 vec 5 "Remove %S from `tramp-default-proxies-alist'" entry1)
		(tramp-cleanup-connection
		 vec 'keep-debug 'keep-password 'keep-processes)
		(setq tramp-default-proxies-alist
		      (delete entry1 tramp-default-proxies-alist)))))
	  ;; Add the hop.
	  (unless (member entry tramp-default-proxies-alist)
	    (tramp-message vec 5 "Add %S to `tramp-default-proxies-alist'" entry)
	    (add-to-list 'tramp-default-proxies-alist entry)
	    (setq changed t))
	  (setq item (tramp-dissect-file-name proxy))))
      ;; Save the new value.
      (when (and tramp-save-ad-hoc-proxies changed)
	(customize-save-variable
	 'tramp-default-proxies-alist tramp-default-proxies-alist)))))

(defun tramp-compute-multi-hops (vec)
  "Expands VEC according to `tramp-default-proxies-alist'."
  (let ((saved-tdpa tramp-default-proxies-alist)
	(target-alist `(,vec))
	(item vec)
	choices proxy)

    ;; `tramp-compute-multi-hops' could be called also for other file
    ;; name handlers, for example in `tramp-clear-passwd'.
    (when (tramp-sh-file-name-handler-p vec)

      ;; Ad-hoc proxy definitions.
      (tramp-add-hops vec)

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
		  (tramp-format-spec
		   proxy
		   (format-spec-make
		    ?u (or (tramp-file-name-user (car target-alist)) "")
		    ?h (or (tramp-file-name-host (car target-alist)) ""))))
	    (with-parsed-tramp-file-name proxy l
	      ;; Add the hop.
	      (push l target-alist)
	      ;; Start next search.
	      (setq choices tramp-default-proxies-alist)))))

      ;; Foreign and out-of-band methods are not supported for
      ;; multi-hops.
      (when (cdr target-alist)
	(setq choices target-alist)
	(while (setq item (pop choices))
	  (unless (tramp-multi-hop-p item)
	    (setq tramp-default-proxies-alist saved-tdpa)
	    (tramp-user-error
	     vec "Method `%s' is not supported for multi-hops"
	     (tramp-file-name-method item)))))

      ;; Some methods ("su", "sg", "sudo", "doas", "run0", "ksu") do
      ;; not use the host name in their command template.  In this
      ;; case, the remote file name must use either a local host name
      ;; (first hop), or a host name matching the previous hop.
      (let ((previous-host (or tramp-local-host-regexp "")))
	(setq choices target-alist)
	(while (setq item (pop choices))
	  (let ((host (tramp-file-name-host item)))
	    (unless
		(or
		 ;; The host name is used for the remote shell command.
		 (member
		  "%h" (flatten-tree
			(tramp-get-method-parameter item 'tramp-login-args)))
		 ;; The host name must match previous hop.
		 (string-match-p previous-host host))
	      (setq tramp-default-proxies-alist saved-tdpa)
	      (tramp-user-error
	       vec "Host name `%s' does not match `%s'" host previous-host))
	    (setq previous-host (rx bol (literal host) eol))))))

    ;; Result.
    target-alist))

(defvar tramp-extra-expand-args nil
  "Method specific arguments.")

(defun tramp-expand-args (vec parameter default &rest spec-list)
  "Expand login arguments as given by PARAMETER in `tramp-methods'.
PARAMETER is a symbol like `tramp-login-args', denoting a list of
list of strings from `tramp-methods', containing %-sequences for
substitution.  DEFAULT is used when PARAMETER is not specified.
SPEC-LIST is a list of char/value pairs used for
`format-spec-make'.  It is appended by `tramp-extra-expand-args',
a connection-local variable."
  (let ((args (tramp-get-method-parameter vec parameter default))
	(extra-spec-list
	 (mapcar
	  #'eval
	  (buffer-local-value
	   'tramp-extra-expand-args (tramp-get-connection-buffer vec))))
	spec)
    ;; Merge both spec lists.  Remove duplicate entries.
    (while spec-list
      (unless (member (car spec-list) extra-spec-list)
	(setq extra-spec-list
	      (append (tramp-compat-take 2 spec-list) extra-spec-list)))
      (setq spec-list (cddr spec-list)))
    (setq spec (apply #'format-spec-make extra-spec-list))
    ;; Expand format spec.
    (flatten-tree
     (mapcar
      (lambda (x)
	(setq x (mapcar (lambda (y) (tramp-format-spec y spec)) x))
	(unless (member "" x) x))
      args))))

(defun tramp-post-process-creation (proc vec)
  "Apply actions after creation of process PROC."
  (declare (tramp-suppress-trace t))
  (process-put proc 'tramp-vector vec)
  (process-put proc 'adjust-window-size-function #'ignore)
  (set-process-query-on-exit-flag proc nil)
  (tramp-taint-remote-process-buffer (process-buffer proc))
  (when (process-command proc)
    (tramp-message vec 6 "%s" (string-join (process-command proc) " "))))

(defvar tramp-direct-async-process nil
  "Whether direct asynchronous processes should be used.
It is not recommended to change this variable globally.  Instead, it
should be set connection-local.")

(defun tramp-direct-async-process-p (&rest args)
  "Whether direct async `make-process' can be called."
  (let ((v (tramp-dissect-file-name default-directory))
	(buffer (plist-get args :buffer))
	(stderr (plist-get args :stderr)))
    ;; Since Tramp 2.7.1.  In a future release, we'll ignore this
    ;; connection property.
    (when (and (not (tramp-compat-connection-local-p
		     tramp-direct-async-process))
	       (tramp-connection-property-p v "direct-async-process"))
      (let ((msg (concat
		  "Connection property \"direct-async-process\" is deprecated, "
		  "use connection-local variable `tramp-direct-async-process'\n"
		  "See (info \"(tramp) Improving performance of "
		  "asynchronous remote processes\")")))
	(if (tramp-get-connection-property
	     tramp-null-hop "direct-async-process-warned")
	    (tramp-message v 2 msg)
	  (tramp-set-connection-property
	   tramp-null-hop "direct-async-process-warned" t)
	  (tramp-warning v msg))))

    (and ;; The method supports it.
         (tramp-get-method-parameter v 'tramp-direct-async)
	 ;; It has been indicated.  We don't use the global value of
	 ;; `tramp-direct-async-process'.
	 (or (and (tramp-compat-connection-local-p tramp-direct-async-process)
		  (tramp-compat-connection-local-value
		   tramp-direct-async-process))
	     ;; Deprecated setting.
             (tramp-get-connection-property v "direct-async-process"))
	 ;; There's no multi-hop.
	 (or (not (tramp-multi-hop-p v))
	     (null (cdr (tramp-compute-multi-hops v))))
	 ;; There's no remote stdout or stderr file.
	 (or (not (stringp buffer)) (not (tramp-tramp-file-p buffer)))
	 (or (not (stringp stderr)) (not (tramp-tramp-file-p stderr))))))

(defun tramp-handle-make-process (&rest args)
  "An alternative `make-process' implementation for Tramp files."
  (tramp-skeleton-make-process args nil nil
    ;; Check for `tramp-sh-file-name-handler' and
    ;; `adb-file-name-handler-p', because something is different
    ;; between tramp-sh.el, and tramp-adb.el or tramp-sshfs.el.
    (let* ((default-directory tramp-compat-temporary-file-directory)
	   (sh-file-name-handler-p (tramp-sh-file-name-handler-p v))
	   (adb-file-name-handler-p (tramp-adb-file-name-p v))
	   (env (mapcar
		 (lambda (elt)
		   (when (string-search "=" elt) elt))
		 tramp-remote-process-environment))
	   ;; We use as environment the difference to toplevel
	   ;; `process-environment'.
	   (env (dolist (elt process-environment env)
		  (when (and
			 (string-search "=" elt)
			 (not
			  (member
			   elt (default-toplevel-value 'process-environment))))
		    (setq env (cons elt env)))))
	   ;; Add remote path if exists.
	   (env (if-let* ((sh-file-name-handler-p)
			  (remote-path
			   (string-join (tramp-get-remote-path v) ":")))
		    (setenv-internal env "PATH" remote-path 'keep)
		  env))
	   ;; Add HISTFILE if indicated.
	   (env (if sh-file-name-handler-p
		    (cond
		     ((stringp tramp-histfile-override)
		      (setenv-internal
		       env "HISTFILE" tramp-histfile-override 'keep))
		     (tramp-histfile-override
		      (setq env (setenv-internal env "HISTFILE" "''" 'keep))
		      (setq env (setenv-internal env "HISTSIZE" "0" 'keep))
		      (setenv-internal env "HISTFILESIZE" "0" 'keep))
		     (t env))
		  env))
	   ;; Add INSIDE_EMACS.
	   (env (setenv-internal env "INSIDE_EMACS" (tramp-inside-emacs) 'keep))
	   (env (mapcar #'tramp-shell-quote-argument (delq nil env)))
	   ;; Quote command.
	   (command (mapconcat #'tramp-shell-quote-argument command " "))
	   ;; Set cwd and environment variables.
	   (command
	    (append
	     `("cd" ,(tramp-shell-quote-argument localname) "&&" "(" "env")
	     env `(,command ")")))
	   ;; Add remote shell if needed.
	   (command
	    (if (consp (tramp-get-method-parameter v 'tramp-direct-async))
		(append
		 (tramp-get-method-parameter v 'tramp-direct-async)
                 `(,(string-join command " ")))
	      command))
	   (login-program
	    (tramp-get-method-parameter v 'tramp-login-program))
	   ;; We don't create the temporary file.  In fact, it is just
	   ;; a prefix for the ControlPath option of ssh; the real
	   ;; temporary file has another name, and it is created and
	   ;; protected by ssh.  It is also removed by ssh when the
	   ;; connection is closed.  The temporary file name is cached
	   ;; in the main connection process, therefore we cannot use
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
	   (device
	    (when adb-file-name-handler-p
	      (tramp-compat-funcall
		  'tramp-adb-get-device v)))
           (pta (unless (eq connection-type 'pipe) "-t"))
	   login-args p)

      ;; Command could be too long, for example due to a longish PATH.
      (when (and sh-file-name-handler-p
		 (length> (string-join command) (tramp-get-remote-pipe-buf v)))
	(signal 'error (cons "Command too long:" command)))

      (setq
       ;; Replace `login-args' place holders.  Split ControlMaster
       ;; options.
       login-args
       (append
	(flatten-tree (tramp-get-method-parameter v 'tramp-async-args))
	(flatten-tree
	 (mapcar
	  (lambda (x) (split-string x " "))
	  (tramp-expand-args
	   v 'tramp-login-args nil
	   ?h (or host "") ?u (or user "") ?p (or port "")
	   ?c (format-spec (or options "") (format-spec-make ?t tmpfile))
	   ?d (or device "") ?a (or pta "") ?l ""))))
       ;; Suppress `internal-default-process-sentinel', which is set
       ;; when :sentinel is nil.  (Bug#71049)
       p (make-process
	  :name name :buffer buffer
	  :command (append `(,login-program) login-args command)
	  :coding coding :noquery noquery :connection-type connection-type
	  :sentinel (or sentinel #'ignore) :stderr stderr))
      ;; Set filter.  Prior Emacs 29.1, it doesn't work reliably to
      ;; provide it as `make-process' argument when filter is t.  See
      ;; Bug#51177.
      (when filter
	(set-process-filter p filter))
      (tramp-post-process-creation p v)
      ;; Query flag is overwritten in `tramp-post-process-creation',
      ;; so we reset it.
      (set-process-query-on-exit-flag p (null noquery))
      (process-put p 'remote-command orig-command)
      (when (bufferp stderr)
	(tramp-taint-remote-process-buffer stderr))

      p)))

(defun tramp-handle-make-symbolic-link
    (_target linkname &optional _ok-if-already-exists)
  "Like `make-symbolic-link' for Tramp files.
This is the fallback implementation for backends which do not
support symbolic links."
  (tramp-error
   (tramp-dissect-file-name (expand-file-name linkname)) 'file-error
   "make-symbolic-link not supported"))

(defun tramp-handle-memory-info ()
  "Like `memory-info' for Tramp files."
  (let ((result (list 0 0 0 0))
        process-file-side-effects)
    (with-temp-buffer
      (cond
       ;; GNU/Linux.
       ((zerop (process-file "cat" nil '(t) nil "/proc/meminfo"))
        (goto-char (point-min))
        (when
            (search-forward-regexp
             (rx bol "MemTotal:" (* space) (group (+ digit)) (* space) "kB" eol)
             nil 'noerror)
          (setcar (nthcdr 0 result) (string-to-number (match-string 1))))
        (goto-char (point-min))
        (when
            (search-forward-regexp
             (rx bol "MemFree:" (* space) (group (+ digit)) (* space) "kB" eol)
             nil 'noerror)
          (setcar (nthcdr 1 result) (string-to-number (match-string 1))))
        (goto-char (point-min))
        (when
            (search-forward-regexp
             (rx bol "SwapTotal:" (* space) (group (+ digit)) (* space) "kB" eol)
             nil 'noerror)
          (setcar (nthcdr 2 result) (string-to-number (match-string 1))))
        (goto-char (point-min))
        (when
            (search-forward-regexp
             (rx bol "SwapFree:" (* space) (group (+ digit)) (* space) "kB" eol)
             nil 'noerror)
          (setcar (nthcdr 3 result) (string-to-number (match-string 1)))))

       ;; BSD.
       ;; https://raw.githubusercontent.com/ocochard/myscripts/master/FreeBSD/freebsd-memory.sh
       ((zerop (process-file "sysctl" nil '(t) nil "-a"))
        (goto-char (point-min))
        (when
            (search-forward-regexp
             (rx bol "hw.pagesize:" (* space) (group (+ digit)) eol)
             nil 'noerror)
          (let ((pagesize (string-to-number (match-string 1))))
            (goto-char (point-min))
            (when
                (search-forward-regexp
                 (rx bol "vm.stats.vm.v_page_count:" (* space)
                     (group (+ digit)) eol)
                 nil 'noerror)
              (setcar
               (nthcdr 0 result)
               (/ (* (string-to-number (match-string 1)) pagesize) 1024)))
            (goto-char (point-min))
            (when
                (search-forward-regexp
                 (rx bol "vm.stats.vm.v_free_count:" (* space)
                     (group (+ digit)) eol)
                 nil 'noerror)
              (setcar
               (nthcdr 1 result)
               (/ (* (string-to-number (match-string 1)) pagesize) 1024)))))
        (erase-buffer)
        (when (zerop (process-file "swapctl" nil '(t) nil "-sk"))
          (goto-char (point-min))
          (when
              (search-forward-regexp
               (rx bol "Total:" (* space)
                   (group (+ digit)) (* space) (group (+ digit)) eol)
               nil 'noerror)
            (setcar (nthcdr 2 result) (string-to-number (match-string 1)))
            (setcar
             (nthcdr 3 result)
             (- (string-to-number (match-string 1))
                (string-to-number (match-string 2)))))))))

    ;; Return result.
    (unless (equal result '(0 0 0 0))
      result)))

(defun tramp-handle-process-attributes (pid)
  "Like `process-attributes' for Tramp files."
  (catch 'result
    (dolist (elt (tramp-get-process-attributes
                  (tramp-dissect-file-name default-directory)))
      (when (= (cdr (assq 'pid elt)) pid)
        (throw 'result elt)))))

(defun tramp-handle-shell-command (command &optional output-buffer error-buffer)
  "Like `shell-command' for Tramp files."
  (let* ((asynchronous (string-match-p (rx (* blank) "&" (* blank) eos) command))
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
	       (if asynchronous
		   (or shell-command-buffer-name-async "*Async Shell Command*")
		 (or shell-command-buffer-name "*Shell Command Output*"))))))
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
      (shell-command-save-pos-or-erase current-buffer-p))

    (if (integerp asynchronous)
	(let ((tramp-remote-process-environment
	       (if (natnump async-shell-command-width)
		   (cons (format "COLUMNS=%d"
				 (bound-and-true-p async-shell-command-width))
			 tramp-remote-process-environment)
		 tramp-remote-process-environment)))
	  (prog1
	      ;; Run the process.
	      (setq p (start-file-process-shell-command
		       (buffer-name output-buffer) buffer command))
	    (when (process-live-p p)
	      ;; Display output.
	      (with-current-buffer output-buffer
		(setq mode-line-process '(":%s"))
                (cond
                 ((boundp 'async-shell-command-mode)
                  ;; Emacs 30+
                  (unless (eq major-mode async-shell-command-mode)
                    (funcall async-shell-command-mode)))
                 ((not (eq major-mode 'shell-mode))
                  (shell-mode)))
		(set-process-filter p #'comint-output-filter)
		(set-process-sentinel p #'shell-command-sentinel)
		(when error-file
		  (add-function
		   :after (process-sentinel p)
		   (lambda (_proc _string)
		     (ignore-errors
		       (with-current-buffer error-buffer
			 (insert-file-contents-literally
			  error-file nil nil nil 'replace))
		       (delete-file error-file)))))
                (if async-shell-command-display-buffer
                    ;; Display buffer immediately.
                    (display-buffer output-buffer '(nil (allow-no-window . t)))
                  ;; Defer displaying buffer until first process output.
                  ;; Use disposable named advice so that the buffer is
                  ;; displayed at most once per process lifetime.
                  (let ((nonce (make-symbol "nonce")))
                    (add-function
		     :before (process-filter p)
                     (lambda (proc _string)
                       (let ((buf (process-buffer proc)))
                         (when (buffer-live-p buf)
                           (remove-function (process-filter proc)
                                            nonce)
                           (display-buffer buf '(nil (allow-no-window . t))))))
                     `((name . ,nonce)))))))
	    ;; Insert error messages if they were separated.
	    (when (and error-file (not (process-live-p p)))
	      (ignore-errors
		(with-current-buffer error-buffer
		  (insert-file-contents-literally error-file))
		(delete-file error-file)))))

      ;; Synchronous case.
      (prog1
	  ;; Run the process.
	  (process-file-shell-command command nil buffer)
	;; Insert error messages if they were separated.
	(when error-file
	  (ignore-errors
	    (with-current-buffer error-buffer
	      (insert-file-contents-literally error-file))
	    (delete-file error-file)))
	(if current-buffer-p
	    ;; This is like exchange-point-and-mark, but doesn't
	    ;; activate the mark.  It is cleaner to avoid activation,
	    ;; even though the command loop would deactivate the mark
	    ;; because we inserted text.
	    (progn
	      (goto-char (prog1 (mark t)
			   (set-marker (mark-marker) (point)
				       (current-buffer))))
	      (shell-command-set-point-after-cmd))
	  ;; There's some output, display it.
	  (when (with-current-buffer output-buffer (> (point-max) (point-min)))
	    (display-message-or-buffer output-buffer)))))))

(defun tramp-handle-start-file-process (name buffer program &rest args)
  "Like `start-file-process' for Tramp files.
BUFFER might be a list, in this case STDERR is separated."
  (make-process
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
  (if (file-name-quoted-p filename)
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
	  (if (string-match-p (rx "//" (| "/" "~")) localname)
	      (setq filename
                    (replace-regexp-in-string
                     (rx bos (+ "/")) "/" (substitute-in-file-name localname)))
	    (setq filename
		  (concat (file-remote-p filename)
			  (replace-regexp-in-string
                           (rx bos (+ "/")) "/"
			   ;; We must disable cygwin-mount file name
			   ;; handlers and alike.
			   (tramp-run-real-handler
			    #'substitute-in-file-name (list localname)))))))
	filename))))

(defconst tramp-time-dont-know '(0 0 0 1000)
  "An invalid time value, used as \"Don't know\" value.")

(defconst tramp-time-doesnt-exist '(-1 65535)
  "An invalid time value, used as \"Doesn't exist\" value.")

(defsubst tramp-defined-time (time)
  "Return TIME or nil (when TIME is not a time spec)."
  (unless (or (time-equal-p time tramp-time-doesnt-exist)
	      (time-equal-p time tramp-time-dont-know))
    time))

(defun tramp-handle-set-visited-file-modtime (&optional time-list)
  "Like `set-visited-file-modtime' for Tramp files."
  (unless (buffer-file-name)
    (error "Can't set-visited-file-modtime: buffer `%s' not visiting a file"
	   (buffer-name)))
  (unless time-list
    (let ((remote-file-name-inhibit-cache t))
      (setq time-list
	    (or (file-attribute-modification-time
		 (file-attributes (buffer-file-name)))
		tramp-time-doesnt-exist))))
  (unless (time-equal-p time-list tramp-time-dont-know)
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
	       (modtime (file-attribute-modification-time attr))
	       (mt (visited-file-modtime)))

	  (cond
	   ;; File exists, and has a known modtime.
	   ((and attr (not (time-equal-p modtime tramp-time-dont-know)))
	    (< (abs (tramp-time-diff modtime mt)) 2))
	   ;; Modtime has the don't know value.
	   (attr t)
	   ;; If file does not exist, say it is not modified if and
	   ;; only if that agrees with the buffer's record.
	   (t (time-equal-p mt tramp-time-doesnt-exist))))))))

(defun tramp-handle-write-region
  (start end filename &optional append visit lockname mustbenew)
  "Like `write-region' for Tramp files."
  (tramp-skeleton-write-region start end filename append visit lockname mustbenew
    (let ((tmpfile (tramp-compat-make-temp-file filename))
	  (modes (tramp-default-file-modes
		  filename (and (eq mustbenew 'excl) 'nofollow))))
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
      ;; Now, `last-coding-system-used' has the right value.  Remember it.
      (setq coding-system-used last-coding-system-used)
      (condition-case nil
	  (rename-file tmpfile filename 'ok-if-already-exists)
	(error
	 (delete-file tmpfile)
	 (tramp-error
	  v 'file-error "Couldn't write region to `%s'" filename))))))

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
  ;; There might be pending output.  Avoid problems with reentrant
  ;; call of Tramp.
  (ignore-errors
    (while (tramp-accept-process-output proc)))
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
		  (process-get proc 'tramp-watch-name))))))

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
    (tramp-message
     vec 6 "\n%s" (tramp-get-buffer-string (tramp-get-connection-buffer vec)))
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
      (unless (or tramp-password-prompt-not-unique
		  (tramp-get-connection-property
		   (tramp-get-connection-property
		    proc "hop-vector"
		    (process-get proc 'tramp-vector))
		   " first-password-request"))
	(tramp-clear-passwd vec))
      (goto-char (point-min))
      (tramp-check-for-regexp proc tramp-process-action-regexp)
      (tramp-message vec 3 "Sending %s" (match-string 1))
      ;; We don't call `tramp-send-string' in order to hide the
      ;; password from the debug buffer and the traces.
      (process-send-string
       proc
       (concat
	(funcall
	 (if tramp-password-prompt-not-unique
	     #'tramp-read-passwd-without-cache #'tramp-read-passwd)
	 proc)
	tramp-local-end-of-line))
      ;; Hide password prompt.
      (narrow-to-region (point-max) (point-max))))
  t)

(defun tramp-action-otp-password (proc vec)
  "Query the user for a one-time password."
  (with-current-buffer (process-buffer proc)
    (let ((case-fold-search t)
	  prompt)
      (goto-char (point-min))
      (tramp-check-for-regexp proc tramp-process-action-regexp)
      (setq prompt (concat (string-trim (match-string 1)) " "))
      (tramp-message vec 3 "Sending %s" (match-string 1))
      ;; We don't call `tramp-send-string' in order to hide the
      ;; password from the debug buffer and the traces.
      (process-send-string
       proc
       (concat
	(tramp-read-passwd-without-cache proc prompt) tramp-local-end-of-line))
      ;; Hide password prompt.
      (narrow-to-region (point-max) (point-max))))
  t)

(defcustom tramp-use-fingerprint t
  "Whether fingerprint prompts shall be used for authentication."
  :version "30.2"
  :type 'boolean
  :link '(tramp-info-link :tag "Tramp manual" tramp-use-fingerprint))

(defun tramp-action-fingerprint (proc vec)
  "Query the user for a fingerprint verification.
Interrupt the query if `tramp-use-fingerprint' is nil."
  (with-current-buffer (process-buffer proc)
    (if tramp-use-fingerprint
	(tramp-action-show-message proc vec)
      (interrupt-process proc)
      ;; Hide message.
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
    (tramp-message
     vec 6 "\n%s" (tramp-get-buffer-string (tramp-get-connection-buffer vec)))
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
    (tramp-message
     vec 6 "\n%s" (tramp-get-buffer-string (tramp-get-connection-buffer vec)))
    (tramp-send-string vec (concat "y" tramp-local-end-of-line)))
  t)

(defun tramp-action-terminal (_proc vec)
  "Tell the remote host which terminal type to use.
The terminal type can be configured with `tramp-terminal-type'."
  (tramp-message vec 5 "Setting `%s' as terminal type." tramp-terminal-type)
  (tramp-message
   vec 6 "\n%s" (tramp-get-buffer-string (tramp-get-connection-buffer vec)))
  (tramp-send-string vec (concat tramp-terminal-type tramp-local-end-of-line))
  t)

(defun tramp-action-show-message (proc vec)
  "Show the user a message for confirmation.
Wait, until the connection buffer changes."
  (with-current-buffer (process-buffer proc)
    (let ((cursor-in-echo-area t)
	  set-message-function clear-message-function tramp-dont-suspend-timers)
      (with-tramp-suspended-timers
	;; Silence byte compiler.
	(ignore set-message-function clear-message-function)
	(tramp-message vec 6 "\n%s" (buffer-string))
	(goto-char (point-min))
	(tramp-check-for-regexp proc tramp-process-action-regexp)
	(with-temp-message (concat (string-trim (match-string 0)) " ")
	  ;; Hide message in buffer.
	  (narrow-to-region (point-max) (point-max))
	  ;; Wait for new output.
	  (while (length= (buffer-string) 0)
	    (tramp-accept-process-output proc))))))
  t)

(defun tramp-action-confirm-message (_proc vec)
  "Return RET in order to confirm the message."
  (tramp-message
   vec 6 "\n%s" (tramp-get-buffer-string (tramp-get-connection-buffer vec)))
  (tramp-send-string vec tramp-local-end-of-line)
  t)

(defun tramp-action-show-and-confirm-message (proc vec)
  "Show the user a message for confirmation.
Wait, until the connection buffer changes."
  (with-current-buffer (process-buffer proc)
    (let ((cursor-in-echo-area t)
	  set-message-function clear-message-function tramp-dont-suspend-timers)
      (with-tramp-suspended-timers
	;; Silence byte compiler.
	(ignore set-message-function clear-message-function)
	(tramp-message vec 6 "\n%s" (buffer-string))
	(goto-char (point-min))
	(tramp-check-for-regexp proc tramp-process-action-regexp)
	(with-temp-message (concat (string-trim (match-string 0)) " ")
	  ;; Hide message in buffer.
	  (narrow-to-region (point-max) (point-max))
	  ;; Wait for new output.
	  (while (not (ignore-error file-error
			(tramp-wait-for-regexp
			 proc 0.1
			 (rx (| (regexp tramp-security-key-confirmed-regexp)
				(regexp tramp-security-key-pin-regexp)
				(regexp tramp-security-key-timeout-regexp))))))
	    (when (tramp-check-for-regexp proc tramp-security-key-timeout-regexp)
	      (throw 'tramp-action 'timeout))
	    (redisplay 'force))))))
  t)

(defun tramp-action-process-alive (proc _vec)
  "Check, whether a process has finished."
  (unless (process-live-p proc)
    ;; There might be pending output.
    (while (tramp-accept-process-output proc))
    (throw 'tramp-action 'process-died)))

(defun tramp-action-out-of-band (proc vec)
  "Check, whether an out-of-band copy has finished."
  ;; There might be pending output for the exit status.
  (while (tramp-accept-process-output proc))
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
	   (if (search-forward-regexp tramp-operation-not-permitted-regexp nil t)
	       (progn
		 (tramp-message vec 5 "'set mode' error ignored.")
		 (tramp-message vec 3 "Process has finished.")
		 (throw 'tramp-action 'ok))
	     (tramp-message vec 3 "Process has died.")
	     (throw 'tramp-action 'out-of-band-failed))))))

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
      (while (tramp-accept-process-output proc))
      ;; Remove ANSI control escape sequences.
      (with-current-buffer (tramp-get-connection-buffer vec)
	(goto-char (point-min))
	(while (search-forward-regexp ansi-color-control-seq-regexp nil t)
	  (replace-match "")))
      (setq todo actions)
      (while todo
	(setq item (pop todo)
	      tramp-process-action-regexp (symbol-value (nth 0 item))
	      pattern (rx (group (regexp tramp-process-action-regexp)) eos)
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
  ;; use the "hop-vector" property in case we have several hops.
  (tramp-set-connection-property
   (tramp-get-connection-property
    proc "hop-vector" (process-get proc 'tramp-vector))
   " first-password-request" tramp-cache-read-persistent-data)
  (save-restriction
    (with-tramp-progress-reporter
	proc 3 "Waiting for prompts from remote shell"
      (let ((enable-recursive-minibuffers t)
	    exit)
	(with-tramp-timeout (timeout (setq exit 'timeout))
	  (while (not exit)
	    (setq exit (catch 'tramp-action
			 (tramp-process-one-action proc vec actions)))))
	(with-current-buffer (tramp-get-connection-buffer vec)
	  (widen)
	  (tramp-message vec 6 "\n%s" (buffer-string)))
	(if (eq exit 'ok)
	    (ignore-errors
	      (when (functionp tramp-password-save-function)
		(funcall tramp-password-save-function)
                (setq tramp-password-save-function nil)))
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
  `(if (tramp-get-connection-property ,proc "locked")
       ;; Be kind for old versions of Emacs.
       (if (member 'remote-file-error debug-ignored-errors)
	   (throw 'non-essential 'non-essential)
	 ;(tramp-backtrace ,proc 'force)
	 (tramp-error
	  ,proc 'remote-file-error "Forbidden reentrant call of Tramp"))
     (with-tramp-suspended-timers
       (unwind-protect
	   (progn
	     (tramp-set-connection-property ,proc "locked" t)
	     ,@body)
	 (tramp-flush-connection-property ,proc "locked")))))

(defun tramp-accept-process-output (proc &optional _timeout)
  "Like `accept-process-output' for Tramp processes.
This is needed in order to hide `last-coding-system-used', which is set
for process communication also.
If the user quits via `C-g', it is propagated up to `tramp-file-name-handler'."
  (declare (advertised-calling-convention (proc) "29.2"))
  ;; There could be other processes which use the same socket for
  ;; communication.  This could block the output for the current
  ;; process.  Read such output first.  (Bug#61350)
  ;; The process property isn't set anymore due to Bug#62194.
  (when-let* (((process-get proc 'tramp-shared-socket))
	      (v (process-get proc 'tramp-vector)))
    (dolist (p (delq proc (process-list)))
      (when (tramp-file-name-equal-p v (process-get p 'tramp-vector))
	(with-tramp-suspended-timers
	  (with-local-quit (accept-process-output p 0 nil t))))))

  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t)
	  last-coding-system-used
	  result)
      ;; This must be protected by the "locked" property.
      (with-tramp-locked-connection proc
	;; JUST-THIS-ONE is set due to Bug#12145.  `with-local-quit'
	;; returns t in order to report success.
	(if (with-local-quit
	      (setq result (accept-process-output proc 0 nil t)) t)
	    (tramp-message
	     proc 10 "%s %s %s\n%s"
	     proc (process-status proc) result (buffer-string))
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
  (search-backward-regexp regexp (max (point-min) (- (point) 256)) 'noerror))

(defun tramp-check-for-regexp (proc regexp)
  "Check, whether REGEXP is contained in process buffer of PROC.
Erase echoed commands if exists."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-min))

    ;; Check whether we need to remove echo output.  The max length of
    ;; the echo mark regexp is taken for search.  We restrict the
    ;; search for the second echo mark to PIPE_BUF characters.
    (when (and (tramp-get-connection-property proc "check-remote-echo")
	       (search-forward-regexp
		tramp-echoed-echo-mark-regexp
		(+ (point) (* 5 tramp-echo-mark-marker-length)) t))
      (let ((begin (match-beginning 0)))
	(when
	    (search-forward-regexp
	     tramp-echoed-echo-mark-regexp
	     (+ (point) (tramp-get-connection-property proc "pipe-buf" 4096)) t)
	  ;; Discard echo from remote output.
	  (tramp-flush-connection-property proc "check-remote-echo")
	  (tramp-message proc 5 "echo-mark found")
	  (forward-line 1)
	  (delete-region begin (point))
	  (goto-char (point-min)))))

    (when (or (not (tramp-get-connection-property proc "check-remote-echo"))
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
    (with-tramp-timeout (timeout)
      (while (not found)
	;; This is needed to yield the CPU, otherwise we'll see 100% CPU load.
	(sit-for 0 'nodisp)
	(tramp-accept-process-output proc)
	(unless (process-live-p proc)
	  (tramp-error-with-buffer
	   nil proc 'file-error "Process has died"))
	(setq found (tramp-check-for-regexp proc regexp))))
    ;; The process could have timed out, for example due to session
    ;; timeout of sudo.  The process buffer does not exist any longer then.
    (ignore-errors
      (tramp-message
       proc 6 "\n%s" (tramp-get-buffer-string (process-buffer proc))))
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
	 (chunksize (tramp-get-connection-property p "chunksize")))
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
	    (string-join (split-string string "\n") tramp-rsh-end-of-line))
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
    (let ((vec (process-get proc 'tramp-vector))
	  (buf (process-buffer proc))
	  (prompt (tramp-get-connection-property proc "prompt")))
      (when vec
	(tramp-message vec 5 "Sentinel called: `%S' `%s'" proc event)
        (tramp-flush-connection-properties proc)
        (tramp-flush-directory-properties vec "/"))
      (when (buffer-live-p buf)
	(with-current-buffer buf
          (when (and prompt (tramp-search-regexp (rx (literal prompt))))
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
  (when-let* ((dir (file-name-directory filename))
	      (modes (file-modes dir))
	      ((not (zerop (logand modes #o2000)))))
    (setq gid (file-attribute-group-id (file-attributes dir))))

  (if (tramp-tramp-file-p filename)
      (funcall (if (tramp-crypt-file-name-p filename)
		   #'tramp-crypt-file-name-handler #'tramp-file-name-handler)
	       #'tramp-set-file-uid-gid filename uid gid)
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
     ((equal id-format 'string) (group-name (group-gid)))
     ((file-attribute-group-id (file-attributes "~/" id-format))))))

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
		 (rx bol (literal (car candidates)) (? "\r") eol)
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
  (when-let* ((offset (cond
                       ((eq ?r access) 1)
                       ((eq ?w access) 2)
                       ((eq ?x access) 3)
                       ((eq ?s access) 3)))
	      (file-attr (file-attributes (tramp-make-tramp-file-name vec)))
	      (remote-uid (tramp-get-remote-uid vec 'integer))
	      (remote-gid (tramp-get-remote-gid vec 'integer)))
    (or
     ;; Not a symlink.
     (eq t (file-attribute-type file-attr))
     (null (file-attribute-type file-attr)))
    (or
     ;; World accessible.
     (eq access (aref (file-attribute-modes file-attr) (+ offset 6)))
     ;; User accessible and owned by user.
     (and
      (eq access (aref (file-attribute-modes file-attr) offset))
      (or (equal remote-uid tramp-root-id-integer)
	  (equal remote-uid tramp-unknown-id-integer)
	  (equal remote-uid (file-attribute-user-id file-attr))
	  (equal tramp-unknown-id-integer (file-attribute-user-id file-attr))))
     ;; Group accessible and owned by user's principal group.
     (and
      (eq access
	  (aref (file-attribute-modes file-attr) (+ offset 3)))
      (or (equal remote-gid tramp-root-id-integer)
	  (equal remote-gid tramp-unknown-id-integer)
	  (equal remote-gid (file-attribute-group-id file-attr))
	  (equal tramp-unknown-id-integer
		 (file-attribute-group-id file-attr))))
     ;; Group accessible and owned by user's secondary group.
     (and
      (eq access
	  (aref (file-attribute-modes file-attr) (+ offset 3)))
      (member (file-attribute-group-id file-attr)
	      (tramp-get-remote-groups vec 'integer))))))

(defmacro tramp-convert-file-attributes (vec localname id-format attr)
  "Convert `file-attributes' ATTR generated Tramp backend functions.
Convert file mode bits to string and set virtual device number.
Set file uid and gid according to ID-FORMAT.  LOCALNAME is used
to cache the result.  Return the modified ATTR."
  (declare (indent 3) (debug t))
  `(when-let*
       ((result
	 (with-tramp-file-property ,vec ,localname "file-attributes"
	   (when-let* ((attr ,attr))
	     (save-match-data
	       ;; Remove ANSI control escape sequences from symlink.
	       (when (stringp (car attr))
		 (while (string-match ansi-color-control-seq-regexp (car attr))
		   (setcar attr (replace-match "" nil nil (car attr)))))
	       ;; Convert uid and gid.  Use `tramp-unknown-id-integer'
	       ;; as indication of unusable value.
	       (when (consp (nth 2 attr))
		 (when (and (numberp (cdr (nth 2 attr)))
			    (< (cdr (nth 2 attr)) 0))
		   (setcdr (car (nthcdr 2 attr)) tramp-unknown-id-integer))
		 (when (and (floatp (cdr (nth 2 attr)))
			    (<= (cdr (nth 2 attr)) most-positive-fixnum))
		   (setcdr (car (nthcdr 2 attr)) (round (cdr (nth 2 attr))))))
	       (when (consp (nth 3 attr))
		 (when (and (numberp (cdr (nth 3 attr)))
			    (< (cdr (nth 3 attr)) 0))
		   (setcdr (car (nthcdr 3 attr)) tramp-unknown-id-integer))
		 (when (and (floatp (cdr (nth 3 attr)))
			    (<= (cdr (nth 3 attr)) most-positive-fixnum))
		   (setcdr (car (nthcdr 3 attr)) (round (cdr (nth 3 attr))))))
	       ;; Convert last access time.
	       (unless (listp (nth 4 attr))
		 (setcar (nthcdr 4 attr) (seconds-to-time (nth 4 attr))))
	       ;; Convert last modification time.
	       (unless (listp (nth 5 attr))
		 (setcar (nthcdr 5 attr) (seconds-to-time (nth 5 attr))))
	       ;; Convert last status change time.
	       (unless (listp (nth 6 attr))
		 (setcar (nthcdr 6 attr) (seconds-to-time (nth 6 attr))))
	       ;; Convert file size.
	       (when (< (nth 7 attr) 0)
		 (setcar (nthcdr 7 attr) -1))
	       (when (and (floatp (nth 7 attr))
			  (<= (nth 7 attr) most-positive-fixnum))
		 (setcar (nthcdr 7 attr) (round (nth 7 attr))))
	       ;; Convert file mode bits to string.
	       (unless (stringp (nth 8 attr))
		 (setcar (nthcdr 8 attr)
			 (tramp-file-mode-from-int (nth 8 attr)))
		 (when (stringp (car attr))
		   (aset (nth 8 attr) 0 ?l)))
	       ;; Convert directory indication bit.
	       (when (string-prefix-p "d" (nth 8 attr))
		 (setcar attr t))
	       ;; Convert symlink from `tramp-do-file-attributes-with-stat'.
	       ;; Decode also multibyte string.
	       (when (consp (car attr))
		 (setcar attr
			 (and (stringp (caar attr))
			      (string-match
			       (rx (+ nonl) " -> " nonl (group (+ nonl)) nonl)
			       (caar attr))
			      (decode-coding-string
			       (match-string 1 (caar attr)) 'utf-8))))
	       ;; Set file's gid change bit.
	       (setcar
		(nthcdr 9 attr)
		(not (= (cdr (nth 3 attr))
			(or (tramp-get-remote-gid ,vec 'integer)
			    tramp-unknown-id-integer))))
	       ;; Convert inode.
	       (when (floatp (nth 10 attr))
		 (setcar (nthcdr 10 attr)
			 (condition-case nil
			     (let ((high (nth 10 attr))
				   middle low)
			       (if (<= high most-positive-fixnum)
				   (floor high)
				 ;; The low 16 bits.
				 (setq low (mod high #x10000)
				       high (/ high #x10000))
				 (if (<= high most-positive-fixnum)
				     (cons (floor high) (floor low))
				   ;; The middle 24 bits.
				   (setq middle (mod high #x1000000)
					 high (/ high #x1000000))
				   (cons (floor high)
					 (cons (floor middle) (floor low))))))
			   ;; Inodes can be incredible huge.  We must
			   ;; hide this.
			   (error (tramp-get-inode ,vec)))))
	       ;; Set virtual device number.
	       (setcar (nthcdr 11 attr)
		       (tramp-get-device ,vec))
	       ;; Set SELinux context.
	       (when (stringp (nth 12 attr))
		 (tramp-set-file-property
		  ,vec ,localname  "file-selinux-context"
		  (split-string (nth 12 attr) ":" 'omit)))
	       ;; Remove optional entries.
	       (setcdr (nthcdr 11 attr) nil)
	       attr)))))

     ;; Return normalized result.
     (append (tramp-compat-take 2 result)
	     (if (eq ,id-format 'string)
		 (list (car (nth 2 result)) (car (nth 3 result)))
	       (list (cdr (nth 2 result)) (cdr (nth 3 result))))
	     (nthcdr 4 result))))

(defun tramp-get-home-directory (vec &optional user)
  "The remote home directory for connection VEC as local file name.
If USER is a string, return its home directory instead of the
user identified by VEC.  If there is no user specified in either
VEC or USER, or if there is no home directory, return nil."
  (and (tramp-file-name-p vec)
       (with-tramp-connection-property vec (concat "~" user)
	 (tramp-file-name-handler #'tramp-get-home-directory vec user))))

(defun tramp-get-remote-uid (vec id-format)
  "The uid of the remote connection VEC, in ID-FORMAT.
ID-FORMAT valid values are `string' and `integer'."
  (or (and (tramp-file-name-p vec)
	   (with-tramp-connection-property vec (format "uid-%s" id-format)
	     (tramp-file-name-handler #'tramp-get-remote-uid vec id-format)))
      ;; Ensure there is a valid result.
      (and (equal id-format 'integer) tramp-unknown-id-integer)
      (and (equal id-format 'string) tramp-unknown-id-string)))

(defun tramp-get-remote-gid (vec id-format)
  "The gid of the remote connection VEC, in ID-FORMAT.
ID-FORMAT valid values are `string' and `integer'."
  (or (and (tramp-file-name-p vec)
	   (with-tramp-connection-property vec (format "gid-%s" id-format)
	     (tramp-file-name-handler #'tramp-get-remote-gid vec id-format)))
      ;; Ensure there is a valid result.
      (and (equal id-format 'integer) tramp-unknown-id-integer)
      (and (equal id-format 'string) tramp-unknown-id-string)))

(defun tramp-get-remote-groups (vec id-format)
  "The list of groups of the remote connection VEC, in ID-FORMAT.
ID-FORMAT valid values are `string' and `integer'."
  (and (tramp-file-name-p vec)
       (with-tramp-connection-property vec (format "groups-%s" id-format)
	 (tramp-file-name-handler #'tramp-get-remote-groups vec id-format))))

(defun tramp-read-id-output (vec)
  "Read in connection buffer the output of the `id' command.
Set connection properties \"{uid,gid,groups}-{integer,string}\"."
  (with-current-buffer (tramp-get-connection-buffer vec)
    (let (uid-integer uid-string
	  gid-integer gid-string
	  groups-integer groups-string)
      (goto-char (point-min))
      ;; Read uid.
      (when (search-forward-regexp
	     (rx "uid=" (group (+ digit)) "(" (group (+ (any "_-" alnum))) ")")
	     nil 'noerror)
	(setq uid-integer (string-to-number (match-string 1))
	      uid-string (match-string 2)))
      ;; Read gid.
      (when (search-forward-regexp
	     (rx "gid=" (group (+ digit)) "(" (group (+ (any "_-" alnum))) ")")
	     nil 'noerror)
	(setq gid-integer (string-to-number (match-string 1))
	      gid-string (match-string 2)))
      ;; Read groups.
      (when (search-forward-regexp (rx "groups=") nil 'noerror)
	(while (looking-at
		(rx (group (+ digit)) "(" (group (+ (any "_-" alnum))) ")"))
	  (setq groups-integer (cons (string-to-number (match-string 1))
				     groups-integer)
		groups-string (cons (match-string 2) groups-string))
	  (goto-char (match-end 0))
	  (skip-chars-forward ",")))
      ;; Set connection properties.
      (tramp-set-connection-property vec "uid-integer" uid-integer)
      (tramp-set-connection-property vec "uid-string" uid-string)
      (tramp-set-connection-property vec "gid-integer" gid-integer)
      (tramp-set-connection-property vec "gid-string" gid-string)
      (tramp-set-connection-property
       vec "groups-integer" (nreverse groups-integer))
      (tramp-set-connection-property
       vec "groups-string" (nreverse groups-string)))))

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
     ;; Direct actions aren't possible for encrypted directories.
     (null tramp-crypt-enabled)
     ;; The local temp directory must be writable for the other user.
     (file-writable-p
      (tramp-make-tramp-file-name vec tramp-compat-temporary-file-directory))
     ;; On some systems, chown runs only for root.
     (or (zerop (user-uid))
	 (= (tramp-get-remote-uid vec 'integer) tramp-root-id-integer)))))

(defun tramp-get-remote-tmpdir (vec)
  "Return directory for temporary files on the remote host identified by VEC."
  (with-tramp-connection-property (tramp-get-process vec) "remote-tmpdir"
    (let ((dir (tramp-make-tramp-file-name
		vec (tramp-get-method-parameter vec 'tramp-tmpdir "/tmp"))))
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
  (let (create-lockfiles)
    (cl-letf (((symbol-function 'tramp-remote-acl-p) #'ignore)
	      ((symbol-function 'tramp-remote-selinux-p) #'ignore)
	      ((symbol-function 'tramp-smb-remote-acl-p) #'ignore)
	      ((symbol-function 'tramp-sudoedit-remote-acl-p) #'ignore)
	      ((symbol-function 'tramp-sudoedit-remote-selinux-p) #'ignore))
      (tramp-file-local-name
       (make-temp-file
	(expand-file-name
	 tramp-temp-name-prefix (tramp-get-remote-tmpdir vec)))))))

(defun tramp-delete-temp-file-function ()
  "Remove temporary files related to current buffer."
  (declare (tramp-suppress-trace t))
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
	       (file-name-unquote (buffer-file-name)))
	      tramp-auto-save-directory)))
	  result)
      (prog1 ;; Run plain `make-auto-save-file-name'.
	  (setq result (tramp-run-real-handler #'make-auto-save-file-name nil))
	;; Protect against security hole.
	(when (and (not tramp-allow-unsafe-temporary-files)
		   auto-save-default
		   (file-in-directory-p result temporary-file-directory)
		   (= (or (file-attribute-user-id
			   (file-attributes filename 'integer))
			  tramp-unknown-id-integer)
		      tramp-root-id-integer)
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
        (while (string-match (rx (literal from)) string)
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
  (let ((temporary-file-directory (temporary-file-directory)))
    (make-temp-file prefix dir-flag suffix)))

;;; Compatibility functions section:

(defun tramp-call-process
  (vec program &optional infile destination display &rest args)
  "Call `call-process' on the local host.
It always returns a return code.  The Lisp error raised when
PROGRAM is nil is trapped also, returning 1.  Furthermore, traces
are written with verbosity of 6."
  (let ((default-directory tramp-compat-temporary-file-directory)
	(temporary-file-directory tramp-compat-temporary-file-directory)
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
		 #'call-process program infile (or destination t) display args)
		output (tramp-get-buffer-string destination))
	  ;; `result' could also be an error string.
	  (when (stringp result)
	    (setq error result
		  result 1)))
      (error
       (setq error (error-message-string err)
	     result 1)))
    (if (tramp-string-empty-or-nil-p error)
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
	(temporary-file-directory tramp-compat-temporary-file-directory)
	(process-environment (default-toplevel-value 'process-environment))
	(buffer (if (eq buffer t) (current-buffer) buffer))
	(vec (or vec (car tramp-current-connection)))
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
          (if (zerop result)
              (tramp-message vec 6 "%d" result)
            (tramp-message
	     vec 6 "%d\n%s" result (tramp-get-buffer-string buffer))))
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
    (tramp-message vec 6 "\n%s" (string-join result "\n"))
    result))

(defun tramp-process-running-p (process-name)
  "Return t if system process PROCESS-NAME is running for `user-login-name'."
  (when (stringp process-name)
    (catch 'result
      (let ((default-directory temporary-file-directory))
	(dolist (pid (list-system-processes))
	  (and-let* ((attributes (process-attributes pid))
		     (comm (cdr (assoc 'comm attributes)))
		     ((string-equal
		       (cdr (assoc 'user attributes)) (user-login-name)))
		     ;; The returned command name could be truncated
		     ;; to 15 characters.  Therefore, we cannot check
		     ;; for `string-equal'.
		     ((string-prefix-p comm process-name))
		     ((throw 'result t)))))))))

;; When calling "emacs -Q", `auth-source-search' won't be called.  If
;; you want to debug exactly this case, call "emacs -Q --eval '(setq
;; tramp-cache-read-persistent-data t)'" instead.
(defun tramp-read-passwd (proc &optional prompt)
  "Read a password from user (compat function).
Consults the auth-source package."
  (declare (tramp-suppress-trace t))
  (let* (;; If `auth-sources' contains "~/.authinfo.gpg", and
	 ;; `exec-path' contains a relative file name like ".", it
	 ;; could happen that the "gpg" command is not found.  So we
	 ;; adapt `default-directory'.  (Bug#39389, Bug#39489)
	 (default-directory tramp-compat-temporary-file-directory)
	 (case-fold-search t)
	 ;; In tramp-sh.el, we must use "hop-vector" and "pw-vector"
	 ;; due to multi-hop.
	 (vec (process-get proc 'tramp-vector))
	 (hop-vec (tramp-get-connection-property proc "hop-vector" vec))
	 (pw-vec (tramp-get-connection-property proc "pw-vector" hop-vec))
	 (key (tramp-make-tramp-file-name pw-vec 'noloc))
	 (method (tramp-file-name-method pw-vec))
	 (user-domain (or (tramp-file-name-user-domain pw-vec)
			  (tramp-get-connection-property pw-vec "login-as")))
	 (host-port (tramp-file-name-host-port pw-vec))
	 (pw-prompt
	  (string-trim-left
	   (or prompt
	       (with-current-buffer (process-buffer proc)
		 (tramp-check-for-regexp proc tramp-password-prompt-regexp)
		 (if (string-match-p "passphrase" (match-string 1))
		     (match-string 0)
		   (format "%s for %s " (capitalize (match-string 1)) key))))))
	 ;; If there is no user name, `:create' triggers to ask for.
	 ;; We suppress it.
	 (pw-spec (list :max 1 :user user-domain :host host-port :port method
			:require (cons :secret (and user-domain '(:user)))
			:create (and user-domain t)))
	 (auth-source-creation-prompts `((secret . ,pw-prompt)))
	 ;; Use connection-local value.
	 (auth-sources (buffer-local-value 'auth-sources (process-buffer proc)))
	 auth-info auth-passwd tramp-dont-suspend-timers)

    (unwind-protect
	(or
	 (setq tramp-password-save-function nil)
	 ;; See if `auth-sources' contains something useful.
	 (ignore-errors
	   (and (tramp-get-connection-property hop-vec " first-password-request")
		(setq auth-info (car (apply #'auth-source-search pw-spec))
		      tramp-password-save-function
		      (plist-get auth-info :save-function)
		      auth-passwd
		      (tramp-compat-auth-info-password auth-info))))

	 ;; Try the password cache.
	 (with-tramp-suspended-timers
	   (setq auth-passwd
		 (password-read
		  pw-prompt (auth-source-format-cache-entry pw-spec))
		 tramp-password-save-function
                 (when auth-source-do-cache
		   (lambda ()
		     (password-cache-add
		      (auth-source-format-cache-entry pw-spec) auth-passwd))))
	   auth-passwd))

      ;; Remember the values.
      (tramp-set-connection-property hop-vec " pw-spec" pw-spec)
      (tramp-set-connection-property hop-vec " first-password-request" nil))))

(defun tramp-read-passwd-without-cache (proc &optional prompt)
  "Read a password from user (compat function)."
  (declare (tramp-suppress-trace t))
  ;; We suspend the timers while reading the password.
  (let (tramp-dont-suspend-timers)
    (with-tramp-suspended-timers
      (password-read
       (or prompt
	   (with-current-buffer (process-buffer proc)
	     (tramp-check-for-regexp proc tramp-password-prompt-regexp)
	     (match-string 0)))))))

(defun tramp-clear-passwd (vec)
  "Clear password cache for connection related to VEC."
  (declare (tramp-suppress-trace t))
  (when-let* ((hop (cadr (reverse (tramp-compute-multi-hops vec)))))
    ;; Clear also the passwords of the hops.
    (tramp-clear-passwd hop))
  (when-let* ((pw-spec (tramp-get-connection-property vec " pw-spec")))
    (auth-source-forget pw-spec)))

(defun tramp-time-diff (t1 t2)
  "Return the difference between the two times, in seconds.
T1 and T2 are time values (as returned by `current-time' for example)."
  (declare (tramp-suppress-trace t))
  (float-time (time-subtract t1 t2)))

(defun tramp-unquote-shell-quote-argument (s)
  "Remove quotation prefix \"/:\" from string S, and quote it then for shell.
Suppress `shell-file-name'.  This is needed on w32 systems, which
would use a wrong quoting for local file names.  See `w32-shell-name'."
  (let (shell-file-name)
    (shell-quote-argument (file-name-unquote s))))

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
      (let ((result (tramp-unquote-shell-quote-argument s)))
	(when (and (>= (length result) 2)
		   (string= (substring result 0 2) "\\~"))
	  (setq result (substring result 1)))
	(replace-regexp-in-string
	 (rx "\\" (literal tramp-rsh-end-of-line))
	 (format "'%s'" tramp-rsh-end-of-line) result)))))

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
	 (process-get proc 'tramp-vector)
	 (format "(\\kill -2 -%d || \\kill -2 %d) 2>%s"
                 pid pid
                 (tramp-get-remote-null-device
		  (process-get proc 'tramp-vector))))
	;; Wait, until the process has disappeared.  If it doesn't,
	;; fall back to the default implementation.
        (while (tramp-accept-process-output proc))
	(not (process-live-p proc))))))

(add-hook 'interrupt-process-functions #'tramp-interrupt-process)
(add-hook
 'tramp-unload-hook
 (lambda ()
   (remove-hook 'interrupt-process-functions #'tramp-interrupt-process)))

(defun tramp-signal-process (process sigcode &optional remote)
  "Send PROCESS the signal with code SIGCODE.
PROCESS may also be a number specifying the process id of the
process to signal; in this case, the process need not be a child of
this Emacs.
If PROCESS is a process object which contains the property
`remote-pid', or PROCESS is a number and REMOTE is a remote file name,
PROCESS is interpreted as process on the respective remote host, which
will be the process to signal.
If PROCESS is a string, it is interpreted as process object with
the respective process name, or as a number.
SIGCODE may be an integer, or a symbol whose name is a signal name."
  (when (stringp process)
    (setq process (or (get-process process)
		      (and (string-match-p (rx bol (+ digit) eol) process)
			   (string-to-number process))
		      (signal 'wrong-type-argument (list #'processp process)))))
  (let (pid vec)
    (cond
     ((processp process)
      (setq pid (process-get process 'remote-pid)
            vec (process-get process 'tramp-vector)))
     ((numberp process)
      (setq pid process
            vec (and (stringp remote) (tramp-dissect-file-name remote))))
     (t (signal 'wrong-type-argument (list #'processp process))))
    (cond
     ((symbolp sigcode)
      (setq sigcode (upcase (symbol-name sigcode)))
      (when (string-prefix-p "SIG" sigcode)
        (setq sigcode (substring sigcode 3))))
     ((not (numberp sigcode))
      (signal 'wrong-type-argument (list #'numberp sigcode))))
    ;; If it's a Tramp process, send SIGCODE remotely.
    (when (and pid vec)
      (tramp-message
       vec 5 "Send signal %s to process %s with pid %s" sigcode process pid)
      ;; This is for tramp-sh.el.  Other backends do not support this (yet).
      (if (tramp-compat-funcall
           'tramp-send-command-and-check
           vec (format "\\kill -%s %d" sigcode pid))
          0 -1))))

;; `signal-process-functions' exists since Emacs 29.1.
(when (boundp 'signal-process-functions)
  (add-hook 'signal-process-functions #'tramp-signal-process)
  (add-hook
   'tramp-unload-hook
   (lambda ()
     (remove-hook 'signal-process-functions #'tramp-signal-process))))

(defun tramp-get-remote-null-device (vec)
  "Return null device on the remote host identified by VEC.
If VEC is `tramp-null-hop', return local null device."
  (if (equal vec tramp-null-hop)
      null-device
    (with-tramp-connection-property vec "null-device"
      (let ((default-directory (tramp-make-tramp-file-name vec)))
        (null-device)))))

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

(put #'tramp-unload-tramp 'tramp-autoload t)

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
;; * Implement file name abbreviation for a different user.  That is,
;;   (abbreviate-file-name "/ssh:user1@host:/home/user2") =>
;;   "/ssh:user1@host:~user2".
;;
;; * Implement file name abbreviation for user and host names.
;;
;; * Implement user and host name completion for multi-hops.  Some
;;   methods in tramp-container.el have it already.
;;
;; * Make it configurable, which environment variables are set in
;;   direct async processes.
;;
;; * Pass working dir for direct async processes, for example for
;;   container methods.

;;; tramp.el ends here
