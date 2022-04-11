;;; tramp-sh.el --- Tramp access functions for (s)sh-like connections  -*- lexical-binding:t -*-

;; Copyright (C) 1998-2022 Free Software Foundation, Inc.

;; (copyright statements below in code to be updated with the above notice)

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

;; The file name handler implementation for ssh-alike remote connections.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'tramp)

(declare-function dired-remove-file "dired-aux")
(defvar dired-compress-file-suffixes)
(defvar process-file-return-signal-string)
(defvar vc-handled-backends)
(defvar vc-bzr-program)
(defvar vc-git-program)
(defvar vc-hg-program)

;;;###tramp-autoload
(defconst tramp-default-remote-shell "/bin/sh"
  "The default remote shell Tramp applies.")

(defcustom tramp-inline-compress-start-size 4096
  "The minimum size of compressing where inline transfer.
When inline transfer, compress transferred data of file whose
size is this value or above (up to `tramp-copy-size-limit' for
out-of-band methods).
If it is nil, no compression at all will be applied."
  :group 'tramp
  :type '(choice (const nil) integer))

(defcustom tramp-copy-size-limit 10240
  "Maximum file size where inline copying is preferred to an out-of-the-band copy.
If it is nil, out-of-the-band copy will be used without a check."
  :group 'tramp
  :type '(choice (const nil) integer))

(defcustom tramp-histfile-override "~/.tramp_history"
  "When invoking a shell, override the HISTFILE with this value.
When setting to a string, it redirects the shell history to that
file.  Be careful when setting to \"/dev/null\"; this might
result in undesired results when using \"bash\" as shell.

The value t unsets any setting of HISTFILE, and sets both
HISTFILESIZE and HISTSIZE to 0.  If you set this variable to nil,
however, the *override* is disabled, so the history will go to
the default storage location, e.g. \"$HOME/.sh_history\"."
  :group 'tramp
  :version "25.2"
  :type '(choice (const :tag "Do not override HISTFILE" nil)
                 (const :tag "Unset HISTFILE" t)
                 (string :tag "Redirect to a file")))

;;;###tramp-autoload
(defconst tramp-display-escape-sequence-regexp "\e[[:digit:];[]+m"
  "Terminal control escape sequences for display attributes.")

(defconst tramp-device-escape-sequence-regexp "\e[[:digit:][]+n"
  "Terminal control escape sequences for device status.")

;; ksh on OpenBSD 4.5 requires that $PS1 contains a `#' character for
;; root users.  It uses the `$' character for other users.  In order
;; to guarantee a proper prompt, we use "#$ " for the prompt.

(defvar tramp-end-of-output
  (format
   "///%s#$"
   (md5 (concat (prin1-to-string process-environment) (current-time-string))))
  "String used to recognize end of output.
The `$' character at the end is quoted; the string cannot be
detected as prompt when being sent on echoing hosts, therefore.")

;;;###tramp-autoload
(defconst tramp-initial-end-of-output "#$ "
  "Prompt when establishing a connection.")

(defconst tramp-end-of-heredoc (md5 tramp-end-of-output)
  "String used to recognize end of heredoc strings.")

(defcustom tramp-use-ssh-controlmaster-options (not (eq system-type 'windows-nt))
  "Whether to use `tramp-ssh-controlmaster-options'.
Set it to nil, if you use Control* or Proxy* options in your ssh
configuration."
  :group 'tramp
  :version "28.1"
  :type 'boolean)

(defvar tramp-ssh-controlmaster-options nil
  "Which ssh Control* arguments to use.

If it is a string, it should have the form
\"-o ControlMaster=auto -o ControlPath=\\='tramp.%%r@%%h:%%p\\='
-o ControlPersist=no\".  Percent characters in the ControlPath
spec must be doubled, because the string is used as format string.

Otherwise, it will be auto-detected by Tramp, if
`tramp-use-ssh-controlmaster-options' is non-nil.  The value
depends on the installed local ssh version.

The string is used in `tramp-methods'.")

(defvar tramp-scp-strict-file-name-checking nil
  "Which scp strict file name checking argument to use.

It is the string \"-T\" if supported by the local scp (since
release 8.0), otherwise the string \"\".  If it is nil, it will
be auto-detected by Tramp.

The string is used in `tramp-methods'.")

;; Initialize `tramp-methods' with the supported methods.
;;;###tramp-autoload
(tramp--with-startup
 (add-to-list 'tramp-methods
              `("rcp"
                (tramp-login-program        "rsh")
                (tramp-login-args           (("%h") ("-l" "%u")))
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))
                (tramp-copy-program         "rcp")
                (tramp-copy-args            (("-p" "%k") ("-r")))
                (tramp-copy-keep-date       t)
                (tramp-copy-recursive       t)))
 (add-to-list 'tramp-methods
              `("remcp"
                (tramp-login-program        "remsh")
                (tramp-login-args           (("%h") ("-l" "%u")))
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))
                (tramp-copy-program         "rcp")
                (tramp-copy-args            (("-p" "%k")))
                (tramp-copy-keep-date       t)))
 (add-to-list 'tramp-methods
              `("scp"
                (tramp-login-program        "ssh")
                (tramp-login-args           (("-l" "%u") ("-p" "%p") ("%c")
				             ("-e" "none") ("%h")))
                (tramp-async-args           (("-q")))
                (tramp-direct-async         t)
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))
                (tramp-copy-program         "scp")
                (tramp-copy-args            (("-P" "%p") ("-p" "%k")
					     ("%x") ("-q") ("-r") ("%c")))
                (tramp-copy-keep-date       t)
                (tramp-copy-recursive       t)))
 (add-to-list 'tramp-methods
              `("scpx"
                (tramp-login-program        "ssh")
                (tramp-login-args           (("-l" "%u") ("-p" "%p") ("%c")
				             ("-e" "none") ("-t" "-t")
					     ("-o" "RemoteCommand=\"%l\"")
					     ("%h")))
                (tramp-async-args           (("-q")))
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))
                (tramp-copy-program         "scp")
                (tramp-copy-args            (("-P" "%p") ("-p" "%k")
				             ("%x") ("-q") ("-r") ("%c")))
                (tramp-copy-keep-date       t)
                (tramp-copy-recursive       t)))
 (add-to-list 'tramp-methods
              `("rsync"
                (tramp-login-program        "ssh")
                (tramp-login-args           (("-l" "%u") ("-p" "%p") ("%c")
				             ("-e" "none") ("%h")))
                (tramp-async-args           (("-q")))
                (tramp-direct-async         t)
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))
                (tramp-copy-program         "rsync")
                (tramp-copy-args            (("-t" "%k") ("-p") ("-r") ("-s")
					     ("-c")))
                (tramp-copy-env             (("RSYNC_RSH") ("ssh") ("%c")))
                (tramp-copy-keep-date       t)
                (tramp-copy-keep-tmpfile    t)
                (tramp-copy-recursive       t)))
 (add-to-list 'tramp-methods
              `("rsh"
                (tramp-login-program        "rsh")
                (tramp-login-args           (("%h") ("-l" "%u")))
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))))
 (add-to-list 'tramp-methods
              `("remsh"
                (tramp-login-program        "remsh")
                (tramp-login-args           (("%h") ("-l" "%u")))
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))))
 (add-to-list 'tramp-methods
              `("ssh"
                (tramp-login-program        "ssh")
                (tramp-login-args           (("-l" "%u") ("-p" "%p") ("%c")
				             ("-e" "none") ("%h")))
                (tramp-async-args           (("-q")))
                (tramp-direct-async         t)
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))))
 (add-to-list 'tramp-methods
              `("sshx"
                (tramp-login-program        "ssh")
                (tramp-login-args           (("-l" "%u") ("-p" "%p") ("%c")
				             ("-e" "none") ("-t" "-t")
					     ("-o" "RemoteCommand=\"%l\"")
					     ("%h")))
                (tramp-async-args           (("-q")))
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))))
 (add-to-list 'tramp-methods
              `("telnet"
                (tramp-login-program        "telnet")
                (tramp-login-args           (("%h") ("%p") ("%n")))
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))))
 (add-to-list 'tramp-methods
              `("nc"
                (tramp-login-program        "telnet")
                (tramp-login-args           (("%h") ("%p") ("%n")))
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))
                (tramp-copy-program         "nc")
                ;; We use "-v" for better error tracking.
                (tramp-copy-args            (("-w" "1") ("-v") ("%h") ("%r")))
                (tramp-remote-copy-program  "nc")
                ;; We use "-p" as required for newer busyboxes.  For older
                ;; busybox/nc versions, the value must be (("-l") ("%r")).  This
                ;; can be achieved by tweaking `tramp-connection-properties'.
                (tramp-remote-copy-args     (("-l") ("-p" "%r") ("%n")))))
 (add-to-list 'tramp-methods
              `("su"
                (tramp-login-program        "su")
                (tramp-login-args           (("-") ("%u")))
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))
                (tramp-connection-timeout   10)))
 (add-to-list 'tramp-methods
              `("sg"
                (tramp-login-program        "sg")
                (tramp-login-args           (("-") ("%u")))
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-args    ("-c"))
                (tramp-connection-timeout   10)))
 (add-to-list 'tramp-methods
              `("sudo"
                (tramp-login-program        "env")
                ;; The password template must be masked.  Otherwise,
                ;; it could be interpreted as password prompt if the
                ;; remote host echoes the command.
		;; The "-p" argument doesn't work reliably, see Bug#50594.
                (tramp-login-args           (("SUDO_PROMPT=P\"\"a\"\"s\"\"s\"\"w\"\"o\"\"r\"\"d\"\":")
                                             ("sudo") ("-u" "%u") ("-s") ("-H")
				             ("%l")))
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))
                (tramp-connection-timeout   10)
                (tramp-session-timeout      300)))
 (add-to-list 'tramp-methods
              `("doas"
                (tramp-login-program        "doas")
                (tramp-login-args           (("-u" "%u") ("-s")))
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-args    ("-c"))
                (tramp-connection-timeout   10)
                (tramp-session-timeout      300)))
 (add-to-list 'tramp-methods
              `("ksu"
                (tramp-login-program        "ksu")
                (tramp-login-args           (("%u") ("-q")))
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))
                (tramp-connection-timeout   10)))
 (add-to-list 'tramp-methods
              `("krlogin"
                (tramp-login-program        "krlogin")
                (tramp-login-args           (("%h") ("-l" "%u") ("-x")))
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))))
 (add-to-list 'tramp-methods
              `("plink"
                (tramp-login-program        "plink")
                (tramp-login-args           (("-l" "%u") ("-P" "%p") ("-ssh")
					     ("-t") ("%h") ("\"")
				             (,(format
				                "env 'TERM=%s' 'PROMPT_COMMAND=' 'PS1=%s'"
				                tramp-terminal-type
				                tramp-initial-end-of-output))
				             ("%l") ("\"")))
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))))
 (add-to-list 'tramp-methods
              `("plinkx"
                (tramp-login-program        "plink")
                (tramp-login-args           (("-load") ("%h") ("-t") ("\"")
				             (,(format
				                "env 'TERM=%s' 'PROMPT_COMMAND=' 'PS1=%s'"
				                tramp-terminal-type
				                tramp-initial-end-of-output))
				             ("%l") ("\"")))
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))))
 (add-to-list 'tramp-methods
              `("pscp"
                (tramp-login-program        "plink")
                (tramp-login-args           (("-l" "%u") ("-P" "%p") ("-ssh")
					     ("-t") ("%h") ("\"")
				             (,(format
				                "env 'TERM=%s' 'PROMPT_COMMAND=' 'PS1=%s'"
				                tramp-terminal-type
				                tramp-initial-end-of-output))
				             ("%l") ("\"")))
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))
                (tramp-copy-program         "pscp")
                (tramp-copy-args            (("-l" "%u") ("-P" "%p") ("-scp")
					     ("-p" "%k") ("-q") ("-r")))
                (tramp-copy-keep-date       t)
                (tramp-copy-recursive       t)))
 (add-to-list 'tramp-methods
              `("psftp"
                (tramp-login-program        "plink")
                (tramp-login-args           (("-l" "%u") ("-P" "%p") ("-ssh")
					     ("-t") ("%h") ("\"")
				             (,(format
				                "env 'TERM=%s' 'PROMPT_COMMAND=' 'PS1=%s'"
				                tramp-terminal-type
				                tramp-initial-end-of-output))
				             ("%l") ("\"")))
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-c"))
                (tramp-copy-program         "pscp")
                (tramp-copy-args            (("-l" "%u") ("-P" "%p") ("-sftp")
					     ("-p" "%k") ("-q")))
                (tramp-copy-keep-date       t)))
 (add-to-list 'tramp-methods
              `("fcp"
                (tramp-login-program        "fsh")
                (tramp-login-args           (("%h") ("-l" "%u") ("sh" "-i")))
                (tramp-remote-shell         ,tramp-default-remote-shell)
                (tramp-remote-shell-login   ("-l"))
                (tramp-remote-shell-args    ("-i") ("-c"))
                (tramp-copy-program         "fcp")
                (tramp-copy-args            (("-p" "%k")))
                (tramp-copy-keep-date       t)))

 (add-to-list 'tramp-default-method-alist
	      `(,tramp-local-host-regexp "\\`root\\'" "su"))

 (add-to-list 'tramp-default-user-alist
	      `(,(concat "\\`" (regexp-opt '("su" "sudo" "doas" "ksu")) "\\'")
	        nil "root"))
 ;; Do not add "ssh" based methods, otherwise ~/.ssh/config would be ignored.
 ;; Do not add "plink" based methods, they ask interactively for the user.
 (add-to-list 'tramp-default-user-alist
	      `(,(concat
		  "\\`"
		  (regexp-opt
		   '("rcp" "remcp" "rsh" "telnet" "nc" "krlogin" "fcp"))
		  "\\'")
	        nil ,(user-login-name))))

;;;###tramp-autoload
(defconst tramp-completion-function-alist-rsh
  '((tramp-parse-rhosts "/etc/hosts.equiv")
    (tramp-parse-rhosts "~/.rhosts"))
  "Default list of (FUNCTION FILE) pairs to be examined for rsh methods.")

;;;###tramp-autoload
(defconst tramp-completion-function-alist-ssh
  `((tramp-parse-rhosts      "/etc/hosts.equiv")
    (tramp-parse-rhosts      "/etc/shosts.equiv")
    ;; On W32 systems, the ssh directory is located somewhere else.
    (tramp-parse-shosts      ,(expand-file-name
			       "ssh/ssh_known_hosts"
			       (or (and (eq system-type 'windows-nt)
					(getenv "ProgramData"))
				   "/etc/")))
    (tramp-parse-sconfig     ,(expand-file-name
			       "ssh/ssh_config"
			       (or (and (eq system-type 'windows-nt)
					(getenv "ProgramData"))
				   "/etc/")))
    (tramp-parse-shostkeys   "/etc/ssh2/hostkeys")
    (tramp-parse-sknownhosts "/etc/ssh2/knownhosts")
    (tramp-parse-rhosts      "~/.rhosts")
    (tramp-parse-rhosts      "~/.shosts")
    ;; On W32 systems, the .ssh directory is located somewhere else.
    (tramp-parse-shosts      ,(expand-file-name
			       ".ssh/known_hosts"
			       (or (and (eq system-type 'windows-nt)
					(getenv "USERPROFILE"))
				   "~/")))
    (tramp-parse-sconfig     ,(expand-file-name
			       ".ssh/config"
			       (or (and (eq system-type 'windows-nt)
					(getenv "USERPROFILE"))
				   "~/")))
    (tramp-parse-shostkeys   "~/.ssh2/hostkeys")
    (tramp-parse-sknownhosts "~/.ssh2/knownhosts"))
  "Default list of (FUNCTION FILE) pairs to be examined for ssh methods.")

;;;###tramp-autoload
(defconst tramp-completion-function-alist-telnet
  '((tramp-parse-hosts "/etc/hosts"))
  "Default list of (FUNCTION FILE) pairs to be examined for telnet methods.")

;;;###tramp-autoload
(defconst tramp-completion-function-alist-su
  '((tramp-parse-passwd "/etc/passwd"))
  "Default list of (FUNCTION FILE) pairs to be examined for su methods.")

;;;###tramp-autoload
(defconst tramp-completion-function-alist-sg
  '((tramp-parse-etc-group "/etc/group"))
  "Default list of (FUNCTION FILE) pairs to be examined for sg methods.")

;;;###tramp-autoload
(defconst tramp-completion-function-alist-putty
  `((tramp-parse-putty
     ,(if (eq system-type 'windows-nt)
	  "HKEY_CURRENT_USER\\Software\\SimonTatham\\PuTTY\\Sessions"
	"~/.putty/sessions")))
 "Default list of (FUNCTION REGISTRY) pairs to be examined for putty sessions.")

;;;###tramp-autoload
(tramp--with-startup
 (tramp-set-completion-function "rcp" tramp-completion-function-alist-rsh)
 (tramp-set-completion-function "remcp" tramp-completion-function-alist-rsh)
 (tramp-set-completion-function "scp" tramp-completion-function-alist-ssh)
 (tramp-set-completion-function "scpx" tramp-completion-function-alist-ssh)
 (tramp-set-completion-function "rsync" tramp-completion-function-alist-ssh)
 (tramp-set-completion-function "rsh" tramp-completion-function-alist-rsh)
 (tramp-set-completion-function "remsh" tramp-completion-function-alist-rsh)
 (tramp-set-completion-function "ssh" tramp-completion-function-alist-ssh)
 (tramp-set-completion-function "sshx" tramp-completion-function-alist-ssh)
 (tramp-set-completion-function
  "telnet" tramp-completion-function-alist-telnet)
 (tramp-set-completion-function "nc" tramp-completion-function-alist-telnet)
 (tramp-set-completion-function "su" tramp-completion-function-alist-su)
 (tramp-set-completion-function "sudo" tramp-completion-function-alist-su)
 (tramp-set-completion-function "doas" tramp-completion-function-alist-su)
 (tramp-set-completion-function "ksu" tramp-completion-function-alist-su)
 (tramp-set-completion-function "sg" tramp-completion-function-alist-sg)
 (tramp-set-completion-function
  "krlogin" tramp-completion-function-alist-rsh)
 (tramp-set-completion-function "plink" tramp-completion-function-alist-ssh)
 (tramp-set-completion-function
  "plinkx" tramp-completion-function-alist-putty)
 (tramp-set-completion-function "pscp" tramp-completion-function-alist-ssh)
 (tramp-set-completion-function "psftp" tramp-completion-function-alist-ssh)
 (tramp-set-completion-function "fcp" tramp-completion-function-alist-ssh))

(defcustom tramp-sh-extra-args
  '(("/bash\\'" . "-noediting -norc -noprofile")
    ("/zsh\\'" . "-f +Z -V"))
  "Alist specifying extra arguments to pass to the remote shell.
Entries are (REGEXP . ARGS) where REGEXP is a regular expression
matching the shell file name and ARGS is a string specifying the
arguments.  These arguments shall disable line editing, see
`tramp-open-shell'.

This variable is only used when Tramp needs to start up another shell
for tilde expansion.  The extra arguments should typically prevent the
shell from reading its init file."
  :group 'tramp
  :version "27.1"
  :type '(alist :key-type regexp :value-type string))

(defconst tramp-actions-before-shell
  '((tramp-login-prompt-regexp tramp-action-login)
    (tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (shell-prompt-pattern tramp-action-succeed)
    (tramp-shell-prompt-pattern tramp-action-succeed)
    (tramp-yesno-prompt-regexp tramp-action-yesno)
    (tramp-yn-prompt-regexp tramp-action-yn)
    (tramp-terminal-prompt-regexp tramp-action-terminal)
    (tramp-antispoof-regexp tramp-action-confirm-message)
    (tramp-security-key-confirm-regexp tramp-action-show-and-confirm-message)
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
    (tramp-security-key-confirm-regexp tramp-action-show-and-confirm-message)
    (tramp-process-alive-regexp tramp-action-out-of-band))
  "List of pattern/action pairs.
This list is used for copying/renaming with out-of-band methods.

See `tramp-actions-before-shell' for more info.")

(defconst tramp-uudecode
  "(echo begin 600 %t; tail -n +2) | uudecode
cat %t
rm -f %t"
  "Shell function to implement `uudecode' to standard output.
Many systems support `uudecode -o /dev/stdout' or `uudecode -o -'
for this or `uudecode -p', but some systems don't, and for them
we have this shell function.
Format specifiers are replaced by `tramp-expand-script', percent
characters need to be doubled.")

(defconst tramp-perl-file-truename
  "%p -e '
use File::Spec;
use Cwd \"realpath\";

sub myrealpath {
    my ($file) = @_;
    return realpath($file) if (-e $file || -l $file);
}

sub recursive {
    my ($volume, @dirs) = @_;
    my $real = myrealpath(File::Spec->catpath(
                   $volume, File::Spec->catdir(@dirs), \"\"));
    if ($real) {
        my ($vol, $dir) = File::Spec->splitpath($real, 1);
        return ($vol, File::Spec->splitdir($dir));
    }
    else {
        my $last = pop(@dirs);
        ($volume, @dirs) = recursive($volume, @dirs);
        push(@dirs, $last);
        return ($volume, @dirs);
    }
}

$result = myrealpath($ARGV[0]);
if (!$result) {
    my ($vol, $dir) = File::Spec->splitpath($ARGV[0], 1);
    ($vol, @dirs) = recursive($vol, File::Spec->splitdir($dir));

    $result = File::Spec->catpath($vol, File::Spec->catdir(@dirs), \"\");
}

$result =~ s/\"/\\\\\"/g;
print \"\\\"$result\\\"\\n\";
' \"$1\" %n"
  "Perl script to produce output suitable for use with `file-truename'
on the remote file system.
Format specifiers are replaced by `tramp-expand-script', percent
characters need to be doubled.")

(defconst tramp-perl-file-name-all-completions
  "%p -e '
opendir(d, $ARGV[0]) || die(\"$ARGV[0]: $!\\nfail\\n\");
@files = readdir(d); closedir(d);
foreach $f (@files) {
 if (-d \"$ARGV[0]/$f\") {
  print \"$f/\\n\";
 }
 else {
  print \"$f\\n\";
 }
}
print \"ok\\n\"
' \"$1\" %n"
  "Perl script to produce output suitable for use with
`file-name-all-completions' on the remote file system.
Format specifiers are replaced by `tramp-expand-script', percent
characters need to be doubled.")

;; Perl script to implement `file-attributes' in a Lisp `read'able
;; output.  If you are hacking on this, note that you get *no* output
;; unless this spits out a complete line, including the '\n' at the
;; end.
;; The device number is returned as "-1", because there will be a virtual
;; device number set in `tramp-sh-handle-file-attributes'.
(defconst tramp-perl-file-attributes
  "%p -e '
@stat = lstat($ARGV[0]);
if (!@stat) {
    print \"nil\\n\";
    exit 0;
}
if (($stat[2] & 0170000) == 0120000)
{
    $type = readlink($ARGV[0]);
    $type =~ s/\"/\\\\\"/g;
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
    \"(%%s %%u %%s %%s (%%u %%u) (%%u %%u) (%%u %%u) %%u %%u t %%u -1)\\n\",
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
    $stat[1]
);' \"$1\" \"$2\" %n"
  "Perl script to produce output suitable for use with `file-attributes'
on the remote file system.
Format specifiers are replaced by `tramp-expand-script', percent
characters need to be doubled.")

(defconst tramp-perl-directory-files-and-attributes
  "%p -e '
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
        $type =~ s/\"/\\\\\"/g;
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
    $filename =~ s/\"/\\\\\"/g;
    printf(
        \"(\\\"%%s\\\" %%s %%u %%s %%s (%%u %%u) (%%u %%u) (%%u %%u) %%u %%u t %%u -1)\\n\",
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
        $stat[1]);
}
printf(\")\\n\");' \"$1\" \"$2\" %n"
  "Perl script implementing `directory-files-and-attributes' as Lisp `read'able
output.
Format specifiers are replaced by `tramp-expand-script', percent
characters need to be doubled.")

;; These two use base64 encoding.
(defconst tramp-perl-encode-with-module
  "%p -MMIME::Base64 -0777 -ne 'print encode_base64($_)' %n"
  "Perl program to use for encoding a file.
This implementation requires the MIME::Base64 Perl module to be installed
on the remote host.
Format specifiers are replaced by `tramp-expand-script', percent
characters need to be doubled.")

(defconst tramp-perl-decode-with-module
  "%p -MMIME::Base64 -0777 -ne 'print decode_base64($_)' %n"
  "Perl program to use for decoding a file.
This implementation requires the MIME::Base64 Perl module to be installed
on the remote host.
Format specifiers are replaced by `tramp-expand-script', percent
characters need to be doubled.")

(defconst tramp-perl-encode
  "%p -e '
# This script contributed by Juanma Barranquero <lektu@terra.es>.
# Copyright (C) 2002-2022 Free Software Foundation, Inc.
use strict;

my %%trans = do {
    my $i = 0;
    map {(substr(unpack(q(B8), chr $i++), 2, 6), $_)}
      split //, q(ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/);
};
my $data;

# We read in chunks of 54 bytes, to generate output lines
# of 72 chars (plus end of line)
while (read STDIN, $data, 54) {
    my $pad = q();

    # Only for the last chunk, and only if did not fill the last
    # three-byte packet
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
}' %n"
  "Perl program to use for encoding a file.
Format specifiers are replaced by `tramp-expand-script', percent
characters need to be doubled.")

(defconst tramp-perl-decode
  "%p -e '
# This script contributed by Juanma Barranquero <lektu@terra.es>.
# Copyright (C) 2002-2022 Free Software Foundation, Inc.
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
}' %n"
  "Perl program to use for decoding a file.
Format specifiers are replaced by `tramp-expand-script', percent
characters need to be doubled.")

(defconst tramp-perl-pack
  "%p -e 'binmode STDIN; binmode STDOUT; print pack(q{u*}, join q{}, <>)' %n"
  "Perl program to use for encoding a file.
Format specifiers are replaced by `tramp-expand-script', percent
characters need to be doubled.")

(defconst tramp-perl-unpack
  "%p -e 'binmode STDIN; binmode STDOUT; print unpack(q{u*}, join q{}, <>)' %n"
  "Perl program to use for decoding a file.
Format specifiers are replaced by `tramp-expand-script', percent
characters need to be doubled.")

(defconst tramp-hexdump-encode "%h -v -e '16/1 \" %%02x\" \"\\n\"'"
  "`hexdump' program to use for encoding a file.
Format specifiers are replaced by `tramp-expand-script', percent
characters need to be doubled.")

(defconst tramp-awk-encode
  "%a '\\
BEGIN {
  b64 = \"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/\"
  b16 = \"0123456789abcdef\"
}
{
  for (c=1; c<=length($0); c++) {
    d=index(b16, substr($0,c,1))
    if (d--) {
      for (b=1; b<=4; b++) {
        o=o*2+int(d/8); d=(d*2)%%16
        if (++obc==6) {
          printf substr(b64,o+1,1)
          if (++rc>75) { printf \"\\n\"; rc=0 }
          obc=0; o=0
        }
      }
    }
  }
}
END {
  if (obc) {
    tail=(obc==2) ? \"==\\n\" : \"=\\n\"
    while (obc++<6) { o=o*2 }
    printf \"%%c\", substr(b64,o+1,1)
  } else {
    tail=\"\\n\"
  }
  printf tail
}'"
  "`awk' program to use for encoding a file.
Format specifiers are replaced by `tramp-expand-script', percent
characters need to be doubled.")

(defconst tramp-hexdump-awk-encode
  (format "%s | %s" tramp-hexdump-encode tramp-awk-encode)
  "`hexdump' / `awk' pipe to use for encoding a file.
Format specifiers are replaced by `tramp-expand-script', percent
characters need to be doubled.")

(defconst tramp-od-encode "%o -v -t x1 -A n"
  "`od' program to use for encoding a file.
Format specifiers are replaced by `tramp-expand-script', percent
characters need to be doubled.")

(defconst tramp-od-awk-encode (format "%s | %s" tramp-od-encode tramp-awk-encode)
  "`od' / `awk' pipe to use for encoding a file.
Format specifiers are replaced by `tramp-expand-script', percent
characters need to be doubled.")

(defconst tramp-awk-decode
  "%a '\\
BEGIN {
  b64 = \"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/\"
}
{
  for (i=1; i<=length($0); i++) {
    c=index(b64, substr($0,i,1))
    if(c--) {
      for(b=0; b<6; b++) {
        o=o*2+int(c/32); c=(c*2)%%64
        if(++obc==8) {
          if (o) {
            printf \"%%c\", o
          } else {
            system(\"dd if=/dev/zero bs=1 count=1 %n\")
          }
          obc=0; o=0
        }
      }
    }
  }
}'"
  "Awk program to use for decoding a file.
Format specifiers are replaced by `tramp-expand-script', percent
characters need to be doubled.")

(defconst tramp-vc-registered-read-file-names
  "echo \"(\"
while read file; do
    quoted=`echo \"$file\" | sed -e \"s/\\\"/\\\\\\\\\\\\\\\\\\\"/\"`
    if %s \"$file\"; then
	echo \"(\\\"$quoted\\\" \\\"file-exists-p\\\" t)\"
    else
	echo \"(\\\"$quoted\\\" \\\"file-exists-p\\\" nil)\"
    fi
    if %s \"$file\"; then
	echo \"(\\\"$quoted\\\" \\\"file-readable-p\\\" t)\"
    else
	echo \"(\\\"$quoted\\\" \\\"file-readable-p\\\" nil)\"
    fi
done
echo \")\""
  "Script to check existence of VC related files.
It must be send formatted with two strings; the tests for file
existence, and file readability.  Input shall be read via
here-document, otherwise the command could exceed maximum length
of command line.
Format specifiers \"%s\" are replaced before the script is used.")

;; New handlers should be added here.
;;;###tramp-autoload
(defconst tramp-sh-file-name-handler-alist
  '((access-file . tramp-handle-access-file)
    (add-name-to-file . tramp-sh-handle-add-name-to-file)
    ;; `byte-compiler-base-file-name' performed by default handler.
    (copy-directory . tramp-sh-handle-copy-directory)
    (copy-file . tramp-sh-handle-copy-file)
    (delete-directory . tramp-sh-handle-delete-directory)
    (delete-file . tramp-sh-handle-delete-file)
    ;; `diff-latest-backup-file' performed by default handler.
    (directory-file-name . tramp-handle-directory-file-name)
    (directory-files . tramp-handle-directory-files)
    (directory-files-and-attributes
     . tramp-sh-handle-directory-files-and-attributes)
    (dired-compress-file . tramp-sh-handle-dired-compress-file)
    (dired-uncache . tramp-handle-dired-uncache)
    (exec-path . tramp-sh-handle-exec-path)
    (expand-file-name . tramp-sh-handle-expand-file-name)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-acl . tramp-sh-handle-file-acl)
    (file-attributes . tramp-sh-handle-file-attributes)
    (file-directory-p . tramp-sh-handle-file-directory-p)
    (file-equal-p . tramp-handle-file-equal-p)
    (file-executable-p . tramp-sh-handle-file-executable-p)
    (file-exists-p . tramp-sh-handle-file-exists-p)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-local-copy . tramp-sh-handle-file-local-copy)
    (file-locked-p . tramp-handle-file-locked-p)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions . tramp-sh-handle-file-name-all-completions)
    (file-name-as-directory . tramp-handle-file-name-as-directory)
    (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p)
    (file-name-completion . tramp-handle-file-name-completion)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    ;; `file-name-sans-versions' performed by default handler.
    (file-newer-than-file-p . tramp-handle-file-newer-than-file-p)
    (file-notify-add-watch . tramp-sh-handle-file-notify-add-watch)
    (file-notify-rm-watch . tramp-handle-file-notify-rm-watch)
    (file-notify-valid-p . tramp-handle-file-notify-valid-p)
    (file-ownership-preserved-p . tramp-sh-handle-file-ownership-preserved-p)
    (file-readable-p . tramp-sh-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-selinux-context . tramp-sh-handle-file-selinux-context)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-system-info . tramp-sh-handle-file-system-info)
    (file-truename . tramp-sh-handle-file-truename)
    (file-writable-p . tramp-sh-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    ;; `get-file-buffer' performed by default handler.
    (insert-directory . tramp-sh-handle-insert-directory)
    (insert-file-contents . tramp-handle-insert-file-contents)
    (load . tramp-handle-load)
    (lock-file . tramp-handle-lock-file)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-directory . tramp-sh-handle-make-directory)
    ;; `make-directory-internal' performed by default handler.
    (make-lock-file-name . tramp-handle-make-lock-file-name)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-process . tramp-sh-handle-make-process)
    (make-symbolic-link . tramp-sh-handle-make-symbolic-link)
    (process-file . tramp-sh-handle-process-file)
    (rename-file . tramp-sh-handle-rename-file)
    (set-file-acl . tramp-sh-handle-set-file-acl)
    (set-file-modes . tramp-sh-handle-set-file-modes)
    (set-file-selinux-context . tramp-sh-handle-set-file-selinux-context)
    (set-file-times . tramp-sh-handle-set-file-times)
    (set-visited-file-modtime . tramp-sh-handle-set-visited-file-modtime)
    (shell-command . tramp-handle-shell-command)
    (start-file-process . tramp-handle-start-file-process)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (temporary-file-directory . tramp-handle-temporary-file-directory)
    (tramp-get-remote-gid . tramp-sh-handle-get-remote-gid)
    (tramp-get-remote-uid . tramp-sh-handle-get-remote-uid)
    (tramp-set-file-uid-gid . tramp-sh-handle-set-file-uid-gid)
    (unhandled-file-name-directory . ignore)
    (unlock-file . tramp-handle-unlock-file)
    (vc-registered . tramp-sh-handle-vc-registered)
    (verify-visited-file-modtime . tramp-sh-handle-verify-visited-file-modtime)
    (write-region . tramp-sh-handle-write-region))
  "Alist of handler functions.
Operations not mentioned here will be handled by the normal Emacs functions.")

;;; File Name Handler Functions:

(defun tramp-sh-handle-make-symbolic-link
    (target linkname &optional ok-if-already-exists)
  "Like `make-symbolic-link' for Tramp files.
If TARGET is a non-Tramp file, it is used verbatim as the target
of the symlink.  If TARGET is a Tramp file, only the localname
component is used as the target of the symlink."
  (if (not (tramp-tramp-file-p (expand-file-name linkname)))
      (tramp-run-real-handler
       #'make-symbolic-link (list target linkname ok-if-already-exists))

    (with-parsed-tramp-file-name linkname nil
      ;; If TARGET is a Tramp name, use just the localname component.
      ;; Don't check for a proper method.
      (let ((non-essential t))
	(when (and (tramp-tramp-file-p target)
		   (tramp-file-name-equal-p v (tramp-dissect-file-name target)))
	  (setq target (tramp-file-local-name (expand-file-name target)))))

      ;; If TARGET is still remote, quote it.
      (if (tramp-tramp-file-p target)
	  (make-symbolic-link (tramp-compat-file-name-quote target 'top)
	   linkname ok-if-already-exists)

	(let ((ln (tramp-get-remote-ln v))
	      (cwd (tramp-run-real-handler
		    #'file-name-directory (list localname))))
	  (unless ln
	    (tramp-error
	     v 'file-error
	     (concat "Making a symbolic link. "
		     "ln(1) does not exist on the remote host.")))

	  ;; Do the 'confirm if exists' thing.
	  (when (file-exists-p linkname)
	    ;; What to do?
	    (if (or (null ok-if-already-exists) ; not allowed to exist
		    (and (numberp ok-if-already-exists)
			 (not
			  (yes-or-no-p
			   (format
			    "File %s already exists; make it a link anyway?"
			    localname)))))
		(tramp-error v 'file-already-exists localname)
	      (delete-file linkname)))

	  (tramp-flush-file-properties v localname)

	  ;; Right, they are on the same host, regardless of user,
	  ;; method, etc.  We now make the link on the remote
	  ;; machine.  This will occur as the user that TARGET belongs to.
	  (and (tramp-send-command-and-check
		v (format "cd %s" (tramp-shell-quote-argument cwd)))
               (tramp-send-command-and-check
		v (format
		   "%s -sf %s %s" ln
		   (tramp-shell-quote-argument target)
		   ;; The command could exceed PATH_MAX, so we use
		   ;; relative file names.  However, relative file
		   ;; names could start with "-".
		   ;; `tramp-shell-quote-argument' does not handle
		   ;; this, we must do it ourselves.
		   (tramp-shell-quote-argument
                    (concat "./" (file-name-nondirectory localname)))))))))))

(defun tramp-sh-handle-file-truename (filename)
  "Like `file-truename' for Tramp files."
  ;; Preserve trailing "/".
  (funcall
   (if (directory-name-p filename) #'file-name-as-directory #'identity)
   ;; Quote properly.
   (funcall
    (if (tramp-compat-file-name-quoted-p filename)
	#'tramp-compat-file-name-quote #'identity)
    (with-parsed-tramp-file-name
	(tramp-compat-file-name-unquote (expand-file-name filename)) nil
      (tramp-make-tramp-file-name
       v
       (with-tramp-file-property v localname "file-truename"
	 (let (result)			; result steps in reverse order
	   (tramp-message v 4 "Finding true name for `%s'" filename)
	   (cond
	    ;; Use GNU readlink --canonicalize-missing where available.
	    ((tramp-get-remote-readlink v)
	     (tramp-send-command-and-check
	      v
	      (format "%s --canonicalize-missing %s"
		      (tramp-get-remote-readlink v)
		      (tramp-shell-quote-argument localname)))
	     (with-current-buffer (tramp-get-connection-buffer v)
	       (goto-char (point-min))
	       (setq result (buffer-substring (point-min) (point-at-eol)))))

	    ;; Use Perl implementation.
	    ((and (tramp-get-remote-perl v)
		  (tramp-get-connection-property v "perl-file-spec" nil)
		  (tramp-get-connection-property v "perl-cwd-realpath" nil))
	     (tramp-maybe-send-script
	      v tramp-perl-file-truename "tramp_perl_file_truename")
	     (setq result
		   (tramp-send-command-and-read
		    v
		    (format "tramp_perl_file_truename %s"
			    (tramp-shell-quote-argument localname)))))

	    ;; Do it yourself.
	    (t (setq
		result
		(tramp-file-local-name (tramp-handle-file-truename filename)))))

	   ;; Detect cycle.
	   (when (and (file-symlink-p filename)
		      (string-equal result localname))
	     (tramp-error
	      v 'file-error
	      "Apparent cycle of symbolic links for %s" filename))
	   ;; If the resulting localname looks remote, we must quote it
	   ;; for security reasons.
	   (when (file-remote-p result)
	     (setq result (tramp-compat-file-name-quote result 'top)))
	   (tramp-message v 4 "True name of `%s' is `%s'" localname result)
	   result))
       'nohop)))))

;; Basic functions.

(defun tramp-sh-handle-file-exists-p (filename)
  "Like `file-exists-p' for Tramp files."
  ;; `file-exists-p' is used as predicate in file name completion.
  ;; We don't want to run it when `non-essential' is t, or there is
  ;; no connection process yet.
  (when (tramp-connectable-p filename)
    (with-parsed-tramp-file-name filename nil
      (with-tramp-file-property v localname "file-exists-p"
	(or (not (null (tramp-get-file-property
			v localname "file-attributes-integer" nil)))
            (not (null (tramp-get-file-property
			v localname "file-attributes-string" nil)))
	    (tramp-send-command-and-check
	     v
	     (format
	      "%s %s"
	      (tramp-get-file-exists-command v)
	      (tramp-shell-quote-argument localname))))))))

(defun tramp-sh-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for Tramp files."
  (unless id-format (setq id-format 'integer))
  (ignore-errors
    ;; Don't modify `last-coding-system-used' by accident.
    (let ((last-coding-system-used last-coding-system-used))
      (with-parsed-tramp-file-name (expand-file-name filename) nil
	(with-tramp-file-property
	    v localname (format "file-attributes-%s" id-format)
	  (tramp-convert-file-attributes
	   v
	   (or
	    (cond
	     ((tramp-get-remote-stat v)
	      (tramp-do-file-attributes-with-stat v localname id-format))
	     ((tramp-get-remote-perl v)
	      (tramp-do-file-attributes-with-perl v localname id-format))
	     (t nil))
	    ;; The scripts could fail, for example with huge file size.
	    (tramp-do-file-attributes-with-ls v localname id-format))))))))

(defun tramp-sh--quoting-style-options (vec)
  "Quoting style options to be used for VEC."
  (or
   (tramp-get-ls-command-with
    vec "--quoting-style=literal --show-control-chars")
   (tramp-get-ls-command-with vec "-w")
   ""))

(defun tramp-do-file-attributes-with-ls (vec localname &optional id-format)
  "Implement `file-attributes' for Tramp files using the ls(1) command."
  (let (symlinkp dirp
	res-inode res-filemodes res-numlinks
	res-uid res-gid res-size res-symlink-target)
    (tramp-message vec 5 "file attributes with ls: %s" localname)
    ;; We cannot send all three commands combined, it could exceed
    ;; NAME_MAX or PATH_MAX.  Happened on macOS, for example.
    (when (or (tramp-send-command-and-check
               vec
               (format "%s %s"
                       (tramp-get-file-exists-command vec)
                       (tramp-shell-quote-argument localname)))
              (tramp-send-command-and-check
               vec
               (format "%s -h %s"
                       (tramp-get-test-command vec)
                       (tramp-shell-quote-argument localname))))
      (tramp-send-command
       vec
       (format "%s %s %s %s"
               (tramp-get-ls-command vec)
               (if (eq id-format 'integer) "-ildn" "-ild")
               ;; On systems which have no quoting style, file names
               ;; with special characters could fail.
               (tramp-sh--quoting-style-options vec)
               (tramp-shell-quote-argument localname)))
      ;; Parse `ls -l' output ...
      (with-current-buffer (tramp-get-buffer vec)
        (when (> (buffer-size) 0)
          (goto-char (point-min))
          ;; ... inode
          (setq res-inode (read (current-buffer)))
          ;; ... file mode flags
          (setq res-filemodes (symbol-name (read (current-buffer))))
          ;; ... number links
          (setq res-numlinks (read (current-buffer)))
          ;; ... uid and gid
          (setq res-uid (read (current-buffer)))
          (setq res-gid (read (current-buffer)))
          (if (eq id-format 'integer)
              (progn
                (unless (numberp res-uid)
		  (setq res-uid tramp-unknown-id-integer))
                (unless (numberp res-gid)
		  (setq res-gid tramp-unknown-id-integer)))
            (progn
              (unless (stringp res-uid) (setq res-uid (symbol-name res-uid)))
              (unless (stringp res-gid) (setq res-gid (symbol-name res-gid)))))
          ;; ... size
          (setq res-size (read (current-buffer)))
          ;; From the file modes, figure out other stuff.
          (setq symlinkp (eq ?l (aref res-filemodes 0)))
          (setq dirp (eq ?d (aref res-filemodes 0)))
          ;; If symlink, find out file name pointed to.
          (when symlinkp
            (search-forward "-> ")
            (setq res-symlink-target
                  (if (looking-at-p "\"")
                      (read (current-buffer))
                    (buffer-substring (point) (point-at-eol)))))
          ;; Return data gathered.
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
           ;; 4. Last access time.
           ;; 5. Last modification time.
           ;; 6. Last status change time.
           tramp-time-dont-know tramp-time-dont-know tramp-time-dont-know
           ;; 7. Size in bytes (-1, if number is out of range).
           res-size
           ;; 8. File modes, as a string of ten letters or dashes as in ls -l.
           res-filemodes
           ;; 9. t if file's gid would change if file were deleted and
           ;; recreated.  Will be set in `tramp-convert-file-attributes'.
           t
           ;; 10. Inode number.
           res-inode
           ;; 11. Device number.  Will be replaced by a virtual device number.
           -1))))))

(defun tramp-do-file-attributes-with-perl
  (vec localname &optional id-format)
  "Implement `file-attributes' for Tramp files using a Perl script."
  (tramp-message vec 5 "file attributes with perl: %s" localname)
  (tramp-maybe-send-script
   vec tramp-perl-file-attributes "tramp_perl_file_attributes")
  (tramp-send-command-and-read
   vec
   (format "tramp_perl_file_attributes %s %s"
	   (tramp-shell-quote-argument localname) id-format)))

(defun tramp-do-file-attributes-with-stat
  (vec localname &optional id-format)
  "Implement `file-attributes' for Tramp files using stat(1) command."
  (tramp-message vec 5 "file attributes with stat: %s" localname)
  (tramp-send-command-and-read
   vec
   (format
    (concat
     ;; Apostrophes in the stat output are masked as
     ;; `tramp-stat-marker', in order to make a proper shell escape of
     ;; them in file names.
     "(%s -c '((%s%%N%s) %%h %s %s %%X %%Y %%Z %%s %s%%A%s t %%i -1)' %s |"
     " sed -e 's/\"/\\\\\"/g' -e 's/%s/\"/g')")
    (tramp-get-remote-stat vec)
    tramp-stat-marker tramp-stat-marker
    (if (eq id-format 'integer)
	"%u"
      (eval-when-compile (concat tramp-stat-marker "%U" tramp-stat-marker)))
    (if (eq id-format 'integer)
	"%g"
      (eval-when-compile (concat tramp-stat-marker "%G" tramp-stat-marker)))
    tramp-stat-marker tramp-stat-marker
    (tramp-shell-quote-argument localname)
    tramp-stat-quoted-marker)
   'noerror))

(defun tramp-sh-handle-set-visited-file-modtime (&optional time-list)
  "Like `set-visited-file-modtime' for Tramp files."
  (unless (buffer-file-name)
    (error "Can't set-visited-file-modtime: buffer `%s' not visiting a file"
	   (buffer-name)))
  (if time-list
      (tramp-run-real-handler #'set-visited-file-modtime (list time-list))
    (let ((f (buffer-file-name))
	  coding-system-used)
      (with-parsed-tramp-file-name f nil
	(let* ((remote-file-name-inhibit-cache t)
	       (attr (file-attributes f))
	       (modtime (or (tramp-compat-file-attribute-modification-time attr)
			    tramp-time-doesnt-exist)))
	  (setq coding-system-used last-coding-system-used)
	  (if (not (tramp-compat-time-equal-p modtime tramp-time-dont-know))
	      (tramp-run-real-handler #'set-visited-file-modtime (list modtime))
	    (progn
	      (tramp-send-command
	       v
	       (format "%s -ild %s"
		       (tramp-get-ls-command v)
		       (tramp-shell-quote-argument localname)))
	      (setq attr (buffer-substring (point) (point-at-eol))))
	    (tramp-set-file-property
	     v localname "visited-file-modtime-ild" attr))
	  (setq last-coding-system-used coding-system-used)
	  nil)))))

;; This function makes the same assumption as
;; `tramp-sh-handle-set-visited-file-modtime'.
(defun tramp-sh-handle-verify-visited-file-modtime (&optional buf)
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
	      (zerop (float-time (visited-file-modtime)))
	      (not (file-remote-p f nil 'connected)))
	  t
	(with-parsed-tramp-file-name f nil
	  (let* ((remote-file-name-inhibit-cache t)
		 (attr (file-attributes f))
		 (modtime (tramp-compat-file-attribute-modification-time attr))
		 (mt (visited-file-modtime)))

	    (cond
	     ;; File exists, and has a known modtime.
	     ((and attr
		   (not
		    (tramp-compat-time-equal-p modtime tramp-time-dont-know)))
	      (< (abs (tramp-time-diff modtime mt)) 2))
	     ;; Modtime has the don't know value.
	     (attr
	      (tramp-send-command
	       v
	       (format "%s -ild %s"
		       (tramp-get-ls-command v)
		       (tramp-shell-quote-argument localname)))
	      (with-current-buffer (tramp-get-buffer v)
		(setq attr (buffer-substring (point) (point-at-eol))))
	      (equal
	       attr
	       (tramp-get-file-property
		v localname "visited-file-modtime-ild" "")))
	     ;; If file does not exist, say it is not modified if and
	     ;; only if that agrees with the buffer's record.
	     (t (tramp-compat-time-equal-p mt tramp-time-doesnt-exist)))))))))

(defun tramp-sh-handle-set-file-modes (filename mode &optional flag)
  "Like `set-file-modes' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    ;; We need "chmod -h" when the flag is set.
    (when (or (not (eq flag 'nofollow))
	      (not (file-symlink-p filename))
	      (tramp-get-remote-chmod-h v))
      (tramp-flush-file-properties v localname)
      ;; FIXME: extract the proper text from chmod's stderr.
      (tramp-barf-unless-okay
       v
       (format
	"chmod %s %o %s"
	(if (and (eq flag 'nofollow) (tramp-get-remote-chmod-h v)) "-h" "")
	mode (tramp-shell-quote-argument localname))
       "Error while changing file's mode %s" filename))))

(defun tramp-sh-handle-set-file-times (filename &optional time flag)
  "Like `set-file-times' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (when (tramp-get-remote-touch v)
      (tramp-flush-file-properties v localname)
      (let ((time
	     (if (or (null time)
		     (tramp-compat-time-equal-p time tramp-time-doesnt-exist)
		     (tramp-compat-time-equal-p time tramp-time-dont-know))
		 (current-time)
	       time)))
	(tramp-send-command-and-check
	 v (format
	    "env TZ=UTC %s %s %s %s"
	    (tramp-get-remote-touch v)
	    (if (tramp-get-connection-property v "touch-t" nil)
		(format "-t %s" (format-time-string "%Y%m%d%H%M.%S" time t))
	      "")
	    (if (eq flag 'nofollow) "-h" "")
	    (tramp-shell-quote-argument localname)))))))

(defun tramp-sh-handle-get-remote-uid (vec id-format)
  "The uid of the remote connection VEC, in ID-FORMAT.
ID-FORMAT valid values are `string' and `integer'."
  (ignore-errors
    (cond
     ((tramp-get-remote-id vec) (tramp-get-remote-uid-with-id vec id-format))
     ((tramp-get-remote-perl vec) (tramp-get-remote-uid-with-perl vec id-format))
     ((tramp-get-remote-python vec)
      (tramp-get-remote-uid-with-python vec id-format)))))

(defun tramp-sh-handle-get-remote-gid (vec id-format)
  "The gid of the remote connection VEC, in ID-FORMAT.
ID-FORMAT valid values are `string' and `integer'."
  (ignore-errors
    (cond
     ((tramp-get-remote-id vec) (tramp-get-remote-gid-with-id vec id-format))
     ((tramp-get-remote-perl vec) (tramp-get-remote-gid-with-perl vec id-format))
     ((tramp-get-remote-python vec)
      (tramp-get-remote-gid-with-python vec id-format)))))

(defun tramp-sh-handle-set-file-uid-gid (filename &optional uid gid)
  "Like `tramp-set-file-uid-gid' for Tramp files."
  ;; Modern Unices allow chown only for root.  So we might need
  ;; another implementation, see `dired-do-chown'.  OTOH, it is mostly
  ;; working with su(do)? when it is needed, so it shall succeed in
  ;; the majority of cases.
  ;; Don't modify `last-coding-system-used' by accident.
  (let ((last-coding-system-used last-coding-system-used))
    (with-parsed-tramp-file-name filename nil
      (if (and (zerop (user-uid)) (tramp-local-host-p v))
	  ;; If we are root on the local host, we can do it directly.
	  (tramp-set-file-uid-gid localname uid gid)
	(let ((uid (or (and (natnump uid) uid)
		       (tramp-get-remote-uid v 'integer)))
	      (gid (or (and (natnump gid) gid)
		       (tramp-get-remote-gid v 'integer))))
	  (tramp-send-command
	   v (format
	      "chown %d:%d %s" uid gid
	      (tramp-shell-quote-argument localname))))))))

(defun tramp-remote-selinux-p (vec)
  "Check, whether SELINUX is enabled on the remote host."
  (with-tramp-connection-property (tramp-get-process vec) "selinux-p"
    (tramp-send-command-and-check vec "selinuxenabled")))

(defun tramp-sh-handle-file-selinux-context (filename)
  "Like `file-selinux-context' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-selinux-context"
      (let ((context '(nil nil nil nil))
	    (regexp (concat "\\([[:alnum:]_]+\\):" "\\([[:alnum:]_]+\\):"
			    "\\([[:alnum:]_]+\\):" "\\([[:alnum:]_]+\\)")))
	(when (and (tramp-remote-selinux-p v)
		   (tramp-send-command-and-check
		    v (format
		       "%s -d -Z %s"
		       (tramp-get-ls-command v)
		       (tramp-shell-quote-argument localname))))
	  (with-current-buffer (tramp-get-connection-buffer v)
	    (goto-char (point-min))
	    (when (re-search-forward regexp (point-at-eol) t)
	      (setq context (list (match-string 1) (match-string 2)
				  (match-string 3) (match-string 4))))))
	;; Return the context.
	context))))

(defun tramp-sh-handle-set-file-selinux-context (filename context)
  "Like `set-file-selinux-context' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (when (and (consp context)
	       (tramp-remote-selinux-p v))
      (let ((user (and (stringp (nth 0 context)) (nth 0 context)))
	    (role (and (stringp (nth 1 context)) (nth 1 context)))
	    (type (and (stringp (nth 2 context)) (nth 2 context)))
	    (range (and (stringp (nth 3 context)) (nth 3 context))))
	(when (tramp-send-command-and-check
	       v (format "chcon %s %s %s %s %s"
			 (if user (format "--user=%s" user) "")
			 (if role (format "--role=%s" role) "")
			 (if type (format "--type=%s" type) "")
			 (if range (format "--range=%s" range) "")
		       (tramp-shell-quote-argument localname)))
	  (if (and user role type range)
	      (tramp-set-file-property
	       v localname "file-selinux-context" context)
	    (tramp-flush-file-property v localname "file-selinux-context"))
	  t)))))

(defun tramp-remote-acl-p (vec)
  "Check, whether ACL is enabled on the remote host."
  (with-tramp-connection-property (tramp-get-process vec) "acl-p"
    (tramp-send-command-and-check vec "getfacl /")))

(defun tramp-sh-handle-file-acl (filename)
  "Like `file-acl' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-acl"
      (when (and (tramp-remote-acl-p v)
		 (tramp-send-command-and-check
		  v (format
		     "getfacl -ac %s"
		     (tramp-shell-quote-argument localname))))
	(with-current-buffer (tramp-get-connection-buffer v)
	  (goto-char (point-max))
	  (delete-blank-lines)
	  (when (> (point-max) (point-min))
	    (substring-no-properties (buffer-string))))))))

(defun tramp-sh-handle-set-file-acl (filename acl-string)
  "Like `set-file-acl' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (if (and (stringp acl-string) (tramp-remote-acl-p v)
	     (progn
	       (tramp-send-command
		v (format "setfacl --set-file=- %s <<'%s'\n%s\n%s\n"
			  (tramp-shell-quote-argument localname)
			  tramp-end-of-heredoc
			  acl-string
			  tramp-end-of-heredoc))
	       (tramp-send-command-and-check v nil)))
	;; Success.
	(progn
	  (tramp-set-file-property v localname "file-acl" acl-string)
	  t)
      ;; In case of errors, we return nil.
      (tramp-flush-file-property v localname "file-acl-string")
      nil)))

;; Simple functions using the `test' command.

(defun tramp-sh-handle-file-executable-p (filename)
  "Like `file-executable-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-executable-p"
      ;; Examine `file-attributes' cache to see if request can be
      ;; satisfied without remote operation.
      (or (tramp-check-cached-permissions v ?x)
	  (tramp-check-cached-permissions v ?s)
	  (tramp-run-test "-x" filename)))))

(defun tramp-sh-handle-file-readable-p (filename)
  "Like `file-readable-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-readable-p"
      (or (tramp-handle-file-readable-p filename)
	  (tramp-run-test "-r" filename)))))

;; Functions implemented using the basic functions above.

(defun tramp-sh-handle-file-directory-p (filename)
  "Like `file-directory-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    ;; `file-directory-p' is used as predicate for file name completion.
    ;; Sometimes, when a connection is not established yet, it is
    ;; desirable to return t immediately for "/method:foo:".  It can
    ;; be expected that this is always a directory.
    (or (zerop (length localname))
	(with-tramp-file-property v localname "file-directory-p"
	  (tramp-run-test "-d" filename)))))

(defun tramp-sh-handle-file-writable-p (filename)
  "Like `file-writable-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-writable-p"
      (if (file-exists-p filename)
	  ;; Examine `file-attributes' cache to see if request can be
	  ;; satisfied without remote operation.
          (or (tramp-check-cached-permissions v ?w)
	      (tramp-run-test "-w" filename))
	;; If file doesn't exist, check if directory is writable.
	(and (tramp-run-test "-d" (file-name-directory filename))
	     (tramp-run-test "-w" (file-name-directory filename)))))))

(defun tramp-sh-handle-file-ownership-preserved-p (filename &optional group)
  "Like `file-ownership-preserved-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property
	v localname
	(format "file-ownership-preserved-p%s" (if group "-group" ""))
      (let ((attributes (file-attributes filename 'integer)))
	;; Return t if the file doesn't exist, since it's true that no
	;; information would be lost by an (attempted) delete and create.
	(or (null attributes)
	    (and
	     (= (tramp-compat-file-attribute-user-id attributes)
		(tramp-get-remote-uid v 'integer))
	     (or (not group)
		 ;; On BSD-derived systems files always inherit the
                 ;; parent directory's group, so skip the group-gid
                 ;; test.
                 (tramp-check-remote-uname v "BSD\\|DragonFly\\|Darwin")
		 (= (tramp-compat-file-attribute-group-id attributes)
		    (tramp-get-remote-gid v 'integer)))))))))

;; Directory listings.

(defun tramp-sh-handle-directory-files-and-attributes
  (directory &optional full match nosort id-format count)
  "Like `directory-files-and-attributes' for Tramp files."
  (unless id-format (setq id-format 'integer))
  (unless (file-exists-p directory)
    (tramp-compat-file-missing
     (tramp-dissect-file-name directory) directory))
  (when (file-directory-p directory)
    (setq directory (expand-file-name directory))
    (let* ((temp
	    (copy-tree
	     (with-parsed-tramp-file-name directory nil
	       (with-tramp-file-property
		   v localname
		   (format "directory-files-and-attributes-%s" id-format)
		 (mapcar
		  (lambda (x)
		    (cons (car x) (tramp-convert-file-attributes v (cdr x))))
		  (cond
		   ((tramp-get-remote-stat v)
		    (tramp-do-directory-files-and-attributes-with-stat
		     v localname id-format))
		   ((tramp-get-remote-perl v)
		    (tramp-do-directory-files-and-attributes-with-perl
		     v localname id-format))
		   (t nil)))))))
	   result item)

      (while temp
	(setq item (pop temp))
	(when (or (null match) (string-match-p match (car item)))
	  (when full
	    (setcar item (expand-file-name (car item) directory)))
	  (push item result)))

      (unless nosort
	(setq result (sort result (lambda (x y) (string< (car x) (car y))))))

      (when (and (natnump count) (> count 0))
	(setq result (nbutlast result (- (length result) count))))

      (or result
	  ;; The scripts could fail, for example with huge file size.
	  (tramp-handle-directory-files-and-attributes
	   directory full match nosort id-format count)))))

;; FIXME: Fix function to work with count parameter.
(defun tramp-do-directory-files-and-attributes-with-perl
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

;; FIXME: Fix function to work with count parameter.
(defun tramp-do-directory-files-and-attributes-with-stat
  (vec localname &optional id-format)
  "Implement `directory-files-and-attributes' for Tramp files with stat(1) command."
  (tramp-message vec 5 "directory-files-and-attributes with stat: %s" localname)
  (tramp-send-command-and-read
   vec
   (format
    (concat
     ;; We must care about file names with spaces, or starting with
     ;; "-"; this would confuse xargs.  "ls -aQ" might be a solution,
     ;; but it does not work on all remote systems.  Therefore, we use
     ;; \000 as file separator.  `tramp-sh--quoting-style-options' do
     ;; not work for file names with spaces piped to "xargs".
     ;; Apostrophes in the stat output are masked as
     ;; `tramp-stat-marker', in order to make a proper shell escape of
     ;; them in file names.
     "cd %s && echo \"(\"; (%s %s -a | tr '\\n\\r' '\\000\\000' | "
     "xargs -0 %s -c "
     "'(%s%%n%s (%s%%N%s) %%h %s %s %%X %%Y %%Z %%s %s%%A%s t %%i -1)' "
     "-- 2>%s | sed -e 's/\"/\\\\\"/g' -e 's/%s/\"/g'); echo \")\"")
    (tramp-shell-quote-argument localname)
    (tramp-get-ls-command vec)
    ;; On systems which have no quoting style, file names with special
    ;; characters could fail.
    (tramp-sh--quoting-style-options vec)
    (tramp-get-remote-stat vec)
    tramp-stat-marker tramp-stat-marker
    tramp-stat-marker tramp-stat-marker
    (if (eq id-format 'integer)
	"%u"
      (eval-when-compile (concat tramp-stat-marker "%U" tramp-stat-marker)))
    (if (eq id-format 'integer)
	"%g"
      (eval-when-compile (concat tramp-stat-marker "%G" tramp-stat-marker)))
    tramp-stat-marker tramp-stat-marker
    (tramp-get-remote-null-device vec)
    tramp-stat-quoted-marker)))

;; This function should return "foo/" for directories and "bar" for
;; files.
(defun tramp-sh-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for Tramp files."
  (unless (tramp-compat-string-search "/" filename)
    (all-completions
     filename
     (with-parsed-tramp-file-name (expand-file-name directory) nil
       (with-tramp-file-property v localname "file-name-all-completions"
	 (let (result)
	   ;; Get a list of directories and files, including reliably
	   ;; tagging the directories with a trailing "/".  Because I
	   ;; rock.  --daniel@danann.net
	   (tramp-send-command
	    v
	    (if (tramp-get-remote-perl v)
		(progn
		  (tramp-maybe-send-script
		   v tramp-perl-file-name-all-completions
		   "tramp_perl_file_name_all_completions")
		  (format "tramp_perl_file_name_all_completions %s"
			  (tramp-shell-quote-argument localname)))

	      (format (concat
		       "(cd %s 2>&1 && %s -a 2>%s"
		       " | while IFS= read f; do"
		       " if %s -d \"$f\" 2>%s;"
		       " then \\echo \"$f/\"; else \\echo \"$f\"; fi; done"
		       " && \\echo ok) || \\echo fail")
		      (tramp-shell-quote-argument localname)
		      (tramp-get-ls-command v)
                      (tramp-get-remote-null-device v)
		      (tramp-get-test-command v)
                      (tramp-get-remote-null-device v))))

	   ;; Now grab the output.
	   (with-current-buffer (tramp-get-buffer v)
	     (goto-char (point-max))

	     ;; Check result code, found in last line of output.
	     (forward-line -1)
	     (if (looking-at-p "^fail$")
		 (progn
		   ;; Grab error message from line before last line
		   ;; (it was put there by `cd 2>&1').
		   (forward-line -1)
		   (tramp-error
		    v 'file-error
		    "tramp-sh-handle-file-name-all-completions: %s"
		    (buffer-substring (point) (point-at-eol))))
	       ;; For peace of mind, if buffer doesn't end in `fail'
	       ;; then it should end in `ok'.  If neither are in the
	       ;; buffer something went seriously wrong on the remote
	       ;; side.
	       (unless (looking-at-p "^ok$")
		 (tramp-error
		  v 'file-error
		  (concat "tramp-sh-handle-file-name-all-completions: "
			  "internal error accessing `%s': `%s'")
		  (tramp-shell-quote-argument localname) (buffer-string))))

	     (while (zerop (forward-line -1))
	       (push (buffer-substring (point) (point-at-eol)) result)))
	   result))))))

;; cp, mv and ln

(defun tramp-sh-handle-add-name-to-file
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

	;; Do the 'confirm if exists' thing.
	(when (file-exists-p newname)
	  ;; What to do?
	  (if (or (null ok-if-already-exists) ; not allowed to exist
		  (and (numberp ok-if-already-exists)
		       (not (yes-or-no-p
			     (format
			      "File %s already exists; make it a link anyway?"
			      v2-localname)))))
	      (tramp-error v2 'file-already-exists newname)
	    (delete-file newname)))
	(tramp-flush-file-properties v2 v2-localname)
	(tramp-barf-unless-okay
	 v1
	 (format "%s %s %s" ln
		 (tramp-shell-quote-argument v1-localname)
		 (tramp-shell-quote-argument v2-localname))
	 "error with add-name-to-file, see buffer `%s' for details"
	 (buffer-name))))))

(defun tramp-sh-handle-copy-file
  (filename newname &optional ok-if-already-exists keep-date
   preserve-uid-gid preserve-extended-attributes)
  "Like `copy-file' for Tramp files."
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))
  (if (or (tramp-tramp-file-p filename)
	  (tramp-tramp-file-p newname))
      (tramp-do-copy-or-rename-file
       'copy filename newname ok-if-already-exists keep-date
       preserve-uid-gid preserve-extended-attributes)
    (tramp-run-real-handler
     #'copy-file
     (list filename newname ok-if-already-exists keep-date
	   preserve-uid-gid preserve-extended-attributes))))

(defun tramp-sh-handle-copy-directory
  (dirname newname &optional keep-date parents copy-contents)
  "Like `copy-directory' for Tramp files."
  (let ((t1 (tramp-tramp-file-p dirname))
	(t2 (tramp-tramp-file-p newname))
	target)
    (with-parsed-tramp-file-name (if t1 dirname newname) nil
      (unless (file-exists-p dirname)
	(tramp-compat-file-missing v dirname))

      ;; `copy-directory-create-symlink' exists since Emacs 28.1.
      (if (and (bound-and-true-p copy-directory-create-symlink)
	       (setq target (file-symlink-p dirname))
	       (tramp-equal-remote dirname newname))
	  (make-symbolic-link
	   target
	   (if (directory-name-p newname)
	       (concat newname (file-name-nondirectory dirname)) newname)
	   t)

	(if (and (not copy-contents)
		 (tramp-get-method-parameter v 'tramp-copy-recursive)
		 ;; When DIRNAME and NEWNAME are remote, they must
		 ;; have the same method.
		 (or (null t1) (null t2)
		     (string-equal
		      (tramp-file-name-method (tramp-dissect-file-name dirname))
		      (tramp-file-name-method
		       (tramp-dissect-file-name newname)))))
	    ;; scp or rsync DTRT.
	    (progn
	      (when (and (file-directory-p newname)
			 (not (directory-name-p newname)))
		(tramp-error v 'file-already-exists newname))
	      (setq dirname (directory-file-name (expand-file-name dirname))
		    newname (directory-file-name (expand-file-name newname)))
	      (when (and (file-directory-p newname)
			 (not (string-equal (file-name-nondirectory dirname)
					    (file-name-nondirectory newname))))
		(setq newname
		      (expand-file-name
		       (file-name-nondirectory dirname) newname)))
	      (unless (file-directory-p (file-name-directory newname))
		(make-directory (file-name-directory newname) parents))
	      (tramp-do-copy-or-rename-file-out-of-band
	       'copy dirname newname 'ok-if-already-exists keep-date))

	  ;; We must do it file-wise.
	  (tramp-run-real-handler
	   #'copy-directory
	   (list dirname newname keep-date parents copy-contents))))

      ;; When newname did exist, we have wrong cached values.
      (when t2
	(with-parsed-tramp-file-name newname nil
	  (tramp-flush-file-properties v localname))))))

(defun tramp-sh-handle-rename-file
  (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for Tramp files."
  ;; Check if both files are local -- invoke normal rename-file.
  ;; Otherwise, use Tramp from local system.
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))
  ;; At least one file a Tramp file?
  (if (or (tramp-tramp-file-p filename)
          (tramp-tramp-file-p newname))
      (tramp-do-copy-or-rename-file
       'rename filename newname ok-if-already-exists
       'keep-time 'preserve-uid-gid)
    (tramp-run-real-handler
     #'rename-file (list filename newname ok-if-already-exists))))

(defun tramp-do-copy-or-rename-file
  (op filename newname &optional ok-if-already-exists keep-date
   preserve-uid-gid preserve-extended-attributes)
  "Copy or rename a remote file.
OP must be `copy' or `rename' and indicates the operation to perform.
FILENAME specifies the file to copy or rename, NEWNAME is the name of
the new file (for copy) or the new name of the file (for rename).
OK-IF-ALREADY-EXISTS means don't barf if NEWNAME exists already.
KEEP-DATE means to make sure that NEWNAME has the same timestamp
as FILENAME.  PRESERVE-UID-GID, when non-nil, instructs to keep
the uid and gid if both files are on the same host.
PRESERVE-EXTENDED-ATTRIBUTES activates selinux and acl commands.

This function is invoked by `tramp-sh-handle-copy-file' and
`tramp-sh-handle-rename-file'.  It is an error if OP is neither
of `copy' and `rename'.  FILENAME and NEWNAME must be absolute
file names."
  (unless (memq op '(copy rename))
    (error "Unknown operation `%s', must be `copy' or `rename'" op))

  (if (and
       (file-directory-p filename)
       (not (tramp-equal-remote filename newname)))
      (progn
	(copy-directory filename newname keep-date t)
	(when (eq op 'rename) (delete-directory filename 'recursive)))

    (let ((t1 (tramp-tramp-file-p filename))
	  (t2 (tramp-tramp-file-p newname))
	  (length (tramp-compat-file-attribute-size
		   (file-attributes (file-truename filename))))
	  (attributes (and preserve-extended-attributes
			   (file-extended-attributes filename)))
	  (msg-operation (if (eq op 'copy) "Copying" "Renaming")))

      (with-parsed-tramp-file-name (if t1 filename newname) nil
	(unless (file-exists-p filename)
	  (tramp-compat-file-missing v filename))
	(when (and (not ok-if-already-exists) (file-exists-p newname))
	  (tramp-error v 'file-already-exists newname))
	(when (and (file-directory-p newname)
		   (not (directory-name-p newname)))
	  (tramp-error v 'file-error "File is a directory %s" newname))

	(with-tramp-progress-reporter
	    v 0 (format "%s %s to %s" msg-operation filename newname)

	  (cond
	   ;; Both are Tramp files.
	   ((and t1 t2)
	    (with-parsed-tramp-file-name filename v1
	      (with-parsed-tramp-file-name newname v2
		(cond
		 ;; Shortcut: if method, host, user are the same for
		 ;; both files, we invoke `cp' or `mv' on the remote
		 ;; host directly.
		 ((tramp-equal-remote filename newname)
		  (tramp-do-copy-or-rename-file-directly
		   op filename newname
		   ok-if-already-exists keep-date preserve-uid-gid))

		 ;; Try out-of-band operation.
		 ((and
		   (tramp-method-out-of-band-p v1 length)
		   (tramp-method-out-of-band-p v2 length))
		  (tramp-do-copy-or-rename-file-out-of-band
		   op filename newname ok-if-already-exists keep-date))

		 ;; No shortcut was possible.  So we copy the file
		 ;; first.  If the operation was `rename', we go back
		 ;; and delete the original file (if the copy was
		 ;; successful).  The approach is simple-minded: we
		 ;; create a new buffer, insert the contents of the
		 ;; source file into it, then write out the buffer to
		 ;; the target file.  The advantage is that it doesn't
		 ;; matter which file name handlers are used for the
		 ;; source and target file.
		 (t
		  (tramp-do-copy-or-rename-file-via-buffer
		   op filename newname ok-if-already-exists keep-date))))))

	   ;; One file is a Tramp file, the other one is local.
	   ((or t1 t2)
	    (cond
	     ;; Fast track on local machine.
	     ((tramp-local-host-p v)
	      (tramp-do-copy-or-rename-file-directly
	       op filename newname
	       ok-if-already-exists keep-date preserve-uid-gid))

	     ;; If the Tramp file has an out-of-band method, the
	     ;; corresponding copy-program can be invoked.
	     ((tramp-method-out-of-band-p v length)
	      (tramp-do-copy-or-rename-file-out-of-band
	       op filename newname ok-if-already-exists keep-date))

	     ;; Use the inline method via a Tramp buffer.
	     (t (tramp-do-copy-or-rename-file-via-buffer
		 op filename newname ok-if-already-exists keep-date))))

	   (t
	    ;; One of them must be a Tramp file.
	    (error "Tramp implementation says this cannot happen")))

	  ;; Handle `preserve-extended-attributes'.  We ignore possible
	  ;; errors, because ACL strings could be incompatible.
	  (when attributes
	    (ignore-errors
	      (set-file-extended-attributes newname attributes)))

	  ;; In case of `rename', we must flush the cache of the source file.
	  (when (and t1 (eq op 'rename))
	    (with-parsed-tramp-file-name filename v1
	      (tramp-flush-file-properties v1 v1-localname)))

	  ;; When newname did exist, we have wrong cached values.
	  (when t2
	    (with-parsed-tramp-file-name newname v2
	      (tramp-flush-file-properties v2 v2-localname))))))))

(defun tramp-do-copy-or-rename-file-via-buffer
    (op filename newname ok-if-already-exists keep-date)
  "Use an Emacs buffer to copy or rename a file.
First arg OP is either `copy' or `rename' and indicates the operation.
FILENAME is the source file, NEWNAME the target file.
KEEP-DATE is non-nil if NEWNAME should have the same timestamp as FILENAME."
  ;; Check, whether file is too large.  Emacs checks in `insert-file-1'
  ;; and `find-file-noselect', but that's not called here.
  (abort-if-file-too-large
   (tramp-compat-file-attribute-size (file-attributes (file-truename filename)))
   (symbol-name op) filename)
  ;; We must disable multibyte, because binary data shall not be
  ;; converted.  We don't want the target file to be compressed, so we
  ;; let-bind `jka-compr-inhibit' to t.  `epa-file-handler' shall not
  ;; be called either.  We remove `tramp-file-name-handler' from
  ;; `inhibit-file-name-handlers'; otherwise the file name handler for
  ;; `insert-file-contents' might be deactivated in some corner cases.
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary)
	(jka-compr-inhibit t)
	(inhibit-file-name-operation 'write-region)
	(inhibit-file-name-handlers
	 (cons 'epa-file-handler
	       (remq 'tramp-file-name-handler inhibit-file-name-handlers))))
    (with-temp-file newname
      (set-buffer-multibyte nil)
      (insert-file-contents-literally filename)))
  ;; KEEP-DATE handling.
  (when keep-date
    (tramp-compat-set-file-times
     newname
     (tramp-compat-file-attribute-modification-time
      (file-attributes filename))
     (unless ok-if-already-exists 'nofollow)))
  ;; Set the mode.
  (set-file-modes newname (tramp-default-file-modes filename))
  ;; If the operation was `rename', delete the original file.
  (unless (eq op 'copy) (delete-file filename)))

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
	(t2 (tramp-tramp-file-p newname))
	(file-times (tramp-compat-file-attribute-modification-time
		     (file-attributes filename)))
	(file-modes (tramp-default-file-modes filename)))
    (with-parsed-tramp-file-name (if t1 filename newname) nil
      (let* ((cmd (cond ((and (eq op 'copy) preserve-uid-gid) "cp -f -p")
			((eq op 'copy) "cp -f")
			((eq op 'rename) "mv -f")
			(t (tramp-error
			    v 'file-error
			    "Unknown operation `%s', must be `copy' or `rename'"
			    op))))
	     (localname1 (tramp-file-local-name filename))
	     (localname2 (tramp-file-local-name newname))
	     (prefix (file-remote-p (if t1 filename newname)))
             cmd-result)
	(when (and (eq op 'copy) (file-directory-p filename))
	  (setq cmd (concat cmd " -R")))

	(cond
	 ;; Both files are on a remote host, with same user.
	 ((and t1 t2)
          (setq cmd-result
                (tramp-send-command-and-check
                 v (format "%s %s %s" cmd
			   (tramp-shell-quote-argument localname1)
			   (tramp-shell-quote-argument localname2))))
	  (with-current-buffer (tramp-get-buffer v)
	    (goto-char (point-min))
	    (unless
		(or
		 (and keep-date
		      ;; Mask cp -f error.
		      (re-search-forward
		       tramp-operation-not-permitted-regexp nil t))
		 cmd-result)
	      (tramp-error-with-buffer
	       nil v 'file-error
	       "Copying directly failed, see buffer `%s' for details."
	       (buffer-name)))))

	 ;; We are on the local host.
	 ((or t1 t2)
	  (cond
	   ;; We can do it directly.
	   ((let (file-name-handler-alist)
	      (and (file-readable-p localname1)
		   ;; No sticky bit when renaming.
		   (or (eq op 'copy)
		       (zerop
			(logand
			 (file-modes (file-name-directory localname1)) #o1000)))
		   (file-writable-p (file-name-directory localname2))
		   (or (file-directory-p localname2)
		       (file-writable-p localname2))))
	    (if (eq op 'copy)
		(copy-file
		 localname1 localname2 ok-if-already-exists
		 keep-date preserve-uid-gid)
	      (tramp-run-real-handler
	       #'rename-file
	       (list localname1 localname2 ok-if-already-exists))))

	   ;; We can do it directly with `tramp-send-command'
	   ((and (file-readable-p (concat prefix localname1))
		 (file-writable-p
		  (file-name-directory (concat prefix localname2)))
		 (or (file-directory-p (concat prefix localname2))
		     (file-writable-p (concat prefix localname2))))
	    (tramp-do-copy-or-rename-file-directly
	     op (concat prefix localname1) (concat prefix localname2)
	     ok-if-already-exists keep-date preserve-uid-gid)
	    ;; We must change the ownership to the local user.
	    (tramp-set-file-uid-gid
	     (concat prefix localname2)
	     (tramp-get-local-uid 'integer)
	     (tramp-get-local-gid 'integer)))

	   ;; We need a temporary file in between.
	   (t
	    ;; Create the temporary file.
	    (let ((tmpfile (tramp-compat-make-temp-file localname1)))
	      (unwind-protect
		  (progn
		    (cond
		     (t1
		      (tramp-barf-unless-okay
		       v (format
			  "%s %s %s" cmd
			  (tramp-shell-quote-argument localname1)
			  (tramp-shell-quote-argument tmpfile))
		       "Copying directly failed, see buffer `%s' for details."
		       (tramp-get-buffer v))
		      ;; We must change the ownership as remote user.
		      ;; Since this does not work reliable, we also
		      ;; give read permissions.
		      (set-file-modes (concat prefix tmpfile) #o0777)
		      (tramp-set-file-uid-gid
		       (concat prefix tmpfile)
		       (tramp-get-local-uid 'integer)
		       (tramp-get-local-gid 'integer)))
		     (t2
		      (if (eq op 'copy)
			  (copy-file
			   localname1 tmpfile t keep-date preserve-uid-gid)
			(tramp-run-real-handler
			 #'rename-file (list localname1 tmpfile t)))
		      ;; We must change the ownership as local user.
		      ;; Since this does not work reliable, we also
		      ;; give read permissions.
		      (set-file-modes tmpfile #o0777)
		      (tramp-set-file-uid-gid
		       tmpfile
		       (tramp-get-remote-uid v 'integer)
		       (tramp-get-remote-gid v 'integer))))

		    ;; Move the temporary file to its destination.
		    (cond
		     (t2
		      (tramp-barf-unless-okay
		       v (format
			  "cp -f -p %s %s"
			  (tramp-shell-quote-argument tmpfile)
			  (tramp-shell-quote-argument localname2))
		       "Copying directly failed, see buffer `%s' for details."
		       (tramp-get-buffer v)))
		     (t1
		      (tramp-run-real-handler
		       #'rename-file
		       (list tmpfile localname2 ok-if-already-exists)))))

		;; Save exit.
		(ignore-errors (delete-file tmpfile)))))))))

      ;; Set the time and mode.  Mask possible errors.
      (ignore-errors
	  (when keep-date
	    (tramp-compat-set-file-times
	     newname file-times (unless ok-if-already-exists 'nofollow))
	    (set-file-modes newname file-modes))))))

(defun tramp-do-copy-or-rename-file-out-of-band
    (op filename newname ok-if-already-exists keep-date)
  "Invoke `scp' program to copy.
The method used must be an out-of-band method."
  (let* ((t1 (tramp-tramp-file-p filename))
	 (t2 (tramp-tramp-file-p newname))
	 (orig-vec (tramp-dissect-file-name (if t1 filename newname)))
	 copy-program copy-args copy-env copy-keep-date listener spec
	 options source target remote-copy-program remote-copy-args p)

    (with-parsed-tramp-file-name (if t1 filename newname) nil
      (if (and t1 t2)

	  ;; Both are Tramp files.  We shall optimize it when the
	  ;; methods for FILENAME and NEWNAME are the same.
	  (let* ((dir-flag (file-directory-p filename))
		 (tmpfile (tramp-compat-make-temp-file localname dir-flag)))
	    (if dir-flag
		(setq tmpfile
		      (expand-file-name
		       (file-name-nondirectory newname) tmpfile)))
	    (unwind-protect
		(progn
		  (tramp-do-copy-or-rename-file-out-of-band
		   op filename tmpfile ok-if-already-exists keep-date)
		  (tramp-do-copy-or-rename-file-out-of-band
		   'rename tmpfile newname ok-if-already-exists keep-date))
	      ;; Save exit.
	      (ignore-errors
		(if dir-flag
		    (delete-directory
		     (expand-file-name ".." tmpfile) 'recursive)
		  (delete-file tmpfile)))))

	;; Check which ones of source and target are Tramp files.
	(setq source (funcall
		      (if (and (string-equal method "rsync")
			       (file-directory-p filename)
			       (not (file-exists-p newname)))
			  #'file-name-as-directory
			#'identity)
		      (if t1
			  (tramp-make-copy-program-file-name v)
			(tramp-compat-file-name-unquote filename)))
	      target (if t2
			 (tramp-make-copy-program-file-name v)
		       (tramp-compat-file-name-unquote newname)))

	;; Check for user.  There might be an interactive setting.
	(setq user (or (tramp-file-name-user v)
		       (tramp-get-connection-property v "login-as" nil)))

	;; Check for listener port.
	(when (tramp-get-method-parameter v 'tramp-remote-copy-args)
	  (setq listener (number-to-string (+ 50000 (random 10000))))
	  (while
	      (zerop (tramp-call-process v "nc" nil nil nil "-z" host listener))
	    (setq listener (number-to-string (+ 50000 (random 10000))))))

	;; Compose copy command.
	(setq options
	      (format-spec
	       (tramp-ssh-controlmaster-options v)
	       (format-spec-make
		?t (tramp-get-connection-property
		    (tramp-get-connection-process v) "temp-file" "")))
	      spec (list
		    ?h (or host "") ?u (or user "") ?p (or port "")
		    ?r listener ?c options ?k (if keep-date " " "")
                    ?n (concat "2>" (tramp-get-remote-null-device v))
		    ?x (tramp-scp-strict-file-name-checking v))
	      copy-program (tramp-get-method-parameter v 'tramp-copy-program)
	      copy-keep-date (tramp-get-method-parameter
			      v 'tramp-copy-keep-date)
	      copy-args
	      ;; " " has either been a replacement of "%k" (when
	      ;; keep-date argument is non-nil), or a replacement for
	      ;; the whole keep-date sublist.
	      (delete " " (apply #'tramp-expand-args v 'tramp-copy-args spec))
	      ;; `tramp-ssh-controlmaster-options' is a string instead
	      ;; of a list.  Unflatten it.
	      copy-args
	      (tramp-compat-flatten-tree
	       (mapcar
		(lambda (x) (if (tramp-compat-string-search " " x)
                                (split-string x) x))
		copy-args))
	      copy-env (apply #'tramp-expand-args v 'tramp-copy-env spec)
	      remote-copy-program
	      (tramp-get-method-parameter v 'tramp-remote-copy-program)
	      remote-copy-args
	      (apply #'tramp-expand-args v 'tramp-remote-copy-args spec))

	;; Check for local copy program.
	(unless (executable-find copy-program)
	  (tramp-error
	   v 'file-error "Cannot find local copy program: %s" copy-program))

	;; Install listener on the remote side.  The prompt must be
	;; consumed later on, when the process does not listen anymore.
	(when remote-copy-program
	  (unless (with-tramp-connection-property
		      v (concat "remote-copy-program-" remote-copy-program)
		    (tramp-find-executable
		     v remote-copy-program (tramp-get-remote-path v)))
	    (tramp-error
	     v 'file-error
	     "Cannot find remote listener: %s" remote-copy-program))
	  (setq remote-copy-program
		(mapconcat
		 #'identity
		 (append
		  (list remote-copy-program) remote-copy-args
		  (list (if t1 (concat "<" source) (concat ">" target)) "&"))
		 " "))
	  (tramp-send-command v remote-copy-program)
	  (with-timeout
	      (60 (tramp-error
		   v 'file-error
		   "Listener process not running on remote host: `%s'"
		   remote-copy-program))
	    (tramp-send-command v (format "netstat -l | grep -q :%s" listener))
	    (while (not (tramp-send-command-and-check v nil))
	      (tramp-send-command
	       v (format "netstat -l | grep -q :%s" listener)))))

	(with-temp-buffer
	  (unwind-protect
	      ;; The default directory must be remote.
	      (let ((default-directory
		      (file-name-directory (if t1 filename newname)))
		    (process-environment (copy-sequence process-environment)))
		;; Set the transfer process properties.
		(tramp-set-connection-property
		 v "process-name" (buffer-name (current-buffer)))
		(tramp-set-connection-property
		 v "process-buffer" (current-buffer))
		(when copy-env
		  (tramp-message
		   orig-vec 6 "%s=\"%s\""
		   (car copy-env) (string-join (cdr copy-env) " "))
		  (setenv (car copy-env) (string-join (cdr copy-env) " ")))
		(setq
		 copy-args
		 (append
		  copy-args
		  (if remote-copy-program
		      (list (if t1 (concat ">" target) (concat "<" source)))
		    (list source target)))
		 ;; Use an asynchronous process.  By this, password
		 ;; can be handled.  We don't set a timeout, because
		 ;; the copying of large files can last longer than 60
		 ;; secs.
		 p (let ((default-directory tramp-compat-temporary-file-directory))
		     (apply
		      #'start-process
		      (tramp-get-connection-name v)
		      (tramp-get-connection-buffer v)
		      copy-program copy-args)))
		(tramp-message orig-vec 6 "%s" (string-join (process-command p) " "))
		(process-put p 'vector orig-vec)
		(process-put p 'adjust-window-size-function #'ignore)
		(set-process-query-on-exit-flag p nil)

		;; We must adapt `tramp-local-end-of-line' for
		;; sending the password.
		(let ((tramp-local-end-of-line tramp-rsh-end-of-line))
		  (tramp-process-actions
		   p v nil tramp-actions-copy-out-of-band)))

	    ;; Reset the transfer process properties.
	    (tramp-flush-connection-property v "process-name")
	    (tramp-flush-connection-property v "process-buffer")
	    ;; Clear the remote prompt.
	    (when (and remote-copy-program
		       (not (tramp-send-command-and-check v nil)))
	      ;; Houston, we have a problem!  Likely, the listener is
	      ;; still running, so let's clear everything (but the
	      ;; cached password).
	      (tramp-cleanup-connection v 'keep-debug 'keep-password))))

	;; Handle KEEP-DATE argument.
	(when (and keep-date (not copy-keep-date))
	  (tramp-compat-set-file-times
	   newname
	   (tramp-compat-file-attribute-modification-time
	    (file-attributes filename))
	   (unless ok-if-already-exists 'nofollow)))

	;; Set the mode.
	(unless (and keep-date copy-keep-date)
	  (ignore-errors
	    (set-file-modes newname (tramp-default-file-modes filename)))))

      ;; If the operation was `rename', delete the original file.
      (unless (eq op 'copy)
	(if (file-regular-p filename)
	    (delete-file filename)
	  (delete-directory filename 'recursive))))))

(defun tramp-sh-handle-make-directory (dir &optional parents)
  "Like `make-directory' for Tramp files."
  (setq dir (expand-file-name dir))
  (with-parsed-tramp-file-name dir nil
    (when (and (null parents) (file-exists-p dir))
      (tramp-error v 'file-already-exists dir))
    ;; When PARENTS is non-nil, DIR could be a chain of non-existent
    ;; directories a/b/c/...  Instead of checking, we simply flush the
    ;; whole cache.
    (tramp-flush-directory-properties
     v (if parents "/" (file-name-directory localname)))
    (tramp-barf-unless-okay
     v (format "%s -m %#o %s"
	       (if parents "mkdir -p" "mkdir")
	       (default-file-modes)
	       (tramp-shell-quote-argument localname))
     "Couldn't make directory %s" dir)))

(defun tramp-sh-handle-delete-directory (directory &optional recursive trash)
  "Like `delete-directory' for Tramp files."
  (tramp-skeleton-delete-directory directory recursive trash
    (tramp-barf-unless-okay
     v (format "cd / && %s %s"
               (if recursive "rm -rf" "rmdir")
	       (tramp-shell-quote-argument localname))
     "Couldn't delete %s" directory)))

(defun tramp-sh-handle-delete-file (filename &optional trash)
  "Like `delete-file' for Tramp files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (tramp-flush-file-properties v localname)
    (if (and delete-by-moving-to-trash trash)
	(move-file-to-trash filename)
      (tramp-barf-unless-okay
       v (format "rm -f %s" (tramp-shell-quote-argument localname))
       "Couldn't delete %s" filename))))

;; Dired.

(defun tramp-sh-handle-dired-compress-file (file)
  "Like `dired-compress-file' for Tramp files."
  ;; Code stolen mainly from dired-aux.el.
  (with-parsed-tramp-file-name file nil
    (tramp-flush-file-properties v localname)
    (let ((suffixes dired-compress-file-suffixes)
	  suffix)
      ;; See if any suffix rule matches this file name.
      (while suffixes
	(let (case-fold-search)
	  (if (string-match-p (car (car suffixes)) localname)
	      (setq suffix (car suffixes) suffixes nil))
	  (setq suffixes (cdr suffixes))))

      (cond ((file-symlink-p file) nil)
	    ((and suffix (nth 2 suffix))
	     ;; We found an uncompression rule.
	     (with-tramp-progress-reporter
                 v 0 (format "Uncompressing %s" file)
	       (when (tramp-send-command-and-check
		      v (concat (nth 2 suffix) " "
				(tramp-shell-quote-argument localname)))
		 (dired-remove-file file)
		 (string-match (car suffix) file)
		 (concat (substring file 0 (match-beginning 0))))))
	    (t
	     ;; We don't recognize the file as compressed, so compress it.
	     ;; Try gzip.
	     (with-tramp-progress-reporter v 0 (format "Compressing %s" file)
	       (when (tramp-send-command-and-check
		      v (concat "gzip -f "
				(tramp-shell-quote-argument localname)))
		 (dired-remove-file file)
		 (cond ((file-exists-p (concat file ".gz"))
			(concat file ".gz"))
		       ((file-exists-p (concat file ".z"))
			(concat file ".z"))
		       (t nil)))))))))

(defun tramp-sh-handle-insert-directory
    (filename switches &optional wildcard full-directory-p)
  "Like `insert-directory' for Tramp files."
  (unless switches (setq switches ""))
  ;; Check, whether directory is accessible.
  (unless wildcard
    (access-file filename "Reading directory"))
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (if (and (featurep 'ls-lisp)
	     (not (symbol-value 'ls-lisp-use-insert-directory-program)))
	(tramp-handle-insert-directory
	 filename switches wildcard full-directory-p)
      (when (stringp switches)
        (setq switches (split-string switches)))
      (setq switches
	    (append switches (split-string (tramp-sh--quoting-style-options v))))
      (unless (tramp-get-ls-command-with v "--dired")
	(setq switches (delete "--dired" switches)))
      (when wildcard
        (setq wildcard (tramp-run-real-handler
			#'file-name-nondirectory (list localname)))
        (setq localname (tramp-run-real-handler
			 #'file-name-directory (list localname))))
      (unless (or full-directory-p (member "-d" switches))
        (setq switches (append switches '("-d"))))
      (setq switches (mapconcat #'tramp-shell-quote-argument switches " "))
      (when wildcard
	(setq switches (concat switches " " wildcard)))
      (tramp-message
       v 4 "Inserting directory `ls %s %s', wildcard %s, fulldir %s"
       switches filename (if wildcard "yes" "no")
       (if full-directory-p "yes" "no"))
      ;; If `full-directory-p', we just say `ls -l FILENAME'.  Else we
      ;; chdir to the parent directory, then say `ls -ld BASENAME'.
      (if full-directory-p
	  (tramp-send-command
	   v (format "%s %s %s 2>%s"
		     (tramp-get-ls-command v)
		     switches
		     (if wildcard
			 localname
		       (tramp-shell-quote-argument (concat localname ".")))
                     (tramp-get-remote-null-device v)))
	(tramp-barf-unless-okay
	 v (format "cd %s" (tramp-shell-quote-argument
			    (tramp-run-real-handler
			     #'file-name-directory (list localname))))
	 "Couldn't `cd %s'"
	 (tramp-shell-quote-argument
	  (tramp-run-real-handler #'file-name-directory (list localname))))
	(tramp-send-command
	 v (format "%s %s %s 2>%s"
		   (tramp-get-ls-command v)
		   switches
		   (if (or wildcard
			   (zerop (length
				   (tramp-run-real-handler
				    #'file-name-nondirectory (list localname)))))
		       ""
		     (tramp-shell-quote-argument
		      (tramp-run-real-handler
                       #'file-name-nondirectory (list localname))))
                   (tramp-get-remote-null-device v))))

      (let ((beg-marker (copy-marker (point) nil))
	    (end-marker (copy-marker (point) t))
	    (emc enable-multibyte-characters))
	;; We cannot use `insert-buffer-substring' because the Tramp
	;; buffer changes its contents before insertion due to calling
	;; `expand-file-name' and alike.
	(insert (with-current-buffer (tramp-get-buffer v) (buffer-string)))

	;; We must enable unibyte strings, because the "--dired"
	;; output counts in bytes.
	(set-buffer-multibyte nil)
	(save-restriction
	  (narrow-to-region beg-marker end-marker)
	  ;; Check for "--dired" output.
	  (when (re-search-backward "^//DIRED//\\s-+\\(.+\\)$" nil 'noerror)
	    (let ((beg (match-beginning 1))
		  (end (match-end 0)))
	      ;; Now read the numeric positions of file names.
	      (goto-char beg)
	      (while (< (point) end)
		(let ((start (+ (point-min) (read (current-buffer))))
		      (end (+ (point-min) (read (current-buffer)))))
		  (if (memq (char-after end) '(?\n ?\ ))
		      ;; End is followed by \n or by " -> ".
		      (put-text-property start end 'dired-filename t))))))
	  ;; Remove trailing lines.
	  (beginning-of-line)
	  (while (looking-at "//")
	    (forward-line 1)
	    (delete-region (match-beginning 0) (point))))
	;; Reset multibyte if needed.
	(set-buffer-multibyte emc)

	(save-restriction
	  (narrow-to-region beg-marker end-marker)
	  ;; Some busyboxes are reluctant to discard colors.
	  (unless (tramp-compat-string-search
		   "color" (tramp-get-connection-property v "ls" ""))
	    (goto-char (point-min))
	    (while (re-search-forward tramp-display-escape-sequence-regexp nil t)
	      (replace-match "")))

          ;; Now decode what read if necessary.  Stolen from `insert-directory'.
	  (let ((coding (or coding-system-for-read
			    file-name-coding-system
			    default-file-name-coding-system
			    'undecided))
		coding-no-eol
		val pos)
	    (when (and enable-multibyte-characters
		       (not (memq (coding-system-base coding)
				  '(raw-text no-conversion))))
	      ;; If no coding system is specified or detection is
	      ;; requested, detect the coding.
	      (if (eq (coding-system-base coding) 'undecided)
		  (setq coding (detect-coding-region (point-min) (point) t)))
	      (unless (eq (coding-system-base coding) 'undecided)
		(setq coding-no-eol
		      (coding-system-change-eol-conversion coding 'unix))
		(goto-char (point-min))
		(while (not (eobp))
		  (setq pos (point)
			val (get-text-property (point) 'dired-filename))
		  (goto-char (next-single-property-change
			      (point) 'dired-filename nil (point-max)))
		  ;; Force no eol conversion on a file name, so that
		  ;; CR is preserved.
		  (decode-coding-region
		   pos (point) (if val coding-no-eol coding))
		  (if val (put-text-property pos (point) 'dired-filename t))))))

	  ;; The inserted file could be from somewhere else.
	  (when (and (not wildcard) (not full-directory-p))
	    (goto-char (point-max))
	    (when (file-symlink-p filename)
	      (goto-char (search-backward "->" (point-min) 'noerror)))
	    (search-backward
	     (if (directory-name-p filename)
		 "."
	       (file-name-nondirectory filename))
	     (point-min) 'noerror)
	    (replace-match (file-relative-name filename) t))

	  ;; Try to insert the amount of free space.
	  (goto-char (point-min))
	  ;; First find the line to put it on.
	  (when (and (re-search-forward "^\\([[:space:]]*total\\)" nil t)
		     ;; Emacs 29.1 or later.
		     (not (fboundp 'dired--insert-disk-space)))
	    (when-let ((available (get-free-disk-space ".")))
	      ;; Replace "total" with "total used", to avoid confusion.
	      (replace-match "\\1 used in directory")
	      (end-of-line)
	      (insert " available " available))))

	(prog1 (goto-char end-marker)
	  (set-marker beg-marker nil)
	  (set-marker end-marker nil))))))

;; Canonicalization of file names.

(defun tramp-sh-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for Tramp files.
If the localname part of the given file name starts with \"/../\" then
the result will be a local, non-Tramp, file name."
  ;; If DIR is not given, use `default-directory' or "/".
  (setq dir (or dir default-directory "/"))
  ;; Handle empty NAME.
  (when (zerop (length name)) (setq name "."))
  ;; On MS Windows, some special file names are not returned properly
  ;; by `file-name-absolute-p'.
  (if (and (eq system-type 'windows-nt)
	   (string-match-p
	    (concat "^\\([[:alpha:]]:\\|" null-device "$\\)") name))
      (tramp-run-real-handler #'expand-file-name (list name dir))
    ;; Unless NAME is absolute, concat DIR and NAME.
    (unless (file-name-absolute-p name)
      (setq name (tramp-compat-file-name-concat dir name)))
    ;; If connection is not established yet, run the real handler.
    (if (not (tramp-connectable-p name))
	(tramp-run-real-handler #'expand-file-name (list name nil))
      ;; Dissect NAME.
      (with-parsed-tramp-file-name name nil
	(unless (tramp-run-real-handler #'file-name-absolute-p (list localname))
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
		       (string-match-p "\\`su\\(do\\)?\\'" method))
	      (setq uname (concat uname user)))
	    (setq uname
		  (with-tramp-connection-property v uname
		    (tramp-send-command
		     v
		     (format "cd %s && pwd" (tramp-shell-quote-argument uname)))
		    (with-current-buffer (tramp-get-buffer v)
		      (goto-char (point-min))
		      (buffer-substring (point) (point-at-eol)))))
	    (setq localname (concat uname fname))))
	;; There might be a double slash, for example when "~/"
	;; expands to "/".  Remove this.
	(while (string-match "//" localname)
	  (setq localname (replace-match "/" t t localname)))
	;; Do not keep "/..".
	(when (string-match-p "^/\\.\\.?$" localname)
	  (setq localname "/"))
	;; No tilde characters in file name, do normal
	;; `expand-file-name' (this does "/./" and "/../").
	;; `default-directory' is bound, because on Windows there
	;; would be problems with UNC shares or Cygwin mounts.
	(let ((default-directory tramp-compat-temporary-file-directory))
	  (tramp-make-tramp-file-name
	   v (tramp-drop-volume-letter
	      (tramp-run-real-handler
	       #'expand-file-name (list localname)))))))))

;;; Remote commands:

;; We use BUFFER also as connection buffer during setup.  Because of
;; this, its original contents must be saved, and restored once
;; connection has been setup.
(defun tramp-sh-handle-make-process (&rest args)
  "Like `make-process' for Tramp files.
STDERR can also be a remote file name.  If method parameter
`tramp-direct-async' and connection property
\"direct-async-process\" are non-nil, an alternative
implementation will be used."
  (if (tramp-direct-async-process-p args)
      (apply #'tramp-handle-make-process args)
    (when args
      (with-parsed-tramp-file-name (expand-file-name default-directory) nil
	(let ((name (plist-get args :name))
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
	  (unless (or (null command) (consp command))
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
	  (unless (or (bufferp stderr) (string-or-null-p stderr))
	    (signal 'wrong-type-argument (list #'bufferp stderr)))
	  (when (and (stringp stderr)
		     (not (tramp-equal-remote default-directory stderr)))
	    (signal 'file-error (list "Wrong stderr" stderr)))

	  (let* ((buffer
		  (if buffer
		      (get-buffer-create buffer)
		    ;; BUFFER can be nil.  We use a temporary buffer.
		    (generate-new-buffer tramp-temp-buffer-name)))
		 ;; STDERR can also be a file name.
		 (tmpstderr
		  (and stderr
		       (tramp-unquote-file-local-name
			(if (stringp stderr)
			    stderr (tramp-make-tramp-temp-name v)))))
		 (remote-tmpstderr
		  (and tmpstderr (tramp-make-tramp-file-name v tmpstderr)))
		 (program (car command))
		 (args (cdr command))
		 ;; When PROGRAM matches "*sh", and the first arg is
		 ;; "-c", it might be that the arguments exceed the
		 ;; command line length.  Therefore, we modify the
		 ;; command.
		 (heredoc (and (not (bufferp stderr))
			       (stringp program)
			       (string-match-p "sh$" program)
			       (= (length args) 2)
			       (string-equal "-c" (car args))
			       ;; Don't if there is a quoted string.
			       (not (string-match-p "'\\|\"" (cadr args)))
			       ;; Check, that /dev/tty is usable.
			       (tramp-get-remote-dev-tty v)))
		 ;; When PROGRAM is nil, we just provide a tty.
		 (args (if (not heredoc) args
			 (let ((i 250))
			   (while (and (< i (length (cadr args)))
				       (string-match " " (cadr args) i))
			     (setcdr
			      args
			      (list
			       (replace-match " \\\\\n" nil nil (cadr args))))
			     (setq i (+ i 250))))
			 (cdr args)))
		 ;; Use a human-friendly prompt, for example for
		 ;; `shell'.  We discard hops, if existing, that's why
		 ;; we cannot use `file-remote-p'.
		 (prompt (format "PS1=%s %s"
				 (tramp-make-tramp-file-name v nil 'nohop)
				 tramp-initial-end-of-output))
		 ;; We use as environment the difference to toplevel
		 ;; `process-environment'.
		 env uenv
		 (env (dolist (elt (cons prompt process-environment) env)
			(or (member
			     elt (default-toplevel-value 'process-environment))
			    (if (tramp-compat-string-search "=" elt)
				(setq env (append env `(,elt)))
			      (setq uenv (cons elt uenv))))))
		 (env (setenv-internal
		       env "INSIDE_EMACS" (tramp-inside-emacs) 'keep))
		 (command
		  (when (stringp program)
		    (format "cd %s && %s exec %s %s env %s %s"
			    (tramp-shell-quote-argument localname)
			    (if uenv
				(format
				 "unset %s &&"
				 (mapconcat
				  #'tramp-shell-quote-argument uenv " "))
			      "")
			    (if heredoc
				(format "<<'%s'" tramp-end-of-heredoc) "")
			    (if tmpstderr (format "2>'%s'" tmpstderr) "")
			    (mapconcat #'tramp-shell-quote-argument env " ")
			    (if heredoc
				(format "%s\n(\n%s\n) </dev/tty\n%s"
					program (car args) tramp-end-of-heredoc)
			      (mapconcat #'tramp-shell-quote-argument
					 (cons program args) " ")))))
		 (tramp-process-connection-type
		  (or (null program) tramp-process-connection-type))
		 (bmp (and (buffer-live-p buffer) (buffer-modified-p buffer)))
		 (name1 name)
		 (i 0)
		 ;; We do not want to raise an error when
		 ;; `make-process' has been started several times in
		 ;; `eshell' and friends.
		 tramp-current-connection
		 p)

	    ;; Handle error buffer.
	    (when (bufferp stderr)
	      (unless (tramp-get-remote-mknod-or-mkfifo v)
		(tramp-error
		 v 'file-error "Stderr buffer `%s' not supported" stderr))
	      (with-current-buffer stderr
		(setq buffer-read-only nil))
	      ;; Create named pipe.
	      (tramp-send-command
	       v (format (tramp-get-remote-mknod-or-mkfifo v) tmpstderr))
	      ;; Create stderr process.
	      (make-process
	       :name (buffer-name stderr)
	       :buffer stderr
	       :command `("cat" ,tmpstderr)
	       :coding coding
	       :noquery t
	       :filter nil
	       :sentinel #'ignore
	       :file-handler t))

	    (while (get-process name1)
	      ;; NAME must be unique as process name.
	      (setq i (1+ i)
		    name1 (format "%s<%d>" name i)))
	    (setq name name1)
	    ;; Set the new process properties.
	    (tramp-set-connection-property v "process-name" name)
	    (tramp-set-connection-property v "process-buffer" buffer)

	    (with-current-buffer (tramp-get-connection-buffer v)
	      (unwind-protect
		  ;; We catch this event.  Otherwise, `make-process'
		  ;; could be called on the local host.
		  (save-excursion
		    (save-restriction
		      ;; Activate narrowing in order to save BUFFER
		      ;; contents.  Clear also the modification time;
		      ;; otherwise we might be interrupted by
		      ;; `verify-visited-file-modtime'.
		      (let ((buffer-undo-list t)
			    (inhibit-read-only t)
			    (mark (point-max))
			    (coding-system-for-write
			     (if (symbolp coding) coding (car coding)))
			    (coding-system-for-read
			     (if (symbolp coding) coding (cdr coding))))
			(clear-visited-file-modtime)
			(narrow-to-region (point-max) (point-max))
			(catch 'suppress
			  ;; Set the pid of the remote shell.  This is
			  ;; needed when sending signals remotely.
			  (let ((pid (tramp-send-command-and-read v "echo $$")))
			    (setq p (tramp-get-connection-process v))
			    (process-put p 'remote-pid pid)
			    (tramp-set-connection-property p "remote-pid" pid))
			  ;; Disable carriage return to newline
			  ;; translation.  This does not work on
			  ;; macOS, see Bug#50748.
			  (when (and (memq connection-type '(nil pipe))
                                     (not (tramp-check-remote-uname v "Darwin")))
			    (tramp-send-command v "stty -icrnl"))
			  ;; `tramp-maybe-open-connection' and
			  ;; `tramp-send-command-and-read' could have
			  ;; trashed the connection buffer.  Remove this.
			  (widen)
			  (delete-region mark (point-max))
			  (narrow-to-region (point-max) (point-max))
			  ;; Now do it.
			  (if command
			      ;; Send the command.
			      (tramp-send-command v command nil t) ; nooutput
			    ;; Check, whether a pty is associated.
			    (unless (process-get p 'remote-tty)
			      (tramp-error
			       v 'file-error
			       "pty association is not supported for `%s'"
			       name))))
			;; Set sentinel and filter.
			(when sentinel
			  (set-process-sentinel p sentinel))
			(when filter
			  (set-process-filter p filter))
			;; Set query flag and process marker for this
			;; process.  We ignore errors, because the
			;; process could have finished already.
			(ignore-errors
			  (set-process-query-on-exit-flag p (null noquery))
			  (set-marker (process-mark p) (point)))
			;; Kill stderr process and delete named pipe.
			(when (bufferp stderr)
			  (add-function
			   :after (process-sentinel p)
			   (lambda (_proc _msg)
			     (ignore-errors
			       (while (accept-process-output
				       (get-buffer-process stderr) 0 nil t))
			       (delete-process (get-buffer-process stderr)))
			     (ignore-errors
			       (delete-file remote-tmpstderr)))))
			;; Return process.
			p)))

		;; Save exit.
		(if (string-prefix-p tramp-temp-buffer-name (buffer-name))
		    (ignore-errors
		      (set-process-buffer p nil)
		      (kill-buffer (current-buffer)))
		  (set-buffer-modified-p bmp))
		(tramp-flush-connection-property v "process-name")
		(tramp-flush-connection-property v "process-buffer")))))))))

(defun tramp-sh-get-signal-strings (vec)
  "Strings to return by `process-file' in case of signals."
  (with-tramp-connection-property
      vec
      (concat
       "signal-strings-" (tramp-get-method-parameter vec 'tramp-remote-shell))
    (let ((default-directory (tramp-make-tramp-file-name vec 'localname))
	  process-file-return-signal-string signals res result)
      (setq signals
	    (append
	     '(0) (split-string (shell-command-to-string "kill -l") nil 'omit)))
      ;; Sanity check.  Sometimes, the first entry is "0", although we
      ;; don't expect it.  Remove it.
      (when (and (stringp (cadr signals)) (string-equal "0" (cadr signals)))
	(setcdr signals (cddr signals)))
      ;; Sanity check.  "kill -l" shall have returned just the signal
      ;; names.  Some shells don't, like the one in "docker alpine".
      (let (signal-hook-function)
	(condition-case nil
	    (dolist (sig (cdr signals))
	      (unless (string-match-p "^[[:alnum:]+-]+$" sig)
		(error nil)))
	  (error (setq signals '(0)))))
      (dotimes (i 128)
	(push
	 (cond
	  ;; Some predefined values, which aren't reported sometimes,
	  ;; or would raise problems (all Stopped signals).
	  ((= i 0) 0)
	  ((string-equal (nth i signals) "HUP") "Hangup")
	  ((string-equal (nth i signals) "INT") "Interrupt")
	  ((string-equal (nth i signals) "QUIT") "Quit")
	  ((string-equal (nth i signals) "STOP") "Stopped (signal)")
	  ((string-equal (nth i signals) "TSTP") "Stopped")
	  ((string-equal (nth i signals) "TTIN") "Stopped (tty input)")
	  ((string-equal (nth i signals) "TTOU") "Stopped (tty output)")
	  (t (setq res
		   (if (null (nth i signals))
		       ""
		     (tramp-send-command
		      vec
		      (format
		       "%s %s %s"
		       (tramp-get-method-parameter vec 'tramp-remote-shell)
		       (mapconcat
			#'identity
			(tramp-get-method-parameter vec 'tramp-remote-shell-args)
			" ")
		       (tramp-shell-quote-argument (format "kill -%d $$" i))))
		     (with-current-buffer (tramp-get-connection-buffer vec)
		       (goto-char (point-min))
		       (buffer-substring (point-at-bol) (point-at-eol)))))
	     (if (string-equal res "")
		 (format "Signal %d" i)
	       res)))
	 result))
      ;; Due to Bug#41287, we cannot add this to the `dotimes' clause.
      (reverse result))))

(defun tramp-sh-handle-process-file
  (program &optional infile destination display &rest args)
  "Like `process-file' for Tramp files."
  ;; The implementation is not complete yet.
  (when (and (numberp destination) (zerop destination))
    (error "Implementation does not handle immediate return"))

  (with-parsed-tramp-file-name (expand-file-name default-directory) nil
    (let (command env uenv input tmpinput stderr tmpstderr outbuf ret)
      ;; Compute command.
      (setq command (mapconcat #'tramp-shell-quote-argument
			       (cons program args) " "))
      ;; We use as environment the difference to toplevel `process-environment'.
      (dolist (elt process-environment)
        (or (member elt (default-toplevel-value 'process-environment))
            (if (tramp-compat-string-search "=" elt)
                (setq env (append env `(,elt)))
              (setq uenv (cons elt uenv)))))
      (setenv-internal env "INSIDE_EMACS" (tramp-inside-emacs) 'keep)
      (when env
	(setq command
	      (format
	       "env %s %s"
	       (mapconcat #'tramp-shell-quote-argument env " ") command)))
      (when uenv
        (setq command
              (format
               "unset %s && %s"
               (mapconcat #'tramp-shell-quote-argument uenv " ") command)))
      ;; Determine input.
      (if (null infile)
	  (setq input (tramp-get-remote-null-device v))
	(setq infile (tramp-compat-file-name-unquote (expand-file-name infile)))
	(if (tramp-equal-remote default-directory infile)
	    ;; INFILE is on the same remote host.
	    (setq input (tramp-unquote-file-local-name infile))
	  ;; INFILE must be copied to remote host.
	  (setq input (tramp-make-tramp-temp-file v)
		tmpinput (tramp-make-tramp-file-name v input 'nohop))
	  (copy-file infile tmpinput t)))
      (when input (setq command (format "%s <%s" command input)))

      ;; Determine output.
      (cond
       ;; Just a buffer.
       ((bufferp destination)
	(setq outbuf destination))
       ;; A buffer name.
       ((stringp destination)
	(setq outbuf (get-buffer-create destination)))
       ;; (REAL-DESTINATION ERROR-DESTINATION)
       ((consp destination)
	;; output.
	(cond
	 ((bufferp (car destination))
	  (setq outbuf (car destination)))
	 ((stringp (car destination))
	  (setq outbuf (get-buffer-create (car destination))))
	 ((car destination)
	  (setq outbuf (current-buffer))))
	;; stderr.
	(cond
	 ((stringp (cadr destination))
	  (setcar (cdr destination) (expand-file-name (cadr destination)))
	  (if (tramp-equal-remote default-directory (cadr destination))
	      ;; stderr is on the same remote host.
	      (setq stderr (tramp-unquote-file-local-name (cadr destination)))
	    ;; stderr must be copied to remote host.  The temporary
	    ;; file must be deleted after execution.
	    (setq stderr (tramp-make-tramp-temp-file v)
		  tmpstderr (tramp-make-tramp-file-name v stderr 'nohop))))
	 ;; stderr to be discarded.
	 ((null (cadr destination))
	  (setq stderr (tramp-get-remote-null-device v)))))
       ;; 't
       (destination
	(setq outbuf (current-buffer))))
      (when stderr (setq command (format "%s 2>%s" command stderr)))

      ;; Send the command.  It might not return in time, so we protect
      ;; it.  Call it in a subshell, in order to preserve working
      ;; directory.
      (condition-case nil
	  (unwind-protect
              (setq ret (tramp-send-command-and-check
			 v (format
			    "cd %s && %s"
			    (tramp-unquote-shell-quote-argument localname)
			    command)
			 t t t))
	    (unless (natnump ret) (setq ret 1))
	    ;; We should add the output anyway.
	    (when outbuf
	      (with-current-buffer outbuf
                (insert
                 (with-current-buffer (tramp-get-connection-buffer v)
                   (buffer-string))))
	      (when (and display (get-buffer-window outbuf t)) (redisplay))))
	;; When the user did interrupt, we should do it also.  We use
	;; return code -1 as marker.
	(quit
	 (kill-buffer (tramp-get-connection-buffer v))
	 (setq ret -1))
	;; Handle errors.
	(error
	 (kill-buffer (tramp-get-connection-buffer v))
	 (setq ret 1)))

      ;; Handle signals.  `process-file-return-signal-string' exists
      ;; since Emacs 28.1.
      (when (and (bound-and-true-p process-file-return-signal-string)
		 (natnump ret) (>= ret 128))
	(setq ret (nth (- ret 128) (tramp-sh-get-signal-strings v))))

      ;; Provide error file.
      (when tmpstderr (rename-file tmpstderr (cadr destination) t))

      ;; Cleanup.  We remove all file cache values for the connection,
      ;; because the remote process could have changed them.
      (when tmpinput (delete-file tmpinput))
      (when process-file-side-effects
        (tramp-flush-directory-properties v ""))

      ;; Return exit status.
      (if (equal ret -1)
	  (keyboard-quit)
	ret))))

(defun tramp-sh-handle-exec-path ()
  "Like `exec-path' for Tramp files."
  (append
   (tramp-get-remote-path (tramp-dissect-file-name default-directory))
   ;; The equivalent to `exec-directory'.
   `(,(tramp-file-local-name (expand-file-name default-directory)))))

(defun tramp-sh-handle-file-local-copy (filename)
  "Like `file-local-copy' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (unless (file-exists-p (file-truename filename))
      (tramp-compat-file-missing v filename))

    (let* ((size (tramp-compat-file-attribute-size
		  (file-attributes (file-truename filename))))
	   (rem-enc (tramp-get-inline-coding v "remote-encoding" size))
	   (loc-dec (tramp-get-inline-coding v "local-decoding" size))
	   (tmpfile (tramp-compat-make-temp-file filename)))

      (condition-case err
	  (cond
	   ;; `copy-file' handles direct copy and out-of-band methods.
	   ((or (tramp-local-host-p v)
		(tramp-method-out-of-band-p v size))
	    (copy-file filename tmpfile 'ok-if-already-exists 'keep-time))

	   ;; Use inline encoding for file transfer.
	   (rem-enc
	    (with-tramp-progress-reporter
		v 3
		(format-message
		 "Encoding remote file `%s' with `%s'" filename rem-enc)
	      (tramp-barf-unless-okay
	       v (format rem-enc (tramp-shell-quote-argument localname))
	       "Encoding remote file failed"))

	    (with-tramp-progress-reporter
		v 3 (format-message
		     "Decoding local file `%s' with `%s'" tmpfile loc-dec)
	      (if (functionp loc-dec)
		  ;; If local decoding is a function, we call it.  We
		  ;; must disable multibyte, because
		  ;; `uudecode-decode-region' doesn't handle it
		  ;; correctly.  Unset `file-name-handler-alist'.
		  ;; Otherwise, epa-file gets confused.
		  (let (file-name-handler-alist
			(coding-system-for-write 'binary)
			(default-directory
			  tramp-compat-temporary-file-directory))
		    (with-temp-file tmpfile
		      (set-buffer-multibyte nil)
		      (insert-buffer-substring (tramp-get-buffer v))
		      (funcall loc-dec (point-min) (point-max))))

		;; If tramp-decoding-function is not defined for this
		;; method, we invoke tramp-decoding-command instead.
		(let ((tmpfile2 (tramp-compat-make-temp-file filename)))
		  ;; Unset `file-name-handler-alist'.  Otherwise,
		  ;; epa-file gets confused.
		  (let (file-name-handler-alist
			(coding-system-for-write 'binary))
		    (with-current-buffer (tramp-get-buffer v)
		      (write-region
		       (point-min) (point-max) tmpfile2 nil 'no-message)))
		  (unwind-protect
		      (tramp-call-local-coding-command
		       loc-dec tmpfile2 tmpfile)
		    (delete-file tmpfile2)))))

	    ;; Set proper permissions.
	    (set-file-modes tmpfile (tramp-default-file-modes filename))
	    ;; Set local user ownership.
	    (tramp-set-file-uid-gid tmpfile))

	   ;; Oops, I don't know what to do.
	   (t (tramp-error
	       v 'file-error "Wrong method specification for `%s'" method)))

	;; Error handling.
	((error quit)
	 (delete-file tmpfile)
	 (signal (car err) (cdr err))))

      (run-hooks 'tramp-handle-file-local-copy-hook)
      tmpfile)))

(defun tramp-sh-handle-write-region
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

      (if (and (tramp-local-host-p v)
	       ;; `file-writable-p' calls `file-expand-file-name'.  We
	       ;; cannot use `tramp-run-real-handler' therefore.
	       (file-writable-p (file-name-directory localname))
	       (or (file-directory-p localname)
		   (file-writable-p localname)))
	  ;; Short track: if we are on the local host, we can run directly.
	  (let ((create-lockfiles (not file-locked)))
	    (write-region start end localname append 'no-message lockname))

	(let* ((modes (tramp-default-file-modes
		       filename (and (eq mustbenew 'excl) 'nofollow)))
	       ;; We use this to save the value of
	       ;; `last-coding-system-used' after writing the tmp
	       ;; file.  At the end of the function, we set
	       ;; `last-coding-system-used' to this saved value.  This
	       ;; way, any intermediary coding systems used while
	       ;; talking to the remote shell or suchlike won't hose
	       ;; this variable.  This approach was snarfed from
	       ;; ange-ftp.el.
	       coding-system-used
	       ;; Write region into a tmp file.  This isn't really
	       ;; needed if we use an encoding function, but currently
	       ;; we use it always because this makes the logic
	       ;; simpler.  We must also set `temporary-file-directory',
	       ;; because it could point to a remote directory.
	       (temporary-file-directory tramp-compat-temporary-file-directory)
	       (tmpfile (or tramp-temp-buffer-file-name
			    (tramp-compat-make-temp-file filename))))

	  ;; If `append' is non-nil, we copy the file locally, and let
	  ;; the native `write-region' implementation do the job.
	  (when (and append (file-exists-p filename))
	    (copy-file filename tmpfile 'ok))

	  ;; We say `no-message' here because we don't want the
	  ;; visited file modtime data to be clobbered from the temp
	  ;; file.  We call `set-visited-file-modtime' ourselves later
	  ;; on.  We must ensure that `file-coding-system-alist'
	  ;; matches `tmpfile'.
	  (let ((file-coding-system-alist
		 (tramp-find-file-name-coding-system-alist filename tmpfile))
                create-lockfiles)
	    (condition-case err
		(write-region start end tmpfile append 'no-message)
	      ((error quit)
	       (setq tramp-temp-buffer-file-name nil)
	       (delete-file tmpfile)
	       (signal (car err) (cdr err))))

	    ;; Now, `last-coding-system-used' has the right value.  Remember it.
	    (setq coding-system-used last-coding-system-used))

	  ;; The permissions of the temporary file should be set.  If
	  ;; FILENAME does not exist (eq modes nil) it has been
	  ;; renamed to the backup file.  This case `save-buffer'
	  ;; handles permissions.
	  ;; Ensure that it is still readable.
	  (when modes
	    (set-file-modes tmpfile (logior (or modes 0) #o0400)))

	  ;; This is a bit lengthy due to the different methods
	  ;; possible for file transfer.  First, we check whether the
	  ;; method uses an scp program.  If so, we call it.
	  ;; Otherwise, both encoding and decoding command must be
	  ;; specified.  However, if the method _also_ specifies an
	  ;; encoding function, then that is used for encoding the
	  ;; contents of the tmp file.
	  (let* ((size (tramp-compat-file-attribute-size
			(file-attributes tmpfile)))
		 (rem-dec (tramp-get-inline-coding v "remote-decoding" size))
		 (loc-enc (tramp-get-inline-coding v "local-encoding" size)))
	    (cond
	     ;; `copy-file' handles direct copy and out-of-band methods.
	     ((or (tramp-local-host-p v)
		  (tramp-method-out-of-band-p v size))
	      (if (and (not (stringp start))
		       (= (or end (point-max)) (point-max))
		       (= (or start (point-min)) (point-min))
		       (tramp-get-method-parameter v 'tramp-copy-keep-tmpfile))
		  (progn
		    (setq tramp-temp-buffer-file-name tmpfile)
		    (condition-case err
			;; We keep the local file for performance
			;; reasons, useful for "rsync".
			(copy-file tmpfile filename t)
		      ((error quit)
		       (setq tramp-temp-buffer-file-name nil)
		       (delete-file tmpfile)
		       (signal (car err) (cdr err)))))
		(setq tramp-temp-buffer-file-name nil)
		;; Don't rename, in order to keep context in SELinux.
		(unwind-protect
		    (copy-file tmpfile filename t)
		  (delete-file tmpfile))))

	     ;; Use inline file transfer.
	     (rem-dec
	      ;; Encode tmpfile.
	      (unwind-protect
		  (with-temp-buffer
		    (set-buffer-multibyte nil)
		    ;; Use encoding function or command.
		    (with-tramp-progress-reporter
			v 3 (format-message
			     "Encoding local file `%s' using `%s'"
			     tmpfile loc-enc)
		      (if (functionp loc-enc)
			  ;; The following `let' is a workaround for
			  ;; the base64.el that comes with pgnus-0.84.
			  ;; If both of the following conditions are
			  ;; satisfied, it tries to write to a local
			  ;; file in default-directory, but at this
			  ;; point, default-directory is remote.
			  ;; (`call-process-region' can't write to
			  ;; remote files, it seems.)  The file in
			  ;; question is a tmp file anyway.
			  (let ((coding-system-for-read 'binary)
				(default-directory
				  tramp-compat-temporary-file-directory))
			    (insert-file-contents-literally tmpfile)
			    (funcall loc-enc (point-min) (point-max)))

			(unless (zerop (tramp-call-local-coding-command
					loc-enc tmpfile t))
			  (tramp-error
			   v 'file-error
			   (concat "Cannot write to `%s', "
				   "local encoding command `%s' failed")
			   filename loc-enc))))

		    ;; Send buffer into remote decoding command which
		    ;; writes to remote file.  Because this happens on
		    ;; the remote host, we cannot use the function.
		    (with-tramp-progress-reporter
			v 3 (format-message
			     "Decoding remote file `%s' using `%s'"
			     filename rem-dec)
		      (goto-char (point-max))
		      (unless (bolp) (newline))
		      (tramp-send-command
		       v
		       (format
			(concat rem-dec " <<'%s'\n%s%s")
			(tramp-shell-quote-argument localname)
			tramp-end-of-heredoc
			(buffer-string)
			tramp-end-of-heredoc))
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
			 (zerop (tramp-call-process v "cksum" tmpfile t))
			 ;; cksum runs remotely.
			 (tramp-send-command-and-check
			  v
			  (format
			   "cksum <%s" (tramp-shell-quote-argument localname)))
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
			  filename rem-dec)))))

		;; Save exit.
		(delete-file tmpfile)))

	     ;; That's not expected.
	     (t
	      (tramp-error
	       v 'file-error
	       (concat "Method `%s' should specify both encoding and "
		       "decoding command or an scp program")
	       method))))

	  ;; Make `last-coding-system-used' have the right value.
	  (when coding-system-used
            (setq last-coding-system-used coding-system-used))))

      (tramp-flush-file-properties v localname)

      ;; We must protect `last-coding-system-used', now we have set it
      ;; to its correct value.
      (let (last-coding-system-used (need-chown t))
	;; Set file modification time.
	(when (or (eq visit t) (stringp visit))
          (let ((file-attr (file-attributes filename 'integer)))
            (set-visited-file-modtime
             ;; We must pass modtime explicitly, because FILENAME can
             ;; be different from (buffer-file-name), f.e. if
             ;; `file-precious-flag' is set.
	     (or (tramp-compat-file-attribute-modification-time file-attr)
		 (current-time)))
            (when (and (= (tramp-compat-file-attribute-user-id file-attr) uid)
                       (= (tramp-compat-file-attribute-group-id file-attr) gid))
              (setq need-chown nil))))

	;; Set the ownership.
        (when need-chown
          (tramp-set-file-uid-gid filename uid gid))

	;; Unlock file.
	(when file-locked
	  ;; `unlock-file' exists since Emacs 28.1.
	  (tramp-compat-funcall 'unlock-file lockname))

	(when (and (null noninteractive)
		   (or (eq visit t) (string-or-null-p visit)))
	  (tramp-message v 0 "Wrote %s" filename))
	(run-hooks 'tramp-handle-write-region-hook)))))

(defvar tramp-vc-registered-file-names nil
  "List used to collect file names, which are checked during `vc-registered'.")

;; VC backends check for the existence of various different special
;; files.  This is very time consuming, because every single check
;; requires a remote command (the file cache must be invalidated).
;; Therefore, we apply a kind of optimization.  We install the file
;; name handler `tramp-vc-file-name-handler', which does nothing but
;; remembers all file names for which `file-exists-p' or
;; `file-readable-p' has been applied.  A first run of `vc-registered'
;; is performed.  Afterwards, a script is applied for all collected
;; file names, using just one remote command.  The result of this
;; script is used to fill the file cache with actual values.  Now we
;; can reset the file name handlers, and we make a second run of
;; `vc-registered', which returns the expected result without sending
;; any other remote command.
;; When called during `revert-buffer', it shouldn't spam the echo area
;; and the *Messages* buffer.
(defun tramp-sh-handle-vc-registered (file)
  "Like `vc-registered' for Tramp files."
  (when vc-handled-backends
    (let ((inhibit-message (or revert-buffer-in-progress-p inhibit-message))
	  (temp-message (unless revert-buffer-in-progress-p "")))
      (with-temp-message temp-message
	(with-parsed-tramp-file-name file nil
          (with-tramp-progress-reporter
	      v 3 (format-message "Checking `vc-registered' for %s" file)

	    ;; There could be new files, created by the vc backend.
	    ;; We cannot reuse the old cache entries, therefore.  In
	    ;; `tramp-get-file-property', `remote-file-name-inhibit-cache'
	    ;; could also be a timestamp as `current-time' returns.  This
	    ;; means invalidate all cache entries with an older timestamp.
	    (let (tramp-vc-registered-file-names
	          (remote-file-name-inhibit-cache (current-time))
	          (file-name-handler-alist
	           `((,tramp-file-name-regexp . tramp-vc-file-name-handler))))

	      ;; Here we collect only file names, which need an operation.
	      (tramp-with-demoted-errors
	          v "Error in 1st pass of `vc-registered': %s"
		(tramp-run-real-handler #'vc-registered (list file)))
	      (tramp-message v 10 "\n%s" tramp-vc-registered-file-names)

	      ;; Send just one command, in order to fill the cache.
	      (when tramp-vc-registered-file-names
		(tramp-maybe-send-script
		 v
		 (format tramp-vc-registered-read-file-names
			 (tramp-get-file-exists-command v)
			 (format "%s -r" (tramp-get-test-command v)))
		 "tramp_vc_registered_read_file_names")

		(dolist
		    (elt
		     (ignore-errors
		       ;; We cannot use `tramp-send-command-and-read',
		       ;; because this does not cooperate well with
		       ;; heredoc documents.
		       (tramp-send-command
			v
			(format
			 "tramp_vc_registered_read_file_names <<'%s'\n%s\n%s\n"
			 tramp-end-of-heredoc
			 (mapconcat #'tramp-shell-quote-argument
				    tramp-vc-registered-file-names
				    "\n")
			 tramp-end-of-heredoc))
		       (with-current-buffer (tramp-get-connection-buffer v)
			 ;; Read the expression.
			 (goto-char (point-min))
			 (read (current-buffer)))))

	          (tramp-set-file-property
	           v (car elt) (cadr elt) (cadr (cdr elt))))))

	    ;; Second run.  Now all `file-exists-p' or `file-readable-p'
	    ;; calls shall be answered from the file cache.  We unset
	    ;; `process-file-side-effects' and `remote-file-name-inhibit-cache'
	    ;; in order to keep the cache.
	    (let ((vc-handled-backends (copy-sequence vc-handled-backends))
	          remote-file-name-inhibit-cache process-file-side-effects)
	      ;; Reduce `vc-handled-backends' in order to minimize
	      ;; process calls.
	      (when (and
		     (memq 'Bzr vc-handled-backends)
		     (or (not (require 'vc-bzr nil 'noerror))
			 (not (with-tramp-connection-property v vc-bzr-program
				(tramp-find-executable
				 v vc-bzr-program (tramp-get-remote-path v))))))
		(setq vc-handled-backends (remq 'Bzr vc-handled-backends)))
	      (when (and
		     (memq 'Git vc-handled-backends)
		     (or (not (require 'vc-git nil 'noerror))
			 (not (with-tramp-connection-property v vc-git-program
				(tramp-find-executable
				 v vc-git-program (tramp-get-remote-path v))))))
		(setq vc-handled-backends (remq 'Git vc-handled-backends)))
	      (when (and
		     (memq 'Hg vc-handled-backends)
		     (or (not (require 'vc-hg nil 'noerror))
			 (not (with-tramp-connection-property v vc-hg-program
				(tramp-find-executable
				 v vc-hg-program (tramp-get-remote-path v))))))
		(setq vc-handled-backends (remq 'Hg vc-handled-backends)))
	      ;; Run.
	      (tramp-with-demoted-errors
	          v "Error in 2nd pass of `vc-registered': %s"
		(tramp-run-real-handler #'vc-registered (list file))))))))))

;;;###tramp-autoload
(defun tramp-sh-file-name-handler (operation &rest args)
  "Invoke remote-shell Tramp file name handler.
Fall back to normal file name handler if no Tramp handler exists."
  (if-let ((fn (assoc operation tramp-sh-file-name-handler-alist)))
      (save-match-data (apply (cdr fn) args))
    (tramp-run-real-handler operation args)))

;;;###tramp-autoload
(defun tramp-sh-file-name-handler-p (vec)
  "Whether VEC uses a method from `tramp-sh-file-name-handler'."
  (and (assoc (tramp-file-name-method vec) tramp-methods)
       (eq (tramp-find-foreign-file-name-handler
	    (tramp-make-tramp-file-name vec nil 'nohop))
	   'tramp-sh-file-name-handler)))

;; This must be the last entry, because `identity' always matches.
;;;###tramp-autoload
(tramp--with-startup
 (tramp-register-foreign-file-name-handler
  #'identity #'tramp-sh-file-name-handler 'append))

(defun tramp-vc-file-name-handler (operation &rest args)
  "Invoke special file name handler, which collects files to be handled."
  (save-match-data
    (let ((filename
	   (tramp-replace-environment-variables
	    (apply #'tramp-file-name-for-operation operation args)))
	  (fn (assoc operation tramp-sh-file-name-handler-alist)))
      (if (tramp-tramp-file-p filename)
	  (with-parsed-tramp-file-name filename nil
	    (cond
	     ;; That's what we want: file names, for which checks are
	     ;; applied.  We assume that VC uses only `file-exists-p'
	     ;; and `file-readable-p' checks; otherwise we must extend
	     ;; the list.  We do not perform any action, but return
	     ;; nil, in order to keep `vc-registered' running.
	     ((and fn (memq operation '(file-exists-p file-readable-p)))
	      (add-to-list 'tramp-vc-registered-file-names localname 'append)
	      nil)
	     ;; `process-file' and `start-file-process' shall be ignored.
	     ((and fn (eq operation 'process-file) 0))
	     ((and fn (eq operation 'start-file-process) nil))
	     ;; Tramp file name handlers like `expand-file-name'.  They
	     ;; must still work.
	     (fn (save-match-data (apply (cdr fn) args)))
	     ;; Default file name handlers, we don't care.
	     (t (tramp-run-real-handler operation args))))

	;; When `tramp-mode' is not enabled, or the file name is
	;; quoted, we don't do anything.
	(tramp-run-real-handler operation args)))))

(defun tramp-sh-handle-file-notify-add-watch (file-name flags _callback)
  "Like `file-notify-add-watch' for Tramp files."
  (setq file-name (expand-file-name file-name))
  (with-parsed-tramp-file-name file-name nil
    (let ((default-directory (file-name-directory file-name))
          (process-environment
           (cons "GIO_USE_FILE_MONITOR=help" process-environment))
	  command events filter p sequence)
      (cond
       ;; "inotifywait".
       ((setq command (tramp-get-remote-inotifywait v))
	(setq filter #'tramp-sh-inotifywait-process-filter
	      events
	      (cond
	       ((and (memq 'change flags) (memq 'attribute-change flags))
		(concat "create,modify,move,moved_from,moved_to,move_self,"
			"delete,delete_self,attrib,ignored"))
	       ((memq 'change flags)
		(concat "create,modify,move,moved_from,moved_to,move_self,"
			"delete,delete_self,ignored"))
	       ((memq 'attribute-change flags) "attrib,ignored"))
	      sequence `(,command "-mq" "-e" ,events ,localname)
	      ;; Make events a list of symbols.
	      events
	      (mapcar
	       (lambda (x) (intern-soft (tramp-compat-string-replace "_" "-" x)))
	       (split-string events "," 'omit))))
       ;; "gio monitor".
       ((setq command (tramp-get-remote-gio-monitor v))
	(setq filter #'tramp-sh-gio-monitor-process-filter
	      events
	      (cond
	       ((and (memq 'change flags) (memq 'attribute-change flags))
		'(created changed changes-done-hint moved deleted
			  attribute-changed))
	       ((memq 'change flags)
		'(created changed changes-done-hint moved deleted))
	       ((memq 'attribute-change flags) '(attribute-changed)))
	      sequence `(,command "monitor" ,localname)))
       ;; None.
       (t (tramp-error
	   v 'file-notify-error
	   "No file notification program found on %s"
	   (file-remote-p file-name))))
      ;; Start process.
      (setq p (apply
	       #'start-file-process
	       (file-name-nondirectory command)
	       (generate-new-buffer
		(format " *%s*" (file-name-nondirectory command)))
	       sequence))
      ;; Return the process object as watch-descriptor.
      (if (not (processp p))
	  (tramp-error
	   v 'file-notify-error
	   "`%s' failed to start on remote host"
	   (string-join sequence " "))
	(tramp-message v 6 "Run `%s', %S" (string-join sequence " ") p)
	(process-put p 'vector v)
	;; Needed for process filter.
	(process-put p 'events events)
	(process-put p 'watch-name localname)
	(set-process-query-on-exit-flag p nil)
	(set-process-filter p filter)
	(set-process-sentinel p #'tramp-file-notify-process-sentinel)
	;; There might be an error if the monitor is not supported.
	;; Give the filter a chance to read the output.
	(while (tramp-accept-process-output p 0))
	(unless (process-live-p p)
	  (tramp-error
	   p 'file-notify-error "Monitoring not supported for `%s'" file-name))
	p))))

(defun tramp-sh-gio-monitor-process-filter (proc string)
  "Read output from \"gio monitor\" and add corresponding `file-notify' events."
  (let ((events (process-get proc 'events))
	(remote-prefix
	 (with-current-buffer (process-buffer proc)
	   (file-remote-p default-directory)))
	(rest-string (process-get proc 'rest-string))
	pos)
    (when rest-string
      (tramp-message proc 10 "Previous string:\n%s" rest-string))
    (tramp-message proc 6 "%S\n%s" proc string)
    (setq string (concat rest-string string)
          ;; Fix action names.
          string (tramp-compat-string-replace
	          "attributes changed" "attribute-changed" string)
          string (tramp-compat-string-replace
	          "changes done" "changes-done-hint" string)
          string (tramp-compat-string-replace
	          "renamed to" "moved" string))

    (catch 'doesnt-work
      ;; https://bugs.launchpad.net/bugs/1742946
      (when
          (string-match-p "Monitoring not supported\\|No locations given" string)
        (delete-process proc)
        (throw 'doesnt-work nil))

      ;; Determine monitor name.
      (unless (tramp-connection-property-p proc "gio-file-monitor")
        (tramp-set-connection-property
         proc "gio-file-monitor"
         (cond
          ;; We have seen this on cygwin gio and on emba.  Let's make
          ;; some assumptions.
          ((string-match
            "Can't find module 'help' specified in GIO_USE_FILE_MONITOR" string)
	   (setq pos (match-end 0))
           (cond
            ((getenv "EMACS_EMBA_CI") 'GInotifyFileMonitor)
            ((eq system-type 'cygwin) 'GPollFileMonitor)
            (t nil)))
          ;; TODO: What happens, if several monitor names are reported?
          ((string-match "\
Supported arguments for GIO_USE_FILE_MONITOR environment variable:
\\s-*\\([[:alpha:]]+\\) - 20" string)
	   (setq pos (match-end 0))
           (intern
	    (format "G%sFileMonitor" (capitalize (match-string 1 string)))))
          (t (setq pos (length string)) nil)))
	(setq string (substring string pos)))

      ;; Delete empty lines.
      (setq string (tramp-compat-string-replace "\n\n" "\n" string))

      (while (string-match
	      (eval-when-compile
	        (concat "^[^:]+:"
		        "[[:space:]]\\([^:]+\\):"
		        "[[:space:]]" (regexp-opt tramp-gio-events t)
		        "\\([[:space:]]\\([^:]+\\)\\)?$"))
	      string)

        (let* ((file (match-string 1 string))
	       (file1 (match-string 4 string))
	       (object
	        (list
	         proc
	         (list
		  (intern-soft (match-string 2 string)))
	         ;; File names are returned as absolute paths.  We
	         ;; must add the remote prefix.
	         (concat remote-prefix file)
	         (when file1 (concat remote-prefix file1)))))
	  (setq string (replace-match "" nil nil string))
	  ;; Usually, we would add an Emacs event now.  Unfortunately,
	  ;; `unread-command-events' does not accept several events at
	  ;; once.  Therefore, we apply the handler directly.
	  (when (member (cl-caadr object) events)
	    (tramp-compat-funcall
	     (lookup-key special-event-map [file-notify])
	     `(file-notify ,object file-notify-callback))))))

    ;; Save rest of the string.
    (while (string-match "^\n" string)
      (setq string (replace-match "" nil nil string)))
    (when (zerop (length string)) (setq string nil))
    (when string (tramp-message proc 10 "Rest string:\n%s" string))
    (process-put proc 'rest-string string)))

(defun tramp-sh-inotifywait-process-filter (proc string)
  "Read output from \"inotifywait\" and add corresponding `file-notify' events."
  (let ((events (process-get proc 'events)))
    (tramp-message proc 6 "%S\n%s" proc string)
    (dolist (line (split-string string "[\n\r]+" 'omit))
      ;; Check, whether there is a problem.
      (unless (string-match
	       (concat "^[^[:blank:]]+"
		       "[[:blank:]]+\\([^[:blank:]]+\\)"
		       "\\([[:blank:]]+\\([^\n\r]+\\)\\)?")
	       line)
	(tramp-error proc 'file-notify-error line))

      (let ((object
	     (list
	      proc
	      (mapcar
	       (lambda (x)
		 (intern-soft
		  (tramp-compat-string-replace "_" "-" (downcase x))))
	       (split-string (match-string 1 line) "," 'omit))
	      (or (match-string 3 line)
		  (file-name-nondirectory (process-get proc 'watch-name))))))
	;; Usually, we would add an Emacs event now.  Unfortunately,
	;; `unread-command-events' does not accept several events at
	;; once.  Therefore, we apply the handler directly.
	(when (member (cl-caadr object) events)
	  (tramp-compat-funcall
	   (lookup-key special-event-map [file-notify])
	   `(file-notify ,object file-notify-callback)))))))

(defun tramp-sh-handle-file-system-info (filename)
  "Like `file-system-info' for Tramp files."
  (ignore-errors
    (with-parsed-tramp-file-name (expand-file-name filename) nil
      (when (tramp-get-remote-df v)
	(tramp-message v 5 "file system info: %s" localname)
	(tramp-send-command
	 v (format
	    "%s %s"
	    (tramp-get-remote-df v) (tramp-shell-quote-argument localname)))
	(with-current-buffer (tramp-get-connection-buffer v)
	  (goto-char (point-min))
	  (forward-line)
	  (when (looking-at
		 (concat "\\(?:^/[^[:space:]]*[[:space:]]\\)?"
			 "[[:space:]]*\\([[:digit:]]+\\)"
			 "[[:space:]]+\\([[:digit:]]+\\)"
			 "[[:space:]]+\\([[:digit:]]+\\)"))
	    (mapcar
	     (lambda (d)
	       (* d (tramp-get-connection-property v "df-blocksize" 0)))
	     (list (string-to-number (match-string 1))
		   ;; The second value is the used size.  We need the
		   ;; free size.
		   (- (string-to-number (match-string 1))
		      (string-to-number (match-string 2)))
		   (string-to-number (match-string 3))))))))))

;;; Internal Functions:

(defun tramp-expand-script (vec script)
  "Expand SCRIPT with remote files or commands.
\"%a\", \"%h\", \"%o\" and \"%p\" format specifiers are replaced
by the respective `awk', `hexdump', `od' and `perl' commands.
\"%n\" is replaced by \"2>/dev/null\", and \"%t\" is replaced by
a temporary file name.
If VEC is nil, the respective local commands are used.
If there is a format specifier which cannot be expanded, this
function returns nil."
  (if (not (string-match-p "\\(^\\|[^%]\\)%[ahnopt]" script))
      script
    (catch 'wont-work
      (let ((awk (when (string-match-p "\\(^\\|[^%]\\)%a" script)
		   (or
		    (if vec (tramp-get-remote-awk vec) (executable-find "awk"))
		    (throw 'wont-work nil))))
	    (hdmp (when (string-match-p "\\(^\\|[^%]\\)%h" script)
		    (or
		     (if vec (tramp-get-remote-hexdump vec)
		       (executable-find "hexdump"))
		     (throw 'wont-work nil))))
	    (dev (when (string-match-p "\\(^\\|[^%]\\)%n" script)
		   (or
		    (if vec (concat "2>" (tramp-get-remote-null-device vec))
		      (if (eq system-type 'windows-nt) ""
			(concat "2>" null-device)))
		    (throw 'wont-work nil))))
	    (od (when (string-match-p "\\(^\\|[^%]\\)%o" script)
		  (or (if vec (tramp-get-remote-od vec) (executable-find "od"))
		      (throw 'wont-work nil))))
	    (perl (when (string-match-p "\\(^\\|[^%]\\)%p" script)
		    (or
		     (if vec
			 (tramp-get-remote-perl vec) (executable-find "perl"))
		     (throw 'wont-work nil))))
	    (tmp (when (string-match-p "\\(^\\|[^%]\\)%t" script)
		   (or
		    (if vec
			(tramp-file-local-name (tramp-make-tramp-temp-name vec))
		      (tramp-compat-make-temp-name))
		    (throw 'wont-work nil)))))
	(format-spec
	 script
	 (format-spec-make ?a awk ?h hdmp ?n dev ?o od ?p perl ?t tmp))))))

(defun tramp-maybe-send-script (vec script name)
  "Define in remote shell function NAME implemented as SCRIPT.
Only send the definition if it has not already been done."
  ;; We cannot let-bind (tramp-get-connection-process vec) because it
  ;; might be nil.
  (let ((scripts (tramp-get-connection-property
		  (tramp-get-connection-process vec) "scripts" nil)))
    (unless (member name scripts)
      (with-tramp-progress-reporter
	  vec 5 (format-message "Sending script `%s'" name)
	;; In bash, leading TABs like in `tramp-vc-registered-read-file-names'
	;; could result in unwanted command expansion.  Avoid this.
	(setq script (tramp-compat-string-replace
		      (make-string 1 ?\t) (make-string 8 ? ) script))
	;; Expand format specifiers.
	(unless (setq script (tramp-expand-script vec script))
	  (tramp-error
	   vec 'file-error
	   (format "Script %s is not applicable on remote host" name)))
	;; Send it.
	(tramp-barf-unless-okay
	 vec
	 (format "%s () {\n%s\n}" name script)
	 "Script %s sending failed" name)
	(tramp-set-connection-property
	 (tramp-get-connection-process vec) "scripts" (cons name scripts))))))

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

(defconst tramp-sunos-unames (regexp-opt '("SunOS 5.10" "SunOS 5.11"))
  "Regexp to determine remote SunOS.")

(defun tramp-find-executable
  (vec progname dirlist &optional ignore-tilde ignore-path)
  "Search for PROGNAME in $PATH and all directories mentioned in DIRLIST.
First arg VEC specifies the connection, PROGNAME is the program
to search for, and DIRLIST gives the list of directories to
search.  If IGNORE-TILDE is non-nil, directory names starting
with \"~\" will be ignored.  If IGNORE-PATH is non-nil, searches
only in DIRLIST.

Returns the absolute file name of PROGNAME, if found, and nil otherwise.

This function expects to be in the right *tramp* buffer."
  (with-current-buffer (tramp-get-connection-buffer vec)
    (let (result)
      ;; Check whether the executable is in $PATH.  "which(1)" does not
      ;; report always a correct error code; therefore we check the
      ;; number of words it returns.  "SunOS 5.10" (and maybe "SunOS
      ;; 5.11") have problems with this command, we disable the call
      ;; therefore.
      (unless (or ignore-path (tramp-check-remote-uname vec tramp-sunos-unames))
	(tramp-send-command vec (format "which \\%s | wc -w" progname))
	(goto-char (point-min))
	(if (looking-at-p "^\\s-*1$")
	    (setq result (concat "\\" progname))))
      (unless result
	(when ignore-tilde
	  ;; Remove all ~/foo directories from dirlist.
	  (let (newdl d)
	    (while dirlist
	      (setq d (car dirlist)
		    dirlist (cdr dirlist))
	      (unless (char-equal ?~ (aref d 0))
		(setq newdl (cons d newdl))))
	    (setq dirlist (nreverse newdl))))
	(tramp-send-command
	 vec
	 (format (concat "while read d; "
			 "do if test -x $d/%s && test -f $d/%s; "
			 "then echo tramp_executable $d/%s; "
			 "break; fi; done <<'%s'\n"
			 "%s\n%s")
		 progname progname progname
		 tramp-end-of-heredoc
		 (string-join dirlist "\n")
		 tramp-end-of-heredoc))
	(goto-char (point-max))
	(when (search-backward "tramp_executable " nil t)
	  (skip-chars-forward "^ ")
	  (skip-chars-forward " ")
	  (setq result (buffer-substring (point) (point-at-eol)))))
    result)))

;; On hydra.nixos.org, the $PATH environment variable is too long to
;; send it.  This is likely not due to PATH_MAX, but PIPE_BUF.  We
;; check it, and use a temporary file in case of.  See Bug#33781.
(defun tramp-set-remote-path (vec)
  "Set the remote environment PATH to existing directories.
I.e., for each directory in `tramp-remote-path', it is tested
whether it exists and if so, it is added to the environment
variable PATH."
  (let ((command
	 (format
	  "PATH=%s && export PATH" (string-join (tramp-get-remote-path vec) ":")))
	(pipe-buf
	 (with-tramp-connection-property vec "pipe-buf"
	   (tramp-send-command-and-read
	    vec
            (format "getconf PIPE_BUF / 2>%s || echo 4096"
                    (tramp-get-remote-null-device vec))
            'noerror)))
	tmpfile chunk chunksize)
    (tramp-message vec 5 "Setting $PATH environment variable")
    (if (< (length command) pipe-buf)
	(tramp-send-command vec command)
      ;; Use a temporary file.  We cannot use `write-region' because
      ;; setting the remote path happens in the early connection
      ;; handshake, and not all external tools are determined yet.
      (setq command (concat command "\n")
	    tmpfile (tramp-make-tramp-temp-file vec))
      (while (not (string-empty-p command))
	(setq chunksize (min (length command) (/ pipe-buf 2))
	      chunk (substring command 0 chunksize)
	      command (substring command chunksize))
	(tramp-send-command vec (format
				 "printf \"%%b\" \"$*\" %s >>%s"
				 (tramp-shell-quote-argument chunk)
				 (tramp-shell-quote-argument tmpfile))))
      (tramp-send-command vec (format ". %s" tmpfile))
      (tramp-send-command vec (format "rm -f %s" tmpfile)))))

;; ------------------------------------------------------------
;; -- Communication with external shell --
;; ------------------------------------------------------------

(defun tramp-find-file-exists-command (vec)
  "Find a command on the remote host for checking if a file exists.
Here, we are looking for a command which has zero exit status if the
file exists and nonzero exit status otherwise."
  (let ((existing "/")
        (nonexistent
	 (tramp-shell-quote-argument "/ this file does not exist "))
	result)
    ;; The algorithm is as follows: we try a list of several commands.
    ;; For each command, we first run `$cmd /' -- this should return
    ;; true, as the root directory always exists.  And then we run
    ;; `$cmd /\ this\ file\ does\ not\ exist\ ', hoping that the file
    ;; indeed does not exist.  This should return false.  We use the
    ;; first command we find that seems to work.
    ;; The list of commands to try is as follows:
    ;; `test -e'          Some Bourne shells have a `test' builtin
    ;;                    which does not know the `-e' option.
    ;; `/bin/test -e'     For those, the `test' binary on disk normally
    ;;                    provides the option.  Alas, the binary
    ;;                    is sometimes `/bin/test' and sometimes it's
    ;;                    `/usr/bin/test'.
    ;; `/usr/bin/test -e' In case `/bin/test' does not exist.
    ;; `ls -d'            This works on most systems, but NetBSD 1.4
    ;;                    has a bug: `ls' always returns zero exit
    ;;                    status, even for files which don't exist.

    (unless (or
	     (ignore-errors
	       (and (setq result (format "%s -e" (tramp-get-test-command vec)))
		    (tramp-send-command-and-check
		     vec (format "%s %s" result existing))
		    (not (tramp-send-command-and-check
			  vec (format "%s %s" result nonexistent)))))
	     (ignore-errors
	       (and (setq result "/bin/test -e")
		    (tramp-send-command-and-check
		     vec (format "%s %s" result existing))
		    (not (tramp-send-command-and-check
			  vec (format "%s %s" result nonexistent)))))
	     (ignore-errors
	       (and (setq result "/usr/bin/test -e")
		    (tramp-send-command-and-check
		     vec (format "%s %s" result existing))
		    (not (tramp-send-command-and-check
			  vec (format "%s %s" result nonexistent)))))
	     (ignore-errors
	       (and (setq result (format "%s -d" (tramp-get-ls-command vec)))
		    (tramp-send-command-and-check
		     vec (format "%s %s" result existing))
		    (not (tramp-send-command-and-check
			  vec (format "%s %s" result nonexistent))))))
      (tramp-error
       vec 'file-error "Couldn't find command to check if file exists"))
    result))

(defun tramp-get-sh-extra-args (shell)
  "Find extra args for SHELL."
  (let ((alist tramp-sh-extra-args)
	item extra-args)
    (while (and alist (null extra-args))
      (setq item (pop alist))
      (when (string-match-p (car item) shell)
	(setq extra-args (cdr item))))
    extra-args))

(defun tramp-open-shell (vec shell)
  "Open shell SHELL."
  ;; Find arguments for this shell.
  (with-tramp-progress-reporter
      vec 5 (format-message "Opening remote shell `%s'" shell)
    ;; It is useful to set the prompt in the following command because
    ;; some people have a setting for $PS1 which /bin/sh doesn't know
    ;; about and thus /bin/sh will display a strange prompt.  For
    ;; example, if $PS1 has "${CWD}" in the value, then ksh will
    ;; display the current working directory but /bin/sh will display
    ;; a dollar sign.  The following command line sets $PS1 to a sane
    ;; value, and works under Bourne-ish shells as well as csh-like
    ;; shells.  We also unset the variable $ENV because that is read
    ;; by some sh implementations (eg, bash when called as sh) on
    ;; startup; this way, we avoid the startup file clobbering $PS1.
    ;; $PROMPT_COMMAND is another way to set the prompt in /bin/bash,
    ;; it must be discarded as well.  $HISTFILE is set according to
    ;; `tramp-histfile-override'.  $TERM and $INSIDE_EMACS set here to
    ;; ensure they have the correct values when the shell starts, not
    ;; just processes run within the shell.  (Which processes include
    ;; our initial probes to ensure the remote shell is usable.)
    ;; For the time being, we assume that all shells interpret -i as
    ;; interactive shell.  Must be the last argument, because (for
    ;; example) bash expects long options first.
    (tramp-send-command
     vec (format
	  (concat
	   "exec env TERM='%s' INSIDE_EMACS='%s' "
	   "ENV=%s %s PROMPT_COMMAND='' PS1=%s PS2='' PS3='' %s %s -i")
          tramp-terminal-type (tramp-inside-emacs)
          (or (getenv-internal "ENV" tramp-remote-process-environment) "")
	  (if (stringp tramp-histfile-override)
	      (format "HISTFILE=%s"
		      (tramp-shell-quote-argument tramp-histfile-override))
	    (if tramp-histfile-override
		"HISTFILE='' HISTFILESIZE=0 HISTSIZE=0"
	      ""))
	  (tramp-shell-quote-argument tramp-end-of-output)
	  shell (or (tramp-get-sh-extra-args shell) ""))
     t)
    ;; Check proper HISTFILE setting.  We give up when not working.
    (when (and (stringp tramp-histfile-override)
	       (file-name-directory tramp-histfile-override))
      (tramp-barf-unless-okay
       vec
       (format
	"(cd %s)"
	(tramp-shell-quote-argument
	 (file-name-directory tramp-histfile-override)))
       "`tramp-histfile-override' uses invalid file `%s'"
       tramp-histfile-override))

    (tramp-set-connection-property
     (tramp-get-connection-process vec) "remote-shell" shell)))

(defun tramp-find-shell (vec)
  "Open a shell on the remote host which groks tilde expansion."
  ;; If we are in `make-process', we don't need another shell.
  (unless (tramp-get-connection-property vec "process-name" nil)
    (with-current-buffer (tramp-get-buffer vec)
      (let ((default-shell (tramp-get-method-parameter vec 'tramp-remote-shell))
	    shell)
	(setq shell
	      (with-tramp-connection-property vec "remote-shell"
		;; CCC: "root" does not exist always, see my QNAP
		;; TS-459.  Which check could we apply instead?
		(tramp-send-command vec "echo ~root" t)
		(if (or (string-match-p "^~root$" (buffer-string))
			;; The default shell (ksh93) of OpenSolaris
			;; and Solaris is buggy.  We've got reports
			;; for "SunOS 5.10" and "SunOS 5.11" so far.
                        (tramp-check-remote-uname vec tramp-sunos-unames))

		    (or (tramp-find-executable
			 vec "bash" (tramp-get-remote-path vec) t t)
			(tramp-find-executable
			 vec "ksh" (tramp-get-remote-path vec) t t)
			;; Maybe it works at least for some other commands.
			(prog1
			    default-shell
			  (tramp-message
			   vec 2
			   (concat
			    "Couldn't find a remote shell which groks tilde "
			    "expansion, using `%s'")
			   default-shell)))

		  default-shell)))

	;; Open a new shell if needed.
	(unless (string-equal shell default-shell)
	  (tramp-message
	   vec 5 "Starting remote shell `%s' for tilde expansion" shell)
	  (tramp-open-shell vec shell))))))

;; Utility functions.

(defun tramp-barf-if-no-shell-prompt (proc timeout &rest error-args)
  "Wait for shell prompt and barf if none appears.
Looks at process PROC to see if a shell prompt appears in TIMEOUT
seconds.  If not, it produces an error message with the given ERROR-ARGS."
  (let ((vec (process-get proc 'vector)))
    (condition-case nil
	(tramp-wait-for-regexp
	 proc timeout
	 (format
	  "\\(%s\\|%s\\)\\'" shell-prompt-pattern tramp-shell-prompt-pattern))
      (error
       (delete-process proc)
       (apply #'tramp-error-with-buffer
	      (tramp-get-connection-buffer vec) vec 'file-error error-args)))))

(defun tramp-open-connection-setup-interactive-shell (proc vec)
  "Set up an interactive shell.
Mainly sets the prompt and the echo correctly.  PROC is the shell
process to set up.  VEC specifies the connection."
  (let ((tramp-end-of-output tramp-initial-end-of-output)
	(case-fold-search t))
    (tramp-open-shell vec (tramp-get-method-parameter vec 'tramp-remote-shell))
    (tramp-message vec 5 "Setting up remote shell environment")

    ;; Disable line editing.
    (tramp-send-command vec "set +o vi +o emacs" t)

    ;; Dump option settings in the traces.
    (when (>= tramp-verbose 9)
      (tramp-send-command vec "set -o" t))

    ;; Disable echo expansion.
    (tramp-send-command
     vec "stty -inlcr -onlcr -echo kill '^U' erase '^H'" t)
    ;; Check whether the echo has really been disabled.  Some
    ;; implementations, like busybox of embedded GNU/Linux, don't
    ;; support disabling.
    (tramp-send-command vec "echo foo" t)
    (with-current-buffer (process-buffer proc)
      (goto-char (point-min))
      (when (looking-at-p "echo foo")
	(tramp-set-connection-property proc "remote-echo" t)
	(tramp-message vec 5 "Remote echo still on. Ok.")
	;; Make sure backspaces and their echo are enabled and no line
	;; width magic interferes with them.
	(tramp-send-command vec "stty icanon erase ^H cols 32767" t))))

  (tramp-message vec 5 "Setting shell prompt")
  (tramp-send-command
   vec (format "PS1=%s PS2='' PS3='' PROMPT_COMMAND=''"
	       (tramp-shell-quote-argument tramp-end-of-output))
   t)

  ;; Check whether the output of "uname -sr" has been changed.  If
  ;; yes, this is a strong indication that we must expire all
  ;; connection properties.  We start again with
  ;; `tramp-maybe-open-connection', it will be caught there.
  (tramp-message vec 5 "Checking system information")
  (let* ((old-uname (tramp-get-connection-property vec "uname" nil))
	 (uname
	  ;; If we are in `make-process', we don't need to recompute.
	  (if (and old-uname
		   (tramp-get-connection-property vec "process-name" nil))
	      old-uname
	    (tramp-set-connection-property
	     vec "uname"
	     (tramp-send-command-and-read vec "echo \\\"`uname -sr`\\\"")))))
    (when (and (stringp old-uname) (not (string-equal old-uname uname)))
      (tramp-message
       vec 3
       "Connection reset, because remote host changed from `%s' to `%s'"
       old-uname uname)
      ;; We want to keep the password.
      (tramp-cleanup-connection vec t t)
      (throw 'uname-changed (tramp-maybe-open-connection vec)))

    ;; Try to set up the coding system correctly.
    ;; CCC this can't be the right way to do it.  Hm.
    (tramp-message vec 5 "Determining coding system")
    (with-current-buffer (process-buffer proc)
      ;; Use MULE to select the right EOL convention for communicating
      ;; with the process.
      (let ((cs (or (and (memq 'utf-8-hfs (coding-system-list))
			 (string-prefix-p "Darwin" uname)
			 (cons 'utf-8-hfs 'utf-8-hfs))
		    (and (memq 'utf-8 (coding-system-list))
			 (string-match-p "utf-?8" (tramp-get-remote-locale vec))
			 (cons 'utf-8 'utf-8))
		    (process-coding-system proc)
		    (cons 'undecided 'undecided)))
	    cs-decode cs-encode)
	(when (symbolp cs) (setq cs (cons cs cs)))
	(setq cs-decode (or (car cs) 'undecided)
	      cs-encode (or (cdr cs) 'undecided)
	      cs-encode
	      (coding-system-change-eol-conversion
	       cs-encode (if (string-prefix-p "Darwin" uname) 'mac 'unix)))
	(tramp-send-command vec "(echo foo ; echo bar)" t)
	(goto-char (point-min))
	(when (search-forward "\r" nil t)
	  (setq cs-decode (coding-system-change-eol-conversion cs-decode 'dos)))
	(set-process-coding-system proc cs-decode cs-encode)
	(tramp-message
	 vec 5 "Setting coding system to `%s' and `%s'" cs-decode cs-encode)))

    ;; Check whether the remote host suffers from buggy
    ;; `send-process-string'.  This is known for FreeBSD (see comment
    ;; in `send_process', file process.c).  I've tested sending 624
    ;; bytes successfully, sending 625 bytes failed.  Emacs makes a
    ;; hack when this host type is detected locally.  It cannot handle
    ;; remote hosts, though.
    (with-tramp-connection-property proc "chunksize"
      (cond
       ((and (integerp tramp-chunksize) (> tramp-chunksize 0))
	tramp-chunksize)
       (t
	(tramp-message
	 vec 5 "Checking remote host type for `send-process-string' bug")
	(if (string-match-p "FreeBSD\\|DragonFly" uname) 500 0))))

    ;; Set remote PATH variable.
    (tramp-set-remote-path vec)

    ;; Search for a good shell before searching for a command which
    ;; checks if a file exists.  This is done because Tramp wants to
    ;; use "test foo; echo $?" to check if various conditions hold,
    ;; and there are buggy /bin/sh implementations which don't execute
    ;; the "echo $?"  part if the "test" part has an error.  In
    ;; particular, the OpenSolaris /bin/sh is a problem.  There are
    ;; also other problems with /bin/sh of OpenSolaris, like
    ;; redirection of stderr in function declarations, or changing
    ;; HISTFILE in place.  Therefore, OpenSolaris' /bin/sh is replaced
    ;; by bash, when detected.
    (tramp-find-shell vec)

    ;; Disable unexpected output.
    (tramp-send-command
     vec
     (format "mesg n 2>%s; biff n 2>%s"
             (tramp-get-remote-null-device vec)
             (tramp-get-remote-null-device vec))
     t)

    ;; IRIX64 bash expands "!" even when in single quotes.  This
    ;; destroys our shell functions, we must disable it.  See
    ;; <https://stackoverflow.com/questions/3291692/irix-bash-shell-expands-expression-in-single-quotes-yet-shouldnt>.
    (when (string-prefix-p "IRIX64" uname)
      (tramp-send-command vec "set +H" t))

    ;; Disable tab expansion.
    (if (string-match-p "BSD\\|DragonFly\\|Darwin" uname)
	(tramp-send-command vec "stty tabs" t)
      (tramp-send-command vec "stty tab0" t))

    ;; Set utf8 encoding.  Needed for macOS, for example.  This is
    ;; non-POSIX, so we must expect errors on some systems.
    (tramp-send-command
     vec (concat "stty iutf8 2>" (tramp-get-remote-null-device vec)) t)

    ;; Set `remote-tty' process property.
    (let ((tty (tramp-send-command-and-read vec "echo \\\"`tty`\\\"" 'noerror)))
      (unless (zerop (length tty))
	(process-put proc 'remote-tty tty)
	(tramp-set-connection-property proc "remote-tty" tty)))

    ;; Dump stty settings in the traces.
    (when (>= tramp-verbose 9)
      (tramp-send-command vec "stty -a" t))

    ;; Set the environment.
    (tramp-message vec 5 "Setting default environment")

    (let (unset vars)
      (dolist (item (reverse
		     (append `(,(tramp-get-remote-locale vec))
			     (copy-sequence tramp-remote-process-environment))))
	(setq item (split-string item "=" 'omit))
	(setcdr item (string-join (cdr item) "="))
	(if (and (stringp (cdr item)) (not (string-equal (cdr item) "")))
	    (push (format "%s %s" (car item) (cdr item)) vars)
	  (push (car item) unset)))
      (when vars
	(tramp-send-command
	 vec
	 (format
	  "while read var val; do export $var=\"$val\"; done <<'%s'\n%s\n%s"
	  tramp-end-of-heredoc
	  (string-join vars "\n")
	  tramp-end-of-heredoc)
	 t))
      (when unset
	(tramp-send-command
	 vec (format "unset %s" (string-join unset " ")) t)))))

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

(autoload 'uudecode-decode-region "uudecode")

(defconst tramp-local-coding-commands
  `((b64 base64-encode-region base64-decode-region)
    (uu  tramp-uuencode-region uudecode-decode-region)
    (pack ,tramp-perl-pack ,tramp-perl-unpack))
  "List of local coding commands for inline transfer.
Each item is a list that looks like this:

\(FORMAT ENCODING DECODING)

FORMAT is a symbol describing the encoding/decoding format.  It can be
`b64' for base64 encoding, `uu' for uu encoding, or `pack' for simple packing.

ENCODING and DECODING can be strings, giving commands, or symbols,
giving functions.  If they are strings, then they can contain
the \"%s\" format specifier.  If that specifier is present, the input
file name will be put into the command line at that spot.  If the
specifier is not present, the input should be read from standard
input.

If they are functions, they will be called with two arguments, start
and end of region, and are expected to replace the region contents
with the encoded or decoded results, respectively.")

(defconst tramp-remote-coding-commands
  '((b64 "base64" "base64 -d -i")
    ;; "-i" is more robust with older base64 from GNU coreutils.
    ;; However, I don't know whether all base64 versions do supports
    ;; this option.
    (b64 "base64" "base64 -d")
    (b64 "openssl enc -base64" "openssl enc -d -base64")
    (b64 "mimencode -b" "mimencode -u -b")
    (b64 "mmencode -b" "mmencode -u -b")
    (b64 "recode data..base64" "recode base64..data")
    (b64 tramp-perl-encode-with-module tramp-perl-decode-with-module)
    (b64 tramp-perl-encode tramp-perl-decode)
    ;; These are painfully slow, so we put them on the end.
    (b64 tramp-hexdump-awk-encode tramp-awk-decode)
    (b64 tramp-od-awk-encode tramp-awk-decode)
    (uu  "uuencode xxx" "uudecode -o /dev/stdout" "test -c /dev/stdout")
    (uu  "uuencode xxx" "uudecode -o -")
    (uu  "uuencode xxx" "uudecode -p")
    (uu  "uuencode xxx" tramp-uudecode)
    (pack tramp-perl-pack tramp-perl-unpack))
  "List of remote coding commands for inline transfer.
Each item is a list that looks like this:

\(FORMAT ENCODING DECODING [TEST])

FORMAT is a symbol describing the encoding/decoding format.  It can be
`b64' for base64 encoding, `uu' for uu encoding, or `pack' for simple packing.

ENCODING and DECODING can be strings, giving commands, or symbols,
giving variables.  If they are strings, then they can contain
the \"%s\" format specifier.  If that specifier is present, the input
file name will be put into the command line at that spot.  If the
specifier is not present, the input should be read from standard
input.

If they are variables, this variable is a string containing a
Perl or Shell implementation for this functionality.  This
program will be transferred to the remote host, and it is
available as shell function with the same name.  A \"%t\" format
specifier in the variable value denotes a temporary file.
\"%a\", \"%h\" and \"%o\" format specifiers are replaced by the
respective `awk', `hexdump' and `od' commands.  \"%n\" is
replaced by \"2>/dev/null\".

The optional TEST command can be used for further tests, whether
ENCODING and DECODING are applicable.")

(defun tramp-find-inline-encoding (vec)
  "Find an inline transfer encoding that works.
Goes through the list `tramp-local-coding-commands' and
`tramp-remote-coding-commands'."
  (save-excursion
    (let ((local-commands tramp-local-coding-commands)
	  (magic "xyzzy")
	  (p (tramp-get-connection-process vec))
	  loc-enc loc-dec rem-enc rem-dec rem-test litem ritem found)
      (while (and local-commands (not found))
	(setq litem (pop local-commands))
	(catch 'wont-work-local
	  (let ((format (nth 0 litem))
		(remote-commands tramp-remote-coding-commands))
	    (setq loc-enc (nth 1 litem)
		  loc-dec (nth 2 litem))
	    ;; If the local encoder or decoder is a string, the
	    ;; corresponding command has to work locally.
	    (if (not (stringp loc-enc))
		(tramp-message
		 vec 5 "Checking local encoding function `%s'" loc-enc)
	      (tramp-message
	       vec 5 "Checking local encoding command `%s' for sanity" loc-enc)
	      (unless (stringp (setq loc-enc (tramp-expand-script nil loc-enc)))
		(throw 'wont-work-local nil))
	      (unless (zerop (tramp-call-local-coding-command loc-enc nil nil))
		(throw 'wont-work-local nil)))
	    (if (not (stringp loc-dec))
		(tramp-message
		 vec 5 "Checking local decoding function `%s'" loc-dec)
	      (tramp-message
	       vec 5 "Checking local decoding command `%s' for sanity" loc-dec)
	      (unless (stringp (setq loc-dec (tramp-expand-script nil loc-dec)))
		(throw 'wont-work-local nil))
	      (unless (zerop (tramp-call-local-coding-command loc-dec nil nil))
		(throw 'wont-work-local nil)))
	    ;; Search for remote coding commands with the same format
	    (while (and remote-commands (not found))
	      (setq ritem (pop remote-commands))
	      (catch 'wont-work-remote
		(when (equal format (nth 0 ritem))
		  (setq rem-enc (nth 1 ritem)
			rem-dec (nth 2 ritem)
			rem-test (nth 3 ritem))
		  ;; Check the remote test command if exists.
		  (when (stringp rem-test)
		    (tramp-message
		     vec 5 "Checking remote test command `%s'" rem-test)
		    (unless (tramp-send-command-and-check vec rem-test t)
		      (throw 'wont-work-remote nil)))
		  ;; Check if remote encoding and decoding commands can be
		  ;; called remotely with null input and output.  This makes
		  ;; sure there are no syntax errors and the command is really
		  ;; found.  Note that we do not redirect stdout to /dev/null,
		  ;; for two reasons: when checking the decoding command, we
		  ;; actually check the output it gives.  And also, when
		  ;; redirecting "mimencode" output to /dev/null, then as root
		  ;; it might change the permissions of /dev/null!
		  (unless (stringp rem-enc)
		    (let ((name (symbol-name rem-enc))
			  (value (symbol-value rem-enc)))
		      (while (string-match "-" name)
			(setq name (replace-match "_" nil t name)))
		      (unless (tramp-expand-script vec value)
			(throw 'wont-work-remote nil))
		      (tramp-maybe-send-script vec value name)
		      (setq rem-enc name)))
		  (tramp-message
		   vec 5
		   "Checking remote encoding command `%s' for sanity" rem-enc)
		  (unless (tramp-send-command-and-check
			   vec
                           (format
                            "%s <%s" rem-enc (tramp-get-remote-null-device vec))
                           t)
		    (throw 'wont-work-remote nil))

		  (unless (stringp rem-dec)
		    (let ((name (symbol-name rem-dec))
			  (value (symbol-value rem-dec)))
		      (while (string-match "-" name)
			(setq name (replace-match "_" nil t name)))
		      (unless (tramp-expand-script vec value)
			(throw 'wont-work-remote nil))
		      (tramp-maybe-send-script vec value name)
		      (setq rem-dec name)))
		  (tramp-message
		   vec 5
		   "Checking remote decoding command `%s' for sanity" rem-dec)
		  (unless (tramp-send-command-and-check
			   vec
			   (format "echo %s | %s | %s" magic rem-enc rem-dec)
			   t)
		    (throw 'wont-work-remote nil))

		  (with-current-buffer (tramp-get-connection-buffer vec)
		    (goto-char (point-min))
		    (unless (looking-at-p (regexp-quote magic))
		      (throw 'wont-work-remote nil)))

		  ;; `rem-enc' and `rem-dec' could be a string meanwhile.
		  (setq rem-enc (nth 1 ritem)
			rem-dec (nth 2 ritem)
			found t)))))))

      (when found
	;; Set connection properties.  Since the commands are risky
	;; (due to output direction), we cache them in the process cache.
	(tramp-message vec 5 "Using local encoding `%s'" loc-enc)
	(tramp-set-connection-property p "local-encoding" loc-enc)
	(tramp-message vec 5 "Using local decoding `%s'" loc-dec)
	(tramp-set-connection-property p "local-decoding" loc-dec)
	(tramp-message vec 5 "Using remote encoding `%s'" rem-enc)
	(tramp-set-connection-property p "remote-encoding" rem-enc)
	(tramp-message vec 5 "Using remote decoding `%s'" rem-dec)
	(tramp-set-connection-property p "remote-decoding" rem-dec)))))

(defun tramp-call-local-coding-command (cmd input output)
  "Call the local encoding or decoding command.
If CMD contains \"%s\", provide input file INPUT there in command.
Otherwise, INPUT is passed via standard input.
INPUT can also be nil which means `null-device'.
OUTPUT can be a string (which specifies a file name), or t (which
means standard output and thus the current buffer), or nil (which
means discard it)."
  (tramp-call-process
   nil tramp-encoding-shell
   (when (and input (not (tramp-compat-string-search "%s" cmd))) input)
   (if (eq output t) t nil)
   nil
   tramp-encoding-command-switch
   (concat
    (if (tramp-compat-string-search "%s" cmd) (format cmd input) cmd)
    (if (stringp output) (concat " >" output) ""))))

(defconst tramp-inline-compress-commands
  '(;; Suppress warnings about obsolete environment variable GZIP.
    ("env GZIP= gzip" "env GZIP= gzip -d")
    ("bzip2" "bzip2 -d")
    ("xz" "xz -d")
    ("zstd --rm" "zstd -d --rm")
    ("compress" "compress -d"))
  "List of compress and decompress commands for inline transfer.
Each item is a list that looks like this:

\(COMPRESS DECOMPRESS)

COMPRESS or DECOMPRESS are strings with the respective commands.")

(defun tramp-find-inline-compress (vec)
  "Find an inline transfer compress command that works.
Goes through the list `tramp-inline-compress-commands'."
  (save-excursion
    (let ((commands tramp-inline-compress-commands)
	  (magic "xyzzy")
	  (p (tramp-get-connection-process vec))
	  item compress decompress found)
      (while (and commands (not found))
	(catch 'next
	  (setq item (pop commands)
		compress (nth 0 item)
		decompress (nth 1 item))
	  (tramp-message
	   vec 5
	   "Checking local compress commands `%s', `%s' for sanity"
	   compress decompress)
          (with-temp-buffer
            (unless (zerop
	             (tramp-call-local-coding-command
	              (format
	               "echo %s | %s | %s" magic
	               ;; Windows shells need the program file name
	               ;; after the pipe symbol be quoted if they use
	               ;; forward slashes as directory separators.
	               (mapconcat
			#'tramp-unquote-shell-quote-argument
			(split-string compress) " ")
	               (mapconcat
			#'tramp-unquote-shell-quote-argument
			(split-string decompress) " "))
	              nil t))
              (throw 'next nil))
	    (goto-char (point-min))
	    (unless (looking-at-p (regexp-quote magic))
	      (throw 'next nil)))
          (tramp-message
	   vec 5
	   "Checking remote compress commands `%s', `%s' for sanity"
	   compress decompress)
	  (unless (tramp-send-command-and-check
		   vec (format "echo %s | %s | %s" magic compress decompress) t)
	    (throw 'next nil))
	  (with-current-buffer (tramp-get-buffer vec)
	    (goto-char (point-min))
	    (unless (looking-at-p (regexp-quote magic))
	      (throw 'next nil)))
	  (setq found t)))

      ;; Did we find something?
      (if found
	  (progn
	    ;; Set connection properties.  Since the commands are
	    ;; risky (due to output direction), we cache them in the
	    ;; process cache.
	    (tramp-message
	     vec 5 "Using inline transfer compress command `%s'" compress)
	    (tramp-set-connection-property p "inline-compress" compress)
	    (tramp-message
	     vec 5 "Using inline transfer decompress command `%s'" decompress)
	    (tramp-set-connection-property p "inline-decompress" decompress))

	(tramp-set-connection-property p "inline-compress" nil)
	(tramp-set-connection-property p "inline-decompress" nil)
	(tramp-message
	 vec 2 "Couldn't find an inline transfer compress command")))))

(defun tramp-ssh-controlmaster-options (vec)
  "Return the Control* arguments of the local ssh."
  (cond
   ;; No options to be computed.
   ((or (null tramp-use-ssh-controlmaster-options)
	(null (assoc "%c" (tramp-get-method-parameter vec 'tramp-login-args))))
    "")

   ;; There is already a value to be used.
   ((stringp tramp-ssh-controlmaster-options) tramp-ssh-controlmaster-options)

   ;; Determine the options.
   (t (setq tramp-ssh-controlmaster-options "")
      (let ((case-fold-search t))
	(ignore-errors
	  (with-tramp-progress-reporter
	      vec 4 "Computing ControlMaster options"
	    ;; We use a non-existing IP address, in order to avoid
	    ;; useless connections, and DNS timeouts.
	    (when (zerop
		   (tramp-call-process
		    vec "ssh" nil nil nil
		    "-G" "-o" "ControlMaster=auto" "0.0.0.1"))
	      (setq tramp-ssh-controlmaster-options
		    "-o ControlMaster=auto")
	      (if (zerop
		   (tramp-call-process
		    vec "ssh" nil nil nil
		    "-G" "-o" "ControlPath='tramp.%C'" "0.0.0.1"))
		  (setq tramp-ssh-controlmaster-options
			(concat tramp-ssh-controlmaster-options
				" -o ControlPath='tramp.%%C'"))
		(setq tramp-ssh-controlmaster-options
		      (concat tramp-ssh-controlmaster-options
			      " -o ControlPath='tramp.%%r@%%h:%%p'")))
	      (when (zerop
		     (tramp-call-process
		      vec "ssh" nil nil nil
		      "-G" "-o" "ControlPersist=no" "0.0.0.1"))
		(setq tramp-ssh-controlmaster-options
		      (concat tramp-ssh-controlmaster-options
			      " -o ControlPersist=no")))))))
      tramp-ssh-controlmaster-options)))

(defun tramp-scp-strict-file-name-checking (vec)
  "Return the strict file name checking argument of the local scp."
  (cond
   ;; No options to be computed.
   ((null (assoc "%x" (tramp-get-method-parameter vec 'tramp-copy-args)))
    "")

   ;; There is already a value to be used.
   ((stringp tramp-scp-strict-file-name-checking)
    tramp-scp-strict-file-name-checking)

   ;; Determine the options.
   (t (setq tramp-scp-strict-file-name-checking "")
      (let ((case-fold-search t))
	(ignore-errors
	  (when (executable-find "scp")
	    (with-tramp-progress-reporter
		vec 4 "Computing strict file name argument"
	      (with-temp-buffer
		(tramp-call-process vec "scp" nil t nil "-T")
		(goto-char (point-min))
		(unless
                    (search-forward-regexp
                     "\\(illegal\\|unknown\\) option -- T" nil t)
		  (setq tramp-scp-strict-file-name-checking "-T")))))))
      tramp-scp-strict-file-name-checking)))

(defun tramp-timeout-session (vec)
  "Close the connection VEC after a session timeout.
If there is just some editing, retry it after 5 seconds."
  (if (and (tramp-get-connection-property
	    (tramp-get-connection-process vec) "locked" nil)
	   (tramp-file-name-equal-p vec (car tramp-current-connection)))
      (progn
	(tramp-message
	 vec 5 "Cannot timeout session, trying it again in %s seconds." 5)
	(run-at-time 5 nil #'tramp-timeout-session vec))
    (tramp-message
     vec 3 "Timeout session %s" (tramp-make-tramp-file-name vec 'noloc))
    (tramp-cleanup-connection vec 'keep-debug nil 'keep-processes)))

(defun tramp-maybe-open-connection (vec)
  "Maybe open a connection VEC.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
  ;; During completion, don't reopen a new connection.
  (unless (tramp-connectable-p vec)
    (throw 'non-essential 'non-essential))

  (let ((p (tramp-get-connection-process vec))
	(process-name (tramp-get-connection-property vec "process-name" nil))
	(process-environment (copy-sequence process-environment))
	(pos (with-current-buffer (tramp-get-connection-buffer vec) (point))))

    ;; If Tramp opens the same connection within a short time frame,
    ;; there is a problem.  We shall signal this.
    (unless (or (process-live-p p)
                (and (processp p) (not non-essential))
		(not (tramp-file-name-equal-p
		      vec (car tramp-current-connection)))
		(time-less-p
		 (time-since (cdr tramp-current-connection))
		 (or tramp-connection-min-time-diff 0)))
      (throw 'suppress 'suppress))

    ;; If too much time has passed since last command was sent, look
    ;; whether process is still alive.  If it isn't, kill it.  When
    ;; using ssh, it can sometimes happen that the remote end has hung
    ;; up but the local ssh client doesn't recognize this until it
    ;; tries to send some data to the remote end.  So that's why we
    ;; try to send a command from time to time, then look again
    ;; whether the process is really alive.
    (condition-case nil
	(when (and (time-less-p
		    60 (time-since
			(tramp-get-connection-property p "last-cmd-time" 0)))
		   (process-live-p p))
	  (tramp-send-command vec "echo are you awake" t t)
	  (unless (and (process-live-p p)
		       (tramp-wait-for-output p 10))
	    ;; The error will be caught locally.
	    (tramp-error vec 'file-error "Awake did fail")))
      (file-error
       (tramp-cleanup-connection vec t)
       (setq p nil)))

    ;; New connection must be opened.
    (condition-case err
	(unless (process-live-p p)
	  (with-tramp-progress-reporter
	      vec 3
	      (if (zerop (length (tramp-file-name-user vec)))
		  (format "Opening connection %s for %s using %s"
			  process-name
			  (tramp-file-name-host vec)
			  (tramp-file-name-method vec))
		(format "Opening connection %s for %s@%s using %s"
			process-name
			(tramp-file-name-user vec)
			(tramp-file-name-host vec)
			(tramp-file-name-method vec)))

	    (catch 'uname-changed
	      ;; Start new process.
	      (when (and p (processp p))
		(delete-process p))
	      (setenv "TERM" tramp-terminal-type)
	      (setenv "LC_ALL" (tramp-get-local-locale vec))
	      (if (stringp tramp-histfile-override)
		  (setenv "HISTFILE" tramp-histfile-override)
		(if tramp-histfile-override
		    (progn
		      (setenv "HISTFILE")
		      (setenv "HISTFILESIZE" "0")
		      (setenv "HISTSIZE" "0"))))
	      (setenv "PROMPT_COMMAND")
	      (setenv "PS1" tramp-initial-end-of-output)
              (unless (stringp tramp-encoding-shell)
                (tramp-error vec 'file-error "`tramp-encoding-shell' not set"))
	      (let* ((current-host tramp-system-name)
		     (target-alist (tramp-compute-multi-hops vec))
		     ;; Needed for `tramp-get-remote-null-device'.
		     (previous-hop nil)
		     ;; We will apply `tramp-ssh-controlmaster-options'
		     ;; only for the first hop.
		     (options (tramp-ssh-controlmaster-options vec))
		     (process-connection-type tramp-process-connection-type)
		     (process-adaptive-read-buffering nil)
		     ;; There are unfortunate settings for "cmdproxy" on
		     ;; W32 systems.
		     (process-coding-system-alist nil)
		     (coding-system-for-read nil)
		     (extra-args (tramp-get-sh-extra-args tramp-encoding-shell))
		     ;; This must be done in order to avoid our file
		     ;; name handler.
		     (p (let ((default-directory
				tramp-compat-temporary-file-directory))
			  (apply
			   #'start-process
			   (tramp-get-connection-name vec)
			   (tramp-get-connection-buffer vec)
			   (append
			    (list tramp-encoding-shell)
			    (and extra-args (split-string extra-args))
			    (and tramp-encoding-command-interactive
				 (list tramp-encoding-command-interactive)))))))

		;; Set sentinel and query flag.  Initialize variables.
		(set-process-sentinel p #'tramp-process-sentinel)
		(process-put p 'vector vec)
		(process-put p 'adjust-window-size-function #'ignore)
		(set-process-query-on-exit-flag p nil)
		(setq tramp-current-connection (cons vec (current-time)))

		(tramp-message vec 6 "%s" (string-join (process-command p) " "))

		;; Set connection-local variables.
		(tramp-set-connection-local-variables vec)

		;; Check whether process is alive.
		(tramp-barf-if-no-shell-prompt
		 p 10
		 "Couldn't find local shell prompt for %s" tramp-encoding-shell)

		;; Now do all the connections as specified.
		(while target-alist
		  (let* ((hop (car target-alist))
			 (l-method (tramp-file-name-method hop))
			 (l-user (tramp-file-name-user hop))
			 (l-domain (tramp-file-name-domain hop))
			 (l-host (tramp-file-name-host hop))
			 (l-port (tramp-file-name-port hop))
			 (remote-shell
			  (tramp-get-method-parameter hop 'tramp-remote-shell))
			 (extra-args (tramp-get-sh-extra-args remote-shell))
			 (async-args
			  (tramp-compat-flatten-tree
			   (tramp-get-method-parameter hop 'tramp-async-args)))
			 (connection-timeout
			  (tramp-get-method-parameter
			   hop 'tramp-connection-timeout))
			 (command
			  (tramp-get-method-parameter hop 'tramp-login-program))
			 ;; We don't create the temporary file.  In
			 ;; fact, it is just a prefix for the
			 ;; ControlPath option of ssh; the real
			 ;; temporary file has another name, and it is
			 ;; created and protected by ssh.  It is also
			 ;; removed by ssh when the connection is
			 ;; closed.  The temporary file name is cached
			 ;; in the main connection process, therefore
			 ;; we cannot use `tramp-get-connection-process'.
			 (tmpfile
			  (with-tramp-connection-property
			      (tramp-get-process vec) "temp-file"
			    (tramp-compat-make-temp-name)))
			 r-shell)

		    ;; Check, whether there is a restricted shell.
		    (dolist (elt tramp-restricted-shell-hosts-alist)
		      (when (string-match-p elt current-host)
			(setq r-shell t)))
		    (setq current-host l-host)

		    ;; Set password prompt vector.
		    (tramp-set-connection-property
		     p "password-vector"
		     (make-tramp-file-name
		      :method l-method :user l-user :domain l-domain
		      :host l-host :port l-port))

		    ;; Set session timeout.
		    (when (tramp-get-method-parameter
			   hop 'tramp-session-timeout)
		      (tramp-set-connection-property
		       p "session-timeout"
		       (tramp-get-method-parameter
			hop 'tramp-session-timeout)))

		    ;; Replace `login-args' place holders.
		    (setq
		     command
		     (mapconcat
		      #'identity
		      (append
		       ;; We do not want to see the trailing local
		       ;; prompt in `start-file-process'.
		       (unless r-shell '("exec"))
		       `(,command)
		       ;; Add arguments for asynchronous processes.
		       (when process-name async-args)
		       (tramp-expand-args
			hop 'tramp-login-args
			?h (or l-host "") ?u (or l-user "") ?p (or l-port "")
			?c (format-spec options (format-spec-make ?t tmpfile))
			?n (concat
			    "2>" (tramp-get-remote-null-device previous-hop))
			?l (concat remote-shell " " extra-args " -i"))
		       ;; A restricted shell does not allow "exec".
		       (when r-shell '("&&" "exit" "||" "exit")))
		      " "))

		    ;; Send the command.
		    (tramp-message vec 3 "Sending command `%s'" command)
		    (tramp-send-command vec command t t)
		    (tramp-process-actions
		     p vec
		     (min
		      pos (with-current-buffer (process-buffer p) (point-max)))
		     tramp-actions-before-shell
		     (or connection-timeout tramp-connection-timeout))
		    (tramp-message
		     vec 3 "Found remote shell prompt on `%s'" l-host)

		    ;; Next hop.
		    (setq options ""
			  target-alist (cdr target-alist)
			  previous-hop hop)))

		;; Activate session timeout.
		(when (tramp-get-connection-property p "session-timeout" nil)
		  (run-at-time
		   (tramp-get-connection-property p "session-timeout" nil) nil
		   #'tramp-timeout-session vec))

		;; Make initial shell settings.
		(tramp-open-connection-setup-interactive-shell p vec)

		;; Mark it as connected.
		(tramp-set-connection-property p "connected" t)))))

      ;; Cleanup, and propagate the signal.
      ((error quit)
       (tramp-cleanup-connection vec t)
       (signal (car err) (cdr err))))))

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
      ;; If we put `tramp-echo-mark' after a trailing newline (which
      ;; is assumed to be unquoted) `tramp-send-string' doesn't see
      ;; that newline and adds `tramp-rsh-end-of-line' right after
      ;; `tramp-echo-mark', so the remote shell sees two consecutive
      ;; trailing line endings and sends two prompts after executing
      ;; the command, which confuses `tramp-wait-for-output'.
      (when (and (not (string-empty-p command))
		 (string-equal (substring command -1) "\n"))
	(setq command (substring command 0 -1)))
      ;; No need to restore a trailing newline here since `tramp-send-string'
      ;; makes sure that the string ends in `tramp-rsh-end-of-line', anyway.
      (setq command (format "%s%s%s" tramp-echo-mark command tramp-echo-mark)))
    ;; Send the command.
    (tramp-message vec 6 "%s" command)
    (tramp-send-string vec command)
    (unless nooutput (tramp-wait-for-output p))))

(defun tramp-wait-for-output (proc &optional timeout)
  "Wait for output from remote command."
  (unless (buffer-live-p (process-buffer proc))
    (delete-process proc)
    (tramp-error proc 'file-error "Process `%s' not available, try again" proc))
  (with-current-buffer (process-buffer proc)
    (let* (;; Initially, `tramp-end-of-output' is "#$ ".  There might
	   ;; be leading escape sequences, which must be ignored.
	   ;; Busyboxes built with the EDITING_ASK_TERMINAL config
	   ;; option send also escape sequences, which must be
	   ;; ignored.
	   (regexp (format "[^#$\n]*%s\\(%s\\)?\r?$"
			   (regexp-quote tramp-end-of-output)
			   tramp-device-escape-sequence-regexp))
	   ;; Sometimes, the commands do not return a newline but a
	   ;; null byte before the shell prompt, for example "git
	   ;; ls-files -c -z ...".
	   (regexp1 (format "\\(^\\|\000\\)%s" regexp))
	   (found (tramp-wait-for-regexp proc timeout regexp1)))
      (if found
	  (let ((inhibit-read-only t))
	    ;; A simple-minded busybox has sent " ^H" sequences.
	    ;; Delete them.
	    (goto-char (point-min))
	    (when (re-search-forward "^\\(.\b\\)+$" (point-at-eol) t)
	      (forward-line 1)
	      (delete-region (point-min) (point)))
	    ;; Delete the prompt.
	    (when (tramp-search-regexp regexp)
	      (delete-region (point) (point-max))))
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

(defun tramp-send-command-and-check
  (vec command &optional subshell dont-suppress-err exit-status)
  "Run COMMAND and check its exit status.
Send `echo $?' along with the COMMAND for checking the exit status.
If COMMAND is nil, just send `echo $?'.  Return t if the exit
status is 0, and nil otherwise.

If the optional argument SUBSHELL is non-nil, the command is
executed in a subshell, ie surrounded by parentheses.  If
DONT-SUPPRESS-ERR is non-nil, stderr won't be sent to \"/dev/null\".
Optional argument EXIT-STATUS, if non-nil, triggers the return of
the exit status."
  (tramp-send-command
   vec
   (concat (if subshell "( " "")
	   command
	   (if command
               (if dont-suppress-err
                   "; " (format " 2>%s; " (tramp-get-remote-null-device vec)))
             "")
	   "echo tramp_exit_status $?"
	   (if subshell " )" "")))
  (with-current-buffer (tramp-get-connection-buffer vec)
    (unless (tramp-search-regexp "tramp_exit_status [[:digit:]]+")
      (tramp-error
       vec 'file-error "Couldn't find exit status of `%s'" command))
    (skip-chars-forward "^ ")
    (prog1
	(if exit-status
	    (read (current-buffer))
	  (zerop (read (current-buffer))))
      (let ((inhibit-read-only t))
	(delete-region (match-beginning 0) (point-max))))))

(defun tramp-barf-unless-okay (vec command fmt &rest args)
  "Run COMMAND, check exit status, throw error if exit status not okay.
Similar to `tramp-send-command-and-check' but accepts two more arguments
FMT and ARGS which are passed to `error'."
  (or (tramp-send-command-and-check vec command)
      (apply #'tramp-error vec 'file-error fmt args)))

(defun tramp-send-command-and-read (vec command &optional noerror marker)
  "Run COMMAND and return the output, which must be a Lisp expression.
If MARKER is a regexp, read the output after that string.
In case there is no valid Lisp expression and NOERROR is nil, it
raises an error."
  (when (if noerror
	    (ignore-errors (tramp-send-command-and-check vec command))
	  (tramp-barf-unless-okay
	   vec command "`%s' returns with error" command))
    (with-current-buffer (tramp-get-connection-buffer vec)
      (goto-char (point-min))
      ;; Read the marker.
      (when (stringp marker)
	(condition-case nil
	    (re-search-forward marker)
	  (error (unless noerror
		   (tramp-error
		    vec 'file-error
		    "`%s' does not return the marker `%s': `%s'"
		    command marker (buffer-string))))))
      ;; Read the expression.
      (condition-case nil
	  (prog1
	      (let ((signal-hook-function
		     (unless noerror signal-hook-function)))
		(read (current-buffer)))
	    ;; Error handling.
	    (when (re-search-forward "\\S-" (point-at-eol) t)
	      (error nil)))
	(error (unless noerror
		 (tramp-error
		  vec 'file-error
		  "`%s' does not return a valid Lisp expression: `%s'"
		  command (buffer-string))))))))

;; FIXME: Move to tramp.el?
;;;###tramp-autoload
(defun tramp-convert-file-attributes (vec attr)
  "Convert `file-attributes' ATTR generated by perl script, stat or ls.
Convert file mode bits to string and set virtual device number.
Return ATTR."
  (when attr
    (save-match-data
      ;; Remove color escape sequences from symlink.
      (when (stringp (car attr))
	(while (string-match tramp-display-escape-sequence-regexp (car attr))
	  (setcar attr (replace-match "" nil nil (car attr)))))
      ;; Convert uid and gid.  Use `tramp-unknown-id-integer' as
      ;; indication of unusable value.
      (when (and (numberp (nth 2 attr)) (< (nth 2 attr) 0))
	(setcar (nthcdr 2 attr) tramp-unknown-id-integer))
      (when (and (floatp (nth 2 attr))
		 (<= (nth 2 attr) most-positive-fixnum))
	(setcar (nthcdr 2 attr) (round (nth 2 attr))))
      (when (and (numberp (nth 3 attr)) (< (nth 3 attr) 0))
	(setcar (nthcdr 3 attr) tramp-unknown-id-integer))
      (when (and (floatp (nth 3 attr))
		 (<= (nth 3 attr) most-positive-fixnum))
	(setcar (nthcdr 3 attr) (round (nth 3 attr))))
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
	(setcar (nthcdr 8 attr) (tramp-file-mode-from-int (nth 8 attr)))
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
		     (string-match ".+ -> .\\(.+\\)." (caar attr))
		     (decode-coding-string
		      (match-string 1 (caar attr)) 'utf-8))))
      ;; Set file's gid change bit.
      (setcar (nthcdr 9 attr)
              (if (numberp (nth 3 attr))
                  (not (= (nth 3 attr)
                          (tramp-get-remote-gid vec 'integer)))
		(not (string-equal
                      (nth 3 attr)
                      (tramp-get-remote-gid vec 'string)))))
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
                  ;; Inodes can be incredible huge.  We must hide this.
                  (error (tramp-get-inode vec)))))
      ;; Set virtual device number.
      (setcar (nthcdr 11 attr)
              (tramp-get-device vec)))
    attr))

(defun tramp-shell-case-fold (string)
  "Convert STRING to shell glob pattern which ignores case."
  (mapconcat
   (lambda (c)
     (if (equal (downcase c) (upcase c))
         (vector c)
       (format "[%c%c]" (downcase c) (upcase c))))
   string
   ""))

(defun tramp-make-copy-program-file-name (vec)
  "Create a file name suitable for `scp', `pscp', or `nc' and workalikes."
  (let ((method (tramp-file-name-method vec))
	(user (tramp-file-name-user vec))
	(host (tramp-file-name-host vec))
	(localname
	 (directory-file-name (tramp-file-name-unquote-localname vec))))
    (when (string-match-p tramp-ipv6-regexp host)
      (setq host (format "[%s]" host)))
    ;; This does not work for MS Windows scp, if there are characters
    ;; to be quoted.  OpenSSH 8 supports disabling of strict file name
    ;; checking in scp, we use it when available.
    (unless (string-match-p "ftp$" method)
      (setq localname (tramp-unquote-shell-quote-argument localname)))
    (cond
     ((tramp-get-method-parameter vec 'tramp-remote-copy-program)
      localname)
     ((zerop (length user)) (format "%s:%s" host localname))
     (t (format "%s@%s:%s" user host localname)))))

(defun tramp-method-out-of-band-p (vec size)
  "Return t if this is an out-of-band method, nil otherwise."
  (and
   ;; It shall be an out-of-band method.
   (tramp-get-method-parameter vec 'tramp-copy-program)
   ;; There must be a size, otherwise the file doesn't exist.
   (numberp size)
   ;; Either the file size is large enough, or (in rare cases) there
   ;; does not exist a remote encoding.
   (or (null tramp-copy-size-limit)
       (> size tramp-copy-size-limit)
       (null (tramp-get-inline-coding vec "remote-encoding" size)))))

;; Variables local to connection.

(defun tramp-check-remote-uname (vec regexp)
  "Check whether REGEXP matches the connection property \"uname\"."
  (string-match-p regexp (tramp-get-connection-property vec "uname" "")))

(defun tramp-get-remote-path (vec)
  "Compile list of remote directories for PATH.
Nonexistent directories are removed from spec."
  (with-current-buffer (tramp-get-connection-buffer vec)
    ;; Expand connection-local variables.
    (tramp-set-connection-local-variables vec)
    (with-tramp-connection-property
	;; When `tramp-own-remote-path' is in `tramp-remote-path', we
	;; cache the result for the session only.  Otherwise, the
	;; result is cached persistently.
	(if (memq 'tramp-own-remote-path tramp-remote-path)
	    (tramp-get-process vec) vec)
	"remote-path"
      (let* ((remote-path (copy-tree tramp-remote-path))
	     (elt1 (memq 'tramp-default-remote-path remote-path))
	     (elt2 (memq 'tramp-own-remote-path remote-path))
	     (default-remote-path
	       (when elt1
		 (or
		  (tramp-send-command-and-read
		   vec
                   (format
                    "echo \\\"`getconf PATH 2>%s`\\\""
                    (tramp-get-remote-null-device vec))
                   'noerror)
		  ;; Default if "getconf" is not available.
		  (progn
		    (tramp-message
		     vec 3
		     "`getconf PATH' not successful, using default value \"%s\"."
		     "/bin:/usr/bin")
		    "/bin:/usr/bin"))))
	     (own-remote-path
	      ;; The login shell could return more than just the $PATH
	      ;; string.  So we use `tramp-end-of-heredoc' as marker.
	      (when elt2
		(or
		 (tramp-send-command-and-read
		  vec
		  (format
		   "%s %s %s 'echo %s \\\"$PATH\\\"'"
		   (tramp-get-method-parameter vec 'tramp-remote-shell)
		   (mapconcat
		    #'identity
		    (tramp-get-method-parameter vec 'tramp-remote-shell-login)
		    " ")
		   (mapconcat
		    #'identity
		    (tramp-get-method-parameter vec 'tramp-remote-shell-args)
		    " ")
		   (tramp-shell-quote-argument tramp-end-of-heredoc))
		  'noerror (regexp-quote tramp-end-of-heredoc))
		 (progn
		   (tramp-message
		    vec 2 "Could not retrieve `tramp-own-remote-path'")
		   nil)))))

	;; Replace place holder `tramp-default-remote-path'.
	(when elt1
	  (setcdr elt1
		  (append
                   (split-string (or default-remote-path "") ":" 'omit)
		   (cdr elt1)))
	  (setq remote-path (delq 'tramp-default-remote-path remote-path)))

	;; Replace place holder `tramp-own-remote-path'.
	(when elt2
	  (setcdr elt2
		  (append
                   (split-string (or own-remote-path "") ":" 'omit)
		   (cdr elt2)))
	  (setq remote-path (delq 'tramp-own-remote-path remote-path)))

	;; Remove double entries.
	(setq elt1 remote-path)
	(while (consp elt1)
	  (while (and (car elt1) (setq elt2 (member (car elt1) (cdr elt1))))
	    (setcar elt2 nil))
	  (setq elt1 (cdr elt1)))

	;; Remove non-existing directories.
	(delq
	 nil
	 (mapcar
	  (lambda (x)
	    (and
	     (stringp x)
	     (file-directory-p (tramp-make-tramp-file-name vec x 'nohop))
	     x))
	  remote-path))))))

(defun tramp-get-remote-locale (vec)
  "Determine remote locale, supporting UTF8 if possible."
  (with-tramp-connection-property vec "locale"
    (tramp-send-command vec "locale -a")
    (let ((candidates '("en_US.utf8" "C.utf8" "en_US.UTF-8" "C.UTF-8"))
	  locale)
      (with-current-buffer (tramp-get-connection-buffer vec)
	(while candidates
	  (goto-char (point-min))
	  (if (string-match-p (format "^%s\r?$" (regexp-quote (car candidates)))
			      (buffer-string))
	      (setq locale (car candidates)
		    candidates nil)
	    (setq candidates (cdr candidates)))))
      ;; Return value.
      (format "LC_ALL=%s" (or locale "C")))))

(defun tramp-get-ls-command (vec)
  "Determine remote `ls' command."
  (with-tramp-connection-property vec "ls"
    (tramp-message vec 5 "Finding a suitable `ls' command")
    (or
     (catch 'ls-found
       (dolist (cmd '("ls" "gnuls" "gls"))
	 (let ((dl (tramp-get-remote-path vec))
	       result)
	   (while (and dl (setq result (tramp-find-executable vec cmd dl t t)))
	     ;; Check parameters.  On busybox, "ls" output coloring is
	     ;; enabled by default sometimes.  So we try to disable it
	     ;; when possible.  $LS_COLORING is not supported there.
	     ;; Some "ls" versions are sensitive to the order of
	     ;; arguments, they fail when "-al" is after the
	     ;; "--color=never" argument (for example on FreeBSD).
	     (when (tramp-send-command-and-check
		    vec (format "%s -lnd /" result))
	       (when (tramp-send-command-and-check
		      vec (format
			   "%s --color=never -al %s"
                           result (tramp-get-remote-null-device vec)))
		 (setq result (concat result " --color=never")))
	       (throw 'ls-found result))
	     (setq dl (cdr dl))))))
     (tramp-error vec 'file-error "Couldn't find a proper `ls' command"))))

(defun tramp-get-ls-command-with (vec option)
  "Return OPTION, if the remote `ls' command supports the OPTION option."
  (with-tramp-connection-property vec (concat "ls" option)
    (tramp-message vec 5 "Checking, whether `ls %s' works" option)
    ;; Some "ls" versions are sensitive to the order of arguments,
    ;; they fail when "-al" is after the "--dired" argument (for
    ;; example on FreeBSD).  Busybox does not support this kind of
    ;; options.
    (and
     (not
      (tramp-send-command-and-check
       vec
       (format
	"%s --help 2>&1 | grep -iq busybox" (tramp-get-ls-command vec))))
     (tramp-send-command-and-check
      vec (format
           "%s %s -al %s"
           (tramp-get-ls-command vec) option (tramp-get-remote-null-device vec)))
     option)))

(defun tramp-get-test-command (vec)
  "Determine remote `test' command."
  (with-tramp-connection-property vec "test"
    (tramp-message vec 5 "Finding a suitable `test' command")
    (if (tramp-send-command-and-check vec "test 0")
	"test"
      (tramp-find-executable vec "test" (tramp-get-remote-path vec)))))

(defun tramp-get-test-nt-command (vec)
  "Check, whether the remote `test' command supports the -nt option."
  ;; Does `test A -nt B' work?  Use abominable `find' construct if it
  ;; doesn't.  BSD/OS 4.0 wants the parentheses around the command,
  ;; for otherwise the shell crashes.
  (with-tramp-connection-property vec "test-nt"
    (or
     (progn
       (tramp-send-command
	vec (format "( %s / -nt / )" (tramp-get-test-command vec)))
       (with-current-buffer (tramp-get-buffer vec)
	 (goto-char (point-min))
	 (when (looking-at-p (regexp-quote tramp-end-of-output))
	   (format "%s %%s -nt %%s" (tramp-get-test-command vec)))))
     (progn
       (tramp-send-command
	vec
	(format
	 "tramp_test_nt () {\n%s -n \"`find $1 -prune -newer $2 -print`\"\n}"
	 (tramp-get-test-command vec)))
       "tramp_test_nt %s %s"))))

(defun tramp-get-file-exists-command (vec)
  "Determine remote command for file existing check."
  (with-tramp-connection-property vec "file-exists"
    (tramp-message vec 5 "Finding command to check if file exists")
    (tramp-find-file-exists-command vec)))

(defun tramp-get-remote-ln (vec)
  "Determine remote `ln' command."
  (with-tramp-connection-property vec "ln"
    (tramp-message vec 5 "Finding a suitable `ln' command")
    (tramp-find-executable vec "ln" (tramp-get-remote-path vec))))

(defun tramp-get-remote-perl (vec)
  "Determine remote `perl' command."
  (with-tramp-connection-property vec "perl"
    (tramp-message vec 5 "Finding a suitable `perl' command")
    (let ((result
	   (or (tramp-find-executable vec "perl5" (tramp-get-remote-path vec))
	       (tramp-find-executable vec "perl" (tramp-get-remote-path vec)))))
      ;; Perform a basic check.
      (and result
	   (null (tramp-send-command-and-check
		  vec (format "%s -e 'print \"Hello\n\";'" result)))
	   (setq result nil))
      ;; We must check also for some Perl modules.
      (when result
	(with-tramp-connection-property vec "perl-file-spec"
	  (tramp-send-command-and-check
	   vec (format "%s -e 'use File::Spec;'" result)))
	(with-tramp-connection-property vec "perl-cwd-realpath"
	  (tramp-send-command-and-check
	   vec (format "%s -e 'use Cwd \"realpath\";'" result))))
      result)))

(defun tramp-get-remote-stat (vec)
  "Determine remote `stat' command."
  (with-tramp-connection-property vec "stat"
    ;; stat on Solaris is buggy.  We've got reports for "SunOS 5.10"
    ;; and "SunOS 5.11" so far.
    (unless (tramp-check-remote-uname vec tramp-sunos-unames)
      (tramp-message vec 5 "Finding a suitable `stat' command")
      (let ((result (tramp-find-executable
		     vec "stat" (tramp-get-remote-path vec)))
	    tmp)
	;; Check whether stat(1) returns usable syntax.  "%s" does not
	;; work on older AIX systems.  Recent GNU stat versions
	;; (8.24?)  use shell quoted format for "%N", we check the
	;; boundaries "`" and "'" and their localized variants,
	;; therefore.  See Bug#23422 in coreutils.  Since GNU stat
	;; 8.26, environment variable QUOTING_STYLE is supported.
	(when result
	  (setq result (concat "env QUOTING_STYLE=locale " result)
		tmp (tramp-send-command-and-read
		     vec (format "%s -c '(\"%%N\" %%s)' /" result) 'noerror))
	  (unless (and (listp tmp) (stringp (car tmp))
		       (string-match-p "^[\"`‘„”«「]/[\"'’“”»」]$" (car tmp))
		       (integerp (cadr tmp)))
	    (setq result nil)))
	result))))

(defun tramp-get-remote-readlink (vec)
  "Determine remote `readlink' command."
  (with-tramp-connection-property vec "readlink"
    (tramp-message vec 5 "Finding a suitable `readlink' command")
    (let ((result (tramp-find-executable
		   vec "readlink" (tramp-get-remote-path vec))))
      (when (and result
		 (tramp-send-command-and-check
		  vec (format "%s --canonicalize-missing /" result)))
	result))))

(defun tramp-get-remote-trash (vec)
  "Determine remote `trash' command.
This command is returned only if `delete-by-moving-to-trash' is non-nil."
  (and delete-by-moving-to-trash
       (with-tramp-connection-property vec "trash"
	 (tramp-message vec 5 "Finding a suitable `trash' command")
	 (tramp-find-executable vec "trash" (tramp-get-remote-path vec)))))

(defun tramp-get-remote-touch (vec)
  "Determine remote `touch' command."
  (with-tramp-connection-property vec "touch"
    (tramp-message vec 5 "Finding a suitable `touch' command")
    (let ((result (tramp-find-executable
		   vec "touch" (tramp-get-remote-path vec)))
	  (tmpfile (tramp-make-tramp-temp-name vec)))
      ;; Busyboxes do support the "-t" option only when they have been
      ;; built with the DESKTOP config option.  Let's check it.
      (when result
	(tramp-set-connection-property
	 vec "touch-t"
	 (tramp-send-command-and-check
	  vec
	  (format
	   "%s -t %s %s"
	   result
	   (format-time-string "%Y%m%d%H%M.%S")
	   (tramp-file-local-name tmpfile))))
	(delete-file tmpfile))
      result)))

(defun tramp-get-remote-df (vec)
  "Determine remote `df' command."
  (with-tramp-connection-property vec "df"
    (tramp-message vec 5 "Finding a suitable `df' command")
    (let ((df (tramp-find-executable vec "df" (tramp-get-remote-path vec)))
	  result)
      (when df
	(cond
	 ;; coreutils.
	 ((tramp-send-command-and-check
	   vec
	   (format
	    "%s /"
	    (setq result
		  (format "%s --block-size=1 --output=size,used,avail" df))))
	  (tramp-set-connection-property vec "df-blocksize" 1)
	  result)
	 ;; POSIX.1
	 ((tramp-send-command-and-check
	   vec (format "%s /" (setq result (format "%s -k" df))))
	  (tramp-set-connection-property vec "df-blocksize" 1024)
	  result))))))

(defun tramp-get-remote-gio-monitor (vec)
  "Determine remote `gio-monitor' command."
  (with-tramp-connection-property vec "gio-monitor"
    (tramp-message vec 5 "Finding a suitable `gio-monitor' command")
    (tramp-find-executable vec "gio" (tramp-get-remote-path vec) t t)))

(defun tramp-get-remote-inotifywait (vec)
  "Determine remote `inotifywait' command."
  (with-tramp-connection-property vec "inotifywait"
    (tramp-message vec 5 "Finding a suitable `inotifywait' command")
    (tramp-find-executable vec "inotifywait" (tramp-get-remote-path vec) t t)))

(defun tramp-get-remote-id (vec)
  "Determine remote `id' command."
  (with-tramp-connection-property vec "id"
    (tramp-message vec 5 "Finding POSIX `id' command")
    (catch 'id-found
      (dolist (cmd '("id" "gid"))
	(let ((dl (tramp-get-remote-path vec))
	      result)
	  (while (and dl (setq result (tramp-find-executable vec cmd dl t t)))
	    ;; Check POSIX parameter.
	    (when (tramp-send-command-and-check vec (format "%s -u" result))
	      (throw 'id-found result))
	    (setq dl (cdr dl))))))))

(defun tramp-get-remote-uid-with-id (vec id-format)
  "Implement `tramp-get-remote-uid' for Tramp files using `id'."
  (tramp-send-command-and-read
   vec
   (format "%s -u%s %s"
	   (tramp-get-remote-id vec)
	   (if (equal id-format 'integer) "" "n")
	   (if (equal id-format 'integer)
	       "" "| sed -e s/^/\\\"/ -e s/\\$/\\\"/"))))

(defun tramp-get-remote-uid-with-perl (vec id-format)
  "Implement `tramp-get-remote-uid' for Tramp files using a Perl script."
  (tramp-send-command-and-read
   vec
   (format "%s -le '%s'"
	   (tramp-get-remote-perl vec)
	   (if (equal id-format 'integer)
	       "print $>"
	     "print \"\\\"\", scalar getpwuid($>), \"\\\"\""))))

(defun tramp-get-remote-python (vec)
  "Determine remote `python' command."
  (with-tramp-connection-property vec "python"
    (tramp-message vec 5 "Finding a suitable `python' command")
    (or (tramp-find-executable vec "python" (tramp-get-remote-path vec))
        (tramp-find-executable vec "python2" (tramp-get-remote-path vec))
        (tramp-find-executable vec "python3" (tramp-get-remote-path vec)))))

(defun tramp-get-remote-uid-with-python (vec id-format)
  "Implement `tramp-get-remote-uid' for Tramp files using `python'."
  (tramp-send-command-and-read
   vec
   (format "%s -c \"%s\""
	   (tramp-get-remote-python vec)
	   (if (equal id-format 'integer)
	       "import os; print (os.getuid())"
    "import os, pwd; print ('\\\"' + pwd.getpwuid(os.getuid())[0] + '\\\"')"))))

(defun tramp-get-remote-gid-with-id (vec id-format)
  "Implement `tramp-get-remote-gid' for Tramp files using `id'."
  (tramp-send-command-and-read
   vec
   (format "%s -g%s %s"
	   (tramp-get-remote-id vec)
	   (if (equal id-format 'integer) "" "n")
	   (if (equal id-format 'integer)
	       "" "| sed -e s/^/\\\"/ -e s/\\$/\\\"/"))))

(defun tramp-get-remote-gid-with-perl (vec id-format)
  "Implement `tramp-get-remote-gid' for Tramp files using a Perl script."
  (tramp-send-command-and-read
   vec
   (format "%s -le '%s'"
	   (tramp-get-remote-perl vec)
	   (if (equal id-format 'integer)
	       "print ($)=~/(\\d+)/)"
	     "print \"\\\"\", scalar getgrgid($)), \"\\\"\""))))

(defun tramp-get-remote-gid-with-python (vec id-format)
  "Implement `tramp-get-remote-gid' for Tramp files using `python'."
  (tramp-send-command-and-read
   vec
   (format "%s -c \"%s\""
	   (tramp-get-remote-python vec)
	   (if (equal id-format 'integer)
	       "import os; print (os.getgid())"
    "import os, grp; print ('\\\"' + grp.getgrgid(os.getgid())[0] + '\\\"')"))))

(defun tramp-get-remote-busybox (vec)
  "Determine remote `busybox' command."
  (with-tramp-connection-property vec "busybox"
    (tramp-message vec 5 "Finding a suitable `busybox' command")
    (tramp-find-executable vec "busybox" (tramp-get-remote-path vec))))

(defun tramp-get-remote-awk (vec)
  "Determine remote `awk' command."
  (with-tramp-connection-property vec "awk"
    (tramp-message vec 5 "Finding a suitable `awk' command")
    (or (tramp-find-executable vec "awk" (tramp-get-remote-path vec))
	(let* ((busybox (tramp-get-remote-busybox vec))
	       (command (format "%s %s" busybox "awk")))
	  (and busybox
	       (tramp-send-command-and-check
		vec (concat command " {} <" (tramp-get-remote-null-device vec)))
	       command)))))

(defun tramp-get-remote-hexdump (vec)
  "Determine remote `hexdump' command."
  (with-tramp-connection-property vec "hexdump"
    (tramp-message vec 5 "Finding a suitable `hexdump' command")
    (or (tramp-find-executable vec "hexdump" (tramp-get-remote-path vec))
	(let* ((busybox (tramp-get-remote-busybox vec))
	       (command (format "%s %s" busybox "hexdump")))
	  (and busybox
	       (tramp-send-command-and-check
                vec (concat command " <" (tramp-get-remote-null-device vec)))
	       command)))))

(defun tramp-get-remote-od (vec)
  "Determine remote `od' command."
  (with-tramp-connection-property vec "od"
    (tramp-message vec 5 "Finding a suitable `od' command")
    (or (tramp-find-executable vec "od" (tramp-get-remote-path vec))
	(let* ((busybox (tramp-get-remote-busybox vec))
	       (command (format "%s %s" busybox "od")))
	  (and busybox
	       (tramp-send-command-and-check
		vec
                (concat command " -A n <" (tramp-get-remote-null-device vec)))
	       command)))))

(defun tramp-get-remote-chmod-h (vec)
  "Check whether remote `chmod' supports nofollow argument."
  (with-tramp-connection-property vec "chmod-h"
    (tramp-message vec 5 "Finding a suitable `chmod' command with nofollow")
    (let ((tmpfile (tramp-make-tramp-temp-name vec)))
      (prog1
	  (tramp-send-command-and-check
	   vec
	   (format
	    "ln -s foo %s && chmod -h %s 0777"
	    (tramp-file-local-name tmpfile) (tramp-file-local-name tmpfile)))
	(delete-file tmpfile)))))

(defun tramp-get-remote-mknod-or-mkfifo (vec)
  "Determine remote `mknod' or `mkfifo' command."
  (with-tramp-connection-property vec "mknod-or-mkfifo"
    (tramp-message vec 5 "Finding a suitable `mknod' or `mkfifo' command")
    (let ((tmpfile (tramp-make-tramp-temp-name vec))
	  command)
      (prog1
	  (or (and (setq command "mknod %s p")
		   (tramp-send-command-and-check
		    vec (format command (tramp-file-local-name tmpfile)))
		   command)
	      (and (setq command "mkfifo %s")
		   (tramp-send-command-and-check
		    vec (format command (tramp-file-local-name tmpfile)))
		   command))
	(delete-file tmpfile)))))

(defun tramp-get-remote-dev-tty (vec)
  "Check, whether remote /dev/tty is usable."
  (with-tramp-connection-property vec "dev-tty"
    (tramp-send-command-and-check
     vec "echo </dev/tty")))

;; Some predefined connection properties.
(defun tramp-get-inline-compress (vec prop size)
  "Return the compress command related to PROP.
PROP is either `inline-compress' or `inline-decompress'.
SIZE is the length of the file to be compressed.

If no corresponding command is found, nil is returned."
  (when (and (integerp tramp-inline-compress-start-size)
	     (> size tramp-inline-compress-start-size))
    (with-tramp-connection-property (tramp-get-process vec) prop
      (tramp-find-inline-compress vec)
      (tramp-get-connection-property (tramp-get-process vec) prop nil))))

(defun tramp-get-inline-coding (vec prop size)
  "Return the coding command related to PROP.
PROP is either `remote-encoding', `remote-decoding',
`local-encoding' or `local-decoding'.

SIZE is the length of the file to be coded.  Depending on SIZE,
compression might be applied.

If no corresponding command is found, nil is returned.
Otherwise, either a string is returned which contains a `%s' mark
to be used for the respective input or output file; or a Lisp
function cell is returned to be applied on a buffer."
  ;; We must catch the errors, because we want to return nil, when
  ;; no inline coding is found.
  (ignore-errors
    (let ((coding
	   (with-tramp-connection-property (tramp-get-process vec) prop
	     (tramp-find-inline-encoding vec)
	     (tramp-get-connection-property (tramp-get-process vec) prop nil)))
	  (prop1 (if (tramp-compat-string-search "encoding" prop)
		     "inline-compress" "inline-decompress"))
	  compress)
      ;; The connection property might have been cached.  So we must
      ;; send the script to the remote side - maybe.
      (when (and coding (symbolp coding)
		 (tramp-compat-string-search "remote" prop))
	(let ((name (symbol-name coding)))
	  (while (string-match "-" name)
	    (setq name (replace-match "_" nil t name)))
	  (tramp-maybe-send-script vec (symbol-value coding) name)
	  (setq coding name)))
      (when coding
	;; Check for the `compress' command.
	(setq compress (tramp-get-inline-compress vec prop1 size))
	;; Return the value.
	(cond
	 ((and compress (symbolp coding))
	  (if (tramp-compat-string-search "decompress" prop1)
	      `(lambda (beg end)
		 (,coding beg end)
		 (let ((coding-system-for-write 'binary)
		       (coding-system-for-read 'binary))
		   (apply
		    #'tramp-call-process-region ',vec (point-min) (point-max)
		    (car (split-string ,compress)) t t nil
		    (cdr (split-string ,compress)))))
	    `(lambda (beg end)
	       (let ((coding-system-for-write 'binary)
		     (coding-system-for-read 'binary))
		 (apply
		  #'tramp-call-process-region ',vec beg end
		  (car (split-string ,compress)) t t nil
		  (cdr (split-string ,compress))))
	       (,coding (point-min) (point-max)))))
	 ((symbolp coding)
	  coding)
	 ((and compress (tramp-compat-string-search "decoding" prop))
	  (format
	   ;; Windows shells need the program file name after
	   ;; the pipe symbol be quoted if they use forward
	   ;; slashes as directory separators.
	   (cond
	    ((and (tramp-compat-string-search "local" prop)
		  (eq system-type 'windows-nt))
	       "(%s | \"%s\")")
	    ((tramp-compat-string-search "local" prop) "(%s | %s)")
	    (t "(%s | %s >%%s)"))
	   coding compress))
	 (compress
	  (format
	   ;; Windows shells need the program file name after
	   ;; the pipe symbol be quoted if they use forward
	   ;; slashes as directory separators.
	   (if (and (tramp-compat-string-search "local" prop)
		    (eq system-type 'windows-nt))
	       "(%s <%%s | \"%s\")"
	     "(%s <%%s | %s)")
	   compress coding))
	 ((tramp-compat-string-search "decoding" prop)
	  (cond
	   ((tramp-compat-string-search "local" prop) (format "%s" coding))
	   (t (format "%s >%%s" coding))))
	 (t
	  (format "%s <%%s" coding)))))))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-sh 'force)))

(provide 'tramp-sh)

;;; TODO:

;; * Don't use globbing for directories with many files, as this is
;;   likely to produce long command lines, and some shells choke on
;;   long command lines.
;;
;; * When editing a remote CVS controlled file as a different user, VC
;;   gets confused about the file locking status.  Try to find out why
;;   the workaround doesn't work.
;;
;; * WIBNI if we had a command "trampclient"?  If I was editing in
;;   some shell with root privileges, it would be nice if I could
;;   just call
;;     trampclient filename.c
;;   as an editor, and the _current_ shell would connect to an Emacs
;;   server and would be used in an existing non-privileged Emacs
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
;;   something.  (David Kastrup)
;;
;; * Avoid the local shell entirely for starting remote processes.  If
;;   so, I think even a signal, when delivered directly to the local
;;   SSH instance, would correctly be propagated to the remote process
;;   automatically; possibly SSH would have to be started with
;;   "-t".  (Markus Triska)
;;
;; * It makes me wonder if tramp couldn't fall back to ssh when scp
;;   isn't on the remote host.  (Mark A. Hershberger)
;;
;; * Use lsh instead of ssh.  (Alfred M. Szmidt)
;;
;; * Optimize out-of-band copying when both methods are scp-like (not
;;   rsync).
;;
;; * Keep a second connection open for out-of-band methods like scp or
;;   rsync.
;;
;; * Implement completion for "/method:user@host:~<abc> TAB".
;;
;; * I think you could get the best of both worlds by using an
;;   approach similar to Tramp but running a little tramp-daemon on
;;   the other end, such that we can use a more efficient
;;   communication protocol (e.g. when saving a file we could locally
;;   diff it against the last version (of which the remote daemon
;;   would also keep a copy), and then only send the diff).
;;
;;   This said, even using such a daemon it might be difficult to get
;;   good performance: part of the problem is the number of
;;   round-trips.  E.g. when saving a file we have to check if the
;;   file was modified in the mean time and whether saving into a new
;;   inode would change the owner (etc...), which each require a
;;   round-trip.  To get rid of these round-trips, we'd have to
;;   shortcut this code and delegate the higher-level "save file"
;;   operation to the remote server, which then has to perform those
;;   tasks but still obeying the locally set customizations about how
;;   to do each one of those tasks.
;;
;;   We could either put higher-level ops in there (like
;;   `save-buffer'), which implies replicating the whole `save-buffer'
;;   behavior, which is a lot of work and likely to be not 100%
;;   faithful.
;;
;;   Or we could introduce new low-level ops that are asynchronous,
;;   and then rewrite save-buffer to use them.  IOW save-buffer would
;;   start with a bunch of calls like `start-getting-file-attributes'
;;   which could immediately be passed on to the remote side, and
;;   later on checks the return value of those calls as and when
;;   needed.  (Stefan Monnier)
;;
;; * Implement detaching/re-attaching remote sessions.  By this, a
;;   session could be reused after a connection loss.  Use dtach, or
;;   screen, or tmux, or mosh.
;;
;; * One interesting solution (with other applications as well) would
;;   be to stipulate, as a directory or connection-local variable, an
;;   additional rc file on the remote machine that is sourced every
;;   time Tramp connects.  <https://emacs.stackexchange.com/questions/62306>

;;; tramp-sh.el ends here
