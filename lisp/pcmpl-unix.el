;;; pcmpl-unix.el --- standard UNIX completions  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2024 Free Software Foundation, Inc.

;; Package: pcomplete

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

;;; Code:

(require 'pcomplete)

;;; User Variables

(defcustom pcmpl-unix-group-file "/etc/group"
  "If non-nil, a string naming the group file on your system."
  :type '(choice file (const nil))
  :group 'pcmpl-unix)

(defcustom pcmpl-unix-passwd-file "/etc/passwd"
  "If non-nil, a string naming the passwd file on your system."
  :type '(choice file (const nil))
  :group 'pcmpl-unix)

(defcustom pcmpl-ssh-known-hosts-file "~/.ssh/known_hosts"
  "If non-nil, a string naming your SSH \"known_hosts\" file.
This allows one method of completion of SSH host names, the other
being via `pcmpl-ssh-config-file'.  Note that newer versions of
ssh hash the hosts by default, to prevent Island-hopping SSH
attacks.  This can be disabled, at some risk, with the SSH option
\"HashKnownHosts no\"."
  :type '(choice file (const nil))
  :group 'pcmpl-unix
  :version "23.1")

(defcustom pcmpl-ssh-config-file "~/.ssh/config"
  "If non-nil, a string naming your SSH \"config\" file.
This allows one method of completion of SSH host names, the other
being via `pcmpl-ssh-known-hosts-file'."
  :type '(choice file (const nil))
  :group 'pcmpl-unix
  :version "24.1")

;;; Shell builtins and core utilities

;;;###autoload
(defun pcomplete/cd ()
  "Completion for `cd'."
  (while (pcomplete-here (pcomplete-dirs))))

;;;###autoload
(defalias 'pcomplete/pushd 'pcomplete/cd)

;;;###autoload
(defun pcomplete/rmdir ()
  "Completion for `rmdir'."
  (while (if (string-prefix-p "-" (pcomplete-arg))
             (pcomplete-here (pcomplete-from-help "rmdir --help"))
           (pcomplete-here (pcomplete-dirs)))))

;;;###autoload
(defun pcomplete/rm ()
  "Completion for the `rm' command."
  (pcomplete-here-using-help "rm --help"))

;;;###autoload
(defun pcomplete/xargs ()
  "Completion for `xargs'."
  (while (string-prefix-p "-" (pcomplete-arg 0))
    (pcomplete-here (pcomplete-from-help "xargs --help"))
    (when (pcomplete-match "\\`-[adEIiLnPs]\\'") (pcomplete-here)))
  (funcall pcomplete-command-completion-function)
  (funcall (or (pcomplete-find-completion-function (pcomplete-arg 1))
	       pcomplete-default-completion-function)))

;;;###autoload
(defun pcomplete/time ()
  "Completion for the `time' command."
  (pcomplete-opt "p")
  (funcall pcomplete-command-completion-function)
  (funcall (or (pcomplete-find-completion-function (pcomplete-arg 1))
	       pcomplete-default-completion-function)))

;;;###autoload
(defun pcomplete/which ()
  "Completion for `which'."
  (while (string-prefix-p "-" (pcomplete-arg 0))
    (pcomplete-here (pcomplete-from-help "which --help")))
  (while (pcomplete-here (funcall pcomplete-command-completion-function))))

(defun pcmpl-unix-read-passwd-file (file)
  "Return an alist correlating gids to group names in FILE.

If FILE is in hashed format (as described in the OpenSSH
documentation), this function returns nil."
  (let (names)
    (when (file-readable-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(while (not (eobp))
	  (let* ((fields
		  (split-string (buffer-substring
				 (point) (progn (end-of-line)
						(point))) ":")))
	    (setq names (cons (nth 0 fields) names)))
	  (forward-line))))
    (pcomplete-uniquify-list names)))

(defsubst pcmpl-unix-group-names ()
  "Read the contents of /etc/group for group names."
  (if pcmpl-unix-group-file
      (pcmpl-unix-read-passwd-file pcmpl-unix-group-file)))

(defsubst pcmpl-unix-user-names ()
  "Read the contents of /etc/passwd for user names."
  (if pcmpl-unix-passwd-file
      (pcmpl-unix-read-passwd-file pcmpl-unix-passwd-file)))

;;;###autoload
(defun pcomplete/cat ()
  "Completion for the `cat' command."
  (pcomplete-here-using-help "cat --help"))

;;;###autoload
(defun pcomplete/tac ()
  "Completion for the `tac' command."
  (pcomplete-here-using-help "tac --help"))

;;;###autoload
(defun pcomplete/nl ()
  "Completion for the `nl' command."
  (pcomplete-here-using-help "nl --help"))

;;;###autoload
(defun pcomplete/od ()
  "Completion for the `od' command."
  (pcomplete-here-using-help "od --help"))

;;;###autoload
(defun pcomplete/base32 ()
  "Completion for the `base32' and `base64' commands."
  (pcomplete-here-using-help "base32 --help"))
;;;###autoload
(defalias 'pcomplete/base64 'pcomplete/base32)

;;;###autoload
(defun pcomplete/basenc ()
  "Completion for the `basenc' command."
  (pcomplete-here-using-help "basenc --help"))

;;;###autoload
(defun pcomplete/fmt ()
  "Completion for the `fmt' command."
  (pcomplete-here-using-help "fmt --help"))

;;;###autoload
(defun pcomplete/pr ()
  "Completion for the `pr' command."
  (pcomplete-here-using-help "pr --help"))

;;;###autoload
(defun pcomplete/fold ()
  "Completion for the `fold' command."
  (pcomplete-here-using-help "fold --help"))

;;;###autoload
(defun pcomplete/head ()
  "Completion for the `head' command."
  (pcomplete-here-using-help "head --help"))

;;;###autoload
(defun pcomplete/tail ()
  "Completion for the `tail' command."
  (pcomplete-here-using-help "tail --help"))

;;;###autoload
(defun pcomplete/split ()
  "Completion for the `split' command."
  (pcomplete-here-using-help "split --help"))

;;;###autoload
(defun pcomplete/csplit ()
  "Completion for the `csplit' command."
  (pcomplete-here-using-help "csplit --help"))

;;;###autoload
(defun pcomplete/wc ()
  "Completion for the `wc' command."
  (pcomplete-here-using-help "wc --help"))

;;;###autoload
(defun pcomplete/sum ()
  "Completion for the `sum' command."
  (pcomplete-here-using-help "sum --help"))

;;;###autoload
(defun pcomplete/cksum ()
  "Completion for the `cksum' command."
  (pcomplete-here-using-help "cksum --help"))

;;;###autoload
(defun pcomplete/b2sum ()
  "Completion for the `b2sum' command."
  (pcomplete-here-using-help "b2sum --help"))

;;;###autoload
(defun pcomplete/md5sum ()
  "Completion for checksum commands."
  (pcomplete-here-using-help "md5sum --help"))
;;;###autoload(defalias 'pcomplete/sha1sum 'pcomplete/md5sum)
;;;###autoload(defalias 'pcomplete/sha224sum 'pcomplete/md5sum)
;;;###autoload(defalias 'pcomplete/sha256sum 'pcomplete/md5sum)
;;;###autoload(defalias 'pcomplete/sha384sum 'pcomplete/md5sum)
;;;###autoload(defalias 'pcomplete/sha521sum 'pcomplete/md5sum)

;;;###autoload
(defun pcomplete/sort ()
  "Completion for the `sort' command."
  (pcomplete-here-using-help "sort --help"))

;;;###autoload
(defun pcomplete/shuf ()
  "Completion for the `shuf' command."
  (pcomplete-here-using-help "shuf --help"))

;;;###autoload
(defun pcomplete/uniq ()
  "Completion for the `uniq' command."
  (pcomplete-here-using-help "uniq --help"))

;;;###autoload
(defun pcomplete/comm ()
  "Completion for the `comm' command."
  (pcomplete-here-using-help "comm --help"))

;;;###autoload
(defun pcomplete/ptx ()
  "Completion for the `ptx' command."
  (pcomplete-here-using-help "ptx --help"))

;;;###autoload
(defun pcomplete/tsort ()
  "Completion for the `tsort' command."
  (pcomplete-here-using-help "tsort --help"))

;;;###autoload
(defun pcomplete/cut ()
  "Completion for the `cut' command."
  (pcomplete-here-using-help "cut --help"))

;;;###autoload
(defun pcomplete/paste ()
  "Completion for the `paste' command."
  (pcomplete-here-using-help "paste --help"))

;;;###autoload
(defun pcomplete/join ()
  "Completion for the `join' command."
  (pcomplete-here-using-help "join --help"))

;;;###autoload
(defun pcomplete/tr ()
  "Completion for the `tr' command."
  (pcomplete-here-using-help "tr --help"))

;;;###autoload
(defun pcomplete/expand ()
  "Completion for the `expand' command."
  (pcomplete-here-using-help "expand --help"))

;;;###autoload
(defun pcomplete/unexpand ()
  "Completion for the `unexpand' command."
  (pcomplete-here-using-help "unexpand --help"))

;;;###autoload
(defun pcomplete/ls ()
  "Completion for the `ls' command."
  (pcomplete-here-using-help "ls --help"))
;;;###autoload(defalias 'pcomplete/dir 'pcomplete/ls)
;;;###autoload(defalias 'pcomplete/vdir 'pcomplete/ls)

;;;###autoload
(defun pcomplete/cp ()
  "Completion for the `cp' command."
  (pcomplete-here-using-help "cp --help"))

;;;###autoload
(defun pcomplete/dd ()
  "Completion for the `dd' command."
  (let ((operands (pcomplete-from-help "dd --help"
                                       :argument "[a-z]+="
                                       :narrow-start "\n\n"
                                       :narrow-end "\n\n")))
    (while
        (cond ((pcomplete-match "\\`[io]f=\\(.*\\)" 0)
               (pcomplete-here (pcomplete-entries)
                               (pcomplete-match-string 1 0)))
              (t (pcomplete-here operands))))))

;;;###autoload
(defun pcomplete/install ()
  "Completion for the `install' command."
  (pcomplete-here-using-help "install --help"))

;;;###autoload
(defun pcomplete/mv ()
  "Completion for the `mv' command."
  (pcomplete-here-using-help "mv --help"))

;;;###autoload
(defun pcomplete/shred ()
  "Completion for the `shred' command."
  (pcomplete-here-using-help "shred --help"))

;;;###autoload
(defun pcomplete/ln ()
  "Completion for the `ln' command."
  (pcomplete-here-using-help "ln --help"))

;;;###autoload
(defun pcomplete/mkdir ()
  "Completion for the `mkdir' command."
  (pcomplete-here-using-help "mkdir --help"))

;;;###autoload
(defun pcomplete/mkfifo ()
  "Completion for the `mkfifo' command."
  (pcomplete-here-using-help "mkfifo --help"))

;;;###autoload
(defun pcomplete/mknod ()
  "Completion for the `mknod' command."
  (pcomplete-here-using-help "mknod --help"))

;;;###autoload
(defun pcomplete/readlink ()
  "Completion for the `readlink' command."
  (pcomplete-here-using-help "readlink --help"))

;;;###autoload
(defun pcomplete/chown ()
  "Completion for the `chown' command."
  (while (pcomplete-match "\\`-" 0)
    (pcomplete-here (pcomplete-from-help "chown --help")))
  (if (pcomplete-match "\\`[^.]*\\'" 0)
      (pcomplete-here* (pcmpl-unix-user-names))
    (if (pcomplete-match "\\.\\([^.]*\\)\\'" 0)
	(pcomplete-here* (pcmpl-unix-group-names)
			 (pcomplete-match-string 1 0))
      (pcomplete-here*)))
  (while (pcomplete-here (pcomplete-entries))))

;;;###autoload
(defun pcomplete/chgrp ()
  "Completion for the `chgrp' command."
  (while (pcomplete-match "\\`-" 0)
    (pcomplete-here (pcomplete-from-help "chgrp --help")))
  (pcomplete-here* (pcmpl-unix-group-names))
  (while (pcomplete-here (pcomplete-entries))))

;;;###autoload
(defun pcomplete/chmod ()
  "Completion for the `chmod' command."
  (pcomplete-here-using-help "chmod --help"))

;;;###autoload
(defun pcomplete/touch ()
  "Completion for the `touch' command."
  (pcomplete-here-using-help "touch --help"))

;;;###autoload
(defun pcomplete/df ()
  "Completion for the `df' command."
  (pcomplete-here-using-help "df --help"))

;;;###autoload
(defun pcomplete/du ()
  "Completion for the `du' command."
  (pcomplete-here-using-help "du --help"))

;;;###autoload
(defun pcomplete/stat ()
  "Completion for the `stat' command."
  (pcomplete-here-using-help "stat --help"))

;;;###autoload
(defun pcomplete/sync ()
  "Completion for the `sync' command."
  (pcomplete-here-using-help "sync --help"))

;;;###autoload
(defun pcomplete/truncate ()
  "Completion for the `truncate' command."
  (pcomplete-here-using-help "truncate --help"))

;;;###autoload
(defun pcomplete/echo ()
  "Completion for the `echo' command."
  (pcomplete-here-using-help '("echo" "--help")))

;;;###autoload
(defun pcomplete/test ()
  "Completion for the `test' command."
  (pcomplete-here-using-help '("[" "--help")
                             :margin "^ +\\([A-Z]+1 \\)?"))
;;;###autoload(defalias (intern "pcomplete/[") 'pcomplete/test)

;;;###autoload
(defun pcomplete/tee ()
  "Completion for the `tee' command."
  (pcomplete-here-using-help "tee --help"))

;;;###autoload
(defun pcomplete/basename ()
  "Completion for the `basename' command."
  (pcomplete-here-using-help "basename --help"))

;;;###autoload
(defun pcomplete/dirname ()
  "Completion for the `dirname' command."
  (pcomplete-here-using-help "dirname --help"))

;;;###autoload
(defun pcomplete/pathchk ()
  "Completion for the `pathchk' command."
  (pcomplete-here-using-help "pathchk --help"))

;;;###autoload
(defun pcomplete/mktemp ()
  "Completion for the `mktemp' command."
  (pcomplete-here-using-help "mktemp --help"))

;;;###autoload
(defun pcomplete/realpath ()
  "Completion for the `realpath' command."
  (pcomplete-here-using-help "realpath --help"))

;;;###autoload
(defun pcomplete/id ()
  "Completion for the `id' command."
  (while (string-prefix-p "-" (pcomplete-arg 0))
    (pcomplete-here (pcomplete-from-help "id --help")))
  (while (pcomplete-here (pcmpl-unix-user-names))))

;;;###autoload
(defun pcomplete/groups ()
  "Completion for the `groups' command."
  (while (pcomplete-here (pcmpl-unix-user-names))))

;;;###autoload
(defun pcomplete/who ()
  "Completion for the `who' command."
  (pcomplete-here-using-help "who --help"))

;;;###autoload
(defun pcomplete/date ()
  "Completion for the `date' command."
  (pcomplete-here-using-help "date --help"))

;;;###autoload
(defun pcomplete/nproc ()
  "Completion for the `nproc' command."
  (pcomplete-here-using-help "nproc --help"))

;;;###autoload
(defun pcomplete/uname ()
  "Completion for the `uname' command."
  (pcomplete-here-using-help "uname --help"))

;;;###autoload
(defun pcomplete/hostname ()
  "Completion for the `hostname' command."
  (pcomplete-here-using-help "hostname --help"))

;;;###autoload
(defun pcomplete/uptime ()
  "Completion for the `uptime' command."
  (pcomplete-here-using-help "uptime --help"))

;;;###autoload
(defun pcomplete/chcon ()
  "Completion for the `chcon' command."
  (pcomplete-here-using-help "chcon --help"))

;;;###autoload
(defun pcomplete/runcon ()
  "Completion for the `runcon' command."
  (while (string-prefix-p "-" (pcomplete-arg 0))
    (pcomplete-here (pcomplete-from-help "runcon --help"))
    (when (pcomplete-match "\\`-[turl]\\'" 0) (pcomplete-here)))
  (funcall pcomplete-command-completion-function)
  (funcall (or (pcomplete-find-completion-function (pcomplete-arg 1))
	       pcomplete-default-completion-function)))

;;;###autoload
(defun pcomplete/chroot ()
  "Completion for the `chroot' command."
  (while (string-prefix-p "-" (pcomplete-arg 0))
    (pcomplete-here (pcomplete-from-help "chroot --help")))
  (pcomplete-here (pcomplete-dirs))
  (funcall pcomplete-command-completion-function)
  (funcall (or (pcomplete-find-completion-function (pcomplete-arg 1))
	       pcomplete-default-completion-function)))

;;;###autoload
(defun pcomplete/env ()
  "Completion for the `env' command."
  (while (string-prefix-p "-" (pcomplete-arg 0))
    (pcomplete-here (pcomplete-from-help "env --help"))
    (when (pcomplete-match "\\`-[uCS]\\'") (pcomplete-here)))
  (while (pcomplete-match "=" 0) (pcomplete-here)) ; FIXME: Complete env vars
  (funcall pcomplete-command-completion-function)
  (funcall (or (pcomplete-find-completion-function (pcomplete-arg 1))
	       pcomplete-default-completion-function)))

;;;###autoload
(defun pcomplete/nice ()
  "Completion for the `nice' command."
  (while (string-prefix-p "-" (pcomplete-arg 0))
    (pcomplete-here (pcomplete-from-help "nice --help"))
    (pcomplete-here))
  (funcall pcomplete-command-completion-function)
  (funcall (or (pcomplete-find-completion-function (pcomplete-arg 1))
	       pcomplete-default-completion-function)))

;;;###autoload
(defun pcomplete/nohup ()
  "Completion for the `nohup' command."
  (while (string-prefix-p "-" (pcomplete-arg 0))
    (pcomplete-here (pcomplete-from-help "nohup --help")))
  (funcall pcomplete-command-completion-function)
  (funcall (or (pcomplete-find-completion-function (pcomplete-arg 1))
	       pcomplete-default-completion-function)))

;;;###autoload
(defun pcomplete/stdbuf ()
  "Completion for the `stdbuf' command."
  (while (string-prefix-p "-" (pcomplete-arg 0))
    (pcomplete-here (pcomplete-from-help "stdbuf --help"))
    (when (pcomplete-match "\\`-[ioe]\\'") (pcomplete-here)))
  (funcall pcomplete-command-completion-function)
  (funcall (or (pcomplete-find-completion-function (pcomplete-arg 1))
	       pcomplete-default-completion-function)))

;;;###autoload
(defun pcomplete/timeout ()
  "Completion for the `timeout' command."
  (while (string-prefix-p "-" (pcomplete-arg 0))
    (pcomplete-here (pcomplete-from-help "timeout --help"))
    (when (pcomplete-match "\\`-[ks]\\'") (pcomplete-here)))
  (pcomplete-here)                      ; eat DURATION argument
  (funcall pcomplete-command-completion-function)
  (funcall (or (pcomplete-find-completion-function (pcomplete-arg 1))
	       pcomplete-default-completion-function)))

;;;###autoload
(defun pcomplete/numfmt ()
  "Completion for the `numfmt' command."
  (pcomplete-here-using-help "numfmt --help"))

;;;###autoload
(defun pcomplete/seq ()
  "Completion for the `seq' command."
  (pcomplete-here-using-help "seq --help"))

;;; Network commands

;; ssh support by Phil Hagelberg.
;; https://www.emacswiki.org/cgi-bin/wiki/pcmpl-ssh.el

(defun pcmpl-ssh-known-hosts ()
  "Return a list of hosts found in `pcmpl-ssh-known-hosts-file'."
  (when (and pcmpl-ssh-known-hosts-file
             (file-readable-p pcmpl-ssh-known-hosts-file))
    (with-temp-buffer
      (insert-file-contents-literally pcmpl-ssh-known-hosts-file)
      (let ((host-re "\\(?:\\([-.[:alnum:]]+\\)\\|\\[\\([-.[:alnum:]]+\\)\\]:[0-9]+\\)[, ]")
            ssh-hosts-list)
        (while (re-search-forward (concat "^ *" host-re) nil t)
          (push (concat (match-string 1)
                        (match-string 2))
                ssh-hosts-list)
          (while (and (eq (char-before) ?,)
                      (re-search-forward host-re (line-end-position) t))
            (push  (concat (match-string 1)
                           (match-string 2))
                   ssh-hosts-list)))
        ssh-hosts-list))))

(defun pcmpl-ssh-config-hosts ()
  "Return a list of hosts found in `pcmpl-ssh-config-file'."
  (when (and pcmpl-ssh-config-file
             (file-readable-p pcmpl-ssh-config-file))
    (with-temp-buffer
      (insert-file-contents-literally pcmpl-ssh-config-file)
      (let (ssh-hosts-list
            (case-fold-search t))
        (while (re-search-forward "^ *host\\(name\\)? +\\([-.[:alnum:]]+\\)"
                                  nil t)
          (push (match-string 2) ssh-hosts-list))
        ssh-hosts-list))))

(defun pcmpl-ssh-hosts ()
  "Return a list of known SSH hosts.
Uses both `pcmpl-ssh-config-file' and `pcmpl-ssh-known-hosts-file'."
  (let ((hosts (pcmpl-ssh-known-hosts)))
    (dolist (h (pcmpl-ssh-config-hosts))
      (push h hosts))
    hosts))

;;;###autoload
(defun pcomplete/ssh ()
  "Completion rules for the `ssh' command."
  (pcomplete-opt "1246AaCfgKkMNnqsTtVvXxYbcDeFiLlmOopRSw")
  (pcomplete-here (pcmpl-ssh-hosts)))

;;;###autoload
(defalias 'pcomplete/rsh    #'pcomplete/ssh)
(defalias 'pcomplete/rlogin #'pcomplete/ssh)

;;;###autoload
(defun pcomplete/scp ()
  "Completion rules for the `scp' command.
Includes files as well as host names followed by a colon."
  (pcomplete-opt "1246BCpqrvcFiloPS")
  (while t (pcomplete-here
            (lambda (string pred action)
              (let ((table
                     (cond
                      ((string-match "\\`[^:/]+:" string) ; Remote file name.
		       (if (and (eq action 'lambda)
				(eq (match-end 0) (length string)))
			   ;; Avoid connecting to the remote host when we're
			   ;; only completing the host name.
			   (list string)
			 (completion-table-subvert (pcomplete-all-entries)
                                                   "" "/ssh:")))
                      ((string-search "/" string) ; Local file name.
                       (pcomplete-all-entries))
                      (t                ;Host name or local file name.
                       (append (all-completions string (pcomplete-all-entries))
                               (mapcar (lambda (host) (concat host ":"))
                                       (pcmpl-ssh-hosts)))))))
                (complete-with-action action table string pred))))))

(defsubst pcmpl-unix-complete-hostname ()
  "Complete a command that wants a hostname for an argument."
  (pcomplete-here (pcomplete-read-host-names)))

(defalias 'pcomplete/ftp    'pcmpl-unix-complete-hostname)
(defalias 'pcomplete/ncftp  'pcmpl-unix-complete-hostname)
(defalias 'pcomplete/ping   'pcmpl-unix-complete-hostname)

;;;###autoload
(defun pcomplete/telnet ()
  (pcomplete-opt "xl(pcmpl-unix-user-names)")
  (pcmpl-unix-complete-hostname))

;;; Miscellaneous

;;;###autoload
(defun pcomplete/sudo ()
  "Completion for the `sudo' command."
  (while (string-prefix-p "-" (pcomplete-arg 0))
    (pcomplete-here (pcomplete-from-help "sudo --help"))
    (when (pcomplete-match "\\`-[CDghpRtTUu]\\'") (pcomplete-here)))
  (funcall pcomplete-command-completion-function)
  (funcall (or (pcomplete-find-completion-function (pcomplete-arg 1))
	       pcomplete-default-completion-function)))

(provide 'pcmpl-unix)

;;; pcmpl-unix.el ends here
