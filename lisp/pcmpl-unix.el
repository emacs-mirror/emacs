;;; pcmpl-unix.el --- standard UNIX completions

;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'pcomplete)

;; User Variables:

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
This allows completion of SSH host names.  Note that newer
versions of ssh hash the hosts by default to prevent
Island-hopping SSH attacks.  This can be disabled, at some risk,
with the SSH option \"HashKnownHosts no\"."
  :type '(choice file (const nil))
  :group 'pcmpl-unix
  :version "23.1")

;; Functions:

;;;###autoload
(defun pcomplete/cd ()
  "Completion for `cd'."
  (pcomplete-here (pcomplete-dirs)))

;;;###autoload
(defalias 'pcomplete/pushd 'pcomplete/cd)

;;;###autoload
(defun pcomplete/rmdir ()
  "Completion for `rmdir'."
  (while (pcomplete-here (pcomplete-dirs))))

;;;###autoload
(defun pcomplete/rm ()
  "Completion for `rm'."
  (let ((pcomplete-help "(fileutils)rm invocation"))
    (pcomplete-opt "dfirRv")
    (while (pcomplete-here (pcomplete-all-entries) nil
			   'expand-file-name))))

;;;###autoload
(defun pcomplete/xargs ()
  "Completion for `xargs'."
  (pcomplete-here (funcall pcomplete-command-completion-function))
  (funcall (or (pcomplete-find-completion-function (pcomplete-arg 1))
	       pcomplete-default-completion-function)))

;;;###autoload
(defalias 'pcomplete/time 'pcomplete/xargs)

;;;###autoload
(defun pcomplete/which ()
  "Completion for `which'."
  (while (pcomplete-here (funcall pcomplete-command-completion-function))))

(defun pcmpl-unix-read-passwd-file (file)
  "Return an alist correlating gids to group names in FILE."
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
    (pcomplete-uniqify-list names)))

(defsubst pcmpl-unix-group-names ()
  "Read the contents of /etc/group for group names."
  (if pcmpl-unix-group-file
      (pcmpl-unix-read-passwd-file pcmpl-unix-group-file)))

(defsubst pcmpl-unix-user-names ()
  "Read the contents of /etc/passwd for user names."
  (if pcmpl-unix-passwd-file
      (pcmpl-unix-read-passwd-file pcmpl-unix-passwd-file)))

;;;###autoload
(defun pcomplete/chown ()
  "Completion for the `chown' command."
  (unless (pcomplete-match "\\`-")
    (if (pcomplete-match "\\`[^.]*\\'" 0)
	(pcomplete-here* (pcmpl-unix-user-names))
      (if (pcomplete-match "\\.\\([^.]*\\)\\'" 0)
	  (pcomplete-here* (pcmpl-unix-group-names)
			   (pcomplete-match-string 1 0))
	(pcomplete-here*))))
  (while (pcomplete-here (pcomplete-entries))))

;;;###autoload
(defun pcomplete/chgrp ()
  "Completion for the `chgrp' command."
  (unless (pcomplete-match "\\`-")
    (pcomplete-here* (pcmpl-unix-group-names)))
  (while (pcomplete-here (pcomplete-entries))))


;; ssh support by Phil Hagelberg.
;; http://www.emacswiki.org/cgi-bin/wiki/pcmpl-ssh.el

(defun pcmpl-ssh-hosts ()
  "Return a list of hosts found in `pcmpl-ssh-known-hosts-file'."
  (when (and pcmpl-ssh-known-hosts-file
             (file-readable-p pcmpl-ssh-known-hosts-file))
    (with-temp-buffer
      (insert-file-contents-literally pcmpl-ssh-known-hosts-file)
      (let (ssh-hosts-list)
        (while (re-search-forward "^ *\\([-.[:alnum:]]+\\)[, ]" nil t)
          (add-to-list 'ssh-hosts-list (match-string 1))
          (while (and (looking-back ",")
                      (re-search-forward "\\([-.[:alnum:]]+\\)[, ]"
                                         (line-end-position) t))
            (add-to-list 'ssh-hosts-list (match-string 1))))
        ssh-hosts-list))))

;;;###autoload
(defun pcomplete/ssh ()
  "Completion rules for the `ssh' command."
  (pcomplete-opt "1246AaCfgKkMNnqsTtVvXxYbcDeFiLlmOopRSw" nil t)
  (pcomplete-here (pcmpl-ssh-hosts)))

;;;###autoload
(defun pcomplete/scp ()
  "Completion rules for the `scp' command.
Includes files as well as host names followed by a colon."
  (pcomplete-opt "1246BCpqrvcFiloPS")
  (while t (pcomplete-here (append (pcomplete-all-entries)
                                   (mapcar (lambda (host)
                                             (concat host ":"))
                                           (pcmpl-ssh-hosts))))))

(provide 'pcmpl-unix)

;; arch-tag: 3f9eb5af-7e0e-449d-b586-381cbbf8fc5c
;;; pcmpl-unix.el ends here
