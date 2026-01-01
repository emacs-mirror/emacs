;;; pcmpl-linux.el --- functions for dealing with GNU/Linux completions  -*- lexical-binding: t -*-

;; Copyright (C) 1999-2026 Free Software Foundation, Inc.

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

;; These functions are for use with GNU/Linux.  Since they depend on a
;; certain knowledge of the layout of such systems, they probably
;; won't work very well on other operating systems.

;;; Code:

(provide 'pcmpl-linux)

(require 'pcomplete)
(eval-when-compile (require 'rx))

;; Functions:

;;;###autoload
(defun pcomplete/kill ()
  "Completion for GNU/Linux `kill', using /proc filesystem."
  (if (pcomplete-match "^-\\(.*\\)" 0)
      (pcomplete-here
       (pcomplete-uniquify-list
	(split-string
	 (pcomplete-process-result "kill" "-l")))
       (pcomplete-match-string 1 0)))
  (while (pcomplete-here
	  (if (file-directory-p "/proc")
              (directory-files "/proc" nil "\\`[0-9]+\\'"))
	  nil #'identity)))

;;;###autoload
(defun pcomplete/umount ()
  "Completion for GNU/Linux `umount'."
  (pcomplete-opt "hVafrnvt(pcmpl-linux-fs-types)")
  (while (pcomplete-here (pcmpl-linux-mounted-directories)
			 nil #'identity)))

;;;###autoload
(defun pcomplete/mount ()
  "Completion for GNU/Linux `mount'."
  (pcomplete-opt "hVanfFrsvwt(pcmpl-linux-fs-types)o?L?U?")
  (while (pcomplete-here (pcomplete-entries) nil #'identity)))

(defvar pcmpl-linux-fs-modules-path-format "/lib/modules/%s/kernel/fs/")

(defun pcmpl-linux-fs-types ()
  "Return a list of available fs modules on GNU/Linux systems."
  (let ((kernel-ver (pcomplete-process-result "uname" "-r")))
    (directory-files
     (format pcmpl-linux-fs-modules-path-format kernel-ver))))

(defvar pcmpl-linux-mtab-file "/etc/mtab")

(defun pcmpl-linux-mounted-directories ()
  "Return a list of mounted directory names."
  (let (points)
    (when (file-readable-p pcmpl-linux-mtab-file)
      (with-temp-buffer
        (insert-file-contents-literally pcmpl-linux-mtab-file)
	(while (not (eobp))
	  (let* ((line (buffer-substring (point) (line-end-position)))
		 (args (split-string line " ")))
	    (setq points (cons (nth 1 args) points)))
	  (forward-line)))
      (pcomplete-uniquify-list points))))

(defun pcomplete-pare-list (l r)
  "Destructively remove from list L all elements matching any in list R.
Test is done using `equal'."
  (while (and l (and r (member (car l) r)))
    (setq l (cdr l)))
  (let ((m l))
    (while m
      (while (and (cdr m)
		  (and r (member (cadr m) r)))
	(setcdr m (cddr m)))
      (setq m (cdr m))))
  l)

(defun pcmpl-linux-mountable-directories ()
  "Return a list of mountable directory names."
  (let (points)
    (when (file-readable-p "/etc/fstab")
      (with-temp-buffer
	(insert-file-contents-literally "/etc/fstab")
	(while (not (eobp))
	  (let* ((line (buffer-substring (point) (line-end-position)))
		 (args (split-string line "\\s-+")))
	    (setq points (cons (nth 1 args) points)))
	  (forward-line)))
      (pcomplete-pare-list
       (pcomplete-uniquify-list points)
       (cons "swap" (pcmpl-linux-mounted-directories))))))

;;; systemd

(defun pcmpl-linux--systemd-units (&rest args)
  "Run `systemd list-units ARGS' and return the output as a list."
  (with-temp-buffer
    (apply #'call-process
           "systemctl" nil '(t nil) nil
           ;; "--legend=no" doesn't exist before systemd v248
           "list-units" "--full" "--no-legend" "--plain" args)
    (goto-char (point-min))
    (let (result)
      (while (re-search-forward (rx bol (group (+ (not space)))
                                    (+ space) (+ (not space))
                                    (+ space) (group (+ (not space)))
                                    (+ space) (+ (not space))
                                    (+ space) (group (* nonl)))
                                nil t)
        (push (match-string 1) result)
        (put-text-property 0 1 'pcomplete-annotation
                           (concat " " (match-string 2))
                           (car result))
        (put-text-property 0 1 'pcomplete-description
                           (match-string 3)
                           (car result)))
      (nreverse result))))

;;;###autoload
(defun pcomplete/systemctl ()
  "Completion for the `systemctl' command."
  (let ((subcmds (pcomplete-from-help
                  "systemctl --help"
                  :margin (rx bol "  " (group) alpha)
                  :argument (rx (+ (any alpha ?-)))
                  :metavar (rx (group (+ " " (>= 2 (any upper "[]|."))))))))
    (while (not (member (pcomplete-arg 1) subcmds))
      (if (string-prefix-p "-" (pcomplete-arg 0))
          (pcomplete-here (pcomplete-from-help "systemctl --help"
                                               :metavar "[^ ]+"
                                               :separator " \\(\\)-"))
        (pcomplete-here subcmds)))
    (let ((subcmd (pcomplete-arg 1))
          (context (if (member "--user" pcomplete-args) "--user" "--system")))
      (while (pcase subcmd
               ((guard (string-prefix-p "-" (pcomplete-arg 0)))
                (pcomplete-here
                 (pcomplete-from-help "systemctl --help")))
               ;; TODO: suggest only relevant units to each subcommand
               ("start"
                (pcomplete-here
                 (pcmpl-linux--systemd-units context "--state" "inactive,failed")))
               ((or "restart" "stop")
                (pcomplete-here
                 (pcmpl-linux--systemd-units context "--state" "active")))
               (_ (pcomplete-here
                   (completion-table-in-turn
                    (pcmpl-linux--systemd-units context "--all")
                    (pcomplete-entries)))))))))

;;;###autoload
(defun pcomplete/journalctl ()
  "Completion for the `journalctl' command."
  (while (if (string-prefix-p "-" (pcomplete-arg 0))
             (pcomplete-here (pcomplete-from-help "journalctl --help"
                                                  :metavar "[^ ]+"
                                                  :separator " \\(\\)-"))
           (pcomplete-here (mapcar (lambda (s) (concat s "="))
                                   (process-lines "journalctl" "--fields"))))))

;;; pcmpl-linux.el ends here
