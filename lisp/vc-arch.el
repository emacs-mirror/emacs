;;; vc-arch.el --- VC backend for the Arch version-control system

;; Copyright (C) 1995,98,99,2000,01,02,03,2004  Free Software Foundation, Inc.

;; Author:      FSF (see vc.el for full credits)
;; Maintainer:  Stefan Monnier <monnier@gnu.org>

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

;; The home page of the Arch version control system is at
;; 
;;      http://www.gnuarch.org/
;; 
;; This is derived from vc-mcvs.el as follows:
;; - cp vc-mcvs.el vc-arch.el and then M-% mcvs RET arch RET
;;
;; Then of course started the hacking.
;;
;; What has been partly tested:
;; - Open a file
;; - C-x v =  without any prefix arg
;; - C-x v v  to commit a change to a single file

;; Bugs:

;; - Opening a new file prompts "blabla was lost; check out? (yes or no)".
;; - *VC-log*'s initial content lacks the `Summary:' lines.
;; - All files under the tree are considered as "under Arch's control"
;;   without regards to =tagging-method and such.
;; - Files are always considered as `edited'.
;; - C-x v l does not work.
;; - C-x v i does not work.
;; - C-x v ~ does not work.
;; - C-x v u does not work.
;; - C-x v s does not work.
;; - C-x v r does not work.
;; - VC-dired does not work.
;; - And more...

;;; Code:

(eval-when-compile (require 'vc))

;;;
;;; Customization options
;;;

(defvar vc-arch-command
  (let ((candidates '("tla")))
    (while (and candidates (not (executable-find (car candidates))))
      (setq candidates (cdr candidates)))
    (or (car candidates) "tla")))

;; Clear up the cache to force vc-call to check again and discover
;; new functions when we reload this file.
(put 'Arch 'vc-functions nil)

;;;###autoload (defun vc-arch-registered (file)
;;;###autoload   (let ((dir file))
;;;###autoload     (while (and (stringp dir)
;;;###autoload                 (not (equal
;;;###autoload                       dir (setq dir (file-name-directory dir))))
;;;###autoload                 dir)
;;;###autoload       (setq dir (if (file-directory-p
;;;###autoload                      (expand-file-name "{arch}" dir))
;;;###autoload                     t (directory-file-name dir))))
;;;###autoload     (if (eq dir t)
;;;###autoload          (progn
;;;###autoload           (load "vc-arch")
;;;###autoload           (vc-arch-registered file)))))

(defun vc-arch-add-tag ()
  "Add an `arch-tag' to the end of the current file."
  (interactive)
  (goto-char (point-max))
  (forward-comment -1)
  (unless (bolp) (insert "\n"))
  (let ((beg (point)))
    (insert "arch-tag: ")
    (call-process "uuidgen" nil t)	;Also inserts a terminal newline.
    (comment-region beg (point))))

(defun vc-arch-root (file)
  "Return the root directory of a Arch project, if any."
  (or (vc-file-getprop file 'arch-root)
      (vc-file-setprop
       file 'arch-root
       (let ((root nil))
	 (while (not (or root
			 (equal file (setq file (file-name-directory file)))
			 (null file)))
	   (if (file-directory-p (expand-file-name "{arch}" file))
	       (setq root file)
	     (setq file (directory-file-name file))))
	 root))))

(defun vc-arch-registered (file)
  ;; Don't check whether it's source or not.  Checking would require
  ;; running TLA, so it's better to not do it, so it also works if TLA is
  ;; not installed.
  (vc-arch-root file))

(defun vc-arch-default-version (file)
  (or (vc-file-getprop (vc-arch-root file) 'arch-default-version)
      (let* ((root (vc-arch-root file))
	     (f (expand-file-name "{arch}/++default-version" root)))
	(if (file-readable-p f)
	    (vc-file-setprop
	     root 'arch-default-version
	     (with-temp-buffer
	       (insert-file-contents f)
	       ;; Strip the terminating newline.
	       (buffer-substring (point-min) (1- (point-max)))))))))

(defun vc-arch-workfile-unchanged-p (file)
  "Check if FILE is unchanged by diffing against the master version.
Return non-nil if FILE is unchanged."
  nil)

(defun vc-arch-state (file)
  ;; There's no checkout operation and merging is not done from VC
  ;; so the only operation that's state dependent that VC supports is commit
  ;; which is only activated if the file is `edited'.
  'edited)

(defun vc-arch-workfile-version (file)
  (let* ((root (expand-file-name "{arch}" (vc-arch-root file)))
	 (defbranch (vc-arch-default-version file)))
    (when (and defbranch (string-match "\\`\\(.+@[^/\n]+\\)/\\(\\(\\(.*\\)--.*\\)--.*\\)\\'" defbranch))
      (let* ((archive (match-string 1 defbranch))
	     (category (match-string 4 defbranch))
	     (branch (match-string 3 defbranch))
	     (version (match-string 2 defbranch))
	     (sealed nil) (rev-nb 0)
	     (rev nil)
	     logdir tmp)
	(setq logdir (expand-file-name category root))
	(setq logdir (expand-file-name branch logdir))
	(setq logdir (expand-file-name version logdir))
	(setq logdir (expand-file-name archive logdir))
	(setq logdir (expand-file-name "patch-log" logdir))
	;; Revision names go: base-0, patch-N, version-0, versionfix-N.
	(dolist (file (directory-files logdir))
	  (when (and (eq (aref file 0) ?v) (not sealed))
	    (setq sealed t rev-nb 0))
	  (if (and (string-match "-\\([0-9]+\\)\\'" file)
		   (setq tmp (string-to-number (match-string 1 file)))
		   (or (not sealed) (eq (aref file 0) ?v))
		   (>= tmp rev-nb))
	      (setq rev-nb tmp rev file)))
	(concat defbranch "--" rev)))))


(defcustom vc-arch-mode-line-rewrite
  '(("\\`.*--\\(.*--.*\\)--\\(v?\\).*-\\([0-9]+\\)\\'" . "\\2\\3[\\1]"))
  "Rewrite rules to shorten Arch's revision names on the mode-line."
  :type '(repeat (cons regexp string)))

(defun vc-arch-mode-line-string (file)
  "Return string for placement in modeline by `vc-mode-line' for FILE."
  (let ((rev (vc-workfile-version file)))
    (dolist (rule vc-arch-mode-line-rewrite)
      (if (string-match (car rule) rev)
	  (setq rev (replace-match (cdr rule) t nil rev))))
    (format "Arch%c%s"
	    (if (memq (vc-state file) '(up-to-date needs-patch)) ?- ?:)
	    rev)))

(defun vc-arch-diff3-rej-p (rej)
  (and (eq (nth 7 (file-attributes rej)) 56)
       (with-temp-buffer
	 (insert-file-contents rej)
	 (goto-char (point-min))
	 (looking-at "Conflicts occured, diff3 conflict markers left in file\\.$"))))

(defun vc-arch-delete-rej-if-obsolete ()
  "For use in `write-file-functions'."
  (let ((rej (concat buffer-file-name ".rej")))
    (when (and buffer-file-name (vc-arch-diff3-rej-p rej))
      (if (not (re-search-forward "^>>>>>>> " nil t))
	  ;; The .rej file is obsolete.
	  (condition-case nil (delete-file rej) (error nil)))))
  ;; This did not save the buffer.
  nil)

(defun vc-arch-find-file-hook ()
  (let ((rej (concat buffer-file-name ".rej")))
    (when (and buffer-file-name (file-exists-p rej))
      (if (vc-arch-diff3-rej-p rej)
	  (save-excursion
	    (goto-char (point-min))
	    (if (not (re-search-forward "^>>>>>>> " nil t))
		;; The .rej file is obsolete.
		(condition-case nil (delete-file rej) (error nil))
	      (smerge-mode 1)
	      (add-hook 'write-file-functions
			'vc-arch-delete-rej-if-obsolete nil t)
	      (message "There are unresolved conflicts in this file")))
	(message "There are unresolved conflicts in %s"
		 (file-name-nondirectory rej))))))

(defun vc-arch-checkout-model (file) 'implicit)

(defun vc-arch-checkin (file rev comment)
  (if rev (error "Committing to a specific revision is unsupported."))
  (let ((summary (file-relative-name file (vc-arch-root file))))
    ;; Extract a summary from the comment.
    (when (or (string-match "\\`Summary:[ \t]*\\(.*[^ \t\n]\\)\\([ \t]*\n\\)*" comment)
	      (string-match "\\`[ \t]*\\(.*[^ \t\n]\\)[ \t]*\\(\n?\\'\\|\n\\([ \t]*\n\\)+\\)" comment))
      (setq summary (match-string 1 comment))
      (setq comment (substring comment (match-end 0))))
    (vc-arch-command nil 0 file "commit" "-s" summary "-L" comment "--"
		     (vc-switches 'Arch 'checkin))))

(defun vc-arch-diff (file &optional oldvers newvers)
  "Get a difference report using Arch between two versions of FILE."
  (if newvers
      (error "Diffing specific revisions not implemented.")
    (let* ((async (fboundp 'start-process))
	   ;; Run the command from the root dir.
	   (default-directory (vc-arch-root file))
	   (status
	    (vc-arch-command
	     "*vc-diff*"
	     (if async 'async 1)
	     nil "file-diffs"
	     ;; Arch does not support the typical flags.
	     ;; (vc-switches 'Arch 'diff)
	     (file-relative-name file)
	     (if (equal oldvers (vc-workfile-version file))
		 nil
	       oldvers))))
      (if async 1 status))))	       ; async diff, pessimistic assumption.

(defun vc-arch-delete-file (file)
  (vc-arch-command nil 0 file "rm"))

(defun vc-arch-rename-file (old new)
  (vc-arch-command nil 0 new "mv" (file-relative-name old)))

(defun vc-arch-command (buffer okstatus file &rest flags)
  "A wrapper around `vc-do-command' for use in vc-arch.el."
  (apply 'vc-do-command buffer okstatus vc-arch-command file flags))

(provide 'vc-arch)

;;; arch-tag: a35c7c1c-5237-429d-88ef-3d718fd2e704
;;; vc-arch.el ends here
