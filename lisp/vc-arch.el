;;; vc-arch.el --- VC backend for the Arch version-control system

;; Copyright (C) 1995, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
;;           Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

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
;; - Open a file.
;; - C-x v =  without any prefix arg.
;; - C-x v v  to commit a change to a single file.

;; Bugs:

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

(eval-when-compile (require 'vc) (require 'cl))

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
;;;###autoload   (if (vc-find-root file "{arch}/=tagging-method")
;;;###autoload       (progn
;;;###autoload         (load "vc-arch")
;;;###autoload         (vc-arch-registered file))))

(defun vc-arch-add-tagline ()
  "Add an `arch-tag' to the end of the current file."
  (interactive)
  (comment-normalize-vars)
  (goto-char (point-max))
  (forward-comment -1)
  (unless (bolp) (insert "\n"))
  (let ((beg (point))
	(idfile (and buffer-file-name
		     (expand-file-name
		      (concat ".arch-ids/"
			      (file-name-nondirectory buffer-file-name)
			      ".id")
		      (file-name-directory buffer-file-name)))))
    (insert "arch-tag: ")
    (if (and idfile (file-exists-p idfile))
	;; If the file is unreadable, we do want to get an error here.
	(progn
	  (insert-file-contents idfile)
	  (forward-line 1)
	  (delete-file idfile))
      (condition-case nil
	  (call-process "uuidgen" nil t)
	(file-error (insert (format "%s <%s> %s"
				    (current-time-string)
				    user-mail-address
				    (+ (nth 2 (current-time))
				       (buffer-size)))))))
    (comment-region beg (point))))

(defconst vc-arch-tagline-re "^\\W*arch-tag:[ \t]*\\(.*[^ \t\n]\\)")

(defun vc-arch-file-source-p (file)
  "Can return nil, `maybe' or a non-nil value.
Only the value `maybe' can be trusted :-(."
  ;; FIXME: Check the tag and name of parent dirs.
  (unless (string-match "\\`[,+]" (file-name-nondirectory file))
    (or (string-match "\\`{arch}/"
		      (file-relative-name file (vc-arch-root file)))
	(file-exists-p
	 ;; Check the presence of an ID file.
	 (expand-file-name
	  (concat ".arch-ids/" (file-name-nondirectory file) ".id")
	  (file-name-directory file)))
	;; Check the presence of a tagline.
	(with-current-buffer (find-file-noselect file)
	  (save-excursion
	    (goto-char (point-max))
	    (or (re-search-backward vc-arch-tagline-re (- (point) 1000) t)
		(progn
		  (goto-char (point-min))
		  (re-search-forward vc-arch-tagline-re (+ (point) 1000) t)))))
	;; FIXME: check =tagging-method to see whether untagged files might
	;; be source or not.
	(with-current-buffer
	    (find-file-noselect (expand-file-name "{arch}/=tagging-method"
						  (vc-arch-root file)))
	  (let ((untagged-source t))	;Default is `names'.
	    (save-excursion
	      (goto-char (point-min))
	      (if (re-search-forward "^[ \t]*\\(\\(tagline\\|implicit\\|names\\)\\|explicit\\)" nil t)
		  (setq untagged-source (match-end 2)))
	      (if (re-search-forward "^[ \t]*untagged-source[ \t]+\\(\\(source\\)\\|precious\\|backup\\|junk\\|unrecognized\\)" nil t)
		  (setq untagged-source (match-end 2))))
	    (if untagged-source 'maybe))))))

(defun vc-arch-file-id (file)
  ;; Don't include the kind of ID this is because it seems to be too messy.
  (let ((idfile (expand-file-name
		 (concat ".arch-ids/" (file-name-nondirectory file) ".id")
		 (file-name-directory file))))
    (if (file-exists-p idfile)
	(with-temp-buffer
	  (insert-file-contents idfile)
	  (looking-at ".*[^ \n\t]")
	  (match-string 0)))
      (with-current-buffer (find-file-noselect file)
	(save-excursion
	  (goto-char (point-max))
	  (if (or (re-search-backward vc-arch-tagline-re (- (point) 1000) t)
		  (progn
		    (goto-char (point-min))
		    (re-search-forward vc-arch-tagline-re (+ (point) 1000) t)))
	      (match-string 1)
	    (concat "./" (file-relative-name file (vc-arch-root file))))))))

(defun vc-arch-tagging-method (file)
  (with-current-buffer
      (find-file-noselect
       (expand-file-name "{arch}/=tagging-method" (vc-arch-root file)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward
	   "^[ \t]*\\(tagline\\|implicit\\|names\\|explicit\\)" nil t)
	  (intern (match-string 1))
	'names))))

(defun vc-arch-root (file)
  "Return the root directory of a Arch project, if any."
  (or (vc-file-getprop file 'arch-root)
      (vc-file-setprop
       ;; Check the =tagging-method, in case someone naively manually
       ;; creates a {arch} directory somewhere.
       file 'arch-root (vc-find-root file "{arch}/=tagging-method"))))

(defun vc-arch-register (file &optional rev comment)
  (if rev (error "Explicit initial revision not supported for Arch"))
  (let ((tagmet (vc-arch-tagging-method file)))
    (if (and (memq tagmet '(tagline implicit)) comment-start)
	(with-current-buffer (find-file-noselect file)
	  (if (buffer-modified-p)
	      (error "Save %s first" (buffer-name)))
	  (vc-arch-add-tagline)
	  (save-buffer))
      (vc-arch-command nil 0 file "add"))))

(defun vc-arch-registered (file)
  ;; Don't seriously check whether it's source or not.  Checking would
  ;; require running TLA, so it's better to not do it, so it also works if
  ;; TLA is not installed.
  (and (vc-arch-root file)
       (vc-arch-file-source-p file)))

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
  (let* ((root (vc-arch-root file))
	 (ver (vc-arch-default-version file))
	 (pat (concat "\\`" (subst-char-in-string ?/ ?% ver)))
	 (dir (expand-file-name ",,inode-sigs/"
				(expand-file-name "{arch}" root)))
	 (sigfile nil))
    (dolist (f (if (file-directory-p dir) (directory-files dir t pat)))
      (if (or (not sigfile) (file-newer-than-file-p f sigfile))
	  (setq sigfile f)))
    (if (not sigfile)
	'edited				;We know nothing.
      (let ((id (vc-arch-file-id file)))
	(setq id (replace-regexp-in-string "[ \t]" "_" id))
	(with-current-buffer (find-file-noselect sigfile)
	  (goto-char (point-min))
	  (while (and (search-forward id nil 'move)
		      (save-excursion
			(goto-char (- (match-beginning 0) 2))
			;; For `names', the lines start with `?./foo/bar'.
			;; For others there's 2 chars before the ./foo/bar.
			(or (not (or (bolp) (looking-at "\n?")))
			    ;; Ignore E_ entries used for foo.id files.
			    (looking-at "E_")))))
	  (if (eobp)
	      ;; ID not found.
	      (if (equal (file-name-nondirectory sigfile)
			 (subst-char-in-string
			  ?/ ?% (vc-arch-workfile-version file)))
		  'added
		;; Might be `added' or `up-to-date' as well.
		;; FIXME: Check in the patch logs to find out.
		'edited)
	    ;; Found the ID, let's check the inode.
	    (if (not (re-search-forward
		      "\t.*mtime=\\([0-9]+\\):size=\\([0-9]+\\)"
		      (line-end-position) t))
		;; Buh?  Unexpected format.
		'edited
	      (let ((ats (file-attributes file)))
		(if (and (eq (nth 7 ats) (string-to-number (match-string 2)))
			 (equal (format-time-string "%s" (nth 5 ats))
				(match-string 1)))
		    'up-to-date
		  'edited)))))))))

(defun vc-arch-workfile-version (file)
  (let* ((root (expand-file-name "{arch}" (vc-arch-root file)))
	 (defbranch (vc-arch-default-version file)))
    (when (and defbranch (string-match "\\`\\(.+@[^/\n]+\\)/\\(\\(\\(.*?\\)\\(?:--.*\\)?\\)--.*\\)\\'" defbranch))
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
	(dolist (file (if (file-directory-p logdir) (directory-files logdir)))
	  ;; Revision names go: base-0, patch-N, version-0, versionfix-M.
	  (when (and (eq (aref file 0) ?v) (not sealed))
	    (setq sealed t rev-nb 0))
	  (if (and (string-match "-\\([0-9]+\\)\\'" file)
		   (setq tmp (string-to-number (match-string 1 file)))
		   (or (not sealed) (eq (aref file 0) ?v))
		   (>= tmp rev-nb))
	      (setq rev-nb tmp rev file)))
	;; Use "none-000" if the tree hasn't yet been committed on the
	;; default branch.  We'll then get "Arch:000[branch]" on the mode-line.
	(concat defbranch "--" (or rev "none-000"))))))


(defcustom vc-arch-mode-line-rewrite
  '(("\\`.*--\\(.*--.*\\)--\\(v?\\).*-\\([0-9]+\\)\\'" . "\\2\\3[\\1]"))
  "Rewrite rules to shorten Arch's revision names on the mode-line."
  :type '(repeat (cons regexp string))
  :group 'vc)

(defun vc-arch-mode-line-string (file)
  "Return string for placement in modeline by `vc-mode-line' for FILE."
  (let ((rev (vc-workfile-version file)))
    (dolist (rule vc-arch-mode-line-rewrite)
      (if (string-match (car rule) rev)
	  (setq rev (replace-match (cdr rule) t nil rev))))
    (format "Arch%c%s"
	    (case (vc-state file)
	      ((up-to-date needs-patch) ?-)
	      (added ?@)
	      (t ?:))
	    rev)))

(defun vc-arch-diff3-rej-p (rej)
  (let ((attrs (file-attributes rej)))
    (and attrs (< (nth 7 attrs) 60)
	 (with-temp-buffer
	   (insert-file-contents rej)
	   (goto-char (point-min))
	   (looking-at "Conflicts occured, diff3 conflict markers left in file\\.")))))

(defun vc-arch-delete-rej-if-obsolete ()
  "For use in `after-save-hook'."
  (save-excursion
    (let ((rej (concat buffer-file-name ".rej")))
      (when (and buffer-file-name (vc-arch-diff3-rej-p rej))
	(if (not (re-search-forward "^<<<<<<< " nil t))
	    ;; The .rej file is obsolete.
	    (condition-case nil (delete-file rej) (error nil)))))))

(defun vc-arch-find-file-hook ()
  (let ((rej (concat buffer-file-name ".rej")))
    (when (and buffer-file-name (file-exists-p rej))
      (if (vc-arch-diff3-rej-p rej)
	  (save-excursion
	    (goto-char (point-min))
	    (if (not (re-search-forward "^<<<<<<< " nil t))
		;; The .rej file is obsolete.
		(condition-case nil (delete-file rej) (error nil))
	      (smerge-mode 1)
	      (add-hook 'after-save-hook
			'vc-arch-delete-rej-if-obsolete nil t)
	      (message "There are unresolved conflicts in this file")))
	(message "There are unresolved conflicts in %s"
		 (file-name-nondirectory rej))))))

(defun vc-arch-find-file-not-found-hook ()
  ;; Do nothing.  We are not sure whether the file is `source' or not,
  ;; so we shouldn't ask the user whether she wants to check it out.
  )

(defun vc-arch-checkout-model (file) 'implicit)

(defun vc-arch-checkin (file rev comment)
  (if rev (error "Committing to a specific revision is unsupported"))
  (let ((summary (file-relative-name file (vc-arch-root file))))
    ;; Extract a summary from the comment.
    (when (or (string-match "\\`Summary:[ \t]*\\(.*[^ \t\n]\\)\\([ \t]*\n\\)*" comment)
	      (string-match "\\`[ \t]*\\(.*[^ \t\n]\\)[ \t]*\\(\n?\\'\\|\n\\([ \t]*\n\\)+\\)" comment))
      (setq summary (match-string 1 comment))
      (setq comment (substring comment (match-end 0))))
    (vc-arch-command nil 0 file "commit" "-s" summary "-L" comment "--"
		     (vc-switches 'Arch 'checkin))))

(defun vc-arch-diff (file &optional oldvers newvers buffer)
  "Get a difference report using Arch between two versions of FILE."
  (if (and newvers
	   (vc-up-to-date-p file)
	   (equal newvers (vc-workfile-version file)))
      ;; Newvers is the base revision and the current file is unchanged,
      ;; so we can diff with the current file.
      (setq newvers nil))
  (if newvers
      (error "Diffing specific revisions not implemented")
    (let* ((async (and (not vc-disable-async-diff) (fboundp 'start-process)))
	   ;; Run the command from the root dir.
	   (default-directory (vc-arch-root file))
	   (status
	    (vc-arch-command
	     (or buffer "*vc-diff*")
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

(defalias 'vc-arch-responsible-p 'vc-arch-root)

(defun vc-arch-command (buffer okstatus file &rest flags)
  "A wrapper around `vc-do-command' for use in vc-arch.el."
  (apply 'vc-do-command buffer okstatus vc-arch-command file flags))

(defun vc-arch-init-version () nil)

(provide 'vc-arch)

;; arch-tag: a35c7c1c-5237-429d-88ef-3d718fd2e704
;;; vc-arch.el ends here
