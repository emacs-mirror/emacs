;;; vc-hooks.el --- resident support for version-control

;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Modified by:
;;   Per Cederqvist <ceder@lysator.liu.se>
;;   Andre Spiegel <spiegel@berlin.informatik.uni-stuttgart.de>

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This is the always-loaded portion of VC.
;; It takes care VC-related activities that are done when you visit a file,
;; so that vc.el itself is loaded only when you use a VC command.
;; See the commentary of vc.el.

;;; Code:

(defvar vc-master-templates
  '(("%sRCS/%s,v" . RCS) ("%s%s,v" . RCS) ("%sRCS/%s" . RCS)
    ("%sSCCS/s.%s" . SCCS) ("%ss.%s". SCCS)
    vc-find-cvs-master)
  "*Where to look for version-control master files.
The first pair corresponding to a given back end is used as a template
when creating new masters.")

(defvar vc-make-backup-files nil
  "*If non-nil, backups of registered files are made as with other files.
If nil (the default), files covered by version control don't get backups.")

(defvar vc-display-status t
  "*If non-nil, display revision number and lock status in modeline.
Otherwise, not displayed.")

;; Tell Emacs about this new kind of minor mode
(if (not (assoc 'vc-mode minor-mode-alist))
    (setq minor-mode-alist (cons '(vc-mode vc-mode)
				 minor-mode-alist)))

(make-variable-buffer-local 'vc-mode)
(put 'vc-mode 'permanent-local t)

;; We need a notion of per-file properties because the version
;; control state of a file is expensive to derive --- we compute
;; them when the file is initially found, keep them up to date 
;; during any subsequent VC operations, and forget them when
;; the buffer is killed.

(defmacro vc-error-occurred (&rest body)
  (list 'condition-case nil (cons 'progn (append body '(nil))) '(error t)))

(defvar vc-file-prop-obarray [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
  "Obarray for per-file properties.")

(defvar vc-buffer-backend t)
(make-variable-buffer-local 'vc-buffer-backend)

(defun vc-file-setprop (file property value)
  ;; set per-file property
  (put (intern file vc-file-prop-obarray) property value))

(defun vc-file-getprop (file property)
  ;; get per-file property
  (get (intern file vc-file-prop-obarray) property))

;;; functions that operate on RCS revision numbers

(defun vc-occurrences (object sequence)
  ;; return the number of occurences of OBJECT in SEQUENCE
  ;; (is it really true that Emacs Lisp doesn't provide such a function?)
  (let ((len (length sequence)) (index 0) (occ 0))
    (while (< index len)
      (if (eq object (elt sequence index))
	  (setq occ (1+ occ)))
      (setq index (1+ index)))
    occ))

(defun vc-trunk-p (rev)
  ;; return t if REV is a revision on the trunk
  (not (eq nil (string-match "\\`[0-9]+\\.[0-9]+\\'" rev))))

(defun vc-branch-p (rev)
  ;; return t if REV is the branch part of a revision, 
  ;; i.e. a revision without a minor number
  (eq 0 (% (vc-occurrences ?. rev) 2)))

(defun vc-minor-revision (rev)
  ;; return the minor revision number of REV, 
  ;; i.e. the number after the last dot.
  (substring rev (1+ (string-match "\\.[0-9]+\\'" rev))))

(defun vc-branch-part (rev)
  ;; return the branch part of a revision number REV
  (substring rev 0 (string-match "\\.[0-9]+\\'" rev)))

;;; actual version-control code starts here

(defun vc-registered (file)
  (let (handler handlers)
    (if (boundp 'file-name-handler-alist)
	(setq handler (find-file-name-handler file 'vc-registered)))
    (if handler
	(funcall handler 'vc-registered file)
      ;; Search for a master corresponding to the given file
      (let ((dirname (or (file-name-directory file) ""))
	    (basename (file-name-nondirectory file)))
	(catch 'found
	  (mapcar
	   (function (lambda (s)
	      (if (atom s)
		  (funcall s dirname basename)
		(let ((trial (format (car s) dirname basename)))
		  (if (and (file-exists-p trial)
			   ;; Make sure the file we found with name
			   ;; TRIAL is not the source file itself.
			   ;; That can happen with RCS-style names
			   ;; if the file name is truncated
			   ;; (e.g. to 14 chars).  See if either
			   ;; directory or attributes differ.
			   (or (not (string= dirname
					     (file-name-directory trial)))
			       (not (equal
				     (file-attributes file)
				     (file-attributes trial)))))
		      (throw 'found (cons trial (cdr s))))))))
	   vc-master-templates)
	  nil)))))

(defun vc-find-cvs-master (dirname basename)
  ;; Check if DIRNAME/BASENAME is handled by CVS.
  ;; If it is, do a (throw 'found (cons MASTER 'CVS)).
  ;; Note: If the file is ``cvs add''ed but not yet ``cvs commit''ed 
  ;; the MASTER will not actually exist yet.  The other parts of VC
  ;; checks for this condition.  This function returns nil if 
  ;; DIRNAME/BASENAME is not handled by CVS.
  (if (and (file-directory-p (concat dirname "CVS/"))
	   (file-readable-p (concat dirname "CVS/Entries")))
      (let ((bufs nil) (fold case-fold-search))
	(unwind-protect
	    (save-excursion
	      (setq bufs (list
			  (find-file-noselect (concat dirname "CVS/Entries"))))
	      (set-buffer (car bufs))
	      (goto-char (point-min))
	      ;; make sure the file name is searched 
	      ;; case-sensitively
	      (setq case-fold-search nil)
	      (cond
	       ((re-search-forward
		 (concat "^/" (regexp-quote basename) "/\\([^/]*\\)/")
		 nil t)
		(setq case-fold-search fold)  ;; restore the old value
		;; We found it.  Store away version number, now
		;; that we are anyhow so close to finding it.
		(vc-file-setprop (concat dirname basename) 
				 'vc-workfile-version
				 (buffer-substring (match-beginning 1)
						   (match-end 1)))
		(setq bufs (cons (find-file-noselect 
				  (concat dirname "CVS/Repository"))
				 bufs))
		(set-buffer (car bufs))
		(let ((master
		       (concat (file-name-as-directory 
				(buffer-substring (point-min)
						  (1- (point-max))))
			       basename
			       ",v")))
		  (throw 'found (cons master 'CVS))))
	       (t (setq case-fold-search fold)  ;; restore the old value
		  nil)))
	  (mapcar (function kill-buffer) bufs)))))

(defun vc-name (file)
  "Return the master name of a file, nil if it is not registered."
  (or (vc-file-getprop file 'vc-name)
      (let ((name-and-type (vc-registered file)))
	(if name-and-type
	    (progn
	      (vc-file-setprop file 'vc-backend (cdr name-and-type))
	      (vc-file-setprop file 'vc-name (car name-and-type)))))))

(defun vc-backend-deduce (file)
  "Return the version-control type of a file, nil if it is not registered."
  (and file
       (or (vc-file-getprop file 'vc-backend)
	   (let ((name-and-type (vc-registered file)))
	     (if name-and-type
		 (progn
		   (vc-file-setprop file 'vc-name (car name-and-type))
		   (vc-file-setprop file 'vc-backend (cdr name-and-type))))))))

(defun vc-buffer-backend ()
  "Return the version-control type of the visited file, or nil if none."
  (if (eq vc-buffer-backend t)
      (setq vc-buffer-backend (vc-backend-deduce (buffer-file-name)))
    vc-buffer-backend))

(defun vc-toggle-read-only (&optional verbose)
  "Change read-only status of current buffer, perhaps via version control.
If the buffer is visiting a file registered with version control,
then check the file in or out.  Otherwise, just change the read-only flag
of the buffer.  With prefix argument, ask for version number."
  (interactive "P")
  (if (vc-backend-deduce (buffer-file-name))
      (vc-next-action verbose)
    (toggle-read-only)))
(define-key global-map "\C-x\C-q" 'vc-toggle-read-only)

(defun vc-mode-line (file &optional label)
  "Set `vc-mode' to display type of version control for FILE.
The value is set in the current buffer, which should be the buffer
visiting FILE.  Second optional arg LABEL is put in place of version
control system name."
  (interactive (list buffer-file-name nil))
  (let ((vc-type (vc-backend-deduce file)))
    (setq vc-mode
	  (concat " " (or label (symbol-name vc-type))
		  (if vc-display-status (vc-status file vc-type))))
;;;    ;; Make the buffer read-only if the file is not locked
;;;    ;; (or unchanged, in the CVS case).
;;;    (if (not (vc-locking-user file))
;;;	(setq buffer-read-only t))
    ;; Even root shouldn't modify a registered file without
    ;; locking it first.
    (and vc-type
	 (not buffer-read-only)
	 (zerop (user-uid))
	 (require 'vc)
	 (not (equal (user-login-name) (vc-locking-user file)))
	 (setq buffer-read-only t))
    (and (null vc-type)
	 (file-symlink-p file)
	 (let ((link-type (vc-backend-deduce (file-symlink-p file))))
	   (if link-type
	       (message
		"Warning: symbolic link to %s-controlled source file"
		link-type))))
    (force-mode-line-update)
    ;;(set-buffer-modified-p (buffer-modified-p)) ;;use this if Emacs 18
    vc-type))

(defun vc-status (file vc-type)
  ;; Return string for placement in modeline by `vc-mode-line'.
  ;; If FILE is not registered, return nil.
  ;; If FILE is registered but not locked, return " REV" if there is a head
  ;; revision and " @@" otherwise.
  ;; If FILE is locked then return all locks in a string of the
  ;; form " LOCKER1:REV1 LOCKER2:REV2 ...", where "LOCKERi:" is empty if you
  ;; are the locker, and otherwise is the name of the locker followed by ":".

  ;; Algorithm: 

  ;; Check for master file corresponding to FILE being visited.
  ;; 
  ;; RCS: Insert the first few characters of the master file into a
  ;; work buffer.  Search work buffer for "locks...;" phrase; if not
  ;; found, then keep inserting more characters until the phrase is
  ;; found.  Extract the locks, and remove control characters
  ;; separating them, like newlines; the string " user1:revision1
  ;; user2:revision2 ..." is returned.
  ;;
  ;; SCCS: Check if the p-file exists.  If it does, read it and
  ;; extract the locks, giving them the right format.  Else use prs to
  ;; find the revision number.
  ;;
  ;; CVS: vc-find-cvs-master has already stored the current revision
  ;; number.  Fetch it from the file property.
  
  ;; Limitations:

  ;; The output doesn't show which version you are actually looking at.
  ;; The modeline can get quite cluttered when there are multiple locks.
  ;; The head revision is probably not what you want if you've used `rcs -b'.

  (let ((master (vc-name file))
	found
	status)

    ;; If master file exists, then parse its contents, otherwise we
    ;; return the nil value of this if form.
    (if (and master vc-type)
        (save-excursion

          ;; Create work buffer.
          (set-buffer (get-buffer-create " *vc-status*"))
          (setq buffer-read-only nil
                default-directory (file-name-directory master))
          (erase-buffer)

	  ;; Set the `status' var to the return value.
	  (cond

	   ;; RCS code.
	   ((eq vc-type 'RCS)
	    ;; Check if we have enough of the header.
	    ;; If not, then keep including more.
	    (while
		(not (or found
			 (let ((s (buffer-size)))
			   (goto-char (1+ s))
			   (zerop (car (cdr (insert-file-contents
					     master nil s (+ s 8192))))))))
	      (beginning-of-line)
	      (setq found (re-search-forward "^locks\\([^;]*\\);" nil t)))

	    (if found
		;; Clean control characters and self-locks from text.
		(let* ((lock-pattern
			(concat "[ \b\t\n\v\f\r]+\\("
				(regexp-quote (user-login-name))
				":\\)?"))
		       (locks
			(save-restriction
			  (narrow-to-region (match-beginning 1) (match-end 1))
			  (goto-char (point-min))
			  (while (re-search-forward lock-pattern nil t)
			    (replace-match (if (eobp) "" ":") t t))
			  (buffer-string))))
		  (setq status
			(if (not (string-equal locks ""))
			    locks
			  (goto-char (point-min))
			  (if (looking-at "head[ \b\t\n\v\f\r]+\\([.0-9]+\\)")
			      (concat "-"
				      (buffer-substring (match-beginning 1)
							(match-end 1)))
			    " @@"))))))

	   ;; SCCS code.
	   ((eq vc-type 'SCCS)
	    ;; Build the name of the p-file and put it in the work buffer.
	    (insert master)
	    (search-backward "/s.")
	    (delete-char 2)
	    (insert "/p")
	    (if (not (file-exists-p (buffer-string)))
		;; No lock.
		(let ((exec-path (if vc-path (append exec-path vc-path)
				   exec-path)))
		  (erase-buffer)
		  (insert "-")
		  (if (zerop (call-process "prs" nil t nil "-d:I:" master))
		      (setq status (buffer-substring 1 (1- (point-max))))))
	      ;; Locks exist.
	      (insert-file-contents (buffer-string) nil nil nil t)
	      (while (looking-at "[^ ]+ \\([^ ]+\\) \\([^ ]+\\).*\n")
		(replace-match " \\2:\\1"))
	      (setq status (buffer-string))
	      (aset status 0 ?:)))
	   ;; CVS code.
	   ((eq vc-type 'CVS)
	    (let ((version (vc-file-getprop
			    file 'vc-your-latest-version)))
	      (setq status (concat ":" (if (string= "0" version)
					   " @@" ;added, not yet committed.
					 version))))))

	  ;; Clean work buffer.
	  (erase-buffer)
	  (set-buffer-modified-p nil)
	  status))))

(defun vc-file-clearprops (file)
  ;; clear all properties of a given file
  (setplist (intern file vc-file-prop-obarray) nil))

;;; install a call to the above as a find-file hook
(defun vc-find-file-hook ()
  ;; Recompute whether file is version controlled,
  ;; if user has killed the buffer and revisited.
  (cond 
   (buffer-file-name
    (vc-file-clearprops buffer-file-name)
    (cond
     ((vc-backend-deduce buffer-file-name)
      (vc-mode-line buffer-file-name)
      (cond ((not vc-make-backup-files)
	     ;; Use this variable, not make-backup-files,
	     ;; because this is for things that depend on the file name.
	     (make-local-variable 'backup-inhibited)
	     (setq backup-inhibited t))))))))

(add-hook 'find-file-hooks 'vc-find-file-hook)

;;; more hooks, this time for file-not-found
(defun vc-file-not-found-hook ()
  "When file is not found, try to check it out from RCS or SCCS.
Returns t if checkout was successful, nil otherwise."
  (if (vc-backend-deduce buffer-file-name)
      (save-excursion
	(require 'vc)
	(not (vc-error-occurred (vc-checkout buffer-file-name))))))

(add-hook 'find-file-not-found-hooks 'vc-file-not-found-hook)

;; Discard info about a file when we kill its buffer.
(defun vc-kill-buffer-hook ()
  (if (stringp (buffer-file-name))
      (progn
	(vc-file-clearprops (buffer-file-name))
	(kill-local-variable 'vc-buffer-backend))))

;;;(add-hook 'kill-buffer-hook 'vc-kill-buffer-hook)

;;; Now arrange for bindings and autoloading of the main package.
;;; Bindings for this have to go in the global map, as we'll often
;;; want to call them from random buffers.

(setq vc-prefix-map (lookup-key global-map "\C-xv"))
(if (not (keymapp vc-prefix-map))
    (progn
      (setq vc-prefix-map (make-sparse-keymap))
      (define-key global-map "\C-xv" vc-prefix-map)
      (define-key vc-prefix-map "a" 'vc-update-change-log)
      (define-key vc-prefix-map "c" 'vc-cancel-version)
      (define-key vc-prefix-map "d" 'vc-directory)
      (define-key vc-prefix-map "h" 'vc-insert-headers)
      (define-key vc-prefix-map "i" 'vc-register)
      (define-key vc-prefix-map "l" 'vc-print-log)
      (define-key vc-prefix-map "r" 'vc-retrieve-snapshot)
      (define-key vc-prefix-map "s" 'vc-create-snapshot)
      (define-key vc-prefix-map "u" 'vc-revert-buffer)
      (define-key vc-prefix-map "v" 'vc-next-action)
      (define-key vc-prefix-map "=" 'vc-diff)
      (define-key vc-prefix-map "~" 'vc-version-other-window)))

(if (not (boundp 'vc-menu-map))
    ;; Don't do the menu bindings if menu-bar.el wasn't loaded to defvar
    ;; vc-menu-map.
    ()
  ;;(define-key vc-menu-map [show-files]
  ;;  '("Show Files under VC" . (vc-directory t)))
  (define-key vc-menu-map [vc-directory] '("Show Locked Files" . vc-directory))
  (define-key vc-menu-map [separator1] '("----"))
  (define-key vc-menu-map [vc-rename-file] '("Rename File" . vc-rename-file))
  (define-key vc-menu-map [vc-version-other-window]
    '("Show Other Version" . vc-version-other-window))
  (define-key vc-menu-map [vc-diff] '("Compare with Last Version" . vc-diff))
  (define-key vc-menu-map [vc-update-change-log]
    '("Update ChangeLog" . vc-update-change-log))
  (define-key vc-menu-map [vc-print-log] '("Show History" . vc-print-log))
  (define-key vc-menu-map [separator2] '("----"))
  (define-key vc-menu-map [undo] '("Undo Last Check-In" . vc-cancel-version))
  (define-key vc-menu-map [vc-revert-buffer]
    '("Revert to Last Version" . vc-revert-buffer))
  (define-key vc-menu-map [vc-insert-header]
    '("Insert Header" . vc-insert-headers))
  (define-key vc-menu-map [vc-menu-check-in] '("Check In" . vc-next-action))
  (define-key vc-menu-map [vc-check-out] '("Check Out" . vc-toggle-read-only))
  (define-key vc-menu-map [vc-register] '("Register" . vc-register))
  (put 'vc-rename-file 'menu-enable 'vc-mode)
  (put 'vc-version-other-window 'menu-enable 'vc-mode)
  (put 'vc-diff 'menu-enable 'vc-mode)
  (put 'vc-update-change-log 'menu-enable
       '(eq (vc-buffer-backend) 'RCS))
  (put 'vc-print-log 'menu-enable 'vc-mode)
  (put 'vc-cancel-version 'menu-enable 'vc-mode)
  (put 'vc-revert-buffer 'menu-enable 'vc-mode)
  (put 'vc-insert-headers 'menu-enable 'vc-mode)
  (put 'vc-next-action 'menu-enable '(and vc-mode (not buffer-read-only)))
  (put 'vc-toggle-read-only 'menu-enable '(and vc-mode buffer-read-only))
  (put 'vc-register 'menu-enable '(not vc-mode))
  )

(provide 'vc-hooks)

;;; vc-hooks.el ends here
