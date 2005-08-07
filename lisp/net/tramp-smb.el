;;; tramp-smb.el --- Tramp access functions for SMB servers -*- coding: iso-8859-1; -*-

;; Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes

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

;; Access functions for SMB servers like SAMBA or M$ Windows from Tramp.

;;; Code:

(require 'tramp)

;; Pacify byte-compiler
(eval-when-compile
  (require 'cl)
  (require 'custom)
  ;; Emacs 19.34 compatibility hack -- is this needed?
  (or (>= emacs-major-version 20)
      (load "cl-seq")))

;; Avoid byte-compiler warnings if the byte-compiler supports this.
;; Currently, XEmacs supports this.
(eval-when-compile
  (when (fboundp 'byte-compiler-options)
    (let (unused-vars) ; Pacify Emacs byte-compiler
      (defalias 'warnings 'identity) ; Pacify Emacs byte-compiler
      (byte-compiler-options (warnings (- unused-vars))))))

;; Define SMB method ...
(defcustom tramp-smb-method "smb"
  "*Method to connect SAMBA and M$ SMB servers."
  :group 'tramp
  :type 'string)

;; ... and add it to the method list.
(add-to-list 'tramp-methods (cons tramp-smb-method nil))

;; Add a default for `tramp-default-method-alist'. Rule: If there is
;; a domain in USER, it must be the SMB method.
(add-to-list 'tramp-default-method-alist
	     (list "" "%" tramp-smb-method))

;; Add completion function for SMB method.
(tramp-set-completion-function
 tramp-smb-method
 '((tramp-parse-netrc "~/.netrc")))

(defcustom tramp-smb-program "smbclient"
  "*Name of SMB client to run."
  :group 'tramp
  :type 'string)

(defconst tramp-smb-prompt "^smb: .+> \\|^\\s-+Server\\s-+Comment$"
  "Regexp used as prompt in smbclient.")

(defconst tramp-smb-errors
  (mapconcat
   'identity
   '(; Connection error
     "Connection to \\S-+ failed"
     ; Samba
     "ERRDOS"
     "ERRSRV"
     "ERRbadfile"
     "ERRbadpw"
     "ERRfilexists"
     "ERRnoaccess"
     "ERRnomem"
     "ERRnosuchshare"
     ; Windows NT 4.0, Windows 5.0 (Windows 2000), Windows 5.1 (Windows XP)
     "NT_STATUS_ACCESS_DENIED"
     "NT_STATUS_ACCOUNT_LOCKED_OUT"
     "NT_STATUS_BAD_NETWORK_NAME"
     "NT_STATUS_CANNOT_DELETE"
     "NT_STATUS_LOGON_FAILURE"
     "NT_STATUS_NETWORK_ACCESS_DENIED"
     "NT_STATUS_NO_SUCH_FILE"
     "NT_STATUS_OBJECT_NAME_INVALID"
     "NT_STATUS_OBJECT_NAME_NOT_FOUND"
     "NT_STATUS_SHARING_VIOLATION"
     "NT_STATUS_WRONG_PASSWORD")
   "\\|")
  "Regexp for possible error strings of SMB servers.
Used instead of analyzing error codes of commands.")

(defvar tramp-smb-share nil
  "Holds the share name for the current buffer.
This variable is local to each buffer.")
(make-variable-buffer-local 'tramp-smb-share)

(defvar tramp-smb-share-cache nil
  "Caches the share names accessible to host related to the current buffer.
This variable is local to each buffer.")
(make-variable-buffer-local 'tramp-smb-share-cache)

(defvar tramp-smb-inodes nil
  "Keeps virtual inodes numbers for SMB files.")

;; New handlers should be added here.
(defconst tramp-smb-file-name-handler-alist
  '(
    ;; `access-file' performed by default handler
    (add-name-to-file . tramp-smb-handle-copy-file) ;; we're on Windows, honey.
    ;; `byte-compiler-base-file-name' performed by default handler
    (copy-file . tramp-smb-handle-copy-file)
    (delete-directory . tramp-smb-handle-delete-directory)
    (delete-file . tramp-smb-handle-delete-file)
    ;; `diff-latest-backup-file' performed by default handler
    (directory-file-name . tramp-handle-directory-file-name)
    (directory-files . tramp-smb-handle-directory-files)
    (directory-files-and-attributes . tramp-smb-handle-directory-files-and-attributes)
    (dired-call-process . tramp-smb-not-handled)
    (dired-compress-file . tramp-smb-not-handled)
    ;; `dired-uncache' performed by default handler
    ;; `expand-file-name' not necessary because we cannot expand "~/"
    (file-accessible-directory-p . tramp-smb-handle-file-directory-p)
    (file-attributes . tramp-smb-handle-file-attributes)
    (file-directory-p .  tramp-smb-handle-file-directory-p)
    (file-executable-p . tramp-smb-handle-file-exists-p)
    (file-exists-p . tramp-smb-handle-file-exists-p)
    (file-local-copy . tramp-smb-handle-file-local-copy)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions . tramp-smb-handle-file-name-all-completions)
    ;; `file-name-as-directory' performed by default handler
    (file-name-completion . tramp-handle-file-name-completion)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    ;; `file-name-sans-versions' performed by default handler
    (file-newer-than-file-p . tramp-smb-handle-file-newer-than-file-p)
    (file-ownership-preserved-p . tramp-smb-not-handled)
    (file-readable-p . tramp-smb-handle-file-exists-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-symlink-p . tramp-smb-not-handled)
    ;; `file-truename' performed by default handler
    (file-writable-p . tramp-smb-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    ;; `find-file-noselect' performed by default handler
    ;; `get-file-buffer' performed by default handler
    (insert-directory . tramp-smb-handle-insert-directory)
    (insert-file-contents . tramp-handle-insert-file-contents)
    (load . tramp-handle-load)
    (make-directory . tramp-smb-handle-make-directory)
    (make-directory-internal . tramp-smb-handle-make-directory-internal)
    (make-symbolic-link . tramp-smb-not-handled)
    (rename-file . tramp-smb-handle-rename-file)
    (set-file-modes . tramp-smb-not-handled)
    (set-visited-file-modtime . tramp-smb-not-handled)
    (shell-command . tramp-smb-not-handled)
    (substitute-in-file-name . tramp-smb-handle-substitute-in-file-name)
    (unhandled-file-name-directory . tramp-handle-unhandled-file-name-directory)
    (vc-registered . tramp-smb-not-handled)
    (verify-visited-file-modtime . tramp-smb-not-handled)
    (write-region . tramp-smb-handle-write-region)
)
  "Alist of handler functions for Tramp SMB method.
Operations not mentioned here will be handled by the default Emacs primitives.")

(defun tramp-smb-file-name-p (filename)
  "Check if it's a filename for SMB servers."
  (let ((v (tramp-dissect-file-name filename)))
    (string=
     (tramp-find-method
      (tramp-file-name-multi-method v)
      (tramp-file-name-method v)
      (tramp-file-name-user v)
      (tramp-file-name-host v))
     tramp-smb-method)))

(defun tramp-smb-file-name-handler (operation &rest args)
  "Invoke the SMB related OPERATION.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION."
  (let ((fn (assoc operation tramp-smb-file-name-handler-alist)))
    (if fn
	(if (eq (cdr fn) 'tramp-smb-not-handled)
	    (apply (cdr fn) operation args)
	  (save-match-data (apply (cdr fn) args)))
      (tramp-run-real-handler operation args))))

(add-to-list 'tramp-foreign-file-name-handler-alist
	     (cons 'tramp-smb-file-name-p 'tramp-smb-file-name-handler))


;; File name primitives

(defun tramp-smb-not-handled (operation &rest args)
  "Default handler for all functions which are disrecarded."
  (tramp-message 10 "Won't be handled: %s %s" operation args)
  nil)

(defun tramp-smb-handle-copy-file
  (filename newname &optional ok-if-already-exists keep-date)
  "Like `copy-file' for tramp files.
KEEP-DATE is not handled in case NEWNAME resides on an SMB server."
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))

  (let ((tmpfile (file-local-copy filename)))

    (if tmpfile
	;; remote filename
	(rename-file tmpfile newname ok-if-already-exists)

      ;; remote newname
      (when (file-directory-p newname)
	(setq newname (expand-file-name
		       (file-name-nondirectory filename) newname)))
      (when (and (not ok-if-already-exists)
		 (file-exists-p newname))
	(error "copy-file: file %s already exists" newname))

      (with-parsed-tramp-file-name newname nil
	(save-excursion
	  (let ((share (tramp-smb-get-share localname))
		(file (tramp-smb-get-localname localname t)))
	    (unless share
	      (error "Target `%s' must contain a share name" filename))
	    (tramp-smb-maybe-open-connection user host share)
	    (tramp-message-for-buffer
	     nil tramp-smb-method user host
	     5 "Copying file %s to file %s..." filename newname)
	    (if (tramp-smb-send-command
		 user host (format "put %s \"%s\"" filename file))
		(tramp-message-for-buffer
		 nil tramp-smb-method user host
		 5 "Copying file %s to file %s...done" filename newname)
	      (error "Cannot copy `%s'" filename))))))))

(defun tramp-smb-handle-delete-directory (directory)
  "Like `delete-directory' for tramp files."
  (setq directory (directory-file-name (expand-file-name directory)))
  (when (file-exists-p directory)
    (with-parsed-tramp-file-name directory nil
      (save-excursion
	(let ((share (tramp-smb-get-share localname))
	      (dir (tramp-smb-get-localname (file-name-directory localname) t))
	      (file (file-name-nondirectory localname)))
	  (tramp-smb-maybe-open-connection user host share)
	  (if (and
	       (tramp-smb-send-command user host (format "cd \"%s\"" dir))
	       (tramp-smb-send-command user host (format "rmdir \"%s\"" file)))
	      ;; Go Home
	      (tramp-smb-send-command user host (format "cd \\"))
	    ;; Error
	    (tramp-smb-send-command user host (format "cd \\"))
	    (error "Cannot delete directory `%s'" directory)))))))

(defun tramp-smb-handle-delete-file (filename)
  "Like `delete-file' for tramp files."
  (setq filename (expand-file-name filename))
  (when (file-exists-p filename)
    (with-parsed-tramp-file-name filename nil
      (save-excursion
	(let ((share (tramp-smb-get-share localname))
	      (dir (tramp-smb-get-localname (file-name-directory localname) t))
	      (file (file-name-nondirectory localname)))
	  (tramp-smb-maybe-open-connection user host share)
	  (if (and
	       (tramp-smb-send-command user host (format "cd \"%s\"" dir))
	       (tramp-smb-send-command user host (format "rm \"%s\"" file)))
	      ;; Go Home
	      (tramp-smb-send-command user host (format "cd \\"))
	    ;; Error
	    (tramp-smb-send-command user host (format "cd \\"))
	    (error "Cannot delete file `%s'" filename)))))))

(defun tramp-smb-handle-directory-files
  (directory &optional full match nosort)
  "Like `directory-files' for tramp files."
  (setq directory (directory-file-name (expand-file-name directory)))
  (with-parsed-tramp-file-name directory nil
    (save-excursion
      (let* ((share (tramp-smb-get-share localname))
	     (file (tramp-smb-get-localname localname nil))
	     (entries (tramp-smb-get-file-entries user host share file)))
	;; Just the file names are needed
	(setq entries (mapcar 'car entries))
	;; Discriminate with regexp
	(when match
	  (setq entries
		(delete nil
			(mapcar (lambda (x) (when (string-match match x) x))
				entries))))
	;; Make absolute localnames if necessary
	(when full
	  (setq entries
		(mapcar (lambda (x)
			  (concat (file-name-as-directory directory) x))
			entries)))
	;; Sort them if necessary
	(unless nosort (setq entries (sort entries 'string-lessp)))
	;; That's it
	entries))))

(defun tramp-smb-handle-directory-files-and-attributes
  (directory &optional full match nosort id-format)
  "Like `directory-files-and-attributes' for tramp files."
  (mapcar
   (lambda (x)
     ;; We cannot call `file-attributes' for backward compatibility reasons.
     ;; Its optional parameter ID-FORMAT is introduced with Emacs 22.
     (cons x (tramp-smb-handle-file-attributes
	(if full x (concat (file-name-as-directory directory) x)) id-format)))
   (directory-files directory full match nosort)))

(defun tramp-smb-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (save-excursion
      (let* ((share (tramp-smb-get-share localname))
	     (file (tramp-smb-get-localname localname nil))
	     (entries (tramp-smb-get-file-entries user host share file))
	     (entry (and entries
			 (assoc (file-name-nondirectory file) entries)))
	     (uid (if (and id-format (equal id-format 'string)) "nobody" -1))
	     (gid (if (and id-format (equal id-format 'string)) "nogroup" -1))
	     (inode (tramp-smb-get-inode share file))
	     (device (tramp-get-device nil tramp-smb-method user host)))

	; check result
	(when entry
	  (list (and (string-match "d" (nth 1 entry))
		     t)         ;0 file type
		-1		;1 link count
		uid		;2 uid
		gid		;3 gid
		'(0 0)		;4 atime
		(nth 3 entry)	;5 mtime
		'(0 0)		;6 ctime
		(nth 2 entry)   ;7 size
		(nth 1 entry)   ;8 mode
		nil		;9 gid weird
		inode		;10 inode number
		device))))))	;11 file system number

(defun tramp-smb-handle-file-directory-p (filename)
  "Like `file-directory-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (save-excursion
      (let* ((share (tramp-smb-get-share localname))
	     (file (tramp-smb-get-localname localname nil))
	     (entries (tramp-smb-get-file-entries user host share file))
	     (entry (and entries
			 (assoc (file-name-nondirectory file) entries))))
	(and entry
	     (string-match "d" (nth 1 entry))
	     t)))))

(defun tramp-smb-handle-file-exists-p (filename)
  "Like `file-exists-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (save-excursion
      (let* ((share (tramp-smb-get-share localname))
	     (file (tramp-smb-get-localname localname nil))
	     (entries (tramp-smb-get-file-entries user host share file)))
	(and entries
	     (member (file-name-nondirectory file) (mapcar 'car entries))
	     t)))))

(defun tramp-smb-handle-file-local-copy (filename)
  "Like `file-local-copy' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (save-excursion
      (let ((share (tramp-smb-get-share localname))
	    (file (tramp-smb-get-localname localname t))
	    (tmpfil (tramp-make-temp-file)))
	(unless (file-exists-p filename)
	  (error "Cannot make local copy of non-existing file `%s'" filename))
	(tramp-message-for-buffer
	 nil tramp-smb-method user host
	 5 "Fetching %s to tmp file %s..." filename tmpfil)
	(tramp-smb-maybe-open-connection user host share)
	(if (tramp-smb-send-command
	     user host (format "get \"%s\" %s" file tmpfil))
	    (tramp-message-for-buffer
	     nil tramp-smb-method user host
	     5 "Fetching %s to tmp file %s...done" filename tmpfil)
	  (error "Cannot make local copy of file `%s'" filename))
	tmpfil))))

;; This function should return "foo/" for directories and "bar" for
;; files.
(defun tramp-smb-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for tramp files."
  (with-parsed-tramp-file-name directory nil
    (save-match-data
      (save-excursion
	(let* ((share (tramp-smb-get-share localname))
	       (file (tramp-smb-get-localname localname nil))
	       (entries (tramp-smb-get-file-entries user host share file)))

	  (all-completions
	   filename
	   (mapcar
	    (lambda (x)
	      (list
	       (if (string-match "d" (nth 1 x))
		   (file-name-as-directory (nth 0 x))
		 (nth 0 x))))
	    entries)))))))

(defun tramp-smb-handle-file-newer-than-file-p (file1 file2)
  "Like `file-newer-than-file-p' for tramp files."
  (cond
   ((not (file-exists-p file1)) nil)
   ((not (file-exists-p file2)) t)
   (t (tramp-smb-time-less-p (file-attributes file2)
			     (file-attributes file1)))))

(defun tramp-smb-handle-file-writable-p (filename)
  "Like `file-writable-p' for tramp files."
  (if (not (file-exists-p filename))
      (let ((dir (file-name-directory filename)))
	(and (file-exists-p dir)
	     (file-writable-p dir)))
    (with-parsed-tramp-file-name filename nil
      (save-excursion
	(let* ((share (tramp-smb-get-share localname))
	       (file (tramp-smb-get-localname localname nil))
	       (entries (tramp-smb-get-file-entries user host share file))
	       (entry (and entries
			   (assoc (file-name-nondirectory file) entries))))
	  (and share entry
	       (string-match "w" (nth 1 entry))
	       t))))))

(defun tramp-smb-handle-insert-directory
  (filename switches &optional wildcard full-directory-p)
  "Like `insert-directory' for tramp files.
WILDCARD and FULL-DIRECTORY-P are not handled."
  (setq filename (expand-file-name filename))
  (when (file-directory-p filename)
    ;; This check is a little bit strange, but in `dired-add-entry'
    ;; this function is called with a non-directory ...
    (setq filename (file-name-as-directory filename)))
  (with-parsed-tramp-file-name filename nil
    (save-match-data
      (let* ((share (tramp-smb-get-share localname))
	     (file (tramp-smb-get-localname localname nil))
	     (entries (tramp-smb-get-file-entries user host share file)))

	;; Delete dummy "" entry, useless entries
	(setq entries
	      (if (file-directory-p filename)
		  (delq (assoc "" entries) entries)
		;; We just need the only and only entry FILENAME.
		(list (assoc (file-name-nondirectory filename) entries))))

	;; Sort entries
	(setq entries
	      (sort
	       entries
	       (lambda (x y)
		 (if (string-match "t" switches)
		     ; sort by date
		     (tramp-smb-time-less-p (nth 3 y) (nth 3 x))
		   ; sort by name
		   (string-lessp (nth 0 x) (nth 0 y))))))

	;; Print entries
	(mapcar
	 (lambda (x)
	   (insert
	    (format
	     "%10s %3d %-8s %-8s %8s %s %s\n"
	     (nth 1 x) ; mode
	     1 "nobody" "nogroup"
	     (nth 2 x) ; size
	     (format-time-string
	      (if (tramp-smb-time-less-p
		   (tramp-smb-time-subtract (current-time) (nth 3 x))
		   tramp-smb-half-a-year)
		  "%b %e %R"
		"%b %e  %Y")
	      (nth 3 x)) ; date
	     (nth 0 x))) ; file name
	   (forward-line)
	   (beginning-of-line))
	 entries)))))

(defun tramp-smb-handle-make-directory (dir &optional parents)
  "Like `make-directory' for tramp files."
  (setq dir (directory-file-name (expand-file-name dir)))
  (unless (file-name-absolute-p dir)
    (setq dir (concat default-directory dir)))
  (with-parsed-tramp-file-name dir nil
    (save-match-data
      (let* ((share (tramp-smb-get-share localname))
	     (ldir (file-name-directory dir)))
	;; Make missing directory parts
	(when (and parents share (not (file-directory-p ldir)))
	  (make-directory ldir parents))
	;; Just do it
	(when (file-directory-p ldir)
	  (make-directory-internal dir))
	(unless (file-directory-p dir)
	  (error "Couldn't make directory %s" dir))))))

(defun tramp-smb-handle-make-directory-internal (directory)
  "Like `make-directory-internal' for tramp files."
  (setq directory (directory-file-name (expand-file-name directory)))
  (unless (file-name-absolute-p directory)
    (setq directory (concat default-directory directory)))
  (with-parsed-tramp-file-name directory nil
    (save-match-data
      (let* ((share (tramp-smb-get-share localname))
	     (file (tramp-smb-get-localname localname nil)))
	(when (file-directory-p (file-name-directory directory))
	  (tramp-smb-maybe-open-connection user host share)
	  (tramp-smb-send-command user host (format "mkdir \"%s\"" file)))
	(unless (file-directory-p directory)
	  (error "Couldn't make directory %s" directory))))))

(defun tramp-smb-handle-rename-file
  (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for tramp files."
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))

  (let ((tmpfile (file-local-copy filename)))

    (if tmpfile
	;; remote filename
	(rename-file tmpfile newname ok-if-already-exists)

      ;; remote newname
      (when (file-directory-p newname)
	(setq newname (expand-file-name
		      (file-name-nondirectory filename) newname)))
      (when (and (not ok-if-already-exists)
		 (file-exists-p newname))
	  (error "rename-file: file %s already exists" newname))

      (with-parsed-tramp-file-name newname nil
	(save-excursion
	  (let ((share (tramp-smb-get-share localname))
		(file (tramp-smb-get-localname localname t)))
	    (tramp-smb-maybe-open-connection user host share)
	    (tramp-message-for-buffer
	     nil tramp-smb-method user host
	     5 "Copying file %s to file %s..." filename newname)
	    (if (tramp-smb-send-command
		 user host (format "put %s \"%s\"" filename file))
		(tramp-message-for-buffer
		 nil tramp-smb-method user host
		 5 "Copying file %s to file %s...done" filename newname)
	      (error "Cannot rename `%s'" filename)))))))

  (delete-file filename))

(defun tramp-smb-handle-substitute-in-file-name (filename)
  "Like `handle-substitute-in-file-name' for tramp files.
Catches errors for shares like \"C$/\", which are common in Microsoft Windows."
  (condition-case nil
      (tramp-run-real-handler 'substitute-in-file-name (list filename))
    (error filename)))

(defun tramp-smb-handle-write-region
  (start end filename &optional append visit lockname confirm)
  "Like `write-region' for tramp files."
  (unless (eq append nil)
    (error "Cannot append to file using tramp (`%s')" filename))
  (setq filename (expand-file-name filename))
  ;; XEmacs takes a coding system as the seventh argument, not `confirm'
  (when (and (not (featurep 'xemacs))
	     confirm (file-exists-p filename))
    (unless (y-or-n-p (format "File %s exists; overwrite anyway? "
                              filename))
      (error "File not overwritten")))
  (with-parsed-tramp-file-name filename nil
    (save-excursion
      (let ((share (tramp-smb-get-share localname))
	    (file (tramp-smb-get-localname localname t))
	    (curbuf (current-buffer))
	    tmpfil)
	;; Write region into a tmp file.
	(setq tmpfil (tramp-make-temp-file))
	;; We say `no-message' here because we don't want the visited file
	;; modtime data to be clobbered from the temp file.  We call
	;; `set-visited-file-modtime' ourselves later on.
	(tramp-run-real-handler
	 'write-region
	 (if confirm ; don't pass this arg unless defined for backward compat.
	     (list start end tmpfil append 'no-message lockname confirm)
	   (list start end tmpfil append 'no-message lockname)))

	(tramp-smb-maybe-open-connection user host share)
	(tramp-message-for-buffer
	 nil tramp-smb-method user host
	 5 "Writing tmp file %s to file %s..." tmpfil filename)
	(if (tramp-smb-send-command
	     user host (format "put %s \"%s\"" tmpfil file))
	    (tramp-message-for-buffer
	     nil tramp-smb-method user host
	     5 "Writing tmp file %s to file %s...done" tmpfil filename)
	  (error "Cannot write `%s'" filename))

	(delete-file tmpfil)
	(unless (equal curbuf (current-buffer))
	  (error "Buffer has changed from `%s' to `%s'"
		 curbuf (current-buffer)))
	(when (eq visit t)
	  (set-visited-file-modtime))))))


;; Internal file name functions

(defun tramp-smb-get-share (localname)
  "Returns the share name of LOCALNAME."
  (save-match-data
    (when (string-match "^/?\\([^/]+\\)/" localname)
      (match-string 1 localname))))

(defun tramp-smb-get-localname (localname convert)
  "Returns the file name of LOCALNAME.
If CONVERT is non-nil exchange \"/\" by \"\\\\\"."
  (save-match-data
    (let ((res localname))

      (setq
       res (if (string-match "^/?[^/]+/\\(.*\\)" res)
	       (if convert
		   (mapconcat
		    (lambda (x) (if (equal x ?/) "\\" (char-to-string x)))
		    (match-string 1 res) "")
		 (match-string 1 res))
	     (if (string-match "^/?\\([^/]+\\)$" res)
		 (match-string 1 res)
	       "")))

      ;; Sometimes we have discarded `substitute-in-file-name'
      (when (string-match "\\(\\$\\$\\)\\(/\\|$\\)" res)
	(setq res (replace-match "$" nil nil res 1)))

      res)))

;; Share names of a host are cached. It is very unlikely that the
;; shares do change during connection.
(defun tramp-smb-get-file-entries (user host share localname)
  "Read entries which match LOCALNAME.
Either the shares are listed, or the `dir' command is executed.
Only entries matching the localname are returned.
Result is a list of (LOCALNAME MODE SIZE MONTH DAY TIME YEAR)."
  (save-excursion
    (save-match-data
      (let ((base (or (and (> (length localname) 0)
			   (string-match "\\([^/]+\\)$" localname)
			   (regexp-quote (match-string 1 localname)))
		      ""))
	    res entry)
	(set-buffer (tramp-get-buffer nil tramp-smb-method user host))
	(if (and (not share) tramp-smb-share-cache)
	    ;; Return cached shares
	    (setq res tramp-smb-share-cache)
	  ;; Read entries
	  (tramp-smb-maybe-open-connection user host share)
	  (when share
	    (tramp-smb-send-command
	     user host
	     (format "dir %s"
		     (if (zerop (length localname)) "" (concat "\"" localname "*\"")))))
	  (goto-char (point-min))
	  ;; Loop the listing
	  (unless (re-search-forward tramp-smb-errors nil t)
	    (while (not (eobp))
	      (setq entry (tramp-smb-read-file-entry share))
	      (forward-line)
	      (when entry (add-to-list 'res entry))))
	  (unless share
	    ;; Cache share entries
	    (setq tramp-smb-share-cache res)))

	;; Add directory itself
	(add-to-list 'res '("" "drwxrwxrwx" 0 (0 0)))

	;; There's a very strange error (debugged with XEmacs 21.4.14)
	;; If there's no short delay, it returns nil.  No idea about
	(when (featurep 'xemacs) (sleep-for 0.01))

	;; Check for matching entries
	(delq nil (mapcar
		   (lambda (x) (and (string-match base (nth 0 x)) x))
		   res))))))

;; Return either a share name (if SHARE is nil), or a file name
;;
;; If shares are listed, the following format is expected
;;
;; \s-\{8,8}                              - leading spaces
;; \S-\(.*\S-\)\s-*                       - share name, 14 char
;; \s-                                    - space delimeter
;; \S-+\s-*                               - type, 8 char, "Disk    " expected
;; \(\s-\{2,2\}.*\)?                      - space delimeter, comment
;;
;; Entries provided by smbclient DIR aren't fully regular.
;; They should have the format
;;
;; \s-\{2,2}                              - leading spaces
;; \S-\(.*\S-\)\s-*                       - file name, 30 chars, left bound
;; \s-+[ADHRSV]*                          - permissions, 7 chars, right bound
;; \s-                                    - space delimeter
;; \s-+[0-9]+                             - size, 8 chars, right bound
;; \s-\{2,2\}                             - space delimeter
;; \w\{3,3\}                              - weekday
;; \s-                                    - space delimeter
;; \w\{3,3\}                              - month
;; \s-                                    - space delimeter
;; [ 19][0-9]                             - day
;; \s-                                    - space delimeter
;; [0-9]\{2,2\}:[0-9]\{2,2\}:[0-9]\{2,2\} - time
;; \s-                                    - space delimeter
;; [0-9]\{4,4\}                           - year
;;
;; samba/src/client.c (http://samba.org/doxygen/samba/client_8c-source.html)
;; has function display_finfo:
;;
;;   d_printf("  %-30s%7.7s %8.0f  %s",
;;            finfo->name,
;;            attrib_string(finfo->mode),
;;            (double)finfo->size,
;;            asctime(LocalTime(&t)));
;;
;; in Samba 1.9, there's the following code:
;;
;;   DEBUG(0,("  %-30s%7.7s%10d  %s",
;;  	   CNV_LANG(finfo->name),
;;	   attrib_string(finfo->mode),
;;	   finfo->size,
;;	   asctime(LocalTime(&t))));
;;
;; Problems:
;; * Modern regexp constructs, like spy groups and counted repetitions, aren't
;;   available in older Emacsen.
;; * The length of constructs (file name, size) might exceed the default.
;; * File names might contain spaces.
;; * Permissions might be empty.
;;
;; So we try to analyze backwards.
(defun tramp-smb-read-file-entry (share)
  "Parse entry in SMB output buffer.
If SHARE is result, entries are of type dir. Otherwise, shares are listed.
Result is the list (LOCALNAME MODE SIZE MTIME)."
  (let ((line (buffer-substring (point) (tramp-point-at-eol)))
	localname mode size month day hour min sec year mtime)

    (if (not share)

	; Read share entries
	(when (string-match "^\\s-+\\(\\S-+\\)\\s-+Disk" line)
	  (setq localname (match-string 1 line)
		mode "dr-xr-xr-x"
		size 0))

      ; Real listing
      (block nil

	;; year
	(if (string-match "\\([0-9]+\\)$" line)
	    (setq year (string-to-number (match-string 1 line))
		  line (substring line 0 -5))
	  (return))

	;; time
	(if (string-match "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)$" line)
	    (setq hour (string-to-number (match-string 1 line))
		  min  (string-to-number (match-string 2 line))
		  sec  (string-to-number (match-string 3 line))
		  line (substring line 0 -9))
	  (return))

	;; day
	(if (string-match "\\([0-9]+\\)$" line)
	    (setq day  (string-to-number (match-string 1 line))
		  line (substring line 0 -3))
	  (return))

	;; month
	(if (string-match "\\(\\w+\\)$" line)
	    (setq month (match-string 1 line)
		  line  (substring line 0 -4))
	  (return))

	;; weekday
	(if (string-match "\\(\\w+\\)$" line)
	    (setq line (substring line 0 -5))
	  (return))

	;; size
	(if (string-match "\\([0-9]+\\)$" line)
	    (let ((length (- (max 10 (1+ (length (match-string 1 line)))))))
	      (setq size (string-to-number (match-string 1 line)))
	      (when (string-match "\\([ADHRSV]+\\)" (substring line length))
		(setq length (+ length (match-end 0))))
	      (setq line (substring line 0 length)))
	  (return))

	;; mode: ARCH, DIR, HIDDEN, RONLY, SYSTEM, VOLID
	(if (string-match "\\([ADHRSV]+\\)?$" line)
	    (setq
	     mode (or (match-string 1 line) "")
	     mode (save-match-data (format
		    "%s%s"
		    (if (string-match "D" mode) "d" "-")
		    (mapconcat
		     (lambda (x) "") "    "
		     (concat "r" (if (string-match "R" mode) "-" "w") "x"))))
	     line (substring line 0 -7))
	  (return))

	;; localname
	(if (string-match "^\\s-+\\(\\S-\\(.*\\S-\\)?\\)\\s-*$" line)
	    (setq localname (match-string 1 line))
	  (return))))

    (when (and localname mode size)
      (setq mtime
	    (if (and sec min hour day month year)
		(encode-time
		 sec min hour day
		 (cdr (assoc (downcase month) tramp-smb-parse-time-months))
		 year)
	      '(0 0)))
      (list localname mode size mtime))))

;; Inodes don't exist for SMB files.  Therefore we must generate virtual ones.
;; Used in `find-buffer-visiting'.
;; The method applied might be not so efficient (Ange-FTP uses hashes). But
;; performance isn't the major issue given that file transfer will take time.

(defun tramp-smb-get-inode (share file)
  "Returns the virtual inode number.
If it doesn't exist, generate a new one."
  (let ((string (concat share "/" (directory-file-name file))))
    (unless (assoc string tramp-smb-inodes)
      (add-to-list 'tramp-smb-inodes
		   (list string (length tramp-smb-inodes))))
    (nth 1 (assoc string tramp-smb-inodes))))


;; Connection functions

(defun tramp-smb-send-command (user host command)
  "Send the COMMAND to USER at HOST (logged into an SMB session).
Erases temporary buffer before sending the command.  Returns nil if
there has been an error message from smbclient."
  (save-excursion
    (set-buffer (tramp-get-buffer nil tramp-smb-method user host))
    (erase-buffer)
    (tramp-send-command nil tramp-smb-method user host command nil t)
    (tramp-smb-wait-for-output user host)))

(defun tramp-smb-maybe-open-connection (user host share)
  "Maybe open a connection to HOST, logging in as USER, using `tramp-smb-program'.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
  (let ((process-connection-type tramp-process-connection-type)
	(p (get-buffer-process
	    (tramp-get-buffer nil tramp-smb-method user host))))
    (save-excursion
      (set-buffer (tramp-get-buffer nil tramp-smb-method user host))
      ;; Check whether it is still the same share
      (unless (and p (processp p) (string-equal tramp-smb-share share))
	(when (and p (processp p))
	  (delete-process p)
	  (setq p nil)))
      ;; If too much time has passed since last command was sent, look
      ;; whether process is still alive.  If it isn't, kill it.
      (when (and tramp-last-cmd-time
		 (> (tramp-time-diff (current-time) tramp-last-cmd-time) 60)
		 p (processp p) (memq (process-status p) '(run open)))
	(unless (and p (processp p) (memq (process-status p) '(run open)))
	  (delete-process p)
	  (setq p nil))))
    (unless (and p (processp p) (memq (process-status p) '(run open)))
      (when (and p (processp p))
        (delete-process p))
      (tramp-smb-open-connection user host share))))

(defun tramp-smb-open-connection (user host share)
  "Open a connection using `tramp-smb-program'.
This starts the command `smbclient //HOST/SHARE -U USER', then waits
for a remote password prompt.  It queries the user for the password,
then sends the password to the remote host.

Domain names in USER and port numbers in HOST are acknowledged."

  (when (and (fboundp 'executable-find)
	     (not (funcall 'executable-find tramp-smb-program)))
    (error "Cannot find command %s in %s" tramp-smb-program exec-path))

  (save-match-data
    (let* ((buffer (tramp-get-buffer nil tramp-smb-method user host))
	   (real-user user)
	   (real-host host)
	   domain port args)

      ; Check for domain ("user%domain") and port ("host#port")
      (when (and user (string-match "\\(.+\\)%\\(.+\\)" user))
	(setq real-user (or (match-string 1 user) user)
	      domain (match-string 2 user)))

      (when (and host (string-match "\\(.+\\)#\\(.+\\)" host))
	(setq real-host (or (match-string 1 host) host)
	      port (match-string 2 host)))

      (if share
	  (setq args (list (concat "//" real-host "/" share)))
	(setq args (list "-L" real-host )))

      (if real-user
	  (setq args (append args (list "-U" real-user)))
	(setq args (append args (list "-N"))))

      (when domain (setq args (append args (list "-W" domain))))
      (when port   (setq args (append args (list "-p" port))))

      ; OK, let's go
      (tramp-pre-connection nil tramp-smb-method user host tramp-chunksize)
      (tramp-message 7 "Opening connection for //%s@%s/%s..."
		     user host (or share ""))

      (let* ((default-directory (tramp-temporary-file-directory))
	     ;; If we omit the conditional here, then we would use
	     ;; `undecided-dos' in some cases.  With the conditional,
	     ;; we use nil in these cases.  Which one is right?
	     (coding-system-for-read (unless (and (not (featurep 'xemacs))
						  (> emacs-major-version 20))
				       tramp-dos-coding-system))
	     (p (apply #'start-process (buffer-name buffer) buffer
		       tramp-smb-program args)))

	(tramp-message 9 "Started process %s" (process-command p))
	(tramp-set-process-query-on-exit-flag p nil)
	(set-buffer buffer)
	(setq tramp-smb-share share)

        ; send password
	(when real-user
	  (let ((pw-prompt "Password:"))
	    (tramp-message 9 "Sending password")
	    (tramp-enter-password p pw-prompt user host)))

	(unless (tramp-smb-wait-for-output user host)
	  (tramp-clear-passwd user host)
	  (error "Cannot open connection //%s@%s/%s"
		 user host (or share "")))))))

;; We don't use timeouts.  If needed, the caller shall wrap around.
(defun tramp-smb-wait-for-output (user host)
  "Wait for output from smbclient command.
Returns nil if an error message has appeared."
  (let ((proc (get-buffer-process (current-buffer)))
	(found (progn (goto-char (point-min))
		      (re-search-forward tramp-smb-prompt nil t)))
	(err   (progn (goto-char (point-min))
		      (re-search-forward tramp-smb-errors nil t))))

    ;; Algorithm: get waiting output.  See if last line contains
    ;; tramp-smb-prompt sentinel or tramp-smb-errors strings.
    ;; If not, wait a bit and again get waiting output.
    (while (and (not found) (not err))

      ;; Accept pending output.
      (tramp-accept-process-output proc)

      ;; Search for prompt.
      (goto-char (point-min))
      (setq found (re-search-forward tramp-smb-prompt nil t))

      ;; Search for errors.
      (goto-char (point-min))
      (setq err (re-search-forward tramp-smb-errors nil t)))

    ;; Add output to debug buffer if appropriate.
    (when tramp-debug-buffer
      (append-to-buffer
       (tramp-get-debug-buffer nil tramp-smb-method user host)
       (point-min) (point-max)))

    ;; Return value is whether no error message has appeared.
    (not err)))


;; Snarfed code from time-date.el and parse-time.el

(defconst tramp-smb-half-a-year '(241 17024)
"Evaluated by \"(days-to-time 183)\".")

(defconst tramp-smb-parse-time-months '(("jan" . 1) ("feb" . 2) ("mar" . 3)
			    ("apr" . 4) ("may" . 5) ("jun" . 6)
			    ("jul" . 7) ("aug" . 8) ("sep" . 9)
			    ("oct" . 10) ("nov" . 11) ("dec" . 12))
"Alist mapping month names to integers.")

(defun tramp-smb-time-less-p (t1 t2)
  "Say whether time value T1 is less than time value T2."
  (unless t1 (setq t1 '(0 0)))
  (unless t2 (setq t2 '(0 0)))
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

(defun tramp-smb-time-subtract (t1 t2)
  "Subtract two time values.
Return the difference in the format of a time value."
  (unless t1 (setq t1 '(0 0)))
  (unless t2 (setq t2 '(0 0)))
  (let ((borrow (< (cadr t1) (cadr t2))))
    (list (- (car t1) (car t2) (if borrow 1 0))
	  (- (+ (if borrow 65536 0) (cadr t1)) (cadr t2)))))


(provide 'tramp-smb)

;;; TODO:

;; * Provide a local smb.conf. The default one might not be readable.
;; * Error handling in case password is wrong.
;; * Read password from "~/.netrc".
;; * Return more comprehensive file permission string.  Think whether it is
;;   possible to implement `set-file-modes'.
;; * Handle WILDCARD and FULL-DIRECTORY-P in
;;   `tramp-smb-handle-insert-directory'.
;; * Handle links (FILENAME.LNK).
;; * Maybe local tmp files should have the same extension like the original
;;   files.  Strange behaviour with jka-compr otherwise?
;; * Copy files in dired from SMB to another method doesn't work.
;; * Try to remove the inclusion of dummy "" directory.  Seems to be at
;;   several places, especially in `tramp-smb-handle-insert-directory'.
;; * Provide variables for debug.
;; * (RMS) Use unwind-protect to clean up the state so as to make the state
;;   regular again.

;;; arch-tag: fcc9dbec-7503-4d73-b638-3c8aa59575f5
;;; tramp-smb.el ends here
