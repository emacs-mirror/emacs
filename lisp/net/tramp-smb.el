;;; tramp-smb.el --- Tramp access functions for SMB servers

;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007,
;;   2008 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Access functions for SMB servers like SAMBA or M$ Windows from Tramp.

;;; Code:

(eval-when-compile (require 'cl))	; block, return
(require 'tramp)
(require 'tramp-cache)
(require 'tramp-compat)

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
	     `(nil "%" ,tramp-smb-method))

;; Add a default for `tramp-default-user-alist'. Rule: For the SMB method,
;; the anonymous user is chosen.
(add-to-list 'tramp-default-user-alist
	     `(,tramp-smb-method nil ""))

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
  ;; `regexp-opt' not possible because of first string.
  (mapconcat
   'identity
   '(;; Connection error / timeout
     "Connection to \\S-+ failed"
     "Read from server failed, maybe it closed the connection"
     "Call timed out: server did not respond"
     ;; Samba
     "ERRDOS"
     "ERRSRV"
     "ERRbadfile"
     "ERRbadpw"
     "ERRfilexists"
     "ERRnoaccess"
     "ERRnomem"
     "ERRnosuchshare"
     ;; Windows 4.0 (Windows NT), Windows 5.0 (Windows 2000),
     ;; Windows 5.1 (Windows XP), Windows 5.2 (Windows Server 2003)
     "NT_STATUS_ACCESS_DENIED"
     "NT_STATUS_ACCOUNT_LOCKED_OUT"
     "NT_STATUS_BAD_NETWORK_NAME"
     "NT_STATUS_CANNOT_DELETE"
     "NT_STATUS_DIRECTORY_NOT_EMPTY"
     "NT_STATUS_DUPLICATE_NAME"
     "NT_STATUS_FILE_IS_A_DIRECTORY"
     "NT_STATUS_LOGON_FAILURE"
     "NT_STATUS_NETWORK_ACCESS_DENIED"
     "NT_STATUS_NO_SUCH_FILE"
     "NT_STATUS_OBJECT_NAME_COLLISION"
     "NT_STATUS_OBJECT_NAME_INVALID"
     "NT_STATUS_OBJECT_NAME_NOT_FOUND"
     "NT_STATUS_SHARING_VIOLATION"
     "NT_STATUS_TRUSTED_RELATIONSHIP_FAILURE"
     "NT_STATUS_WRONG_PASSWORD")
   "\\|")
  "Regexp for possible error strings of SMB servers.
Used instead of analyzing error codes of commands.")

(defconst tramp-smb-actions-with-share
  '((tramp-smb-prompt tramp-action-succeed)
    (tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (tramp-smb-errors tramp-action-permission-denied)
    (tramp-process-alive-regexp tramp-action-process-alive))
  "List of pattern/action pairs.
This list is used for login to SMB servers.

See `tramp-actions-before-shell' for more info.")

(defconst tramp-smb-actions-without-share
  '((tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (tramp-smb-errors tramp-action-permission-denied)
    (tramp-process-alive-regexp tramp-action-out-of-band))
  "List of pattern/action pairs.
This list is used for login to SMB servers.

See `tramp-actions-before-shell' for more info.")

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
    (dired-call-process . ignore)
    (dired-compress-file . ignore)
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
    (file-ownership-preserved-p . ignore)
    (file-readable-p . tramp-smb-handle-file-exists-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-symlink-p . tramp-handle-file-symlink-p)
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
    (make-symbolic-link . ignore)
    (rename-file . tramp-smb-handle-rename-file)
    (set-file-modes . ignore)
    (set-visited-file-modtime . ignore)
    (shell-command . ignore)
    (substitute-in-file-name . tramp-smb-handle-substitute-in-file-name)
    (unhandled-file-name-directory . tramp-handle-unhandled-file-name-directory)
    (vc-registered . ignore)
    (verify-visited-file-modtime . ignore)
    (write-region . tramp-smb-handle-write-region)
)
  "Alist of handler functions for Tramp SMB method.
Operations not mentioned here will be handled by the default Emacs primitives.")

(defun tramp-smb-file-name-p (filename)
  "Check if it's a filename for SMB servers."
  (let ((v (tramp-dissect-file-name filename)))
    (string= (tramp-file-name-method v) tramp-smb-method)))

(defun tramp-smb-file-name-handler (operation &rest args)
  "Invoke the SMB related OPERATION.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION."
  (let ((fn (assoc operation tramp-smb-file-name-handler-alist)))
    (if fn
	(save-match-data (apply (cdr fn) args))
      (tramp-run-real-handler operation args))))

(add-to-list 'tramp-foreign-file-name-handler-alist
	     (cons 'tramp-smb-file-name-p 'tramp-smb-file-name-handler))


;; File name primitives

(defun tramp-smb-handle-copy-file
  (filename newname &optional ok-if-already-exists keep-date preserve-uid-gid)
  "Like `copy-file' for Tramp files.
KEEP-DATE is not handled in case NEWNAME resides on an SMB server.
PRESERVE-UID-GID is completely ignored."
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))

  (let ((tmpfile (file-local-copy filename)))

    (if tmpfile
	;; Remote filename.
	(rename-file tmpfile newname ok-if-already-exists)

      ;; Remote newname.
      (when (file-directory-p newname)
	(setq newname (expand-file-name
		       (file-name-nondirectory filename) newname)))

      (with-parsed-tramp-file-name newname nil
	(when (and (not ok-if-already-exists)
		   (file-exists-p newname))
	  (tramp-error v 'file-already-exists newname))

	;; We must also flush the cache of the directory, because
	;; file-attributes reads the values from there.
	(tramp-flush-file-property v (file-name-directory localname))
	(tramp-flush-file-property v localname)
	(let ((share (tramp-smb-get-share localname))
	      (file (tramp-smb-get-localname localname t)))
	  (unless share
	    (tramp-error
	     v 'file-error "Target `%s' must contain a share name" newname))
	  (tramp-message v 0 "Copying file %s to file %s..." filename newname)
	  (if (tramp-smb-send-command
	       v (format "put %s \"%s\"" filename file))
	      (tramp-message
	       v 0 "Copying file %s to file %s...done" filename newname)
	    (tramp-error v 'file-error "Cannot copy `%s'" filename)))))))

(defun tramp-smb-handle-delete-directory (directory)
  "Like `delete-directory' for Tramp files."
  (setq directory (directory-file-name (expand-file-name directory)))
  (when (file-exists-p directory)
    (with-parsed-tramp-file-name directory nil
      ;; We must also flush the cache of the directory, because
      ;; file-attributes reads the values from there.
      (tramp-flush-file-property v (file-name-directory localname))
      (tramp-flush-directory-property v localname)
      (let ((dir (tramp-smb-get-localname (file-name-directory localname) t))
	    (file (file-name-nondirectory localname)))
	(unwind-protect
	    (unless (and
		     (tramp-smb-send-command v (format "cd \"%s\"" dir))
		     (tramp-smb-send-command v (format "rmdir \"%s\"" file)))
	      ;; Error
	      (with-current-buffer (tramp-get-connection-buffer v)
		(goto-char (point-min))
		(search-forward-regexp tramp-smb-errors nil t)
		(tramp-error
		 v 'file-error "%s `%s'" (match-string 0) directory)))
	  ;; Always go home
	  (tramp-smb-send-command v (format "cd \\")))))))

(defun tramp-smb-handle-delete-file (filename)
  "Like `delete-file' for Tramp files."
  (setq filename (expand-file-name filename))
  (when (file-exists-p filename)
    (with-parsed-tramp-file-name filename nil
      ;; We must also flush the cache of the directory, because
      ;; file-attributes reads the values from there.
      (tramp-flush-file-property v (file-name-directory localname))
      (tramp-flush-file-property v localname)
      (let ((dir (tramp-smb-get-localname (file-name-directory localname) t))
	    (file (file-name-nondirectory localname)))
	(unwind-protect
	    (unless (and
		     (tramp-smb-send-command v (format "cd \"%s\"" dir))
		     (tramp-smb-send-command v (format "rm \"%s\"" file)))
	      ;; Error
	      (with-current-buffer (tramp-get-connection-buffer v)
		(goto-char (point-min))
		(search-forward-regexp tramp-smb-errors nil t)
		(tramp-error
		 v 'file-error "%s `%s'" (match-string 0) filename)))
	  ;; Always go home
	  (tramp-smb-send-command v (format "cd \\")))))))

(defun tramp-smb-handle-directory-files
  (directory &optional full match nosort)
  "Like `directory-files' for Tramp files."
  (let ((result (mapcar 'directory-file-name
			(file-name-all-completions "" directory))))
    ;; Discriminate with regexp
    (when match
      (setq result
	    (delete nil
		    (mapcar (lambda (x) (when (string-match match x) x))
			    result))))
    ;; Append directory
    (when full
      (setq result
	    (mapcar
	     (lambda (x) (expand-file-name x directory))
	     result)))
    ;; Sort them if necessary
    (unless nosort (setq result (sort result 'string-lessp)))
    ;; That's it
    result))

(defun tramp-smb-handle-directory-files-and-attributes
  (directory &optional full match nosort id-format)
  "Like `directory-files-and-attributes' for Tramp files."
  (mapcar
   (lambda (x)
     ;; We cannot call `file-attributes' for backward compatibility reasons.
     ;; Its optional parameter ID-FORMAT is introduced with Emacs 22.
     (cons x (tramp-smb-handle-file-attributes
	(if full x (expand-file-name x directory)) id-format)))
   (directory-files directory full match nosort)))

(defun tramp-smb-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for Tramp files."
  ;; Reading just the filename entry via "dir localname" is not
  ;; possible, because when filename is a directory, some smbclient
  ;; versions return the content of the directory, and other versions
  ;; don't.  Therefore, the whole content of the upper directory is
  ;; retrieved, and the entry of the filename is extracted from.
  (with-parsed-tramp-file-name filename nil
    (with-file-property v localname (format "file-attributes-%s" id-format)
      (let* ((entries (tramp-smb-get-file-entries
		       (file-name-directory filename)))
	     (entry (and entries
			 (assoc (file-name-nondirectory filename) entries)))
	     (uid (if (and id-format (equal id-format 'string)) "nobody" -1))
	     (gid (if (and id-format (equal id-format 'string)) "nogroup" -1))
	     (inode (tramp-get-inode v))
	     (device (tramp-get-device v)))

        ;; Check result.
	(when entry
	  (list (and (string-match "d" (nth 1 entry))
		     t)       ;0 file type
		-1	      ;1 link count
		uid	      ;2 uid
		gid	      ;3 gid
		'(0 0)	      ;4 atime
		(nth 3 entry) ;5 mtime
		'(0 0)	      ;6 ctime
		(nth 2 entry) ;7 size
		(nth 1 entry) ;8 mode
		nil	      ;9 gid weird
		inode	      ;10 inode number
		device))))))  ;11 file system number

(defun tramp-smb-handle-file-directory-p (filename)
  "Like `file-directory-p' for Tramp files."
  (and (file-exists-p filename)
       (eq ?d (aref (nth 8 (file-attributes filename)) 0))))

(defun tramp-smb-handle-file-exists-p (filename)
  "Like `file-exists-p' for Tramp files."
  (not (null (file-attributes filename))))

(defun tramp-smb-handle-file-local-copy (filename)
  "Like `file-local-copy' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (let ((file (tramp-smb-get-localname localname t))
	  (tmpfile (tramp-compat-make-temp-file filename)))
      (unless (file-exists-p filename)
	(tramp-error
	 v 'file-error
	 "Cannot make local copy of non-existing file `%s'" filename))
      (tramp-message v 4 "Fetching %s to tmp file %s..." filename tmpfile)
      (if (tramp-smb-send-command v (format "get \"%s\" %s" file tmpfile))
	  (tramp-message
	   v 4 "Fetching %s to tmp file %s...done" filename tmpfile)
	(tramp-error
	 v 'file-error
	 "Cannot make local copy of file `%s'" filename))
      tmpfile)))

;; This function should return "foo/" for directories and "bar" for
;; files.
(defun tramp-smb-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for Tramp files."
  (all-completions
   filename
   (with-parsed-tramp-file-name directory nil
     (with-file-property v localname "file-name-all-completions"
       (save-match-data
	 (let ((entries (tramp-smb-get-file-entries directory)))
	   (mapcar
	    (lambda (x)
	      (list
	       (if (string-match "d" (nth 1 x))
		   (file-name-as-directory (nth 0 x))
		 (nth 0 x))))
	    entries)))))))

(defun tramp-smb-handle-file-newer-than-file-p (file1 file2)
  "Like `file-newer-than-file-p' for Tramp files."
  (cond
   ((not (file-exists-p file1)) nil)
   ((not (file-exists-p file2)) t)
   (t (tramp-time-less-p (nth 5 (file-attributes file2))
			 (nth 5 (file-attributes file1))))))

(defun tramp-smb-handle-file-writable-p (filename)
  "Like `file-writable-p' for Tramp files."
  (if (file-exists-p filename)
      (string-match "w" (or (nth 8 (file-attributes filename)) ""))
    (let ((dir (file-name-directory filename)))
      (and (file-exists-p dir)
	   (file-writable-p dir)))))

(defun tramp-smb-handle-insert-directory
  (filename switches &optional wildcard full-directory-p)
  "Like `insert-directory' for Tramp files."
  (setq filename (expand-file-name filename))
  (when full-directory-p
    ;; Called from `dired-add-entry'.
    (setq filename (file-name-as-directory filename)))
  (with-parsed-tramp-file-name filename nil
    (tramp-flush-file-property v (file-name-directory localname))
    (save-match-data
      (let ((base (file-name-nondirectory filename))
	    ;; We should not destroy the cache entry.
	    (entries (copy-sequence
		      (tramp-smb-get-file-entries
		       (file-name-directory filename)))))

	(when wildcard
	  (string-match "\\." base)
	  (setq base (replace-match "\\\\." nil nil base))
	  (string-match "\\*" base)
	  (setq base (replace-match ".*" nil nil base))
	  (string-match "\\?" base)
	  (setq base (replace-match ".?" nil nil base)))

	;; Filter entries.
	(setq entries
	      (delq
	       nil
	       (if (or wildcard (zerop (length base)))
		   ;; Check for matching entries.
		   (mapcar
		    (lambda (x)
		      (when (string-match
			     (format "^%s" base) (nth 0 x))
			x))
		    entries)
		 ;; We just need the only and only entry FILENAME.
		 (list (assoc base entries)))))

	;; Sort entries.
	(setq entries
	      (sort
	       entries
	       (lambda (x y)
		 (if (string-match "t" switches)
		     ;; Sort by date.
		     (tramp-time-less-p (nth 3 y) (nth 3 x))
		   ;; Sort by name.
		   (string-lessp (nth 0 x) (nth 0 y))))))

	;; Handle "-F" switch.
	(when (string-match "F" switches)
	  (mapc
	   (lambda (x)
	     (when (not (zerop (length (car x))))
	       (cond
		((char-equal ?d (string-to-char (nth 1 x)))
		 (setcar x (concat (car x) "/")))
		((char-equal ?x (string-to-char (nth 1 x)))
		 (setcar x (concat (car x) "*"))))))
	   entries))

	;; Print entries.
	(mapcar
	 (lambda (x)
	   (when (not (zerop (length (nth 0 x))))
	     (insert
	      (format
	       "%10s %3d %-8s %-8s %8s %s %s\n"
	       (nth 1 x) ; mode
	       1 "nobody" "nogroup"
	       (nth 2 x) ; size
	       (format-time-string
		(if (tramp-time-less-p
		     (tramp-time-subtract (current-time) (nth 3 x))
		     tramp-half-a-year)
		    "%b %e %R"
		  "%b %e  %Y")
		(nth 3 x)) ; date
	       (nth 0 x))) ; file name
	     (forward-line)
	     (beginning-of-line)))
	   entries)))))

(defun tramp-smb-handle-make-directory (dir &optional parents)
  "Like `make-directory' for Tramp files."
  (setq dir (directory-file-name (expand-file-name dir)))
  (unless (file-name-absolute-p dir)
    (setq dir (expand-file-name dir default-directory)))
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
	  (tramp-error v 'file-error "Couldn't make directory %s" dir))))))

(defun tramp-smb-handle-make-directory-internal (directory)
  "Like `make-directory-internal' for Tramp files."
  (setq directory (directory-file-name (expand-file-name directory)))
  (unless (file-name-absolute-p directory)
    (setq directory (expand-file-name directory default-directory)))
  (with-parsed-tramp-file-name directory nil
    (save-match-data
      (let* ((file (tramp-smb-get-localname localname t)))
	(when (file-directory-p (file-name-directory directory))
	  (tramp-smb-send-command v (format "mkdir \"%s\"" file))
	  ;; We must also flush the cache of the directory, because
	  ;; file-attributes reads the values from there.
	  (tramp-flush-file-property v (file-name-directory localname)))
	(unless (file-directory-p directory)
	  (tramp-error
	   v 'file-error "Couldn't make directory %s" directory))))))

(defun tramp-smb-handle-rename-file
  (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for Tramp files."
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

      (with-parsed-tramp-file-name newname nil
	(when (and (not ok-if-already-exists)
		   (file-exists-p newname))
	  (tramp-error v 'file-already-exists newname))
	;; We must also flush the cache of the directory, because
	;; file-attributes reads the values from there.
	(tramp-flush-file-property v (file-name-directory localname))
	(tramp-flush-file-property v localname)
	(let ((file (tramp-smb-get-localname localname t)))
	  (tramp-message v 0 "Copying file %s to file %s..." filename newname)
	  (if (tramp-smb-send-command v (format "put %s \"%s\"" filename file))
	      (tramp-message
	       v 0 "Copying file %s to file %s...done" filename newname)
	    (tramp-error v 'file-error "Cannot rename `%s'" filename))))))

  (delete-file filename))

(defun tramp-smb-handle-substitute-in-file-name (filename)
  "Like `handle-substitute-in-file-name' for Tramp files.
\"//\" substitutes only in the local filename part.  Catches
errors for shares like \"C$/\", which are common in Microsoft Windows."
  (with-parsed-tramp-file-name filename nil
    ;; Ignore in LOCALNAME everything before "//".
    (when (and (stringp localname) (string-match ".+?/\\(/\\|~\\)" localname))
      (setq filename
	    (concat (file-remote-p filename)
		    (replace-match "\\1" nil nil localname)))))
  (condition-case nil
      (tramp-run-real-handler 'substitute-in-file-name (list filename))
    (error filename)))

(defun tramp-smb-handle-write-region
  (start end filename &optional append visit lockname confirm)
  "Like `write-region' for Tramp files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (unless (eq append nil)
      (tramp-error
	 v 'file-error "Cannot append to file using Tramp (`%s')" filename))
    ;; XEmacs takes a coding system as the seventh argument, not `confirm'.
    (when (and (not (featurep 'xemacs))
	       confirm (file-exists-p filename))
      (unless (y-or-n-p (format "File %s exists; overwrite anyway? "
				filename))
	(tramp-error v 'file-error "File not overwritten")))
    ;; We must also flush the cache of the directory, because
    ;; `file-attributes' reads the values from there.
    (tramp-flush-file-property v (file-name-directory localname))
    (tramp-flush-file-property v localname)
    (let ((file (tramp-smb-get-localname localname t))
	  (curbuf (current-buffer))
	  (tmpfile (tramp-compat-make-temp-file filename)))
      ;; We say `no-message' here because we don't want the visited file
      ;; modtime data to be clobbered from the temp file.  We call
      ;; `set-visited-file-modtime' ourselves later on.
      (tramp-run-real-handler
       'write-region
       (if confirm ; don't pass this arg unless defined for backward compat.
	   (list start end tmpfile append 'no-message lockname confirm)
	 (list start end tmpfile append 'no-message lockname)))

      (tramp-message v 5 "Writing tmp file %s to file %s..." tmpfile filename)
      (if (tramp-smb-send-command v (format "put %s \"%s\"" tmpfile file))
	  (tramp-message
	   v 5 "Writing tmp file %s to file %s...done" tmpfile filename)
	(tramp-error v 'file-error "Cannot write `%s'" filename))

      (delete-file tmpfile)
      (unless (equal curbuf (current-buffer))
	(tramp-error
	 v 'file-error
	 "Buffer has changed from `%s' to `%s'" curbuf (current-buffer)))
      (when (eq visit t)
	(set-visited-file-modtime)))))


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
(defun tramp-smb-get-file-entries (directory)
  "Read entries which match DIRECTORY.
Either the shares are listed, or the `dir' command is executed.
Result is a list of (LOCALNAME MODE SIZE MONTH DAY TIME YEAR)."
  (with-parsed-tramp-file-name directory nil
    (setq localname (or localname "/"))
    (with-file-property v localname "file-entries"
      (with-current-buffer (tramp-get-buffer v)
	(let* ((share (tramp-smb-get-share localname))
	       (file (tramp-smb-get-localname localname nil))
	       (cache (tramp-get-connection-property v "share-cache" nil))
	       res entry)

	  (if (and (not share) cache)
	      ;; Return cached shares
	      (setq res cache)

	    ;; Read entries
	    (setq file (file-name-as-directory file))
	    (when (string-match "^\\./" file)
	      (setq file (substring file 1)))
	    (if share
		(tramp-smb-send-command v (format "dir \"%s*\"" file))
	      ;; `tramp-smb-maybe-open-connection' lists also the share names
	      (tramp-smb-maybe-open-connection v))

	    ;; Loop the listing
	    (goto-char (point-min))
	    (unless (re-search-forward tramp-smb-errors nil t)
	      (while (not (eobp))
		(setq entry (tramp-smb-read-file-entry share))
		(forward-line)
		(when entry (add-to-list 'res entry))))

	    ;; Cache share entries
	    (unless share
	      (tramp-set-connection-property v "share-cache" res)))

	  ;; Add directory itself
	  (add-to-list 'res '("" "drwxrwxrwx" 0 (0 0)))

	  ;; There's a very strange error (debugged with XEmacs 21.4.14)
	  ;; If there's no short delay, it returns nil.  No idea about.
	  (when (featurep 'xemacs) (sleep-for 0.01))

	  ;; Return entries
	  (delq nil res))))))

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
;; [ 12][0-9]                             - day
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
;; We are called from `tramp-smb-get-file-entries', which sets the
;; current buffer.
  (let ((line (buffer-substring (point) (tramp-compat-line-end-position)))
	localname mode size month day hour min sec year mtime)

    (if (not share)

	;; Read share entries.
	(when (string-match "^\\s-+\\(\\S-\\(.*\\S-\\)?\\)\\s-+Disk" line)
	  (setq localname (match-string 1 line)
		mode "dr-xr-xr-x"
		size 0))

      ;; Real listing.
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
		 (cdr (assoc (downcase month) tramp-parse-time-months))
		 year)
	      '(0 0)))
      (list localname mode size mtime))))


;; Connection functions

(defun tramp-smb-send-command (vec command)
  "Send the COMMAND to connection VEC.
Returns nil if there has been an error message from smbclient."
  (tramp-smb-maybe-open-connection vec)
  (tramp-message vec 6 "%s" command)
  (tramp-send-string vec command)
  (tramp-smb-wait-for-output vec))

(defun tramp-smb-maybe-open-connection (vec)
  "Maybe open a connection to HOST, log in as USER, using `tramp-smb-program'.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
  (let* ((share (tramp-smb-get-share (tramp-file-name-localname vec)))
	 (buf (tramp-get-buffer vec))
	 (p (get-buffer-process buf)))

    ;; If too much time has passed since last command was sent, look
    ;; whether has been an error message; maybe due to connection timeout.
    (with-current-buffer buf
      (goto-char (point-min))
      (when (and (> (tramp-time-diff
		     (current-time)
		     (tramp-get-connection-property
		      p "last-cmd-time" '(0 0 0)))
		    60)
		 p (processp p) (memq (process-status p) '(run open))
		 (re-search-forward tramp-smb-errors nil t))
	(delete-process p)
	(setq p nil)))

    ;; Check whether it is still the same share.
    (unless
	(and p (processp p) (memq (process-status p) '(run open))
	     (string-equal
	      share
	      (tramp-get-connection-property p "smb-share" "")))

      (save-match-data
	;; There might be unread output from checking for share names.
	(when buf (with-current-buffer buf (erase-buffer)))
	(when (and p (processp p)) (delete-process p))

	(unless (let ((default-directory
			(tramp-compat-temporary-file-directory)))
		  (executable-find tramp-smb-program))
	  (error "Cannot find command %s in %s" tramp-smb-program exec-path))

	(let* ((user (tramp-file-name-user vec))
	       (host (tramp-file-name-host vec))
	       (real-user user)
	       (real-host host)
	       domain port args)

	  ;; Check for domain ("user%domain") and port ("host#port").
	  (when (and user (string-match "\\(.+\\)%\\(.+\\)" user))
	    (setq real-user (or (match-string 1 user) user)
		  domain (match-string 2 user)))

	  (when (and host (string-match "\\(.+\\)#\\(.+\\)" host))
	    (setq real-host (or (match-string 1 host) host)
		  port (match-string 2 host)))

	  (if share
	      (setq args (list (concat "//" real-host "/" share)))
	    (setq args (list "-L" real-host )))

	  (if (not (zerop (length real-user)))
	      (setq args (append args (list "-U" real-user)))
	    (setq args (append args (list "-N"))))

	  (when domain (setq args (append args (list "-W" domain))))
	  (when port   (setq args (append args (list "-p" port))))
	  (setq args (append args (list "-s" "/dev/null")))

	  ;; OK, let's go.
	  (tramp-message
	   vec 3 "Opening connection for //%s%s/%s..."
	   (if (not (zerop (length user))) (concat user "@") "")
	   host (or share ""))

	  (let* ((coding-system-for-read nil)
		 (process-connection-type tramp-process-connection-type)
		 (p (let ((default-directory
			    (tramp-compat-temporary-file-directory)))
		      (apply #'start-process
			     (tramp-buffer-name vec) (tramp-get-buffer vec)
			     tramp-smb-program args))))

	    (tramp-message
	     vec 6 "%s" (mapconcat 'identity (process-command p) " "))
	    (tramp-set-process-query-on-exit-flag p nil)
	    (tramp-set-connection-property p "smb-share" share)

	    ;; Set variables for computing the prompt for reading password.
	    (setq tramp-current-method tramp-smb-method
		  tramp-current-user user
		  tramp-current-host host)

	    ;; Set chunksize.  Otherwise, `tramp-send-string' might
	    ;; try it itself.
	    (tramp-set-connection-property p "chunksize" tramp-chunksize)

	    ;; Play login scenario.
	    (tramp-process-actions
	     p vec
	     (if share
		 tramp-smb-actions-with-share
	       tramp-smb-actions-without-share))

	    (tramp-message
	     vec 3 "Opening connection for //%s%s/%s...done"
	     (if (not (zerop (length user))) (concat user "@") "")
	     host (or share ""))))))))

;; We don't use timeouts.  If needed, the caller shall wrap around.
(defun tramp-smb-wait-for-output (vec)
  "Wait for output from smbclient command.
Returns nil if an error message has appeared."
  (with-current-buffer (tramp-get-buffer vec)
    (let ((p (get-buffer-process (current-buffer)))
	  (found (progn (goto-char (point-min))
			(re-search-forward tramp-smb-prompt nil t)))
	  (err   (progn (goto-char (point-min))
			(re-search-forward tramp-smb-errors nil t))))

      ;; Algorithm: get waiting output.  See if last line contains
      ;; tramp-smb-prompt sentinel or tramp-smb-errors strings.
      ;; If not, wait a bit and again get waiting output.
      (while (and (not found) (not err))

	;; Accept pending output.
	(tramp-accept-process-output p)

	;; Search for prompt.
	(goto-char (point-min))
	(setq found (re-search-forward tramp-smb-prompt nil t))

	;; Search for errors.
	(goto-char (point-min))
	(setq err (re-search-forward tramp-smb-errors nil t)))

      ;; When the process is still alive, read pending output.
      (while (and (not found) (memq (process-status p) '(run open)))

	;; Accept pending output.
	(tramp-accept-process-output p)

	;; Search for prompt.
	(goto-char (point-min))
	(setq found (re-search-forward tramp-smb-prompt nil t)))

      ;; Return value is whether no error message has appeared.
      (tramp-message vec 6 "\n%s" (buffer-string))
      (not err))))


(provide 'tramp-smb)

;;; TODO:

;; * Error handling in case password is wrong.
;; * Read password from "~/.netrc".
;; * Return more comprehensive file permission string.  Think whether it is
;;   possible to implement `set-file-modes'.
;; * Handle links (FILENAME.LNK).
;; * Try to remove the inclusion of dummy "" directory.  Seems to be at
;;   several places, especially in `tramp-smb-handle-insert-directory'.
;; * (RMS) Use unwind-protect to clean up the state so as to make the state
;;   regular again.
;; * Make it multi-hop capable.

;; arch-tag: fcc9dbec-7503-4d73-b638-3c8aa59575f5
;;; tramp-smb.el ends here
