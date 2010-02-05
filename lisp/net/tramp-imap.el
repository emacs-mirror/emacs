;;; tramp-imap.el --- Tramp interface to IMAP through imap.el

;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: mail, comm

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

;; Package to provide Tramp over IMAP

;;; Setup:

;; just load and open files, e.g.
;; /imaps:user@yourhosthere.com:/INBOX.test/1
;; or
;; /imap:user@yourhosthere.com:/INBOX.test/1

;; where `imap' goes over IMAP, while `imaps' goes over IMAP+SSL

;; This module will use imap-hash.el to access the IMAP mailbox.

;; This module will use auth-source.el to authenticate against the
;; IMAP server, PLUS it will use auth-source.el to get your passphrase
;; for the symmetrically encrypted messages.  For the former, use the
;; usual IMAP ports.  For the latter, use the port "tramp-imap".

;; example .authinfo / .netrc file:

;; machine yourhosthere.com port tramp-imap login USER password SYMMETRIC-PASSPHRASE

;; note above is the symmetric encryption passphrase for GPG
;; below is the regular password for IMAP itself and other things on that host

;; machine yourhosthere.com login USER password NORMAL-PASSWORD


;;; Code:

(require 'assoc)
(require 'tramp)
(require 'tramp-compat)

(autoload 'auth-source-user-or-password "auth-source")
(autoload 'epg-context-operation "epg")
(autoload 'epg-context-set-armor "epg")
(autoload 'epg-context-set-passphrase-callback "epg")
(autoload 'epg-context-set-progress-callback "epg")
(autoload 'epg-decrypt-string "epg")
(autoload 'epg-encrypt-string "epg")
(autoload 'imap-hash-get "imap-hash")
(autoload 'imap-hash-make "imap-hash")
(autoload 'imap-hash-map "imap-hash")
(autoload 'imap-hash-put "imap-hash")
(autoload 'imap-hash-rem "imap-hash")

;; We use the additional header "X-Size" for encoding the size of a file.
(eval-after-load "imap-hash"
  '(add-to-list 'imap-hash-headers 'X-Size 'append))

;; Define Tramp IMAP method ...
(defconst tramp-imap-method "imap"
  "*Method to connect via IMAP protocol.")

(add-to-list 'tramp-methods (list tramp-imap-method '(tramp-default-port 143)))

;; Add a default for `tramp-default-user-alist'.  Default is the local user.
(add-to-list 'tramp-default-user-alist
	     `(,tramp-imap-method nil ,(user-login-name)))

;; Define Tramp IMAPS method ...
(defconst tramp-imaps-method "imaps"
  "*Method to connect via secure IMAP protocol.")

;; ... and add it to the method list.
(add-to-list 'tramp-methods (list tramp-imaps-method '(tramp-default-port 993)))

;; Add a default for `tramp-default-user-alist'.  Default is the local user.
(add-to-list 'tramp-default-user-alist
	     `(,tramp-imaps-method nil ,(user-login-name)))

;; Add completion function for IMAP method.
;; (tramp-set-completion-function
;;  tramp-imap-method tramp-completion-function-alist-ssh) ; TODO: test this
;;  tramp-imaps-method tramp-completion-function-alist-ssh) ; TODO: test this

;; New handlers should be added here.
(defconst tramp-imap-file-name-handler-alist
  '(
    ;; `access-file' performed by default handler
    (add-name-to-file . ignore)
    ;; `byte-compiler-base-file-name' performed by default handler
    ;; `copy-directory' performed by default handler
    (copy-file . tramp-imap-handle-copy-file)
    (delete-directory . ignore) ;; tramp-imap-handle-delete-directory)
    (delete-file . tramp-imap-handle-delete-file)
    ;; `diff-latest-backup-file' performed by default handler
    (directory-file-name . tramp-handle-directory-file-name)
    (directory-files . tramp-handle-directory-files)
    (directory-files-and-attributes
     . tramp-imap-handle-directory-files-and-attributes)
    (dired-call-process . ignore)
    ;; `dired-compress-file' performed by default handler
    ;; `dired-uncache' performed by default handler
    (expand-file-name . tramp-imap-handle-expand-file-name)
    ;; `file-accessible-directory-p' performed by default handler
    (file-attributes . tramp-imap-handle-file-attributes)
    (file-directory-p .  tramp-imap-handle-file-directory-p)
    (file-executable-p . tramp-imap-handle-file-executable-p)
    (file-exists-p . tramp-imap-handle-file-exists-p)
    (file-local-copy . tramp-imap-handle-file-local-copy)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions . tramp-imap-handle-file-name-all-completions)
    (file-name-as-directory . tramp-handle-file-name-as-directory)
    (file-name-completion . tramp-handle-file-name-completion)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    ;; `file-name-sans-versions' performed by default handler
    (file-newer-than-file-p . tramp-imap-handle-file-newer-than-file-p)
    (file-ownership-preserved-p . ignore)
    (file-readable-p . tramp-imap-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-symlink-p . tramp-handle-file-symlink-p)
    ;; `file-truename' performed by default handler
    (file-writable-p . tramp-imap-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    ;; `find-file-noselect' performed by default handler
    ;; `get-file-buffer' performed by default handler
    (insert-directory . tramp-imap-handle-insert-directory)
    (insert-file-contents . tramp-imap-handle-insert-file-contents)
    (load . tramp-handle-load)
    (make-directory . ignore) ;; tramp-imap-handle-make-directory)
    (make-directory-internal . ignore) ;; tramp-imap-handle-make-directory-internal)
    (make-symbolic-link . ignore)
    (rename-file . tramp-imap-handle-rename-file)
    (set-file-modes . ignore)
    (set-file-times . ignore) ;; tramp-imap-handle-set-file-times)
    (set-visited-file-modtime . ignore)
    (shell-command . ignore)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (unhandled-file-name-directory . tramp-handle-unhandled-file-name-directory)
    (vc-registered . ignore)
    (verify-visited-file-modtime . ignore)
    (write-region . tramp-imap-handle-write-region)
    (executable-find . ignore)
    (start-file-process . ignore)
    (process-file . ignore)
)
  "Alist of handler functions for Tramp IMAP method.
Operations not mentioned here will be handled by the default Emacs primitives.")

(defgroup tramp-imap nil
  "Tramp over IMAP configuration."
  :version "23.2"
  :group 'applications)

(defcustom tramp-imap-subject-marker "tramp-imap-subject-marker"
  "The subject marker that Tramp-IMAP will use."
  :type 'string
  :version "23.2"
  :group 'tramp-imap)

;; TODO: these will be defcustoms later.
(defvar tramp-imap-passphrase-cache nil) ;; can be t or 'never
(defvar tramp-imap-passphrase nil)

(defun tramp-imap-file-name-p (filename)
  "Check if it's a filename for IMAP protocol."
  (let ((v (tramp-dissect-file-name filename)))
    (or
     (string= (tramp-file-name-method v) tramp-imap-method)
     (string= (tramp-file-name-method v) tramp-imaps-method))))

(defun tramp-imap-file-name-handler (operation &rest args)
  "Invoke the IMAP related OPERATION.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION."
  (let ((fn (assoc operation tramp-imap-file-name-handler-alist)))
    (if fn
	(save-match-data (apply (cdr fn) args))
      (tramp-run-real-handler operation args))))

(add-to-list 'tramp-foreign-file-name-handler-alist
	     (cons 'tramp-imap-file-name-p 'tramp-imap-file-name-handler))

(defun tramp-imap-handle-copy-file
  (filename newname &optional ok-if-already-exists keep-date preserve-uid-gid)
  "Like `copy-file' for Tramp files."
  (tramp-imap-do-copy-or-rename-file
   'copy filename newname ok-if-already-exists keep-date preserve-uid-gid))

(defun tramp-imap-handle-rename-file
  (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for Tramp files."
  (tramp-imap-do-copy-or-rename-file
   'rename filename newname ok-if-already-exists t t))

(defun tramp-imap-do-copy-or-rename-file
  (op filename newname &optional ok-if-already-exists keep-date preserve-uid-gid)
  "Copy or rename a remote file.
OP must be `copy' or `rename' and indicates the operation to perform.
FILENAME specifies the file to copy or rename, NEWNAME is the name of
the new file (for copy) or the new name of the file (for rename).
OK-IF-ALREADY-EXISTS means don't barf if NEWNAME exists already.
KEEP-DATE means to make sure that NEWNAME has the same timestamp
as FILENAME.  PRESERVE-UID-GID, when non-nil, instructs to keep
the uid and gid if both files are on the same host.

This function is invoked by `tramp-imap-handle-copy-file' and
`tramp-imap-handle-rename-file'.  It is an error if OP is neither
of `copy' and `rename'."
  (unless (memq op '(copy rename))
    (error "Unknown operation `%s', must be `copy' or `rename'" op))
  (setq filename (expand-file-name filename))
  (setq newname (expand-file-name newname))
  (when (file-directory-p newname)
    (setq newname (expand-file-name (file-name-nondirectory filename) newname)))

  (let ((t1 (and (tramp-tramp-file-p filename)
		 (tramp-imap-file-name-p filename)))
	(t2 (and (tramp-tramp-file-p newname)
		 (tramp-imap-file-name-p newname))))

    (when (and (not ok-if-already-exists) (file-exists-p newname))
      (with-parsed-tramp-file-name (if t1 filename newname) nil
	(tramp-error
	 v 'file-already-exists "File %s already exists" newname)))

    (with-parsed-tramp-file-name (if t1 filename newname) nil
      (tramp-message v 0 "Transferring %s to %s..." filename newname))

    ;; We just make a local copy of FILENAME, and write it then to
    ;; NEWNAME.  This must be optimized, when both files are located
    ;; on the same IMAP server.
    (with-temp-buffer
      (if (and t1 t2)
	  ;; We don't encrypt.
	  (with-parsed-tramp-file-name newname nil
	    (insert (tramp-imap-get-file filename nil))
	    (tramp-imap-put-file
	     v (current-buffer)
	     (tramp-imap-file-name-name v)
	     nil nil (nth 7 (file-attributes filename))))
	;; One of them is not located on a IMAP mailbox.
	(insert-file-contents filename)
	(write-region (point-min) (point-max) newname)))

    (with-parsed-tramp-file-name (if t1 filename newname) nil
      (tramp-message v 0 "Transferring %s to %s...done" filename newname))

    (when (eq op 'rename)
      (delete-file filename))))

;; TODO: revise this much
(defun tramp-imap-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for Tramp files."
  ;; If DIR is not given, use DEFAULT-DIRECTORY or "/".
  (setq dir (or dir default-directory "/"))
  ;; Unless NAME is absolute, concat DIR and NAME.
  (unless (file-name-absolute-p name)
    (setq name (concat (file-name-as-directory dir) name)))
  ;; If NAME is not a Tramp file, run the real handler.
  (if (or (tramp-completion-mode-p) (not (tramp-tramp-file-p name)))
      (tramp-drop-volume-letter
       (tramp-run-real-handler 'expand-file-name (list name nil)))
    ;; Dissect NAME.
    (with-parsed-tramp-file-name name nil
      (unless (tramp-run-real-handler 'file-name-absolute-p (list localname))
	(setq localname (concat "/" localname)))
      ;; There might be a double slash, for example when "~/"
      ;; expands to "/".  Remove this.
      (while (string-match "//" localname)
	(setq localname (replace-match "/" t t localname)))
      ;; Do normal `expand-file-name' (this does "/./" and "/../").
      ;; We bind `directory-sep-char' here for XEmacs on Windows,
      ;; which would otherwise use backslash.  `default-directory' is
      ;; bound, because on Windows there would be problems with UNC
      ;; shares or Cygwin mounts.
      (let ((default-directory (tramp-compat-temporary-file-directory)))
	(tramp-make-tramp-file-name
	 method user host
	 (tramp-drop-volume-letter
	  (tramp-run-real-handler
	   'expand-file-name (list localname))))))))

;; This function should return "foo/" for directories and "bar" for
;; files.
(defun tramp-imap-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for Tramp files."
  (all-completions
   filename
   (with-parsed-tramp-file-name (expand-file-name directory) nil
     (save-match-data
       (let ((entries
	      (tramp-imap-get-file-entries v localname)))
	 (mapcar
	  (lambda (x)
	    (list
	     (if (string-match "d" (nth 9 x))
		 (file-name-as-directory (nth 0 x))
	       (nth 0 x))))
	  entries))))))

(defun tramp-imap-get-file-entries (vec localname &optional exact)
  "Read entries returned by IMAP server. EXACT limits to exact matches.
Result is a list of (LOCALNAME LINK COUNT UID GID ATIME MTIME CTIME
SIZE MODE WEIRD INODE DEVICE)."
  (tramp-message vec 5 "working on %s" localname)
  (let* ((name (tramp-imap-file-name-name vec))
	 (search-name (or name ""))
	 (search-name (if exact (concat search-name "$") search-name))
	 (iht (tramp-imap-make-iht vec search-name)))
;; TODO: catch errors
    ;; (tramp-error vec 'none "bad name %s or mailbox %s" name mbox))
    (imap-hash-map (lambda (uid headers body)
		     (let ((subject (substring
				     (aget headers 'Subject "")
				     (length tramp-imap-subject-marker)))
			   (from (aget headers 'From ""))
			   (date (date-to-time (aget headers 'Date "")))
			   (size (string-to-number
				  (or (aget headers 'X-Size "0") "0"))))
		       (setq from
			     (if (string-match "<\\([^@]+\\)@" from)
				 (match-string 1 from)
			       "nobody"))
		       (list
			subject
			nil
			-1
			from
			"nogroup"
			date
			date
			date
			size
			"-rw-rw-rw-"
			nil
			uid
			(tramp-get-device vec))))
		   iht t)))

(defun tramp-imap-handle-write-region (start end filename &optional append visit lockname confirm)
  "Like `write-region' for Tramp files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    ;; XEmacs takes a coding system as the seventh argument, not `confirm'.
    (when (and (not (featurep 'xemacs))
	       confirm (file-exists-p filename))
      (unless (y-or-n-p (format "File %s exists; overwrite anyway? "
				filename))
	(tramp-error v 'file-error "File not overwritten")))
    (tramp-flush-file-property v localname)
    (let* ((old-buffer (current-buffer))
	   (inode (tramp-imap-get-file-inode filename))
	   (min 1)
	   (max (point-max))
	   ;; Make sure we have good start and end values.
	   (start (or start min))
	   (end (or end max))
	   temp-buffer)
      (with-temp-buffer
	(setq temp-buffer (if (and (eq start min) (eq end max))
			      old-buffer
			    ;; If this is a region write, insert the substring.
			    (insert
			     (with-current-buffer old-buffer
			       (buffer-substring-no-properties start end)))
			    (current-buffer)))
	(tramp-imap-put-file v
			     temp-buffer
			     (tramp-imap-file-name-name v)
			     inode
			     t)))
    (when (eq visit t)
      (set-visited-file-modtime))))

(defun tramp-imap-handle-insert-directory
  (filename switches &optional wildcard full-directory-p)
  "Like `insert-directory' for Tramp files."
  (setq filename (expand-file-name filename))
  (if full-directory-p
      ;; Called from `dired-add-entry'.
      (setq filename (file-name-as-directory filename))
    (setq filename (directory-file-name filename)))
  (with-parsed-tramp-file-name filename nil
    (save-match-data
      (let ((base (file-name-nondirectory localname))
	    (entries (copy-sequence
		      (tramp-imap-get-file-entries
		       v (file-name-directory localname)))))

	(when wildcard
	  (when (string-match "\\." base)
	    (setq base (replace-match "\\\\." nil nil base)))
	  (when (string-match "\\*" base)
	    (setq base (replace-match ".*" nil nil base)))
	  (when (string-match "\\?" base)
	    (setq base (replace-match ".?" nil nil base))))

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
		     (tramp-time-less-p (nth 6 y) (nth 6 x))
		   ;; Sort by name.
		   (string-lessp (nth 0 x) (nth 0 y))))))

	;; Handle "-F" switch.
	(when (string-match "F" switches)
	  (mapc
	   (lambda (x)
	     (when (not (zerop (length (car x))))
	       (cond
		((char-equal ?d (string-to-char (nth 9 x)))
		 (setcar x (concat (car x) "/")))
		((char-equal ?x (string-to-char (nth 9 x)))
		 (setcar x (concat (car x) "*"))))))
	   entries))

	;; Print entries.
	(mapcar
	 (lambda (x)
	   (when (not (zerop (length (nth 0 x))))
	     (insert
	      (format
	       "%10s %3d %-8s %-8s %8s %s "
	       (nth 9 x) ; mode
	       (nth 11 x) ; inode
	       (nth 3 x) ; uid
	       (nth 4 x) ; gid
	       (nth 8 x) ; size
	       (format-time-string
		(if (tramp-time-less-p
		     (tramp-time-subtract (current-time) (nth 6 x))
		     tramp-half-a-year)
		    "%b %e %R"
		  "%b %e  %Y")
		(nth 6 x)))) ; date
	     ;; For the file name, we set the `dired-filename'
	     ;; property.  This allows to handle file names with
	     ;; leading or trailing spaces as well.  The inserted name
	     ;; could be from somewhere else, so we use the relative
	     ;; file name of `default-directory'.
	     (let ((pos (point)))
	       (insert
		(format
		 "%s\n"
		 (file-relative-name
		  (expand-file-name (nth 0 x) (file-name-directory filename)))))
	       (put-text-property pos (1- (point)) 'dired-filename t))
	     (forward-line)
	     (beginning-of-line)))
	 entries)))))

(defun tramp-imap-handle-insert-file-contents
  (filename &optional visit beg end replace)
  "Like `insert-file-contents' for Tramp files."
  (barf-if-buffer-read-only)
  (when visit
    (setq buffer-file-name (expand-file-name filename))
    (set-visited-file-modtime)
    (set-buffer-modified-p nil))
  (with-parsed-tramp-file-name filename nil
    (if (not (file-exists-p filename))
	(tramp-error
	 v 'file-error "File `%s' not found on remote host" filename)
      (let ((point (point))
	    size data)
	(tramp-message v 4 "Fetching file %s..." filename)
	(insert (tramp-imap-get-file filename t))
	(setq size (- (point) point))
;;; TODO: handle ranges.
;;; 	       (let ((beg (or beg (point-min)))
;;; 		   (end (min (or end (point-max)) (point-max))))
;;; 		 (setq size (- end beg))
;;; 	       (buffer-substring beg end))
	(goto-char point)
	(tramp-message v 4 "Fetching file %s...done" filename)
	(list (expand-file-name filename) size)))))

(defun tramp-imap-handle-file-exists-p (filename)
  "Like `file-exists-p' for Tramp files."
  (and (file-attributes filename) t))

(defun tramp-imap-handle-file-directory-p (filename)
  "Like `file-directory-p' for Tramp-IMAP files."
  ;; We allow only mailboxes to be a directory.
  (with-parsed-tramp-file-name (expand-file-name filename default-directory) nil
    (and (string-match "^/[^/]*$" (directory-file-name localname)) t)))

(defun tramp-imap-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for Tramp-IMAP FILENAME."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (let ((res (cdr-safe (nth 0 (tramp-imap-get-file-entries v localname)))))
      (unless (or (null res) (eq id-format 'string))
	(setcar (nthcdr 2 res) 1)
	(setcar (nthcdr 3 res) 1))
      res)))

(defun tramp-imap-get-file-inode (filename &optional id-format)
  "Get inode equivalent \(actually the UID) for Tramp-IMAP FILENAME."
  (nth 10 (tramp-compat-file-attributes filename id-format)))

(defun tramp-imap-handle-file-executable-p (filename)
  "Like `file-executable-p' for Tramp files.  False for IMAP."
  nil)

(defun tramp-imap-handle-file-readable-p (filename)
  "Like `file-readable-p' for Tramp files.  True for IMAP."
  (file-exists-p filename))

(defun tramp-imap-handle-file-writable-p (filename)
  "Like `file-writable-p' for Tramp files.  True for IMAP."
  ;; `file-exists-p' does not work yet for directories.
  ;; (file-exists-p (file-name-directory filename)))
  (file-directory-p (file-name-directory filename)))

(defun tramp-imap-handle-delete-file (filename)
  "Like `delete-file' for Tramp files."
  (cond
   ((not (file-exists-p filename)) nil)
   (t (with-parsed-tramp-file-name (expand-file-name filename) nil
	(let ((iht (tramp-imap-make-iht v)))
	  (imap-hash-rem (tramp-imap-get-file-inode filename) iht))))))

(defun tramp-imap-handle-directory-files-and-attributes
  (directory &optional full match nosort id-format)
  "Like `directory-files-and-attributes' for Tramp files."
  (mapcar
   (lambda (x)
     (cons x (tramp-compat-file-attributes
	      (if full x (expand-file-name x directory)) id-format)))
   (directory-files directory full match nosort)))

;; TODO: fix this in tramp-imap-get-file-entries.
(defun tramp-imap-handle-file-newer-than-file-p (file1 file2)
  "Like `file-newer-than-file-p' for Tramp files."
  (cond
   ((not (file-exists-p file1)) nil)
   ((not (file-exists-p file2)) t)
   (t (tramp-time-less-p (nth 5 (file-attributes file2))
			 (nth 5 (file-attributes file1))))))

(defun tramp-imap-handle-file-local-copy (filename)
  "Like `file-local-copy' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (unless (file-exists-p filename)
      (tramp-error
       v 'file-error
       "Cannot make local copy of non-existing file `%s'" filename))
    (let ((tmpfile (tramp-compat-make-temp-file filename)))
      (tramp-message v 4 "Fetching %s to tmp file %s..." filename tmpfile)
      (with-temp-buffer
	(insert-file-contents filename)
 	(write-region (point-min) (point-max) tmpfile)
 	(tramp-message v 4 "Fetching %s to tmp file %s...done" filename tmpfile)
 	tmpfile))))

(defun tramp-imap-put-file
  (vec filename-or-buffer &optional subject inode encode size)
  "Write contents of FILENAME-OR-BUFFER to Tramp-IMAP file VEC with name SUBJECT.
When INODE is given, delete that old remote file after writing the new one
\(normally this is the old file with the same name).  A non-nil ENCODE
forces the encoding of the buffer or file.  SIZE, when available, indicates
the file size; this is needed, if the file or buffer is already encoded."
  ;; `tramp-current-host' is used in `tramp-imap-passphrase-callback-function'.
  (let ((tramp-current-host (tramp-file-name-real-host vec))
	(iht (tramp-imap-make-iht vec)))
    (imap-hash-put (list
		    (list (cons
			   'Subject
			   (format
			    "%s%s"
			    tramp-imap-subject-marker
			    (or subject "no subject")))
			  (cons
			   'X-Size
			   (number-to-string
			    (cond
			     ((numberp size) size)
			     ((bufferp filename-or-buffer)
			      (buffer-size filename-or-buffer))
			     ((stringp filename-or-buffer)
			      (nth 7 (file-attributes filename-or-buffer)))
			     ;; We don't know the size.
			     (t -1)))))
		    (cond ((bufferp filename-or-buffer)
			   (with-current-buffer filename-or-buffer
			     (if encode
				 (tramp-imap-encode-buffer)
			       (buffer-string))))
			  ;; TODO: allow file names.
			  (t "No body available")))
		   iht
		   inode)))

(defun tramp-imap-get-file (filename &optional decode)
  ;; (debug (tramp-imap-get-file-inode filename))
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (condition-case ()
	;; `tramp-current-host' is used in
	;; `tramp-imap-passphrase-callback-function'.
	(let* ((tramp-current-host (tramp-file-name-real-host v))
	       (iht (tramp-imap-make-iht v))
	       (inode (tramp-imap-get-file-inode filename))
	       (data (imap-hash-get inode iht t)))
	  (if decode
	      (with-temp-buffer
		(insert (nth 1 data))
		;;(debug inode (buffer-string))
		(tramp-imap-decode-buffer))
	    (nth 1 data)))
      (error (tramp-error
	      v 'file-error "File `%s' could not be read" filename)))))

(defun tramp-imap-passphrase-callback-function (context key-id handback)
  "Called by EPG to get a passphrase for Tramp-IMAP.
CONTEXT is the encryption/decryption EPG context.
HANDBACK is just carried through.
KEY-ID can be 'SYM or 'PIN among others."
  (let* ((server tramp-current-host)
	 (port "tramp-imap")		; this is NOT the server password!
	 (auth-passwd
	  (auth-source-user-or-password "password" server port)))
    (or
     (copy-sequence auth-passwd)
     ;; If we cache the passphrase and we have one.
     (if (and (eq tramp-imap-passphrase-cache t)
	      tramp-imap-passphrase)
	 ;; Do we reuse it?
	 (if (y-or-n-p "Reuse the passphrase? ")
	     (copy-sequence tramp-imap-passphrase)
	   ;; Don't reuse: revert caching behavior to nil, erase passphrase,
	   ;; call ourselves again.
	   (setq tramp-imap-passphrase-cache nil)
	   (setq tramp-imap-passphrase nil)
	   (tramp-imap-passphrase-callback-function context key-id handback))
       (let ((p (if (eq key-id 'SYM)
		    (read-passwd
		     "Tramp-IMAP passphrase for symmetric encryption: "
		     (eq (epg-context-operation context) 'encrypt)
		     tramp-imap-passphrase)
		  (read-passwd
		   (if (eq key-id 'PIN)
		       "Tramp-IMAP passphrase for PIN: "
		     (let ((entry (assoc key-id
					 (symbol-value 'epg-user-id-alist))))
		       (if entry
			   (format "Tramp-IMAP passphrase for %s %s: "
				   key-id (cdr entry))
			 (format "Tramp-IMAP passphrase for %s: " key-id))))
		   nil
		   tramp-imap-passphrase))))

	 ;; If we have an answer, the passphrase has changed,
	 ;; the user hasn't declined keeping the passphrase,
	 ;; and they answer yes to keep it now...
	 (when (and
		p
		(not (equal tramp-imap-passphrase p))
		(not (eq tramp-imap-passphrase-cache 'never))
		(y-or-n-p "Keep the passphrase? "))
	   (setq tramp-imap-passphrase (copy-sequence p))
	   (setq tramp-imap-passphrase-cache t))

	 ;; If we still don't have a passphrase, the user didn't want
	 ;; to keep it.
	 (when (and
		p
		(not tramp-imap-passphrase))
	   (setq tramp-imap-passphrase-cache 'never))

	 p)))))

(defun tramp-imap-encode-buffer ()
  (let ((context (epg-make-context 'OpenPGP))
	cipher)
    (epg-context-set-armor context t)
    (epg-context-set-passphrase-callback context
					 #'tramp-imap-passphrase-callback-function)
    (epg-context-set-progress-callback context
				       (cons #'epa-progress-callback-function
					     "Encrypting..."))
    (message "Encrypting...")
    (setq cipher (epg-encrypt-string
		  context
		  (encode-coding-string (buffer-string) 'utf-8)
		  nil))
    (message "Encrypting...done")
    cipher))

(defun tramp-imap-decode-buffer ()
  (let ((context (epg-make-context 'OpenPGP))
	plain)
    (epg-context-set-passphrase-callback context
					 #'tramp-imap-passphrase-callback-function)
    (epg-context-set-progress-callback context
				       (cons #'epa-progress-callback-function
					     "Decrypting..."))
    (message "Decrypting...")
    (setq plain (decode-coding-string
		 (epg-decrypt-string context (buffer-string))
		 'utf-8))
    (message "Decrypting...done")
    plain))

(defun tramp-imap-file-name-mailbox (vec)
  (nth 0 (tramp-imap-file-name-parse vec)))

(defun tramp-imap-file-name-name (vec)
  (nth 1 (tramp-imap-file-name-parse vec)))

(defun tramp-imap-file-name-localname (vec)
  (nth 1 (tramp-imap-file-name-parse vec)))

(defun tramp-imap-file-name-parse (vec)
  (let ((name (substring-no-properties (tramp-file-name-localname vec))))
    (if (string-match "^/\\([^/]+\\)/?\\(.*\\)$" name)
	(list (match-string 1 name)
	      (match-string 2 name))
      nil)))

(defun tramp-imap-make-iht (vec &optional needed-subject)
  "Translate the Tramp vector VEC to the imap-hash structure.
With NEEDED-SUBJECT, alters the imap-hash test accordingly."
  (let* ((mbox (tramp-imap-file-name-mailbox vec))
	 (server (tramp-file-name-real-host vec))
	 (method (tramp-file-name-method vec))
	 (user (tramp-file-name-user vec))
	 (ssl (string-equal method tramp-imaps-method))
	 (port (or (tramp-file-name-port vec)
		   (tramp-get-method-parameter method 'tramp-default-port)))
	 (result (imap-hash-make server port mbox user nil ssl)))
    ;; Return the IHT with a test override to look for the subject
    ;; marker.
    (plist-put
     result
     :test (format "^%s%s"
		   tramp-imap-subject-marker
		   (if needed-subject needed-subject "")))))

;;; TODO:

;; * Implement `tramp-imap-handle-delete-directory',
;;   `tramp-imap-handle-make-directory',
;;   `tramp-imap-handle-make-directory-internal',
;;   `tramp-imap-handle-set-file-times'.

;; * Encode the subject.  If the filename has trailing spaces (like
;;   "test   "), those characters get lost, for example in dired listings.

;; * When opening a dired buffer, like "/imap::INBOX.test", there are
;;   several error messages:
;;   "Buffer has a running process; kill it? (yes or no) "
;;   "error in process filter: Internal error, tag 6 status BAD code nil text No mailbox selected."
;;   Afterwards, everything seems to be fine.

;; * imaps works for local IMAP servers.  Accessing
;;   "/imaps:imap.gmail.com:/INBOX.test/" results in error
;;   "error in process filter: Internal error, tag 5 status BAD code nil text UNSELECT not allowed now."

;; * Improve `tramp-imap-handle-file-attributes' for directories.

;; * Saving a file creates a second one, instead of overwriting.

;; * Backup files: just *one* is kept.

;; * Password requests shall have a descriptive prompt.

;; * Exiting Emacs, there are running IMAP processes.  Make them quiet
;;   by `set-process-query-on-exit-flag'.

(provide 'tramp-imap)
;;; tramp-imap.el ends here

;; Ignore, for testing only.

;;; (setq tramp-imap-subject-marker "T")
;;; (tramp-imap-get-file-entries (tramp-dissect-file-name "/imap:yourhosthere.com:/INBOX.test/4") t)
;;; (tramp-imap-get-file-entries (tramp-dissect-file-name "/imap:yourhosthere.com:/INBOX.test/") t)
;;; (tramp-imap-get-file-entries (tramp-dissect-file-name "/imap:yourhosthere.com:/test/4") t)
;;; (tramp-imap-get-file-entries (tramp-dissect-file-name "/imap:yourhosthere.com:/test/") t)
;;; (tramp-imap-get-file-entries (tramp-dissect-file-name "/imap:yourhosthere.com:/test/welcommen") t)
;;; (tramp-imap-get-file-entries (tramp-dissect-file-name "/imap:yourhosthere.com:/test/welcommen") t t)
;;;(tramp-imap-get-file-inode "/imap:yourhosthere.com:/test/welcome")
;;; (dired-copy-file "/etc/fstab" "/imap:yourhosthere.com:/test/welcome" t)
;;; (write-region 1 100 "/imap:yourhosthere.com:/test/welcome")
;;; (tramp-imap-get-file "/imap:yourhosthere.com:/test/welcome" t)
;;(with-temp-buffer (insert "hello") (write-file "/imap:yourhosthere.com:/test/welcome"))
;;(with-temp-buffer (insert "hello") (write-file "/imap:yourhosthere.com:/test/welcome2"))
;;(file-writable-p "/imap:yourhosthere.com:/test/welcome2")
;;(file-name-directory "/imap:yourhosthere.com:/test/welcome2")
;;(with-temp-buffer (insert "hello") (delete-file "/tmp/hellotest") (write-file "/tmp/hellotest") (write-file "/imap:yourhosthere.com:/test/welcome2"))
;;;(file-exists-p "/imap:yourhosthere.com:/INBOX.test/4")
;;;(file-attributes "/imap:yourhosthere.com:/INBOX.test/4")
;;;(setq vec (tramp-dissect-file-name "/imap:yourhosthere.com:/INBOX.test/4"))
;;;(tramp-imap-handle-file-attributes "/imap:yourhosthere.com:/INBOX.test/4")
;;; (tramp-imap-handle-insert-file-contents "/imap:user@yourhosthere.com:/INBOX.test/4" nil nil nil nil)
;;;(insert-file-contents "/imap:yourhosthere.com:/INBOX.test/4")
;;;(file-attributes "/imap:yourhosthere.com:/test/welcommen")
;;;(insert-file-contents "/imap:yourhosthere.com:/test/welcome")
;;;(file-exists-p "/imap:yourhosthere.com:/test/welcome2")
;;;(tramp-imap-handle-file-attributes "/imap:yourhosthere.com:/test/welcome")
;;;(tramp-imap-get-file-inode "/imap:yourhosthere.com:/test/welcommen")
;;;(tramp-imap-get-file-inode "/imap:yourhosthere.com:/test/welcome")
;;;(file-writable-p "/imap:yourhosthere.com:/test/welcome2")
;;; (delete-file "/imap:yourhosthere.com:/test/welcome")
;;; (tramp-imap-get-file "/imap:yourhosthere.com:/test/welcommen" t)
;;; (tramp-imap-get-file "/imap:yourhosthere.com:/test/welcome" t)
;;;(tramp-imap-file-name-mailbox (tramp-dissect-file-name "/imap:yourhosthere.com:/INBOX.test"))
;;;(tramp-imap-file-name-mailbox (tramp-dissect-file-name "/imap:yourhosthere.com:/INBOX.test/new/old"))
;;;(tramp-imap-file-name-mailbox (tramp-dissect-file-name "/imap:yourhosthere.com:/INBOX.test/new"))
;;;(tramp-imap-file-name-parse (tramp-dissect-file-name "/imap:yourhosthere.com:/INBOX.test/new/two"))
;;;(tramp-imap-file-name-parse (tramp-dissect-file-name "/imap:yourhosthere.com:/INBOX.test/new/one"))
;;;(tramp-imap-file-name-parse (tramp-dissect-file-name "/imap:yourhosthere.com:/INBOX.test"))
;;; (tramp-imap-file-name-parse (tramp-dissect-file-name "/imap:yourhosthere.com:/test/4"))
;;; (tramp-imap-file-name-parse (tramp-dissect-file-name "/imap:yourhosthere.com:/test/"))
;;; (tramp-imap-file-name-parse (tramp-dissect-file-name "/imap:yourhosthere.com:/test/welcommen"))
;;; (tramp-imap-file-name-parse (tramp-dissect-file-name "/imap:yourhosthere.com:/test/welcommen"))
;;; (tramp-imap-make-iht (tramp-dissect-file-name "/imap:yourhosthere.com:/test/welcommen"))
;;; (tramp-imap-make-iht (tramp-dissect-file-name "/imap:yourhosthere.com:/INBOX.test/4"))
;;; (tramp-imap-make-iht (tramp-dissect-file-name "/imap:yourhosthere.com:/INBOX.test/4") "extra")

;; arch-tag: f2723749-58fb-4f29-894e-39708096e850
