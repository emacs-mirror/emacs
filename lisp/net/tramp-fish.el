;;; tramp-fish.el --- Tramp access functions for FISH protocol

;; Copyright (C) 2006, 2007, 2008 Free Software Foundation, Inc.

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

;; Access functions for FIles transferred over SHell protocol from Tramp.

;; FISH is a protocol developped for the GNU Midnight Commander
;; <https://savannah.gnu.org/projects/mc>.  A client connects to a
;; remote host via ssh (or rsh, shall be configurable), and starts
;; there a fish server via the command "start_fish_server".  All
;; commands from the client have the form "#FISH_COMMAND\n" (always
;; one line), followed by equivalent shell commands in case there is
;; no fish server running.

;; The fish server (or the equivalent shell commands) must return the
;; response, which is finished by a line "### xxx  <optional text>\n".
;; "xxx" stands for 3 digits, representing a return code.  Return
;; codes "# 000" and "# 001" are reserved for fallback implementation
;; with native shell commands; they are not used inside the server.  See
;; <http://cvs.savannah.gnu.org/viewcvs/mc/vfs/README.fish?root=mc&view=markup>
;; for details of original specification.

;; The GNU Midnight Commander implements the original fish protocol
;; version 0.0.2.  The KDE Konqueror has its own implementation, which
;; can be found at
;; <http://websvn.kde.org/branches/KDE/3.5/kdebase/kioslave/fish>.  It
;; implements an extended protocol version 0.0.3.  Additionally, it
;; provides a fish server implementation in Perl (which is the only
;; implementation I've heard of).  The following command reference is
;; based on that implementation.

;; All commands return either "### 2xx\n" (OK) or "### 5xx <optional text>\n"
;; (NOK).  Return codes are mentioned only if they are different from this.
;; Spaces in any parameter must be escaped by "\ ".

;; Command/Return Code			Comment
;;
;; #FISH				initial connection, not used
;;					in .fishsrv.pl
;; ### 100 transfer fish server		missing server, or wrong checksum
;;					version 0.0.3 only

;; #VER a.b.c <commands requested>
;; VER x.y.z <commands offered>		.fishsrv.pl response is not uptodate

;; #PWD
;; /path/to/file

;; #CWD /some/path

;; #COPY /path/a /path/b		version 0.0.3 only

;; #RENAME /path/a /path/b

;; #SYMLINK /path/a /path/b

;; #LINK /path/a /path/b

;; #DELE /some/path

;; #MKD /some/path

;; #RMD /some/path

;; #CHOWN user /file/name

;; #CHGRP group /file/name

;; #CHMOD 1234 file

;; #READ <offset> <size> /path/and/filename
;; ### 291				successful exit when reading
;;					ended at eof
;; ### 292				successful exit when reading
;;					did not end at eof

;; #WRITE <offset> <size> /path/and/filename

;; #APPEND <size> /path/and/filename	version 0.0.3 only

;; #LIST /directory
;; <number of entries>			version 0.0.3 only
;; ### 100				version 0.0.3 only
;; P<unix permissions> <owner>.<group>
;; S<size>
;; d<3-letters month name> <day> <year or HH:MM>
;; D<year> <month> <day> <hour> <minute> <second>[.1234]
;; E<major-of-device>,<minor>
;; :<filename>
;; L<filename symlink points to>
;; M<mimetype>				version 0.0.3 only
;; <blank line to separate items>

;; #STAT /file				version 0.0.3 only
;;					like #LIST except for directories
;; <number of entries>
;; ### 100
;; P<unix permissions> <owner>.<group>
;; S<size>
;; d<3-letters month name> <day> <year or HH:MM>
;; D<year> <month> <day> <hour> <minute> <second>[.1234]
;; E<major-of-device>,<minor>
;; :<filename>
;; L<filename symlink points to>
;; <blank line to separate items>

;; #RETR /some/name
;; <filesize>
;; ### 100
;; <binary data>			exactly filesize bytes
;; ### 200				with no preceding newline

;; #STOR <size> /file/name
;; ### 100
;; <data>				exactly size bytes
;; ### 001				partial success

;; #EXEC <command> <tmpfile>		version 0.0.3 only
;; <tmpfile> must not exists.  It contains the output of <command>.
;; It can be retrieved afterwards.  Last line is
;; ###RESULT: <returncode>

;; This implementation is meant as proof of the concept, whether there
;; is a better performance compared with the native ssh method.  It
;; looks like the file information retrieval is slower, especially the
;; #LIST command.  On the other hand, the file contents transmission
;; seems to perform better than other inline methods, because there is
;; no need for data encoding/decoding, and it supports the APPEND
;; parameter of `write-region'.  Transfer of binary data fails due to
;; Emacs' process input/output handling.


;;; Code:

(require 'tramp)
(require 'tramp-cache)
(require 'tramp-compat)

;; Define FISH method ...
(defcustom tramp-fish-method "fish"
  "*Method to connect via FISH protocol."
  :group 'tramp
  :type 'string)

;; ... and add it to the method list.
(add-to-list 'tramp-methods (cons tramp-fish-method nil))

;; Add a default for `tramp-default-user-alist'. Default is the local user.
(add-to-list 'tramp-default-user-alist
	     `(,tramp-fish-method nil ,(user-login-name)))

;; Add completion function for FISH method.
(tramp-set-completion-function
 tramp-fish-method tramp-completion-function-alist-ssh)

(defconst tramp-fish-continue-prompt-regexp "^### 100.*\n"
  "FISH return code OK.")

;; It cannot be a defconst, occasionally we bind it locally.
(defvar tramp-fish-ok-prompt-regexp "^### 200\n"
  "FISH return code OK.")

(defconst tramp-fish-error-prompt-regexp "^### \\(4\\|5\\)[0-9]+.*\n"
  "Regexp for possible error strings of FISH servers.
Used instead of analyzing error codes of commands.")

(defcustom tramp-fish-start-fish-server-command
  (concat "stty intr \"\" quit \"\" erase \"\" kill \"\" eof \"\" eol \"\" eol2 \"\" swtch \"\" start \"\" stop \"\" susp \"\" rprnt \"\" werase \"\" lnext \"\" flush \"\"; "
	  "perl .fishsrv.pl "
	    "`grep 'ARGV\\[0\\]' .fishsrv.pl | "
	    "sed -e 's/^[^\"]*\"//' -e 's/\"[^\"]*$//'`; "
	  "exit")
  "*Command to connect via FISH protocol."
  :group 'tramp
  :type 'string)

;; New handlers should be added here.
(defconst tramp-fish-file-name-handler-alist
  '(
    ;; `access-file' performed by default handler
    (add-name-to-file . tramp-fish-handle-add-name-to-file)
    ;; `byte-compiler-base-file-name' performed by default handler
    (copy-file . tramp-fish-handle-copy-file)
    (delete-directory . tramp-fish-handle-delete-directory)
    (delete-file . tramp-fish-handle-delete-file)
    ;; `diff-latest-backup-file' performed by default handler
    (directory-file-name . tramp-handle-directory-file-name)
    (directory-files . tramp-handle-directory-files)
    (directory-files-and-attributes . tramp-fish-handle-directory-files-and-attributes)
    ;; `dired-call-process' performed by default handler
    ;; `dired-compress-file' performed by default handler
    ;; `dired-uncache' performed by default handler
    (expand-file-name . tramp-fish-handle-expand-file-name)
    ;; `file-accessible-directory-p' performed by default handler
    (file-attributes . tramp-fish-handle-file-attributes)
    (file-directory-p .  tramp-fish-handle-file-directory-p)
    (file-executable-p . tramp-fish-handle-file-executable-p)
    (file-exists-p . tramp-fish-handle-file-exists-p)
    (file-local-copy . tramp-fish-handle-file-local-copy)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions . tramp-fish-handle-file-name-all-completions)
    ;; `file-name-as-directory' performed by default handler
    (file-name-completion . tramp-handle-file-name-completion)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    ;; `file-name-sans-versions' performed by default handler
    (file-newer-than-file-p . tramp-fish-handle-file-newer-than-file-p)
    (file-ownership-preserved-p . ignore)
    (file-readable-p . tramp-fish-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-symlink-p . tramp-handle-file-symlink-p)
    ;; `file-truename' performed by default handler
    (file-writable-p . tramp-fish-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    ;; `find-file-noselect' performed by default handler
    ;; `get-file-buffer' performed by default handler
    (insert-directory . tramp-fish-handle-insert-directory)
    (insert-file-contents . tramp-fish-handle-insert-file-contents)
    (load . tramp-handle-load)
    (make-directory . tramp-fish-handle-make-directory)
    (make-directory-internal . tramp-fish-handle-make-directory-internal)
    (make-symbolic-link . tramp-fish-handle-make-symbolic-link)
    (rename-file . tramp-fish-handle-rename-file)
    (set-file-modes . tramp-fish-handle-set-file-modes)
    (set-file-times . tramp-fish-handle-set-file-times)
    (set-visited-file-modtime . ignore)
    (shell-command . tramp-handle-shell-command)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (unhandled-file-name-directory . tramp-handle-unhandled-file-name-directory)
    (vc-registered . ignore)
    (verify-visited-file-modtime . ignore)
    (write-region . tramp-fish-handle-write-region)
    (executable-find . tramp-fish-handle-executable-find)
    (start-file-process . ignore)
    (process-file . tramp-fish-handle-process-file)
)
  "Alist of handler functions for Tramp FISH method.
Operations not mentioned here will be handled by the default Emacs primitives.")

(defun tramp-fish-file-name-p (filename)
  "Check if it's a filename for FISH protocol."
  (let ((v (tramp-dissect-file-name filename)))
    (string= (tramp-file-name-method v) tramp-fish-method)))

(defun tramp-fish-file-name-handler (operation &rest args)
  "Invoke the FISH related OPERATION.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION."
  (let ((fn (assoc operation tramp-fish-file-name-handler-alist)))
    (if fn
	(save-match-data (apply (cdr fn) args))
      (tramp-run-real-handler operation args))))

(add-to-list 'tramp-foreign-file-name-handler-alist
	     (cons 'tramp-fish-file-name-p 'tramp-fish-file-name-handler))


;; File name primitives

(defun tramp-fish-handle-add-name-to-file
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
      (when (and (not ok-if-already-exists)
		 (file-exists-p newname)
		 (not (numberp ok-if-already-exists))
		 (y-or-n-p
		  (format
		   "File %s already exists; make it a new name anyway? "
		   newname)))
	(tramp-error
	 v2 'file-error
	 "add-name-to-file: file %s already exists" newname))
      (tramp-flush-file-property v2 v2-localname)
      (unless (tramp-fish-send-command-and-check
	       v1 (format "#LINK %s %s" v1-localname v2-localname))
	(tramp-error
	 v1 'file-error "Error with add-name-to-file %s" newname)))))

(defun tramp-fish-handle-copy-file
  (filename newname &optional ok-if-already-exists keep-date preserve-uid-gid)
  "Like `copy-file' for Tramp files."
  (tramp-fish-do-copy-or-rename-file
   'copy filename newname ok-if-already-exists keep-date preserve-uid-gid))

(defun tramp-fish-handle-delete-directory (directory)
  "Like `delete-directory' for Tramp files."
  (when (file-exists-p directory)
    (with-parsed-tramp-file-name
	(directory-file-name (expand-file-name directory)) nil
      (tramp-flush-directory-property v localname)
      (tramp-fish-send-command-and-check v (format "#RMD %s" localname)))))

(defun tramp-fish-handle-delete-file (filename)
  "Like `delete-file' for Tramp files."
  (when (file-exists-p filename)
    (with-parsed-tramp-file-name (expand-file-name filename) nil
      (tramp-flush-file-property v localname)
      (tramp-fish-send-command-and-check v (format "#DELE %s" localname)))))

(defun tramp-fish-handle-directory-files-and-attributes
  (directory &optional full match nosort id-format)
  "Like `directory-files-and-attributes' for Tramp files."
  (mapcar
   (lambda (x)
     ;; We cannot call `file-attributes' for backward compatibility reasons.
     ;; Its optional parameter ID-FORMAT is introduced with Emacs 22.
     (cons x (tramp-fish-handle-file-attributes
	(if full x (expand-file-name x directory)) id-format)))
   (directory-files directory full match nosort)))

(defun tramp-fish-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for Tramp files."
  ;; If DIR is not given, use DEFAULT-DIRECTORY or "/".
  (setq dir (or dir default-directory "/"))
  ;; Unless NAME is absolute, concat DIR and NAME.
  (unless (file-name-absolute-p name)
    (setq name (concat (file-name-as-directory dir) name)))
  ;; If NAME is not a Tramp file, run the real handler,
  (if (or (tramp-completion-mode-p) (not (tramp-tramp-file-p name)))
      (tramp-drop-volume-letter
       (tramp-run-real-handler 'expand-file-name (list name nil)))
    ;; Dissect NAME.
    (with-parsed-tramp-file-name name nil
      (unless (tramp-run-real-handler 'file-name-absolute-p (list localname))
	(setq localname (concat "~/" localname)))
      ;; Tilde expansion if necessary.
      (when (string-match "\\`\\(~[^/]*\\)\\(.*\\)\\'" localname)
	(let ((uname (match-string 1 localname))
	      (fname (match-string 2 localname)))
	  ;; We cannot apply "~user/", because this is not supported
	  ;; by the FISH protocol.
	  (unless (string-equal uname "~")
	    (tramp-error
	     v 'file-error "Tilde expansion not supported for %s" name))
	  (setq uname
	    (with-connection-property v uname
	      (tramp-fish-send-command-and-check v "#PWD")
	      (with-current-buffer (tramp-get-buffer v)
		(goto-char (point-min))
		(buffer-substring (point) (tramp-compat-line-end-position)))))
	  (setq localname (concat uname fname))))
      ;; There might be a double slash, for example when "~/"
      ;; expands to "/". Remove this.
      (while (string-match "//" localname)
	(setq localname (replace-match "/" t t localname)))
      ;; No tilde characters in file name, do normal
      ;; expand-file-name (this does "/./" and "/../").  We bind
      ;; `directory-sep-char' here for XEmacs on Windows, which
      ;; would otherwise use backslash.  `default-directory' is
      ;; bound, because on Windows there would be problems with UNC
      ;; shares or Cygwin mounts.
      (let ((directory-sep-char ?/)
	    (default-directory (tramp-compat-temporary-file-directory)))
	(tramp-make-tramp-file-name
	 method user host
	 (tramp-drop-volume-letter
	  (tramp-run-real-handler
	   'expand-file-name (list localname))))))))

(defun tramp-fish-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-file-property v localname (format "file-attributes-%s" id-format)
      (cdr (car (tramp-fish-get-file-entries v localname nil))))))

(defun tramp-fish-handle-file-directory-p (filename)
  "Like `file-directory-p' for Tramp files."
  (let ((attributes (file-attributes filename)))
    (and attributes
	 (or (string-match "d" (nth 8 attributes))
	     (and (file-symlink-p filename)
		  (with-parsed-tramp-file-name filename nil
		    (file-directory-p
		     (tramp-make-tramp-file-name
		      method user host (nth 0 attributes))))))
	 t)))

(defun tramp-fish-handle-file-exists-p (filename)
  "Like `file-exists-p' for Tramp files."
  (and (file-attributes filename) t))

(defun tramp-fish-handle-file-executable-p (filename)
  "Like `file-executable-p' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-file-property v localname "file-executable-p"
      (when (file-exists-p filename)
	(let ((mode-chars (string-to-vector (nth 8 (file-attributes filename))))
	      (home-directory
	       (tramp-make-tramp-file-name
		method user host
		(tramp-get-connection-property v "home-directory" nil))))
	  (or (and (char-equal (aref mode-chars 3) ?x)
		   (equal (nth 2 (file-attributes filename))
			  (nth 2 (file-attributes home-directory))))
	      (and (char-equal (aref mode-chars 6) ?x)
		   (equal (nth 3 (file-attributes filename))
			  (nth 3 (file-attributes home-directory))))
	      (char-equal (aref mode-chars 9) ?x)))))))

(defun tramp-fish-handle-file-readable-p (filename)
  "Like `file-readable-p' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-file-property v localname "file-readable-p"
      (when (file-exists-p filename)
	(let ((mode-chars (string-to-vector (nth 8 (file-attributes filename))))
	      (home-directory
	       (tramp-make-tramp-file-name
		method user host
		(tramp-get-connection-property v "home-directory" nil))))
	  (or (and (char-equal (aref mode-chars 1) ?r)
		   (equal (nth 2 (file-attributes filename))
			  (nth 2 (file-attributes home-directory))))
	      (and (char-equal (aref mode-chars 4) ?r)
		   (equal (nth 3 (file-attributes filename))
			  (nth 3 (file-attributes home-directory))))
	      (char-equal (aref mode-chars 7) ?r)))))))

(defun tramp-fish-handle-file-writable-p (filename)
  "Like `file-writable-p' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-file-property v localname "file-writable-p"
      (if (not (file-exists-p filename))
	  ;; If file doesn't exist, check if directory is writable.
	  (and (file-directory-p (file-name-directory filename))
	       (file-writable-p (file-name-directory filename)))
	;; Existing files must be writable.
	(let ((mode-chars (string-to-vector (nth 8 (file-attributes filename))))
	      (home-directory
	       (tramp-make-tramp-file-name
		method user host
		(tramp-get-connection-property v "home-directory" nil))))
	  (or (and (char-equal (aref mode-chars 2) ?w)
		   (equal (nth 2 (file-attributes filename))
			  (nth 2 (file-attributes home-directory))))
	      (and (char-equal (aref mode-chars 5) ?w)
		   (equal (nth 3 (file-attributes filename))
			  (nth 3 (file-attributes home-directory))))
	      (char-equal (aref mode-chars 8) ?w)))))))

(defun tramp-fish-handle-file-local-copy (filename)
  "Like `file-local-copy' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (unless (file-exists-p filename)
      (tramp-error
       v 'file-error
       "Cannot make local copy of non-existing file `%s'" filename))
    (let ((tmpfile (tramp-compat-make-temp-file filename)))
      (tramp-message v 4 "Fetching %s to tmp file %s..." filename tmpfile)
      (when (tramp-fish-retrieve-data v)
	;; Save file
	(with-current-buffer (tramp-get-buffer v)
	  (write-region (point-min) (point-max) tmpfile))
	(tramp-message v 4 "Fetching %s to tmp file %s...done" filename tmpfile)
	tmpfile))))

;; This function should return "foo/" for directories and "bar" for
;; files.
(defun tramp-fish-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for Tramp files."
  (all-completions
   filename
   (with-parsed-tramp-file-name (expand-file-name directory) nil
     (with-file-property v localname "file-name-all-completions"
       (save-match-data
	 (let ((entries
		(with-file-property v localname "file-entries"
		  (tramp-fish-get-file-entries v localname t))))
	   (mapcar
	    (lambda (x)
	      (list
	       (if (string-match "d" (nth 9 x))
		   (file-name-as-directory (nth 0 x))
		 (nth 0 x))))
	    entries)))))))

(defun tramp-fish-handle-file-newer-than-file-p (file1 file2)
  "Like `file-newer-than-file-p' for Tramp files."
  (cond
   ((not (file-exists-p file1)) nil)
   ((not (file-exists-p file2)) t)
   (t (tramp-time-less-p (nth 5 (file-attributes file2))
			 (nth 5 (file-attributes file1))))))

(defun tramp-fish-handle-insert-directory
  (filename switches &optional wildcard full-directory-p)
  "Like `insert-directory' for Tramp files.
WILDCARD and FULL-DIRECTORY-P are not handled."
  (setq filename (expand-file-name filename))
  (when (file-directory-p filename)
    ;; This check is a little bit strange, but in `dired-add-entry'
    ;; this function is called with a non-directory ...
    (setq filename (file-name-as-directory filename)))

  (with-parsed-tramp-file-name filename nil
    (tramp-flush-file-property v localname)
    (save-match-data
      (let ((entries
	     (with-file-property v localname "file-entries"
	       (tramp-fish-get-file-entries v localname t))))

	;; Sort entries
	(setq entries
	      (sort
	       entries
	       (lambda (x y)
		 (if (string-match "t" switches)
		     ;; Sort by date.
		     (tramp-time-less-p (nth 6 y) (nth 6 x))
		   ;; Sort by name.
		   (string-lessp (nth 0 x) (nth 0 y))))))

	;; Print entries.
	(mapcar
	 (lambda (x)
	   (insert
	    (format
	     "%10s %3d %-8s %-8s %8s %s %s%s\n"
	     (nth 9 x) ; mode
	     1         ; hardlinks
	     (nth 3 x) ; uid
	     (nth 4 x) ; gid
	     (nth 8 x) ; size
	     (format-time-string
	      (if (tramp-time-less-p
		   (tramp-time-subtract (current-time) (nth 6 x))
		   tramp-half-a-year)
		  "%b %e %R"
		"%b %e  %Y")
	      (nth 6 x)) ; date
	     (nth 0 x) ; file name
	     (if (stringp (nth 1 x)) (format " -> %s" (nth 1 x)) "")))
	   (forward-line)
	   (beginning-of-line))
	 entries)))))

(defun tramp-fish-handle-insert-file-contents
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
	 v 'file-error "File %s not found on remote host" filename)

      (let ((point (point))
	    size)
	(tramp-message v 4 "Fetching file %s..." filename)
	(when (tramp-fish-retrieve-data v)
	  ;; Insert file
	  (insert
	   (with-current-buffer (tramp-get-buffer v)
	     (let ((beg (or beg (point-min)))
		   (end (min (or end (point-max)) (point-max))))
	       (setq size (- end beg))
	       (buffer-substring beg end))))
	  (goto-char point))
	(tramp-message v 4 "Fetching file %s...done" filename)

	(list (expand-file-name filename) size)))))

(defun tramp-fish-handle-make-directory (dir &optional parents)
  "Like `make-directory' for Tramp files."
  (setq dir (directory-file-name (expand-file-name dir)))
  (unless (file-name-absolute-p dir)
    (setq dir (expand-file-name dir default-directory)))
  (with-parsed-tramp-file-name dir nil
    (save-match-data
      (let ((ldir (file-name-directory dir)))
	;; Make missing directory parts
	(when (and parents (not (file-directory-p ldir)))
	  (make-directory ldir parents))
	;; Just do it
	(when (file-directory-p ldir)
	  (make-directory-internal dir))
	(unless (file-directory-p dir)
	  (tramp-error v 'file-error "Couldn't make directory %s" dir))))))

(defun tramp-fish-handle-make-directory-internal (directory)
  "Like `make-directory-internal' for Tramp files."
  (setq directory (directory-file-name (expand-file-name directory)))
  (unless (file-name-absolute-p directory)
    (setq directory (expand-file-name directory default-directory)))
  (when (file-directory-p (file-name-directory directory))
    (with-parsed-tramp-file-name directory nil
      (save-match-data
	(unless
	    (tramp-fish-send-command-and-check v (format "#MKD %s" localname))
	  (tramp-error
	   v 'file-error "Couldn't make directory %s" directory))))))

(defun tramp-fish-handle-make-symbolic-link
  (filename linkname &optional ok-if-already-exists)
  "Like `make-symbolic-link' for Tramp files.
If LINKNAME is a non-Tramp file, it is used verbatim as the target of
the symlink.  If LINKNAME is a Tramp file, only the localname component is
used as the target of the symlink.

If LINKNAME is a Tramp file and the localname component is relative, then
it is expanded first, before the localname component is taken.  Note that
this can give surprising results if the user/host for the source and
target of the symlink differ."
  (with-parsed-tramp-file-name linkname nil
    ;; Do the 'confirm if exists' thing.
    (when (file-exists-p linkname)
      ;; What to do?
      (if (or (null ok-if-already-exists) ; not allowed to exist
	      (and (numberp ok-if-already-exists)
		   (not (yes-or-no-p
			 (format
			  "File %s already exists; make it a link anyway? "
			  localname)))))
	  (tramp-error
	   v 'file-already-exists "File %s already exists" localname)
	(delete-file linkname)))

    ;; If FILENAME is a Tramp name, use just the localname component.
    (when (tramp-tramp-file-p filename)
      (setq filename (tramp-file-name-localname
		      (tramp-dissect-file-name (expand-file-name filename)))))

    ;; Right, they are on the same host, regardless of user, method, etc.
    ;; We now make the link on the remote machine. This will occur as the user
    ;; that FILENAME belongs to.
    (unless
	(tramp-fish-send-command-and-check
	 v (format "#SYMLINK %s %s" filename localname))
      (tramp-error v 'file-error "Error creating symbolic link %s" linkname))))

(defun tramp-fish-handle-rename-file
  (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for Tramp files."
  (tramp-fish-do-copy-or-rename-file
   'rename filename newname ok-if-already-exists t))

(defun tramp-fish-handle-set-file-modes (filename mode)
  "Like `set-file-modes' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (tramp-flush-file-property v localname)
    (unless (tramp-fish-send-command-and-check
	     v (format "#CHMOD %s %s"
		       (tramp-decimal-to-octal mode)
		       (tramp-shell-quote-argument localname)))
      (tramp-error
       v 'file-error "Error while changing file's mode %s" filename))))

(defun tramp-fish-handle-set-file-times (filename &optional time)
  "Like `set-file-times' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (let ((time (if (or (null time) (equal time '(0 0))) (current-time) time)))
      (zerop (process-file
	      "touch" nil nil nil "-t"
	      (format-time-string "%Y%m%d%H%M.%S" time)
	      (tramp-shell-quote-argument localname))))))

(defun tramp-fish-handle-write-region
  (start end filename &optional append visit lockname confirm)
  "Like `write-region' for Tramp files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    ;; XEmacs takes a coding system as the seventh argument, not `confirm'
    (when (and (not (featurep 'xemacs))
	       confirm (file-exists-p filename))
      (unless (y-or-n-p (format "File %s exists; overwrite anyway? "
				filename))
	(tramp-error v 'file-error "File not overwritten")))

    (tramp-flush-file-property v localname)

    ;; Send command
    (let ((tramp-fish-ok-prompt-regexp
	   (concat
	    tramp-fish-ok-prompt-regexp "\\|"
	    tramp-fish-continue-prompt-regexp)))
      (tramp-fish-send-command
       v (format "%s %d %s\n### 100"
		 (if append "#APPEND" "#STOR") (- end start) localname)))

    ;; Send data, if there are any.
    (when (> end start)
      (tramp-fish-send-command v (buffer-substring-no-properties start end)))

    (when (eq visit t)
      (set-visited-file-modtime))))

(defun tramp-fish-handle-executable-find (command)
  "Like `executable-find' for Tramp files."
  (with-temp-buffer
    (if (zerop (process-file "which" nil t nil command))
	(progn
	  (goto-char (point-min))
	  (buffer-substring (point-min) (tramp-compat-line-end-position))))))

(defun tramp-fish-handle-process-file
  (program &optional infile destination display &rest args)
  "Like `process-file' for Tramp files."
  ;; The implementation is not complete yet.
  (when (and (numberp destination) (zerop destination))
    (error "Implementation does not handle immediate return"))

  (with-parsed-tramp-file-name default-directory nil
    (let (command input tmpinput output tmpoutput stderr tmpstderr
		  outbuf tmpfile ret)
      ;; Compute command.
      (setq command (mapconcat 'tramp-shell-quote-argument
			       (cons program args) " "))
      ;; Determine input.
      (if (null infile)
	  (setq input "/dev/null")
	(setq infile (expand-file-name infile))
	(if (tramp-equal-remote default-directory infile)
	    ;; INFILE is on the same remote host.
	    (setq input (with-parsed-tramp-file-name infile nil localname))
	  ;; INFILE must be copied to remote host.
	  (setq input (tramp-make-tramp-temp-file v)
		tmpinput (tramp-make-tramp-file-name method user host input))
	  (copy-file infile tmpinput t)))
      (when input (setq command (format "%s <%s" command input)))

      ;; Determine output.
      (setq output (tramp-make-tramp-temp-file v)
	    tmpoutput (tramp-make-tramp-file-name method user host output))
      (cond
       ;; Just a buffer
       ((bufferp destination)
	(setq outbuf destination))
       ;; A buffer name
       ((stringp destination)
	(setq outbuf (get-buffer-create destination)))
       ;; (REAL-DESTINATION ERROR-DESTINATION)
       ((consp destination)
	;; output
	(cond
	 ((bufferp (car destination))
	  (setq outbuf (car destination)))
	 ((stringp (car destination))
	  (setq outbuf (get-buffer-create (car destination)))))
	;; stderr
	(cond
	 ((stringp (cadr destination))
	  (setcar (cdr destination) (expand-file-name (cadr destination)))
	  (if (tramp-equal-remote default-directory (cadr destination))
	      ;; stderr is on the same remote host.
	      (setq stderr (with-parsed-tramp-file-name
			       (cadr destination) nil localname))
	    ;; stderr must be copied to remote host.  The temporary
	    ;; file must be deleted after execution.
	    (setq stderr (tramp-make-tramp-temp-file v)
		  tmpstderr (tramp-make-tramp-file-name
			     method user host stderr))))
	 ;; stderr to be discarded
	 ((null (cadr destination))
	  (setq stderr "/dev/null"))))
       ;; 't
       (destination
	(setq outbuf (current-buffer))))
      (when stderr (setq command (format "%s 2>%s" command stderr)))

      ;; Goto working directory.
      (unless
	  (tramp-fish-send-command-and-check
	   v (format "#CWD %s" (tramp-shell-quote-argument localname)))
	(tramp-error v 'file-error "No such directory: %s" default-directory))
      ;; Send the command.  It might not return in time, so we protect it.
      (condition-case nil
	  (unwind-protect
	      (unless (tramp-fish-send-command-and-check
		       v (format
			  "#EXEC %s %s"
			  (tramp-shell-quote-argument command) output))
		(error nil))
	    ;; Check return code.
	    (setq tmpfile
		  (file-local-copy
		   (tramp-make-tramp-file-name method user host output)))
	    (with-temp-buffer
	      (insert-file-contents tmpfile)
	      (goto-char (point-max))
	      (forward-line -1)
	      (looking-at "^###RESULT: \\([0-9]+\\)")
	      (setq ret (string-to-number (match-string 1)))
	      (delete-region (point) (point-max))
	      (write-region (point-min) (point-max) tmpfile))
	    ;; We should show the output anyway.
	    (when outbuf
	      (with-current-buffer outbuf (insert-file-contents tmpfile))
	      (when display (display-buffer outbuf))))
	;; When the user did interrupt, we should do it also.
	(error (setq ret 1)))

      ;; Provide error file.
      (when tmpstderr (rename-file tmpstderr (cadr destination) t))
      ;; Cleanup.
      (when tmpinput (delete-file tmpinput))
      (when tmpoutput (delete-file tmpoutput))
      ;; Return exit status.
      ret)))


;; Internal file name functions

(defun tramp-fish-do-copy-or-rename-file
  (op filename newname &optional ok-if-already-exists keep-date preserve-uid-gid)
  "Copy or rename a remote file.
OP must be `copy' or `rename' and indicates the operation to
perform.  FILENAME specifies the file to copy or rename, NEWNAME
is the name of the new file (for copy) or the new name of the
file (for rename).  OK-IF-ALREADY-EXISTS means don't barf if
NEWNAME exists already.  KEEP-DATE means to make sure that
NEWNAME has the same timestamp as FILENAME.

This function is invoked by `tramp-fish-handle-copy-file' and
`tramp-fish-handle-rename-file'.  It is an error if OP is neither
of `copy' and `rename'.  FILENAME and NEWNAME must be absolute
file names."
  (unless (memq op '(copy rename))
    (error "Unknown operation `%s', must be `copy' or `rename'" op))
  (let ((t1 (tramp-tramp-file-p filename))
	(t2 (tramp-tramp-file-p newname)))

    (unless ok-if-already-exists
      (when (and t2 (file-exists-p newname))
	(with-parsed-tramp-file-name newname nil
	  (tramp-error
	   v 'file-already-exists "File %s already exists" newname))))

    (prog1
	(cond
	 ;; Both are Tramp files.
	 ((and t1 t2)
	  (cond
	   ;; Shortcut: if method, host, user are the same for both
	   ;; files, we invoke `cp' or `mv' on the remote host
	   ;; directly.
	   ((tramp-equal-remote filename newname)
	    (tramp-fish-do-copy-or-rename-file-directly
	     op filename newname keep-date preserve-uid-gid))
	   ;; No shortcut was possible.  So we copy the
	   ;; file first.  If the operation was `rename', we go
	   ;; back and delete the original file (if the copy was
	   ;; successful).  The approach is simple-minded: we
	   ;; create a new buffer, insert the contents of the
	   ;; source file into it, then write out the buffer to
	   ;; the target file.  The advantage is that it doesn't
	   ;; matter which filename handlers are used for the
	   ;; source and target file.
	   (t
	    (tramp-do-copy-or-rename-file-via-buffer
	     op filename newname keep-date))))

	 ;; One file is a Tramp file, the other one is local.
	 ((or t1 t2)
	  ;; Use the generic method via a Tramp buffer.
	  (tramp-do-copy-or-rename-file-via-buffer
	   op filename newname keep-date))

	 (t
	  ;; One of them must be a Tramp file.
	  (error "Tramp implementation says this cannot happen")))
      ;; When newname did exist, we have wrong cached values.
      (when t2
	(with-parsed-tramp-file-name newname nil
	  (tramp-flush-file-property v localname)
	  (tramp-flush-file-property v (file-name-directory localname)))))))

(defun tramp-fish-do-copy-or-rename-file-directly
  (op filename newname keep-date preserve-uid-gid)
  "Invokes `COPY' or `RENAME' on the remote system.
OP must be one of `copy' or `rename', indicating `cp' or `mv',
respectively.  VEC specifies the connection.  LOCALNAME1 and
LOCALNAME2 specify the two arguments of `cp' or `mv'.  If
KEEP-DATE is non-nil, preserve the time stamp when copying.
PRESERVE-UID-GID is completely ignored."
  (with-parsed-tramp-file-name filename v1
    (with-parsed-tramp-file-name newname v2
      (tramp-fish-send-command
       v1
       (format "%s %s %s"
	       (if (eq op 'copy) "#COPY" "#RENAME")
	       (tramp-shell-quote-argument v1-localname)
	       (tramp-shell-quote-argument v2-localname)))))
  ;; KEEP-DATE handling.
  (when (and keep-date (functionp 'set-file-times))
    (set-file-times newname (nth 5 (file-attributes filename))))
  ;; Set the mode.
  (set-file-modes newname (file-modes filename)))

(defun tramp-fish-get-file-entries (vec localname list)
  "Read entries returned by FISH server.
When LIST is true, a #LIST command will be sent, including all entries
of a directory.  Otherwise, #STAT is sent for just one entry.
Result is a list of (LOCALNAME LINK COUNT UID GID ATIME MTIME CTIME
SIZE MODE WEIRD INODE DEVICE)."
  (block nil
    (with-current-buffer (tramp-get-buffer vec)
      ;; #LIST does not work properly with trailing "/", at least in
      ;; .fishsrv.pl.
      (when (string-match "/$" localname)
	(setq localname (concat localname ".")))

      (let ((command (format "%s %s" (if list "#LIST" "#STAT") localname))
	    buffer-read-only num res)

	;; Send command
	(tramp-fish-send-command vec command)

	;; Read number of entries
	(goto-char (point-min))
	(condition-case nil
	    (unless (integerp (setq num (read (current-buffer)))) (error nil))
	  (error (return nil)))
	(forward-line)
	(delete-region (point-min) (point))

	;; Read return code
	(goto-char (point-min))
	(condition-case nil
	    (unless (looking-at tramp-fish-continue-prompt-regexp) (error nil))
	  (error (return nil)))
	(forward-line)
	(delete-region (point-min) (point))

	;; Loop the listing
	(dotimes (i num)
	  (let ((item (tramp-fish-read-file-entry)))
	    ;; Add inode and device.
	    (add-to-list
	     'res (append item
			  (list (tramp-get-inode vec)
				(tramp-get-device vec))))))

	;; Read return code
	(goto-char (point-min))
	(condition-case nil
	    (unless (looking-at tramp-fish-ok-prompt-regexp) (error nil))
	  (error (tramp-error
		  vec 'file-error
		  "`%s' does not return a valid Lisp expression: `%s'"
		  command (buffer-string))))
	(forward-line)
	(delete-region (point-min) (point))

	res))))

(defun tramp-fish-read-file-entry ()
  "Parse entry in output buffer.
Result is the list (LOCALNAME LINK COUNT UID GID ATIME MTIME CTIME
SIZE MODE WEIRD)."
  ;; We are called from `tramp-fish-get-file-entries', which sets the
  ;; current buffer.
  (let (buffer-read-only localname link uid gid mtime size mode)
    (block nil
      (while t
	(cond
	 ;; P<unix permissions> <owner>.<group>
	 ((looking-at "^P\\(.+\\)\\s-\\(.+\\)\\.\\(.+\\)$")
	  (setq mode (match-string 1))
	  (setq uid (match-string 2))
	  (setq gid (match-string 3))
	  (when (string-match "^d" mode) (setq link t)))
	 ;; S<size>
	 ((looking-at "^S\\([0-9]+\\)$")
	  (setq size (string-to-number (match-string 1))))
	 ;; D<year> <month> <day> <hour> <minute> <second>[.1234]
	 ((looking-at
	   "^D\\([0-9]+\\)\\s-\\([0-9]+\\)\\s-\\([0-9]+\\)\\s-\\([0-9]+\\)\\s-\\([0-9]+\\)\\s-\\(\\S-+\\)$")
	  (setq mtime
		(encode-time
		 (string-to-number (match-string 6))
		 (string-to-number (match-string 5))
		 (string-to-number (match-string 4))
		 (string-to-number (match-string 3))
		 (string-to-number (match-string 2))
		 (string-to-number (match-string 1)))))
	 ;; d<3-letters month name> <day> <year or HH:MM>
	 ((looking-at "^d") nil)
	 ;; E<major-of-device>,<minor>
	 ((looking-at "^E") nil)
	 ;; :<filename>
	 ((looking-at "^:\\(.+\\)$")
	  (setq localname (match-string 1)))
	 ;; L<filename symlink points to>
	 ((looking-at "^L\\(.+\\)$")
	  (setq link (match-string 1)))
	 ;; M<mimetype>
	 ((looking-at "^M\\(.+\\)$") nil)
	 ;; last line
	 ((looking-at "^$")
	  (return)))
	;; delete line
	(forward-line)
	(delete-region (point-min) (point))))

    ;; delete trailing empty line
    (forward-line)
    (delete-region (point-min) (point))

    ;; Return entry in file-attributes format
    (list localname link -1 uid gid '(0 0) mtime '(0 0) size mode nil)))

(defun tramp-fish-retrieve-data (vec)
  "Reads remote data for FISH protocol.
The data are left in the connection buffer of VEC for further processing.
Returns the size of the data."
  (block nil
    (with-current-buffer (tramp-get-buffer vec)
      ;; The retrieved data might be in binary format, without
      ;; trailing newline.  Therefore, the OK prompt might not start
      ;; at the beginning of a line.
      (let ((tramp-fish-ok-prompt-regexp "### 200\n")
	    size)

	;; Send command
	(tramp-fish-send-command
	 vec (format "#RETR %s" (tramp-file-name-localname vec)))

	;; Read filesize
	(goto-char (point-min))
	(condition-case nil
	    (unless (integerp (setq size (read (current-buffer)))) (error nil))
	  (error (return nil)))
	(forward-line)
	(delete-region (point-min) (point))

	;; Read return code
	(goto-char (point-min))
	(condition-case nil
	    (unless (looking-at tramp-fish-continue-prompt-regexp) (error nil))
	  (error (return nil)))
	(forward-line)
	(delete-region (point-min) (point))

	;; The received data might contain the OK prompt already, so
	;; there might be outstanding data.
	(while (/= (+ size (length tramp-fish-ok-prompt-regexp))
		   (- (point-max) (point-min)))
	  (tramp-wait-for-regexp
	   (tramp-get-connection-process vec) nil
	   (concat tramp-fish-ok-prompt-regexp "$")))

	;; Read return code
	(goto-char (+ (point-min) size))
	(condition-case nil
	    (unless (looking-at tramp-fish-ok-prompt-regexp) (error nil))
	  (error (return nil)))
	(delete-region (+ (point-min) size) (point-max))
	size))))


;; Connection functions

(defun tramp-fish-maybe-open-connection (vec)
  "Maybe open a connection VEC.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
  (let ((process-connection-type tramp-process-connection-type)
	(p (get-buffer-process (tramp-get-buffer vec))))

    ;; New connection must be opened.
    (unless (and p (processp p) (memq (process-status p) '(run open)))

      ;; Set variables for computing the prompt for reading password.
      (setq tramp-current-method (tramp-file-name-method vec)
	    tramp-current-user (tramp-file-name-user vec)
	    tramp-current-host (tramp-file-name-host vec))

      ;; Start new process.
      (when (and p (processp p))
	(delete-process p))
      (setenv "TERM" tramp-terminal-type)
      (setenv "PS1" "$ ")
      (tramp-message
       vec 3 "Opening connection for %s@%s using %s..."
       tramp-current-user tramp-current-host tramp-current-method)

      (let* ((process-connection-type tramp-process-connection-type)
	     (inhibit-eol-conversion nil)
	     (coding-system-for-read 'binary)
	     (coding-system-for-write 'binary)
	     ;; This must be done in order to avoid our file name handler.
	     (p (let ((default-directory
			(tramp-compat-temporary-file-directory)))
		  (start-process
		   (or (tramp-get-connection-property vec "process-name" nil)
		       (tramp-buffer-name vec))
		   (tramp-get-connection-buffer vec)
		   "ssh" "-l"
		   (tramp-file-name-user vec)
		   (tramp-file-name-host vec)))))
	(tramp-message vec 6 "%s" (mapconcat 'identity (process-command p) " "))

	;; Check whether process is alive.
	(tramp-set-process-query-on-exit-flag p nil)

	(tramp-process-actions p vec tramp-actions-before-shell 60)
	(tramp-fish-send-command vec tramp-fish-start-fish-server-command)
	(tramp-message
	 vec 3
	 "Found remote shell prompt on `%s'" (tramp-file-name-host vec))))))

(defun tramp-fish-send-command (vec command)
  "Send the COMMAND to connection VEC."
  (tramp-fish-maybe-open-connection vec)
  (tramp-message vec 6 "%s" command)
  (tramp-send-string vec command)
  (tramp-wait-for-regexp
   (tramp-get-connection-process vec) nil
   (concat tramp-fish-ok-prompt-regexp "\\|" tramp-fish-error-prompt-regexp)))

(defun tramp-fish-send-command-and-check (vec command)
  "Send the COMMAND to connection VEC.
Returns nil if there has been an error message."

  ;; Send command.
  (tramp-fish-send-command vec command)

  ;; Read return code.
  (with-current-buffer (tramp-get-buffer vec)
    (goto-char (point-min))
    (looking-at tramp-fish-ok-prompt-regexp)))

(provide 'tramp-fish)
;
;;;; TODO:
;
;; * Evaluate the MIME information with #LIST or #STAT.
;

;; arch-tag: a66df7df-5f29-42a7-a921-643ceb29db49
;;;; tramp-fish.el ends here
