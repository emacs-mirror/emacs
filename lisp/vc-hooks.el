;;; vc-hooks.el --- resident support for version-control

;; Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.

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

;; Customization Variables (the rest is in vc.el)

(defvar vc-default-back-end nil
  "*Back-end actually used by this interface; may be SCCS or RCS.
The value is only computed when needed to avoid an expensive search.")

(defvar vc-path
  (if (file-directory-p "/usr/sccs")
      '("/usr/sccs")
    nil)
  "*List of extra directories to search for version control commands.")

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

(defvar vc-consult-headers t
  "*Identify work files by searching for version headers.")

(defvar vc-mistrust-permissions nil
  "*Don't assume that permissions and ownership track version-control status.")

(defvar vc-keep-workfiles t
  "*If non-nil, don't delete working files after registering changes.
If the back-end is CVS, workfiles are always kept, regardless of the
value of this flag.")

;; Tell Emacs about this new kind of minor mode
(if (not (assoc 'vc-mode minor-mode-alist))
    (setq minor-mode-alist (cons '(vc-mode vc-mode)
				 minor-mode-alist)))

(make-variable-buffer-local 'vc-mode)
(put 'vc-mode 'permanent-local t)


;; branch identification

(defun vc-occurrences (object sequence)
  ;; return the number of occurences of OBJECT in SEQUENCE
  ;; (is it really true that Emacs Lisp doesn't provide such a function?)
  (let ((len (length sequence)) (index 0) (occ 0))
    (while (< index len)
      (if (eq object (elt sequence index))
	  (setq occ (1+ occ)))
      (setq index (1+ index)))
    occ))

(defun vc-branch-p (rev)
  ;; return t if REV is the branch part of a revision, 
  ;; i.e. a revision without a minor number
  (eq 0 (% (vc-occurrences ?. rev) 2)))

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

(defun vc-file-clearprops (file)
  ;; clear all properties of a given file
  (setplist (intern file vc-file-prop-obarray) nil))

;; basic properties 

(defun vc-name (file)
  "Return the master name of a file, nil if it is not registered."
  (or (vc-file-getprop file 'vc-name)
      (let ((name-and-type (vc-registered file)))
	(if name-and-type
	    (progn
	      (vc-file-setprop file 'vc-backend (cdr name-and-type))
	      (vc-file-setprop file 'vc-name (car name-and-type)))))))

(defun vc-backend (file)
  "Return the version-control type of a file, nil if it is not registered."
  (and file
       (or (vc-file-getprop file 'vc-backend)
	   (let ((name-and-type (vc-registered file)))
	     (if name-and-type
		 (progn
		   (vc-file-setprop file 'vc-name (car name-and-type))
		   (vc-file-setprop file 'vc-backend (cdr name-and-type))))))))

;; Functions for querying the master and lock files.

(defun vc-match-substring (bn)
  (buffer-substring (match-beginning bn) (match-end bn)))

(defun vc-lock-file (file)
  ;; Generate lock file name corresponding to FILE
  (let ((master (vc-name file)))
    (and
     master
     (string-match "\\(.*/\\)s\\.\\(.*\\)" master)
     (concat
      (substring master (match-beginning 1) (match-end 1))
      "p."
      (substring master (match-beginning 2) (match-end 2))))))

(defun vc-parse-buffer (patterns &optional file properties)
  ;; Use PATTERNS to parse information out of the current buffer.
  ;; Each element of PATTERNS is a list of 2 to 3 elements. The first element
  ;; is the pattern to be matched, and the second (an integer) is the 
  ;; number of the subexpression that should be returned. If there's
  ;; a third element (also the number of a subexpression), that 
  ;; subexpression is assumed to be a date field and we want the most
  ;; recent entry matching the template.
  ;; If FILE and PROPERTIES are given, the latter must be a list of
  ;; properties of the same length as PATTERNS; each property is assigned 
  ;; the corresponding value.
  (mapcar (function (lambda (p)
	    (goto-char (point-min))
	    (cond 
	     ((eq (length p) 2)  ;; search for first entry
	      (let ((value nil))
		(if (re-search-forward (car p) nil t)
		    (setq value (vc-match-substring (elt p 1))))
		(if file
		    (progn (vc-file-setprop file (car properties) value)
			   (setq properties (cdr properties))))
		value))
	     ((eq (length p) 3)  ;; search for latest entry
	      (let ((latest-date "") (latest-val))
		(while (re-search-forward (car p) nil t)
		  (let ((date (vc-match-substring (elt p 2))))
		    (if (string< latest-date date)
			(progn
			  (setq latest-date date)
			  (setq latest-val
				(vc-match-substring (elt p 1)))))))
		(if file
		    (progn (vc-file-setprop file (car properties) latest-val)
			   (setq properties (cdr properties))))
		latest-val)))))
	  patterns)
  )

(defun vc-master-info (file fields &optional rfile properties)
  ;; Search for information in a master file.
  (if (and file (file-exists-p file))
      (save-excursion
	(let ((buf))
	  (setq buf (create-file-buffer file))
	  (set-buffer buf))
	(erase-buffer)
	(insert-file-contents file)
	(set-buffer-modified-p nil)
	(auto-save-mode nil)
	(prog1
	    (vc-parse-buffer fields rfile properties)
	  (kill-buffer (current-buffer)))
	)
    (if rfile
	(mapcar
	 (function (lambda (p) (vc-file-setprop rfile p nil)))
	 properties))
    )
  )

(defun vc-log-info (command file flags patterns &optional properties)
  ;; Search for information in log program output.
  ;; If there is a string `\X' in any of the PATTERNS, replace
  ;; it with a regexp to search for a branch revision.
  (if (and file (file-exists-p file))
      (save-excursion
	;; Run the command (not using vc-do-command, as that is
        ;; only available within vc.el)
	;; Don't switch to the *vc* buffer before running the command
	;; because that would change its default-directory.
	(save-excursion (set-buffer (get-buffer-create "*vc*"))
			(erase-buffer))
	(let ((exec-path (append vc-path exec-path))
	      ;; Add vc-path to PATH for the execution of this command.
	      (process-environment
	       (cons (concat "PATH=" (getenv "PATH")
			     ":" (mapconcat 'identity vc-path ":"))
		     process-environment)))
	  (apply 'call-process command nil "*vc*" nil 
		 (append flags (list (file-name-nondirectory file)))))
	(set-buffer (get-buffer "*vc*"))
	(set-buffer-modified-p nil)
	;; in the RCS case, insert branch version into
        ;; any patterns that contain \X
	(if (eq (vc-backend file) 'RCS)
	    (let ((branch 
		   (car (vc-parse-buffer 
			 '(("^branch:[ \t]+\\([0-9.]+\\)$" 1))))))
	      (setq patterns
		(mapcar 
		 (function 
  		  (lambda (p)
 		    (if (string-match "\\\\X" (car p))
			(if branch
			    (cond ((vc-branch-p branch)
				   (cons 
				    (concat 
				     (substring (car p) 0 (match-beginning 0))
				     (regexp-quote branch)
				     "\\.[0-9]+"
				     (substring (car p) (match-end 0)))
				    (cdr p)))
				  (t
				   (cons
				    (concat 
				     (substring (car p) 0 (match-beginning 0))
				     (regexp-quote branch)
				     (substring (car p) (match-end 0)))
				    (cdr p))))
			  ;; if there is no current branch, 
			  ;; return a completely different regexp, 
			  ;; which searches for the *head*
			  '("^head:[ \t]+\\([0-9.]+\\)$" 1))
		      p)))
		 patterns))))
	(prog1
	    (vc-parse-buffer patterns file properties)
	  (kill-buffer (current-buffer))
	  )
	)
    (if file
	(mapcar
	 (function (lambda (p) (vc-file-setprop file p nil)))
	 properties))
    )
  )

;;; Functions that determine property values, by examining the 
;;; working file, the master file, or log program output

(defun vc-consult-rcs-headers (file)
  ;; Search for RCS headers in FILE, and set properties
  ;; accordingly.  This function can be disabled by setting
  ;; vc-consult-headers to nil.  
  ;; Returns: nil            if no headers were found 
  ;;                         (or if the feature is disabled,
  ;;                         or if there is currently no buffer 
  ;;                         visiting FILE)
  ;;          'rev           if a workfile revision was found
  ;;          'rev-and-lock  if revision and lock info was found 
  (cond 
   ((or (not vc-consult-headers) 
	(not (get-file-buffer file)) nil))
   ((save-excursion
      (set-buffer (get-file-buffer file))
      (goto-char (point-min))
      (cond  
       ;; search for $Id or $Header
       ;; -------------------------
       ((re-search-forward "\\$\\(Id\\|Header\\): [^ ]+ \\([0-9.]+\\) "
			   nil t)
	;; if found, store the revision number ...
	(let ((rev (buffer-substring (match-beginning 2)
				     (match-end 2))))
	  ;; ... and check for the locking state
	  (if (re-search-forward 
	       (concat "\\=[0-9]+/[0-9]+/[0-9]+ "    ; date
		          "[0-9]+:[0-9]+:[0-9]+ "    ; time
		          "[^ ]+ [^ ]+ ")            ; author & state
	       nil t)
	      (cond 
	       ;; unlocked revision
	       ((looking-at "\\$")
		(vc-file-setprop file 'vc-workfile-version rev)
		(vc-file-setprop file 'vc-locking-user nil)
		(vc-file-setprop file 'vc-locked-version nil)
		'rev-and-lock)
	       ;; revision is locked by some user
	       ((looking-at "\\([^ ]+\\) \\$")
		(vc-file-setprop file 'vc-workfile-version rev)
		(vc-file-setprop file 'vc-locking-user 
				 (buffer-substring (match-beginning 1)
						   (match-end 1)))
		(vc-file-setprop file 'vc-locked-version rev) 
		'rev-and-lock)
	       ;; everything else: false
	       (nil))
	    ;; unexpected information in
	    ;; keyword string --> quit
	    nil)))
       ;; search for $Revision
       ;; --------------------
       ((re-search-forward (concat "\\$" 
				   "Revision: \\([0-9.]+\\) \\$")
			   nil t)
	;; if found, store the revision number ...
	(let ((rev (buffer-substring (match-beginning 1)
				     (match-end 1))))
	  ;; and see if there's any lock information
	  (goto-char (point-min))
	  (if (re-search-forward (concat "\\$" "Locker:") nil t)
	      (cond ((looking-at " \\([^ ]+\\) \\$")
		     (vc-file-setprop file 'vc-workfile-version rev)
		     (vc-file-setprop file 'vc-locking-user
				      (buffer-substring (match-beginning 1)
							(match-end 1)))
		     (vc-file-setprop file 'vc-locked-version rev)
		     'rev-and-lock)
		    ((looking-at " *\\$") 
		     (vc-file-setprop file 'vc-workfile-version rev)
		     (vc-file-setprop file 'vc-locking-user nil)
		     (vc-file-setprop file 'vc-locked-version nil)
		     'rev-and-lock)
		    (t 
		     (vc-file-setprop file 'vc-workfile-version rev)
		     'rev-and-lock))
	    (vc-file-setprop file 'vc-workfile-version rev)
	    'rev)))
       ;; else: nothing found
       ;; -------------------
       (t nil))))))

(defun vc-fetch-properties (file)
  ;; Re-fetch some properties associated with the given file.
  (cond 
   ((eq (vc-backend file) 'SCCS)
    (progn
      (vc-master-info (vc-lock-file file)
		      (list
		       '("^[^ ]+ [^ ]+ \\([^ ]+\\)" 1)
		       '("^\\([^ ]+\\)" 1))
		      file
		      '(vc-locking-user vc-locked-version))
      (vc-master-info (vc-name file)
		      (list
		       '("^\001d D \\([^ ]+\\)" 1)
		       (list (concat "^\001d D \\([^ ]+\\) .* " 
				     (regexp-quote (user-login-name)) " ")
			     1)
		       )
		      file
		      '(vc-latest-version vc-your-latest-version))
      ))
   ((eq (vc-backend file) 'RCS)
    (vc-log-info "rlog" file nil
		 (list
		  '("^locks: strict\n\t\\([^:]+\\)" 1)
		  '("^locks: strict\n\t[^:]+: \\(.+\\)" 1)
		  '("^revision[\t ]+\\([0-9.]+\\)\\(\n\\|[ \t].*\n\\)date: \\([ /0-9:]+\\);" 1 3)
		  (list 
		   (concat
		    "^revision[\t ]+\\([0-9.]+\\)\\(\n\\|[ \t].*\n\\)date: \\([ /0-9:]+\\); *author: "
		    (regexp-quote (user-login-name))
		    ";") 1 3)
		  ;; special regexp to search for branch revision:
		  ;; \X will be replaced by vc-log-info (see there)
		  '("^revision[\t ]+\\(\\X\\)\\(\n\\|[ \t].*\n\\)date: \\([ /0-9:]+\\);" 1 3))

		 '(vc-locking-user 
		   vc-locked-version
		   vc-latest-version 
		   vc-your-latest-version
		   vc-branch-version)))
   ((eq (vc-backend file) 'CVS)
    (vc-log-info "cvs" file '("status")
    ;; CVS 1.3 says "RCS Version:", other releases "RCS Revision:",
    ;; and CVS 1.4a1 says "Repository revision:".
    '(("\\(RCS Version\\|RCS Revision\\|Repository revision\\):[\t ]+\\([0-9.]+\\)" 2)
      ("^File: [^ \t]+[ \t]+Status: \\(.*\\)" 1))
    '(vc-latest-version vc-cvs-status))
    ;; Translate those status values that are needed into symbols.
    ;; Any other value is converted to nil.
    (let ((status (vc-file-getprop file 'vc-cvs-status)))
      (cond ((string-match "Up-to-date" status)
	     (vc-file-setprop file 'vc-cvs-status 'up-to-date)
	     (vc-file-setprop file 'vc-checkout-time 
			      (nth 5 (file-attributes file))))
	    ((string-match "Locally Modified" status)
	     (vc-file-setprop file 'vc-cvs-status 'locally-modified))
	    ((string-match "Needs Merge" status)
	     (vc-file-setprop file 'vc-cvs-status 'needs-merge))
	    (t (vc-file-setprop file 'vc-cvs-status nil))))
   )))

(defun vc-backend-subdirectory-name (&optional file)
  ;; Where the master and lock files for the current directory are kept
  (symbol-name
   (or
    (and file (vc-backend file))
    vc-default-back-end
    (setq vc-default-back-end (if (vc-find-binary "rcs") 'RCS 'SCCS)))))


;;; Access functions to file properties
;;; (Properties should be _set_ using vc-file-setprop, but
;;; _retrieved_ only through these functions, which decide
;;; if the property is already known or not. A property should
;;; only be retrieved by vc-file-getprop if there is no 
;;; access function.)

;; functions vc-name and vc-backend come earlier above, 
;; because they are needed by vc-log-info etc.

(defun vc-cvs-status (file)
  ;; Return the cvs status of FILE
  ;; (Status field in output of "cvs status")
  (cond ((vc-file-getprop file 'vc-cvs-status))
	(t (vc-fetch-properties file)
	   (vc-file-getprop file 'vc-cvs-status))))

(defun vc-locking-user (file)
  "Return the name of the person currently holding a lock on FILE.
Return nil if there is no such person.
Under CVS, a file is considered locked if it has been modified since it
was checked out.  Under CVS, this will sometimes return the uid of
the owner of the file (as a number) instead of a string."
  ;; The property is cached. If it is non-nil, it is simply returned.
  ;; The other routines clear it when the locking state changes.
  (setq file (expand-file-name file));; ??? Work around bug in 19.0.4
  (cond
   ((vc-file-getprop file 'vc-locking-user))
   ((eq (vc-backend file) 'CVS)
    (if (eq (vc-cvs-status file) 'up-to-date)
	nil
      ;; The expression below should return the username of the owner
      ;; of the file.  It doesn't.  It returns the username if it is
      ;; you, or otherwise the UID of the owner of the file.  The
      ;; return value from this function is only used by
      ;; vc-dired-reformat-line, and it does the proper thing if a UID
      ;; is returned.
      ;; 
      ;; The *proper* way to fix this would be to implement a built-in
      ;; function in Emacs, say, (username UID), that returns the
      ;; username of a given UID.
      ;;
      ;; The result of this hack is that vc-directory will print the
      ;; name of the owner of the file for any files that are
      ;; modified.
      (let ((uid (nth 2 (file-attributes file))))
	(if (= uid (user-uid))
	    (vc-file-setprop file 'vc-locking-user (user-login-name))
	  (vc-file-setprop file 'vc-locking-user uid)))))
   (t
    (if (and (eq (vc-backend file) 'RCS)
	     (eq (vc-consult-rcs-headers file) 'rev-and-lock))
	(vc-file-getprop file 'vc-locking-user)
      (if (or (not vc-keep-workfiles)
	      (eq vc-mistrust-permissions 't)
	      (and vc-mistrust-permissions
		   (funcall vc-mistrust-permissions 
			    (vc-backend-subdirectory-name file))))
	  (vc-file-setprop file 'vc-locking-user (vc-true-locking-user file))
	;; This implementation assumes that any file which is under version
	;; control and has -rw-r--r-- is locked by its owner.  This is true
	;; for both RCS and SCCS, which keep unlocked files at -r--r--r--.
	;; We have to be careful not to exclude files with execute bits on;
	;; scripts can be under version control too.  Also, we must ignore
	;; the group-read and other-read bits, since paranoid users turn them off.
	;; This hack wins because calls to the very expensive vc-fetch-properties
	;; function only have to be made if (a) the file is locked by someone
	;; other than the current user, or (b) some untoward manipulation
	;; behind vc's back has changed the owner or the `group' or `other'
	;; write bits.
	(let ((attributes (file-attributes file)))
	  (cond ((string-match ".r-..-..-." (nth 8 attributes))
		 nil)
		((and (= (nth 2 attributes) (user-uid))
		      (string-match ".rw..-..-." (nth 8 attributes)))
		 (vc-file-setprop file 'vc-locking-user (user-login-name)))
		(t
		 (vc-file-setprop file 'vc-locking-user 
				  (vc-true-locking-user file))))))))))

(defun vc-true-locking-user (file)
  ;; The slow but reliable version
  (vc-fetch-properties file)
  (vc-file-getprop file 'vc-locking-user))

(defun vc-latest-version (file)
  ;; Return version level of the latest version of FILE
  (vc-fetch-properties file)
  (vc-file-getprop file 'vc-latest-version))

(defun vc-your-latest-version (file)
  ;; Return version level of the latest version of FILE checked in by you
  (vc-fetch-properties file)
  (vc-file-getprop file 'vc-your-latest-version))

(defun vc-branch-version (file)
  ;; Return version level of the highest revision on the default branch
  ;; If there is no default branch, return the highest version number
  ;; on the trunk.
  ;; This property is defined for RCS only.
  (vc-fetch-properties file)
  (vc-file-getprop file 'vc-branch-version))

(defun vc-workfile-version (file)
  ;; Return version level of the current workfile FILE
  ;; This is attempted by first looking at the RCS keywords.
  ;; If there are no keywords in the working file, 
  ;; vc-branch-version is taken.
  ;; Note that this property is cached, that is, it is only 
  ;; looked up if it is nil.
  ;; For SCCS, this property is equivalent to vc-latest-version.
  (cond ((vc-file-getprop file 'vc-workfile-version))
	((eq (vc-backend file) 'SCCS) (vc-latest-version file))
	((eq (vc-backend file) 'RCS)
	 (if (vc-consult-rcs-headers file)
	     (vc-file-getprop file 'vc-workfile-version)
	   (let ((rev (cond ((vc-branch-version file))
			    ((vc-latest-version file)))))
	     (vc-file-setprop file 'vc-workfile-version rev)
	     rev)))
	((eq (vc-backend file) 'CVS)
	 (if (vc-consult-rcs-headers file)   ;; CVS
	     (vc-file-getprop file 'vc-workfile-version)
	   (vc-find-cvs-master (file-name-directory file)
			       (file-name-nondirectory file))
	   (vc-file-getprop file 'vc-workfile-version)))))

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

(defun vc-buffer-backend ()
  "Return the version-control type of the visited file, or nil if none."
  (if (eq vc-buffer-backend t)
      (setq vc-buffer-backend (vc-backend (buffer-file-name)))
    vc-buffer-backend))

(defun vc-toggle-read-only (&optional verbose)
  "Change read-only status of current buffer, perhaps via version control.
If the buffer is visiting a file registered with version control,
then check the file in or out.  Otherwise, just change the read-only flag
of the buffer.  With prefix argument, ask for version number."
  (interactive "P")
  (if (vc-backend (buffer-file-name))
      (vc-next-action verbose)
    (toggle-read-only)))
(define-key global-map "\C-x\C-q" 'vc-toggle-read-only)

(defun vc-mode-line (file &optional label)
  "Set `vc-mode' to display type of version control for FILE.
The value is set in the current buffer, which should be the buffer
visiting FILE.  Second optional arg LABEL is put in place of version
control system name."
  (interactive (list buffer-file-name nil))
  (let ((vc-type (vc-backend file))
	(vc-status-string (and vc-display-status (vc-status file))))
    (setq vc-mode
	  (concat " " (or label (symbol-name vc-type)) vc-status-string))
    ;; Make the buffer read-only if the file is not locked
    ;; (or unchanged, in the CVS case).
    ;; Determine this by looking at the mode string, 
    ;; so that no further external status query is necessary
    (if vc-status-string
	(if (eq (elt vc-status-string 0) ?-)
	    (setq buffer-read-only t))
      (if (not (vc-locking-user file))
	  (setq buffer-read-only t)))
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
	 (let ((link-type (vc-backend (file-symlink-p file))))
	   (if link-type
	       (message
		"Warning: symbolic link to %s-controlled source file"
		link-type))))
    (force-mode-line-update)
    ;;(set-buffer-modified-p (buffer-modified-p)) ;;use this if Emacs 18
    vc-type))

(defun vc-status (file)
  ;; Return string for placement in modeline by `vc-mode-line'.
  ;; Format:
  ;;
  ;;   "-REV"        if the revision is not locked
  ;;   ":REV"        if the revision is locked by the user
  ;;   ":LOCKER:REV" if the revision is locked by somebody else
  ;;   " @@"         for a CVS file that is added, but not yet committed
  ;;
  ;; In the CVS case, a "locked" working file is a 
  ;; working file that is modified with respect to the master.
  ;; The file is "locked" from the moment when the user makes 
  ;; the buffer writable.
  ;; 
  ;; This function assumes that the file is registered.

  (let ((locker (vc-locking-user file))
	(rev (vc-workfile-version file)))
    (cond ((string= "0" rev)
	   " @@")
	  ((not locker)
	   (concat "-" rev))
	  ((if (stringp locker)
	       (string= locker (user-login-name))
	     (= locker (user-uid)))
	   (concat ":" rev))
	  (t 
	   (concat ":" locker ":" rev)))))

;;; install a call to the above as a find-file hook
(defun vc-find-file-hook ()
  ;; Recompute whether file is version controlled,
  ;; if user has killed the buffer and revisited.
  (cond 
   (buffer-file-name
    (vc-file-clearprops buffer-file-name)
    (cond
     ((vc-backend buffer-file-name)
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
  (if (vc-backend buffer-file-name)
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
