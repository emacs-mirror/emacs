;;; vc-hooks.el --- resident support for version-control

;; Copyright (C) 1992, 1993, 1994, 1995, 1996, 1998 Free Software Foundation, Inc.

;; Author:     Eric S. Raymond <esr@snark.thyrsus.com>
;; Maintainer: Andre Spiegel <spiegel@inf.fu-berlin.de>

;; $Id: vc-hooks.el,v 1.1 2000/01/10 13:25:12 gerd Exp gerd $

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

;; This is the always-loaded portion of VC.
;; It takes care VC-related activities that are done when you visit a file,
;; so that vc.el itself is loaded only when you use a VC command.
;; See the commentary of vc.el.

;;; Code:

;; Customization Variables (the rest is in vc.el)

(defcustom vc-default-back-end nil
  "*Back-end actually used by this interface; may be SCCS or RCS.
The value is only computed when needed to avoid an expensive search."
  :type '(choice (const nil) (const RCS) (const SCCS))
  :group 'vc)

(defcustom vc-handle-cvs t
  "*If non-nil, use VC for files managed with CVS.
If it is nil, don't use VC for those files."
  :type 'boolean
  :group 'vc)

(defcustom vc-rcsdiff-knows-brief nil
  "*Indicates whether rcsdiff understands the --brief option.
The value is either `yes', `no', or nil.  If it is nil, VC tries
to use --brief and sets this variable to remember whether it worked."
  :type '(choice (const nil) (const yes) (const no))
  :group 'vc)

(defcustom vc-path
  (if (file-directory-p "/usr/sccs")
      '("/usr/sccs")
    nil)
  "*List of extra directories to search for version control commands."
  :type '(repeat directory)
  :group 'vc)

(defcustom vc-master-templates
  '(("%sRCS/%s,v" . RCS) ("%s%s,v" . RCS) ("%sRCS/%s" . RCS)
    ("%sSCCS/s.%s" . SCCS) ("%ss.%s". SCCS)
    vc-find-cvs-master
    vc-search-sccs-project-dir)
  "*Where to look for version-control master files.
The first pair corresponding to a given back end is used as a template
when creating new masters.
Setting this variable to nil turns off use of VC entirely."
  :type '(repeat sexp)
  :group 'vc)

(defcustom vc-make-backup-files nil
  "*If non-nil, backups of registered files are made as with other files.
If nil (the default), files covered by version control don't get backups."
  :type 'boolean
  :group 'vc)

(defcustom vc-follow-symlinks 'ask
  "*Indicates what to do if you visit a symbolic link to a file
that is under version control.  Editing such a file through the
link bypasses the version control system, which is dangerous and
probably not what you want.  
  If this variable is t, VC follows the link and visits the real file,
telling you about it in the echo area.  If it is `ask', VC asks for
confirmation whether it should follow the link.  If nil, the link is
visited and a warning displayed."
  :type '(choice (const ask) (const nil) (const t))
  :group 'vc)

(defcustom vc-display-status t
  "*If non-nil, display revision number and lock status in modeline.
Otherwise, not displayed."
  :type 'boolean
  :group 'vc)


(defcustom vc-consult-headers t
  "*If non-nil, identify work files by searching for version headers."
  :type 'boolean
  :group 'vc)

(defcustom vc-keep-workfiles t
  "*If non-nil, don't delete working files after registering changes.
If the back-end is CVS, workfiles are always kept, regardless of the
value of this flag."
  :type 'boolean
  :group 'vc)

(defcustom vc-mistrust-permissions nil
  "*If non-nil, don't assume that permissions and ownership track 
version-control status.  If nil, do rely on the permissions.
See also variable `vc-consult-headers'."
  :type 'boolean
  :group 'vc)

(defcustom vc-ignore-vc-files nil
  "*If non-nil don't look for version control information when finding files.

It may be useful to set this if (say) you edit files in a directory
containing corresponding RCS files but don't have RCS available;
similarly for other version control systems."
  :type 'boolean
  :group 'vc
  :version "20.3")

(defun vc-mistrust-permissions (file)
  ;; Access function to the above.
  (or (eq vc-mistrust-permissions 't)
      (and vc-mistrust-permissions
	   (funcall vc-mistrust-permissions 
		    (vc-backend-subdirectory-name file)))))

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

(defun vc-file-clearprops (file)
  ;; clear all properties of a given file
  (setplist (intern file vc-file-prop-obarray) nil))

;;; Functions that determine property values, by examining the 
;;; working file, the master file, or log program output

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
  ;; recent entry matching the template; this works for RCS format dates only.
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
		    ;; Most (but not all) versions of RCS use two-digit years
		    ;; to represent dates in the range 1900 through 1999.
		    ;; The two-digit and four-digit notations can both appear
		    ;; in the same file.  Normalize the two-digit versions.
		    (save-match-data
		      (if (string-match "\\`[0-9][0-9]\\." date)
                          (setq date (concat "19" date))))
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

(defun vc-insert-file (file &optional limit blocksize)
  ;; Insert the contents of FILE into the current buffer.
  ;; Optional argument LIMIT is a regexp. If present,
  ;; the file is inserted in chunks of size BLOCKSIZE
  ;; (default 8 kByte), until the first occurrence of
  ;; LIMIT is found. The function returns nil if FILE 
  ;; doesn't exist.
  (erase-buffer)
  (cond ((file-exists-p file)
	 (cond (limit
		(if (not blocksize) (setq blocksize 8192))
		(let (found s)
		  (while (not found)
		    (setq s (buffer-size))
		    (goto-char (1+ s))
		    (setq found 
			  (or (zerop (car (cdr 
			      (insert-file-contents file nil s 
			       (+ s blocksize)))))
			      (progn (beginning-of-line)
				     (re-search-forward limit nil t)))))))
	       (t (insert-file-contents file)))
	 (set-buffer-modified-p nil)
	 (auto-save-mode nil)
	 t)
	(t nil)))

(defun vc-parse-locks (file locks)
  ;; Parse RCS or SCCS locks.
  ;; The result is a list of the form ((VERSION USER) (VERSION USER) ...),
  ;; which is returned and stored into the property `vc-master-locks'.
  (if (not locks) 
      (vc-file-setprop file 'vc-master-locks 'none)
    (let ((found t) (index 0) master-locks version user)
      (cond ((eq (vc-backend file) 'SCCS)
	     (while (string-match "^\\([0-9.]+\\) [0-9.]+ \\([^ ]+\\) .*\n?"
				   locks index)
	       (setq version (substring locks 
					(match-beginning 1) (match-end 1)))
	       (setq user (substring locks 
				     (match-beginning 2) (match-end 2)))
	       (setq master-locks (append master-locks 
					  (list (cons version user))))
	       (setq index (match-end 0))))
	    ((eq (vc-backend file) 'RCS)
	     (while (string-match "[ \t\n]*\\([^:]+\\):\\([0-9.]+\\)"
				  locks index)
	       (setq version (substring locks 
					(match-beginning 2) (match-end 2)))
	       (setq user (substring locks 
				     (match-beginning 1) (match-end 1)))
	       (setq master-locks (append master-locks 
					  (list (cons version user))))
	       (setq index (match-end 0)))
	     (if (string-match ";[ \t\n]+strict;" locks index)
		 (vc-file-setprop file 'vc-checkout-model 'manual)
	       (vc-file-setprop file 'vc-checkout-model 'implicit))))
      (vc-file-setprop file 'vc-master-locks (or master-locks 'none)))))

(defun vc-simple-command (okstatus command file &rest args)
  ;; Simple version of vc-do-command, for use in vc-hooks only.
  ;; Don't switch to the *vc-info* buffer before running the
  ;; command, because that would change its default directory
  (save-excursion (set-buffer (get-buffer-create "*vc-info*"))
		  (erase-buffer))
  (let ((exec-path (append vc-path exec-path)) exec-status
	;; Add vc-path to PATH for the execution of this command.
	(process-environment
	 (cons (concat "PATH=" (getenv "PATH")
		       path-separator 
		       (mapconcat 'identity vc-path path-separator))
	       process-environment)))
    (setq exec-status 
	  (apply 'call-process command nil "*vc-info*" nil 
		 (append args (list file))))
    (cond ((> exec-status okstatus)
	   (switch-to-buffer (get-file-buffer file))
	   (shrink-window-if-larger-than-buffer
	    (display-buffer "*vc-info*"))
	   (error "Couldn't find version control information")))
    exec-status))

(defun vc-parse-cvs-status (&optional full)
  ;; Parse output of "cvs status" command in the current buffer and
  ;; set file properties accordingly.  Unless FULL is t, parse only
  ;; essential information.
  (let (file status)
    (goto-char (point-min))
    (if (re-search-forward "^File: " nil t)
        (cond 
         ((looking-at "no file") nil)
         ((re-search-forward "\\=\\([^ \t]+\\)" nil t)
          (setq file (concat default-directory (match-string 1)))
          (vc-file-setprop file 'vc-backend 'CVS)
          (if (not (re-search-forward "\\=[ \t]+Status: \\(.*\\)" nil t))
              (setq status "Unknown")
            (setq status (match-string 1)))
          (if (and full 
                   (re-search-forward 
  "\\(RCS Version\\|RCS Revision\\|Repository revision\\):[\t ]+\\([0-9.]+\\)"
                    nil t))
              (vc-file-setprop file 'vc-latest-version (match-string 2)))
          (cond 
           ((string-match "Up-to-date" status)
            (vc-file-setprop file 'vc-cvs-status 'up-to-date)
            (vc-file-setprop file 'vc-checkout-time 
                             (nth 5 (file-attributes file))))
           ((vc-file-setprop file 'vc-cvs-status
             (cond 
              ((string-match "Locally Modified"    status) 'locally-modified)
              ((string-match "Needs Merge"         status) 'needs-merge)
              ((string-match "Needs \\(Checkout\\|Patch\\)" status) 
               'needs-checkout)
              ((string-match "Unresolved Conflict" status) 
	       'unresolved-conflict)
	      ((string-match "File had conflicts on merge" status)
	       'unresolved-conflict)
              ((string-match "Locally Added"       status) 'locally-added)
              ((string-match "New file!"           status) 'locally-added)
              (t 'unknown))))))))))

(defun vc-fetch-master-properties (file)
  ;; Fetch those properties of FILE that are stored in the master file.
  ;; For an RCS file, we don't get vc-latest-version vc-your-latest-version
  ;; here because that is slow.
  ;; That gets done if/when the functions vc-latest-version
  ;; and vc-your-latest-version get called.
  (save-excursion
    (cond
     ((eq (vc-backend file) 'SCCS)
      (set-buffer (get-buffer-create "*vc-info*"))
      (if (vc-insert-file (vc-lock-file file))
	  (vc-parse-locks file (buffer-string))
	(vc-file-setprop file 'vc-master-locks 'none))
      (vc-insert-file (vc-name file) "^\001e")
      (vc-parse-buffer 
       (list '("^\001d D \\([^ ]+\\)" 1)
	     (list (concat "^\001d D \\([^ ]+\\) .* " 
			   (regexp-quote (vc-user-login-name)) " ") 1))
       file
       '(vc-latest-version vc-your-latest-version)))

     ((eq (vc-backend file) 'RCS)
      (set-buffer (get-buffer-create "*vc-info*"))
      (vc-insert-file (vc-name file) "^[0-9]")
      (vc-parse-buffer 
       (list '("^head[ \t\n]+\\([^;]+\\);" 1)
	     '("^branch[ \t\n]+\\([^;]+\\);" 1)
	     '("^locks[ \t\n]*\\([^;]*;\\([ \t\n]*strict;\\)?\\)" 1))
       file
       '(vc-head-version
	 vc-default-branch
	 vc-master-locks))
      ;; determine vc-master-workfile-version: it is either the head
      ;; of the trunk, the head of the default branch, or the 
      ;; "default branch" itself, if that is a full revision number.
      (let ((default-branch (vc-file-getprop file 'vc-default-branch)))
	(cond 
	 ;; no default branch
	 ((or (not default-branch) (string= "" default-branch))
	  (vc-file-setprop file 'vc-master-workfile-version 
			   (vc-file-getprop file 'vc-head-version)))
	 ;; default branch is actually a revision
	 ((string-match "^[0-9]+\\.[0-9]+\\(\\.[0-9]+\\.[0-9]+\\)*$" 
			default-branch)
	  (vc-file-setprop file 'vc-master-workfile-version default-branch))
	 ;; else, search for the head of the default branch
	 (t (vc-insert-file (vc-name file) "^desc")
	    (vc-parse-buffer (list (list 
	       (concat "^\\(" 
		       (regexp-quote default-branch)
		       "\\.[0-9]+\\)\ndate[ \t]+\\([0-9.]+\\);") 1 2))
			 file '(vc-master-workfile-version)))))
      ;; translate the locks
      (vc-parse-locks file (vc-file-getprop file 'vc-master-locks)))

     ((eq (vc-backend file) 'CVS)
      (save-excursion
        ;; Call "cvs status" in the right directory, passing only the
        ;; nondirectory part of the file name -- otherwise CVS might 
        ;; silently give a wrong result.
        (let ((default-directory (file-name-directory file)))
          (vc-simple-command 0 "cvs" (file-name-nondirectory file) "status"))
	(set-buffer (get-buffer "*vc-info*"))
        (vc-parse-cvs-status t))))
    (if (get-buffer "*vc-info*")
	(kill-buffer (get-buffer "*vc-info*")))))

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
	(not (get-file-buffer file))) nil)
   ((let (status version locking-user)
     (save-excursion
      (set-buffer (get-file-buffer file))
      (goto-char (point-min))
      (cond  
       ;; search for $Id or $Header
       ;; -------------------------
       ;; The `\ 's below avoid an RCS 5.7 bug when checking in this file.
       ((or (and (search-forward "$Id\ : " nil t)
		 (looking-at "[^ ]+ \\([0-9.]+\\) "))
	    (and (progn (goto-char (point-min))
			(search-forward "$Header\ : " nil t))
		 (looking-at "[^ ]+ \\([0-9.]+\\) ")))
	(goto-char (match-end 0))
	;; if found, store the revision number ...
	(setq version (buffer-substring-no-properties (match-beginning 1)
						      (match-end 1)))
	;; ... and check for the locking state
	(cond 
	 ((looking-at
	   (concat "[0-9]+[/-][01][0-9][/-][0-3][0-9] "             ; date
	    "[0-2][0-9]:[0-5][0-9]+:[0-6][0-9]+\\([+-][0-9:]+\\)? " ; time
	           "[^ ]+ [^ ]+ "))                       ; author & state
	  (goto-char (match-end 0)) ; [0-6] in regexp handles leap seconds
	  (cond 
	   ;; unlocked revision
	   ((looking-at "\\$")
	    (setq locking-user 'none)
	    (setq status 'rev-and-lock))
	   ;; revision is locked by some user
	   ((looking-at "\\([^ ]+\\) \\$")
	    (setq locking-user
		  (buffer-substring-no-properties (match-beginning 1)
						  (match-end 1)))
	    (setq status 'rev-and-lock))
	   ;; everything else: false
	   (nil)))
	 ;; unexpected information in
	 ;; keyword string --> quit
	 (nil)))
       ;; search for $Revision
       ;; --------------------
       ((re-search-forward (concat "\\$" 
				   "Revision: \\([0-9.]+\\) \\$")
			   nil t)
	;; if found, store the revision number ...
	(setq version (buffer-substring-no-properties (match-beginning 1)
						      (match-end 1)))
	;; and see if there's any lock information
	(goto-char (point-min))
	(if (re-search-forward (concat "\\$" "Locker:") nil t)
	    (cond ((looking-at " \\([^ ]+\\) \\$")
		   (setq locking-user (buffer-substring-no-properties
				       (match-beginning 1)
				       (match-end 1)))
		   (setq status 'rev-and-lock))
		  ((looking-at " *\\$") 
		   (setq locking-user 'none)
		   (setq status 'rev-and-lock))
		  (t 
		   (setq locking-user 'none)
		   (setq status 'rev-and-lock)))
	  (setq status 'rev)))
       ;; else: nothing found
       ;; -------------------
       (t nil)))
     (if status (vc-file-setprop file 'vc-workfile-version version))
     (and (eq status 'rev-and-lock)
	  (eq (vc-backend file) 'RCS)
	  (vc-file-setprop file 'vc-locking-user locking-user)
	  ;; If the file has headers, we don't want to query the master file,
	  ;; because that would eliminate all the performance gain the headers
	  ;; brought us.  We therefore use a heuristic for the checkout model 
	  ;; now:  If we trust the file permissions, and the file is not 
          ;; locked, then if the file is read-only the checkout model is 
	  ;; `manual', otherwise `implicit'.
	  (not (vc-mistrust-permissions file))
	  (not (vc-locking-user file))
	  (if (string-match ".r-..-..-." (nth 8 (file-attributes file)))
	      (vc-file-setprop file 'vc-checkout-model 'manual)
	    (vc-file-setprop file 'vc-checkout-model 'implicit)))
     status))))

;;; Access functions to file properties
;;; (Properties should be _set_ using vc-file-setprop, but
;;; _retrieved_ only through these functions, which decide
;;; if the property is already known or not. A property should
;;; only be retrieved by vc-file-getprop if there is no 
;;; access function.)

;;; properties indicating the backend 
;;; being used for FILE

(defun vc-backend-subdirectory-name (&optional file)
  ;; Where the master and lock files for the current directory are kept
  (symbol-name
   (or
    (and file (vc-backend file))
    vc-default-back-end
    (setq vc-default-back-end (if (vc-find-binary "rcs") 'RCS 'SCCS)))))

(defun vc-name (file)
  "Return the master name of a file, nil if it is not registered.
For CVS, the full name of CVS/Entries is returned."
  (or (vc-file-getprop file 'vc-name)
      ;; Use the caching mechanism of vc-backend, below.
      (if (vc-backend file)
	  (vc-file-getprop file 'vc-name))))

(defun vc-backend (file)
  "Return the version-control type of a file, nil if it is not registered."
  ;; Note that internally, Emacs remembers unregistered 
  ;; files by setting the property to `none'.
  (if file
      (let ((property (vc-file-getprop file 'vc-backend))
	    (name-and-type))
	(cond ((eq property 'none) nil)
	      (property)
	      (t (setq name-and-type (vc-registered file))
		 (if name-and-type
		     (progn
		       (vc-file-setprop file 'vc-name (car name-and-type))
		       (vc-file-setprop file 'vc-backend (cdr name-and-type)))
		   (vc-file-setprop file 'vc-backend 'none)
		   nil))))))

(defun vc-checkout-model (file)
  ;; Return `manual' if the user has to type C-x C-q to check out FILE.
  ;; Return `implicit' if the file can be modified without locking it first.
  (or
   (vc-file-getprop file 'vc-checkout-model)
   (cond 
    ((eq (vc-backend file) 'SCCS)
     (vc-file-setprop file 'vc-checkout-model 'manual))
    ((eq (vc-backend file) 'RCS) 
     (vc-consult-rcs-headers file)
     (or (vc-file-getprop file 'vc-checkout-model)
	 (progn (vc-fetch-master-properties file)
		(vc-file-getprop file 'vc-checkout-model))))
    ((eq (vc-backend file) 'CVS)
     (vc-file-setprop file 'vc-checkout-model
      (cond
       ((getenv "CVSREAD") 'manual)
       ;; If the file is not writeable, this is probably because the
       ;; file is being "watched" by other developers.  Use "manual"
       ;; checkout in this case.  (If vc-mistrust-permissions was t,
       ;; we actually shouldn't trust this, but there is no other way
       ;; to learn this from CVS at the moment (version 1.9).)
       ((string-match "r-..-..-." (nth 8 (file-attributes file)))
        'manual)
       (t 'implicit)))))))

;;; properties indicating the locking state

(defun vc-cvs-status (file)
  ;; Return the cvs status of FILE
  ;; (Status field in output of "cvs status")
  (cond ((vc-file-getprop file 'vc-cvs-status))
	(t (vc-fetch-master-properties file)
	   (vc-file-getprop file 'vc-cvs-status))))

(defun vc-master-locks (file)
  ;; Return the lock entries in the master of FILE.
  ;; Return 'none if there are no such entries, and a list
  ;; of the form ((VERSION USER) (VERSION USER) ...) otherwise.
  (cond ((vc-file-getprop file 'vc-master-locks))
	(t (vc-fetch-master-properties file)
	   (vc-file-getprop file 'vc-master-locks))))

(defun vc-master-locking-user (file)
  ;; Return the master file's idea of who is locking 
  ;; the current workfile version of FILE.  
  ;; Return 'none if it is not locked.
  (let ((master-locks (vc-master-locks file)) lock)
    (if (eq master-locks 'none) 'none
      ;; search for a lock on the current workfile version
      (setq lock (assoc (vc-workfile-version file) master-locks))
      (cond (lock (cdr lock))
	    ('none)))))

(defun vc-lock-from-permissions (file)
  ;; If the permissions can be trusted for this file, determine the
  ;; locking state from them.  Returns (user-login-name), `none', or nil.
   ;;   This implementation assumes that any file which is under version
  ;; control and has -rw-r--r-- is locked by its owner.  This is true
  ;; for both RCS and SCCS, which keep unlocked files at -r--r--r--.
  ;; We have to be careful not to exclude files with execute bits on;
  ;; scripts can be under version control too.  Also, we must ignore the
  ;; group-read and other-read bits, since paranoid users turn them off.
  ;;   This hack wins because calls to the somewhat expensive 
  ;; `vc-fetch-master-properties' function only have to be made if 
  ;; (a) the file is locked by someone other than the current user, 
  ;; or (b) some untoward manipulation behind vc's back has changed 
  ;; the owner or the `group' or `other' write bits.
  (let ((attributes (file-attributes file)))
    (if (not (vc-mistrust-permissions file))
	(cond ((string-match ".r-..-..-." (nth 8 attributes))
	       (vc-file-setprop file 'vc-locking-user 'none))
	      ((and (= (nth 2 attributes) (user-uid))
		    (string-match ".rw..-..-." (nth 8 attributes)))
	       (vc-file-setprop file 'vc-locking-user (vc-user-login-name)))
	      (nil)))))

(defun vc-user-login-name (&optional uid)
  ;; Return the name under which the user is logged in, as a string.
  ;; (With optional argument UID, return the name of that user.)
  ;; This function does the same as `user-login-name', but unlike
  ;; that, it never returns nil.  If a UID cannot be resolved, that
  ;; UID is returned as a string.
  (or (user-login-name uid)
      (and uid (number-to-string uid))
      (number-to-string (user-uid))))

(defun vc-file-owner (file)
  ;; Return who owns FILE (user name, as a string).
  (vc-user-login-name (nth 2 (file-attributes file))))

(defun vc-rcs-lock-from-diff (file)
  ;; Diff the file against the master version.  If differences are found,
  ;; mark the file locked.  This is only used for RCS with non-strict
  ;; locking.  (If "rcsdiff" doesn't understand --brief, we do a double-take
  ;; and remember the fact for the future.)
  (let* ((version (concat "-r" (vc-workfile-version file)))
         (status (if (eq vc-rcsdiff-knows-brief 'no)
                     (vc-simple-command 1 "rcsdiff" file version)
                   (vc-simple-command 2 "rcsdiff" file "--brief" version))))
    (if (eq status 2)
        (if (not vc-rcsdiff-knows-brief)
            (setq vc-rcsdiff-knows-brief 'no
                  status (vc-simple-command 1 "rcsdiff" file version))
          (error "rcsdiff failed."))
      (if (not vc-rcsdiff-knows-brief) (setq vc-rcsdiff-knows-brief 'yes)))
    (if (zerop status)
        (vc-file-setprop file 'vc-locking-user 'none)
      (vc-file-setprop file 'vc-locking-user (vc-file-owner file)))))

(defun vc-locking-user (file)
  ;; Return the name of the person currently holding a lock on FILE.
  ;; Return nil if there is no such person.
  ;;   Under CVS, a file is considered locked if it has been modified since
  ;; it was checked out.
  ;;   The property is cached.  It is only looked up if it is currently nil.
  ;; Note that, for a file that is not locked, the actual property value
  ;; is `none', to distinguish it from an unknown locking state.  That value
  ;; is converted to nil by this function, and returned to the caller.
  (let ((locking-user (vc-file-getprop file 'vc-locking-user)))
    (if locking-user
	;; if we already know the property, return it
	(if (eq locking-user 'none) nil locking-user)

      ;; otherwise, infer the property...
      (cond
       ((eq (vc-backend file) 'CVS)
	(or (and (eq (vc-checkout-model file) 'manual)
		 (vc-lock-from-permissions file))
	    (and (equal (vc-file-getprop file 'vc-checkout-time)
			(nth 5 (file-attributes file)))
		 (vc-file-setprop file 'vc-locking-user 'none))
	    (vc-file-setprop file 'vc-locking-user (vc-file-owner file))))

       ((eq (vc-backend file) 'RCS)
	(let (p-lock)

	  ;; Check for RCS headers first
	  (or (eq (vc-consult-rcs-headers file) 'rev-and-lock)

	      ;; If there are no headers, try to learn it 
	      ;; from the permissions.
	      (and (setq p-lock (vc-lock-from-permissions file))
		   (if (eq p-lock 'none)

		       ;; If the permissions say "not locked", we know
		       ;; that the checkout model must be `manual'.
		       (vc-file-setprop file 'vc-checkout-model 'manual)

		     ;; If the permissions say "locked", we can only trust
		     ;; this *if* the checkout model is `manual'.
		     (eq (vc-checkout-model file) 'manual)))

	      ;; Otherwise, use lock information from the master file.
	      (vc-file-setprop file 'vc-locking-user
			       (vc-master-locking-user file)))

	  ;; Finally, if the file is not explicitly locked
	  ;; it might still be locked implicitly.
	  (and (eq (vc-file-getprop file 'vc-locking-user) 'none)
	       (eq (vc-checkout-model file) 'implicit)
	       (vc-rcs-lock-from-diff file))))

      ((eq (vc-backend file) 'SCCS)
       (or (vc-lock-from-permissions file)
	   (vc-file-setprop file 'vc-locking-user 
			    (vc-master-locking-user file)))))
  
      ;; convert a possible 'none value
      (setq locking-user (vc-file-getprop file 'vc-locking-user))
      (if (eq locking-user 'none) nil locking-user))))

;;; properties to store current and recent version numbers

(defun vc-latest-version (file)
  ;; Return version level of the latest version of FILE
  (cond ((vc-file-getprop file 'vc-latest-version))
	(t (vc-fetch-properties file)
	   (vc-file-getprop file 'vc-latest-version))))

(defun vc-your-latest-version (file)
  ;; Return version level of the latest version of FILE checked in by you
  (cond ((vc-file-getprop file 'vc-your-latest-version))
	(t (vc-fetch-properties file)
	   (vc-file-getprop file 'vc-your-latest-version))))

(defun vc-master-workfile-version (file)
  ;; Return the master file's idea of what is the current workfile version.
  ;; This property is defined for RCS only.
  (cond ((vc-file-getprop file 'vc-master-workfile-version))
	(t (vc-fetch-master-properties file)
	   (vc-file-getprop file 'vc-master-workfile-version))))

(defun vc-fetch-properties (file)
  ;; Fetch vc-latest-version and vc-your-latest-version
  ;; if that wasn't already done.
  (cond
   ((eq (vc-backend file) 'RCS)
    (save-excursion
      (set-buffer (get-buffer-create "*vc-info*"))
      (vc-insert-file (vc-name file) "^desc")
      (vc-parse-buffer 
       (list '("^\\([0-9]+\\.[0-9.]+\\)\ndate[ \t]+\\([0-9.]+\\);" 1 2)
	     (list (concat "^\\([0-9]+\\.[0-9.]+\\)\n"
			   "date[ \t]+\\([0-9.]+\\);[ \t]+"
			   "author[ \t]+"
			   (regexp-quote (vc-user-login-name)) ";") 1 2))
       file
       '(vc-latest-version vc-your-latest-version))
      (if (get-buffer "*vc-info*")
	  (kill-buffer (get-buffer "*vc-info*")))))
   (t (vc-fetch-master-properties file))
   ))

(defun vc-workfile-version (file)
  ;; Return version level of the current workfile FILE
  ;; This is attempted by first looking at the RCS keywords.
  ;; If there are no keywords in the working file, 
  ;; vc-master-workfile-version is taken.
  ;; Note that this property is cached, that is, it is only 
  ;; looked up if it is nil.
  ;; For SCCS, this property is equivalent to vc-latest-version.
  (cond ((vc-file-getprop file 'vc-workfile-version))
	((eq (vc-backend file) 'SCCS) (vc-latest-version file))
	((eq (vc-backend file) 'RCS)
	 (if (vc-consult-rcs-headers file)
	     (vc-file-getprop file 'vc-workfile-version)
	   (let ((rev (cond ((vc-master-workfile-version file))
			    ((vc-latest-version file)))))
	     (vc-file-setprop file 'vc-workfile-version rev)
	     rev)))
	((eq (vc-backend file) 'CVS)
	 (if (vc-consult-rcs-headers file)   ;; CVS
	     (vc-file-getprop file 'vc-workfile-version)
	   (catch 'found
	     (vc-find-cvs-master (file-name-directory file)
				 (file-name-nondirectory file)))
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

(defun vc-sccs-project-dir () 
  ;; Return the full pathname of the SCCS PROJECTDIR, if it exists,
  ;; otherwise nil.  The PROJECTDIR is indicated by the environment
  ;; variable of the same name.  If its value starts with a slash,
  ;; it must be an absolute path name that points to the 
  ;; directory where SCCS history files reside.  If it does not
  ;; begin with a slash, it is taken as the name of a user,
  ;; and history files reside in an "src" or "source" subdirectory
  ;; of that user's home directory.
  (let ((project-dir (getenv "PROJECTDIR")))
    (and project-dir
         (if (eq (elt project-dir 0) ?/)
             (if (file-exists-p (concat project-dir "/SCCS"))
                 (concat project-dir "/SCCS/")
               (if (file-exists-p project-dir)
                   project-dir))
           (setq project-dir (expand-file-name (concat "~" project-dir)))
           (let (trial)
             (setq trial (concat project-dir "/src/SCCS"))
             (if (file-exists-p trial)
                 (concat trial "/")
               (setq trial (concat project-dir "/src"))
               (if (file-exists-p trial)
                   (concat trial "/")
                 (setq trial (concat project-dir "/source/SCCS"))
                 (if (file-exists-p trial)
                     (concat trial "/")
                   (setq trial (concat project-dir "/source/"))
                   (if (file-exists-p trial)
                       (concat trial "/"))))))))))

(defun vc-search-sccs-project-dir (dirname basename)
  ;; Check if there is a master file for BASENAME in the 
  ;; SCCS project directory.  If yes, throw `found' as
  ;; expected by vc-registered.  If not, return nil.
  (let* ((project-dir (vc-sccs-project-dir))
         (master-file (and project-dir (concat project-dir "s." basename))))
    (and master-file
         (file-exists-p master-file)
         (throw 'found (cons master-file 'SCCS)))))

(defun vc-find-cvs-master (dirname basename)
  ;; Check if DIRNAME/BASENAME is handled by CVS.
  ;; If it is, do a (throw 'found (cons MASTER-FILE 'CVS)).
  ;; Note: This function throws the name of CVS/Entries
  ;; NOT that of the RCS master file (because we wouldn't be able
  ;; to access it under remote CVS).
  ;; The function returns nil if DIRNAME/BASENAME is not handled by CVS.
  (if (and vc-handle-cvs
	   (file-directory-p (concat dirname "CVS/"))
	   (file-readable-p (concat dirname "CVS/Entries")))
      (let ((file (concat dirname basename))
            buffer)
	(unwind-protect
	    (save-excursion
	      (setq buffer (set-buffer (get-buffer-create "*vc-info*")))
	      (vc-insert-file (concat dirname "CVS/Entries"))
	      (goto-char (point-min))
	      ;; make sure that the file name is searched 
	      ;; case-sensitively - case-fold-search is a buffer-local
	      ;; variable, so setting it here won't affect any other buffers
	      (setq case-fold-search nil)
	      (cond
	       ;; entry for a "locally added" file (not yet committed)
	       ((re-search-forward
		 (concat "^/" (regexp-quote basename) "/0/") nil t)
		(vc-file-setprop file 'vc-checkout-time 0)
		(vc-file-setprop file 'vc-workfile-version "0")
		(throw 'found (cons (concat dirname "CVS/Entries") 'CVS)))
	       ;; normal entry
	       ((re-search-forward
		 (concat "^/" (regexp-quote basename) 
                         ;; revision
                         "/\\([^/]*\\)" 
                         ;; timestamp
                         "/[A-Z][a-z][a-z]"       ;; week day (irrelevant)
                         " \\([A-Z][a-z][a-z]\\)" ;; month name
                         " *\\([0-9]*\\)"         ;; day of month
                         " \\([0-9]*\\):\\([0-9]*\\):\\([0-9]*\\)"  ;; hms
                         " \\([0-9]*\\)"          ;; year
                         ;; optional conflict field
                         "\\(+[^/]*\\)?/")
		 nil t)
		;; We found it.  Store away version number now that we 
		;; are anyhow so close to finding it.
		(vc-file-setprop file
				 'vc-workfile-version
				 (match-string 1))
		;; If the file hasn't been modified since checkout,
		;; store the checkout-time.
		(let ((mtime (nth 5 (file-attributes file)))
		      (second (string-to-number (match-string 6)))
		      (minute (string-to-number (match-string 5)))
		      (hour (string-to-number (match-string 4)))
		      (day (string-to-number (match-string 3)))
		      (year (string-to-number (match-string 7))))
		  (if (equal mtime
			     (encode-time
			      second minute hour day
			      (/ (string-match
				  (match-string 2)
				  "xxxJanFebMarAprMayJunJulAugSepOctNovDec")
				 3)
			      year 0))
		      (vc-file-setprop file 'vc-checkout-time mtime)
		    (vc-file-setprop file 'vc-checkout-time 0)))
		(throw 'found (cons (concat dirname "CVS/Entries") 'CVS)))
               ;; entry with arbitrary text as timestamp
               ;; (this means we should consider it modified)
	       ((re-search-forward
		 (concat "^/" (regexp-quote basename) 
                         ;; revision
                         "/\\([^/]*\\)" 
                         ;; timestamp (arbitrary text)
                         "/[^/]*"
                         ;; optional conflict field
                         "\\(+[^/]*\\)?/")
		 nil t)
		;; We found it.  Store away version number now that we 
		;; are anyhow so close to finding it.
		(vc-file-setprop file 'vc-workfile-version (match-string 1))
		(vc-file-setprop file 'vc-checkout-time 0)
		(throw 'found (cons (concat dirname "CVS/Entries") 'CVS)))
	       (t nil)))
	  (kill-buffer buffer)))))

(defun vc-buffer-backend ()
  "Return the version-control type of the visited file, or nil if none."
  (if (eq vc-buffer-backend t)
      (setq vc-buffer-backend (vc-backend (buffer-file-name)))
    vc-buffer-backend))

(defun vc-toggle-read-only (&optional verbose)
  "Change read-only status of current buffer, perhaps via version control.
If the buffer is visiting a file registered with version control,
then check the file in or out.  Otherwise, just change the read-only flag
of the buffer.
With prefix argument, ask for version number to check in or check out.
Check-out of a specified version number does not lock the file;
to do that, use this command a second time with no argument."
  (interactive "P")
  (if (or (and (boundp 'vc-dired-mode) vc-dired-mode)
          ;; use boundp because vc.el might not be loaded
          (vc-backend (buffer-file-name)))
      (vc-next-action verbose)
    (toggle-read-only)))
(define-key global-map "\C-x\C-q" 'vc-toggle-read-only)

(defun vc-after-save ()
  ;; Function to be called by basic-save-buffer (in files.el).
  ;; If the file in the current buffer is under version control,
  ;; not locked, and the checkout model for it is `implicit',
  ;; mark it "locked" and redisplay the mode line.
  (let ((file (buffer-file-name)))
    (and (vc-backend file)
	 (or (and (equal (vc-file-getprop file 'vc-checkout-time)
			 (nth 5 (file-attributes file)))
		  ;; File has been saved in the same second in which
		  ;; it was checked out.  Clear the checkout-time
		  ;; to avoid confusion.
		  (vc-file-setprop file 'vc-checkout-time nil))
	     t)
	 (not (vc-locking-user file))
	 (eq (vc-checkout-model file) 'implicit)
	 (vc-file-setprop file 'vc-locking-user (vc-user-login-name))
	 (or (and (eq (vc-backend file) 'CVS) 
		  (vc-file-setprop file 'vc-cvs-status nil))
	     t)
	 (vc-mode-line file))))

(defun vc-mode-line (file &optional label)
  "Set `vc-mode' to display type of version control for FILE.
The value is set in the current buffer, which should be the buffer
visiting FILE.  Second optional arg LABEL is put in place of version
control system name."
  (interactive (list buffer-file-name nil))
  (let ((vc-type (vc-backend file)))
    (setq vc-mode
	  (and vc-type
	       (concat " " (or label (symbol-name vc-type)) 
		       (and vc-display-status (vc-status file)))))
    ;; If the file is locked by some other user, make
    ;; the buffer read-only.  Like this, even root
    ;; cannot modify a file that someone else has locked.
    (and vc-type 
	 (equal file (buffer-file-name))
	 (vc-locking-user file)
	 (not (string= (vc-user-login-name) (vc-locking-user file)))
	 (setq buffer-read-only t))
    ;; If the user is root, and the file is not owner-writable,
    ;; then pretend that we can't write it
    ;; even though we can (because root can write anything).
    ;; This way, even root cannot modify a file that isn't locked.
    (and vc-type
	 (equal file (buffer-file-name))
	 (not buffer-read-only)
	 (zerop (user-real-uid))
	 (zerop (logand (file-modes (buffer-file-name)) 128))
	 (setq buffer-read-only t))
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
  ;; The file is "locked" from the moment when the user saves
  ;; the modified buffer.
  ;; 
  ;; This function assumes that the file is registered.

  (let ((locker (vc-locking-user file))
	(rev (vc-workfile-version file)))
    (cond ((string= "0" rev)
	   " @@")
	  ((not locker)
	   (concat "-" rev))
	  ((string= locker (vc-user-login-name))
	   (concat ":" rev))
	  (t 
	   (concat ":" locker ":" rev)))))

(defun vc-follow-link ()
  ;; If the current buffer visits a symbolic link, this function makes it
  ;; visit the real file instead.  If the real file is already visited in 
  ;; another buffer, make that buffer current, and kill the buffer 
  ;; that visits the link.
  (let* ((truename (abbreviate-file-name (file-chase-links buffer-file-name)))
         (true-buffer (find-buffer-visiting truename))
	 (this-buffer (current-buffer)))
    (if (eq true-buffer this-buffer)
	(progn
	  (kill-buffer this-buffer)
	  ;; In principle, we could do something like set-visited-file-name.
	  ;; However, it can't be exactly the same as set-visited-file-name.
	  ;; I'm not going to work out the details right now. -- rms.
	  (set-buffer (find-file-noselect truename)))
      (set-buffer true-buffer)
      (kill-buffer this-buffer))))

;;; install a call to the above as a find-file hook
(defun vc-find-file-hook ()
  ;; Recompute whether file is version controlled,
  ;; if user has killed the buffer and revisited.
  (cond 
   ((and (not vc-ignore-vc-files) buffer-file-name)
    (vc-file-clearprops buffer-file-name)
    (cond
     ((vc-backend buffer-file-name)
      (vc-mode-line buffer-file-name)
      (cond ((not vc-make-backup-files)
	     ;; Use this variable, not make-backup-files,
	     ;; because this is for things that depend on the file name.
	     (make-local-variable 'backup-inhibited)
	     (setq backup-inhibited t))))
     ((let* ((link (file-symlink-p buffer-file-name))
	     (link-type (and link (vc-backend (file-chase-links link)))))
	(if link-type
            (cond ((eq vc-follow-symlinks nil)
                   (message
        "Warning: symbolic link to %s-controlled source file" link-type))
                  ((or (not (eq vc-follow-symlinks 'ask))
		       ;; If we already visited this file by following
		       ;; the link, don't ask again if we try to visit
		       ;; it again.  GUD does that, and repeated questions
		       ;; are painful.
		       (get-file-buffer
			(abbreviate-file-name (file-chase-links buffer-file-name))))
		       
		   (vc-follow-link)
		   (message "Followed link to %s" buffer-file-name)
		   (vc-find-file-hook))
                  (t
                   (if (yes-or-no-p (format
        "Symbolic link to %s-controlled source file; follow link? " link-type))
                       (progn (vc-follow-link)
                              (message "Followed link to %s" buffer-file-name)
                              (vc-find-file-hook))
                     (message 
        "Warning: editing through the link bypasses version control")
                     ))))))))))

(add-hook 'find-file-hooks 'vc-find-file-hook)

;;; more hooks, this time for file-not-found
(defun vc-file-not-found-hook ()
  "When file is not found, try to check it out from RCS or SCCS.
Returns t if checkout was successful, nil otherwise."
  ;; When a file does not exist, ignore cached info about it
  ;; from a previous visit.
  (vc-file-clearprops buffer-file-name)
  (if (and (not vc-ignore-vc-files) 
           (vc-backend buffer-file-name))
      (save-excursion
	(require 'vc)
	(setq default-directory (file-name-directory (buffer-file-name)))
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
      (define-key vc-prefix-map "g" 'vc-annotate)
      (define-key vc-prefix-map "h" 'vc-insert-headers)
      (define-key vc-prefix-map "i" 'vc-register)
      (define-key vc-prefix-map "l" 'vc-print-log)
      (define-key vc-prefix-map "m" 'vc-merge)
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
  (define-key vc-menu-map [vc-retrieve-snapshot]
    '("Retrieve Snapshot" . vc-retrieve-snapshot))
  (define-key vc-menu-map [vc-create-snapshot]
    '("Create Snapshot" . vc-create-snapshot))
  (define-key vc-menu-map [vc-directory] '("VC Directory Listing" . vc-directory))
  (define-key vc-menu-map [separator1] '("----"))
  (define-key vc-menu-map [vc-annotate] '("Annotate" . vc-annotate))
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
  (define-key vc-menu-map [vc-next-action] '("Check In/Out" . vc-next-action))
  (define-key vc-menu-map [vc-register] '("Register" . vc-register)))

;;; These are not correct and it's not currently clear how doing it
;;; better (with more complicated expressions) might slow things down
;;; on older systems.

;;;(put 'vc-rename-file 'menu-enable 'vc-mode)
;;;(put 'vc-annotate 'menu-enable '(eq (vc-buffer-backend) 'CVS))
;;;(put 'vc-version-other-window 'menu-enable 'vc-mode)
;;;(put 'vc-diff 'menu-enable 'vc-mode)
;;;(put 'vc-update-change-log 'menu-enable
;;;     '(eq (vc-buffer-backend) 'RCS))
;;;(put 'vc-print-log 'menu-enable 'vc-mode)
;;;(put 'vc-cancel-version 'menu-enable 'vc-mode)
;;;(put 'vc-revert-buffer 'menu-enable 'vc-mode)
;;;(put 'vc-insert-headers 'menu-enable 'vc-mode)
;;;(put 'vc-next-action 'menu-enable 'vc-mode)
;;;(put 'vc-register 'menu-enable '(and buffer-file-name (not vc-mode)))

(provide 'vc-hooks)

;;; vc-hooks.el ends here
