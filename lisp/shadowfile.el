;;; shadowfile.el -- automatic file copying for Emacs 19

;;; Author: Boris Goldowsky <boris@gnu.ai.mit.edu>
;;; Keywords: comm

;;; Copyright (c) 1993, 1994 Free Software Foundation
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author or from the Free Software Foundation, Inc., 675
;;; Mass Ave, Cambridge, MA 02139, USA.

;;; LCD Archive Entry:
;;; shadowfile|Boris Goldowsky|boris@gnu.ai.mit.edu|
;;; Helps you keep identical copies of files in multiple places.|
;;; $Date: 1994/05/03 22:57:18 $ |$Revision: 1.3 $|~/misc/shadowfile.el.Z|

;;; Commentary:
;;;
;;;  This package helps you to keep identical copies of files in more than one
;;;  place - possibly on different machines.  When you save a file, it checks
;;;  whether it is on the list of files with "shadows", and if so, it tries to
;;;  copy it when you exit emacs (or use the shadow-copy-files command).

;;; Installation & Use:
;;;
;;;      Put (require 'shadowfile) in your .emacs; add clusters (if necessary)
;;;  and file groups with shadow-define-cluster, shadow-define-literal-group,
;;;  and shadow-define-regexp-group (see the documentation for these functions
;;;  for information on how and when to use them).  After doing this once,
;;;  everything should be automatic.
;;;      The lists of clusters and shadows are saved in a file called
;;;  .shadows, so that they can be remembered from one emacs session to
;;;  another, even (as much as possible) if the emacs session terminates
;;;  abnormally.  The files needing to be copied are stored in .shadow_todo; if
;;;  a file cannot be copied for any reason, it will stay on the list to be
;;;  tried again next time.  The .shadows file should itself have shadows on
;;;  all your accounts so that the information in it is consistent everywhere,
;;;  but .shadow_todo is local information and should have no shadows.
;;;     If you do not want to copy a particular file, you can answer "no"
;;;  and be asked again next time you hit C-x 4 s or exit emacs.  If you do not
;;;  want to be asked again, use shadow-cancel, and you will not be asked until
;;;  you change the file and save it again.  If you do not want to shadow
;;;  that file ever again, you can edit it out of the .shadows buffer.
;;;  Anytime you edit the .shadows buffer, you must type M-x shadow-read-files
;;;  to load in the new information, or your changes will be overwritten!

;;; Bugs & Warnings:
;;;
;;;  - It is bad to have two emacses both running shadowfile at the same
;;;  time.  It tries to detect this condition, but is not always successful.
;;;
;;;  - You have to be careful not to edit a file in two locations
;;;  before shadowfile has had a chance to copy it; otherwise
;;;  "updating shadows" will overwrite one of the changed versions.
;;;
;;;  - It ought to check modification times of both files to make sure
;;;  it is doing the right thing.  This will have to wait until
;;;  file-newer-than-file-p works between machines.
;;;
;;;  - It will not make directories for you, it just fails to copy files
;;;  that belong in non-existent directories.
;;;
;;;  Please report any bugs to me (boris@gnu.ai.mit.edu).  Also let me know
;;;  if you have suggestions or would like to be informed of updates.

;;; Code:

(provide 'shadowfile)
(require 'ange-ftp)

(setq find-file-visit-truename t)	; makes life easier with symbolic links

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar shadow-noquery nil
  "*If t, always copy shadow files without asking.
If nil \(the default), always ask.  If not nil and not t, ask only if there
is no buffer currently visiting the file.")

(defvar shadow-inhibit-message nil
  "*If nonnil, do not display a message when a file needs copying.")

(defvar shadow-inhibit-overload nil
  "If nonnil, shadowfile won't redefine C-x C-c.
Normally it overloads the function `save-buffers-kill-emacs' to check
for files have been changed and need to be copied to other systems.")

(defvar shadow-info-file nil
  "File to keep shadow information in.  
The shadow-info-file should be shadowed to all your accounts to
ensure consistency.  Default: ~/.shadows")

(defvar shadow-todo-file nil
  "File to store the list of uncopied shadows in.
This means that if a remote system is down, or for any reason you cannot or
decide not to copy your shadow files at the end of one emacs session, it will
remember and ask you again in your next emacs session.
This file must NOT be shadowed to any other system, it is host-specific.
Default: ~/.shadow_todo")

;;; The following two variables should in most cases initialize themselves
;;; correctly.  They are provided as variables in case the defaults are wrong
;;; on your machine \(and for efficiency).

(defvar shadow-system-name (system-name)
  "The complete hostname of this machine.")

(defvar shadow-homedir nil
  "Your home directory on this machine.")

;;;
;;; Internal variables whose values are stored in the info and todo files:
;;;

(defvar shadow-clusters nil
  "List of host clusters \(see shadow-define-cluster).")

(defvar shadow-literal-groups nil
  "List of files that are shared between hosts.
This list contains shadow structures with literal filenames, created by
shadow-define-group.")

(defvar shadow-regexp-groups nil
  "List of file types that are shared between hosts.
This list contains shadow structures with regexps matching filenames, 
created by shadow-define-regexp-group.")

;;;
;;; Other internal variables:
;;;

(defvar shadow-files-to-copy nil)	; List of files that need to
					; be copied to remote hosts.

(defvar shadow-hashtable nil)		; for speed

(defvar shadow-info-buffer nil)		; buf visiting shadow-info-file
(defvar shadow-todo-buffer nil)		; buf visiting shadow-todo-file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntactic sugar; General list and string manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro shadow-when (condition &rest body)
  ;; From cl.el
  "(shadow-when CONDITION . BODY) => evaluate BODY if CONDITION is true."
  (` (if (not (, condition))  ()  (,@ body))))
  
(defun shadow-union (a b)
  "Add members of list A to list B
if they are not equal to items already in B."
  (if (null a)
      b
    (if (member (car a) b)
	(shadow-union (cdr a) b)
      (shadow-union (cdr a) (cons (car a) b)))))

(defun shadow-find (func list)
  "If FUNC applied to some element of LIST is nonnil, 
return the first such element."
  (while (and list (not (funcall func (car list))))
    (setq list (cdr list)))
  (car list))

(defun shadow-remove-if (func list)
  "Remove elements satisfying FUNC from LIST.
Nondestructive; actually returns a copy of the list with the elements removed."
  (if list
      (if (funcall func (car list))
	  (shadow-remove-if func (cdr list))
	(cons (car list) (shadow-remove-if func (cdr list))))
    nil))

(defun shadow-join (strings sep)
  "Concatenate elements of the list of STRINGS with SEP between each."
  (cond ((null strings) "")
	((null (cdr strings)) (car strings))
	((concat (car strings) " " (shadow-join (cdr strings) sep)))))

(defun shadow-regexp-superquote (string)
  "Like regexp-quote, but includes the ^ and $ 
to make sure regexp matches nothing but STRING."
  (concat "^" (regexp-quote string) "$"))

(defun shadow-suffix (prefix string)
  "If PREFIX begins STRING, return the rest.
Return value is nonnil if PREFIX and STRING are string= up to the length of
PREFIX."
  (let ((lp (length prefix))
	(ls (length string)))
    (if (and (>= ls lp)
	     (string= prefix (substring string 0 lp)))
	(substring string lp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clusters and sites
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; I use the term `site' to refer to a string which may be the name of a
;;; cluster or a literal hostname.  All user-level commands should accept
;;; either.

(defun shadow-make-cluster (name primary regexp)
  "Creates a shadow cluster 
called NAME, using the PRIMARY hostname, REGEXP matching all hosts in the
cluster.  The variable shadow-clusters associates the names of clusters to
these structures. 
   This function is for program use: to create clusters interactively, use
shadow-define-cluster instead."
  (list name primary regexp))

(defmacro shadow-cluster-name (cluster)
  "Return the name of the CLUSTER."
  (list 'elt cluster 0))

(defmacro shadow-cluster-primary (cluster)
  "Return the primary hostname of a CLUSTER."
  (list 'elt cluster 1))

(defmacro shadow-cluster-regexp (cluster)
  "Return the regexp matching hosts in a CLUSTER."
  (list 'elt cluster 2))

(defun shadow-set-cluster (name primary regexp)
  "Put cluster NAME on the list of clusters,
replacing old definition if any.  PRIMARY and REGEXP are the
information defining the cluster.  For interactive use, call
shadow-define-cluster instead."
  (let ((rest (shadow-remove-if
	       (function (lambda (x) (equal name (car x))))
	       shadow-clusters)))
    (setq shadow-clusters 
	  (cons (shadow-make-cluster name primary regexp)
		rest))))

(defmacro shadow-get-cluster (name)
  "Return cluster named NAME, or nil."
  (list 'assoc name 'shadow-clusters))

(defun shadow-site-primary (site)
  "If SITE is a cluster, return primary host, otherwise return SITE."
  (let ((c (shadow-get-cluster site)))
    (if c
	(shadow-cluster-primary c)
      site)))

;;; SITES

(defun shadow-site-cluster (site)
  "Given a SITE \(hostname or cluster name), return the cluster
that it is in, or nil."
  (or (assoc site shadow-clusters)
      (shadow-find
       (function (lambda (x)
		   (string-match (shadow-cluster-regexp x)
				 site)))
       shadow-clusters)))

(defun shadow-read-site ()
  "Read a cluster name or hostname from the minibuffer."
  (let ((ans (completing-read "Host or cluster name [RET when done]: "
			      shadow-clusters)))
    (if (equal "" ans)
	nil
      ans)))

(defun shadow-site-match (site1 site2)
  "Nonnil iff SITE1 is or includes SITE2.  
Each may be a host or cluster name; if they are clusters, regexp of site1 will
be matched against the primary of site2."
  (or (string-equal site1 site2) ; quick check
      (let* ((cluster1 (shadow-get-cluster site1))
	     (primary2 (shadow-site-primary site2)))
	(if cluster1
	    (string-match (shadow-cluster-regexp cluster1) primary2)
	  (string-equal site1 primary2)))))

(defun shadow-get-user (site)
  "Returns the default username for a site."
  (ange-ftp-get-user (shadow-site-primary site)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filename manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shadow-parse-fullpath (fullpath)
  "Parse PATH into \(site user path) list,
or leave it alone if it already is one.  Returns nil if the argument is not a
full ange-ftp pathname."
  (if (listp fullpath)
      fullpath
    (ange-ftp-ftp-name fullpath)))

(defun shadow-parse-path (path)
  "Parse any PATH into \(site user path) list.
Argument can be a simple path, full ange-ftp path, or already a hup list."
  (or (shadow-parse-fullpath path)
      (list shadow-system-name
	    (user-login-name)
	    path)))

(defsubst shadow-make-fullpath (host user path)
  "Make an ange-ftp style fullpath out of HOST, USER (optional), and PATH.
This is probably not as general as it ought to be."
  (concat "/" 
	  (if user (concat user "@"))
	  host ":"
	  path))

(defun shadow-replace-path-component (fullpath newpath)
  "Return FULLPATH with the pathname component changed to NEWPATH."
  (let ((hup (shadow-parse-fullpath fullpath)))
    (shadow-make-fullpath (nth 0 hup) (nth 1 hup) newpath)))

(defun shadow-local-file (file)
  "If FILENAME is at this site,
remove /user@host part.  If refers to a different system or a different user on
this system, return nil."
  (let ((hup (shadow-parse-fullpath file)))
    (cond ((null hup) file)
	  ((and (shadow-site-match (nth 0 hup) shadow-system-name)
		(string-equal (nth 1 hup) (user-login-name)))
	   (nth 2 hup))
	  (t nil))))

(defun shadow-expand-cluster-in-file-name (file)
  "If hostname part of FILE is a cluster, expand it
into the cluster's primary hostname.  Will return the pathname bare if it is
a local file."
  (let ((hup (shadow-parse-path file))
	cluster)
    (cond ((null hup) file)
	  ((shadow-local-file hup))
	  ((shadow-make-fullpath (shadow-site-primary (nth 0 hup))
				 (nth 1 hup)
				 (nth 2 hup))))))

(defun shadow-expand-file-name (file &optional default)
  "Expand file name and get file's true name."
  (file-truename (expand-file-name file default)))

(defun shadow-contract-file-name (file)
  "Simplify FILENAME
by replacing (when possible) home directory with ~, and hostname with cluster
name that includes it.  Filename should be absolute and true."
  (let* ((hup (shadow-parse-path file))
	 (homedir (if (shadow-local-file hup)
		      shadow-homedir
		    (file-name-as-directory
		     (nth 2 (shadow-parse-fullpath 
			     (expand-file-name
			      (shadow-make-fullpath
			       (nth 0 hup) (nth 1 hup) "~")))))))
	 (suffix (shadow-suffix homedir (nth 2 hup)))
	 (cluster (shadow-site-cluster (nth 0 hup))))
    (shadow-make-fullpath
     (if cluster
	 (shadow-cluster-name cluster)
       (nth 0 hup))
     (nth 1 hup)
     (if suffix 
	 (concat "~/" suffix)
       (nth 2 hup)))))

(defun shadow-same-site (pattern file)
  "True if the site of PATTERN and of FILE are on the same site.
If usernames are supplied, they must also match exactly.  PATTERN and FILE may
be lists of host, user, path, or ange-ftp pathnames.  FILE may also be just a
local filename."
  (let ((pattern-sup (shadow-parse-fullpath pattern))
	(file-sup    (shadow-parse-path file)))
    (and
     (shadow-site-match (nth 0 pattern-sup) (nth 0 file-sup))
     (or (null (nth 1 pattern-sup))
	 (string-equal (nth 1 pattern-sup) (nth 1 file-sup))))))

(defun shadow-file-match (pattern file &optional regexp)
 "Returns t if PATTERN matches FILE.
If REGEXP is supplied and nonnil, the pathname part of the pattern is a regular
expression, otherwise it must match exactly.  The sites and usernames must
match---see shadow-same-site.  The pattern must be in full ange-ftp format, but
the file can be any valid filename.  This function does not do any filename
expansion or contraction, you must do that yourself first."
 (let* ((pattern-sup (shadow-parse-fullpath pattern))
	(file-sup (shadow-parse-path file)))
   (and (shadow-same-site pattern-sup file-sup)
	(if regexp 
	    (string-match (nth 2 pattern-sup) (nth 2 file-sup))
	  (string-equal (nth 2 pattern-sup) (nth 2 file-sup))))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User-level Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shadow-define-cluster (name)
  "Edit \(or create) the definition of a cluster.
This is a group of hosts that share directories, so that copying to or from
one of them is sufficient to update the file on all of them.  Clusters are
defined by a name, the network address of a primary host \(the one we copy
files to), and a regular expression that matches the hostnames of all the sites
in the cluster."
  (interactive (list (completing-read "Cluster name: " shadow-clusters () ())))
  (let* ((old (shadow-get-cluster name))
	 (primary (read-string "Primary host: "
			       (if old (shadow-cluster-primary old) 
				 name)))
	 (regexp   (let (try-regexp)
		     (while (not
			     (string-match 
			      (setq try-regexp
				    (read-string 
				     "Regexp matching all host names: "
				     (if old (shadow-cluster-regexp old)
				       (shadow-regexp-superquote primary))))
			      primary))
		       (message "Regexp doesn't include the primary host!")
		       (sit-for 2))
		     try-regexp))
;	 (username (read-no-blanks-input 
;		    (format "Username [default: %s]: " 
;			    (shadow-get-user primary))
;		    (if old (or (shadow-cluster-username old) "")
;		      (user-login-name))))
	 )
;    (if (string-equal "" username) (setq username nil))
    (shadow-set-cluster name primary regexp)))

(defun shadow-define-literal-group ()
  "Declare a single file to be shared between sites.
It may have different filenames on each site.  When this file is edited, the
new version will be copied to each of the other locations.  Sites can be
specific hostnames, or names of clusters \(see shadow-define-cluster)."
  (interactive)
  (let* ((hup (shadow-parse-fullpath 
	       (shadow-contract-file-name (buffer-file-name))))
	 (path (nth 2 hup))
	 user site group)
    (while (setq site (shadow-read-site))
      (setq user (read-string (format "Username [default %s]: "
				      (shadow-get-user site)))
	    path (read-string "Filename: " path))
      (setq group (cons (shadow-make-fullpath site 
					      (if (string-equal "" user)
						  (shadow-get-user site)
						user)
					      path)
			group)))
    (setq shadow-literal-groups (cons group shadow-literal-groups)))
  (shadow-write-info-file))

(defun shadow-define-regexp-group ()
  "Make each of a group of files be shared between hosts.
Prompts for regular expression; files matching this are shared between a list
of sites, which are also prompted for. The filenames must be identical on all
hosts \(if they aren't, use shadow-define-group instead of this function).
Each site can be either a hostname or the name of a cluster \(see
shadow-define-cluster)."
  (interactive)
  (let ((regexp (read-string 
		 "Filename regexp: " 
		 (if (buffer-file-name)
		     (shadow-regexp-superquote
		      (nth 2
			   (shadow-parse-path
			    (shadow-contract-file-name
			     (buffer-file-name))))))))
	site sites usernames)
    (while (setq site (shadow-read-site))
      (setq sites (cons site sites))
      (setq usernames 
	    (cons (read-string (format "Username for %s: " site)
			       (shadow-get-user site))
		  usernames)))
    (setq shadow-regexp-groups 
	  (cons (shadow-make-group regexp sites usernames)
		shadow-regexp-groups))
    (shadow-write-info-file)))
    
(defun shadow-shadows ()
  ;; Mostly for debugging.
  "Interactive function to display shadows of a buffer."
  (interactive)
  (let ((msg (shadow-join (mapcar (function cdr)
				  (shadow-shadows-of (buffer-file-name)))
			  " ")))
    (message (if (zerop (length msg)) 
		 "No shadows."
	       msg))))

(defun shadow-copy-files (&optional arg)
  "Copy all pending shadow files.
With prefix argument, copy all pending files without query.
Pending copies are stored in variable shadow-files-to-copy, and in
shadow-todo-file if necessary.  This function is invoked by
shadow-save-buffers-kill-emacs, so it is not usually necessary to
call it manually."
  (interactive "P")
  (if (and (not shadow-files-to-copy) (interactive-p))
      (message "No files need to be shadowed.")
    (save-excursion
      (map-y-or-n-p (function
		     (lambda (pair)
		       (or arg shadow-noquery
			   (format "Copy shadow file %s? " (cdr pair)))))
		    (function shadow-copy-file)
		    shadow-files-to-copy
		    '("shadow" "shadows" "copy"))
      (shadow-write-todo-file t))))

(defun shadow-cancel ()
  "Cancel the instruction to copy some files.
Prompts for which copy operations to cancel.  You will not be asked to copy
them again, unless you make more changes to the files.  To cancel a shadow
permanently, remove the group from shadow-literal-groups or
shadow-regexp-groups."
  (interactive)
  (map-y-or-n-p (function (lambda (pair)
			    (format "Cancel copying %s to %s? " 
				    (car pair) (cdr pair))))
		(function (lambda (pair) 
			    (shadow-remove-from-todo pair)))
		shadow-files-to-copy
		'("shadow" "shadows" "cancel copy"))
  (message (format "There are %d shadows to be updated." 
		   (length shadow-files-to-copy)))
  (shadow-write-todo-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shadow-make-group (regexp sites usernames)
  "Makes a description of a file group---
actually a list of regexp ange-ftp file names---from REGEXP \(name of file to
be shadowed), list of SITES, and corresponding list of USERNAMES for each
site."
  (if sites
      (cons (shadow-make-fullpath (car sites) (car usernames) regexp)
	    (shadow-make-group regexp (cdr sites) (cdr usernames)))
    nil))

(defun shadow-copy-file (s)
  "Copy one shadow file."
  (let* ((buffer 
	  (cond ((get-file-buffer 
		  (abbreviate-file-name (shadow-expand-file-name (car s)))))
		((not (file-readable-p (car s)))
		 (if (y-or-n-p
		      (format "Cannot find file %s--cancel copy request?"
			      (car s)))
		     (shadow-remove-from-todo s))
		 nil)
		((or (eq t shadow-noquery)
		     (y-or-n-p 
		      (format "No buffer for %s -- update shadow anyway?"
			      (car s))))
		 (find-file-noselect (car s)))))
	 (to (shadow-expand-cluster-in-file-name (cdr s))))
    (shadow-when buffer
      (set-buffer buffer)
      (save-restriction
	(widen)
	(condition-case i 
	    (progn
	      (write-region (point-min) (point-max) to)
	      (shadow-remove-from-todo s))
	  (error (message (format "Shadow %s not updated!" 
				  (cdr s)))))))))

(defun shadow-shadows-of (file)
  "Returns copy operations needed to update FILE.
Filename should have clusters expanded, but otherwise can have any format.  
Return value is a list of dotted pairs like \(from . to), where from
and to are absolute file names."
  (or (symbol-value (intern-soft file shadow-hashtable))
      (let* ((absolute-file (shadow-expand-file-name
			     (or (shadow-local-file file) file)
			     shadow-homedir))
	     (canonical-file (shadow-contract-file-name absolute-file))
	     (shadows 
	      (mapcar (function (lambda (shadow)
				  (cons absolute-file shadow)))
		      (append
		       (shadow-shadows-of-1
			canonical-file shadow-literal-groups nil)
		       (shadow-shadows-of-1
			canonical-file shadow-regexp-groups t)))))
	(set (intern file shadow-hashtable) shadows))))

(defun shadow-shadows-of-1 (file groups regexp)
  "Return list of FILE's shadows in GROUPS, 
which are considered as regular expressions if third arg REGEXP is true."
  (if groups
      (let ((nonmatching
	     (shadow-remove-if 
	      (function (lambda (x) (shadow-file-match x file regexp)))
	      (car groups))))
	(append (cond ((equal nonmatching (car groups)) nil)
		      (regexp 
		       (let ((realpath (nth 2 (shadow-parse-fullpath file))))
			 (mapcar 
			  (function 
			   (lambda (x) 
			     (shadow-replace-path-component x realpath)))
			  nonmatching)))
		      (t nonmatching))
		(shadow-shadows-of-1 file (cdr groups) regexp)))))

(defun shadow-add-to-todo ()
  "If current buffer has shadows, add them to the list
of files needing to be copied."
  (let ((shadows (shadow-shadows-of 
		  (shadow-expand-file-name 
		   (buffer-file-name (current-buffer))))))
    (shadow-when shadows
      (setq shadow-files-to-copy
	    (shadow-union shadows shadow-files-to-copy))
      (shadow-when (not shadow-inhibit-message)
	(message (substitute-command-keys
		  "Use \\[shadow-copy-files] to update shadows."))
	(sit-for 1))
      (shadow-write-todo-file)))
  nil)     ; Return nil for write-file-hooks

(defun shadow-remove-from-todo (pair)
  "Remove PAIR from shadow-files-to-copy.
PAIR must be (eq to) one of the elements of that list."
  (setq shadow-files-to-copy 
	(shadow-remove-if (function (lambda (s) (eq s pair)))
			  shadow-files-to-copy)))

(defun shadow-read-files ()
  "Visits and loads shadow-info-file and shadow-todo-file,
thus restoring shadowfile's state from your last emacs session.
Returns t unless files were locked; then returns nil."
  (interactive)
  (if (and (fboundp 'file-locked-p)
	   (or (stringp (file-locked-p shadow-info-file))
	       (stringp (file-locked-p shadow-todo-file))))
      (progn
	(message "Shadowfile is running in another emacs; can't have two.")
	(beep)
	(sit-for 3)
	nil)
    (save-excursion
      (shadow-when shadow-info-file
	(set-buffer (setq shadow-info-buffer
			  (find-file-noselect shadow-info-file)))
	(shadow-when (and (not (buffer-modified-p))
			  (file-newer-than-file-p (make-auto-save-file-name)
						  shadow-info-file))
	  (erase-buffer)
	  (message "Data recovered from %s." 
		   (car (insert-file-contents (make-auto-save-file-name))))
	  (sit-for 1))
	(eval-current-buffer))
      (shadow-when shadow-todo-file
	(set-buffer (setq shadow-todo-buffer 
			  (find-file-noselect shadow-todo-file)))
	(shadow-when (and (not (buffer-modified-p))
			  (file-newer-than-file-p (make-auto-save-file-name)
						  shadow-todo-file))
	  (erase-buffer)
	  (message "Data recovered from %s." 
		   (car (insert-file-contents (make-auto-save-file-name))))
	  (sit-for 1))
	(eval-current-buffer nil))
      (shadow-invalidate-hashtable))
    t))

(defun shadow-write-info-file ()
  "Write out information to shadow-info-file.
Also clears shadow-hashtable, since when there are new shadows defined, the old
hashtable info is invalid."
  (shadow-invalidate-hashtable)
  (if shadow-info-file
      (save-excursion
	(if (not shadow-info-buffer)
	    (setq shadow-info-buffer (find-file-noselect shadow-info-file)))
	(set-buffer shadow-info-buffer)
	(delete-region (point-min) (point-max))
	(shadow-insert-var 'shadow-clusters)
	(shadow-insert-var 'shadow-literal-groups)
	(shadow-insert-var 'shadow-regexp-groups))))

(defun shadow-write-todo-file (&optional save)
  "Write out information to shadow-todo-file.  
With nonnil argument also saves the buffer."
  (save-excursion
    (if (not shadow-todo-buffer)
	(setq shadow-todo-buffer (find-file-noselect shadow-todo-file)))
    (set-buffer shadow-todo-buffer)
    (delete-region (point-min) (point-max))
    (shadow-insert-var 'shadow-files-to-copy)
    (if save (shadow-save-todo-file))))

(defun shadow-save-todo-file ()
  (if (and shadow-todo-buffer (buffer-modified-p shadow-todo-buffer))
      (save-excursion
	(set-buffer shadow-todo-buffer)
	(condition-case nil		; have to continue even in case of 
	    (basic-save-buffer)		; error, otherwise kill-emacs might
	  (error			; not work!
	   (message "WARNING: Can't save shadow todo file; it is locked!")
	   (sit-for 1))))))

(defun shadow-invalidate-hashtable ()
  (setq shadow-hashtable (make-vector 37 0)))

(defun shadow-insert-var (variable)
  "Prettily insert a setq command for VARIABLE.
which, when later evaluated, will restore it to its current setting.
SYMBOL must be the name of a variable whose value is a list."
  (let ((standard-output (current-buffer)))
    (insert (format "(setq %s" variable))
    (cond ((consp (eval variable))
	   (insert "\n  '(") 
	   (prin1 (car (eval variable)))
	   (let ((rest (cdr (eval variable))))
	     (while rest
	       (insert "\n    ")
	       (prin1 (car rest))
	       (setq rest (cdr rest)))
	     (insert "))\n\n")))
	  (t (insert " ")
	     (prin1 (eval variable))
	     (insert ")\n\n")))))

(defun shadow-save-buffers-kill-emacs (&optional arg)
  "Offer to save each buffer and copy shadows, then kill this Emacs process.
With prefix arg, silently save all file-visiting buffers, then kill.

Extended by shadowfile to automatically save `shadow-todo-file' and
look for files that have been changed and need to be copied to other systems."
  ;; This function is necessary because we need to get control and save
  ;; the todo file /after/ saving other files, but /before/ the warning
  ;; message about unsaved buffers (because it can get modified by the
  ;; action of saving other buffers).  `kill-emacs-hook' is no good
  ;; because it is not called at the correct time, and also because it is
  ;; called when the terminal is disconnected and we cannot ask whether
  ;; to copy files.
  (interactive "P")
  (shadow-save-todo-file)
  (save-some-buffers arg t)
  (shadow-copy-files)
  (shadow-save-todo-file)
  (and (or (not (memq t (mapcar (function
				 (lambda (buf) (and (buffer-file-name buf)
						    (buffer-modified-p buf))))
				(buffer-list))))
	   (yes-or-no-p "Modified buffers exist; exit anyway? "))
       (or (not (fboundp 'process-list))
	   ;; process-list is not defined on VMS.
	   (let ((processes (process-list))
		 active)
	     (while processes
	       (and (memq (process-status (car processes)) '(run stop open))
		    (let ((val (process-kill-without-query (car processes))))
		      (process-kill-without-query (car processes) val)
		      val)
		    (setq active t))
	       (setq processes (cdr processes)))
	     (or (not active)
		 (yes-or-no-p "Active processes exist; kill them and exit anyway? "))))
       (kill-emacs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lucid Emacs compatibility 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is on hold until someone tells me about a working version of
;; map-ynp for Lucid Emacs.

;(shadow-when (string-match "Lucid" emacs-version)
;  (require 'symlink-fix)
;  (require 'ange-ftp)
;  (require 'map-ynp)
;  (if (not (fboundp 'file-truename))
;      (fset 'shadow-expand-file-name 
;	    (symbol-function 'symlink-expand-file-name)))
;  (if (not (fboundp 'ange-ftp-ftp-name))
;      (fset 'ange-ftp-ftp-name
;	    (symbol-function 'ange-ftp-ftp-path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hook us up
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; File shadowing is activated at load time, unless this this file is
;;; being preloaded, in which case it is added to after-init-hook.
;;; Thanks to Richard Caley for this scheme.

(defun shadow-initialize ()
  (if (null shadow-homedir)
      (setq shadow-homedir
	    (file-name-as-directory (shadow-expand-file-name "~"))))
  (if (null shadow-info-file)
      (setq shadow-info-file 
	    (shadow-expand-file-name "~/.shadows")))
  (if (null shadow-todo-file)
      (setq shadow-todo-file 
	    (shadow-expand-file-name "~/.shadow_todo")))
  (if (not (shadow-read-files))
      (progn
	(message "Shadowfile information files not found - aborting")
	(beep)
	(sit-for 3))
    (shadow-when (and (not shadow-inhibit-overload)
		      (not (fboundp 'shadow-orig-save-buffers-kill-emacs)))
      (fset 'shadow-orig-save-buffers-kill-emacs 
	    (symbol-function 'save-buffers-kill-emacs))
      (fset 'save-buffers-kill-emacs
	    (symbol-function 'shadow-save-buffers-kill-emacs)))
    (add-hook 'write-file-hooks 'shadow-add-to-todo)
    (define-key ctl-x-4-map "s" 'shadow-copy-files)))

(if noninteractive
    (add-hook 'after-init-hook 'shadow-initialize)
  (shadow-initialize))

;;;Local Variables:
;;;eval:(put 'shadow-when 'lisp-indent-hook 1)
;;;End:

;;; shadowfile.el ends here
