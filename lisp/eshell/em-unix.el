;;; em-unix.el --- UNIX command aliases  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2026 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains implementations of several UNIX command in Emacs
;; Lisp, for several reasons:
;;
;;   1) it makes them available on all platforms where the Lisp
;;      functions used are available
;;
;;   2) it makes their functionality accessible and modified by the
;;      Lisp programmer.
;;
;;   3) it allows Eshell to refrain from having to invoke external
;;      processes for common operations.

;;; Code:

(require 'esh-mode)
(require 'pcomplete)

;;;###esh-module-autoload
(progn
(defgroup eshell-unix nil
  "This module defines many of the more common UNIX utilities as
aliases implemented in Lisp.  These include mv, ln, cp, rm, etc.  If
the user passes arguments which are too complex, or are unrecognized
by the Lisp variant, the external version will be called (if
available).  The only reason not to use them would be because they are
usually much slower.  But in several cases their tight integration
with Eshell makes them more versatile than their traditional cousins
\(such as being able to use `kill' to kill Eshell background processes
by name)."
  :tag "UNIX commands in Lisp"
  :group 'eshell-module))

(defcustom eshell-unix-load-hook nil
  "A list of functions to run when `eshell-unix' is loaded."
  :version "24.1"			; removed eshell-unix-initialize
  :type 'hook
  :group 'eshell-unix)

(defcustom eshell-plain-grep-behavior nil
  "If non-nil, standalone \"grep\" commands will behave normally.
Standalone in this context means not redirected, and not on the
receiving side of a command pipeline."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-no-grep-available (not (eshell-search-path "grep"))
  "If non-nil, no grep is available on the current machine."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-plain-diff-behavior nil
  "If non-nil, standalone \"diff\" commands will behave normally.
Standalone in this context means not redirected, and not on the
receiving side of a command pipeline."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-plain-locate-behavior (featurep 'xemacs)
  "If non-nil, standalone \"locate\" commands will behave normally.
Standalone in this context means not redirected, and not on the
receiving side of a command pipeline."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-rm-removes-directories nil
  "If non-nil, `rm' will remove directory entries.
Otherwise, `rmdir' is required."
  :type 'boolean
  :group 'eshell-unix)

(define-widget 'eshell-interactive-query 'radio
  "When to interactively query the user about a particular operation.
If t, always query.  If nil, never query.  If `root', query when
the user is logged in as root (including when `default-directory'
is remote with a root user)."
  :args '((const :tag "Never" nil)
          (const :tag "Always" t)
          (const :tag "When root" root)))

(defcustom eshell-rm-interactive-query 'root
  "When `rm' should query before removing anything.
If t, always query.  If nil, never query.  If `root', query when
the user is logged in as root (including when `default-directory'
is remote with a root user)."
  :type 'eshell-interactive-query
  :group 'eshell-unix)

(defcustom eshell-mv-interactive-query 'root
  "When `mv' should query before overwriting anything.
If t, always query.  If nil, never query.  If `root', query when
the user is logged in as root (including when `default-directory'
is remote with a root user)."
  :type 'eshell-interactive-query
  :group 'eshell-unix)

(defcustom eshell-mv-overwrite-files t
  "If non-nil, `mv' will overwrite files without warning."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-cp-interactive-query 'root
  "When `cp' should query before overwriting anything.
If t, always query.  If nil, never query.  If `root', query when
the user is logged in as root (including when `default-directory'
is remote with a root user)."
  :type 'eshell-interactive-query
  :group 'eshell-unix)

(defcustom eshell-cp-overwrite-files t
  "If non-nil, `cp' will overwrite files without warning."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-ln-interactive-query 'root
  "When `ln' should query before overwriting anything.
If t, always query.  If nil, never query.  If `root', query when
the user is logged in as root (including when `default-directory'
is remote with a root user)."
  :type 'eshell-interactive-query
  :group 'eshell-unix)

(defcustom eshell-ln-overwrite-files nil
  "If non-nil, `ln' will overwrite files without warning."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-default-target-is-dot nil
  "If non-nil, the default destination for cp, mv or ln is `.'."
  :type 'boolean
  :group 'eshell-unix)

(defcustom eshell-du-prefer-over-ange nil
  "Use Eshell's du in ange-ftp remote directories.
Otherwise, Emacs will attempt to use rsh to invoke du on the remote machine."
  :type 'boolean
  :group 'eshell-unix)

;;; Functions:

(defun eshell-unix-initialize ()    ;Called from `eshell-mode' via intern-soft!
  "Initialize the UNIX support/emulation code."
  (when (eshell-using-module 'eshell-cmpl)
    (add-hook 'pcomplete-try-first-hook
	      'eshell-complete-host-reference nil t))
  (setq-local eshell-complex-commands
	      (append '("compile" "grep" "egrep" "fgrep" "agrep"
                        "rgrep" "glimpse" "locate" "cat" "time" "cp"
                        "mv" "make" "du" "diff")
		      eshell-complex-commands)))

(defalias 'eshell/date     'current-time-string)
(defalias 'eshell/basename 'file-name-nondirectory)
(defalias 'eshell/dirname  'file-name-directory)

(defvar em-interactive)
(defvar em-preview)
(defvar em-recursive)
(defvar em-verbose)

(defun eshell-interactive-query-p (value)
  "Return non-nil if a command should query the user according to VALUE.
If VALUE is nil, return nil (never query).  If `root', return
non-nil if the user is logged in as root (including when
`default-directory' is remote with a root user; see
`file-user-uid').  If VALUE is any other non-nil value, return
non-nil (always query)."
  (if (eq value 'root)
      (= (file-user-uid) 0)
    value))

(defun eshell/man (&rest args)
  "Invoke man, flattening the arguments appropriately."
  (funcall 'man (apply 'eshell-flatten-and-stringify args)))

(put 'eshell/man 'eshell-no-numeric-conversions t)

(defun eshell/info (&rest args)
  "Run the info command in-frame with the same behavior as command-line `info'.
For example:
  `info'           => goes to top info window
  `info arg1'      => IF arg1 is a file, then visits arg1
  `info arg1'      => OTHERWISE goes to top info window and then menu item arg1
  `info arg1 arg2' => does action for arg1 (either visit-file or menu-item) and
                      then menu item arg2
  etc."
  (eval-and-compile (require 'info))
  (let ((file (cond
                ((not (stringp (car args)))
                 nil)
                ((file-exists-p (expand-file-name (car args)))
                 (expand-file-name (car args)))
                ((file-exists-p (concat (expand-file-name (car args)) ".info"))
                 (concat (expand-file-name (car args)) ".info")))))

    ;; If the first arg is a file, then go to that file's Top node
    ;; Otherwise, go to the global directory
    (if file
      (progn
        (setq args (cdr args))
        (Info-find-node file "Top"))
      (Info-directory))

    ;; Treat all remaining args as menu references
    (while args
      (Info-menu (car args))
      (setq args (cdr args)))))

(defun eshell-remove-entries (files &optional toplevel)
  "Remove all of the given FILES, perhaps interactively."
  (while files
    (if (string-match "\\`\\.\\.?\\'"
		      (file-name-nondirectory (car files)))
	(if toplevel
	    (eshell-error "rm: cannot remove `.' or `..'\n"))
      (if (and (file-directory-p (car files))
	       (not (file-symlink-p (car files))))
	  (progn
	    (if em-verbose
		(eshell-printn (format-message "rm: removing directory `%s'"
					       (car files))))
	    (unless
		(or em-preview
		    (and em-interactive
			 (not (y-or-n-p
			       (format-message "rm: remove directory `%s'? "
					       (car files))))))
	      (eshell-funcalln 'delete-directory (car files) t t)))
	(if em-verbose
	    (eshell-printn (format-message "rm: removing file `%s'"
					   (car files))))
	(unless (or em-preview
		    (and em-interactive
			 (not (y-or-n-p
			       (format-message "rm: remove `%s'? "
					       (car files))))))
	  (eshell-funcalln 'delete-file (car files) t))))
    (setq files (cdr files))))

(defun eshell/rm (&rest args)
  "Implementation of rm in Lisp.
This is implemented to call either `delete-file', `kill-buffer',
`kill-process', or `unintern', depending on the nature of the
argument."
  (setq args (flatten-tree args))
  (eshell-eval-using-options
   "rm" args
   '((?h "help" nil nil "show this usage screen")
     (?f "force" nil force-removal "force removal")
     (?i "interactive" nil em-interactive "prompt before any removal")
     (?n "preview" nil em-preview "don't change anything on disk")
     (?r "recursive" nil em-recursive
	 "remove the contents of directories recursively")
     (?R nil nil em-recursive "(same)")
     (?v "verbose" nil em-verbose "explain what is being done")
     :preserve-args
     :external "rm"
     :show-usage
     :usage "[OPTION]... FILE...
Remove (unlink) the FILE(s).")
   (unless em-interactive
     (setq em-interactive (eshell-interactive-query-p
                           eshell-rm-interactive-query)))
   (if (and force-removal em-interactive)
       (setq em-interactive nil))
   (while args
     (let ((entry (if (stringp (car args))
		      (directory-file-name (car args))
		    (if (numberp (car args))
			(number-to-string (car args))
		      (car args)))))
       (cond
	((bufferp entry)
	 (if em-verbose
	     (eshell-printn (format-message "rm: removing buffer `%s'" entry)))
	 (unless (or em-preview
		     (and em-interactive
			  (not (y-or-n-p (format-message
					  "rm: delete buffer `%s'? "
					  entry)))))
	   (eshell-funcalln 'kill-buffer entry)))
	((eshell-processp entry)
	 (if em-verbose
	     (eshell-printn (format-message "rm: killing process `%s'" entry)))
	 (unless (or em-preview
		     (and em-interactive
			  (not (y-or-n-p (format-message
					  "rm: kill process `%s'? "
					  entry)))))
	   (eshell-funcalln 'kill-process entry)))
	((symbolp entry)
	 (if em-verbose
	     (eshell-printn (format-message
			     "rm: uninterning symbol `%s'" entry)))
	 (unless
	     (or em-preview
		 (and em-interactive
		      (not (y-or-n-p (format-message
				      "rm: unintern symbol `%s'? "
				      entry)))))
	   (eshell-funcalln 'unintern entry)))
	((stringp entry)
	 ;; -f should silently ignore missing files (bug#15373).
	 (unless (and force-removal
		      (not (file-exists-p entry)))
	   (if (and (file-directory-p entry)
		    (not (file-symlink-p entry)))
	       (if (or em-recursive
		       eshell-rm-removes-directories)
		   (if (or em-preview
			   (not em-interactive)
			   (y-or-n-p
			    (format-message "rm: descend into directory `%s'? "
					    entry)))
		     (eshell-remove-entries (list entry) t))
		 (eshell-error (format "rm: %s: is a directory\n" entry)))
	     (eshell-remove-entries (list entry) t))))))
     (setq args (cdr args)))
   nil))

(put 'eshell/rm 'eshell-no-numeric-conversions t)
(put 'eshell/rm 'eshell-filename-arguments t)

(defun eshell/mkdir (&rest args)
  "Implementation of mkdir in Lisp."
  (eshell-eval-using-options
   "mkdir" args
   '((?h "help" nil nil "show this usage screen")
     (?p "parents" nil em-parents "make parent directories as needed")
     :external "mkdir"
     :show-usage
     :usage "[OPTION] DIRECTORY...
Create the DIRECTORY(ies), if they do not already exist.")
   (while args
     (eshell-funcalln 'make-directory (car args) em-parents)
     (setq args (cdr args)))
   nil))

(put 'eshell/mkdir 'eshell-no-numeric-conversions t)
(put 'eshell/mkdir 'eshell-filename-arguments t)

(defun eshell/rmdir (&rest args)
  "Implementation of rmdir in Lisp."
  (eshell-eval-using-options
   "rmdir" args
   '((?h "help" nil nil "show this usage screen")
     :external "rmdir"
     :show-usage
     :usage "[OPTION] DIRECTORY...
Remove the DIRECTORY(ies), if they are empty.")
   (while args
     (eshell-funcalln 'delete-directory (car args))
     (setq args (cdr args)))
   nil))

(put 'eshell/rmdir 'eshell-no-numeric-conversions t)
(put 'eshell/rmdir 'eshell-filename-arguments t)

(defvar no-dereference)

(defvar eshell-warn-dot-directories t)

(defun eshell-shuffle-files (command action files target func deep &rest args)
  "Shuffle around some filesystem entries, using FUNC to do the work."
  (let ((attr-target (eshell-file-attributes target))
	(is-dir (or (file-directory-p target)
		    (and em-preview (not eshell-warn-dot-directories))))
	attr)
    (if (and (not em-preview) (not is-dir)
	     (> (length files) 1))
	(error "%s: when %s multiple files, last argument must be a directory"
	       command action))
    (while files
      (setcar files (directory-file-name (car files)))
      (cond
       ((string-match "\\`\\.\\.?\\'"
		      (file-name-nondirectory (car files)))
	(if eshell-warn-dot-directories
	    (eshell-error (format "%s: %s: omitting directory\n"
				  command (car files)))))
       ((and attr-target
	     (or (not (eshell-under-windows-p))
		 (eq system-type 'ms-dos))
	     (setq attr (eshell-file-attributes (car files)))
	     (file-attribute-inode-number attr-target)
	     (file-attribute-inode-number attr)
	     (file-attribute-device-number attr-target)
	     (file-attribute-device-number attr)
	     (equal (file-attribute-file-identifier attr-target)
		    (file-attribute-file-identifier attr)))
	(eshell-error (format-message "%s: `%s' and `%s' are the same file\n"
				      command (car files) target)))
       (t
	(let ((source (car files))
	      (target (if is-dir
			  (expand-file-name
			   (file-name-nondirectory (car files)) target)
			target))
	      link)
	  (if (and (file-directory-p source)
		   (or (not no-dereference)
		       (not (file-symlink-p source)))
		   (not (memq func '(make-symbolic-link
				     add-name-to-file))))
	      (if (and (eq func 'copy-file)
		       (not em-recursive))
		  (eshell-error (format "%s: %s: omitting directory\n"
					command (car files)))
		(let (eshell-warn-dot-directories)
		  (if (and (not deep)
			   (eq func 'rename-file)
			   (equal (file-attribute-device-number
				   (eshell-file-attributes
				    (file-name-directory
				     (directory-file-name
				      (expand-file-name source)))))
				  (file-attribute-device-number
				   (eshell-file-attributes
				    (file-name-directory
				     (directory-file-name
				      (expand-file-name target)))))))
		      (apply 'eshell-funcalln func source target args)
		  (unless (file-directory-p target)
		    (if em-verbose
			(eshell-printn
			 (format "%s: making directory %s"
				 command target)))
		    (unless em-preview
		      (eshell-funcalln 'make-directory target)))
		  (apply 'eshell-shuffle-files
			 command action
			 (mapcar
                          (lambda (file)
                            (concat source "/" file))
			  (directory-files source))
			 target func t args)
		  (when (eq func 'rename-file)
		    (if em-verbose
			(eshell-printn
			 (format "%s: deleting directory %s"
				 command source)))
		    (unless em-preview
		      (eshell-funcalln 'delete-directory source))))))
	    (if em-verbose
		(eshell-printn (format "%s: %s -> %s" command
				       source target)))
	    (unless em-preview
	      (if (and no-dereference
		       (setq link (file-symlink-p source)))
		  (progn
		    (apply 'eshell-funcalln 'make-symbolic-link
			   link target
                           ;; `make-symbolic-link' doesn't have
                           ;; KEEP-TIME; just OK-IF-ALREADY-EXISTS.
                           (list (car args)))
		    (if (eq func 'rename-file)
			(if (and (file-directory-p source)
				 (not (file-symlink-p source)))
			    (eshell-funcalln 'delete-directory source)
			  (eshell-funcalln 'delete-file source))))
		(apply 'eshell-funcalln func source target args)))))))
      (setq files (cdr files)))))

(defun eshell-shorthand-tar-command (command args)
  "Rewrite `cp -v dir a.tar.gz' to `tar cvzf a.tar.gz dir'."
  (let* ((archive (car (last args)))
	 (tar-args
	  (cond ((string-match "z2" archive) "If")
		((string-match "gz" archive) "zf")
		((string-match "\\(az\\|Z\\)" archive) "Zf")
		(t "f"))))
    (if (file-exists-p archive)
	(setq tar-args (concat "u" tar-args))
      (setq tar-args (concat "c" tar-args)))
    (if em-verbose
	(setq tar-args (concat "v" tar-args)))
    (if (equal command "mv")
	(setq tar-args (concat "--remove-files -" tar-args)))
    ;; truncate the archive name from the arguments
    (setcdr (last args 2) nil)
    (throw 'eshell-replace-command
	   (eshell-parse-command
	    (format "tar %s %s" tar-args archive) args))))

;; this is to avoid duplicating code...
(defmacro eshell-mvcpln-template (command action func query-var
					  force-var &optional preserve)
  `(let ((len (length args)))
     (if (or (= len 0)
	     (and (= len 1) (null eshell-default-target-is-dot)))
	 (error "%s: missing destination file or directory" ,command))
     (if (= len 1)
	 (nconc args '(".")))
     (setq args (eshell-stringify-list (flatten-tree args)))
     (if (and ,(not (equal command "ln"))
	      (string-match eshell-tar-regexp (car (last args)))
	      (or (> (length args) 2)
		  (and (file-directory-p (car args))
		       (or (not no-dereference)
			   (not (file-symlink-p (car args)))))))
	 (eshell-shorthand-tar-command ,command args)
       (let ((target (car (last args))))
	 (setcdr (last args 2) nil)
	 (eshell-shuffle-files
	  ,command ,action args target ,func nil
	  ,@(append
	     `((if (and (or em-interactive
			    ,query-var)
			(not force))
		   1 (or force ,force-var)))
	     (if preserve
		 (list preserve)))))
       nil)))

(defun eshell/mv (&rest args)
  "Implementation of mv in Lisp."
  (eshell-eval-using-options
   "mv" args
   '((?f "force" nil force
	 "remove existing destinations, never prompt")
     (?i "interactive" nil em-interactive
	 "request confirmation if target already exists")
     (?n "preview" nil em-preview
	 "don't change anything on disk")
     (?v "verbose" nil em-verbose
	 "explain what is being done")
     (nil "help" nil nil "show this usage screen")
     :preserve-args
     :external "mv"
     :show-usage
     :usage "[OPTION]... SOURCE DEST
   or: mv [OPTION]... SOURCE... DIRECTORY
Rename SOURCE to DEST, or move SOURCE(s) to DIRECTORY.
[OPTION] DIRECTORY...")
   (let ((no-dereference t))
     (eshell-mvcpln-template "mv" "moving" 'rename-file
                             (eshell-interactive-query-p
                              eshell-mv-interactive-query)
			     eshell-mv-overwrite-files))))

(put 'eshell/mv 'eshell-no-numeric-conversions t)
(put 'eshell/mv 'eshell-filename-arguments t)

(defun eshell/cp (&rest args)
  "Implementation of cp in Lisp."
  (eshell-eval-using-options
   "cp" args
   '((?a "archive" nil archive
	 "same as -dpR")
     (?d "no-dereference" nil no-dereference
	 "preserve links")
     (?f "force" nil force
	 "remove existing destinations, never prompt")
     (?i "interactive" nil em-interactive
	 "request confirmation if target already exists")
     (?n "preview" nil em-preview
	 "don't change anything on disk")
     (?p "preserve" nil preserve
	 "preserve file attributes if possible")
     (?r "recursive" nil em-recursive
	 "copy directories recursively")
     (?R nil nil em-recursive
	 "as for -r")
     (?v "verbose" nil em-verbose
	 "explain what is being done")
     (nil "help" nil nil "show this usage screen")
     :preserve-args
     :external "cp"
     :show-usage
     :usage "[OPTION]... SOURCE DEST
   or: cp [OPTION]... SOURCE... DIRECTORY
Copy SOURCE to DEST, or multiple SOURCE(s) to DIRECTORY.")
   (if archive
       (setq preserve t no-dereference t em-recursive t))
   (eshell-mvcpln-template "cp" "copying" 'copy-file
                           (eshell-interactive-query-p
                            eshell-cp-interactive-query)
			   eshell-cp-overwrite-files preserve)))

(put 'eshell/cp 'eshell-no-numeric-conversions t)
(put 'eshell/cp 'eshell-filename-arguments t)

(defun eshell/ln (&rest args)
  "Implementation of ln in Lisp."
  (eshell-eval-using-options
   "ln" args
   '((?h "help" nil nil "show this usage screen")
     (?s "symbolic" nil symbolic
	 "make symbolic links instead of hard links")
     (?i "interactive" nil em-interactive
	 "request confirmation if target already exists")
     (?f "force" nil force "remove existing destinations, never prompt")
     (?n "preview" nil em-preview
	 "don't change anything on disk")
     (?v "verbose" nil em-verbose "explain what is being done")
     :preserve-args
     :external "ln"
     :show-usage
     :usage "[OPTION]... TARGET LINK_NAME
   or: ln [OPTION]... TARGET... DIRECTORY
Create a link to the specified TARGET with LINK_NAME.  If there is more
than one TARGET, the last argument must be a directory;  create links in
DIRECTORY to each TARGET.  Create hard links by default, symbolic links
with `--symbolic'.  When creating hard links, each TARGET must exist.")
   (let ((no-dereference t))
     (eshell-mvcpln-template "ln" "linking"
			     (if symbolic
				 'make-symbolic-link
			       'add-name-to-file)
                             (eshell-interactive-query-p
                              eshell-ln-interactive-query)
			     eshell-ln-overwrite-files))))

(put 'eshell/ln 'eshell-no-numeric-conversions t)
(put 'eshell/ln 'eshell-filename-arguments t)

(defun eshell/cat (&rest args)
  "Implementation of cat in Lisp.
If in a pipeline, or the file is not a regular file, directory or
symlink, then revert to the system's definition of cat."
  (setq args (eshell-stringify-list (flatten-tree args)))
  (if (or eshell-in-pipeline-p
	  (catch 'special
	    (dolist (arg args)
	      (unless (or (and (stringp arg)
			       (> (length arg) 0)
			       (eq (aref arg 0) ?-))
			  (let ((attrs (eshell-file-attributes arg)))
			    (and attrs
				 (memq (aref (file-attribute-modes attrs) 0)
					     '(?d ?l ?-)))))
		(throw 'special t)))))
      (let ((ext-cat (eshell-search-path "cat")))
	(if ext-cat
	    (throw 'eshell-external (eshell-external-command ext-cat args))
	  (if eshell-in-pipeline-p
	      (error "Eshell's `cat' does not work in pipelines")
	    (error "Eshell's `cat' cannot display one of the files given"))))
    (eshell-eval-using-options
     "cat" args
     '((?h "help" nil nil "show this usage screen")
       :external "cat"
       :show-usage
       :usage "[OPTION] FILE...
Concatenate FILE(s), or standard input, to standard output.")
     (dolist (file args)
       (if (string= file "-")
	   (throw 'eshell-external
		  (eshell-external-command "cat" args))))
     (let ((curbuf (current-buffer)))
       (eshell-with-buffered-print
         (dolist (file args)
	   (with-temp-buffer
	     (insert-file-contents file)
	     (goto-char (point-min))
             (while (not (eobp))
               (let* ((pos (min (+ (point) eshell-buffered-print-size)
                                (point-max)))
                      (str (buffer-substring (point) pos)))
                 (with-current-buffer curbuf
                   (eshell-buffered-print str))
                 (goto-char pos))))))))))

(put 'eshell/cat 'eshell-no-numeric-conversions t)
(put 'eshell/cat 'eshell-filename-arguments t)

;; special front-end functions for compilation-mode buffers

(defun eshell-compile (command args &optional method mode)
  "Run an external COMMAND with ARGS using a compilation buffer when possible.
COMMAND should be a list of command-line arguments.  By default,
if the command is outputting to the screen and is not part of a
pipeline or subcommand, open an compilation buffer to hold the
results; otherwise, write the output on stdout.

If METHOD is `interactive', always open a compilation buffer.  If
METHOD is `plain', always write to stdout.

MODE, if specified, is the major mode to set in the compilation
buffer (see `compilation-start')."
  (if (and (not (eq method 'interactive))
           (or (eq method 'plain)
               eshell-in-pipeline-p
               eshell-in-subcommand-p
               (not (eshell-interactive-output-p))))
      (throw 'eshell-replace-command
              (eshell-parse-command (concat "*" command) args))
    (compile
     (mapconcat #'shell-quote-argument
                (eshell-stringify-list (flatten-tree (cons command args)))
                " ")
     mode)))

(defun eshell/compile (&rest args)
  "Run an external COMMAND using a compilation buffer when possible.
See `eshell-compile'."
  (eshell-eval-using-options
   "compile" args
   '((?m "mode" t mode "the mode to set in the compilation buffer")
     (?i "interactive" 'interactive method "always open a compilation buffer")
     (?p "plain" 'plain method "always write to stdout")
     :usage "[-p | -i] [-m MODE] COMMAND...
Run COMMAND in a compilation buffer when outputting to the screen and
not part of a pipeline or subcommand."
     :parse-leading-options-only)
   (when (stringp mode)
     (setq mode (intern mode)))
   (eshell-compile (car args) (cdr args) method mode)))

(put 'eshell/compile 'eshell-no-numeric-conversions t)

(defun eshell/make (&rest args)
  "Use `compile' to do background makes.
Fallback to standard make when called synchronously."
  (eshell-compile "make" args
                  ;; Use plain output unless we're executing in the
                  ;; background.
                  (unless eshell-current-subjob-p 'plain)))

(put 'eshell/make 'eshell-no-numeric-conversions t)

(defun eshell-occur-mode-goto-occurrence ()
  "Go to the occurrence the current line describes."
  (interactive)
  (let ((pos (occur-mode-find-occurrence)))
    (pop-to-buffer (marker-buffer pos))
    (goto-char (marker-position pos))))

(defun eshell-occur-mode-mouse-goto (event)
  "In Occur mode, go to the occurrence whose line you click on."
  (interactive "e")
  (let (pos)
    (with-current-buffer (window-buffer (posn-window (event-end event)))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq pos (occur-mode-find-occurrence))))
    (pop-to-buffer (marker-buffer pos))
    (goto-char (marker-position pos))))

(defun eshell-poor-mans-grep (args)
  "A poor version of grep that opens every file and uses `occur'.
This eats up memory, since it leaves the buffers open (to speed future
searches), and it's very slow.  But, if your system has no grep
available..."
  (save-selected-window
    (let ((default-dir default-directory))
      (with-current-buffer (get-buffer-create "*grep*")
	(let ((inhibit-read-only t)
	      (default-directory default-dir))
	  (erase-buffer)
	  (occur-mode)
	  (let ((files (eshell-stringify-list
			(flatten-tree (cdr args))))
		(inhibit-redisplay t)
		string)
	    (when (car args)
	      (if (get-buffer "*Occur*")
		  (kill-buffer (get-buffer "*Occur*")))
	      (setq string nil)
	      (while files
		(with-current-buffer (find-file-noselect (car files))
		  (save-excursion
		    (ignore-errors
		      (occur (car args))))
		  (if (get-buffer "*Occur*")
		      (with-current-buffer "*Occur*"
			(setq string (buffer-string))
			(kill-buffer (current-buffer)))))
		(if string (insert string))
		(setq string nil
		      files (cdr files)))))
	  (local-set-key [mouse-2] 'eshell-occur-mode-mouse-goto)
	  (local-set-key [(control ?c) (control ?c)]
			 'eshell-occur-mode-goto-occurrence)
	  (local-set-key [(control ?m)]
			 'eshell-occur-mode-goto-occurrence)
	  (local-set-key [return] 'eshell-occur-mode-goto-occurrence)
	  (pop-to-buffer (current-buffer) t)
	  (goto-char (point-min))
	  (resize-temp-buffer-window))))))

(defvar compilation-scroll-output)

(defun eshell-grep (command args &optional maybe-use-occur)
  "Generic service function for the various grep aliases.
It calls Emacs's grep utility if the command is not redirecting output,
and if it's not part of a command pipeline.  Otherwise, it calls the
external command."
  (if (and maybe-use-occur eshell-no-grep-available)
      (eshell-poor-mans-grep args)
    (eshell-compile command (cons "-n" args)
                    (when eshell-plain-grep-behavior
                      'plain)
                     #'grep-mode)))

(defun eshell/grep (&rest args)
  "Use Emacs grep facility instead of calling external grep."
  (eshell-grep "grep" (append '("-H") args) t))

(defun eshell/egrep (&rest args)
  "Use Emacs grep facility instead of calling external grep -E."
  (eshell-grep "grep" (append '("-EH") args) t))

(defun eshell/fgrep (&rest args)
  "Use Emacs grep facility instead of calling external grep -F."
  (eshell-grep "grep" (append '("-FH") args) t))

(defun eshell/agrep (&rest args)
  "Use Emacs grep facility instead of calling external agrep."
  (eshell-grep "agrep" args))

(defun eshell/rgrep (&rest args)
  "Use Emacs grep facility instead of calling external rgrep."
  (eshell-grep "grep" (append '("-rH") args) t))

(defun eshell/glimpse (&rest args)
  "Use Emacs grep facility instead of calling external glimpse."
  (eshell-grep "glimpse" (append '("-z" "-y") args)))

;; completions rules for some common UNIX commands

(autoload 'pcmpl-unix-complete-hostname "pcmpl-unix")
(define-obsolete-function-alias 'eshell-complete-hostname
  #'pcmpl-unix-complete-hostname "28.1")

(defun eshell-complete-host-reference ()
  "If there is a host reference, complete it."
  (let ((arg (pcomplete-actual-arg)))
    (when (string-match
           (rx ;; Match an "@", but not immediately following a "$".
               (or string-start (not "$")) "@"
               (group (* (any "a-z.")))
               string-end)
           arg)
      (setq pcomplete-stub (substring arg (match-beginning 1))
	    pcomplete-last-completion-raw t)
      (throw 'pcomplete-completions (pcomplete-read-host-names)))))

(cl-defun eshell-du-sum-directory (path depth-remaining &rest args
                                   &key print-function show-all
                                   dereference-links only-one-filesystem
                                   seen-files)
  "Summarize PATH, and its member directories."
  (let ((size 0.0))
    (dolist (entry (eshell-directory-files-and-attributes path))
      (unless (or (string-match "\\`\\.\\.?\\'" (car entry))
                  (gethash (file-attribute-file-identifier (cdr entry))
                           seen-files))
        (puthash (file-attribute-file-identifier (cdr entry)) t seen-files)
        (let* ((file-name (concat path "/" (car entry)))
               (file-type (file-attribute-type (cdr entry)))
               (symlink (and (stringp file-type) file-type)))
	  (unless (or (and symlink (not dereference-links))
		      (and only-one-filesystem
			   (/= only-one-filesystem
                               (file-attribute-device-number (cdr entry)))))
            (when symlink
              (setq file-name symlink))
	    (setq size
		  (+ size
                     (if (eq file-type t) ; This is a directory.
                         (apply #'eshell-du-sum-directory file-name
                                (when depth-remaining (1- depth-remaining))
                                args)
		       (let ((file-size (file-attribute-size (cdr entry))))
                         (when show-all
                           (funcall print-function file-size file-name))
                         file-size))))))))
    (when (or (not depth-remaining)
              (natnump depth-remaining))
      (funcall print-function size (directory-file-name path)))
    size))

(defun eshell/du (&rest args)
  "Implementation of \"du\" in Lisp, passing ARGS."
  (let ((original-args args))
    (eshell-eval-using-options
     "du" args
     '((?a "all" nil show-all
           "write counts for all files, not just directories")
       (nil "block-size" t block-size
            "use SIZE-byte blocks (i.e., --block-size SIZE)")
       (?b "bytes" 1 block-size
           "print size in bytes")
       (?c "total" nil grand-total
           "produce a grand total")
       (?d "max-depth" t max-depth
           "display data only this many levels of data")
       (?h "human-readable" 1024 human-readable
           "print sizes in human readable format")
       (?H "si" 1000 human-readable
           "likewise, but use powers of 1000 not 1024")
       (?k "kilobytes" 1024 block-size
           "like --block-size 1024")
       (?L "dereference" nil dereference-links
           "dereference all symbolic links")
       (?m "megabytes" 1048576 block-size
           "like --block-size 1048576")
       (?s "summarize" 0 max-depth
           "display only a total for each argument")
       (?x "one-file-system" nil only-one-filesystem
           "skip directories on different filesystems")
       (nil "help" nil nil
            "show this usage screen")
       :external "du"
       :usage "[OPTION]... FILE...
Summarize disk usage of each FILE, recursively for directories.")
     ;; If possible, use the external "du" command.
     (when-let* (((not (seq-some
                        (lambda (i) (and (stringp i) (file-remote-p i)))
                        args)))
                 (ext-du (eshell-search-path "du")))
       (throw 'eshell-external (eshell-external-command ext-du original-args)))
     (setq block-size (or block-size 1024))
     (when (stringp block-size)
       (setq block-size (string-to-number block-size)))
     (when (stringp max-depth)
       (setq max-depth (string-to-number max-depth)))
     ;; Filesystem support means nothing under MS-Windows.
     (when (eshell-under-windows-p)
       (setq only-one-filesystem nil))
     (let ((size 0.0)
           (seen-files (make-hash-table :test #'equal))
           (print-function
            (lambda (size name)
              (let ((size-str (eshell-printable-size size human-readable
                                                     block-size t)))
                (eshell-print (concat (string-pad size-str 8) name "\n"))))))
       (dolist (arg (or args '(".")))
         (when only-one-filesystem
           (setq only-one-filesystem
                 (file-attribute-device-number
                  (eshell-file-attributes (file-name-as-directory arg)))))
         (setq size (+ size (eshell-du-sum-directory
                             (directory-file-name arg) max-depth
                             :print-function print-function :show-all show-all
                             :dereference-links dereference-links
                             :only-one-filesystem only-one-filesystem
                             :seen-files seen-files))))
       (when grand-total
         (funcall print-function size "total"))))))

(put 'eshell/du 'eshell-filename-arguments t)

(defvar eshell-time-start nil)

(defun eshell-show-elapsed-time ()
  (let ((elapsed (format "%.3f secs\n"
			 (float-time (time-since eshell-time-start)))))
    (set-text-properties 0 (length elapsed) '(face bold) elapsed)
    (eshell-interactive-print elapsed))
  (remove-hook 'eshell-post-command-hook 'eshell-show-elapsed-time t))

(defun eshell/time (&rest args)
  "Implementation of \"time\" in Lisp."
  (let ((time-args (copy-alist args))
	(continue t)
	last-arg)
    (while (and continue args)
      (if (not (string-match "^-" (car args)))
	  (progn
	    (if last-arg
		(setcdr last-arg nil)
	      (setq args '("")))
	    (setq continue nil))
	(setq last-arg args
	      args (cdr args))))
    (eshell-eval-using-options
     "time" args
     '((?h "help" nil nil "show this usage screen")
       :external "time"
       :show-usage
       :usage "COMMAND...
Show wall-clock time elapsed during execution of COMMAND.")
     (setq eshell-time-start (float-time))
     (add-hook 'eshell-post-command-hook 'eshell-show-elapsed-time nil t)
     ;; after setting
     (throw 'eshell-replace-command
	    (eshell-parse-command (car time-args)
;;; https://lists.gnu.org/r/bug-gnu-emacs/2007-08/msg00205.html
				  (eshell-stringify-list
				   (flatten-tree (cdr time-args))))))))

(defun eshell/whoami ()
  "Make \"whoami\" Tramp aware."
  (eshell-user-login-name))

(defun eshell-nil-blank-string (string)
  "Return STRING, or nil if STRING contains only blank characters."
  (cond
    ((string-match "[^[:blank:]]" string) string)
    (nil)))

(autoload 'diff-no-select "diff")

(defun eshell/diff (&rest args)
  "Alias \"diff\" to call Emacs `diff' function."
  (let ((orig-args (eshell-stringify-list (flatten-tree args))))
    (if (or eshell-plain-diff-behavior
	    (not (and (eshell-interactive-output-p)
		      (not eshell-in-pipeline-p)
		      (not eshell-in-subcommand-p))))
	(throw 'eshell-replace-command
	       (eshell-parse-command "*diff" orig-args))
      (setq args (copy-sequence orig-args))
      (if (< (length args) 2)
	  (throw 'eshell-replace-command
		 (eshell-parse-command "*diff" orig-args)))
      (let ((old (car (last args 2)))
            (new (car (last args))))
	(if (= (length args) 2)
	    (setq args nil)
	  (setcdr (last args 3) nil))
	(with-current-buffer
	    (condition-case nil
		(diff-no-select
		 old new
                 (eshell-nil-blank-string (eshell-flatten-and-stringify args)))
	      (error
	       (throw 'eshell-replace-command
		      (eshell-parse-command "*diff" orig-args))))
	  (pop-to-buffer (current-buffer))))))
  nil)

(put 'eshell/diff 'eshell-no-numeric-conversions t)
(put 'eshell/diff 'eshell-filename-arguments t)

(defvar locate-history-list)

(defun eshell/locate (&rest args)
  "Alias \"locate\" to call Emacs `locate' function."
  (if (or eshell-plain-locate-behavior
	  (not (and (eshell-interactive-output-p)
		    (not eshell-in-pipeline-p)
		    (not eshell-in-subcommand-p)))
	  (and (stringp (car args))
	       (string-match "^-" (car args))))
      (throw 'eshell-replace-command
	     (eshell-parse-command "*locate" (eshell-stringify-list
					      (flatten-tree args))))
    (save-selected-window
      (let ((locate-history-list (list (car args))))
	(locate-with-filter (car args) (cadr args))))))

(put 'eshell/locate 'eshell-no-numeric-conversions t)

(defun eshell/occur (&rest args)
  "Alias \"occur\" to call Emacs `occur' function."
  (let ((inhibit-read-only t))
    (if (> (length args) 2)
	(error "usage: occur: (REGEXP &optional NLINES)")
      (apply 'occur args))))

(put 'eshell/occur 'eshell-no-numeric-conversions t)

(define-obsolete-function-alias 'nil-blank-string #'eshell-nil-blank-string "29.1")
(defvar eshell-diff-window-config nil)
(make-obsolete-variable 'eshell-diff-window-config "no longer used." "30.1")
(define-obsolete-function-alias 'eshell-diff-quit #'ignore "30.1")

(provide 'em-unix)
;;; em-unix.el ends here
