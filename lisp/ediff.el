;;; ediff.el --- a comprehensive visual interface to diff & patch
;;; Copyright (C) 1994, 1995 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.sunysb.edu>
;; Created: February 2, 1994
;; Keywords: comparing, merging, patching, version control.

(defconst ediff-version "2.47" "The current version of Ediff")
(defconst ediff-date "October 11, 1995" "Date of last update")  

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
;;  ----------

;; Never read that diff output again!
;; Apply patch selectively, like a pro!
;; Merge with ease!

;; This package provides a convenient way of simultaneous browsing through
;; the differences between a pair (or a triple) of files or buffers.  The
;; files being compared, file-A, file-B, and file-C (if applicable) are
;; shown in separate windows (side by side, one above the another, or in
;; separate frames), and the differences are highlighted as you step
;; through them.  You can also copy difference regions from one buffer to
;; another (and recover old differences if you change your mind).

;; Ediff also supports merging operations on files and buffers, including
;; merging using ancestor versions. Both comparison and merging operations can
;; be performed on directories, i.e., by pairwise comparison of files in those
;; directories.

;; In addition, Ediff can apply a patch to a file and then let you step
;; though both files, the patched and the original one, simultaneously,
;; difference-by-difference.  You can even apply a patch right out of a
;; mail buffer, i.e., patches received by mail don't even have to be saved.
;; Since Ediff lets you copy differences between buffers, you can, in
;; effect, apply patches selectively (i.e., you can copy a difference
;; region from file_orig to file, thereby undoing any particular patch that
;; you don't like).

;; Ediff is aware of version control, which lets the user compare
;; files with their older versions. Ediff can also work with remote and
;; compressed files. Details are given below.

;; Finally, Ediff supports directory-level comparison and merging operations.
;; See the on-line manual for details.

;; This package builds upon the ideas borrowed from emerge.el and several
;; Ediff's functions are adaptations from emerge.el. Much of the functionality
;; Ediff provides is also influenced by emerge.el.

;; The present version of Ediff supersedes Emerge. It provides a superior user
;; interface and has numerous major features not found in Emerge. In
;; particular, it can do patching, and 2-way and 3-way file comparison,
;; merging, and directory operations.



;;; Bugs:
;;  -----

;;  1. The undo command doesn't restore deleted regions well. That is, if
;;  you delete all characters in a difference region and then invoke
;;  `undo', the reinstated text will most likely be inserted outside of
;;  what Ediff thinks is the current difference region. (This problem
;;  doesn't seem to exist with XEmacs.)
;;
;;  If at any point you feel that difference regions are no longer correct,
;;  you can hit '!' to recompute the differences.

;;  2. On a monochrome display, the repertoire of faces with which to
;;  highlight fine differences is limited. By default, Ediff is using
;;  underlining. However, if the region is already underlied by some other
;;  overlays, there is no simple way to temporarily remove that residual
;;  underlining. This problem occurs when a buffer is highlighted with
;;  hilit19.el or font-lock.el packages. If this residual highlighting gets
;;  in the way, you can do the following. Both font-lock.el and hilit19.el
;;  provide commands for unhighlighting buffers. You can either place these
;;  commands in `ediff-prepare-buffer-hook' (which will unhighlight every
;;  buffer used by Ediff) or you can execute them interactively, at any time
;;  and on any buffer.
;;


;;; Acknowledgements:

;; Ediff was inspired by Dale R. Worley's <drw@math.mit.edu> emerge.el.
;; Ediff would not have been possible without the help and encouragement of
;; its many users. See Ediff on-line Info for the full list of those who
;; helped. Improved defaults in Ediff file-name reading commands.


;;; Code:

(require 'ediff-init)
(require 'ediff-mult)

(defvar ediff-use-last-dir nil
  "*If t, Ediff uses previous directory as default when reading file name.")
  
(defvar ediff-last-dir-A nil
  "Last directory used by an Ediff command for file-A.")
(defvar ediff-last-dir-B nil
  "Last directory used by an Ediff command for file-B.")
(defvar ediff-last-dir-C nil
  "Last directory used by an Ediff command for file-C.")
(defvar ediff-last-dir-ancestor nil
  "Last directory used by an Ediff command for the ancestor file.")
(defvar ediff-last-dir-patch nil
  "Last directory used by an Ediff command for file to patch.")

;;; Patching

(defvar ediff-backup-extension 
  (if (memq system-type '(vax-vms axp-vms emx ms-dos windows-nt windows-95))
      "_orig" ".orig")
  "Default backup extension for the patch program.")

;;;###autoload
(defun ediff-patch-file (source-filename &optional startup-hooks job-name)
  "Run Ediff by patching FILE-TP-PATCH."
  ;; This now returns the control buffer
  (interactive 
   (list (ediff-read-file-name
	  "File to patch"
	  (if ediff-use-last-dir
	      ediff-last-dir-patch
	    default-directory)
	  (ediff-get-default-file-name))))
  
  (setq source-filename (expand-file-name source-filename))
  (ediff-get-patch-buffer
   (if (eq job-name 'ediff-patch-buffer)
       (ediff-eval-in-buffer (get-file-buffer source-filename)
	 default-directory)
     (file-name-directory source-filename)))
  
  (let* ((backup-extension 
	  ;; if the user specified a -b option, extract the backup
	  ;; extension from there; else use ediff-backup-extension
	  (substring ediff-patch-options
		     (if (string-match "-b[ \t]+" ediff-patch-options)
			 (match-end 0) 0)
		     (if (string-match "-b[ \t]+[^ \t]+" ediff-patch-options)
			 (match-end 0) 0)))
	 (shell-file-name ediff-shell)
	 ;; ediff-find-file may use a temp file to do the patch
	 ;; so, we save source-filename and true-source-filename as a var
	 ;; that initially is source-filename but may be changed to a temp
	 ;; file for the purpose of patching.
	 (true-source-filename source-filename)
	 (target-filename source-filename)
	 target-buf buf-to-patch file-name-magic-p ctl-buf)
	  
    ;; if the user didn't specify a backup extension, use
    ;; ediff-backup-extension 
    (if (string= backup-extension "")
	(setq backup-extension ediff-backup-extension))
					
    ;; Make a temp file, if source-filename has a magic file handler (or if
    ;; it is handled via auto-mode-alist and similar magic).
    ;; Check if there is a buffer visiting source-filename and if they are in
    ;; synch; arrange for the deletion of temp file.
    (ediff-find-file 'true-source-filename 'buf-to-patch
		     'ediff-last-dir-patch 'startup-hooks)

    ;; Check if source file name has triggered black magic, such as file name
    ;; handlers or auto mode alist, and make a note of it.
    ;; true-source-filename should be either the original name or a
    ;; temporary file where we put the after-product of the file handler.
    (setq file-name-magic-p (not (equal (file-truename true-source-filename)
					(file-truename source-filename))))
    
    ;; Checkout orig file, if necessary, so that the patched file could be
    ;; checked back in.
    (if (ediff-file-checked-in-p (buffer-file-name buf-to-patch))
	(ediff-toggle-read-only buf-to-patch))
    
    (ediff-eval-in-buffer ediff-patch-diagnostics
      (message "Applying patch ... ")
      ;;(sit-for 0)
      ;; always pass patch the -f option, so it won't ask any questions
      (shell-command-on-region 
       (point-min) (point-max)
       (format "%s -f %s -b %s %s"
	       ediff-patch-program ediff-patch-options
	       backup-extension
	       (expand-file-name true-source-filename))
       t))
    ;;(message "Applying patch ... done")(sit-for 0)
    (switch-to-buffer ediff-patch-diagnostics)
    (sit-for 0) ; synchronize - let the user see diagnostics
    
    (or (file-exists-p (concat true-source-filename backup-extension))
	(error "Patch failed or didn't modify the original file"))
  
    ;; If black magic is involved, apply patch to a temp copy of the
    ;; file. Otherwise, apply patch to the orig copy.  If patch is applied
    ;; to temp copy, we name the result old-name_patched for local files
    ;; and temp-copy_patched for remote files. The orig file name isn't
    ;; changed, and the temp copy of the original is later deleted.
    ;; Without magic, the original file is renamed (usually into
    ;; old-name_orig) and the result of patching will have the same name as
    ;; the original.
    (if (not file-name-magic-p)
	(ediff-eval-in-buffer buf-to-patch
	  (set-visited-file-name (concat source-filename backup-extension))
	  (set-buffer-modified-p nil))
      
      ;; Black magic in effect.
      ;; If orig file was remote, put the patched file in the temp directory.
      ;; If orig file is local, put the patched file in the directory of
      ;; the orig file.
      (setq target-filename
	    (concat
	     (if (ediff-file-remote-p (file-truename source-filename))
		 true-source-filename
	       source-filename)
	     "_patched"))
      
      (rename-file true-source-filename target-filename t)
      
      ;; arrange that the temp copy of orig will be deleted
      (rename-file (concat true-source-filename backup-extension)
		   true-source-filename t))
    
    ;; make orig buffer read-only
    (setq startup-hooks
	  (cons 'ediff-set-read-only-in-buf-A startup-hooks))
    
    ;; set up a buf for the patched file
    (setq target-buf (find-file-noselect target-filename))
    
    (setq ctl-buf
	  (ediff-buffers-internal
	   buf-to-patch target-buf nil
	   startup-hooks (or job-name 'ediff-patch-file)))
  
    (bury-buffer ediff-patch-diagnostics)
    (message "Patch diagnostics are available in buffer %s"
	     (buffer-name ediff-patch-diagnostics))
    ctl-buf))
  
(defun ediff-set-read-only-in-buf-A ()
  "Used as a startup hook to set `_orig' patch file read-only."
  (ediff-eval-in-buffer ediff-buffer-A
    (toggle-read-only 1)))

;; Return a plausible default for ediff's first file:
;; In dired, return the file name under the point, unless it is a directory
;; If the buffer has a file name, return that file name.
(defun ediff-get-default-file-name ()
  (cond ((eq major-mode 'dired-mode)
	 (let ((f (dired-get-filename nil 'no-error)))
	   (if (and (stringp f) (not (file-directory-p f)))
	       f)))
	((buffer-file-name (current-buffer))
	 (file-name-nondirectory (buffer-file-name (current-buffer))))
	))

;;;###autoload
(defalias 'epatch 'ediff-patch-file)
;;;###autoload
(defalias 'epatch-buffer 'ediff-patch-buffer)

;;; Compare files/buffers

;;;###autoload
(defun ediff-files (file-A file-B &optional startup-hooks)
  "Run Ediff on a pair of files, FILE-A and FILE-B."
  (interactive
   (let ((dir-A (if ediff-use-last-dir
		    ediff-last-dir-A
		  default-directory))
	 dir-B f)
     (list (setq f (ediff-read-file-name
		    "File A to compare" dir-A 
		    (ediff-get-default-file-name)))
	   (ediff-read-file-name "File B to compare" 
				 (setq dir-B
				       (if ediff-use-last-dir
					   ediff-last-dir-B 
					 (file-name-directory f)))
				 (progn
				   (setq file-name-history
					 (cons (ediff-abbreviate-file-name
						(expand-file-name
						 (file-name-nondirectory f)
						 dir-B))
					       file-name-history))
				   f))
	   )))
  (ediff-files-internal file-A 
			(if (file-directory-p file-B)
			    (expand-file-name
			     (file-name-nondirectory file-A) file-B)
			  file-B)
			nil ; file-C
			startup-hooks
			'ediff-files))
  
;;;###autoload
(defun ediff-files3 (file-A file-B file-C &optional startup-hooks)
  "Run Ediff on three files, FILE-A, FILE-B, and FILE-C."
  (interactive
   (let ((dir-A (if ediff-use-last-dir
		    ediff-last-dir-A
		  default-directory))
	 dir-B dir-C f ff)
     (list (setq f (ediff-read-file-name
		    "File A to compare" dir-A
		    (ediff-get-default-file-name)))
	   (setq ff (ediff-read-file-name "File B to compare" 
					  (setq dir-B
						(if ediff-use-last-dir
						    ediff-last-dir-B
						  (file-name-directory f)))
					  (progn
					    (setq file-name-history
						  (cons
						   (ediff-abbreviate-file-name
						    (expand-file-name
						     (file-name-nondirectory f)
						     dir-B))
						   file-name-history))
					    f)))
	   (ediff-read-file-name "File C to compare" 
				 (setq dir-C (if ediff-use-last-dir
						 ediff-last-dir-C
					       (file-name-directory ff)))
				 (progn
				   (setq file-name-history
					 (cons (ediff-abbreviate-file-name
						(expand-file-name
						 (file-name-nondirectory ff)
						 dir-C))
					       file-name-history))
				   ff))
	   )))
  (ediff-files-internal file-A 
			(if (file-directory-p file-B)
			    (expand-file-name
			     (file-name-nondirectory file-A) file-B)
			  file-B)
			(if (file-directory-p file-C)
			    (expand-file-name
			     (file-name-nondirectory file-A) file-C)
			  file-C)
			startup-hooks
			'ediff-files3))

;;;###autoload
(defalias 'ediff3 'ediff-files3)


(defun ediff-find-file (file-var buffer-name &optional last-dir hooks-var)
  "Visit FILE and arrange its buffer to Ediff's liking. 
FILE is actually a variable symbol that must contain a true file name.
BUFFER-NAME is a variable symbol, which will get the buffer object into which
FILE is read.  LAST-DIR is the directory variable symbol where FILE's
directory name should be returned. HOOKS is a variable symbol that will be
assigned the hook to be executed after `ediff-startup' is finished.
`ediff-find-file' arranges that the temp files it might create will be
deleted."
  (let* ((file (symbol-value file-var))
	 (file-magic (find-file-name-handler file 'find-file-noselect))
	 (temp-file-name-prefix (file-name-nondirectory file)))
    (cond ((not (file-readable-p file))
	   (error "File `%s' does not exist or is not readable" file))
	  ((file-directory-p file)
	   (error "File `%s' is a directory" file)))
	
    ;; some of the command, below, require full file name
    (setq file (expand-file-name file))
  
    ;; Record the directory of the file
    (if last-dir
	(set last-dir (expand-file-name (file-name-directory file))))
    
    ;; Setup the buffer
    (set buffer-name (find-file-noselect file))
  
    (ediff-eval-in-buffer (symbol-value buffer-name)
      (widen) ; Make sure the entire file is seen
      (cond (file-magic  ;; file has handler, such as jka-compr-handler or
	     ;; ange-ftp-hook-function--arrange for temp file
	     (ediff-verify-file-buffer 'magic)
	     (setq file
		   (ediff-make-temp-file
		    (current-buffer) temp-file-name-prefix))
	     (set hooks-var (cons (` (lambda () (delete-file (, file))))
				  (symbol-value hooks-var))))
	    ;; file processed via auto-mode-alist, a la uncompress.el
	    ((not (equal (file-truename file)
			 (file-truename (buffer-file-name))))
	     (setq file
		   (ediff-make-temp-file
		    (current-buffer) temp-file-name-prefix))
	     (set hooks-var (cons (` (lambda () (delete-file (, file))))
				  (symbol-value hooks-var))))
	    (t ;; plain file---just check that the file matches the buffer
	     (ediff-verify-file-buffer))))
    (set file-var file)))

(defun ediff-files-internal (file-A file-B file-C startup-hooks job-name)
  (let (buf-A buf-B buf-C)
    (message "Reading file %s ... " file-A)
    ;;(sit-for 0)
    (ediff-find-file 'file-A 'buf-A 'ediff-last-dir-A 'startup-hooks)
    (message "Reading file %s ... " file-B)
    ;;(sit-for 0)
    (ediff-find-file 'file-B 'buf-B 'ediff-last-dir-B 'startup-hooks)
    (if (stringp file-C)
	(progn
	  (message "Reading file %s ... " file-C)
	  ;;(sit-for 0)
	  (ediff-find-file
	   'file-C 'buf-C
	   (if (eq job-name 'ediff-merge-files-with-ancestor)
	       'ediff-last-dir-ancestor 'ediff-last-dir-C)
	   'startup-hooks)))
    (ediff-setup buf-A file-A
		 buf-B file-B
		 buf-C file-C
		 startup-hooks
		 (list (cons 'ediff-job-name job-name)))))
  

;;;###autoload
(defalias 'ediff 'ediff-files)


;;;###autoload
(defun ediff-buffers (buffer-A buffer-B &optional startup-hooks job-name)
  "Run Ediff on a pair of buffers, BUFFER-A and BUFFER-B."
  (interactive 
   (let (bf)
     (list (setq bf (read-buffer "Buffer A to compare: "
				 (ediff-other-buffer "") t))
	   (read-buffer "Buffer B to compare: "
			(progn
			  ;; realign buffers so that two visible bufs will be
			  ;; at the top
			  (save-window-excursion (other-window 1))
			  (ediff-other-buffer bf))
			t))))
  
  (or job-name (setq job-name 'ediff-buffers))
  (ediff-buffers-internal buffer-A buffer-B nil startup-hooks job-name))
      
;;;###autoload
(defun ediff-buffers3 (buffer-A buffer-B buffer-C
				 &optional startup-hooks job-name)
  "Run Ediff on three buffers, BUFFER-A, BUFFER-B, and BUFFER-C."
  (interactive 
   (let (bf bff)
     (list (setq bf (read-buffer "Buffer A to compare: "
				 (ediff-other-buffer "") t))
	   (setq bff (read-buffer "Buffer B to compare: "
				  (progn
				    ;; realign buffers so that two visible
				    ;; bufs will be at the top
				    (save-window-excursion (other-window 1))
				    (ediff-other-buffer bf))
				  t))
	   (read-buffer "Buffer C to compare: "
				  (progn
				    ;; realign buffers so that three visible
				    ;; bufs will be at the top
				    (save-window-excursion (other-window 1))
				    (ediff-other-buffer (list bf bff)))
				  t)
	   )))
  
  (or job-name (setq job-name 'ediff-buffers3))
  (ediff-buffers-internal buffer-A buffer-B buffer-C startup-hooks job-name))
      

			
(defun ediff-buffers-internal (buf-A buf-B buf-C startup-hooks job-name)
  (let* ((buf-A-file-name (buffer-file-name (get-buffer buf-A)))
	 (buf-B-file-name (buffer-file-name (get-buffer buf-B)))
	 (buf-C-is-alive (ediff-buffer-live-p buf-C))
	 (buf-C-file-name (if buf-C-is-alive
			      (buffer-file-name (get-buffer buf-B))))
	 file-A file-B file-C)
    (if (not (ediff-buffer-live-p buf-A))
	(error "Buffer %S doesn't exist" buf-A))
    (if (not (ediff-buffer-live-p buf-B))
	(error "Buffer %S doesn't exist" buf-B))
    (let ((ediff-job-name job-name))
      (if (and ediff-3way-comparison-job
	       (not buf-C-is-alive))
	  (error "Buffer %S doesn't exist" buf-C)))
    (if (stringp buf-A-file-name)
	(setq buf-A-file-name (file-name-nondirectory buf-A-file-name)))
    (if (stringp buf-B-file-name)
	(setq buf-B-file-name (file-name-nondirectory buf-B-file-name)))
    (if (stringp buf-C-file-name)
	(setq buf-C-file-name (file-name-nondirectory buf-C-file-name)))
	
    (setq file-A (ediff-make-temp-file buf-A buf-A-file-name))
    (setq file-B (ediff-make-temp-file buf-B buf-B-file-name))
    (if buf-C-is-alive
	(setq file-C (ediff-make-temp-file buf-C buf-C-file-name)))
	  
    (ediff-setup (get-buffer buf-A) file-A
		 (get-buffer buf-B) file-B
		 (if buf-C-is-alive (get-buffer buf-C))
		 file-C
		 (cons (` (lambda ()
			    (delete-file (, file-A))
			    (delete-file (, file-B))
			    (if (stringp (, file-C)) (delete-file (, file-C)))
			    ))
		       startup-hooks)
		 (list (cons 'ediff-job-name job-name))
		 )))


;;; Directory and file group operations

;; Get appropriate default name for directory:
;; If ediff-use-last-dir, use ediff-last-dir-A.
;; In dired mode, use the directory that is under the point (if any);
;; otherwise, use default-directory
(defun ediff-get-default-directory-name ()
  (cond (ediff-use-last-dir ediff-last-dir-A)
	((eq major-mode 'dired-mode)
	 (let ((f (dired-get-filename nil 'noerror)))
	   (if (and (stringp f) (file-directory-p f))
	       f
	     default-directory)))
	(t default-directory)))


;;;###autoload
(defun ediff-directories (dir1 dir2 regexp)
  "Run Ediff on a pair of directories, DIR1 and DIR2, comparing files that have
the same name in both. The third argument, REGEXP, is a regular expression that
further filters the file names."
  (interactive
   (let ((dir-A (ediff-get-default-directory-name))
	 f)
     (list (setq f (ediff-read-file-name "Directory A to compare" dir-A nil))
	   (ediff-read-file-name "Directory B to compare" 
				 (if ediff-use-last-dir
				     ediff-last-dir-B 
				   (ediff-strip-last-dir f))
				 nil)
	   (read-string "Filter through regular expression: "
			nil ediff-filtering-regexp-history)
	   )))
  (ediff-directories-internal
   dir1 dir2 nil regexp 'ediff-files 'ediff-directories
   ))

;;;###autoload
(defalias 'edirs 'ediff-directories)


;;;###autoload
(defun ediff-directory-revisions (dir1 regexp)
  "Run Ediff on a directory, DIR1, comparing its files with their revisions.
The second argument, REGEXP, is a regular expression that filters the file
names. Only the files that are under revision control are taken into account."
  (interactive
   (let ((dir-A (ediff-get-default-directory-name)))
     (list (ediff-read-file-name
	    "Directory to compare with revision" dir-A nil)
	   (read-string "Filter through regular expression: "
			nil ediff-filtering-regexp-history)
	   )))
  (ediff-directory-revisions-internal
   dir1 regexp 'ediff-revision 'ediff-directory-revisions
   ))

;;;###autoload
(defalias 'edir-revisions 'ediff-directory-revisions)


;;;###autoload
(defun ediff-directories3 (dir1 dir2 dir3 regexp)
  "Run Ediff on three directories, DIR1, DIR2, and DIR3, comparing files that
have the same name in all three. The last argument, REGEXP, is a regular
expression that further filters the file names."
  (interactive
   (let ((dir-A (ediff-get-default-directory-name))
	 f)
     (list (setq f (ediff-read-file-name "Directory A to compare" dir-A nil))
	   (setq f (ediff-read-file-name "Directory B to compare" 
					 (if ediff-use-last-dir
					     ediff-last-dir-B 
					   (ediff-strip-last-dir f))
					 nil))
	   (ediff-read-file-name "Directory C to compare" 
				 (if ediff-use-last-dir
				     ediff-last-dir-C 
				   (ediff-strip-last-dir f))
				 nil)
	   (read-string "Filter through regular expression: "
			nil ediff-filtering-regexp-history)
	   )))
  (ediff-directories-internal
   dir1 dir2 dir3 regexp 'ediff-files3 'ediff-directories3
   ))

;;;###autoload
(defalias 'edirs3 'ediff-directories3)

;;;###autoload
(defun ediff-merge-directories (dir1 dir2 regexp)
  "Run Ediff on a pair of directories, DIR1 and DIR2, merging files that have
the same name in both. The third argument, REGEXP, is a regular expression that
further filters the file names."
  (interactive
   (let ((dir-A (ediff-get-default-directory-name))
	 f)
     (list (setq f (ediff-read-file-name "Directory A to merge" dir-A nil))
	   (ediff-read-file-name "Directory B to merge" 
				 (if ediff-use-last-dir
				     ediff-last-dir-B 
				   (ediff-strip-last-dir f))
				 nil)
	   (read-string "Filter through regular expression: "
			nil ediff-filtering-regexp-history)
	   )))
  (ediff-directories-internal
   dir1 dir2 nil regexp 'ediff-merge-files 'ediff-merge-directories
   ))

;;;###autoload
(defalias 'edirs-merge 'ediff-merge-directories)

;;;###autoload
(defun ediff-merge-directories-with-ancestor (dir1 dir2 dir3 regexp)
  "Run Ediff on a pair of directories, DIR1 and DIR2, merging files that have
the same name in both. The third argument, REGEXP, is a regular expression that
further filters the file names."
  (interactive
   (let ((dir-A (ediff-get-default-directory-name))
	 f)
     (list (setq f (ediff-read-file-name "Directory A to merge" dir-A nil))
	   (setq f (ediff-read-file-name "Directory B to merge" 
				 (if ediff-use-last-dir
				     ediff-last-dir-B 
				   (ediff-strip-last-dir f))
				 nil))
	   (ediff-read-file-name "Ancestor directory: "
				 (if ediff-use-last-dir
				     ediff-last-dir-C 
				   (ediff-strip-last-dir f))
				 nil)
	   (read-string "Filter through regular expression: "
			nil ediff-filtering-regexp-history)
	   )))
  (ediff-directories-internal
   dir1 dir2 dir3 regexp
   'ediff-merge-files-with-ancestor 'ediff-merge-directories-with-ancestor
   ))

;;;###autoload
(defun ediff-merge-directory-revisions (dir1 regexp)
  "Run Ediff on a directory, DIR1, merging its files with their revisions.
The second argument, REGEXP, is a regular expression that filters the file
names. Only the files that are under revision control are taken into account."
  (interactive
   (let ((dir-A (ediff-get-default-directory-name)))
     (list (ediff-read-file-name
	    "Directory to merge with revisions" dir-A nil)
	   (read-string "Filter through regular expression: "
			nil ediff-filtering-regexp-history)
	   )))
  (ediff-directory-revisions-internal
   dir1 regexp 'ediff-merge-revisions 'ediff-merge-directory-revisions
   ))

;;;###autoload
(defalias 'edir-merge-revisions 'ediff-merge-directory-revisions)

;;;###autoload
(defun ediff-merge-directory-revisions-with-ancestor (dir1 regexp)
  "Run Ediff on a directory, DIR1, merging its files with their revisions and ancestors.
The second argument, REGEXP, is a regular expression that filters the file
names. Only the files that are under revision control are taken into account."
  (interactive
   (let ((dir-A (ediff-get-default-directory-name)))
     (list (ediff-read-file-name
	    "Directory to merge with revisions and ancestors" dir-A nil)
	   (read-string "Filter through regular expression: "
			nil ediff-filtering-regexp-history)
	   )))
  (ediff-directory-revisions-internal
   dir1 regexp 'ediff-merge-revisions-with-ancestor
   'ediff-merge-directory-revisions-with-ancestor
   ))

;;;###autoload
(defalias
  'edir-merge-revisions-with-ancestor
  'ediff-merge-directory-revisions-with-ancestor) 

;;;###autoload
(defalias 'edirs-merge-with-ancestor 'ediff-merge-directories-with-ancestor)

;; Run ediff-action (ediff-files, ediff-merge, ediff-merge-with-ancestors)
;; on a pair of directories (three directories, in case of ancestor).
;; The third argument, REGEXP, is a regular expression that further filters the
;; file names.
;; JOBNAME is the symbol indicating the meta-job to be performed.
(defun ediff-directories-internal (dir1 dir2 dir3 regexp 
					action jobname 
					&optional startup-hooks)
  ;; ediff-read-file-name is set to attach a previously entered file name if
  ;; the currently entered file is a directory. This code takes care of that.
  (setq dir1 (if (file-directory-p dir1) dir1 (file-name-directory dir1))
	dir2 (if (file-directory-p dir2) dir2 (file-name-directory dir2)))

  (if (stringp dir3)
      (setq dir3 (if (file-directory-p dir3) dir3 (file-name-directory dir3))))

  (cond ((string= dir1 dir2)
	 (error "Directories A and B are the same: %s" dir1))
	((and (eq jobname 'ediff-directories3)
	      (string= dir1 dir3))
	 (error "Directories A and C are the same: %s" dir1))
	((and (eq jobname 'ediff-directories3)
	      (string= dir2 dir3))
	 (error "Directories B and C are the same: %s" dir1)))

  (let (diffs ; var where ediff-intersect-directories returns the diff list
	file-list meta-buf)
    (setq file-list (ediff-intersect-directories 
		     jobname 'diffs regexp dir1 dir2 dir3))
    (setq startup-hooks
	  ;; this sets various vars in the meta buffer inside
	  ;; ediff-prepare-meta-buffer
	  (cons (` (lambda ()
		     ;; tell what to do if the user clicks on a session record
		     (setq ediff-session-action-function (quote (, action)))
		     ;; set ediff-dir-difference-list 
		     (setq ediff-dir-difference-list (quote (, diffs)))))
		startup-hooks))
    (setq meta-buf (ediff-prepare-meta-buffer 
		    'ediff-dir-action
		    file-list
		    "*Ediff Session Group Panel"
		    'ediff-redraw-directory-group-buffer
		    jobname
		    startup-hooks))
    (ediff-show-meta-buffer meta-buf)
    ))

(defun ediff-directory-revisions-internal (dir1 regexp action jobname 
						&optional startup-hooks)
  (setq dir1 (if (file-directory-p dir1) dir1 (file-name-directory dir1)))
  (let (file-list meta-buf)
    (setq file-list
	  (ediff-get-directory-files-under-revision jobname regexp dir1))
    (setq startup-hooks
	  ;; this sets various vars in the meta buffer inside
	  ;; ediff-prepare-meta-buffer
	  (cons (` (lambda ()
		     ;; tell what to do if the user clicks on a session record
		     (setq ediff-session-action-function (quote (, action)))
		     ))
		startup-hooks))
    (setq meta-buf (ediff-prepare-meta-buffer 
		    'ediff-dir-action
		    file-list
		    "*Ediff Session Group Panel"
		    'ediff-redraw-directory-group-buffer
		    jobname
		    startup-hooks))
    (ediff-show-meta-buffer meta-buf)
    ))


;;; Compare regions and windows

;;;###autoload
(defun ediff-windows-wordwise (dumb-mode &optional wind-A wind-B startup-hooks)
  "Compare WIND-A and WIND-B, which are selected by clicking, wordwise.
With prefix argument, DUMB-MODE, or on a non-windowing display, works as
follows:
If WIND-A is nil, use selected window.
If WIND-B is nil, use window next to WIND-A."
  (interactive "P")
  (ediff-windows dumb-mode wind-A wind-B
		 startup-hooks 'ediff-windows-wordwise 'word-mode))
		 
;;;###autoload
(defun ediff-windows-linewise (dumb-mode &optional wind-A wind-B startup-hooks)
  "Compare WIND-A and WIND-B, which are selected by clicking, linewise.
With prefix argument, DUMB-MODE, or on a non-windowing display, works as
follows:
If WIND-A is nil, use selected window.
If WIND-B is nil, use window next to WIND-A."
  (interactive "P")
  (ediff-windows dumb-mode wind-A wind-B
		 startup-hooks 'ediff-windows-linewise nil))
      
;; Compare WIND-A and WIND-B, which are selected by clicking.
;; With prefix argument, DUMB-MODE, or on a non-windowing display,
;; works as follows:
;; If WIND-A is nil, use selected window.
;; If WIND-B is nil, use window next to WIND-A.
(defun ediff-windows (dumb-mode wind-A wind-B startup-hooks job-name word-mode)
  (if (or dumb-mode (not (ediff-window-display-p)))
      (setq wind-A (ediff-get-next-window wind-A nil)
	    wind-B (ediff-get-next-window wind-B wind-A))
    (setq wind-A (ediff-get-window-by-clicking wind-A nil 1)
	  wind-B (ediff-get-window-by-clicking wind-B wind-A 2)))
      
  (let ((buffer-A (window-buffer wind-A))
	(buffer-B (window-buffer wind-B))
	beg-A end-A beg-B end-B)
    
    (save-excursion
      (save-window-excursion
	(sit-for 0) ; synch before using window-start/end -- a precaution
	(select-window wind-A)
	(setq beg-A (window-start)
	      end-A (window-end))
	(select-window wind-B)
	(setq beg-B (window-start)
	      end-B (window-end))))
    (ediff-regions-internal
     buffer-A beg-A end-A buffer-B beg-B end-B
     startup-hooks job-name word-mode)))
     
;;;###autoload
(defun ediff-regions-wordwise (buffer-A buffer-B &optional startup-hooks)
  "Run Ediff on a pair of regions in two different buffers.
Regions \(i.e., point and mark\) are assumed to be set in advance.
This function is effective only for relatively small regions, up to 200
lines. For large regions, use `ediff-regions-linewise'."
  (interactive 
   (let (bf)
     (list (setq bf (read-buffer "Region's A buffer: "
				 (ediff-other-buffer "") t))
	   (read-buffer "Region's B buffer: "
			(progn
			  ;; realign buffers so that two visible bufs will be
			  ;; at the top
			  (save-window-excursion (other-window 1))
			  (ediff-other-buffer bf))
			t))))
  (if (not (ediff-buffer-live-p buffer-A))
      (error "Buffer %S doesn't exist" buffer-A))
  (if (not (ediff-buffer-live-p buffer-B))
      (error "Buffer %S doesn't exist" buffer-B))
  
  
  (let (reg-A-beg reg-A-end reg-B-beg reg-B-end)
    (save-excursion
      (set-buffer buffer-A)
      (setq reg-A-beg (region-beginning)
	    reg-A-end (region-end))
      (set-buffer buffer-B)
      (setq reg-B-beg (region-beginning)
	    reg-B-end (region-end)))
	    
    (ediff-regions-internal
     (get-buffer buffer-A) reg-A-beg reg-A-end
     (get-buffer buffer-B) reg-B-beg reg-B-end
     startup-hooks 'ediff-regions-wordwise 'word-mode)))
     
;;;###autoload
(defun ediff-regions-linewise (buffer-A buffer-B &optional startup-hooks)
  "Run Ediff on a pair of regions in two different buffers.
Regions \(i.e., point and mark\) are assumed to be set in advance.
Each region is enlarged to contain full lines.
This function is effective for large regions, over 100-200
lines. For small regions, use `ediff-regions-wordwise'."
  (interactive 
   (let (bf)
     (list (setq bf (read-buffer "Region A's buffer: "
				 (ediff-other-buffer "") t))
	   (read-buffer "Region B's buffer: "
			(progn
			  ;; realign buffers so that two visible bufs will be
			  ;; at the top
			  (save-window-excursion (other-window 1))
			  (ediff-other-buffer bf))
			t))))
  (if (not (ediff-buffer-live-p buffer-A))
      (error "Buffer %S doesn't exist" buffer-A))
  (if (not (ediff-buffer-live-p buffer-B))
      (error "Buffer %S doesn't exist" buffer-B))
  
  (let (reg-A-beg reg-A-end reg-B-beg reg-B-end)
    (save-excursion
      (set-buffer buffer-A)
      (setq reg-A-beg (region-beginning)
	    reg-A-end (region-end))
      ;; enlarge the region to hold full lines
      (goto-char reg-A-beg) 
      (beginning-of-line)
      (setq reg-A-beg (point))
      (goto-char reg-A-end) 
      (end-of-line)
      (or (eobp) (forward-char)) ; include the newline char
      (setq reg-A-end (point))
      
      (set-buffer buffer-B)
      (setq reg-B-beg (region-beginning)
	    reg-B-end (region-end))
      ;; enlarge the region to hold full lines
      (goto-char reg-A-beg) 
      (goto-char reg-B-beg) 
      (beginning-of-line)
      (setq reg-B-beg (point))
      (goto-char reg-B-end) 
      (end-of-line)
      (or (eobp) (forward-char)) ; include the newline char
      (setq reg-B-end (point))
      ) ; save excursion
	    
    (ediff-regions-internal
     (get-buffer buffer-A) reg-A-beg reg-A-end
     (get-buffer buffer-B) reg-B-beg reg-B-end
     startup-hooks 'ediff-regions-linewise nil))) ; no word mode
	
;; compare region beg-A to end-A of buffer-A
;; to regions beg-B -- end-B in buffer-B. 
(defun ediff-regions-internal (buffer-A beg-A end-A buffer-B beg-B end-B
					startup-hooks job-name word-mode)
  (let ((tmp-buffer (get-buffer-create ediff-tmp-buffer))
	overl-A overl-B
	file-A file-B)
	
    ;; in case beg/end-A/B aren't markers--make them into markers
    (ediff-eval-in-buffer buffer-A
      (setq beg-A (move-marker (make-marker) beg-A)
	    end-A (move-marker (make-marker) end-A)))
    (ediff-eval-in-buffer buffer-B
      (setq beg-B (move-marker (make-marker) beg-B)
	    end-B (move-marker (make-marker) end-B)))
	
    (if (and (eq buffer-A buffer-B)
	     (or (and (< beg-A end-B) (<= beg-B beg-A))   ; b-B b-A e-B
		 (and (< beg-B end-A) (<= end-A end-B)))) ; b-B e-A e-B
	(progn
	  (with-output-to-temp-buffer ediff-msg-buffer
	    (princ "
You have requested to compare overlapping regions of the same buffer.

In this case, Ediff's highlighting may be confusing---in the same window,
you may see highlighted regions that belong to different regions.

Continue anyway? (y/n) "))

	  (if (y-or-n-p "Continue anyway? ")
	      ()
	    (error "%S aborted" job-name))))
	    
    ;; make file-A
    (if word-mode
	(ediff-wordify beg-A end-A buffer-A tmp-buffer)
      (ediff-copy-to-buffer beg-A end-A buffer-A tmp-buffer))
    (setq file-A (ediff-make-temp-file tmp-buffer "regA"))
    
    ;; make file-B
    (if word-mode
	(ediff-wordify beg-B end-B buffer-B tmp-buffer)
      (ediff-copy-to-buffer beg-B end-B buffer-B tmp-buffer))
    (setq file-B (ediff-make-temp-file tmp-buffer "regB"))
     
    (setq overl-A (ediff-make-bullet-proof-overlay beg-A end-A buffer-A))
    (setq overl-B (ediff-make-bullet-proof-overlay beg-B end-B buffer-B))
    (ediff-setup buffer-A file-A
		 buffer-B file-B
		 nil nil	    ; buffer & file C
		 (cons (` (lambda ()
			    (delete-file (, file-A))
			    (delete-file (, file-B))))
		       startup-hooks)
		 (list (cons 'ediff-word-mode  word-mode)
		       (cons 'ediff-narrow-bounds (list overl-A overl-B))
		       (cons 'ediff-job-name job-name))
		 )
    ))
    
 
;;; Merge files and buffers
  
;;;###autoload
(defalias 'ediff-merge 'ediff-merge-files)
  
(defsubst ediff-merge-on-startup ()
  (ediff-do-merge 0)
  (ediff-eval-in-buffer ediff-buffer-C
    (set-buffer-modified-p nil)))

;;;###autoload
(defun ediff-merge-files (file-A file-B &optional startup-hooks)
  "Merge two files without ancestor."
  (interactive
   (let ((dir-A (if ediff-use-last-dir
		    ediff-last-dir-A
		  default-directory))
	 dir-B f)
     (list (setq f (ediff-read-file-name
		    "File A to merge" dir-A
		    (ediff-get-default-file-name)))
	   (ediff-read-file-name "File B to merge" 
				 (setq dir-B
				       (if ediff-use-last-dir
					   ediff-last-dir-B 
					 (file-name-directory f)))
				 (progn
				   (setq file-name-history
					 (cons (ediff-abbreviate-file-name
						(expand-file-name
						 (file-name-nondirectory f)
						 dir-B))
					       file-name-history))
				   f))
	   )))
  (setq startup-hooks (cons 'ediff-merge-on-startup startup-hooks))
  (ediff-files-internal file-A 
			(if (file-directory-p file-B)
			    (expand-file-name
			     (file-name-nondirectory file-A) file-B)
			  file-B)
			  nil ; file-C
			  startup-hooks
			  'ediff-merge-files))
			  
;;;###autoload
(defun ediff-merge-files-with-ancestor (file-A file-B file-ancestor
					       &optional startup-hooks)
  "Merge two files with ancestor."
  (interactive
   (let ((dir-A (if ediff-use-last-dir
		    ediff-last-dir-A
		  default-directory))
	 dir-B dir-ancestor f ff)
     (list (setq f (ediff-read-file-name
		    "File A to merge" dir-A
		    (ediff-get-default-file-name)))
	   (setq ff (ediff-read-file-name "File B to merge" 
					  (setq dir-B
						(if ediff-use-last-dir
						    ediff-last-dir-B 
						  (file-name-directory f)))
					  (progn
					    (setq file-name-history
						  (cons
						   (ediff-abbreviate-file-name
						    (expand-file-name
						     (file-name-nondirectory f)
						     dir-B))
						   file-name-history))
					    f)))
	   (ediff-read-file-name "Ancestor file" 
				 (setq dir-ancestor
				       (if ediff-use-last-dir
					   ediff-last-dir-ancestor
					 (file-name-directory ff)))
				 (progn
				   (setq file-name-history
					 (cons (ediff-abbreviate-file-name
						(expand-file-name
						 (file-name-nondirectory ff)
						 dir-ancestor))
					       file-name-history))
				   ff))
	   )))
  (setq startup-hooks (cons 'ediff-merge-on-startup startup-hooks))
  (ediff-files-internal file-A 
			(if (file-directory-p file-B)
			    (expand-file-name
			     (file-name-nondirectory file-A) file-B)
			  file-B)
			  file-ancestor
			  startup-hooks
			  'ediff-merge-files-with-ancestor))
			  
;;;###autoload
(defalias 'ediff-merge-with-ancestor 'ediff-merge-files-with-ancestor)
			  
;;;###autoload
(defun ediff-merge-buffers (buffer-A buffer-B &optional startup-hooks job-name)
  "Merge buffers without ancestor."
  (interactive 
   (let (bf)
     (list (setq bf (read-buffer "Buffer A to merge: "
				 (ediff-other-buffer "") t))
	   (read-buffer "Buffer B to merge: "
			(progn
			  ;; realign buffers so that two visible bufs will be
			  ;; at the top
			  (save-window-excursion (other-window 1))
			  (ediff-other-buffer bf))
			t))))
  
  (setq startup-hooks (cons 'ediff-merge-on-startup startup-hooks))
  (or job-name (setq job-name 'ediff-merge-buffers))
  (ediff-buffers-internal
   buffer-A buffer-B nil startup-hooks job-name))
   
;;;###autoload
(defun ediff-merge-buffers-with-ancestor (buffer-A 
					  buffer-B buffer-ancestor
					  &optional startup-hooks job-name)
  "Merge buffers with ancestor."
  (interactive 
   (let (bf bff)
     (list (setq bf (read-buffer "Buffer A to merge: "
				 (ediff-other-buffer "") t))
	   (setq bff (read-buffer "Buffer B to merge: "
				  (progn
				    ;; realign buffers so that two visible
				    ;; bufs will be at the top
				    (save-window-excursion (other-window 1))
				    (ediff-other-buffer bf))
				  t))
	   (read-buffer "Ancestor buffer: "
				  (progn
				    ;; realign buffers so that three visible
				    ;; bufs will be at the top
				    (save-window-excursion (other-window 1))
				    (ediff-other-buffer (list bf bff)))
				  t)
	   )))
  
  (setq startup-hooks (cons 'ediff-merge-on-startup startup-hooks))
  (or job-name (setq job-name 'ediff-merge-buffers-with-ancestor))
  (ediff-buffers-internal
   buffer-A buffer-B buffer-ancestor startup-hooks job-name))
      

;;;###autoload
(defun ediff-merge-revisions (&optional file startup-hooks)
  "Run Ediff by merging two revisions of a file.
The file is the optional FILE argument or the file visited by the current
buffer."
  (interactive)
  (ediff-load-version-control)
  (if (stringp file) (find-file file))
  (let (rev1 rev2 buf1 buf2)
    (setq rev1
	  (read-string
	   (format
	    "Version 1 to merge (default: %s's latest version): "
	    (if (stringp file)
		(file-name-nondirectory file) "current buffer")))
	  rev2
	  (read-string
	   (format
	    "Version 2 to merge (default: %s): "
	    (if (stringp file)
		(file-name-nondirectory file) "current buffer"))))
    (cond ((eq ediff-version-control-package 'vc)
	   (save-excursion
	     (vc-version-other-window rev1)
	     (setq buf1 (current-buffer)))
	   (save-excursion
	     (or (string= rev2 "")
		 (vc-version-other-window rev2))
	     (setq buf2 (current-buffer)))
	   (setq startup-hooks 
		 (cons 
		  (` (lambda () 
		       (delete-file (, (buffer-file-name buf1)))
		       (or (, (string= rev2 ""))
			   (delete-file (, (buffer-file-name buf2))))))
		  startup-hooks)))
	  ((eq ediff-version-control-package 'rcs)
	   (setq buf1 (rcs-ediff-view-revision rev1)
		 buf2 (if (string= rev2 "")
			  (current-buffer)
			(rcs-ediff-view-revision rev2))))
	  ((eq ediff-version-control-package 'generic-sc)
	   (save-excursion
	     (if (string= rev1 "")
		 (setq rev1 (generic-sc-get-latest-rev)))
	     (sc-visit-previous-revision rev1)
	     (setq buf1 (current-buffer)))
	   (save-excursion
	     (or (string= rev2 "")
		 (sc-visit-previous-revision rev2))
	     (setq buf2 (current-buffer))))
	  ) ; cond
    (ediff-merge-buffers buf1 buf2 startup-hooks 'ediff-merge-revisions)))
    

;;;###autoload
(defun ediff-merge-revisions-with-ancestor (&optional file startup-hooks)
  "Run Ediff by merging two revisions of a file with a common ancestor.
The file is the the optional FILE argument or the file visited by the current
buffer."
  (interactive)
  (ediff-load-version-control)
  (if (stringp file) (find-file file))
  (let (rev1 rev2 ancestor-rev buf1 buf2 ancestor-buf)
    (setq rev1
	  (read-string
	   (format
	    "Version 1 to merge (default: %s's latest version): "
	    (if (stringp file)
		(file-name-nondirectory file) "current buffer")))
	  rev2
	  (read-string
	   (format
	    "Version 2 to merge (default: %s): "
	    (if (stringp file)
		(file-name-nondirectory file) "current buffer")))
	  ancestor-rev
	  (read-string
	   (format
	    "Ancestor version (default: %s): "
	    (if (stringp file)
		(file-name-nondirectory file) "current buffer"))))
    (cond ((eq ediff-version-control-package 'vc)
	   (save-excursion
	     (vc-version-other-window rev1)
	     (setq buf1 (current-buffer)))
	   (save-excursion
	     (or (string= rev2 "")
		 (vc-version-other-window rev2))
	     (setq buf2 (current-buffer)))
	   (save-excursion
	     (or (string= ancestor-rev "")
		 (vc-version-other-window ancestor-rev))
	     (setq ancestor-buf (current-buffer)))
	   (setq startup-hooks 
		 (cons
		  (` (lambda () 
		       (delete-file (, (buffer-file-name buf1)))
		       (or (, (string= rev2 ""))
			   (delete-file (, (buffer-file-name buf2))))
		       (or (, (string= ancestor-rev ""))
			   (delete-file (, (buffer-file-name ancestor-buf))))))
		  startup-hooks)))
	  ((eq ediff-version-control-package 'rcs)
	   (setq buf1 (rcs-ediff-view-revision rev1)
		 buf2 (if (string= rev2 "")
			  (current-buffer)
			(rcs-ediff-view-revision rev2))
		 ancestor-buf (if (string= ancestor-rev "")
				  (current-buffer)
				(rcs-ediff-view-revision ancestor-rev))))
	  ((eq ediff-version-control-package 'generic-sc)
	   (save-excursion
	     (if (string= rev1 "")
		 (setq rev1 (generic-sc-get-latest-rev)))
	     (sc-visit-previous-revision rev1)
	     (setq buf1 (current-buffer)))
	   (save-excursion
	     (or (string= rev2 "")
		 (sc-visit-previous-revision rev2))
	     (setq buf2 (current-buffer)))
	   (save-excursion
	     (or (string= ancestor-rev "")
		 (sc-visit-previous-revision ancestor-rev))
	     (setq ancestor-buf (current-buffer))))
	  ) ; cond
    (ediff-merge-buffers-with-ancestor
     buf1 buf2 ancestor-buf
     startup-hooks 'ediff-merge-revisions-with-ancestor)))
     
     
;;; Apply patch
    
;;;###autoload
(defun ediff-patch-buffer (buffer-name &optional startup-hooks)		  
  "Run Ediff by patching BUFFER-NAME."
  (interactive "bBuffer to patch: ")
  
  (let* ((buf-to-patch (get-buffer buffer-name))
	 (file-name-ok (if buf-to-patch (buffer-file-name  buf-to-patch)))
	 (buf-mod-status (buffer-modified-p buf-to-patch))
	 default-dir file-name ctl-buf)
    (if file-name-ok
	(setq file-name file-name-ok)
      (ediff-eval-in-buffer buffer-name
	(setq default-dir default-directory)
	(setq file-name (ediff-make-temp-file buffer-name))
	(set-visited-file-name file-name)
	(setq buffer-auto-save-file-name nil) ; don't create auto-save file
	(rename-buffer buffer-name) ; don't confuse the user with new buf name
	(set-buffer-modified-p nil)
	(set-visited-file-modtime) ; sync buffer and temp file
	(setq default-directory default-dir)
	))
    
    (setq ctl-buf
	  (ediff-patch-file file-name startup-hooks 'ediff-patch-buffer))
    
    (if file-name-ok
	()
      (ediff-eval-in-buffer ctl-buf
	(delete-file (buffer-file-name ediff-buffer-A))
	(delete-file (buffer-file-name ediff-buffer-B))
	(ediff-eval-in-buffer ediff-buffer-A
	  (if default-dir (setq default-directory default-dir))
	  (set-visited-file-name nil)
	  (rename-buffer buffer-name)
	  (set-buffer-modified-p buf-mod-status))
	(ediff-eval-in-buffer ediff-buffer-B
	  (setq buffer-auto-save-file-name nil) ; don't create auto-save file
	  (if default-dir (setq default-directory default-dir))
	  (set-visited-file-name nil)
	  (rename-buffer (ediff-unique-buffer-name 
			  (concat buffer-name "_patched") ""))
	  (set-buffer-modified-p t))))
    ))


(defun ediff-get-patch-buffer (dir)
  "Obtain patch buffer.  If patch is already in a buffer---use it.
Else, read patch file into a new buffer."
  (if (y-or-n-p "Is the patch file already in a buffer? ")
      (setq ediff-patch-buf
	    (get-buffer (read-buffer "Patch buffer name: " nil t))) ;must match
    (setq ediff-patch-buf
	  (find-file-noselect (read-file-name "Patch file name: " dir))))
  
  (setq ediff-patch-diagnostics
	(get-buffer-create "*ediff patch diagnostics*"))
  (ediff-eval-in-buffer ediff-patch-diagnostics
    (insert-buffer ediff-patch-buf)))


      


;;; Versions Control functions      
      
;;;###autoload
(defun ediff-revision (&optional file startup-hooks)
  "Run Ediff by comparing versions of a file.
The file is an optional FILE argument or the file visited by the current
buffer. Use `vc.el' or `rcs.el' depending on `ediff-version-control-package'."
  ;; if buffer is non-nil, use that buffer instead of the current buffer
  (interactive "P")
  (if (stringp file) (find-file file))
  (let (rev1 rev2)
    (setq rev1
	  (read-string
	   (format "Version 1 to compare (default: %s's latest version): "
		   (if (stringp file)
		       (file-name-nondirectory file) "current buffer")))
	  rev2
	  (read-string 
	   (format "Version 2 to compare (default: %s): "
		   (if (stringp file)
		       (file-name-nondirectory file) "current buffer"))))
    (ediff-load-version-control)
    (funcall
     (intern (format "%S-ediff-internal" ediff-version-control-package))
     rev1 rev2 startup-hooks)
    ))
   
   
;; Test if version control package is loaded and load if not
;; Is SILENT is non-nil, don't report error if package is not found.
(defun ediff-load-version-control (&optional silent)
  (or (featurep ediff-version-control-package)
      (if (locate-library (symbol-name ediff-version-control-package))
	  (progn
	    (message "") ; kill the message from `locate-library'
	    (require ediff-version-control-package))
	(or silent
	    (error "Version control package %S.el not found. Use vc.el instead"
		   ediff-version-control-package)))))

      
(defun vc-ediff-internal (rev1 rev2 &optional startup-hooks)
  "Run Ediff on versions of the current buffer.
If REV2 is \"\" then compare current buffer with REV1.
If the current buffer is named `F', the version is named `F.~REV~'.
If `F.~REV~' already exists, it is used instead of being re-created."
  (let (file1 file2 rev1buf rev2buf)
    (save-excursion
      (vc-version-other-window rev1)
      (setq rev1buf (current-buffer)
	    file1 (buffer-file-name)))
    (save-excursion
      (or (string= rev2 "") 		; use current buffer
	  (vc-version-other-window rev2))
      (setq rev2buf (current-buffer)
	    file2 (buffer-file-name)))
    (setq startup-hooks
	  (cons (` (lambda ()
		     (delete-file (, file1))
		     (or (, (string= rev2 "")) (delete-file (, file2)))
		     ))
		startup-hooks))
    (ediff-buffers
     rev1buf rev2buf
     startup-hooks
     'ediff-revision)))
    
(defun rcs-ediff-view-revision (&optional rev)
  "View previous RCS revision of current file.
With prefix argument, prompts for a revision name." 
  (interactive (list (if current-prefix-arg 
			 (read-string "Revision: "))))
  (let* ((filename (buffer-file-name (current-buffer)))
	 (switches (append '("-p")
			   (if rev (list (concat "-r" rev)) nil)))
	 (buff (concat (file-name-nondirectory filename) ".~" rev "~")))
    (message "Working ...")
    (setq filename (expand-file-name filename))
    (with-output-to-temp-buffer buff
      (let ((output-buffer (ediff-rcs-get-output-buffer filename buff)))
	(delete-windows-on output-buffer)
	(save-excursion
	  (set-buffer output-buffer)
	  (apply 'call-process "co" nil t nil
		 ;; -q: quiet (no diagnostics)
		 (append switches rcs-default-co-switches
			 (list "-q" filename))))) 
      (message "")
      buff)))    
      
(defun ediff-rcs-get-output-buffer (file name)
  ;; Get a buffer for RCS output for FILE, make it writable and clean it up.
  ;; Optional NAME is name to use instead of `*RCS-output*'.
  ;; This is a modified version from rcs.el v1.1. I use it here to make
  ;; Ediff immune to changes in rcs.el
  (let* ((default-major-mode 'fundamental-mode) ; no frills!
	 (buf (get-buffer-create name)))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil
	    default-directory (file-name-directory (expand-file-name file)))
      (erase-buffer))
    buf))

(defun rcs-ediff-internal (rev1 rev2 &optional startup-hooks)
  "Run Ediff on versions of the current buffer.
If REV2 is \"\" then use current buffer."
  (let ((rev2buf (if (string= rev2 "")
		     (current-buffer)
		   (rcs-ediff-view-revision rev2)))
	(rev1buf (rcs-ediff-view-revision rev1)))
	
    ;; rcs.el doesn't create temp version files, so we don't have to delete
    ;; anything in startup hooks to ediff-buffers
    (ediff-buffers rev1buf rev2buf startup-hooks 'ediff-revision)
    ))

(defun generic-sc-get-latest-rev ()
  (cond ((eq sc-mode 'CCASE)
	 (eval "main/LATEST"))
	(t (eval "")))
  )

(defun generic-sc-ediff-internal (rev1 rev2 &optional startup-hooks)
  "Run Ediff on versions of the current buffer.
If REV2 is \"\" then compare current buffer with REV1.
If the current buffer is named `F', the version is named `F.~REV~'.
If `F.~REV~' already exists, it is used instead of being re-created."
  (let (rev1buf rev2buf)
    (save-excursion
      (if (or (not rev1) (string= rev1 ""))
	  (setq rev1 (generic-sc-get-latest-rev)))
      (sc-visit-previous-revision rev1)
      (setq rev1buf (current-buffer)))
    (save-excursion
      (or (string= rev2 "") 		; use current buffer
	  (sc-visit-previous-revision rev2))
      (setq rev2buf (current-buffer)))
    (ediff-buffers rev1buf rev2buf startup-hooks 'ediff-revision)))

;;;###autoload
(defun ediff-version ()
  "Return string describing the version of Ediff.
When called interactively, displays the version."
  (interactive)
  (if (interactive-p)
      (message (ediff-version))
    (format "Ediff %s of %s" ediff-version ediff-date)))


(provide 'ediff)
(require 'ediff-util)

;;; ediff.el ends here
