;;; ediff-ptch.el --- Ediff's  patch support

;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.sunysb.edu>

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


;;; Code:

(defvar ediff-last-dir-patch nil
  "Last directory used by an Ediff command for file to patch.")

(defvar ediff-backup-extension 
  (if (memq system-type '(vax-vms axp-vms emx ms-dos windows-nt windows-95))
      "_orig" ".orig")
  "Default backup extension for the patch program.")

(defvar ediff-patch-default-directory nil
  "*Default directory to look for patches.")

(defvar ediff-context-diff-label-regexp
  (concat "\\(" 	; context diff 2-liner
	  "^\\*\\*\\* \\([^ \t]+\\)[^*]+[\t ]*\n--- \\([^ \t]+\\)"
	  "\\|" 	; GNU unified format diff 2-liner
	  "^--- \\([^ \t]+\\)[^-]+[\t ]*\n\\+\\+\\+ \\([^ \t]+\\)"
	  "\\)")
  "*Regexp matching filename 2-liners at the start of each context diff.")

(defvar ediff-patch-program "patch"
  "*Name of the program that applies patches.")
(defvar ediff-patch-options ""
  "*Options to pass to ediff-patch-program.")

;; The buffer of the patch file. Local to control buffer.
(ediff-defvar-local ediff-patchbufer nil "")

;; The buffer where patch displays its diagnostics.
(ediff-defvar-local ediff-patch-diagnostics nil "")

;; Map of patch buffer. Has the form:
;;    ((filename1 marker1 marker2) (filename2 marker1 marker2) ...)
;; where filenames are files to which patch would have applied the patch;
;; marker1 delimits the beginning of the corresponding patch and marker2 does
;; it for the end.
(ediff-defvar-local ediff-patch-map nil "")

;; strip prefix from filename
;; returns /dev/null, if can't strip prefix
(defsubst ediff-file-name-sans-prefix (filename prefix)
  (save-match-data
    (if (string-match (concat "^" prefix) filename)
	(substring filename (match-end 0))
      (concat "/null/" filename))))



;; no longer used
;; return the number of matches of regexp in buf starting from the beginning
(defun ediff-count-matches (regexp buf)
  (ediff-eval-in-buffer buf
    (let ((count 0) opoint)
      (save-excursion
	(goto-char (point-min))
	(while (and (not (eobp))
		    (progn (setq opoint (point))
			   (re-search-forward regexp nil t)))
	  (if (= opoint (point))
	      (forward-char 1)
	    (setq count (1+ count)))))
      count)))

;; Scan BUF (which is supposed to contain a patch) and make a list of the form 
;;    ((filename1 marker1 marker2) (filename2 marker1 marker2) ...)
;; where filenames are files to which patch would have applied the patch;
;; marker1 delimits the beginning of the corresponding patch and marker2 does
;; it for the end. This list is then assigned to ediff-patch-map.
;; Returns the number of elements in the list ediff-patch-map
(defun ediff-map-patch-buffer (buf)
  (ediff-eval-in-buffer buf
    (let ((count 0)
	  (mark1 (move-marker (make-marker) (point-min)))
	  (mark1-end (point-min))
	  (possible-file-names '("/dev/null" . "/dev/null"))
	  mark2-end mark2 filenames
	  beg1 beg2 end1 end2
	  patch-map opoint)
      (save-excursion
	(goto-char (point-min))
	(setq opoint (point))
	(while (and (not (eobp))
		    (re-search-forward ediff-context-diff-label-regexp nil t))
	  (if (= opoint (point))
	      (forward-char 1) ; ensure progress towards the end
	    (setq mark2 (move-marker (make-marker) (match-beginning 0))
		  mark2-end (match-end 0)
		  beg1 (match-beginning 2)
		  end1 (match-end 2)
		  beg2 (match-beginning 3)
		  end2 (match-end 3))
	    ;; possible-file-names is holding the new file names until we
	    ;; insert the old file name in the patch map
	    ;; It is a pair (filename from 1st header line . fn from 2nd line)
	    (setq possible-file-names
		  (cons (if (and beg1 end1)
			    (buffer-substring beg1 end1)
			  "/dev/null")
			(if (and beg2 end2)
			    (buffer-substring beg2 end2)
			  "/dev/null")))
	    ;; check for any `Index:' or `Prereq:' lines, but don't use them
	    (if (re-search-backward "^Index:" mark1-end 'noerror)
		(move-marker mark2 (match-beginning 0)))
	    (if (re-search-backward "^Prereq:" mark1-end 'noerror)
		(move-marker mark2 (match-beginning 0)))

	    (goto-char mark2-end)
	    
	    (if filenames
		(setq patch-map (cons (list filenames mark1 mark2) patch-map)))
	    (setq mark1 mark2
		  mark1-end mark2-end
		  filenames possible-file-names))
	  (setq opoint (point)
		count (1+ count))))
      (setq mark2 (point-max-marker)
	    patch-map (cons (list possible-file-names mark1 mark2) patch-map))
      (setq ediff-patch-map (nreverse patch-map))
      count)))

;; Fix up the file names in the list using the argument FILENAME
;; Algorithm: find the first file's directory and cut it out from each file
;; name in the patch. Prepend the directory of FILENAME to each file in the
;; patch. In addition, the first file in the patch is replaced by FILENAME.
;; Each file is actually a file-pair of files found in the context diff header
;; In the end, for each pair, we select the shortest existing file.
;; Note: Ediff doesn't recognize multi-file patches that are separated
;; with the `Index:' line. It treats them as a single-file patch.
;;
;; Executes inside the patch buffer
(defun ediff-fixup-patch-map (filename)
  (setq filename (expand-file-name filename))
  (let ((actual-dir (if (file-directory-p filename)
			;; directory part of filename
			(file-name-as-directory filename)
		      (file-name-directory filename)))
	;; directory part of the first file in the patch
	(base-dir1 (file-name-directory (car (car (car ediff-patch-map)))))
	(base-dir2 (file-name-directory (cdr (car (car ediff-patch-map)))))
	)

    ;; chop off base-dirs
    (mapcar (function (lambda (triple)
			(or (string= (car (car triple)) "/dev/null")
			    (setcar (car triple)
				    (ediff-file-name-sans-prefix
				     (car (car triple)) base-dir1)))
			(or (string= (cdr (car triple)) "/dev/null")
			    (setcdr (car triple)
				    (ediff-file-name-sans-prefix
				     (cdr (car triple)) base-dir2)))
			))
	    ediff-patch-map)

    ;; take the given file name into account
    (or (file-directory-p filename)
	(string= "/dev/null" filename)
	(progn
	  (setcar (car ediff-patch-map)
		  (cons (file-name-nondirectory filename)
			(file-name-nondirectory filename)))))

    ;; prepend actual-dir
    (mapcar (function (lambda (triple)
			 (if (and (string-match "^/null/" (car (car triple)))
				  (string-match "^/null/" (cdr (car triple))))
			     ;; couldn't strip base-dir1 and base-dir2
			     ;; hence, something wrong
			     (progn
			       (with-output-to-temp-buffer ediff-msg-buffer
				 (princ
				  (format "
The patch file contains a context diff for
	%s
	%s

However, Ediff cannot infer the name of the actual file
to be patched on your system. If you know the correct file name,
please enter it now.

If you don't know and still would like to apply patches to
other files, enter /dev/null
"
					  (substring (car (car triple)) 6)
					  (substring (cdr (car triple)) 6))))
			       (let ((directory t)
				     user-file)
				 (while directory
				   (setq user-file
					 (read-file-name
					  "Please enter file name: "
					  actual-dir actual-dir t))
				   (if (not (file-directory-p user-file))
				       (setq directory nil)
				     (setq directory t)
				     (beep)
				     (message "%s is a directory" user-file)
				     (sit-for 2)))
				 (setcar triple (cons user-file user-file))))
			   (setcar (car triple)
				   (expand-file-name 
				    (concat actual-dir (car (car triple)))))
			   (setcdr (car triple)
				   (expand-file-name 
				    (concat actual-dir (cdr (car triple))))))
			 ))
	    ediff-patch-map)
    ;; check for the shorter existing file in each pair and discard the other
    ;; one
    (mapcar (function (lambda (triple)
			(let* ((file1 (car (car triple)))
			       (file2 (cdr (car triple)))
			       (f1-exists (file-exists-p file1))
			       (f2-exists (file-exists-p file2)))
			  (cond
			   ((and (< (length file2) (length file1))
				 f2-exists)
			    (setcar triple file2))
			   ((and (< (length file1) (length file2))
				 f1-exists)
			    (setcar triple file1))
			   ((and f1-exists f2-exists
				 (string= file1 file2))
			    (setcar triple file1))
			   ((and f1-exists f2-exists)
			    (with-output-to-temp-buffer ediff-msg-buffer
			      (princ (format "
Ediff has inferred that
	%s
	%s
are possible targets for applying the patch.
Both files seem to be plausible alternatives.

Please advice:
    Type `y' to use %s as the target;
    Type `n' to use %s as the target.
"
					     file1 file2 file2 file1)))
			    (setcar triple
				    (if (y-or-n-p (format "Use %s ? " file2))
					file2 file1)))
			   (f2-exists (setcar triple file2))
			   (f1-exists (setcar triple file1))
			   (t
			    (with-output-to-temp-buffer ediff-msg-buffer
			      (princ (format "
Ediff inferred that 
	%s
	%s
are possible alternative targets for this patch.

However, these files do not exist.

Please enter an alternative patch target ... 
"
					     file1 file2)))
			    (let ((directory t)
				  target)
			      (while directory
				(setq target (read-file-name 
					      "Please enter a patch target: "
					      actual-dir actual-dir t))
				(if (not (file-directory-p target))
				    (setq directory nil)
				  (beep)
				  (message "%s is a directory" target)
				  (sit-for 2)))
			      (setcar triple target)))))))
	    ediff-patch-map)
    ))

(defun ediff-show-patch-diagnostics ()
  (interactive)
  (cond ((window-live-p ediff-window-A)
	 (set-window-buffer ediff-window-A ediff-patch-diagnostics))
	((window-live-p ediff-window-B)
	 (set-window-buffer ediff-window-B ediff-patch-diagnostics))
	(t (display-buffer ediff-patch-diagnostics 'not-this-window))))

(defun ediff-get-patch-buffer ()
  "Obtain patch buffer.  If patch is already in a buffer---use it.
Else, read patch file into a new buffer."
  (let ((dir (cond (ediff-patch-default-directory) ; try patch default dir
		   (ediff-use-last-dir ediff-last-dir-patch)
		   (t default-directory)))
	patch-buf)
    (if (y-or-n-p "Is the patch already in a buffer? ")
	(setq patch-buf
	      (get-buffer
	       (read-buffer
		"Which buffer contains the patch? "
		(current-buffer) 'must-match)))
      (setq patch-buf
	    (find-file-noselect
	     (read-file-name "Which file contains the patch? " dir))))
    
    (ediff-eval-in-buffer patch-buf
      (goto-char (point-min))
      (or (ediff-get-visible-buffer-window patch-buf)
	  (progn
	    (pop-to-buffer patch-buf 'other-window)
	    (select-window (previous-window)))))
    (ediff-map-patch-buffer patch-buf)
    patch-buf))

;; Dispatch the right patch file function: regular or meta-level,
;; depending on how many patches are in the patch file.
;; At present, there is no support for meta-level patches.
;; Should return either the ctl buffer or the meta-buffer
(defun ediff-dispatch-file-patching-job (patch-buf filename
						   &optional startup-hooks)
  (ediff-eval-in-buffer patch-buf
    ;; relativize names in the patch with respect to source-file
    (ediff-fixup-patch-map filename)
    (if (< (length ediff-patch-map) 2)
	(ediff-patch-file-internal
	 patch-buf
	 (if (and (not (string-match "^/dev/null" (car (car ediff-patch-map))))
		  (> (length (car (car ediff-patch-map))) 1))
	     (car (car ediff-patch-map))
	   filename)
	 startup-hooks)
      (ediff-multi-patch-internal patch-buf startup-hooks))
    ))


(defun ediff-patch-buffer-internal (patch-buf buf-to-patch-name
					      &optional startup-hooks)
  (let* ((buf-to-patch (get-buffer buf-to-patch-name))
	 (file-name-ok (if buf-to-patch (buffer-file-name  buf-to-patch)))
	 (buf-mod-status (buffer-modified-p buf-to-patch))
	 (multifile-patch-p (> (length (ediff-eval-in-buffer patch-buf
					 ediff-patch-map)) 1))
	 default-dir file-name ctl-buf)
    (if file-name-ok
	(setq file-name file-name-ok)
      (if multifile-patch-p
	  (error
	   "Can't apply multi-file patches to buffers that visit no files"))
      (ediff-eval-in-buffer buf-to-patch
	(setq default-dir default-directory)
	(setq file-name (ediff-make-temp-file buf-to-patch))
	(set-visited-file-name file-name)
	(setq buffer-auto-save-file-name nil) ; don't create auto-save file
	;;don't confuse the user with a new bufname
	(rename-buffer buf-to-patch-name)
	(set-buffer-modified-p nil)
	(set-visited-file-modtime) ; sync buffer and temp file
	(setq default-directory default-dir)
	))
    
    ;; dispatch a patch function
    (setq ctl-buf (ediff-dispatch-file-patching-job
		   patch-buf file-name startup-hooks))
    
    (if file-name-ok
	()
      ;; buffer wasn't visiting any file,
      ;; so we will not run meta-level ediff here
      (ediff-eval-in-buffer ctl-buf
	(delete-file (buffer-file-name ediff-buffer-A))
	(delete-file (buffer-file-name ediff-buffer-B))
	(ediff-eval-in-buffer ediff-buffer-A
	  (if default-dir (setq default-directory default-dir))
	  (set-visited-file-name nil)
	  (rename-buffer buf-to-patch-name)
	  (set-buffer-modified-p buf-mod-status))
	(ediff-eval-in-buffer ediff-buffer-B
	  (setq buffer-auto-save-file-name nil) ; don't create auto-save file
	  (if default-dir (setq default-directory default-dir))
	  (set-visited-file-name nil)
	  (rename-buffer (ediff-unique-buffer-name 
			  (concat buf-to-patch-name "_patched") ""))
	  (set-buffer-modified-p t))))
    ))

(defun ediff-patch-file-internal (patch-buf source-filename
					    &optional startup-hooks)
  (setq source-filename (expand-file-name source-filename))
  
  (let* ((backup-extension 
	  ;; if the user specified a -b option, extract the backup
	  ;; extension from there; else use ediff-backup-extension
	  (substring ediff-patch-options
		     (if (string-match "-b[ \t]+" ediff-patch-options)
			 (match-end 0) 0)
		     (if (string-match "-b[ \t]+[^ \t]+" ediff-patch-options)
			 (match-end 0) 0)))
	 (shell-file-name ediff-shell)
	 (patch-diagnostics (get-buffer-create "*ediff patch diagnostics*"))
	 ;; ediff-find-file may use a temp file to do the patch
	 ;; so, we save source-filename and true-source-filename as a var
	 ;; that initially is source-filename but may be changed to a temp
	 ;; file for the purpose of patching.
	 (true-source-filename source-filename)
	 (target-filename source-filename)
	 target-buf buf-to-patch file-name-magic-p ctl-buf backup-style)
	  
    ;; if the user didn't specify a backup extension, use
    ;; ediff-backup-extension 
    (if (string= backup-extension "")
	(setq backup-extension ediff-backup-extension))
    (if (string-match "-V" ediff-patch-options)
	(error
	 "Ediff doesn't take the -V option in `ediff-patch-options'--sorry"))
					
    ;; Make a temp file, if source-filename has a magic file handler (or if
    ;; it is handled via auto-mode-alist and similar magic).
    ;; Check if there is a buffer visiting source-filename and if they are in
    ;; sync; arrange for the deletion of temp file.
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

    (ediff-eval-in-buffer patch-diagnostics
      (insert-buffer patch-buf)
      (message "Applying patch ... ")
      ;; fix environment for gnu patch, so it won't make numbered extensions
      (setq backup-style (getenv "VERSION_CONTROL"))
      (setenv "VERSION_CONTROL" nil)
      ;; always pass patch the -f option, so it won't ask any questions
      (shell-command-on-region 
       (point-min) (point-max)
       (format "%s -f %s -b %s %s"
	       ediff-patch-program ediff-patch-options
	       backup-extension
	       (expand-file-name true-source-filename))
       t)
      ;; restore environment for gnu patch
      (setenv "VERSION_CONTROL" backup-style))

    (message "Applying patch ... done")
    (message "")

    (switch-to-buffer patch-diagnostics)
    (sit-for 0) ; synchronize - let the user see diagnostics
    
    (or (file-exists-p (concat true-source-filename backup-extension))
	(error "Patch appears to have failed"))
  
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
	   startup-hooks 'epatch))
    (ediff-eval-in-buffer ctl-buf
      (setq ediff-patchbufer patch-buf
	    ediff-patch-diagnostics patch-diagnostics))
  
    (bury-buffer patch-diagnostics)
    (message "Type `P', if you need to see patch diagnostics")
    ctl-buf))

(defun ediff-multi-patch-internal (patch-buf &optional startup-hooks)
  (let (meta-buf)
    (setq startup-hooks
	  ;; this sets various vars in the meta buffer inside
	  ;; ediff-prepare-meta-buffer
	  (cons (` (lambda ()
		     ;; tell what to do if the user clicks on a session record
		     (setq ediff-session-action-function
			   'ediff-patch-file-form-meta
			   ediff-meta-patchbufer patch-buf)
		     ))
		startup-hooks))
    (setq meta-buf (ediff-prepare-meta-buffer 
		    'ediff-filegroup-action
		    (ediff-eval-in-buffer patch-buf
		      ;; nil replaces a regular expression
		      (cons (list nil (format "%S" patch-buf))
			    ediff-patch-map))
		    "*Ediff Session Group Panel"
		    'ediff-redraw-directory-group-buffer
		    'ediff-multifile-patch
		    startup-hooks))
    (ediff-show-meta-buffer meta-buf)
    ))

  
      

;;; Local Variables:
;;; eval: (put 'ediff-defvar-local 'lisp-indent-hook 'defun)
;;; eval: (put 'ediff-eval-in-buffer 'lisp-indent-hook 1)
;;; End:

(provide 'ediff-ptch)

;;; ediff-ptch.el ends here
