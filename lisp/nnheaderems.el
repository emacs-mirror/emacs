;;; nnheaderems.el --- making Gnus backends work under different Emacsen
;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

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

;;; Code:

(defun nnheader-xmas-run-at-time (time repeat function &rest args)
  (start-itimer
   "nnheader-run-at-time"
   `(lambda ()
      (,function ,@args))
   time repeat))

(defun nnheader-xmas-cancel-timer (timer)
  (delete-itimer timer))

;; Written by Erik Naggum <erik@naggum.no>.
;; Saved by Steve Baur <steve@miranova.com>.
(defun nnheader-xmas-insert-file-contents-literally (filename &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but only reads in the file.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hooks, etc.
  This function ensures that none of these modifications will take place."
  (let (                                ; (file-name-handler-alist nil)
        (format-alist nil)
        (after-insert-file-functions nil)
        (find-buffer-file-type-function 
         (if (fboundp 'find-buffer-file-type)
             (symbol-function 'find-buffer-file-type)
           nil)))
    (unwind-protect
        (progn
          (fset 'find-buffer-file-type (lambda (filename) t))
          (insert-file-contents filename visit beg end replace))
      (if find-buffer-file-type-function
          (fset 'find-buffer-file-type find-buffer-file-type-function)
        (fmakunbound 'find-buffer-file-type)))))

(defun nnheader-xmas-find-file-noselect (filename &optional nowarn rawfile)
  "Read file FILENAME into a buffer and return the buffer.
If a buffer exists visiting FILENAME, return that one, but
verify that the file has not changed since visited or saved.
The buffer is not selected, just returned to the caller."
  (setq filename
	(abbreviate-file-name
	 (expand-file-name filename)))
  (if (file-directory-p filename)
      (if find-file-run-dired
	  (dired-noselect filename)
	(error "%s is a directory." filename))
    (let* ((buf (get-file-buffer filename))
	   (truename (abbreviate-file-name (file-truename filename)))
	   (number (nthcdr 10 (file-attributes truename)))
	   ;; Find any buffer for a file which has same truename.
	   (other (and (not buf) 
		       (if (fboundp 'find-buffer-visiting)
			   (find-buffer-visiting filename)
			 (get-file-buffer filename))))
	   error)
      ;; Let user know if there is a buffer with the same truename.
      (if other
	  (progn
	    (or nowarn
		(string-equal filename (buffer-file-name other))
		(message "%s and %s are the same file"
			 filename (buffer-file-name other)))
	    ;; Optionally also find that buffer.
	    (if (or (and (boundp 'find-file-existing-other-name)
			 find-file-existing-other-name)
		    find-file-visit-truename)
		(setq buf other))))
      (if buf
	  (or nowarn
	      (verify-visited-file-modtime buf)
	      (cond ((not (file-exists-p filename))
		     (error "File %s no longer exists!" filename))
		    ((yes-or-no-p
		      (if (string= (file-name-nondirectory filename)
				   (buffer-name buf))
			  (format
			   (if (buffer-modified-p buf)
			       "File %s changed on disk.  Discard your edits? "
			     "File %s changed on disk.  Reread from disk? ")
			   (file-name-nondirectory filename))
			(format
			 (if (buffer-modified-p buf)
			     "File %s changed on disk.  Discard your edits in %s? "
			   "File %s changed on disk.  Reread from disk into %s? ")
			 (file-name-nondirectory filename)
			 (buffer-name buf))))
		     (save-excursion
		       (set-buffer buf)
		       (revert-buffer t t)))))
	(save-excursion
;;; The truename stuff makes this obsolete.
;;;	  (let* ((link-name (car (file-attributes filename)))
;;;		 (linked-buf (and (stringp link-name)
;;;				  (get-file-buffer link-name))))
;;;	    (if (bufferp linked-buf)
;;;		(message "Symbolic link to file in buffer %s"
;;;			 (buffer-name linked-buf))))
	  (setq buf (create-file-buffer filename))
	  ;;	  (set-buffer-major-mode buf)
	  (set-buffer buf)
	  (erase-buffer)
	  (if rawfile
	      (condition-case ()
		  (nnheader-insert-file-contents-literally filename t)
		(file-error
		 ;; Unconditionally set error
		 (setq error t)))
	    (condition-case ()
		(insert-file-contents filename t)
	      (file-error
	       ;; Run find-file-not-found-hooks until one returns non-nil.
	       (or t			; (run-hook-with-args-until-success 'find-file-not-found-hooks)
		   ;; If they fail too, set error.
		   (setq error t)))))
	  ;; Find the file's truename, and maybe use that as visited name.
	  (setq buffer-file-truename truename)
	  (setq buffer-file-number number)
	  ;; On VMS, we may want to remember which directory in a search list
	  ;; the file was found in.
	  (and (eq system-type 'vax-vms)
	       (let (logical)
		 (if (string-match ":" (file-name-directory filename))
		     (setq logical (substring (file-name-directory filename)
					      0 (match-beginning 0))))
		 (not (member logical find-file-not-true-dirname-list)))
	       (setq buffer-file-name buffer-file-truename))
	  (if find-file-visit-truename
	      (setq buffer-file-name
		    (setq filename
			  (expand-file-name buffer-file-truename))))
	  ;; Set buffer's default directory to that of the file.
	  (setq default-directory (file-name-directory filename))
	  ;; Turn off backup files for certain file names.  Since
	  ;; this is a permanent local, the major mode won't eliminate it.
	  (and (not (funcall backup-enable-predicate buffer-file-name))
	       (progn
		 (make-local-variable 'backup-inhibited)
		 (setq backup-inhibited t)))
	  (if rawfile
	      nil
	    (after-find-file error (not nowarn)))))
      buf)))

(defun nnheader-ms-strip-cr ()
  "Strip ^M from the end of all lines."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\r$" nil t)
      (delete-backward-char 1))))

(eval-and-compile
  (cond 
   ;; Do XEmacs function bindings.
   ((string-match "XEmacs\\|Lucid" emacs-version)
    (fset 'nnheader-run-at-time 'nnheader-xmas-run-at-time)
    (fset 'nnheader-cancel-timer 'nnheader-xmas-cancel-timer)
    (fset 'nnheader-find-file-noselect 'nnheader-xmas-find-file-noselect)
    (fset 'nnheader-insert-file-contents-literally
	  (if (fboundp 'insert-file-contents-literally)
	      'insert-file-contents-literally
	    'nnheader-xmas-insert-file-contents-literally)))
   ;; Do Emacs function bindings.
   (t
    (fset 'nnheader-run-at-time 'run-at-time)
    (fset 'nnheader-cancel-timer 'cancel-timer)
    (fset 'nnheader-find-file-noselect 'find-file-noselect)
    (fset 'nnheader-insert-file-contents-literally
	  'insert-file-contents-literally)
    ))
  (when (memq system-type '(windows-nt))
    (add-hook 'nnmail-prepare-incoming-hook 'nnheader-ms-strip-cr)))

(provide 'nnheaderems)

;;; nnheaderems.el ends here.
