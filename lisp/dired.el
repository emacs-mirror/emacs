;;; dired.el --- DIRED commands for Emacs

;;; Missing: P command, sorting, setting file modes.
;;; Dired buffer containing multiple directories gets totally confused
;;; Implement insertion of subdirectories in situ --- tree dired

;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;;###autoload
(defvar dired-listing-switches "-al" "\
Switches passed to ls for dired.  MUST contain the `l' option.
CANNOT contain the `F' option.")

(defvar dired-chown-program
  (if (memq system-type '(hpux usg-unix-v))
      "/bin/chown" "/etc/chown")
  "Pathname of chown command.")

(defvar dired-directory nil)

(defun dired-readin (dirname buffer)
  (save-excursion
    (message "Reading directory %s..." dirname)
    (set-buffer buffer)
    (let ((buffer-read-only nil))
      (widen)
      (erase-buffer)
      (setq dirname (expand-file-name dirname))
      (if (eq system-type 'vax-vms)
	  (vms-read-directory dirname dired-listing-switches buffer)
	(if (file-directory-p dirname)
	    (call-process "ls" nil buffer nil
			  dired-listing-switches dirname)
	  (if (not (file-readable-p (directory-file-name (file-name-directory dirname))))
	      (insert "Directory " dirname " inaccessible or nonexistent.\n")
	    (let ((default-directory (file-name-directory dirname)))
	      (call-process shell-file-name nil buffer nil
			    "-c" (concat "ls -d " dired-listing-switches " "
					 (file-name-nondirectory dirname)))))))
      (goto-char (point-min))
      (indent-rigidly (point-min) (point-max) 2))
    (set-buffer-modified-p nil)
    (message "Reading directory %s...done" dirname)))

(defun dired-find-buffer (dirname)
  (let ((blist (buffer-list))
	found)
    (while blist
      (save-excursion
        (set-buffer (car blist))
	(if (and (eq major-mode 'dired-mode)
		 (equal dired-directory dirname))
	    (setq found (car blist)
		  blist nil)
	  (setq blist (cdr blist)))))
    (or found
	(create-file-buffer (directory-file-name dirname)))))

;;;###autoload
(defun dired (dirname)
  "\"Edit\" directory DIRNAME--delete, rename, print, etc. some files in it.
Dired displays the list of files in DIRNAME.
You can move around in it with the usual movement commands.
You can flag files for deletion with \\<dired-mode-map>\\[dired-flag-file-deleted]
and then delete them by typing `x'.
Type `h' after entering dired for more info."
  (interactive (list (read-file-name "Dired (directory): "
				     nil default-directory nil)))
  (switch-to-buffer (dired-noselect dirname)))
;;;###autoload
(define-key ctl-x-map "d" 'dired)

;;;###autoload
(defun dired-other-window (dirname)
  "\"Edit\" directory DIRNAME.  Like `dired' but selects in another window."
  (interactive (list (read-file-name "Dired in other window (directory): "
				     nil default-directory nil)))
  (switch-to-buffer-other-window (dired-noselect dirname)))
;;;###autoload
(define-key ctl-x-4-map "d" 'dired-other-window)

;;;###autoload
(defun dired-noselect (dirname)
  "Like `dired' but returns the dired buffer as value, does not select it."
  (or dirname (setq dirname default-directory))
  (setq dirname (expand-file-name (directory-file-name dirname)))
  (if (file-directory-p dirname)
      (setq dirname (file-name-as-directory dirname)))
  (let ((buffer (dired-find-buffer dirname)))
    (save-excursion
      (set-buffer buffer)
      (dired-readin dirname buffer)
      (while (and (not (dired-move-to-filename)) (not (eobp)))
	(forward-line 1))
      (dired-mode dirname))
    buffer))

(defun dired-revert (&optional arg noconfirm)
  (let ((opoint (point))
	(ofile (dired-get-filename t t))
	(buffer-read-only nil)
	delete-list already-deleted column-dots)
    (goto-char 1)
    (if (re-search-forward "^D" nil t)
	(progn
	  (beginning-of-line)
	  (while (re-search-forward "^D" nil t)
	    (setq delete-list (cons (dired-get-filename t) delete-list)))))
    (dired-readin dired-directory (current-buffer))
    (while (and (not (dired-move-to-filename)) (not (eobp)))
      (forward-line 1))
    (setq column-dots (concat "^" (make-string (current-column) ?.))
	  delete-list (nreverse delete-list))
    (while delete-list
      ;; assumptions: the directory was reread with the files listed in the
      ;; same order as they were originally.  the string of "."s is rather silly
      ;; but it seems the fastest way to avoid messing with -F flags and
      ;; matches that occur in places other than the filename column
      (if (re-search-forward
	   (concat column-dots (regexp-quote (car delete-list))) nil t)
	  (progn (beginning-of-line)
		 (delete-char 1)
		 (insert "D"))
	(setq already-deleted (cons (car delete-list) already-deleted)))
      (setq delete-list (cdr delete-list)))
    (goto-char 0)
    (or (and ofile (re-search-forward (concat column-dots (regexp-quote ofile))
				      nil t))
	(goto-char opoint))
    (dired-move-to-filename)
    (if already-deleted (message "Already deleted: %s"
				 (prin1-to-string (reverse already-deleted))))))

(defvar dired-mode-map nil "Local keymap for dired-mode buffers.")
(if dired-mode-map
    nil
  (setq dired-mode-map (make-keymap))
  (suppress-keymap dired-mode-map)
  (define-key dired-mode-map "r" 'dired-rename-file)
  (define-key dired-mode-map "\C-d" 'dired-flag-file-deleted)
  (define-key dired-mode-map "d" 'dired-flag-file-deleted)
  (define-key dired-mode-map "v" 'dired-view-file)
  (define-key dired-mode-map "e" 'dired-find-file)
  (define-key dired-mode-map "f" 'dired-find-file)
  (define-key dired-mode-map "o" 'dired-find-file-other-window)
  (define-key dired-mode-map "u" 'dired-unflag)
  (define-key dired-mode-map "x" 'dired-do-deletions)
  (define-key dired-mode-map "\177" 'dired-backup-unflag)
  (define-key dired-mode-map "?" 'dired-summary)
  (define-key dired-mode-map "c" 'dired-copy-file)
  (define-key dired-mode-map "#" 'dired-flag-auto-save-files)
  (define-key dired-mode-map "~" 'dired-flag-backup-files)
  (define-key dired-mode-map "F" 'dired-flag-regexp-files)
  (define-key dired-mode-map "." 'dired-clean-directory)
  (define-key dired-mode-map "h" 'describe-mode)
  (define-key dired-mode-map " "  'dired-next-line)
  (define-key dired-mode-map "\C-n" 'dired-next-line)
  (define-key dired-mode-map "\C-p" 'dired-previous-line)
  (define-key dired-mode-map "n" 'dired-next-line)
  (define-key dired-mode-map "p" 'dired-previous-line)
  (define-key dired-mode-map "g" 'revert-buffer)
  (define-key dired-mode-map "D" 'dired-create-directory)
  (define-key dired-mode-map "m" 'dired-move-file)
  (define-key dired-mode-map "C" 'dired-compress)
  (define-key dired-mode-map "U" 'dired-uncompress)
  (define-key dired-mode-map "B" 'dired-byte-recompile)
  (define-key dired-mode-map "M" 'dired-chmod)
  (define-key dired-mode-map "G" 'dired-chgrp)
  (define-key dired-mode-map "O" 'dired-chown)
  (define-key dired-mode-map "=" 'dired-diff)
  (define-key dired-mode-map "<" 'dired-up-directory))


;; Dired mode is suitable only for specially formatted data.
(put 'dired-mode 'mode-class 'special)

(defun dired-mode (&optional dirname)
  "Mode for \"editing\" directory listings.
In dired, you are \"editing\" a list of the files in a directory.
You can move using the usual cursor motion commands.
Letters no longer insert themselves.
Instead, use the following commands:
\\{dired-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'dired-revert)
  (setq major-mode 'dired-mode)
  (setq mode-name "Dired")
  (make-local-variable 'dired-directory)
  (setq dired-directory (or dirname default-directory))
  (make-local-variable 'list-buffers-directory)
  (setq list-buffers-directory dired-directory)
  (set (make-local-variable 'dired-used-F)
       (string-match "F" dired-listing-switches))
  (if dirname
      (setq default-directory
	    (if (file-directory-p dirname)
		dirname (file-name-directory dirname))))
  (setq mode-line-buffer-identification '("Dired: %17f"))
  (setq case-fold-search nil)
  (setq buffer-read-only t)
  (use-local-map dired-mode-map)
  (run-hooks 'dired-mode-hook))

;; FUNCTION receives no arguments
;; and should return t iff it deletes the current line from the buffer.
(defun dired-repeat-over-lines (arg function)
  (beginning-of-line)
  (while (and (> arg 0) (not (eobp)))
    (setq arg (1- arg))
    (let (deleted)
      (save-excursion
	(beginning-of-line)
	(and (bobp) (looking-at "  total")
	     (error "No file on this line"))
	(setq deleted (funcall function)))
      (or deleted
	  (forward-line 1)))
    (dired-move-to-filename))
  (while (and (< arg 0) (not (bobp)))
    (setq arg (1+ arg))
    (forward-line -1)
    (dired-move-to-filename)
    (save-excursion
      (beginning-of-line)
      (funcall function))))

(defun dired-flag-file-deleted (arg)
  "In dired, flag the current line's file for deletion.
With prefix arg, repeat over several lines."
  (interactive "p")
  (dired-repeat-over-lines arg
    '(lambda ()
       (let ((buffer-read-only nil))
	 (delete-char 1)
	 (insert "D")
	 nil))))

(defun dired-flag-regexp-files (regexp)
  "In dired, flag all files matching the specified REGEXP for deletion."
  (interactive "sFlagging regexp: ")
  (save-excursion
   (let ((buffer-read-only nil))
     (goto-char (point-min))
     (while (not (eobp))
       (and (not (looking-at "  d"))
	    (not (eolp))
	    (let ((fn (dired-get-filename t t)))
	      (if fn (string-match regexp fn)))
	    (progn (beginning-of-line)
		   (delete-char 1)
		   (insert "D")))
       (forward-line 1)))))

(defun dired-summary ()
  (interactive)
  ;>> this should check the key-bindings and use substitute-command-keys if non-standard
  (message
   "d-elete, u-ndelete, x-ecute, f-ind, o-ther window, r-ename, c-opy, v-iew"))

(defun dired-unflag (arg)
  "In dired, remove the current line's delete flag then move to next line."
  (interactive "p")
  (dired-repeat-over-lines arg
    '(lambda ()
       (let ((buffer-read-only nil))
	 (delete-char 1)
	 (insert " ")
	 (forward-char -1)
	 nil))))

(defun dired-backup-unflag (arg)
  "In dired, move up lines and remove deletion flag there.
Optional prefix ARG says how many lines to unflag; default is one line."
  (interactive "p")
  (dired-unflag (- arg)))

(defun dired-next-line (arg)
  "Move down lines then position at filename.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (next-line arg)
  (dired-move-to-filename))

(defun dired-previous-line (arg)
  "Move up lines then position at filename.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (previous-line arg)
  (dired-move-to-filename))

(defun dired-up-directory ()
  "Run dired on the parent of the current directory."
  (interactive)
  (find-file ".."))

(defun dired-find-file ()
  "In dired, visit the file or directory named on this line."
  (interactive)
  (find-file (dired-get-filename)))

(defun dired-view-file ()
  "In dired, examine a file in view mode, returning to dired when done."
  (interactive)
  (if (file-directory-p (dired-get-filename))
      (dired (dired-get-filename))
    (view-file (dired-get-filename))))

(defun dired-find-file-other-window ()
  "In dired, visit this file or directory in another window."
  (interactive)
  (find-file-other-window (dired-get-filename)))

(defun dired-get-filename (&optional localp no-error-if-not-filep)
  "In dired, return name of file mentioned on this line.
Value returned normally includes the directory name.
Optional arg LOCALP means don't include it.
Optional arg NO-ERROR-IF-NOT-FILEP means return nil if no filename
on this line, otherwise an error occurs."
  (let (eol file type ex (case-fold-search nil))
    (save-excursion
      (end-of-line)
      (setq eol (point))
      (beginning-of-line)
      (if (eq system-type 'vax-vms)
	  ;; Non-filename lines don't match
	  ;; because they have lower case letters.
	  (if (re-search-forward "^..\\([][.A-Z-0-9_$;<>]+\\)" eol t)
	      (setq file (buffer-substring (match-beginning 1) (match-end 1))))
	;; Unix case
	(if (not (re-search-forward
		  "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)[ ]+[0-9]+"
		  eol t)) ()
	  (skip-chars-forward " ")
	  (skip-chars-forward "^ " eol)
	  (skip-chars-forward " " eol)
	  (setq file (buffer-substring (point) eol))
	  (re-search-backward "\\(.\\)[-r][-w]\\(.\\)[-r][-w]\\(.\\)[-r][-w]\\(.\\)")
	  (setq flag (buffer-substring (match-beginning 1) (match-end 1))
		ex (string-match "[xst]"  ;; execute bit set anywhere?
		    (concat
		     (buffer-substring (match-beginning 2) (match-end 2))
		     (buffer-substring (match-beginning 3) (match-end 3))
		     (buffer-substring (match-beginning 4) (match-end 4)))))
	  (cond
	   ((string= flag "l")
	    ;; strip the link name.  Bombs if file contains " ->"
	    (if (string-match " ->" file)
		(setq file (substring file 0 (match-beginning 0)))))
	   ((and dired-used-F ;; strip off -F stuff if there
		 (or (string= flag "d") (string= flag "s") ex))
	    (setq file (substring file 0 -1)))))))
    (or no-error-if-not-filep file
	(error "No file on this line"))
    ;; ??? uses default-directory, could lose on cd, multiple.
    (or localp (setq file (expand-file-name file default-directory)))
    file))

(defun dired-move-to-filename ()
  "In dired, move to first char of filename on this line.
Returns position (point) or nil if no filename on this line."
  (let ((eol (progn (end-of-line) (point))))
    (beginning-of-line)
    (if (re-search-forward
	 "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)[ ]+[0-9]+"
	 eol t)
	(progn
	  (skip-chars-forward " ")
	  (skip-chars-forward "^ " eol)
	  (skip-chars-forward " " eol)
	  (point)))))

(defun dired-map-dired-file-lines (fn)
  "Perform function FN with point at the end of each non-directory line.
The arguments given to FN are the short and long filename"
  (save-excursion
    (let (filename longfilename (buffer-read-only nil))
      (goto-char (point-min))
      (while (not (eobp))
	(save-excursion
	  (and (not (looking-at " \\s *[0-9]*\\s *[0-9]* d"))
	       (not (eolp))
	       (setq filename (dired-get-filename t t)
		     longfilename (dired-get-filename nil t))
	       (progn (end-of-line)
		      (funcall fn filename longfilename))))
	(forward-line 1)))))

(defun dired-flag-auto-save-files (unflag-p)
  "Flag for deletion files whose names suggest they are auto save files.
A prefix argument says to unflag those files instead."
  (interactive "P")
  (save-excursion
   (let ((buffer-read-only nil))
     (goto-char (point-min))
     (while (not (eobp))
       (and (not (looking-at " \\s *[0-9]*\\s *[0-9]* d"))
	    (not (eolp))
	    (if (fboundp 'auto-save-file-name-p)
		(let ((fn (dired-get-filename t t)))
		  (if fn (auto-save-file-name-p fn)))
	      (if (dired-move-to-filename)
		  (looking-at "#")))
	    (progn (beginning-of-line)
		   (delete-char 1)
		   (insert (if unflag-p " " "D"))))
       (forward-line 1)))))

(defun dired-clean-directory (keep)
  "Flag numerical backups for deletion.
Spares `dired-kept-versions' latest versions, and `kept-old-versions' oldest.
Positive prefix arg KEEP overrides `dired-kept-versions';
Negative prefix arg KEEP overrides `kept-old-versions' with KEEP made positive.

To clear the flags on these files, you can use \\[dired-flag-backup-files]
with a prefix argument."
  (interactive "P")
  (setq keep (if keep (prefix-numeric-value keep) dired-kept-versions))
  (let ((early-retention (if (< keep 0) (- keep) kept-old-versions))
	(late-retention (if (<= keep 0) dired-kept-versions keep))
	(file-version-assoc-list ()))
    ;; Look at each file.
    ;; If the file has numeric backup versions,
    ;; put on file-version-assoc-list an element of the form
    ;; (FILENAME . VERSION-NUMBER-LIST)
    (dired-map-dired-file-lines 'dired-collect-file-versions)
    ;; Sort each VERSION-NUMBER-LIST,
    ;; and remove the versions not to be deleted.
    (let ((fval file-version-assoc-list))
      (while fval
	(let* ((sorted-v-list (cons 'q (sort (cdr (car fval)) '<)))
	       (v-count (length sorted-v-list)))
	  (if (> v-count (+ early-retention late-retention))
	      (rplacd (nthcdr early-retention sorted-v-list)
		      (nthcdr (- v-count late-retention)
			      sorted-v-list)))
	  (rplacd (car fval)
		  (cdr sorted-v-list)))
	(setq fval (cdr fval))))
    ;; Look at each file.  If it is a numeric backup file,
    ;; find it in a VERSION-NUMBER-LIST and maybe flag it for deletion.
    (dired-map-dired-file-lines 'dired-trample-file-versions)))

(defun dired-collect-file-versions (ignore fn)
  "If it looks like file FN has versions, return a list of the versions.
That is a list of strings which are file names.
The caller may want to flag some of these files for deletion."
    (let* ((base-versions
	    (concat (file-name-nondirectory fn) ".~"))
	   (bv-length (length base-versions))
	   (possibilities (file-name-all-completions
			   base-versions
			   (file-name-directory fn)))
	   (versions (mapcar 'backup-extract-version possibilities)))
      (if versions
	  (setq file-version-assoc-list (cons (cons fn versions)
					      file-version-assoc-list)))))

(defun dired-trample-file-versions (ignore fn)
  (let* ((start-vn (string-match "\\.~[0-9]+~$" fn))
	 base-version-list)
    (and start-vn
	 (setq base-version-list	; there was a base version to which
	       (assoc (substring fn 0 start-vn)	; this looks like a
		      file-version-assoc-list))	; subversion
	 (not (memq (string-to-int (substring fn (+ 2 start-vn)))
		    base-version-list))	; this one doesn't make the cut
	 (dired-flag-this-line-for-DEATH))))

(defun dired-flag-this-line-for-DEATH ()
  (beginning-of-line)
  (delete-char 1)
  (insert "D"))

(defun dired-flag-backup-files (unflag-p)
  "Flag all backup files (names ending with `~') for deletion.
With prefix argument, unflag these files."
  (interactive "P")
  (save-excursion
   (let ((buffer-read-only nil))
     (goto-char (point-min))
     (while (not (eobp))
       (and (not (looking-at "  d"))
	    (not (eolp))
	    (if (fboundp 'backup-file-name-p)
		(let ((fn (dired-get-filename t t)))
		  (if fn (backup-file-name-p fn)))
	      (end-of-line)
	      (forward-char -1)
	      (looking-at "~"))
	    (progn (beginning-of-line)
		   (delete-char 1)
		   (insert (if unflag-p " " "D"))))
       (forward-line 1)))))

(defun dired-flag-backup-and-auto-save-files (unflag-p)
  "Flag all backup and temporary files for deletion.
Backup files have names ending in `~'.
Auto save file names usually start with `#'.
With prefix argument, unflag these files."
  (interactive "P")
  (dired-flag-backup-files unflag-p)
  (dired-flag-auto-save-files unflag-p))

(defun dired-create-directory (directory)
  "Create a directory called DIRECTORY."
  (interactive "FCreate directory: ")
  (let ((expanded (expand-file-name directory)))
    (make-directory expanded)
    (dired-add-entry (file-name-directory expanded)
		     (file-name-nondirectory expanded))
  (dired-next-line 1)))

(defun dired-move-file (to-dir &optional count)
  "Move this file to directory TO-DIR.
Optional second argument COUNT (the prefix argument)
specifies moving several consecutive files."
  (interactive
   (let ((count (prefix-numeric-value current-prefix-arg)))
     (list (read-file-name (format "Move %s to directory: "
				   (if (> count 1)
				       (format "%d files" count)
				     (file-name-nondirectory (dired-get-filename))))
			   nil t)
	   count)))
  (let ((dir (file-name-as-directory (expand-file-name to-dir))))
    (dired-repeat-over-lines
     count
     (function (lambda ()
		 (let ((this (dired-get-filename)))
		   (rename-file this
				(expand-file-name (file-name-nondirectory this)
						  dir)))
		 (let ((buffer-read-only nil))
		   (beginning-of-line)
		   (delete-region (point) (progn (forward-line 1) (point))))
		 t)))))

(defun dired-rename-file (to-file)
  "Rename the current file to TO-FILE."
  (interactive
   (list (read-file-name (format "Rename %s to: "
				 (file-name-nondirectory (dired-get-filename)))
			 nil (dired-get-filename))))
  (setq to-file (expand-file-name to-file))
  (let ((filename (dired-get-filename))
	(buffer-read-only nil))
    (rename-file filename to-file)
    (beginning-of-line)
    (delete-region (point) (progn (forward-line 1) (point)))
    (setq to-file (expand-file-name to-file))
    (dired-add-entry (file-name-directory to-file)
		     (file-name-nondirectory to-file))
    ;; Optionally rename the visited file of any buffer visiting this file.
    (and (get-file-buffer filename)
	 (y-or-n-p (message "Change visited file name of buffer %s too? "
			    (buffer-name (get-file-buffer filename))))
	 (save-excursion
	   (set-buffer (get-file-buffer filename))
	   (let ((modflag (buffer-modified-p)))
	     (set-visited-file-name to-file)
	     (set-buffer-modified-p modflag))))))

(defun dired-copy-file (to-file)
  "Copy the current file to TO-FILE."
  (interactive "FCopy to: ")
  (copy-file (dired-get-filename) to-file)
  (setq to-file (expand-file-name to-file))
  (dired-add-entry (file-name-directory to-file)
		   (file-name-nondirectory to-file)))

(defun dired-add-entry (directory filename)
  ;; If tree dired is implemented, this function will have to do
  ;; something smarter with the directory.  Currently, just check
  ;; default directory, if same, add the new entry at point.  With tree
  ;; dired, should call 'dired-current-directory' or similar.  Note
  ;; that this adds the entry 'out of order' if files sorted by time,
  ;; etc.
  (if (string-equal directory default-directory)
      (let ((buffer-read-only nil))
	(beginning-of-line)
	(call-process "ls" nil t nil
		      "-d" dired-listing-switches (concat directory filename))
	(forward-line -1)
	(insert "  ")
	(dired-move-to-filename)
	(let* ((beg (point))
	       (end (progn (end-of-line) (point))))
	  (setq filename (buffer-substring beg end))
	  (delete-region beg end)
	  (insert (file-name-nondirectory filename)))
	(beginning-of-line))))

(defun dired-diff (point mark)
  "Compare files at POINT1 and POINT2 by running `diff'.
Interactively, these are the files at point and mark.
The file at mark (POINT2) is the first file given to `diff'.
See the command `diff'."
  (interactive "d\nm")
  (let (name1 name2)
    (setq name2 (dired-get-filename))
    (save-excursion
      (goto-char mark)
      (setq name1 (dired-get-filename)))
    (diff name1 name2)))

(defun dired-compress ()
  "Compress the current file."
  (interactive)
  (let* ((buffer-read-only nil)
	 (error-buffer (get-buffer-create " *Dired compress output*"))
	 (from-file (dired-get-filename))
	 (to-file (concat from-file ".Z")))
    (if (string-match "\\.Z$" from-file)
	(error "%s is already compressed!" from-file))
    (message "Compressing %s..." from-file)
    (unwind-protect
	(progn
	  (save-excursion
	    (set-buffer error-buffer)
	    (erase-buffer))
	  ;; Must have default-directory of dired buffer in call-process
	  (call-process "compress" nil error-buffer nil "-f" from-file)
	  (if (save-excursion
		(set-buffer error-buffer)
		(= 0 (buffer-size)))
	      (progn
		(message "Compressing %s... done" from-file)
		(kill-buffer error-buffer))
	    (display-buffer error-buffer)
	    (setq error-buffer nil)
	    (error "Compress error on %s." from-file)))
      (if error-buffer (kill-buffer error-buffer)))
    (dired-redisplay to-file)))

(defun dired-uncompress ()
  "Uncompress the current file."
  (interactive)
  (let* ((buffer-read-only nil)
	 (error-buffer (get-buffer-create " *Dired compress output*"))
	 (from-file (dired-get-filename))
	 (to-file (substring from-file 0 -2)))
    (if (string-match "\\.Z$" from-file) nil
	(error "%s is not compressed!" from-file))
    (message "Uncompressing %s..." from-file)
    (unwind-protect
	(progn
	  (save-excursion
	    (set-buffer error-buffer)
	    (erase-buffer))
	  ;; Must have default-directory of dired buffer in call-process
	  (call-process "uncompress" nil error-buffer nil "-f" from-file)
	  (if (save-excursion
		(set-buffer error-buffer)
		(= 0 (buffer-size)))
	      (progn
		(message "Uncompressing %s... done" from-file)
		(kill-buffer error-buffer))
	    (display-buffer error-buffer)
	    (setq error-buffer nil)
	    (error "Uncompress error on %s." from-file)))
      (if error-buffer (kill-buffer error-buffer)))
    (dired-redisplay to-file)))

(defun dired-byte-recompile ()
  "Byte recompile the current file."
  (interactive)
  (let* ((buffer-read-only nil)
	 (from-file (dired-get-filename))
	 (to-file (substring from-file 0 -3)))
    (if (string-match "\\.el$" from-file) nil
	(error "%s is uncompilable!" from-file))
    (byte-compile-file from-file)))

(defun dired-chmod (mode)
  "Change mode of the current file to MODE."
  (interactive "sChange to Mode: ")
  (let ((buffer-read-only nil)
	(file (dired-get-filename)))
    (call-process "/bin/chmod" nil nil nil mode file)
    (dired-redisplay file)))

(defun dired-chgrp (group)
  "Change group of the current file to GROUP."
  (interactive "sChange to Group: ")
  (let ((buffer-read-only nil)
	(file (dired-get-filename)))
    (call-process "/bin/chgrp" nil nil nil group file)
    (dired-redisplay file)))

(defun dired-chown (owner)
  "Change owner of the current file to OWNER."
  (interactive "sChange to Owner: ")
  (let ((buffer-read-only nil)
	(file (dired-get-filename)))
    (call-process dired-chown-program
		  nil nil nil owner file)
    (dired-redisplay file)))

(defun dired-redisplay (&optional file)
  "Delete the current line, and insert an entry for file FILE.
If FILE is nil, then just delete the current line."
  (beginning-of-line)
  (delete-region (point) (progn (forward-line 1) (point)))
  (if file (dired-add-entry (file-name-directory    file)
			    (file-name-nondirectory file)))
  (dired-move-to-filename))

(defun dired-do-deletions ()
  "In dired, delete the files flagged for deletion."
  (interactive)
  (let (delete-list answer)
    (save-excursion
      (goto-char 1)
      (while (re-search-forward "^D" nil t)
	(setq delete-list
	      (cons (cons (dired-get-filename t) (1- (point)))
		    delete-list))))
    (if (null delete-list)
	(message "(No deletions requested)")
      (save-window-excursion
	(set-buffer (get-buffer-create " *Deletions*"))
	(funcall (if (> (length delete-list) (* (window-height) 2))
		     'switch-to-buffer 'switch-to-buffer-other-window)
		 (current-buffer))
	(erase-buffer)
	(setq fill-column 70)
	(let ((l (reverse delete-list)))
	  ;; Files should be in forward order for this loop.
	  (while l
	    (if (> (current-column) 59)
		(insert ?\n)
	      (or (bobp)
		  (indent-to (* (/ (+ (current-column) 19) 20) 20) 1)))
	    (insert (car (car l)))
	    (setq l (cdr l))))
	(goto-char (point-min))
	(setq answer (yes-or-no-p "Delete these files? ")))
      (if answer
	  (let ((l delete-list)
		failures)
	    ;; Files better be in reverse order for this loop!
	    ;; That way as changes are made in the buffer
	    ;; they do not shift the lines still to be changed.
	    (while l
	      (goto-char (cdr (car l)))
	      (let ((buffer-read-only nil))
		(condition-case ()
		    (let ((fn (concat default-directory (car (car l)))))
		      (if (file-directory-p fn)
			  (progn
			    (remove-directory fn)
			    (if (file-exists-p fn) (delete-file fn)))
			(delete-file fn))
		      (delete-region (point)
				     (progn (forward-line 1) (point))))
		  (error (delete-char 1)
			 (insert " ")
			 (setq failures (cons (car (car l)) failures)))))
	      (setq l (cdr l)))
	    (if failures
		(message "Deletions failed: %s"
			 (prin1-to-string failures))))))))

(provide 'dired)

;;; dired.el ends here
