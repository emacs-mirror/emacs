;;; dired-aux.el --- less commonly used parts of dired  -*-byte-compile-dynamic: t;-*-

;; Copyright (C) 1985, 1986, 1992, 1994 Free Software Foundation, Inc.

;; Author: Sebastian Kremer <sk@thp.uni-koeln.de>.

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

;; The parts of dired mode not normally used.  This is a space-saving hack
;; to avoid having to load a large mode when all that's wanted are a few
;; functions.

;; Rewritten in 1990/1991 to add tree features, file marking and
;; sorting by Sebastian Kremer <sk@thp.uni-koeln.de>.
;; Finished up by rms in 1992.

;;; Code:

;; We need macros in dired.el to compile properly.
(eval-when-compile (require 'dired))

;;; 15K
;;;###begin dired-cmd.el
;; Diffing and compressing

;;;###autoload
(defun dired-diff (file &optional switches)
  "Compare file at point with file FILE using `diff'.
FILE defaults to the file at the mark.
The prompted-for file is the first file given to `diff'.
With prefix arg, prompt for second argument SWITCHES,
 which is options for `diff'."
  (interactive
   (let ((default (if (mark t)
		      (save-excursion (goto-char (mark t))
				      (dired-get-filename t t)))))
     (require 'diff)
     (list (read-file-name (format "Diff %s with: %s"
				   (dired-get-filename t)
				   (if default
				       (concat "(default " default ") ")
				     ""))
			   (dired-current-directory) default t)
	   (if current-prefix-arg
	       (read-string "Options for diff: "
			    (if (stringp diff-switches)
				diff-switches
			      (mapconcat 'identity diff-switches " ")))))))
  (diff file (dired-get-filename t) switches))

;;;###autoload
(defun dired-backup-diff (&optional switches)
  "Diff this file with its backup file or vice versa.
Uses the latest backup, if there are several numerical backups.
If this file is a backup, diff it with its original.
The backup file is the first file given to `diff'.
With prefix arg, prompt for argument SWITCHES which is options for `diff'."
  (interactive
    (if current-prefix-arg
	(list (read-string "Options for diff: "
			   (if (stringp diff-switches)
			       diff-switches
			     (mapconcat 'identity diff-switches " "))))
      nil))
  (diff-backup (dired-get-filename) switches))

(defun dired-do-chxxx (attribute-name program op-symbol arg)
  ;; Change file attributes (mode, group, owner) of marked files and
  ;; refresh their file lines.
  ;; ATTRIBUTE-NAME is a string describing the attribute to the user.
  ;; PROGRAM is the program used to change the attribute.
  ;; OP-SYMBOL is the type of operation (for use in dired-mark-pop-up).
  ;; ARG describes which files to use, as in dired-get-marked-files.
  (let* ((files (dired-get-marked-files t arg))
	 (new-attribute
	  (dired-mark-read-string
	   (concat "Change " attribute-name " of %s to: ")
	   nil op-symbol arg files))
	 (operation (concat program " " new-attribute))
	 failures)
    (setq failures
	  (dired-bunch-files 10000
			     (function dired-check-process)
			     (list operation program new-attribute)
			     files))
    (dired-do-redisplay arg);; moves point if ARG is an integer
    (if failures
	(dired-log-summary
	 (format "%s: error" operation)
	 nil))))

;;;###autoload
(defun dired-do-chmod (&optional arg)
  "Change the mode of the marked (or next ARG) files.
This calls chmod, thus symbolic modes like `g+w' are allowed."
  (interactive "P")
  (dired-do-chxxx "Mode" dired-chmod-program 'chmod arg))

;;;###autoload
(defun dired-do-chgrp (&optional arg)
  "Change the group of the marked (or next ARG) files."
  (interactive "P")
  (if (memq system-type '(ms-dos windows-nt))
      (error "chgrp not supported on this system."))
  (dired-do-chxxx "Group" "chgrp" 'chgrp arg))

;;;###autoload
(defun dired-do-chown (&optional arg)
  "Change the owner of the marked (or next ARG) files."
  (interactive "P")
  (if (memq system-type '(ms-dos windows-nt))
      (error "chown not supported on this system."))
  (dired-do-chxxx "Owner" dired-chown-program 'chown arg))

;; Process all the files in FILES in batches of a convenient size,
;; by means of (FUNCALL FUNCTION ARGS... SOME-FILES...).
;; Batches are chosen to need less than MAX chars for the file names,
;; allowing 3 extra characters of separator per file name.
(defun dired-bunch-files (max function args files)
  (let (pending
	(pending-length 0)
	failures)
    ;; Accumulate files as long as they fit in MAX chars,
    ;; then process the ones accumulated so far.
    (while files
      (let* ((thisfile (car files))
	     (thislength (+ (length thisfile) 3))
	     (rest (cdr files)))
	;; If we have at least 1 pending file
	;; and this file won't fit in the length limit, process now.
	(if (and pending (> (+ thislength pending-length) max))
	    (setq failures
		  (nconc (apply function (append args pending))
			 failures)
		  pending nil
		  pending-length 0))
	;; Do (setq pending (cons thisfile pending))
	;; but reuse the cons that was in `files'.
	(setcdr files pending)
	(setq pending files)
	(setq pending-length (+ thislength pending-length))
	(setq files rest)))
    (nconc (apply function (append args pending))
	   failures)))

;;;###autoload
(defun dired-do-print (&optional arg)
  "Print the marked (or next ARG) files.
Uses the shell command coming from variables `lpr-command' and
`lpr-switches' as default."
  (interactive "P")
  (let* ((file-list (dired-get-marked-files t arg))
	 (command (dired-mark-read-string
		   "Print %s with: "
 		   (mapconcat 'identity
			      (cons lpr-command
				    (if (stringp lpr-switches)
					(list lpr-switches)
				      lpr-switches))
			      " ")
		   'print arg file-list)))
    (dired-run-shell-command (dired-shell-stuff-it command file-list nil))))

;; Read arguments for a marked-files command that wants a string
;; that is not a file name,
;; perhaps popping up the list of marked files.
;; ARG is the prefix arg and indicates whether the files came from
;; marks (ARG=nil) or a repeat factor (integerp ARG).
;; If the current file was used, the list has but one element and ARG
;; does not matter. (It is non-nil, non-integer in that case, namely '(4)).

(defun dired-mark-read-string (prompt initial op-symbol arg files)
  ;; PROMPT for a string, with INITIAL input.
  ;; Other args are used to give user feedback and pop-up:
  ;; OP-SYMBOL of command, prefix ARG, marked FILES.
  (dired-mark-pop-up
   nil op-symbol files
   (function read-string)
   (format prompt (dired-mark-prompt arg files)) initial))

;;; Cleaning a directory: flagging some backups for deletion.

(defvar dired-file-version-alist)

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
	(dired-file-version-alist ()))
    (message "Cleaning numerical backups (keeping %d late, %d old)..."
	     late-retention early-retention)
    ;; Look at each file.
    ;; If the file has numeric backup versions,
    ;; put on dired-file-version-alist an element of the form
    ;; (FILENAME . VERSION-NUMBER-LIST)
    (dired-map-dired-file-lines (function dired-collect-file-versions))
    ;; Sort each VERSION-NUMBER-LIST,
    ;; and remove the versions not to be deleted.
    (let ((fval dired-file-version-alist))
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
    (dired-map-dired-file-lines (function dired-trample-file-versions))
    (message "Cleaning numerical backups...done")))

;;; Subroutines of dired-clean-directory.

(defun dired-map-dired-file-lines (fun)
  ;; Perform FUN with point at the end of each non-directory line.
  ;; FUN takes one argument, the filename (complete pathname).
  (save-excursion
    (let (file buffer-read-only)
      (goto-char (point-min))
      (while (not (eobp))
	(save-excursion
	  (and (not (looking-at dired-re-dir))
	       (not (eolp))
	       (setq file (dired-get-filename nil t)) ; nil on non-file
	       (progn (end-of-line)
		      (funcall fun file))))
	(forward-line 1)))))

(defun dired-collect-file-versions (fn)
  (let ((fn (file-name-sans-versions fn)))
    ;; Only do work if this file is not already in the alist.
    (if (assoc fn dired-file-version-alist)
	nil
      ;; If it looks like file FN has versions, return a list of the versions.
      ;;That is a list of strings which are file names.
      ;;The caller may want to flag some of these files for deletion.
      (let* ((base-versions
	      (concat (file-name-nondirectory fn) ".~"))
	     (bv-length (length base-versions))
	     (possibilities (file-name-all-completions
			     base-versions
			     (file-name-directory fn)))
	     (versions (mapcar 'backup-extract-version possibilities)))
	(if versions
	    (setq dired-file-version-alist
		  (cons (cons fn versions)
			dired-file-version-alist)))))))

(defun dired-trample-file-versions (fn)
  (let* ((start-vn (string-match "\\.~[0-9]+~$" fn))
	 base-version-list)
    (and start-vn
	 (setq base-version-list	; there was a base version to which
	       (assoc (substring fn 0 start-vn)	; this looks like a
		      dired-file-version-alist))	; subversion
	 (not (memq (string-to-int (substring fn (+ 2 start-vn)))
		    base-version-list))	; this one doesn't make the cut
	 (progn (beginning-of-line)
		(delete-char 1)
		(insert dired-del-marker)))))

;;; Shell commands
;;>>> install (move this function into simple.el)
(defun dired-shell-quote (filename)
  "Quote a file name for inferior shell (see variable `shell-file-name')."
  ;; Quote everything except POSIX filename characters.
  ;; This should be safe enough even for really weird shells.
  (let ((result "") (start 0) end)
    (while (string-match "[^-0-9a-zA-Z_./]" filename start)
      (setq end (match-beginning 0)
	    result (concat result (substring filename start end)
			   "\\" (substring filename end (1+ end)))
	    start (1+ end)))
    (concat result (substring filename start))))

(defun dired-read-shell-command (prompt arg files)
;;  "Read a dired shell command prompting with PROMPT (using read-string).
;;ARG is the prefix arg and may be used to indicate in the prompt which
;;  files are affected.
;;This is an extra function so that you can redefine it, e.g., to use gmhist."
  (dired-mark-pop-up
   nil 'shell files
   (function read-string)
   (format prompt (dired-mark-prompt arg files))))

;; The in-background argument is only needed in Emacs 18 where
;; shell-command doesn't understand an appended ampersand `&'.
;;;###autoload
(defun dired-do-shell-command (command &optional arg)
  "Run a shell command COMMAND on the marked files.
If no files are marked or a specific numeric prefix arg is given,
the next ARG files are used.  Just \\[universal-argument] means the current file.
The prompt mentions the file(s) or the marker, as appropriate.

If there is output, it goes to a separate buffer.

Normally the command is run on each file individually.
However, if there is a `*' in the command then it is run
just once with the entire file list substituted there.

No automatic redisplay of dired buffers is attempted, as there's no
telling what files the command may have changed.  Type
\\[dired-do-redisplay] to redisplay the marked files.

The shell command has the top level directory as working directory, so
output files usually are created there instead of in a subdir."
;;Functions dired-run-shell-command and dired-shell-stuff-it do the
;;actual work and can be redefined for customization.
  (interactive (list
		;; Want to give feedback whether this file or marked files are used:
		(dired-read-shell-command (concat "! on "
						  "%s: ")
					  current-prefix-arg
					  (dired-get-marked-files
					   t current-prefix-arg))
		current-prefix-arg))
  (let* ((on-each (not (string-match "\\*" command)))
	 (file-list (dired-get-marked-files t arg)))
    (if on-each
	(dired-bunch-files
	 (- 10000 (length command))
	 (function (lambda (&rest files)
		     (dired-run-shell-command
		      (dired-shell-stuff-it command files t arg))))
	 nil
	 file-list)
      ;; execute the shell command
      (dired-run-shell-command
       (dired-shell-stuff-it command file-list nil arg)))))

;; Might use {,} for bash or csh:
(defvar dired-mark-prefix ""
  "Prepended to marked files in dired shell commands.")
(defvar dired-mark-postfix ""
  "Appended to marked files in dired shell commands.")
(defvar dired-mark-separator " "
  "Separates marked files in dired shell commands.")

(defun dired-shell-stuff-it (command file-list on-each &optional raw-arg)
;; "Make up a shell command line from COMMAND and FILE-LIST.
;; If ON-EACH is t, COMMAND should be applied to each file, else
;; simply concat all files and apply COMMAND to this.
;; FILE-LIST's elements will be quoted for the shell."
;; Might be redefined for smarter things and could then use RAW-ARG
;; (coming from interactive P and currently ignored) to decide what to do.
;; Smart would be a way to access basename or extension of file names.
;; See dired-trns.el for an approach to this.
  ;; Bug: There is no way to quote a *
  ;; On the other hand, you can never accidentally get a * into your cmd.
  (let ((stuff-it
	 (if (string-match "\\*" command)
	     (function (lambda (x)
			 (dired-replace-in-string "\\*" x command)))
	   (function (lambda (x) (concat command " " x))))))
    (if on-each
	(mapconcat stuff-it (mapcar 'dired-shell-quote file-list) ";")
      (let ((fns (mapconcat 'dired-shell-quote
			    file-list dired-mark-separator)))
	(if (> (length file-list) 1)
	    (setq fns (concat dired-mark-prefix fns dired-mark-postfix)))
	(funcall stuff-it fns)))))

;; This is an extra function so that it can be redefined by ange-ftp.
(defun dired-run-shell-command (command)
  (shell-command command)
  ;; Return nil for sake of nconc in dired-bunch-files.
  nil)

;; In Emacs 19 this will return program's exit status.
;; This is a separate function so that ange-ftp can redefine it.
(defun dired-call-process (program discard &rest arguments)
;  "Run PROGRAM with output to current buffer unless DISCARD is t.
;Remaining arguments are strings passed as command arguments to PROGRAM."
  (apply 'call-process program nil (not discard) nil arguments))

(defun dired-check-process (msg program &rest arguments)
;  "Display MSG while running PROGRAM, and check for output.
;Remaining arguments are strings passed as command arguments to PROGRAM.
; On error, insert output
; in a log buffer and return the offending ARGUMENTS or PROGRAM.
; Caller can cons up a list of failed args.
;Else returns nil for success."
  (let (err-buffer err (dir default-directory))
    (message "%s..." msg)
    (save-excursion
      ;; Get a clean buffer for error output:
      (setq err-buffer (get-buffer-create " *dired-check-process output*"))
      (set-buffer err-buffer)
      (erase-buffer)
      (setq default-directory dir	; caller's default-directory
	    err (/= 0
		 (apply (function dired-call-process) program nil arguments)))
      (if err
	  (progn
	    (dired-log (concat program " " (prin1-to-string arguments) "\n"))
	    (dired-log err-buffer)
	    (or arguments program t))
	(kill-buffer err-buffer)
	(message "%s...done" msg)
	nil))))

;; Commands that delete or redisplay part of the dired buffer.

(defun dired-kill-line (&optional arg)
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (let (buffer-read-only file)
    (while (/= 0 arg)
      (setq file (dired-get-filename nil t))
      (if (not file)
	  (error "Can only kill file lines.")
	(save-excursion (and file
			     (dired-goto-subdir file)
			     (dired-kill-subdir)))
	(delete-region (progn (beginning-of-line) (point))
		       (progn (forward-line 1) (point)))
	(if (> arg 0)
	    (setq arg (1- arg))
	  (setq arg (1+ arg))
	  (forward-line -1))))
    (dired-move-to-filename)))

;;;###autoload
(defun dired-do-kill-lines (&optional arg fmt)
  "Kill all marked lines (not the files).
With a prefix argument, kill that many lines starting with the current line.
\(A negative argument kills lines before the current line.)
To kill an entire subdirectory, go to its directory header line
and use this command with a prefix argument (the value does not matter)."
  ;; Returns count of killed lines.  FMT="" suppresses message.
  (interactive "P")
  (if arg
      (if (dired-get-subdir)
	  (dired-kill-subdir)
	(dired-kill-line arg))
    (save-excursion
      (goto-char (point-min))
      (let (buffer-read-only (count 0))
	(if (not arg)			; kill marked lines
	    (let ((regexp (dired-marker-regexp)))
	      (while (and (not (eobp))
			  (re-search-forward regexp nil t))
		(setq count (1+ count))
		(delete-region (progn (beginning-of-line) (point))
			       (progn (forward-line 1) (point)))))
	  ;; else kill unmarked lines
	  (while (not (eobp))
	    (if (or (dired-between-files)
		    (not (looking-at "^  ")))
		(forward-line 1)
	      (setq count (1+ count))
	      (delete-region (point) (save-excursion
				       (forward-line 1)
				       (point))))))
	(or (equal "" fmt)
	    (message (or fmt "Killed %d line%s.") count (dired-plural-s count)))
	count))))

;;;###end dired-cmd.el

;;; 30K
;;;###begin dired-cp.el

(defun dired-compress ()
  ;; Compress or uncompress the current file.
  ;; Return nil for success, offending filename else.
  (let* (buffer-read-only
	 (from-file (dired-get-filename))
	 (new-file (dired-compress-file from-file)))
    (if new-file
	(let ((start (point)))
	  ;; Remove any preexisting entry for the name NEW-FILE.
	  (condition-case nil
	      (dired-remove-entry new-file)
	    (error nil))
	  (goto-char start)
	  ;; Now replace the current line with an entry for NEW-FILE.
	  (dired-update-file-line new-file) nil)
      (dired-log (concat "Failed to compress" from-file))
      from-file)))

;;;###autoload
(defun dired-compress-file (file)
  ;; Compress or uncompress FILE.
  ;; Return the name of the compressed or uncompressed file.
  ;; Return nil if no change in files.
  (let ((handler (find-file-name-handler file 'dired-compress-file)))
    (cond (handler
	   (funcall handler 'dired-compress-file file))
	  ((file-symlink-p file)
	   nil)
	  ((let (case-fold-search)
	     (string-match "\\.Z$" file))
	   (if (not (dired-check-process (concat "Uncompressing " file)
					 "uncompress" file))
	       (substring file 0 -2)))
	  ((let (case-fold-search)
	     (string-match "\\.gz$" file))
	   (if (not (dired-check-process (concat "Uncompressing " file)
					 "gunzip" file))
	       (substring file 0 -3)))
	  ;; For .z, try gunzip.  It might be an old gzip file,
	  ;; or it might be from compact? pack? (which?) but gunzip handles
	  ;; both.
	  ((let (case-fold-search)
	     (string-match "\\.z$" file))
	   (if (not (dired-check-process (concat "Uncompressing " file)
					 "gunzip" file))
	       (substring file 0 -2)))
	  (t
	   ;;; Try gzip; if we don't have that, use compress.
	   (condition-case nil
	       (if (not (dired-check-process (concat "Compressing " file)
					     "gzip" "-f" file))
		   (cond ((file-exists-p (concat file ".gz"))
			  (concat file ".gz"))
			 (t (concat file ".z"))))
	     (file-error
	      (if (not (dired-check-process (concat "Compressing " file)
					    "compress" "-f" file))
		  (concat file ".Z"))))))))

(defun dired-mark-confirm (op-symbol arg)
  ;; Request confirmation from the user that the operation described
  ;; by OP-SYMBOL is to be performed on the marked files.
  ;; Confirmation consists in a y-or-n question with a file list
  ;; pop-up unless OP-SYMBOL is a member of `dired-no-confirm'.
  ;; The files used are determined by ARG (as in dired-get-marked-files).
  (or (memq op-symbol dired-no-confirm)
      (let ((files (dired-get-marked-files t arg))
	    (string (if (eq op-symbol 'compress) "Compress or uncompress"
		      (capitalize (symbol-name op-symbol)))))
	(dired-mark-pop-up nil op-symbol files (function y-or-n-p)
			   (concat string " "
				   (dired-mark-prompt arg files) "? ")))))

(defun dired-map-over-marks-check (fun arg op-symbol &optional show-progress)
;  "Map FUN over marked files (with second ARG like in dired-map-over-marks)
; and display failures.

; FUN takes zero args.  It returns non-nil (the offending object, e.g.
; the short form of the filename) for a failure and probably logs a
; detailed error explanation using function `dired-log'.

; OP-SYMBOL is a symbol describing the operation performed (e.g.
; `compress').  It is used with `dired-mark-pop-up' to prompt the user
; (e.g. with `Compress * [2 files]? ') and to display errors (e.g.
; `Failed to compress 1 of 2 files - type W to see why ("foo")')

; SHOW-PROGRESS if non-nil means redisplay dired after each file."
  (if (dired-mark-confirm op-symbol arg)
      (let* ((total-list;; all of FUN's return values
	      (dired-map-over-marks (funcall fun) arg show-progress))
	     (total (length total-list))
	     (failures (delq nil total-list))
	     (count (length failures))
	     (string (if (eq op-symbol 'compress) "Compress or uncompress"
		       (capitalize (symbol-name op-symbol)))))
	(if (not failures)
	    (message "%s: %d file%s."
		     string total (dired-plural-s total))
	  ;; end this bunch of errors:
	  (dired-log-summary
	   (format "Failed to %s %d of %d file%s"
		   (downcase string) count total (dired-plural-s total))
	   failures)))))

(defvar dired-query-alist
  '((?\y . y) (?\040 . y)		; `y' or SPC means accept once
    (?n . n) (?\177 . n)		; `n' or DEL skips once
    (?! . yes)				; `!' accepts rest
    (?q. no) (?\e . no)			; `q' or ESC skips rest
    ;; None of these keys quit - use C-g for that.
    ))

(defun dired-query (qs-var qs-prompt &rest qs-args)
  ;; Query user and return nil or t.
  ;; Store answer in symbol VAR (which must initially be bound to nil).
  ;; Format PROMPT with ARGS.
  ;; Binding variable help-form will help the user who types the help key.
  (let* ((char (symbol-value qs-var))
	 (action (cdr (assoc char dired-query-alist))))
    (cond ((eq 'yes action)
	   t)				; accept, and don't ask again
	  ((eq 'no action)
	   nil)				; skip, and don't ask again
	  (t;; no lasting effects from last time we asked - ask now
	   (let ((qprompt (concat qs-prompt
				  (if help-form
				      (format " [Type yn!q or %s] "
					      (key-description
					       (char-to-string help-char)))
				    " [Type y, n, q or !] ")))
		 result elt)
	     ;; Actually it looks nicer without cursor-in-echo-area - you can
	     ;; look at the dired buffer instead of at the prompt to decide.
	     (apply 'message qprompt qs-args)
	     (setq char (set qs-var (read-char)))
	     (while (not (setq elt (assoc char dired-query-alist)))
	       (message "Invalid char - type %c for help." help-char)
	       (ding)
	       (sit-for 1)
	       (apply 'message qprompt qs-args)
	       (setq char (set qs-var (read-char))))
	     (memq (cdr elt) '(t y yes)))))))

;;;###autoload
(defun dired-do-compress (&optional arg)
  "Compress or uncompress marked (or next ARG) files."
  (interactive "P")
  (dired-map-over-marks-check (function dired-compress) arg 'compress t))

;; Commands for Emacs Lisp files - load and byte compile

(defun dired-byte-compile ()
  ;; Return nil for success, offending file name else.
  (let* ((filename (dired-get-filename))
	 elc-file buffer-read-only failure)
    (condition-case err
	(save-excursion (byte-compile-file filename))
      (error
       (setq failure err)))
    (setq elc-file (byte-compile-dest-file filename))
    (if failure
	(progn
	  (dired-log "Byte compile error for %s:\n%s\n" filename failure)
	  (dired-make-relative filename))
      (dired-remove-file elc-file)
      (forward-line)			; insert .elc after its .el file
      (dired-add-file elc-file)
      nil)))

;;;###autoload
(defun dired-do-byte-compile (&optional arg)
  "Byte compile marked (or next ARG) Emacs Lisp files."
  (interactive "P")
  (dired-map-over-marks-check (function dired-byte-compile) arg 'byte-compile t))

(defun dired-load ()
  ;; Return nil for success, offending file name else.
  (let ((file (dired-get-filename)) failure)
    (condition-case err
      (load file nil nil t)
      (error (setq failure err)))
    (if (not failure)
	nil
      (dired-log "Load error for %s:\n%s\n" file failure)
      (dired-make-relative file))))

;;;###autoload
(defun dired-do-load (&optional arg)
  "Load the marked (or next ARG) Emacs Lisp files."
  (interactive "P")
  (dired-map-over-marks-check (function dired-load) arg 'load t))

;;;###autoload
(defun dired-do-redisplay (&optional arg test-for-subdir)
  "Redisplay all marked (or next ARG) files.
If on a subdir line, redisplay that subdirectory.  In that case,
a prefix arg lets you edit the `ls' switches used for the new listing."
  ;; Moves point if the next ARG files are redisplayed.
  (interactive "P\np")
  (if (and test-for-subdir (dired-get-subdir))
      (dired-insert-subdir
       (dired-get-subdir)
       (if arg (read-string "Switches for listing: " dired-actual-switches)))
    (message "Redisplaying...")
    ;; message much faster than making dired-map-over-marks show progress
    (dired-map-over-marks (let ((fname (dired-get-filename)))
			    (message "Redisplaying... %s" fname)
			    (dired-update-file-line fname))
			  arg)
    (dired-move-to-filename)
    (message "Redisplaying...done")))

(defun dired-update-file-line (file)
  ;; Delete the current line, and insert an entry for FILE.
  ;; If FILE is nil, then just delete the current line.
  ;; Keeps any marks that may be present in column one (doing this
  ;; here is faster than with dired-add-entry's optional arg).
  ;; Does not update other dired buffers.  Use dired-relist-entry for that.
  (beginning-of-line)
  (let ((char (following-char)) (opoint (point))
	(buffer-read-only))
    (delete-region (point) (progn (forward-line 1) (point)))
    (if file
	(progn
	  (dired-add-entry file)
	  ;; Replace space by old marker without moving point.
	  ;; Faster than goto+insdel inside a save-excursion?
	  (subst-char-in-region opoint (1+ opoint) ?\040 char))))
  (dired-move-to-filename))

(defun dired-fun-in-all-buffers (directory fun &rest args)
  ;; In all buffers dired'ing DIRECTORY, run FUN with ARGS.
  ;; Return list of buffers where FUN succeeded (i.e., returned non-nil).
  (let ((buf-list (dired-buffers-for-dir (expand-file-name directory)))
	(obuf (current-buffer))
	buf success-list)
    (while buf-list
      (setq buf (car buf-list)
	    buf-list (cdr buf-list))
      (unwind-protect
	  (progn
	    (set-buffer buf)
	    (if (apply fun args)
		(setq success-list (cons (buffer-name buf) success-list))))
	(set-buffer obuf)))
    success-list))

;;;###autoload
(defun dired-add-file (filename &optional marker-char)
  (dired-fun-in-all-buffers
   (file-name-directory filename)
   (function dired-add-entry) filename marker-char))

(defun dired-add-entry (filename &optional marker-char)
  ;; Add a new entry for FILENAME, optionally marking it
  ;; with MARKER-CHAR (a character, else dired-marker-char is used).
  ;; Note that this adds the entry `out of order' if files sorted by
  ;; time, etc.
  ;; At least this version inserts in the right subdirectory (if present).
  ;; And it skips "." or ".." (see `dired-trivial-filenames').
  ;; Hidden subdirs are exposed if a file is added there.
  (setq filename (directory-file-name filename))
  ;; Entry is always for files, even if they happen to also be directories
  (let ((opoint (point))
	(cur-dir (dired-current-directory))
	(orig-file-name filename)
	(directory (file-name-directory filename))
	reason)
    (setq filename (file-name-nondirectory filename)
	  reason
	  (catch 'not-found
	    (if (string= directory cur-dir)
		(progn
		  (skip-chars-forward "^\r\n")
		  (if (eq (following-char) ?\r)
		      (dired-unhide-subdir))
		  ;; We are already where we should be, except when
		  ;; point is before the subdir line or its total line.
		  (let ((p (dired-after-subdir-garbage cur-dir)))
		    (if (< (point) p)
			(goto-char p))))
	      ;; else try to find correct place to insert
	      (if (dired-goto-subdir directory)
		  (progn;; unhide if necessary
		    (if (looking-at "\r");; point is at end of subdir line
			(dired-unhide-subdir))
		    ;; found - skip subdir and `total' line
		    ;; and uninteresting files like . and ..
		    ;; This better not moves into the next subdir!
		    (dired-goto-next-nontrivial-file))
		;; not found
		(throw 'not-found "Subdir not found")))
	    (let (buffer-read-only opoint)
	      (beginning-of-line)
	      (setq opoint (point))
	      (dired-add-entry-do-indentation marker-char)
	      ;; don't expand `.'.  Show just the file name within directory.
	      (let ((default-directory directory))
		(insert-directory filename
				  (concat dired-actual-switches "d")))
	      ;; Compensate for a bug in ange-ftp.
	      ;; It inserts the file's absolute name, rather than
	      ;; the relative one.  That may be hard to fix since it
	      ;; is probably controlled by something in ftp.
	      (goto-char opoint)	
	      (let ((inserted-name (dired-get-filename 'no-dir)))
		(if (file-name-directory inserted-name)
		    (progn
		      (end-of-line)
		      (delete-char (- (length inserted-name)))
		      (insert filename)
		      (forward-char 1))
		  (forward-line 1)))
	      ;; Give each line a text property recording info about it.
	      (dired-insert-set-properties opoint (point))
	      (forward-line -1)
	      (if dired-after-readin-hook;; the subdir-alist is not affected...
		  (save-excursion;; ...so we can run it right now:
		    (save-restriction
		      (beginning-of-line)
		      (narrow-to-region (point) (save-excursion
						  (forward-line 1) (point)))
		      (run-hooks 'dired-after-readin-hook))))
	      (dired-move-to-filename))
	    ;; return nil if all went well
	    nil))
    (if reason				; don't move away on failure
	(goto-char opoint))
    (not reason)))			; return t on success, nil else

;; This is a separate function for the sake of nested dired format.
(defun dired-add-entry-do-indentation (marker-char)
  ;; two spaces or a marker plus a space:
  (insert (if marker-char
	      (if (integerp marker-char) marker-char dired-marker-char)
	    ?\040)
	  ?\040))

(defun dired-after-subdir-garbage (dir)
  ;; Return pos of first file line of DIR, skipping header and total
  ;; or wildcard lines.
  ;; Important: never moves into the next subdir.
  ;; DIR is assumed to be unhidden.
  ;; Will probably be redefined for VMS etc.
  (save-excursion
    (or (dired-goto-subdir dir) (error "This cannot happen"))
    (forward-line 1)
    (while (and (not (eolp))		; don't cross subdir boundary
		(not (dired-move-to-filename)))
	(forward-line 1))
    (point)))

;;;###autoload
(defun dired-remove-file (file)
  (dired-fun-in-all-buffers
   (file-name-directory file) (function dired-remove-entry) file))

(defun dired-remove-entry (file)
  (save-excursion
    (and (dired-goto-file file)
	 (let (buffer-read-only)
	   (delete-region (progn (beginning-of-line) (point))
			  (save-excursion (forward-line 1) (point)))))))

;;;###autoload
(defun dired-relist-file (file)
  (dired-fun-in-all-buffers (file-name-directory file)
			    (function dired-relist-entry) file))

(defun dired-relist-entry (file)
  ;; Relist the line for FILE, or just add it if it did not exist.
  ;; FILE must be an absolute pathname.
  (let (buffer-read-only marker)
    ;; If cursor is already on FILE's line delete-region will cause
    ;; save-excursion to fail because of floating makers,
    ;; moving point to beginning of line.  Sigh.
    (save-excursion
      (and (dired-goto-file file)
	   (delete-region (progn (beginning-of-line)
				 (setq marker (following-char))
				 (point))
			  (save-excursion (forward-line 1) (point))))
      (setq file (directory-file-name file))
      (dired-add-entry file (if (eq ?\040 marker) nil marker)))))

;;; Copy, move/rename, making hard and symbolic links

(defvar dired-backup-overwrite nil
  "*Non-nil if Dired should ask about making backups before overwriting files.
Special value `always' suppresses confirmation.")

(defvar dired-overwrite-confirmed)

(defun dired-handle-overwrite (to)
  ;; Save old version of a to be overwritten file TO.
  ;; `dired-overwrite-confirmed' and `overwrite-backup-query' are fluid vars
  ;; from dired-create-files.
  (if (and dired-backup-overwrite
	   dired-overwrite-confirmed
	   (or (eq 'always dired-backup-overwrite)
	       (dired-query 'overwrite-backup-query
			(format "Make backup for existing file `%s'? " to))))
      (let ((backup (car (find-backup-file-name to))))
	(rename-file to backup 0)	; confirm overwrite of old backup
	(dired-relist-entry backup))))

;;;###autoload
(defun dired-copy-file (from to ok-flag)
  (dired-handle-overwrite to)
  (copy-file from to ok-flag dired-copy-preserve-time))

;;;###autoload
(defun dired-rename-file (from to ok-flag)
  (dired-handle-overwrite to)
  (rename-file from to ok-flag)		; error is caught in -create-files
  ;; Silently rename the visited file of any buffer visiting this file.
  (and (get-file-buffer from)
       (save-excursion
	 (set-buffer (get-file-buffer from))
	 (let ((modflag (buffer-modified-p)))
	   (set-visited-file-name to)
	   (set-buffer-modified-p modflag))))
  (dired-remove-file from)
  ;; See if it's an inserted subdir, and rename that, too.
  (dired-rename-subdir from to))

(defun dired-rename-subdir (from-dir to-dir)
  (setq from-dir (file-name-as-directory from-dir)
	to-dir (file-name-as-directory to-dir))
  (dired-fun-in-all-buffers from-dir
			    (function dired-rename-subdir-1) from-dir to-dir)
  ;; Update visited file name of all affected buffers
  (let ((expanded-from-dir (expand-file-name from-dir))
	(blist (buffer-list)))
    (while blist
      (save-excursion
	(set-buffer (car blist))
	(if (and buffer-file-name
		 (dired-in-this-tree buffer-file-name expanded-from-dir))
	    (let ((modflag (buffer-modified-p))
		  (to-file (dired-replace-in-string
			    (concat "^" (regexp-quote from-dir))
			    to-dir
			    buffer-file-name)))
	      (set-visited-file-name to-file)
	      (set-buffer-modified-p modflag))))
      (setq blist (cdr blist)))))

(defun dired-rename-subdir-1 (dir to)
  ;; Rename DIR to TO in headerlines and dired-subdir-alist, if DIR or
  ;; one of its subdirectories is expanded in this buffer.
  (let ((expanded-dir (expand-file-name dir))
	(alist dired-subdir-alist)
	(elt nil))
    (while alist
      (setq elt (car alist)
	    alist (cdr alist))
      (if (dired-in-this-tree (car elt) expanded-dir)
	  ;; ELT's subdir is affected by the rename
	  (dired-rename-subdir-2 elt dir to)))
    (if (equal dir default-directory)
	;; if top level directory was renamed, lots of things have to be
	;; updated:
	(progn
	  (dired-unadvertise dir)	; we no longer dired DIR...
	  (setq default-directory to
		dired-directory (expand-file-name;; this is correct
				 ;; with and without wildcards
				 (file-name-nondirectory dired-directory)
				 to))
	  (let ((new-name (file-name-nondirectory
			   (directory-file-name dired-directory))))
	    ;; try to rename buffer, but just leave old name if new
	    ;; name would already exist (don't try appending "<%d>")
	    (or (get-buffer new-name)
		(rename-buffer new-name)))
	  ;; ... we dired TO now:
	  (dired-advertise)))))

(defun dired-rename-subdir-2 (elt dir to)
  ;; Update the headerline and dired-subdir-alist element of directory
  ;; described by alist-element ELT to reflect the moving of DIR to TO.
  ;; Thus, ELT describes either DIR itself or a subdir of DIR.
  (save-excursion
    (let ((regexp (regexp-quote (directory-file-name dir)))
	  (newtext (directory-file-name to))
	  buffer-read-only)
      (goto-char (dired-get-subdir-min elt))
      ;; Update subdir headerline in buffer
      (if (not (looking-at dired-subdir-regexp))
	  (error "%s not found where expected - dired-subdir-alist broken?"
		 dir)
	(goto-char (match-beginning 1))
	(if (re-search-forward regexp (match-end 1) t)
	    (replace-match newtext t t)
	  (error "Expected to find `%s' in headerline of %s" dir (car elt))))
      ;; Update buffer-local dired-subdir-alist
      (setcar elt
	      (dired-normalize-subdir
	       (dired-replace-in-string regexp newtext (car elt)))))))

(defun dired-expand-newtext (string newtext)
  ;; Expand \& and \1..\9 (referring to STRING) in NEWTEXT, using match data.
  ;; Note that in Emacs 18 match data are clipped to current buffer
  ;; size...so the buffer should better not be smaller than STRING.
  (let ((pos 0)
	(len (length newtext))
	(expanded-newtext ""))
    (while (< pos len)
      (setq expanded-newtext
	    (concat expanded-newtext
		    (let ((c (aref newtext pos)))
		      (if (= ?\\ c)
			  (cond ((= ?\& (setq c
					      (aref newtext
						    (setq pos (1+ pos)))))
				 (substring string
					    (match-beginning 0)
					    (match-end 0)))
				((and (>= c ?1) (<= c ?9))
				 ;; return empty string if N'th
				 ;; sub-regexp did not match:
				 (let ((n (- c ?0)))
				   (if (match-beginning n)
				       (substring string
						  (match-beginning n)
						  (match-end n))
				     "")))
				(t
				 (char-to-string c)))
			(char-to-string c)))))
      (setq pos (1+ pos)))
    expanded-newtext))

;; The basic function for half a dozen variations on cp/mv/ln/ln -s.
(defun dired-create-files (file-creator operation fn-list name-constructor
					&optional marker-char)

;; Create a new file for each from a list of existing files.  The user
;; is queried, dired buffers are updated, and at the end a success or
;; failure message is displayed

;; FILE-CREATOR must accept three args: oldfile newfile ok-if-already-exists

;; It is called for each file and must create newfile, the entry of
;; which will be added.  The user will be queried if the file already
;; exists.  If oldfile is removed by FILE-CREATOR (i.e, it is a
;; rename), it is FILE-CREATOR's responsibility to update dired
;; buffers.  FILE-CREATOR must abort by signalling a file-error if it
;; could not create newfile.  The error is caught and logged.

;; OPERATION (a capitalized string, e.g. `Copy') describes the
;; operation performed.  It is used for error logging.

;; FN-LIST is the list of files to copy (full absolute pathnames).

;; NAME-CONSTRUCTOR returns a newfile for every oldfile, or nil to
;; skip.  If it skips files for other reasons than a direct user
;; query, it is supposed to tell why (using dired-log).

;; Optional MARKER-CHAR is a character with which to mark every
;; newfile's entry, or t to use the current marker character if the
;; oldfile was marked.

  (let (failures skipped (success-count 0) (total (length fn-list)))
    (let (to overwrite-query
	     overwrite-backup-query)	; for dired-handle-overwrite
      (mapcar
       (function
	(lambda (from)
	  (setq to (funcall name-constructor from))
	  (if (equal to from)
	      (progn
		(setq to nil)
		(dired-log "Cannot %s to same file: %s\n"
			   (downcase operation) from)))
	  (if (not to)
	      (setq skipped (cons (dired-make-relative from) skipped))
	    (let* ((overwrite (file-exists-p to))
		   (dired-overwrite-confirmed	; for dired-handle-overwrite
		    (and overwrite
			 (let ((help-form '(format "\
Type SPC or `y' to overwrite file `%s',
DEL or `n' to skip to next,
ESC or `q' to not overwrite any of the remaining files,
`!' to overwrite all remaining files with no more questions." to)))
			   (dired-query 'overwrite-query
					"Overwrite `%s'?" to))))
		   ;; must determine if FROM is marked before file-creator
		   ;; gets a chance to delete it (in case of a move).
		   (actual-marker-char
		    (cond  ((integerp marker-char) marker-char)
			   (marker-char (dired-file-marker from)) ; slow
			   (t nil))))
	      (condition-case err
		  (progn
		    (funcall file-creator from to dired-overwrite-confirmed)
		    (if overwrite
			;; If we get here, file-creator hasn't been aborted
			;; and the old entry (if any) has to be deleted
			;; before adding the new entry.
			(dired-remove-file to))
		    (setq success-count (1+ success-count))
		    (message "%s: %d of %d" operation success-count total)
		    (dired-add-file to actual-marker-char))
		(file-error		; FILE-CREATOR aborted
		 (progn
		   (setq failures (cons (dired-make-relative from) failures))
		   (dired-log "%s `%s' to `%s' failed:\n%s\n"
			      operation from to err))))))))
       fn-list))
    (cond
     (failures
      (dired-log-summary
       (format "%s failed for %d of %d file%s"
		operation (length failures) total
		(dired-plural-s total))
       failures))
     (skipped
      (dired-log-summary
       (format "%s: %d of %d file%s skipped"
		operation (length skipped) total
		(dired-plural-s total))
       skipped))
     (t
      (message "%s: %s file%s"
	       operation success-count (dired-plural-s success-count)))))
  (dired-move-to-filename))

(defun dired-do-create-files (op-symbol file-creator operation arg
					     &optional marker-char op1
					     how-to)
  ;; Create a new file for each marked file.
  ;; Prompts user for target, which is a directory in which to create
  ;;   the new files.  Target may be a plain file if only one marked
  ;;   file exists.
  ;; OP-SYMBOL is the symbol for the operation.  Function `dired-mark-pop-up'
  ;;   will determine whether pop-ups are appropriate for this OP-SYMBOL.
  ;; FILE-CREATOR and OPERATION as in dired-create-files.
  ;; ARG as in dired-get-marked-files.
  ;; Optional arg OP1 is an alternate form for OPERATION if there is
  ;;   only one file.
  ;; Optional arg MARKER-CHAR as in dired-create-files.
  ;; Optional arg HOW-TO determines how to treat target:
  ;;   If HOW-TO is not given (or nil), and target is a directory, the
  ;;     file(s) are created inside the target directory.  If target
  ;;     is not a directory, there must be exactly one marked file,
  ;;     else error.
  ;;   If HOW-TO is t, then target is not modified.  There must be
  ;;     exactly one marked file, else error.
  ;; Else HOW-TO is assumed to be a function of one argument, target,
  ;;     that looks at target and returns a value for the into-dir
  ;;     variable.  The function dired-into-dir-with-symlinks is provided
  ;;     for the case (common when creating symlinks) that symbolic
  ;;     links to directories are not to be considered as directories
  ;;     (as file-directory-p would if HOW-TO had been nil).
  (or op1 (setq op1 operation))
  (let* ((fn-list (dired-get-marked-files nil arg))
	 (fn-count (length fn-list))
	 (target (expand-file-name
		   (dired-mark-read-file-name
		    (concat (if (= 1 fn-count) op1 operation) " %s to: ")
		    (dired-dwim-target-directory)
		    op-symbol arg (mapcar (function dired-make-relative) fn-list))))
	 (into-dir (cond ((null how-to) (file-directory-p target))
			 ((eq how-to t) nil)
			 (t (funcall how-to target)))))
    (if (and (> fn-count 1)
	     (not into-dir))
	(error "Marked %s: target must be a directory: %s" operation target))
    ;; rename-file bombs when moving directories unless we do this:
    (or into-dir (setq target (directory-file-name target)))
    (dired-create-files
     file-creator operation fn-list
     (if into-dir			; target is a directory
	 ;; This function uses fluid vars into-dir and target when called
	 ;; inside dired-create-files:
	 (function (lambda (from)
		     (expand-file-name (file-name-nondirectory from) target)))
       (function (lambda (from) target)))
     marker-char)))

;; Read arguments for a marked-files command that wants a file name,
;; perhaps popping up the list of marked files.
;; ARG is the prefix arg and indicates whether the files came from
;; marks (ARG=nil) or a repeat factor (integerp ARG).
;; If the current file was used, the list has but one element and ARG
;; does not matter. (It is non-nil, non-integer in that case, namely '(4)).

(defun dired-mark-read-file-name (prompt dir op-symbol arg files)
  (dired-mark-pop-up
   nil op-symbol files
   (function read-file-name)
   (format prompt (dired-mark-prompt arg files)) dir))

(defun dired-dwim-target-directory ()
  ;; Try to guess which target directory the user may want.
  ;; If there is a dired buffer displayed in the next window, use
  ;; its current subdir, else use current subdir of this dired buffer.
  (let ((this-dir (and (eq major-mode 'dired-mode)
		       (dired-current-directory))))
    ;; non-dired buffer may want to profit from this function, e.g. vm-uudecode
    (if dired-dwim-target
	(let* ((other-buf (window-buffer (next-window)))
	       (other-dir (save-excursion
			    (set-buffer other-buf)
			    (and (eq major-mode 'dired-mode)
				 (dired-current-directory)))))
	  (or other-dir this-dir))
      this-dir)))

;;;###autoload
(defun dired-create-directory (directory)
  "Create a directory called DIRECTORY."
  (interactive
   (list (read-file-name "Create directory: " (dired-current-directory))))
  (let ((expanded (directory-file-name (expand-file-name directory))))
    (make-directory expanded)
    (dired-add-file expanded)
    (dired-move-to-filename)))

(defun dired-into-dir-with-symlinks (target)
  (and (file-directory-p target)
       (not (file-symlink-p target))))
;; This may not always be what you want, especially if target is your
;; home directory and it happens to be a symbolic link, as is often the
;; case with NFS and automounters.  Or if you want to make symlinks
;; into directories that themselves are only symlinks, also quite
;; common.

;; So we don't use this function as value for HOW-TO in
;; dired-do-symlink, which has the minor disadvantage of
;; making links *into* a symlinked-dir, when you really wanted to
;; *overwrite* that symlink.  In that (rare, I guess) case, you'll
;; just have to remove that symlink by hand before making your marked
;; symlinks.

;;;###autoload
(defun dired-do-copy (&optional arg)
  "Copy all marked (or next ARG) files, or copy the current file.
This normally preserves the last-modified date when copying.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory
and new symbolic links are made in that directory
with the same names that the files currently have."
  (interactive "P")
  (dired-do-create-files 'copy (function dired-copy-file)
			   (if dired-copy-preserve-time "Copy [-p]" "Copy")
			   arg dired-keep-marker-copy))

;;;###autoload
(defun dired-do-symlink (&optional arg)
  "Make symbolic links to current file or all marked (or next ARG) files.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory
and new symbolic links are made in that directory
with the same names that the files currently have."
  (interactive "P")
  (dired-do-create-files 'symlink (function make-symbolic-link)
			   "Symlink" arg dired-keep-marker-symlink))

;;;###autoload
(defun dired-do-hardlink (&optional arg)
  "Add names (hard links) current file or all marked (or next ARG) files.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory
and new hard links are made in that directory
with the same names that the files currently have."
  (interactive "P")
  (dired-do-create-files 'hardlink (function add-name-to-file)
			   "Hardlink" arg dired-keep-marker-hardlink))

;;;###autoload
(defun dired-do-rename (&optional arg)
  "Rename current file or all marked (or next ARG) files.
When renaming just the current file, you specify the new name.
When renaming multiple or marked files, you specify a directory."
  (interactive "P")
  (dired-do-create-files 'move (function dired-rename-file)
			 "Move" arg dired-keep-marker-rename "Rename"))
;;;###end dired-cp.el

;;; 5K
;;;###begin dired-re.el
(defun dired-do-create-files-regexp
  (file-creator operation arg regexp newname &optional whole-path marker-char)
  ;; Create a new file for each marked file using regexps.
  ;; FILE-CREATOR and OPERATION as in dired-create-files.
  ;; ARG as in dired-get-marked-files.
  ;; Matches each marked file against REGEXP and constructs the new
  ;;   filename from NEWNAME (like in function replace-match).
  ;; Optional arg WHOLE-PATH means match/replace the whole pathname
  ;;   instead of only the non-directory part of the file.
  ;; Optional arg MARKER-CHAR as in dired-create-files.
  (let* ((fn-list (dired-get-marked-files nil arg))
	 (fn-count (length fn-list))
	 (operation-prompt (concat operation " `%s' to `%s'?"))
	 (rename-regexp-help-form (format "\
Type SPC or `y' to %s one match, DEL or `n' to skip to next,
`!' to %s all remaining matches with no more questions."
					  (downcase operation)
					  (downcase operation)))
	 (regexp-name-constructor
	  ;; Function to construct new filename using REGEXP and NEWNAME:
	  (if whole-path		; easy (but rare) case
	      (function
	       (lambda (from)
		 (let ((to (dired-string-replace-match regexp from newname))
		       ;; must bind help-form directly around call to
		       ;; dired-query
		       (help-form rename-regexp-help-form))
		   (if to
		       (and (dired-query 'rename-regexp-query
					 operation-prompt
					 from
					 to)
			    to)
		     (dired-log "%s: %s did not match regexp %s\n"
				operation from regexp)))))
	    ;; not whole-path, replace non-directory part only
	    (function
	     (lambda (from)
	       (let* ((new (dired-string-replace-match
			    regexp (file-name-nondirectory from) newname))
		      (to (and new	; nil means there was no match
			       (expand-file-name new
						 (file-name-directory from))))
		      (help-form rename-regexp-help-form))
		 (if to
		     (and (dired-query 'rename-regexp-query
				       operation-prompt
				       (dired-make-relative from)
				       (dired-make-relative to))
			  to)
		   (dired-log "%s: %s did not match regexp %s\n"
			      operation (file-name-nondirectory from) regexp)))))))
	 rename-regexp-query)
    (dired-create-files
     file-creator operation fn-list regexp-name-constructor marker-char)))

(defun dired-mark-read-regexp (operation)
  ;; Prompt user about performing OPERATION.
  ;; Read and return list of: regexp newname arg whole-path.
  (let* ((whole-path
	  (equal 0 (prefix-numeric-value current-prefix-arg)))
	 (arg
	  (if whole-path nil current-prefix-arg))
	 (regexp
	  (dired-read-regexp
	   (concat (if whole-path "Path " "") operation " from (regexp): ")))
	 (newname
	  (read-string
	   (concat (if whole-path "Path " "") operation " " regexp " to: "))))
    (list regexp newname arg whole-path)))

;;;###autoload
(defun dired-do-rename-regexp (regexp newname &optional arg whole-path)
  "Rename marked files containing REGEXP to NEWNAME.
As each match is found, the user must type a character saying
  what to do with it.  For directions, type \\[help-command] at that time.
NEWNAME may contain \\=\\<n> or \\& as in `query-replace-regexp'.
REGEXP defaults to the last regexp used.
With a zero prefix arg, renaming by regexp affects the complete
  pathname - usually only the non-directory part of file names is used
  and changed."
  (interactive (dired-mark-read-regexp "Rename"))
  (dired-do-create-files-regexp
   (function dired-rename-file)
   "Rename" arg regexp newname whole-path dired-keep-marker-rename))

;;;###autoload
(defun dired-do-copy-regexp (regexp newname &optional arg whole-path)
  "Copy all marked files containing REGEXP to NEWNAME.
See function `dired-rename-regexp' for more info."
  (interactive (dired-mark-read-regexp "Copy"))
  (dired-do-create-files-regexp
   (function dired-copy-file)
   (if dired-copy-preserve-time "Copy [-p]" "Copy")
   arg regexp newname whole-path dired-keep-marker-copy))

;;;###autoload
(defun dired-do-hardlink-regexp (regexp newname &optional arg whole-path)
  "Hardlink all marked files containing REGEXP to NEWNAME.
See function `dired-rename-regexp' for more info."
  (interactive (dired-mark-read-regexp "HardLink"))
  (dired-do-create-files-regexp
   (function add-name-to-file)
   "HardLink" arg regexp newname whole-path dired-keep-marker-hardlink))

;;;###autoload
(defun dired-do-symlink-regexp (regexp newname &optional arg whole-path)
  "Symlink all marked files containing REGEXP to NEWNAME.
See function `dired-rename-regexp' for more info."
  (interactive (dired-mark-read-regexp "SymLink"))
  (dired-do-create-files-regexp
   (function make-symbolic-link)
   "SymLink" arg regexp newname whole-path dired-keep-marker-symlink))

(defun dired-create-files-non-directory
  (file-creator basename-constructor operation arg)
  ;; Perform FILE-CREATOR on the non-directory part of marked files
  ;; using function BASENAME-CONSTRUCTOR, with query for each file.
  ;; OPERATION like in dired-create-files, ARG as in dired-get-marked-files.
  (let (rename-non-directory-query)
    (dired-create-files
     file-creator
     operation
     (dired-get-marked-files nil arg)
     (function
      (lambda (from)
	(let ((to (concat (file-name-directory from)
			  (funcall basename-constructor
				   (file-name-nondirectory from)))))
	  (and (let ((help-form (format "\
Type SPC or `y' to %s one file, DEL or `n' to skip to next,
`!' to %s all remaining matches with no more questions."
					(downcase operation)
					(downcase operation))))
		 (dired-query 'rename-non-directory-query
			      (concat operation " `%s' to `%s'")
			      (dired-make-relative from)
			      (dired-make-relative to)))
	       to))))
     dired-keep-marker-rename)))

(defun dired-rename-non-directory (basename-constructor operation arg)
  (dired-create-files-non-directory
   (function dired-rename-file)
   basename-constructor operation arg))

;;;###autoload
(defun dired-upcase (&optional arg)
  "Rename all marked (or next ARG) files to upper case."
  (interactive "P")
  (dired-rename-non-directory (function upcase) "Rename upcase" arg))

;;;###autoload
(defun dired-downcase (&optional arg)
  "Rename all marked (or next ARG) files to lower case."
  (interactive "P")
  (dired-rename-non-directory (function downcase) "Rename downcase" arg))

;;;###end dired-re.el

;;; 13K
;;;###begin dired-ins.el

;;;###autoload
(defun dired-maybe-insert-subdir (dirname &optional
					  switches no-error-if-not-dir-p)
  "Insert this subdirectory into the same dired buffer.
If it is already present, just move to it (type \\[dired-do-redisplay] to refresh),
  else inserts it at its natural place (as `ls -lR' would have done).
With a prefix arg, you may edit the ls switches used for this listing.
  You can add `R' to the switches to expand the whole tree starting at
  this subdirectory.
This function takes some pains to conform to `ls -lR' output."
  (interactive
   (list (dired-get-filename)
	 (if current-prefix-arg
	     (read-string "Switches for listing: " dired-actual-switches))))
  (let ((opoint (point)))
    ;; We don't need a marker for opoint as the subdir is always
    ;; inserted *after* opoint.
    (setq dirname (file-name-as-directory dirname))
    (or (and (not switches)
	     (dired-goto-subdir dirname))
	(dired-insert-subdir dirname switches no-error-if-not-dir-p))
    ;; Push mark so that it's easy to find back.  Do this after the
    ;; insert message so that the user sees the `Mark set' message.
    (push-mark opoint)))

(defun dired-insert-subdir (dirname &optional switches no-error-if-not-dir-p)
  "Insert this subdirectory into the same dired buffer.
If it is already present, overwrites previous entry,
  else inserts it at its natural place (as `ls -lR' would have done).
With a prefix arg, you may edit the `ls' switches used for this listing.
  You can add `R' to the switches to expand the whole tree starting at
  this subdirectory.
This function takes some pains to conform to `ls -lR' output."
  ;; NO-ERROR-IF-NOT-DIR-P needed for special filesystems like
  ;; Prospero where dired-ls does the right thing, but
  ;; file-directory-p has not been redefined.
  (interactive
   (list (dired-get-filename)
	 (if current-prefix-arg
	     (read-string "Switches for listing: " dired-actual-switches))))
  (setq dirname (file-name-as-directory (expand-file-name dirname)))
  (dired-insert-subdir-validate dirname switches)
  (or no-error-if-not-dir-p
      (file-directory-p dirname)
      (error  "Attempt to insert a non-directory: %s" dirname))
  (let ((elt (assoc dirname dired-subdir-alist))
	 switches-have-R mark-alist case-fold-search buffer-read-only)
    ;; case-fold-search is nil now, so we can test for capital `R':
    (if (setq switches-have-R (and switches (string-match "R" switches)))
	;; avoid duplicated subdirs
	(setq mark-alist (dired-kill-tree dirname t)))
    (if elt
	;; If subdir is already present, remove it and remember its marks
	(setq mark-alist (nconc (dired-insert-subdir-del elt) mark-alist))
      (dired-insert-subdir-newpos dirname)) ; else compute new position
    (dired-insert-subdir-doupdate
     dirname elt (dired-insert-subdir-doinsert dirname switches))
    (if switches-have-R (dired-build-subdir-alist))
    (dired-initial-position dirname)
    (save-excursion (dired-mark-remembered mark-alist))))

;; This is a separate function for dired-vms.
(defun dired-insert-subdir-validate (dirname &optional switches)
  ;; Check that it is valid to insert DIRNAME with SWITCHES.
  ;; Signal an error if invalid (e.g. user typed `i' on `..').
  (or (dired-in-this-tree dirname (expand-file-name default-directory))
      (error  "%s: not in this directory tree" dirname))
  (if switches
      (let (case-fold-search)
	(mapcar
	 (function
	  (lambda (x)
	    (or (eq (null (string-match x switches))
		    (null (string-match x dired-actual-switches)))
		(error "Can't have dirs with and without -%s switches together"
		       x))))
	 ;; all switches that make a difference to dired-get-filename:
	 '("F" "b")))))

(defun dired-alist-add (dir new-marker)
  ;; Add new DIR at NEW-MARKER.  Sort alist.
  (dired-alist-add-1 dir new-marker)
  (dired-alist-sort))

(defun dired-alist-sort ()
  ;; Keep the alist sorted on buffer position.
  (setq dired-subdir-alist
	(sort dired-subdir-alist
	      (function (lambda (elt1 elt2)
			  (> (dired-get-subdir-min elt1)
			     (dired-get-subdir-min elt2)))))))

(defun dired-kill-tree (dirname &optional remember-marks)
  ;;"Kill all proper subdirs of DIRNAME, excluding DIRNAME itself.
  ;; With optional arg REMEMBER-MARKS, return an alist of marked files."
  (interactive "DKill tree below directory: ")
  (setq dirname (expand-file-name dirname))
  (let ((s-alist dired-subdir-alist) dir m-alist)
    (while s-alist
      (setq dir (car (car s-alist))
	    s-alist (cdr s-alist))
      (if (and (not (string-equal dir dirname))
	       (dired-in-this-tree dir dirname)
	       (dired-goto-subdir dir))
	  (setq m-alist (nconc (dired-kill-subdir remember-marks) m-alist))))
    m-alist))

(defun dired-insert-subdir-newpos (new-dir)
  ;; Find pos for new subdir, according to tree order.
  ;;(goto-char (point-max))
  (let ((alist dired-subdir-alist) elt dir pos new-pos)
    (while alist
      (setq elt (car alist)
	    alist (cdr alist)
	    dir (car elt)
	    pos (dired-get-subdir-min elt))
      (if (dired-tree-lessp dir new-dir)
	  ;; Insert NEW-DIR after DIR
	  (setq new-pos (dired-get-subdir-max elt)
		alist nil)))
    (goto-char new-pos))
  ;; want a separating newline between subdirs
  (or (eobp)
      (forward-line -1))
  (insert "\n")
  (point))

(defun dired-insert-subdir-del (element)
  ;; Erase an already present subdir (given by ELEMENT) from buffer.
  ;; Move to that buffer position.  Return a mark-alist.
  (let ((begin-marker (dired-get-subdir-min element)))
    (goto-char begin-marker)
    ;; Are at beginning of subdir (and inside it!).  Now determine its end:
    (goto-char (dired-subdir-max))
    (or (eobp);; want a separating newline _between_ subdirs:
	(forward-char -1))
    (prog1
	(dired-remember-marks begin-marker (point))
      (delete-region begin-marker (point)))))

(defun dired-insert-subdir-doinsert (dirname switches)
  ;; Insert ls output after point and put point on the correct
  ;; position for the subdir alist.
  ;; Return the boundary of the inserted text (as list of BEG and END).
  (let ((begin (point)) end)
    (message "Reading directory %s..." dirname)
    (let ((dired-actual-switches
	   (or switches
	       (dired-replace-in-string "R" "" dired-actual-switches))))
      (if (equal dirname (car (car (reverse dired-subdir-alist))))
	  ;; top level directory may contain wildcards:
	  (dired-readin-insert dired-directory)
	(let ((opoint (point)))
	  (insert-directory dirname dired-actual-switches nil t)
	  (dired-insert-set-properties opoint (point)))))
    (message "Reading directory %s...done" dirname)
    (setq end (point-marker))
    (indent-rigidly begin end 2)
    ;;  call dired-insert-headerline afterwards, as under VMS dired-ls
    ;;  does insert the headerline itself and the insert function just
    ;;  moves point.
    ;;  Need a marker for END as this inserts text.
    (goto-char begin)
    (dired-insert-headerline dirname)
    ;; point is now like in dired-build-subdir-alist
    (prog1
	(list begin (marker-position end))
      (set-marker end nil))))

(defun dired-insert-subdir-doupdate (dirname elt beg-end)
  ;; Point is at the correct subdir alist position for ELT,
  ;; BEG-END is the subdir-region (as list of begin and end).
  (if elt				; subdir was already present
      ;; update its position (should actually be unchanged)
      (set-marker (dired-get-subdir-min elt) (point-marker))
    (dired-alist-add dirname (point-marker)))
  ;; The hook may depend on the subdir-alist containing the just
  ;; inserted subdir, so run it after dired-alist-add:
  (if dired-after-readin-hook
      (save-excursion
	(let ((begin (nth 0 beg-end))
	      (end (nth 1 beg-end)))
	  (goto-char begin)
	  (save-restriction
	    (narrow-to-region begin end)
	    ;; hook may add or delete lines, but the subdir boundary
	    ;; marker floats
	    (run-hooks 'dired-after-readin-hook))))))

(defun dired-tree-lessp (dir1 dir2)
  ;; Lexicographic order on pathname components, like `ls -lR':
  ;; DIR1 < DIR2 iff DIR1 comes *before* DIR2 in an `ls -lR' listing,
  ;;   i.e., iff DIR1 is a (grand)parent dir of DIR2,
  ;;   or DIR1 and DIR2 are in the same parentdir and their last
  ;;   components are string-lessp.
  ;; Thus ("/usr/" "/usr/bin") and ("/usr/a/" "/usr/b/") are tree-lessp.
  ;; string-lessp could arguably be replaced by file-newer-than-file-p
  ;;   if dired-actual-switches contained `t'.
  (setq dir1 (file-name-as-directory dir1)
	dir2 (file-name-as-directory dir2))
  (let ((components-1 (dired-split "/" dir1))
	(components-2 (dired-split "/" dir2)))
    (while (and components-1
		components-2
		(equal (car components-1) (car components-2)))
      (setq components-1 (cdr components-1)
	    components-2 (cdr components-2)))
    (let ((c1 (car components-1))
	  (c2 (car components-2)))

      (cond ((and c1 c2)
	     (string-lessp c1 c2))
	    ((and (null c1) (null c2))
	     nil)			; they are equal, not lessp
	    ((null c1)			; c2 is a subdir of c1: c1<c2
	     t)
	    ((null c2)			; c1 is a subdir of c2: c1>c2
	     nil)
	    (t (error "This can't happen"))))))

;; There should be a builtin split function - inverse to mapconcat.
(defun dired-split (pat str &optional limit)
  "Splitting on regexp PAT, turn string STR into a list of substrings.
Optional third arg LIMIT (>= 1) is a limit to the length of the
resulting list.
Thus, if SEP is a regexp that only matches itself,

   (mapconcat 'identity (dired-split SEP STRING) SEP)

is always equal to STRING."
  (let* ((start (string-match pat str))
	 (result (list (substring str 0 start)))
	 (count 1)
	 (end (if start (match-end 0))))
    (if end				; else nothing left
	(while (and (or (not (integerp limit))
			(< count limit))
		    (string-match pat str end))
	  (setq start (match-beginning 0)
		count (1+ count)
		result (cons (substring str end start) result)
		end (match-end 0)
		start end)
	  ))
    (if (and (or (not (integerp limit))
		 (< count limit))
	     end)			; else nothing left
	(setq result
	      (cons (substring str end) result)))
    (nreverse result)))

;;; moving by subdirectories

;;;###autoload
(defun dired-prev-subdir (arg &optional no-error-if-not-found no-skip)
  "Go to previous subdirectory, regardless of level.
When called interactively and not on a subdir line, go to this subdir's line."
  ;;(interactive "p")
  (interactive
   (list (if current-prefix-arg
	     (prefix-numeric-value current-prefix-arg)
	   ;; if on subdir start already, don't stay there!
	   (if (dired-get-subdir) 1 0))))
  (dired-next-subdir (- arg) no-error-if-not-found no-skip))

(defun dired-subdir-min ()
  (save-excursion
    (if (not (dired-prev-subdir 0 t t))
	(error "Not in a subdir!")
      (point))))

;;;###autoload
(defun dired-goto-subdir (dir)
  "Go to end of header line of DIR in this dired buffer.
Return value of point on success, otherwise return nil.
The next char is either \\n, or \\r if DIR is hidden."
  (interactive
   (prog1				; let push-mark display its message
       (list (expand-file-name
	      (completing-read "Goto in situ directory: " ; prompt
			       dired-subdir-alist ; table
			       nil	; predicate
			       t	; require-match
			       (dired-current-directory))))
     (push-mark)))
  (setq dir (file-name-as-directory dir))
  (let ((elt (assoc dir dired-subdir-alist)))
    (and elt
	 (goto-char (dired-get-subdir-min elt))
	 ;; dired-subdir-hidden-p and dired-add-entry depend on point being
	 ;; at either \r or \n after this function succeeds.
	 (progn (skip-chars-forward "^\r\n")
		(point)))))

;;;###autoload
(defun dired-mark-subdir-files ()
  "Mark all files except `.' and `..'."
  (interactive)
  (let ((p-min (dired-subdir-min)))
    (dired-mark-files-in-region p-min (dired-subdir-max))))

;;;###autoload
(defun dired-kill-subdir (&optional remember-marks)
  "Remove all lines of current subdirectory.
Lower levels are unaffected."
  ;; With optional REMEMBER-MARKS, return a mark-alist.
  (interactive)
  (let ((beg (dired-subdir-min))
	(end (dired-subdir-max))
	buffer-read-only cur-dir)
    (setq cur-dir (dired-current-directory))
    (if (equal cur-dir default-directory)
	(error "Attempt to kill top level directory"))
    (prog1
	(if remember-marks (dired-remember-marks beg end))
      (delete-region beg end)
      (if (eobp)			; don't leave final blank line
	  (delete-char -1))
      (dired-unsubdir cur-dir))))

(defun dired-unsubdir (dir)
  ;; Remove DIR from the alist
  (setq dired-subdir-alist
	(delq (assoc dir dired-subdir-alist) dired-subdir-alist)))

;;;###autoload
(defun dired-tree-up (arg)
  "Go up ARG levels in the dired tree."
  (interactive "p")
  (let ((dir (dired-current-directory)))
    (while (>= arg 1)
      (setq arg (1- arg)
	    dir (file-name-directory (directory-file-name dir))))
    ;;(setq dir (expand-file-name dir))
    (or (dired-goto-subdir dir)
	(error "Cannot go up to %s - not in this tree." dir))))

;;;###autoload
(defun dired-tree-down ()
  "Go down in the dired tree."
  (interactive)
  (let ((dir (dired-current-directory)) ; has slash
	pos case-fold-search)		; filenames are case sensitive
    (let ((rest (reverse dired-subdir-alist)) elt)
      (while rest
	(setq elt (car rest)
	      rest (cdr rest))
	(if (dired-in-this-tree (directory-file-name (car elt)) dir)
	    (setq rest nil
		  pos (dired-goto-subdir (car elt))))))
    (if pos
	(goto-char pos)
      (error "At the bottom"))))

;;; hiding

(defun dired-unhide-subdir ()
  (let (buffer-read-only)
    (subst-char-in-region (dired-subdir-min) (dired-subdir-max) ?\r ?\n)))

(defun dired-hide-check ()
  (or selective-display
      (error "selective-display must be t for subdir hiding to work!")))

(defun dired-subdir-hidden-p (dir)
  (and selective-display
       (save-excursion
	 (dired-goto-subdir dir)
	 (looking-at "\r"))))

;;;###autoload
(defun dired-hide-subdir (arg)
  "Hide or unhide the current subdirectory and move to next directory.
Optional prefix arg is a repeat factor.
Use \\[dired-hide-all] to (un)hide all directories."
  (interactive "p")
  (dired-hide-check)
  (while (>=  (setq arg (1- arg)) 0)
    (let* ((cur-dir (dired-current-directory))
	   (hidden-p (dired-subdir-hidden-p cur-dir))
	   (elt (assoc cur-dir dired-subdir-alist))
	   (end-pos (1- (dired-get-subdir-max elt)))
	   buffer-read-only)
      ;; keep header line visible, hide rest
      (goto-char (dired-get-subdir-min elt))
      (skip-chars-forward "^\n\r")
      (if hidden-p
	  (subst-char-in-region (point) end-pos ?\r ?\n)
	(subst-char-in-region (point) end-pos ?\n ?\r)))
    (dired-next-subdir 1 t)))

;;;###autoload
(defun dired-hide-all (arg)
  "Hide all subdirectories, leaving only their header lines.
If there is already something hidden, make everything visible again.
Use \\[dired-hide-subdir] to (un)hide a particular subdirectory."
  (interactive "P")
  (dired-hide-check)
  (let (buffer-read-only)
    (if (save-excursion
	  (goto-char (point-min))
	  (search-forward "\r" nil t))
	;; unhide - bombs on \r in filenames
	(subst-char-in-region (point-min) (point-max) ?\r ?\n)
      ;; hide
      (let ((pos (point-max))		; pos of end of last directory
	    (alist dired-subdir-alist))
	(while alist			; while there are dirs before pos
	  (subst-char-in-region (dired-get-subdir-min (car alist)) ; pos of prev dir
				(save-excursion
				  (goto-char pos) ; current dir
				  ;; we're somewhere on current dir's line
				  (forward-line -1)
				  (point))
				?\n ?\r)
	  (setq pos (dired-get-subdir-min (car alist)))	; prev dir gets current dir
	  (setq alist (cdr alist)))))))

;;;###end dired-ins.el


;; Functions for searching in tags style among marked files.

;;;###autoload
(defun dired-do-search (regexp)
  "Search through all marked files for a match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue]."
  (interactive "sSearch marked files (regexp): ")
  (tags-search regexp '(dired-get-marked-files)))

;;;###autoload
(defun dired-do-query-replace (from to &optional delimited)
  "Do `query-replace-regexp' of FROM with TO, on all marked files.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit] or ESC), you can resume the query replace
with the command \\[tags-loop-continue]."
  (interactive
   "sQuery replace in marked files (regexp): \nsQuery replace %s by: \nP")
  (tags-query-replace from to delimited '(dired-get-marked-files)))


(provide 'dired-aux)

;;; dired-aux.el ends here
