;;; autoload.el --- maintain autoloads in loaddefs.el.

;;; Copyright (C) 1991, 1992, 1993, 1994 Free Software Foundation, Inc.
;;;
;; Author: Roland McGrath <roland@gnu.ai.mit.edu>
;; Keywords: maint

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
;;; program's author (send electronic mail to roland@ai.mit.edu) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;

;;; Commentary:

;; This code helps GNU Emacs maintainers keep the loaddefs.el file up to
;; date.  It interprets magic cookies of the form ";;;###autoload" in
;; lisp source files in various useful ways.  To learn more, read the
;; source; if you're going to use this, you'd better be able to.

;;; Code:

(defun make-autoload (form file)
  "Turn FORM, a defun or defmacro, into an autoload for source file FILE.
Returns nil if FORM is not a defun, define-skeleton or defmacro."
  (let ((car (car-safe form)))
    (if (memq car '(defun define-skeleton defmacro))
	(let ((macrop (eq car 'defmacro))
	      name doc)
	  (setq form (cdr form)
		name (car form)
		;; Ignore the arguments.
		form (cdr (if (eq car 'define-skeleton)
			      form
			    (cdr form)))
		doc (car form))
	  (if (stringp doc)
	      (setq form (cdr form))
	    (setq doc nil))
	  (list 'autoload (list 'quote name) file doc
		(or (eq car 'define-skeleton)
		    (eq (car-safe (car form)) 'interactive))
		(if macrop (list 'quote 'macro) nil)))
      nil)))

(put 'define-skeleton 'doc-string-elt 3)

(defconst generate-autoload-cookie ";;;###autoload"
  "Magic comment indicating the following form should be autoloaded.
Used by \\[update-file-autoloads].  This string should be
meaningless to Lisp (e.g., a comment).

This string is used:

;;;###autoload
\(defun function-to-be-autoloaded () ...)

If this string appears alone on a line, the following form will be
read and an autoload made for it.  If there is further text on the line,
that text will be copied verbatim to `generated-autoload-file'.")

(defconst generate-autoload-section-header "\f\n;;;### "
  "String inserted before the form identifying
the section of autoloads for a file.")

(defconst generate-autoload-section-trailer "\n;;;***\n"
  "String which indicates the end of the section of autoloads for a file.")

;;; Forms which have doc-strings which should be printed specially.
;;; A doc-string-elt property of ELT says that (nth ELT FORM) is
;;; the doc-string in FORM.
;;;
;;; There used to be the following note here:
;;; ;;; Note: defconst and defvar should NOT be marked in this way.
;;; ;;; We don't want to produce defconsts and defvars that
;;; ;;; make-docfile can grok, because then it would grok them twice,
;;; ;;; once in foo.el (where they are given with ;;;###autoload) and
;;; ;;; once in loaddefs.el.
;;;
;;; Counter-note: Yes, they should be marked in this way.
;;; make-docfile only processes those files that are loaded into the
;;; dumped Emacs, and those files should never have anything
;;; autoloaded here.  The above-feared problem only occurs with files
;;; which have autoloaded entries *and* are processed by make-docfile;
;;; there should be no such files.

(put 'autoload 'doc-string-elt 3)
(put 'defun    'doc-string-elt 3)
(put 'defvar   'doc-string-elt 3)
(put 'defconst 'doc-string-elt 3)
(put 'defmacro 'doc-string-elt 3)

(defun autoload-trim-file-name (file)
  ;; Returns a relative pathname of FILE
  ;; starting from the directory that loaddefs.el is in.
  ;; That is normally a directory in load-path,
  ;; which means Emacs will be able to find FILE when it looks.
  ;; Any extra directory names here would prevent finding the file.
  (setq file (expand-file-name file))
  (file-relative-name file
		      (file-name-directory generated-autoload-file)))

(defun generate-file-autoloads (file)
  "Insert at point a loaddefs autoload section for FILE.
autoloads are generated for defuns and defmacros in FILE
marked by `generate-autoload-cookie' (which see).
If FILE is being visited in a buffer, the contents of the buffer
are used."
  (interactive "fGenerate autoloads for file: ")
  (let ((outbuf (current-buffer))
	(autoloads-done '())
	(load-name (let ((name (file-name-nondirectory file)))
		     (if (string-match "\\.elc?$" name)
			 (substring name 0 (match-beginning 0))
		       name)))
	(print-length nil)
	(print-readably t)		; This does something in Lucid Emacs.
	(float-output-format nil)
	(done-any nil)
	(visited (get-file-buffer file))
	output-end)

    ;; If the autoload section we create here uses an absolute
    ;; pathname for FILE in its header, and then Emacs is installed
    ;; under a different path on another system,
    ;; `update-autoloads-here' won't be able to find the files to be
    ;; autoloaded.  So, if FILE is in the same directory or a
    ;; subdirectory of the current buffer's directory, we'll make it
    ;; relative to the current buffer's directory.
    (setq file (expand-file-name file))
    (let* ((source-truename (file-truename file))
	   (dir-truename (file-name-as-directory
			  (file-truename default-directory)))
	   (len (length dir-truename)))
      (if (and (< len (length source-truename))
	       (string= dir-truename (substring source-truename 0 len)))
	  (setq file (substring source-truename len))))

    (message "Generating autoloads for %s..." file)
    (save-excursion
      (unwind-protect
	  (progn
	    (set-buffer (find-file-noselect file))
	    (save-excursion
	      (save-restriction
		(widen)
		(goto-char (point-min))
		(while (not (eobp))
		  (skip-chars-forward " \t\n\f")
		  (cond
		   ((looking-at (regexp-quote generate-autoload-cookie))
		    (search-forward generate-autoload-cookie)
		    (skip-chars-forward " \t")
		    (setq done-any t)
		    (if (eolp)
			;; Read the next form and make an autoload.
			(let* ((form (prog1 (read (current-buffer))
				       (or (bolp) (forward-line 1))))
			       (autoload (make-autoload form load-name))
			       (doc-string-elt (get (car-safe form)
						    'doc-string-elt)))
			  (if autoload
			      (setq autoloads-done (cons (nth 1 form)
							 autoloads-done))
			    (setq autoload form))
			  (if (and doc-string-elt
				   (stringp (nth doc-string-elt autoload)))
			      ;; We need to hack the printing because the
			      ;; doc-string must be printed specially for
			      ;; make-docfile (sigh).
			      (let* ((p (nthcdr (1- doc-string-elt)
						autoload))
				     (elt (cdr p)))
				(setcdr p nil)
				(princ "\n(" outbuf)
				(let ((print-escape-newlines t))
				  (mapcar (function (lambda (elt)
						      (prin1 elt outbuf)
						      (princ " " outbuf)))
					  autoload))
				(princ "\"\\\n" outbuf)
				(let ((begin (save-excursion
					       (set-buffer outbuf)
					       (point))))
				  (princ (substring
					  (prin1-to-string (car elt)) 1)
					 outbuf)
				  ;; Insert a backslash before each ( that
				  ;; appears at the beginning of a line in
				  ;; the doc string.
				  (save-excursion
				    (set-buffer outbuf)
				    (save-excursion
				      (while (search-backward "\n(" begin t)
					(forward-char 1)
					(insert "\\"))))
				  (if (null (cdr elt))
				      (princ ")" outbuf)
				    (princ " " outbuf)
				    (princ (substring
					    (prin1-to-string (cdr elt))
					    1)
					   outbuf))
				  (terpri outbuf)))
			    (let ((print-escape-newlines t))
			      (print autoload outbuf))))
			  ;; Copy the rest of the line to the output.
			  (let ((begin (point)))
			    (forward-line 1)
			    (princ (buffer-substring begin (point)) outbuf))))
		   ((looking-at ";")
		    ;; Don't read the comment.
		    (forward-line 1))
		   (t
		    (forward-sexp 1)
		    (forward-line 1)))))))
	(or visited
	    ;; We created this buffer, so we should kill it.
	    (kill-buffer (current-buffer)))
	(set-buffer outbuf)
	(setq output-end (point-marker))))
    (if done-any
	(progn
	  (insert generate-autoload-section-header)
	  (prin1 (list 'autoloads autoloads-done load-name
		       (autoload-trim-file-name file)
		       (nth 5 (file-attributes file)))
		 outbuf)
	  (terpri outbuf)
	  (insert ";;; Generated autoloads from "
		  (autoload-trim-file-name file) "\n")
	  ;; Warn if we put a line in loaddefs.el
	  ;; that is long enough to cause trouble.
	  (while (< (point) output-end)
	    (let ((beg (point)))
	      (end-of-line)
	      (if (> (- (point) beg) 900)
		  (progn
		    (message "A line is too long--over 900 characters")
		    (sleep-for 2)
		    (goto-char output-end))))
	    (forward-line 1))
	  (goto-char output-end)
	  (insert generate-autoload-section-trailer)))
    (message "Generating autoloads for %s...done" file)))

(defconst generated-autoload-file "loaddefs.el"
   "*File \\[update-file-autoloads] puts autoloads into.
A .el file can set this in its local variables section to make its
autoloads go somewhere else.")

;;;###autoload
(defun update-file-autoloads (file)
  "Update the autoloads for FILE in `generated-autoload-file'
\(which FILE might bind in its local variables)."
  (interactive "fUpdate autoloads for file: ")
  (let ((load-name (let ((name (file-name-nondirectory file)))
		     (if (string-match "\\.elc?$" name)
			 (substring name 0 (match-beginning 0))
		       name)))
	(found nil)
	(existing-buffer (get-file-buffer file)))
    (save-excursion
      ;; We want to get a value for generated-autoload-file from
      ;; the local variables section if it's there.
      (set-buffer (find-file-noselect file))
      (set-buffer (find-file-noselect generated-autoload-file))
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  ;; Look for the section for LOAD-NAME.
	  (while (and (not found)
		      (search-forward generate-autoload-section-header nil t))
	    (let ((form (condition-case ()
			    (read (current-buffer))
			  (end-of-file nil))))
	      (cond ((string= (nth 2 form) load-name)
		     ;; We found the section for this file.
		     ;; Check if it is up to date.
		     (let ((begin (match-beginning 0))
			   (last-time (nth 4 form))
			   (file-time (nth 5 (file-attributes file))))
		       (if (and (or (null existing-buffer)
				    (not (buffer-modified-p existing-buffer)))
				(listp last-time) (= (length last-time) 2)
				(or (> (car last-time) (car file-time))
				    (and (= (car last-time) (car file-time))
					 (>= (nth 1 last-time)
					     (nth 1 file-time)))))
			   (progn
			     (message "Autoload section for %s is up to date."
				      file)
			     (setq found 'up-to-date))
			 (search-forward generate-autoload-section-trailer)
			 (delete-region begin (point))
			 (setq found t))))
		    ((string< load-name (nth 2 form))
		     ;; We've come to a section alphabetically later than
		     ;; LOAD-NAME.  We assume the file is in order and so
		     ;; there must be no section for LOAD-NAME.  We will
		     ;; insert one before the section here.
		     (goto-char (match-beginning 0))
		     (setq found 'new)))))
	  (or (eq found 'up-to-date)
	      (and (eq found 'new)
		   ;; Check that FILE has any cookies before generating a
		   ;; new section for it.
		   (save-excursion
		     (set-buffer (find-file-noselect file))
		     (save-excursion
		       (widen)
		       (goto-char (point-min))
		       (if (search-forward (concat "\n"
						   generate-autoload-cookie)
					   nil t)
			   nil
			 (if (interactive-p)
			     (message file " has no autoloads"))
			 t))))
	      (generate-file-autoloads file))))
      (if (interactive-p) (save-buffer))
      (if (and (null existing-buffer)
	       (setq existing-buffer (get-file-buffer file)))
	  (kill-buffer existing-buffer)))))

;;;###autoload
(defun update-autoloads-here ()
  "\
Update sections of the current buffer generated by \\[update-file-autoloads]."
  (interactive)
  (let ((generated-autoload-file (buffer-file-name)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward generate-autoload-section-header nil t)
	(let* ((form (condition-case ()
			 (read (current-buffer))
		       (end-of-file nil)))
	       (file (nth 3 form)))
	  (if (and (stringp file)
		   (or (get-file-buffer file)
		       (file-exists-p file)))
	      ()
	    (setq file (if (y-or-n-p (format "Can't find library `%s'; remove its autoloads? "
					     (nth 2 form) file))
			   t
			 (condition-case ()
			     (read-file-name (format "Find `%s' load file: "
						     (nth 2 form))
					     nil nil t)
			   (quit nil)))))
	  (if file
	      (let ((begin (match-beginning 0)))
		(search-forward generate-autoload-section-trailer)
		(delete-region begin (point))))
	  (if (stringp file)
	      (generate-file-autoloads file)))))))

;;;###autoload
(defun update-directory-autoloads (dir)
  "Run \\[update-file-autoloads] on each .el file in DIR."
  (interactive "DUpdate autoloads for directory: ")
  (let ((enable-local-eval nil))
    (mapcar 'update-file-autoloads
	    (directory-files dir t "^[^=].*\\.el$")))
  (if (interactive-p)
      (save-excursion
	(set-buffer (find-file-noselect generated-autoload-file))
	(save-buffer))))

;;;###autoload
(defun batch-update-autoloads ()
  "Update the autoloads for the files or directories on the command line.
Runs \\[update-file-autoloads] on files and \\[update-directory-autoloads]
on directories.  Must be used only with -batch, and kills Emacs on completion.
Each file will be processed even if an error occurred previously.
For example, invoke `emacs -batch -f batch-update-autoloads *.el'."
  (if (not noninteractive)
      (error "batch-update-autoloads is to be used only with -batch"))
  (let ((lost nil)
	(args command-line-args-left)
	(enable-local-eval nil))	;Don't query in batch mode.
    (message "Updating autoloads in %s..." generated-autoload-file)
    (let ((frob (function
 		 (lambda (file)
 		   (condition-case lossage
 		       (update-file-autoloads file)
 		     (error
 		      (princ ">>Error processing ")
 		      (princ file)
 		      (princ ": ")
 		      (if (fboundp 'display-error)
 			  (display-error lossage nil)
 			(prin1 lossage))
 		      (princ "\n")
 		      (setq lost t)))))))
      (while args
 	(if (file-directory-p (expand-file-name (car args)))
 	    (let ((rest (directory-files (car args) t "\\.el$")))
 	      (while rest
 		(funcall frob (car rest))
 		(setq rest (cdr rest))))
 	  (funcall frob (car args)))
 	(setq args (cdr args))))
    (save-some-buffers t)
    (message "Done")
    (kill-emacs (if lost 1 0))))

(provide 'autoload)

;;; autoload.el ends here
