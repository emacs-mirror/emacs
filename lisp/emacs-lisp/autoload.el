;; autoload.el --- maintain autoloads in loaddefs.el

;; Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996, 1997, 2001, 2002, 2003,
;;   2004, 2005 Free Software Foundation, Inc.

;; Author: Roland McGrath <roland@gnu.org>
;; Keywords: maint

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This code helps GNU Emacs maintainers keep the loaddefs.el file up to
;; date.  It interprets magic cookies of the form ";;;###autoload" in
;; lisp source files in various useful ways.  To learn more, read the
;; source; if you're going to use this, you'd better be able to.

;;; Code:

(require 'lisp-mode)			;for `doc-string-elt' properties.
(require 'help-fns)			;for help-add-fundoc-usage.
(eval-when-compile (require 'cl))

(defvar generated-autoload-file "loaddefs.el"
   "*File \\[update-file-autoloads] puts autoloads into.
A `.el' file can set this in its local variables section to make its
autoloads go somewhere else.  The autoload file is assumed to contain a
trailer starting with a FormFeed character.")

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
  "String that marks the form at the start of a new file's autoload section.")

(defconst generate-autoload-section-trailer "\n;;;***\n"
  "String which indicates the end of the section of autoloads for a file.")

(defconst generate-autoload-section-continuation ";;;;;; "
  "String to add on each continuation of the section header form.")

(defun make-autoload (form file)
  "Turn FORM into an autoload or defvar for source file FILE.
Returns nil if FORM is not a special autoload form (i.e. a function definition
or macro definition or a defcustom)."
  (let ((car (car-safe form)) expand)
    (cond
     ;; For complex cases, try again on the macro-expansion.
     ((and (memq car '(easy-mmode-define-global-mode define-global-minor-mode
		       easy-mmode-define-minor-mode define-minor-mode))
	   (setq expand (let ((load-file-name file)) (macroexpand form)))
	   (eq (car expand) 'progn)
	   (memq :autoload-end expand))
      (let ((end (memq :autoload-end expand)))
	;; Cut-off anything after the :autoload-end marker.
	(setcdr end nil)
	(cons 'progn
	      (mapcar (lambda (form) (make-autoload form file))
		      (cdr expand)))))

     ;; For special function-like operators, use the `autoload' function.
     ((memq car '(defun define-skeleton defmacro define-derived-mode
                   define-compilation-mode
		   define-generic-mode easy-mmode-define-minor-mode
		   easy-mmode-define-global-mode
		   define-minor-mode defun* defmacro*))
      (let* ((macrop (memq car '(defmacro defmacro*)))
	     (name (nth 1 form))
	     (args (case car
		    ((defun defmacro defun* defmacro*) (nth 2 form))
		    ((define-skeleton) '(&optional str arg))
		    ((define-generic-mode define-derived-mode
                       define-compilation-mode) nil)
		    (t)))
	     (body (nthcdr (get car 'doc-string-elt) form))
	     (doc (if (stringp (car body)) (pop body))))
	(when (listp args)
	  ;; Add the usage form at the end where describe-function-1
	  ;; can recover it.
	  (setq doc (help-add-fundoc-usage doc args)))
	;; `define-generic-mode' quotes the name, so take care of that
	(list 'autoload (if (listp name) name (list 'quote name)) file doc
	      (or (and (memq car '(define-skeleton define-derived-mode
				    define-generic-mode
				    easy-mmode-define-global-mode
				    easy-mmode-define-minor-mode
				    define-minor-mode)) t)
		  (eq (car-safe (car body)) 'interactive))
	      (if macrop (list 'quote 'macro) nil))))

     ;; Convert defcustom to less space-consuming data.
     ((eq car 'defcustom)
      (let ((varname (car-safe (cdr-safe form)))
	    (init (car-safe (cdr-safe (cdr-safe form))))
	    (doc (car-safe (cdr-safe (cdr-safe (cdr-safe form)))))
	    ;; (rest (cdr-safe (cdr-safe (cdr-safe (cdr-safe form)))))
	    )
	`(progn
	   (defvar ,varname ,init ,doc)
	   (custom-autoload ',varname ,file)
           ;; The use of :require in a defcustom can be annoying, especially
           ;; when defcustoms are moved from one file to another between
           ;; releases because the :require arg gets placed in the user's
           ;; .emacs.  In order for autoloaded minor modes not to need the
           ;; use of :require, we arrange to store their :setter.
           ,(let ((setter (condition-case nil
                              (cadr (memq :set form))
                            (error nil))))
              (if (equal setter ''custom-set-minor-mode)
                  `(put ',varname 'custom-set 'custom-set-minor-mode))))))

     ;; nil here indicates that this is not a special autoload form.
     (t nil))))

;; Forms which have doc-strings which should be printed specially.
;; A doc-string-elt property of ELT says that (nth ELT FORM) is
;; the doc-string in FORM.
;; Those properties are now set in lisp-mode.el.


(defun autoload-trim-file-name (file)
  ;; Returns a relative file path for FILE
  ;; starting from the directory that loaddefs.el is in.
  ;; That is normally a directory in load-path,
  ;; which means Emacs will be able to find FILE when it looks.
  ;; Any extra directory names here would prevent finding the file.
  (setq file (expand-file-name file))
  (file-relative-name file
		      (file-name-directory generated-autoload-file)))

(defun autoload-read-section-header ()
  "Read a section header form.
Since continuation lines have been marked as comments,
we must copy the text of the form and remove those comment
markers before we call `read'."
  (save-match-data
    (let ((beginning (point))
	  string)
      (forward-line 1)
      (while (looking-at generate-autoload-section-continuation)
	(forward-line 1))
      (setq string (buffer-substring beginning (point)))
      (with-current-buffer (get-buffer-create " *autoload*")
	(erase-buffer)
	(insert string)
	(goto-char (point-min))
	(while (search-forward generate-autoload-section-continuation nil t)
	  (replace-match " "))
	(goto-char (point-min))
	(read (current-buffer))))))

(defvar autoload-print-form-outbuf nil
  "Buffer which gets the output of `autoload-print-form'.")

(defun autoload-print-form (form)
  "Print FORM such that `make-docfile' will find the docstrings.
The variable `autoload-print-form-outbuf' specifies the buffer to
put the output in."
  (cond
   ;; If the form is a sequence, recurse.
   ((eq (car form) 'progn) (mapcar 'autoload-print-form (cdr form)))
   ;; Symbols at the toplevel are meaningless.
   ((symbolp form) nil)
   (t
    (let ((doc-string-elt (get (car-safe form) 'doc-string-elt))
	  (outbuf autoload-print-form-outbuf))
      (if (and doc-string-elt (stringp (nth doc-string-elt form)))
	  ;; We need to hack the printing because the
	  ;; doc-string must be printed specially for
	  ;; make-docfile (sigh).
	  (let* ((p (nthcdr (1- doc-string-elt) form))
		 (elt (cdr p)))
	    (setcdr p nil)
	    (princ "\n(" outbuf)
	    (let ((print-escape-newlines t)
		  (print-escape-nonascii t))
	      (dolist (elt form)
		(prin1 elt outbuf)
		(princ " " outbuf)))
	    (princ "\"\\\n" outbuf)
	    (let ((begin (with-current-buffer outbuf (point))))
	      (princ (substring (prin1-to-string (car elt)) 1)
		     outbuf)
	      ;; Insert a backslash before each ( that
	      ;; appears at the beginning of a line in
	      ;; the doc string.
	      (with-current-buffer outbuf
		(save-excursion
		  (while (re-search-backward "\n[[(]" begin t)
		    (forward-char 1)
		    (insert "\\"))))
	      (if (null (cdr elt))
		  (princ ")" outbuf)
		(princ " " outbuf)
		(princ (substring (prin1-to-string (cdr elt)) 1)
		       outbuf))
	      (terpri outbuf)))
	(let ((print-escape-newlines t)
	      (print-escape-nonascii t))
	  (print form outbuf)))))))

(defun autoload-ensure-default-file (file)
  "Make sure that the autoload file FILE exists and if not create it."
  (unless (file-exists-p file)
    (write-region
     (concat ";;; " (file-name-nondirectory file)
	     " --- automatically extracted autoloads\n"
	     ";;\n"
	     ";;; Code:\n\n"
	     "\n;; Local Variables:\n"
	     ";; version-control: never\n"
	     ";; no-byte-compile: t\n"
	     ";; no-update-autoloads: t\n"
	     ";; End:\n"
	     ";;; " (file-name-nondirectory file)
	     " ends here\n")
     nil file))
  file)

(defun autoload-insert-section-header (outbuf autoloads load-name file time)
  "Insert the section-header line,
which lists the file name and which functions are in it, etc."
  (insert generate-autoload-section-header)
  (prin1 (list 'autoloads autoloads load-name
	       (if (stringp file) (autoload-trim-file-name file) file)
	       time)
	 outbuf)
  (terpri outbuf)
  ;; Break that line at spaces, to avoid very long lines.
  ;; Make each sub-line into a comment.
  (with-current-buffer outbuf
    (save-excursion
      (forward-line -1)
      (while (not (eolp))
	(move-to-column 64)
	(skip-chars-forward "^ \n")
	(or (eolp)
	    (insert "\n" generate-autoload-section-continuation))))))

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
		     (if (string-match "\\.elc?\\(\\.\\|$\\)" name)
			 (substring name 0 (match-beginning 0))
		       name)))
	(print-length nil)
	(print-readably t)		; This does something in Lucid Emacs.
	(float-output-format nil)
	(done-any nil)
	(visited (get-file-buffer file))
	output-end)

    ;; If the autoload section we create here uses an absolute
    ;; file name for FILE in its header, and then Emacs is installed
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
	    (if visited
		(set-buffer visited)
	      ;; It is faster to avoid visiting the file.
	      (set-buffer (get-buffer-create " *generate-autoload-file*"))
	      (kill-all-local-variables)
	      (erase-buffer)
	      (setq buffer-undo-list t
		    buffer-read-only nil)
	      (emacs-lisp-mode)
	      (insert-file-contents file nil))
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
			       (autoload (make-autoload form load-name)))
			  (if autoload
			      (setq autoloads-done (cons (nth 1 form)
							 autoloads-done))
			    (setq autoload form))
			  (let ((autoload-print-form-outbuf outbuf))
			    (autoload-print-form autoload)))

		      ;; Copy the rest of the line to the output.
		      (princ (buffer-substring
			      (progn
				;; Back up over whitespace, to preserve it.
				(skip-chars-backward " \f\t")
				(if (= (char-after (1+ (point))) ? )
				    ;; Eat one space.
				    (forward-char 1))
				(point))
			      (progn (forward-line 1) (point)))
			     outbuf)))
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
	  ;; Insert the section-header line
	  ;; which lists the file name and which functions are in it, etc.
	  (autoload-insert-section-header outbuf autoloads-done load-name file
					  (nth 5 (file-attributes file)))
	  (insert ";;; Generated autoloads from "
		  (autoload-trim-file-name file) "\n")
	  (goto-char output-end)
	  (insert generate-autoload-section-trailer)))
    (message "Generating autoloads for %s...done" file)))

;;;###autoload
(defun update-file-autoloads (file &optional save-after)
  "Update the autoloads for FILE in `generated-autoload-file'
\(which FILE might bind in its local variables).
If SAVE-AFTER is non-nil (which is always, when called interactively),
save the buffer too.

Return FILE if there was no autoload cookie in it, else nil."
  (interactive "fUpdate autoloads for file: \np")
  (let ((load-name (let ((name (file-name-nondirectory file)))
		     (if (string-match "\\.elc?\\(\\.\\|$\\)" name)
			 (substring name 0 (match-beginning 0))
		       name)))
	(found nil)
	(existing-buffer (get-file-buffer file))
	(no-autoloads nil))
    (save-excursion
      ;; We want to get a value for generated-autoload-file from
      ;; the local variables section if it's there.
      (if existing-buffer
	  (set-buffer existing-buffer))
      ;; We must read/write the file without any code conversion,
      ;; but still decode EOLs.
      (let ((coding-system-for-read 'raw-text))
	(set-buffer (find-file-noselect
		     (autoload-ensure-default-file
		      (expand-file-name generated-autoload-file
					(expand-file-name "lisp"
							  source-directory)))))
	;; This is to make generated-autoload-file have Unix EOLs, so
	;; that it is portable to all platforms.
	(setq buffer-file-coding-system 'raw-text-unix))
      (or (> (buffer-size) 0)
	  (error "Autoloads file %s does not exist" buffer-file-name))
      (or (file-writable-p buffer-file-name)
	  (error "Autoloads file %s is not writable" buffer-file-name))
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  ;; Look for the section for LOAD-NAME.
	  (while (and (not found)
		      (search-forward generate-autoload-section-header nil t))
	    (let ((form (autoload-read-section-header)))
	      (cond ((string= (nth 2 form) load-name)
		     ;; We found the section for this file.
		     ;; Check if it is up to date.
		     (let ((begin (match-beginning 0))
			   (last-time (nth 4 form))
			   (file-time (nth 5 (file-attributes file))))
		       (if (and (or (null existing-buffer)
				    (not (buffer-modified-p existing-buffer)))
				(listp last-time) (= (length last-time) 2)
				(not (time-less-p last-time file-time)))
			   (progn
			     (if (interactive-p)
				 (message "\
Autoload section for %s is up to date."
					  file))
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
	  (or found
	      (progn
		(setq found 'new)
		;; No later sections in the file.  Put before the last page.
		(goto-char (point-max))
		(search-backward "\f" nil t)))
	  (or (eq found 'up-to-date)
	      (and (eq found 'new)
		   ;; Check that FILE has any cookies before generating a
		   ;; new section for it.
		   (save-excursion
		     (if existing-buffer
			 (set-buffer existing-buffer)
		       ;; It is faster to avoid visiting the file.
		       (set-buffer (get-buffer-create " *autoload-file*"))
		       (kill-all-local-variables)
		       (erase-buffer)
		       (setq buffer-undo-list t
			     buffer-read-only nil)
		       (emacs-lisp-mode)
		       (insert-file-contents file nil))
		     (save-excursion
		       (save-restriction
			 (widen)
			 (goto-char (point-min))
			 (prog1
			     (if (re-search-forward
				  (concat "^" (regexp-quote
					       generate-autoload-cookie))
				  nil t)
				 nil
			       (if (interactive-p)
				   (message "%s has no autoloads" file))
			       (setq no-autoloads t)
			       t)
			   (or existing-buffer
			       (kill-buffer (current-buffer))))))))
	      (generate-file-autoloads file))))
      (and save-after
	   (buffer-modified-p)
	   (save-buffer))

      (if no-autoloads file))))

(defun autoload-remove-section (begin)
  (goto-char begin)
  (search-forward generate-autoload-section-trailer)
  (delete-region begin (point)))

;;;###autoload
(defun update-directory-autoloads (&rest dirs)
  "\
Update loaddefs.el with all the current autoloads from DIRS, and no old ones.
This uses `update-file-autoloads' (which see) to do its work.
In an interactive call, you must give one argument, the name
of a single directory.  In a call from Lisp, you can supply multiple
directories as separate arguments, but this usage is discouraged.

The function does NOT recursively descend into subdirectories of the
directory or directories specified."
  (interactive "DUpdate autoloads from directory: ")
  (let* ((files-re (let ((tmp nil))
		     (dolist (suf load-suffixes
				  (concat "^[^=.].*" (regexp-opt tmp t) "\\'"))
		       (unless (string-match "\\.elc" suf) (push suf tmp)))))
	 (files (apply 'nconc
		       (mapcar (lambda (dir)
				 (directory-files (expand-file-name dir)
						  t files-re))
			       dirs)))
	 (this-time (current-time))
	 (no-autoloads nil)		;files with no autoload cookies.
	 (autoloads-file
	  (expand-file-name generated-autoload-file
			    (expand-file-name "lisp" source-directory)))
	 (top-dir (file-name-directory autoloads-file)))

    (with-current-buffer
	(find-file-noselect (autoload-ensure-default-file autoloads-file))
      (save-excursion

	;; Canonicalize file names and remove the autoload file itself.
	(setq files (delete (autoload-trim-file-name buffer-file-name)
			    (mapcar 'autoload-trim-file-name files)))

	(goto-char (point-min))
	(while (search-forward generate-autoload-section-header nil t)
	  (let* ((form (autoload-read-section-header))
		 (file (nth 3 form)))
	    (cond ((and (consp file) (stringp (car file)))
		   ;; This is a list of files that have no autoload cookies.
		   ;; There shouldn't be more than one such entry.
		   ;; Remove the obsolete section.
		   (autoload-remove-section (match-beginning 0))
		   (let ((last-time (nth 4 form)))
		     (dolist (file file)
		       (let ((file-time (nth 5 (file-attributes file))))
			 (when (and file-time
				    (not (time-less-p last-time file-time)))
			   ;; file unchanged
			   (push file no-autoloads)
			   (setq files (delete file files)))))))
		  ((not (stringp file)))
		  ((not (file-exists-p (expand-file-name file top-dir)))
		   ;; Remove the obsolete section.
		   (autoload-remove-section (match-beginning 0)))
		  ((equal (nth 4 form) (nth 5 (file-attributes file)))
		   ;; File hasn't changed.
		   nil)
		  (t
		   (update-file-autoloads file)))
	    (setq files (delete file files)))))
      ;; Elements remaining in FILES have no existing autoload sections yet.
      (setq no-autoloads
	    (append no-autoloads
		    (delq nil (mapcar 'update-file-autoloads files))))
      (when no-autoloads
	;; Sort them for better readability.
	(setq no-autoloads (sort no-autoloads 'string<))
	;; Add the `no-autoloads' section.
	(goto-char (point-max))
	(search-backward "\f" nil t)
	(autoload-insert-section-header
	 (current-buffer) nil nil no-autoloads this-time)
	(insert generate-autoload-section-trailer))

      (save-buffer))))

;;;###autoload
(defun batch-update-autoloads ()
  "Update loaddefs.el autoloads in batch mode.
Calls `update-directory-autoloads' on the command line arguments."
  (apply 'update-directory-autoloads command-line-args-left)
  (setq command-line-args-left nil))

(provide 'autoload)

;; arch-tag: 00244766-98f4-4767-bf42-8a22103441c6
;;; autoload.el ends here
