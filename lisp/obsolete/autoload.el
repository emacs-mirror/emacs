;;; autoload.el --- maintain autoloads in loaddefs.el  -*- lexical-binding: t -*-

;; Copyright (C) 1991-1997, 2001-2024 Free Software Foundation, Inc.

;; Author: Roland McGrath <roland@gnu.org>
;; Keywords: maint
;; Package: emacs
;; Obsolete-since: 29.1

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

;; This code helps GNU Emacs maintainers keep the loaddefs.el file up to
;; date.  It interprets magic cookies of the form ";;;###autoload" in
;; Lisp source files in various useful ways.  To learn more, read the
;; source; if you're going to use this, you'd better be able to.

;; The functions in this file have been superseded by loaddefs-gen.el.

;; Note: When removing this file, also remove the references to
;; `make-directory-autoloads' and `update-directory-autoloads' in
;; subr.el.

;;; Code:

(require 'lisp-mode)			;for `doc-string-elt' properties.
(require 'lisp-mnt)
(require 'cl-lib)
(require 'loaddefs-gen)

;; This feels like it should be a defconst, but MH-E sets it to
;; ";;;###mh-autoload" for the autoloads that are to go into mh-loaddefs.el.
(defvar generate-autoload-cookie ";;;###autoload"
  "Magic comment indicating the following form should be autoloaded.
Used by \\[update-file-autoloads].  This string should be
meaningless to Lisp (e.g., a comment).

This string is used:

\;;;###autoload
\(defun function-to-be-autoloaded () ...)

If this string appears alone on a line, the following form will be
read and an autoload made for it.  If there is further text on the line,
that text will be copied verbatim to `generated-autoload-file'.")

(defvar autoload-excludes nil
  "If non-nil, list of absolute file names not to scan for autoloads.")

(defconst generate-autoload-section-header "\f\n;;;### "
  "String that marks the form at the start of a new file's autoload section.")

(defconst generate-autoload-section-trailer "\n;;;***\n"
  "String which indicates the end of the section of autoloads for a file.")

(defconst generate-autoload-section-continuation ";;;;;; "
  "String to add on each continuation of the section header form.")

;; In some ways it would be nicer to use a value that is recognizably
;; not a time-value, eg t, but that can cause issues if an older Emacs
;; that does not expect non-time-values loads the file.
(defconst autoload--non-timestamp '(0 0 0 0)
  "Value to insert when `autoload-timestamps' is nil.")

(defvar autoload-timestamps nil		; experimental, see bug#22213
  "Non-nil means insert a timestamp for each input file into the output.
We use these in incremental updates of the output file to decide
if we need to rescan an input file.  If you set this to nil,
then we use the timestamp of the output file instead.  As a result:
 - for fixed inputs, the output will be the same every time
 - incremental updates of the output file might not be correct if:
   i) the timestamp of the output file cannot be trusted (at least
     relative to that of the input files)
   ii) any of the input files can be modified during the time it takes
      to create the output
   iii) only a subset of the input files are scanned
   These issues are unlikely to happen in practice, and would arguably
   represent bugs in the build system.  Item iii) will happen if you
   use a command like `update-file-autoloads', though, since it only
   checks a single input file.")

(defvar autoload-modified-buffers)      ;Dynamically scoped var.

(defalias 'make-autoload #'loaddefs-generate--make-autoload)

;; Forms which have doc-strings which should be printed specially.
;; A doc-string-elt property of ELT says that (nth ELT FORM) is
;; the doc-string in FORM.
;; Those properties are now set in lisp-mode.el.

(defun autoload-find-generated-file (file)
  "Visit the autoload file for the current buffer, and return its buffer."
  (let ((enable-local-variables :safe)
        (enable-local-eval nil)
        (find-file-hook nil)
        (delay-mode-hooks t))
    ;; We used to use `raw-text' to read this file, but this causes
    ;; problems when the file contains non-ASCII characters.
    (with-current-buffer (find-file-noselect
                          (autoload-ensure-file-writeable file))
      (if (zerop (buffer-size)) (insert (autoload-rubric file nil t)))
      (current-buffer))))

(defun autoload-generated-file (outfile)
  "Return OUTFILE as an absolute name.
If `generated-autoload-file' is bound locally in the current
buffer, that is used instead, and it is expanded using the
default directory; otherwise, `source-directory'/lisp is used."
  (expand-file-name (if (local-variable-p 'generated-autoload-file)
                        generated-autoload-file
                      outfile)
                    ;; File-local settings of generated-autoload-file should
                    ;; be interpreted relative to the file's location,
                    ;; of course.
                    (if (not (local-variable-p 'generated-autoload-file))
                        (expand-file-name "lisp" source-directory))))

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
   ((eq (car form) 'progn) (mapcar #'autoload-print-form (cdr form)))
   ;; Symbols at the toplevel are meaningless.
   ((symbolp form) nil)
   (t
    (let ((doc-string-elt (function-get (car-safe form) 'doc-string-elt))
	  (outbuf autoload-print-form-outbuf))
      (if (and (numberp doc-string-elt) (stringp (nth doc-string-elt form)))
	  ;; We need to hack the printing because the
	  ;; doc-string must be printed specially for
	  ;; make-docfile (sigh).
	  (let* ((p (nthcdr (1- doc-string-elt) form))
		 (elt (cdr p)))
	    (setcdr p nil)
	    (princ "\n(" outbuf)
	    (let ((print-escape-newlines t)
		  (print-escape-control-characters t)
                  (print-quoted t)
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
	      (print-escape-control-characters t)
              (print-quoted t)
	      (print-escape-nonascii t))
	  (print form outbuf)))))))

(defalias 'autoload-rubric #'loaddefs-generate--rubric)

(defvar autoload-ensure-writable nil
  "Non-nil means `autoload-find-generated-file' makes existing file writable.")
;; Just in case someone tries to get you to overwrite a file that you
;; don't want to.
;;;###autoload
(put 'autoload-ensure-writable 'risky-local-variable t)

(defun autoload-ensure-file-writeable (file)
  ;; Probably pointless, but replaces the old AUTOGEN_VCS in lisp/Makefile,
  ;; which was designed to handle CVSREAD=1 and equivalent.
  (and autoload-ensure-writable
       (let ((modes (file-modes file)))
	 (if (and modes (zerop (logand modes #o0200)))
             ;; Ignore any errors here, and let subsequent attempts
             ;; to write the file raise any real error.
             (ignore-errors (set-file-modes file (logior modes #o0200))))))
  file)

(defun autoload-insert-section-header (outbuf autoloads load-name file time)
  "Insert into buffer OUTBUF the section-header line for FILE.
The header line lists the file name, its \"load name\", its autoloads,
and the time the FILE was last updated (the time is inserted only
if `autoload-timestamps' is non-nil, otherwise a fixed fake time is inserted)."
  ;; (cl-assert ;Make sure we don't insert it in the middle of another section.
  ;;  (save-excursion
  ;;    (or (not (re-search-backward
  ;;              (concat "\\("
  ;;                      (regexp-quote generate-autoload-section-header)
  ;;                      "\\)\\|\\("
  ;;                      (regexp-quote generate-autoload-section-trailer)
  ;;                      "\\)")
  ;;              nil t))
  ;;        (match-end 2))))
  (insert generate-autoload-section-header)
  (prin1 `(autoloads ,autoloads ,load-name ,file ,time)
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

(defun autoload-find-file (file)
  "Fetch FILE and put it in a temp buffer.  Return the buffer."
  ;; It is faster to avoid visiting the file.
  (setq file (expand-file-name file))
  (with-current-buffer (get-buffer-create " *autoload-file*")
    (kill-all-local-variables)
    (erase-buffer)
    (setq buffer-undo-list t
          buffer-read-only nil)
    (delay-mode-hooks (emacs-lisp-mode))
    (setq default-directory (file-name-directory file))
    (insert-file-contents file nil)
    (let ((enable-local-variables :safe)
	  (enable-local-eval nil))
      (hack-local-variables))
    (current-buffer)))

(defalias 'autoload-file-load-name #'loaddefs-generate--file-load-name)

(defun generate-file-autoloads (file)
  "Insert at point a loaddefs autoload section for FILE.
Autoloads are generated for defuns and defmacros in FILE
marked by `generate-autoload-cookie' (which see).
If FILE is being visited in a buffer, the contents of the buffer
are used.
Return non-nil in the case where no autoloads were added at point."
  (interactive "fGenerate autoloads for file: ")
  (let ((autoload-modified-buffers nil))
    (autoload-generate-file-autoloads file (current-buffer) buffer-file-name)
    autoload-modified-buffers))

(defconst autoload-def-prefixes-max-entries 5
  "Target length of the list of definition prefixes per file.
If set too small, the prefixes will be too generic (i.e. they'll use little
memory, we'll end up looking in too many files when we need a particular
prefix), and if set too large, they will be too specific (i.e. they will
cost more memory use).")

(defconst autoload-def-prefixes-max-length 12
  "Target size of definition prefixes.
Don't try to split prefixes that are already longer than that.")

(defalias 'autoload--make-defs-autoload #'loaddefs-generate--make-prefixes)

(defun autoload--setup-output (otherbuf outbuf absfile load-name output-file)
  (let ((outbuf
         (or (if otherbuf
                 ;; A file-local setting of
                 ;; autoload-generated-file says we
                 ;; should ignore OUTBUF.
                 nil
               outbuf)
             (autoload-find-destination absfile load-name output-file)
             ;; The file has autoload cookies, but they're
             ;; already up-to-date. If OUTFILE is nil, the
             ;; entries are in the expected OUTBUF,
             ;; otherwise they're elsewhere.
             (throw 'done otherbuf))))
    (with-current-buffer outbuf
      (point-marker))))

(defun autoload--print-cookie-text (output-start load-name file)
  (let ((standard-output (marker-buffer output-start)))
     (search-forward generate-autoload-cookie)
     (skip-chars-forward " \t")
     (if (eolp)
         (condition-case-unless-debug err
             ;; Read the next form and make an autoload.
             (let* ((form (prog1 (read (current-buffer))
                            (or (bolp) (forward-line 1))))
                    (autoload (make-autoload form load-name)))
               (if autoload
                   nil
                 (setq autoload form))
               (let ((autoload-print-form-outbuf
                      standard-output))
                 (autoload-print-form autoload)))
           (error
            (message "Autoload cookie error in %s:%s %S"
                     file (count-lines (point-min) (point)) err)))

       ;; Copy the rest of the line to the output.
       (princ (buffer-substring
               (progn
                 ;; Back up over whitespace, to preserve it.
                 (skip-chars-backward " \f\t")
                 (if (= (char-after (1+ (point))) ? )
                     ;; Eat one space.
                     (forward-char 1))
                 (point))
              (progn (forward-line 1) (point)))))))

(defvar autoload-builtin-package-versions nil)

(defun autoload-generate-file-autoloads (file &optional outbuf outfile)
  "Insert an autoload section for FILE in the appropriate buffer.
Autoloads are generated for defuns and defmacros in FILE
marked by `generate-autoload-cookie' (which see).

If FILE is being visited in a buffer, the contents of the buffer are used.
OUTBUF is the buffer in which the autoload statements should be inserted.

If OUTBUF is nil, the output will go to OUTFILE, unless there's a
buffer-local setting of `generated-autoload-file' in FILE.

Return non-nil if and only if FILE adds no autoloads to OUTFILE
\(or OUTBUF if OUTFILE is nil).  The actual return value is
FILE's modification time."
  ;; Include the file name in any error messages
  (condition-case err
      (let (load-name
            (print-length nil)
            (print-level nil)
            (float-output-format nil)
            (visited (get-file-buffer file))
            (otherbuf nil)
            (absfile (expand-file-name file))
          (defs '())
            ;; nil until we found a cookie.
            output-start)
        (when
            (catch 'done
              (with-current-buffer (or visited
                                       ;; It is faster to avoid visiting the file.
                                       (autoload-find-file file))
                ;; Obey the no-update-autoloads file local variable.
                (unless no-update-autoloads
                  (or noninteractive (message "Generating autoloads for %s..." file))
                  (setq load-name
                        (if (stringp generated-autoload-load-name)
                            generated-autoload-load-name
                          (autoload-file-load-name absfile outfile)))
                  ;; FIXME? Comparing file-names for equality with just equal
                  ;; is fragile, eg if one has an automounter prefix and one
                  ;; does not, but both refer to the same physical file.
                  (when (and outfile
                             (not outbuf)
                             (not
                              (if (memq system-type '(ms-dos windows-nt))
                                  (equal (downcase outfile)
                                         (downcase (autoload-generated-file
                                                    outfile)))
                                (equal outfile (autoload-generated-file
                                                outfile)))))
                    (setq otherbuf t))
                  (save-excursion
                    (save-restriction
                      (widen)
                      (when autoload-builtin-package-versions
                        (let ((version (lm-header "version"))
                              package)
                          (and version
                               (setq version (ignore-errors (version-to-list version)))
                               (setq package (or (lm-header "package")
                                                 (file-name-sans-extension
                                                  (file-name-nondirectory file))))
                               (setq output-start (autoload--setup-output
                                                   otherbuf outbuf absfile
                                                   load-name outfile))
                               (let ((standard-output (marker-buffer output-start))
                                     (print-quoted t))
                                 (princ `(push (purecopy
                                                ',(cons (intern package) version))
                                               package--builtin-versions))
                                 (princ "\n")))))

                      ;; Do not insert autoload entries for excluded files.
                      (unless (member absfile autoload-excludes)
                        (goto-char (point-min))
                        (while (not (eobp))
                          (skip-chars-forward " \t\n\f")
                          (cond
                           ((looking-at (regexp-quote generate-autoload-cookie))
                            ;; If not done yet, figure out where to insert this text.
                            (unless output-start
                              (setq output-start (autoload--setup-output
                                                  otherbuf outbuf absfile
                                                  load-name outfile)))
                            (autoload--print-cookie-text output-start load-name file))
                           ((= (following-char) ?\;)
                            ;; Don't read the comment.
                            (forward-line 1))
                           (t
                  ;; Avoid (defvar <foo>) by requiring a trailing space.
                  ;; Also, ignore this prefix business
                  ;; for ;;;###tramp-autoload and friends.
                  (when (and (equal generate-autoload-cookie ";;;###autoload")
                             (looking-at "(\\(def[^ ]+\\) ['(]*\\([^' ()\"\n]+\\)[\n \t]")
                             (not (member
                                   (match-string 1)
                                   autoload-ignored-definitions)))
                    (push (match-string-no-properties 2) defs))
                            (forward-sexp 1)
                            (forward-line 1)))))))

          (when (and autoload-compute-prefixes defs)
            ;; This output needs to always go in the main loaddefs.el,
            ;; regardless of generated-autoload-file.
            ;; FIXME: the files that don't have autoload cookies but
            ;; do have definitions end up listed twice in loaddefs.el:
            ;; once for their register-definition-prefixes and once in
            ;; the list of "files without any autoloads".
            (let ((form (autoload--make-defs-autoload defs load-name)))
              (cond
               ((null form))             ;All defs obey the default rule, yay!
               ((not otherbuf)
                (unless output-start
                  (setq output-start (autoload--setup-output
                                      nil outbuf absfile load-name outfile)))
                (let ((autoload-print-form-outbuf
                       (marker-buffer output-start)))
                  (autoload-print-form form)))
               (t
                (let* ((other-output-start
                        ;; To force the output to go to the main loaddefs.el
                        ;; rather than to generated-autoload-file,
                        ;; there are two cases: if outbuf is non-nil,
                        ;; then passing otherbuf=nil is enough, but if
                        ;; outbuf is nil, that won't cut it, so we
                        ;; locally bind generated-autoload-file.
                        (autoload--setup-output nil outbuf absfile load-name
                                                outfile))
                       (autoload-print-form-outbuf
                        (marker-buffer other-output-start)))
                  (autoload-print-form form)
                  (with-current-buffer (marker-buffer other-output-start)
                    (save-excursion
                      ;; Insert the section-header line which lists
                      ;; the file name and which functions are in it, etc.
                      (goto-char other-output-start)
                      (let ((relfile (file-relative-name absfile)))
                        (autoload-insert-section-header
                         (marker-buffer other-output-start)
                         "actual autoloads are elsewhere" load-name relfile
			 (if autoload-timestamps
			     (file-attribute-modification-time
			      (file-attributes absfile))
			   autoload--non-timestamp))
                        (insert ";;; Generated autoloads from " relfile "\n")))
                    (insert generate-autoload-section-trailer)))))))

                  (when output-start
                    (let ((secondary-autoloads-file-buf
                           (if otherbuf (current-buffer))))
                      (with-current-buffer (marker-buffer output-start)
                        (cl-assert (> (point) output-start))
                        (save-excursion
                          ;; Insert the section-header line which lists the file name
                          ;; and which functions are in it, etc.
                          (goto-char output-start)
                          (let ((relfile (file-relative-name absfile)))
                            (autoload-insert-section-header
                             (marker-buffer output-start)
                             () load-name relfile
                             (if secondary-autoloads-file-buf
                                 ;; MD5 checksums are much better because they do not
                                 ;; change unless the file changes (so they'll be
                                 ;; equal on two different systems and will change
                                 ;; less often than time-stamps, thus leading to fewer
                                 ;; unneeded changes causing spurious conflicts), but
                                 ;; using time-stamps is a very useful optimization,
                                 ;; so we use time-stamps for the main autoloads file
                                 ;; (loaddefs.el) where we have special ways to
                                 ;; circumvent the "random change problem", and MD5
                                 ;; checksum in secondary autoload files where we do
                                 ;; not need the time-stamp optimization because it is
                                 ;; already provided by the primary autoloads file.
                                 (md5 secondary-autoloads-file-buf
                                      ;; We'd really want to just use
                                      ;; `emacs-internal' instead.
                                      nil nil 'emacs-mule-unix)
                               (if autoload-timestamps
                                   (file-attribute-modification-time
				    (file-attributes relfile))
                                 autoload--non-timestamp)))
                            (insert ";;; Generated autoloads from " relfile "\n")))
                        (insert generate-autoload-section-trailer))))
                  (or noninteractive
                      (message "Generating autoloads for %s...done" file)))
                (or visited
                    ;; We created this buffer, so we should kill it.
                    (kill-buffer (current-buffer))))
              (or (not output-start)
                  ;; If the entries were added to some other buffer, then the file
                  ;; doesn't add entries to OUTFILE.
                  otherbuf))
          (file-attribute-modification-time (file-attributes absfile))))
    (error
     ;; Probably unbalanced parens in forward-sexp. In that case, the
     ;; condition is scan-error, and the signal data includes point
     ;; where the error was found; we'd like to convert that to
     ;; line:col, but line-number-at-pos gets the wrong line in batch
     ;; mode for some reason.
     ;;
     ;; At least this gets the file name in the error message; the
     ;; developer can use goto-char to get to the error position.
     (error "%s:0:0: error: %s: %s" file (car err) (cdr err)))
    ))

;; For parallel builds, to stop another process reading a half-written file.
(defun autoload--save-buffer ()
  "Save current buffer to its file, atomically."
  ;; Similar to byte-compile-file.
  (let* ((version-control 'never)
         (tempfile (make-temp-file buffer-file-name))
	 (default-modes (default-file-modes))
	 (temp-modes (logand default-modes #o600))
	 (desired-modes (logand default-modes
				(or (file-modes buffer-file-name) #o666)))
         (kill-emacs-hook
          (cons (lambda () (ignore-errors (delete-file tempfile)))
                kill-emacs-hook)))
    (unless (= temp-modes desired-modes)
      (set-file-modes tempfile desired-modes 'nofollow))
    (write-region (point-min) (point-max) tempfile nil 1)
    (backup-buffer)
    (rename-file tempfile buffer-file-name t))
  (set-buffer-modified-p nil)
  (set-visited-file-modtime)
  (or noninteractive (message "Wrote %s" buffer-file-name)))

(defun autoload-save-buffers ()
  (while autoload-modified-buffers
    (with-current-buffer (pop autoload-modified-buffers)
      (autoload--save-buffer))))

;; FIXME This command should be deprecated.
;; See https://debbugs.gnu.org/22213#41
;;;###autoload
(defun update-file-autoloads (file &optional save-after outfile)
  "Update the autoloads for FILE.
If prefix arg SAVE-AFTER is non-nil, save the buffer too.

If FILE binds `generated-autoload-file' as a file-local variable,
autoloads are written into that file.  Otherwise, the autoloads
file is determined by OUTFILE.  If called interactively, prompt
for OUTFILE; if called from Lisp with OUTFILE nil, use the
existing value of `generated-autoload-file'.

Return FILE if there was no autoload cookie in it, else nil."
  (interactive (list (read-file-name "Update autoloads for file: ")
		     current-prefix-arg
		     (read-file-name "Write autoload definitions to file: ")))
  (setq outfile (or outfile generated-autoload-file))
  (let* ((autoload-modified-buffers nil)
	 ;; We need this only if the output file handles more than one input.
	 ;; See https://debbugs.gnu.org/22213#38 and subsequent.
	 (autoload-timestamps t)
         (no-autoloads (autoload-generate-file-autoloads
                        file nil
                        (if (local-variable-p 'generated-autoload-file)
                            generated-autoload-file
                          outfile))))
    (if autoload-modified-buffers
        (if save-after (autoload-save-buffers))
      (if (called-interactively-p 'interactive)
          (message "Autoload section for %s is up to date." file)))
    (if no-autoloads file)))

(defun autoload-find-destination (file load-name output-file)
  "Find the destination point of the current buffer's autoloads.
FILE is the file name of the current buffer.
LOAD-NAME is the name as it appears in the output.
Returns a buffer whose point is placed at the requested location.
Returns nil if the file's autoloads are up-to-date, otherwise
removes any prior now out-of-date autoload entries."
  (catch 'up-to-date
    (let* ((buf (current-buffer))
           (existing-buffer (if buffer-file-name buf))
           (output-file (autoload-generated-file output-file))
           (output-time (if (file-exists-p output-file)
                            (file-attribute-modification-time
			     (file-attributes output-file))))
           (found nil))
      (with-current-buffer (autoload-find-generated-file output-file)
        ;; This is to make generated-autoload-file have Unix EOLs, so
        ;; that it is portable to all platforms.
        (or (eq 0 (coding-system-eol-type buffer-file-coding-system))
	    (set-buffer-file-coding-system 'unix))
        (or (> (buffer-size) 0)
            (error "Autoloads file %s lacks boilerplate" buffer-file-name))
        (or (file-writable-p buffer-file-name)
            (error "Autoloads file %s is not writable" buffer-file-name))
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
                         (file-time (file-attribute-modification-time
				     (file-attributes file))))
                     (if (and (or (null existing-buffer)
                                  (not (buffer-modified-p existing-buffer)))
                              (cond
                               ;; FIXME? Arguably we should throw a
                               ;; user error, or some kind of warning,
                               ;; if we were called from update-file-autoloads,
                               ;; which can update only a single input file.
                               ;; It's not appropriate to use the output
                               ;; file modtime in such a case,
                               ;; if there are multiple input files
                               ;; contributing to the output.
                               ((and output-time
				     (member last-time
					     (list t autoload--non-timestamp)))
                                (not (time-less-p output-time file-time)))
                               ;; last-time is the time-stamp (specifying
                               ;; the last time we looked at the file) and
                               ;; the file hasn't been changed since.
                               ((listp last-time)
                                (not (time-less-p last-time file-time)))
                               ;; last-time is an MD5 checksum instead.
                               ((stringp last-time)
                                (equal last-time
				       (md5 buf nil nil 'emacs-mule)))))
                         (throw 'up-to-date nil)
                       (autoload-remove-section begin)
                       (setq found t))))
                  ((string< load-name (nth 2 form))
                   ;; We've come to a section alphabetically later than
                   ;; LOAD-NAME.  We assume the file is in order and so
                   ;; there must be no section for LOAD-NAME.  We will
                   ;; insert one before the section here.
                   (goto-char (match-beginning 0))
                   (setq found t)))))
        (or found
            (progn
              ;; No later sections in the file.  Put before the last page.
              (goto-char (point-max))
              (search-backward "\f" nil t)))
        (unless (memq (current-buffer) autoload-modified-buffers)
          (push (current-buffer) autoload-modified-buffers))
        (current-buffer)))))

(defun autoload-remove-section (begin)
  (goto-char begin)
  (search-forward generate-autoload-section-trailer)
  (delete-region begin (point)))

;;;###autoload
(defun update-directory-autoloads (&rest dirs)
  "Update autoload definitions for Lisp files in the directories DIRS.
In an interactive call, you must give one argument, the name of a
single directory.  In a call from Lisp, you can supply multiple
directories as separate arguments, but this usage is discouraged.

The function does NOT recursively descend into subdirectories of the
directory or directories specified.

In an interactive call, prompt for a default output file for the
autoload definitions.  When called from Lisp, use the existing
value of `generated-autoload-file'.  If any Lisp file binds
`generated-autoload-file' as a file-local variable, write its
autoloads into the specified file instead."
  (declare (obsolete loaddefs-generate "29.1"))
  (interactive "DUpdate autoloads from directory: ")
  (make-directory-autoloads
   dirs
   (if (called-interactively-p 'interactive)
       (read-file-name "Write autoload definitions to file: ")
     generated-autoload-file)))

;;;###autoload
(defun make-directory-autoloads (dir output-file)
  "Update autoload definitions for Lisp files in the directories DIR.
DIR can be either a single directory or a list of
directories.  (The latter usage is discouraged.)

The autoloads will be written to OUTPUT-FILE.  If any Lisp file
binds `generated-autoload-file' as a file-local variable, write
its autoloads into the specified file instead.

The function does NOT recursively descend into subdirectories of the
directory or directories specified."
  (declare (obsolete loaddefs-generate "29.1"))
  (interactive "DUpdate autoloads from directory: \nFWrite to file: ")
  (let* ((files-re (let ((tmp nil))
		     (dolist (suf (get-load-suffixes))
                       ;; We don't use module-file-suffix below because
                       ;; we don't want to depend on whether Emacs was
                       ;; built with or without modules support, nor
                       ;; what is the suffix for the underlying OS.
		       (unless (string-match "\\.\\(elc\\|so\\|dll\\)" suf)
                         (push suf tmp)))
                     (concat "\\`[^=.].*" (regexp-opt tmp t) "\\'")))
	 (files (apply #'nconc
		       (mapcar (lambda (d)
				 (directory-files (expand-file-name d)
                                                  t files-re))
			       (if (consp dir) dir (list dir)))))
         (done ())                      ;Files processed; to remove duplicates.
         (changed nil)                  ;Non-nil if some change occurred.
	 (last-time)
         ;; Files with no autoload cookies or whose autoloads go to other
         ;; files because of file-local autoload-generated-file settings.
	 (no-autoloads nil)
         ;; Ensure that we don't do odd things when putting the doc
         ;; strings into the autoloads file.
         (left-margin 0)
         (autoload-modified-buffers nil)
	 (output-time
	  (and (file-exists-p output-file)
	       (file-attribute-modification-time
                (file-attributes output-file)))))

    (with-current-buffer (autoload-find-generated-file output-file)
      (save-excursion
	;; Canonicalize file names and remove the autoload file itself.
	(setq files (delete (file-relative-name buffer-file-name)
			    (mapcar #'file-relative-name files)))

	(goto-char (point-min))
	(while (search-forward generate-autoload-section-header nil t)
	  (let* ((form (autoload-read-section-header))
		 (file (nth 3 form)))
	    (cond ((and (consp file) (stringp (car file)))
		   ;; This is a list of files that have no autoload cookies.
		   ;; There shouldn't be more than one such entry.
		   ;; Remove the obsolete section.
		   (autoload-remove-section (match-beginning 0))
		   (setq last-time (nth 4 form))
		   (if (member last-time (list t autoload--non-timestamp))
		       (setq last-time output-time))
		   (dolist (file file)
		     (let ((file-time (file-attribute-modification-time
				       (file-attributes file))))
		       (when (and file-time
				  (not (time-less-p last-time file-time)))
			 ;; file unchanged
			 (push file no-autoloads)
			 (setq files (delete file files))))))
		  ((not (stringp file)))
		  ((or (not (file-exists-p file))
                       ;; Remove duplicates as well, just in case.
                       (member file done))
                   ;; Remove the obsolete section.
                   (setq changed t)
		   (autoload-remove-section (match-beginning 0)))
		  ((not (time-less-p (let ((oldtime (nth 4 form)))
				       (if (member oldtime
						   (list
						    t autoload--non-timestamp))
					   output-time
					 oldtime))
                                     (file-attribute-modification-time
				      (file-attributes file))))
		   ;; File hasn't changed.
		   nil)
		  (t
                   (setq changed t)
                   (autoload-remove-section (match-beginning 0))
                   (if (autoload-generate-file-autoloads
                        ;; Passing `current-buffer' makes it insert at point.
                        file (current-buffer) buffer-file-name)
                       (push file no-autoloads))))
            (push file done)
	    (setq files (delete file files)))))
      ;; Elements remaining in FILES have no existing autoload sections yet.
      (let ((no-autoloads-time (or last-time '(0 0 0 0)))
            (progress (make-progress-reporter
                       (byte-compile-info
                        (concat "Scraping files for "
                                (file-relative-name output-file)))
                       0 (length files) nil 10))
            (file-count 0)
            file-time)
	(dolist (file files)
          (progress-reporter-update progress (setq file-count (1+ file-count)))
	  (cond
	   ;; Passing nil as second argument forces
	   ;; autoload-generate-file-autoloads to look for the right
	   ;; spot where to insert each autoloads section.
	   ((setq file-time
		  (autoload-generate-file-autoloads file nil buffer-file-name))
	    (push file no-autoloads)
	    (if (time-less-p no-autoloads-time file-time)
		(setq no-autoloads-time file-time)))
           (t (setq changed t))))
        (progress-reporter-done progress)

	(when no-autoloads
	  ;; Sort them for better readability.
	  (setq no-autoloads (sort no-autoloads 'string<))
	  ;; Add the `no-autoloads' section.
	  (goto-char (point-max))
	  (search-backward "\f" nil t)
	  (autoload-insert-section-header
	   (current-buffer) nil nil
           ;; Filter out the other loaddefs files, because it makes
           ;; the list unstable (and leads to spurious changes in
           ;; ldefs-boot.el) since the loaddef files can be created in
           ;; any order.
           (seq-filter (lambda (file)
                         (not (string-match-p "[/-]loaddefs.el" file)))
                       no-autoloads)
           (if autoload-timestamps
	       no-autoloads-time
	     autoload--non-timestamp))
	  (insert generate-autoload-section-trailer)))

      ;; Don't modify the file if its content has not been changed, so `make'
      ;; dependencies don't trigger unnecessarily.
      (if (not changed)
          (set-buffer-modified-p nil)
        (autoload--save-buffer))

      ;; In case autoload entries were added to other files because of
      ;; file-local autoload-generated-file settings.
      (autoload-save-buffers))))

(defun batch-update-autoloads--summary (strings)
  (let ((message ""))
    (while strings
      (when (> (length (concat message " " (car strings))) 64)
        (byte-compile-info (concat message " ...") t "SCRAPE")
        (setq message ""))
      (setq message (if (zerop (length message))
                        (car strings)
                      (concat message " " (car strings))))
      (setq strings (cdr strings)))
    (when (> (length message) 0)
      (byte-compile-info message t "SCRAPE"))))

;;;###autoload
(defun batch-update-autoloads ()
  "Update loaddefs.el autoloads in batch mode.
Calls `update-directory-autoloads' on the command line arguments.
Definitions are written to `generated-autoload-file' (which
should be non-nil)."
  ;; For use during the Emacs build process only.
  ;; Exclude those files that are preloaded on ALL platforms.
  ;; These are the ones in loadup.el where "(load" is at the start
  ;; of the line (crude, but it works).
  (unless autoload-excludes
    (let ((default-directory (file-name-directory generated-autoload-file))
	  file)
      (when (file-readable-p "loadup.el")
	(with-temp-buffer
	  (insert-file-contents "loadup.el")
	  (while (re-search-forward "^(load \"\\([^\"]+\\)\"" nil t)
	    (setq file (match-string 1))
	    (or (string-match "\\.el\\'" file)
		(setq file (format "%s.el" file)))
	    (or (string-match "\\`site-" file)
		(push (expand-file-name file) autoload-excludes)))))))
  (let ((args command-line-args-left))
    (batch-update-autoloads--summary args)
    (setq command-line-args-left nil)
    (with-suppressed-warnings ((obsolete make-directory-autoloads))
      (make-directory-autoloads args generated-autoload-file))))

(provide 'autoload)

;;; autoload.el ends here
