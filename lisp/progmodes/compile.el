;; Run compiler as inferior of Emacs, and parse its error messages.
;; Copyright (C) 1985, 1986, 1988, 1989 Free Software Foundation, Inc.

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

(provide 'compile)

(defvar compilation-error-list nil
  "List of error message descriptors for visiting erring functions.
Each error descriptor is a list of length two.
Its car is a marker pointing to an error message.
Its cadr is a marker pointing to the text of the line the message is about,
  or nil if that is not interesting.
The value may be t instead of a list;
this means that the buffer of error messages should be reparsed
the next time the list of errors is wanted.")

(defvar compilation-old-error-list nil
  "Value of `compilation-error-list' after errors were parsed.")

(defvar compilation-last-error nil
  "List describing the error found by last call to \\[next-error].
A list of two markers (ERROR-POS CODE-POS),
pointing to the error message and the erroneous code, respectively.
CODE-POS can be nil, if the error message has no specific source location.")

(defvar compilation-parse-errors-hook 'compilation-parse-errors
  "Function to call (no args) to parse error messages from a compilation.
It should read in the source files which have errors
and set `compilation-error-list' to a list with an element
for each error message found.  See that variable for more info.")

(defvar compilation-error-buffer nil
  "Current compilation buffer for compilation error processing.") 

(defvar compilation-parsing-end nil
  "Position of end of buffer when last error messages parsed.")

(defvar compilation-error-message nil
  "Message to print when no more matches for compilation-error-regexp are found")

;; The filename excludes colons to avoid confusion when error message
;; starts with digits.
(defvar compilation-error-regexp
  "\\([^ :\n]+\\(: *\\|, line \\|(\\)[0-9]+\\)\\|\\([0-9]+ *of *[^ \n]+\\)\\|\\(\"[^ \n]+\",L[0-9]+\\)"
  "Regular expression for filename/linenumber in error in compilation log.")

(defvar compile-window-height nil
  "*Desired height of compilation window.  nil means use Emacs default.")

(defvar compile-command "make -k "
  "Last shell command used to do a compilation; default for next compilation.

Sometimes it is useful for files to supply local values for this variable.
You might also use mode hooks to specify it in certain modes, like this:

    (setq c-mode-hook
      '(lambda () (or (file-exists-p \"makefile\") (file-exists-p \"Makefile\")
		      (progn (make-local-variable 'compile-command)
			     (setq compile-command
				    (concat \"make -k \"
					    buffer-file-name))))))")

(defvar compilation-search-path '(nil)
  "List of directories to search for source files named in error messages.
Elements should be directory names, not file names of directories.
nil as an element means to try the default directory.")

(defun compile (command)
  "Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer `*compilation*'.
You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

To run more than one compilation at once, start one and rename the
`*compilation*' buffer to some other name.  Then start the next one."
  (interactive (list (read-string "Compile command: " compile-command)))
  (setq compile-command command)
  (save-some-buffers nil nil)
  (compile-internal compile-command "No more errors")
  (and compile-window-height
       (= (window-width) (screen-width))
       (enlarge-window (- (- (screen-height) (window-height))
			  compile-window-height) nil)))

(defun grep (command-args)
  "Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.  It is expected that `grep-command'
has a `-n' flag, so that line numbers are displayed for each match."
  (interactive
   (list (read-string (concat "Run "
			      (substring grep-command 0
					 (string-match "[\t ]+" grep-command))
			      " (with args): ")
		      (progn
			(string-match "-n[\t ]+" grep-command)
			(substring grep-command (match-end 0))))))
  ;; why a redundant string-match?  It might not be interactive ...
  (setq grep-command (concat (substring grep-command 0
					(progn
					  (string-match "-n" grep-command)
					  (match-end 0)))
			     " " command-args))
  (compile-internal (concat grep-command " /dev/null")
		    "No more grep hits" "grep"))

(defun compile-internal (command error-message
				 &optional name-of-mode parser regexp)
  "Run compilation command COMMAND (low level interface).
ERROR-MESSAGE is a string to print if the user asks to see another error
and there are no more errors.  Third argument NAME-OF-MODE is the name
to display as the major mode in the `*compilation*' buffer.

Fourth arg PARSER is the error parser function (nil means the default).
Fifth arg REGEXP is the error message regexp to use (nil means the default).
The defaults for these variables are the global values of
 `compilation-parse-errors-hook' and `compilation-error-regexp'."
  (save-excursion
    (set-buffer (get-buffer-create "*compilation*"))
    (setq buffer-read-only nil)
    (let ((comp-proc (get-buffer-process (current-buffer))))
      (if comp-proc
	  (if (or (not (eq (process-status comp-proc) 'run))
		  (yes-or-no-p "A compilation process is running; kill it? "))
	      (condition-case ()
		  (progn
		    (interrupt-process comp-proc)
		    (sit-for 1)
		    (delete-process comp-proc))
		(error nil))
	  (error "Cannot have two processes in `*compilation*' at once"))))
    ;; In case *compilation* is current buffer,
    ;; make sure we get the global values of compilation-error-regexp, etc.
    (kill-all-local-variables))
  (compilation-forget-errors)
  (start-process-shell-command "compilation" "*compilation*" command)
  (with-output-to-temp-buffer "*compilation*"
    (princ "cd ")
    (princ default-directory)
    (terpri)
    (princ command)
    (terpri))
  (let* ((regexp (or regexp compilation-error-regexp))
	 (parser (or parser compilation-parse-errors-hook))
	 (thisdir default-directory)
	 (outbuf (get-buffer "*compilation*"))
	 (outwin (get-buffer-window outbuf)))
    (if (eq outbuf (current-buffer))
	(goto-char (point-max)))
    (set-process-sentinel (get-buffer-process outbuf)
			  'compilation-sentinel)
    (save-excursion
      (set-buffer outbuf)
      (if (or (eq compilation-error-buffer outbuf)
	      (eq compilation-error-list t)
	      (and (null compilation-error-list)
		   (not (and (get-buffer-process compilation-error-buffer)
			     (eq (process-status compilation-error-buffer)
				 'run)))))
	  (setq compilation-error-list t
		compilation-error-buffer outbuf))
      (setq default-directory thisdir)
      (compilation-mode)
      (set-window-start outwin (point-min))
      (setq mode-name (or name-of-mode "Compilation"))
      (setq buffer-read-only t)
      (or (eq outwin (selected-window))
	  (set-window-point outwin (point-min))))))

(defvar compilation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'compile-goto-error)
    map)
  "Keymap for compilation log buffers.")

(defun compilation-mode ()
  "Major mode for compilation log buffers.
\\<compilation-mode-map>To visit the source for a line-numbered error,
move point to the error message line and type \\[compile-goto-error]."
  (interactive)
  (fundamental-mode)
  (use-local-map compilation-mode-map)
  (make-local-variable 'compilation-parse-errors-hook)
  (setq compilation-parse-errors-hook parser)
  (make-local-variable 'compilation-error-message)
  (setq compilation-error-message error-message)
  (make-local-variable 'compilation-error-regexp)
  (setq compilation-error-regexp regexp)
  (buffer-disable-undo (current-buffer))
  (setq major-mode 'compilation-mode)
  (setq mode-name "Compilation")
  ;; Make log buffer's mode line show process state
  (setq mode-line-process '(": %s")))

;; Called when compilation process changes state.

(defun compilation-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 (let* ((obuf (current-buffer))
		omax opoint)
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 (setq omax (point-max) opoint (point))
		 (goto-char (point-max))
		 (insert ?\n mode-name " " msg)
		 (forward-char -1)
		 (insert " at " (substring (current-time-string) 0 19))
		 (forward-char 1)
		 (setq mode-line-process
		       (concat ": "
			       (symbol-name (process-status proc))))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Force mode line redisplay soon
	     (set-buffer-modified-p (buffer-modified-p)))
	   (if (and opoint (< opoint omax))
	       (goto-char opoint))
	   (set-buffer obuf)))))

(defun kill-compilation ()
  "Kill the process made by the \\[compile] command."
  (interactive)
  (let ((buffer
	 (if (assq 'compilation-parse-errors-hook (buffer-local-variables))
	     (current-buffer)
	   (get-buffer "*compilation*"))))
    (if (get-buffer-process buffer)
	(interrupt-process (get-buffer-process buffer)))))

;; Reparse errors or parse more/new errors, if appropriate.
(defun compile-reinitialize-errors (argp)
  ;; If we are out of errors, or if user says "reparse",
  ;; or if we are in a different buffer from the known errors,
  ;; discard the info we have, to force reparsing.
  (if (or (eq compilation-error-list t)
	  (consp argp)
	  (if (assq 'compilation-parse-errors-hook (buffer-local-variables))
	      (not (eq compilation-error-buffer
		       (setq compilation-error-buffer (current-buffer))))))
      (progn (compilation-forget-errors)
	     (setq compilation-parsing-end 1)))
  (if compilation-error-list
      nil
    (save-excursion
      (switch-to-buffer compilation-error-buffer)
      (set-buffer-modified-p nil)
      (let ((at-start (= compilation-parsing-end 1)))
	(run-hooks 'compilation-parse-errors-hook)
	;; Remember the entire list for compilation-forget-errors.
	;; If this is an incremental parse, append to previous list.
	(if at-start
	    (setq compilation-old-error-list compilation-error-list)
	  (setq compilation-old-error-list
		(nconc compilation-old-error-list compilation-error-list)))))))

(defun compile-goto-error (&optional argp)
  "Visit the source for the error message point is on.
Use this command in a compilation log buffer.
C-u as a prefix arg means to reparse the buffer's error messages first;
other kinds of prefix arguments are ignored."
  (interactive "P")
  (compile-reinitialize-errors argp)
  (save-excursion
    (beginning-of-line)
    (setq compilation-error-list
	  (memq (assoc (point-marker) compilation-old-error-list)
		compilation-old-error-list)))
  ;; Move to another window, so that next-error's window changes
  ;; result in the desired setup.
  (or (one-window-p)
      (other-window -1))
  (next-error 1))

(defun next-error (&optional argp)
  "Visit next compilation error message and corresponding source code.
This operates on the output from the \\[compile] command.
If all preparsed error messages have been processed,
the error message buffer is checked for new ones.

A prefix arg specifies how many error messages to move;
negative means move back to previous error messages.
Just C-u as a prefix means reparse the error message buffer
and start at the first error.

\\[next-error] normally applies to the most recent compilation started,
but as long as you are in the middle of parsing errors from one compilation
output buffer, you stay with that compilation output buffer.

Use \\[next-error] in a compilation output buffer to switch to
processing errors from that compilation.

See variables `compilation-parse-errors-hook' and `compilation-error-regexp'
for customization ideas.  When we return, `compilation-last-error'
points to the error message and the erroneous code."
  (interactive "P")
  (compile-reinitialize-errors argp)
  (if (consp argp)
      (setq argp nil))
  (let* ((next-errors (nthcdr (+ (- (length compilation-old-error-list)
				    (length compilation-error-list)
				    1)
				 (prefix-numeric-value argp))
			      compilation-old-error-list))
	 (next-error (car next-errors)))
    (if (null next-error)
	(save-excursion
	  (if argp (if (> (prefix-numeric-value argp) 0)
		       (error "Moved past last error")
		     (error "Moved back past first error")))
	  (set-buffer compilation-error-buffer)
	  (compilation-forget-errors)
	  (error (concat compilation-error-message
			 (if (and (get-buffer-process (current-buffer))
				  (eq (process-status (current-buffer))
				      'run))
			     " yet" "")))))
    (setq compilation-error-list (cdr next-errors))
    ;; If we have an error to go to, go there.
    (if (null (car (cdr next-error)))
	nil
      (switch-to-buffer (marker-buffer (car (cdr next-error))))
      (goto-char (car (cdr next-error)))
      ;; If narrowing got in the way of going to the right place, widen.
      (or (= (point) (car (cdr next-error)))
	  (progn
	    (widen)
	    (goto-char (car (cdr next-error))))))
    ;; Show compilation buffer in other window, scrolled to this error.
    (let* ((pop-up-windows t)
	   (w (display-buffer (marker-buffer (car next-error)))))
      (set-window-point w (car next-error))
      (set-window-start w (car next-error)))
    (setq compilation-last-error next-error)))

;; Set compilation-error-list to nil, and
;; unchain the markers that point to the error messages and their text,
;; so that they no longer slow down gap motion.
;; This would happen anyway at the next garbage collection,
;; but it is better to do it right away.
(defun compilation-forget-errors ()
  (while compilation-old-error-list
    (let ((next-error (car compilation-old-error-list)))
      (set-marker (car next-error) nil)
      (if (car (cdr next-error))
	  (set-marker (car (cdr next-error)) nil)))
    (setq compilation-old-error-list (cdr compilation-old-error-list)))
  (setq compilation-error-list nil))

(defun compilation-parse-errors ()
  "Parse the current buffer as grep, cc or lint error messages.
See variable `compilation-parse-errors-hook' for the interface it uses."
  (setq compilation-error-list nil)
  (message "Parsing error messages...")
  (let (text-buffer
	last-filename last-linenum)
    ;; Don't reparse messages already seen at last parse.
    (goto-char compilation-parsing-end)
    ;; Don't parse the first two lines as error messages.
    ;; This matters for grep.
    (if (bobp)
	(forward-line 2))
    (while (re-search-forward compilation-error-regexp nil t)
      (let (linenum filename
	    error-marker text-marker)
	;; Extract file name and line number from error message.
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (goto-char (point-max))
	  (skip-chars-backward "[0-9]")
	  ;; If it's a lint message, use the last file(linenum) on the line.
	  ;; Normally we use the first on the line.
	  (if (= (preceding-char) ?\()
	      (progn
		(narrow-to-region (point-min) (1+ (buffer-size)))
		(end-of-line)
		(re-search-backward compilation-error-regexp)
		(skip-chars-backward "^ \t\n")
		(narrow-to-region (point) (match-end 0))
		(goto-char (point-max))
		(skip-chars-backward "[0-9]")))
	  ;; Are we looking at a "filename-first" or "line-number-first" form?
	  (if (looking-at "[0-9]")
	      (progn
		(setq linenum (read (current-buffer)))
		(goto-char (point-min)))
	    ;; Line number at start, file name at end.
	    (progn
	      (goto-char (point-min))
	      (setq linenum (read (current-buffer)))
	      (goto-char (point-max))
	      (skip-chars-backward "^ \t\n")))
	  (setq filename (compilation-grab-filename)))
	;; Locate the erring file and line.
	(if (and (equal filename last-filename)
		 (= linenum last-linenum))
	    nil
	  (beginning-of-line 1)
	  (setq error-marker (point-marker))
	  ;; text-buffer gets the buffer containing this error's file.
	  (if (not (equal filename last-filename))
	      (setq last-filename filename
		    text-buffer (compilation-find-file filename)
		    last-linenum 0))
	  (if text-buffer
	      ;; Go to that buffer and find the erring line.
	      (save-excursion
		(set-buffer text-buffer)
		(if (zerop last-linenum)
		    (progn
		      (goto-char 1)
		      (setq last-linenum 1)))
		(forward-line (- linenum last-linenum))
		(setq last-linenum linenum)
		(setq text-marker (point-marker))
		(setq compilation-error-list
		      (cons (list error-marker text-marker)
			    compilation-error-list)))))
	(forward-line 1)))
    (setq compilation-parsing-end (point-max)))
  (message "Parsing error messages...done")
  (setq compilation-error-list (nreverse compilation-error-list)))

;; Find or create a buffer for file FILENAME.
;; Search the directories in compilation-search-path
;; after trying the current directory.
(defun compilation-find-file (filename)
  (let ((dirs compilation-search-path)
	result)
    (while (and dirs (null result))
      (let ((name (if (car dirs)
		      (concat (car dirs) filename)
		    filename)))
	(setq result
	      (and (file-exists-p name)
		   (find-file-noselect name))))
      (setq dirs (cdr dirs)))
    result))

(defun compilation-grab-filename ()
  "Return a string which is a filename, starting at point.
Ignore quotes and parentheses around it, as well as trailing colons."
  (if (eq (following-char) ?\")
      (save-restriction
	(narrow-to-region (point)
			  (progn (forward-sexp 1) (point)))
	(goto-char (point-min))
	(read (current-buffer)))
    (buffer-substring (point)
		      (progn
			(skip-chars-forward "^ :,\n\t(")
			(point)))))

(define-key ctl-x-map "`" 'next-error)
