;;; cmulisp.el --- improved version of standard inferior-lisp mode

;;; Copyright Olin Shivers (1988).

;; Keywords: processes, lisp

;;; Please imagine a long, tedious, legalistic 5-page gnu-style copyright
;;; notice appearing here to the effect that you may use this code any
;;; way you like, as long as you don't charge money for it, remove this
;;; notice, or hold me liable for its results.

;;; Commentary:

;;; This replaces the standard inferior-lisp mode.
;;; Hacked from tea.el by Olin Shivers (shivers@cs.cmu.edu). 8/88
;;; Please send me bug reports, bug fixes, and extensions, so that I can
;;; merge them into the master source.
;;;
;;; Change log at end of file.

;;; This file defines a a lisp-in-a-buffer package (cmulisp mode) built on top
;;; of comint mode.  Cmulisp mode is similar to, and intended to replace, its
;;; counterpart in the standard gnu emacs release. This replacements is more
;;; featureful, robust, and uniform than the released version.  The key
;;; bindings are also more compatible with the bindings of Hemlock and Zwei
;;; (the Lisp Machine emacs).

;;; Since this mode is built on top of the general command-interpreter-in-
;;; a-buffer mode (comint mode), it shares a common base functionality, 
;;; and a common set of bindings, with all modes derived from comint mode.
;;; This makes these modes easier to use.

;;; For documentation on the functionality provided by comint mode, and
;;; the hooks available for customising it, see the file comint.el.
;;; For further information on cmulisp mode, see the comments below.

;;; Needs fixin:
;;; The load-file/compile-file default mechanism could be smarter -- it
;;; doesn't know about the relationship between filename extensions and
;;; whether the file is source or executable. If you compile foo.lisp
;;; with compile-file, then the next load-file should use foo.bin for
;;; the default, not foo.lisp. This is tricky to do right, particularly
;;; because the extension for executable files varies so much (.o, .bin,
;;; .lbin, .mo, .vo, .ao, ...).
;;;
;;; It would be nice if cmulisp (and inferior scheme, T, ...) modes
;;; had a verbose minor mode wherein sending or compiling defuns, etc.
;;; would be reflected in the transcript with suitable comments, e.g.
;;; ";;; redefining fact". Several ways to do this. Which is right?
;;;
;;; When sending text from a source file to a subprocess, the process-mark can 
;;; move off the window, so you can lose sight of the process interactions.
;;; Maybe I should ensure the process mark is in the window when I send
;;; text to the process? Switch selectable?

(require 'comint)
;; YOUR .EMACS FILE
;;=============================================================================
;; Some suggestions for your .emacs file.
;;
;; ; If cmulisp lives in some non-standard directory, you must tell emacs
;; ; where to get it. This may or may not be necessary.
;; (setq load-path (cons (expand-file-name "~jones/lib/emacs") load-path))
;;
;; ; Autoload cmulisp from file cmulisp.el
;; (autoload 'cmulisp "cmulisp"
;;           "Run an inferior Lisp process."
;;           t)
;;
;; ; Define C-c t to run my favorite command in cmulisp mode:
;; (setq cmulisp-load-hook
;;       '((lambda () 
;;           (define-key cmulisp-mode-map "\C-ct" 'favorite-cmd))))


;;; Brief Command Documentation:
;;;============================================================================
;;; Comint Mode Commands: (common to cmulisp and all comint-derived modes)
;;;
;;; m-p	    comint-previous-input    	    Cycle backwards in input history
;;; m-n	    comint-next-input  	    	    Cycle forwards
;;; m-c-r   comint-previous-input-matching  Search backwards in input history
;;; return  comint-send-input
;;; c-a     comint-bol                      Beginning of line; skip prompt.
;;; c-d	    comint-delchar-or-maybe-eof	    Delete char unless at end of buff.
;;; c-c c-u comint-kill-input	    	    ^u
;;; c-c c-w backward-kill-word    	    ^w
;;; c-c c-c comint-interrupt-subjob 	    ^c
;;; c-c c-z comint-stop-subjob	    	    ^z
;;; c-c c-\ comint-quit-subjob	    	    ^\
;;; c-c c-o comint-kill-output		    Delete last batch of process output
;;; c-c c-r comint-show-output		    Show last batch of process output
;;;         send-invisible                  Read line w/o echo & send to proc
;;;         comint-continue-subjob	    Useful if you accidentally suspend
;;;					        top-level job.
;;; comint-mode-hook is the comint mode hook.

;;; CMU Lisp Mode Commands:
;;; c-m-x   lisp-send-defun     This binding is a gnu convention.
;;; c-c c-l lisp-load-file  	Prompt for file name; tell Lisp to load it.
;;; c-c c-k lisp-compile-file	Prompt for file name; tell Lisp to kompile it.
;;; 	    	    	    	Filename completion is available, of course.
;;;
;;; Additionally, these commands are added to the key bindings of Lisp mode:
;;; c-m-x   lisp-eval-defun         This binding is a gnu convention.
;;; c-c c-e lisp-eval-defun 	    Send the current defun to Lisp process.
;;; c-x c-e lisp-eval-last-sexp     Send the previous sexp to Lisp process.
;;; c-c c-r lisp-eval-region        Send the current region to Lisp process.
;;; c-c c-c lisp-compile-defun      Compile the current defun in Lisp process.
;;; c-c c-z switch-to-lisp          Switch to the Lisp process buffer.
;;; c-c c-l lisp-load-file          (See above. In a Lisp file buffer, default
;;; c-c c-k lisp-compile-file        is to load/compile the current file.)
;;; c-c c-d lisp-describe-sym	    Query Lisp for a symbol's description.
;;; c-c c-a lisp-show-arglist	    Query Lisp for function's arglist.
;;; c-c c-f lisp-show-function-documentation Query Lisp for a function's doc.
;;; c-c c-v lisp-show-variable-documentation Query Lisp for a variable's doc.

;;; cmulisp	    	    	    Fires up the Lisp process.
;;; lisp-compile-region     	    Compile all forms in the current region.
;;;
;;; CMU Lisp Mode Variables:
;;; cmulisp-filter-regexp	    Match this => don't get saved on input hist
;;; inferior-lisp-program   	    Name of Lisp program run-lisp executes
;;; inferior-lisp-load-command	    Customises lisp-load-file
;;; cmulisp-mode-hook  	    
;;; inferior-lisp-prompt	    Initialises comint-prompt-regexp.
;;;				    Backwards compatibility.
;;; lisp-source-modes               Anything loaded into a buffer that's in
;;;                                 one of these modes is considered Lisp 
;;;                                 source by lisp-load/compile-file.

;;; Read the rest of this file for more information.


;;; Code:

(defvar cmulisp-filter-regexp "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'"
  "*What not to save on inferior Lisp's input history
Input matching this regexp is not saved on the input history in cmulisp
mode. Default is whitespace followed by 0 or 1 single-letter :keyword 
(as in :a, :c, etc.)")

(defvar cmulisp-mode-map nil)
(cond ((not cmulisp-mode-map)
       (setq cmulisp-mode-map
	     (nconc (full-copy-sparse-keymap comint-mode-map)
		    shared-lisp-mode-map))
       (define-key cmulisp-mode-map "\C-x\C-e" 'lisp-eval-last-sexp)
       (define-key cmulisp-mode-map "\C-c\C-l" 'lisp-load-file)
       (define-key cmulisp-mode-map "\C-c\C-k" 'lisp-compile-file)
       (define-key cmulisp-mode-map "\C-c\C-a" 'lisp-show-arglist)
       (define-key cmulisp-mode-map "\C-c\C-d" 'lisp-describe-sym)
       (define-key cmulisp-mode-map "\C-c\C-f" 'lisp-show-function-documentation)
       (define-key cmulisp-mode-map "\C-c\C-v" 'lisp-show-variable-documentation)))

;;; These commands augment Lisp mode, so you can process Lisp code in
;;; the source files.
(define-key lisp-mode-map "\M-\C-x"  'lisp-eval-defun)     ; Gnu convention
(define-key lisp-mode-map "\C-x\C-e" 'lisp-eval-last-sexp) ; Gnu convention
(define-key lisp-mode-map "\C-c\C-e" 'lisp-eval-defun)
(define-key lisp-mode-map "\C-c\C-r" 'lisp-eval-region)
(define-key lisp-mode-map "\C-c\C-c" 'lisp-compile-defun)
(define-key lisp-mode-map "\C-c\C-z" 'switch-to-lisp)
(define-key lisp-mode-map "\C-c\C-l" 'lisp-load-file)
(define-key lisp-mode-map "\C-c\C-k" 'lisp-compile-file)  ; "kompile" file
(define-key lisp-mode-map "\C-c\C-a" 'lisp-show-arglist)
(define-key lisp-mode-map "\C-c\C-d" 'lisp-describe-sym)
(define-key lisp-mode-map "\C-c\C-f" 'lisp-show-function-documentation)
(define-key lisp-mode-map "\C-c\C-v" 'lisp-show-variable-documentation)

(defvar cmulisp-buffer)

;;; This function exists for backwards compatibility.
;;; Previous versions of this package bound commands to C-c <letter>
;;; bindings, which is not allowed by the gnumacs standard.

(defun cmulisp-install-letter-bindings ()
  "This function binds many cmulisp commands to C-c <letter> bindings,
where they are more accessible. C-c <letter> bindings are reserved for the
user, so these bindings are non-standard. If you want them, you should
have this function called by the cmulisp-load-hook:
    (setq cmulisp-load-hook '(cmulisp-install-letter-bindings))
You can modify this function to install just the bindings you want."

  (define-key lisp-mode-map "\C-ce" 'lisp-eval-defun-and-go)
  (define-key lisp-mode-map "\C-cr" 'lisp-eval-region-and-go)
  (define-key lisp-mode-map "\C-cc" 'lisp-compile-defun-and-go)
  (define-key lisp-mode-map "\C-cz" 'switch-to-lisp)
  (define-key lisp-mode-map "\C-cl" 'lisp-load-file)
  (define-key lisp-mode-map "\C-ck" 'lisp-compile-file)
  (define-key lisp-mode-map "\C-ca" 'lisp-show-arglist)
  (define-key lisp-mode-map "\C-cd" 'lisp-describe-sym)
  (define-key lisp-mode-map "\C-cf" 'lisp-show-function-documentation)
  (define-key lisp-mode-map "\C-cv" 'lisp-show-variable-documentation)

  (define-key cmulisp-mode-map "\C-cl" 'lisp-load-file)
  (define-key cmulisp-mode-map "\C-ck" 'lisp-compile-file)
  (define-key cmulisp-mode-map "\C-ca" 'lisp-show-arglist)
  (define-key cmulisp-mode-map "\C-cd" 'lisp-describe-sym)
  (define-key cmulisp-mode-map "\C-cf" 'lisp-show-function-documentation)
  (define-key cmulisp-mode-map "\C-cv" 'lisp-show-variable-documentation))


(defvar inferior-lisp-program "lisp"
  "*Program name for invoking an inferior Lisp with `cmulisp'.")

(defvar inferior-lisp-load-command "(load \"%s\")\n"
  "*Format-string for building a Lisp expression to load a file.
This format string should use %s to substitute a file name
and should result in a Lisp expression that will command the inferior Lisp
to load that file.  The default works acceptably on most Lisps.
The string \"(progn (load \\\"%s\\\" :verbose nil :print t) (values))\\\n\"
produces cosmetically superior output for this application,
but it works only in Common Lisp.")

(defvar inferior-lisp-prompt "^[^> ]*>+:? *"
  "Regexp to recognise prompts in the inferior Lisp.
Defaults to \"^[^> ]*>+:? *\", which works pretty good for Lucid, kcl,
and franz. This variable is used to initialise comint-prompt-regexp in the 
cmulisp buffer.

More precise choices:
Lucid Common Lisp: \"^\\(>\\|\\(->\\)+\\) *\"
franz: \"^\\(->\\|<[0-9]*>:\\) *\"
kcl: \"^>+ *\"

This is a fine thing to set in your .emacs file.")

(defvar cmulisp-mode-hook '()
  "*Hook for customising cmulisp mode")

(defun cmulisp-mode () 
  "Major mode for interacting with an inferior Lisp process.  
Runs a Lisp interpreter as a subprocess of Emacs, with Lisp I/O through an
Emacs buffer.  Variable inferior-lisp-program controls which Lisp interpreter
is run.  Variables inferior-lisp-prompt, cmulisp-filter-regexp and
inferior-lisp-load-command can customize this mode for different Lisp
interpreters.

For information on running multiple processes in multiple buffers, see
documentation for variable cmulisp-buffer.

\\{cmulisp-mode-map}

Customisation: Entry to this mode runs the hooks on comint-mode-hook and
cmulisp-mode-hook (in that order).

You can send text to the inferior Lisp process from other buffers containing
Lisp source.  
    switch-to-lisp switches the current buffer to the Lisp process buffer.
    lisp-eval-defun sends the current defun to the Lisp process.
    lisp-compile-defun compiles the current defun.
    lisp-eval-region sends the current region to the Lisp process.
    lisp-compile-region compiles the current region.

    Prefixing the lisp-eval/compile-defun/region commands with
    a \\[universal-argument] causes a switch to the Lisp process buffer after sending
    the text.

Commands:
Return after the end of the process' output sends the text from the 
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for Lisp; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp inferior-lisp-prompt)
  (setq major-mode 'cmulisp-mode)
  (setq mode-name "CMU Lisp")
  (setq mode-line-process '(": %s"))
  (lisp-mode-variables t)
  (use-local-map cmulisp-mode-map)    ;c-c c-k for "kompile" file
  (setq comint-get-old-input (function lisp-get-old-input))
  (setq comint-input-filter (function lisp-input-filter))
  (setq comint-input-sentinel 'ignore)
  (run-hooks 'cmulisp-mode-hook))

(defun lisp-get-old-input ()
  "Snarf the sexp ending at point"
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun lisp-input-filter (str)
  "Don't save anything matching cmulisp-filter-regexp"
  (not (string-match cmulisp-filter-regexp str)))

(defun cmulisp (cmd)
  "Run an inferior Lisp process, input and output via buffer *cmulisp*.
If there is a process already running in *cmulisp*, just switch to that buffer.
With argument, allows you to edit the command line (default is value
of inferior-lisp-program).  Runs the hooks from cmulisp-mode-hook (after the
comint-mode-hook is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list (if current-prefix-arg
			 (read-string "Run lisp: " inferior-lisp-program)
			 inferior-lisp-program)))
  (if (not (comint-check-proc "*cmulisp*"))
      (let ((cmdlist (cmulisp-args-to-list cmd)))
	 (set-buffer (apply (function make-comint) "cmulisp" (car cmdlist) nil
			    (cdr cmdlist)))
	 (cmulisp-mode)))
  (setq cmulisp-buffer "*cmulisp*")
  (switch-to-buffer "*cmulisp*"))

;;; Break a string up into a list of arguments.
;;; This will break if you have an argument with whitespace, as in
;;; string = "-ab +c -x 'you lose'".
(defun cmulisp-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (tea-args-to-list (substring string (+ 1 where)
					      (length string)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if (null pos)
		   nil
		 (cmulisp-args-to-list (substring string pos
						  (length string)))))))))

(defun lisp-eval-region (start end &optional and-go)
  "Send the current region to the inferior Lisp process.
Prefix argument means switch-to-lisp afterwards."
  (interactive "r\nP")
  (comint-send-region (cmulisp-proc) start end)
  (comint-send-string (cmulisp-proc) "\n")
  (if and-go (switch-to-lisp t)))

(defun lisp-eval-defun (&optional and-go)
  "Send the current defun to the inferior Lisp process.
Prefix argument means switch-to-lisp afterwards."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (skip-chars-backward " \t\n\r\f") ;  Makes allegro happy
    (let ((end (point)))
      (beginning-of-defun)
      (lisp-eval-region (point) end)))
  (if and-go (switch-to-lisp t)))

(defun lisp-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the inferior Lisp process.
Prefix argument means switch-to-lisp afterwards."
  (interactive "P")
  (lisp-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))

;;; Common Lisp COMPILE sux. 
(defun lisp-compile-region (start end &optional and-go)
  "Compile the current region in the inferior Lisp process.
Prefix argument means switch-to-lisp afterwards."
  (interactive "r\nP")
  (comint-send-string (cmulisp-proc)
    (format "(funcall (compile nil `(lambda () (progn 'compile %s))))\n"
	    (buffer-substring start end)))
  (if and-go (switch-to-lisp t)))
			 
(defun lisp-compile-defun (&optional and-go)
  "Compile the current defun in the inferior Lisp process.
Prefix argument means switch-to-lisp afterwards."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (skip-chars-backward " \t\n\r\f") ;  Makes allegro happy
    (let ((e (point)))
      (beginning-of-defun)
      (lisp-compile-region (point) e)))
  (if and-go (switch-to-lisp t)))

(defun switch-to-lisp (eob-p)
  "Switch to the inferior Lisp process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer cmulisp-buffer)
      (pop-to-buffer cmulisp-buffer)
      (error "No current process buffer. See variable cmulisp-buffer."))
  (cond (eob-p
	 (push-mark)
	 (goto-char (point-max)))))


;;; Now that lisp-compile/eval-defun/region takes an optional prefix arg,
;;; these commands are redundant. But they are kept around for the user
;;; to bind if he wishes, for backwards functionality, and because it's
;;; easier to type C-c e than C-u C-c C-e.

(defun lisp-eval-region-and-go (start end)
  "Send the current region to the inferior Lisp, 
and switch to the process buffer."
  (interactive "r")
  (lisp-eval-region start end t))

(defun lisp-eval-defun-and-go ()
  "Send the current defun to the inferior Lisp, 
and switch to the process buffer."
  (interactive)
  (lisp-eval-defun t))

(defun lisp-compile-region-and-go (start end)
  "Compile the current region in the inferior Lisp, 
and switch to the process buffer."
  (interactive "r")
  (lisp-compile-region start end t))

(defun lisp-compile-defun-and-go ()
  "Compile the current defun in the inferior Lisp, 
and switch to the process buffer."
  (interactive)
  (lisp-compile-defun t))

;;; A version of the form in H. Shevis' soar-mode.el package. Less robust.
;(defun lisp-compile-sexp (start end)
;  "Compile the s-expression bounded by START and END in the inferior lisp.
;If the sexp isn't a DEFUN form, it is evaluated instead."
;    (cond ((looking-at "(defun\\s +")
;	   (goto-char (match-end 0))
;	   (let ((name-start (point)))
;	     (forward-sexp 1)
;	     (process-send-string "cmulisp" (format "(compile '%s #'(lambda "
;						 (buffer-substring name-start
;								   (point)))))
;	   (let ((body-start (point)))
;	     (goto-char start) (forward-sexp 1) ; Can't use end-of-defun.
;	     (process-send-region "cmulisp" (buffer-substring body-start (point))))
;	   (process-send-string "cmulisp" ")\n"))
;	  (t (lisp-eval-region start end)))))
;
;(defun lisp-compile-region (start end)
;  "Each s-expression in the current region is compiled (if a DEFUN)
;or evaluated (if not) in the inferior lisp."
;  (interactive "r")
;  (save-excursion
;    (goto-char start) (end-of-defun) (beginning-of-defun) ; error check
;    (if (< (point) start) (error "region begins in middle of defun"))
;    (goto-char start)
;    (let ((s start))
;      (end-of-defun)
;      (while (<= (point) end) ; Zip through
;	(lisp-compile-sexp s (point)) ; compiling up defun-sized chunks.
;	(setq s (point))
;	(end-of-defun))
;      (if (< s end) (lisp-compile-sexp s end)))))
;;;
;;; End of HS-style code


(defvar lisp-prev-l/c-dir/file nil
  "Saves the (directory . file) pair used in the last lisp-load-file or
lisp-compile-file command. Used for determining the default in the 
next one.")

(defvar lisp-source-modes '(lisp-mode)
  "*Used to determine if a buffer contains Lisp source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a Lisp source file by lisp-load-file and lisp-compile-file.
Used by these commands to determine defaults.")

(defun lisp-load-file (file-name)
  "Load a Lisp file into the inferior Lisp process."
  (interactive (comint-get-source "Load Lisp file: " lisp-prev-l/c-dir/file
				  lisp-source-modes nil)) ; NIL because LOAD
                                                   ; doesn't need an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq lisp-prev-l/c-dir/file (cons (file-name-directory    file-name)
				     (file-name-nondirectory file-name)))
  (comint-send-string (cmulisp-proc)
		      (format inferior-lisp-load-command file-name))
  (switch-to-lisp t))


(defun lisp-compile-file (file-name)
  "Compile a Lisp file in the inferior Lisp process."
  (interactive (comint-get-source "Compile Lisp file: " lisp-prev-l/c-dir/file
				  lisp-source-modes nil)) ; NIL = don't need
                                                          ; suffix .lisp
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq lisp-prev-l/c-dir/file (cons (file-name-directory    file-name)
				     (file-name-nondirectory file-name)))
  (comint-send-string (cmulisp-proc) (concat "(compile-file \""
					     file-name
					     "\"\)\n"))
  (switch-to-lisp t))



;;; Documentation functions: function doc, var doc, arglist, and
;;; describe symbol.
;;; ===========================================================================

;;; Command strings
;;; ===============

(defvar lisp-function-doc-command
  "(let ((fn '%s))
     (format t \"Documentation for ~a:~&~a\"
	     fn (documentation fn 'function))
     (values))\n"
  "Command to query inferior Lisp for a function's documentation.")

(defvar lisp-var-doc-command
  "(let ((v '%s))
     (format t \"Documentation for ~a:~&~a\"
	     v (documentation v 'variable))
     (values))\n"
  "Command to query inferior Lisp for a variable's documentation.")

(defvar lisp-arglist-command
  "(let ((fn '%s))
     (format t \"Arglist for ~a: ~a\" fn (arglist fn))
     (values))\n"
  "Command to query inferior Lisp for a function's arglist.")

(defvar lisp-describe-sym-command
  "(describe '%s)\n"
  "Command to query inferior Lisp for a variable's documentation.")


;;; Ancillary functions
;;; ===================

;;; Reads a string from the user.
(defun lisp-symprompt (prompt default)
  (list (let* ((prompt (if default
			   (format "%s (default %s): " prompt default)
			   (concat prompt ": ")))
	       (ans (read-string prompt)))
	  (if (zerop (length ans)) default ans))))


;;; Adapted from function-called-at-point in help.el.
(defun lisp-fn-called-at-pt ()
  "Returns the name of the function called in the current call.
Nil if it can't find one."
  (condition-case nil
      (save-excursion
	(save-restriction
	  (narrow-to-region (max (point-min) (- (point) 1000)) (point-max))
	  (backward-up-list 1)
	  (forward-char 1)
	  (let ((obj (read (current-buffer))))
	    (and (symbolp obj) obj))))
    (error nil)))


;;; Adapted from variable-at-point in help.el.
(defun lisp-var-at-pt ()
  (condition-case ()
      (save-excursion
	(forward-sexp -1)
	(skip-chars-forward "'")
	(let ((obj (read (current-buffer))))
	  (and (symbolp obj) obj)))
    (error nil)))


;;; Documentation functions: fn and var doc, arglist, and symbol describe.
;;; ======================================================================

(defun lisp-show-function-documentation (fn)
  "Send a command to the inferior Lisp to give documentation for function FN.
See variable lisp-function-doc-command."
  (interactive (lisp-symprompt "Function doc" (lisp-fn-called-at-pt)))
  (comint-proc-query (cmulisp-proc) (format lisp-function-doc-command fn)))

(defun lisp-show-variable-documentation (var)
  "Send a command to the inferior Lisp to give documentation for function FN.
See variable lisp-var-doc-command."
  (interactive (lisp-symprompt "Variable doc" (lisp-var-at-pt)))
  (comint-proc-query (cmulisp-proc) (format lisp-var-doc-command var)))

(defun lisp-show-arglist (fn)
  "Sends an query to the inferior Lisp for the arglist for function FN.
See variable lisp-arglist-command."
  (interactive (lisp-symprompt "Arglist" (lisp-fn-called-at-pt)))
  (comint-proc-query (cmulisp-proc) (format lisp-arglist-command fn)))

(defun lisp-describe-sym (sym)
  "Send a command to the inferior Lisp to describe symbol SYM.
See variable lisp-describe-sym-command."
  (interactive (lisp-symprompt "Describe" (lisp-var-at-pt)))
  (comint-proc-query (cmulisp-proc) (format lisp-describe-sym-command sym)))


(defvar cmulisp-buffer nil "*The current cmulisp process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
Cmulisp.el supports, in a fairly simple fashion, running multiple Lisp
processes. To run multiple Lisp processes, you start the first up with
\\[cmulisp]. It will be in a buffer named *cmulisp*. Rename this buffer
with \\[rename-buffer]. You may now start up a new process with another
\\[cmulisp]. It will be in a new buffer, named *cmulisp*. You can
switch between the different process buffers with \\[switch-to-buffer].

Commands that send text from source buffers to Lisp processes --
like lisp-eval-defun or lisp-show-arglist -- have to choose a process
to send to, when you have more than one Lisp process around. This
is determined by the global variable cmulisp-buffer. Suppose you
have three inferior lisps running:
    Buffer	Process
    foo		cmulisp
    bar		cmulisp<2>
    *cmulisp*   cmulisp<3>
If you do a \\[lisp-eval-defun] command on some Lisp source code, 
what process do you send it to?

- If you're in a process buffer (foo, bar, or *cmulisp*), 
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer cmulisp-buffer.
This process selection is performed by function cmulisp-proc.

Whenever \\[cmulisp] fires up a new process, it resets cmulisp-buffer
to be the new process's buffer. If you only run one process, this will
do the right thing. If you run multiple processes, you can change
cmulisp-buffer to another process buffer with \\[set-variable].

More sophisticated approaches are, of course, possible. If you find youself
needing to switch back and forth between multiple processes frequently,
you may wish to consider ilisp.el, a larger, more sophisticated package
for running inferior Lisp processes. The approach taken here is for a
minimal, simple implementation. Feel free to extend it.")

(defun cmulisp-proc ()
  "Returns the current cmulisp process. See variable cmulisp-buffer."
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-lisp-mode)
				      (current-buffer)
				      cmulisp-buffer))))
    (or proc
	(error "No current process. See variable cmulisp-buffer"))))


;;; Do the user's customisation...
;;;===============================
(defvar cmulisp-load-hook nil
  "This hook is run when cmulisp is loaded in.
This is a good place to put keybindings.")
	
(run-hooks 'cmulisp-load-hook)

;;; CHANGE LOG
;;; ===========================================================================
;;; 5/24/90 Olin
;;; - Split cmulisp and cmushell modes into separate files. 
;;;   Not only is this a good idea, it's apparently the way it'll be rel 19.
;;; - Upgraded process sends to use comint-send-string instead of
;;;   process-send-string.
;;; - Explicit references to process "cmulisp" have been replaced with
;;;   (cmulisp-proc). This allows better handling of multiple process bufs.
;;; - Added process query and var/function/symbol documentation
;;;   commands. Based on code written by Douglas Roberts.
;;; - Added lisp-eval-last-sexp, bound to C-x C-e.
;;;
;;; 9/20/90 Olin
;;; Added a save-restriction to lisp-fn-called-at-pt. This bug and fix
;;; reported by Lennart Staflin.
;;;
;;; 3/12/90 Olin
;;; - lisp-load-file and lisp-compile-file no longer switch-to-lisp.
;;;   Tale suggested this.
;;; - Reversed this decision 7/15/91. You need the visual feedback.
;;;
;;; 7/25/91 Olin
;;; Changed all keybindings of the form C-c <letter>. These are
;;; supposed to be reserved for the user to bind. This affected
;;; mainly the compile/eval-defun/region[-and-go] commands.
;;; This was painful, but necessary to adhere to the gnumacs standard.
;;; For some backwards compatibility, see the 
;;;     cmulisp-install-letter-bindings
;;; function.
;;;
;;; 8/2/91 Olin
;;; - The lisp-compile/eval-defun/region commands now take a prefix arg,
;;;   which means switch-to-lisp after sending the text to the Lisp process.
;;;   This obsoletes all the -and-go commands. The -and-go commands are
;;;   kept around for historical reasons, and because the user can bind
;;;   them to key sequences shorter than C-u C-c C-<letter>.
;;; - If M-x cmulisp is invoked with a prefix arg, it allows you to
;;;   edit the command line.

(provide 'cmulisp)

;;; cmulisp.el ends here
