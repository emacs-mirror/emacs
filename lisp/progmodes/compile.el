;;; compile.el --- run compiler as inferior of Emacs, parse error messages.

;; Copyright (C) 1985, 86, 87, 93, 94, 1995 Free Software Foundation, Inc.

;; Author: Roland McGrath <roland@prep.ai.mit.edu>
;; Maintainer: FSF
;; Keywords: tools, processes

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

;; This package provides the compile and grep facilities documented in
;; the Emacs user's manual.

;;; Code:

;;;###autoload
(defvar compilation-mode-hook nil
  "*List of hook functions run by `compilation-mode' (see `run-hooks').")

;;;###autoload
(defvar compilation-window-height nil
  "*Number of lines in a compilation window.  If nil, use Emacs default.")

(defvar compilation-error-list nil
  "List of error message descriptors for visiting erring functions.
Each error descriptor is a cons (or nil).  Its car is a marker pointing to
an error message.  If its cdr is a marker, it points to the text of the
line the message is about.  If its cdr is a cons, it is a list
\(\(DIRECTORY . FILE\) LINE [COLUMN]\).  Or its cdr may be nil if that
error is not interesting.

The value may be t instead of a list; this means that the buffer of
error messages should be reparsed the next time the list of errors is wanted.

Some other commands (like `diff') use this list to control the error
message tracking facilites; if you change its structure, you should make
sure you also change those packages.  Perhaps it is better not to change
it at all.")

(defvar compilation-old-error-list nil
  "Value of `compilation-error-list' after errors were parsed.")

(defvar compilation-parse-errors-function 'compilation-parse-errors 
  "Function to call to parse error messages from a compilation.
It takes args LIMIT-SEARCH and FIND-AT-LEAST.
If LIMIT-SEARCH is non-nil, don't bother parsing past that location.
If FIND-AT-LEAST is non-nil, don't bother parsing after finding that 
many new errors.
It should read in the source files which have errors and set
`compilation-error-list' to a list with an element for each error message
found.  See that variable for more info.")

;;;###autoload
(defvar compilation-buffer-name-function nil
  "Function to compute the name of a compilation buffer.
The function receives one argument, the name of the major mode of the
compilation buffer.  It should return a string.
nil means compute the name with `(concat \"*\" (downcase major-mode) \"*\")'.")

;;;###autoload
(defvar compilation-finish-function nil
  "*Function to call when a compilation process finishes.
It is called with two arguments: the compilation buffer, and a string
describing how the process finished.")

(defvar compilation-last-buffer nil
  "The most recent compilation buffer.
A buffer becomes most recent when its compilation is started
or when it is used with \\[next-error] or \\[compile-goto-error].")

(defvar compilation-in-progress nil
  "List of compilation processes now running.")
(or (assq 'compilation-in-progress minor-mode-alist)
    (setq minor-mode-alist (cons '(compilation-in-progress " Compiling")
				 minor-mode-alist)))

(defvar compilation-parsing-end nil
  "Position of end of buffer when last error messages were parsed.")

(defvar compilation-error-message "No more errors"
  "Message to print when no more matches are found.")

(defvar compilation-num-errors-found)

(defvar compilation-error-regexp-alist
  '(
    ;; NOTE!  See also grep-regexp-alist, below.

    ;; 4.3BSD grep, cc, lint pass 1:
    ;; 	/usr/src/foo/foo.c(8): warning: w may be used before set
    ;; or GNU utilities:
    ;; 	foo.c:8: error message
    ;; or HP-UX 7.0 fc:
    ;; 	foo.f          :16    some horrible error message
    ;; or GNU utilities with column (GNAT 1.82):
    ;;   foo.adb:2:1: Unit name does not match file name
    ;; 
    ;; We'll insist that the number be followed by a colon or closing
    ;; paren, because otherwise this matches just about anything
    ;; containing a number with spaces around it.
    ("\n\
\\([^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\)\\([) \t]\\|\
:\\([^0-9\n]\\|\\([0-9]+:\\)\\)\\)" 1 2 5)

    ;; Borland C++:
    ;;  Error ping.c 15: Unable to open include file 'sys/types.h'
    ;;  Warning ping.c 68: Call to function 'func' with no prototype
    ("\n\\(Error\\|Warning\\) \\([^:( \t\n]+\\)\
 \\([0-9]+\\)\\([) \t]\\|:[^0-9\n]\\)" 2 3)

    ;; 4.3BSD lint pass 2
    ;; 	strcmp: variable # of args. llib-lc(359)  ::  /usr/src/foo/foo.c(8)
    ("[ \t:]\\([^:( \t\n]+\\)[:(](+[ \t]*\\([0-9]+\\))[:) \t]*$" 1 2)

    ;; 4.3BSD lint pass 3
    ;; 	bloofle defined( /users/wolfgang/foo.c(4) ), but never used
    ;; This used to be
    ;; ("[ \t(]+\\([^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]+" 1 2)
    ;; which is regexp Impressionism - it matches almost anything!
    ("([ \t]*\\([^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\))" 1 2)

    ;; Ultrix 3.0 f77:
    ;;  fort: Severe: addstf.f, line 82: Missing operator or delimiter symbol
    ;; Some SGI cc version:
    ;;  cfe: Warning 835: foo.c, line 2: something
    ("\n\\(cfe\\|fort\\): [^:\n]*: \\([^ \n]*\\), line \\([0-9]+\\):" 2 3)
    ;;  Error on line 3 of t.f: Execution error unclassifiable statement    
    ;; Unknown who does this:
    ;;  Line 45 of "foo.c": bloofel undefined
    ;; Absoft FORTRAN 77 Compiler 3.1.3
    ;;  error on line 19 of fplot.f: spelling error?
    ;;  warning on line 17 of fplot.f: data type is undefined for variable d
    ("\\(\n\\|on \\)[Ll]ine[ \t]+\\([0-9]+\\)[ \t]+\
of[ \t]+\"?\\([^\":\n]+\\)\"?:" 3 2)

    ;; Apollo cc, 4.3BSD fc:
    ;;	"foo.f", line 3: Error: syntax error near end of statement
    ;; IBM RS6000:
    ;;  "vvouch.c", line 19.5: 1506-046 (S) Syntax error.
    ;; Unknown compiler:
    ;;  File "foobar.ml", lines 5-8, characters 20-155: blah blah
    ;; Microtec mcc68k:
    ;;  "foo.c", line 32 pos 1; (E) syntax error; unexpected symbol: "lossage"
    ;; GNAT (as of July 94): 
    ;;  "foo.adb", line 2(11): warning: file name does not match ...
    ("\"\\([^,\" \n\t]+\\)\", lines? \\([0-9]+\\)[:., (-]" 1 2)

    ;; MIPS RISC CC - the one distributed with Ultrix:
    ;;	ccom: Error: foo.c, line 2: syntax error
    ;; DEC AXP OSF/1 cc
    ;;  /usr/lib/cmplrs/cc/cfe: Error: foo.c: 1: blah blah 
    ("rror: \\([^,\" \n\t]+\\)[,:] \\(line \\)?\\([0-9]+\\):" 1 3)

    ;; IBM AIX PS/2 C version 1.1:
    ;;	****** Error number 140 in line 8 of file errors.c ******
    ("in line \\([0-9]+\\) of file \\([^ \n]+[^. \n]\\)\\.? " 2 1)
    ;; IBM AIX lint is too painful to do right this way.  File name
    ;; prefixes entire sections rather than being on each line.

    ;; Lucid Compiler, lcc 3.x
    ;; E, file.cc(35,52) Illegal operation on pointers
    ("\n[EW], \\([^(\n]*\\)(\\([0-9]+\\),[ \t]*\\([0-9]+\\)" 1 2 3)

    ;; GNU messages with program name and optional column number.
    ("\n[^0-9 \n\t:]+:[ \t]*\\([^ \n\t:]+\\):\
\\([0-9]+\\):\\(\\([0-9]+\\)[: \t]\\)?" 1 2 4)

    ;; Cray C compiler error messages
    ("\n\\(cc\\| cft\\)-[0-9]+ c\\(c\\|f77\\): ERROR \\([^,]+, \\)* File = \\([^,]+\\), Line = \\([0-9]+\\)" 4 5)

    ;; IBM C/C++ Tools 2.01:
    ;;  foo.c(2:0) : informational EDC0804: Function foo is not referenced.
    ;;  foo.c(3:8) : warning EDC0833: Implicit return statement encountered.
    ;;  foo.c(5:5) : error EDC0350: Syntax error.
    ("\n *\\([^(]+\\)(\\([0-9]+\\):\\([0-9]+\\)) : " 1 2 3)
    )
  "Alist that specifies how to match errors in compiler output.
Each elt has the form (REGEXP FILE-IDX LINE-IDX [COLUMN-IDX FILE-FORMAT...])
If REGEXP matches, the FILE-IDX'th subexpression gives the file name, and
the LINE-IDX'th subexpression gives the line number.  If COLUMN-IDX is
given, the COLUMN-IDX'th subexpression gives the column number on that line.
If any FILE-FORMAT is given, each is a format string to produce a file name to
try; %s in the string is replaced by the text matching the FILE-IDX'th
subexpression.")

(defvar compilation-read-command t
  "If not nil, M-x compile reads the compilation command to use.
Otherwise, M-x compile just uses the value of `compile-command'.")

(defvar compilation-ask-about-save t
  "If not nil, M-x compile asks which buffers to save before compiling.
Otherwise, it saves all modified buffers without asking.")

(defvar grep-regexp-alist
  '(("^\\([^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]" 1 2))
  "Regexp used to match grep hits.  See `compilation-error-regexp-alist'.")

(defvar grep-command "grep -n "
  "Last grep command used in \\[grep]; default for next grep.")

;;;###autoload
(defvar compilation-search-path '(nil)
  "*List of directories to search for source files named in error messages.
Elements should be directory names, not file names of directories.
nil as an element means to try the default directory.")

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

(defvar compilation-enter-directory-regexp
  ": Entering directory `\\(.*\\)'$"
  "Regular expression matching lines that indicate a new current directory.
This must contain one \\(, \\) pair around the directory name.

The default value matches lines printed by the `-w' option of GNU Make.")

(defvar compilation-leave-directory-regexp
  ": Leaving directory `\\(.*\\)'$"
  "Regular expression matching lines that indicate restoring current directory.
This may contain one \\(, \\) pair around the name of the directory
being moved from.  If it does not, the last directory entered \(by a
line matching `compilation-enter-directory-regexp'\) is assumed.

The default value matches lines printed by the `-w' option of GNU Make.")

(defvar compilation-directory-stack nil
  "Stack of previous directories for `compilation-leave-directory-regexp'.
The head element is the directory the compilation was started in.")

;; History of compile commands.
(defvar compile-history nil)
;; History of grep commands.
(defvar grep-history nil)

(defvar compilation-mode-font-lock-keywords
  '(("^\\([^\n:]*:\\([0-9]+:\\)+\\)\\(.*\\)$" 1 font-lock-function-name-face))
;;;  ("^\\([^\n:]*:\\([0-9]+:\\)+\\)\\(.*\\)$" 0 font-lock-keyword-face keep)
  "Additional expressions to highlight in Compilation mode.")

;;;###autoload
(defun compile (command)
  "Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer `*compilation*'.

You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

Interactively, prompts for the command if `compilation-read-command' is
non-nil; otherwise uses `compile-command'.  With prefix arg, always prompts.

To run more than one compilation at once, start one and rename the
\`*compilation*' buffer to some other name with \\[rename-buffer].
Then start the next one.

The name used for the buffer is actually whatever is returned by
the function in `compilation-buffer-name-function', so you can set that
to a function that generates a unique name."
  (interactive
   (if (or compilation-read-command current-prefix-arg)
       (list (read-from-minibuffer "Compile command: "
                                 compile-command nil nil
                                 '(compile-history . 1)))
     (list compile-command)))
  (setq compile-command command)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (compile-internal compile-command "No more errors"))

;;; run compile with the default command line
(defun recompile ()
  "Re-compile the program including the current buffer."
  (interactive)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (compile-internal compile-command "No more errors"))
  
;;;###autoload
(defun grep (command-args)
  "Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a grep command."
  (interactive
   (list (read-from-minibuffer "Run grep (like this): "
			       grep-command nil nil 'grep-history)))
  (compile-internal (concat command-args " /dev/null")
		    "No more grep hits" "grep"
		    ;; Give it a simpler regexp to match.
		    nil grep-regexp-alist))

(defun compile-internal (command error-message
				 &optional name-of-mode parser regexp-alist
				 name-function)
  "Run compilation command COMMAND (low level interface).
ERROR-MESSAGE is a string to print if the user asks to see another error
and there are no more errors.  Third argument NAME-OF-MODE is the name
to display as the major mode in the compilation buffer.

Fourth arg PARSER is the error parser function (nil means the default).  Fifth
arg REGEXP-ALIST is the error message regexp alist to use (nil means the
default).  Sixth arg NAME-FUNCTION is a function called to name the buffer (nil
means the default).  The defaults for these variables are the global values of
\`compilation-parse-errors-function', `compilation-error-regexp-alist', and
\`compilation-buffer-name-function', respectively.

Returns the compilation buffer created."
  (let (outbuf)
    (save-excursion
      (or name-of-mode
	  (setq name-of-mode "Compilation"))
      (setq outbuf
	    (get-buffer-create
	     (funcall (or name-function compilation-buffer-name-function
			  (function (lambda (mode)
				      (concat "*" (downcase mode) "*"))))
		      name-of-mode)))
      (set-buffer outbuf)
      (let ((comp-proc (get-buffer-process (current-buffer))))
	(if comp-proc
	    (if (or (not (eq (process-status comp-proc) 'run))
		    (yes-or-no-p
		     (format "A %s process is running; kill it? "
			     name-of-mode)))
		(condition-case ()
		    (progn
		      (interrupt-process comp-proc)
		      (sit-for 1)
		      (delete-process comp-proc))
		  (error nil))
	      (error "Cannot have two processes in `%s' at once"
		     (buffer-name))
	      )))
      ;; In case the compilation buffer is current, make sure we get the global
      ;; values of compilation-error-regexp-alist, etc.
      (kill-all-local-variables))
    (let ((regexp-alist (or regexp-alist compilation-error-regexp-alist))
	  (parser (or parser compilation-parse-errors-function))
	  (thisdir default-directory)
	  outwin) 
      (save-excursion
	;; Clear out the compilation buffer and make it writable.
	;; Change its default-directory to the directory where the compilation
	;; will happen, and insert a `cd' command to indicate this.
	(set-buffer outbuf)
	(setq buffer-read-only nil)
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	(buffer-enable-undo (current-buffer))
	(setq default-directory thisdir)
	(insert "cd " thisdir "\n" command "\n")
	(set-buffer-modified-p nil))
      ;; If we're already in the compilation buffer, go to the end
      ;; of the buffer, so point will track the compilation output.
      (if (eq outbuf (current-buffer))
	  (goto-char (point-max)))
      ;; Pop up the compilation buffer.
      (setq outwin (display-buffer outbuf))
      (save-excursion
	(set-buffer outbuf)
	(compilation-mode)
	;; (setq buffer-read-only t)  ;;; Non-ergonomic.
	(set (make-local-variable 'compilation-parse-errors-function) parser)
	(set (make-local-variable 'compilation-error-message) error-message)
	(set (make-local-variable 'compilation-error-regexp-alist) regexp-alist)
	(setq default-directory thisdir
	      compilation-directory-stack (list default-directory))
	(set-window-start outwin (point-min))
	(setq mode-name name-of-mode)
	(or (eq outwin (selected-window))
	    (set-window-point outwin (point-min)))
	(compilation-set-window-height outwin)
	;; Start the compilation.
	(if (fboundp 'start-process)
	    (let* ((process-environment (cons "EMACS=t" process-environment))
		   (proc (start-process-shell-command (downcase mode-name)
						      outbuf
						      command)))
	      (set-process-sentinel proc 'compilation-sentinel)
	      (set-process-filter proc 'compilation-filter)
	      (set-marker (process-mark proc) (point) outbuf)
	      (setq compilation-in-progress 
		    (cons proc compilation-in-progress)))
	  ;; No asynchronous processes available
	  (message (format "Executing `%s'..." command))
	  (sit-for 0) ;; Force redisplay
	  (let ((status (call-process shell-file-name nil outbuf nil "-c"
				      command))))
	  (message (format "Executing `%s'...done" command)))))
    ;; Make it so the next C-x ` will use this buffer.
    (setq compilation-last-buffer outbuf)))

;; Set the height of WINDOW according to compilation-window-height.
(defun compilation-set-window-height (window)
  (and compilation-window-height
       (= (window-width window) (frame-width (window-frame window)))
       ;; If window is alone in its frame, aside from a minibuffer,
       ;; don't change its height.
       (not (eq window (frame-root-window (window-frame window))))
       ;; This save-excursion prevents us from changing the current buffer,
       ;; which might not be the same as the selected window's buffer.
       (save-excursion
	 (let ((w (selected-window)))
	   (unwind-protect
	       (progn
		 (select-window window)
		 (enlarge-window (- compilation-window-height
				    (window-height))))
	     (select-window w))))))

(defvar compilation-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'compile-mouse-goto-error)
    (define-key map "\C-c\C-c" 'compile-goto-error)
    (define-key map "\C-m" 'compile-goto-error)
    (define-key map "\C-c\C-k" 'kill-compilation)
    (define-key map "\M-n" 'compilation-next-error)
    (define-key map "\M-p" 'compilation-previous-error)
    (define-key map "\M-{" 'compilation-previous-file)
    (define-key map "\M-}" 'compilation-next-file)
    map)
  "Keymap for `compilation-minor-mode'.")

(defvar compilation-mode-map
  (let ((map (cons 'keymap compilation-minor-mode-map)))
    (define-key map " " 'scroll-up)
    (define-key map "\^?" 'scroll-down)
    ;; Set up the menu-bar
    (define-key map [menu-bar compilation-menu]
      (cons "Compile" (make-sparse-keymap "Compile")))

    (define-key map [menu-bar compilation-menu compilation-mode-kill-compilation]
      '("Stop compilation" . kill-compilation))
    (define-key map [menu-bar compilation-menu compilation-mode-separator2]
      '("----" . nil))
    (define-key map [menu-bar compilation-menu compilation-mode-first-error]
      '("First error" . first-error))
    (define-key map [menu-bar compilation-menu compilation-mode-previous-error]
      '("Previous error" . previous-error))
    (define-key map [menu-bar compilation-menu compilation-mode-next-error]
      '("Next error" . next-error))
    (define-key map [menu-bar compilation-menu compilation-separator2]
      '("----" . nil))
    (define-key map [menu-bar compilation-menu compilation-mode-grep]
      '("Grep" . grep))
    (define-key map [menu-bar compilation-menu compilation-mode-recompile]
      '("Recompile" . recompile))
    (define-key map [menu-bar compilation-menu compilation-mode-compile]
      '("Compile" . compile))
    map)
  "Keymap for compilation log buffers.
`compilation-minor-mode-map' is a cdr of this.")

(defun compilation-mode ()
  "Major mode for compilation log buffers.
\\<compilation-mode-map>To visit the source for a line-numbered error,
move point to the error message line and type \\[compile-goto-error].
To kill the compilation, type \\[kill-compilation].

Runs `compilation-mode-hook' with `run-hooks' (which see)."
  (interactive)
  (kill-all-local-variables)
  (use-local-map compilation-mode-map)
  (setq major-mode 'compilation-mode
	mode-name "Compilation")
  (compilation-setup)
  (set (make-local-variable 'font-lock-defaults)
       '(compilation-mode-font-lock-keywords t))
  (run-hooks 'compilation-mode-hook))

;; Prepare the buffer for the compilation parsing commands to work.
(defun compilation-setup ()
  ;; Make the buffer's mode line show process state.
  (setq mode-line-process '(":%s"))
  (set (make-local-variable 'compilation-error-list) nil)
  (set (make-local-variable 'compilation-old-error-list) nil)
  (set (make-local-variable 'compilation-parsing-end) 1)
  (set (make-local-variable 'compilation-directory-stack) nil)
  (setq compilation-last-buffer (current-buffer)))

(defvar compilation-minor-mode nil
  "Non-nil when in compilation-minor-mode.
In this minor mode, all the error-parsing commands of the
Compilation major mode are available.")
(make-variable-buffer-local 'compilation-minor-mode)

(or (assq 'compilation-minor-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(compilation-minor-mode " Compilation")
				 minor-mode-alist)))
(or (assq 'compilation-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'compilation-minor-mode
					   compilation-minor-mode-map)
				     minor-mode-map-alist)))

;;;###autoload
(defun compilation-minor-mode (&optional arg)
  "Toggle compilation minor mode.
With arg, turn compilation mode on if and only if arg is positive.
See `compilation-mode'."
  (interactive "P")
  (if (setq compilation-minor-mode (if (null arg)
				       (null compilation-minor-mode)
				     (> (prefix-numeric-value arg) 0)))
      (compilation-setup)))

;; Called when compilation process changes state.
(defun compilation-sentinel (proc msg)
  "Sentinel for compilation buffers."
  (let ((buffer (process-buffer proc)))
    (if (memq (process-status proc) '(signal exit))
	(progn
	  (if (null (buffer-name buffer))
	      ;; buffer killed
	      (set-process-buffer proc nil)
	    (let ((obuf (current-buffer))
		  omax opoint)
	      ;; save-excursion isn't the right thing if
	      ;; process-buffer is current-buffer
	      (unwind-protect
		  (progn
		    ;; Write something in the compilation buffer
		    ;; and hack its mode line.
		    (set-buffer buffer)
		    (let ((buffer-read-only nil))
		      (setq omax (point-max)
			    opoint (point))
		      (goto-char omax)
		      ;; Record where we put the message, so we can ignore it
		      ;; later on.
		      (insert ?\n mode-name " " msg)
		      (forward-char -1)
		      (insert " at " (substring (current-time-string) 0 19))
		      (forward-char 1)
		      (setq mode-line-process
			    (format ":%s [%d]" (process-status proc)
				    (process-exit-status proc)))
		      ;; Since the buffer and mode line will show that the
		      ;; process is dead, we can delete it now.  Otherwise it
		      ;; will stay around until M-x list-processes.
		      (delete-process proc)
		      ;; Force mode line redisplay soon.
		      (force-mode-line-update))
		    (if (and opoint (< opoint omax))
			(goto-char opoint))
		    (if compilation-finish-function
			(funcall compilation-finish-function buffer msg)))
		(set-buffer obuf))))
	  (setq compilation-in-progress (delq proc compilation-in-progress))
	  ))))

(defun compilation-filter (proc string)
  "Process filter for compilation buffers.
Just inserts the text, but uses `insert-before-markers'."
  (if (buffer-name (process-buffer proc))
      (save-excursion
	(set-buffer (process-buffer proc))
	(let ((buffer-read-only nil))
	  (save-excursion
	    (goto-char (process-mark proc))
	    (insert-before-markers string)
	    (set-marker (process-mark proc) (point)))))))

;; Return the cdr of compilation-old-error-list for the error containing point.
(defun compile-error-at-point ()
  (compile-reinitialize-errors nil (point))
  (let ((errors compilation-old-error-list))
    (while (and errors
		(> (point) (car (car errors))))
      (setq errors (cdr errors)))
    errors))

(defsubst compilation-buffer-p (buffer)
  (save-excursion
    (set-buffer buffer)
    (or compilation-minor-mode (eq major-mode 'compilation-mode))))

(defun compilation-next-error (n)
  "Move point to the next error in the compilation buffer.
Does NOT find the source line like \\[next-error]."
  (interactive "p")
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation buffer."))
  (setq compilation-last-buffer (current-buffer))

  (let ((errors (compile-error-at-point)))

    ;; Move to the error after the one containing point.
    (goto-char (car (if (< n 0)
			(let ((i 0)
			      (e compilation-old-error-list))
			  ;; See how many cdrs away ERRORS is from the start.
			  (while (not (eq e errors))
			    (setq i (1+ i)
				  e (cdr e)))
			  (if (> (- n) i)
			      (error "Moved back past first error")
			    (nth (+ i n) compilation-old-error-list)))
		      (let ((compilation-error-list (cdr errors)))
			(compile-reinitialize-errors nil nil n)
			(if compilation-error-list
			    (nth (1- n) compilation-error-list)
			  (error "Moved past last error"))))))))

(defun compilation-previous-error (n)
  "Move point to the previous error in the compilation buffer.
Does NOT find the source line like \\[next-error]."
  (interactive "p")
  (compilation-next-error (- n)))


;; Given an elt of `compilation-error-list', return an object representing
;; the referenced file which is equal to (but not necessarily eq to) what
;; this function would return for another error in the same file.
(defsubst compilation-error-filedata (data)
  (setq data (cdr data))
  (if (markerp data)
      (marker-buffer data)
    (car data)))

;; Return a string describing a value from compilation-error-filedata.
;; This value is not necessarily useful as a file name, but should be
;; indicative to the user of what file's errors are being referred to.
(defsubst compilation-error-filedata-file-name (filedata)
  (if (bufferp filedata)
      (buffer-file-name filedata)
    (car filedata)))

(defun compilation-next-file (n)
  "Move point to the next error for a different file than the current one."
  (interactive "p")
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation buffer."))
  (setq compilation-last-buffer (current-buffer))

  (let ((reversed (< n 0))
	errors filedata)

    (if (not reversed)
	(setq errors (or (compile-error-at-point)
			 (error "Moved past last error")))

      ;; Get a reversed list of the errors up through the one containing point.
      (compile-reinitialize-errors nil (point))
      (setq errors (reverse compilation-old-error-list)
	    n (- n))

      ;; Ignore errors after point.  (car ERRORS) will be the error
      ;; containing point, (cadr ERRORS) the one before it.
      (while (and errors
		  (< (point) (car (car errors))))
	(setq errors (cdr errors))))

    (while (> n 0)
      (setq filedata (compilation-error-filedata (car errors)))

      ;; Skip past the following errors for this file.
      (while (equal filedata
		    (compilation-error-filedata
		     (car (or errors
			      (if reversed
				  (error "%s the first erring file"
					 (compilation-error-filedata-file-name
					  filedata))
				(let ((compilation-error-list nil))
				  ;; Parse some more.
				  (compile-reinitialize-errors nil nil 2)
				  (setq errors compilation-error-list)))
			      (error "%s is the last erring file" 
				     (compilation-error-filedata-file-name
				      filedata))))))
	(setq errors (cdr errors)))

      (setq n (1- n)))

    ;; Move to the following error.
    (goto-char (car (car (or errors
			     (if reversed
				 (error "This is the first erring file")
			       (let ((compilation-error-list nil))
				 ;; Parse the last one.
				 (compile-reinitialize-errors nil nil 1)
				 compilation-error-list))))))))

(defun compilation-previous-file (n)
  "Move point to the previous error for a different file than the current one."
  (interactive "p")
  (compilation-next-file (- n)))


(defun kill-compilation ()
  "Kill the process made by the \\[compile] command."
  (interactive)
  (let ((buffer (compilation-find-buffer)))
    (if (get-buffer-process buffer)
	(interrupt-process (get-buffer-process buffer))
      (error "The compilation process is not running."))))


;; Parse any new errors in the compilation buffer,
;; or reparse from the beginning if the user has asked for that.
(defun compile-reinitialize-errors (reparse
				    &optional limit-search find-at-least)
  (save-excursion
    (set-buffer compilation-last-buffer)
    ;; If we are out of errors, or if user says "reparse",
    ;; discard the info we have, to force reparsing.
    (if (or (eq compilation-error-list t)
	    reparse)
	(compilation-forget-errors))
    (if (and compilation-error-list
	     (or (not limit-search)
		 (> compilation-parsing-end limit-search))
	     (or (not find-at-least)
		 (>= (length compilation-error-list) find-at-least)))
	;; Since compilation-error-list is non-nil, it points to a specific
	;; error the user wanted.  So don't move it around.
	nil
      ;; This was here for a long time (before my rewrite); why? --roland
      ;;(switch-to-buffer compilation-last-buffer)
      (set-buffer-modified-p nil)
      (if (< compilation-parsing-end (point-max))
	  ;; compilation-error-list might be non-nil if we have a non-nil
	  ;; LIMIT-SEARCH or FIND-AT-LEAST arg.  In that case its value
	  ;; records the current position in the error list, and we must
	  ;; preserve that after reparsing.
	  (let ((error-list-pos compilation-error-list))
	    (funcall compilation-parse-errors-function
		     limit-search
		     (and find-at-least
			  ;; We only need enough new parsed errors to reach
			  ;; FIND-AT-LEAST errors past the current
			  ;; position.
			  (- find-at-least (length compilation-error-list))))
	    ;; Remember the entire list for compilation-forget-errors.  If
	    ;; this is an incremental parse, append to previous list.  If
	    ;; we are parsing anew, compilation-forget-errors cleared
	    ;; compilation-old-error-list above.
	    (setq compilation-old-error-list
		  (nconc compilation-old-error-list compilation-error-list))
	    (if error-list-pos
		;; We started in the middle of an existing list of parsed
		;; errors before parsing more; restore that position.
		(setq compilation-error-list error-list-pos))
	    )))))

(defun compile-mouse-goto-error (event)
  (interactive "e")
  (save-excursion
    (set-buffer (window-buffer (posn-window (event-end event))))
    (goto-char (posn-point (event-end event)))

    (or (compilation-buffer-p (current-buffer))
	(error "Not in a compilation buffer."))
    (setq compilation-last-buffer (current-buffer))
    (compile-reinitialize-errors nil (point))

    ;; Move to bol; the marker for the error on this line will point there.
    (beginning-of-line)

    ;; Move compilation-error-list to the elt of compilation-old-error-list
    ;; we want.
    (setq compilation-error-list compilation-old-error-list)
    (while (and compilation-error-list
		(> (point) (car (car compilation-error-list))))
      (setq compilation-error-list (cdr compilation-error-list)))
    (or compilation-error-list
	(error "No error to go to")))
  (select-window (posn-window (event-end event)))
  ;; Move to another window, so that next-error's window changes
  ;; result in the desired setup.
  (or (one-window-p)
      (progn
	(other-window -1)
	;; other-window changed the selected buffer,
	;; but we didn't want to do that.
	(set-buffer compilation-last-buffer)))

  (push-mark)
  (next-error 1))

(defun compile-goto-error (&optional argp)
  "Visit the source for the error message point is on.
Use this command in a compilation log buffer.  Sets the mark at point there.
\\[universal-argument] as a prefix arg means to reparse the buffer's error messages first;
other kinds of prefix arguments are ignored."
  (interactive "P")
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation buffer."))
  (setq compilation-last-buffer (current-buffer))
  (compile-reinitialize-errors (consp argp) (point))

  ;; Move to bol; the marker for the error on this line will point there.
  (beginning-of-line)

  ;; Move compilation-error-list to the elt of compilation-old-error-list
  ;; we want.
  (setq compilation-error-list compilation-old-error-list)
  (while (and compilation-error-list
	      (> (point) (car (car compilation-error-list))))
    (setq compilation-error-list (cdr compilation-error-list)))

  ;; Move to another window, so that next-error's window changes
  ;; result in the desired setup.
  (or (one-window-p)
      (progn
	(other-window -1)
	;; other-window changed the selected buffer,
	;; but we didn't want to do that.
	(set-buffer compilation-last-buffer)))

  (push-mark)
  (next-error 1))

;; Return a compilation buffer.
;; If the current buffer is a compilation buffer, return it.
;; If compilation-last-buffer is set to a live buffer, use that.
;; Otherwise, look for a compilation buffer and signal an error
;; if there are none.
(defun compilation-find-buffer (&optional other-buffer)
  (if (and (not other-buffer)
	   (compilation-buffer-p (current-buffer)))
      ;; The current buffer is a compilation buffer.
      (current-buffer)
    (if (and compilation-last-buffer (buffer-name compilation-last-buffer)
	     (or (not other-buffer) (not (eq compilation-last-buffer
					     (current-buffer)))))
	compilation-last-buffer
      (let ((buffers (buffer-list)))
	(while (and buffers (or (not (compilation-buffer-p (car buffers)))
				(and other-buffer
				     (eq (car buffers) (current-buffer)))))
	  (setq buffers (cdr buffers)))
	(if buffers
	    (car buffers)
	  (or (and other-buffer
		   (compilation-buffer-p (current-buffer))
		   ;; The current buffer is a compilation buffer.
		   (progn
		     (if other-buffer
			 (message "This is the only compilation buffer."))
		     (current-buffer)))
	      (error "No compilation started!")))))))

;;;###autoload
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

See variables `compilation-parse-errors-function' and
\`compilation-error-regexp-alist' for customization ideas."
  (interactive "P")
  (setq compilation-last-buffer (compilation-find-buffer))
  (compilation-goto-locus (compilation-next-error-locus
			   ;; We want to pass a number here only if
			   ;; we got a numeric prefix arg, not just C-u.
			   (and (not (consp argp))
				(prefix-numeric-value argp))
			   (consp argp))))
;;;###autoload (define-key ctl-x-map "`" 'next-error)

(defun previous-error ()
  "Visit previous compilation error message and corresponding source code.
This operates on the output from the \\[compile] command."
  (interactive)
  (next-error '-1))

(defun first-error ()
  "Reparse the error message buffer and start at the first error
Visit corresponding source code.
This operates on the output from the \\[compile] command."
  (interactive)
  (next-error '(1.1)))

(defun compilation-next-error-locus (&optional move reparse silent)
  "Visit next compilation error and return locus in corresponding source code.
This operates on the output from the \\[compile] command.
If all preparsed error messages have been processed,
the error message buffer is checked for new ones.

Returns a cons (ERROR . SOURCE) of two markers: ERROR is a marker at the
location of the error message in the compilation buffer, and SOURCE is a
marker at the location in the source code indicated by the error message.

Optional first arg MOVE says how many error messages to move forwards (or
backwards, if negative); default is 1.  Optional second arg REPARSE, if
non-nil, says to reparse the error message buffer and reset to the first
error (plus MOVE - 1).  If optional third argument SILENT is non-nil, return 
nil instead of raising an error if there are no more errors.

The current buffer should be the desired compilation output buffer."
  (or move (setq move 1))
  (compile-reinitialize-errors reparse nil (and (not reparse)
						(if (< move 1) 0 (1- move))))
  (let (next-errors next-error)
    (catch 'no-next-error
      (save-excursion
	(set-buffer compilation-last-buffer)
	;; compilation-error-list points to the "current" error.
	(setq next-errors 
	      (if (> move 0)
		  (nthcdr (1- move)
			  compilation-error-list)
		;; Zero or negative arg; we need to move back in the list.
		(let ((n (1- move))
		      (i 0)
		      (e compilation-old-error-list))
		  ;; See how many cdrs away the current error is from the start.
		  (while (not (eq e compilation-error-list))
		    (setq i (1+ i)
			  e (cdr e)))
		  (if (> (- n) i)
		      (error "Moved back past first error")
		    (nthcdr (+ i n) compilation-old-error-list))))
	      next-error (car next-errors))
	(while
	    (if (null next-error)
		(progn
		  (and move (/= move 1)
		       (error (if (> move 0)
				  "Moved past last error")
			      "Moved back past first error"))
		  ;; Forget existing error messages if compilation has finished.
		  (if (not (and (get-buffer-process (current-buffer))
				(eq (process-status
				     (get-buffer-process
				      (current-buffer)))
				    'run)))
		      (compilation-forget-errors))
		  (if silent
		      (throw 'no-next-error nil)
		    (error (concat compilation-error-message
				   (and (get-buffer-process (current-buffer))
					(eq (process-status
					     (get-buffer-process
					      (current-buffer)))
					    'run)
					" yet")))))
	      (setq compilation-error-list (cdr next-errors))
	      (if (null (cdr next-error))
		  ;; This error is boring.  Go to the next.
		  t
		(or (markerp (cdr next-error))
		    ;; This error has a filename/lineno pair.
		    ;; Find the file and turn it into a marker.
		    (let* ((fileinfo (car (cdr next-error)))
			   (buffer (apply 'compilation-find-file
					  (car next-error) fileinfo)))
		      (if (null buffer)
			  ;; We can't find this error's file.
			  ;; Remove all errors in the same file.
			  (progn
			    (setq next-errors compilation-old-error-list)
			    (while next-errors
			      (and (consp (cdr (car next-errors)))
				   (equal (car (cdr (car next-errors)))
					  fileinfo)
				   (progn
				     (set-marker (car (car next-errors)) nil)
				     (setcdr (car next-errors) nil)))
			      (setq next-errors (cdr next-errors)))
			    ;; Look for the next error.
			    t)
			;; We found the file.  Get a marker for this error.
			;; compilation-old-error-list is a buffer-local
			;; variable, so we must be careful to extract its value
			;; before switching to the source file buffer.
			(let ((errors compilation-old-error-list)
			      (last-line (nth 1 (cdr next-error)))
			      (column (nth 2 (cdr next-error))))
			  (set-buffer buffer)
			  (save-excursion
			    (save-restriction
			      (widen)
			      (goto-line last-line)
			      (if (and column (> column 0))
				  ;; Columns in error msgs are 1-origin.
				  (move-to-column (1- column))
				(beginning-of-line))
			      (setcdr next-error (point-marker))
			      ;; Make all the other error messages referring
			      ;; to the same file have markers into the buffer.
			      (while errors
				(and (consp (cdr (car errors)))
				     (equal (car (cdr (car errors))) fileinfo)
				     (let* ((this (nth 1 (cdr (car errors))))
					    (column (nth 2 (cdr (car errors))))
					    (lines (- this last-line)))
				       (if (eq selective-display t)
					   ;; When selective-display is t,
					   ;; each C-m is a line boundary,
					   ;; as well as each newline.
					   (if (< lines 0)
					       (re-search-backward "[\n\C-m]"
								   nil 'end
								   (- lines))
					     (re-search-forward "[\n\C-m]"
								nil 'end
								lines))
					 (forward-line lines))
				       (if column
					   (move-to-column (1- column)))
				       (setq last-line this)
				       (setcdr (car errors) (point-marker))))
				(setq errors (cdr errors)))))))))
		;; If we didn't get a marker for this error, or this
		;; marker's buffer was killed, go on to the next one.
		(or (not (markerp (cdr next-error)))
		    (not (marker-buffer (cdr next-error))))))
	  (setq next-errors compilation-error-list
		next-error (car next-errors)))))

    ;; Skip over multiple error messages for the same source location,
    ;; so the next C-x ` won't go to an error in the same place.
    (while (and compilation-error-list
		(equal (cdr (car compilation-error-list)) (cdr next-error)))
      (setq compilation-error-list (cdr compilation-error-list)))

    ;; We now have a marker for the position of the error source code.
    ;; NEXT-ERROR is a cons (ERROR . SOURCE) of two markers.
    next-error))

(defun compilation-goto-locus (next-error)
  "Jump to an error locus returned by `compilation-next-error-locus'.
Takes one argument, a cons (ERROR . SOURCE) of two markers.
Selects a window with point at SOURCE, with another window displaying ERROR."
  (if (and (window-dedicated-p (selected-window))
	   (eq (selected-window) (frame-root-window)))
      (switch-to-buffer-other-frame (marker-buffer (cdr next-error)))
    (switch-to-buffer (marker-buffer (cdr next-error))))
  (goto-char (cdr next-error))
  ;; If narrowing got in the way of
  ;; going to the right place, widen.
  (or (= (point) (marker-position (cdr next-error)))
      (progn
	(widen)
	(goto-char (cdr next-error))))

  ;; Show compilation buffer in other window, scrolled to this error.
  (let* ((pop-up-windows t)
	 (w (or (get-buffer-window (marker-buffer (car next-error)) 'visible)
		(display-buffer (marker-buffer (car next-error))))))
    (set-window-point w (car next-error))
    (set-window-start w (car next-error))
    (compilation-set-window-height w)))

;; Find a buffer for file FILENAME.
;; Search the directories in compilation-search-path.
;; A nil in compilation-search-path means to try the
;; current directory, which is passed in DIR.
;; If FILENAME is not found at all, ask the user where to find it.
;; Pop up the buffer containing MARKER and scroll to MARKER if we ask the user.
(defun compilation-find-file (marker filename dir &rest formats)
  (or formats (setq formats '("%s")))
  (let ((dirs compilation-search-path)
	result thisdir fmts name)
    (while (and dirs (null result))
      (setq thisdir (or (car dirs) dir)
	    fmts formats)
      (while (and fmts (null result))
	(setq name (expand-file-name (format (car fmts) filename) thisdir)
	      result (and (file-exists-p name)
			  (find-file-noselect name))
	      fmts (cdr fmts)))
      (setq dirs (cdr dirs)))
    (or result
	;; The file doesn't exist.
	;; Ask the user where to find it.
	;; If he hits C-g, then the next time he does
	;; next-error, he'll skip past it.
	(progn
	  (let* ((pop-up-windows t)
		 (w (display-buffer (marker-buffer marker))))
	    (set-window-point w marker)
	    (set-window-start w marker))
	  (setq name
		(expand-file-name
		 (read-file-name (format "Find this error in: (default %s) "
					 filename)
				 dir filename t)))
	  (if (file-directory-p name)
	      (setq name (concat (file-name-as-directory name) filename)))
	  (if (file-exists-p name)
	      (find-file-noselect name))))))

;; Set compilation-error-list to nil, and unchain the markers that point to the
;; error messages and their text, so that they no longer slow down gap motion.
;; This would happen anyway at the next garbage collection, but it is better to
;; do it right away.
(defun compilation-forget-errors ()
  (while compilation-old-error-list
    (let ((next-error (car compilation-old-error-list)))
      (set-marker (car next-error) nil)
      (if (markerp (cdr next-error))
	  (set-marker (cdr next-error) nil)))
    (setq compilation-old-error-list (cdr compilation-old-error-list)))
  (setq compilation-error-list nil
	compilation-directory-stack nil
	compilation-parsing-end 1))


(defun count-regexp-groupings (regexp)
  "Return the number of \\( ... \\) groupings in REGEXP (a string)."
  (let ((groupings 0)
	(len (length regexp))
	(i 0)
	c)
    (while (< i len)
      (setq c (aref regexp i)
	    i (1+ i))
      (cond ((= c ?\[)
	     ;; Find the end of this [...].
	     (while (and (< i len)
			 (not (= (aref regexp i) ?\])))
	       (setq i (1+ i))))
	    ((= c ?\\)
	     (if (< i len)
		 (progn
		   (setq c (aref regexp i)
			 i (1+ i))
		   (if (= c ?\))
		       ;; We found the end of a grouping,
		       ;; so bump our counter.
		       (setq groupings (1+ groupings))))))))
    groupings))

(defun compilation-parse-errors (limit-search find-at-least)
  "Parse the current buffer as grep, cc or lint error messages.
See variable `compilation-parse-errors-function' for the interface it uses."
  (setq compilation-error-list nil)
  (message "Parsing error messages...")
  (let (text-buffer orig orig-expanded parent-expanded
	regexp enter-group leave-group error-group
	alist subexpr error-regexp-groups
	(found-desired nil)
	(compilation-num-errors-found 0))

    ;; Don't reparse messages already seen at last parse.
    (goto-char compilation-parsing-end)
    ;; Don't parse the first two lines as error messages.
    ;; This matters for grep.
    (if (bobp)
	(progn
	  (forward-line 2)
	  ;; Move back so point is before the newline.
	  ;; This matters because some error regexps use \n instead of ^
	  ;; to be faster.
	  (forward-char -1)))

    ;; Compile all the regexps we want to search for into one.
    (setq regexp (concat "\\(" compilation-enter-directory-regexp "\\)\\|"
			 "\\(" compilation-leave-directory-regexp "\\)\\|"
			 "\\(" (mapconcat (function
					   (lambda (elt)
					     (concat "\\(" (car elt) "\\)")))
					  compilation-error-regexp-alist
					  "\\|") "\\)"))

    ;; Find out how many \(...\) groupings are in each of the regexps, and set
    ;; *-GROUP to the grouping containing each constituent regexp (whose
    ;; subgroups will come immediately thereafter) of the big regexp we have
    ;; just constructed.
    (setq enter-group 1
	  leave-group (+ enter-group
			 (count-regexp-groupings
			  compilation-enter-directory-regexp)
			 1)
	  error-group (+ leave-group
			 (count-regexp-groupings
			  compilation-leave-directory-regexp)
			 1))

    ;; Compile an alist (IDX FILE LINE [COL]), where IDX is the number of
    ;; the subexpression for an entire error-regexp, and FILE and LINE (and
    ;; possibly COL) are the numbers for the subexpressions giving the file
    ;; name and line number (and possibly column number).
    (setq alist (or compilation-error-regexp-alist
		    (error "compilation-error-regexp-alist is empty!"))
	  subexpr (1+ error-group))
    (while alist
      (setq error-regexp-groups
	    (cons (list subexpr
			(+ subexpr (nth 1 (car alist)))
			(+ subexpr (nth 2 (car alist)))
			(and (nth 3 (car alist))
			     (+ subexpr (nth 3 (car alist)))))
		  error-regexp-groups))
      (setq subexpr (+ subexpr 1 (count-regexp-groupings (car (car alist)))))
      (setq alist (cdr alist)))

    (setq orig default-directory)
    (setq orig-expanded (file-truename orig))
    (setq parent-expanded (expand-file-name "../" orig-expanded))

    (while (and (not found-desired)
		;; We don't just pass LIMIT-SEARCH to re-search-forward
		;; because we want to find matches containing LIMIT-SEARCH
		;; but which extend past it.
		(re-search-forward regexp nil t))

      ;; Figure out which constituent regexp matched.
      (cond ((match-beginning enter-group)
	     ;; The match was the enter-directory regexp.
	     (let ((dir
		    (file-name-as-directory
		     (expand-file-name
		      (buffer-substring (match-beginning (+ enter-group 1))
					(match-end (+ enter-group 1)))))))
	       ;; The directory name in the "entering" message
	       ;; is a truename.  Try to convert it to a form
	       ;; like what the user typed in.
	       (setq dir
		     (compile-abbreviate-directory dir orig orig-expanded
						   parent-expanded))
	       (setq compilation-directory-stack
		     (cons dir compilation-directory-stack))
	       (and (file-directory-p dir)
		    (setq default-directory dir)))

	     (and limit-search (>= (point) limit-search)
		  ;; The user wanted a specific error, and we're past it.
		  ;; We do this check here (and in the leave-group case)
		  ;; rather than at the end of the loop because if the last
		  ;; thing seen is an error message, we must carefully
		  ;; discard the last error when it is the first in a new
		  ;; file (see below in the error-group case).
		  (setq found-desired t)))

	    ((match-beginning leave-group)
	     ;; The match was the leave-directory regexp.
	     (let ((beg (match-beginning (+ leave-group 1)))
		   (stack compilation-directory-stack))
	       (if beg
		   (let ((dir
			  (file-name-as-directory
			   (expand-file-name
			    (buffer-substring beg
					      (match-end (+ leave-group
							    1)))))))
		     ;; The directory name in the "entering" message
		     ;; is a truename.  Try to convert it to a form
		     ;; like what the user typed in.
		     (setq dir
			   (compile-abbreviate-directory dir orig orig-expanded
							 parent-expanded))
		     (while (and stack
				 (not (string-equal (car stack) dir)))
		       (setq stack (cdr stack)))))
	       (setq compilation-directory-stack (cdr stack))
	       (setq stack (car compilation-directory-stack))
	       (if stack
		   (setq default-directory stack))
	       )

	     (and limit-search (>= (point) limit-search)
		  ;; The user wanted a specific error, and we're past it.
		  ;; We do this check here (and in the enter-group case)
		  ;; rather than at the end of the loop because if the last
		  ;; thing seen is an error message, we must carefully
		  ;; discard the last error when it is the first in a new
		  ;; file (see below in the error-group case).
		  (setq found-desired t)))

	    ((match-beginning error-group)
	     ;; The match was the composite error regexp.
	     ;; Find out which individual regexp matched.
	     (setq alist error-regexp-groups)
	     (while (and alist
			 (null (match-beginning (car (car alist)))))
	       (setq alist (cdr alist)))
	     (if alist
		 (setq alist (car alist))
	       (error "compilation-parse-errors: impossible regexp match!"))
	     
	     ;; Extract the file name and line number from the error message.
	     (let ((beginning-of-match (match-beginning 0)) ;looking-at nukes
		   (filename (buffer-substring (match-beginning (nth 1 alist))
					       (match-end (nth 1 alist))))
		   (linenum (string-to-int
			     (buffer-substring
			      (match-beginning (nth 2 alist))
			      (match-end (nth 2 alist)))))
		   (column (and (nth 3 alist)
				(match-beginning (nth 3 alist))
				(string-to-int
				 (buffer-substring
				  (match-beginning (nth 3 alist))
				  (match-end (nth 3 alist)))))))

	       ;; Check for a comint-file-name-prefix and prepend it if
	       ;; appropriate.  (This is very useful for
	       ;; compilation-minor-mode in an rlogin-mode buffer.)
	       (and (boundp 'comint-file-name-prefix)
		    ;; If the file name is relative, default-directory will
		    ;; already contain the comint-file-name-prefix (done by
		    ;; compile-abbreviate-directory).
		    (file-name-absolute-p filename)
		    (setq filename (concat comint-file-name-prefix filename)))
	       (setq filename (cons filename (cons default-directory
						   (nthcdr 4 alist))))
				     

	       ;; Locate the erring file and line.
	       ;; Cons a new elt onto compilation-error-list,
	       ;; giving a marker for the current compilation buffer
	       ;; location, and the file and line number of the error.
	       (save-excursion
		 (beginning-of-line 1)
		 (let ((this (cons (point-marker)
				   (list filename linenum column))))
		   ;; Don't add the same source line more than once.
		   (if (equal (cdr this) (cdr (car compilation-error-list)))
		       nil
		     (setq compilation-error-list
			   (cons this
				 compilation-error-list))
		     (setq compilation-num-errors-found
			   (1+ compilation-num-errors-found)))))
	       (and (or (and find-at-least (> compilation-num-errors-found
					      find-at-least))
			(and limit-search (>= (point) limit-search)))
		    ;; We have found as many new errors as the user wants,
		    ;; or past the buffer position he indicated.  We
		    ;; continue to parse until we have seen all the
		    ;; consecutive errors in the same file, so the error
		    ;; positions will be recorded as markers in this buffer
		    ;; that might change.
		    (cdr compilation-error-list) ; Must check at least two.
		    (not (equal (car (cdr (nth 0 compilation-error-list)))
				(car (cdr (nth 1 compilation-error-list)))))
		    (progn
		      ;; Discard the error just parsed, so that the next
		      ;; parsing run can get it and the following errors in
		      ;; the same file all at once.  If we didn't do this, we
		      ;; would have the same problem we are trying to avoid
		      ;; with the test above, just delayed until the next run!
		      (setq compilation-error-list
			    (cdr compilation-error-list))
		      (goto-char beginning-of-match)
		      (setq found-desired t)))
	       )
	     )
	    (t
	     (error "compilation-parse-errors: known groups didn't match!")))

      (message "Parsing error messages...%d (%.0f%% of buffer)"
	       compilation-num-errors-found
	       ;; Use floating-point because (* 100 (point)) frequently
	       ;; exceeds the range of Emacs Lisp integers.
	       (/ (* 100.0 (point)) (point-max)))

      (and limit-search (>= (point) limit-search)
	   ;; The user wanted a specific error, and we're past it.
	   (setq found-desired t)))
    (setq compilation-parsing-end (if found-desired
				      (point)
				    ;; We have searched the whole buffer.
				    (point-max))))
  (setq compilation-error-list (nreverse compilation-error-list))
  (message "Parsing error messages...done"))

;; If directory DIR is a subdir of ORIG or of ORIG's parent,
;; return a relative name for it starting from ORIG or its parent.
;; ORIG-EXPANDED is an expanded version of ORIG.
;; PARENT-EXPANDED is an expanded version of ORIG's parent.
;; Those two args could be computed here, but we run faster by
;; having the caller compute them just once.
(defun compile-abbreviate-directory (dir orig orig-expanded parent-expanded)
  ;; Check for a comint-file-name-prefix and prepend it if appropriate.
  ;; (This is very useful for compilation-minor-mode in an rlogin-mode
  ;; buffer.)
  (if (boundp 'comint-file-name-prefix)
      (setq dir (concat comint-file-name-prefix dir)))

  (if (and (> (length dir) (length orig-expanded))
	   (string= orig-expanded
		    (substring dir 0 (length orig-expanded))))
      (setq dir
	    (concat orig
		    (substring dir (length orig-expanded)))))
  (if (and (> (length dir) (length parent-expanded))
	   (string= parent-expanded
		    (substring dir 0 (length parent-expanded))))
    (setq dir
	  (concat (file-name-directory
		   (directory-file-name orig))
		  (substring dir (length parent-expanded)))))
  dir)

(provide 'compile)

;;; compile.el ends here
