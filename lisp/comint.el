;;; comint.el --- general command interpreter in a window stuff

;; Copyright (C) 1988, 1990, 1992, 1993, 1994 Free Software Foundation, Inc.

;; Author: Olin Shivers <shivers@cs.cmu.edu>
;; Adapted-by: Simon Marshall <simon@gnu.ai.mit.edu>
;; Keywords: processes

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

;;; Please send me bug reports, bug fixes, and extensions, so that I can
;;; merge them into the master source.
;;;     - Olin Shivers (shivers@cs.cmu.edu)
;;;     - Simon Marshall (simon@gnu.ai.mit.edu)

;;; This file defines a general command-interpreter-in-a-buffer package
;;; (comint mode). The idea is that you can build specific process-in-a-buffer
;;; modes on top of comint mode -- e.g., lisp, shell, scheme, T, soar, ....
;;; This way, all these specific packages share a common base functionality, 
;;; and a common set of bindings, which makes them easier to use (and
;;; saves code, implementation time, etc., etc.).

;;; Several packages are already defined using comint mode:
;;; - shell.el defines a shell-in-a-buffer mode.
;;; - cmulisp.el defines a simple lisp-in-a-buffer mode.
;;;
;;; - The file cmuscheme.el defines a scheme-in-a-buffer mode.
;;; - The file tea.el tunes scheme and inferior-scheme modes for T.
;;; - The file soar.el tunes lisp and inferior-lisp modes for Soar.
;;; - cmutex.el defines tex and latex modes that invoke tex, latex, bibtex,
;;;   previewers, and printers from within emacs.
;;; - background.el allows csh-like job control inside emacs.
;;; It is pretty easy to make new derived modes for other processes.

;;; For documentation on the functionality provided by comint mode, and
;;; the hooks available for customising it, see the comments below.
;;; For further information on the standard derived modes (shell, 
;;; inferior-lisp, inferior-scheme, ...), see the relevant source files.

;;; For hints on converting existing process modes (e.g., tex-mode,
;;; background, dbx, gdb, kermit, prolog, telnet) to use comint-mode
;;; instead of shell-mode, see the notes at the end of this file.


;;; Brief Command Documentation:
;;;============================================================================
;;; Comint Mode Commands: (common to all derived modes, like shell & cmulisp
;;; mode)
;;;
;;; m-p	    comint-previous-input    	    Cycle backwards in input history
;;; m-n	    comint-next-input  	    	    Cycle forwards
;;; m-r     comint-previous-matching-input  Previous input matching a regexp
;;; m-s     comint-next-matching-input      Next input that matches
;;; m-c-l   comint-show-output		    Show last batch of process output
;;; return  comint-send-input
;;; c-d	    comint-delchar-or-maybe-eof     Delete char unless at end of buff
;;; c-c c-a comint-bol                      Beginning of line; skip prompt
;;; c-c c-u comint-kill-input	    	    ^u
;;; c-c c-w backward-kill-word    	    ^w
;;; c-c c-c comint-interrupt-subjob 	    ^c
;;; c-c c-z comint-stop-subjob	    	    ^z
;;; c-c c-\ comint-quit-subjob	    	    ^\
;;; c-c c-o comint-kill-output		    Delete last batch of process output
;;; c-c c-r comint-show-output		    Show last batch of process output
;;; c-c c-l comint-dynamic-list-input-ring  List input history
;;;
;;; Not bound by default in comint-mode (some are in shell mode)
;;; comint-run				Run a program under comint-mode
;;; send-invisible			Read a line w/o echo, and send to proc
;;; comint-dynamic-complete-filename	Complete filename at point.
;;; comint-dynamic-complete-variable    Complete variable name at point.
;;; comint-dynamic-list-filename-completions List completions in help buffer.
;;; comint-replace-by-expanded-filename	Expand and complete filename at point;
;;;					replace with expanded/completed name.
;;; comint-replace-by-expanded-history	Expand history at point;
;;;					replace with expanded name.
;;; comint-magic-space                  Expand history and add (a) space(s).
;;; comint-kill-subjob			No mercy.
;;; comint-show-maximum-output          Show as much output as possible.
;;; comint-continue-subjob		Send CONT signal to buffer's process
;;;					group. Useful if you accidentally
;;;					suspend your process (with C-c C-z).

;;; comint-mode-hook is the comint mode hook. Basically for your keybindings.

;;; Code:

(require 'ring)

;;; Buffer Local Variables:
;;;============================================================================
;;; Comint mode buffer local variables:
;;;     comint-prompt-regexp    - string       comint-bol uses to match prompt
;;;     comint-delimiter-argument-list - list  For delimiters and arguments
;;;     comint-last-input-start - marker       Handy if inferior always echoes
;;;     comint-last-input-end   - marker       For comint-kill-output command
;;;     comint-input-ring-size  - integer      For the input history
;;;     comint-input-ring       - ring             mechanism
;;;     comint-input-ring-index - number           ...
;;;     comint-input-autoexpand - symbol           ...
;;;     comint-input-ignoredups - boolean          ...
;;;     comint-last-input-match - string           ...
;;;     comint-dynamic-complete-functions - hook   For the completion mechanism
;;;     comint-completion-fignore - list           ...
;;;     comint-get-old-input    - function     Hooks for specific 
;;;     comint-input-filter-functions - hook     process-in-a-buffer
;;;     comint-output-filter-functions - hook    function modes.
;;;     comint-input-filter     - function         ...
;;;     comint-input-send	- function         ...
;;;     comint-eol-on-send	- boolean          ...
;;;     comint-process-echoes   - boolean          ...
;;;     comint-scroll-to-bottom-on-input - symbol For scroll behavior
;;;     comint-scroll-to-bottom-on-output - symbol ...
;;;     comint-scroll-show-maximum-output - boolean...
;;;
;;; Comint mode non-buffer local variables:
;;;     comint-completion-addsuffix - boolean  For file name completion
;;;     comint-completion-autolist  - boolean      behavior
;;;     comint-completion-recexact  - boolean      ...

(defvar comint-prompt-regexp "^"
  "Regexp to recognise prompts in the inferior process.
Defaults to \"^\", the null string at BOL.

Good choices:
  Canonical Lisp: \"^[^> \\n]*>+:? *\" (Lucid, franz, kcl, T, cscheme, oaklisp)
  Lucid Common Lisp: \"^\\\\(>\\\\|\\\\(->\\\\)+\\\\) *\"
  franz: \"^\\\\(->\\\\|<[0-9]*>:\\\\) *\"
  kcl: \"^>+ *\"
  shell: \"^[^#$%>\\n]*[#$%>] *\"
  T: \"^>+ *\"

This is a good thing to set in mode hooks.")

(defvar comint-delimiter-argument-list ()
  "List of characters to recognise as separate arguments in input.
Strings comprising a character in this list will separate the arguments
surrounding them, and also be regarded as arguments in their own right (unlike
whitespace).  See `comint-arguments'.
Defaults to the empty list.

For shells, a good value is (?\\| ?& ?< ?> ?\\( ?\\) ?;).

This is a good thing to set in mode hooks.")

(defvar comint-input-autoexpand nil
  "*If non-nil, expand input command history references on completion.
This mirrors the optional behavior of tcsh (its autoexpand and histlit).

If the value is `input', then the expansion is seen on input.
If the value is `history', then the expansion is only when inserting
into the buffer's input ring.  See also `comint-magic-space' and
`comint-dynamic-complete'.

This variable is buffer-local.")

(defvar comint-input-ignoredups nil
  "*If non-nil, don't add input matching the last on the input ring.
This mirrors the optional behavior of bash.

This variable is buffer-local.")

(defvar comint-input-ring-file-name nil
  "*If non-nil, name of the file to read/write input history.
See also `comint-read-input-ring' and `comint-write-input-ring'.

This variable is buffer-local, and is a good thing to set in mode hooks.")

(defvar comint-scroll-to-bottom-on-input nil
  "*Controls whether input to interpreter causes window to scroll.
If nil, then do not scroll.  If t or `all', scroll all windows showing buffer.
If `this', scroll only the selected window.

The default is nil.

See `comint-preinput-scroll-to-bottom'.  This variable is buffer-local.")

(defvar comint-scroll-to-bottom-on-output nil
  "*Controls whether interpreter output causes window to scroll.
If nil, then do not scroll.  If t or `all', scroll all windows showing buffer.
If `this', scroll only the selected window.
If `others', scroll only those that are not the selected window.

The default is nil.

See variable `comint-scroll-show-maximum-output' and function
`comint-postoutput-scroll-to-bottom'.  This variable is buffer-local.")

(defvar comint-scroll-show-maximum-output nil
  "*Controls how interpreter output causes window to scroll.
If non-nil, then show the maximum output when the window is scrolled.

See variable `comint-scroll-to-bottom-on-output' and function
`comint-postoutput-scroll-to-bottom'.  This variable is buffer-local.")

(defvar comint-input-ring-size 32
  "Size of input history ring.")

(defvar comint-process-echoes nil
  "*If non-nil, assume that the subprocess echoes any input.
If so, delete one copy of the input so that only one copy eventually
appears in the buffer.

This variable is buffer-local.")

(defvar comint-password-prompt-regexp
  "\\(^[Pp]assword\\|pass phrase\\):\\s *\\'"
  "*Regexp matching prompts for passwords in the inferior process.
This is used by `comint-watch-for-password-prompt'.")

;;; Here are the per-interpreter hooks.
(defvar comint-get-old-input (function comint-get-old-input-default)
  "Function that returns old text in comint mode.
This function is called when return is typed while the point is in old text.
It returns the text to be submitted as process input.  The default is
`comint-get-old-input-default', which grabs the current line, and strips off
leading text matching `comint-prompt-regexp'.")

(defvar comint-dynamic-complete-functions
  '(comint-replace-by-expanded-history comint-dynamic-complete-filename)
  "List of functions called to perform completion.
Functions should return non-nil if completion was performed.
See also `comint-dynamic-complete'.

This is a good thing to set in mode hooks.")

(defvar comint-input-filter
  (function (lambda (str) (not (string-match "\\`\\s *\\'" str))))
  "Predicate for filtering additions to input history.
Takes one argument, the input.  If non-nil, the input may be saved on the input
history list.  Default is to save anything that isn't all whitespace.")

(defvar comint-input-filter-functions '()
  "Functions to call before input is sent to the process.
These functions get one argument, a string containing the text to send.

This variable is buffer-local.")

(defvar comint-output-filter-functions '(comint-postoutput-scroll-to-bottom) 
  "Functions to call after output is inserted into the buffer.
One possible function is `comint-postoutput-scroll-to-bottom'.
These functions get one argument, a string containing the text just inserted.

This variable is buffer-local.")

(defvar comint-input-sender (function comint-simple-send)
  "Function to actually send to PROCESS the STRING submitted by user.
Usually this is just `comint-simple-send', but if your mode needs to 
massage the input string, put a different function here.
`comint-simple-send' just sends the string plus a newline.
This is called from the user command `comint-send-input'.")

(defvar comint-eol-on-send t
  "*Non-nil means go to the end of the line before sending input.
See `comint-send-input'.")

(defvar comint-mode-hook '()
  "Called upon entry into comint-mode
This is run before the process is cranked up.")

(defvar comint-exec-hook '()
  "Called each time a process is exec'd by `comint-exec'.
This is called after the process is cranked up.  It is useful for things that
must be done each time a process is executed in a comint mode buffer (e.g.,
`(process-kill-without-query)').  In contrast, the `comint-mode-hook' is only
executed once when the buffer is created.")

(defvar comint-mode-map nil)

(defvar comint-ptyp t
  "Non-nil if communications via pty; false if by pipe.  Buffer local.
This is to work around a bug in Emacs process signalling.")

(defvar comint-input-ring nil)
(defvar comint-last-input-start)
(defvar comint-last-input-end)
(defvar comint-last-output-start)
(defvar comint-input-ring-index nil
  "Index of last matched history element.")
(defvar comint-matching-input-from-input-string ""
  "Input previously used to match input history.")

(put 'comint-replace-by-expanded-history 'menu-enable 'comint-input-autoexpand)
(put 'comint-input-ring 'permanent-local t)
(put 'comint-input-ring-index 'permanent-local t)
(put 'comint-input-autoexpand 'permanent-local t)
(put 'comint-input-filter-functions 'permanent-local t)
(put 'comint-output-filter-functions 'permanent-local t)
(put 'comint-scroll-to-bottom-on-input 'permanent-local t)
(put 'comint-scroll-to-bottom-on-output 'permanent-local t)
(put 'comint-scroll-show-maximum-output 'permanent-local t)
(put 'comint-ptyp 'permanent-local t)

(defun comint-mode ()
  "Major mode for interacting with an inferior interpreter.
Interpreter name is same as buffer name, sans the asterisks.
Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
Setting variable `comint-eol-on-send' means jump to the end of the line
before submitting new input.

This mode is customised to create major modes such as Inferior Lisp
mode, Shell mode, etc.  This can be done by setting the hooks
`comint-input-filter-functions', `comint-input-filter', `comint-input-sender'
and `comint-get-old-input' to appropriate functions, and the variable
`comint-prompt-regexp' to the appropriate regular expression.

An input history is maintained of size `comint-input-ring-size', and
can be accessed with the commands \\[comint-next-input], \\[comint-previous-input], and \\[comint-dynamic-list-input-ring].
Input ring history expansion can be achieved with the commands
\\[comint-replace-by-expanded-history] or \\[comint-magic-space].
Input ring expansion is controlled by the variable `comint-input-autoexpand',
and addition is controlled by the variable `comint-input-ignoredups'.

Commands with no default key bindings include `send-invisible',
`comint-dynamic-complete', `comint-dynamic-list-filename-completions', and 
`comint-magic-space'.

Input to, and output from, the subprocess can cause the window to scroll to
the end of the buffer.  See variables `comint-output-filter-functions',
`comint-scroll-to-bottom-on-input', and `comint-scroll-to-bottom-on-output'.

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

\\{comint-mode-map}

Entry to this mode runs the hooks on `comint-mode-hook'."
  (interactive)
  ;; Do not remove this.  All major modes must do this.
  (kill-all-local-variables)
  (setq major-mode 'comint-mode)
  (setq mode-name "Comint")
  (setq mode-line-process '(":%s"))
  (use-local-map comint-mode-map)
  (make-local-variable 'comint-last-input-start)
  (setq comint-last-input-start (make-marker))
  (set-marker comint-last-input-start (point-min))
  (make-local-variable 'comint-last-input-end)
  (setq comint-last-input-end (make-marker))
  (set-marker comint-last-input-end (point-min))
  (make-local-variable 'comint-last-output-start)
  (setq comint-last-output-start (make-marker))
  (make-local-variable 'comint-prompt-regexp)        ; Don't set; default
  (make-local-variable 'comint-input-ring-size)      ; ...to global val.
  (make-local-variable 'comint-input-ring)
  (make-local-variable 'comint-input-ring-file-name)
  (or (and (boundp 'comint-input-ring) comint-input-ring)
      (setq comint-input-ring (make-ring comint-input-ring-size)))
  (make-local-variable 'comint-input-ring-index)
  (or (and (boundp 'comint-input-ring-index) comint-input-ring-index)
      (setq comint-input-ring-index nil))
  (make-local-variable 'comint-matching-input-from-input-string)
  (make-local-variable 'comint-input-autoexpand)
  (make-local-variable 'comint-input-ignoredups)
  (make-local-variable 'comint-delimiter-argument-list)
  (make-local-variable 'comint-dynamic-complete-functions)
  (make-local-variable 'comint-completion-fignore)
  (make-local-variable 'comint-get-old-input)
  (make-local-variable 'comint-input-filter-functions)
  (make-local-variable 'comint-input-filter)
  (make-local-variable 'comint-input-sender)
  (make-local-variable 'comint-eol-on-send)
  (make-local-variable 'comint-scroll-to-bottom-on-input)
  (make-local-variable 'comint-scroll-to-bottom-on-output)
  (make-local-variable 'comint-scroll-show-maximum-output)
  (make-local-variable 'pre-command-hook)
  (add-hook 'pre-command-hook 'comint-preinput-scroll-to-bottom)
  (make-local-variable 'comint-output-filter-functions)
  (make-local-variable 'comint-ptyp)
  (make-local-variable 'comint-exec-hook)
  (make-local-variable 'comint-process-echoes)
  (run-hooks 'comint-mode-hook))

(if comint-mode-map
    nil
  ;; Keys:
  (setq comint-mode-map (make-sparse-keymap))
  (define-key comint-mode-map "\ep" 'comint-previous-input)
  (define-key comint-mode-map "\en" 'comint-next-input)
  (define-key comint-mode-map [C-up] 'comint-previous-input)
  (define-key comint-mode-map [C-down] 'comint-next-input)
  (define-key comint-mode-map "\er" 'comint-previous-matching-input)
  (define-key comint-mode-map "\es" 'comint-next-matching-input)
  (define-key comint-mode-map [?\A-\M-r] 'comint-previous-matching-input-from-input)
  (define-key comint-mode-map [?\A-\M-s] 'comint-next-matching-input-from-input)
  (define-key comint-mode-map "\e\C-l" 'comint-show-output)
  (define-key comint-mode-map "\C-m" 'comint-send-input)
  (define-key comint-mode-map "\C-d" 'comint-delchar-or-maybe-eof)
  (define-key comint-mode-map "\C-c\C-a" 'comint-bol)
  (define-key comint-mode-map "\C-c\C-u" 'comint-kill-input)
  (define-key comint-mode-map "\C-c\C-w" 'backward-kill-word)
  (define-key comint-mode-map "\C-c\C-c" 'comint-interrupt-subjob)
  (define-key comint-mode-map "\C-c\C-z" 'comint-stop-subjob)
  (define-key comint-mode-map "\C-c\C-\\" 'comint-quit-subjob)
  (define-key comint-mode-map "\C-c\C-m" 'comint-copy-old-input)
  (define-key comint-mode-map "\C-c\C-o" 'comint-kill-output)
  (define-key comint-mode-map "\C-c\C-r" 'comint-show-output)
  (define-key comint-mode-map "\C-c\C-e" 'comint-show-maximum-output)
  (define-key comint-mode-map "\C-c\C-l" 'comint-dynamic-list-input-ring)
  (define-key comint-mode-map "\C-c\C-n" 'comint-next-prompt)
  (define-key comint-mode-map "\C-c\C-p" 'comint-previous-prompt)
  (define-key comint-mode-map "\C-c\C-d" 'comint-send-eof)
  ;; Menu bars:
  ;; completion:
  (define-key comint-mode-map [menu-bar completion] 
    (cons "Complete" (make-sparse-keymap "Complete")))
  (define-key comint-mode-map [menu-bar completion complete-expand]
    '("Expand File Name" . comint-replace-by-expanded-filename))
  (define-key comint-mode-map [menu-bar completion complete-listing]
    '("File Completion Listing" . comint-dynamic-list-filename-completions))
  (define-key comint-mode-map [menu-bar completion complete-file]
    '("Complete File Name" . comint-dynamic-complete-filename))
  (define-key comint-mode-map [menu-bar completion complete]
    '("Complete Before Point" . comint-dynamic-complete))
  ;; Input history:
  (define-key comint-mode-map [menu-bar inout] 
    (cons "In/Out" (make-sparse-keymap "In/Out")))
  (define-key comint-mode-map [menu-bar inout kill-output]
    '("Kill Current Output Group" . comint-kill-output))
  (define-key comint-mode-map [menu-bar inout next-prompt]
    '("Forward Output Group" . comint-next-prompt))
  (define-key comint-mode-map [menu-bar inout previous-prompt]
    '("Backward Output Group" . comint-previous-prompt))
  (define-key comint-mode-map [menu-bar inout show-maximum-output]
    '("Show Maximum Output" . comint-show-maximum-output))
  (define-key comint-mode-map [menu-bar inout show-output]
    '("Show Current Output Group" . comint-show-output))
  (define-key comint-mode-map [menu-bar inout kill-input]
    '("Kill Current Input" . comint-kill-input))
  (define-key comint-mode-map [menu-bar inout copy-input]
    '("Copy Old Input" . comint-copy-old-input))
  (define-key comint-mode-map [menu-bar inout forward-matching-history]
    '("Forward Matching Input..." . comint-forward-matching-input))
  (define-key comint-mode-map [menu-bar inout backward-matching-history]
    '("Backward Matching Input..." . comint-backward-matching-input))
  (define-key comint-mode-map [menu-bar inout next-matching-history]
    '("Next Matching Input..." . comint-next-matching-input))
  (define-key comint-mode-map [menu-bar inout previous-matching-history]
    '("Previous Matching Input..." . comint-previous-matching-input))
  (define-key comint-mode-map [menu-bar inout next-matching-history-from-input]
    '("Next Matching Current Input" . comint-next-matching-input-from-input))
  (define-key comint-mode-map [menu-bar inout previous-matching-history-from-input]
    '("Previous Matching Current Input" . comint-previous-matching-input-from-input))
  (define-key comint-mode-map [menu-bar inout next-history]
    '("Next Input" . comint-next-input))
  (define-key comint-mode-map [menu-bar inout previous-history]
    '("Previous Input" . comint-previous-input))
  (define-key comint-mode-map [menu-bar inout list-history]
    '("List Input History" . comint-dynamic-list-input-ring))
  (define-key comint-mode-map [menu-bar inout expand-history]
    '("Expand History Before Point" . comint-replace-by-expanded-history))
  ;; Signals
  (define-key comint-mode-map [menu-bar signals]
    (cons "Signals" (make-sparse-keymap "Signals")))
  (define-key comint-mode-map [menu-bar signals eof]
    '("EOF" . comint-send-eof))
  (define-key comint-mode-map [menu-bar signals kill]
    '("KILL" . comint-kill-subjob))
  (define-key comint-mode-map [menu-bar signals quit]
    '("QUIT" . comint-quit-subjob))
  (define-key comint-mode-map [menu-bar signals cont]
    '("CONT" . comint-continue-subjob))
  (define-key comint-mode-map [menu-bar signals stop]
    '("STOP" . comint-stop-subjob))
  (define-key comint-mode-map [menu-bar signals break]
    '("BREAK" . comint-interrupt-subjob))
  ;; Put them in the menu bar:
  (setq menu-bar-final-items (append '(completion inout signals)
				     menu-bar-final-items))
  )

(defun comint-check-proc (buffer)
  "Return t if there is a living process associated w/buffer BUFFER.
Living means the status is `open', `run', or `stop'.
BUFFER can be either a buffer or the name of one."
  (let ((proc (get-buffer-process buffer)))
    (and proc (memq (process-status proc) '(open run stop)))))

;;; Note that this guy, unlike shell.el's make-shell, barfs if you pass it ()
;;; for the second argument (program).
;;;###autoload
(defun make-comint (name program &optional startfile &rest switches)
  "Make a comint process NAME in a buffer, running PROGRAM.
The name of the buffer is made by surrounding NAME with `*'s.
PROGRAM should be either a string denoting an executable program to create
via `start-process', or a cons pair of the form (HOST . SERVICE) denoting a TCP
connection to be opened via `open-network-stream'.  If there is already a
running process in that buffer, it is not restarted.  Optional third arg
STARTFILE is the name of a file to send the contents of to the process.

If PROGRAM is a string, any more args are arguments to PROGRAM."
  (let ((buffer (get-buffer-create (concat "*" name "*"))))
    ;; If no process, or nuked process, crank up a new one and put buffer in
    ;; comint mode.  Otherwise, leave buffer and existing process alone.
    (cond ((not (comint-check-proc buffer))
	   (save-excursion
	     (set-buffer buffer)
	     (comint-mode)) ; Install local vars, mode, keymap, ...
	   (comint-exec buffer name program startfile switches)))
    buffer))

;;;###autoload
(defun comint-run (program)
  "Run PROGRAM in a comint buffer and switch to it.
The buffer name is made by surrounding the file name of PROGRAM with `*'s.
The file name is used to make a symbol name, such as `comint-sh-hook', and any
hooks on this symbol are run in the buffer.
See `make-comint' and `comint-exec'."
  (interactive "sRun program: ")
  (let ((name (file-name-nondirectory program)))
    (switch-to-buffer (make-comint name program))
    (run-hooks (intern-soft (concat "comint-" name "-hook")))))

(defun comint-exec (buffer name command startfile switches)
  "Start up a process in buffer BUFFER for comint modes.
Blasts any old process running in the buffer.  Doesn't set the buffer mode.
You can use this to cheaply run a series of processes in the same comint
buffer.  The hook `comint-exec-hook' is run after each exec."
  (save-excursion
    (set-buffer buffer)
    (let ((proc (get-buffer-process buffer)))	; Blast any old process.
      (if proc (delete-process proc)))
    ;; Crank up a new process
    (let ((proc
	   (if (consp command)
	       (open-network-stream name buffer (car command) (cdr command))
	     (comint-exec-1 name buffer command switches))))
      (set-process-filter proc 'comint-output-filter)
      (make-local-variable 'comint-ptyp)
      (setq comint-ptyp process-connection-type) ; T if pty, NIL if pipe.
      ;; Jump to the end, and set the process mark.
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
      ;; Feed it the startfile.
      (cond (startfile
	     ;;This is guaranteed to wait long enough
	     ;;but has bad results if the comint does not prompt at all
	     ;;	     (while (= size (buffer-size))
	     ;;	       (sleep-for 1))
	     ;;I hope 1 second is enough!
	     (sleep-for 1)
	     (goto-char (point-max))
	     (insert-file-contents startfile)
	     (setq startfile (buffer-substring (point) (point-max)))
	     (delete-region (point) (point-max))
	     (comint-send-string proc startfile)))
    (run-hooks 'comint-exec-hook)
    buffer)))

;;; This auxiliary function cranks up the process for comint-exec in
;;; the appropriate environment.

(defun comint-exec-1 (name buffer command switches)
  (let ((process-environment
	 (nconc
	  ;; If using termcap, we specify `emacs' as the terminal type
	  ;; because that lets us specify a width.
	  ;; If using terminfo, we specify `unknown' because that is
	  ;; a defined terminal type.  `emacs' is not a defined terminal type
	  ;; and there is no way for us to define it here.
	  ;; Some programs that use terminfo get very confused
	  ;; if TERM is not a valid terminal type.
	  (if (and (boundp 'system-uses-terminfo) system-uses-terminfo)
	      (list "EMACS=t" "TERM=unknown"
		    (format "COLUMNS=%d" (frame-width)))
	    (list "EMACS=t" "TERM=emacs"
		  (format "TERMCAP=emacs:co#%d:tc=unknown:" (frame-width))))
	  process-environment))
	(default-directory
	  (if (file-directory-p default-directory)
	      default-directory
	    "/")))
    (apply 'start-process name buffer command switches)))

;;; Input history processing in a buffer
;;; ===========================================================================
;;; Useful input history functions, courtesy of the Ergo group.

;;; Eleven commands:
;;; comint-dynamic-list-input-ring	List history in help buffer.
;;; comint-previous-input		Previous input...
;;; comint-previous-matching-input	...matching a string.
;;; comint-previous-matching-input-from-input ... matching the current input.
;;; comint-next-input			Next input...
;;; comint-next-matching-input		...matching a string.
;;; comint-next-matching-input-from-input     ... matching the current input.
;;; comint-backward-matching-input      Backwards input...
;;; comint-forward-matching-input       ...matching a string.
;;; comint-replace-by-expanded-history	Expand history at point;
;;;					replace with expanded history.
;;; comint-magic-space			Expand history and insert space.
;;;
;;; Three functions:
;;; comint-read-input-ring              Read into comint-input-ring...
;;; comint-write-input-ring             Write to comint-input-ring-file-name.
;;; comint-replace-by-expanded-history-before-point Workhorse function.

(defun comint-read-input-ring (&optional silent)
  "Sets the buffer's `comint-input-ring' from a history file.
The name of the file is given by the variable `comint-input-ring-file-name'.
The history ring is of size `comint-input-ring-size', regardless of file size.
If `comint-input-ring-file-name' is nil this function does nothing.

If the optional argument SILENT is non-nil, we say nothing about a
failure to read the history file.

This function is useful for major mode commands and mode hooks.

The structure of the history file should be one input command per line,
with the most recent command last.
See also `comint-input-ignoredups' and `comint-write-input-ring'."
  (cond ((or (null comint-input-ring-file-name)
	     (equal comint-input-ring-file-name ""))
	 nil)
	((not (file-readable-p comint-input-ring-file-name))
	 (or silent
	     (message "Cannot read history file %s"
		      comint-input-ring-file-name)))
	(t
	 (let ((history-buf (get-buffer-create " *temp*"))
	       (file comint-input-ring-file-name)
	       (count 0)
	       (ring (make-ring comint-input-ring-size)))
	   (unwind-protect
	       (save-excursion
		 (set-buffer history-buf)
		 (widen)
		 (erase-buffer)
		 (insert-file-contents file)
		 ;; Save restriction in case file is already visited...
		 ;; Watch for those date stamps in history files!
		 (goto-char (point-max))
		 (while (and (< count comint-input-ring-size)
			     (re-search-backward "^[ \t]*\\([^#\n].*\\)[ \t]*$"
						 nil t))
		   (let ((history (buffer-substring (match-beginning 1)
						    (match-end 1))))
		     (if (or (null comint-input-ignoredups)
			     (ring-empty-p ring)
			     (not (string-equal (ring-ref ring 0) history)))
			 (ring-insert-at-beginning ring history)))
		   (setq count (1+ count))))
	     (kill-buffer history-buf))
	   (setq comint-input-ring ring
		 comint-input-ring-index nil)))))

(defun comint-write-input-ring ()
  "Writes the buffer's `comint-input-ring' to a history file.
The name of the file is given by the variable `comint-input-ring-file-name'.
The original contents of the file are lost if `comint-input-ring' is not empty.
If `comint-input-ring-file-name' is nil this function does nothing.

Useful within process sentinels.

See also `comint-read-input-ring'."
  (cond ((or (null comint-input-ring-file-name)
	     (equal comint-input-ring-file-name "")
	     (null comint-input-ring) (ring-empty-p comint-input-ring))
	 nil)
	((not (file-writable-p comint-input-ring-file-name))
	 (message "Cannot write history file %s" comint-input-ring-file-name))
	(t
	 (let* ((history-buf (get-buffer-create " *Temp Input History*"))
		(ring comint-input-ring)
		(file comint-input-ring-file-name)
		(index (ring-length ring)))
	   ;; Write it all out into a buffer first.  Much faster, but messier,
	   ;; than writing it one line at a time.
	   (save-excursion
	     (set-buffer history-buf)
	     (erase-buffer)
	     (while (> index 0)
	       (setq index (1- index))
	       (insert (ring-ref ring index) ?\n))
	     (write-region (buffer-string) nil file nil 'no-message)
	     (kill-buffer nil))))))


(defun comint-dynamic-list-input-ring ()
  "List in help buffer the buffer's input history."
  (interactive)
  (if (or (not (ring-p comint-input-ring))
	  (ring-empty-p comint-input-ring))
      (message "No history")
    (let ((history nil)
	  (history-buffer " *Input History*")
	  (index (1- (ring-length comint-input-ring)))
	  (conf (current-window-configuration)))
      ;; We have to build up a list ourselves from the ring vector.
      (while (>= index 0)
	(setq history (cons (ring-ref comint-input-ring index) history)
	      index (1- index)))
      ;; Change "completion" to "history reference"
      ;; to make the display accurate.
      (with-output-to-temp-buffer history-buffer
	(display-completion-list history)
	(set-buffer history-buffer)
	(forward-line 3)
	(while (search-backward "completion" nil 'move)
	  (replace-match "history reference")))
      (sit-for 0)
      (message "Hit space to flush")
      (let ((ch (read-event)))
	(if (eq ch ?\ )
	    (set-window-configuration conf)
	  (setq unread-command-events (list ch)))))))


(defun comint-regexp-arg (prompt)
  ;; Return list of regexp and prefix arg using PROMPT.
  (let* ((minibuffer-history-sexp-flag nil)
	 ;; Don't clobber this.
	 (last-command last-command)
	 (regexp (read-from-minibuffer prompt nil nil nil
				       'minibuffer-history-search-history)))
    (list (if (string-equal regexp "")
	      (setcar minibuffer-history-search-history
		      (nth 1 minibuffer-history-search-history))
	    regexp)
	  (prefix-numeric-value current-prefix-arg))))

(defun comint-search-arg (arg)
  ;; First make sure there is a ring and that we are after the process mark
  (cond ((not (comint-after-pmark-p))
	 (error "Not at command line"))
	((or (null comint-input-ring)
	     (ring-empty-p comint-input-ring))
	 (error "Empty input ring"))
	((zerop arg)
	 ;; arg of zero resets search from beginning, and uses arg of 1
	 (setq comint-input-ring-index nil)
	 1)
	(t
	 arg)))

(defun comint-search-start (arg)
  ;; Index to start a directional search, starting at comint-input-ring-index
  (if comint-input-ring-index
      ;; If a search is running, offset by 1 in direction of arg
      (mod (+ comint-input-ring-index (if (> arg 0) 1 -1))
	   (ring-length comint-input-ring))
    ;; For a new search, start from beginning or end, as appropriate
    (if (>= arg 0)
	0				       ; First elt for forward search
      (1- (ring-length comint-input-ring)))))  ; Last elt for backward search

(defun comint-previous-input-string (arg)
  "Return the string ARG places along the input ring.
Moves relative to `comint-input-ring-index'."
  (ring-ref comint-input-ring (if comint-input-ring-index
				  (mod (+ arg comint-input-ring-index) 
				       (ring-length comint-input-ring))
				arg)))

(defun comint-previous-input (arg)
  "Cycle backwards through input history."
  (interactive "*p")
  (comint-previous-matching-input "." arg))

(defun comint-next-input (arg)
  "Cycle forwards through input history."
  (interactive "*p")
  (comint-previous-input (- arg)))

(defun comint-previous-matching-input-string (regexp arg)
  "Return the string matching REGEXP ARG places along the input ring.
Moves relative to `comint-input-ring-index'."
  (let* ((pos (comint-previous-matching-input-string-position regexp arg)))
    (if pos (ring-ref comint-input-ring pos))))

(defun comint-previous-matching-input-string-position (regexp arg &optional start)
  "Return the index matching REGEXP ARG places along the input ring.
Moves relative to START, or `comint-input-ring-index'."
  (if (or (not (ring-p comint-input-ring))
	  (ring-empty-p comint-input-ring))
      (error "No history"))
  (let* ((len (ring-length comint-input-ring))
	 (motion (if (> arg 0) 1 -1))
	 (n (mod (- (or start (comint-search-start arg)) motion) len))
	 (tried-each-ring-item nil)
	 (prev nil))
    ;; Do the whole search as many times as the argument says.
    (while (and (/= arg 0) (not tried-each-ring-item))
      ;; Step once.
      (setq prev n
	    n (mod (+ n motion) len))
      ;; If we haven't reached a match, step some more.
      (while (and (< n len) (not tried-each-ring-item)
		  (not (string-match regexp (ring-ref comint-input-ring n))))
	(setq n (mod (+ n motion) len)
	      ;; If we have gone all the way around in this search.
	      tried-each-ring-item (= n prev)))
      (setq arg (if (> arg 0) (1- arg) (1+ arg))))
    ;; Now that we know which ring element to use, if we found it, return that.
    (if (string-match regexp (ring-ref comint-input-ring n))
	n)))

(defun comint-previous-matching-input (regexp arg)
  "Search backwards through input history for match for REGEXP.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, find the next or Nth next match."
  (interactive (comint-regexp-arg "Previous input matching (regexp): "))
  (setq arg (comint-search-arg arg))
  (let ((pos (comint-previous-matching-input-string-position regexp arg)))
    ;; Has a match been found?
    (if (null pos)
	(error "Not found")
      (setq comint-input-ring-index pos)
      (message "History item: %d" (1+ pos))
      (delete-region 
       ;; Can't use kill-region as it sets this-command
       (process-mark (get-buffer-process (current-buffer))) (point))
      (insert (ring-ref comint-input-ring pos)))))

(defun comint-next-matching-input (regexp arg)
  "Search forwards through input history for match for REGEXP.
\(Later history elements are more recent commands.)
With prefix argument N, search for Nth following match.
If N is negative, find the previous or Nth previous match."
  (interactive (comint-regexp-arg "Next input matching (regexp): "))
  (comint-previous-matching-input regexp (- arg)))

(defun comint-previous-matching-input-from-input (arg)
  "Search backwards through input history for match for current input.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, search forwards for the -Nth following match."
  (interactive "p")
  (if (not (memq last-command '(comint-previous-matching-input-from-input
				comint-next-matching-input-from-input)))
      ;; Starting a new search
      (setq comint-matching-input-from-input-string
	    (buffer-substring 
	     (process-mark (get-buffer-process (current-buffer))) 
	     (point))
	    comint-input-ring-index nil))
  (comint-previous-matching-input
   (concat "^" (regexp-quote comint-matching-input-from-input-string))
   arg))

(defun comint-next-matching-input-from-input (arg)
  "Search forwards through input history for match for current input.
\(Following history elements are more recent commands.)
With prefix argument N, search for Nth following match.
If N is negative, search backwards for the -Nth previous match."
  (interactive "p")
  (comint-previous-matching-input-from-input (- arg)))


(defun comint-replace-by-expanded-history (&optional silent)
  "Expand input command history references before point.
Expansion is dependent on the value of `comint-input-autoexpand'.

This function depends on the buffer's idea of the input history, which may not
match the command interpreter's idea, assuming it has one.

Assumes history syntax is like typical Un*x shells'.  However, since emacs
cannot know the interpreter's idea of input line numbers, assuming it has one,
it cannot expand absolute input line number references.

If the optional argument SILENT is non-nil, never complain
even if history reference seems erroneous.

See `comint-magic-space' and `comint-replace-by-expanded-history-before-point'.

Returns t if successful."
  (interactive)
  (if (and comint-input-autoexpand
	   (string-match "[!^]" (funcall comint-get-old-input))
	   (save-excursion (beginning-of-line)
			   (looking-at comint-prompt-regexp)))
      ;; Looks like there might be history references in the command.
      (let ((previous-modified-tick (buffer-modified-tick)))
	(message "Expanding history references...")
	(comint-replace-by-expanded-history-before-point silent)
	(/= previous-modified-tick (buffer-modified-tick)))))


(defun comint-replace-by-expanded-history-before-point (silent)
  "Expand directory stack reference before point.
See `comint-replace-by-expanded-history'.  Returns t if successful."
  (save-excursion
    (let ((toend (- (save-excursion (end-of-line nil) (point)) (point)))
	  (start (progn (comint-bol nil) (point))))
      (while (progn
	       (skip-chars-forward "^!^"
				   (save-excursion
				     (end-of-line nil) (- (point) toend)))
	       (< (point)
		  (save-excursion
		    (end-of-line nil) (- (point) toend))))
	;; This seems a bit complex.  We look for references such as !!, !-num,
	;; !foo, !?foo, !{bar}, !?{bar}, ^oh, ^my^, ^god^it, ^never^ends^.
	;; If that wasn't enough, the plings can be suffixed with argument
	;; range specifiers.
	;; Argument ranges are complex too, so we hive off the input line,
	;; referenced with plings, with the range string to `comint-args'.
	(setq comint-input-ring-index nil)
	(cond ((or (= (preceding-char) ?\\)
		   (comint-within-quotes start (point)))
	       ;; The history is quoted, or we're in quotes.
	       (goto-char (1+ (point))))
	      ((looking-at "![0-9]+\\($\\|[^-]\\)")
	       ;; We cannot know the interpreter's idea of input line numbers.
	       (goto-char (match-end 0))
	       (message "Absolute reference cannot be expanded"))
	      ((looking-at "!-\\([0-9]+\\)\\(:?[0-9^$*-]+\\)?")
	       ;; Just a number of args from `number' lines backward.
	       (let ((number (1- (string-to-number
				  (buffer-substring (match-beginning 1)
						    (match-end 1))))))
		 (if (<= number (ring-length comint-input-ring))
		     (progn
		       (replace-match
			(comint-args (comint-previous-input-string number)
				     (match-beginning 2) (match-end 2))
			t t)
		       (setq comint-input-ring-index number)
		       (message "History item: %d" (1+ number)))
		   (goto-char (match-end 0))
		   (message "Relative reference exceeds input history size"))))
	      ((or (looking-at "!!?:?\\([0-9^$*-]+\\)") (looking-at "!!"))
	       ;; Just a number of args from the previous input line.
	       (replace-match
		(comint-args (comint-previous-input-string 0)
			     (match-beginning 1) (match-end 1))
		t t)
	       (message "History item: previous"))
	      ((looking-at
		"!\\??\\({\\(.+\\)}\\|\\(\\sw+\\)\\)\\(:?[0-9^$*-]+\\)?")
	       ;; Most recent input starting with or containing (possibly
	       ;; protected) string, maybe just a number of args.  Phew.
	       (let* ((mb1 (match-beginning 1)) (me1 (match-end 1))
		      (mb2 (match-beginning 2)) (me2 (match-end 2))
		      (exp (buffer-substring (or mb2 mb1) (or me2 me1)))
		      (pref (if (save-match-data (looking-at "!\\?")) "" "^"))
		      (pos (save-match-data
			     (comint-previous-matching-input-string-position
			      (concat pref (regexp-quote exp)) 1))))
		 (if (null pos)
		     (progn
		       (goto-char (match-end 0))
		       (or silent
			   (progn (message "Not found")
				  (ding))))
		   (setq comint-input-ring-index pos)
		   (replace-match
		    (comint-args (ring-ref comint-input-ring pos)
				 (match-beginning 4) (match-end 4))
		    t t)
		   (message "History item: %d" (1+ pos)))))
	      ((looking-at "\\^\\([^^]+\\)\\^?\\([^^]*\\)\\^?")
	       ;; Quick substitution on the previous input line.
	       (let ((old (buffer-substring (match-beginning 1) (match-end 1)))
		     (new (buffer-substring (match-beginning 2) (match-end 2)))
		     (pos nil))
		 (replace-match (comint-previous-input-string 0) t t)
		 (setq pos (point))
		 (goto-char (match-beginning 0))
		 (if (not (search-forward old pos t))
		     (or silent
			 (error "Not found"))
		   (replace-match new t t)
		   (message "History item: substituted"))))
	      (t
	       (goto-char (match-end 0))))))))


(defun comint-magic-space (arg)
  "Expand input history references before point and insert ARG spaces.
A useful command to bind to SPC.  See `comint-replace-by-expanded-history'."
  (interactive "p")
  (comint-replace-by-expanded-history)
  (self-insert-command arg))

(defun comint-within-quotes (beg end)
  "Return t if the number of quotes between BEG and END is odd.
Quotes are single and double."
  (let ((countsq (comint-how-many-region "\\(^\\|[^\\\\]\\)\'" beg end))
	(countdq (comint-how-many-region "\\(^\\|[^\\\\]\\)\"" beg end)))
    (or (= (mod countsq 2) 1) (= (mod countdq 2) 1))))

(defun comint-how-many-region (regexp beg end)
  "Return number of matches for REGEXP from BEG to END."
  (let ((count 0))
    (save-excursion
      (save-match-data
	(goto-char beg)
	(while (re-search-forward regexp end t)
	  (setq count (1+ count)))))
    count))

(defun comint-args (string begin end)
  ;; From STRING, return the args depending on the range specified in the text
  ;; from BEGIN to END.  If BEGIN is nil, assume all args.  Ignore leading `:'.
  ;; Range can be x-y, x-, -y, where x/y can be [0-9], *, ^, $.
  (save-match-data
    (if (null begin)
	(comint-arguments string 0 nil)
      (let* ((range (buffer-substring
		     (if (eq (char-after begin) ?:) (1+ begin) begin) end))
	     (nth (cond ((string-match "^[*^]" range) 1)
			((string-match "^-" range) 0)
			((string-equal range "$") nil)
			(t (string-to-number range))))
	     (mth (cond ((string-match "[-*$]$" range) nil)
			((string-match "-" range)
			 (string-to-number (substring range (match-end 0))))
			(t nth))))
	(comint-arguments string nth mth)))))

;; Return a list of arguments from ARG.  Break it up at the
;; delimiters in comint-delimiter-argument-list.  Returned list is backwards.
(defun comint-delim-arg (arg)
  (if (null comint-delimiter-argument-list)
      (list arg)
    (let ((args nil)
	  (pos 0)
	  (len (length arg)))
      (while (< pos len)
	(let ((char (aref arg pos))
	      (start pos))
	  (if (memq char comint-delimiter-argument-list)
	      (while (and (< pos len) (eq (aref arg pos) char))
		(setq pos (1+ pos)))
	    (while (and (< pos len)
			(not (memq (aref arg pos)
				   comint-delimiter-argument-list)))
	      (setq pos (1+ pos))))
	  (setq args (cons (substring arg start pos) args))))
      args)))

(defun comint-arguments (string nth mth)
  "Return from STRING the NTH to MTH arguments.
NTH and/or MTH can be nil, which means the last argument.
Returned arguments are separated by single spaces.
We assume whitespace separates arguments, except within quotes.
Also, a run of one or more of a single character
in `comint-delimiter-argument-list' is a separate argument.
Argument 0 is the command name."
  (let ((argpart "[^ \n\t\"'`]+\\|\\(\"[^\"]*\"\\|'[^']*'\\|`[^`]*`\\)")
	(args ()) (pos 0)
	(count 0)
	beg str value quotes)
    ;; Build a list of all the args until we have as many as we want.
    (while (and (or (null mth) (<= count mth))
		(string-match argpart string pos))
      (if (and beg (= pos (match-beginning 0)))
	  ;; It's contiguous, part of the same arg.
	  (setq pos (match-end 0)
		quotes (or quotes (match-beginning 1)))
	;; It's a new separate arg.
	(if beg
	    ;; Put the previous arg, if there was one, onto ARGS.
	    (setq str (substring string beg pos)
		  args (if quotes (cons str args)
			 (nconc (comint-delim-arg str) args))
		  count (1+ count)))
	(setq quotes (match-beginning 1))
	(setq beg (match-beginning 0))
	(setq pos (match-end 0))))
    (if beg
	(setq str (substring string beg pos)
	      args (if quotes (cons str args)
		     (nconc (comint-delim-arg str) args))
	      count (1+ count)))
    (let ((n (or nth (1- count)))
	  (m (if mth (1- (- count mth)) 0)))
      (mapconcat
       (function (lambda (a) a)) (nthcdr n (nreverse (nthcdr m args))) " "))))

;;;
;;; Input processing stuff
;;;

(defun comint-send-input () 
  "Send input to process.
After the process output mark, sends all text from the process mark to
point as input to the process.  Before the process output mark, calls value
of variable `comint-get-old-input' to retrieve old input, copies it to the
process mark, and sends it.  If variable `comint-process-echoes' is nil,
a terminal newline is also inserted into the buffer and sent to the process
\(if it is non-nil, all text from the process mark to point is deleted,
since it is assumed the remote process will re-echo it).

Any history reference may be expanded depending on the value of the variable
`comint-input-autoexpand'.  The list of function names contained in the value
of `comint-input-filter-functions' is called on the input before sending it.
The input is entered into the input history ring, if the value of variable
`comint-input-filter' returns non-nil when called on the input.

If variable `comint-eol-on-send' is non-nil, then point is moved to the
end of line before sending the input.

The values of `comint-get-old-input', `comint-input-filter-functions', and
`comint-input-filter' are chosen according to the command interpreter running
in the buffer.  E.g.,

If the interpreter is the csh,
    comint-get-old-input is the default: take the current line, discard any
        initial string matching regexp comint-prompt-regexp.
    comint-input-filter-functions monitors input for \"cd\", \"pushd\", and
        \"popd\" commands. When it sees one, it cd's the buffer.
    comint-input-filter is the default: returns t if the input isn't all white
	space.

If the comint is Lucid Common Lisp, 
    comint-get-old-input snarfs the sexp ending at point.
    comint-input-filter-functions does nothing.
    comint-input-filter returns nil if the input matches input-filter-regexp,
        which matches (1) all whitespace (2) :a, :c, etc.

Similarly for Soar, Scheme, etc."
  (interactive)
  ;; Note that the input string does not include its terminal newline.
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (error "Current buffer has no process")
	(let* ((pmark (process-mark proc))
	       (intxt (if (>= (point) (marker-position pmark))
			  (progn (if comint-eol-on-send (end-of-line))
				 (buffer-substring pmark (point)))
			(let ((copy (funcall comint-get-old-input)))
			  (goto-char pmark)
			  (insert copy)
			  copy)))
	       (input (if (not (eq comint-input-autoexpand 'input))
			  ;; Just whatever's already there
			  intxt
			;; Expand and leave it visible in buffer
			(comint-replace-by-expanded-history t)
			(buffer-substring pmark (point))))
	       (history (if (not (eq comint-input-autoexpand 'history))
			    input
			  ;; This is messy 'cos ultimately the original
			  ;; functions used do insertion, rather than return
			  ;; strings.  We have to expand, then insert back.
			  (comint-replace-by-expanded-history t)
			  (let ((copy (buffer-substring pmark (point))))
			    (delete-region pmark (point))
			    (insert input)
			    copy))))
          (if comint-process-echoes
              (delete-region pmark (point))
	    (insert ?\n))
	  (if (and (funcall comint-input-filter history)
		   (or (null comint-input-ignoredups)
		       (not (ring-p comint-input-ring))
		       (ring-empty-p comint-input-ring)
		       (not (string-equal (ring-ref comint-input-ring 0)
					  history))))
	      (ring-insert comint-input-ring history))
	  (let ((functions comint-input-filter-functions))
	    (while functions
	      (funcall (car functions) (concat input "\n"))
	      (setq functions (cdr functions))))
	  (setq comint-input-ring-index nil)
	  ;; Update the markers before we send the input
	  ;; in case we get output amidst sending the input.
	  (set-marker comint-last-input-start pmark)
	  (set-marker comint-last-input-end (point))
	  (set-marker (process-mark proc) (point))
	  (funcall comint-input-sender proc input)
	  (comint-output-filter proc "")))))

;; The purpose of using this filter for comint processes
;; is to keep comint-last-input-end from moving forward
;; when output is inserted.
(defun comint-output-filter (process string)
  ;; First check for killed buffer
  (let ((oprocbuf (process-buffer process)))
    (if (and oprocbuf (buffer-name oprocbuf))
	(let ((obuf (current-buffer))
	      (opoint nil) (obeg nil) (oend nil))
	  (set-buffer oprocbuf)
	  (setq opoint (point))
	  (setq obeg (point-min))
	  (setq oend (point-max))
	  (let ((buffer-read-only nil)
		(nchars (length string))
		(ostart nil))
	    (widen)
	    (goto-char (process-mark process))
	    (setq ostart (point))
	    (if (<= (point) opoint)
		(setq opoint (+ opoint nchars)))
	    ;; Insert after old_begv, but before old_zv.
	    (if (< (point) obeg)
		(setq obeg (+ obeg nchars)))
	    (if (<= (point) oend)
		(setq oend (+ oend nchars)))
	    (insert-before-markers string)
	    ;; Don't insert initial prompt outside the top of the window.
	    (if (= (window-start (selected-window)) (point))
		(set-window-start (selected-window) (- (point) (length string))))
	    (if (and comint-last-input-end
		     (marker-buffer comint-last-input-end)
		     (= (point) comint-last-input-end))
		(set-marker comint-last-input-end (- comint-last-input-end nchars)))
	    (set-marker comint-last-output-start ostart)
	    (set-marker (process-mark process) (point))
	    (force-mode-line-update))

	  (narrow-to-region obeg oend)
	  (goto-char opoint)
	  (let ((functions comint-output-filter-functions))
	    (while functions
	      (funcall (car functions) string)
	      (setq functions (cdr functions))))
	  (set-buffer obuf)))))

(defun comint-preinput-scroll-to-bottom ()
  "Go to the end of buffer in all windows showing it.
Movement occurs if point in the selected window is not after the process mark,
and `this-command' is an insertion command.  Insertion commands recognised
are `self-insert-command', `comint-magic-space', `yank', and `hilit-yank'.
Depends on the value of `comint-scroll-to-bottom-on-input'.

This function should be a pre-command hook."
  (if (and comint-scroll-to-bottom-on-input
	   (memq this-command '(self-insert-command comint-magic-space yank
				hilit-yank)))
      (let* ((selected (selected-window))
	     (current (current-buffer))
	     (process (get-buffer-process current))
	     (scroll comint-scroll-to-bottom-on-input))
	(if (and process (< (point) (process-mark process)))
	    (if (eq scroll 'this)
		(goto-char (point-max))
	      (walk-windows
	       (function (lambda (window)
		 (if (and (eq (window-buffer window) current)
			  (or (eq scroll t) (eq scroll 'all)))
		     (progn
		       (select-window window)
		       (goto-char (point-max))
		       (select-window selected)))))
	       nil t))))))

(defun comint-postoutput-scroll-to-bottom (string)
  "Go to the end of buffer in all windows showing it.
Does not scroll if the current line is the last line in the buffer.
Depends on the value of `comint-scroll-to-bottom-on-output' and
`comint-scroll-show-maximum-output'.

This function should be in the list `comint-output-filter-functions'."
  (let* ((selected (selected-window))
	 (current (current-buffer))
	 (process (get-buffer-process current))
	 (scroll comint-scroll-to-bottom-on-output))
    (unwind-protect
	(if process
	    (walk-windows
	     (function (lambda (window)
	       (if (eq (window-buffer window) current)
		   (progn
		     (select-window window)
		     (if (and (< (point) (process-mark process))
			      (or (eq scroll t) (eq scroll 'all)
				  ;; Maybe user wants point to jump to the end.
				  (and (eq scroll 'this) (eq selected window))
				  (and (eq scroll 'others) (not (eq selected window)))
				  ;; If point was at the end, keep it at the end.
				  (>= (point)
				      (- (process-mark process) (length string)))))
			 (goto-char (process-mark process)))
		     ;; Optionally scroll so that the text
		     ;; ends at the bottom of the window.
		     (if (and comint-scroll-show-maximum-output
			      (>= (point) (process-mark process)))
			 (save-excursion
			   (goto-char (point-max))
			   (recenter -1)))
		     (select-window selected)))))
	     nil t))
      (set-buffer current))))

(defun comint-strip-ctrl-m (&optional string)
  "Strip trailing `^M' characters from the current output group.

This function could be in the list `comint-output-filter-functions' or bound to
a key."
  (interactive)
  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char
       (if (interactive-p) comint-last-input-end comint-last-output-start))
      (while (re-search-forward "\r+$" pmark t)
	(replace-match "" t t)))))
(defalias 'shell-strip-ctrl-m 'comint-strip-ctrl-m)

(defun comint-show-maximum-output ()
  "Put the end of the buffer at the bottom of the window."
  (interactive)
  (goto-char (point-max))
  (recenter -1))

(defun comint-get-old-input-default ()
  "Default for `comint-get-old-input'.
Take the current line, and discard any initial text matching
`comint-prompt-regexp'."
  (save-excursion
    (beginning-of-line)
    (comint-skip-prompt)
    (let ((beg (point)))
      (end-of-line)
      (buffer-substring beg (point)))))

(defun comint-copy-old-input ()
  "Insert after prompt old input at point as new input to be edited.
Calls `comint-get-old-input' to get old input."
  (interactive)
  (let ((input (funcall comint-get-old-input))
 	(process (get-buffer-process (current-buffer))))
    (if (not process)
	(error "Current buffer has no process")
      (goto-char (process-mark process))
      (insert input))))

(defun comint-skip-prompt ()
  "Skip past the text matching regexp `comint-prompt-regexp'.
If this takes us past the end of the current line, don't skip at all."
  (let ((eol (save-excursion (end-of-line) (point))))
    (if (and (looking-at comint-prompt-regexp)
	     (<= (match-end 0) eol))
	(goto-char (match-end 0)))))

(defun comint-after-pmark-p ()
  "Return t if point is after the process output marker."
  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (<= (marker-position pmark) (point))))

(defun comint-simple-send (proc string)
  "Default function for sending to PROC input STRING.
This just sends STRING plus a newline. To override this,
set the hook `comint-input-sender'."
  (comint-send-string proc string)
  (comint-send-string proc "\n"))

(defun comint-bol (arg)
  "Goes to the beginning of line, then skips past the prompt, if any.
If prefix argument is given (\\[universal-argument]) the prompt is not skipped. 

The prompt skip is done by skipping text matching the regular expression
`comint-prompt-regexp', a buffer local variable."
  (interactive "P")
  (beginning-of-line)
  (if (null arg) (comint-skip-prompt)))

;;; These three functions are for entering text you don't want echoed or
;;; saved -- typically passwords to ftp, telnet, or somesuch.
;;; Just enter m-x send-invisible and type in your line, or add
;;; `comint-watch-for-password-prompt' to `comint-output-filter-functions'.

(defun comint-read-noecho (prompt &optional stars)
  "Read a single line of text from user without echoing, and return it. 
Prompt with argument PROMPT, a string.  Optional argument STARS causes
input to be echoed with '*' characters on the prompt line.  Input ends with
RET, LFD, or ESC.  DEL or C-h rubs out.  C-u kills line.  C-g aborts (if
`inhibit-quit' is set because e.g. this function was called from a process
filter and C-g is pressed, this function returns nil rather than a string).

Note that the keystrokes comprising the text can still be recovered
\(temporarily) with \\[view-lossage].  This may be a security bug for some
applications."
  (let ((ans "")
	(c 0)
	(echo-keystrokes 0)
	(cursor-in-echo-area t)
        (done nil))
    (while (not done)
      (if stars
          (message "%s%s" prompt (make-string (length ans) ?*))
        (message prompt))
      ;; Use this instead of `read-char' to avoid "Non-character input-event".
      (setq c (read-char-exclusive))
      (cond ((= c ?\C-g)
             ;; This function may get called from a process filter, where
             ;; inhibit-quit is set.  In later versions of emacs read-char
             ;; may clear quit-flag itself and return C-g.  That would make
             ;; it impossible to quit this loop in a simple way, so
             ;; re-enable it here (for backward-compatibility the check for
             ;; quit-flag below would still be necessary, so this seems
             ;; like the simplest way to do things).
             (setq quit-flag t
                   done t))
            ((or (= c ?\r) (= c ?\n) (= c ?\e))
             (setq done t))
            ((= c ?\C-u)
             (setq ans ""))
            ((and (/= c ?\b) (/= c ?\177))
             (setq ans (concat ans (char-to-string c))))
            ((> (length ans) 0)
             (setq ans (substring ans 0 -1)))))
    (if quit-flag
        ;; Emulate a true quit, except that we have to return a value.
        (prog1
            (setq quit-flag nil)
          (message "Quit")
          (beep t))
      (message "")
      ans)))

(defun send-invisible (str)
  "Read a string without echoing.
Then send it to the process running in the current buffer.  A new-line
is additionally sent.  String is not saved on comint input history list.
Security bug: your string can still be temporarily recovered with
\\[view-lossage]."
  (interactive "P") ; Defeat snooping via C-x esc
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc)
	(error "Current buffer has no process")
      (comint-send-string
       proc (if (stringp str) str (comint-read-noecho "Non-echoed text: " t)))
      (comint-send-string proc "\n"))))

(defun comint-watch-for-password-prompt (string) 
  "Prompt in the minibuffer for password and send without echoing.
This function uses `send-invisible' to read and send a password to the buffer's
process if STRING contains a password prompt defined by 
`comint-password-prompt-regexp'.

This function could be in the list `comint-output-filter-functions'."
  (if (string-match comint-password-prompt-regexp string)
      (send-invisible nil)))

;;; Low-level process communication

(defalias 'comint-send-string 'process-send-string)
(defalias 'comint-send-region 'process-send-region)

;;; Random input hackage

(defun comint-kill-output ()
  "Kill all output from interpreter since last input.
Does not delete the prompt."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
	(replacement nil))
    (save-excursion
      (let ((pmark (progn (goto-char (process-mark proc))
			  (beginning-of-line nil)
			  (point-marker))))
	(delete-region comint-last-input-end pmark)
	(comint-skip-prompt)
	(setq replacement (concat "*** output flushed ***\n"
				  (buffer-substring pmark (point))))
	(delete-region pmark (point))))
    ;; Output message and put back prompt
    (comint-output-filter proc replacement)))

(defun comint-show-output ()
  "Display start of this batch of interpreter output at top of window.
Sets mark to the value of point when this command is run."
  (interactive)
  (push-mark)
  (let ((pos (point)))
    (goto-char (or (marker-position comint-last-input-end) (point-max)))
    (beginning-of-line 0)
    (set-window-start (selected-window) (point))
    (comint-skip-prompt)))

(defun comint-interrupt-subjob ()
  "Interrupt the current subjob."
  (interactive)
  (interrupt-process nil comint-ptyp))

(defun comint-kill-subjob ()
  "Send kill signal to the current subjob."
  (interactive)
  (kill-process nil comint-ptyp))

(defun comint-quit-subjob ()
  "Send quit signal to the current subjob."
  (interactive)
  (quit-process nil comint-ptyp))

(defun comint-stop-subjob ()
  "Stop the current subjob.
WARNING: if there is no current subjob, you can end up suspending
the top-level process running in the buffer. If you accidentally do
this, use \\[comint-continue-subjob] to resume the process. (This
is not a problem with most shells, since they ignore this signal.)"
  (interactive)
  (stop-process nil comint-ptyp))

(defun comint-continue-subjob ()
  "Send CONT signal to process buffer's process group.
Useful if you accidentally suspend the top-level process."
  (interactive)
  (continue-process nil comint-ptyp))

(defun comint-kill-input ()
  "Kill all text from last stuff output by interpreter to point."
  (interactive)
  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (if (> (point) (marker-position pmark))
	(kill-region pmark (point)))))

(defun comint-delchar-or-maybe-eof (arg)
  "Delete ARG characters forward, or (if at eob) send an EOF to subprocess."
  (interactive "p")
  (if (eobp)
      (process-send-eof)
    (delete-char arg)))

(defun comint-send-eof ()
  "Send an EOF to the current buffer's process."
  (interactive)
  (process-send-eof))


(defun comint-backward-matching-input (regexp arg)
  "Search backward through buffer for match for REGEXP.
Matches are searched for on lines that match `comint-prompt-regexp'.
With prefix argument N, search for Nth previous match.
If N is negative, find the next or Nth next match."
  (interactive (comint-regexp-arg "Backward input matching (regexp): "))
  (let* ((re (concat comint-prompt-regexp ".*" regexp))
	 (pos (save-excursion (end-of-line (if (> arg 0) 0 1))
			      (if (re-search-backward re nil t arg)
				  (point)))))
    (if (null pos)
	(progn (message "Not found")
	       (ding))
      (goto-char pos)
      (comint-bol nil))))

(defun comint-forward-matching-input (regexp arg)
  "Search forward through buffer for match for REGEXP.
Matches are searched for on lines that match `comint-prompt-regexp'.
With prefix argument N, search for Nth following match.
If N is negative, find the previous or Nth previous match."
  (interactive (comint-regexp-arg "Forward input matching (regexp): "))
  (comint-backward-matching-input regexp (- arg)))


(defun comint-next-prompt (n)
  "Move to end of Nth next prompt in the buffer.
See `comint-prompt-regexp'."
  (interactive "p")
  (let ((paragraph-start comint-prompt-regexp))
    (end-of-line (if (> n 0) 1 0))
    (forward-paragraph n)
    (comint-skip-prompt)))

(defun comint-previous-prompt (n)
  "Move to end of Nth previous prompt in the buffer.
See `comint-prompt-regexp'."
  (interactive "p")
  (comint-next-prompt (- n)))

;;; Support for source-file processing commands.
;;;============================================================================
;;; Many command-interpreters (e.g., Lisp, Scheme, Soar) have
;;; commands that process files of source text (e.g. loading or compiling
;;; files). So the corresponding process-in-a-buffer modes have commands
;;; for doing this (e.g., lisp-load-file). The functions below are useful
;;; for defining these commands.
;;;
;;; Alas, these guys don't do exactly the right thing for Lisp, Scheme
;;; and Soar, in that they don't know anything about file extensions.
;;; So the compile/load interface gets the wrong default occasionally.
;;; The load-file/compile-file default mechanism could be smarter -- it
;;; doesn't know about the relationship between filename extensions and
;;; whether the file is source or executable. If you compile foo.lisp
;;; with compile-file, then the next load-file should use foo.bin for
;;; the default, not foo.lisp. This is tricky to do right, particularly
;;; because the extension for executable files varies so much (.o, .bin,
;;; .lbin, .mo, .vo, .ao, ...).


;;; COMINT-SOURCE-DEFAULT -- determines defaults for source-file processing
;;; commands.
;;;
;;; COMINT-CHECK-SOURCE -- if FNAME is in a modified buffer, asks you if you
;;; want to save the buffer before issuing any process requests to the command
;;; interpreter.
;;;
;;; COMINT-GET-SOURCE -- used by the source-file processing commands to prompt
;;; for the file to process.

;;; (COMINT-SOURCE-DEFAULT previous-dir/file source-modes)
;;;============================================================================
;;; This function computes the defaults for the load-file and compile-file
;;; commands for tea, soar, cmulisp, and cmuscheme modes. 
;;; 
;;; - PREVIOUS-DIR/FILE is a pair (directory . filename) from the last 
;;; source-file processing command. NIL if there hasn't been one yet.
;;; - SOURCE-MODES is a list used to determine what buffers contain source
;;; files: if the major mode of the buffer is in SOURCE-MODES, it's source.
;;; Typically, (lisp-mode) or (scheme-mode).
;;; 
;;; If the command is given while the cursor is inside a string, *and*
;;; the string is an existing filename, *and* the filename is not a directory,
;;; then the string is taken as default. This allows you to just position
;;; your cursor over a string that's a filename and have it taken as default.
;;;
;;; If the command is given in a file buffer whose major mode is in
;;; SOURCE-MODES, then the the filename is the default file, and the
;;; file's directory is the default directory.
;;; 
;;; If the buffer isn't a source file buffer (e.g., it's the process buffer),
;;; then the default directory & file are what was used in the last source-file
;;; processing command (i.e., PREVIOUS-DIR/FILE).  If this is the first time
;;; the command has been run (PREVIOUS-DIR/FILE is nil), the default directory
;;; is the cwd, with no default file. (\"no default file\" = nil)
;;; 
;;; SOURCE-REGEXP is typically going to be something like (tea-mode)
;;; for T programs, (lisp-mode) for Lisp programs, (soar-mode lisp-mode)
;;; for Soar programs, etc.
;;; 
;;; The function returns a pair: (default-directory . default-file).

(defun comint-source-default (previous-dir/file source-modes)
  (cond ((and buffer-file-name (memq major-mode source-modes))
	 (cons (file-name-directory    buffer-file-name)
	       (file-name-nondirectory buffer-file-name)))
	(previous-dir/file)
	(t
	 (cons default-directory nil))))


;;; (COMINT-CHECK-SOURCE fname)
;;;============================================================================
;;; Prior to loading or compiling (or otherwise processing) a file (in the CMU
;;; process-in-a-buffer modes), this function can be called on the filename.
;;; If the file is loaded into a buffer, and the buffer is modified, the user
;;; is queried to see if he wants to save the buffer before proceeding with
;;; the load or compile.

(defun comint-check-source (fname)
  (let ((buff (get-file-buffer fname)))
    (if (and buff
	     (buffer-modified-p buff)
	     (y-or-n-p (format "Save buffer %s first? " (buffer-name buff))))
	;; save BUFF.
	(let ((old-buffer (current-buffer)))
	  (set-buffer buff)
	  (save-buffer)
	  (set-buffer old-buffer)))))


;;; (COMINT-GET-SOURCE prompt prev-dir/file source-modes mustmatch-p)
;;;============================================================================
;;; COMINT-GET-SOURCE is used to prompt for filenames in command-interpreter
;;; commands that process source files (like loading or compiling a file).
;;; It prompts for the filename, provides a default, if there is one,
;;; and returns the result filename.
;;; 
;;; See COMINT-SOURCE-DEFAULT for more on determining defaults.
;;; 
;;; PROMPT is the prompt string. PREV-DIR/FILE is the (directory . file) pair
;;; from the last source processing command.  SOURCE-MODES is a list of major
;;; modes used to determine what file buffers contain source files.  (These
;;; two arguments are used for determining defaults). If MUSTMATCH-P is true,
;;; then the filename reader will only accept a file that exists.
;;; 
;;; A typical use:
;;; (interactive (comint-get-source "Compile file: " prev-lisp-dir/file
;;;                                 '(lisp-mode) t))

;;; This is pretty stupid about strings. It decides we're in a string
;;; if there's a quote on both sides of point on the current line.
(defun comint-extract-string ()
  "Return string around POINT that starts the current line, or nil." 
  (save-excursion
    (let* ((point (point))
	   (bol (progn (beginning-of-line) (point)))
	   (eol (progn (end-of-line) (point)))
	   (start (progn (goto-char point) 
			 (and (search-backward "\"" bol t) 
			      (1+ (point)))))
	   (end (progn (goto-char point)
		       (and (search-forward "\"" eol t)
			    (1- (point))))))
      (and start end
	   (buffer-substring start end)))))

(defun comint-get-source (prompt prev-dir/file source-modes mustmatch-p)
  (let* ((def (comint-source-default prev-dir/file source-modes))
         (stringfile (comint-extract-string))
	 (sfile-p (and stringfile
		       (condition-case ()
			   (file-exists-p stringfile)
			 (error nil))
		       (not (file-directory-p stringfile))))
	 (defdir  (if sfile-p (file-name-directory stringfile)
                      (car def)))
	 (deffile (if sfile-p (file-name-nondirectory stringfile)
                      (cdr def)))
	 (ans (read-file-name (if deffile (format "%s(default %s) "
						  prompt    deffile)
				  prompt)
			      defdir
			      (concat defdir deffile)
			      mustmatch-p)))
    (list (expand-file-name (substitute-in-file-name ans)))))

;;; I am somewhat divided on this string-default feature. It seems
;;; to violate the principle-of-least-astonishment, in that it makes
;;; the default harder to predict, so you actually have to look and see
;;; what the default really is before choosing it. This can trip you up.
;;; On the other hand, it can be useful, I guess. I would appreciate feedback
;;; on this.
;;;     -Olin


;;; Simple process query facility.
;;; ===========================================================================
;;; This function is for commands that want to send a query to the process
;;; and show the response to the user. For example, a command to get the
;;; arglist for a Common Lisp function might send a "(arglist 'foo)" query
;;; to an inferior Common Lisp process.
;;; 
;;; This simple facility just sends strings to the inferior process and pops
;;; up a window for the process buffer so you can see what the process
;;; responds with.  We don't do anything fancy like try to intercept what the
;;; process responds with and put it in a pop-up window or on the message
;;; line. We just display the buffer. Low tech. Simple. Works good.

;;; Send to the inferior process PROC the string STR. Pop-up but do not select
;;; a window for the inferior process so that its response can be seen.
(defun comint-proc-query (proc str)
  (let* ((proc-buf (process-buffer proc))
	 (proc-mark (process-mark proc)))
    (display-buffer proc-buf)
    (set-buffer proc-buf) ; but it's not the selected *window*
    (let ((proc-win (get-buffer-window proc-buf))
	  (proc-pt (marker-position proc-mark)))
      (comint-send-string proc str) ; send the query
      (accept-process-output proc)  ; wait for some output
      ;; Try to position the proc window so you can see the answer.
      ;; This is bogus code. If you delete the (sit-for 0), it breaks.
      ;; I don't know why. Wizards invited to improve it.
      (if (not (pos-visible-in-window-p proc-pt proc-win))
	  (let ((opoint (window-point proc-win)))
	    (set-window-point proc-win proc-mark)
	    (sit-for 0)
	    (if (not (pos-visible-in-window-p opoint proc-win))
		(push-mark opoint)
	      (set-window-point proc-win opoint)))))))


;;; Filename/command/history completion in a buffer
;;; ===========================================================================
;;; Useful completion functions, courtesy of the Ergo group.

;;; Six commands:
;;; comint-dynamic-complete		Complete or expand command, filename,
;;;                                     history at point.
;;; comint-dynamic-complete-filename	Complete filename at point.
;;; comint-dynamic-list-filename-completions List completions in help buffer.
;;; comint-replace-by-expanded-filename	Expand and complete filename at point;
;;;					replace with expanded/completed name.
;;; comint-dynamic-simple-complete	Complete stub given candidates.

;;; These are not installed in the comint-mode keymap. But they are
;;; available for people who want them. Shell-mode installs them:
;;; (define-key shell-mode-map "\t" 'comint-dynamic-complete)
;;; (define-key shell-mode-map "\M-?"
;;;             'comint-dynamic-list-filename-completions)))
;;;
;;; Commands like this are fine things to put in load hooks if you
;;; want them present in specific modes.

(defvar comint-completion-autolist nil
  "*If non-nil, automatically list possiblities on partial completion.
This mirrors the optional behavior of tcsh.")

(defvar comint-completion-addsuffix t
  "*If non-nil, add a `/' to completed directories, ` ' to file names.
This mirrors the optional behavior of tcsh.")

(defvar comint-completion-recexact nil
  "*If non-nil, use shortest completion if characters cannot be added.
This mirrors the optional behavior of tcsh.

A non-nil value is useful if `comint-completion-autolist' is non-nil too.")

(defvar comint-completion-fignore nil
  "*List of suffixes to be disregarded during file completion.
This mirrors the optional behavior of bash and tcsh.

Note that this applies to `comint-dynamic-complete-filename' only.")

(defvar comint-file-name-prefix ""
  "Prefix prepended to absolute file names taken from process input.
This is used by comint's and shell's completion functions, and by shell's
directory tracking functions.")


(defun comint-directory (directory)
  ;; Return expanded DIRECTORY, with `comint-file-name-prefix' if absolute.
  (expand-file-name (if (file-name-absolute-p directory)
			(concat comint-file-name-prefix directory)
		      directory)))


(defun comint-word (word-chars)
  "Return the word of WORD-CHARS at point, or nil if non is found.
Word constituents are considered to be those in WORD-CHARS, which is like the
inside of a \"[...]\" (see `skip-chars-forward')."
  (save-excursion
    (let ((limit (point))
	  (word (concat "[" word-chars "]"))
	  (non-word (concat "[^" word-chars "]")))
      (if (re-search-backward non-word nil 'move)
	  (forward-char 1))
      ;; Anchor the search forwards.
      (if (or (eolp) (looking-at non-word))
	  nil
	(re-search-forward (concat word "+") limit)
	(buffer-substring (match-beginning 0) (match-end 0))))))


(defun comint-match-partial-filename ()
  "Return the filename at point, or nil if non is found.
Environment variables are substituted.  See `comint-word'."
  (let ((filename (comint-word "~/A-Za-z0-9+@:_.$#%,={}-")))
    (and filename (substitute-in-file-name filename))))


(defun comint-dynamic-complete ()
  "Dynamically perform completion at point.
Calls the functions in `comint-dynamic-complete-functions' to perform
completion until a function returns non-nil, at which point completion is
assumed to have occurred."
  (interactive)
  (let ((functions comint-dynamic-complete-functions))
    (while (and functions (null (funcall (car functions))))
      (setq functions (cdr functions)))))


(defun comint-dynamic-complete-filename ()
  "Dynamically complete the filename at point.
Completes if after a filename.  See `comint-match-partial-filename' and
`comint-dynamic-complete-as-filename'.
This function is similar to `comint-replace-by-expanded-filename', except that
it won't change parts of the filename already entered in the buffer; it just
adds completion characters to the end of the filename.  A completions listing
may be shown in a help buffer if completion is ambiguous.

Completion is dependent on the value of `comint-completion-addsuffix',
`comint-completion-recexact' and `comint-completion-fignore', and the timing of
completions listing is dependent on the value of `comint-completion-autolist'.

Returns t if successful."
  (interactive)
  (if (comint-match-partial-filename)
      (prog2 (or (eq (selected-window) (minibuffer-window))
		 (message "Completing file name..."))
	  (comint-dynamic-complete-as-filename))))


(defun comint-dynamic-complete-as-filename ()
  "Dynamically complete at point as a filename.
See `comint-dynamic-complete-filename'.  Returns t if successful."
  (let* ((completion-ignore-case nil)
	 (completion-ignored-extensions comint-completion-fignore)
	 (success t)
	 (filename (or (comint-match-partial-filename) ""))
	 (pathdir (file-name-directory filename))
	 (pathnondir (file-name-nondirectory filename))
	 (directory (if pathdir (comint-directory pathdir) default-directory))
	 (completion (file-name-completion pathnondir directory))
	 (mini-flag (eq (selected-window) (minibuffer-window))))
    (cond ((null completion)
           (message "No completions of %s" filename)
	   (setq success nil))
          ((eq completion t)            ; Means already completed "file".
           (if comint-completion-addsuffix (insert " "))
           (or mini-flag (message "Sole completion")))
          ((string-equal completion "") ; Means completion on "directory/".
           (comint-dynamic-list-filename-completions))
          (t                            ; Completion string returned.
           (let ((file (concat (file-name-as-directory directory) completion)))
             (insert (substring (directory-file-name completion)
                                (length pathnondir)))
             (cond ((symbolp (file-name-completion completion directory))
                    ;; We inserted a unique completion.
                    (if comint-completion-addsuffix
                        (insert (if (file-directory-p file) "/" " ")))
                    (or mini-flag (message "Completed")))
                   ((and comint-completion-recexact comint-completion-addsuffix
                         (string-equal pathnondir completion)
                         (file-exists-p file))
                    ;; It's not unique, but user wants shortest match.
                    (insert (if (file-directory-p file) "/" " "))
                    (or mini-flag (message "Completed shortest")))
                   ((or comint-completion-autolist
                        (string-equal pathnondir completion))
                    ;; It's not unique, list possible completions.
                    (comint-dynamic-list-filename-completions))
                   (t
                    (or mini-flag (message "Partially completed")))))))
    success))


(defun comint-replace-by-expanded-filename ()
  "Dynamically expand and complete the filename at point.
Replace the filename with an expanded, canonicalised and completed replacement.
\"Expanded\" means environment variables (e.g., $HOME) and `~'s are replaced
with the corresponding directories.  \"Canonicalised\" means `..'  and `.' are
removed, and the filename is made absolute instead of relative.  For expansion
see `expand-file-name' and `substitute-in-file-name'.  For completion see
`comint-dynamic-complete-filename'."
  (interactive)
  (replace-match (expand-file-name (comint-match-partial-filename)) t t)
  (comint-dynamic-complete-filename))


(defun comint-dynamic-simple-complete (stub candidates)
  "Dynamically complete STUB from CANDIDATES list.
This function inserts completion characters at point by completing STUB from
the strings in CANDIDATES.  A completions listing may be shown in a help buffer
if completion is ambiguous.

Returns nil if no completion was inserted.
Returns `sole' if completed with the only completion match.
Returns `shortest' if completed with the shortest of the completion matches.
Returns `partial' if completed as far as possible with the completion matches.
Returns `listed' if a completion listing was shown.

See also `comint-dynamic-complete-filename'."
  (let* ((completion-ignore-case nil)
	 (candidates (mapcar (function (lambda (x) (list x))) candidates))
	 (completions (all-completions stub candidates)))
    (cond ((null completions)
 	   (message "No completions of %s" stub)
	   nil)
 	  ((= 1 (length completions))	; Gotcha!
 	   (let ((completion (car completions)))
 	     (if (string-equal completion stub)
 		 (message "Sole completion")
 	       (insert (substring completion (length stub)))
 	       (message "Completed"))
	     (if comint-completion-addsuffix (insert " "))
	     'sole))
 	  (t				; There's no unique completion.
 	   (let ((completion (try-completion stub candidates)))
 	     ;; Insert the longest substring.
 	     (insert (substring completion (length stub)))
 	     (cond ((and comint-completion-recexact comint-completion-addsuffix
 			 (string-equal stub completion)
 			 (member completion completions))
 		    ;; It's not unique, but user wants shortest match.
 		    (insert " ")
 		    (message "Completed shortest")
		    'shortest)
 		   ((or comint-completion-autolist
 			(string-equal stub completion))
 		    ;; It's not unique, list possible completions.
 		    (comint-dynamic-list-completions completions)
		    'listed)
 		   (t
		    (message "Partially completed")
		    'partial)))))))


(defun comint-dynamic-list-filename-completions ()
  "List in help buffer possible completions of the filename at point."
  (interactive)
  (let* ((completion-ignore-case nil)
	 (filename (or (comint-match-partial-filename) ""))
	 (pathdir (file-name-directory filename))
	 (pathnondir (file-name-nondirectory filename))
	 (directory (if pathdir (comint-directory pathdir) default-directory))
	 (completions (file-name-all-completions pathnondir directory)))
    (if completions
	(comint-dynamic-list-completions completions)
      (message "No completions of %s" filename))))


(defun comint-dynamic-list-completions (completions)
  "List in help buffer sorted COMPLETIONS.
Typing SPC flushes the help buffer."
  (let ((conf (current-window-configuration)))
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list (sort completions 'string-lessp)))
    (message "Hit space to flush")
    (let (key first)
      (if (save-excursion
	    (set-buffer (get-buffer "*Completions*"))
	    (setq key (read-key-sequence nil)
		  first (aref key 0))
	    (and (consp first) (consp (event-start first))
		 (eq (window-buffer (posn-window (event-start first)))
		     (get-buffer "*Completions*"))
		 (eq (key-binding key) 'mouse-choose-completion)))
	  ;; If the user does mouse-choose-completion with the mouse,
	  ;; execute the command, then delete the completion window.
	  (progn
	    (mouse-choose-completion first)
	    (set-window-configuration conf))
	(if (eq first ?\ )
	    (set-window-configuration conf)
	  (setq unread-command-events (listify-key-sequence key)))))))

;;; Converting process modes to use comint mode
;;; ===========================================================================
;;; The code in the Emacs 19 distribution has all been modified to use comint
;;; where needed.  However, there are `third-party' packages out there that
;;; still use the old shell mode.  Here's a guide to conversion.
;;;
;;; Renaming variables
;;; Most of the work is renaming variables and functions. These are the common
;;; ones:
;;; Local variables:
;;;	last-input-start	comint-last-input-start
;;; 	last-input-end		comint-last-input-end
;;;	shell-prompt-pattern	comint-prompt-regexp
;;;     shell-set-directory-error-hook <no equivalent>
;;; Miscellaneous:
;;;	shell-set-directory	<unnecessary>
;;; 	shell-mode-map		comint-mode-map
;;; Commands:
;;;	shell-send-input	comint-send-input
;;;	shell-send-eof		comint-delchar-or-maybe-eof
;;; 	kill-shell-input	comint-kill-input
;;;	interrupt-shell-subjob	comint-interrupt-subjob
;;;	stop-shell-subjob	comint-stop-subjob
;;;	quit-shell-subjob	comint-quit-subjob
;;;	kill-shell-subjob	comint-kill-subjob
;;;	kill-output-from-shell	comint-kill-output
;;;	show-output-from-shell	comint-show-output
;;;	copy-last-shell-input	Use comint-previous-input/comint-next-input
;;;
;;; SHELL-SET-DIRECTORY is gone, its functionality taken over by
;;; SHELL-DIRECTORY-TRACKER, the shell mode's comint-input-filter-functions.
;;; Comint mode does not provide functionality equivalent to
;;; shell-set-directory-error-hook; it is gone.
;;;
;;; comint-last-input-start is provided for modes which want to munge
;;; the buffer after input is sent, perhaps because the inferior
;;; insists on echoing the input.  The LAST-INPUT-START variable in
;;; the old shell package was used to implement a history mechanism,
;;; but you should think twice before using comint-last-input-start
;;; for this; the input history ring often does the job better.
;;; 
;;; If you are implementing some process-in-a-buffer mode, called foo-mode, do
;;; *not* create the comint-mode local variables in your foo-mode function.
;;; This is not modular.  Instead, call comint-mode, and let *it* create the
;;; necessary comint-specific local variables. Then create the
;;; foo-mode-specific local variables in foo-mode.  Set the buffer's keymap to
;;; be foo-mode-map, and its mode to be foo-mode.  Set the comint-mode hooks
;;; (comint-{prompt-regexp, input-filter, input-filter-functions,
;;; get-old-input) that need to be different from the defaults.  Call
;;; foo-mode-hook, and you're done. Don't run the comint-mode hook yourself;
;;; comint-mode will take care of it. The following example, from shell.el,
;;; is typical:
;;; 
;;; (defvar shell-mode-map '())
;;; (cond ((not shell-mode-map)
;;;        (setq shell-mode-map (copy-keymap comint-mode-map))
;;;        (define-key shell-mode-map "\C-c\C-f" 'shell-forward-command)
;;;        (define-key shell-mode-map "\C-c\C-b" 'shell-backward-command)
;;;        (define-key shell-mode-map "\t" 'comint-dynamic-complete)
;;;        (define-key shell-mode-map "\M-?"
;;;          'comint-dynamic-list-filename-completions)))
;;;
;;; (defun shell-mode ()
;;;   (interactive)
;;;   (comint-mode)
;;;   (setq comint-prompt-regexp shell-prompt-pattern)
;;;   (setq major-mode 'shell-mode)
;;;   (setq mode-name "Shell")
;;;   (use-local-map shell-mode-map)
;;;   (make-local-variable 'shell-directory-stack)
;;;   (setq shell-directory-stack nil)
;;;   (add-hook 'comint-input-filter-functions 'shell-directory-tracker)
;;;   (run-hooks 'shell-mode-hook))
;;;
;;;
;;; Note that make-comint is different from make-shell in that it
;;; doesn't have a default program argument. If you give make-shell
;;; a program name of NIL, it cleverly chooses one of explicit-shell-name,
;;; $ESHELL, $SHELL, or /bin/sh. If you give make-comint a program argument
;;; of NIL, it barfs. Adjust your code accordingly...
;;;
;;; Completion for comint-mode users
;;; 
;;; For modes that use comint-mode, comint-dynamic-complete-functions is the
;;; hook to add completion functions to.  Functions on this list should return
;;; non-nil if completion occurs (i.e., further completion should not occur).
;;; You could use comint-dynamic-simple-complete to do the bulk of the
;;; completion job.

(provide 'comint)

;;; comint.el ends here
