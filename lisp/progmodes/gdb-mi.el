;;; gdb-mi.el --- User Interface for running GDB

;; Author: Nick Roberts <nickrob@gnu.org>,
;; Maintainer: FSF
;; Keywords: unix, tools

;; Copyright (C) 2007, 2008, 2009 Free Software Foundation, Inc.

; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Credits:

;; This file was written by by Nick Roberts following the general design
;; used in gdb-ui.el for Emacs 22.1 - 23.1.  It is currently being developed
;; by Dmitry Dzhus <dima@sphinx.net.ru> as part of the Google Summer
;; of Code 2009 Project "Emacs GDB/MI migration".

;;; Commentary:

;; This mode acts as a graphical user interface to GDB.  You can interact with
;; GDB through the GUD buffer in the usual way, but there are also further
;; buffers which control the execution and describe the state of your program.
;; It separates the input/output of your program from that of GDB and displays
;; expressions and their current values in their own buffers.  It also uses
;; features of Emacs 21 such as the fringe/display margin for breakpoints, and
;; the toolbar (see the GDB Graphical Interface section in the Emacs info
;; manual).

;; M-x gdb will start the debugger.

;; This file uses GDB/MI as the primary interface to GDB.  It is still under
;; development and is part of a process to migrate Emacs from annotations (as
;; used in gdb-ui.el) to GDB/MI.  It runs gdb with GDB/MI (-interp=mi) and
;; access CLI using "-interpreter-exec console cli-command".  This code works
;; without gdb-ui.el and uses MI tokens instead of queues. Eventually MI
;; should be asynchronous.

;; This mode will PARTLY WORK WITH RECENT GDB RELEASES (status in modeline
;; doesn't update properly when execution commands are issued from GUD buffer)
;; and WORKS BEST when GDB runs asynchronously: maint set linux-async on.
;;
;; You need development version of GDB 7.0 for the thread buffer to work.

;; This file replaces gdb-ui.el and is for development with GDB.  Use the
;; release branch of Emacs 22 for the latest version of gdb-ui.el.

;; Windows Platforms:

;; If you are using Emacs and GDB on Windows you will need to flush the buffer
;; explicitly in your program if you want timely display of I/O in Emacs.
;; Alternatively you can make the output stream unbuffered, for example, by
;; using a macro:

;;           #ifdef UNBUFFERED
;;	     setvbuf (stdout, (char *) NULL, _IONBF, 0);
;;	     #endif

;; and compiling with -DUNBUFFERED while debugging.

;; If you are using Cygwin GDB and find that the source is not being displayed
;; in Emacs when you step through it, possible solutions are to:

;;   1) Use Cygwin X Windows and Cygwin Emacs.
;;        (Since 22.1 Emacs builds under Cygwin.)
;;   2) Use MinGW GDB instead.
;;   3) Use cygwin-mount.el

;;; Mac OSX:

;; GDB in Emacs on Mac OSX works best with FSF GDB as Apple have made
;; some changes to the version that they include as part of Mac OSX.
;; This requires GDB version 7.0 or later (estimated release date Aug 2009)
;; as earlier versions don not compile on Mac OSX.

;;; Known Bugs:

;; 1) Stack buffer doesn't parse MI output if you stop in a routine without
;;    line information, e.g., a routine in libc (just a TODO item).

;; TODO:
;; 1) Use MI command -data-read-memory for memory window.
;; 2) Watch windows to work with threads.
;; 3) Use treebuffer.el instead of the speedbar for watch-expressions?
;; 4) Mark breakpoint locations on scroll-bar of source buffer?

;;; Code:

(require 'gud)
(require 'json)
(require 'fadr)

(defvar tool-bar-map)
(defvar speedbar-initial-expansion-list-name)

(defvar gdb-pc-address nil "Initialization for Assembler buffer.
Set to \"main\" at start if `gdb-show-main' is t.")
(defvar gdb-selected-frame nil)
(defvar gdb-selected-file nil)
(defvar gdb-selected-line nil)
(defvar gdb-frame-number nil)
(defvar gdb-current-language nil)
(defvar gdb-var-list nil
  "List of variables in watch window.
Each element has the form (VARNUM EXPRESSION NUMCHILD TYPE VALUE STATUS) where
STATUS is nil (unchanged), `changed' or `out-of-scope'.")
(defvar gdb-main-file nil "Source file from which program execution begins.")
(defvar gdb-overlay-arrow-position nil)
(defvar gdb-stack-position nil)
(defvar gdb-location-alist nil
  "Alist of breakpoint numbers and full filenames.  Only used for files that
Emacs can't find.")
(defvar gdb-active-process nil
  "GUD tooltips display variable values when t, and macro definitions otherwise.")
(defvar gdb-error "Non-nil when GDB is reporting an error.")
(defvar gdb-macro-info nil
  "Non-nil if GDB knows that the inferior includes preprocessor macro info.")
(defvar gdb-register-names nil "List of register names.")
(defvar gdb-changed-registers nil
  "List of changed register numbers (strings).")
(defvar gdb-buffer-fringe-width nil)
(defvar gdb-last-command nil)
(defvar gdb-prompt-name nil)
(defvar gdb-token-number 0)
(defvar gdb-handler-alist '())
(defvar gdb-handler-number nil)
(defvar gdb-source-file-list nil
  "List of source files for the current executable.")
(defvar gdb-first-done-or-error t)
(defvar gdb-source-window nil)
(defvar gdb-inferior-status nil)
(defvar gdb-continuation nil)
(defvar gdb-filter-output nil
  "Message to be shown in GUD console.

This variable is updated in `gdb-done-or-error' and returned by
`gud-gdbmi-marker-filter'.")

(defvar gdb-buffer-type nil
  "One of the symbols bound in `gdb-buffer-rules'.")
(make-variable-buffer-local 'gdb-buffer-type)

(defvar gdb-output-sink 'nil
  "The disposition of the output of the current gdb command.
Possible values are these symbols:

    `user' -- gdb output should be copied to the GUD buffer
              for the user to see.

    `emacs' -- output should be collected in the partial-output-buffer
	       for subsequent processing by a command.  This is the
	       disposition of output generated by commands that
	       gdb mode sends to gdb on its own behalf.")

(defvar gdb-pending-triggers '()
  "A list of trigger functions that have run later than their output handlers.")

(defcustom gdb-debug-log-max 128
  "Maximum size of `gdb-debug-log'.  If nil, size is unlimited."
  :group 'gdb
  :type '(choice (integer :tag "Number of elements")
		 (const   :tag "Unlimited" nil))
  :version "22.1")

(defvar gdb-debug-log nil
  "List of commands sent to and replies received from GDB.
Most recent commands are listed first.  This list stores only the last
`gdb-debug-log-max' values.  This variable is used to debug GDB-MI.")

;;;###autoload
(defcustom gdb-enable-debug nil
  "Non-nil means record the process input and output in `gdb-debug-log'."
  :type 'boolean
  :group 'gdb
  :version "22.1")

(defcustom gdb-cpp-define-alist-program "gcc -E -dM -"
  "Shell command for generating a list of defined macros in a source file.
This list is used to display the #define directive associated
with an identifier as a tooltip.  It works in a debug session with
GDB, when `gud-tooltip-mode' is t.

Set `gdb-cpp-define-alist-flags' for any include paths or
predefined macros."
  :type 'string
  :group 'gdb
  :version "22.1")

(defcustom gdb-cpp-define-alist-flags ""
  "Preprocessor flags for `gdb-cpp-define-alist-program'."
  :type 'string
  :group 'gdb
  :version "22.1")

 (defcustom gdb-create-source-file-list t
   "Non-nil means create a list of files from which the executable was built.
 Set this to nil if the GUD buffer displays \"initializing...\" in the mode
 line for a long time when starting, possibly because your executable was
 built from a large number of files.  This allows quicker initialization
 but means that these files are not automatically enabled for debugging,
 e.g., you won't be able to click in the fringe to set a breakpoint until
 execution has already stopped there."
   :type 'boolean
   :group 'gdb
   :version "23.1")

(defcustom gdb-show-main nil
  "Non-nil means display source file containing the main routine at startup.
Also display the main routine in the disassembly buffer if present."
  :type 'boolean
  :group 'gdb
  :version "22.1")

; Note: This mode requires a separate buffer for inferior IO.
(defconst gdb-use-separate-io-buffer t)

(defun gdb-force-mode-line-update (status)
  (let ((buffer gud-comint-buffer))
    (if (and buffer (buffer-name buffer))
	(with-current-buffer buffer
	  (setq mode-line-process
		(format ":%s [%s]"
			(process-status (get-buffer-process buffer)) status))
	  ;; Force mode line redisplay soon.
	  (force-mode-line-update)))))

(defun gdb-enable-debug (arg)
  "Toggle logging of transaction between Emacs and Gdb.
The log is stored in `gdb-debug-log' as an alist with elements
whose cons is send, send-item or recv and whose cdr is the string
being transferred.  This list may grow up to a size of
`gdb-debug-log-max' after which the oldest element (at the end of
the list) is deleted every time a new one is added (at the front)."
  (interactive "P")
  (setq gdb-enable-debug
	(if (null arg)
	    (not gdb-enable-debug)
	  (> (prefix-numeric-value arg) 0)))
  (message (format "Logging of transaction %sabled"
		   (if gdb-enable-debug "en" "dis"))))

(defun gdb-find-watch-expression ()
  (let* ((var (nth (- (line-number-at-pos (point)) 2) gdb-var-list))
	 (varnum (car var)) expr array)
    (string-match "\\(var[0-9]+\\)\\.\\(.*\\)" varnum)
    (let ((var1 (assoc (match-string 1 varnum) gdb-var-list)) var2 varnumlet
	  (component-list (split-string (match-string 2 varnum) "\\." t)))
      (setq expr (nth 1 var1))
      (setq varnumlet (car var1))
      (dolist (component component-list)
	(setq var2 (assoc varnumlet gdb-var-list))
	(setq expr (concat expr
			   (if (string-match ".*\\[[0-9]+\\]$" (nth 3 var2))
			       (concat "[" component "]")
			     (concat "." component))))
	(setq varnumlet (concat varnumlet "." component)))
      expr)))

(defvar gdb-locals-font-lock-keywords
  '(
    ;; var = type value
    ( "\\(^\\(\\sw\\|[_.]\\)+\\)\t+\\(\\(\\sw\\|[_.]\\)+\\)"
      (1 font-lock-variable-name-face)
      (3 font-lock-type-face))
    )
  "Font lock keywords used in `gdb-local-mode'.")

;;;###autoload
(defun gdb (command-line)
  "Run gdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

If `gdb-many-windows' is nil (the default value) then gdb just
pops up the GUD buffer unless `gdb-show-main' is t.  In this case
it starts with two windows: one displaying the GUD buffer and the
other with the source file with the main routine of the inferior.

If `gdb-many-windows' is t, regardless of the value of
`gdb-show-main', the layout below will appear unless
`gdb-use-separate-io-buffer' is nil when the source buffer
occupies the full width of the frame.  Keybindings are shown in
some of the buffers.

Watch expressions appear in the speedbar/slowbar.

The following commands help control operation :

`gdb-many-windows'    - Toggle the number of windows gdb uses.
`gdb-restore-windows' - To restore the window layout.

See Info node `(emacs)GDB Graphical Interface' for a more
detailed description of this mode.


+----------------------------------------------------------------------+
|                               GDB Toolbar                            |
+-----------------------------------+----------------------------------+
| GUD buffer (I/O of GDB)           | Locals buffer                    |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
+-----------------------------------+----------------------------------+
| Source buffer                     | I/O buffer (of debugged program) |
|                                   | (comint-mode)                    |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
+-----------------------------------+----------------------------------+
| Stack buffer                      | Breakpoints buffer               |
| RET      gdb-frames-select        | SPC    gdb-toggle-breakpoint     |
|                                   | RET    gdb-goto-breakpoint       |
|                                   | D      gdb-delete-breakpoint     |
+-----------------------------------+----------------------------------+"
  ;;
  (interactive (list (gud-query-cmdline 'gdb)))

  (when (and gud-comint-buffer
	   (buffer-name gud-comint-buffer)
	   (get-buffer-process gud-comint-buffer)
	   (with-current-buffer gud-comint-buffer (eq gud-minor-mode 'gdba)))
	(gdb-restore-windows)
	(error
	 "Multiple debugging requires restarting in text command mode"))
  ;;
  (gud-common-init command-line nil 'gud-gdbmi-marker-filter)
  (set (make-local-variable 'gud-minor-mode) 'gdbmi)
  (setq comint-input-sender 'gdb-send)

  (gud-def gud-tbreak "tbreak %f:%l" "\C-t"
	   "Set temporary breakpoint at current line.")
  (gud-def gud-jump
	   (progn (gud-call "tbreak %f:%l") (gud-call "jump %f:%l"))
	   "\C-j" "Set execution address to current line.")

  (gud-def gud-up     "up %p"     "<" "Up N stack frames (numeric arg).")
  (gud-def gud-down   "down %p"   ">" "Down N stack frames (numeric arg).")
  (gud-def gud-print  "print %e"  "\C-p" "Evaluate C expression at point.")
  (gud-def gud-pstar  "print* %e" nil
	   "Evaluate C dereferenced pointer expression at point.")

  (gud-def gud-step   "-exec-step %p"              "\C-s"
	   "Step one source line with display.")
  (gud-def gud-stepi  "-exec-step-instruction %p"  "\C-i"
	   "Step one instruction with display.")
  (gud-def gud-next   "-exec-next %p"              "\C-n"
	   "Step one line (skip functions).")
  (gud-def gud-nexti  "nexti %p" nil
	   "Step one instruction (skip functions).")
  (gud-def gud-cont   "-exec-continue"             "\C-r"
	   "Continue with display.")
  (gud-def gud-finish "-exec-finish"               "\C-f"
	   "Finish executing current function.")
  (gud-def gud-run    "-exec-run"	     nil    "Runn the program.")

  (local-set-key "\C-i" 'gud-gdb-complete-command)
  (setq gdb-first-prompt t)
  (setq gud-running nil)
  (gdb-update)
  (run-hooks 'gdb-mode-hook))

(defun gdb-init-1 ()
  (gud-def gud-break (if (not (string-equal mode-name "Machine"))
			 (gud-call "break %f:%l" arg)
		       (save-excursion
			 (beginning-of-line)
			 (forward-char 2)
			 (gud-call "break *%a" arg)))
	   "\C-b" "Set breakpoint at current line or address.")
  ;;
  (gud-def gud-remove (if (not (string-equal mode-name "Machine"))
			  (gud-call "clear %f:%l" arg)
			(save-excursion
			  (beginning-of-line)
			  (forward-char 2)
			  (gud-call "clear *%a" arg)))
	   "\C-d" "Remove breakpoint at current line or address.")
  ;;
  (gud-def gud-until  (if (not (string-equal mode-name "Machine"))
			  (gud-call "-exec-until %f:%l" arg)
			(save-excursion
			  (beginning-of-line)
			  (forward-char 2)
			  (gud-call "-exec-until *%a" arg)))
	   "\C-u" "Continue to current line or address.")
  ;;
  (gud-def
   gud-go (gud-call (if gdb-active-process "-exec-continue" "-exec-run") arg)
   nil "Start or continue execution.")

  ;; For debugging Emacs only.
  (gud-def gud-pp
	   (gud-call
	    (concat
	     "pp1 " (if (eq (buffer-local-value
			     'major-mode (window-buffer)) 'speedbar-mode)
			(gdb-find-watch-expression) "%e")) arg)
	   nil   "Print the Emacs s-expression.")

  (define-key gud-minor-mode-map [left-margin mouse-1]
    'gdb-mouse-set-clear-breakpoint)
  (define-key gud-minor-mode-map [left-fringe mouse-1]
    'gdb-mouse-set-clear-breakpoint)
   (define-key gud-minor-mode-map [left-margin C-mouse-1]
    'gdb-mouse-toggle-breakpoint-margin)
  (define-key gud-minor-mode-map [left-fringe C-mouse-1]
    'gdb-mouse-toggle-breakpoint-fringe)

  (define-key gud-minor-mode-map [left-margin drag-mouse-1]
    'gdb-mouse-until)
  (define-key gud-minor-mode-map [left-fringe drag-mouse-1]
    'gdb-mouse-until)
  (define-key gud-minor-mode-map [left-margin mouse-3]
    'gdb-mouse-until)
  (define-key gud-minor-mode-map [left-fringe mouse-3]
    'gdb-mouse-until)

  (define-key gud-minor-mode-map [left-margin C-drag-mouse-1]
    'gdb-mouse-jump)
  (define-key gud-minor-mode-map [left-fringe C-drag-mouse-1]
    'gdb-mouse-jump)
  (define-key gud-minor-mode-map [left-fringe C-mouse-3]
    'gdb-mouse-jump)
  (define-key gud-minor-mode-map [left-margin C-mouse-3]
    'gdb-mouse-jump)
  ;;
  ;; (re-)initialise
  (setq gdb-pc-address (if gdb-show-main "main" nil))
  (setq gdb-selected-frame nil
	gdb-frame-number nil
	gdb-var-list nil
	gdb-pending-triggers nil
	gdb-output-sink 'user
	gdb-location-alist nil
	gdb-source-file-list nil
	gdb-last-command nil
	gdb-token-number 0
	gdb-handler-alist '()
	gdb-handler-number nil
	gdb-prompt-name nil
	gdb-first-done-or-error t
	gdb-buffer-fringe-width (car (window-fringes))
	gdb-debug-log nil
	gdb-source-window nil
	gdb-inferior-status nil
	gdb-continuation nil)
  ;;
  (setq gdb-buffer-type 'gdbmi)
  ;;
  (gdb-force-mode-line-update
   (propertize "initializing..." 'face font-lock-variable-name-face))

  (when gdb-use-separate-io-buffer
    (gdb-get-buffer-create 'gdb-inferior-io)
    (gdb-clear-inferior-io)
    (set-process-filter (get-process "gdb-inferior") 'gdb-inferior-filter)
    (gdb-input
     ;; Needs GDB 6.4 onwards
     (list (concat "-inferior-tty-set "
		   (process-tty-name (get-process "gdb-inferior")) "\n")
	   'ignore)))
  (if (eq window-system 'w32)
      (gdb-input (list "-gdb-set new-console off\n" 'ignore)))
  (gdb-input (list "-gdb-set height 0\n" 'ignore))
  ;; find source file and compilation directory here
  (gdb-input
   ; Needs GDB 6.2 onwards.
   (list "-file-list-exec-source-files\n" 'gdb-get-source-file-list))
  (if gdb-create-source-file-list
      (gdb-input
        ; Needs GDB 6.0 onwards.
       (list "-file-list-exec-source-file\n" 'gdb-get-source-file)))
  (gdb-input
   (list "-data-list-register-names\n" 'gdb-get-register-names))
  (gdb-input
   (list "-gdb-show prompt\n" 'gdb-get-prompt)))

(defvar gdb-define-alist nil "Alist of #define directives for GUD tooltips.")

(defun gdb-create-define-alist ()
  "Create an alist of #define directives for GUD tooltips."
  (let* ((file (buffer-file-name))
	 (output
	  (with-output-to-string
	    (with-current-buffer standard-output
 	      (and file
		   (file-exists-p file)
 		   ;; call-process doesn't work with remote file names.
		   (not (file-remote-p default-directory))
 		   (call-process shell-file-name file
				 (list t nil) nil "-c"
				 (concat gdb-cpp-define-alist-program " "
					 gdb-cpp-define-alist-flags))))))
	(define-list (split-string output "\n" t))
	(name))
    (setq gdb-define-alist nil)
    (dolist (define define-list)
      (setq name (nth 1 (split-string define "[( ]")))
      (push (cons name define) gdb-define-alist))))

(declare-function tooltip-show "tooltip" (text &optional use-echo-area))
(defvar tooltip-use-echo-area)

(defun gdb-tooltip-print (expr)
  (tooltip-show
   (with-current-buffer (gdb-get-buffer 'gdb-partial-output-buffer)
     (goto-char (point-min))
     (let ((string
	    (if (search-forward "=" nil t)
		(concat expr (buffer-substring (- (point) 2) (point-max)))
	      (buffer-string))))
       ;; remove newline for gud-tooltip-echo-area
       (substring string 0 (- (length string) 1))))
   (or gud-tooltip-echo-area tooltip-use-echo-area
       (not (display-graphic-p)))))
 
;; If expr is a macro for a function don't print because of possible dangerous
;; side-effects. Also printing a function within a tooltip generates an
;; unexpected starting annotation (phase error).
(defun gdb-tooltip-print-1 (expr)
  (with-current-buffer (gdb-get-buffer 'gdb-partial-output-buffer)
    (goto-char (point-min))
    (if (search-forward "expands to: " nil t)
	(unless (looking-at "\\S-+.*(.*).*")
	  (gdb-input
	   (list  (concat "print " expr "\n")
		  `(lambda () (gdb-tooltip-print ,expr))))))))

(defun gdb-init-buffer ()
  (set (make-local-variable 'gud-minor-mode) 'gdbmi)
  (set (make-local-variable 'tool-bar-map) gud-tool-bar-map)
  (when gud-tooltip-mode
    (make-local-variable 'gdb-define-alist)
    (gdb-create-define-alist)
    (add-hook 'after-save-hook 'gdb-create-define-alist nil t)))

(defmacro gdb-if-arrow (arrow-position &rest body)
  `(if ,arrow-position
      (let ((buffer (marker-buffer ,arrow-position)) (line))
	(if (equal buffer (window-buffer (posn-window end)))
	    (with-current-buffer buffer
	      (when (or (equal start end)
			(equal (posn-point start)
			       (marker-position ,arrow-position)))
		,@body))))))

(defun gdb-mouse-until (event)
  "Continue running until a source line past the current line.
The destination source line can be selected either by clicking
with mouse-3 on the fringe/margin or dragging the arrow
with mouse-1 (default bindings)."
  (interactive "e")
  (let ((start (event-start event))
	(end (event-end event)))
    (gdb-if-arrow gud-overlay-arrow-position
		  (setq line (line-number-at-pos (posn-point end)))
		  (gud-call (concat "until " (number-to-string line))))
    (gdb-if-arrow gdb-overlay-arrow-position
		  (save-excursion
		    (goto-line (line-number-at-pos (posn-point end)))
		    (forward-char 2)
		    (gud-call (concat "until *%a"))))))

(defun gdb-mouse-jump (event)
  "Set execution address/line.
The destination source line can be selected either by clicking with C-mouse-3
on the fringe/margin or dragging the arrow with C-mouse-1 (default bindings).
Unlike `gdb-mouse-until' the destination address can be before the current
line, and no execution takes place."
  (interactive "e")
  (let ((start (event-start event))
	(end (event-end event)))
    (gdb-if-arrow gud-overlay-arrow-position
		  (setq line (line-number-at-pos (posn-point end)))
		  (progn
		    (gud-call (concat "tbreak " (number-to-string line)))
		    (gud-call (concat "jump " (number-to-string line)))))
    (gdb-if-arrow gdb-overlay-arrow-position
		  (save-excursion
		    (goto-line (line-number-at-pos (posn-point end)))
		    (forward-char 2)
		    (progn
		      (gud-call (concat "tbreak *%a"))
		      (gud-call (concat "jump *%a")))))))

(defcustom gdb-show-changed-values t
  "If non-nil change the face of out of scope variables and changed values.
Out of scope variables are suppressed with `shadow' face.
Changed values are highlighted with the face `font-lock-warning-face'."
  :type 'boolean
  :group 'gdb
  :version "22.1")

(defcustom gdb-max-children 40
  "Maximum number of children before expansion requires confirmation."
  :type 'integer
  :group 'gdb
  :version "22.1")

(defcustom gdb-delete-out-of-scope t
  "If non-nil delete watch expressions automatically when they go out of scope."
  :type 'boolean
  :group 'gdb
  :version "22.2")

(defcustom gdb-speedbar-auto-raise nil
  "If non-nil raise speedbar every time display of watch expressions is\
 updated."
  :type 'boolean
  :group 'gdb
  :version "22.1")

(defcustom gdb-use-colon-colon-notation nil
  "If non-nil use FUN::VAR format to display variables in the speedbar."
  :type 'boolean
  :group 'gdb
  :version "22.1")

(defun gdb-speedbar-auto-raise (arg)
  "Toggle automatic raising of the speedbar for watch expressions.
With prefix argument ARG, automatically raise speedbar if ARG is
positive, otherwise don't automatically raise it."
  (interactive "P")
  (setq gdb-speedbar-auto-raise
	(if (null arg)
	    (not gdb-speedbar-auto-raise)
	  (> (prefix-numeric-value arg) 0)))
  (message (format "Auto raising %sabled"
		   (if gdb-speedbar-auto-raise "en" "dis"))))

(define-key gud-minor-mode-map "\C-c\C-w" 'gud-watch)
(define-key global-map (concat gud-key-prefix "\C-w") 'gud-watch)

(declare-function tooltip-identifier-from-point "tooltip" (point))

(defun gud-watch (&optional arg event)
  "Watch expression at point.
With arg, enter name of variable to be watched in the minibuffer."
  (interactive (list current-prefix-arg last-input-event))
  (let ((minor-mode (buffer-local-value 'gud-minor-mode gud-comint-buffer)))
    (if (eq minor-mode 'gdbmi)
	(progn
	  (if event (posn-set-point (event-end event)))
	  (require 'tooltip)
	  (save-selected-window
	    (let ((expr
		   (if arg
		       (completing-read "Name of variable: "
					'gud-gdb-complete-command)
		     (if (and transient-mark-mode mark-active)
			 (buffer-substring (region-beginning) (region-end))
		       (concat (if (eq major-mode 'gdb-registers-mode) "$")
			       (tooltip-identifier-from-point (point)))))))
	      (set-text-properties 0 (length expr) nil expr)
	      (gdb-input
	       (list (concat"-var-create - * "  expr "\n")
		     `(lambda () (gdb-var-create-handler ,expr)))))))
      (message "gud-watch is a no-op in this mode."))))

(defconst gdb-var-create-regexp
  "name=\"\\(.*?\\)\",.*numchild=\"\\(.*?\\)\",\\(?:.*value=\\(\".*\"\\),\\)?.*type=\"\\(.*?\\)\"")

(defun gdb-var-create-handler (expr)
  (goto-char (point-min))
  (if (re-search-forward gdb-var-create-regexp nil t)
      (let ((var (list
		  (match-string 1)
		  (if (and (string-equal gdb-current-language "c")
			   gdb-use-colon-colon-notation gdb-selected-frame)
		      (setq expr (concat gdb-selected-frame "::" expr))
		    expr)
		  (match-string 2)
		  (match-string 4)
		  (if (match-string 3) (read (match-string 3)))
		  nil)))
	(push var gdb-var-list)
	(speedbar 1)
	(unless (string-equal
		 speedbar-initial-expansion-list-name "GUD")
	  (speedbar-change-initial-expansion-list "GUD"))
	(gdb-input
	 (list
	  (concat "-var-evaluate-expression " (car var) "\n")
	  `(lambda () (gdb-var-evaluate-expression-handler
		       ,(car var) nil)))))
    (message-box "No symbol \"%s\" in current context." expr)))

(defun gdb-speedbar-update ()
  (when (and (boundp 'speedbar-frame) (frame-live-p speedbar-frame)
	     (not (member 'gdb-speedbar-timer gdb-pending-triggers)))
    ;; Dummy command to update speedbar even when idle.
    (gdb-input (list "-environment-pwd\n" 'gdb-speedbar-timer-fn))
    ;; Keep gdb-pending-triggers non-nil till end.
    (push 'gdb-speedbar-timer gdb-pending-triggers)))

(defun gdb-speedbar-timer-fn ()
  (if gdb-speedbar-auto-raise
      (raise-frame speedbar-frame))
  (setq gdb-pending-triggers
	(delq 'gdb-speedbar-timer gdb-pending-triggers))
  (speedbar-timer-fn))

(defun gdb-var-evaluate-expression-handler (varnum changed)
  (goto-char (point-min))
  (re-search-forward ".*value=\\(\".*\"\\)" nil t)
  (let ((var (assoc varnum gdb-var-list)))
    (when var
      (if changed (setcar (nthcdr 5 var) 'changed))
      (setcar (nthcdr 4 var) (read (match-string 1)))))
  (gdb-speedbar-update))

; Uses "-var-list-children --all-values".  Needs GDB 6.1 onwards.
(defun gdb-var-list-children (varnum)
  (gdb-input
   (list (concat "-var-update " varnum "\n") 'ignore))
  (gdb-input
   (list (concat "-var-list-children --all-values "
		varnum "\n")
	     `(lambda () (gdb-var-list-children-handler ,varnum)))))

(defconst gdb-var-list-children-regexp
  "child={.*?name=\"\\(.+?\\)\".*?,exp=\"\\(.+?\\)\".*?,\
numchild=\"\\(.+?\\)\".*?,value=\\(\".*?\"\\).*?,type=\"\\(.+?\\)\".*?}")

(defun gdb-var-list-children-handler (varnum)
  (goto-char (point-min))
  (let ((var-list nil))
    (catch 'child-already-watched
      (dolist (var gdb-var-list)
	(if (string-equal varnum (car var))
	    (progn
	      (push var var-list)
	      (while (re-search-forward gdb-var-list-children-regexp nil t)
		(let ((varchild (list (match-string 1)
				      (match-string 2)
				      (match-string 3)
				      (match-string 5)
				      (read (match-string 4))
				      nil)))
		  (if (assoc (car varchild) gdb-var-list)
		      (throw 'child-already-watched nil))
		  (push varchild var-list))))
	  (push var var-list)))
      (setq gdb-var-list (nreverse var-list))))
  (gdb-speedbar-update))

(defun gdb-var-set-format (format)
  "Set the output format for a variable displayed in the speedbar."
  (let* ((var (nth (- (count-lines (point-min) (point)) 2) gdb-var-list))
	 (varnum (car var)))
    (gdb-input
     (list (concat "-var-set-format " varnum " " format "\n") 'ignore))
    (gdb-var-update)))

(defun gdb-var-delete-1 (varnum)
  (gdb-input
   (list (concat "-var-delete " varnum "\n") 'ignore))
  (setq gdb-var-list (delq var gdb-var-list))
  (dolist (varchild gdb-var-list)
    (if (string-match (concat (car var) "\\.") (car varchild))
	(setq gdb-var-list (delq varchild gdb-var-list)))))

(defun gdb-var-delete ()
  "Delete watch expression at point from the speedbar."
  (interactive)
  (let ((text (speedbar-line-text)))
    (string-match "\\(\\S-+\\)" text)
       (let* ((var (nth (- (count-lines (point-min) (point)) 2) gdb-var-list))
	      (varnum (car var)))
	 (if (string-match "\\." (car var))
	     (message-box "Can only delete a root expression")
	   (gdb-var-delete-1 varnum)))))

(defun gdb-var-delete-children (varnum)
  "Delete children of variable object at point from the speedbar."
  (gdb-input
   (list (concat "-var-delete -c " varnum "\n") 'ignore)))

(defun gdb-edit-value (text token indent)
  "Assign a value to a variable displayed in the speedbar."
  (let* ((var (nth (- (count-lines (point-min) (point)) 2) gdb-var-list))
	 (varnum (car var)) (value))
    (setq value (read-string "New value: "))
    (gdb-input
     (list (concat "-var-assign " varnum " " value "\n")
	   `(lambda () (gdb-edit-value-handler ,value))))))

(defconst gdb-error-regexp "\\^error,msg=\\(\".+\"\\)")

(defun gdb-edit-value-handler (value)
  (goto-char (point-min))
  (if (re-search-forward gdb-error-regexp nil t)
      (message-box "Invalid number or expression (%s)" value)))

; Uses "-var-update --all-values".  Needs GDB 6.4 onwards.
(defun gdb-var-update ()
  (if (not (member 'gdb-var-update gdb-pending-triggers))
      (gdb-input
       (list "-var-update --all-values *\n" 'gdb-var-update-handler)))
  (push 'gdb-var-update gdb-pending-triggers))

(defconst gdb-var-update-regexp
  "{.*?name=\"\\(.*?\\)\".*?,\\(?:value=\\(\".*?\"\\),\\)?.*?\
in_scope=\"\\(.*?\\)\".*?}")

(defun gdb-var-update-handler ()
  (dolist (var gdb-var-list)
    (setcar (nthcdr 5 var) nil))
  (goto-char (point-min))
  (while (re-search-forward gdb-var-update-regexp nil t)
    (let* ((varnum (match-string 1))
	   (var (assoc varnum gdb-var-list)))
      (when var
	(let ((match (match-string 3)))
	  (cond ((string-equal match "false")
		 (if gdb-delete-out-of-scope
		     (gdb-var-delete-1 varnum)
		   (setcar (nthcdr 5 var) 'out-of-scope)))
		((string-equal match "true")
		 (setcar (nthcdr 5 var) 'changed)
		 (setcar (nthcdr 4 var)
			 (read (match-string 2))))
		((string-equal match "invalid")
		 (gdb-var-delete-1 varnum)))))))
  (setq gdb-pending-triggers
   (delq 'gdb-var-update gdb-pending-triggers))
  (gdb-speedbar-update))

(defun gdb-speedbar-expand-node (text token indent)
  "Expand the node the user clicked on.
TEXT is the text of the button we clicked on, a + or - item.
TOKEN is data related to this node.
INDENT is the current indentation depth."
  (cond ((string-match "+" text)        ;expand this node
	 (let* ((var (assoc token gdb-var-list))
		(expr (nth 1 var)) (children (nth 2 var)))
	   (if (or (<= (string-to-number children) gdb-max-children)
		   (y-or-n-p
		    (format "%s has %s children. Continue? " expr children)))
	       (gdb-var-list-children token))))
	((string-match "-" text)	;contract this node
	 (dolist (var gdb-var-list)
	   (if (string-match (concat token "\\.") (car var))
	       (setq gdb-var-list (delq var gdb-var-list))))
	 (gdb-var-delete-children token)
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun gdb-get-target-string ()
  (with-current-buffer gud-comint-buffer
    gud-target-name))


;;
;; gdb buffers.
;;
;; Each buffer has a TYPE -- a symbol that identifies the function
;; of that particular buffer.
;;
;; The usual gdb interaction buffer is given the type `gdbmi' and
;; is constructed specially.
;;
;; Others are constructed by gdb-get-buffer-create and
;; named according to the rules set forth in the gdb-buffer-rules-assoc

(defvar gdb-buffer-rules-assoc '())

(defun gdb-get-buffer (key)
  "Return the gdb buffer tagged with type KEY.
The key should be one of the cars in `gdb-buffer-rules-assoc'."
  (save-excursion
    (gdb-look-for-tagged-buffer key (buffer-list))))

(defun gdb-get-buffer-create (key)
  "Create a new gdb buffer of the type specified by KEY.
The key should be one of the cars in `gdb-buffer-rules-assoc'."
  (or (gdb-get-buffer key)
      (let* ((rules (assoc key gdb-buffer-rules-assoc))
	     (name (funcall (gdb-rules-name-maker rules)))
	     (new (get-buffer-create name)))
	(with-current-buffer new
	  (let ((trigger))
	    (if (cdr (cdr rules))
		(setq trigger (funcall (car (cdr (cdr rules))))))
	    (setq gdb-buffer-type key)
	    (set (make-local-variable 'gud-minor-mode)
		 (buffer-local-value 'gud-minor-mode gud-comint-buffer))
	    (set (make-local-variable 'tool-bar-map) gud-tool-bar-map)
	    (if trigger (funcall trigger)))
	  new))))

(defun gdb-rules-name-maker (rules) (car (cdr rules)))

(defun gdb-look-for-tagged-buffer (key bufs)
  (let ((retval nil))
    (while (and (not retval) bufs)
      (set-buffer (car bufs))
      (if (eq gdb-buffer-type key)
	  (setq retval (car bufs)))
      (setq bufs (cdr bufs)))
    retval))

;; Used to define all gdb-frame-*-buffer functions except
;; `gdb-frame-separate-io-buffer'
(defmacro gdb-def-frame-for-buffer (name buffer &optional doc)
  "Define a function NAME which shows gdb BUFFER in a separate frame.

DOC is an optional documentation string."
  `(defun ,name ()
     ,(when doc doc)
     (interactive)
     (let ((special-display-regexps (append special-display-regexps '(".*")))
           (special-display-frame-alist gdb-frame-parameters))
       (display-buffer (gdb-get-buffer-create ,buffer)))))

(defmacro gdb-def-display-buffer (name buffer &optional doc)
  "Define a function NAME which shows gdb BUFFER.

DOC is an optional documentation string."
  `(defun ,name ()
     (interactive)
     (gdb-display-buffer
      (gdb-get-buffer-create ,name) t)))

;;
;; This assoc maps buffer type symbols to rules.  Each rule is a list of
;; at least one and possible more functions.  The functions have these
;; roles in defining a buffer type:
;;
;;     NAME - Return a name for this  buffer type.
;;
;; The remaining function(s) are optional:
;;
;;     MODE - called in a new buffer with no arguments, should establish
;;	      the proper mode for the buffer.
;;

(defun gdb-set-buffer-rules (buffer-type &rest rules)
  (let ((binding (assoc buffer-type gdb-buffer-rules-assoc)))
    (if binding
	(setcdr binding rules)
      (push (cons buffer-type rules)
	    gdb-buffer-rules-assoc))))

;; GUD buffers are an exception to the rules
(gdb-set-buffer-rules 'gdbmi 'error)

;; Partial-output buffer : This accumulates output from a command executed on
;; behalf of emacs (rather than the user).
;;
(gdb-set-buffer-rules 'gdb-partial-output-buffer
		      'gdb-partial-output-name)

(defun gdb-partial-output-name ()
  (concat " *partial-output-"
	  (gdb-get-target-string)
	  "*"))


(gdb-set-buffer-rules 'gdb-inferior-io
		      'gdb-inferior-io-name
		      'gdb-inferior-io-mode)

(defun gdb-inferior-io-name ()
  (concat "*input/output of "
	  (gdb-get-target-string)
	  "*"))

(defun gdb-display-separate-io-buffer ()
  "Display IO of debugged program in a separate window."
  (interactive)
  (if gdb-use-separate-io-buffer
      (gdb-display-buffer
       (gdb-get-buffer-create 'gdb-inferior-io) t)))

(defconst gdb-frame-parameters
  '((height . 14) (width . 80)
    (unsplittable . t)
    (tool-bar-lines . nil)
    (menu-bar-lines . nil)
    (minibuffer . nil)))

(defun gdb-frame-separate-io-buffer ()
  "Display IO of debugged program in a new frame."
  (interactive)
  (if gdb-use-separate-io-buffer
      (let ((special-display-regexps (append special-display-regexps '(".*")))
	    (special-display-frame-alist gdb-frame-parameters))
	(display-buffer (gdb-get-buffer-create 'gdb-inferior-io)))))

(defvar gdb-inferior-io-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'gdb-separate-io-interrupt)
    (define-key map "\C-c\C-z" 'gdb-separate-io-stop)
    (define-key map "\C-c\C-\\" 'gdb-separate-io-quit)
    (define-key map "\C-c\C-d" 'gdb-separate-io-eof)
    (define-key map "\C-d" 'gdb-separate-io-eof)
    map))

(define-derived-mode gdb-inferior-io-mode comint-mode "Inferior I/O"
  "Major mode for gdb inferior-io."
  :syntax-table nil :abbrev-table nil
  ;; We want to use comint because it has various nifty and familiar
  ;; features.  We don't need a process, but comint wants one, so create
  ;; a dummy one.
  (make-comint-in-buffer
   "gdb-inferior" (current-buffer) "sleep" nil "1000000000"))

(defun gdb-inferior-filter (proc string)
  (unless (string-equal string "")
    (gdb-display-buffer (gdb-get-buffer-create 'gdb-inferior-io) t))
  (with-current-buffer (gdb-get-buffer-create 'gdb-inferior-io)
    (insert-before-markers string)))

(defun gdb-separate-io-interrupt ()
  "Interrupt the program being debugged."
  (interactive)
  (interrupt-process
   (get-buffer-process gud-comint-buffer) comint-ptyp))

(defun gdb-separate-io-quit ()
  "Send quit signal to the program being debugged."
  (interactive)
  (quit-process
   (get-buffer-process gud-comint-buffer) comint-ptyp))

(defun gdb-separate-io-stop ()
  "Stop the program being debugged."
  (interactive)
  (stop-process
   (get-buffer-process gud-comint-buffer) comint-ptyp))

(defun gdb-separate-io-eof ()
  "Send end-of-file to the program being debugged."
  (interactive)
  (process-send-eof
   (get-buffer-process gud-comint-buffer)))

(defun gdb-clear-inferior-io ()
  (with-current-buffer (gdb-get-buffer-create 'gdb-inferior-io)
    (erase-buffer)))


(defconst breakpoint-xpm-data
  "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"10 10 2 1\",
\"  c red\",
\"+ c None\",
/* pixels */
\"+++    +++\",
\"++      ++\",
\"+        +\",
\"          \",
\"          \",
\"          \",
\"          \",
\"+        +\",
\"++      ++\",
\"+++    +++\",
};"
  "XPM data used for breakpoint icon.")

(defconst breakpoint-enabled-pbm-data
  "P1
10 10\",
0 0 0 0 1 1 1 1 0 0 0 0
0 0 0 1 1 1 1 1 1 0 0 0
0 0 1 1 1 1 1 1 1 1 0 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 0 1 1 1 1 1 1 1 1 0 0
0 0 0 1 1 1 1 1 1 0 0 0
0 0 0 0 1 1 1 1 0 0 0 0"
  "PBM data used for enabled breakpoint icon.")

(defconst breakpoint-disabled-pbm-data
  "P1
10 10\",
0 0 1 0 1 0 1 0 0 0
0 1 0 1 0 1 0 1 0 0
1 0 1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1 0 1
1 0 1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1 0 1
1 0 1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1 0 1
0 0 1 0 1 0 1 0 1 0
0 0 0 1 0 1 0 1 0 0"
  "PBM data used for disabled breakpoint icon.")

(defvar breakpoint-enabled-icon nil
  "Icon for enabled breakpoint in display margin.")

(defvar breakpoint-disabled-icon nil
  "Icon for disabled breakpoint in display margin.")

(declare-function define-fringe-bitmap "fringe.c"
		  (bitmap bits &optional height width align))

(and (display-images-p)
     ;; Bitmap for breakpoint in fringe
     (define-fringe-bitmap 'breakpoint
       "\x3c\x7e\xff\xff\xff\xff\x7e\x3c")
     ;; Bitmap for gud-overlay-arrow in fringe
     (define-fringe-bitmap 'hollow-right-triangle
       "\xe0\x90\x88\x84\x84\x88\x90\xe0"))

(defface breakpoint-enabled
  '((t
     :foreground "red1"
     :weight bold))
  "Face for enabled breakpoint icon in fringe."
  :group 'gdb)

(defface breakpoint-disabled
  '((((class color) (min-colors 88)) :foreground "grey70")
    ;; Ensure that on low-color displays that we end up something visible.
    (((class color) (min-colors 8) (background light))
     :foreground "black")
    (((class color) (min-colors 8) (background dark))
     :foreground "white")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Face for disabled breakpoint icon in fringe."
  :group 'gdb)


(defun gdb-send (proc string)
  "A comint send filter for gdb."
  (with-current-buffer gud-comint-buffer
    (let ((inhibit-read-only t))
      (remove-text-properties (point-min) (point-max) '(face))))
  ;; mimic <RET> key to repeat previous command in GDB
  (if (not (string-match "^\\s+$" string))
      (setq gdb-last-command string)
    (if gdb-last-command (setq string gdb-last-command)))
  (if gdb-enable-debug
      (push (cons 'mi-send (concat string "\n")) gdb-debug-log))
  (if (string-match "^-" string)
      ;; MI command
      (progn
	(setq gdb-first-done-or-error t)
	(process-send-string proc (concat string "\n")))
    ;; CLI command
    (if (string-match "\\\\$" string)
	(setq gdb-continuation (concat gdb-continuation string "\n"))
      (setq gdb-first-done-or-error t)
      (process-send-string proc (concat "-interpreter-exec console \""
					gdb-continuation string "\"\n"))
      (setq gdb-continuation nil))))

(defun gdb-input (item)
  (if gdb-enable-debug (push (cons 'send-item item) gdb-debug-log))
  (setq gdb-token-number (1+ gdb-token-number))
  (setcar item (concat (number-to-string gdb-token-number) (car item)))
  (push (cons gdb-token-number (car (cdr item))) gdb-handler-alist)
  (process-send-string (get-buffer-process gud-comint-buffer)
		       (car item)))


(defcustom gud-gdb-command-name "gdb -i=mi"
  "Default command to execute an executable under the GDB debugger."
  :type 'string
  :group 'gdb)

(defun gdb-resync()
  (setq gud-running nil)
  (setq gdb-output-sink 'user)
  (setq gdb-pending-triggers nil))

(defun gdb-update ()
  "Update buffers showing status of debug session."
  (when gdb-first-prompt
    (gdb-force-mode-line-update
     (propertize "initializing..." 'face font-lock-variable-name-face))
    (gdb-init-1)
    (setq gdb-first-prompt nil))
  (gdb-get-selected-frame)
  (gdb-invalidate-frames)
  ;; Regenerate breakpoints buffer in case it has been inadvertantly deleted.
  (gdb-get-buffer-create 'gdb-breakpoints-buffer)
  (gdb-invalidate-breakpoints)
  (gdb-invalidate-threads)
  (gdb-get-changed-registers)
  (gdb-invalidate-registers)
  (gdb-invalidate-locals)
  (when (and (boundp 'speedbar-frame) (frame-live-p speedbar-frame))
    (dolist (var gdb-var-list)
      (setcar (nthcdr 5 var) nil))
    (gdb-var-update)))

;; GUD displays the selected GDB frame.  This might might not be the current
;; GDB frame (after up, down etc).  If no GDB frame is visible but the last
;; visited breakpoint is, use that window.
(defun gdb-display-source-buffer (buffer)
  (let* ((last-window (if gud-last-last-frame
			 (get-buffer-window
			  (gud-find-file (car gud-last-last-frame)))))
	 (source-window (or last-window
			    (if (and gdb-source-window
				     (window-live-p gdb-source-window))
				gdb-source-window))))
    (when source-window
      (setq gdb-source-window source-window)
      (set-window-buffer source-window buffer))
    source-window))

(defun gdb-car< (a b)
  (< (car a) (car b)))

(defvar gdbmi-record-list
  '((gdb-gdb . "(gdb) \n")
    (gdb-done . "\\([0-9]*\\)\\^done,?\\(.*?\\)\n")
    (gdb-starting . "\\([0-9]*\\)\\^running\n")
    (gdb-error . "\\([0-9]*\\)\\^error,\\(.*?\\)\n")
    (gdb-console . "~\\(\".*?\"\\)\n")
    (gdb-internals . "&\\(\".*?\"\\)\n")
    (gdb-stopped . "\\*stopped,?\\(.*?\n\\)")
    (gdb-running . "\\*running,\\(.*?\n\\)")
    (gdb-thread-created . "=thread-created,\\(.*?\n\\)")
    (gdb-thread-exited . "=thread-exited,\\(.*?\n\\)")))

(defun gud-gdbmi-marker-filter (string)
  "Filter GDB/MI output."

  ;; Record transactions if logging is enabled.
  (when gdb-enable-debug
    (push (cons 'recv string) gdb-debug-log)
    (if (and gdb-debug-log-max
	     (> (length gdb-debug-log) gdb-debug-log-max))
	(setcdr (nthcdr (1- gdb-debug-log-max) gdb-debug-log) nil)))

  ;; Recall the left over gud-marker-acc from last time
  (setq gud-marker-acc (concat gud-marker-acc string))

  ;; Start accumulating output for the GUD buffer
  (setq gdb-filter-output "")
  (let ((output-record) (output-record-list))

    ;; Process all the complete markers in this chunk.
    (dolist (gdbmi-record gdbmi-record-list)
      (while (string-match (cdr gdbmi-record) gud-marker-acc)
	(push (list (match-beginning 0)
		    (car gdbmi-record)
		    (match-string 1 gud-marker-acc)
		    (match-string 2 gud-marker-acc)
		    (match-end 0))
	      output-record-list)
	(setq gud-marker-acc
	      (concat (substring gud-marker-acc 0 (match-beginning 0))
		      ;; Pad with spaces to preserve position.
		      (make-string (length (match-string 0 gud-marker-acc)) 32)
		      (substring gud-marker-acc (match-end 0))))))

    (setq output-record-list (sort output-record-list 'gdb-car<))

    (dolist (output-record output-record-list)
      (let ((record-type (cadr output-record))
	    (arg1 (caddr output-record))
	    (arg2 (cadddr output-record)))
	(if (eq record-type 'gdb-error)
	    (gdb-done-or-error arg2 arg1 'error)
	  (if (eq record-type 'gdb-done)
	      (gdb-done-or-error arg2 arg1 'done)
	    ;; Suppress "No registers." since GDB 6.8 and earlier duplicates MI
	    ;; error message on internal stream.  Don't print to GUD buffer.
	    (unless (and (eq record-type 'gdb-internals)
		     (string-equal (read arg1) "No registers.\n"))
	      (funcall record-type arg1))))))

    (setq gdb-output-sink 'user)
    ;; Remove padding.
    (string-match "^ *" gud-marker-acc)
    (setq gud-marker-acc (substring gud-marker-acc (match-end 0)))

    gdb-filter-output))

(defun gdb-gdb (output-field))
(defun gdb-thread-created (output-field))
(defun gdb-thread-exited (output-field))

(defun gdb-running (output-field)
  (setq gdb-inferior-status "running")
  (gdb-force-mode-line-update
   (propertize gdb-inferior-status 'face font-lock-type-face))
  (setq gdb-active-process t)
  (setq gud-running t))

(defun gdb-starting (output-field)
  ;; CLI commands don't emit ^running at the moment so use gdb-running too.
  (setq gdb-inferior-status "running")
  (gdb-force-mode-line-update
   (propertize gdb-inferior-status 'face font-lock-type-face))
  (setq gdb-active-process t)
  (setq gud-running t))

;; -break-insert -t didn't give a reason before gdb 6.9
(defconst gdb-stopped-regexp
 "\\(reason=\"\\(.*?\\)\"\\)?\\(\\(,exit-code=.*?\\)*\n\\|.*?,file=\".*?\".*?,fullname=\"\\(.*?\\)\".*?,line=\"\\(.*?\\)\".*?\n\\)")

(defun gdb-stopped (output-field)
  (setq gud-running nil)
  (string-match gdb-stopped-regexp output-field)
  (let ((reason (match-string 2 output-field))
	(file (match-string 5 output-field)))

;;; Don't set gud-last-frame here as it's currently done in gdb-frame-handler
;;; because synchronous GDB doesn't give these fields with CLI.
;;;     (when file
;;;       (setq
;;;        ;; Extract the frame position from the marker.
;;;        gud-last-frame (cons file
;;; 			    (string-to-number
;;; 			     (match-string 6 gud-marker-acc)))))

    (setq gdb-inferior-status (if reason reason "unknown"))
    (gdb-force-mode-line-update
     (propertize gdb-inferior-status 'face font-lock-warning-face))
    (if (string-equal reason "exited-normally")
	(setq gdb-active-process nil)))

  (when gdb-first-done-or-error
    (setq gdb-filter-output (concat gdb-filter-output gdb-prompt-name))
    (gdb-update)
    (setq gdb-first-done-or-error nil)))

;; Remove the trimmings from log stream containing debugging messages
;; being produced by GDB's internals, use warning face and send to GUD
;; buffer.
(defun gdb-internals (output-field)
  (setq gdb-filter-output
	(gdb-concat-output
	 gdb-filter-output
	 (let ((error-message
		(read output-field)))
	   (put-text-property
	    0 (length error-message)
	    'face font-lock-warning-face
	    error-message)
	   error-message))))

;; Remove the trimmings from the console stream and send to GUD buffer
;; (frontend MI commands should not print to this stream)
(defun gdb-console (output-field)
   (setq gdb-filter-output
	(gdb-concat-output
	 gdb-filter-output
	 (read output-field))))

(defun gdb-done-or-error (output-field token-number type)
  (if (string-equal token-number "")
      ;; Output from command entered by user
      (progn
	(setq gdb-output-sink 'user)
	(setq token-number nil)
	;; MI error - send to minibuffer
	(when (eq type 'error)
            ;; Skip "msg=" from `output-field'
	    (message (read (substring output-field 4)))
	    ;; Don't send to the console twice.  (If it is a console error
	    ;; it is also in the console stream.)
	    (setq output-field nil)))
    ;; Output from command from frontend.
    (setq gdb-output-sink 'emacs))

  (gdb-clear-partial-output)
  (when gdb-first-done-or-error
    (unless (or token-number gud-running)
      (setq gdb-filter-output (concat gdb-filter-output gdb-prompt-name)))
    (gdb-update)
    (setq gdb-first-done-or-error nil))

  (setq gdb-filter-output
	(gdb-concat-output gdb-filter-output output-field))

  (if token-number
      (progn
	(with-current-buffer
	    (gdb-get-buffer-create 'gdb-partial-output-buffer)
	  (funcall
	   (cdr (assoc (string-to-number token-number) gdb-handler-alist))))
	(setq gdb-handler-alist
	      (assq-delete-all token-number gdb-handler-alist)))))

(defun gdb-concat-output (so-far new)
  (let ((sink gdb-output-sink))
    (cond
     ((eq sink 'user) (concat so-far new))
     ((eq sink 'emacs)
      (gdb-append-to-partial-output new)
      so-far))))

(defun gdb-append-to-partial-output (string)
  (with-current-buffer (gdb-get-buffer-create 'gdb-partial-output-buffer)
    (goto-char (point-max))
    (insert string)))

(defun gdb-clear-partial-output ()
  (with-current-buffer (gdb-get-buffer-create 'gdb-partial-output-buffer)
    (erase-buffer)))

(defun json-partial-output (&optional fix-key)
  "Parse gdb-partial-output-buffer with `json-read'.

If FIX-KEY is non-nil, strip all \"FIX-KEY=\" occurences from
partial output. This is used to get rid of useless keys in lists
in MI messages, e.g.: [key=.., key=..]. -stack-list-frames and
-break-info are examples of MI commands which issue such
responses.

Note that GDB/MI output syntax is different from JSON both
cosmetically and (in some cases) structurally, so correct results
are not guaranteed."
  (with-current-buffer (gdb-get-buffer-create 'gdb-partial-output-buffer)
    (goto-char (point-min))
    (while (re-search-forward (concat "[\\[,]\\(" fix-key "=\\)") nil t)
      (replace-match "" nil nil nil 1))
     (goto-char (point-min))
     (insert "{")
    ;; Wrap field names in double quotes and replace equal sign with
    ;; semicolon.
    ;; TODO: This breaks badly with foo= inside constants
    (while (re-search-forward "\\([[:alpha:]-_]+\\)=" nil t)
      (replace-match "\"\\1\":" nil nil))
    (goto-char (point-max))
    (insert "}")
    (goto-char (point-min))
    (let ((json-array-type 'list))
      (json-read))))

;; NAME is the function name. DEMAND-PREDICATE tests if output is really needed.
;; GDB-COMMAND is a string of such.  OUTPUT-HANDLER is the function bound to the
;; current input.

(defmacro def-gdb-auto-update-trigger (name demand-predicate gdb-command
					    output-handler)
  `(defun ,name (&optional ignored)
     (if (and ,demand-predicate
	      (not (member ',name
			   gdb-pending-triggers)))
	 (progn
	   (gdb-input
	    (list ,gdb-command ',output-handler))
	   (push ',name gdb-pending-triggers)))))

(defmacro def-gdb-auto-update-handler (name trigger buf-key custom-defun)
  `(defun ,name ()
     (setq gdb-pending-triggers
      (delq ',trigger
	    gdb-pending-triggers))
     (let ((buf (gdb-get-buffer ',buf-key)))
       (and buf
	    (with-current-buffer buf
	      (let* ((window (get-buffer-window buf 0))
		     (start (window-start window))
		     (p (window-point window))
		    (buffer-read-only nil))
		(erase-buffer)
		(insert-buffer-substring (gdb-get-buffer-create
					  'gdb-partial-output-buffer))
		(set-window-start window start)
		(set-window-point window p)))))
     ;; put customisation here
     (,custom-defun)))


;; Breakpoint buffer : This displays the output of `-break-list'.
;;
(gdb-set-buffer-rules 'gdb-breakpoints-buffer
		      'gdb-breakpoints-buffer-name
		      'gdb-breakpoints-mode)

(def-gdb-auto-update-trigger gdb-invalidate-breakpoints
  (gdb-get-buffer 'gdb-breakpoints-buffer)
  "-break-list\n"
  gdb-break-list-handler)

(defconst gdb-break-list-regexp
"bkpt={.*?number=\"\\(.*?\\)\".*?,type=\"\\(.*?\\)\".*?,disp=\"\\(.*?\\)\".*?,\
enabled=\"\\(.\\)\".*?,addr=\"\\(.*?\\)\",\\(?:.*?func=\"\\(.*?\\)\".*?,\
file=\"\\(.*?\\)\".*?,fullname=\".*?\".*?,line=\"\\(.*?\\)\",\
\\|\\(?:.*?what=\"\\(.*?\\)\",\\)*\\).*?times=\"\\(.*?\\)\".*?}")

(defun gdb-break-list-handler ()
  (setq gdb-pending-triggers (delq 'gdb-invalidate-breakpoints
				  gdb-pending-triggers))
  (let ((breakpoint) (breakpoints-list))
    (with-current-buffer (gdb-get-buffer-create 'gdb-partial-output-buffer)
      (goto-char (point-min))
      (while (re-search-forward gdb-break-list-regexp nil t)
	(let ((breakpoint (list (match-string 1)
				(match-string 2)
				(match-string 3)
				(match-string 4)
				(match-string 5)
				(match-string 6)
				(match-string 7)
				(match-string 8)
				(match-string 9)
				(match-string 10))))
	  (push breakpoint breakpoints-list))))
    (let ((buf (gdb-get-buffer 'gdb-breakpoints-buffer)))
      (and buf (with-current-buffer buf
		 (let ((p (point))
		       (buffer-read-only nil))
		   (erase-buffer)
		   (insert "Num Type           Disp Enb Hits Addr       What\n")
		   (dolist (breakpoint breakpoints-list)
		     (insert
		      (concat
		       (nth 0 breakpoint) "   "
		       (nth 1 breakpoint) "     "
		       (nth 2 breakpoint) " "
		       (propertize (nth 3 breakpoint)
			  'face (if (eq (string-to-char (nth 3 breakpoint)) ?y)
				    font-lock-warning-face
				  font-lock-type-face)) "   "
		       (nth 9 breakpoint) " "
		       (nth 4 breakpoint) " "
		       (if (nth 5 breakpoint)
			   (concat "in " (nth 5 breakpoint) " at " (nth 6 breakpoint) ":" (nth 7 breakpoint) "\n")
			 (concat (nth 8 breakpoint) "\n")))))
		   (goto-char p))))))
  (gdb-break-list-custom))

;; Put breakpoint icons in relevant margins (even those set in the GUD buffer).
(defun gdb-break-list-custom ()
  (let ((flag) (bptno))
    ;; Remove all breakpoint-icons in source buffers but not assembler buffer.
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(if (and (eq gud-minor-mode 'gdbmi)
		 (not (string-match "\\` ?\\*.+\\*\\'" (buffer-name))))
	    (gdb-remove-breakpoint-icons (point-min) (point-max)))))
    (with-current-buffer (gdb-get-buffer 'gdb-breakpoints-buffer)
      (save-excursion
	(goto-char (point-min))
	(while (< (point) (- (point-max) 1))
	  (forward-line 1)
	  (if (looking-at "[^\t].*?breakpoint")
	      (progn
		(looking-at "\\([0-9]+\\)\\s-+\\S-+\\s-+\\S-+\\s-+\\(.\\)")
		(setq bptno (match-string 1))
		(setq flag (char-after (match-beginning 2)))
		(beginning-of-line)
		(if (re-search-forward " in \\(.*\\) at\\s-+" nil t)
		    (progn
		      (let ((buffer-read-only nil))
			(add-text-properties (match-beginning 1) (match-end 1)
					     '(face font-lock-function-name-face)))
		      (looking-at "\\(\\S-+\\):\\([0-9]+\\)")
		      (let ((line (match-string 2)) (buffer-read-only nil)
			    (file (match-string 1)))
			(add-text-properties (line-beginning-position)
					     (line-end-position)
			 '(mouse-face highlight
			   help-echo "mouse-2, RET: visit breakpoint"))
			(unless (file-exists-p file)
			   (setq file (cdr (assoc bptno gdb-location-alist))))
			(if (and file
				 (not (string-equal file "File not found")))
			    (with-current-buffer
				(find-file-noselect file 'nowarn)
			      (gdb-init-buffer)
			      ;; Only want one breakpoint icon at each location.
			      (save-excursion
				(goto-line (string-to-number line))
				(gdb-put-breakpoint-icon (eq flag ?y) bptno)))
			  (gdb-input
			   (list (concat "list "
					 (match-string-no-properties 3) ":1\n")
				 'ignore))
			  (gdb-input
			   (list "-file-list-exec-source-file\n"
				 `(lambda () (gdb-get-location
					      ,bptno ,line ,flag))))))))))))
      (end-of-line))))

(defvar gdb-source-file-regexp "fullname=\"\\(.*?\\)\"")

(defun gdb-get-location (bptno line flag)
  "Find the directory containing the relevant source file.
Put in buffer and place breakpoint icon."
  (goto-char (point-min))
  (catch 'file-not-found
    (if (re-search-forward gdb-source-file-regexp nil t)
	(delete (cons bptno "File not found") gdb-location-alist)
	(push (cons bptno (match-string 1)) gdb-location-alist)
      (gdb-resync)
      (unless (assoc bptno gdb-location-alist)
	(push (cons bptno "File not found") gdb-location-alist)
	(message-box "Cannot find source file for breakpoint location.
Add directory to search path for source files using the GDB command, dir."))
      (throw 'file-not-found nil))
    (with-current-buffer (find-file-noselect (match-string 1))
      (gdb-init-buffer)
      ;; only want one breakpoint icon at each location
      (save-excursion
	(goto-line (string-to-number line))
	(gdb-put-breakpoint-icon (eq flag ?y) bptno)))))

(add-hook 'find-file-hook 'gdb-find-file-hook)

(defun gdb-find-file-hook ()
  "Set up buffer for debugging if file is part of the source code
of the current session."
  (if (and (buffer-name gud-comint-buffer)
	   ;; in case gud or gdb-ui is just loaded
	   gud-comint-buffer
	   (eq (buffer-local-value 'gud-minor-mode gud-comint-buffer)
	       'gdbmi))
      (if (member buffer-file-name gdb-source-file-list)
	  (with-current-buffer (find-buffer-visiting buffer-file-name)
	    (gdb-init-buffer)))))

(declare-function gud-remove "gdb-ui" t t) ; gud-def
(declare-function gud-break  "gdb-ui" t t) ; gud-def
(declare-function fringe-bitmaps-at-pos "fringe.c" (&optional pos window))

(defun gdb-mouse-set-clear-breakpoint (event)
  "Set/clear breakpoint in left fringe/margin at mouse click.
If not in a source or disassembly buffer just set point."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((posn (event-end event)))
    (with-selected-window (posn-window posn)
      (if (or (buffer-file-name) (eq major-mode 'gdb-assembler-mode))
	  (if (numberp (posn-point posn))
	      (save-excursion
		(goto-char (posn-point posn))
		(if (or (posn-object posn)
			(eq (car (fringe-bitmaps-at-pos (posn-point posn)))
			    'breakpoint))
		    (gud-remove nil)
		  (gud-break nil)))))
      (posn-set-point posn))))

(defun gdb-mouse-toggle-breakpoint-margin (event)
  "Enable/disable breakpoint in left margin with mouse click."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((posn (event-end event)))
    (if (numberp (posn-point posn))
	(with-selected-window (posn-window posn)
	  (save-excursion
	    (goto-char (posn-point posn))
	    (if	(posn-object posn)
		(gud-basic-call
		 (let ((bptno (get-text-property
			       0 'gdb-bptno (car (posn-string posn)))))
		   (concat
		    (if (get-text-property
			 0 'gdb-enabled (car (posn-string posn)))
			"-break-disable "
		      "-break-enable ")
		    bptno "\n")))))))))

(defun gdb-mouse-toggle-breakpoint-fringe (event)
  "Enable/disable breakpoint in left fringe with mouse click."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let* ((posn (event-end event))
	 (pos (posn-point posn))
	 obj)
    (when (numberp pos)
      (with-selected-window (posn-window posn)
	(save-excursion
	  (set-buffer (window-buffer (selected-window)))
	  (goto-char pos)
	  (dolist (overlay (overlays-in pos pos))
	    (when (overlay-get overlay 'put-break)
	      (setq obj (overlay-get overlay 'before-string))))
	  (when (stringp obj)
	    (gud-basic-call
	     (concat
	      (if (get-text-property 0 'gdb-enabled obj)
		  "-break-disable "
		"-break-enable ")
	       (get-text-property 0 'gdb-bptno obj) "\n"))))))))

(defun gdb-breakpoints-buffer-name ()
  (with-current-buffer gud-comint-buffer
    (concat "*breakpoints of " (gdb-get-target-string) "*")))

(gdb-def-display-buffer
 gdb-display-breakpoints-buffer
 'gdb-breakpoints-buffer
 "Display status of user-settable breakpoints.")

(gdb-def-frame-for-buffer
 gdb-frame-breakpoints-buffer
 'gdb-breakpoints-buffer
 "Display status of user-settable breakpoints in a new frame.")

(defvar gdb-breakpoints-mode-map
  (let ((map (make-sparse-keymap))
	(menu (make-sparse-keymap "Breakpoints")))
    (define-key menu [quit] '("Quit"   . gdb-delete-frame-or-window))
    (define-key menu [goto] '("Goto"   . gdb-goto-breakpoint))
    (define-key menu [delete] '("Delete" . gdb-delete-breakpoint))
    (define-key menu [toggle] '("Toggle" . gdb-toggle-breakpoint))
    (suppress-keymap map)
    (define-key map [menu-bar breakpoints] (cons "Breakpoints" menu))
    (define-key map " " 'gdb-toggle-breakpoint)
    (define-key map "D" 'gdb-delete-breakpoint)
    ;; Don't bind "q" to kill-this-buffer as we need it for breakpoint icons.
    (define-key map "q" 'gdb-delete-frame-or-window)
    (define-key map "\r" 'gdb-goto-breakpoint)
    (define-key map [mouse-2] 'gdb-goto-breakpoint)
    (define-key map [follow-link] 'mouse-face)
    map))

(defun gdb-delete-frame-or-window ()
  "Delete frame if there is only one window.  Otherwise delete the window."
  (interactive)
  (if (one-window-p) (delete-frame)
    (delete-window)))

;;from make-mode-line-mouse-map
(defun gdb-make-header-line-mouse-map (mouse function) "\
Return a keymap with single entry for mouse key MOUSE on the header line.
MOUSE is defined to run function FUNCTION with no args in the buffer
corresponding to the mode line clicked."
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'header-line mouse) function)
    (define-key map (vector 'header-line 'down-mouse-1) 'ignore)
    map))


;; uses "-thread-info". Needs GDB 7.0 onwards.
;;; Threads view

(defun gdb-jump-to (file line)
  (find-file-other-window file)
  (goto-line line))

(define-button-type 'gdb-file-button
  'help-echo "Push to jump to source code"
;  'face 'bold
  'action
  (lambda (b)
    (gdb-jump-to (button-get b 'file)
                 (button-get b 'line))))

(defun gdb-insert-file-location-button (file line)
  "Insert text button which allows jumping to FILE:LINE.

FILE is a full path."
  (insert-text-button
   (format "%s:%d" (file-name-nondirectory file) line)
   :type 'gdb-file-button
   'file file
   'line line))

(defun gdb-threads-buffer-name ()
  (concat "*threads of " (gdb-get-target-string) "*"))

(gdb-def-display-buffer
 gdb-display-threads-buffer
 'gdb-threads-buffer
 "Display GDB threads.")

(gdb-def-frame-for-buffer
 gdb-frame-threads-buffer
 'gdb-threads-buffer
 "Display GDB threads in a new frame.")

(gdb-set-buffer-rules 'gdb-threads-buffer
                      'gdb-threads-buffer-name
                      'gdb-threads-mode)

(def-gdb-auto-update-trigger gdb-invalidate-threads
  (gdb-get-buffer-create 'gdb-threads-buffer)
  "-thread-info\n"
  gdb-thread-list-handler)

(defvar gdb-threads-font-lock-keywords
  '(("in \\([^ ]+\\) ("  (1 font-lock-function-name-face))
    (" \\(stopped\\) in "  (1 font-lock-warning-face))
    ("\\(\\(\\sw\\|[_.]\\)+\\)="  (1 font-lock-variable-name-face)))
  "Font lock keywords used in `gdb-threads-mode'.")

(defun gdb-threads-mode ()
  "Major mode for GDB threads.

\\{gdb-threads-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'gdb-threads-mode)
  (setq mode-name "Threads")
  (use-local-map gdb-threads-mode-map)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (setq header-line-format gdb-breakpoints-header)
  (set (make-local-variable 'font-lock-defaults)
       '(gdb-threads-font-lock-keywords))
  (run-mode-hooks 'gdb-threads-mode-hook)
  'gdb-invalidate-threads)

(defvar gdb-threads-mode-map
  ;; TODO
  (make-sparse-keymap))

(defun gdb-thread-list-handler ()
  (setq gdb-pending-triggers (delq 'gdb-invalidate-threads
                                   gdb-pending-triggers))
  (let* ((res (json-partial-output))
         (threads-list (fadr-q "res.threads"))
         (buf (gdb-get-buffer 'gdb-threads-buffer)))
    (and buf
         (with-current-buffer buf
           (let ((buffer-read-only nil))
             (erase-buffer)
             (dolist (thread threads-list)
               (insert (fadr-format "~.id (~.target-id) ~.state in ~.frame.func " thread))
               ;; Arguments
               (insert "(")
               (let ((args (fadr-q "thread.frame.args")))
                 (dolist (arg args)
                   (insert (fadr-format "~.name=~.value," arg)))
                 (when args (kill-backward-chars 1)))
               (insert ")")
               (insert-frame-location (fadr-q "thread.frame"))
               (insert (fadr-format " at ~.frame.addr\n" thread))))))))


;;; Memory view
(defun gdb-todo-memory ()
  (interactive)
  (message-box
   "TODO: Implement memory buffer using\nMI command -data-read-memory"))

;;; Disassembly view

(defun gdb-disassembly-buffer-name ()
  (concat "*disassembly of " (gdb-get-target-string) "*"))

(gdb-def-display-buffer
 gdb-display-disassembly-buffer
 'gdb-disassembly-buffer
 "Display disassembly for current stack frame.")

(gdb-def-frame-for-buffer
 gdb-frame-disassembly-buffer
 'gdb-disassembly-buffer
 "Display disassembly in a new frame.")

(gdb-set-buffer-rules 'gdb-disassembly-buffer
                      'gdb-disassembly-buffer-name
                      'gdb-disassembly-mode)

(def-gdb-auto-update-trigger gdb-invalidate-disassembly
  (gdb-get-buffer-create 'gdb-disassembly-buffer)
  (let ((file (or gdb-selected-file gdb-main-file))
        (line (or gdb-selected-line 1)))
    (if file
        (format "-data-disassemble -f %s -l %d -n -1 -- 0\n" file line)
      ""))
  gdb-disassembly-handler)

(defvar gdb-disassembly-font-lock-keywords
  '(;; <__function.name+n>
    ("<\\(\\(\\sw\\|[_.]\\)+\\)\\(\\+[0-9]+\\)?>"
     (1 font-lock-function-name-face))
    ;; 0xNNNNNNNN <__function.name+n>: opcode
    ("^0x[0-9a-f]+ \\(<\\(\\(\\sw\\|[_.]\\)+\\)\\+[0-9]+>\\)?:[ \t]+\\(\\sw+\\)"
     (4 font-lock-keyword-face))
    ;; %register(at least i386)
    ("%\\sw+" . font-lock-variable-name-face)
    ("^\\(Dump of assembler code for function\\) \\(.+\\):"
     (1 font-lock-comment-face)
     (2 font-lock-function-name-face))
    ("^\\(End of assembler dump\\.\\)" . font-lock-comment-face))
  "Font lock keywords used in `gdb-disassembly-mode'.")

(defvar gdb-disassembly-mode-map
  ;; TODO
  (make-sparse-keymap))

(defun gdb-disassembly-mode ()
  "Major mode for GDB disassembly information.

\\{gdb-disassembly-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'gdb-disassembly-mode)
  (setq mode-name "Disassembly")
  (use-local-map gdb-disassembly-mode-map)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (set (make-local-variable 'font-lock-defaults)
       '(gdb-disassembly-font-lock-keywords))
  (run-mode-hooks 'gdb-disassembly-mode-hook)
  'gdb-invalidate-disassembly)

(defun gdb-disassembly-handler ()
  (setq gdb-pending-triggers (delq 'gdb-invalidate-disassembly
                                   gdb-pending-triggers))
  (let* ((res (json-partial-output))
         (instructions (fadr-member res ".asm_insns"))
         (buf (gdb-get-buffer 'gdb-disassembly-buffer)))
    (and buf
         (with-current-buffer buf
           (let ((buffer-read-only nil))
             (erase-buffer)
             (dolist (instr instructions)
               (insert (fadr-format "~.address <~.func-name+~.offset>:\t~.inst\n" instr))))))))


;;; Breakpoints view

(defvar gdb-breakpoints-header
 `(,(propertize "Breakpoints"
		'help-echo "mouse-1: select"
		'mouse-face 'mode-line-highlight
		'face 'mode-line
		'local-map
		(gdb-make-header-line-mouse-map
		 'mouse-1
		 (lambda (event) (interactive "e")
		   (save-selected-window
		     (select-window (posn-window (event-start event)))
		     (set-window-dedicated-p (selected-window) nil)
		     (switch-to-buffer
		      (gdb-get-buffer-create 'gdb-breakpoints-buffer))
		     (set-window-dedicated-p (selected-window) t)))))
   " "
   ,(propertize "Threads"
		'help-echo "mouse-1: select"
		'mouse-face 'mode-line-highlight
		'face 'mode-line
		'local-map
		(gdb-make-header-line-mouse-map
		 'mouse-1
                 ;; TODO: same code few lines above
		 (lambda (event) (interactive "e")
		   (save-selected-window
		     (select-window (posn-window (event-start event)))
		     (set-window-dedicated-p (selected-window) nil)
		     (switch-to-buffer
		      (gdb-get-buffer-create 'gdb-threads-buffer))
		     (set-window-dedicated-p (selected-window) t)))
))))

(defun gdb-breakpoints-mode ()
  "Major mode for gdb breakpoints.

\\{gdb-breakpoints-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'gdb-breakpoints-mode)
  (setq mode-name "Breakpoints")
  (use-local-map gdb-breakpoints-mode-map)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (setq header-line-format gdb-breakpoints-header)
  (run-mode-hooks 'gdb-breakpoints-mode-hook)
  'gdb-invalidate-breakpoints)

(defconst gdb-breakpoint-regexp
  "\\([0-9]+\\).*?\\(?:point\\|catch\\s-+\\S-+\\)\\s-+\\S-+\\s-+\\(.\\)\\s-+")

(defun gdb-toggle-breakpoint ()
  "Enable/disable breakpoint at current line."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (if (looking-at gdb-breakpoint-regexp)
	(gud-basic-call
	 (concat (if (eq ?y (char-after (match-beginning 2)))
		     "-break-disable "
		   "-break-enable ")
		 (match-string 1)))
      (error "Not recognized as break/watchpoint line"))))

(defun gdb-delete-breakpoint ()
  "Delete the breakpoint at current line."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (if (looking-at gdb-breakpoint-regexp)
	(gud-basic-call (concat "-break-delete " (match-string 1)))
      (error "Not recognized as break/watchpoint line"))))

(defun gdb-goto-breakpoint (&optional event)
  "Display the breakpoint location specified at current line."
  (interactive (list last-input-event))
  (if event (posn-set-point (event-end event)))
  ;; Hack to stop gdb-goto-breakpoint displaying in GUD buffer.
  (let ((window (get-buffer-window gud-comint-buffer)))
    (if window (save-selected-window  (select-window window))))
  (save-excursion
    (beginning-of-line 1)
    (if (looking-at "\\([0-9]+\\) .+ in .+ at\\s-+\\(\\S-+\\):\\([0-9]+\\)")
	(let ((bptno (match-string 1))
	      (file  (match-string 2))
	      (line  (match-string 3)))
	  (save-selected-window
	    (let* ((buffer (find-file-noselect
			 (if (file-exists-p file) file
			   (cdr (assoc bptno gdb-location-alist)))))
		   (window (or (gdb-display-source-buffer buffer)
			       (display-buffer buffer))))
	      (setq gdb-source-window window)
	      (with-current-buffer buffer
		(goto-line (string-to-number line))
		(set-window-point window (point))))))
      (error "Not recognized as break/watchpoint line"))))


;; Frames buffer.  This displays a perpetually correct bactrack trace.
;;
(gdb-set-buffer-rules 'gdb-stack-buffer
		      'gdb-stack-buffer-name
		      'gdb-frames-mode)

(def-gdb-auto-update-trigger gdb-invalidate-frames
  (gdb-get-buffer 'gdb-stack-buffer)
  "-stack-list-frames\n"
  gdb-stack-list-frames-handler)

(defun insert-frame-location (frame)
  "Insert \"file:line\" button or library name for FRAME object."
  (let ((file (fadr-q "frame.fullname"))
        (line (fadr-q "frame.line"))
        (from (fadr-q "frame.from")))
    (cond (file
           ;; Filename with line number
           (insert " of ")
           (gdb-insert-file-location-button
            file (string-to-number line)))
          ;; Library
          (from (insert (format " of %s" from))))))

(defun gdb-stack-list-frames-handler ()
  (setq gdb-pending-triggers (delq 'gdb-invalidate-frames
				  gdb-pending-triggers))
  (with-current-buffer (gdb-get-buffer-create 'gdb-partial-output-buffer)
    (let* ((res (json-partial-output "frame"))
           (stack (fadr-q "res.stack"))
           (buf (gdb-get-buffer 'gdb-stack-buffer)))
      (and buf 
           (with-current-buffer buf
             (let ((buffer-read-only nil))
               (erase-buffer)
               (dolist (frame (nreverse stack))
                 (insert (fadr-expand "~.level in ~.func" frame))
                 (insert-frame-location frame)
                 (newline))
               (gdb-stack-list-frames-custom)))))))

(defun gdb-stack-list-frames-custom ()
  (with-current-buffer (gdb-get-buffer 'gdb-stack-buffer)
    (save-excursion
      (let ((buffer-read-only nil))
	(goto-char (point-min))
	(forward-line 1)
	(while (< (point) (point-max))
	  (add-text-properties (point-at-bol) (1+ (point-at-bol))
			     '(mouse-face highlight
			       help-echo "mouse-2, RET: Select frame"))
	  (beginning-of-line)
	  (when (and (looking-at "^[0-9]+\\s-+\\S-+\\s-+\\(\\S-+\\)")
		     (equal (match-string 1) gdb-selected-frame))
		(if (> (car (window-fringes)) 0)
		    (progn
		      (or gdb-stack-position
			  (setq gdb-stack-position (make-marker)))
		      (set-marker gdb-stack-position (point)))
		  (let ((bl (point-at-bol)))
		    (put-text-property bl (+ bl 4)
				       'face '(:inverse-video t)))))
	  (forward-line 1))))))

(defun gdb-stack-buffer-name ()
  (with-current-buffer gud-comint-buffer
    (concat "*stack frames of " (gdb-get-target-string) "*")))

(gdb-def-display-buffer
 gdb-display-stack-buffer
 'gdb-stack-buffer
 "Display backtrace of current stack.")

(gdb-def-frame-for-buffer
 gdb-frame-stack-buffer
 'gdb-stack-buffer
 "Display backtrace of current stack in a new frame.")

(defvar gdb-frames-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-this-buffer)
    (define-key map "\r" 'gdb-frames-select)
    (define-key map [mouse-2] 'gdb-frames-select)
    (define-key map [follow-link] 'mouse-face)
    map))

(defvar gdb-frames-font-lock-keywords
  '(("in \\([^ ]+\\) of "  (1 font-lock-function-name-face)))
  "Font lock keywords used in `gdb-frames-mode'.")

(defun gdb-frames-mode ()
  "Major mode for gdb call stack.

\\{gdb-frames-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'gdb-frames-mode)
  (setq mode-name "Frames")
  (setq gdb-stack-position nil)
  (add-to-list 'overlay-arrow-variable-list 'gdb-stack-position)
  (setq truncate-lines t)  ;; Make it easier to see overlay arrow.
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (use-local-map gdb-frames-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(gdb-frames-font-lock-keywords))
  (run-mode-hooks 'gdb-frames-mode-hook)
  'gdb-invalidate-frames)

(defun gdb-get-frame-number ()
  (save-excursion
    (end-of-line)
    (let* ((pos (re-search-backward "^\\([0-9]+\\)" nil t))
	   (n (or (and pos (match-string-no-properties 1)) "0")))
      n)))

(defun gdb-frames-select (&optional event)
  "Select the frame and display the relevant source."
  (interactive (list last-input-event))
  (if event (posn-set-point (event-end event)))
  (gud-basic-call (concat "-stack-select-frame " (gdb-get-frame-number))))


;; Locals buffer.
;; uses "-stack-list-locals --simple-values". Needs GDB 6.1 onwards.
(gdb-set-buffer-rules 'gdb-locals-buffer
		      'gdb-locals-buffer-name
		      'gdb-locals-mode)

(def-gdb-auto-update-trigger gdb-invalidate-locals
  (gdb-get-buffer 'gdb-locals-buffer)
  "-stack-list-locals --simple-values\n"
  gdb-stack-list-locals-handler)

(defconst gdb-stack-list-locals-regexp
  (concat "name=\"\\(.*?\\)\",type=\"\\(.*?\\)\""))

(defvar gdb-locals-watch-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\r" 'gud-watch)
    (define-key map [mouse-2] 'gud-watch)
    map)
 "Keymap to create watch expression of a complex data type local variable.")

(defvar gdb-edit-locals-map-1
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\r" 'gdb-edit-locals-value)
    (define-key map [mouse-2] 'gdb-edit-locals-value)
    map)
 "Keymap to edit value of a simple data type local variable.")

(defun gdb-edit-locals-value (&optional event)
  "Assign a value to a variable displayed in the locals buffer."
  (interactive (list last-input-event))
  (save-excursion
    (if event (posn-set-point (event-end event)))
    (beginning-of-line)
    (let* ((var (current-word))
	   (value (read-string (format "New value (%s): " var))))
      (gud-basic-call
       (concat  "-gdb-set variable " var " = " value)))))

;; Dont display values of arrays or structures.
;; These can be expanded using gud-watch.
(defun gdb-stack-list-locals-handler nil
  (setq gdb-pending-triggers (delq 'gdb-invalidate-locals
				  gdb-pending-triggers))
   (let (local locals-list)
    (goto-char (point-min))
    (while (re-search-forward gdb-stack-list-locals-regexp nil t)
      (let ((local (list (match-string 1)
			 (match-string 2)
			 nil)))
	(if (looking-at ",value=\\(\".*\"\\)}")
	    (setcar (nthcdr 2 local) (read (match-string 1))))
	(push local locals-list)))
    (let ((buf (gdb-get-buffer 'gdb-locals-buffer)))
      (and buf (with-current-buffer buf
		 (let* ((window (get-buffer-window buf 0))
			(start (window-start window))
			(p (window-point window))
			(buffer-read-only nil) (name) (value))
		   (erase-buffer)
		   (dolist (local locals-list)
		     (setq name (car local))
		     (setq value (nth 2 local))
		     (if (or (not value)
			     (string-match "\\0x" value))
		       (add-text-properties 0 (length name)
			    `(mouse-face highlight
			      help-echo "mouse-2: create watch expression"
			      local-map ,gdb-locals-watch-map)
			    name)
			 (add-text-properties 0 (length value)
			      `(mouse-face highlight
			        help-echo "mouse-2: edit value"
			        local-map ,gdb-edit-locals-map-1)
			      value))
		       (insert
			(concat name "\t" (nth 1 local)
				"\t" (nth 2 local) "\n")))
		   (set-window-start window start)
		   (set-window-point window p)))))))

(defvar gdb-locals-header
 `(,(propertize "Locals"
		'help-echo "mouse-1: select"
		'mouse-face 'mode-line-highlight
		'face 'mode-line
		'local-map
		(gdb-make-header-line-mouse-map
		 'mouse-1
		 (lambda (event) (interactive "e")
		   (save-selected-window
		     (select-window (posn-window (event-start event)))
		     (set-window-dedicated-p (selected-window) nil)
		     (switch-to-buffer
		      (gdb-get-buffer-create 'gdb-locals-buffer))
		     (set-window-dedicated-p (selected-window) t)))))
   " "
   ,(propertize "Registers"
		'help-echo "mouse-1: select"
		'mouse-face 'mode-line-highlight
		'face 'mode-line
		'local-map
		(gdb-make-header-line-mouse-map
		 'mouse-1
		 (lambda (event) (interactive "e")
		   (save-selected-window
		     (select-window (posn-window (event-start event)))
		     (set-window-dedicated-p (selected-window) nil)
		     (switch-to-buffer
		      (gdb-get-buffer-create 'gdb-registers-buffer))
		     (set-window-dedicated-p (selected-window) t)))))))

(defvar gdb-locals-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-this-buffer)
     map))

(defun gdb-locals-mode ()
  "Major mode for gdb locals.

\\{gdb-locals-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'gdb-locals-mode)
  (setq mode-name (concat "Locals:" gdb-selected-frame))
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (setq header-line-format gdb-locals-header)
  (use-local-map gdb-locals-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(gdb-locals-font-lock-keywords))
  (run-mode-hooks 'gdb-locals-mode-hook)
  'gdb-invalidate-locals)

(defun gdb-locals-buffer-name ()
  (with-current-buffer gud-comint-buffer
    (concat "*locals of " (gdb-get-target-string) "*")))

(gdb-def-display-buffer
 gdb-display-local-buffer
 'gdb-locals-buffer
 "Display local variables of current stack and their values.")

(gdb-def-frame-for-buffer
 gdb-frame-locals-buffer
 'gdb-locals-buffer
 "Display local variables of current stack and their values in a new frame.")


;; Registers buffer.
;;
(gdb-set-buffer-rules 'gdb-registers-buffer
		      'gdb-registers-buffer-name
		      'gdb-registers-mode)

(def-gdb-auto-update-trigger gdb-invalidate-registers
  (gdb-get-buffer 'gdb-registers-buffer)
  "-data-list-register-values x\n"
  gdb-data-list-register-values-handler)

(defconst gdb-data-list-register-values-regexp
  "number=\"\\(.*?\\)\",value=\"\\(.*?\\)\"")

(defun gdb-data-list-register-values-handler ()
  (setq gdb-pending-triggers (delq 'gdb-invalidate-registers
				   gdb-pending-triggers))
  (goto-char (point-min))
  (if (re-search-forward gdb-error-regexp nil t)
      (progn
	(let ((match nil))
	  (setq match (match-string 1))
	  (with-current-buffer (gdb-get-buffer 'gdb-registers-buffer)
	    (let ((buffer-read-only nil))
	      (erase-buffer)
	      (insert match)
	      (goto-char (point-min))))))
    (let ((register-list (reverse gdb-register-names))
	  (register nil) (register-string nil) (register-values nil))
      (goto-char (point-min))
      (while (re-search-forward gdb-data-list-register-values-regexp nil t)
	(setq register (pop register-list))
	(setq register-string (concat register "\t" (match-string 2) "\n"))
	(if (member (match-string 1) gdb-changed-registers)
	    (put-text-property 0 (length register-string)
			       'face 'font-lock-warning-face
			       register-string))
	(setq register-values
	      (concat register-values register-string)))
      (let ((buf (gdb-get-buffer 'gdb-registers-buffer)))
	(with-current-buffer buf
	  (let ((p (window-point (get-buffer-window buf 0)))
		(buffer-read-only nil))
	    (erase-buffer)
	    (insert register-values)
	    (set-window-point (get-buffer-window buf 0) p))))))
  (gdb-data-list-register-values-custom))

(defun gdb-data-list-register-values-custom ()
  (with-current-buffer (gdb-get-buffer 'gdb-registers-buffer)
    (save-excursion
      (let ((buffer-read-only nil)
	    bl)
	(goto-char (point-min))
	(while (< (point) (point-max))
	  (setq bl (line-beginning-position))
	  (when (looking-at "^[^\t]+")
	    (put-text-property bl (match-end 0)
			       'face font-lock-variable-name-face))
	  (forward-line 1))))))

(defvar gdb-registers-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-this-buffer)
     map))

(defun gdb-registers-mode ()
  "Major mode for gdb registers.

\\{gdb-registers-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'gdb-registers-mode)
  (setq mode-name "Registers")
  (setq header-line-format gdb-locals-header)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (use-local-map gdb-registers-mode-map)
  (run-mode-hooks 'gdb-registers-mode-hook)
  'gdb-invalidate-registers)

(defun gdb-registers-buffer-name ()
  (with-current-buffer gud-comint-buffer
    (concat "*registers of " (gdb-get-target-string) "*")))

(gdb-def-display-buffer
 gdb-display-registers-buffer
 'gdb-registers-buffer
 "Display integer register contents.")

(gdb-def-frame-for-buffer
 gdb-frame-registers-buffer
 'gdb-registers-buffer
  "Display integer register contents in a new frame.")

;; Needs GDB 6.4 onwards (used to fail with no stack).
(defun gdb-get-changed-registers ()
  (if (and (gdb-get-buffer 'gdb-registers-buffer)
	   (not (member 'gdb-get-changed-registers gdb-pending-triggers)))
      (progn
	(gdb-input
	 (list
	  "-data-list-changed-registers\n"
	  'gdb-get-changed-registers-handler))
	(push 'gdb-get-changed-registers gdb-pending-triggers))))

(defconst gdb-data-list-register-names-regexp "\"\\(.*?\\)\"")

(defun gdb-get-changed-registers-handler ()
  (setq gdb-pending-triggers
	(delq 'gdb-get-changed-registers gdb-pending-triggers))
  (setq gdb-changed-registers nil)
  (goto-char (point-min))
  (while (re-search-forward gdb-data-list-register-names-regexp nil t)
    (push (match-string 1) gdb-changed-registers)))

(defun gdb-get-register-names ()
  "Create a list of register names."
  (goto-char (point-min))
  (setq gdb-register-names nil)
  (while (re-search-forward gdb-data-list-register-names-regexp nil t)
    (push (match-string 1) gdb-register-names)))


(defun gdb-get-source-file-list ()
  "Create list of source files for current GDB session.
If buffers already exist for any of these files, gud-minor-mode
is set in them."
  (goto-char (point-min))
  (while (re-search-forward gdb-source-file-regexp nil t)
    (push (match-string 1) gdb-source-file-list))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (member buffer-file-name gdb-source-file-list)
	(gdb-init-buffer))))
  (gdb-force-mode-line-update
   (propertize "ready" 'face font-lock-variable-name-face)))

(defun gdb-get-selected-frame ()
  (if (not (member 'gdb-get-selected-frame gdb-pending-triggers))
      (progn
	(gdb-input
	 (list "-stack-info-frame\n" 'gdb-frame-handler))
	(push 'gdb-get-selected-frame
	       gdb-pending-triggers))))

(defun gdb-frame-handler ()
  (setq gdb-pending-triggers
	(delq 'gdb-get-selected-frame gdb-pending-triggers))
  (let ((frame (fadr-member (json-partial-output) ".frame")))
    (when frame
      (setq gdb-frame-number (fadr-q "frame.level"))
      (setq gdb-pc-address (fadr-q "frame.addr"))
      (setq gdb-selected-frame (fadr-q "frame.func"))
      (setq gdb-selected-file (fadr-q "frame.fullname"))
      (let ((line (fadr-q "frame.line")))
        (setq gdb-selected-line (or (and line (string-to-number line))
                                    nil))) ; don't fail if line is nil
      (setq gud-last-frame (cons gdb-selected-file gdb-selected-line))
      (gud-display-frame)
      (if (gdb-get-buffer 'gdb-locals-buffer)
          (with-current-buffer (gdb-get-buffer 'gdb-locals-buffer)
            (setq mode-name (concat "Locals:" gdb-selected-frame))))
      (if (gdb-get-buffer 'gdb-disassembly-buffer)
          (with-current-buffer (gdb-get-buffer 'gdb-disassembly-buffer)
            (setq mode-name (concat "Machine:" gdb-selected-frame))))
      (if gud-overlay-arrow-position
          (let ((buffer (marker-buffer gud-overlay-arrow-position))
                (position (marker-position gud-overlay-arrow-position)))
            (when buffer
              (with-current-buffer buffer
                (setq fringe-indicator-alist
                      (if (string-equal gdb-frame-number "0")
                          nil
                        '((overlay-arrow . hollow-right-triangle))))
                (setq gud-overlay-arrow-position (make-marker))
                (set-marker gud-overlay-arrow-position position)))))
      (gdb-invalidate-disassembly))))
  
(defvar gdb-prompt-name-regexp "value=\"\\(.*?\\)\"")

(defun gdb-get-prompt ()
  "Find prompt for GDB session."
  (goto-char (point-min))
  (setq gdb-prompt-name nil)
  (re-search-forward gdb-prompt-name-regexp nil t)
  (setq gdb-prompt-name (match-string 1))
  ;; Insert first prompt.
  (setq gdb-filter-output (concat gdb-filter-output gdb-prompt-name)))

;;;; Window management
(defun gdb-display-buffer (buf dedicated &optional frame)
  (let ((answer (get-buffer-window buf (or frame 0))))
    (if answer
	(display-buffer buf nil (or frame 0)) ;Deiconify the frame if necessary.
      (let ((window (get-lru-window)))
	(if (eq (buffer-local-value 'gud-minor-mode (window-buffer window))
		  'gdbmi)
	    (let* ((largest (get-largest-window))
		   (cur-size (window-height largest)))
	      (setq answer (split-window largest))
	      (set-window-buffer answer buf)
	      (set-window-dedicated-p answer dedicated)
	      answer)
	  (set-window-buffer window buf)
	  window)))))


;;; Shared keymap initialization:

(let ((menu (make-sparse-keymap "GDB-Windows")))
  (define-key gud-menu-map [displays]
    `(menu-item "GDB-Windows" ,menu
		:visible (eq gud-minor-mode 'gdbmi)))
  (define-key menu [gdb] '("Gdb" . gdb-display-gdb-buffer))
  (define-key menu [threads] '("Threads" . gdb-display-threads-buffer))
;  (define-key menu [memory] '("Memory" . gdb-display-memory-buffer))
  (define-key menu [memory] '("Memory" . gdb-todo-memory))
  (define-key menu [disassembly]
    '("Disassembly" . gdb-display-assembler-buffer))
  (define-key menu [registers] '("Registers" . gdb-display-registers-buffer))
  (define-key menu [inferior]
    '(menu-item "Separate IO" gdb-display-separate-io-buffer
		:enable gdb-use-separate-io-buffer))
  (define-key menu [locals] '("Locals" . gdb-display-locals-buffer))
  (define-key menu [frames] '("Stack" . gdb-display-stack-buffer))
  (define-key menu [breakpoints]
    '("Breakpoints" . gdb-display-breakpoints-buffer)))

(let ((menu (make-sparse-keymap "GDB-Frames")))
  (define-key gud-menu-map [frames]
    `(menu-item "GDB-Frames" ,menu
		:visible (eq gud-minor-mode 'gdbmi)))
  (define-key menu [gdb] '("Gdb" . gdb-frame-gdb-buffer))
  (define-key menu [threads] '("Threads" . gdb-frame-threads-buffer))
;  (define-key menu [memory] '("Memory" . gdb-frame-memory-buffer))
  (define-key menu [memory] '("Memory" . gdb-todo-memory))
  (define-key menu [disassembly] '("Disassembly" . gdb-frame-assembler-buffer))
  (define-key menu [registers] '("Registers" . gdb-frame-registers-buffer))
  (define-key menu [inferior]
    '(menu-item "Separate IO" gdb-frame-separate-io-buffer
		:enable gdb-use-separate-io-buffer))
  (define-key menu [locals] '("Locals" . gdb-frame-locals-buffer))
  (define-key menu [frames] '("Stack" . gdb-frame-stack-buffer))
  (define-key menu [breakpoints]
    '("Breakpoints" . gdb-frame-breakpoints-buffer)))

(let ((menu (make-sparse-keymap "GDB-MI")))
  (define-key gud-menu-map [mi]
    `(menu-item "GDB-MI" ,menu :visible (eq gud-minor-mode 'gdbmi)))
  (define-key menu [gdb-customize]
  '(menu-item "Customize" (lambda () (interactive) (customize-group 'gdb))
	      :help "Customize Gdb Graphical Mode options."))
  (define-key menu [gdb-use-separate-io]
  '(menu-item "Separate IO" gdb-use-separate-io-buffer
	      :help "Toggle separate IO for debugged program."
	      :button (:toggle . gdb-use-separate-io-buffer)))
  (define-key menu [gdb-many-windows]
  '(menu-item "Display Other Windows" gdb-many-windows
	      :help "Toggle display of locals, stack and breakpoint information"
	      :button (:toggle . gdb-many-windows)))
  (define-key menu [gdb-restore-windows]
  '(menu-item "Restore Window Layout" gdb-restore-windows
	      :help "Restore standard layout for debug session.")))

(defun gdb-frame-gdb-buffer ()
  "Display GUD buffer in a new frame."
  (interactive)
  (let ((special-display-regexps (append special-display-regexps '(".*")))
	(special-display-frame-alist
	 (remove '(menu-bar-lines) (remove '(tool-bar-lines)
					   gdb-frame-parameters)))
	(same-window-regexps nil))
    (display-buffer gud-comint-buffer)))

(defun gdb-display-gdb-buffer ()
  "Display GUD buffer."
  (interactive)
  (let ((same-window-regexps nil))
    (select-window (display-buffer gud-comint-buffer nil 0))))

(defun gdb-set-window-buffer (name)
  (set-window-buffer (selected-window) (get-buffer name))
  (set-window-dedicated-p (selected-window) t))

(defun gdb-setup-windows ()
  "Layout the window pattern for `gdb-many-windows'."
  (gdb-display-locals-buffer)
  (gdb-display-stack-buffer)
  (delete-other-windows)
  (gdb-display-breakpoints-buffer)
  (delete-other-windows)
  ; Don't dedicate.
  (pop-to-buffer gud-comint-buffer)
  (split-window nil ( / ( * (window-height) 3) 4))
  (split-window nil ( / (window-height) 3))
  (split-window-horizontally)
  (other-window 1)
  (gdb-set-window-buffer (gdb-locals-buffer-name))
  (other-window 1)
  (switch-to-buffer
       (if gud-last-last-frame
	   (gud-find-file (car gud-last-last-frame))
	 (if gdb-main-file
	     (gud-find-file gdb-main-file)
	   ;; Put buffer list in window if we
	   ;; can't find a source file.
	   (list-buffers-noselect))))
  (setq gdb-source-window (selected-window))
  (when gdb-use-separate-io-buffer
    (split-window-horizontally)
    (other-window 1)
    (gdb-set-window-buffer
     (gdb-get-buffer-create 'gdb-inferior-io)))
  (other-window 1)
  (gdb-set-window-buffer (gdb-stack-buffer-name))
  (split-window-horizontally)
  (other-window 1)
  (gdb-set-window-buffer (gdb-breakpoints-buffer-name))
  (other-window 1))

(defcustom gdb-many-windows nil
  "If nil just pop up the GUD buffer unless `gdb-show-main' is t.
In this case it starts with two windows: one displaying the GUD
buffer and the other with the source file with the main routine
of the debugged program.  Non-nil means display the layout shown for
`gdb'."
  :type 'boolean
  :group 'gdb
  :version "22.1")

(defun gdb-many-windows (arg)
  "Toggle the number of windows in the basic arrangement.
With arg, display additional buffers iff arg is positive."
  (interactive "P")
  (setq gdb-many-windows
	(if (null arg)
	    (not gdb-many-windows)
	  (> (prefix-numeric-value arg) 0)))
  (message (format "Display of other windows %sabled"
		   (if gdb-many-windows "en" "dis")))
  (if (and gud-comint-buffer
	   (buffer-name gud-comint-buffer))
      (condition-case nil
	  (gdb-restore-windows)
	(error nil))))

(defun gdb-restore-windows ()
  "Restore the basic arrangement of windows used by gdb.
This arrangement depends on the value of `gdb-many-windows'."
  (interactive)
  (pop-to-buffer gud-comint-buffer)	;Select the right window and frame.
    (delete-other-windows)
  (if gdb-many-windows
      (gdb-setup-windows)
    (when (or gud-last-last-frame gdb-show-main)
      (split-window)
      (other-window 1)
      (switch-to-buffer
       (if gud-last-last-frame
	   (gud-find-file (car gud-last-last-frame))
	 (gud-find-file gdb-main-file)))
      (setq gdb-source-window (selected-window))
      (other-window 1))))

(defun gdb-reset ()
  "Exit a debugging session cleanly.
Kills the gdb buffers, and resets variables and the source buffers."
  (dolist (buffer (buffer-list))
    (unless (eq buffer gud-comint-buffer)
      (with-current-buffer buffer
	(if (eq gud-minor-mode 'gdbmi)
 	    (if (string-match "\\` ?\\*.+\\*\\'" (buffer-name))
		(kill-buffer nil)
	      (gdb-remove-breakpoint-icons (point-min) (point-max) t)
	      (setq gud-minor-mode nil)
	      (kill-local-variable 'tool-bar-map)
	      (kill-local-variable 'gdb-define-alist))))))
  (setq gdb-overlay-arrow-position nil)
  (setq overlay-arrow-variable-list
	(delq 'gdb-overlay-arrow-position overlay-arrow-variable-list))
  (setq fringe-indicator-alist '((overlay-arrow . right-triangle)))
  (setq gdb-stack-position nil)
  (setq overlay-arrow-variable-list
	(delq 'gdb-stack-position overlay-arrow-variable-list))
  (if (boundp 'speedbar-frame) (speedbar-timer-fn))
  (setq gud-running nil)
  (setq gdb-active-process nil)
  (remove-hook 'after-save-hook 'gdb-create-define-alist t))

(defun gdb-get-source-file ()
  "Find the source file where the program starts and display it with related
buffers, if required."
  (goto-char (point-min))
  (if (re-search-forward gdb-source-file-regexp nil t)
      (setq gdb-main-file (match-string 1)))
 (if gdb-many-windows
      (gdb-setup-windows)
   (gdb-get-buffer-create 'gdb-breakpoints-buffer)
   (if gdb-show-main
       (let ((pop-up-windows t))
	 (display-buffer (gud-find-file gdb-main-file))))))

;;from put-image
(defun gdb-put-string (putstring pos &optional dprop &rest sprops)
  "Put string PUTSTRING in front of POS in the current buffer.
PUTSTRING is displayed by putting an overlay into the current buffer with a
`before-string' string that has a `display' property whose value is
PUTSTRING."
  (let ((string (make-string 1 ?x))
	(buffer (current-buffer)))
    (setq putstring (copy-sequence putstring))
    (let ((overlay (make-overlay pos pos buffer))
	  (prop (or dprop
		    (list (list 'margin 'left-margin) putstring))))
      (put-text-property 0 1 'display prop string)
      (if sprops
	  (add-text-properties 0 1 sprops string))
      (overlay-put overlay 'put-break t)
      (overlay-put overlay 'before-string string))))

;;from remove-images
(defun gdb-remove-strings (start end &optional buffer)
  "Remove strings between START and END in BUFFER.
Remove only strings that were put in BUFFER with calls to `gdb-put-string'.
BUFFER nil or omitted means use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (dolist (overlay (overlays-in start end))
    (when (overlay-get overlay 'put-break)
	  (delete-overlay overlay))))

(defun gdb-put-breakpoint-icon (enabled bptno)
  (let ((start (- (line-beginning-position) 1))
	(end (+ (line-end-position) 1))
	(putstring (if enabled "B" "b"))
	(source-window (get-buffer-window (current-buffer) 0)))
    (add-text-properties
     0 1 '(help-echo "mouse-1: clear bkpt, mouse-3: enable/disable bkpt")
     putstring)
    (if enabled
	(add-text-properties
	 0 1 `(gdb-bptno ,bptno gdb-enabled t) putstring)
      (add-text-properties
       0 1 `(gdb-bptno ,bptno gdb-enabled nil) putstring))
    (gdb-remove-breakpoint-icons start end)
    (if (display-images-p)
	(if (>= (or left-fringe-width
		    (if source-window (car (window-fringes source-window)))
		    gdb-buffer-fringe-width) 8)
	    (gdb-put-string
	     nil (1+ start)
	     `(left-fringe breakpoint
			   ,(if enabled
				'breakpoint-enabled
			      'breakpoint-disabled))
	     'gdb-bptno bptno
	     'gdb-enabled enabled)
	  (when (< left-margin-width 2)
	    (save-current-buffer
	      (setq left-margin-width 2)
	      (if source-window
		  (set-window-margins
		   source-window
		   left-margin-width right-margin-width))))
	  (put-image
	   (if enabled
	       (or breakpoint-enabled-icon
		   (setq breakpoint-enabled-icon
			 (find-image `((:type xpm :data
					      ,breakpoint-xpm-data
					      :ascent 100 :pointer hand)
				       (:type pbm :data
					      ,breakpoint-enabled-pbm-data
					      :ascent 100 :pointer hand)))))
	     (or breakpoint-disabled-icon
		 (setq breakpoint-disabled-icon
		       (find-image `((:type xpm :data
					    ,breakpoint-xpm-data
					    :conversion disabled
					    :ascent 100 :pointer hand)
				     (:type pbm :data
					    ,breakpoint-disabled-pbm-data
					    :ascent 100 :pointer hand))))))
	   (+ start 1)
	   putstring
	   'left-margin))
      (when (< left-margin-width 2)
	(save-current-buffer
	  (setq left-margin-width 2)
	  (let ((window (get-buffer-window (current-buffer) 0)))
	    (if window
	      (set-window-margins
	       window left-margin-width right-margin-width)))))
      (gdb-put-string
       (propertize putstring
		   'face (if enabled 'breakpoint-enabled 'breakpoint-disabled))
       (1+ start)))))

(defun gdb-remove-breakpoint-icons (start end &optional remove-margin)
  (gdb-remove-strings start end)
  (if (display-images-p)
      (remove-images start end))
  (when remove-margin
    (setq left-margin-width 0)
    (let ((window (get-buffer-window (current-buffer) 0)))
      (if window
	  (set-window-margins
	   window left-margin-width right-margin-width)))))

(provide 'gdb-mi)

;;; gdb-mi.el ends here
