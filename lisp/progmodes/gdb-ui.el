;;; gdb-ui.el --- User Interface for running GDB

;; Author: Nick Roberts <nickrob@gnu.org>
;; Maintainer: FSF
;; Keywords: unix, tools

;; Copyright (C) 2002, 2003, 2004  Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This mode acts as a graphical user interface to GDB. You can interact with
;; GDB through the GUD buffer in the usual way, but there are also further
;; buffers which control the execution and describe the state of your program.
;; It separates the input/output of your program from that of GDB, if
;; required, and displays expressions and their current values in their own
;; buffers. It also uses features of Emacs 21 such as the display margin for
;; breakpoints, and the toolbar (see the GDB Graphical Interface section in
;; the Emacs info manual).

;; Start the debugger with M-x gdba.

;; This file has evolved from gdba.el from GDB 5.0 written by Tom Lord and Jim
;; Kingdon and uses GDB's annotation interface. You don't need to know about
;; annotations to use this mode as a debugger, but if you are interested
;; developing the mode itself, then see the Annotations section in the GDB
;; info manual. Some GDB/MI commands are also used through th CLI command
;; 'interpreter mi <mi-command>'.
;;
;; Known Bugs:
;;

;;; Code:

(require 'gud)

(defvar gdb-current-address "main" "Initialisation for Assembler buffer.")
(defvar gdb-previous-address nil)
(defvar gdb-previous-frame nil)
(defvar gdb-current-frame "main")
(defvar gdb-current-language nil)
(defvar gdb-view-source t "Non-nil means that source code can be viewed.")
(defvar gdb-selected-view 'source "Code type that user wishes to view.")
(defvar gdb-var-list nil "List of variables in watch window")
(defvar gdb-var-changed nil "Non-nil means that gdb-var-list has changed.")
(defvar gdb-buffer-type nil)
(defvar gdb-overlay-arrow-position nil)
(defvar gdb-variables '()
  "A list of variables that are local to the GUD buffer.")

;;;###autoload
(defun gdba (command-line)
  "Run gdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

If `gdb-many-windows' is nil (the default value) then gdb just
pops up the GUD buffer unless `gdb-show-main' is t. In this case
it starts with two windows: one displaying the GUD buffer and the
other with the source file with the main routine of the debugee.

If `gdb-many-windows' is t, regardless of the value of
`gdb-show-main', the layout below will appear unless
`gdb-use-inferior-io-buffer' is nil when the source buffer
occupies the full width of the frame. Keybindings are given in
relevant buffer.

Watch expressions appear in the speedbar/slowbar.

The following interactive lisp functions help control operation :

`gdb-many-windows'    - Toggle the number of windows gdb uses.
`gdb-restore-windows' - To restore the window layout.

See Info node `(emacs)GDB Graphical Interface' for a more
detailed description of this mode.


---------------------------------------------------------------------
                               GDB Toolbar
---------------------------------------------------------------------
 GUD buffer (I/O of GDB)          | Locals buffer
                                  |
                                  |
                                  |
---------------------------------------------------------------------
 Source buffer                    | Input/Output (of debugee) buffer
                                  | (comint-mode)
                                  |
                                  |
                                  |
                                  |
                                  |
                                  |
---------------------------------------------------------------------
 Stack buffer                     | Breakpoints buffer
 RET      gdb-frames-select       | SPC    gdb-toggle-breakpoint
                                  | RET    gdb-goto-breakpoint
                                  |   d    gdb-delete-breakpoint
---------------------------------------------------------------------
"
  ;;
  (interactive (list (gud-query-cmdline 'gdba)))
  ;;
  ;; Let's start with a basic gud-gdb buffer and then modify it a bit.
  (gdb command-line)
  (gdb-ann3))

(defvar gdb-debug-log nil)

(defcustom gdb-enable-debug-log nil
 "Non-nil means record the process input and output in `gdb-debug-log'."
  :type 'boolean
  :group 'gud)

(defcustom gdb-use-inferior-io-buffer nil
 "Non-nil means display output from the inferior in a separate buffer."
  :type 'boolean
  :group 'gud)

(defun gdb-ann3 ()
  (setq gdb-debug-log nil)
  (set (make-local-variable 'gud-minor-mode) 'gdba)
  (set (make-local-variable 'gud-marker-filter) 'gud-gdba-marker-filter)
  ;;
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
			  (gud-call "until %f:%l" arg)
			(save-excursion
			  (beginning-of-line)
			  (forward-char 2)
			  (gud-call "until *%a" arg)))
	   "\C-u" "Continue to current line or address.")

  (define-key gud-minor-mode-map [left-margin mouse-1]
    'gdb-mouse-toggle-breakpoint)
  (define-key gud-minor-mode-map [left-fringe mouse-1]
    'gdb-mouse-toggle-breakpoint)

  (setq comint-input-sender 'gdb-send)
  ;;
  ;; (re-)initialise
  (setq gdb-current-address "main")
  (setq gdb-previous-address nil)
  (setq gdb-previous-frame nil)
  (setq gdb-current-frame "main")
  (setq gdb-view-source t)
  (setq gdb-selected-view 'source)
  (setq gdb-var-list nil)
  (setq gdb-var-changed nil)
  (setq gdb-first-prompt nil)
  ;;
  (mapc 'make-local-variable gdb-variables)
  (setq gdb-buffer-type 'gdba)
  ;;
  (if gdb-use-inferior-io-buffer (gdb-clear-inferior-io))
  ;;
  (if (eq window-system 'w32)
      (gdb-enqueue-input (list "set new-console off\n" 'ignore)))
  (gdb-enqueue-input (list "set height 0\n" 'ignore))
  ;; find source file and compilation directory here
  (gdb-enqueue-input (list "server list main\n"   'ignore))   ; C program
  (gdb-enqueue-input (list "server list MAIN__\n" 'ignore))   ; Fortran program
  (gdb-enqueue-input (list "server info source\n" 'gdb-source-info))
  ;;
  (run-hooks 'gdba-mode-hook))

(defcustom gdb-use-colon-colon-notation nil
  "Non-nil means use FUNCTION::VARIABLE format to display variables in the
speedbar."
  :type 'boolean
  :group 'gud)

(defun gud-watch ()
  "Watch expression at point."
  (interactive)
  (require 'tooltip)
  (let ((expr (tooltip-identifier-from-point (point))))
    (if (and (string-equal gdb-current-language "c")
	     gdb-use-colon-colon-notation)
	(setq expr (concat gdb-current-frame "::" expr)))
    (catch 'already-watched
      (dolist (var gdb-var-list)
	(if (string-equal expr (car var)) (throw 'already-watched nil)))
      (set-text-properties 0 (length expr) nil expr)
      (gdb-enqueue-input
       (list (concat "server interpreter mi \"-var-create - * "  expr "\"\n")
	     `(lambda () (gdb-var-create-handler ,expr))))))
  (select-window (get-buffer-window gud-comint-buffer 'visible)))

(defun gdb-goto-info ()
  "Go to Emacs info node: GDB Graphical Interface."
  (interactive)
  (select-frame (make-frame))
  (require 'info)
  (Info-goto-node "(emacs)GDB Graphical Interface"))

(defconst gdb-var-create-regexp
"name=\"\\(.*?\\)\",numchild=\"\\(.*?\\)\",type=\"\\(.*?\\)\"")

(defun gdb-var-create-handler (expr)
  (with-current-buffer (gdb-get-create-buffer 'gdb-partial-output-buffer)
    (goto-char (point-min))
    (if (re-search-forward gdb-var-create-regexp nil t)
	(let ((var (list expr
			 (match-string 1)
			 (match-string 2)
			 (match-string 3)
			 nil nil)))
	  (push var gdb-var-list)
	  (setq speedbar-update-flag t)
	  (speedbar 1)
	  (if (equal (nth 2 var) "0")
	      (gdb-enqueue-input
	       (list (concat "server interpreter mi \"-var-evaluate-expression "
			     (nth 1 var) "\"\n")
		     `(lambda () (gdb-var-evaluate-expression-handler
				  ,(nth 1 var) nil))))
	    (setq gdb-var-changed t)))
      (if (re-search-forward "Undefined command" nil t)
	  (message "Watching expressions requires gdb 6.0 onwards")
	(message "No symbol %s in current context." expr)))))

(defun gdb-var-evaluate-expression-handler (varnum changed)
  (with-current-buffer (gdb-get-create-buffer 'gdb-partial-output-buffer)
    (goto-char (point-min))
    (re-search-forward ".*value=\"\\(.*?\\)\"" nil t)
    (catch 'var-found
      (let ((var-list nil) (num 0))
	(dolist (var gdb-var-list)
	  (if (string-equal varnum (cadr var))
	      (progn
		(if changed (setcar (nthcdr 5 var) t))
		(setcar (nthcdr 4 var) (match-string 1))
		(setcar (nthcdr num gdb-var-list) var)
		(throw 'var-found nil)))
	  (setq num (+ num 1))))))
  (setq gdb-var-changed t))

(defun gdb-var-list-children (varnum)
  (gdb-enqueue-input
   (list (concat "server interpreter mi \"-var-list-children "  varnum "\"\n")
	     `(lambda () (gdb-var-list-children-handler ,varnum)))))

(defconst gdb-var-list-children-regexp
"name=\"\\(.*?\\)\",exp=\"\\(.*?\\)\",numchild=\"\\(.*?\\)\"")

(defun gdb-var-list-children-handler (varnum)
  (with-current-buffer (gdb-get-create-buffer 'gdb-partial-output-buffer)
    (goto-char (point-min))
    (let ((var-list nil))
     (catch 'child-already-watched
       (dolist (var gdb-var-list)
	 (if (string-equal varnum (cadr var))
	     (progn
	       (push var var-list)
	       (while (re-search-forward gdb-var-list-children-regexp nil t)
		 (let ((varchild (list (match-string 2)
				       (match-string 1)
				       (match-string 3)
				       nil nil nil)))
		   (if (looking-at ",type=\"\\(.*?\\)\"")
		       (setcar (nthcdr 3 varchild) (match-string 1)))
		   (dolist (var1 gdb-var-list)
		     (if (string-equal (cadr var1) (cadr varchild))
			 (throw 'child-already-watched nil)))
		   (push varchild var-list)
		   (if (equal (nth 2 varchild) "0")
		       (gdb-enqueue-input
			(list
			 (concat
			  "server interpreter mi \"-var-evaluate-expression "
				 (nth 1 varchild) "\"\n")
			 `(lambda () (gdb-var-evaluate-expression-handler
				      ,(nth 1 varchild) nil))))))))
	   (push var var-list)))
       (setq gdb-var-list (nreverse var-list))))))

(defun gdb-var-update ()
  (if (not (member 'gdb-var-update (gdb-get-pending-triggers)))
      (progn
	(gdb-enqueue-input (list "server interpreter mi \"-var-update *\"\n"
				 'gdb-var-update-handler))
	(gdb-set-pending-triggers (cons 'gdb-var-update
					(gdb-get-pending-triggers))))))

(defconst gdb-var-update-regexp "name=\"\\(.*?\\)\"")

(defun gdb-var-update-handler ()
  (with-current-buffer (gdb-get-create-buffer 'gdb-partial-output-buffer)
    (goto-char (point-min))
    (while (re-search-forward gdb-var-update-regexp nil t)
	(let ((varnum (match-string 1)))
	  (gdb-enqueue-input
	   (list (concat "server interpreter mi \"-var-evaluate-expression "
			 varnum "\"\n")
		     `(lambda () (gdb-var-evaluate-expression-handler
				  ,varnum t)))))))
  (gdb-set-pending-triggers
   (delq 'gdb-var-update (gdb-get-pending-triggers))))

(defun gdb-var-delete ()
  "Delete watched expression from the speedbar."
  (interactive)
  (if (with-current-buffer gud-comint-buffer (eq gud-minor-mode 'gdba))
      (let ((text (speedbar-line-text)))
	(string-match "\\(\\S-+\\)" text)
	(let* ((expr (match-string 1 text))
	       (var (assoc expr gdb-var-list))
	       (varnum (cadr var)))
	  (unless (string-match "\\." varnum)
	    (gdb-enqueue-input
	     (list (concat "server interpreter mi \"-var-delete "
			   varnum "\"\n")
		   'ignore))
	    (setq gdb-var-list (delq var gdb-var-list))
	    (dolist (varchild gdb-var-list)
	      (if (string-match (concat (nth 1 var) "\\.") (nth 1 varchild))
		  (setq gdb-var-list (delq varchild gdb-var-list))))
	    (setq gdb-var-changed t))))))

(defun gdb-edit-value (text token indent)
  "Assign a value to a variable displayed in the speedbar"
  (let* ((var (nth (- (count-lines (point-min) (point)) 2) gdb-var-list))
	 (varnum (cadr var)) (value))
    (setq value (read-string "New value: "))
    (gdb-enqueue-input
     (list (concat "server interpreter mi \"-var-assign "
		   varnum " " value "\"\n")
	   'ignore))))

(defcustom gdb-show-changed-values t
  "Non-nil means use font-lock-warning-face to display values that have
recently changed in the speedbar."
  :type 'boolean
  :group 'gud)

(defun gdb-speedbar-expand-node (text token indent)
  "Expand the node the user clicked on.
TEXT is the text of the button we clicked on, a + or - item.
TOKEN is data related to this node.
INDENT is the current indentation depth."
  (cond ((string-match "+" text)        ;expand this node
	 (gdb-var-list-children token))
	((string-match "-" text)	;contract this node
	 (dolist (var gdb-var-list)
	   (if (string-match (concat token "\\.") (nth 1 var))
	       (setq gdb-var-list (delq var gdb-var-list))))
	 (setq gdb-var-changed t))))


;; ======================================================================
;;
;; In this world, there are gdb variables (of unspecified
;; representation) and buffers associated with those objects.
;; The list of  variables is built up by the expansions of
;; def-gdb-variable

(defmacro def-gdb-var (root-symbol &optional default doc)
  (let* ((root (symbol-name root-symbol))
	 (accessor (intern (concat "gdb-get-" root)))
	 (setter (intern (concat "gdb-set-" root)))
	 (name (intern (concat "gdb-" root))))
    `(progn
       (defvar ,name ,default ,doc)
       (if (not (memq ',name gdb-variables))
	   (push ',name gdb-variables))
       (defun ,accessor ()
	 (buffer-local-value ',name gud-comint-buffer))
       (defun ,setter (val)
	 (with-current-buffer gud-comint-buffer
	   (setq ,name val))))))

(def-gdb-var buffer-type nil
  "One of the symbols bound in `gdb-buffer-rules'.")

(def-gdb-var burst ""
  "A string of characters from gdb that have not yet been processed.")

(def-gdb-var input-queue ()
  "A list of gdb command objects.")

(def-gdb-var prompting nil
  "True when gdb is idle with no pending input.")

(def-gdb-var output-sink 'user
  "The disposition of the output of the current gdb command.
Possible values are these symbols:

    user -- gdb output should be copied to the GUD buffer
            for the user to see.

    inferior -- gdb output should be copied to the inferior-io buffer

    pre-emacs -- output should be ignored util the post-prompt
                 annotation is received.  Then the output-sink
		 becomes:...
    emacs -- output should be collected in the partial-output-buffer
	     for subsequent processing by a command.  This is the
	     disposition of output generated by commands that
	     gdb mode sends to gdb on its own behalf.
    post-emacs -- ignore output until the prompt annotation is
		  received, then go to USER disposition.
")

(def-gdb-var current-item nil
  "The most recent command item sent to gdb.")

(def-gdb-var pending-triggers '()
  "A list of trigger functions that have run later than their output
handlers.")

;; end of gdb variables

(defun gdb-get-target-string ()
  (with-current-buffer gud-comint-buffer
    gud-target-name))


;;
;; gdb buffers.
;;
;; Each buffer has a TYPE -- a symbol that identifies the function
;; of that particular buffer.
;;
;; The usual gdb interaction buffer is given the type `gdba' and
;; is constructed specially.
;;
;; Others are constructed by gdb-get-create-buffer and
;; named according to the rules set forth in the gdb-buffer-rules-assoc

(defvar gdb-buffer-rules-assoc '())

(defun gdb-get-buffer (key)
  "Return the gdb buffer tagged with type KEY.
The key should be one of the cars in `gdb-buffer-rules-assoc'."
  (save-excursion
    (gdb-look-for-tagged-buffer key (buffer-list))))

(defun gdb-get-create-buffer (key)
  "Create a new gdb  buffer of the type specified by KEY.
The key should be one of the cars in `gdb-buffer-rules-assoc'."
  (or (gdb-get-buffer key)
      (let* ((rules (assoc key gdb-buffer-rules-assoc))
	     (name (funcall (gdb-rules-name-maker rules)))
	     (new (get-buffer-create name)))
	(with-current-buffer new
	  ;; FIXME: This should be set after calling the function, since the
	  ;; function should run kill-all-local-variables.
	  (set (make-local-variable 'gdb-buffer-type) key)
	  (if (cdr (cdr rules))
	      (funcall (car (cdr (cdr rules)))))
	  (set (make-local-variable 'gud-comint-buffer) gud-comint-buffer)
	  (set (make-local-variable 'gud-minor-mode) 'gdba)
	  (set (make-local-variable 'tool-bar-map) gud-tool-bar-map)
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
(gdb-set-buffer-rules 'gdba 'error)

;;
;; Partial-output buffer : This accumulates output from a command executed on
;; behalf of emacs (rather than the user).
;;
(gdb-set-buffer-rules 'gdb-partial-output-buffer
		      'gdb-partial-output-name)

(defun gdb-partial-output-name ()
  (concat "*partial-output-"
	  (gdb-get-target-string)
	  "*"))


(gdb-set-buffer-rules 'gdb-inferior-io
		      'gdb-inferior-io-name
		      'gdb-inferior-io-mode)

(defun gdb-inferior-io-name ()
  (concat "*input/output of "
	  (gdb-get-target-string)
	  "*"))

(defvar gdb-inferior-io-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'gdb-inferior-io-interrupt)
    (define-key map "\C-c\C-z" 'gdb-inferior-io-stop)
    (define-key map "\C-c\C-\\" 'gdb-inferior-io-quit)
    (define-key map "\C-c\C-d" 'gdb-inferior-io-eof)
    map))

(define-derived-mode gdb-inferior-io-mode comint-mode "Debuggee I/O"
  "Major mode for gdb inferior-io."
  :syntax-table nil :abbrev-table nil
  ;; We want to use comint because it has various nifty and familiar
  ;; features.  We don't need a process, but comint wants one, so create
  ;; a dummy one.
  (make-comint-in-buffer
   (substring (buffer-name) 1 (- (length (buffer-name)) 1))
   (current-buffer) "hexl")
  (setq comint-input-sender 'gdb-inferior-io-sender))

(defun gdb-inferior-io-sender (proc string)
  ;; PROC is the pseudo-process created to satisfy comint.
  (with-current-buffer (process-buffer proc)
    (setq proc (get-buffer-process gud-comint-buffer))
    (process-send-string proc string)
    (process-send-string proc "\n")))

(defun gdb-inferior-io-interrupt ()
  "Interrupt the program being debugged."
  (interactive)
  (interrupt-process
   (get-buffer-process gud-comint-buffer) comint-ptyp))

(defun gdb-inferior-io-quit ()
  "Send quit signal to the program being debugged."
  (interactive)
  (quit-process
   (get-buffer-process gud-comint-buffer) comint-ptyp))

(defun gdb-inferior-io-stop ()
  "Stop the program being debugged."
  (interactive)
  (stop-process
   (get-buffer-process gud-comint-buffer) comint-ptyp))

(defun gdb-inferior-io-eof ()
  "Send end-of-file to the program being debugged."
  (interactive)
  (process-send-eof
   (get-buffer-process gud-comint-buffer)))


;;
;; gdb communications
;;

;; INPUT: things sent to gdb
;;
;; The queues are lists.  Each element is either a string (indicating user or
;; user-like input) or a list of the form:
;;
;;    (INPUT-STRING  HANDLER-FN)
;;
;; The handler function will be called from the partial-output buffer when the
;; command completes.  This is the way to write commands which invoke gdb
;; commands autonomously.
;;
;; These lists are consumed tail first.
;;

(defun gdb-send (proc string)
  "A comint send filter for gdb.
This filter may simply queue output for a later time."
  (if gud-running
      (process-send-string proc (concat string "\n"))
    (gdb-enqueue-input (concat string "\n"))))

;; Note: Stuff enqueued here will be sent to the next prompt, even if it
;; is a query, or other non-top-level prompt.

(defun gdb-enqueue-input (item)
  (if (gdb-get-prompting)
      (progn
	(gdb-send-item item)
	(gdb-set-prompting nil))
    (gdb-set-input-queue
     (cons item (gdb-get-input-queue)))))

(defun gdb-dequeue-input ()
  (let ((queue (gdb-get-input-queue)))
    (and queue
	 (let ((last (car (last queue))))
	   (unless (nbutlast queue) (gdb-set-input-queue '()))
	   last))))


;;
;; output -- things gdb prints to emacs
;;
;; GDB output is a stream interrupted by annotations.
;; Annotations can be recognized by their beginning
;; with \C-j\C-z\C-z<tag><opt>\C-j
;;
;; The tag is a string obeying symbol syntax.
;;
;; The optional part `<opt>' can be either the empty string
;; or a space followed by more data relating to the annotation.
;; For example, the SOURCE annotation is followed by a filename,
;; line number and various useless goo.  This data must not include
;; any newlines.
;;

(defcustom gud-gdba-command-name "gdb -annotate=3"
  "Default command to execute an executable under the GDB-UI debugger."
  :type 'string
  :group 'gud)

(defvar gdb-annotation-rules
  '(("pre-prompt" gdb-pre-prompt)
    ("prompt" gdb-prompt)
    ("commands" gdb-subprompt)
    ("overload-choice" gdb-subprompt)
    ("query" gdb-subprompt)
    ("nquery" gdb-subprompt)
    ("prompt-for-continue" gdb-subprompt)
    ("post-prompt" gdb-post-prompt)
    ("source" gdb-source)
    ("starting" gdb-starting)
    ("exited" gdb-stopping)
    ("signalled" gdb-stopping)
    ("signal" gdb-stopping)
    ("breakpoint" gdb-stopping)
    ("watchpoint" gdb-stopping)
    ("frame-begin" gdb-frame-begin)
    ("stopped" gdb-stopped)
    ) "An assoc mapping annotation tags to functions which process them.")

(defconst gdb-source-spec-regexp
  "\\(.*\\):\\([0-9]*\\):[0-9]*:[a-z]*:\\(0x[a-f0-9]*\\)")

;; Do not use this except as an annotation handler.
(defun gdb-source (args)
  (string-match gdb-source-spec-regexp args)
  ;; Extract the frame position from the marker.
  (setq gud-last-frame
	(cons
	 (match-string 1 args)
	 (string-to-int (match-string 2 args))))
  (setq gdb-current-address (match-string 3 args))
  (setq gdb-view-source t)
  ;; cover for auto-display output which comes *before*
  ;; stopped annotation
  (if (eq (gdb-get-output-sink) 'inferior) (gdb-set-output-sink 'user)))

(defun gdb-send-item (item)
  (if gdb-enable-debug-log (push (cons 'send item) gdb-debug-log))
  (gdb-set-current-item item)
  (if (stringp item)
      (progn
	(gdb-set-output-sink 'user)
	(process-send-string (get-buffer-process gud-comint-buffer) item))
    (progn
      (gdb-clear-partial-output)
      (gdb-set-output-sink 'pre-emacs)
      (process-send-string (get-buffer-process gud-comint-buffer)
			   (car item)))))

(defun gdb-pre-prompt (ignored)
  "An annotation handler for `pre-prompt'. This terminates the collection of
output from a previous command if that happens to be in effect."
  (let ((sink (gdb-get-output-sink)))
    (cond
     ((eq sink 'user) t)
     ((eq sink 'emacs)
      (gdb-set-output-sink 'post-emacs))
     (t
      (gdb-set-output-sink 'user)
      (error "Phase error in gdb-pre-prompt (got %s)" sink)))))

(defun gdb-prompt (ignored)
  "An annotation handler for `prompt'.
This sends the next command (if any) to gdb."
  (when gdb-first-prompt (gdb-ann3))
  (let ((sink (gdb-get-output-sink)))
    (cond
     ((eq sink 'user) t)
     ((eq sink 'post-emacs)
      (gdb-set-output-sink 'user)
      (let ((handler
	     (car (cdr (gdb-get-current-item)))))
	(with-current-buffer (gdb-get-create-buffer 'gdb-partial-output-buffer)
	  (funcall handler))))
     (t
      (gdb-set-output-sink 'user)
      (error "Phase error in gdb-prompt (got %s)" sink))))
  (let ((input (gdb-dequeue-input)))
    (if input
	(gdb-send-item input)
      (progn
	(gdb-set-prompting t)
	(gud-display-frame)))))

(defun gdb-subprompt (ignored)
  "An annotation handler for non-top-level prompts."
  (gdb-set-prompting t))

(defun gdb-starting (ignored)
  "An annotation handler for `starting'.  This says that I/O for the
subprocess is now the program being debugged, not GDB."
  (let ((sink (gdb-get-output-sink)))
    (cond
     ((eq sink 'user)
      (progn
	(setq gud-running t)
	(if gdb-use-inferior-io-buffer
	    (gdb-set-output-sink 'inferior))))
     (t (error "Unexpected `starting' annotation")))))

(defun gdb-stopping (ignored)
  "An annotation handler for `exited' and other annotations which say that I/O
for the subprocess is now GDB, not the program being debugged."
  (if gdb-use-inferior-io-buffer
      (let ((sink (gdb-get-output-sink)))
	(cond
	 ((eq sink 'inferior)
	  (gdb-set-output-sink 'user))
	 (t (error "Unexpected stopping annotation"))))))

(defun gdb-frame-begin (ignored)
  (let ((sink (gdb-get-output-sink)))
    (cond
     ((eq sink 'inferior)
      (gdb-set-output-sink 'user))
     ((eq sink 'user) t)
     ((eq sink 'emacs) t)
     (t (error "Unexpected frame-begin annotation (%S)" sink)))))

(defun gdb-stopped (ignored)
  "An annotation handler for `stopped'.  It is just like gdb-stopping, except
that if we already set the output sink to 'user in gdb-stopping, that is fine."
  (setq gud-running nil)
  (let ((sink (gdb-get-output-sink)))
    (cond
     ((eq sink 'inferior)
      (gdb-set-output-sink 'user))
     ((eq sink 'user) t)
     (t (error "Unexpected stopped annotation")))))

(defun gdb-post-prompt (ignored)
  "An annotation handler for `post-prompt'. This begins the collection of
output from the current command if that happens to be appropriate."
  (if (not (gdb-get-pending-triggers))
      (progn
	(gdb-get-current-frame)
	(gdb-invalidate-frames)
	(gdb-invalidate-breakpoints)
	(gdb-invalidate-assembler)
	(gdb-invalidate-registers)
	(gdb-invalidate-locals)
	(gdb-invalidate-threads)
	(unless (eq system-type 'darwin) ;Breaks on Darwin's GDB-5.3.
	  ;; FIXME: with GDB-6 on Darwin, this might very well work.
	  (dolist (frame (frame-list))
	    (when (string-equal (frame-parameter frame 'name) "Speedbar")
	      (setq gdb-var-changed t)    ; force update
	      (dolist (var gdb-var-list)
		(setcar (nthcdr 5 var) nil))))
	  (gdb-var-update))))
  (let ((sink (gdb-get-output-sink)))
    (cond
     ((eq sink 'user) t)
     ((eq sink 'pre-emacs)
      (gdb-set-output-sink 'emacs))
     (t
      (gdb-set-output-sink 'user)
      (error "Phase error in gdb-post-prompt (got %s)" sink)))))

(defun gud-gdba-marker-filter (string)
  "A gud marker filter for gdb. Handle a burst of output from GDB."
  (if gdb-enable-debug-log (push (cons 'recv string) gdb-debug-log))
  ;; Recall the left over gud-marker-acc from last time
  (setq gud-marker-acc (concat gud-marker-acc string))
  ;; Start accumulating output for the GUD buffer
  (let ((output ""))
    ;;
    ;; Process all the complete markers in this chunk.
    (while (string-match "\n\032\032\\(.*\\)\n" gud-marker-acc)
      (let ((annotation (match-string 1 gud-marker-acc)))
	;;
	;; Stuff prior to the match is just ordinary output.
	;; It is either concatenated to OUTPUT or directed
	;; elsewhere.
	(setq output
	      (gdb-concat-output
	       output
	       (substring gud-marker-acc 0 (match-beginning 0))))
        ;;
	;; Take that stuff off the gud-marker-acc.
	(setq gud-marker-acc (substring gud-marker-acc (match-end 0)))
        ;;
	;; Parse the tag from the annotation, and maybe its arguments.
	(string-match "\\(\\S-*\\) ?\\(.*\\)" annotation)
	(let* ((annotation-type (match-string 1 annotation))
	       (annotation-arguments (match-string 2 annotation))
	       (annotation-rule (assoc annotation-type
				       gdb-annotation-rules)))
	  ;; Call the handler for this annotation.
	  (if annotation-rule
	      (funcall (car (cdr annotation-rule))
		       annotation-arguments)
	    ;; Else the annotation is not recognized.  Ignore it silently,
	    ;; so that GDB can add new annotations without causing
	    ;; us to blow up.
	    ))))
    ;;
    ;; Does the remaining text end in a partial line?
    ;; If it does, then keep part of the gud-marker-acc until we get more.
    (if (string-match "\n\\'\\|\n\032\\'\\|\n\032\032.*\\'"
		      gud-marker-acc)
	(progn
	  ;; Everything before the potential marker start can be output.
	  (setq output
		(gdb-concat-output output
				   (substring gud-marker-acc 0
					      (match-beginning 0))))
	  ;;
	  ;; Everything after, we save, to combine with later input.
	  (setq gud-marker-acc (substring gud-marker-acc (match-beginning 0))))
      ;;
      ;; In case we know the gud-marker-acc contains no partial annotations:
      (progn
	(setq output (gdb-concat-output output gud-marker-acc))
	(setq gud-marker-acc "")))
    output))

(defun gdb-concat-output (so-far new)
  (let ((sink (gdb-get-output-sink )))
    (cond
     ((eq sink 'user) (concat so-far new))
     ((or (eq sink 'pre-emacs) (eq sink 'post-emacs)) so-far)
     ((eq sink 'emacs)
      (gdb-append-to-partial-output new)
      so-far)
     ((eq sink 'inferior)
      (gdb-append-to-inferior-io new)
      so-far)
     (t (error "Bogon output sink %S" sink)))))

(defun gdb-append-to-partial-output (string)
  (with-current-buffer (gdb-get-create-buffer 'gdb-partial-output-buffer)
    (goto-char (point-max))
    (insert string)))

(defun gdb-clear-partial-output ()
  (with-current-buffer (gdb-get-create-buffer 'gdb-partial-output-buffer)
    (erase-buffer)))

(defun gdb-append-to-inferior-io (string)
  (with-current-buffer (gdb-get-create-buffer 'gdb-inferior-io)
    (goto-char (point-max))
    (insert-before-markers string))
  (if (not (string-equal string ""))
      (gdb-display-buffer (gdb-get-create-buffer 'gdb-inferior-io))))

(defun gdb-clear-inferior-io ()
  (with-current-buffer (gdb-get-create-buffer 'gdb-inferior-io)
    (erase-buffer)))


;; One trick is to have a command who's output is always available in a buffer
;; of it's own, and is always up to date.  We build several buffers of this
;; type.
;;
;; There are two aspects to this: gdb has to tell us when the output for that
;; command might have changed, and we have to be able to run the command
;; behind the user's back.
;;
;; The output phasing associated with the variable gdb-output-sink
;; help us to run commands behind the user's back.
;;
;; Below is the code for specificly managing buffers of output from one
;; command.
;;

;; The trigger function is suitable for use in the assoc GDB-ANNOTATION-RULES
;; It adds an input for the command we are tracking.  It should be the
;; annotation rule binding of whatever gdb sends to tell us this command
;; might have changed it's output.
;;
;; NAME is the function name.  DEMAND-PREDICATE tests if output is really needed.
;; GDB-COMMAND is a string of such.  OUTPUT-HANDLER is the function bound to the
;; input in the input queue (see comment about ``gdb communications'' above).

(defmacro def-gdb-auto-update-trigger (name demand-predicate gdb-command
					    output-handler)
  `(defun ,name (&optional ignored)
     (if (and (,demand-predicate)
	      (not (member ',name
			   (gdb-get-pending-triggers))))
	 (progn
	   (gdb-enqueue-input
	    (list ,gdb-command ',output-handler))
	   (gdb-set-pending-triggers
	    (cons ',name
		  (gdb-get-pending-triggers)))))))

(defmacro def-gdb-auto-update-handler (name trigger buf-key custom-defun)
  `(defun ,name ()
     (gdb-set-pending-triggers
      (delq ',trigger
	    (gdb-get-pending-triggers)))
     (let ((buf (gdb-get-buffer ',buf-key)))
       (and buf
	    (with-current-buffer buf
	      (let ((p (point))
		    (buffer-read-only nil))
		(erase-buffer)
		(insert-buffer-substring (gdb-get-create-buffer
					  'gdb-partial-output-buffer))
		(goto-char p)))))
     ;; put customisation here
     (,custom-defun)))

(defmacro def-gdb-auto-updated-buffer (buffer-key trigger-name gdb-command
						  output-handler-name custom-defun)
  `(progn
     (def-gdb-auto-update-trigger ,trigger-name
       ;; The demand predicate:
       (lambda () (gdb-get-buffer ',buffer-key))
       ,gdb-command
       ,output-handler-name)
     (def-gdb-auto-update-handler ,output-handler-name
       ,trigger-name ,buffer-key ,custom-defun)))


;;
;; Breakpoint buffer : This displays the output of `info breakpoints'.
;;
(gdb-set-buffer-rules 'gdb-breakpoints-buffer
		      'gdb-breakpoints-buffer-name
		      'gdb-breakpoints-mode)

(def-gdb-auto-updated-buffer gdb-breakpoints-buffer
  ;; This defines the auto update rule for buffers of type
  ;; `gdb-breakpoints-buffer'.
  ;;
  ;; It defines a function to serve as the annotation handler that
  ;; handles the `foo-invalidated' message.  That function is called:
  gdb-invalidate-breakpoints
  ;;
  ;; To update the buffer, this command is sent to gdb.
  "server info breakpoints\n"
  ;;
  ;; This also defines a function to be the handler for the output
  ;; from the command above.  That function will copy the output into
  ;; the appropriately typed buffer.  That function will be called:
  gdb-info-breakpoints-handler
  ;; buffer specific functions
  gdb-info-breakpoints-custom)

(defvar gdb-cdir nil "Compilation directory.")

(defconst breakpoint-xpm-data "/* XPM */
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
  "Icon for enabled breakpoint in display margin")

(defvar breakpoint-disabled-icon nil
  "Icon for disabled breakpoint in display margin")

(defvar breakpoint-bitmap nil
  "Bitmap for breakpoint in fringe")

(defface breakpoint-enabled-bitmap-face
  '((t
     :inherit fringe
     :foreground "red"))
  "Face for enabled breakpoint icon in fringe.")

(defface breakpoint-disabled-bitmap-face
  '((t
     :inherit fringe
     :foreground "grey60"))
  "Face for disabled breakpoint icon in fringe.")


;;-put breakpoint icons in relevant margins (even those set in the GUD buffer)
(defun gdb-info-breakpoints-custom ()
  (let ((flag)(address))
    ;;
    ;; remove all breakpoint-icons in source buffers but not assembler buffer
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(if (and (eq gud-minor-mode 'gdba)
		 (not (string-match "^\*" (buffer-name))))
	    (gdb-remove-breakpoint-icons (point-min) (point-max)))))
    (with-current-buffer (gdb-get-buffer 'gdb-breakpoints-buffer)
      (save-excursion
	(goto-char (point-min))
	(while (< (point) (- (point-max) 1))
	  (forward-line 1)
	  (if (looking-at "[^\t].*breakpoint")
	      (progn
		(looking-at "[0-9]*\\s-*\\S-*\\s-*\\S-*\\s-*\\(.\\)")
		(setq flag (char-after (match-beginning 1)))
		(beginning-of-line)
		(if (re-search-forward "in\\s-+\\S-+\\s-+at\\s-+" nil t)
		    (progn
		      (looking-at "\\(\\S-*\\):\\([0-9]+\\)")
		      (let ((line (match-string 2)) (buffer-read-only nil)
			    (file (match-string 1)))
			(add-text-properties (point-at-bol) (point-at-eol)
			 '(mouse-face highlight
			   help-echo "mouse-2, RET: visit breakpoint"))
			(with-current-buffer
			    (find-file-noselect
			     (if (file-exists-p file) file
			       (expand-file-name file gdb-cdir)))
			  (save-current-buffer
			    (set (make-local-variable 'gud-minor-mode) 'gdba)
			    (set (make-local-variable 'tool-bar-map)
				 gud-tool-bar-map))
			  ;; only want one breakpoint icon at each location
			  (save-excursion
			    (goto-line (string-to-number line))
			    (gdb-put-breakpoint-icon (eq flag ?y)))))))))
	  (end-of-line)))))
  (if (gdb-get-buffer 'gdb-assembler-buffer) (gdb-assembler-custom)))

(defun gdb-mouse-toggle-breakpoint (event)
  "Toggle breakpoint in left fringe/margin with mouse click"
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((posn (event-end event)))
    (if (numberp (posn-point posn))
	(with-selected-window (posn-window posn)
	  (save-excursion
	    (goto-char (posn-point posn))
	    (if (or (posn-object posn)
		    (and breakpoint-bitmap
			 (eq (car (fringe-bitmaps-at-pos (posn-point posn)))
			     breakpoint-bitmap)))
		(gud-remove nil)
	      (gud-break nil)))))))

(defun gdb-breakpoints-buffer-name ()
  (with-current-buffer gud-comint-buffer
    (concat "*breakpoints of " (gdb-get-target-string) "*")))

(defun gdb-display-breakpoints-buffer ()
  "Display status of user-settable breakpoints."
  (interactive)
  (gdb-display-buffer
   (gdb-get-create-buffer 'gdb-breakpoints-buffer)))

(defconst gdb-frame-parameters
  '((height . 12) (width . 60)
    (unsplittable . t)
    (tool-bar-lines . nil)
    (menu-bar-lines . nil)
    (minibuffer . nil)))

(defun gdb-frame-breakpoints-buffer ()
  "Display status of user-settable breakpoints in a new frame."
  (interactive)
  (select-frame (make-frame gdb-frame-parameters))
  (switch-to-buffer (gdb-get-create-buffer 'gdb-breakpoints-buffer))
  (set-window-dedicated-p (selected-window) t))

(defvar gdb-breakpoints-mode-map
  (let ((map (make-sparse-keymap))
	(menu (make-sparse-keymap "Breakpoints")))
    (define-key menu [toggle] '("Toggle" . gdb-toggle-breakpoint))
    (define-key menu [delete] '("Delete" . gdb-delete-breakpoint))
    (define-key menu [goto] '("Goto"   . gdb-goto-breakpoint))

    (suppress-keymap map)
    (define-key map [menu-bar breakpoints] (cons "Breakpoints" menu))
    (define-key map " " 'gdb-toggle-breakpoint)
    (define-key map "d" 'gdb-delete-breakpoint)
    (define-key map "\r" 'gdb-goto-breakpoint)
    (define-key map [mouse-2] 'gdb-mouse-goto-breakpoint)
    map))

(defun gdb-breakpoints-mode ()
  "Major mode for gdb breakpoints.

\\{gdb-breakpoints-mode-map}"
  (setq major-mode 'gdb-breakpoints-mode)
  (setq mode-name "Breakpoints")
  (use-local-map gdb-breakpoints-mode-map)
  (setq buffer-read-only t)
  (gdb-invalidate-breakpoints))

(defun gdb-toggle-breakpoint ()
  "Enable/disable the breakpoint at current line."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (if (not (looking-at "\\([0-9]+\\).*point\\s-*\\S-*\\s-*\\(.\\)"))
	(error "Not recognized as break/watchpoint line")
      (gdb-enqueue-input
       (list
	(concat
	 (if (eq ?y (char-after (match-beginning 2)))
	     "server disable "
	   "server enable ")
	 (match-string 1) "\n")
	'ignore)))))

(defun gdb-delete-breakpoint ()
  "Delete the breakpoint at current line."
  (interactive)
  (beginning-of-line 1)
  (if (not (looking-at "\\([0-9]+\\).*point\\s-*\\S-*\\s-*\\(.\\)"))
      (error "Not recognized as break/watchpoint line")
    (gdb-enqueue-input
     (list (concat "server delete " (match-string 1) "\n") 'ignore))))

(defun gdb-goto-breakpoint ()
  "Display the breakpoint location specified at current line."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (re-search-forward "in\\s-+\\S-+\\s-+at\\s-+" nil t)
    (looking-at "\\(\\S-*\\):\\([0-9]+\\)"))
  (if (match-string 2)
      (let ((line (match-string 2))
	    (file (match-string 1)))
	(save-selected-window
	  (let* ((buf (find-file-noselect (if (file-exists-p file)
					      file
					    (expand-file-name file gdb-cdir))))
		 (window (gdb-display-buffer buf)))
		 (with-current-buffer buf
		   (goto-line (string-to-number line))
		   (set-window-point window (point))))))))

(defun gdb-mouse-goto-breakpoint (event)
  "Display the breakpoint location that you click on."
  (interactive "e")
  (mouse-set-point event)
  (gdb-goto-breakpoint))

;;
;; Frames buffer.  This displays a perpetually correct bactracktrace
;; (from the command `where').
;;
;; Alas, if your stack is deep, it is costly.
;;
(gdb-set-buffer-rules 'gdb-stack-buffer
		      'gdb-stack-buffer-name
		      'gdb-frames-mode)

(def-gdb-auto-updated-buffer gdb-stack-buffer
  gdb-invalidate-frames
  "server where\n"
  gdb-info-frames-handler
  gdb-info-frames-custom)

(defun gdb-info-frames-custom ()
  (with-current-buffer (gdb-get-buffer 'gdb-stack-buffer)
    (save-excursion
      (let ((buffer-read-only nil))
	(goto-char (point-min))
	(while (< (point) (point-max))
	  (add-text-properties (point-at-bol) (point-at-eol)
			     '(mouse-face highlight
			       help-echo "mouse-2, RET: Select frame"))
	  (beginning-of-line)
	  (when (and (or (looking-at "^#[0-9]*\\s-*\\S-* in \\(\\S-*\\)")
			 (looking-at "^#[0-9]*\\s-*\\(\\S-*\\)"))
		     (equal (match-string 1) gdb-current-frame))
	    (put-text-property (point-at-bol) (point-at-eol)
			       'face '(:inverse-video t)))
	  (forward-line 1))))))

(defun gdb-stack-buffer-name ()
  (with-current-buffer gud-comint-buffer
    (concat "*stack frames of " (gdb-get-target-string) "*")))

(defun gdb-display-stack-buffer ()
  "Display backtrace of current stack."
  (interactive)
  (gdb-display-buffer
   (gdb-get-create-buffer 'gdb-stack-buffer)))

(defun gdb-frame-stack-buffer ()
  "Display backtrace of current stack in a new frame."
  (interactive)
  (select-frame (make-frame gdb-frame-parameters))
  (switch-to-buffer (gdb-get-create-buffer 'gdb-stack-buffer))
  (set-window-dedicated-p (selected-window) t))

(defvar gdb-frames-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\r" 'gdb-frames-select)
    (define-key map [mouse-2] 'gdb-frames-mouse-select)
    map))

(defun gdb-frames-mode ()
  "Major mode for gdb frames.

\\{gdb-frames-mode-map}"
  (setq major-mode 'gdb-frames-mode)
  (setq mode-name "Frames")
  (setq buffer-read-only t)
  (use-local-map gdb-frames-mode-map)
  (font-lock-mode -1)
  (gdb-invalidate-frames))

(defun gdb-get-frame-number ()
  (save-excursion
    (let* ((pos (re-search-backward "^#\\([0-9]*\\)" nil t))
	   (n (or (and pos (match-string-no-properties 1)) "0")))
      n)))

(defun gdb-frames-select ()
  "Select the frame and display the relevant source."
  (interactive)
  (gdb-enqueue-input
   (list (concat "server frame " (gdb-get-frame-number) "\n") 'ignore))
  (gud-display-frame))

(defun gdb-frames-mouse-select (event)
  "Select the frame you click on and display the relevant source."
  (interactive "e")
  (mouse-set-point event)
  (gdb-frames-select))

;;
;; Threads buffer.  This displays a selectable thread list.
;;
(gdb-set-buffer-rules 'gdb-threads-buffer
		      'gdb-threads-buffer-name
		      'gdb-threads-mode)

(def-gdb-auto-updated-buffer gdb-threads-buffer
  gdb-invalidate-threads
  "server info threads\n"
  gdb-info-threads-handler
  gdb-info-threads-custom)

(defun gdb-info-threads-custom ()
  (with-current-buffer (gdb-get-buffer 'gdb-threads-buffer)
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (< (point) (point-max))
	(add-text-properties (point-at-bol) (point-at-eol)
			     '(mouse-face highlight
			       help-echo "mouse-2, RET: select thread"))
	(forward-line 1)))))

(defun gdb-threads-buffer-name ()
  (with-current-buffer gud-comint-buffer
    (concat "*threads of " (gdb-get-target-string) "*")))

(defun gdb-display-threads-buffer ()
  "Display IDs of currently known threads."
  (interactive)
  (gdb-display-buffer
   (gdb-get-create-buffer 'gdb-threads-buffer)))

(defun gdb-frame-threads-buffer ()
  "Display IDs of currently known threads in a new frame."
  (interactive)
  (select-frame (make-frame gdb-frame-parameters))
  (switch-to-buffer (gdb-get-create-buffer 'gdb-threads-buffer))
  (set-window-dedicated-p (selected-window) t))

(defvar gdb-threads-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\r" 'gdb-threads-select)
    (define-key map [mouse-2] 'gdb-threads-mouse-select)
    map))

(defun gdb-threads-mode ()
  "Major mode for gdb frames.

\\{gdb-frames-mode-map}"
  (setq major-mode 'gdb-threads-mode)
  (setq mode-name "Threads")
  (setq buffer-read-only t)
  (use-local-map gdb-threads-mode-map)
  (gdb-invalidate-threads))

(defun gdb-get-thread-number ()
  (save-excursion
    (re-search-backward "^\\s-*\\([0-9]*\\)" nil t)
    (match-string-no-properties 1)))

(defun gdb-threads-select ()
  "Select the thread and display the relevant source."
  (interactive)
  (gdb-enqueue-input
   (list (concat "thread " (gdb-get-thread-number) "\n") 'ignore))
  (gud-display-frame))

(defun gdb-threads-mouse-select (event)
  "Select the thread you click on and display the relevant source."
  (interactive "e")
  (mouse-set-point event)
  (gdb-threads-select))

;;
;; Registers buffer.
;;
(gdb-set-buffer-rules 'gdb-registers-buffer
		      'gdb-registers-buffer-name
		      'gdb-registers-mode)

(def-gdb-auto-updated-buffer gdb-registers-buffer
  gdb-invalidate-registers
  "server info registers\n"
  gdb-info-registers-handler
  gdb-info-registers-custom)

(defun gdb-info-registers-custom ())

(defvar gdb-registers-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    map))

(defun gdb-registers-mode ()
  "Major mode for gdb registers.

\\{gdb-registers-mode-map}"
  (setq major-mode 'gdb-registers-mode)
  (setq mode-name "Registers")
  (setq buffer-read-only t)
  (use-local-map gdb-registers-mode-map)
  (gdb-invalidate-registers))

(defun gdb-registers-buffer-name ()
  (with-current-buffer gud-comint-buffer
    (concat "*registers of " (gdb-get-target-string) "*")))

(defun gdb-display-registers-buffer ()
  "Display integer register contents."
  (interactive)
  (gdb-display-buffer
   (gdb-get-create-buffer 'gdb-registers-buffer)))

(defun gdb-frame-registers-buffer ()
  "Display integer register contents in a new frame."
  (interactive)
  (select-frame (make-frame gdb-frame-parameters))
  (switch-to-buffer (gdb-get-create-buffer 'gdb-registers-buffer))
  (set-window-dedicated-p (selected-window) t))

;;
;; Locals buffer.
;;
(gdb-set-buffer-rules 'gdb-locals-buffer
		      'gdb-locals-buffer-name
		      'gdb-locals-mode)

(def-gdb-auto-updated-buffer gdb-locals-buffer
  gdb-invalidate-locals
  "server info locals\n"
  gdb-info-locals-handler
  gdb-info-locals-custom)

;; Abbreviate for arrays and structures.
;; These can be expanded using gud-display.
(defun gdb-info-locals-handler nil
  (gdb-set-pending-triggers (delq 'gdb-invalidate-locals
				  (gdb-get-pending-triggers)))
  (let ((buf (gdb-get-buffer 'gdb-partial-output-buffer)))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (re-search-forward "^ .*\n" nil t)
	(replace-match "" nil nil))
      (goto-char (point-min))
      (while (re-search-forward "{[-0-9, {}\]*\n" nil t)
	(replace-match "(array);\n" nil nil))
      (goto-char (point-min))
      (while (re-search-forward "{.*=.*\n" nil t)
	(replace-match "(structure);\n" nil nil))))
  (let ((buf (gdb-get-buffer 'gdb-locals-buffer)))
    (and buf (with-current-buffer buf
	       (let ((p (point))
		     (buffer-read-only nil))
		 (delete-region (point-min) (point-max))
		 (insert-buffer-substring (gdb-get-create-buffer
					   'gdb-partial-output-buffer))
		 (goto-char p)))))
  (run-hooks 'gdb-info-locals-hook))

(defun gdb-info-locals-custom ()
  nil)

(defvar gdb-locals-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    map))

(defun gdb-locals-mode ()
  "Major mode for gdb locals.

\\{gdb-locals-mode-map}"
  (setq major-mode 'gdb-locals-mode)
  (setq mode-name "Locals")
  (setq buffer-read-only t)
  (use-local-map gdb-locals-mode-map)
  (gdb-invalidate-locals))

(defun gdb-locals-buffer-name ()
  (with-current-buffer gud-comint-buffer
    (concat "*locals of " (gdb-get-target-string) "*")))

(defun gdb-display-locals-buffer ()
  "Display local variables of current stack and their values."
  (interactive)
  (gdb-display-buffer
   (gdb-get-create-buffer 'gdb-locals-buffer)))

(defun gdb-frame-locals-buffer ()
  "Display local variables of current stack and their values in a new frame."
  (interactive)
  (select-frame (make-frame gdb-frame-parameters))
  (switch-to-buffer (gdb-get-create-buffer 'gdb-locals-buffer))
  (set-window-dedicated-p (selected-window) t))


;;;; Window management

;;; The way we abuse the dedicated-p flag is pretty gross, but seems
;;; to do the right thing.  Seeing as there is no way for Lisp code to
;;; get at the use_time field of a window, I'm not sure there exists a
;;; more elegant solution without writing C code.

(defun gdb-display-buffer (buf &optional size)
  (let ((must-split nil)
	(answer nil))
    (unwind-protect
	(progn
	  (walk-windows
	   #'(lambda (win)
	      (if (eq gud-comint-buffer (window-buffer win))
		  (set-window-dedicated-p win t))))
	  (setq answer (get-buffer-window buf 'visible))
	  (if (not answer)
	      (let ((window (get-lru-window 'visible)))
		(if window
		    (progn
		      (set-window-buffer window buf)
		      (setq answer window))
		  (setq must-split t)))))
      (walk-windows
       #'(lambda (win)
	  (if (eq gud-comint-buffer (window-buffer win))
	      (set-window-dedicated-p win nil)))))
    (if must-split
	(let* ((largest (get-largest-window 'visible))
	       (cur-size (window-height largest))
	       (new-size (and size (< size cur-size) (- cur-size size))))
	  (setq answer (split-window largest new-size))
	  (set-window-buffer answer buf)))
    answer))

(defun gdb-display-source-buffer (buffer)
  (if (eq gdb-selected-view 'source)
	(gdb-display-buffer buffer)
    (gdb-display-buffer (gdb-get-buffer 'gdb-assembler-buffer)))
    (get-buffer-window buffer 'visible))


;;; Shared keymap initialization:

(let ((menu (make-sparse-keymap "GDB-Frames")))
  (define-key gud-menu-map [frames]
    `(menu-item "GDB-Frames" ,menu :visible (eq gud-minor-mode 'gdba)))
  (define-key menu [gdb] '("Gdb" . gdb-frame-gdb-buffer))
  (define-key menu [threads] '("Threads" . gdb-frame-threads-buffer))
  (define-key menu [registers] '("Registers" . gdb-frame-registers-buffer))
  (define-key menu [locals] '("Locals" . gdb-frame-locals-buffer))
  (define-key menu [frames] '("Stack" . gdb-frame-stack-buffer))
  (define-key menu [breakpoints] '("Breakpoints" . gdb-frame-breakpoints-buffer))
;  (define-key menu [assembler] '("Machine" . gdb-frame-assembler-buffer))
)

(let ((menu (make-sparse-keymap "GDB-Windows")))
  (define-key gud-menu-map [displays]
    `(menu-item "GDB-Windows" ,menu :visible (eq gud-minor-mode 'gdba)))
  (define-key menu [gdb] '("Gdb" . gdb-display-gdb-buffer))
  (define-key menu [threads] '("Threads" . gdb-display-threads-buffer))
  (define-key menu [registers] '("Registers" . gdb-display-registers-buffer))
  (define-key menu [locals] '("Locals" . gdb-display-locals-buffer))
  (define-key menu [frames] '("Stack" . gdb-display-stack-buffer))
  (define-key menu [breakpoints] '("Breakpoints" . gdb-display-breakpoints-buffer))
;  (define-key menu [assembler] '("Machine" . gdb-display-assembler-buffer))
)

(let ((menu (make-sparse-keymap "View")))
   (define-key gud-menu-map [view]
     `(menu-item "View" ,menu :visible (eq gud-minor-mode 'gdba)))
;  (define-key menu [both] '(menu-item "Both" gdb-view-both
;	       :help "Display both source and assembler"
;	       :button (:radio . (eq gdb-selected-view 'both))))
   (define-key menu [assembler] '(menu-item "Machine" gdb-view-assembler
	       :help "Display assembler only"
	       :button (:radio . (eq gdb-selected-view 'assembler))))
   (define-key menu [source] '(menu-item "Source" gdb-view-source-function
	       :help "Display source only"
	       :button (:radio . (eq gdb-selected-view 'source)))))

(let ((menu (make-sparse-keymap "GDB-UI")))
  (define-key gud-menu-map [ui]
    `(menu-item "GDB-UI" ,menu :visible (eq gud-minor-mode 'gdba)))
  (define-key menu [gdb-restore-windows]
    '("Restore window layout" . gdb-restore-windows))
  (define-key menu [gdb-many-windows]
    (menu-bar-make-toggle gdb-many-windows gdb-many-windows
			  "Display other windows" "Many Windows %s"
			  "Display locals, stack and breakpoint information")))

(defun gdb-frame-gdb-buffer ()
  "Display GUD buffer in a new frame."
  (interactive)
  (select-frame (make-frame gdb-frame-parameters))
  (switch-to-buffer (gdb-get-create-buffer 'gdba))
  (set-window-dedicated-p (selected-window) t))

(defun gdb-display-gdb-buffer ()
  "Display GUD buffer."
  (interactive)
  (gdb-display-buffer
   (gdb-get-create-buffer 'gdba)))

(defvar gdb-main-file nil "Source file from which program execution begins.")

(defun gdb-view-source-function ()
  "Select source view."
  (interactive)
  (if gdb-view-source
      (gdb-display-buffer
       (if gud-last-last-frame
	   (gud-find-file (car gud-last-last-frame))
	 (gud-find-file gdb-main-file))))
  (setq gdb-selected-view 'source))

(defun gdb-view-assembler()
  "Select disassembly view."
  (interactive)
  (gdb-display-buffer (gdb-get-create-buffer 'gdb-assembler-buffer))
  (gdb-invalidate-assembler)
  (setq gdb-selected-view 'assembler))

;(defun gdb-view-both()
;(interactive)
;(setq gdb-selected-view 'both))

(defcustom gdb-show-main nil
  "Nil means don't display source file containing the main routine."
  :type 'boolean
  :group 'gud)

(defun gdb-setup-windows ()
  "Layout the window pattern for gdb-many-windows."
  (gdb-display-locals-buffer)
  (gdb-display-stack-buffer)
  (delete-other-windows)
  (gdb-display-breakpoints-buffer)
  (delete-other-windows)
  (switch-to-buffer gud-comint-buffer)
  (split-window nil ( / ( * (window-height) 3) 4))
  (split-window nil ( / (window-height) 3))
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer (gdb-locals-buffer-name))
  (other-window 1)
  (switch-to-buffer
   (if (and gdb-view-source
	    (eq gdb-selected-view 'source))
       (if gud-last-last-frame
	   (gud-find-file (car gud-last-last-frame))
	 (gud-find-file gdb-main-file))
     (gdb-get-create-buffer 'gdb-assembler-buffer)))
  (when gdb-use-inferior-io-buffer
    (split-window-horizontally)
    (other-window 1)
    (switch-to-buffer (gdb-inferior-io-name)))
  (other-window 1)
  (switch-to-buffer (gdb-stack-buffer-name))
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer (gdb-breakpoints-buffer-name))
  (other-window 1))

(defcustom gdb-many-windows nil
  "Nil (the default value) means just pop up the GUD buffer
unless `gdb-show-main' is t. In this case it starts with two
windows: one displaying the GUD buffer and the other with the
source file with the main routine of the debugee. Non-nil means
display the layout shown for `gdba'."
  :type 'boolean
  :group 'gud)

(defun gdb-many-windows (arg)
"Toggle the number of windows in the basic arrangement."
  (interactive "P")
  (setq gdb-many-windows
	(if (null arg)
	    (not gdb-many-windows)
	  (> (prefix-numeric-value arg) 0)))
  (condition-case nil
      (gdb-restore-windows)
    (error nil)))

(defun gdb-restore-windows ()
  "Restore the basic arrangement of windows used by gdba.
This arrangement depends on the value of `gdb-many-windows'."
  (interactive)
  (if gdb-many-windows
      (progn
	(switch-to-buffer gud-comint-buffer)
	(delete-other-windows)
	(gdb-setup-windows))
    (switch-to-buffer gud-comint-buffer)
    (delete-other-windows)
    (split-window)
    (other-window 1)
    (switch-to-buffer
     (if (and gdb-view-source
	      (eq gdb-selected-view 'source))
	 (if gud-last-last-frame
	     (gud-find-file (car gud-last-last-frame))
	   (gud-find-file gdb-main-file))
       (gdb-get-create-buffer 'gdb-assembler-buffer)))
    (other-window 1)))

(defun gdb-reset ()
  "Exit a debugging session cleanly by killing the gdb buffers and resetting
 the source buffers."
  (dolist (buffer (buffer-list))
    (if (not (eq buffer gud-comint-buffer))
	(with-current-buffer buffer
	  (if (memq gud-minor-mode '(gdba pdb))
	      (if (string-match "^\*.+*$" (buffer-name))
		  (kill-buffer nil)
		(gdb-remove-breakpoint-icons (point-min) (point-max) t)
		(setq gud-minor-mode nil)
		(kill-local-variable 'tool-bar-map)
		(setq gud-running nil))))))
  (when (markerp gdb-overlay-arrow-position)
    (move-marker gdb-overlay-arrow-position nil)
    (setq gdb-overlay-arrow-position nil))
  (setq overlay-arrow-variable-list
	(delq 'gdb-overlay-arrow-position overlay-arrow-variable-list)))

(defun gdb-source-info ()
  "Find the source file where the program starts and displays it with related
buffers."
  (goto-char (point-min))
  (if (search-forward "directory is " nil t)
      (if (looking-at "\\S-*:\\(\\S-*\\)")
	  (setq gdb-cdir (match-string 1))
	(looking-at "\\S-*")
	(setq gdb-cdir (match-string 0))))
  (if (search-forward "Located in " nil t)
      (if (looking-at "\\S-*")
	  (setq gdb-main-file (match-string 0)))
    (setq gdb-view-source nil))
  (if gdb-many-windows
      (gdb-setup-windows)
    (gdb-get-create-buffer 'gdb-breakpoints-buffer)
    (when gdb-show-main
      (switch-to-buffer gud-comint-buffer)
      (delete-other-windows)
      (split-window)
      (other-window 1)
      (switch-to-buffer
       (if gdb-view-source
	   (gud-find-file gdb-main-file)
	 (gdb-get-create-buffer 'gdb-assembler-buffer)))
      (other-window 1))))

;;from put-image
(defun gdb-put-string (putstring pos &optional dprop)
  "Put string PUTSTRING in front of POS in the current buffer.
PUTSTRING is displayed by putting an overlay into the current buffer with a
`before-string' STRING that has a `display' property whose value is
PUTSTRING."
  (let ((gdb-string "x")
	(buffer (current-buffer)))
    (let ((overlay (make-overlay pos pos buffer))
	  (prop (or dprop
		    (list (list 'margin 'left-margin) putstring))))
      (put-text-property 0 (length gdb-string) 'display prop gdb-string)
      (overlay-put overlay 'put-break t)
      (overlay-put overlay 'before-string gdb-string))))

;;from remove-images
(defun gdb-remove-strings (start end &optional buffer)
  "Remove strings between START and END in BUFFER.
Remove only strings that were put in BUFFER with calls to `gdb-put-string'.
BUFFER nil or omitted means use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (let ((overlays (overlays-in start end)))
    (while overlays
      (let ((overlay (car overlays)))
	(when (overlay-get overlay 'put-break)
	  (delete-overlay overlay)))
      (setq overlays (cdr overlays)))))

(defun gdb-put-breakpoint-icon (enabled)
  (let ((start (progn (beginning-of-line) (- (point) 1)))
	(end (progn (end-of-line) (+ (point) 1))))
    (gdb-remove-breakpoint-icons start end)
    (if (display-images-p)
	(if (>= (car (window-fringes)) 8)
	    (gdb-put-string
	     nil (1+ start)
	     `(left-fringe
	       ,(or breakpoint-bitmap
		    (setq breakpoint-bitmap
			  (define-fringe-bitmap
			    "\x3c\x7e\xff\xff\xff\xff\x7e\x3c")))
	       ,(if enabled
		    'breakpoint-enabled-bitmap-face
		  'breakpoint-disabled-bitmap-face)))
	  (when (< left-margin-width 2)
	    (save-current-buffer
	      (setq left-margin-width 2)
	      (if (get-buffer-window (current-buffer) 'visible)
		  (set-window-margins 
		   (get-buffer-window (current-buffer) 'visible)
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
					    :ascent 100)
				     (:type pbm :data
					    ,breakpoint-disabled-pbm-data
					    :ascent 100))))))
	   (+ start 1) nil 'left-margin))
      (when (< left-margin-width 2)
	(save-current-buffer
	  (setq left-margin-width 2)
	  (if (get-buffer-window (current-buffer) 'visible)
	      (set-window-margins 
	       (get-buffer-window (current-buffer) 'visible)
	       left-margin-width right-margin-width))))
      (gdb-put-string (if enabled "B" "b") (1+ start)))))

(defun gdb-remove-breakpoint-icons (start end &optional remove-margin)
  (gdb-remove-strings start end)
  (if (display-images-p)
      (remove-images start end))
  (when remove-margin
    (setq left-margin-width 0)
    (if (get-buffer-window (current-buffer) 'visible)
	(set-window-margins 
	 (get-buffer-window (current-buffer) 'visible)
	 left-margin-width right-margin-width))))


;;
;; Assembler buffer.
;;
(gdb-set-buffer-rules 'gdb-assembler-buffer
		      'gdb-assembler-buffer-name
		      'gdb-assembler-mode)

(def-gdb-auto-updated-buffer gdb-assembler-buffer
  gdb-invalidate-assembler
  (concat "server disassemble " gdb-current-address "\n")
  gdb-assembler-handler
  gdb-assembler-custom)

(defun gdb-assembler-custom ()
  (let ((buffer (gdb-get-buffer 'gdb-assembler-buffer))
	(pos 1) (address) (flag))
    (with-current-buffer buffer
      (if (not (equal gdb-current-address "main"))
	  (progn
	    (goto-char (point-min))
	    (if (re-search-forward gdb-current-address nil t)
		(progn
		  (setq pos (point))
		  (beginning-of-line)
		  (or gdb-overlay-arrow-position
		      (setq gdb-overlay-arrow-position (make-marker)))
		  (set-marker gdb-overlay-arrow-position
			      (point) (current-buffer))))))
      ;; remove all breakpoint-icons in assembler buffer before updating.
      (gdb-remove-breakpoint-icons (point-min) (point-max)))
    (with-current-buffer (gdb-get-buffer 'gdb-breakpoints-buffer)
      (goto-char (point-min))
      (while (< (point) (- (point-max) 1))
	(forward-line 1)
	(if (looking-at "[^\t].*breakpoint")
	    (progn
	      (looking-at
	       "[0-9]*\\s-*\\S-*\\s-*\\S-*\\s-*\\(.\\)\\s-*0x\\(\\S-*\\)")
	      (setq flag (char-after (match-beginning 1)))
	      (setq address (match-string 2))
	      ;; remove leading 0s from output of info break.
	      (if (string-match "^0+\\(.*\\)" address)
		  (setq address (match-string 1 address)))
	      (with-current-buffer buffer
		  (goto-char (point-min))
		  (if (re-search-forward address nil t)
		      (gdb-put-breakpoint-icon (eq flag ?y))))))))
    (if (not (equal gdb-current-address "main"))
	(set-window-point (get-buffer-window buffer 'visible) pos))))

(defvar gdb-assembler-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    map))

(defun gdb-assembler-mode ()
  "Major mode for viewing code assembler.

\\{gdb-assembler-mode-map}"
  (setq major-mode 'gdb-assembler-mode)
  (setq mode-name "Machine")
  (setq gdb-overlay-arrow-position nil)
  (add-to-list 'overlay-arrow-variable-list 'gdb-overlay-arrow-position)
  (put 'gdb-overlay-arrow-position 'overlay-arrow-string "=>")
  (setq fringes-outside-margins t)
  (setq buffer-read-only t)
  (use-local-map gdb-assembler-mode-map)
  (gdb-invalidate-assembler))

(defun gdb-assembler-buffer-name ()
  (with-current-buffer gud-comint-buffer
    (concat "*Machine Code " (gdb-get-target-string) "*")))

(defun gdb-display-assembler-buffer ()
  "Display disassembly view."
  (interactive)
  (gdb-display-buffer
   (gdb-get-create-buffer 'gdb-assembler-buffer)))

(defun gdb-frame-assembler-buffer ()
  "Display disassembly view in a new frame."
  (interactive)
  (select-frame (make-frame gdb-frame-parameters))
  (switch-to-buffer (gdb-get-create-buffer 'gdb-assembler-buffer))
  (set-window-dedicated-p (selected-window) t))

;; modified because if gdb-current-address has changed value a new command
;; must be enqueued to update the buffer with the new output
(defun gdb-invalidate-assembler (&optional ignored)
  (if (gdb-get-buffer 'gdb-assembler-buffer)
      (progn
	(unless (string-equal gdb-current-frame gdb-previous-frame)
	  (if (or (not (member 'gdb-invalidate-assembler
			       (gdb-get-pending-triggers)))
		  (not (string-equal gdb-current-address
				     gdb-previous-address)))
	  (progn
	    ;; take previous disassemble command off the queue
	    (with-current-buffer gud-comint-buffer
	      (let ((queue (gdb-get-input-queue)) (item))
		(dolist (item queue)
		  (if (equal (cdr item) '(gdb-assembler-handler))
		      (gdb-set-input-queue
		       (delete item (gdb-get-input-queue)))))))
	    (gdb-enqueue-input
	     (list (concat "server disassemble " gdb-current-address "\n")
		   'gdb-assembler-handler))
	    (gdb-set-pending-triggers
	     (cons 'gdb-invalidate-assembler
		   (gdb-get-pending-triggers)))
	    (setq gdb-previous-address gdb-current-address)
	    (setq gdb-previous-frame gdb-current-frame)))))))

(defun gdb-get-current-frame ()
  (if (not (member 'gdb-get-current-frame (gdb-get-pending-triggers)))
      (progn
	(gdb-enqueue-input
	 (list (concat "server info frame\n") 'gdb-frame-handler))
	(gdb-set-pending-triggers
	 (cons 'gdb-get-current-frame
	       (gdb-get-pending-triggers))))))

(defun gdb-frame-handler ()
  (gdb-set-pending-triggers
   (delq 'gdb-get-current-frame (gdb-get-pending-triggers)))
  (with-current-buffer (gdb-get-create-buffer 'gdb-partial-output-buffer)
    (goto-char (point-min))
    (forward-line)
    (if (looking-at ".*=\\s-+0x\\(\\S-*\\)\\s-+in\\s-+\\(\\S-*?\\);? ")
	(progn
	  (setq gdb-current-frame (match-string 2))
	  (let ((address (match-string 1)))
	    ;; remove leading 0s from output of info frame command.
	    (if (string-match "^0+\\(.*\\)" address)
		(setq gdb-current-address
		      (concat "0x" (match-string 1 address)))
	      (setq gdb-current-address (concat "0x" address))))
	  (if (or (if (not (re-search-forward "(\\S-*:[0-9]*);" nil t))
		      (progn (setq gdb-view-source nil) t))
		  (eq gdb-selected-view 'assembler))
	      (progn
		(gdb-display-buffer
		 (gdb-get-create-buffer 'gdb-assembler-buffer))
		;;update with new frame for machine code if necessary
		(gdb-invalidate-assembler))))))
    (if (re-search-forward " source language \\(\\S-*\\)\." nil t)
	(setq gdb-current-language (match-string 1))))

(provide 'gdb-ui)

;;; arch-tag: e9fb00c5-74ef-469f-a088-37384caae352
;;; gdb-ui.el ends here
