;;; gdb-ui.el --- User Interface for running GDB

;; Author: Nick Roberts <nick@nick.uklinux.net>
;; Maintainer: FSF
;; Keywords: unix, tools

;; Copyright (C) 2002  Free Software Foundation, Inc.

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
;; It separates the input/output of your program from that of GDB and displays
;; expressions and their current values in their own buffers. It also uses
;; features of Emacs 21 such as the display margin for breakpoints, and the
;; toolbar (see the GDB User Interface section in the Emacs info manual).

;; Start the debugger with M-x gdba.

;; This file is based on gdba.el from GDB 5.0 written by Tom Lord and Jim
;; Kingdon and uses GDB's annotation interface. You don't need to know about
;; annotations to use this mode as a debugger, but if you are interested
;; developing the mode itself, then see the Annotations section in the GDB
;; info manual.
;;
;;  Known Bugs: Does not auto-display arrays of structures or structures
;;  containing arrays.

;;; Code:

(require 'gud)

(defcustom gdb-window-height 20
  "*Number of lines in a frame for a displayed expression in GDB-UI."
  :type 'integer
  :group 'gud)

(defcustom gdb-window-width 30
  "Width of a frame for a displayed expression in GDB-UI."
  :type 'integer
  :group 'gud)

(defvar gdb-main-or-pc nil "Initialisation for Assembler buffer.")
(defvar gdb-current-address nil)
(defvar gdb-display-in-progress nil)
(defvar gdb-dive nil)
(defvar gdb-buffer-type nil)
(defvar gdb-variables '()
  "A list of variables that are local to the GUD buffer.")


;;;###autoload
(defun gdba (command-line)
  "Run gdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

If `gdb-many-windows' is nil (the default value) then gdb starts with
just two windows : the GUD and the source buffer. If it is t the
following layout will appear (keybindings given in relevant buffer) :

---------------------------------------------------------------------
                               GDB Toolbar
---------------------------------------------------------------------
GUD buffer (I/O of GDB)           | Locals buffer
                                  |
                                  |
                                  |
---------------------------------------------------------------------
Source buffer                     | Input/Output (of debuggee) buffer
                                  | (comint-mode)
                                  |
                                  |
                                  |
                                  |
                                  |
                                  |
---------------------------------------------------------------------
Stack buffer                      | Breakpoints buffer
 RET      gdb-frames-select       | SPC    gdb-toggle-breakpoint
                                  | RET    gdb-goto-breakpoint
                                  |   d    gdb-delete-breakpoint
---------------------------------------------------------------------

All the buffers share the toolbar and source should always display in the same
window e.g after typing g on a breakpoint in the breakpoints buffer. Breakpoint
icons are displayed both by setting a break with gud-break and by typing break
in the GUD buffer.

This works best (depending on the size of your monitor) using most of the
screen.

Displayed expressions appear in separate frames. Arrays may be displayed
as slices and visualised using the graph program from plotutils if installed.
Pointers in structures may be followed in a tree-like fashion.

The following interactive lisp functions help control operation :

`gdb-many-windows'    - Toggle the number of windows gdb uses.
`gdb-restore-windows' - To restore the window layout.
`gdb-quit'            - To delete (most) of the buffers used by GDB-UI and
                        reset variables."
  ;;
  (interactive (list (gud-query-cmdline 'gdba)))
  ;;
  ;; Let's start with a basic gud-gdb buffer and then modify it a bit.
  (gdb command-line)
  ;;
  (set (make-local-variable 'gud-minor-mode) 'gdba)
  (set (make-local-variable 'gud-marker-filter) 'gud-gdba-marker-filter)
  ;;
  (gud-def gud-break (if (not (string-equal mode-name "Assembler"))
			 (gud-call "break %f:%l" arg)
		       (save-excursion
			 (beginning-of-line)
			 (forward-char 2)
			 (gud-call "break *%a" arg)))
	   "\C-b" "Set breakpoint at current line or address.")
  ;;
  (gud-def gud-remove (if (not (string-equal mode-name "Assembler"))
			  (gud-call "clear %f:%l" arg)
			(save-excursion
			  (beginning-of-line)
			  (forward-char 2)
			  (gud-call "clear *%a" arg)))
	   "\C-d" "Remove breakpoint at current line or address.")
  ;;
  (setq comint-input-sender 'gdb-send)
  ;;
  ;; (re-)initialise
  (setq gdb-main-or-pc "main")
  (setq gdb-current-address nil)
  (setq gdb-display-in-progress nil)
  (setq gdb-dive nil)
  ;;
  (mapc 'make-local-variable gdb-variables)
  (setq gdb-buffer-type 'gdba)
  ;;
  (gdb-clear-inferior-io)
  ;;
  (gdb-enqueue-input (list "set height 0\n" 'ignore))
  ;; find source file and compilation directory here
  (gdb-enqueue-input (list "server list\n" 'ignore))
  (gdb-enqueue-input (list "server info source\n"
			   'gdb-source-info))
  ;;
  (run-hooks 'gdba-mode-hook))

(defun gud-display ()
  "Auto-display (possibly dereferenced) C expression at point."
  (interactive)
  (save-excursion
    (let ((expr (gud-find-c-expr)))
      (gdb-enqueue-input
       (list (concat "server ptype " expr "\n")
	     `(lambda () (gud-display1 ,expr)))))))

(defun gud-display1 (expr)
  (goto-char (point-min))
  (if (looking-at "No symbol")
      (progn
	(gdb-set-output-sink 'user)
	(gud-call (concat "server ptype " expr)))
    (goto-char (- (point-max) 1))
    (if (equal (char-before) (string-to-char "\*"))
	(gdb-enqueue-input
	 (list (concat "server display* " expr "\n") 'ignore))
      (gdb-enqueue-input
       (list (concat "server display " expr "\n") 'ignore)))))

; this would messy because these bindings don't work with M-x gdb
; (define-key global-map "\C-x\C-a\C-a" 'gud-display)
; (define-key gud-minor-mode-map "\C-c\C-a" 'gud-display)



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
  "One of the symbols bound in gdb-buffer-rules")

(def-gdb-var burst ""
  "A string of characters from gdb that have not yet been processed.")

(def-gdb-var input-queue ()
  "A list of high priority gdb command objects.")

(def-gdb-var idle-input-queue ()
  "A list of low priority gdb command objects.")

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
    post-emacs -- ignore input until the prompt annotation is
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
   (current-buffer) "cat")
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
;; There is a high and low priority input queue.  Low priority input is sent
;; only when the high priority queue is idle.
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
  (gdb-enqueue-input (concat string "\n")))

;; Note: Stuff enqueued here will be sent to the next prompt, even if it
;; is a query, or other non-top-level prompt.  To guarantee stuff will get
;; sent to the top-level prompt, currently it must be put in the idle queue.
;;				 ^^^^^^^^^
;; [This should encourage gdb extensions that invoke gdb commands to let
;;  the user go first; it is not a bug.     -t]
;;

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
	 (if (not (cdr queue))
	     (let ((answer (car queue)))
	       (gdb-set-input-queue '())
	       answer)
	   (gdb-take-last-elt queue)))))

(defun gdb-enqueue-idle-input (item)
  (if (and (gdb-get-prompting)
	   (not (gdb-get-input-queue)))
      (progn
	(gdb-send-item item)
	(gdb-set-prompting nil))
    (gdb-set-idle-input-queue
     (cons item (gdb-get-idle-input-queue)))))

(defun gdb-dequeue-idle-input ()
  (let ((queue (gdb-get-idle-input-queue)))
    (and queue
	 (if (not (cdr queue))
	     (let ((answer (car queue)))
	       (gdb-set-idle-input-queue '())
	       answer)
	   (gdb-take-last-elt queue)))))

;; Don't use this in general.
(defun gdb-take-last-elt (l)
  (if (cdr (cdr l))
      (gdb-take-last-elt (cdr l))
    (let ((answer (car (cdr l))))
      (setcdr l '())
      answer)))


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

(defcustom gud-gdba-command-name "gdb -annotate=2"
  "Default command to execute an executable under the GDB-UI debugger."
  :type 'string
  :group 'gud)

(defvar gdb-annotation-rules
  '(("frames-invalid" gdb-invalidate-frame-and-assembler)
    ("breakpoints-invalid" gdb-invalidate-breakpoints-and-assembler)
    ("pre-prompt" gdb-pre-prompt)
    ("prompt" gdb-prompt)
    ("commands" gdb-subprompt)
    ("overload-choice" gdb-subprompt)
    ("query" gdb-subprompt)
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
    ("display-begin" gdb-display-begin)
    ("display-end" gdb-display-end)
; GDB commands info stack, info locals and frame generate an error-begin
; annotation at start when there is no stack but this is a quirk/bug in
; annotations.
;    ("error-begin" gdb-error-begin)
    ("display-number-end" gdb-display-number-end)
    ("array-section-begin" gdb-array-section-begin)
    ("array-section-end" gdb-array-section-end)
    ;; ("elt" gdb-elt)
    ("field-begin" gdb-field-begin)
    ("field-end" gdb-field-end)
    ) "An assoc mapping annotation tags to functions which process them.")

(defun gdb-ignore-annotation (args)
  nil)

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
  (setq gdb-main-or-pc gdb-current-address)
  ;;update with new frame for machine code if necessary
  (gdb-invalidate-assembler))

(defun gdb-send-item (item)
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
      (gdb-set-output-sink 'post-emacs)
      (let ((handler
	     (car (cdr (gdb-get-current-item)))))
	(save-excursion
	  (set-buffer (gdb-get-create-buffer
		       'gdb-partial-output-buffer))
	  (funcall handler))))
     (t
      (gdb-set-output-sink 'user)
      (error "Phase error in gdb-pre-prompt (got %s)" sink)))))

(defun gdb-prompt (ignored)
  "An annotation handler for `prompt'.
This sends the next command (if any) to gdb."
  (let ((sink (gdb-get-output-sink)))
    (cond
     ((eq sink 'user) t)
     ((eq sink 'post-emacs)
      (gdb-set-output-sink 'user))
     (t
      (gdb-set-output-sink 'user)
      (error "Phase error in gdb-prompt (got %s)" sink))))
  (let ((highest (gdb-dequeue-input)))
    (if highest
	(gdb-send-item highest)
      (let ((lowest (gdb-dequeue-idle-input)))
	(if lowest
	    (gdb-send-item lowest)
	  (progn
	    (gdb-set-prompting t)
	    (gud-display-frame)))))))

(defun gdb-subprompt (ignored)
  "An annotation handler for non-top-level prompts."
  (let ((highest (gdb-dequeue-input)))
    (if highest
	(gdb-send-item highest)
      (gdb-set-prompting t))))

(defun gdb-starting (ignored)
  "An annotation handler for `starting'.  This says that I/O for the
subprocess is now the program being debugged, not GDB."
  (let ((sink (gdb-get-output-sink)))
    (cond
     ((eq sink 'user)
      (progn
	(setq gud-running t)
	(gdb-set-output-sink 'inferior)))
     (t (error "Unexpected `starting' annotation")))))

(defun gdb-stopping (ignored)
  "An annotation handler for `exited' and other annotations which say that I/O
for the subprocess is now GDB, not the program being debugged."
  (let ((sink (gdb-get-output-sink)))
    (cond
     ((eq sink 'inferior)
      (gdb-set-output-sink 'user))
     (t (error "Unexpected stopping annotation")))))

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
	(gdb-invalidate-registers ignored)
	(gdb-invalidate-locals ignored)
	(gdb-invalidate-display ignored)))
  (let ((sink (gdb-get-output-sink)))
    (cond
     ((eq sink 'user) t)
     ((eq sink 'pre-emacs)
      (gdb-set-output-sink 'emacs))
     (t
      (gdb-set-output-sink 'user)
      (error "Phase error in gdb-post-prompt (got %s)" sink)))))

;; If we get an error whilst evaluating one of the expressions
;; we won't get the display-end annotation. Set the sink back to
;; user to make sure that the error message is seen.
;; NOT USED: see annotation-rules for reason.
;(defun gdb-error-begin (ignored)
;  (gdb-set-output-sink 'user))

(defun gdb-display-begin (ignored)
  (gdb-set-output-sink 'emacs)
  (gdb-clear-partial-output)
  (setq gdb-display-in-progress t))

(defvar gdb-expression-buffer-name)
(defvar gdb-display-number)
(defvar gdb-dive-display-number)

(defun gdb-display-number-end (ignored)
  (set-buffer (gdb-get-buffer 'gdb-partial-output-buffer))
  (setq gdb-display-number (buffer-string))
  (setq gdb-expression-buffer-name
	(concat "*display " gdb-display-number "*"))
  (save-excursion
    (if (progn
	  (set-buffer (window-buffer))
	  gdb-dive)
	(progn
	  (let ((number gdb-display-number))
	    (switch-to-buffer
	     (set-buffer (get-buffer-create gdb-expression-buffer-name)))
	    (gdb-expressions-mode)
	    (setq gdb-dive-display-number number)))
      (set-buffer (get-buffer-create gdb-expression-buffer-name))
      (gdb-expressions-mode)
      (if (and (display-graphic-p) (not gdb-dive))
	  (catch 'frame-exists
	    (dolist (frame (frame-list))
	      (if (string-equal (frame-parameter frame 'name)
				gdb-expression-buffer-name)
		  (throw 'frame-exists nil)))
	    (make-frame `((height . ,gdb-window-height)
			  (width . ,gdb-window-width)
			  (tool-bar-lines . nil)
			  (menu-bar-lines . nil)
			  (minibuffer . nil))))
	(gdb-display-buffer (get-buffer gdb-expression-buffer-name)))))
  (set-buffer (gdb-get-buffer 'gdb-partial-output-buffer))
  (setq gdb-dive nil))

(defvar gdb-current-frame nil)
(defvar gdb-nesting-level)
(defvar gdb-expression)
(defvar gdb-point)
(defvar gdb-annotation-arg)

(defun gdb-delete-line ()
  "Delete the current line."
  (delete-region (line-beginning-position) (line-beginning-position 2)))

(defun gdb-display-end (ignored)
  (set-buffer (gdb-get-buffer 'gdb-partial-output-buffer))
  (goto-char (point-min))
  (search-forward ": ")
  (looking-at "\\(.*?\\) =")
  (let ((char "")
	(gdb-temp-value (match-string 1)))
    ;;move * to front of expression if necessary
    (if (looking-at ".*\\*")
	(progn
	  (setq char "*")
	  (setq gdb-temp-value (substring gdb-temp-value 1 nil))))
    (save-excursion
      (set-buffer gdb-expression-buffer-name)
      (setq gdb-expression gdb-temp-value)
      (if (not (string-match "::" gdb-expression))
	  (setq gdb-expression (concat char gdb-current-frame
				       "::" gdb-expression))
	;;else put * back on if necessary
	(setq gdb-expression (concat char gdb-expression)))
      (if (not header-line-format)
	  (setq header-line-format (concat "-- " gdb-expression " %-")))))
  ;;
  ;;-if scalar/string
  (if (not (re-search-forward "##" nil t))
      (progn
	(save-excursion
	  (set-buffer gdb-expression-buffer-name)
	  (let ((buffer-read-only nil))
	    (delete-region (point-min) (point-max))
	    (insert-buffer-substring
	     (gdb-get-buffer 'gdb-partial-output-buffer)))))
    ;; display expression name...
    (goto-char (point-min))
    (let ((start (progn (point)))
	  (end (progn (end-of-line) (point))))
      (save-excursion
	(set-buffer gdb-expression-buffer-name)
	(setq buffer-read-only nil)
	(delete-region (point-min) (point-max))
	(insert-buffer-substring (gdb-get-buffer
				  'gdb-partial-output-buffer)
				 start end)
	(insert "\n")))
    (goto-char (point-min))
    (re-search-forward "##" nil t)
    (setq gdb-nesting-level 0)
    (if (looking-at "array-section-begin")
	(progn
	  (gdb-delete-line)
	  (setq gdb-point (point))
	  (gdb-array-format)))
    (if (looking-at "field-begin \\(.\\)")
	(progn
	  (setq gdb-annotation-arg (match-string 1))
	  (gdb-field-format-begin))))
  (save-excursion
    (set-buffer gdb-expression-buffer-name)
    (if gdb-dive-display-number
	(progn
	  (let ((buffer-read-only nil))
	    (goto-char (point-max))
	    (insert "\n")
	    (insert-text-button "[back]" 'type 'gdb-display-back)))))
  (gdb-clear-partial-output)
  (gdb-set-output-sink 'user)
  (setq gdb-display-in-progress nil))

(define-button-type 'gdb-display-back
  'help-echo (purecopy "mouse-2, RET: go back to previous display buffer")
  'action (lambda (button) (gdb-display-go-back)))

(defun gdb-display-go-back ()
  ;; delete display so they don't accumulate and delete buffer
  (let ((number gdb-display-number))
    (gdb-enqueue-input
     (list (concat "server delete display " number "\n") 'ignore))
    (switch-to-buffer (concat "*display " gdb-dive-display-number "*"))
    (kill-buffer (get-buffer (concat "*display " number "*")))))

;; prefix annotations with ## and process whole output in one chunk
;; in gdb-partial-output-buffer (to allow recursion).

;; array-section flags are just removed again but after counting. They
;; might also be useful for arrays of structures and structures with arrays.
(defun gdb-array-section-begin (args)
  (if gdb-display-in-progress
      (progn
	(save-excursion
	  (set-buffer (gdb-get-buffer 'gdb-partial-output-buffer))
	  (goto-char (point-max))
	  (insert (concat "\n##array-section-begin " args "\n"))))))

(defun gdb-array-section-end (ignored)
  (if gdb-display-in-progress
      (progn
	(save-excursion
	  (set-buffer (gdb-get-buffer 'gdb-partial-output-buffer))
	  (goto-char (point-max))
	  (insert "\n##array-section-end\n")))))

(defun gdb-field-begin (args)
  (if gdb-display-in-progress
      (progn
	(save-excursion
	  (set-buffer (gdb-get-buffer 'gdb-partial-output-buffer))
	  (goto-char (point-max))
	  (insert (concat "\n##field-begin " args "\n"))))))

(defun gdb-field-end (ignored)
  (if gdb-display-in-progress
      (progn
	(save-excursion
	  (set-buffer (gdb-get-buffer 'gdb-partial-output-buffer))
	  (goto-char (point-max))
	  (insert "\n##field-end\n")))))

(defun gdb-elt (ignored)
  (if gdb-display-in-progress
      (progn
	(goto-char (point-max))
	(insert "\n##elt\n"))))

(defun gdb-field-format-begin ()
  ;; get rid of ##field-begin
  (gdb-delete-line)
  (gdb-insert-field)
  (setq gdb-nesting-level (+ gdb-nesting-level 1))
  (while (re-search-forward "##" nil t)
    ;; keep making recursive calls...
    (if (looking-at "field-begin \\(.\\)")
	(progn
	  (setq gdb-annotation-arg (match-string 1))
	  (gdb-field-format-begin)))
    ;; until field-end.
    (if (looking-at "field-end") (gdb-field-format-end))))

(defun gdb-field-format-end ()
  ;; get rid of ##field-end and `,' or `}'
  (gdb-delete-line)
  (gdb-delete-line)
  (setq gdb-nesting-level (- gdb-nesting-level 1)))

(defvar gdb-dive-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'gdb-dive)
    (define-key map [S-mouse-2] 'gdb-dive-new-frame)
    map))

(defun gdb-dive (event)
  "Dive into structure."
  (interactive "e")
  (setq gdb-dive t)
  (gdb-dive-new-frame event))

(defun gdb-dive-new-frame (event)
  "Dive into structure and display in a new frame."
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (let ((point (point)) (gdb-full-expression gdb-expression)
	  (end (progn (end-of-line) (point)))
	  (gdb-part-expression "") (gdb-last-field nil) (gdb-display-char nil))
      (beginning-of-line)
      (if (looking-at "\*") (setq gdb-display-char "*"))
      (re-search-forward "\\(\\S-+\\) = " end t)
      (setq gdb-last-field (match-string-no-properties 1))
      (goto-char (match-beginning 1))
      (let ((last-column (current-column)))
	(while (re-search-backward "\\s-\\(\\S-+\\) = {" nil t)
	  (goto-char (match-beginning 1))
	  (if (and (< (current-column) last-column)
		   (> (count-lines 1 (point)) 1))
	      (progn
		(setq gdb-part-expression
		      (concat "." (match-string-no-properties 1)
			      gdb-part-expression))
		(setq last-column (current-column))))))
      ;; * not needed for components of a pointer to a structure in gdb
      (if (string-equal "*" (substring gdb-full-expression 0 1))
	  (setq gdb-full-expression (substring gdb-full-expression 1 nil)))
      (setq gdb-full-expression
	    (concat gdb-full-expression gdb-part-expression "." gdb-last-field))
      (gdb-enqueue-input
       (list (concat "server display" gdb-display-char
		     " " gdb-full-expression "\n")
	     'ignore)))))

(defun gdb-insert-field ()
  (let ((start (progn (point)))
	(end (progn (next-line) (point)))
	(num 0))
    (save-excursion
      (set-buffer gdb-expression-buffer-name)
      (let ((buffer-read-only nil))
	(if (string-equal gdb-annotation-arg "\*") (insert "\*"))
	(while (<= num gdb-nesting-level)
	  (insert "\t")
	  (setq num (+ num 1)))
	(insert-buffer-substring (gdb-get-buffer
				  'gdb-partial-output-buffer)
				 start end)
	(put-text-property (- (point) (- end start)) (- (point) 1)
			   'mouse-face 'highlight)
	(put-text-property (- (point) (- end start)) (- (point) 1)
			   'local-map gdb-dive-map)))
    (delete-region start end)))

(defvar gdb-values)

(defun gdb-array-format ()
  (while (re-search-forward "##" nil t)
    ;; keep making recursive calls...
    (if (looking-at "array-section-begin")
	(progn
	  ;;get rid of ##array-section-begin
	  (gdb-delete-line)
	  (setq gdb-nesting-level (+ gdb-nesting-level 1))
	  (gdb-array-format)))
    ;;until *matching* array-section-end is found
    (if (looking-at "array-section-end")
	(if (eq gdb-nesting-level 0)
	    (progn
	      (let ((values (buffer-substring gdb-point (- (point) 2))))
		(save-excursion
		  (set-buffer gdb-expression-buffer-name)
		  (setq gdb-values
			(concat "{" (replace-regexp-in-string "\n" "" values)
				"}"))
		  (gdb-array-format1))))
	  ;;else get rid of ##array-section-end etc
	  (gdb-delete-line)
	  (setq gdb-nesting-level (- gdb-nesting-level 1))
	  (gdb-array-format)))))

(defvar gdb-array-start)
(defvar gdb-array-stop)

(defvar gdb-array-slice-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'gdb-array-slice)
    map))

(defun gdb-array-slice (event)
  "Select an array slice to display."
  (interactive "e")
  (mouse-set-point event)
  (save-excursion
    (let ((n -1) (stop 0) (start 0) (point (point)))
      (beginning-of-line)
      (while (search-forward "[" point t)
	(setq n (+ n 1)))
      (setq start (string-to-int (read-string "Start index: ")))
      (aset gdb-array-start n start)
      (setq stop (string-to-int (read-string "Stop index: ")))
      (aset gdb-array-stop n stop)))
  (gdb-array-format1))

(defvar gdb-display-string)
(defvar gdb-array-size)

(defun gdb-array-format1 ()
  (setq gdb-display-string "")
  (let ((buffer-read-only nil))
    (delete-region (point-min) (point-max))
    (let ((gdb-value-list (split-string gdb-values  ", ")))
      (string-match "\\({+\\)" (car gdb-value-list))
      (let* ((depth (- (match-end 1) (match-beginning 1)))
	     (indices  (make-vector depth '0))
	     (index 0) (num 0) (array-start "")
	     (array-stop "") (array-slice "") (array-range nil)
	     (flag t) (indices-string ""))
	(dolist (gdb-value gdb-value-list)
	  (string-match "{*\\([^}]*\\)\\(}*\\)" gdb-value)
	  (setq num 0)
	  (while (< num depth)
	    (setq indices-string
		  (concat indices-string
			  "[" (int-to-string (aref indices num)) "]"))
	    (if (not (= (aref gdb-array-start num) -1))
		(if (or (< (aref indices num) (aref gdb-array-start num))
			(> (aref indices num) (aref gdb-array-stop num)))
		    (setq flag nil))
	      (aset gdb-array-size num (aref indices num)))
	    (setq num (+ num 1)))
	  (if flag
	      (let ((gdb-display-value (match-string 1 gdb-value)))
		(setq gdb-display-string (concat gdb-display-string " "
						 gdb-display-value))
		(insert
		 (concat indices-string "\t" gdb-display-value "\n"))))
	  (setq indices-string "")
	  (setq flag t)
	  ;; 0<= index < depth, start at right : (- depth 1)
	  (setq index (- (- depth 1)
			 (- (match-end 2) (match-beginning 2))))
	  ;;don't set for very last brackets
	  (when (>= index 0)
	    (aset indices index (+ 1 (aref indices index)))
	    (setq num (+ 1 index))
	    (while (< num depth)
	      (aset indices num 0)
	      (setq num (+ num 1)))))
	(setq num 0)
	(while (< num depth)
	  (if (= (aref gdb-array-start num) -1)
	      (progn
		(aset gdb-array-start num 0)
		(aset gdb-array-stop num (aref indices num))))
	  (setq array-start (int-to-string (aref gdb-array-start num)))
	  (setq array-stop (int-to-string (aref gdb-array-stop num)))
	  (setq array-range (concat "[" array-start
				    ":" array-stop "]"))
	  (put-text-property 1 (+ (length array-start)
				  (length array-stop) 2)
			     'mouse-face 'highlight array-range)
	  (put-text-property 1 (+ (length array-start)
				  (length array-stop) 2)
			     'local-map gdb-array-slice-map array-range)
	  (goto-char (point-min))
	  (setq array-slice (concat array-slice array-range))
	  (setq num (+ num 1)))
	(goto-char (point-min))
	(insert "Array Size : ")
	(setq num 0)
	(while (< num depth)
	  (insert
	   (concat "["
		   (int-to-string (+ (aref gdb-array-size num) 1)) "]"))
	  (setq num (+ num 1)))
	(insert
	 (concat "\n     Slice : " array-slice "\n\nIndex\tValues\n\n"))))))

(defun gud-gdba-marker-filter (string)
  "A gud marker filter for gdb. Handle a burst of output from GDB."
  (let (
	;; Recall the left over burst from last time
	(burst (concat (gdb-get-burst) string))
	;; Start accumulating output for the GUD buffer
	(output ""))
    ;;
    ;; Process all the complete markers in this chunk.
    (while (string-match "\n\032\032\\(.*\\)\n" burst)
      (let ((annotation (match-string 1 burst)))
	;;
	;; Stuff prior to the match is just ordinary output.
	;; It is either concatenated to OUTPUT or directed
	;; elsewhere.
	(setq output
	      (gdb-concat-output
	       output
	       (substring burst 0 (match-beginning 0))))

	;; Take that stuff off the burst.
	(setq burst (substring burst (match-end 0)))

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
    ;; If it does, then keep part of the burst until we get more.
    (if (string-match "\n\\'\\|\n\032\\'\\|\n\032\032.*\\'"
		      burst)
	(progn
	  ;; Everything before the potential marker start can be output.
	  (setq output
		(gdb-concat-output output
				   (substring burst 0 (match-beginning 0))))
	  ;;
	  ;; Everything after, we save, to combine with later input.
	  (setq burst (substring burst (match-beginning 0))))
      ;;
      ;; In case we know the burst contains no partial annotations:
      (progn
	(setq output (gdb-concat-output output burst))
	(setq burst "")))
    ;;
    ;; Save the remaining burst for the next call to this function.
    (gdb-set-burst burst)
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
  (save-excursion
    (set-buffer
     (gdb-get-create-buffer 'gdb-partial-output-buffer))
    (goto-char (point-max))
    (insert string)))

(defun gdb-clear-partial-output ()
  (save-excursion
    (set-buffer
     (gdb-get-create-buffer 'gdb-partial-output-buffer))
    (delete-region (point-min) (point-max))))

(defun gdb-append-to-inferior-io (string)
  (save-excursion
    (set-buffer
     (gdb-get-create-buffer 'gdb-inferior-io))
    (goto-char (point-max))
    (insert-before-markers string))
  (if (not (string-equal string ""))
      (gdb-display-buffer
       (gdb-get-create-buffer 'gdb-inferior-io))))

(defun gdb-clear-inferior-io ()
  (save-excursion
    (set-buffer
     (gdb-get-create-buffer 'gdb-inferior-io))
    (delete-region (point-min) (point-max))))


;; One trick is to have a command who's output is always available in a buffer
;; of it's own, and is always up to date.  We build several buffers of this
;; type.
;;
;; There are two aspects to this: gdb has to tell us when the output for that
;; command might have changed, and we have to be able to run the command
;; behind the user's back.
;;
;; The idle input queue and the output phasing associated with the variable
;; gdb-output-sink help us to run commands behind the user's back.
;;
;; Below is the code for specificly managing buffers of output from one
;; command.
;;

;; The trigger function is suitable for use in the assoc GDB-ANNOTATION-RULES
;; It adds an idle input for the command we are tracking.  It should be the
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
	   (gdb-enqueue-idle-input
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
	    (save-excursion
	      (set-buffer buf)
	      (let ((p (point))
		    (buffer-read-only nil))
		(delete-region (point-min) (point-max))
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
(defvar breakpoint-enabled-icon)
(defvar breakpoint-disabled-icon)

;;-put breakpoint icons in relevant margins (even those set in the GUD buffer)
(defun gdb-info-breakpoints-custom ()
  (let ((flag)(address))
    ;;
    ;; remove all breakpoint-icons in source buffers but not assembler buffer
    (dolist (buffer (buffer-list))
      (save-excursion
	(set-buffer buffer)
	(if (and (eq gud-minor-mode 'gdba)
		 (not (string-match "^\*" (buffer-name))))
	    (if (display-graphic-p)
		(remove-images (point-min) (point-max))
	      (remove-strings (point-min) (point-max))))))
    (save-excursion
      (set-buffer (gdb-get-buffer 'gdb-breakpoints-buffer))
      (save-excursion
	(goto-char (point-min))
	(while (< (point) (- (point-max) 1))
	  (forward-line 1)
	  (if (looking-at "[^\t].*breakpoint")
	      (progn
		(looking-at "\\([0-9]*\\)\\s-*\\S-*\\s-*\\S-*\\s-*\\(.\\)")
		(setq flag (char-after (match-beginning 2)))
		(beginning-of-line)
		(if (re-search-forward "in\\s-+\\S-+\\s-+at\\s-+" nil t)
		    (progn
		      (looking-at "\\(\\S-*\\):\\([0-9]+\\)")
		      (let ((line (match-string 2)) (buffer-read-only nil)
			    (file (match-string 1)))
			(put-text-property (progn (beginning-of-line) (point))
					   (progn (end-of-line) (point))
					   'mouse-face 'highlight)
			(save-excursion
			  (set-buffer
			   (find-file-noselect
			    (if (file-exists-p file) file
			      (expand-file-name file gdb-cdir))))
			  (save-current-buffer
			    (set (make-local-variable 'gud-minor-mode) 'gdba)
			    (set (make-local-variable 'tool-bar-map)
				 gud-tool-bar-map)
			    (setq left-margin-width 2)
			    (if (get-buffer-window (current-buffer))
				(set-window-margins (get-buffer-window
						     (current-buffer))
						    left-margin-width
						    right-margin-width)))
			  ;; only want one breakpoint icon at each location
			  (save-excursion
			    (goto-line (string-to-number line))
			    (let ((start (progn (beginning-of-line)
						(- (point) 1)))
				  (end (progn (end-of-line) (+ (point) 1))))
			      (if (display-graphic-p)
				  (progn
				    (remove-images start end)
				    (if (eq ?y flag)
					(put-image breakpoint-enabled-icon
						   (point)
						   "breakpoint icon enabled"
						   'left-margin)
				      (put-image breakpoint-disabled-icon (point)
						 "breakpoint icon disabled"
						 'left-margin)))
				(remove-strings start end)
				(if (eq ?y flag)
				    (put-string "B" (point) "enabled"
						'left-margin)
				  (put-string "b" (point) "disabled"
					      'left-margin)))))))))))
	  (end-of-line))))))

(defun gdb-breakpoints-buffer-name ()
  (with-current-buffer gud-comint-buffer
    (concat "*breakpoints of " (gdb-get-target-string) "*")))

(defun gdb-display-breakpoints-buffer ()
  (interactive)
  (gdb-display-buffer
   (gdb-get-create-buffer 'gdb-breakpoints-buffer)))

(defun gdb-frame-breakpoints-buffer ()
  (interactive)
  (switch-to-buffer-other-frame
   (gdb-get-create-buffer 'gdb-breakpoints-buffer)))

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

(defvar gdb-source-window nil)

(defun gdb-goto-breakpoint ()
  "Display the file in the source buffer at the breakpoint specified on the
current line."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (re-search-forward "in\\s-+\\S-+\\s-+at\\s-+" nil t)
    (looking-at "\\(\\S-*\\):\\([0-9]+\\)"))
  (if (match-string 2)
      (let ((line (match-string 2))
	    (file (match-string 1)))
	(save-selected-window
	  (select-window gdb-source-window)
	  (switch-to-buffer (find-file-noselect
			     (if (file-exists-p file)
				 file
			       (expand-file-name file gdb-cdir))))
	  (goto-line (string-to-number line))))))

(defun gdb-mouse-goto-breakpoint (event)
  "Display the file in the source buffer at the selected breakpoint."
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
  (save-excursion
    (set-buffer (gdb-get-buffer 'gdb-stack-buffer))
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (< (point) (point-max))
	(put-text-property (progn (beginning-of-line) (point))
			   (progn (end-of-line) (point))
			   'mouse-face 'highlight)
	(forward-line 1)))))

(defun gdb-stack-buffer-name ()
  (with-current-buffer gud-comint-buffer
    (concat "*stack frames of " (gdb-get-target-string) "*")))

(defun gdb-display-stack-buffer ()
  (interactive)
  (gdb-display-buffer
   (gdb-get-create-buffer 'gdb-stack-buffer)))

(defun gdb-frame-stack-buffer ()
  (interactive)
  (switch-to-buffer-other-frame
   (gdb-get-create-buffer 'gdb-stack-buffer)))

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
  (gdb-invalidate-frames))

(defun gdb-get-frame-number ()
  (save-excursion
    (let* ((pos (re-search-backward "^#\\([0-9]*\\)" nil t))
	   (n (or (and pos (match-string-no-properties 1)) "0")))
      n)))

(defun gdb-frames-select ()
  "Make the frame on the current line become the current frame and display the
source in the source buffer."
  (interactive)
  (gdb-enqueue-input
   (list (concat "server frame " (gdb-get-frame-number) "\n") 'ignore))
  (gud-display-frame))

(defun gdb-frames-mouse-select (event)
  "Make the selected frame become the current frame and display the source in
the source buffer."
  (interactive "e")
  (mouse-set-point event)
  (gdb-frames-select))

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
  (interactive)
  (gdb-display-buffer
   (gdb-get-create-buffer 'gdb-registers-buffer)))

(defun gdb-frame-registers-buffer ()
  (interactive)
  (switch-to-buffer-other-frame
   (gdb-get-create-buffer 'gdb-registers-buffer)))

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
    (save-excursion
      (set-buffer buf)
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
    (and buf (save-excursion
	       (set-buffer buf)
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
  (interactive)
  (gdb-display-buffer
   (gdb-get-create-buffer 'gdb-locals-buffer)))

(defun gdb-frame-locals-buffer ()
  (interactive)
  (switch-to-buffer-other-frame
   (gdb-get-create-buffer 'gdb-locals-buffer)))

;;
;; Display expression buffer.
;;
(gdb-set-buffer-rules 'gdb-display-buffer
		      'gdb-display-buffer-name
		      'gdb-display-mode)

(def-gdb-auto-updated-buffer gdb-display-buffer
  ;; `gdb-display-buffer'.
  gdb-invalidate-display
  "server info display\n"
  gdb-info-display-handler
  gdb-info-display-custom)

(defun gdb-info-display-custom ()
  (let ((display-list nil))
    (save-excursion
      (set-buffer (gdb-get-buffer 'gdb-display-buffer))
      (goto-char (point-min))
      (while (< (point) (- (point-max) 1))
	(forward-line 1)
	(if (looking-at "\\([0-9]+\\):   \\([ny]\\)")
	    (setq display-list
		  (cons (string-to-int (match-string 1)) display-list)))
	(end-of-line)))
    (if (not (display-graphic-p))
	(progn
	  (dolist (buffer (buffer-list))
	    (if (string-match "\\*display \\([0-9]+\\)\\*" (buffer-name buffer))
		(progn
		  (let ((number
			 (match-string 1 (buffer-name buffer))))
		    (if (not (memq (string-to-int number) display-list))
			(kill-buffer
			 (get-buffer (concat "*display " number "*")))))))))
      (gdb-delete-frames display-list))))

(defun gdb-delete-frames (display-list)
  (dolist (frame (frame-list))
    (let ((frame-name (frame-parameter frame 'name)))
      (if (string-match "\\*display \\([0-9]+\\)\\*" frame-name)
	  (progn
	    (let ((number (match-string 1 frame-name)))
	      (if (not (memq (string-to-int number) display-list))
		  (progn (kill-buffer
			  (get-buffer (concat "*display " number "*")))
			 (delete-frame frame)))))))))

(defvar gdb-display-mode-map
  (let ((map (make-sparse-keymap))
	(menu (make-sparse-keymap "Display")))
    (define-key menu [toggle] '("Toggle" . gdb-toggle-display))
    (define-key menu [delete] '("Delete" . gdb-delete-display))

    (suppress-keymap map)
    (define-key map [menu-bar display] (cons "Display" menu))
    (define-key map " " 'gdb-toggle-display)
    (define-key map "d" 'gdb-delete-display)
    map))

(defun gdb-display-mode ()
  "Major mode for gdb display.

\\{gdb-display-mode-map}"
  (setq major-mode 'gdb-display-mode)
  (setq mode-name "Display")
  (setq buffer-read-only t)
  (use-local-map gdb-display-mode-map)
  (gdb-invalidate-display))

(defun gdb-display-buffer-name ()
  (with-current-buffer gud-comint-buffer
    (concat "*Displayed expressions of " (gdb-get-target-string) "*")))

(defun gdb-display-display-buffer ()
  (interactive)
  (gdb-display-buffer
   (gdb-get-create-buffer 'gdb-display-buffer)))

(defun gdb-frame-display-buffer ()
  (interactive)
  (switch-to-buffer-other-frame
   (gdb-get-create-buffer 'gdb-display-buffer)))

(defun gdb-toggle-display ()
  "Enable/disable the displayed expression at current line."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (if (not (looking-at "\\([0-9]+\\):   \\([ny]\\)"))
	(error "No expression on this line")
      (gdb-enqueue-input
       (list
	(concat
	 (if (eq ?y (char-after (match-beginning 2)))
	     "server disable display "
	   "server enable display ")
	 (match-string 1) "\n")
	'ignore)))))

(defun gdb-delete-display ()
  "Delete the displayed expression at current line."
  (interactive)
  (save-excursion
    (set-buffer
     (gdb-get-buffer 'gdb-display-buffer))
    (beginning-of-line 1)
    (if (not (looking-at "\\([0-9]+\\):   \\([ny]\\)"))
	(error "No expression on this line")
      (let ((number (match-string 1)))
	(gdb-enqueue-input
	 (list (concat "server delete display " number "\n") 'ignore))))))

(defvar gdb-expressions-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "v" 'gdb-array-visualise)
    (define-key map "q" 'gdb-delete-expression)
    (define-key map [mouse-3] 'gdb-expressions-popup-menu)
    map))

(defvar gdb-expressions-mode-menu
  '("GDB Expressions Commands"
    "----"
    ["Visualise" gdb-array-visualise t]
    ["Delete" 	 gdb-delete-expression  t])
  "Menu for `gdb-expressions-mode'.")

(defun gdb-expressions-popup-menu (event)
  "Explicit Popup menu as this buffer doesn't have a menubar."
  (interactive "@e")
  (mouse-set-point event)
  (popup-menu gdb-expressions-mode-menu))

(defun gdb-expressions-mode ()
  "Major mode for display expressions.

\\{gdb-expressions-mode-map}"
  (setq major-mode 'gdb-expressions-mode)
  (setq mode-name "Expressions")
  (use-local-map gdb-expressions-mode-map)
  (make-local-variable 'gdb-display-number)
  (make-local-variable 'gdb-values)
  (make-local-variable 'gdb-expression)
  (set (make-local-variable 'gdb-display-string) nil)
  (set (make-local-variable 'gdb-dive-display-number) nil)
  (set (make-local-variable 'gud-minor-mode) 'gdba)
  (set (make-local-variable 'gdb-array-start) (make-vector 16 '-1))
  (set (make-local-variable 'gdb-array-stop)  (make-vector 16 '-1))
  (set (make-local-variable 'gdb-array-size)  (make-vector 16 '-1))
  (setq buffer-read-only t))


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
	   '(lambda (win)
	      (if (or (eq gud-comint-buffer (window-buffer win))
		      (eq gdb-source-window win))
		  (set-window-dedicated-p win t))))
	  (setq answer (get-buffer-window buf))
	  (if (not answer)
	      (let ((window (get-lru-window)))
		(if window
		    (progn
		      (set-window-buffer window buf)
		      (setq answer window))
		  (setq must-split t)))))
      (walk-windows
       '(lambda (win)
	  (if (or (eq gud-comint-buffer (window-buffer win))
		  (eq gdb-source-window win))
	      (set-window-dedicated-p win nil)))))
    (if must-split
	(let* ((largest (get-largest-window))
	       (cur-size (window-height largest))
	       (new-size (and size (< size cur-size) (- cur-size size))))
	  (setq answer (split-window largest new-size))
	  (set-window-buffer answer buf)))
    answer))

(defun gdb-display-source-buffer (buffer)
  (set-window-buffer gdb-source-window buffer)
  gdb-source-window)


;;; Shared keymap initialization:

(defun gdb-display-gdb-buffer ()
  (interactive)
  (gdb-display-buffer
   (gdb-get-create-buffer 'gdba)))

(let ((menu (make-sparse-keymap "GDB-Windows")))
  (define-key gud-menu-map [displays]
    `(menu-item "GDB-Windows" ,menu :visible (eq gud-minor-mode 'gdba)))
  (define-key menu [gdb] '("Gdb" . gdb-display-gdb-buffer))
  (define-key menu [locals] '("Locals" . gdb-display-locals-buffer))
  (define-key menu [registers] '("Registers" . gdb-display-registers-buffer))
  (define-key menu [frames] '("Stack" . gdb-display-stack-buffer))
  (define-key menu [breakpoints] '("Breakpoints" . gdb-display-breakpoints-buffer))
  (define-key menu [display] '("Display" . gdb-display-display-buffer))
  (define-key menu [assembler] '("Assembler" . gdb-display-assembler-buffer)))

(defun gdb-frame-gdb-buffer ()
  (interactive)
  (switch-to-buffer-other-frame
   (gdb-get-create-buffer 'gdba)))

(let ((menu (make-sparse-keymap "GDB-Frames")))
  (define-key gud-menu-map [frames]
    `(menu-item "GDB-Frames" ,menu :visible (eq gud-minor-mode 'gdba)))
  (define-key menu [gdb] '("Gdb" . gdb-frame-gdb-buffer))
  (define-key menu [locals] '("Locals" . gdb-frame-locals-buffer))
  (define-key menu [registers] '("Registers" . gdb-frame-registers-buffer))
  (define-key menu [frames] '("Stack" . gdb-frame-stack-buffer))
  (define-key menu [breakpoints] '("Breakpoints" . gdb-frame-breakpoints-buffer))
  (define-key menu [display] '("Display" . gdb-frame-display-buffer))
  (define-key menu [assembler] '("Assembler" . gdb-frame-assembler-buffer)))

(defvar gdb-main-file nil "Source file from which program execution begins.")

;; layout for all the windows
(defun gdb-setup-windows ()
  (gdb-display-locals-buffer)
  (gdb-display-stack-buffer)
  (delete-other-windows)
  (gdb-display-breakpoints-buffer)
  (gdb-display-display-buffer)
  (delete-other-windows)
  (split-window nil ( / ( * (window-height) 3) 4))
  (split-window nil ( / (window-height) 3))
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer (gdb-locals-buffer-name))
  (other-window 1)
  (switch-to-buffer
   (if gud-last-last-frame
       (gud-find-file (car gud-last-last-frame))
     (gud-find-file gdb-main-file)))
  (setq gdb-source-window (get-buffer-window (current-buffer)))
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer (gdb-inferior-io-name))
  (other-window 1)
  (switch-to-buffer (gdb-stack-buffer-name))
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer (gdb-breakpoints-buffer-name))
  (other-window 1))

(define-minor-mode gdb-many-windows
  "Toggle the number of windows in the basic arrangement."
  :group 'gud
  :init-value nil
  (gdb-restore-windows))

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
     (if gud-last-last-frame
	 (gud-find-file (car gud-last-last-frame))
       (gud-find-file gdb-main-file)))
    (other-window 1)))

(defconst breakpoint-xpm-data "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"12 12 2 1\",
\"  c red\",
\"+ c None\",
/* pixels */
\"+++++  +++++\",
\"+++      +++\",
\"++        ++\",
\"+          +\",
\"+          +\",
\"            \",
\"            \",
\"+          +\",
\"+          +\",
\"++        ++\",
\"+++      +++\",
\"+++++  +++++\"
};"
  "XPM file used for breakpoint icon.")

(defvar breakpoint-enabled-icon
  (find-image `((:type xpm :data ,breakpoint-xpm-data)))
  "Icon for enabled breakpoint in display margin")
(defvar breakpoint-disabled-icon
  (find-image `((:type xpm :data ,breakpoint-xpm-data
		       :conversion laplace)))
  "Icon for disabled breakpoint in display margin")

(defun gdb-reset ()
  "Exit a debugging session cleanly by killing the gdb buffers and resetting
 the source buffers."
  (gdb-delete-frames '())
  (dolist (buffer (buffer-list))
    (if (not (eq buffer gud-comint-buffer))
	(with-current-buffer buffer
	  (if (eq gud-minor-mode 'gdba)
	      (if (string-match "^\*.+*$" (buffer-name))
		  (kill-buffer nil)
		(if (display-graphic-p)
		    (remove-images (point-min) (point-max))
		  (remove-strings (point-min) (point-max)))
		(setq left-margin-width 0)
		(setq gud-minor-mode nil)
		(kill-local-variable 'tool-bar-map)
		(setq gud-running nil)
		(if (get-buffer-window (current-buffer))
		    (set-window-margins (get-buffer-window
					 (current-buffer))
					left-margin-width
					right-margin-width))))))))

(defun gdb-source-info ()
  "Find the source file where the program starts and displays it with related
buffers."
  (goto-char (point-min))
  (when (search-forward "directory is " nil t)
    (looking-at "\\S-*")
    (setq gdb-cdir (match-string 0))
    (search-forward "Located in ")
    (looking-at "\\S-*")
    (setq gdb-main-file (match-string 0))
    ;; Make sure we are not in the minibuffer window when we try to delete
    ;; all other windows.
    (if (window-minibuffer-p (selected-window))
	(other-window 1))
    (delete-other-windows)
    (if gdb-many-windows
	(gdb-setup-windows)
      (gdb-display-breakpoints-buffer)
      (gdb-display-display-buffer)
      (delete-other-windows)
      (split-window)
      (other-window 1)
      (switch-to-buffer (gud-find-file gdb-main-file))
      (setq gdb-source-window (get-buffer-window (current-buffer)))
      (other-window 1))))

;;from put-image
(defun put-string (putstring pos &optional string area)
  "Put string PUTSTRING in front of POS in the current buffer.
PUTSTRING is displayed by putting an overlay into the current buffer with a
`before-string' STRING that has a `display' property whose value is
PUTSTRING.  STRING is defaulted if you omit it.
POS may be an integer or marker.
AREA is where to display the string.  AREA nil or omitted means
display it in the text area, a value of `left-margin' means
display it in the left marginal area, a value of `right-margin'
means display it in the right marginal area."
  (unless string (setq string "x"))
  (let ((buffer (current-buffer)))
    (unless (or (null area) (memq area '(left-margin right-margin)))
      (error "Invalid area %s" area))
    (setq string (copy-sequence string))
    (let ((overlay (make-overlay pos pos buffer))
	  (prop (if (null area) putstring (list (list 'margin area) putstring))))
      (put-text-property 0 (length string) 'display prop string)
      (overlay-put overlay 'put-text t)
      (overlay-put overlay 'before-string string))))

;;from remove-images
(defun remove-strings (start end &optional buffer)
  "Remove strings between START and END in BUFFER.
Remove only images that were put in BUFFER with calls to `put-string'.
BUFFER nil or omitted means use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (let ((overlays (overlays-in start end)))
    (while overlays
      (let ((overlay (car overlays)))
	(when (overlay-get overlay 'put-text)
	  (delete-overlay overlay)))
      (setq overlays (cdr overlays)))))

(defun put-arrow (putstring pos &optional string area)
  "Put arrow string PUTSTRING in front of POS in the current buffer.
PUTSTRING is displayed by putting an overlay into the current buffer with a
`before-string' \"gdb-arrow\" that has a `display' property whose value is
PUTSTRING. STRING is defaulted if you omit it.
POS may be an integer or marker.
AREA is where to display the string.  AREA nil or omitted means
display it in the text area, a value of `left-margin' means
display it in the left marginal area, a value of `right-margin'
means display it in the right marginal area."
  (setq string "gdb-arrow")
  (let ((buffer (current-buffer)))
    (unless (or (null area) (memq area '(left-margin right-margin)))
      (error "Invalid area %s" area))
    (setq string (copy-sequence string))
    (let ((overlay (make-overlay pos pos buffer))
	  (prop (if (null area) putstring (list (list 'margin area) putstring))))
      (put-text-property 0 (length string) 'display prop string)
      (overlay-put overlay 'put-text t)
      (overlay-put overlay 'before-string string))))

(defun remove-arrow (&optional buffer)
  "Remove arrow in BUFFER.
Remove only images that were put in BUFFER with calls to `put-arrow'.
BUFFER nil or omitted means use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (let ((overlays (overlays-in (point-min) (point-max))))
    (while overlays
      (let ((overlay (car overlays)))
	(when (string-equal (overlay-get overlay 'before-string) "gdb-arrow")
	  (delete-overlay overlay)))
      (setq overlays (cdr overlays)))))

(defun gdb-array-visualise ()
  "Visualise arrays and slices using graph program from plotutils."
  (interactive)
  (when (and (display-graphic-p) gdb-display-string)
    (let ((n 0) m)
      (catch 'multi-dimensional
	(while (eq (aref gdb-array-start n) (aref gdb-array-stop n))
	  (setq n (+ n 1)))
	(setq m (+ n 1))
	(while (< m (length gdb-array-start))
	  (if (not (eq (aref gdb-array-start m) (aref gdb-array-stop m)))
	      (progn
		(x-popup-dialog
		 t `(,(concat "Only one dimensional data can be visualised.\n"
			      "Use an array slice to reduce the number of\n"
			      "dimensions") ("OK" t)))
		(throw 'multi-dimensional nil))
	    (setq m (+ m 1))))
	(shell-command (concat "echo" gdb-display-string " | graph -a 1 "
			       (int-to-string (aref gdb-array-start n))
			       " -x "
			       (int-to-string (aref gdb-array-start n))
			       " "
			       (int-to-string (aref gdb-array-stop  n))
			       " 1 -T X"))))))

(defun gdb-delete-expression ()
  "Delete displayed expression and its frame."
  (interactive)
  (gdb-enqueue-input
   (list (concat "server delete display " gdb-display-number "\n")
	 'ignore)))

;;
;; Assembler buffer.
;;
(gdb-set-buffer-rules 'gdb-assembler-buffer
		      'gdb-assembler-buffer-name
		      'gdb-assembler-mode)

(def-gdb-auto-updated-buffer gdb-assembler-buffer
  gdb-invalidate-assembler
  (concat "server disassemble " gdb-main-or-pc "\n")
  gdb-assembler-handler
  gdb-assembler-custom)

(defun gdb-assembler-custom ()
  (let ((buffer (gdb-get-buffer 'gdb-assembler-buffer))
	(gdb-arrow-position) (address) (flag))
    (if gdb-current-address
	(progn
	  (save-excursion
	    (set-buffer buffer)
	    (remove-arrow)
	    (goto-char (point-min))
	    (re-search-forward gdb-current-address)
	    (setq gdb-arrow-position (point))
	    (put-arrow "=>" gdb-arrow-position nil 'left-margin))))
    ;; remove all breakpoint-icons in assembler buffer  before updating.
    (save-excursion
      (set-buffer buffer)
      (if (display-graphic-p)
	  (remove-images (point-min) (point-max))
	(remove-strings (point-min) (point-max))))
    (save-excursion
      (set-buffer (gdb-get-buffer 'gdb-breakpoints-buffer))
      (goto-char (point-min))
      (while (< (point) (- (point-max) 1))
	(forward-line 1)
	(if (looking-at "[^\t].*breakpoint")
	    (progn
	      (looking-at
	       "\\([0-9]*\\)\\s-*\\S-*\\s-*\\S-*\\s-*\\(.\\)\\s-*0x0\\(\\S-*\\)")
	      ;; info break gives '0x0' (8 digit) while dump gives '0x' (7 digit)
	      (setq address (concat "0x" (match-string 3)))
	      (setq flag (char-after (match-beginning 2)))
	      (save-excursion
		(set-buffer buffer)
		(goto-char (point-min))
		(if (re-search-forward address nil t)
		    (let ((start (progn (beginning-of-line) (- (point) 1)))
			  (end (progn (end-of-line) (+ (point) 1))))
		      (if (display-graphic-p)
			  (progn
			    (remove-images start end)
			    (if (eq ?y flag)
				(put-image breakpoint-enabled-icon (point)
					   "breakpoint icon enabled"
					   'left-margin)
			      (put-image breakpoint-disabled-icon (point)
					 "breakpoint icon disabled"
					 'left-margin)))
			(remove-strings start end)
			(if (eq ?y flag)
			    (put-string "B" (point) "enabled" 'left-margin)
			  (put-string "b" (point) "disabled"
				      'left-margin))))))))))
    (if gdb-current-address
	(set-window-point (get-buffer-window buffer) gdb-arrow-position))))

(defvar gdb-assembler-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    map))

(defun gdb-assembler-mode ()
  "Major mode for viewing code assembler.

\\{gdb-assembler-mode-map}"
  (setq major-mode 'gdb-assembler-mode)
  (setq mode-name "Assembler")
  (setq left-margin-width 2)
  (setq buffer-read-only t)
  (use-local-map gdb-assembler-mode-map)
  (gdb-invalidate-assembler)
  (gdb-invalidate-breakpoints))

(defun gdb-assembler-buffer-name ()
  (with-current-buffer gud-comint-buffer
    (concat "*Machine Code " (gdb-get-target-string) "*")))

(defun gdb-display-assembler-buffer ()
  (interactive)
  (gdb-display-buffer
   (gdb-get-create-buffer 'gdb-assembler-buffer)))

(defun gdb-frame-assembler-buffer ()
  (interactive)
  (switch-to-buffer-other-frame
   (gdb-get-create-buffer 'gdb-assembler-buffer)))

(defun gdb-invalidate-frame-and-assembler (&optional ignored)
  (gdb-invalidate-frames)
  (gdb-invalidate-assembler))

(defun gdb-invalidate-breakpoints-and-assembler (&optional ignored)
  (gdb-invalidate-breakpoints)
  (gdb-invalidate-assembler))

(defvar gdb-prev-main-or-pc nil)

;; modified because if gdb-main-or-pc has changed value a new command
;; must be enqueued to update the buffer with the new output
(defun gdb-invalidate-assembler (&optional ignored)
  (if (and (gdb-get-buffer 'gdb-assembler-buffer)
	   (or (not (member 'gdb-invalidate-assembler
			    (gdb-get-pending-triggers)))
	       (not (string-equal gdb-main-or-pc gdb-prev-main-or-pc))))
      (progn
	;; take previous disassemble command off the queue
	(save-excursion
	  (set-buffer gud-comint-buffer)
	  (let ((queue gdb-idle-input-queue) (item))
	    (while queue
	      (setq item (car queue))
	      (if (equal (cdr item) '(gdb-assembler-handler))
		  (delete item gdb-idle-input-queue))
	      (setq queue (cdr queue)))))
	(gdb-enqueue-idle-input
	 (list (concat "server disassemble " gdb-main-or-pc "\n")
	       'gdb-assembler-handler))
	(gdb-set-pending-triggers
	 (cons 'gdb-invalidate-assembler
	       (gdb-get-pending-triggers)))
	(setq gdb-prev-main-or-pc gdb-main-or-pc))))

(defun gdb-get-current-frame ()
  (if (not (member 'gdb-get-current-frame (gdb-get-pending-triggers)))
      (progn
	(gdb-enqueue-idle-input
	 (list (concat "server frame\n") 'gdb-frame-handler))
	(gdb-set-pending-triggers
	 (cons 'gdb-get-current-frame
	       (gdb-get-pending-triggers))))))

(defun gdb-frame-handler ()
  (gdb-set-pending-triggers
   (delq 'gdb-get-current-frame (gdb-get-pending-triggers)))
  (save-excursion
    (set-buffer (gdb-get-create-buffer 'gdb-partial-output-buffer))
    (goto-char (point-min))
    (if (looking-at "^#[0-9]*\\s-*0x\\S-* in \\(\\S-*\\)")
	(setq gdb-current-frame (match-string 1))
      (if (looking-at "^#[0-9]*\\s-*\\(\\S-*\\)")
	  (setq gdb-current-frame (match-string 1))))))

(provide 'gdb-ui)

;;; gdb-ui.el ends here
