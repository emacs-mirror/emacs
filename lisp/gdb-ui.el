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

;;  Extension of gdba.el written by Jim Kingdon from gdb 5.0

;;; Code:

(require 'mygud)

(defcustom gdb-many-windows t
  "If t, using gdba, start gdb with ancillary buffers visible.
Use `toggle-gdb-windows' to change this value during a gdb session"
  :type 'boolean
  :group 'gud)

(defvar gdb-main-file nil "Source file from which program execution begins.")
(defvar gdb-cdir nil "Compilation directory.")
(defvar gdb-main-or-pc nil "Initialisation for Assembler buffer.")
(defvar gdb-prev-main-or-pc nil)

(defun gdba (command-line)
  "Run gdb on program FILE in buffer *gdb-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

If `gdb-many-windows' is set to t this works best in X (depending on the size
of your monitor) using most of the screen. After a short delay the following
layout will appear (keybindings given in relevant buffer) :

---------------------------------------------------------------------
                               GDB Toolbar
---------------------------------------------------------------------
GUD buffer (I/O of gdb)           | Locals buffer
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
\[mouse-2\]   gdb-frames-select     | SPC    gdb-toggle-bp-this-line
                                  |   g    gdb-goto-bp-this-line
                                  |   d    gdb-delete-bp-this-line
---------------------------------------------------------------------

All the buffers share the toolbar and source should always display in the same
window e.g after typing g on a breakpoint in the breakpoints buffer. Breakpoint
icons are displayed both by setting a break with gud-break and by typing break
in the GUD buffer.

Displayed expressions appear in separate frames. Arrays may be displayed
as slices and visualised using the graph program from plotutils if installed.
 
If `gdb-many-windows' is set to nil then gdb starts with just two windows :
the GUD and the source buffer.

The following interactive lisp functions help control operation :

`toggle-gdb-windows'  - Toggle the number of windows gdb uses.
`gdb-restore-windows' - to restore the layout if its lost.
`gdb-quit'            - to delete (most) of the buffers used by gdb."

  (interactive (list (gud-query-cmdline 'gdba)))

  (gdba-common-init command-line nil
		   'gdba-marker-filter 'gud-gdb-find-file)

  (set (make-local-variable 'gud-minor-mode) 'gdba)

;  (gud-def gud-break  "break %f:%l"  "\C-b" "Set breakpoint at current line.")
  (gud-def gud-tbreak "tbreak %f:%l" "\C-t" "Set breakpoint at current line.")
;  (gud-def gud-remove "clear %f:%l"  "\C-d" "Remove breakpoint at current line")
  (gud-def gud-run    "run"	     nil    "Run the program.")
  (gud-def gud-stepi  "stepi %p"     "\C-i" "Step one instruction with display.")
  (gud-def gud-step   "step %p"      "\C-s" "Step one source line with display.")
  (gud-def gud-next   "next %p"      "\C-n" "Step one line (skip functions).")
  (gud-def gud-finish "finish"       "\C-f" "Finish executing current function.")
  (gud-def gud-cont   "cont"         "\C-r" "Continue with display.")
  (gud-def gud-up     "up %p"        "<" "Up N stack frames (numeric arg).")
  (gud-def gud-down   "down %p"      ">" "Down N stack frames (numeric arg).")
  (gud-def gud-print  "print %e"     "\C-p" "Evaluate C expression at point.")
  (gud-def gud-goto  "until %f:%l"     "\C-u" "Continue up to current line.")

  (define-key gud-mode-map "\C-c\C-b" 'gud-break)
  (define-key global-map "\C-x\C-a\C-b" 'gud-break)

  (define-key gud-mode-map "\C-c\C-d" 'gud-remove)
  (define-key global-map "\C-x\C-a\C-d" 'gud-remove)

  (local-set-key "\C-i" 'gud-gdb-complete-command)

  (setq comint-prompt-regexp "^(.*gdb[+]?) *")
  (setq comint-input-sender 'gdb-send)

; (re-)initialise
  (setq gdb-main-or-pc "main")
  (setq gdb-current-address nil)
  (setq gdb-display-in-progress nil)
  (setq gdb-dive nil)
  (setq gud-last-last-frame nil)

  (run-hooks 'gdb-mode-hook)
  (let ((instance
	 (make-gdb-instance (get-buffer-process (current-buffer)))))
    (if gdb-first-time (gdb-clear-inferior-io instance))

; find source file and compilation directory here
    (gdb-instance-enqueue-idle-input instance (list "server list\n"
					       '(lambda () nil)))
    (gdb-instance-enqueue-idle-input instance (list "server info source\n"
				'(lambda () (gdb-source-info))))))

(defun gud-break (arg)
  "Set breakpoint at current line or address."
  (interactive "p")
  (if (not (string-equal mode-name "Assembler"))
      (gud-call "break %f:%l" arg)
;else
    (save-excursion
      (beginning-of-line)
      (forward-char 2)
      (gud-call "break *%a" arg))))

(defun gud-remove (arg)
  "Remove breakpoint at current line or address."
  (interactive "p")
  (if (not (string-equal mode-name "Assembler"))
    (gud-call "clear %f:%l" arg)
;else
    (save-excursion
      (beginning-of-line)
      (forward-char 2)
      (gud-call "clear *%a" arg))))

(defun gud-display ()
  "Display (possibly dereferenced) C expression at point."
  (interactive)
  (save-excursion
    (let ((expr (gud-find-c-expr)))
      (gdb-instance-enqueue-idle-input
       gdb-buffer-instance
       (list (concat "server whatis " expr "\n")
	     `(lambda () (gud-display1 ,expr)))))))

(defun gud-display1 (expr)
  (goto-char (point-min))
    (if (re-search-forward "\*" nil t)
	(gdb-instance-enqueue-idle-input
	 gdb-buffer-instance
	 (list (concat "server display* " expr "\n")
	       '(lambda () nil)))
;else
      (gdb-instance-enqueue-idle-input
       gdb-buffer-instance
       (list (concat "server display " expr "\n")
	     '(lambda () nil)))))


;; The completion process filter is installed temporarily to slurp the
;; output of GDB up to the next prompt and build the completion list.
;; It must also handle annotations.
(defun gdba-complete-filter (string)
  (gdb-output-burst gdb-buffer-instance string)
  (while (string-match "\n\032\032\\(.*\\)\n" string)
    (setq string (concat (substring string 0 (match-beginning 0))
			 (substring string (match-end 0)))))
  (setq string (concat gud-gdb-complete-string string))
  (while (string-match "\n" string)
    (setq gud-gdb-complete-list
	  (cons (substring string gud-gdb-complete-break (match-beginning 0))
		gud-gdb-complete-list))
    (setq string (substring string (match-end 0))))
  (if (string-match comint-prompt-regexp string)
      (progn
	(setq gud-gdb-complete-in-progress nil)
	string)
    (progn
      (setq gud-gdb-complete-string string)
      "")))


(defun gdba-common-init (command-line massage-args marker-filter &optional find-file)

  (let* ((words (split-string command-line))
	 (program (car words))

	 ;; Extract the file name from WORDS
	 ;; and put t in its place.
	 ;; Later on we will put the modified file name arg back there.
	 (file-word (let ((w (cdr words)))
		      (while (and w (= ?- (aref (car w) 0)))
			(setq w (cdr w)))
		      (and w
			   (prog1 (car w)
			     (setcar w t)))))
	 (file-subst
	  (and file-word (substitute-in-file-name file-word)))

	 (args (cdr words))

	 ;; If a directory was specified, expand the file name.
	 ;; Otherwise, don't expand it, so GDB can use the PATH.
	 ;; A file name without directory is literally valid
	 ;; only if the file exists in ., and in that case,
	 ;; omitting the expansion here has no visible effect.
	 (file (and file-word
		    (if (file-name-directory file-subst)
			(expand-file-name file-subst)
		      file-subst)))
	 (filepart (and file-word (file-name-nondirectory file)))
	 (buffer-name (concat "*gdb-" filepart "*")))

    (setq gdb-first-time (not (get-buffer-process buffer-name)))

    (switch-to-buffer buffer-name)
    ;; Set default-directory to the file's directory.
    (and file-word
	 gud-chdir-before-run
	 ;; Don't set default-directory if no directory was specified.
	 ;; In that case, either the file is found in the current directory,
	 ;; in which case this setq is a no-op,
	 ;; or it is found by searching PATH,
	 ;; in which case we don't know what directory it was found in.
	 (file-name-directory file)
	 (setq default-directory (file-name-directory file)))
    (or (bolp) (newline))
    (insert "Current directory is " default-directory "\n")
    ;; Put the substituted and expanded file name back in its place.
    (let ((w args))
      (while (and w (not (eq (car w) t)))
	(setq w (cdr w)))
      (if w
	  (setcar w file)))
    (let ((old-instance gdb-buffer-instance))
      (apply 'make-comint (concat "gdb-" filepart) program nil args)
      (gud-mode)
      (make-variable-buffer-local 'old-gdb-buffer-instance)
      (setq old-gdb-buffer-instance old-instance))
    (setq gdb-target-name filepart))
  (make-local-variable 'gud-marker-filter)
  (setq gud-marker-filter marker-filter)
  (if find-file (set (make-local-variable 'gud-find-file) find-file))

  (set-process-filter (get-buffer-process (current-buffer)) 'gud-filter)
  (set-process-sentinel (get-buffer-process (current-buffer)) 'gud-sentinel)
  (gud-set-buffer))


;; ======================================================================
;;
;; In this world, there are gdb instance objects (of unspecified
;; representation) and buffers associated with those objects.
;;

;; 
;; gdb-instance objects
;; 

(defun make-gdb-instance (proc)
  "Create a gdb instance object from a gdb process."
  (setq last-proc proc)
  (let ((instance (cons 'gdb-instance proc)))
    (save-excursion
      (set-buffer (process-buffer proc))
      (setq gdb-buffer-instance instance)
      (progn
	(mapcar 'make-variable-buffer-local gdb-instance-variables)
	(setq gdb-buffer-type 'gdba)
	;; If we're taking over the buffer of another process,
	;; take over it's ancillery buffers as well.
	;;
	(let ((dead (or old-gdb-buffer-instance)))
	  (mapcar
	   (function
	    (lambda (b)
	      (progn
		(set-buffer b)
		(if (eq dead gdb-buffer-instance)
		    (setq gdb-buffer-instance instance)))))
	     (buffer-list)))))
    instance))

(defun gdb-instance-process (inst) (cdr inst))

;;; The list of instance variables is built up by the expansions of
;;; DEF-GDB-VARIABLE
;;;
(defvar gdb-instance-variables '()
  "A list of variables that are local to the GUD buffer associated
with a gdb instance.")

(defmacro def-gdb-variable
  (name accessor setter &optional default doc)
  `(progn
     (defvar ,name ,default ,(or doc "undocumented"))
     (if (not (memq ',name gdb-instance-variables))
	 (setq gdb-instance-variables
	       (cons ',name gdb-instance-variables)))
     ,(and accessor
	     `(defun ,accessor (instance)
		(let
		    ((buffer (gdb-get-instance-buffer instance 'gdba)))
		  (and buffer
		       (save-excursion
			 (set-buffer buffer)
			 ,name)))))
     ,(and setter
	     `(defun ,setter (instance val)
		(let
		    ((buffer (gdb-get-instance-buffer instance 'gdba)))
		  (and buffer
		       (save-excursion
			 (set-buffer buffer)
			 (setq ,name val))))))))

(defmacro def-gdb-var (root-symbol &optional default doc)
  (let* ((root (symbol-name root-symbol))
	 (accessor (intern (concat "gdb-instance-" root)))
	 (setter (intern (concat "set-gdb-instance-" root)))
	 (var-name (intern (concat "gdb-" root))))
    `(def-gdb-variable
	 ,var-name ,accessor ,setter
	 ,default ,doc)))

(def-gdb-var buffer-instance nil
  "In an instance buffer, the buffer's instance.")

(def-gdb-var buffer-type nil
  "One of the symbols bound in gdb-instance-buffer-rules")

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

(defun in-gdb-instance-context (instance form)
  "Funcall FORM in the GUD buffer of INSTANCE."
  (save-excursion
    (set-buffer (gdb-get-instance-buffer instance 'gdba))
    (funcall form)))

;; end of instance vars

;;
;; finding instances
;;

(defun gdb-proc->instance (proc)
  (save-excursion
    (set-buffer (process-buffer proc))
    gdb-buffer-instance))

(defun gdb-mru-instance-buffer ()
  "Return the most recently used (non-auxiliary) GUD buffer."
  (save-excursion
    (gdb-goto-first-gdb-instance (buffer-list))))

(defun gdb-goto-first-gdb-instance (blist)
  "Use gdb-mru-instance-buffer -- not this."
  (and blist
       (progn
	 (set-buffer (car blist))
	 (or (and gdb-buffer-instance
		  (eq gdb-buffer-type 'gdba)
		  (car blist))
	     (gdb-goto-first-gdb-instance (cdr blist))))))

(defun buffer-gdb-instance (buf)
  (save-excursion
    (set-buffer buf)
    gdb-buffer-instance))

(defun gdb-needed-default-instance ()
  "Return the most recently used gdb instance or signal an error."
  (let ((buffer (gdb-mru-instance-buffer)))
    (or (and buffer (buffer-gdb-instance buffer))
	(error "No instance of gdb found"))))

(defun gdb-instance-target-string (instance)
  "The apparent name of the program being debugged by a gdb instance.
For sure this the root string used in smashing together the gdb
buffer's name, even if that doesn't happen to be the name of a
program."
  (in-gdb-instance-context
   instance
   (function (lambda () gdb-target-name))))



;;
;; Instance Buffers.
;;

;; More than one buffer can be associated with a gdb instance.
;;
;; Each buffer has a TYPE -- a symbol that identifies the function
;; of that particular buffer.
;;
;; The usual gdb interaction buffer is given the type `gdb' and
;; is constructed specially.
;;
;; Others are constructed by gdb-get-create-instance-buffer and
;; named according to the rules set forth in the gdb-instance-buffer-rules-assoc

(defun gdb-get-instance-buffer (instance key)
  "Return the instance buffer for INSTANCE tagged with type KEY.
The key should be one of the cars in `gdb-instance-buffer-rules-assoc'."
  (save-excursion
    (gdb-look-for-tagged-buffer instance key (buffer-list))))

(defun gdb-get-create-instance-buffer (instance key)
  "Create a new gdb instance buffer of the type specified by KEY.
The key should be one of the cars in `gdb-instance-buffer-rules-assoc'."
  (or (gdb-get-instance-buffer instance key)
      (let* ((rules (assoc key gdb-instance-buffer-rules-assoc))
	     (name (funcall (gdb-rules-name-maker rules) instance))
	     (new (get-buffer-create name)))
	(save-excursion
	  (set-buffer new)
	  (make-variable-buffer-local 'gdb-buffer-type)
	  (setq gdb-buffer-type key)
	  (make-variable-buffer-local 'gdb-buffer-instance)
	  (setq gdb-buffer-instance instance)
	  (if (cdr (cdr rules))
	      (funcall (car (cdr (cdr rules)))))
	  new))))

(defun gdb-rules-name-maker (rules) (car (cdr rules)))

(defun gdb-look-for-tagged-buffer (instance key bufs)
  (let ((retval nil))
    (while (and (not retval) bufs)
      (set-buffer (car bufs))
      (if (and (eq gdb-buffer-instance instance)
	       (eq gdb-buffer-type key))
	  (setq retval (car bufs)))
      (setq bufs (cdr bufs)))
    retval))

(defun gdb-instance-buffer-p (buf)
  (save-excursion
    (set-buffer buf)
    (and gdb-buffer-type
	 (not (eq gdb-buffer-type 'gdba)))))

;;
;; This assoc maps buffer type symbols to rules.  Each rule is a list of
;; at least one and possible more functions.  The functions have these
;; roles in defining a buffer type:
;;
;;     NAME - take an instance, return a name for this type buffer for that
;;	      instance.
;; The remaining function(s) are optional:
;;
;;     MODE - called in new new buffer with no arguments, should establish
;;	      the proper mode for the buffer.
;;

(defvar gdb-instance-buffer-rules-assoc '())

(defun gdb-set-instance-buffer-rules (buffer-type &rest rules)
  (let ((binding (assoc buffer-type gdb-instance-buffer-rules-assoc)))
    (if binding
	(setcdr binding rules)
      (setq gdb-instance-buffer-rules-assoc
	    (cons (cons buffer-type rules)
		  gdb-instance-buffer-rules-assoc)))))

; GUD buffers are an exception to the rules
(gdb-set-instance-buffer-rules 'gdba 'error)

;;
;; partial-output buffers
;;
;; These accumulate output from a command executed on
;; behalf of emacs (rather than the user).
;;

(gdb-set-instance-buffer-rules 'gdb-partial-output-buffer
			       'gdb-partial-output-name)

(defun gdb-partial-output-name (instance)
  (concat "*partial-output-"
	  (gdb-instance-target-string instance)
	  "*"))


(gdb-set-instance-buffer-rules 'gdb-inferior-io
			       'gdb-inferior-io-name
			       'gdb-inferior-io-mode)

(defun gdb-inferior-io-name (instance)
  (concat "*input/output of "
	  (gdb-instance-target-string instance)
	  "*"))

(defvar gdb-inferior-io-mode-map (copy-keymap comint-mode-map))
(define-key comint-mode-map "\C-c\C-c" 'gdb-inferior-io-interrupt)
(define-key comint-mode-map "\C-c\C-z" 'gdb-inferior-io-stop)
(define-key comint-mode-map "\C-c\C-\\" 'gdb-inferior-io-quit)
(define-key comint-mode-map "\C-c\C-d" 'gdb-inferior-io-eof)

(defun gdb-inferior-io-mode ()
  "Major mode for gdb inferior-io.

\\{comint-mode-map}"
  ;; We want to use comint because it has various nifty and familiar
  ;; features.  We don't need a process, but comint wants one, so create
  ;; a dummy one.
  (make-comint (substring (buffer-name) 1 (- (length (buffer-name)) 1))
	       "/bin/cat")
  (setq major-mode 'gdb-inferior-io-mode)
  (setq mode-name "Debuggee I/O")
  (set (make-local-variable 'gud-minor-mode) 'gdba)
  (set (make-local-variable 'tool-bar-map) gud-tool-bar-map)
  (setq comint-input-sender 'gdb-inferior-io-sender))

(defun gdb-inferior-io-sender (proc string)
  (save-excursion
    (set-buffer (process-buffer proc))
    (let ((instance gdb-buffer-instance))
      (set-buffer (gdb-get-instance-buffer instance 'gdba))
      (let ((gdb-proc (get-buffer-process (current-buffer))))
	(process-send-string gdb-proc string)
	(process-send-string gdb-proc "\n")))))

(defun gdb-inferior-io-interrupt (instance)
  "Interrupt the program being debugged."
  (interactive (list (gdb-needed-default-instance)))
  (interrupt-process
   (get-buffer-process (gdb-get-instance-buffer instance 'gdba)) comint-ptyp))

(defun gdb-inferior-io-quit (instance)
  "Send quit signal to the program being debugged."
  (interactive (list (gdb-needed-default-instance)))
  (quit-process
   (get-buffer-process (gdb-get-instance-buffer instance 'gdba)) comint-ptyp))

(defun gdb-inferior-io-stop (instance)
  "Stop the program being debugged."
  (interactive (list (gdb-needed-default-instance)))
  (stop-process
   (get-buffer-process (gdb-get-instance-buffer instance 'gdba)) comint-ptyp))

(defun gdb-inferior-io-eof (instance)
  "Send end-of-file to the program being debugged."
  (interactive (list (gdb-needed-default-instance)))
  (process-send-eof
   (get-buffer-process (gdb-get-instance-buffer instance 'gdba))))


;;
;; gdb communications
;;

;; INPUT: things sent to gdb
;;
;; Each instance has a high and low priority
;; input queue.  Low priority input is sent only
;; when the high priority queue is idle.
;;
;; The queues are lists.  Each element is either
;; a string (indicating user or user-like input)
;; or a list of the form:
;;
;;    (INPUT-STRING  HANDLER-FN)
;;
;;
;; The handler function will be called from the
;; partial-output buffer when the command completes.
;; This is the way to write commands which
;; invoke gdb commands autonomously.
;;
;; These lists are consumed tail first.
;;

(defun gdb-send (proc string)
  "A comint send filter for gdb.
This filter may simply queue output for a later time."
  (let ((instance (gdb-proc->instance proc)))
    (gdb-instance-enqueue-input instance (concat string "\n"))))

;; Note: Stuff enqueued here will be sent to the next prompt, even if it
;; is a query, or other non-top-level prompt.  To guarantee stuff will get
;; sent to the top-level prompt, currently it must be put in the idle queue.
;;				 ^^^^^^^^^
;; [This should encourage gdb extensions that invoke gdb commands to let
;;  the user go first; it is not a bug.     -t]
;;

(defun gdb-instance-enqueue-input (instance item)
  (if (gdb-instance-prompting instance)
      (progn
	(gdb-send-item instance item)
	(set-gdb-instance-prompting instance nil))
    (set-gdb-instance-input-queue
     instance
     (cons item (gdb-instance-input-queue instance)))))

(defun gdb-instance-dequeue-input (instance)
  (let ((queue (gdb-instance-input-queue instance)))
    (and queue
       (if (not (cdr queue))
	   (let ((answer (car queue)))
	     (set-gdb-instance-input-queue instance '())
	     answer)
	 (gdb-take-last-elt queue)))))

(defun gdb-instance-enqueue-idle-input (instance item)
  (if (and (gdb-instance-prompting instance)
	   (not (gdb-instance-input-queue instance)))
      (progn
	(gdb-send-item instance item)
	(set-gdb-instance-prompting instance nil))
    (set-gdb-instance-idle-input-queue
     instance
     (cons item (gdb-instance-idle-input-queue instance)))))

(defun gdb-instance-dequeue-idle-input (instance)
  (let ((queue (gdb-instance-idle-input-queue instance)))
    (and queue
       (if (not (cdr queue))
	   (let ((answer (car queue)))
	     (set-gdb-instance-idle-input-queue instance '())
	     answer)
	 (gdb-take-last-elt queue)))))

; Don't use this in general.
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
  "Default command to execute an executable under the GDB debugger (gdb-ui.el)."
   :type 'string
   :group 'gud)

(defun gdba-marker-filter (string)
  "A gud marker filter for gdb."
  ;; Bogons don't tell us the process except through scoping crud.
  (let ((instance (gdb-proc->instance proc)))
    (gdb-output-burst instance string)))

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
    ("display-number-end" gdb-display-number-end)
    ("array-section-begin" gdb-array-section-begin)
    ("array-section-end" gdb-array-section-end)
;    ("elt" gdb-elt)
    ("field-begin" gdb-field-begin)
    ("field-end" gdb-field-end)
    ) "An assoc mapping annotation tags to functions which process them.")

(defun gdb-ignore-annotation (instance args)
  nil)

(defconst gdb-source-spec-regexp
  "\\(.*\\):\\([0-9]*\\):[0-9]*:[a-z]*:\\(0x[a-f0-9]*\\)")

;; Do not use this except as an annotation handler."
(defun gdb-source (instance args)
  (string-match gdb-source-spec-regexp args)
  ;; Extract the frame position from the marker.
  (setq gud-last-frame
	(cons
	 (substring args (match-beginning 1) (match-end 1))
	 (string-to-int (substring args
				   (match-beginning 2)
				   (match-end 2)))))
  (setq gdb-current-address (substring args (match-beginning 3)
				       (match-end 3)))
  (setq gdb-main-or-pc gdb-current-address)
;update with new frame for machine code if necessary
  (gdb-invalidate-assembler instance))

;; An annotation handler for `prompt'.
;; This sends the next command (if any) to gdb.
(defun gdb-prompt (instance ignored)
  (let ((sink (gdb-instance-output-sink instance)))
    (cond
     ((eq sink 'user) t)
     ((eq sink 'post-emacs)
      (set-gdb-instance-output-sink instance 'user))
     (t
      (set-gdb-instance-output-sink instance 'user)
      (error "Phase error in gdb-prompt (got %s)" sink))))
  (let ((highest (gdb-instance-dequeue-input instance)))
    (if highest
	(gdb-send-item instance highest)
      (let ((lowest (gdb-instance-dequeue-idle-input instance)))
	(if lowest
	    (gdb-send-item instance lowest)
	  (progn
	    (set-gdb-instance-prompting instance t)
	    (gud-display-frame)))))))

;; An annotation handler for non-top-level prompts.
(defun gdb-subprompt (instance ignored)
  (let ((highest (gdb-instance-dequeue-input instance)))
    (if highest
	(gdb-send-item instance highest)
      (set-gdb-instance-prompting instance t))))

(defun gdb-send-item (instance item)
  (set-gdb-instance-current-item instance item)
  (if (stringp item)
      (progn
	(set-gdb-instance-output-sink instance 'user)
	(process-send-string (gdb-instance-process instance)
			     item))
    (progn
      (gdb-clear-partial-output instance)
      (set-gdb-instance-output-sink instance 'pre-emacs)
      (process-send-string (gdb-instance-process instance)
			   (car item)))))

;; An annotation handler for `pre-prompt'.
;; This terminates the collection of output from a previous
;; command if that happens to be in effect.
(defun gdb-pre-prompt (instance ignored)
  (let ((sink (gdb-instance-output-sink instance)))
    (cond
     ((eq sink 'user) t)
     ((eq sink 'emacs)
      (set-gdb-instance-output-sink instance 'post-emacs)
      (let ((handler
	     (car (cdr (gdb-instance-current-item instance)))))
	(save-excursion
	  (set-buffer (gdb-get-create-instance-buffer
		       instance 'gdb-partial-output-buffer))
	  (funcall handler))))
     (t
      (set-gdb-instance-output-sink instance 'user)
      (error "Output sink phase error 1")))))

;; An annotation handler for `starting'.  This says that I/O for the subprocess
;; is now the program being debugged, not GDB.
(defun gdb-starting (instance ignored)
  (let ((sink (gdb-instance-output-sink instance)))
    (cond
     ((eq sink 'user)
      (set-gdb-instance-output-sink instance 'inferior))
     (t (error "Unexpected `starting' annotation")))))

;; An annotation handler for `exited' and other annotations which say that
;; I/O for the subprocess is now GDB, not the program being debugged.
(defun gdb-stopping (instance ignored)
  (let ((sink (gdb-instance-output-sink instance)))
    (cond
     ((eq sink 'inferior)
      (set-gdb-instance-output-sink instance 'user))
     (t (error "Unexpected stopping annotation")))))

;; An annotation handler for `stopped'.  It is just like gdb-stopping, except
;; that if we already set the output sink to 'user in gdb-stopping, that is
;; fine.
(defun gdb-stopped (instance ignored)
  (let ((sink (gdb-instance-output-sink instance)))
    (cond
     ((eq sink 'inferior)
      (set-gdb-instance-output-sink instance 'user))
     ((eq sink 'user) t)
     (t (error "Unexpected stopped annotation")))))

(defun gdb-frame-begin (instance ignored)
  (let ((sink (gdb-instance-output-sink instance)))
    (cond
     ((eq sink 'inferior)
      (set-gdb-instance-output-sink instance 'user))
     ((eq sink 'user) t)
     ((eq sink 'emacs) t)
     (t (error "Unexpected frame-begin annotation (%S)" sink)))))

;; An annotation handler for `post-prompt'.
;; This begins the collection of output from the current
;; command if that happens to be appropriate."
(defun gdb-post-prompt (instance ignored)
  (if (not (gdb-instance-pending-triggers instance))
      (progn
	(gdb-invalidate-registers instance ignored)
	(gdb-invalidate-locals instance ignored)
	(gdb-invalidate-display instance ignored)))
  (let ((sink (gdb-instance-output-sink instance)))
    (cond
     ((eq sink 'user) t)
     ((eq sink 'pre-emacs)
      (set-gdb-instance-output-sink instance 'emacs))

     (t
      (set-gdb-instance-output-sink instance 'user)
      (error "Output sink phase error 3")))))

;; If we get an error whilst evaluating one of the expressions
;; we won't get the display-end annotation. Set the sink back to
;; user to make sure that the error message is seen

(defun gdb-error-begin (instance ignored)
  (set-gdb-instance-output-sink instance 'user))

(defun gdb-display-begin (instance ignored)
  (if (gdb-get-instance-buffer instance 'gdb-display-buffer)
      (progn
	(set-gdb-instance-output-sink instance 'emacs)
	(gdb-clear-partial-output instance)
	(setq gdb-display-in-progress t))
    (set-gdb-instance-output-sink instance 'user)))

(defun gdb-display-number-end (instance ignored)
  (set-buffer (gdb-get-instance-buffer
	       instance 'gdb-partial-output-buffer))
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
;else
      (set-buffer (get-buffer-create gdb-expression-buffer-name))
      (if (and (display-graphic-p) (not gdb-dive))
	  (catch 'frame-exists
	    (let ((frames (frame-list)))
	      (while frames
		(if (string-equal (frame-parameter (car frames) 'name)
				  gdb-expression-buffer-name)
		    (throw 'frame-exists nil))
		(setq frames (cdr frames)))
	      (if (not frames)
		  (progn
		    (gdb-expressions-mode)
		    (make-frame '((height . 20) (width . 40)
				  (tool-bar-lines . nil)
				  (menu-bar-lines . nil)
				  (minibuffer . nil))))))))))
  (set-buffer (gdb-get-instance-buffer
	       instance 'gdb-partial-output-buffer))
  (setq gdb-dive nil))

(defun gdb-display-end (instance ignored)
  (set-buffer (gdb-get-instance-buffer instance 'gdb-partial-output-buffer))
  (goto-char (point-min))
  (search-forward ": ")
  (looking-at "\\(.*?\\) =")
  (let ((char "")
	(gdb-temp-value (buffer-substring (match-beginning 1)
					  (match-end 1))))
;move * to front of expression if necessary
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
;else put * back on if necessary
	(setq gdb-expression (concat char gdb-expression)))
      (setq header-line-format (concat "-- " gdb-expression " %-"))))

;-if scalar/string
  (if (not (re-search-forward "##" nil t))
      (progn
	(save-excursion
	  (set-buffer gdb-expression-buffer-name)
	  (setq buffer-read-only nil)
	  (delete-region (point-min) (point-max))
	  (insert-buffer (gdb-get-instance-buffer
			  instance 'gdb-partial-output-buffer))
	  (setq buffer-read-only t)))
; else
; display expression name...
    (goto-char (point-min))
    (let ((start (progn (point)))
	  (end (progn (end-of-line) (point))))
      (save-excursion
	(set-buffer gdb-expression-buffer-name)
	(setq buffer-read-only nil)
	(delete-region (point-min) (point-max))
	(insert-buffer-substring (gdb-get-instance-buffer
				  gdb-buffer-instance
				  'gdb-partial-output-buffer)
				 start end)
	(insert "\n")))

    (goto-char (point-min))
    (re-search-forward "##" nil t)
    (setq gdb-nesting-level 0)
    (if (looking-at "array-section-begin")
	(progn
	  (gdb-delete-line)
	  (beginning-of-line)
	  (setq gdb-point (point))
	  (gdb-array-format)))
    (if (looking-at "field-begin \\(.\\)")
	(progn
	  (setq gdb-annotation-arg (buffer-substring (match-beginning 1)
						     (match-end 1)))
	  (gdb-field-format-begin))))
  (save-excursion
  (set-buffer gdb-expression-buffer-name)
  (if gdb-dive-display-number
      (progn
	(setq buffer-read-only nil)
	(goto-char (point-max))
	(insert "\n")
	(insert-text-button "[back]" 'type 'gdb-display-back)
	(setq buffer-read-only t))))
  (gdb-clear-partial-output instance)
  (set-gdb-instance-output-sink instance 'user)
  (setq gdb-display-in-progress nil))

(define-button-type 'gdb-display-back
  'help-echo (purecopy "mouse-2, RET: go back to previous display buffer")
  'action (lambda (button) (gdb-display-go-back)))

(defun gdb-display-go-back ()
  ; delete display so they don't accumulate and delete buffer
  (let ((number gdb-display-number))
    (gdb-instance-enqueue-idle-input
     gdb-buffer-instance
     (list (concat "server delete display " number "\n")
	   '(lambda () nil)))
    (switch-to-buffer (concat "*display " gdb-dive-display-number "*"))
    (kill-buffer (get-buffer (concat "*display " number "*")))))

; prefix annotations with ## and process whole output in one chunk
; in gdb-partial-output-buffer (to allow recursion).

; array-section flags are just removed again but after counting. They
; might also be useful for arrays of structures and structures with arrays.
(defun gdb-array-section-begin (instance args)
  (if gdb-display-in-progress
      (progn
	(save-excursion
	  (set-buffer (gdb-get-instance-buffer
	       instance 'gdb-partial-output-buffer))
	  (goto-char (point-max))
	  (insert (concat "\n##array-section-begin " args "\n"))))))

(defun gdb-array-section-end (instance ignored)
  (if gdb-display-in-progress
      (progn
	(save-excursion
	  (set-buffer (gdb-get-instance-buffer
	       instance 'gdb-partial-output-buffer))
	  (goto-char (point-max))
	  (insert "\n##array-section-end\n")))))

(defun gdb-field-begin (instance args)
  (if gdb-display-in-progress
      (progn
	(save-excursion
	  (set-buffer (gdb-get-instance-buffer
	       instance 'gdb-partial-output-buffer))
	  (goto-char (point-max))
	  (insert (concat "\n##field-begin " args "\n"))))))

(defun gdb-field-end (instance ignored)
  (if gdb-display-in-progress
      (progn
	(save-excursion
	  (set-buffer (gdb-get-instance-buffer
	       instance 'gdb-partial-output-buffer))
	  (goto-char (point-max))
	  (insert "\n##field-end\n")))))

(defun gdb-elt (instance ignored)
  (if gdb-display-in-progress
      (progn
	(goto-char (point-max))
	(insert "\n##elt\n"))))

(defun gdb-field-format-begin ()
; get rid of ##field-begin
	(gdb-delete-line)
	(gdb-insert-field)
	(setq gdb-nesting-level (+ gdb-nesting-level 1))
	(while (re-search-forward "##" nil t)
; keep making recursive calls...
	  (if (looking-at "field-begin \\(.\\)")
	      (progn
		(setq gdb-annotation-arg (buffer-substring (match-beginning 1)
							   (match-end 1)))
		(gdb-field-format-begin)))
; until field-end.
	  (if (looking-at "field-end") (gdb-field-format-end))))

(defun gdb-field-format-end ()
; get rid of ##field-end and `,' or `}'
  (gdb-delete-line)
  (gdb-delete-line)
  (setq gdb-nesting-level (- gdb-nesting-level 1)))

(defun gdb-insert-field ()
  (let ((start (progn (point)))
	(end (progn (next-line) (point)))
	(num 0))
    (save-excursion
      (set-buffer gdb-expression-buffer-name)
      (setq buffer-read-only nil)
      (if (string-equal gdb-annotation-arg "\*") (insert "\*"))
      (while (<= num gdb-nesting-level)
	(insert "\t")
	(setq num (+ num 1)))
      (insert-buffer-substring (gdb-get-instance-buffer
				gdb-buffer-instance
				'gdb-partial-output-buffer)
			       start end)
      (put-text-property (- (point) (- end start)) (- (point) 1)
			 'mouse-face 'highlight)
      (put-text-property (- (point) (- end start)) (- (point) 1)
                         'local-map gdb-dive-map)
      (setq buffer-read-only t))
    (delete-region start end)))

(defun gdb-array-format ()
  (while (re-search-forward "##" nil t)
; keep making recursive calls...
    (if (looking-at "array-section-begin")
	(progn
;get rid of ##array-section-begin
	  (gdb-delete-line)
	  (setq gdb-nesting-level (+ gdb-nesting-level 1))
	  (gdb-array-format)))
;until *matching* array-section-end is found
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
;else get rid of ##array-section-end etc
	  (gdb-delete-line)
	  (setq gdb-nesting-level (- gdb-nesting-level 1))
	  (gdb-array-format)))))

(defun gdb-array-format1 ()
  (setq gdb-display-string "")
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))
  (let ((gdb-value-list (split-string gdb-values  ", ")))
    (string-match "\\({+\\)" (car gdb-value-list))
    (let* ((depth (- (match-end 1) (match-beginning 1)))
	   (indices  (make-vector depth '0))
	   (index 0) (num 0) (array-start "")
	   (array-stop "") (array-slice "")
	   (flag t) (indices-string ""))
      (while gdb-value-list
	(string-match "{*\\([^}]*\\)\\(}*\\)" (car gdb-value-list))
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
	    (let ((gdb-display-value (substring (car gdb-value-list)
						(match-beginning 1)
						(match-end 1))))
	      (setq gdb-display-string (concat gdb-display-string " "
					       gdb-display-value))
	      (insert
	       (concat indices-string "\t" gdb-display-value "\n"))))
	(setq indices-string "")
	(setq flag t)
					; 0<= index < depth, start at right : (- depth 1)
	(setq index (- (- depth 1)
		       (- (match-end 2) (match-beginning 2))))
					;don't set for very last brackets
	(if (>= index 0)
	    (progn
	      (aset indices index (+ 1 (aref indices index)))
	      (setq num (+ 1 index))
	      (while (< num depth)
		(aset indices num 0)
		(setq num (+ num 1)))))
	(setq gdb-value-list (cdr gdb-value-list)))
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
       (concat "\n     Slice : " array-slice "\n\nIndex\tValues\n\n"))))
  (setq buffer-read-only t))

(defvar gdb-dive-map nil)
(setq gdb-dive-map (make-keymap))
(define-key gdb-dive-map [mouse-2] 'gdb-dive)
(define-key gdb-dive-map [S-mouse-2] 'gdb-dive-new-frame)

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
      (setq gdb-last-field (buffer-substring-no-properties
			    (match-beginning 1)
			    (match-end 1)))
      (goto-char (match-beginning 1))
      (let ((last-column (current-column)))
	(while (re-search-backward "\\s-\\(\\S-+\\) = {" nil t)
	  (goto-char (match-beginning 1))
	  (if (and (< (current-column) last-column)
		   (> (count-lines 1 (point)) 1))
	      (progn
		(setq gdb-part-expression
		      (concat "." (buffer-substring-no-properties
				   (match-beginning 1)
				   (match-end 1)) gdb-part-expression))
		(setq last-column (current-column))))))
; * not needed for components of a pointer to a structure in gdb
      (if (string-equal "*" (substring gdb-full-expression 0 1))
	  (setq gdb-full-expression (substring gdb-full-expression 1 nil)))
      (setq gdb-full-expression
	    (concat gdb-full-expression gdb-part-expression "." gdb-last-field))
      (gdb-instance-enqueue-idle-input gdb-buffer-instance
				       (list
					(concat "server display" gdb-display-char
						" " gdb-full-expression "\n")
			      '(lambda () nil))))))

;; Handle a burst of output from a gdb instance.
;; This function is (indirectly) used as a gud-marker-filter.
;; It must return output (if any) to be insterted in the gdb
;; buffer.

(defun gdb-output-burst (instance string)
  "Handle a burst of output from a gdb instance.
This function is (indirectly) used as a gud-marker-filter.
It must return output (if any) to be insterted in the gdb
buffer."

  (save-match-data
    (let (
	  ;; Recall the left over burst from last time
	  (burst (concat (gdb-instance-burst instance) string))
	  ;; Start accumulating output for the GUD buffer
	  (output ""))

      ;; Process all the complete markers in this chunk.

      (while (string-match "\n\032\032\\(.*\\)\n" burst)
	(let ((annotation (substring burst
				     (match-beginning 1)
				     (match-end 1))))
	    
	  ;; Stuff prior to the match is just ordinary output.
	  ;; It is either concatenated to OUTPUT or directed
	  ;; elsewhere.
	  (setq output
		(gdb-concat-output
		 instance
		 output
		 (substring burst 0 (match-beginning 0))))

	  ;; Take that stuff off the burst.
	  (setq burst (substring burst (match-end 0)))
	    
	  ;; Parse the tag from the annotation, and maybe its arguments.
	  (string-match "\\(\\S-*\\) ?\\(.*\\)" annotation)
	  (let* ((annotation-type (substring annotation
					     (match-beginning 1)
					     (match-end 1)))
		 (annotation-arguments (substring annotation
						  (match-beginning 2)
						  (match-end 2)))
		 (annotation-rule (assoc annotation-type
					 gdb-annotation-rules)))
	    ;; Call the handler for this annotation.
	    (if annotation-rule
		(funcall (car (cdr annotation-rule))
			 instance
			 annotation-arguments)
	      ;; Else the annotation is not recognized.  Ignore it silently,
	      ;; so that GDB can add new annotations without causing
	      ;; us to blow up.
	      ))))


      ;; Does the remaining text end in a partial line?
      ;; If it does, then keep part of the burst until we get more.
      (if (string-match "\n\\'\\|\n\032\\'\\|\n\032\032.*\\'"
			burst)
	  (progn
	    ;; Everything before the potential marker start can be output.
	    (setq output
		  (gdb-concat-output
		   instance
		   output
		   (substring burst 0 (match-beginning 0))))

	    ;; Everything after, we save, to combine with later input.
	    (setq burst (substring burst (match-beginning 0))))

	;; In case we know the burst contains no partial annotations:
	(progn
	  (setq output (gdb-concat-output instance output burst))
	  (setq burst "")))

      ;; Save the remaining burst for the next call to this function.
      (set-gdb-instance-burst instance burst)
      output)))

(defun gdb-concat-output (instance so-far new)
  (let ((sink (gdb-instance-output-sink instance)))
    (cond
     ((eq sink 'user) (concat so-far new))
     ((or (eq sink 'pre-emacs) (eq sink 'post-emacs)) so-far)
     ((eq sink 'emacs)
      (gdb-append-to-partial-output instance new)
      so-far)
     ((eq sink 'inferior)
      (gdb-append-to-inferior-io instance new)
      so-far)
     (t (error "Bogon output sink %S" sink)))))

(defun gdb-append-to-partial-output (instance string)
  (save-excursion
    (set-buffer
     (gdb-get-create-instance-buffer
      instance 'gdb-partial-output-buffer))
    (goto-char (point-max))
    (insert string)))

(defun gdb-clear-partial-output (instance)
  (save-excursion
    (set-buffer
     (gdb-get-create-instance-buffer
      instance 'gdb-partial-output-buffer))
    (delete-region (point-min) (point-max))))

(defun gdb-append-to-inferior-io (instance string)
  (save-excursion
    (set-buffer
     (gdb-get-create-instance-buffer
      instance 'gdb-inferior-io))
    (goto-char (point-max))
    (insert-before-markers string))
  (gdb-display-buffer
   (gdb-get-create-instance-buffer instance
				   'gdb-inferior-io)))

(defun gdb-clear-inferior-io (instance)
  (save-excursion
    (set-buffer
     (gdb-get-create-instance-buffer
      instance 'gdb-inferior-io))
    (delete-region (point-min) (point-max))))



;; One trick is to have a command who's output is always available in
;; a buffer of it's own, and is always up to date.  We build several
;; buffers of this type.
;;
;; There are two aspects to this: gdb has to tell us when the output
;; for that command might have changed, and we have to be able to run
;; the command behind the user's back.
;;
;; The idle input queue and the output phasing associated with
;; the instance variable `(gdb-instance-output-sink instance)' help
;; us to run commands behind the user's back.
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
(defmacro def-gdb-auto-update-trigger (name demand-predicate gdb-command output-handler)
  `(defun ,name (instance &optional ignored)
     (if (and (,demand-predicate instance)
	      (not (member ',name
			   (gdb-instance-pending-triggers instance))))
	 (progn
	   (gdb-instance-enqueue-idle-input
	    instance
	    (list ,gdb-command ',output-handler))
	   (set-gdb-instance-pending-triggers
	    instance
	    (cons ',name
		  (gdb-instance-pending-triggers instance)))))))
		
(defmacro def-gdb-auto-update-handler (name trigger buf-key custom-defun)
  `(defun ,name ()
     (set-gdb-instance-pending-triggers
      instance
      (delq ',trigger
	    (gdb-instance-pending-triggers instance)))
     (let ((buf (gdb-get-instance-buffer instance
					  ',buf-key)))
       (and buf
	    (save-excursion
	      (set-buffer buf)
	      (let ((p (point))
		    (buffer-read-only nil))
		(delete-region (point-min) (point-max))
		(insert-buffer (gdb-get-create-instance-buffer
				instance
				'gdb-partial-output-buffer))
		(goto-char p)))))
; put customisation here
     (,custom-defun)))

(defmacro def-gdb-auto-updated-buffer
  (buffer-key trigger-name gdb-command output-handler-name custom-defun)
   `(progn
     (def-gdb-auto-update-trigger ,trigger-name
       ;; The demand predicate:
       (lambda (instance)
	 (gdb-get-instance-buffer instance ',buffer-key))
       ,gdb-command
       ,output-handler-name)
     (def-gdb-auto-update-handler ,output-handler-name
       ,trigger-name ,buffer-key ,custom-defun)))


;;
;; Breakpoint buffers
;; 
;; These display the output of `info breakpoints'.
;;


(gdb-set-instance-buffer-rules 'gdb-breakpoints-buffer
			       'gdb-breakpoints-buffer-name
			       'gdb-breakpoints-mode)

(def-gdb-auto-updated-buffer gdb-breakpoints-buffer
  ;; This defines the auto update rule for buffers of type
  ;; `gdb-breakpoints-buffer'.
  ;;
  ;; It defines a function to serve as the annotation handler that
  ;; handles the `foo-invalidated' message.  That function is called:
  gdb-invalidate-breakpoints

  ;; To update the buffer, this command is sent to gdb.
  "server info breakpoints\n"

  ;; This also defines a function to be the handler for the output
  ;; from the command above.  That function will copy the output into
  ;; the appropriately typed buffer.  That function will be called:
  gdb-info-breakpoints-handler
;; buffer specific functions
  gdb-info-breakpoints-custom)

;-put breakpoint icons in relevant margins (even those set in the GUD buffer)
(defun gdb-info-breakpoints-custom ()
  (let ((flag)(address))

; remove all breakpoint-icons in source buffers but not assembler buffer
    (let ((buffers (buffer-list)))
      (save-excursion
	(while buffers
	  (set-buffer (car buffers))
	  (if (and (eq gud-minor-mode 'gdba)
		   (not (string-match "^\*" (buffer-name))))
	      (if (display-graphic-p)
		  (remove-images (point-min) (point-max))
		(remove-strings (point-min) (point-max))))
	  (setq buffers (cdr buffers)))))

    (save-excursion
      (set-buffer (gdb-get-instance-buffer instance 'gdb-breakpoints-buffer))
      (save-excursion
	(goto-char (point-min))
	(while (< (point) (- (point-max) 1))
	  (forward-line 1)
	  (if (looking-at "[^\t].*breakpoint")
	      (progn
		(looking-at "\\([0-9]*\\)\\s-*\\S-*\\s-*\\S-*\\s-*\\(.\\)")
		(setq flag (char-after (match-beginning 2)))
		(beginning-of-line)
		(re-search-forward "in\\s-+\\S-+\\s-+at\\s-+")
		(looking-at "\\(\\S-*\\):\\([0-9]+\\)")
		(let ((line (buffer-substring (match-beginning 2)
					      (match-end 2)))
		      (file (buffer-substring (match-beginning 1)
					      (match-end 1))))
		  (save-excursion
		    (set-buffer
		     (if (file-exists-p file)
			 (find-file-noselect file)
		     ;else
		       (find-file-noselect (concat gdb-cdir "/" file))))
		    (with-current-buffer (current-buffer)
		      (progn
			(set (make-local-variable 'gud-minor-mode) 'gdba)
			(set (make-local-variable 'tool-bar-map)
			     gud-tool-bar-map)
			(set (make-variable-buffer-local 'left-margin-width) 2)
			(if (get-buffer-window (current-buffer))
			    (set-window-margins (get-buffer-window
						 (current-buffer))
						left-margin-width
						right-margin-width))))
			      ; only want one breakpoint icon at each location
		    (save-excursion
		      (goto-line (string-to-number line))
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
			      (put-string "B" (point) "enabled"
					  'left-margin)
			    (put-string "b" (point) "disabled"
					'left-margin)))))))))
	  (end-of-line))))))

(defun gdb-breakpoints-buffer-name (instance)
  (save-excursion
    (set-buffer (process-buffer (gdb-instance-process instance)))
    (concat "*breakpoints of " (gdb-instance-target-string instance) "*")))

(defun gdb-display-breakpoints-buffer (instance)
  (interactive (list (gdb-needed-default-instance)))
  (gdb-display-buffer
   (gdb-get-create-instance-buffer instance
				    'gdb-breakpoints-buffer)))

(defun gdb-frame-breakpoints-buffer (instance)
  (interactive (list (gdb-needed-default-instance)))
  (switch-to-buffer-other-frame
   (gdb-get-create-instance-buffer instance
				    'gdb-breakpoints-buffer)))

(defvar gdb-breakpoints-mode-map nil)
(setq gdb-breakpoints-mode-map (make-keymap))
(suppress-keymap gdb-breakpoints-mode-map)

(define-key gdb-breakpoints-mode-map [menu-bar breakpoints]
  (cons "Breakpoints" (make-sparse-keymap "Breakpoints")))
(define-key gdb-breakpoints-mode-map [menu-bar breakpoints toggle]
  '("Toggle" . gdb-toggle-bp-this-line))
(define-key gdb-breakpoints-mode-map [menu-bar breakpoints delete]
  '("Delete" . gdb-delete-bp-this-line))
(define-key gdb-breakpoints-mode-map [menu-bar breakpoints goto]
  '("Goto"   . gdb-goto-bp-this-line))

(define-key gdb-breakpoints-mode-map " " 'gdb-toggle-bp-this-line)
(define-key gdb-breakpoints-mode-map "d" 'gdb-delete-bp-this-line)
(define-key gdb-breakpoints-mode-map "g" 'gdb-goto-bp-this-line)

(defun gdb-breakpoints-mode ()
  "Major mode for gdb breakpoints.

\\{gdb-breakpoints-mode-map}"
  (setq major-mode 'gdb-breakpoints-mode)
  (setq mode-name "Breakpoints")
  (use-local-map gdb-breakpoints-mode-map)
  (set (make-local-variable 'gud-minor-mode) 'gdba)
  (set (make-local-variable 'tool-bar-map) gud-tool-bar-map)
  (setq buffer-read-only t)
  (gdb-invalidate-breakpoints gdb-buffer-instance))

(defun gdb-toggle-bp-this-line ()
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (if (not (looking-at "\\([0-9]+\\).*point\\s-*\\S-*\\s-*\\(.\\)"))
	(error "Not recognized as break/watchpoint line")
      (Gdb-instance-enqueue-idle-input
       gdb-buffer-instance
       (list
	(concat
	 (if (eq ?y (char-after (match-beginning 2)))
	     "server disable "
	   "server enable ")
	 (buffer-substring (match-beginning 0)
			   (match-end 1))
	 "\n")
	'(lambda () nil))))))

(defun gdb-delete-bp-this-line ()
  (interactive)
    (beginning-of-line 1)
    (if (not (looking-at "\\([0-9]+\\).*point\\s-*\\S-*\\s-*\\(.\\)"))
	(error "Not recognized as break/watchpoint line")
      (gdb-instance-enqueue-idle-input
       gdb-buffer-instance
       (list
	(concat
	 "server delete "
	 (buffer-substring (match-beginning 0)
			   (match-end 1))
	 "\n")
	'(lambda () nil)))))

(defun gdb-goto-bp-this-line ()
"Display the file at the breakpoint specified."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (re-search-forward "in\\s-+\\S-+\\s-+at\\s-+")
    (looking-at "\\(\\S-*\\):\\([0-9]+\\)"))
  (let ((line (buffer-substring (match-beginning 2)
				  (match-end 2)))
	(file (buffer-substring (match-beginning 1)
				(match-end 1))))
    (if (file-exists-p file)
	(set-window-buffer gdb-source-window (find-file-noselect file))
      ;else
      (setq file (concat gdb-cdir "/" file))
      (set-window-buffer gdb-source-window (find-file-noselect file)))
    (goto-line (string-to-number line))))

;;
;; Frames buffers.  These display a perpetually correct bactracktrace
;; (from the command `where').
;;
;; Alas, if your stack is deep, they are costly.
;;

(gdb-set-instance-buffer-rules 'gdb-stack-buffer
			       'gdb-stack-buffer-name
			       'gdb-frames-mode)

(def-gdb-auto-updated-buffer gdb-stack-buffer
  gdb-invalidate-frames
  "server where\n"
  gdb-info-frames-handler
  gdb-info-frames-custom)

(defun gdb-info-frames-custom ()
  (save-excursion
    (set-buffer (gdb-get-instance-buffer instance 'gdb-stack-buffer))
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (looking-at "\\S-*\\s-*\\(\\S-*\\)")
      (setq gdb-current-frame (buffer-substring (match-beginning 1) (match-end 1)))
      (while (< (point) (point-max))
	(put-text-property (progn (beginning-of-line) (point))
			   (progn (end-of-line) (point))
			   'mouse-face 'highlight)
	(forward-line 1)))))

(defun gdb-stack-buffer-name (instance)
  (save-excursion
    (set-buffer (process-buffer (gdb-instance-process instance)))
    (concat "*stack frames of "
	    (gdb-instance-target-string instance) "*")))

(defun gdb-display-stack-buffer (instance)
  (interactive (list (gdb-needed-default-instance)))
  (gdb-display-buffer
   (gdb-get-create-instance-buffer instance
				   'gdb-stack-buffer)))

(defun gdb-frame-stack-buffer (instance)
  (interactive (list (gdb-needed-default-instance)))
  (switch-to-buffer-other-frame
   (gdb-get-create-instance-buffer instance
				   'gdb-stack-buffer)))

(defvar gdb-frames-mode-map nil)
(setq gdb-frames-mode-map (make-keymap))
(suppress-keymap gdb-frames-mode-map)
(define-key gdb-frames-mode-map [mouse-2]
  'gdb-frames-select-by-mouse)

(defun gdb-frames-mode ()
  "Major mode for gdb frames.

\\{gdb-frames-mode-map}"
  (setq major-mode 'gdb-frames-mode)
  (setq mode-name "Frames")
  (set (make-local-variable 'gud-minor-mode) 'gdba)
  (set (make-local-variable 'tool-bar-map) gud-tool-bar-map)
  (setq buffer-read-only t)
  (use-local-map gdb-frames-mode-map)
  (gdb-invalidate-frames gdb-buffer-instance))

(defun gdb-get-frame-number ()
  (save-excursion
    (let* ((pos (re-search-backward "^#\\([0-9]*\\)" nil t))
	   (n (or (and pos
		       (string-to-int
			(buffer-substring (match-beginning 1)
					  (match-end 1))))
		  0)))
      n)))

(defun gdb-frames-select-by-mouse (e)
"Display the source of the selected frame."
  (interactive "e")
  (let (selection)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end e))))
      (save-excursion
	(goto-char (posn-point (event-end e)))
	(setq selection (gdb-get-frame-number))))
    (select-window (posn-window (event-end e)))
    (save-excursion
      (set-buffer (gdb-get-instance-buffer (gdb-needed-default-instance) 'gdba))
  (gdb-instance-enqueue-idle-input
   gdb-buffer-instance
   (list
    (concat (gud-format-command "server frame %p" selection)
		 "\n")
    '(lambda () nil)))
      (gud-display-frame))))


;;
;; Registers buffers
;;

(def-gdb-auto-updated-buffer gdb-registers-buffer
  gdb-invalidate-registers
  "server info registers\n"
  gdb-info-registers-handler
  gdb-info-registers-custom)

(defun gdb-info-registers-custom ())

(gdb-set-instance-buffer-rules 'gdb-registers-buffer
			       'gdb-registers-buffer-name
			       'gdb-registers-mode)

(defvar gdb-registers-mode-map nil)
(setq gdb-registers-mode-map (make-keymap))
(suppress-keymap gdb-registers-mode-map)

(defun gdb-registers-mode ()
  "Major mode for gdb registers.

\\{gdb-registers-mode-map}"
  (setq major-mode 'gdb-registers-mode)
  (setq mode-name "Registers")
  (set (make-local-variable 'gud-minor-mode) 'gdba)
  (set (make-local-variable 'tool-bar-map) gud-tool-bar-map)
  (setq buffer-read-only t)
  (use-local-map gdb-registers-mode-map)
  (gdb-invalidate-registers gdb-buffer-instance))

(defun gdb-registers-buffer-name (instance)
  (save-excursion
    (set-buffer (process-buffer (gdb-instance-process instance)))
    (concat "*registers of " (gdb-instance-target-string instance) "*")))

(defun gdb-display-registers-buffer (instance)
  (interactive (list (gdb-needed-default-instance)))
  (gdb-display-buffer
   (gdb-get-create-instance-buffer instance
				   'gdb-registers-buffer)))

(defun gdb-frame-registers-buffer (instance)
  (interactive (list (gdb-needed-default-instance)))
  (switch-to-buffer-other-frame
   (gdb-get-create-instance-buffer instance
				   'gdb-registers-buffer)))

;;
;; Locals buffers
;;

(def-gdb-auto-updated-buffer gdb-locals-buffer
  gdb-invalidate-locals
  "server info locals\n"
  gdb-info-locals-handler
  gdb-info-locals-custom)


;Abbreviate for arrays and structures. These can be expanded using gud-display
(defun gdb-info-locals-handler nil
  (set-gdb-instance-pending-triggers
   instance (delq (quote gdb-invalidate-locals)
		  (gdb-instance-pending-triggers instance)))
  (let ((buf (gdb-get-instance-buffer instance
				      (quote gdb-partial-output-buffer))))
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (replace-regexp "^ .*\n" "")
      (goto-char (point-min))
      (replace-regexp "{[-0-9, {}\]*\n" "(array);\n")))
      (goto-char (point-min))
      (replace-regexp "{.*=.*\n" "(structure);\n")
  (let ((buf (gdb-get-instance-buffer instance (quote gdb-locals-buffer))))
    (and buf (save-excursion
	       (set-buffer buf)
	       (let ((p (point))
		     (buffer-read-only nil))
		 (delete-region (point-min) (point-max))
		 (insert-buffer (gdb-get-create-instance-buffer
				 instance
				 (quote gdb-partial-output-buffer)))
		 (goto-char p)))))
  (run-hooks (quote gdb-info-locals-hook)))

(defun gdb-info-locals-custom ()
  nil)

(gdb-set-instance-buffer-rules 'gdb-locals-buffer
			       'gdb-locals-buffer-name
			       'gdb-locals-mode)

(defvar gdb-locals-mode-map nil)
(setq gdb-locals-mode-map (make-keymap))
(suppress-keymap gdb-locals-mode-map)

(defun gdb-locals-mode ()
  "Major mode for gdb locals.

\\{gdb-locals-mode-map}"
  (setq major-mode 'gdb-locals-mode)
  (setq mode-name "Locals")
  (set (make-local-variable 'gud-minor-mode) 'gdba)
  (set (make-local-variable 'tool-bar-map) gud-tool-bar-map)
  (setq buffer-read-only t)
  (use-local-map gdb-locals-mode-map)
  (gdb-invalidate-locals gdb-buffer-instance))

(defun gdb-locals-buffer-name (instance)
  (save-excursion
    (set-buffer (process-buffer (gdb-instance-process instance)))
    (concat "*locals of " (gdb-instance-target-string instance) "*")))

(defun gdb-display-locals-buffer (instance)
  (interactive (list (gdb-needed-default-instance)))
  (gdb-display-buffer
   (gdb-get-create-instance-buffer instance
				   'gdb-locals-buffer)))

(defun gdb-frame-locals-buffer (instance)
  (interactive (list (gdb-needed-default-instance)))
  (switch-to-buffer-other-frame
   (gdb-get-create-instance-buffer instance
				   'gdb-locals-buffer)))
;;
;; Display expression buffers (just allow one to start with)
;;
(gdb-set-instance-buffer-rules 'gdb-display-buffer
			       'gdb-display-buffer-name
			       'gdb-display-mode)

(def-gdb-auto-updated-buffer gdb-display-buffer
  ;; This defines the auto update rule for buffers of type
  ;; `gdb-display-buffer'.
  ;;
  ;; It defines a function to serve as the annotation handler that
  ;; handles the `foo-invalidated' message.  That function is called:
  gdb-invalidate-display

  ;; To update the buffer, this command is sent to gdb.
  "server info display\n"

  ;; This also defines a function to be the handler for the output
  ;; from the command above.  That function will copy the output into
  ;; the appropriately typed buffer.  That function will be called:
  gdb-info-display-handler
; buffer specific functions
  gdb-info-display-custom)

(defun gdb-info-display-custom ()
; TODO: ensure frames of expressions that have been deleted are also deleted
;       these can be missed currently eg through GUD buffer, restarting a
;       recompiled program.
)

(defvar gdb-display-mode-map nil)
(setq gdb-display-mode-map (make-keymap))
(suppress-keymap gdb-display-mode-map)

(define-key gdb-display-mode-map [menu-bar display]
  (cons "Display" (make-sparse-keymap "Display")))
(define-key gdb-display-mode-map [menu-bar display toggle]
  '("Toggle" . gdb-toggle-disp-this-line))
(define-key gdb-display-mode-map [menu-bar display delete]
  '("Delete" . gdb-delete-disp-this-line))

(define-key gdb-display-mode-map " " 'gdb-toggle-disp-this-line)
(define-key gdb-display-mode-map "d" 'gdb-delete-disp-this-line)

(defun gdb-display-mode ()
  "Major mode for gdb display.

\\{gdb-display-mode-map}"
  (setq major-mode 'gdb-display-mode)
  (setq mode-name "Display")
  (set (make-local-variable 'gud-minor-mode) 'gdba)
  (set (make-local-variable 'tool-bar-map) gud-tool-bar-map)
  (setq buffer-read-only t)
  (use-local-map gdb-display-mode-map)
  (gdb-invalidate-display gdb-buffer-instance))

(defun gdb-display-buffer-name (instance)
  (save-excursion
    (set-buffer (process-buffer (gdb-instance-process instance)))
    (concat "*Displayed expressions of " (gdb-instance-target-string instance) "*")))

(defun gdb-display-display-buffer (instance)
  (interactive (list (gdb-needed-default-instance)))
  (gdb-display-buffer
   (gdb-get-create-instance-buffer instance
				   'gdb-display-buffer)))

(defun gdb-frame-display-buffer (instance)
  (interactive (list (gdb-needed-default-instance)))
  (switch-to-buffer-other-frame
   (gdb-get-create-instance-buffer instance
				   'gdb-display-buffer)))

(defun gdb-toggle-disp-this-line ()
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (if (not (looking-at "\\([0-9]+\\):   \\([ny]\\)"))
	(error "No expression on this line")
      (gdb-instance-enqueue-idle-input
       gdb-buffer-instance
       (list
	(concat
	 (if (eq ?y (char-after (match-beginning 2)))
	     "server disable display "
	   "server enable display ")
	 (buffer-substring (match-beginning 0)
			   (match-end 1))
	 "\n")
	'(lambda () nil))))))

(defun gdb-delete-disp-this-line ()
  (interactive)
  (save-excursion
    (set-buffer
     (gdb-get-instance-buffer gdb-buffer-instance 'gdb-display-buffer))
    (beginning-of-line 1)
    (if (not (looking-at "\\([0-9]+\\):   \\([ny]\\)"))
	(error "No expression on this line")
      (let ((number (buffer-substring (match-beginning 0)
				      (match-end 1))))
	(gdb-instance-enqueue-idle-input
	 gdb-buffer-instance
	 (list (concat "server delete display " number "\n")
	       '(lambda () nil)))
	(if (not (display-graphic-p))
	    (kill-buffer (get-buffer (concat "*display " number "*")))
	  ;else
	  (catch 'frame-found
	    (let ((frames (frame-list)))
	      (while frames
		(if (string-equal (frame-parameter (car frames) 'name)
				  (concat "*display " number "*"))
		    (progn (kill-buffer
			    (get-buffer (concat "*display " number "*")))
			   (delete-frame (car frames))
			   (throw 'frame-found nil)))
		(setq frames (cdr frames))))))))))

(defvar gdb-expressions-mode-map nil)
(setq gdb-expressions-mode-map (make-keymap))
(suppress-keymap gdb-expressions-mode-map)

(defvar gdb-expressions-mode-menu
  '("GDB Expressions Commands"
    "----"
    ["Visualise" gdb-array-visualise t]
    ["Delete" 	 gdb-delete-display  t])
  "Menu for `gdb-expressions-mode'.")

(define-key gdb-expressions-mode-map "v" 'gdb-array-visualise)
(define-key gdb-expressions-mode-map "q" 'gdb-delete-display)
(define-key gdb-expressions-mode-map [mouse-3] 'gdb-expressions-popup-menu)

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

;;; FIXME: This should only return true for buffers in the current instance
(defun gdb-protected-buffer-p (buffer)
  "Is BUFFER a buffer which we want to leave displayed?"
  (save-excursion
    (set-buffer buffer)
    (or gdb-buffer-type
	overlay-arrow-position)))

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
	      (if (gdb-protected-buffer-p (window-buffer win))
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
	  (if (gdb-protected-buffer-p (window-buffer win))
	      (set-window-dedicated-p win nil)))))
    (if must-split
	(let* ((largest (get-largest-window))
	       (cur-size (window-height largest))
	       (new-size (and size (< size cur-size) (- cur-size size))))
	  (setq answer (split-window largest new-size))
	  (set-window-buffer answer buf)))
    answer))
      
(defun gdb-display-source-buffer (buffer)
  (set-window-buffer gdb-source-window buffer))


;;; Shared keymap initialization:

(defun gdb-display-gdb-buffer (instance)
  (interactive (list (gdb-needed-default-instance)))
  (gdb-display-buffer
   (gdb-get-create-instance-buffer instance 'gdba)))

(defun make-windows-menu (map)
  (define-key map [menu-bar displays]
    (cons "GDB-Windows" (make-sparse-keymap "GDB-Windows")))
  (define-key map [menu-bar displays gdb]
    '("Gdb" . gdb-display-gdb-buffer))
  (define-key map [menu-bar displays locals]
    '("Locals" . gdb-display-locals-buffer))
  (define-key map [menu-bar displays registers]
    '("Registers" . gdb-display-registers-buffer))
  (define-key map [menu-bar displays frames]
    '("Stack" . gdb-display-stack-buffer))
  (define-key map [menu-bar displays breakpoints]
    '("Breakpoints" . gdb-display-breakpoints-buffer))
  (define-key map [menu-bar displays display]
    '("Display" . gdb-display-display-buffer))
  (define-key map [menu-bar displays assembler]
    '("Assembler" . gdb-display-assembler-buffer)))

(define-key gud-minor-mode-map "\C-c\M-\C-r" 'gdb-display-registers-buffer)
(define-key gud-minor-mode-map "\C-c\M-\C-f" 'gdb-display-stack-buffer)
(define-key gud-minor-mode-map "\C-c\M-\C-b" 'gdb-display-breakpoints-buffer)

(make-windows-menu gud-minor-mode-map)

(defun gdb-frame-gdb-buffer (instance)
  (interactive (list (gdb-needed-default-instance)))
  (switch-to-buffer-other-frame
   (gdb-get-create-instance-buffer instance 'gdba)))

(defun make-frames-menu (map)
  (define-key map [menu-bar frames]
    (cons "GDB-Frames" (make-sparse-keymap "GDB-Frames")))
  (define-key map [menu-bar frames gdb]
    '("Gdb" . gdb-frame-gdb-buffer))
  (define-key map [menu-bar frames locals]
    '("Locals" . gdb-frame-locals-buffer))
  (define-key map [menu-bar frames registers]
    '("Registers" . gdb-frame-registers-buffer))
  (define-key map [menu-bar frames frames]
    '("Stack" . gdb-frame-stack-buffer))
  (define-key map [menu-bar frames breakpoints]
    '("Breakpoints" . gdb-frame-breakpoints-buffer))
  (define-key map [menu-bar frames display]
    '("Display" . gdb-frame-display-buffer))
  (define-key map [menu-bar frames  assembler]
    '("Assembler" . gdb-frame-assembler-buffer)))

(if (display-graphic-p)
    (make-frames-menu gud-minor-mode-map))

(defvar gdb-target-name "--unknown--"
  "The apparent name of the program being debugged in a gud buffer.")

(defun gdb-proc-died (proc)
  ;; Stop displaying an arrow in a source file.
  (setq overlay-arrow-position nil)

  ;; Kill the dummy process, so that C-x C-c won't worry about it.
  (save-excursion
    (set-buffer (process-buffer proc))
    (kill-process
     (get-buffer-process
      (gdb-get-instance-buffer gdb-buffer-instance 'gdb-inferior-io)))))
;; end of functions from gdba.el

;; new functions for gdb-ui.el
;; layout for all the windows
(defun gdb-setup-windows (instance)
  (gdb-display-locals-buffer instance)
  (gdb-display-stack-buffer instance)
  (delete-other-windows)
  (gdb-display-breakpoints-buffer instance)
  (gdb-display-display-buffer instance)
  (delete-other-windows)
  (split-window nil ( / ( * (window-height) 3) 4))
  (split-window nil ( / (window-height) 3))
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer (gdb-locals-buffer-name instance))
  (other-window 1)
  (switch-to-buffer
   (if gud-last-last-frame
       (gud-find-file (car gud-last-last-frame))
     (gud-find-file gdb-main-file)))
  (setq gdb-source-window (get-buffer-window (current-buffer)))
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer (gdb-inferior-io-name instance))
  (other-window 1)
  (switch-to-buffer (gdb-stack-buffer-name instance))
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer (gdb-breakpoints-buffer-name instance))
  (other-window 1))

(defun gdb-restore-windows ()
  "Restore the basic arrangement of windows used by gdba.
This arrangement depends on the value of `gdb-many-windows'"
  (interactive)
  (if gdb-many-windows
      (progn
	(switch-to-buffer gud-comint-buffer)
	(delete-other-windows)
	(gdb-setup-windows gdb-buffer-instance))
;else
    (switch-to-buffer gud-comint-buffer)
    (delete-other-windows)
    (split-window)
    (other-window 1)
    (switch-to-buffer
     (if gud-last-last-frame
	 (gud-find-file (car gud-last-last-frame))
       (gud-find-file gdb-main-file)))
    (other-window 1)))

(defun toggle-gdb-windows ()
  "Toggle the number of windows in the basic arrangement."
  (interactive)
  (if gdb-many-windows
      (progn
	(switch-to-buffer gud-comint-buffer)
	(delete-other-windows)
	(split-window)
	(other-window 1)
	(switch-to-buffer
	 (if gud-last-last-frame
	     (gud-find-file (car gud-last-last-frame))
	   (gud-find-file gdb-main-file)))
	(other-window 1)
	(setq gdb-many-windows nil))
;else
    (switch-to-buffer gud-comint-buffer)
    (delete-other-windows)
    (gdb-setup-windows gdb-buffer-instance)
    (setq gdb-many-windows t)))

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

(setq breakpoint-enabled-icon (find-image
			       `((:type xpm :data ,breakpoint-xpm-data))))
(setq breakpoint-disabled-icon  (find-image
			       `((:type xpm :data ,breakpoint-xpm-data
					    :conversion laplace))))

(defun gdb-quit ()
  "Kill the GUD and ancillary (including source) buffers.
Just the partial-output buffer is left."
  (interactive)
  (let ((buffers (buffer-list)))
    (save-excursion
      (while buffers
	(set-buffer (car buffers))
	(if (eq gud-minor-mode 'gdba)
	    (if (string-match "^\*" (buffer-name))
		(kill-buffer nil)
	      (if (display-graphic-p)
		  (remove-images (point-min) (point-max))
		(remove-strings (point-min) (point-max)))
	      (setq left-margin-width 0)
	      (if (get-buffer-window (current-buffer))
		  (set-window-margins (get-buffer-window
				       (current-buffer))
				      left-margin-width
				      right-margin-width))))
	(setq buffers (cdr buffers)))))
  (if (eq (selected-window) (minibuffer-window))
      (other-window 1))
  (delete-other-windows))

(defun gdb-source-info ()
  (goto-char (point-min))
  (re-search-forward "directory is ")
  (looking-at "\\(\\S-*\\)")
  (setq gdb-cdir (buffer-substring (match-beginning 1) (match-end 1)))
  (re-search-forward "Located in ")
  (looking-at "\\(\\S-*\\)")
  (setq gdb-main-file (buffer-substring (match-beginning 1) (match-end 1)))
  ;; Make sure we are not in the minibuffer window when we try to delete
  ;; all other windows.
  (if (eq (selected-window) (minibuffer-window))
      (other-window 1))
  (delete-other-windows)
  (if gdb-many-windows
      (gdb-setup-windows gdb-buffer-instance)
;else
    (gdb-display-breakpoints-buffer gdb-buffer-instance)
    (gdb-display-display-buffer instance)
    (gdb-display-stack-buffer instance)
    (delete-other-windows)
    (split-window)
    (other-window 1)
    (switch-to-buffer (gud-find-file gdb-main-file))
    (other-window 1)
    (setq gdb-source-window (get-buffer-window (current-buffer)))))

;from put-image
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

;from remove-images
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

(defvar gdb-array-slice-map nil)
(setq gdb-array-slice-map (make-keymap))
(define-key gdb-array-slice-map [mouse-2] 'gdb-array-slice)

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

(defun gdb-array-visualise ()
  "Visualise arrays and slices using graph program from plotutils."
  (Interactive)
  (if (and (display-graphic-p) gdb-display-string)
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
	       (throw 'multi-dimensional))
	   (setq m (+ m 1))))
       (shell-command (concat "echo" gdb-display-string " | graph -a 1 "
			      (int-to-string (aref gdb-array-start n))
			      " -x "
			      (int-to-string (aref gdb-array-start n))
			      " "
			      (int-to-string (aref gdb-array-stop  n))
			      " 1 -T X"))))))

(defun gdb-delete-display ()
  "Delete displayed expression and its frame."
  (interactive)
  (gdb-instance-enqueue-idle-input
   gdb-buffer-instance
   (list (concat "server delete display " gdb-display-number "\n")
	 '(lambda () nil)))
  (kill-buffer nil)
  (delete-frame))

;;
;; Assembler buffer
;;

(def-gdb-auto-updated-buffer gdb-assembler-buffer
  gdb-invalidate-assembler
  (concat "server disassemble " gdb-main-or-pc "\n")
  gdb-assembler-handler
  gdb-assembler-custom)

(defun gdb-assembler-custom ()
  (let ((buffer (gdb-get-instance-buffer gdb-buffer-instance
					 'gdb-assembler-buffer))
	(gdb-arrow-position))
    (if gdb-current-address
	(progn
	  (save-excursion
	    (set-buffer buffer)
	    (remove-arrow)
	    (goto-char (point-min))
	    (re-search-forward gdb-current-address)
	    (setq gdb-arrow-position (point))
	    (put-arrow "=>" gdb-arrow-position nil 'left-margin))))

; remove all breakpoint-icons in assembler buffer  before updating.
    (save-excursion
      (set-buffer buffer)
      (if (display-graphic-p)
	  (remove-images (point-min) (point-max))
	(remove-strings (point-min) (point-max))))
    (save-excursion
      (set-buffer (gdb-get-instance-buffer instance 'gdb-breakpoints-buffer))
      (goto-char (point-min))
      (while (< (point) (- (point-max) 1))
	(forward-line 1)
	(if (looking-at "[^\t].*breakpoint")
	    (progn
	      (looking-at
	       "\\([0-9]*\\)\\s-*\\S-*\\s-*\\S-*\\s-*\\(.\\)\\s-*0x0\\(\\S-*\\)")
	      ; info break gives '0x0' (8 digit) while dump gives '0x' (7 digit)
	      (setq address (concat "0x" (buffer-substring (match-beginning 3)
							   (match-end 3))))
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

(gdb-set-instance-buffer-rules 'gdb-assembler-buffer
			       'gdb-assembler-buffer-name
			       'gdb-assembler-mode)

(defvar gdb-assembler-mode-map nil)
(setq gdb-assembler-mode-map (make-keymap))
(suppress-keymap gdb-assembler-mode-map)

(defun gdb-assembler-mode ()
  "Major mode for viewing code assembler.

\\{gdb-assembler-mode-map}"
  (setq major-mode 'gdb-assembler-mode)
  (setq mode-name "Assembler")
  (set (make-local-variable 'gud-minor-mode) 'gdba)
  (set (make-local-variable 'tool-bar-map) gud-tool-bar-map)
  (set (make-variable-buffer-local 'left-margin-width) 2)
  (setq buffer-read-only t)
  (use-local-map gdb-assembler-mode-map)
  (gdb-invalidate-assembler gdb-buffer-instance)
  (gdb-invalidate-breakpoints gdb-buffer-instance))

(defun gdb-assembler-buffer-name (instance)
  (save-excursion
    (set-buffer (process-buffer (gdb-instance-process instance)))
    (concat "*Machine Code " (gdb-instance-target-string instance) "*")))

(defun gdb-display-assembler-buffer (instance)
  (interactive (list (gdb-needed-default-instance)))
  (gdb-display-buffer
   (gdb-get-create-instance-buffer instance
				   'gdb-assembler-buffer)))

(defun gdb-frame-assembler-buffer (instance)
  (interactive (list (gdb-needed-default-instance)))
  (switch-to-buffer-other-frame
   (gdb-get-create-instance-buffer instance
				   'gdb-assembler-buffer)))

(defun gdb-invalidate-frame-and-assembler (instance &optional ignored)
  (gdb-invalidate-frames instance)
  (gdb-invalidate-assembler instance))

(defun gdb-invalidate-breakpoints-and-assembler (instance &optional ignored)
  (gdb-invalidate-breakpoints instance)
  (gdb-invalidate-assembler instance))

; modified because if gdb-main-or-pc has changed value a new command
; must be enqueued to update the buffer with the new output
(defun gdb-invalidate-assembler (instance &optional ignored)
  (if (and ((lambda (instance)
	      (gdb-get-instance-buffer instance
				       (quote gdb-assembler-buffer))) instance)
	   (or (not (member (quote gdb-invalidate-assembler)
			(gdb-instance-pending-triggers instance)))
	   (not (string-equal gdb-main-or-pc gdb-prev-main-or-pc))))
      (progn

        ; take previous disassemble command off the queue
	(save-excursion
	  (set-buffer (gdb-get-instance-buffer instance 'gdba))
	   (let ((queue gdb-idle-input-queue) (item))
	     (while queue
	       (setq item (car queue))
	       (if (equal (cdr item) '(gdb-assembler-handler))
		   (delete item gdb-idle-input-queue))
		 (setq queue (cdr queue)))))

	(gdb-instance-enqueue-idle-input
	      instance (list (concat "server disassemble " gdb-main-or-pc "\n")
			     (quote gdb-assembler-handler)))
	     (set-gdb-instance-pending-triggers
	      instance (cons (quote gdb-invalidate-assembler)
			     (gdb-instance-pending-triggers instance)))
	     (setq gdb-prev-main-or-pc gdb-main-or-pc))))

(defun gdb-delete-line ()
"Delete current line."
(interactive)
  (let ((start (progn (beginning-of-line) (point)))
	(end (progn (end-of-line) (+ (point) 1))))
    (delete-region start end)))

(provide 'gdb-ui)

;;; gdb-ui.el ends here
