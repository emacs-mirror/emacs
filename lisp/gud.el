;;; gud.el --- Grand Unified Debugger mode for gdb, sdb, or dbx under Emacs

;; Author: Eric S. Raymond <eric@snark.thyrsus.com>
;; Keywords: unix, tools

;;	%W%

;; Copyright (C) 1992 Free Software Foundation, Inc.

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

;; The ancestral gdb.el was by W. Schelter <wfs@rascal.ics.utexas.edu>
;; It was later rewritten by rms.  Some ideas were due to Masanobu. 
;; Grand Unification (sdb/dbx support) by Eric S. Raymond <esr@thyrsus.com>
;; The overloading code was then rewritten by Barry Warsaw <bwarsaw@cen.com>,
;; who also hacked the mode to use comint.el.

;; Note: use of this package with sdb requires that your tags.el support
;; the find-tag-noselect entry point.  Stock distributions up to 18.57 do 
;; *not* include this feature; if it's not included with this file, email
;; esr@snark.thyrsus.com for it or get 18.58.

;; Further note: due to lossage in the Emacs-18 byte compiler, compiled
;; versions of this code will fail with a complaint about gud-step if
;; you invoke the gdb or sdb initializers.  This should be fixed in 19.

;;; Code:

(require 'comint)
(require 'etags)

;; ======================================================================
;; the overloading mechanism

(defun gud-overload-functions (gud-overload-alist)
  "Overload functions defined in GUD-OVERLOAD-ALIST.
This association list has elements of the form
     (ORIGINAL-FUNCTION-NAME  OVERLOAD-FUNCTION)"
  (mapcar
   (function (lambda (p) (fset (car p) (symbol-function (cdr p)))))
   gud-overload-alist))

(defun gud-debugger-startup (f d)
  (error "GUD not properly entered."))

(defun gud-marker-filter (proc s)
  (error "GUD not properly entered."))

(defun gud-visit-file (f)
  (error "GUD not properly entered."))

(defun gud-set-break (proc f n rest)
  (error "GUD not properly entered."))

;; This macro is used below to define some basic debugger interface commands.
;; Of course you may use `gud-def' with any other debugger command, including
;; user defined ones.

;; A macro call like (gud-def FUNC NAME KEY DOC) expands to a form
;; which defines FUNC to send the command NAME to the debugger, gives
;; it the docstring DOC, and binds that function to KEY.  NAME should
;; be a string.  If a numeric prefix argument is given to FUNC, it
;; gets sent after NAME.

(defmacro gud-def (func name key &optional doc)
  (let* ((cstr (list 'if '(not (= 1 arg))
		     (list 'format "%s %s" name 'arg)
		     name)))
    (list 'progn
 	  (list 'defun func '(arg)
		(or doc "")
		'(interactive "p")
		(list 'gud-call cstr))
	  (if key
	      (list 'define-key 'gud-mode-map key  (list 'quote func))))))

;; Where gud-display-frame should put the debugging arrow.  This is
;; set by the marker-filter, which scans the debugger's output for
;; indications of the current pc.
(defvar gud-last-frame nil)

;; All debugger-specific information is collected here
;; Here's how it works, in case you ever need to add a debugger to the table.
;;
;; Each entry must define the following at startup:
;;
;;<name>
;; comint-prompt-regexp
;; gud-<name>-debugger-startup
;; gud-<name>-marker-filter
;; gud-<name>-visit-file
;; gud-<name>-set-break
;;
;; The job of the startup-command method is to fire up a copy of the debugger,
;; given an object file and source directory.
;;
;; The job of the marker-filter method is to detect file/line markers in
;; strings and set the global gud-last-frame to indicate what display
;; action (if any) should be triggered by the marker.  Note that only
;; whetever the method *returns* is displayed in the buffer; thus, you
;; can filter the debugger's output, interpreting some and passing on
;; the rest.
;;
;; The job of the visit-file method is to visit and return the buffer indicated
;; by the car of gud-tag-frame.  This may be a file name, a tag name, or
;; something else.
;;
;; The job of the gud-set-break method is to send the commands
;; necessary to set a breakpoint at a given line in a given source
;; file.  If its third argument TEMP is non-nil, the breakpoint set
;; should be temporary - it should be deleted when it is reached.  If
;; the debugger doesn't support such breakpoints, it should set an
;; ordinary breakpoint.
;;
;; Debugger-specific information begins here:

;; ======================================================================
;; gdb functions

(defun gud-gdb-debugger-startup (f d)
  (make-comint (concat "gud-" f) "gdb" nil "-fullname" "-cd" d f))

(defun gud-gdb-marker-filter (proc string)
  (if (string-match  "\032\032\\([^:\n]*\\):\\([0-9]*\\):.*\n" string)
      (progn
	(setq gud-last-frame
	      (cons
	       (substring string (match-beginning 1) (match-end 1))
	       (string-to-int
		(substring string (match-beginning 2) (match-end 2)))))
	;; this computation means the ^Z^Z-initiated marker in the
	;; input string is never emitted.
	(concat
	 (substring string 0 (match-beginning 0))
	 (substring string (match-end 0))
	 ))
    string))

(defun gud-gdb-visit-file (f)
  (find-file-noselect f))

(defun gud-gdb-set-break (proc f n temp) 
  (gud-call "%s %s:%d" (if temp "tbreak" "break") f n))

;;;###autoload
(defun gdb (path)
  "Run gdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive "fRun gdb on file: ")
  (gud-overload-functions '((gud-debugger-startup . gud-gdb-debugger-startup)
			    (gud-marker-filter    . gud-gdb-marker-filter)
			    (gud-visit-file       . gud-gdb-visit-file)
			    (gud-set-break        . gud-gdb-set-break)))

  (gud-def gud-step   "step"   "\C-c\C-s" "Step one source line with display")
  (gud-def gud-stepi  "stepi"  "\C-c\C-i" "Step one instruction with display")
  (gud-def gud-next   "next"   "\C-c\C-n" "Step one line (skip functions)")
  (gud-def gud-cont   "cont"   "\C-c\C-r" "Continue with display")

  (gud-def gud-finish "finish" "\C-c\C-f" "Finish executing current function")
  (gud-def gud-up     "up"     "\C-c<"    "Up N stack frames (numeric arg)")
  (gud-def gud-down   "down"   "\C-c>"    "Down N stack frames (numeric arg)")

  (gud-common-init path)

  (setq comint-prompt-regexp "^(.*gdb[+]?) *")
  (run-hooks 'gdb-mode-hook)
  )


;; ======================================================================
;; sdb functions

(defun gud-sdb-debugger-startup (f d)
  (make-comint (concat "gud-" f) "sdb" nil f "-" d))

(defun gud-sdb-marker-filter (proc string)
  (if (string-match "\\(^0x\\w* in \\|^\\|\n\\)\\([^:\n]*\\):\\([0-9]*\\):.*\n"
		    string)
      (setq gud-last-frame
	    (cons
	     (substring string (match-beginning 2) (match-end 2))
	     (string-to-int 
	      (substring string (match-beginning 3) (match-end 3))))))
  string)

(defun gud-sdb-visit-file (f)
  (find-tag-noselect f))

;;; We'll just ignore the TEMP argument for now; I don't know how to
;;; set temporary breakpoints in sdb.  (See the description of the
;;; gud-set-break method for details.)
(defun gud-sdb-set-break (proc f n temp)
  (gud-queue-send (format "e %s" f) (format "%d b" n)))

;;;###autoload
(defun sdb (path)
  "Run sdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive "fRun sdb on file: ")
  (if (not (and (boundp 'tags-file-name) (file-exists-p tags-file-name)))
      (error "The sdb support requires a valid tags table to work."))
  (gud-overload-functions '((gud-debugger-startup . gud-sdb-debugger-startup)
			    (gud-marker-filter    . gud-sdb-marker-filter)
			    (gud-visit-file       . gud-sdb-visit-file)
			    (gud-set-break        . gud-sdb-set-break)))

  (gud-def gud-step  "s"   "\C-c\C-s"	"Step one source line with display")
  (gud-def gud-stepi "i"   "\C-c\C-i"	"Step one instruction with display")
  (gud-def gud-next  "S"   "\C-c\C-n"	"Step one source line (skip functions)")
  (gud-def gud-cont  "c"   "\C-c\C-r"	"Continue with display (`resume')")

  (gud-common-init path)

  (setq comint-prompt-regexp  "\\(^\\|\n\\)\\*")
  (run-hooks 'sdb-mode-hook)
  )

;; ======================================================================
;; dbx functions

(defun gud-dbx-debugger-startup (f d)
  (make-comint (concat "gud-" f) "dbx" nil f))

(defun gud-dbx-marker-filter (proc string)
  (if (string-match
       "stopped in .* at line \\([0-9]*\\) in file \"\\([^\"]*\\)\"" string)
      (setq gud-last-frame
	    (cons
	     (substring string (match-beginning 2) (match-end 2))
	     (string-to-int 
	      (substring string (match-beginning 1) (match-end 1))))))
  string)

(defun gud-dbx-visit-file (f)
  (find-file-noselect f))

;;; We'll just ignore the TEMP argument for now; I don't know how to
;;; set temporary breakpoints in dbx.  (See the description of the
;;; gud-set-break method for details.)
(defun gud-dbx-set-break (proc f n temp)
  (gud-call "stop at \"%s\":%d" f n))

;;;###autoload
(defun dbx (path)
  "Run dbx on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive "fRun dbx on file: ")
  (gud-overload-functions '((gud-debugger-startup . gud-dbx-debugger-startup)
			    (gud-marker-filter    . gud-dbx-marker-filter)
			    (gud-visit-file       . gud-dbx-visit-file)
			    (gud-set-break        . gud-dbx-set-break)))

  (gud-def gud-step   "step"   "\C-c\C-s" "Step one source line with display")
  (gud-def gud-stepi  "stepi"  "\C-c\C-i" "Step one instruction with display")
  (gud-def gud-next   "next"   "\C-c\C-n" "Step one line (skip functions)")
  (gud-def gud-cont   "cont"   "\C-c\C-r" "Continue with display (`resume')")

  (gud-def gud-up     "up"     "\C-c<"    "Up N stack frames (numeric arg)")
  (gud-def gud-down   "down"   "\C-c>"    "Down N stack frames (numeric arg)")

  (gud-common-init path)
  (setq comint-prompt-regexp  "^[^)]*dbx) *")

  (run-hooks 'dbx-mode-hook)
  )

;;
;; End of debugger-specific information
;;

(defvar gud-mode-map nil
  "Keymap for gud-mode.")

(defvar gud-commands nil
  "List of strings or functions used by send-gud-command.
It is for customization by you.")

(defvar gud-command-queue nil)

;;; When we send a command to the debugger via gud-call, it's annoying
;;; to see the command and the new prompt inserted into the debugger's
;;; buffer; we have other ways of knowing the command has completed.
;;;
;;; If the buffer looks like this:
;;; --------------------
;;; (gdb) set args foo bar
;;; (gdb) -!-
;;; --------------------
;;; (the -!- marks the location of point), and we type `C-x SPC' in a
;;; source file to set a breakpoint, we want the buffer to end up like
;;; this:
;;; --------------------
;;; (gdb) set args foo bar
;;; Breakpoint 1 at 0x92: file make-docfile.c, line 49.
;;; (gdb) -!-
;;; --------------------
;;; Essentially, the old prompt is deleted, and the command's output
;;; and the new prompt take its place.
;;;
;;; Not echoing the command is easy enough; you send it directly using
;;; process-send-string, and it never enters the buffer.  However,
;;; getting rid of the old prompt is trickier; you don't want to do it
;;; when you send the command, since that will result in an annoying
;;; flicker as the prompt is deleted, redisplay occurs while Emacs
;;; waits for a response from the debugger, and the new prompt is
;;; inserted.  Instead, we'll wait until we actually get some output
;;; from the subprocess before we delete the prompt.  If the command
;;; produced no output other than a new prompt, that prompt will most
;;; likely be in the first chunk of output received, so we will delete
;;; the prompt and then replace it with an identical one.  If the
;;; command produces output, the prompt is moving anyway, so the
;;; flicker won't be annoying.
;;;
;;; So - when we want to delete the prompt upon receipt of the next
;;; chunk of debugger output, we position gud-delete-prompt-marker at
;;; the start of the prompt; the process filter will notice this, and
;;; delete all text between it and the process output marker.  If
;;; gud-delete-prompt-marker points nowhere, we leave the current
;;; prompt alone.
(defvar gud-delete-prompt-marker nil)

(if gud-mode-map
   nil
  (setq gud-mode-map (copy-keymap comint-mode-map))
  (define-key gud-mode-map "\C-c\C-l" 'gud-refresh))

;; Global mappings --- we'll invoke these from a source buffer.
(define-key ctl-x-map " " 'gud-break)
(define-key ctl-x-map "&" 'send-gud-command)


(defun gud-mode ()
  "Major mode for interacting with an inferior debugger process.

   You start it up with one of the commands M-x gdb, M-x sdb, or
M-x dbx.  Each entry point finishes by executing a hook; gdb-mode-hook,
sdb-mode-hook or dbx-mode-hook respectively.

After startup, the following commands are available:

\\{gud-mode-map}

\\[gud-refresh] displays in the other window the last line referred to
in the gud buffer.

\\[gud-step], \\[gud-next], and \\[gud-stepi] in the gud window,
do a step-one-line, step-one-line (not entering function calls), and 
step-one-instruction and then update the other window
with the current file and position.  \\[gud-cont] continues
execution.

The above commands are common to all supported debuggers.  If you are
using gdb or dbx, the following additional commands will be available:

\\[gud-up] pops up through an enclosing stack frame.  \\[gud-down] drops
back down through one.

If you are using gdb, \\[gdb-finish] runs execution to the return from
the current function and stops.

These functions repeat themselves the appropriate number of times if you give a
prefix argument.

If you are in a source file, you may do the following:

Set a breakpoint at the current line by doing \\[gud-break].  This causes
an appropriate set-break to be send to the debugger; of course, if the file
you're visiting doesn't correspond to any code in the executable this will
have no effect or raise an error.

Execute a user-defined command at point with \\[send-gud-command]; the
prefix argument is taken as an index into the list of strings gud-commands.
A %s in a gud-commands string is substituted with a number or address picked
up from point.

Other commands for interacting with the debugger process are inherited from
comint mode, which see."
  (interactive)
  (comint-mode)
; (kill-all-local-variables)
  (setq major-mode 'gud-mode)
  (setq mode-name "Debugger")
  (setq mode-line-process '(": %s"))
  (use-local-map gud-mode-map)
  (make-local-variable 'gud-last-frame)
  (setq gud-last-frame nil)
  (make-local-variable 'comint-prompt-regexp)
  (make-local-variable 'gud-delete-prompt-marker)
  (setq gud-delete-prompt-marker (make-marker))
  (run-hooks 'gud-mode-hook)
)

(defvar current-gud-buffer nil)

(defun gud-common-init (path)
  ;; perform initializations common to all debuggers
  (setq path (expand-file-name path))
  (let ((file (file-name-nondirectory path)))
    (switch-to-buffer (concat "*gud-" file "*"))
    (setq default-directory (file-name-directory path))
    (or (bolp) (newline))
    (insert "Current directory is " default-directory "\n")
    (gud-debugger-startup file default-directory))
  (gud-mode)
  (set-process-filter (get-buffer-process (current-buffer)) 'gud-filter)
  (set-process-sentinel (get-buffer-process (current-buffer)) 'gud-sentinel)
  (setq gud-command-queue nil)
  (gud-set-buffer)
  )

(defun gud-set-buffer ()
  (cond ((eq major-mode 'gud-mode)
	(setq current-gud-buffer (current-buffer)))))

(defun gud-filter (proc string)
  ;; This function is responsible for inserting output from your debugger
  ;; into the buffer.  The hard work is done by the method that is
  ;; the value of gud-marker-filter.
  (let ((inhibit-quit t))
    (gud-filter-insert proc (gud-marker-filter proc string))
    ;; If we've got queued commands and we see a prompt, pop one and send it.
    ;; In theory we should check that a prompt has been issued before sending
    ;; queued commands.  In practice, command responses from the first through
    ;; penultimate elements of a command sequence are short enough that we
    ;; don't really have to bother.
    (if gud-command-queue
	(progn
	  (gud-call (car gud-command-queue))
	  (setq gud-command-queue (cdr gud-command-queue))
	  )
      )))

(defun gud-filter-insert (proc string)
  ;; Here's where the actual buffer insertion is done
  (save-excursion
    (set-buffer (process-buffer proc))
    (let ((moving (= (point) (process-mark proc)))
	  (output-after-point (< (point) (process-mark proc))))
      (save-excursion
	(goto-char (process-mark proc))
	;; If we have been so requested, delete the debugger prompt.
	(if (marker-buffer gud-delete-prompt-marker)
	    (progn
	      (delete-region (point) gud-delete-prompt-marker)
	      (set-marker gud-delete-prompt-marker nil)))
	(insert-before-markers string)
	;; Check for a filename-and-line number.
	;; Don't display the specified file
	;; unless (1) point is at or after the position where output appears
	;; and (2) this buffer is on the screen.
	(if (and gud-last-frame
		 (not output-after-point)
		 (get-buffer-window (current-buffer)))
	    (gud-display-frame)))
      (if moving (goto-char (process-mark proc))))))

(defun gud-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 ;; Fix the mode line.
	 (setq mode-line-process
	       (concat ": "
		       (symbol-name (process-status proc))))
	 (let* ((obuf (current-buffer)))
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 ;; Force mode line redisplay soon
		 (set-buffer-modified-p (buffer-modified-p))
		 (if (eobp)
		     (insert ?\n mode-name " " msg)
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the gud buffer.
	     (set-buffer obuf))))))


(defun gud-refresh (&optional arg)
  "Fix up a possibly garbled display, and redraw the arrow."
  (interactive "P")
  (recenter arg)
  (gud-display-frame))

(defun gud-display-frame ()
  "Find and obey the last filename-and-line marker from the debugger.
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (if gud-last-frame
   (progn 
     (gud-set-buffer)
     (gud-display-line (car gud-last-frame) (cdr gud-last-frame))
     (setq gud-last-frame nil))))

;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
;; and that its line LINE is visible.
;; Put the overlay-arrow on the line LINE in that buffer.

(defun gud-display-line (true-file line)
  (let* ((buffer (gud-visit-file true-file))
	 (window (display-buffer buffer t))
	 (pos))
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(widen)
	(goto-line line)
	(setq pos (point))
	(setq overlay-arrow-string "=>")
	(or overlay-arrow-position
	    (setq overlay-arrow-position (make-marker)))
	(set-marker overlay-arrow-position (point) (current-buffer)))
      (cond ((or (< pos (point-min)) (> pos (point-max)))
	     (widen)
	     (goto-char pos))))
    (set-window-point window overlay-arrow-position)))

(defun gud-call (command &rest args)
  "Invoke the debugger COMMAND displaying source in other window."
  (interactive)
  (gud-set-buffer)
  (let ((command (concat (apply 'format command args) "\n"))
	(proc (get-buffer-process current-gud-buffer)))

    ;; Arrange for the current prompt to get deleted.
    (save-excursion
      (set-buffer current-gud-buffer)
      (goto-char (process-mark proc))
      (beginning-of-line)
      (if (looking-at comint-prompt-regexp)
	  (set-marker gud-delete-prompt-marker (point))))

    (goto-char (point-max))
    (process-send-string proc command)))

(defun gud-queue-send (&rest cmdlist)
  ;; Send the first command, queue the rest for send after successive
  ;; send on subsequent prompts
  (interactive)
  (gud-call (car cmdlist))
  (setq gud-command-queue (append gud-command-queue (cdr cmdlist))))

(defun gud-apply-from-source (func &rest args)
  ;; Apply a method from the gud buffer environment, passing it file
  ;; and line, then ARGS.  This is intended to be used for gud
  ;; commands called from a source file.
  (if (not buffer-file-name)
      (error "There is no file associated with this buffer")) 
  (let ((file (file-name-nondirectory buffer-file-name))
	(line (save-restriction (widen) (1+ (count-lines 1 (point))))))
    (save-excursion
      (gud-set-buffer)
      (apply func
	     (get-buffer-process current-gud-buffer)
	     file
	     line
	     args)
      )))

(defun gud-break (arg)
  "Set breakpoint at this source line.
With prefix argument, set a temporary breakpoint, if the debugger in
use supports such things.  (A temporary breakpoint is one which will
be deleted when it is reached.)"
  (interactive "P")
  (gud-apply-from-source 'gud-set-break arg))

(defun gud-read-address ()
  "Return a string containing the core-address found in the buffer at point."
  (save-excursion
    (let ((pt (point)) found begin)
      (setq found (if (search-backward "0x" (- pt 7) t)(point)))
      (cond
       (found (forward-char 2)
	      (buffer-substring found
				(progn (re-search-forward "[^0-9a-f]")
				       (forward-char -1)
				       (point))))
       (t (setq begin (progn (re-search-backward "[^0-9]") 
			     (forward-char 1)
			     (point)))
	  (forward-char 1)
	  (re-search-forward "[^0-9]")
	  (forward-char -1)
	  (buffer-substring begin (point)))))))


(defun send-gud-command (arg)
  "This command reads the number where the cursor is positioned.  A numeric arg
selects the ARG'th member COMMAND of the list gud-commands.  If COMMAND is a
string, (format COMMAND ADDR) is inserted at the end of the debugger buffer,
otherwise (funcall COMMAND ADDR) is inserted.
   For example, \"p (rtx)%s->fld[0].rtint\" is a possible string to be a
member of gud-commands."
  (interactive "P")
  (let (comm addr)
    (if arg (setq comm (nth arg gud-commands)))
    (setq addr (gud-read-address))
    (if (eq (current-buffer) current-gud-buffer)
	(set-mark (point)))
    (cond (comm
	   (setq comm
		 (if (stringp comm) (format comm addr) (funcall comm addr))))
	  (t (setq comm addr)))
    (switch-to-buffer current-gud-buffer)
    (goto-char (point-max))
    (insert-string comm)))

;;; gud.el ends here
