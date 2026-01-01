;;; esh-mode.el --- user interface  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2026 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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

;; Basically, Eshell is used just like shell mode (<M-x shell>).  The
;; keystrokes for navigating the buffer, and accessing the command
;; history, are identical.  Unlike shell mode, however, Eshell mode's
;; governing process is Emacs itself.  With shell mode, an inferior
;; shell process is executed that communicates with Emacs via comint
;; -- a mode for handling sub-process interaction.  Eshell mode, on
;; the other hand, is a truly native Emacs shell.  No subprocess are
;; invoked except the ones requested by the user at the prompt.
;;
;; After entering a command, use <RET> to invoke it ([Command
;; invocation]) .  If there is a command on disk, it will be executed
;; as in a normal shell.  If there is no command by that name on disk,
;; but a Lisp function with that name is defined, the Lisp function
;; will be called, using the arguments passed on the command line.
;;
;; Some of the other features of the command interaction mode are:
;;
;; @ <M-RET> can be used to accumulate further commands while a
;;   command is currently running.  Since all input is passed to the
;;   subprocess being executed, there is no automatic input queueing
;;   as there is with other shells.
;;
;; @ <C-c C-t> can be used to truncate the buffer if it grows too
;;   large.
;;
;; @ <C-c C-r> will move point to the beginning of the output of the
;;   last command.  With a prefix argument, it will narrow to view
;;   only that output.
;;
;; @ <C-c C-o> will delete the output from the last command.
;;
;; @ <C-c C-f> will move forward a complete shell argument.
;;
;; @ <C-c C-b> will move backward a complete shell argument.

;;; Code:

;; Load the core Eshell modules; we'll call their initialization
;; functions below in `eshell-mode'.
(require 'esh-arg)
(require 'esh-cmd)
(require 'esh-ext)
(require 'esh-io)
(require 'esh-module)
(require 'esh-proc)
(require 'esh-util)
(require 'esh-var)

(defgroup eshell-mode nil
  "This module contains code for handling input from the user."
  :tag "User interface"
  :group 'eshell)

;;; User Variables:

(defcustom eshell-mode-unload-hook nil
  "A hook that gets run when `eshell-mode' is unloaded."
  :type 'hook)
(make-obsolete-variable 'eshell-mode-unload-hook nil "30.1")

(defcustom eshell-mode-hook nil
  "A hook that gets run when `eshell-mode' is entered."
  :type 'hook)

(defcustom eshell-first-time-mode-hook nil
  "A hook that gets run the first time `eshell-mode' is entered.
That is to say, the first time during an Emacs session."
  :type 'hook)

(defcustom eshell-after-initialize-hook nil
  "A hook that gets run after an Eshell session has been fully initialized."
  :type 'hook)

(defcustom eshell-exit-hook nil
  "A hook that is run whenever `eshell' is exited.
This hook is only run if exiting actually kills the buffer."
  :version "24.1"                       ; removed eshell-query-kill-processes
  :type 'hook)

(defcustom eshell-kill-on-exit t
  "If non-nil, kill the Eshell buffer on the `exit' command.
Otherwise, the buffer will simply be buried."
  :type 'boolean)

(defcustom eshell-input-filter-functions nil
  "Functions to call before input is processed.
The input is contained in the region from `eshell-last-input-start' to
`eshell-last-input-end'."
  :type 'hook)

(defcustom eshell-send-direct-to-subprocesses nil
  "If t, send any input immediately to a subprocess."
  :type 'boolean)

(defcustom eshell-expand-input-functions nil
  "Functions to call before input is parsed.
Each function is passed two arguments, which bounds the region of the
current input text."
  :type 'hook)

(defcustom eshell-scroll-to-bottom-on-input nil
  "Controls whether input to interpreter causes window to scroll.
If nil, then do not scroll.  If t or `all', scroll all windows showing
buffer.  If `this', scroll only the selected window.

See `eshell-preinput-scroll-to-bottom'."
  :type '(radio (const :tag "Do not scroll Eshell windows" nil)
                (choice :tag "Scroll all windows showing the buffer"
                        :value all
                        (const t)
                        (const all))
                (const :tag "Scroll only the selected window" this)))

(defcustom eshell-scroll-to-bottom-on-output nil
  "Controls whether interpreter output causes window to scroll.
If nil, then do not scroll.  If t or `all', scroll all windows showing
buffer.  If `this', scroll only the selected window.  If `others',
scroll only those that are not the selected window.

See variable `eshell-scroll-show-maximum-output' and function
`eshell-postoutput-scroll-to-bottom'."
  :type '(radio (const :tag "Do not scroll Eshell windows" nil)
                (choice :tag "Scroll all windows showing the buffer"
                        :value all
                        (const t)
                        (const all))
		(const :tag "Scroll only the selected window" this)
                (const :tag "Scroll all windows other than selected" others)))

(defcustom eshell-scroll-show-maximum-output t
  "Controls how interpreter output causes window to scroll.
If non-nil, then show the maximum output when the window is scrolled.

See variable `eshell-scroll-to-bottom-on-output' and function
`eshell-postoutput-scroll-to-bottom'."
  :type 'boolean)

(defcustom eshell-buffer-maximum-lines 1024
  "The maximum size in lines for eshell buffers.
Eshell buffers are truncated from the top to be no greater than this
number, if the function `eshell-truncate-buffer' is on
`eshell-output-filter-functions'."
  :type 'natnum)

(defcustom eshell-output-filter-functions
  '(eshell-postoutput-scroll-to-bottom
    eshell-handle-control-codes
    eshell-handle-ansi-color
    eshell-watch-for-password-prompt)
  "Functions to call before output is displayed.
These functions are only called for output that is displayed
interactively (see `eshell-interactive-filter'), and not for
output which is redirected."
  :type 'hook)

(defcustom eshell-preoutput-filter-functions nil
  "Functions to call before output is inserted into the buffer.
These functions get one argument, a string containing the text to be
inserted.  They return the string as it should be inserted."
  :type 'hook)

(defcustom eshell-password-prompt-regexp
  (format "%s[^%s]*[%s]\\s *\\'"
          (regexp-opt password-word-equivalents t)
          (apply #'string password-colon-equivalents)
          (apply #'string password-colon-equivalents))
  "Regexp matching prompts for passwords in the inferior process.
This is used by `eshell-watch-for-password-prompt'."
  :type 'regexp
  :version "27.1")

(defcustom eshell-skip-prompt-function nil
  "A function called from beginning of line to skip the prompt."
  :type '(choice (const nil) function))

(make-obsolete-variable 'eshell-skip-prompt-function nil "30.1")

(defcustom eshell-status-in-mode-line t
  "If non-nil, let the user know a command is running in the mode line."
  :type 'boolean)

(defcustom eshell-directory-name
  (locate-user-emacs-file "eshell/" ".eshell/")
  "The directory where Eshell control files should be kept."
  :type 'directory
  :group 'eshell)

(defvar eshell-password-prompt-max-length 256
  "The maximum amount of text to examine when matching password prompts.
This is used by `eshell-watch-for-password-prompt' to reduce the amount
of time spent searching for password prompts.")

(defvar eshell-first-time-p t
  "A variable which is non-nil the first time Eshell is loaded.")

(defvar eshell-non-interactive-p nil
  "A variable which is non-nil when Eshell is not running interactively.
Modules should use this variable so that they don't clutter
non-interactive sessions, such as when using `eshell-command'.")

;; Internal Variables:

;; these are only set to nil initially for the sake of the
;; byte-compiler, when compiling other files which `require' this one
(defvar eshell-mode nil)
(defvar eshell-command-running-string "--")
(defvar eshell-last-input-start nil)
(defvar eshell-last-input-end nil)
(defvar eshell-last-output-start nil)
(defvar eshell-last-output-block-begin nil)
(defvar eshell-last-output-end nil)

(defvar eshell-currently-handling-window nil)

(define-abbrev-table 'eshell-mode-abbrev-table ())

(defvar eshell-mode-syntax-table
  (let ((st (make-syntax-table))
        (i 0))
    (while (< i ?0)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (modify-syntax-entry ?  "    " st)
    (modify-syntax-entry ?\t "    " st)
    (modify-syntax-entry ?\f "    " st)
    (modify-syntax-entry ?\n ">   " st)
    ;; Give CR the same syntax as newline, for selective-display.
    (modify-syntax-entry ?\^m ">   " st)
    ;; (modify-syntax-entry ?\; "<   " st)
    (modify-syntax-entry ?` "'   " st)
    (modify-syntax-entry ?' "'   " st)
    (modify-syntax-entry ?, "'   " st)
    ;; Used to be singlequote; changed for flonums.
    (modify-syntax-entry ?. "_   " st)
    (modify-syntax-entry ?- "_   " st)
    (modify-syntax-entry ?| ".   " st)
    (modify-syntax-entry ?# "'   " st)
    (modify-syntax-entry ?\" "\"    " st)
    (modify-syntax-entry ?\\ "/   " st)
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)
    (modify-syntax-entry ?\{ "(}  " st)
    (modify-syntax-entry ?\} "){  " st)
    (modify-syntax-entry ?\[ "(]  " st)
    (modify-syntax-entry ?\] ")[  " st)
    ;; All non-word multibyte characters should be `symbol'.
    (map-char-table
     (lambda (key _val)
       (and (if (consp key)
                (and (>= (car key) 128)
                     (/= (char-syntax (car key)) ?w))
              (and (>= key 256)
                   (/= (char-syntax key) ?w)))
            (modify-syntax-entry key "_   " st)))
     (standard-syntax-table))
    st))

(defvar-keymap eshell-mode-map
  "C-c"   'eshell-command-map
  "RET"   #'eshell-send-input
  "M-RET" #'eshell-queue-input
  "C-M-l" #'eshell-show-output)

(defvar-keymap eshell-command-map
  :prefix 'eshell-command-map
  "M-o" #'eshell-mark-output
  "M-d" #'eshell-toggle-direct-send
  "C-a" #'move-beginning-of-line
  "C-b" #'eshell-backward-argument
  "C-e" #'eshell-show-maximum-output
  "C-f" #'eshell-forward-argument
  "C-m" #'eshell-copy-old-input
  "C-o" #'eshell-delete-output
  "C-r" #'eshell-show-output
  "C-t" #'eshell-truncate-buffer
  "C-u" #'eshell-kill-input
  "C-w" #'backward-kill-word
  "C-y" #'eshell-repeat-argument)

(defvar-keymap eshell-command-repeat-map
  :doc "Keymap to repeat eshell-command key sequences.  Used in `repeat-mode'."
  :repeat t
  "C-f" #'eshell-forward-argument
  "C-b" #'eshell-backward-argument)

;;; User Functions:

(defun eshell-kill-buffer-function ()
  "Function added to `kill-buffer-hook' in Eshell buffers.
This runs the function `eshell-kill-processes-on-exit',
and the hook `eshell-exit-hook'."
  ;; It's fine to run this unconditionally since it can be customized
  ;; via the `eshell-kill-processes-on-exit' variable.
  (and (fboundp 'eshell-query-kill-processes)
       (not (memq #'eshell-query-kill-processes eshell-exit-hook))
       (eshell-query-kill-processes))
  (run-hooks 'eshell-exit-hook))

;;;###autoload
(define-derived-mode eshell-mode fundamental-mode "Eshell"
  "Emacs shell interactive mode."
  (setq-local eshell-mode t)

  (when (and eshell-status-in-mode-line
             (listp mode-line-format))
    (make-local-variable 'eshell-command-running-string)
    (let ((fmt (copy-sequence mode-line-format)))
      (setq-local mode-line-format fmt))
    (let ((mode-line-elt (cdr (memq 'mode-line-front-space mode-line-format))))
      (if mode-line-elt
	  (setcar mode-line-elt 'eshell-command-running-string))))

  (setq-local bookmark-make-record-function #'eshell-bookmark-make-record)
  (setq local-abbrev-table eshell-mode-abbrev-table)

  (setq-local window-point-insertion-type t)

  (setq-local list-buffers-directory (expand-file-name default-directory))

  ;; always set the tab width to 8 in Eshell buffers, since external
  ;; commands which do their own formatting almost always expect this
  (setq-local tab-width 8)

  ;; don't ever use auto-fill in Eshell buffers
  (setq auto-fill-function nil)

  ;; always display everything from a return value
  (setq-local print-length nil)
  (setq-local print-level nil)

  ;; set require-final-newline to nil; otherwise, all redirected
  ;; output will end with a newline, whether or not the source
  ;; indicated it!
  (setq-local require-final-newline nil)

  (setq-local max-lisp-eval-depth (max 3000 max-lisp-eval-depth))

  (setq-local eshell-last-input-start (point-marker))
  (setq-local eshell-last-input-end (point-marker))
  (setq-local eshell-last-output-start (point-marker))
  (setq-local eshell-last-output-end (point-marker))
  (setq-local eshell-last-output-block-begin (point))

  (add-function :filter-return (local 'filter-buffer-substring-function)
                #'eshell--unmark-string-as-output)

  (let ((modules-list (copy-sequence eshell-modules-list)))
    (setq-local eshell-modules-list modules-list))

  ;; This is to avoid making the paragraph base direction
  ;; right-to-left if the first word just happens to start with a
  ;; strong R2L character.
  (setq bidi-paragraph-direction 'left-to-right)

  ;; Load extension modules into memory.
  (eshell-load-modules eshell-modules-list)

  (unless (file-exists-p eshell-directory-name)
    (with-demoted-errors "Error creating Eshell directory: %s"
      (eshell-make-private-directory eshell-directory-name t)))

  ;; Initialize core Eshell modules, then extension modules, for this session.
  (eshell-initialize-modules (eshell-subgroups 'eshell))
  (eshell-initialize-modules eshell-modules-list)

  (if eshell-send-direct-to-subprocesses
      (add-hook 'pre-command-hook #'eshell-intercept-commands t t))

  (if eshell-scroll-to-bottom-on-input
      (add-hook 'pre-command-hook #'eshell-preinput-scroll-to-bottom t t))

  (when eshell-scroll-show-maximum-output
    (setq-local scroll-conservatively 1000))

  (when eshell-status-in-mode-line
    (add-hook 'eshell-pre-command-hook #'eshell-command-started nil t)
    (add-hook 'eshell-post-command-hook #'eshell-command-finished nil t))

  (add-hook 'kill-buffer-hook #'eshell-kill-buffer-function t t)

  (when eshell-first-time-p
    (setq eshell-first-time-p nil)
    (run-hooks 'eshell-first-time-mode-hook))
  (run-hooks 'eshell-after-initialize-hook)
  (run-hooks 'eshell-post-command-hook))

(put 'eshell-mode 'mode-class 'special)

(defun eshell-command-started ()
  "Indicate in the mode line that a command has started."
  (setq eshell-command-running-string "**")
  (force-mode-line-update))

(defun eshell-command-finished ()
  "Indicate in the mode line that a command has finished."
  (setq eshell-command-running-string "--")
  (force-mode-line-update))

;;; Internal Functions:

(defun eshell-toggle-direct-send ()
  (interactive)
  (if eshell-send-direct-to-subprocesses
      (progn
	(setq eshell-send-direct-to-subprocesses nil)
	(remove-hook 'pre-command-hook #'eshell-intercept-commands t)
	(message "Sending subprocess input on RET"))
    (setq eshell-send-direct-to-subprocesses t)
    (add-hook 'pre-command-hook #'eshell-intercept-commands t t)
    (message "Sending subprocess input directly")))

(defun eshell-self-insert-command ()
  (interactive)
  (process-send-string
   (eshell-head-process)
   (char-to-string (if (symbolp last-command-event)
		       (get last-command-event 'ascii-character)
		     last-command-event))))

(defun eshell-intercept-commands ()
  (when (and eshell-foreground-command
	     (not (and (integerp last-input-event)
		       (memq last-input-event '(?\C-x ?\C-c)))))
    (let ((possible-events (where-is-internal this-command))
	  (name (symbol-name this-command))
	  (intercept t))
      ;; Assume that any multikey combination which does NOT target an
      ;; Eshell command, is a combo the user wants invoked rather than
      ;; sent to the underlying subprocess.
      (unless (and (> (length name) 7)
		   (equal (substring name 0 7) "eshell-"))
	(while possible-events
	  (if (> (length (car possible-events)) 1)
	      (setq intercept nil possible-events nil)
	    (setq possible-events (cdr possible-events)))))
      (if intercept
	  (setq this-command 'eshell-self-insert-command)))))

(declare-function find-tag-interactive "etags" (prompt &optional no-default))

(defun eshell-find-tag (&optional tagname next-p regexp-p)
  "A special version of `find-tag' that ignores whether the text is read-only."
  (declare (obsolete xref-find-definition "27.1"))
  (interactive)
  (require 'etags)
  (let ((inhibit-read-only t)
	(no-default (eobp))
	(find-tag-default-function 'ignore))
    (setq tagname (car (find-tag-interactive "Find tag" no-default)))
    (with-suppressed-warnings ((obsolete find-tag))
      (find-tag tagname next-p regexp-p))))

(defun eshell-move-argument (limit func property arg)
  "Move forward ARG arguments."
  (catch 'eshell-incomplete
    (eshell-parse-arguments (save-excursion (beginning-of-line) (point))
			    (line-end-position)))
  (let ((pos (save-excursion
	       (funcall func 1)
	       (while (and (> arg 0) (/= (point) limit))
		 (if (get-text-property (point) property)
		     (setq arg (1- arg)))
		 (if (> arg 0)
		     (funcall func 1)))
	       (point))))
    (goto-char pos)
    (if (and (eq func 'forward-char)
	     (= (1+ pos) limit))
	(forward-char 1))))

(defun eshell-forward-argument (&optional arg)
  "Move forward ARG arguments."
  (interactive "p")
  (eshell-move-argument (point-max) 'forward-char 'arg-end arg))

(defun eshell-backward-argument (&optional arg)
  "Move backward ARG arguments."
  (interactive "p")
  (eshell-move-argument (point-min) 'backward-char 'arg-begin arg))

(defun eshell-repeat-argument (&optional arg)
  (interactive "p")
  (let ((begin (save-excursion
		 (eshell-backward-argument arg)
		 (point))))
    (kill-ring-save begin (point))
    (yank)))

(define-obsolete-function-alias 'eshell-bol #'beginning-of-line "30.1")

(defsubst eshell-push-command-mark ()
  "Push a mark at the end of the last input text."
  (push-mark (1- eshell-last-input-end) t))

(custom-add-option 'eshell-pre-command-hook #'eshell-push-command-mark)

(defsubst eshell-goto-input-start ()
  "Goto the start of the last command input.
Putting this function on `eshell-pre-command-hook' will mimic Plan 9's
9term behavior."
  (goto-char eshell-last-input-start))

(custom-add-option 'eshell-pre-command-hook #'eshell-goto-input-start)

(defun eshell-interactive-print (string)
  "Print STRING to the eshell display buffer."
  (when string
    (eshell-interactive-output-filter nil string)))

(defsubst eshell-begin-on-new-line ()
  "Output a newline if not at beginning of line."
  (save-excursion
    (goto-char eshell-last-output-end)
    (or (bolp)
	(eshell-interactive-print "\n"))))

(defsubst eshell-reset (&optional no-hooks)
  "Output a prompt on a new line, aborting any current input.
If NO-HOOKS is non-nil, then `eshell-post-command-hook' won't be run."
  (goto-char (point-max))
  (setq eshell-last-input-start (point-marker)
	eshell-last-input-end (point-marker)
	eshell-last-output-start (point-marker)
	eshell-last-output-block-begin (point)
	eshell-last-output-end (point-marker))
  (eshell-begin-on-new-line)
  (unless no-hooks
    (run-hooks 'eshell-post-command-hook)
    (goto-char (point-max))))

(defun eshell-parse-command-input (beg end &optional args)
  "Parse the command input from BEG to END.
The difference is that `eshell-parse-command' expects a complete
command string (and will error if it doesn't get one), whereas this
function will inform the caller whether more input is required.

If nil is returned, more input is necessary (probably because a
multi-line input string wasn't terminated properly).  Otherwise, it
will return the parsed command."
  (let (delim command)
    (if (setq delim
	      (catch 'eshell-incomplete
		(ignore
		 (setq command (eshell-parse-command (cons beg end)
						     args t)))))
	(ignore
         (message "Expecting completion of delimiter %s ..."
		  (if (listp delim)
		      (car delim)
		    delim)))
      command)))

(defun eshell-update-markers (pmark)
  "Update the input and output markers relative to point and PMARK."
  (set-marker eshell-last-input-start pmark)
  (set-marker eshell-last-input-end (point))
  (set-marker eshell-last-output-end (point)))

(defun eshell-queue-input (&optional use-region)
  "Queue the current input text for execution by Eshell.
Particularly, don't send the text to the current process, even if it's
waiting for input."
  (interactive "P")
  (eshell-send-input use-region t))

(defun eshell-send-input (&optional use-region queue-p no-newline)
  "Send the input received to Eshell for parsing and processing.
After `eshell-last-output-end', sends all text from that marker to
point as input.  Before that marker, calls `eshell-get-old-input' to
retrieve old input, copies it to the end of the buffer, and sends it.

If USE-REGION is non-nil, the current region (between point and mark)
will be used as input.

If QUEUE-P is non-nil, input will be queued until the next prompt,
rather than sent to the currently active process.  If no process, the
input is processed immediately.

If NO-NEWLINE is non-nil, the input is sent without an implied final
newline."
  (interactive "P")
  ;; Note that the input string does not include its terminal newline.
  (let* ((proc-running-p (eshell-head-process))
         (send-to-process-p (and proc-running-p (not queue-p))))
    (unless (and send-to-process-p
		 (not (eq (process-status
			   (eshell-head-process))
                          'run)))
      (if (or send-to-process-p
	      (>= (point) eshell-last-output-end))
	  (goto-char (point-max))
	(let ((copy (eshell-get-old-input use-region)))
	  (goto-char eshell-last-output-end)
	  (insert-and-inherit copy)))
      (unless (or no-newline
		  (and eshell-send-direct-to-subprocesses
		       send-to-process-p))
	(insert-before-markers-and-inherit ?\n))
      ;; Delete and reinsert input.  This seems like a no-op, except
      ;; for the resulting entries in the undo list: undoing this
      ;; insertion will delete the region, moving the process mark
      ;; back to its original position.
      (let ((text (buffer-substring eshell-last-output-end (point)))
            (inhibit-read-only t))
        (delete-region eshell-last-output-end (point))
        (insert text))
      (if send-to-process-p
	  (progn
	    (eshell-update-markers eshell-last-output-end)
	    (if (or eshell-send-direct-to-subprocesses
		    (= eshell-last-input-start eshell-last-input-end))
		(unless no-newline
		  (process-send-string (eshell-head-process) "\n"))
	      (process-send-region (eshell-head-process)
				   eshell-last-input-start
				   eshell-last-input-end)))
	(if (= eshell-last-output-end (point))
	    (run-hooks 'eshell-post-command-hook)
	  (let (input)
	    (eshell-condition-case err
		(progn
		  (setq input (buffer-substring-no-properties
			       eshell-last-output-end (1- (point))))
		  (run-hook-with-args 'eshell-expand-input-functions
				      eshell-last-output-end (1- (point)))
		  (let ((cmd (eshell-parse-command-input
			      eshell-last-output-end (1- (point)))))
		    (when cmd
		      (eshell-update-markers eshell-last-output-end)
		      (setq input (buffer-substring-no-properties
				   eshell-last-input-start
				   (1- eshell-last-input-end)))
		      (run-hooks 'eshell-input-filter-functions)
		      (and (catch 'eshell-terminal
			     (ignore
			      (if (and (not proc-running-p)
                                       (eshell-invoke-directly-p cmd))
				  (eval cmd)
				(eshell-eval-command cmd input))))
			   (eshell-life-is-too-much)))))
	      (error
	       (eshell-reset t)
	       (eshell-interactive-print
		(concat (error-message-string err) "\n"))
               (run-hooks 'eshell-post-command-hook)
	       (insert-and-inherit input)))))))))

(defun eshell-send-eof-to-process ()
  "Send EOF to the currently-running \"head\" process."
  (interactive)
  (eshell-send-input nil nil t)
  (when (eshell-head-process)
    (process-send-eof (eshell-head-process))))

(defsubst eshell-kill-new ()
  "Add the last input text to the kill ring."
  (kill-ring-save eshell-last-input-start eshell-last-input-end))

(custom-add-option 'eshell-input-filter-functions 'eshell-kill-new)

(defun eshell-interactive-filter (buffer string)
  "Send STRING to the interactive display, using BUFFER.
This is done after all necessary filtering has been done."
  (unless buffer
    (setq buffer (current-buffer)))
  (when (and string (buffer-live-p buffer))
    (with-current-buffer buffer
      (let ((functions eshell-preoutput-filter-functions))
        (while (and functions string)
          (setq string (funcall (car functions) string))
          (setq functions (cdr functions))))
      (when string
        (let (opoint obeg oend)
          (setq opoint (point))
          (setq obeg (point-min))
          (setq oend (point-max))
          (let ((buffer-read-only nil)
                (nchars (length string))
                (ostart nil))
            (widen)
            (goto-char eshell-last-output-end)
            (setq ostart (point))
            (if (<= (point) opoint)
                (setq opoint (+ opoint nchars)))
            (if (< (point) obeg)
                (setq obeg (+ obeg nchars)))
            (if (<= (point) oend)
                (setq oend (+ oend nchars)))
            ;; Let the ansi-color overlay hooks run.
            (let ((inhibit-modification-hooks nil))
              (insert string))
            (if (= (window-start) (point))
                (set-window-start (selected-window)
                                  (- (point) nchars)))
            (set-marker eshell-last-output-start ostart)
            (set-marker eshell-last-output-end (point))
            (force-mode-line-update))
          (narrow-to-region obeg oend)
          (goto-char opoint)
          (eshell-run-output-filters))))))

(defun eshell-interactive-output-filter (buffer string)
  "Send STRING to the interactive display as command output, using BUFFER.
This is like `eshell-interactive-filter', but marks the inserted string
as command output (see `eshell--mark-as-output')."
  (let ((eshell-output-filter-functions
         (cons (lambda ()
                 (eshell--mark-as-output eshell-last-output-start
                                         eshell-last-output-end))
               eshell-output-filter-functions)))
    (eshell-interactive-filter buffer string)))

(defun eshell-run-output-filters ()
  "Run the `eshell-output-filter-functions' on the current output."
  (save-current-buffer
    (run-hooks 'eshell-output-filter-functions))
  (setq eshell-last-output-block-begin
	(marker-position eshell-last-output-end)))

;;; jww (1999-10-23): this needs testing
(defun eshell-preinput-scroll-to-bottom ()
  "Go to the end of buffer in all windows showing it.
Movement occurs if point in the selected window is not after the
process mark, and `this-command' is an insertion command.  Insertion
commands recognized are `self-insert-command', `yank', and
`hilit-yank'.  Depends on the value of
`eshell-scroll-to-bottom-on-input'.

This function should be a pre-command hook."
  (if (memq this-command '(self-insert-command yank hilit-yank))
      (let* ((selected (selected-window))
	     (current (current-buffer))
	     (scroll eshell-scroll-to-bottom-on-input))
	(if (< (point) eshell-last-output-end)
	    (if (eq scroll 'this)
		(goto-char (point-max))
	      (walk-windows
               (lambda (window)
                 (when (and (eq (window-buffer window) current)
                            (or (eq scroll t) (eq scroll 'all)))
                   (select-window window)
                   (goto-char (point-max))
                   (select-window selected)))
	       nil t))))))

;;; jww (1999-10-23): this needs testing
(defun eshell-postoutput-scroll-to-bottom ()
  "Go to the end of buffer in all windows showing it.
Does not scroll if the current line is the last line in the buffer.
Depends on the value of `eshell-scroll-to-bottom-on-output' and
`eshell-scroll-show-maximum-output'.

This function should be in the list `eshell-output-filter-functions'."
  (let* ((selected (selected-window))
	 (current (current-buffer))
	 (scroll eshell-scroll-to-bottom-on-output))
    (unwind-protect
        (dolist (window (get-buffer-window-list current nil t))
          (with-selected-window window
            (when (and (< (point) eshell-last-output-end)
                       (or (eq scroll t) (eq scroll 'all)
                           ;; Maybe user wants point to jump to end.
                           (and (eq scroll 'this)
                                (eq selected window))
                           (and (eq scroll 'others)
                                (not (eq selected window)))
                           ;; If point was at the end, keep it at end.
                           (>= (point) eshell-last-output-start)))
              (goto-char eshell-last-output-end))
            ;; Optionally scroll so that the text ends at the bottom of
            ;; the window.
            (when (and eshell-scroll-show-maximum-output
                       (>= (point) eshell-last-output-end))
              (save-excursion
                (goto-char (point-max))
                (recenter -1)))))
      (set-buffer current))))

(defun eshell-beginning-of-input ()
  "Return the location of the start of the previous input."
  eshell-last-input-start)

(defun eshell-beginning-of-output ()
  "Return the location of the end of the previous output block."
  eshell-last-input-end)

(defun eshell-end-of-output ()
  "Return the location of the end of the previous output block."
  (if (eshell-using-module 'eshell-prompt)
      eshell-last-output-start
    eshell-last-output-end))

(defun eshell-delete-output (&optional kill)
  "Delete all output from interpreter since last input.
If KILL is non-nil (interactively, the prefix), save the killed text in
the kill ring.

This command does not delete the prompt."
  (interactive "P")
  (save-excursion
    (goto-char (eshell-beginning-of-output))
    (insert "*** output flushed ***\n")
    (when kill
      (copy-region-as-kill (point) (eshell-end-of-output)))
    (delete-region (point) (eshell-end-of-output))))

(define-obsolete-function-alias 'eshell-kill-output
  #'eshell-delete-output "30.1")

(defun eshell-show-output (&optional arg)
  "Display start of this batch of interpreter output at top of window.
Sets mark to the value of point when this command is run.
With a prefix argument, narrows region to last command output."
  (interactive "P")
  (goto-char (eshell-beginning-of-output))
  (set-window-start (selected-window)
		    (save-excursion
		      (goto-char (eshell-beginning-of-input))
		      (line-beginning-position)))
  (if arg
      (narrow-to-region (eshell-beginning-of-output)
			(eshell-end-of-output)))
  (eshell-end-of-output))

(defun eshell-mark-output (&optional arg)
  "Display start of this batch of interpreter output at top of window.
Sets mark to the value of point when this command is run.
With a prefix argument, narrows region to last command output."
  (interactive "P")
  (push-mark (eshell-show-output arg)))

(defun eshell-kill-input ()
  "Kill all text from last stuff output by interpreter to point."
  (interactive)
  (if (> (point) eshell-last-output-end)
      (kill-region eshell-last-output-end (point))
    (let ((here (point)))
      (beginning-of-line)
      (kill-region (point) here))))

(defun eshell-show-maximum-output (&optional interactive)
  "Put the end of the buffer at the bottom of the window.
When run interactively, widen the buffer first."
  (interactive "p")
  (if interactive
      (widen))
  (goto-char (point-max))
  (recenter -1))

(defun eshell-clear (&optional clear-scrollback)
  "Scroll contents of the Eshell window out of sight, leaving a blank window.
If CLEAR-SCROLLBACK is non-nil (interactively, with the prefix
argument), clear the scrollback contents.

Otherwise, the behavior depends on `eshell-scroll-show-maximum-output'.
If non-nil, fill newlines before the current prompt so that the prompt
is the last line in the window; if nil, just scroll the window so that
the prompt is the first line in the window."
  (interactive "P")
  (cond
   (clear-scrollback
    (let ((inhibit-read-only t))
      (widen)
      (delete-region (point-min) (eshell-end-of-output))))
   (eshell-scroll-show-maximum-output
    (save-excursion
      (goto-char (eshell-end-of-output))
      (let ((inhibit-read-only t))
        (insert-and-inherit (make-string (window-size) ?\n))))
    (when (< (point) eshell-last-output-end)
      (goto-char eshell-last-output-end)))
  (t
   (when (< (point) eshell-last-output-end)
     (goto-char eshell-last-output-end))
   (set-window-start nil (eshell-end-of-output)))))

(defun eshell/clear (&optional clear-scrollback)
  "Scroll contents of the Eshell window out of sight, leaving a blank window.
If CLEAR-SCROLLBACK is non-nil, clear the scrollback contents.

Otherwise, the behavior depends on `eshell-scroll-show-maximum-output'.
If non-nil, fill newlines before the current prompt so that the prompt
is the last line in the window; if nil, just scroll the window so that
the prompt is the first line in the window.

This command is for use as an Eshell command (entered at the prompt);
for clearing the Eshell buffer from elsewhere (e.g. via
\\[execute-extended-command]), use `eshell-clear'."
  (interactive)
  (cond
   ((null eshell-current-handles)
    (eshell-clear clear-scrollback))
   (clear-scrollback
    (let ((inhibit-read-only t))
      (erase-buffer)))
   (eshell-scroll-show-maximum-output
    (let ((eshell-input-filter-functions nil))
      (ignore (eshell-interactive-print (make-string (window-size) ?\n)))))
   (t
    (recenter 0))))

(defun eshell/clear-scrollback ()
  "Clear the scrollback content of the Eshell window."
  (eshell/clear t))

(defun eshell-get-old-input (&optional use-current-region)
  "Return the command input on the current line.
If USE-CURRENT-REGION is non-nil, return the current region."
  (if use-current-region
      (buffer-substring (min (point) (mark))
			(max (point) (mark)))
    (save-excursion
      (let ((inhibit-field-text-motion t))
        (end-of-line))
      (let ((inhibit-field-text-motion)
            (end (point)))
        (beginning-of-line)
        (buffer-substring-no-properties (point) end)))))

(defun eshell-copy-old-input ()
  "Insert after prompt old input at point as new input to be edited."
  (interactive)
  (let ((input (eshell-get-old-input)))
    (goto-char eshell-last-output-end)
    (insert-and-inherit input)))

(defun eshell/exit ()
  "Leave or kill the Eshell buffer, depending on `eshell-kill-on-exit'."
  (throw 'eshell-terminal t))

(defun eshell-life-is-too-much ()
  "Kill the current buffer (or bury it).  Good-bye Eshell."
  (interactive)
  (if (not eshell-kill-on-exit)
      (bury-buffer)
    (kill-buffer (current-buffer))))

(defun eshell-truncate-buffer ()
  "Truncate the buffer to `eshell-buffer-maximum-lines'.
This function could be on `eshell-output-filter-functions' or bound to
a key."
  (interactive)
  (save-excursion
    (goto-char eshell-last-output-end)
    (let ((lines (count-lines 1 (point)))
	  (inhibit-read-only t))
      (forward-line (- eshell-buffer-maximum-lines))
      (beginning-of-line)
      (let ((pos (point)))
	(if (bobp)
	    (if (called-interactively-p 'interactive)
		(message "Buffer too short to truncate"))
	  (delete-region (point-min) (point))
	  (if (called-interactively-p 'interactive)
	      (message "Truncated buffer from %d to %d lines (%.1fk freed)"
		       lines eshell-buffer-maximum-lines
		       (/ pos 1024.0))))))))

(custom-add-option 'eshell-output-filter-functions
		   'eshell-truncate-buffer)

(defun eshell-send-invisible ()
  "Read a string without echoing.
Then send it to the process running in the current buffer."
  (interactive) ; Don't pass str as argument, to avoid snooping via C-x ESC ESC
  (let ((str (read-passwd
	      (format "%s Password: "
		      (process-name (eshell-head-process))))))
    (if (stringp str)
	(process-send-string (eshell-head-process)
			     (concat str "\n"))
      (message "Warning: text will be echoed"))))

(defun eshell-watch-for-password-prompt ()
  "Prompt in the minibuffer for password and send without echoing.
This function uses `eshell-send-invisible' to read and send a password to the
buffer's process if STRING contains a password prompt defined by
`eshell-password-prompt-regexp'.

This function could be in the list `eshell-output-filter-functions'."
  (when (eshell-head-process)
    (save-excursion
      (goto-char (max eshell-last-output-block-begin
                      (- eshell-last-output-end
                         eshell-password-prompt-max-length)))
      (when (let ((case-fold-search t))
              (re-search-forward eshell-password-prompt-regexp
                                 eshell-last-output-end t))
        ;; Use `run-at-time' in order not to pause execution of the
        ;; process filter with a minibuffer.
        (run-at-time
         0 nil
         (lambda (current-buf)
           (with-current-buffer current-buf
             (eshell-send-invisible)))
         (current-buffer))))))

(custom-add-option 'eshell-output-filter-functions
		   'eshell-watch-for-password-prompt)

(defun eshell-handle-control-codes ()
  "Act properly when certain control codes are seen."
  (save-excursion
    (goto-char eshell-last-output-block-begin)
    (unless (eolp)
      (beginning-of-line))
    (while (re-search-forward (rx (any ?\r ?\a ?\C-h))
                              eshell-last-output-end t)
      (let ((char (char-before)))
        (cond
         ((eq char ?\r)
          (if (< (point) eshell-last-output-end)
              (if (memq (char-after (point)) '(?\n ?\r))
                  (delete-char -1)
                (let ((end (point)))
                  (beginning-of-line)
                  (delete-region (point) end)))
            (add-text-properties (1- (point)) (point)
                                 '(invisible t))))
         ((eq char ?\a)
          (delete-char -1)
          (beep))
         ((eq char ?\C-h)
          (delete-region (- (point) 2) (point))))))))

(custom-add-option 'eshell-output-filter-functions
		   'eshell-handle-control-codes)

(autoload 'ansi-color-apply-on-region "ansi-color")
(defvar ansi-color-apply-face-function)
(declare-function ansi-color-apply-text-property-face "ansi-color"
                  (BEG END FACE))

(defun eshell-handle-ansi-color ()
  "Handle ANSI color codes."
  (require 'ansi-color)
  (let ((ansi-color-apply-face-function #'ansi-color-apply-text-property-face))
    (ansi-color-apply-on-region eshell-last-output-start
                                eshell-last-output-end)))

(custom-add-option 'eshell-output-filter-functions
		   'eshell-handle-ansi-color)

;;; Bookmark support:

(declare-function bookmark-prop-get "bookmark" (bookmark prop))

(defun eshell-bookmark-name ()
  (format "eshell-%s"
          (file-name-nondirectory
           (directory-file-name
            (file-name-directory default-directory)))))

(defun eshell-bookmark-make-record ()
  "Create a bookmark for the current Eshell buffer."
  `(,(eshell-bookmark-name)
    (location . ,default-directory)
    (handler . eshell-bookmark-jump)))

;;;###autoload
(defun eshell-bookmark-jump (bookmark)
  "Default bookmark handler for Eshell buffers."
  (let ((default-directory (bookmark-prop-get bookmark 'location)))
    (eshell)))

(put 'eshell-bookmark-jump 'bookmark-handler-type "Eshell")

(provide 'esh-mode)
;;; esh-mode.el ends here
