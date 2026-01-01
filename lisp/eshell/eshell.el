;;; eshell.el --- the Emacs command shell  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2026 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Version: 2.4.2
;; Keywords: processes

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

;;;_* What does Eshell offer you?
;;
;; Despite the sheer fact that running an Emacs shell can be fun, here
;; are a few of the unique features offered by Eshell:
;;
;; @ Integration with the Emacs Lisp programming environment
;;
;; @ A high degree of configurability
;;
;; @ The ability to have the same shell on every system Emacs has been
;;   ported to.  Since Eshell imposes no external requirements, and
;;   relies upon only the Lisp functions exposed by Emacs, it is quite
;;   operating system independent.  Several of the common UNIX
;;   commands, such as ls, mv, rm, ln, etc., have been implemented in
;;   Lisp in order to provide a more consistent work environment.
;;
;; For those who might be using an older version of Eshell, version
;; 2.1 represents an entirely new, module-based architecture.  It
;; supports most of the features offered by modern shells.  Here is a
;; brief list of some of its more visible features:
;;
;; @ Command argument completion (tcsh, zsh)
;; @ Input history management (bash)
;; @ Intelligent output scrolling
;; @ Pseudo-devices (such as "/dev/clip" for copying to the clipboard)
;; @ Extended globbing (zsh)
;; @ Argument and globbing predication (zsh)
;; @ I/O redirection to buffers, files, symbols, processes, etc.
;; @ Many niceties otherwise seen only in 4DOS
;; @ Alias functions, both Lisp and Eshell-syntax
;; @ Piping, sequenced commands, background jobs, etc...
;;
;;;_* How to begin
;;
;; To start using Eshell, simply type `M-x eshell'.
;;
;;;_* Philosophy
;;
;; A shell is a layer which metaphorically surrounds the kernel, or
;; heart of an operating system.  This kernel can be seen as an engine
;; of pure functionality, waiting to serve, while the user programs
;; take advantage of that functionality to accomplish their purpose.
;;
;; The shell's role is to make that functionality accessible to the
;; user in an unformed state.  Very roughly, it associates kernel
;; functionality with textual commands, allowing the user to interact
;; with the operating system via linguistic constructs.  Process
;; invocation is perhaps the most significant form this takes, using
;; the kernel's `fork' and `exec' functions.
;;
;; Other programs also interact with the functionality of the kernel,
;; but these user applications typically offer a specific range of
;; functionality, and thus are not classed as "shells" proper.
;; (What they lose in quiddity, they gain in rigidity).
;;
;; Emacs is also a user application, but it does make the
;; functionality of the kernel accessible through an interpreted
;; language -- namely, Lisp.  For that reason, there is little
;; preventing Emacs from serving the same role as a modern shell.  It
;; too can manipulate the kernel in an unpredetermined way to cause
;; system changes.  All it's missing is the shell-ish linguistic
;; model.
;;
;; Enter Eshell.  Eshell translates "shell-like" syntax into Lisp
;; in order to exercise the kernel in the same manner as typical
;; system shells.  There is a fundamental difference here, however,
;; although it may seem subtle at first...
;;
;; Shells like csh and Bourne shell were written several decades ago,
;; in different times, under more restrictive circumstances.  This
;; confined perspective shows itself in the paradigm used by nearly
;; all command-line shells since.  They are linear in conception, byte
;; stream-based, sequential, and confined to movement within a single
;; host machine.
;;
;; Emacs, on the other hand, is more than just a limited translator
;; that can invoke subprocesses and redirect file handles.  It also
;; manages character buffers, windowing frames, network connections,
;; registers, bookmarks, processes, etc.  In other words, it's a very
;; multi-dimensional environment, within which eshell emulates a highly
;; linear methodology.
;;
;; Taking a moment, let's look at how this could affect the future of
;; a shell allowed to develop in such a wider field of play:
;;
;; @ There is no reason why directory movement should be linear, and
;;   confined to a single file-system.  Emacs, through w3 and ange-ftp,
;;   has access to the entire Web.  Why not allow a user to cd to
;;   multiple directories simultaneously, for example?  It might make
;;   some tasks easier, such as diff'ing files separated by very long
;;   pathnames.
;;
;; @ Data sources are available from anywhere Emacs can derive
;;   information from: not just from files or the output of other
;;   processes.
;;
;; @ Multiple shell invocations all share the same environment -- even
;;   the same process list!  It would be possible to have "process
;;   views", so that one buffer is watching standard output, another
;;   standard error, and another the result of standard output grep'd
;;   through a regular expression...
;;
;; @ It is not necessary to "leave" the shell, losing all input and
;;   output history, environment variables, directory stack, etc.
;;   Emacs could save the contents of your eshell environment, and
;;   restore all of it (or at least as much as possible) each time you
;;   restart.  This could occur automatically, without requiring
;;   complex initialization scripts.
;;
;; @ Typos occur all of the time; many of them are repeats of common
;;   errors, such as 'dri' for `dir'.  Since executing non-existent
;;   programs is rarely the intention of the user, eshell could prompt
;;   for the replacement string, and then record that in a database of
;;   known misspellings.  (Note: The typo at the beginning of this
;;   paragraph wasn't discovered until two months after I wrote the
;;   text; it was not intentional).
;;
;; @ Emacs's register and bookmarking facilities can be used for
;;   remembering where you've been, and what you've seen -- to varying
;;   levels of persistence.  They could perhaps even be tied to
;;   specific "moments" during eshell execution, which would include
;;   the environment at that time, as well as other variables.
;;   Although this would require functionality orthogonal to Emacs's
;;   own bookmarking facilities, the interface used could be made to
;;   operate very similarly.
;;
;; This presents a brief idea of what the fuller dimensionality of an
;; Emacs shell could offer.  It's not just the language of a shell
;; that determines how it's used, but also the Weltanschauung
;; underlying its design -- and which is felt behind even the smallest
;; feature.  I would hope the freedom provided by using Emacs as a
;; parent environment will invite rich ideas from others.  It
;; certainly feels as though all I've done so far is to tie down the
;; horse, so to speak, so that he will run at a man's pace.
;;
;;;_* Influences
;;
;; The author of Eshell has been a long-time user of the following
;; shells, all of which contributed to Eshell's design:
;;
;; @ rc
;; @ bash
;; @ zsh
;; @ sh
;; @ 4nt
;; @ csh

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'esh-util)
(require 'esh-module)                   ;For eshell-using-module
(require 'esh-proc)                     ;For eshell-wait-for-processes
(require 'esh-io)                       ;For eshell-last-command-status
(require 'esh-cmd)

(defgroup eshell nil
  "Command shell implemented entirely in Emacs Lisp.
It invokes no external processes beyond those requested by the
user, and is intended to be a functional replacement for command
shells such as bash, zsh, rc, 4dos."
  :link '(info-link "(eshell)Top")
  :version "21.1"
  :group 'applications)

;;;_* User Options
;;
;; The following user options modify the behavior of Eshell overall.
(defvar eshell-buffer-name)

(defcustom eshell-load-hook nil
  "A hook run once Eshell has been loaded."
  :type 'hook
  :group 'eshell)

(defcustom eshell-unload-hook nil
  "A hook run when Eshell is unloaded from memory."
  :type 'hook
  :group 'eshell)
(make-obsolete-variable 'eshell-unload-hook nil "30.1")

(defcustom eshell-buffer-name "*eshell*"
  "The basename used for Eshell buffers.
This is the default name used when running `eshell'.

With a numeric prefix argument to `eshell', the buffer name will
be the value of this variable followed by the number.  For
example, with the numeric prefix argument 2, the buffer would be
named \"*eshell*<2>\"."
  :type 'string
  :group 'eshell)

(defcustom eshell-command-async-buffer 'confirm-new-buffer
  "What to do when the output buffer is used by another shell command.
This option specifies how to resolve the conflict where a new command
wants to direct its output to the buffer whose name is stored
in `eshell-command-buffer-name-async', but that buffer is already
taken by another running shell command.

The value `confirm-kill-process' is used to ask for confirmation before
killing the already running process and running a new process in the
same buffer, `confirm-new-buffer' for confirmation before running the
command in a new buffer with a name other than the default buffer name,
`new-buffer' for doing the same without confirmation,
`confirm-rename-buffer' for confirmation before renaming the existing
output buffer and running a new command in the default buffer,
`rename-buffer' for doing the same without confirmation."
  :type '(choice (const :tag "Confirm killing of running command"
                        confirm-kill-process)
                 (const :tag "Confirm creation of a new buffer"
                        confirm-new-buffer)
                 (const :tag "Create a new buffer"
                        new-buffer)
                 (const :tag "Confirm renaming of existing buffer"
                        confirm-rename-buffer)
                 (const :tag "Rename the existing buffer"
                        rename-buffer))
  :group 'eshell
  :version "31.1")

;;;_* Running Eshell
;;
;; There are only three commands used to invoke Eshell.  The first two
;; are intended for interactive use, while the third is meant for
;; programmers.  They are:

;;;###autoload
(defun eshell (&optional arg)
  "Create an interactive Eshell buffer.
Start a new Eshell session, or switch to an already active
session.  Return the buffer selected (or created).

With a nonnumeric prefix arg, create a new session.

With a numeric prefix arg (as in `\\[universal-argument] 42 \\[eshell]'), switch
to the session with that number, or create it if it doesn't
already exist.

The buffer name used for Eshell sessions is determined by the
value of `eshell-buffer-name', which see.

Eshell is a shell-like command interpreter.  For more
information on Eshell, see Info node `(eshell)Top'."
  (interactive "P")
  (cl-assert eshell-buffer-name)
  (let ((buf (cond ((numberp arg)
		    (get-buffer-create (format "%s<%d>"
					       eshell-buffer-name
					       arg)))
		   (arg
		    (generate-new-buffer eshell-buffer-name))
		   (t
		    (get-buffer-create eshell-buffer-name)))))
    (cl-assert (and buf (buffer-live-p buf)))
    (pop-to-buffer buf (append display-buffer--same-window-action
                               '((category . comint))))
    (unless (derived-mode-p 'eshell-mode)
      (eshell-mode))
    buf))

(declare-function eshell-add-input-to-history "em-hist" (input))
(declare-function eshell--save-history "em-hist" ())

(defun eshell-command-mode-exit ()
  "Exit the `eshell-command-mode' minibuffer and save Eshell history."
  (interactive)
  (when (eshell-using-module 'eshell-hist)
    (eshell-add-input-to-history
     (buffer-substring (minibuffer-prompt-end) (point-max)))
    (eshell--save-history))
  (exit-minibuffer))

(define-minor-mode eshell-command-mode
  "Minor mode for `eshell-command' input.
\\{eshell-command-mode-map}"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [(control ?g)] #'abort-recursive-edit)
            (define-key map [(control ?m)] #'eshell-command-mode-exit)
            (define-key map [(control ?j)] #'eshell-command-mode-exit)
            (define-key map [(meta control ?m)] #'eshell-command-mode-exit)
            map))

(define-obsolete-function-alias 'eshell-return-exits-minibuffer
  #'eshell-command-mode "28.1")

(defvar eshell-non-interactive-p)       ; Defined in esh-mode.el.

(declare-function eshell-add-input-to-history "em-hist" (input))

(defun eshell-read-command (&optional prompt)
  "Read an Eshell command from the minibuffer, prompting with PROMPT."
  (let ((prompt (or prompt "Emacs shell command: "))
        (eshell-non-interactive-p t))
    ;; Enable `eshell-mode' only in this minibuffer.
    (minibuffer-with-setup-hook (lambda ()
                                  (eshell-mode)
                                  (eshell-command-mode +1))
      (read-from-minibuffer prompt))))

(defvar eshell-command-buffer-name-async "*Eshell Async Command Output*")
(defvar eshell-command-buffer-name-sync "*Eshell Command Output*")

;;;###autoload
(defun eshell-command (command &optional output-target error-target)
  "Execute the Eshell command string COMMAND.
If OUTPUT-TARGET is t (interactively, with the prefix argument), write
the command's standard output to the current buffer at point.  If nil,
write the output to a new output buffer.  For any other value, output to
that Eshell target (see `eshell-get-target').

ERROR-TARGET is similar to OUTPUT-TARGET, except that it controls where
to write standard error, and a nil value means to write standard error
to the same place as standard output.  (To suppress standard error, you
can write to the Eshell virtual target \"/dev/null\".)

When \"&\" is added at end of command, the command is async and its
output appears in a specific buffer.  You can customize
`eshell-command-async-buffer' to specify what to do when this output
buffer is already taken by another running shell command."
  (interactive (list (eshell-read-command)
                     (not (not current-prefix-arg))))
  (save-excursion
    (let ((stdout (cond ((eq output-target t) (current-buffer))
                        ((not output-target) t)
                        (t output-target)))
          (stderr (if (eq error-target t) (current-buffer) error-target))
          (buf (set-buffer (generate-new-buffer " *eshell cmd*")))
	  (eshell-non-interactive-p t))
      (eshell-mode)
      (let* ((proc (eshell-eval-command
                    `(eshell-with-handles (',stdout 'insert ',stderr 'insert)
                       (let ((eshell-current-subjob-p))
		         ,(eshell-parse-command command)))
                    command))
             (async (eq (car-safe proc) :eshell-background))
             (bufname (cond
                       ((not (eq stdout t)) nil)
                       (async eshell-command-buffer-name-async)
                       (t eshell-command-buffer-name-sync)))
             unique)
        (when bufname
          (when (buffer-live-p (get-buffer bufname))
            (cond
             ((with-current-buffer bufname
                (and (null eshell-foreground-command)
                     (null eshell-background-commands)))
              ;; The old buffer is done executing; kill it so we can
              ;; take its place.
              (kill-buffer bufname))
             ((eq eshell-command-async-buffer 'confirm-kill-process)
              (shell-command--same-buffer-confirm "Kill it")
              (with-current-buffer bufname
                ;; Stop all the processes in the old buffer (there may
                ;; be several).
                (eshell-round-robin-kill))
              (kill-buffer bufname))
             ((eq eshell-command-async-buffer 'confirm-new-buffer)
              (shell-command--same-buffer-confirm "Use a new buffer")
              (setq unique t))
             ((eq eshell-command-async-buffer 'new-buffer)
              (setq unique t))
             ((eq eshell-command-async-buffer 'confirm-rename-buffer)
              (shell-command--same-buffer-confirm "Rename it")
              (with-current-buffer bufname
                (rename-uniquely)))
             ((eq eshell-command-async-buffer 'rename-buffer)
              (with-current-buffer bufname
                (rename-uniquely)))))
          (rename-buffer bufname unique))
	;; things get a little coarse here, since the desire is to
	;; make the output as attractive as possible, with no
	;; extraneous newlines
        (unless async
	  (funcall #'eshell-wait-for-processes (cadr eshell-foreground-command))
	  (cl-assert (not eshell-foreground-command))
	  (goto-char (point-max))
	  (while (and (bolp) (not (bobp)))
	    (delete-char -1)))
	(cl-assert (and buf (buffer-live-p buf)))
        (unless bufname
          (let ((len (if async 2
		       (count-lines (point-min) (point-max)))))
	    (cond
	     ((= len 0)
	      (message "(There was no command output)")
	      (kill-buffer buf))
	     ((= len 1)
	      (message "%s" (buffer-string))
	      (kill-buffer buf))
	     (t
	      (save-selected-window
		(select-window (display-buffer buf))
		(goto-char (point-min))
		;; cause the output buffer to take up as little screen
		;; real-estate as possible, if temp buffer resizing is
		;; enabled
                (and (not async) temp-buffer-resize-mode
		     (resize-temp-buffer-window)))))))))))

;;;###autoload
(defun eshell-command-result (command &optional status-var)
  "Execute the given Eshell COMMAND, and return the result.
The result might be any Lisp object.
If STATUS-VAR is a symbol, it will be set to the exit status of the
command.  This is the only way to determine whether the value returned
corresponding to a successful execution."
  ;; a null command produces a null, successful result
  (if (not command)
      (ignore
       (if (and status-var (symbolp status-var))
	   (set status-var 0)))
    (with-temp-buffer
      (let ((eshell-non-interactive-p t))
	(eshell-mode)
        (eshell-debug-command-start command)
	(let ((result (eshell-do-eval
		       (list 'eshell-commands
			     (list 'eshell-command-to-value
				   (eshell-parse-command command)))
                       t)))
	  (cl-assert (eq (car result) 'quote))
	  (if (and status-var (symbolp status-var))
	      (set status-var eshell-last-command-status))
	  (cadr result))))))

(defun eshell-unload-function ()
  (eshell-unload-extension-modules)
  ;; Wait to unload core modules until after `eshell' has finished
  ;; unloading.  `eshell' depends on several of them, so they can't be
  ;; unloaded immediately.
  (run-at-time 0 nil #'eshell-unload-modules
               (reverse (eshell-subgroups 'eshell)) 'core)
  nil)

(run-hooks 'eshell-load-hook)

(provide 'eshell)
;;; eshell.el ends here
