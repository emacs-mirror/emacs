;;; resume.el --- process command line args from within a suspended Emacs job

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Author: Joe Wells <jbw@bucsf.bu.edu>
;; Adapted-By: ESR
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

;; Theory: the first time you start Emacs, command line arguments are
;; handled normally.  Then, you suspend your emacs job.  When you want to edit
;; something else, you type "emacs filename" as usual, but instead of
;; starting a new emacs job, the old job is resumed instead, and the command
;; line arguments are placed in a file where the old emacs job looks for
;; them.

;; Stephan Gildea suggested bug fix (gildea@bbn.com).
;; Ideas from Michael DeCorte and other people.

;; For csh users, insert the following alias in your .cshrc file
;; (after removing the leading double semicolons, of course):
;;
;;# The following line could be just EMACS_CMD=emacs, but this depends on
;;# your site.
;;if (! $?EMACS_CMD) set EMACS_CMD=emacs
;;set JOBS_FILE=/tmp/jobs.$USER.$$
;;set ARGS_FILE=~/.emacs_args
;;set STOP_PATT='^\[[0-9]*\] *[ +-] Stopped ............ '
;;set SUNVIEW_CMD='emacstool -nw -f emacstool-init -f server-start'
;;set X_CMD=\'\''$EMACS_CMD -i -f server-start'
;;alias emacs \
;;' \\
;;   jobs >! "$JOBS_FILE" \\
;;   && grep "$STOP_PATT$EMACS_CMD" "$JOBS_FILE" >& /dev/null \\
;;   && echo `pwd` \!* >! "$ARGS_FILE" && ""fg %$EMACS_CMD \\
;;|| if (! -e ~/.emacs_server || -f ~/.emacs_server) set status=1 \\
;;   && emacsclient \!* \\
;;|| @ status=1 - $?DISPLAY && eval "$X_CMD -i \!* &" \\
;;|| @ status=1 - $?WINDOW_PARENT && eval "$SUNVIEW_CMD \!* &" \\
;;|| ""$EMACS_CMD -nw \!* \\
;;'
;;
;; The alias works as follows:
;; 1. If there is a suspended Emacs job that is a child of the
;; current shell, place its arguments in the ~/.emacs_args file and
;; resume it.
;; 2. Else if the ~/.emacs_server socket has been created, presume an
;; Emacs server is running and attempt to connect to it.  If no Emacs
;; server is listening on the socket, this will fail.
;; 3. Else if the DISPLAY environment variable is set, presume we are
;; running under X Windows and start a new GNU Emacs process in the
;; background as an X client.
;; 4. Else if the WINDOW_PARENT environment variable is set, presume we
;; are running under SunView and start an emacstool process in the
;; background.
;; 5. Else start a regular Emacs process.
;;
;; Notes:
;; The output of the "jobs" command is not piped directly into "grep"
;; because that would run the "jobs" command in a subshell.
;; Before resuming a suspended emacs, the current directory and all
;; command line arguments are placed in a file name ~/.emacs_args.
;; The "-nw" switch to Emacs means no windowing system.

;; Insert this in your .emacs file:
;;(add-hook 'suspend-hook 'resume-suspend-hook)

;; Finally, put the rest in a file named "resume.el" in a lisp library
;; directory.

;;; Code:

(defvar resume-emacs-args-file (expand-file-name "~/.emacs_args")
  "*This file is where arguments are placed for a suspended emacs job.")

(defvar resume-emacs-args-buffer " *Command Line Args*"
  "Buffer that is used by resume-process-args.")

(defun resume-process-args ()
  "Handler for command line args given when Emacs is resumed."
  (let ((start-buffer (current-buffer))
	(args-buffer (get-buffer-create resume-emacs-args-buffer))
	length args
	(command-line-default-directory default-directory))
    (unwind-protect
	(progn
	  (set-buffer args-buffer)
	  (erase-buffer)
	  ;; get the contents of resume-emacs-args-file
	  (condition-case ()
	      (let ((result (insert-file-contents resume-emacs-args-file)))
		(setq length (car (cdr result))))
	    ;; the file doesn't exist, ergo no arguments
	    (file-error
	      (erase-buffer)
	      (setq length 0)))
	  (if (<= length 0)
	      (setq args nil)
	    ;; get the arguments from the buffer
	    (goto-char (point-min))
	    (while (not (eobp))
	      (skip-chars-forward " \t\n")
	      (let ((begin (point)))
		(skip-chars-forward "^ \t\n")
		(setq args (cons (buffer-substring begin (point)) args)))
	      (skip-chars-forward " \t\n"))
	    ;; arguments are now in reverse order
	    (setq args (nreverse args))
	    ;; make sure they're not read again
	    (erase-buffer))		
	  (resume-write-buffer-to-file (current-buffer) resume-emacs-args-file)
	  ;; if nothing was in buffer, args will be null
	  (or (null args)
	      (setq command-line-default-directory
		    (file-name-as-directory (car args))
		    args (cdr args)))
	  ;; actually process the arguments
	  (command-line-1 args))
      ;; If the command line args don't result in a find-file, the
      ;; buffer will be left in args-buffer.  So we change back to the
      ;; original buffer.  The reason I don't just use
      ;; (let ((default-directory foo))
      ;;    (command-line-1 args))
      ;; in the context of the original buffer is because let does not
      ;; work properly with buffer-local variables.
      (if (eq (current-buffer) args-buffer)
	  (set-buffer start-buffer)))))

;;;###autoload
(defun resume-suspend-hook ()
  "Clear out the file used for transmitting args when Emacs resumes."
  (save-excursion
    (set-buffer (get-buffer-create resume-emacs-args-buffer))
    (erase-buffer)
    (resume-write-buffer-to-file (current-buffer) resume-emacs-args-file)))

(defun resume-write-buffer-to-file (buffer file)
  "Writes the contents of BUFFER into FILE, if permissions allow."
  (if (not (file-writable-p file))
      (error "No permission to write file %s" file))
  (save-excursion
    (set-buffer buffer)
    (clear-visited-file-modtime)
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) file nil 'quiet))
    (set-buffer-modified-p nil)))

(provide 'resume)

;;; resume.el ends here
