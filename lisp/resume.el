;; process command line arguments from within a suspended emacs job
;; Copyright (C) 1988 Free Software Foundation, Inc.

;; This file is not yet part of GNU Emacs, but soon will be.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.
;;
;; by Joe Wells
;; jbw@bucsf.bu.edu
;; joew@uswat.uswest.com (maybe, ... the mailer there sucks)

;; Stephan Gildea suggested bug fix (gildea@bbn.com).
;; Ideas from Michael DeCorte and other people.

;; For csh users, insert the following alias in your .cshrc file
;; (after removing the leading double semicolons):
;;
;;# The following line could be just EMACS=emacs, but this depends on
;;# your site.
;;set EMACS=emacs
;;set EMACS_PATTERN="^\[[0-9]\]  . Stopped ............ $EMACS"
;;alias emacs \
;;' \\
;;   jobs >! /tmp/jobs$$ \\
;;   && grep "$EMACS_PATTERN" /tmp/jobs$$ >& /dev/null \\
;;   && echo `pwd` \!* >! ~/.emacs_args && eval "%$EMACS" \\
;;|| test -S ~/.emacs_server && emacsclient \!* \\
;;|| test "$?DISPLAY" = 1 && eval "\$EMACS -i \!* &" \\
;;|| test "$?WINDOW_PARENT" = 1 && eval "emacstool -f emacstool-init \!* &" \\
;;|| eval "\$EMACS -nw \!*"'
;;
;; The alias works as follows:
;; 1. If there is a suspended emacs jobs that is a child of the
;; current shell, place its arguments in the ~/.emacs_args file and
;; resume it.
;; 2. Else if the ~/.emacs_server socket has been created, presume an
;; emacs server is running and attempt to connect to it.  If no emacs
;; server is listening on the socket, this will fail.
;; 3. Else if the DISPLAY environment variable is set, presume we are
;; running under X Windows and start a new X Gnu Emacs process in the
;; background.
;; 4. Else if the WINDOW_PARENT environment variable is set, presume we
;; are running under Sunview and Suntools and start an emacstool
;; process in the background.
;; 5. Else start a regular emacs process.
;;
;; Notes:
;; "test -S" checks if a unix domain socket by that name exists.
;; The output of the "jobs" command is not piped directly into "grep"
;; because that would run the "jobs" command in a subshell.
;; Before resuming a suspended emacs, the current directory and all
;; command line arguments are placed in a file.
;; The command to run emacs is always preceded by a \ to prevent
;; possible alias loops.
;; The "-nw" switch in the last line is is undocumented, and it means
;; no windowing system.

(setq suspend-resume-hook 'resume-process-args)
(setq suspend-hook 'resume-preparation)

(defvar emacs-args-file "~/.emacs_args"
  "*This file is where arguments are placed for a suspended emacs job.")

(defun resume-preparation ()
  (condition-case ()
      (delete-file emacs-args-file)
    (error nil)))

(defun resume-process-args ()
  "This should be called from inside of suspend-resume-hook.
Grabs the contents of the file whose name is stored in
emacs-args-file, and processes these arguments like command line options."
  (let ((resume-start-buffer (current-buffer))
	(resume-args-buffer (get-buffer-create " *Command Line Args*"))
	resume-args)
    (unwind-protect
	(progn
	  (set-buffer resume-args-buffer)
	  (erase-buffer)
	  ;; Get the contents of emacs-args-file, then delete the file.
	  (condition-case ()
	      (progn
		(insert-file-contents emacs-args-file)
		(delete-file emacs-args-file))
	    ;; The file doesn't exist or we can't delete it, ergo no arguments.
	    ;; (If we can't delete it now, we probably couldn't delete it
	    ;; before suspending, and that implies it may be vestigial.)
	    (file-error (erase-buffer)))
	  ;; Get the arguments from the buffer.
	  (goto-char (point-min))
	  (while (progn (skip-chars-forward " \t\n") (not (eobp)))
	    (setq resume-args
		  (cons (buffer-substring (point)
					  (progn
					    (skip-chars-forward "^ \t\n")
					    (point)))
			     resume-args)))
	  (cond (resume-args
		 ;; Arguments are now in reverse order.
		 (setq resume-args (nreverse resume-args))
		 ;; The "first argument" is really a default directory to use
		 ;; while processing the rest of the arguments.
		 (setq default-directory (concat (car resume-args) "/"))
		 ;; Actually process the arguments.
		 (command-line-1  (cdr resume-args)))))
      ;; If the command line args don't result in a find-file, the
      ;; buffer will be left in resume-args-buffer.  So we change back to the
      ;; original buffer.  The reason I don't just use
      ;; (let ((default-directory foo))
      ;;    (command-line-1 args))
      ;; in the context of the original buffer is because let does not
      ;; work properly with buffer-local variables.
      (if (eq (current-buffer) resume-args-buffer)
	  (set-buffer resume-start-buffer)))))
