;;; whitespace.el --- Warn about and clean bogus whitespaces in the file.

;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Rajesh Vaidheeswarran <rv@dsmit.com>
;; Keywords: convenience

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

;; Whitespace.el URL: http://www.dsmit.com/lisp/

;; Exported functions:

;; `whitespace-buffer' - To check the current buffer for whitespace problems.
;; `whitespace-cleanup' - To cleanup all whitespaces in the current buffer.
;; `whitespace-region' - To check between point and mark for whitespace
;;                       problems.
;; `whitespace-cleanup-region' - To cleanup all whitespaces between point
;;                               and mark in the current buffer.
;; `whitespace-describe' - A simple introduction to the library.

;;; Code:

;; add a hook to find-file-hooks and kill-buffer-hook
(add-hook 'find-file-hooks 'whitespace-buffer)
(add-hook 'kill-buffer-hook 'whitespace-buffer)

(defvar whitespace-version "2.8" "Version of the whitespace library.")
;; Find out what type of Emacs we are running in.
(defvar whitespace-running-emacs (if (string-match "XEmacs\\|Lucid"
						   emacs-version) nil t)
  "If this is Emacs, not XEmacs, this is t.")

(if whitespace-running-emacs (require 'timer))

(defvar whitespace-all-buffer-files nil
  "An associated list of buffers and files checked for whitespace cleanliness.

This is to enable periodic checking of whitespace cleanliness in the files
visited by the buffers.")

(defvar whitespace-rescan-timer nil
  "Timer object used to rescan the files in buffers that have been modified.")

;; Tell Emacs about this new kind of minor mode
(defvar whitespace-mode nil
  "Non-nil when Whitespace mode (a minor mode) is enabled.")
(make-variable-buffer-local 'whitespace-mode)
(put 'whitespace-mode 'permanent-local nil)

(defvar whitespace-mode-line nil
  "String to display in the mode line for Whitespace mode.")
(make-variable-buffer-local 'whitespace-mode-line)
(put 'whitespace-mode-line 'permanent-local nil)

;; For flavors of Emacs which don't define `defgroup' and `defcustom'.
(eval-when-compile
  (if (not (fboundp 'defgroup))
      (defmacro defgroup (sym memb doc &rest args)
	"Null macro for defgroup in all versions of Emacs that don't define
defgroup"
	t))
  (if (not (fboundp 'defcustom))
      (defmacro defcustom (sym val doc &rest args)
	"Macro to alias defcustom to defvar in all versions of Emacs that
don't define defcustom"
	`(defvar ,sym ,val ,doc))))

(defgroup whitespace nil
  "Check for and fix five different types of whitespaces in source code."
  ;; Since XEmacs doesn't have a 'convenience group, use the next best group
  ;; which is 'editing?
  :group (if whitespace-running-emacs 'convenience 'editing))

(defcustom whitespace-check-leading-whitespace t
  "Flag to check leading whitespace."
  :type 'boolean
  :group 'whitespace)

(defcustom whitespace-check-trailing-whitespace t
  "Flag to check trailing whitespace."
  :type 'boolean
  :group 'whitespace)

(defcustom whitespace-check-spacetab-whitespace t
  "Flag to check space followed by a TAB."
  :type 'boolean
  :group 'whitespace)

(defcustom whitespace-spacetab-regexp " \t"
  "Regexp to match a space followed by a TAB."
  :type 'string
  :group 'whitespace)

(defcustom whitespace-check-indent-whitespace t
  "Flag to check indentation whitespace."
  :type 'boolean
  :group 'whitespace)

(defcustom whitespace-indent-regexp (concat "^\\(\t*\\)    " "    ")
  "Regexp to match (any TABS followed by) 8/more whitespaces at start of line."
  :type 'string
  :group 'whitespace)

(defcustom whitespace-check-ateol-whitespace t
  "Flag to check end-of-line whitespace."
  :type 'boolean
  :group 'whitespace)

(defcustom whitespace-ateol-regexp "[ \t]$"
  "Regexp to match a TAB or a space at the EOL."
  :type 'string
  :group 'whitespace)

(defcustom whitespace-errbuf "*Whitespace Errors*"
  "The buffer where whitespace related messages will be logged."
  :type 'string
  :group 'whitespace)

(defcustom whitespace-auto-cleanup nil
  "Cleanup a buffer automatically on finding it whitespace unclean."
  :type  'boolean
  :group 'whitespace)

(defcustom whitespace-silent nil
  "All whitespace errors will be shown only in the modeline when t.

Note that setting this may cause all whitespaces introduced in a file to go
unnoticed when the buffer is killed, unless the user visits the `*Whitespace
Errors*' buffer before opening (or closing) another file."
  :type 'boolean
  :group 'whitespace)

(defcustom whitespace-modes '(ada-mode asm-mode autoconf-mode awk-mode
				       c-mode c++-mode cc-mode
				       change-log-mode cperl-mode
				       electric-nroff-mode emacs-lisp-mode
				       f90-mode fortran-mode html-mode
				       html3-mode java-mode jde-mode
				       ksh-mode latex-mode LaTeX-mode
				       lisp-mode m4-mode makefile-mode
				       modula-2-mode nroff-mode objc-mode
				       pascal-mode perl-mode prolog-mode
				       python-mode scheme-mode sgml-mode
				       sh-mode shell-script-mode simula-mode
				       tcl-mode tex-mode texinfo-mode
				       vrml-mode xml-mode)

  "Major Modes in which we turn on whitespace checking.

These are mostly programming and documentation modes. But you may add other
modes that you want whitespaces checked in by adding something like the
following to your `.emacs':

\(setq whitespace-modes (cons 'my-mode (cons 'my-other-mode
					    whitespace-modes))\)

Or, alternately, you can use the Emacs `customize' command to set this."
  :group 'whitespace)

(defcustom whitespace-rescan-timer-time 60
  "Period in seconds to rescan modified buffers for whitespace creep.

This is the period after which the timer will fire causing
`whitespace-rescan-files-in-buffers' to check for whitespace creep in
modified buffers.

To disable timer scans, set this to zero."
  :type 'integer
  :group 'whitespace)

(defcustom whitespace-display-in-modeline t
  "Display whitespace errors on the modeline."
  :type 'boolean
  :group 'whitespace)

(if (not (assoc 'whitespace-mode minor-mode-alist))
    (setq minor-mode-alist (cons '(whitespace-mode whitespace-mode-line)
				 minor-mode-alist)))

(defun whitespace-check-whitespace-mode (&optional arg)
  "Test and set the whitespace-mode in qualifying buffers."
  (if (null whitespace-mode)
      (setq whitespace-mode
	    (if (or arg (member major-mode whitespace-modes))
		t
	      nil))))

;;;###autoload
(defun whitespace-buffer (&optional quiet)
  "Find five different types of white spaces in buffer:

1. Leading space \(empty lines at the top of a file\).
2. Trailing space \(empty lines at the end of a file\).
3. Indentation space \(8 or more spaces, that should be replaced with TABS\).
4. Spaces followed by a TAB. \(Almost always, we never want that\).
5. Spaces or TABS at the end of a line.

Check for whitespace only if this buffer really contains a non-empty file
and:
1. the major mode is one of the whitespace-modes, or
2. `whitespace-buffer' was explicitly called with a prefix argument."
  (interactive)
  (let ((whitespace-error nil))
    (whitespace-check-whitespace-mode current-prefix-arg)
    (if (and buffer-file-name (> (buffer-size) 0) whitespace-mode)
	(progn
	  (whitespace-check-buffer-list (buffer-name) buffer-file-name)
	  (whitespace-tickle-timer)
	  (if whitespace-auto-cleanup
	      (if buffer-read-only
		  (if (not quiet)
		      (message "Can't cleanup: %s is read-only" (buffer-name)))
		(whitespace-cleanup))
	    (let ((whitespace-leading (if whitespace-check-leading-whitespace
					  (whitespace-buffer-leading)
					nil))
		  (whitespace-trailing (if whitespace-check-trailing-whitespace
					   (whitespace-buffer-trailing)
					 nil))
		  (whitespace-indent (if whitespace-check-indent-whitespace
					 (whitespace-buffer-search
					  whitespace-indent-regexp)
				       nil))
		  (whitespace-spacetab (if whitespace-check-spacetab-whitespace
					   (whitespace-buffer-search
					    whitespace-spacetab-regexp)
					 nil))
		  (whitespace-ateol (if whitespace-check-ateol-whitespace
					(whitespace-buffer-search
					 whitespace-ateol-regexp)
				      nil))
		  (whitespace-errmsg nil)
		  (whitespace-filename buffer-file-name)
		  (whitespace-this-modeline ""))

	      ;; Now let's complain if we found any of the above.
	      (setq whitespace-error (or whitespace-leading whitespace-indent
					 whitespace-spacetab whitespace-ateol
					 whitespace-trailing))

	      (if whitespace-error
		  (progn
		    (setq whitespace-errmsg
			  (concat whitespace-filename " contains:\n"
				  (if whitespace-leading
				      "Leading whitespace\n")
				  (if whitespace-indent
				      (concat "Indentation whitespace"
					      whitespace-indent "\n"))
				  (if whitespace-spacetab
				      (concat "Space followed by Tab"
					      whitespace-spacetab "\n"))
				  (if whitespace-ateol
				      (concat "End-of-line whitespace"
					      whitespace-ateol "\n"))
				  (if whitespace-trailing
				      "Trailing whitespace\n")
				  "\ntype `M-x whitespace-cleanup' to "
				  "cleanup the file."))
		    (setq whitespace-this-modeline
			  (concat (if whitespace-ateol "e")
				  (if whitespace-indent "i")
				  (if whitespace-leading "l")
				  (if whitespace-spacetab "s")
				  (if whitespace-trailing "t")))))
	      (whitespace-update-modeline whitespace-this-modeline)
	      (save-excursion
		(get-buffer-create whitespace-errbuf)
		(kill-buffer whitespace-errbuf)
		(get-buffer-create whitespace-errbuf)
		(set-buffer whitespace-errbuf)
		(if whitespace-errmsg
		    (progn
		      (insert whitespace-errmsg)
		      (if (not (or quiet whitespace-silent))
			  (display-buffer whitespace-errbuf t))
		      (if (not quiet)
			  (message "Whitespaces: [%s%s] in %s"
				   whitespace-this-modeline
				   (let ((whitespace-unchecked
					  (whitespace-unchecked-whitespaces)))
				     (if whitespace-unchecked
					 (concat "!" whitespace-unchecked)
				       ""))
				   whitespace-filename)))
		  (if (not quiet)
		      (message "%s clean" whitespace-filename))))))))
    (if whitespace-error
	t
      nil)))

;;;###autoload
(defun whitespace-region (s e)
  "Check a region specified by point and mark for whitespace errors."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region s e)
      (whitespace-buffer))))

;;;###autoload
(defun whitespace-cleanup ()
  "Cleanup the five different kinds of whitespace problems.

Use \\[describe-function] whitespace-describe to read a summary of the
whitespace problems."
  (interactive)
  ;; If this buffer really contains a file, then run, else quit.
  (whitespace-check-whitespace-mode current-prefix-arg)
  (if (and buffer-file-name whitespace-mode)
      (let ((whitespace-any nil)
	    (whitespace-tabwith 8)
	    (whitespace-tabwith-saved tab-width))

	;; since all printable TABS should be 8, irrespective of how
	;; they are displayed.
	(setq tab-width whitespace-tabwith)

	(if (and whitespace-check-leading-whitespace
		 (whitespace-buffer-leading))
	    (progn
	      (whitespace-buffer-leading-cleanup)
	      (setq whitespace-any t)))

	(if (and whitespace-check-trailing-whitespace
		 (whitespace-buffer-trailing))
	    (progn
	      (whitespace-buffer-trailing-cleanup)
	      (setq whitespace-any t)))

	(if (and whitespace-check-indent-whitespace
		 (whitespace-buffer-search whitespace-indent-regexp))
	    (progn
	      (whitespace-indent-cleanup)
	      (setq whitespace-any t)))

	(if (and whitespace-check-spacetab-whitespace
		 (whitespace-buffer-search whitespace-spacetab-regexp))
	    (progn
	      (whitespace-buffer-cleanup whitespace-spacetab-regexp "\t")
	      (setq whitespace-any t)))

	(if (and whitespace-check-ateol-whitespace
		 (whitespace-buffer-search whitespace-ateol-regexp))
	    (progn
	      (whitespace-buffer-cleanup whitespace-ateol-regexp "")
	      (setq whitespace-any t)))

	;; Call this recursively till everything is taken care of
	(if whitespace-any
	    (whitespace-cleanup)
	  (progn
	    (message "%s clean" buffer-file-name)
	    (whitespace-update-modeline)))
	(setq tab-width whitespace-tabwith-saved))))

;;;###autoload
(defun whitespace-cleanup-region (s e)
  "Whitespace cleanup on a region specified by point and mark."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region s e)
      (whitespace-cleanup))
    (whitespace-buffer t)))

(defun whitespace-buffer-leading ()
  "Check to see if there are any empty lines at the top of the file."
  (save-excursion
    (let ((pmin nil)
	  (pmax nil))
      (goto-char (point-min))
      (beginning-of-line)
      (setq pmin (point))
      (end-of-line)
      (setq pmax (point))
      (if (equal pmin pmax)
	  t
	nil))))

(defun whitespace-buffer-leading-cleanup ()
  "Remove any empty lines at the top of the file."
  (save-excursion
    (let ((pmin nil)
	  (pmax nil))
      (goto-char (point-min))
      (beginning-of-line)
      (setq pmin (point))
      (end-of-line)
      (setq pmax (point))
      (if (equal pmin pmax)
	  (progn
	    (kill-line)
	    (whitespace-buffer-leading-cleanup))))))

(defun whitespace-buffer-trailing ()
  "Check to see if are is more than one empty line at the bottom."
  (save-excursion
    (let ((pmin nil)
	  (pmax nil))
      (goto-char (point-max))
      (beginning-of-line)
      (setq pmin (point))
      (end-of-line)
      (setq pmax (point))
      (if (equal pmin pmax)
	  (progn
	    (goto-char (- (point) 1))
	    (beginning-of-line)
	    (setq pmin (point))
	    (end-of-line)
	    (setq pmax (point))
	    (if (equal pmin pmax)
		t
	      nil))
	nil))))

(defun whitespace-buffer-trailing-cleanup ()
  "Delete all the empty lines at the bottom."
  (save-excursion
    (let ((pmin nil)
	  (pmax nil))
      (goto-char (point-max))
      (beginning-of-line)
      (setq pmin (point))
      (end-of-line)
      (setq pmax (point))
      (if (equal pmin pmax)
	  (progn
	    (goto-char (1- pmin))
	    (beginning-of-line)
	    (setq pmin (point))
	    (end-of-line)
	    (setq pmax (point))
	    (if (equal pmin pmax)
		(progn
		  (goto-char (1- (point-max)))
		  (beginning-of-line)
		  (kill-line)
		  (whitespace-buffer-trailing-cleanup))))))))

(defun whitespace-buffer-search (regexp)
  "Search for any given whitespace REGEXP."
  (let ((whitespace-retval ""))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(setq whitespace-retval (format "%s %s " whitespace-retval
					(match-beginning 0))))
      (if (equal "" whitespace-retval)
	  nil
	whitespace-retval))))

(defun whitespace-buffer-cleanup (regexp newregexp)
  "Search for any given whitespace REGEXP and replace it with the NEWREGEXP."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (replace-match newregexp))))

(defun whitespace-indent-cleanup ()
  "Search for 8/more spaces at the start of a line and replace it with tabs."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward whitespace-indent-regexp nil t)
      (let ((column (current-column))
	    (indent-tabs-mode t))
	(delete-region (match-beginning 0) (point))
	(indent-to column)))))

(defun whitespace-unchecked-whitespaces ()
  "Return the list of whitespaces whose testing has been suppressed."
  (let ((whitespace-this-modeline
	 (concat (if (not whitespace-check-ateol-whitespace) "e")
		 (if (not whitespace-check-indent-whitespace) "i")
		 (if (not whitespace-check-leading-whitespace) "l")
		 (if (not whitespace-check-spacetab-whitespace) "s")
		 (if (not whitespace-check-trailing-whitespace) "t"))))
    (if (not (equal whitespace-this-modeline ""))
	whitespace-this-modeline
      nil)))

(defun whitespace-update-modeline (&optional whitespace-err)
  "Update modeline with whitespace errors and whitespaces whose testing has
been turned off."
  (if whitespace-display-in-modeline
      (progn
	(setq whitespace-mode-line nil)
	;; Whitespace errors
	(if (and whitespace-err (not (equal whitespace-err "")))
	    (setq whitespace-mode-line whitespace-err))
	;; Whitespace suppressed errors
	(let ((whitespace-unchecked (whitespace-unchecked-whitespaces)))
	  (if whitespace-unchecked
	      (setq whitespace-mode-line
		    (concat whitespace-mode-line "!" whitespace-unchecked))))
	;; Add the whitespace modeline prefix
	(setq whitespace-mode-line (if whitespace-mode-line
				       (concat " W:" whitespace-mode-line)
				     nil))
	(whitespace-force-mode-line-update))))

;; Force mode line updation for different Emacs versions
(defun whitespace-force-mode-line-update ()
  "Force the mode line update for different flavors of Emacs."
  (if whitespace-running-emacs
      (force-mode-line-update)          ; Emacs
    (redraw-modeline)))                 ; XEmacs

(defun whitespace-check-buffer-list (buf-name buf-file)
  "Add a buffer and its file to the whitespace monitor list.

The buffer named BUF-NAME and its associated file BUF-FILE are now monitored
periodically for whitespace."
  (if (and whitespace-mode (not (member (list buf-file buf-name)
					whitespace-all-buffer-files)))
      (add-to-list 'whitespace-all-buffer-files (list buf-file buf-name))))

(defun whitespace-tickle-timer ()
  "Tickle timer to periodically to scan qualifying files for whitespace creep.

If timer is not set, then set it to scan the files in
`whitespace-all-buffer-files' periodically (defined by
`whitespace-rescan-timer-time') for whitespace creep."
  (if (and whitespace-rescan-timer-time (not whitespace-rescan-timer))
      (setq whitespace-rescan-timer
	    (if whitespace-running-emacs
		(run-at-time nil whitespace-rescan-timer-time
			     'whitespace-rescan-files-in-buffers)
	      (add-timeout whitespace-rescan-timer-time
			   'whitespace-rescan-files-in-buffers nil
			   whitespace-rescan-timer-time)))))

(defun whitespace-rescan-files-in-buffers (&optional arg)
  "Check monitored files for whitespace creep since last scan."
  (let ((whitespace-all-my-files whitespace-all-buffer-files)
	buffile bufname thiselt buf)
    (if (not whitespace-all-my-files)
	(progn
	  (if whitespace-running-emacs
	      (cancel-timer whitespace-rescan-timer)
	    (disable-timeout whitespace-rescan-timer))
	  (setq whitespace-rescan-timer nil))
      (while whitespace-all-my-files
	(setq thiselt (car whitespace-all-my-files))
	(setq whitespace-all-my-files (cdr whitespace-all-my-files))
	(setq buffile (car thiselt))
	(setq bufname (cadr thiselt))
	(setq buf (get-buffer bufname))
	(if (buffer-live-p buf)
	    (save-excursion
	      ;;(message "buffer %s live" bufname)
	      (set-buffer bufname)
	      (if whitespace-mode
		  (progn
		    ;;(message "checking for whitespace in %s" bufname)
		    (if whitespace-auto-cleanup
			(progn
			  ;;(message "cleaning up whitespace in %s" bufname)
			  (whitespace-cleanup))
		      (progn
			;;(message "whitespace-buffer %s." (buffer-name))
			(whitespace-buffer t))))
		;;(message "Removing %s from refresh list" bufname)
		(whitespace-refresh-rescan-list buffile bufname)))
	  ;;(message "Removing %s from refresh list" bufname)
	  (whitespace-refresh-rescan-list buffile bufname))))))

(defun whitespace-refresh-rescan-list (buffile bufname)
  "Refresh the list of files to be rescaned for whitespace creep."
  (if whitespace-all-buffer-files
      (progn
	(setq whitespace-all-buffer-files
	      (delete (list buffile bufname) whitespace-all-buffer-files)))
    (progn
      (if (and whitespace-running-emacs (timerp whitespace-rescan-timer))
	  (cancel-timer whitespace-rescan-timer))
      (if (and (not whitespace-running-emacs) whitespace-rescan-timer)
	  (disable-timeout whitespace-rescan-timer))
      (if whitespace-rescan-timer
	  (setq whitespace-rescan-timer nil)))))

;;;###autoload
(defun whitespace-describe ()
  "A summary of whitespaces and what this library can do about them.

The whitespace library is intended to find and help fix five different types
of whitespace problems that commonly exist in source code.

1. Leading space (empty lines at the top of a file).
2. Trailing space (empty lines at the end of a file).
3. Indentation space (8 or more spaces at beginning of line, that should be
		      replaced with TABS).
4. Spaces followed by a TAB. (Almost always, we never want that).
5. Spaces or TABS at the end of a line.

Whitespace errors are reported in a buffer, and on the modeline.

Modeline will show a W:<x>!<y> to denote a particular type of whitespace,
where `x' and `y' can be one (or more) of:

e - End-of-Line whitespace.
i - Indentation whitespace.
l - Leading whitespace.
s - Space followed by Tab.
t - Trailing whitespace.

If any of the whitespace checks is turned off, the modeline will display a
!<y>.

    (since (3) is the most controversial one, here is the rationale: Most
    terminal drivers and printer drivers have TAB configured or even
    hardcoded to be 8 spaces. (Some of them allow configuration, but almost
    always they default to 8.)

    Changing tab-width to other than 8 and editing will cause your code to
    look different from within Emacs, and say, if you cat it or more it, or
    even print it.

    Almost all the popular programming modes let you define an offset (like
    c-basic-offset or perl-indent-level) to configure the offset, so you
    should never have to set your tab-width to be other than 8 in all these
    modes. In fact, with an indent level of say, 4, 2 TABS will cause Emacs
    to replace your 8 spaces with one \t (try it). If vi users in your
    office complain, tell them to use vim, which distinguishes between
    tabstop and shiftwidth (vi equivalent of our offsets), and also ask them
    to set smarttab.)

All the above have caused (and will cause) unwanted codeline integration and
merge problems.

whitespace.el will complain if it detects whitespaces on opening a file, and
warn you on closing a file also. (if in case you had inserted any
whitespaces during the process of your editing.)"
  (interactive)
  (message "Use C-h f whitespace-describe to read about whitespace.el v%s."
	   whitespace-version))

(provide 'whitespace)

;;; whitespace.el ends here
