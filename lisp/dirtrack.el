;;; dirtrack.el --- Directory Tracking by watching the prompt

;; Copyright (C) 1996, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

;; Author: Peter Breton <pbreton@cs.umb.edu>
;; Created: Sun Nov 17 1996
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Shell directory tracking by watching the prompt.
;;
;; This is yet another attempt at a directory-tracking package for
;; Emacs shell-mode. However, this package makes one strong assumption:
;; that you can customize your shell's prompt to contain the
;; current working directory. Most shells do support this, including
;; almost every type of Bourne and C shell on Unix, the native shells on
;; Windows95 (COMMAND.COM) and Windows NT (CMD.EXE), and most 3rd party
;; Windows shells. If you cannot do this, or do not wish to, this package
;; will be useless to you.
;;
;; Installation:
;;
;; 1) Set your shell's prompt to contain the current working directory.
;; You may need to consult your shell's documentation to find out how to
;; do this.
;;
;; Note that directory tracking is done by matching regular expressions,
;; therefore it is *VERY IMPORTANT* for your prompt to be easily
;; distinguishable from other output. If your prompt regexp is too general,
;; you will see error messages from the dirtrack filter as it attempts to cd
;; to non-existent directories.
;;
;; 2) Set the variable `dirtrack-list' to an appropriate value. This
;; should be a list of two elements: the first is a regular expression
;; which matches your prompt up to and including the pathname part.
;; The second is a number which tells which regular expression group to
;; match to extract only the pathname. If you use a multi-line prompt,
;; add 't' as a third element. Note that some of the functions in
;; 'comint.el' assume a single-line prompt (eg, comint-bol).
;;
;; Determining this information may take some experimentation. Setting
;; the variable `dirtrack-debug' may help; it causes the directory-tracking
;; filter to log messages to the buffer `dirtrack-debug-buffer'. You can easily
;; toggle this setting with the `dirtrack-debug-toggle' function.
;;
;; 3) Add a hook to shell-mode to enable the directory tracking:
;;
;; (add-hook 'shell-mode-hook
;;   (function (lambda ()
;; 	      (setq comint-preoutput-filter-functions
;; 		    (append (list 'dirtrack)
;; 			    comint-preoutput-filter-functions)))))
;;
;; You may wish to turn ordinary shell tracking off by calling
;; `shell-dirtrack-toggle' or setting `shell-dirtrackp'.
;;
;; Examples:
;;
;; 1) On Windows NT, my prompt is set to emacs$S$P$G.
;; 'dirtrack-list' is set to (list "^emacs \\([a-zA-Z]:.*\\)>" 1)
;;
;; 2) On Solaris running bash, my prompt is set like this:
;;    PS1="\w\012emacs@\h(\!) [\t]% "
;;    'dirtrack-list' is set to (list "^\\([/~].*\\)\nemacs@[^%]+% *" 1 t)
;;
;; I'd appreciate other examples from people who use this package.
;;
;; Here's one from Stephen Eglen:
;;
;;   Running under tcsh:
;;   (setq-default dirtrack-list '("^%E \\([^ ]+\\)" 1))
;;
;;   It might be worth mentioning in your file that emacs sources start up
;;   files of the form: ~/.emacs_<SHELL> where <SHELL> is the name of the
;;   shell.  So for example, I have the following in ~/.emacs_tcsh:
;;
;;   set prompt = "%%E %~ %h% "
;;
;;   This produces a prompt of the form:
;;   %E /var/spool 10%
;;
;;   This saves me from having to use the %E prefix in other non-emacs
;;   shells.
;;
;; A final note:
;;
;;   I run LOTS of shell buffers through Emacs, sometimes as different users
;;   (eg, when logged in as myself, I'll run a root shell in the same Emacs).
;;   If you do this, and the shell prompt contains a ~, Emacs will interpret
;;   this relative to the user which owns the Emacs process, not the user
;;   who owns the shell buffer. This may cause dirtrack to behave strangely
;;   (typically it reports that it is unable to cd to a directory
;;   with a ~ in it).
;;
;;   The same behavior can occur if you use dirtrack with remote filesystems
;;   (using telnet, rlogin, etc) as Emacs will be checking the local
;;   filesystem, not the remote one. This problem is not specific to dirtrack,
;;   but also affects file completion, etc.

;;; Code:

(eval-when-compile
  (require 'comint)
  (require 'shell))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup dirtrack nil
  "Directory tracking by watching the prompt."
  :prefix "dirtrack-"
  :group 'shell)

(defcustom dirtrack-list (list "^emacs \\([a-zA-Z]:.*\\)>" 1)
  "*List for directory tracking.
First item is a regexp that describes where to find the path in a prompt.
Second is a number, the regexp group to match.  Optional third item is
whether the prompt is multi-line.  If nil or omitted, prompt is assumed to
be on a single line."
  :group 'dirtrack
  :type  '(sexp (regexp  :tag "Prompt Expression")
		(integer :tag "Regexp Group")
		(boolean :tag "Multiline Prompt")
		)
  )

(make-variable-buffer-local 'dirtrack-list)

(defcustom dirtrack-debug nil
  "*If non-nil, the function `dirtrack' will report debugging info."
  :group 'dirtrack
  :type  'boolean
  )

(defcustom dirtrack-debug-buffer "*Directory Tracking Log*"
  "Buffer to write directory tracking debug information."
  :group 'dirtrack
  :type  'string
  )

(defcustom dirtrackp t
  "*If non-nil, directory tracking via `dirtrack' is enabled."
  :group 'dirtrack
  :type  'boolean
  )

(make-variable-buffer-local 'dirtrackp)

(defcustom dirtrack-directory-function
  (if (memq system-type (list 'ms-dos 'windows-nt 'cygwin))
      'dirtrack-windows-directory-function
    'dirtrack-default-directory-function)
  "*Function to apply to the prompt directory for comparison purposes."
  :group 'dirtrack
  :type  'function
  )

(defcustom dirtrack-canonicalize-function
  (if (memq system-type (list 'ms-dos 'windows-nt 'cygwin))
      'downcase 'identity)
  "*Function to apply to the default directory for comparison purposes."
  :group 'dirtrack
  :type  'function
  )

(defcustom dirtrack-directory-change-hook nil
  "Hook that is called when a directory change is made."
  :group 'dirtrack
  :type 'hook
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dirtrack-default-directory-function (dir)
  "Return a canonical directory for comparison purposes.
Such a directory ends with a forward slash."
  (let ((directory dir))
    (if (not (char-equal ?/ (string-to-char (substring directory -1))))
	(concat directory "/")
      directory)))

(defun dirtrack-windows-directory-function (dir)
  "Return a canonical directory for comparison purposes.
Such a directory is all lowercase, has forward-slashes as delimiters,
and ends with a forward slash."
  (let ((directory dir))
    (setq directory (downcase (dirtrack-replace-slash directory t)))
    (if (not (char-equal ?/ (string-to-char (substring directory -1))))
	(concat directory "/")
      directory)))

(defun dirtrack-cygwin-directory-function (dir)
  "Return a canonical directory taken from a Cygwin path for comparison purposes."
  (if (string-match "/cygdrive/\\([A-Z]\\)\\(.*\\)" dir)
      (concat (match-string 1 dir) ":" (match-string 2 dir))
    dir))

(defconst dirtrack-forward-slash  (regexp-quote "/"))
(defconst dirtrack-backward-slash (regexp-quote "\\"))

(defun dirtrack-replace-slash (string &optional opposite)
  "Replace forward slashes with backwards ones.
If additional argument is non-nil, replace backwards slashes with
forward ones."
  (let ((orig     (if opposite
		      dirtrack-backward-slash
		    dirtrack-forward-slash))
	(replace  (if opposite
		      dirtrack-forward-slash
		    dirtrack-backward-slash))
	(newstring string)
	)
    (while (string-match orig newstring)
      (setq newstring (replace-match replace nil t newstring)))
  newstring))

;; Copied from shell.el
(defun dirtrack-toggle ()
  "Enable or disable Dirtrack directory tracking in a shell buffer."
  (interactive)
  (setq dirtrackp (not dirtrackp))
  (message "Directory tracking %s" (if dirtrackp "ON" "OFF")))

(defun dirtrack-debug-toggle ()
  "Enable or disable Dirtrack debugging."
  (interactive)
  (setq dirtrack-debug (not dirtrack-debug))
  (message "Directory debugging %s" (if dirtrack-debug "ON" "OFF"))
  (and dirtrack-debug
       (display-buffer (get-buffer-create dirtrack-debug-buffer))))

(defun dirtrack-debug-message (string)
  (let ((buf (current-buffer))
	(debug-buf (get-buffer-create dirtrack-debug-buffer))
	)
    (set-buffer debug-buf)
    (goto-char (point-max))
    (insert (concat string "\n"))
    (set-buffer buf)
  ))

;;;###autoload
(defun dirtrack (input)
  "Determine the current directory by scanning the process output for a prompt.
The prompt to look for is the first item in `dirtrack-list'.

You can toggle directory tracking by using the function `dirtrack-toggle'.

If directory tracking does not seem to be working, you can use the
function `dirtrack-debug-toggle' to turn on debugging output.

You can enable directory tracking by adding this function to
`comint-output-filter-functions'.
"
  (if (null dirtrackp)
      nil
    (let (prompt-path
	  matched
	  (current-dir default-directory)
	  (dirtrack-regexp    (nth 0 dirtrack-list))
	  (match-num	      (nth 1 dirtrack-list))
	  (multi-line	      (nth 2 dirtrack-list))
	  )
      ;; No output?
      (if (eq (point) (point-min))
	  nil
	(save-excursion
	  (setq matched (string-match dirtrack-regexp input)))
	  ;; No match
	  (if (null matched)
	      (and dirtrack-debug
		   (dirtrack-debug-message
		    (format
		     "Input `%s' failed to match regexp: %s"
		    input dirtrack-regexp)))
	    (setq prompt-path
		  (substring input
		   (match-beginning match-num) (match-end match-num)))
	    ;; Empty string
	    (if (not (> (length prompt-path) 0))
		(and dirtrack-debug
		     (dirtrack-debug-message "Match is empty string"))
	      ;; Transform prompts into canonical forms
	      (setq prompt-path (funcall dirtrack-directory-function
					 prompt-path))
	      (setq current-dir (funcall dirtrack-canonicalize-function
					 current-dir))
	      (and dirtrack-debug
		   (dirtrack-debug-message
		    (format
		     "Prompt is %s\nCurrent directory is %s"
		     prompt-path current-dir)))
	      ;; Compare them
	      (if (or (string= current-dir prompt-path)
		      (string= current-dir
			       (abbreviate-file-name prompt-path)))
		  (and dirtrack-debug
		       (dirtrack-debug-message
			(format "Not changing directory")))
		;; It's possible that Emacs will think the directory
		;; won't exist (eg, rlogin buffers)
		(if (file-accessible-directory-p prompt-path)
		    ;; Change directory
		    (and (shell-process-cd prompt-path)
			 (run-hooks 'dirtrack-directory-change-hook)
			 dirtrack-debug
			 (dirtrack-debug-message
			  (format "Changing directory to %s" prompt-path)))
		  (error "Directory %s does not exist" prompt-path)))
	      )))))
  input)

(provide 'dirtrack)

;;; arch-tag: 168de071-be88-4937-aff6-2aba9f328d5a
;;; dirtrack.el ends here
