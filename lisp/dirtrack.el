;;; dirtrack.el --- Directory Tracking by watching the prompt

;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author:  Peter Breton
;; Created: Sun Nov 17 1996
;; Keywords: processes
;; Time-stamp: <96/12/26 09:23:01 peter>

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
;; 2) Set the variable 'dirtrack-list' to an appropriate value. This
;; should be a list of two elements: the first is a regular expression
;; which matches your prompt up to and including the pathname part.
;; The second is a number which tells which regular expression group to 
;; match to extract only the pathname. If you use a multi-line prompt,
;; add 't' as a third element. Note that some of the functions in
;; 'comint.el' assume a single-line prompt (eg, comint-bol).
;; 
;; Determining this information may take some experimentation. Setting
;; the variable 'dirtrack-debug' may help; it causes the directory-tracking
;; filter to log messages to the buffer 'dirtrack-debug-buffer'.
;;
;; 3) Autoload directory tracking by adding the following to your .emacs:
;; 
;; (autoload 'dirtrack "dirtrack"
;;  "Directory tracking by watching the prompt")
;; 
;; 4) Add a hook to shell-mode to enable the directory tracking:
;;
;; (add-hook 'shell-mode-hook
;;   (function (lambda ()
;; 	      (setq comint-output-filter-functions
;; 		    (append (list 'dirtrack)
;; 			    comint-output-filter-functions)))))
;;
;; You may wish to turn ordinary shell tracking off by calling
;; 'shell-dirtrack-toggle' or setting 'shell-dirtrackp'.
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

;;; Code:

(eval-when-compile
  (require 'comint)
  (require 'shell))

(defvar dirtrack-list (list "^emacs \\([a-zA-Z]:.*\\)>" 1)
  "*List for directory tracking.
First item is a regexp that describes where to find the path in a prompt.
Second is a number, the regexp group to match. Optional third item is 
whether the prompt is multi-line. If nil or omitted, prompt is assumed to 
be on a single line.")

(make-variable-buffer-local 'dirtrack-list)

(defvar dirtrack-debug nil
  "*If non-nil, the function 'dirtrack' will report debugging info.")

(defvar dirtrack-debug-buffer "*Directory Tracking Log*"
  "Buffer to write directory tracking debug information.")

(defvar dirtrackp t
  "*If non-nil, directory tracking via 'dirtrack' is enabled.")

(make-variable-buffer-local 'dirtrackp)

(defvar dirtrack-directory-function 
  (if (memq system-type (list 'ms-dos 'windows-nt))
      'dirtrack-windows-directory-function
    'dirtrack-default-directory-function)
  "*Function to apply to the prompt directory for comparison purposes.")

(defvar dirtrack-canonicalize-function  
  (if (memq system-type (list 'ms-dos 'windows-nt))
      'downcase 'identity)
  "*Function to apply to the default directory for comparison purposes.")

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
    (setq directory (downcase (replace-slash directory t)))
    (if (not (char-equal ?/ (string-to-char (substring directory -1))))
	(concat directory "/")
      directory)))

(defconst forward-slash  (regexp-quote "/"))
(defconst backward-slash (regexp-quote "\\"))

(defun replace-slash (string &optional opposite)
  "Replace forward slashes with backwards ones.
If additional argument is non-nil, replace backwards slashes with 
forward ones."
  (let ((orig     (if opposite backward-slash forward-slash))
	(replace  (if opposite forward-slash backward-slash))
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

(defun dirtrack-debug-message (string)
  (let ((buf (current-buffer))
	(debug-buf (get-buffer-create dirtrack-debug-buffer))
	)
    (set-buffer debug-buf)
    (insert (concat string "\n"))
    (set-buffer buf)
  ))

;;;###autoload
(defun dirtrack (input)
  (if (null dirtrackp)
      nil
    (let ((prompt-path)
	  (current-dir default-directory)
	  (matched)
	  (dirtrack-regexp (nth 0 dirtrack-list))
	  (match-num	      (nth 1 dirtrack-list))
	  (multi-line	      (nth 2 dirtrack-list))
	  )
      ;; No output?
      (if (eq (point) (point-min))
	  nil
	(save-excursion
	  (goto-char comint-last-output-start)
	  ;; Look for the prompt
	  (if multi-line
	      (and
	       (goto-char (point-max))
	       (setq matched 
		     (re-search-backward 
		      dirtrack-regexp 
		      comint-last-output-start
		      t)))
	    (beginning-of-line)
	    (setq matched (looking-at dirtrack-regexp)))
	  ;; No match
	  (if (null matched)
	      (and dirtrack-debug
		   (dirtrack-debug-message 
		    (format 
		     "Failed to match regexp: %s" 
		    dirtrack-regexp)))
	    (setq prompt-path 
		  (buffer-substring-no-properties
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
		;; Change directory
		(shell-process-cd prompt-path)
		(and dirtrack-debug
		     (dirtrack-debug-message 
		      (format "Changing directory to %s" prompt-path))))
	      )))))))

(provide 'dirtrack)

;;; dirtrack.el ends here
