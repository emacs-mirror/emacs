;;; executable.el --- base functionality for executable interpreter scripts

;; Copyright (C) 1994, 1995, 1996 by Free Software Foundation, Inc.

;; Author: Daniel.Pfeiffer@Informatik.START.dbp.de, fax (+49 69) 7588-2389
;; Keywords: languages, unix

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

;; executable.el is used by certain major modes to insert a suitable
;; #! line at the beginning of the file, if the file does not already
;; have one.

;; Unless it has a magic number, a Unix file with executable mode is passed to
;; a new instance of the running shell (or to a Bourne shell if a csh is
;; running and the file starts with `:').  Only a shell can start such a file,
;; exec() cannot, which is why it is important to have a magic number in every
;; executable script.  Such a magic number is made up by the characters `#!'
;; the filename of an interpreter (in COFF, ELF or somesuch format) and one
;; optional argument.

;; This library is for certain major modes like sh-, awk-, perl-, tcl- or
;; makefile-mode to insert or update a suitable #! line at the beginning of
;; the file, if the file does not already have one and the file is not a
;; default file of that interpreter (like .profile or makefile).  It also
;; makes the file executable if it wasn't, as soon as it's saved.

;; It also allows debugging scripts, with an adaptation of compile, as far
;; as interpreters give out meaningful error messages.

;; Modes that use this should nconc `executable-map' to the end of their own
;; keymap and `executable-font-lock-keywords' to the end of their own font
;; lock keywords.  Their mode-setting commands should call
;; `executable-set-magic'.

;;; Code:

(defvar executable-insert 'not-modified
  "*What to do when newly found file has no or wrong magic number:
	nil	do nothing
	t	insert or update magic number
	other	insert or update magic number, but mark as unmodified.
When the insertion is marked as unmodified, you can save it with  \\[write-file] RET.
This variable is used when `executable-set-magic' is called as a function,
e.g. when Emacs sets some Un*x interpreter script mode.
With \\[executable-set-magic], this is always treated as if it were `t'.")


(defvar executable-query 'function
  "*If non-`nil', ask user before inserting or changing magic number.
When this is `function', only ask when called non-interactively.")


(defvar executable-magicless-file-regexp "/[Mm]akefile$\\|/\\.\\(z?profile\\|bash_profile\\|z?login\\|bash_login\\|z?logout\\|bash_logout\\|.+shrc\\|esrc\\|rcrc\\|[kz]shenv\\)$"
  "*On files with this kind of name no magic is inserted or changed.")


(defvar executable-prefix "#! "
  "*Interpreter magic number prefix inserted when there was no magic number.")



(defvar executable-chmod 73
  "*After saving, if the file is not executable, set this mode.
This mode passed to `set-file-modes' is taken absolutely when negative, or
relative to the files existing modes.  Do nothing if this is nil.
Typical values are 73 (+x) or -493 (rwxr-xr-x).")


(defvar executable-command nil)

(defvar executable-self-display "tail"
  "*Command you use with argument `+2' to make text files self-display.
Note that the like of `more' doesn't work too well under Emacs  \\[shell].")


(defvar executable-font-lock-keywords
  '(("\\`#!.*/\\([^ \t\n]+\\)" 1 font-lock-keyword-face t))
  "*Rules for highlighting executable scripts' magic number.
This can be included in `font-lock-keywords' by modes that call `executable'.")


(defvar executable-error-regexp-alist
  '(;; /bin/xyz: syntax error at line 14: `(' unexpected
    ;; /bin/xyz[5]: syntax error at line 8 : ``' unmatched
    ("^\\(.*[^[/]\\)\\(\\[[0-9]+\\]\\)?: .* error .* line \\([0-9]+\\)" 1 3)
    ;; /bin/xyz[27]: ehco:  not found
    ("^\\(.*[^/]\\)\\[\\([0-9]+\\)\\]: .*: " 1 2)
    ;; /bin/xyz: syntax error near unexpected token `)'
    ;; /bin/xyz: /bin/xyz: line 2: `)'
    ("^\\(.*[^/]\\): [^0-9\n]+\n\\1: \\1: line \\([0-9]+\\):" 1 2)
    ;; /usr/bin/awk: syntax error at line 5 of file /bin/xyz
    (" error .* line \\([0-9]+\\) of file \\(.+\\)$" 2 1)
    ;; /usr/bin/awk: calling undefined function toto
    ;;  input record number 3, file awktestdata
    ;;  source line 4 of file /bin/xyz
    ("^[^ ].+\n\\( .+\n\\)* line \\([0-9]+\\) of file \\(.+\\)$" 3 2)
    ;; makefile:1: *** target pattern contains no `%'.  Stop.
    ("^\\(.+\\):\\([0-9]+\\): " 1 2))
  "Alist of regexps used to match script errors.
See `compilation-error-regexp-alist'.")

;; The C function openp slightly modified would do the trick fine
(defun executable-find (command)
  "Search for COMMAND in exec-path and return the absolute file name.
Return nil if COMMAND is not found anywhere in `exec-path'."
  (let ((list exec-path)
	file)
    (while list
      (setq list (if (and (setq file (expand-file-name command (car list)))
			  (file-executable-p file)
			  (not (file-directory-p file)))
		     nil
		   (setq file nil)
		   (cdr list))))
    file))


(defun executable-chmod ()
  "This gets called after saving a file to assure that it be executable.
You can set the absolute or relative mode in variable `executable-chmod' for
non-executable files."
  (and executable-chmod
       buffer-file-name
       (or (file-executable-p buffer-file-name)
	   (set-file-modes buffer-file-name
			   (if (< executable-chmod 0)
			       (- executable-chmod)
			     (logior executable-chmod
				     (file-modes buffer-file-name)))))))


(defun executable-interpret (command)
  "Run script with user-specified args, and collect output in a buffer.
While script runs asynchronously, you can use the \\[next-error] command
to find the next error."
  (interactive (list (read-string "Run script: "
				  (or executable-command
				      buffer-file-name))))
  (require 'compile)
  (save-some-buffers (not compilation-ask-about-save))
  (make-local-variable 'executable-command)
  (compile-internal (setq executable-command command)
		    "No more errors." "Interpretation"
		    ;; Give it a simpler regexp to match.
		    nil executable-error-regexp-alist))



;;;###autoload
(defun executable-set-magic (interpreter &optional argument
					 no-query-flag insert-flag)
  "Set this buffer's interpreter to INTERPRETER with optional ARGUMENT.
The variables `executable-magicless-file-regexp', `executable-prefix',
`executable-insert', `executable-query' and `executable-chmod' control
when and how magic numbers are inserted or replaced and scripts made
executable."
  (interactive
   (let* ((name (read-string "Name or file name of interpreter: "))
	  (arg (read-string (format "Argument for %s: " name))))
     (list name arg (eq executable-query 'function) t)))
  (setq interpreter (if (file-name-absolute-p interpreter)
			interpreter
		      (or (executable-find interpreter)
			  (error "Interpreter %s not recognized" interpreter)))
	argument (concat interpreter
			 (and argument (string< "" argument) " ")
			 argument))
  (or buffer-read-only
      (if buffer-file-name
	  (string-match executable-magicless-file-regexp
			buffer-file-name))
      (not (or insert-flag executable-insert))
      (> (point-min) 1)
      (save-excursion
	(let ((point (point-marker))
	      (buffer-modified-p (buffer-modified-p)))
	  (goto-char (point-min))
	  (make-local-hook 'after-save-hook)
	  (add-hook 'after-save-hook 'executable-chmod nil t)
	  (if (looking-at "#![ \t]*\\(.*\\)$")
	      (and (goto-char (match-beginning 1))
		   ;; If the line ends in a space,
		   ;; don't offer to change it.
		   (not (= (char-after (1- (match-end 1))) ?\ ))
		   (not (string= argument
				 (buffer-substring (point) (match-end 1))))
		   (if (or (not executable-query) no-query-flag
			   (save-window-excursion
			     ;; Make buffer visible before question.
			     (switch-to-buffer (current-buffer))
			     (y-or-n-p (concat "Replace magic number by `"
					       executable-prefix argument "'? "))))
		       (progn
			 (replace-match argument t t nil 1)
			 (message "Magic number changed to `%s'"
				  (concat executable-prefix argument)))))
	    (insert executable-prefix argument ?\n)
	    (message "Magic number changed to `%s'"
		     (concat executable-prefix argument)))
;;;	  (or insert-flag
;;;	      (eq executable-insert t)
;;;	      (set-buffer-modified-p buffer-modified-p))
	  )))
  interpreter)



;;;###autoload
(defun executable-self-display ()
  "Turn a text file into a self-displaying Un*x command.
The magic number of such a command displays all lines but itself."
  (interactive)
  (if (eq this-command 'executable-self-display)
      (setq this-command 'executable-set-magic))
  (executable-set-magic executable-self-display "+2"))



(provide 'executable)

;; executable.el ends here
