;;; executable.el --- base functionality for executable interpreter scripts
;; Copyright (C) 1994, 1995 by Free Software Foundation, Inc.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Provide or modify interpreter magic number for buffer, and make file
;; executable if it isn't.  Support code for the likes of sh-, awk-, perl-,
;; tcl- or makefile-mode.

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


;;;###autoload
(or (assoc "tail" interpreter-mode-alist)
    (nconc interpreter-mode-alist
	   '(("tail" . text-mode)
	     ("more" . text-mode)
	     ("less" . text-mode)
	     ("pg" . text-mode))))

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

;; The C function openp() slightly modified would do the trick fine
(defun executable (command)
  "If COMMAND is an executable in $PATH its full name is returned.  Else nil."
  (let ((list exec-path)
	path)
    (while list
      (setq list (if (and (setq path (expand-file-name command (car list)))
			  (file-executable-p path)
			  (not (file-directory-p path)))
		     nil
		   (setq path nil)
		   (cdr list))))
    path))


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
(defun executable-set-magic (interpreter &optional argument)
  "Set this buffer's interpreter to INTERPRETER with optional ARGUMENT.
The variables `executable-magicless-file-regexp', `executable-prefix',
`executable-insert', `executable-query' and `executable-chmod' control
when and how magic numbers are inserted or replaced and scripts made
executable."
  (interactive "sName or path of interpreter: \nsArgument for %s: ")
  (setq interpreter (if (file-name-absolute-p interpreter)
			interpreter
		      (or (executable interpreter)
			  (error "Cannot find %s." interpreter)))
	argument (concat interpreter
			 (and argument (string< "" argument) " ")
			 argument))
  (or buffer-read-only
      (if buffer-file-name
	  (string-match executable-magicless-file-regexp
			buffer-file-name))
      (not (or (eq this-command 'executable-set-magic)
	       executable-insert))
      (> (point-min) 1)
      (let ((point (point-marker))
	    (buffer-modified-p (buffer-modified-p)))
	(goto-char (point-min))
	(make-local-variable 'after-save-hook)
	(add-hook 'after-save-hook 'executable-chmod)
	(if (looking-at "#![ \t]*\\(.*\\)$")
	    (and (goto-char (match-beginning 1))
		 (not (string= argument
			       (buffer-substring (point) (match-end 1))))
		 (save-window-excursion
		   ;; make buffer visible before question or message
		   (switch-to-buffer (current-buffer))
		   (if (or (not executable-query)
			   (and (eq executable-query 'function)
				(eq this-command 'executable-set-magic)))
		       (message "%s Magic number ``%s'' replaced." this-command
				(buffer-substring (point-min) (match-end 1)))
		     (y-or-n-p (concat "Replace magic number by ``"
				       executable-prefix argument "''? "))))
		 (not (delete-region (point) (match-end 1)))
		 (insert argument))
	  (insert executable-prefix argument ?\n))
	(or (< (marker-position point) (point))
	    (goto-char point))
	(or (eq this-command 'executable-set-magic))
	    (eq executable-insert t)
	    (set-buffer-modified-p buffer-modified-p)))
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
