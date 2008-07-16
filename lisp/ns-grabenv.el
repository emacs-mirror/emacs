;;; ns-grabenv.el --- functions to set environment variables by running a subshell

;; Copyright (C) 1993, 1994, 2005, 2006, 2008 Free Software Foundation, Inc.

;; Author: Carl Edman, Christian Limpach, Scott Bender, Christophe de Dinechin, Adrian Robert
;; Keywords: terminals

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Idea based on NS 4.2 distribution, this version of code based on
;;; mac-read-environment-vars-from-shell () by David Reitter in Aquamacs dist..


;; utility function
(defun ns-make-command-string (cmdlist)
  (let ((str "")
	(cmds cmdlist))
    (while cmds
      (if (not (eq str "")) (setq str (format "%s ; " str)))
      (setq str (format "%s%s" str (car cmds)))
      (setq cmds (cdr cmds)))
    str))


;;;###autoload
(defun ns-grabenv (&optional shell-path &optional startup)
  "Run a shell subprocess, and interpret its output as a series of environment\n\
variables to insert into the emacs environment.  The first optional argument\n\
gives the path to the shell (defaults to the current setting of\n\
shell-file-name).  The remaining arguments are interpreted as a list of\n\
commands for it to execute (defaults to \"printenv\")."
  (interactive)
  (with-temp-buffer
    (let ((shell-file-name (if shell-path shell-path shell-file-name))
	  (cmd (ns-make-command-string (if startup startup '("printenv")))))
      (shell-command cmd t)
      (while (search-forward-regexp "^\\([A-Za-z_0-9]+\\)=\\(.*\\)$" nil t)
	(setenv (match-string 1)
		(if (equal (match-string 1) "PATH")
		    (concat (getenv "PATH") ":" (match-string 2))
		  (match-string 2)))))))

(provide 'ns-grabenv)

;; arch-tag: e65e1dd8-1566-460c-ad66-07948588be56
;;; ns-grabenv.el ends here
